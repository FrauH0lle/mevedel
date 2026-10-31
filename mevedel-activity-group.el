;;; mevedel-activity-group.el --- Fold consecutive activity into groups -*- lexical-binding: t; -*-

;;; Commentary:

;; The single owner of the activity-group rule for both the Emacs view and
;; the browser projection.  An activity group is the collapsible summary of
;; consecutive routine tool calls, agent messages and the transcript owner's
;; background completions within one transcript scope.
;;
;; Each surface normalizes the activity it already decoded into entries once
;; and calls `mevedel-activity-group-partition'.  The module is pure: it
;; renders nothing, reads no buffer and keeps no state.  An entry is a plist:
;;
;;   :role        `member' may join a run; `boundary' ends one (notices,
;;                rows that need individual presentation, prose, reasoning,
;;                turn boundaries and non-displayed separators such as
;;                standalone audits); `transparent' neither joins nor splits
;;                a run (only semantically empty rows, such as the transcript
;;                owner's own completion mail).  Hidden alone is not
;;                transparent.
;;   :kind        `tool', `message' or `completion'.
;;   :name        A tool's name, for the summary.
;;   :status      A tool's `completed', `running', `failed' or `warning'.
;;                In-flight calls enter as running tools.
;;   :completion  For a completion, (KEY . STATUS): KEY identifies the
;;                execution, STATUS is its outcome word.
;;   :scope       Compared with `equal'; a change ends the run.
;;
;; Callers may add their own keys; entries are returned unchanged.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defconst mevedel-activity-group--verbs
  '(("Grep" "searched %d pattern" "searched %d patterns")
    ("Glob" "matched %d glob" "matched %d globs")
    ("Read" "read %d file" "read %d files")
    ("Bash" "ran %d command" "ran %d commands")
    ("Eval" "evaluated %d form" "evaluated %d forms")
    ("WebFetch" "fetched %d page" "fetched %d pages")
    ("WebSearch" "ran %d web search" "ran %d web searches"))
  "Verb phrases for activity group summaries, per tool name.
Each entry is (NAME SINGULAR PLURAL) with a `%d' count slot.  Tools
without an entry -- MCP tools included -- fall back to \"NAME xN\".")

(defconst mevedel-activity-group--failed-names-width 30
  "Most columns a failure label spends naming its failed tools.")

(defun mevedel-activity-group--summary (entries)
  "Return the one-line summary of member ENTRIES.
Tools count by name in first-appearance order, then distinct completions by
outcome, then messages."
  (let (names counts completions (messages 0))
    (dolist (entry entries)
      (pcase (plist-get entry :kind)
        ('message (setq messages (1+ messages)))
        ('completion
         (let ((completion (plist-get entry :completion)))
           (unless (assoc (car completion) completions)
             (push completion completions))))
        (_
         (let ((name (or (plist-get entry :name) "Tool")))
           (unless (member name names)
             (push name names))
           (setf (alist-get name counts 0 nil #'equal)
                 (1+ (alist-get name counts 0 nil #'equal)))))))
    (setq names (nreverse names))
    (let* ((parts
            (append
             (mapcar
              (lambda (name)
                (let ((count (alist-get name counts 0 nil #'equal))
                      (verb (assoc name mevedel-activity-group--verbs)))
                  (cond
                   (verb (format (if (= count 1) (nth 1 verb) (nth 2 verb)) count))
                   ((= count 1) name)
                   (t (format "%s ×%d" name count)))))
              names)
             (let (statuses parts)
               (dolist (completion completions)
                 (setf (alist-get (cdr completion) statuses 0 nil #'equal)
                       (1+ (alist-get (cdr completion) statuses 0 nil #'equal))))
               (dolist (status '("completed" "failed" "stopped" "timed out")
                               (nreverse parts))
                 (when-let* ((count (alist-get status statuses nil nil #'equal)))
                   (push (if parts
                             (format "%d %s" count status)
                           (format "%d command%s %s" count
                                   (if (= count 1) "" "s") status))
                         parts))))
             (when (> messages 0)
               (list (format (if (= messages 1) "received %d message"
                               "received %d messages")
                             messages)))))
           (summary (string-join parts ", ")))
      ;; Capitalize only a verb phrase; a leading tool name keeps its casing.
      (if (and (not (string-empty-p summary))
               (assoc (car names) mevedel-activity-group--verbs))
          (concat (upcase (substring summary 0 1)) (substring summary 1))
        summary))))

(defun mevedel-activity-group--failure-label (count names)
  "Return `N failed' for COUNT failures, naming the failed tools in NAMES.
NAMES are shown once each, in order, and end in `…' past
`mevedel-activity-group--failed-names-width' columns, so a folded row can
still say which call failed without growing past one line."
  (let ((label (format "%d failed" count))
        (names (delete-dups (copy-sequence names))))
    (if (not names)
        label
      (concat label ": "
              (truncate-string-to-width
               (string-join names ", ") mevedel-activity-group--failed-names-width
               nil nil "…")))))

(defun mevedel-activity-group--group (entries)
  "Return the group of run ENTRIES, which start and end with members."
  (let ((running 0) failed warning members)
    (dolist (entry entries)
      (unless (eq (plist-get entry :role) 'transparent)
        (push entry members)
        (when (eq (plist-get entry :kind) 'tool)
          (pcase (plist-get entry :status)
            ('running (setq running (1+ running)))
            ('failed (push (or (plist-get entry :name) "Tool") failed))
            ('warning (setq warning t))))))
    (setq members (nreverse members)
          failed (nreverse failed))
    (list :group t
          :entries entries
          :ids (mapcar (lambda (entry) (plist-get entry :id)) entries)
          :summary (mevedel-activity-group--summary members)
          :running running
          :failed (length failed)
          :warning warning
          :failure-label (and failed (mevedel-activity-group--failure-label
                                      (length failed) failed))
          :status (cond ((> running 0) 'running)
                        (failed 'failed)
                        (warning 'warning)
                        (t 'completed)))))

(defun mevedel-activity-group-partition (entries threshold)
  "Partition activity ENTRIES into single entries and activity groups.
Return a list in source order whose elements are either an entry or a
group.  A run of consecutive members within one scope becomes a group when
it holds more than THRESHOLD tools; zero disables grouping, and nil groups
any run with a tool, for rebuilding a group already formed.  Transparent
entries between a run's members travel with the group; outside one they
stay single.

A group is a plist: `:group' t, `:entries' its member and enclosed
transparent entries, `:ids' their `:id's, `:summary', `:running' (running
tools), `:failed' (failed tools), `:warning' (non-nil when a tool warned),
`:failure-label' (nil without failures),
and `:status', which is `running', `failed', `warning' or `completed'.
Running and failure facts are independent: a running group keeps its
failure count and label."
  (let (out run tail tools scope)
    (cl-flet ((close ()
                (when run
                  (let ((run (nreverse run)))
                    (if (and (> tools 0)
                             (or (null threshold)
                                 (and (> threshold 0) (> tools threshold))))
                        (push (mevedel-activity-group--group run) out)
                      (dolist (entry run) (push entry out)))))
                (dolist (entry (nreverse tail)) (push entry out))
                (setq run nil tail nil tools 0)))
      (setq tools 0)
      (dolist (entry entries)
        (pcase (plist-get entry :role)
          ('member
           (unless (or (null run) (equal scope (plist-get entry :scope)))
             (close))
           (setq scope (plist-get entry :scope)
                 run (cons entry (append tail run))
                 tail nil)
           (when (eq (plist-get entry :kind) 'tool)
             (setq tools (1+ tools))))
          ('transparent
           (if run (push entry tail) (push entry out)))
          (_ (close) (push entry out))))
      (close))
    (nreverse out)))

(provide 'mevedel-activity-group)
;;; mevedel-activity-group.el ends here
