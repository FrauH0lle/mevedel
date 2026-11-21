;;; niwa-activity-group.el --- Fold consecutive activity into groups -*- lexical-binding: t; -*-

;;; Commentary:

;; The single owner of the activity-group rule for both the Emacs view and
;; the browser projection.  An activity group is the collapsible summary of
;; consecutive routine tool calls, agent messages and the transcript owner's
;; background completions within one transcript scope.
;;
;; Each surface normalizes the activity it already decoded into entries once
;; and calls `niwa-activity-group-partition'.  The module is pure: it
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
;;                Only a tool starts a run; leading messages and completions
;;                stay single after any boundary or scope change.
;;   :name        A tool's name, for the summary.
;;   :status      A tool's `completed', `running', `failed' or `warning'.
;;                In-flight calls enter as running tools.
;;   :completion  For a completion, (KEY . STATUS): KEY identifies the
;;                execution, STATUS is its outcome word.
;;   :unconfined  Non-nil for a tool that ran without sandbox confinement.
;;   :scope       Compared with `equal'; a change ends the run.
;;
;; Callers may add their own keys; entries are returned unchanged.
;;
;; A group's summary is one plain string for every surface.  Its call count
;; and failure counts carry the `niwa-activity-group-part' text property
;; (`count', `failure' or `muted'), so a surface that styles them needs no
;; second parse; one that ignores properties shows the same text.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defconst niwa-activity-group--verbs
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

(defun niwa-activity-group--part (text part)
  "Return TEXT marked as summary PART for surfaces that style it."
  (propertize text 'niwa-activity-group-part part))

(defun niwa-activity-group--summary (entries)
  "Return the one-line summary of member ENTRIES.
It starts with the call count, as in `5 calls:'.  Tools count by name in
first-appearance order, each followed by its failures, as in `ran 4
commands (2 failed)'; then distinct completions by outcome, then messages."
  (let (names counts failures completions (messages 0) (tools 0))
    (dolist (entry entries)
      (pcase (plist-get entry :kind)
        ('message (setq messages (1+ messages)))
        ('completion
         (let ((completion (plist-get entry :completion)))
           (unless (assoc (car completion) completions)
             (push completion completions))))
        (_
         (let ((name (or (plist-get entry :name) "Tool")))
           (setq tools (1+ tools))
           (unless (member name names)
             (push name names))
           (setf (alist-get name counts 0 nil #'equal)
                 (1+ (alist-get name counts 0 nil #'equal)))
           (when (eq (plist-get entry :status) 'failed)
             (setf (alist-get name failures 0 nil #'equal)
                   (1+ (alist-get name failures 0 nil #'equal))))))))
    (setq names (nreverse names))
    (concat
     (niwa-activity-group--part
      (format (if (= tools 1) "%d call" "%d calls") tools) 'count)
     ": "
     (string-join
      (append
       (mapcar
        (lambda (name)
          (let ((count (alist-get name counts 0 nil #'equal))
                (failed (alist-get name failures nil nil #'equal))
                (verb (assoc name niwa-activity-group--verbs)))
            (concat
             (cond
              (verb (format (if (= count 1) (nth 1 verb) (nth 2 verb)) count))
              ((= count 1) name)
              (t (format "%s ×%d" name count)))
             (when failed
               (concat " "
                       (niwa-activity-group--part "(" 'muted)
                       (niwa-activity-group--part
                        (format "%d failed" failed) 'failure)
                       (niwa-activity-group--part ")" 'muted))))))
        names)
       (let (statuses parts)
         (dolist (completion completions)
           (setf (alist-get (cdr completion) statuses 0 nil #'equal)
                 (1+ (alist-get (cdr completion) statuses 0 nil #'equal))))
         (dolist (status '("completed" "failed" "stopped" "timed out" "ended" "lost")
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
                       messages))))
      ", "))))

(defun niwa-activity-group--group (entries)
  "Return the group of run ENTRIES, which start and end with members."
  (let ((running 0) (failed 0) (tools 0) warning unconfined members)
    (dolist (entry entries)
      (unless (eq (plist-get entry :role) 'transparent)
        (push entry members)
        (when (eq (plist-get entry :kind) 'tool)
          (cl-incf tools)
          (when (plist-get entry :unconfined)
            (setq unconfined t))
          (pcase (plist-get entry :status)
            ('running (setq running (1+ running)))
            ('failed (setq failed (1+ failed)))
            ('warning (setq warning t))))))
    (setq members (nreverse members))
    (list :group t
          :entries entries
          :ids (mapcar (lambda (entry) (plist-get entry :id)) entries)
          :summary (niwa-activity-group--summary members)
          :running running
          :failed failed
          :tools tools
          :warning warning
          :unconfined unconfined
          :status (cond ((> running 0) 'running)
                        ((and (> failed 0) (= failed tools)) 'failed)
                        ((or (> failed 0) warning) 'warning)
                        (t 'completed)))))

(defun niwa-activity-group-partition (entries threshold)
  "Partition activity ENTRIES into single entries and activity groups.
Return a list in source order whose elements are either an entry or a
group.  A run of consecutive members within one scope becomes a group when
it holds more than THRESHOLD tools; zero disables grouping, and nil groups
any run with a tool, for rebuilding a group already formed.  Only tools
start runs: messages and completions before the first tool stay single,
including after a boundary or scope change.  Transparent
entries between a run's members travel with the group; outside one they
stay single.

A group is a plist: `:group' t, `:entries' its member and enclosed
transparent entries, `:ids' their `:id's, `:summary', `:running' (running
tools), `:failed' (failed tools), `:tools' (all tools), `:warning' (non-nil
when a tool warned),
`:unconfined' (non-nil when a tool ran without sandbox confinement), and
`:status', which is `running', `failed', `warning' or `completed'.  The
summary counts each tool's failures beside it.  A group is `failed' only
when every tool failed; some failures make it `warning'.  Running and
failure facts are independent: a running group keeps its failure counts."
  (let (out run tail tools scope)
    (cl-flet ((close ()
                (when run
                  (let ((run (nreverse run)))
                    (if (and (> tools 0)
                             (or (null threshold)
                                 (and (> threshold 0) (> tools threshold))))
                        (push (niwa-activity-group--group run) out)
                      (dolist (entry run) (push entry out)))))
                (dolist (entry (nreverse tail)) (push entry out))
                (setq run nil tail nil tools 0)))
      (setq tools 0)
      (dolist (entry entries)
        (pcase (plist-get entry :role)
          ('member
           (unless (or (null run) (equal scope (plist-get entry :scope)))
             (close))
           (if (or run (eq (plist-get entry :kind) 'tool))
               (progn
                 (setq scope (plist-get entry :scope)
                       run (cons entry (append tail run))
                       tail nil)
                 (when (eq (plist-get entry :kind) 'tool)
                   (setq tools (1+ tools))))
             (push entry out)))
          ('transparent
           (if run (push entry tail) (push entry out)))
          (_ (close) (push entry out))))
      (close))
    (nreverse out)))

(provide 'niwa-activity-group)
;;; niwa-activity-group.el ends here
