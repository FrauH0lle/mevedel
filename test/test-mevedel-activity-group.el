;;; test-mevedel-activity-group.el --- Activity group tests -*- lexical-binding: t -*-

;;; Commentary:
;; The Emacs view and the browser projection must fold the same transcript
;; into the same activity groups with the same summaries.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'gptel)
(require 'mevedel-activity-group)
(require 'mevedel-collaboration-projection)
(require 'mevedel-tool-exec-registration)
(require 'mevedel-tool-render-data)
(require 'mevedel-tools)
(require 'mevedel-view)
(require 'mevedel-view-render)

(defun mevedel-activity-group-test--tool (name &optional status id)
  "Return a member tool entry NAME with STATUS and ID."
  (list :role 'member :kind 'tool :name name :status (or status 'completed) :id id))

(defun mevedel-activity-group-test--shape (items)
  "Return ITEMS as ids, with each group as a vector of its ids."
  (mapcar (lambda (item)
            (if (plist-get item :group)
                (vconcat (plist-get item :ids))
              (plist-get item :id)))
          items))

(mevedel-deftest mevedel-activity-group-partition ()
  ,test
  (test)
  :doc "a run groups only when it holds more tools than the threshold"
  (let ((run (list (mevedel-activity-group-test--tool "Read" nil 1)
                   (mevedel-activity-group-test--tool "Read" nil 2)
                   (mevedel-activity-group-test--tool "Read" nil 3))))
    (should (equal '([1 2 3]) (mevedel-activity-group-test--shape
                               (mevedel-activity-group-partition run 2))))
    (should (equal '(1 2 3) (mevedel-activity-group-test--shape
                             (mevedel-activity-group-partition run 3))))
    (should (equal '(1 2 3) (mevedel-activity-group-test--shape
                             (mevedel-activity-group-partition run 0))))
    (should (equal '([1 2 3]) (mevedel-activity-group-test--shape
                               (mevedel-activity-group-partition run nil)))))
  :doc "messages and completions join a run without counting toward it"
  (should (equal '(1 m 2)
                 (mevedel-activity-group-test--shape
                  (mevedel-activity-group-partition
                   (list (mevedel-activity-group-test--tool "Read" nil 1)
                         '(:role member :kind message :id m)
                         (mevedel-activity-group-test--tool "Read" nil 2))
                   2))))
  :doc "boundaries end runs; transparent entries ride inside one but not at its edges"
  (should (equal '(t0 [1 t1 2] t2 b [3 4])
                 (mevedel-activity-group-test--shape
                  (mevedel-activity-group-partition
                   (list '(:role transparent :id t0)
                         (mevedel-activity-group-test--tool "Read" nil 1)
                         '(:role transparent :id t1)
                         (mevedel-activity-group-test--tool "Read" nil 2)
                         '(:role transparent :id t2)
                         '(:role boundary :id b)
                         (mevedel-activity-group-test--tool "Read" nil 3)
                         (mevedel-activity-group-test--tool "Read" nil 4))
                   1))))
  :doc "leading messages and completions stay single, even when rebuilding a group"
  (dolist (threshold '(2 nil))
    (should (equal '(m c [1 2 3])
                   (mevedel-activity-group-test--shape
                    (mevedel-activity-group-partition
                     (list '(:role member :kind message :id m)
                           '(:role member :kind completion :id c
                             :completion ("exec" . "completed"))
                           (mevedel-activity-group-test--tool "Read" nil 1)
                           (mevedel-activity-group-test--tool "Read" nil 2)
                           (mevedel-activity-group-test--tool "Read" nil 3))
                     threshold)))))
  :doc "boundaries and scope changes reset the tool-first rule"
  (dolist (separator '((:role boundary :id b)
                       (:role member :kind message :id b :scope next)))
    (should (equal '(1 b m [2 3])
                   (mevedel-activity-group-test--shape
                    (mevedel-activity-group-partition
                     (list (mevedel-activity-group-test--tool "Read" nil 1)
                           separator
                           '(:role member :kind message :id m :scope next)
                           (plist-put (mevedel-activity-group-test--tool "Read" nil 2)
                                      :scope 'next)
                           (plist-put (mevedel-activity-group-test--tool "Read" nil 3)
                                      :scope 'next))
                     1)))))
  :doc "a hidden boundary still splits; transparency is a role, not visibility"
  (should (equal '(1 b 2)
                 (mevedel-activity-group-test--shape
                  (mevedel-activity-group-partition
                   (list (mevedel-activity-group-test--tool "Read" nil 1)
                         '(:role boundary :id b :hidden t)
                         (mevedel-activity-group-test--tool "Read" nil 2))
                   1))))
  :doc "a scope change ends the run"
  (should (equal '([1 2] 3)
                 (mevedel-activity-group-test--shape
                  (mevedel-activity-group-partition
                   (list (mevedel-activity-group-test--tool "Read" nil 1)
                         (mevedel-activity-group-test--tool "Read" nil 2)
                         (plist-put (mevedel-activity-group-test--tool "Read" nil 3)
                                    :scope 'directive))
                   1))))
  :doc "summaries count tools by verb in first-appearance order"
  (let ((summary (lambda (&rest names)
                   (plist-get (car (mevedel-activity-group-partition
                                    (mapcar #'mevedel-activity-group-test--tool names)
                                    nil))
                              :summary))))
    (should (equal "Searched 2 patterns, read 1 file, ran 2 commands"
                   (funcall summary "Grep" "Grep" "Read" "Bash" "Bash")))
    (should (equal "mcp_linear ×2, read 1 file"
                   (funcall summary "mcp_linear" "mcp_linear" "Read")))
    (should (equal "Read 2 files, LspRename"
                   (funcall summary "Read" "LspRename" "Read"))))
  :doc "distinct completions count by outcome, then messages"
  (should (equal "Read 1 file, 1 command completed, 1 failed, received 1 message"
                 (plist-get
                  (car (mevedel-activity-group-partition
                        (list (mevedel-activity-group-test--tool "Read")
                              '(:role member :kind completion
                                :completion (("e1" "/root") . "completed"))
                              '(:role member :kind completion
                                :completion (("e1" "/root") . "completed"))
                              '(:role member :kind completion
                                :completion (("e2" "/root") . "failed"))
                              '(:role member :kind message))
                        nil))
                  :summary)))
  :doc "pending calls run; a mixed failed and running group keeps its failures"
  (let ((group (car (mevedel-activity-group-partition
                     (list (mevedel-activity-group-test--tool "Read" 'warning)
                           (mevedel-activity-group-test--tool "Grep" 'failed)
                           (mevedel-activity-group-test--tool "Bash" 'running)
                           (mevedel-activity-group-test--tool "Grep" 'failed)
                           (mevedel-activity-group-test--tool "Read" 'running))
                     2))))
    (should (eq 'running (plist-get group :status)))
    (should (= 2 (plist-get group :running)))
    (should (= 2 (plist-get group :failed)))
    (should (plist-get group :warning))
    (should (equal "2 failed: Grep" (plist-get group :failure-label))))
  :doc "status falls from failed to warning to completed"
  (should (equal '(failed warning completed)
                 (mapcar (lambda (status)
                           (plist-get (car (mevedel-activity-group-partition
                                            (list (mevedel-activity-group-test--tool
                                                   "Read" status))
                                            nil))
                                      :status))
                         '(failed warning completed))))
  :doc "a long list of failed names ends in an ellipsis within its budget"
  (let ((label (plist-get
                (car (mevedel-activity-group-partition
                      (list (mevedel-activity-group-test--tool
                             "mcp__server__first_long_tool" 'failed)
                            (mevedel-activity-group-test--tool
                             "mcp__server__second_long_tool" 'failed))
                      nil))
                :failure-label)))
    (should (string-suffix-p "…" label))
    (should (<= (string-width label)
                (+ (string-width "2 failed: ")
                   mevedel-activity-group--failed-names-width)))))

(defun mevedel-activity-group-test--part (label kind &optional name status)
  "Return transcript text for part LABEL of KIND.
A tool part runs tool NAME and settles with STATUS."
  (pcase kind
    ('tool
     (concat "#+begin_tool\n"
             (propertize
              (concat
               (format "(:name %S :args (:file_path \"/tmp/%s\"))\n%s\n"
                       (or name "Read") label
                       (if (eq status 'failed) "Error: failed"
                         (format "result %s" label)))
               (when (memq status '(warning running denied))
                 (mevedel-tool-render-data-format (list :status status) label)))
              'gptel (cons 'tool label))
             "#+end_tool\n"))
    ('poll
     (concat "#+begin_tool\n"
             (propertize
              (concat "(:name \"WriteStdin\" :args (:execution_id \"exec\" :chars \"\"))\nstill running\n"
                      (mevedel-tool-render-data-format
                       '(:kind bash :execution-control poll :control-succeeded-p t :status success)
                       label))
              'gptel (cons 'tool label))
             "#+end_tool\n"))
    ('mail (format "<agent-message type=\"MAIL\" sender=\"/root/a\">\n%s\n</agent-message>\n"
                   label))
    ('notice (format "<agent-result sender=\"/root/a\" outcome=\"completed\">\n%s\n</agent-result>\n"
                     label))
    ('own (format (concat "<agent-message type=\"EXECUTION\" sender=\"/root\" "
                          "execution_id=\"%s\" description=\"own\" "
                          "outcome=\"success\" exit_code=\"0\">\nout\n</agent-message>\n")
                  (or name label)))
    ((or 'dstart 'dend)
     (mevedel--format-hook-audit-record
      (append (list :type 'directive-turn-boundary
                    :edge (if (eq kind 'dstart) 'start 'end)
                    :directive-id "scope" :action 'discuss :turn 1)
              (and (eq kind 'dend)
                   '(:outcome success :activity-kind discussion :sequence 1)))))
    ('directive-prompt "Question :discuss:\n:PROMPT:\nFull directive request\n:END:\n")
    ('crumb (mevedel--format-hook-audit-record
             (list :type 'execution-breadcrumb :owner "/root" :execution-id label
                   :command "make" :facts '(:kind bash :outcome success))))
    ('audit (mevedel--format-hook-audit-record
             (list :type (or name 'guest-prompt) :name label)))
    ('reasoning (propertize (format "#+begin_reasoning\n%s\n#+end_reasoning\n" label)
                            'gptel 'ignore))
    ('text (propertize (format "%s.\n\n" label) 'gptel 'response))
    ('prompt (format "%s?\n\n" label))
    ('summary (mevedel-tool-render-data-format
               (list :kind 'request-summary :elapsed-seconds 1 :outcome (or name 'success)
                     :message label)))))

(defun mevedel-activity-group-test--view-groups (view-buf positions pending)
  "Return VIEW-BUF's rendered activity groups as (LABELS . HEADER) pairs.
POSITIONS maps each settled label to its start in the data buffer;
PENDING labels in-flight calls, which a group header counts as running."
  (with-current-buffer view-buf
    (let ((pos (point-min)) groups seen)
      (while (< pos mevedel-view--input-marker)
        (let* ((eol (save-excursion (goto-char pos) (line-end-position)))
               (at (or (text-property-any pos eol 'mevedel-view-type 'tool-group)
                       (text-property-any pos eol 'mevedel-view-type 'tool-pending)))
               (type (and at (get-text-property at 'mevedel-view-type)))
               (line (buffer-substring-no-properties pos eol))
               (running (and (string-match "· \\([0-9]+\\) running" line)
                             (string-to-number (match-string 1 line)))))
          (when (or (eq type 'tool-group) (and (eq type 'tool-pending) running))
            (let* ((source (get-text-property at 'mevedel-view-source))
                   (start (mevedel-view-disclosure-source-start source))
                   (end (and (consp source) (marker-position (cdr source)))))
              (unless (and start (member start seen))
                (push start seen)
                (push (cons (append
                             (and start
                                  (cl-loop for (label . at) in positions
                                           when (and (<= start at) (< at end))
                                           collect label))
                             (and running
                                  (prog1 (seq-take pending running)
                                    (setq pending (nthcdr running pending)))))
                            line)
                      groups))))
          (setq pos (min mevedel-view--input-marker (1+ eol)))))
      (nreverse groups))))

(defun mevedel-activity-group-test--projection-groups (records labels)
  "Return activity groups of projected RECORDS as (LABELS ACTIVITY) pairs."
  (let (groups)
    (dolist (record records)
      (when-let* ((activity (plist-get record :activity))
                  (label (cl-find-if
                          (lambda (label)
                            (or (equal label (plist-get record :toolUseId))
                                (equal label (plist-get record :tool-use-id))
                                (equal label (plist-get (plist-get record :execution) :id))
                                (and (stringp (plist-get record :text))
                                     (string-search label (plist-get record :text)))))
                          labels)))
        (let ((cell (assoc (plist-get activity :id) groups)))
          (if cell (setcdr cell (list (append (cadr cell) (list label)) activity))
            (push (list (plist-get activity :id) (list label) activity) groups)))))
    (mapcar #'cdr (nreverse groups))))

(defun mevedel-activity-group-test--surfaces (parts &optional pending)
  "Render PARTS in both surfaces; return (VIEW-GROUPS PROJECTION-GROUPS).
Each part is (LABEL KIND [NAME STATUS]).  PENDING labels in-flight Read
calls that follow PARTS in a turn without a final response."
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-tool-group-collapse-threshold 2)
          positions)
      (mevedel-tool-exec--register)
      (mevedel-tool-register
       (mevedel-tool--create :name "CoalesceProbe" :category "mevedel"
                             :renderer (lambda (&rest _)
                                         '(:header "CoalesceProbe: latest" :body "latest"
                                           :coalesce-key "probe"))))
      (with-current-buffer data-buf
        (insert "Start?\n\n")
        (dolist (part parts)
          (goto-char (point-max))
          ;; Audits, prose and polls are never members; an own delivery
          ;; naming a breadcrumb's execution repeats that breadcrumb.
          (when (and (memq (cadr part) '(tool mail own crumb))
                     (not (and (eq (cadr part) 'own) (caddr part))))
            (push (cons (car part) (point)) positions))
          (insert (apply #'mevedel-activity-group-test--part part)))
        (unless pending
          (insert (propertize "Done.\n" 'gptel 'response))))
      (setq positions (nreverse positions))
      (with-current-buffer view-buf
        (let ((mevedel-view--render-pending
               (and pending
                    (list (mapcar (lambda (label)
                                    (list label "Read" label (list :tool-use-id label)))
                                  pending)))))
          (mevedel-view--full-rerender)))
      (let* ((canonical (mevedel-collaboration--canonical-records
                         data-buf nil nil nil "/root"))
             (room (list :data-buffer data-buf
                         :pending-tools
                         (mapcar (lambda (label)
                                   (mevedel-collaboration--record
                                    (format "tool-%s" label) "tool"
                                    :revision 0 :name "Read" :status "running"
                                    :summary "Read" :result "" :pending t
                                    :tool-use-id label :presentation nil
                                    :baseline-tool-count
                                    (length (mevedel-collaboration--tool-records canonical))
                                    :baseline-record-count (length canonical)))
                                 pending)))
             (records (if pending (mevedel-collaboration--project-records room)
                        canonical)))
        (list (mevedel-activity-group-test--view-groups view-buf positions pending)
              (mevedel-activity-group-test--projection-groups
               records (append (mapcar #'car positions) pending)))))))

(defconst mevedel-activity-group-test--agreement-cases
  '((plain (t1 tool) (t2 tool) (t3 tool))
    (leading-breadcrumb (x1 text) (c1 crumb) (t1 tool) (t2 tool) (t3 tool))
    (leading-mail (x1 text) (m1 mail) (t1 tool) (t2 tool) (t3 tool))
    (leading-mail-breadcrumb (x1 text) (m1 mail) (c1 crumb)
                             (t1 tool) (t2 tool) (t3 tool))
    (leading-breadcrumb-mail (x1 text) (c1 crumb) (m1 mail)
                             (t1 tool) (t2 tool) (t3 tool))
    (leading-audit-mail (x1 text) (a1 audit) (m1 mail)
                        (t1 tool) (t2 tool) (t3 tool))
    (leading-mail-audit (x1 text) (m1 mail) (a1 audit)
                        (t1 tool) (t2 tool) (t3 tool))
    (leading-mail-audit-breadcrumb (x1 text) (m1 mail) (a1 audit) (c1 crumb)
                                   (t1 tool) (t2 tool) (t3 tool))
    (leading-breadcrumb-audit-mail (x1 text) (c1 crumb) (a1 audit) (m1 mail)
                                   (t1 tool) (t2 tool) (t3 tool))
    (leading-hidden-audit-mail (x1 text) (a1 audit provider-tool-batch) (m1 mail)
                               (t1 tool) (t2 tool) (t3 tool))
    (mail-after-boundary (t0 tool) (m0 mail) (a1 audit) (m1 mail) (c1 crumb)
                         (t1 tool) (t2 tool) (t3 tool))
    (merged-audit (t1 tool) (t2 tool) (a1 audit) (t3 tool) (t4 tool))
    (hidden-merged-audit (t1 tool) (t2 tool) (a1 audit provider-tool-batch) (t3 tool))
    (leading-audit (a1 audit) (t1 tool) (t2 tool) (t3 tool))
    (audit-after-mail (t1 tool) (t2 tool) (t3 tool) (m1 mail) (a1 audit)
                      (t4 tool) (t5 tool) (t6 tool))
    (mail (t1 tool) (m1 mail) (t2 tool) (t3 tool))
    (notice (t1 tool) (t2 tool) (n1 notice) (t3 tool) (t4 tool) (t5 tool))
    (own-completion (t1 tool) (o1 own) (t2 tool) (t3 tool))
    (own-completion-with-breadcrumb (t1 tool) (c1 crumb) (o1 own "c1") (t2 tool) (t3 tool))
    (scope (t1 tool) (t2 tool) (t3 tool) (d1 dstart) (q1 directive-prompt)
           (t4 tool) (t5 tool) (t6 tool) (x1 text) (d2 dend))
    (breadcrumb (t1 tool) (c1 crumb) (t2 tool) (t3 tool))
    (poll (t1 tool) (t2 tool) (h1 poll) (t3 tool) (t4 tool))
    (reasoning (t1 tool) (t2 tool) (t3 tool) (r1 reasoning) (t4 tool) (t5 tool) (t6 tool))
    (assistant-text (t1 tool) (t2 tool) (x1 text) (t3 tool) (t4 tool) (t5 tool))
    (prompt (t1 tool) (t2 tool) (t3 tool) (p1 prompt) (t4 tool) (t5 tool) (t6 tool))
    (mixed-status (t1 tool "Bash" running) (t2 tool "Grep" failed) (t3 tool "Read" warning))
    (warning (t1 tool) (t2 tool "Grep" warning) (t3 tool))
    (denied (t1 tool) (t2 tool "Bash" denied) (t3 tool))
    (request-summary (t1 tool) (t2 tool) (s1 summary) (t3 tool) (t4 tool))
    (failed-request-summary (t1 tool) (t2 tool) (s1 summary error) (t3 tool) (t4 tool))
    (coalescing (t1 tool) (t2 tool) (w1 tool "CoalesceProbe") (t3 tool))
    (pending :pending (p1 p2) (t1 tool) (t2 tool))
    (pending-below :pending (p1 p2) (t1 tool)))
  "Transcripts whose activity grouping both surfaces must agree on.")

(mevedel-deftest mevedel-activity-group-agreement ()
  ,test
  (test)
  :doc "the view and the projection fold the same transcript into the same groups"
  (let (disagreements details)
    (dolist (case mevedel-activity-group-test--agreement-cases)
      (let* ((pending (and (eq (cadr case) :pending)
                           (mapcar #'symbol-name (caddr case))))
             (parts (mapcar (lambda (part)
                              (cons (symbol-name (car part)) (cdr part)))
                            (if pending (cdddr case) (cdr case))))
             (surfaces (mevedel-activity-group-test--surfaces parts pending))
             (view (car surfaces))
             (projection (cadr surfaces)))
        (unless (and (equal (mapcar #'car view) (mapcar #'car projection))
                     ;; Each browser summary and failure label appears in
                     ;; the matching Emacs group row.
                     (cl-every (lambda (pair)
                                 (let ((activity (cadr (cdr pair)))
                                       (header (cdr (car pair))))
                                   (and (string-search (plist-get activity :summary) header)
                                        (or (null (plist-get activity :failures))
                                            (string-search (plist-get activity :failures)
                                                           header)))))
                               (cl-mapcar #'cons view projection)))
          (push (car case) disagreements)
          (push (list (car case) :view view
                      :projection (mapcar (lambda (group)
                                            (list (car group)
                                                  (plist-get (cadr group) :summary)
                                                  (plist-get (cadr group) :failures)))
                                          projection))
                details))))
    (ert-info ((prin1-to-string (nreverse details)))
      (should-not (nreverse disagreements)))))

(provide 'test-mevedel-activity-group)
;;; test-mevedel-activity-group.el ends here
