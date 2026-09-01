;;; test-mevedel-skills-input.el -- Skill input tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests user skill parsing, binding, projection, and dispatch.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel)
(require 'mevedel-agent-control)
(require 'mevedel-compact-estimation)
(require 'mevedel-file-state)
(require 'mevedel-mention-bindings)
(require 'mevedel-mentions)
(require 'mevedel-pending-inputs)
(require 'mevedel-pipeline)
(require 'mevedel-resource)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-skills-core)
(require 'mevedel-skills-input)
(require 'mevedel-skills-invoke)
(require 'mevedel-structs)
(require 'mevedel-transcript)
(require 'mevedel-tool-render-data)
(require 'mevedel-workspace)

(mevedel-deftest mevedel-skills-input-current-prompt-region ()
  ,test
  (test)
  :doc "uses the `gptel' text property to find the boundary after a response"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (insert "response body")
      (put-text-property (point-min) (point-max) 'gptel 'response)
      (insert "/help")
      (goto-char (point-max))
      (let ((region (mevedel-skills-input-current-prompt-region)))
        (should region)
        (should (equal "/help"
                       (buffer-substring-no-properties
                        (car region) (cdr region)))))))

  :doc "falls back to the configured prompt prefix when no property is set"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### older\nplain response\n### /help")
      (goto-char (point-max))
      (let ((region (mevedel-skills-input-current-prompt-region)))
        (should region)
        (should (equal "/help"
                       (buffer-substring-no-properties
                        (car region) (cdr region)))))))

  :doc "without prefix or property the whole buffer is the pending prompt"
  (let ((session (mevedel-skills-test--make-session)))
    (with-temp-buffer
      (setq mevedel--session session)
      (insert "/help alone")
      (goto-char (point-max))
      (let ((region (mevedel-skills-input-current-prompt-region)))
        (should region)
        (should (equal "/help alone"
                       (buffer-substring-no-properties
                        (car region) (cdr region)))))))

  :doc "empty buffer returns nil"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (should (null (mevedel-skills-input-current-prompt-region))))))


;;
;;; Leading and inline invocation bridges

(mevedel-deftest mevedel-skills-input-parse-skill-line ()
  ,test
  (test)
  :doc "plain `$skill' parses to (name \"\" 0)"
  (should (equal '("greet" "" 0)
                 (mevedel-skills-input-parse-skill-line "$greet")))

  :doc "`$skill args' parses to (name args 0)"
  (should (equal '("greet" "world" 0)
                 (mevedel-skills-input-parse-skill-line "$greet world")))

  :doc "plugin-prefixed skill names parse as one skill name"
  (should (equal '("superpowers:brainstorming" "now" 0)
                 (mevedel-skills-input-parse-skill-line
                  "$superpowers:brainstorming now")))

  :doc "additional lines after the skill name are appended to ARGS"
  (should (equal '("delegate"
                   "Launch three explorer agents:\n  (a) ...\n  (b) ..."
                   0)
                 (mevedel-skills-input-parse-skill-line
                  "$delegate Launch three explorer agents:
  (a) ...
  (b) ...")))

  :doc "multi-line ARGS work even when no first-line arguments"
  (should (equal '("delegate"
                   "Multi-line task body\nspanning lines"
                   0)
                 (mevedel-skills-input-parse-skill-line
                  "$delegate
Multi-line task body
spanning lines")))

  :doc "text not starting with `$' returns nil"
  (should (null (mevedel-skills-input-parse-skill-line "hello $greet")))
  (should (null (mevedel-skills-input-parse-skill-line "/greet")))

  :doc "non-identifier skill names are rejected"
  (should (null (mevedel-skills-input-parse-skill-line "$hi!")))
  (should (null (mevedel-skills-input-parse-skill-line "$")))

  :doc "leading whitespace is reported via offset"
  (should (equal '("greet" "" 3)
                 (mevedel-skills-input-parse-skill-line "   $greet"))))

(mevedel-deftest mevedel-skills-input--inline-skill-mentions ()
  ,test
  (test)
  :doc "finds inline skills, dedupes, and skips literal forms"
  (let* ((session (mevedel-skills-test--make-session))
         (alpha (mevedel-skill--create :name "alpha" :body "A"))
         (beta (mevedel-skill--create :name "beta" :body "B")))
    (setf (mevedel-session-skills session) (list alpha beta))
    (cl-letf (((symbol-function 'mevedel-skills-ensure-fresh)
               (lambda (&rest _) nil)))
      (let ((mentions
             (mevedel-skills-input--inline-skill-mentions
              "Use $alpha, \"$beta\", '$beta', \\$beta, `$beta`, $alpha, and $beta."
              session)))
        (should (equal '("alpha" "beta")
                       (mapcar (lambda (item) (plist-get item :name))
                               mentions))))))

  :doc "single-backtick code spans and quoted phrases stay literal"
  (let* ((session (mevedel-skills-test--make-session))
         (alpha (mevedel-skill--create :name "alpha" :body "A"))
         (beta (mevedel-skill--create :name "beta" :body "B")))
    (setf (mevedel-session-skills session) (list alpha beta))
    (cl-letf (((symbol-function 'mevedel-skills-ensure-fresh)
               (lambda (&rest _) nil)))
      (should-not
       (mevedel-skills-input--inline-skill-mentions
        "Use `$beta` here and say \"please $alpha\"." session))
      (should-not
       (mevedel-skills-input--inline-skill-mentions
        "Use '$beta.' here and \"please $alpha.\"." session))))

  :doc "leading command-style skill is not an inline mention"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "alpha" :body "A")))
    (setf (mevedel-session-skills session) (list skill))
    (cl-letf (((symbol-function 'mevedel-skills-ensure-fresh)
               (lambda (&rest _) nil)))
      (should-not
       (mevedel-skills-input--inline-skill-mentions "$alpha run this"
                                                    session))))

  :doc "inline mention scan ignores unknown dollar text"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "alpha" :body "A")))
    (setf (mevedel-session-skills session) (list skill))
    (cl-letf (((symbol-function 'mevedel-skills-ensure-fresh)
               (lambda (&rest _) nil)))
      (should-not
       (mevedel-skills-input--inline-skill-mentions
        "Keep $PATH literal" session))
      (should
       (mevedel-skills-input--inline-skill-mentions
        "Use $alpha here" session))))

  :doc "known fork skill becomes a non-forking instruction mention"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "review" :body "R" :context 'fork)))
    (setf (mevedel-session-skills session) (list skill))
    (cl-letf (((symbol-function 'mevedel-skills-ensure-fresh)
               (lambda (&rest _) nil)))
      (let ((result (mevedel-skills-input--inline-skill-mentions
                     "Please use $review here" session)))
        (should (equal '("review")
                       (mapcar (lambda (item) (plist-get item :name))
                               result))))))

  :doc "known disabled skill becomes an unavailable inline occurrence"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "disabled" :body "D")))
    (setf (mevedel-session-skills session) (list skill))
    (cl-letf (((symbol-function 'mevedel-skills-ensure-fresh)
               (lambda (&rest _) nil))
              ((symbol-function 'mevedel-skills-skill-enabled-p)
               (lambda (_skill) nil)))
      (let ((result (mevedel-skills-input--inline-skill-mentions
                     "Please use $disabled here" session)))
        (should (equal "disabled" (plist-get (car result) :name)))
        (should (plist-get (car result) :unavailable))
        (should (string-match-p "disabled"
                                (plist-get (car result) :message))))))

  :doc "same-named bindings preserve distinct canonical sources"
  (let* ((root (make-temp-file "mevedel-inline-sources-" t))
         (source-a (file-name-concat root "a.md"))
         (source-b (file-name-concat root "b.md"))
         (session (mevedel-skills-test--make-session))
         (skill-a (mevedel-skill--create
                   :name "alpha" :body "A" :source-file source-a))
         (skill-b (mevedel-skill--create
                   :name "alpha" :body "B" :source-file source-b))
         (input (copy-sequence "Use $alpha then $alpha")))
    (unwind-protect
        (progn
          (with-temp-file source-a (insert "A"))
          (with-temp-file source-b (insert "B"))
          (setf (mevedel-session-skills session) (list skill-a skill-b))
          (mevedel-mention-bindings-set
           4 10
           (list :kind 'skill :token "$alpha" :source-file source-a)
           input)
          (mevedel-mention-bindings-set
           16 22
           (list :kind 'skill :token "$alpha" :source-file source-b)
           input)
          (cl-letf (((symbol-function 'mevedel-skills-ensure-fresh)
                     (lambda (&rest _) nil)))
            (should (equal (list source-a source-b)
                           (mapcar
                            (lambda (mention)
                              (plist-get mention :source-file))
                            (mevedel-skills-input--inline-skill-mentions
                             input session))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-skills-input-scan-tokens ()
  ,test
  (test)
  :doc "centralizes root, inline, quote, escape, and code token classification"
  (let ((lookup (lambda (name _start _end)
                  (and (equal name "alpha") 'skill))))
    (should
     (equal '((:start 33 :end 39 :name "alpha" :value skill))
            (mevedel-skills-input-scan-tokens
             "$alpha \\$alpha `$alpha` \"$alpha\" $alpha" lookup)))
    (should (= 2 (length (mevedel-skills-input-scan-tokens
                          "$alpha then $alpha" lookup t)))))

  :doc "a bound shorter name wins before punctuation-like name lookup"
  (let* ((source (make-temp-file "mevedel-bound-scan-" nil ".md"))
         (input (copy-sequence "use $alpha."))
         (start (string-match "\\$alpha" input))
         (resolver
          (lambda (name token-start token-end)
            (let ((binding (mevedel-mention-bindings-starting-at
                            input token-start)))
              (if binding
                  (and (mevedel-mention-bindings-at
                        input token-start token-end 'skill (concat "$" name))
                       'bound-alpha)
                (and (equal name "alpha.") 'wrong-alpha-dot))))))
    (unwind-protect
        (progn
          (mevedel-mention-bindings-set
           start (+ start 6)
           (list :kind 'skill :token "$alpha" :source-file source)
           input)
          (let ((tokens (mevedel-skills-input-scan-tokens
                         input resolver)))
            (should (equal '(bound-alpha)
                           (mapcar (lambda (token)
                                     (plist-get token :value))
                                   tokens)))))
      (delete-file source))))

(mevedel-deftest mevedel-skills-input-resolve-mention ()
  ,test
  (test)
  :doc "bound mentions resolve only through their exact canonical source"
  (let* ((root (make-temp-file "mevedel-resolve-bound-" t))
         (source-a (file-name-concat root "a.md"))
         (source-b (file-name-concat root "b.md"))
         (skill-a (mevedel-skill--create
                   :name "alpha" :source-file source-a))
         (skill-b (mevedel-skill--create
                   :name "alpha" :source-file source-b))
         (session (mevedel-skills-test--make-session))
         (input (copy-sequence "$alpha")))
    (unwind-protect
        (progn
          (with-temp-file source-a (insert "A"))
          (with-temp-file source-b (insert "B"))
          (setf (mevedel-session-skills session) (list skill-a skill-b))
          (mevedel-mention-bindings-set
           0 6
           (list :kind 'skill :token "$alpha" :source-file source-b)
           input)
          (cl-letf (((symbol-function 'mevedel-skills-ensure-fresh)
                     (lambda (&rest _) nil)))
            (let ((outcome (mevedel-skills-input-resolve-mention
                            input session 0 6 "alpha")))
              (should (eq 'ok (plist-get outcome :status)))
              (should (eq skill-b (plist-get outcome :skill))))))
      (delete-directory root t)))

  :doc "bound skill resolution consults the canonical source locator"
  (let* ((root (make-temp-file "mevedel-resolve-resource-" t))
         (source (file-name-concat root "skill.md"))
         (skill (mevedel-skill--create :name "alpha" :source-file source))
         (session (mevedel-skills-test--make-session))
         (input (copy-sequence "$alpha"))
         seen)
    (unwind-protect
        (progn
          (with-temp-file source (insert "body"))
          (setf (mevedel-session-skills session) (list skill))
          (mevedel-mention-bindings-set
           0 6
           (list :kind 'skill :token "$alpha" :source-file source)
           input)
          (cl-letf (((symbol-function 'mevedel-resource-prepare)
                     (lambda (operation address context)
                       (setq seen (list operation address context))
                       'resource-attempt))
                    ((symbol-function 'mevedel-resource-execute)
                     (lambda (attempt executor)
                       (should (eq attempt 'resource-attempt))
                       (funcall executor source "skill-address")))
                    ((symbol-function 'mevedel-skills-ensure-fresh)
                     (lambda (&rest _) nil)))
            (let ((outcome (mevedel-skills-input-resolve-mention
                            input session 0 6 "alpha")))
              (should (eq 'ok (plist-get outcome :status)))
              (should (eq skill (plist-get outcome :skill)))
              (should (equal
                       (list 'read
                             (format "skill://alpha@%s"
                                     (mevedel-resource-skill-digest source))
                             (list :session session))
                       seen)))))
      (delete-directory root t)))

  :doc "a missing bound source is unavailable without same-name fallback"
  (let* ((root (make-temp-file "mevedel-resolve-missing-" t))
         (source (file-name-concat root "live.md"))
         (missing (file-name-concat root "missing.md"))
         (skill (mevedel-skill--create :name "alpha" :source-file source))
         (session (mevedel-skills-test--make-session))
         (input (copy-sequence "$alpha")))
    (unwind-protect
        (progn
          (with-temp-file source (insert "live"))
          (setf (mevedel-session-skills session) (list skill))
          (mevedel-mention-bindings-set
           0 6
           (list :kind 'skill :token "$alpha" :source-file missing)
           input)
          (cl-letf (((symbol-function 'mevedel-skills-ensure-fresh)
                     (lambda (&rest _) nil)))
            (let ((outcome (mevedel-skills-input-resolve-mention
                            input session 0 6 "alpha")))
              (should (eq 'unavailable (plist-get outcome :status)))
              (should-not (plist-get outcome :skill)))))
      (delete-directory root t)))

  :doc "unknown unbound names remain literal successful text"
  (let ((outcome (mevedel-skills-input-resolve-mention
                  "$unknown" (mevedel-skills-test--make-session)
                  0 8 "unknown")))
    (should (eq 'ok (plist-get outcome :status)))
    (should-not (plist-get outcome :skill))))

(mevedel-deftest mevedel-skills-input-refresh-bound-input ()
  ,test
  (test)
  :doc "rescans only bound input and exposes the latest exact source body"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-refresh-bound-" t))
         (skill-dir (file-name-concat root ".mevedel/skills"))
         (mevedel-skill-dirs '(".mevedel/skills"))
         (ws (mevedel-workspace--create
              :type 'file :id root :root root :name "refresh-bound"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         source
         bound)
    (unwind-protect
        (progn
          (setq source
                (mevedel-skills-test--write-skill
                 skill-dir "alpha"
                 "name: alpha\ndescription: Alpha\ncontext: inline\n"
                 "ALPHA V1"))
          (with-temp-buffer
            (mevedel-skills-install session (current-buffer))
            (let ((installed (mevedel-session-skills session)))
              (mevedel-skills-input-refresh-bound-input "plain text" session)
              (should (eq installed (mevedel-session-skills session))))
            (setq bound (copy-sequence "use $alpha"))
            (mevedel-mention-bindings-set
             4 10
             (list :kind 'skill :token "$alpha" :source-file source)
             bound)
            (mevedel-skills-test--write-skill
             skill-dir "alpha"
             "name: alpha\ndescription: Alpha\ncontext: inline\n"
             "ALPHA V2")
            (should (eq bound
                        (mevedel-skills-input-refresh-bound-input bound session)))
            (should (equal
                     "ALPHA V2"
                     (mevedel-skill-load-body
                      (mevedel-session-get-skill-by-source
                       session source))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-skills-input-clear-pending ()
  ,test
  (test)
  :doc "clears both input- and invocation-owned pending request state"
  (with-temp-buffer
    (setq-local mevedel-skills-input--pending-inline-attachments
                '((:name "alpha")))
    (setq-local mevedel-skills--pending-request-context
                '(:model fast))
    (mevedel-skills-input-clear-pending)
    (should-not mevedel-skills-input--pending-inline-attachments)
    (should-not mevedel-skills--pending-request-context)))

(mevedel-deftest mevedel-skills-input-dispatch-inline-attachments (:quiet t)
  ,test
  (test)
  :doc "prepares distinct inline skills, appends metadata, and continues"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "alpha" :body "Alpha body")))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (setq-local mevedel-skills--pending-request-context nil)
      (insert "Please use $alpha and $alpha here")
      (let (continued)
        (cl-letf (((symbol-function 'mevedel-skills-ensure-fresh)
                   (lambda (&rest _) nil)))
          (should (eq 'skill
                      (mevedel-skills-input-dispatch-inline-attachments
                       (lambda () (setq continued t))))))
        (should continued)
        (should (= 1 (length mevedel-skills-input--pending-inline-attachments)))
        (should (equal "Alpha body"
                       (plist-get
                        (car mevedel-skills-input--pending-inline-attachments)
                        :body)))
        (should (string-search "inline-skill-attachments"
                               (buffer-string)))
        (should (= 1 (length (plist-get
                              mevedel-skills--pending-request-context
                              :invoked-skills)))))))

  :doc "unavailable inline skills stage an annotation and continue"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create :name "alpha" :body "Alpha body")))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (insert "Please use $alpha here")
      (let (continued)
        (cl-letf (((symbol-function 'mevedel-skills-skill-enabled-p)
                   (lambda (_skill) nil)))
          (should (eq 'skill
                      (mevedel-skills-input-dispatch-inline-attachments
                       (lambda () (setq continued t))))))
        (should continued)
        (should (plist-get
                 (car mevedel-skills-input--pending-inline-attachments)
                 :unavailable)))))

  :doc "raw fork-context attachment is an instruction and ignores policy"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "review" :body "Review body" :context 'fork
                 :model "invalid-selector" :effort 'impossible
                 :hooks '((PreToolUse nil)))))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (setq-local mevedel-skills--pending-request-context nil)
      (insert "Please use $review here")
      (let (continued)
        (cl-letf (((symbol-function 'mevedel-skills-ensure-fresh)
                   (lambda (&rest _) nil))
                  ((symbol-function 'mevedel-agent-control-spawn)
                   (lambda (&rest _) (ert-fail "instruction forked"))))
          (should (eq 'skill
                      (mevedel-skills-input-dispatch-inline-attachments
                       (lambda () (setq continued t)))))
          (should continued))
        (should (= 1 (length mevedel-skills-input--pending-inline-attachments)))
        (should-not (plist-member mevedel-skills--pending-request-context
                                  :model))
        (should-not (plist-member mevedel-skills--pending-request-context
                                  :effort))
        (should-not (plist-member mevedel-skills--pending-request-context
                                  :hook-rules))
        (should (= 1 (length (plist-get
                              mevedel-skills--pending-request-context
                              :invoked-skills))))))))

(defun test-mevedel-skills-input--staged-bodies (fsm)
  "Return and clear reminder bodies staged on FSM."
  (let* ((info (gptel-fsm-info fsm))
         (entries (plist-get info :mevedel-reminder-entries)))
    (setf (gptel-fsm-info fsm)
          (plist-put info :mevedel-reminder-entries nil))
    (mapcar (lambda (entry) (plist-get entry :body)) entries)))

(mevedel-deftest mevedel-skills-input-transform-inline-attachments ()
  ,test
  (test)
  :doc "replaces live mentions with placeholders and stages reminders"
  (let* ((session (mevedel-skills-test--make-session))
         (chat (generate-new-buffer " *mevedel-inline-skill-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel-skills-input--pending-inline-attachments
                        (list (list :name "alpha" :body "Alpha body")
                              (list :name "beta" :body "Beta body"))))
          (with-temp-buffer
            (insert (propertize
                     "Use $alpha and \"$alpha\", `$beta`, then $beta."
                     'gptel 'prompt))
            (mevedel-skills-input-transform-inline-attachments fsm)
            (let ((text (buffer-string)))
              (should (string-match-p
                       "\\[skill:alpha -- attached\\]" text))
              (should (string-match-p
                       "\\[skill:beta -- attached\\]" text))
              (should (string-match-p (regexp-quote "\"$alpha\"") text))
              (should (string-match-p (regexp-quote "`$beta`") text))
              ;; Bodies ride the staged reminder entries, not the text.
              (should-not (string-match-p "Alpha body" text))
              (should-not (string-match-p "<system-reminder>" text)))
            (goto-char (point-min))
            (search-forward "Use [skill:alpha -- attached]")
            (let ((prompt-start (match-beginning 0)))
              (should (= prompt-start
                         (mevedel-transcript-prompt-transform-start))))
            (should (eq 'prompt
                        (get-text-property
                         (+ (match-beginning 0) (length "Use "))
                         'gptel))))
          (let ((bodies (test-mevedel-skills-input--staged-bodies fsm)))
            (should (= 2 (length bodies)))
            (should (string-match-p "host already attached the skill"
                                    (car bodies)))
            (should (string-match-p "must follow it without calling `Skill`"
                                    (car bodies)))
            (should (string-match-p "Alpha body" (car bodies)))
            (should (string-match-p "Beta body" (cadr bodies))))
          (with-current-buffer chat
            (should-not mevedel-skills-input--pending-inline-attachments)))
      (kill-buffer chat)))

  :doc "same-named bound skills retain distinct canonical sources"
  (let* ((session (mevedel-skills-test--make-session))
         (chat (generate-new-buffer " *mevedel-inline-skill-source-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat)))
         (source-a "/tmp/source-a/SKILL.md")
         (source-b "/tmp/source-b/SKILL.md")
         (input (propertize "Use $alpha then $alpha." 'gptel 'prompt)))
    (mevedel-mention-bindings-set
     4 10 (list :kind 'skill :token "$alpha" :source-file source-a) input)
    (mevedel-mention-bindings-set
     16 22 (list :kind 'skill :token "$alpha" :source-file source-b) input)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local
             mevedel-skills-input--pending-inline-attachments
             (list (list :name "alpha" :source-file source-a :body "Body A")
                   (list :name "alpha" :source-file source-b :body "Body B"))))
          (with-temp-buffer
            (insert input)
            (mevedel-skills-input-transform-inline-attachments fsm)
            (should (= 2 (how-many "\\[skill:alpha -- attached\\]"
                                   (point-min) (point-max)))))
          (let ((bodies (test-mevedel-skills-input--staged-bodies fsm)))
            (should (= 2 (length bodies)))
            (should (string-match-p "Body A" (car bodies)))
            (should (string-match-p "Body B" (cadr bodies)))))
      (kill-buffer chat)))

  :doc "coexists with earlier mention expansion reminders"
  (let* ((root (make-temp-file "mevedel-inline-skill-mentions-" t))
         (file (file-name-concat root "notes.txt"))
         (ws (mevedel-skills-test--make-workspace root))
         (session (mevedel-session-create "main" ws))
         (chat (generate-new-buffer " *mevedel-inline-skill-mention-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "Mention body\n"))
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel-skills-input--pending-inline-attachments
                        (list (list :name "alpha" :body "Alpha body"))))
          (with-temp-buffer
            (insert (propertize
                     (format "Read @file:%s and use $alpha." file)
                     'gptel 'prompt))
            (mevedel--transform-expand-mentions fsm)
            (mevedel-skills-input-transform-inline-attachments fsm)
            (let ((text (buffer-string)))
              (should (string-match-p
                       "\\[file:.* -- contents attached above\\]" text))
              (should (string-match-p
                       "\\[skill:alpha -- attached\\]" text))
              (should-not (string-match-p "<system-reminder>" text))))
          (let ((bodies (test-mevedel-skills-input--staged-bodies fsm)))
            (should (= 2 (length bodies)))
            (should (string-match-p "Mention body" (car bodies)))
            (should (string-match-p "Alpha body" (cadr bodies)))))
      (kill-buffer chat)
      (delete-directory root t)))

  :doc "does not rewrite earlier system-reminder bodies"
  (let* ((session (mevedel-skills-test--make-session))
         (chat (generate-new-buffer " *mevedel-inline-skill-reminder-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel-skills-input--pending-inline-attachments
                        (list (list :name "alpha" :body "Alpha body"))))
          (with-temp-buffer
            (insert "<system-reminder>\n"
                    "Reminder text keeps $alpha literal.\n"
                    "</system-reminder>\n\n"
                    "User prompt uses $alpha.")
            (mevedel-skills-input-transform-inline-attachments fsm)
            (let ((text (buffer-string)))
              (should (string-match-p
                       (regexp-quote "Reminder text keeps $alpha literal.")
                       text))
              (should (= 1 (how-many "\\[skill:alpha -- attached\\]"
                                     (point-min) (point-max))))))
          (should (string-match-p
                   "Alpha body"
                   (car (test-mevedel-skills-input--staged-bodies fsm)))))
      (kill-buffer chat)))

  :doc "replaces unavailable mentions without injecting instructions"
  (let* ((session (mevedel-skills-test--make-session))
         (chat (generate-new-buffer " *mevedel-inline-unavailable-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local mevedel--session session)
            (setq-local mevedel-skills-input--pending-inline-attachments
                        '((:name "alpha" :unavailable t
                                 :message "skill $alpha is disabled"))))
          (with-temp-buffer
            (insert (propertize "Use $alpha here" 'gptel 'prompt))
            (mevedel-skills-input-transform-inline-attachments fsm)
            (should (string-match-p
                     (regexp-quote "[skill:alpha -- unavailable]")
                     (buffer-string)))
            (should-not (string-match-p "<system-reminder>"
                                        (buffer-string)))))
      (kill-buffer chat))))

(mevedel-deftest mevedel-skills-input-dispatch-command (:quiet t)
  ,test
  (test)
  :doc "skill expansion inserts prepared body and returns 'skill"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "greet"
                 :body "Hello $0!")))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### $greet world")
        (goto-char (point-max))
        (should (eq 'skill (mevedel-skills-input-dispatch-command)))
        (should (equal "### Hello world!"
                       (mevedel-tool-render-data-strip
                        (buffer-string))))
        (should (string-search "<!-- mevedel-render-data -->"
                               (buffer-string)))
        (let ((data (cdr (mevedel-tool-render-data-extract
                          (buffer-string)))))
          (should (equal "Hello world!"
                         (plist-get data :expanded-prompt)))))))

  :doc "a draft shortened during preparation does not break the dispatch"
  ;; Preparation is asynchronous whenever the body injects, and the range to
  ;; replace is captured before it starts.  If the draft shrinks meanwhile,
  ;; that captured end is past the end of the buffer.
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "greet"
                 :body "Hello !el`(+ 1 2)`"))
         (pending nil))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (cl-letf (((symbol-function 'mevedel-pipeline-run-tool)
                   (lambda (_tool callback &rest _)
                     (setq pending callback))))
          (insert "### $greet world and more text here")
          (goto-char (point-max))
          (mevedel-skills-input-dispatch-command)
          (should pending)
          ;; The user trims the draft while the injection is still running.
          (delete-region (- (point-max) 20) (point-max)))
        (funcall pending "3")
        (should (stringp (buffer-string))))))

  :doc "a draft replaced during preparation is left alone and not sent"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "greet"
                 :body "Hello !el`(+ 1 2)`"))
         (pending nil)
         (sent nil))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (cl-letf (((symbol-function 'mevedel-pipeline-run-tool)
                   (lambda (_tool callback &rest _)
                     (setq pending callback))))
          (insert "### $greet world")
          (goto-char (point-max))
          (mevedel-skills-input-dispatch-command
           (lambda () (setq sent t)))
          (should pending)
          ;; The user rewrites the command into something else entirely.
          (delete-region (point-min) (point-max))
          (insert "### never mind"))
        (funcall pending "3")
        ;; The prepared body belongs to arguments that no longer exist.
        (should (equal "### never mind" (buffer-string)))
        (should-not sent)
        (should-not mevedel-skills--pending-request-context))))

  :doc "a same-length edit to the command is not treated as the same command"
  ;; The corruption half: an edit that keeps the length still moves what the
  ;; captured range covers, so the old code replaced the wrong text and sent
  ;; a body prepared for arguments the draft no longer names.
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "greet"
                 :body "Hello !el`(+ 1 2)`"))
         (pending nil)
         (sent nil))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (cl-letf (((symbol-function 'mevedel-pipeline-run-tool)
                   (lambda (_tool callback &rest _)
                     (setq pending callback))))
          (insert "### $greet world")
          (goto-char (point-max))
          (mevedel-skills-input-dispatch-command
           (lambda () (setq sent t)))
          (should pending)
          ;; Same length, different argument.
          (goto-char (- (point-max) 5))
          (delete-char 5)
          (insert "there"))
        (funcall pending "3")
        (should (equal "### $greet there" (buffer-string)))
        (should-not sent)
        (should-not mevedel-skills--pending-request-context))))

  :doc "user-invocable: false leading skill falls through to inline planning"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "internal-only"
                 :body "ignored"
                 :user-invocable-p nil)))
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (insert "### $internal-only")
        (goto-char (point-max))
        (should-not (mevedel-skills-input-dispatch-command))
        (should (string-match-p "\\$internal-only" (buffer-string)))
        (should-not mevedel-skills-input--pending-inline-attachments))))

  :doc "unknown dollar command is left for normal sending"
  (let ((session (mevedel-skills-test--make-session)))
    (mevedel-skills-test--with-chat-buffer session
      (insert "### $PATH is useful context")
      (goto-char (point-max))
      (should (null (mevedel-skills-input-dispatch-command)))
      (should (equal "### $PATH is useful context" (buffer-string)))))

  :doc "fork-context skill dispatches directly and inserts result"
  (let* ((session (mevedel-skills-test--make-session))
         (agent (mevedel-agent--create :name "explorer"))
         (skill (mevedel-skill--create
                 :name "delegate"
                 :body "should-not-appear"
                 :context 'fork
                 :agent "explorer"))
         save-called status-called stop-called
         baseline-recorded permission-restored
         queue-drain-scheduled post-hook-called)
    (setf (mevedel-session-skills session) (list skill))
    (mevedel-skills-test--with-chat-buffer session
      (let ((mevedel-slash-commands nil))
        (cl-letf (((symbol-function 'mevedel-agent-get)
                   (lambda (n) (and (equal n "explorer") agent)))
                  ((symbol-function 'mevedel-agent-control-spawn)
                   (lambda (_session _task _message callback &rest keys)
                     (let ((record
                            (mevedel-agent-record--create
                             :path "/root/skill_delegate" :activity 'idle)))
                       (funcall callback
                                (list :outcome 'success :record record)))
                     (funcall
                      (plist-get keys :result-handler)
                      '(:type RESULT :sender "/root/skill_delegate"
                              :recipient "/root" :outcome completed
                              :payload "agent finished"))
                     #'ignore))
                  ((symbol-function 'mevedel-session-artifacts-save)
                   (lambda (session buffer &optional settled)
                     (setq save-called (list session buffer settled))
                     "saved"))
                  ((symbol-function 'mevedel-compact-estimation-record-token-baseline)
                   (lambda (_fsm) (setq baseline-recorded t)))
                  ((symbol-function 'mevedel--run-turn-terminal-hook)
                   (lambda (_fsm event status)
                     (setq stop-called
                           (list event status
                                 (not
                                  (null
                                   (bound-and-true-p
                                    mevedel--current-request)))))))
                  ((symbol-function
                    'mevedel--implementation-permission-mode-restore)
                   (lambda () (setq permission-restored t)))
                  ((symbol-function
                    'mevedel-view--schedule-follow-up-drain)
                   (lambda (_fsm) (setq queue-drain-scheduled t)))
                  ((symbol-function 'gptel--update-status)
                   (lambda (&rest args) (setq status-called args))))
          (let ((gptel-response-separator "\n\n")
                (gptel-post-response-functions
                 (list (lambda (_start _end)
                         (setq post-hook-called t)
                         (error "Broken post-response hook")))))
            (insert "### $delegate do the thing")
            (goto-char (point-max))
            (should (eq 'skill (mevedel-skills-input-dispatch-command)))
            (let ((buf (buffer-string)))
              (should (string-match-p "\\$delegate do the thing" buf))
              (should (string-match-p "agent finished" buf))
              (should-not (string-match-p "should-not-appear" buf)))
            (should-not mevedel--current-request)
            (should (= 1 (mevedel-session-turn-count session)))
            (should (equal (list session (current-buffer) t) save-called))
            (should (equal '(Stop completed t) stop-called))
            (should baseline-recorded)
            (should permission-restored)
            (should queue-drain-scheduled)
            (should post-hook-called)
            (should (equal '(" Ready" success) status-called)))))))

  :doc "no-prefix chat: skill body is placed on a fresh line with blank separator"
  (let* ((session (mevedel-skills-test--make-session))
         (skill (mevedel-skill--create
                 :name "greet"
                 :body "Hello $0!")))
    (setf (mevedel-session-skills session) (list skill))
    (with-temp-buffer
      (setq mevedel--session session)
      (let ((mevedel-slash-commands nil))
        (insert "Old response")
        (put-text-property (point-min) (point-max) 'gptel 'response)
        (insert "\n$greet world")
        (goto-char (point-max))
        (should (eq 'skill (mevedel-skills-input-dispatch-command)))
        (should (equal "Old response\n\nHello world!"
                       (mevedel-tool-render-data-strip
                        (buffer-string))))))))


(provide 'test-mevedel-skills-input)
;;; test-mevedel-skills-input.el ends here
