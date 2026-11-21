;;; test-niwa-skills-input.el -- Skill input tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests user skill parsing, binding, projection, and dispatch.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel)
(require 'niwa-agent-control)
(require 'niwa-compact-estimation)
(require 'niwa-file-state)
(require 'niwa-mention-bindings)
(require 'niwa-mentions)
(require 'niwa-pending-inputs)
(require 'niwa-pipeline)
(require 'niwa-resource)
(require 'niwa-session-artifacts)
(require 'niwa-session-persistence)
(require 'niwa-skills-core)
(require 'niwa-skills-input)
(require 'niwa-skills-invoke)
(require 'niwa-structs)
(require 'niwa-transcript)
(require 'niwa-tool-render-data)
(require 'niwa-workspace)

(niwa-deftest niwa-skills-input-current-prompt-region ()
  ,test
  (test)
  :doc "uses the `gptel' text property to find the boundary after a response"
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (insert "response body")
      (put-text-property (point-min) (point-max) 'gptel 'response)
      (insert "/help")
      (goto-char (point-max))
      (let ((region (niwa-skills-input-current-prompt-region)))
        (should region)
        (should (equal "/help"
                       (buffer-substring-no-properties
                        (car region) (cdr region)))))))

  :doc "without a `gptel' property the whole buffer is the pending prompt"
  (let ((session (niwa-skills-test--make-session)))
    (with-temp-buffer
      (setq niwa--session session)
      (insert "/help alone")
      (goto-char (point-max))
      (let ((region (niwa-skills-input-current-prompt-region)))
        (should region)
        (should (equal "/help alone"
                       (buffer-substring-no-properties
                        (car region) (cdr region)))))))

  :doc "empty buffer returns nil"
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (should (null (niwa-skills-input-current-prompt-region))))))

(niwa-deftest niwa-skills-input-command-delete-start ()
  ,test
  (test)
  :doc "a command preceded only by indentation deletes from the line start"
  (with-temp-buffer
    (insert "Response\n  /help")
    (should (= (line-beginning-position)
               (niwa-skills-input-command-delete-start (- (point) 5)))))
  :doc "a command after other text on its line deletes from the command"
  (with-temp-buffer
    (insert "please /help")
    (should (= 8 (niwa-skills-input-command-delete-start 8)))))


;;
;;; Leading and inline invocation bridges

(niwa-deftest niwa-skills-input-parse-skill-line ()
  ,test
  (test)
  :doc "plain `$skill' parses to (name \"\" 0)"
  (should (equal '("greet" "" 0)
                 (niwa-skills-input-parse-skill-line "$greet")))

  :doc "`$skill args' parses to (name args 0)"
  (should (equal '("greet" "world" 0)
                 (niwa-skills-input-parse-skill-line "$greet world")))

  :doc "plugin-prefixed skill names parse as one skill name"
  (should (equal '("superpowers:brainstorming" "now" 0)
                 (niwa-skills-input-parse-skill-line
                  "$superpowers:brainstorming now")))

  :doc "additional lines after the skill name are appended to ARGS"
  (should (equal '("delegate"
                   "Launch three explorer agents:\n  (a) ...\n  (b) ..."
                   0)
                 (niwa-skills-input-parse-skill-line
                  "$delegate Launch three explorer agents:
  (a) ...
  (b) ...")))

  :doc "multi-line ARGS work even when no first-line arguments"
  (should (equal '("delegate"
                   "Multi-line task body\nspanning lines"
                   0)
                 (niwa-skills-input-parse-skill-line
                  "$delegate
Multi-line task body
spanning lines")))

  :doc "text not starting with `$' returns nil"
  (should (null (niwa-skills-input-parse-skill-line "hello $greet")))
  (should (null (niwa-skills-input-parse-skill-line "/greet")))

  :doc "non-identifier skill names are rejected"
  (should (null (niwa-skills-input-parse-skill-line "$hi!")))
  (should (null (niwa-skills-input-parse-skill-line "$")))

  :doc "leading whitespace is reported via offset"
  (should (equal '("greet" "" 3)
                 (niwa-skills-input-parse-skill-line "   $greet"))))

(niwa-deftest niwa-skills-input--inline-skill-mentions ()
  ,test
  (test)
  :doc "finds inline skills, dedupes, and skips literal forms"
  (let* ((session (niwa-skills-test--make-session))
         (alpha (niwa-skill--create :name "alpha" :body "A"))
         (beta (niwa-skill--create :name "beta" :body "B")))
    (setf (niwa-session-skills session) (list alpha beta))
    (cl-letf (((symbol-function 'niwa-skills-ensure-fresh)
               (lambda (&rest _) nil)))
      (let ((mentions
             (niwa-skills-input--inline-skill-mentions
              "Use $alpha, \"$beta\", '$beta', \\$beta, `$beta`, $alpha, and $beta."
              session)))
        (should (equal '("alpha" "beta")
                       (mapcar (lambda (item) (plist-get item :name))
                               mentions))))))

  :doc "single-backtick code spans and quoted phrases stay literal"
  (let* ((session (niwa-skills-test--make-session))
         (alpha (niwa-skill--create :name "alpha" :body "A"))
         (beta (niwa-skill--create :name "beta" :body "B")))
    (setf (niwa-session-skills session) (list alpha beta))
    (cl-letf (((symbol-function 'niwa-skills-ensure-fresh)
               (lambda (&rest _) nil)))
      (should-not
       (niwa-skills-input--inline-skill-mentions
        "Use `$beta` here and say \"please $alpha\"." session))
      (should-not
       (niwa-skills-input--inline-skill-mentions
        "Use '$beta.' here and \"please $alpha.\"." session))))

  :doc "leading command-style skill is not an inline mention"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create :name "alpha" :body "A")))
    (setf (niwa-session-skills session) (list skill))
    (cl-letf (((symbol-function 'niwa-skills-ensure-fresh)
               (lambda (&rest _) nil)))
      (should-not
       (niwa-skills-input--inline-skill-mentions "$alpha run this"
                                                    session))))

  :doc "inline mention scan ignores unknown dollar text"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create :name "alpha" :body "A")))
    (setf (niwa-session-skills session) (list skill))
    (cl-letf (((symbol-function 'niwa-skills-ensure-fresh)
               (lambda (&rest _) nil)))
      (should-not
       (niwa-skills-input--inline-skill-mentions
        "Keep $PATH literal" session))
      (should
       (niwa-skills-input--inline-skill-mentions
        "Use $alpha here" session))))

  :doc "known fork skill becomes a non-forking instruction mention"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "review" :body "R" :context 'fork)))
    (setf (niwa-session-skills session) (list skill))
    (cl-letf (((symbol-function 'niwa-skills-ensure-fresh)
               (lambda (&rest _) nil)))
      (let ((result (niwa-skills-input--inline-skill-mentions
                     "Please use $review here" session)))
        (should (equal '("review")
                       (mapcar (lambda (item) (plist-get item :name))
                               result))))))

  :doc "known disabled skill becomes an unavailable inline occurrence"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create :name "disabled" :body "D")))
    (setf (niwa-session-skills session) (list skill))
    (cl-letf (((symbol-function 'niwa-skills-ensure-fresh)
               (lambda (&rest _) nil))
              ((symbol-function 'niwa-skills-skill-enabled-p)
               (lambda (_skill) nil)))
      (let ((result (niwa-skills-input--inline-skill-mentions
                     "Please use $disabled here" session)))
        (should (equal "disabled" (plist-get (car result) :name)))
        (should (plist-get (car result) :unavailable))
        (should (string-match-p "disabled"
                                (plist-get (car result) :message))))))

  :doc "same-named bindings preserve distinct canonical sources"
  (let* ((root (make-temp-file "niwa-inline-sources-" t))
         (source-a (file-name-concat root "a.md"))
         (source-b (file-name-concat root "b.md"))
         (session (niwa-skills-test--make-session))
         (skill-a (niwa-skill--create
                   :name "alpha" :body "A" :source-file source-a))
         (skill-b (niwa-skill--create
                   :name "alpha" :body "B" :source-file source-b))
         (input (copy-sequence "Use $alpha then $alpha")))
    (unwind-protect
        (progn
          (with-temp-file source-a (insert "A"))
          (with-temp-file source-b (insert "B"))
          (setf (niwa-session-skills session) (list skill-a skill-b))
          (niwa-mention-bindings-set
           4 10
           (list :kind 'skill :token "$alpha" :source-file source-a)
           input)
          (niwa-mention-bindings-set
           16 22
           (list :kind 'skill :token "$alpha" :source-file source-b)
           input)
          (cl-letf (((symbol-function 'niwa-skills-ensure-fresh)
                     (lambda (&rest _) nil)))
            (should (equal (list source-a source-b)
                           (mapcar
                            (lambda (mention)
                              (plist-get mention :source-file))
                            (niwa-skills-input--inline-skill-mentions
                             input session))))))
      (delete-directory root t))))

(niwa-deftest niwa-skills-input-scan-tokens ()
  ,test
  (test)
  :doc "centralizes root, inline, quote, escape, and code token classification"
  (let ((lookup (lambda (name _start _end)
                  (and (equal name "alpha") 'skill))))
    (should
     (equal '((:start 33 :end 39 :name "alpha" :value skill))
            (niwa-skills-input-scan-tokens
             "$alpha \\$alpha `$alpha` \"$alpha\" $alpha" lookup)))
    (should (= 2 (length (niwa-skills-input-scan-tokens
                          "$alpha then $alpha" lookup t)))))

  :doc "a bound shorter name wins before punctuation-like name lookup"
  (let* ((source (make-temp-file "niwa-bound-scan-" nil ".md"))
         (input (copy-sequence "use $alpha."))
         (start (string-match "\\$alpha" input))
         (resolver
          (lambda (name token-start token-end)
            (let ((binding (niwa-mention-bindings-starting-at
                            input token-start)))
              (if binding
                  (and (niwa-mention-bindings-at
                        input token-start token-end 'skill (concat "$" name))
                       'bound-alpha)
                (and (equal name "alpha.") 'wrong-alpha-dot))))))
    (unwind-protect
        (progn
          (niwa-mention-bindings-set
           start (+ start 6)
           (list :kind 'skill :token "$alpha" :source-file source)
           input)
          (let ((tokens (niwa-skills-input-scan-tokens
                         input resolver)))
            (should (equal '(bound-alpha)
                           (mapcar (lambda (token)
                                     (plist-get token :value))
                                   tokens)))))
      (delete-file source))))

(niwa-deftest niwa-skills-input-resolve-mention ()
  ,test
  (test)
  :doc "bound mentions resolve only through their exact canonical source"
  (let* ((root (make-temp-file "niwa-resolve-bound-" t))
         (source-a (file-name-concat root "a.md"))
         (source-b (file-name-concat root "b.md"))
         (skill-a (niwa-skill--create
                   :name "alpha" :source-file source-a))
         (skill-b (niwa-skill--create
                   :name "alpha" :source-file source-b))
         (session (niwa-skills-test--make-session))
         (input (copy-sequence "$alpha")))
    (unwind-protect
        (progn
          (with-temp-file source-a (insert "A"))
          (with-temp-file source-b (insert "B"))
          (setf (niwa-session-skills session) (list skill-a skill-b))
          (niwa-mention-bindings-set
           0 6
           (list :kind 'skill :token "$alpha" :source-file source-b)
           input)
          (cl-letf (((symbol-function 'niwa-skills-ensure-fresh)
                     (lambda (&rest _) nil)))
            (let ((outcome (niwa-skills-input-resolve-mention
                            input session 0 6 "alpha")))
              (should (eq 'ok (plist-get outcome :status)))
              (should (eq skill-b (plist-get outcome :skill))))))
      (delete-directory root t)))

  :doc "bound skill resolution consults the canonical source locator"
  (let* ((root (make-temp-file "niwa-resolve-resource-" t))
         (source (file-name-concat root "skill.md"))
         (skill (niwa-skill--create :name "alpha" :source-file source))
         (session (niwa-skills-test--make-session))
         (input (copy-sequence "$alpha"))
         seen)
    (unwind-protect
        (progn
          (with-temp-file source (insert "body"))
          (setf (niwa-session-skills session) (list skill))
          (niwa-mention-bindings-set
           0 6
           (list :kind 'skill :token "$alpha" :source-file source)
           input)
          (cl-letf (((symbol-function 'niwa-resource-prepare)
                     (lambda (operation address context)
                       (setq seen (list operation address context))
                       'resource-attempt))
                    ((symbol-function 'niwa-resource-execute)
                     (lambda (attempt executor)
                       (should (eq attempt 'resource-attempt))
                       (funcall executor source "skill-address")))
                    ((symbol-function 'niwa-skills-ensure-fresh)
                     (lambda (&rest _) nil)))
            (let ((outcome (niwa-skills-input-resolve-mention
                            input session 0 6 "alpha")))
              (should (eq 'ok (plist-get outcome :status)))
              (should (eq skill (plist-get outcome :skill)))
              (should (equal
                       (list 'read
                             (format "skill://alpha@%s"
                                     (niwa-resource-skill-digest source))
                             (list :session session))
                       seen)))))
      (delete-directory root t)))

  :doc "a missing bound source is unavailable without same-name fallback"
  (let* ((root (make-temp-file "niwa-resolve-missing-" t))
         (source (file-name-concat root "live.md"))
         (missing (file-name-concat root "missing.md"))
         (skill (niwa-skill--create :name "alpha" :source-file source))
         (session (niwa-skills-test--make-session))
         (input (copy-sequence "$alpha")))
    (unwind-protect
        (progn
          (with-temp-file source (insert "live"))
          (setf (niwa-session-skills session) (list skill))
          (niwa-mention-bindings-set
           0 6
           (list :kind 'skill :token "$alpha" :source-file missing)
           input)
          (cl-letf (((symbol-function 'niwa-skills-ensure-fresh)
                     (lambda (&rest _) nil)))
            (let ((outcome (niwa-skills-input-resolve-mention
                            input session 0 6 "alpha")))
              (should (eq 'unavailable (plist-get outcome :status)))
              (should-not (plist-get outcome :skill)))))
      (delete-directory root t)))

  :doc "unknown unbound names remain literal successful text"
  (let ((outcome (niwa-skills-input-resolve-mention
                  "$unknown" (niwa-skills-test--make-session)
                  0 8 "unknown")))
    (should (eq 'ok (plist-get outcome :status)))
    (should-not (plist-get outcome :skill))))

(niwa-deftest niwa-skills-input-refresh-bound-input ()
  ,test
  (test)
  :doc "rescans only bound input and exposes the latest exact source body"
  (let* ((niwa-skills-include-bundled nil)
         (niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-refresh-bound-" t))
         (skill-dir (file-name-concat root ".niwa/skills"))
         (niwa-skill-dirs '(".niwa/skills"))
         (ws (niwa-workspace--create
              :type 'file :id root :root root :name "refresh-bound"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         source
         bound)
    (unwind-protect
        (progn
          (setq source
                (niwa-skills-test--write-skill
                 skill-dir "alpha"
                 "name: alpha\ndescription: Alpha\ncontext: inline\n"
                 "ALPHA V1"))
          (with-temp-buffer
            (niwa-skills-install session (current-buffer))
            (let ((installed (niwa-session-skills session)))
              (niwa-skills-input-refresh-bound-input "plain text" session)
              (should (eq installed (niwa-session-skills session))))
            (setq bound (copy-sequence "use $alpha"))
            (niwa-mention-bindings-set
             4 10
             (list :kind 'skill :token "$alpha" :source-file source)
             bound)
            (niwa-skills-test--write-skill
             skill-dir "alpha"
             "name: alpha\ndescription: Alpha\ncontext: inline\n"
             "ALPHA V2")
            (should (eq bound
                        (niwa-skills-input-refresh-bound-input bound session)))
            (should (equal
                     "ALPHA V2"
                     (niwa-skill-load-body
                      (niwa-session-get-skill-by-source
                       session source))))))
      (delete-directory root t))))

(niwa-deftest niwa-skills-input-clear-pending ()
  ,test
  (test)
  :doc "clears both input- and invocation-owned pending request state"
  (with-temp-buffer
    (setq-local niwa-skills-input--pending-inline-attachments
                '((:name "alpha")))
    (setq-local niwa-skills--pending-request-context
                '(:model fast))
    (niwa-skills-input-clear-pending)
    (should-not niwa-skills-input--pending-inline-attachments)
    (should-not niwa-skills--pending-request-context)))

(niwa-deftest niwa-skills-input-dispatch-inline-attachments (:quiet t)
  ,test
  (test)
  :doc "prepares distinct inline skills, appends metadata, and continues"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "alpha" :body "Alpha body")))
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (setq-local niwa-skills--pending-request-context nil)
      (insert "Please use $alpha and $alpha here")
      (let (continued)
        (cl-letf (((symbol-function 'niwa-skills-ensure-fresh)
                   (lambda (&rest _) nil)))
          (should (eq 'skill
                      (niwa-skills-input-dispatch-inline-attachments
                       (lambda () (setq continued t))))))
        (should continued)
        (should (= 1 (length niwa-skills-input--pending-inline-attachments)))
        (should (equal "Alpha body"
                       (plist-get
                        (car niwa-skills-input--pending-inline-attachments)
                        :body)))
        (should (string-search "inline-skill-attachments"
                               (buffer-string)))
        (should (= 1 (length (plist-get
                              niwa-skills--pending-request-context
                              :invoked-skills)))))))

  :doc "unavailable inline skills stage an annotation and continue"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create :name "alpha" :body "Alpha body")))
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (insert "Please use $alpha here")
      (let (continued)
        (cl-letf (((symbol-function 'niwa-skills-skill-enabled-p)
                   (lambda (_skill) nil)))
          (should (eq 'skill
                      (niwa-skills-input-dispatch-inline-attachments
                       (lambda () (setq continued t))))))
        (should continued)
        (should (plist-get
                 (car niwa-skills-input--pending-inline-attachments)
                 :unavailable)))))

  :doc "raw fork-context attachment is an instruction and ignores policy"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "review" :body "Review body" :context 'fork
                 :model "invalid-selector" :effort 'impossible
                 :hooks '((PreToolUse nil)))))
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (setq-local niwa-skills--pending-request-context nil)
      (insert "Please use $review here")
      (let (continued)
        (cl-letf (((symbol-function 'niwa-skills-ensure-fresh)
                   (lambda (&rest _) nil))
                  ((symbol-function 'niwa-agent-control-spawn)
                   (lambda (&rest _) (ert-fail "instruction forked"))))
          (should (eq 'skill
                      (niwa-skills-input-dispatch-inline-attachments
                       (lambda () (setq continued t)))))
          (should continued))
        (should (= 1 (length niwa-skills-input--pending-inline-attachments)))
        (should-not (plist-member niwa-skills--pending-request-context
                                  :model))
        (should-not (plist-member niwa-skills--pending-request-context
                                  :effort))
        (should-not (plist-member niwa-skills--pending-request-context
                                  :hook-rules))
        (should (= 1 (length (plist-get
                              niwa-skills--pending-request-context
                              :invoked-skills))))))))

(defun test-niwa-skills-input--staged-bodies (fsm)
  "Return reminder bodies staged on FSM."
  (mapcar (lambda (entry) (plist-get entry :body))
          (plist-get (gptel-fsm-info fsm) :niwa-reminder-entries)))

(niwa-deftest niwa-skills-input-transform-inline-attachments ()
  ,test
  (test)
  :doc "replaces live mentions with placeholders and stages reminders"
  (let* ((session (niwa-skills-test--make-session))
         (chat (generate-new-buffer " *niwa-inline-skill-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local niwa--session session)
            (setq-local niwa-skills-input--pending-inline-attachments
                        (list (list :name "alpha" :body "Alpha body")
                              (list :name "beta" :body "Beta body"))))
          (with-temp-buffer
            (insert (propertize
                     "Use $alpha and \"$alpha\", `$beta`, then $beta."
                     'gptel 'prompt))
            (niwa-skills-input-transform-inline-attachments fsm)
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
                         (niwa-transcript-prompt-transform-start))))
            (should (eq 'prompt
                        (get-text-property
                         (+ (match-beginning 0) (length "Use "))
                         'gptel))))
          (let ((bodies (test-niwa-skills-input--staged-bodies fsm)))
            (should (= 2 (length bodies)))
            (should (string-match-p "host already attached the skill"
                                    (car bodies)))
            (should (string-match-p "must follow it without calling `Skill`"
                                    (car bodies)))
            (should (string-match-p "Alpha body" (car bodies)))
            (should (string-match-p "Beta body" (cadr bodies)))
            (should-not (string-search "skill://" (mapconcat #'identity bodies "\n"))))
          (with-current-buffer chat
            (should-not niwa-skills-input--pending-inline-attachments)))
      (kill-buffer chat)))

  :doc "same-named bound skills retain distinct canonical sources"
  (let* ((root (make-temp-file "skill-inline-sources-" t))
         (session (niwa-skills-test--make-session nil root))
         (chat (generate-new-buffer " *niwa-inline-skill-source-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat)))
         (source-a (niwa-skills-test--write-skill
                    (file-name-concat root "local") "alpha"
                    "name: alpha\ndescription: Local\n" "Body A"))
         (source-b (niwa-skills-test--write-skill
                    (file-name-concat root "global") "alpha"
                    "name: alpha\ndescription: Global\n" "Body B"))
         (skills (mapcar (lambda (source)
                           (niwa-skill--create :name "alpha" :source-file source))
                         (list source-a source-b)))
         (input (propertize "Use $alpha then $alpha." 'gptel 'prompt)))
    (setf (niwa-session-skills session) skills)
    (niwa-mention-bindings-set
     4 10 (list :kind 'skill :token "$alpha" :source-file source-a) input)
    (niwa-mention-bindings-set
     16 22 (list :kind 'skill :token "$alpha" :source-file source-b) input)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local niwa--session session)
            (setq-local
             niwa-skills-input--pending-inline-attachments
             (list (list :name "alpha" :source-file source-a :body "Body A")
                   (list :name "alpha" :source-file source-b :body "Body B"))))
          (with-temp-buffer
            (insert input)
            (niwa-skills-input-transform-inline-attachments fsm)
            (should (= 2 (how-many "\\[skill:alpha -- attached\\]"
                                   (point-min) (point-max)))))
          (let ((bodies (test-niwa-skills-input--staged-bodies fsm)))
            (should (= 2 (length bodies)))
            (should (string-match-p "Body A" (car bodies)))
            (should (string-match-p "Body B" (cadr bodies)))
            (cl-mapc
             (lambda (skill body expected)
               (let* ((address (niwa-resource--skill-address skill))
                      (attempt (niwa-resource-prepare
                                'read address (list :session session))))
                 (should (string-search address body))
                 (should (string-search
                          expected
                          (niwa-resource-execute
                           attempt (lambda (path _address)
                                     (with-temp-buffer
                                       (insert-file-contents path)
                                       (buffer-string))))))))
             skills bodies '("Body A" "Body B"))))
      (kill-buffer chat)
      (delete-directory root t)))

  :doc "coexists with earlier mention expansion reminders"
  (let* ((root (make-temp-file "niwa-inline-skill-mentions-" t))
         (file (file-name-concat root "notes.txt"))
         (ws (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "main" ws))
         (chat (generate-new-buffer " *niwa-inline-skill-mention-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "Mention body\n"))
          (with-current-buffer chat
            (setq-local niwa--session session)
            (setq-local niwa-skills-input--pending-inline-attachments
                        (list (list :name "alpha" :body "Alpha body"))))
          (with-temp-buffer
            (insert (propertize
                     (format "Read @file:%s and use $alpha." file)
                     'gptel 'prompt))
            (niwa--transform-expand-mentions fsm)
            (niwa-skills-input-transform-inline-attachments fsm)
            (let ((text (buffer-string)))
              (should (string-match-p
                       "\\[file:.* -- contents attached above\\]" text))
              (should (string-match-p
                       "\\[skill:alpha -- attached\\]" text))
              (should-not (string-match-p "<system-reminder>" text))))
          (let ((bodies (test-niwa-skills-input--staged-bodies fsm)))
            (should (= 2 (length bodies)))
            (should (string-match-p "Mention body" (car bodies)))
            (should (string-match-p "Alpha body" (cadr bodies)))))
      (kill-buffer chat)
      (delete-directory root t)))

  :doc "does not rewrite earlier system-reminder bodies"
  (let* ((session (niwa-skills-test--make-session))
         (chat (generate-new-buffer " *niwa-inline-skill-reminder-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local niwa--session session)
            (setq-local niwa-skills-input--pending-inline-attachments
                        (list (list :name "alpha" :body "Alpha body"))))
          (with-temp-buffer
            (insert "<system-reminder>\n"
                    "Reminder text keeps $alpha literal.\n"
                    "</system-reminder>\n\n"
                    "User prompt uses $alpha.")
            (niwa-skills-input-transform-inline-attachments fsm)
            (let ((text (buffer-string)))
              (should (string-match-p
                       (regexp-quote "Reminder text keeps $alpha literal.")
                       text))
              (should (= 1 (how-many "\\[skill:alpha -- attached\\]"
                                     (point-min) (point-max))))))
          (should (string-match-p
                   "Alpha body"
                   (car (test-niwa-skills-input--staged-bodies fsm)))))
      (kill-buffer chat)))

  :doc "replaces unavailable mentions without injecting instructions"
  (let* ((session (niwa-skills-test--make-session))
         (chat (generate-new-buffer " *niwa-inline-unavailable-chat*"))
         (fsm (gptel-make-fsm :info (list :buffer chat))))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local niwa--session session)
            (setq-local niwa-skills-input--pending-inline-attachments
                        '((:name "alpha" :unavailable t
                                 :message "skill $alpha is disabled"))))
          (with-temp-buffer
            (insert (propertize "Use $alpha here" 'gptel 'prompt))
            (niwa-skills-input-transform-inline-attachments fsm)
            (should (string-match-p
                     (regexp-quote "[skill:alpha -- unavailable]")
                     (buffer-string)))
            (should-not (string-match-p "<system-reminder>"
                                        (buffer-string)))))
      (kill-buffer chat))))

(niwa-deftest niwa-skills-input--format-inline-render-data ()
  ,test
  (test)
  :doc "records the display text, root prompt, and structured attachments"
  (let* ((skill (niwa-skill--create :name "doc"))
         (attachments
          '((:name "artifact" :body "Artifact body")
            (:name "artifact-design" :body "Design body")))
         (data (cdr (niwa-tool-render-data-extract
                     (niwa-skills-input--format-inline-render-data
                      skill "write it" "Prepared body." attachments)))))
    (should (eq 'inline-skill (plist-get data :kind)))
    (should (equal "$doc write it" (plist-get data :display-text)))
    (should (equal "Prepared body." (plist-get data :prompt)))
    (should
     (equal
      (mapcar (lambda (attachment)
                (list :name (plist-get attachment :name)
                      :body (niwa-skills-format-attachment attachment)))
              attachments)
      (plist-get data :attachments))))

  :doc "omits attachment names when the skill has no dependencies"
  (let* ((skill (niwa-skill--create :name "doc"))
         (data (cdr (niwa-tool-render-data-extract
                     (niwa-skills-input--format-inline-render-data
                      skill "" "Prepared body." nil)))))
    (should (equal "$doc" (plist-get data :display-text)))
    (should-not (plist-get data :attachments))))

(niwa-deftest niwa-skills-input--handle-user-skill-outcome ()
  ,test
  (test)
  :doc "keeps the provider prompt flat while recording nested reminder text structurally"
  (let* ((skill (niwa-skill--create :name "doc"))
         (attachment
          '(:name "artifact-design"
            :body "Example:\n<system-reminder>\nliteral\n</system-reminder>\nTail."))
         (outcome (list :status 'ok :kind 'inline :body "Root prompt."
                        :arguments "write it"
                        :required-attachments (list attachment)))
         (provider-prompt (niwa-skills-format-model-input outcome)))
    (with-temp-buffer
      (insert "$doc write it")
      (niwa-skills-input--handle-user-skill-outcome
       skill outcome (point-min) (point-max) nil)
      (should (equal provider-prompt
                     (niwa-tool-render-data-strip (buffer-string))))
      (let ((data (cdr (niwa-tool-render-data-extract (buffer-string)))))
        (should (equal "Root prompt." (plist-get data :prompt)))
        (should
         (equal
          (list (list :name "artifact-design"
                      :body (niwa-skills-format-attachment attachment)))
          (plist-get data :attachments))))))

  :doc "raw user command projection delivers root and dependency sources without wrapping render prompt"
  (let ((root (make-temp-file "skill-user-source-" t)))
    (unwind-protect
        (let* ((source (niwa-skills-test--write-skill
                        root "doc" "name: doc\ndescription: Doc\n" "Root"))
               (child-source (niwa-skills-test--write-skill
                              root "base" "name: base\ndescription: Base\n" "Base"))
               (skill (niwa-skill--create :name "doc" :source-file source))
               (child (niwa-skill--create :name "base" :source-file child-source))
               (outcome (list :status 'ok :kind 'inline :skill skill
                              :body "Root prepared" :arguments "write"
                              :required-attachments
                              (list (list :name "base" :skill child
                                          :source-file child-source :body "Base prepared")))))
          (with-temp-buffer
            (insert "$doc write")
            (niwa-skills-input--handle-user-skill-outcome
             skill outcome (point-min) (point-max) nil)
            (let* ((parts (niwa-tool-render-data-extract (buffer-string)))
                   (provider (car parts))
                   (data (cdr parts)))
              (should (< (string-search (niwa-resource--skill-address child) provider)
                         (string-search (niwa-resource--skill-address skill) provider)))
              (should (equal "Root prepared" (plist-get data :prompt)))
              (should (equal "$doc write" (plist-get data :display-text)))
              (should (string-suffix-p "Root prepared" provider)))))
      (delete-directory root t))))

(niwa-deftest niwa-skills-input-dispatch-command (:quiet t)
  ,test
  (test)
  :doc "skill expansion inserts prepared body and returns 'skill"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "greet"
                 :body "Hello $0!")))
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (insert "$greet world")
        (goto-char (point-max))
        (should (eq 'skill (niwa-skills-input-dispatch-command)))
        (should (equal "Hello world!"
                       (niwa-tool-render-data-strip
                        (buffer-string))))
        (should (string-search "<!-- niwa-render-data -->"
                               (buffer-string)))
        (let ((data (cdr (niwa-tool-render-data-extract
                          (buffer-string)))))
          (should (equal "Hello world!"
                         (plist-get data :prompt)))
          (should-not (plist-get data :attachments))))))


  :doc "a draft shortened during preparation does not break the dispatch"
  ;; Preparation is asynchronous whenever the body injects, and the range to
  ;; replace is captured before it starts.  If the draft shrinks meanwhile,
  ;; that captured end is past the end of the buffer.
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "greet"
                 :body "Hello !el`(+ 1 2)`"))
         (pending nil))
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (cl-letf (((symbol-function 'niwa-pipeline-run-tool)
                   (lambda (_tool callback &rest _)
                     (setq pending callback))))
          (insert "$greet world and more text here")
          (goto-char (point-max))
          (niwa-skills-input-dispatch-command)
          (should pending)
          ;; The user trims the draft while the injection is still running.
          (delete-region (- (point-max) 20) (point-max)))
        (funcall pending "3")
        (should (stringp (buffer-string))))))

  :doc "a draft replaced during preparation is left alone and not sent"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "greet"
                 :body "Hello !el`(+ 1 2)`"))
         (pending nil)
         (sent nil))
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (cl-letf (((symbol-function 'niwa-pipeline-run-tool)
                   (lambda (_tool callback &rest _)
                     (setq pending callback))))
          (insert "$greet world")
          (goto-char (point-max))
          (niwa-skills-input-dispatch-command
           (lambda () (setq sent t)))
          (should pending)
          ;; The user rewrites the command into something else entirely.
          (delete-region (point-min) (point-max))
          (insert "never mind"))
        (funcall pending "3")
        ;; The prepared body belongs to arguments that no longer exist.
        (should (equal "never mind" (buffer-string)))
        (should-not sent)
        (should-not niwa-skills--pending-request-context))))

  :doc "a same-length edit to the command is not treated as the same command"
  ;; The corruption half: an edit that keeps the length still moves what the
  ;; captured range covers, so the old code replaced the wrong text and sent
  ;; a body prepared for arguments the draft no longer names.
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "greet"
                 :body "Hello !el`(+ 1 2)`"))
         (pending nil)
         (sent nil))
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (cl-letf (((symbol-function 'niwa-pipeline-run-tool)
                   (lambda (_tool callback &rest _)
                     (setq pending callback))))
          (insert "$greet world")
          (goto-char (point-max))
          (niwa-skills-input-dispatch-command
           (lambda () (setq sent t)))
          (should pending)
          ;; Same length, different argument.
          (goto-char (- (point-max) 5))
          (delete-char 5)
          (insert "there"))
        (funcall pending "3")
        (should (equal "$greet there" (buffer-string)))
        (should-not sent)
        (should-not niwa-skills--pending-request-context))))

  :doc "user-invocable: false leading skill falls through to inline planning"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "internal-only"
                 :body "ignored"
                 :user-invocable-p nil)))
    (setf (niwa-session-skills session) (list skill))
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (insert "$internal-only")
        (goto-char (point-max))
        (should-not (niwa-skills-input-dispatch-command))
        (should (string-match-p "\\$internal-only" (buffer-string)))
        (should-not niwa-skills-input--pending-inline-attachments))))

  :doc "unknown dollar command is left for normal sending"
  (let ((session (niwa-skills-test--make-session)))
    (niwa-skills-test--with-chat-buffer session
      (insert "$PATH is useful context")
      (goto-char (point-max))
      (should (null (niwa-skills-input-dispatch-command)))
      (should (equal "$PATH is useful context" (buffer-string)))))

  :doc "fork-context skill dispatches directly and inserts result"
  (let* ((session (niwa-skills-test--make-session))
         (agent (niwa-agent--create :name "explorer"))
         (skill (niwa-skill--create
                 :name "delegate"
                 :body "should-not-appear"
                 :context 'fork
                 :agent "explorer"))
         save-called status-called stop-called
         baseline-recorded scoped-permission-seen
         queue-drain-scheduled post-hook-called)
    (setf (niwa-session-skills session) (list skill)
          (niwa-session-permission-mode session) 'ask)
    (niwa-skills-test--with-chat-buffer session
      (let ((niwa-slash-commands nil))
        (cl-letf (((symbol-function 'niwa-agent-get)
                   (lambda (n) (and (equal n "explorer") agent)))
                  ((symbol-function 'niwa-agent-control-spawn)
                   (lambda (_session _task _message callback &rest keys)
                     (let ((record
                            (niwa-agent-record--create
                             :path "/root/skill_delegate" :activity 'idle)))
                       (funcall callback
                                (list :outcome 'success :record record)))
                     (funcall
                      (plist-get keys :result-handler)
                      '(:type RESULT :sender "/root/skill_delegate"
                              :recipient "/root" :outcome completed
                              :payload "agent finished"))
                     #'ignore))
                  ((symbol-function 'niwa-session-artifacts-save)
                   (lambda (session buffer &optional settled)
                     (setq save-called (list session buffer settled))
                     "saved"))
                  ((symbol-function 'niwa-compact-estimation-record-token-baseline)
                   (lambda (_fsm) (setq baseline-recorded t)))
                  ((symbol-function 'niwa--run-turn-terminal-hook)
                   (lambda (_fsm event status)
                     (setq scoped-permission-seen
                           (and (eq 'full-auto
                                    (niwa-request-permission-mode-override
                                     niwa--current-request))
                                (null niwa--pending-permission-mode)))
                     (setq stop-called
                           (list event status
                                 (not
                                  (null
                                   (bound-and-true-p
                                    niwa--current-request)))))))
                  ((symbol-function
                    'niwa-view--schedule-follow-up-drain)
                   (lambda (_fsm) (setq queue-drain-scheduled t)))
                  ((symbol-function 'gptel--update-status)
                   (lambda (&rest args) (setq status-called args))))
          (let ((gptel-response-separator "\n\n")
                (gptel-post-response-functions
                 (list (lambda (_start _end)
                         (setq post-hook-called t)
                         (error "Broken post-response hook")))))
            (setq niwa--pending-permission-mode 'full-auto)
            (insert "$delegate do the thing")
            (goto-char (point-max))
            (should (eq 'skill (niwa-skills-input-dispatch-command)))
            (let ((buf (buffer-string)))
              (should (string-match-p "\\$delegate do the thing" buf))
              (should (string-match-p "agent finished" buf))
              (should-not (string-match-p "should-not-appear" buf)))
            (with-timeout (3 (ert-fail "Fork settlement did not finish"))
              (while (niwa-turn-busy-p (current-buffer))
                (accept-process-output nil 0.01)))
            (should-not niwa--current-request)
            (should (= 1 (niwa-session-turn-count session)))
            (should (equal (list session (current-buffer) t) save-called))
            (should (equal '(Stop completed t) stop-called))
            (should baseline-recorded)
            (should scoped-permission-seen)
            (should-not niwa--pending-permission-mode)
            (should (eq 'ask (niwa-session-permission-mode session)))
            (should queue-drain-scheduled)
            (should post-hook-called)
            (should (equal '(" Ready" success) status-called)))))))

  :doc "skill body after a response is placed on a fresh line with blank separator"
  (let* ((session (niwa-skills-test--make-session))
         (skill (niwa-skill--create
                 :name "greet"
                 :body "Hello $0!")))
    (setf (niwa-session-skills session) (list skill))
    (with-temp-buffer
      (setq niwa--session session)
      (let ((niwa-slash-commands nil))
        (insert "Old response")
        (put-text-property (point-min) (point-max) 'gptel 'response)
        (insert "\n$greet world")
        (goto-char (point-max))
        (should (eq 'skill (niwa-skills-input-dispatch-command)))
        (should (equal "Old response\n\nHello world!"
                       (niwa-tool-render-data-strip
                        (buffer-string))))))))

(niwa-deftest niwa-skills-input--prepare-inline-attachments ()
  ,test
  (test)
  :doc "flattens required children before their user instruction attachment"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-input-graph-" t))
         (dir (file-name-concat root ".niwa" "skills"))
         (workspace (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "input-graph" workspace root))
         result)
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "child" "name: child\ndescription: Child\n" "Child body")
          (niwa-skills-test--write-skill
           dir "parent" "name: parent\ndescription: Parent\n"
           "Parent body\n!$child")
          (setf (niwa-session-skills session)
                (niwa-skills-scan root '(".niwa/skills") workspace))
          (let ((parent (niwa-session-get-skill session "parent")))
            (with-temp-buffer
              (setq-local niwa--session session)
              (niwa-skills-input--prepare-inline-attachments
               (list (list :name "parent" :start 0 :end 7
                           :source-file (niwa-skill-source-file parent)
                           :skill parent))
               (lambda (value) (setq result value)))))
          (should (eq 'ok (plist-get result :status)))
          (let ((attachments (plist-get result :attachments)))
            (should (equal '("child" "parent")
                           (mapcar (lambda (item) (plist-get item :name))
                                   attachments)))
            (should (equal '("child" "parent")
                           (mapcar
                            #'niwa-skill-invocation-record-name
                            (plist-get (car attachments) :invoked-skills))))))
      (delete-directory root t)))

  :doc "leading user commands insert complete required model input"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-command-graph-" t))
         (dir (file-name-concat root ".niwa" "skills"))
         (workspace (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "command-graph" workspace root)))
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "child" "name: child\ndescription: Child\n" "Child contract")
          (niwa-skills-test--write-skill
           dir "parent" "name: parent\ndescription: Parent\n"
           "Parent command\n!$child")
          (setf (niwa-session-skills session)
                (niwa-skills-scan root '(".niwa/skills") workspace))
          (niwa-skills-test--with-chat-buffer session
            (let ((niwa-slash-commands nil))
              (insert "$parent")
              (goto-char (point-max))
              (should (eq 'skill (niwa-skills-input-dispatch-command)))
              (let ((text (niwa-tool-render-data-strip (buffer-string))))
                (should (string-match-p "Child contract" text))
                (should (string-match-p "Parent command" text))))))
      (delete-directory root t))))

(provide 'test-niwa-skills-input)
;;; test-niwa-skills-input.el ends here
