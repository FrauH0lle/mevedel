;;; test-mevedel-skills-plan.el -- Deterministic skill plan tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-mentions)
(require 'mevedel-pipeline)
(require 'mevedel-skill-bindings)
(require 'mevedel-skills-core)
(require 'mevedel-skills-invoke)
(require 'mevedel-skills-plan)
(require 'mevedel-structs)


;;
;;; Helpers

(defun mevedel-skills-plan-test--skill (root name &optional context source)
  "Return a user-invocable NAME skill below ROOT with CONTEXT and SOURCE."
  (let ((file (or source
                  (mevedel-skills-test--write-skill
                   root name
                   (format "name: %s\ndescription: Test %s\ncontext: %s\n"
                           name name (or context 'inline))
                   (upcase name)))))
    (mevedel-skill--create
     :name name :source-file file :source-dir (file-name-directory file)
     :source 'project :active-p t :user-invocable-p t
     :model-invocable-p t :context (or context 'inline))))

(defun mevedel-skills-plan-test--session (&rest skills)
  "Return a transient session containing SKILLS."
  (mevedel-session--create :name "plan" :skills skills))

(defun mevedel-skills-plan-test--names (items)
  "Return plan entry names from ITEMS."
  (mapcar #'mevedel-skill-plan-entry-name items))


;;
;;; Planning phases

(mevedel-deftest mevedel-skills-plan--resolve ()
  ,test
  (test)
  :doc "resolves live user tokens to their exact session skills"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (alpha (mevedel-skills-plan-test--skill root "alpha"))
         (session (mevedel-skills-plan-test--session alpha)))
    (unwind-protect
        (let* ((outcome (mevedel-skills-plan--resolve "Use $alpha" session))
               (token (car (plist-get outcome :tokens))))
          (should (eq 'ok (plist-get outcome :status)))
          (should (equal "alpha" (plist-get token :name)))
          (should (eq alpha (plist-get token :value))))
      (delete-directory root t)))

  :doc "consumes structured binding reasons without parsing their messages"
  (cl-letf (((symbol-function 'mevedel-skill-bindings-resolve-outcome)
             (lambda (&rest _)
               '(:status error :reason not-user-invocable
                 :message "Opaque policy rejection"))))
    (let ((outcome (mevedel-skills-plan--resolve "$alpha" nil)))
      (should (eq 'not-user-invocable (plist-get outcome :reason)))
      (should (equal "Opaque policy rejection" (plist-get outcome :message))))))

(mevedel-deftest mevedel-skills-plan--command-layout ()
  ,test
  (test)
  :doc "finds the bounded leading command stack and raw argument boundary"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (alpha (mevedel-skills-plan-test--skill root "alpha"))
         (beta (mevedel-skills-plan-test--skill root "beta"))
         (session (mevedel-skills-plan-test--session alpha beta))
         (text "$alpha $beta -- work"))
    (unwind-protect
        (let* ((tokens (plist-get
                        (mevedel-skills-plan--resolve text session) :tokens))
               (layout (mevedel-skills-plan--command-layout text tokens))
               (starts (plist-get layout :command-starts)))
          (should (= 2 (hash-table-count starts)))
          (should (gethash 0 starts))
          (should (gethash 7 starts))
          (should (= 15 (plist-get layout :arguments-start)))
          (should-not (plist-get layout :fork-p)))
      (delete-directory root t))))

(mevedel-deftest mevedel-skills-plan--classify ()
  ,test
  (test)
  :doc "classifies command and instruction occurrences and trims arguments"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (alpha (mevedel-skills-plan-test--skill root "alpha"))
         (beta (mevedel-skills-plan-test--skill root "beta"))
         (session (mevedel-skills-plan-test--session alpha beta))
         (text "$alpha use $beta  "))
    (unwind-protect
        (let* ((tokens (plist-get
                        (mevedel-skills-plan--resolve text session) :tokens))
               (classification (mevedel-skills-plan--classify text tokens)))
          (should (equal '(command instruction)
                         (mapcar #'mevedel-skill-plan-occurrence-role
                                 (plist-get classification :occurrences))))
          (should (equal "use $beta"
                         (plist-get classification :arguments))))
      (delete-directory root t))))

(mevedel-deftest mevedel-skills-plan--build ()
  ,test
  (test)
  :doc "builds command-first entries with command source precedence"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (alpha (mevedel-skills-plan-test--skill root "alpha"))
         (alias (mevedel-skills-plan-test--skill
                 root "alias" 'inline (mevedel-skill-source-file alpha)))
         (beta (mevedel-skills-plan-test--skill root "beta"))
         (session (mevedel-skills-plan-test--session alpha alias beta))
         (text "$alpha use $alias and $beta"))
    (unwind-protect
        (let* ((tokens (plist-get
                        (mevedel-skills-plan--resolve text session) :tokens))
               (plan (mevedel-skills-plan--build
                      text (mevedel-skills-plan--classify text tokens))))
          (should (equal '("alpha" "beta")
                         (mevedel-skills-plan-test--names
                          (mevedel-skill-invocation-plan-entries plan))))
          (should (= 3 (length
                        (mevedel-skill-invocation-plan-occurrences plan)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-skills-plan--aggregate-prepared ()
  ,test
  (test)
  :doc "aggregates command policy and instruction reminders in entry order"
  (let* ((command (mevedel-skill-plan-entry--create
                   :name "alpha" :role 'command))
         (instruction (mevedel-skill-plan-entry--create
                       :name "beta" :role 'instruction))
         (aggregate
          (mevedel-skills-plan--aggregate-prepared
           (list
            (list :entry command
                  :outcome
                  '(:body "command" :hook-audits (command-audit)
                    :request-context
                    (:permission-rules (permission)
                     :hook-rules (hook) :invoked-skills (command-record)
                     :model selected :effort high)))
            (list :entry instruction
                  :outcome
                  '(:body "instruction" :hook-audits (instruction-audit)
                    :request-context (:invoked-skills
                                      (instruction-record))))))))
    (should (= 1 (plist-get aggregate :command-count)))
    (should (equal '("command") (plist-get aggregate :command-bodies)))
    (should (equal '(permission) (plist-get aggregate :permission-rules)))
    (should (equal '(hook) (plist-get aggregate :hook-rules)))
    (should (equal '(command-record instruction-record)
                   (plist-get aggregate :invoked-skills)))
    (should (= 1 (length (plist-get aggregate :instruction-reminders))))
    (should (equal '(command-audit instruction-audit)
                   (plist-get aggregate :hook-audits)))))

(mevedel-deftest mevedel-skills-plan--prepared-outcome ()
  ,test
  (test)
  :doc "composes instruction reminders and inert placeholders into one outcome"
  (let* ((entry (mevedel-skill-plan-entry--create
                 :name "alpha" :role 'instruction))
         (occurrence (mevedel-skill-plan-occurrence--create
                      :start 4 :end 10 :name "alpha" :role 'instruction))
         (plan (mevedel-skill-invocation-plan--create
                :text "Use $alpha" :occurrences (list occurrence)))
         (pairs
          (list
           (list :entry entry
                 :outcome
                 '(:body "instruction body"
                   :request-context (:invoked-skills (record))))))
         (outcome (mevedel-skills-plan--prepared-outcome plan pairs)))
    (should (eq 'ok (plist-get outcome :status)))
    (should (string-match-p (regexp-quote "[skill:alpha -- attached]")
                            (plist-get outcome :model-input)))
    (should (string-match-p (regexp-quote "instruction body")
                            (plist-get outcome :model-input)))
    (should (equal '(record)
                   (plist-get (plist-get outcome :request-context)
                              :invoked-skills)))))


;;
;;; Planning

(mevedel-deftest mevedel-skills-plan-user-input ()
  ,test
  (test)
  :doc "classifies prose mentions as ordered, deduplicated instructions"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (alpha (mevedel-skills-plan-test--skill root "alpha"))
         (beta (mevedel-skills-plan-test--skill root "beta"))
         (session (mevedel-skills-plan-test--session alpha beta)))
    (unwind-protect
        (let* ((outcome (mevedel-skills-plan-user-input
                         "Use $beta, then $alpha and $beta." session))
               (plan (plist-get outcome :plan)))
          (should (eq 'ok (plist-get outcome :status)))
          (should-not (mevedel-skill-invocation-plan-commands plan))
          (should (equal '("beta" "alpha")
                         (mevedel-skills-plan-test--names
                          (mevedel-skill-invocation-plan-instructions plan))))
          (should (= 3 (length
                        (mevedel-skill-invocation-plan-occurrences plan))))
          (should (cl-every
                   (lambda (occurrence)
                     (eq 'instruction
                         (mevedel-skill-plan-occurrence-role occurrence)))
                   (mevedel-skill-invocation-plan-occurrences plan))))
      (delete-directory root t)))

  :doc "leading inline commands stack, share arguments, and honor --"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (alpha (mevedel-skills-plan-test--skill root "alpha"))
         (beta (mevedel-skills-plan-test--skill root "beta"))
         (delta (mevedel-skills-plan-test--skill root "delta"))
         (session (mevedel-skills-plan-test--session alpha beta delta)))
    (unwind-protect
        (let* ((outcome (mevedel-skills-plan-user-input
                         "$alpha $alpha $beta -- $delta details" session))
               (plan (plist-get outcome :plan)))
          (should (equal '("alpha" "beta")
                         (mevedel-skills-plan-test--names
                          (mevedel-skill-invocation-plan-commands plan))))
          (should (equal '("delta")
                         (mevedel-skills-plan-test--names
                          (mevedel-skill-invocation-plan-instructions plan))))
          (should (equal "$delta details"
                         (mevedel-skill-invocation-plan-arguments plan)))
          (should (equal '(command command command instruction)
                         (mapcar #'mevedel-skill-plan-occurrence-role
                                 (mevedel-skill-invocation-plan-occurrences
                                  plan)))))
      (delete-directory root t)))

  :doc "six distinct commands is fixed and the seventh starts instructions"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (skills (mapcar (lambda (name)
                           (mevedel-skills-plan-test--skill root name))
                         '("a" "b" "c" "d" "e" "f" "g")))
         (session (apply #'mevedel-skills-plan-test--session skills)))
    (unwind-protect
        (let* ((plan (plist-get
                      (mevedel-skills-plan-user-input
                       "$a $b $c $d $e $f $g rest" session)
                      :plan)))
          (should (equal '("a" "b" "c" "d" "e" "f")
                         (mevedel-skills-plan-test--names
                          (mevedel-skill-invocation-plan-commands plan))))
          (should (equal '("g")
                         (mevedel-skills-plan-test--names
                          (mevedel-skill-invocation-plan-instructions plan))))
          (should (equal "$g rest"
                         (mevedel-skill-invocation-plan-arguments plan))))
      (delete-directory root t)))

  :doc "a leading fork runs alone while later forks are instructions"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (alpha (mevedel-skills-plan-test--skill root "alpha"))
         (fork (mevedel-skills-plan-test--skill root "fork" 'fork))
         (session (mevedel-skills-plan-test--session alpha fork)))
    (unwind-protect
        (let* ((leading (plist-get
                         (mevedel-skills-plan-user-input
                          "$fork inspect with $alpha" session)
                         :plan))
               (later (plist-get
                       (mevedel-skills-plan-user-input
                        "$alpha $fork inspect" session)
                       :plan)))
          (should (mevedel-skill-invocation-plan-fork-p leading))
          (should (equal '("fork")
                         (mevedel-skills-plan-test--names
                          (mevedel-skill-invocation-plan-commands leading))))
          (should (equal "inspect with $alpha"
                         (mevedel-skill-invocation-plan-arguments leading)))
          (should (equal '("alpha")
                         (mevedel-skills-plan-test--names
                          (mevedel-skill-invocation-plan-instructions leading))))
          (should-not (mevedel-skill-invocation-plan-fork-p later))
          (should (equal '("alpha")
                         (mevedel-skills-plan-test--names
                          (mevedel-skill-invocation-plan-commands later))))
          (should (equal '("fork")
                         (mevedel-skills-plan-test--names
                          (mevedel-skill-invocation-plan-instructions later)))))
      (delete-directory root t)))

  :doc "canonical source dedupe lets a command win over an alias instruction"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (alpha (mevedel-skills-plan-test--skill root "alpha"))
         (alias (mevedel-skills-plan-test--skill
                 root "alias" 'inline (mevedel-skill-source-file alpha)))
         (beta (mevedel-skills-plan-test--skill root "beta"))
         (session (mevedel-skills-plan-test--session alpha alias beta)))
    (unwind-protect
        (let* ((plan (plist-get
                      (mevedel-skills-plan-user-input
                       "$alpha work with $alias and $beta" session)
                      :plan)))
          (should (equal '("alpha" "beta")
                         (mevedel-skills-plan-test--names
                          (mevedel-skill-invocation-plan-entries plan))))
          (should (= 3 (length
                        (mevedel-skill-invocation-plan-occurrences plan)))))
      (delete-directory root t)))

  :doc "unknown and intentionally literal syntax stays unplanned"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (alpha (mevedel-skills-plan-test--skill root "alpha"))
         (session (mevedel-skills-plan-test--session alpha)))
    (unwind-protect
        (let* ((plan (plist-get
                      (mevedel-skills-plan-user-input
                       "$unknown \\$alpha `$alpha` \"$alpha\"" session)
                      :plan)))
          (should-not (mevedel-skill-invocation-plan-occurrences plan)))
      (delete-directory root t)))

  :doc "unavailable skills stay literal when escaped, quoted, or in code"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (mevedel-user-dir (make-temp-file "mevedel-skill-plan-state-" t))
         (disabled (mevedel-skills-plan-test--skill root "disabled"))
         (internal (mevedel-skills-plan-test--skill root "internal"))
         (session (mevedel-skills-plan-test--session disabled internal))
         (input (copy-sequence
                 "Use \\$disabled, \"$internal\", and `$missing`."))
         (missing-start (string-match "\\$missing" input)))
    (setf (mevedel-skill-user-invocable-p internal) nil)
    (mevedel-mentions-set-binding
     missing-start (+ missing-start (length "$missing"))
     (list :kind 'skill :name "missing"
           :source-file (file-name-concat root "missing/SKILL.md"))
     input)
    (unwind-protect
        (progn
          (mevedel-skills--set-enabled disabled nil)
          (let* ((outcome (mevedel-skills-plan-user-input input session))
                 (plan (plist-get outcome :plan)))
            (should (eq 'ok (plist-get outcome :status)))
            (should-not (mevedel-skill-invocation-plan-occurrences plan))))
      (delete-directory root t)
      (delete-directory mevedel-user-dir t)))

  :doc "missing exact bindings, disabled skills, and invocation gates are errors"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (mevedel-user-dir (make-temp-file "mevedel-skill-plan-state-" t))
         (disabled (mevedel-skills-plan-test--skill root "disabled"))
         (internal (mevedel-skills-plan-test--skill root "internal"))
         (session (mevedel-skills-plan-test--session disabled internal))
         (bound (copy-sequence "Use $missing"))
         (missing (file-name-concat root "missing/SKILL.md")))
    (setf (mevedel-skill-user-invocable-p internal) nil)
    (mevedel-mentions-set-binding
     4 12
     (list :kind 'skill :name "missing" :source-file missing)
     bound)
    (unwind-protect
        (progn
          (mevedel-skills--set-enabled disabled nil)
          (should (eq 'bound-source
                      (plist-get (mevedel-skills-plan-user-input bound session)
                                 :reason)))
          (should (eq 'disabled
                      (plist-get
                       (mevedel-skills-plan-user-input "$disabled" session)
                       :reason)))
          (should (eq 'not-user-invocable
                      (plist-get
                       (mevedel-skills-plan-user-input "$internal" session)
                       :reason))))
      (delete-directory root t)
      (delete-directory mevedel-user-dir t))))

(mevedel-deftest mevedel-skills-plan-replace-instructions ()
  ,test
  (test)
  :doc "replaces selected instruction extents in a source slice without scanning"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (alpha (mevedel-skills-plan-test--skill root "alpha"))
         (beta (mevedel-skills-plan-test--skill root "beta"))
         (delta (mevedel-skills-plan-test--skill root "delta"))
         (session (mevedel-skills-plan-test--session alpha beta delta))
         (input "$alpha inspect $beta with $delta")
         (plan (plist-get (mevedel-skills-plan-user-input input session) :plan))
         (offset (mevedel-skill-invocation-plan-arguments-start plan))
         (slice (substring input offset))
         (instructions (mevedel-skill-invocation-plan-occurrences plan)))
    (unwind-protect
        (progn
          (should (equal "inspect [skill:beta -- attached] with [skill:delta -- attached]"
                         (mevedel-skills-plan-replace-instructions
                          slice instructions offset)))
          (should (equal "inspect $beta with [skill:delta -- attached]"
                         (mevedel-skills-plan-replace-instructions
                          slice (last instructions) offset))))
      (delete-directory root t))))

(mevedel-deftest mevedel-skills-plan-prepare ()
  ,test
  (test)
  :doc "prepares unique commands then instructions and composes inert arguments"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (alpha (mevedel-skills-plan-test--skill root "alpha"))
         (beta (mevedel-skills-plan-test--skill root "beta"))
         (delta (mevedel-skills-plan-test--skill root "delta"))
         (session (mevedel-skills-plan-test--session alpha beta delta))
         (plan (plist-get
                (mevedel-skills-plan-user-input
                 "$alpha $beta inspect $delta and $alpha" session)
                :plan))
         calls
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-skills-prepare)
                   (lambda (skill arguments callback &rest keys)
                     (let* ((name (mevedel-skill-name skill))
                            (role (plist-get keys :role))
                            (record (list name role)))
                       (setq calls (append calls (list (list name arguments))))
                       (funcall
                        callback
                        (list :status 'ok
                              :body (format "%s{%s} $hook-literal"
                                            name arguments)
                              :hook-audits (list (concat "audit-" name))
                              :request-context
                              (list :permission-rules
                                    (and (eq role 'command)
                                         (list (concat "rule-" name)))
                                    :hook-rules
                                    (and (eq role 'command)
                                         (list (concat "hook-" name)))
                                    :model (concat "model-" name)
                                    :effort (concat "effort-" name)
                                    :invoked-skills (list record))))))))
          (mevedel-skills-plan-prepare plan (lambda (value) (setq result value))))
      (delete-directory root t))
    (should (equal '("alpha" "beta" "delta") (mapcar #'car calls)))
    (should (equal
             "inspect [skill:delta -- attached] and [skill:alpha -- attached]"
             (cadar calls)))
    (should (equal (cadar calls) (cadadr calls)))
    (should (equal "" (cadar (cddr calls))))
    (should (equal '("rule-alpha" "rule-beta")
                   (plist-get (plist-get result :request-context)
                              :permission-rules)))
    (should (equal '("hook-alpha" "hook-beta")
                   (plist-get (plist-get result :request-context)
                              :hook-rules)))
    (should (= 3 (length (plist-get (plist-get result :request-context)
                                    :invoked-skills))))
    (should-not (plist-member (plist-get result :request-context) :model))
    (should-not (plist-member (plist-get result :request-context) :effort))
    (should (string-match-p
             (regexp-quote "[skill:delta -- attached]")
             (plist-get result :model-input)))
    (should (string-match-p (regexp-quote "$hook-literal")
                            (plist-get result :model-input)))
    (should (= 3 (length (plist-get result :hook-audits))))
    (should (= 3 (length (plist-get result :prepared-entries)))))

  :doc "one command retains model and effort while command source wins dedupe"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (alpha (mevedel-skills-plan-test--skill root "alpha"))
         (alias (mevedel-skills-plan-test--skill
                 root "alias" 'inline (mevedel-skill-source-file alpha)))
         (session (mevedel-skills-plan-test--session alpha alias))
         (plan (plist-get
                (mevedel-skills-plan-user-input
                 "$alpha use $alias" session)
                :plan))
         calls
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-skills-prepare)
                   (lambda (skill arguments callback &rest _)
                     (push (mevedel-skill-name skill) calls)
                     (funcall callback
                              (list :status 'ok :body arguments
                                    :request-context
                                    '(:model selected :effort high
                                      :invoked-skills (record)))))))
          (mevedel-skills-plan-prepare plan (lambda (value) (setq result value))))
      (delete-directory root t))
    (should (equal '("alpha") calls))
    (should (eq 'selected
                (plist-get (plist-get result :request-context) :model)))
    (should (eq 'high
                (plist-get (plist-get result :request-context) :effort)))
    (should (string-match-p (regexp-quote "[skill:alias -- attached]")
                            (plist-get result :model-input))))

  :doc "instruction-only plans add unique reminders to placeholderized prose"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (alpha (mevedel-skills-plan-test--skill root "alpha"))
         (session (mevedel-skills-plan-test--session alpha))
         (plan (plist-get
                (mevedel-skills-plan-user-input
                 "Use $alpha and $alpha; keep $body-literal" session)
                :plan))
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-skills-prepare)
                   (lambda (_skill arguments callback &rest _)
                     (should (equal "" arguments))
                     (funcall callback
                              '(:status ok :body "Body says $nested"
                                :request-context
                                (:invoked-skills (record)))))))
          (mevedel-skills-plan-prepare plan (lambda (value) (setq result value))))
      (delete-directory root t))
    (should (= 1 (length (plist-get result :prepared-entries))))
    (should
     (string-match-p
      (regexp-quote
       "Use [skill:alpha -- attached] and [skill:alpha -- attached]; keep $body-literal")
      (plist-get result :model-input)))
    (should (string-match-p (regexp-quote "$nested")
                            (plist-get result :model-input)))
    (should (string-match-p (regexp-quote "$body-literal")
                            (plist-get result :model-input))))

  :doc "the first failure short-circuits remaining entries"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (alpha (mevedel-skills-plan-test--skill root "alpha"))
         (beta (mevedel-skills-plan-test--skill root "beta"))
         (delta (mevedel-skills-plan-test--skill root "delta"))
         (session (mevedel-skills-plan-test--session alpha beta delta))
         (plan (plist-get
                (mevedel-skills-plan-user-input
                 "$alpha $beta use $delta" session)
                :plan))
         calls
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-skills-prepare)
                   (lambda (skill _arguments callback &rest _)
                     (let ((name (mevedel-skill-name skill)))
                       (push name calls)
                       (funcall callback
                                (if (equal name "beta")
                                    '(:status error :reason failed
                                      :message "beta failed")
                                  '(:status ok :body "ok"
                                    :request-context nil)))))))
          (mevedel-skills-plan-prepare plan (lambda (value) (setq result value))))
      (delete-directory root t))
    (should (equal '("beta" "alpha") calls))
    (should (eq 'failed (plist-get result :reason)))
    (should (equal "beta" (plist-get result :name))))

  :doc "cancellation settles once and ignores repeated late callbacks"
  (let* ((mevedel-skills-check-for-modifications nil)
         (root (make-temp-file "mevedel-skill-plan-" t))
         (alpha (mevedel-skills-plan-test--skill root "alpha"))
         (beta (mevedel-skills-plan-test--skill root "beta"))
         (session (mevedel-skills-plan-test--session alpha beta))
         (plan (plist-get
                (mevedel-skills-plan-user-input "$alpha $beta work" session)
                :plan))
         cancelled
         pending
         prepares
         results)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-skills-prepare)
                   (lambda (_skill _arguments callback &rest _)
                     (setq prepares (1+ (or prepares 0))
                           pending callback))))
          (mevedel-skills-plan-prepare
           plan
           (lambda (value) (push value results))
           (lambda () cancelled))
          (setq cancelled t)
          (funcall pending '(:status ok :body "late" :request-context nil))
          (funcall pending '(:status ok :body "later" :request-context nil)))
      (delete-directory root t))
    (should (= 1 prepares))
    (should (= 1 (length results)))
    (should (eq 'cancelled (plist-get (car results) :reason)))))

(mevedel-deftest mevedel-skills-plan-render-data ()
  ,test
  (test)
  :doc "uses the existing hidden render envelope with exact original text"
  (let* ((text (propertize "$alpha original" 'mevedel-mention-binding
                           '(:kind skill)))
         (plan (mevedel-skill-invocation-plan--create
                :text text))
         (block (mevedel-skills-plan-render-data plan))
         (data (cdr (mevedel-pipeline-extract-render-data block))))
    (should (eq 'ignore (get-text-property 0 'gptel block)))
    (should (eq 'inline-skill (plist-get data :kind)))
    (should (equal text (plist-get data :display-text)))))

(provide 'test-mevedel-skills-plan)
;;; test-mevedel-skills-plan.el ends here
