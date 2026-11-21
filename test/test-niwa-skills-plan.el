;;; test-niwa-skills-plan.el -- Deterministic skill plan tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-mention-bindings)
(require 'niwa-mentions)
(require 'niwa-pipeline)
(require 'niwa-tool-render-data)
(require 'niwa-skills-core)
(require 'niwa-skills-invoke)
(require 'niwa-skills-plan)
(require 'niwa-structs)

;; `gptel'
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)


;;
;;; Helpers

(defun niwa-skills-plan-test--skill
    (root name &optional context source workspace)
  "Return a user-invocable NAME skill below ROOT.
CONTEXT and SOURCE customize invocation and identity.  WORKSPACE owns
persisted enablement when supplied."
  (let ((file (or source
                  (niwa-skills-test--write-skill
                   root name
                   (format "name: %s\ndescription: Test %s\ncontext: %s\n"
                           name name (or context 'inline))
                   (upcase name)))))
    (niwa-skill--create
     :name name :source-file file :source-dir (file-name-directory file)
     :source 'project :active-p t :user-invocable-p t
     :model-invocable-p t :context (or context 'inline)
     :workspace workspace)))

(defun niwa-skills-plan-test--session (&rest skills)
  "Return a transient session containing SKILLS."
  (niwa-session--create :name "plan" :skills skills))

(defun niwa-skills-plan-test--names (items)
  "Return plan entry names from ITEMS."
  (mapcar #'niwa-skill-plan-entry-name items))


;;
;;; Planning phases

(niwa-deftest niwa-skills-plan--resolve ()
  ,test
  (test)
  :doc "resolves live user tokens to their exact session skills"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (session (niwa-skills-plan-test--session alpha)))
    (unwind-protect
        (let* ((outcome (niwa-skills-plan--resolve "Use $alpha" session))
               (token (car (plist-get outcome :tokens))))
          (should (equal "alpha" (plist-get token :name)))
          (should (eq alpha (plist-get token :value))))
      (delete-directory root t)))

  :doc "consumes structured binding outcomes without parsing their messages"
  (cl-letf (((symbol-function 'niwa-skills-input-resolve-mention)
             (lambda (&rest _)
               '(:status unavailable :reason not-user-invocable
                 :message "Opaque policy rejection"))))
    (let ((outcome (niwa-skills-plan--resolve "$alpha" nil)))
      (should (equal "Opaque policy rejection"
                     (niwa-skill-plan-occurrence-message
                      (car (plist-get outcome :unavailable))))))))

(niwa-deftest niwa-skills-plan--command-layout ()
  ,test
  (test)
  :doc "finds the bounded leading command stack and raw argument boundary"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (beta (niwa-skills-plan-test--skill root "beta"))
         (session (niwa-skills-plan-test--session alpha beta))
         (text "$alpha $beta -- work"))
    (unwind-protect
        (let* ((tokens (plist-get
                        (niwa-skills-plan--resolve text session) :tokens))
               (layout (niwa-skills-plan--command-layout text tokens))
               (starts (plist-get layout :command-starts)))
          (should (= 2 (hash-table-count starts)))
          (should (gethash 0 starts))
          (should (gethash 7 starts))
          (should (= 15 (plist-get layout :arguments-start)))
          (should-not (plist-get layout :fork-p)))
      (delete-directory root t))))

(niwa-deftest niwa-skills-plan--classify ()
  ,test
  (test)
  :doc "classifies command and instruction occurrences and trims arguments"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (beta (niwa-skills-plan-test--skill root "beta"))
         (session (niwa-skills-plan-test--session alpha beta))
         (text "$alpha use $beta  "))
    (unwind-protect
        (let* ((tokens (plist-get
                        (niwa-skills-plan--resolve text session) :tokens))
               (classification (niwa-skills-plan--classify text tokens)))
          (should (equal '(command instruction)
                         (mapcar #'niwa-skill-plan-occurrence-role
                                 (plist-get classification :occurrences))))
          (should (equal "use $beta"
                         (plist-get classification :arguments))))
      (delete-directory root t))))

(niwa-deftest niwa-skills-plan--build ()
  ,test
  (test)
  :doc "builds command-first entries with command source precedence"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (alias (niwa-skills-plan-test--skill
                 root "alias" 'inline (niwa-skill-source-file alpha)))
         (beta (niwa-skills-plan-test--skill root "beta"))
         (session (niwa-skills-plan-test--session alpha alias beta))
         (text "$alpha use $alias and $beta"))
    (unwind-protect
        (let* ((tokens (plist-get
                        (niwa-skills-plan--resolve text session) :tokens))
               (plan (niwa-skills-plan--build
                      text (niwa-skills-plan--classify text tokens))))
          (should (equal '("alpha" "beta")
                         (niwa-skills-plan-test--names
                          (niwa-skill-invocation-plan-entries plan))))
          (should (= 3 (length
                        (niwa-skill-invocation-plan-occurrences plan)))))
      (delete-directory root t))))

(niwa-deftest niwa-skills-plan--aggregate-prepared ()
  ,test
  (test)
  :doc "aggregates command policy and instruction attachments in entry order"
  (let* ((command (niwa-skill-plan-entry--create
                   :name "alpha" :role 'command))
         (instruction (niwa-skill-plan-entry--create
                       :name "beta" :role 'instruction))
         (aggregate
          (niwa-skills-plan--aggregate-prepared
           (list
            (list :entry command
                  :outcome
                  '(:body "command" :hook-context "command-context"
                    :hook-audits (command-audit)
                    :request-context
                    (:permission-rules (permission)
                     :ptc-primitives ("Read" "Grep")
                     :hook-rules (hook) :invoked-skills (command-record)
                     :model selected :effort high)))
            (list :entry instruction
                  :outcome
                  '(:body "instruction" :hook-context "instruction-context"
                    :hook-audits (instruction-audit)
                    :request-context (:ptc-primitives ("Bash") :invoked-skills
                                      (instruction-record))))))))
    (should (= 1 (plist-get aggregate :command-count)))
    (should (equal '("command") (plist-get aggregate :command-bodies)))
    (should (equal '(permission) (plist-get aggregate :permission-rules)))
    (should (equal '("Read" "Grep") (plist-get aggregate :ptc-primitives)))
    (should (equal '(hook) (plist-get aggregate :hook-rules)))
    (should (equal '("beta")
                   (mapcar
                    (lambda (attachment) (plist-get attachment :name))
                    (plist-get aggregate :instruction-attachments)))))

  :doc "intersects restrictions from stacked command skills"
  (let ((first (niwa-skill-plan-entry--create :name "a" :role 'command))
        (second (niwa-skill-plan-entry--create :name "b" :role 'command)))
    (should
     (equal '("Grep")
            (plist-get
             (niwa-skills-plan--aggregate-prepared
              (list
               (list :entry first :outcome
                     '(:body "a" :request-context
                       (:ptc-primitives ("Read" "Grep"))))
               (list :entry second :outcome
                     '(:body "b" :request-context
                       (:ptc-primitives ("Grep" "Bash"))))))
             :ptc-primitives)))))

(niwa-deftest niwa-skills-plan--prepared-outcome ()
  ,test
  (test)
  :doc "composes instruction reminders and inert placeholders into one outcome"
  (let* ((entry (niwa-skill-plan-entry--create
                 :name "alpha" :role 'instruction))
         (occurrence (niwa-skill-plan-occurrence--create
                      :start 4 :end 10 :name "alpha" :role 'instruction))
         (plan (niwa-skill-invocation-plan--create
                :text "Use $alpha" :occurrences (list occurrence)))
         (pairs
          (list
           (list :entry entry
                 :outcome
                 '(:body "instruction body"
                   :hook-context "expansion context"
                   :request-context (:invoked-skills (record))))))
         (outcome (niwa-skills-plan--prepared-outcome
                   plan pairs
                   '(:invoked-skills (record)
                     :hook-context "expansion context"))))
    (should (eq 'ok (plist-get outcome :status)))
    (should (string-match-p (regexp-quote "[skill:alpha -- attached]")
                            (plist-get outcome :model-input)))
    (should (string-match-p (regexp-quote "instruction body")
                            (plist-get outcome :model-input)))
    (should (equal "expansion context" (plist-get outcome :hook-context)))
    (should (equal '(record)
                   (plist-get (plist-get outcome :request-context)
                              :invoked-skills))))

  :doc "synthetic stacked contributions and their dependencies gain no fabricated address"
  (let* ((first (niwa-skill--create :name "first"))
         (second (niwa-skill--create :name "second"))
         (plan (niwa-skill-invocation-plan--create :text "$first $second"))
         (pairs
          (list (list :entry (niwa-skill-plan-entry--create
                              :name "first" :skill first :role 'command)
                      :outcome '(:body "First body"
                                 :required-attachments ((:name "base" :body "Base body"))))
                (list :entry (niwa-skill-plan-entry--create
                              :name "second" :skill second :role 'command)
                      :outcome '(:body "Second body"))))
         (outcome (niwa-skills-plan--prepared-outcome plan pairs nil)))
    (should-not (string-search "skill://" (plist-get outcome :model-input)))
    (should (string-suffix-p "First body\n\nSecond body"
                             (plist-get outcome :model-input)))
    (should (equal "First body\n\nSecond body" (plist-get outcome :render-prompt)))))


;;
;;; Planning

(niwa-deftest niwa-skills-plan-user-input ()
  ,test
  (test)
  :doc "combines only explicitly selected skills while argument tokens stay literal"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "selected-skills-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (beta (niwa-skills-plan-test--skill root "beta"))
         (gamma (niwa-skills-plan-test--skill root "gamma"))
         (session (niwa-skills-plan-test--session alpha beta gamma))
         (text "$gamma should stay literal"))
    (unwind-protect
        (let ((plan (niwa-skills-plan-user-input
                     text session '("alpha" "beta"))))
          (should (equal '("alpha" "beta")
                         (niwa-skills-plan-test--names
                          (niwa-skill-invocation-plan-entries plan))))
          (should (equal text (niwa-skill-invocation-plan-arguments plan)))
          (should (equal "$alpha $beta $gamma should stay literal"
                         (niwa-skill-invocation-plan-text plan)))
          (should-error (niwa-skills-plan-user-input
                         text session '("missing")) :type 'user-error))
      (delete-directory root t)))

  :doc "classifies prose mentions as ordered, deduplicated instructions"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (beta (niwa-skills-plan-test--skill root "beta"))
         (session (niwa-skills-plan-test--session alpha beta)))
    (unwind-protect
        (let ((plan (niwa-skills-plan-user-input
                     "Use $beta, then $alpha and $beta." session)))
          (should-not (niwa-skill-invocation-plan-commands plan))
          (should (equal '("beta" "alpha")
                         (niwa-skills-plan-test--names
                          (niwa-skill-invocation-plan-instructions plan))))
          (should (= 3 (length
                        (niwa-skill-invocation-plan-occurrences plan))))
          (should (cl-every
                   (lambda (occurrence)
                     (eq 'instruction
                         (niwa-skill-plan-occurrence-role occurrence)))
                   (niwa-skill-invocation-plan-occurrences plan))))
      (delete-directory root t)))

  :doc "leading inline commands stack, share arguments, and honor --"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (beta (niwa-skills-plan-test--skill root "beta"))
         (delta (niwa-skills-plan-test--skill root "delta"))
         (session (niwa-skills-plan-test--session alpha beta delta)))
    (unwind-protect
        (let ((plan (niwa-skills-plan-user-input
                     "$alpha $alpha $beta -- $delta details" session)))
          (should (equal '("alpha" "beta")
                         (niwa-skills-plan-test--names
                          (niwa-skill-invocation-plan-commands plan))))
          (should (equal '("delta")
                         (niwa-skills-plan-test--names
                          (niwa-skill-invocation-plan-instructions plan))))
          (should (equal "$delta details"
                         (niwa-skill-invocation-plan-arguments plan)))
          (should (equal '(command command command instruction)
                         (mapcar #'niwa-skill-plan-occurrence-role
                                 (niwa-skill-invocation-plan-occurrences
                                  plan)))))
      (delete-directory root t)))

  :doc "six distinct commands is fixed and the seventh starts instructions"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (skills (mapcar (lambda (name)
                           (niwa-skills-plan-test--skill root name))
                         '("a" "b" "c" "d" "e" "f" "g")))
         (session (apply #'niwa-skills-plan-test--session skills)))
    (unwind-protect
        (let ((plan (niwa-skills-plan-user-input
                     "$a $b $c $d $e $f $g rest" session)))
          (should (equal '("a" "b" "c" "d" "e" "f")
                         (niwa-skills-plan-test--names
                          (niwa-skill-invocation-plan-commands plan))))
          (should (equal '("g")
                         (niwa-skills-plan-test--names
                          (niwa-skill-invocation-plan-instructions plan))))
          (should (equal "$g rest"
                         (niwa-skill-invocation-plan-arguments plan))))
      (delete-directory root t)))

  :doc "a leading fork runs alone while later forks are instructions"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (fork (niwa-skills-plan-test--skill root "fork" 'fork))
         (session (niwa-skills-plan-test--session alpha fork)))
    (unwind-protect
        (let ((leading (niwa-skills-plan-user-input
                        "$fork inspect with $alpha" session))
              (later (niwa-skills-plan-user-input
                      "$alpha $fork inspect" session)))
          (should (niwa-skill-invocation-plan-fork-p leading))
          (should (equal '("fork")
                         (niwa-skills-plan-test--names
                          (niwa-skill-invocation-plan-commands leading))))
          (should (equal "inspect with $alpha"
                         (niwa-skill-invocation-plan-arguments leading)))
          (should (equal '("alpha")
                         (niwa-skills-plan-test--names
                          (niwa-skill-invocation-plan-instructions leading))))
          (should-not (niwa-skill-invocation-plan-fork-p later))
          (should (equal '("alpha")
                         (niwa-skills-plan-test--names
                          (niwa-skill-invocation-plan-commands later))))
          (should (equal '("fork")
                         (niwa-skills-plan-test--names
                          (niwa-skill-invocation-plan-instructions later)))))
      (delete-directory root t)))

  :doc "canonical source dedupe lets a command win over an alias instruction"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (alias (niwa-skills-plan-test--skill
                 root "alias" 'inline (niwa-skill-source-file alpha)))
         (beta (niwa-skills-plan-test--skill root "beta"))
         (session (niwa-skills-plan-test--session alpha alias beta)))
    (unwind-protect
        (let ((plan (niwa-skills-plan-user-input
                     "$alpha work with $alias and $beta" session)))
          (should (equal '("alpha" "beta")
                         (niwa-skills-plan-test--names
                          (niwa-skill-invocation-plan-entries plan))))
          (should (= 3 (length
                        (niwa-skill-invocation-plan-occurrences plan)))))
      (delete-directory root t)))

  :doc "unknown and intentionally literal syntax stays unplanned"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (session (niwa-skills-plan-test--session alpha)))
    (unwind-protect
        (let ((plan (niwa-skills-plan-user-input
                     "$unknown \\$alpha `$alpha` \"$alpha\"" session)))
          (should-not (niwa-skill-invocation-plan-occurrences plan)))
      (delete-directory root t)))

  :doc "malformed hidden bindings block direct planning"
  (let ((input (copy-sequence "Use $alpha")))
    (put-text-property
     4 10 'niwa-mention-binding
     '(:kind skill :token "$alpha") input)
    (should-error (niwa-skills-plan-user-input input nil)
                  :type 'user-error))

  :doc "unavailable skills stay literal when escaped, quoted, or in code"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (niwa-user-dir (make-temp-file "niwa-skill-plan-state-" t))
         (workspace (niwa-skills-test--make-workspace root))
         (disabled (niwa-skills-plan-test--skill
                    root "disabled" nil nil workspace))
         (internal (niwa-skills-plan-test--skill root "internal"))
         (session (niwa-session--create
                   :name "plan" :workspace workspace
                   :skills (list disabled internal)))
         (input (copy-sequence
                 "Use \\$disabled, \"$internal\", and `$missing`."))
         (missing-start (string-match "\\$missing" input)))
    (setf (niwa-skill-user-invocable-p internal) nil)
    (niwa-mention-bindings-set
     missing-start (+ missing-start (length "$missing"))
     (list :kind 'skill :token "$missing"
           :source-file (file-name-concat root "missing/SKILL.md"))
     input)
    (unwind-protect
        (progn
          (niwa-skills-set-enabled disabled nil)
          (let ((plan (niwa-skills-plan-user-input input session)))
            (should-not (niwa-skill-invocation-plan-occurrences plan))))
      (delete-directory root t)
      (delete-directory niwa-user-dir t)))

  :doc "missing exact bindings, disabled skills, and invocation gates continue"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (niwa-user-dir (make-temp-file "niwa-skill-plan-state-" t))
         (workspace (niwa-skills-test--make-workspace root))
         (disabled (niwa-skills-plan-test--skill
                    root "disabled" nil nil workspace))
         (internal (niwa-skills-plan-test--skill root "internal"))
         (session (niwa-session--create
                   :name "plan" :workspace workspace
                   :skills (list disabled internal)))
         (bound (copy-sequence "Use $missing"))
         (missing (file-name-concat root "missing/SKILL.md")))
    (setf (niwa-skill-user-invocable-p internal) nil)
    (niwa-mention-bindings-set
     4 12
     (list :kind 'skill :token "$missing" :source-file missing)
     bound)
    (unwind-protect
        (progn
          (niwa-skills-set-enabled disabled nil)
          (dolist (case `((,bound . "unavailable")
                          ("$disabled" . "disabled")
                          ("$internal" . "not user-invocable")))
            (let* ((plan
                    (niwa-skills-plan-user-input (car case) session))
                   (unavailable
                    (cl-find-if
                     (lambda (occurrence)
                       (eq 'unavailable
                           (niwa-skill-plan-occurrence-role occurrence)))
                     (niwa-skill-invocation-plan-occurrences plan)))
                   prepared)
              (should (string-match-p
                       (cdr case)
                       (niwa-skill-plan-occurrence-message unavailable)))
              (niwa-skills-plan-prepare
               plan (lambda (value) (setq prepared value)))
              (should (eq 'ok (plist-get prepared :status)))
              (should (string-match-p
                       "\\[skill:.* -- unavailable\\]"
                       (plist-get prepared :model-input)))
              (should (plist-get prepared :warnings)))))
      (delete-directory root t)
      (delete-directory niwa-user-dir t))))

(niwa-deftest niwa-skills-plan-replace-instructions ()
  ,test
  (test)
  :doc "replaces selected instruction extents in a source slice without scanning"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (beta (niwa-skills-plan-test--skill root "beta"))
         (delta (niwa-skills-plan-test--skill root "delta"))
         (session (niwa-skills-plan-test--session alpha beta delta))
         (input "$alpha inspect $beta with $delta")
         (plan (niwa-skills-plan-user-input input session))
         (offset (niwa-skill-invocation-plan-arguments-start plan))
         (slice (substring input offset))
         (instructions (niwa-skill-invocation-plan-occurrences plan)))
    (unwind-protect
        (progn
          (should (equal "inspect [skill:beta -- attached] with [skill:delta -- attached]"
                         (niwa-skills-plan-replace-instructions
                          slice instructions offset)))
          (should (equal "inspect $beta with [skill:delta -- attached]"
                         (niwa-skills-plan-replace-instructions
                          slice (last instructions) offset))))
      (delete-directory root t))))

(niwa-deftest niwa-skills-plan-prepare ()
  ,test
  (test)
  :doc "prepares unique commands then instructions and composes inert arguments"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (beta (niwa-skills-plan-test--skill root "beta"))
         (delta (niwa-skills-plan-test--skill root "delta"))
         (session (niwa-skills-plan-test--session alpha beta delta))
         (plan (niwa-skills-plan-user-input
                "$alpha $beta inspect $delta and $alpha" session))
         calls
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-skills-prepare-many)
                   (lambda (roots callback &rest _)
                     (let (outcomes records audits)
                       (dolist (root roots)
                         (let* ((skill (plist-get root :skill))
                                (arguments (plist-get root :arguments))
                                (name (niwa-skill-name skill))
                                (role (plist-get root :role))
                                (record (list name role)))
                           (setq calls
                                 (append calls
                                         (list
                                          (list name arguments role
                                                (plist-get
                                                 root :policy-owner-p)))))
                           (push record records)
                           (push (concat "audit-" name) audits)
                           (push
                            (list :status 'ok
                                  :skill skill
                                  :arguments arguments
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
                                        :invoked-skills (list record)))
                            outcomes)))
                       (funcall callback
                                (list :status 'ok
                                      :outcomes (nreverse outcomes)
                                      :invoked-skills
                                      (nreverse records)
                                      :hook-audits (nreverse audits)))))))
          (niwa-skills-plan-prepare plan (lambda (value) (setq result value))))
      (delete-directory root t))
    (should (equal '("alpha" "beta" "delta") (mapcar #'car calls)))
    (should (equal '(nil nil nil) (mapcar #'cadddr calls)))
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
    (should (= 3 (length (plist-get result :prepared-entries))))
    (should (equal
             "alpha{inspect [skill:delta -- attached] and [skill:alpha -- attached]} $hook-literal\n\nbeta{inspect [skill:delta -- attached] and [skill:alpha -- attached]} $hook-literal"
             (plist-get result :render-prompt)))
    (should (equal '("delta")
                   (mapcar
                    (lambda (attachment) (plist-get attachment :name))
                    (plist-get result :render-attachments)))))

  :doc "one command retains model and effort while command source wins dedupe"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (alias (niwa-skills-plan-test--skill
                 root "alias" 'inline (niwa-skill-source-file alpha)))
         (session (niwa-skills-plan-test--session alpha alias))
         (plan (niwa-skills-plan-user-input
                "$alpha use $alias" session))
         calls
         owner
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-skills-prepare-many)
                   (lambda (roots callback &rest _)
                     (let* ((root (car roots))
                            (skill (plist-get root :skill))
                            (arguments (plist-get root :arguments)))
                       (push (niwa-skill-name skill) calls)
                       (setq owner (plist-get root :policy-owner-p))
                       (funcall callback
                                (list :status 'ok :invoked-skills '(record)
                                      :outcomes
                                      (list
                                       (list :status 'ok :skill skill
                                             :arguments arguments
                                             :body arguments
                                             :request-context
                                             '(:model selected :effort high
                                               :invoked-skills (record))))))))))
          (niwa-skills-plan-prepare plan (lambda (value) (setq result value))))
      (delete-directory root t))
    (should (equal '("alpha") calls))
    (should owner)
    (should (eq 'selected
                (plist-get (plist-get result :request-context) :model)))
    (should (eq 'high
                (plist-get (plist-get result :request-context) :effort)))
    (should (string-match-p (regexp-quote "[skill:alias -- attached]")
                            (plist-get result :model-input))))

  :doc "instruction-only plans add unique reminders to placeholderized prose"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (session (niwa-skills-plan-test--session alpha))
         (plan (niwa-skills-plan-user-input
                "Use $alpha and $alpha; keep $body-literal" session))
         owner
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-skills-prepare-many)
                   (lambda (roots callback &rest _)
                     (let ((root (car roots)))
                       (should (equal "" (plist-get root :arguments)))
                       (setq owner (plist-get root :policy-owner-p))
                       (funcall callback
                                (list :status 'ok :invoked-skills '(record)
                                      :outcomes
                                      (list
                                       (list :status 'ok
                                             :skill (plist-get root :skill)
                                             :arguments ""
                                             :body "Body says $nested"
                                             :request-context
                                             '(:invoked-skills (record))))))))))
          (niwa-skills-plan-prepare plan (lambda (value) (setq result value))))
      (delete-directory root t))
    (should-not owner)
    (should (= 1 (length (plist-get result :prepared-entries))))
    (should
     (string-match-p
      (regexp-quote
       "Use [skill:alpha -- attached] and [skill:alpha -- attached]; keep $body-literal")
      (plist-get result :model-input)))
    (should (string-match-p (regexp-quote "$nested")
                            (plist-get result :model-input)))
    (should (string-match-p (regexp-quote "$body-literal")
                            (plist-get result :model-input)))
    (should (equal
             "Use [skill:alpha -- attached] and [skill:alpha -- attached]; keep $body-literal"
             (plist-get result :render-prompt)))
    (should (equal '("alpha")
                   (mapcar
                    (lambda (attachment) (plist-get attachment :name))
                    (plist-get result :render-attachments)))))

  :doc "invalid skill and preset policy is ignored by stacks and instructions"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (beta (niwa-skills-plan-test--skill root "beta"))
         (delta (niwa-skills-plan-test--skill root "delta"))
         (session (niwa-skills-plan-test--session alpha beta delta))
         (niwa-model-workloads
          '(($alpha :provider "Missing:alpha" :effort impossible)
            ($beta :provider "Missing:beta" :effort impossible)
            ($delta :provider "Missing:delta" :effort impossible)))
         stacked-result
         instruction-result)
    (setf (niwa-skill-model alpha) "invalid alpha"
          (niwa-skill-effort alpha) 'impossible
          (niwa-skill-model beta) "invalid beta"
          (niwa-skill-effort beta) 'impossible
          (niwa-skill-model delta) "invalid delta"
          (niwa-skill-effort delta) 'impossible)
    (unwind-protect
        (progn
          (niwa-skills-plan-prepare
           (niwa-skills-plan-user-input
            "$alpha $beta inspect" session)
           (lambda (value) (setq stacked-result value)))
          (niwa-skills-plan-prepare
           (niwa-skills-plan-user-input
            "Use $delta to inspect" session)
           (lambda (value) (setq instruction-result value))))
      (delete-directory root t))
    (should (eq 'ok (plist-get stacked-result :status)))
    (should (eq 'ok (plist-get instruction-result :status)))
    (should-not (plist-member (plist-get stacked-result :request-context)
                              :model))
    (should-not (plist-member (plist-get stacked-result :request-context)
                              :effort))
    (should-not (plist-member (plist-get instruction-result :request-context)
                              :model))
    (should-not (plist-member (plist-get instruction-result :request-context)
                              :effort)))

  :doc "one owner retains preset-over-frontmatter policy in aggregate context"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (session (niwa-skills-plan-test--session alpha))
         (niwa-model-tiers '((strong)))
         (niwa-model-workloads '(($alpha :tier strong :effort high)))
         (gptel-backend 'session-backend)
         (gptel-model 'session-model)
         (gptel-reasoning-effort 'medium)
         result)
    (setf (niwa-skill-model alpha) "invalid superseded selector"
          (niwa-skill-effort alpha) 'low)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-model-validate-effort)
                   (lambda (_model effort) effort)))
          (niwa-skills-plan-prepare
           (niwa-skills-plan-user-input "$alpha inspect" session)
           (lambda (value) (setq result value))))
      (delete-directory root t))
    (should (eq 'ok (plist-get result :status)))
    (should (equal '(:tier strong)
                   (plist-get (plist-get result :request-context) :model)))
    (should (eq 'high
                (plist-get (plist-get result :request-context) :effort))))

  :doc "the first failure short-circuits remaining entries"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (beta (niwa-skills-plan-test--skill root "beta"))
         (delta (niwa-skills-plan-test--skill root "delta"))
         (session (niwa-skills-plan-test--session alpha beta delta))
         (plan (niwa-skills-plan-user-input
                "$alpha $beta use $delta" session))
         calls
         result)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-skills-prepare-many)
                   (lambda (roots callback &rest _)
                     (catch 'failed
                       (dolist (root roots)
                         (let ((name
                                (niwa-skill-name
                                 (plist-get root :skill))))
                           (push name calls)
                           (when (equal name "beta")
                             (funcall callback
                                      '(:status error :reason failed
                                        :message "beta failed" :name "beta"))
                             (throw 'failed nil))))))))
          (niwa-skills-plan-prepare plan (lambda (value) (setq result value))))
      (delete-directory root t))
    (should (equal '("beta" "alpha") calls))
    (should (eq 'failed (plist-get result :reason)))
    (should (equal "beta" (plist-get result :name))))

  :doc "cancellation settles once and ignores repeated late callbacks"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-" t))
         (alpha (niwa-skills-plan-test--skill root "alpha"))
         (beta (niwa-skills-plan-test--skill root "beta"))
         (session (niwa-skills-plan-test--session alpha beta))
         (plan (niwa-skills-plan-user-input "$alpha $beta work" session))
         cancelled
         pending
         prepares
         results)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-skills-prepare-many)
                   (lambda (_roots callback &rest _)
                     (setq prepares (1+ (or prepares 0))
                           pending callback))))
          (niwa-skills-plan-prepare
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

(niwa-deftest niwa-skills-plan-render-data ()
  ,test
  (test)
  :doc "uses the hidden render envelope with structured prompt data"
  (let* ((text (propertize "$alpha original" 'niwa-mention-binding
                           '(:kind skill)))
         (prepared '(:render-prompt "Prepared root"
                     :render-attachments
                     ((:name "child" :body "Child body"))))
         (plan (niwa-skill-invocation-plan--create
                :text text))
         (block (niwa-skills-plan-render-data plan prepared))
         (data (cdr (niwa-tool-render-data-extract block))))
    (should (eq 'niwa-render-data (get-text-property 0 'gptel block)))
    (should (eq 'inline-skill (plist-get data :kind)))
    (should (equal text (plist-get data :display-text)))
    (should (equal "Prepared root" (plist-get data :prompt)))
    (should (equal "child"
                   (plist-get (car (plist-get data :attachments)) :name)))
    (should (string-match-p
             "Child body"
             (plist-get (car (plist-get data :attachments)) :body)))))

(niwa-deftest niwa-skills-plan-prepare-recursive ()
  ,test
  (test)
  :doc "rejects cross-root required-argument conflicts before expansion"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-graph-" t))
         (dir (file-name-concat root ".niwa" "skills"))
         (workspace (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "plan-graph" workspace root))
         expanded
         result)
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "child" "name: child\ndescription: Child\n" "Child")
          (niwa-skills-test--write-skill
           dir "one" "name: one\ndescription: One\n" "!$child -- ONE")
          (niwa-skills-test--write-skill
           dir "two" "name: two\ndescription: Two\n" "!$child -- TWO")
          (setf (niwa-session-skills session)
                (niwa-skills-scan root '(".niwa/skills") workspace))
          (with-temp-buffer
            (setq-local niwa--session session)
            (cl-letf (((symbol-function
                        'niwa-skills-preparation-expand-body)
                       (lambda (&rest _) (setq expanded t))))
              (niwa-skills-plan-prepare
               (niwa-skills-plan-user-input "$one $two inspect" session)
               (lambda (value) (setq result value)))))
          (should (eq 'dependency-conflict (plist-get result :reason)))
          (should-not expanded))
      (delete-directory root t)))

  :doc "aggregates required instruction context before placeholderized prose"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "niwa-skill-plan-attach-" t))
         (dir (file-name-concat root ".niwa" "skills"))
         (workspace (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "plan-attach" workspace root))
         result)
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           dir "child" "name: child\ndescription: Child\n" "Child contract")
          (niwa-skills-test--write-skill
           dir "parent" "name: parent\ndescription: Parent\n"
           "Parent contract\n!$child")
          (setf (niwa-session-skills session)
                (niwa-skills-scan root '(".niwa/skills") workspace))
          (with-temp-buffer
            (setq-local niwa--session session)
            (niwa-skills-plan-prepare
             (niwa-skills-plan-user-input "Use $parent here" session)
             (lambda (value) (setq result value))))
          (should (eq 'ok (plist-get result :status)))
          (let ((input (plist-get result :model-input))
                (records (plist-get (plist-get result :request-context)
                                    :invoked-skills)))
            (should (< (string-match-p "Child contract" input)
                       (string-match-p "Parent contract" input)))
            (should (string-match-p
                     (regexp-quote "Use [skill:parent -- attached] here")
                     input))
            (should (equal '("child" "parent")
                           (mapcar #'niwa-skill-invocation-record-name
                                   records)))))
      (delete-directory root t)))

  :doc "stacked source-backed commands disclose sources once while render bodies stay raw"
  (let* ((niwa-skills-check-for-modifications nil)
         (root (make-temp-file "skill-source-stack-" t))
         (local (file-name-concat root ".niwa" "skills"))
         (other (file-name-concat root ".agents" "skills"))
         (workspace (niwa-skills-test--make-workspace root))
         (session (niwa-session-create "source-stack" workspace root))
         result)
    (unwind-protect
        (progn
          (niwa-skills-test--write-skill
           local "base" "name: base\ndescription: Base\n" "BASE RAW")
          (niwa-skills-test--write-skill
           local "one" "name: one\ndescription: One\n"
           "ONE RAW\n!$base")
          (niwa-skills-test--write-skill
           other "two" "name: two\ndescription: Two\n"
           "TWO RAW\n!$base")
          (setf (niwa-session-skills session)
                (niwa-skills-scan
                 root '(".niwa/skills" ".agents/skills") workspace))
          (with-temp-buffer
            (setq-local niwa--session session)
            (niwa-skills-plan-prepare
             (niwa-skills-plan-user-input "$one $two $one inspect" session)
             (lambda (value) (setq result value))))
          (should (eq 'ok (plist-get result :status)))
          (let ((input (plist-get result :model-input))
                (render (plist-get result :render-prompt))
                (previous -1))
            (dolist (name '("base" "one" "two"))
              (let* ((skill (niwa-session-get-skill session name))
                     (address (niwa-resource--skill-address skill))
                     (position (string-search address input)))
                (should position)
                (should (< previous position))
                (should-not (string-search address input (1+ position)))
                (setq previous position)))
            (should (equal "ONE RAW\n[skill:base -- attached]\n\nARGUMENTS: inspect\n\nTWO RAW\n[skill:base -- attached]\n\nARGUMENTS: inspect"
                           render))
            (should-not (string-search "Skill source:" render))
            (should (equal '("base" "one" "two")
                           (mapcar #'niwa-skill-invocation-record-name
                                   (plist-get (plist-get result :request-context)
                                              :invoked-skills))))
            (dolist (record (plist-get (plist-get result :request-context)
                                      :invoked-skills))
              (should-not (string-search
                           "Skill source:"
                           (niwa-skill-invocation-record-prepared-body record))))))
      (delete-directory root t))))

(provide 'test-niwa-skills-plan)
;;; test-niwa-skills-plan.el ends here
