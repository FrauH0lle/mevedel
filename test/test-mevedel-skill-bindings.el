;;; test-mevedel-skill-bindings.el --- Skill binding tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-mentions)
(require 'mevedel-skill-bindings)
(require 'mevedel-skills-core)
(require 'mevedel-structs)


;;
;;; Helpers

(defun mevedel-skill-bindings-test--bind (text name source-file)
  "Return TEXT with `$NAME' bound to SOURCE-FILE."
  (let* ((copy (copy-sequence text))
         (start (string-match (regexp-quote (concat "$" name)) copy)))
    (mevedel-mentions-set-binding
     start (+ start 1 (length name))
     (list :kind 'skill :name name :source-file source-file)
     copy)
    copy))


;;
;;; Validation

(mevedel-deftest mevedel-skill-bindings-token-start-p ()
  ,test
  (test)
  :doc "accepts standalone token starts, including a bare completion prefix"
  (should (mevedel-skill-bindings-token-start-p "$" 0))
  (should (mevedel-skill-bindings-token-start-p "use $alpha" 4))
  (should-not (mevedel-skill-bindings-token-start-p "x$alpha" 1)))

(mevedel-deftest mevedel-skill-bindings-token-occurrences ()
  ,test
  (test)
  :doc "owns lexical boundaries and ambiguous punctuation candidates"
  (should
   (equal '((:start 6
             :candidates ("alpha..." "alpha.." "alpha." "alpha"))
            (:start 16 :candidates ("alpha.child"))
            (:start 30 :candidates ("beta")))
          (mevedel-skill-bindings-token-occurrences
           "x$bad $alpha... $alpha.child \\$beta"))))

(mevedel-deftest mevedel-skill-bindings-valid-p ()
  ,test
  (test)
  :doc "accepts scanner-trimmed trailing punctuation"
  (let ((source (make-temp-file "mevedel-skill-binding-" nil ".md")))
    (unwind-protect
        (dolist (suffix '("." ":" "-"))
          (should
           (mevedel-skill-bindings-valid-p
            (mevedel-skill-bindings-test--bind
             (concat "use $alpha" suffix) "alpha" source))))
      (delete-file source)))

  :doc "rejects bindings embedded in an unambiguous longer identifier"
  (let ((source (make-temp-file "mevedel-skill-binding-" nil ".md")))
    (unwind-protect
        (should-not
         (mevedel-skill-bindings-valid-p
          (mevedel-skill-bindings-test--bind
           "use $alphax" "alpha" source)))
      (delete-file source)))

  :doc "rejects punctuation that continues into a longer skill name"
  (let ((source (make-temp-file "mevedel-skill-binding-" nil ".md")))
    (unwind-protect
        (dolist (suffix '(".child" ":child" "-child"))
          (should-not
           (mevedel-skill-bindings-valid-p
            (mevedel-skill-bindings-test--bind
             (concat "use $alpha" suffix) "alpha" source))))
      (delete-file source))))

  :doc "rejects corrupt non-plist binding values without signaling"
  (let ((input (copy-sequence "use $alpha")))
    (put-text-property 4 10 'mevedel-mention-binding 'corrupt input)
    (should-not (mevedel-skill-bindings-valid-p input)))

(mevedel-deftest mevedel-skill-bindings-sanitize ()
  ,test
  (test)
  :doc "removes invalid runs without changing visible text"
  (let* ((source (make-temp-file "mevedel-skill-binding-" nil ".md"))
         (input (mevedel-skill-bindings-test--bind
                 "use $alphax" "alpha" source))
         (clean nil))
    (unwind-protect
        (progn
          (setq clean (mevedel-skill-bindings-sanitize input))
          (should (equal "use $alphax" clean))
          (should-not (text-property-not-all
                       0 (length clean) 'mevedel-mention-binding nil clean)))
      (delete-file source))))

(mevedel-deftest mevedel-skill-bindings-at ()
  ,test
  (test)
  :doc "returns a binding only for its exact name and extent"
  (let* ((source (make-temp-file "mevedel-skill-binding-" nil ".md"))
         (input (mevedel-skill-bindings-test--bind
                 "use $alpha." "alpha" source))
         (start (string-match "\\$alpha" input)))
    (unwind-protect
        (progn
          (should (mevedel-skill-bindings-at
                   input start (+ start 6) "alpha"))
          (should-not (mevedel-skill-bindings-at
                       input start (+ start 7) "alpha."))
          (should-not (mevedel-skill-bindings-at
                       input start (+ start 6) "beta")))
      (delete-file source))))

(mevedel-deftest mevedel-skill-bindings-starting-at ()
  ,test
  (test)
  :doc "finds a valid binding through canonical punctuation candidates"
  (let* ((source (make-temp-file "mevedel-skill-binding-" nil ".md"))
         (input (mevedel-skill-bindings-test--bind
                 "use $alpha." "alpha" source))
         (start (string-match "\\$alpha" input)))
    (unwind-protect
        (progn
          (should (mevedel-skill-bindings-starting-at input start))
          (should-not (mevedel-skill-bindings-starting-at input (1+ start))))
      (delete-file source))))

(mevedel-deftest mevedel-skill-bindings-resolve ()
  ,test
  (test)
  :doc "an exact binding wins over same-name session lookup"
  (let* ((mevedel-skills-check-for-modifications nil)
         (bound-source (make-temp-file "mevedel-bound-source-" nil ".md"))
         (other-source (make-temp-file "mevedel-other-source-" nil ".md"))
         (bound-skill (mevedel-skill--create
                       :name "alpha" :source-file bound-source
                       :source 'project :active-p t :user-invocable-p t))
         (other-skill (mevedel-skill--create
                       :name "alpha" :source-file other-source
                       :source 'user :active-p t :user-invocable-p t))
         (session (mevedel-session--create
                   :name "main" :skills (list other-skill bound-skill)))
         (input (mevedel-skill-bindings-test--bind
                 "use $alpha" "alpha" bound-source))
         (start (string-match "\\$alpha" input)))
    (unwind-protect
        (with-temp-buffer
          (should (eq bound-skill
                      (mevedel-skill-bindings-resolve
                       input session start (+ start 6) "alpha"))))
      (delete-file bound-source)
      (delete-file other-source))))


;;
;;; Live editing

(mevedel-deftest mevedel-skill-bindings-invalidate-edit ()
  ,test
  (test)
  :doc "invalidates an edited token but preserves adjacent punctuation"
  (let ((source (make-temp-file "mevedel-skill-binding-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert (mevedel-skill-bindings-test--bind
                     "use $alpha" "alpha" source))
            (goto-char (point-max))
            (let ((start (point)))
              (insert ".")
              (mevedel-skill-bindings-invalidate-edit
               start (point) (point-min) (point-max)))
            (should (get-text-property 5 'mevedel-mention-binding))
            (let ((start (point)))
              (insert "b")
              (mevedel-skill-bindings-invalidate-edit
               start (point) (point-min) (point-max)))
            (should-not (get-text-property 5 'mevedel-mention-binding)))
          (with-temp-buffer
            (insert (mevedel-skill-bindings-test--bind
                     "use $alpha" "alpha" source))
            (goto-char (point-min))
            (let ((start (point)))
              (insert "Note: ")
              (mevedel-skill-bindings-invalidate-edit
               start (point) (point-min) (point-max)))
            (should (get-text-property 11 'mevedel-mention-binding))
            (goto-char (point-min))
            (search-forward "$alpha")
            (backward-char 3)
            (let ((start (point)))
              (insert "x")
              (mevedel-skill-bindings-invalidate-edit
               start (point) (point-min) (point-max)))
            (should-not (get-text-property 5 'mevedel-mention-binding))))
      (delete-file source))))

(provide 'test-mevedel-skill-bindings)
;;; test-mevedel-skill-bindings.el ends here
