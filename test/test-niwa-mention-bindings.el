;;; test-niwa-mention-bindings.el --- Atomic mention binding tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-mention-bindings)


;;
;;; Helpers

(defun niwa-mention-bindings-test--bind (text token binding)
  "Return TEXT with TOKEN carrying BINDING."
  (let* ((copy (copy-sequence text))
         (start (string-match (regexp-quote token) copy)))
    (niwa-mention-bindings-set
     start (+ start (length token)) binding copy)
    copy))


;;
;;; Validation

(niwa-deftest niwa-mention-bindings-file-token-path ()
  ,test
  (test)
  :doc "decodes file tokens without disturbing match data or input properties"
  (dolist (case '(("" . "") ("plain\\file.el" . "plain\\file.el")
                  ("{" . "{") ("}" . "}") ("{}" . "")
                  ("{space #file.el}" . "space #file.el")
                  ("{a\\}b\\\\c}" . "a}b\\c")
                  ("{a\\qb\\}" . "aqb\\")
                  ("{a\nb}" . "a\nb")
                  ("{\u03bb.el}" . "\u03bb.el")))
    (string-match "\\(sentinel\\)" "sentinel")
    (let ((before (match-data)))
      (should (equal (cdr case)
                     (niwa-mention-bindings-file-token-path (car case))))
      (should (equal before (match-data)))))
  (let ((bare (propertize "file.el" 'face 'bold))
        (braced (propertize "{file.el}" 'face 'bold)))
    (should (eq bare (niwa-mention-bindings-file-token-path bare)))
    (should (equal-including-properties
             "file.el" (niwa-mention-bindings-file-token-path braced)))
    (should (get-text-property 0 'face braced))))

(niwa-deftest niwa-mention-bindings-set ()
  ,test
  (test)
  :doc "copies the locator and keeps appended text outside the binding"
  (with-temp-buffer
    (let ((binding '(:kind skill :token "$alpha"
                     :source-file "/skills/alpha/SKILL.md")))
      (insert "$alpha")
      (niwa-mention-bindings-set (point-min) (point-max) binding)
      (setf (plist-get binding :source-file) "/changed")
      (should
       (equal "/skills/alpha/SKILL.md"
              (plist-get
               (get-text-property (point-min) 'niwa-mention-binding)
               :source-file)))
      (goto-char (point-max))
      (insert "x")
      (should-not
       (get-text-property (1- (point)) 'niwa-mention-binding)))))

(niwa-deftest niwa-mention-bindings-ranges ()
  ,test
  (test)
  :doc "enumerates different bound kinds in occurrence order"
  (let* ((text (copy-sequence "$alpha then @ref:2"))
         (skill '(:kind skill :token "$alpha"
                  :source-file "/skills/alpha/SKILL.md"))
         (reference '(:kind ref :token "@ref:2"
                      :reference-uuid "uuid-2")))
    (niwa-mention-bindings-set 0 6 skill text)
    (niwa-mention-bindings-set 12 18 reference text)
    (should
     (equal `((:start 0 :end 6 :binding ,skill)
              (:start 12 :end 18 :binding ,reference))
            (niwa-mention-bindings-ranges text)))))

(niwa-deftest niwa-mention-bindings-skill-token-start-p ()
  ,test
  (test)
  :doc "accepts standalone dollar tokens but rejects identifier suffixes"
  (should (niwa-mention-bindings-skill-token-start-p "$" 0))
  (should (niwa-mention-bindings-skill-token-start-p "use $alpha" 4))
  (should-not
   (niwa-mention-bindings-skill-token-start-p "x$alpha" 1)))

(niwa-deftest niwa-mention-bindings-skill-token-occurrences ()
  ,test
  (test)
  :doc "retains exact names and ambiguous trailing punctuation candidates"
  (should
   (equal '((:start 6
             :candidates ("alpha..." "alpha.." "alpha." "alpha"))
            (:start 16 :candidates ("alpha.child"))
            (:start 30 :candidates ("beta")))
          (niwa-mention-bindings-skill-token-occurrences
           "x$bad $alpha... $alpha.child \\$beta"))))

(niwa-deftest niwa-mention-bindings-valid-p ()
  ,test
  (test)
  :doc "accepts every specified binding kind"
  (dolist (case
           '(("Use $alpha."
              "$alpha"
              (:kind skill :token "$alpha" :source-file "/skills/alpha/SKILL.md"))
             ("Use @ref:2."
              "@ref:2"
              (:kind ref :token "@ref:2" :reference-uuid "uuid-2"))
             ("Use @file:/tmp/a.el"
              "@file:/tmp/a.el"
              (:kind file :token "@file:/tmp/a.el" :path "/tmp/a.el"))
             ("Use @mcp:docs:file:///api"
              "@mcp:docs:file:///api"
              (:kind mcp :token "@mcp:docs:file:///api"
               :server "docs" :uri "file:///api"))))
    (should
     (niwa-mention-bindings-valid-p
      (niwa-mention-bindings-test--bind
       (nth 0 case) (nth 1 case) (nth 2 case)))))

  :doc "accepts self-delimited file bindings before punctuation"
  (dolist (case
           '(("Use @file:{/tmp/a b}."
              "@file:{/tmp/a b}"
              (:kind file :token "@file:{/tmp/a b}" :path "/tmp/a b"))
             ("Use @file:/tmp/a#L2."
              "@file:/tmp/a#L2"
              (:kind file :token "@file:/tmp/a#L2" :path "/tmp/a"))))
    (should
     (niwa-mention-bindings-valid-p
      (niwa-mention-bindings-test--bind
       (nth 0 case) (nth 1 case) (nth 2 case)))))

  :doc "rejects malformed and token-mismatched bindings"
  (let ((malformed (copy-sequence "Use $alpha")))
    (put-text-property 4 10 'niwa-mention-binding 'corrupt malformed)
    (should-not (niwa-mention-bindings-valid-p malformed)))
  (should-not
   (niwa-mention-bindings-valid-p
    (niwa-mention-bindings-test--bind
     "Use $alpha" "$alpha"
     '(:kind skill :token "$beta" :source-file "/skills/alpha/SKILL.md"))))

  :doc "rejects a binding whose lexical token extends past its property"
  (dolist (case
           '(("$alpha-child"
              "$alpha"
              (:kind skill :token "$alpha" :source-file "/skills/alpha/SKILL.md"))
             ("@ref:23"
              "@ref:2"
              (:kind ref :token "@ref:2" :reference-uuid "uuid-2"))
             ("@file:/tmp/a.elc"
              "@file:/tmp/a.el"
              (:kind file :token "@file:/tmp/a.el" :path "/tmp/a.el"))
             ("@mcp:docs:file:///api-v2"
              "@mcp:docs:file:///api"
              (:kind mcp :token "@mcp:docs:file:///api"
               :server "docs" :uri "file:///api"))))
    (should-not
     (niwa-mention-bindings-valid-p
      (niwa-mention-bindings-test--bind
       (nth 0 case) (nth 1 case) (nth 2 case))))))

(niwa-deftest niwa-mention-bindings-at ()
  ,test
  (test)
  :doc "returns only the exact kind and token extent"
  (let* ((input
          (niwa-mention-bindings-test--bind
           "Use $alpha." "$alpha"
           '(:kind skill :token "$alpha"
             :source-file "/skills/alpha/SKILL.md")))
         (start (string-match "\\$alpha" input)))
    (should (niwa-mention-bindings-at
             input start (+ start 6) 'skill "$alpha"))
    (should-not (niwa-mention-bindings-at
                 input start (+ start 7) 'skill "$alpha."))
    (should-not (niwa-mention-bindings-at
                 input start (+ start 6) 'ref "$alpha"))))

(niwa-deftest niwa-mention-bindings-starting-at ()
  ,test
  (test)
  :doc "finds only a binding beginning at the requested token start"
  (let* ((input
          (niwa-mention-bindings-test--bind
           "Use $alpha." "$alpha"
           '(:kind skill :token "$alpha"
             :source-file "/skills/alpha/SKILL.md")))
         (start (string-match "\\$alpha" input)))
    (should (niwa-mention-bindings-starting-at input start))
    (should-not (niwa-mention-bindings-starting-at input (1+ start)))))

(niwa-deftest niwa-mention-bindings-copy-text ()
  ,test
  (test)
  :doc "copies bindings and strips unrelated text properties"
  (let* ((input
          (niwa-mention-bindings-test--bind
           "$alpha text" "$alpha"
           '(:kind skill :token "$alpha"
             :source-file "/skills/alpha/SKILL.md")))
         (_ (put-text-property 7 11 'face 'bold input))
         (copy (niwa-mention-bindings-copy-text input)))
    (should (equal-including-properties
             (get-text-property 0 'niwa-mention-binding input)
             (get-text-property 0 'niwa-mention-binding copy)))
    (should-not (get-text-property 7 'face copy))))

(niwa-deftest niwa-mention-bindings-mixed-prompt ()
  ,test
  (test)
  :doc "mixed bindings copy and invalidate independently without UI state"
  (let* ((text (copy-sequence
                "$alpha @ref:2 @file:/tmp/a @mcp:docs:file:///api"))
         (specs
          '(("$alpha"
             (:kind skill :token "$alpha"
              :source-file "/skills/alpha/SKILL.md"))
            ("@ref:2"
             (:kind ref :token "@ref:2" :reference-uuid "uuid-2"))
            ("@file:/tmp/a"
             (:kind file :token "@file:/tmp/a" :path "/tmp/a"))
            ("@mcp:docs:file:///api"
             (:kind mcp :token "@mcp:docs:file:///api"
              :server "docs" :uri "file:///api")))))
    (dolist (spec specs)
      (let ((start (string-match (regexp-quote (car spec)) text)))
        (niwa-mention-bindings-set
         start (+ start (length (car spec))) (cadr spec) text)))
    (let ((copy (niwa-mention-bindings-copy-text text)))
      (should (equal specs
                     (mapcar
                      (lambda (range)
                        (list (substring copy
                                         (plist-get range :start)
                                         (plist-get range :end))
                              (plist-get range :binding)))
                      (niwa-mention-bindings-ranges copy))))
      (dolist (range (niwa-mention-bindings-ranges copy))
        (should-not (get-text-property
                     (plist-get range :start) 'face copy)))
      (with-temp-buffer
        (insert copy " and " copy)
        (goto-char (point-min))
        (search-forward "@ref:2")
        (let ((start (point)))
          (insert "3")
          (niwa-mention-bindings-invalidate-edit
           start (point) (point-min) (point-max)))
        (let ((ranges (niwa-mention-bindings-ranges (buffer-string))))
          (should (= 7 (length ranges)))
          (should (= 2 (cl-count 'skill ranges
                                 :key (lambda (range)
                                        (plist-get
                                         (plist-get range :binding) :kind)))))
          (should (= 1 (cl-count 'ref ranges
                                 :key (lambda (range)
                                        (plist-get
                                         (plist-get range :binding) :kind)))))
          (should (= 2 (cl-count 'file ranges
                                 :key (lambda (range)
                                        (plist-get
                                         (plist-get range :binding) :kind)))))
          (should (= 2 (cl-count 'mcp ranges
                                 :key (lambda (range)
                                        (plist-get
                                         (plist-get range :binding) :kind))))))))))


;;
;;; Live editing

(niwa-deftest niwa-mention-bindings-invalidate-edit ()
  ,test
  (test)
  :doc "invalidates only the edited or lexically extended occurrence"
  (with-temp-buffer
    (insert
     (niwa-mention-bindings-test--bind
      "$alpha and $beta" "$alpha"
      '(:kind skill :token "$alpha"
        :source-file "/skills/alpha/SKILL.md")))
    (let ((beta-start (save-excursion
                        (goto-char (point-min))
                        (search-forward "$beta")
                        (- (point) 5))))
      (niwa-mention-bindings-set
       beta-start (+ beta-start 5)
       '(:kind skill :token "$beta"
         :source-file "/skills/beta/SKILL.md")))
    (goto-char (+ (point-min) 6))
    (let ((start (point)))
      (insert "-child")
      (niwa-mention-bindings-invalidate-edit
       start (point) (point-min) (point-max)))
    (should-not (get-text-property (point-min) 'niwa-mention-binding))
    (should (text-property-not-all
             (point-min) (point-max) 'niwa-mention-binding nil)))

  :doc "preserves prose punctuation and outside edits, then invalidates a word suffix"
  (with-temp-buffer
    (insert
     (niwa-mention-bindings-test--bind
      "Use $alpha" "$alpha"
      '(:kind skill :token "$alpha"
        :source-file "/skills/alpha/SKILL.md")))
    (goto-char (point-min))
    (let ((start (point)))
      (insert "Please ")
      (niwa-mention-bindings-invalidate-edit
       start (point) (point-min) (point-max)))
    (should (get-text-property 12 'niwa-mention-binding))
    (goto-char (point-max))
    (let ((start (point)))
      (insert ".")
      (niwa-mention-bindings-invalidate-edit
       start (point) (point-min) (point-max)))
    (should (get-text-property 12 'niwa-mention-binding))
    (let ((start (point)))
      (insert "child")
      (niwa-mention-bindings-invalidate-edit
       start (point) (point-min) (point-max)))
    (should-not (get-text-property 12 'niwa-mention-binding)))

  :doc "editing a file range invalidates the whole occurrence"
  (with-temp-buffer
    (let ((token "@file:/tmp/a.el#L2"))
      (insert token)
      (niwa-mention-bindings-set
       (point-min) (point-max)
       (list :kind 'file :token token :path "/tmp/a.el"))
      (goto-char (point-max))
      (delete-char -1)
      (let ((start (point)))
        (insert "3")
        (niwa-mention-bindings-invalidate-edit
         start (point) (point-min) (point-max)))
      (should-not (text-property-not-all
                   (point-min) (point-max)
                   'niwa-mention-binding nil)))))

(provide 'test-niwa-mention-bindings)
;;; test-niwa-mention-bindings.el ends here
