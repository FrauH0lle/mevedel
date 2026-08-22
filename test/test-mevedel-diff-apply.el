;;; test-mevedel-diff-apply.el --- Transactional diff application tests -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'mevedel)
(require 'mevedel-diff-apply)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defun mevedel-test--create-diff-buffer
    (modified file-buffer &optional workspace-root)
  "Return a unified diff buffer from FILE-BUFFER to MODIFIED.
Resolve labels relative to WORKSPACE-ROOT when non-nil."
  (let* ((original-file (buffer-file-name file-buffer))
         (original
          (if (file-exists-p original-file)
              (with-temp-buffer
                (insert-file-contents-literally original-file)
                (buffer-string))
            ""))
         (source-file
          (if (file-exists-p original-file)
              original-file
            (make-temp-file "mevedel-test-original-" nil ".txt")))
         (modified-file
          (make-temp-file "mevedel-test-modified-" nil ".txt"))
         (relative
          (file-relative-name original-file
                              (or workspace-root temporary-file-directory)))
         (buffer (generate-new-buffer " *mevedel-test-diff*")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert modified)
            (let ((coding-system-for-write
                   (buffer-local-value 'buffer-file-coding-system file-buffer)))
              (write-region (point-min) (point-max) modified-file nil 'silent)))
          (with-current-buffer buffer
            (insert (format "diff --git a/%s b/%s\n" relative relative))
            (cond
             ((and (string-empty-p original)
                   (not (string-empty-p modified)))
              (insert "new file mode 100644\n"))
             ((and (not (string-empty-p original))
                   (string-empty-p modified))
              (insert "deleted file mode 100644\n")))
            (let ((coding-system-for-read
                   (buffer-local-value 'buffer-file-coding-system file-buffer)))
              (call-process "diff" nil t nil "-u"
                            "--label" (if (string-empty-p original)
                                          "/dev/null"
                                        (concat "a/" relative))
                            "--label" (if (string-empty-p modified)
                                          "/dev/null"
                                        (concat "b/" relative))
                            source-file modified-file))
            (diff-mode)
            (read-only-mode +1)))
      (delete-file modified-file)
      (unless (equal source-file original-file)
        (delete-file source-file)))
    buffer))

(defun mevedel-test--create-multi-diff-buffer (entries &optional workspace-root)
  "Return one diff buffer for ENTRIES of (MODIFIED FILE-BUFFER).
Resolve labels relative to WORKSPACE-ROOT when non-nil."
  (let ((result (generate-new-buffer " *mevedel-test-multi-diff*")))
    (dolist (entry entries)
      (let ((part (mevedel-test--create-diff-buffer
                   (car entry) (cadr entry) workspace-root)))
        (unwind-protect
            (with-current-buffer result
              (insert-buffer-substring part))
          (kill-buffer part))))
    (with-current-buffer result
      (diff-mode)
      (read-only-mode +1))
    result))

(defun mevedel-test--overlay-on-text (buffer text type)
  "Register a TYPE instruction overlay on TEXT in BUFFER.
TYPE is `reference' or `directive'."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward (regexp-quote text) nil t)
        (error "Fixture text not found in %s" (buffer-name)))
      (if (eq type 'reference)
          (mevedel--create-reference-in
           buffer (match-beginning 0) (match-end 0))
        (mevedel--create-directive-in
         buffer (match-beginning 0) (match-end 0) nil "rename outputs")))))

(defun mevedel-test--overlay-text (overlay)
  "Return the text OVERLAY currently covers."
  (with-current-buffer (overlay-buffer overlay)
    (buffer-substring-no-properties
     (overlay-start overlay) (overlay-end overlay))))

(defun mevedel-test--apply-diff
    (diff-buffer workspace-file &optional workspace-root)
  "Apply DIFF-BUFFER in a temporary workspace identified by WORKSPACE-FILE.
Use WORKSPACE-ROOT as its root when non-nil."
  (let ((workspace-root (or workspace-root temporary-file-directory)))
    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _)
                 (mevedel-workspace-get-or-create
                  'file workspace-file workspace-root
                  (file-name-nondirectory workspace-file)))))
      (with-current-buffer diff-buffer
        (let ((default-directory workspace-root)
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))))


;;
;;; mevedel--path-has-suffix-p

(mevedel-deftest mevedel--path-has-suffix-p ()
  ,test
  (test)
  :doc "matches a complete trailing path"
  (should (mevedel--path-has-suffix-p "/tmp/dev/null" "dev/null"))
  :doc "rejects a partial component match"
  (should-not (mevedel--path-has-suffix-p "/tmp/notdev/null" "dev/null")))


;;
;;; mevedel-diff-apply--stage-buffer

(mevedel-deftest mevedel-diff-apply--stage-buffer ()
  ,test
  (test)
  :doc "applies resolved edits from the end of a snapshot"
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (should
     (equal "one\nTWO\nTHREE\n"
            (mevedel-diff-apply--stage-buffer
             (current-buffer)
             '((:pos (9 . 15) :dst ("THREE\n"))
               (:pos (5 . 9) :dst ("TWO\n")))))))
  :doc "stages the complete source despite narrowing"
  (with-temp-buffer
    (insert "before\ninside\nafter\n")
    (narrow-to-region 8 15)
    (should
     (equal "before\nchanged\nafter\n"
            (mevedel-diff-apply--stage-buffer
             (current-buffer)
             '((:pos (8 . 15) :dst ("changed\n"))))))))


;;
;;; mevedel-diff-apply-buffer

(mevedel-deftest mevedel-diff-apply-buffer
  (:before-each
   (mevedel-workspace-clear-registry)
   (setf (mevedel--instruction-alist) nil)
   (setf (mevedel--instruction-id-counter) 0)
   (setf (mevedel--instruction-id-usage-map) (make-hash-table))
   (setf (mevedel--instruction-retired-ids) nil)
   (setq mevedel--instruction-states (make-hash-table :test #'equal))
   (setq mevedel--instruction-current-state-key :global)
   :after-each
   (mevedel-workspace-clear-registry))
  ,test
  (test)

  ;; MUST-WORK USER CASE.  This fixture is a real change a user made in an
  ;; R/Shiny module: three renamed `renderUI' outputs across separate hunks,
  ;; with one registered directive spanning the layout block and one reference
  ;; on each renamed definition line.  Do not delete this case when the diff
  ;; application implementation changes -- port it.  Change what it expects
  ;; only when the product requirement itself changes.
  :doc "keeps a directive and its references on their text across a real-world multi-hunk rename"
  (let* ((buffer-text "
      # plots is to high too display on one page and thus, it makes sense to
      # have a division between multiple pages.
      shiny$div(
        # Dynamic layout - columns adjust based on which cards are visible
        shiny$uiOutput(ns(\"dynamic_layout_ui\"))
      ),
      shiny$div(
        style = \"margin-top: 3px;\",
        shiny$uiOutput(ns(\"dynamic_layout_ui_pt2\"))
      ),
      shiny$div(
        style = \"margin-top: 3px;\",
        shiny$uiOutput(ns(\"dynamic_layout_ui_pt3\"))
      )
    )
  )

Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur


    # Create entire layout dynamically based on checkbox states
    output$dynamic_layout_ui <- shiny$renderUI({
      ns <- session$ns


Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur


    ### Intervention costs -----------------------------------------------------

    output$dynamic_layout_ui_pt2 <- shiny$renderUI({
      ns <- session$ns

      if (input$show_costs) {
        costs$ui(id = ns(\"costs\"))
      }
    })

    ### Impact maps ------------------------------------------------------------

    output$dynamic_layout_ui_pt3 <- shiny$renderUI({
      ns <- session$ns


")
         (directive-text "      shiny$div(\n        # Dynamic layout - columns adjust based on which cards are visible\n        shiny$uiOutput(ns(\"dynamic_layout_ui\"))\n      ),\n      shiny$div(\n        style = \"margin-top: 3px;\",\n        shiny$uiOutput(ns(\"dynamic_layout_ui_pt2\"))\n      ),\n      shiny$div(\n        style = \"margin-top: 3px;\",\n        shiny$uiOutput(ns(\"dynamic_layout_ui_pt3\"))\n      )\n")
         (reference-1-text "    output$dynamic_layout_ui <- shiny$renderUI({\n")
         (reference-2-text "    output$dynamic_layout_ui_pt2 <- shiny$renderUI({\n")
         (reference-3-text "    output$dynamic_layout_ui_pt3 <- shiny$renderUI({\n")
         (new-text "
      # plots is to high too display on one page and thus, it makes sense to
      # have a division between multiple pages.
      shiny$div(
        # Main plots and maps layout
        shiny$uiOutput(ns(\"main_plots_maps_ui\"))
      ),
      shiny$div(
        style = \"margin-top: 3px;\",
        shiny$uiOutput(ns(\"costs_analysis_ui\"))
      ),
      shiny$div(
        style = \"margin-top: 3px;\",
        shiny$uiOutput(ns(\"impact_maps_ui\"))
      )
    )
  )

Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur


    # Create main plots and maps layout dynamically based on checkbox states
    output$main_plots_maps_ui <- shiny$renderUI({
      ns <- session$ns


Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur


    ### Intervention costs -----------------------------------------------------

    output$costs_analysis_ui <- shiny$renderUI({
      ns <- session$ns

      if (input$show_costs) {
        costs$ui(id = ns(\"costs\"))
      }
    })

    ### Impact maps ------------------------------------------------------------

    output$impact_maps_ui <- shiny$renderUI({
      ns <- session$ns


")
         (expected-directive "      shiny$div(\n        # Main plots and maps layout\n        shiny$uiOutput(ns(\"main_plots_maps_ui\"))\n      ),\n      shiny$div(\n        style = \"margin-top: 3px;\",\n        shiny$uiOutput(ns(\"costs_analysis_ui\"))\n      ),\n      shiny$div(\n        style = \"margin-top: 3px;\",\n        shiny$uiOutput(ns(\"impact_maps_ui\"))\n      )\n")
         (expected-ref1 "    output$main_plots_maps_ui <- shiny$renderUI({\n")
         (expected-ref2 "    output$costs_analysis_ui <- shiny$renderUI({\n")
         (expected-ref3 "    output$impact_maps_ui <- shiny$renderUI({\n")
         (file (make-temp-file "mevedel-test-real-world-" nil ".R" buffer-text))
         (file-buffer (find-file-noselect file))
         directive reference-1 reference-2 reference-3 diff-buffer)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function #'mevedel-workspace)
                     (lambda (&rest _)
                       (mevedel-workspace-get-or-create
                        'file file (file-name-directory file)
                        (file-name-nondirectory file)))))
            (setq directive (mevedel-test--overlay-on-text
                             file-buffer directive-text 'directive)
                  reference-1 (mevedel-test--overlay-on-text
                               file-buffer reference-1-text 'reference)
                  reference-2 (mevedel-test--overlay-on-text
                               file-buffer reference-2-text 'reference)
                  reference-3 (mevedel-test--overlay-on-text
                               file-buffer reference-3-text 'reference)
                  diff-buffer (mevedel-test--create-diff-buffer
                               new-text file-buffer))
            (with-current-buffer diff-buffer
              (goto-char (point-min))
              (should (< 1 (how-many "^@@"))))
            (with-current-buffer diff-buffer
              (let ((default-directory temporary-file-directory)
                    (inhibit-message t))
                (mevedel-diff-apply-buffer))))
          (with-current-buffer file-buffer
            (should (equal new-text (buffer-string)))
            (should (equal expected-directive
                           (mevedel-test--overlay-text directive)))
            (should (equal expected-ref1
                           (mevedel-test--overlay-text reference-1)))
            (should (equal expected-ref2
                           (mevedel-test--overlay-text reference-2)))
            (should (equal expected-ref3
                           (mevedel-test--overlay-text reference-3))))
          (should (equal new-text
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))))
      (when (buffer-live-p diff-buffer) (kill-buffer diff-buffer))
      (kill-buffer file-buffer)
      (delete-file file)))
  :doc "rejects ambiguous hunks without changing the diff or target"
  (let* ((original "alpha\nold\nomega\n")
         (file (make-temp-file "mevedel-test-ambiguous-" nil ".txt" original))
         (file-buffer (find-file-noselect file))
         (diff-buffer
          (mevedel-test--create-diff-buffer "alpha\nnew\nomega\n" file-buffer))
         patch-before
         first-error)
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (re-search-forward "^ alpha$")
              (beginning-of-line)
              (delete-char 1)
              (goto-char (point-max))
              (dotimes (_ 1000) (insert " \n"))
              (goto-char (point-min))
              (re-search-forward (regexp-quote "@@ -1,3 +1,3 @@"))
              (replace-match "@@ -1,1003 +1,1003 @@" t t))
            (setq patch-before (buffer-string)))
          (cl-letf (((symbol-function #'mevedel-workspace)
                     (lambda (&rest _)
                       (mevedel-workspace-get-or-create
                        'file file (file-name-directory file)
                        (file-name-nondirectory file))))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) (error "Unexpected prompt"))))
            (with-current-buffer diff-buffer
              (let ((default-directory temporary-file-directory))
                (setq first-error
                      (error-message-string
                       (should-error (mevedel-diff-apply-buffer t)))))))
          (should (string-match-p "Rejected ambiguous diff hunk" first-error))
          (with-current-buffer diff-buffer
            (should (equal patch-before (buffer-string))))
          (with-current-buffer file-buffer
            (should (equal original (buffer-string)))
            (should-not (buffer-modified-p)))
          (should (equal original
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))))
      (kill-buffer diff-buffer)
      (kill-buffer file-buffer)
      (delete-file file)))
  :doc "writes full final bytes from a narrowed source and retains a reference"
  (let* ((original
          "head\none\ntwo\nthree\nfour\nfive\ntarget\nsix\nseven\neight\nnine\nten\ntail\n")
         (expected
          "HEAD\none\ntwo\nthree\nfour\nfive\ntarget\nsix\nseven\neight\nnine\nten\nTAIL\n")
         (file (make-temp-file "mevedel-test-reference-" nil ".txt" original))
         (file-buffer (find-file-noselect file))
         reference
         diff-buffer)
    (unwind-protect
        (progn
          (with-current-buffer file-buffer
            (goto-char (point-min))
            (re-search-forward "^target$")
            (setq reference
                  (mevedel--create-reference-in
                   file-buffer (line-beginning-position) (line-beginning-position 2)))
            (narrow-to-region (overlay-start reference) (overlay-end reference)))
          (setq diff-buffer
                (mevedel-test--create-diff-buffer expected file-buffer))
          (with-current-buffer diff-buffer
            (goto-char (point-min))
            (should (= 2 (how-many "^@@"))))
          (mevedel-test--apply-diff diff-buffer file)
          (with-current-buffer file-buffer
            (should (= (point-min) (overlay-start reference)))
            (should (= (point-max) (overlay-end reference)))
            (save-restriction
              (widen)
              (should (equal expected (buffer-string))))
            (should (equal "target\n"
                           (buffer-substring-no-properties
                            (overlay-start reference) (overlay-end reference)))))
          (should (equal expected
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))))
      (when (buffer-live-p diff-buffer) (kill-buffer diff-buffer))
      (kill-buffer file-buffer)
      (delete-file file)))
  :doc "whole-range deletion detaches the registered directive"
  (let* ((original "before\nremove me\nafter\n")
         (file (make-temp-file "mevedel-test-directive-" nil ".txt" original))
         (file-buffer (find-file-noselect file))
         directive
         record
         diff-buffer)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function #'mevedel-workspace)
                     (lambda (&rest _)
                       (mevedel-workspace-get-or-create
                        'file file (file-name-directory file)
                        (file-name-nondirectory file)))))
            (with-current-buffer file-buffer
              (setq directive
                    (mevedel--create-directive-in file-buffer 8 18 nil "remove"))
              (setq record (mevedel--directive-record directive)))
            (setq diff-buffer
                  (mevedel-test--create-diff-buffer "before\nafter\n" file-buffer))
            (with-current-buffer diff-buffer
              (let ((default-directory temporary-file-directory)
                    (inhibit-message t))
                (mevedel-diff-apply-buffer))))
          (should record)
          (should (eq 'detached
                      (plist-get (mevedel-directive-anchor record) :state))))
      (when (buffer-live-p diff-buffer) (kill-buffer diff-buffer))
      (kill-buffer file-buffer)
      (delete-file file)))
  :doc "preserves literal bytes for a non-default file coding system"
  (let* ((original
          ";; -*- coding: iso-latin-1 -*-\nold: \u00e4\nkeep: \u00f6\n")
         (expected
          ";; -*- coding: iso-latin-1 -*-\nnew: \u00e4\nkeep: \u00f6\n")
         (file (make-temp-file "mevedel-test-coding-" nil ".el"))
         file-buffer diff-buffer coding)
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'iso-latin-1))
            (with-temp-file file (insert original)))
          (setq file-buffer (find-file-noselect file)
                coding (buffer-local-value 'buffer-file-coding-system
                                           file-buffer)
                diff-buffer
                (mevedel-test--create-diff-buffer expected file-buffer))
          (mevedel-test--apply-diff diff-buffer file)
          (should
           (equal (encode-coding-string expected coding)
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally file)
                    (buffer-string)))))
      (when (buffer-live-p diff-buffer) (kill-buffer diff-buffer))
      (when (buffer-live-p file-buffer) (kill-buffer file-buffer))
      (delete-file file)))
  :doc "creates a new file through the shared transaction"
  (let* ((file (make-temp-name
                (expand-file-name "mevedel-test-created-"
                                  temporary-file-directory)))
         (file-buffer (find-file-noselect file))
         (diff-buffer
          (mevedel-test--create-diff-buffer "created\n" file-buffer)))
    (unwind-protect
        (progn
          (mevedel-test--apply-diff diff-buffer file)
          (should (equal "created\n"
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))))
          (with-current-buffer file-buffer
            (should (equal "created\n" (buffer-string)))))
      (kill-buffer diff-buffer)
      (kill-buffer file-buffer)
      (when (file-exists-p file) (delete-file file))))
  :doc "deleting a file marks its registered directive source missing"
  (let* ((file (make-temp-file "mevedel-test-deleted-" nil ".txt" "old\n"))
         (file-buffer (find-file-noselect file))
         (diff-buffer (mevedel-test--create-diff-buffer "" file-buffer))
         directive record)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function #'mevedel-workspace)
                     (lambda (&rest _)
                       (mevedel-workspace-get-or-create
                        'file file (file-name-directory file)
                        (file-name-nondirectory file)))))
            (with-current-buffer file-buffer
              (setq directive
                    (mevedel--create-directive-in file-buffer 1 4 nil "delete"))
              (setq record (mevedel--directive-record directive)))
            (with-current-buffer diff-buffer
              (let ((default-directory temporary-file-directory)
                    (inhibit-message t))
                (mevedel-diff-apply-buffer))))
          (should-not (file-exists-p file))
          (should (buffer-live-p file-buffer))
          (should-not (overlay-buffer directive))
          (should (eq 'source-missing
                      (plist-get (mevedel-directive-anchor record) :state))))
      (kill-buffer diff-buffer)
      (kill-buffer file-buffer)
      (when (file-exists-p file) (delete-file file))))
  :doc "rolls back create placeholders when a later write fails"
  (let* ((root (make-temp-file "mevedel-test-write-rollback-" t))
         (first-dir (file-name-concat root "created" "first"))
         (sibling-dir (file-name-concat root "created" "sibling"))
         (second-dir (file-name-concat root "second"))
         (first (file-name-concat first-dir "one.txt"))
         (sibling (file-name-concat sibling-dir "sibling.txt"))
         (second (file-name-concat second-dir "two.txt"))
         first-buffer sibling-buffer second-buffer diff-buffer first-modtime
         prompted)
    (unwind-protect
        (progn
          (make-directory second-dir)
          (write-region "two\n" nil second nil 'silent)
          (setq first-buffer (find-file-noselect first)
                sibling-buffer (find-file-noselect sibling)
                second-buffer (find-file-noselect second)
                first-modtime
                (with-current-buffer first-buffer (visited-file-modtime))
                diff-buffer
                (mevedel-test--create-multi-diff-buffer
                 `(("ONE\n" ,first-buffer)
                   ("SIBLING\n" ,sibling-buffer)
                   ("TWO\n" ,second-buffer))
                 root))
          (mevedel-workspace-clear-registry)
          (set-file-modes second-dir #o500)
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (&rest _)
                       (setq prompted t)
                       first)))
            (should-error (mevedel-test--apply-diff diff-buffer first root)))
          (should-not prompted)
          (should-not (file-exists-p first))
          (should-not (file-exists-p sibling))
          (should-not (file-exists-p (file-name-concat root "created")))
          (should (equal "two\n"
                         (with-temp-buffer
                           (insert-file-contents second)
                           (buffer-string))))
          (with-current-buffer first-buffer
            (should (equal "" (buffer-string)))
            (should (equal first-modtime (visited-file-modtime)))
            (should (verify-visited-file-modtime first-buffer)))
          (with-current-buffer second-buffer
            (should (equal "two\n" (buffer-string)))))
      (when (file-directory-p second-dir) (set-file-modes second-dir #o700))
      (when (buffer-live-p diff-buffer) (kill-buffer diff-buffer))
      (when (buffer-live-p first-buffer) (kill-buffer first-buffer))
      (when (buffer-live-p sibling-buffer) (kill-buffer sibling-buffer))
      (when (buffer-live-p second-buffer) (kill-buffer second-buffer))
      (when (file-directory-p root) (delete-directory root t))))
  :doc "restores earlier deletions when a later deletion fails"
  (let* ((root (make-temp-file "mevedel-test-delete-rollback-" t))
         (first-dir (file-name-concat root "first"))
         (second-dir (file-name-concat root "second"))
         (first (file-name-concat first-dir "one.txt"))
         (second (file-name-concat second-dir "two.txt"))
         first-buffer second-buffer diff-buffer)
    (unwind-protect
        (progn
          (make-directory first-dir)
          (make-directory second-dir)
          (write-region "one\n" nil first nil 'silent)
          (write-region "two\n" nil second nil 'silent)
          (setq first-buffer (find-file-noselect first)
                second-buffer (find-file-noselect second)
                diff-buffer
                (mevedel-test--create-multi-diff-buffer
                 `(("" ,first-buffer) ("" ,second-buffer))))
          (set-file-modes second-dir #o500)
          (should-error (mevedel-test--apply-diff diff-buffer first))
          (should (equal "one\n"
                         (with-temp-buffer
                           (insert-file-contents first)
                           (buffer-string))))
          (should (equal "two\n"
                         (with-temp-buffer
                           (insert-file-contents second)
                           (buffer-string)))))
      (when (file-directory-p second-dir) (set-file-modes second-dir #o700))
      (when (buffer-live-p diff-buffer) (kill-buffer diff-buffer))
      (when (buffer-live-p first-buffer) (kill-buffer first-buffer))
      (when (buffer-live-p second-buffer) (kill-buffer second-buffer))
      (when (file-directory-p root) (delete-directory root t))))
  :doc "rolls back files and directive state when a later buffer hook fails"
  (let* ((root (make-temp-file "mevedel-test-sync-rollback-" t))
         (first (file-name-concat root "one.txt"))
         (second (file-name-concat root "two.txt"))
         first-buffer second-buffer diff-buffer directive record)
    (unwind-protect
        (progn
          (write-region "one\n" nil first nil 'silent)
          (write-region "two\n" nil second nil 'silent)
          (setq first-buffer (find-file-noselect first)
                second-buffer (find-file-noselect second)
                diff-buffer
                (mevedel-test--create-multi-diff-buffer
                 `(("" ,first-buffer) ("TWO\n" ,second-buffer))))
          (cl-letf (((symbol-function #'mevedel-workspace)
                     (lambda (&rest _)
                       (mevedel-workspace-get-or-create
                        'file first root (file-name-nondirectory first)))))
            (with-current-buffer first-buffer
              (setq directive
                    (mevedel--create-directive-in first-buffer 1 4 nil "keep"))
              (setq record (mevedel--directive-record directive)))
            (with-current-buffer second-buffer
              (add-hook 'before-change-functions
                        (lambda (&rest _) (error "Sync failure")) nil t))
            (with-current-buffer diff-buffer
              (let ((default-directory temporary-file-directory)
                    (inhibit-message t))
                (should-error (mevedel-diff-apply-buffer))))
            (with-current-buffer first-buffer
              (should
               (memq directive
                     (alist-get first-buffer
                                (mevedel--instruction-alist-value))))))
          (should (equal "one\n"
                         (with-temp-buffer
                           (insert-file-contents first)
                           (buffer-string))))
          (should (equal "two\n"
                         (with-temp-buffer
                           (insert-file-contents second)
                           (buffer-string))))
          (with-current-buffer first-buffer
            (should (equal "one\n" (buffer-string)))
            (should (eq first-buffer (overlay-buffer directive)))
            (should (= 1 (overlay-start directive)))
            (should (= 4 (overlay-end directive))))
          (should (eq 'attached
                      (plist-get (mevedel-directive-anchor record) :state)))
          (with-current-buffer second-buffer
            (should (equal "two\n" (buffer-string)))))
      (when (buffer-live-p diff-buffer) (kill-buffer diff-buffer))
      (when (buffer-live-p first-buffer) (kill-buffer first-buffer))
      (when (buffer-live-p second-buffer) (kill-buffer second-buffer))
      (when (file-directory-p root) (delete-directory root t)))))

(provide 'test-mevedel-diff-apply)

;;; test-mevedel-diff-apply.el ends here
