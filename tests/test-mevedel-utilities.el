;;; tests/test-mevedel-utilities.el -- Unit tests for mevedel-utilities.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'mevedel)
(load (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "helpers"))

;; NOTE 2025-10-01: For better DEBUG output, use the following snippet in tests:
;;
;; (message "DEBUG TEST: ov-orig-start=%s ov-orig-end=%s" ov-orig-start ov-orig-end)
;; (message "DEBUG TEST: buffer-text=%S (len=%s)" buffer-text (length buffer-text))
;; (message "DEBUG TEST: ov-text=%S (len=%s)" ov-text (length ov-text))
;; (with-current-buffer test-buffer
;;   (message "DEBUG TEST: actual buffer content=%S" (buffer-substring-no-properties (point-min) (point-max)))
;;   (message "DEBUG TEST: actual overlay content=%S" (buffer-substring-no-properties ov-orig-start ov-orig-end)))
;; (with-current-buffer diff-buffer
;;   (let ((default-directory (temporary-file-directory))
;;         (inhibit-message t))
;;     (mevedel-diff-apply-buffer)))
;; (message "DEBUG TEST AFTER: ov-start=%s ov-end=%s" (overlay-start ov) (overlay-end ov))
;; (with-current-buffer test-buffer
;;   (message "DEBUG TEST AFTER: actual overlay content=%S" (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))

(describe "mevedel--diff-calculate-ov-adjustment:"

  (after-each
    ;; Clean up test buffers and files
    (dolist (buf (buffer-list))
      (when (string-match-p "mevedel-test" (buffer-name buf))
        (when (buffer-file-name buf)
          (let ((file (buffer-file-name buf)))
            (when (file-exists-p file)
              (delete-file file))))
        (kill-buffer buf))))

  (describe "Case 1a: Change is completely after the overlay"
    (it "Changes after overlay don't affect it"
      (let* ((buffer-text "Lorem ipsum dolor sit amet, consetetur


Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur
")
             (ov-text "Lorem ipsum dolor sit amet, consetetur\n")
             (new-text "Lorem ipsum dolor sit amet, consetetur


Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (ov-orig-start (overlay-start ov))
             (ov-orig-end (overlay-end ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        (expect (overlay-start ov) :to-equal ov-orig-start)
        (expect (overlay-end ov) :to-equal ov-orig-end))))


  (describe "Case 1b: Change starts exactly at overlay end (adjacent)"
    (it "Additions immediately after overlay should extend it"
      (let* ((buffer-text "

Lorem ipsum dolor sit amet, consetetur
")
             (ov-text "Lorem ipsum dolor sit amet, consetetur")
             (new-text "

Lorem ipsum dolor sit amet, conseteturLorem ipsum dolor sit amet, consetetur
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (ov-orig-start (overlay-start ov))
             (ov-orig-end (overlay-end ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        (expect (overlay-start ov) :to-equal ov-orig-start)
        (expect (overlay-end ov) :to-equal (+ ov-orig-end (length (nth 3 (mevedel--safe-string-diff-regions buffer-text new-text)))))))

    (it "Removals starting exactly at overlay end don't affect it"
      (let* ((buffer-text "
Lorem ipsum dolor sit amet, consetetur sadipscing
")
             (ov-text "Lorem ipsum dolor sit amet, consetetur")
             (new-text "
Lorem ipsum dolor sit amet, consetetur
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (ov-orig-start (overlay-start ov))
             (ov-orig-end (overlay-end ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        (expect (overlay-start ov) :to-equal ov-orig-start)
        (expect (overlay-end ov) :to-equal ov-orig-end)))

    (it "Replacements at overlay end extend it"
      (let* ((buffer-text "
Lorem ipsum dolor sit amet, consetetur sadipscing
")
             (ov-text "Lorem ipsum dolor sit amet, consetetur")
             (new-text "
Lorem ipsum dolor sit amet, consetetur11111111111
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (ov-orig-start (overlay-start ov))
             (ov-orig-end (overlay-end ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        (expect (overlay-start ov) :to-equal ov-orig-start)
        (expect (overlay-end ov) :to-equal (+ ov-orig-end (length "11111111111"))))))


  (describe "Case 2: Change is completely before the overlay"
    (it "Additions before overlay extend and shift it right by delta"
      (let* ((buffer-text "
Lorem ipsum dolor sit amet, consetetur sadipscing
")
             (ov-text "Lorem ipsum dolor sit amet, consetetur")
             (new-text "
INSERTED Lorem ipsum dolor sit amet, consetetur sadipscing
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (ov-orig-start (overlay-start ov))
             (ov-orig-end (overlay-end ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Overlay should now start at "inserted" and end after "bar"
        (expect (overlay-start ov) :to-equal 2)
        (expect (overlay-end ov) :to-equal (+ ov-orig-end (length "INSERTED ")))))

    (it "Removals/replacements before overlay shift it left by delta"
      (let* ((buffer-text "
REMOVELorem ipsum dolor sit amet, consetetur sadipscing
")
             (ov-text "Lorem ipsum dolor sit amet, consetetur")
             (new-text "
Lorem ipsum dolor sit amet, consetetur sadipscing
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (ov-orig-start (overlay-start ov))
             (ov-orig-end (overlay-end ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Overlay should shift left by the length of "removed\n"
        (expect (overlay-start ov) :to-equal 2)
        (expect (overlay-end ov) :to-equal (- ov-orig-end (length "REMOVE"))))))


  (describe "Case 3: Change completely encompasses the overlay"
    (it "When removing content that contains the overlay, move to safe position"
      (let* ((buffer-text "before
overlay content
after
")
             (ov-text "overlay content")
             (new-text "before
after
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Overlay should still exist and be at a safe position
        (expect (overlay-buffer ov) :to-equal test-buffer)
        (expect (overlay-start ov) :to-equal 1)
        (expect (overlay-end ov) :to-equal 2)))

    (it "When adding/replacing, overlay should cover the new content"
      (let* ((buffer-text "before
this is old content
after
")
             (ov-text "is old")
             (new-text "before
something new happened here
after
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (ov-orig-start (overlay-start ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Overlay should cover the new replacement text
        (expect (overlay-start ov) :to-equal 8)
        (expect (with-current-buffer test-buffer
                  (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
                :to-match "something new happened here"))))


  (describe "Case 4: Change is completely within the overlay"
    (it "Changes within overlay adjust end by delta (expansion)"
      (let* ((buffer-text "
prefix overlay content suffix
")
             (ov-text "overlay content")
             (new-text "
prefix overlay expanded content suffix
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (ov-orig-start (overlay-start ov))
             (ov-orig-end (overlay-end ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Start should remain the same, end should expand
        (expect (overlay-start ov) :to-equal ov-orig-start)
        (expect (overlay-end ov) :to-equal (+ ov-orig-end (length " expanded")))))

    (it "Changes within overlay adjust end by delta (shrinkage)"
      (let* ((buffer-text "
prefix overlay expanded content suffix
")
             (ov-text "overlay expanded content")
             (new-text "
prefix overlay content suffix
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (ov-orig-start (overlay-start ov))
             (ov-orig-end (overlay-end ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Start should remain the same, end should shrink
        (expect (overlay-start ov) :to-equal ov-orig-start)
        (expect (overlay-end ov) :to-equal (- ov-orig-end (length " expanded"))))))

  (describe "Case 5: Change overlaps the start of the overlay"
    (it "Adding content that overlaps overlay start extends it"
      (let* ((buffer-text "before text here
overlay text content
")
             (ov-text "here
overlay text content")
             (new-text "something else happened here inserted words
overlay text content
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (ov-orig-start (overlay-start ov))
             (ov-orig-end (overlay-end ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Overlay should extend to include the inserted content
        (expect (overlay-start ov) :to-equal 1)
        (expect (overlay-end ov) :to-equal (length new-text))
        (expect (with-current-buffer test-buffer
                  (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
                :to-match "something else happened here inserted words")))

    (it "Removing content that overlaps overlay start shrinks it"
      (let* ((buffer-text "before text here remove this
overlay text content
")
             (ov-text "here remove this
overlay text content")
             (new-text "here
overlay text content
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (ov-orig-start (overlay-start ov))
             (ov-orig-end (overlay-end ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Overlay should shrink - start moves to where the change started
        (expect (overlay-start ov) :to-equal 1)
        (expect (overlay-end ov) :to-equal (- ov-orig-end (length "before text remove this")))
        (expect (with-current-buffer test-buffer
                  (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
                :to-match "here\noverlay text content")))

    (it "Replacement spanning start extends it"
      (let* ((buffer-text "before text here
overlay text content
")
             (ov-text "here
overlay text content")
             (new-text "1111111111111111
overlay text content
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (ov-orig-start (overlay-start ov))
             (ov-orig-end (overlay-end ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Overlay should extend to include the inserted content
        (expect (overlay-start ov) :to-equal 1)
        (expect (overlay-end ov) :to-equal (length new-text))
        (expect (with-current-buffer test-buffer
                  (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
                :to-match "1111111111111111\noverlay text content"))))


  (describe "Case 6: Change overlaps the end of the overlay"
    (it "Adding content that overlaps overlay end extends it"
      (let* ((buffer-text "overlay text content
  after text here
")
             (ov-text "overlay text content")
             (new-text "overlay text newly inserted words
  something happened here
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (ov-orig-start (overlay-start ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Overlay should extend to include inserted content
        (expect (overlay-start ov) :to-equal ov-orig-start)
        (expect (overlay-end ov) :to-equal (- (length new-text) (length " here")))
        (expect (with-current-buffer test-buffer
                  (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
                :to-match "overlay text newly inserted words\n  something happened")))

    (it "Removing content that overlaps overlay end shrinks it"
      (let* ((buffer-text "overlay text content
  after text here and some more
")
             (ov-text "overlay text content
  after text here")
             (new-text "overlay text
  here
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (ov-orig-start (overlay-start ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Overlay should extend to include inserted content
        (expect (overlay-start ov) :to-equal ov-orig-start)
        (expect (overlay-end ov) :to-equal (- (length new-text) (length "\n  here")))
        (expect (with-current-buffer test-buffer
                  (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
                :to-match "overlay text")))

    (it "Replacement that overlaps overlay end extends it"
      (let* ((buffer-text "overlay text content
  after text here and some more
")
             (ov-text "overlay text content
  after text here")
             (new-text "overlay text content
  11111111111111111111111111111
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay buffer-text nil nil ov-text))
             (test-buffer (car buf-setup))
             (ov (cdr buf-setup))
             (ov-orig-start (overlay-start ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Overlay should extend to include inserted content
        (expect (overlay-start ov) :to-equal ov-orig-start)
        (expect (overlay-end ov) :to-equal (length new-text))
        (expect (with-current-buffer test-buffer
                  (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
                :to-match "overlay text content\n  11111111111111111111111111111")))))


(describe "Nested overlays:"
  (after-each
    ;; Clean up test buffers and files
    (dolist (buf (buffer-list))
      (when (string-match-p "mevedel-test" (buffer-name buf))
        (when (buffer-file-name buf)
          (let ((file (buffer-file-name buf)))
            (when (file-exists-p file)
              (delete-file file))))
        (kill-buffer buf))))

  (describe "Reference nested within directive"
    (it "Both overlays handled when change encompasses and removes both"
      ;; When overlays are completely encompassed by a removal, they are moved
      ;; to a safe position. If no safe position exists, they may be detached.
      (let* ((buffer-text "prefix
directive start
  reference content here
directive end
suffix
")
             (directive-text "directive start
  reference content here
directive end")
             (reference-text "reference content here")
             (new-text "prefix
completely new content
suffix
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay
                         buffer-text nil nil directive-text 'directive))
             (test-buffer (car buf-setup))
             (directive-ov (cdr buf-setup))
             ;; Create nested reference overlay
             (reference-ov (mevedel-tests--create-overlay
                            test-buffer nil nil reference-text 'reference))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Buffer should be modified correctly
        (expect (with-current-buffer test-buffer
                  (buffer-substring-no-properties (point-min) (point-max)))
                :to-match "completely new content")
        ;; Both overlays should still exist and span the new content
        (expect (overlay-buffer directive-ov) :to-equal test-buffer)
        (expect (overlay-buffer reference-ov) :to-equal test-buffer)
        ;; Directive should cover the new content
        (expect (with-current-buffer test-buffer
                  (buffer-substring-no-properties (overlay-start directive-ov) (overlay-end directive-ov)))
                :to-match "completely new content")
        ;; Reference should be nested within directive (child adjusted to fit
        ;; parent)
        (expect (>= (overlay-start reference-ov) (overlay-start directive-ov)) :to-be-truthy)
        (expect (<= (overlay-end reference-ov) (overlay-end directive-ov)) :to-be-truthy)))

    (it "Both overlays expand when change is within directive but after reference"
      (let* ((buffer-text "prefix
directive start
  reference content
  more directive content
directive end
")
             (directive-text "directive start
  reference content
  more directive content
directive end")
             (reference-text "reference content")
             (new-text "prefix
directive start
  reference content
  more directive EXPANDED content
directive end
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay
                         buffer-text nil nil directive-text 'directive))
             (test-buffer (car buf-setup))
             (directive-ov (cdr buf-setup))
             (directive-orig-start (overlay-start directive-ov))
             (directive-orig-end (overlay-end directive-ov))
             ;; Create nested reference overlay
             (reference-ov (mevedel-tests--create-overlay
                            test-buffer nil nil reference-text 'reference))
             (reference-orig-start (overlay-start reference-ov))
             (reference-orig-end (overlay-end reference-ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Directive should expand
        (expect (overlay-start directive-ov) :to-equal directive-orig-start)
        (expect (overlay-end directive-ov) :to-be-greater-than directive-orig-end)
        ;; Reference should stay the same (change was after it)
        (expect (overlay-start reference-ov) :to-equal reference-orig-start)
        (expect (overlay-end reference-ov) :to-equal reference-orig-end)))

    (it "Both overlays adjust when change overlaps reference within directive"
      (let* ((buffer-text "prefix
directive start
  reference content here
directive end
")
             (directive-text "directive start
  reference content here
directive end")
             (reference-text "reference content here")
             (new-text "prefix
directive start
  reference MODIFIED content here
directive end
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay
                         buffer-text nil nil directive-text 'directive))
             (test-buffer (car buf-setup))
             (directive-ov (cdr buf-setup))
             (directive-orig-start (overlay-start directive-ov))
             (directive-orig-end (overlay-end directive-ov))
             ;; Create nested reference overlay
             (reference-ov (mevedel-tests--create-overlay
                            test-buffer nil nil reference-text 'reference))
             (reference-orig-start (overlay-start reference-ov))
             (reference-orig-end (overlay-end reference-ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Both should expand by the added text length
        (expect (overlay-start directive-ov) :to-equal directive-orig-start)
        (expect (overlay-end directive-ov) :to-be-greater-than directive-orig-end)
        (expect (overlay-start reference-ov) :to-equal reference-orig-start)
        (expect (overlay-end reference-ov) :to-be-greater-than reference-orig-end)
        ;; Check that reference contains the modified text
        (expect (with-current-buffer test-buffer
                  (buffer-substring-no-properties (overlay-start reference-ov) (overlay-end reference-ov)))
                :to-match "MODIFIED")))

    (it "Directive expands and reference is part of the change region"
      (let* ((buffer-text "directive start
  reference content
directive end
")
             (directive-text "directive start
  reference content
directive end")
             (reference-text "reference content")
             (new-text "directive start
  INSERTED TEXT
  reference content
directive end
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay
                         buffer-text nil nil directive-text 'directive))
             (test-buffer (car buf-setup))
             (directive-ov (cdr buf-setup))
             (directive-orig-start (overlay-start directive-ov))
             (directive-orig-end (overlay-end directive-ov))
             ;; Create nested reference overlay
             (reference-ov (mevedel-tests--create-overlay
                            test-buffer nil nil reference-text 'reference))
             (reference-orig-start (overlay-start reference-ov))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Directive should expand to include new content
        (expect (overlay-start directive-ov) :to-equal directive-orig-start)
        (expect (overlay-end directive-ov) :to-be-greater-than directive-orig-end)
        ;; Reference should be adjusted (overlays process the diff region)
        ;; The exact behavior depends on whether the diff encompasses the reference
        (expect (overlay-buffer reference-ov) :to-equal test-buffer)
        (expect (<= (overlay-start reference-ov) (overlay-end reference-ov)) :to-be-truthy)
        ;; Reference should still contain "reference content"
        (expect (with-current-buffer test-buffer
                  (buffer-substring-no-properties (overlay-start reference-ov) (overlay-end reference-ov)))
                :to-match "reference content")))

    (it "Child deleted when parent shrinks to single character"
      ;; When parent becomes too small (1 char), child should be deleted
      (let* ((buffer-text "prefix
directive start
  reference content
directive end
suffix
")
             (directive-text "directive start
  reference content
directive end")
             (reference-text "reference content")
             (new-text "prefix
X
suffix
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay
                         buffer-text nil nil directive-text 'directive))
             (test-buffer (car buf-setup))
             (directive-ov (cdr buf-setup))
             ;; Create nested reference overlay
             (reference-ov (mevedel-tests--create-overlay
                            test-buffer nil nil reference-text 'reference))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Parent should still exist at safe position
        (expect (overlay-buffer directive-ov) :to-equal test-buffer)
        ;; Child should be deleted (buffer nil means deleted)
        (expect (overlay-buffer reference-ov) :to-equal nil)))

    (it "Child deleted when parent and child content purely removed"
      ;; Complete removal with no replacement text - both should be moved/deleted
      (let* ((buffer-text "prefix
directive start
  reference content
directive end
suffix
")
             (directive-text "directive start
  reference content
directive end")
             (reference-text "reference content")
             (new-text "prefix
suffix
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay
                         buffer-text nil nil directive-text 'directive))
             (test-buffer (car buf-setup))
             (directive-ov (cdr buf-setup))
             ;; Create nested reference overlay
             (reference-ov (mevedel-tests--create-overlay
                            test-buffer nil nil reference-text 'reference))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Parent should be moved to safe position
        (expect (overlay-buffer directive-ov) :to-equal test-buffer)
        ;; Child should be deleted (parent too small after pure removal)
        (expect (overlay-buffer reference-ov) :to-equal nil)))

    (it "Child content replaced while parent expands"
      ;; Remove child's content while expanding parent
      (let* ((buffer-text "directive start
  reference content
  more content
directive end
")
             (directive-text "directive start
  reference content
  more content
directive end")
             (reference-text "reference content")
             (new-text "directive start
  EXPANDED TEXT HERE
  much more content now
directive end
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay
                         buffer-text nil nil directive-text 'directive))
             (test-buffer (car buf-setup))
             (directive-ov (cdr buf-setup))
             (directive-orig-start (overlay-start directive-ov))
             (directive-orig-end (overlay-end directive-ov))
             ;; Create nested reference overlay
             (reference-ov (mevedel-tests--create-overlay
                            test-buffer nil nil reference-text 'reference))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Parent should expand
        (expect (overlay-start directive-ov) :to-equal directive-orig-start)
        (expect (overlay-end directive-ov) :to-be-greater-than directive-orig-end)
        ;; Child should still exist (replaced but within parent)
        (expect (overlay-buffer reference-ov) :to-equal test-buffer)
        ;; Child should be within parent bounds
        (expect (>= (overlay-start reference-ov) (overlay-start directive-ov)) :to-be-truthy)
        (expect (<= (overlay-end reference-ov) (overlay-end directive-ov)) :to-be-truthy)))

    (it "Multiple children adjusted independently"
      ;; Two references within directive, both should be adjusted
      (let* ((buffer-text "directive start
  first reference
  middle content
  second reference
directive end
")
             (directive-text "directive start
  first reference
  middle content
  second reference
directive end")
             (ref1-text "first reference")
             (ref2-text "second reference")
             (new-text "directive start
  first reference MODIFIED
  middle content
  second reference MODIFIED
directive end
")
             (buf-setup (mevedel-tests--create-buffer-with-overlay
                         buffer-text nil nil directive-text 'directive))
             (test-buffer (car buf-setup))
             (directive-ov (cdr buf-setup))
             (directive-orig-start (overlay-start directive-ov))
             (directive-orig-end (overlay-end directive-ov))
             ;; Create two nested reference overlays
             (ref1-ov (mevedel-tests--create-overlay
                       test-buffer nil nil ref1-text 'reference))
             (ref2-ov (mevedel-tests--create-overlay
                       test-buffer nil nil ref2-text 'reference))
             (diff-buffer (mevedel-tests--create-diff-buffer new-text test-buffer)))

        (spy-on 'macher-workspace :and-return-value `(file . ,(buffer-file-name test-buffer)))

        (with-current-buffer diff-buffer
          (let ((default-directory (temporary-file-directory))
                (inhibit-message t))
            (mevedel-diff-apply-buffer)))
        ;; Parent should expand
        (expect (overlay-start directive-ov) :to-equal directive-orig-start)
        (expect (overlay-end directive-ov) :to-be-greater-than directive-orig-end)
        ;; Both children should still exist
        (expect (overlay-buffer ref1-ov) :to-equal test-buffer)
        (expect (overlay-buffer ref2-ov) :to-equal test-buffer)
        ;; Both children should be within parent bounds
        (expect (>= (overlay-start ref1-ov) (overlay-start directive-ov)) :to-be-truthy)
        (expect (<= (overlay-end ref1-ov) (overlay-end directive-ov)) :to-be-truthy)
        (expect (>= (overlay-start ref2-ov) (overlay-start directive-ov)) :to-be-truthy)
        (expect (<= (overlay-end ref2-ov) (overlay-end directive-ov)) :to-be-truthy)
        ;; Both should contain "MODIFIED"
        (expect (with-current-buffer test-buffer
                  (buffer-substring-no-properties (overlay-start ref1-ov) (overlay-end ref1-ov)))
                :to-match "MODIFIED")
        (expect (with-current-buffer test-buffer
                  (buffer-substring-no-properties (overlay-start ref2-ov) (overlay-end ref2-ov)))
                :to-match "MODIFIED")))))


(describe "mevedel--tag-query-prefix-from-infix:"
  (describe "Valid infix to prefix conversions"
    (it "converts 'foo and not bar or baz'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo and not bar or baz))
              :to-equal '(or (and foo (not bar)) baz)))

    (it "converts 'john or not [jane]'"
      (expect (mevedel--tag-query-prefix-from-infix '(john or not [jane]))
              :to-equal '(or john (not [jane]))))

    (it "converts 'alice and bob and charlie'"
      (expect (mevedel--tag-query-prefix-from-infix '(alice and bob and charlie))
              :to-equal '(and alice bob charlie)))

    (it "converts single tag 'foo'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo))
              :to-equal 'foo))

    (it "converts 'foo bar baz not john'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo bar baz not john))
              :to-equal '(and foo bar baz (not john))))

    (it "converts '((foo))'"
      (expect (mevedel--tag-query-prefix-from-infix '((foo)))
              :to-equal 'foo))

    (it "converts '(((foo)))'"
      (expect (mevedel--tag-query-prefix-from-infix '(((foo))))
              :to-equal 'foo))

    (it "converts '(((foo foo foo)))'"
      (expect (mevedel--tag-query-prefix-from-infix '(((foo foo foo))))
              :to-equal '(and foo foo foo)))

    (it "converts 'not bar and baz'"
      (expect (mevedel--tag-query-prefix-from-infix '(not bar and baz))
              :to-equal '(and (not bar) baz)))

    (it "converts 'bar or bar or baz'"
      (expect (mevedel--tag-query-prefix-from-infix '(bar or bar or baz))
              :to-equal '(or bar bar baz)))

    (it "converts 'bar bar or baz'"
      (expect (mevedel--tag-query-prefix-from-infix '(bar bar or baz))
              :to-equal '(or (and bar bar) baz)))

    (it "converts empty list to nil"
      (expect (mevedel--tag-query-prefix-from-infix '())
              :to-equal nil))

    (it "converts '((()))' to nil"
      (expect (mevedel--tag-query-prefix-from-infix '((())))
              :to-equal nil))

    (it "converts 'danny and (joey and boris)'"
      (expect (mevedel--tag-query-prefix-from-infix '(danny and (joey and boris)))
              :to-equal '(and danny (and joey boris))))

    (it "converts '((danny and (joey and boris)) and (foo or bar))'"
      (expect (mevedel--tag-query-prefix-from-infix '((danny and (joey and boris)) and (foo or bar)))
              :to-equal '(and (and danny (and joey boris)) (or foo bar))))

    (it "converts '((alice or bob) and (charlie or dave))'"
      (expect (mevedel--tag-query-prefix-from-infix '((alice or bob) and (charlie or dave)))
              :to-equal '(and (or alice bob) (or charlie dave))))

    (it "converts '((alice and bob) or (charlie and dave))'"
      (expect (mevedel--tag-query-prefix-from-infix '((alice and bob) or (charlie and dave)))
              :to-equal '(or (and alice bob) (and charlie dave)))))

  (describe "Invalid infix queries"
    (it "rejects '(and)'"
      (expect (mevedel--tag-query-prefix-from-infix '(and))
              :to-throw))

    (it "rejects '(or)'"
      (expect (mevedel--tag-query-prefix-from-infix '(or))
              :to-throw))

    (it "rejects '(not)'"
      (expect (mevedel--tag-query-prefix-from-infix '(not))
              :to-throw))

    (it "rejects '(and foo)'"
      (expect (mevedel--tag-query-prefix-from-infix '(and foo))
              :to-throw))

    (it "rejects '(or foo)'"
      (expect (mevedel--tag-query-prefix-from-infix '(or foo))
              :to-throw))

    (it "rejects '(and foo or bar)'"
      (expect (mevedel--tag-query-prefix-from-infix '(and foo or bar))
              :to-throw))

    (it "rejects '(or and foo bar)'"
      (expect (mevedel--tag-query-prefix-from-infix '(or and foo bar))
              :to-throw))

    (it "rejects '(and (or foo) bar)'"
      (expect (mevedel--tag-query-prefix-from-infix '(and (or foo) bar))
              :to-throw))

    (it "rejects '(foo (or bar))'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo (or bar)))
              :to-throw))

    (it "rejects '(foo or (and bar))'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo or (and bar)))
              :to-throw))

    (it "rejects '(foo bar and (not))'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo bar and (not)))
              :to-throw))

    (it "rejects '((or bar))'"
      (expect (mevedel--tag-query-prefix-from-infix '((or bar)))
              :to-throw))

    (it "rejects '((and foo))'"
      (expect (mevedel--tag-query-prefix-from-infix '((and foo)))
              :to-throw))

    (it "rejects '(foo or (and))'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo or (and)))
              :to-throw))

    (it "rejects '(or ())'"
      (expect (mevedel--tag-query-prefix-from-infix '(or ()))
              :to-throw))

    (it "rejects '(foo or not)'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo or not))
              :to-throw))

    (it "rejects '(and (and foo bar))'"
      (expect (mevedel--tag-query-prefix-from-infix '(and (and foo bar)))
              :to-throw))

    (it "rejects '(or (or(foo and bar)))'"
      (expect (mevedel--tag-query-prefix-from-infix '(or (or(foo and bar))))
              :to-throw))))

(provide 'test-mevedel-utilities)
;;; test-mevedel-utilities.el ends here
