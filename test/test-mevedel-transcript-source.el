;;; test-mevedel-transcript-source.el --- Transcript change frontier tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Adversarial coverage for the proven transcript change frontier: text and
;; property edits, independent consumers, unpaired and nested hooks, missed
;; hooks, known metadata rebasing, and randomized frontier soundness.

;;; Code:

(require 'mevedel-transcript-source)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defconst test-mevedel-transcript-source--metadata "META\n"
  "Leading metadata every fixture buffer starts with.")

(defconst test-mevedel-transcript-source--body "abcdefghij\nsecond line\n"
  "Body text every fixture buffer holds after its metadata.")

(defvar test-mevedel-transcript-source--fire nil
  "Non-nil makes the next global test hook edit the body prefix silently.")

(defun test-mevedel-transcript-source--silent-global-edit ()
  "Insert at the body start, once, when `test-mevedel-transcript-source--fire'."
  (when test-mevedel-transcript-source--fire
    (setq test-mevedel-transcript-source--fire nil)
    (save-excursion
      (goto-char 6)
      (insert "G"))))

(defun test-mevedel-transcript-source--hook-overlay (action &optional self-removing)
  "Make an overlay over the body whose hooks run ACTION, once.
With SELF-REMOVING the overlay deletes itself afterwards.  Return the overlay."
  (let* ((overlay (make-overlay 6 (point-max) nil nil t))
         (fired nil)
         (hook (lambda (_overlay after _start _end &optional _length)
                 (unless (or after fired)
                   (setq fired t)
                   (funcall action)
                   (when self-removing (delete-overlay overlay))))))
    (overlay-put overlay 'modification-hooks (list hook))
    (overlay-put overlay 'insert-in-front-hooks (list hook))
    (overlay-put overlay 'insert-behind-hooks (list hook))
    overlay))

(defmacro test-mevedel-transcript-source--with-clone (clone &rest body)
  "Run BODY with CLONE bound to a clone of the current buffer, then kill it.
The clone inherits the original's local hooks and consumer list."
  (declare (indent 1))
  `(let ((,clone (clone-buffer " *transcript-source-clone*" nil)))
     (unwind-protect
         (progn ,@body)
       (when (buffer-live-p ,clone) (kill-buffer ,clone)))))

(defun test-mevedel-transcript-source--setup ()
  "Fill the current buffer with metadata and body; return an accepted source.
The source's origin is the first body character."
  (insert test-mevedel-transcript-source--metadata
          test-mevedel-transcript-source--body)
  (let ((source (mevedel-transcript-source-create
                 (1+ (length test-mevedel-transcript-source--metadata)))))
    (mevedel-transcript-source-accept source)
    source))

(defun test-mevedel-transcript-source--content-start ()
  "Return the first body position, computed from the text alone."
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n")
    (point)))

(defun test-mevedel-transcript-source--body-string ()
  "Return the text after the content start, without properties."
  (buffer-substring-no-properties
   (test-mevedel-transcript-source--content-start) (point-max)))

(defun test-mevedel-transcript-source--origin (source)
  "Return SOURCE's origin position."
  (marker-position (mevedel-transcript-source-origin source)))

(mevedel-deftest mevedel-transcript-source-create
  ()
  ,test
  (test)
  :doc "records origin, mode and tick, and demands a full first derivation"
  (with-temp-buffer
    (insert test-mevedel-transcript-source--metadata
            test-mevedel-transcript-source--body)
    (let ((source (mevedel-transcript-source-create 6)))
      (should (mevedel-transcript-source-p source))
      (should (eq (current-buffer)
                  (marker-buffer (mevedel-transcript-source-origin source))))
      (should (= 6 (test-mevedel-transcript-source--origin source)))
      (should (eq major-mode (mevedel-transcript-source-mode source)))
      (should (eql (buffer-modified-tick) (mevedel-transcript-source-tick source)))
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "copies its origin so the caller's marker does not move the proof"
  (with-temp-buffer
    (insert test-mevedel-transcript-source--metadata
            test-mevedel-transcript-source--body)
    (let* ((marker (copy-marker 6))
           (source (mevedel-transcript-source-create marker)))
      (set-marker marker 10)
      (should-not (eq marker (mevedel-transcript-source-origin source)))
      (should (= 6 (test-mevedel-transcript-source--origin source)))))

  :doc "installs buffer-local hooks and registers the consumer once per buffer"
  (with-temp-buffer
    (insert test-mevedel-transcript-source--body)
    (let ((first (mevedel-transcript-source-create 1))
          (second (mevedel-transcript-source-create 1)))
      (should (memq #'mevedel-transcript-source--before-change
                    before-change-functions))
      (should (memq #'mevedel-transcript-source--after-change
                    after-change-functions))
      (should-not (memq #'mevedel-transcript-source--before-change
                        (default-value 'before-change-functions)))
      (should (= 1 (cl-count #'mevedel-transcript-source--before-change
                             before-change-functions)))
      (should (eq #'mevedel-transcript-source--before-change
                  (car before-change-functions)))
      (should (eq #'mevedel-transcript-source--before-seal
                  (car (last (remq t before-change-functions)))))
      (should (eq #'mevedel-transcript-source--after-change
                  (car after-change-functions)))
      (should (equal (list second first) mevedel-transcript-source--consumers))))

  :doc "consumers of different buffers never share proofs"
  (let (one two first second)
    (unwind-protect
        (progn
          (setq one (generate-new-buffer " *transcript-source-one*")
                two (generate-new-buffer " *transcript-source-two*"))
          (with-current-buffer one
            (insert "aaaa")
            (setq first (mevedel-transcript-source-create 1))
            (mevedel-transcript-source-accept first))
          (with-current-buffer two
            (insert "bbbb")
            (setq second (mevedel-transcript-source-create 1))
            (mevedel-transcript-source-accept second)
            (goto-char 3)
            (insert "X")
            (should (eql 2 (mevedel-transcript-source-change second))))
          (with-current-buffer one
            (should-not (mevedel-transcript-source-change first))))
      (mapc (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list one two)))))

(mevedel-deftest mevedel-transcript-source--shape ()
  ,test
  (test)
  :doc "follows multibyteness and size"
  (with-temp-buffer
    (insert "abc")
    (should (equal '(t . 3) (mevedel-transcript-source--shape)))
    (set-buffer-multibyte nil)
    (should (equal '(nil . 3) (mevedel-transcript-source--shape)))))

(mevedel-deftest mevedel-transcript-source-change
  ()
  ,test
  (test)
  :doc "invalidates the proof when multibyteness changes the text silently"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (goto-char (point-max))
      (insert "é")
      (mevedel-transcript-source-accept source)
      (let ((tick (buffer-modified-tick)))
        (set-buffer-multibyte nil)
        ;; The conversion neither moves the tick nor runs a change hook.
        (should (eql tick (buffer-modified-tick))))
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "a later observed edit cannot hide an earlier multibyte conversion"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (goto-char (point-max))
      (insert "é")
      (mevedel-transcript-source-accept source)
      (set-buffer-multibyte nil)
      (goto-char (point-max))
      (insert "tail")
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "reports no change after an accepted baseline"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (should-not (mevedel-transcript-source-change source))
      (should-not (mevedel-transcript-source-change source))))

  :doc "reports the body-relative offset of appends, inserts, deletes and replaces"
  (with-temp-buffer
    (let* ((source (test-mevedel-transcript-source--setup))
           (length (length test-mevedel-transcript-source--body)))
      (goto-char (point-max))
      (insert "tail")
      (should (eql length (mevedel-transcript-source-change source)))
      (mevedel-transcript-source-accept source)
      (goto-char 9)
      (insert "X")
      (should (eql 3 (mevedel-transcript-source-change source)))
      (mevedel-transcript-source-accept source)
      (delete-region 8 10)
      (should (eql 2 (mevedel-transcript-source-change source)))
      (mevedel-transcript-source-accept source)
      (goto-char 12)
      (delete-char 2)
      (insert "ZZZ")
      (should (eql 6 (mevedel-transcript-source-change source)))))

  :doc "treats property-only edits as dirty at their offset"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (put-text-property 10 12 'face 'bold)
      (should (eql 4 (mevedel-transcript-source-change source)))
      (mevedel-transcript-source-accept source)
      (add-text-properties 7 8 '(mevedel-test t))
      (should (eql 1 (mevedel-transcript-source-change source)))
      (mevedel-transcript-source-accept source)
      (remove-text-properties 7 8 '(mevedel-test nil))
      (should (eql 1 (mevedel-transcript-source-change source)))))

  :doc "keeps the earliest frontier across several edits and consumers"
  (with-temp-buffer
    (let* ((first (test-mevedel-transcript-source--setup))
           (second (mevedel-transcript-source-create 6)))
      (mevedel-transcript-source-accept second)
      (goto-char (point-max))
      (insert "late")
      (goto-char 9)
      (insert "mid")
      (goto-char (point-max))
      (insert "later")
      (should (eql 3 (mevedel-transcript-source-change first)))
      (should (eql 3 (mevedel-transcript-source-change second)))))

  :doc "invalidates the whole proof for edits that skipped the hooks"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (let ((inhibit-modification-hooks t))
        (goto-char (point-max))
        (insert "unobserved"))
      (should (eql 0 (mevedel-transcript-source-change source)))
      (mevedel-transcript-source-accept source)
      (should-not (mevedel-transcript-source-change source))
      (with-silent-modifications
        (put-text-property 7 9 'face 'italic))
      (should (eql 0 (mevedel-transcript-source-change source)))
      (mevedel-transcript-source-accept source)
      (with-silent-modifications
        (goto-char (point-max))
        (insert "silent text"))
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "does not let a later observed edit rehabilitate a missed one"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (with-silent-modifications
        (goto-char 7)
        (insert "early silent"))
      (goto-char (point-max))
      (insert "late observed")
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "invalidates on a changed major mode, removed hooks and a foreign buffer"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (setq major-mode 'mevedel-transcript-source-test-mode)
      (should (eql 0 (mevedel-transcript-source-change source)))))
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (remove-hook 'before-change-functions
                   #'mevedel-transcript-source--before-change t)
      (should (eql 0 (mevedel-transcript-source-change source)))))
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (remove-hook 'after-change-functions
                   #'mevedel-transcript-source--after-change t)
      (should (eql 0 (mevedel-transcript-source-change source)))))
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (with-temp-buffer
        (should (eql 0 (mevedel-transcript-source-change source))))
      ;; The foreign query already invalidated it for good.
      (should (eql 0 (mevedel-transcript-source-change source)))))
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (kill-all-local-variables)
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "fails closed when another hook is ordered outside the proof's bracket"
  (dolist (spec '((before-change-functions -200 2)
                  (before-change-functions 200 2)
                  (after-change-functions -200 3)))
    (with-temp-buffer
      (let ((source (test-mevedel-transcript-source--setup))
            (hook (make-symbol "outer-hook")))
        (fset hook (if (= 2 (nth 2 spec))
                       (lambda (_start _end))
                     (lambda (_start _end _length))))
        (add-hook (nth 0 spec) hook (nth 1 spec) t)
        (should (eql 0 (mevedel-transcript-source-change source))))))
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (remove-hook 'before-change-functions
                   #'mevedel-transcript-source--before-seal t)
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "fails closed when the global-hook marker leaves the proof's bracket"
  (dolist (spec `((before-change-functions
                   (t mevedel-transcript-source--before-change
                      mevedel-transcript-source--before-seal))
                  (before-change-functions
                   (mevedel-transcript-source--before-change
                    mevedel-transcript-source--before-seal t))
                  (after-change-functions
                   (t mevedel-transcript-source--after-change))))
    (with-temp-buffer
      (let ((source (test-mevedel-transcript-source--setup)))
        (set (make-local-variable (car spec)) (copy-sequence (cadr spec)))
        (should (eql 0 (mevedel-transcript-source-change source))))))

  :doc "invalidates when a real global hook edits silently inside the bracket"
  (dolist (variable '(before-change-functions after-change-functions))
    (let ((hook (if (eq variable 'before-change-functions)
                    (lambda (_start _end)
                      (test-mevedel-transcript-source--silent-global-edit))
                  (lambda (_start _end _length)
                    (test-mevedel-transcript-source--silent-global-edit)))))
      (unwind-protect
          (progn
            (add-hook variable hook)
            (with-temp-buffer
              (let ((source (test-mevedel-transcript-source--setup)))
                (setq test-mevedel-transcript-source--fire t)
                (goto-char (point-max))
                (insert "tail")
                (should-not test-mevedel-transcript-source--fire)
                (should (eql 0 (mevedel-transcript-source-change source))))))
        (remove-hook variable hook)
        (setq test-mevedel-transcript-source--fire nil))
      (should-not (memq hook (default-value variable)))))

  :doc "text-property hooks that edit silently invalidate the proof, harmless ones do not"
  (dolist (spec '((modification-hooks nil t)
                  (modification-hooks t t)
                  (insert-behind-hooks nil t)
                  (insert-behind-hooks t t)
                  (insert-in-front-hooks nil t)
                  (insert-in-front-hooks t t)))
    (with-temp-buffer
      (let* ((source (test-mevedel-transcript-source--setup))
             (property (nth 0 spec))
             (self-removing (nth 1 spec))
             (position (1- (point-max)))
             (hook (lambda (&rest _)
                     (let ((inhibit-modification-hooks t))
                       (save-excursion (goto-char 6) (insert "X"))
                       (when self-removing
                         (remove-text-properties
                          1 (point-max) (list property nil)))))))
        (put-text-property position (1+ position) property (list hook))
        (mevedel-transcript-source-accept source)
        (pcase property
          ('modification-hooks (delete-region position (1+ position)))
          ('insert-behind-hooks (goto-char (point-max)) (insert "tail"))
          ('insert-in-front-hooks (goto-char position) (insert "Z")))
        (should (eql 0 (mevedel-transcript-source-change source))))))
  (dolist (property '(modification-hooks insert-behind-hooks insert-in-front-hooks))
    (with-temp-buffer
      (let* ((source (test-mevedel-transcript-source--setup))
             (position (1- (point-max))))
        (put-text-property position (1+ position) property (list #'ignore))
        (mevedel-transcript-source-accept source)
        (pcase property
          ('modification-hooks (delete-region position (1+ position)))
          ('insert-behind-hooks (goto-char (point-max)) (insert "tail"))
          ('insert-in-front-hooks (goto-char position) (insert "Z")))
        (should (eql (pcase property
                       ('insert-behind-hooks (- (point-max) 4 6))
                       (_ (- position 6)))
                     (mevedel-transcript-source-change source))))))

  :doc "reports an edit before the origin as a full invalidation"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (goto-char 2)
      (insert "unknown")
      (should (eql 0 (mevedel-transcript-source-change source)))
      (should (= 13 (test-mevedel-transcript-source--origin source)))))

  :doc "reports an unpaired before-change at its offset until a second one nests"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source--before-change 9 9)
      (should (eql 3 (mevedel-transcript-source-change source)))
      (mevedel-transcript-source--before-change 12 12)
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "never reports a frontier later than the first differing body offset"
  (let ((trials 400)
        (state 20241020))
    (cl-flet ((random (limit)
                (setq state (% (+ (* state 1103515245) 12345) 2147483648))
                (% (/ state 65536) limit)))
    (dotimes (trial trials)
      (with-temp-buffer
        (insert test-mevedel-transcript-source--metadata)
        (dotimes (_ (+ 20 (random 40)))
          (insert (+ ?a (random 26))))
        (insert "\n")
        (let* ((source (mevedel-transcript-source-create
                        (test-mevedel-transcript-source--content-start)))
               (_ (mevedel-transcript-source-accept source))
               (before (test-mevedel-transcript-source--body-string))
               property-edit)
          (dotimes (_ (1+ (random 4)))
            (let* ((start (test-mevedel-transcript-source--origin source))
                   (size (- (point-max) start)))
              (pcase (random 6)
                (0 (goto-char (+ start (random (1+ size))))
                   (insert (make-string (1+ (random 3)) (+ ?A (random 26)))))
                (1 (when (> size 0)
                     (let* ((from (+ start (random size)))
                            (to (min (point-max) (+ from 1 (random 4)))))
                       (delete-region from to))))
                (2 (when (> size 0)
                     (let* ((from (+ start (random size)))
                            (to (min (point-max) (+ from 1 (random 3)))))
                       (goto-char from)
                       (delete-region from to)
                       (insert (make-string (random 4) ?Q)))))
                (3 (when (> size 1)
                     (let ((from (+ start (random (1- size)))))
                       (put-text-property from (1+ from) 'face 'bold)
                       (setq property-edit t))))
                (4 (mevedel-transcript-source-call-with-metadata-change
                    (lambda ()
                      (goto-char (+ 1 (random 4)))
                      (insert "m"))))
                (_ (goto-char (point-max))
                   (insert "end")))))
          (let* ((after (test-mevedel-transcript-source--body-string))
                 (frontier (mevedel-transcript-source-change source))
                 (difference (compare-strings before nil nil after nil nil)))
            ;; The origin must also agree with the text, checked on its own.
            (should (= (test-mevedel-transcript-source--content-start)
                       (test-mevedel-transcript-source--origin source)))
            (unless (eq t difference)
              (should (integerp frontier))
              (should (<= frontier (1- (abs difference)))))
            (when property-edit
              (should (integerp frontier)))
            (ignore trial))))))))

(mevedel-deftest mevedel-transcript-source-accept
  ()
  ,test
  (test)
  :doc "adopts the current contents, tick and mode as the new baseline"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (goto-char (point-max))
      (insert "more")
      (should (mevedel-transcript-source-change source))
      (setq major-mode 'mevedel-transcript-source-test-mode)
      (mevedel-transcript-source-accept source)
      (should (eq 'mevedel-transcript-source-test-mode
                  (mevedel-transcript-source-mode source)))
      (should (eql (buffer-modified-tick) (mevedel-transcript-source-tick source)))
      (should-not (mevedel-transcript-source-before source))
      (should-not (mevedel-transcript-source-change source))))

  :doc "re-baselines after an unobserved edit and clears an unpaired before"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (let ((inhibit-modification-hooks t))
        (insert "skipped"))
      (mevedel-transcript-source--before-change 8 8)
      (mevedel-transcript-source-accept source)
      (should-not (mevedel-transcript-source-change source))
      (goto-char (point-max))
      (insert "x")
      (should (integerp (mevedel-transcript-source-change source)))))

  :doc "keeps every consumer's baseline independent"
  (with-temp-buffer
    (let* ((first (test-mevedel-transcript-source--setup))
           (second (mevedel-transcript-source-create 6)))
      (mevedel-transcript-source-accept second)
      (goto-char 9)
      (insert "X")
      (mevedel-transcript-source-accept first)
      (should-not (mevedel-transcript-source-change first))
      (should (eql 3 (mevedel-transcript-source-change second)))
      (goto-char (point-max))
      (insert "tail")
      (should (eql (length test-mevedel-transcript-source--body)
                   (1- (mevedel-transcript-source-change first))))
      (should (eql 3 (mevedel-transcript-source-change second)))))

  :doc "refuses to accept a proof owned by another buffer"
  (let (source other)
    (unwind-protect
        (with-temp-buffer
          (setq source (test-mevedel-transcript-source--setup))
          (setq other (generate-new-buffer " *transcript-source-other*"))
          (with-current-buffer other
            (should-error (mevedel-transcript-source-accept source)))
          (should-not (mevedel-transcript-source-change source)))
      (when (buffer-live-p other) (kill-buffer other)))))

(mevedel-deftest mevedel-transcript-source-release
  ()
  ,test
  (test)
  :doc "unregisters the consumer and detaches its origin"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source-release source)
      (should-not (memq source mevedel-transcript-source--consumers))
      (should-not (marker-buffer (mevedel-transcript-source-origin source)))
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "leaves other consumers tracking and ignores edits for the released one"
  (with-temp-buffer
    (let* ((first (test-mevedel-transcript-source--setup))
           (second (mevedel-transcript-source-create 6)))
      (mevedel-transcript-source-accept second)
      (mevedel-transcript-source-release first)
      (should (equal (list second) mevedel-transcript-source--consumers))
      (goto-char 9)
      (insert "X")
      (should (eql 3 (mevedel-transcript-source-change second)))
      (should (eql 0 (mevedel-transcript-source-change first)))))

  :doc "is idempotent and tolerates a killed source buffer"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source-release source)
      (mevedel-transcript-source-release source)))
  (let ((buffer (generate-new-buffer " *transcript-source-killed*"))
        source)
    (with-current-buffer buffer
      (insert "abc")
      (setq source (mevedel-transcript-source-create 1)))
    (kill-buffer buffer)
    (mevedel-transcript-source-release source)
    (should-not (marker-buffer (mevedel-transcript-source-origin source)))))

(mevedel-deftest mevedel-transcript-source--before-change
  ()
  ,test
  (test)
  :doc "records the earliest offset and marks the proof as awaiting its pair"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source--before-change 12 12)
      (should (eql 6 (mevedel-transcript-source-dirty source)))
      (should (mevedel-transcript-source-before source))))

  :doc "invalidates a nested or tick-mismatched change"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source--before-change 12 12)
      (mevedel-transcript-source--before-change 14 14)
      (should (eql 0 (mevedel-transcript-source-dirty source)))))
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (let ((inhibit-modification-hooks t))
        (insert "silent"))
      (mevedel-transcript-source--before-change (point-max) (point-max))
      (should (eql 0 (mevedel-transcript-source-dirty source)))))

  :doc "treats a start before the origin as offset zero unless exempt"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source--before-change 2 3)
      (should (eql 0 (mevedel-transcript-source-dirty source)))))

  :doc "a later before-change hook's silent edit invalidates the proof"
  (with-temp-buffer
    (let* ((source (test-mevedel-transcript-source--setup))
           (hook (make-symbol "late-self-removing")))
      (fset hook (lambda (_start _end)
                   (save-excursion (goto-char 6) (insert "L"))
                   (remove-hook 'before-change-functions hook t)))
      (add-hook 'before-change-functions hook 200 t)
      (goto-char (point-max))
      (insert "tail")
      (should-not (memq hook before-change-functions))
      (should (mevedel-transcript-source--hooks-ordered-p))
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "a hook that joins the after list during the before phase and removes itself invalidates the proof"
  (with-temp-buffer
    (let* ((source (test-mevedel-transcript-source--setup))
           (after (make-symbol "early-after"))
           (joiner (make-symbol "joiner")))
      (fset after (lambda (_start _end _length)
                    (save-excursion (goto-char 6) (insert "E"))
                    (remove-hook 'after-change-functions after t)))
      (fset joiner (lambda (_start _end)
                     (add-hook 'after-change-functions after -200 t)
                     (remove-hook 'before-change-functions joiner t)))
      (add-hook 'before-change-functions joiner 0 t)
      (goto-char (point-max))
      (insert "tail")
      (should (mevedel-transcript-source--hooks-ordered-p))
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "a later before-change hook's silent edit invalidates the proof"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup))
          (done nil))
      (add-hook 'before-change-functions
                (lambda (_start _end)
                  (unless done
                    (setq done t)
                    (save-excursion (goto-char 6) (insert "X"))))
                90 t)
      (goto-char (point-max))
      (insert "tail")
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "an overlay hook's silent text edit invalidates the proof even when it removes itself"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (test-mevedel-transcript-source--hook-overlay
       (lambda () (save-excursion (goto-char 6) (insert "O")))
       t)
      (goto-char (point-max))
      (insert "tail")
      (should-not (test-mevedel-transcript-source--overlay-present-p))
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "an overlay hook's silent property-only edit invalidates the proof"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (test-mevedel-transcript-source--hook-overlay
       (lambda () (put-text-property 7 9 'face 'bold))
       t)
      (goto-char (point-max))
      (insert "tail")
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "an overlay with hooks invalidates every change while it exists"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (test-mevedel-transcript-source--hook-overlay #'ignore)
      (goto-char (point-max))
      (insert "tail")
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "an overlay without hooks, or whose hooks are gone, keeps the tail frontier"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup))
          (overlay (make-overlay 6 (point-max))))
      (overlay-put overlay 'face 'bold)
      (overlay-put overlay 'modification-hooks nil)
      (goto-char (point-max))
      (insert "tail")
      (should (eql (length test-mevedel-transcript-source--body)
                   (mevedel-transcript-source-change source)))))
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (delete-overlay (test-mevedel-transcript-source--hook-overlay #'ignore))
      (goto-char (point-max))
      (insert "tail")
      (should (eql (length test-mevedel-transcript-source--body)
                   (mevedel-transcript-source-change source)))))

  :doc "a cloned buffer's edit leaves the original's proof untouched and tracks its own"
  (with-temp-buffer
    (let* ((source (test-mevedel-transcript-source--setup))
           (tick (mevedel-transcript-source-tick source)))
      (test-mevedel-transcript-source--with-clone clone
        (with-current-buffer clone
          (let ((mine (mevedel-transcript-source-create 6)))
            (mevedel-transcript-source-accept mine)
            (goto-char (point-max))
            (insert "clone tail")
            (should (eql (length test-mevedel-transcript-source--body)
                         (mevedel-transcript-source-change mine)))))
        (should-not (mevedel-transcript-source-dirty source))
        (should-not (mevedel-transcript-source-before source))
        (should (eql tick (mevedel-transcript-source-tick source)))
        (should-not (mevedel-transcript-source-before-tick source))
        (should-not (mevedel-transcript-source-change source)))))

  :doc "skips released consumers and handles several consumers separately"
  (with-temp-buffer
    (let* ((first (test-mevedel-transcript-source--setup))
           (second (mevedel-transcript-source-create 10))
           (released (mevedel-transcript-source-create 6)))
      (mevedel-transcript-source-accept second)
      (mevedel-transcript-source-accept released)
      (mevedel-transcript-source-release released)
      (mevedel-transcript-source--before-change 12 12)
      (should (eql 6 (mevedel-transcript-source-dirty first)))
      (should (eql 2 (mevedel-transcript-source-dirty second)))
      (should-not (mevedel-transcript-source-before released)))))

(mevedel-deftest mevedel-transcript-source-begin-change ()
  ,test
  (test)
  :doc "finishes once before observers without blessing their later silent edits"
  (with-temp-buffer
    (let* ((source (test-mevedel-transcript-source--setup))
           (start (point-max))
           (finish (mevedel-transcript-source-begin-change start start)))
      (let ((inhibit-modification-hooks t))
        (goto-char start)
        (insert "tail"))
      (funcall finish)
      (should (= (- start 6) (mevedel-transcript-source-change source)))
      (mevedel-transcript-source-accept source)
      (let ((inhibit-modification-hooks t))
        (goto-char 6)
        (insert "unobserved"))
      (funcall finish)
      (should (= 0 (mevedel-transcript-source-change source)))))
  :doc "finishes in its owning buffer and tolerates that buffer's death"
  (let ((buffer (generate-new-buffer " *transcript-change-owner*")) finish source)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq source (test-mevedel-transcript-source--setup)
                  finish (mevedel-transcript-source-begin-change 10 10))
            (let ((inhibit-modification-hooks t)) (goto-char 10) (insert "x")))
          (with-temp-buffer (funcall finish))
          (with-current-buffer buffer
            (should (= 4 (mevedel-transcript-source-change source)))
            (setq finish (mevedel-transcript-source-begin-change 10 10)))
          (kill-buffer buffer)
          (funcall finish))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(mevedel-deftest mevedel-transcript-source-call-with-unproven-changes
  ()
  ,test
  (test)
  :doc "returns the value and leaves proofs alone when the buffer did not change"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (should (eq 'value
                  (mevedel-transcript-source-call-with-unproven-changes
                   (lambda () 'value))))
      (should-not (mevedel-transcript-source-dirty source))
      (should-not (mevedel-transcript-source-change source))))

  :doc "fails closed when foreign code redirects point or the current buffer"
  (dolist (redirect '(point buffer))
    (with-temp-buffer
      (let ((source (test-mevedel-transcript-source--setup))
            (owner (current-buffer)))
        (unwind-protect
            (with-temp-buffer
              (let ((other (current-buffer)))
                (with-current-buffer owner
                  (mevedel-transcript-source-call-with-unproven-changes
                   (lambda ()
                     (if (eq redirect 'point)
                         (goto-char (point-min))
                       (set-buffer other)))))))
          (with-current-buffer owner
            (should (eql 0 (mevedel-transcript-source-dirty source))))))))

  :doc "fails closed when foreign code converts multibyteness without moving point"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (goto-char (point-max))
      (insert "é")
      (mevedel-transcript-source-accept source)
      (goto-char (point-min))
      (mevedel-transcript-source-call-with-unproven-changes
       (lambda () (set-buffer-multibyte nil)))
      (should (eql 0 (mevedel-transcript-source-dirty source)))))

  :doc "fails every own proof closed after a silent text, property or observed edit"
  (dolist (edit (list (lambda ()
                        (let ((inhibit-modification-hooks t))
                          (goto-char (point-max))
                          (insert "x")))
                      (lambda ()
                        (with-silent-modifications
                          (put-text-property 8 9 'face 'bold)))
                      (lambda ()
                        (goto-char (point-max))
                        (insert "observed"))))
    (with-temp-buffer
      (let ((first (test-mevedel-transcript-source--setup))
            (second (mevedel-transcript-source-create 6)))
        (mevedel-transcript-source-accept second)
        (mevedel-transcript-source-call-with-unproven-changes edit)
        (should (eql 0 (mevedel-transcript-source-dirty first)))
        (should (eql 0 (mevedel-transcript-source-dirty second))))))

  :doc "lowers an existing frontier to zero and never raises one"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (goto-char (point-max))
      (insert "tail")
      (should (eql (length test-mevedel-transcript-source--body)
                   (mevedel-transcript-source-dirty source)))
      (mevedel-transcript-source-call-with-unproven-changes #'ignore)
      (should (eql (length test-mevedel-transcript-source--body)
                   (mevedel-transcript-source-dirty source)))
      (mevedel-transcript-source-call-with-unproven-changes
       (lambda () (let ((inhibit-modification-hooks t)) (insert "x"))))
      (should (eql 0 (mevedel-transcript-source-dirty source)))))

  :doc "keeps the zero frontier sticky under an enclosing change that adopts the tick"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source-call-with-change
       25 (point-max)
       (lambda ()
         (insert "tail")
         (mevedel-transcript-source-call-with-unproven-changes
          (lambda ()
            (save-excursion
              (goto-char 6)
              (insert "S"))))))
      (should (eql (buffer-modified-tick) (mevedel-transcript-source-tick source)))
      (should-not (mevedel-transcript-source-before source))
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "marks proofs closed even when the function fails"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (should-error
       (mevedel-transcript-source-call-with-unproven-changes
        (lambda ()
          (let ((inhibit-modification-hooks t)) (insert "x"))
          (error "Foreign failure"))))
      (should (eql 0 (mevedel-transcript-source-dirty source)))))

  :doc "ignores edits to other buffers and tolerates a killed buffer"
  (let ((other (generate-new-buffer " *transcript-source-foreign*")))
    (unwind-protect
        (with-temp-buffer
          (let ((source (test-mevedel-transcript-source--setup)))
            (mevedel-transcript-source-call-with-unproven-changes
             (lambda () (with-current-buffer other (insert "elsewhere"))))
            (should-not (mevedel-transcript-source-dirty source))))
      (kill-buffer other)))
  (let ((buffer (generate-new-buffer " *transcript-source-killed-in-call*")))
    (with-current-buffer buffer
      (test-mevedel-transcript-source--setup)
      (mevedel-transcript-source-call-with-unproven-changes
       (lambda () (kill-buffer buffer))))
    (should-not (buffer-live-p buffer)))

  :doc "does not touch the inherited proofs of a cloned buffer"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (test-mevedel-transcript-source--with-clone clone
        (with-current-buffer clone
          (mevedel-transcript-source-call-with-unproven-changes
           (lambda () (let ((inhibit-modification-hooks t)) (insert "clone")))))
        (should-not (mevedel-transcript-source-dirty source))))))

(mevedel-deftest mevedel-transcript-source--own-consumers
  ()
  ,test
  (test)
  :doc "returns only proofs whose origin lives in the current buffer"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (should (equal (list source) (mevedel-transcript-source--own-consumers)))
      (test-mevedel-transcript-source--with-clone clone
        (with-current-buffer clone
          (should (memq source mevedel-transcript-source--consumers))
          (should-not (mevedel-transcript-source--own-consumers))
          (let ((mine (mevedel-transcript-source-create 6)))
            (should (equal (list mine)
                           (mevedel-transcript-source--own-consumers))))))
      (should (equal (list source) (mevedel-transcript-source--own-consumers)))))

  :doc "excludes released proofs and is empty without consumers"
  (with-temp-buffer
    (should-not (mevedel-transcript-source--own-consumers))
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source-release source)
      (should-not (mevedel-transcript-source--own-consumers)))))

(defun test-mevedel-transcript-source--overlay-present-p ()
  "Return non-nil when the current buffer still has an overlay."
  (let ((lists (overlay-lists)))
    (or (car lists) (cdr lists))))

(mevedel-deftest mevedel-transcript-source--overlay-hooks-p
  ()
  ,test
  (test)
  :doc "is nil without overlays and for overlays without modification hooks"
  (with-temp-buffer
    (insert "abcdef")
    (should-not (mevedel-transcript-source--overlay-hooks-p))
    (let ((overlay (make-overlay 2 4)))
      (overlay-put overlay 'face 'bold)
      (overlay-put overlay 'modification-hooks nil)
      (should-not (mevedel-transcript-source--overlay-hooks-p))))

  :doc "finds each of the three hook properties"
  (dolist (property '(modification-hooks insert-in-front-hooks insert-behind-hooks))
    (with-temp-buffer
      (insert "abcdef")
      (let ((overlay (make-overlay 2 4)))
        (overlay-put overlay property (list #'ignore))
        (should (mevedel-transcript-source--overlay-hooks-p))
        (delete-overlay overlay)
        (should-not (mevedel-transcript-source--overlay-hooks-p)))))

  :doc "inspects every overlay, including empty ones, other endpoints and outside narrowing"
  (with-temp-buffer
    (insert "abcdef")
    (overlay-put (make-overlay 1 1) 'modification-hooks (list #'ignore))
    (should (mevedel-transcript-source--overlay-hooks-p)))
  (with-temp-buffer
    (insert "abcdef")
    (overlay-put (make-overlay 6 7) 'insert-behind-hooks (list #'ignore))
    (narrow-to-region 1 3)
    (should (mevedel-transcript-source--overlay-hooks-p)))

  :doc "ignores overlays of other buffers"
  (let ((other (generate-new-buffer " *transcript-source-overlay*")))
    (unwind-protect
        (progn
          (with-current-buffer other
            (insert "abc")
            (overlay-put (make-overlay 1 2) 'modification-hooks (list #'ignore)))
          (with-temp-buffer
            (insert "abc")
            (should-not (mevedel-transcript-source--overlay-hooks-p))))
      (kill-buffer other))))

(mevedel-deftest mevedel-transcript-source--hooks-ordered-p
  ()
  ,test
  (test)
  :doc "accepts the installed bracket and rejects a buffer without proof hooks"
  (with-temp-buffer
    (should-not (mevedel-transcript-source--hooks-ordered-p))
    (test-mevedel-transcript-source--setup)
    (should (mevedel-transcript-source--hooks-ordered-p))
    (mevedel-transcript-source-create 1)
    (should (mevedel-transcript-source--hooks-ordered-p)))

  :doc "keeps the global-hook marker between guard and seal"
  (with-temp-buffer
    (test-mevedel-transcript-source--setup)
    (should (memq t before-change-functions))
    (should (memq t after-change-functions))
    (should (< (cl-position #'mevedel-transcript-source--before-change
                            before-change-functions)
               (cl-position t before-change-functions)
               (cl-position #'mevedel-transcript-source--before-seal
                            before-change-functions)))
    (should (mevedel-transcript-source--hooks-ordered-p)))

  :doc "rejects a guard or seal that is no longer outermost"
  (dolist (spec '((before-change-functions -200 2)
                  (before-change-functions 200 2)
                  (after-change-functions -200 3)))
    (with-temp-buffer
      (test-mevedel-transcript-source--setup)
      (let ((hook (make-symbol "outer-hook")))
        (fset hook (if (= 2 (nth 2 spec))
                       (lambda (_start _end))
                     (lambda (_start _end _length))))
        (add-hook (nth 0 spec) hook (nth 1 spec) t)
        (should-not (mevedel-transcript-source--hooks-ordered-p))
        (remove-hook (nth 0 spec) hook t)
        (should (mevedel-transcript-source--hooks-ordered-p)))))

  :doc "rejects missing proof hooks and a marker outside the bracket"
  (dolist (hook '((before-change-functions
                   mevedel-transcript-source--before-change)
                  (before-change-functions
                   mevedel-transcript-source--before-seal)
                  (after-change-functions
                   mevedel-transcript-source--after-change)))
    (with-temp-buffer
      (test-mevedel-transcript-source--setup)
      (remove-hook (car hook) (cadr hook) t)
      (should-not (mevedel-transcript-source--hooks-ordered-p))))
  (dolist (spec '((before-change-functions
                   (t mevedel-transcript-source--before-change
                      mevedel-transcript-source--before-seal))
                  (before-change-functions
                   (mevedel-transcript-source--before-change
                    mevedel-transcript-source--before-seal t))
                  (after-change-functions
                   (t mevedel-transcript-source--after-change))))
    (with-temp-buffer
      (test-mevedel-transcript-source--setup)
      (set (make-local-variable (car spec)) (copy-sequence (cadr spec)))
      (should-not (mevedel-transcript-source--hooks-ordered-p)))))

(mevedel-deftest mevedel-transcript-source--before-seal
  ()
  ,test
  (test)
  :doc "keeps the frontier while the tick has not moved since the guard"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source--before-change 12 12)
      (mevedel-transcript-source--before-seal 12 12)
      (should (eql 6 (mevedel-transcript-source-dirty source)))))

  :doc "invalidates a proof whose buffer changed silently after its guard"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source--before-change 12 12)
      (let ((inhibit-modification-hooks t))
        (goto-char 6)
        (insert "silent"))
      (mevedel-transcript-source--before-seal 12 12)
      (should (eql 0 (mevedel-transcript-source-dirty source)))))

  :doc "invalidates a proof whose text a before-change hook converted silently"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup))
          (converter (make-symbol "multibyte-converter")))
      (goto-char 7)
      (insert "é")
      (mevedel-transcript-source-accept source)
      (fset converter (lambda (_start _end)
                        (remove-hook 'before-change-functions converter t)
                        (set-buffer-multibyte nil)))
      (add-hook 'before-change-functions converter 0 t)
      (goto-char (point-max))
      (insert "tail")
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "an overlay that joins during the before phase and removes itself invalidates the proof"
  (with-temp-buffer
    (let* ((source (test-mevedel-transcript-source--setup))
           (joiner (make-symbol "overlay-joiner")))
      (fset joiner (lambda (_start _end)
                     (remove-hook 'before-change-functions joiner t)
                     (test-mevedel-transcript-source--hook-overlay
                      (lambda () (save-excursion (goto-char 6) (insert "J")))
                      t)))
      (add-hook 'before-change-functions joiner 0 t)
      (goto-char (point-max))
      (insert "tail")
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "a cloned buffer's sealed change does not touch the original's proof"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (test-mevedel-transcript-source--with-clone clone
        (with-current-buffer clone
          (mevedel-transcript-source--before-seal 1 1))
        (should-not (mevedel-transcript-source-dirty source)))
      (mevedel-transcript-source--before-change 12 12)
      (test-mevedel-transcript-source--with-clone clone
        (with-current-buffer clone
          (let ((inhibit-modification-hooks t)) (insert "silent"))
          (mevedel-transcript-source--before-seal 1 1)))
      (should (eql 6 (mevedel-transcript-source-dirty source)))))

  :doc "ignores consumers that have no guarded change in flight"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (let ((inhibit-modification-hooks t))
        (insert "silent"))
      (mevedel-transcript-source--before-seal 1 1)
      (should-not (mevedel-transcript-source-dirty source)))))

(mevedel-deftest mevedel-transcript-source-call-with-change
  ()
  ,test
  (test)
  :doc "observes a silent property normalization of its region"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source-call-with-change
       9 11
       (lambda () (put-text-property 9 11 'face 'bold)))
      (should (eql 3 (mevedel-transcript-source-change source)))
      (should (eql (buffer-modified-tick) (mevedel-transcript-source-tick source)))
      (should-not (mevedel-transcript-source-before source))))

  :doc "pairs the change even when the function fails, and serves every consumer"
  (with-temp-buffer
    (let* ((first (test-mevedel-transcript-source--setup))
           (second (mevedel-transcript-source-create 8)))
      (mevedel-transcript-source-accept second)
      (should-error
       (mevedel-transcript-source-call-with-change
        10 12
        (lambda ()
          (put-text-property 10 12 'face 'bold)
          (error "Normalization failed"))))
      (should-not (mevedel-transcript-source-before first))
      (should (eql 4 (mevedel-transcript-source-change first)))
      (should (eql 2 (mevedel-transcript-source-change second)))))

  :doc "does not bless an earlier unobserved edit"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (let ((inhibit-modification-hooks t))
        (goto-char 7)
        (insert "skipped"))
      (mevedel-transcript-source-call-with-change
       12 14 (lambda () (put-text-property 12 14 'face 'bold)))
      (should (eql 0 (mevedel-transcript-source-change source))))))

(mevedel-deftest mevedel-transcript-source--after-change
  ()
  ,test
  (test)
  :doc "adopts the tick of a paired change and clears its before mark"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (insert "x")
      (should (eql (buffer-modified-tick) (mevedel-transcript-source-tick source)))
      (should-not (mevedel-transcript-source-before source))
      (should (integerp (mevedel-transcript-source-dirty source)))))

  :doc "invalidates an after-change that was never preceded by a before-change"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source--after-change 1 1 0)
      (should (eql 0 (mevedel-transcript-source-dirty source)))
      (should-not (mevedel-transcript-source-before source))))

  :doc "an earlier-ordered after-change hook's silent edit invalidates the proof"
  (with-temp-buffer
    (let* ((source (test-mevedel-transcript-source--setup))
           (hook (make-symbol "early-self-removing")))
      (fset hook (lambda (_start _end _length)
                   (save-excursion (goto-char 6) (insert "E"))
                   (remove-hook 'after-change-functions hook t)))
      (add-hook 'after-change-functions hook -200 t)
      (goto-char (point-max))
      (insert "tail")
      (should-not (memq hook after-change-functions))
      (should (mevedel-transcript-source--hooks-ordered-p))
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "an earlier-ordered after-change hook's silent edit invalidates the proof"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup))
          (done nil))
      (add-hook 'after-change-functions
                (lambda (_start _end _length)
                  (unless done
                    (setq done t)
                    (save-excursion (goto-char 6) (insert "X"))))
                -90 t)
      (goto-char (point-max))
      (insert "tail")
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "a cloned buffer's change does not adopt a tick for the original's proof"
  (with-temp-buffer
    (let* ((source (test-mevedel-transcript-source--setup))
           (tick (mevedel-transcript-source-tick source)))
      (test-mevedel-transcript-source--with-clone clone
        (with-current-buffer clone
          (mevedel-transcript-source--after-change 1 1 0)))
      (should (eql tick (mevedel-transcript-source-tick source)))
      (should-not (mevedel-transcript-source-dirty source))))

  :doc "completes property-only modifications and several consumers independently"
  (with-temp-buffer
    (let* ((first (test-mevedel-transcript-source--setup))
           (second (mevedel-transcript-source-create 6)))
      (mevedel-transcript-source-accept second)
      (put-text-property 8 9 'face 'bold)
      (should (eql (buffer-modified-tick) (mevedel-transcript-source-tick first)))
      (should (eql (buffer-modified-tick) (mevedel-transcript-source-tick second)))
      (should (eql 2 (mevedel-transcript-source-dirty first)))
      (should (eql 2 (mevedel-transcript-source-dirty second))))))

(mevedel-deftest mevedel-transcript-source-call-with-metadata-change
  ()
  ,test
  (test)
  :doc "returns the function's value and binds the exemption only during the call"
  (with-temp-buffer
    (should-not mevedel-transcript-source--metadata-change-p)
    (should (eq 'value
                (mevedel-transcript-source-call-with-metadata-change
                 (lambda ()
                   (should mevedel-transcript-source--metadata-change-p)
                   'value))))
    (should-not mevedel-transcript-source--metadata-change-p)
    (should-error
     (mevedel-transcript-source-call-with-metadata-change
      (lambda () (error "Escapes"))))
    (should-not mevedel-transcript-source--metadata-change-p))

  :doc "rebases the origin over a leading metadata edit without dirtying the body"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source-call-with-metadata-change
       (lambda ()
         (goto-char 2)
         (insert "-extra")))
      (should-not (mevedel-transcript-source-change source))
      (should (= (+ 6 (length "-extra"))
                 (test-mevedel-transcript-source--origin source)))
      (mevedel-transcript-source-call-with-metadata-change
       (lambda () (delete-region 2 5)))
      (should-not (mevedel-transcript-source-change source))
      (mevedel-transcript-source-call-with-metadata-change
       (lambda () (delete-region 2 4)))
      (should-not (mevedel-transcript-source-change source))
      ;; The origin is validated against the text, separately from the proof.
      (should (= (test-mevedel-transcript-source--content-start)
                 (test-mevedel-transcript-source--origin source)))
      (should (equal test-mevedel-transcript-source--body
                     (test-mevedel-transcript-source--body-string)))))

  :doc "leaves the proof's validity to the consumer when the origin is moved by hand"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (set-marker (mevedel-transcript-source-origin source) 8)
      (should-not (mevedel-transcript-source-change source))
      (should-not (= (test-mevedel-transcript-source--content-start)
                     (test-mevedel-transcript-source--origin source)))))

  :doc "still dirties body edits made inside the exemption"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source-call-with-metadata-change
       (lambda ()
         (goto-char 9)
         (insert "X")))
      (should (eql 3 (mevedel-transcript-source-change source)))))
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source-call-with-metadata-change
       (lambda ()
         (goto-char 6)
         (insert "X")))
      (should (eql 0 (mevedel-transcript-source-change source)))))
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source-call-with-metadata-change
       (lambda () (delete-region 4 9)))
      (should (eql 0 (mevedel-transcript-source-change source)))))
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source-call-with-metadata-change
       (lambda () (put-text-property 8 10 'face 'bold)))
      (should (eql 2 (mevedel-transcript-source-change source)))))

  :doc "does not let a metadata write cover a later or earlier body change"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source-call-with-metadata-change
       (lambda ()
         (goto-char 2)
         (insert "!")))
      (goto-char (point-max))
      (insert "body change")
      (should (eql (length test-mevedel-transcript-source--body)
                   (mevedel-transcript-source-change source)))))
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (goto-char 9)
      (insert "body first")
      (mevedel-transcript-source-call-with-metadata-change
       (lambda ()
         (goto-char 2)
         (insert "!")))
      (should (eql 3 (mevedel-transcript-source-change source)))))

  :doc "does not bless unknown or unobserved edits made inside the exemption"
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (mevedel-transcript-source-call-with-metadata-change
       (lambda ()
         (let ((inhibit-modification-hooks t))
           (goto-char 2)
           (insert "silent"))))
      (should (eql 0 (mevedel-transcript-source-change source)))))
  (with-temp-buffer
    (let ((source (test-mevedel-transcript-source--setup)))
      (goto-char 2)
      (insert "outside the wrapper")
      (should (eql 0 (mevedel-transcript-source-change source)))))

  :doc "rebases every consumer and nests without leaking the exemption"
  (with-temp-buffer
    (let* ((first (test-mevedel-transcript-source--setup))
           (second (mevedel-transcript-source-create 10)))
      (mevedel-transcript-source-accept second)
      (mevedel-transcript-source-call-with-metadata-change
       (lambda ()
         (mevedel-transcript-source-call-with-metadata-change
          (lambda () (goto-char 2) (insert "ab")))
         (should mevedel-transcript-source--metadata-change-p)))
      (should-not mevedel-transcript-source--metadata-change-p)
      (should-not (mevedel-transcript-source-change first))
      (should-not (mevedel-transcript-source-change second))
      (should (= 8 (test-mevedel-transcript-source--origin first)))
      (should (= 12 (test-mevedel-transcript-source--origin second))))))

;;; test-mevedel-transcript-source.el ends here
