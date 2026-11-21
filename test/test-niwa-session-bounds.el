;;; test-niwa-session-bounds.el --- Incremental gptel bounds -*- lexical-binding: t -*-

;;; Commentary:

;; Every case compares the retained-run answer with the byte-for-byte result
;; of the unadvised `gptel--get-buffer-bounds' on the same buffer, then checks
;; which path answered.  The oracle is the original function itself: outside
;; `niwa-session-bounds-call-with-cache' the advice passes straight through.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'gptel)
(require 'org)
(require 'niwa-session-artifacts)
(require 'niwa-session-bounds)
(require 'niwa-transcript)
(require 'niwa-transcript-source)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-niwa-bounds--insert (text &optional prop)
  "Insert TEXT at point, giving it the `gptel' PROP when non-nil."
  (let ((start (point)))
    (insert text)
    (when prop
      (put-text-property start (point) 'gptel prop))))

(defun test-niwa-bounds--fill (&optional chunks controls)
  "Fill the current buffer with a drawer and CHUNKS transcript chunks.
With CONTROLS, tool results are canonical closed tool blocks and every fourth
chunk also holds a closed reasoning block, which gives normalization a recent
closed control to resume from.  Without it a tool result is a bare `gptel'
tool run, an orphan whose later close could still recover it."
  (insert ":PROPERTIES:\n:GPTEL_BOUNDS: placeholder\n:END:\n")
  (dotimes (index (or chunks 40))
    (test-niwa-bounds--insert (format "user %d\n" index))
    (test-niwa-bounds--insert (format "response %d\n" index) 'response)
    (when (zerop (% index 3))
      (test-niwa-bounds--insert
       (if controls
           (format "#+begin_tool\n(:name \"Read\" :args nil)\n\nresult %d\n#+end_tool\n"
                   index)
         (format "tool %d\n" index))
       (cons 'tool (format "id-%d" index))))
    (when (zerop (% index 5))
      (test-niwa-bounds--insert (format "ignored %d\n" index) 'ignore))
    (when (and controls (zerop (% index 4)))
      (test-niwa-bounds--insert
       (format "#+begin_reasoning\nthought %d\n#+end_reasoning\n" index)
       'ignore))))

(defmacro test-niwa-bounds--isolated (&rest body)
  "Run BODY, then restore the bounds statistics and the gptel advice.
The statistics are rebound for the call.  The advice is removed afterwards
unless it was already installed when BODY began."
  (declare (indent 0))
  (let ((installed (make-symbol "installed")))
    `(let ((niwa-session-bounds--stats nil)
           (,installed (advice-member-p #'niwa-session-bounds--advice
                                        'gptel--get-buffer-bounds)))
       (unwind-protect (progn ,@body)
         (unless ,installed
           (advice-remove 'gptel--get-buffer-bounds
                          #'niwa-session-bounds--advice))))))

(defmacro test-niwa-bounds--with-buffer (&rest body)
  "Run BODY in a fresh, isolated Org buffer filled with a transcript."
  (declare (indent 0))
  `(test-niwa-bounds--isolated
     (with-temp-buffer
       (let ((org-agenda-file-menu-enabled nil))
         (org-mode))
       (test-niwa-bounds--fill)
       ,@body)))

(defun test-niwa-bounds--oracle ()
  "Return the original function's bounds."
  (gptel--get-buffer-bounds))

(defun test-niwa-bounds--cached ()
  "Return the retained-run bounds."
  (niwa-session-bounds-call-with-cache #'gptel--get-buffer-bounds))

(defun test-niwa-bounds--agree ()
  "Assert both answers are `equal' and return the cached one."
  (let ((oracle (test-niwa-bounds--oracle))
        (cached (test-niwa-bounds--cached)))
    (should (equal oracle cached))
    cached))

(defun test-niwa-bounds--stat (key)
  "Return how many answers of kind KEY were given."
  (or (plist-get niwa-session-bounds--stats key) 0))

(defun test-niwa-bounds--origin ()
  "Return the body start of the current buffer."
  (niwa-transcript--skip-leading-properties-drawer (point-min)))

(defmacro test-niwa-bounds--announcing (&rest body)
  "Run BODY, returning the body-side changes normalization announced.
Each is `(START END)' of a `niwa-transcript-source-call-with-change' whose
START is at or after the body start; the leading metadata clear is excluded.
Nothing is printed."
  (declare (indent 0))
  `(let ((announced nil)
         (change (symbol-function 'niwa-transcript-source-call-with-change)))
     (cl-letf (((symbol-function 'niwa-transcript-source-call-with-change)
                (lambda (start end function)
                  (when (>= start (niwa-session-bounds--content-start))
                    (push (list start end) announced))
                  (funcall change start end function))))
       ,@body)
     (nreverse announced)))

(defun test-niwa-bounds--runs (bounds)
  "Return BOUNDS' runs as ascending `(BEG END)' pairs."
  (sort (cl-loop for (_name . runs) in bounds
                 append (mapcar (lambda (run) (list (nth 0 run) (nth 1 run))) runs))
        (lambda (left right) (< (car left) (car right)))))

(defun test-niwa-bounds--width (runs origin dirty size origin-after)
  "Return the body characters a rescan after DIRTY must cover.
RUNS are the pairs before the edit, in coordinates of ORIGIN.  DIRTY is the
first changed absolute position.  The cache retains runs ending strictly
before it and rescans from the last retained end to the end of a body that
ends at SIZE and starts at ORIGIN-AFTER."
  (let ((from (or (cl-loop for run in runs
                           when (< (nth 1 run) dirty)
                           maximize (nth 1 run))
                  origin)))
    (- (- size origin-after) (- from origin))))

(defun test-niwa-bounds--measure-append (controls text prop)
  "Settle a transcript, append TEXT carrying PROP, and settle it again.
CONTROLS selects the fixture.  Return a plist of what the second pass did:
`:announced', `:stats', `:expected' (the width implied by the earliest
change) and `:oracle' (the settled bounds, which the original computes)."
  (test-niwa-bounds--fill 30 controls)
  (niwa-session-artifacts-stabilize-gptel-bounds)
  (let* ((before (test-niwa-bounds--runs (test-niwa-bounds--oracle)))
         (origin (test-niwa-bounds--origin))
         (old-end (point-max))
         size announced)
    (setq niwa-session-bounds--stats nil)
    (setq announced
          (test-niwa-bounds--announcing
            (goto-char (point-max))
            (test-niwa-bounds--insert text prop)
            (setq size (point-max))
            (niwa-session-artifacts-stabilize-gptel-bounds)))
    (list :announced announced
          :stats (copy-sequence niwa-session-bounds--stats)
          :expected (test-niwa-bounds--width
                     before origin
                     (apply #'min old-end (mapcar #'car announced))
                     (point-max) (test-niwa-bounds--origin))
          :size size
          :oracle (test-niwa-bounds--oracle))))

(niwa-deftest niwa-session-bounds-get
  (:vars ((niwa-session-bounds--stats nil)
         (bounds-advice-installed
          (advice-member-p (function niwa-session-bounds--advice)
                           (quote gptel--get-buffer-bounds))))
   :after-each
   (unless bounds-advice-installed
     (advice-remove (quote gptel--get-buffer-bounds)
                    (function niwa-session-bounds--advice))))
  ,test
  (test)
  :doc "the first answer is computed in full and an unchanged buffer is a hit"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--agree)
    (should (= 1 (test-niwa-bounds--stat :full)))
    (test-niwa-bounds--agree)
    (should (= 1 (test-niwa-bounds--stat :hit)))
    ;; A drawer and a first body run without values are no straddling run.
    (should (= 0 (test-niwa-bounds--stat :fallback))))
  :doc "a tail append rescans only the tail"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--agree)
    (goto-char (point-max))
    (test-niwa-bounds--insert "tail response\n" 'response)
    (test-niwa-bounds--agree)
    (should (= 1 (test-niwa-bounds--stat :incremental)))
    (should (= 1 (test-niwa-bounds--stat :full))))
  :doc "an insertion and a deletion in the middle are recomputed from the edit"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--agree)
    (goto-char (+ (test-niwa-bounds--origin) 400))
    (test-niwa-bounds--insert "middle tool\n" (cons 'tool "middle"))
    (test-niwa-bounds--agree)
    (delete-region (+ (test-niwa-bounds--origin) 200)
                   (+ (test-niwa-bounds--origin) 260))
    (test-niwa-bounds--agree)
    (should (= 2 (test-niwa-bounds--stat :incremental)))
    (should (= 1 (test-niwa-bounds--stat :full))))
  :doc "a property-only change in the middle is observed"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--agree)
    (let ((from (+ (test-niwa-bounds--origin) 300)))
      (put-text-property from (+ from 90) 'gptel 'ignore))
    (test-niwa-bounds--agree)
    (let ((from (+ (test-niwa-bounds--origin) 500)))
      (remove-text-properties from (+ from 70) '(gptel nil)))
    (test-niwa-bounds--agree)
    (should (= 2 (test-niwa-bounds--stat :incremental))))
  :doc "equal but non-eq property values stay separate runs, as in the original"
  (test-niwa-bounds--with-buffer
    (goto-char (point-max))
    (test-niwa-bounds--insert "first\n" (cons 'tool "same"))
    (test-niwa-bounds--insert "second\n" (cons 'tool "same"))
    (let ((cached (test-niwa-bounds--agree)))
      (should (= 2 (length (cl-remove-if-not
                            (lambda (bound) (equal "same" (nth 2 bound)))
                            (alist-get 'tool cached))))))
    ;; Making one object serve both runs merges them in both answers.
    (let ((shared (cons 'tool "same")))
      (put-text-property (- (point-max) 13) (point-max) 'gptel shared))
    (let ((cached (test-niwa-bounds--agree)))
      (should (= 1 (length (cl-remove-if-not
                            (lambda (bound) (equal "same" (nth 2 bound)))
                            (alist-get 'tool cached)))))))
  :doc "an overlay carrying gptel falls back to the original function"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--agree)
    (let ((overlay (make-overlay (+ (test-niwa-bounds--origin) 20)
                                 (+ (test-niwa-bounds--origin) 80))))
      (overlay-put overlay 'gptel 'ignore)
      (test-niwa-bounds--agree)
      (should (= 1 (test-niwa-bounds--stat :fallback)))
      (delete-overlay overlay))
    ;; An overlay without it is irrelevant, and the cache recovers.
    (let ((overlay (make-overlay 5 30)))
      (overlay-put overlay 'face 'bold)
      (test-niwa-bounds--agree)
      (delete-overlay overlay))
    (should (= 1 (test-niwa-bounds--stat :fallback))))
  :doc "narrowing during the call does not change the answer"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--agree)
    (let ((from (+ (test-niwa-bounds--origin) 300)))
      (narrow-to-region from (+ from 300))
      (goto-char (point-max))
      (insert "narrowed\n")
      (let ((oracle (test-niwa-bounds--oracle))
            (cached (test-niwa-bounds--cached)))
        (should (equal oracle cached)))
      (should (= (point-min) from)))
    (widen)
    (test-niwa-bounds--agree))
  :doc "known metadata growth moves positions without rescanning the body"
  (test-niwa-bounds--with-buffer
    (let ((before (test-niwa-bounds--agree)))
      (should (equal before (test-niwa-bounds--oracle))))
    (dolist (growth '(1 7 90 1000))
      (niwa-transcript-source-call-with-metadata-change
       (lambda ()
         (save-excursion
           (goto-char 20)
           (insert (make-string growth ?x)))))
      (test-niwa-bounds--agree))
    (should (= 4 (test-niwa-bounds--stat :rebuilt)))
    (should (= 1 (test-niwa-bounds--stat :full)))
    (should (= 0 (test-niwa-bounds--stat :incremental)))
    ;; Shrinking is the same.
    (niwa-transcript-source-call-with-metadata-change
     (lambda () (delete-region 20 120)))
    (test-niwa-bounds--agree)
    (should (= 5 (test-niwa-bounds--stat :rebuilt))))
  :doc "a metadata edit that was not announced is a body edit and stays correct"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--agree)
    (save-excursion (goto-char 20) (insert "unannounced"))
    (test-niwa-bounds--agree)
    (should (= 2 (test-niwa-bounds--stat :full))))
  :doc "an edit with modification hooks inhibited falls back to a full rescan"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--agree)
    (let ((inhibit-modification-hooks t)
          (from (+ (test-niwa-bounds--origin) 700)))
      (put-text-property from (+ from 50) 'gptel 'ignore))
    (test-niwa-bounds--agree)
    (with-silent-modifications
      (let ((from (+ (test-niwa-bounds--origin) 100)))
        (put-text-property from (+ from 30) 'gptel 'response)))
    (test-niwa-bounds--agree)
    (should (= 3 (test-niwa-bounds--stat :full)))
    (should (= 0 (test-niwa-bounds--stat :incremental))))
  :doc "rewriting the whole buffer, as compaction does, is recomputed"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--agree)
    (erase-buffer)
    (test-niwa-bounds--fill 5)
    (test-niwa-bounds--agree)
    (should (= 2 (test-niwa-bounds--stat :full))))
  :doc "properties in the drawer are part of the answer"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--agree)
    (niwa-transcript-source-call-with-metadata-change
     (lambda () (put-text-property 3 9 'gptel 'ignore)))
    (let ((cached (test-niwa-bounds--agree)))
      (should (equal '(3 9) (car (alist-get 'ignore cached)))))
    (should (= 1 (test-niwa-bounds--stat :full))))
  :doc "a run straddling the body start is answered by the original function"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--agree)
    (let ((origin (test-niwa-bounds--origin)))
      (niwa-transcript-source-call-with-metadata-change
       (lambda () (put-text-property (- origin 4) origin 'gptel 'ignore)))
      (put-text-property origin (+ origin 5) 'gptel 'ignore))
    (test-niwa-bounds--agree)
    (should (= 1 (test-niwa-bounds--stat :fallback))))
  :doc "empty and property-free buffers answer nil"
  (with-temp-buffer
    (org-mode)
    (should-not (test-niwa-bounds--agree))
    (insert ":PROPERTIES:\n:END:\nplain\n")
    (should-not (test-niwa-bounds--agree)))
  :doc "a random edit sequence always matches the original byte for byte"
  (test-niwa-bounds--with-buffer
    (let ((props (list 'response 'ignore 'niwa-render-data nil))
          ;; Deterministic, and local: the global random state is not touched.
          (rand (let ((state 20241020))
                  (lambda (limit)
                    (setq state (% (+ (* state 1103515245) 12345) 2147483648))
                    (% (/ state 65536) limit)))))
      (dotimes (step 140)
        (let* ((origin (test-niwa-bounds--origin))
               (size (max 1 (- (point-max) origin)))
               (position (+ origin (funcall rand size)))
               (width (1+ (funcall rand 80)))
               (prop (if (zerop (funcall rand 4))
                         (cons 'tool (format "id-%d" (funcall rand 6)))
                       (nth (funcall rand (length props)) props))))
          (pcase (funcall rand 9)
            (0 (goto-char (point-max))
               (test-niwa-bounds--insert (format "tail %d\n" step) prop))
            (1 (goto-char position)
               (test-niwa-bounds--insert (format "mid %d\n" step) prop))
            (2 (delete-region position (min (point-max) (+ position width))))
            (3 (put-text-property position (min (point-max) (+ position width))
                                  'gptel prop))
            (4 (remove-text-properties
                position (min (point-max) (+ position width)) '(gptel nil)))
            (5 (niwa-transcript-source-call-with-metadata-change
                (lambda ()
                  (save-excursion
                    (goto-char (+ 12 (funcall rand 8)))
                    (insert (make-string (1+ (funcall rand 30)) ?m))))))
            (6 (with-silent-modifications
                 (put-text-property position (min (point-max) (+ position width))
                                    'gptel prop)))
            (7 (let ((inhibit-modification-hooks t))
                 (goto-char position)
                 (test-niwa-bounds--insert "quiet\n" prop)))
            (8 (narrow-to-region (min position (1- (point-max))) (point-max))))
          (let ((oracle (test-niwa-bounds--oracle))
                (cached (test-niwa-bounds--cached)))
            (should (equal oracle cached)))
          (widen)))
      (should (> (test-niwa-bounds--stat :incremental) 0))
      (should (> (test-niwa-bounds--stat :full) 1))))
  :doc "a cloned buffer neither reuses nor invalidates its original's proof"
  (test-niwa-bounds--with-buffer
    (let* ((first (test-niwa-bounds--cached))
           (cache niwa-session-bounds--cache)
           (clone (clone-buffer nil nil)))
      (unwind-protect
          (progn
            ;; The clone inherited the variable and the cache object itself.
            (should (eq cache (buffer-local-value 'niwa-session-bounds--cache clone)))
            (with-current-buffer clone
              (setq niwa-session-bounds--stats nil)
              (should (equal (test-niwa-bounds--oracle) (test-niwa-bounds--cached)))
              ;; Derived afresh from its own proof, not the inherited one.
              (should (= 1 (test-niwa-bounds--stat :full)))
              (should-not (eq cache niwa-session-bounds--cache))
              (should (eq clone (marker-buffer
                                 (niwa-transcript-source-origin
                                  (niwa-session-bounds--cache-source
                                   niwa-session-bounds--cache))))))
            (setq niwa-session-bounds--stats nil)
            (should (equal first (test-niwa-bounds--cached)))
            (should (eq cache niwa-session-bounds--cache))
            (should (= 1 (test-niwa-bounds--stat :hit))))
        (kill-buffer clone)))))

(niwa-deftest niwa-session-bounds-call-with-cache
  (:vars ((niwa-session-bounds--stats nil)
         (bounds-advice-installed
          (advice-member-p (function niwa-session-bounds--advice)
                           (quote gptel--get-buffer-bounds))))
   :after-each
   (unless bounds-advice-installed
     (advice-remove (quote gptel--get-buffer-bounds)
                    (function niwa-session-bounds--advice))))
  ,test
  (test)
  :doc "only the marked buffer is served from the cache"
  (test-niwa-bounds--with-buffer
    (let ((other (generate-new-buffer " *bounds-other*")))
      (unwind-protect
          (progn
            (niwa-session-bounds-install)
            (test-niwa-bounds--oracle)
            (should (= 0 (test-niwa-bounds--stat :full)))
            (niwa-session-bounds-call-with-cache
             (lambda ()
               (with-current-buffer other
                 (insert "unmarked")
                 (put-text-property 1 5 'gptel 'response)
                 (should (equal '((response (1 5)))
                                (gptel--get-buffer-bounds))))))
            (should (= 0 (test-niwa-bounds--stat :full)))
            (test-niwa-bounds--cached)
            (should (= 1 (test-niwa-bounds--stat :full))))
        (kill-buffer other))))
  :doc "installing twice leaves one advice"
  (progn
    (niwa-session-bounds-install)
    (niwa-session-bounds-install)
    (let ((count 0))
      (advice-mapc (lambda (function _props)
                     (when (eq function #'niwa-session-bounds--advice)
                       (cl-incf count)))
                   'gptel--get-buffer-bounds)
      (should (= 1 count)))))

(niwa-deftest niwa-session-bounds--count ()
  ,test
  (test)
  :doc "counts each kind of answer separately"
  (let ((niwa-session-bounds--stats nil))
    (niwa-session-bounds--count :hit)
    (niwa-session-bounds--count :hit)
    (niwa-session-bounds--count :full)
    (should (equal '(2 1) (list (plist-get niwa-session-bounds--stats :hit)
                                (plist-get niwa-session-bounds--stats :full))))))

(niwa-deftest niwa-session-bounds--overlay-gptel-p ()
  ,test
  (test)
  :doc "only an overlay carrying gptel counts"
  (test-niwa-bounds--with-buffer
    (should-not (niwa-session-bounds--overlay-gptel-p))
    (let ((other (make-overlay 5 20)))
      (overlay-put other 'face 'bold)
      (should-not (niwa-session-bounds--overlay-gptel-p))
      (delete-overlay other))
    (let ((marked (make-overlay 5 20)))
      (overlay-put marked 'gptel 'ignore)
      (should (niwa-session-bounds--overlay-gptel-p))
      (delete-overlay marked))))

(niwa-deftest niwa-session-bounds--scan ()
  ,test
  (test)
  :doc "returns ascending non-nil runs, split on eq and clipped to the range"
  (with-temp-buffer
    (insert "aaaabbbbccccdddd")
    (put-text-property 1 5 'gptel 'response)
    (put-text-property 9 13 'gptel (cons 'tool "x"))
    (put-text-property 13 17 'gptel (cons 'tool "x"))
    (should (equal '((1 5 response) (9 13 (tool . "x")) (13 17 (tool . "x")))
                   (niwa-session-bounds--scan 1 17)))
    (should (equal '((9 13 (tool . "x")))
                   (niwa-session-bounds--scan 9 13)))
    (should (equal '((3 5 response))
                   (niwa-session-bounds--scan 3 7)))
    (should-not (niwa-session-bounds--scan 5 9))
    (should-not (niwa-session-bounds--scan 7 7))))

(niwa-deftest niwa-session-bounds--content-start ()
  ,test
  (test)
  :doc "is the position after the leading drawer, or the buffer start"
  (test-niwa-bounds--with-buffer
    (should (= (test-niwa-bounds--origin)
               (niwa-session-bounds--content-start)))
    (should (> (niwa-session-bounds--content-start) (point-min))))
  :doc "a buffer without a drawer starts at its first position"
  (with-temp-buffer
    (insert "plain text\n")
    (should (= (point-min) (niwa-session-bounds--content-start)))))

(niwa-deftest niwa-session-bounds--reset ()
  ,test
  (test)
  :doc "drops the cache and releases its proof"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--cached)
    (should niwa-session-bounds--cache)
    (should niwa-transcript-source--consumers)
    (let ((marker (niwa-transcript-source-origin
                   (niwa-session-bounds--cache-source
                    niwa-session-bounds--cache))))
      (niwa-session-bounds--reset)
      (should-not niwa-session-bounds--cache)
      (should-not niwa-transcript-source--consumers)
      (should-not (marker-buffer marker)))
    ;; Resetting nothing is harmless, and the next answer is derived afresh.
    (niwa-session-bounds--reset)
    (test-niwa-bounds--agree)
    (should (= 2 (test-niwa-bounds--stat :full))))
  :doc "resetting in a clone leaves its original's proof registered"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--cached)
    (let* ((original (current-buffer))
           (source (niwa-session-bounds--cache-source
                    niwa-session-bounds--cache))
           (clone (clone-buffer nil nil)))
      (unwind-protect
          (progn
            (with-current-buffer clone
              ;; The inherited cache holds the original's proof.
              (niwa-session-bounds--reset)
              (should-not niwa-session-bounds--cache))
            (should (eq original (marker-buffer
                                  (niwa-transcript-source-origin source))))
            (should (memq source (buffer-local-value
                                  'niwa-transcript-source--consumers original)))
            (should niwa-session-bounds--cache))
        (kill-buffer clone)))))

(niwa-deftest niwa-session-bounds--absolute ()
  ,test
  (test)
  :doc "turns body relative runs into absolute ones from a list or a vector"
  (should (equal '((11 15 response) (20 30 ignore))
                 (niwa-session-bounds--absolute '((1 5 response) (10 20 ignore)) 10)))
  (should (equal '((11 15 response))
                 (niwa-session-bounds--absolute (vector '(1 5 response)) 10)))
  (should-not (niwa-session-bounds--absolute [] 10)))

(niwa-deftest niwa-session-bounds--refresh ()
  ,test
  (test)
  :doc "an unchanged body returns the very same result object"
  (test-niwa-bounds--with-buffer
    (let ((first (test-niwa-bounds--cached)))
      (should (eq first (test-niwa-bounds--cached)))
      (should (= 1 (test-niwa-bounds--stat :hit)))))
  :doc "a moved origin rebuilds the alist from unchanged runs"
  (test-niwa-bounds--with-buffer
    (let ((first (test-niwa-bounds--cached)))
      (niwa-transcript-source-call-with-metadata-change
       (lambda () (save-excursion (goto-char 20) (insert "grown"))))
      (let ((second (test-niwa-bounds--cached)))
        (should-not (eq first second))
        (should (equal second (test-niwa-bounds--oracle)))
        (should (= 1 (test-niwa-bounds--stat :rebuilt)))
        (should (= 1 (test-niwa-bounds--stat :full))))))
  :doc "a silent normalization announced for a body suffix is incremental"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--cached)
    (let ((from (+ (test-niwa-bounds--origin) 500)))
      (niwa-transcript-source-call-with-change
       from (point-max)
       (lambda ()
         (with-silent-modifications
           (put-text-property from (+ from 40) 'gptel 'ignore)))))
    (test-niwa-bounds--agree)
    (should (= 1 (test-niwa-bounds--stat :incremental)))
    (should (= 1 (test-niwa-bounds--stat :full))))
  :doc "an announced silent change from before the body is a full rescan"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--cached)
    (niwa-transcript-source-call-with-change
     (point-min) (point-max)
     (lambda ()
       (with-silent-modifications
         (put-text-property (+ (test-niwa-bounds--origin) 100)
                            (+ (test-niwa-bounds--origin) 130) 'gptel 'ignore))))
    (test-niwa-bounds--agree)
    (should (= 2 (test-niwa-bounds--stat :full))))
  :doc "an announced drawer-only normalization keeps the runs"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--cached)
    (let ((origin (test-niwa-bounds--origin)))
      (niwa-transcript-source-call-with-metadata-change
       (lambda ()
         (niwa-transcript-source-call-with-change
          (point-min) origin
          (lambda ()
            (with-silent-modifications
              (remove-text-properties (point-min) origin '(gptel nil))))))))
    (test-niwa-bounds--agree)
    (should (= 1 (test-niwa-bounds--stat :full)))
    (should (> (+ (test-niwa-bounds--stat :hit)
                  (test-niwa-bounds--stat :rebuilt))
               0)))
  :doc "a body edit is incremental and records the new baseline"
  (test-niwa-bounds--with-buffer
    (test-niwa-bounds--cached)
    (goto-char (point-max))
    (test-niwa-bounds--insert "late\n" 'response)
    (test-niwa-bounds--agree)
    (should (= 1 (test-niwa-bounds--stat :incremental)))
    (test-niwa-bounds--agree)
    (should (= 1 (test-niwa-bounds--stat :hit)))))

(niwa-deftest niwa-session-bounds--advice ()
  ,test
  (test)
  :doc "an unmarked buffer reaches the original with its arguments"
  (let ((seen nil))
    (let ((niwa-session-bounds--active-buffer nil))
      (should (eq 'original
                  (niwa-session-bounds--advice
                   (lambda (&rest arguments) (setq seen arguments) 'original)
                   1 2))))
    (should (equal '(1 2) seen)))
  :doc "the marked buffer is answered by the cache, using the original on fallback"
  (test-niwa-bounds--with-buffer
    (let ((niwa-session-bounds--active-buffer (current-buffer)))
      (should (equal (test-niwa-bounds--oracle)
                     (niwa-session-bounds--advice #'gptel--get-buffer-bounds))))
    (should (= 1 (test-niwa-bounds--stat :full)))))

(niwa-deftest niwa-session-bounds--retained ()
  ,test
  (test)
  :doc "counts the runs that end strictly before the first dirty offset"
  (let ((runs (vector '(0 5 a) '(8 12 b) '(12 20 c) '(30 40 d))))
    (should (= 0 (niwa-session-bounds--retained runs 0)))
    (should (= 0 (niwa-session-bounds--retained runs 5)))
    (should (= 1 (niwa-session-bounds--retained runs 6)))
    (should (= 2 (niwa-session-bounds--retained runs 13)))
    (should (= 2 (niwa-session-bounds--retained runs 20)))
    (should (= 4 (niwa-session-bounds--retained runs 41))))
  :doc "an empty run list retains nothing"
  (should (= 0 (niwa-session-bounds--retained [] 10))))

(niwa-deftest niwa-session-bounds--alist ()
  ,test
  (test)
  :doc "orders keys and runs as the original backward scan does"
  (let ((runs (list (list 1 4 'response) (list 4 9 (cons 'tool "a"))
                    (list 9 12 'response) (list 12 15 (cons 'tool "b"))
                    (list 15 20 'ignore))))
    (should (equal '((response (1 4) (9 12))
                     (tool (4 9 "a") (12 15 "b"))
                     (ignore (15 20)))
                   (niwa-session-bounds--alist runs))))
  :doc "a tool run without an identifier keeps its two element form"
  (should (equal '((tool (1 3)))
                 (niwa-session-bounds--alist
                  (list (list 1 3 (cons 'tool nil)))))))

(niwa-deftest niwa-session-artifacts-stabilize-gptel-bounds
  (:vars ((niwa-session-bounds--stats nil)
         (bounds-advice-installed
          (advice-member-p (function niwa-session-bounds--advice)
                           (quote gptel--get-buffer-bounds))))
   :after-each
   (unless bounds-advice-installed
     (advice-remove (quote gptel--get-buffer-bounds)
                    (function niwa-session-bounds--advice))))
  ,test
  (test)
  :doc "the settled GPTEL_BOUNDS and buffer equal the original loop's result"
  (let (reference)
    (with-temp-buffer
      (let ((org-agenda-file-menu-enabled nil)) (org-mode))
      (test-niwa-bounds--fill 60)
      (let ((copy (current-buffer)))
        ;; The original loop, on the unadvised function, in a copy.
        (with-temp-buffer
          (let ((org-agenda-file-menu-enabled nil)) (org-mode))
          (insert-buffer-substring copy)
          (let ((last nil) (done nil) (attempts 0) (print-length nil) (print-level nil))
            (niwa-transcript-normalize-properties)
            (while (and (not done) (< attempts 8))
              (setq attempts (1+ attempts))
              (let ((serialized (when-let* ((bounds (gptel--get-buffer-bounds)))
                                  (prin1-to-string bounds))))
                (cond
                 ((null serialized)
                  (niwa-session-artifacts-property-delete-direct "GPTEL_BOUNDS")
                  (setq done t))
                 ((equal serialized last) (setq done t))
                 (t (setq last serialized)
                    (niwa-session-artifacts--property-put-direct
                     "GPTEL_BOUNDS" serialized))))))
          (setq reference (buffer-string)))
        (setq niwa-session-bounds--stats nil)
        (niwa-session-artifacts-stabilize-gptel-bounds)
        (should (equal-including-properties reference (buffer-string)))
        ;; Stabilizing again, with the drawer already settled, changes nothing.
        (niwa-session-artifacts-stabilize-gptel-bounds)
        (should (equal-including-properties reference (buffer-string)))
        ;; Normalization writes properties with hooks inhibited, so the
        ;; proof rescans until it records its own silent changes.
        (should (> (+ (test-niwa-bounds--stat :hit)
                      (test-niwa-bounds--stat :rebuilt)
                      (test-niwa-bounds--stat :incremental)
                      (test-niwa-bounds--stat :full))
                   0)))))
  :doc "a canonical tail append rescans exactly from the earliest announced change"
  (with-temp-buffer
    (let ((org-agenda-file-menu-enabled nil)) (org-mode))
    (let* ((seen (test-niwa-bounds--measure-append
                  t "appended response\n" 'response))
           (stats (plist-get seen :stats))
           (announced (plist-get seen :announced)))
      ;; Normalization announces one suffix that starts at or before the tail.
      (should (= 1 (length announced)))
      (should (= (plist-get seen :size) (cadr (car announced))))
      (should (= 0 (or (plist-get stats :full) 0)))
      (should (= 1 (plist-get stats :incremental)))
      (should (= (plist-get seen :expected) (plist-get stats :scanned)))
      ;; And that is far less than the body.
      (should (< (plist-get stats :scanned)
                 (/ (- (point-max) (test-niwa-bounds--origin)) 2)))
      (should (equal (plist-get seen :oracle) (test-niwa-bounds--cached)))
      (let ((incremental (buffer-string)))
        (niwa-session-bounds--reset)
        (niwa-session-artifacts-stabilize-gptel-bounds)
        (should (equal-including-properties incremental (buffer-string))))))
  :doc "an orphan tail append announces no body change and rescans only the tail"
  (with-temp-buffer
    (let ((org-agenda-file-menu-enabled nil)) (org-mode))
    (let* ((seen (test-niwa-bounds--measure-append
                  nil "appended response\n" 'response))
           (stats (plist-get seen :stats)))
      ;; The orphans leave nothing to rewrite, so only the append is dirty.
      (should-not (plist-get seen :announced))
      (should (= 0 (or (plist-get stats :full) 0)))
      (should (= 1 (plist-get stats :incremental)))
      (should (= (plist-get seen :expected) (plist-get stats :scanned)))
      (should (equal (plist-get seen :oracle) (test-niwa-bounds--cached)))
      (let ((incremental (buffer-string)))
        (niwa-session-bounds--reset)
        (niwa-session-artifacts-stabilize-gptel-bounds)
        (should (equal-including-properties incremental (buffer-string))))))
  :doc "a closed control after orphans announces from the earliest orphan and rescans fully"
  (with-temp-buffer
    (let ((org-agenda-file-menu-enabled nil)) (org-mode))
    (test-niwa-bounds--fill 30)
    (niwa-session-artifacts-stabilize-gptel-bounds)
    (let* ((origin (test-niwa-bounds--origin))
           (orphan (car (alist-get 'tool (test-niwa-bounds--oracle))))
           (announced nil)
           size)
      (setq niwa-session-bounds--stats nil)
      (setq announced
            (test-niwa-bounds--announcing
              (goto-char (point-max))
              (test-niwa-bounds--insert
               "#+begin_reasoning\nlater\n#+end_reasoning\n" 'ignore)
              (setq size (point-max))
              (niwa-session-artifacts-stabilize-gptel-bounds)))
      ;; Normalization resumes at the earliest orphan's line, before the
      ;; control that gave it something to write.
      (should (equal (list (car orphan)) (mapcar #'car announced)))
      (should (= size (cadr (car announced))))
      ;; No run ends before that offset, so the whole body is rescanned once.
      (should (= 1 (plist-get niwa-session-bounds--stats :full)))
      (should (= 0 (or (plist-get niwa-session-bounds--stats :incremental) 0)))
      (should (= (- size origin) (plist-get niwa-session-bounds--stats :scanned)))
      (should (equal (test-niwa-bounds--oracle) (test-niwa-bounds--cached)))
      (let ((full (buffer-string)))
        (niwa-session-bounds--reset)
        (niwa-session-artifacts-stabilize-gptel-bounds)
        (should (equal-including-properties full (buffer-string))))))
  :doc "stabilizing an unchanged transcript never rescans the body"
  (with-temp-buffer
    (let ((org-agenda-file-menu-enabled nil)) (org-mode))
    (test-niwa-bounds--fill 30 t)
    (niwa-session-artifacts-stabilize-gptel-bounds)
    (setq niwa-session-bounds--stats nil)
    (niwa-session-artifacts-stabilize-gptel-bounds)
    (should (= 0 (test-niwa-bounds--stat :full)))
    (should (= 0 (test-niwa-bounds--stat :incremental)))
    (should (> (+ (test-niwa-bounds--stat :hit)
                  (test-niwa-bounds--stat :rebuilt))
               0))))

(provide 'test-niwa-session-bounds)

;;; test-niwa-session-bounds.el ends here
