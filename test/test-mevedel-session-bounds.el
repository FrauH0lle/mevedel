;;; test-mevedel-session-bounds.el --- Incremental gptel bounds -*- lexical-binding: t -*-

;;; Commentary:

;; Every case compares the retained-run answer with the byte-for-byte result
;; of the unadvised `gptel--get-buffer-bounds' on the same buffer, then checks
;; which path answered.  The oracle is the original function itself: outside
;; `mevedel-session-bounds-call-with-cache' the advice passes straight through.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'gptel)
(require 'org)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-bounds)
(require 'mevedel-transcript)
(require 'mevedel-transcript-source)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-bounds--insert (text &optional prop)
  "Insert TEXT at point, giving it the `gptel' PROP when non-nil."
  (let ((start (point)))
    (insert text)
    (when prop
      (put-text-property start (point) 'gptel prop))))

(defun test-mevedel-bounds--fill (&optional chunks controls)
  "Fill the current buffer with a drawer and CHUNKS transcript chunks.
With CONTROLS, tool results are canonical closed tool blocks and every fourth
chunk also holds a closed reasoning block, which gives normalization a recent
closed control to resume from.  Without it a tool result is a bare `gptel'
tool run, an orphan whose later close could still recover it."
  (insert ":PROPERTIES:\n:GPTEL_BOUNDS: placeholder\n:END:\n")
  (dotimes (index (or chunks 40))
    (test-mevedel-bounds--insert (format "user %d\n" index))
    (test-mevedel-bounds--insert (format "response %d\n" index) 'response)
    (when (zerop (% index 3))
      (test-mevedel-bounds--insert
       (if controls
           (format "#+begin_tool\n(:name \"Read\" :args nil)\n\nresult %d\n#+end_tool\n"
                   index)
         (format "tool %d\n" index))
       (cons 'tool (format "id-%d" index))))
    (when (zerop (% index 5))
      (test-mevedel-bounds--insert (format "ignored %d\n" index) 'ignore))
    (when (and controls (zerop (% index 4)))
      (test-mevedel-bounds--insert
       (format "#+begin_reasoning\nthought %d\n#+end_reasoning\n" index)
       'ignore))))

(defmacro test-mevedel-bounds--isolated (&rest body)
  "Run BODY, then restore the bounds statistics and the gptel advice.
The statistics are rebound for the call.  The advice is removed afterwards
unless it was already installed when BODY began."
  (declare (indent 0))
  (let ((installed (make-symbol "installed")))
    `(let ((mevedel-session-bounds--stats nil)
           (,installed (advice-member-p #'mevedel-session-bounds--advice
                                        'gptel--get-buffer-bounds)))
       (unwind-protect (progn ,@body)
         (unless ,installed
           (advice-remove 'gptel--get-buffer-bounds
                          #'mevedel-session-bounds--advice))))))

(defmacro test-mevedel-bounds--with-buffer (&rest body)
  "Run BODY in a fresh, isolated Org buffer filled with a transcript."
  (declare (indent 0))
  `(test-mevedel-bounds--isolated
     (with-temp-buffer
       (let ((org-agenda-file-menu-enabled nil))
         (org-mode))
       (test-mevedel-bounds--fill)
       ,@body)))

(defun test-mevedel-bounds--oracle ()
  "Return the original function's bounds."
  (gptel--get-buffer-bounds))

(defun test-mevedel-bounds--cached ()
  "Return the retained-run bounds."
  (mevedel-session-bounds-call-with-cache #'gptel--get-buffer-bounds))

(defun test-mevedel-bounds--agree ()
  "Assert both answers are `equal' and return the cached one."
  (let ((oracle (test-mevedel-bounds--oracle))
        (cached (test-mevedel-bounds--cached)))
    (should (equal oracle cached))
    cached))

(defun test-mevedel-bounds--stat (key)
  "Return how many answers of kind KEY were given."
  (or (plist-get mevedel-session-bounds--stats key) 0))

(defun test-mevedel-bounds--origin ()
  "Return the body start of the current buffer."
  (mevedel-transcript--skip-leading-properties-drawer (point-min)))

(defmacro test-mevedel-bounds--announcing (&rest body)
  "Run BODY, returning the body-side changes normalization announced.
Each is `(START END)' of a `mevedel-transcript-source-call-with-change' whose
START is at or after the body start; the leading metadata clear is excluded.
Nothing is printed."
  (declare (indent 0))
  `(let ((announced nil)
         (change (symbol-function 'mevedel-transcript-source-call-with-change)))
     (cl-letf (((symbol-function 'mevedel-transcript-source-call-with-change)
                (lambda (start end function)
                  (when (>= start (mevedel-session-bounds--content-start))
                    (push (list start end) announced))
                  (funcall change start end function))))
       ,@body)
     (nreverse announced)))

(defun test-mevedel-bounds--runs (bounds)
  "Return BOUNDS' runs as ascending `(BEG END)' pairs."
  (sort (cl-loop for (_name . runs) in bounds
                 append (mapcar (lambda (run) (list (nth 0 run) (nth 1 run))) runs))
        (lambda (left right) (< (car left) (car right)))))

(defun test-mevedel-bounds--width (runs origin dirty size origin-after)
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

(defun test-mevedel-bounds--measure-append (controls text prop)
  "Settle a transcript, append TEXT carrying PROP, and settle it again.
CONTROLS selects the fixture.  Return a plist of what the second pass did:
`:announced', `:stats', `:expected' (the width implied by the earliest
change) and `:oracle' (the settled bounds, which the original computes)."
  (test-mevedel-bounds--fill 30 controls)
  (mevedel-session-artifacts-stabilize-gptel-bounds)
  (let* ((before (test-mevedel-bounds--runs (test-mevedel-bounds--oracle)))
         (origin (test-mevedel-bounds--origin))
         (old-end (point-max))
         size announced)
    (setq mevedel-session-bounds--stats nil)
    (setq announced
          (test-mevedel-bounds--announcing
            (goto-char (point-max))
            (test-mevedel-bounds--insert text prop)
            (setq size (point-max))
            (mevedel-session-artifacts-stabilize-gptel-bounds)))
    (list :announced announced
          :stats (copy-sequence mevedel-session-bounds--stats)
          :expected (test-mevedel-bounds--width
                     before origin
                     (apply #'min old-end (mapcar #'car announced))
                     (point-max) (test-mevedel-bounds--origin))
          :size size
          :oracle (test-mevedel-bounds--oracle))))

(mevedel-deftest mevedel-session-bounds-get
  (:vars ((mevedel-session-bounds--stats nil)
         (bounds-advice-installed
          (advice-member-p (function mevedel-session-bounds--advice)
                           (quote gptel--get-buffer-bounds))))
   :after-each
   (unless bounds-advice-installed
     (advice-remove (quote gptel--get-buffer-bounds)
                    (function mevedel-session-bounds--advice))))
  ,test
  (test)
  :doc "the first answer is computed in full and an unchanged buffer is a hit"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--agree)
    (should (= 1 (test-mevedel-bounds--stat :full)))
    (test-mevedel-bounds--agree)
    (should (= 1 (test-mevedel-bounds--stat :hit)))
    ;; A drawer and a first body run without values are no straddling run.
    (should (= 0 (test-mevedel-bounds--stat :fallback))))
  :doc "a tail append rescans only the tail"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--agree)
    (goto-char (point-max))
    (test-mevedel-bounds--insert "tail response\n" 'response)
    (test-mevedel-bounds--agree)
    (should (= 1 (test-mevedel-bounds--stat :incremental)))
    (should (= 1 (test-mevedel-bounds--stat :full))))
  :doc "an insertion and a deletion in the middle are recomputed from the edit"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--agree)
    (goto-char (+ (test-mevedel-bounds--origin) 400))
    (test-mevedel-bounds--insert "middle tool\n" (cons 'tool "middle"))
    (test-mevedel-bounds--agree)
    (delete-region (+ (test-mevedel-bounds--origin) 200)
                   (+ (test-mevedel-bounds--origin) 260))
    (test-mevedel-bounds--agree)
    (should (= 2 (test-mevedel-bounds--stat :incremental)))
    (should (= 1 (test-mevedel-bounds--stat :full))))
  :doc "a property-only change in the middle is observed"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--agree)
    (let ((from (+ (test-mevedel-bounds--origin) 300)))
      (put-text-property from (+ from 90) 'gptel 'ignore))
    (test-mevedel-bounds--agree)
    (let ((from (+ (test-mevedel-bounds--origin) 500)))
      (remove-text-properties from (+ from 70) '(gptel nil)))
    (test-mevedel-bounds--agree)
    (should (= 2 (test-mevedel-bounds--stat :incremental))))
  :doc "equal but non-eq property values stay separate runs, as in the original"
  (test-mevedel-bounds--with-buffer
    (goto-char (point-max))
    (test-mevedel-bounds--insert "first\n" (cons 'tool "same"))
    (test-mevedel-bounds--insert "second\n" (cons 'tool "same"))
    (let ((cached (test-mevedel-bounds--agree)))
      (should (= 2 (length (cl-remove-if-not
                            (lambda (bound) (equal "same" (nth 2 bound)))
                            (alist-get 'tool cached))))))
    ;; Making one object serve both runs merges them in both answers.
    (let ((shared (cons 'tool "same")))
      (put-text-property (- (point-max) 13) (point-max) 'gptel shared))
    (let ((cached (test-mevedel-bounds--agree)))
      (should (= 1 (length (cl-remove-if-not
                            (lambda (bound) (equal "same" (nth 2 bound)))
                            (alist-get 'tool cached)))))))
  :doc "an overlay carrying gptel falls back to the original function"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--agree)
    (let ((overlay (make-overlay (+ (test-mevedel-bounds--origin) 20)
                                 (+ (test-mevedel-bounds--origin) 80))))
      (overlay-put overlay 'gptel 'ignore)
      (test-mevedel-bounds--agree)
      (should (= 1 (test-mevedel-bounds--stat :fallback)))
      (delete-overlay overlay))
    ;; An overlay without it is irrelevant, and the cache recovers.
    (let ((overlay (make-overlay 5 30)))
      (overlay-put overlay 'face 'bold)
      (test-mevedel-bounds--agree)
      (delete-overlay overlay))
    (should (= 1 (test-mevedel-bounds--stat :fallback))))
  :doc "narrowing during the call does not change the answer"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--agree)
    (let ((from (+ (test-mevedel-bounds--origin) 300)))
      (narrow-to-region from (+ from 300))
      (goto-char (point-max))
      (insert "narrowed\n")
      (let ((oracle (test-mevedel-bounds--oracle))
            (cached (test-mevedel-bounds--cached)))
        (should (equal oracle cached)))
      (should (= (point-min) from)))
    (widen)
    (test-mevedel-bounds--agree))
  :doc "known metadata growth moves positions without rescanning the body"
  (test-mevedel-bounds--with-buffer
    (let ((before (test-mevedel-bounds--agree)))
      (should (equal before (test-mevedel-bounds--oracle))))
    (dolist (growth '(1 7 90 1000))
      (mevedel-transcript-source-call-with-metadata-change
       (lambda ()
         (save-excursion
           (goto-char 20)
           (insert (make-string growth ?x)))))
      (test-mevedel-bounds--agree))
    (should (= 4 (test-mevedel-bounds--stat :rebuilt)))
    (should (= 1 (test-mevedel-bounds--stat :full)))
    (should (= 0 (test-mevedel-bounds--stat :incremental)))
    ;; Shrinking is the same.
    (mevedel-transcript-source-call-with-metadata-change
     (lambda () (delete-region 20 120)))
    (test-mevedel-bounds--agree)
    (should (= 5 (test-mevedel-bounds--stat :rebuilt))))
  :doc "a metadata edit that was not announced is a body edit and stays correct"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--agree)
    (save-excursion (goto-char 20) (insert "unannounced"))
    (test-mevedel-bounds--agree)
    (should (= 2 (test-mevedel-bounds--stat :full))))
  :doc "an edit with modification hooks inhibited falls back to a full rescan"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--agree)
    (let ((inhibit-modification-hooks t)
          (from (+ (test-mevedel-bounds--origin) 700)))
      (put-text-property from (+ from 50) 'gptel 'ignore))
    (test-mevedel-bounds--agree)
    (with-silent-modifications
      (let ((from (+ (test-mevedel-bounds--origin) 100)))
        (put-text-property from (+ from 30) 'gptel 'response)))
    (test-mevedel-bounds--agree)
    (should (= 3 (test-mevedel-bounds--stat :full)))
    (should (= 0 (test-mevedel-bounds--stat :incremental))))
  :doc "rewriting the whole buffer, as compaction does, is recomputed"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--agree)
    (erase-buffer)
    (test-mevedel-bounds--fill 5)
    (test-mevedel-bounds--agree)
    (should (= 2 (test-mevedel-bounds--stat :full))))
  :doc "properties in the drawer are part of the answer"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--agree)
    (mevedel-transcript-source-call-with-metadata-change
     (lambda () (put-text-property 3 9 'gptel 'ignore)))
    (let ((cached (test-mevedel-bounds--agree)))
      (should (equal '(3 9) (car (alist-get 'ignore cached)))))
    (should (= 1 (test-mevedel-bounds--stat :full))))
  :doc "a run straddling the body start is answered by the original function"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--agree)
    (let ((origin (test-mevedel-bounds--origin)))
      (mevedel-transcript-source-call-with-metadata-change
       (lambda () (put-text-property (- origin 4) origin 'gptel 'ignore)))
      (put-text-property origin (+ origin 5) 'gptel 'ignore))
    (test-mevedel-bounds--agree)
    (should (= 1 (test-mevedel-bounds--stat :fallback))))
  :doc "empty and property-free buffers answer nil"
  (with-temp-buffer
    (org-mode)
    (should-not (test-mevedel-bounds--agree))
    (insert ":PROPERTIES:\n:END:\nplain\n")
    (should-not (test-mevedel-bounds--agree)))
  :doc "a random edit sequence always matches the original byte for byte"
  (test-mevedel-bounds--with-buffer
    (let ((props (list 'response 'ignore 'mevedel-render-data nil))
          ;; Deterministic, and local: the global random state is not touched.
          (rand (let ((state 20241020))
                  (lambda (limit)
                    (setq state (% (+ (* state 1103515245) 12345) 2147483648))
                    (% (/ state 65536) limit)))))
      (dotimes (step 140)
        (let* ((origin (test-mevedel-bounds--origin))
               (size (max 1 (- (point-max) origin)))
               (position (+ origin (funcall rand size)))
               (width (1+ (funcall rand 80)))
               (prop (if (zerop (funcall rand 4))
                         (cons 'tool (format "id-%d" (funcall rand 6)))
                       (nth (funcall rand (length props)) props))))
          (pcase (funcall rand 9)
            (0 (goto-char (point-max))
               (test-mevedel-bounds--insert (format "tail %d\n" step) prop))
            (1 (goto-char position)
               (test-mevedel-bounds--insert (format "mid %d\n" step) prop))
            (2 (delete-region position (min (point-max) (+ position width))))
            (3 (put-text-property position (min (point-max) (+ position width))
                                  'gptel prop))
            (4 (remove-text-properties
                position (min (point-max) (+ position width)) '(gptel nil)))
            (5 (mevedel-transcript-source-call-with-metadata-change
                (lambda ()
                  (save-excursion
                    (goto-char (+ 12 (funcall rand 8)))
                    (insert (make-string (1+ (funcall rand 30)) ?m))))))
            (6 (with-silent-modifications
                 (put-text-property position (min (point-max) (+ position width))
                                    'gptel prop)))
            (7 (let ((inhibit-modification-hooks t))
                 (goto-char position)
                 (test-mevedel-bounds--insert "quiet\n" prop)))
            (8 (narrow-to-region (min position (1- (point-max))) (point-max))))
          (let ((oracle (test-mevedel-bounds--oracle))
                (cached (test-mevedel-bounds--cached)))
            (should (equal oracle cached)))
          (widen)))
      (should (> (test-mevedel-bounds--stat :incremental) 0))
      (should (> (test-mevedel-bounds--stat :full) 1))))
  :doc "a cloned buffer neither reuses nor invalidates its original's proof"
  (test-mevedel-bounds--with-buffer
    (let* ((first (test-mevedel-bounds--cached))
           (cache mevedel-session-bounds--cache)
           (clone (clone-buffer nil nil)))
      (unwind-protect
          (progn
            ;; The clone inherited the variable and the cache object itself.
            (should (eq cache (buffer-local-value 'mevedel-session-bounds--cache clone)))
            (with-current-buffer clone
              (setq mevedel-session-bounds--stats nil)
              (should (equal (test-mevedel-bounds--oracle) (test-mevedel-bounds--cached)))
              ;; Derived afresh from its own proof, not the inherited one.
              (should (= 1 (test-mevedel-bounds--stat :full)))
              (should-not (eq cache mevedel-session-bounds--cache))
              (should (eq clone (marker-buffer
                                 (mevedel-transcript-source-origin
                                  (mevedel-session-bounds--cache-source
                                   mevedel-session-bounds--cache))))))
            (setq mevedel-session-bounds--stats nil)
            (should (equal first (test-mevedel-bounds--cached)))
            (should (eq cache mevedel-session-bounds--cache))
            (should (= 1 (test-mevedel-bounds--stat :hit))))
        (kill-buffer clone)))))

(mevedel-deftest mevedel-session-bounds-call-with-cache
  (:vars ((mevedel-session-bounds--stats nil)
         (bounds-advice-installed
          (advice-member-p (function mevedel-session-bounds--advice)
                           (quote gptel--get-buffer-bounds))))
   :after-each
   (unless bounds-advice-installed
     (advice-remove (quote gptel--get-buffer-bounds)
                    (function mevedel-session-bounds--advice))))
  ,test
  (test)
  :doc "only the marked buffer is served from the cache"
  (test-mevedel-bounds--with-buffer
    (let ((other (generate-new-buffer " *bounds-other*")))
      (unwind-protect
          (progn
            (mevedel-session-bounds-install)
            (test-mevedel-bounds--oracle)
            (should (= 0 (test-mevedel-bounds--stat :full)))
            (mevedel-session-bounds-call-with-cache
             (lambda ()
               (with-current-buffer other
                 (insert "unmarked")
                 (put-text-property 1 5 'gptel 'response)
                 (should (equal '((response (1 5)))
                                (gptel--get-buffer-bounds))))))
            (should (= 0 (test-mevedel-bounds--stat :full)))
            (test-mevedel-bounds--cached)
            (should (= 1 (test-mevedel-bounds--stat :full))))
        (kill-buffer other))))
  :doc "installing twice leaves one advice"
  (progn
    (mevedel-session-bounds-install)
    (mevedel-session-bounds-install)
    (let ((count 0))
      (advice-mapc (lambda (function _props)
                     (when (eq function #'mevedel-session-bounds--advice)
                       (cl-incf count)))
                   'gptel--get-buffer-bounds)
      (should (= 1 count)))))

(mevedel-deftest mevedel-session-bounds--count ()
  ,test
  (test)
  :doc "counts each kind of answer separately"
  (let ((mevedel-session-bounds--stats nil))
    (mevedel-session-bounds--count :hit)
    (mevedel-session-bounds--count :hit)
    (mevedel-session-bounds--count :full)
    (should (equal '(2 1) (list (plist-get mevedel-session-bounds--stats :hit)
                                (plist-get mevedel-session-bounds--stats :full))))))

(mevedel-deftest mevedel-session-bounds--overlay-gptel-p ()
  ,test
  (test)
  :doc "only an overlay carrying gptel counts"
  (test-mevedel-bounds--with-buffer
    (should-not (mevedel-session-bounds--overlay-gptel-p))
    (let ((other (make-overlay 5 20)))
      (overlay-put other 'face 'bold)
      (should-not (mevedel-session-bounds--overlay-gptel-p))
      (delete-overlay other))
    (let ((marked (make-overlay 5 20)))
      (overlay-put marked 'gptel 'ignore)
      (should (mevedel-session-bounds--overlay-gptel-p))
      (delete-overlay marked))))

(mevedel-deftest mevedel-session-bounds--scan ()
  ,test
  (test)
  :doc "returns ascending non-nil runs, split on eq and clipped to the range"
  (with-temp-buffer
    (insert "aaaabbbbccccdddd")
    (put-text-property 1 5 'gptel 'response)
    (put-text-property 9 13 'gptel (cons 'tool "x"))
    (put-text-property 13 17 'gptel (cons 'tool "x"))
    (should (equal '((1 5 response) (9 13 (tool . "x")) (13 17 (tool . "x")))
                   (mevedel-session-bounds--scan 1 17)))
    (should (equal '((9 13 (tool . "x")))
                   (mevedel-session-bounds--scan 9 13)))
    (should (equal '((3 5 response))
                   (mevedel-session-bounds--scan 3 7)))
    (should-not (mevedel-session-bounds--scan 5 9))
    (should-not (mevedel-session-bounds--scan 7 7))))

(mevedel-deftest mevedel-session-bounds--content-start ()
  ,test
  (test)
  :doc "is the position after the leading drawer, or the buffer start"
  (test-mevedel-bounds--with-buffer
    (should (= (test-mevedel-bounds--origin)
               (mevedel-session-bounds--content-start)))
    (should (> (mevedel-session-bounds--content-start) (point-min))))
  :doc "a buffer without a drawer starts at its first position"
  (with-temp-buffer
    (insert "plain text\n")
    (should (= (point-min) (mevedel-session-bounds--content-start)))))

(mevedel-deftest mevedel-session-bounds--reset ()
  ,test
  (test)
  :doc "drops the cache and releases its proof"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--cached)
    (should mevedel-session-bounds--cache)
    (should mevedel-transcript-source--consumers)
    (let ((marker (mevedel-transcript-source-origin
                   (mevedel-session-bounds--cache-source
                    mevedel-session-bounds--cache))))
      (mevedel-session-bounds--reset)
      (should-not mevedel-session-bounds--cache)
      (should-not mevedel-transcript-source--consumers)
      (should-not (marker-buffer marker)))
    ;; Resetting nothing is harmless, and the next answer is derived afresh.
    (mevedel-session-bounds--reset)
    (test-mevedel-bounds--agree)
    (should (= 2 (test-mevedel-bounds--stat :full))))
  :doc "resetting in a clone leaves its original's proof registered"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--cached)
    (let* ((original (current-buffer))
           (source (mevedel-session-bounds--cache-source
                    mevedel-session-bounds--cache))
           (clone (clone-buffer nil nil)))
      (unwind-protect
          (progn
            (with-current-buffer clone
              ;; The inherited cache holds the original's proof.
              (mevedel-session-bounds--reset)
              (should-not mevedel-session-bounds--cache))
            (should (eq original (marker-buffer
                                  (mevedel-transcript-source-origin source))))
            (should (memq source (buffer-local-value
                                  'mevedel-transcript-source--consumers original)))
            (should mevedel-session-bounds--cache))
        (kill-buffer clone)))))

(mevedel-deftest mevedel-session-bounds--absolute ()
  ,test
  (test)
  :doc "turns body relative runs into absolute ones from a list or a vector"
  (should (equal '((11 15 response) (20 30 ignore))
                 (mevedel-session-bounds--absolute '((1 5 response) (10 20 ignore)) 10)))
  (should (equal '((11 15 response))
                 (mevedel-session-bounds--absolute (vector '(1 5 response)) 10)))
  (should-not (mevedel-session-bounds--absolute [] 10)))

(mevedel-deftest mevedel-session-bounds--refresh ()
  ,test
  (test)
  :doc "an unchanged body returns the very same result object"
  (test-mevedel-bounds--with-buffer
    (let ((first (test-mevedel-bounds--cached)))
      (should (eq first (test-mevedel-bounds--cached)))
      (should (= 1 (test-mevedel-bounds--stat :hit)))))
  :doc "a moved origin rebuilds the alist from unchanged runs"
  (test-mevedel-bounds--with-buffer
    (let ((first (test-mevedel-bounds--cached)))
      (mevedel-transcript-source-call-with-metadata-change
       (lambda () (save-excursion (goto-char 20) (insert "grown"))))
      (let ((second (test-mevedel-bounds--cached)))
        (should-not (eq first second))
        (should (equal second (test-mevedel-bounds--oracle)))
        (should (= 1 (test-mevedel-bounds--stat :rebuilt)))
        (should (= 1 (test-mevedel-bounds--stat :full))))))
  :doc "a silent normalization announced for a body suffix is incremental"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--cached)
    (let ((from (+ (test-mevedel-bounds--origin) 500)))
      (mevedel-transcript-source-call-with-change
       from (point-max)
       (lambda ()
         (with-silent-modifications
           (put-text-property from (+ from 40) 'gptel 'ignore)))))
    (test-mevedel-bounds--agree)
    (should (= 1 (test-mevedel-bounds--stat :incremental)))
    (should (= 1 (test-mevedel-bounds--stat :full))))
  :doc "an announced silent change from before the body is a full rescan"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--cached)
    (mevedel-transcript-source-call-with-change
     (point-min) (point-max)
     (lambda ()
       (with-silent-modifications
         (put-text-property (+ (test-mevedel-bounds--origin) 100)
                            (+ (test-mevedel-bounds--origin) 130) 'gptel 'ignore))))
    (test-mevedel-bounds--agree)
    (should (= 2 (test-mevedel-bounds--stat :full))))
  :doc "an announced drawer-only normalization keeps the runs"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--cached)
    (let ((origin (test-mevedel-bounds--origin)))
      (mevedel-transcript-source-call-with-metadata-change
       (lambda ()
         (mevedel-transcript-source-call-with-change
          (point-min) origin
          (lambda ()
            (with-silent-modifications
              (remove-text-properties (point-min) origin '(gptel nil))))))))
    (test-mevedel-bounds--agree)
    (should (= 1 (test-mevedel-bounds--stat :full)))
    (should (> (+ (test-mevedel-bounds--stat :hit)
                  (test-mevedel-bounds--stat :rebuilt))
               0)))
  :doc "a body edit is incremental and records the new baseline"
  (test-mevedel-bounds--with-buffer
    (test-mevedel-bounds--cached)
    (goto-char (point-max))
    (test-mevedel-bounds--insert "late\n" 'response)
    (test-mevedel-bounds--agree)
    (should (= 1 (test-mevedel-bounds--stat :incremental)))
    (test-mevedel-bounds--agree)
    (should (= 1 (test-mevedel-bounds--stat :hit)))))

(mevedel-deftest mevedel-session-bounds--advice ()
  ,test
  (test)
  :doc "an unmarked buffer reaches the original with its arguments"
  (let ((seen nil))
    (let ((mevedel-session-bounds--active-buffer nil))
      (should (eq 'original
                  (mevedel-session-bounds--advice
                   (lambda (&rest arguments) (setq seen arguments) 'original)
                   1 2))))
    (should (equal '(1 2) seen)))
  :doc "the marked buffer is answered by the cache, using the original on fallback"
  (test-mevedel-bounds--with-buffer
    (let ((mevedel-session-bounds--active-buffer (current-buffer)))
      (should (equal (test-mevedel-bounds--oracle)
                     (mevedel-session-bounds--advice #'gptel--get-buffer-bounds))))
    (should (= 1 (test-mevedel-bounds--stat :full)))))

(mevedel-deftest mevedel-session-bounds--retained ()
  ,test
  (test)
  :doc "counts the runs that end strictly before the first dirty offset"
  (let ((runs (vector '(0 5 a) '(8 12 b) '(12 20 c) '(30 40 d))))
    (should (= 0 (mevedel-session-bounds--retained runs 0)))
    (should (= 0 (mevedel-session-bounds--retained runs 5)))
    (should (= 1 (mevedel-session-bounds--retained runs 6)))
    (should (= 2 (mevedel-session-bounds--retained runs 13)))
    (should (= 2 (mevedel-session-bounds--retained runs 20)))
    (should (= 4 (mevedel-session-bounds--retained runs 41))))
  :doc "an empty run list retains nothing"
  (should (= 0 (mevedel-session-bounds--retained [] 10))))

(mevedel-deftest mevedel-session-bounds--alist ()
  ,test
  (test)
  :doc "orders keys and runs as the original backward scan does"
  (let ((runs (list (list 1 4 'response) (list 4 9 (cons 'tool "a"))
                    (list 9 12 'response) (list 12 15 (cons 'tool "b"))
                    (list 15 20 'ignore))))
    (should (equal '((response (1 4) (9 12))
                     (tool (4 9 "a") (12 15 "b"))
                     (ignore (15 20)))
                   (mevedel-session-bounds--alist runs))))
  :doc "a tool run without an identifier keeps its two element form"
  (should (equal '((tool (1 3)))
                 (mevedel-session-bounds--alist
                  (list (list 1 3 (cons 'tool nil)))))))

(mevedel-deftest mevedel-session-artifacts-stabilize-gptel-bounds
  (:vars ((mevedel-session-bounds--stats nil)
         (bounds-advice-installed
          (advice-member-p (function mevedel-session-bounds--advice)
                           (quote gptel--get-buffer-bounds))))
   :after-each
   (unless bounds-advice-installed
     (advice-remove (quote gptel--get-buffer-bounds)
                    (function mevedel-session-bounds--advice))))
  ,test
  (test)
  :doc "the settled GPTEL_BOUNDS and buffer equal the original loop's result"
  (let (reference)
    (with-temp-buffer
      (let ((org-agenda-file-menu-enabled nil)) (org-mode))
      (test-mevedel-bounds--fill 60)
      (let ((copy (current-buffer)))
        ;; The original loop, on the unadvised function, in a copy.
        (with-temp-buffer
          (let ((org-agenda-file-menu-enabled nil)) (org-mode))
          (insert-buffer-substring copy)
          (let ((last nil) (done nil) (attempts 0) (print-length nil) (print-level nil))
            (mevedel-transcript-normalize-properties)
            (while (and (not done) (< attempts 8))
              (setq attempts (1+ attempts))
              (let ((serialized (when-let* ((bounds (gptel--get-buffer-bounds)))
                                  (prin1-to-string bounds))))
                (cond
                 ((null serialized)
                  (mevedel-session-artifacts-property-delete-direct "GPTEL_BOUNDS")
                  (setq done t))
                 ((equal serialized last) (setq done t))
                 (t (setq last serialized)
                    (mevedel-session-artifacts--property-put-direct
                     "GPTEL_BOUNDS" serialized))))))
          (setq reference (buffer-string)))
        (setq mevedel-session-bounds--stats nil)
        (mevedel-session-artifacts-stabilize-gptel-bounds)
        (should (equal-including-properties reference (buffer-string)))
        ;; Stabilizing again, with the drawer already settled, changes nothing.
        (mevedel-session-artifacts-stabilize-gptel-bounds)
        (should (equal-including-properties reference (buffer-string)))
        ;; Normalization writes properties with hooks inhibited, so the
        ;; proof rescans until it records its own silent changes.
        (should (> (+ (test-mevedel-bounds--stat :hit)
                      (test-mevedel-bounds--stat :rebuilt)
                      (test-mevedel-bounds--stat :incremental)
                      (test-mevedel-bounds--stat :full))
                   0)))))
  :doc "a canonical tail append rescans exactly from the earliest announced change"
  (with-temp-buffer
    (let ((org-agenda-file-menu-enabled nil)) (org-mode))
    (let* ((seen (test-mevedel-bounds--measure-append
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
                 (/ (- (point-max) (test-mevedel-bounds--origin)) 2)))
      (should (equal (plist-get seen :oracle) (test-mevedel-bounds--cached)))
      (let ((incremental (buffer-string)))
        (mevedel-session-bounds--reset)
        (mevedel-session-artifacts-stabilize-gptel-bounds)
        (should (equal-including-properties incremental (buffer-string))))))
  :doc "an orphan tail append announces no body change and rescans only the tail"
  (with-temp-buffer
    (let ((org-agenda-file-menu-enabled nil)) (org-mode))
    (let* ((seen (test-mevedel-bounds--measure-append
                  nil "appended response\n" 'response))
           (stats (plist-get seen :stats)))
      ;; The orphans leave nothing to rewrite, so only the append is dirty.
      (should-not (plist-get seen :announced))
      (should (= 0 (or (plist-get stats :full) 0)))
      (should (= 1 (plist-get stats :incremental)))
      (should (= (plist-get seen :expected) (plist-get stats :scanned)))
      (should (equal (plist-get seen :oracle) (test-mevedel-bounds--cached)))
      (let ((incremental (buffer-string)))
        (mevedel-session-bounds--reset)
        (mevedel-session-artifacts-stabilize-gptel-bounds)
        (should (equal-including-properties incremental (buffer-string))))))
  :doc "a closed control after orphans announces from the earliest orphan and rescans fully"
  (with-temp-buffer
    (let ((org-agenda-file-menu-enabled nil)) (org-mode))
    (test-mevedel-bounds--fill 30)
    (mevedel-session-artifacts-stabilize-gptel-bounds)
    (let* ((origin (test-mevedel-bounds--origin))
           (orphan (car (alist-get 'tool (test-mevedel-bounds--oracle))))
           (announced nil)
           size)
      (setq mevedel-session-bounds--stats nil)
      (setq announced
            (test-mevedel-bounds--announcing
              (goto-char (point-max))
              (test-mevedel-bounds--insert
               "#+begin_reasoning\nlater\n#+end_reasoning\n" 'ignore)
              (setq size (point-max))
              (mevedel-session-artifacts-stabilize-gptel-bounds)))
      ;; Normalization resumes at the earliest orphan's line, before the
      ;; control that gave it something to write.
      (should (equal (list (car orphan)) (mapcar #'car announced)))
      (should (= size (cadr (car announced))))
      ;; No run ends before that offset, so the whole body is rescanned once.
      (should (= 1 (plist-get mevedel-session-bounds--stats :full)))
      (should (= 0 (or (plist-get mevedel-session-bounds--stats :incremental) 0)))
      (should (= (- size origin) (plist-get mevedel-session-bounds--stats :scanned)))
      (should (equal (test-mevedel-bounds--oracle) (test-mevedel-bounds--cached)))
      (let ((full (buffer-string)))
        (mevedel-session-bounds--reset)
        (mevedel-session-artifacts-stabilize-gptel-bounds)
        (should (equal-including-properties full (buffer-string))))))
  :doc "stabilizing an unchanged transcript never rescans the body"
  (with-temp-buffer
    (let ((org-agenda-file-menu-enabled nil)) (org-mode))
    (test-mevedel-bounds--fill 30 t)
    (mevedel-session-artifacts-stabilize-gptel-bounds)
    (setq mevedel-session-bounds--stats nil)
    (mevedel-session-artifacts-stabilize-gptel-bounds)
    (should (= 0 (test-mevedel-bounds--stat :full)))
    (should (= 0 (test-mevedel-bounds--stat :incremental)))
    (should (> (+ (test-mevedel-bounds--stat :hit)
                  (test-mevedel-bounds--stat :rebuilt))
               0))))

(provide 'test-mevedel-session-bounds)

;;; test-mevedel-session-bounds.el ends here
