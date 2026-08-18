;;; test-mevedel-buddy-note.el --- Tests for buddy notes and tools -*- lexical-binding: t -*-

;;; Commentary:

;; Seam 2: tool calls in, note overlays out.  No model.

;;; Code:

(require 'mevedel-buddy-note)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defvar mevedel-test--note-buffers nil
  "Buffers created by the note tests, killed in teardown.")

(defun mevedel-test--note-buffer (name content)
  "Return a new buffer NAME holding CONTENT, in scope for notes."
  (let ((buf (generate-new-buffer name)))
    (push buf mevedel-test--note-buffers)
    (with-current-buffer buf (insert content))
    (push name mevedel-buddy-note--scope-buffers)
    buf))

(defun mevedel-test--note-cleanup ()
  "Kill note test buffers and discard every note."
  (mevedel-buddy-note-clear-all)
  (setq mevedel-buddy-note--scope-buffers nil)
  (dolist (buf mevedel-test--note-buffers)
    (when (buffer-live-p buf) (kill-buffer buf)))
  (setq mevedel-test--note-buffers nil))

(defun mevedel-test--note-overlays (buffer)
  "Return BUFFER's note overlays."
  (with-current-buffer buffer
    (seq-filter (lambda (ov) (overlay-get ov 'mevedel-buddy-note))
                (overlays-in (point-min) (point-max)))))

(defun mevedel-test--note-text (buffer)
  "Return the concatenated note text visible in BUFFER."
  (mapconcat (lambda (ov) (or (overlay-get ov 'after-string) ""))
             (mevedel-test--note-overlays buffer)
             "\n"))

(defun mevedel-test--add-note (buffer line note &optional severity)
  "Add NOTE at LINE of BUFFER with SEVERITY and return its id."
  (mevedel-buddy-note-add (buffer-name buffer) line note
                          (or severity "significant")))


;;
;;; Adding, updating, removing

(mevedel-deftest mevedel-buddy-note-add
  (:after-each (mevedel-test--note-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy-note-add' puts an overlay on the named line"
  (let* ((buf (mevedel-test--note-buffer "note-add" "one\ntwo\nthree\n"))
         (id (mevedel-test--add-note buf 2 "look here")))
    (should (integerp id))
    (should (= 1 (length (mevedel-test--note-overlays buf))))
    (should (string-match-p "look here" (mevedel-test--note-text buf)))
    (with-current-buffer buf
      (should (= 2 (line-number-at-pos
                    (overlay-start
                     (car (mevedel-test--note-overlays buf))))))))

  :doc "`mevedel-buddy-note-add' assigns distinct ids"
  (let* ((buf (mevedel-test--note-buffer "note-ids" "one\ntwo\n"))
         (first (mevedel-test--add-note buf 1 "first"))
         (second (mevedel-test--add-note buf 2 "second")))
    (should-not (= first second)))

  :doc "`mevedel-buddy-note-add' refuses a buffer outside the scope"
  (let ((buf (mevedel-test--note-buffer "note-scope" "one\n")))
    (let ((mevedel-buddy-note--scope-buffers (list "other-buffer")))
      (should (string-match-p
               "not in the review scope"
               (mevedel-buddy-note-add (buffer-name buf) 1 "nope" "trivial")))
      (should (null (mevedel-test--note-overlays buf)))))

  :doc "`mevedel-buddy-note-add' refuses everything when no review is running"
  (let ((buf (mevedel-test--note-buffer "note-noscope" "one\n")))
    (let ((mevedel-buddy-note--scope-buffers nil))
      (should (string-match-p
               "not in the review scope"
               (mevedel-buddy-note-add (buffer-name buf) 1 "nope" "trivial")))
      (should (null (mevedel-test--note-overlays buf)))))

  :doc "`mevedel-buddy-note-add' clamps a line past the end of the buffer"
  (let* ((buf (mevedel-test--note-buffer "note-clamp" "one\ntwo\n"))
         (id (mevedel-test--add-note buf 99 "past the end")))
    (should (integerp id))
    (should (= 1 (length (mevedel-test--note-overlays buf))))))

(mevedel-deftest mevedel-buddy-note-update
  (:after-each (mevedel-test--note-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy-note-update' replaces the text and keeps the line"
  (let* ((buf (mevedel-test--note-buffer "note-update" "one\ntwo\nthree\n"))
         (id (mevedel-test--add-note buf 2 "first wording"))
         (line (with-current-buffer buf
                 (line-number-at-pos
                  (overlay-start (car (mevedel-test--note-overlays buf)))))))
    (mevedel-buddy-note-update id "second wording")
    (should (string-match-p "second wording" (mevedel-test--note-text buf)))
    (should-not (string-match-p "first wording" (mevedel-test--note-text buf)))
    (with-current-buffer buf
      (should (= line (line-number-at-pos
                       (overlay-start
                        (car (mevedel-test--note-overlays buf))))))))

  :doc "`mevedel-buddy-note-update' reports an unknown id without signalling"
  (should (string-match-p "Unknown note" (mevedel-buddy-note-update 4242 "x"))))

(mevedel-deftest mevedel-buddy-note-remove
  (:after-each (mevedel-test--note-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy-note-remove' deletes the overlay"
  (let* ((buf (mevedel-test--note-buffer "note-remove" "one\ntwo\n"))
         (id (mevedel-test--add-note buf 1 "gone soon")))
    (mevedel-buddy-note-remove id)
    (should (null (mevedel-test--note-overlays buf))))

  :doc "`mevedel-buddy-note-remove' reports an unknown id without signalling"
  (should (string-match-p "Unknown note" (mevedel-buddy-note-remove 4242))))


;;
;;; Severity

(mevedel-deftest mevedel-buddy-note--face
  (:doc "`mevedel-buddy-note--face' maps each severity to its own face")
  (should (equal ,face (mevedel-buddy-note--face ,severity)))
  (severity face)
  "trivial"     'mevedel-buddy-note-trivial-face
  "significant" 'mevedel-buddy-note-significant-face
  "critical"    'mevedel-buddy-note-critical-face
  "nonsense"    'mevedel-buddy-note-significant-face)


;;
;;; Lifecycle

(mevedel-deftest mevedel-buddy-note--lifecycle
  (:after-each (mevedel-test--note-cleanup))
  ,test
  (test)

  :doc "a note follows its text when lines are inserted above it"
  (let* ((buf (mevedel-test--note-buffer "note-drift" "one\ntwo\nthree\n"))
         (_id (mevedel-test--add-note buf 3 "about three")))
    (with-current-buffer buf
      (goto-char (point-min))
      (insert "zero\n")
      (should (= 4 (line-number-at-pos
                    (overlay-start
                     (car (mevedel-test--note-overlays buf))))))))

  :doc "editing an annotated line marks the note for review"
  (let* ((buf (mevedel-test--note-buffer "note-review" "one\ntwo\nthree\n"))
         (id (mevedel-test--add-note buf 2 "about two")))
    (should-not (plist-get (mevedel-buddy-note--find id) :review-needed))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line 1)
      (end-of-line)
      (insert " changed"))
    (should (plist-get (mevedel-buddy-note--find id) :review-needed)))

  :doc "dismissing a note deletes its overlay and keeps the record"
  (let* ((buf (mevedel-test--note-buffer "note-dismiss" "one\ntwo\n"))
         (id (mevedel-test--add-note buf 1 "dismiss me")))
    (mevedel-buddy-note-dismiss id "user")
    (should (null (mevedel-test--note-overlays buf)))
    (should (eq 'dismissed (plist-get (mevedel-buddy-note--find id) :status))))

  :doc "killing a buffer drops its notes and leaves no live overlay"
  (let* ((buf (mevedel-test--note-buffer "note-kill" "one\ntwo\n"))
         (id (mevedel-test--add-note buf 1 "doomed")))
    (kill-buffer buf)
    (should (null (mevedel-buddy-note--find id)))))


;;
;;; Serialization for the prompt

(mevedel-deftest mevedel-buddy-note-serialize
  (:after-each (mevedel-test--note-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy-note-serialize' returns empty with no notes"
  (should (string-empty-p (mevedel-buddy-note-serialize)))

  :doc "`mevedel-buddy-note-serialize' lists active notes with id and line"
  (let* ((buf (mevedel-test--note-buffer "note-ser" "one\ntwo\n"))
         (id (mevedel-test--add-note buf 2 "still true")))
    (let ((text (mevedel-buddy-note-serialize)))
      (should (string-match-p (format "note_id %d" id) text))
      (should (string-match-p "note-ser:2" text))
      (should (string-match-p "still true" text))))

  :doc "`mevedel-buddy-note-serialize' keeps dismissed notes, labelled"
  (let* ((buf (mevedel-test--note-buffer "note-ser-dismissed" "one\n"))
         (id (mevedel-test--add-note buf 1 "rejected advice")))
    (mevedel-buddy-note-dismiss id "user")
    (let ((text (mevedel-buddy-note-serialize)))
      (should (string-match-p "dismissed" text))
      (should (string-match-p "rejected advice" text))))

  :doc "`mevedel-buddy-note-serialize' marks a note needing review"
  (let* ((buf (mevedel-test--note-buffer "note-ser-review" "one\ntwo\n"))
         (_id (mevedel-test--add-note buf 2 "check me")))
    (with-current-buffer buf
      (goto-char (point-max))
      (forward-line -1)
      (end-of-line)
      (insert " edited"))
    (should (string-match-p "changed since" (mevedel-buddy-note-serialize))))

  :doc "`mevedel-buddy-note-serialize' caps how many notes it sends"
  (let ((buf (mevedel-test--note-buffer "note-cap" "one\n"))
        (mevedel-buddy-note-serialize-limit 3))
    (dotimes (index 10)
      (mevedel-buddy-note-dismiss
       (mevedel-test--add-note buf 1 (format "note number %d" index))
       "user"))
    (let ((text (mevedel-buddy-note-serialize)))
      (should (= 3 (length (seq-filter (lambda (line)
                                         (string-prefix-p "- note_id" line))
                                       (split-string text "\n")))))
      (should (string-match-p "note number 9" text))
      (should-not (string-match-p "note number 0" text)))))


;;
;;; Instruction isolation

(mevedel-deftest mevedel-buddy-note--not-an-instruction
  (:after-each (mevedel-test--note-cleanup)
   :doc "note overlays are invisible to instruction enumeration")
  (let ((buf (mevedel-test--note-buffer "note-instr" "one\ntwo\n")))
    (mevedel-test--add-note buf 1 "not an instruction")
    (with-current-buffer buf
      (should (null (seq-filter
                     (lambda (ov) (overlay-get ov 'mevedel-instruction))
                     (overlays-in (point-min) (point-max))))))))

;;
;;; Commands

(mevedel-deftest mevedel-buddy-dismiss-note
  (:after-each (mevedel-test--note-cleanup) :quiet t)
  ,test
  (test)

  :doc "`mevedel-buddy-dismiss-note' dismisses the note under point"
  (let* ((buf (mevedel-test--note-buffer "cmd-one" "one\ntwo\n"))
         (id (mevedel-test--add-note buf 1 "dismiss me")))
    (with-current-buffer buf
      (goto-char (point-min))
      (mevedel-buddy-dismiss-note))
    (should (eq 'dismissed (plist-get (mevedel-buddy-note--find id) :status))))

  :doc "`mevedel-buddy-dismiss-note' errors when point has no note"
  (let ((buf (mevedel-test--note-buffer "cmd-none" "one\n")))
    (with-current-buffer buf
      (should-error (mevedel-buddy-dismiss-note) :type 'user-error))))

(mevedel-deftest mevedel-buddy-dismiss-notes
  (:after-each (mevedel-test--note-cleanup) :quiet t
   :doc "`mevedel-buddy-dismiss-notes' clears this buffer's notes only")
  (let* ((here (mevedel-test--note-buffer "cmd-here" "one\ntwo\n"))
         (elsewhere (mevedel-test--note-buffer "cmd-elsewhere" "one\n"))
         (kept (mevedel-test--add-note elsewhere 1 "stays")))
    (mevedel-test--add-note here 1 "goes")
    (mevedel-test--add-note here 2 "also goes")
    (with-current-buffer here (mevedel-buddy-dismiss-notes))
    (should (null (mevedel-test--note-overlays here)))
    (should (eq 'active (plist-get (mevedel-buddy-note--find kept) :status)))))

;;
;;; Scope isolation

(mevedel-deftest mevedel-buddy-note--in-scope-p
  (:after-each (mevedel-test--note-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy-note-update' refuses a note outside the scope"
  (let* ((buf (mevedel-test--note-buffer "scope-update" "one\n"))
         (id (mevedel-test--add-note buf 1 "original")))
    (let ((mevedel-buddy-note--scope-buffers (list "elsewhere")))
      (should (string-match-p "not in the review scope"
                              (mevedel-buddy-note-update id "rewritten"))))
    (should (equal "original" (plist-get (mevedel-buddy-note--find id) :note))))

  :doc "`mevedel-buddy-note-remove' refuses a note outside the scope"
  (let* ((buf (mevedel-test--note-buffer "scope-remove" "one\n"))
         (id (mevedel-test--add-note buf 1 "keep me")))
    (let ((mevedel-buddy-note--scope-buffers (list "elsewhere")))
      (should (string-match-p "not in the review scope"
                              (mevedel-buddy-note-remove id))))
    (should (mevedel-buddy-note--find id)))

  :doc "`mevedel-buddy-note-serialize' describes only in-scope notes"
  (let ((here (mevedel-test--note-buffer "scope-here" "one\n"))
        (elsewhere (mevedel-test--note-buffer "scope-elsewhere" "one\n")))
    (mevedel-test--add-note here 1 "mine")
    (mevedel-test--add-note elsewhere 1 "other project")
    (let* ((mevedel-buddy-note--scope-buffers (list "scope-here"))
           (text (mevedel-buddy-note-serialize)))
      (should (string-match-p "mine" text))
      (should-not (string-match-p "other project" text))))

  :doc "`mevedel-buddy-note-read-buffer' refuses a buffer outside the scope"
  (let ((buf (mevedel-test--note-buffer "scope-read" "one\n")))
    (let ((mevedel-buddy-note--scope-buffers (list "elsewhere")))
      (should (string-match-p
               "not in the review scope"
               (mevedel-buddy-note-read-buffer (buffer-name buf)))))))


;;
;;; Markers

(mevedel-deftest mevedel-buddy-note-capture-markers
  (:after-each (mevedel-test--note-cleanup))
  ,test
  (test)

  :doc "a note lands on its original text after lines are inserted above"
  (let ((buf (mevedel-test--note-buffer "marker-drift" "one\ntwo\nthree\n")))
    ;; The review is assembled and its markers taken, then the user types
    ;; while the request is in flight, and only then does the note arrive.
    (mevedel-buddy-note-capture-markers (list (cons "marker-drift" '(1 2 3))))
    (with-current-buffer buf
      (goto-char (point-min))
      (insert "zero\n"))
    (mevedel-test--add-note buf 3 "about three")
    (with-current-buffer buf
      (should (equal "three"
                     (buffer-substring-no-properties
                      (overlay-start (car (mevedel-test--note-overlays buf)))
                      (overlay-end (car (mevedel-test--note-overlays buf))))))))

  :doc "`mevedel-buddy-note-capture-markers' marks only the lines it is given"
  (let ((_buf (mevedel-test--note-buffer "marker-few" "one\ntwo\nthree\n")))
    (mevedel-buddy-note-capture-markers (list (cons "marker-few" '(2))))
    (should (= 1 (length (cdr (assoc "marker-few"
                                     mevedel-buddy-note--markers))))))

  :doc "`mevedel-buddy-note-release-markers' unsets every marker"
  (let ((_buf (mevedel-test--note-buffer "marker-release" "one\ntwo\n")))
    (mevedel-buddy-note-capture-markers (list (cons "marker-release" '(1 2))))
    (let ((markers (mapcar #'cdr
                           (cdr (assoc "marker-release"
                                       mevedel-buddy-note--markers)))))
      (mevedel-buddy-note-release-markers)
      (should (null mevedel-buddy-note--markers))
      (should (seq-every-p (lambda (m) (null (marker-buffer m))) markers)))))

(provide 'test-mevedel-buddy-note)
;;; test-mevedel-buddy-note.el ends here
