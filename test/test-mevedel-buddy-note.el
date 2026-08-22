;;; test-mevedel-buddy-note.el --- Tests for buddy notes and tools -*- lexical-binding: t -*-

;;; Commentary:

;; Seam 2: tool calls in, note overlays out.  No model.

;;; Code:

(require 'mevedel-buddy-note)
(require 'gptel)
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
    (push (cons name buf) mevedel-buddy-note--scope-buffers)
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
  (mevedel-buddy-note-capture-markers
   (list (cons (buffer-name buffer) (list line))))
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
    (let ((mevedel-buddy-note--scope-buffers
           (list (cons "other-buffer" (current-buffer)))))
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

  :doc "`mevedel-buddy-note-add' refuses a line the review was not shown"
  (let ((buf (mevedel-test--note-buffer "note-unshown" "one\ntwo\nthree\n")))
    (mevedel-buddy-note-capture-markers '(("note-unshown" . (2))))
    (with-current-buffer buf
      (goto-char (point-min))
      (insert "zero\n"))
    (should (string-match-p
             "not shown"
             (mevedel-buddy-note-add
              (buffer-name buf) 1 "wrong line" "significant")))
    (should-not (mevedel-test--note-overlays buf)))

  :doc "`mevedel-buddy-note-add' refuses nonpositive and overlarge lines"
  (let ((buf (mevedel-test--note-buffer "note-range" "one\ntwo\n")))
    (mevedel-buddy-note-capture-markers '(("note-range" . (1 2))))
    (dolist (line '(0 -1 99))
      (should (string-match-p
               "not shown"
               (mevedel-buddy-note-add
                (buffer-name buf) line "wrong line" "significant"))))
    (should-not (mevedel-test--note-overlays buf)))

  :doc "`mevedel-buddy-note-add' accepts a line number sent as a string"
  ;; The buddy workload commonly resolves to a local model, and those send
  ;; a JSON number as a string often enough that refusing one would refuse
  ;; real notes.
  (let ((buf (mevedel-test--note-buffer "note-stringy" "one\ntwo\n")))
    (mevedel-buddy-note-capture-markers '(("note-stringy" . (2))))
    (let ((id (mevedel-buddy-note-add
               (buffer-name buf) "2" "about two" "significant")))
      (should (integerp id))
      (should (= 2 (plist-get (mevedel-buddy-note--find id) :line)))
      (with-current-buffer buf
        (should (= 2 (line-number-at-pos
                      (overlay-start
                       (car (mevedel-test--note-overlays buf)))))))))

  :doc "`mevedel-buddy-note-add' refuses a released marker"
  (let ((buf (mevedel-test--note-buffer "note-released" "one\n")))
    (mevedel-buddy-note-capture-markers '(("note-released" . (1))))
    (mevedel-buddy-note-release-markers)
    (should (string-match-p
             "not shown"
             (mevedel-buddy-note-add
              (buffer-name buf) 1 "too late" "significant")))
    (should-not (mevedel-test--note-overlays buf)))

  :doc "`mevedel-buddy-note-add' rejects a captured buffer name reused elsewhere"
  (let ((original (mevedel-test--note-buffer "note-reused" "original\n")))
    (mevedel-buddy-note-capture-markers '(("note-reused" . (1))))
    (with-current-buffer original
      (rename-buffer "note-renamed"))
    (let ((replacement (mevedel-test--note-buffer "note-reused" "replacement\n")))
      (should (string-match-p
               "not shown"
               (mevedel-buddy-note-add
                (buffer-name replacement) 1 "wrong buffer" "significant")))
      (should-not (mevedel-test--note-overlays replacement)))))

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

  :doc "`mevedel-buddy-note-update' measures in the note's own buffer"
  ;; A tool call runs in the buffer the review started from, which under a
  ;; workspace-wide scope is often not the one holding the note.  Measuring
  ;; the line in whatever buffer happens to be current lays the note out to
  ;; the wrong width.
  (let* ((long-line (concat "(" (make-string 40 ?x) ")"))
         (target (mevedel-test--note-buffer
                  "measure-target" (concat "one\n" long-line "\n")))
         (elsewhere (mevedel-test--note-buffer "measure-elsewhere" "x\n"))
         (note (string-join (make-list 20 "word") " "))
         (mevedel-buddy-note-width 60)
         (mevedel-buddy-note-current-line-style 'eol)
         (mevedel-buddy-note-other-lines-style 'eol)
         (id (mevedel-test--add-note target 2 note))
         (from-own-buffer
          (with-current-buffer target
            (goto-char (point-min))
            (mevedel-buddy-note-update id note)
            (mevedel-test--note-text target))))
    (with-current-buffer elsewhere
      (mevedel-buddy-note-update id note))
    (should (equal from-own-buffer (mevedel-test--note-text target))))

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
    (let ((mevedel-buddy-note--scope-buffers
           (list (cons "elsewhere" (current-buffer)))))
      (should (string-match-p "not in the review scope"
                              (mevedel-buddy-note-update id "rewritten"))))
    (should (equal "original" (plist-get (mevedel-buddy-note--find id) :note))))

  :doc "`mevedel-buddy-note-remove' refuses a note outside the scope"
  (let* ((buf (mevedel-test--note-buffer "scope-remove" "one\n"))
         (id (mevedel-test--add-note buf 1 "keep me")))
    (let ((mevedel-buddy-note--scope-buffers
           (list (cons "elsewhere" (current-buffer)))))
      (should (string-match-p "not in the review scope"
                              (mevedel-buddy-note-remove id))))
    (should (mevedel-buddy-note--find id)))

  :doc "`mevedel-buddy-note-serialize' describes only in-scope notes"
  (let ((here (mevedel-test--note-buffer "scope-here" "one\n"))
        (elsewhere (mevedel-test--note-buffer "scope-elsewhere" "one\n")))
    (mevedel-test--add-note here 1 "mine")
    (mevedel-test--add-note elsewhere 1 "other project")
    (let* ((mevedel-buddy-note--scope-buffers
            (list (cons "scope-here" here)))
           (text (mevedel-buddy-note-serialize)))
      (should (string-match-p "mine" text))
      (should-not (string-match-p "other project" text)))))


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
    (mevedel-buddy-note-add (buffer-name buf) 3 "about three" "significant")
    (with-current-buffer buf
      (should (equal "three"
                     (buffer-substring-no-properties
                      (overlay-start (car (mevedel-test--note-overlays buf)))
                      (overlay-end (car (mevedel-test--note-overlays buf))))))))

  :doc "a note follows its text when insertion starts exactly at its marker"
  (let ((buf (mevedel-test--note-buffer "marker-boundary" "one\ntwo\nthree\n")))
    (mevedel-buddy-note-capture-markers '(("marker-boundary" . (2))))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line 1)
      (insert "inserted\n"))
    (mevedel-buddy-note-add
     (buffer-name buf) 2 "about two" "significant")
    (with-current-buffer buf
      (should (equal "two"
                     (buffer-substring-no-properties
                      (overlay-start (car (mevedel-test--note-overlays buf)))
                      (overlay-end (car (mevedel-test--note-overlays buf))))))))

  :doc "a note stays on a shown line when that whole line is replaced"
  (let ((buf (mevedel-test--note-buffer "marker-replaced" "one\ntwo\nthree\n")))
    (mevedel-buddy-note-capture-markers '(("marker-replaced" . (2))))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line 1)
      (delete-region (point) (line-beginning-position 2))
      (insert "changed\n"))
    (mevedel-buddy-note-add
     (buffer-name buf) 2 "about the replacement" "significant")
    (with-current-buffer buf
      (should (equal "changed"
                     (buffer-substring-no-properties
                      (overlay-start (car (mevedel-test--note-overlays buf)))
                      (overlay-end (car (mevedel-test--note-overlays buf))))))))

  :doc "a note is rejected when its shown line was deleted"
  (let ((buf (mevedel-test--note-buffer "marker-deleted" "one\ntwo\nthree\n")))
    (mevedel-buddy-note-capture-markers '(("marker-deleted" . (2))))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line 1)
      (delete-region (point) (line-beginning-position 2)))
    (should (string-match-p
             "not shown"
             (mevedel-buddy-note-add
              (buffer-name buf) 2 "about two" "significant")))
    (should-not (mevedel-test--note-overlays buf)))

  :doc "`mevedel-buddy-note-capture-markers' marks only the lines it is given"
  (let ((_buf (mevedel-test--note-buffer "marker-few" "one\ntwo\nthree\n")))
    (mevedel-buddy-note-capture-markers (list (cons "marker-few" '(2))))
    (should (= 1 (length (cdr (assoc "marker-few"
                                     mevedel-buddy-note--markers))))))

  :doc "`mevedel-buddy-note-release-markers' unsets every marker"
  (let ((_buf (mevedel-test--note-buffer "marker-release" "one\ntwo\n")))
    (mevedel-buddy-note-capture-markers (list (cons "marker-release" '(1 2))))
    (let ((markers (mapcan (lambda (entry)
                             (list (nth 1 entry) (nth 2 entry)))
                           (cdr (assoc "marker-release"
                                       mevedel-buddy-note--markers)))))
      (mevedel-buddy-note-release-markers)
      (should (null mevedel-buddy-note--markers))
      (should (seq-every-p (lambda (m) (null (marker-buffer m))) markers)))))

;;
;;; Buffer reading

(mevedel-deftest mevedel-buddy-note-read-buffer
  (:after-each (mevedel-test--note-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy-note-read-buffer' numbers the lines it returns"
  (let ((_buf (mevedel-test--note-buffer "read-plain" "one\ntwo\nthree\n")))
    (let ((out (mevedel-buddy-note-read-buffer "read-plain" 2 3)))
      (should (string-match-p "^ +2  two$" out))
      (should (string-match-p "^ +3  three$" out))
      (should-not (string-match-p "one" out))))

  :doc "`mevedel-buddy-note-read-buffer' refuses a buffer outside the scope"
  (let ((_buf (mevedel-test--note-buffer "read-scope" "one\n")))
    (let ((mevedel-buddy-note--scope-buffers
           (list (cons "other-buffer" (current-buffer)))))
      (should (string-match-p
               "not in the review scope"
               (mevedel-buddy-note-read-buffer "read-scope" 1 1)))))

  :doc "`mevedel-buddy-note-read-buffer' refuses everything with no review"
  (let ((_buf (mevedel-test--note-buffer "read-noscope" "one\n")))
    (let ((mevedel-buddy-note--scope-buffers nil))
      (should (string-match-p
               "not in the review scope"
               (mevedel-buddy-note-read-buffer "read-noscope" 1 1)))))

  :doc "`mevedel-buddy-note-read-buffer' refuses a buffer that is not live"
  (let ((dead (generate-new-buffer "read-dead")))
    (kill-buffer dead)
    (let ((mevedel-buddy-note--scope-buffers (list (cons "read-dead" dead))))
      (should (string-match-p
               "Unknown buffer"
               (mevedel-buddy-note-read-buffer "read-dead" 1 1)))))

  :doc "`mevedel-buddy-note-read-buffer' rejects a replacement with the same name"
  (let ((original (mevedel-test--note-buffer "read-reused" "original\n")))
    (kill-buffer original)
    (let ((replacement (generate-new-buffer "read-reused")))
      (push replacement mevedel-test--note-buffers)
      (with-current-buffer replacement (insert "replacement\n"))
      (should (string-match-p
               "not in the review scope"
               (mevedel-buddy-note-read-buffer "read-reused" 1 1)))))

  :doc "`mevedel-buddy-note-read-buffer' requires both bounds"
  (let ((_buf (mevedel-test--note-buffer "read-bounds" "one\ntwo\n")))
    (dolist (bounds '((nil 2) (1 nil) (nil nil)))
      (should (string-match-p
               "required"
               (mevedel-buddy-note-read-buffer
                "read-bounds" (car bounds) (cadr bounds))))))

  :doc "`mevedel-buddy-note-read-buffer' accepts bounds sent as strings"
  ;; The buddy workload commonly resolves to a local model, and those
  ;; send a JSON number as a string often enough to matter.
  (let ((_buf (mevedel-test--note-buffer "read-strings" "one\ntwo\nthree\n")))
    (should (string-match-p
             "^ +2  two$"
             (mevedel-buddy-note-read-buffer "read-strings" "2" "2"))))

  :doc "`mevedel-buddy-note-read-buffer' refuses a reversed range"
  (let ((_buf (mevedel-test--note-buffer "read-reversed" "one\ntwo\n")))
    (should (string-match-p
             "comes after"
             (mevedel-buddy-note-read-buffer "read-reversed" 2 1))))

  :doc "`mevedel-buddy-note-read-buffer' clamps an end past the last line"
  (let ((_buf (mevedel-test--note-buffer "read-clamp" "one\ntwo")))
    (let ((out (mevedel-buddy-note-read-buffer "read-clamp" 1 999)))
      (should (string-match-p "^ +1  one$" out))
      (should (string-match-p "^ +2  two$" out))
      (should-not (string-match-p "^ +3" out))))

  :doc "`mevedel-buddy-note-read-buffer' reports a begin past the last line"
  (let ((_buf (mevedel-test--note-buffer "read-past" "one\ntwo")))
    (should (string-match-p
             "only 2 lines"
             (mevedel-buddy-note-read-buffer "read-past" 50 60))))

  :doc "`mevedel-buddy-note-read-buffer' stops at the read limit"
  (let* ((content (mapconcat (lambda (n) (format "line %d" n))
                             (number-sequence 1 30) "\n"))
         (_buf (mevedel-test--note-buffer "read-limit" content))
         (mevedel-buddy-note-read-limit 5)
         (out (mevedel-buddy-note-read-buffer "read-limit" 1 30)))
    (should (string-match-p "^ +1  line 1$" out))
    (should (string-match-p "^ +5  line 5$" out))
    (should-not (string-match-p "^ +6  line 6$" out))
    (should (string-match-p "Read again from line 6" out)))

  :doc "`mevedel-buddy-note-read-buffer' refuses a nonpositive read limit"
  (let ((_buf (mevedel-test--note-buffer "read-zero-limit" "one\ntwo\n"))
        (mevedel-buddy-note-read-limit 0))
    (should (string-match-p
             "limit must be positive"
             (mevedel-buddy-note-read-buffer "read-zero-limit" 1 2))))

  :doc "`mevedel-buddy-note-read-buffer' makes the lines it returns annotatable"
  (let ((buf (mevedel-test--note-buffer
              "read-annotate" "one\ntwo\nthree\nfour\nfive\n")))
    (mevedel-buddy-note-capture-markers '(("read-annotate" . (5))))
    ;; Line 2 was not in the diff, so it cannot carry a note yet.
    (should (string-match-p
             "not shown"
             (mevedel-buddy-note-add "read-annotate" 2 "early" "significant")))
    (mevedel-buddy-note-read-buffer "read-annotate" 1 3)
    (let ((id (mevedel-buddy-note-add
               "read-annotate" 2 "about two" "significant")))
      (should (integerp id))
      (with-current-buffer buf
        (should (= 2 (line-number-at-pos
                      (overlay-start
                       (car (mevedel-test--note-overlays buf)))))))))

  :doc "a note on a read line follows its text when lines move above it"
  (let ((buf (mevedel-test--note-buffer
              "read-follow" "one\ntwo\nthree\nfour\n")))
    (mevedel-buddy-note-capture-markers '(("read-follow" . (4))))
    (mevedel-buddy-note-read-buffer "read-follow" 1 3)
    (mevedel-buddy-note-add "read-follow" 3 "about three" "significant")
    (with-current-buffer buf
      (goto-char (point-min))
      (insert "zero\n")
      (should (= 4 (line-number-at-pos
                    (overlay-start
                     (car (mevedel-test--note-overlays buf))))))))

  :doc "`mevedel-buddy-note-read-buffer' keeps the markers a line already has"
  ;; Recapturing would move an existing note's anchor to wherever that
  ;; number points now, which is what markers exist to prevent.
  (let ((_buf (mevedel-test--note-buffer "read-remark" "one\ntwo\nthree\n")))
    (mevedel-buddy-note-capture-markers '(("read-remark" . (2))))
    (let ((before (assq 2 (cdr (assoc "read-remark"
                                      mevedel-buddy-note--markers)))))
      (mevedel-buddy-note-read-buffer "read-remark" 1 3)
      (let* ((entry (cdr (assoc "read-remark" mevedel-buddy-note--markers)))
             (after (assq 2 entry)))
        (should (= 3 (length entry)))
        (should (eq (nth 1 before) (nth 1 after))))))

  :doc "`mevedel-buddy-note-read-buffer' stops before a stale numeric marker"
  (let ((buf (mevedel-test--note-buffer
              "read-shifted" "one\ntwo\nthree\n")))
    (mevedel-buddy-note-capture-markers '(("read-shifted" . (2))))
    (with-current-buffer buf
      (goto-char (point-min))
      (insert "zero\n"))
    (let ((out (mevedel-buddy-note-read-buffer "read-shifted" 2 2)))
      (should (string-match-p "no longer names" out))
      (should-not (string-match-p "^ +2  one$" out)))
    (let ((id (mevedel-buddy-note-add
               "read-shifted" 2 "about two" "significant")))
      (should (integerp id))
      (with-current-buffer buf
        (should (= 3 (line-number-at-pos
                      (overlay-start
                       (car (mevedel-test--note-overlays buf)))))))))

  :doc "`mevedel-buddy-note-read-buffer' returns only lines marked before the ceiling"
  ;; Emacs walks a buffer marker list on every insertion and the user is
  ;; typing throughout, so the marker set must grow with what was edited,
  ;; not with what the model asked to read.
  (let* ((content (mapconcat (lambda (n) (format "line %d" n))
                             (number-sequence 1 20) "\n"))
         (_buf (mevedel-test--note-buffer "read-ceiling" content))
         (mevedel-buddy-note--marker-ceiling 3)
         (out (mevedel-buddy-note-read-buffer "read-ceiling" 1 20)))
    (should (string-match-p "^ +3  line 3$" out))
    (should-not (string-match-p "^ +4  line 4$" out))
    (should (string-match-p "annotatable-line limit" out))
    (should (= 3 (length (cdr (assoc "read-ceiling"
                                     mevedel-buddy-note--markers)))))
    (dolist (line '(1 2 3))
      (should (integerp (mevedel-buddy-note-add
                         "read-ceiling" line "annotatable" "significant")))))

  :doc "`mevedel-buddy-note-read-buffer' refuses once its review is over"
  (let* ((_buf (mevedel-test--note-buffer "read-stale" "one\ntwo\n"))
         (stale (mevedel-buddy-note-tools (lambda () nil)))
         (read (seq-find (lambda (tool)
                           (equal "read_buffer" (gptel-tool-name tool)))
                         stale)))
    (should (string-match-p
             "review has ended"
             (funcall (gptel-tool-function read) "read-stale" 1 2)))))


;;
;;; Tool schemas

(mevedel-deftest mevedel-buddy-note-tools
  (:doc "`mevedel-buddy-note-tools' follows gptel's argument convention")
  ,test
  (test)

  :doc "every tool is built and named"
  (let ((names (mapcar #'gptel-tool-name
                       (mevedel-buddy-note-tools (lambda () t)))))
    (should (equal '("read_buffer" "add_note" "update_note" "remove_note")
                   names)))

  :doc "no argument carries `:required'"
  ;; gptel marks optional arguments and derives the schema's required
  ;; array itself.  A stray `:required' is passed through into the
  ;; per-property schema, where JSON Schema expects an array, and strict
  ;; providers reject the whole request.
  (dolist (tool (mevedel-buddy-note-tools (lambda () t)))
    (dolist (arg (gptel-tool-args tool))
      (should-not (plist-member arg :required))))

  :doc "only genuinely optional arguments are marked optional"
  (let* ((tools (mevedel-buddy-note-tools (lambda () t)))
         (optional
          (mapcan
           (lambda (tool)
             (mapcar (lambda (arg) (plist-get arg :name))
                     (seq-filter (lambda (arg) (plist-get arg :optional))
                                 (gptel-tool-args tool))))
           tools)))
    (should-not optional))

  :doc "every argument declares a type and a description"
  (dolist (tool (mevedel-buddy-note-tools (lambda () t)))
    (dolist (arg (gptel-tool-args tool))
      (should (plist-get arg :type))
      (should (plist-get arg :description))))

  :doc "a tool refuses once its own review is over"
  ;; A request outlives the review that started it, and the next review
  ;; repopulates the buffer allowlist, so a straggler's calls would pass
  ;; the scope check and land in that review -- `remove_note' deleting its
  ;; notes, since ids come from one counter.
  (let* ((buf (mevedel-test--note-buffer "stale-tool" "one\ntwo\n"))
         (id (mevedel-test--add-note buf 1 "belongs to the new review"))
         (stale (mevedel-buddy-note-tools (lambda () nil)))
         (remove (seq-find (lambda (tool)
                             (equal "remove_note" (gptel-tool-name tool)))
                           stale)))
    (should (string-match-p "review has ended"
                            (funcall (gptel-tool-function remove) id)))
    (should (mevedel-buddy-note--find id))
    (should (mevedel-test--note-overlays buf))))

;;
;;; Layout

(mevedel-deftest mevedel-buddy-note--wrap
  (:doc "`mevedel-buddy-note--wrap' fills text to the given width")
  ,test
  (test)

  :doc "long text becomes several lines, none over the width"
  (let ((lines (mevedel-buddy-note--wrap
                (string-join (make-list 20 "word") " ") 30)))
    (should (> (length lines) 1))
    (should (seq-every-p (lambda (line) (<= (length line) 30)) lines)))

  :doc "short text stays one line"
  (should (equal '("short enough")
                 (mevedel-buddy-note--wrap "short enough" 30))))

(mevedel-deftest mevedel-buddy-note--eol-string
  (:doc "`mevedel-buddy-note--eol-string' shortens a note to the room left")
  ,test
  (test)

  :doc "a note that fits is shown whole"
  (let ((mevedel-buddy-note-width 72))
    (should (string-match-p "typo here"
                            (mevedel-buddy-note--eol-string
                             "typo here" "significant" 10))))

  :doc "a note that does not fit is shortened"
  (let* ((mevedel-buddy-note-width 40)
         (note (concat (string-join (make-list 20 "word") " ") " caboose"))
         (shown (mevedel-buddy-note--eol-string note "significant" 10)))
    (should (< (length shown) (length note)))
    (should-not (string-match-p "caboose" shown)))

  :doc "a long code line still leaves a readable minimum"
  (let* ((mevedel-buddy-note-width 40)
         (shown (mevedel-buddy-note--eol-string
                 "a note about something" "significant" 200)))
    (should (> (length shown) 10)))

  :doc "newlines in a note are collapsed so it stays one line"
  (let ((mevedel-buddy-note-width 72))
    (should-not (string-match-p
                 "\n" (mevedel-buddy-note--eol-string
                       "first\nsecond" "significant" 0)))))

(mevedel-deftest mevedel-buddy-note--below-string
  (:doc "`mevedel-buddy-note--below-string' lays a note out under the code")
  ,test
  (test)

  :doc "the note starts on its own line"
  (should (string-prefix-p
           "\n" (mevedel-buddy-note--below-string
                 "a note" "significant" 4)))

  :doc "every line is indented to the code"
  (let* ((mevedel-buddy-note-width 40)
         (shown (mevedel-buddy-note--below-string
                 (string-join (make-list 20 "word") " ") "significant" 6))
         (lines (cdr (split-string shown "\n"))))
    (should (> (length lines) 1))
    (should (seq-every-p (lambda (line) (string-prefix-p "      " line))
                         lines)))

  :doc "the full text survives the layout"
  (let ((shown (mevedel-buddy-note--below-string
                "accumulator is never updated" "significant" 2)))
    (should (string-match-p "accumulator" shown))
    (should (string-match-p "updated" shown))))

(mevedel-deftest mevedel-buddy-note--style-for
  (:after-each (mevedel-test--note-cleanup))
  ,test
  (test)

  :doc "the line at point uses the current-line style"
  (let* ((buf (mevedel-test--note-buffer "style-here" "one\ntwo\nthree\n"))
         (_id (mevedel-test--add-note buf 2 "about two"))
         (mevedel-buddy-note-current-line-style 'below)
         (mevedel-buddy-note-other-lines-style 'eol))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line 1)
      (should (eq 'below (mevedel-buddy-note--style-for
                          (car (mevedel-test--note-overlays buf))))))) 

  :doc "any other line uses the other-lines style"
  (let* ((buf (mevedel-test--note-buffer "style-other" "one\ntwo\nthree\n"))
         (_id (mevedel-test--add-note buf 2 "about two"))
         (mevedel-buddy-note-current-line-style 'below)
         (mevedel-buddy-note-other-lines-style 'eol))
    (with-current-buffer buf
      (goto-char (point-min))
      (should (eq 'eol (mevedel-buddy-note--style-for
                        (car (mevedel-test--note-overlays buf))))))))

(mevedel-deftest mevedel-buddy-note--relayout
  (:after-each (mevedel-test--note-cleanup))
  ,test
  (test)

  :doc "moving onto a note's line lays it out in full"
  (let* ((buf (mevedel-test--note-buffer "relayout" "one\ntwo\nthree\n"))
         (note (string-join (make-list 20 "word") " "))
         (mevedel-buddy-note-width 40)
         (mevedel-buddy-note-current-line-style 'below)
         (mevedel-buddy-note-other-lines-style 'eol))
    (mevedel-test--add-note buf 2 note)
    (with-current-buffer buf
      (goto-char (point-min))
      (mevedel-buddy-note--relayout)
      ;; Off the line: one shortened line, no newline in the layout.
      (should-not (string-match-p "\n" (mevedel-test--note-text buf)))
      (forward-line 1)
      (mevedel-buddy-note--relayout)
      ;; On the line: the whole note, across several lines.
      (should (string-match-p "\n" (mevedel-test--note-text buf)))))

  :doc "a note is hidden when its style is nil"
  (let* ((buf (mevedel-test--note-buffer "relayout-nil" "one\ntwo\n"))
         (mevedel-buddy-note-current-line-style 'below)
         (mevedel-buddy-note-other-lines-style nil))
    (mevedel-test--add-note buf 2 "about two")
    (with-current-buffer buf
      (goto-char (point-min))
      (mevedel-buddy-note--relayout)
      (should (string-empty-p (mevedel-test--note-text buf)))))

  :doc "`mevedel-buddy-note--relayout' stops the hook once notes are gone"
  (let* ((buf (mevedel-test--note-buffer "relayout-hook" "one\ntwo\n"))
         (id (mevedel-test--add-note buf 1 "goes away")))
    (with-current-buffer buf
      (should (memq #'mevedel-buddy-note--relayout post-command-hook))
      (mevedel-buddy-note-remove id)
      (should-not (memq #'mevedel-buddy-note--relayout post-command-hook)))))

(mevedel-deftest mevedel-buddy-note-clear-all
  (:after-each (mevedel-test--note-cleanup)
   :doc "`mevedel-buddy-note-clear-all' drops the layout hooks too")
  (let ((buf (mevedel-test--note-buffer "clear-hook" "one\ntwo\n")))
    (mevedel-test--add-note buf 1 "a note")
    (with-current-buffer buf
      (should (memq #'mevedel-buddy-note--relayout post-command-hook)))
    (mevedel-buddy-note-clear-all)
    (with-current-buffer buf
      (should-not (memq #'mevedel-buddy-note--relayout post-command-hook)))))

(mevedel-deftest mevedel-buddy-note--render
  (:after-each (mevedel-test--note-cleanup))
  ,test
  (test)

  :doc "`mevedel-buddy-note--render' measures the line in display columns"
  ;; A tab is one character and eight columns.  Two lines that occupy the
  ;; same columns must leave the note the same room, however they are
  ;; indented; counting characters instead makes the tabbed line look
  ;; short and overflows the budget the fixed width exists to hold.
  (let* ((tab-width 8)
         (note (string-join (make-list 20 "word") " "))
         (mevedel-buddy-note-width 60)
         (mevedel-buddy-note-current-line-style 'eol)
         (mevedel-buddy-note-other-lines-style 'eol)
         (tabbed (mevedel-test--note-buffer "tabbed" "\t\t\tcode\n"))
         (spaced (mevedel-test--note-buffer
                  "spaced" (concat (make-string 24 ?\s) "code\n"))))
    (mevedel-test--add-note tabbed 1 note)
    (mevedel-test--add-note spaced 1 note)
    (should (equal (substring-no-properties (mevedel-test--note-text spaced))
                   (substring-no-properties
                    (mevedel-test--note-text tabbed)))))

  :doc "a note keeps its full layout when updated from another buffer"
  ;; `update_note' runs as a tool call from the buffer the review started
  ;; in.  Deciding the style there would flip a note the user is sitting on
  ;; to the truncated style, and `--relayout' would not restore it until
  ;; point left the line and came back.
  (let* ((target (mevedel-test--note-buffer "style-target" "one\ntwo\n"))
         (elsewhere (mevedel-test--note-buffer "style-elsewhere" "x\n"))
         (note (string-join (make-list 20 "word") " "))
         (mevedel-buddy-note-width 40)
         (mevedel-buddy-note-current-line-style 'below)
         (mevedel-buddy-note-other-lines-style 'eol)
         (id (mevedel-test--add-note target 2 note)))
    (with-current-buffer target
      (goto-char (point-min))
      (forward-line 1)
      (mevedel-buddy-note--relayout)
      (should (string-match-p "\n" (mevedel-test--note-text target))))
    (with-current-buffer elsewhere
      (mevedel-buddy-note-update id note))
    ;; Point never moved, so the note is still laid out in full.
    (should (string-match-p "\n" (mevedel-test--note-text target)))))

(provide 'test-mevedel-buddy-note)
;;; test-mevedel-buddy-note.el ends here
