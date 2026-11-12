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

(defvar mevedel-test--note-directory nil
  "Private workspace shared by the current case's source files.")

(defvar mevedel-test--note-workspace nil
  "Workspace owning the current case's source files.")

(defun mevedel-test--note-buffer (name content)
  "Return a new buffer NAME holding CONTENT, in scope for notes."
  (unless mevedel-test--note-directory
    (setq mevedel-test--note-directory (make-temp-file "mevedel-buddy-note-" t)
          mevedel-test--note-workspace
          (mevedel-workspace--create
           :type 'file :id mevedel-test--note-directory
           :root mevedel-test--note-directory :name "buddy-note"))
    (make-directory (file-name-concat mevedel-test--note-directory ".mevedel")))
  (let* ((file (make-temp-file
                (file-name-concat mevedel-test--note-directory "source-")))
         (_ (write-region content nil file nil 'silent))
         (buf (find-file-noselect file)))
    (push buf mevedel-test--note-buffers)
    (with-current-buffer buf
      (rename-buffer name t)
      (setq-local mevedel--workspace mevedel-test--note-workspace))
    (push (cons name buf) mevedel-buddy-note--scope-buffers)
    buf))

(defun mevedel-test--note-cleanup ()
  "Kill note test buffers and discard every note."
  (when mevedel-test--note-workspace
    (mevedel--clear-instruction-state mevedel-test--note-workspace))
  (mevedel-buddy-note-release-markers)
  (setq mevedel-buddy-note--scope-buffers nil
        mevedel-buddy-note--notes nil
        mevedel-buddy-note--changed-workspaces nil
        mevedel-instruction-store--dirty nil)
  (remove-hook 'post-command-hook #'mevedel-instruction-store-flush)
  (dolist (buf mevedel-test--note-buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)))
  (when mevedel-test--note-directory (delete-directory mevedel-test--note-directory t))
  (setq mevedel-test--note-buffers nil
        mevedel-test--note-workspace nil
        mevedel-test--note-directory nil))

(defun mevedel-test--note-overlays (buffer)
  "Return BUFFER's note overlays."
  (with-current-buffer buffer
    (seq-filter (lambda (ov) (eq 'buddy (overlay-get ov 'mevedel-note-author)))
                (overlays-in (point-min) (point-max)))))

(defun mevedel-test--note-text (buffer)
  "Return the concatenated note text visible in BUFFER."
  (mapconcat (lambda (ov)
               (or (when-let* ((label (overlay-get ov 'mevedel-label-overlay)))
                     (overlay-get label 'after-string)) ""))
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

  :doc "`mevedel-buddy-note-update' renders the same label from another buffer"
  (let* ((long-line (concat "(" (make-string 40 ?x) ")"))
         (target (mevedel-test--note-buffer
                  "measure-target" (concat "one\n" long-line "\n")))
         (elsewhere (mevedel-test--note-buffer "measure-elsewhere" "x\n"))
         (note (string-join (make-list 20 "word") " "))
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
    (should-not (string-match-p "changed since" (mevedel-buddy-note-serialize)))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line 1)
      (end-of-line)
      (insert " changed"))
    (should (mevedel-buddy-note--find id))
    (should (string-match-p "changed since" (mevedel-buddy-note-serialize))))

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

(mevedel-deftest mevedel-buddy-note--shared-instruction
  (:after-each (mevedel-test--note-cleanup)
   :doc "Buddy remarks are ordinary notes in instruction enumeration")
  (let ((buf (mevedel-test--note-buffer "note-instr" "one\ntwo\n")))
    (mevedel-test--add-note buf 1 "a shared note")
    (with-current-buffer buf
      (let ((notes (seq-filter
                    (lambda (ov) (overlay-get ov 'mevedel-instruction))
                    (overlays-in (point-min) (point-max)))))
        (should (= 1 (length notes)))
        (should (eq 'note (overlay-get (car notes) 'mevedel-instruction-type)))))))

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
  (:after-each (mevedel-test--note-cleanup) :quiet t)
  ,test
  (test)
  :doc "dismiss-all clears Buddy remarks across this workspace"
  (let* ((here (mevedel-test--note-buffer "cmd-here" "one\ntwo\n"))
         (elsewhere (mevedel-test--note-buffer "cmd-elsewhere" "one\n"))
         (kept (mevedel-test--add-note elsewhere 1 "stays")))
    (mevedel-test--add-note here 1 "goes")
    (mevedel-test--add-note here 2 "also goes")
    (with-current-buffer here (mevedel-buddy-dismiss-notes))
    (should (null (mevedel-test--note-overlays here)))
    (should (eq 'dismissed (plist-get (mevedel-buddy-note--find kept) :status))))
  :doc "dismiss-all includes unopened files after reload and leaves authored notes alone"
  (let* ((here (mevedel-test--note-buffer "cmd-current" "one\ntwo\n"))
         (elsewhere (mevedel-test--note-buffer "cmd-unopened" "one\n"))
         (other-file (buffer-file-name elsewhere)))
    (mevedel-test--add-note here 1 "First concern")
    (mevedel-test--add-note elsewhere 1 "Other concern")
    (with-current-buffer here
      (goto-char (point-min))
      (mevedel-note-create "Keep this user note"))
    (mevedel-instruction-store-flush t)
    (kill-buffer elsewhere)
    (mevedel--clear-instruction-state mevedel-test--note-workspace)
    (with-current-buffer here
      (should (= 2 (mevedel-buddy-dismiss-notes))))
    (push (find-buffer-visiting other-file) mevedel-test--note-buffers)
    (mevedel--clear-instruction-state mevedel-test--note-workspace)
    (mevedel-instruction-store-observe mevedel-test--note-workspace)
    (should (= 2 (length (mevedel-notes-closed mevedel-test--note-workspace))))
    (with-current-buffer here
      (should (cl-some (lambda (note)
                         (equal "Keep this user note" (overlay-get note 'mevedel-note-text)))
                       (overlays-in (point-min) (point-max)))))))

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
  (:after-each (mevedel-test--note-cleanup)
   :doc "`mevedel-buddy-note-tools' follows gptel's argument convention")
  ,test
  (test)

  :doc "builds named tools with typed, described, required arguments"
  (let ((tools (mevedel-buddy-note-tools (lambda () t))))
    (should (equal '("read_buffer" "add_note" "update_note" "remove_note" "note_looks_done")
                   (mapcar #'gptel-tool-name tools)))
    (dolist (tool tools)
      (dolist (arg (gptel-tool-args tool))
        ;; gptel derives the schema's required array from :optional; a
        ;; :required property here would make strict providers reject it.
        (should-not (plist-member arg :required))
        (should-not (plist-get arg :optional))
        (should (plist-get arg :type))
        (should (plist-get arg :description)))))

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

(provide 'test-mevedel-buddy-note)
;;; test-mevedel-buddy-note.el ends here
