;;; test-niwa-buddy-note.el --- Tests for buddy notes and tools -*- lexical-binding: t -*-

;;; Commentary:

;; Seam 2: tool calls in, note overlays out.  No model.

;;; Code:

(require 'niwa-buddy-note)
(require 'niwa-buddy)
(require 'niwa-persistence)
(require 'gptel)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defvar niwa-test--note-buffers nil
  "Buffers created by the note tests, killed in teardown.")

(defvar niwa-test--note-directory nil
  "Private workspace shared by the current case's source files.")

(defvar niwa-test--note-workspace nil
  "Workspace owning the current case's source files.")

(defun niwa-test--note-buffer (name content)
  "Return a new buffer NAME holding CONTENT, in scope for notes."
  (unless niwa-test--note-directory
    (setq niwa-test--note-directory (make-temp-file "niwa-buddy-note-" t)
          niwa-test--note-workspace
          (niwa-workspace--create
           :type 'file :id niwa-test--note-directory
           :root niwa-test--note-directory :name "buddy-note"))
    (make-directory (file-name-concat niwa-test--note-directory ".niwa")))
  (let* ((file (make-temp-file
                (file-name-concat niwa-test--note-directory "source-")))
         (_ (write-region content nil file nil 'silent))
         (buf (find-file-noselect file)))
    (push buf niwa-test--note-buffers)
    (with-current-buffer buf
      (rename-buffer name t)
      (setq-local niwa--workspace niwa-test--note-workspace))
    (unless niwa-buddy-note--run
      (setq niwa-buddy-note--run (niwa-buddy-note--make-run)))
    (push (cons name buf) (niwa-buddy-note--run-scope niwa-buddy-note--run))
    buf))

(defun niwa-test--note-cleanup ()
  "Kill note test buffers and discard every note."
  (when niwa-test--note-workspace
    (niwa--clear-instruction-state niwa-test--note-workspace))
  (niwa-buddy-note-release-markers)
  (setq niwa-buddy-note--run nil
        niwa-buddy-note--notes nil
        niwa-instruction-store--dirty nil)
  (remove-hook 'post-command-hook #'niwa-instruction-store-flush)
  (dolist (buf niwa-test--note-buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)))
  (when niwa-test--note-directory (delete-directory niwa-test--note-directory t))
  (setq niwa-test--note-buffers nil
        niwa-test--note-workspace nil
        niwa-test--note-directory nil))

(defun niwa-test--note-overlays (buffer)
  "Return BUFFER's note overlays."
  (with-current-buffer buffer
    (seq-filter (lambda (ov) (eq 'buddy (overlay-get ov 'niwa-note-author)))
                (overlays-in (point-min) (point-max)))))

(defun niwa-test--note-text (buffer)
  "Return the concatenated note text visible in BUFFER."
  (mapconcat (lambda (ov)
               (or (when-let* ((label (overlay-get ov 'niwa-label-overlay)))
                     (overlay-get label 'after-string)) ""))
             (niwa-test--note-overlays buffer)
             "\n"))

(defun niwa-test--add-note (buffer line note &optional severity)
  "Add NOTE at LINE of BUFFER with SEVERITY and return its id."
  (niwa-buddy-note-capture-markers
   (list (cons (buffer-name buffer) (list line))))
  (niwa-buddy-note-add (buffer-name buffer) line note
                          (or severity "significant")))


;;
;;; Adding, updating, removing

(niwa-deftest niwa-buddy-note-add
  (:after-each (niwa-test--note-cleanup))
  (let ((niwa-note-labels 'expanded)) ,test)
  (test)

  :doc "`niwa-buddy-note-add' puts an overlay on the named line"
  (let* ((buf (niwa-test--note-buffer "note-add" "one\ntwo\nthree\n"))
         (id (niwa-test--add-note buf 2 "look here")))
    (should (integerp id))
    (should (= 1 (length (niwa-test--note-overlays buf))))
    (should (string-match-p "look here" (niwa-test--note-text buf)))
    (with-current-buffer buf
      (should (= 2 (line-number-at-pos
                    (overlay-start
                     (car (niwa-test--note-overlays buf))))))))

  :doc "`niwa-buddy-note-add' assigns distinct ids"
  (let* ((buf (niwa-test--note-buffer "note-ids" "one\ntwo\n"))
         (first (niwa-test--add-note buf 1 "first"))
         (second (niwa-test--add-note buf 2 "second")))
    (should-not (= first second)))

  :doc "`niwa-buddy-note-add' refuses a buffer outside the scope"
  (let ((buf (niwa-test--note-buffer "note-scope" "one\n")))
    (let ((niwa-buddy-note--run
           (niwa-buddy-note--make-run
            :scope (list (cons "other-buffer" (current-buffer))))))
      (should (string-match-p
               "not in the review scope"
               (niwa-buddy-note-add (buffer-name buf) 1 "nope" "trivial")))
      (should (null (niwa-test--note-overlays buf)))))

  :doc "`niwa-buddy-note-add' refuses everything when no review is running"
  (let ((buf (niwa-test--note-buffer "note-noscope" "one\n")))
    (let ((niwa-buddy-note--run nil))
      (should (string-match-p
               "not in the review scope"
               (niwa-buddy-note-add (buffer-name buf) 1 "nope" "trivial")))
      (should (null (niwa-test--note-overlays buf)))))

  :doc "`niwa-buddy-note-add' refuses a line the review was not shown"
  (let ((buf (niwa-test--note-buffer "note-unshown" "one\ntwo\nthree\n")))
    (niwa-buddy-note-capture-markers '(("note-unshown" . (2))))
    (with-current-buffer buf
      (goto-char (point-min))
      (insert "zero\n"))
    (should (string-match-p
             "not shown"
             (niwa-buddy-note-add
              (buffer-name buf) 1 "wrong line" "significant")))
    (should-not (niwa-test--note-overlays buf)))

  :doc "`niwa-buddy-note-add' refuses nonpositive and overlarge lines"
  (let ((buf (niwa-test--note-buffer "note-range" "one\ntwo\n")))
    (niwa-buddy-note-capture-markers '(("note-range" . (1 2))))
    (dolist (line '(0 -1 99))
      (should (string-match-p
               "not shown"
               (niwa-buddy-note-add
                (buffer-name buf) line "wrong line" "significant"))))
    (should-not (niwa-test--note-overlays buf)))

  :doc "`niwa-buddy-note-add' accepts a line number sent as a string"
  ;; The buddy workload commonly resolves to a local model, and those send
  ;; a JSON number as a string often enough that refusing one would refuse
  ;; real notes.
  (let ((buf (niwa-test--note-buffer "note-stringy" "one\ntwo\n")))
    (niwa-buddy-note-capture-markers '(("note-stringy" . (2))))
    (let ((id (niwa-buddy-note-add
               (buffer-name buf) "2" "about two" "significant")))
      (should (integerp id))
      (should (= 2 (plist-get (niwa-buddy-note--find id) :line)))
      (with-current-buffer buf
        (should (= 2 (line-number-at-pos
                      (overlay-start
                       (car (niwa-test--note-overlays buf)))))))))

  :doc "`niwa-buddy-note-add' refuses a released marker"
  (let ((buf (niwa-test--note-buffer "note-released" "one\n")))
    (niwa-buddy-note-capture-markers '(("note-released" . (1))))
    (niwa-buddy-note-release-markers)
    (should (string-match-p
             "not shown"
             (niwa-buddy-note-add
              (buffer-name buf) 1 "too late" "significant")))
    (should-not (niwa-test--note-overlays buf)))

  :doc "`niwa-buddy-note-add' rejects a captured buffer name reused elsewhere"
  (let ((original (niwa-test--note-buffer "note-reused" "original\n")))
    (niwa-buddy-note-capture-markers '(("note-reused" . (1))))
    (with-current-buffer original
      (rename-buffer "note-renamed"))
    (let ((replacement (niwa-test--note-buffer "note-reused" "replacement\n")))
      (should (string-match-p
               "not shown"
               (niwa-buddy-note-add
                (buffer-name replacement) 1 "wrong buffer" "significant")))
      (should-not (niwa-test--note-overlays replacement)))))

(niwa-deftest niwa-buddy-note-update
  (:after-each (niwa-test--note-cleanup))
  (let ((niwa-note-labels 'expanded)) ,test)
  (test)

  :doc "`niwa-buddy-note-update' replaces the text and keeps the line"
  (let* ((buf (niwa-test--note-buffer "note-update" "one\ntwo\nthree\n"))
         (id (niwa-test--add-note buf 2 "first wording"))
         (line (with-current-buffer buf
                 (line-number-at-pos
                  (overlay-start (car (niwa-test--note-overlays buf)))))))
    (niwa-buddy-note-update id "second wording")
    (should (string-match-p "second wording" (niwa-test--note-text buf)))
    (should-not (string-match-p "first wording" (niwa-test--note-text buf)))
    (with-current-buffer buf
      (should (= line (line-number-at-pos
                       (overlay-start
                        (car (niwa-test--note-overlays buf))))))))

  :doc "`niwa-buddy-note-update' renders the same label from another buffer"
  (let* ((long-line (concat "(" (make-string 40 ?x) ")"))
         (target (niwa-test--note-buffer
                  "measure-target" (concat "one\n" long-line "\n")))
         (elsewhere (niwa-test--note-buffer "measure-elsewhere" "x\n"))
         (note (string-join (make-list 20 "word") " "))
         (id (niwa-test--add-note target 2 note))
         (from-own-buffer
          (with-current-buffer target
            (goto-char (point-min))
            (niwa-buddy-note-update id note)
            (niwa-test--note-text target))))
    (with-current-buffer elsewhere
      (niwa-buddy-note-update id note))
    (should (equal from-own-buffer (niwa-test--note-text target))))

  :doc "`niwa-buddy-note-update' reports an unknown id without signalling"
  (should (string-match-p "Unknown note" (niwa-buddy-note-update 4242 "x"))))

(niwa-deftest niwa-buddy-note-remove
  (:after-each (niwa-test--note-cleanup))
  ,test
  (test)

  :doc "`niwa-buddy-note-remove' deletes the overlay"
  (let* ((buf (niwa-test--note-buffer "note-remove" "one\ntwo\n"))
         (id (niwa-test--add-note buf 1 "gone soon")))
    (niwa-buddy-note-remove id)
    (should (null (niwa-test--note-overlays buf))))

  :doc "`niwa-buddy-note-remove' reports an unknown id without signalling"
  (should (string-match-p "Unknown note" (niwa-buddy-note-remove 4242))))


;;
;;; Lifecycle

(niwa-deftest niwa-buddy-note--lifecycle
  (:after-each (niwa-test--note-cleanup))
  ,test
  (test)

  :doc "a note follows its text when lines are inserted above it"
  (let* ((buf (niwa-test--note-buffer "note-drift" "one\ntwo\nthree\n"))
         (_id (niwa-test--add-note buf 3 "about three")))
    (with-current-buffer buf
      (goto-char (point-min))
      (insert "zero\n")
      (should (= 4 (line-number-at-pos
                    (overlay-start
                     (car (niwa-test--note-overlays buf))))))))

  :doc "editing an annotated line marks the note for review"
  (let* ((buf (niwa-test--note-buffer "note-review" "one\ntwo\nthree\n"))
         (id (niwa-test--add-note buf 2 "about two")))
    (should-not (string-match-p "changed since" (niwa-buddy-note-serialize)))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line 1)
      (end-of-line)
      (insert " changed"))
    (should (niwa-buddy-note--find id))
    (should (string-match-p "changed since" (niwa-buddy-note-serialize))))

  :doc "dismissing a note deletes its overlay and keeps the record"
  (let* ((buf (niwa-test--note-buffer "note-dismiss" "one\ntwo\n"))
         (id (niwa-test--add-note buf 1 "dismiss me")))
    (niwa-buddy-note--dismiss-overlay
     (plist-get (niwa-buddy-note--find id) :overlay) "user")
    (should (null (niwa-test--note-overlays buf)))
    (should (eq 'dismissed (plist-get (niwa-buddy-note--find id) :status))))

  :doc "killing a buffer drops its notes and leaves no live overlay"
  (let* ((buf (niwa-test--note-buffer "note-kill" "one\ntwo\n"))
         (id (niwa-test--add-note buf 1 "doomed")))
    (kill-buffer buf)
    (should (null (niwa-buddy-note--find id)))))


;;
;;; Serialization for the prompt

(niwa-deftest niwa-buddy-note-serialize
  (:after-each (niwa-test--note-cleanup))
  ,test
  (test)

  :doc "`niwa-buddy-note-serialize' returns empty with no notes"
  (should (string-empty-p (niwa-buddy-note-serialize)))

  :doc "`niwa-buddy-note-serialize' lists active notes with id and line"
  (let* ((buf (niwa-test--note-buffer "note-ser" "one\ntwo\n"))
         (id (niwa-test--add-note buf 2 "still true")))
    (let ((text (niwa-buddy-note-serialize)))
      (should (string-match-p (format "note_id %d" id) text))
      (should (string-match-p "note-ser:2" text))
      (should (string-match-p "still true" text))))

  :doc "`niwa-buddy-note-serialize' keeps dismissed notes, labelled"
  (let* ((buf (niwa-test--note-buffer "note-ser-dismissed" "one\n"))
         (id (niwa-test--add-note buf 1 "rejected advice")))
    (niwa-buddy-note--dismiss-overlay
     (plist-get (niwa-buddy-note--find id) :overlay) "user")
    (let ((text (niwa-buddy-note-serialize)))
      (should (string-match-p "dismissed" text))
      (should (string-match-p "rejected advice" text))))

  :doc "`niwa-buddy-note-serialize' marks a note needing review"
  (let* ((buf (niwa-test--note-buffer "note-ser-review" "one\ntwo\n"))
         (_id (niwa-test--add-note buf 2 "check me")))
    (with-current-buffer buf
      (goto-char (point-max))
      (forward-line -1)
      (end-of-line)
      (insert " edited"))
    (should (string-match-p "changed since" (niwa-buddy-note-serialize))))

  :doc "`niwa-buddy-note-serialize' caps how many notes it sends"
  (let ((buf (niwa-test--note-buffer "note-cap" "one\n"))
        (niwa-buddy-note-serialize-limit 3))
    (dotimes (index 10)
      (let ((id (niwa-test--add-note buf 1 (format "note number %d" index))))
        (niwa-buddy-note--dismiss-overlay
         (plist-get (niwa-buddy-note--find id) :overlay) "user")))
    (let ((text (niwa-buddy-note-serialize)))
      (should (= 3 (length (seq-filter (lambda (line)
                                         (string-prefix-p "- note_id" line))
                                       (split-string text "\n")))))
      (should (string-match-p "note number 9" text))
      (should-not (string-match-p "note number 0" text)))))


;;
;;; Instruction isolation

(niwa-deftest niwa-buddy-note--shared-instruction
  (:after-each (niwa-test--note-cleanup)
   :doc "Buddy remarks are ordinary notes in instruction enumeration")
  (let ((buf (niwa-test--note-buffer "note-instr" "one\ntwo\n")))
    (niwa-test--add-note buf 1 "a shared note")
    (with-current-buffer buf
      (let ((notes (seq-filter
                    (lambda (ov) (overlay-get ov 'niwa-instruction))
                    (overlays-in (point-min) (point-max)))))
        (should (= 1 (length notes)))
        (should (eq 'note (overlay-get (car notes) 'niwa-instruction-type)))))))

;;
;;; Commands

(niwa-deftest niwa-buddy-dismiss-note
  (:after-each (niwa-test--note-cleanup) :quiet t)
  ,test
  (test)

  :doc "`niwa-buddy-dismiss-note' dismisses the note under point"
  (let* ((buf (niwa-test--note-buffer "cmd-one" "one\ntwo\n"))
         (id (niwa-test--add-note buf 1 "dismiss me")))
    (with-current-buffer buf
      (goto-char (point-min))
      (niwa-buddy-dismiss-note))
    (should (eq 'dismissed (plist-get (niwa-buddy-note--find id) :status))))

  :doc "`niwa-buddy-dismiss-note' errors when point has no note"
  (let ((buf (niwa-test--note-buffer "cmd-none" "one\n")))
    (with-current-buffer buf
      (should-error (niwa-buddy-dismiss-note) :type 'user-error))))

(niwa-deftest niwa-buddy-dismiss-notes
  (:after-each (niwa-test--note-cleanup) :quiet t)
  ,test
  (test)
  :doc "dismiss-all clears Buddy remarks across this workspace"
  (let* ((here (niwa-test--note-buffer "cmd-here" "one\ntwo\n"))
         (elsewhere (niwa-test--note-buffer "cmd-elsewhere" "one\n"))
         (kept (niwa-test--add-note elsewhere 1 "stays")))
    (niwa-test--add-note here 1 "goes")
    (niwa-test--add-note here 2 "also goes")
    (with-current-buffer here (niwa-buddy-dismiss-notes))
    (should (null (niwa-test--note-overlays here)))
    (should (eq 'dismissed (plist-get (niwa-buddy-note--find kept) :status))))
  :doc "dismiss-all includes unopened files after reload and leaves authored notes alone"
  (let* ((here (niwa-test--note-buffer "cmd-current" "one\ntwo\n"))
         (elsewhere (niwa-test--note-buffer "cmd-unopened" "one\n"))
         (other-file (buffer-file-name elsewhere)))
    (niwa-test--add-note here 1 "First concern")
    (niwa-test--add-note elsewhere 1 "Other concern")
    (with-current-buffer here
      (goto-char (point-min))
      (niwa-note-create "Keep this user note"))
    (niwa-instruction-store-flush t)
    (kill-buffer elsewhere)
    (niwa--clear-instruction-state niwa-test--note-workspace)
    (with-current-buffer here
      (should (= 2 (niwa-buddy-dismiss-notes))))
    (push (find-buffer-visiting other-file) niwa-test--note-buffers)
    (niwa--clear-instruction-state niwa-test--note-workspace)
    (niwa-instruction-store-observe niwa-test--note-workspace)
    (should (= 2 (length (niwa-notes-closed niwa-test--note-workspace))))
    (with-current-buffer here
      (should (cl-some (lambda (note)
                         (equal "Keep this user note" (overlay-get note 'niwa-note-text)))
                       (overlays-in (point-min) (point-max)))))))

;;
;;; Scope isolation

(niwa-deftest niwa-buddy-note--in-scope-p
  (:after-each (niwa-test--note-cleanup))
  ,test
  (test)

  :doc "`niwa-buddy-note-update' refuses a note outside the scope"
  (let* ((buf (niwa-test--note-buffer "scope-update" "one\n"))
         (id (niwa-test--add-note buf 1 "original")))
    (let ((niwa-buddy-note--run
           (niwa-buddy-note--make-run
            :scope (list (cons "elsewhere" (current-buffer))))))
      (should (string-match-p "not in the review scope"
                              (niwa-buddy-note-update id "rewritten"))))
    (should (equal "original" (plist-get (niwa-buddy-note--find id) :note))))

  :doc "`niwa-buddy-note-remove' refuses a note outside the scope"
  (let* ((buf (niwa-test--note-buffer "scope-remove" "one\n"))
         (id (niwa-test--add-note buf 1 "keep me")))
    (let ((niwa-buddy-note--run
           (niwa-buddy-note--make-run
            :scope (list (cons "elsewhere" (current-buffer))))))
      (should (string-match-p "not in the review scope"
                              (niwa-buddy-note-remove id))))
    (should (niwa-buddy-note--find id)))

  :doc "`niwa-buddy-note-serialize' describes only in-scope notes"
  (let ((here (niwa-test--note-buffer "scope-here" "one\n"))
        (elsewhere (niwa-test--note-buffer "scope-elsewhere" "one\n")))
    (niwa-test--add-note here 1 "mine")
    (niwa-test--add-note elsewhere 1 "other project")
    (let* ((niwa-buddy-note--run
            (niwa-buddy-note--make-run :scope (list (cons "scope-here" here))))
           (text (niwa-buddy-note-serialize)))
      (should (string-match-p "mine" text))
      (should-not (string-match-p "other project" text)))))


;;
;;; Markers

(niwa-deftest niwa-buddy-note-capture-markers
  (:after-each (niwa-test--note-cleanup))
  ,test
  (test)

  :doc "a note lands on its original text after lines are inserted above"
  (let ((buf (niwa-test--note-buffer "marker-drift" "one\ntwo\nthree\n")))
    ;; The review is assembled and its markers taken, then the user types
    ;; while the request is in flight, and only then does the note arrive.
    (niwa-buddy-note-capture-markers (list (cons "marker-drift" '(1 2 3))))
    (with-current-buffer buf
      (goto-char (point-min))
      (insert "zero\n"))
    (niwa-buddy-note-add (buffer-name buf) 3 "about three" "significant")
    (with-current-buffer buf
      (should (equal "three"
                     (buffer-substring-no-properties
                      (overlay-start (car (niwa-test--note-overlays buf)))
                      (overlay-end (car (niwa-test--note-overlays buf))))))))

  :doc "a note follows its text when insertion starts exactly at its marker"
  (let ((buf (niwa-test--note-buffer "marker-boundary" "one\ntwo\nthree\n")))
    (niwa-buddy-note-capture-markers '(("marker-boundary" . (2))))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line 1)
      (insert "inserted\n"))
    (niwa-buddy-note-add
     (buffer-name buf) 2 "about two" "significant")
    (with-current-buffer buf
      (should (equal "two"
                     (buffer-substring-no-properties
                      (overlay-start (car (niwa-test--note-overlays buf)))
                      (overlay-end (car (niwa-test--note-overlays buf))))))))

  :doc "a note stays on a shown line when that whole line is replaced"
  (let ((buf (niwa-test--note-buffer "marker-replaced" "one\ntwo\nthree\n")))
    (niwa-buddy-note-capture-markers '(("marker-replaced" . (2))))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line 1)
      (delete-region (point) (line-beginning-position 2))
      (insert "changed\n"))
    (niwa-buddy-note-add
     (buffer-name buf) 2 "about the replacement" "significant")
    (with-current-buffer buf
      (should (equal "changed"
                     (buffer-substring-no-properties
                      (overlay-start (car (niwa-test--note-overlays buf)))
                      (overlay-end (car (niwa-test--note-overlays buf))))))))

  :doc "a note is rejected when its shown line was deleted"
  (let ((buf (niwa-test--note-buffer "marker-deleted" "one\ntwo\nthree\n")))
    (niwa-buddy-note-capture-markers '(("marker-deleted" . (2))))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line 1)
      (delete-region (point) (line-beginning-position 2)))
    (should (string-match-p
             "not shown"
             (niwa-buddy-note-add
              (buffer-name buf) 2 "about two" "significant")))
    (should-not (niwa-test--note-overlays buf)))

  :doc "`niwa-buddy-note-capture-markers' marks only the lines it is given"
  (let ((_buf (niwa-test--note-buffer "marker-few" "one\ntwo\nthree\n")))
    (niwa-buddy-note-capture-markers (list (cons "marker-few" '(2))))
    (should (= 1 (length (cdr (assoc "marker-few"
                                     niwa-buddy-note--markers))))))

  :doc "`niwa-buddy-note-release-markers' unsets every marker"
  (let ((_buf (niwa-test--note-buffer "marker-release" "one\ntwo\n")))
    (niwa-buddy-note-capture-markers (list (cons "marker-release" '(1 2))))
    (let ((markers (mapcan (lambda (entry)
                             (list (nth 1 entry) (nth 2 entry)))
                           (cdr (assoc "marker-release"
                                       niwa-buddy-note--markers)))))
      (niwa-buddy-note-release-markers)
      (should (null niwa-buddy-note--markers))
      (should (seq-every-p (lambda (m) (null (marker-buffer m))) markers)))))

;;
;;; Buffer reading

(niwa-deftest niwa-buddy-note-read-buffer
  (:after-each (niwa-test--note-cleanup))
  ,test
  (test)

  :doc "`niwa-buddy-note-read-buffer' numbers the lines it returns"
  (let ((_buf (niwa-test--note-buffer "read-plain" "one\ntwo\nthree\n")))
    (let ((out (niwa-buddy-note-read-buffer "read-plain" 2 3)))
      (should (string-match-p "^ +2  two$" out))
      (should (string-match-p "^ +3  three$" out))
      (should-not (string-match-p "one" out))))

  :doc "`niwa-buddy-note-read-buffer' refuses a buffer outside the scope"
  (let ((_buf (niwa-test--note-buffer "read-scope" "one\n")))
    (let ((niwa-buddy-note--run
           (niwa-buddy-note--make-run
            :scope (list (cons "other-buffer" (current-buffer))))))
      (should (string-match-p
               "not in the review scope"
               (niwa-buddy-note-read-buffer "read-scope" 1 1)))))

  :doc "`niwa-buddy-note-read-buffer' refuses everything with no review"
  (let ((_buf (niwa-test--note-buffer "read-noscope" "one\n")))
    (let ((niwa-buddy-note--run nil))
      (should (string-match-p
               "not in the review scope"
               (niwa-buddy-note-read-buffer "read-noscope" 1 1)))))

  :doc "`niwa-buddy-note-read-buffer' refuses a buffer that is not live"
  (let ((dead (generate-new-buffer "read-dead")))
    (kill-buffer dead)
    (let ((niwa-buddy-note--run
           (niwa-buddy-note--make-run :scope (list (cons "read-dead" dead)))))
      (should (string-match-p
               "Unknown buffer"
               (niwa-buddy-note-read-buffer "read-dead" 1 1)))))

  :doc "`niwa-buddy-note-read-buffer' rejects a replacement with the same name"
  (let ((original (niwa-test--note-buffer "read-reused" "original\n")))
    (kill-buffer original)
    (let ((replacement (generate-new-buffer "read-reused")))
      (push replacement niwa-test--note-buffers)
      (with-current-buffer replacement (insert "replacement\n"))
      (should (string-match-p
               "not in the review scope"
               (niwa-buddy-note-read-buffer "read-reused" 1 1)))))

  :doc "`niwa-buddy-note-read-buffer' requires both bounds"
  (let ((_buf (niwa-test--note-buffer "read-bounds" "one\ntwo\n")))
    (dolist (bounds '((nil 2) (1 nil) (nil nil)))
      (should (string-match-p
               "required"
               (niwa-buddy-note-read-buffer
                "read-bounds" (car bounds) (cadr bounds))))))

  :doc "`niwa-buddy-note-read-buffer' accepts bounds sent as strings"
  ;; The buddy workload commonly resolves to a local model, and those
  ;; send a JSON number as a string often enough to matter.
  (let ((_buf (niwa-test--note-buffer "read-strings" "one\ntwo\nthree\n")))
    (should (string-match-p
             "^ +2  two$"
             (niwa-buddy-note-read-buffer "read-strings" "2" "2"))))

  :doc "`niwa-buddy-note-read-buffer' refuses a reversed range"
  (let ((_buf (niwa-test--note-buffer "read-reversed" "one\ntwo\n")))
    (should (string-match-p
             "comes after"
             (niwa-buddy-note-read-buffer "read-reversed" 2 1))))

  :doc "`niwa-buddy-note-read-buffer' clamps an end past the last line"
  (let ((_buf (niwa-test--note-buffer "read-clamp" "one\ntwo")))
    (let ((out (niwa-buddy-note-read-buffer "read-clamp" 1 999)))
      (should (string-match-p "^ +1  one$" out))
      (should (string-match-p "^ +2  two$" out))
      (should-not (string-match-p "^ +3" out))))

  :doc "`niwa-buddy-note-read-buffer' reports a begin past the last line"
  (let ((_buf (niwa-test--note-buffer "read-past" "one\ntwo")))
    (should (string-match-p
             "only 2 lines"
             (niwa-buddy-note-read-buffer "read-past" 50 60))))

  :doc "`niwa-buddy-note-read-buffer' stops at the read limit"
  (let* ((content (mapconcat (lambda (n) (format "line %d" n))
                             (number-sequence 1 30) "\n"))
         (_buf (niwa-test--note-buffer "read-limit" content))
         (niwa-buddy-note-read-limit 5)
         (out (niwa-buddy-note-read-buffer "read-limit" 1 30)))
    (should (string-match-p "^ +1  line 1$" out))
    (should (string-match-p "^ +5  line 5$" out))
    (should-not (string-match-p "^ +6  line 6$" out))
    (should (string-match-p "Read again from line 6" out)))

  :doc "`niwa-buddy-note-read-buffer' refuses a nonpositive read limit"
  (let ((_buf (niwa-test--note-buffer "read-zero-limit" "one\ntwo\n"))
        (niwa-buddy-note-read-limit 0))
    (should (string-match-p
             "limit must be positive"
             (niwa-buddy-note-read-buffer "read-zero-limit" 1 2))))

  :doc "`niwa-buddy-note-read-buffer' makes the lines it returns annotatable"
  (let ((buf (niwa-test--note-buffer
              "read-annotate" "one\ntwo\nthree\nfour\nfive\n")))
    (niwa-buddy-note-capture-markers '(("read-annotate" . (5))))
    ;; Line 2 was not in the diff, so it cannot carry a note yet.
    (should (string-match-p
             "not shown"
             (niwa-buddy-note-add "read-annotate" 2 "early" "significant")))
    (niwa-buddy-note-read-buffer "read-annotate" 1 3)
    (let ((id (niwa-buddy-note-add
               "read-annotate" 2 "about two" "significant")))
      (should (integerp id))
      (with-current-buffer buf
        (should (= 2 (line-number-at-pos
                      (overlay-start
                       (car (niwa-test--note-overlays buf)))))))))

  :doc "a note on a read line follows its text when lines move above it"
  (let ((buf (niwa-test--note-buffer
              "read-follow" "one\ntwo\nthree\nfour\n")))
    (niwa-buddy-note-capture-markers '(("read-follow" . (4))))
    (niwa-buddy-note-read-buffer "read-follow" 1 3)
    (niwa-buddy-note-add "read-follow" 3 "about three" "significant")
    (with-current-buffer buf
      (goto-char (point-min))
      (insert "zero\n")
      (should (= 4 (line-number-at-pos
                    (overlay-start
                     (car (niwa-test--note-overlays buf))))))))

  :doc "`niwa-buddy-note-read-buffer' keeps the markers a line already has"
  ;; Recapturing would move an existing note's anchor to wherever that
  ;; number points now, which is what markers exist to prevent.
  (let ((_buf (niwa-test--note-buffer "read-remark" "one\ntwo\nthree\n")))
    (niwa-buddy-note-capture-markers '(("read-remark" . (2))))
    (let ((before (assq 2 (cdr (assoc "read-remark"
                                      niwa-buddy-note--markers)))))
      (niwa-buddy-note-read-buffer "read-remark" 1 3)
      (let* ((entry (cdr (assoc "read-remark" niwa-buddy-note--markers)))
             (after (assq 2 entry)))
        (should (= 3 (length entry)))
        (should (eq (nth 1 before) (nth 1 after))))))

  :doc "`niwa-buddy-note-read-buffer' stops before a stale numeric marker"
  (let ((buf (niwa-test--note-buffer
              "read-shifted" "one\ntwo\nthree\n")))
    (niwa-buddy-note-capture-markers '(("read-shifted" . (2))))
    (with-current-buffer buf
      (goto-char (point-min))
      (insert "zero\n"))
    (let ((out (niwa-buddy-note-read-buffer "read-shifted" 2 2)))
      (should (string-match-p "no longer names" out))
      (should-not (string-match-p "^ +2  one$" out)))
    (let ((id (niwa-buddy-note-add
               "read-shifted" 2 "about two" "significant")))
      (should (integerp id))
      (with-current-buffer buf
        (should (= 3 (line-number-at-pos
                      (overlay-start
                       (car (niwa-test--note-overlays buf)))))))))

  :doc "`niwa-buddy-note-read-buffer' returns only lines marked before the ceiling"
  ;; Emacs walks a buffer marker list on every insertion and the user is
  ;; typing throughout, so the marker set must grow with what was edited,
  ;; not with what the model asked to read.
  (let* ((content (mapconcat (lambda (n) (format "line %d" n))
                             (number-sequence 1 20) "\n"))
         (_buf (niwa-test--note-buffer "read-ceiling" content))
         (niwa-buddy-note--marker-ceiling 3)
         (out (niwa-buddy-note-read-buffer "read-ceiling" 1 20)))
    (should (string-match-p "^ +3  line 3$" out))
    (should-not (string-match-p "^ +4  line 4$" out))
    (should (string-match-p "annotatable-line limit" out))
    (should (= 3 (length (cdr (assoc "read-ceiling"
                                     niwa-buddy-note--markers)))))
    (dolist (line '(1 2 3))
      (should (integerp (niwa-buddy-note-add
                         "read-ceiling" line "annotatable" "significant")))))

  :doc "`niwa-buddy-note-read-buffer' refuses once its review is over"
  (let* ((_buf (niwa-test--note-buffer "read-stale" "one\ntwo\n"))
         (stale (niwa-buddy-note-tools (lambda () nil)))
         (read (seq-find (lambda (tool)
                           (equal "read_buffer" (gptel-tool-name tool)))
                         stale)))
    (should (string-match-p
             "review has ended"
             (funcall (gptel-tool-function read) "read-stale" 1 2)))))


;;
;;; Tool schemas

(niwa-deftest niwa-buddy-note-tools
  (:after-each (niwa-test--note-cleanup)
   :doc "`niwa-buddy-note-tools' follows gptel's argument convention")
  ,test
  (test)

  :doc "builds named tools with typed, described, required arguments"
  (let ((tools (niwa-buddy-note-tools (lambda () t))))
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
  (let* ((buf (niwa-test--note-buffer "stale-tool" "one\ntwo\n"))
         (id (niwa-test--add-note buf 1 "belongs to the new review"))
         (stale (niwa-buddy-note-tools (lambda () nil)))
         (remove (seq-find (lambda (tool)
                             (equal "remove_note" (gptel-tool-name tool)))
                           stale)))
    (should (string-match-p "review has ended"
                            (funcall (gptel-tool-function remove) id)))
    (should (niwa-buddy-note--find id))
    (should (niwa-test--note-overlays buf))))

;;
;;; The review run

(niwa-deftest niwa-buddy-note-begin-review
  (:after-each (niwa-test--note-cleanup))
  ,test
  (test)

  :doc "`niwa-buddy-note-begin-review' returns payload, taken notes and hash"
  (let ((_buf (niwa-test--note-buffer "begin-plain" "one\ntwo\n")))
    (setq niwa-buddy-note--run nil)
    (let* ((payload (niwa-buddy-guide-payload (point-min) (point-max)))
           (begun (with-current-buffer _buf
                    (niwa-buddy-note-begin-review
                     '("begin-plain") (niwa-buddy-guide-payload (point-min) (point-max))
                     nil '("scope") nil))))
      (should (string-prefix-p "=== Buffer: " payload))
      (should (string-search "=== Buffer: begin-plain" (plist-get begun :payload)))
      (should (null (plist-get begun :taken-notes)))
      (should (stringp (plist-get begun :review-hash)))
      (should (assoc "begin-plain" (niwa-buddy-note--field 'scope)))
      (should niwa-buddy-note--markers)))

  :doc "`niwa-buddy-note-end-review' clears the run and persists accepted remarks"
  (let ((buf (niwa-test--note-buffer "end-normal" "one\ntwo\n"))
        touched flushed)
    (setq niwa-buddy-note--run nil)
    (with-current-buffer buf
      (niwa-buddy-note-begin-review
       '("end-normal") (niwa-buddy-guide-payload (point-min) (point-max))
       nil '("scope") nil))
    (should (integerp (niwa-buddy-note-add "end-normal" 1 "kept" "trivial")))
    (cl-letf (((symbol-function 'niwa-instruction-store-touch)
               (lambda (workspace) (push workspace touched)))
              ((symbol-function 'niwa-instruction-store-flush)
               (lambda () (setq flushed t))))
      (niwa-buddy-note-end-review))
    (should (equal (list niwa-test--note-workspace) touched))
    (should flushed)
    (should-not niwa-buddy-note--run)
    (should-not niwa-buddy-note--markers)
    (should (niwa-test--note-overlays buf)))

  :doc "`niwa-buddy-note-end-review' cleans up after a partly initialised begin"
  (let ((buf (niwa-test--note-buffer "end-partial" "one\n")))
    (setq niwa-buddy-note--run nil)
    (cl-letf (((symbol-function 'niwa-buddy-note--restore)
               (lambda () (error "Index failed"))))
      (should-error (with-current-buffer buf
                      (niwa-buddy-note-begin-review
                       '("end-partial") "" nil '("scope") nil))))
    (should niwa-buddy-note--run)
    (niwa-buddy-note-end-review)
    (should-not niwa-buddy-note--run)
    (should-not niwa-buddy-note--markers))

  :doc "a call after the review ended is denied, not errored"
  (let ((buf (niwa-test--note-buffer "end-late" "one\ntwo\n")))
    (setq niwa-buddy-note--run nil)
    (with-current-buffer buf
      (niwa-buddy-note-begin-review
       '("end-late") (niwa-buddy-guide-payload (point-min) (point-max))
       'active '("scope") nil))
    (let ((id (niwa-buddy-note-add "end-late" 1 "mine" "trivial")))
      (should (integerp id))
      (niwa-buddy-note-end-review)
      (should (string-match-p "not in the review scope"
                              (niwa-buddy-note-add "end-late" 2 "x" "trivial" "idea")))
      (should (string-match-p "not in the review scope"
                              (niwa-buddy-note-read-buffer "end-late" 1 1)))
      (should (string-match-p "Unknown note\\|not in the review scope"
                              (niwa-buddy-note-update id "y")))
      (should (string-match-p "not in the review scope" (niwa-buddy-note-remove id)))
      (should (string-match-p "changed; review it again"
                              (niwa-buddy-note-looks-done id)))
      (should-not (niwa-buddy-note--admission (current-buffer) 1 "idea")))))

(provide 'test-niwa-buddy-note)
;;; test-niwa-buddy-note.el ends here
