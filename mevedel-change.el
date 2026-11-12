;;; mevedel-change.el --- Checked recorded file changes -*- lexical-binding: t; -*-

;;; Commentary:

;; Prepare a recorded before/after change against current bytes, or its
;; reverse, without writing anything.  Later edits outside the changed line
;; runs survive; overlapping edits produce per-hunk file/line conflicts.
;; Snapshots contain :path, :exists, literal :bytes, and :mode.  A prepared
;; replacement contains :before (the current snapshot) and :after; the writer
;; must recheck :before before mutating, including when using a remote fence.
;;
;; Transaction rollback uses the same preparation in strict mode: only the
;; complete recorded after-state may be restored, preserving ApplyPatch's
;; whole-file freshness contract.  Filesystem and buffer coordination stay
;; with the caller, not with this byte-oriented engine.

;;; Code:

(require 'cl-lib)

(defun mevedel-change-same-p (left right)
  "Return non-nil when LEFT and RIGHT have identical path, bytes and mode."
  (and left right
       (cl-every (lambda (key) (equal (plist-get left key) (plist-get right key)))
                 '(:path :exists :bytes :mode))))

(defun mevedel-change--runs (before after)
  "Return zero-based line runs changing literal bytes BEFORE to AFTER.
Each run is (OLD-START OLD-END NEW-START NEW-END), with exclusive ends."
  (let ((old (make-temp-file "mevedel-change-old-"))
        (new (make-temp-file "mevedel-change-new-"))
        (output (generate-new-buffer " *mevedel change diff*"))
        (default-directory temporary-file-directory)
        (coding-system-for-read 'no-conversion)
        (coding-system-for-write 'no-conversion)
        runs)
    (unwind-protect
        (progn
          (write-region before nil old nil 'silent)
          (write-region after nil new nil 'silent)
          ;; Interactive diff helpers run user major-mode hooks and shell
          ;; commands.  This is an exact byte comparison, not a diff view.
          (let ((status (call-process "diff" nil output nil "-a" "-U0" "--" old new)))
            (unless (memq status '(0 1))
              (error "Diff failed: %s" status)))
          (with-current-buffer output
            (goto-char (point-min))
            (while (re-search-forward
                    "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
                    nil t)
              (let* ((old-count (if (match-string 2)
                                    (string-to-number (match-string 2)) 1))
                     (new-count (if (match-string 4)
                                    (string-to-number (match-string 4)) 1))
                     (old-start (- (string-to-number (match-string 1))
                                   (if (zerop old-count) 0 1)))
                     (new-start (- (string-to-number (match-string 3))
                                   (if (zerop new-count) 0 1))))
                (push (list old-start (+ old-start old-count)
                            new-start (+ new-start new-count)) runs))))
          (unless (or runs (equal before after))
            (error "Diff produced no changed line runs"))
          (nreverse runs))
      (delete-file old)
      (delete-file new)
      (kill-buffer output))))

(defun mevedel-change--overlap-p (start end other-start other-end)
  "Return non-nil when START..END overlaps OTHER-START..OTHER-END.
Empty runs touch both adjacent lines: an insertion at the same gap, or a
replacement reaching a deleted run's gap, must not be guessed through."
  (if (or (= start end) (= other-start other-end))
      (and (<= start other-end) (<= other-start end))
    (and (< start other-end) (< other-start end))))

(defun mevedel-change--line-offsets (bytes)
  "Return a vector of zero-based line starts and the final end in BYTES."
  (let ((offsets (list 0)) (position 0))
    (while (string-match "\n" bytes position)
      (setq position (match-end 0))
      (push position offsets))
    (unless (= position (length bytes)) (push (length bytes) offsets))
    (vconcat (nreverse offsets))))

(defun mevedel-change--merge (base proposed current path)
  "Prepare BASE to PROPOSED over CURRENT literal bytes at PATH.
Return :bytes or :conflicts, plus zero-based current-coordinate :ranges."
  (let ((changes (mevedel-change--runs base proposed))
        (intervening (mevedel-change--runs base current))
        (source-offsets (mevedel-change--line-offsets proposed))
        (current-offsets (mevedel-change--line-offsets current))
        edits conflicts ranges)
    (dolist (change changes)
      (pcase-let ((`(,start ,end ,new-start ,new-end) change)
                  (offset 0)
                  (end-offset 0)
                  (touched nil))
        (dolist (other intervening)
          (pcase-let ((`(,a ,b ,c ,d) other))
            (when (mevedel-change--overlap-p start end a b)
              (push other touched))
            (when (<= b start)
              (setq offset (+ offset (- (- d c) (- b a)))))
            (when (<= b end)
              (setq end-offset (+ end-offset (- (- d c) (- b a)))))))
        (cond
         ((and (= (length touched) 1)
               (= start (nth 0 (car touched)))
               (= end (nth 1 (car touched)))
               (equal (substring proposed (aref source-offsets new-start)
                                 (aref source-offsets new-end))
                      (substring current (aref current-offsets (nth 2 (car touched)))
                                 (aref current-offsets (nth 3 (car touched))))))
          (push (list (nth 2 (car touched)) (nth 3 (car touched))) ranges))
         (touched
            (let ((first (1+ (apply #'min (mapcar (lambda (run) (nth 2 run)) touched))))
                  (last (apply #'max (mapcar (lambda (run) (nth 3 run)) touched))))
              (push (list (min (+ start offset) (1- first))
                          (max (+ end end-offset) last)) ranges)
              (push (list :path path :start-line first :end-line (max first last)
                          :base-start-line (1+ start) :base-end-line (max (1+ start) end))
                    conflicts)))
         (t
          (push (list (+ start offset) (+ end offset)) ranges)
          (push (list (+ start offset) (+ end offset) new-start new-end) edits)))))
    (if conflicts
        (list :conflicts (nreverse conflicts) :ranges (nreverse ranges))
      (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert current)
          ;; Descending coordinates preserve the positions of earlier runs.
          (dolist (edit edits)
            (pcase-let ((`(,start ,end ,new-start ,new-end) edit))
              (delete-region (1+ (aref current-offsets start))
                             (1+ (aref current-offsets end)))
              (goto-char (1+ (aref current-offsets start)))
              (insert (substring proposed (aref source-offsets new-start)
                                 (aref source-offsets new-end)))))
          (list :bytes (buffer-string) :ranges (nreverse ranges))))))

(defun mevedel-change-prepare (before after current &optional reverse strict)
  "Prepare a BEFORE/AFTER recorded change against CURRENT, without writing.
REVERSE prepares its revert.  STRICT requires the entire base snapshot to
match, as transaction rollback does.  Otherwise only overlapping line edits
conflict; unrelated current edits and mode changes survive.  File creation,
deletion and binary changes require the whole base to match.

Return (:before CURRENT :after SNAPSHOT), or (:conflicts PLISTS).  Conflicts
contain :path, :start-line and :end-line (one-based current coordinates).
Text merges also return :ranges, zero-based current line runs for all hunks.
The caller must collect conflicts before writing and recheck each prepared
:before snapshot at mutation time.  No partial merged text is returned."
  (let* ((base (if reverse after before))
         (target (if reverse before after))
         (path (plist-get base :path))
         (conflict (list :conflicts
                         (list (list :path path :start-line 1 :end-line 1)))))
    (unless (and (equal path (plist-get target :path))
                 (equal path (plist-get current :path)))
      (error "Change snapshots name different files"))
    (cond
     ((mevedel-change-same-p base current)
      (list :before current :after target))
     ((or strict
          (not (cl-every (lambda (snapshot) (plist-get snapshot :exists))
                         (list base target current)))
          (cl-some (lambda (snapshot)
                     (string-search (unibyte-string 0) (plist-get snapshot :bytes)))
                   (list base target current)))
      conflict)
     ((and (not (equal (plist-get base :mode) (plist-get target :mode)))
           (not (equal (plist-get base :mode) (plist-get current :mode))))
      conflict)
     (t
      (let ((merged (mevedel-change--merge
                     (plist-get base :bytes) (plist-get target :bytes)
                     (plist-get current :bytes) path)))
        (if (plist-get merged :conflicts) merged
          (list :before current
                :ranges (plist-get merged :ranges)
                :after (list :path path :exists t :bytes (plist-get merged :bytes)
                             :mode (if (equal (plist-get base :mode) (plist-get target :mode))
                                       (plist-get current :mode)
                                     (plist-get target :mode))))))))))

(defun mevedel-change-rollback (snapshots expected read mutate &optional authorize)
  "Restore SNAPSHOTS only from transaction-owned EXPECTED after-states.
READ returns the current snapshot of a path.  MUTATE receives checked
before/after snapshots and performs the replacement, fencing it if needed.
AUTHORIZE, when non-nil, must signal if mutation authority is lost.
Skip unattempted paths and already restored files.  Return failed path/error
pairs; continue restoring the other paths after a refusal or I/O failure."
  (let (failures)
    (dolist (snapshot snapshots (nreverse failures))
      (let* ((path (plist-get snapshot :path))
             (after (cl-find path expected :key (lambda (item) (plist-get item :path))
                             :test #'equal)))
        (when after
          (condition-case err
              (unless (mevedel-change-same-p snapshot (funcall read path))
                (when authorize (funcall authorize))
                (let ((prepared (mevedel-change-prepare snapshot after (funcall read path) t t)))
                  (when (plist-get prepared :conflicts)
                    (error "File changed after the transaction write: %s" path))
                  (funcall mutate after snapshot)))
            (error (push (cons path err) failures))))))))

(provide 'mevedel-change)
;;; mevedel-change.el ends here
