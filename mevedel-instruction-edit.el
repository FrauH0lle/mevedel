;;; mevedel-instruction-edit.el --- Line-mapped source rewrites -*- lexical-binding: t; -*-

;;; Commentary:

;; Synchronize visited source buffers in changed line runs.  Character-minimal
;; edits inside each run still preserve ordinary markers, while instruction
;; hooks see the complete replacement, without unchanged hunk context.

;;; Code:

(require 'diff)
(require 'mevedel-directive-source)

(defun mevedel-instruction-edit--line-mapping (beg end text)
  "Map line instructions intersecting BEG..END onto replacement TEXT.
Reserve the new run's prefix and suffix for old lines outside each overlay.
When too few new lines remain to separate them, cover the whole replacement."
  (when (and (< beg end) (not (string-empty-p text)))
    (let* ((first (line-number-at-pos beg))
           (last (line-number-at-pos (1- end)))
           (offsets (list 0))
           (pos 0)
           mappings)
      (while (string-match "\n" text pos)
        (setq pos (match-end 0))
        (push pos offsets))
      (unless (= pos (length text)) (push (length text) offsets))
      (setq offsets (vconcat (nreverse offsets)))
      (let ((count (1- (length offsets))))
        (dolist (overlay (mevedel--instructions-in beg end))
          (when (and (eq (overlay-get overlay 'mevedel-span) 'line)
                     (< (overlay-start overlay) (overlay-end overlay))
                     (< (overlay-start overlay) end)
                     (< beg (overlay-end overlay)))
            (let* ((before (max 0 (- (line-number-at-pos (overlay-start overlay)) first)))
                   (after (max 0 (- last (line-number-at-pos
                                         (1- (overlay-end overlay))))))
                   (middle (- count before after))
                   (start (+ beg (aref offsets (if (< middle 1) 0 before))))
                   (finish (+ beg (aref offsets (if (< middle 1) count (- count after)))))
                   (eol-end (save-excursion (goto-char (overlay-end overlay)) (not (bolp)))))
              (when (< (overlay-start overlay) beg)
                (setq start (overlay-start overlay)))
              (if (> (overlay-end overlay) end)
                  (setq finish (+ (overlay-end overlay) (- (length text) (- end beg))))
                (when (and eol-end (> finish start)
                         (= (aref text (- finish beg 1)) ?\n))
                  (setq finish (1- finish))))
              (push (list overlay start finish) mappings)))))
      mappings)))

(defun mevedel-instruction-replace-buffer-contents (source)
  "Replace current buffer contents with SOURCE using changed line runs.
Instruction overlays receive explicit line-hunk replacements instead of one
whole-buffer change notification.  SOURCE must be a live buffer."
  (save-restriction
    (widen)
    (if (not (mevedel--instructions-in (point-min) (point-max)))
        (replace-region-contents (point-min) (point-max) source)
      (let ((target (current-buffer))
            (diff-buffer (generate-new-buffer " *mevedel instruction diff*"))
            edits)
        (unwind-protect
            (progn
              (diff-no-select target source "-U0" t diff-buffer)
              (with-current-buffer diff-buffer
                (goto-char (point-min))
                (while (re-search-forward
                        "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
                        nil t)
                  (let* ((old-line (string-to-number (match-string 1)))
                         (old-count (if (match-string 2)
                                        (string-to-number (match-string 2)) 1))
                         (new-line (string-to-number (match-string 3)))
                         (new-count (if (match-string 4)
                                        (string-to-number (match-string 4)) 1))
                         (bounds
                          (with-current-buffer target
                            (save-excursion
                              (goto-char (point-min))
                              (forward-line (if (zerop old-count) old-line
                                              (1- old-line)))
                              (let ((start (point)))
                                (forward-line old-count)
                                (cons start (point))))))
                         (text
                          (with-current-buffer source
                            (save-restriction
                              (widen)
                              (save-excursion
                                (goto-char (point-min))
                                (forward-line (if (zerop new-count) new-line
                                                (1- new-line)))
                                (let ((start (point)))
                                  (forward-line new-count)
                                  (buffer-substring-no-properties start (point))))))))
                    (if (and (= old-count new-count) (> old-count 1))
                        ;; Equal-sized replacements have a direct line map;
                        ;; adjacent rewritten lines must not absorb each other.
                        (let ((old-pos (car bounds))
                              (new-pos 0))
                          (dotimes (_ old-count)
                            (let ((old-end
                                   (with-current-buffer target
                                     (save-excursion
                                       (goto-char old-pos)
                                       (line-beginning-position 2))))
                                  (new-end (if (string-match "\n" text new-pos)
                                               (match-end 0) (length text))))
                              (push (list old-pos old-end
                                          (substring text new-pos new-end)) edits)
                              (setq old-pos old-end new-pos new-end))))
                      (push (list (car bounds) (cdr bounds) text) edits)))))
              ;; Descending source coordinates remain valid after each edit.
              (dolist (edit edits)
                (pcase-let ((`(,beg ,end ,text) edit))
                  (let ((mevedel--instruction-line-mapping
                         (mevedel-instruction-edit--line-mapping beg end text)))
                    (combine-change-calls beg end
                      (replace-region-contents beg end (lambda () text)))))))
          (kill-buffer diff-buffer))))))

(provide 'mevedel-instruction-edit)
;;; mevedel-instruction-edit.el ends here
