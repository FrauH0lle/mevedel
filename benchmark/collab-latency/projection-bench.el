;;; projection-bench.el --- Publication cost as transcripts grow -*- lexical-binding: t; -*-

;;; Commentary:

;; A room publish projects the transcript into records, diffs each record
;; against the previous publication and JSON-encodes the changed ones.
;; This times that work for synthetic transcripts of growing length, each
;; turn a prompt, a streamed response and three tool blocks, after one
;; more streamed sentence -- the steady state while a reply streams.
;; `full' projects without retained state, as a room's first publish does;
;; `retained' projects with the room's per-segment cache, as every later
;; publish does, and must return the same records.
;;
;;   emacs -Q --batch -L SOURCE -L DEPS... -l projection-bench.el

;;; Code:

(require 'mevedel-collaboration-projection)
(require 'mevedel-transcript)

(defun projection-bench--turn (index)
  "Insert synthetic turn INDEX at point."
  (insert (format "Please look at module %d and explain what it does.\n\n" index))
  (insert (propertize (concat (string-join (make-list 12 "The module parses its input, validates the fields and hands the result on to the next stage of the pipeline.") " ") "\n\n")
                      'gptel 'response))
  (dotimes (tool 3)
    (insert "#+begin_tool\n"
            (propertize
             (format "(:name \"Read\" :args (:file_path \"/tmp/m%d-%d.el\"))\n%s\n" index tool
                     (string-join (make-list 40 "(defun example () \"Body line.\" nil)") "\n"))
             'gptel (cons 'tool (format "call-%d-%d" index tool)))
            "#+end_tool\n\n")))

(defun projection-bench--time (fn)
  "Return FN's median wall time over five runs in milliseconds."
  (let (times)
    (dotimes (_ 5)
      (garbage-collect)
      (let ((start (float-time)))
        (funcall fn)
        (push (* 1000 (- (float-time) start)) times)))
    (nth 2 (sort times #'<))))

(defun projection-bench--stream ()
  "Append one streamed sentence to the current transcript."
  (goto-char (point-max))
  (insert (propertize "Another streamed sentence.\n" 'gptel 'response)))

(dolist (turns '(10 50 100 200 400))
  (with-temp-buffer
    (mevedel--transcript-org-mode)
    (dotimes (index turns) (projection-bench--turn index))
    (let* ((cache (mevedel-collaboration--projection-cache-create))
           (old (mevedel-collaboration--canonical-records
                 (current-buffer) nil nil cache))
           (_ (projection-bench--stream))
           (full (projection-bench--time
                  (lambda () (mevedel-collaboration--canonical-records (current-buffer)))))
           (retained (projection-bench--time
                      (lambda ()
                        (projection-bench--stream)
                        (mevedel-collaboration--canonical-records
                         (current-buffer) nil nil cache))))
           (new (mevedel-collaboration--canonical-records (current-buffer)))
           (diff (projection-bench--time
                  (lambda ()
                    (let ((reused (mevedel-collaboration--reuse-record-ids old new))
                          (by-id (make-hash-table :test #'equal))
                          (changed 0))
                      (dolist (record old) (puthash (plist-get record :id) record by-id))
                      (dolist (record reused)
                        (unless (equal (gethash (plist-get record :id) by-id) record)
                          (setq changed (1+ changed))))
                      changed)))))
      (unless (equal new (mevedel-collaboration--canonical-records
                          (current-buffer) nil nil cache))
        (error "Retained projection differs from a full projection"))
      (princ (format "turns=%4d chars=%8d records=%5d full=%7.1fms retained=%6.1fms diff=%5.1fms\n"
                     turns (buffer-size) (length new) full retained diff)))))

;;; projection-bench.el ends here
