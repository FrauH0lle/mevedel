;;; test-mevedel-session-artifacts-checkpoint.el --- Save capture tests -*- lexical-binding: t -*-

;;; Commentary:

;; Root transcript capture is shared by synchronous saves and background jobs.

;;; Code:

(require 'mevedel-session-publication-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "mevedel-session-publication-test-helpers"))

(mevedel-deftest mevedel-session-artifacts-prepare-save
  (:quiet t
   :vars ((gptel-advice-installed
           (advice-member-p #'mevedel-session-artifacts--save-gptel-state-around
                            'gptel--save-state)))
   :after-each
   (unless gptel-advice-installed
     (advice-remove 'gptel--save-state
                    #'mevedel-session-artifacts--save-gptel-state-around)))
  ,test
  (test)
  :doc "real transcript save hooks retain a suffix proof across warm checkpoints"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (with-current-buffer buffer
       (mevedel--transcript-org-mode)
       (setq-local mevedel--session session)
       (mevedel-transcript-enable-gptel-mode)
       (mevedel-session-artifacts-install-gptel-save-state-advice)
       (dotimes (index 20)
         (insert (format "Prompt %d\n" index)
                 "#+begin_tool\n"
                 (propertize (format "(:name \"Read\" :args nil)\nOutput %d\n" index)
                             'gptel (cons 'tool (format "warm-%d" index)))
                 "#+end_tool\n"
                 (propertize "Answer\n" 'gptel 'response)))
       (mevedel-session-artifacts-save session buffer)
       (let ((units (symbol-function 'mevedel-session-artifacts--prompt-units))
             observed)
         (cl-letf (((symbol-function 'mevedel-session-artifacts--prompt-units)
                    (lambda (cache origin dirty full)
                      (push (list dirty full (- (point-max) origin)) observed)
                      (funcall units cache origin dirty full))))
           (dotimes (_ 2)
             (goto-char (point-max))
             (insert (propertize "More answer\n" 'gptel 'response))
             (mevedel-session-artifacts-save session buffer)
             (should-not jit-lock-mode)
             (should (> (mevedel-transcript-cache-restart
                         mevedel-transcript--normalization-cache)
                        (/ (buffer-size) 2)))))
         (should observed)
         (dolist (proof observed)
           (should (> (car proof) (/ (nth 2 proof) 2)))
           (should-not (cadr proof)))))))

  :doc "incremental preparation freezes identical bytes, index and candidate to a cold derive"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (with-current-buffer buffer
       (goto-char (point-max))
       (insert "\nPrompt\n#+begin_tool\n"
               (propertize "(:name \"Read\" :args nil)\noutput\n" 'gptel '(tool . "one"))
               "#+end_tool\n"
               (propertize "Answer\n" 'gptel 'response))
       (mevedel-session-artifacts-save session buffer)
       (let ((clock (current-time)))
         (cl-letf (((symbol-function 'current-time) (lambda () clock)))
           (dotimes (iteration 4)
             (pcase iteration
               (0 (goto-char (point-max))
                  (insert (propertize "Appended answer\n" 'gptel 'response)))
               (1 (goto-char (mevedel-session-artifacts-content-start buffer))
                  (insert "Middle prompt edit\n"))
               (2 (delete-region (- (point-max) 8) (point-max)))
               (3 (goto-char (point-max))
                  (insert "<agent-result sender=\"/root/a\">\nResult\n</agent-result>\n")))
             (let ((incremental
                    (mevedel-session-artifacts-call-with-checkpoint
                     session
                     (lambda ()
                       (mevedel-session-artifacts-prepare-save session buffer nil t))))
                   full)
               ;; Drop every derived cache but not committed/checkpoint state.
               ;; The second capture must independently derive the same proof.
               (setq mevedel-transcript--normalized-tick nil
                     mevedel-transcript--normalization-cache nil
                     mevedel-session-artifacts--prompt-cache nil)
               (mevedel-session-bounds--reset)
               (setq full
                     (mevedel-session-artifacts-call-with-checkpoint
                      session
                      (lambda ()
                        (mevedel-session-artifacts-prepare-save session buffer nil t))))
               (should (equal (plist-get incremental :artifacts)
                              (plist-get full :artifacts))))))))))
  :doc "unchanged bytes prepare no publication and finish without changing the head"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let* ((head (plist-get (mevedel-session-publication session) :head))
            (prepared (with-current-buffer buffer
                        (mevedel-session-artifacts-call-with-checkpoint
                         session
                         (lambda ()
                           (mevedel-session-artifacts-prepare-save session buffer))))))
       (should-not (plist-get prepared :artifacts))
       (should (functionp (plist-get prepared :finish)))
       (should (equal (mevedel-session-save-path session)
                      (funcall (plist-get prepared :finish))))
       (should (equal head (plist-get (mevedel-session-publication session) :head))))))

  :doc "capture publishes nothing and finish preserves edits newer than frozen bytes"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((head (plist-get (mevedel-session-publication session) :head)) prepared)
       (with-current-buffer buffer
         (goto-char (point-max))
         (insert (propertize "Frozen answer\n" 'gptel 'response))
         (setq prepared
               (mevedel-session-artifacts-call-with-checkpoint
                session
                (lambda () (mevedel-session-artifacts-prepare-save session buffer))))
         (should (buffer-modified-p))
         (should (equal head (plist-get (mevedel-session-publication session) :head)))
         (let* ((artifacts (plist-get prepared :artifacts))
                (marker (car (last artifacts))))
           (should (equal "session.meta.el"
                          (file-name-nondirectory (plist-get marker :path))))
           (should (plist-get marker :commit-marker))
           (should (plist-get marker :checkpoint))
           (should (seq-some
                    (lambda (artifact)
                      (string-match-p "Frozen answer" (or (plist-get artifact :content) "")))
                    artifacts))
           (goto-char (point-max))
           (insert "Newer edit\n")
           (mevedel-session-publication-publish session artifacts nil t)
           (funcall (plist-get prepared :finish))
           (should (buffer-modified-p))
           (let ((disk (mevedel-session-artifacts-read-artifact
                        session "segment-0001.chat.org" t)))
             (should (string-match-p "Frozen answer" disk))
             (should-not (string-match-p "Newer edit" disk))))))))

  :doc "an exact ACKed character proof skips capture, but an encoding change does not"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (with-current-buffer buffer
       (should mevedel-session-artifacts--committed-transcript-proof)
       (set-buffer-modified-p t)
       (let ((prepared
              (mevedel-session-artifacts-call-with-checkpoint
               session
               (lambda ()
                 (mevedel-session-artifacts-prepare-save session buffer nil nil t)))))
         (should-not (seq-find
                      (lambda (artifact)
                        (equal (plist-get artifact :path) buffer-file-name))
                      (plist-get prepared :artifacts)))
         (should (functionp (plist-get prepared :select-staged))))
       (setq-local buffer-file-coding-system 'utf-8-dos)
       (let* ((prepared
               (mevedel-session-artifacts-call-with-checkpoint
                session
                (lambda ()
                  (mevedel-session-artifacts-prepare-save session buffer nil nil t))))
              (transcript (seq-find
                           (lambda (artifact)
                             (equal (plist-get artifact :path) buffer-file-name))
                           (plist-get prepared :artifacts))))
         (should transcript)
         (should (eq 'utf-8-dos (plist-get transcript :coding)))))))

  :doc "property-only changes serialized into GPTEL_BOUNDS invalidate the byte proof"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (with-current-buffer buffer
       (mevedel--transcript-org-mode)
       (setq-local mevedel--session session)
       (mevedel-transcript-enable-gptel-mode)
       (mevedel-session-artifacts-install-gptel-save-state-advice)
       (goto-char (point-max))
       (insert "\nProperty-sensitive answer\n")
       (mevedel-session-artifacts-save session buffer)
       (goto-char (point-max))
       (search-backward "Property-sensitive answer")
       (let ((tick (buffer-chars-modified-tick)))
         (put-text-property (point) (point-max) 'gptel 'response)
         (should (= tick (buffer-chars-modified-tick)))
         (set-buffer-modified-p t)
         (let* ((prepared
                 (mevedel-session-artifacts-call-with-checkpoint
                  session
                  (lambda ()
                    (mevedel-session-artifacts-prepare-save session buffer nil nil t))))
                (transcript (seq-find
                             (lambda (artifact)
                               (equal (plist-get artifact :path) buffer-file-name))
                             (plist-get prepared :artifacts))))
           (should (> (buffer-chars-modified-tick) tick))
           (should transcript)
           (should (string-match-p ":GPTEL_BOUNDS:"
                                   (plist-get transcript :content))))))))

  :doc "a property-only change after freeze remains dirty until its bounds are saved"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (with-current-buffer buffer
       (mevedel--transcript-org-mode)
       (setq-local mevedel--session session)
       (mevedel-transcript-enable-gptel-mode)
       (mevedel-session-artifacts-install-gptel-save-state-advice)
       (goto-char (point-max))
       (insert "\nPost-freeze property answer\n")
       (let* ((prepared
               (mevedel-session-artifacts-call-with-checkpoint
                session
                (lambda () (mevedel-session-artifacts-prepare-save session buffer))))
              (tick (buffer-chars-modified-tick)))
         (goto-char (point-max))
         (search-backward "Post-freeze property answer")
         (put-text-property (point) (point-max) 'gptel 'response)
         (should (= tick (buffer-chars-modified-tick)))
         (mevedel-session-publication-publish
          session (plist-get prepared :artifacts) nil t)
         (funcall (plist-get prepared :finish))
         (should (buffer-modified-p))
         (mevedel-session-artifacts-save session buffer)
         (should-not (buffer-modified-p))
         (should (string-match-p
                  ":GPTEL_BOUNDS:.*response"
                  (mevedel-session-artifacts-read-artifact
                   session "segment-0001.chat.org" t)))))))

  :doc "a coding change after freeze remains dirty and never certifies unsaved bytes"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (with-current-buffer buffer
       (goto-char (point-max))
       (insert "Frozen UTF-8 answer\n")
       (let* ((proof mevedel-session-artifacts--committed-transcript-proof)
              (prepared
               (mevedel-session-artifacts-call-with-checkpoint
                session
                (lambda () (mevedel-session-artifacts-prepare-save session buffer)))))
         (setq-local buffer-file-coding-system 'utf-8-dos)
         (mevedel-session-publication-publish
          session (plist-get prepared :artifacts) nil t)
         (funcall (plist-get prepared :finish))
         (should (buffer-modified-p))
         (should (eq proof mevedel-session-artifacts--committed-transcript-proof)))))))

(provide 'test-mevedel-session-artifacts-checkpoint)

;;; test-mevedel-session-artifacts-checkpoint.el ends here
