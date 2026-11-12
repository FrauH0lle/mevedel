;;; test-mevedel-session-persistence-retired-preset.el -- Removed preset resume -*- lexical-binding: t -*-

;;; Commentary:
;; A session saved under the removed `mevedel-discuss' preset must reopen on
;; the ordinary preset with a one-line notice instead of blocking requests.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'mevedel-recovery)

(mevedel-deftest mevedel-session-persistence-resume-id/retired-discuss-preset
  (:doc "reopens a saved mevedel-discuss session on the ordinary preset")
  (let* ((root (make-temp-file "mevedel-retired-preset-" t))
         (mevedel-user-dir (file-name-concat root "user"))
         (mevedel-preset--registry nil)
         (gptel--known-presets (copy-tree gptel--known-presets))
         (gptel--known-tools (copy-tree gptel--known-tools))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "retired-preset" workspace))
         (owner (generate-new-buffer " *retired-preset-owner*"))
         restored restored-session session-id echoed)
    (unwind-protect
        (progn
          (mevedel-tools-register)
          (mevedel--define-presets)
          (should-not (assq 'mevedel-discuss mevedel-preset--registry))
          ;; Model a file written before the preset was removed: the real save
          ;; path persists the selected preset name into the sidecar.
          (setf (mevedel-session-preset-name session) 'mevedel-discuss)
          (with-current-buffer owner
            (org-mode)
            (setq-local mevedel--session session
                        mevedel--workspace workspace
                        default-directory (file-name-as-directory root))
            (insert "Discuss the design\n"
                    (propertize "Earlier answer\n" 'gptel 'response))
            (mevedel-session-artifacts-save session owner))
          (setq session-id (mevedel-session-session-id session))
          (let ((sidecar (car (read-from-string
                               (mevedel-session-artifacts-read-artifact
                                session "session.meta.el" t)))))
            (should (eq 'mevedel-discuss (plist-get sidecar :preset-name))))
          (test-mevedel-session-persistence--release-and-kill owner session)
          (setq owner nil)
          ;; Reopen from committed artifacts through the production resume path.
          (mevedel-test--with-captured-diagnostics echoed
            (setq restored (mevedel-session-persistence-resume-id workspace session-id)))
          (should (buffer-live-p restored))
          (with-current-buffer restored
            (setq restored-session mevedel--session)
            (should (string-match-p "Earlier answer" (buffer-string)))
            (should (eq 'mevedel-implement (mevedel-session-preset-name restored-session)))
            (should (eq 'mevedel-implement gptel--preset))
            (should-not (mevedel-recovery-blocker restored-session))
            (should-not (cl-find "preset" (mevedel-session-recovery-issues restored-session)
                                 :key (lambda (issue) (plist-get issue :id))
                                 :test #'equal))
            (mevedel-recovery-assert-ready restored-session))
          ;; Exactly one echoed line tells the user about the replacement.
          (should (= 1 (cl-count-if (lambda (line) (string-search "mevedel-discuss" line))
                                    (split-string echoed "\n" t)))))
      (test-mevedel-session-persistence--release-and-kill owner session)
      (test-mevedel-session-persistence--release-and-kill restored restored-session)
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-session-persistence-retired-preset)
;;; test-mevedel-session-persistence-retired-preset.el ends here
