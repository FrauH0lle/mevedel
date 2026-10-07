;;; Disposable isolated loading measurement. -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'json)
(setq native-comp-jit-compilation nil
      gc-cons-threshold 800000 gc-cons-percentage 1.0)
(defun lazy-scan-features ()
  (sort (mapcar #'symbol-name
                (cl-remove-if-not
                 (lambda (s) (string-prefix-p "mevedel" (symbol-name s))) features))
        #'string<))
(defun lazy-scan-phase (name action)
  (let ((start (float-time)) (gc-start gc-elapsed) (gc-count gcs-done)
        (before (lazy-scan-features)))
    (funcall action)
    (list :phase name :seconds (- (float-time) start)
          :gc-seconds (- gc-elapsed gc-start) :gcs (- gcs-done gc-count)
          :new-modules (vconcat (cl-set-difference (lazy-scan-features) before :test #'equal))
          :module-count (length (lazy-scan-features))
          :process-count (length (process-list))
          :timer-count (+ (length timer-list) (length timer-idle-list)))))
(let* ((default-directory (file-name-as-directory (make-temp-file "mevedel-load-probe-" t)))
       (user-emacs-directory (file-name-concat default-directory "emacs/"))
       (inhibit-message t)
       (catalog nil)
       (phases nil))
  (unwind-protect
      (progn
        (when lazy-scan-preload
          (push (lazy-scan-phase "gptel" (lambda () (require 'gptel))) phases))
        (push (lazy-scan-phase "require" (lambda () (require 'mevedel))) phases)
        (push (lazy-scan-phase "install" #'mevedel-install) phases)
        (setq catalog
              (secure-hash
               'sha256
               (prin1-to-string
                (sort
                 (mapcar
                  (lambda (tool)
                    (list (mevedel-tool-category tool) (mevedel-tool-name tool)
                          (mevedel-tool-description tool) (mevedel-tool-summary tool)
                          (mevedel-tool-prompt tool) (mevedel-tool-args tool)
                          (mevedel-tool-groups tool) (mevedel-tool-read-only-p tool)
                          (mevedel-tool-snapshot-p tool) (mevedel-tool-destructive-p tool)
                          (mevedel-tool-async-p tool) (mevedel-tool-max-result-size tool)
                          (gptel-tool-args (mevedel-tool-gptel-tool tool))))
                  (mevedel-tool-all))
                 (lambda (a b) (string< (concat (car a) (cadr a))
                                       (concat (car b) (cadr b))))))))
        (if (eq lazy-scan-scenario 'view)
            (progn
              (push (lazy-scan-phase "first-view" #'mevedel) phases)
              (push (lazy-scan-phase "repeat-view" #'mevedel) phases)
              (let* ((file (file-name-concat default-directory "sample.txt"))
                     (buffer (mevedel--active-chat-buffer))
                     (tool (mevedel-tool-get "Read"))
                     (read-file
                      (lambda ()
                        (with-current-buffer buffer
                          (let (result)
                            (funcall (gptel-tool-function (mevedel-tool-gptel-tool tool))
                                     (lambda (value) (setq result value)) file)
                            (with-timeout (5 (error "Read timed out"))
                              (while (not result) (accept-process-output nil 0.01)))
                            (unless (or (string-match-p "measurement evidence" result)
                                        (string-match-p "unchanged since last read" result))
                              (error "Read failed: %s" result)))))))
                (write-region "measurement evidence\n" nil file nil 'silent)
                (push (lazy-scan-phase "first-read" read-file) phases)
                (push (lazy-scan-phase "repeat-read" read-file) phases)))
          (let* ((session (mevedel-session--create :authority-mode 'pid-lock :name "measure"))
                 (tool (mevedel-tool-get "ListExecutions"))
                 (invoke (lambda ()
                           (with-temp-buffer
                             (setq-local mevedel--session session)
                             (let (result)
                               (funcall (gptel-tool-function (mevedel-tool-gptel-tool tool))
                                        (lambda (value) (setq result value)))
                               (unless (and (stringp result) (not (string-prefix-p "Error:" result)))
                                 (error "Tool failed: %S" result)))))))
            (push (lazy-scan-phase "first-tool" invoke) phases)
            (push (lazy-scan-phase "repeat-tool" invoke) phases)))
        (let ((dependencies
               (mapcar
                (lambda (feature)
                  (let ((path (locate-library (symbol-name feature))))
                    (list :name (symbol-name feature) :path path
                          :sha256 (when path (with-temp-buffer
                                               (insert-file-contents-literally path)
                                               (secure-hash 'sha256 (current-buffer)))))))
                '(gptel gptel-request acp yaml orderless websocket qrencode))))
          (with-temp-file lazy-scan-output
            (insert (json-encode
                     (list :emacs emacs-version :gc-threshold gc-cons-threshold
                           :gc-percentage gc-cons-percentage
                           :variant lazy-scan-variant :scenario (symbol-name lazy-scan-scenario)
                           :mode lazy-scan-mode :preloaded (if lazy-scan-preload t :json-false)
                           :entry-library (symbol-file 'mevedel-install 'defun)
                           :catalog-sha256 catalog
                           :dependencies (vconcat dependencies)
                           :phases (vconcat (nreverse phases))))))))
    (dolist (buffer (buffer-list))
      (when (buffer-local-value 'mevedel--session buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))
    (when (fboundp 'mevedel-uninstall) (mevedel-uninstall))
    (delete-directory default-directory t)))
