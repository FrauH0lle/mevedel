;;; test-mevedel-gptel-bridge.el -- Tests for gptel bridge -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel)
(require 'gptel-transient)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-cockpit)
(require 'mevedel-gptel-bridge)
(require 'mevedel-mentions)
(require 'mevedel-permissions)
(require 'mevedel-structs)
(require 'mevedel-view)
(require 'mevedel-view-history)
(require 'mevedel-workspace)

;; `gptel'
(defvar gptel-model)
(defvar gptel-system-prompt)
(defvar gptel-tools)

;; `gptel-transient'
(declare-function gptel--set-with-scope "ext:gptel-transient"
                  (sym value &optional scope))

;; `transient'
(defvar transient--prefix)
(defvar transient-post-exit-hook)

(defmacro mevedel-gptel-bridge-test--with-buffers (&rest body)
  "Execute BODY with a paired data and view buffer."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-gptel-bridge-root-" t))
          (mevedel-user-dir
           (file-name-as-directory
            (make-temp-file "mevedel-gptel-bridge-user-" t)))
          (workspace (mevedel-workspace-get-or-create
                      'project (format "gptel-bridge-%s" root) root "mevedel"))
          (session (mevedel-session-create "main" workspace))
          (data-buf (generate-new-buffer " *gptel-bridge-data*"))
          (view-buf (generate-new-buffer " *gptel-bridge-view*")))
     (unwind-protect
         (progn
           (mevedel-gptel-bridge--clear-return-state)
           (mevedel-gptel-bridge--cleanup-advice)
           (when (get-buffer "*gptel-prompt*")
             (kill-buffer "*gptel-prompt*"))
           (with-current-buffer data-buf
             (org-mode)
             (setq-local default-directory (file-name-as-directory root))
             (setq-local mevedel--session session)
             (setq-local gptel-model 'gpt-5.5)
             (setq-local gptel-tools '(read edit)))
           (mevedel-view--setup view-buf data-buf)
           ,@body)
       (mevedel-gptel-bridge--clear-return-state)
       (mevedel-gptel-bridge--cleanup-advice)
       (when (get-buffer "*gptel-prompt*")
         (kill-buffer "*gptel-prompt*"))
       (when (buffer-live-p view-buf) (kill-buffer view-buf))
       (when (buffer-live-p data-buf) (kill-buffer data-buf))
       (when (file-directory-p mevedel-user-dir)
         (delete-directory mevedel-user-dir t))
       (when (file-directory-p root)
         (delete-directory root t)))))

(mevedel-deftest mevedel-gptel-bridge-open ()
  ,test
  (test)
  :doc "view-launched bridge runs gptel-menu in the data buffer"
  (mevedel-gptel-bridge-test--with-buffers
    (let ((window (selected-window))
          called-buffer
          called-prompt
          called-window-buffer)
      (with-current-buffer data-buf
        (setq-local gptel-system-prompt "data prompt")
        (setq-local gptel-model 'data-model)
        (setq-local gptel-tools '(data-tool)))
      (with-current-buffer view-buf
        (setq-local gptel-system-prompt "view prompt")
        (setq-local gptel-model 'view-model)
        (setq-local gptel-tools '(view-tool)))
      (unwind-protect
          (progn
            (set-window-buffer window view-buf)
            (cl-letf (((symbol-function 'gptel-menu)
                       (lambda ()
                         (interactive)
                         (setq called-buffer (current-buffer)
                               called-prompt gptel-system-prompt
                               called-window-buffer
                               (window-buffer (selected-window)))
                         (with-current-buffer called-window-buffer
                           (gptel--set-with-scope
                            'gptel-system-prompt "bridge prompt" t)
                           (gptel--set-with-scope
                            'gptel-model 'bridge-model t)
                           (gptel--set-with-scope
                            'gptel-tools '(bridge-tool) t)))))
              (with-current-buffer view-buf
                (mevedel-gptel-bridge-open)))
            (mevedel-gptel-bridge--return-to-view))
        (when (window-live-p window)
          (set-window-buffer window view-buf)))
      (should (eq called-buffer data-buf))
      (should (eq called-window-buffer data-buf))
      (should (equal called-prompt "data prompt"))
      (with-current-buffer data-buf
        (should (equal gptel-system-prompt "bridge prompt"))
        (should (eq gptel-model 'bridge-model))
        (should (equal gptel-tools '(bridge-tool))))
      (with-current-buffer view-buf
        (should (equal gptel-system-prompt "view prompt"))
        (should (eq gptel-model 'view-model))
        (should (equal gptel-tools '(view-tool))))))

  :doc "transient exit restores the launching view window"
  (mevedel-gptel-bridge-test--with-buffers
    (let ((window (selected-window)))
      (unwind-protect
          (progn
            (set-window-buffer window view-buf)
            (cl-letf (((symbol-function 'gptel-menu)
                       (lambda ()
                         (interactive)
                         (set-window-buffer window data-buf))))
              (with-current-buffer view-buf
                (mevedel-gptel-bridge-open)))
            (should (eq (window-buffer window) data-buf))
            (should (memq #'mevedel-gptel-bridge--return-to-view
                          transient-post-exit-hook))
            (mevedel-gptel-bridge--return-to-view)
            (should (eq (window-buffer window) view-buf)))
        (when (window-live-p window)
          (set-window-buffer window view-buf)))))

  :doc "raw data-buffer bridge does not schedule view restoration"
  (mevedel-gptel-bridge-test--with-buffers
    (cl-letf (((symbol-function 'gptel-menu) #'ignore))
      (with-current-buffer data-buf
        (mevedel-gptel-bridge-open)))
    (should-not mevedel-gptel-bridge--return-view-buffer)))

(mevedel-deftest mevedel-gptel-bridge--return-to-view ()
  ,test
  (test)
  :doc "prompt editing keeps restoration pending until data is displayed"
  (mevedel-gptel-bridge-test--with-buffers
    (let ((window (selected-window))
          (prompt-buffer (get-buffer-create "*gptel-prompt*")))
      (unwind-protect
          (progn
            (set-window-buffer window view-buf)
            (mevedel-gptel-bridge--schedule-return-to-view view-buf data-buf)
            (with-current-buffer prompt-buffer
              (mevedel-gptel-bridge--return-to-view))
            (should (eq mevedel-gptel-bridge--return-view-buffer view-buf))
            (should (memq #'mevedel-gptel-bridge--return-to-view
                          transient-post-exit-hook))
            (set-window-buffer window data-buf)
            (mevedel-gptel-bridge--return-to-view)
            (should (eq (window-buffer window) view-buf))
            (should-not mevedel-gptel-bridge--return-view-buffer))
        (when (buffer-live-p prompt-buffer)
          (kill-buffer prompt-buffer))
        (when (window-live-p window)
          (set-window-buffer window view-buf)))))

  :doc "restores non-origin windows changed to the data buffer"
  (mevedel-gptel-bridge-test--with-buffers
    (let ((configuration (current-window-configuration))
          (other-buf (generate-new-buffer " *test-gptel-bridge-other*")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (let* ((left-window (selected-window))
                   (right-window (split-window-right)))
              (set-window-buffer left-window other-buf)
              (set-window-buffer right-window view-buf)
              (select-window right-window)
              (mevedel-gptel-bridge--schedule-return-to-view
               view-buf data-buf)
              (should (eq mevedel-gptel-bridge--return-window
                          right-window))
              (set-window-buffer left-window data-buf)
              (select-window left-window)
              (mevedel-gptel-bridge--return-to-view)
              (should (eq (window-buffer left-window) other-buf))
              (should (eq (window-buffer right-window) view-buf))
              (should (eq (selected-window) right-window))
              (should-not mevedel-gptel-bridge--return-view-buffer)))
        (when (window-configuration-p configuration)
          (set-window-configuration configuration))
        (when (buffer-live-p other-buf)
          (kill-buffer other-buf))))))

(mevedel-deftest mevedel-gptel-bridge--edit-directive-advice ()
  ,test
  (test)
  :doc "edit-directive callback runs in the data buffer"
  (mevedel-gptel-bridge-test--with-buffers
    (let (callback callback-buffer callback-prompt)
      (with-current-buffer data-buf
        (setq-local gptel-system-prompt "data prompt"))
      (with-current-buffer view-buf
        (setq-local gptel-system-prompt "view prompt"))
      (mevedel-gptel-bridge--schedule-return-to-view view-buf data-buf)
      (mevedel-gptel-bridge--edit-directive-advice
       (lambda (&rest args)
         (setq callback (plist-get (cdr args) :callback)))
       'gptel-system-prompt
       :callback
       (lambda (_message)
         (setq callback-buffer (current-buffer)
               callback-prompt gptel-system-prompt)))
      (should callback)
      (with-temp-buffer
        (setq-local gptel-system-prompt "prompt buffer")
        (funcall callback "new prompt"))
      (should (eq callback-buffer data-buf))
      (should (equal callback-prompt "data prompt"))
      (should-not (mevedel-gptel-bridge--active-p))))

  :doc "edit-directive callback restores windows before reopening menu"
  (mevedel-gptel-bridge-test--with-buffers
    (let ((configuration (current-window-configuration))
          (other-buf (generate-new-buffer " *test-gptel-bridge-callback*"))
          callback-message
          callback-buffer
          callback-window)
      (unwind-protect
          (progn
            (delete-other-windows)
            (let* ((left-window (selected-window))
                   (right-window (split-window-right)))
              (set-window-buffer left-window other-buf)
              (set-window-buffer right-window view-buf)
              (with-current-buffer data-buf
                (setq-local gptel-system-prompt "data prompt"))
              (select-window right-window)
              (mevedel-gptel-bridge--schedule-return-to-view
               view-buf data-buf)
              (let* ((args
                      (mevedel-gptel-bridge--edit-directive-args
                       (list 'gptel-system-prompt
                             :callback
                             (lambda (message)
                               (setq callback-message message
                                     callback-buffer (current-buffer)
                                     callback-window (selected-window))
                               (should
                                (equal gptel-system-prompt
                                       "data prompt"))))))
                     (callback (plist-get (cdr args) :callback)))
                (set-window-buffer left-window data-buf)
                (select-window left-window)
                (funcall callback "new prompt")
                (should (equal callback-message "new prompt"))
                (should (eq callback-buffer data-buf))
                (should (eq callback-window right-window))
                (should (eq (window-buffer left-window) other-buf))
                (should (eq (window-buffer right-window) view-buf)))))
        (mevedel-gptel-bridge--clear-return-state)
        (when (window-configuration-p configuration)
          (set-window-configuration configuration))
        (when (buffer-live-p other-buf)
          (kill-buffer other-buf)))))

  :doc "keeps bridge state when the callback opens another transient"
  (mevedel-gptel-bridge-test--with-buffers
    (let (callback)
      (mevedel-gptel-bridge--schedule-return-to-view view-buf data-buf)
      (mevedel-gptel-bridge--edit-directive-advice
       (lambda (&rest args)
         (setq callback (plist-get (cdr args) :callback)))
       'gptel-system-prompt
       :callback
       (lambda (_message) nil))
      (should callback)
      (let ((transient--prefix t))
        (funcall callback "new prompt"))
      (should (mevedel-gptel-bridge--active-p)))))

(mevedel-deftest mevedel-gptel-bridge--cleanup-advice ()
  ,test
  (test)
  :doc "keeps temporary advice while view restoration is pending"
  (mevedel-gptel-bridge-test--with-buffers
    (mevedel-gptel-bridge--schedule-return-to-view view-buf data-buf)
    (mevedel-gptel-bridge--install-advice)
    (mevedel-gptel-bridge--cleanup-advice)
    (should (advice-member-p
             #'mevedel-gptel-bridge--edit-directive-advice
             'gptel--edit-directive))
    (should (memq #'mevedel-gptel-bridge--cleanup-advice
                  transient-post-exit-hook)))

  :doc "removes temporary advice after restoration clears"
  (mevedel-gptel-bridge-test--with-buffers
    (mevedel-gptel-bridge--install-advice)
    (mevedel-gptel-bridge--clear-return-state)
    (mevedel-gptel-bridge--cleanup-advice)
    (should-not
     (advice-member-p
      #'mevedel-gptel-bridge--edit-directive-advice
      'gptel--edit-directive))
    (should-not (memq #'mevedel-gptel-bridge--cleanup-advice
                      transient-post-exit-hook))))

(provide 'test-mevedel-gptel-bridge)

;;; test-mevedel-gptel-bridge.el ends here
