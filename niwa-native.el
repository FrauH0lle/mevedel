;;; niwa-native.el --- Optional native module construction -*- lexical-binding: t -*-

;;; Commentary:

;; niwa's optional native module serves PGTK frames on Wayland: cheaper
;; presentation (`niwa-present') and independent animation surfaces
;; (`niwa-view-native').  It also gives telemetry timed condition waits
;; that dispatch no events (`niwa-telemetry').  Every C source under native/ builds into one
;; library, without shared headers: package recipes list native/*.c.
;; Loading prefers a downloaded prebuilt module that matches the checksum
;; pinned in native/prebuilt.eld, then a cached local build, then a new
;; local build.  A failure is reported once per session.

;;; Code:

(require 'cl-lib)
(require 'niwa-structs)
(require 'niwa-utilities)

;; `url'
(declare-function url-retrieve-synchronously "url" (url &optional silent inhibit-cookies timeout))
(defvar url-http-response-status)

(defconst niwa-native--directory
  (niwa-library-source-directory (or load-file-name buffer-file-name))
  "Package source directory, including its native module sources.")

(defvar niwa-native--load-state nil
  "Native module loading result: nil, ready, or an explanatory string.
A failure is not retried in this session; set this to nil to retry.")

(defconst niwa-native--release-url
  "https://github.com/FrauH0lle/niwa/releases/download/native-%s/niwa-native-%s.so"
  "Prebuilt module download, by source hash and architecture.")

(defun niwa-native--include-flags ()
  "Return `-I' flags locating the running Emacs's `emacs-module.h'.
An Emacs installed under its own prefix keeps the header in that prefix's
include directory, and one run from its build tree beside the binary."
  (cl-loop for directory in (list (expand-file-name "../include" invocation-directory)
                                  invocation-directory)
           when (file-exists-p (expand-file-name "emacs-module.h" directory))
           collect (concat "-I" directory)))

(defun niwa-native--sources ()
  "Return the module's C sources in byte order."
  (directory-files (file-name-concat niwa-native--directory "native") t "\\.c\\'"))

(defun niwa-native--source-hash ()
  "Return the first 16 hex digits of the SHA-256 of all module sources.
The release workflow hashes the same files in the same order."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (dolist (file (niwa-native--sources))
      (goto-char (point-max))
      (insert-file-contents-literally file))
    (substring (secure-hash 'sha256 (current-buffer)) 0 16)))

(defun niwa-native--arch ()
  "Return this system's architecture as prebuilt modules name it."
  (car (split-string system-configuration "-")))

(defun niwa-native--pin ()
  "Return the pinned SHA-256 of the prebuilt module for this source and system.
CI builds each source revision and records its checksums in
native/prebuilt.eld; a download that does not match is never loaded."
  (when-let* ((file (file-name-concat niwa-native--directory
                                      "native/prebuilt.eld"))
              ((file-readable-p file))
              (pins (with-temp-buffer
                      (insert-file-contents file)
                      (read (current-buffer)))))
    (cdr (assoc (niwa-native--arch)
                (cdr (assoc (niwa-native--source-hash) pins))))))

(defun niwa-native--prebuilt-path ()
  "Return where a downloaded prebuilt module for this source is kept."
  (file-name-concat niwa-user-dir "native"
                    (format "prebuilt-%s-%s%s" (niwa-native--source-hash)
                            (niwa-native--arch) module-file-suffix)))

(defun niwa-native--verified-prebuilt ()
  "Return the downloaded prebuilt module when it matches its pin, or nil."
  (when-let* ((pin (niwa-native--pin))
              (path (niwa-native--prebuilt-path))
              ((file-exists-p path)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (and (equal pin (secure-hash 'sha256 (current-buffer))) path))))

(defun niwa-native--build ()
  "Return the locally built module, building it first when needed."
  (let* ((hash (with-temp-buffer
                 (insert (niwa-native--source-hash) system-configuration emacs-version)
                 (secure-hash 'sha256 (current-buffer))))
         (directory (file-name-concat niwa-user-dir "native"))
         (module (file-name-concat
                  directory (concat "native-" (substring hash 0 16)
                                    module-file-suffix))))
    (unless (file-exists-p module)
      (unless (and (executable-find "cc") (executable-find "pkg-config"))
        (error "The native module needs cc and pkg-config"))
      (make-directory directory t)
      (let ((temporary (make-temp-file (file-name-concat directory "build-")))
            (default-directory directory))
        (unwind-protect
            (with-temp-buffer
              (unless (zerop (call-process
                              "pkg-config" nil t nil "--cflags" "--libs"
                              "gtk+-3.0" "wayland-client"))
                (error "GTK/Wayland development files are unavailable"))
              (let ((flags (split-string-and-unquote (buffer-string))))
                (erase-buffer)
                ;; No -Werror: a warning from newer headers must not
                ;; disable the module; development builds use it.
                (unless (zerop (apply #'call-process "cc" nil t nil
                                      "-shared" "-fPIC" "-O2"
                                      (append (niwa-native--include-flags)
                                              (niwa-native--sources)
                                              (list "-o" temporary)
                                              flags '("-lm"))))
                  (error "Native module compilation failed: %s"
                         (string-trim (buffer-string)))))
              (rename-file temporary module t))
          (when (file-exists-p temporary) (delete-file temporary)))))
    module))

(defun niwa-native--report-failure ()
  "Say once why the native module is unavailable and how to enable it.
Without it, PGTK presentation and animation cost several times the CPU."
  (niwa-diagnostic-message 'global "Native module unavailable (%s); PGTK frames use more CPU.  %s"
           niwa-native--load-state
           (if (niwa-native--pin)
               "M-x niwa-native-install downloads a prebuilt module."
             "Install cc, pkg-config and the GTK 3/Wayland development files to build it.")))

(defun niwa-native-load ()
  "Load the optional native module once, at its first use.
A verified prebuilt download comes first, then a cached local build, then
a new local build.  A failure is reported once and not retried.  Return
non-nil when the module is loaded; an Emacs without module support never
tries."
  (when (and (fboundp 'module-load) module-file-suffix
             (not niwa-native--load-state))
    (setq niwa-native--load-state
          (condition-case err
              (progn
                (module-load (or (niwa-native--verified-prebuilt)
                                 (niwa-native--build)))
                'ready)
            (error (error-message-string err))))
    (unless (eq niwa-native--load-state 'ready)
      (niwa-native--report-failure)))
  (eq niwa-native--load-state 'ready))

(defun niwa-native--download (url)
  "Return URL's body as a unibyte string, or signal an error."
  (require 'url)
  (let ((buffer (url-retrieve-synchronously url t t 60)))
    (unless buffer (error "Download failed: %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (unless (eql 200 (bound-and-true-p url-http-response-status))
            (error "Download failed with HTTP status %s"
                   (bound-and-true-p url-http-response-status)))
          (set-buffer-multibyte nil)
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n")
          (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer buffer))))

;;;###autoload
(defun niwa-native-install ()
  "Download the prebuilt native module for this system and load it.
For systems without a C compiler or the GTK 3/Wayland development files.
The download must match the checksum pinned in niwa's source.  With a
module already loaded, the download loads in the next session."
  (interactive)
  (niwa-with-diagnostic-errors 'global
    (unless (and (fboundp 'module-load) module-file-suffix)
      (user-error "This Emacs cannot load modules"))
    (let ((pin (or (niwa-native--pin)
                   (user-error "No prebuilt module for %s at this niwa revision"
                               (niwa-native--arch))))
          (path (niwa-native--prebuilt-path))
          (url (format niwa-native--release-url
                       (niwa-native--source-hash) (niwa-native--arch))))
      (message "Downloading %s..." url)
      (let ((data (niwa-native--download url)))
        (unless (equal pin (secure-hash 'sha256 data))
          (user-error "Downloaded module does not match its pinned checksum"))
        (make-directory (file-name-directory path) t)
        (let ((temporary (make-temp-file (concat path "-"))))
          (unwind-protect
              (let ((coding-system-for-write 'no-conversion))
                (write-region data nil temporary nil 'silent)
                (rename-file temporary path t))
            (when (file-exists-p temporary) (delete-file temporary)))))
      (cond
       ;; A second module would run beside the loaded one, whose surfaces and
       ;; hooks it cannot see.
       ((eq niwa-native--load-state 'ready)
        (message "Native module installed; it loads in the next Emacs session"))
       ((progn (setq niwa-native--load-state nil) (niwa-native-load))
        (message "Native module installed"))
       (t (user-error "Native module did not load: %s" niwa-native--load-state))))))

(provide 'niwa-native)
;;; niwa-native.el ends here
