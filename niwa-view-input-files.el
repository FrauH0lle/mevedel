;;; niwa-view-input-files.el -- File input for the view composer -*- lexical-binding: t -*-

;;; Commentary:

;; Owns local file drops and clipboard-image insertion for chat composers.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'dnd)

;; `cl-seq'
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-position "cl-seq" (cl-item cl-seq &rest cl-keys))
(declare-function cl-remove "cl-seq" (cl-item cl-seq &rest cl-keys))

;; `dnd'
(declare-function dnd-get-local-file-name "dnd"
                  (uri &optional must-exist))
(defvar dnd-protocol-alist)

;; `niwa-mention-bindings'
(declare-function niwa-mention-bindings-set
                  "niwa-mention-bindings"
                  (start end binding &optional object))
(autoload 'niwa-mention-bindings-set "niwa-mention-bindings")

;; `niwa-mentions'
(declare-function niwa-mentions-file-paths-in-text
                  "niwa-mentions" (text))
(declare-function niwa-mentions-file-token "niwa-mentions" (path))
(autoload 'niwa-mentions-file-paths-in-text "niwa-mentions")
(autoload 'niwa-mentions-file-token "niwa-mentions")

;; `niwa-resource'
(declare-function niwa-resource-normalize-file-path
                  "niwa-resource" (value &optional directory))
(autoload 'niwa-resource-normalize-file-path "niwa-resource")

;; `niwa-structs'
(declare-function niwa-session-activate-dropped-file-grants
                  "niwa-structs" (session paths))
(declare-function niwa-session-add-dropped-file-grant
                  "niwa-structs" (session path))
(declare-function niwa-session-pop-dropped-file-grants
                  "niwa-structs" (session paths))
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)

;; `niwa-utilities'
(declare-function niwa-diagnostic-message "niwa-utilities" (scope format-string &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format-string &rest args))
(autoload 'niwa-diagnostic-message "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")

;; `niwa-view-composer'
(declare-function niwa-view--ensure-interactive-chat-view
                  "niwa-view-composer" ())
(declare-function niwa-view--session "niwa-view-composer" ())
(declare-function niwa-view-composer-buffer "niwa-view-composer" (&optional view))
(declare-function niwa-view-composer-draft-end "niwa-view-composer" (&optional view))
(declare-function niwa-view-composer-draft-start "niwa-view-composer" (&optional view))
(declare-function niwa-view-composer-in-draft-p "niwa-view-composer" (position &optional view))
(declare-function niwa-view-composer-insert "niwa-view-composer" (text &optional position view))

;; `niwa-view-markdown'
(declare-function niwa-view--normalize-local-file-uri-path
                  "niwa-view-markdown" (path))

;; `niwa-workspace'
(declare-function niwa-workspace-ensure-generated-state-ignored
                  "niwa-workspace" (workspace))
(declare-function niwa-workspace-state-dir "niwa-workspace" (workspace))
(autoload 'niwa-workspace-state-dir "niwa-workspace")

;; `select'
(declare-function gui-get-selection "select" (selection-symbol target-type))

(defcustom niwa-view-clipboard-image-handlers
  (list
   (list (cons :command "wl-paste")
         (cons :save (lambda (file-path)
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (let ((coding-system-for-read 'binary)
                               (exit-code
                                (call-process "wl-paste" nil (list t nil)
                                              nil "--type" "image/png")))
                           (unless (zerop exit-code)
                             (error "Command wl-paste failed with exit code %d"
                                    exit-code))
                           (let ((coding-system-for-write 'binary))
                             (write-region (point-min) (point-max)
                                           file-path nil 'silent)))))))
   (list (cons :command "pngpaste")
         (cons :save (lambda (file-path)
                       (let ((exit-code
                              (call-process "pngpaste" nil nil nil
                                            file-path)))
                         (unless (zerop exit-code)
                           (error "Command pngpaste failed with exit code %d"
                                  exit-code))))))
   (list (cons :command "xclip")
         (cons :save (lambda (file-path)
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (let ((exit-code
                                (call-process "xclip" nil t nil
                                              "-selection" "clipboard"
                                              "-t" "image/png" "-o")))
                           (unless (zerop exit-code)
                             (error "Command xclip failed with exit code %d"
                                    exit-code))
                           (let ((coding-system-for-write 'binary))
                             (write-region (point-min) (point-max)
                                           file-path nil 'silent)))))))
   (list (cons :command "powershell")
         (cons :save (lambda (file-path)
                       (let ((exit-code
                              (call-process
                               "powershell" nil nil nil
                               "-Command"
                               (format "& {(Get-Clipboard -Format image).Save(%s)}"
                                       (shell-quote-argument file-path)))))
                         (unless (zerop exit-code)
                           (error "Command powershell failed with exit code %d"
                                  exit-code)))))))
  "Handlers for saving a clipboard image to a file.
Each handler is an alist with `:command' and `:save'.  The first
handler whose command exists is used by `niwa-view-yank-dwim'."
  :type '(repeat (alist :key-type keyword :value-type sexp))
  :group 'niwa)

(defun niwa-view--insert-dropped-file-mentions (paths)
  "Insert @file mentions for dropped PATHS into the composer."
  (niwa-view--ensure-interactive-chat-view)
  (let ((session (niwa-view--session))
        (composer (niwa-view-composer-buffer))
        tokens expanded-paths)
    (unless session
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No active session for dropped files"))
    (unless composer
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No editable composer for dropped files"))
    (dolist (path paths)
      (let* ((expanded (expand-file-name path))
             (token (niwa-mentions-file-token expanded)))
        (niwa-mention-bindings-set
         0 (length token)
         (list :kind 'file :token token :path expanded)
         token)
        (push token tokens)
        (push expanded expanded-paths)))
    (when tokens
      (let* ((start (niwa-view-composer-draft-start))
             (end (niwa-view-composer-draft-end))
             (pos (if (and (eq (current-buffer) composer)
                           (niwa-view-composer-in-draft-p (point)))
                      (point) end)))
        (with-current-buffer composer
          (save-restriction
            (widen)
            (niwa-view-composer-insert
             (concat (unless (or (= pos start)
                                 (memq (char-before pos) '(?\s ?\t ?\n)))
                       " ")
                     (string-join (nreverse tokens) " ")
                     (unless (or (= pos end)
                                 (memq (char-after pos) '(?\s ?\t ?\n)))
                       " "))
             pos)
            (dolist (path (nreverse expanded-paths))
              (niwa-session-add-dropped-file-grant session path))
            (font-lock-flush start (niwa-view-composer-draft-end))))))))

(defun niwa-view--pop-dropped-file-grants-for-input (input session)
  "Consume SESSION's pending drag/drop grants referenced by INPUT."
  (when session
    (niwa-session-pop-dropped-file-grants
     session
     (niwa-mentions-file-paths-in-text input))))

(defun niwa-view--activate-dropped-file-grants (paths session)
  "Activate exact-file drag/drop grant PATHS for SESSION."
  (when (and session paths)
    (niwa-session-activate-dropped-file-grants session paths)))

(defun niwa-view--dnd-local-file-paths (uris)
  "Return existing regular local file paths from DND URIS.
Directories are ignored; directory-drop expansion is intentionally out
of scope for the composer."
  (let (paths)
    (dolist (uri (ensure-list uris))
      (let ((path (and (stringp uri)
                       (niwa-view--normalize-local-file-uri-path
                        (dnd-get-local-file-name uri nil)))))
        (cond
         ((not path)
          (niwa-diagnostic-message (niwa-diagnostic-scope (current-buffer)) "ignored non-local drop: %s" uri))
         ((not (file-exists-p path))
          (niwa-diagnostic-message (niwa-diagnostic-scope (current-buffer)) "ignored missing dropped file: %s" path))
         ((file-directory-p path)
          (niwa-diagnostic-message (niwa-diagnostic-scope (current-buffer)) "ignored directory drop: %s" path))
         (t
          (push path paths)))))
    (nreverse paths)))

(defun niwa-view--dnd-handle-files (uris action)
  "Handle dropped local file URIS with DND ACTION.
URIS may be a single URI string or a list of URI strings.  Some DND
paths call protocol handlers in the single-URL shape even when the
handler advertises `dnd-multiple-handler'."
  (let ((paths (niwa-view--dnd-local-file-paths uris)))
    (when paths
      (niwa-view--insert-dropped-file-mentions paths)
      (or action 'copy))))

(put 'niwa-view--dnd-handle-files 'dnd-multiple-handler t)

(defun niwa-view--media-dir ()
  "Return the workspace media directory for clipboard images."
  (let* ((session (niwa-view--session))
         (workspace (and session (niwa-session-workspace session))))
    (unless workspace
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No active session for clipboard image"))
    (let ((dir (file-name-concat (niwa-workspace-state-dir workspace)
                                 "state" "media")))
      (make-directory dir t)
      (niwa-workspace-ensure-generated-state-ignored workspace)
      dir)))

(defun niwa-view--clipboard-image-path (dir)
  "Return a fresh clipboard image path under DIR."
  (let* ((stamp (format-time-string "%Y%m%d-%H%M%S"))
         (base (file-name-concat dir (format "clipboard-%s" stamp)))
         (path (concat base ".png"))
         (n 1))
    (while (file-exists-p path)
      (setq path (format "%s-%d.png" base n))
      (cl-incf n))
    path))

(defun niwa-view--save-clipboard-image (&optional no-error)
  "Save a clipboard image under `.niwa/state/media/'.
Return the saved image path.  When NO-ERROR is non-nil, return nil
instead of signaling when no image is available."
  (condition-case err
      (let* ((dir (niwa-view--media-dir))
             (file-path (niwa-view--clipboard-image-path dir))
             (handler (cl-find-if
                       (lambda (entry)
                         (executable-find (alist-get :command entry)))
                       niwa-view-clipboard-image-handlers)))
        (cond
         ((not handler)
          (unless no-error
            (error "No clipboard image utility found")))
         (t
          (condition-case err
              (funcall (alist-get :save handler) file-path)
            (error
             (when (file-exists-p file-path)
               (delete-file file-path))
             (unless no-error
               (signal (car err) (cdr err)))))
          (cond
           ((not (file-exists-p file-path))
            (unless no-error
              (error "Clipboard image file was not created")))
           ((zerop (nth 7 (file-attributes file-path)))
            (delete-file file-path)
            (unless no-error
              (error "No image found in clipboard")))
           (t file-path)))))
    (error
     (unless no-error
       (signal (car err) (cdr err))))))

(defun niwa-view--clipboard-text-only-p ()
  "Return non-nil when the clipboard advertises only text and metadata."
  (let ((targets (ignore-errors (gui-get-selection 'CLIPBOARD 'TARGETS)))
        text)
    (and (vectorp targets)
         (cl-every (lambda (target)
                     (or (memq target '(TARGETS TIMESTAMP MULTIPLE SAVE_TARGETS))
                         (when (or (memq target '(UTF8_STRING STRING TEXT COMPOUND_TEXT text/plain))
                                   (and (symbolp target)
                                        (string-prefix-p "text/plain;" (symbol-name target))))
                           (setq text t))))
                   targets)
         text)))

(put 'niwa-view-yank-dwim 'delete-selection 'yank)
(defun niwa-view-yank-dwim (&optional arg)
  "Yank text, or save a clipboard image and insert it as an `@file'.
ARG is passed through from the interactive prefix."
  (interactive "*P")
  (if-let* (((window-system))
            ;; Avoid workspace I/O (possibly TRAMP) for a known text clipboard.
            ((not (niwa-view--clipboard-text-only-p)))
            (path (niwa-view--save-clipboard-image t)))
      (niwa-view--insert-dropped-file-mentions (list path))
    (yank arg)))

(defun niwa-view--install-dnd ()
  "Install local file drag/drop support in the current view or composer."
  (setq-local dnd-protocol-alist
              (cons '("^file:" . niwa-view--dnd-handle-files)
                    (cl-remove #'niwa-view--dnd-handle-files
                               dnd-protocol-alist :key #'cdr))))

(provide 'niwa-view-input-files)
;;; niwa-view-input-files.el ends here
