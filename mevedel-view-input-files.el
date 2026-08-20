;;; mevedel-view-input-files.el -- File input for the view composer -*- lexical-binding: t -*-

;;; Commentary:

;; Owns local file drops and clipboard-image insertion for chat composers.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `cl-seq'
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-position "cl-seq" (cl-item cl-seq &rest cl-keys))

;; `dnd'
(declare-function dnd-get-local-file-name "dnd"
                  (uri &optional must-exist))
(defvar dnd-protocol-alist)

;; `mevedel-mention-bindings'
(declare-function mevedel-mention-bindings-set
                  "mevedel-mention-bindings"
                  (start end binding &optional object))

;; `mevedel-mentions'
(declare-function mevedel-mentions-file-paths-in-text
                  "mevedel-mentions" (text))
(declare-function mevedel-mentions-file-token "mevedel-mentions" (path))

;; `mevedel-structs'
(declare-function mevedel-session-activate-dropped-file-grants
                  "mevedel-structs" (session paths))
(declare-function mevedel-session-add-dropped-file-grant
                  "mevedel-structs" (session path))
(declare-function mevedel-session-pop-dropped-file-grants
                  "mevedel-structs" (session paths))
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)

;; `mevedel-view-composer'
(declare-function mevedel-view--ensure-interactive-chat-view
                  "mevedel-view-composer" ())
(declare-function mevedel-view--input-start "mevedel-view-composer" ())
(declare-function mevedel-view--session "mevedel-view-composer" ())

;; `mevedel-view-markdown'
(declare-function mevedel-view--normalize-local-file-uri-path
                  "mevedel-view-markdown" (path))

;; `mevedel-workspace'
(declare-function mevedel-workspace-ensure-generated-state-ignored
                  "mevedel-workspace" (workspace))
(declare-function mevedel-workspace-state-dir "mevedel-workspace" (workspace))

;; `select'
(declare-function gui-get-selection "select" (selection-symbol target-type))

(defcustom mevedel-view-clipboard-image-handlers
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
                       (when-let* ((targets (and (eq (window-system) 'x)
                                                 (gui-get-selection
                                                  'CLIPBOARD 'TARGETS)))
                                   ((vectorp targets))
                                   ((not (cl-position 'image/png targets))))
                         (error "No image/png in clipboard"))
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
handler whose command exists is used by `mevedel-view-yank-dwim'."
  :type '(repeat (alist :key-type keyword :value-type sexp))
  :group 'mevedel)

(defun mevedel-view--insert-dropped-file-mentions (paths)
  "Insert @file mentions for dropped PATHS into the composer."
  (require 'mevedel-mention-bindings)
  (mevedel-view--ensure-interactive-chat-view)
  (let ((session (mevedel-view--session))
        tokens)
    (unless session
      (user-error "No active session for dropped files"))
    (dolist (path paths)
      (let* ((expanded (expand-file-name path))
             (token (mevedel-mentions-file-token expanded)))
        (mevedel-mention-bindings-set
         0 (length token)
         (list :kind 'file :token token :path expanded)
         token)
        (push token tokens)
        (mevedel-session-add-dropped-file-grant session expanded)))
    (setq tokens (nreverse tokens))
    (when tokens
      (when (< (point) (mevedel-view--input-start))
        (goto-char (point-max)))
      (unless (or (= (point) (mevedel-view--input-start))
                  (memq (char-before) '(?\s ?\t ?\n)))
        (insert " "))
      (insert (string-join tokens " "))
      (unless (or (eobp) (memq (char-after) '(?\s ?\t ?\n)))
        (insert " "))
      (font-lock-flush (mevedel-view--input-start) (point-max)))))

(defun mevedel-view--mentioned-file-paths (input)
  "Return expanded @file paths mentioned in INPUT."
  (require 'mevedel-mentions)
  (require 'mevedel-resource)
  (mevedel-mentions-file-paths-in-text input))

(defun mevedel-view--pop-dropped-file-grants-for-input (input session)
  "Consume SESSION's pending drag/drop grants referenced by INPUT."
  (when session
    (mevedel-session-pop-dropped-file-grants
     session
     (mevedel-view--mentioned-file-paths input))))

(defun mevedel-view--activate-dropped-file-grants (paths session)
  "Activate exact-file drag/drop grant PATHS for SESSION."
  (when (and session paths)
    (mevedel-session-activate-dropped-file-grants session paths)))

(defun mevedel-view--dnd-local-file-paths (uris)
  "Return existing regular local file paths from DND URIS.
Directories are ignored; directory-drop expansion is intentionally out
of scope for the composer."
  (let (paths)
    (dolist (uri (ensure-list uris))
      (let ((path (and (stringp uri)
                       (mevedel-view--normalize-local-file-uri-path
                        (dnd-get-local-file-name uri nil)))))
        (cond
         ((not path)
          (message "mevedel: ignored non-local drop: %s" uri))
         ((not (file-exists-p path))
          (message "mevedel: ignored missing dropped file: %s" path))
         ((file-directory-p path)
          (message "mevedel: ignored directory drop: %s" path))
         (t
          (push path paths)))))
    (nreverse paths)))

(defun mevedel-view--dnd-handle-files (uris action)
  "Handle dropped local file URIS with DND ACTION.
URIS may be a single URI string or a list of URI strings.  Some DND
paths call protocol handlers in the single-URL shape even when the
handler advertises `dnd-multiple-handler'."
  (let ((paths (mevedel-view--dnd-local-file-paths uris)))
    (when paths
      (mevedel-view--insert-dropped-file-mentions paths)
      (or action 'copy))))

(put 'mevedel-view--dnd-handle-files 'dnd-multiple-handler t)

(defun mevedel-view--media-dir ()
  "Return the workspace media directory for clipboard images."
  (require 'mevedel-workspace)
  (let* ((session (mevedel-view--session))
         (workspace (and session (mevedel-session-workspace session))))
    (unless workspace
      (user-error "No active session for clipboard image"))
    (let ((dir (file-name-concat (mevedel-workspace-state-dir workspace)
                                 "media")))
      (make-directory dir t)
      (mevedel-workspace-ensure-generated-state-ignored workspace)
      dir)))

(defun mevedel-view--clipboard-image-path (dir)
  "Return a fresh clipboard image path under DIR."
  (let* ((stamp (format-time-string "%Y%m%d-%H%M%S"))
         (base (file-name-concat dir (format "clipboard-%s" stamp)))
         (path (concat base ".png"))
         (n 1))
    (while (file-exists-p path)
      (setq path (format "%s-%d.png" base n))
      (cl-incf n))
    path))

(defun mevedel-view--save-clipboard-image (&optional no-error)
  "Save a clipboard image under `.mevedel/media/'.
Return the saved image path.  When NO-ERROR is non-nil, return nil
instead of signaling when no image is available."
  (condition-case err
      (let* ((dir (mevedel-view--media-dir))
             (file-path (mevedel-view--clipboard-image-path dir))
             (handler (cl-find-if
                       (lambda (entry)
                         (executable-find (alist-get :command entry)))
                       mevedel-view-clipboard-image-handlers)))
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

(put 'mevedel-view-yank-dwim 'delete-selection 'yank)
(defun mevedel-view-yank-dwim (&optional arg)
  "Yank text, or save a clipboard image and insert it as an `@file'.
ARG is passed through from the interactive prefix."
  (interactive "*P")
  (if-let* (((window-system))
            (path (mevedel-view--save-clipboard-image t)))
      (mevedel-view--insert-dropped-file-mentions (list path))
    (yank arg)))

(defun mevedel-view--install-dnd ()
  "Install local file drag/drop support for the current view buffer."
  (require 'dnd)
  (let (rest)
    (dolist (entry dnd-protocol-alist)
      (unless (eq (cdr entry) 'mevedel-view--dnd-handle-files)
        (push entry rest)))
    (setq-local dnd-protocol-alist
                (cons '("^file:" . mevedel-view--dnd-handle-files)
                      (nreverse rest)))))

(provide 'mevedel-view-input-files)
;;; mevedel-view-input-files.el ends here
