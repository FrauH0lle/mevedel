;;; niwa-tool-repair-gptel.el --- Lossless gptel argument bridge -*- lexical-binding: t -*-

;;; Commentary:

;; Adapts gptel's response decoding so niwa tool validation can
;; distinguish JSON null from an empty object.

;;; Code:

;; `gptel'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `niwa-chat'
(defvar niwa--session)

;; `niwa-tool-registry'
(declare-function niwa-tool-get "niwa-tool-registry"
                  (name &optional category))

;; `niwa-tool-repair'
(defvar niwa-tool-repair--parsing-response nil)

(defun niwa-tool-repair--preserve-empty-objects (value)
  "Restore empty JSON objects inside decoded VALUE."
  (cond
   ((null value) (make-hash-table))
   ((vectorp value)
    (vconcat (mapcar #'niwa-tool-repair--preserve-empty-objects value)))
   ((listp value)
    (let (result)
      (while value
        (setq result
              (plist-put result (pop value)
                         (niwa-tool-repair--preserve-empty-objects
                          (pop value)))))
      result))
   (t value)))

(defun niwa-tool-repair--with-lossless-json (function &rest args)
  "Call FUNCTION with ARGS while preserving JSON nulls in gptel responses."
  (let* ((info (cl-find-if
                (lambda (arg) (and (listp arg) (plist-member arg :buffer)))
                args))
         (buffer (and info (plist-get info :buffer)))
         (niwa-tool-repair--parsing-response
          (and (buffer-live-p buffer)
               (buffer-local-value 'niwa--session buffer))))
    (apply function args)))

(defun niwa-tool-repair--json-parse-string (function string &rest args)
  "Call FUNCTION on JSON STRING and parser ARGS without lossy gptel nulls."
  (when niwa-tool-repair--parsing-response
    (when-let* ((tail (plist-member args :null-object))
                ((null (cadr tail))))
      (setcar (cdr tail) :null)))
  (apply function string args))

(defun niwa-tool-repair--restore-argument-shapes (fsm)
  "Restore empty objects in niwa tool calls held by FSM."
  (dolist (tool-call (plist-get (gptel-fsm-info fsm) :tool-use))
    (when-let* (((niwa-tool-get (plist-get tool-call :name)))
                (args (plist-get tool-call :args))
                ((listp args)))
      (plist-put tool-call :args
                 (niwa-tool-repair--preserve-empty-objects args)))))

(defconst niwa-tool-repair--shape-advice
  '((gptel--parse-response :around niwa-tool-repair--with-lossless-json)
    (gptel-curl--parse-stream :around niwa-tool-repair--with-lossless-json)
    (json-parse-string :around niwa-tool-repair--json-parse-string)
    (gptel--handle-pre-tool :before niwa-tool-repair--restore-argument-shapes))
  "Native functions, advice positions and lossless argument adapters.")

(defun niwa-tool-repair-install-shape-adapter ()
  "Install the temporary lossless gptel tool-input adapter."
  (dolist (spec niwa-tool-repair--shape-advice)
    (unless (advice-member-p (nth 2 spec) (car spec))
      (advice-add (car spec) (cadr spec) (nth 2 spec)))))

(defun niwa-tool-repair-uninstall-shape-adapter ()
  "Remove the temporary lossless gptel tool-input adapter."
  (dolist (spec niwa-tool-repair--shape-advice)
    (advice-remove (car spec) (nth 2 spec))))

(provide 'niwa-tool-repair-gptel)

;;; niwa-tool-repair-gptel.el ends here
