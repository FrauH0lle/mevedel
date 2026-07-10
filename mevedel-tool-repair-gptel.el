;;; mevedel-tool-repair-gptel.el --- Lossless gptel argument bridge -*- lexical-binding: t -*-

;;; Commentary:

;; Adapts gptel's response decoding so mevedel tool validation can
;; distinguish JSON null from an empty object.

;;; Code:

;; `gptel'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `mevedel-chat'
(defvar mevedel--session)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-get "mevedel-tool-registry"
                  (name &optional category))

;; `mevedel-tool-repair'
(defvar mevedel-tool-repair--parsing-response nil)

(defun mevedel-tool-repair--preserve-empty-objects (value)
  "Restore empty JSON objects inside decoded VALUE."
  (cond
   ((null value) (make-hash-table))
   ((vectorp value)
    (vconcat (mapcar #'mevedel-tool-repair--preserve-empty-objects value)))
   ((listp value)
    (let (result)
      (while value
        (setq result
              (plist-put result (pop value)
                         (mevedel-tool-repair--preserve-empty-objects
                          (pop value)))))
      result))
   (t value)))

(defun mevedel-tool-repair--with-lossless-json (function &rest args)
  "Call FUNCTION with ARGS while preserving JSON nulls in gptel responses."
  (let* ((info (cl-find-if
                (lambda (arg) (and (listp arg) (plist-member arg :buffer)))
                args))
         (buffer (and info (plist-get info :buffer)))
         (mevedel-tool-repair--parsing-response
          (and (buffer-live-p buffer)
               (buffer-local-value 'mevedel--session buffer))))
    (apply function args)))

(defun mevedel-tool-repair--json-parse-string (function string &rest args)
  "Call FUNCTION on JSON STRING and parser ARGS without lossy gptel nulls."
  (when mevedel-tool-repair--parsing-response
    (when-let* ((tail (plist-member args :null-object))
                ((null (cadr tail))))
      (setcar (cdr tail) :null)))
  (apply function string args))

(defun mevedel-tool-repair--restore-argument-shapes (fsm)
  "Restore empty objects in mevedel tool calls held by FSM."
  (dolist (tool-call (plist-get (gptel-fsm-info fsm) :tool-use))
    (when-let* (((mevedel-tool-get (plist-get tool-call :name)))
                (args (plist-get tool-call :args))
                ((listp args)))
      (plist-put tool-call :args
                 (mevedel-tool-repair--preserve-empty-objects args)))))

(defun mevedel-tool-repair-install-shape-adapter ()
  "Install the temporary lossless gptel tool-input adapter."
  (dolist (spec '((gptel--parse-response :around
                                          mevedel-tool-repair--with-lossless-json)
                  (gptel-curl--parse-stream :around
                                            mevedel-tool-repair--with-lossless-json)
                  (json-parse-string :around
                                     mevedel-tool-repair--json-parse-string)
                  (gptel--handle-pre-tool :before
                                          mevedel-tool-repair--restore-argument-shapes)))
    (unless (advice-member-p (nth 2 spec) (car spec))
      (advice-add (car spec) (cadr spec) (nth 2 spec)))))

(defun mevedel-tool-repair-uninstall-shape-adapter ()
  "Remove the temporary lossless gptel tool-input adapter."
  (dolist (spec '((gptel--parse-response
                   mevedel-tool-repair--with-lossless-json)
                  (gptel-curl--parse-stream
                   mevedel-tool-repair--with-lossless-json)
                  (json-parse-string mevedel-tool-repair--json-parse-string)
                  (gptel--handle-pre-tool
                   mevedel-tool-repair--restore-argument-shapes)))
    (advice-remove (car spec) (cadr spec))))

(provide 'mevedel-tool-repair-gptel)

;;; mevedel-tool-repair-gptel.el ends here
