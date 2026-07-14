;;; mevedel-mention-bindings.el --- Atomic mention bindings -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the shared atomic mention text property, binding schemas, lexical
;; validation, safe string copying, and range-aware invalidation after edits.
;; Mention kinds keep ownership of target lookup and request-time expansion.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `mevedel-mentions'
(defvar mevedel-mentions--file-regexp)
(defvar mevedel-mentions--mcp-regexp)


;;
;;; Binding text properties

(defun mevedel-mention-bindings-set (start end binding &optional object)
  "Attach atomic mention BINDING to START..END in OBJECT.
OBJECT is a string or buffer and defaults to the current buffer.  The
binding property is nonsticky at the end so appended text does not
silently join the token."
  (add-text-properties
   start end
   `(mevedel-mention-binding ,(copy-tree binding)
     rear-nonsticky (mevedel-mention-binding))
   object))

(defun mevedel-mention-bindings-ranges (text)
  "Return atomic mention bindings in TEXT in occurrence order.
Each result is a plist with :start, :end, and :binding."
  (let ((position 0)
        ranges)
    (while (< position (length text))
      (let* ((binding (get-text-property
                       position 'mevedel-mention-binding text))
             (end (next-single-property-change
                   position 'mevedel-mention-binding text (length text))))
        (when binding
          (push (list :start position :end end :binding binding) ranges))
        (setq position end)))
    (nreverse ranges)))

(defun mevedel-mention-bindings-copy-text (text)
  "Return TEXT with only atomic mention binding properties retained."
  (let ((copy (substring-no-properties text)))
    (dolist (range (mevedel-mention-bindings-ranges text))
      (mevedel-mention-bindings-set
       (plist-get range :start)
       (plist-get range :end)
       (plist-get range :binding)
       copy))
    copy))


;;
;;; Lexical tokens

(defun mevedel-mention-bindings--skill-candidate-names (raw-name)
  "Return lexical skill-name candidates for RAW-NAME in priority order."
  (let ((name raw-name)
        names)
    (while (> (length name) 0)
      (push name names)
      (let ((last (aref name (1- (length name)))))
        (setq name (if (memq last '(?. ?: ?-))
                       (substring name 0 -1)
                     ""))))
    (nreverse names)))

(defun mevedel-mention-bindings-skill-token-start-p (text start)
  "Return non-nil when START begins a lexical `$skill' token in TEXT."
  (and (>= start 0)
       (< start (length text))
       (eq (aref text start) ?$)
       (or (= start 0)
           (not (string-match-p
                 "[[:alnum:]_]"
                 (char-to-string (aref text (1- start))))))))

(defun mevedel-mention-bindings-skill-token-occurrences (text)
  "Return lexical `$skill' occurrences in TEXT in source order.
Each occurrence contains :start and :candidates.  Candidates retain the
exact punctuation-bearing name first, then trim only trailing punctuation
that is ambiguous with prose."
  (let ((position 0)
        occurrences)
    (while (string-match "\\$\\([A-Za-z0-9_.:-]+\\)" text position)
      (let ((start (match-beginning 0)))
        (when (mevedel-mention-bindings-skill-token-start-p text start)
          (push (list :start start
                      :candidates
                      (mevedel-mention-bindings--skill-candidate-names
                       (match-string 1 text)))
                occurrences)))
      (setq position (match-end 0)))
    (nreverse occurrences)))


;;
;;; Validation

(defun mevedel-mention-bindings--plist-shape-p (binding keys)
  "Return non-nil when BINDING contains exactly KEYS."
  (and (proper-list-p binding)
       (= (length binding) (* 2 (length keys)))
       (cl-every (lambda (key) (plist-member binding key)) keys)))

(defun mevedel-mention-bindings--nonempty-string-p (value)
  "Return non-nil when VALUE is a nonempty string."
  (and (stringp value) (> (length value) 0)))

(defun mevedel-mention-bindings--schema-valid-p (binding)
  "Return non-nil when BINDING has one supported exact schema."
  (let ((kind (and (proper-list-p binding) (plist-get binding :kind)))
        (token (and (proper-list-p binding) (plist-get binding :token))))
    (and (mevedel-mention-bindings--nonempty-string-p token)
         (pcase kind
           ('skill
            (and (mevedel-mention-bindings--plist-shape-p
                  binding '(:kind :token :source-file))
                 (string-match-p
                  "\\`\\$[A-Za-z0-9_.:-]+\\'" token)
                 (file-name-absolute-p
                  (or (plist-get binding :source-file) ""))))
           ('ref
            (and (mevedel-mention-bindings--plist-shape-p
                  binding '(:kind :token :reference-uuid))
                 (string-match-p "\\`@ref:[0-9]+\\'" token)
                 (mevedel-mention-bindings--nonempty-string-p
                  (plist-get binding :reference-uuid))))
           ('file
            (require 'mevedel-mentions)
            (and (mevedel-mention-bindings--plist-shape-p
                  binding '(:kind :token :path))
                 (string-match-p
                  (concat "\\`" mevedel-mentions--file-regexp "\\'")
                  token)
                 (file-name-absolute-p (or (plist-get binding :path) ""))))
           ('mcp
            (require 'mevedel-mentions)
            (and (mevedel-mention-bindings--plist-shape-p
                  binding '(:kind :token :server :uri))
                 (string-match
                  (concat "\\`" mevedel-mentions--mcp-regexp "\\'")
                  token)
                 (equal (match-string 1 token)
                        (plist-get binding :server))
                 (equal (match-string 2 token)
                        (plist-get binding :uri))))
           (_ nil)))))

(defun mevedel-mention-bindings--skill-range-valid-p
    (text start token)
  "Return non-nil when skill TOKEN starts lexically at START in TEXT."
  (when-let* ((occurrence
               (cl-find start
                        (mevedel-mention-bindings-skill-token-occurrences text)
                        :key (lambda (item) (plist-get item :start)))))
    (member (substring token 1)
            (plist-get occurrence :candidates))))

(defun mevedel-mention-bindings--range-valid-p (text start end binding)
  "Return non-nil when BINDING validly describes TEXT START..END."
  (let ((token (and (mevedel-mention-bindings--schema-valid-p binding)
                    (plist-get binding :token))))
    (and token
         (= (- end start) (length token))
         (equal token (substring text start end))
         (or (= start 0)
             (not (string-match-p
                   "[[:alnum:]_]"
                   (char-to-string (aref text (1- start))))))
         (pcase (plist-get binding :kind)
           ('skill
            (mevedel-mention-bindings--skill-range-valid-p
             text start token))
           ('ref
            (or (= end (length text))
                (not (string-match-p
                      "[[:alnum:]_]"
                      (char-to-string (aref text end))))))
           ((or 'file 'mcp)
            (or (= end (length text))
                (string-match-p
                 "[[:space:]]"
                 (char-to-string (aref text end)))))
           (_ nil)))))

(defun mevedel-mention-bindings-valid-p (text)
  "Return non-nil when every atomic mention binding in TEXT is valid."
  (and (stringp text)
       (cl-every
        (lambda (range)
          (mevedel-mention-bindings--range-valid-p
           text
           (plist-get range :start)
           (plist-get range :end)
           (plist-get range :binding)))
        (mevedel-mention-bindings-ranges text))))

(defun mevedel-mention-bindings-starting-at (text start)
  "Return the valid atomic mention binding starting at START in TEXT."
  (let* ((binding (and (< start (length text))
                       (get-text-property
                        start 'mevedel-mention-binding text)))
         (end (and binding
                   (or (next-single-property-change
                        start 'mevedel-mention-binding text (length text))
                       (length text)))))
    (and binding
         (or (= start 0)
             (not (get-text-property
                   (1- start) 'mevedel-mention-binding text)))
         (mevedel-mention-bindings--range-valid-p text start end binding)
         binding)))

(defun mevedel-mention-bindings-at (text start end kind token)
  "Return TEXT's exact KIND binding for TOKEN at START..END, or nil."
  (let ((binding (mevedel-mention-bindings-starting-at text start)))
    (and binding
         (= end (or (next-single-property-change
                     start 'mevedel-mention-binding text (length text))
                    (length text)))
         (eq kind (plist-get binding :kind))
         (equal token (plist-get binding :token))
         binding)))


;;
;;; Live editing

(defun mevedel-mention-bindings--buffer-run-at (position minimum maximum)
  "Return the binding run at POSITION between MINIMUM and MAXIMUM."
  (when (and (>= position minimum) (< position maximum))
    (when-let* ((binding (get-text-property
                          position 'mevedel-mention-binding)))
      (list :start
            (or (previous-single-property-change
                 (1+ position) 'mevedel-mention-binding nil minimum)
                minimum)
            :end
            (or (next-single-property-change
                 position 'mevedel-mention-binding nil maximum)
                maximum)
            :binding binding))))

(defun mevedel-mention-bindings-invalidate-edit
    (start end minimum maximum)
  "Invalidate mention bindings touched by an edit from START to END.
MINIMUM and MAXIMUM delimit the editable composer.  Only property runs
adjacent to the changed range are inspected."
  (let ((before start)
        runs)
    (while (and (> before minimum)
                (memq (char-before before) '(?. ?: ?-)))
      (setq before (1- before)))
    (dolist (position (delete-dups
                       (list (1- before)
                             (1- start) start (1- end) end)))
      (when-let* ((run (mevedel-mention-bindings--buffer-run-at
                        position minimum maximum)))
        (unless (cl-find run runs :test #'equal)
          (push run runs))))
    (dolist (run runs)
      (let ((run-start (plist-get run :start))
            (run-end (plist-get run :end)))
        (unless (mevedel-mention-bindings--range-valid-p
                 (buffer-substring minimum maximum)
                 (- run-start minimum)
                 (- run-end minimum)
                 (plist-get run :binding))
          (remove-text-properties
           run-start run-end
           '(mevedel-mention-binding nil rear-nonsticky nil)))))))

(provide 'mevedel-mention-bindings)
;;; mevedel-mention-bindings.el ends here
