;;; mevedel-resource-capf.el --- Resource-address completion -*- lexical-binding: t; -*-

;;; Commentary:

;; Bounded metadata completion for canonical resource addresses.  Candidates
;; are always plain address strings; completion never binds or reads a target.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `cl-lib'
(declare-function cl-find-if "cl-lib" (predicate sequence &rest args))

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-settled-result
                  "mevedel-agent-control" (record))
(declare-function mevedel-agent-record-conversation-buffer
                  "mevedel-agent-control" (record) t)
(declare-function mevedel-agent-record-conversation-location
                  "mevedel-agent-control" (record) t)

;; `mevedel-resource'
(declare-function mevedel-resource-completion-metadata
                  "mevedel-resource" (context))
(declare-function mevedel-resource-encode-component
                  "mevedel-resource" (value))
(defvar mevedel-resource-supported-schemes)

;; `mevedel-skills-core'
(declare-function mevedel-skill-description "mevedel-skills-core" (skill) t)
(declare-function mevedel-skill-name "mevedel-skills-core" (skill) t)
(declare-function mevedel-skill-source-dir "mevedel-skills-core" (skill) t)

(defvar mevedel--data-buffer)
(defvar mevedel--session)


;;
;;; Candidate helpers

(defun mevedel-resource-capf--session ()
  "Return the session owning the current composer, if any."
  (or (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer)
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))))

(defun mevedel-resource-capf--decode (component metadata &optional allow-separator)
  "Decode canonical COMPONENT, or return nil when it is incomplete.

When ALLOW-SEPARATOR is non-nil, decode a component such as an MCP server
name without treating an encoded slash as a path separator."
  (condition-case nil
      (funcall (plist-get metadata :decode-component)
               component allow-separator)
    (error nil)))

(defun mevedel-resource-capf--result (start end entries)
  "Build a non-exclusive CAPF result for START, END, and ENTRIES."
  (when entries
    (let ((entries (delete-dups entries)))
      (list start end (mapcar #'car entries)
            :exclusive 'no
            :annotation-function
            (let ((annotations entries))
              (lambda (candidate)
                (or (cdr (assoc candidate annotations)) "")))))))

(defun mevedel-resource-capf--directory-files (directory)
  "Return visible immediate entries in DIRECTORY, or nil on failure."
  (condition-case nil
      (when (file-directory-p directory)
        (sort (directory-files directory t "\\`[^.]" t)
              (lambda (left right)
                (string-lessp (file-name-nondirectory left)
                              (file-name-nondirectory right)))))
    (error nil)))

(defun mevedel-resource-capf--address (prefix components)
  "Return PREFIX joined with encoded COMPONENTS."
  (concat prefix
          (unless (or (string-suffix-p "/" prefix)
                      (null components))
            "/")
          (mapconcat #'mevedel-resource-encode-component components "/")))

(defun mevedel-resource-capf--path-entries
    (scheme root tail metadata &optional address-prefix annotation)
  "Complete one directory level below ROOT for SCHEME and TAIL.

TAIL is the path below ADDRESS-PREFIX.  Only directory metadata is
consulted; no candidate file is opened."
  (when (and root (file-directory-p root))
    (let* ((trailing (string-suffix-p "/" tail))
           (parts (if (string-empty-p tail)
                      nil
                    (split-string (if trailing
                                      (substring tail 0 -1)
                                    tail)
                                  "/" nil)))
           (invalid-p (or (string-prefix-p "/" tail)
                          (string-match-p "//" tail)))
           (directory-raw (if trailing parts (butlast parts)))
           (directory-components
            (and (not invalid-p)
                 (mapcar (lambda (component)
                           (mevedel-resource-capf--decode
                            component metadata))
                         directory-raw))))
      (when (and (or (null directory-components)
                     (not (member nil directory-components))))
        (let* ((directory
                (condition-case nil
                    (funcall (plist-get metadata :safe-path)
                             root directory-components)
                  (error nil)))
               (files (and directory
                           (mevedel-resource-capf--directory-files directory)))
               (prefix (or address-prefix (format "%s://" scheme)))
               entries)
          (dolist (file files (nreverse entries))
            (let* ((name (file-name-nondirectory file))
                   (components (append directory-components (list name)))
                   (relative
                    (mapconcat #'mevedel-resource-encode-component
                               components "/"))
                   (address (mevedel-resource-capf--address prefix components))
                   (safe
                    (condition-case nil
                        (funcall (plist-get metadata :safe-path)
                                 root components)
                      (error nil))))
              (when (and safe
                         (not (and (eq scheme 'artifact)
                                   (string= (car components)
                                            ".mevedel-pending-executions")))
                         (string-prefix-p tail relative))
                (push
                 (cons address
                       (format " %s [%s]"
                               (or annotation (format "[%s]" scheme))
                               (if (file-directory-p file) "dir" "file")))
                 entries)))))))))

(defun mevedel-resource-capf--skills (address-tail metadata)
  "Complete exact skills and one package level below ADDRESS-TAIL."
  (let (entries)
    (dolist (entry (plist-get metadata :skills))
      (let* ((skill (plist-get entry :skill))
             (address (plist-get entry :address)))
        (if (string-match-p "/" address-tail)
            (let ((slash (string-match "/" address-tail)))
              (when (string=
                     (substring address-tail 0 slash)
                     (substring address (length "skill://")))
                (setq entries
                      (append
                       (mevedel-resource-capf--path-entries
                        'skill (mevedel-skill-source-dir skill)
                        (substring address-tail (1+ slash)) metadata address
                        (format "[skill] %s"
                                (mevedel-skill-name skill)))
                       entries))))
          (when (string-prefix-p
                 address-tail (substring address (length "skill://")))
            (push
             (cons address
                   (format " [skill] %s%s"
                           (mevedel-skill-name skill)
                           (if-let ((description
                                     (mevedel-skill-description skill)))
                               (format " - %s" description)
                             "")))
             entries)))))
    (nreverse entries)))

(defun mevedel-resource-capf--agents (tail metadata &optional history)
  "Complete retained agent paths for TAIL and METADATA.

History candidates are limited to records with retained conversations."
  (let ((prefix (if history "history://" "agent://"))
        entries)
    (dolist (entry (plist-get metadata :agents))
      (let* ((item (plist-get entry :item))
             (path (plist-get item :path))
             (record (plist-get entry :record))
             (has-history
              (and record
                   (or (and (fboundp
                             'mevedel-agent-record-conversation-buffer)
                            (mevedel-agent-record-conversation-buffer record))
                       (and (fboundp
                             'mevedel-agent-record-conversation-location)
                            (mevedel-agent-record-conversation-location
                             record)))))
             (ready (and record
                         (fboundp 'mevedel-agent-control-settled-result)
                         (mevedel-agent-control-settled-result record)))
             (address (and path (concat prefix (substring path 1)))))
          (when (and address
                     (not (equal path "/root"))
                     (or (not history) has-history)
                     (string-prefix-p tail (substring address (length prefix))))
            (push
             (cons address
                   (format " [%s] %s (%s)"
                           (if history "history" "agent")
                           (or (plist-get item :role) "default")
                           (if ready
                               "ready"
                             (or (plist-get item :activity) "not-ready"))))
             entries))))
    (nreverse entries)))

(defun mevedel-resource-capf--memory (tail metadata)
  "Complete memory root topics one directory level below TAIL."
  (let (entries)
    (when (string-prefix-p tail "root")
      (push (cons "memory://root" " [memory] configured union") entries))
    (dolist (entry (plist-get metadata :memory-roots))
      (let* ((root (plist-get entry :root))
             (key (plist-get entry :key))
             (base (concat "memory://" key))
             (prefix (concat key "/"))
             (relative
              (cond
               ((string-prefix-p prefix tail)
                (substring tail (length prefix)))
               ((string-prefix-p tail prefix) "")
               (t nil))))
          (when relative
            (let ((root-entries
                   (mevedel-resource-capf--path-entries
                    'memory (plist-get root :dir) relative metadata base
                    (format "[memory] %s"
                            (or (plist-get root :label) key)))))
              (dolist (entry root-entries)
                (when (string-prefix-p
                       tail (substring (car entry) (length "memory://")))
                  (push entry entries)))))))
    (nreverse entries)))

(defun mevedel-resource-capf--mcp (tail metadata)
  "Complete MCP servers and advertised resource metadata for TAIL."
  (let ((servers (plist-get metadata :mcp-servers))
        entries)
    (if (not (string-match "/" tail))
        (dolist (server servers (nreverse entries))
          (let* ((name (plist-get server :name))
                 (encoded (and (stringp name)
                               (mevedel-resource-encode-component name)))
                 (address (and encoded (concat "mcp://" encoded))))
            (when (and address (string-prefix-p tail encoded))
              (push
               (cons address
                     (format " [mcp] %s (%s)"
                             name (or (plist-get server :status) "unknown")))
               entries))))
      (let* ((slash (string-match "/" tail))
             (raw-server (substring tail 0 slash))
             (server (mevedel-resource-capf--decode raw-server metadata t))
             (info (cl-find-if
                    (lambda (candidate)
                      (equal server (plist-get candidate :name)))
                    servers))
             (resources (and info (plist-get info :resources)))
             (resource-list (if (vectorp resources)
                                (append resources nil)
                              resources))
             (base (concat "mcp://" raw-server "/"))
             (resource-tail (substring tail (1+ slash))))
        (dolist (resource resource-list (nreverse entries))
          (let* ((uri (plist-get resource :uri))
                 (encoded-uri (and (stringp uri)
                                   (mevedel-resource-encode-component uri)))
                 (address (and encoded-uri (concat base encoded-uri))))
            (when (and address
                       (string-prefix-p resource-tail encoded-uri))
              (push
               (cons address
                     (format " [mcp] %s%s"
                             (or (plist-get resource :name)
                                 (plist-get resource :description)
                                 "resource")
                             (if info
                                 (format " (%s)"
                                         (or (plist-get info :status)
                                             "unknown"))
                               "")))
               entries))))))))

(defun mevedel-resource-capf ()
  "Complete canonical resource addresses at point from bounded metadata."
  (require 'mevedel-resource)
  (let* ((end (point))
         (start (save-excursion
                  (skip-chars-backward "^ \t\n\r")
                  (point)))
         (token (buffer-substring-no-properties start end))
         (session (mevedel-resource-capf--session))
         entries)
    (cond
     ((or (string-empty-p token)
          (string-match-p "\\`[[:alpha:]][[:alnum:]-]*\\'" token))
      (dolist (scheme mevedel-resource-supported-schemes)
        (let ((address (concat (symbol-name scheme) "://")))
          (when (string-prefix-p token address)
            (push (cons address (format " [%s] resource" scheme)) entries))))
      (mevedel-resource-capf--result start end (nreverse entries)))
     ((string-match
       "\\`\\(local\\|artifact\\|skill\\|agent\\|history\\|memory\\|mcp\\)://\\(.*\\)\\'"
       token)
      (let* ((scheme (intern (match-string 1 token)))
             (tail (match-string 2 token))
             (metadata (mevedel-resource-completion-metadata
                        (list :session session))))
        (setq entries
              (pcase scheme
                ((or 'local 'artifact)
                 (mevedel-resource-capf--path-entries
                  scheme (cdr (assq scheme (plist-get metadata :roots)))
                  tail metadata))
                ('skill (mevedel-resource-capf--skills tail metadata))
                ('agent (mevedel-resource-capf--agents tail metadata))
                ('history (mevedel-resource-capf--agents tail metadata t))
                ('memory (mevedel-resource-capf--memory tail metadata))
                ('mcp (mevedel-resource-capf--mcp tail metadata)))))
        (mevedel-resource-capf--result start end entries)))))

(provide 'mevedel-resource-capf)
;;; mevedel-resource-capf.el ends here
