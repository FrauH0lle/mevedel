;;; mevedel-memory-library.el --- Current memory inventory and user deletion -*- lexical-binding: t -*-

;;; Commentary:
;; Discover current topic files independently of proposal history.  User
;; deletion shares captured scope, accepted changes, and checked undo with the
;; consolidation pipeline, without invoking a model.

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'mevedel-memory-decision)
(require 'mevedel-report)
(require 'yaml)

(defun mevedel-memory-library--metadata (text)
  "Read inert YAML metadata from bounded topic TEXT, as in gptel-agent."
  (when (string-match "\\`---[ \t]*\n\\(\\(?:.\\|\n\\)*?\\)\n---[ \t]*\\(?:\n\\|\\'\\)" text)
    (yaml-parse-string (match-string 1 text) :object-type 'plist :object-key-type 'keyword)))

(defun mevedel-memory-library-list (workspace)
  "Return current memory topic rows for WORKSPACE, including unindexed files.
Missing roots are empty.  Unavailable roots and incomplete inventories remain
visible.  Discovery neither creates storage nor changes memory content."
  (let (rows seen)
    (dolist (root (mevedel-system--memory-roots workspace))
      (let ((configured (plist-get root :dir)))
        (condition-case err
            (let* ((directory (file-name-as-directory (file-truename configured)))
                   (inventory (mevedel-memory-scope--inventory directory))
                   (index (mevedel-memory-scope--snapshot (file-name-concat directory "MEMORY.md") mevedel-memory-scope--max-file-bytes))
                   (index-text (decode-coding-string (or (plist-get index :bytes) "") 'utf-8-unix)))
              (unless (member directory seen)
                (push directory seen)
                (dolist (file (plist-get inventory :existing))
                  (when (and (mevedel-memory-proposal--file-p file)
                             (not (equal (file-name-nondirectory file) "MEMORY.md"))
                             (not (member file (plist-get inventory :directories))))
                    (let* ((path (file-name-concat directory file))
                           (row (list :kind 'memory :id (list 'memory directory file) :file file
                                      :configured configured :root directory :target path
                                      :client (unless (file-remote-p directory) (mevedel-workspace-identity-client))
                                      :title (file-name-base file) :type ""
                                      :status (if (string-match-p
                                                   (regexp-quote (concat "](" (mapconcat #'url-hexify-string (split-string file "/") "/") ")"))
                                                   index-text) 'indexed 'unindexed))))
                      (condition-case problem
                          (let* ((snapshot (mevedel-memory-scope--snapshot path mevedel-memory-scope--max-file-bytes))
                                 (metadata (mevedel-memory-library--metadata (decode-coding-string (plist-get snapshot :bytes) 'utf-8-unix))))
                            (dolist (pair '((:name . :title) (:type . :type)))
                              (when (stringp (plist-get metadata (car pair)))
                                (setq row (plist-put row (cdr pair) (plist-get metadata (car pair)))))))
                        (error (setq row (plist-put row :error (error-message-string problem)))))
                      (push row rows))))
                (unless (plist-get inventory :complete)
                  (push (list :kind 'memory :id (list 'incomplete directory) :title "Incomplete inventory"
                              :status 'unavailable :target directory :error "Memory inventory reached its entry limit") rows))))
          (error (push (list :kind 'memory :id (list 'unavailable configured) :title "Unavailable memory root"
                             :status 'unavailable :target configured :error (error-message-string err)) rows)))))
    (nreverse rows)))

(defun mevedel-memory-library-path (workspace item)
  "Revalidate WORKSPACE ITEM's original root and return its topic path."
  (unless (and (eq (plist-get item :kind) 'memory) (plist-get item :file))
    (user-error "Select a stored memory"))
  (let ((directory (plist-get item :root)) (client (plist-get item :client)))
    (unless (and (or (not client) (equal client (mevedel-workspace-identity-client)))
                 (seq-some (lambda (root) (equal (plist-get root :dir) (plist-get item :configured)))
                           (mevedel-system--memory-roots workspace))
                 (equal directory (file-name-as-directory (file-truename (plist-get item :configured)))))
      (user-error "Original memory root is unavailable"))
    (let ((path (file-name-concat directory (plist-get item :file))))
      (unless (and (mevedel-memory-proposal--file-p (plist-get item :file))
                   (equal path (file-truename path)))
        (user-error "Memory topic is outside its original root"))
      path)))

(defun mevedel-memory-library-details (workspace item)
  "Return a current-memory report for WORKSPACE ITEM."
  (let* ((path (mevedel-memory-library-path workspace item))
         (snapshot (mevedel-memory-scope--snapshot path mevedel-memory-scope--max-file-bytes)))
    (unless (plist-get snapshot :exists) (user-error "Memory topic no longer exists"))
    (list :title (plist-get item :title) :subtitle path :identity (plist-get item :id)
          :sections (list (list :id 'body :title "Stored memory" :mode 'markdown-mode
                               :body (decode-coding-string (plist-get snapshot :bytes) 'utf-8-unix))
                          (list :id 'location :title "Location" :body (mevedel-report-fields
                                                                    (list "File" path)
                                                                    (list "Index" (plist-get item :status))))))))

(defun mevedel-memory-library-delete (workspace item)
  "Delete WORKSPACE ITEM and its index entry through a checked user decision.
The caller owns interactive confirmation.  Retain the exact transaction for
recovery and reversal, recording `user' as producer without inference."
  (mevedel-memory-library-path workspace item)
  (mevedel-workspace-identity-ensure (mevedel-workspace-root workspace))
  (let* ((scope (mevedel-memory-scope-capture workspace (cons (plist-get item :configured) (plist-get item :file))))
         (root (car (plist-get scope :roots)))
         (claim (or (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)
                    (user-error "Memory review is busy"))))
    (unwind-protect
        (progn
          (unless (and root
                       (equal (plist-get (cdr root) :dir) (plist-get item :root))
                       (equal (plist-get (cdr root) :client) (plist-get item :client))
                       (member (plist-get item :file) (plist-get (cdr root) :files)))
            (user-error "Memory topic or index could not be captured"))
          (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil "User deletion"))
                 (proposal (list :action 'remove :root (car root) :file (plist-get item :file)
                                 :type (if (member (plist-get item :type) '("user" "feedback" "project" "reference"))
                                           (plist-get item :type) "project")
                                 :title (plist-get item :title) :hook "User deletion"
                                 :reason "Deleted by user from memory cockpit." :evidence nil :body ""))
                 (accepted (mevedel-memory-store-accept-proposals
                            workspace prepared (list :proposals (list proposal) :no-action nil) nil "user" nil)))
            (unless accepted (error "Memory deletion lost ownership"))
            (mevedel-memory-store-publish workspace (plist-get prepared :id))
            (mevedel-memory-decision-apply workspace (plist-get prepared :id)
                                           (plist-get (car (plist-get accepted :proposals)) :id))))
      (mevedel-journal-claim-settle claim 'cancelled ""))))

(provide 'mevedel-memory-library)
;;; mevedel-memory-library.el ends here
