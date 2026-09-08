;;; mevedel-memory-reference.el -- Bounded reference observations -*- lexical-binding: t -*-

;;; Commentary:

;; Checks candidate backticked references from admitted memory topics. A found
;; path only establishes occurrence in the workspace, never that its associated
;; lesson, command, flag, or symbol is correct. Unsupported checks stay unknown.
;; This is read-only and does not stamp or rewrite topic files.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-memory-scope)
(require 'mevedel-session-control-fs)

(defconst mevedel-memory-reference--max-checks 200
  "Maximum distinct topic/token observations in one pre-check.")

(defun mevedel-memory-reference--path (token)
  "Return a recognizable workspace-relative path in TOKEN, or nil.
An optional line number suffix is not part of the path being checked."
  (let ((path (replace-regexp-in-string ":[0-9]+\\(?::[0-9]+\\)?\\'" "" token)))
    (and (<= (length path) 256)
         (string-match-p "\\`[[:alnum:]_.][[:alnum:]_./@+-]*\\'" path)
         (not (string-prefix-p "-" path))
         (not (member ".." (split-string path "/")))
         (or (string-search "/" path)
             (string-match-p "[[:alnum:]_]\\.[[:alnum:]]+\\'" path))
         path)))

(defun mevedel-memory-reference--result (scope token)
  "Observe TOKEN's workspace path in SCOPE, returning a closed result string."
  (if-let* ((relative (mevedel-memory-reference--path token)))
      (condition-case nil
          (if (mevedel-session-control-fs-path-exists-p
               (mevedel-memory-scope-source-path scope relative))
              "found" "not found")
        (error "unknown"))
    "unknown"))

(defun mevedel-memory-reference-check (scope)
  "Return bounded reference observations from SCOPE's admitted memory topics.
The result has :references, :complete, and :omissions. Each reference uses
the journal review's closed topic/token/scope/result/checked fields. Only
workspace-relative path existence is checked; unsupported reference kinds
remain unknown. No field certifies semantic correctness or grants authority.

At most 200 distinct topic/token pairs are returned. Oversized tokens and
unavailable topics are omitted explicitly, and reaching the limit marks the
observation incomplete. Topics with no candidates receive no certification."
  (let ((checked (mevedel-journal-store-timestamp nil))
        (seen (make-hash-table :test #'equal))
        (count 0) (complete t) references omissions)
    (catch 'limit
      (dolist (row (plist-get scope :roots))
        (when (eq (plist-get (cdr row) :kind) 'memory)
          (dolist (file (plist-get (cdr row) :files))
            (condition-case nil
                (let* ((topic (concat (car row) "/" file))
                       (snapshot (mevedel-memory-scope-before scope (car row) file))
                       (text (decode-coding-string (plist-get snapshot :bytes) 'utf-8-unix))
                       (position 0))
                  (while (string-match "\\(`+\\)\\([^`\n]+\\)\\1" text position)
                    (let* ((start (match-beginning 0))
                           (end (match-end 0))
                           (token (match-string 2 text))
                           (key (cons topic token)))
                      (setq position end)
                      (unless (or (and (> start 0) (= (aref text (1- start)) ?`))
                                  (and (< end (length text)) (= (aref text end) ?`))
                                  (gethash key seen))
                        (puthash key t seen)
                        (when (>= count mevedel-memory-reference--max-checks)
                          (setq complete nil)
                          (push "Reference count limit reached" omissions)
                          (throw 'limit nil))
                        (cl-incf count)
                        (if (or (> (string-bytes token) 256) (> (string-bytes topic) 4096)
                                (string-match-p "[[:cntrl:]]" (concat topic token)))
                            (progn
                              (setq complete nil)
                              (push "Oversized or non-scalar reference omitted" omissions))
                          (push (list :topic topic :token token
                                      :scope "workspace-relative path existence only; no symbol or command verification"
                                      :result (mevedel-memory-reference--result scope token)
                                      :checked checked)
                                references))))))
              (error
               (setq complete nil)
               (push (format "Unavailable topic in root %s" (car row)) omissions)))))))
    (list :references (nreverse references) :complete complete
          :omissions (delete-dups (nreverse omissions)))))

(provide 'mevedel-memory-reference)
;;; mevedel-memory-reference.el ends here
