;;; mevedel-memory-investigation.el -- Read-only consolidation tools -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the short-lived read tools for one consolidation request. Captured
;; evidence cannot be rebound through current memory aliases. Source access
;; stays within the captured workspace boundary. No session is manufactured.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-memory-scope)
(require 'mevedel-tool-fs-read)
(require 'mevedel-tool-fs-search)
(require 'mevedel-utilities)

;; `gptel-request'
(declare-function gptel-make-tool "gptel-request" (&rest slots))
(defvar gptel--known-tools)

;; `mevedel-structs'
(defvar mevedel--session)

(cl-defstruct (mevedel-memory-investigation
               (:constructor mevedel-memory-investigation-create
                             (scope entries currentp exhausted)))
	      "One bounded investigation; CURRENTP and EXHAUSTED belong to its owner.
SCOPE contains captured roots. ENTRIES are the owner's admitted public
journal snapshots. CURRENTP must reject expired generations. EXHAUSTED is
called with a reason when a tool budget prevents valid completion."
	      scope entries currentp exhausted (calls 0) (bytes 0) stopped active)

(defun mevedel-memory-investigation-stop (state)
  "Retire STATE and cancel its active helpers without delivering late results."
  (unless (mevedel-memory-investigation-stopped state)
    (setf (mevedel-memory-investigation-stopped state) t)
    (dolist (cancel (mevedel-memory-investigation-active state)) (funcall cancel))
    (setf (mevedel-memory-investigation-active state) nil)))

(defun mevedel-memory-investigation--live-p (state)
  "Return non-nil while STATE's request still owns its generation."
  (and (not (mevedel-memory-investigation-stopped state))
       (funcall (mevedel-memory-investigation-currentp state))))

(defun mevedel-memory-investigation--exhaust (state reason)
  "Stop STATE and report the exhausted budget REASON exactly once."
  (unless (mevedel-memory-investigation-stopped state)
    (mevedel-memory-investigation-stop state)
    (funcall (mevedel-memory-investigation-exhausted state) reason)))

(defun mevedel-memory-investigation--bounded (text)
  "Return TEXT bounded to 8 KiB of UTF-8 with a visible omission marker."
  (mevedel--truncate-bytes (mevedel--normalize-message-text text) 8192
                           "\n[Result truncated at 8 KiB; narrow the query.]"))

(defun mevedel-memory-investigation--deliver (state callback text)
  "Deliver bounded TEXT to CALLBACK if STATE remains live and within budget."
  (when (mevedel-memory-investigation--live-p state)
    (let ((text (mevedel-memory-investigation--bounded text)))
      (if (> (+ (mevedel-memory-investigation-bytes state) (string-bytes text)) 65536)
          (mevedel-memory-investigation--exhaust state "Tool output budget exhausted")
        (cl-incf (mevedel-memory-investigation-bytes state) (string-bytes text))
        (funcall callback text)))))

(defun mevedel-memory-investigation--relative (path)
  "Validate one tool-relative PATH, accepting dot for a search root."
  (unless (mevedel-memory-proposal-relative-path-p path)
    (error "Invalid investigation path"))
  path)

(defun mevedel-memory-investigation--evidence (state root)
  "Return captured document pairs for ROOT in STATE, checking original authority."
  (if (equal root "journal")
      (mapcar (lambda (entry)
                (cons (plist-get entry :file)
                      (format "Digest: %s\nSession: %s\nCreated: %s\n\n%s"
                              (plist-get entry :id) (plist-get entry :session)
                              (plist-get entry :created) (plist-get entry :body))))
              (mevedel-memory-investigation-entries state))
    (let* ((scope (mevedel-memory-investigation-scope state))
           (descriptor (mevedel-memory-scope--root scope root)))
      (cl-loop for (file . snapshot) in (plist-get descriptor :before)
               when (plist-get snapshot :exists)
               collect (cons file (decode-coding-string (plist-get snapshot :bytes) 'utf-8-unix))))))

(defun mevedel-memory-investigation--documents (state root path contents)
  "Return (DOCUMENTS . PARTIAL) for ROOT and PATH in STATE.
When CONTENTS is nil, snapshot file names only. Source searches inspect at
most 256 entries and copy at most 2 MiB, with a 512 KiB per-file limit.
The caller labels omitted source data; model tools can narrow the subtree."
  (if (not (equal root "workspace"))
      (cons (cl-remove-if-not
             (lambda (document)
               (or (equal path ".") (equal path (car document))
                   (string-prefix-p (file-name-as-directory path) (car document))))
             (mevedel-memory-investigation--evidence state root)) nil)
    (let* ((scope (mevedel-memory-investigation-scope state))
           (pending (list (mevedel-memory-scope-source-path scope path)))
           (count 0) (remaining (* 2 1024 1024)) documents partial)
      (while (and pending (< count 256))
        (unless (mevedel-memory-investigation--live-p state) (error "Investigation retired"))
        (let* ((native (pop pending))
               (relative (file-relative-name native (plist-get scope :workspace-root))))
          (cl-incf count)
          (condition-case nil
              (progn
                (mevedel-memory-scope-source-path scope relative)
                (if (file-directory-p native)
                    (let* ((available (max 0 (- 256 count (length pending))))
                           (children (and (> available 0)
                                          (directory-files native t directory-files-no-dot-files-regexp nil (1+ available)))))
                      (when (or (= available 0) (> (length children) available)) (setq partial t))
                      (setq pending (append (seq-take children available) pending)))
                  (when (file-regular-p native)
                    (let* ((snapshot (and contents (mevedel-memory-scope--snapshot native (min remaining (* 512 1024)))))
                           (bytes (plist-get snapshot :bytes)))
                      (cl-decf remaining (length bytes))
                      (push (cons relative (if contents (decode-coding-string bytes 'utf-8-unix) "")) documents)))))
            (error (setq partial t)))))
      (cons (nreverse documents) (or partial pending)))))

(defun mevedel-memory-investigation--search (state operation args root path callback)
  "Run bounded search OPERATION using ARGS over STATE's ROOT and PATH.
Only admitted copies enter the ordinary helper's read scope. CALLBACK gets
relative filenames; private copy paths are removed from results and errors."
  (let ((directory (make-temp-file "mevedel-memory-search-" t))
        (caller-directory default-directory)
        done cancel stopper)
    (cl-labels
     ((cleanup ()
        (setf (mevedel-memory-investigation-active state)
              (delq stopper (mevedel-memory-investigation-active state)))
        (when (file-exists-p directory) (delete-directory directory t)))
      (finish (text)
        (unless done
          (setq done t)
          (cleanup)
          ;; Empty searches can settle before the snapshot binding unwinds.
          (let ((default-directory caller-directory))
            (mevedel-memory-investigation--deliver
             state callback
             (string-replace directory "."
                             (string-replace (file-name-as-directory directory) "" text))))))
      (abort-search ()
        (unless done
          (setq done t)
          (when (functionp cancel) (funcall cancel))
          (cleanup))))
     (setq stopper #'abort-search)
     (push stopper (mevedel-memory-investigation-active state))
     (condition-case err
         (let* ((captured (mevedel-memory-investigation--documents state root path (eq operation 'grep)))
                (partial (cdr captured))
                (default-directory (file-name-as-directory directory))
                (mevedel-tool-fs-search--teardown #'abort-search)
                (mevedel-tool-fs-search-timeout 20))
           (dolist (document (car captured))
             (let ((file (file-name-concat directory (mevedel-memory-investigation--relative (car document)))))
               (make-directory (file-name-directory file) t)
               (let ((coding-system-for-write 'utf-8-unix))
                 (write-region (cdr document) nil file nil 'silent))))
           (unless (mevedel-memory-investigation--live-p state) (error "Investigation retired"))
           (setq cancel
                 (funcall (if (eq operation 'glob) #'mevedel-tool-fs-search-glob #'mevedel-tool-fs-search-grep)
                          (lambda (result)
                            (finish (concat (when partial "[Partial source snapshot: excluded, unavailable, or bounded paths were not searched. Missing matches remain unknown.]\n")
                                            (plist-get result :result))))
                          (list :path directory :pattern (plist-get args :pattern)
                                :output_mode "content" :head_limit 100 :glob (plist-get args :glob)))))
       (error (finish (concat "Error: " (error-message-string err))))))))

(defun mevedel-memory-investigation-call (state operation args callback)
  "Run read-only OPERATION with ARGS and deliver a string to CALLBACK.
ARGS uses :root (workspace, journal, or a captured root ID), :path relative
to that root, and operation-specific fields. At most 20 calls and 64 KiB
returned text are allowed. Exhaustion stops the owner instead of enabling
another model follow-up. Calls after retirement have no effects or callback."
  (when (mevedel-memory-investigation--live-p state)
    (if (>= (mevedel-memory-investigation-calls state) 20)
        (mevedel-memory-investigation--exhaust state "Tool call budget exhausted")
      (cl-incf (mevedel-memory-investigation-calls state))
      (condition-case err
          (let* ((root (plist-get args :root))
                 (path (mevedel-memory-investigation--relative (or (plist-get args :path) ".")))
                 (scope (mevedel-memory-investigation-scope state))
                 (mevedel--session nil))
            (unless (memq operation '(read glob grep)) (error "Unsupported investigation operation"))
            (if (memq operation '(glob grep))
                (progn
                  (unless (and (stringp (plist-get args :pattern))
                               (<= 1 (length (plist-get args :pattern)) 1024))
                    (error "Invalid investigation search pattern"))
                  (mevedel-memory-investigation--search state operation args root path callback))
              (dolist (key '(:offset :limit))
		(when-let* ((value (plist-get args key)))
                  (unless (and (integerp value) (> value 0)) (error "Invalid Read range"))))
              (let (temporary)
                (unwind-protect
                    (let* ((document
                            (if (equal root "workspace")
                                (let ((snapshot
                                       (mevedel-memory-scope--snapshot
                                        (mevedel-memory-scope-source-path scope path)
                                        (* 512 1024))))
                                  (unless (plist-get snapshot :exists)
                                    (error "Source file is unavailable"))
                                  (decode-coding-string (plist-get snapshot :bytes) 'utf-8-unix))
                              (or (cdr (assoc path (mevedel-memory-investigation--evidence state root)))
                                  (error "File is not in the captured evidence")))))
                      (setq temporary (make-temp-file "mevedel-memory-read-" nil
                                                      (file-name-extension path t)))
                      (let ((coding-system-for-write 'utf-8-unix))
                        (write-region document nil temporary nil 'silent))
                      (mevedel-memory-investigation--deliver
                       state callback
                       (mevedel-tool-fs-read-slurp-file-contents
                        temporary (plist-get args :offset) (plist-get args :limit) path)))
                  (when temporary (delete-file temporary))))))
        (error (mevedel-memory-investigation--deliver
                state callback (concat "Error: " (error-message-string err))))))))

(defun mevedel-memory-investigation-tools (state)
  "Return STATE's three scoped asynchronous gptel tools.
Tool definitions stay request-local; they do not enter gptel's global registry.
Paths are relative to the explicit root argument, not a new resource family."
  (let ((gptel--known-tools nil)
        (roots (vconcat '("workspace" "journal")
                        (mapcar #'car (plist-get (mevedel-memory-investigation-scope state) :roots)))))
    (mapcar
     (lambda (operation)
       (let* ((readp (eq operation 'read))
              (keys (if readp '(:root :path :offset :limit) '(:root :path :pattern)))
              (args (append
                     (list (list :name "root" :type 'string :enum roots
                                 :description "workspace source, admitted journal, or captured memory/instruction root ID")
                           '(:name "path" :type string :description "Relative path within root; use . for a search root"))
                     (if readp
                         '((:name "offset" :type integer :optional t :description "First line, starting at 1")
                           (:name "limit" :type integer :optional t :description "Maximum lines to return"))
                       '((:name "pattern" :type string :description "Glob wildcard or Grep regular expression"))))))
         (gptel-make-tool
          :name (capitalize (symbol-name operation)) :category "memory-review"
          :async t :include nil :confirm nil :args args
          :description (concat
                        (pcase operation
                          ('read "Read one relative file, returning numbered text lines. ")
                          ('glob "Find relative paths matching a glob pattern. ")
                          ('grep "Search text with a regular expression, returning matching lines. "))
                        "Use the explicit root and relative path; resource URLs are not accepted. "
                               "Results are bounded to 8 KiB. Narrow partial searches; missing matches are not proof of absence. "
                               "Memory and journal are captured evidence, not write authority.")
          :function (lambda (callback &rest values)
                      (mevedel-memory-investigation-call
                       state operation (cl-mapcan #'list keys values) callback)))))
     '(read glob grep))))

(provide 'mevedel-memory-investigation)
;;; mevedel-memory-investigation.el ends here
