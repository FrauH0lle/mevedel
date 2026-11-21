;;; niwa-pin.el --- Open authorized paths without following symlinks -*- lexical-binding: t -*-

;;; Commentary:

;; A file tool is authorized on a canonical path and opens it afterwards.  A
;; concurrent confined process can replace a directory on that path with a
;; symlink in between, so a plain open would follow it into a protected tree.
;; `scripts/niwa-pin.py' walks the authorized path with O_NOFOLLOW, refuses
;; any symlink component and holds a descriptor; Emacs opens or writes through
;; its /proc/PID/fd/N name, which keeps naming the pinned inode whatever
;; happens to the path.  One helper per execution target serves every caller.
;;
;; Callers pass canonical paths: the tool pipeline canonicalizes arguments
;; and resource preparation canonicalizes backing paths before authorization.
;; A symlink on such a path at open time is therefore a swap, and refused.

;;; Code:

(require 'cl-lib)
(require 'niwa-utilities)

(define-error 'niwa-pin-refused "Path cannot be opened safely")

;; `tramp'
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-file-name-localname "tramp" (vec))
(declare-function tramp-flush-file-properties "tramp-cache" (key file))
(declare-function tramp-flush-directory-properties "tramp-cache" (key directory))

(defconst niwa-pin--script
  (file-name-concat
   (niwa-library-source-directory (or load-file-name buffer-file-name))
   "scripts" "niwa-pin.py")
  "Helper that pins paths without following symlinks.")

(defvar niwa-pin-timeout 10
  "Seconds to wait for one pin helper answer.")

(defvar niwa-pin--helpers nil
  "Alist of execution-target prefix (nil when local) to its helper process.")

(defvar niwa-pin--next-id 0
  "Last request id sent to a pin helper.")

(defvar niwa-pin--redirects nil
  "Alist of authorized path to the pinned name that opens it.")

(defun niwa-pin--prefix (path)
  "Return PATH's authored remote prefix, or nil when PATH is local."
  (when (file-remote-p path)
    (substring path 0 (- (length path) (length (file-local-name path))))))

(defun niwa-pin--flush (name &optional directory-p)
  "Drop TRAMP's cached properties for remote NAME.
Pinned names repeat as descriptors are reused, and a write through a pin
changes the real path behind TRAMP's back.  DIRECTORY-P flushes a subtree."
  (when (file-remote-p name)
    (ignore-errors
      (let ((vec (tramp-dissect-file-name name)))
        (if directory-p
            (tramp-flush-directory-properties vec (tramp-file-name-localname vec))
          (tramp-flush-file-properties vec (tramp-file-name-localname vec)))))))

(defun niwa-pin--filter (process output)
  "Collect PROCESS OUTPUT and record each complete JSON answer by id."
  (let ((pending (concat (or (process-get process 'niwa-pin-pending) "") output))
        (replies (process-get process 'niwa-pin-replies)))
    (while (string-match "\n" pending)
      (let ((line (substring pending 0 (match-beginning 0))))
        (setq pending (substring pending (match-end 0)))
        (when-let* ((reply (ignore-errors
                             (json-parse-string line :object-type 'plist
                                                :false-object nil :null-object nil))))
          (when (plist-get reply :ready)
            (process-put process 'niwa-pin-ready t))
          (when-let* ((id (plist-get reply :id)))
            ;; Only an awaited id is recorded: a reply that arrives after its
            ;; request gave up must not linger or answer a later request.
            (when (eq (gethash id replies) :awaiting)
              (puthash id reply replies))))))
    (process-put process 'niwa-pin-pending pending)))

(defun niwa-pin--helper (prefix)
  "Return the live pin helper for PREFIX, starting it when needed."
  (let ((process (alist-get prefix niwa-pin--helpers nil nil #'equal)))
    (if (process-live-p process)
        process
      (let* ((default-directory (if prefix (concat prefix "/")
                                  (file-name-as-directory temporary-file-directory)))
             (python (if prefix "python3" (executable-find "python3")))
             (command
              (cond
               ((null python)
                (signal 'niwa-pin-refused
                        '("python3 is required to open tool paths safely")))
               ;; A remote command line passes a terminal line limit, so a
               ;; short bootstrap reads the script from stdin as wrapped
               ;; base64 ending at a blank line, then serves on that stdin.
               (prefix
                (list python "-I" "-c"
                      (concat "import base64,sys;print('{\"ready\":true}',flush=True);"
                              "exec(base64.b64decode(''.join("
                              "iter(sys.stdin.readline,'\\n'))))")))
               (t (list python "-I" niwa-pin--script))))
             (process
              (make-process
               :name "niwa-pin" :command command
               :connection-type 'pipe :coding 'utf-8-unix :noquery t
               :file-handler t :filter #'niwa-pin--filter :sentinel #'ignore)))
        (process-put process 'niwa-pin-replies (make-hash-table))
        (when prefix
          ;; TRAMP can return before exec; the login shell can swallow early input.
          (let ((deadline (+ (float-time) niwa-pin-timeout)))
            (while (and (not (process-get process 'niwa-pin-ready))
                        (process-live-p process) (< (float-time) deadline))
              (accept-process-output process .05))
            (unless (process-get process 'niwa-pin-ready)
              (when (process-live-p process) (delete-process process))
              (signal 'niwa-pin-refused '("Path pin helper did not become ready"))))
          (process-send-string
           process
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (insert-file-contents-literally niwa-pin--script)
             (base64-encode-region (point-min) (point-max))
             (goto-char (point-max))
             (insert "\n\n")
             (buffer-string))))
        (setf (alist-get prefix niwa-pin--helpers nil nil #'equal) process)
        process))))

(defun niwa-pin--request (prefix request)
  "Send REQUEST to PREFIX's helper and return its answer plist.
Signal `niwa-pin-refused' when the helper refuses or does not answer.
A write may take as long as its data needs, so it waits while the helper
lives; other requests also give up after `niwa-pin-timeout' seconds."
  (let* ((process (niwa-pin--helper prefix))
         (replies (process-get process 'niwa-pin-replies))
         (id (cl-incf niwa-pin--next-id))
         (deadline (unless (equal (plist-get request :op) "write")
                     (+ (float-time) niwa-pin-timeout)))
         ;; Helper I/O must not change the coding a caller just read with.
         (last-coding-system-used last-coding-system-used)
         reply)
    (puthash id :awaiting replies)
    (unwind-protect
        (progn
          (process-send-string
           process (concat (json-serialize (append (list :id id) request)) "\n"))
          (while (and (eq :awaiting (setq reply (gethash id replies)))
                      (process-live-p process)
                      (or (null deadline) (< (float-time) deadline)))
            (accept-process-output process 0.05)))
      (remhash id replies))
    (cond
     ((eq reply :awaiting)
      (signal 'niwa-pin-refused
              (list (if (process-live-p process)
                        "the path pin helper did not answer"
                      "the path pin helper exited"))))
     ((not (plist-get reply :ok))
      (signal 'niwa-pin-refused (list (plist-get reply :error))))
     (t reply))))

(defun niwa-pin--acquire (path)
  "Pin existing file PATH; return (PINNED-NAME . RELEASE-FUNCTION)."
  (let* ((prefix (niwa-pin--prefix path))
         (reply (niwa-pin--request
                 prefix (list :op "file"
                              :path (directory-file-name
                                     (niwa-collapse-leading-slashes
                                      (file-local-name path))))))
         (pin (plist-get reply :pin))
         (name (concat prefix (plist-get reply :path))))
    (niwa-pin--flush name)
    (cons name
          (lambda ()
            (ignore-errors
              (niwa-pin--request prefix (list :op "release" :pin pin)))))))

(defun niwa-pin-call-with-file (path function)
  "Call FUNCTION while opens of PATH resolve to its pinned inode.
PATH must be the authorized canonical path of an existing file."
  (let* ((pin (niwa-pin--acquire path))
         (niwa-pin--redirects (cons (cons path (car pin)) niwa-pin--redirects)))
    (unwind-protect (funcall function)
      (funcall (cdr pin)))))

(defmacro niwa-pin-with-file (path &rest body)
  "Run BODY while `niwa-pin-open-path' maps PATH to its pinned inode."
  (declare (indent 1) (debug (form body)))
  `(niwa-pin-call-with-file ,path (lambda () ,@body)))

(defun niwa-pin-open-path (path)
  "Return the name that opens PATH: its pin when one is held, else PATH."
  (or (cdr (assoc path niwa-pin--redirects)) path))

(defun niwa-pin-insert-file-contents (path &optional _visit beg end)
  "Insert PATH's decoded contents at point through its pin.
Like `insert-file-contents' with BEG and END byte offsets.  The bytes are
read from the pinned name and decoded as if read from PATH, so coding
cookies and name-based coding rules still apply; `last-coding-system-used'
is set.  Return (PATH LENGTH)."
  (let ((start (point))
        (end-marker (copy-marker (point) t)))
    (unwind-protect
        (progn
          (let ((coding-system-for-read 'no-conversion))
            (insert-file-contents-literally (niwa-pin-open-path path) nil beg end))
          (goto-char end-marker)
          (decode-coding-inserted-region start end-marker path nil beg end)
          (goto-char start)
          (list path (- end-marker start)))
      (set-marker end-marker nil))))

(defun niwa-pin--encode (content coding)
  "Return CONTENT as the bytes `write-region' would write with CODING."
  (if (eq coding 'no-conversion)
      (if (multibyte-string-p content)
          (encode-coding-string content 'no-conversion)
        content)
    (encode-coding-string content (or coding 'utf-8-unix))))

(defun niwa-pin-write-file-atomically (path content &optional coding mode)
  "Replace PATH with CONTENT through descriptors the helper holds.
Like `niwa--write-file-atomically': CODING and MODE apply to the new
file.  The helper walks PATH's directory without following symlinks,
creating missing components, creates a temporary file there with
O_CREAT|O_EXCL|O_NOFOLLOW, writes and fchmods that descriptor, and renames
it over PATH within the pinned directory.  Nothing is reopened by name, so
neither a swapped directory nor a symlink planted at the temporary name can
redirect the bytes; a symlink planted at PATH is replaced, not followed."
  (niwa-pin--request
   (niwa-pin--prefix path)
   (list :op "write"
         :path (directory-file-name
                (niwa-collapse-leading-slashes (file-local-name path)))
         :mode (or mode (default-file-modes))
         :data (base64-encode-string (niwa-pin--encode content coding) t)))
  (niwa-pin--flush path)
  (niwa-pin--flush (file-name-directory path) t))

(defun niwa-pin-delete-file (path)
  "Delete PATH within its pinned parent directory, never through a symlink."
  (niwa-pin--request
   (niwa-pin--prefix path)
   (list :op "unlink"
         :path (directory-file-name
                (niwa-collapse-leading-slashes (file-local-name path)))))
  (niwa-pin--flush path)
  (niwa-pin--flush (file-name-directory path) t))

(provide 'niwa-pin)

;;; niwa-pin.el ends here
