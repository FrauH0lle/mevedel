;;; mevedel-tool-web.el -- Web tool definitions -*- lexical-binding: t -*-

;; Copyright (C) 2025 Karthik Chikmagalur
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Search, readability and YouTube handlers adapted from gptel-agent-tools.el.

;;; Commentary:

;; Native WebSearch and WebFetch tools using EWW, SHR and url-retrieve.
;; Every HTTP stage owns its timeout, response buffers and settlement.
;; Tool calls retain the mevedel permission, persistence and display pipeline.

;;; Code:

(require 'cl-lib)
(require 'eww)
(require 'mail-parse)
(require 'url-http)

(eval-when-compile
  (require 'mevedel-tool-registry))

;; `gptel-request'
(declare-function gptel--model-capable-p "ext:gptel-request" (cap &optional model))
(declare-function gptel--model-mime-capable-p "ext:gptel-request" (mime &optional model))
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

;; `mevedel-execution'
(declare-function mevedel-execution-start-helper
                  "mevedel-execution"
                  (callback name command read-paths writable-roots &rest keys))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-run-tool
                  "mevedel-pipeline" (tool callback args))
(declare-function mevedel-pipeline-tool-results-dir
                  "mevedel-pipeline" (session buffer &optional request))

;; `mevedel-resource'
(declare-function mevedel-resource-artifact-address "mevedel-resource" (path session))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-publish-text
                  "mevedel-session-artifacts" (session path content &optional coding))

;; `mevedel-tool-permission'
(declare-function mevedel-tool-permission-decide-now
                  "mevedel-tool-permission" (tool-name args &optional buffer reason))

;; `mevedel-tool-registry'
(declare-function mevedel-tool--positional-to-plist
                  "mevedel-tool-registry" (raw-args specs))
(declare-function mevedel-tool--resolve-prompt
                  "mevedel-tool-registry" (prompt))
(declare-function mevedel-tool-register "mevedel-tool-registry" (tool))

;; `mevedel-turn'
(declare-function mevedel-current-origin "mevedel-turn" ())

;; `mevedel-view-render'
(declare-function mevedel-view-data-buffer-major-mode "mevedel-view-render" ())
(autoload 'mevedel-view-data-buffer-major-mode "mevedel-view-render")

;; `url-http'
(defvar url-http-data)
(defvar url-http-extra-headers)
(defvar url-http-method)
(defvar url-http-response-status)


;;
;;; Helpers

(defun mevedel-tool-web--url-host (url)
  "Return the host component of URL, or nil if it cannot be parsed."
  (when (stringp url)
    (ignore-errors
      (let ((host (url-host (url-generic-parse-url url))))
        (and host (not (string-empty-p host)) host)))))

(defun mevedel-tool-web--unsupported-url (url)
  "Return why URL cannot be retrieved, or nil for an http(s) URL with a host."
  (unless (and (stringp url)
               (string-match-p "\\`https?://" (downcase url))
               (mevedel-tool-web--url-host url))
    (format "Unsupported URL (only http and https are retrieved): %s" url)))

(defun mevedel-tool-web--local-host-p (host)
  "Return non-nil when HOST names this machine or a private network address."
  (when host
    (let ((host (downcase (string-trim host "\\[" "\\]"))))
      (or (equal host "localhost")
          (string-suffix-p ".localhost" host)
          (member host '("::" "::1" "0.0.0.0"))
          (string-match-p
           (rx bos (or "127." "10." "192.168." "169.254."
                       (seq "172." (or (seq "1" (any "6-9")) (seq "2" digit) "30" "31")
                            ".")))
           host)
          (string-match-p (rx bos (or (seq "f" (any "cd") (= 2 hex) ":") "fe80:"))
                          host)))))


;;
;;; Request ownership

(defvar mevedel-tool-web--timeout 30
  "Maximum seconds for one retrieval, including redirects.")

(defvar mevedel-tool-web--max-redirects 10
  "Maximum redirects one retrieval follows.")

(cl-defun mevedel-tool-web--retrieve (url parse callback &key redirect accept)
  "Retrieve URL, parse its body with PARSE, then call CALLBACK.
CALLBACK receives (VALUE ERROR), where ERROR is nil on success or an
error string.  Only http and https URLs are retrieved.

Redirects are followed here, at most `mevedel-tool-web--max-redirects'.
A redirect to the same host is followed.  Otherwise REDIRECT, called
with the redirecting and the target URL, returns `follow', or
\(result . TEXT) or (error . TEXT) to settle with TEXT instead.
Without REDIRECT, every redirect is followed.  ACCEPT, when non-nil,
is the Accept header of every request.

One timeout covers the whole chain.  Each retrieval owns its timer
and response buffers.  Cleanup precedes delivery, exactly once."
  (let ((token (make-symbol "mevedel-web-request"))
        (redirects 0)
        buffer timer done)
    (cl-labels
        ((cleanup ()
           (when timer (cancel-timer timer))
           (let ((kill-buffer-query-functions nil))
             (dolist (candidate (buffer-list))
               (when (or (eq candidate buffer)
                         (with-current-buffer candidate
                           (memq token (bound-and-true-p url-callback-arguments))))
                 (kill-buffer candidate)))))
         (finish (value error)
           (unless done
             (setq done t)
             (cleanup)
             (funcall callback value error)))
         (request (target method data headers)
           (condition-case err
               (if-let* ((problem (mevedel-tool-web--unsupported-url target)))
                   (finish nil problem)
                 ;; url-http copies these into each response buffer, so
                 ;; every hop binds them again.
                 (let ((url-max-redirections 0)
                       (url-request-noninteractive t)
                       (url-request-method method)
                       (url-request-data data)
                       (url-request-extra-headers headers)
                       (url-mime-accept-string (or accept url-mime-accept-string))
                       (inhibit-message t))
                   (setq buffer (url-retrieve target #'receive (list token) t t))
                   ;; Some URL handlers call back before returning their buffer.
                   (cond (done (cleanup))
                         ((not (buffer-live-p buffer))
                          (finish nil "Retrieval did not create a response buffer")))))
             (error (finish nil (error-message-string err)))))
         (follow (target)
           ;; url-http already switched 302 and 303 to GET and dropped
           ;; Authorization in this redirect response's buffer.
           (let ((from (url-recreate-url url-current-object))
                 (method url-http-method)
                 (data url-http-data)
                 (headers url-http-extra-headers))
             (if (>= redirects mevedel-tool-web--max-redirects)
                 (finish nil (format "Too many redirects (more than %d)"
                                     mevedel-tool-web--max-redirects))
               (cl-incf redirects)
               (pcase (if (or (null redirect)
                              (equal (downcase (or (mevedel-tool-web--url-host from) ""))
                                     (downcase (or (mevedel-tool-web--url-host target) ""))))
                          'follow
                        (funcall redirect from target))
                 ('follow (request target method data headers))
                 (`(result . ,text) (finish text nil))
                 (`(error . ,text) (finish nil text))
                 (decision (finish nil (format "Invalid redirect decision: %S"
                                               decision)))))))
         (receive (status _token)
           (unless done
             (condition-case err
                 (let ((failure (plist-get status :error)))
                   (cond
                    ((eq (car-safe (cdr-safe failure)) 'http-redirect-limit)
                     (follow (nth 2 failure)))
                    (failure (finish nil (format "%S" failure)))
                    (t
                     (goto-char (point-min))
                     (if (bound-and-true-p url-http-end-of-headers)
                         ;; The marker sits on the blank line ending the
                         ;; headers; the body starts after it.
                         (progn (goto-char url-http-end-of-headers)
                                (when (eq (char-after) ?\n) (forward-char 1)))
                       (unless (re-search-forward "\r?\n\r?\n" nil t)
                         (error "Response has no HTTP headers")))
                     (finish (funcall parse) nil))))
               (error (finish nil (error-message-string err)))))))
      (setq timer (run-at-time
                   mevedel-tool-web--timeout nil
                   (lambda ()
                     (finish nil (format "Request timed out after %s seconds"
                                         mevedel-tool-web--timeout)))))
      (request url url-request-method url-request-data url-request-extra-headers))))

(defun mevedel-tool-web--content-type ()
  "Return the response's (MIME-TYPE . CHARSET), each a string or nil."
  (let ((parsed (and (bound-and-true-p url-http-content-type)
                     (mail-header-parse-content-type url-http-content-type))))
    (cons (and (car parsed) (downcase (car parsed)))
          (cdr (assq 'charset (cdr parsed))))))

(defun mevedel-tool-web--body (&optional html-p)
  "Return the response body after point, decoded.
The Content-Type charset wins; HTML-P also consults a meta charset.
Unknown or missing charsets decode as UTF-8."
  (let* ((charset (or (cdr (mevedel-tool-web--content-type))
                      (save-excursion (eww-detect-charset html-p))))
         (coding (or (and charset
                          (ignore-errors
                            (coding-system-from-name (downcase charset))))
                     'utf-8)))
    (decode-coding-string
     (buffer-substring-no-properties (point) (point-max)) coding)))

(defun mevedel-tool-web--html-dom ()
  "Return the DOM of the HTML response body at point."
  (let ((html (mevedel-tool-web--body t)))
    (with-temp-buffer
      (insert html)
      (libxml-parse-html-region (point-min) (point-max)))))

(defun mevedel-tool-web--page-text (&optional base)
  "Return readable text from the HTML response body at point.
Links become markdown links whose relative targets resolve against
BASE, the page's URL."
  (let* ((dom (mevedel-tool-web--html-dom))
         (readable (or (eww-readable-dom dom) dom)))
    (with-temp-buffer
      (let ((shr-use-fonts nil) (shr-width 80))
        ;; `shr-insert-document' resets `shr-base'; a base element sets it.
        (shr-insert-document (if base (eww-document-base base readable) readable)))
      (mevedel-tool-web--markdown-links base)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun mevedel-tool-web--without-fragment (url)
  "Return URL without its fragment."
  (replace-regexp-in-string "#.*\\'" "" url))

(defun mevedel-tool-web--markdown-link (text url page)
  "Return TEXT linking to URL as markdown, or nil to keep TEXT plain.
Only http(s) links with text are kept; a link to PAGE, the fetched
page's URL without fragment, is a same-page anchor.  The text keeps
its surrounding whitespace and loses its line wrapping."
  (let ((label (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " text))))
    (when (and (stringp url)
               (string-match-p "\\`https?://" url)
               (not (string-empty-p label))
               (not (equal (mevedel-tool-web--without-fragment url) page)))
      (let ((target (replace-regexp-in-string
                     "[()]" (lambda (paren) (if (equal paren "(") "%28" "%29"))
                     url t t)))
        (concat (and (string-match "\\`[ \t\n\r]+" text) (match-string 0 text))
                (if (equal label url)
                    target
                  (format "[%s](%s)"
                          (replace-regexp-in-string "[][]" "\\\\\\&" label)
                          target))
                (and (string-match "[ \t\n\r]+\\'" text) (match-string 0 text)))))))

(defun mevedel-tool-web--markdown-links (base)
  "Rewrite the current buffer's `shr-url' runs as markdown links.
BASE is the page's URL."
  (let ((page (and base (mevedel-tool-web--without-fragment base))))
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let* ((start (point))
             (url (get-text-property start 'shr-url))
             (end (or (next-single-property-change start 'shr-url) (point-max)))
             (link (and url (mevedel-tool-web--markdown-link
                             (buffer-substring-no-properties start end) url page))))
        (if (not link)
            (goto-char end)
          (delete-region start end)
          (goto-char start)
          (insert link))))))


;;
;;; Search

(defvar mevedel-tool-web--search-url "https://html.duckduckgo.com/html/?q="
  "DuckDuckGo HTML endpoint; the hexified query is appended.")

(defvar mevedel-tool-web--search-limit 10
  "Maximum number of results one search returns.")

(defvar mevedel-tool-web--search-active 0
  "Number of active web searches.")
(defvar mevedel-tool-web--search-queue nil
  "FIFO of pending (URL PARSE CALLBACK) searches.")

(defun mevedel-tool-web--class-p (node class)
  "Return non-nil when NODE's class attribute has the token CLASS."
  (and (consp node)
       (member class (split-string (or (dom-attr node 'class) "")))))

(defun mevedel-tool-web--by-class (dom class)
  "Return the elements of DOM whose class attribute has the token CLASS."
  (dom-search dom (lambda (node) (mevedel-tool-web--class-p node class))))

(defun mevedel-tool-web--node-text (node)
  "Return NODE's text with whitespace runs collapsed."
  (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " (dom-inner-text node))))

(defun mevedel-tool-web--result-url (href)
  "Return the destination of DuckDuckGo result link HREF, or nil.
DuckDuckGo wraps destinations as the `uddg' parameter of its own
redirect link; an absolute http(s) HREF is already the destination."
  (when (stringp href)
    (if-let* ((start (string-search "?" href))
              (target (car (alist-get "uddg"
                                      (url-parse-query-string
                                       (substring href (1+ start)))
                                      nil nil #'equal))))
        (url-encode-url (decode-coding-string target 'utf-8))
      (and (string-match-p "\\`https?://" href) href))))

(defun mevedel-tool-web--domains (domains)
  "Return DOMAINS, a list or vector of host suffixes, normalized."
  (mapcar (lambda (domain)
            (downcase (string-remove-prefix
                       "." (string-remove-prefix "*" (string-trim domain)))))
          (append domains nil)))

(defun mevedel-tool-web--domain-match-p (url domains)
  "Return non-nil when URL's host is one of DOMAINS or a subdomain of one."
  (when-let* ((host (mevedel-tool-web--url-host url)))
    (setq host (downcase host))
    (seq-some (lambda (domain)
                (or (string= host domain)
                    (string-suffix-p (concat "." domain) host)))
              domains)))

(defun mevedel-tool-web--search-results (allowed blocked)
  "Return formatted DuckDuckGo results from the response body at point.
When ALLOWED is non-nil, keep only results on those domains; drop
results on BLOCKED domains.  Signal an error for a bot challenge."
  (let ((dom (mevedel-tool-web--html-dom))
        (seen (make-hash-table :test #'equal))
        (found 0)
        results)
    (dolist (block (mevedel-tool-web--by-class dom "result"))
      (when-let* (((not (mevedel-tool-web--class-p block "result--ad")))
                  (link (car (mevedel-tool-web--by-class block "result__a")))
                  (url (mevedel-tool-web--result-url (dom-attr link 'href)))
                  ((not (gethash url seen))))
        (puthash url t seen)
        (cl-incf found)
        (when (and (< (length results) mevedel-tool-web--search-limit)
                   (or (null allowed)
                       (mevedel-tool-web--domain-match-p url allowed))
                   (not (mevedel-tool-web--domain-match-p url blocked)))
          (push (list (mevedel-tool-web--node-text link) url
                      (when-let* ((snippet (car (mevedel-tool-web--by-class
                                                 block "result__snippet"))))
                        (mevedel-tool-web--node-text snippet)))
                results))))
    (cond
     (results
      (let ((index 0))
        (mapconcat (pcase-lambda (`(,title ,url ,snippet))
                     (format "%d. %s\n   %s%s" (cl-incf index) title url
                             (if (and snippet (not (string-empty-p snippet)))
                                 (concat "\n   " snippet)
                               "")))
                   (nreverse results) "\n\n")))
     ((or (dom-by-id dom "\\`challenge-form\\'")
          (mevedel-tool-web--by-class dom "anomaly-modal__modal"))
      (error "DuckDuckGo refused the search (bot challenge); retry later"))
     ((> found 0) "No results on the requested domains.")
     (t "No results."))))

(defun mevedel-tool-web--start-searches ()
  "Start queued searches while fewer than two retrievals are active."
  (while (and mevedel-tool-web--search-queue
              (< mevedel-tool-web--search-active 2))
    (pcase-let ((`(,url ,parse ,callback) (pop mevedel-tool-web--search-queue)))
      (cl-incf mevedel-tool-web--search-active)
      (mevedel-tool-web--retrieve
       url parse
       (lambda (value error)
         (cl-decf mevedel-tool-web--search-active)
         (unwind-protect
             (funcall callback (if error
                                   (list :result (concat "Error: " error) :status 'error)
                                 (list :result value)))
           (mevedel-tool-web--start-searches)))))))

(defun mevedel-tool-web--websearch (callback args)
  "Search for the query in ARGS and deliver a handler result to CALLBACK.
Optional `allowed_domains' or `blocked_domains' in ARGS add `site:'
terms to the query and filter the returned results."
  (let ((allowed (mevedel-tool-web--domains (plist-get args :allowed_domains)))
        (blocked (mevedel-tool-web--domains (plist-get args :blocked_domains))))
    (if (and allowed blocked)
        (funcall callback
                 (list :result "Error: Pass allowed_domains or blocked_domains, not both."
                       :status 'error))
      (let ((query (string-join
                    (cons (plist-get args :query)
                          (if allowed
                              (list (mapconcat (lambda (domain) (concat "site:" domain))
                                               allowed " OR "))
                            (mapcar (lambda (domain) (concat "-site:" domain))
                                    blocked)))
                    " ")))
        (setq mevedel-tool-web--search-queue
              (nconc mevedel-tool-web--search-queue
                     (list (list (concat mevedel-tool-web--search-url
                                         (url-hexify-string query))
                                 (lambda ()
                                   (mevedel-tool-web--search-results allowed blocked))
                                 callback))))
        (mevedel-tool-web--start-searches)))))


;;
;;; Page and YouTube retrieval

(defun mevedel-tool-web--redirect-decision (buffer from target)
  "Return how WebFetch handles a redirect from FROM to TARGET.
A target that a fresh WebFetch call could fetch without prompting under
BUFFER's permission policy is followed; a denied target is an error.
Otherwise the target is handed back to the model, whose WebFetch call
on it gets the ordinary permission check.  A redirect from a public
host into the local machine or a private network is always handed back."
  (let ((host (mevedel-tool-web--url-host from)))
    (pcase (if (and (mevedel-tool-web--local-host-p
                     (mevedel-tool-web--url-host target))
                    (not (mevedel-tool-web--local-host-p host)))
               'ask
             (mevedel-tool-permission-decide-now
              "WebFetch" (list :url target) buffer 'redirect))
      ('allow 'follow)
      ('deny (cons 'error (format "Redirect from %s to %s blocked: the target \
host is denied by permission rules" host target)))
      (_ (cons 'result (format "REDIRECT: %s redirects to %s.  That host needs \
approval; call WebFetch with url=%S to continue." host target target))))))

(defvar mevedel-tool-web--accept "text/markdown, text/html;q=0.9, */*;q=0.8"
  "Accept header of WebFetch requests; servers able to send markdown do.")

(defvar mevedel-tool-web--image-max-bytes (* 10 1024 1024)
  "Maximum size of an image WebFetch attaches.")

(defconst mevedel-tool-web--image-types
  '("image/png" "image/jpeg" "image/gif" "image/webp")
  "Image MIME types WebFetch can attach as media.")

(defun mevedel-tool-web--body-kind (type)
  "Return the kind of the response body at point with MIME TYPE.
The kind is `html', `text', `image', `pdf' or `binary'.  Without TYPE,
the body itself decides."
  (cond
   ((null type)
    (let ((case-fold-search t))
      (cond ((looking-at-p "%PDF-") 'pdf)
            ((looking-at-p "[ \t\r\n]*<\\(?:!doctype html\\|html\\)") 'html)
            ((save-excursion (search-forward "\0" (min (point-max) (+ (point) 1024)) t))
             'binary)
            (t 'text))))
   ((member type '("text/html" "application/xhtml+xml")) 'html)
   ((or (string-prefix-p "text/" type)
        (string-match-p (rx bos "application/"
                            (or "json" "xml" "javascript" "ecmascript" "x-javascript"
                                (seq (+ (not (any ";"))) "+" (or "json" "xml")))
                            eos)
                        type))
    'text)
   ((member type mevedel-tool-web--image-types) 'image)
   ((equal type "application/pdf") 'pdf)
   (t 'binary)))

(defun mevedel-tool-web--response ()
  "Return the response at point as a plist for WebFetch.
:url is the final URL, :code the HTTP status, :type the MIME type,
:kind the body's kind and :bytes its size.  HTML and text bodies are
decoded into :text; others stay raw bytes in :data."
  (let* ((type (car (mevedel-tool-web--content-type)))
         (kind (mevedel-tool-web--body-kind type)))
    (append (list :url (url-recreate-url url-current-object)
                  :code (bound-and-true-p url-http-response-status)
                  :type type :kind kind
                  :bytes (- (point-max) (point)))
            (pcase kind
              ('html (list :text (mevedel-tool-web--page-text
                                  (url-recreate-url url-current-object))))
              ('text (list :text (mevedel-tool-web--body)))
              (_ (list :data (buffer-substring-no-properties (point) (point-max))))))))

(defun mevedel-tool-web--image-data-p (type data)
  "Return non-nil when DATA starts like an image of MIME TYPE."
  (pcase type
    ("image/png" (string-prefix-p (unibyte-string #x89 ?P ?N ?G ?\r ?\n #x1a ?\n) data))
    ("image/jpeg" (string-prefix-p (unibyte-string #xff #xd8 #xff) data))
    ("image/gif" (or (string-prefix-p "GIF87a" data) (string-prefix-p "GIF89a" data)))
    ("image/webp" (and (string-prefix-p "RIFF" data)
                       (>= (length data) 12)
                       (equal "WEBP" (substring data 8 12))))))

(defun mevedel-tool-web--model-image-types ()
  "Return the image MIME types the current model accepts as media."
  (and (fboundp 'gptel--model-capable-p)
       (gptel--model-capable-p 'media)
       (seq-filter #'gptel--model-mime-capable-p mevedel-tool-web--image-types)))

(defun mevedel-tool-web--pdf-text (data origin callback)
  "Extract the text of PDF DATA with `pdftotext', then call CALLBACK.
CALLBACK receives (TEXT ERROR) once.  The helper runs on this machine,
whatever the session's execution target, owned by agent ORIGIN."
  (let ((file (make-temp-file "mevedel-web-" nil ".pdf"))
        done)
    (cl-flet ((settle (text error)
                (unless done
                  (setq done t)
                  (ignore-errors (delete-file file))
                  (funcall callback text error))))
      (condition-case err
          (if (not (executable-find "pdftotext"))
              (settle nil "PDF text extraction needs 'pdftotext'")
            (let ((coding-system-for-write 'no-conversion))
              (write-region data nil file nil 'silent))
            (mevedel-execution-start-helper
             (lambda (result)
               (let ((output (decode-coding-string (or (plist-get result :output) "")
                                                   'utf-8))
                     (failure (plist-get result :error)))
                 (cond
                  (failure (settle nil (if (stringp failure) failure
                                         (error-message-string failure))))
                  ((plist-get result :timed-out-p)
                   (settle nil "'pdftotext' timed out"))
                  ((eql 0 (plist-get result :exit-code)) (settle output nil))
                  (t (settle nil (format "'pdftotext' failed: %s" (string-trim output)))))))
             "mevedel-pdftotext" (list "pdftotext" "-q" "-layout" file "-") (list file) nil
             :timeout mevedel-tool-web--timeout :session nil :owner origin
             :teardown-callback (lambda () (settle nil "Helper owner was torn down"))))
        (error (settle nil (error-message-string err)))))))

(defvar mevedel-tool-web--pdf-save-max-bytes (* 25 1024 1024)
  "Maximum size of a fetched PDF WebFetch saves as a session artifact.")

(defun mevedel-tool-web--save-pdf (data session buffer)
  "Save PDF DATA as an artifact of SESSION and return its address.
BUFFER is the dispatching buffer.  Return nil without durable session
storage or when DATA exceeds `mevedel-tool-web--pdf-save-max-bytes'."
  (when-let* ((session)
              ((<= (length data) mevedel-tool-web--pdf-save-max-bytes))
              (dir (mevedel-pipeline-tool-results-dir session buffer)))
    (let ((path (concat (make-temp-name (file-name-concat dir "WebFetch-")) ".pdf")))
      (mevedel-session-artifacts-publish-text session path data)
      (mevedel-resource-artifact-address path session))))

(defun mevedel-tool-web--pdf-result (text error saved)
  "Return WebFetch's text for a PDF with extracted TEXT or ERROR.
SAVED is the PDF's artifact address, or an error string when saving
failed, or nil."
  (concat
   (cond ((and saved (string-prefix-p "artifact://" saved))
          (format "PDF saved as %s; Read it to view the pages themselves.\n\n" saved))
         (saved (format "The PDF could not be saved: %s\n\n" saved)))
   (cond (error (format "No text extracted: %s." error))
         ;; pdftotext separates pages with form feeds.
         ((string-match-p "\\`[[:space:]\f]*\\'" text)
          "No text extracted; the PDF may contain only scanned images.")
         (t text))))

(defun mevedel-tool-web--fetch-result (url response context deliver)
  "Deliver WebFetch's handler result for URL's RESPONSE to DELIVER.
CONTEXT holds what the handler captured at entry: the `:images' types
the model accepts, the `:origin' owning a PDF helper, and the
`:session' and `:buffer' that save a PDF.  Signal an error for content
WebFetch cannot return."
  (let* ((final (plist-get response :url))
         (type (plist-get response :type))
         (bytes (plist-get response :bytes))
         (redirected (and (not (equal final (url-recreate-url (url-generic-parse-url url))))
                          final))
         (prefix (if redirected (format "Redirected to %s\n\n" final) ""))
         (render (list :kind 'web :tool "WebFetch"
                       :host (mevedel-tool-web--url-host url)
                       :final-host (and redirected (mevedel-tool-web--url-host final))
                       :code (plist-get response :code)
                       :content-type type :bytes bytes)))
    (cl-flet ((text-result (text)
                (let ((result (concat prefix text)))
                  (funcall deliver (list :result result
                                         :render-data (append render
                                                              (list :chars (length result))))))))
      (pcase (plist-get response :kind)
        ((or 'html 'text) (text-result (plist-get response :text)))
        ('image
         (let ((data (plist-get response :data)))
           (cond
            ((not (member type (plist-get context :images)))
             (error "The current model does not accept %s images" type))
            ((> bytes mevedel-tool-web--image-max-bytes)
             (error "Image is too large (%d bytes > %d bytes)"
                    bytes mevedel-tool-web--image-max-bytes))
            ((not (mevedel-tool-web--image-data-p type data))
             (error "Response is not a valid %s image" type))
            (t
             (let ((result (format "%sImage %s (%s, %d bytes)." prefix final type bytes)))
               (funcall deliver
                        (list :result result
                              :media (list (list :kind 'image :mime type
                                                 :data (base64-encode-string data t)
                                                 :source final))
                              :render-data (append render
                                                   (list :chars (length result))))))))))
        ('pdf
         (let* ((data (plist-get response :data))
                (saved (condition-case err
                           (mevedel-tool-web--save-pdf
                            data (plist-get context :session) (plist-get context :buffer))
                         (error (error-message-string err)))))
           (mevedel-tool-web--pdf-text
            data (plist-get context :origin)
            (lambda (text error)
              (if (and error (not saved))
                  (funcall deliver (list :result (concat "Error: " error) :status 'error))
                (text-result (mevedel-tool-web--pdf-result text error saved)))))))
        (_ (error "Binary content (%s, %d bytes) is not readable by WebFetch"
                  (or type "unknown type") bytes))))))

(defun mevedel-tool-web--fetch (callback args)
  "Fetch the URL in ARGS and deliver its content to CALLBACK.
HTML becomes readable text, other text is returned verbatim, images
are attached as media when the model accepts them, and PDFs become
their extracted text."
  (let* ((url (plist-get args :url))
         (buffer (current-buffer))
         (context (list :images (mevedel-tool-web--model-image-types)
                        :origin (mevedel-current-origin)
                        :session (bound-and-true-p mevedel--session)
                        :buffer buffer))
         done
         (deliver (lambda (result)
                    (unless done
                      (setq done t)
                      (funcall callback result))))
         (fail (lambda (error)
                 (funcall deliver (list :result (concat "Error: " error) :status 'error)))))
    (if-let* ((video-id (mevedel-tool-web--yt-video-id url)))
        (mevedel-tool-web--yt-fetch
         (lambda (value error)
           (funcall deliver
                    (if error
                        (list :result (or value (concat "Error: " error)) :status 'error)
                      (list :result value
                            :render-data (list :kind 'web :tool "WebFetch"
                                               :host (mevedel-tool-web--url-host url)
                                               :chars (length value))))))
         video-id)
      (mevedel-tool-web--retrieve
       url #'mevedel-tool-web--response
       (lambda (value error)
         (cond
          (error (funcall fail error))
          ((stringp value)
           (funcall deliver
                    (list :result value
                          :render-data (list :kind 'web :tool "WebFetch"
                                             :host (mevedel-tool-web--url-host url)
                                             :redirect t :chars (length value)))))
          (t (condition-case err
                 (mevedel-tool-web--fetch-result url value context deliver)
               (error (funcall fail (error-message-string err)))))))
       :redirect (lambda (from target)
                   (mevedel-tool-web--redirect-decision buffer from target))
       :accept mevedel-tool-web--accept))))

(defun mevedel-tool-web--yt-fetch (callback video-id)
  "Fetch VIDEO-ID's description and captions, delivering to CALLBACK."
  (mevedel-tool-web--retrieve
   (format "https://youtube.com/watch?v=%s" video-id)
   (lambda ()
     (unless (re-search-forward "\"INNERTUBE_API_KEY\":\"\\([a-zA-Z0-9_-]+\\)" nil t)
       (error "Could not extract YouTube API key"))
     (match-string 1))
   (lambda (api-key error)
     (if error (funcall callback nil error)
       (let ((url-request-method "POST")
             (url-request-extra-headers
              '(("Content-Type" . "application/json") ("Accept-Language" . "en-US")))
             (url-request-data
              (encode-coding-string
               (json-encode
                `((context . ((client . ((clientName . "ANDROID")
                                        (clientVersion . "20.10.38")))))
                  (videoId . ,video-id))) 'utf-8)))
         (mevedel-tool-web--retrieve
          (format "https://www.youtube.com/youtubei/v1/player?key=%s" api-key)
          (lambda () (json-parse-buffer :object-type 'plist))
          (lambda (metadata error)
            (if error (funcall callback nil error)
              ;; Metadata used POST; caption retrieval is always a fresh GET,
              ;; even when a transport invokes its callback synchronously.
              (let ((url-request-method "GET")
                    (url-request-extra-headers nil)
                    (url-request-data nil))
                (condition-case err
                    (mevedel-tool-web--yt-captions callback metadata)
                  (error (funcall callback nil (error-message-string err)))))))))))))

(defun mevedel-tool-web--yt-captions (callback metadata)
  "Read captions from METADATA and deliver description/transcript to CALLBACK."
  (let* ((description (or (map-nested-elt metadata '(:videoDetails :shortDescription))
                          "No description available."))
         (tracks (map-nested-elt metadata
                                '(:captions :playerCaptionsTracklistRenderer :captionTracks)))
         (english (seq-find
                   (lambda (track)
                     (string-prefix-p "en" (or (plist-get track :languageCode) "")))
                   tracks))
         (render (lambda (transcript)
                   (format "# Description\n\n%s\n\n# Transcript\n\n%s"
                           description transcript))))
    (if (not english)
        (funcall callback (funcall render (if tracks "No English transcript available."
                                           "No transcript available.")) nil)
      (mevedel-tool-web--retrieve
       (replace-regexp-in-string "&fmt=srv3" "" (plist-get english :baseUrl))
       (lambda ()
         (or (mevedel-tool-web--yt-format-captions
              (mevedel-tool-web--yt-parse-captions
               (buffer-substring-no-properties (point) (point-max))))
             (error "Could not parse YouTube transcript")))
       (lambda (text error)
         (funcall callback
                  (funcall render (if error
                                      (concat "Error fetching transcript: " error)
                                    text))
                  error))))))

(defun mevedel-tool-web--yt-video-id (url)
  "Return the video ID if URL is a YouTube video URL, nil otherwise."
  (and (string-match
        (rx bol (opt "http" (opt "s") "://")
            (opt "www.") "youtu" (or ".be" "be.com") "/"
            (opt "watch?v=")
            (group (one-or-more (not (any "?&")))))
        url)
       (match-string 1 url)))

(defun mevedel-tool-web--yt-parse-captions (xml-string)
  "Parse YouTube caption XML-STRING and return DOM."
  (with-temp-buffer
    (insert xml-string)
    (set-buffer-multibyte t)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (goto-char (point-min))
    ;; Clean up the XML
    (dolist (reps '(("\n" . " ")
                    ("&amp;" . "&")
                    ("&quot;" . "\"")
                    ("&#39;" . "'")
                    ("&lt;" . "<")
                    ("&gt;" . ">")))
      (save-excursion
        (while (search-forward (car reps) nil t)
          (replace-match (cdr reps) nil t))))
    (libxml-parse-xml-region (point-min) (point-max))))

(defun mevedel-tool-web--yt-format-captions (caption-dom &optional chunk-time)
  "Format CAPTION-DOM as paragraphs with timestamps.

CHUNK-TIME is the number of seconds per paragraph (default 30)."
  (when (and (listp caption-dom)
             (eq (car-safe caption-dom) 'transcript))
    (let ((chunk-time (or chunk-time 30))
          (result "")
          (current-para "")
          (para-start-time 0))
      (dolist (elem (cddr caption-dom)) ;; Process each text element
        (when (and (listp elem) (eq (car elem) 'text))
          (let* ((attrs (cadr elem))
                 (text (caddr elem))
                 (start (string-to-number (cdr (assoc 'start attrs))))
                 ;; Check if we've crossed into a new chunk-time boundary
                 (should-chunk (and (> (abs (- start para-start-time)) 3)
                                    (not (= (floor para-start-time chunk-time)
                                            (floor start chunk-time))))))
            (when (and should-chunk (> (length current-para) 0))
              ;; Add completed paragraph
              (setq result (concat result
                                   (format "[%d:%02d]\n%s\n\n"
                                           (floor para-start-time 60)
                                           (mod para-start-time 60)
                                           (string-trim current-para))))
              (setq current-para "")
              (setq para-start-time start))

            (when text
              (setq current-para (concat current-para " " text))))))

      ;; Add final paragraph
      (when (> (length current-para) 0)
        (setq result (concat result
                             (format "[%d:%02d]\n%s\n\n"
                                     (floor para-start-time 60)
                                     (mod para-start-time 60)
                                     (string-trim current-para)))))
      result)))


;;
;;; Renderers

(defun mevedel-tool-web--render-transform (name args result)
  "Return bounded render metadata for WebSearch NAME with ARGS and RESULT."
  (list :kind 'web
        :tool name
        :query (plist-get args :query)
        :results (mevedel-tool-web--result-count result)
        :chars (length result)))

(defun mevedel-tool-web--result-count (result)
  "Return the number of numbered search entries in RESULT."
  (let ((count 0) (start 0))
    (while (string-match "^[0-9]+\\. " result start)
      (setq count (1+ count) start (match-end 0)))
    count))

(defun mevedel-tool-web--render-fetch (name args result render-data)
  "Return rendering plist for NAME using ARGS, RESULT, and RENDER-DATA.
Header shows the URL's host, the final host after redirects, and the
fetched size, status and content type; body fontifies in the data
buffer's major mode.  The view parser passes renderers unescaped tool
results, so `org-mode' storage escapes are not shown in the expanded
body."
  (when (stringp result)
    (let* ((url (plist-get args :url))
           (host (or (plist-get render-data :host)
                     (mevedel-tool-web--url-host url) url "?"))
           (final (plist-get render-data :final-host))
           (bytes (plist-get render-data :bytes))
           (details
            (cond
             ((plist-get render-data :redirect) "redirect needs approval")
             (bytes (string-join
                     (delq nil (list (file-size-human-readable bytes nil " " "B")
                                     (and-let* ((code (plist-get render-data :code)))
                                       (number-to-string code))
                                     (plist-get render-data :content-type)))
                     ", "))
             (t (format "%d chars" (or (plist-get render-data :chars)
                                       (length result)))))))
      (list :header (format "%s: %s%s \u2014 %s"
                            (or name "WebFetch") host
                            (if (and final (not (equal final host)))
                                (concat " \u2192 " final)
                              "")
                            details)
            :body result
            :body-mode (mevedel-view-data-buffer-major-mode)
            :initially-collapsed-p t))))

(defun mevedel-tool-web--render-search (name args result render-data)
  "Return rendering plist for NAME using ARGS, RESULT, and RENDER-DATA.
Header shows the query and result count; body fontifies in the data
buffer's major mode (see `mevedel-tool-web--render-fetch' for why)."
  (when (stringp result)
    (let* ((query (or (plist-get args :query) ""))
           (count (or (plist-get render-data :results)
                      (mevedel-tool-web--result-count result))))
      (list :header (format "%s: %s (%d result%s)"
                            (or name "WebSearch") query count
                            (if (= count 1) "" "s"))
            :body result
            :body-mode (mevedel-view-data-buffer-major-mode)
            :initially-collapsed-p t))))

(provide 'mevedel-tool-web)
;;; mevedel-tool-web.el ends here
