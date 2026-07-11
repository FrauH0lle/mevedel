;;; mevedel-tool-media.el -- Tool media storage and serialization -*- lexical-binding: t -*-

;;; Commentary:

;; Owns tool-result media validation, opaque side-channel storage,
;; transcript scrubbing, and provider payload conversion.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function gptel--model-capable-p "ext:gptel-request"
                  (cap &optional model))
(declare-function gptel--model-mime-capable-p "ext:gptel-request"
                  (mime &optional model))

(defvar read-eval)

(defconst mevedel-tool-media--data-open "<!-- mevedel-media-data -->"
  "Opening delimiter marking a hidden tool media side-channel block.")

(defconst mevedel-tool-media--data-close "<!-- /mevedel-media-data -->"
  "Closing delimiter marking the end of a tool media side-channel block.")
(defvar mevedel-tool-media--store (make-hash-table :test #'equal)
  "In-memory lookup table for media side-channel records.")

(defun mevedel-tool-media--store-id ()
  "Return a fresh opaque id for a media side-channel record."
  (secure-hash 'sha256
               (format "%S:%S:%S" (current-time) (emacs-pid) (random t))))

(defun mevedel-tool-media--store-dir (tool-results-dir &optional create)
  "Return media directory below TOOL-RESULTS-DIR.
When CREATE is non-nil, create the directory first."
  (when (stringp tool-results-dir)
    (let ((dir (file-name-concat tool-results-dir "media")))
      (when create (make-directory dir t))
      (and (or create (file-directory-p dir)) dir))))

(defun mevedel-tool-media--write-media-store-record
    (id items tool-results-dir tool-use-id)
  "Persist ITEMS under ID in TOOL-RESULTS-DIR, returning the file path.

TOOL-USE-ID records the tool call that owns the media."
  (when-let* ((dir (mevedel-tool-media--store-dir tool-results-dir t)))
    (let ((file (file-name-concat dir (concat "media-" id ".el"))))
      (with-temp-buffer
        (let ((print-level nil)
              (print-length nil)
              (print-circle t))
          (prin1 (list :version 1 :id id
                       :tool-use-id tool-use-id
                       :items items)
                 (current-buffer)))
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region nil nil file nil 'silent)))
      file)))

(defun mevedel-tool-media--read-media-store-record
    (id tool-results-dir expected-tool-use-id)
  "Return media items for ID from TOOL-RESULTS-DIR.
EXPECTED-TOOL-USE-ID rejects stale records when non-nil."
  (when-let* ((id (and (stringp id) id))
              (dir (mevedel-tool-media--store-dir tool-results-dir))
              (file (file-name-concat dir (concat "media-" id ".el"))))
    (when (and (file-readable-p file)
               (string-match-p (rx string-start (+ hex) string-end) id))
      (condition-case _
          (let* ((read-eval nil)
                 (record (with-temp-buffer
                           (insert-file-contents-literally file)
                           (read (current-buffer))))
                 (items (and (equal id (plist-get record :id))
                             (equal expected-tool-use-id
                                    (plist-get record :tool-use-id))
                             (plist-get record :items))))
            (mevedel-tool-media-normalize-items items))
        (error nil)))))

(defun mevedel-tool-media--store-record-items
    (record id payload-tool-use-id expected-tool-use-id)
  "Return media items from RECORD when provenance matches.
ID and PAYLOAD-TOOL-USE-ID come from the model-visible side-channel
payload.  EXPECTED-TOOL-USE-ID comes from the gptel tool-call record."
  (let ((items (and (equal id (plist-get record :id))
                    (equal payload-tool-use-id
                           (plist-get record :tool-use-id))
                    (or (null expected-tool-use-id)
                        (equal expected-tool-use-id payload-tool-use-id))
                    (plist-get record :items))))
    (mevedel-tool-media-normalize-items items)))

(defun mevedel-tool-media--store-media-data
    (media &optional tool-results-dir tool-use-id)
  "Store MEDIA in TOOL-RESULTS-DIR, returning a side-channel reference.

TOOL-USE-ID records the tool call that owns the media."
  (when-let* ((items (mevedel-tool-media-normalize-items media)))
    (let* ((id (mevedel-tool-media--store-id))
           (record (list :version 1 :id id
                         :tool-use-id tool-use-id
                         :items items)))
      (mevedel-tool-media--write-media-store-record
       id items tool-results-dir tool-use-id)
      (puthash id record mevedel-tool-media--store)
      (append (list :id id)
              (when (stringp tool-use-id)
                (list :tool-use-id
                      (substring-no-properties tool-use-id)))))))

(defun mevedel-tool-media--format-media-data-block
    (media &optional tool-results-dir tool-use-id)
  "Return serialized side-channel block for MEDIA.

TOOL-RESULTS-DIR and TOOL-USE-ID describe the tool call owner."
  (if-let* ((ref (mevedel-tool-media--store-media-data
                  media tool-results-dir tool-use-id)))
      (propertize
       (concat "\n" mevedel-tool-media--data-open "\n"
               (let ((print-level nil)
                     (print-length nil)
                     (print-circle t))
                 (prin1-to-string ref))
               "\n" mevedel-tool-media--data-close "\n")
       'invisible t
       'mevedel-media-data t)
    ""))

(defun mevedel-tool-media-attach-result
    (result media tool-results-dir tool-use-id)
  "Attach MEDIA to RESULT using TOOL-RESULTS-DIR and TOOL-USE-ID."
  (if (and (stringp result) media)
      (concat (mevedel-tool-media--envelope-summary result)
              (mevedel-tool-media--format-media-data-block
               media tool-results-dir tool-use-id))
    result))

(defun mevedel-tool-media-strip-blocks (string)
  "Return STRING with generated media side-channel blocks removed.
Literal delimiter text is left intact; only blocks carrying the
pipeline-owned `mevedel-media-data' text property are treated as hidden
side-channel data."
  (if (not (stringp string))
      string
    (let ((last 0)
          (search 0)
          (chunks nil)
          open)
      (while (setq open (string-search mevedel-tool-media--data-open
                                       string search))
        (if (not (get-text-property open 'mevedel-media-data string))
            (setq search (+ open (length mevedel-tool-media--data-open)))
          (let ((close (string-search mevedel-tool-media--data-close
                                      string open)))
            (if (null close)
                (setq search (+ open (length mevedel-tool-media--data-open)))
              (let* ((strip-start
                      (if (and (> open 0)
                               (= (aref string (1- open)) ?\n)
                               (get-text-property (1- open)
                                                  'mevedel-media-data string))
                          (1- open)
                        open))
                     (after-close (+ close
                                     (length
                                      mevedel-tool-media--data-close)))
                     (strip-end
                      (if (and (< after-close (length string))
                               (= (aref string after-close) ?\n)
                               (get-text-property after-close
                                                  'mevedel-media-data string))
                          (1+ after-close)
                        after-close)))
                (push (substring string last strip-start) chunks)
                (setq last strip-end)
                (setq search strip-end))))))
      (push (substring string last) chunks)
      (apply #'concat (nreverse chunks)))))

(defun mevedel-tool-media--read-media-reference
    (reference tool-results-dir expected-tool-use-id)
  "Return media stored for REFERENCE when its provenance matches.
TOOL-RESULTS-DIR selects persisted storage and EXPECTED-TOOL-USE-ID
selects the owning tool call."
  (let ((id (plist-get reference :id))
        (tool-use-id (plist-get reference :tool-use-id)))
    (or (and (stringp id)
             (mevedel-tool-media--store-record-items
              (gethash id mevedel-tool-media--store)
              id tool-use-id expected-tool-use-id))
        (and expected-tool-use-id
             (equal expected-tool-use-id tool-use-id)
             (mevedel-tool-media--read-media-store-record
              id tool-results-dir expected-tool-use-id)))))

(defun mevedel-tool-media--candidate-data
    (payload tool-results-dir expected-tool-use-id
             allow-payload-tool-use-id terminal-p)
  "Return trusted media for a candidate side-channel PAYLOAD.
TOOL-RESULTS-DIR selects persisted storage.  EXPECTED-TOOL-USE-ID is
preferred; when it is nil, ALLOW-PAYLOAD-TOOL-USE-ID and TERMINAL-P may
  authorize the id in the terminal payload itself."
  (condition-case _
      (let* ((read-eval nil)
             (reference (read payload))
             (payload-tool-use-id (plist-get reference :tool-use-id)))
        (or (and expected-tool-use-id
                 (mevedel-tool-media--read-media-reference
                  reference tool-results-dir expected-tool-use-id))
            (and allow-payload-tool-use-id terminal-p payload-tool-use-id
                 (mevedel-tool-media--read-media-reference
                  reference tool-results-dir payload-tool-use-id))))
    (error nil)))

(defun mevedel-tool-media-extract
    (result-string &optional tool-results-dir expected-tool-use-id
                   allow-payload-tool-use-id)
  "Return (VISIBLE-PART . MEDIA) parsed from RESULT-STRING.
VISIBLE-PART is the tool result with media side-channel blocks stripped.
MEDIA is the Lisp object deserialized from inside the block, or nil.
TOOL-RESULTS-DIR, EXPECTED-TOOL-USE-ID, and ALLOW-PAYLOAD-TOOL-USE-ID
control trusted side-channel lookup."
  (if (not (stringp result-string))
      (cons result-string nil)
    (let ((open nil)
          (close nil)
          (data nil)
          (search 0))
      (while (and (not open)
                  (setq open (string-search mevedel-tool-media--data-open
                                            result-string search)))
        (let* ((payload-start (+ open (length
                                       mevedel-tool-media--data-open)))
               (candidate-close
                (string-search mevedel-tool-media--data-close
                               result-string payload-start))
               (candidate-payload
                (and candidate-close
                     (string-trim
                      (substring result-string payload-start
                                 candidate-close))))
               (candidate-terminal-p
                (and candidate-close
                     (string-match-p
                      "\\`[ \t\n\r]*\\'"
                      (substring result-string
                                 (+ candidate-close
                                    (length
                                     mevedel-tool-media--data-close))))))
               (candidate-data
                (and candidate-payload
                     (mevedel-tool-media--candidate-data
                      candidate-payload tool-results-dir
                      expected-tool-use-id allow-payload-tool-use-id
                      candidate-terminal-p))))
          (if candidate-data
              (setq close candidate-close
                    data candidate-data)
            (setq search (+ open
                            (length mevedel-tool-media--data-open))
                  open nil))))
      (if open
          (let* ((strip-start (if (and (> open 0)
                                       (= (aref result-string (1- open)) ?\n))
                                  (1- open)
                                open))
                 (after-close (+ close
                                 (length
                                  mevedel-tool-media--data-close)))
                 (trail-end (if (and (< after-close (length result-string))
                                     (= (aref result-string after-close) ?\n))
                                (1+ after-close)
                              after-close)))
            (cons (concat (substring result-string 0 strip-start)
                          (substring result-string trail-end))
                  data))
        (cons result-string nil)))))

(defun mevedel-tool-media--valid-media-item-p (item)
  "Return non-nil when ITEM is a well-formed media side-channel plist."
  (let ((mime (and (listp item) (plist-get item :mime)))
        (kind (and (listp item) (plist-get item :kind)))
        (data (and (listp item) (plist-get item :data))))
    (and (listp item)
         (memq kind '(image document))
         (stringp mime)
         (member mime '("application/pdf"
                        "image/png" "image/jpeg" "image/gif" "image/webp"))
         (stringp data)
         (> (length data) 0))))

(defun mevedel-tool-media--valid-media-items-p (items)
  "Return non-nil when ITEMS is a non-empty list of valid media items."
  (and (consp items)
       (cl-every #'mevedel-tool-media--valid-media-item-p items)))

(defun mevedel-tool-media--sanitize-media-items (items)
  "Return ITEMS with only supported media side-channel keys retained."
  (mapcar
   (lambda (item)
     (let ((clean (list :mime (plist-get item :mime)
                        :kind (plist-get item :kind)
                        :data (plist-get item :data))))
       (when-let* ((path (plist-get item :path)))
         (when (stringp path)
           (setq clean (plist-put clean :path path))))
       (when-let* ((source (plist-get item :source)))
         (when (stringp source)
           (setq clean (plist-put clean :source source))))
       (when-let* ((page (plist-get item :page)))
         (when (integerp page)
           (setq clean (plist-put clean :page page))))
       clean))
   items))

(defun mevedel-tool-media-normalize-items (items)
  "Return sanitized media ITEMS, or nil when the collection is invalid."
  (when (mevedel-tool-media--valid-media-items-p items)
    (mevedel-tool-media--sanitize-media-items items)))

(defun mevedel-tool-media--envelope-summary (text &optional replacement)
  "Return TEXT with base64 media payload bodies replaced by REPLACEMENT."
  (let ((body-start-marker "data:\n")
        (body-end-marker "\n</media-file>")
        (summary (or replacement "<native media block attached>"))
        (pos 0)
        (chunks nil)
        (done nil))
    (while (not done)
      (let ((body-start (string-search body-start-marker text pos)))
        (if (not body-start)
            (setq done t)
          (let* ((payload-start (+ body-start (length body-start-marker)))
                 (body-end (string-search body-end-marker text payload-start)))
            (if (not body-end)
                (setq done t)
              (push (substring text pos payload-start) chunks)
              (push summary chunks)
              (setq pos body-end))))))
    (if chunks
        (mapconcat #'identity (nreverse (cons (substring text pos) chunks)) "")
      text)))

(defun mevedel-tool-media-result-for-hooks (result)
  "Return RESULT with media payloads and reference blocks hidden."
  (if (not (stringp result))
      result
    (mevedel-tool-media--envelope-summary
     (mevedel-tool-media-strip-blocks result)
     "<media omitted: media payload not exposed to hooks>")))

(defun mevedel-tool-media--anthropic-media-block (item)
  "Return an Anthropic content block for media ITEM."
  (let* ((mime (plist-get item :mime))
         ;; Replay only captured bytes; `:path' is metadata, never an input.
         (data (plist-get item :data))
         (type (cond
                ((and mime (string-prefix-p "image/" mime)) "image")
                ((equal mime "application/pdf") "document")
                (t nil))))
    (when (and type data)
      `(:type ,type
        :source (:type "base64"
                 :media_type ,mime
                 :data ,data)))))

(defun mevedel-tool-media--bedrock-media-block (item)
  "Return a Bedrock content block for media ITEM."
  (let* ((mime (plist-get item :mime))
         (path (plist-get item :path))
         (data (plist-get item :data))
         (image-format (cdr (assoc mime '(("image/jpg" . "jpeg")
                                          ("image/jpeg" . "jpeg")
                                          ("image/png" . "png")
                                          ("image/gif" . "gif")
                                          ("image/webp" . "webp")))))
         (doc-format (and (equal mime "application/pdf") "pdf")))
    (cond
     ((and image-format data)
      `(:image (:format ,image-format
                :source (:bytes ,data))))
     ((and doc-format data)
      `(:document (:format ,doc-format
                   :name ,(file-name-base (or path "document.pdf"))
                   :source (:bytes ,data)))))))

(defun mevedel-tool-media--data-url (item)
  "Return a data URL for media ITEM."
  (when-let* ((mime (plist-get item :mime))
              (data (plist-get item :data)))
    (concat "data:" mime ";base64," data)))

(defun mevedel-tool-media--openai-image-media-p (item)
  "Return non-nil when ITEM can be sent through gptel's OpenAI image path."
  (and (eq (plist-get item :kind) 'image)
       (string-prefix-p "image/" (or (plist-get item :mime) ""))))

(defun mevedel-tool-media--openai-responses-media-block (item)
  "Return an OpenAI Responses input_image block for media ITEM."
  (when-let* (((mevedel-tool-media--openai-image-media-p item))
              (url (mevedel-tool-media--data-url item)))
    `(:type "input_image" :image_url ,url)))

(defun mevedel-tool-media--openai-media-block (item)
  "Return an OpenAI chat-completions image_url block for media ITEM."
  (when-let* (((mevedel-tool-media--openai-image-media-p item))
              (url (mevedel-tool-media--data-url item)))
    `(:type "image_url" :image_url (:url ,url))))

(defun mevedel-tool-media--message-text (media)
  "Return a short model-facing description for attached MEDIA."
  (let ((paths (delq nil (mapcar (lambda (item) (plist-get item :path)) media))))
    (concat "Media returned by Read is attached as native input."
            (when paths
              (concat " Source: " (mapconcat #'identity paths ", "))))))

(defun mevedel-tool-media--openai-responses-media-message (media)
  "Return an OpenAI Responses user message carrying MEDIA."
  (when-let* ((blocks (delq nil
                            (mapcar
                             #'mevedel-tool-media--openai-responses-media-block
                             media))))
    (list :role "user"
          :content (vconcat
                    (list (list :type "input_text"
                                :text (mevedel-tool-media--message-text
                                       media)))
                    blocks))))

(defun mevedel-tool-media--openai-media-message (media)
  "Return an OpenAI chat-completions user message carrying MEDIA."
  (when-let* ((blocks (delq nil
                            (mapcar #'mevedel-tool-media--openai-media-block
                                    media))))
    (list :role "user"
          :content (vconcat
                    (list (list :type "text"
                                :text (mevedel-tool-media--message-text
                                       media)))
                    blocks))))

(defun mevedel-tool-media--backend-class-p (backend class)
  "Return non-nil when BACKEND is a cl-struct of CLASS."
  (and backend (ignore-errors (cl-typep backend class))))

(defun mevedel-tool-media--model-capable-p (media)
  "Return non-nil when the current model can accept all MEDIA items."
  (and (fboundp 'gptel--model-capable-p)
       (gptel--model-capable-p 'media)
       (cl-every
        (lambda (item)
          (let ((mime (plist-get item :mime)))
            (or (not (fboundp 'gptel--model-mime-capable-p))
                (gptel--model-mime-capable-p mime))))
        media)))

(defun mevedel-tool-media--supported-p (backend media)
  "Return non-nil when BACKEND and current model can receive MEDIA natively."
  (or (and (or (mevedel-tool-media--backend-class-p backend 'gptel-anthropic)
               (mevedel-tool-media--backend-class-p backend 'gptel-bedrock))
           (mevedel-tool-media--model-capable-p media))
      (and (or (mevedel-tool-media--backend-class-p backend 'gptel-openai)
               (mevedel-tool-media--backend-class-p
                backend 'gptel-openai-responses))
           (mevedel-tool-media--model-capable-p media)
           (cl-every #'mevedel-tool-media--openai-image-media-p media))))

(defun mevedel-tool-media--result-for-model (result media backend)
  "Return model-visible text for RESULT with MEDIA under BACKEND."
  (cond
   ((not (stringp result)) result)
   ((and media (mevedel-tool-media--supported-p backend media))
    (mevedel-tool-media--envelope-summary result))
   ((and media (not (mevedel-tool-media--model-capable-p media)))
    (mevedel-tool-media--envelope-summary
     result
     "<media omitted: current model does not support this media type>"))
   (media
    (mevedel-tool-media--envelope-summary
     result
     "<media omitted: backend cannot attach this media type>"))
   (t result)))

(defun mevedel-tool-media-prepare-tool-result
    (backend tool-call tool-results-dir)
  "Prepare TOOL-CALL text and media for BACKEND.
TOOL-RESULTS-DIR selects persisted media.  Return
(MODEL-RESULT . NATIVE-MEDIA)."
  (let* ((original (plist-get tool-call :result))
         (read-p (equal (plist-get tool-call :name) "Read"))
         (tool-use-id (plist-get tool-call :id))
         (extracted
          (and read-p tool-use-id (stringp original)
               (mevedel-tool-media-extract
                original tool-results-dir tool-use-id)))
         (media
          (and read-p tool-use-id
               (or (mevedel-tool-media-normalize-items
                    (plist-get tool-call :media))
                   (cdr extracted))))
         (without-media
          (and (stringp original)
               (if (and read-p tool-use-id)
                   (car (or extracted (cons original nil)))
                 (mevedel-tool-media-strip-blocks original))))
         (model-result
          (mevedel-tool-media--result-for-model without-media media backend))
         (native-media
          (and media (mevedel-tool-media--supported-p backend media) media)))
    (cons model-result native-media)))

(defun mevedel-tool-media-add-to-provider-result (backend parsed media-by-index)
  "Attach MEDIA-BY-INDEX to PARSED tool results when BACKEND supports it.
Returns PARSED unchanged for backends whose tool-result media shape is
unknown."
  (cond
   ((mevedel-tool-media--backend-class-p backend 'gptel-openai-responses)
    (append parsed
            (delq nil
                  (mapcar #'mevedel-tool-media--openai-responses-media-message
                          media-by-index))))
   ((mevedel-tool-media--backend-class-p backend 'gptel-openai)
    (append parsed
            (delq nil
                  (mapcar #'mevedel-tool-media--openai-media-message
                          media-by-index))))
   ((mevedel-tool-media--backend-class-p backend 'gptel-anthropic)
    (let* ((content (plist-get parsed :content))
           (blocks (and (vectorp content) (append content nil)))
           (i 0))
      (when blocks
        (plist-put
         parsed :content
         (vconcat
          (mapcar
           (lambda (block)
             (prog1
                 (if-let* ((media (nth i media-by-index)))
                     (let* ((text (plist-get block :content))
                            (native (delq nil
                                          (mapcar
                                           #'mevedel-tool-media--anthropic-media-block
                                           media))))
                       (if native
                           (plist-put
                            block :content
                            (vconcat
                             (list (list :type "text"
                                         :text (if (stringp text)
                                                   (mevedel-tool-media--envelope-summary text)
                                                 (prin1-to-string text))))
                             native))
                         block))
                   block)
               (cl-incf i)))
           blocks)))))
    parsed)
   ((mevedel-tool-media--backend-class-p backend 'gptel-bedrock)
    (let* ((content (plist-get parsed :content))
           (blocks (and (vectorp content) (append content nil)))
           (i 0))
      (when blocks
        (plist-put
         parsed :content
         (vconcat
          (mapcar
           (lambda (block)
             (prog1
                 (if-let* ((media (nth i media-by-index))
                           (tool-result (plist-get block :toolResult))
                           (native (delq nil
                                         (mapcar
                                          #'mevedel-tool-media--bedrock-media-block
                                          media))))
                     (progn
                       (plist-put
                        tool-result :content
                        (vconcat
                         (list (list :text
                                     (mevedel-tool-media--envelope-summary
                                      (or (plist-get (aref (plist-get tool-result :content) 0)
                                                     :text)
                                          ""))))
                         native))
                       block)
                   block)
               (cl-incf i)))
           blocks)))))
    parsed)
   (t parsed)))

(provide 'mevedel-tool-media)
;;; mevedel-tool-media.el ends here
