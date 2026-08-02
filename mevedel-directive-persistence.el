;;; mevedel-directive-persistence.el -- Directive record codec -*- lexical-binding: t -*-

;;; Commentary:

;; Serializes and validates workspace-owned directive activity records.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-structs'
(declare-function mevedel-directive--create "mevedel-structs" (&rest slots))
(declare-function mevedel-directive-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt--create
                  "mevedel-structs" (&rest slots))
(declare-function mevedel-directive-attempt-capture
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-captured-at
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-checkpoint
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-consumed-subdirectives
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-covered-files
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-directive-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-gaps
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-outcome
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-patch
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-result
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempt-sequence
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-attempts "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn--create
                  "mevedel-structs" (&rest slots))
(declare-function mevedel-directive-discussion-turn-attempt-index
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-checkpoint
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-directive-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-message
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-outcome
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-request
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-result
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-discussion-turn-sequence
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-state "mevedel-structs" (cl-x) t)
(declare-function mevedel-directive-subdirectives
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective--create
                  "mevedel-structs" (&rest slots))
(declare-function mevedel-subdirective-anchor "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-subdirective-request "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-directives "mevedel-structs" (cl-x) t)

;; `mevedel-utilities'
(declare-function mevedel--file-relative-name-or-absolute
                  "mevedel-utilities" (file directory))

(defun mevedel--serialize-directive-anchor (anchor base-directory)
  "Return serializable ANCHOR relative to BASE-DIRECTORY."
  (let ((copy (copy-tree anchor)))
    (when-let* ((file (plist-get copy :file))
                ((stringp base-directory)))
      (setq copy
            (plist-put copy :file
                       (mevedel--file-relative-name-or-absolute
                        file base-directory))))
    copy))

(defun mevedel--serialize-subdirective (subdirective base-directory)
  "Return SUBDIRECTIVE relative to BASE-DIRECTORY."
  (list :id (mevedel-subdirective-id subdirective)
        :request (substring-no-properties
                  (mevedel-subdirective-request subdirective))
        :anchor (mevedel--serialize-directive-anchor
                 (mevedel-subdirective-anchor subdirective)
                 base-directory)))

(defun mevedel--serialize-directives (workspace base-directory)
  "Return WORKSPACE directive records relative to BASE-DIRECTORY."
  (mapcar
   (lambda (directive)
     (list :id (mevedel-directive-id directive)
           :request (substring-no-properties
                     (mevedel-directive-request directive))
           :anchor (mevedel--serialize-directive-anchor
                    (mevedel-directive-anchor directive)
                    base-directory)
           :subdirectives
           (mapcar (lambda (subdirective)
                     (mevedel--serialize-subdirective
                      subdirective base-directory))
                   (mevedel-directive-subdirectives directive))
           :state (mevedel-directive-state directive)
           :session-id (mevedel-directive-session-id directive)
           :attempts
           (mapcar
            (lambda (attempt)
              (list
               :sequence (mevedel-directive-attempt-sequence attempt)
               :directive-request
               (substring-no-properties
                (mevedel-directive-attempt-directive-request attempt))
               :request (substring-no-properties
                         (mevedel-directive-attempt-request attempt))
               :result (substring-no-properties
                        (mevedel-directive-attempt-result attempt))
               :outcome (mevedel-directive-attempt-outcome attempt)
               :patch (substring-no-properties
                       (mevedel-directive-attempt-patch attempt))
               :capture (mevedel-directive-attempt-capture attempt)
               :covered-files
               (mapcar
                (lambda (file)
                  (mevedel--file-relative-name-or-absolute
                   file base-directory))
                (mevedel-directive-attempt-covered-files attempt))
               :gaps
               (mapcar
                (lambda (gap)
                  (cons
                   (mevedel--file-relative-name-or-absolute
                    (car gap) base-directory)
                   (cdr gap)))
                (mevedel-directive-attempt-gaps attempt))
               :captured-at
               (mevedel-directive-attempt-captured-at attempt)
               :checkpoint
               (copy-tree (mevedel-directive-attempt-checkpoint attempt))
               :consumed-subdirectives
               (mapcar (lambda (subdirective)
                         (mevedel--serialize-subdirective
                          subdirective base-directory))
                       (mevedel-directive-attempt-consumed-subdirectives
                        attempt))))
            (mevedel-directive-attempts directive))
           :discussion
           (mapcar
            (lambda (turn)
              (list
               :sequence
               (mevedel-directive-discussion-turn-sequence turn)
               :directive-request
               (substring-no-properties
                (mevedel-directive-discussion-turn-directive-request turn))
               :message
               (substring-no-properties
                (mevedel-directive-discussion-turn-message turn))
               :request
               (substring-no-properties
                (mevedel-directive-discussion-turn-request turn))
               :result
               (substring-no-properties
                (mevedel-directive-discussion-turn-result turn))
               :outcome
               (mevedel-directive-discussion-turn-outcome turn)
               :attempt-index
               (mevedel-directive-discussion-turn-attempt-index turn)
               :checkpoint
               (copy-tree
                (mevedel-directive-discussion-turn-checkpoint turn))))
            (mevedel-directive-discussion directive))))
   (mevedel-workspace-directives workspace)))

(defun mevedel--deserialize-subdirective (serialized base-directory)
  "Return a nested directive from SERIALIZED relative to BASE-DIRECTORY."
  (let ((id (plist-get serialized :id))
        (request (plist-get serialized :request))
        (anchor (copy-tree (plist-get serialized :anchor))))
    (unless (and (stringp id)
                 (stringp request)
                 (eq 'attached (plist-get anchor :state))
                 (stringp (plist-get anchor :file))
                 (natnump (plist-get anchor :start))
                 (natnump (plist-get anchor :end))
                 (listp (plist-get anchor :evidence))
                 (listp (plist-get anchor :properties)))
      (user-error "Malformed mevedel directive list"))
    (setq anchor
          (plist-put anchor :file
                     (expand-file-name (plist-get anchor :file)
                                       base-directory)))
    (mevedel-subdirective--create
     :id id :request request :anchor anchor)))

(defun mevedel--deserialize-directives (serialized base-directory)
  "Return directive records from SERIALIZED relative to BASE-DIRECTORY."
  (mapcar
   (lambda (entry)
     (let ((id (plist-get entry :id))
           (request (plist-get entry :request))
           (anchor (copy-tree (plist-get entry :anchor)))
           (subdirectives (plist-get entry :subdirectives))
           (session-id (plist-get entry :session-id))
           (attempts (plist-get entry :attempts))
           (discussion (plist-get entry :discussion)))
       (unless (and (stringp id) (stringp request)
                    (plist-member entry :subdirectives)
                    (listp subdirectives)
                    (plist-member entry :session-id)
                    (or (null session-id) (stringp session-id))
                    (plist-member entry :attempts)
                    (listp attempts)
                    (plist-member entry :discussion)
                    (listp discussion)
                    (memq (plist-get anchor :state)
                          '(attached detached source-missing archived))
                    (pcase (plist-get anchor :state)
                      ('attached t)
                      ('detached
                       (and (stringp (plist-get anchor :file))
                            (natnump (plist-get anchor :position))
                            (let ((order (plist-get anchor :source-order)))
                              (and (listp order)
                                   (= (length order) 2)
                                   (cl-every #'natnump order)))))
                      ((or 'source-missing 'archived)
                       (and (stringp (plist-get anchor :file))
                            (natnump (plist-get anchor :start))
                            (natnump (plist-get anchor :end))
                            (listp (plist-get anchor :evidence))
                            (listp (plist-get anchor :properties))))
                      (_ nil)))
         (user-error "Malformed mevedel directive list"))
       (when-let* ((file (plist-get anchor :file)))
         (setq anchor
               (plist-put anchor :file
                          (expand-file-name file base-directory))))
       (mevedel-directive--create
        :id id :request request :anchor anchor
        :subdirectives
        (mapcar (lambda (subdirective)
                  (mevedel--deserialize-subdirective
                   subdirective base-directory))
                subdirectives)
        :state (plist-get entry :state)
        :session-id session-id
        :attempts
        (mapcar
         (lambda (attempt)
           (let ((sequence (plist-get attempt :sequence))
                 (directive-request
                  (plist-get attempt :directive-request))
                 (attempt-request (plist-get attempt :request))
                 (result (plist-get attempt :result))
                 (outcome (plist-get attempt :outcome))
                 (patch (plist-get attempt :patch))
                 (capture (plist-get attempt :capture))
                 (covered-files (plist-get attempt :covered-files))
                 (gaps (plist-get attempt :gaps))
                 (captured-at (plist-get attempt :captured-at))
                 (checkpoint (plist-get attempt :checkpoint))
                 (consumed-subdirectives
                  (plist-get attempt :consumed-subdirectives)))
             (unless
                 (and (natnump sequence) (> sequence 0)
                      (stringp directive-request)
                      (stringp attempt-request)
                      (stringp result)
                      (memq outcome '(success error aborted))
                      (stringp patch)
                      (memq capture '(complete incomplete))
                      (listp covered-files)
                      (cl-every #'stringp covered-files)
                      (listp gaps)
                      (cl-every
                       (lambda (gap)
                         (and (consp gap) (stringp (car gap))))
                       gaps)
                      (stringp captured-at)
                      (plist-member attempt :consumed-subdirectives)
                      (listp consumed-subdirectives)
                      (stringp (plist-get checkpoint :session-id))
                      (natnump (plist-get checkpoint :turn)))
               (user-error "Malformed mevedel directive list"))
             (mevedel-directive-attempt--create
              :sequence sequence
              :directive-request directive-request
              :request attempt-request :result result
              :outcome outcome :patch patch :capture capture
              :covered-files
              (mapcar (lambda (file)
                        (expand-file-name file base-directory))
                      covered-files)
              :gaps
              (mapcar (lambda (gap)
                        (cons (expand-file-name (car gap) base-directory)
                              (cdr gap)))
                      gaps)
              :captured-at captured-at
              :checkpoint (copy-tree checkpoint)
              :consumed-subdirectives
              (mapcar (lambda (subdirective)
                        (mevedel--deserialize-subdirective
                         subdirective base-directory))
                      consumed-subdirectives))))
         attempts)
        :discussion
        (mapcar
         (lambda (turn)
           (let ((sequence (plist-get turn :sequence))
                 (directive-request (plist-get turn :directive-request))
                 (message (plist-get turn :message))
                 (turn-request (plist-get turn :request))
                 (result (plist-get turn :result))
                 (outcome (plist-get turn :outcome))
                 (attempt-index (plist-get turn :attempt-index))
                 (checkpoint (plist-get turn :checkpoint)))
             (unless
                 (and (natnump sequence) (> sequence 0)
                      (stringp directive-request)
                      (stringp message)
                      (stringp turn-request)
                      (stringp result)
                      (memq outcome '(success error aborted))
                      (or (null attempt-index)
                          (and (natnump attempt-index)
                               (> attempt-index 0)))
                      (stringp (plist-get checkpoint :session-id))
                      (natnump (plist-get checkpoint :turn)))
               (user-error "Malformed mevedel directive list"))
             (mevedel-directive-discussion-turn--create
              :sequence sequence
              :directive-request directive-request
              :message message :request turn-request :result result
              :outcome outcome :attempt-index attempt-index
              :checkpoint (copy-tree checkpoint))))
         discussion))))
   serialized))

(provide 'mevedel-directive-persistence)

;;; mevedel-directive-persistence.el ends here
