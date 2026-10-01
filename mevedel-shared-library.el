;;; mevedel-shared-library.el --- Whiteboard element library -*- lexical-binding: t; -*-

;;; Commentary:

;; The session host's personal Excalidraw library, offered to every writable
;; whiteboard editor in the room, and the public collection at
;; libraries.excalidraw.com.  Emacs fetches the collection because the
;; browser editor has no network access.  Items stay opaque JSON here; the
;; editor validates them when they are inserted into a board.

;;; Code:

(require 'cl-lib)
(require 'json)

;; `mevedel-tool-web'
(declare-function mevedel-tool-web--retrieve "mevedel-tool-web" (url parse callback))

(defcustom mevedel-shared-library-file
  (file-name-concat user-emacs-directory "mevedel" "library.excalidrawlib")
  "The personal `.excalidrawlib' library offered to whiteboard editors.
It uses Excalidraw's library format, so excalidraw.com and other editors
can use the same file."
  :type 'file :group 'mevedel)

(defcustom mevedel-shared-library-catalog-url "https://libraries.excalidraw.com/"
  "Base URL of the public library collection.
It serves the index `libraries.json' and each library under `libraries/'."
  :type 'string :group 'mevedel)

(defconst mevedel-shared-library--limit (* 8 1024 1024)
  "Maximum size in bytes of a library file or fetched library.")

(defun mevedel-shared-library--parse (text)
  "Parse library JSON TEXT and return its items as a vector."
  (when (> (string-bytes text) mevedel-shared-library--limit)
    (error "The library is larger than 8 MiB"))
  (let ((data (json-parse-string text :object-type 'hash-table :array-type 'array
                                 :null-object :null :false-object :false)))
    (unless (and (hash-table-p data) (equal (gethash "type" data) "excalidrawlib"))
      (error "Not an Excalidraw library"))
    (let ((items (or (gethash "libraryItems" data) (gethash "library" data) [])))
      (unless (vectorp items) (error "Invalid library items"))
      items)))

(defun mevedel-shared-library--text (items)
  "Return version 2 library JSON for ITEMS."
  (let ((data (make-hash-table :test #'equal)))
    (puthash "type" "excalidrawlib" data)
    (puthash "version" 2 data)
    (puthash "source" "https://github.com/FrauH0lle/mevedel" data)
    (puthash "libraryItems" items data)
    (decode-coding-string
     (json-serialize data :null-object :null :false-object :false) 'utf-8-unix)))

(defun mevedel-shared-library--items ()
  "Return the personal library's items, or an empty vector."
  (if (file-readable-p mevedel-shared-library-file)
      (with-temp-buffer
        (insert-file-contents mevedel-shared-library-file)
        (mevedel-shared-library--parse (buffer-string)))
    []))

(defun mevedel-shared-library--save (items)
  "Write ITEMS as the personal library and return its JSON text."
  (let ((text (mevedel-shared-library--text items)))
    (when (> (string-bytes text) mevedel-shared-library--limit)
      (error "The library would exceed 8 MiB; remove items first"))
    (make-directory (file-name-directory mevedel-shared-library-file) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region text nil mevedel-shared-library-file nil 'silent))
    text))

(defun mevedel-shared-library--id (item)
  "Return the id of library ITEM, or nil for a version 1 element list."
  (and (hash-table-p item) (gethash "id" item)))

(defun mevedel-shared-library--add (text)
  "Prepend the items of library TEXT not already in the personal library."
  (let* ((items (mevedel-shared-library--items))
         (known (delq nil (mapcar #'mevedel-shared-library--id items)))
         (added (cl-remove-if (lambda (item) (member (mevedel-shared-library--id item) known))
                              (append (mevedel-shared-library--parse text) nil))))
    (mevedel-shared-library--save (vconcat added items))))

(defun mevedel-shared-library--remove (ids)
  "Remove the items with IDS from the personal library."
  (unless (and (vectorp ids) (cl-every #'stringp ids)) (error "Invalid library item ids"))
  (mevedel-shared-library--save
   (cl-remove-if (lambda (item) (member (mevedel-shared-library--id item) (append ids nil)))
                 (mevedel-shared-library--items))))

(defun mevedel-shared-library--catalog-entry (entry)
  "Return the fields of catalog ENTRY that the editor shows."
  (let ((authors (gethash "authors" entry [])))
    (list :name (format "%s" (gethash "name" entry ""))
          :description (format "%s" (gethash "description" entry ""))
          :authors (mapconcat (lambda (author)
                                (format "%s" (if (hash-table-p author) (gethash "name" author "") author)))
                              (if (vectorp authors) authors []) ", ")
          :source (gethash "source" entry))))

(defun mevedel-shared-library--fetch (path parse callback)
  "Fetch PATH below the catalog URL, PARSE its body, then call CALLBACK.
CALLBACK receives a reply plist with `:result' or `:error'."
  (require 'mevedel-tool-web)
  (mevedel-tool-web--retrieve
   (concat (file-name-as-directory mevedel-shared-library-catalog-url) path)
   (lambda ()
     (when (> (- (point-max) (point)) mevedel-shared-library--limit)
       (error "The library is larger than 8 MiB"))
     ;; Point is at the end of the headers; the JSON body follows.
     (funcall parse (string-trim (decode-coding-string
                                  (buffer-substring-no-properties (point) (point-max)) 'utf-8))))
   (lambda (value error)
     (funcall callback (if error (list :error (format "Library request failed: %s" error))
                         (list :result value))))))

(defun mevedel-shared-library-handle (args callback)
  "Run library request ARGS and call CALLBACK with a reply plist.
Library contents are returned as Excalidraw library JSON text."
  (condition-case err
      (pcase (plist-get args :action)
        ("library"
         (funcall callback (list :result (list :text (mevedel-shared-library--text
                                                      (mevedel-shared-library--items))))))
        ("library-add"
         (funcall callback (list :result (list :text (mevedel-shared-library--add
                                                      (plist-get args :text))))))
        ("library-remove"
         (funcall callback (list :result (list :text (mevedel-shared-library--remove
                                                      (plist-get args :ids))))))
        ("library-catalog"
         (mevedel-shared-library--fetch
          "libraries.json"
          (lambda (text)
            (let ((entries (json-parse-string text :object-type 'hash-table :array-type 'array
                                              :null-object nil :false-object nil)))
              (unless (vectorp entries) (error "Unexpected library index"))
              (list :libraries
                    (vconcat (cl-remove-if-not
                              (lambda (entry) (stringp (plist-get entry :source)))
                              (mapcar #'mevedel-shared-library--catalog-entry entries))))))
          callback))
        ("library-fetch"
         (let ((source (plist-get args :source)))
           ;; Only library files of the public collection, never another path or host.
           (unless (and (stringp source)
                        (string-match-p
                         "\\`[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+\\.excalidrawlib\\'" source)
                        (not (string-match-p "\\.\\." source)))
             (error "Invalid library source"))
           (mevedel-shared-library--fetch
            (concat "libraries/" source)
            (lambda (text) (mevedel-shared-library--parse text) (list :text text))
            callback)))
        (_ (error "Unknown library request")))
    (error (funcall callback (list :error (error-message-string err))))))

(provide 'mevedel-shared-library)
;;; mevedel-shared-library.el ends here
