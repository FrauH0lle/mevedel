;;; mevedel-shared-library.el --- Whiteboard element libraries -*- lexical-binding: t; -*-

;;; Commentary:

;; Whiteboard element libraries kept on the session host: the personal
;; library, libraries installed from the public collection at
;; libraries.excalidraw.com or dropped into the library directory, and
;; mevedel's built-in library.  Every writable whiteboard editor and the
;; model see the same libraries.  Emacs fetches the collection because the
;; browser editor has no network access.  Items stay opaque JSON here; the
;; editor and helper validate them when they are inserted into a board.

;;; Code:

(require 'cl-lib)
(require 'json)

;; `mevedel-tool-web'
(declare-function mevedel-tool-web--retrieve "mevedel-tool-web" (url parse callback))

(defcustom mevedel-shared-library-directory
  (file-name-concat user-emacs-directory "mevedel" "libraries")
  "Directory of `.excalidrawlib' libraries offered to whiteboard editors.
Each file is one library named after the file.  Installing from the public
collection adds a file; any Excalidraw library copied here appears too."
  :type 'directory :group 'mevedel)

(defcustom mevedel-shared-library-catalog-url "https://libraries.excalidraw.com/"
  "Base URL of the public library collection.
It serves the index `libraries.json', download counts in `stats.json' and
each library under `libraries/'."
  :type 'string :group 'mevedel)

(defconst mevedel-shared-library--personal "My library"
  "Name of the personal library that Add selection writes to.")

(defconst mevedel-shared-library--builtin
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "shared-editing" "builtin.excalidrawlib")
  "The built-in library packaged with mevedel.")

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

(defun mevedel-shared-library--name (name)
  "Return library NAME when it can name a file in the library directory."
  (unless (and (stringp name) (<= 1 (length name) 100)
               (not (string-match-p "\\`[.]\\|[/\\\\:*?\"<>|[:cntrl:]]" name)))
    (error "Invalid library name"))
  name)

(defun mevedel-shared-library--file (name)
  "Return the file of library NAME in the library directory."
  (file-name-concat mevedel-shared-library-directory
                    (concat (mevedel-shared-library--name name) ".excalidrawlib")))

(defun mevedel-shared-library--read (file)
  "Return the contents of library FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun mevedel-shared-library-libraries ()
  "Return the host's libraries as plists of `:name', `:kind' and `:text'.
Kinds are \"personal\", \"installed\" and \"builtin\", in that order.
Unreadable files are skipped, so one broken library hides no others."
  (let* ((files (and (file-directory-p mevedel-shared-library-directory)
                     (directory-files mevedel-shared-library-directory t
                                      "\\`[^.].*\\.excalidrawlib\\'")))
         (libraries
          (delq nil
                (mapcar
                 (lambda (file)
                   (let ((name (file-name-base file)))
                     (condition-case nil
                         (let ((text (mevedel-shared-library--read file)))
                           (mevedel-shared-library--parse text)
                           (list :name name :text text
                                 :kind (if (equal name mevedel-shared-library--personal)
                                           "personal" "installed")))
                       (error nil))))
                 files))))
    (append (cl-sort libraries #'string<
                     :key (lambda (l) (concat (if (equal (plist-get l :kind) "personal") "0" "1")
                                              (plist-get l :name))))
            (list (list :name "Built-in" :kind "builtin"
                        :text (mevedel-shared-library--read mevedel-shared-library--builtin))))))

(defun mevedel-shared-library--listing ()
  "Return the reply describing every library."
  (list :libraries (vconcat (mevedel-shared-library-libraries))))

(defun mevedel-shared-library--write (name text)
  "Write library TEXT as library NAME."
  (when (> (string-bytes text) mevedel-shared-library--limit)
    (error "The library would exceed 8 MiB; remove items first"))
  (make-directory mevedel-shared-library-directory t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil (mevedel-shared-library--file name) nil 'silent)))

(defun mevedel-shared-library--items (name)
  "Return the items of library NAME, or an empty vector."
  (let ((file (mevedel-shared-library--file name)))
    (if (file-readable-p file)
        (mevedel-shared-library--parse (mevedel-shared-library--read file))
      [])))

(defun mevedel-shared-library--id (item)
  "Return the id of library ITEM, or nil for a version 1 element list."
  (and (hash-table-p item) (gethash "id" item)))

(defun mevedel-shared-library--add (text)
  "Prepend the items of library TEXT not already in the personal library."
  (let* ((items (mevedel-shared-library--items mevedel-shared-library--personal))
         (known (delq nil (mapcar #'mevedel-shared-library--id items)))
         (added (cl-remove-if (lambda (item) (member (mevedel-shared-library--id item) known))
                              (append (mevedel-shared-library--parse text) nil))))
    (mevedel-shared-library--write mevedel-shared-library--personal
                                   (mevedel-shared-library--text (vconcat added items)))))

(defun mevedel-shared-library--remove (ids)
  "Remove the items with IDS from the personal library."
  (unless (and (vectorp ids) (cl-every #'stringp ids)) (error "Invalid library item ids"))
  (mevedel-shared-library--write
   mevedel-shared-library--personal
   (mevedel-shared-library--text
    (cl-remove-if (lambda (item) (member (mevedel-shared-library--id item) (append ids nil)))
                  (mevedel-shared-library--items mevedel-shared-library--personal)))))

(defun mevedel-shared-library--uninstall (name)
  "Delete installed library NAME."
  (when (member name (list mevedel-shared-library--personal "Built-in"))
    (error "Only installed libraries can be removed"))
  (let ((file (mevedel-shared-library--file name)))
    (unless (file-exists-p file) (error "Library %s is not installed" name))
    (delete-file file)))

(defun mevedel-shared-library-item (library id)
  "Return library LIBRARY's text and check that it holds item ID."
  (let ((text (plist-get (cl-find library (mevedel-shared-library-libraries)
                                  :key (lambda (l) (plist-get l :name)) :test #'equal)
                         :text)))
    (unless (and text (cl-find id (mevedel-shared-library--parse text)
                               :key #'mevedel-shared-library--id :test #'equal))
      (error "No library item %s/%s; list items with SharedRead :library t" library id))
    text))

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

(defun mevedel-shared-library--source (source)
  "Return SOURCE when it names a library file of the public collection."
  ;; Only library files of the public collection, never another path or host.
  (unless (and (stringp source)
               (string-match-p "\\`[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+\\.excalidrawlib\\'" source)
               (not (string-match-p "\\.\\." source)))
    (error "Invalid library source"))
  source)

(defun mevedel-shared-library--catalog-entry (entry stats)
  "Return the fields of catalog ENTRY, with downloads from STATS, for the editor."
  (let* ((authors (gethash "authors" entry []))
         (source (gethash "source" entry))
         (key (and (stringp source)
                   (replace-regexp-in-string
                    "/" "-" (string-remove-suffix ".excalidrawlib" (downcase source)))))
         (counts (and key (hash-table-p stats) (gethash key stats))))
    (list :name (format "%s" (gethash "name" entry ""))
          :description (format "%s" (gethash "description" entry ""))
          :authors (mapconcat (lambda (author)
                                (format "%s" (if (hash-table-p author) (gethash "name" author "") author)))
                              (if (vectorp authors) authors []) ", ")
          :created (format "%s" (gethash "created" entry ""))
          :updated (format "%s" (gethash "updated" entry ""))
          :downloads (let ((total (and (hash-table-p counts) (gethash "total" counts))))
                       (if (numberp total) total 0))
          :source source)))

(defun mevedel-shared-library-handle (args callback)
  "Run library request ARGS and call CALLBACK with a reply plist.
Library contents travel as Excalidraw library JSON text."
  (condition-case err
      (pcase (plist-get args :action)
        ("library" (funcall callback (list :result (mevedel-shared-library--listing))))
        ("library-add"
         (mevedel-shared-library--add (plist-get args :text))
         (funcall callback (list :result (mevedel-shared-library--listing))))
        ("library-remove"
         (mevedel-shared-library--remove (plist-get args :ids))
         (funcall callback (list :result (mevedel-shared-library--listing))))
        ("library-uninstall"
         (mevedel-shared-library--uninstall (plist-get args :name))
         (funcall callback (list :result (mevedel-shared-library--listing))))
        ("library-install"
         (let ((name (mevedel-shared-library--name (plist-get args :name))))
           (when (member name (list mevedel-shared-library--personal "Built-in"))
             (error "Choose another library name"))
           (mevedel-shared-library--fetch
            (concat "libraries/" (mevedel-shared-library--source (plist-get args :source)))
            (lambda (text)
              (mevedel-shared-library--parse text)
              (mevedel-shared-library--write name text)
              (mevedel-shared-library--listing))
            callback)))
        ("library-catalog"
         (mevedel-shared-library--fetch
          "libraries.json"
          (lambda (text)
            (let ((entries (json-parse-string text :object-type 'hash-table :array-type 'array
                                              :null-object nil :false-object nil)))
              (unless (vectorp entries) (error "Unexpected library index"))
              entries))
          (lambda (reply)
            (if (plist-get reply :error) (funcall callback reply)
              ;; Download counts are a nicety: list the libraries without them.
              (mevedel-shared-library--fetch
               "stats.json"
               (lambda (text) (json-parse-string text :object-type 'hash-table))
               (lambda (stats)
                 (funcall callback
                          (list :result
                                (list :libraries
                                      (vconcat
                                       (cl-remove-if-not
                                        (lambda (entry) (stringp (plist-get entry :source)))
                                        (mapcar (lambda (entry)
                                                  (mevedel-shared-library--catalog-entry
                                                   entry (plist-get stats :result)))
                                                (plist-get reply :result)))))))))))))
        ("library-fetch"
         (mevedel-shared-library--fetch
          (concat "libraries/" (mevedel-shared-library--source (plist-get args :source)))
          (lambda (text) (mevedel-shared-library--parse text) (list :text text))
          callback))
        (_ (error "Unknown library request")))
    (error (funcall callback (list :error (error-message-string err))))))

(provide 'mevedel-shared-library)
;;; mevedel-shared-library.el ends here
