;;; niwa-shared-library.el --- Whiteboard element libraries -*- lexical-binding: t; -*-

;;; Commentary:

;; Whiteboard element libraries kept on the session host: the personal
;; library, libraries installed from the public collection at
;; libraries.excalidraw.com or dropped into the library directory, and
;; niwa's built-in library.  Every writable whiteboard editor and the
;; model see the same libraries.  Emacs fetches the collection because the
;; browser editor has no network access.  Items stay opaque JSON here; the
;; editor and helper validate them when they are inserted into a board.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'niwa-structs)

;; `niwa-tool-web'
(declare-function niwa-tool-web--retrieve "niwa-tool-web" (url parse callback))

(defcustom niwa-shared-library-directory
  (file-name-concat niwa-user-dir "whiteboard-libraries")
  "Directory of `.excalidrawlib' libraries offered to whiteboard editors.
Each file is one library named after the file.  Installing from the public
collection adds a file; any Excalidraw library copied here appears too."
  :type 'directory :group 'niwa)

(defcustom niwa-shared-library-catalog-url "https://libraries.excalidraw.com/"
  "Base URL of the public library collection.
It serves the index `libraries.json', download counts in `stats.json' and
each library under `libraries/'."
  :type 'string :group 'niwa)

(defconst niwa-shared-library--personal "My library"
  "Name of the personal library that Add selection writes to.")

(defconst niwa-shared-library--builtin
  (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                    "shared-editing" "builtin.excalidrawlib")
  "The built-in library packaged with niwa.")

(defconst niwa-shared-library--limit (* 8 1024 1024)
  "Maximum size in bytes of a library file or fetched library.")

(defun niwa-shared-library--parse (text)
  "Parse library JSON TEXT and return its items as a vector."
  (when (> (string-bytes text) niwa-shared-library--limit)
    (error "The library is larger than 8 MiB"))
  (let ((data (json-parse-string text :object-type 'hash-table :array-type 'array
                                 :null-object :null :false-object :false)))
    (unless (and (hash-table-p data) (equal (gethash "type" data) "excalidrawlib"))
      (error "Not an Excalidraw library"))
    (let ((items (or (gethash "libraryItems" data) (gethash "library" data) [])))
      (unless (vectorp items) (error "Invalid library items"))
      items)))

(defun niwa-shared-library--text (items)
  "Return version 2 library JSON for ITEMS."
  (let ((data (make-hash-table :test #'equal)))
    (puthash "type" "excalidrawlib" data)
    (puthash "version" 2 data)
    (puthash "source" "https://github.com/FrauH0lle/niwa" data)
    (puthash "libraryItems" items data)
    (decode-coding-string
     (json-serialize data :null-object :null :false-object :false) 'utf-8-unix)))

(defun niwa-shared-library--name (name)
  "Return library NAME when it can name a file in the library directory."
  (unless (and (stringp name) (<= 1 (length name) 100)
               (not (string-match-p "\\`[.]\\|[/\\\\:*?\"<>|[:cntrl:]]" name)))
    (error "Invalid library name"))
  name)

(defun niwa-shared-library--file (name)
  "Return the file of library NAME in the library directory."
  (file-name-concat niwa-shared-library-directory
                    (concat (niwa-shared-library--name name) ".excalidrawlib")))

(defun niwa-shared-library--read (file)
  "Return the contents of library FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun niwa-shared-library-libraries ()
  "Return the host's libraries as plists of `:name', `:kind' and `:text'.
Kinds are \"personal\", \"installed\" and \"builtin\", in that order.
Unreadable files are skipped, so one broken library hides no others."
  (let* ((files (and (file-directory-p niwa-shared-library-directory)
                     (directory-files niwa-shared-library-directory t
                                      "\\`[^.].*\\.excalidrawlib\\'")))
         (libraries
          (delq nil
                (mapcar
                 (lambda (file)
                   (let ((name (file-name-base file)))
                     (condition-case nil
                         (let ((text (niwa-shared-library--read file)))
                           (niwa-shared-library--parse text)
                           (list :name name :text text
                                 :kind (if (equal name niwa-shared-library--personal)
                                           "personal" "installed")))
                       (error nil))))
                 files))))
    (append (cl-sort libraries #'string<
                     :key (lambda (l) (concat (if (equal (plist-get l :kind) "personal") "0" "1")
                                              (plist-get l :name))))
            (list (list :name "Built-in" :kind "builtin"
                        :text (niwa-shared-library--read niwa-shared-library--builtin))))))

(defun niwa-shared-library--listing ()
  "Return the reply describing every library."
  (list :libraries (vconcat (niwa-shared-library-libraries))))

(defun niwa-shared-library--write (name text)
  "Write library TEXT as library NAME."
  (when (> (string-bytes text) niwa-shared-library--limit)
    (error "The library would exceed 8 MiB; remove items first"))
  (make-directory niwa-shared-library-directory t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil (niwa-shared-library--file name) nil 'silent)))

(defun niwa-shared-library--items (name)
  "Return the items of library NAME, or an empty vector."
  (let ((file (niwa-shared-library--file name)))
    (if (file-readable-p file)
        (niwa-shared-library--parse (niwa-shared-library--read file))
      [])))

(defun niwa-shared-library--id (item)
  "Return the id of library ITEM, or nil for a version 1 element list."
  (and (hash-table-p item) (gethash "id" item)))

(defun niwa-shared-library--add (text)
  "Prepend the items of library TEXT not already in the personal library."
  (let* ((items (niwa-shared-library--items niwa-shared-library--personal))
         (known (delq nil (mapcar #'niwa-shared-library--id items)))
         (added (cl-remove-if (lambda (item) (member (niwa-shared-library--id item) known))
                              (append (niwa-shared-library--parse text) nil))))
    (niwa-shared-library--write niwa-shared-library--personal
                                   (niwa-shared-library--text (vconcat added items)))))

(defun niwa-shared-library--remove (ids)
  "Remove the items with IDS from the personal library."
  (unless (and (vectorp ids) (cl-every #'stringp ids)) (error "Invalid library item ids"))
  (niwa-shared-library--write
   niwa-shared-library--personal
   (niwa-shared-library--text
    (cl-remove-if (lambda (item) (member (niwa-shared-library--id item) (append ids nil)))
                  (niwa-shared-library--items niwa-shared-library--personal)))))

(defun niwa-shared-library--uninstall (name)
  "Delete installed library NAME."
  (when (member name (list niwa-shared-library--personal "Built-in"))
    (error "Only installed libraries can be removed"))
  (let ((file (niwa-shared-library--file name)))
    (unless (file-exists-p file) (error "Library %s is not installed" name))
    (delete-file file)))

(defun niwa-shared-library-text (library)
  "Return the text of host library LIBRARY.
The helper resolves item references, since it derives the ids of version 1
items that carry none."
  (or (plist-get (cl-find library (niwa-shared-library-libraries)
                          :key (lambda (l) (plist-get l :name)) :test #'equal)
                 :text)
      (error "No library %s; Read shared://library for installed libraries" library)))

(defun niwa-shared-library--fetch (path parse callback)
  "Fetch PATH below the catalog URL, PARSE its body, then call CALLBACK.
CALLBACK receives a reply plist with `:result' or `:error'."
  (require 'niwa-tool-web)
  (niwa-tool-web--retrieve
   (concat (file-name-as-directory niwa-shared-library-catalog-url) path)
   (lambda ()
     (when (> (- (point-max) (point)) niwa-shared-library--limit)
       (error "The library is larger than 8 MiB"))
     ;; Point is at the end of the headers; the JSON body follows.
     (funcall parse (string-trim (decode-coding-string
                                  (buffer-substring-no-properties (point) (point-max)) 'utf-8))))
   (lambda (value error)
     (funcall callback (if error (list :error (format "Library request failed: %s" error))
                         (list :result value))))))

(defun niwa-shared-library--source (source)
  "Return SOURCE when it names a library file of the public collection."
  ;; Only library files of the public collection, never another path or host.
  (unless (and (stringp source)
               (string-match-p "\\`[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+\\.excalidrawlib\\'" source)
               (not (string-match-p "\\.\\." source)))
    (error "Invalid library source"))
  source)

(defun niwa-shared-library--catalog-entry (entry stats)
  "Return the fields of catalog ENTRY, with downloads from STATS, for the editor."
  (let* ((authors (gethash "authors" entry []))
         (source (gethash "source" entry))
         (key (and (stringp source)
                   (replace-regexp-in-string
                    "/" "-" (string-remove-suffix ".excalidrawlib" (downcase source)))))
         (counts (and key (hash-table-p stats) (gethash key stats))))
    (list :name (string-trim (format "%s" (gethash "name" entry "")))
          :description (format "%s" (gethash "description" entry ""))
          :authors (mapconcat (lambda (author)
                                (format "%s" (if (hash-table-p author) (gethash "name" author "") author)))
                              (if (vectorp authors) authors []) ", ")
          :created (format "%s" (gethash "created" entry ""))
          :updated (format "%s" (gethash "updated" entry ""))
          :downloads (let ((total (and (hash-table-p counts) (gethash "total" counts))))
                       (if (numberp total) total 0))
          :source source)))

(defun niwa-shared-library-handle (args callback)
  "Run library request ARGS and call CALLBACK with a reply plist.
Library contents travel as Excalidraw library JSON text."
  (condition-case err
      (pcase (plist-get args :action)
        ("library" (funcall callback (list :result (niwa-shared-library--listing))))
        ("library-add"
         (niwa-shared-library--add (plist-get args :text))
         (funcall callback (list :result (niwa-shared-library--listing))))
        ("library-remove"
         (niwa-shared-library--remove (plist-get args :ids))
         (funcall callback (list :result (niwa-shared-library--listing))))
        ("library-uninstall"
         (niwa-shared-library--uninstall (plist-get args :name))
         (funcall callback (list :result (niwa-shared-library--listing))))
        ("library-install"
         (let ((name (niwa-shared-library--name (plist-get args :name))))
           (when (member name (list niwa-shared-library--personal "Built-in"))
             (error "Choose another library name"))
           (niwa-shared-library--fetch
            (concat "libraries/" (niwa-shared-library--source (plist-get args :source)))
            (lambda (text)
              (niwa-shared-library--parse text)
              (niwa-shared-library--write name text)
              (niwa-shared-library--listing))
            callback)))
        ("library-catalog"
         (niwa-shared-library--fetch
          "libraries.json"
          (lambda (text)
            (let ((entries (json-parse-string text :object-type 'hash-table :array-type 'array
                                              :null-object nil :false-object nil)))
              (unless (vectorp entries) (error "Unexpected library index"))
              entries))
          (lambda (reply)
            (if (plist-get reply :error) (funcall callback reply)
              ;; Download counts are a nicety: list the libraries without them.
              (niwa-shared-library--fetch
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
                                                  (niwa-shared-library--catalog-entry
                                                   entry (plist-get stats :result)))
                                                (plist-get reply :result)))))))))))))
        ("library-fetch"
         (niwa-shared-library--fetch
          (concat "libraries/" (niwa-shared-library--source (plist-get args :source)))
          (lambda (text) (niwa-shared-library--parse text) (list :text text))
          callback))
        (_ (error "Unknown library request")))
    (error (funcall callback (list :error (error-message-string err))))))

(provide 'niwa-shared-library)
;;; niwa-shared-library.el ends here
