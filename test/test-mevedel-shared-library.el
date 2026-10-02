;;; test-mevedel-shared-library.el --- Whiteboard element libraries -*- lexical-binding: t; -*-

;;; Commentary:

;; The host's library directory and the public collection through the same
;; request handler the room bridge uses.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-shared-library)

(defun test-mevedel-shared-library--call (args)
  "Run library request ARGS and return its reply, waiting for fetches."
  (let (reply)
    (mevedel-shared-library-handle args (lambda (value) (setq reply value)))
    (let ((deadline (+ (float-time) 5)))
      (while (and (not reply) (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should reply)
    reply))

(defun test-mevedel-shared-library--names (reply)
  "Return the library names and kinds in listing REPLY."
  (mapcar (lambda (library) (cons (plist-get library :name) (plist-get library :kind)))
          (plist-get (plist-get reply :result) :libraries)))

(defconst test-mevedel-shared-library--box
  "{\"type\":\"excalidrawlib\",\"version\":2,\"libraryItems\":[{\"id\":\"box\",\"status\":\"unpublished\",\"elements\":[{\"id\":\"r\",\"type\":\"rectangle\",\"x\":0,\"y\":0,\"width\":10,\"height\":10,\"frameId\":null,\"locked\":false,\"customData\":{}}]}]}"
  "A one-item library with null, false and empty-object values.")

(mevedel-deftest mevedel-shared-library-handle
  (:doc "keeps personal and installed libraries in the library directory")
  (let ((mevedel-shared-library-directory (make-temp-file "mevedel-library-" t)))
    (unwind-protect
        (progn
          (should (equal '(("Built-in" . "builtin"))
                         (test-mevedel-shared-library--names
                          (test-mevedel-shared-library--call '(:action "library")))))
          (dotimes (_ 2)
            (test-mevedel-shared-library--call
             (list :action "library-add" :text test-mevedel-shared-library--box)))
          (let ((text (with-temp-buffer
                        (insert-file-contents (file-name-concat mevedel-shared-library-directory
                                                                "My library.excalidrawlib"))
                        (buffer-string))))
            (should (= 1 (length (mevedel-shared-library--parse text))))
            (should (string-match-p "\"frameId\":null,\"locked\":false,\"customData\":{}" text)))
          ;; A library copied into the directory appears beside the others.
          (copy-file (file-name-concat mevedel-shared-library-directory "My library.excalidrawlib")
                     (file-name-concat mevedel-shared-library-directory "Shapes.excalidrawlib"))
          (with-temp-file (file-name-concat mevedel-shared-library-directory "Broken.excalidrawlib")
            (insert "{"))
          (should (equal '(("My library" . "personal") ("Shapes" . "installed") ("Built-in" . "builtin"))
                         (test-mevedel-shared-library--names
                          (test-mevedel-shared-library--call '(:action "library")))))
          (should (string-match-p "\"box\"" (mevedel-shared-library-text "Shapes")))
          (should-error (mevedel-shared-library-text "Missing"))
          (should (string-match-p "builtin-database" (mevedel-shared-library-text "Built-in")))
          (should (equal '(("My library" . "personal") ("Built-in" . "builtin"))
                         (test-mevedel-shared-library--names
                          (test-mevedel-shared-library--call '(:action "library-uninstall" :name "Shapes")))))
          (dolist (name '("My library" "Built-in" "../escape"))
            (should (plist-get (test-mevedel-shared-library--call
                                (list :action "library-uninstall" :name name))
                               :error)))
          (let ((reply (test-mevedel-shared-library--call '(:action "library-remove" :ids ["box"]))))
            (should (equal "personal" (plist-get (aref (plist-get (plist-get reply :result) :libraries) 0) :kind)))
            (should (equal [] (mevedel-shared-library--parse
                               (plist-get (aref (plist-get (plist-get reply :result) :libraries) 0) :text)))))
          (should (string-match-p "Not an Excalidraw library"
                                  (plist-get (test-mevedel-shared-library--call
                                              '(:action "library-add" :text "{\"type\":\"excalidraw\"}"))
                                             :error))))
      (delete-directory mevedel-shared-library-directory t))))

(mevedel-deftest mevedel-shared-library--fetch
  (:doc "lists, previews and installs public libraries, and only their library files")
  (let ((mevedel-shared-library-directory (make-temp-file "mevedel-library-" t))
        (requests nil))
    (unwind-protect
        (mevedel-test-http
         (lambda (request)
           (push request requests)
           (cond
            ((string-match-p " /libraries.json " request)
             '("200 OK" "Content-Type: application/json\r\n"
               "[{\"name\":\" Boxes\",\"description\":\"Shapes\",\"authors\":[{\"name\":\"Ann\"},{\"name\":\"Bo\"}],\"source\":\"ann/boxes.excalidrawlib\",\"created\":\"2021-01-02\",\"updated\":\"2022-03-04\"},{\"name\":\"Broken\"}]"))
            ((string-match-p " /stats.json " request)
             '("200 OK" "Content-Type: application/json\r\n" "{\"ann-boxes\":{\"total\":42,\"week\":3}}"))
            ((string-match-p " /libraries/ann/boxes.excalidrawlib " request)
             (list "200 OK" "Content-Type: application/json\r\n" test-mevedel-shared-library--box))
            (t '("404 Not Found" "" "missing"))))
         (lambda (base)
           (let ((mevedel-shared-library-catalog-url (concat base "/")))
             (should (equal [(:name "Boxes" :description "Shapes" :authors "Ann, Bo"
                                    :created "2021-01-02" :updated "2022-03-04" :downloads 42
                                    :source "ann/boxes.excalidrawlib")]
                            (plist-get (plist-get (test-mevedel-shared-library--call
                                                   '(:action "library-catalog"))
                                                  :result)
                                       :libraries)))
             (should (equal test-mevedel-shared-library--box
                            (plist-get (plist-get (test-mevedel-shared-library--call
                                                   '(:action "library-fetch" :source "ann/boxes.excalidrawlib"))
                                                  :result)
                                       :text)))
             (should (equal '(("Boxes" . "installed") ("Built-in" . "builtin"))
                            (test-mevedel-shared-library--names
                             (test-mevedel-shared-library--call
                              '(:action "library-install" :source "ann/boxes.excalidrawlib" :name "Boxes")))))
             (should (file-exists-p (file-name-concat mevedel-shared-library-directory "Boxes.excalidrawlib")))
             (should (string-match-p "Library request failed"
                                     (plist-get (test-mevedel-shared-library--call
                                                 '(:action "library-fetch" :source "ann/missing.excalidrawlib"))
                                                :error)))
             (dolist (source '("../secret.excalidrawlib" "ann/../x.excalidrawlib" "http://evil/x.excalidrawlib"
                               "ann/boxes.json"))
               (should (equal "Invalid library source"
                              (plist-get (test-mevedel-shared-library--call
                                          (list :action "library-fetch" :source source))
                                         :error))))
             (should (= 5 (length requests))))))
      (delete-directory mevedel-shared-library-directory t))))

(provide 'test-mevedel-shared-library)
;;; test-mevedel-shared-library.el ends here
