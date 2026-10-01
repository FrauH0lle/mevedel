;;; test-mevedel-shared-library.el --- Whiteboard element library -*- lexical-binding: t; -*-

;;; Commentary:

;; The personal library file and the public collection through the same
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

(defun test-mevedel-shared-library--items (reply)
  "Return the item ids in library REPLY."
  (mapcar (lambda (item) (gethash "id" item))
          (mevedel-shared-library--parse (plist-get (plist-get reply :result) :text))))

(defconst test-mevedel-shared-library--box
  "{\"type\":\"excalidrawlib\",\"version\":2,\"libraryItems\":[{\"id\":\"box\",\"status\":\"unpublished\",\"elements\":[{\"id\":\"r\",\"type\":\"rectangle\",\"x\":0,\"y\":0,\"width\":10,\"height\":10,\"frameId\":null,\"locked\":false,\"customData\":{}}]}]}"
  "A one-item library with null, false and empty-object values.")

(mevedel-deftest mevedel-shared-library-handle
  (:doc "keeps a personal Excalidraw library file and adds each item once")
  (let* ((directory (make-temp-file "mevedel-library-" t))
         (mevedel-shared-library-file (file-name-concat directory "lib" "library.excalidrawlib")))
    (unwind-protect
        (progn
          (should (equal '() (test-mevedel-shared-library--items
                              (test-mevedel-shared-library--call '(:action "library")))))
          (dotimes (_ 2)
            (should (equal '("box") (test-mevedel-shared-library--items
                                     (test-mevedel-shared-library--call
                                      (list :action "library-add" :text test-mevedel-shared-library--box))))))
          (let ((text (with-temp-buffer
                        (insert-file-contents mevedel-shared-library-file)
                        (buffer-string))))
            (should (string-match-p "\"type\":\"excalidrawlib\"" text))
            (should (string-match-p "\"frameId\":null,\"locked\":false,\"customData\":{}" text)))
          (should (equal '() (test-mevedel-shared-library--items
                              (test-mevedel-shared-library--call
                               '(:action "library-remove" :ids ["box"])))))
          (should (string-match-p "Not an Excalidraw library"
                                  (plist-get (test-mevedel-shared-library--call
                                              '(:action "library-add" :text "{\"type\":\"excalidraw\"}"))
                                             :error))))
      (delete-directory directory t))))

(mevedel-deftest mevedel-shared-library--fetch
  (:doc "lists and fetches the public collection, and only its library files")
  (let ((requests nil))
    (mevedel-test-http
     (lambda (request)
       (push request requests)
       (cond
        ((string-match-p " /libraries.json " request)
         '("200 OK" "Content-Type: application/json\r\n"
           "[{\"name\":\"Boxes\",\"description\":\"Shapes\",\"authors\":[{\"name\":\"Ann\"},{\"name\":\"Bo\"}],\"source\":\"ann/boxes.excalidrawlib\"},{\"name\":\"Broken\"}]"))
        ((string-match-p " /libraries/ann/boxes.excalidrawlib " request)
         (list "200 OK" "Content-Type: application/json\r\n" test-mevedel-shared-library--box))
        (t '("404 Not Found" "" "missing"))))
     (lambda (base)
       (let ((mevedel-shared-library-catalog-url (concat base "/")))
         (should (equal [(:name "Boxes" :description "Shapes" :authors "Ann, Bo"
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
         (should (= 3 (length requests))))))))

(provide 'test-mevedel-shared-library)
;;; test-mevedel-shared-library.el ends here
