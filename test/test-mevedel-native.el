;;; test-mevedel-native.el --- Native module construction tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-native)

(defun mevedel-native-test--count (regexp string)
  "Count REGEXP in STRING."
  (with-temp-buffer (insert string) (how-many regexp (point-min) (point-max))))

(defmacro mevedel-native-test--with-tree (sources &rest body)
  "Run BODY in a package tree whose native/ holds SOURCES, (FILE . TEXT) pairs.
ROOT is both the package and the user directory."
  (declare (indent 1) (debug t))
  `(let* ((root (make-temp-file "mevedel-native-" t))
          (mevedel-user-dir root)
          (mevedel-native--directory root)
          (mevedel-native--load-state nil))
     (unwind-protect
         (progn
           (make-directory (file-name-concat root "native"))
           (pcase-dolist (`(,file . ,text) ,sources)
             (with-temp-file (file-name-concat root "native" file) (insert text)))
           ,@body)
       (delete-directory root t))))

(defmacro mevedel-native-test--with-prebuilt (module &rest body)
  "Run BODY with a source tree whose pins match MODULE's bytes.
ROOT is the package and user directory; PATH is where the prebuilt goes."
  (declare (indent 1) (debug t))
  `(mevedel-native-test--with-tree '(("a.c" . "source"))
     (with-temp-file (file-name-concat root "native/prebuilt.eld")
       (insert (format ";; pins\n((%S (%S . %S)))\n"
                       (mevedel-native--source-hash)
                       (mevedel-native--arch)
                       (secure-hash 'sha256 ,module))))
     (let ((path (mevedel-native--prebuilt-path)))
       ,@body)))

(mevedel-deftest mevedel-native--source-hash
  (:doc "Covers every C source, in the release workflow's order.")
  (mevedel-native-test--with-tree '(("b.c" . "two") ("a.c" . "one") ("prebuilt.eld" . "x"))
    ;; `cat native/*.c | sha256sum' under LC_ALL=C.
    (should (equal (mevedel-native--source-hash)
                   (substring (secure-hash 'sha256 "onetwo") 0 16)))))

(mevedel-deftest mevedel-native-load
  (:doc "Builds every C source; failures leave no artifact and are not retried.")
  (mevedel-native-test--with-tree '(("a.c" . "one") ("b.c" . "two") ("prebuilt.eld" . "()"))
    (let ((calls 0) compiled)
      (cl-letf (((symbol-function 'executable-find) (lambda (_) "/fake/compiler"))
                ((symbol-function 'call-process)
                 (lambda (program &rest args)
                   (cl-incf calls)
                   (when (equal program "cc") (setq compiled args))
                   (if (equal program "pkg-config") 0 1))))
        (let (reported)
          (mevedel-test--with-captured-diagnostics reported
            (should-not (mevedel-native-load))
            (should (string-match-p "compilation failed" mevedel-native--load-state))
            (should-not (mevedel-native-load)))
          ;; Reported once, with the next step for this system.
          (should (= 1 (mevedel-native-test--count "native module unavailable" reported)))
          (should (string-search "GTK 3/Wayland development files" reported)))
        (should (= calls 2))
        (should (member (file-name-concat root "native/a.c") compiled))
        (should (member (file-name-concat root "native/b.c") compiled))
        (should-not (member (file-name-concat root "native/prebuilt.eld") compiled))
        (should-not (directory-files (file-name-concat root "native") nil "^build-"))))))

(mevedel-deftest mevedel-native--verified-prebuilt
  (:doc "Loads a downloaded module only when it matches its pinned checksum.")
  (mevedel-native-test--with-prebuilt "module bytes"
    (should (mevedel-native--pin))
    (should-not (mevedel-native--verified-prebuilt))
    (make-directory (file-name-directory path) t)
    (let ((coding-system-for-write 'no-conversion))
      (write-region "tampered" nil path nil 'silent))
    (should-not (mevedel-native--verified-prebuilt))
    (let ((coding-system-for-write 'no-conversion))
      (write-region "module bytes" nil path nil 'silent))
    (should (equal path (mevedel-native--verified-prebuilt)))))

(mevedel-deftest mevedel-native-install
  (:doc "Writes and loads a download only after it matches its pin, once.")
  (progn
    (mevedel-native-test--with-prebuilt "module bytes"
      (let (loaded urls)
        (cl-letf (((symbol-function 'mevedel-native--download)
                   (lambda (url) (push url urls) "tampered"))
                  ((symbol-function 'module-load) (lambda (file) (push file loaded))))
          (mevedel-test--with-captured-diagnostics nil
            (should-error (mevedel-native-install) :type 'user-error))
          (should-not (file-exists-p path))
          (should (string-match-p "/native-[0-9a-f]\\{16\\}/mevedel-native-" (car urls))))
        (cl-letf (((symbol-function 'mevedel-native--download)
                   (lambda (_url) "module bytes"))
                  ((symbol-function 'module-load) (lambda (file) (push file loaded))))
          (let (messages)
            (mevedel-test--with-captured-diagnostics messages
              (mevedel-native-install))
            (should (string-search "installed" messages)))
          (should (equal (list path) loaded))
          (should (eq 'ready mevedel-native--load-state))
          ;; A loaded module is never joined by a second one.
          (let (messages)
            (mevedel-test--with-captured-diagnostics messages
              (mevedel-native-install))
            (should (string-search "next Emacs session" messages)))
          (should (equal (list path) loaded)))))
    (mevedel-native-test--with-tree '(("a.c" . "unpinned"))
      (should-error (mevedel-native-install) :type 'user-error))))

(mevedel-deftest mevedel-native--include-flags
  (:doc "Finds emacs-module.h in the running Emacs's prefix or build tree.")
  (let* ((root (make-temp-file "mevedel-native-include-" t))
         (bin (file-name-as-directory (file-name-concat root "bin")))
         (include (file-name-concat root "include")))
    (unwind-protect
        (progn
          (make-directory bin)
          (make-directory include)
          (let ((invocation-directory bin))
            (should-not (mevedel-native--include-flags))
            (with-temp-file (file-name-concat include "emacs-module.h"))
            (should (equal (mevedel-native--include-flags)
                           (list (concat "-I" (expand-file-name "../include" bin)))))
            (with-temp-file (file-name-concat bin "emacs-module.h"))
            (should (= 2 (length (mevedel-native--include-flags))))))
      (delete-directory root t))))

(provide 'test-mevedel-native)

;;; test-mevedel-native.el ends here
