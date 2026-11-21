;;; test-niwa-change.el --- Checked change engine regressions -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise public preparation over literal snapshots of real files.

;;; Code:

(require 'niwa-change)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-change-prepare
  ()
  (let* ((directory (make-temp-file "change-prepare-" t))
         (path (file-name-concat directory "source")))
    (unwind-protect
        (cl-labels ((snapshot (text)
                      (write-region text nil path nil 'silent)
                      (list :path path :exists t
                            :bytes (with-temp-buffer
                                     (insert-file-contents-literally path)
                                     (buffer-string)))))
          ,test)
      (delete-directory directory t)))
  (test)
  :doc "identical replacements already applied retain disjoint edits"
  (let* ((before (snapshot "one\ntwo\nthree\nfour\n"))
         (after (snapshot "ONE\ntwo\nthree\nfour\n"))
         (current (snapshot "one\ntwo\nthree\nuser\n"))
         (prepared (niwa-change-prepare before after current t)))
    (should-not (plist-get prepared :conflicts))
    (should (equal "one\ntwo\nthree\nuser\n" (plist-get (plist-get prepared :after) :bytes))))
  :doc "identical insertions already applied are not inserted twice"
  (let* ((before (snapshot "one\ntwo\nthree\nfour\n"))
         (after (snapshot "one\ninserted\ntwo\nthree\nfour\n"))
         (current (snapshot "one\ninserted\ntwo\nthree\nuser\n"))
         (prepared (niwa-change-prepare before after current)))
    (should-not (plist-get prepared :conflicts))
    (should (equal "one\ninserted\ntwo\nthree\nuser\n" (plist-get (plist-get prepared :after) :bytes))))
  :doc "different overlapping replacements still refuse the whole change"
  (let* ((before (snapshot "one\ntwo\nthree\nfour\n"))
         (after (snapshot "ONE\ntwo\nthree\nfour\n"))
         (current (snapshot "mine\ntwo\nthree\nuser\n"))
         (prepared (niwa-change-prepare before after current)))
    (should (plist-get prepared :conflicts))
    (should-not (plist-get prepared :after))))

(provide 'test-niwa-change)
;;; test-niwa-change.el ends here
