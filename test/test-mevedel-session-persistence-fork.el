;;; test-mevedel-session-persistence-fork.el -- Fork transcript copy tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for fork-now agent transcript copying: agent transcript files
;; are copied for any agent whose :parent-turn falls within the copied
;; segments' turn range.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-session-persistence)


;;
;;; Pure helper: --agent-files-for-segments

(mevedel-deftest mevedel-session-persistence--agent-files-for-segments
  (:doc "filters agent-transcripts entries by copied segment turn ranges")
  ,test
  (test)

  :doc "returns entries whose :parent-turn falls in copied segment ranges"
  (let ((index
         '((1 . ((:cum-turn 1) (:cum-turn 2)))
           (2 . ((:cum-turn 10) (:cum-turn 11)))))
        (entries
         '(("a--1" :parent-turn 1 :path "agents/a--1.chat.org")
           ("b--2" :parent-turn 2 :path "agents/b--2.chat.org")
           ("gap--5" :parent-turn 5 :path "agents/gap.chat.org")
           ("c--10" :parent-turn 10 :path "agents/c--10.chat.org")
           ("d--12" :parent-turn 12 :path "agents/d--12.chat.org"))))
    (let ((result
           (mevedel-session-persistence--agent-files-for-segments
            index entries 2 10)))
      (should (= 3 (length result)))
      (should (assoc "a--1" result))
      (should (assoc "b--2" result))
      (should (assoc "c--10" result))
      (should-not (assoc "gap--5" result))
      (should-not (assoc "d--12" result))))

  :doc "excludes entries with non-integer :parent-turn"
  (let ((index '((1 . ((:cum-turn 1) (:cum-turn 2)))))
        (entries
         '(("good--1" :parent-turn 1 :path "agents/good.chat.org")
           ("bad--2"  :parent-turn nil :path "agents/bad.chat.org")
           ("ugly--3" :parent-turn "string" :path "agents/ugly.chat.org"))))
    (let ((result
           (mevedel-session-persistence--agent-files-for-segments
            index entries 1 2)))
      (should (= 1 (length result)))
      (should (equal "good--1" (caar result)))))

  :doc "empty input returns empty"
  (should (null
           (mevedel-session-persistence--agent-files-for-segments
            nil nil 1 1)))

  :doc "picked-cum-turn before all prompts excludes everything"
  (let ((index '((1 . ((:cum-turn 1)))))
        (entries
         '(("a--1" :parent-turn 1 :path "agents/a.chat.org"))))
    (should (null
             (mevedel-session-persistence--agent-files-for-segments
              index entries 1 0)))))


(provide 'test-mevedel-session-persistence-fork)
;;; test-mevedel-session-persistence-fork.el ends here
