;;; test-mevedel-session-persistence-fork.el -- Fork transcript copy tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the spec-23 amendment to spec 19's fork-now: agent
;; transcript files are copied for any agent whose :parent-turn falls
;; within the copied segments' turn range.

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
  (:doc "filters agent-transcripts entries by :parent-turn upper bound")
  ,test
  (test)

  :doc "returns entries with :parent-turn <= max-cum-turn"
  (let ((entries
         '(("a--1" :parent-turn 1 :path "agents/a--1.chat.org")
           ("b--2" :parent-turn 2 :path "agents/b--2.chat.org")
           ("c--3" :parent-turn 3 :path "agents/c--3.chat.org")
           ("d--5" :parent-turn 5 :path "agents/d--5.chat.org"))))
    ;; Cap at turn 3 — first three are in, fourth is out.
    (let ((result
           (mevedel-session-persistence--agent-files-for-segments
            entries 3)))
      (should (= 3 (length result)))
      (should (cl-every (lambda (e) (<= (plist-get (cdr e) :parent-turn) 3))
                        result))))

  :doc "excludes entries with non-integer :parent-turn"
  (let ((entries
         '(("good--1" :parent-turn 1 :path "agents/good.chat.org")
           ("bad--2"  :parent-turn nil :path "agents/bad.chat.org")
           ("ugly--3" :parent-turn "string" :path "agents/ugly.chat.org"))))
    (let ((result
           (mevedel-session-persistence--agent-files-for-segments
            entries 10)))
      (should (= 1 (length result)))
      (should (equal "good--1" (caar result)))))

  :doc "empty input returns empty"
  (should (null
           (mevedel-session-persistence--agent-files-for-segments nil 5)))

  :doc "max-cum-turn = 0 excludes everything (all parent-turns are positive)"
  (let ((entries
         '(("a--1" :parent-turn 1 :path "agents/a.chat.org"))))
    (should (null
             (mevedel-session-persistence--agent-files-for-segments
              entries 0)))))


(provide 'test-mevedel-session-persistence-fork)
;;; test-mevedel-session-persistence-fork.el ends here
