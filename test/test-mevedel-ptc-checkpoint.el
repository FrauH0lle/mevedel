;;; test-mevedel-ptc-checkpoint.el -- Durable ToolScript checkpoint tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the compact ToolScript checkpoint lifecycle and transcript recovery.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-ptc-checkpoint)
(require 'mevedel-structs)
(require 'mevedel-tool-render-data)

(defun test-mevedel-ptc-checkpoint--session (&optional checkpoints)
  "Return a passive session containing CHECKPOINTS."
  (mevedel-session--create :name "test" :ptc-checkpoints checkpoints))


;;
;;; Checkpoint lifecycle

(mevedel-deftest mevedel-ptc-checkpoint--put ()
  ,test
  (test)
  :doc "replaces the checkpoint with the same identity"
  (let ((session
         (test-mevedel-ptc-checkpoint--session
          '((:id "p/1" :state running :value old)))))
    (mevedel-ptc-checkpoint--put
     session '(:id "p/1" :state settled :value new))
    (should (equal '((:id "p/1" :state settled :value new))
                   (mevedel-session-ptc-checkpoints session)))))

(mevedel-deftest mevedel-ptc-checkpoint-start ()
  ,test
  (test)
  :doc "does nothing without a durable session"
  (should-not (mevedel-ptc-checkpoint-start nil nil "p" "(+ 1 2)"))
  :doc "rolls back when no authoritative buffer can publish the checkpoint"
  (let ((session (test-mevedel-ptc-checkpoint--session)))
    (should-not
     (mevedel-ptc-checkpoint-start session nil "p" "(+ 1 2)"))
    (should-not (mevedel-session-ptc-checkpoints session))))

(mevedel-deftest mevedel-ptc-checkpoint-update ()
  ,test
  (test)
  :doc "treats an absent session as already durable"
  (should (mevedel-ptc-checkpoint-update nil nil "p" '(:state settled)))
  :doc "rolls memory back when the sidecar rewrite fails"
  (let* ((before '((:id "p" :state running)))
         (session (test-mevedel-ptc-checkpoint--session before)))
    (cl-letf (((symbol-function
                'mevedel-session-persistence-write-sidecar-now)
               (lambda (&rest _) nil)))
      (should-not
       (mevedel-ptc-checkpoint-update
        session nil "p" '(:state settled)))
      (should (equal before
                     (mevedel-session-ptc-checkpoints session))))))

(mevedel-deftest mevedel-ptc-checkpoint-clear-settled ()
  ,test
  (test)
  :doc "keeps running checkpoints and removes settled ones"
  (let ((session
         (test-mevedel-ptc-checkpoint--session
          '((:id "running" :state running)
            (:id "settled" :state settled)))))
    (mevedel-ptc-checkpoint-clear-settled session)
    (should (equal '((:id "running" :state running))
                   (mevedel-session-ptc-checkpoints session)))))


;;
;;; Restore

(mevedel-deftest mevedel-ptc-checkpoint--interrupted-render-data ()
  ,test
  (test)
  :doc "marks unfinished children and the envelope interrupted"
  (let* ((checkpoint
          '(:render-data
            (:kind ptc :outcome running
             :calls ((:id "p/1" :status running)
                     (:id "p/2" :status success)))))
         (data (mevedel-ptc-checkpoint--interrupted-render-data checkpoint)))
    (should (eq 'interrupted (plist-get data :outcome)))
    (should (= 2 (plist-get data :nested-call-count)))
    (should (equal '(interrupted success)
                   (mapcar (lambda (call) (plist-get call :status))
                           (plist-get data :calls))))))

(mevedel-deftest mevedel-ptc-checkpoint--insert ()
  ,test
  (test)
  :doc "appends a parseable interrupted ToolScript tool block"
  (with-temp-buffer
    (mevedel-ptc-checkpoint--insert
     '(:id "p" :args (:script "(+ 1 2)") :state running
       :render-data (:kind ptc :outcome running :calls nil)))
    (should (string-match-p "^#\\+begin_tool (ToolScript recovered"
                            (buffer-string)))
    (should (string-match-p "script interrupted by Emacs restart"
                            (buffer-string)))
    (should (mevedel-tool-render-data-segment-bounds "p"))
    (should (plist-get
             (cdr (mevedel-tool-render-data-extract (buffer-string) nil "p" t))
             :kind)))
  :doc "escapes settled results that contain Org block terminators"
  (with-temp-buffer
    (mevedel-ptc-checkpoint--insert
     '(:id "p" :args (:script "1") :state settled
       :result "before\n#+end_tool\nafter"
       :render-data (:kind ptc :outcome completed :calls nil)))
    (goto-char (point-min))
    (should (= 1 (how-many "^#\\+end_tool$")))
    (should (search-forward ",#+end_tool" nil t))
    (should (mevedel-tool-render-data-segment-bounds "p"))))

(mevedel-deftest mevedel-ptc-checkpoint-reconcile ()
  ,test
  (test)
  :doc "inserts only missing rows and consumes every checkpoint"
  (let ((session
         (test-mevedel-ptc-checkpoint--session
          '((:id "present" :args (:script "1") :state settled
             :result "1" :render-data (:kind ptc :outcome completed :calls nil))
            (:id "missing" :args (:script "2") :state running
             :render-data (:kind ptc :outcome running :calls nil))))))
    (with-temp-buffer
      (insert (propertize "existing" 'gptel '(tool . "present")))
      (should (equal '(1 . 2)
                     (mevedel-ptc-checkpoint-reconcile session)))
      (should (mevedel-tool-render-data-segment-bounds "missing"))
      (should-not (mevedel-session-ptc-checkpoints session))))
  :doc "consumes a checkpoint whose row is already present"
  (let ((session
         (test-mevedel-ptc-checkpoint--session
          '((:id "present" :args (:script "1") :state settled
             :result "1" :render-data (:kind ptc :outcome completed))))))
    (with-temp-buffer
      (insert (propertize "existing" 'gptel '(tool . "present")))
      (should (equal '(0 . 1)
                     (mevedel-ptc-checkpoint-reconcile session)))
      (should-not (mevedel-session-ptc-checkpoints session)))))

(provide 'test-mevedel-ptc-checkpoint)

;;; test-mevedel-ptc-checkpoint.el ends here
