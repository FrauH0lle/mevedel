;;; test-mevedel-collaboration-background-source.el --- Background source tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only source location uses the same native audit lookup as native UI.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'gptel)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-background)
(require 'mevedel-session-artifacts)
(require 'mevedel-view-audit)
(require 'mevedel-agents)

(defun mevedel-background-source-test--write (path)
  "Write a canonical nested execution source archive to PATH."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
    (insert (propertize "(:name \"ToolCall\")\noriginal result"
                        'gptel '(tool . "outer")))
    (insert (mevedel-tool-render-data-format
             '(:kind ptc :calls
               ((:id "outer/1" :tool "Bash" :status success
                 :render-data (:execution-id "exec-old" :state completed
                               :execution-output "retained output"))))
             "outer"))
    (mevedel-session-artifacts-stabilize-gptel-bounds)
    (write-region (point-min) (point-max) path nil 'silent)))

(mevedel-deftest mevedel-background-source-native-lookup
  (:doc "Pure native source helpers find nested root and child archive rows without selecting a view")
  (dolist (agent '(nil t))
    (let* ((root (make-temp-file "mevedel-background-source-" t))
           (data (generate-new-buffer " *background source live*"))
           (session (mevedel-session--create :save-path root :authority-mode 'pid-lock
                                            :current-segment 3))
           (agents (file-name-concat root "agents"))
           (archive (if agent (file-name-concat agents "worker.compact-0001.chat.org")
                      (mevedel-session-artifacts-segment-path root 1))))
      (unwind-protect
          (progn
            (make-directory agents t)
            (with-current-buffer data
              (setq buffer-file-name (if agent (file-name-concat agents "worker.chat.org")
                                       (mevedel-session-artifacts-segment-path root 3)))
              (setq-local mevedel--session session)
              (insert "current transcript\n")
              (goto-char 4))
            (mevedel-background-source-test--write archive)
            (let ((buffers (buffer-list))
                  (window (selected-window)))
              (let* ((record (mevedel-view-audit-execution-record session data "exec-old" agent))
                     (source (mevedel-view-audit-locate-source
                              session data (plist-get record :tool-use-id) agent)))
                (should (equal "outer/1" (plist-get record :tool-use-id)))
                (should (= 1 (plist-get source :number)))
                (should-not (plist-get source :current-p)))
              (should-not (mevedel-view-audit-locate-source session data "foreign" agent))
              (should-not (mevedel-view-audit-execution-record session data "foreign" agent))
              (should (eq window (selected-window)))
              (should (= 4 (with-current-buffer data (point))))
              (should-not (cl-set-difference (buffer-list) buffers))))
        (kill-buffer data)
        (delete-directory root t)))))

(mevedel-deftest mevedel-background-source-dispatch
  (:doc "Every authenticated tier locates an exact native source; foreign identities are unavailable")
  (let* ((root (make-temp-file "mevedel-background-locate-" t))
         (data (generate-new-buffer " *background locate live*"))
         (session (mevedel-session--create :save-path root :authority-mode 'pid-lock
                                          :current-segment 3))
         (room (list :session session :data-buffer data :transport 'transport))
         (guest (list :writable nil))
         sent)
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq-local mevedel--session session)
            (setq buffer-file-name (mevedel-session-artifacts-segment-path root 3)))
          (mevedel-background-source-test--write (mevedel-session-artifacts-segment-path root 1))
          (cl-letf (((symbol-function 'mevedel-collaboration--guest) (lambda (&rest _) guest))
                    ((symbol-function 'mevedel-collaboration--room-for-session) (lambda (_) room))
                    ((symbol-function 'mevedel-transport-run-at-time)
                     (lambda (_ fn &rest args) (apply fn args)))
                    ((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport _peer frame) (push frame sent))))
            (dolist (tier '(view full owner))
              (setq guest (list :writable (memq tier '(full owner)) :owner (eq tier 'owner)))
              (mevedel-collaboration--dispatch-frame
               room 1 '(:t "background-locate" :reqId 10 :owner "/root" :id "exec-old"))
              (let ((frame (car sent)))
                (should (equal "background-source" (plist-get frame :t)))
                (should (equal "/root" (plist-get frame :owner)))
                (should (equal "outer/1" (plist-get frame :toolUseId)))
                (should (= 1 (plist-get frame :segment)))
                (should (eq :json-false (plist-get frame :current)))
                (should-not (plist-get frame :unavailable))))
            (dolist (request '((:owner "/root/foreign" :id "exec-old")
                               (:owner "/root" :id "foreign")
                               (:path "/root")))
              (setq guest (list :writable t :owner t))
              (mevedel-collaboration--dispatch-frame
               room 1 (append '(:t "background-locate" :reqId 11) request))
              (should (equal "Original source row is unavailable."
                             (plist-get (car sent) :unavailable))))
            (setq guest nil sent nil)
            (mevedel-collaboration--dispatch-frame
             room 1 '(:t "background-locate" :reqId 12 :owner "/root" :id "exec-old"))
            (should-not sent)))
      (kill-buffer data)
      (delete-directory root t))))

(mevedel-deftest mevedel-background-source-agent-and-child
  (:doc "Child executions use child archives; an agent launch resolves its parent current transcript")
  (let* ((root (make-temp-file "mevedel-background-child-source-" t))
         (data (generate-new-buffer " *background parent source*"))
         (child (generate-new-buffer " *background child source*"))
         (session (mevedel-session--create :save-path root :authority-mode 'pid-lock :current-segment 4))
         (invocation (mevedel-agent-invocation--create :parent-tool-use-id "launch/1"))
         (record (mevedel-agent-record--create :path "/root/child" :parent-path "/root"
                                              :invocation invocation :conversation-buffer child))
         (room (list :session session :data-buffer data))
         (agents (file-name-concat root "agents")))
    (unwind-protect
        (progn
          (setf (mevedel-session-agent-registry session) (list (cons "/root/child" record)))
          (make-directory agents t)
          (with-current-buffer data
            (setq-local mevedel--session session)
            (setq buffer-file-name (mevedel-session-artifacts-segment-path root 4))
            (insert (propertize "Agent source" 'gptel '(tool . "launch"))))
          (with-current-buffer child
            (setq-local mevedel--session session)
            (setq buffer-file-name (file-name-concat agents "worker.chat.org")))
          (mevedel-background-source-test--write
           (file-name-concat agents "worker.compact-0001.chat.org"))
          (let ((reply (mevedel-collaboration--background-source
                        room '(:owner "/root/child" :id "exec-old"))))
            (should (equal "/root/child" (plist-get reply :owner)))
            (should (equal "outer/1" (plist-get reply :toolUseId)))
            (should (= 1 (plist-get reply :segment)))
            (should (eq :json-false (plist-get reply :current))))
          (let ((reply (mevedel-collaboration--background-source room '(:path "/root/child"))))
            (should (equal "/root" (plist-get reply :owner)))
            (should (equal "launch/1" (plist-get reply :toolUseId)))
            (should (eq t (plist-get reply :current)))
            (should-not (plist-member reply :segment)))
          ;; Being retained authorizes read-only hydration, never buffer selection.
          (setf (mevedel-agent-record-conversation-buffer record) nil)
          (cl-letf (((symbol-function 'mevedel-agent-persistence-ensure-conversation)
                     (lambda (s r parent readonly)
                       (should (eq session s)) (should (eq data parent)) (should readonly)
                       (setf (mevedel-agent-record-conversation-buffer r) child))))
            (should (eq child (mevedel-collaboration--background-source-buffer room "/root/child"))))
          (should-error (mevedel-collaboration--background-source
                         room '(:path "/root/child" :owner "/root" :id "ambiguous"))))
      (kill-buffer child)
      (kill-buffer data)
      (delete-directory root t))))

(mevedel-deftest mevedel-background-source-departed-guest
  (:doc "Deferred source I/O rechecks guest admission and does not run after departure")
  (let* ((session (mevedel-session--create :name "deferred"))
         (room (list :session session))
         (guest (list :writable nil))
         pending called)
    (cl-letf (((symbol-function 'mevedel-collaboration--guest) (lambda (&rest _) guest))
              ((symbol-function 'mevedel-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'mevedel-transport-run-at-time)
               (lambda (_ fn &rest _) (setq pending fn)))
              ((symbol-function 'mevedel-collaboration--background-source)
               (lambda (&rest _) (setq called t))))
      (mevedel-collaboration--handle-background-locate
       room 1 '(:reqId 1 :owner "/root" :id "one"))
      (should pending)
      (setq guest nil)
      (funcall pending)
      (should-not called))))

(provide 'test-mevedel-collaboration-background-source)
;;; test-mevedel-collaboration-background-source.el ends here
