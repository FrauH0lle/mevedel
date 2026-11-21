;;; test-niwa-collaboration-background-source.el --- Background source tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only source location uses the same native audit lookup as native UI.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "niwa-session-test-support"))
(require 'gptel)
(require 'niwa-collaboration)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-background)
(require 'niwa-session-artifacts)
(require 'niwa-view-audit)
(require 'niwa-agents)

(defun niwa-background-source-test--write (session path)
  "Write and publish SESSION's canonical nested source archive at PATH."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n")
    (insert (propertize "(:name \"ToolCall\")\noriginal result"
                        'gptel '(tool . "outer")))
    (insert (niwa-tool-render-data-format
             '(:kind ptc :calls
               ((:id "outer/1" :tool "Bash" :status success
                 :render-data (:execution-id "exec-old" :state completed
                               :execution-output "retained output"))))
             "outer"))
    (niwa-session-artifacts-stabilize-gptel-bounds)
    (write-region (point-min) (point-max) path nil 'silent)
    (niwa-session-publication-publish
     session (list (list :path path :content (buffer-string))
                   '(:path "session.meta.el" :content "(:fixture archive-only)"
                     :commit-marker t)) t)))

(niwa-deftest niwa-background-source-native-lookup
  (:doc "Pure native source helpers find nested root and child archive rows without selecting a view")
  (dolist (agent '(nil t))
    (let* ((root (make-temp-file "niwa-background-source-" t))
           (data (generate-new-buffer " *background source live*"))
           (session (test-niwa-session-persistence--leased-session root
                                            :current-segment 3))
           (agents (file-name-concat root "agents"))
           (archive (if agent (file-name-concat agents "worker.compact-0001.chat.org")
                      (niwa-session-artifacts-segment-path root 1))))
      (unwind-protect
          (progn
            (make-directory agents t)
            (with-current-buffer data
              (setq buffer-file-name (if agent (file-name-concat agents "worker.chat.org")
                                       (niwa-session-artifacts-segment-path root 3)))
              (setq-local niwa--session session)
              (insert "current transcript\n")
              (goto-char 4))
            (niwa-background-source-test--write session archive)
            (let ((buffers (buffer-list))
                  (window (selected-window)))
              (let* ((record (niwa-view-audit-execution-record session data "exec-old" agent))
                     (source (niwa-view-audit-locate-source
                              session data (plist-get record :tool-use-id) agent)))
                (should (equal "outer/1" (plist-get record :tool-use-id)))
                (should (= 1 (plist-get source :number)))
                (should-not (plist-get source :current-p)))
              (should-not (niwa-view-audit-locate-source session data "foreign" agent))
              (should-not (niwa-view-audit-execution-record session data "foreign" agent))
              (should (eq window (selected-window)))
              (should (= 4 (with-current-buffer data (point))))
              (should-not (cl-set-difference (buffer-list) buffers))))
        (kill-buffer data)
        (niwa-session-durability-lease-release root session)
        (niwa-workspace-clear-registry)
        (delete-directory root t)))))

(niwa-deftest niwa-background-source-dispatch
  (:doc "Every authenticated tier locates an exact native source; foreign identities are unavailable")
  (let* ((root (make-temp-file "niwa-background-locate-" t))
         (data (generate-new-buffer " *background locate live*"))
         (session (test-niwa-session-persistence--leased-session root
                                          :current-segment 3))
         (room (list :session session :data-buffer data :transport 'transport))
         (guest (list :writable nil))
         sent)
    (unwind-protect
        (progn
          (with-current-buffer data
            (setq-local niwa--session session)
            (setq buffer-file-name (niwa-session-artifacts-segment-path root 3)))
          (niwa-background-source-test--write session (niwa-session-artifacts-segment-path root 1))
          (cl-letf (((symbol-function 'niwa-collaboration--guest) (lambda (&rest _) guest))
                    ((symbol-function 'niwa-collaboration--room-for-session) (lambda (_) room))
                    ((symbol-function 'niwa-transport-run-at-time)
                     (lambda (_ fn &rest args) (apply fn args)))
                    ((symbol-function 'niwa-collaboration--transport-send)
                     (lambda (_transport _peer frame) (push frame sent))))
            (dolist (tier '(view full owner))
              (setq guest (list :writable (memq tier '(full owner)) :owner (eq tier 'owner)))
              (niwa-collaboration--dispatch-frame
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
              (niwa-collaboration--dispatch-frame
               room 1 (append '(:t "background-locate" :reqId 11) request))
              (should (equal "Original source row is unavailable."
                             (plist-get (car sent) :unavailable))))
            (setq guest nil sent nil)
            (niwa-collaboration--dispatch-frame
             room 1 '(:t "background-locate" :reqId 12 :owner "/root" :id "exec-old"))
            (should-not sent)))
      (kill-buffer data)
      (niwa-session-durability-lease-release root session)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-background-source-agent-and-child
  (:doc "Child executions use child archives; an agent launch resolves its parent current transcript")
  (let* ((root (make-temp-file "niwa-background-child-source-" t))
         (data (generate-new-buffer " *background parent source*"))
         (child (generate-new-buffer " *background child source*"))
         (session (test-niwa-session-persistence--leased-session root :current-segment 4))
         (invocation (niwa-agent-invocation--create :parent-tool-use-id "launch/1"))
         (record (niwa-agent-record--create :path "/root/child" :parent-path "/root"
                                              :invocation invocation :conversation-buffer child))
         (room (list :session session :data-buffer data))
         (agents (file-name-concat root "agents")))
    (unwind-protect
        (progn
          (setf (niwa-session-agent-registry session) (list (cons "/root/child" record)))
          (make-directory agents t)
          (with-current-buffer data
            (setq-local niwa--session session)
            (setq buffer-file-name (niwa-session-artifacts-segment-path root 4))
            (insert (propertize "Agent source" 'gptel '(tool . "launch"))))
          (with-current-buffer child
            (setq-local niwa--session session)
            (setq buffer-file-name (file-name-concat agents "worker.chat.org")))
          (niwa-background-source-test--write
           session (file-name-concat agents "worker.compact-0001.chat.org"))
          (let ((reply (niwa-collaboration--background-source
                        room '(:owner "/root/child" :id "exec-old"))))
            (should (equal "/root/child" (plist-get reply :owner)))
            (should (equal "outer/1" (plist-get reply :toolUseId)))
            (should (= 1 (plist-get reply :segment)))
            (should (eq :json-false (plist-get reply :current))))
          (let ((reply (niwa-collaboration--background-source room '(:path "/root/child"))))
            (should (equal "/root" (plist-get reply :owner)))
            (should (equal "launch/1" (plist-get reply :toolUseId)))
            (should (eq t (plist-get reply :current)))
            (should-not (plist-member reply :segment)))
          ;; Being retained authorizes read-only hydration, never buffer selection.
          (setf (niwa-agent-record-conversation-buffer record) nil)
          (cl-letf (((symbol-function 'niwa-agent-persistence-ensure-conversation)
                     (lambda (s r parent readonly)
                       (should (eq session s)) (should (eq data parent)) (should readonly)
                       (setf (niwa-agent-record-conversation-buffer r) child))))
            (should (eq child (niwa-collaboration--background-source-buffer room "/root/child"))))
          (should-error (niwa-collaboration--background-source
                         room '(:path "/root/child" :owner "/root" :id "ambiguous"))))
      (kill-buffer child)
      (kill-buffer data)
      (niwa-session-durability-lease-release root session)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-background-source-departed-guest
  (:doc "Deferred source I/O rechecks guest admission and does not run after departure")
  (let* ((session (niwa-session--create :name "deferred"))
         (room (list :session session))
         (guest (list :writable nil))
         pending called)
    (cl-letf (((symbol-function 'niwa-collaboration--guest) (lambda (&rest _) guest))
              ((symbol-function 'niwa-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'niwa-transport-run-at-time)
               (lambda (_ fn &rest _) (setq pending fn)))
              ((symbol-function 'niwa-collaboration--background-source)
               (lambda (&rest _) (setq called t))))
      (niwa-collaboration--handle-background-locate
       room 1 '(:reqId 1 :owner "/root" :id "one"))
      (should pending)
      (setq guest nil)
      (funcall pending)
      (should-not called))))

(provide 'test-niwa-collaboration-background-source)
;;; test-niwa-collaboration-background-source.el ends here
