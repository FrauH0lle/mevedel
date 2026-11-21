;;; test-niwa-collaboration-agent-archive.el --- Child archive fetch tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise child archive fetches through authenticated guest frames and real
;; canonical transcript descriptors, without borrowing parent history.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'niwa-agent-control)
(require 'niwa-collaboration)
(require 'niwa-collaboration-agent)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-artifact)
(require 'niwa-collaboration-projection)
(require 'niwa-collaboration-transport)
(require 'niwa-session-artifacts)
(require 'niwa-tool-render-data)
(require 'niwa-workspace)

(defmacro niwa-agent-archive-test--with-room (&rest body)
  "Run BODY with an isolated retained child and real archive files."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "niwa-child-archive-" t))
          (agents (file-name-concat directory "agents"))
          (archive (file-name-concat agents "child.compact-0001.chat.org"))
          (data (generate-new-buffer " *child archive live*"))
          (child (niwa-agent-record--create
                  :path "/root/child" :conversation-buffer data))
          (session (niwa-session--create
                    :name "archive" :save-path directory :authority-mode 'portable
                    :current-segment 3
                    :workspace (niwa-workspace--create
                                :type 'file :id "archive" :root directory)
                    :agent-registry (list (cons "/root/child" child))))
          (guests (make-hash-table :test #'eql))
          (room (list :session session :guests guests :transport 'test))
          sent)
     (unwind-protect
         (progn
           (make-directory agents t)
           (write-region "PARENT ARCHIVE MUST NOT LEAK" nil
                         (niwa-session-artifacts-segment-path directory 1)
                         nil 'silent)
           (with-current-buffer data
             (delay-mode-hooks (org-mode))
             (setq buffer-file-name (file-name-concat agents "child.chat.org"))
             (setq-local niwa--session session)
             (insert "CURRENT CHILD ONLY\n"))
           (with-temp-buffer
             (delay-mode-hooks (org-mode))
             (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\nARCHIVED CHILD ONLY\n")
             (insert (propertize
                      (concat "#+begin_tool (ApplyPatch :patch \"patch\")\n"
                              "(:name \"ApplyPatch\" :args (:patch \"patch\"))\n\nApplied patch\n"
                              (niwa-tool-render-data-format
                               '(:kind patch :files
                                 ((:kind add :path "design.html" :added 1 :deleted 0 :diff "")))
                               "old-patch")
                              "#+end_tool\n")
                      'gptel '(tool . "old-patch")))
             (niwa-session-artifacts-stabilize-gptel-bounds)
             (write-region (point-min) (point-max) archive nil 'silent))
           (puthash 1 (list :ready t :writable nil) guests)
           ;; The archive is a raw file here, not a published generation.
           (cl-letf (((symbol-function 'niwa-session-artifacts-transcript-segments)
                      (lambda (_session live &rest _)
                        ;; Only a file-backed child with an archive on disk has one.
                        (when (and (buffer-local-value 'buffer-file-name live)
                                   (file-exists-p archive))
                          (list (list :number 1 :status 'readable :current-p nil
                                      :logical "agents/child.compact-0001.chat.org")
                                (list :number 2 :status 'readable :current-p t)))))
                     ((symbol-function 'niwa-session-artifacts-find-artifact-noselect)
                      (lambda (_session logical &rest _)
                        (let ((buffer (generate-new-buffer " *raw archive*")))
                          (with-current-buffer buffer
                            (insert-file-contents (file-name-concat directory logical))
                            (delay-mode-hooks (org-mode)))
                          buffer)))
                     ((symbol-function 'niwa-collaboration--transport-send)
                      (lambda (_transport _peer frame)
                        (push (niwa-test--frame frame) sent) t)))
             (cl-labels ((fetch (frame &optional peer)
                           (setq sent nil)
                           (plist-put (gethash 1 guests) :last-agent-fetch nil)
                           (plist-put (gethash 1 guests) :last-agent-archive-fetch nil)
                           (niwa-collaboration--dispatch-frame
                            room (or peer 1)
                            (append frame '(:t "fetch-agent" :reqId 1 :path "/root/child")))
                           (car sent)))
               ,@body)))
       (when (buffer-live-p data) (kill-buffer data))
       (niwa-collaboration--artifact-stat-invalidate)
       (delete-directory directory t))))

(niwa-deftest niwa-collaboration-agent-archive-fetch
  (:doc "All tiers fetch only the child's numbered archive with stable scoped IDs and artifact authority")
  (niwa-agent-archive-test--with-room
    (let* ((artifact (file-name-concat directory ".niwa" "artifacts" "design.html"))
           (project (symbol-function 'niwa-collaboration--canonical-records))
           archived-buffer projected)
      (make-directory (file-name-directory artifact) t)
      (write-region "<h1>Child design</h1>" nil artifact nil 'silent)
      (cl-letf (((symbol-function 'niwa-collaboration--canonical-records)
                 (lambda (buffer &optional completion-buffer completions cache path)
                   (should (equal "/root/child" path))
                   (unless (eq buffer data)
                     (setq archived-buffer buffer)
                     (should (eq session (buffer-local-value 'niwa--session buffer))))
                   (setq projected (funcall project buffer completion-buffer completions cache path)))))
        (dolist (tier '(view full owner))
          (puthash 1 (list :ready t :writable (memq tier '(full owner))
                          :owner (eq tier 'owner)) guests)
          (let* ((frame (fetch '(:segment 1)))
                 (json (json-encode sent))
                 (records (append (plist-get frame :records) nil)))
            (should (= 1 (plist-get frame :segment)))
            (should (equal "/root/child" (plist-get frame :path)))
            (should (plist-get frame :final))
            (should-not (plist-get frame :error))
            (should (string-match-p "ARCHIVED CHILD ONLY" json))
            (should-not (string-match-p "CURRENT CHILD ONLY\\|PARENT ARCHIVE" json))
            (should-not (string-match-p "artifact-path\\|niwa-render-data" json))
            (should-not (buffer-live-p archived-buffer))
            (should records)
            (dolist (record records)
              (should (string-prefix-p "agent-history-1-" (plist-get record :id))))
            ;; The host keeps full artifact metadata; the wire never gets paths.
            (let ((metadata (car (plist-get (gethash 1 guests) :agent-artifacts))))
              (should (equal artifact (plist-get metadata :artifact-path)))
              (should (equal "design.html" (plist-get metadata :artifact)))
              (should (equal (plist-get metadata :artifact-path)
                             (plist-get (cl-find-if
                                         (lambda (record) (plist-get record :artifact))
                                         projected)
                                        :artifact-path)))
              (setq sent nil)
              (niwa-collaboration--handle-artifact-get
               room 1 (list :reqId 2 :id (plist-get metadata :id)))
              (should (equal "<h1>Child design</h1>"
                             (base64-decode-string (plist-get (car sent) :data)))))
            (let ((unchanged (fetch (list :segment 1 :known (plist-get frame :digest)))))
              (should (plist-get unchanged :unchanged))
              (should (= 1 (plist-get unchanged :segment)))
              (should-not (plist-member unchanged :records))))))
      ;; Omitting the optional selector still projects the current child.
      (let ((frame (fetch nil)))
        (should-not (plist-member frame :segment))
        (should (string-match-p "CURRENT CHILD ONLY" (json-encode sent)))
        (should-not (string-match-p "ARCHIVED CHILD ONLY" (json-encode sent)))))))

(niwa-deftest niwa-collaboration-agent-archive-refusal
  (:doc "Malformed, missing, fileless, foreign and unreadable child archives never fall back to root history")
  (niwa-agent-archive-test--with-room
    (dolist (number '(nil 0 -1 1.5 "1" "../segment-0001.chat.org" 2 3 99))
      (let ((frame (fetch (list :segment number))))
        (should (stringp (plist-get frame :error)))
        (should-not (plist-member frame :records))))
    (should-not (fetch '(:segment 1) 999))
    (let ((frame (fetch '(:segment 1 :path "/root/foreign"))))
      (should (plist-get frame :error)))
    ;; Fileless native children have current evidence only, not root archives.
    (with-current-buffer data (setq buffer-file-name nil))
    (should (plist-get (fetch '(:segment 1)) :error))
    (with-current-buffer data
      (setq buffer-file-name (file-name-concat agents "child.chat.org")))
    (delete-file archive)
    (should (plist-get (fetch '(:segment 1)) :error))))

(niwa-deftest niwa-collaboration-agent-archive-cleanup
  (:doc "Projection failure returns an explicit error and releases the canonical archive buffer")
  (niwa-agent-archive-test--with-room
    (let (inspection)
      (cl-letf (((symbol-function 'niwa-collaboration--canonical-records)
                 (lambda (buffer &rest _)
                   (setq inspection buffer)
                   (error "Injected projection failure"))))
        (should (plist-get (fetch '(:segment 1)) :error)))
      (should inspection)
      (should-not (eq data inspection))
      (should-not (buffer-live-p inspection))
      (should (buffer-live-p data)))))

(niwa-deftest niwa-collaboration-agent-archive-cold
  (:doc "One cold archive request completes after queued hydration without a second guest poll")
  (niwa-agent-archive-test--with-room
    (plist-put room :data-buffer data)
    (dolist (outcome '(success failure disconnected stopped))
      (setf (niwa-agent-record-conversation-buffer child) nil)
      (plist-put room :agent-loads nil)
      (puthash 1 (list :ready t :writable nil) guests)
      (let (timers loaded stopped)
        (cl-letf (((symbol-function 'niwa-transport-run-at-time)
                   (lambda (_seconds fn &rest args) (push (cons fn args) timers)))
                  ((symbol-function 'niwa-collaboration--room-for-buffer)
                   (lambda (_) room))
                  ((symbol-function 'niwa-collaboration--room-for-session)
                   (lambda (_) (unless stopped room)))
                  ((symbol-function 'niwa-agent-persistence-ensure-conversation)
                   (lambda (seen-session seen-child root &optional _readonly)
                     (should (eq session seen-session))
                     (should (eq child seen-child))
                     (should (eq data root))
                     (setq loaded t)
                     (when (eq outcome 'failure) (error "Injected hydration failure"))
                     (setf (niwa-agent-record-conversation-buffer child) data))))
          (fetch '(:segment 1 :reqId 42))
          (should-not sent)
          (should-not loaded)
          (should (= 1 (length timers)))
          (when (eq outcome 'disconnected) (remhash 1 guests))
          (setq stopped (eq outcome 'stopped))
          (apply (caar timers) (cdar timers))
          (should loaded)
          (pcase outcome
            ('success
             (should (= 42 (plist-get (car sent) :reqId)))
             (should (= 1 (plist-get (car sent) :segment)))
             (should (string-match-p "ARCHIVED CHILD ONLY" (json-encode sent))))
            ('failure (should (plist-get (car sent) :error)))
            (_ (should-not sent)))
          (should-not (plist-get room :agent-fetch-waiters)))))))

(niwa-deftest niwa-collaboration-agent-archive-deferred
  (:doc "Remote-busy archive fetch defers source reads and revalidates its guest and room")
  (niwa-agent-archive-test--with-room
    (dolist (outcome '(success disconnected stopped))
      (puthash 1 (list :ready t :writable nil) guests)
      (let ((busy t) resume stopped)
        (cl-letf (((symbol-function 'niwa-transport-busy-p) (lambda (&optional _) busy))
                  ((symbol-function 'niwa-collaboration--room-for-session)
                   (lambda (_) (unless stopped room)))
                  ((symbol-function 'niwa-transport-run-when-idle)
                   (lambda (_key path thunk &rest _)
                     (should (equal directory path))
                     (setq resume thunk))))
          (fetch '(:segment 1))
          (should resume)
          (should-not sent)
          (when (eq outcome 'disconnected) (remhash 1 guests))
          (setq busy nil stopped (eq outcome 'stopped))
          (funcall resume)
          (if (eq outcome 'success)
              (should (string-match-p "ARCHIVED CHILD ONLY" (json-encode sent)))
            (should-not sent)))))))

(niwa-deftest niwa-collaboration-agent-archive-chunks
  (:doc "Every chunk stays within the encoded frame budget including its archive selector")
  (niwa-agent-archive-test--with-room
    (let ((niwa-collaboration--max-frame-json-bytes 300))
      (cl-letf (((symbol-function 'niwa-collaboration--canonical-records)
                 (lambda (&rest _)
                   (cl-loop for number below 12 collect
                            (list :id (format "u%d" number) :kind "user" :revision 0
                                  :text "bounded archived user record")))))
        (fetch '(:segment 1))
        (should (> (length sent) 1))
        (dolist (frame sent)
          (should (= 1 (plist-get frame :segment)))
          (should (<= (string-bytes (json-encode frame)) 300)))))))

(niwa-deftest niwa-collaboration-agent-archive-transition
  (:doc "A one-shot archive open replies 0.2 seconds after a live poll without changing live throttling")
  (niwa-agent-archive-test--with-room
    (let ((now 1000.0))
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
        (niwa-collaboration--dispatch-frame
         room 1 '(:t "fetch-agent" :reqId 10 :path "/root/child"))
        (should (string-match-p "CURRENT CHILD ONLY" (json-encode sent)))
        (setq sent nil now 1000.2)
        (niwa-collaboration--dispatch-frame
         room 1 '(:t "fetch-agent" :reqId 11 :path "/root/child" :segment 1))
        (should sent)
        (should (= 11 (plist-get (car sent) :reqId)))
        (should (= 1 (plist-get (car sent) :segment)))
        (should (string-match-p "ARCHIVED CHILD ONLY" (json-encode sent)))
        (should-not (plist-get (car sent) :error))
        (should (= 1000.0 (plist-get (gethash 1 guests) :last-agent-fetch)))
        ;; Live polls still use their original silent throttle and cadence.
        (setq sent nil now 1000.3)
        (niwa-collaboration--dispatch-frame
         room 1 '(:t "fetch-agent" :reqId 12 :path "/root/child"))
        (should-not sent)
        (setq now 1001.1)
        (niwa-collaboration--dispatch-frame
         room 1 '(:t "fetch-agent" :reqId 13 :path "/root/child"))
        (should (= 13 (plist-get (car sent) :reqId)))
        (should (string-match-p "CURRENT CHILD ONLY" (json-encode sent)))))))

(niwa-deftest niwa-collaboration-agent-archive-throttle
  (:doc "Repeated one-shot archive requests get a correlated retry error, never silence or lost artifact authority")
  (niwa-agent-archive-test--with-room
    (let ((now 1000.0))
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
        (niwa-collaboration--dispatch-frame
         room 1 '(:t "fetch-agent" :reqId 20 :path "/root/child" :segment 1))
        (should (string-match-p "ARCHIVED CHILD ONLY" (json-encode sent)))
        (let ((artifacts (plist-get (gethash 1 guests) :agent-artifacts)))
          (should artifacts)
          (setq sent nil now 1000.2)
          (niwa-collaboration--dispatch-frame
           room 1 '(:t "fetch-agent" :reqId 21 :path "/root/child" :segment 1))
          (should (= 1 (length sent)))
          (should (= 21 (plist-get (car sent) :reqId)))
          (should (equal "/root/child" (plist-get (car sent) :path)))
          (should (= 1 (plist-get (car sent) :segment)))
          (should (equal "Please wait a moment, then retry." (plist-get (car sent) :error)))
          (should-not (plist-member (car sent) :records))
          (should (equal artifacts (plist-get (gethash 1 guests) :agent-artifacts))))
        ;; A rejected request does not extend the admitted request's window.
        (setq sent nil now 1001.1)
        (niwa-collaboration--dispatch-frame
         room 1 '(:t "fetch-agent" :reqId 22 :path "/root/child" :segment 1))
        (should (= 22 (plist-get (car sent) :reqId)))
        (should-not (plist-get (car sent) :error))
        (should (string-match-p "ARCHIVED CHILD ONLY" (json-encode sent)))))))

(niwa-deftest niwa-collaboration--agent-frame-overhead
  (:doc "Encoded overhead accounts exactly for the optional archive selector")
  (dolist (segment '(nil 1 123456789))
    (should
     (= (niwa-collaboration--agent-frame-overhead 7 "/root/child" segment)
        (string-bytes
         (json-encode
          (append (list :t "agent" :reqId 7 :path "/root/child"
                        :digest (make-string 64 ?0) :records [] :final :json-false)
                  (when segment (list :segment segment)))))))))

(provide 'test-niwa-collaboration-agent-archive)
;;; test-niwa-collaboration-agent-archive.el ends here
