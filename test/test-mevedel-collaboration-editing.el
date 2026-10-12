;;; test-mevedel-collaboration-editing.el --- Editing transfer authority -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared editing tests through the real host and existing public seams.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-artifact)
(require 'mevedel-collaboration-editing)

(mevedel-deftest mevedel-collaboration-editing--browser-value
  (:doc "Browser projection keeps editable state and attribution without repeated image snapshots")
  (let* ((image (make-string (* 1024 1024) ?x))
         (changes (vector (list :id "picture" :before (list :src image)
                                :after (list :src image))
                          (list :id "removed" :before (list :src image) :after nil)))
         (tx (list :id "move" :actor "Agent: test" :revision 7 :time 123 :changes changes))
         (result (list :id "board" :kind "whiteboard" :revision 7 :crdt "editable-state"
                       :content (vector (list :src image)) :transaction tx
                       :transactions (vector tx) :discussions []
                       :snapshot (list :content "question context")))
         (value (list :result result))
         (projected (mevedel-collaboration-editing--browser-value value))
         (visible (plist-get projected :result)))
    (should (< (length (mevedel-shared-editing--json projected)) 1000))
    (should (equal (plist-get visible :crdt) "editable-state"))
    (should (equal (plist-get visible :snapshot) (plist-get result :snapshot)))
    (should-not (plist-member visible :content))
    (should-not (plist-member visible :transaction))
    (should (equal (plist-get (aref (plist-get visible :transactions) 0) :changes)
                   [(:id "picture" :after t) (:id "removed" :after :json-false)]))
    (should (eq (plist-get result :transactions) (plist-get (plist-get value :result) :transactions)))
    (should (plist-member result :content))
    (should (equal (plist-get (aref changes 0) :after) (list :src image)))
    (dolist (plain (list [(:id "board" :kind "whiteboard")]
                        '(:error "Save refused")
                        '(:result (:data "export bytes" :mime "image/png"))))
      (should (equal (mevedel-collaboration-editing--browser-value plain) plain)))))

(mevedel-deftest mevedel-collaboration-editing--send
  (:doc "Read replies and broadcast events both use compact browser contributions")
  (let* ((tx (list :id "move" :actor "Guest: test" :revision 2 :time 10
                   :changes (vector (list :id "picture" :after
                                          (list :src (make-string (* 1024 1024) ?x))))))
         (payload (list :id "board" :revision 2 :transactions (vector tx)))
         frames)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_transport _peer frame) (push frame frames))))
      (dolist (value (list payload (list :result payload)))
        (setq frames nil)
        (mevedel-collaboration-editing--send nil 1 "event" value)
        (should (= (length frames) 1))
        (should (< (plist-get (car frames) :total) 1000))))))

(mevedel-deftest mevedel-collaboration-editing-handle
		 (:doc "Incomplete transfers clean up and authority is checked after assembly and before commit")
		 (let* ((directory (make-temp-file "mevedel-editing-transfer-" t))
			(workspace (mevedel-workspace--create :type 'file :id "w" :root directory :name "w"))
			(session (mevedel-session--create :save-path directory :authority-mode 'pid-lock
							  :workspace workspace))
			(mevedel-shared-editing--runtimes (make-hash-table :test #'equal))
			(mevedel-artifact-lease--held (make-hash-table :test #'equal))
			(buffer (generate-new-buffer " *editing-transfer-test*"))
			(guests (make-hash-table :test #'eql))
			(guest (list :name "Alice" :writable t))
			(room (list :session session :data-buffer buffer :transport 'test :guests guests))
			(mevedel-collaboration--rooms (mevedel-test-room-registry room))
			(data (base64-encode-string
			       "{\"action\":\"create\",\"kind\":\"whiteboard\",\"id\":\"one\",\"opId\":\"op\",\"title\":\"Board\"}" t))
			frames)
		   (unwind-protect
		       (with-current-buffer buffer
			 (setq-local mevedel--session session)
			 (setf (mevedel-session-root-buffer session) buffer)
			 (puthash 1 guest guests)
			 (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
				    (lambda (_transport _peer frame) (push frame frames))))
			   (cl-labels ((chunk (offset text)
					 (mevedel-collaboration-editing-handle
					  room 1 (list :t "editing" :reqId 1 :offset offset
						       :total (length data) :data text)))
				       (last-error ()
					 (plist-get (mevedel-shared-editing--parse
						     (base64-decode-string (plist-get (car frames) :data)))
						    :error)))
			     (chunk 0 (substring data 0 8))
			     (let ((timer (plist-get (plist-get guest :editing-transfer) :timer)))
			       (chunk 9 (substring data 8))
			       (should (string-match-p "interrupted" (last-error)))
			       (should-not (memq timer timer-list))
			       (should-not (plist-get guest :editing-transfer)))
			     (chunk 0 (substring data 0 8))
			     (plist-put (plist-get guest :editing-transfer) :started (- (float-time) 61))
			     (chunk 8 (substring data 8))
			     (should (string-match-p "interrupted" (last-error)))
			     (should-not (plist-get guest :editing-transfer))
			     (chunk 0 (substring data 0 8))
			     (plist-put guest :writable nil)
			     (chunk 8 (substring data 8))
			     (should (string-match-p "does not permit" (last-error)))
			     (should-not (mevedel-shared-editing-list workspace))
			     (plist-put guest :writable t)
			     (chunk 0 data)
			     (plist-put guest :writable nil)
			     (let ((deadline (+ (float-time) 10)))
			       (while (and (or (plist-get (mevedel-shared-editing--runtime workspace) :queue)
					       (plist-get (mevedel-shared-editing--runtime workspace) :active))
					   (< (float-time) deadline))
				 (accept-process-output nil 0.05)))
			     (should-not (mevedel-shared-editing-list workspace))
			     (plist-put guest :writable t)
			     (chunk 0 (substring data 0 8))
			     (let ((timer (plist-get (plist-get guest :editing-transfer) :timer)))
			       (mevedel-collaboration-editing-depart room 1)
			       (should-not (memq timer timer-list))
			       (should-not (plist-get guest :editing-transfer))))))
		     (mevedel-shared-editing-stop)
		     (mevedel-artifact-lease-release-all)
		     (when (buffer-live-p buffer) (kill-buffer buffer))
		     (delete-directory directory t))))

(mevedel-deftest mevedel-collaboration-editing--presence
  (:doc "Terminal clears bypass sample throttling once, with item and writer authority intact")
  ,test
  (test)
  (let* ((guests (make-hash-table :test #'eql))
         (guest (list :name "Alice" :writable t :editing-item "board"))
         (room (list :guests guests :transport 'test))
         (now 100.0)
         frames)
    (puthash 1 guest guests)
    (puthash 2 (list :editing-item "board") guests)
    (puthash 3 (list :editing-item "other") guests)
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) now))
              ((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_transport peer frame) (push (cons peer frame) frames))))
      (cl-labels ((point (mode &optional id)
                    (mevedel-collaboration-editing--presence
                     room 1 guest (list :id (or id "board") :mode mode
                                        :point (unless (equal mode "clear") '(20 30))))))
        (mevedel-collaboration-editing--presence
         room 1 guest '(:id "board" :mode "laser" :point (20 30)
                        :trail ((10 20 40 0) (20 30 0 7))))
        (should (equal (plist-get (cdar frames) :trail) [[10 20 40 0] [20 30 0 7]]))
        (point "laser")
        (should (= (length frames) 1))
        (point "clear" "other")
        (should (= (length frames) 1))
        (point "clear")
        (should (= (length frames) 2))
        (should (= (caar frames) 2))
        (should (equal (plist-get (cdar frames) :mode) "clear"))
        (point "clear")
        (should (= (length frames) 2))
        (setq now 100.06)
        (dolist (trail (list '((20 30 -1 0)) '((20 30 551 0)) '((20 30 "bad" 0))
                             '((20 30 0)) '((20 30 0 -1)) '((20 30 0 1.5))
                             (make-list 65 '(20 30 0 0))))
          (mevedel-collaboration-editing--presence
           room 1 guest (list :id "board" :mode "laser" :point '(20 30) :trail trail)))
        (should (= (length frames) 2))
        (point "cursor")
        (should (= (length frames) 3))
        (plist-put guest :writable nil)
        (setq now 100.12)
        (point "laser")
        (should (= (length frames) 3)))))

  :doc "Movement previews are bounded writer-only metadata scoped to the active item"
  (let* ((guests (make-hash-table :test #'eql))
         (guest (list :name "Alice" :writable t :editing-item "board"))
         (room (list :guests guests :transport 'test))
         (preview '(:opId "save" :shapes ((:id "shape" :box (10 20 100 80)))))
         frames)
    (puthash 1 guest guests)
    (puthash 2 (list :editing-item "board") guests)
    (puthash 3 (list :editing-item "other") guests)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
               (lambda (_ peer value) (push (cons peer value) frames))))
      (cl-labels ((send (value &optional id)
                    (plist-put guest :editing-presence-at 0)
                    (mevedel-collaboration-editing--presence
                     room 1 guest (list :id (or id "board") :mode "cursor" :point '(0 0) :preview value))))
        (send preview)
        (should (= (length frames) 1))
        (should (= (caar frames) 2))
        (should (equal (plist-get (cdar frames) :preview)
                       '(:opId "save" :shapes [(:id "shape" :box [10 20 100 80])])))
        (dolist (bad (list '(:shapes ((:id "shape" :box (1 2 -1 4))))
                          '(:shapes ((:id "shape" :box (1 2 3 "bad"))))
                          '(:shapes ((:id "shape" :box (1 2 3 1000001))))
                          (list :shapes (make-list 101 '(:id "shape" :box (1 2 3 4))))))
          (send bad))
        (send preview "other")
        (plist-put guest :writable nil)
        (send preview)
        (should (= (length frames) 1))))))

(require 'mevedel-view)
(require 'mevedel-pending-inputs)

(mevedel-deftest mevedel-collaboration-editing--dispatch
  ()
  ,test
  (test)
  :doc "Guest requests forward only closed keys, including a board area, with host attribution"
  (let* ((guests (make-hash-table :test #'eql))
         (guest (list :name "Alice" :writable t))
         (session (mevedel-session--create
                   :workspace (mevedel-workspace--create :type 'file :id "w" :root "/tmp/")))
         (room (list :session session :guests guests))
         (mevedel-collaboration--rooms (mevedel-test-room-registry room))
         requests)
    (puthash 1 guest guests)
    (cl-letf (((symbol-function 'mevedel-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'mevedel-collaboration--guest) (lambda (_room _peer) guest))
              ((symbol-function 'mevedel-shared-editing-call)
               (lambda (_workspace request &rest _) (push request requests))))
      (mevedel-collaboration-editing--dispatch
       room 1 guest 7 '(:action "rename" :id "board" :opId "op" :title "Here"
                                :selection ["a"] :actor "Agent: forged" :state (:revision 99)))
      (let ((request (car requests)))
        (should (equal (plist-get request :actor) "Guest: Alice"))
        (should-not (plist-member request :state))
        (should-not (plist-member request :selection))
        (should (equal (plist-get request :action) "rename")))
      ;; Discussions belong to the discussion mechanism, which names the item
      ;; by its store identity; the reply returns on the editor's channel.
      (setq requests nil)
      (let (performed sent)
        (cl-letf (((symbol-function 'mevedel-collaboration-discussions-perform)
                   (lambda (_room _guest args callback &rest _)
                     (setq performed args)
                     (funcall callback (list :discussionId "general" :queued t))))
                  ((symbol-function 'mevedel-collaboration-editing--send)
                   (lambda (_room _peer req-id value) (push (cons req-id value) sent))))
          (mevedel-collaboration-editing--dispatch
           room 1 guest 8 '(:action "discussion" :op "post" :id "board"
                                    :target (:kind "whole") :messageId "m" :text "Make it pretty"
                                    :toAssistant t)))
        (should-not requests)
        (should (equal (plist-get performed :action) "post"))
        (should (equal (plist-get performed :item) "artifact:board"))
        (should (equal (plist-get performed :text) "Make it pretty"))
        (should (equal sent '((8 :result (:discussionId "general" :queued t))))))
      ;; Full links delete with their own attribution; view links cannot.
      (setq requests nil)
      (mevedel-collaboration-editing--dispatch room 1 guest 9 '(:action "delete" :id "board"))
      (should (equal '("delete" "board" "Guest: Alice")
                     (let ((request (car requests)))
                       (list (plist-get request :action) (plist-get request :id)
                             (plist-get request :actor)))))
      (let ((viewer (list :name "Bob")))
        (cl-letf (((symbol-function 'mevedel-collaboration--guest) (lambda (_room _peer) viewer)))
          (should-error (mevedel-collaboration-editing--dispatch
                         room 2 viewer 10 '(:action "delete" :id "board")))))))

  :doc "A deleted item reaches every guest of the workspace's rooms, and its viewers leave it"
  (let* ((guests (make-hash-table :test #'eql))
         (room (list :session 'session :guests guests))
         (lobby (list :workspace 'workspace :guests (make-hash-table :test #'eql)))
         sent)
    (puthash 1 (list :name "Alice" :editing-item "board") guests)
    (puthash 2 (list :name "Bob" :editing-item "notes") guests)
    (puthash 3 (list :name "Lobby guest") (plist-get lobby :guests))
    (cl-letf (((symbol-function 'mevedel-collaboration--workspace-rooms)
               (lambda (workspace) (should (eq workspace 'workspace)) (list room lobby)))
              ((symbol-function 'mevedel-collaboration-editing--send)
               (lambda (_room peer req-id value) (push (list peer req-id value) sent))))
      (mevedel-collaboration-editing--changed
       'workspace '(:id "board" :deleted t :actor "Guest: Ann") nil))
    (should (equal '((0 "event" (:event "deleted" :id "board" :actor "Ann"))) sent))
    (should-not (plist-get (gethash 1 guests) :editing-item))
    (should (equal "notes" (plist-get (gethash 2 guests) :editing-item))))

  :doc "A change is one broadcast per room, carrying the update only while someone views the item"
  (let* ((guests (make-hash-table :test #'eql))
         (room (list :session 'session :guests guests))
         (empty (list :session 'other :guests (make-hash-table :test #'eql)))
         (state '(:id "board" :kind "whiteboard" :title "Board" :revision 4 :transactions [t]))
         sent)
    (puthash 1 (list :name "Alice" :editing-item "notes") guests)
    (puthash 2 (list :name "Bob") guests)
    (cl-letf (((symbol-function 'mevedel-collaboration--workspace-rooms)
               (lambda (_workspace) (list room empty)))
              ((symbol-function 'mevedel-collaboration-editing--send)
               (lambda (sent-room peer req-id value)
                 (should (eq sent-room room))
                 (push (list peer req-id value) sent))))
      (mevedel-collaboration-editing--changed 'workspace state '(:update "u" :discussions []))
      (should (equal '((0 "event" (:event "changed" :id "board" :kind "whiteboard"
                                  :title "Board" :revision 4)))
                     sent))
      (setq sent nil)
      (plist-put (gethash 2 guests) :editing-item "board")
      (mevedel-collaboration-editing--changed 'workspace state '(:update "u" :discussions []))
      (should (equal '((0 "event" (:event "changed" :id "board" :kind "whiteboard"
                                  :title "Board" :revision 4
                                  :update "u" :discussions [] :transactions [t])))
                     sent))))

  :doc "Library requests reach the host's library only for writable links"
  (let* ((directory (make-temp-file "mevedel-editing-library-" t))
         (mevedel-shared-library-directory directory)
         (session (mevedel-session--create))
         (guests (make-hash-table :test #'eql))
         (guest (list :name "Alice" :writable t))
         (room (list :session session :transport 'test :guests guests))
         (mevedel-collaboration--rooms (mevedel-test-room-registry room))
         frames)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                   (lambda (_transport _peer frame) (push frame frames))))
          (puthash 1 guest guests)
          ;; Refusals signal before any reply; the room handler reports them.
          (cl-labels ((reply (args)
                        (setq frames nil)
                        (condition-case err
                            (progn
                              (mevedel-collaboration-editing--dispatch room 1 guest 7 args)
                              (mevedel-shared-editing--parse
                               (base64-decode-string (plist-get (car frames) :data))))
                          (error (list :error (error-message-string err))))))
            (should (equal "Built-in"
                           (plist-get (aref (plist-get (plist-get (reply '(:action "library")) :result)
                                                       :libraries) 0)
                                      :name)))
            (plist-put guest :writable nil)
            (should (string-match-p "does not permit"
                                    (plist-get (reply '(:action "library")) :error)))
            (should-not (directory-files directory nil "excalidrawlib"))))
      (delete-directory directory t))))

(provide 'test-mevedel-collaboration-editing)
;;; test-mevedel-collaboration-editing.el ends here
