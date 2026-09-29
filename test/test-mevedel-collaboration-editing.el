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
                       :transactions (vector tx) :comments []
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
			(session (mevedel-session--create :save-path directory :authority-mode 'pid-lock))
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
			     (should-not (mevedel-shared-editing-list session))
			     (plist-put guest :writable t)
			     (chunk 0 data)
			     (plist-put guest :writable nil)
			     (let ((deadline (+ (float-time) 10)))
			       (while (and (or (plist-get mevedel-shared-editing--runtime :queue)
					       (plist-get mevedel-shared-editing--runtime :active))
					   (< (float-time) deadline))
				 (accept-process-output nil 0.05)))
			     (should-not (mevedel-shared-editing-list session))
			     (plist-put guest :writable t)
			     (chunk 0 (substring data 0 8))
			     (let ((timer (plist-get (plist-get guest :editing-transfer) :timer)))
			       (mevedel-collaboration-editing-depart room 1)
			       (should-not (memq timer timer-list))
			       (should-not (plist-get guest :editing-transfer))))))
		     (when (buffer-live-p buffer)
		       (with-current-buffer buffer (mevedel-shared-editing-stop))
		       (kill-buffer buffer))
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
                        :trail ((10 20 40) (20 30 0))))
        (should (equal (plist-get (cdar frames) :trail) [[10 20 40] [20 30 0]]))
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
        (dolist (trail (list '((20 30 -1)) '((20 30 551)) '((20 30 "bad"))
                             (make-list 65 '(20 30 0))))
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

(mevedel-deftest mevedel-collaboration-editing--question-key
  (:doc "Retry fingerprints bind the item, text, comment and frozen context")
  (let* ((args '(:id "one" :text "Why?" :commentId "c" :expected (:title "A")))
         (key (mevedel-collaboration-editing--question-key args)))
    (should (equal key (mevedel-collaboration-editing--question-key (copy-tree args))))
    (dolist (change '((:id . "two") (:text . "How?") (:commentId . "d") (:commentVersion . "new-reply")
                      (:expected . (:title "B"))))
      (should-not (equal key (mevedel-collaboration-editing--question-key
                             (plist-put (copy-tree args) (car change) (cdr change))))))))

(mevedel-deftest mevedel-collaboration-editing--ask
  (:doc "Questions use the ordinary queue and remain idempotent after transcript delivery")
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create :type 'file :id "shared-ask" :root "/tmp"))
           (session (mevedel-session-create "main" workspace))
           (room (list :session session :data-buffer data-buf))
           (guest '(:name "Alice" :guest-id "alice" :role "full"))
           (args '(:id "document" :questionId "question" :commentId "comment" :commentVersion "reply"
                       :text "Why?" :expected (:kind "document" :scope "selection")))
           (result '(:id "document" :title "Notes" :revision 7 :quote "a passage"
                         :snapshot (:id "document" :title "Notes" :revision 7
                                        :kind "document" :scope "selection" :content "a passage"))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session mevedel--workspace workspace))
      (mevedel-session-set-pending-input-paused session t)
      (let* ((receipt (mevedel-collaboration-editing--ask room guest args result))
             (entry (car (mevedel-session-pending-follow-ups session)))
             (shared (plist-get entry :shared-question)))
        (should (plist-get receipt :queued))
        (should (equal (plist-get shared :revision) 7))
        (should (equal (plist-get shared :commentVersion) "reply"))
        (should (string-match-p "a passage" (plist-get entry :input)))
        (should (equal receipt (mevedel-collaboration-editing--ask room guest args result)))
        (should (= 1 (length (mevedel-session-pending-follow-ups session))))
        (should-error (mevedel-collaboration-editing--find-question
                       room (plist-put (copy-tree args) :id "other")))
        ;; Exercise the actual composer insertion and audit projection, retaining
        ;; another session's in-flight attribution across this session's drain.
        (with-temp-buffer
          (setq-local mevedel-view--pending-guest-attribution '(:name "Other"))
          (mevedel-session-set-pending-input-paused session nil)
          (cl-letf (((symbol-function 'gptel-send) #'ignore))
            (mevedel-view--drain-follow-up data-buf))
          (should (equal mevedel-view--pending-guest-attribution '(:name "Other"))))
        (should-not (mevedel-session-pending-follow-ups session))
        (with-current-buffer data-buf
          (should-not mevedel-view--pending-guest-attribution)
          (should (equal shared (plist-get (cdar (mevedel-transcript-audit-guest-prompts)) :shared))))
        (should (plist-get (mevedel-collaboration-editing--find-question room args) :delivered))
        (should (plist-get (mevedel-collaboration-editing--ask room guest args result) :delivered))
        (should-not (mevedel-session-pending-follow-ups session))))))

(mevedel-deftest mevedel-collaboration-editing--dispatch
  (:doc "Guest requests forward only closed keys, including a board area, with host attribution")
  (let* ((guests (make-hash-table :test #'eql))
         (guest (list :name "Alice" :writable t))
         (session 'session)
         (room (list :session session :guests guests))
         (mevedel-collaboration--rooms (mevedel-test-room-registry room))
         requests)
    (puthash 1 guest guests)
    (cl-letf (((symbol-function 'mevedel-collaboration--room-for-session) (lambda (_) room))
              ((symbol-function 'mevedel-collaboration--guest) (lambda (_room _peer) guest))
              ((symbol-function 'mevedel-shared-editing-call)
               (lambda (_session request &rest _) (push request requests))))
      (mevedel-collaboration-editing--dispatch
       room 1 guest 7 '(:action "comment" :id "board" :opId "op" :text "Here" :selection ["a"]
                                :region [0 0 40 30] :expected (:scope "selection")
                                :actor "Agent: forged" :state (:revision 99)))
      (let ((request (car requests)))
        (should (equal (plist-get request :region) [0 0 40 30]))
        (should (equal (plist-get request :selection) ["a"]))
        (should (equal (plist-get request :actor) "Guest: Alice"))
        (should-not (plist-member request :state))
        (should (equal (plist-get request :action) "comment")))
      ;; A room message about a whole item reaches the helper as a
      ;; question read carrying the whole-item flag.
      (setq requests nil)
      (cl-letf (((symbol-function 'mevedel-collaboration-editing--find-question) #'ignore))
        (mevedel-collaboration-editing--dispatch
         room 1 guest 8 '(:action "ask" :id "board" :questionId "q" :text "Make it pretty"
                                  :whole t)))
      (let ((request (car requests)))
        (should (equal (plist-get request :action) "read"))
        (should (eq (plist-get request :question) t))
        (should (eq (plist-get request :whole) t))
        (should (equal (plist-get request :text) "Make it pretty"))))))

(provide 'test-mevedel-collaboration-editing)
;;; test-mevedel-collaboration-editing.el ends here
