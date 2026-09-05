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

(provide 'test-mevedel-collaboration-editing)
;;; test-mevedel-collaboration-editing.el ends here
