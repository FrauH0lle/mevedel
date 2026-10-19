;;; test-mevedel-collaboration-rename.el --- Browser rename authority -*- lexical-binding: t; -*-
;;; Commentary:
;; Browser names use the same writer and ownership as native manual names.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory load-file-name) "helpers"))
(require 'mevedel-collaboration-recovery)
(require 'mevedel-collaboration-lobby)
(require 'mevedel-session-naming)
(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory load-file-name) "mevedel-engine-test-support"))

(mevedel-deftest mevedel-collaboration-browser-rename
  (:quiet t :doc "owner settings rename uses native Unicode normalization and explicit ownership")
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (let* ((guests (make-hash-table :test #'eql))
           (room (list :session session :data-buffer buffer :guests guests)))
      (puthash 1 (list :owner t :writable t) guests)
      (puthash 2 (list :writable t) guests)
      (cl-letf (((symbol-function 'mevedel-collaboration--publish-status) #'ignore)
                ((symbol-function 'mevedel-collaboration--publish-queue) #'ignore)
                ((symbol-function 'mevedel-collaboration-recovery-send) #'ignore))
        (should-error (mevedel-collaboration-recovery-handle
                       room 2 '(:action "rename" :value "forged")) :type 'user-error)
        (mevedel-collaboration-recovery-handle
         room 1 '(:action "rename" :value "  Révision  du projet  "))
        (should (equal "Révision du projet" (mevedel-session-name session)))
        (should (eq 'explicit (mevedel-session-naming-state session)))
        ;; Native rename is metadata-only and remains available during a turn.
        (cl-letf (((symbol-function 'mevedel-turn-busy-p) (lambda (&rest _) t)))
          (mevedel-collaboration-recovery-handle
           room 1 '(:action "rename" :value "During turn")))
        (should (equal "During turn" (mevedel-session-name session)))
        (should-error (mevedel-collaboration-recovery-handle
                       room 1 '(:action "rename" :value "  ")) :type 'user-error)))))

(mevedel-deftest mevedel-collaboration-lobby--handle-rename
  (:quiet t :doc "only an owner can resolve a listed session and rename its native buffer")
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (let* ((guests (make-hash-table :test #'eql))
           (lobby (list :workspace workspace :guests guests :transport 'test))
           sent resolved)
      (puthash 1 (list :owner t :writable t) guests)
      (puthash 2 (list :writable t) guests)
      (cl-letf (((symbol-function 'mevedel-collaboration-lobby--session-buffer)
                 (lambda (_lobby id)
                   (setq resolved t)
                   (unless (equal id "listed") (error "No such session"))
                   buffer))
                ((symbol-function 'mevedel-collaboration--transport-send)
                 (lambda (_transport _peer frame) (push frame sent)))
                ((symbol-function 'mevedel-collaboration-lobby--send-listing) #'ignore))
        (mevedel-collaboration-lobby--handle-rename
         lobby 2 '(:reqId 1 :id "listed" :name "forged"))
        (should-not resolved)
        (should (eq :json-false (plist-get (car sent) :ok)))
        (mevedel-collaboration-lobby--handle-rename
         lobby 1 '(:reqId 2 :id "../outside" :name "forged"))
        (should (eq :json-false (plist-get (car sent) :ok)))
        (mevedel-collaboration-lobby--handle-rename
         lobby 1 '(:reqId 3 :id "listed" :name "Équipe   alpha"))
        (should (eq t (plist-get (car sent) :ok)))
        (should (equal "Équipe alpha" (mevedel-session-name session)))
        (should (eq 'explicit (mevedel-session-naming-state session)))))))

(mevedel-deftest mevedel-collaboration-lobby-rename-saved
  (:quiet t :doc "renaming a saved session restores it through the native writer without sharing")
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (mevedel-session-artifacts-save session buffer)
    (let* ((id (mevedel-session-session-id session))
           (guests (make-hash-table :test #'eql))
           (lobby (list :workspace workspace :guests guests))
           reply)
      (puthash 1 '(:owner t :writable t) guests)
      (test-mevedel-session-persistence--release-and-kill buffer session)
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                 (lambda (_transport _peer frame) (setq reply frame))))
        (mevedel-collaboration-lobby--handle-rename
         lobby 1 (list :reqId 1 :id id :name "Saved revision")))
      ;; Adopt the restored buffer even on failure so fixture teardown releases it.
      (when-let* ((restored (seq-find
                            (lambda (candidate)
                              (when-let* ((value (buffer-local-value 'mevedel--session candidate)))
                                (equal id (mevedel-session-session-id value))))
                            (buffer-list))))
        (setq buffer restored session (buffer-local-value 'mevedel--session restored)))
      (should (eq t (plist-get reply :ok)))
      (should (buffer-live-p buffer))
      (should (equal "Saved revision" (mevedel-session-name session)))
      (should (equal id (mevedel-session-session-id session)))
      (should-not (mevedel-collaboration--room-for-buffer buffer)))))

(mevedel-deftest mevedel-collaboration-lobby-rename-observer
  (:quiet t :doc "renames refresh only the same workspace and isolate disconnected peers")
  (let* ((session (mevedel-session--create :workspace 'workspace))
         (mevedel-collaboration-lobby--lobbies (make-hash-table :test #'equal))
         (guests (make-hash-table :test #'eql))
         sent)
    (puthash 1 '(:owner t) guests)
    (puthash 2 '(:writable t) guests)
    (puthash "same" (list :workspace 'workspace :guests guests)
             mevedel-collaboration-lobby--lobbies)
    (puthash "other" (list :workspace 'other :guests guests)
             mevedel-collaboration-lobby--lobbies)
    (cl-letf (((symbol-function 'mevedel-collaboration-lobby--send-listing)
               (lambda (lobby peer)
                 (should (eq 'workspace (plist-get lobby :workspace)))
                 (push peer sent)
                 (when (= peer 1) (error "Disconnected peer")))))
      (mevedel-collaboration-lobby--session-renamed session)
      (should (equal '(1 2) (sort sent #'<))))))

(provide 'test-mevedel-collaboration-rename)
;;; test-mevedel-collaboration-rename.el ends here
