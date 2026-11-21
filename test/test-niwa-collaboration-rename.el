;;; test-niwa-collaboration-rename.el --- Browser rename authority -*- lexical-binding: t; -*-
;;; Commentary:
;; Browser names use the same writer and ownership as native manual names.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory load-file-name) "helpers"))
(require 'niwa-collaboration-recovery)
(require 'niwa-collaboration-lobby)
(require 'niwa-collaboration-transport)
(require 'niwa-session-naming)
(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory load-file-name) "niwa-engine-test-support"))

(niwa-deftest niwa-collaboration-browser-rename
  (:quiet t :doc "owner settings rename uses native Unicode normalization and explicit ownership")
  (niwa-engine-test--with-session
    (niwa-request-end)
    (let* ((guests (make-hash-table :test #'eql))
           (room (list :session session :data-buffer buffer :guests guests)))
      (puthash 1 (list :owner t :writable t) guests)
      (puthash 2 (list :writable t) guests)
      (cl-letf (((symbol-function 'niwa-collaboration--publish-status) #'ignore)
                ((symbol-function 'niwa-collaboration--publish-queue) #'ignore)
                ((symbol-function 'niwa-collaboration-recovery-send) #'ignore))
        (should-error (niwa-collaboration-recovery-handle
                       room 2 '(:action "rename" :value "forged")) :type 'user-error)
        (niwa-collaboration-recovery-handle
         room 1 '(:action "rename" :value "  Révision  du projet  "))
        (should (equal "Révision du projet" (niwa-session-name session)))
        (should (eq 'explicit (niwa-session-naming-state session)))
        ;; Native rename is metadata-only and remains available during a turn.
        (cl-letf (((symbol-function 'niwa-turn-busy-p) (lambda (&rest _) t)))
          (niwa-collaboration-recovery-handle
           room 1 '(:action "rename" :value "During turn")))
        (should (equal "During turn" (niwa-session-name session)))
        (should-error (niwa-collaboration-recovery-handle
                       room 1 '(:action "rename" :value "  ")) :type 'user-error)))))

(niwa-deftest niwa-collaboration-lobby--handle-rename
  (:quiet t :doc "only an owner can resolve a listed session and rename its native buffer")
  (niwa-engine-test--with-session
    (niwa-request-end)
    (let* ((guests (make-hash-table :test #'eql))
           (lobby (list :workspace workspace :guests guests :transport 'test))
           sent resolved)
      (puthash 1 (list :owner t :writable t) guests)
      (puthash 2 (list :writable t) guests)
      (cl-letf (((symbol-function 'niwa-collaboration-lobby--session-buffer)
                 (lambda (_lobby id)
                   (setq resolved t)
                   (unless (equal id "listed") (error "No such session"))
                   buffer))
                ((symbol-function 'niwa-collaboration--transport-send)
                 (lambda (_transport _peer frame) (push frame sent)))
                ((symbol-function 'niwa-collaboration-lobby--send-listing) #'ignore))
        (niwa-collaboration-lobby--handle-rename
         lobby 2 '(:reqId 1 :id "listed" :name "forged"))
        (should-not resolved)
        (should (eq :json-false (plist-get (car sent) :ok)))
        (niwa-collaboration-lobby--handle-rename
         lobby 1 '(:reqId 2 :id "../outside" :name "forged"))
        (should (eq :json-false (plist-get (car sent) :ok)))
        (niwa-collaboration-lobby--handle-rename
         lobby 1 '(:reqId 3 :id "listed" :name "Équipe   alpha"))
        (should (eq t (plist-get (car sent) :ok)))
        (should (equal "Équipe alpha" (niwa-session-name session)))
        (should (eq 'explicit (niwa-session-naming-state session)))))))

(niwa-deftest niwa-collaboration-lobby-rename-saved
  (:quiet t :doc "renaming a saved session restores it through the native writer without sharing")
  (niwa-engine-test--with-session
    (niwa-request-end)
    (niwa-session-artifacts-save session buffer)
    (let* ((id (niwa-session-session-id session))
           (guests (make-hash-table :test #'eql))
           (lobby (list :workspace workspace :guests guests))
           reply)
      (puthash 1 '(:owner t :writable t) guests)
      (test-niwa-session-persistence--release-and-kill buffer session)
      (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                 (lambda (_transport _peer frame) (setq reply frame))))
        (niwa-collaboration-lobby--handle-rename
         lobby 1 (list :reqId 1 :id id :name "Saved revision")))
      ;; Adopt the restored buffer even on failure so fixture teardown releases it.
      (when-let* ((restored (seq-find
                            (lambda (candidate)
                              (when-let* ((value (buffer-local-value 'niwa--session candidate)))
                                (equal id (niwa-session-session-id value))))
                            (buffer-list))))
        (setq buffer restored session (buffer-local-value 'niwa--session restored)))
      (should (eq t (plist-get reply :ok)))
      (should (buffer-live-p buffer))
      (should (equal "Saved revision" (niwa-session-name session)))
      (should (equal id (niwa-session-session-id session)))
      (should-not (niwa-collaboration--room-for-buffer buffer)))))

(niwa-deftest niwa-collaboration-lobby-rename-observer
  (:quiet t :doc "renames refresh only the same workspace and isolate disconnected peers")
  (niwa-engine-test--with-session :file-workspace t
    (let ((niwa-collaboration-lobby--lobbies (make-hash-table :test #'equal))
          (guests (make-hash-table :test #'eql))
          (other (niwa-workspace--create :root (file-name-concat root "other")))
          sent)
      (puthash 1 '(:owner t :writable t) guests)
      (puthash 2 '(:writable t) guests)
      (puthash (niwa-workspace-root workspace) (list :workspace workspace :guests guests)
               niwa-collaboration-lobby--lobbies)
      (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                 (lambda (_transport peer frame)
                   (push (cons peer (plist-get frame :t)) sent)
                   (when (= peer 1) (error "Disconnected peer")))))
        (niwa-collaboration-lobby--session-renamed (niwa-session--create :workspace other))
        (should-not sent)
        (niwa-collaboration-lobby--session-renamed session)
        (should (equal '((1 . "lobby") (2 . "lobby")) (sort sent :key #'car :lessp #'<)))))))

(provide 'test-niwa-collaboration-rename)
;;; test-niwa-collaboration-rename.el ends here
