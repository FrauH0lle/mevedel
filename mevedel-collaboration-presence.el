;;; mevedel-collaboration-presence.el --- Who else is on a guest's page -*- lexical-binding: t; -*-

;;; Commentary:

;; Tells every browser guest who else is on the page it shows.  A guest's
;; page is the store artifact it has open, or its room itself: the
;; conversation of a session room, the listing of a lobby.  One artifact
;; can be open from several rooms of a workspace, so an artifact page
;; spans the workspace's rooms and lobby; a room page is that room's
;; alone.  A browser counts once per page however many of its tabs are
;; there, and is active when any of them is.
;;
;; Lobby guests also learn how many people are in each live session
;; room and on each artifact.
;;
;; The browser reports its page and whether its tab is visible in its
;; hello and in a `viewing' frame on every change.  Admission, departure
;; and renames republish too.
;; Guests learn display names only; guest ids never leave the host.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'mevedel-collaboration)

;; `mevedel-artifact-store'
(declare-function mevedel-artifact-store-id-p "mevedel-artifact-store" (id))
(autoload 'mevedel-artifact-store-id-p "mevedel-artifact-store")

;; `mevedel-collaboration-guest'
(declare-function mevedel-collaboration--workspace-key
                  "mevedel-collaboration-guest" (room))

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-send
                  "mevedel-collaboration-transport" (transport peer frame))

;; `mevedel-structs'
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x))


(defun mevedel-collaboration-presence--rooms (key)
  "Return the live session rooms and lobby of the workspace with KEY."
  (let ((rooms (cl-remove-if-not
                (lambda (room)
                  (equal key (mevedel-collaboration--workspace-key room)))
                (mevedel-collaboration--room-list))))
    (when (boundp 'mevedel-collaboration-lobby--lobbies)
      (maphash (lambda (_root lobby)
                 (when (equal key (mevedel-collaboration--workspace-key lobby))
                   (push lobby rooms)))
               mevedel-collaboration-lobby--lobbies))
    rooms))

(defun mevedel-collaboration-presence--people (rooms)
  "Return (ROOM PEER GUEST IDENTITY) for every admitted guest of ROOMS.
IDENTITY is the browser's guest id, or one naming the peer alone when
the browser sent none."
  (let (people)
    (dolist (room rooms (nreverse people))
      (when (hash-table-p (plist-get room :guests))
        (maphash (lambda (peer guest)
                   (push (list room peer guest
                               (or (plist-get guest :guest-id)
                                   (format "peer:%d:%s" (sxhash-eq room) peer)))
                         people))
                 (plist-get room :guests))))))

(defun mevedel-collaboration-presence--counts (people key-of)
  "Return how many browsers of PEOPLE share each non-nil KEY-OF value.
KEY-OF maps one (ROOM PEER GUEST IDENTITY) entry to its key.  The
result is a vector of `(:id KEY :n COUNT)' plists."
  (let ((seen (make-hash-table :test #'equal))
        (counts (make-hash-table :test #'equal))
        result)
    (dolist (person people)
      (when-let* ((key (funcall key-of person))
                  ((not (gethash (cons key (nth 3 person)) seen))))
        (puthash (cons key (nth 3 person)) t seen)
        (puthash key (1+ (gethash key counts 0)) counts)))
    (maphash (lambda (key n) (push (list :id key :n n) result)) counts)
    (vconcat (sort result (lambda (a b) (string< (plist-get a :id)
                                                 (plist-get b :id)))))))

(defun mevedel-collaboration-presence--frame (person people)
  "Return the presence frame for PERSON, one entry of PEOPLE."
  (pcase-let* ((`(,room ,_peer ,guest ,self) person)
               (page (plist-get guest :viewing))
               (others (make-hash-table :test #'equal))
               (names nil))
    (pcase-dolist (`(,other-room ,_ ,other ,identity) people)
      (when (and (not (equal identity self))
                 (equal page (plist-get other :viewing))
                 (or page (eq room other-room)))
        (let ((known (gethash identity others)))
          (puthash identity
                   (list :name (if known (plist-get known :name)
                                 (plist-get other :name))
                         :active (if (or (eq t (plist-get known :active))
                                         (plist-get other :active))
                                     t :json-false))
                   others))))
    (maphash (lambda (_identity entry) (push entry names)) others)
    (append
     (list :t "presence"
           :people (vconcat (sort names (lambda (a b)
                                          (string< (plist-get a :name)
                                                   (plist-get b :name))))))
     ;; A lobby has no session of its own.
     (unless (plist-get room :session)
       (list :sessions
             (mevedel-collaboration-presence--counts
              people (lambda (entry)
                       (when-let* ((session (plist-get (car entry) :session)))
                         (mevedel-session-session-id session))))
             :artifacts
             (mevedel-collaboration-presence--counts
              people (lambda (entry) (plist-get (nth 2 entry) :viewing))))))))

(defun mevedel-collaboration-presence-publish (room)
  "Tell every guest of ROOM's workspace who else is on its page.
A guest is sent a frame only when its content changed.  ROOM need not
be live any more: a stopped room's workspace is told it is gone."
  (when-let* ((key (mevedel-collaboration--workspace-key room)))
    ;; ponytail: every change recomputes the workspace, quadratic in its
    ;; guests; index pages per workspace if rooms ever hold hundreds.
    (let ((people (mevedel-collaboration-presence--people
                   (mevedel-collaboration-presence--rooms key))))
      (dolist (person people)
        (let ((frame (mevedel-collaboration-presence--frame person people))
              (guest (nth 2 person)))
          (unless (equal frame (plist-get guest :presence-sent))
            (plist-put guest :presence-sent frame)
            (mevedel-collaboration--transport-send
             (plist-get (car person) :transport) (nth 1 person) frame)))))))

(defun mevedel-collaboration-presence-state (frame)
  "Return the `(:viewing PAGE :active ACTIVE)' a guest FRAME reports.
FRAME's `:page' names the open store artifact, or is null for the room
itself; `:active' is t while the guest's tab is visible.  A page that
is no artifact id signals an error."
  (let ((page (plist-get frame :page)))
    (unless (or (null page) (mevedel-artifact-store-id-p page))
      (error "Invalid artifact id: %S" page))
    (list :viewing page :active (eq t (plist-get frame :active)))))

(defun mevedel-collaboration-presence-handle (room peer frame)
  "Record the page and activity guest PEER of ROOM reports in FRAME."
  (when-let* ((guest (mevedel-collaboration--guest room peer)))
    (let ((state (mevedel-collaboration-presence-state frame)))
      (unless (and (equal (plist-get state :viewing) (plist-get guest :viewing))
                   (eq (plist-get state :active) (plist-get guest :active)))
        (plist-put guest :viewing (plist-get state :viewing))
        (plist-put guest :active (plist-get state :active))
        (mevedel-collaboration-presence-publish room)))))

(provide 'mevedel-collaboration-presence)
;;; mevedel-collaboration-presence.el ends here
