;;; test-niwa-collaboration-presence.el --- Guest presence tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests who-else-is-here frames: pages spanning a workspace's rooms and
;; lobby, one count per browser, lobby counts, and change-only sending.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'subr-x)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-lobby)
(require 'niwa-collaboration-presence)
(require 'niwa-structs)
(require 'niwa-workspace)

(defvar niwa-collaboration-artifact-room--rooms)

(defun niwa-collaboration-presence-test--room (transport workspace id &rest guests)
  "Return a room on TRANSPORT for a WORKSPACE session ID holding GUESTS.
Each of GUESTS is (PEER NAME GUEST-ID VIEWING ACTIVE).  A nil ID makes
the room a lobby of WORKSPACE."
  (let ((table (make-hash-table :test #'eql)))
    (pcase-dolist (`(,peer ,name ,guest-id ,viewing ,active) guests)
      (puthash peer (list :name name :guest-id guest-id
                          :viewing viewing :active active)
               table))
    (if id
        (let ((session (niwa-session--create :name id :workspace workspace)))
          (setf (niwa-session-session-id session) id)
          (list :session session :transport transport :guests table))
      (list :workspace workspace :transport transport :guests table))))

(defmacro niwa-collaboration-presence-test--with-rooms (&rest body)
  "Run BODY with two workspaces' rooms and a lobby; capture sends in `sent'.
`sent' holds (TRANSPORT PEER FRAME) entries, newest first."
  (declare (indent 0))
  `(let* ((ws (niwa-workspace--create :type 'project :id "/a/" :root "/a/"))
          (other (niwa-workspace--create :type 'project :id "/b/" :root "/b/"))
          (r1 (niwa-collaboration-presence-test--room
               'r1 ws "s1"
               '(1 "Ann" "ann-browser-1" nil t)
               '(2 "Bob" "bob-browser-1" nil t)
               '(3 "Ann" "ann-browser-1" "board" t)))
          (r2 (niwa-collaboration-presence-test--room
               'r2 ws "s2" '(1 "Cleo" "cleo-browser" "board" nil)))
          (r3 (niwa-collaboration-presence-test--room
               'r3 other "s3" '(1 "Eve" "eve-browser1" "board" t)))
          (lobby (niwa-collaboration-presence-test--room
                  'lobby ws nil '(1 "Dan" "dan-browser1" nil t)))
          (niwa-collaboration--rooms (niwa-test-room-registry r1 r2 r3))
          (niwa-collaboration-lobby--lobbies
           (let ((table (make-hash-table :test #'equal)))
             (puthash "/a/" lobby table)
             table))
          (sent nil))
     (ignore r2 r3)
     (cl-letf (((symbol-function 'niwa-collaboration--transport-send)
                (lambda (transport peer frame)
                  (push (list transport peer frame) sent))))
       ,@body)))

(defun niwa-collaboration-presence-test--frame (sent transport peer)
  "Return the newest frame in SENT for PEER on TRANSPORT."
  (nth 2 (cl-find-if (lambda (entry)
                       (and (eq (car entry) transport) (eql (nth 1 entry) peer)))
                     sent)))

(defun niwa-collaboration-presence-test--names (frame)
  "Return FRAME's people as (NAME . ACTIVE) pairs."
  (mapcar (lambda (person) (cons (plist-get person :name)
                                 (plist-get person :active)))
          (plist-get frame :people)))

(niwa-deftest niwa-collaboration-presence-publish ()
  ,test
  (test)
  :doc "tells each guest who else shares its page across the workspace, once per browser"
  (niwa-collaboration-presence-test--with-rooms
    (niwa-collaboration-presence-publish r1)
    (let ((frame (lambda (transport peer)
                   (niwa-collaboration-presence-test--frame sent transport peer))))
      ;; A room page is that room's; Ann's board tab is elsewhere.
      (should (equal (niwa-collaboration-presence-test--names (funcall frame 'r1 1))
                     '(("Bob" . t))))
      (should (equal (niwa-collaboration-presence-test--names (funcall frame 'r1 2))
                     '(("Ann" . t))))
      ;; An artifact page spans the workspace's rooms, not other workspaces.
      (should (equal (niwa-collaboration-presence-test--names (funcall frame 'r1 3))
                     '(("Cleo" . :json-false))))
      (should (equal (niwa-collaboration-presence-test--names (funcall frame 'r2 1))
                     '(("Ann" . t))))
      (should (equal (niwa-collaboration-presence-test--names (funcall frame 'r3 1))
                     nil))
      ;; The lobby counts browsers per session and per artifact.
      (let ((lobby-frame (funcall frame 'lobby 1)))
        (should (equal (plist-get lobby-frame :people) []))
        (should (equal (plist-get lobby-frame :sessions)
                       [(:id "s1" :n 2) (:id "s2" :n 1)]))
        (should (equal (plist-get lobby-frame :artifacts)
                       [(:id "board" :n 2)])))
      ;; Session rooms carry no lobby counts, and guest ids never leave.
      (should-not (plist-member (funcall frame 'r1 1) :sessions))
      (should-not (string-search "browser" (format "%S" sent)))))

  :doc "sends a guest nothing when its frame has not changed"
  (niwa-collaboration-presence-test--with-rooms
    (niwa-collaboration-presence-publish r1)
    (setq sent nil)
    (niwa-collaboration-presence-publish lobby)
    (should-not sent))

  :doc "a stopped room's guests leave the pages of the rooms still live"
  (niwa-collaboration-presence-test--with-rooms
    (niwa-collaboration-presence-publish r1)
    (remhash (cl-find-if (lambda (key) (eq (gethash key niwa-collaboration--rooms) r1))
                         (hash-table-keys niwa-collaboration--rooms))
             niwa-collaboration--rooms)
    (setq sent nil)
    (niwa-collaboration-presence-publish r1)
    (should (equal (niwa-collaboration-presence-test--names
                    (niwa-collaboration-presence-test--frame sent 'r2 1))
                   nil))
    (should (equal (plist-get (niwa-collaboration-presence-test--frame sent 'lobby 1)
                              :sessions)
                   [(:id "s2" :n 1)])))

  :doc "lists an artifact room's consumers by name on their artifact, sending them nothing"
  (niwa-collaboration-presence-test--with-rooms
    (let* ((consumers (plist-put
                       (niwa-collaboration-presence-test--room
                        'artifact ws nil '(1 "laptop agent" "agent-0001" "board" t))
                       :room-kind 'artifact))
           (niwa-collaboration-artifact-room--rooms
            (let ((table (make-hash-table :test #'equal)))
              (puthash '("/a/" . "board") consumers table)
              table)))
      (niwa-collaboration-presence-publish consumers)
      ;; The board's page lists the consumer, marked remote, beside Cleo.
      (should (equal (niwa-collaboration-presence-test--names
                      (niwa-collaboration-presence-test--frame sent 'r1 3))
                     '(("Cleo" . :json-false) ("laptop agent (remote)" . t))))
      ;; The room page and other workspaces do not.
      (should (equal (niwa-collaboration-presence-test--names
                      (niwa-collaboration-presence-test--frame sent 'r1 1))
                     '(("Bob" . t))))
      (should-not (niwa-collaboration-presence-test--names
                   (niwa-collaboration-presence-test--frame sent 'r3 1)))
      (should (equal (plist-get (niwa-collaboration-presence-test--frame sent 'lobby 1)
                                :artifacts)
                     [(:id "board" :n 3)]))
      ;; Consumers hear no presence at all.
      (should-not (cl-find 'artifact sent :key #'car)))))

(niwa-deftest niwa-collaboration-presence-state
  (:doc "reads a reported page and visibility, refusing a page that is no artifact id")
  (progn
    (should (equal (niwa-collaboration-presence-state '(:page "board" :active t))
                   '(:viewing "board" :active t)))
    ;; Only t is visible: a JSON false or a missing flag is away.
    (should (equal (niwa-collaboration-presence-state '(:page nil :active :json-false))
                   '(:viewing nil :active nil)))
    (should (equal (niwa-collaboration-presence-state '(:t "hello"))
                   '(:viewing nil :active nil)))
    (should-error (niwa-collaboration-presence-state '(:page "a/b")))
    (should-error (niwa-collaboration-presence-state '(:page 7)))))

(niwa-deftest niwa-collaboration-presence-handle ()
  ,test
  (test)
  :doc "records a reported page and republishes only when it changed"
  (niwa-collaboration-presence-test--with-rooms
    (niwa-collaboration-presence-publish r1)
    (setq sent nil)
    (niwa-collaboration-presence-handle r1 2 '(:t "viewing" :page "board" :active t))
    (should (equal (niwa-collaboration-presence-test--names
                    (niwa-collaboration-presence-test--frame sent 'r2 1))
                   '(("Ann" . t) ("Bob" . t))))
    (should (equal (niwa-collaboration-presence-test--names
                    (niwa-collaboration-presence-test--frame sent 'r1 1))
                   nil))
    (setq sent nil)
    (niwa-collaboration-presence-handle r1 2 '(:t "viewing" :page "board" :active t))
    (should-not sent))

  :doc "rejects a page that is not an artifact id and ignores unknown peers"
  (niwa-collaboration-presence-test--with-rooms
    (should-error (niwa-collaboration-presence-handle
                   r1 2 '(:t "viewing" :page "../etc" :active t)))
    (should-not (plist-get (gethash 2 (plist-get r1 :guests)) :viewing))
    (niwa-collaboration-presence-handle r1 9 '(:t "viewing" :page nil :active t))
    (should-not sent)))

(provide 'test-niwa-collaboration-presence)
;;; test-niwa-collaboration-presence.el ends here
