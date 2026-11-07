;;; test-mevedel-migrate-instructions.el --- Instruction store migration tests -*- lexical-binding: t -*-

;;; Commentary:
;; Run the standalone migration's instruction step on closed sessions whose
;; snapshots diverge, and compare its reports semantically: as sets of
;; (UUID REASON) facts, not printed text.  Each staging case has a control
;; that writes the store once the discriminating difference is gone.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-session-test-support"))
(require 'mevedel-persistence)
;; Name the source: stale bytecode beside the script would otherwise shadow the
;; script under test.
(require 'mevedel-migrate-session
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           ".." "scripts" "migrate-sessions.el"))

(defun mevedel-migrate-instructions-test--attempt (session turn sequence &optional result)
  "Return a serialized attempt of SESSION's TURN with SEQUENCE and RESULT."
  (list :sequence sequence :action 'implement :directive-request "req"
        :request "req" :result (or result "done") :outcome 'success :patch ""
        :capture 'incomplete :covered-files nil :gaps nil :untracked-effects nil
        :captured-at "2026-11-07T00:00:00+0100"
        :checkpoint (list :session-id session :turn turn)
        :plan nil :plan-context nil :plan-selection nil
        :consumed-subdirectives nil))

(defun mevedel-migrate-instructions-test--directive (uuid request &rest keys)
  "Return serialized directive UUID with REQUEST; KEYS override fields."
  (let ((record (list :id uuid :request request
                      :anchor (list :state 'attached :file "source.el"
                                    :start 1 :end 6 :evidence nil)
                      :subdirectives nil :session-id nil :planning-enabled nil
                      :skills nil :plan nil :planning nil :attempts nil
                      :discussion nil)))
    (while keys
      (setq record (plist-put record (pop keys) (pop keys))))
    record))

(defun mevedel-migrate-instructions-test--presentation (uuid id &optional parent type)
  "Return the serialized overlay of instruction UUID with display ID.
PARENT names the enclosing instruction; TYPE defaults to `directive'."
  (list :overlay-start 1 :overlay-end 6
        :anchor (list :schema 1 :uuid uuid :parent-uuid parent :bodyless nil
                      :text-hash nil :text "(defu" :prefix "" :suffix "n beta")
        :properties (list 'mevedel-instruction t 'mevedel-id id 'mevedel-uuid uuid
                          'mevedel-instruction-type (or type 'directive))))

(defun mevedel-migrate-instructions-test--snapshot (directives presentations)
  "Return an instruction snapshot of DIRECTIVES and PRESENTATIONS."
  (let ((ids (mapcar (lambda (instr) (plist-get (plist-get instr :properties) 'mevedel-id))
                     presentations)))
    (list :version "0.0.0"
          :ids (list :id-counter (apply #'max 0 ids) :used-ids ids :retired-ids nil)
          :directives directives
          :files (and presentations
                      (list (cons "source.el" (list :anchor-schema 1
                                                    :instructions presentations)))))))

(defun mevedel-migrate-instructions-test--session (root id current &optional turns)
  "Create closed PID-lock session ID below ROOT with CURRENT instructions.
TURNS is an alist from turn numbers to turn snapshots."
  (let ((directory (file-name-concat root ".mevedel" "sessions" id)))
    (make-directory (file-name-concat directory "instructions") t)
    (mevedel-migrate-session--write
     (file-name-concat directory "session.meta.el")
     (test-mevedel-session-persistence--complete-sidecar
      (list :session-id id :session-name id :authority-mode 'pid-lock
            :workspace (list :type 'file :workspace-id (make-string 64 ?a)
                             :target-native-root "/tmp/" :name "w"))))
    (when current
      (mevedel-migrate-session--write
       (file-name-concat directory "instructions" "current.el") current))
    (dolist (turn turns)
      (mevedel-migrate-session--write
       (file-name-concat directory "instructions" (format "turn-%06d.el" (car turn)))
       (cdr turn)))
    directory))

(defun mevedel-migrate-instructions-test--portable-session (root id current stale)
  "Create closed portable session ID below ROOT publishing CURRENT instructions.
Its fixed `instructions/current.el' cache holds STALE, which is no authority."
  (let* ((directory (file-name-concat root ".mevedel" "sessions" id))
         (generation ".publications/generation-cccccccccccccccccccc")
         (sidecar (concat generation "/000001.data"))
         (instructions (concat generation "/000002.data"))
         (head (concat generation "/manifest.el")))
    (make-directory (file-name-concat directory generation) t)
    (make-directory (file-name-concat directory ".lease") t)
    (make-directory (file-name-concat directory "instructions") t)
    (mevedel-migrate-session--write
     (file-name-concat directory sidecar)
     (test-mevedel-session-persistence--complete-sidecar
      (list :session-id id :session-name id :authority-mode 'portable)))
    (mevedel-migrate-session--write (file-name-concat directory instructions) current)
    (mevedel-migrate-session--write
     (file-name-concat directory "instructions" "current.el") stale)
    (mevedel-migrate-session--write
     (file-name-concat directory head)
     (list :sidecar "session.meta.el"
           :artifacts (list (list "session.meta.el" :published sidecar
                                  :sha256 (mevedel-migrate-session--hash
                                           (file-name-concat directory sidecar)))
                            (list "instructions/current.el" :published instructions
                                  :sha256 (mevedel-migrate-session--hash
                                           (file-name-concat directory instructions))))))
    (mevedel-migrate-session--write
     (file-name-concat directory ".lease" "00000000000000000001.el")
     (list :generation 1 :transfer-generation 1 :status 'released
           :publication-head head :unsettled-mutation nil
           :client-id (make-string 64 ?a) :renewed-at 1 :expires-at 2))
    directory))

(defun mevedel-migrate-instructions-test--facts (rows)
  "Return report ROWS as a sorted list of (UUID REASON) facts."
  (sort (delete-dups (mapcar (lambda (row) (list (car row) (cadr row))) rows))
        (lambda (a b) (string< (format "%S" a) (format "%S" b)))))

(defun mevedel-migrate-instructions-test--tree (directory)
  "Return DIRECTORY's files with their bytes, to prove they did not change."
  (mapcar (lambda (file)
            (cons file (with-temp-buffer
                         (insert-file-contents-literally file)
                         (buffer-string))))
          (directory-files-recursively directory ".")))

(defmacro mevedel-migrate-instructions-test--with-root (vars &rest body)
  "Run BODY with VARS (ROOT WORKSPACE) for a fresh project, deleted after."
  (declare (indent 1))
  `(let* ((,(nth 0 vars) (file-name-as-directory
                          (make-temp-file "mevedel-migrate-instructions-" t)))
          (,(nth 1 vars) (mevedel-workspace--create :type 'project :id "w"
                                                    :root ,(nth 0 vars))))
     (unwind-protect (progn ,@body)
       (delete-directory ,(nth 0 vars) t))))

(defun mevedel-migrate-instructions-test--store (workspace)
  "Return WORKSPACE's store record read through the runtime decoder."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally (mevedel-instruction-store-path workspace))
    (let ((decoded (mevedel-instruction-store--decode (buffer-string) workspace)))
      (should (eq 'ok (plist-get decoded :status)))
      (plist-get decoded :data))))

(mevedel-deftest mevedel-migrate-instructions ()
  ,test
  (test)
  :doc "divergent sessions merge; display-id collisions are renumbered and reported"
  (mevedel-migrate-instructions-test--with-root (root workspace)
    (let ((a (mevedel-migrate-instructions-test--session
              root "a" (mevedel-migrate-instructions-test--snapshot
                        (list (mevedel-migrate-instructions-test--directive "D1" "one"))
                        (list (mevedel-migrate-instructions-test--presentation "D1" 1)))))
          (b (mevedel-migrate-instructions-test--session
              root "b" (mevedel-migrate-instructions-test--snapshot
                        (list (mevedel-migrate-instructions-test--directive "D2" "two"))
                        (list (mevedel-migrate-instructions-test--presentation "D2" 1))))))
      (let ((result (mevedel-migrate-instructions (list a b))))
        (should (eq 'written (plist-get result :status)))
        (should (equal '(("D1" :absent-from) ("D2" :absent-from))
                       (mevedel-migrate-instructions-test--facts
                        (plist-get result :ambiguous))))
        (should (equal '(("D2" :renumbered))
                       (mevedel-migrate-instructions-test--facts
                        (plist-get result :renumbered)))))
      (let* ((record (mevedel-migrate-instructions-test--store workspace))
             (ids (mapcar (lambda (instr) (plist-get (plist-get instr :properties) 'mevedel-id))
                          (plist-get (cdar (plist-get record :files)) :instructions))))
        (should (equal '("one" "two")
                       (sort (mapcar (lambda (d) (plist-get d :request))
                                     (plist-get record :directives))
                             #'string<)))
        (should (equal '(1 2) (sort ids #'<))))))
  :doc "control: an identical instruction in two sessions collapses to one"
  (mevedel-migrate-instructions-test--with-root (root workspace)
    (let* ((snapshot (mevedel-migrate-instructions-test--snapshot
                      (list (mevedel-migrate-instructions-test--directive "D1" "one"))
                      (list (mevedel-migrate-instructions-test--presentation "D1" 1))))
           (result (mevedel-migrate-instructions
                    (list (mevedel-migrate-instructions-test--session root "a" snapshot)
                          (mevedel-migrate-instructions-test--session root "b" snapshot)))))
      (should (eq 'written (plist-get result :status)))
      (should-not (plist-get result :ambiguous))
      (should-not (plist-get result :renumbered))
      (should (= 1 (length (plist-get (mevedel-migrate-instructions-test--store workspace)
                                      :directives))))))
  :doc "the same UUID at differing revisions stages the workspace, originals intact"
  (mevedel-migrate-instructions-test--with-root (root workspace)
    (let* ((a (mevedel-migrate-instructions-test--session
               root "a" (mevedel-migrate-instructions-test--snapshot
                         (list (mevedel-migrate-instructions-test--directive "D1" "older"))
                         (list (mevedel-migrate-instructions-test--presentation "D1" 1)))))
           (b (mevedel-migrate-instructions-test--session
               root "b" (mevedel-migrate-instructions-test--snapshot
                         (list (mevedel-migrate-instructions-test--directive "D1" "newer"))
                         (list (mevedel-migrate-instructions-test--presentation "D1" 1)))))
           (sessions (file-name-concat root ".mevedel" "sessions"))
           (before (mevedel-migrate-instructions-test--tree sessions))
           (result (mevedel-migrate-instructions (list a b))))
      (should (eq 'staged (plist-get result :status)))
      (should-not (file-exists-p (mevedel-instruction-store-path workspace)))
      (should (equal before (mevedel-migrate-instructions-test--tree sessions)))
      (let ((report (mevedel-migrate-session--read (plist-get result :path))))
        (should (equal '(("D1" :authored-conflict))
                       (mevedel-migrate-instructions-test--facts
                        (plist-get report :unresolved))))
        ;; One complete candidate per session, each preferring its session.
        (should (equal '("older" "newer")
                       (mapcar (lambda (candidate)
                                 (plist-get (car (plist-get (mevedel-migrate-session--read
                                                             candidate)
                                                            :directives))
                                            :request))
                               (plist-get report :candidates)))))
      ;; The runtime then sees a pending workspace, not an empty one.
      (let ((runtime (mevedel-workspace--create :type 'project :id "w-runtime"
                                                :root root)))
        (should (eq 'pending (mevedel-instruction-store-state runtime)))
        ;; Loading a candidate resolves it into the store.
        (unwind-protect
            (with-temp-buffer
              (setq-local mevedel--workspace runtime)
              (mevedel-load-instructions
               (car (plist-get (mevedel-migrate-session--read (plist-get result :path))
                               :candidates)))
              (should (eq 'active (mevedel-instruction-store-state runtime)))
              (should (equal '("older")
                             (mapcar (lambda (d) (plist-get d :request))
                                     (plist-get (mevedel-migrate-instructions-test--store
                                                 runtime)
                                                :directives)))))
          (mevedel--clear-instruction-state runtime)
          (test-mevedel-session-persistence--reset-instructions)))))
  :doc "inconsistent and cyclic trees stage the workspace"
  (mevedel-migrate-instructions-test--with-root (root _workspace)
    (let ((result (mevedel-migrate-instructions
                   (list (mevedel-migrate-instructions-test--session
                          root "a" (mevedel-migrate-instructions-test--snapshot
                                    nil
                                    (list (mevedel-migrate-instructions-test--presentation
                                           "R1" 1 "gone" 'reference)
                                          (mevedel-migrate-instructions-test--presentation
                                           "R2" 2 "R3" 'reference)
                                          (mevedel-migrate-instructions-test--presentation
                                           "R3" 3 "R2" 'reference))))))))
      (should (eq 'staged (plist-get result :status)))
      (should (equal '(("R1" :missing-parent) ("R2" :cycle) ("R3" :cycle))
                     (mevedel-migrate-instructions-test--facts
                      (plist-get (mevedel-migrate-session--read (plist-get result :path))
                                 :unresolved))))))
  :doc "conflicting attempts with the same sequence stage the workspace"
  (mevedel-migrate-instructions-test--with-root (root _workspace)
    (let* ((attempt (mevedel-migrate-instructions-test--attempt "a" 1 1 "first"))
           (other (mevedel-migrate-instructions-test--attempt "a" 1 1 "different"))
           (with (lambda (attempts)
                   (mevedel-migrate-instructions-test--snapshot
                    (list (mevedel-migrate-instructions-test--directive
                           "D1" "one" :session-id "a" :attempts attempts))
                    (list (mevedel-migrate-instructions-test--presentation "D1" 1)))))
           (result (mevedel-migrate-instructions
                    (list (mevedel-migrate-instructions-test--session
                           root "a" (funcall with (list attempt))
                           (list (cons 1 (funcall with (list attempt)))))
                          (mevedel-migrate-instructions-test--session
                           root "b" (funcall with (list other)))))))
      (should (eq 'staged (plist-get result :status)))
      (should (member '("D1" :activity-conflict)
                      (mevedel-migrate-instructions-test--facts
                       (plist-get (mevedel-migrate-session--read (plist-get result :path))
                                  :unresolved))))))
  :doc "an attempt whose turn a Rewind discarded lacks evidence and stages"
  (mevedel-migrate-instructions-test--with-root (root _workspace)
    (let* ((kept (mevedel-migrate-instructions-test--attempt "a" 1 1))
           (discarded (mevedel-migrate-instructions-test--attempt "a" 2 2))
           (snapshot (lambda (attempts)
                       (mevedel-migrate-instructions-test--snapshot
                        (list (mevedel-migrate-instructions-test--directive
                               "D1" "one" :session-id "a" :attempts attempts))
                        (list (mevedel-migrate-instructions-test--presentation "D1" 1)))))
           ;; A stale current snapshot still holds turn 2's attempt, but
           ;; Rewind deleted turn 2's snapshot.
           (result (mevedel-migrate-instructions
                    (list (mevedel-migrate-instructions-test--session
                           root "a" (funcall snapshot (list kept discarded))
                           (list (cons 1 (funcall snapshot (list kept)))))))))
      (should (eq 'staged (plist-get result :status)))
      (should (equal '(("D1" :missing-evidence))
                     (mevedel-migrate-instructions-test--facts
                      (plist-get (mevedel-migrate-session--read (plist-get result :path))
                                 :unresolved))))))
  :doc "control: an attempt its turn snapshot holds is kept and the store written"
  (mevedel-migrate-instructions-test--with-root (root workspace)
    (let* ((kept (mevedel-migrate-instructions-test--attempt "a" 1 1))
           (snapshot (mevedel-migrate-instructions-test--snapshot
                      (list (mevedel-migrate-instructions-test--directive
                             "D1" "one" :session-id "a" :attempts (list kept)))
                      (list (mevedel-migrate-instructions-test--presentation "D1" 1))))
           (result (mevedel-migrate-instructions
                    (list (mevedel-migrate-instructions-test--session
                           root "a" snapshot (list (cons 1 snapshot)))))))
      (should (eq 'written (plist-get result :status)))
      (should (= 1 (length (plist-get (car (plist-get (mevedel-migrate-instructions-test--store
                                                       workspace)
                                                      :directives))
                                      :attempts))))))
  :doc "an instruction missing from a later session is kept and reported"
  (mevedel-migrate-instructions-test--with-root (root workspace)
    (let ((result (mevedel-migrate-instructions
                   (list (mevedel-migrate-instructions-test--session
                          root "a" (mevedel-migrate-instructions-test--snapshot
                                    (list (mevedel-migrate-instructions-test--directive
                                           "D1" "one"))
                                    (list (mevedel-migrate-instructions-test--presentation
                                           "D1" 1))))
                         (mevedel-migrate-instructions-test--session
                          root "b" (mevedel-migrate-instructions-test--snapshot nil nil))))))
      (should (eq 'written (plist-get result :status)))
      (should (equal '(("D1" :absent-from))
                     (mevedel-migrate-instructions-test--facts
                      (plist-get result :ambiguous))))
      (should (equal '("one") (mapcar (lambda (d) (plist-get d :request))
                                      (plist-get (mevedel-migrate-instructions-test--store
                                                  workspace)
                                                 :directives))))))
  :doc "an existing store is current and the sessions are not read"
  (mevedel-migrate-instructions-test--with-root (root workspace)
    (let ((a (mevedel-migrate-instructions-test--session
              root "a" (mevedel-migrate-instructions-test--snapshot
                        (list (mevedel-migrate-instructions-test--directive "D1" "x"))
                        nil))))
      (make-directory (file-name-directory (mevedel-instruction-store-path workspace)) t)
      (write-region "kept" nil (mevedel-instruction-store-path workspace) nil 'silent)
      (should (eq 'current (plist-get (mevedel-migrate-instructions (list a)) :status)))
      (should (equal "kept" (with-temp-buffer
                              (insert-file-contents
                               (mevedel-instruction-store-path workspace))
                              (buffer-string))))))
  :doc "a portable session's published snapshot is read, not its fixed cache"
  (mevedel-migrate-instructions-test--with-root (root workspace)
    (let ((result
           (mevedel-migrate-instructions
            (list (mevedel-migrate-instructions-test--portable-session
                   root "p"
                   (mevedel-migrate-instructions-test--snapshot
                    (list (mevedel-migrate-instructions-test--directive "D1" "published"))
                    nil)
                   (mevedel-migrate-instructions-test--snapshot
                    (list (mevedel-migrate-instructions-test--directive "D1" "stale cache"))
                    nil))))))
      (should (eq 'written (plist-get result :status)))
      (should (equal '("published")
                     (mapcar (lambda (d) (plist-get d :request))
                             (plist-get (mevedel-migrate-instructions-test--store workspace)
                                        :directives)))))))

(mevedel-deftest mevedel-migrate-session-main/instructions ()
  ,test
  (test)
  :doc "a staged project run reports the report path and exits nonzero"
  (mevedel-migrate-instructions-test--with-root (root _workspace)
    (mevedel-migrate-instructions-test--session
     root "a" (mevedel-migrate-instructions-test--snapshot
               (list (mevedel-migrate-instructions-test--directive "D1" "older")) nil))
    (mevedel-migrate-instructions-test--session
     root "b" (mevedel-migrate-instructions-test--snapshot
               (list (mevedel-migrate-instructions-test--directive "D1" "newer")) nil))
    (let* ((command-line-args-left (list "--" root))
           (output (with-output-to-string
                     (should-error (mevedel-migrate-session-main)))))
      (should (string-match-p "instructions: STAGED (1 unresolved" output))))
  :doc "control: a run without conflicts writes the store and exits normally"
  (mevedel-migrate-instructions-test--with-root (root workspace)
    (mevedel-migrate-instructions-test--session
     root "a" (mevedel-migrate-instructions-test--snapshot
               (list (mevedel-migrate-instructions-test--directive "D1" "one")) nil))
    (let* ((command-line-args-left (list "--" root))
           (output (with-output-to-string
                     (condition-case err (mevedel-migrate-session-main)
                       (error (princ (format "SIGNALLED %S" err)))))))
      (should-not (string-match-p "SIGNALLED" output))
      (should (string-match-p "instructions: WRITTEN" output))
      (should (file-exists-p (mevedel-instruction-store-path workspace))))))

(provide 'test-mevedel-migrate-instructions)
;;; test-mevedel-migrate-instructions.el ends here
