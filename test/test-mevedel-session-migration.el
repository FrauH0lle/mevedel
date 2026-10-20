;;; test-mevedel-session-migration.el --- Explicit migration tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the standalone converter on real files, then use the normal reader.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-session-test-support"))
(require 'mevedel-plan-handoff)
;; Name the source: stale bytecode beside the script would otherwise shadow the
;; converter under test.
(require 'mevedel-migrate-session
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           ".." "scripts" "migrate-sessions.el"))

(defun mevedel-migration-test--stable-attributes (file)
  "Return FILE's attributes except access time, which reads may change."
  (let ((attributes (file-attributes file)))
    (setf (nth 4 attributes) nil)
    attributes))

(defmacro mevedel-migration-test--with-source (version &rest body)
  "Run BODY with a two-head VERSION session and a separate destination."
  (declare (indent 1) (debug t))
  `(let* ((root (make-temp-file "mevedel-migration-" t))
          (source (file-name-concat root "old"))
          (destination (file-name-concat root "converted"))
          (session (test-mevedel-session-persistence--make-session root))
          (workspace (mevedel-session-workspace session))
          (old (cl-loop for (key value) on (mevedel-session-codec-serialize session) by #'cddr
                        ;; Strip exactly the fields VERSION predates.
                        unless (or (and (not (equal ,version "v0.5.13"))
                                        (eq key :chapters))
                                   (and (not (member ,version '("v0.5.12" "v0.5.13")))
                                        (eq key :naming-input))
                                   (and (member ,version '("v0.5.6" "v0.5.9"))
                                        (memq key '(:recovery-issues :pending-follow-ups :pending-steering
                                                                     :pending-input-next-id :pending-input-paused
                                                                     :pending-input-failure-paused)))
                                   (and (member ,version '("v0.5.6" "v0.5.9" "v0.5.10"))
                                        (memq key '(:attached-artifacts :dedicated-artifact)))
                                   (and (equal ,version "v0.5.6") (eq key :external-conversations)))
                        append (list key value)))
          (head ".publications/generation-bbbbbbbbbbbbbbbbbbbb/manifest.el")
          (lease-file (file-name-concat source ".lease/00000000000000000001.el")))
     (unwind-protect
         (progn
           (setq old (plist-put old :version ,version))
           ;; The native writer shares equal object identities with #N=/#N#.
           (setq old (plist-put old :first-user-message "Shared preview"))
           (setq old (plist-put old :latest-user-message (plist-get old :first-user-message)))
           (make-directory (file-name-concat source ".lease") t)
           (mevedel-migrate-session--write (file-name-concat source "session.meta.el") old)
           (dolist (name '("aaaaaaaaaaaaaaaaaaaa" "bbbbbbbbbbbbbbbbbbbb"))
             (let* ((directory (concat ".publications/generation-" name))
                    (meta (concat directory "/000001.data"))
                    (chat (concat directory "/000002.data")))
               (make-directory (file-name-concat source directory) t)
               (mevedel-migrate-session--write (file-name-concat source meta) old)
               (with-temp-file (file-name-concat source chat)
                 (insert "#+title: Kept transcript\nUser and assistant text.\n"))
               (mevedel-migrate-session--write
                (file-name-concat source directory "manifest.el")
                (list :sidecar "session.meta.el" :artifacts
                      (list (list "session.meta.el" :published meta :sha256
                                  (mevedel-migrate-session--hash (file-name-concat source meta)))
                            (list "segment-0001.chat.org" :published chat :sha256
                                  (mevedel-migrate-session--hash (file-name-concat source chat))))))))
           (mevedel-migrate-session--write
            lease-file (list :generation 1 :transfer-generation 1 :status 'released
                             :publication-head head :unsettled-mutation nil
                             :client-id (make-string 64 ?a) :renewed-at 1 :expires-at 2))
           ,@body)
       (delete-directory root t)
       (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-migrate-session--sidecar ()
  ,test
  (test)
  :doc "v0.5.6 keeps Goal accounting and gains the current defaults"
  (let* ((data (test-mevedel-session-persistence--complete-sidecar nil))
         (goal '(:id "g" :objective "Keep the goal" :status active :reason nil
                     :implementation-policy nil
                     :token-budget 100 :tokens-used 20 :time-used-seconds 1
                     :turns-run 1 :plan-reference nil :created-at "created" :updated-at "updated")))
    (dolist (key '(:external-conversations :recovery-issues :pending-follow-ups :pending-steering
                                           :pending-input-next-id :pending-input-paused :pending-input-failure-paused
                                           :attached-artifacts :dedicated-artifact :naming-input
                                           :chapters))
      (cl-remf data key))
    (setq data (plist-put (plist-put data :version "v0.5.6") :goal goal))
    (let ((restored (mevedel-session-codec--goal-from-plist
                     (plist-get (mevedel-migrate-session--sidecar data) :goal))))
      (should (= 20 (mevedel-goal-tokens-used restored)))
      (should-not (mevedel-goal-implementation-policy restored))
      (should-not (mevedel-goal-tokens-incomplete-p restored)))
    (should-not (plist-member goal :tokens-incomplete-p)))

  :doc "v0.5.9 retains native history and initializes only newly durable fields"
  (mevedel-migration-test--with-source "v0.5.9"
    (let* ((history (list (list "root" :engine 'claude-code :id "retained"
                                :host "fixture" :directory root :state 'in-flight
                                :input-boundary '(1 . 12))))
           (data (plist-put (copy-tree old) :external-conversations history))
           (converted (mevedel-migrate-session--sidecar data))
           (loaded (plist-get (mevedel-session-codec-deserialize converted workspace) :session)))
      (should (equal mevedel-session-codec-format-version (plist-get converted :version)))
      (should (plist-member converted :naming-input))
      (should-not (plist-get converted :naming-input))
      (should (equal history (plist-get converted :external-conversations)))
      (should (equal "v0.5.9" (plist-get data :version)))
      (should-not (plist-member data :pending-follow-ups))
      (dolist (key '(:recovery-issues :pending-follow-ups :pending-steering
                                      :pending-input-paused :pending-input-failure-paused))
        (should (plist-member converted key))
        (should-not (plist-get converted key)))
      (should (= 0 (plist-get converted :pending-input-next-id)))
      ;; The ordinary reader keeps an interrupted native turn paused.
      (should (mevedel-session-pending-input-failure-paused loaded))
      (should (eq 'uncertain (plist-get (cdar (mevedel-session-external-conversations loaded)) :state)))))

  :doc "v0.5.9 Goals gain incomplete-usage tracking and invalid Goals fail loudly"
  (mevedel-migration-test--with-source "v0.5.9"
    (let* ((goal '(:id "g" :objective "Keep the goal" :status active :reason nil
                       :implementation-policy
                       (:model-provider "Unavailable:implementer"
                        :reasoning-effort high :permission-mode edits)
                       :token-budget 100 :tokens-used 20 :time-used-seconds 1
                       :turns-run 1 :plan-reference nil :created-at "created"
                       :updated-at "updated"))
           (converted (mevedel-migrate-session--sidecar
                       (plist-put (copy-tree old) :goal (copy-tree goal))))
           (loaded (plist-get (mevedel-session-codec-deserialize converted workspace)
                              :session)))
      (should (= 20 (mevedel-goal-tokens-used (mevedel-session-goal loaded))))
      (should (equal (plist-get goal :implementation-policy)
                     (mevedel-goal-implementation-policy
                      (mevedel-session-goal loaded))))
      (should-error (mevedel-migrate-session--sidecar
                     (plist-put (copy-tree old) :goal
                                (plist-put (copy-tree goal) :status 'unknown))))
      (let ((missing-policy (copy-tree goal)))
        (cl-remf missing-policy :implementation-policy)
        (should-error
         (mevedel-migrate-session--sidecar
          (plist-put (copy-tree old) :goal missing-policy))))))

  :doc "v0.5.10 keeps its fields and starts with no attached artifacts"
  (let ((data (test-mevedel-session-persistence--complete-sidecar
               '(:version "v0.5.10" :pending-input-next-id 4))))
    (cl-remf data :attached-artifacts)
    (cl-remf data :dedicated-artifact)
    (cl-remf data :naming-input)
    (cl-remf data :chapters)
    (let ((converted (mevedel-migrate-session--sidecar data)))
      (should (equal mevedel-session-codec-format-version (plist-get converted :version)))
      (should (= 4 (plist-get converted :pending-input-next-id)))
      (should (plist-member converted :attached-artifacts))
      (should-not (plist-get converted :attached-artifacts))
      (should (plist-member converted :dedicated-artifact))
      (should-not (plist-get converted :dedicated-artifact))
      (should-error (mevedel-migrate-session--sidecar
                     (plist-put (copy-tree data) :attached-artifacts '("x"))))))

  :doc "v0.5.11 keeps titles, attachments and recovery and gains no recorded naming input"
  (dolist (state '(pending attempted explicit))
    (let ((data (test-mevedel-session-persistence--complete-sidecar
                 (list :version "v0.5.11" :session-name "Kept title" :naming-state state
                       :attached-artifacts '("artifact-1") :pending-input-next-id 2
                       :pending-follow-ups '((:id 1 :category follow-up :input "Queued"))
                       :recovery-issues '((:id "session-naming" :category "request"
                                               :message "Naming failed" :blocking nil))))))
      (cl-remf data :naming-input)
      (cl-remf data :chapters)
      (let ((converted (mevedel-migrate-session--sidecar data)))
        (should (equal mevedel-session-codec-format-version (plist-get converted :version)))
        (should (plist-member converted :naming-input))
        (should-not (plist-get converted :naming-input))
        (cl-loop for (key value) on data by #'cddr unless (eq key :version) do
                 (should (equal value (plist-get converted key)))))
      ;; Legacy artifacts must not be dropped silently from a store-aware session.
      (should-error (mevedel-migrate-session--sidecar (copy-tree data) '("moved")))
      ;; A field the source version never had is refused, not overwritten.
      (should-error (mevedel-migrate-session--sidecar
                     (plist-put (copy-tree data) :naming-input "Unexpected")))))

  :doc "v0.5.12 keeps its fields, including naming input, and gains no chat history"
  (let ((data (test-mevedel-session-persistence--complete-sidecar
               (list :version "v0.5.12" :session-name "Kept title"
                     :naming-state 'attempted :naming-input "Fix this"))))
    (cl-remf data :chapters)
    (let ((converted (mevedel-migrate-session--sidecar data)))
      (should (equal mevedel-session-codec-format-version (plist-get converted :version)))
      (should (plist-member converted :chapters))
      (should-not (plist-get converted :chapters))
      (cl-loop for (key value) on data by #'cddr unless (eq key :version) do
               (should (equal value (plist-get converted key)))))
    ;; A field the source version never had is refused, not overwritten.
    (should-error (mevedel-migrate-session--sidecar
                   (plist-put (copy-tree data) :chapters nil))))

  :doc "current sidecars retain queued input, recovery state and naming input unchanged"
  (let ((data (test-mevedel-session-persistence--complete-sidecar nil)))
    (setq data (plist-put data :pending-follow-ups '((:id 3 :category follow-up :input "Keep this")))
          data (plist-put data :pending-input-next-id 3)
          data (plist-put data :pending-input-paused t)
          data (plist-put data :pending-input-failure-paused t)
          data (plist-put data :recovery-issues '((:id "model" :category "model" :message "Choose a model" :blocking t))))
    (should (equal data (mevedel-migrate-session--sidecar data)))
    (setq data (plist-put (plist-put data :naming-state 'attempted) :naming-input "Fix this"))
    (should (equal data (mevedel-migrate-session--sidecar data)))
    (should-error (mevedel-migrate-session--sidecar data '("moved"))))

  :doc "unprepared Worktree summaries migrate to fork-first preparation"
  (let* ((retry '(:step prepare-summary
                 :selection (:location worktree :context summary :execution direct
                             :mode edits :branch "plan/topic" :model-provider "Unavailable:model")
                 :accepted (:path "local/plans/accepted.md" :hash "abc")
                 :failure "Summary failed"))
         (data (test-mevedel-session-persistence--complete-sidecar
                (list :plan-metadata
                      (list :status 'accepted :implementation-retry retry))))
         (converted (mevedel-migrate-session--sidecar data))
         (actual (plist-get (plist-get converted :plan-metadata) :implementation-retry)))
    (should (eq 'prepare-worktree (plist-get actual :step)))
    (should (equal (plist-get actual :selection)
                   (mevedel-plan-handoff--validate-record actual)))
    (should (equal (plist-put (copy-tree retry) :step 'prepare-worktree) actual))
    (should (eq 'prepare-summary (plist-get retry :step)))
    (should (equal converted (mevedel-migrate-session--sidecar converted))))

  :doc "prepared legacy Worktree retries refuse conversion instead of routing to the source"
  (dolist (field '(:target-directory :target-session-id :target-accepted :target-save-path :summary))
    (let* ((retry (list :step 'prepare-worktree
                        :selection '(:location worktree :context fresh)
                        field "retained target"))
           (data (test-mevedel-session-persistence--complete-sidecar
                  (list :plan-metadata (list :implementation-retry retry)))))
      (should-error (mevedel-migrate-session--sidecar data))))
  (dolist (step '(prepare-target submit))
    (let ((data (test-mevedel-session-persistence--complete-sidecar
                 (list :plan-metadata
                       (list :implementation-retry
                             (list :step step :selection '(:location worktree :context fresh)))))))
      (should-error (mevedel-migrate-session--sidecar data))))

  :doc "cached legacy Worktree summaries cannot prove whether compaction already ran"
  (let ((data (test-mevedel-session-persistence--complete-sidecar
               '(:plan-metadata
                 (:implementation-retry
                  (:step prepare-summary :summary "Kept handoff"
                   :selection (:location worktree :context summary)))))))
    (should-error (mevedel-migrate-session--sidecar data)))

  :doc "Here retries and current fork-first Worktree retries remain unchanged"
  (dolist (retry '((:step prepare-summary :selection (:location here :context summary))
                   (:step submit :selection (:location here :context current))
                   (:step prepare-worktree :selection (:location worktree :context current)
                    :fork-point-id "reply" :worktree-reservation
                    (:branch "plan/topic" :directory "/tmp/topic/")
                    :target-save-path "/tmp/fork/")))
    (let ((data (test-mevedel-session-persistence--complete-sidecar
                 (list :plan-metadata (list :implementation-retry retry)))))
      (should (equal data (mevedel-migrate-session--sidecar data)))))

  :doc "legacy recovery fields are rejected instead of silently overwriting work"
  (mevedel-migration-test--with-source "v0.5.9"
    (dolist (key '(:recovery-issues :pending-follow-ups :pending-steering
                                    :pending-input-next-id :pending-input-paused :pending-input-failure-paused))
      (should-error (mevedel-migrate-session--sidecar (plist-put (copy-tree old) key nil))))))

(mevedel-deftest mevedel-migrate-session-copy (:quiet t)
  ,test
  (test)
  :doc "converts all retained heads, preserves bytes and loads through the current reader"
  (dolist (version '("v0.5.6" "v0.5.9" "v0.5.10" "v0.5.11" "v0.5.12"))
    (mevedel-migration-test--with-source version
      (let ((before (mapcar (lambda (file) (cons file (mevedel-migrate-session--hash file)))
                            (directory-files-recursively source "."))))
        (should (= 3 (mevedel-migrate-session-copy source destination)))
        (dolist (row before)
          (should (equal (cdr row) (mevedel-migrate-session--hash (car row)))))
        (dolist (name '("aaaaaaaaaaaaaaaaaaaa" "bbbbbbbbbbbbbbbbbbbb"))
          (let* ((publication (mevedel-session-publication-read
                               destination (concat ".publications/generation-" name "/manifest.el")))
                 (data (mevedel-session-codec-read (plist-get publication :sidecar)))
                 (loaded (plist-get (mevedel-session-codec-deserialize data workspace) :session)))
            (should (equal mevedel-session-codec-format-version (plist-get data :version)))
            (should (plist-member data :naming-input))
            (should-not (mevedel-session-naming-input loaded))
            (should (plist-member data :external-conversations))
            (should-not (mevedel-session-external-conversations loaded))
            (should (equal (mevedel-session-session-id session) (mevedel-session-session-id loaded)))
            (cl-loop for (key value) on old by #'cddr unless (eq key :version) do
                     (should (equal value (plist-get data key))))
            (let ((relative (concat ".publications/generation-" name "/000002.data")))
              (should (equal (mevedel-migrate-session--hash (file-name-concat source relative))
                             (mevedel-migrate-session--hash (file-name-concat destination relative)))))))
        ;; Runtime loading remains strict; only this explicit script accepts old data.
        (should-error (mevedel-session-codec-deserialize old workspace)))))

  :doc "converts unprepared Worktree retries in every head and refuses prepared ones without source writes"
  (dolist (prepared '(nil t))
    (mevedel-migration-test--with-source "v0.5.12"
      (setq old
            (plist-put old :plan-metadata
                       (list :status 'accepted :implementation-retry
                             (append '(:step prepare-summary
                                       :selection (:location worktree :context summary :execution direct
                                                   :mode edits :branch "plan/topic"
                                                   :model-provider "Unavailable:model")
                                       :accepted (:path "local/plans/accepted.md" :hash "abc"))
                                     (when prepared '(:target-directory "/tmp/legacy/"))))))
      (mevedel-migrate-session--write (file-name-concat source "session.meta.el") old)
      (dolist (name '("aaaaaaaaaaaaaaaaaaaa" "bbbbbbbbbbbbbbbbbbbb"))
        (let* ((directory (concat ".publications/generation-" name))
               (file (file-name-concat source directory "000001.data"))
               (manifest-file (file-name-concat source directory "manifest.el"))
               (manifest (mevedel-migrate-session--read manifest-file)))
          (mevedel-migrate-session--write file old)
          (setf (plist-get (cdar (plist-get manifest :artifacts)) :sha256)
                (mevedel-migrate-session--hash file))
          (mevedel-migrate-session--write manifest-file manifest)))
      (let ((before (mapcar (lambda (file) (cons file (mevedel-migrate-session--hash file)))
                            (directory-files-recursively source "."))))
        (if prepared
            (progn
              (should-error (mevedel-migrate-session-copy source destination))
              (should-not (file-exists-p destination)))
          (should (= 3 (mevedel-migrate-session-copy source destination)))
          (dolist (name '("aaaaaaaaaaaaaaaaaaaa" "bbbbbbbbbbbbbbbbbbbb"))
            (let* ((publication (mevedel-session-publication-read
                                 destination (concat ".publications/generation-" name "/manifest.el")))
                   (data (mevedel-session-codec-read (plist-get publication :sidecar)))
                   (loaded (plist-get (mevedel-session-codec-deserialize data workspace) :session)))
              (let ((retry (plist-get (mevedel-session-plan-metadata loaded) :implementation-retry)))
                (should (eq 'prepare-worktree (plist-get retry :step)))
                (should (equal (plist-get retry :selection)
                               (mevedel-plan-handoff--validate-record retry)))))))
        (dolist (row before)
          (should (equal (cdr row) (mevedel-migrate-session--hash (car row))))))))

  :doc "drops retained generations and fixed caches no reader accepts"
  (mevedel-migration-test--with-source "v0.5.9"
    (let* ((stale ".publications/generation-aaaaaaaaaaaaaaaaaaaa")
           (data (concat stale "/000001.data"))
           (manifest-file (file-name-concat source stale "manifest.el"))
           (manifest (mevedel-migrate-session--read manifest-file)))
      (mevedel-migrate-session--write (file-name-concat source data)
                                      (plist-put (copy-tree old) :version "v0.5.5"))
      (setf (plist-get (cdar (plist-get manifest :artifacts)) :sha256)
            (mevedel-migrate-session--hash (file-name-concat source data)))
      (mevedel-migrate-session--write manifest-file manifest)
      (mevedel-migrate-session--write (file-name-concat source "session.meta.el")
                                      (plist-put (copy-tree old) :version "v0.5.5"))
      (should (mevedel-migrate-session-copy source destination))
      (should-not (file-exists-p (file-name-concat destination stale "manifest.el")))
      (should (file-exists-p (file-name-concat destination data)))
      (should-not (file-exists-p (file-name-concat destination "session.meta.el")))
      (should (file-exists-p (file-name-concat destination head)))))

  :doc "takes an expired active lease, left by a crash, as closed"
  (mevedel-migration-test--with-source "v0.5.9"
    (mevedel-migrate-session--write
     lease-file (plist-put (mevedel-migrate-session--read lease-file) :status 'active))
    (should (mevedel-migrate-session-copy source destination)))

  :doc "refuses corrupt, unsupported, live and unsafe sources without changing them"
  (dolist (fault '(version checksum path active unsettled missing-lease lock recovery symlink))
    (mevedel-migration-test--with-source "v0.5.9"
      (pcase fault
        ;; The published head's sidecar; a stale fixed cache would be dropped.
        ('version
         (let* ((data ".publications/generation-bbbbbbbbbbbbbbbbbbbb/000001.data")
                (file (file-name-concat source head))
                (manifest (mevedel-migrate-session--read file)))
           (mevedel-migrate-session--write (file-name-concat source data)
                                           (plist-put (copy-tree old) :version "v0.5.0"))
           (setf (plist-get (cdar (plist-get manifest :artifacts)) :sha256)
                 (mevedel-migrate-session--hash (file-name-concat source data)))
           (mevedel-migrate-session--write file manifest)))
        ('checksum (with-temp-file (file-name-concat source ".publications/generation-aaaaaaaaaaaaaaaaaaaa/000002.data")
                     (insert "corrupt")))
        ('path (let* ((file (file-name-concat source head))
                      (manifest (mevedel-migrate-session--read file)))
                 (setf (plist-get (cdar (plist-get manifest :artifacts)) :published) "../../outside")
                 (mevedel-migrate-session--write file manifest)))
        ((or 'active 'unsettled)
         (let ((lease (mevedel-migrate-session--read lease-file)))
           ;; A live holder still renews its lease.
           (setq lease (if (eq fault 'active)
                           (plist-put (plist-put lease :status 'active)
                                      :expires-at (+ (float-time) 600))
                         (plist-put lease :unsettled-mutation t)))
           (mevedel-migrate-session--write lease-file lease)))
        ('missing-lease (delete-directory (file-name-concat source ".lease") t))
        ('lock (with-temp-file (file-name-concat source ".lock") (insert "held")))
        ('recovery (make-directory (file-name-concat source ".recovery"))
                   (with-temp-file (file-name-concat source ".recovery/pending") (insert "repair")))
        ('symlink (make-symbolic-link root (file-name-concat source "escape"))))
      (let ((before (mevedel-migrate-session--hash (file-name-concat source "session.meta.el"))))
        (should-error (mevedel-migrate-session-copy source destination))
        (should-not (file-exists-p destination))
        (should (equal before (mevedel-migrate-session--hash (file-name-concat source "session.meta.el")))))))

  :doc "converts an unlocked file-workspace session without a publication lease"
  (mevedel-migration-test--with-source "v0.5.9"
    (delete-directory (file-name-concat source ".lease") t)
    (delete-directory (file-name-concat source ".publications") t)
    (setq old (plist-put old :authority-mode 'pid-lock))
    (setf (plist-get (plist-get old :workspace) :type) 'file)
    (mevedel-migrate-session--write (file-name-concat source "session.meta.el") old)
    (should (= 1 (mevedel-migrate-session-copy source destination)))
    (should (equal mevedel-session-codec-format-version
                   (plist-get (mevedel-session-codec-read
                               (file-name-concat destination "session.meta.el")) :version))))

  :doc "never overwrites an existing destination or copies into itself"
  (mevedel-migration-test--with-source "v0.5.9"
    (make-directory destination)
    (should-error (mevedel-migrate-session-copy source destination))
    (should (file-directory-p destination))
    (should-error (mevedel-migrate-session-copy source (file-name-concat source "nested")))
    (should-not (file-exists-p (file-name-concat source "nested")))))

(mevedel-deftest mevedel-migrate-session--read ()
  (dolist (text '("#1=(:a #1#)" "(:a 1) (:b 2)" "(:a 1 :a 2)" "(setq migration-executed t)"))
    (let ((file (make-temp-file "mevedel-migration-read-")))
      (unwind-protect
          (progn
            (with-temp-file file (insert text))
            (should-error (mevedel-migrate-session--read file)))
        (delete-file file)))))

(mevedel-deftest mevedel-migrate-session--steps ()
  ,test
  (test)
  :doc "each step receives its declared source and the next step receives its output"
  (mevedel-migration-test--with-source "v0.5.6"
    (let* ((original mevedel-migrate-session--steps)
           seen
           (mevedel-migrate-session--steps
            (mapcar
             (lambda (step)
               (pcase-let ((`(,from ,to ,convert) step))
                 (list from to
                       (lambda (&rest args)
                         (should (equal from (plist-get (car args) :version)))
                         (push from seen)
                         (apply convert args)))))
             original)))
      (should (equal mevedel-session-codec-format-version
                     (plist-get (mevedel-migrate-session--sidecar old) :version)))
      (should (equal (mapcar #'car original) (nreverse seen)))))

  :doc "dropping the oldest chain prefix rejects retired sources and preserves later migrations"
  (let ((mevedel-migrate-session--steps (cdr mevedel-migrate-session--steps)))
    (mevedel-migration-test--with-source "v0.5.6"
      (should-error (mevedel-migrate-session--sidecar old)))
    (mevedel-migration-test--with-source "v0.5.9"
      (should (equal mevedel-session-codec-format-version
                     (plist-get (mevedel-migrate-session--sidecar old) :version)))))

  :doc "an implementation with a stale target fails instead of pretending to reach latest"
  (let ((mevedel-session-codec-format-version "v99.0.0"))
    (mevedel-migration-test--with-source "v0.5.9"
      (should-error (mevedel-migrate-session--sidecar old))))

  :doc "removing every old step leaves current-only validation"
  (let ((mevedel-migrate-session--steps nil))
    (mevedel-migration-test--with-source "v0.5.13"
      (should (equal old (mevedel-migrate-session--sidecar old))))
    (mevedel-migration-test--with-source "v0.5.12"
      (should-error (mevedel-migrate-session--sidecar old)))))

(mevedel-deftest mevedel-migrate-session-main (:quiet t)
  ,test
  (test)
  :doc "one directory argument migrates in place and reports the retained backup"
  (mevedel-migration-test--with-source "v0.5.9"
    (let ((command-line-args-left (list "--" source)))
      (let ((output (with-output-to-string (mevedel-migrate-session-main))))
        (should (string-search "old" output))
        (should (string-search "backup" (downcase output))))
      (should-not command-line-args-left)
      (should (equal mevedel-session-codec-format-version
                     (plist-get (mevedel-migrate-session--read
                                 (file-name-concat source "session.meta.el")) :version)))))

  :doc "help needs no directory and invalid argument lists fail"
  (let ((command-line-args-left '("--" "--help")))
    (should (string-search "DIRECTORY" (with-output-to-string (mevedel-migrate-session-main))))
    (should-not command-line-args-left))
  (dolist (args '(nil ("--") ("--" "one" "two") ("--unknown")))
    (let ((command-line-args-left args))
      (should-error (mevedel-migrate-session-main)))))

(mevedel-deftest mevedel-migrate-sessions (:quiet t)
  ,test
  (test)
  :doc "in-place migration preserves original bytes in a backup and reruns are a no-op"
  (mevedel-migration-test--with-source "v0.5.9"
    (let* ((before (mapcar (lambda (file)
                            (cons (file-relative-name file source)
                                  (mevedel-migrate-session--hash file)))
                          (directory-files-recursively source ".")))
           (row (cdar (mevedel-migrate-sessions source)))
           (backup (plist-get row :backup)))
      (should (eq 'migrated (plist-get row :status)))
      (should (file-directory-p backup))
      (should-not (file-in-directory-p backup source))
      (dolist (entry before)
        (should (equal (cdr entry)
                       (mevedel-migrate-session--hash (file-name-concat backup (car entry))))))
      (let ((files (directory-files root t))
            (attributes (mevedel-migration-test--stable-attributes (file-name-concat source head))))
        (should (eq 'current (plist-get (cdar (mevedel-migrate-sessions source)) :status)))
        (should (equal files (directory-files root t)))
        (should (equal attributes (mevedel-migration-test--stable-attributes
                                   (file-name-concat source head)))))
      (should (equal "v0.5.9"
                     (plist-get (mevedel-migrate-session--read
                                 (file-name-concat backup "session.meta.el")) :version)))))

  :doc "already-current metadata is not rewritten or backed up"
  (mevedel-migration-test--with-source "v0.5.13"
    (let ((before (directory-files root t))
          (attributes (mevedel-migration-test--stable-attributes
                       (file-name-concat source "session.meta.el"))))
      (should (eq 'current (plist-get (cdar (mevedel-migrate-sessions source)) :status)))
      (should (equal before (directory-files root t)))
      (should (equal attributes (mevedel-migration-test--stable-attributes
                                 (file-name-concat source "session.meta.el"))))))

  :doc "project migration continues after unsupported sessions and leaves them unchanged"
  (mevedel-migration-test--with-source "v0.5.9"
    (let* ((sessions (file-name-concat root ".mevedel" "sessions"))
           (good (file-name-concat sessions "good"))
           (bad (file-name-concat sessions "unsupported")))
      (make-directory sessions t)
      (rename-file source good)
      (make-directory bad)
      (mevedel-migrate-session--write (file-name-concat bad "session.meta.el")
                                      '(:version "v0.5.8"))
      (let* ((before (mevedel-migrate-session--hash (file-name-concat bad "session.meta.el")))
             (report (mevedel-migrate-sessions root)))
        (should (eq 'migrated (plist-get (cdr (assoc "good" report)) :status)))
        (should (eq 'failed (plist-get (cdr (assoc "unsupported" report)) :status)))
        (should (stringp (plist-get (cdr (assoc "unsupported" report)) :reason)))
        (should (equal before (mevedel-migrate-session--hash
                               (file-name-concat bad "session.meta.el")))))
      ;; Backups/staging must not become newly discoverable sessions.
      (should (equal '("good" "unsupported")
                     (mapcar #'car (mevedel-migrate-sessions root))))))

  :doc "a project with a live session is refused before any other session is changed"
  (mevedel-migration-test--with-source "v0.5.9"
    (let* ((sessions (file-name-concat root ".mevedel" "sessions"))
           (good (file-name-concat sessions "a-closed"))
           (live (file-name-concat sessions "z-live")))
      (make-directory sessions t)
      (rename-file source good)
      (make-directory live)
      (mevedel-migrate-session--write (file-name-concat live "session.meta.el") old)
      (with-temp-file (file-name-concat live ".lock") (insert "held"))
      (let ((before (mevedel-migrate-session--hash (file-name-concat good "session.meta.el"))))
        (should-error (mevedel-migrate-sessions root))
        (should (equal before (mevedel-migrate-session--hash
                               (file-name-concat good "session.meta.el")))))))

  :doc "installation failure restores the original instead of leaving its path missing"
  (mevedel-migration-test--with-source "v0.5.9"
    (let ((rename (symbol-function 'rename-file))
          (before (mevedel-migrate-session--hash (file-name-concat source "session.meta.el")))
          injected)
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (from to &optional overwrite)
                   (if (and (not injected)
                            (equal (directory-file-name to) (directory-file-name source)))
                       (progn (setq injected t) (error "Injected install failure"))
                     (funcall rename from to overwrite)))))
        (let ((row (cdar (mevedel-migrate-sessions source))))
          (should injected)
          (should (eq 'failed (plist-get row :status)))
          (should (string-search "Injected install failure" (plist-get row :reason)))))
      (should (equal before (mevedel-migrate-session--hash
                             (file-name-concat source "session.meta.el"))))))

  :doc "source changes during staging abort replacement"
  (mevedel-migration-test--with-source "v0.5.9"
    (let ((copy (symbol-function 'mevedel-migrate-session-copy))
          (chat (file-name-concat source ".publications/generation-bbbbbbbbbbbbbbbbbbbb/000002.data"))
          injected)
      (cl-letf (((symbol-function 'mevedel-migrate-session-copy)
                 (lambda (&rest args)
                   (prog1 (apply copy args)
                     (unless injected
                       (setq injected t)
                       (write-region "\nConcurrent edit" nil chat t 'silent))))))
        (let ((row (cdar (mevedel-migrate-sessions source))))
          (should injected)
          (should (eq 'failed (plist-get row :status)))))
      (should (equal "v0.5.9" (plist-get (mevedel-migrate-session--read
                                        (file-name-concat source "session.meta.el")) :version)))
      (should (string-suffix-p "Concurrent edit"
                               (with-temp-buffer (insert-file-contents chat) (buffer-string))))))

  :doc "project state links are refused rather than migrating their target"
  (mevedel-migration-test--with-source "v0.5.9"
    (let* ((state (file-name-concat root "elsewhere"))
           (sessions (file-name-concat state "sessions"))
           (actual (file-name-concat sessions "session")))
      (rename-file (file-name-concat root ".mevedel") state)
      (make-directory sessions t)
      (rename-file source actual)
      (make-symbolic-link state (file-name-concat root ".mevedel"))
      (let ((before (mevedel-migrate-session--hash (file-name-concat actual "session.meta.el"))))
        (should-error (mevedel-migrate-sessions root))
        (should (equal before (mevedel-migrate-session--hash
                               (file-name-concat actual "session.meta.el")))))))

  :doc "an existing migration lock blocks replacement and is not removed by the contender"
  (mevedel-migration-test--with-source "v0.5.9"
    (let ((lock (file-name-concat root ".migration-lock"))
          (before (mevedel-migrate-session--hash (file-name-concat source "session.meta.el"))))
      (make-directory lock)
      (should-error (mevedel-migrate-sessions source))
      (should (file-directory-p lock))
      (should (equal before (mevedel-migrate-session--hash
                             (file-name-concat source "session.meta.el"))))))

  :doc "nil authoritative metadata is invalid, not an unpublished empty session"
  (dolist (authority '(pid-lock portable))
    (mevedel-migration-test--with-source "v0.5.9"
      (if (eq authority 'pid-lock)
          (progn
            (delete-directory (file-name-concat source ".lease") t)
            (delete-directory (file-name-concat source ".publications") t)
            (mevedel-migrate-session--write (file-name-concat source "session.meta.el") nil))
        (let* ((file (file-name-concat source (file-name-directory head) "000001.data"))
               (manifest-file (file-name-concat source head))
               (manifest (mevedel-migrate-session--read manifest-file)))
          (mevedel-migrate-session--write file nil)
          (setf (plist-get (cdar (plist-get manifest :artifacts)) :sha256)
                (mevedel-migrate-session--hash file))
          (mevedel-migrate-session--write manifest-file manifest)))
      (let ((before (directory-files root t))
            (row (cdar (mevedel-migrate-sessions source))))
        (should (eq 'failed (plist-get row :status)))
        (should (stringp (plist-get row :reason)))
        (should (equal before (directory-files root t))))))

  :doc "unsupported, future and missing versions fail without a source rewrite"
  (dolist (version '("v0.5.5" "v0.5.7" "v0.5.8" "v99.0.0" nil))
    (mevedel-migration-test--with-source "v0.5.9"
      (delete-directory (file-name-concat source ".lease") t)
      (delete-directory (file-name-concat source ".publications") t)
      (mevedel-migrate-session--write (file-name-concat source "session.meta.el")
                                      (list :version version))
      (let ((before (directory-files root t))
            (digest (mevedel-migrate-session--hash (file-name-concat source "session.meta.el"))))
        (should (eq 'failed (plist-get (cdar (mevedel-migrate-sessions source)) :status)))
        (should (equal before (directory-files root t)))
        (should (equal digest (mevedel-migrate-session--hash
                               (file-name-concat source "session.meta.el"))))))))

(mevedel-deftest mevedel-migrate-session-cli (:quiet t)
  ,test
  (test)
  :doc "real batch invocation migrates, no-ops and returns nonzero for unsupported versions"
  (mevedel-migration-test--with-source "v0.5.9"
    (let ((script (file-name-concat (file-name-directory (locate-library "mevedel"))
                                   "scripts" "migrate-sessions.el")))
      (dolist (expected '(0 0 1))
        (when (= expected 1)
          (delete-directory (file-name-concat source ".lease") t)
          (delete-directory (file-name-concat source ".publications") t)
          (mevedel-migrate-session--write (file-name-concat source "session.meta.el")
                                          '(:version "v99.0.0")))
        (with-temp-buffer
          (let ((status (call-process
                         (expand-file-name invocation-name invocation-directory) nil t nil
                         "--batch" "-Q" "--eval"
                         (prin1-to-string `(setq load-path ',load-path debug-on-error nil))
                         "-l" script "-f" "mevedel-migrate-session-main" "--" source)))
            (if (zerop expected) (should (zerop status)) (should-not (zerop status)))
            (should (string-search "old" (buffer-string)))))))))

(provide 'test-mevedel-session-migration)
;;; test-mevedel-session-migration.el ends here
