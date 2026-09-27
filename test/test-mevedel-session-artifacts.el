;;; test-mevedel-session-artifacts.el --- Session artifact and segment tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `mevedel-session-artifacts'.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))

(require 'mevedel-journal-evidence)

(mevedel-deftest mevedel-session-artifacts--obsolete-snapshot-artifacts ()
  ,test
  (test)
  :doc "drops only unindexed file history and retains both sides of every turn"
  (let ((session (mevedel-session--create
                  :save-path "/session/"
                  :file-snapshots
                  '((1 ("file" :backup-name "post" :pre-backup-name "pre")))
                  :publication
                  '(:artifacts (("file-history/post") ("file-history/pre")
                                ("file-history/stale") ("artifacts/keep"))))))
    (should (equal '((:path "/session/file-history/stale" :delete t))
                   (mevedel-session-artifacts--obsolete-snapshot-artifacts session))))
  :doc "PID collection selects only unreferenced generated files and rejects a linked store"
  (let* ((root (make-temp-file "mevedel-history-collection-" t))
         (directory (file-name-concat root "file-history"))
         (kept (make-string 64 ?a)) (stale (make-string 64 ?b))
         (session (mevedel-session--create
                   :save-path root
                   :file-snapshots `((1 ("file" :pre-backup-name ,kept))))))
    (unwind-protect
        (progn
          (make-directory directory)
          (dolist (name (list kept stale "notes"))
            (write-region "bytes" nil (file-name-concat directory name) nil 'silent))
          (should (equal (list (list :path (file-name-concat directory stale) :delete t))
                         (mevedel-session-artifacts--obsolete-snapshot-artifacts session t)))
          (rename-file directory (file-name-concat root "outside"))
          (make-symbolic-link (file-name-concat root "outside") directory)
          (should-error (mevedel-session-artifacts--obsolete-snapshot-artifacts session t)))
      (delete-directory root t))))

(mevedel-deftest mevedel-session-artifacts--finalize-segment-file ()
  ,test
  (test)
  :doc "preserves archived roles and hides internal notes after disk restoration"
  (let ((file (make-temp-file "mevedel-finalize-" nil ".org"))
        before)
    (unwind-protect
        (progn
          (with-temp-buffer
            (mevedel--transcript-org-mode)
            (insert ":PROPERTIES:\n:END:\n\nComplete user prompt.\n")
            (insert (propertize "Complete assistant response.\n"
                                'gptel 'response))
            (insert (propertize "\n<system-reminder>\nINTERNAL-NOTE\n</system-reminder>\n"
                                'gptel 'ignore))
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (setq before (mevedel-agent-conversation-project-history
                          (current-buffer)))
            (should (string-match-p "provenance: assistant" before))
            (should-not (string-match-p "INTERNAL-NOTE" before))
            (write-region (point-min) (point-max) file nil 'silent))
          (mevedel-session-artifacts--finalize-segment-file file)
          (with-temp-buffer
            (insert-file-contents file)
            (mevedel--transcript-org-mode)
            (mevedel-transcript-restore-properties)
            (should (org-entry-get (point-min) "MEVEDEL_SEGMENT_FINALIZED_AT"))
            (should (equal before (mevedel-agent-conversation-project-history
                                   (current-buffer))))))
      (delete-file file)))
  :doc "matches portable finalization while preserving file encoding and newlines"
  (dolist (coding '(utf-8-unix utf-8-dos iso-latin-1-unix
                   utf-16le-with-signature-dos))
    (let ((file (make-temp-file "mevedel-finalize-coding-" nil ".org")))
      (unwind-protect
          (cl-letf (((symbol-function 'format-time-string)
                     (lambda (&rest _) "2026-01-01T00-00-00")))
            (with-temp-buffer
              (insert ":PROPERTIES:\n:END:\n\nUser caf\u00e9.\n")
              (let ((coding-system-for-write coding))
                (write-region (point-min) (point-max) file nil 'silent)))
            (let ((expected
                   (with-temp-buffer
                     (insert-file-contents file)
                     (should (eq coding buffer-file-coding-system))
                     (encode-coding-string
                      (mevedel-session-artifacts-finalized-segment-text
                       (buffer-string) buffer-file-coding-system)
                      buffer-file-coding-system))))
              (mevedel-session-artifacts--finalize-segment-file file)
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally file)
                (should (equal expected (buffer-string))))))
        (delete-file file)))))

(mevedel-deftest mevedel-session-artifacts-finalized-segment-text ()
  ,test
  (test)
  :doc "preserves plain and propertized portable archives after restoration"
  (let (text before)
    (with-temp-buffer
      (mevedel--transcript-org-mode)
      (insert ":PROPERTIES:\n:END:\n\nComplete user prompt.\n")
      (insert (propertize "Complete assistant response.\n" 'gptel 'response))
      (insert (propertize "\n<system-reminder>\nINTERNAL-NOTE\n</system-reminder>\n"
                          'gptel 'ignore))
      (mevedel-session-artifacts-stabilize-gptel-bounds)
      (setq text (buffer-string)
            before (mevedel-agent-conversation-project-history (current-buffer))))
    (should (string-match-p "provenance: assistant" before))
    (should-not (string-match-p "INTERNAL-NOTE" before))
    (dolist (source (list (substring-no-properties text) text))
      (with-temp-buffer
        ;; A portable publication serializes strings without text properties.
        (insert (substring-no-properties
                 (mevedel-session-artifacts-finalized-segment-text
                  source 'utf-8-unix)))
        (mevedel--transcript-org-mode)
        (mevedel-transcript-restore-properties)
        (should (org-entry-get (point-min) "MEVEDEL_SEGMENT_FINALIZED_AT"))
        (should (equal before (mevedel-agent-conversation-project-history
                              (current-buffer)))))))
  :doc "rebases existing continuations even when later prompts occupy the segment"
  (let* ((prompt (list :turn 1 :file-turn 1 :cum-turn 1 :pos 1
                       :preview "Earlier request"))
         (session (mevedel-session--create
                   :current-segment 2 :turn-count 2
                   :prompt-index (list (list 1 prompt))))
         (id (make-string 32 ?a))
         text old-index before)
    (with-temp-buffer
      (org-mode)
      (insert (mevedel-session-artifacts-summary-block "Earlier work"))
      (insert (propertize "Finished continued work\n" 'gptel 'response))
      (insert (mevedel--format-hook-audit-record
               (list :type 'fork-point :fork-point-id id :segment 2
                     :turn 1 :file-turn 1 :cum-turn 1 :captured-file-turn 1)))
      (insert "\nLater request\n")
      (insert (propertize "Later response\n" 'gptel 'response))
      (mevedel-session-artifacts-stabilize-gptel-bounds)
      (let ((span (car (mevedel-session-artifacts-fork-point-spans (current-buffer)))))
        (plist-put prompt :continuation
                   (list :segment 2
                         :start (mevedel-session-artifacts--continuation-start (current-buffer))
                         :fork-point-id id
                         :transcript-cutoff (plist-get span :transcript-cutoff))))
      (mevedel-session-artifacts-update-prompt-index session (current-buffer))
      (setq old-index (mevedel-session-prompt-index session)
            before (copy-tree old-index)
            text (mevedel-session-artifacts-finalized-segment-text
                  (buffer-string) 'utf-8-unix session 2)))
    (should (equal before old-index))
    (should (= 2 (mevedel-session-current-segment session)))
    (with-temp-buffer
      (insert (substring-no-properties text))
      (org-mode)
      (mevedel-transcript-restore-properties)
      (let* ((index (mevedel-session-prompt-index session))
             (continued (plist-get (cadr (assoc 1 index)) :continuation))
             (later (cadr (assoc 2 index)))
             (span (car (mevedel-session-artifacts-fork-point-spans (current-buffer)))))
        (should (equal id (plist-get continued :fork-point-id)))
        (should (= (plist-get continued :start)
                   (mevedel-session-artifacts--continuation-start (current-buffer))))
        (should (= (plist-get continued :transcript-cutoff)
                   (plist-get span :transcript-cutoff)))
        (should (= 2 (plist-get later :cum-turn)))
        (should (= (plist-get later :pos)
                   (plist-get (car (mevedel-session-artifacts-collect-prompts
                                    (current-buffer))) :pos))))))
  :doc "missing direct or continued completion fails without changing the index"
  (dolist (continued '(nil t))
    (let* ((source (list :fork-point-id (make-string 32 ?a) :transcript-cutoff 50))
           (prompt (append (list :turn 1 :file-turn 1 :cum-turn 1 :pos 1)
                           (if continued
                               (list :continuation (append (list :segment 2 :start 1) source))
                             source)))
           (index (list (list 1 prompt)))
           (session (mevedel-session--create :current-segment (if continued 2 1)
                                             :prompt-index index)))
      (should-error
       (mevedel-session-artifacts-finalized-segment-text
        "Prompt\n" 'utf-8-unix session))
      (should (eq index (mevedel-session-prompt-index session))))))

(mevedel-deftest mevedel-session-artifacts-replace-transcript-contents ()
  ,test
  (test)
  :doc "bounds wholesale swaps and preserves markers for close texts"
  (let ((source (generate-new-buffer " *replace-source*"))
        (target (generate-new-buffer " *replace-target*"))
        (bulk (mapconcat (lambda (index)
                           (format "line %06d of a long transcript\n" index))
                         (number-sequence 1 8000) ""))
        marker)
    (unwind-protect
        (progn
          (with-current-buffer source (insert "\n\n"))
          (with-current-buffer target
            (insert bulk)
            (let ((started (float-time)))
              (mevedel-session-artifacts-replace-transcript-contents source)
              (should (equal "\n\n" (buffer-string)))
              (should (< (- (float-time) started) 10))))
          (with-current-buffer source
            (erase-buffer)
            (insert bulk))
          (with-current-buffer target
            (let ((started (float-time)))
              (mevedel-session-artifacts-replace-transcript-contents source)
              (should (equal bulk (buffer-string)))
              (should (< (- (float-time) started) 10))))
          (with-current-buffer source
            (erase-buffer)
            (insert "alpha\nbeta\ngamma\n"))
          (with-current-buffer target
            (erase-buffer)
            (insert "alpha\nbeta\n")
            (setq marker (copy-marker 3))
            (mevedel-session-artifacts-replace-transcript-contents source)
            (should (equal "alpha\nbeta\ngamma\n" (buffer-string)))
            (should (= 3 (marker-position marker)))))
      (kill-buffer source)
      (kill-buffer target))))

(mevedel-deftest mevedel-session-artifacts--run-save-hooks-silently ()
  ,test
  (test)
  :doc "runs the hook with echo-area and log reporting suppressed"
  (let ((ran nil)
        (noise-suppressed 'unseen))
    (cl-letf (((symbol-function 'message)
               (lambda (&optional fmt &rest _args)
                 (when (and fmt (string-search "NOISE" fmt))
                   (setq noise-suppressed
                         (and inhibit-message (null message-log-max))))
                 nil)))
      (let ((after-save-hook
             (list (lambda () (setq ran t) (message "NOISE written")))))
        (mevedel-session-artifacts--run-save-hooks-silently 'after-save-hook)))
    (should ran)
    (should (eq noise-suppressed t))))

(mevedel-deftest mevedel-session-artifacts-inhibit-so-long ()
  ,test
  (test)
  :doc "reapplies native inhibition only in the prepared session buffer"
  (with-temp-buffer
    (mevedel-session-artifacts-inhibit-so-long)
    (should so-long--inhibited)
    (setq so-long--inhibited nil)
    (fundamental-mode)
    (should so-long--inhibited)
    (should (memq #'mevedel-session-artifacts-inhibit-so-long
                  after-change-major-mode-hook)))
  (with-temp-buffer
    (should-not
     (memq #'mevedel-session-artifacts-inhibit-so-long
           after-change-major-mode-hook))))


(mevedel-deftest mevedel-session-artifacts-save-buffer-silently ()
  ,test
  (test)
  :doc "writes the file while suppressing save and hook messages"
  (let* ((file (make-temp-file "mevedel-silent-save-"))
         (buffer (find-file-noselect file))
         (noise-suppressed 'unseen))
    (unwind-protect
        (with-current-buffer buffer
          (insert "content")
          (cl-letf (((symbol-function 'message)
                     (lambda (&optional fmt &rest _args)
                       (when (and fmt (string-search "NOISE" fmt))
                         (setq noise-suppressed
                               (and inhibit-message
                                    (null message-log-max))))
                       nil)))
            (let ((after-save-hook
                   (cons (lambda () (message "NOISE written"))
                         after-save-hook)))
              (mevedel-session-artifacts-save-buffer-silently)))
          (should (eq noise-suppressed t))
          (should-not (buffer-modified-p))
          (should (equal "content"
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))))
      (when (buffer-live-p buffer)
        (set-buffer-modified-p nil)
        (kill-buffer buffer))
      (when (file-exists-p file)
        (delete-file file)))))


(mevedel-deftest mevedel-session-artifacts--set-visited-segment-file ()
  ,test
  (test)
  :doc "sets canonical visited-file identity without renaming the live buffer"
  (let* ((directory (make-temp-file "mevedel-segment-identity-" t))
         (old-path (file-name-concat directory "segment-0001.chat.org"))
         (new-path (file-name-concat directory "segment-0002.chat.org"))
         (buffer (generate-new-buffer "*test-segment-identity*")))
    (unwind-protect
        (progn
          (write-region "old" nil old-path nil 'silent)
          (write-region "new" nil new-path nil 'silent)
          (with-current-buffer buffer
            (set-visited-file-name old-path t)
            (insert "new")
            (let ((name (buffer-name)))
              (mevedel-session-artifacts--set-visited-segment-file new-path)
              (should (equal name (buffer-name))))
            (should (file-equal-p new-path buffer-file-name))
            (should (equal (file-truename new-path) buffer-file-truename))
            (should (verify-visited-file-modtime buffer))
            (should-not (buffer-modified-p))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-directory-p directory) (delete-directory directory t)))))


(mevedel-deftest mevedel-session-artifacts--publish-segment-text ()
  ,test
  (test)
  :doc "atomically publishes text and installs matching live-buffer state"
  (let* ((directory (make-temp-file "mevedel-segment-publish-" t))
         (old-path (file-name-concat directory "segment-0001.chat.org"))
         (new-path (file-name-concat directory "segment-0002.chat.org"))
         (buffer (generate-new-buffer "*test-segment-publish*")))
    (unwind-protect
        (progn
          (write-region "old" nil old-path nil 'silent)
          (with-current-buffer buffer
            (set-visited-file-name old-path t)
            (insert "old")
            (set-buffer-modified-p nil)
            (mevedel-session-artifacts--publish-segment-text
             new-path "replacement")
            (should (equal "replacement" (buffer-string)))
            (should (file-equal-p new-path buffer-file-name))
            (should (verify-visited-file-modtime buffer)))
          (should
           (equal "replacement"
                  (with-temp-buffer
                    (insert-file-contents new-path)
                    (buffer-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-directory-p directory) (delete-directory directory t)))))


;;
;;; Phase 2: ID generation, paths, lazy materialization

(mevedel-deftest mevedel-session-artifacts-sanitize ()
  ,test
  (test)
  :doc "leaves alphanumerics, underscores, dashes alone"
  (should (equal "main" (mevedel-session-artifacts-sanitize "main")))
  (should (equal "alt-1_2"
                 (mevedel-session-artifacts-sanitize "alt-1_2")))
  :doc "replaces spaces and slashes with underscores"
  (should (equal "alt_branch"
                 (mevedel-session-artifacts-sanitize "alt branch")))
  (should (equal "a_b_c"
                 (mevedel-session-artifacts-sanitize "a/b/c")))
  :doc "handles nil input"
  (should (equal "" (mevedel-session-artifacts-sanitize nil))))


(mevedel-deftest mevedel-session-artifacts-segment-path ()
  ,test
  (test)
  :doc "zero-pads segment number to four digits"
  (should (equal "/x/segment-0001.chat.org"
                 (mevedel-session-artifacts-segment-path "/x" 1)))
  (should (equal "/x/segment-0042.chat.org"
                 (mevedel-session-artifacts-segment-path "/x" 42)))
  (should (equal "/x/segment-1000.chat.org"
                 (mevedel-session-artifacts-segment-path "/x" 1000))))


(mevedel-deftest mevedel-session-artifacts--read-published-artifact ()
  ,test
  (test)
  :doc "reads immutable publication bytes and verifies their digest"
  (let* ((directory (make-temp-file "mevedel-published-read-" t))
         (path (file-name-concat directory "published"))
         (content "published bytes")
         (publication
          (list :artifacts
                (list
                 (list "segment-0001.chat.org"
                       :published path
                       :sha256 (secure-hash 'sha256 content))))))
    (unwind-protect
        (progn
          (write-region content nil path nil 'silent)
          (should
           (equal content
                  (mevedel-session-artifacts--read-published-artifact
                   publication "segment-0001.chat.org")))
          (write-region "corrupt" nil path nil 'silent)
          (should-error
           (mevedel-session-artifacts--read-published-artifact
            publication "segment-0001.chat.org"))
          (should-error
           (mevedel-session-artifacts--read-published-artifact
            publication "segment-0002.chat.org")))
      (delete-directory directory t))))


(mevedel-deftest mevedel-session-artifacts-segment-number ()
  ,test
  (test)
  :doc "accepts only positive canonical round-trip segment names"
  (should (= 1 (mevedel-session-artifacts-segment-number
                "segment-0001.chat.org")))
  (should (= 10000 (mevedel-session-artifacts-segment-number
                    "segment-10000.chat.org")))
  (dolist (logical '("segment-0000.chat.org"
                     "segment-01.chat.org"
                     "segment-00001.chat.org"
                     "segment--001.chat.org"
                     "nested/segment-0001.chat.org"
                     "segment-0001.chat.org.bak"))
    (should-not
     (mevedel-session-artifacts-segment-number logical))))


(mevedel-deftest mevedel-session-artifacts--cold-segments ()
  ,test
  (test)
  :doc "enumerates PID-lock regular canonical entries with gaps in numeric order"
  (let* ((directory (make-temp-file "mevedel-cold-segments-" t))
         (target (file-name-concat directory "target")))
    (unwind-protect
        (progn
          (dolist (logical '("segment-10000.chat.org"
                             "segment-0005.chat.org"
                             "segment-0001.chat.org"
                             "segment-01.chat.org"
                             "segment-0000.chat.org"))
            (write-region logical nil
                          (file-name-concat directory logical) nil 'silent))
          (make-directory
           (file-name-concat directory "segment-0003.chat.org"))
          (write-region "target" nil target nil 'silent)
          (make-symbolic-link target
                              (file-name-concat
                               directory "segment-0004.chat.org"))
          (should
           (equal '("segment-0001.chat.org"
                    "segment-0005.chat.org"
                    "segment-10000.chat.org")
                  (mevedel-session-artifacts--cold-segments
                   directory 'pid-lock nil))))
      (delete-directory directory t)))
  :doc "enumerates portable manifest names without consulting fixed caches"
  (let ((directory (make-temp-file "mevedel-portable-segments-" t))
        (publication
         '(:artifacts
           (("segment-0010.chat.org" :published "/published/ten")
            ("segment-0002.chat.org" :published "/published/two")
            ("segment-02.chat.org" :published "/published/malformed")
            ("plans/current.md" :published "/published/plan")))))
    (unwind-protect
        (progn
          (write-region "stale" nil
                        (file-name-concat
                         directory "segment-9999.chat.org") nil 'silent)
          (should
           (equal '("segment-0002.chat.org" "segment-0010.chat.org")
                  (mevedel-session-artifacts--cold-segments
                   directory 'portable publication))))
      (delete-directory directory t))))


(mevedel-deftest mevedel-session-artifacts--select-cold-segment ()
  ,test
  (test)
  :doc "returns a singleton without prompting"
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _)
               (ert-fail "Prompted for a single segment"))))
    (should
     (equal "segment-0001.chat.org"
            (mevedel-session-artifacts--select-cold-segment
             '("segment-0001.chat.org")))))
  :doc "prompts for multiple segments with the newest as default"
  (let (seen-candidates seen-default)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt table _predicate _require-match
                        _initial _history default)
                 (setq seen-candidates (all-completions "" table)
                       seen-default default)
                 (should (eq 'identity
                             (alist-get 'display-sort-function
                                        (cdr (funcall table "" nil 'metadata)))))
                 (car seen-candidates))))
      (should
       (equal "segment-0001.chat.org"
              (mevedel-session-artifacts--select-cold-segment
               '("segment-0001.chat.org" "segment-0003.chat.org")))))
    (should
     (equal '("segment-0001.chat.org" "segment-0003.chat.org")
            seen-candidates))
    (should (equal "segment-0003.chat.org" seen-default))))


(mevedel-deftest mevedel-session-artifacts-inspect-cold-session ()
  ,test
  (test)
  :doc "creates unique safe bare Org buffers from verified UTF-8 PID bytes"
  (let* ((directory (make-temp-file "mevedel-cold-inspect-" t))
         (path (file-name-concat directory "segment-0001.chat.org"))
         (content ":PROPERTIES:\n:GPTEL_BOUNDS: ((1 . 2))\n:END:\n* hello\n")
         (hook-ran nil)
         buffers)
    (unwind-protect
        (progn
          (write-region content nil path nil 'silent)
          (let ((org-mode-hook (list (lambda () (setq hook-ran t)))))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _)
                         (ert-fail "Prompted for a single segment"))))
              (push (mevedel-session-artifacts-inspect-cold-session
                     directory 'pid-lock)
                    buffers)
              (push (mevedel-session-artifacts-inspect-cold-session
                     directory 'pid-lock)
                    buffers)))
          (should-not hook-ran)
          (should-not (equal (buffer-name (car buffers))
                             (buffer-name (cadr buffers))))
          (dolist (buffer buffers)
            (with-current-buffer buffer
              (should (derived-mode-p 'org-mode))
              (should (equal content (buffer-string)))
              (should mevedel-session--inspection-buffer-p)
              (should so-long--inhibited)
              (should buffer-read-only)
              (should-not (buffer-modified-p))
              (should-not buffer-file-name)
              (should-not buffer-offer-save)
              (should-not (bound-and-true-p gptel-mode))
              (should-not (text-property-not-all
                           (point-min) (point-max) 'gptel nil)))))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (delete-directory directory t)))
  :doc "reads PID-lock bytes through the pinned no-follow filesystem helper"
  (let ((read-path nil)
        buffer)
    (unwind-protect
        (cl-letf
            (((symbol-function 'mevedel-session-artifacts--cold-segments)
              (lambda (&rest _) '("segment-0001.chat.org")))
             ((symbol-function 'mevedel-session-control-fs-read-file)
              (lambda (path coding)
                (setq read-path path)
                (should (eq coding 'no-conversion))
                (encode-coding-string "* pinned\n" 'utf-8-unix)))
             ((symbol-function 'mevedel-session-artifacts-read-file-raw)
              (lambda (&rest _)
                (ert-fail "PID inspection used the unpinned raw reader"))))
          (setq buffer
                (mevedel-session-artifacts-inspect-cold-session
                 "/session/" 'pid-lock))
          (should (equal "/session/segment-0001.chat.org" read-path)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  :doc "uses published bytes only and rejects a failed portable hash"
  (let* ((directory (make-temp-file "mevedel-cold-portable-" t))
         (published (file-name-concat directory "published"))
         (fixed (file-name-concat directory "segment-0001.chat.org"))
         (content "* committed\n")
         (publication
          (list :artifacts
                (list
                 (list "segment-0001.chat.org"
                       :published published
                       :sha256 (secure-hash 'sha256 content)))))
         buffer)
    (unwind-protect
        (progn
          (write-region content nil published nil 'silent)
          (write-region "* stale cache\n" nil fixed nil 'silent)
          (setq buffer
                (mevedel-session-artifacts-inspect-cold-session
                 directory 'portable publication))
          (with-current-buffer buffer
            (should (equal content (buffer-string))))
          (write-region "corrupt" nil published nil 'silent)
          (let ((before (buffer-list)))
            (should-error
             (mevedel-session-artifacts-inspect-cold-session
              directory 'portable publication)
             :type 'user-error)
            (should (equal before (buffer-list)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t)))
  :doc "rejects invalid UTF-8 before creating an inspection buffer"
  (let* ((directory (make-temp-file "mevedel-cold-invalid-" t))
         (logical "segment-0001.chat.org")
         (path (file-name-concat directory logical))
         (expected-name
          (format "*mevedel inspection %s %s*"
                  (file-name-nondirectory (directory-file-name directory))
                  logical)))
    (unwind-protect
        ;; A stray byte, an overlong form, a surrogate, a scalar past
        ;; U+10FFFF, and a truncated sequence.
        (dolist (bytes (list (unibyte-string #xff)
                             (unibyte-string #xc0 #x80)
                             (unibyte-string #xed #xa0 #x80)
                             (unibyte-string #xf4 #x90 #x80 #x80)
                             (unibyte-string #xe2 #x82)))
          (let ((coding-system-for-write 'no-conversion))
            (write-region bytes nil path nil 'silent))
          (should-error
           (mevedel-session-artifacts-inspect-cold-session
            directory 'pid-lock)
           :type 'user-error)
          (should-not (get-buffer expected-name)))
      (delete-directory directory t)))
  :doc "reports no authoritative segment with a stable user error"
  (let ((directory (make-temp-file "mevedel-cold-empty-" t)))
    (unwind-protect
        (dolist (authority '(pid-lock portable))
          (let ((err (should-error
                      (mevedel-session-artifacts-inspect-cold-session
                       directory authority)
                      :type 'user-error)))
            (should
             (equal
              (format "No authoritative transcript segment is available: %s"
                      directory)
              (error-message-string err)))))
      (delete-directory directory t)))
  :doc "kills a partially initialized buffer when mode setup fails"
  (let* ((directory (make-temp-file "mevedel-cold-failed-" t))
         (logical "segment-0001.chat.org")
         (expected-name
          (format "*mevedel inspection %s %s*"
                  (file-name-nondirectory (directory-file-name directory))
                  logical)))
    (unwind-protect
        (progn
          (write-region "* transcript\n" nil
                        (file-name-concat directory logical) nil 'silent)
          (cl-letf (((symbol-function 'mevedel--transcript-org-mode)
                     (lambda () (error "Injected setup failure"))))
            (should-error
             (mevedel-session-artifacts-inspect-cold-session
              directory 'pid-lock)
             :type 'user-error))
          (should-not (get-buffer expected-name)))
      (delete-directory directory t))))


(mevedel-deftest mevedel-session-artifacts-segments ()
  ,test
  (test)
  :doc "lists the canonical range without hiding broken archived segments"
  (let* ((directory (make-temp-file "mevedel-segment-list-" t))
         (session
          (mevedel-session--create
           :name "segments"
           :save-path (file-name-as-directory directory)
           :authority-mode 'pid-lock
           :current-segment 4
           :prompt-index
           '((1 . ((:cum-turn 1 :preview "first prompt")))
             (2 . ((:cum-turn 2 :preview "missing prompt")))
             (3 . ((:cum-turn 3 :preview "unreadable prompt")))
             (4 . ((:cum-turn 4 :preview "live prompt"))))))
         (live-buffer (generate-new-buffer " *segment-list-live*")))
    (unwind-protect
        (progn
          (write-region "segment one\n" nil
                        (mevedel-session-artifacts-segment-path
                         directory 1)
                        nil 'silent)
          (make-directory
           (mevedel-session-artifacts-segment-path directory 3))
          (let ((segments
                 (mevedel-session-artifacts-segments
                  session live-buffer)))
            (should (equal '(1 2 3 4)
                           (mapcar
                            (lambda (entry) (plist-get entry :number))
                            segments)))
            (should (equal '(readable missing unreadable readable)
                           (mapcar
                            (lambda (entry) (plist-get entry :status))
                            segments)))
            (should (equal '(nil nil nil t)
                           (mapcar
                            (lambda (entry) (plist-get entry :current-p))
                            segments)))
            (should (equal
                     '("first prompt" "missing prompt"
                       "unreadable prompt" "live prompt")
                     (mapcar
                      (lambda (entry) (plist-get entry :preview))
                      segments)))))
      (when (buffer-live-p live-buffer)
        (kill-buffer live-buffer))
      (delete-directory directory t))))


(mevedel-deftest mevedel-session-artifacts-read-segment ()
  ,test
  (test)
  :doc "loads restored transcript properties into a non-authoritative buffer"
  (let* ((directory (make-temp-file "mevedel-segment-read-" t))
         (path (mevedel-session-artifacts-segment-path directory 1))
         (session
          (mevedel-session--create
           :name "segments"
           :save-path (file-name-as-directory directory)
           :authority-mode 'pid-lock
           :current-segment 2))
         inspection)
    (unwind-protect
        (progn
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n"
                    "Prompt\n"
                    "Archived answer.\n")
            (dotimes (_ 3)
              (goto-char (point-min))
              (search-forward "Archived answer.")
              (org-entry-put
               (point-min) "GPTEL_BOUNDS"
               (prin1-to-string
                `((response (,(match-beginning 0) ,(match-end 0)))))))
            (write-region (point-min) (point-max) path nil 'silent))
          (setq inspection
                (mevedel-session-artifacts-read-segment session 1))
          (with-current-buffer inspection
            (should (derived-mode-p 'org-mode))
            (should buffer-read-only)
            (should (bound-and-true-p
                     mevedel-session--inspection-buffer-p))
            (should-not (bound-and-true-p mevedel--session))
            (should-not buffer-file-name)
            (goto-char (point-min))
            (search-forward "Archived answer.")
            (should (eq 'response
                        (get-text-property (match-beginning 0) 'gptel)))
            (should-not
             (mevedel-session-persistence-authoritative-buffer
              inspection))))
      (when (buffer-live-p inspection)
        (kill-buffer inspection))
      (delete-directory directory t)))

  :doc "reports the exact missing path"
  (let* ((directory (make-temp-file "mevedel-segment-missing-" t))
         (session
          (mevedel-session--create
           :name "segments"
           :save-path (file-name-as-directory directory)
           :authority-mode 'pid-lock
           :current-segment 2))
         (path (mevedel-session-artifacts-segment-path directory 1)))
    (unwind-protect
        (let ((error
               (should-error
                (mevedel-session-artifacts-read-segment session 1)
                :type 'user-error)))
          (should (string-search path (error-message-string error))))
      (delete-directory directory t))))


(mevedel-deftest mevedel-session-artifacts-ensure-files ()
  ,test
  (test)
  :doc "lazily materializes the session directory tree"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Hello LLM\n")
                (let ((path (mevedel-session-artifacts-ensure-files
                             session buf)))
                  (should path)
                  (should (file-directory-p path))
                  (should (file-directory-p (file-name-concat path "agents")))
                  (should (file-directory-p
                           (file-name-concat path "file-history")))
                  ;; `ensure-files' leaves sidecar writing to `save'
                  ;; (one write instead of two on first materialization).
                  (should (file-exists-p
                           (file-name-concat path "segment-0001.chat.org")))
                  ;; Struct fields populated
                  (should (mevedel-session-session-id session))
                  (should (mevedel-session-created-at session))
                  (should (= 1 (mevedel-session-current-segment session)))
                  ;; Buffer wired to segment file
                  (should (equal (file-name-concat path "segment-0001.chat.org")
                                 buffer-file-name))
                  (let ((identity
                         (mevedel-workspace-identity-read
                          (mevedel-workspace-root workspace))))
                    (should (string-match-p
                             "\\`[0-9a-f]\\{64\\}\\'" identity))
                    (let ((saved-workspace
                           (plist-get
                            (mevedel-session-codec-serialize session)
                            :workspace)))
                      (should (equal identity
                                     (plist-get saved-workspace
                                                :workspace-id)))))
                  ;; Idempotent: second call returns same path, no churn
                  (should (equal path
                                 (mevedel-session-artifacts-ensure-files
                                  session buf)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "backfills diagnostics recorded before first materialization"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-data-buf*"))
               (repair-event
                '(:time "now" :origin "main" :backend backend
                  :model model :tool "Read" :outcome valid
                  :repair-enabled t :rules nil :paths nil
                  :issue-kinds nil :execution executed :result success)))
          (unwind-protect
              (progn
                (mevedel-hooks--log
                 session '(:event UserPromptSubmit :status ok))
                (mevedel-tool-repair-log-event session repair-event)
                (mevedel-permission-log
                 session 'permission-decision :tool-name "Read")
                (mevedel-telemetry-record
                 session 'test-lifecycle :outcome 'buffered)
                (with-current-buffer buf
                  (org-mode)
                  (mevedel-session-artifacts-ensure-files session buf))
                (dolist (file '("hook-log.el" "repair-log.el"
                                "permission-log.el" "telemetry-log.el"))
                  (should
                   (file-readable-p
                    (file-name-concat
                     (mevedel-session-save-path session) file))))
                (should-not
                 (mevedel-session-permission-log-pending session))
                (should-not
                 (mevedel-session-telemetry-pending session))
                (should-not
                 (mevedel-session-hook-log-pending session))
                (should-not
                 (mevedel-session-repair-log-pending session)))
            (when (buffer-live-p buf)
              (kill-buffer buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "repairs shallowly materialized sessions before saving data buffers"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (wrong-buf (generate-new-buffer "*test-wrong-buf*"))
               (data-buf (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (let ((path (with-current-buffer wrong-buf
                            (org-mode)
                            (mevedel-session-persistence-shallow-ensure-files
                             session wrong-buf))))
                (should path)
                (should-not
                 (file-exists-p
                  (file-name-concat path "segment-0001.chat.org")))
                (with-current-buffer data-buf
                  (org-mode)
                  (insert "Hello after shallow materialization\n")
                  (should-not buffer-file-name)
                  (should (equal path
                                 (mevedel-session-artifacts-ensure-files
                                  session data-buf)))
                  (should (equal (file-name-concat
                                  path "segment-0001.chat.org")
                                 buffer-file-name))
                  (should
                   (file-exists-p
                    (file-name-concat path "segment-0001.chat.org")))
                  (let ((segment-file buffer-file-name))
                    (should (string-match-p
                             "Hello after shallow materialization"
                             (with-temp-buffer
                               (insert-file-contents segment-file)
                               (buffer-string)))))))
            (when (buffer-live-p wrong-buf) (kill-buffer wrong-buf))
            (when (buffer-live-p data-buf) (kill-buffer data-buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-artifacts-save (:quiet t)
  ,test
  (test)
  :doc "compares a changed segment once while preserving no-op and forced saves"
  (let* ((root (make-temp-file "mevedel-save-comparison-" t))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *save-comparison*"))
         (digest (symbol-function 'mevedel-session-publication--content-sha256))
         (comparisons 0))
    (unwind-protect
        (with-current-buffer buffer
          (mevedel-chat-prepare-transcript-buffer)
          (setq-local mevedel--session session mevedel--workspace workspace)
          (insert "First prompt\n")
          (mevedel-session-artifacts-save session buffer)
          (goto-char (point-max))
          (setq-local buffer-file-coding-system 'utf-8-dos)
          (insert "Changed transcript\n" (string #x3bb) "\n")
          (cl-letf (((symbol-function 'mevedel-session-publication--content-sha256)
                     (lambda (artifact)
                       (when (equal "segment-0001.chat.org" (plist-get artifact :logical))
                         (cl-incf comparisons)
                         (should-not (multibyte-string-p (plist-get artifact :content)))
                         (should (eq 'no-conversion (plist-get artifact :coding))))
                       (funcall digest artifact))))
            (mevedel-session-artifacts-save session buffer))
          (should (= comparisons 1))
          (should (equal (encode-coding-string (buffer-string) 'utf-8-dos)
                         (mevedel-session-artifacts-read-artifact
                          session "segment-0001.chat.org" t)))
          (should (string-search "Changed transcript"
                                 (mevedel-session-artifacts-read-artifact
                                  session "segment-0001.chat.org" t)))
          (let ((head (plist-get (mevedel-session-publication session) :head))
                (entries (plist-get (mevedel-session-publication session) :artifacts)))
            (mevedel-session-artifacts-save session buffer)
            (should (equal head (plist-get (mevedel-session-publication session) :head)))
            (mevedel-session-artifacts-save session buffer nil t)
            (should-not (equal head (plist-get (mevedel-session-publication session) :head)))
            (should (equal (assoc "segment-0001.chat.org" entries)
                           (assoc "segment-0001.chat.org"
                                  (plist-get (mevedel-session-publication session) :artifacts))))))
      (test-mevedel-session-persistence--release-and-kill buffer session)
      (mevedel-workspace-clear-registry)
      (delete-directory root t)))

  :doc "unchanged request checkpoints reuse names and preserve prior publication bytes"
  (let* ((root (make-temp-file "mevedel-checkpoint-save-" t))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *checkpoint-save*"))
         (path (file-name-concat root "source.txt"))
         (pre (make-hash-table :test #'equal)))
    (unwind-protect
        (with-current-buffer buffer
          (mevedel-chat-prepare-transcript-buffer)
          (setq-local mevedel--session session)
          (setq-local mevedel--workspace workspace)
          (setq-local mevedel--current-request
                      (mevedel-request--create :file-snapshots pre))
          (insert "Edit the file\n")
          (puthash path "before\n" pre)
          (write-region "after\n" nil path nil 'silent)
          (mevedel-session-artifacts-save session buffer)
          (let* ((first (mevedel-session-publication session))
                 (entry (cdr (assoc path (cdr (assoc 0 (mevedel-session-file-snapshots session))))))
                 (logical (file-name-concat "file-history" (plist-get entry :backup-name)))
                 (old-bytes (plist-get (cdr (assoc logical (plist-get first :artifacts))) :published)))
            (mevedel-session-artifacts-save session buffer)
            (should (equal (plist-get first :head)
                           (plist-get (mevedel-session-publication session) :head)))
            (should (= 0 (length (directory-files
                                 (file-name-concat (mevedel-session-save-path session) "file-history")
                                 nil "^[^.]"))))
            (write-region "changed again\n" nil path nil 'silent)
            (mevedel-session-artifacts-save session buffer)
            (let* ((updated (cdr (assoc path (cdr (assoc 0 (mevedel-session-file-snapshots session))))))
                   (new-logical (file-name-concat "file-history" (plist-get updated :backup-name))))
              (should (= (plist-get entry :version) (plist-get updated :version)))
              (should (equal (plist-get entry :pre-backup-name) (plist-get updated :pre-backup-name)))
              (should-not (equal logical new-logical))
              (should-not (assoc logical (plist-get (mevedel-session-publication session) :artifacts)))
              (should (equal "changed again\n"
                             (mevedel-session-artifacts-read-artifact session new-logical t))))
            (should (equal "after\n"
                           (mevedel-session-control-fs-read-file old-bytes)))
            (should (= 2 (cl-count-if
                          (lambda (artifact) (string-prefix-p "file-history/" (car artifact)))
                          (plist-get (mevedel-session-publication session) :artifacts))))
            ;; Reverting the file removes its current checkpoint references,
            ;; while the prior publication remains independently readable.
            (write-region "before\n" nil path nil 'silent)
            (mevedel-session-artifacts-save session buffer)
            (should-not (seq-some
                         (lambda (artifact) (string-prefix-p "file-history/" (car artifact)))
                         (plist-get (mevedel-session-publication session) :artifacts)))
            (should (equal "after\n" (mevedel-session-control-fs-read-file old-bytes)))))
      (with-current-buffer buffer (setq-local mevedel--current-request nil))
      (test-mevedel-session-persistence--release-and-kill buffer session)
      (mevedel-workspace-clear-registry)
      (delete-directory root t)))
  :doc "PID snapshot cleanup follows durable commit and preserves failed-save recovery"
  (cl-destructuring-bind (session . root)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (with-current-buffer "*test-data-buf*"
          (let* ((path (file-name-concat root "changing.txt"))
                 (pre (make-hash-table :test #'equal))
                 old-backup pre-backup)
            (setq-local mevedel--session session)
            (setq-local mevedel--current-request (mevedel-request--create :file-snapshots pre))
            (puthash path "before" pre)
            (write-region "first" nil path nil 'silent)
            (mevedel-session-artifacts-save session (current-buffer))
            (let ((entry (cdr (assoc path (cdr (assoc 0 (mevedel-session-file-snapshots session)))))))
              (setq old-backup (mevedel-session-artifacts-backup-path
                                (mevedel-session-save-path session) (plist-get entry :backup-name))
                    pre-backup (mevedel-session-artifacts-backup-path
                                (mevedel-session-save-path session) (plist-get entry :pre-backup-name))))
            (write-region "second" nil path nil 'silent)
            (cl-letf (((symbol-function 'mevedel-session-codec-write)
                       (lambda (&rest _) (error "Injected sidecar failure"))))
              (should-error (mevedel-session-artifacts-save session (current-buffer))))
            (should (equal "first" (mevedel-session-artifacts-read-file-raw old-backup)))
            (should (equal "before" (mevedel-session-artifacts-read-file-raw pre-backup)))
            (mevedel-session-artifacts-save session (current-buffer))
            (should-not (file-exists-p old-backup))
            (should (equal "before" (mevedel-session-artifacts-read-file-raw pre-backup)))
            (should (= 2 (length (directory-files (file-name-directory old-backup) nil "^[^.]"))))
            (setq-local mevedel--current-request nil)))
      (test-mevedel-session-persistence--cleanup root)))
  :doc "assigns stable fork-point identity only to settled responses"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-fork-point-save*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "First prompt\n")
                (insert (propertize "First response\n" 'gptel 'response))
                (mevedel-session-artifacts-save session buf)
                (should-not
                 (plist-get
                  (car (cdr (assoc 1
                                   (mevedel-session-prompt-index session))))
                  :fork-point-id))
                (mevedel-session-artifacts-save session buf t)
                (let* ((entry
                        (car (cdr (assoc
                                   1
                                   (mevedel-session-prompt-index session)))))
                       (fork-point-id (plist-get entry :fork-point-id)))
                  (should (stringp fork-point-id))
                  (should-not (string-empty-p fork-point-id))
                  (should
                   (mevedel-transcript-audit-records
                    (buffer-string) 'fork-point))
                  (mevedel-session-artifacts-save session buf t)
                  (should
                   (equal
                    fork-point-id
                    (plist-get
                     (car (cdr (assoc
                                1
                                (mevedel-session-prompt-index session))))
                     :fork-point-id)))
                  (let* ((sidecar
                          (mevedel-session-codec-read
                           (mevedel-session-artifacts-sidecar-path
                            (mevedel-session-save-path session))))
                         (persisted
                          (car (cdr (assoc
                                     1
                                     (plist-get sidecar :prompt-index))))))
                    (should
                     (equal fork-point-id
                            (plist-get persisted :fork-point-id))))
                  (erase-buffer)
                  (insert "First prompt\n")
                  (insert (propertize "First response\n" 'gptel 'response))
                  (mevedel-session-artifacts-save session buf t)
                  (should-not
                   (equal
                    fork-point-id
                    (plist-get
                     (car (cdr (assoc
                                1
                                (mevedel-session-prompt-index session))))
                     :fork-point-id)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "leaves no Emacs backup or lock beside the segment"
  ;; A backup is one whole-segment copy over the connection and a lock is a
  ;; symlink per modify-and-save cycle, both answering questions the
  ;; publication and the lease already answer -- and both would sit in a
  ;; directory another client resumes from.
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-save-machinery*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "First prompt\n")
                (mevedel-session-artifacts-save session buf)
                ;; The backup lands on the second save, once the segment it
                ;; would copy exists.
                (insert "Second prompt\n")
                (mevedel-session-artifacts-save session buf)
                (should-not make-backup-files)
                (should-not create-lockfiles)
                (let ((entries (directory-files
                                (mevedel-session-save-path session))))
                  (should (member "segment-0001.chat.org" entries))
                  (should-not
                   (seq-find (lambda (entry)
                               (or (string-suffix-p "~" entry)
                                   (string-prefix-p ".#" entry)))
                             entries))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "advances updated-at across saves"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (mevedel-test--with-shifted-clock
                  (insert "First prompt\n")
                  (mevedel-session-artifacts-save session buf)
                  (let ((first-updated (mevedel-session-updated-at session)))
                    (should first-updated)
                    ;; Advance the clock the stamps see instead of sleeping
                    ;; past a second boundary.
                    (setq mevedel-test--timestamp-offset 2)
                    (insert "Second prompt\n")
                    (mevedel-session-artifacts-save session buf)
                    (should-not (equal first-updated
                                       (mevedel-session-updated-at session))))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "sidecar updates current state while preserving the first prompt"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Refactor the permission chain\n")
                (mevedel-session-artifacts-save session buf)
                (let* ((sidecar-path
                        (mevedel-session-artifacts-sidecar-path
                         (mevedel-session-save-path session)))
                       (plist (mevedel-session-codec-read sidecar-path)))
                  (should (equal "main" (plist-get plist :session-name)))
                  (should (equal "Refactor the permission chain"
                                 (plist-get plist :first-user-message)))
                  (should (equal "Refactor the permission chain"
                                 (plist-get plist :latest-user-message)))
                  (should (= 1 (plist-get plist :current-segment))))
                (erase-buffer)
                (insert "Later prompt\n")
                (mevedel-session-artifacts-save session buf)
                (let ((plist (mevedel-session-codec-read
                              (mevedel-session-artifacts-sidecar-path
                               (mevedel-session-save-path session)))))
                  (should (equal "Refactor the permission chain"
                                 (plist-get plist :first-user-message)))
                  (should (equal "Later prompt"
                                 (plist-get plist :latest-user-message)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "latest sidecar preview follows the newest prompt"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "First prompt\n")
                (insert (propertize "Assistant response\n" 'gptel 'response))
                (insert "Second prompt\n")
                (mevedel-session-artifacts-save session buf)
                (let* ((sidecar-path
                        (mevedel-session-artifacts-sidecar-path
                         (mevedel-session-save-path session)))
                       (plist (mevedel-session-codec-read sidecar-path)))
                  (should (equal "First prompt"
                                 (plist-get plist :first-user-message)))
                  (should (equal "Second prompt"
                                 (plist-get plist :latest-user-message)))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "view buffers save through their data buffer without becoming files"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (data-buf (generate-new-buffer " *test-data*"))
               (view-buf (generate-new-buffer " *test-view*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
                  (setq-local mevedel--session session)
                  (setq-local mevedel--workspace workspace)
                  (insert "Persist data prompt\n"))
                (mevedel-view--setup view-buf data-buf)
                (with-current-buffer view-buf
                  (let ((inhibit-read-only t)
                        (inhibit-modification-hooks t))
                    (goto-char mevedel-view--input-marker)
                    (insert "Working view chrome\n"))
                  (set-buffer-modified-p t))
                (cl-letf (((symbol-function 'read-file-name)
                           (lambda (&rest _)
                             (error "View buffer requested a save filename"))))
                  (mevedel-session-artifacts-save session data-buf))
                (with-current-buffer view-buf
                  (should-not buffer-file-name)
                  (should-not buffer-file-truename))
                (let ((segment-path
                       (mevedel-session-artifacts-segment-path
                        (mevedel-session-save-path session) 1)))
                  (should (file-exists-p segment-path))
                  (with-temp-buffer
                    (insert-file-contents segment-path)
                    (should (string-match-p "Persist data prompt"
                                            (buffer-string)))
                    (should-not (string-match-p "Working view chrome"
                                                (buffer-string))))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf (set-buffer-modified-p nil))
              (kill-buffer view-buf))
            (test-mevedel-session-persistence--release-and-kill
             data-buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "retries queued diagnostics after the next successful save"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer " *test-diagnostic-retry*"))
               (blocked (file-name-concat tempdir "blocked"))
               save-path)
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Persist diagnostics\n")
                (mevedel-session-artifacts-save session buf)
                (setq save-path (mevedel-session-save-path session))
                (write-region "not a directory" nil blocked nil 'silent)
                (setf (mevedel-session-save-path session) blocked)
                (mevedel-hooks--log
                 session '(:event Stop :status completed))
                (mevedel-tool-repair-log-event
                 session
                 '(:time "now" :origin "/root" :backend backend
                         :model model :tool "Read" :outcome repaired
                         :repair-enabled t :rules (array-to-list)
                         :paths ((names)) :issue-kinds (wrong-shape)
                         :execution executed :result success))
                (should (mevedel-session-hook-log-pending session))
                (should (mevedel-session-repair-log-pending session))
                (setf (mevedel-session-save-path session) save-path)
                (mevedel-session-artifacts-save session buf)
                (should-not (mevedel-session-hook-log-pending session))
                (should-not (mevedel-session-repair-log-pending session))
                (should
                 (file-readable-p (file-name-concat save-path "hook-log.el")))
                (should
                 (file-readable-p
                  (file-name-concat save-path "repair-log.el"))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-artifacts--published-artifact-files
  (:doc "captures nested literal files and tombstones committed deletions")
  (let* ((save-path (make-temp-file "mevedel-published-artifacts-" t))
         (dir (mevedel-session-artifacts-artifacts-dir save-path))
         (nested (file-name-concat dir "nested" "mockup.bin"))
         (missing (file-name-concat dir "gone.html"))
         (session (mevedel-session--create :name "s" :save-path save-path)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory nested) t)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert (unibyte-string 0 255 1))
            (let ((coding-system-for-write 'no-conversion))
              (write-region nil nil nested nil 'silent)))
          (setf (mevedel-session-publication session)
                (list :artifacts
                      `(("artifacts/nested/mockup.bin" :published "unused"
                         :sha256 ,(make-string 64 ?a))
                        ("artifacts/gone.html" :published "unused"
                         :sha256 ,(make-string 64 ?b)))))
          (let* ((artifacts
                  (mevedel-session-artifacts--published-artifact-files session))
                 (current (cl-find nested artifacts
                                   :key (lambda (artifact)
                                          (plist-get artifact :path))
                                   :test #'equal))
                 (deleted (cl-find missing artifacts
                                   :key (lambda (artifact)
                                          (plist-get artifact :path))
                                   :test #'equal)))
            (should (equal (unibyte-string 0 255 1)
                           (plist-get current :content)))
            (should (eq t (plist-get deleted :delete)))))
      (delete-directory save-path t))))

(mevedel-deftest mevedel-session-artifacts-materialize-published-artifacts
  (:doc "replaces stale fixed artifacts only after verifying every committed byte")
  (let* ((source-root (make-temp-file "mevedel-artifact-source-" t))
         (destination-root (make-temp-file "mevedel-artifact-target-" t))
         (published (file-name-concat source-root "mockup.bin"))
         (destination-dir
          (mevedel-session-artifacts-artifacts-dir destination-root))
         (destination
          (file-name-concat destination-dir "nested" "mockup.bin"))
         (stale (file-name-concat destination-dir "stale.txt"))
         (content (unibyte-string 0 255 1))
         (session
          (mevedel-session--create
           :name "portable" :save-path source-root :authority-mode 'portable)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert content)
            (let ((coding-system-for-write 'no-conversion))
              (write-region nil nil published nil 'silent)))
          (setf (mevedel-session-publication session)
                `(:artifacts
                  (("artifacts/nested/mockup.bin"
                    :published ,published
                    :sha256 ,(secure-hash 'sha256 content)))))
          (make-directory (file-name-directory destination) t)
          (write-region "poison" nil destination nil 'silent)
          (write-region "stale" nil stale nil 'silent)
          (mevedel-session-artifacts-materialize-published-artifacts
           session destination-root)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally destination)
            (should (equal content (buffer-string))))
          (should-not (file-exists-p stale))
          ;; Verification failure leaves the current folder intact.
          (write-region "corrupt" nil published nil 'silent)
          (write-region "keep" nil destination nil 'silent)
          (should-error
           (mevedel-session-artifacts-materialize-published-artifacts
            session destination-root))
          (with-temp-buffer
            (insert-file-contents-literally destination)
            (should (equal "keep" (buffer-string)))))
      (delete-directory source-root t)
      (delete-directory destination-root t))))

(mevedel-deftest mevedel-session-artifacts-save-agent-registry (:quiet t)
  ,test
  (test)
  :doc "rewrites the sidecar without touching the segment"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-registry-save*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (mevedel-test--with-shifted-clock
                  (insert "First prompt\n")
                  (mevedel-session-artifacts-save session buf)
                  (let* ((segment (buffer-file-name))
                         (segment-text
                          (with-temp-buffer
                            (insert-file-contents segment)
                            (buffer-string)))
                         (first-updated
                          (mevedel-session-updated-at session)))
                    (setq mevedel-test--timestamp-offset 2)
                    (insert "Streamed text not yet settled\n")
                    (should
                     (equal (mevedel-session-artifacts-save-agent-registry
                             session buf)
                            (mevedel-session-save-path session)))
                    ;; The segment on disk stays as the last settlement
                    ;; wrote it, and the buffer keeps its unsaved
                    ;; modification for the next settlement save.
                    (should (buffer-modified-p))
                    (should (equal segment-text
                                   (with-temp-buffer
                                     (insert-file-contents segment)
                                     (buffer-string))))
                    ;; The sidecar itself was rewritten.
                    (should-not (equal first-updated
                                       (mevedel-session-updated-at session)))
                    (should
                     (equal (mevedel-session-updated-at session)
                            (plist-get
                             (mevedel-session-codec-read
                              (mevedel-session-artifacts-sidecar-path
                               (mevedel-session-save-path session)))
                             :updated-at))))))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "publishes only the sidecar for a portable session"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-registry-portable-" t)))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "main" workspace))
         (buf (generate-new-buffer " *test-registry-portable*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local mevedel--session session)
          (mevedel-test--with-shifted-clock
            (insert "First prompt\n")
            (mevedel-session-artifacts-save session buf)
            (let* ((artifacts-before
                    (plist-get (mevedel-session-publication session)
                               :artifacts))
                   (segment-before
                    (cdr (assoc "segment-0001.chat.org" artifacts-before)))
                   (sidecar-before
                    (cdr (assoc "session.meta.el" artifacts-before)))
                   (committed-segment
                    (mevedel-session-artifacts-read-artifact
                     session "segment-0001.chat.org" t))
                   (first-updated (mevedel-session-updated-at session)))
              (should segment-before)
              (should sidecar-before)
              (setq mevedel-test--timestamp-offset 2)
              (insert "Streamed text not yet settled\n")
              (should
               (equal (mevedel-session-artifacts-save-agent-registry
                       session buf)
                      (mevedel-session-save-path session)))
              (let ((artifacts-after
                     (plist-get (mevedel-session-publication session)
                                :artifacts)))
                ;; The segment entry is the committed one, untouched; the
                ;; sidecar entry is new.
                (should (equal segment-before
                               (cdr (assoc "segment-0001.chat.org"
                                           artifacts-after))))
                (should-not
                 (equal (plist-get sidecar-before :published)
                        (plist-get (cdr (assoc "session.meta.el"
                                               artifacts-after))
                                   :published))))
              (should (equal committed-segment
                             (mevedel-session-artifacts-read-artifact
                              session "segment-0001.chat.org" t)))
              (should (buffer-modified-p))
              (should-not (equal first-updated
                                 (mevedel-session-updated-at session)))
              (should
               (equal (mevedel-session-updated-at session)
                      (plist-get
                       (mevedel-session-codec-read
                        (mevedel-session-artifacts-sidecar-path
                         (mevedel-session-save-path session)))
                       :updated-at))))))
      (test-mevedel-session-persistence--release-and-kill buf session)
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))

  :doc "skips a portable session whose sidecar is not yet committed"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-registry-shallow-" t)))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "main" workspace))
         (buf (generate-new-buffer " *test-registry-shallow*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local mevedel--session session)
          (let ((save-path (mevedel-session-persistence-shallow-ensure-files
                            session buf)))
            (should save-path)
            (should-not (mevedel-session-artifacts-save-agent-registry
                         session buf))
            (should-not (file-exists-p
                         (file-name-concat save-path "session.meta.el")))))
      (test-mevedel-session-persistence--release-and-kill buf session)
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))

  :doc "a portable publication failure leaves the committed snapshot intact"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-registry-failure-" t)))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "main" workspace))
         (buf (generate-new-buffer " *test-registry-failure*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (setq-local mevedel--session session)
          (insert "First prompt\n")
          (mevedel-session-artifacts-save session buf)
          (let ((committed (mevedel-session-artifacts-read-artifact
                            session "segment-0001.chat.org" t)))
            (cl-letf (((symbol-function 'mevedel-session-publication-publish)
                       (lambda (&rest _) (error "Publication unavailable"))))
              (should-error (mevedel-session-artifacts-save-agent-registry
                             session buf)))
            (should (equal committed
                           (mevedel-session-artifacts-read-artifact
                            session "segment-0001.chat.org" t)))))
      (test-mevedel-session-persistence--release-and-kill buf session)
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))

  :doc "does nothing for an unmaterialized session"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-registry-cold*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (should-not (mevedel-session-artifacts-save-agent-registry
                             session buf))
                (should-not (mevedel-session-save-path session)))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-artifacts-instruction-snapshots (:quiet t)
  ,test
  (test)
  :doc "saves current and per-turn instruction snapshots"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let* ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "(defun alpha () t)\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (mevedel--create-reference-in source-buf (point-min) (point-max)))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain alpha\n")
              (mevedel-session-artifacts-save session data-buf))
            (let ((current-path
                   (mevedel-session-artifacts-instructions-current-path
                    (mevedel-session-save-path session)))
                  (turn-path
                   (mevedel-session-artifacts-instructions-turn-path
                    (mevedel-session-save-path session) 1)))
              (should (file-exists-p current-path))
              (should (file-exists-p turn-path))
              (let* ((current-save (with-temp-buffer
                                     (insert-file-contents current-path)
                                     (read (current-buffer))))
                     (turn-save (with-temp-buffer
                                  (insert-file-contents turn-path)
                                  (read (current-buffer))))
                     (current-file-plist
                      (cdr (assoc "source.el"
                                  (plist-get current-save :files))))
                     (turn-file-plist
                      (cdr (assoc "source.el"
                                  (plist-get turn-save :files))))
                     (instruction
                      (car (plist-get current-file-plist :instructions)))
                     (turn-instruction
                      (car (plist-get turn-file-plist :instructions)))
                     (properties
                      (plist-get instruction :properties))
                     (anchor (plist-get turn-instruction :anchor)))
                (should (plist-member current-file-plist :original-content))
                (should-not (plist-member turn-file-plist
                                          :original-content))
                (should (= 1 (plist-get turn-file-plist :anchor-schema)))
                (should (plist-get turn-file-plist :content-hash))
                (should (= 1 (plist-get anchor :schema)))
                (should (plist-get anchor :uuid))
                (should (plist-member anchor :bodyless))
                (should (plist-get anchor :text-hash))
                (should (memq 'mevedel-instruction properties))
                (should-not (memq 'before-string properties))
                (should-not (memq 'face properties))
                (should-not (memq 'keymap properties))
                (should-not (memq 'mevedel-bg-color properties)))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "restores instruction overlays after clearing live state"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let* ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "(defun beta () t)\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (mevedel--create-reference-in source-buf (point-min) (point-max)))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain beta\n")
              (mevedel-session-artifacts-save session data-buf)
              (mevedel--clear-instruction-state workspace)
              (should-not (mevedel--all-instructions))
              (mevedel-session-artifacts-load-instructions session data-buf 1))
            (mevedel--instruction-activate-workspace workspace)
            (should (= 1 (length (alist-get source-buf (mevedel--instruction-alist))))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "strips transient text properties from persisted instruction strings"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let* ((source-file (file-name-concat tempdir "source.el"))
                 (directive-text (copy-sequence "Fix beta")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "(defun beta () t)\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (add-text-properties 0 (length directive-text)
                                 `(tabulated-list-id ,source-buf)
                                 directive-text)
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (mevedel--create-directive-in
               source-buf (point-min) (point-max) nil directive-text))
            (setq session (mevedel-session-create "main" workspace))
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain beta\n")
              (mevedel-session-artifacts-save session data-buf))
            (let* ((current-path
                    (mevedel-session-artifacts-instructions-current-path
                     (mevedel-session-save-path session)))
                   (save-file (with-temp-buffer
                                (insert-file-contents current-path)
                                (read (current-buffer))))
                   (directive
                    (car (plist-get save-file :directives)))
                   (request (plist-get directive :request)))
              (should (equal "Fix beta" request))
              (should-not (text-properties-at 0 request)))
            (with-current-buffer data-buf
              (mevedel--clear-instruction-state workspace)
              (mevedel-session-artifacts-load-instructions session data-buf))
            (mevedel--instruction-activate-workspace workspace)
            (let* ((ov (car (alist-get source-buf (mevedel--instruction-alist))))
                   (request (mevedel--directive-text ov)))
              (should (equal "Fix beta" request))
              (should-not (text-properties-at 0 request))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "loads historical presentations without replacing current directive records"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "EARLY\nLATER\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (search-forward "EARLY")
              (mevedel--create-directive-in
               source-buf (match-beginning 0) (match-end 0) nil "Early"))
            (setq session (mevedel-session-create "main" workspace)
                  data-buf (generate-new-buffer "*test-data-buf*"))
            (setf (mevedel-session-turn-count session) 1)
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "First turn\n")
              (mevedel-session-artifacts-save session data-buf))
            (let ((early (car (mevedel-workspace-directives workspace))))
              (setf (mevedel-directive-attempts early)
                    (list
                     (mevedel-directive-attempt--create
                      :directive-request "Early" :request "Early prompt"
                      :result "Done" :outcome 'success :patch ""
                      :capture 'complete :captured-at "2026-08-02T03:00:00+0200"
                      :checkpoint '(:session-id "session" :turn 1))))
              (mevedel-directive-set-request early "Current early edit")
              (with-current-buffer source-buf
                (goto-char (point-min))
                (search-forward "LATER")
                (mevedel--create-directive-in
                 source-buf (match-beginning 0) (match-end 0) nil "Later"))
              (let* ((later (car (mevedel-workspace-directives workspace)))
                     (records (copy-sequence
                               (mevedel-workspace-directives workspace))))
                (setf (mevedel-directive-attempts later)
                      (list
                       (mevedel-directive-attempt--create
                        :directive-request "Later" :outcome 'success
                        :checkpoint '(:session-id "session" :turn 2))))
                (with-current-buffer data-buf
                  (mevedel-session-artifacts-load-instructions
                   session data-buf 1 records))
                (should (equal records
                               (mevedel-workspace-directives workspace)))
                (should (equal "Current early edit"
                               (mevedel-directive-request early)))
                (should (= 1 (length (mevedel-directive-attempts early))))
                (should (= 1 (length (mevedel-directive-attempts later))))
                (dolist (record records)
                  (should
                   (eq record
                       (mevedel--directive-record
                        (mevedel--instruction-with-uuid
                         (mevedel-directive-id record) workspace))))))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "ignores unreadable instruction snapshots during session restore"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((data-buf nil)
          (session nil))
      (unwind-protect
          (progn
            (test-mevedel-session-persistence--reset-instructions)
            (setq session (mevedel-session-create "main" workspace))
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain gamma\n")
              (mevedel-session-artifacts-save session data-buf))
            (let ((path (mevedel-session-artifacts-instructions-current-path
                         (mevedel-session-save-path session))))
              (make-directory (file-name-directory path) t)
              (write-region "(:files ((\"source.el\" . #<marker>)))"
                            nil path nil 'silent)
              (should-not
               (mevedel-session-artifacts-load-instructions
                session data-buf))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "loads remote instruction bytes from the immutable publication"
  (let* ((host "instruction-publication")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-instruction-publication-" t)))
         data-buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace session session-dir _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let* ((client-id (make-string 64 ?a))
                   (mevedel-session-durability--client-id client-id)
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (instruction-path
                    (mevedel-session-artifacts-instructions-current-path
                     session-dir))
                   (published "(:published instruction snapshot)\n")
                   observed-source
                   observed-content
                   observed-base)
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (unwind-protect
                  (progn
                    (should
                     (mevedel-session-durability-lease-acquire
                      session-dir "*instruction-publisher*" session))
                    (let* ((publication
                            (mevedel-session-publication-read
                             session-dir))
                           (sidecar
                            (mevedel-session-persistence-load-sidecar
                             (plist-get publication :sidecar))))
                      (mevedel-session-publication-publish
                       session
                       (list
                        (list :path instruction-path :content published)
                        (list
                         :path
                         (mevedel-session-artifacts-sidecar-path session-dir)
                         :content
                         (mevedel-session-artifacts-printed-value sidecar)
                         :commit-marker t))))
                    (mevedel-session-durability-lease-release
                     session-dir session)
                    (make-directory (file-name-directory instruction-path) t)
                    (write-region "poisoned fixed cache\n" nil
                                  instruction-path nil 'silent)
                    (setq data-buffer
                          (generate-new-buffer " *remote instructions*"))
                    (cl-letf
                        (((symbol-function 'mevedel--load-instructions-file)
                          (lambda (source base &rest _)
                            (setq observed-source source
                                  observed-base base
                                  observed-content
                                  (mevedel-session-artifacts-read-file-raw source))
                            t)))
                      (should
                       (mevedel-session-artifacts-load-instructions
                        session data-buffer)))
                    (should-not (file-remote-p observed-source))
                    (should-not (equal observed-source instruction-path))
                    (should (equal published observed-content))
                    (should
                     (equal (mevedel-workspace-root workspace)
                            observed-base)))
                (when (mevedel-session-durability-lease-owned-p session)
                  (mevedel-session-durability-lease-release
                   session-dir session))))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-artifacts-instruction-anchor-restore (:quiet t)
  ,test
  (test)
  :doc "reanchors an instruction after text is inserted before it"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "aaa\nTARGET\nbbb\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (search-forward "TARGET\n")
              (mevedel--create-reference-in
               source-buf (match-beginning 0) (match-end 0)))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain target\n")
              (mevedel-session-artifacts-save session data-buf)
              (mevedel--clear-instruction-state workspace)
              (with-current-buffer source-buf
                (goto-char (point-min))
                (insert "inserted\n"))
              (mevedel-session-artifacts-load-instructions
               session data-buf 1))
            (mevedel--instruction-activate-workspace workspace)
            (let ((ov (car (alist-get source-buf (mevedel--instruction-alist)))))
              (should ov)
              (with-current-buffer source-buf
                (should (equal "TARGET\n"
                               (buffer-substring-no-properties
                                (overlay-start ov) (overlay-end ov)))))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "uses parent containment to resolve duplicate child text"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "PARENT\nchild\nEND\noutside child\n"
                          nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (let ((parent-start (point)))
                (search-forward "END\n")
                (mevedel--create-reference-in
                 source-buf parent-start (point)))
              (goto-char (point-min))
              (search-forward "child")
              (mevedel--create-reference-in
               source-buf (match-beginning 0) (match-end 0)))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain nested target\n")
              (mevedel-session-artifacts-save session data-buf)
              (mevedel--clear-instruction-state workspace)
              (with-current-buffer source-buf
                (goto-char (point-min))
                (insert "inserted\n"))
              (mevedel-session-artifacts-load-instructions
               session data-buf 1))
            (mevedel--instruction-activate-workspace workspace)
            (let* ((ovs (alist-get source-buf (mevedel--instruction-alist)))
                   (child (cl-find-if
                           (lambda (ov)
                             (with-current-buffer source-buf
                               (equal "child"
                                      (buffer-substring-no-properties
                                       (overlay-start ov)
                                       (overlay-end ov)))))
                           ovs)))
              (should (= 2 (length ovs)))
              (should child)
              (with-current-buffer source-buf
                (save-excursion
                  (goto-char (overlay-start child))
                  (should (search-backward "PARENT" nil t))))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "leaves ambiguous anchors unresolved instead of restoring stale bounds"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil)
          (old-context mevedel-instruction-anchor-context-chars))
      (unwind-protect
          (let ((source-file (file-name-concat tempdir "source.el")))
            (setq mevedel-instruction-anchor-context-chars 0)
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "x\ndup\ny\nx\ndup\ny\n"
                          nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (search-forward "dup\n")
              (mevedel--create-reference-in
               source-buf (match-beginning 0) (match-end 0)))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain ambiguous target\n")
              (mevedel-session-artifacts-save session data-buf)
              (mevedel--clear-instruction-state workspace)
              (with-current-buffer source-buf
                (goto-char (point-min))
                (insert "inserted\n"))
              (mevedel-session-artifacts-load-instructions
               session data-buf 1))
            (mevedel--instruction-activate-workspace workspace)
            (with-current-buffer source-buf
              (should-not (mevedel--instructions-in
                           (point-min) (point-max)))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (setq mevedel-instruction-anchor-context-chars old-context)
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry))))
  :doc "reanchors a bodyless directive by surrounding context"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((source-buf nil)
          (data-buf nil)
          (session nil))
      (unwind-protect
          (let ((source-file (file-name-concat tempdir "source.el")))
            (test-mevedel-session-persistence--reset-instructions)
            (write-region "before TARGET after\n" nil source-file nil 'silent)
            (setq source-buf (find-file-noselect source-file))
            (with-current-buffer source-buf
              (setq-local mevedel--workspace workspace)
              (goto-char (point-min))
              (search-forward "TARGET")
              (mevedel--create-directive-in
               source-buf (match-beginning 0) (match-beginning 0)
               t "Do it"))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-turn-count session) 1)
            (setq data-buf (generate-new-buffer "*test-data-buf*"))
            (with-current-buffer data-buf
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--session session)
              (org-mode)
              (insert "Explain bodyless target\n")
              (mevedel-session-artifacts-save session data-buf)
              (mevedel--clear-instruction-state workspace)
              (with-current-buffer source-buf
                (goto-char (point-min))
                (insert "inserted\n"))
              (mevedel-session-artifacts-load-instructions
               session data-buf 1))
            (mevedel--instruction-activate-workspace workspace)
            (let ((ov (car (alist-get source-buf (mevedel--instruction-alist)))))
              (should ov)
              (should (= (overlay-start ov) (overlay-end ov)))
              (with-current-buffer source-buf
                (goto-char (overlay-start ov))
                (should (looking-at-p "TARGET")))))
        (when (and data-buf (buffer-live-p data-buf))
          (test-mevedel-session-persistence--release-and-kill data-buf session))
        (when (buffer-live-p source-buf)
          (with-current-buffer source-buf (set-buffer-modified-p nil))
          (kill-buffer source-buf))
        (delete-directory tempdir t)
        (test-mevedel-session-persistence--reset-instructions)
        (mevedel-workspace-clear-registry)))))


(mevedel-deftest mevedel-session-artifacts--save-gptel-state-around ()
  ,test
  (test)
  :doc "hides the system prompt from the delegated save and strips its metadata"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session
                      (mevedel-session-create
                       "main"
                       (test-mevedel-session-persistence--make-workspace root)))
          (let ((gptel-system-prompt "Frozen prompt")
                delegated-system
                orig-fun)
            (setq orig-fun
                  (lambda ()
                    (setq delegated-system gptel-system-prompt)
                    (org-entry-put (point-min) "GPTEL_BOUNDS"
                                   "((response (42 55)))")))
            (org-entry-put (point-min) "GPTEL_SYSTEM" "Frozen prompt")
            (mevedel-session-artifacts--save-gptel-state-around orig-fun)
            (should-not delegated-system)
            (should-not (org-entry-get (point-min) "GPTEL_SYSTEM"))
            (should-not (org-entry-get (point-min) "GPTEL_BOUNDS"))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "removes accumulated and case-variant GPTEL_SYSTEM properties"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert ":PROPERTIES:\n:gptel_system: Lowercase prompt\n:GPTEL_SYSTEM+: Extra prompt\n:OTHER: Keep\n:END:\n")
          (setq-local mevedel--session
                      (mevedel-session-create
                       "main"
                       (test-mevedel-session-persistence--make-workspace root)))
          (let ((gptel-system-prompt "Frozen prompt"))
            (cl-letf (((symbol-function 'gptel--get-buffer-bounds)
                         (lambda () nil)))
              (mevedel-session-artifacts--save-gptel-state-around
               (lambda () nil)))
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              (should-not (string-match-p "GPTEL_SYSTEM" text))
              (should-not (string-match-p "gptel_system" text))
              (should (string-match-p ":OTHER: Keep" text)))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "removes multiline GPTEL_SYSTEM values"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (org-entry-put (point-min) "GPTEL_SYSTEM"
                         "Frozen first\nFrozen second")
          (goto-char (point-max))
          (insert "Body\n")
          (setq-local mevedel--session
                      (mevedel-session-create
                       "main"
                       (test-mevedel-session-persistence--make-workspace root)))
          (let ((gptel-system-prompt "Frozen prompt"))
            (cl-letf (((symbol-function 'gptel--get-buffer-bounds)
                         (lambda () nil)))
              (mevedel-session-artifacts--save-gptel-state-around
               (lambda () nil))))
          (let ((text (buffer-substring-no-properties
                       (point-min) (point-max))))
            (should-not (string-match-p "GPTEL_SYSTEM" text))
            (should-not (string-match-p "Frozen first" text))
            (should-not (string-match-p "Frozen second" text))
            (should (string-match-p "Body" text))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "routes top-level property writes around Org entry helpers"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert ":PROPERTIES:\n:GPTEL_SYSTEM: Frozen prompt\n:END:\n")
          (let (start end)
            (setq start (point))
            (insert "Assistant body\n")
            (setq end (point))
            (add-text-properties start end '(gptel response))
            (setq-local mevedel--session
                        (mevedel-session-create
                         "main"
                         (test-mevedel-session-persistence--make-workspace root)))
            (let ((orig-fun
                   (lambda ()
                     (org-entry-put (point-min) "GPTEL_MODEL" "fake-model")
                     (org-entry-delete (point-min) "GPTEL_SYSTEM"))))
              (cl-letf (((symbol-function 'gptel--get-buffer-bounds)
                         (lambda () `((response (,start ,end)))))
                        ((symbol-function 'org-entry-put)
                         (lambda (&rest _)
                           (error "Slow org-entry-put should not run")))
                        ((symbol-function 'org-entry-delete)
                         (lambda (&rest _)
                           (error "Slow org-entry-delete should not run"))))
                (mevedel-session-artifacts--save-gptel-state-around
                 orig-fun)))
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              ;; The routed GPTEL_MODEL write succeeded (the slow Org
              ;; stubs would have signaled), and cleanup then
              ;; stripped it as request config.
              (should (string-match-p ":GPTEL_BOUNDS: " text))
              (should-not (string-match-p ":GPTEL_MODEL:" text))
              (should-not (string-match-p ":GPTEL_SYSTEM:" text)))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "stabilizes GPTEL_BOUNDS after delegated save resizes the property drawer"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* main\n")
          (setq-local mevedel--session
                      (mevedel-session-create
                       "main"
                       (test-mevedel-session-persistence--make-workspace root)))
          (let ((start (point-marker))
                end
                orig-fun)
            (insert "Assistant body\n")
            (setq end (point-marker))
            (add-text-properties start end '(gptel response))
            (setq orig-fun
                  (lambda ()
                    (org-entry-put (point-min) "GPTEL_BOUNDS"
                                   "((response (1 2)))")))
            (mevedel-session-artifacts--save-gptel-state-around orig-fun)
            (pcase-let ((`((response (,beg ,stored-end)))
                         (read (org-entry-get (point-min) "GPTEL_BOUNDS"))))
              (should (= beg (marker-position start)))
              (should (= stored-end (marker-position end))))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "hides static system prompts while delegating, then strips gptel config"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session
                      (mevedel-session-create
                       "main"
                       (test-mevedel-session-persistence--make-workspace root)))
          (let ((gptel-system-prompt "Custom prompt")
                delegated-system
                system-present-at-delegate
                orig-fun)
            (setq orig-fun
                  (lambda ()
                    (setq delegated-system gptel-system-prompt)
                    (setq system-present-at-delegate
                          (org-entry-get (point-min) "GPTEL_SYSTEM"))))
            (org-entry-put (point-min) "GPTEL_SYSTEM" "Frozen prompt")
            (mevedel-session-artifacts--save-gptel-state-around orig-fun)
            (should-not delegated-system)
            (should (equal "Frozen prompt" system-present-at-delegate))
            (should-not (org-entry-get (point-min) "GPTEL_SYSTEM"))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "rebases a live view when gptel metadata shifts transcript positions"
  (let ((view (generate-new-buffer " *mevedel-save-state-view*"))
        deltas)
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session t)
          (setq-local mevedel--view-buffer view)
          (let ((data (current-buffer)))
            (with-current-buffer view
              (setq-local mevedel--data-buffer data))
            (let ((observer
                   (lambda (event &rest args)
                     (when (eq event 'rebase-data-sources)
                       (apply #'mevedel-view--rebase-data-sources args)))))
              (mevedel-session-control-transfer-register-observer
               t observer)
              (unwind-protect
                  (progn
                    (insert "Transcript body\n")
                    (cl-letf (((symbol-function 'gptel--get-buffer-bounds)
                               (lambda () '((response (1 2)))))
                              ((symbol-function
                                'mevedel-view--rebase-data-sources)
                               (lambda (delta) (push delta deltas))))
                      (mevedel-session-artifacts--save-gptel-state-around
                       (lambda ()
                         (org-entry-put (point-min) "GPTEL_BOUNDS"
                                        "((response (1 2)))"))))
                    (should (= 1 (length deltas)))
                    (should-not (= 0 (car deltas))))
                (mevedel-session-control-transfer-unregister-observer
                 t observer)))))
      (when (buffer-live-p view)
        (kill-buffer view))))
  :doc "strips gptel request-config properties after delegation, keeping bounds"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session
                      (mevedel-session-create
                       "main"
                       (test-mevedel-session-persistence--make-workspace root)))
          (cl-letf (((symbol-function 'gptel--get-buffer-bounds)
                     (lambda () '((response (1 2))))))
            (mevedel-session-artifacts--save-gptel-state-around
             (lambda ()
               ;; Mimic `gptel-org-set-properties' + bounds.
               (org-entry-put (point-min) "GPTEL_BACKEND" "Codex")
               (org-entry-put (point-min) "GPTEL_MODEL" "gpt-5.6-sol")
               (org-entry-put (point-min) "GPTEL_REASONING_EFFORT" "max")
               (org-entry-put (point-min) "GPTEL_PRESET" "mevedel-implement")
               (org-entry-put (point-min) "GPTEL_TOOLS" "Read Bash")
               (org-entry-put (point-min) "GPTEL_BOUNDS"
                              "((response (1 2)))"))))
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p ":GPTEL_BOUNDS: " text))
            (should-not (string-match-p ":GPTEL_BACKEND:" text))
            (should-not (string-match-p ":GPTEL_MODEL:" text))
            (should-not (string-match-p ":GPTEL_REASONING_EFFORT:" text))
            (should-not (string-match-p ":GPTEL_PRESET:" text))
            (should-not (string-match-p ":GPTEL_TOOLS:" text))))
      (when (file-directory-p root)
        (delete-directory root t))))
  :doc "strips partial config writes when the delegated save fails"
  (let ((root (make-temp-file "mevedel-test-proj-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session
                      (mevedel-session-create
                       "main"
                       (test-mevedel-session-persistence--make-workspace root)))
          (cl-letf (((symbol-function 'gptel--get-buffer-bounds)
                     (lambda () nil)))
            (should-error
             (mevedel-session-artifacts--save-gptel-state-around
              (lambda ()
                (org-entry-put (point-min) "GPTEL_MODEL" "stale-model")
                (error "Delegated save failed")))
             :type 'error))
          (should-not (string-match-p ":GPTEL_MODEL:" (buffer-string))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-session-artifacts-strip-gptel-config-properties ()
  ,test
  (test)
  :doc "deletes every gptel config property while keeping GPTEL_BOUNDS"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n"
            ":GPTEL_BACKEND: Codex\n"
            ":GPTEL_MODEL: gpt-5.6-sol\n"
            ":GPTEL_REASONING_EFFORT: max\n"
            ":GPTEL_PRESET: mevedel-implement\n"
            ":GPTEL_SYSTEM: Frozen prompt\n"
            ":GPTEL_TEMPERATURE: 0.7\n"
            ":GPTEL_MAX_TOKENS: 4096\n"
            ":GPTEL_NUM_MESSAGES_TO_SEND: 4\n"
            ":GPTEL_TOOLS: Read Bash\n"
            ":GPTEL_FUTURE2: x\n"
            ":GPTEL_FUTURE-V3: y\n"
            ":GPTEL_FUTURE+V4: z\n"
            ":GPTEL_BOUNDS:EXTRA: w\n"
            ":GPTEL_BOUNDS: ((response (1 2)))\n"
            ":END:\n"
            "Body\n")
    (mevedel-session-artifacts-strip-gptel-config-properties)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p ":GPTEL_BOUNDS: ((response (1 2)))" text))
      (should (string-match-p "Body" text))
      (should-not (string-match-p ":GPTEL_\\(?:BACKEND\\|MODEL\\|REASONING_EFFORT\\|PRESET\\|SYSTEM\\|TEMPERATURE\\|MAX_TOKENS\\|NUM_MESSAGES_TO_SEND\\|TOOLS\\|FUTURE2\\|FUTURE-V3\\|FUTURE\\+V4\\|BOUNDS:EXTRA\\):" text))))
  :doc "no-ops in a buffer without a property drawer"
  (with-temp-buffer
    (org-mode)
    (insert "Body\n")
    (mevedel-session-artifacts-strip-gptel-config-properties)
    (should (equal "Body\n"
                   (buffer-substring-no-properties (point-min) (point-max))))))


(mevedel-deftest mevedel-session-artifacts--file-history-backup-name ()
  ,test
  (test)
  :doc "addresses exact raw bytes including empty and non-text contents"
  (progn
    (should (equal (mevedel-session-artifacts--file-history-backup-name "")
                   "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
    (should (equal (mevedel-session-artifacts--file-history-backup-name "abc")
                   "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
    (should-not (equal (mevedel-session-artifacts--file-history-backup-name
                        (unibyte-string 255))
                       (mevedel-session-artifacts--file-history-backup-name
                        (unibyte-string 254))))))


(mevedel-deftest mevedel-session-artifacts--file-history-latest-version ()
  ,test
  (test)
  :doc "returns 0 for unknown path"
  (let ((session (mevedel-session-create
                  "x" (mevedel-workspace-get-or-create
                       'project "id" "/tmp" "x"))))
    (should (= 0 (mevedel-session-artifacts--file-history-latest-version
                  session "/tmp/foo")))
    (mevedel-workspace-clear-registry))
  :doc "finds max across multiple turn entries"
  (let ((session (mevedel-session-create
                  "x" (mevedel-workspace-get-or-create
                       'project "id2" "/tmp" "x"))))
    (setf (mevedel-session-file-snapshots session)
          '((1 . (("/tmp/foo" . (:backup-name "abc@v1" :version 1))))
            (3 . (("/tmp/foo" . (:backup-name "abc@v3" :version 3))
                  ("/tmp/bar" . (:backup-name "def@v1" :version 1))))
            (2 . (("/tmp/foo" . (:backup-name "abc@v2" :version 2))))))
    (should (= 3 (mevedel-session-artifacts--file-history-latest-version session "/tmp/foo")))
    (should (= 1 (mevedel-session-artifacts--file-history-latest-version session "/tmp/bar")))
    (should (= 0 (mevedel-session-artifacts--file-history-latest-version session "/tmp/baz")))
    (mevedel-workspace-clear-registry)))


(mevedel-deftest mevedel-session-artifacts-snapshot-modified (:quiet t)
  ,test
  (test)
  :doc "writes a pre-turn checkpoint for a modified file"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "foo.el"))
               (pre          (make-hash-table :test #'equal)))
          (write-region "old content" nil tracked-file nil 'silent)
          (puthash tracked-file "old content" pre)
          (write-region "new content" nil tracked-file nil 'silent)
          (let ((written (mevedel-session-artifacts-snapshot-modified
                          session 1 pre)))
            (should (= 2 (length written)))
            (let* ((entry (assoc tracked-file
                                 (cdr (assoc 1 (mevedel-session-file-snapshots
                                                session)))))
                   (backup-name (plist-get (cdr entry) :pre-backup-name))
                   (backup-path (mevedel-session-artifacts-backup-path
                                 (mevedel-session-save-path session)
                                 backup-name)))
              (should backup-name)
              (should (file-exists-p backup-path))
              (with-temp-buffer
                (insert-file-contents-literally backup-path)
                (should (equal "old content" (buffer-string)))))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "shares identical snapshot bytes across files and turns without losing checkpoints"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((paths (list (file-name-concat tempdir "one.bin")
                            (file-name-concat tempdir "two.bin")))
               (before (unibyte-string 0 255 10 13))
               (after (unibyte-string 1 254 13 10))
               (pre (make-hash-table :test #'equal))
               (history (file-name-concat (mevedel-session-save-path session)
                                          "file-history")))
          (dotimes (turn 6)
            (dolist (path paths)
              (puthash path before pre)
              (let ((coding-system-for-write 'no-conversion))
                (write-region after nil path nil 'silent)))
            (dotimes (_ 2)
              (mevedel-session-artifacts-snapshot-modified session (1+ turn) pre))
            (cl-rotatef before after))
          (should (= 6 (length (mevedel-session-file-snapshots session))))
          (dolist (turn (mevedel-session-file-snapshots session))
            (should (= 2 (length (cdr turn))))
            (dolist (entry (cdr turn))
              (should (= (car turn) (plist-get (cdr entry) :version)))
              (cl-loop for key in '(:pre-backup-name :backup-name)
                       for expected in (if (cl-oddp (car turn))
                                           (list before after) (list after before))
                       for path = (mevedel-session-artifacts-backup-path
                                   (mevedel-session-save-path session)
                                   (plist-get (cdr entry) key))
                       do (should (equal expected
                                         (mevedel-session-artifacts-read-file-raw path)))
                       do (should (= #o600 (file-modes path))))))
          (should (= 2 (length (directory-files history nil "^[^.].*")))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "records an empty checkpoint when tracked files are unchanged"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "foo.el"))
               (pre          (make-hash-table :test #'equal)))
          (write-region "same content" nil tracked-file nil 'silent)
          (puthash tracked-file "same content" pre)
          (let ((written (mevedel-session-artifacts-snapshot-modified
                          session 1 pre)))
            (should (null written))
            (should (assoc 1 (mevedel-session-file-snapshots session)))
            (should-not
             (cdr (assoc 1 (mevedel-session-file-snapshots session))))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "records prior content when a file is deleted during the turn"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "gone.el"))
               (pre          (make-hash-table :test #'equal)))
          (puthash tracked-file "had content" pre)
          (mevedel-session-artifacts-snapshot-modified session 2 pre)
          (let* ((entry (assoc tracked-file
                               (cdr (assoc 2 (mevedel-session-file-snapshots
                                              session))))))
            (should entry)
            (should (plist-get (cdr entry) :pre-backup-name))
            (should-not (plist-get (cdr entry) :backup-name))
            (should
             (equal
              "had content"
              (mevedel-session-artifacts--file-text
               (mevedel-session-artifacts-backup-path
                (mevedel-session-save-path session)
                (plist-get (cdr entry) :pre-backup-name)))))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "records an absent checkpoint for a file created during the turn"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "new.el"))
               (pre          (make-hash-table :test #'equal)))
          (puthash tracked-file nil pre)
          (write-region "fresh" nil tracked-file nil 'silent)
          (mevedel-session-artifacts-snapshot-modified session 3 pre)
          (let* ((entry (assoc tracked-file
                               (cdr (assoc 3 (mevedel-session-file-snapshots
                                              session))))))
            (should entry)
            (should (plist-get (cdr entry) :backup-name))
            (should-not (plist-get (cdr entry) :pre-backup-name))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "records files exceeding the size cap as checkpoint gaps"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((tracked-file (file-name-concat tempdir "huge.el"))
               (pre          (make-hash-table :test #'equal))
               (mevedel-file-history-max-snapshot-bytes 10))
          (puthash tracked-file (make-string 100 ?x) pre)
          (write-region "changed" nil tracked-file nil 'silent)
          (let ((written (mevedel-session-artifacts-snapshot-modified
                          session 1 pre)))
            (should (null written))
            (should
             (string-match-p
              "exceeds"
              (plist-get
               (cdr
                (assoc tracked-file
                       (cdr (assoc
                             1
                             (mevedel-session-file-snapshots session)))))
               :gap)))))
      (test-mevedel-session-persistence--cleanup tempdir))))


;;
;;; Phase 4: split-on-compact

(mevedel-deftest mevedel-session-artifacts-rotate-segment ()
  ,test
  (test)
  :doc "creates a new segment file and bumps the segment counter"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (_orig-segment buffer-file-name)
               notified
               (new-path
                (cl-letf (((symbol-function 'mevedel-collaboration-notify-history-changed)
                           (lambda (buffer)
                             (setq notified (list buffer (mevedel-session-current-segment session))))))
                  (mevedel-session-artifacts-rotate-segment
                   session buf "Summary of the prior conversation."))))
          (should (equal notified (list buf 2)))
          (with-current-buffer buf
            (should new-path)
            (should (= 2 (mevedel-session-current-segment session)))
            (should (file-exists-p new-path))
            ;; Old segment file still exists.
            (let ((seg1 (mevedel-session-artifacts-segment-path
                         (mevedel-session-save-path session) 1)))
              (should (file-exists-p seg1))
              ;; Old segment got finalized property
              (with-temp-buffer
                (insert-file-contents seg1)
                (should (string-match-p "MEVEDEL_SEGMENT_FINALIZED_AT"
                                        (buffer-string)))))
            ;; New buffer points at the new segment file.
            (should (file-equal-p new-path buffer-file-name))
            ;; Buffer body contains the summary.
            (should (string-match-p "Summary of the prior conversation."
                                    (buffer-string)))
            ;; Buffer also contains the segment-2 number property.
            (should (string-match-p "MEVEDEL_SEGMENT_NUMBER:[ \t]*2"
                                    (buffer-string)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "refreshes matching stale visited modtime before editing"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (set-file-times buffer-file-name (time-add (current-time) 5))
            (should-not (verify-visited-file-modtime buf)))
          (should (mevedel-session-artifacts-rotate-segment
                   session buf "Summary after stale modtime."))
          (with-current-buffer buf
            (should (verify-visited-file-modtime buf))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "refreshes matching stale visited modtime before deleting pending text"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (set-visited-file-modtime)
            (set-buffer-modified-p nil)
            (goto-char (point-max))
            (insert "\nPending prompt")
            (set-file-times buffer-file-name (time-add (current-time) 5))
            (should-not (verify-visited-file-modtime buf)))
                  (cl-letf (((symbol-function 'ask-user-about-supersession-threat)
                             (lambda (&rest _args)
                               (error "Supersession prompt"))))
            (should (mevedel-session-artifacts-rotate-segment
                     session buf "Summary after stale pending text."
                     :pending-text "\nPending prompt")))
          (with-current-buffer buf
            (should (string-suffix-p "Pending prompt\n" (buffer-string)))
            (should (verify-visited-file-modtime buf))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "noninteractive publication:
rotation never saves through a rebound temporary visited filename or prompts"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (original-save-buffer (symbol-function 'save-buffer))
               new-path)
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "unsaved transcript\n"))
          (cl-letf (((symbol-function 'ask-user-about-supersession-threat)
                     (lambda (&rest _) (error "Supersession prompt")))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (error "yes-or-no prompt")))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) (error "y-or-n prompt")))
                    ((symbol-function 'save-buffer)
                     (lambda (&rest args)
                       (when (and buffer-file-name
                                  (string-suffix-p ".tmp" buffer-file-name))
                         (error "Temporary visited-file save"))
                       (apply original-save-buffer args))))
            (setq new-path
                  (mevedel-session-artifacts-rotate-segment
                   session buf "Noninteractive summary.")))
          (with-current-buffer buf
            (should (file-equal-p new-path buffer-file-name))
            (should (equal (file-truename new-path) buffer-file-truename))
            (should (verify-visited-file-modtime buf))
            (should-not (buffer-modified-p))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "signals a controlled error when current segment differs on disk"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (write-region "external edit\n" nil buffer-file-name nil 'silent)
            (should-not (verify-visited-file-modtime buf)))
          (should-error
           (mevedel-session-artifacts-rotate-segment
            session buf "Summary should not be written.")
           :type 'error))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "signals a controlled error when current segment was deleted on disk"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (delete-file buffer-file-name)
            (should-not (file-exists-p buffer-file-name)))
          (should-error
           (mevedel-session-artifacts-rotate-segment
            session buf "Summary should not be written.")
           :type 'error)
          (should (= 1 (mevedel-session-current-segment session))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "sidecar reflects bumped current-segment after rotation"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (mevedel-session-artifacts-rotate-segment
           session buf "First summary.")
          (let ((plist (mevedel-session-codec-read
                        (mevedel-session-artifacts-sidecar-path
                         (mevedel-session-save-path session)))))
            (should (= 2 (plist-get plist :current-segment)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "two consecutive rotations produce three segment files"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (mevedel-session-artifacts-rotate-segment session buf "Summary A.")
          (mevedel-session-artifacts-rotate-segment session buf "Summary B.")
          (let ((dir (mevedel-session-save-path session)))
            (should (file-exists-p
                     (mevedel-session-artifacts-segment-path dir 1)))
            (should (file-exists-p
                     (mevedel-session-artifacts-segment-path dir 2)))
            (should (file-exists-p
                     (mevedel-session-artifacts-segment-path dir 3)))
            (should (= 3 (mevedel-session-current-segment session)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "no-op when session is not materialized"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "no-mat" "/tmp/x" "x"))
         (session (mevedel-session-create "main" workspace))
         (buf     (generate-new-buffer "*test-rotate-buf*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (should (null (mevedel-session-artifacts-rotate-segment
                         session buf "Won't happen."))))
      (kill-buffer buf)
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-artifacts-start-fresh-segment ()
  ,test
  (test)
  :doc "creates an empty new segment without a compaction summary"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (_baseline
                (with-current-buffer buf
                  (setq-local mevedel-compact-estimation--known-token-baseline
                              (list :tokens 258387
                                    :position (copy-marker (point-max))))))
               (new-path (mevedel-session-artifacts-start-fresh-segment
                          session buf :initial-text "### ")))
          (with-current-buffer buf
            (should new-path)
            (should-not mevedel-compact-estimation--known-token-baseline)
            (should (= 2 (mevedel-session-current-segment session)))
            (should (file-exists-p new-path))
            (should (file-equal-p new-path buffer-file-name))
            (should (string-match-p "MEVEDEL_SEGMENT_NUMBER:[ \t]*2"
                                    (buffer-string)))
            (should (string-suffix-p "### " (buffer-string)))
            (should-not (string-match-p "#\\+begin_summary"
                                        (buffer-string)))
            (with-temp-buffer
              (insert-file-contents new-path)
              (should-not (string-match-p "### " (buffer-string))))
            (let ((seg1 (mevedel-session-artifacts-segment-path
                         (mevedel-session-save-path session) 1)))
              (should (file-exists-p seg1))
              (with-temp-buffer
                (insert-file-contents seg1)
                (should (string-match-p "Initial prompt"
                                        (buffer-string)))
                (should (string-match-p "MEVEDEL_SEGMENT_FINALIZED_AT"
                                        (buffer-string)))))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "refreshes matching stale visited modtime before fresh segment edit"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (set-file-times buffer-file-name (time-add (current-time) 5))
            (should-not (verify-visited-file-modtime buf)))
          (should (mevedel-session-artifacts-start-fresh-segment
                   session buf :initial-text "### "))
          (with-current-buffer buf
            (should (verify-visited-file-modtime buf))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "sidecar and prompt index point at the new empty segment"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (mevedel-session-artifacts-start-fresh-segment
           session buf :initial-text "### ")
          (let ((plist (mevedel-session-codec-read
                        (mevedel-session-artifacts-sidecar-path
                         (mevedel-session-save-path session)))))
            (should (= 2 (plist-get plist :current-segment)))
            (should-not (assoc 2 (plist-get plist :prompt-index)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "refreshes finalized segment prompt index before bumping segment"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (setf (mevedel-session-prompt-index session)
                '((1 . ((:turn 1 :pos 999 :preview "stale")))))
          (with-current-buffer buf
            (goto-char (point-max))
            (let ((response-start (point)))
              (insert "\nAssistant response\n")
              (put-text-property response-start (point) 'gptel 'response))
            (insert "\nFresh unsaved prompt\n"))
          (mevedel-session-artifacts-start-fresh-segment
           session buf :initial-text "### ")
          (let* ((plist (mevedel-session-codec-read
                         (mevedel-session-artifacts-sidecar-path
                          (mevedel-session-save-path session))))
                 (seg1 (cdr (assoc 1 (plist-get plist :prompt-index)))))
            (should seg1)
            (should (equal "Initial prompt" (plist-get (car seg1) :preview)))
            (should (equal "Fresh unsaved prompt"
                           (plist-get (cadr seg1) :preview)))
            (should-not (equal 999 (plist-get (car seg1) :pos)))))
      (test-mevedel-session-persistence--cleanup tempdir)))
  :doc "no-op when session is not materialized"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "fresh-no-mat" "/tmp/x" "x"))
         (session (mevedel-session-create "main" workspace))
         (buf     (generate-new-buffer "*test-fresh-buf*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (should (null (mevedel-session-artifacts-start-fresh-segment
                         session buf :initial-text "### "))))
      (kill-buffer buf)
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-artifacts-rotate-segment-rollback ()
  ,test
  (test)
  :doc "rolls live buffer and segment counter back on sidecar write failure"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (old-segment (with-current-buffer buf buffer-file-name))
               (old-text (with-current-buffer buf
                           (buffer-substring (point-min) (point-max)))))
                  (cl-letf (((symbol-function 'mevedel-session-codec-write)
                             (lambda (&rest _)
                               (error "Sidecar write failed"))))
            (should-error
             (mevedel-session-artifacts-rotate-segment
              session buf "Summary that will not commit.")))
          (with-current-buffer buf
            (should (= 1 (mevedel-session-current-segment session)))
            (should (file-equal-p old-segment buffer-file-name))
            (should (equal old-text
                           (buffer-substring (point-min) (point-max)))))
          (should-not
           (file-exists-p
            (mevedel-session-artifacts-segment-path
             (mevedel-session-save-path session) 2))))
      (test-mevedel-session-persistence--cleanup tempdir)))

  :doc "retains committed rotation when instructions fail after sidecar publish"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (sidecar (mevedel-session-artifacts-sidecar-path
                         (mevedel-session-save-path session))))
          (cl-letf (((symbol-function
                      'mevedel-session-artifacts-save-instructions)
                             (lambda (&rest _)
                               (error "Instruction save failed"))))
            (should-error
             (mevedel-session-artifacts-rotate-segment
              session buf "Summary that will not commit.")))
          (let ((plist (mevedel-session-codec-read sidecar)))
            (should (= 2 (mevedel-session-current-segment session)))
            (should (= 2 (plist-get plist :current-segment)))
            (should (equal (mevedel-session-prompt-index session)
                           (plist-get plist :prompt-index))))
          (with-current-buffer buf
            (should
             (file-equal-p
              (mevedel-session-artifacts-segment-path
               (mevedel-session-save-path session) 2)
              buffer-file-name)))
          (should
           (file-exists-p
            (mevedel-session-artifacts-segment-path
             (mevedel-session-save-path session) 2))))
      (test-mevedel-session-persistence--cleanup tempdir)))

  :doc "restores pending prompt when predecessor save fails"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let* ((buf (get-buffer "*test-data-buf*"))
               (old-segment (with-current-buffer buf buffer-file-name)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "Pending prompt\n"))
            (set-buffer-modified-p t))
                  (cl-letf (((symbol-function 'save-buffer)
                             (lambda (&rest _)
                               (error "Save failed"))))
            (should-error
             (mevedel-session-artifacts-rotate-segment
              session buf "Summary."
              :pending-text "Pending prompt\n")))
          (with-current-buffer buf
            (should (= 1 (mevedel-session-current-segment session)))
            (should (file-equal-p old-segment buffer-file-name))
            (should (string-match-p "Pending prompt" (buffer-string)))))
      (test-mevedel-session-persistence--cleanup tempdir))))


(mevedel-deftest mevedel-session-artifacts-finalized-index-rollback ()
  ,test
  (test)
  :doc "failed rotation and fresh transitions restore index and predecessor bytes"
  (dolist (portable '(nil t))
    (dolist (fresh '(nil t))
     (dolist (interruption '(error quit))
      (let* ((root (make-temp-file "mevedel-index-rollback-" t))
             (workspace (if portable
                            (test-mevedel-session-persistence--make-workspace root)
                          (test-mevedel-session-persistence--make-file-workspace root)))
             (session (mevedel-session-create "main" workspace))
             (buffer (generate-new-buffer " *index-rollback*")))
        (unwind-protect
            (with-current-buffer buffer
              (org-mode)
              (setq-local mevedel--session session)
              (insert "Request\n" (propertize "Completed response\n" 'gptel 'response))
              (setf (mevedel-session-turn-count session) 1)
              (mevedel-session-artifacts-save session buffer t)
              (let ((index (copy-tree (mevedel-session-prompt-index session)))
                    (bytes (mevedel-session-artifacts-read-artifact
                            session "segment-0001.chat.org" t))
                    (head (plist-get (mevedel-session-publication session) :head))
                    (failure (if portable
                                 'mevedel-session-artifacts--instruction-artifacts
                               'mevedel-session-codec-write)))
                ;; Both failures occur after the finalized index was prepared;
                ;; the file failure also rolls back finalized predecessor bytes.
                (cl-letf (((symbol-function failure)
                           (lambda (&rest _) (signal interruption nil))))
                  (should
                   (eq interruption
                       (condition-case nil
                           (progn
                             (if fresh
                                 (mevedel-session-artifacts-start-fresh-segment session buffer)
                               (mevedel-session-artifacts-rotate-segment session buffer "Summary"))
                             'returned)
                         (error 'error)
                         (quit 'quit)))))
                (should (= 1 (mevedel-session-current-segment session)))
                (should (equal index (mevedel-session-prompt-index session)))
                (should (equal bytes (mevedel-session-artifacts-read-artifact
                                      session "segment-0001.chat.org" t)))
                (should (equal head (plist-get (mevedel-session-publication session) :head)))
                (let ((sidecar (mevedel-session-codec-read
                                (mevedel-session-artifacts-sidecar-path
                                 (mevedel-session-save-path session)))))
                  (should (= 1 (plist-get sidecar :current-segment)))
                  (should (equal index (plist-get sidecar :prompt-index))))))
          (test-mevedel-session-persistence--release-and-kill buffer session)
          (delete-directory root t)
          (mevedel-workspace-clear-registry)))))))

(mevedel-deftest mevedel-session-artifacts-rotate-segment-quit (:quiet t)
  ,test
  (test)
  :doc "quit retains committed file or portable transitions and pending portable publication"
  (dolist (phase '(file-committed portable-committed portable-pending))
    (let* ((portable (not (eq phase 'file-committed)))
           (root (make-temp-file "mevedel-rotation-quit-" t))
           (workspace (if portable
                          (test-mevedel-session-persistence--make-workspace root)
                        (test-mevedel-session-persistence--make-file-workspace root)))
           (session (mevedel-session-create "main" workspace))
           (buffer (generate-new-buffer " *rotation-quit*")))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--session session)
            (insert "Request\n" (propertize "Completed response\n" 'gptel 'response))
            (setf (mevedel-session-turn-count session) 1)
            (mevedel-session-artifacts-save session buffer t)
            (goto-char (point-max))
            (insert "\nPending request\n")
            (let* ((head (plist-get (mevedel-session-publication session) :head))
                   (transition (symbol-function
                                'mevedel-session-artifacts--publish-remote-segment-transition))
                   (seam (if portable
                             'mevedel-session-artifacts--publish-remote-segment-transition
                           'mevedel-session-artifacts-save-instructions)))
              (cl-letf (((symbol-function seam)
                         (lambda (&rest args)
                           (should inhibit-quit)
                           (when portable
                             (if (eq phase 'portable-pending)
                                 (cl-letf (((symbol-function
                                             'mevedel-session-publication--commit-marker-publication)
                                            (lambda (&rest _) (error "Injected publication failure"))))
                                   (condition-case nil (apply transition args) (error nil)))
                               (apply transition args)))
                           (signal 'quit nil))))
                (should (eq 'quit
                            (condition-case nil
                                (progn
                                  (mevedel-session-artifacts-rotate-segment
                                   session buffer "Summary" :pending-text "\nPending request\n")
                                  'returned)
                              (quit 'quit)))))
              (should (= 2 (mevedel-session-current-segment session)))
              (should (equal buffer-file-name
                             (mevedel-session-artifacts-segment-path
                              (mevedel-session-save-path session) 2)))
              (should (= 2 (length (split-string (buffer-string) "Pending request"))))
              (if (eq phase 'portable-pending)
                  (progn
                    (should (mevedel-session-pending-publication session))
                    (should (equal head (plist-get (mevedel-session-publication session) :head)))
                    (mevedel-session-publication-retry session))
                (should-not (mevedel-session-pending-publication session)))
              (let* ((index (mevedel-session-prompt-index session))
                     (sidecar (mevedel-session-codec-read
                               (mevedel-session-artifacts-sidecar-path
                                (mevedel-session-save-path session))))
                     (prompt (cadr (assoc 1 index)))
                     (evidence (mevedel-journal-evidence-turns
                                session
                                (list (list :number 1 :segment 1
                                            :fork-point (plist-get prompt :fork-point-id)
                                            :start (plist-get prompt :pos)
                                            :end (plist-get prompt :transcript-cutoff))))))
                (should (= 2 (plist-get sidecar :current-segment)))
                (should (equal index (plist-get sidecar :prompt-index)))
                (should (string-search "Completed response" (plist-get evidence :text)))
                (should-not (string-search "Pending request" (plist-get evidence :text))))))
        (test-mevedel-session-persistence--release-and-kill buffer session)
        (delete-directory root t)
        (mevedel-workspace-clear-registry)))))

(mevedel-deftest mevedel-session-artifacts-segment-summary-bounds ()
  ,test
  (test)
  :doc "rejects summary markers inside a long conversation after metadata"
  (with-temp-buffer
    (insert ":PROPERTIES:\n:END:\n" (make-string 140000 ?x)
            "\n#+begin_summary\nQuoted summary\n#+end_summary\n")
    (should-not (mevedel-session-artifacts-segment-summary-bounds)))
  :doc "accepts leading summaries with whitespace and large property drawers"
  (dolist (prefix (list "" " \t\n"
                       (concat "\n:PROPERTIES:\n:VALUE: "
                               (make-string 140000 ?x) "\n:END:\n\n")))
    (with-temp-buffer
      (insert prefix "#+begin_summary\nSummary\n#+end_summary\n")
      (let* ((position (point))
             (bounds (mevedel-session-artifacts-segment-summary-bounds)))
        (should (= (point) position))
        (should (= (plist-get bounds :begin) (1+ (length prefix))))
        (should (equal (buffer-substring-no-properties
                        (plist-get bounds :body-begin)
                        (plist-get bounds :body-end))
                       "Summary\n"))
        (should (= (plist-get bounds :end) (1- (point-max)))))))
  :doc "rejects missing delimiters and content after the first drawer"
  (dolist (text '(":PROPERTIES:\n#+begin_summary\nSummary\n#+end_summary\n"
                  "#+begin_summary\nSummary\n"
                  ":PROPERTIES:\n:END:\nConversation\n:END:\n#+begin_summary\nSummary\n#+end_summary\n"))
    (with-temp-buffer
      (insert text)
      (should-not (mevedel-session-artifacts-segment-summary-bounds)))))

(mevedel-deftest mevedel-session-artifacts-summary-block ()
  ,test
  (test)
  :doc "wraps summary in #+begin_summary block"
  (let ((wrapped (mevedel-session-artifacts-summary-block "hello")))
    (should (string-match-p "#\\+begin_summary" wrapped))
    (should (string-match-p "#\\+end_summary" wrapped))
    (should (string-match-p "Another language model started" wrapped))
    (should (string-match-p "hello" wrapped)))
  :doc "marker lines carry gptel ignore property"
  (let ((wrapped (mevedel-session-artifacts-summary-block "x")))
    ;; The first character is in the begin_summary marker.
    (should (eq 'ignore (get-text-property 0 'gptel wrapped)))))


(mevedel-deftest mevedel-session-artifacts-strip-summary-handoff-prefix ()
  ,test
  (test)
  :doc "removes the model-facing handoff prefix before summary reuse"
  (let* ((summary "## Goal\n- continue")
         (prefixed (concat mevedel-session-artifacts--summary-handoff-prefix
                           summary)))
    (should (equal summary
                   (mevedel-session-artifacts-strip-summary-handoff-prefix
                    prefixed)))
    (should (equal summary
                   (mevedel-session-artifacts-strip-summary-handoff-prefix
                    summary)))))


(mevedel-deftest mevedel-session-artifacts-rotate-segment-tail ()
  ,test
  (test)
  :doc "rotates into summary followed by preserved tail and pending prompt"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (mevedel-session-artifacts-rotate-segment
           session buf "Summary."
           :tail-text "Tail turn.\n"
           :pending-text "Pending prompt.\n")
          (with-current-buffer buf
            (let ((text (buffer-string)))
              (should (string-match-p "#\\+begin_summary mevedel-role=compaction-summary" text))
              (should (string-match-p "Summary\\." text))
              (should (string-match-p "Tail turn\\." text))
              (should (string-match-p "Pending prompt\\." text)))))
      (test-mevedel-session-persistence--cleanup tempdir))))


(mevedel-deftest mevedel-session-artifacts-rotate-segment-pending-save ()
  ,test
  (test)
  :doc "pending prompts are not saved before request completion"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "Old prompt\n")
              (insert (propertize "Old response\n" 'gptel 'response))
              (insert "Pending prompt\n"))
            (set-buffer-modified-p t))
          (mevedel-session-artifacts-rotate-segment
           session buf "Summary."
           :pending-text "Pending prompt\n")
          (let ((seg1 (mevedel-session-artifacts-segment-path
                       (mevedel-session-save-path session) 1))
                (seg2 (mevedel-session-artifacts-segment-path
                       (mevedel-session-save-path session) 2)))
            (with-temp-buffer
              (insert-file-contents seg1)
              (should-not (string-match-p "Pending prompt" (buffer-string))))
            (with-temp-buffer
              (insert-file-contents seg2)
              (should-not (string-match-p "Pending prompt" (buffer-string))))
            (with-current-buffer buf
              (should (string-match-p "Pending prompt" (buffer-string)))
              (should-not (buffer-modified-p)))))
      (test-mevedel-session-persistence--cleanup tempdir))))


(mevedel-deftest mevedel-session-artifacts-rotate-segment-completed-evidence ()
  ,test
  (test)
  :doc "rotation and fresh segments index completed evidence at finalized positions"
  (dolist (portable '(nil t))
    (dolist (fresh '(nil t))
      (let* ((tempdir (make-temp-file "mevedel-rotation-evidence-" t))
             (workspace (if portable
                            (test-mevedel-session-persistence--make-workspace tempdir)
                          (test-mevedel-session-persistence--make-file-workspace tempdir)))
             (session (mevedel-session-create "main" workspace))
             (buf (generate-new-buffer " *rotation-evidence*")))
        (unwind-protect
            (with-current-buffer buf
              (org-mode)
              (setq-local mevedel--session session)
              (insert "Initial prompt\n")
              (insert (propertize "Working\n" 'gptel 'response))
              (insert "#+begin_tool (Read :file_path \"big.txt\")\n")
              (let ((tool-start (point)))
                (insert "(:name \"Read\" :args (:file_path \"big.txt\"))\n\n")
                (insert (make-string 20000 ?x) "\n#+end_tool\n")
                (put-text-property tool-start (point) 'gptel '(tool . "call-1")))
              (setf (mevedel-session-turn-count session) 1)
              (mevedel-session-artifacts-save session buf t)
              (let* ((before (cadr (assoc 1 (mevedel-session-prompt-index session))))
                     (id (plist-get before :fork-point-id)))
                (should id)
                (if fresh
                    (mevedel-session-artifacts-start-fresh-segment
                     session buf :initial-text "Pending prompt\n")
                  (goto-char (point-max))
                  (insert "\nPending prompt\n")
                  (mevedel-session-artifacts-rotate-segment
                   session buf "Summary."
                   :pending-text (buffer-substring
                                  (mevedel-compact-evidence-find-boundary) (point-max))))
                (let ((indexed (cadr (assoc 1 (mevedel-session-prompt-index session))))
                      (sidecar (mevedel-session-codec-read
                                (mevedel-session-artifacts-sidecar-path
                                 (mevedel-session-save-path session)))))
                  (should (equal (mevedel-session-prompt-index session)
                                 (plist-get sidecar :prompt-index)))
                  (let ((evidence
                         (mevedel-journal-evidence-turns
                          session
                          (list (list :number 1 :segment 1 :fork-point id
                                      :start (plist-get indexed :pos)
                                      :end (plist-get indexed :transcript-cutoff))))))
                    (should (string-search "Working" (plist-get evidence :text)))
                    (should (string-search (make-string 80 ?x) (plist-get evidence :text)))
                    (should-not (string-search "Pending prompt" (plist-get evidence :text))))
                  (with-temp-buffer
                    (insert (decode-coding-string
                             (mevedel-session-artifacts-read-artifact
                              session "segment-0001.chat.org" t)
                             'utf-8-unix))
                    (org-mode)
                    (mevedel-transcript-restore-properties)
                    (let ((actual (car (mevedel-session-artifacts-fork-point-spans
                                        (current-buffer)))))
                      (should (equal id (plist-get actual :fork-point-id)))
                      (should (= (plist-get indexed :transcript-cutoff)
                                 (plist-get actual :transcript-cutoff)))
                      (should (= (plist-get indexed :pos)
                                 (plist-get (car (mevedel-session-artifacts-collect-prompts
                                                  (current-buffer))) :pos))))
                    (should-not (string-search "Pending prompt" (buffer-string)))))))
          (test-mevedel-session-persistence--release-and-kill buf session)
          (delete-directory tempdir t)
          (mevedel-workspace-clear-registry))))))

(mevedel-deftest mevedel-session-artifacts-rotate-segment-tail-index ()
  ,test
  (test)
  :doc "copied tail prompts do not consume new cumulative turn ids"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (unwind-protect
        (let ((buf (get-buffer "*test-data-buf*"))
              (tail-text
               (concat
                "Tail prompt 1\n"
                (propertize "Tail response 1\n" 'gptel 'response)
                "Tail prompt 2\n"
                (propertize "Tail response 2\n" 'gptel 'response))))
          (setf (mevedel-session-turn-count session) 10)
          (with-current-buffer buf
            (erase-buffer)
            (dotimes (turn 10)
              (insert (format "Prompt %d\n" (1+ turn)))
              (insert (propertize "Response\n" 'gptel 'response)))
            (mevedel-session-artifacts-update-prompt-index session buf))
          (mevedel-session-artifacts-rotate-segment
           session buf "Summary."
           :tail-text tail-text
           :pending-text "Next real prompt\n")
          (mevedel-session-artifacts-update-prompt-index session buf)
          (let ((seg2 (cdr (assoc 2 (mevedel-session-prompt-index session)))))
            (should (= 1 (length seg2)))
            (should (= 1 (plist-get (car seg2) :turn)))
            (should (= 3 (plist-get (car seg2) :file-turn)))
            (should (= 11 (plist-get (car seg2) :cum-turn)))
            (should (equal "Next real prompt"
                           (plist-get (car seg2) :preview)))))
      (test-mevedel-session-persistence--cleanup tempdir))))


(mevedel-deftest mevedel-session-artifacts-ensure-files-acquires-lock ()
  ,test
  (test)
  :doc "lazy materialization writes the .lock file"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (insert "Hi\n")
                (let ((path (mevedel-session-artifacts-ensure-files
                             session buf)))
                  (should (file-exists-p
                           (mevedel-session-persistence--lock-path path)))))
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-artifacts-project-materialization ()
  ,test
  (test)
  :doc "materializes local project authority as lease and publication without a lock"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-local-project-authority-" t)))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (should
           (mevedel-session-artifacts-assert-mutation-authority
            session (current-buffer)))
          (let ((save-path (mevedel-session-save-path session)))
            (should save-path)
            (should (file-directory-p
                     (file-name-concat save-path ".lease")))
            (should-not (file-exists-p
                         (file-name-concat save-path ".lock")))
            (should
             (mevedel-session-publication-publish
              session
              (list
               (list :path (file-name-concat save-path "session.meta.el")
                     :content "(:version \"v0.5.2\")"
                     :commit-marker t))))
            (should (mevedel-session-publication-read save-path))
            (let ((other-client (make-string 64 ?b))
                  (other (mevedel-session-create "main" workspace)))
              (setf (mevedel-session-save-path other) save-path)
              (let ((mevedel-session-durability--client-id other-client))
                (with-temp-buffer
                  (setq-local mevedel--session other)
                  (should-error
                   (mevedel-session-artifacts-assert-mutation-authority
                    other (current-buffer))
                   :type 'user-error))))
            (mevedel-session-persistence-lock-release save-path session)))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-artifacts-assert-mutation-authority ()
  ,test
  (test)
  :doc "fences remote file-session grants after target replacement"
  (let* ((host "file-incarnation")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-file-incarnation-" t)))
         (remote-root (format "/mevedelmock:%s:%s" host local-root))
         session buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) t)))
            (mevedel-workspace-identity-ensure remote-root)
            (let* ((workspace
                    (mevedel-workspace-get-or-create
                     'file "remote-file" remote-root "remote-file"))
                 (_ (setq session (mevedel-session-create "main" workspace)))
                 (target (mevedel-session-execution-target session))
                 (grant (file-name-concat remote-root "granted.txt")))
            (setq buffer (generate-new-buffer " *remote-file-incarnation*"))
            (with-current-buffer buffer
              (org-mode)
              (setq-local mevedel--workspace workspace
                          mevedel--session session)
              (setq default-directory remote-root)
              (insert "Remote file session\n")
              (should
               (mevedel-session-artifacts-save session buffer t))
              (set-buffer-modified-p nil))
            (write-region "grant\n" nil grant nil 'silent)
            (setf (mevedel-session-resource-grants session)
                  (list (list :path grant :access 'read)))
            (with-current-buffer buffer
              (mevedel-session-artifacts-save session buffer t)
              (set-buffer-modified-p nil))
            (mevedel-execution-target--record-incarnation
             target "replacement-incarnation")
            (with-current-buffer buffer
              (should
               (mevedel-session-artifacts-assert-mutation-authority
                session buffer)))
            (should-not (mevedel-session-resource-grants session))
            (should-not
             (mevedel-execution-target-incarnation-changed-p target))
            (let ((sidecar
                   (mevedel-session-codec-read
                    (mevedel-session-artifacts-sidecar-path
                     (mevedel-session-save-path session)))))
              (should
               (equal "replacement-incarnation"
                      (plist-get sidecar :target-incarnation)))
              (should-not (plist-get sidecar :resource-grants))))))
      (when (and session (mevedel-session-save-path session))
        (ignore-errors
          (mevedel-session-persistence-lock-release
           (mevedel-session-save-path session) session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-artifacts-mixed-authority-controls ()
  ,test
  (test)
  :doc "surfaces mixed controls as incompatible while restore and admission reject them"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-mixed-authority-" t)))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session-dir
          (file-name-as-directory
           (file-name-concat root ".mevedel" "sessions" "mixed")))
         (session (mevedel-session-create "mixed" workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-concat session-dir ".lease") t)
          (with-temp-file (file-name-concat session-dir ".lock")
            (insert "mixed controls"))
          (setf (mevedel-session-save-path session) session-dir)
          (should-error
           (mevedel-session-codec-authority-mode-for-path session-dir)
           :type 'error)
          (with-temp-buffer
            (setq-local mevedel--session session)
            (should-error
             (mevedel-session-artifacts-assert-mutation-authority
              session (current-buffer))
             :type 'error))
          (let ((listing
                 (mevedel-session-persistence--enumerate-sessions workspace)))
            (should-not (plist-get listing :sessions))
            (should
             (eq 'undiscoverable
                 (plist-get (car (plist-get listing :incompatible))
                            :status))))
          (should-error
           (mevedel-session-persistence-restore
            session-dir nil nil workspace)
           :type 'error))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-artifacts-transfer-admission ()
  ,test
  (test)
  :doc "blocks new mutation admission while preserving a quiet drain"
  (let ((session (mevedel-session--create :name "transfer-admission"))
        (ordinary-called nil))
    (setf (mevedel-session-control-transfer session)
          '(:state quiescing))
    (cl-letf (((symbol-function
                'mevedel-session-artifacts-assert-mutation-authority)
               (lambda (&rest _) (setq ordinary-called t))))
      (should-error
       (mevedel-session-artifacts-assert-new-mutation-authority session)
       :type 'user-error)
      (should-not ordinary-called))))


(mevedel-deftest mevedel-session-artifacts-read-artifact ()
  ,test
  (test)
  :doc "reads local artifacts from their fixed logical paths"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "mevedel-local-artifact-resolver-" t)))
         (workspace
          (test-mevedel-session-persistence--make-file-workspace root))
         (session (mevedel-session-create "resolver" workspace))
         (session-dir
          (file-name-as-directory (file-name-concat root "session")))
         (segment (file-name-concat session-dir "segment-0001.chat.org")))
    (make-directory session-dir t)
    (setf (mevedel-session-save-path session) session-dir
          (mevedel-session-publication session)
          '(:artifacts
            (("segment-0001.chat.org"
              :published "/must/not/be/read"
              :sha256
              "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))
    (unwind-protect
        (progn
          (write-region "fixed" nil segment nil 'silent)
          (should
           (equal "fixed"
                  (mevedel-session-artifacts-read-artifact
                   session "segment-0001.chat.org")))
          (should-error
           (mevedel-session-artifacts-read-artifact session "../escape")))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry)))
  :doc "resolves remote staged owner bytes or verified committed bytes"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "mevedel-artifact-resolver-" t)))
         (host "artifact-resolver")
         (mevedel-session-durability--client-id (make-string 64 ?a)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host root "fixed cache"))
                       (sidecar
                        (mevedel-session-artifacts-sidecar-path session-dir)))
            (unwind-protect
                (progn
                  (should
                   (mevedel-session-durability-lease-acquire
                    session-dir "*resolver*" session))
                  (should
                   (mevedel-session-publication-publish
                    session
                    (list (list :path segment :content "committed")
                          (list :path sidecar :content "sidecar"
                                :commit-marker t))))
                  (write-region "stale fixed cache" nil segment nil 'silent)
                  (let ((checks 0))
                    (cl-letf (((symbol-function 'mevedel-session-durability-lease-owned-p)
                               (lambda (_) (cl-incf checks) t)))
                      (should (mevedel-session-artifacts-artifact-present-p
                               session "segment-0001.chat.org"))
                      (should (equal "committed"
                                     (mevedel-session-artifacts-read-artifact
                                      session "segment-0001.chat.org"))))
                    (should (zerop checks)))
                  (should
                   (equal "committed"
                          (mevedel-session-artifacts-read-artifact
                           session "segment-0001.chat.org" t)))
                  (should
                   (mevedel-session-publication-publish
                    session (list (list :path segment :content "staged"))))
                  (should
                   (equal "staged"
                          (mevedel-session-artifacts-read-artifact
                           session "segment-0001.chat.org")))
                  (cl-letf (((symbol-function 'mevedel-session-durability-lease-owned-p)
                             (lambda (_) nil)))
                    (should (equal "committed"
                                   (mevedel-session-artifacts-read-artifact
                                    session "segment-0001.chat.org"))))
                  (should
                   (equal "committed"
                          (mevedel-session-artifacts-read-artifact
                           session "segment-0001.chat.org" t)))
                  (let* ((entry
                          (cdr
                           (assoc
                            "segment-0001.chat.org"
                            (plist-get (mevedel-session-publication session)
                                       :artifacts))))
                         (published (plist-get entry :published)))
                    (write-region "corrupt" nil published nil 'silent)
                    (should-error
                     (mevedel-session-artifacts-read-artifact
                      session "segment-0001.chat.org" t))))
              (mevedel-session-durability-lease-release session-dir session)
              (ignore workspace))))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-artifacts-artifact-present-p ()
  ,test
  (test)
  :doc "uses fixed logical-path existence for local sessions"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "mevedel-local-artifact-present-" t)))
         (workspace
          (test-mevedel-session-persistence--make-file-workspace root))
         (session (mevedel-session-create "resolver" workspace))
         (session-dir
          (file-name-as-directory (file-name-concat root "session")))
         (logical "plans/current.md")
         (path (file-name-concat session-dir logical)))
    (setf (mevedel-session-save-path session) session-dir)
    (unwind-protect
        (progn
          (should-not
           (mevedel-session-artifacts-artifact-present-p session logical))
          (make-directory (file-name-directory path) t)
          (write-region "plan" nil path nil 'silent)
          (should
           (mevedel-session-artifacts-artifact-present-p session logical))
          (should-error
           (mevedel-session-artifacts-artifact-present-p
            session ".publications/escape")))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry)))
  :doc "uses remote staged or captured membership without fixed-cache fallback"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-artifact-present-" t)))
         (host "artifact-present")
         (mevedel-session-durability--client-id (make-string 64 ?b)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host root "fixed cache"))
                       (sidecar
                        (mevedel-session-artifacts-sidecar-path session-dir)))
            (unwind-protect
                (progn
                  (should
                   (mevedel-session-durability-lease-acquire
                    session-dir "*present*" session))
                  (should
                   (mevedel-session-publication-publish
                    session
                    (list (list :path segment :content "committed")
                          (list :path sidecar :content "sidecar"
                                :commit-marker t))))
                  (delete-file segment)
                  (should
                   (mevedel-session-artifacts-artifact-present-p
                    session "segment-0001.chat.org"))
                  (should-not
                   (mevedel-session-artifacts-artifact-present-p
                    session "plans/missing.md"))
                  (let ((plan (file-name-concat session-dir "plans/current.md")))
                    (should
                     (mevedel-session-publication-publish
                      session (list (list :path plan :content "staged"))))
                    (should
                     (mevedel-session-artifacts-artifact-present-p
                      session "plans/current.md"))))
              (mevedel-session-durability-lease-release session-dir session))))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-artifacts-find-artifact-noselect ()
  ,test
  (test)
  :doc "visits the logical path with verified remote bytes for inspection"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "mevedel-find-artifact-" t)))
         (host "find-artifact")
         (mevedel-session-durability--client-id (make-string 64 ?c))
         buffer
         authoritative)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host root "fixed cache"))
                       (sidecar
                        (mevedel-session-artifacts-sidecar-path session-dir)))
            (unwind-protect
                (progn
                  (should
                   (mevedel-session-durability-lease-acquire
                    session-dir "*find-artifact*" session))
                  (should
                   (mevedel-session-publication-publish
                    session
                    (list (list :path segment :content "committed transcript")
                          (list :path sidecar :content "sidecar"
                                :commit-marker t))))
                  (write-region "stale fixed cache" nil segment nil 'silent)
                  (setq buffer
                        (mevedel-session-artifacts-find-artifact-noselect
                         session "segment-0001.chat.org" t))
                  (with-current-buffer buffer
                    (should (equal buffer-file-name segment))
                    (should (equal (buffer-string) "committed transcript"))
                    (should buffer-read-only)
                    (should mevedel-session--inspection-buffer-p)
                    (should-not (buffer-modified-p))
                    (should (verify-visited-file-modtime buffer)))
                  (cl-letf
                      (((symbol-function
                         'mevedel-session-persistence--find-file-noselect)
                        (lambda (&rest _)
                          (ert-fail "Remote adapter read the fixed cache"))))
                    (setq
                     authoritative
                     (mevedel-session-artifacts-find-artifact-noselect
                      session "segment-0001.chat.org")))
                  (should-not (eq buffer authoritative))
                  (with-current-buffer authoritative
                    (should (equal buffer-file-name segment))
                    (should (equal (buffer-string) "committed transcript"))
                    (should-not buffer-read-only)
                    (should-not mevedel-session--inspection-buffer-p)
                    (should-not (buffer-modified-p))))
              (mevedel-session-durability-lease-release session-dir session))))
      (dolist (candidate (list buffer authoritative))
        (when (buffer-live-p candidate)
          (with-current-buffer candidate
            (setq buffer-read-only nil)
            (set-buffer-modified-p nil))
          (kill-buffer candidate)))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-artifacts-publish-text ()
  ,test
  (test)
  :doc "writes local bytes atomically and returns their path"
  (let* ((root (make-temp-file "mevedel-publish-text-" t))
         (path (file-name-concat root "state.el"))
         (workspace
          (test-mevedel-session-persistence--make-file-workspace root))
         (session (mevedel-session-create "publish" workspace)))
    (unwind-protect
        (progn
          (should
           (equal path
                  (mevedel-session-artifacts-publish-text
                   session path "local state\n" 'utf-8-unix)))
          (should
           (equal "local state\n"
                  (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string)))))
      (delete-directory root t)))
  :doc "returns the remote queue outcome without claiming publication"
  (let* ((target
          (mevedel-execution-target-create "/ssh:user@host:/srv/project/"))
         (session (mevedel-session--create
                   :execution-target target
                   :authority-mode 'portable))
         (path "/ssh:user@host:/srv/project/state.el")
         published)
    (cl-letf
        (((symbol-function
           'mevedel-session-artifacts-assert-mutation-authority)
          (lambda (_) t))
         ((symbol-function 'mevedel-session-publication-publish)
          (lambda (_session artifacts)
            (setq published artifacts)
            'queued)))
      (should
       (eq 'queued
           (mevedel-session-artifacts-publish-text
            session path "remote state" 'utf-8-unix)))
      (should
       (equal
        (list (list :path path :content "remote state" :coding 'utf-8-unix))
        published)))))


(mevedel-deftest mevedel-session-artifacts-publish-transcript-state ()
  ,test
  (test)
  :doc "commits a remote transcript and authoritative sidecar in one head"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "mevedel-transcript-state-" t)))
         (host "transcript-state")
         (mevedel-session-durability--client-id (make-string 64 ?d))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host root "old transcript"))
                       (sidecar
                        (mevedel-session-artifacts-sidecar-path
                         session-dir))
                       (mevedel-session-durability--disclosed-targets
                        (make-hash-table :test #'equal)))
            (puthash
             (mevedel-execution-target-identity
              (mevedel-session-execution-target session))
             t mevedel-session-durability--disclosed-targets)
            (unwind-protect
                (progn
                  (should
                   (mevedel-session-durability-lease-acquire
                    session-dir "*transcript-state*" session))
                  (setf (mevedel-session-publication session)
                        (mevedel-session-publication-read
                         session-dir))
                  (setq buffer
                        (generate-new-buffer " *transcript-state-root*"))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local mevedel--session session)
                    (setq buffer-file-name segment)
                    (insert "old transcript"))
                  (dolist (operation '(sidecar transcript))
                    (setf (mevedel-session-publication-active-p session) t)
                    (unwind-protect
                        (progn
                          (should-error
                           (if (eq operation 'sidecar)
                               (mevedel-session-artifacts-publish-sidecar-state
                                session buffer)
                             (mevedel-session-artifacts-publish-transcript-state
                              session buffer segment "queued terminal"))
                           :type 'user-error)
                          (should-not (mevedel-session-publication-queue session))
                          (should-not (mevedel-session-pending-publication session)))
                      (setf (mevedel-session-publication-active-p session) nil)
                      (mevedel-session-publication-clear-transient session)))
                  (let ((head-before
                         (plist-get (mevedel-session-publication session)
                                    :head)))
                    ;; Fixed caches are not publication authority.
                    (delete-file segment)
                    (delete-file sidecar)
                    (should
                     (mevedel-session-artifacts-publish-transcript-state
                      session buffer segment "archived terminal\n"))
                    (should-not
                     (equal head-before
                            (plist-get
                             (mevedel-session-publication session) :head)))
                    (should
                     (equal
                      "archived terminal\n"
                      (mevedel-session-artifacts-read-artifact
                       session "segment-0001.chat.org" t)))
                    (should
                     (plist-get
                      (with-temp-buffer
                        (insert
                         (mevedel-session-artifacts-read-artifact
                          session "session.meta.el" t))
                        (goto-char (point-min))
                        (read (current-buffer)))
                      :session-id))))
              (when session
                (mevedel-session-durability-lease-release
                 session-dir session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-artifacts-publish-sidecar-state (:quiet t)
  ,test
  (test)
  :doc "commits only the remote sidecar and propagates publication failure"
  (let* ((root
          (file-name-as-directory
           (make-temp-file "mevedel-sidecar-state-" t)))
         (host "sidecar-state")
         (mevedel-session-durability--client-id (make-string 64 ?e))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host root "published transcript"))
                       (sidecar
                        (mevedel-session-artifacts-sidecar-path
                         session-dir))
                       (publications
                        (file-name-concat
                         root ".mevedel" "sessions"
                         (mevedel-session-session-id session)
                         ".publications"))
                       (mevedel-session-durability--disclosed-targets
                        (make-hash-table :test #'equal)))
            (puthash
             (mevedel-execution-target-identity
              (mevedel-session-execution-target session))
             t mevedel-session-durability--disclosed-targets)
            (unwind-protect
                (progn
                  (should
                   (mevedel-session-durability-lease-acquire
                    session-dir "*sidecar-state*" session))
                  (setf (mevedel-session-publication session)
                        (mevedel-session-publication-read
                         session-dir))
                  (setq buffer
                        (generate-new-buffer " *sidecar-state-root*"))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local mevedel--session session)
                    (setq buffer-file-name segment)
                    (insert "published transcript"))
                  (let ((head-before
                         (plist-get (mevedel-session-publication session)
                                    :head)))
                    (setf (mevedel-session-name session) "sidecar-only")
                    (delete-file sidecar)
                    (should
                     (mevedel-session-artifacts-publish-sidecar-state
                      session buffer))
                    (should-not
                     (equal head-before
                            (plist-get
                             (mevedel-session-publication session) :head)))
                    (should
                     (equal
                      "published transcript"
                      (mevedel-session-artifacts-read-artifact
                       session "segment-0001.chat.org" t)))
                    (should
                     (equal
                      "sidecar-only"
                      (plist-get
                       (with-temp-buffer
                         (insert
                          (mevedel-session-artifacts-read-artifact
                           session "session.meta.el" t))
                         (goto-char (point-min))
                         (read (current-buffer)))
                       :session-name))))
                  (let ((directories
                         (cons
                          publications
                          (cl-remove-if-not
                           #'file-directory-p
                           (directory-files-recursively
                            publications ".*" t)))))
                    (unwind-protect
                        (progn
                          (mapc (lambda (path) (set-file-modes path #o500))
                                directories)
                          (setf (mevedel-session-name session) "blocked")
                          (should-error
                           (mevedel-session-artifacts-publish-sidecar-state
                            session buffer)
                           :type 'file-error)
                          (should
                           (mevedel-session-pending-publication session)))
                      (mapc (lambda (path) (set-file-modes path #o700))
                            directories))
                    (should
                     (mevedel-session-publication-retry session))))
              (when session
                (mevedel-session-durability-lease-release
                 session-dir session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry)))

  :doc "rejects reentrant queueing as an uncommitted strict marker"
  (with-temp-buffer
    (let ((session (mevedel-session--create :name "queued")))
      (cl-letf
          (((symbol-function
             'mevedel-session-artifacts--sidecar-publication-artifact)
            (lambda (_session _root-buffer)
              '(:path "/target/session.meta.el"
                :content "sidecar"
                :commit-marker t)))
           ((symbol-function 'mevedel-session-publication-publish)
            (lambda (_session artifacts &optional require-commit)
              (should (plist-get (car artifacts) :commit-marker))
              (should require-commit)
              (user-error "Required session publication cannot be queued"))))
        (should-error
         (mevedel-session-artifacts-publish-sidecar-state
          session (current-buffer))
         :type 'user-error)))))


;;
;;; Phase 7: rewind picker

(mevedel-deftest mevedel-session-artifacts-collect-prompts ()
  ,test
  (test)
  :doc "extracts user prompt regions in document order"
  (with-temp-buffer
    (insert "First prompt\n")
    (insert (propertize "Sure, I'll do that.\n" 'gptel 'response))
    (insert "Second prompt\n")
    (insert (propertize "Okay.\n" 'gptel 'response))
    (insert "Third prompt\n")
    (let ((prompts (mevedel-session-artifacts-collect-prompts
                    (current-buffer))))
      (should (= 3 (length prompts)))
      (should (= 1 (plist-get (nth 0 prompts) :turn)))
      (should (= 2 (plist-get (nth 1 prompts) :turn)))
      (should (= 3 (plist-get (nth 2 prompts) :turn)))
      (should (string-match-p "First prompt"
                              (plist-get (nth 0 prompts) :preview)))
      (should (string-match-p "Third prompt"
                              (plist-get (nth 2 prompts) :preview)))))
  :doc "skips blank-only regions"
  (with-temp-buffer
    (insert "   \n\n  \t\n")
    (insert (propertize "response" 'gptel 'response))
    (insert "Real prompt\n")
    (let ((prompts (mevedel-session-artifacts-collect-prompts
                    (current-buffer))))
      (should (= 1 (length prompts)))
      (should (string-match-p "Real prompt"
                              (plist-get (car prompts) :preview)))))
  :doc "skips indented leading property drawer"
  (with-temp-buffer
    (insert "  :PROPERTIES:\n")
    (insert "  :MEVEDEL_SESSION: metadata\n")
    (insert "  :END:\n")
    (let ((prompt-start (point)))
      (insert "Real prompt after metadata\n")
      (insert (propertize "response" 'gptel 'response))
      (let ((prompts (mevedel-session-artifacts-collect-prompts
                      (current-buffer))))
        (should (= 1 (length prompts)))
        (should (= prompt-start (plist-get (car prompts) :pos)))
        (should (equal "Real prompt after metadata"
                       (plist-get (car prompts) :preview))))))
  :doc "skips unpropertized gptel org tool and reasoning scaffolding"
  (with-temp-buffer
    (insert "Fetch a page\n")
    (insert (propertize "Initial answer text.\n" 'gptel 'response))
    (insert "#+begin_reasoning\nThinking text.\n")
    (insert "#+begin_tool (WebFetch :url \"https://example.com\")\n")
    (insert (propertize
             "(:name \"WebFetch\" :args (:url \"https://example.com\"))\n\nbody\n"
             'gptel '(tool . "call_1")))
    (insert "#+end_tool\nMore thinking.\n#+end_reasoning\n")
    (insert "Search for docs\n")
    (insert (propertize "Second answer.\n" 'gptel 'response))
    (let ((prompts (mevedel-session-artifacts-collect-prompts
                    (current-buffer))))
      (should (= 2 (length prompts)))
      (should (equal "Fetch a page"
                     (plist-get (nth 0 prompts) :preview)))
      (should (equal "Search for docs"
                     (plist-get (nth 1 prompts) :preview)))))
  :doc "keeps user-authored org block marker as prompt start"
  (with-temp-buffer
    (let ((prompt-start (point)))
      (insert "#+begin_src emacs-lisp\n")
      (insert "(message \"hello\")\n")
      (insert "#+end_src\n")
      (insert (propertize "Response.\n" 'gptel 'response))
      (let ((prompts (mevedel-session-artifacts-collect-prompts
                      (current-buffer))))
        (should (= 1 (length prompts)))
        (should (= prompt-start (plist-get (car prompts) :pos)))
        (should (equal "#+begin_src emacs-lisp"
                       (plist-get (car prompts) :preview))))))
  :doc "indexes a directive prompt at its paired boundary"
  (with-temp-buffer
    (insert "ordinary\n" (propertize "answer\n" 'gptel 'response))
    (let ((boundary-start (point)))
      (insert (mevedel--format-hook-audit-record
               '(:type directive-turn-boundary :edge start
                 :directive-id "directive-123" :action discuss :turn 2)))
      (insert "directive prompt\n")
      (insert (propertize "directive answer\n" 'gptel 'response))
      (insert (mevedel--format-hook-audit-record
               '(:type directive-turn-boundary :edge end
                 :directive-id "directive-123" :action discuss :turn 2
                 :outcome success :sequence 1)))
      (let ((prompt (nth 1 (mevedel-session-artifacts-collect-prompts
                            (current-buffer)))))
        (should (= boundary-start (plist-get prompt :pos)))
        (should (eq 'directive (plist-get prompt :kind)))
        (should (equal "directive-123" (plist-get prompt :directive-id)))
        (should (= 2 (plist-get prompt :reserved-turn))))))
  :doc "keeps mixed chat and directive follow-ups in one chronology"
  (with-temp-buffer
    (insert "ordinary one\n" (propertize "answer one\n" 'gptel 'response))
    (insert (mevedel--format-hook-audit-record
             '(:type directive-turn-boundary :edge start
               :directive-id "directive-123" :action discuss :turn 2)))
    (insert "directive one\n"
            (propertize "directive answer one\n" 'gptel 'response))
    (insert (mevedel--format-hook-audit-record
             '(:type directive-turn-boundary :edge end
               :directive-id "directive-123" :action discuss :turn 2
               :outcome success :sequence 1)))
    (insert "ordinary two\n" (propertize "answer two\n" 'gptel 'response))
    (insert (mevedel--format-hook-audit-record
             '(:type directive-turn-boundary :edge start
               :directive-id "directive-123" :action discuss :turn 4)))
    (insert "directive follow-up\n"
            (propertize "directive answer two\n" 'gptel 'response))
    (insert (mevedel--format-hook-audit-record
             '(:type directive-turn-boundary :edge end
               :directive-id "directive-123" :action discuss :turn 4
               :outcome success :sequence 2)))
    (let ((prompts (mevedel-session-artifacts-collect-prompts
                    (current-buffer))))
      (should (equal '(1 2 3 4) (mapcar (lambda (entry)
                                         (plist-get entry :turn))
                                       prompts)))
      (should (equal '(nil directive nil directive)
                     (mapcar (lambda (entry) (plist-get entry :kind))
                             prompts)))
      (should (equal '(2 4)
                     (delq nil
                           (mapcar (lambda (entry)
                                     (plist-get entry :reserved-turn))
                                   prompts)))))))


(mevedel-deftest mevedel-session-artifacts-update-prompt-index ()
  ,test
  (test)
  :doc "updates only the live segment's entry"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                ;; Pre-seed with a finalized segment 1 entry.
                (setf (mevedel-session-prompt-index session)
                      '((1 . ((:turn 1 :file-turn 1 :cum-turn 1
                              :pos 1 :preview "old prompt")))))
                (setf (mevedel-session-current-segment session) 2)
                (insert "New live prompt\n")
                (mevedel-session-artifacts-update-prompt-index
                 session buf)
                (let ((index (mevedel-session-prompt-index session)))
                  ;; Segment 1 untouched.
                  (should (= 1 (length (cdr (assoc 1 index)))))
                  ;; Segment 2 has the new prompt.
                  (should (assoc 2 index))
                  (should (= 1 (length (cdr (assoc 2 index)))))
                  (should
                   (string-match-p
                    "New live prompt"
                    (plist-get (car (cdr (assoc 2 index))) :preview)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "uses the directive boundary as the canonical cumulative turn"
  (let ((session (mevedel-session--create :current-segment 1)))
    (with-temp-buffer
      (insert "ordinary\n" (propertize "answer\n" 'gptel 'response))
      (insert (mevedel--format-hook-audit-record
               '(:type directive-turn-boundary :edge start
                 :directive-id "directive-123" :action discuss :turn 2)))
      (insert "directive prompt\n")
      (insert (propertize "directive answer\n" 'gptel 'response))
      (insert (mevedel--format-hook-audit-record
               '(:type directive-turn-boundary :edge end
                 :directive-id "directive-123" :action discuss :turn 2
                 :outcome success :sequence 1)))
      (mevedel-session-artifacts-update-prompt-index
       session (current-buffer))
      (let ((prompt (nth 1 (cdr (assoc 1
                                       (mevedel-session-prompt-index
                                        session))))))
        (should (= 2 (plist-get prompt :cum-turn)))
        (should (= 2 (plist-get prompt :reserved-turn)))))))


(mevedel-deftest mevedel-session-artifacts--ensure-latest-fork-point ()
  ,test
  (test)
  :doc "indexes a settling turn that auto-compaction carried into a new segment"
  (let* ((prompt (list :turn 1 :file-turn 1 :cum-turn 4 :pos 1
                       :preview "Implement the plan"))
         (session (mevedel-session--create
                   :current-segment 2 :turn-count 4
                   :prompt-index (list (list 1 prompt)))))
    (with-temp-buffer
      (org-mode)
      (insert (mevedel-session-artifacts-summary-block "Work so far."))
      (let ((resume (point)))
        (insert (propertize "Finished the remaining checks.\n"
                            'gptel 'response))
        (setq-local mevedel--current-request
                    (mevedel-request--create :turn 4))
        (should (mevedel-session-artifacts--ensure-latest-fork-point
                 session (current-buffer)))
        (mevedel-session-artifacts-update-prompt-index session (current-buffer))
        (let ((continuation (plist-get prompt :continuation)))
          (should (eql 2 (plist-get continuation :segment)))
          (should (<= (plist-get continuation :start) resume))
          (should (stringp (plist-get continuation :fork-point-id)))
          (should (> (plist-get continuation :transcript-cutoff) resume)))
        ;; The entry keeps its own shape, and the next save changes nothing.
        (should-not (plist-get prompt :fork-point-id))
        (should-not (mevedel-session-artifacts--ensure-latest-fork-point
                     session (current-buffer))))))

  :doc "leaves an earlier unfinished turn alone when another request settles"
  (let* ((prompt (list :turn 1 :file-turn 1 :cum-turn 3 :pos 1 :preview "Try"))
         (session (mevedel-session--create
                   :current-segment 2 :turn-count 3
                   :prompt-index (list (list 1 prompt)))))
    (with-temp-buffer
      (org-mode)
      (insert (mevedel-session-artifacts-summary-block "Work so far."))
      (insert (propertize "Copied answer.\n" 'gptel 'response))
      (setq-local mevedel--current-request (mevedel-request--create :turn 5))
      (should-not (mevedel-session-artifacts--ensure-latest-fork-point
                   session (current-buffer)))
      (mevedel-session-artifacts-update-prompt-index session (current-buffer))
      (should-not (plist-get prompt :continuation)))))


(mevedel-deftest mevedel-session-artifacts--latest-user-message-from-index ()
  ,test
  (test)
  :doc "returns newest prompt by cumulative turn"
  (should
   (equal "third"
          (mevedel-session-artifacts--latest-user-message-from-index
           '((2 . ((:turn 1 :cum-turn 3 :preview "third")))
             (1 . ((:turn 1 :cum-turn 1 :preview "first")
                   (:turn 2 :cum-turn 2 :preview "second")))))))
  :doc "ignores blank previews"
  (should
   (null (mevedel-session-artifacts--latest-user-message-from-index
          '((1 . ((:turn 1 :preview "   "))))))))


(mevedel-deftest mevedel-session-artifacts-fork-point-spans ()
  ,test
  (test)
  :doc "reads bounded absolute positions without copying the transcript"
  (with-temp-buffer
    (insert (make-string 100000 ?x))
    (let ((start (point)))
      (insert (mevedel--format-hook-audit-record
               '(:type fork-point :fork-point-id "inside")))
      (let* ((end (point))
             (span (car (mevedel-transcript-audit-buffer-spans
                         'fork-point start end))))
        (insert (mevedel--format-hook-audit-record
                 '(:type fork-point :fork-point-id "outside")))
        (save-restriction
          (narrow-to-region start end)
          (cl-letf (((symbol-function 'buffer-string)
                     (lambda () (error "Whole transcript copy"))))
            (let ((actual (mevedel-session-artifacts-fork-point-spans
                           (current-buffer))))
              (should (= 1 (length actual)))
              (should (equal "inside" (plist-get (car actual) :fork-point-id)))
              (should (= (plist-get span :start)
                         (plist-get (car actual) :record-start)))
              (should (= (plist-get span :end)
                         (plist-get (car actual) :transcript-cutoff)))))
          (should (= start (point-min)))
          (should (= end (point-max)))))))
  :doc "trust changes and restriction changes cannot reuse stale fork points"
  (with-temp-buffer
    (insert (mevedel--format-hook-audit-record
             '(:type fork-point :fork-point-id "one")))
    (should (= 1 (length (mevedel-session-artifacts-fork-point-spans
                         (current-buffer)))))
    (save-restriction
      (narrow-to-region (point-max) (point-max))
      (should-not (mevedel-session-artifacts-fork-point-spans
                   (current-buffer))))
    (remove-text-properties (point-min) (point-max)
                            '(gptel nil mevedel-hook-audit nil))
    (should-not (mevedel-session-artifacts-fork-point-spans (current-buffer)))))



;;
;;; Phase 11: relocation, self-heal, save-failure flag

(mevedel-deftest mevedel-session-artifacts-reconcile-relocation (:quiet t)
  ,test
  (test)
  :doc "rewrites permission rules whose :path is under the saved root"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "id" "/new/root/" "ws"))
         (session   (mevedel-session-create "x" workspace)))
    (setf (mevedel-session-permission-rules session)
          '(("Read"  :path "/old/root/foo/**" :action allow)
            ("Read"  :path "/old/root/bar/baz" :action allow)
            ("Bash"  :pattern "git log*"      :action allow)
            ("Read"  :path "/elsewhere/baz"   :action deny)))
    (mevedel-session-artifacts-reconcile-relocation
     session '(:type project :workspace-id "id"
               :target-native-root "/old/root/" :name "ws"))
    (let ((rules (mevedel-session-permission-rules session)))
      (should (equal (file-name-concat (expand-file-name "/new/root/")
                                       "foo/**")
                     (plist-get (cdr (nth 0 rules)) :path)))
      (should (equal (file-name-concat (expand-file-name "/new/root/")
                                       "bar/baz")
                     (plist-get (cdr (nth 1 rules)) :path)))
      ;; Bash rule untouched (no :path).
      (should (equal "git log*" (plist-get (cdr (nth 2 rules)) :pattern)))
      ;; Out-of-tree path untouched.
      (should (equal "/elsewhere/baz"
                     (plist-get (cdr (nth 3 rules)) :path))))
    (mevedel-workspace-clear-registry))
  :doc "no-op when saved root matches current"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "id2" "/same/root/" "ws"))
         (session   (mevedel-session-create "x" workspace))
         (orig-rules '(("Read" :path "/same/root/foo" :action allow))))
    (setf (mevedel-session-permission-rules session) orig-rules)
    (mevedel-session-artifacts-reconcile-relocation
     session '(:type project :workspace-id "id2"
               :target-native-root "/same/root/" :name "ws"))
    (should (equal orig-rules
                   (mevedel-session-permission-rules session)))
    (mevedel-workspace-clear-registry))
  :doc "does not rewrite permission paths already under nested current root"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project "id3" "/old/root/packages/api/" "ws"))
         (session   (mevedel-session-create "x" workspace))
         (orig-rules '(("Read" :path "/old/root/packages/api/foo" :action allow)
                       ("Read" :path "/old/root/other" :action allow))))
    (setf (mevedel-session-permission-rules session) orig-rules)
    (mevedel-session-artifacts-reconcile-relocation
     session '(:type project :workspace-id "id3"
               :target-native-root "/old/root/" :name "ws"))
    (let ((rules (mevedel-session-permission-rules session)))
      (should (equal "/old/root/packages/api/foo"
                     (plist-get (cdr (nth 0 rules)) :path)))
      (should (equal (file-name-concat
                      (expand-file-name "/old/root/packages/api/")
                      "other")
                     (plist-get (cdr (nth 1 rules)) :path))))
    (mevedel-workspace-clear-registry)))


(mevedel-deftest mevedel-session-artifacts-detect-highest-segment ()
  ,test
  (test)
  :doc "returns the maximum segment number on disk"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-segdetect-" t))))
    (unwind-protect
        (progn
          (write-region "" nil
                        (file-name-concat tempdir "segment-0001.chat.org")
                        nil 'silent)
          (write-region "" nil
                        (file-name-concat tempdir "segment-0003.chat.org")
                        nil 'silent)
          (write-region "" nil
                        (file-name-concat tempdir "segment-0002.chat.org")
                        nil 'silent)
          ;; Decoy file shouldn't count.
          (write-region "" nil
                        (file-name-concat tempdir "session.meta.el")
                        nil 'silent)
          (should (= 3 (mevedel-session-artifacts-detect-highest-segment
                        tempdir))))
      (delete-directory tempdir t)))
  :doc "returns 0 when no segment files exist"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-segdetect-" t))))
    (unwind-protect
        (should (= 0 (mevedel-session-artifacts-detect-highest-segment
                      tempdir)))
      (delete-directory tempdir t))))


(mevedel-deftest mevedel-session-artifacts-self-heal-segment-counter ()
  ,test
  (test)
  :doc "finalizes predecessor when healing upward"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-selfheal-" t))))
    (unwind-protect
        (let ((session (mevedel-session-create
                        "x"
                        (mevedel-workspace-get-or-create
                         'project "id" "/" "x")))
              (seg1 (file-name-concat tempdir "segment-0001.chat.org")))
          (setf (mevedel-session-current-segment session) 1)
          (with-temp-buffer
            (org-mode)
            (insert "Completed request\n"
                    (propertize "Completed response\n" 'gptel 'response))
            (insert (mevedel--format-hook-audit-record
                     (list :type 'fork-point :fork-point-id (make-string 32 ?a)
                           :segment 1 :turn 1 :file-turn 1 :cum-turn 1
                           :captured-file-turn 1)))
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (mevedel-session-artifacts-update-prompt-index session (current-buffer))
            (write-region (point-min) (point-max) seg1 nil 'silent))
          (write-region "* Chat\n" nil
                        (file-name-concat tempdir "segment-0002.chat.org")
                        nil 'silent)
          (cl-letf (((symbol-function 'display-warning) #'ignore))
            (mevedel-session-artifacts-self-heal-segment-counter
             session tempdir))
          (should (= 2 (mevedel-session-current-segment session)))
          (with-temp-buffer
            (insert-file-contents seg1)
            (org-mode)
            (mevedel-transcript-restore-properties)
            (let ((prompt (cadr (assoc 1 (mevedel-session-prompt-index session))))
                  (span (car (mevedel-session-artifacts-fork-point-spans (current-buffer)))))
              (should (= (plist-get prompt :transcript-cutoff)
                         (plist-get span :transcript-cutoff))))
            (should (string-match-p "MEVEDEL_SEGMENT_FINALIZED_AT"
                                    (buffer-string)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-artifacts-self-heal-crash-restore ()
  ,test
  (test)
  :doc "restore repairs and persists predecessor coordinates across file transition crash windows"
  (let* ((root (make-temp-file "mevedel-rotation-crash-" t))
         (workspace (test-mevedel-session-persistence--make-file-workspace root))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *rotation-crash*"))
         (mevedel-journal-enabled nil)
         restored restored-session)
    (unwind-protect
        (let (directory predecessor)
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--session session)
            (insert "Request\n" (propertize "Completed response\n" 'gptel 'response))
            (setf (mevedel-session-turn-count session) 1)
            (mevedel-session-artifacts-save session buffer t)
            (setq directory (mevedel-session-save-path session)
                  predecessor buffer-file-name)
            ;; A nonlocal exit bypasses caught-error rollback, leaving
            ;; the same files a process crash at this write would leave.
            (should
             (eq 'crashed
                 (catch 'crash
                   (cl-letf (((symbol-function 'mevedel-session-codec-write)
                              (lambda (&rest _) (throw 'crash 'crashed))))
                     (mevedel-session-artifacts-rotate-segment session buffer "Summary")))))
            (let ((state (mevedel-session-codec-read
                          (mevedel-session-artifacts-sidecar-path directory))))
              (should (= 1 (plist-get state :current-segment))))
            (should (file-exists-p (mevedel-session-artifacts-segment-path directory 2))))
          (test-mevedel-session-persistence--release-and-kill buffer session)
          (setq buffer nil)
          (mevedel-test--with-captured-diagnostics nil
            (setq restored (mevedel-session-persistence-restore directory nil nil workspace)))
          (setq restored-session (buffer-local-value 'mevedel--session restored))
          (should (= 2 (mevedel-session-current-segment restored-session)))
          (let* ((index (mevedel-session-prompt-index restored-session))
                 (state (mevedel-session-codec-read
                         (mevedel-session-artifacts-sidecar-path directory)))
                 (prompt (cadr (assoc 1 index))))
            (should (equal index (plist-get state :prompt-index)))
            (should (= 2 (plist-get state :current-segment)))
            (with-temp-buffer
              (insert-file-contents predecessor)
              (org-mode)
              (mevedel-transcript-restore-properties)
              (should (org-entry-get (point-min) "MEVEDEL_SEGMENT_FINALIZED_AT"))
              (let ((span (car (mevedel-session-artifacts-fork-point-spans (current-buffer)))))
                (should (= (plist-get prompt :transcript-cutoff)
                           (plist-get span :transcript-cutoff)))))
            (let ((evidence (mevedel-journal-evidence-turns
                             restored-session
                             (list (list :number 1 :segment 1
                                         :fork-point (plist-get prompt :fork-point-id)
                                         :start (plist-get prompt :pos)
                                         :end (plist-get prompt :transcript-cutoff))))))
              (should (string-search "Completed response" (plist-get evidence :text))))
            ;; Recovery is complete: an ordinary second inspection must not
            ;; rewrite the already-finalized predecessor or its index.
            (should-not (mevedel-session-artifacts-self-heal-segment-counter
                         restored-session directory))
            (should (equal index (mevedel-session-prompt-index restored-session)))))
      (test-mevedel-session-persistence--release-and-kill buffer session)
      (test-mevedel-session-persistence--release-and-kill restored restored-session)
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-session-artifacts/file-history-roundtrip ()
  ,test
  (test)
  :doc "a modifying tool routed through the pipeline lands a backup in file-history"
  (cl-destructuring-bind (session . tempdir)
      (test-mevedel-session-persistence--make-materialized-session)
    (setf (mevedel-session-permission-mode session) 'edits)
    (unwind-protect
        (let* ((data-buf (get-buffer "*test-data-buf*"))
               (tracked  (file-name-concat tempdir "tracked.el"))
               ;; Plant pre-edit content so the snapshot has a
               ;; non-nil "original" to compare against at save time.
               (_ (let ((coding-system-for-write 'utf-8-unix))
                    (write-region "ORIGINAL\n" nil tracked nil 'silent)))
               ;; Mock tool with `get-path' so the pipeline's
               ;; snapshot step fires for it.  Handler mutates the
               ;; file to simulate what a real Edit / Write would do.
               (tool (mevedel-tool--create
                      :name "WriteMock"
                      :groups '(edit)
                      :handler (lambda (args)
                                 (let ((p (plist-get args :path))
                                       (c (plist-get args :content)))
                                   (let ((coding-system-for-write 'utf-8-unix))
                                     (write-region c nil p nil 'silent))
                                   '(:result "ok")))
                      :args '((path string :required "Path")
                              (content string :required "Content"))
                      :get-path (lambda (args) (plist-get args :path))
                      :read-only-p nil
                      :snapshot-p t
                      :async-p nil))
               result)
          ;; Plant the session buffer-locally so
          ;; `mevedel-pipeline-run-tool' captures it as the context.
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace
                        (mevedel-session-workspace session))
            ;; Begin a request so tool-fs records the original content.
            (mevedel-request-begin session)
            (unwind-protect
                (progn
                  (mevedel-pipeline-run-tool
                   tool (lambda (r) (setq result r))
                   (list :path tracked :content "MODIFIED\n"))
                  (should (equal "ok" result))
                  ;; Snapshot step captured the pre-edit content.
                  (let ((ht (mevedel-request-file-snapshots
                             mevedel--current-request)))
                    (should (hash-table-p ht))
                    (should (equal "ORIGINAL\n" (gethash tracked ht))))
                  ;; Drive a save (what the DONE terminal handler
                  ;; would do in production) and verify a backup file
                  ;; landed under file-history/.
                  (mevedel-session-artifacts-save session data-buf)
                  (let* ((snaps (mevedel-session-file-snapshots session))
                         (turn-entry (cdar snaps))
                         (file-entry (assoc tracked turn-entry))
                         (backup-name (plist-get (cdr file-entry)
                                                 :backup-name))
                         (pre-backup-name
                          (plist-get (cdr file-entry) :pre-backup-name))
                         (backup-path (mevedel-session-artifacts-backup-path
                                       (mevedel-session-save-path session)
                                       backup-name))
                         (pre-backup-path
                          (mevedel-session-artifacts-backup-path
                           (mevedel-session-save-path session)
                           pre-backup-name)))
                    (should snaps)
                    (should backup-name)
                    (should pre-backup-name)
                    (should (file-exists-p backup-path))
                    (should (file-exists-p pre-backup-path))
                    (with-temp-buffer
                      (insert-file-contents-literally backup-path)
                      (should (equal "MODIFIED\n" (buffer-string))))
                    (with-temp-buffer
                      (insert-file-contents-literally pre-backup-path)
                      (should (equal "ORIGINAL\n" (buffer-string))))))
              (mevedel-request-end))))
      (test-mevedel-session-persistence--cleanup tempdir))))

(provide 'test-mevedel-session-artifacts)
;;; test-mevedel-session-artifacts.el ends here
