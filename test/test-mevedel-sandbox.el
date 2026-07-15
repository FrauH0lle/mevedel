;;; test-mevedel-sandbox.el --- Tests for child confinement -*- lexical-binding: t -*-

;;; Commentary:

;; Tests pure Bubblewrap preparation and the optional real Linux backend.

;;; Code:

(require 'cl-lib)
(require 'mevedel-sandbox)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Sandbox preparation

(mevedel-deftest mevedel-sandbox--probe-command ()
  ,test
  (test)
  :doc "Bubblewrap probe argv:
`mevedel-sandbox--probe-command' uses the supplied executable and core profile"
  (let ((command (mevedel-sandbox--probe-command "/test/bwrap")))
    (should (equal (car command) "/test/bwrap"))
    (should (member "--ro-bind" command))
    (should (member "--unshare-user" command))
    (should (member "--unshare-pid" command))
    (should (member "--unshare-net" command))
    (should (member "--proc" command))))

(mevedel-deftest mevedel-sandbox-probe ()
  ,test
  (test)
  :doc "cached probe:
`mevedel-sandbox-probe' reuses the first real availability decision"
  (let* ((cached '(:available nil :reason "cached result"))
         (mevedel-sandbox--probe-cache cached))
    (should (eq (mevedel-sandbox-probe) cached)))
  :doc "host probe:
`mevedel-sandbox-probe' always returns explicit availability facts"
  (let ((mevedel-sandbox--probe-cache nil))
    (let ((result (mevedel-sandbox-probe)))
      (should (plist-member result :available))
      (if (plist-get result :available)
          (should (stringp (plist-get result :executable)))
        (should (stringp (plist-get result :reason)))))))

(mevedel-deftest mevedel-sandbox--canonical-directories ()
  ,test
  (test)
  :doc "canonical writable roots:
`mevedel-sandbox--canonical-directories' drops missing and duplicate roots"
  (let* ((root (make-temp-file "mevedel-sandbox-canonical-" t))
         (missing (file-name-concat root "missing")))
    (unwind-protect
        (should
         (equal (mevedel-sandbox--canonical-directories
                 (list root root missing nil))
                (list (file-name-as-directory (file-truename root)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-sandbox--first-missing-path ()
  ,test
  (test)
  :doc "first missing component:
`mevedel-sandbox--first-missing-path' returns the narrow mount target"
  (let ((root (make-temp-file "mevedel-sandbox-missing-" t)))
    (unwind-protect
        (should
         (equal (mevedel-sandbox--first-missing-path
                 (file-name-concat root "absent" "child"))
                (file-name-concat root "absent")))
      (delete-directory root t))))

(mevedel-deftest mevedel-sandbox--writable-symlink-component ()
  ,test
  (test)
  :doc "writable symlink crossing:
`mevedel-sandbox--writable-symlink-component' identifies mutable indirection"
  (let* ((root (make-temp-file "mevedel-sandbox-symlink-" t))
         (outside (make-temp-file "mevedel-sandbox-symlink-target-" t))
         (link (file-name-concat root "link")))
    (unwind-protect
        (progn
          (make-symbolic-link outside link)
          (should
           (equal (mevedel-sandbox--writable-symlink-component
                   (file-name-concat link "secret") (list root))
                  link))
          (should-not
           (mevedel-sandbox--writable-symlink-component
            (file-name-concat link "secret") (list outside))))
      (delete-directory root t)
      (delete-directory outside t))))

(mevedel-deftest mevedel-sandbox--git-pointer-target ()
  ,test
  (test)
  :doc "Git directory pointer:
`mevedel-sandbox--git-pointer-target' resolves a relative worktree target"
  (let* ((root (make-temp-file "mevedel-sandbox-gitdir-" t))
         (checkout (file-name-concat root "checkout"))
         (metadata (file-name-concat root "metadata"))
         (pointer (file-name-concat checkout ".git")))
    (unwind-protect
        (progn
          (make-directory checkout)
          (make-directory metadata)
          (with-temp-file pointer
            (insert "gitdir: ../metadata\n"))
          (should
           (equal (mevedel-sandbox--git-pointer-target pointer)
                  metadata)))
      (delete-directory root t))))

(mevedel-deftest mevedel-sandbox--protected-candidates ()
  ,test
  (test)
  :doc "protected glob expansion:
`mevedel-sandbox--protected-candidates' finds concrete and missing roots"
  (let* ((root (make-temp-file "mevedel-sandbox-candidates-" t))
         (secondary (make-temp-file "mevedel-sandbox-secondary-" t))
         (nested (file-name-concat root "nested"))
         (dot-git (file-name-concat nested ".git"))
         (secondary-dot-git (file-name-concat secondary "nested" ".git"))
         (unreadable (file-name-concat secondary "unreadable"))
         (credentials (file-name-concat root "credentials"))
         (missing (file-name-concat root "missing"))
         (mevedel-protected-paths
          `(("**/.git/**" . read-only)
            (,(concat credentials "/**") . inaccessible)
            (,(concat missing "/**") . inaccessible))))
    (unwind-protect
        (progn
          (make-directory dot-git t)
          (make-directory secondary-dot-git t)
          (make-directory unreadable)
          (set-file-modes unreadable #o000)
          (make-directory credentials)
          (let ((candidates
                 (mevedel-sandbox--protected-candidates
                  root (list root secondary))))
            (should (cl-find dot-git candidates
                             :key (lambda (item) (plist-get item :path))
                             :test #'string-equal))
            (should (cl-find secondary-dot-git candidates
                             :key (lambda (item) (plist-get item :path))
                             :test #'string-equal))
            (should
             (eq (plist-get
                  (cl-find unreadable candidates
                           :key (lambda (item) (plist-get item :path))
                           :test #'string-equal)
                  :mode)
                 'inaccessible))
            (should (eq (plist-get
                         (cl-find credentials candidates
                                  :key (lambda (item) (plist-get item :path))
                                  :test #'string-equal)
                         :mode)
                        'inaccessible))
            (should (cl-find missing candidates
                             :key (lambda (item) (plist-get item :path))
                             :test #'string-equal))))
      (delete-directory root t)
      (set-file-modes unreadable #o700)
      (delete-directory secondary t))))

(mevedel-deftest mevedel-sandbox-cleanup ()
  ,test
  (test)
  :doc "owned mount target cleanup:
`mevedel-sandbox-cleanup' removes an unchanged empty synthetic directory"
  (let* ((root (make-temp-file "mevedel-sandbox-cleanup-" t))
         (path (file-name-concat root "synthetic")))
    (unwind-protect
        (progn
          (make-directory path)
          (let ((inode (file-attribute-inode-number
                        (file-attributes path 'string))))
            (mevedel-sandbox-cleanup
             (list :cleanup-paths (list (list :path path :inode inode)))))
          (should-not (file-exists-p path)))
      (delete-directory root t))))

(mevedel-deftest mevedel-sandbox--protected-restrictions ()
  ,test
  (test)
  :doc "mount layering:
`mevedel-sandbox--protected-restrictions' masks after writable roots"
  (let* ((root (make-temp-file "mevedel-sandbox-restrictions-" t))
         (dot-git (file-name-concat root ".git"))
         (credentials (file-name-concat root "credentials"))
         (missing (file-name-concat root "missing"))
         (mevedel-protected-paths
          `((,(concat dot-git "/**") . read-only)
            (,(concat credentials "/**") . inaccessible)
            (,(concat missing "/**") . inaccessible)))
         restrictions)
    (unwind-protect
        (progn
          (make-directory dot-git)
          (make-directory credentials)
          (setq restrictions
                (mevedel-sandbox--protected-restrictions root (list root)))
          (let ((arguments (plist-get restrictions :arguments)))
            (should (member dot-git arguments))
            (should (member "--ro-bind" arguments))
            (should (member credentials arguments))
            (should (member "--tmpfs" arguments))
            (should (member missing arguments)))
          (should (file-directory-p missing))
          (mevedel-sandbox-cleanup restrictions)
          (should-not (file-exists-p missing)))
      (mevedel-sandbox-cleanup restrictions)
      (delete-directory root t)))
  :doc "Git pointer target:
`mevedel-sandbox--protected-restrictions' protects worktree metadata targets"
  (let* ((root (make-temp-file "mevedel-sandbox-pointer-root-" t))
         (metadata (make-temp-file "mevedel-sandbox-pointer-meta-" t))
         (pointer (file-name-concat root ".git"))
         (mevedel-protected-paths '(("**/.git/**" . read-only))))
    (unwind-protect
        (progn
          (with-temp-file pointer
            (insert (format "gitdir: %s\n" metadata)))
          (let* ((restrictions
                  (mevedel-sandbox--protected-restrictions root (list root)))
                 (arguments (plist-get restrictions :arguments)))
            (should (member pointer arguments))
            (should (member metadata arguments))))
      (delete-directory root t)
      (delete-directory metadata t)))
  :doc "writable symlink ambiguity:
`mevedel-sandbox--protected-restrictions' refuses a mutable symlink crossing"
  (let* ((root (make-temp-file "mevedel-sandbox-restriction-link-" t))
         (outside (make-temp-file "mevedel-sandbox-restriction-out-" t))
         (link (file-name-concat root "link"))
         (mevedel-protected-paths
          `((,(concat link "/secret/**") . inaccessible))))
    (unwind-protect
        (progn
          (make-symbolic-link outside link)
          (should-error
           (mevedel-sandbox--protected-restrictions root (list root))
           :type 'mevedel-sandbox-policy-error))
      (delete-directory root t)
      (delete-directory outside t)))
  :doc "root sibling containment:
`mevedel-sandbox--protected-restrictions' does not mutate a lexical prefix"
  (let* ((parent (make-temp-file "mevedel-sandbox-prefix-" t))
         (root (file-name-concat parent "work"))
         (sibling (file-name-concat parent "work-other"))
         (protected (file-name-concat sibling "missing"))
         (mevedel-protected-paths
          `((,(concat protected "/**") . inaccessible))))
    (unwind-protect
        (progn
          (make-directory root)
          (make-directory sibling)
          (let ((restrictions
                 (mevedel-sandbox--protected-restrictions root (list root))))
            (should-not (file-exists-p protected))
            (should-not (member protected
                                (plist-get restrictions :arguments)))))
      (delete-directory parent t))))

(mevedel-deftest mevedel-sandbox--unrestricted-facts ()
  ,test
  (test)
  :doc "unrestricted facts:
`mevedel-sandbox--unrestricted-facts' discloses both weakened boundaries"
  (should
   (equal (mevedel-sandbox--unrestricted-facts 'off "disabled")
          '(:sandbox off :filesystem unrestricted :network unrestricted
            :reason "disabled"))))

(mevedel-deftest mevedel-sandbox--direct-preparation ()
  ,test
  (test)
  :doc "direct preparation:
`mevedel-sandbox--direct-preparation' records and returns unrestricted facts"
  (let ((mevedel-sandbox--last-facts nil))
    (let ((prepared
           (mevedel-sandbox--direct-preparation
            '("true") 'unavailable "probe failed")))
      (should (eq (plist-get prepared :state) 'unrestricted))
      (should (equal (plist-get prepared :command) '("true")))
      (should (eq (plist-get prepared :facts)
                  mevedel-sandbox--last-facts)))))

(mevedel-deftest mevedel-sandbox--confined-preparation ()
  ,test
  (test)
  :doc "confined preparation:
`mevedel-sandbox--confined-preparation' canonicalizes authority and adds marker"
  (let ((root (make-temp-file "mevedel-sandbox-confined-" t))
        (mevedel-sandbox--last-facts nil))
    (unwind-protect
        (let ((prepared
               (mevedel-sandbox--confined-preparation
                '("true") root (list root) "/test/bwrap" nil)))
          (should (eq (plist-get prepared :state) 'confined))
          (should (equal (car (plist-get prepared :command)) "/test/bwrap"))
          (should (string-prefix-p "MEVEDEL_SANDBOX_STARTED_"
                                   (plist-get prepared :marker)))
          (should (eq (plist-get prepared :facts)
                      mevedel-sandbox--last-facts)))
      (delete-directory root t))))

(mevedel-deftest mevedel-sandbox-prepare ()
  ,test
  (test)
  :doc "available backend:
`mevedel-sandbox-prepare' builds the confined read-only-host profile"
  (let* ((executable (or (executable-find "bwrap") "bwrap"))
         (root (make-temp-file "mevedel-sandbox-root-" t))
         (workdir (file-name-as-directory root))
         (mevedel-sandbox-mode 'auto)
         (mevedel-sandbox--probe-cache
          (list :available t :executable executable))
         (prepared
          (mevedel-sandbox-prepare
           '("sh" "-c" "printf ok") workdir (list root))))
    (unwind-protect
        (let ((command (plist-get prepared :command)))
          (should (eq (plist-get prepared :state) 'confined))
          (should (equal (car command) executable))
          (should (member "--ro-bind" command))
          (should (member "--bind" command))
          (should (member "--unshare-user" command))
          (should (member "--unshare-pid" command))
          (should (member "--unshare-net" command))
          (should (member "--proc" command))
          (should (member "--chdir" command))
          (should (equal (plist-get (plist-get prepared :facts) :network)
                         'isolated)))
      (delete-directory root t)))
  :doc "automatic fallback:
`mevedel-sandbox-prepare' discloses unrestricted direct execution"
  (let ((mevedel-sandbox-mode 'auto)
        (mevedel-sandbox--probe-cache
         '(:available nil :reason "test backend unavailable")))
    (let ((prepared
           (mevedel-sandbox-prepare '("true") temporary-file-directory nil)))
      (should (equal (plist-get prepared :command) '("true")))
      (should (eq (plist-get prepared :state) 'unrestricted))
      (should (eq (plist-get (plist-get prepared :facts) :network)
                  'unrestricted))
      (should (string-match-p
               "test backend unavailable"
               (plist-get (plist-get prepared :facts) :reason)))))
  :doc "additive network profile:
`mevedel-sandbox-prepare' changes only network isolation"
  (let* ((root (make-temp-file "mevedel-sandbox-network-" t))
         (mevedel-sandbox-mode 'required)
         (mevedel-sandbox--probe-cache
          '(:available t :executable "/test/bwrap"))
         (default
          (mevedel-sandbox-prepare '("true") root (list root)))
         (network
          (mevedel-sandbox-prepare
           '("true") root (list root) '(:network t))))
    (unwind-protect
        (let ((default-command (plist-get default :command))
              (network-command (plist-get network :command)))
          (should (member "--unshare-net" default-command))
          (should-not (member "--unshare-net" network-command))
          (let ((default-without-network
                 (delete "--unshare-net" (copy-sequence default-command))))
            (setcar (member (plist-get default :marker)
                            default-without-network)
                    "SANDBOX_MARKER")
            (setcar (member (plist-get network :marker) network-command)
                    "SANDBOX_MARKER")
            (should (equal default-without-network network-command)))
          (should (eq 'isolated
                      (plist-get (plist-get default :facts) :network)))
          (should (eq 'unrestricted
                      (plist-get (plist-get network :facts) :network)))
          (should (equal (plist-get (plist-get default :facts)
                                    :writable-roots)
                         (plist-get (plist-get network :facts)
                                    :writable-roots)))
          (should (= (plist-get (plist-get default :facts)
                                :protected-paths)
                     (plist-get (plist-get network :facts)
                                :protected-paths))))
      (mevedel-sandbox-cleanup default)
      (mevedel-sandbox-cleanup network)
      (delete-directory root t)))
  :doc "additive filesystem profile:
exact read and write mounts follow protected masks without broadening siblings"
  (let* ((root (make-temp-file "mevedel-sandbox-filesystem-" t))
         (dot-git (file-name-concat root ".git"))
         (config (file-name-concat dot-git "config"))
         (head (file-name-concat dot-git "HEAD"))
         (credentials (file-name-concat root "credentials"))
         (token (file-name-concat credentials "token"))
         (sibling (file-name-concat credentials "sibling"))
         (mevedel-protected-paths
          `(("**/.git/**" . read-only)
            (,(concat credentials "/**") . inaccessible)))
         (mevedel-sandbox-mode 'required)
         (mevedel-sandbox--probe-cache
          '(:available t :executable "/test/bwrap"))
         prepared)
    (unwind-protect
        (progn
          (make-directory dot-git)
          (make-directory credentials)
          (dolist (file (list config head token sibling))
            (with-temp-file file (insert "value")))
          (setq prepared
                (mevedel-sandbox-prepare
                 '("true") root (list root)
                 `(:file-system ((:path ,token :access read)
                                 (:path ,config :access write)))))
          (let ((command (plist-get prepared :command)))
            (should
             (cl-loop for tail on command
                      thereis (equal (seq-take tail 3)
                                     (list "--ro-bind-fd" "10" token))))
            (should
             (cl-loop for tail on command
                      thereis (equal (seq-take tail 3)
                                     (list "--bind-fd" "11" config))))
            (should (member "/test/bwrap" command))
            (should-not (member head command))
            (should-not (member sibling command))
            (should (member "--unshare-net" command))
            (should (= 2 (plist-get (plist-get prepared :facts)
                                    :additional-filesystem)))))
      (mevedel-sandbox-cleanup prepared)
      (delete-directory root t)))
  :doc "required backend:
`mevedel-sandbox-prepare' refuses execution when confinement is unavailable"
  (let ((mevedel-sandbox-mode 'required)
        (mevedel-sandbox--probe-cache
         '(:available nil :reason "required backend unavailable")))
    (let ((prepared
           (mevedel-sandbox-prepare '("true") temporary-file-directory nil)))
      (should-not (plist-get prepared :command))
      (should (eq (plist-get prepared :state) 'refused))
      (should (string-match-p "required backend unavailable"
                              (plist-get prepared :error)))))
  :doc "working-directory boundary:
`mevedel-sandbox-prepare' never converts invalid authority into auto fallback"
  (let* ((root (make-temp-file "mevedel-sandbox-authority-" t))
         (outside (make-temp-file "mevedel-sandbox-outside-" t))
         (mevedel-sandbox-mode 'auto)
         (mevedel-sandbox--probe-cache
          (list :available t
                :executable (or (executable-find "bwrap") "bwrap"))))
    (unwind-protect
        (let ((prepared
               (mevedel-sandbox-prepare '("true") outside (list root))))
          (should-not (plist-get prepared :command))
          (should (eq (plist-get prepared :state) 'refused))
          (should (string-match-p "outside writable roots"
                                  (plist-get prepared :error))))
      (delete-directory root t)
      (delete-directory outside t)))
  :doc "disabled backend:
`mevedel-sandbox-prepare' executes directly with visible disclosure"
  (let ((mevedel-sandbox-mode 'off)
        (mevedel-sandbox--probe-cache 'unconsulted))
    (let ((prepared
           (mevedel-sandbox-prepare '("true") temporary-file-directory nil)))
      (should (equal (plist-get prepared :command) '("true")))
      (should (eq (plist-get prepared :state) 'unrestricted))
      (should (eq mevedel-sandbox--probe-cache 'unconsulted))
      (should (eq (plist-get (plist-get prepared :facts) :sandbox) 'off))))
  :doc "real backend:
`mevedel-sandbox-prepare' permits one writable root and keeps a sibling read-only"
  (let ((mevedel-sandbox-mode 'required)
        (mevedel-sandbox--probe-cache nil))
    (let ((availability (mevedel-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (or (plist-get availability :reason)
                      "Bubblewrap unavailable")))
      (let* ((parent (make-temp-file "mevedel-sandbox-real-" t))
             (root (expand-file-name "allowed" parent))
             (outside (expand-file-name "outside" parent))
             (inside-file (expand-file-name "inside" root))
             (outside-file (expand-file-name "denied" outside))
             (descendant-command
              (format "printf inside > %s; printf outside > %s || true"
                      (shell-quote-argument inside-file)
                      (shell-quote-argument outside-file)))
             prepared output-buffer)
        (make-directory root)
        (make-directory outside)
        (unwind-protect
            (progn
              (setq prepared
                    (mevedel-sandbox-prepare
                     (list "sh" "-c"
                           (format
                            (concat "sh -c %s; "
                                    "test \"$(wc -l < /proc/net/route)\" -eq 1")
                            (shell-quote-argument descendant-command)))
                     root (list root)))
              (setq output-buffer (generate-new-buffer " *mevedel-bwrap-test*"))
              (should (zerop
                       (apply #'call-process
                              (car (plist-get prepared :command)) nil
                              output-buffer nil
                              (cdr (plist-get prepared :command)))))
              (should (file-exists-p inside-file))
              (should-not (file-exists-p outside-file))
              (with-current-buffer output-buffer
                (should (member (plist-get prepared :marker)
                                (split-string (buffer-string) "\n" nil)))))
          (when (buffer-live-p output-buffer)
            (kill-buffer output-buffer))
          (mevedel-sandbox-cleanup prepared)
          (delete-directory parent t)))))
  :doc "real additive network:
default confinement cannot reach a host listener but network escalation can"
  (let ((mevedel-sandbox-mode 'required)
        (mevedel-sandbox--probe-cache nil))
    (let ((availability (mevedel-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (or (plist-get availability :reason)
                      "Bubblewrap unavailable")))
      (let* ((root (make-temp-file "mevedel-sandbox-network-real-" t))
             (server (make-network-process
                      :name "mevedel-sandbox-network-test"
                      :server t :host "127.0.0.1" :service t :noquery t))
             (port (process-contact server :service))
             (command
              (list "bash" "-c"
                    (format "exec 3<>/dev/tcp/127.0.0.1/%d" port)))
             default network)
        (unwind-protect
            (progn
              (setq default
                    (mevedel-sandbox-prepare command root (list root)))
              (setq network
                    (mevedel-sandbox-prepare
                     command root (list root) '(:network t)))
              (should-not
               (zerop
                (apply #'call-process
                       (car (plist-get default :command)) nil nil nil
                       (cdr (plist-get default :command)))))
              (should
               (zerop
                (apply #'call-process
                       (car (plist-get network :command)) nil nil nil
                       (cdr (plist-get network :command))))))
          (when (processp server) (delete-process server))
          (mevedel-sandbox-cleanup default)
          (mevedel-sandbox-cleanup network)
          (delete-directory root t)))))
  :doc "real additive filesystem:
the named protected files reopen while parent and sibling restrictions remain"
  (let ((mevedel-sandbox-mode 'required)
        (mevedel-sandbox--probe-cache nil))
    (let ((availability (mevedel-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (or (plist-get availability :reason)
                      "Bubblewrap unavailable")))
      (let* ((root (make-temp-file "mevedel-sandbox-filesystem-real-" t))
             (dot-git (file-name-concat root ".git"))
             (config (file-name-concat dot-git "config"))
             (head (file-name-concat dot-git "HEAD"))
             (credentials (file-name-concat root "credentials"))
             (token (file-name-concat credentials "token"))
             (sibling (file-name-concat credentials "sibling"))
             (token-copy (file-name-concat root "token-copy"))
             (sibling-copy (file-name-concat root "sibling-copy"))
             (mevedel-protected-paths
              `(("**/.git/**" . read-only)
                (,(concat credentials "/**") . inaccessible)))
             prepared)
        (unwind-protect
            (progn
              (make-directory dot-git)
              (make-directory credentials)
              (with-temp-file config (insert "old-config"))
              (with-temp-file head (insert "old-head"))
              (with-temp-file token (insert "token-value"))
              (with-temp-file sibling (insert "sibling-value"))
              (let ((command
                     (list
                      "sh" "-c"
                      (format
                       (concat
                        "cat %s > %s; "
                        "printf new-config > %s; "
                        "if value=$(cat %s 2>/dev/null); then "
                        "printf '%%s' \"$value\" > %s; fi; "
                        "printf new-head > %s 2>/dev/null || true")
                       (shell-quote-argument token)
                       (shell-quote-argument token-copy)
                       (shell-quote-argument config)
                       (shell-quote-argument sibling)
                       (shell-quote-argument sibling-copy)
                       (shell-quote-argument head)))))
                (setq prepared
                      (mevedel-sandbox-prepare
                       command root (list root)
                       `(:file-system
                         ((:path ,token :access read)
                          (:path ,config :access write)))))
                (should
                 (zerop
                  (apply #'call-process
                         (car (plist-get prepared :command)) nil nil nil
                         (cdr (plist-get prepared :command))))))
              (should (equal "token-value"
                             (with-temp-buffer
                               (insert-file-contents token-copy)
                               (buffer-string))))
              (should (equal "new-config"
                             (with-temp-buffer
                               (insert-file-contents config)
                               (buffer-string))))
              (should-not (file-exists-p sibling-copy))
              (should (equal "old-head"
                             (with-temp-buffer
                               (insert-file-contents head)
                               (buffer-string)))))
          (mevedel-sandbox-cleanup prepared)
          (delete-directory root t)))))
  :doc "real protected paths:
`mevedel-sandbox-prepare' keeps Git readable, hides credentials, and guards missing roots"
  (let ((mevedel-sandbox-mode 'required)
        (mevedel-sandbox--probe-cache nil))
    (let ((availability (mevedel-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (or (plist-get availability :reason)
                      "Bubblewrap unavailable")))
      (let* ((root (make-temp-file "mevedel-sandbox-protected-real-" t))
             (dot-git (file-name-concat root ".git"))
             (head (file-name-concat dot-git "HEAD"))
             (credentials (file-name-concat root "credentials"))
             (secret (file-name-concat credentials "token"))
             (missing (file-name-concat root "missing"))
             (blocked (file-name-concat root "blocked"))
             (blocked-head (file-name-concat blocked ".git" "HEAD"))
             (read-result (file-name-concat root "git-head"))
             (leak-result (file-name-concat root "leaked"))
             (mevedel-protected-paths
              `(("**/.git/**" . read-only)
                (,(concat credentials "/**") . inaccessible)
                (,(concat missing "/**") . inaccessible)))
             prepared output-buffer)
        (unwind-protect
            (progn
              (make-directory dot-git)
              (make-directory credentials)
              (make-directory (file-name-directory blocked-head) t)
              (with-temp-file head (insert "ref: refs/heads/main\n"))
              (with-temp-file blocked-head
                (insert "ref: refs/heads/blocked\n"))
              (with-temp-file secret (insert "do-not-leak\n"))
              (set-file-modes blocked #o000)
              (let* ((descendant
                      (format
                       (concat "cat %s > %s; "
                               "printf mutate > %s 2>/dev/null || true; "
                               "if value=$(cat %s 2>/dev/null); then "
                               "printf '%%s' \"$value\" > %s; fi; "
                               "mkdir %s 2>/dev/null || true; "
                               "chmod 700 %s 2>/dev/null || true; "
                               "printf escaped > %s 2>/dev/null || true")
                       (shell-quote-argument head)
                       (shell-quote-argument read-result)
                       (shell-quote-argument head)
                       (shell-quote-argument secret)
                       (shell-quote-argument leak-result)
                       (shell-quote-argument missing)
                       (shell-quote-argument blocked)
                       (shell-quote-argument blocked-head)))
                     (command (list "sh" "-c"
                                    (format "sh -c %s"
                                            (shell-quote-argument descendant)))))
                (setq prepared
                      (mevedel-sandbox-prepare command root (list root))))
              (setq output-buffer
                    (generate-new-buffer " *mevedel-protected-bwrap-test*"))
              (should
               (zerop
                (apply #'call-process
                       (car (plist-get prepared :command)) nil output-buffer nil
                       (cdr (plist-get prepared :command)))))
              (should
               (equal
                (with-temp-buffer
                  (insert-file-contents read-result)
                  (buffer-string))
                "ref: refs/heads/main\n"))
              (should
               (equal
                (with-temp-buffer
                  (insert-file-contents head)
                  (buffer-string))
                "ref: refs/heads/main\n"))
              (should-not (file-exists-p leak-result)))
          (when (buffer-live-p output-buffer)
            (kill-buffer output-buffer))
          (mevedel-sandbox-cleanup prepared)
          (set-file-modes blocked #o700)
          (should
           (equal
            (with-temp-buffer
              (insert-file-contents blocked-head)
              (buffer-string))
            "ref: refs/heads/blocked\n"))
          (should-not (file-exists-p missing))
          (delete-directory root t))))))

(mevedel-deftest mevedel-sandbox-launch-failed-p ()
  ,test
  (test)
  :doc "pre-exec failure:
`mevedel-sandbox-launch-failed-p' permits fallback only before the marker"
  (let ((preparation '(:state confined :marker "private-start-marker")))
    (should (mevedel-sandbox-launch-failed-p
             preparation '(:exit-code 125 :timed-out-p nil :output "failed")))
    (should-not
     (mevedel-sandbox-launch-failed-p
      preparation
      '(:exit-code 125 :timed-out-p nil
        :output "private-start-marker\ncommand failed")))
    (should-not
     (mevedel-sandbox-launch-failed-p
      preparation '(:exit-code -1 :timed-out-p t :output "")))))

(mevedel-deftest mevedel-sandbox-strip-marker ()
  ,test
  (test)
  :doc "private marker:
`mevedel-sandbox-strip-marker' preserves command output and removes its line"
  (should
   (equal
    (plist-get
     (mevedel-sandbox-strip-marker
      '(:marker "private-start-marker")
      '(:exit-code 0 :output "notice\nprivate-start-marker\ncommand\n"))
     :output)
    "notice\ncommand\n")))

(mevedel-deftest mevedel-sandbox-status-text ()
  ,test
  (test)
  :doc "confined status:
`mevedel-sandbox-status-text' reports filesystem and network boundaries"
  (should
   (equal
    (mevedel-sandbox-status-text
     '(:sandbox bubblewrap :filesystem workspace-write :network isolated))
    "sandbox: bubblewrap; filesystem: workspace-write; network: isolated"))
  :doc "unrestricted status:
`mevedel-sandbox-status-text' includes the persistent fallback reason"
  (should
   (equal
    (mevedel-sandbox-status-text
     '(:sandbox unavailable :filesystem unrestricted :network unrestricted
       :reason "probe failed"))
    "sandbox: unavailable; filesystem: unrestricted; network: unrestricted; reason: probe failed")))

(mevedel-deftest mevedel-sandbox--record-launch-failure ()
  ,test
  (test)
  :doc "launch failure state:
`mevedel-sandbox--record-launch-failure' disables later probes and records facts"
  (let ((mevedel-sandbox--probe-cache '(:available t))
        (mevedel-sandbox--last-facts nil))
    (let ((facts
           (mevedel-sandbox--record-launch-failure
            '(:exit-code 125 :output "backend refused"))))
      (should-not (plist-get mevedel-sandbox--probe-cache :available))
      (should (eq facts mevedel-sandbox--last-facts))
      (should (eq (plist-get facts :sandbox) 'unavailable))
      (should (string-match-p "backend refused" (plist-get facts :reason))))))

(provide 'test-mevedel-sandbox)

;;; test-mevedel-sandbox.el ends here
