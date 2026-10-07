;;; loading.el --- Cold startup scenarios -*- lexical-binding: t -*-

;;; Commentary:
;; Runs only in Eask-isolated fresh subprocesses, with no test helpers.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'gptel-openai)
(require 'json)

(defconst mevedel-test-loading--root
  (expand-file-name "../.." (file-name-directory load-file-name)))
(defconst mevedel-test-loading--deferred
  '(mevedel-tool-exec mevedel-tool-exec-permission mevedel-tool-patch
    mevedel-tool-web mevedel-tool-fs-read mevedel-tool-fs-search
    mevedel-shared-editing mevedel-shared-library mevedel-view
    mevedel-collaboration-guest mevedel-collaboration-files
    mevedel-collaboration-artifact mevedel-collaboration-editing
    mevedel-collaboration-transport mevedel-claude-code mevedel-acp acp
    mevedel-mcp mevedel-claude-code-usage))

(defconst mevedel-test-loading--answer "cold stub answer")

(defun mevedel-test-loading--provider ()
  "Serve a local OpenAI-compatible endpoint and select it as the backend.
Every request receives `mevedel-test-loading--answer', streamed when asked."
  (let* ((reply
          (lambda (process request)
            (let* ((stream (string-match-p "\"stream\": *true" request))
                   (message `((role . "assistant")
                              (content . ,mevedel-test-loading--answer)))
                   (body
                    (if stream
                        (concat
                         "data: " (json-encode
                                   `((choices . [((index . 0) (delta . ,message))])))
                         "\n\ndata: " (json-encode
                                        `((choices . [((index . 0)
                                                       (delta . ,(make-hash-table))
                                                       (finish_reason . "stop"))])))
                         "\n\ndata: [DONE]\n\n")
                      (json-encode
                       `((choices . [((index . 0) (message . ,message)
                                      (finish_reason . "stop"))]))))))
              (process-send-string
               process
               (concat "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Type: "
                       (if stream "text/event-stream" "application/json")
                       "\r\n\r\n" body))
              (process-send-eof process))))
         (server
          (make-network-process
           :name "mevedel-loading-provider" :server t :host 'local :service t
           :family 'ipv4 :noquery t :coding 'binary
           :filter
           (lambda (process input)
             (let* ((request (concat (process-get process 'request) input))
                    (case-fold-search t)
                    (end (string-search "\r\n\r\n" request))
                    (length (and end (string-match "^content-length: *\\([0-9]+\\)" request)
                                 (string-to-number (match-string 1 request)))))
               (process-put process 'request request)
               (when (and end (string-match-p "^expect: *100-continue" request)
                          (not (process-get process 'continued)))
                 (process-put process 'continued t)
                 (process-send-string process "HTTP/1.1 100 Continue\r\n\r\n"))
               (when (and end (>= (- (length request) end 4) (or length 0))
                          (not (process-get process 'answered)))
                 (process-put process 'answered t)
                 (funcall reply process request)))))))
    (setq gptel-backend
          (gptel-make-openai "Cold stub"
            :host (format "127.0.0.1:%d" (process-contact server :service))
            :protocol "http" :stream t :key "unused")
          gptel-model 'gpt-4o-mini)))

(defun mevedel-test-loading--await (predicate)
  "Process events until PREDICATE holds."
  (with-timeout (15 (ert-fail "Cold scenario timed out"))
    (while (not (funcall predicate)) (accept-process-output nil 0.02))))

(defun mevedel-test-loading--session ()
  "Return the live session's data buffer."
  (seq-find (lambda (buffer)
              (and (buffer-local-value 'mevedel--session buffer)
                   (not (provided-mode-derived-p
                         (buffer-local-value 'major-mode buffer) 'mevedel-view-mode))))
            (buffer-list)))

(defun mevedel-test-loading--answers ()
  "Return how often the session transcript holds the stub answer."
  (with-current-buffer (mevedel-test-loading--session)
    (how-many (regexp-quote mevedel-test-loading--answer) (point-min) (point-max))))

(defun mevedel-test-loading--settle (answers)
  "Wait until the session is idle with ANSWERS stub answers."
  (mevedel-test-loading--await
   (lambda ()
     (and (mevedel-test-loading--session)
          (not (mevedel-turn-busy-p (mevedel-test-loading--session)))
          (= answers (mevedel-test-loading--answers))))))

(defun mevedel-test-loading--send (text)
  "Submit TEXT from the session view composer and await its answer."
  (let ((answers (mevedel-test-loading--answers)))
    (with-current-buffer (buffer-local-value
                          'mevedel--view-buffer (mevedel-test-loading--session))
      (goto-char (point-max))
      (insert text)
      (mevedel-view-send))
    (mevedel-test-loading--settle (1+ answers))))

(defun mevedel-test-loading--directive (command &optional answers)
  "Run directive COMMAND on a fresh directive in a project file.
With ANSWERS, wait until the directive session holds that many answers."
  (call-process "git" nil nil nil "init" "-q")
  (mevedel-install)
  (mevedel-test-loading--provider)
  (write-region "line one\n" nil "a.txt" nil 'silent)
  (with-current-buffer (find-file-noselect "a.txt")
    (mevedel--create-directive-in (current-buffer) (point-min) (line-end-position)
                                  nil "Append the word done")
    (goto-char (1+ (point-min)))
    ;; The offer to save rebinds `inhibit-message' and clears the echo area,
    ;; which batch Emacs prints as a bare newline.
    (cl-letf (((symbol-function 'save-some-buffers) #'ignore)
              ((symbol-function 'completing-read) (lambda (&rest _) ""))
              ((symbol-function 'y-or-n-p) #'always)
              ((symbol-function 'yes-or-no-p) #'always))
      (call-interactively command)
      (when answers (mevedel-test-loading--settle answers)))
    (set-buffer-modified-p nil)))

(defun mevedel-test-loading--entry-points ()
  "Return documented and `;;;###autoload' command candidates."
  (let (names)
    (with-temp-buffer
      (insert-file-contents (file-name-concat mevedel-test-loading--root "README.md"))
      (while (re-search-forward "^| `\\(mevedel[[:alnum:]-]*\\)`" nil t)
        (push (intern (match-string 1)) names)))
    (dolist (file (directory-files mevedel-test-loading--root t "\\`mevedel.*\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (while (re-search-forward "^;;;###autoload\n(\\(?:defun\\|define-[a-z-]*mode\\) \\([^ ()]+\\)" nil t)
          (push (intern (match-string 1)) names))))
    (delete-dups names)))

(defun mevedel-test-loading (scenario &optional state)
  "Exercise cold SCENARIO through installed entry points.
STATE names a directory that outlives the process; it defaults to a
temporary one."
  (let* ((root (or state (make-temp-file "mevedel-loading-state-" t)))
         (default-directory (file-name-as-directory root))
         (user-emacs-directory (file-name-concat root "emacs/"))
         (inhibit-message t))
    (unwind-protect
        (progn
          (require 'mevedel)
          (setq mevedel-user-dir (file-name-concat root "user/"))
          (pcase scenario
            ('commands
             ;; Documented commands and autoload cookies are M-x entry points
             ;; as soon as the package loads, also from a source checkout.
             (let ((unreachable (seq-remove #'commandp (mevedel-test-loading--entry-points)))
                   stale)
               ;; README tables also name options; every module tells them apart.
               (dolist (file (directory-files mevedel-test-loading--root nil
                                              "\\`mevedel.*\\.el\\'"))
                 (require (intern (file-name-base file))))
               (ert-info ((format "Unreachable commands: %S" unreachable))
                 (should-not (seq-filter #'commandp unreachable)))
               (mapatoms (lambda (symbol)
                           (when (and (string-prefix-p "mevedel" (symbol-name symbol))
                                      (autoloadp (symbol-function symbol)))
                             (push symbol stale))))
               (ert-info ((format "Autoloads their file does not define: %S" stale))
                 (should-not stale))))
            ('send
             (call-process "git" nil nil nil "init" "-q")
             (mevedel-install)
             (mevedel-test-loading--provider)
             (mevedel)
             (mevedel-test-loading--send "first cold message")
             ;; Memory writes load the patch implementation on demand.
             (should-not (featurep 'mevedel-tool-patch))
             (with-current-buffer (mevedel-test-loading--session)
               (call-interactively #'mevedel-save-session)))
            ('resume
             (mevedel-install)
             (mevedel-test-loading--provider)
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt table &rest _)
                          (seq-find (lambda (entry) (string-prefix-p "Resume" entry))
                                    (all-completions "" table)))))
               (mevedel))
             (should (= 1 (mevedel-test-loading--answers)))
             (mevedel-test-loading--send "resumed cold message")
             (with-current-buffer (mevedel-test-loading--session)
               (should (string-search "first cold message" (buffer-string)))))
            ('discuss
             (mevedel-test-loading--directive #'mevedel-discuss-directive 1))
            ('implement
             (mevedel-test-loading--directive #'mevedel-implement-directive 1))
            ('preview
             (mevedel-test-loading--directive #'mevedel-preview-directive-prompt))
            ('installation
             (mevedel-uninstall)
             (dotimes (_ 2)
               (mevedel-install)
               (should (functionp #'mevedel-create-directive))
               (should (commandp 'mevedel-compact))
               (should (commandp 'mevedel-claude-code-setup))
               (should (memq #'mevedel-journal-idle-session-opened mevedel-session-start-hook))
               (dolist (tool (mevedel-tool-all))
                 (should (functionp (mevedel-tool-handler tool)))
                 (should (stringp (mevedel-tool-prompt tool))))
               (should (> (length (mevedel-tool-all)) 35))
               (when after-focus-change-function (funcall after-focus-change-function))
               (set-face-attribute 'default nil :foreground (face-attribute 'default :foreground))
               (run-hook-with-args 'enable-theme-functions 'user)
               (dolist (feature mevedel-test-loading--deferred)
                 (ert-info ((format "Unexpected eager feature: %s" feature))
                   (should-not (featurep feature))))
               (dolist (spec mevedel-tool-repair--shape-advice)
                 (should (advice-member-p (nth 2 spec) (car spec))))
               (mevedel-uninstall)
               (dolist (spec mevedel-tool-repair--shape-advice)
                 (should-not (advice-member-p (nth 2 spec) (car spec))))
               (dolist (feature mevedel-test-loading--deferred)
                 (should-not (featurep feature)))))
            ('gptel
             (mevedel-install)
             (with-temp-buffer
               (setq-local gptel-backend (gptel-make-openai "Cold native" :key "unused")
                           gptel-model 'gpt-4o-mini)
               (let ((fsm (gptel-request "hello" :dry-run t)))
                 (should (plist-get (gptel-fsm-info fsm) :data))))
             (dolist (feature '(acp mevedel-acp mevedel-mcp mevedel-claude-code))
               (should-not (featurep feature))))
            ('chat
             (mevedel-install)
             (mevedel)
             (should (featurep 'mevedel-view))
             (should (cl-some (lambda (buffer)
                                (with-current-buffer buffer
                                  (derived-mode-p 'mevedel-view-mode)))
                              (buffer-list)))
             (should (featurep 'mevedel-journal-idle))
             (require 'mevedel-readiness)
             (dolist (feature '(acp mevedel-acp mevedel-mcp mevedel-claude-code
                               mevedel-claude-code-usage))
               (should-not (featurep feature))))
            ('tools
             (mevedel-install)
             ;; Persisted references recover the catalog before implementations.
             (mevedel-tool-clear-registry)
             (dolist (name '("Bash" "Eval" "Read" "Glob" "ApplyPatch" "WebFetch"))
               (should (mevedel-tool-ensure name)))
             (dolist (feature mevedel-test-loading--deferred)
               (should-not (featurep feature)))
             (require 'mevedel-permission-mode)
             (require 'mevedel-permission-rules)
             (let ((mevedel-permission-mode 'full-auto)
                   (mevedel-permission-rules nil) outcome)
               (funcall (mevedel-tool-check-permission-async (mevedel-tool-get "Eval"))
                        nil '(:expression "(+ 1 2)")
                        (lambda (value) (setq outcome value)))
               (should (eq 'allow outcome)))
             (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "cold"))
                   result)
               (with-temp-buffer
                 (setq-local mevedel--session session)
                 (funcall (gptel-tool-function
                           (mevedel-tool-gptel-tool (mevedel-tool-ensure "ListExecutions")))
                          (lambda (value) (setq result value))))
               (should (stringp result))
               (should-not (string-prefix-p "Error:" result)))
             (should (featurep 'mevedel-tool-exec))
             (let ((file (file-name-concat root "sample.txt")))
               (write-region "cold read evidence\n" nil file nil 'silent)
               (let (result)
                 (funcall (mevedel-tool-handler (mevedel-tool-ensure "Read"))
                          (lambda (value) (setq result value)) (list :file_path file))
                 (should (string-match-p "cold read evidence" (plist-get result :result)))))
             (let ((tool (mevedel-tool-ensure "ApplyPatch")))
               (should (listp (funcall (mevedel-tool-get-paths tool)
                                      '(:patch "*** Begin Patch\n*** Add File: cold.txt\n+hello\n*** End Patch")))))
             (let ((tool (mevedel-tool-ensure "WebFetch")))
               (should (equal "example.com"
                              (funcall (mevedel-tool-get-domain tool)
                                       '(:url "https://example.com/path"))))))
            ('renderers
             (mevedel-install)
             (dolist (row '(("Read" (:file_path "sample.txt") nil)
                            ("Glob" (:pattern "*.el") nil)
                            ("Bash" (:command "printf hello") nil)
                            ("ApplyPatch" nil (:kind patch :files nil))
                            ("WebFetch" (:url "https://example.com") nil)))
               (let* ((tool (mevedel-tool-ensure (car row)))
                      (renderer (mevedel-tool-renderer tool)))
                 (when (listp renderer) (setq renderer (cdr (assq 'success renderer))))
                 (should (plist-get (funcall renderer (car row) (cadr row) "hello" (caddr row))
                                    :header)))))
            ('claude
             (let* ((backend (mevedel-claude-code-register))
                    (gptel-backend backend) (gptel-model 'sonnet)
                    (gptel-tools nil) response)
               (should (mevedel-engine-external-p backend))
               (should-not (featurep 'acp))
               ;; Substitute the paid launcher only; real generic dispatch,
               ;; transport loading, ACP protocol, and callbacks run unchanged.
               (autoload 'mevedel-claude-code-launch "mevedel-claude-code")
               (require 'mevedel-claude-code-backend)
               (advice-add
                'mevedel-claude-code-launch :override
                (lambda (&rest _)
                  (list :command (executable-find "python3")
                        :args (list (file-name-concat mevedel-test-loading--root
                                                     "test/fixtures/acp-agent.py"))
                        :cwd root)))
               (mevedel-engine-request-text
                backend "hello" "system" (lambda (text _info) (setq response text)))
               (with-timeout (5 (ert-fail "Claude fixture timed out"))
                 (while (not response) (accept-process-output nil 0.01)))
               (should (equal "answer:hello" response))
               (should (featurep 'acp))
               (let (done text)
                 (mevedel-claude-code-request-workload
                  "workload" "system" nil
                  (lambda (value _info)
                    (if (eq value t) (setq done t)
                      (when (stringp value) (push value text)))) nil nil)
                 (with-timeout (5 (ert-fail "Claude workload fixture timed out"))
                   (while (not done) (accept-process-output nil 0.01)))
                 (should (equal "answer:workload" (apply #'concat (nreverse text)))))))
            ('lobby
             (require 'mevedel-collaboration-lobby)
             (mevedel-collaboration-lobby-restore)
             (make-directory mevedel-user-dir t)
             (dolist (text '("" "nil" "(" "42"))
               (with-temp-file (mevedel-collaboration-lobby--intent-path) (insert text))
               (mevedel-collaboration-lobby-restore)
               (should-not (featurep 'mevedel-collaboration-guest)))
             (mevedel-collaboration-lobby--set-intended default-directory t)
             (autoload 'mevedel-collaboration--transport-open "mevedel-collaboration-transport")
             (advice-add 'mevedel-collaboration--transport-open :override
                         (lambda (&rest _) (error "Fixture relay unavailable")))
             (let (warnings)
               (cl-letf (((symbol-function 'display-warning)
                          (lambda (&rest args) (push args warnings))))
                 (mevedel-collaboration-lobby-restore))
               (should (featurep 'mevedel-collaboration-guest))
               (should warnings)
               (should (equal (list default-directory) (mevedel-collaboration-lobby--intended))))
             (let ((opened 0))
               (cl-letf (((symbol-function 'mevedel-collaboration--transport-open)
                          (lambda (&rest _) (cl-incf opened) nil)))
                 (mevedel-collaboration-lobby-restore)
                 (mevedel-collaboration-lobby-restore)
                 (should (= opened 1))
                 (mevedel-collaboration-lobby-stop)
                 (should-not (mevedel-collaboration-lobby--intended)))))))
      (dolist (buffer (buffer-list))
        (when (buffer-local-value 'mevedel--session buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (mevedel-uninstall)
      (unless state (delete-directory root t)))))
