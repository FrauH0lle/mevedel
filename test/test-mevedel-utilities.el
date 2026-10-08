;;; tests/test-mevedel-utilities.el -- Unit tests for mevedel-utilities.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'saveplace)
(require 'mevedel-execution-target)
(require 'mevedel-session-control-fs)
(require 'mevedel-structs)
(require 'mevedel-tool-render-data)
(require 'mevedel-transcript)
(require 'mevedel-utilities)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(eval-when-compile (require 'tramp))

(mevedel-deftest mevedel--run-periodic-timer
  (:doc "shares one host wakeup and dispatches all due callbacks once")
  (let ((mevedel--coalesced-timers nil)
        (mevedel--coalesced-timer nil)
        first second calls)
    (unwind-protect
        (progn
          (setq first (mevedel--run-periodic-timer 1 (lambda () (push 'first calls)))
                second (mevedel--run-periodic-timer 1 (lambda () (push 'second calls))))
          (dolist (timer (list first second))
            (let ((due (float-time (timer--time timer))))
              (should (= due (floor due)))))
          (should-not (memq first timer-list))
          (should-not (memq second timer-list))
          (should (mevedel--ui-timer-pending-p first))
          (should (memq mevedel--coalesced-timer (default-toplevel-value 'timer-list)))
          (timer-set-time first (time-subtract nil 1) 1)
          (timer-set-time second (time-subtract nil 1) 1)
          (mevedel--coalesced-timer-tick)
          (should (equal '(first second) calls))
          (should (time-less-p nil (timer--time first)))
          (should (time-less-p nil (timer--time second))))
      (mevedel--ui-timer-cancel first)
      (mevedel--ui-timer-cancel second))
    (should-not mevedel--coalesced-timer)))

(mevedel-deftest mevedel--coalesced-timer-tick ()
  ,test
  (test)
  :doc "cancellation during dispatch prevents an already-due callback"
  (let ((mevedel--coalesced-timers nil)
        (mevedel--coalesced-timer nil)
        first second called)
    (unwind-protect
        (progn
          (setq second (mevedel--run-periodic-timer 1 (lambda () (setq called t)))
                first (mevedel--run-periodic-timer
                       1 (lambda () (mevedel--ui-timer-cancel second))))
          (timer-set-time first (time-subtract nil 1) 1)
          (timer-set-time second (time-subtract nil 1) 1)
          (mevedel--coalesced-timer-tick)
          (should-not called)
          (should-not (mevedel--ui-timer-pending-p second)))
      (mevedel--ui-timer-cancel first)
      (mevedel--ui-timer-cancel second)))

  :doc "one-shot work can rearm itself while another callback fails"
  (let ((mevedel--coalesced-timers nil)
        (mevedel--coalesced-timer nil)
        (debug-on-error nil)
        good bad calls notices)
    (unwind-protect
        (progn
          (setq good (timer-create)
                bad (mevedel--run-periodic-timer 1 (lambda () (error "Expected failure"))))
          (timer-set-time good (time-subtract nil 1))
          (timer-set-function good (lambda ()
                                     (push 'good calls)
                                     (timer-set-time good (time-add nil 5))
                                     (mevedel--ui-timer-activate good t)))
          (mevedel--ui-timer-activate good t)
          (timer-set-time bad (time-subtract nil 1) 1)
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) notices))))
            (mevedel--coalesced-timer-tick))
          (should (equal '(good) calls))
          (should (= 1 (length notices)))
          (should (string-match-p "Expected failure" (car notices)))
          (should (mevedel--ui-timer-pending-p good)))
      (mevedel--ui-timer-cancel good)
      (mevedel--ui-timer-cancel bad))
    (should-not mevedel--coalesced-timer))

  :doc "yields between callbacks for pending input without dropping due work"
  (let ((mevedel--coalesced-timers nil)
        (mevedel--coalesced-timer nil)
        (pending nil) (calls 0) first second)
    (unwind-protect
        (cl-letf (((symbol-function 'input-pending-p) (lambda () pending)))
          (setq second (mevedel--run-periodic-timer 1 (lambda () (cl-incf calls)))
                first (mevedel--run-periodic-timer
                       1 (lambda () (cl-incf calls) (setq pending t))))
          (timer-set-time first (time-subtract nil 1) 1)
          (timer-set-time second (time-subtract nil 1) 1)
          (mevedel--coalesced-timer-tick)
          (should (= 1 calls))
          (should (mevedel--ui-timer-pending-p second))
          (should (time-less-p (timer--time second) nil))
          (setq pending nil)
          (mevedel--coalesced-timer-tick)
          (should (= 2 calls)))
      (mevedel--ui-timer-cancel first)
      (mevedel--ui-timer-cancel second))))

(mevedel-deftest mevedel--coalesced-timer-arm
  (:doc "owns one persistent host timer across temporary transport bindings")
  (let ((mevedel--coalesced-timers nil)
        (mevedel--coalesced-timer nil)
        owned clock)
    (unwind-protect
        (let ((timer-list nil))
          (setq owned (mevedel--run-periodic-timer 1 #'ignore)
                clock mevedel--coalesced-timer)
          (should (memq clock (default-toplevel-value 'timer-list)))
          (should (mevedel--ui-timer-pending-p owned))
          (mevedel--coalesced-timer-arm)
          (should (eq clock mevedel--coalesced-timer))
          (mevedel--ui-timer-cancel owned)
          (should-not (memq clock (default-toplevel-value 'timer-list)))
          (should-not mevedel--coalesced-timer))
      (mevedel--ui-timer-cancel owned))))

(mevedel-deftest mevedel--coalesced-timer-call
  (:doc "restores the caller's current buffer after a callback changes it")
  (with-temp-buffer
    (let ((caller (current-buffer))
          (other (generate-new-buffer " *mevedel-timer-other*"))
          (timer (timer-create)))
      (unwind-protect
          (progn
            (timer-set-function timer (lambda () (set-buffer other)))
            (mevedel--coalesced-timer-call timer)
            (should (eq caller (current-buffer))))
        (kill-buffer other)))))

(mevedel-deftest mevedel--ui-timer-activate/untimed
  (:doc "rejects a coalesced timer without a time instead of breaking the queue")
  (let ((mevedel--coalesced-timers nil)
        (mevedel--coalesced-timer nil)
        (untimed (timer-create)))
    (timer-set-function untimed #'ignore)
    (should-error (mevedel--ui-timer-activate untimed t))
    (should-not mevedel--coalesced-timers)
    (should-not mevedel--coalesced-timer)))

(mevedel-deftest mevedel--ui-timer-activate
  (:doc "schedules on the host list during TRAMP without disturbing foreign timers")
  (let ((earlier (run-at-time 60 nil #'ignore))
        (later (run-at-time 180 nil #'ignore))
        (owned (timer-create)))
    (unwind-protect
        (progn
          (timer-set-time owned (time-add (current-time) (seconds-to-time 120)))
          (timer-set-function owned #'ignore)
          (with-tramp-suspended-timers
            (mevedel--ui-timer-activate owned)
            (should-not (memq owned timer-list))
            (should (equal (cl-remove-if-not
                            (lambda (timer) (memq timer (list earlier owned later)))
                            (default-toplevel-value 'timer-list))
                           (list earlier owned later))))
          (should (memq owned timer-list)))
      (mevedel--ui-timer-cancel owned)
      (cancel-timer earlier)
      (cancel-timer later))))

(mevedel-deftest mevedel--ui-timer-pending-p
  (:doc "recognizes a timer hidden by nested TRAMP bindings")
  (let ((owned (run-at-time 60 nil #'ignore)))
    (unwind-protect
        (with-tramp-suspended-timers
          (let ((timer-list nil))
            (should-not (mevedel--timer-pending-p owned))
            (should (mevedel--ui-timer-pending-p owned))))
      (mevedel--ui-timer-cancel owned))))

(mevedel-deftest mevedel--ui-timer-cancel
  (:doc "removes only the owned outer timer on stop inside suspension")
  (let ((owned (run-at-time 60 nil #'ignore))
        (foreign (run-at-time 120 nil #'ignore)))
    (unwind-protect
        (progn
          (with-tramp-suspended-timers
            (mevedel--ui-timer-cancel owned)
            (should-not (memq owned (default-toplevel-value 'timer-list)))
            (should (memq foreign (default-toplevel-value 'timer-list))))
          (should-not (memq owned timer-list))
          (should (memq foreign timer-list)))
      (mevedel--ui-timer-cancel owned)
      (cancel-timer foreign))))

(defun test-mevedel-utilities--raw-bytes (&rest bytes)
  "Return BYTES as an Emacs string of raw byte characters."
  (apply #'string (mapcar #'unibyte-char-to-multibyte bytes)))

(defun test-mevedel-utilities--raw-byte-string-p (string)
  "Return non-nil for STRING with raw byte characters."
  (catch 'found
    (dotimes (index (length string))
      (when (eq (char-charset (aref string index)) 'eight-bit)
        (throw 'found t)))
    nil))

(mevedel-deftest mevedel-library-source-directory
  (:doc "resolves source siblings without mistaking compiled build files for roots")
  (let* ((root (make-temp-file "mevedel-library-layout-" t))
         (source (file-name-concat root "source"))
         (build (file-name-concat root "build"))
         (el (file-name-concat source "fixture.el"))
         (linked (file-name-concat build "fixture.el"))
         (elc (file-name-concat build "fixture.elc")))
    (unwind-protect
        (progn
          (make-directory source)
          (make-directory build)
          (with-temp-file el (insert ";; Source.\n"))
          (with-temp-file elc (insert "compiled fixture\n"))
          (should (equal (file-name-as-directory source)
                         (mevedel-library-source-directory el)))
          ;; A source-less installation keeps its own data root.
          (should (equal (file-name-as-directory build)
                         (mevedel-library-source-directory elc)))
          (make-symbolic-link el linked)
          (should (equal (file-name-as-directory source)
                         (mevedel-library-source-directory linked)))
          (should (equal (file-name-as-directory source)
                         (mevedel-library-source-directory elc)))
          ;; A dangling source link cannot redirect a usable compiled install.
          (delete-file el)
          (should (equal (file-name-as-directory build)
                         (mevedel-library-source-directory elc)))
          (delete-file linked)
          (with-temp-file linked (insert ";; Co-located source.\n"))
          (should (equal (file-name-as-directory build)
                         (mevedel-library-source-directory elc))))
      (delete-directory root t))))

(mevedel-deftest mevedel--diagnostic-value
  (:doc "copies nested data and preserves diagnostic keyword normalization")
  (let ((value (list :duplicate 1 :duplicate 2 :tail)))
    (should (equal '(:duplicate 2 :tail nil)
                   (mevedel--diagnostic-value value)))
    (should (equal '(:duplicate 1 :duplicate 2 :tail) value))))

(mevedel-deftest mevedel--diagnostic-entry-text
  (:doc "prints a complete readable line despite ambient print limits")
  (let ((print-length 1) (print-level 1) (print-quoted nil))
    (should (equal "(:nested (a b c) :quoted 'value)\n"
                   (mevedel--diagnostic-entry-text
                    '(:nested (a b c) :quoted (quote value)))))))

(mevedel-deftest mevedel--plain-data-p ()
  ,test
  (test)
  :doc "accepts nested read-safe values and rejects runtime objects"
  (should (mevedel--plain-data-p
           '(nil symbol car "text" 4 (dotted . pair) [1 "two"])))
  (should-not (mevedel--plain-data-p (lambda () t)))
  (should-not (mevedel--plain-data-p (make-hash-table))))

(mevedel-deftest mevedel--ordered-completion-table ()
  ,test
  (test)
  :doc "completes over the displays in their given order under CATEGORY"
  (let* ((displays '("2h ago       new" "yesterday    old"))
         (table (mevedel--ordered-completion-table displays 'mevedel-session))
         (metadata (cdr (funcall table "" nil 'metadata))))
    (should (equal displays (all-completions "" table)))
    (should (eq 'mevedel-session (alist-get 'category metadata)))
    (should (eq 'identity (alist-get 'display-sort-function metadata)))
    (should (eq 'identity (alist-get 'cycle-sort-function metadata)))))

(defvar gptel-include-tool-results)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-prefix-alist)
(defvar org-element-cache-persistent)

(mevedel-deftest mevedel--transcript-org-mode ()
  ,test
  (test)

  :doc "cold Org configuration survives without running on transcript storage"
  ;; A fresh child is needed: other tests may already have loaded Org.
  ;; It inherits Eask's isolated HOME and XDG roots.
  (let ((root (file-name-directory (locate-library "mevedel-utilities")))
        (emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-buffer
      (let ((status
             (call-process
              emacs nil t nil "--batch" "-Q" "-L" root
              "--eval"
              (prin1-to-string
               '(progn
                  (require 'mevedel-utilities)
                  (when (featurep 'org)
                    (error "Org must start unloaded"))
                  ;; Activate buffer-local binding machinery, as editor
                  ;; packages do before the first transcript is opened.
                  (with-temp-buffer
                    (make-local-variable 'after-change-major-mode-hook))
                  (let* ((ran nil)
                         (hook (lambda ()
                                 (setq ran t)
                                 (add-hook 'after-change-major-mode-hook
                                           #'ignore nil t))))
                    (with-eval-after-load 'org
                      (add-hook 'org-mode-hook hook))
                    (with-temp-buffer
                      (mevedel--transcript-org-mode)
                      (unless (derived-mode-p 'org-mode)
                        (error "Transcript did not enter Org mode")))
                    (when ran
                      (error "Cold-loaded user hook ran on transcript storage"))
                    (unless (memq hook (default-value 'org-mode-hook))
                      (error "Cold-loaded user hook was lost"))))))))
        (let ((hook-localization-warning
               (string-match-p
                "Making after-change-major-mode-hook buffer-local while locally let-bound!"
                (buffer-string))))
          (should-not hook-localization-warning))
        (should (equal "" (buffer-string)))
        (should (equal 0 status)))))

  :doc "transcript startup skips persistent Org cache without changing user settings"
  (progn
    (require 'org)
    (require 'org-element)
    (require 'org-persist)
    (let ((org-element-cache-persistent t) (reads 0))
      (cl-letf (((symbol-function 'org-persist-load)
                 (lambda (&rest _) (cl-incf reads))))
        (with-temp-buffer
          (insert "* Saved transcript\nAnswer\n")
          (mevedel--transcript-org-mode)
          (should (derived-mode-p 'org-mode))
          (should-not org-element-cache-persistent)))
      (should (= 0 reads))
      (should org-element-cache-persistent)))

  :doc "owns gptel's transcript shape regardless of the user's chat settings"
  (let ((gptel-prompt-prefix-alist '((org-mode . "*** ")))
        (gptel-response-prefix-alist '((org-mode . "Assistant: ")))
        (gptel-include-tool-results 'auto))
    (with-temp-buffer
      (mevedel--transcript-org-mode)
      (should (local-variable-p 'gptel-prompt-prefix-alist))
      (should-not gptel-prompt-prefix-alist)
      (should (local-variable-p 'gptel-response-prefix-alist))
      (should-not gptel-response-prefix-alist)
      (should (local-variable-p 'gptel-include-tool-results))
      (should (eq t gptel-include-tool-results)))
    (should (equal '((org-mode . "*** ")) gptel-prompt-prefix-alist))
    (should (eq 'auto gptel-include-tool-results)))

  :doc "suppresses org-indent-mode while transcript Org hooks run"
  (progn
    (require 'org)
    (with-temp-buffer
      (let ((org-mode-hook (list (lambda () (org-indent-mode +1))))
            (redraws 0))
        (cl-letf (((symbol-function 'redraw-display)
                   (lambda () (cl-incf redraws))))
          (mevedel--transcript-org-mode))
        (should (derived-mode-p 'org-mode))
        (should-not (bound-and-true-p org-indent-mode))
        (should (= 0 redraws)))))

  :doc "runs no mode hook, so a user Org setup cannot attach to storage"
  ;; Every minor mode attached here would run on each model and tool
  ;; insertion, and several of them reach the target on a remote workspace.
  (progn
    (require 'org)
    (with-temp-buffer
      (let* ((ran nil)
             (note (lambda (hook) (lambda () (push hook ran))))
             (org-mode-hook (list (funcall note 'org)))
             (text-mode-hook (list (funcall note 'text)))
             (outline-mode-hook (list (funcall note 'outline)))
             (change-major-mode-after-body-hook (list (funcall note 'body)))
             (after-change-major-mode-hook (list (funcall note 'after))))
        (setq-local change-major-mode-hook
                    (list (funcall note 'change)))
        (mevedel--transcript-org-mode)
        (should (derived-mode-p 'org-mode))
        (should-not ran)
        (should-not delayed-mode-hooks))))

  :doc "inhibits the Org startup block the mode body runs directly"
  ;; These are in the mode body, not a hook.  `org-inhibit-startup' covers
  ;; them.
  (progn
    (require 'org)
    (with-temp-buffer
      (let ((org-startup-truncated t)
            (org-startup-numerated t))
        (setq truncate-lines nil)
        (mevedel--transcript-org-mode)
        (should (derived-mode-p 'org-mode))
        (should-not truncate-lines)
        (should-not (bound-and-true-p org-num-mode)))))

  :doc "a Local Variables block in stored model output is not honoured"
  ;; These buffers visit files whose contents are model output.
  (progn
    (require 'org)
    (let ((file (make-temp-file "mevedel-transcript-" nil ".org"
                                "# Local Variables:\n# fill-column: 12\n# End:\n")))
      (unwind-protect
          (with-temp-buffer
            (insert-file-contents file)
            (setq buffer-file-name file)
            (unwind-protect
                (let ((fill-column 70))
                  (mevedel--transcript-org-mode)
                  (should (derived-mode-p 'org-mode))
                  (should (= 70 fill-column)))
              (setq buffer-file-name nil)))
        (delete-file file)))))

(mevedel-deftest mevedel--head-tail-preview-parts ()
  ,test
  (test)

  :doc "returns short content unchanged"
  (let ((preview (mevedel--head-tail-preview-parts "short" "short" 5 10)))
    (should (equal "short" (plist-get preview :text)))
    (should (= 0 (plist-get preview :omitted-chars))))

  :doc "uses bounded newline-aware parts and an exact character count"
  (let ((preview
         (mevedel--head-tail-preview-parts
          "1234\n67890" "abc\ndefghi" 30 10)))
    (should (equal
             "1234\n[mevedel: tool output truncated; omitted 20 chars]\nefghi"
             (plist-get preview :text)))
    (should (= 20 (plist-get preview :omitted-chars)))))

(mevedel-deftest mevedel--clamped-integer ()
  ,test
  (test)

  :doc "keeps in-range integers and clamps out-of-range ones"
  (should (= 500 (mevedel--clamped-integer 500 100 10 1000)))
  (should (= 10 (mevedel--clamped-integer 3 100 10 1000)))
  (should (= 1000 (mevedel--clamped-integer 4000 100 10 1000)))

  :doc "coerces floats and numeric strings"
  (should (= 500 (mevedel--clamped-integer 499.6 100 10 1000)))
  (should (= 500 (mevedel--clamped-integer "500" 100 10 1000)))
  (should (= 500 (mevedel--clamped-integer " 500.0 " 100 10 1000)))

  :doc "falls back to the default for absent or malformed values"
  (should (= 100 (mevedel--clamped-integer nil 100 10 1000)))
  (should (= 100 (mevedel--clamped-integer "fast" 100 10 1000)))
  (should (= 100 (mevedel--clamped-integer t 100 10 1000)))
  (should (= 10 (mevedel--clamped-integer nil 3 10 1000))))

(mevedel-deftest mevedel--timer-pending-p ()
  ,test
  (test)

  :doc "recognizes an armed timer and its cancellation"
  (let ((timer (run-at-time 3600 nil #'ignore)))
    (unwind-protect
        (should (mevedel--timer-pending-p timer))
      (cancel-timer timer))
    (should-not (mevedel--timer-pending-p timer)))

  :doc "rejects a timer discarded with a let-bound `timer-list'"
  (let (lost)
    (let (timer-list)
      (setq lost (run-at-time 3600 nil #'ignore)))
    (should (timerp lost))
    (should-not (mevedel--timer-pending-p lost)))

  :doc "recognizes a repeating timer from inside its own function"
  ;; Emacs marks it triggered while it runs; a caller that re-arms an
  ;; unscheduled timer from there must not start a duplicate.
  (let (timer inside)
    (setq timer (run-at-time 0 0.01 (lambda ()
                                      (setq inside (mevedel--timer-pending-p timer))
                                      (cancel-timer timer))))
    (with-timeout (2 (cancel-timer timer) (ert-fail "Repeating timer never ran"))
      (while (memq timer timer-list) (accept-process-output nil 0.01)))
    (should inside))

  :doc "counts a timer an exclusive section suspended"
  (require 'mevedel-transport)
  (let ((timer (run-at-time 3600 nil #'ignore)) inside)
    (unwind-protect
        (progn
          (mevedel-transport-with-exclusive-connection
            (setq inside (mevedel--timer-pending-p timer)))
          (should inside))
      (cancel-timer timer)))

  :doc "rejects values that are not timers"
  (progn
    (should-not (mevedel--timer-pending-p nil))
    (should-not (mevedel--timer-pending-p 'scheduled))))

(mevedel-deftest mevedel--invalid-message-char-p ()
  ,test
  (test)

  :doc "recognizes characters that are not Unicode scalar values"
  (should-not (mevedel--invalid-message-char-p ?a))
  (should-not (mevedel--invalid-message-char-p #x10ffff))
  (should (mevedel--invalid-message-char-p
           (aref (test-mevedel-utilities--raw-bytes #x80) 0)))
  (should (mevedel--invalid-message-char-p #xd800))
  (should (mevedel--invalid-message-char-p #xdfff))
  (should (mevedel--invalid-message-char-p #x110000)))

(mevedel-deftest mevedel--escape-invalid-message-chars ()
  ,test
  (test)

  :doc "escapes each invalid character as its uppercase UTF-8 bytes"
  (should
   (equal
    "a\\x80\\xED\\xA0\\x80\\xF4\\x90\\x80\\x80z"
    (mevedel--escape-invalid-message-chars
     (concat "a"
             (test-mevedel-utilities--raw-bytes #x80)
             (string #xd800 #x110000)
             "z")))))

(mevedel-deftest mevedel--normalize-message-text ()
  ,test
  (test)

  :doc "returns ASCII without scanning or copying its text and properties"
  (let* ((text (propertize (make-string (* 1024 1024) ?x) 'face 'bold))
         (matcher (symbol-function 'string-match-p))
         (scans 0))
    (cl-letf (((symbol-function 'string-match-p)
               (lambda (regexp string &optional start)
                 (when (eq string text) (cl-incf scans))
                 (funcall matcher regexp string start))))
      (should (eq text (mevedel--normalize-message-text text))))
    (should (zerop scans))
    (should (eq 'bold (get-text-property 0 'face text))))

  :doc "leaves valid large Unicode text and its properties without per-character Lisp work"
  (let* ((text (concat (make-string 100000 ?x)
                       (string 0 #x7f #x80 #xff #xd7ff #xe000 #x10ffff)))
         (predicate (symbol-function 'mevedel--invalid-message-char-p))
         (calls 0))
    (put-text-property 10 20 'gptel 'response text)
    (cl-letf (((symbol-function 'mevedel--invalid-message-char-p)
               (lambda (char) (cl-incf calls) (funcall predicate char))))
      (should (eq text (mevedel--normalize-message-text text))))
    (should (eq 'response (get-text-property 15 'gptel text)))
    (should (< calls 10)))

  :doc "decodes raw UTF-8 bytes into normal Unicode"
  (let* ((raw (test-mevedel-utilities--raw-bytes
               #xe2 #x80 #x9c ?x #xe2 #x80 #x9d))
         (normalized (mevedel--normalize-message-text raw)))
    (should (equal "“x”" normalized))
    (should-not (test-mevedel-utilities--raw-byte-string-p normalized)))

  :doc "preserves existing Unicode while decoding raw UTF-8 runs"
  (let* ((raw (concat "lambda λ "
                      (test-mevedel-utilities--raw-bytes
                       #xe2 #x80 #x94)
                      " dash"))
         (normalized (mevedel--normalize-message-text raw)))
    (should (equal "lambda λ — dash" normalized))
    (should-not (test-mevedel-utilities--raw-byte-string-p normalized)))

  :doc "escapes invalid raw bytes visibly"
  (let* ((raw (concat "bad "
                      (test-mevedel-utilities--raw-bytes #xff)
                      " byte"))
         (normalized (mevedel--normalize-message-text raw)))
    (should (equal "bad \\xFF byte" normalized))
    (should-not (test-mevedel-utilities--raw-byte-string-p normalized)))

  :doc "escapes UTF-8 decoded beyond Unicode's maximum"
  (let* ((text (decode-coding-string
                (unibyte-string #xf4 #x90 #x80 #x80) 'utf-8-unix t))
         (normalized (mevedel--normalize-message-text text)))
    (should (equal "\\xF4\\x90\\x80\\x80" normalized))
    (should (json-serialize normalized)))

  :doc "escapes surrogate code points"
  (dolist (case '((#xd800 . "\\xED\\xA0\\x80")
                  (#xdbff . "\\xED\\xAF\\xBF")
                  (#xdc00 . "\\xED\\xB0\\x80")
                  (#xdfff . "\\xED\\xBF\\xBF")))
    (let ((normalized (mevedel--normalize-message-text
                       (string (car case)))))
      (should (equal (cdr case) normalized))
      (should (json-serialize normalized)))))

(mevedel-deftest mevedel--path-alias-helpers ()
  ,test
  (test)

  :doc "same-file comparison accepts aliased parent directories"
  (let ((alias-root (expand-file-name "/alias/root"))
        (real-root (expand-file-name "/real/root")))
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (a b)
                 (let ((a (directory-file-name a))
                       (b (directory-file-name b)))
                   (or (and (equal a alias-root)
                            (equal b real-root))
                       (and (equal a real-root)
                            (equal b alias-root)))))))
      (should (mevedel--same-file-p
               (file-name-concat alias-root "source.el")
               (file-name-concat real-root "source.el")))))

  :doc "directory containment accepts aliased parent directories"
  (let ((alias-root (expand-file-name "/alias/root"))
        (real-root (expand-file-name "/real/root")))
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (a b)
                 (let ((a (directory-file-name a))
                       (b (directory-file-name b)))
                   (or (and (equal a alias-root)
                            (equal b real-root))
                       (and (equal a real-root)
                            (equal b alias-root)))))))
      (should (mevedel--file-in-directory-p
               (file-name-concat alias-root "source.el")
               (file-name-as-directory real-root)))))

  :doc "relative-name helper keeps aliased children relative"
  (let ((alias-root (expand-file-name "/alias/root"))
        (real-root (expand-file-name "/real/root")))
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (a b)
                 (let ((a (directory-file-name a))
                       (b (directory-file-name b)))
                   (or (and (equal a alias-root)
                            (equal b real-root))
                       (and (equal a real-root)
                            (equal b alias-root)))))))
      (should (equal "source.el"
                     (mevedel--file-relative-name-or-absolute
                      (file-name-concat alias-root "source.el")
                      (file-name-as-directory real-root))))))

  :doc "relative-name helper avoids plain relative paths across aliases"
  (let* ((alias-root (expand-file-name "/alias/root"))
         (real-root (expand-file-name "/real/root"))
         (alias-file (file-name-concat alias-root "source.el"))
         (original-file-in-directory-p
          (symbol-function 'file-in-directory-p)))
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (a b)
                 (let ((a (directory-file-name a))
                       (b (directory-file-name b)))
                   (or (and (equal a alias-root)
                            (equal b real-root))
                       (and (equal a real-root)
                            (equal b alias-root))))))
              ((symbol-function 'file-in-directory-p)
               (lambda (file directory)
                 (or (and (equal (directory-file-name file) alias-file)
                          (equal (directory-file-name directory) real-root))
                     (funcall original-file-in-directory-p file directory)))))
      (should (equal "source.el"
                     (mevedel--file-relative-name-or-absolute
                      alias-file
                      (file-name-as-directory real-root))))))

  :doc "macOS system volume var aliases stay inside var roots"
  (let ((actual-system-type system-type)
        (system-type 'darwin))
    (should (equal "/var/folders/k8/x/T/root/source.el"
                   (mevedel--file-macos-var-alias
                    "/System/Volumes/Data/private/var/folders/k8/x/T/root/source.el")))
    (should
     (equal "/var/folders/k8/x/T/root/source.el"
            (mevedel--file-macos-var-alias
             "/System/Volumes/Data/var/folders/k8/x/T/root/source.el")))
    (unless (eq actual-system-type 'windows-nt)
      (should
       (mevedel--file-in-directory-p
        "/System/Volumes/Data/private/var/folders/k8/x/T/root/.worktrees/foo/"
        "/var/folders/k8/x/T/root/.worktrees/"))
      (should (equal "source.el"
                     (mevedel--file-relative-name-or-absolute
                      "/System/Volumes/Data/private/var/folders/k8/x/T/root/source.el"
                      "/var/folders/k8/x/T/root/")))))

  :doc "Windows long-name aliases accept trailing directory arguments"
  (let* ((system-type 'windows-nt)
         (short-root (expand-file-name
                      "/runner/RUNNER~1/AppData/Local/Temp/root"))
         (long-root (expand-file-name
                     "/runner/runneradmin/AppData/Local/Temp/root")))
    (cl-letf (((symbol-function 'w32-long-file-name)
               (lambda (file)
                 (unless (string-suffix-p "/" file)
                   (let ((file (directory-file-name file)))
                     (cond
                      ((string-prefix-p short-root file)
                       (concat long-root
                               (substring file (length short-root))))
                      ((string-prefix-p long-root file)
                       file)))))))
      (should (equal "source.el"
                     (mevedel--file-relative-name-or-absolute
                      (file-name-concat long-root "source.el")
                      (file-name-as-directory short-root))))
      (should
       (mevedel--file-in-directory-p
        (file-name-concat long-root ".worktrees" "foo")
        (file-name-as-directory
         (file-name-concat short-root ".worktrees"))))))

  :doc "relative-name helper leaves outside files absolute"
  (let ((file (expand-file-name "/elsewhere/source.el")))
    (should (equal file
                   (mevedel--file-relative-name-or-absolute
                    file "/real/root/")))))

(mevedel-deftest mevedel--tint ()
  ,test
  (test)
  :doc "resolves noninteractive default-face colors without returning white"
  (should (equal "#ff7f7f" (mevedel--tint "unspecified-bg" "red" 0.5)))
  (should (equal "#7f7f7f" (mevedel--tint "unspecified-fg" "white" 0.5))))

(mevedel-deftest mevedel--environment-info-string ()
  ,test
  (test)
  :doc "renders cached target readiness facts without launching a process"
  (let* ((process-environment
          (cons "MEVEDEL_CLIENT_SECRET=do-not-forward" process-environment))
         (target (mevedel-execution-target-create
                  "/ssh:user@host:/srv/project/")))
    (setf (mevedel-execution-target-readiness target)
          '(:status ready
            :operating-system "Linux"
            :operating-system-version "6.8.0-target"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _args) (error "Unexpected executable lookup")))
              ((symbol-function 'process-file)
               (lambda (&rest _args) (error "Unexpected target process"))))
      (let ((result
             (mevedel--environment-info-string
              (mevedel-workspace--create
               :root "/ssh:user@host:/srv/project/")
              "/ssh:user@host:/srv/project/lib/"
              target)))
        (should (string-prefix-p "Execution target: ssh:user@host\n" result))
        (should (string-match-p "Working directory: /srv/project/lib/"
                                result))
        (should (string-match-p "Platform: linux" result))
        (should (string-match-p "OS Version: 6.8.0-target" result)))))

  :doc "identifies remote directories with or without a target and no readiness"
  (dolist (case '(("/ssh:user@host:/workspace/" . "ssh:user@host")
                  ("/docker:dev:/workspace/" . "docker:dev")
                  ("/podman:dev:/workspace/" . "podman:dev")
                  ("/ssh:jump|ssh:user@host:/workspace/" . "ssh:user@host")))
    (let ((target (mevedel-execution-target-create (car case)))
          unexpected-io)
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (&rest _)
                   (push 'executable-find unexpected-io)
                   (error "Unexpected executable lookup")))
                ((symbol-function 'process-file)
                 (lambda (&rest _)
                   (push 'process-file unexpected-io)
                   (error "Unexpected target process")))
                ((symbol-function 'tramp-send-command)
                 (lambda (&rest _)
                   (push 'tramp-send-command unexpected-io)
                   (error "Unexpected remote command")))
                ((symbol-function 'file-attributes)
                 (lambda (&rest _)
                   (push 'file-attributes unexpected-io)
                   (error "Unexpected filesystem probe"))))
        (dolist (supplied-target (list target nil))
          (let ((result (mevedel--environment-info-string
                         nil (car case) supplied-target)))
            (should (string-prefix-p
                     (format "Execution target: %s\nWorking directory: /workspace/\n"
                             (cdr case))
                     result))
            (should (string-match-p "Platform: unknown" result))
            (should (string-match-p "OS Version: unknown" result))))
        (should-not unexpected-io))))

  :doc "identifies a local directory without a session target"
  (let ((result (mevedel--environment-info-string nil temporary-file-directory)))
    (should (string-prefix-p "Execution target: local\nWorking directory: " result))
    (should (string-match-p (regexp-quote emacs-version) result))))

(mevedel-deftest mevedel--clear-user-turn-gptel-properties ()
  ,test
  (test)
  :doc "clears assistant metadata from inserted user transcript text"
  (with-temp-buffer
    (insert (propertize "Assistant answer.\n" 'gptel 'response))
    (let ((start (point)))
      (insert (propertize "\nUser follow-up\n"
                          'gptel 'response
                          'response t
                          'invisible t
                          'front-sticky '(gptel)))
      (mevedel--clear-user-turn-gptel-properties start (point))
      (should (eq 'response (get-text-property (point-min) 'gptel)))
      (goto-char start)
      (while (< (point) (point-max))
        (should-not (text-properties-at (point)))
        (forward-char 1))))

  :doc "clears copied view/tool properties from user transcript text"
  (with-temp-buffer
    (let ((start (point)))
      (insert (propertize "Bash: git diff\n"
                          'gptel '(tool . "call_1")
                          'read-only t
                          'keymap (make-sparse-keymap)
                          'mevedel-view-source '(1 . 42)
                          'mevedel-view-type 'tool-summary
                          'font-lock-face 'mevedel-view-tool-name))
      (mevedel--clear-user-turn-gptel-properties start (point))
      (goto-char start)
      (while (< (point) (point-max))
        (should-not (text-properties-at (point)))
        (forward-char 1))))

  :doc "preserves atomic mention bindings while clearing copied UI properties"
  (with-temp-buffer
    (let ((start (point))
          (binding '(:kind skill :token "$alpha"
                     :source-file "/tmp/alpha/SKILL.md")))
      (insert (propertize "$alpha"
                          'mevedel-mention-binding binding
                          'gptel 'response
                          'read-only t))
      (mevedel--clear-user-turn-gptel-properties start (point))
      (should (equal binding
                     (get-text-property start 'mevedel-mention-binding)))
      (should-not (get-text-property start 'gptel))
      (should-not (get-text-property start 'read-only))))

  :doc "preserves generated render provenance but not literal marker text"
  (with-temp-buffer
    (let (start block-start block-end literal-start literal-end)
      (setq start (point))
      (insert (propertize "Expanded prompt\n"
                          'gptel 'response
                          'response t
                          'invisible t
                          'front-sticky '(gptel)))
      (setq block-start (point))
      (insert (mevedel-tool-render-data-format
               '(:kind inline-skill :name "demo")))
      (setq block-end (point))
      (setq literal-start (point))
      (insert (substring-no-properties
               (mevedel-tool-render-data-format
                '(:kind inline-skill :name "literal"))))
      (setq literal-end (point))
      (set-text-properties literal-start literal-end nil)
      (mevedel--clear-user-turn-gptel-properties start (point))
      (goto-char start)
      (while (< (point) block-start)
        (should-not (get-text-property (point) 'gptel))
        (forward-char 1))
      (goto-char block-start)
      (while (< (point) block-end)
        (should (eq t (get-text-property (point) 'mevedel-render-data)))
        (should-not (eq 'response (get-text-property (point) 'gptel)))
        (should-not (get-text-property (point) 'response))
        (should-not (get-text-property (point) 'invisible))
        (should-not (get-text-property (point) 'front-sticky))
        (forward-char 1))
      (goto-char literal-start)
      (search-forward "<!-- mevedel-render-data -->" literal-end)
      (should-not (text-properties-at (match-beginning 0)))
      (should (string-search
               ":name \"literal\""
               (mevedel-tool-render-data-strip
                (buffer-string)))))))

(mevedel-deftest mevedel--hook-audit-helpers ()
  ,test
  (test)

  :doc "formats hook audit blocks with producer-specific provenance"
  (let* ((record
          `(:type prompt-rewrite
                  :event "UserPromptSubmit"
                  :submitted ,(propertize
                                "new <!-- /mevedel-hook-audit -->"
                                'face 'bold)
                  :nested (:original ,(propertize "old" 'face 'italic))))
         (block (mevedel--format-hook-audit-record record))
         parsed)
    (should (eq 'mevedel-hook-audit (get-text-property 0 'gptel block)))
    (should (eq t (get-text-property 0 'mevedel-hook-audit block)))
    (should (get-text-property 0 'invisible block))
    (with-temp-buffer
      (insert block)
      (goto-char (point-min))
      (search-forward mevedel--hook-audit-open)
      (let ((body-start (point)))
        (search-forward mevedel--hook-audit-close)
        (let ((payload (buffer-substring-no-properties
                        body-start (match-beginning 0))))
          (should-not (string-match-p
                       "<!-- /mevedel-hook-audit -->"
                       payload))
          (setq parsed (mevedel--read-hook-audit-record payload)))))
    (should-not (text-properties-at 0 (plist-get parsed :submitted)))
    (should-not (text-properties-at
                 0 (plist-get (plist-get parsed :nested) :original))))

  :doc "strips generated hook audit blocks from model-visible text"
  (let ((block (mevedel--format-hook-audit-record
                '(:type prompt-rewrite
                  :event "UserPromptSubmit"
                  :submitted "<!-- /mevedel-hook-audit --> tail"))))
    (should (equal "beforeafter"
                   (mevedel--strip-hook-audit-blocks
                    (concat "before" block "after")))))

  :doc "does not authorize property-free copied hook audit blocks"
  (with-temp-buffer
    (insert "before"
            (substring-no-properties
             (mevedel--format-hook-audit-record
              '(:type prompt-rewrite :event "UserPromptSubmit")))
            "after")
    (mevedel-transcript-restore-ignored-properties
     (point-min) (point-max))
    (goto-char (point-min))
    (search-forward mevedel--hook-audit-open)
    (should-not (get-text-property (match-beginning 0) 'gptel)))

  :doc "keeps trailing tool whitespace inside the ignored audit span"
  (with-temp-buffer
    (insert
     (propertize
      (concat
       "(:name \"Read\" :args nil)\n\nresult"
       (mevedel--format-hook-audit-record
        '(:type tool-input-repair :state committed))
       "\n")
      'gptel '(tool . "call-1")))
    (insert (propertize "#+end_tool\nThe next response."
                        'gptel 'ignore))
    (mevedel-transcript-restore-ignored-properties
     (point-min) (point-max))
    (goto-char (point-min))
    (search-forward mevedel--hook-audit-close)
    (while (looking-at-p "[ \t\r\n]")
      (should (eq 'mevedel-hook-audit
                  (get-text-property (point) 'gptel)))
      (should (eq t (get-text-property (point) 'mevedel-hook-audit)))
      (forward-char 1)))

  :doc "builds prompt rewrite audit records only when the prompt changed"
  (should-not
   (mevedel--hook-prompt-rewrite-audit-record
    'UserPromptSubmit "same" "same" "why"))
  (should
   (equal
    '(:type prompt-rewrite
            :event "UserPromptSubmit"
            :original "old"
            :submitted "new"
            :reason "why")
    (mevedel--hook-prompt-rewrite-audit-record
     'UserPromptSubmit "old" "new" "why"))))

(mevedel-deftest mevedel--tag-query-prefix-from-infix ()
  ,test
  (test)
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'foo and not bar or baz'"
  (should (equal '(or (and foo (not bar)) baz)
                 (mevedel--tag-query-prefix-from-infix '(foo and not bar or baz))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'john or not [jane]'"
  (should (equal '(or john (not [jane]))
                 (mevedel--tag-query-prefix-from-infix '(john or not [jane]))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'alice and bob and charlie'"
  (should (equal '(and alice bob charlie)
                 (mevedel--tag-query-prefix-from-infix '(alice and bob and charlie))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts single tag 'foo'"
  (should (equal 'foo
                 (mevedel--tag-query-prefix-from-infix '(foo))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'foo bar baz not john'"
  (should (equal '(and foo bar baz (not john))
                 (mevedel--tag-query-prefix-from-infix '(foo bar baz not john))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts '((foo))'"
  (should (equal 'foo
                 (mevedel--tag-query-prefix-from-infix '((foo)))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts '(((foo)))'"
  (should (equal 'foo
                 (mevedel--tag-query-prefix-from-infix '(((foo))))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts '(((foo foo foo)))'"
  (should (equal '(and foo foo foo)
                 (mevedel--tag-query-prefix-from-infix '(((foo foo foo))))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'not bar and baz'"
  (should (equal '(and (not bar) baz)
                 (mevedel--tag-query-prefix-from-infix '(not bar and baz))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'bar or bar or baz'"
  (should (equal '(or bar bar baz)
                 (mevedel--tag-query-prefix-from-infix '(bar or bar or baz))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'bar bar or baz'"
  (should (equal '(or (and bar bar) baz)
                 (mevedel--tag-query-prefix-from-infix '(bar bar or baz))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts empty list to nil"
  (should (equal nil
                 (mevedel--tag-query-prefix-from-infix '())))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts '((()))' to nil"
  (should (equal nil
                 (mevedel--tag-query-prefix-from-infix '(((()))))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts 'danny and (joey and boris)'"
  (should (equal '(and danny (and joey boris))
                 (mevedel--tag-query-prefix-from-infix '(danny and (joey and boris)))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts '((danny and (joey and boris)) and (foo or bar))'"
  (should (equal '(and (and danny (and joey boris)) (or foo bar))
                 (mevedel--tag-query-prefix-from-infix '((danny and (joey and boris)) and (foo or bar)))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts '((alice or bob) and (charlie or dave))'"
  (should (equal '(and (or alice bob) (or charlie dave))
                 (mevedel--tag-query-prefix-from-infix '((alice or bob) and (charlie or dave)))))
  :doc "Valid infix to prefix conversions:
`mevedel--tag-query-prefix-from-infix' converts '((alice and bob) or (charlie and dave))'"
  (should (equal '(or (and alice bob) (and charlie dave))
                 (mevedel--tag-query-prefix-from-infix '((alice and bob) or (charlie and dave)))))
  :doc "Valid infix to prefix conversions:
mixed implicit and explicit conjunctions retain precedence"
  (should (equal '(or (and foo bar baz) (and qux quux))
                 (mevedel--tag-query-prefix-from-infix
                  '(foo bar and baz or qux quux))))
  :doc "Valid infix to prefix conversions:
explicit grouping permits nested negation"
  (should (equal '(not (not foo))
                 (mevedel--tag-query-prefix-from-infix '(not (not foo)))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(and)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(and)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(or)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(or)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(not)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(not)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(and foo)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(and foo)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(or foo)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(or foo)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(and foo or bar)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(and foo or bar)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(or and foo bar)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(or and foo bar)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(and (or foo) bar)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(and (or foo) bar)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(foo (or bar))'"
  (should-error (mevedel--tag-query-prefix-from-infix '(foo (or bar))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(foo or (and bar))'"
  (should-error (mevedel--tag-query-prefix-from-infix '(foo or (and bar))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(foo bar and (not))'"
  (should-error (mevedel--tag-query-prefix-from-infix '(foo bar and (not))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '((or bar))'"
  (should-error (mevedel--tag-query-prefix-from-infix '((or bar))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '((and foo))'"
  (should-error (mevedel--tag-query-prefix-from-infix '((and foo))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(foo or (and))'"
  (should-error (mevedel--tag-query-prefix-from-infix '(foo or (and))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(or ())'"
  (should-error (mevedel--tag-query-prefix-from-infix '(or ())))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(foo or not)'"
  (should-error (mevedel--tag-query-prefix-from-infix '(foo or not)))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(and (and foo bar))'"
  (should-error (mevedel--tag-query-prefix-from-infix '(and (and foo bar))))
  :doc "Invalid infix queries:
`mevedel--tag-query-prefix-from-infix' rejects '(or (or(foo and bar)))'"
  (should-error (mevedel--tag-query-prefix-from-infix '(or (or(foo and bar)))))
  :doc "Invalid infix queries:
rejects a nested empty operand"
  (should-error (mevedel--tag-query-prefix-from-infix '(foo and (()))))
  :doc "Invalid infix queries:
rejects consecutive negation without an explicit group"
  (should-error (mevedel--tag-query-prefix-from-infix '(not not foo)))
  :doc "Invalid infix queries:
rejects trailing binary operators"
  (should-error (mevedel--tag-query-prefix-from-infix '(foo and)))
  (should-error (mevedel--tag-query-prefix-from-infix '(foo or))))

(mevedel-deftest mevedel--gc-hold ()
  ,test
  (test)
  :doc "keeps the busy threshold while a hold lives and restores it afterwards"
  (let ((gc-cons-threshold 800000)
        (mevedel-gc-cons-threshold-while-busy (* 64 1024 1024))
        (mevedel-gc-cons-threshold-while-typing nil)
        (noninteractive nil)
        (mevedel--gc-holds (make-hash-table :test #'eq))
        (mevedel--gc-restore nil)
        (mevedel--gc-timer nil))
    (unwind-protect
        (progn
          (mevedel--gc-hold 'first #'always)
          (mevedel--gc-hold 'second #'always)
          (should (= (* 64 1024 1024) gc-cons-threshold))
          (should (timerp mevedel--gc-timer))
          ;; Idle tuning lowers it again; the next maintenance restores it.
          (setq gc-cons-threshold 800000)
          (mevedel--gc-maintain)
          (should (= (* 64 1024 1024) gc-cons-threshold))
          (mevedel--gc-release 'first)
          (should (= (* 64 1024 1024) gc-cons-threshold))
          (mevedel--gc-release 'second)
          (should (= 800000 gc-cons-threshold))
          (should-not mevedel--gc-timer))
      (when (timerp mevedel--gc-timer) (mevedel--ui-timer-cancel mevedel--gc-timer))))

  :doc "drops a dead hold and leaves a value someone else chose"
  (let ((gc-cons-threshold 800000)
        (mevedel-gc-cons-threshold-while-busy (* 64 1024 1024))
        (mevedel-gc-cons-threshold-while-typing nil)
        (noninteractive nil)
        (mevedel--gc-holds (make-hash-table :test #'eq))
        (mevedel--gc-restore nil)
        (mevedel--gc-timer nil)
        (alive t))
    (unwind-protect
        (progn
          (mevedel--gc-hold 'request (lambda () alive))
          (setq gc-cons-threshold (* 128 1024 1024))
          (setq alive nil)
          (mevedel--gc-maintain)
          (should (= 0 (hash-table-count mevedel--gc-holds)))
          (should (= (* 128 1024 1024) gc-cons-threshold))
          (should-not mevedel--gc-timer))
      (when (timerp mevedel--gc-timer) (mevedel--ui-timer-cancel mevedel--gc-timer))))

  :doc "defers collection while typing and lets it run once input pauses"
  (let ((gc-cons-threshold 800000)
        (mevedel-gc-cons-threshold-while-busy (* 64 1024 1024))
        (mevedel-gc-cons-threshold-while-typing (* 256 1024 1024))
        (noninteractive nil)
        (mevedel--gc-holds (make-hash-table :test #'eq))
        (mevedel--gc-restore nil)
        (mevedel--gc-timer nil)
        (idle nil))
    (unwind-protect
        (cl-letf (((symbol-function 'current-idle-time)
                   (lambda () (and idle (seconds-to-time idle)))))
          ;; A command is running: input is current.
          (mevedel--gc-hold 'work #'always)
          (should (= (* 256 1024 1024) gc-cons-threshold))
          (should (memq #'mevedel--gc-note-input pre-command-hook))
          ;; A pause returns the busy floor so the collection can run.
          (setq idle 2.0)
          (mevedel--gc-maintain)
          (should (= (* 64 1024 1024) gc-cons-threshold))
          ;; Recent input raises it again at the next command.
          (setq idle 0.2)
          (mevedel--gc-note-input)
          (should (= (* 256 1024 1024) gc-cons-threshold))
          (mevedel--gc-release 'work)
          (should (= 800000 gc-cons-threshold))
          (should-not (memq #'mevedel--gc-note-input pre-command-hook)))
      (remove-hook 'pre-command-hook #'mevedel--gc-note-input)
      (when (timerp mevedel--gc-timer) (mevedel--ui-timer-cancel mevedel--gc-timer))))

  :doc "changes nothing in a batch Emacs or when disabled"
  (let ((gc-cons-threshold 800000)
        (mevedel--gc-holds (make-hash-table :test #'eq))
        (mevedel--gc-restore nil)
        (mevedel--gc-timer nil))
    (let ((noninteractive t)
          (mevedel-gc-cons-threshold-while-busy (* 64 1024 1024)))
      (mevedel--gc-hold 'batch #'always)
      (should (= 800000 gc-cons-threshold)))
    (let ((noninteractive nil)
          (mevedel-gc-cons-threshold-while-busy nil))
      (mevedel--gc-hold 'disabled #'always)
      (should (= 800000 gc-cons-threshold))
      (should-not mevedel--gc-timer)
      (mevedel--gc-release 'disabled))))

(mevedel-deftest mevedel--with-gc-busy ()
  ,test
  (test)
  :doc "holds the busy threshold for its body and releases it on exit"
  (let ((gc-cons-threshold 800000)
        (mevedel-gc-cons-threshold-while-busy (* 64 1024 1024))
        (mevedel-gc-cons-threshold-while-typing nil)
        (noninteractive nil)
        (mevedel--gc-holds (make-hash-table :test #'eq))
        (mevedel--gc-restore nil)
        (mevedel--gc-timer nil))
    (unwind-protect
        (progn
          (should (eq 'done (mevedel--with-gc-busy
                              (should (= (* 64 1024 1024) gc-cons-threshold))
                              'done)))
          (should (= 800000 gc-cons-threshold))
          (should-error (mevedel--with-gc-busy (error "Failed copy")))
          (should (= 800000 gc-cons-threshold))
          (should (= 0 (hash-table-count mevedel--gc-holds)))
          (should-not mevedel--gc-timer))
      (when (timerp mevedel--gc-timer) (mevedel--ui-timer-cancel mevedel--gc-timer)))))

(mevedel-deftest mevedel--optimize-transcript-buffer ()
  ,test
  (test)
  :doc "keeps no undo history in generated transcript storage"
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "streamed chunk")
    (should (consp buffer-undo-list))
    (mevedel--optimize-transcript-buffer)
    (should (eq t buffer-undo-list))
    (insert "tool result")
    (should (eq t buffer-undo-list))
    (should-not save-place-mode)))

(mevedel-deftest mevedel--forget-place ()
  ,test
  (test)
  :doc "keeps a persisted mevedel buffer out of `save-place-alist'"
  (let ((save-place-loaded t)
        (save-place-alist nil)
        (default-directory temporary-file-directory))
    (save-place-mode +1)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name
                (file-name-concat temporary-file-directory
                                  "segment-0001.chat.org"))
          (should save-place-mode)
          (mevedel--forget-place)
          (should-not save-place-mode)
          (insert "transcript\n")
          (save-place-to-alist)
          (should-not save-place-alist)
          (setq buffer-file-name nil))
      (save-place-mode -1))))

(defun test-mevedel-utilities--stub-helper (result &optional capture)
  "Return a `mevedel-execution-start-helper' stub settling with RESULT.
CAPTURE, when non-nil, is called with the stub's arguments."
  (lambda (callback &rest args)
    (when capture (funcall capture args))
    (funcall callback result)
    #'ignore))

(defun test-mevedel-utilities--diff (original modified filepath &optional labels-real)
  "Return the settled (DIFF . ERROR) of `mevedel-generate-diff'."
  (let (settled)
    (mevedel-generate-diff original modified filepath
                           (lambda (diff error) (setq settled (cons diff error)))
                           labels-real)
    (with-timeout (10 (ert-fail "Diff never settled"))
      (while (not settled) (accept-process-output nil 0.02)))
    settled))

(mevedel-deftest mevedel-start-helper-capturing-output ()
  ,test
  (test)
  :doc "routes a structured command and declared paths through the helper layer"
  (let ((session (mevedel-session--create))
        captured settled)
    (cl-letf (((symbol-function 'mevedel-execution-start-helper)
               (test-mevedel-utilities--stub-helper
                '(:exit-code 7 :output " helper output \n")
                (lambda (args) (setq captured args)))))
      (let ((mevedel--session session))
        (should (functionp
                 (mevedel-start-helper-capturing-output
                  (lambda (&rest values) (setq settled values))
                  "media-helper" '("helper" "--flag") '("/input")
                  '("/artifacts"))))))
    (should (equal '(7 " helper output \n" nil) settled))
    (should (equal '("media-helper" ("helper" "--flag") ("/input")
                     ("/artifacts") :session)
                   (seq-take captured 5)))
    (should (eq session (nth 5 captured)))
    (should (eq :owner (nth 6 captured)))
    (should (equal "/root" (nth 7 captured))))

  :doc "settles once with the error when the helper cannot start"
  (let (settled)
    (cl-letf (((symbol-function 'mevedel-execution-start-helper)
               (lambda (&rest _) (error "No helper"))))
      (should-not (mevedel-start-helper-capturing-output
                   (lambda (&rest values) (push values settled))
                   "helper" '("helper") nil)))
    (should (= 1 (length settled)))
    (should (equal '(error "No helper") (nth 2 (car settled)))))

  :doc "settles with an error when the helper's owner is torn down"
  (let (settled)
    (cl-letf (((symbol-function 'mevedel-execution-start-helper)
               (lambda (_callback &rest args)
                 (funcall (plist-get (nthcdr 4 args) :teardown-callback))
                 #'ignore)))
      (mevedel-start-helper-capturing-output
       (lambda (&rest values) (push values settled))
       "helper" '("helper") nil))
    (should (= 1 (length settled)))
    (should (nth 2 (car settled))))

  :doc "lets an error raised by the callback propagate"
  (cl-letf (((symbol-function 'mevedel-execution-start-helper)
             (test-mevedel-utilities--stub-helper '(:exit-code 0 :output ""))))
    (should-error (mevedel-start-helper-capturing-output
                   (lambda (&rest _) (error "Callback failed"))
                   "helper" '("helper") nil))))

(mevedel-deftest mevedel-generate-diff ()
  ,test
  (test)
  :doc "runs local snapshot diffing locally despite an ambient remote session"
  (let* ((target
          (mevedel-execution-target-create
           "/ssh:builder@example.test:/srv/project/"))
         (session (mevedel-session--create :execution-target target))
         captured)
    (cl-letf (((symbol-function 'mevedel-execution-start-helper)
               (test-mevedel-utilities--stub-helper
                '(:exit-code 1 :output "unified diff")
                (lambda (args) (setq captured args)))))
      (let ((mevedel--session session))
        (should (equal '("unified diff\n")
                       (test-mevedel-utilities--diff "old" "new" "file.el")))))
    (should (equal "diff" (car (nth 1 captured))))
    (should (= 2 (length (nth 2 captured))))
    (should-not (nth 5 captured))
    (should (eq :owner (nth 6 captured)))
    (should (equal "/root" (nth 7 captured))))

  :doc "produces a real unified diff through the helper layer"
  (let ((settled (test-mevedel-utilities--diff "a\nb\n" "a\nc\n" "file.txt")))
    (should-not (cdr settled))
    (should (string-match-p "^-b$" (car settled)))
    (should (string-match-p "^\\+c$" (car settled)))
    (should (equal '("") (test-mevedel-utilities--diff "same\n" "same\n" "file.txt"))))

  :doc "reports a failed helper as an error and removes its spools"
  (let (spools)
    (cl-letf (((symbol-function 'mevedel-execution-start-helper)
               (test-mevedel-utilities--stub-helper
                '(:exit-code -1 :output "" :error (error "Helper failed"))
                (lambda (args) (setq spools (nth 2 args))))))
      (let ((settled (test-mevedel-utilities--diff "old" "new" "file.el")))
        (should-not (car settled))
        (should (equal '(error "Helper failed") (cdr settled)))))
    (should (= 2 (length spools)))
    (dolist (path spools) (should-not (file-exists-p path))))

  :doc "labels a relative path a/ and b/ and an absolute path as itself"
  ;; The a/ and b/ prefixes belong to git-style patches over
  ;; repository-relative paths.  The edited-file reminder diffs absolute
  ;; paths, where prefixing spelled the label `a//home/user/file'.
  (dolist (case '(("src/file.el" "a/src/file.el" "b/src/file.el")
                  ("/home/user/file.el" "/home/user/file.el"
                   "/home/user/file.el")))
    (let (captured)
      (cl-letf (((symbol-function 'mevedel-execution-start-helper)
                 (test-mevedel-utilities--stub-helper
                  '(:exit-code 1 :output "unified diff")
                  (lambda (args) (setq captured args)))))
        (test-mevedel-utilities--diff "old" "new" (car case)))
      (let ((command (nth 1 captured)))
        (should (equal (list "diff" "-u"
                             "--label" (nth 1 case)
                             "--label" (nth 2 case))
                       (seq-take command 6))))))

  :doc "spools Unicode and literal cache bytes without asking for a coding system"
  (let* ((old "Old \u03bb \u2192\n")
         (new "New \u03bb \u2014\n")
         (old-bytes (encode-coding-string old 'utf-8-unix))
         (new-bytes (encode-coding-string new 'utf-8-unix)))
    (dolist (inputs (list (list old new)
                          (list old-bytes new-bytes)
                          (list (string-to-multibyte old-bytes)
                                (string-to-multibyte new-bytes))
                          (list old (string-to-multibyte new-bytes))))
      (let ((coding-system-for-write nil)
            spools)
        (cl-letf (((symbol-function 'select-safe-coding-system-interactively)
                   (lambda (&rest _) (ert-fail "Unexpected encoding prompt")))
                  ((symbol-function 'mevedel-execution-start-helper)
                   (lambda (callback &rest args)
                     (setq spools (nth 2 args))
                     (should
                      (equal (list old-bytes new-bytes)
                             (mapcar
                              (lambda (path)
                                (with-temp-buffer
                                  (set-buffer-multibyte nil)
                                  (insert-file-contents-literally path)
                                  (buffer-string)))
                              spools)))
                     (funcall callback '(:exit-code 1 :output "diff"))
                     #'ignore)))
          (let ((noninteractive nil))
            (should (equal '("diff\n")
                           (test-mevedel-utilities--diff
                            (car inputs) (cadr inputs) "unicode.md")))))
        (should (= 2 (length spools)))
        (dolist (path spools) (should-not (file-exists-p path))))))

  :doc "preserves a trailing blank context line in unified output"
  (cl-letf (((symbol-function 'mevedel-execution-start-helper)
             (test-mevedel-utilities--stub-helper
              '(:exit-code 1 :output "@@ -1 +1 @@\n-old\n+new\n \n"))))
    (should
     (equal '("@@ -1 +1 @@\n-old\n+new\n \n")
            (test-mevedel-utilities--diff "old\n\n" "new\n\n" "file.el")))))

(mevedel-deftest mevedel--write-file-atomically ()
  ,test
  (test)

  :doc "writes content with ordinary file modes, creating the parent"
  (let* ((root (make-temp-file "mevedel-atomic-" t))
         (path (file-name-concat root "deep" "state.eld")))
    (unwind-protect
        (progn
          (mevedel--write-file-atomically path "(:answer 42)\n")
          (should (equal "(:answer 42)\n"
                         (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))))
          (should (= (file-modes path) (default-file-modes)))
          (should-not (directory-files (file-name-directory path) nil
                                       "mevedel-write")))
      (delete-directory root t)))

  :doc "no-conversion writes literal bytes"
  (let* ((root (make-temp-file "mevedel-atomic-" t))
         (path (file-name-concat root "blob"))
         (bytes (unibyte-string 0 255 10 128)))
    (unwind-protect
        (progn
          (mevedel--write-file-atomically path bytes 'no-conversion)
          (should (equal bytes
                         (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally path)
                           (buffer-string)))))
      (delete-directory root t)))

  :doc "a mode argument overrides the default"
  (let* ((root (make-temp-file "mevedel-atomic-" t))
         (path (file-name-concat root "script.sh")))
    (unwind-protect
        (progn
          (mevedel--write-file-atomically path "#!/bin/sh\n" nil #o755)
          (should (= (file-modes path) #o755)))
      (delete-directory root t)))

  :doc "a write that dies leaves the previous content and no staging file"
  (let* ((root (make-temp-file "mevedel-atomic-" t))
         (path (file-name-concat root "state.eld")))
    (unwind-protect
        (progn
          (mevedel--write-file-atomically path "previous\n")
          (cl-letf (((symbol-function 'write-region)
                     (lambda (&rest _) (error "Disk full"))))
            (should-error (mevedel--write-file-atomically path "next\n")))
          (should (equal "previous\n"
                         (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))))
          (should-not (directory-files root nil "mevedel-write")))
      (delete-directory root t)))

  :doc "replaces a remote file in one target program with the requested mode"
  (let* ((host "atomic-remote-host")
         (root (file-name-as-directory (make-temp-file "mevedel-atomic-remote-" t)))
         (local (file-name-concat root "script.sh"))
         (program (symbol-function 'mevedel-session-control-fs-run-program))
         (programs 0))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (let ((path (format "/mevedelmock:%s:%s" host local)))
            (write-region "old\n" nil local nil 'silent)
            ;; Prime TRAMP's attribute cache with the old file.
            (should (file-exists-p path))
            (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                       (lambda (&rest args) (cl-incf programs) (apply program args)))
                      ((symbol-function 'rename-file)
                       (lambda (&rest _) (ert-fail "Fell back to TRAMP file operations"))))
              (mevedel--write-file-atomically path "#!/bin/sh\n" nil #o755))
            (should (= 1 programs))
            (should (equal "#!/bin/sh\n"
                           (with-temp-buffer (insert-file-contents local) (buffer-string))))
            (should (= #o755 (file-modes local)))
            ;; TRAMP no longer answers from the old file's attributes.
            (should (= #o755 (file-modes path)))
            (should-not (directory-files root nil "mevedel-control-fs"))))
      (delete-directory root t)))

  :doc "falls back to TRAMP file operations where the program refuses"
  (let* ((host "atomic-remote-fallback-host")
         (root (file-name-as-directory (make-temp-file "mevedel-atomic-fallback-" t)))
         (target (file-name-concat root "target.txt"))
         (link (file-name-concat root "link.txt"))
         (nested (file-name-concat root "new" "dir" "file.txt")))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (write-region "target\n" nil target nil 'silent)
          (make-symbolic-link target link)
          ;; A symlinked leaf is replaced as a file, as before.
          (mevedel--write-file-atomically
           (format "/mevedelmock:%s:%s" host link) "replaced\n")
          (should-not (file-symlink-p link))
          (should (equal "target\n"
                         (with-temp-buffer (insert-file-contents target) (buffer-string))))
          ;; Missing parents are still created.
          (mevedel--write-file-atomically
           (format "/mevedelmock:%s:%s" host nested) "nested\n")
          (should (equal "nested\n"
                         (with-temp-buffer (insert-file-contents nested) (buffer-string)))))
      (delete-directory root t))))

(mevedel-deftest mevedel--warn-once ()
  ,test
  (test)
  :doc "warns on the first call per key and demotes repeats to messages"
  (let ((mevedel--warn-once-table (make-hash-table :test #'equal))
        warnings messages)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (type text &rest _) (push (cons type text) warnings)))
              ((symbol-function 'message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (mevedel--warn-once 'test-key "problem %d" 1)
      (mevedel--warn-once 'test-key "problem %d" 2)
      (mevedel--warn-once (list 'test-site "a") "subject a")
      (mevedel--warn-once (list 'test-site "b") "subject b"))
    (should (equal '((mevedel . "problem 1")
                     (mevedel . "subject a")
                     (mevedel . "subject b"))
                   (nreverse warnings)))
    (should (equal '("mevedel: problem 2") messages)))

  :doc "repeats bind `inhibit-message' so the echo area stays untouched"
  (let ((mevedel--warn-once-table (make-hash-table :test #'equal))
        inhibited)
    (cl-letf (((symbol-function 'display-warning) #'ignore)
              ((symbol-function 'message)
               (lambda (&rest _) (setq inhibited inhibit-message))))
      (mevedel--warn-once 'test-key "problem")
      (mevedel--warn-once 'test-key "problem"))
    (should inhibited)))

(mevedel-deftest mevedel--warn-once-reset-site ()
  ,test
  (test)
  :doc "re-arms plain and composite keys only for the requested site"
  (let ((mevedel--warn-once-table (make-hash-table :test #'equal))
        warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type text &rest _) (push text warnings)))
              ((symbol-function 'message) #'ignore))
      (mevedel--warn-once 'test-site "site")
      (mevedel--warn-once (list 'test-site "a") "site a")
      (mevedel--warn-once (list 'test-site "b") "site b")
      (mevedel--warn-once (list 'other-site "c") "other c")
      (mevedel--warn-once-reset-site 'test-site)
      (mevedel--warn-once 'test-site "site")
      (mevedel--warn-once (list 'test-site "a") "site a")
      (mevedel--warn-once (list 'other-site "c") "other c"))
    (should (equal '("site" "site a" "site b" "other c" "site" "site a")
                   (nreverse warnings)))))

(mevedel-deftest mevedel--with-gc-batched ()
  ,test
  (test)
  :doc "raises the GC threshold for the dynamic extent of the body"
  (let ((gc-cons-threshold 800000))
    (mevedel--with-gc-batched
      (should (>= gc-cons-threshold (* 64 1024 1024))))
    (should (= gc-cons-threshold 800000)))

  :doc "never lowers an already higher threshold"
  (let ((gc-cons-threshold (* 128 1024 1024)))
    (mevedel--with-gc-batched
      (should (= gc-cons-threshold (* 128 1024 1024)))))

  :doc "raises the percentage trigger and never lowers it"
  (let ((gc-cons-percentage 0.1))
    (mevedel--with-gc-batched
      (should (>= gc-cons-percentage 0.5)))
    (should (= gc-cons-percentage 0.1)))
  (let ((gc-cons-percentage 0.8))
    (mevedel--with-gc-batched
      (should (= gc-cons-percentage 0.8)))))

(mevedel-deftest mevedel--file-name-candidates ()
  ,test
  (test)

  :doc "a target path resolves no aliases, and so touches no target"
  ;; The alias forms are local concepts and cannot apply to another host;
  ;; the directory walks call this once per ancestor during dispatch.
  (let ((truenames 0))
    (mevedel-test--with-captured-diagnostics nil
      (cl-letf (((symbol-function 'file-truename)
                 (lambda (name &rest _) (setq truenames (1+ truenames)) name)))
        (should (equal '("/mevedelmock:host:/srv/project/main.py")
                       (mevedel--file-name-candidates
                        "/mevedelmock:host:/srv/project/main.py")))
        (should (= 0 truenames)))))

  :doc "a local path still resolves its aliases"
  (let* ((dir (make-temp-file "mevedel-cand-" t))
         (file (file-name-concat dir "f.el")))
    (unwind-protect
        (progn
          (write-region "" nil file nil 'silent)
          (should (member (directory-file-name (expand-file-name file))
                          (mevedel--file-name-candidates file))))
      (delete-directory dir t))))

(mevedel-deftest mevedel--executable-find ()
  ,test
  (test)

  :doc "one lookup per (target, name), positive and negative alike"
  ;; Glob, Grep, Read and every spawn probe for a tool on the hot path,
  ;; from inside gptel's curl sentinel; an uncached remote probe walks the
  ;; whole PATH with a stat per entry.
  (let ((lookups 0))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (name &rest _)
                     (setq lookups (1+ lookups))
                     (and (equal name "rg") "/bin/rg"))))
          (clrhash mevedel--executable-cache)
          (should (equal "/bin/rg"
                         (mevedel--executable-find "rg" "/mevedelmock:host:")))
          (should-not (mevedel--executable-find "nope" "/mevedelmock:host:"))
          (should (= 2 lookups))
          (should (equal "/bin/rg"
                         (mevedel--executable-find "rg" "/mevedelmock:host:")))
          (should-not (mevedel--executable-find "nope" "/mevedelmock:host:"))
          (should (= 2 lookups)))
      (clrhash mevedel--executable-cache)))

  :doc "local and remote answers do not share a cache entry"
  (let (asked)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (name &optional remote)
                     (push remote asked)
                     (and remote "/remote/bin/rg"))))
          (clrhash mevedel--executable-cache)
          (should-not (mevedel--executable-find "rg"))
          (should (equal "/remote/bin/rg"
                         (mevedel--executable-find "rg" "/mevedelmock:host:")))
          (should (equal '("/mevedelmock:host:" nil) asked)))
      (clrhash mevedel--executable-cache))))

(mevedel-deftest mevedel--truncate-display ()
  ,test
  (test)

  :doc "returns a fitting string untouched and raises nothing"
  ;; `truncate-string-to-width' ends its scan by running `aref' off the
  ;; end and catching the `args-out-of-range' itself, so a label shorter
  ;; than WIDTH always raised one internally -- which TRAMP's signal hook
  ;; then logged to *Messages* on every remote render.
  (let ((raised 0))
    (advice-add 'signal :before
                (lambda (&rest _) (setq raised (1+ raised)))
                '((name . mevedel-truncate-count)))
    (unwind-protect
        (progn
          (should (equal "git status"
                         (mevedel--truncate-display "git status" 60 "...")))
          (should (= 0 raised)))
      (advice-remove 'signal 'mevedel-truncate-count)))

  :doc "truncates and marks a string that does not fit"
  (should (equal "ex..." (mevedel--truncate-display "exactly-ten" 5 "...")))

  :doc "an empty string and a non-string are handled"
  (should (equal "" (mevedel--truncate-display "" 10 "...")))
  (should-not (mevedel--truncate-display nil 10 "...")))

(mevedel-deftest mevedel--truncate-bytes ()
  ,test
  (test)
  :doc "returns fitting text untouched"
  (should (equal "héllo" (mevedel--truncate-bytes "héllo" 6 "[..]")))
  :doc "truncates on a character boundary and appends the marker within the budget"
  (let ((bounded (mevedel--truncate-bytes "héllo wörld" 9 "[..]")))
    (should (equal "héll[..]" bounded))
    (should (<= (string-bytes bounded) 9)))
  :doc "rejects a budget that cannot hold its marker"
  (should-error (mevedel--truncate-bytes "text" 2 "[...]")))

(mevedel-deftest mevedel--unified-diff ()
  ,test
  (test)
  :doc "returns nil for identical text and hunks for a change"
  (should-not (mevedel--unified-diff "same\n" "same\n"))
  (let ((diff (mevedel--unified-diff "one\ntwo\n" "one\nthree\n")))
    (should (string-prefix-p "@@" diff))
    (should (string-match-p "^-two$" diff))
    (should (string-match-p "^\\+three$" diff))
    (should-not (string-match-p "Diff finished" diff)))
  :doc "honours the requested context width"
  (let ((original (mapconcat #'number-to-string (number-sequence 1 20) "\n"))
        (changed (mapconcat (lambda (n) (if (= n 10) "x" (number-to-string n)))
                            (number-sequence 1 20) "\n")))
    (should (string-match-p "^ 4$" (mevedel--unified-diff original changed 6)))
    (should-not (string-match-p "^ 4$" (mevedel--unified-diff original changed)))))

(mevedel-deftest mevedel--duration-label
  (:doc "Uses whole seconds with safe zero and minute/hour boundaries.")
  (dolist (entry '((nil . "0s") (-1 . "0s") (0.99 . "0s")
                   (59.99 . "59s") (60 . "1m 00s") (185 . "3m 05s")
                   (3600 . "1h 00m") (3720 . "1h 02m")))
    (should (equal (mevedel--duration-label (car entry)) (cdr entry)))))

(provide 'test-mevedel-utilities)
;;; test-mevedel-utilities.el ends here
