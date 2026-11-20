;;; test-mevedel-view-composer-graphical.el --- Native docked composer tests -*- lexical-binding: t -*-

;;; Commentary:

;; Show a real chat view and edit through Emacs's command loop, on a
;; graphical frame or on the terminal-check pseudo-terminal.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel)
(require 'mevedel-chat)
(require 'mevedel-tool-ui)
(require 'mevedel-hooks)
(require 'mevedel-pending-inputs)
(require 'mevedel-skills-core)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-native)
(require 'mevedel-view-render)

(declare-function mevedel-view-native--stats "ext:mevedel-view-native" ())
(require 'mevedel-view-composer-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-view-composer-test-helpers"))

(defun mevedel-view-composer-graphical--header (window)
  "Return the rendered header format for WINDOW."
  (format-mode-line (buffer-local-value 'header-line-format (window-buffer window))
                    'header-line window))

(ert-deftest mevedel-view-composer-graphical/pair-navigation ()
  "Pair keys leave the draft caret alone and page the paired transcript."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (transcript (mevedel-view-transcript-window dock)))
      (mevedel-view-test--insert-data
       data-buf (mapconcat (lambda (n) (format "Response line %d." n))
                           (number-sequence 1 180) "\n") 'response)
      (with-current-buffer view-buf (mevedel-view--full-rerender))
      (mevedel-view-transcript-show-end transcript)
      (redisplay t)
      (mevedel-view-composer-graphical--type "Keep this draft")
      (let ((caret (point)) (bottom (window-start transcript)))
        (mevedel-view-composer-graphical--keys "C-M-S-v" 'scroll-other-window-down)
        (should (< (window-start transcript) bottom))
        (let ((earlier (window-start transcript)))
          (mevedel-view-composer-graphical--keys "C-M-v" 'scroll-other-window)
          (should (> (window-start transcript) earlier)))
        (should (eq (selected-window) dock))
        (should (= (point) caret))
        (mevedel-view-composer-graphical--keys "C-c C-SPC" 'mevedel-view-pair-toggle-focus)
        (should (eq (selected-window) transcript))
        (mevedel-view-composer-graphical--keys "C-c C-SPC" 'mevedel-view-pair-toggle-focus)
        (should (eq (selected-window) dock))
        (should (= (point) caret))))))

(ert-deftest mevedel-view-composer-graphical/draft-height ()
  "A short draft grows, caps with its caret visible, and shrinks again."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (transcript (mevedel-view-transcript-window dock))
           (pair-height (+ (window-pixel-height dock) (window-pixel-height transcript)))
           (line (frame-char-height))
           (start (mevedel-view-composer-draft-start)))
      (mevedel-view-composer-graphical--type "First line")
      (let ((short (window-pixel-height dock)))
        (should (<= (window-body-height dock t) (* 2 line)))
        (mevedel-view-composer-graphical--keys "RET" 'newline)
        (mevedel-view-composer-graphical--type "Second line")
        (should (= (window-pixel-height dock) (+ short line)))
        (insert (make-string (* 2 pair-height) ?\n) "Last line")
        (mevedel-view-composer-graphical--keys "C-e" (key-binding (kbd "C-e")))
        (should (<= (window-pixel-height dock)
                    (/ (+ (window-pixel-height dock) (window-pixel-height transcript)) 3)))
        (should (> (window-pixel-height dock) (+ short line)))
        (should (> (window-start dock) start))
        (should (pos-visible-in-window-p (point) dock))
        (should (string-search (format "↑ %d more" (- (line-number-at-pos (window-start dock)) 2))
                               (mevedel-view-composer-graphical--header dock)))
        (mevedel-view-composer-graphical--keys "M-<" 'beginning-of-buffer)
        (should-not (string-search " more" (mevedel-view-composer-graphical--header dock)))
        (delete-region start (point-max))
        (goto-char start)
        (mevedel-view-composer-graphical--type "Short again")
        (should (= (window-pixel-height dock) short))
        (should (= (window-start dock) (point-min)))))))

(ert-deftest mevedel-view-composer-graphical/reopen-dock-at-draft-height ()
  "Restoring a dock for a long draft does not start at a placeholder height."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (transcript (mevedel-view-transcript-window dock))
           (pair-height (+ (window-pixel-height dock) (window-pixel-height transcript))))
      (insert (mapconcat (lambda (_) "wrapped draft ") (number-sequence 1 500)))
      (select-window transcript)
      (let ((ignore-window-parameters t)) (delete-window dock))
      (let* ((reopened (mevedel-view-composer-display transcript))
             (height (window-pixel-height reopened)))
        (should (= height (/ pair-height 3)))
        (should (eq (selected-window) transcript))
        (mevedel-view-composer-graphical--settle)
        (should (= (window-pixel-height reopened) height))))))

(ert-deftest mevedel-view-composer-graphical/dock-chrome ()
  "The rule is above the rail and the pair's mode line is below the draft."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (transcript (mevedel-view-transcript-window dock))
           (width (window-pixel-width dock)))
      (should (zerop (window-mode-line-height transcript)))
      (should (> (window-mode-line-height dock) 0))
      (should (> (window-header-line-height dock) 0))
      (dolist (x (list 0 (- width (frame-char-width))))
        (should (eq (posn-area (posn-at-x-y x 0 dock)) 'header-line)))
      (should (eq (posn-area (posn-at-x-y 1 (1- (window-pixel-height dock)) dock))
                  'mode-line))
      (if (display-graphic-p)
          (let* ((x (- (nth 2 (window-inside-pixel-edges dock))
                       (car (window-pixel-edges dock))))
                 (y (window-header-line-height dock))
                 (header (posn-at-x-y x 1 dock t))
                 (object (posn-string header)))
            (should (eq (posn-area (posn-at-x-y x y dock t)) 'right-fringe))
            (should object)
            (should (eq (get-text-property (cdr object) 'face (car object)) 'fringe))
            (should (zerop (car (posn-object-x-y header))))
            (should (= (car (posn-object-width-height header)) (nth 1 (window-fringes dock)))))
        (should (equal (mevedel-view-composer-graphical--header dock)
                       (make-string (window-total-width dock) ?─)))))))

(ert-deftest mevedel-view-composer-graphical/back-to-bottom ()
  "The native button appears off the end and jumps without selecting the transcript."
  (mevedel-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (transcript (mevedel-view-transcript-window dock)))
      (mevedel-view-test--insert-data
       data-buf (mapconcat (lambda (n) (format "Earlier line %d." n))
                           (number-sequence 1 180) "\n") 'response)
      (with-current-buffer view-buf (mevedel-view--full-rerender))
      (mevedel-view-transcript-show-end transcript)
      (mevedel-view-composer-graphical--type "Unsent draft")
      (let ((caret (point)))
        (should (mevedel-view-transcript-end-visible-p transcript))
        (should-not (string-search "Back to bottom" (mevedel-view-composer-graphical--header dock)))
        (mevedel-view-composer-graphical--keys "C-M-S-v" 'scroll-other-window-down)
        (should-not (mevedel-view-transcript-end-visible-p transcript))
        (should (string-search "Back to bottom" (mevedel-view-composer-graphical--header dock)))
        (let ((pos (cl-loop for x downfrom (1- (window-pixel-width dock)) to 0
                            for pos = (posn-at-x-y x 1 dock)
                            for (string . index) = (posn-string pos)
                            when (and string (get-text-property index 'local-map string))
                            return pos)))
          (should pos)
          (mevedel-view-composer-graphical--events
           (list (list 'mouse-1 pos)) 'mevedel-composer-back-to-bottom))
        (should (mevedel-view-transcript-end-visible-p transcript))
        (should-not (string-search "Back to bottom" (mevedel-view-composer-graphical--header dock)))
        (should (eq (selected-window) dock))
        (should (= (point) caret))))))

(ert-deftest mevedel-view-composer-graphical/header-reuses-unchanged-transcript ()
  "A dock header does not walk unchanged transcript rows on every format."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((dock (selected-window))
           (transcript (mevedel-view-transcript-window dock))
           (queries 0)
           (observe (lambda (original &optional position window partial)
                      (when (and (eq window transcript) (integerp position)
                                 (= position (with-current-buffer view-buf (point-max))))
                        (cl-incf queries))
                      (funcall original position window partial))))
      (mevedel-view-test--insert-data
       data-buf (mapconcat (lambda (n) (format "Earlier line %d." n))
                           (number-sequence 1 180) "\n") 'response)
      (with-current-buffer view-buf (mevedel-view--full-rerender))
      (mevedel-view-transcript-show-end transcript)
      (mevedel-view-composer-graphical--settle)
      (unwind-protect
          (progn
            (advice-add 'pos-visible-in-window-p :around observe)
            (dotimes (_ 3)
              (should-not (string-search "Back to bottom"
                                         (mevedel-view-composer-graphical--header dock))))
            (should (<= queries 1))
            (set-window-start transcript (with-current-buffer view-buf (point-min)))
            (should (string-search "Back to bottom"
                                   (mevedel-view-composer-graphical--header dock))))
        (advice-remove 'pos-visible-in-window-p observe)))))

(ert-deftest mevedel-view-composer-graphical/rail-at-top ()
  "Activity appears on the composer's first line without editing either buffer."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let ((mevedel-view-spinner-animate nil)
          (view-text (with-current-buffer view-buf (buffer-string))))
      (should (string-match-p "\\`\\[[^]\n]+\\]" (mevedel-view-test--rail-text)))
      (mevedel-view-composer-graphical--type "unsent draft")
      (let ((draft-start (mevedel-view-composer-draft-start))
            (draft-point (point)))
        (mevedel-view-stream-show-progress view-buf "Checking the patch")
        (redisplay t)
        (should (pos-visible-in-window-p (point-min)))
        (should (string-search "Checking the patch" (mevedel-view-test--rail-text)))
        (should (equal view-text (with-current-buffer view-buf (buffer-string))))
        (should (= draft-start (mevedel-view-composer-draft-start)))
        (should (= draft-point (point)))))))

(ert-deftest mevedel-view-composer-graphical/rail-power-policy ()
  "The activity animates under full power and holds under a zero battery cap."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let ((mevedel-view-spinner-style 'ascii)
          (mevedel-view-spinner-animate t)
          (mevedel-view-spinner-power-policy 'save)
          (mevedel-view-spinner-battery-framerate 0)
          (mevedel-view-native-enabled nil)
          (chat-frame (selected-frame))
          (native-focus (symbol-function 'frame-focus-state))
          (activity "Checking power policy"))
      ;; The private compositor presents the frame without granting it seat focus.
      (cl-letf (((symbol-function 'frame-focus-state)
                 (lambda (&optional frame)
                   (or (and (display-graphic-p chat-frame)
                            (eq (or frame (selected-frame)) chat-frame))
                       (funcall native-focus frame)))))
        (cl-labels ((sample ()
                      (let* ((rail (mevedel-view-test--rail-text))
                             (end (string-search activity rail)))
                        (should end)
                        (substring rail 0 end))))
          (mevedel-view-stream-show-progress view-buf activity)
          (redisplay t)
          (let ((resting (sample)))
            (sit-for 0.4)
            (should (equal resting (sample)))
            (let ((mevedel-view-spinner-power-policy 'full)
                  (deadline (+ (float-time) 2)))
              (mevedel-view-stream-show-progress view-buf activity)
              (while (and (equal resting (sample)) (< (float-time) deadline))
                (sit-for 0.04))
              (should-not (equal resting (sample))))))))))

(ert-deftest mevedel-view-composer-graphical/rail-frame-does-not-dirty-transcript-chrome ()
  "Portable rail animation does not force the transcript's mode line to update."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let ((mevedel-view-spinner-style 'ascii)
          (mevedel-view-spinner-animate t)
          (mevedel-view-spinner-power-policy 'full)
          (mevedel-view-native-enabled nil)
          (chat-frame (selected-frame))
          (native-focus (symbol-function 'frame-focus-state))
          (updates 0))
      (cl-letf (((symbol-function 'frame-focus-state)
                 (lambda (&optional frame)
                   (or (and (display-graphic-p chat-frame)
                            (eq (or frame (selected-frame)) chat-frame))
                       (funcall native-focus frame)))))
        (mevedel-view-stream-show-progress view-buf "Checking animation")
        (redisplay t)
        (let* ((deadline (+ (float-time) 2))
               (before (mevedel-view-test--rail-text))
               (column (string-search "Checking animation" before))
               (observe (lambda (&rest _)
                          (when (eq (current-buffer) view-buf) (cl-incf updates)))))
          (should column)
          (unwind-protect
              (progn
                (advice-add 'set-buffer-modified-p :before observe)
                (while (and (eq t (compare-strings before 0 column
                                                  (mevedel-view-test--rail-text) 0 column))
                            (< (float-time) deadline))
                  (sit-for 0.01))
                (should-not (eq t (compare-strings before 0 column
                                                   (mevedel-view-test--rail-text) 0 column)))
                (should (zerop updates)))
            (advice-remove 'set-buffer-modified-p observe)))))))

(ert-deftest mevedel-view-composer-graphical/rail-presents-native-frames ()
  "A Wayland surface fits the activity cell between the badge and elapsed time."
  (mevedel-view-composer-graphical--with-chat
    (when (mevedel-view-native-available-p (selected-frame))
      (let* ((chat-frame (selected-frame))
             (native-focus (symbol-function 'frame-focus-state))
             (mevedel-view-spinner-style 'shimmer)
             (mevedel-view-spinner-power-policy 'full)
             (mevedel-view-spinner-animate t)
             (before (mevedel-view-native--stats)))
        (cl-letf (((symbol-function 'frame-focus-state)
                   (lambda (&optional frame)
                     (or (eq (or frame (selected-frame)) chat-frame)
                         (funcall native-focus frame)))))
          (mevedel-view-stream-show-progress view-buf "Working on the benchmark request")
          (redisplay t)
          (let ((deadline (+ (float-time) 2)))
            (while (and (<= (aref (mevedel-view-native--stats) 1) (1+ (aref before 1)))
                        (< (float-time) deadline))
              (sit-for 0.02)))
          (should (= (aref (mevedel-view-native--stats) 0) (1+ (aref before 0))))
          (should (> (aref (mevedel-view-native--stats) 1) (1+ (aref before 1)))))))))

(ert-deftest mevedel-view-composer-graphical/rail-visits-card ()
  "A question announces itself in the dock without moving the transcript."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((composer (selected-window))
           (transcript (get-buffer-window view-buf (selected-frame)))
           (mevedel-view-spinner-animate nil)
           answered)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (insert (make-string 75 ?\n)))
        (set-window-point transcript (point-min))
        (set-window-start transcript (point-min)))
      (redisplay t)
      (should (< (window-end transcript) (with-current-buffer view-buf (point-max))))
      (mevedel-view-composer-graphical--type "unsent draft")
      (backward-char 5)
      (let ((offset (- (point) (mevedel-view-composer-draft-start)))
            (start (window-start transcript)))
        (with-current-buffer data-buf
          (mevedel-tool-ask-ui-show
           (lambda (answer) (setq answered answer))
           [(:question "Use the dock?" :options ["Yes" "No"])]))
        (redisplay t)
        (should (eq composer (selected-window)))
        (should (= start (window-start transcript)))
        (should (string-search "1 question" (mevedel-view-test--rail-text)))
        (mevedel-view-composer-graphical--keys "C-c C-j" 'mevedel-view-jump-to-interaction)
        (should (eq transcript (selected-window)))
        (should (get-text-property (point) 'mevedel-prompt-focus))
        (mevedel-view-composer-graphical--keys "RET" (key-binding (kbd "RET")))
        (mevedel-view-composer-graphical--keys "C-c C-c" (key-binding (kbd "C-c C-c")))
        (should (string-match-p "A1: Yes" answered))
        (should (eq composer (selected-window)))
        (should (= offset (- (point) (mevedel-view-composer-draft-start))))
        (should (equal "unsent draft" (mevedel-view-composer-graphical--draft view-buf)))
        (should-not (string-search "1 question" (mevedel-view-test--rail-text)))))))

(ert-deftest mevedel-view-composer-graphical/transcript-chrome-width ()
  "A narrow dock does not shrink the transcript's status chrome."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((composer (selected-window))
           (transcript (get-buffer-window view-buf (selected-frame)))
           (width (window-body-width transcript)))
      (set-window-margins composer (/ width 4) (/ width 4))
      (redisplay t)
      (should (< (window-body-width composer) width))
      (with-current-buffer view-buf
        (should (= width (mevedel-view-zone-width)))))))

(ert-deftest mevedel-view-composer-graphical/docked-typing ()
  "Show selects an ordinary composer split and typing stays out of the transcript."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let* ((composer (mevedel-view-composer-buffer view-buf))
           (window (selected-window))
           (transcript (get-buffer-window view-buf (selected-frame)))
           (view-text (with-current-buffer view-buf (buffer-string))))
      (should (buffer-live-p composer))
      (should-not (eq composer view-buf))
      (should (eq (window-buffer window) composer))
      (should (eq (window-in-direction 'below transcript) window))
      (should (= (window-pixel-left window) (window-pixel-left transcript)))
      (should (= (window-pixel-width window) (window-pixel-width transcript)))
      (should-not (window-parameter window 'window-side))
      (should (string-search "An earlier response." view-text))
      (should-not (mevedel-pending-inputs--event-wake-refusal session data-buf view-buf))
      (mevedel-view-composer-graphical--type "> unsent draft")
      (mevedel-view-composer-graphical--keys "RET" 'newline)
      (mevedel-view-composer-graphical--type "second line")
      (should (eq (selected-window) window))
      (should (equal "> unsent draft\nsecond line"
                     (mevedel-view-composer-graphical--draft view-buf)))
      (should (eq 'composer-draft
                  (mevedel-pending-inputs--event-wake-refusal session data-buf view-buf)))
      (should (equal view-text (with-current-buffer view-buf (buffer-string)))))))

(ert-deftest mevedel-view-composer-graphical/send-and-history ()
  "Send folds the echo and clears input; history restores an unfinished draft."
  :tags '(mevedel-docked-composer)
  (mevedel-test--with-captured-diagnostics nil
    (mevedel-view-composer-graphical--with-chat
      (let ((mevedel-view-user-input-collapse-line-threshold 1)
            (submitted "Submitted once\nA folded line")
            requests)
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda (&rest _) (push (current-buffer) requests))))
          (mevedel-view-composer-graphical--type "Submitted once")
          (mevedel-view-composer-graphical--keys "RET" 'newline)
          (mevedel-view-composer-graphical--type "A folded line")
          (mevedel-view-composer-graphical--keys "C-c RET" 'mevedel-view-send))
        (should (equal requests (list data-buf)))
        (should (equal "" (mevedel-view-composer-graphical--draft view-buf)))
        (with-current-buffer data-buf
          (should (string-search submitted (buffer-string))))
        (with-current-buffer view-buf
          (should (string-search "Submitted once (+1 line)" (buffer-string)))
          (should-not (string-search "A folded line" (buffer-string))))
        (mevedel-view-composer-graphical--type "Keep this unfinished draft")
        (mevedel-view-composer-graphical--keys "M-p" 'mevedel-view-history-previous)
        (should (equal submitted (mevedel-view-composer-graphical--draft view-buf)))
        (mevedel-view-composer-graphical--keys "M-n" 'mevedel-view-history-next)
        (should (equal "Keep this unfinished draft"
                       (mevedel-view-composer-graphical--draft view-buf)))))))

(ert-deftest mevedel-view-composer-graphical/completion ()
  "Native completion extends mention, skill and slash tokens in the composer."
  :tags '(mevedel-docked-composer)
  (mevedel-view-composer-graphical--with-chat
    (let ((suggest-key-bindings nil)
          (skill (file-name-concat root ".mevedel/skills/graphical-skill/SKILL.md")))
      (make-directory (file-name-directory skill) t)
      (with-temp-file skill
        (insert "---\nname: graphical-skill\ndescription: A completion fixture.\n---\n\nReview the fixture.\n"))
      (mevedel-skills-install session data-buf)
      (dolist (pair '(("@fi" . "@file:") ("$graphical-sk" . "$graphical-skill")
                      ("/hel" . "/help")))
        (mevedel-view-composer-replace "" view-buf)
        (mevedel-view-composer-graphical--type (car pair))
        (mevedel-view-composer-graphical--events
         (append (kbd "M-x") "completion-at-point" (listify-key-sequence (kbd "RET")))
         'completion-at-point)
        (should (equal (cdr pair)
                       (string-trim-right
                        (mevedel-view-composer-graphical--draft view-buf))))))))

(ert-deftest mevedel-view-composer-graphical/async-send-keeps-new-input ()
  "An accepted prompt leaves edits made while UserPromptSubmit is pending."
  (mevedel-test--with-captured-diagnostics nil
    (mevedel-view-composer-graphical--with-chat
      (let* (;; User command hooks run from the rule source directory.
             (user-emacs-directory root)
             (release (file-name-concat root "release-hook"))
             (mevedel-hook-rules
              `((UserPromptSubmit
                 ((:matcher "*"
                   :hooks ((:type command :timeout 10 :fail-closed t
                            :command ,(format "while [ ! -e %s ]; do sleep .02; done; printf '{}'"
                                              (shell-quote-argument release)))))))))
             (window (selected-window))
             requests)
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda (&rest _) (push (current-buffer) requests))))
          (unwind-protect
              (progn
                (mevedel-view-composer-graphical--type "First draft")
                (mevedel-view-composer-graphical--keys "C-c RET" 'mevedel-view-send)
                (should-not requests)
                (mevedel-view-composer-graphical--keys "C-c C-u" 'mevedel-view-history-clear-input)
                (mevedel-view-composer-graphical--type "> later draft")
                (mevedel-view-composer-graphical--keys "RET" 'newline)
                (mevedel-view-composer-graphical--type "second line")
                (let ((caret (point)))
                  (write-region "" nil release nil 'silent)
                  (let ((deadline (+ (float-time) 5)))
                    (while (and (not requests) (< (float-time) deadline))
                      (accept-process-output nil 0.02)))
                  (ert-info ((format "Hook log: %S; diagnostics: %s"
                                     (mevedel-session-hook-log session)
                                     mevedel-test--captured))
                    (should (equal requests (list data-buf)))
                    (should (eq (plist-get (car (last (mevedel-session-hook-log session)))
                                           :status)
                                'ok)))
                  (should (equal "> later draft\nsecond line"
                                 (mevedel-view-composer-graphical--draft view-buf)))
                  (should (eq window (selected-window)))
                  (should (= caret (point)))
                  (with-current-buffer data-buf
                    (should (string-search "First draft" (buffer-string)))
                    (should-not (string-search "later draft" (buffer-string))))))
            (unless requests
              (with-current-buffer view-buf
                (mevedel-view--cancel-pending-submission))
              (write-region "" nil release nil 'silent))))))))

(provide 'test-mevedel-view-composer-graphical)
;;; test-mevedel-view-composer-graphical.el ends here
