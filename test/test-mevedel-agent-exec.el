;;; test-mevedel-agent-exec.el --- Tests for sub-agent runtime -*- lexical-binding: t -*-

;;; Commentary:

;; Regression coverage for the extracted sub-agent runtime (spec 18).
;; The callback contract is the high-value surface: upstream
;; `gptel-agent--task' fired its main callback on every streamed chunk
;; and dropped gptel's `t' completion signal, so the parent's
;; tool_result was frozen at the first chunk.  These tests pin the
;; corrected contract: chunks accumulate, MAIN-CB fires exactly once on
;; `t', tool-use guards hold, and the task overlay carries the mevedel
;; marker.

;;; Code:

(require 'gptel)
(require 'mevedel-agent-exec)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Test helpers

(defmacro mevedel-agent-exec-test--with-callback (callback-sym &rest body)
  "Bind CALLBACK-SYM to a freshly-constructed runner callback and run BODY.
The bound callback writes into a fresh partial-cell (seeded with an
empty prefix for readability), and a locally-bound `fired' accumulates
the arguments of every `main-cb' invocation so the test can assert
fire-count and payload."
  (declare (indent 1) (debug t))
  `(let* ((fired nil)
          (main-cb (lambda (&rest args) (push args fired)))
          (partial-cell (list ""))
          (,callback-sym (mevedel-agent-exec--make-callback
                          main-cb "explore" "Test task"
                          (point-min-marker) partial-cell)))
     (ignore main-cb partial-cell)
     ,@body))


;;
;;; Callback contract

(mevedel-deftest mevedel-agent-exec--make-callback ()
  ,test
  (test)

  :doc "streaming: chunks accumulate; MAIN-CB fires exactly once on `t'"
  ;; `:stream t' in info suppresses the non-streaming terminal path so
  ;; chunks accumulate without firing until gptel sends the terminal
  ;; `t' event.
  (mevedel-agent-exec-test--with-callback cb
    (let ((info '(:stream t)))
      (funcall cb "Found" info)
      (funcall cb " 2 defcustom" info)
      (funcall cb "s with :set" info)
      ;; Before the `t' signal, MAIN-CB must not have fired.
      (should (null fired))
      (funcall cb t info)
      (should (= 1 (length fired)))
      (should (equal "Found 2 defcustoms with :set"
                     (car (car fired))))))

  :doc "non-streaming (single chunk + `t'): single delivery unchanged"
  (mevedel-agent-exec-test--with-callback cb
    (funcall cb "complete response" nil)
    (funcall cb t nil)
    (should (= 1 (length fired)))
    (should (equal "complete response" (car (car fired)))))

  :doc "non-streaming (:stream absent, no `t'): string branch fires MAIN-CB"
  ;; gptel removes `:stream' from info when the request is non-streaming
  ;; and never sends a terminal `t' event -- see gptel-request.el 2864.
  ;; The string must be treated as terminal here.
  (mevedel-agent-exec-test--with-callback cb
    (funcall cb "complete non-streaming response" nil)
    (should (= 1 (length fired)))
    (should (equal "complete non-streaming response"
                   (car (car fired)))))

  :doc "non-streaming tool-use turn: string does not fire MAIN-CB"
  ;; In non-streaming, intermediate tool-use turns also arrive as
  ;; strings; we must only finalize on the final text-only turn.
  (mevedel-agent-exec-test--with-callback cb
    (funcall cb "reasoning about tool call" (list :tool-use '((:name "Read"))))
    (should (null fired))
    (funcall cb " continuation after tools" nil)
    (should (= 1 (length fired)))
    (should (equal "reasoning about tool call continuation after tools"
                   (car (car fired)))))

  :doc "streaming: single chunk on non-streaming path does not double-fire on later `t'"
  ;; Defensive: if gptel ever delivers both a non-streaming string and
  ;; a `t' (shouldn't happen in current gptel), the fired latch keeps
  ;; MAIN-CB at one invocation.
  (mevedel-agent-exec-test--with-callback cb
    (funcall cb "one-shot" nil)
    (funcall cb t nil)
    (should (= 1 (length fired)))
    (should (equal "one-shot" (car (car fired)))))

  :doc "streaming chunk with `:stream' t: string branch defers to `t'"
  ;; With `:stream' set, per-chunk firing is disabled; the 't signal
  ;; remains the terminal.  Mirrors real streaming backends.
  (mevedel-agent-exec-test--with-callback cb
    (funcall cb "partial " (list :stream t))
    (funcall cb "text" (list :stream t))
    (should (null fired))
    (funcall cb t (list :stream t))
    (should (= 1 (length fired)))
    (should (equal "partial text" (car (car fired)))))

  :doc "empty response (`t' with no prior `stringp'): single empty delivery"
  (mevedel-agent-exec-test--with-callback cb
    (funcall cb t nil)
    (should (= 1 (length fired)))
    (should (equal "" (car (car fired)))))

  :doc "tool-use guard: `t' while :tool-use is non-nil does not fire MAIN-CB"
  ;; Streaming scenario: `:stream t' keeps string chunks off the
  ;; non-streaming terminal path; the `t' signal with :tool-use set
  ;; must not fire either.
  (mevedel-agent-exec-test--with-callback cb
    (funcall cb "intermediate chunk" (list :stream t))
    (funcall cb t (list :stream t :tool-use '((:name "Read"))))
    (should (null fired))
    ;; Completion after tool-use clears fires exactly once.
    (funcall cb " continuation" (list :stream t))
    (funcall cb t (list :stream t))
    (should (= 1 (length fired)))
    (should (equal "intermediate chunk continuation"
                   (car (car fired)))))

  :doc "error (`nil'): MAIN-CB receives formatted error string once"
  (mevedel-agent-exec-test--with-callback cb
    (funcall cb nil (list :error "boom"))
    (should (= 1 (length fired)))
    (should (string-match-p "could not finish" (car (car fired)))))

  :doc "abort (`'abort'): MAIN-CB receives formatted abort string once"
  (mevedel-agent-exec-test--with-callback cb
    (funcall cb 'abort nil)
    (should (= 1 (length fired)))
    (should (string-match-p "aborted by the user" (car (car fired))))))


;;
;;; Task overlay marker

(mevedel-deftest mevedel-agent-exec--task-overlay ()
  ,test
  (test)

  :doc "overlay is tagged with the `mevedel-agent' property"
  (let ((buf (generate-new-buffer " *mev-agent-exec-ov*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "some content\n")
          (let ((ov (mevedel-agent-exec--task-overlay
                     (point-min) "explore" "Investigate thing")))
            (should (overlayp ov))
            (should (eq t (overlay-get ov 'mevedel-agent)))
            ;; Legacy upstream marker must not leak in; reads in
            ;; mevedel-chat / mevedel-tool-tutor rely on the rename.
            (should-not (overlay-get ov 'gptel-agent))))
      (when (buffer-live-p buf) (kill-buffer buf))))

  :doc "overlay carries `msg' and zero `count' state used by indicators"
  (let ((buf (generate-new-buffer " *mev-agent-exec-ov2*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "body\n")
          (let ((ov (mevedel-agent-exec--task-overlay
                     (point-min) "planner" "Draft plan")))
            (should (= 0 (overlay-get ov 'count)))
            (should (stringp (overlay-get ov 'msg)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))


(provide 'test-mevedel-agent-exec)
;;; test-mevedel-agent-exec.el ends here
