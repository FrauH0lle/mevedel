;;; mevedel-ptc-interpreter.el --- Sandboxed orchestration-script interpreter -*- lexical-binding: t -*-

;;; Commentary:
;;
;; The guest interpreter for Programmatic Tool Calling.  A "guest" script is
;; model-authored text.  It is never evaluated by Emacs; it is read into data
;; and interpreted here, and its entire world is what this file hands it: a
;; closed table of pure primitives, a closed table of syntax transformers, a
;; closed set of special forms, and the tool primitives named in its roster.
;;
;; The execution interface knows nothing about gptel, the tool pipeline, or
;; sessions:
;;
;;   (mevedel-ptc-start SCRIPT ROSTER)   -> state
;;   (mevedel-ptc-step STATE &optional RESUME-VALUE)
;;      -> (:tool NAME ARGS) | (:tools CALLS) | (:pause)
;;         | (:done VALUE) | (:error KIND MESSAGE)
;;   (mevedel-ptc-close STATE)
;;
;; The driver also uses `mevedel-ptc-keyword-p',
;; `mevedel-ptc-pure-primitives', `mevedel-ptc-check-value', and
;; `mevedel-ptc-intern' at the guest-value boundary.
;;
;; A `:tool' step suspends the script; the driver runs that call through the
;; ordinary tool pipeline and resumes with its result.  A `:pause' step is
;; interpreter fuel: the driver reschedules so Emacs stays responsive.
;;
;; Boundary rules, each load-bearing:
;;
;; - Host `macroexpand-all' is never applied to guest text.  `macroexpand-all'
;;   dispatches to whatever `macro-function' the global obarray holds, and some
;;   expanders evaluate their body at expansion time, so merely tidying a
;;   script would execute it.  Macro-shaped forms are instead a closed table of
;;   transformers consulted only when a name appears in operator position.
;; - Operators resolve before arguments evaluate.  Otherwise an unknown
;;   operator wrapping a real tool call would run the tool and only then report
;;   the operator as forbidden.
;; - Guest text is read into a private obarray, so guest identifiers never
;;   enter the host obarray.  Guest symbols are therefore not `eq' to host
;;   symbols and every table here is keyed by name string.
;; - The pure-primitive table is a hand-audited literal.  It is never derived
;;   from `pure' / `side-effect-free' properties, which form an open set that
;;   includes host-state readers and shifts with whatever packages are loaded.

;;; Code:

(require 'cl-lib)
(require 'lisp-mode)

(define-error 'mevedel-ptc-error "ToolScript error")

;; `lisp-mode'
(defvar emacs-lisp-mode-syntax-table)


;;;; Limits

(defgroup mevedel-ptc nil
  "Programmatic Tool Calling."
  :group 'mevedel)

(defcustom mevedel-ptc-max-input-bytes 20000
  "Maximum size in bytes of an orchestration script."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-ptc-max-nodes 10000
  "Maximum number of nodes in a parsed orchestration script.
This also bounds the structural walk, so circular reader syntax is
rejected by exhausting the budget rather than by cycle detection."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-ptc-max-depth 100
  "Maximum nesting depth of a parsed orchestration script."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-ptc-max-steps 500000
  "Maximum number of evaluation steps one script may take."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-ptc-step-slice 2000
  "Evaluation steps between fuel pauses.
`with-timeout' cannot bound guest computation, because Emacs runs
timers only when it is waiting for input and a tight guest loop never
yields to them.  Counting steps and pausing is what keeps Emacs
responsive."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-ptc-slice-seconds 0.03
  "Wall-clock ceiling on one uninterrupted slice of guest evaluation.

Emacs is single threaded, so a slice runs to completion with nothing else
able to run.  Bounding a slice by step count alone is not enough: the
cost of a step varies by three orders of magnitude between interpreted
and byte-compiled code, so a step-only bound blocks for milliseconds in
one configuration and minutes in another.  This bounds the block itself."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-ptc-max-tool-calls 200
  "Maximum number of nested tool calls one script may make."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-ptc-max-seconds 300
  "Wall-clock bound on one script, checked at fuel pauses.
This is a secondary bound; the step budget is the primary one."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-ptc-max-transformed-nodes 20000
  "Maximum node count of a single transformer's output."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-ptc-max-value-bytes 262144
  "Maximum approximate size of one guest value or primitive argument list."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-ptc-max-value-nodes 50000
  "Maximum number of aggregate nodes in one guest value."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-ptc-max-retained-bytes 4194304
  "Maximum cumulative size of values produced by one script.
This is deliberately conservative: values no longer reachable by the guest
still count until the script ends."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-ptc-max-regexp-bytes 2048
  "Maximum byte size of a regexp passed to a guest primitive."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-ptc-max-regexp-work 4194304
  "Maximum estimated regexp work for one guest operation.
The estimate is regexp bytes times input bytes raised to at least the
number of quantified atoms."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-ptc-max-number-bits 4096
  "Maximum binary width of an integer guest value."
  :type 'natnum
  :group 'mevedel)


;;;; Guest values

(cl-defstruct (mevedel-ptc-closure (:constructor mevedel-ptc--make-closure)
                                   (:copier nil))
  "A guest lambda.  A struct so guest data cannot forge one."
  params body env)

(cl-defstruct (mevedel-ptc-state (:constructor mevedel-ptc--make-state)
                                 (:copier nil))
  "One mutable, suspended script.
CONTROL and ENV name the next guest form and its lexical environment.
STACK holds tagged continuation frames describing what remains after that
form returns.  MODE is `eval', `return', `waiting', `done', `error', or
`closed'."
  roster obarray control env stack value error-kind (mode 'eval)
  (steps 0) (tool-calls 0) (transitions 0) (next-pause 0)
  (retained-bytes 0) batch-building slice-deadline deadline)


;;;; Reader

(defun mevedel-ptc--fail (kind format-string &rest args)
  "Signal a typed guest failure of KIND formatted from FORMAT-STRING and ARGS."
  (signal 'mevedel-ptc-error
          (list kind (apply #'format format-string args))))

(defun mevedel-ptc-keyword-p (x)
  "Return non-nil when X is a guest keyword symbol.
Guest text is read into a private obarray, where `keywordp' is nil even
for a symbol whose name begins with a colon, because `keywordp' also
requires interning in the standard obarray.  Test the name instead."
  (and x (symbolp x)
       (let ((name (symbol-name x)))
         (and (> (length name) 1) (eq (aref name 0) ?:)))))

(defun mevedel-ptc--canonicalize-atom (x)
  "Return X with a guest `nil' or `t' symbol replaced by the host value.
Guest text is read into a private obarray, so the symbols named \"nil\"
and \"t\" are ordinary private symbols rather than the host constants.
Left alone they would both be truthy."
  (if (and (symbolp x) (not (mevedel-ptc-keyword-p x)))
      (let ((name (symbol-name x)))
        (cond ((equal name "nil") nil)
              ((equal name "t") t)
              (t x)))
    x))

(defun mevedel-ptc--validate (form)
  "Validate FORM and canonicalize its `nil' and `t' atoms in place.
Signal on an unsupported literal type, an improper list, or a script
that exceeds the node or depth budget.  Return the canonical FORM."
  (let ((nodes 0))
    (cl-labels
        ((count-node ()
           (setq nodes (1+ nodes))
           (when (> nodes mevedel-ptc-max-nodes)
             (mevedel-ptc--fail
              'input "Script exceeds the %d node limit"
              mevedel-ptc-max-nodes)))
         (walk (x depth)
           (count-node)
           (when (> depth mevedel-ptc-max-depth)
             (mevedel-ptc--fail
              'input "Script nesting exceeds the depth limit of %d"
              mevedel-ptc-max-depth))
           (cond
            ((null x) nil)
            ((symbolp x) nil)
            ((stringp x) nil)
            ((numberp x) nil)
            ((consp x)
             (let ((cur x))
               (while (consp cur)
                 (count-node)
                 (setcar cur (mevedel-ptc--canonicalize-atom (car cur)))
                 (walk (car cur) (1+ depth))
                 (setq cur (cdr cur)))
               (when cur
                 (error "Improper list in script"))))
            (t
             (error "Unsupported literal of type %s in script" (type-of x))))))
      (walk form 0)))
  form)

(defun mevedel-ptc--read (script)
  "Read SCRIPT into (FORM . OBARRAY).
Reading uses a private obarray so guest identifiers never enter the host
obarray.  Every top-level form is read; anything the reader cannot
consume is an error, so no trailing text is silently ignored.

There is deliberately no `read-eval' binding: that variable is Common
Lisp's, not Emacs's, and Emacs's reader has no `#.' syntax to disable."
  (unless (stringp script)
    (error "Script must be a string"))
  (when (> (string-bytes script) mevedel-ptc-max-input-bytes)
    (mevedel-ptc--fail
     'input "Script exceeds the %d byte limit" mevedel-ptc-max-input-bytes))
  (let ((forms nil)
        (pos 0)
        (private (obarray-make)))
    (let ((obarray private))
      (condition-case err
          (while t
            (let ((res (read-from-string script pos)))
              (push (car res) forms)
              (setq pos (cdr res))))
        (end-of-file
         (unless
             (with-temp-buffer
               (set-syntax-table emacs-lisp-mode-syntax-table)
               (insert (substring script pos))
               (goto-char (point-min))
               (forward-comment (point-max))
               (= (point) (point-max)))
           (signal (car err) (cdr err))))))
    (setq forms (nreverse forms))
    (unless forms
      (error "Script is empty"))
    (cons (mevedel-ptc--validate (cons 'progn forms)) private)))


;;;; Transformers

;; Pure syntax: form in, form out.  They cons lists and never evaluate.  They
;; are host functions, they run at expansion time, and that is safe precisely
;; because they are audited and do nothing but restructure.

(defun mevedel-ptc--tx-when (form)
  "Transform a guest `when' FORM."
  (unless (>= (length form) 2)
    (error "'when': expects (when CONDITION BODY...)"))
  (list 'if (nth 1 form) (cons 'progn (nthcdr 2 form))))

(defun mevedel-ptc--tx-unless (form)
  "Transform a guest `unless' FORM."
  (unless (>= (length form) 2)
    (error "'unless': expects (unless CONDITION BODY...)"))
  (list 'if (nth 1 form) nil (cons 'progn (nthcdr 2 form))))

(defun mevedel-ptc--tx-push (form)
  "Transform a guest `push' FORM.  Symbol places only."
  (unless (= (length form) 3)
    (error "'push': expects (push VALUE VARIABLE)"))
  (let ((place (nth 2 form)))
    (unless (and place (symbolp place) (not (mevedel-ptc-keyword-p place)))
      (error "'push': PLACE must be a variable name, not %S" place))
    (list 'setq place (list 'cons (nth 1 form) place))))

(defun mevedel-ptc--tx-dolist (form)
  "Transform a guest `dolist' FORM.
Only (dolist (VAR LIST) BODY...) is accepted.  The three-element spec
with a RESULT form is rejected rather than half-supported: under lexical
binding Emacs evaluates RESULT outside any binding of VAR, so the
commonly assumed behaviour is not the real one."
  (let ((spec (nth 1 form)))
    (unless (and (consp spec) (= (length spec) 2))
      (error "'dolist': expects (dolist (VAR LIST) BODY...); the RESULT form is not supported"))
    (let ((var (nth 0 spec))
          (lst (nth 1 spec))
          (tail (make-symbol "tail")))
      (unless (and var (symbolp var) (not (mevedel-ptc-keyword-p var)))
        (error "'dolist': VAR must be a variable name, not %S" var))
      (list 'let (list (list tail lst))
            (list 'while tail
                  (cons 'let (cons (list (list var (list 'car tail)))
                                   (nthcdr 2 form)))
                  (list 'setq tail (list 'cdr tail)))))))

(defun mevedel-ptc--tx-dotimes (form)
  "Transform a guest `dotimes' FORM.
Only (dotimes (VAR COUNT) BODY...) is accepted.  The RESULT form is
rejected for the same reason as `dolist''s: its binding behaviour under
lexical binding is not the commonly assumed one."
  (let ((spec (nth 1 form)))
    (unless (and (consp spec) (= (length spec) 2))
      (error "'dotimes': expects (dotimes (VAR COUNT) BODY...); the RESULT form is not supported"))
    (let ((var (nth 0 spec))
          (count (nth 1 spec))
          (counter (make-symbol "counter"))
          (limit (make-symbol "limit")))
      (unless (and var (symbolp var) (not (mevedel-ptc-keyword-p var)))
        (error "'dotimes': VAR must be a variable name, not %S" var))
      (list 'let (list (list limit count) (list counter 0))
            (list 'while (list '< counter limit)
                  (cons 'let (cons (list (list var counter))
                                   (nthcdr 2 form)))
                  (list 'setq counter (list '+ counter 1)))))))

(defconst mevedel-ptc--transformers
  '(("when" . mevedel-ptc--tx-when)
    ("unless" . mevedel-ptc--tx-unless)
    ("push" . mevedel-ptc--tx-push)
    ("dolist" . mevedel-ptc--tx-dolist)
    ("dotimes" . mevedel-ptc--tx-dotimes))
  "Closed table of guest macro-shaped forms.
There is no macro system: no `defmacro', no `macrolet', no backquote,
no guest-visible `macroexpand', and no generalized places.")


;;;; Pure primitives

(defun mevedel-ptc--split-string
    (string &optional separators omit-nulls trim)
  "Split STRING without consulting host separator configuration."
  (split-string string (or separators "[ \f\t\n\r\v]+") omit-nulls trim))

(defun mevedel-ptc--sort (list)
  "Return a copy of LIST sorted ascending by `value<'.
The comparator is fixed on the host side because guest closures cannot
be called from host code."
  (unless (listp list)
    (error "'sort': expects a list"))
  (sort (copy-sequence list) #'value<))

(defconst mevedel-ptc-pure-primitives
  '(("car" . car) ("cdr" . cdr) ("caar" . caar) ("cadr" . cadr)
    ("cdar" . cdar) ("cddr" . cddr) ("cons" . cons) ("list" . list)
    ("append" . append) ("length" . length) ("nth" . nth)
    ("nthcdr" . nthcdr) ("last" . last) ("take" . take)
    ("reverse" . reverse) ("sort" . mevedel-ptc--sort)
    ("member" . member) ("memq" . memq) ("assoc" . assoc) ("assq" . assq)
    ("plist-get" . plist-get) ("plist-member" . plist-member)
    ("concat" . concat) ("format" . format) ("substring" . substring)
    ("split-string" . mevedel-ptc--split-string)
    ("string-join" . string-join)
    ("file-name-nondirectory" . file-name-nondirectory)
    ("file-name-directory" . file-name-directory)
    ("file-name-concat" . file-name-concat)
    ("file-name-extension" . file-name-extension)
    ("file-name-sans-extension" . file-name-sans-extension)
    ("file-name-base" . file-name-base)
    ("string-trim" . string-trim) ("string-prefix-p" . string-prefix-p)
    ("string-suffix-p" . string-suffix-p) ("string-match-p" . string-match-p)
    ("string-search" . string-search) ("regexp-quote" . regexp-quote)
    ("upcase" . upcase) ("downcase" . downcase) ("capitalize" . capitalize)
    ("number-to-string" . number-to-string)
    ("string-to-number" . string-to-number)
    ("+" . +) ("-" . -) ("*" . *) ("/" . /) ("%" . %) ("mod" . mod)
    ("abs" . abs) ("max" . max) ("min" . min)
    ("float" . float) ("truncate" . truncate) ("round" . round)
    ("floor" . floor) ("ceiling" . ceiling)
    ("=" . =) ("/=" . /=) ("<" . <) (">" . >) ("<=" . <=) (">=" . >=)
    ("eq" . eq) ("eql" . eql) ("equal" . equal)
    ("null" . null) ("not" . not) ("atom" . atom) ("consp" . consp)
    ("listp" . listp) ("stringp" . stringp) ("numberp" . numberp)
    ("integerp" . integerp) ("floatp" . floatp) ("symbolp" . symbolp)
    ("sequencep" . sequencep) ("identity" . identity))
  "Closed, hand-audited table of side-effect-free data operations.
Deliberately not derived from the `pure' or `side-effect-free' symbol
properties: that set is open, shifts with loaded packages, and includes
host-state readers such as `getenv', `buffer-substring',
`symbol-function', and `user-login-name'.  Ambient host reads belong to
the tool channel, where they are permission-checked and audited.")

;;;; Budgets

(defun mevedel-ptc--value-size
    (value &optional count-shared-p serializable-p)
  "Return an approximate (BYTES . NODES) size for guest VALUE.
Signal when VALUE contains a host object unavailable to the guest.
When COUNT-SHARED-P is non-nil, count each reference to a shared aggregate;
this bounds atomic work that processes the same input more than once.
When SERIALIZABLE-P is non-nil, reject guest closures."
  (let ((bytes 0)
        (nodes 0)
        (seen (make-hash-table :test #'eq))
        (pending (list (cons nil value))))
    (while pending
      (pcase-let* ((`(,exit-p . ,item) (pop pending)))
        (if exit-p
            (puthash item 'done seen)
          (setq nodes (1+ nodes))
          (when (> nodes mevedel-ptc-max-value-nodes)
            (mevedel-ptc--fail
             'value "Guest value exceeds its %d node budget"
             mevedel-ptc-max-value-nodes))
          (cond
           ((null item) nil)
           ((stringp item) (setq bytes (+ bytes (string-bytes item))))
           ((integerp item)
            (when (>= (abs item) (ash 1 mevedel-ptc-max-number-bits))
              (mevedel-ptc--fail
               'value "Guest integer exceeds its %d bit budget"
               mevedel-ptc-max-number-bits))
            (setq bytes (+ bytes (length (number-to-string item)))))
           ((floatp item) (setq bytes (+ bytes 8)))
           ((symbolp item)
            (setq bytes (+ bytes (string-bytes (symbol-name item)))))
           ((mevedel-ptc-closure-p item)
            (when serializable-p
              (mevedel-ptc--fail
               'value "Guest closures cannot cross a tool or result boundary"))
            (setq bytes (+ bytes 8)))
           ((or (consp item) (vectorp item))
            (let ((visit (gethash item seen)))
              (when (eq visit 'visiting)
                (error "Circular aggregate in guest value"))
              (when (or count-shared-p (not (eq visit 'done)))
               (puthash item 'visiting seen)
               (push (cons t item) pending)
               (if (consp item)
                   (progn
                     (setq bytes (1+ bytes))
                     (push (cons nil (cdr item)) pending)
                     (push (cons nil (car item)) pending))
                 (setq bytes (+ bytes (length item)))
                 (dotimes (index (length item))
                   (push (cons nil (aref item index)) pending))))))
           (t (error "Unsupported guest value: %S" item)))
          (when (> bytes mevedel-ptc-max-value-bytes)
            (mevedel-ptc--fail
             'value "Guest value exceeds its value byte budget of %d"
             mevedel-ptc-max-value-bytes)))))
    (cons bytes nodes)))

(defun mevedel-ptc-check-value
    (state value &optional retain-p count-shared-p serializable-p)
  "Validate VALUE against guest budgets and return it.
When RETAIN-P is non-nil, conservatively charge its size to STATE.
When COUNT-SHARED-P is non-nil, count repeated aggregate references.
When SERIALIZABLE-P is non-nil, reject guest closures."
  (let ((bytes (car (mevedel-ptc--value-size
                     value count-shared-p serializable-p))))
    (when retain-p
      ;; ponytail: cumulative charging avoids a guest heap tracker; replace it
      ;; with reachability accounting only if real scripts hit false positives.
      (let ((total (+ (mevedel-ptc-state-retained-bytes state) bytes)))
        (when (> total mevedel-ptc-max-retained-bytes)
          (mevedel-ptc--fail
           'retained-value
           "Script exceeded its retained value budget of %d bytes"
           mevedel-ptc-max-retained-bytes))
        (setf (mevedel-ptc-state-retained-bytes state) total)))
    value))

(defconst mevedel-ptc--max-regexp-quantifiers 8
  "Maximum quantified atoms in one guest regexp.
Unnested quantifiers on single atoms cannot blow up exponentially, but
adjacent overlapping ones (\"a*a*b\") still backtrack polynomially; this
cap and `mevedel-ptc-max-regexp-work' bound that residual.")

(defun mevedel-ptc--regexp-quantifier-count (regexp)
  "Return REGEXP's quantified-atom count, or nil when it is unsafe.

The subset allows `*', `+', `?', and `\\=\\{n,m\\}' on a single atom: a
literal character, an escaped character, `.', or a bracket class.  It
excludes quantified groups, a quantifier stacked on another quantifier,
alternation, backreferences, and group extensions.  Those constructs can
make Emacs's backtracking matcher spend an unbounded amount of time
inside one otherwise fuel-bounded guest step."
  (let ((index 0)
        (length (length regexp))
        (quantifiers 0)
        in-class
        ;; What the previous token was: nil (nothing quantifiable), `atom'
        ;; (a quantifiable single atom), `group' (a closed group), or
        ;; `quantifier' (a quantifier that must not be stacked on).
        prev)
    (cl-flet ((quantify ()
                (unless (eq prev 'atom) (throw 'unsafe nil))
                (when (> (cl-incf quantifiers)
                         mevedel-ptc--max-regexp-quantifiers)
                  (throw 'unsafe nil))
                (setq prev 'quantifier)))
      (catch 'unsafe
        (while (< index length)
          (let ((char (aref regexp index)))
            (cond
             ((eq char ?\\)
              (setq index (1+ index))
              (when (< index length)
                (let ((escaped (aref regexp index)))
                  (cond
                   (in-class nil)
                   ((eq escaped ?|) (throw 'unsafe nil))
                   ((and (>= escaped ?1) (<= escaped ?9)) (throw 'unsafe nil))
                   ((eq escaped ?\()
                    (when (and (< (1+ index) length)
                               (eq (aref regexp (1+ index)) ??))
                      (throw 'unsafe nil))
                    (setq prev nil))
                   ((eq escaped ?\)) (setq prev 'group))
                   ((eq escaped ?{)
                    ;; Bounded repetition: digits, optional comma+digits.
                    (quantify)
                    (let ((start (1+ index))
                          (comma-seen nil)
                          (digits 0))
                      (setq index start)
                      (catch 'closed
                        (while (< index length)
                          (let ((c (aref regexp index)))
                            (cond
                             ((and (eq c ?\\)
                                   (< (1+ index) length)
                                   (eq (aref regexp (1+ index)) ?}))
                              (when (zerop digits) (throw 'unsafe nil))
                              (setq index (1+ index))
                              (throw 'closed t))
                             ((and (>= c ?0) (<= c ?9))
                              (setq digits (1+ digits)))
                             ((and (eq c ?,) (not comma-seen) (> digits 0))
                              (setq comma-seen t))
                             (t (throw 'unsafe nil))))
                          (setq index (1+ index)))
                        ;; Unterminated \{ never reached \}.
                        (throw 'unsafe nil))))
                   (t (setq prev 'atom))))))
             ((and (not in-class) (eq char ?\[))
              (setq in-class t))
             ((and in-class (eq char ?\]))
              (setq in-class nil
                    prev 'atom))
             (in-class nil)
             ((memq char '(?* ?+ ??))
              (quantify))
             ((memq char '(?^ ?$))
              (setq prev nil))
             (t (setq prev 'atom))))
          (setq index (1+ index)))
        quantifiers))))

(defun mevedel-ptc--check-regexp (regexp input &optional reject-empty-p)
  "Validate REGEXP against atomic budgets for INPUT.
When REJECT-EMPTY-P is non-nil, reject an empty REGEXP."
  (when (stringp regexp)
    (when (> (string-bytes regexp) mevedel-ptc-max-regexp-bytes)
      (mevedel-ptc--fail
       'atomic-work "Guest regexp exceeds its %d byte budget"
       mevedel-ptc-max-regexp-bytes))
    (let ((quantifiers (mevedel-ptc--regexp-quantifier-count regexp)))
      (unless (numberp quantifiers)
        (mevedel-ptc--fail
         'atomic-work
         "Guest regexp %S is outside the bounded subset: quantifiers apply only to one literal, escape, dot, or bracket class; quantified groups, stacked quantifiers, alternation, and backreferences are rejected"
         regexp))
      (condition-case err
          (string-match-p regexp "")
        (invalid-regexp
         (mevedel-ptc--fail
          'atomic-work "Invalid guest regexp %S: %s"
          regexp (error-message-string err))))
      (when (stringp input)
        (let ((work (string-bytes regexp))
              (factor (max 1 (string-bytes input))))
          (dotimes (_ (max 1 quantifiers))
            (when (> work (/ mevedel-ptc-max-regexp-work factor))
              (mevedel-ptc--fail
               'atomic-work "Guest regexp exceeds its atomic work budget"))
            (setq work (* work factor))))))
    (when (and reject-empty-p (zerop (length regexp)))
      (mevedel-ptc--fail
       'atomic-work "'split-string': an empty separator is forbidden"))))

(defun mevedel-ptc--check-primitive-input (state name args)
  "Validate primitive NAME and its evaluated ARGS before host execution."
  (mevedel-ptc-check-value state args)
  (mevedel-ptc--value-size args t)
  (pcase name
    ("string-match-p"
     (mevedel-ptc--check-regexp (nth 0 args) (nth 1 args)))
    ("split-string"
     (mevedel-ptc--check-regexp (nth 1 args) (nth 0 args) t)
     (mevedel-ptc--check-regexp (nth 3 args) (nth 0 args))
     (when (and (stringp (car args))
                (> (* 2 (1+ (length (car args))))
                   mevedel-ptc-max-value-nodes))
       (mevedel-ptc--fail
        'atomic-work "'split-string': result may exceed the guest node budget")))
    ("string-trim"
     (mevedel-ptc--check-regexp (nth 1 args) (nth 0 args))
     (mevedel-ptc--check-regexp (nth 2 args) (nth 0 args))))
  (when (and (equal name "string-to-number")
             (stringp (car args))
             (> (string-bytes (car args)) 2048))
    (mevedel-ptc--fail
     'atomic-work "'string-to-number': input exceeds its atomic work budget"))
  (when (equal name "format")
    (let ((format-string (car args))
          (start 0))
      (when (stringp format-string)
        (while (string-match "%[^%[:alpha:]]*[[:alpha:]]"
                             format-string start)
          (let ((spec (match-string 0 format-string))
                (number-start 0))
            (while (string-match "[0-9]+" spec number-start)
              (when (> (string-to-number (match-string 0 spec))
                       mevedel-ptc-max-value-bytes)
                (mevedel-ptc--fail
                 'value "'format': field size exceeds the guest value byte budget"))
              (setq number-start (match-end 0))))
          (setq start (match-end 0)))))))

(defun mevedel-ptc--apply-pure (state name function args)
  "Apply pure guest FUNCTION named NAME to ARGS under atomic budgets.
File-name handlers are disabled so the `file-name-*' primitives stay
purely syntactic even on remote-looking strings."
  (save-match-data
    (mevedel-ptc--check-primitive-input state name args)
    (let ((file-name-handler-alist nil))
      (mevedel-ptc-check-value state (apply function args) t))))

(defun mevedel-ptc--charge (state)
  "Charge one guest-form evaluation step against STATE."
  (cl-incf (mevedel-ptc-state-steps state))
  (when (> (mevedel-ptc-state-steps state) mevedel-ptc-max-steps)
    (mevedel-ptc--fail
     'step "Script exceeded its step budget of %d after %d steps; narrow the script"
     mevedel-ptc-max-steps (mevedel-ptc-state-steps state))))

(defconst mevedel-ptc--clock-check-interval 64
  "Machine transitions between wall-clock checks inside one slice.
Guest-form steps account for script fuel.  Transitions independently
bound interpreter bookkeeping that must not hold the Emacs command loop.")

(defun mevedel-ptc--due-for-pause-p (state)
  "Return non-nil when STATE should yield a fuel pause."
  (let ((steps (mevedel-ptc-state-steps state)))
    (or (>= steps (mevedel-ptc-state-next-pause state))
        (and (zerop (% (mevedel-ptc-state-transitions state)
                       mevedel-ptc--clock-check-interval))
             (let ((deadline (mevedel-ptc-state-slice-deadline state)))
               (and deadline (> (float-time) deadline)))))))

(defun mevedel-ptc--begin-slice (state)
  "Start a new fuel slice on STATE and enforce its wall-clock bound."
  (setf (mevedel-ptc-state-next-pause state)
        (+ (mevedel-ptc-state-steps state) (max 1 mevedel-ptc-step-slice)))
  (setf (mevedel-ptc-state-slice-deadline state)
        (and mevedel-ptc-slice-seconds
             (+ (float-time) mevedel-ptc-slice-seconds)))
  (let ((deadline (mevedel-ptc-state-deadline state)))
    (when (and deadline (> (float-time) deadline))
      (mevedel-ptc--fail
       'time "Script exceeded its time budget of %s seconds after %d steps"
       mevedel-ptc-max-seconds (mevedel-ptc-state-steps state)))))

(defun mevedel-ptc--count-nodes (form limit)
  "Return non-nil when FORM has more than LIMIT nodes."
  (let ((n 0)
        (stack (list form)))
    (catch 'over
      (while stack
        (let ((x (pop stack)))
          (setq n (1+ n))
          (when (> n limit) (throw 'over t))
          (when (consp x)
            (push (car x) stack)
            (push (cdr x) stack))))
      nil)))


;;;; Evaluator

(defun mevedel-ptc--operator-name (form)
  "Return the operator name string of compound FORM, or nil."
  (let ((op (car form)))
    (and (symbolp op) (not (mevedel-ptc-keyword-p op)) op (symbol-name op))))

(defun mevedel-ptc--lookup (env symbol)
  "Return the binding cell for SYMBOL in ENV, or nil."
  (assq symbol env))

(defun mevedel-ptc--set-eval (state form env)
  "Make STATE evaluate FORM in ENV next."
  (setf (mevedel-ptc-state-control state) form
        (mevedel-ptc-state-env state) env
        (mevedel-ptc-state-value state) nil
        (mevedel-ptc-state-mode state) 'eval))

(defun mevedel-ptc--set-return (state value)
  "Make STATE deliver VALUE to its next continuation frame."
  (setf (mevedel-ptc-state-control state) nil
        (mevedel-ptc-state-env state) nil
        (mevedel-ptc-state-value state) value
        (mevedel-ptc-state-mode state) 'return))

(defun mevedel-ptc--schedule-body (state forms env)
  "Make STATE evaluate FORMS in ENV and return their last value."
  (cond
   ((null forms) (mevedel-ptc--set-return state nil))
   ((null (cdr forms)) (mevedel-ptc--set-eval state (car forms) env))
   (t
    (push (list :body (cdr forms) env) (mevedel-ptc-state-stack state))
    (mevedel-ptc--set-eval state (car forms) env))))

(defun mevedel-ptc--schedule-args (state forms env)
  "Make STATE evaluate argument FORMS left to right in ENV."
  (if forms
      (progn
        (push (list :args (cdr forms) env nil)
              (mevedel-ptc-state-stack state))
        (mevedel-ptc--set-eval state (car forms) env))
    (mevedel-ptc--set-return state nil)))

(defun mevedel-ptc--check-callable (callable state)
  "Signal unless CALLABLE is invocable under STATE.
Checked before the remaining arguments are evaluated."
  (cond
   ((mevedel-ptc-closure-p callable) t)
   ((and callable (symbolp callable))
    (let ((name (symbol-name callable)))
      (unless (or (assoc name mevedel-ptc-pure-primitives)
                  (member name (mevedel-ptc-state-roster state)))
        (error "Void or forbidden function: %s" name))))
   (t (error "Not callable: %S" callable))))

(defun mevedel-ptc--suspend-tool (state name args)
  "Suspend STATE for a tool call to NAME with ARGS."
  (when (mevedel-ptc-state-batch-building state)
    (error "A parallel call argument may not call another tool"))
  (cl-incf (mevedel-ptc-state-tool-calls state))
  (when (> (mevedel-ptc-state-tool-calls state) mevedel-ptc-max-tool-calls)
    (mevedel-ptc--fail
     'tool-call
     "Script exceeded its budget of %d tool calls before nested call %s at call %d"
     mevedel-ptc-max-tool-calls name
     (mevedel-ptc-state-tool-calls state)))
  (setf (mevedel-ptc-state-mode state) 'waiting)
  (throw 'mevedel-ptc-yield (list :tool name args)))

(defun mevedel-ptc--suspend-tools (state calls)
  "Suspend STATE for ordered parallel CALLS of the form (NAME ARGS)."
  (cl-incf (mevedel-ptc-state-tool-calls state) (length calls))
  (when (> (mevedel-ptc-state-tool-calls state) mevedel-ptc-max-tool-calls)
    (mevedel-ptc--fail
     'tool-call
     "Script exceeded its budget of %d tool calls before a %d-call parallel batch at call %d"
     mevedel-ptc-max-tool-calls (length calls)
     (mevedel-ptc-state-tool-calls state)))
  (setf (mevedel-ptc-state-batch-building state) nil
        (mevedel-ptc-state-mode state) 'waiting)
  (throw 'mevedel-ptc-yield (list :tools calls)))

(defun mevedel-ptc--parallel-call-parts (state form)
  "Return (NAME ARGS) for direct tool call FORM under STATE."
  (let ((name (and (consp form) (mevedel-ptc--operator-name form))))
    (unless (and name (member name (mevedel-ptc-state-roster state)))
      (error "'parallel': each entry must be one direct tool call"))
    (list name (cdr form))))

(defun mevedel-ptc--schedule-parallel-call (state forms env out)
  "Evaluate the next direct tool call in parallel FORMS.
OUT contains completed (NAME ARGS) descriptors in reverse order."
  (if forms
      (pcase-let* ((`(,name ,args)
                    (mevedel-ptc--parallel-call-parts state (car forms))))
        (setf (mevedel-ptc-state-batch-building state) t)
        (push (list :parallel-call name (cdr forms) env out)
              (mevedel-ptc-state-stack state))
        (mevedel-ptc--schedule-args state args env))
    (if out
        (mevedel-ptc--suspend-tools state (nreverse out))
      (setf (mevedel-ptc-state-batch-building state) nil)
      (mevedel-ptc--set-return state nil))))

(defun mevedel-ptc--schedule-parallel-map-call
    (state items param name arg-forms env out)
  "Build ordered NAME calls for parallel-map ITEMS.
PARAM, ARG-FORMS, and ENV describe the restricted one-call lambda."
  (if items
      (progn
        (setf (mevedel-ptc-state-batch-building state) t)
        (push (list :parallel-map-call (cdr items) param name arg-forms env out)
              (mevedel-ptc-state-stack state))
        (mevedel-ptc--schedule-args
         state arg-forms (cons (cons param (car items)) env)))
    (if out
        (mevedel-ptc--suspend-tools state (nreverse out))
      (setf (mevedel-ptc-state-batch-building state) nil)
      (mevedel-ptc--set-return state nil))))

(defun mevedel-ptc--schedule-invoke (state callable args)
  "Make STATE apply CALLABLE to already-evaluated ARGS."
  (cond
   ((mevedel-ptc-closure-p callable)
    (let ((env (mevedel-ptc-closure-env callable))
          (params (mevedel-ptc-closure-params callable))
          (rest args))
      (while params
        (let ((param (pop params)))
          (cond
           ((equal (symbol-name param) "&rest")
            (setq env (cons (cons (pop params) rest) env))
            (setq rest nil))
           ((equal (symbol-name param) "&optional") nil)
           (t
            (setq env (cons (cons param (pop rest)) env))))))
      (mevedel-ptc--schedule-body state
                                  (mevedel-ptc-closure-body callable) env)))
   ((and callable (symbolp callable))
    (let* ((name (symbol-name callable))
           (pure (assoc name mevedel-ptc-pure-primitives)))
      (cond
       (pure (mevedel-ptc--set-return
              state (mevedel-ptc--apply-pure state name (cdr pure) args)))
       ((member name (mevedel-ptc-state-roster state))
        (mevedel-ptc--suspend-tool state name args))
       (t (error "Void or forbidden function: %s" name)))))
   (t (error "Not callable: %S" callable))))

(defun mevedel-ptc--schedule-cond (state clauses env)
  "Make STATE evaluate COND CLAUSES in ENV."
  (if clauses
      (let ((clause (car clauses)))
        (push (list :cond (cdr clauses) env (cdr clause))
              (mevedel-ptc-state-stack state))
        (mevedel-ptc--set-eval state (car clause) env))
    (mevedel-ptc--set-return state nil)))

(defun mevedel-ptc--schedule-let-binding
    (state sequential bindings base-env new-env body)
  "Make STATE evaluate the next LET BINDINGS entry.
SEQUENTIAL selects `let*' semantics.  BASE-ENV evaluates parallel
initializers; NEW-ENV accumulates completed bindings."
  (if bindings
      (let* ((binding (car bindings))
             (var (if (consp binding) (car binding) binding))
             (value-form (and (consp binding) (cadr binding))))
        (push (list :let sequential (cdr bindings) base-env new-env body var)
              (mevedel-ptc-state-stack state))
        (mevedel-ptc--set-eval state value-form
                               (if sequential new-env base-env)))
    (mevedel-ptc--schedule-body state body new-env)))

(defun mevedel-ptc--schedule-setq (state forms env)
  "Make STATE evaluate the next SETQ pair from FORMS in ENV."
  (if forms
      (let ((var (car forms)))
        (unless (and var (symbolp var) (not (mevedel-ptc-keyword-p var)))
          (error "'setq': bad variable name %S" var))
        (push (list :setq var (cddr forms) env)
              (mevedel-ptc-state-stack state))
        (mevedel-ptc--set-eval state (cadr forms) env))
    (mevedel-ptc--set-return state nil)))

(defun mevedel-ptc--eval-compound (state form env)
  "Make STATE evaluate compound FORM in ENV.

The operator is resolved before any argument is evaluated.  An operator
this interpreter does not know is an error with no side effects, so an
unknown wrapper around a tool call cannot run the tool first."
  (let ((name (mevedel-ptc--operator-name form))
        (args (cdr form)))
    (cond
     ((null name)
      (error "Operator must be a name, not %S" (car form)))

     ;; quote
     ((equal name "quote") (mevedel-ptc--set-return state (nth 1 form)))

     ;; transformer
     ((assoc name mevedel-ptc--transformers)
      (mevedel-ptc--charge state)
      (let ((expansion (funcall (cdr (assoc name mevedel-ptc--transformers)) form)))
        (when (mevedel-ptc--count-nodes expansion mevedel-ptc-max-transformed-nodes)
          (mevedel-ptc--fail
           'expansion "Expansion of %s exceeded its size limit" name))
        (mevedel-ptc--set-eval state expansion env)))

     ((equal name "if")
      (push (list :if (nth 2 form) (nthcdr 3 form) env)
            (mevedel-ptc-state-stack state))
      (mevedel-ptc--set-eval state (nth 1 form) env))

     ((equal name "progn")
      (mevedel-ptc--schedule-body state args env))

     ((equal name "cond")
      (mevedel-ptc--schedule-cond state args env))

     ((equal name "and")
      (if args
          (progn
            (push (list :and (cdr args) env)
                  (mevedel-ptc-state-stack state))
            (mevedel-ptc--set-eval state (car args) env))
        (mevedel-ptc--set-return state t)))

     ((equal name "or")
      (if args
          (progn
            (push (list :or (cdr args) env)
                  (mevedel-ptc-state-stack state))
            (mevedel-ptc--set-eval state (car args) env))
        (mevedel-ptc--set-return state nil)))

     ;; Binding lists are destructured here, so a variable that happens to
     ;; share a transformer's name is never dispatched as an operator.
     ((member name '("let" "let*"))
      (mevedel-ptc--schedule-let-binding
       state (equal name "let*") (nth 1 form) env env (nthcdr 2 form)))

     ((equal name "setq")
      (mevedel-ptc--schedule-setq state args env))

     ((equal name "while")
      (push (list :while-test (nth 1 form) (nthcdr 2 form) env)
            (mevedel-ptc-state-stack state))
      (mevedel-ptc--set-eval state (nth 1 form) env))

     ((equal name "lambda")
      (mevedel-ptc--set-return
       state (mevedel-ptc--make-closure :params (nth 1 form)
                                        :body (nthcdr 2 form)
                                        :env env)))

     ((equal name "funcall")
      (push (list :funcall-callable (nthcdr 2 form) env)
            (mevedel-ptc-state-stack state))
      (mevedel-ptc--set-eval state (nth 1 form) env))

     ((equal name "apply")
      (push (list :apply-callable (nthcdr 2 form) env)
            (mevedel-ptc-state-stack state))
      (mevedel-ptc--set-eval state (nth 1 form) env))

     ((equal name "mapcar")
      (push (list :mapcar-callable (nth 2 form) env)
            (mevedel-ptc-state-stack state))
      (mevedel-ptc--set-eval state (nth 1 form) env))

     ((equal name "parallel")
      (mevedel-ptc--schedule-parallel-call state args env nil))

     ((equal name "parallel-map")
      (unless (= (length args) 2)
        (error "'parallel-map': expects a lambda and one list"))
      (let* ((lambda-form (car args))
             (params (and (consp lambda-form) (nth 1 lambda-form)))
             (body (and (consp lambda-form) (nthcdr 2 lambda-form))))
        (unless (and (equal "lambda" (mevedel-ptc--operator-name lambda-form))
                     (= (length params) 1)
                     (symbolp (car params))
                     (not (mevedel-ptc-keyword-p (car params)))
                     (= (length body) 1))
          (error "'parallel-map': lambda must have one parameter and one direct tool call"))
        (pcase-let* ((`(,tool-name ,tool-args)
                      (mevedel-ptc--parallel-call-parts state (car body))))
          (setf (mevedel-ptc-state-batch-building state) t)
          (push (list :parallel-map-sequence (car params) tool-name tool-args env)
                (mevedel-ptc-state-stack state))
          (mevedel-ptc--set-eval state (cadr args) env))))

     ;; tool primitive
     ((member name (mevedel-ptc-state-roster state))
      (push (list :direct name) (mevedel-ptc-state-stack state))
      (mevedel-ptc--schedule-args state args env))

     ;; pure primitive
     ((assoc name mevedel-ptc-pure-primitives)
      (push (list :direct name) (mevedel-ptc-state-stack state))
      (mevedel-ptc--schedule-args state args env))

     (t (error "Void or forbidden function: %s" name)))))

(defun mevedel-ptc--eval-form (state)
  "Advance STATE by beginning one guest-form evaluation."
  (mevedel-ptc--charge state)
  (let ((form (mevedel-ptc-state-control state))
        (env (mevedel-ptc-state-env state)))
  (cond
   ((null form) (mevedel-ptc--set-return state nil))
   ((eq form t) (mevedel-ptc--set-return state t))
   ((mevedel-ptc-keyword-p form) (mevedel-ptc--set-return state form))
   ((symbolp form)
    (let ((cell (mevedel-ptc--lookup env form)))
      (unless cell
        (error "Unbound variable: %s" (symbol-name form)))
      (mevedel-ptc--set-return state (cdr cell))))
   ((consp form)
    (mevedel-ptc--eval-compound state form env))
   (t (mevedel-ptc--set-return state form)))))

(defun mevedel-ptc--continue (state)
  "Deliver STATE's current value through one continuation frame."
  (if (null (mevedel-ptc-state-stack state))
      (progn
        (setf (mevedel-ptc-state-mode state) 'done)
        (throw 'mevedel-ptc-yield
               (list :done (mevedel-ptc-state-value state))))
    (let ((frame (pop (mevedel-ptc-state-stack state)))
          (value (mevedel-ptc-state-value state)))
      (pcase frame
        (`(:body ,forms ,env)
         (mevedel-ptc--schedule-body state forms env))
        (`(:args ,forms ,env ,out)
         (let ((out (cons value out)))
           (if forms
               (progn
                 (push (list :args (cdr forms) env out)
                       (mevedel-ptc-state-stack state))
                 (mevedel-ptc--set-eval state (car forms) env))
             (mevedel-ptc--set-return
              state (mevedel-ptc-check-value state (nreverse out) t)))))
        (`(:if ,then ,else ,env)
         (if value
             (mevedel-ptc--set-eval state then env)
           (mevedel-ptc--schedule-body state else env)))
        (`(:cond ,clauses ,env ,body)
         (if value
             (if body
                 (mevedel-ptc--schedule-body state body env)
               (mevedel-ptc--set-return state value))
           (mevedel-ptc--schedule-cond state clauses env)))
        (`(:and ,forms ,env)
         (if (and value forms)
             (progn
               (push (list :and (cdr forms) env)
                     (mevedel-ptc-state-stack state))
               (mevedel-ptc--set-eval state (car forms) env))
           (mevedel-ptc--set-return state value)))
        (`(:or ,forms ,env)
         (if (or value (null forms))
             (mevedel-ptc--set-return state value)
           (push (list :or (cdr forms) env)
                 (mevedel-ptc-state-stack state))
           (mevedel-ptc--set-eval state (car forms) env)))
        (`(:let ,sequential ,bindings ,base-env ,new-env ,body ,var)
         (unless (and var (symbolp var) (not (mevedel-ptc-keyword-p var)))
           (error "'let': bad binding name %S" var))
         (mevedel-ptc--schedule-let-binding
          state sequential bindings base-env (cons (cons var value) new-env)
          body))
        (`(:setq ,var ,forms ,env)
         (let ((cell (mevedel-ptc--lookup env var)))
           (unless cell
             (error "'setq': unbound variable %s" (symbol-name var)))
           (setcdr cell value))
         (if forms
             (mevedel-ptc--schedule-setq state forms env)
           (mevedel-ptc--set-return state value)))
        (`(:while-test ,test ,body ,env)
         (if value
             (progn
               (push (list :while-body test body env)
                     (mevedel-ptc-state-stack state))
               (mevedel-ptc--schedule-body state body env))
           (mevedel-ptc--set-return state nil)))
        (`(:while-body ,test ,body ,env)
         (push (list :while-test test body env)
               (mevedel-ptc-state-stack state))
         (mevedel-ptc--set-eval state test env))
        (`(:funcall-callable ,forms ,env)
         (mevedel-ptc--check-callable value state)
         (push (list :invoke value) (mevedel-ptc-state-stack state))
         (mevedel-ptc--schedule-args state forms env))
        (`(:apply-callable ,forms ,env)
         (mevedel-ptc--check-callable value state)
         (push (list :apply-invoke value) (mevedel-ptc-state-stack state))
         (mevedel-ptc--schedule-args state forms env))
        (`(:mapcar-callable ,sequence-form ,env)
         (mevedel-ptc--check-callable value state)
         (push (list :mapcar-sequence value)
               (mevedel-ptc-state-stack state))
         (mevedel-ptc--set-eval state sequence-form env))
        (`(:invoke ,callable)
         (mevedel-ptc--schedule-invoke state callable value))
        (`(:apply-invoke ,callable)
         (let ((spread (car (last value)))
               (fixed (butlast value)))
           (unless (listp spread)
             (error "'apply': last argument must be a list"))
           (mevedel-ptc--schedule-invoke state callable
                                         (append fixed spread))))
        (`(:mapcar-sequence ,callable)
         (unless (listp value)
           (error "'mapcar': expects a list"))
         (if value
             (progn
               (push (list :mapcar-item callable (cdr value) nil)
                     (mevedel-ptc-state-stack state))
               (mevedel-ptc--schedule-invoke state callable (list (car value))))
           (mevedel-ptc--set-return state nil)))
        (`(:mapcar-item ,callable ,items ,out)
         (let ((out (cons value out)))
           (if items
               (progn
                 (push (list :mapcar-item callable (cdr items) out)
                       (mevedel-ptc-state-stack state))
                 (mevedel-ptc--schedule-invoke state callable
                                               (list (car items))))
             (mevedel-ptc--set-return
              state (mevedel-ptc-check-value state (nreverse out) t)))))
        (`(:parallel-call ,name ,forms ,env ,out)
         (mevedel-ptc--schedule-parallel-call
          state forms env (cons (list name value) out)))
        (`(:parallel-map-sequence ,param ,name ,arg-forms ,env)
         (unless (listp value)
           (error "'parallel-map': expects a list"))
         (mevedel-ptc--schedule-parallel-map-call
          state value param name arg-forms env nil))
        (`(:parallel-map-call ,items ,param ,name ,arg-forms ,env ,out)
         (mevedel-ptc--schedule-parallel-map-call
          state items param name arg-forms env
          (cons (list name value) out)))
        (`(:direct ,name)
         (if (member name (mevedel-ptc-state-roster state))
             (mevedel-ptc--suspend-tool state name value)
           (mevedel-ptc--set-return
            state (mevedel-ptc--apply-pure
                   state name (cdr (assoc name mevedel-ptc-pure-primitives))
                   value))))
        (_ (error "Unknown interpreter continuation: %S" frame))))))


;;;; Preflight

(defconst mevedel-ptc--special-form-names
  '("quote" "if" "progn" "cond" "and" "or" "let" "let*" "setq" "while"
    "lambda" "funcall" "apply" "mapcar" "parallel" "parallel-map")
  "Operator names `mevedel-ptc--eval-compound' handles directly.")

(defun mevedel-ptc--suggestion (name known)
  "Return the KNOWN name closest to unknown NAME, or nil.
A known name that survives stripping a namespace-style prefix or suffix
from NAME counts as closest, so \"seq-take\" suggests \"take\".  Distant
matches are suppressed rather than guessed."
  (let (best (best-distance 3))
    (dolist (candidate known best)
      (let ((distance
             (if (or (string-suffix-p (concat "-" candidate) name)
                     (string-prefix-p (concat candidate "-") name))
                 1
               (string-distance name candidate))))
        (when (< distance best-distance)
          (setq best candidate
                best-distance distance))))))

(defun mevedel-ptc--preflight-literal-regexps (name args)
  "Validate literal regexp arguments of primitive NAME applied to ARGS."
  (dolist (index (pcase name
                   ("string-match-p" '(0))
                   ("split-string" '(1 3))
                   ("string-trim" '(1 2))
                   (_ nil)))
    (let ((argument (nth index args)))
      (when (stringp argument)
        (mevedel-ptc--check-regexp
         argument nil
         (and (equal name "split-string") (= index 1)))))))

(defun mevedel-ptc--preflight (form roster)
  "Reject FORM when it names operators outside the closed tables.

The evaluator resolves every compound form's operator against the same
closed tables, never against the lexical environment, so this static
walk is exact for evaluated positions.  Unknown names in never-taken
branches are rejected too; dead code is not a reason to ship an unknown
call.  Quoted data, binding-name positions, and lambda parameter lists
are skipped.  Literal regexp arguments to the regexp primitives are
validated here so a bad regexp fails before any nested tool runs.

All unknown operators are collected and reported in one error, each with
the nearest known name from ROSTER and the closed tables."
  (let ((unknown nil))
    (cl-labels
        ((walk-all (forms)
           (dolist (item forms) (walk item)))
         (walk (form)
           (when (consp form)
             (let ((name (mevedel-ptc--operator-name form))
                   (args (cdr form)))
               (cond
                ((null name) (walk-all form))
                ((equal name "quote") nil)
                ((equal name "lambda")
                 (walk-all (nthcdr 2 form)))
                ((member name '("let" "let*"))
                 (dolist (binding (nth 1 form))
                   (when (consp binding)
                     (walk-all (cdr binding))))
                 (walk-all (nthcdr 2 form)))
                ((equal name "cond")
                 (dolist (clause args)
                   (when (listp clause)
                     (walk-all clause))))
                ((member name '("dolist" "dotimes"))
                 (let ((spec (nth 1 form)))
                   (when (consp spec)
                     (walk (nth 1 spec))))
                 (walk-all (nthcdr 2 form)))
                ((or (member name mevedel-ptc--special-form-names)
                     (assoc name mevedel-ptc--transformers)
                     (member name roster))
                 (walk-all args))
                ((assoc name mevedel-ptc-pure-primitives)
                 (mevedel-ptc--preflight-literal-regexps name args)
                 (walk-all args))
                (t
                 (unless (member name unknown)
                   (push name unknown))
                 (walk-all args)))))))
      (walk form))
    (when unknown
      (let ((known (append mevedel-ptc--special-form-names
                           (mapcar #'car mevedel-ptc--transformers)
                           (mapcar #'car mevedel-ptc-pure-primitives)
                           roster)))
        (mevedel-ptc--fail
         'preflight
         "Unknown functions: %s.  Callable names are limited to the documented special forms and conveniences, the pure data primitives, and this request's tools"
         (mapconcat
          (lambda (name)
            (if-let* ((suggestion (mevedel-ptc--suggestion name known)))
                (format "%s (try: %s)" name suggestion)
              name))
          (nreverse unknown) ", "))))))


;;;; Public interface

(defun mevedel-ptc-start (script roster)
  "Begin interpreting SCRIPT with tool primitives named in ROSTER.
ROSTER is a list of tool-name strings.  Return an opaque state for
`mevedel-ptc-step', or signal if SCRIPT cannot be read or names
operators outside the closed tables.

Guest state is fresh for every call: nothing persists between scripts."
  (let* ((parsed (mevedel-ptc--read script))
         (now (float-time)))
    (mevedel-ptc--preflight (car parsed) roster)
    (mevedel-ptc--make-state
     :roster roster
     :obarray (cdr parsed)
     :control (car parsed)
     :next-pause (max 1 mevedel-ptc-step-slice)
     :slice-deadline (and mevedel-ptc-slice-seconds
                          (+ now mevedel-ptc-slice-seconds))
     :deadline (and mevedel-ptc-max-seconds
                    (+ now mevedel-ptc-max-seconds)))))

(defun mevedel-ptc-step (state &optional resume-value resume-retained-p)
  "Advance STATE, resuming a suspended call with RESUME-VALUE.

RESUME-RETAINED-P means the driver already charged RESUME-VALUE to STATE.

Return one of:
  (:tool NAME ARGS)  the script is suspended on a tool call
  (:tools CALLS)     suspended on ordered parallel (NAME ARGS) calls
  (:pause)           interpreter fuel; reschedule and step again
  (:done VALUE)      the script finished with VALUE
  (:error KIND MESSAGE) the script failed"
  (condition-case err
      (catch 'mevedel-ptc-yield
        (pcase (mevedel-ptc-state-mode state)
          ('waiting
           (mevedel-ptc--set-return
            state (mevedel-ptc-check-value
                   state resume-value (not resume-retained-p))))
          ('done (throw 'mevedel-ptc-yield
                        (list :done (mevedel-ptc-state-value state))))
          ('error (throw 'mevedel-ptc-yield
                         (list :error
                               (mevedel-ptc-state-error-kind state)
                               (mevedel-ptc-state-value state))))
          ('closed (throw 'mevedel-ptc-yield
                          '(:error closed "Interpreter is closed"))))
        (while t
          (cl-incf (mevedel-ptc-state-transitions state))
          (when (mevedel-ptc--due-for-pause-p state)
            (mevedel-ptc--begin-slice state)
            (throw 'mevedel-ptc-yield '(:pause)))
          (pcase (mevedel-ptc-state-mode state)
            ('eval (mevedel-ptc--eval-form state))
            ('return (mevedel-ptc--continue state))
            (_ (error "Invalid interpreter state: %S"
                      (mevedel-ptc-state-mode state))))))
    ((mevedel-ptc-error error)
     (let ((kind (if (eq (car err) 'mevedel-ptc-error)
                     (nth 1 err)
                   'script))
           (message (if (eq (car err) 'mevedel-ptc-error)
                        (nth 2 err)
                      (error-message-string err))))
       (setf (mevedel-ptc-state-control state) nil
             (mevedel-ptc-state-env state) nil
             (mevedel-ptc-state-stack state) nil
             (mevedel-ptc-state-value state) message
             (mevedel-ptc-state-error-kind state) kind
             (mevedel-ptc-state-mode state) 'error)
       (list :error kind message)))))

(defun mevedel-ptc-close (state)
  "Discard STATE and its suspended continuation."
  (setf (mevedel-ptc-state-roster state) nil
        (mevedel-ptc-state-obarray state) nil
        (mevedel-ptc-state-control state) nil
        (mevedel-ptc-state-env state) nil
        (mevedel-ptc-state-stack state) nil
        (mevedel-ptc-state-value state) nil
        (mevedel-ptc-state-mode state) 'closed))

(defun mevedel-ptc-intern (state name)
  "Intern NAME in STATE's private symbol universe.
The driver uses this to build guest-visible values, such as the keyword
of a nested-call error plist, so the script's own keyword of the same
name matches it."
  (intern name (mevedel-ptc-state-obarray state)))

(provide 'mevedel-ptc-interpreter)
;;; mevedel-ptc-interpreter.el ends here
