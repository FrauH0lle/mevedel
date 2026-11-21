;;; niwa-ptc-interpreter.el --- Sandboxed orchestration-script interpreter -*- lexical-binding: t -*-

;;; Commentary:
;;
;; The guest interpreter for Programmatic Tool Calling.  A "guest" script is
;; model-authored text.  It is never evaluated by Emacs; it is read into data
;; and interpreted here, and its entire world is what this file hands it: a
;; closed table of pure primitives, a closed table of syntax transformers, a
;; closed set of special forms, the tool primitives named in its roster, and
;; the script's own top-level `defun'/`defmacro' definitions, whose bodies
;; run inside this same evaluator.
;;
;; The execution interface knows nothing about gptel, the tool pipeline, or
;; sessions:
;;
;;   (niwa-ptc-start SCRIPT ROSTER)   -> state
;;   (niwa-ptc-step STATE &optional RESUME-VALUE)
;;      -> (:tool NAME ARGS) | (:tools CALLS) | (:pause)
;;         | (:done VALUE) | (:error KIND MESSAGE)
;;   (niwa-ptc-close STATE)
;;
;; The driver also uses `niwa-ptc-keyword-p',
;; `niwa-ptc-pure-primitives', `niwa-ptc-check-value', and
;; `niwa-ptc-intern' at the guest-value boundary.
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

(define-error 'niwa-ptc-error "ToolCall error")

;; `lisp-mode'
(defvar emacs-lisp-mode-syntax-table)


;;;; Limits

(defgroup niwa-ptc nil
  "Programmatic Tool Calling."
  :group 'niwa)

(defcustom niwa-ptc-max-input-bytes 20000
  "Maximum size in bytes of an orchestration script."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-ptc-max-nodes 10000
  "Maximum number of nodes in a parsed orchestration script.
This also bounds the structural walk, so circular reader syntax is
rejected by exhausting the budget rather than by cycle detection."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-ptc-max-depth 100
  "Maximum nesting depth of a parsed orchestration script."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-ptc-max-steps 500000
  "Maximum number of evaluation steps one script may take."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-ptc-step-slice 2000
  "Evaluation steps between fuel pauses.
`with-timeout' cannot bound guest computation, because Emacs runs
timers only when it is waiting for input and a tight guest loop never
yields to them.  Counting steps and pausing is what keeps Emacs
responsive."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-ptc-slice-seconds 0.03
  "Wall-clock ceiling on one uninterrupted slice of guest evaluation.

Emacs is single threaded, so a slice runs to completion with nothing else
able to run.  Bounding a slice by step count alone is not enough: the
cost of a step varies by three orders of magnitude between interpreted
and byte-compiled code, so a step-only bound blocks for milliseconds in
one configuration and minutes in another.  This bounds the block itself."
  :type 'number
  :group 'niwa)

(defcustom niwa-ptc-max-tool-calls 200
  "Maximum number of nested tool calls one script may make."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-ptc-max-seconds 300
  "Wall-clock bound on one script, checked at fuel pauses.
This is a secondary bound; the step budget is the primary one."
  :type 'number
  :group 'niwa)

(defcustom niwa-ptc-max-transformed-nodes 20000
  "Maximum node count of a single transformer's output."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-ptc-max-value-bytes 262144
  "Maximum approximate size of one guest value or primitive argument list."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-ptc-max-value-nodes 50000
  "Maximum number of aggregate nodes in one guest value."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-ptc-max-retained-bytes 4194304
  "Maximum cumulative size of values produced by one script.
This is deliberately conservative: values no longer reachable by the guest
still count until the script ends."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-ptc-max-regexp-bytes 2048
  "Maximum byte size of a regexp passed to a guest primitive."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-ptc-max-regexp-work 4194304
  "Maximum estimated regexp work for one guest operation.
The estimate is regexp bytes times input bytes raised to at least the
number of quantified atoms."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-ptc-max-number-bits 4096
  "Maximum binary width of an integer guest value."
  :type 'natnum
  :group 'niwa)

(defcustom niwa-ptc-max-stack-depth 2000
  "Maximum continuation-stack depth of a running script.
Static nesting is bounded at parse time, and loops evaluate at constant
depth, so only closure invocation can grow the stack without bound.
This caps guest recursion depth before it exhausts host memory within
the step budget."
  :type 'natnum
  :group 'niwa)


;;;; Guest values

(cl-defstruct (niwa-ptc-closure (:constructor niwa-ptc--make-closure)
                                   (:copier nil))
  "A guest lambda.  A struct so guest data cannot forge one."
  params body env)

(cl-defstruct (niwa-ptc-state (:constructor niwa-ptc--make-state)
                                 (:copier nil))
  "One mutable, suspended script.
CONTROL and ENV name the next guest form and its lexical environment.
STACK holds tagged continuation frames describing what remains after that
form returns.  DEFS holds the script's top-level definitions as an alist
of (NAME KIND . CLOSURE), where NAME is a string and KIND is `function'
or `macro'.  MODE is `eval', `return', `waiting', `done', `error', or
`closed'."
  roster direct-tool obarray control env stack defs value error-kind (mode 'eval)
  (steps 0) (tool-calls 0) (transitions 0) (next-pause 0)
  (retained-bytes 0) batch-building slice-deadline deadline)


;;;; Reader

(defun niwa-ptc--fail (kind format-string &rest args)
  "Signal a typed guest failure of KIND formatted from FORMAT-STRING and ARGS."
  (signal 'niwa-ptc-error
          (list kind (apply #'format format-string args))))

(defun niwa-ptc-keyword-p (x)
  "Return non-nil when X is a guest keyword symbol.
Guest text is read into a private obarray, where `keywordp' is nil even
for a symbol whose name begins with a colon, because `keywordp' also
requires interning in the standard obarray.  Test the name instead."
  (and x (symbolp x)
       (let ((name (symbol-name x)))
         (and (> (length name) 1) (eq (aref name 0) ?:)))))

(defun niwa-ptc--canonicalize-atom (x)
  "Return X with a guest `nil' or `t' symbol replaced by the host value.
Guest text is read into a private obarray, so the symbols named \"nil\"
and \"t\" are ordinary private symbols rather than the host constants.
Left alone they would both be truthy."
  (if (and (symbolp x) (not (niwa-ptc-keyword-p x)))
      (let ((name (symbol-name x)))
        (cond ((equal name "nil") nil)
              ((equal name "t") t)
              (t x)))
    x))

(defun niwa-ptc--validate (form)
  "Validate FORM and canonicalize its `nil' and `t' atoms in place.
Signal on an unsupported literal type, an improper list, or a script
that exceeds the node or depth budget.  Return the canonical FORM."
  (let ((nodes 0))
    (cl-labels
        ((count-node ()
           (setq nodes (1+ nodes))
           (when (> nodes niwa-ptc-max-nodes)
             (niwa-ptc--fail
              'input "Script exceeds the %d node limit"
              niwa-ptc-max-nodes)))
         (walk (x depth)
           (count-node)
           (when (> depth niwa-ptc-max-depth)
             (niwa-ptc--fail
              'input "Script nesting exceeds the depth limit of %d"
              niwa-ptc-max-depth))
           (cond
            ((or (symbolp x) (stringp x) (numberp x)) nil)
            ((vectorp x)
             (dotimes (index (length x))
               (aset x index (niwa-ptc--canonicalize-atom (aref x index)))
               (walk (aref x index) (1+ depth))))
            ((consp x)
             (let ((cur x))
               (while (consp cur)
                 (count-node)
                 (setcar cur (niwa-ptc--canonicalize-atom (car cur)))
                 (walk (car cur) (1+ depth))
                 (setq cur (cdr cur)))
               (when cur
                 (error "Improper list in script"))))
            (t
             (error "Unsupported literal of type %s in script" (type-of x))))))
      (walk form 0)))
  form)

(defun niwa-ptc--read (script)
  "Read SCRIPT into (FORM . OBARRAY).
Reading uses a private obarray so guest identifiers never enter the host
obarray.  Every top-level form is read; anything the reader cannot
consume is an error, so no trailing text is silently ignored.

There is deliberately no `read-eval' binding: that variable is Common
Lisp's, not Emacs's, and Emacs's reader has no `#.' syntax to disable."
  (unless (stringp script)
    (error "Script must be a string"))
  (when (> (string-bytes script) niwa-ptc-max-input-bytes)
    (niwa-ptc--fail
     'input "Script exceeds the %d byte limit" niwa-ptc-max-input-bytes))
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
    (cons (niwa-ptc--validate (cons 'progn forms)) private)))


;;;; Transformers

;; Pure syntax: form in, form out.  They cons lists and never evaluate.  They
;; are host functions, they run at expansion time, and that is safe precisely
;; because they are audited and do nothing but restructure.

(defun niwa-ptc--tx-when (form)
  "Transform a guest `when' FORM."
  (unless (>= (length form) 2)
    (error "'when': expects (when CONDITION BODY...)"))
  (list 'if (nth 1 form) (cons 'progn (nthcdr 2 form))))

(defun niwa-ptc--tx-unless (form)
  "Transform a guest `unless' FORM."
  (unless (>= (length form) 2)
    (error "'unless': expects (unless CONDITION BODY...)"))
  (list 'if (nth 1 form) nil (cons 'progn (nthcdr 2 form))))

(defun niwa-ptc--tx-push (form)
  "Transform a guest `push' FORM.  Symbol places only."
  (unless (= (length form) 3)
    (error "'push': expects (push VALUE VARIABLE)"))
  (let ((place (nth 2 form)))
    (unless (and place (symbolp place) (not (niwa-ptc-keyword-p place)))
      (error "'push': PLACE must be a variable name, not %S" place))
    (list 'setq place (list 'cons (nth 1 form) place))))

(defun niwa-ptc--tx-dolist (form)
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
      (unless (and var (symbolp var) (not (niwa-ptc-keyword-p var)))
        (error "'dolist': VAR must be a variable name, not %S" var))
      (list 'let (list (list tail lst))
            (list 'while tail
                  (cons 'let (cons (list (list var (list 'car tail)))
                                   (nthcdr 2 form)))
                  (list 'setq tail (list 'cdr tail)))))))

(defun niwa-ptc--tx-dotimes (form)
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
      (unless (and var (symbolp var) (not (niwa-ptc-keyword-p var)))
        (error "'dotimes': VAR must be a variable name, not %S" var))
      (list 'let (list (list limit count) (list counter 0))
            (list 'while (list '< counter limit)
                  (cons 'let (cons (list (list var counter))
                                   (nthcdr 2 form)))
                  (list 'setq counter (list '+ counter 1)))))))

(defun niwa-ptc--qq-tag (form)
  "Return `unquote', `splice', or `backquote' when FORM is that construct.
Also return the tag for a bare symbol spelled like one, so callers can
reject the dotted-unquote reader convention explicitly."
  (let ((symbol (if (consp form) (car form) form)))
    (when (and symbol (symbolp symbol))
      (pcase (symbol-name symbol)
        ("," 'unquote)
        (",@" 'splice)
        ("`" 'backquote)))))

(defun niwa-ptc--qq-unquoted (form tag)
  "Return the single expression of unquote FORM, validating its shape.
TAG names the construct for error messages."
  (unless (and (consp form) (= (length form) 2))
    (error "Malformed '%s': expects exactly one expression"
           (if (eq tag 'splice) ",@" ",")))
  (nth 1 form))

(defun niwa-ptc--qq-constant-p (form)
  "Return non-nil when template FORM contains no quasiquote construct."
  (cond
   ((niwa-ptc--qq-tag form) nil)
   ((consp form)
    (and (niwa-ptc--qq-constant-p (car form))
         (niwa-ptc--qq-constant-p (cdr form))))
   (t t)))

(defun niwa-ptc--qq-expand (template)
  "Expand one-level quasiquote TEMPLATE into `quote'/`list'/`append' calls."
  (pcase (niwa-ptc--qq-tag template)
    ('unquote (niwa-ptc--qq-unquoted template 'unquote))
    ('splice (error "',@' is only valid as a list element inside a backquote"))
    ('backquote (error "Nested backquote is not supported"))
    (_
     (if (niwa-ptc--qq-constant-p template)
         (list 'quote template)
       ;; The reader rejects improper lists, so TEMPLATE is a proper list.
       ;; Merge consecutive non-splice elements into one `list' call.
       (let ((segments nil)
             (current nil))
        (cl-flet ((flush ()
                    (when current
                      (push (cons 'list (nreverse current)) segments)
                      (setq current nil))))
          (dolist (element template)
            (pcase (niwa-ptc--qq-tag element)
              ('splice
               (when (symbolp element)
                 (error "Dotted ',@' is not supported inside a backquote"))
               (flush)
               (push (niwa-ptc--qq-unquoted element 'splice) segments))
              ((or 'unquote 'backquote)
               (when (symbolp element)
                 (error "Dotted ',' is not supported inside a backquote"))
               (push (niwa-ptc--qq-expand element) current))
              (_
               (push (niwa-ptc--qq-expand element) current))))
          (flush))
        (if (null (cdr segments))
            (car segments)
          (cons 'append (nreverse segments))))))))

(defun niwa-ptc--tx-quasiquote (form)
  "Transform a guest backquote FORM."
  (unless (= (length form) 2)
    (error "Malformed backquote"))
  (niwa-ptc--qq-expand (nth 1 form)))

(defconst niwa-ptc--transformers
  '(("when" . niwa-ptc--tx-when)
    ("unless" . niwa-ptc--tx-unless)
    ("push" . niwa-ptc--tx-push)
    ("dolist" . niwa-ptc--tx-dolist)
    ("dotimes" . niwa-ptc--tx-dotimes)
    ("`" . niwa-ptc--tx-quasiquote))
  "Closed table of guest macro-shaped forms.
Backquote expands one level here; guest `defmacro' definitions expand
inside the closed evaluator itself.  There is no `macrolet', no
guest-visible `macroexpand', no generalized places, and host
`macroexpand-all' is never applied to guest text.")


;;;; Pure primitives

(defun niwa-ptc--split-string
    (string &optional separators omit-nulls trim)
  "Split STRING without consulting host separator configuration."
  (split-string string (or separators "[ \f\t\n\r\v]+") omit-nulls trim))

(defun niwa-ptc--sort (list)
  "Return a copy of LIST sorted ascending by `value<'.
The comparator is fixed on the host side because guest closures cannot
be called from host code."
  (unless (listp list)
    (error "'sort': expects a list"))
  (sort (copy-sequence list) #'value<))

(defun niwa-ptc--gensym (&optional prefix)
  "Return a fresh uninterned guest symbol, optionally named with PREFIX.
Freshness is identity, not the name: the symbol is interned nowhere, so
it can collide with no binding even if its printed name repeats."
  (unless (or (null prefix) (stringp prefix))
    (error "'gensym': prefix must be a string"))
  (make-symbol (or prefix "g")))

(defconst niwa-ptc-pure-primitives
  '(("car" . car) ("cdr" . cdr) ("caar" . caar) ("cadr" . cadr)
    ("cdar" . cdar) ("cddr" . cddr) ("cons" . cons) ("list" . list)
    ("append" . append) ("length" . length) ("nth" . nth)
    ("nthcdr" . nthcdr) ("last" . last) ("take" . take)
    ("reverse" . reverse) ("sort" . niwa-ptc--sort)
    ("member" . member) ("memq" . memq) ("assoc" . assoc) ("assq" . assq)
    ("plist-get" . plist-get) ("plist-member" . plist-member)
    ("concat" . concat) ("format" . format) ("substring" . substring)
    ("split-string" . niwa-ptc--split-string)
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
    ("sequencep" . sequencep) ("identity" . identity)
    ("gensym" . niwa-ptc--gensym))
  "Closed, hand-audited table of side-effect-free data operations.
Deliberately not derived from the `pure' or `side-effect-free' symbol
properties: that set is open, shifts with loaded packages, and includes
host-state readers such as `getenv', `buffer-substring',
`symbol-function', and `user-login-name'.  Ambient host reads belong to
the tool channel, where they are permission-checked and audited.
`gensym' reads no host state and its symbols are fresh by identity, not
by name.")

;;;; Budgets

(defun niwa-ptc--value-size
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
          (when (> nodes niwa-ptc-max-value-nodes)
            (niwa-ptc--fail
             'value "Guest value exceeds its %d node budget"
             niwa-ptc-max-value-nodes))
          (cond
           ((null item) nil)
           ((stringp item) (setq bytes (+ bytes (string-bytes item))))
           ((integerp item)
            (when (>= (abs item) (ash 1 niwa-ptc-max-number-bits))
              (niwa-ptc--fail
               'value "Guest integer exceeds its %d bit budget"
               niwa-ptc-max-number-bits))
            (setq bytes (+ bytes (length (number-to-string item)))))
           ((floatp item) (setq bytes (+ bytes 8)))
           ((symbolp item)
            (setq bytes (+ bytes (string-bytes (symbol-name item)))))
           ((niwa-ptc-closure-p item)
            (when serializable-p
              (niwa-ptc--fail
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
          (when (> bytes niwa-ptc-max-value-bytes)
            (niwa-ptc--fail
             'value "Guest value exceeds its value byte budget of %d"
             niwa-ptc-max-value-bytes)))))
    (cons bytes nodes)))

(defun niwa-ptc-check-value
    (state value &optional retain-p count-shared-p serializable-p)
  "Validate VALUE against guest budgets and return it.
When RETAIN-P is non-nil, conservatively charge its size to STATE.
When COUNT-SHARED-P is non-nil, count repeated aggregate references.
When SERIALIZABLE-P is non-nil, reject guest closures."
  (let ((bytes (car (niwa-ptc--value-size
                     value count-shared-p serializable-p))))
    (when retain-p
      ;; ponytail: cumulative charging avoids a guest heap tracker; replace it
      ;; with reachability accounting only if real scripts hit false positives.
      (let ((total (+ (niwa-ptc-state-retained-bytes state) bytes)))
        (when (> total niwa-ptc-max-retained-bytes)
          (niwa-ptc--fail
           'retained-value
           "Script exceeded its retained value budget of %d bytes"
           niwa-ptc-max-retained-bytes))
        (setf (niwa-ptc-state-retained-bytes state) total)))
    value))

(defconst niwa-ptc--max-regexp-quantifiers 8
  "Maximum quantified atoms in one guest regexp.
Unnested quantifiers on single atoms cannot blow up exponentially, but
adjacent overlapping ones (\"a*a*b\") still backtrack polynomially; this
cap and `niwa-ptc-max-regexp-work' bound that residual.")

(defun niwa-ptc--regexp-quantifier-count (regexp)
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
                         niwa-ptc--max-regexp-quantifiers)
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

(defun niwa-ptc--check-regexp (regexp input &optional reject-empty-p)
  "Validate REGEXP against atomic budgets for INPUT.
When REJECT-EMPTY-P is non-nil, reject an empty REGEXP."
  (when (stringp regexp)
    (when (> (string-bytes regexp) niwa-ptc-max-regexp-bytes)
      (niwa-ptc--fail
       'atomic-work "Guest regexp exceeds its %d byte budget"
       niwa-ptc-max-regexp-bytes))
    (let ((quantifiers (niwa-ptc--regexp-quantifier-count regexp)))
      (unless (numberp quantifiers)
        (niwa-ptc--fail
         'atomic-work
         "Guest regexp %S is outside the bounded subset: quantifiers apply only to one literal, escape, dot, or bracket class; quantified groups, stacked quantifiers, alternation, and backreferences are rejected"
         regexp))
      (condition-case err
          (string-match-p regexp "")
        (invalid-regexp
         (niwa-ptc--fail
          'atomic-work "Invalid guest regexp %S: %s"
          regexp (error-message-string err))))
      (when (stringp input)
        (let ((work (string-bytes regexp))
              (factor (max 1 (string-bytes input))))
          (dotimes (_ (max 1 quantifiers))
            (when (> work (/ niwa-ptc-max-regexp-work factor))
              (niwa-ptc--fail
               'atomic-work "Guest regexp exceeds its atomic work budget"))
            (setq work (* work factor))))))
    (when (and reject-empty-p (zerop (length regexp)))
      (niwa-ptc--fail
       'atomic-work "'split-string': an empty separator is forbidden"))))

(defun niwa-ptc--check-primitive-input (state name args)
  "Validate primitive NAME and its evaluated ARGS before host execution."
  (niwa-ptc-check-value state args)
  (niwa-ptc--value-size args t)
  (pcase name
    ("string-match-p"
     (niwa-ptc--check-regexp (nth 0 args) (nth 1 args)))
    ("split-string"
     (niwa-ptc--check-regexp (nth 1 args) (nth 0 args) t)
     (niwa-ptc--check-regexp (nth 3 args) (nth 0 args))
     (when (and (stringp (car args))
                (> (* 2 (1+ (length (car args))))
                   niwa-ptc-max-value-nodes))
       (niwa-ptc--fail
        'atomic-work "'split-string': result may exceed the guest node budget")))
    ("string-trim"
     (niwa-ptc--check-regexp (nth 1 args) (nth 0 args))
     (niwa-ptc--check-regexp (nth 2 args) (nth 0 args))))
  (when (and (equal name "string-to-number")
             (stringp (car args))
             (> (string-bytes (car args)) 2048))
    (niwa-ptc--fail
     'atomic-work "'string-to-number': input exceeds its atomic work budget"))
  (when (equal name "format")
    (let ((format-string (car args))
          (start 0))
      (when (stringp format-string)
        (while (string-match "%[^%[:alpha:]]*[[:alpha:]]"
                             format-string start)
          (let ((spec (match-string 0 format-string))
                (number-start 0))
            (setq start (match-end 0))
            (while (string-match "[0-9]+" spec number-start)
              (when (> (string-to-number (match-string 0 spec))
                       niwa-ptc-max-value-bytes)
                (niwa-ptc--fail
                 'value "'format': field size exceeds the guest value byte budget"))
              (setq number-start (match-end 0)))))))))

(defun niwa-ptc--apply-pure (state name function args)
  "Apply pure guest FUNCTION named NAME to ARGS under atomic budgets.
File-name handlers are disabled so the `file-name-*' primitives stay
purely syntactic even on remote-looking strings."
  (save-match-data
    (niwa-ptc--check-primitive-input state name args)
    (let ((file-name-handler-alist nil))
      (niwa-ptc-check-value state (apply function args) t))))

(defun niwa-ptc--charge (state)
  "Charge one guest-form evaluation step against STATE."
  (cl-incf (niwa-ptc-state-steps state))
  (when (> (niwa-ptc-state-steps state) niwa-ptc-max-steps)
    (niwa-ptc--fail
     'step "Script exceeded its step budget of %d after %d steps; narrow the script"
     niwa-ptc-max-steps (niwa-ptc-state-steps state))))

(defconst niwa-ptc--clock-check-interval 64
  "Machine transitions between wall-clock checks inside one slice.
Guest-form steps account for script fuel.  Transitions independently
bound interpreter bookkeeping that must not hold the Emacs command loop.")

(defun niwa-ptc--due-for-pause-p (state)
  "Return non-nil when STATE should yield a fuel pause."
  (let ((steps (niwa-ptc-state-steps state)))
    (or (>= steps (niwa-ptc-state-next-pause state))
        (and (zerop (% (niwa-ptc-state-transitions state)
                       niwa-ptc--clock-check-interval))
             (let ((deadline (niwa-ptc-state-slice-deadline state)))
               (and deadline (> (float-time) deadline)))))))

(defun niwa-ptc--begin-slice (state)
  "Start a new fuel slice on STATE and enforce its wall-clock bound."
  (setf (niwa-ptc-state-next-pause state)
        (+ (niwa-ptc-state-steps state) (max 1 niwa-ptc-step-slice)))
  (setf (niwa-ptc-state-slice-deadline state)
        (and niwa-ptc-slice-seconds
             (+ (float-time) niwa-ptc-slice-seconds)))
  (let ((deadline (niwa-ptc-state-deadline state)))
    (when (and deadline (> (float-time) deadline))
      (niwa-ptc--fail
       'time "Script exceeded its time budget of %s seconds after %d steps"
       niwa-ptc-max-seconds (niwa-ptc-state-steps state)))))

(defun niwa-ptc--count-nodes (form limit)
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

(defun niwa-ptc--operator-name (form)
  "Return the operator name string of compound FORM, or nil."
  (let ((op (car form)))
    (and (symbolp op) (not (niwa-ptc-keyword-p op)) op (symbol-name op))))

(defun niwa-ptc--set-eval (state form env)
  "Make STATE evaluate FORM in ENV next."
  (setf (niwa-ptc-state-control state) form
        (niwa-ptc-state-env state) env
        (niwa-ptc-state-value state) nil
        (niwa-ptc-state-mode state) 'eval))

(defun niwa-ptc--set-return (state value)
  "Make STATE deliver VALUE to its next continuation frame."
  (setf (niwa-ptc-state-control state) nil
        (niwa-ptc-state-env state) nil
        (niwa-ptc-state-value state) value
        (niwa-ptc-state-mode state) 'return))

(defun niwa-ptc--schedule-body (state forms env)
  "Make STATE evaluate FORMS in ENV and return their last value."
  (cond
   ((null forms) (niwa-ptc--set-return state nil))
   ((null (cdr forms)) (niwa-ptc--set-eval state (car forms) env))
   (t
    (push (list :body (cdr forms) env) (niwa-ptc-state-stack state))
    (niwa-ptc--set-eval state (car forms) env))))

(defun niwa-ptc--schedule-args (state forms env)
  "Make STATE evaluate argument FORMS left to right in ENV."
  (if forms
      (progn
        (push (list :args (cdr forms) env nil)
              (niwa-ptc-state-stack state))
        (niwa-ptc--set-eval state (car forms) env))
    (niwa-ptc--set-return state nil)))

(defun niwa-ptc--check-params (params)
  "Validate lambda-list PARAMS and return it.
Only plain symbols, `&optional', and one `&rest' followed by exactly one
name are accepted; `cl-lib' markers such as `&key' fail closed instead
of silently binding as positional parameters."
  (unless (listp params)
    (error "Parameter list must be a list, not %S" params))
  (let ((names nil)
        (optional-seen nil)
        (rest-state nil))
    (dolist (param params)
      (unless (and param (symbolp param) (not (eq param t))
                   (not (niwa-ptc-keyword-p param)))
        (error "Bad parameter name: %S" param))
      (let ((name (symbol-name param)))
        (cond
         ((eq rest-state 'done)
          (error "Nothing may follow the '&rest' parameter"))
         ((equal name "&optional")
          (when (or optional-seen rest-state)
            (error "Misplaced '&optional' in parameter list"))
          (setq optional-seen t))
         ((equal name "&rest")
          (when rest-state
            (error "Duplicate '&rest' in parameter list"))
          (setq rest-state 'pending))
         ((string-prefix-p "&" name)
          (error "Unsupported lambda-list marker '%s'; only '&optional' and '&rest' exist"
                 name))
         (t
          (when (member name names)
            (error "Duplicate parameter: %s" name))
          (push name names)
          (when (eq rest-state 'pending)
            (setq rest-state 'done))))))
    (when (eq rest-state 'pending)
      (error "'&rest' must be followed by one parameter name")))
  params)

(defun niwa-ptc--check-arity (params args)
  "Signal unless ARGS satisfies the validated lambda-list PARAMS."
  (let ((required 0)
        (total 0)
        (optional nil)
        (restp nil))
    (dolist (param params)
      (pcase (symbol-name param)
        ("&optional" (setq optional t))
        ("&rest" (setq restp t))
        (_ (unless restp
             (setq total (1+ total))
             (unless optional (setq required (1+ required)))))))
    (let ((count (length args)))
      (when (or (< count required) (and (not restp) (> count total)))
        (error "Wrong number of arguments: expected %s, got %d"
               (cond (restp (format "at least %d" required))
                     ((= required total) (number-to-string required))
                     (t (format "%d to %d" required total)))
               count)))))

(defun niwa-ptc--lookup-def (state name)
  "Return STATE's definition entry (KIND . CLOSURE) for NAME, or nil."
  (cdr (assoc name (niwa-ptc-state-defs state))))

(defun niwa-ptc--check-callable (callable state)
  "Signal unless CALLABLE is invocable under STATE.
Checked before the remaining arguments are evaluated."
  (cond
   ((niwa-ptc-closure-p callable) t)
   ((and callable (symbolp callable))
    (let* ((name (symbol-name callable))
           (def (niwa-ptc--lookup-def state name)))
      (cond
       ((eq (car-safe def) 'function) t)
       (def (error "Macro %s cannot be used as a function value" name))
       ((or (assoc name niwa-ptc-pure-primitives)
            (member name (niwa-ptc-state-roster state)))
        t)
       (t (error "Void or forbidden function: %s" name)))))
   (t (error "Not callable: %S" callable))))

(defun niwa-ptc--suspend-tool (state name args)
  "Suspend STATE for a tool call to NAME with ARGS."
  (when (niwa-ptc-state-batch-building state)
    (error "A parallel call argument may not call another tool"))
  (cl-incf (niwa-ptc-state-tool-calls state))
  (when (> (niwa-ptc-state-tool-calls state) niwa-ptc-max-tool-calls)
    (niwa-ptc--fail
     'tool-call
     "Script exceeded its budget of %d tool calls before nested call %s at call %d"
     niwa-ptc-max-tool-calls name
     (niwa-ptc-state-tool-calls state)))
  (setf (niwa-ptc-state-mode state) 'waiting)
  (throw 'niwa-ptc-yield (list :tool name args)))

(defun niwa-ptc--suspend-tools (state calls)
  "Suspend STATE for ordered parallel CALLS of the form (NAME ARGS)."
  (cl-incf (niwa-ptc-state-tool-calls state) (length calls))
  (when (> (niwa-ptc-state-tool-calls state) niwa-ptc-max-tool-calls)
    (niwa-ptc--fail
     'tool-call
     "Script exceeded its budget of %d tool calls before a %d-call parallel batch at call %d"
     niwa-ptc-max-tool-calls (length calls)
     (niwa-ptc-state-tool-calls state)))
  (setf (niwa-ptc-state-batch-building state) nil
        (niwa-ptc-state-mode state) 'waiting)
  (throw 'niwa-ptc-yield (list :tools calls)))

(defun niwa-ptc--parallel-call-parts (state form)
  "Return (NAME ARGS) for direct tool call FORM under STATE."
  (let ((name (and (consp form) (niwa-ptc--operator-name form))))
    (unless (and name (member name (niwa-ptc-state-roster state)))
      (error "'parallel': each entry must be one direct tool call"))
    (list name (cdr form))))

(defun niwa-ptc--schedule-parallel-call (state forms env out)
  "Evaluate the next direct tool call in parallel FORMS.
OUT contains completed (NAME ARGS) descriptors in reverse order."
  (if forms
      (pcase-let* ((`(,name ,args)
                    (niwa-ptc--parallel-call-parts state (car forms))))
        (setf (niwa-ptc-state-batch-building state) t)
        (push (list :parallel-call name (cdr forms) env out)
              (niwa-ptc-state-stack state))
        (niwa-ptc--schedule-args state args env))
    (if out
        (niwa-ptc--suspend-tools state (nreverse out))
      (setf (niwa-ptc-state-batch-building state) nil)
      (niwa-ptc--set-return state nil))))

(defun niwa-ptc--schedule-parallel-map-call
    (state items param name arg-forms env out)
  "Build ordered NAME calls for parallel-map ITEMS.
PARAM, ARG-FORMS, and ENV describe the restricted one-call lambda."
  (if items
      (progn
        (setf (niwa-ptc-state-batch-building state) t)
        (push (list :parallel-map-call (cdr items) param name arg-forms env out)
              (niwa-ptc-state-stack state))
        (niwa-ptc--schedule-args
         state arg-forms (cons (cons param (car items)) env)))
    (if out
        (niwa-ptc--suspend-tools state (nreverse out))
      (setf (niwa-ptc-state-batch-building state) nil)
      (niwa-ptc--set-return state nil))))

(defun niwa-ptc--schedule-invoke (state callable args)
  "Make STATE apply CALLABLE to already-evaluated ARGS."
  (cond
   ((niwa-ptc-closure-p callable)
    ;; Closure invocation is the only stack growth unbounded by the parse
    ;; depth limit, so recursion depth is capped here.
    (when (> (length (niwa-ptc-state-stack state))
             niwa-ptc-max-stack-depth)
      (niwa-ptc--fail
       'stack "Script exceeded its stack budget of %d frames; recursion is too deep"
       niwa-ptc-max-stack-depth))
    (let ((env (niwa-ptc-closure-env callable))
          (params (niwa-ptc-closure-params callable))
          (rest args))
      (niwa-ptc--check-arity params args)
      (while params
        (let ((param (pop params)))
          (cond
           ((equal (symbol-name param) "&rest")
            (setq env (cons (cons (pop params) rest) env))
            (setq rest nil))
           ((equal (symbol-name param) "&optional") nil)
           (t
            (setq env (cons (cons param (pop rest)) env))))))
      (niwa-ptc--schedule-body state
                                  (niwa-ptc-closure-body callable) env)))
   ((and callable (symbolp callable))
    (let* ((name (symbol-name callable))
           (def (niwa-ptc--lookup-def state name))
           (pure (assoc name niwa-ptc-pure-primitives)))
      (cond
       ((eq (car-safe def) 'function)
        (niwa-ptc--schedule-invoke state (cdr def) args))
       (def (error "Macro %s cannot be used as a function value" name))
       (pure (niwa-ptc--set-return
              state (niwa-ptc--apply-pure state name (cdr pure) args)))
       ((member name (niwa-ptc-state-roster state))
        (niwa-ptc--suspend-tool state name args))
       (t (error "Void or forbidden function: %s" name)))))
   (t (error "Not callable: %S" callable))))

(defun niwa-ptc--schedule-cond (state clauses env)
  "Make STATE evaluate COND CLAUSES in ENV."
  (if clauses
      (let ((clause (car clauses)))
        (push (list :cond (cdr clauses) env (cdr clause))
              (niwa-ptc-state-stack state))
        (niwa-ptc--set-eval state (car clause) env))
    (niwa-ptc--set-return state nil)))

(defun niwa-ptc--schedule-let-binding
    (state sequential bindings base-env new-env body)
  "Make STATE evaluate the next LET BINDINGS entry.
SEQUENTIAL selects `let*' semantics.  BASE-ENV evaluates parallel
initializers; NEW-ENV accumulates completed bindings."
  (if bindings
      (let* ((binding (car bindings))
             (var (if (consp binding) (car binding) binding))
             (value-form (and (consp binding) (cadr binding))))
        (push (list :let sequential (cdr bindings) base-env new-env body var)
              (niwa-ptc-state-stack state))
        (niwa-ptc--set-eval state value-form
                               (if sequential new-env base-env)))
    (niwa-ptc--schedule-body state body new-env)))

(defun niwa-ptc--schedule-setq (state forms env)
  "Make STATE evaluate the next SETQ pair from FORMS in ENV."
  (if forms
      (let ((var (car forms)))
        (unless (and var (symbolp var) (not (niwa-ptc-keyword-p var)))
          (error "'setq': bad variable name %S" var))
        (push (list :setq var (cddr forms) env)
              (niwa-ptc-state-stack state))
        (niwa-ptc--set-eval state (cadr forms) env))
    (niwa-ptc--set-return state nil)))

(defun niwa-ptc--eval-compound (state form env)
  "Make STATE evaluate compound FORM in ENV.

The operator is resolved before any argument is evaluated.  An operator
this interpreter does not know is an error with no side effects, so an
unknown wrapper around a tool call cannot run the tool first."
  (let ((name (niwa-ptc--operator-name form))
        (args (cdr form)))
    (cond
     ((null name)
      (error "Operator must be a name, not %S" (car form)))

     ;; quote
     ((equal name "quote") (niwa-ptc--set-return state (nth 1 form)))

     ;; transformer
     ((assoc name niwa-ptc--transformers)
      (niwa-ptc--charge state)
      (let ((expansion (funcall (cdr (assoc name niwa-ptc--transformers)) form)))
        (when (niwa-ptc--count-nodes expansion niwa-ptc-max-transformed-nodes)
          (niwa-ptc--fail
           'expansion "Expansion of %s exceeded its size limit" name))
        (niwa-ptc--set-eval state expansion env)))

     ((equal name "if")
      (push (list :if (nth 2 form) (nthcdr 3 form) env)
            (niwa-ptc-state-stack state))
      (niwa-ptc--set-eval state (nth 1 form) env))

     ((equal name "progn")
      (niwa-ptc--schedule-body state args env))

     ((equal name "cond")
      (niwa-ptc--schedule-cond state args env))

     ((equal name "and")
      (if args
          (progn
            (push (list :and (cdr args) env)
                  (niwa-ptc-state-stack state))
            (niwa-ptc--set-eval state (car args) env))
        (niwa-ptc--set-return state t)))

     ((equal name "or")
      (if args
          (progn
            (push (list :or (cdr args) env)
                  (niwa-ptc-state-stack state))
            (niwa-ptc--set-eval state (car args) env))
        (niwa-ptc--set-return state nil)))

     ;; Binding lists are destructured here, so a variable that happens to
     ;; share a transformer's name is never dispatched as an operator.
     ((member name '("let" "let*"))
      (niwa-ptc--schedule-let-binding
       state (equal name "let*") (nth 1 form) env env (nthcdr 2 form)))

     ((equal name "setq")
      (niwa-ptc--schedule-setq state args env))

     ((equal name "while")
      (push (list :while-test (nth 1 form) (nthcdr 2 form) env)
            (niwa-ptc-state-stack state))
      (niwa-ptc--set-eval state (nth 1 form) env))

     ((equal name "lambda")
      (niwa-ptc--set-return
       state (niwa-ptc--make-closure
              :params (niwa-ptc--check-params (nth 1 form))
              :body (nthcdr 2 form)
              :env env)))

     ((member name '("defun" "defmacro"))
      (error "'%s' is only allowed as a top-level form" name))

     ((equal name "funcall")
      (push (list :funcall-callable (nthcdr 2 form) env)
            (niwa-ptc-state-stack state))
      (niwa-ptc--set-eval state (nth 1 form) env))

     ((equal name "apply")
      (push (list :apply-callable (nthcdr 2 form) env)
            (niwa-ptc-state-stack state))
      (niwa-ptc--set-eval state (nth 1 form) env))

     ((equal name "mapcar")
      (push (list :mapcar-callable (nth 2 form) env)
            (niwa-ptc-state-stack state))
      (niwa-ptc--set-eval state (nth 1 form) env))

     ((equal name "parallel")
      (niwa-ptc--schedule-parallel-call state args env nil))

     ((equal name "parallel-map")
      (unless (= (length args) 2)
        (error "'parallel-map': expects a lambda and one list"))
      (let* ((lambda-form (car args))
             (params (and (consp lambda-form) (nth 1 lambda-form)))
             (body (and (consp lambda-form) (nthcdr 2 lambda-form))))
        (niwa-ptc--check-params params)
        (unless (and (equal "lambda" (niwa-ptc--operator-name lambda-form))
                     (= (length params) 1)
                     (symbolp (car params))
                     (not (string-prefix-p "&" (symbol-name (car params))))
                     (not (niwa-ptc-keyword-p (car params)))
                     (= (length body) 1))
          (error "'parallel-map': lambda must have one parameter and one direct tool call"))
        (pcase-let* ((`(,tool-name ,tool-args)
                      (niwa-ptc--parallel-call-parts state (car body))))
          (setf (niwa-ptc-state-batch-building state) t)
          (push (list :parallel-map-sequence (car params) tool-name tool-args env)
                (niwa-ptc-state-stack state))
          (niwa-ptc--set-eval state (cadr args) env))))

     ;; guest definition
     ((niwa-ptc--lookup-def state name)
      (pcase-let ((`(,kind . ,closure) (niwa-ptc--lookup-def state name)))
        (if (eq kind 'macro)
            (progn
              ;; The macro body receives the call's argument forms as data
              ;; and runs in the closed evaluator; the expansion then
              ;; evaluates in the call site's environment.
              (push (list :macro-expand env) (niwa-ptc-state-stack state))
              (niwa-ptc--schedule-invoke state closure args))
          (push (list :invoke closure) (niwa-ptc-state-stack state))
          (niwa-ptc--schedule-args state args env))))

     ;; Tool and pure primitives share argument evaluation.  The :direct
     ;; continuation dispatches through the same closed tables afterward.
     ((or (member name (niwa-ptc-state-roster state))
          (assoc name niwa-ptc-pure-primitives))
      (push (list :direct name) (niwa-ptc-state-stack state))
      (niwa-ptc--schedule-args state args env))

     (t (error "Void or forbidden function: %s" name)))))

(defun niwa-ptc--eval-form (state)
  "Advance STATE by beginning one guest-form evaluation."
  (niwa-ptc--charge state)
  (let ((form (niwa-ptc-state-control state))
        (env (niwa-ptc-state-env state)))
  (cond
   ((null form) (niwa-ptc--set-return state nil))
   ((eq form t) (niwa-ptc--set-return state t))
   ((niwa-ptc-keyword-p form) (niwa-ptc--set-return state form))
   ((symbolp form)
    (let ((cell (assq form env)))
      (unless cell
        (when (member (symbol-name form) '("true" "false" "null"))
          (error "Unbound variable: %s; use t for true, nil or :json-false for false, :null for null"
                 (symbol-name form)))
        (error "Unbound variable: %s" (symbol-name form)))
      (niwa-ptc--set-return state (cdr cell))))
   ((consp form)
    (niwa-ptc--eval-compound state form env))
   (t (niwa-ptc--set-return state form)))))

(defun niwa-ptc--continue (state)
  "Deliver STATE's current value through one continuation frame."
  (if (null (niwa-ptc-state-stack state))
      (progn
        (setf (niwa-ptc-state-mode state) 'done)
        (throw 'niwa-ptc-yield
               (list :done (niwa-ptc-state-value state))))
    (let ((frame (pop (niwa-ptc-state-stack state)))
          (value (niwa-ptc-state-value state)))
      (pcase frame
        (`(:body ,forms ,env)
         (niwa-ptc--schedule-body state forms env))
        (`(:args ,forms ,env ,out)
         (let ((out (cons value out)))
           (if forms
               (progn
                 (push (list :args (cdr forms) env out)
                       (niwa-ptc-state-stack state))
                 (niwa-ptc--set-eval state (car forms) env))
             (niwa-ptc--set-return
              state (niwa-ptc-check-value state (nreverse out) t)))))
        (`(:if ,then ,else ,env)
         (if value
             (niwa-ptc--set-eval state then env)
           (niwa-ptc--schedule-body state else env)))
        (`(:cond ,clauses ,env ,body)
         (if value
             (if body
                 (niwa-ptc--schedule-body state body env)
               (niwa-ptc--set-return state value))
           (niwa-ptc--schedule-cond state clauses env)))
        (`(:and ,forms ,env)
         (if (and value forms)
             (progn
               (push (list :and (cdr forms) env)
                     (niwa-ptc-state-stack state))
               (niwa-ptc--set-eval state (car forms) env))
           (niwa-ptc--set-return state value)))
        (`(:or ,forms ,env)
         (if (or value (null forms))
             (niwa-ptc--set-return state value)
           (push (list :or (cdr forms) env)
                 (niwa-ptc-state-stack state))
           (niwa-ptc--set-eval state (car forms) env)))
        (`(:let ,sequential ,bindings ,base-env ,new-env ,body ,var)
         (unless (and var (symbolp var) (not (niwa-ptc-keyword-p var)))
           (error "'let': bad binding name %S" var))
         (niwa-ptc--schedule-let-binding
          state sequential bindings base-env (cons (cons var value) new-env)
          body))
        (`(:setq ,var ,forms ,env)
         (let ((cell (assq var env)))
           (unless cell
             (error "'setq': unbound variable %s" (symbol-name var)))
           (setcdr cell value))
         (if forms
             (niwa-ptc--schedule-setq state forms env)
           (niwa-ptc--set-return state value)))
        (`(:while-test ,test ,body ,env)
         (if value
             (progn
               (push (list :while-body test body env)
                     (niwa-ptc-state-stack state))
               (niwa-ptc--schedule-body state body env))
           (niwa-ptc--set-return state nil)))
        (`(:while-body ,test ,body ,env)
         (push (list :while-test test body env)
               (niwa-ptc-state-stack state))
         (niwa-ptc--set-eval state test env))
        (`(:funcall-callable ,forms ,env)
         (niwa-ptc--check-callable value state)
         (push (list :invoke value) (niwa-ptc-state-stack state))
         (niwa-ptc--schedule-args state forms env))
        (`(:apply-callable ,forms ,env)
         (niwa-ptc--check-callable value state)
         (push (list :apply-invoke value) (niwa-ptc-state-stack state))
         (niwa-ptc--schedule-args state forms env))
        (`(:mapcar-callable ,sequence-form ,env)
         (niwa-ptc--check-callable value state)
         (push (list :mapcar-sequence value)
               (niwa-ptc-state-stack state))
         (niwa-ptc--set-eval state sequence-form env))
        (`(:invoke ,callable)
         (niwa-ptc--schedule-invoke state callable value))
        (`(:macro-expand ,env)
         ;; The expansion is guest data about to become code: bound it like
         ;; a transformer's output, then hold it to the reader's contract
         ;; (proper lists, supported atoms, no closures) before evaluating
         ;; it in the call site's environment.
         (when (niwa-ptc--count-nodes value niwa-ptc-max-transformed-nodes)
           (niwa-ptc--fail
            'expansion "Macro expansion exceeded its size limit"))
         (niwa-ptc--validate value)
         (niwa-ptc--set-eval state value env))
        (`(:apply-invoke ,callable)
         (let ((spread (car (last value)))
               (fixed (butlast value)))
           (unless (listp spread)
             (error "'apply': last argument must be a list"))
           (niwa-ptc--schedule-invoke state callable
                                         (append fixed spread))))
        (`(:mapcar-sequence ,callable)
         (unless (listp value)
           (error "'mapcar': expects a list"))
         (if value
             (progn
               (push (list :mapcar-item callable (cdr value) nil)
                     (niwa-ptc-state-stack state))
               (niwa-ptc--schedule-invoke state callable (list (car value))))
           (niwa-ptc--set-return state nil)))
        (`(:mapcar-item ,callable ,items ,out)
         (let ((out (cons value out)))
           (if items
               (progn
                 (push (list :mapcar-item callable (cdr items) out)
                       (niwa-ptc-state-stack state))
                 (niwa-ptc--schedule-invoke state callable
                                               (list (car items))))
             (niwa-ptc--set-return
              state (niwa-ptc-check-value state (nreverse out) t)))))
        (`(:parallel-call ,name ,forms ,env ,out)
         (niwa-ptc--schedule-parallel-call
          state forms env (cons (list name value) out)))
        (`(:parallel-map-sequence ,param ,name ,arg-forms ,env)
         (unless (listp value)
           (error "'parallel-map': expects a list"))
         (niwa-ptc--schedule-parallel-map-call
          state value param name arg-forms env nil))
        (`(:parallel-map-call ,items ,param ,name ,arg-forms ,env ,out)
         (niwa-ptc--schedule-parallel-map-call
          state items param name arg-forms env
          (cons (list name value) out)))
        (`(:direct ,name)
         (if (member name (niwa-ptc-state-roster state))
             (niwa-ptc--suspend-tool state name value)
           (niwa-ptc--set-return
            state (niwa-ptc--apply-pure
                   state name (cdr (assoc name niwa-ptc-pure-primitives))
                   value))))
        (_ (error "Unknown interpreter continuation: %S" frame))))))


;;;; Preflight

(defconst niwa-ptc--special-form-names
  '("quote" "if" "progn" "cond" "and" "or" "let" "let*" "setq" "while"
    "lambda" "funcall" "apply" "mapcar" "parallel" "parallel-map")
  "Operator names `niwa-ptc--eval-compound' handles directly.")

(defun niwa-ptc--suggestion (name known)
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

(defun niwa-ptc--preflight-literal-regexps (name args)
  "Validate literal regexp arguments of primitive NAME applied to ARGS."
  (dolist (index (pcase name
                   ("string-match-p" '(0))
                   ("split-string" '(1 3))
                   ("string-trim" '(1 2))
                   (_ nil)))
    (let ((argument (nth index args)))
      (when (stringp argument)
        (niwa-ptc--check-regexp
         argument nil
         (and (equal name "split-string") (= index 1)))))))

(defun niwa-ptc--preflight (form roster &optional fn-names macro-names forbidden-tools)
  "Reject FORM when it names operators outside the closed tables.

The evaluator resolves every compound form's operator against the same
closed tables, never against the lexical environment, so this static
walk is exact for evaluated positions.  Unknown names in never-taken
branches are rejected too; dead code is not a reason to ship an unknown
call.  Quoted data, binding-name positions, and lambda parameter lists
are skipped.  Literal regexp arguments to the regexp primitives are
validated here so a bad regexp fails before any nested tool runs.

FN-NAMES and MACRO-NAMES are the script's own top-level definitions.
Function call arguments are walked as code; macro call arguments are
skipped, because only the macro decides which of them are code.
Backquote templates are walked through their real expansion, so a
malformed template fails here with the transformer's own message.

FORBIDDEN-TOOLS names standalone tools unavailable in a composed expression.
Literal indirect references are checked too; generated calls remain subject
to the same runtime roster before dispatch.

All unknown operators are collected and reported in one error, each with
the nearest known name from ROSTER and the closed tables."
  (let ((unknown nil))
    (cl-labels
        ((walk-all (forms)
           (dolist (item forms) (walk item)))
         (walk (form)
           (when (consp form)
             (let ((name (niwa-ptc--operator-name form))
                   (args (cdr form)))
               (cond
                ((null name) (walk-all form))
                ((equal name "quote") nil)
                ((or (member name forbidden-tools)
                     (and (member name '("funcall" "apply" "mapcar"))
                          (consp (car args))
                          (equal (niwa-ptc--operator-name (car args)) "quote")
                          (symbolp (cadr (car args)))
                          (member (symbol-name (cadr (car args))) forbidden-tools)))
                 (niwa-ptc--fail
                  'preflight
                  "Standalone-only tools require one direct call as the entire expression"))
                ((equal name "`")
                 (walk (niwa-ptc--tx-quasiquote form)))
                ((member name '("," ",@"))
                 (niwa-ptc--fail
                  'preflight
                  "'%s' is only valid inside a backquote template" name))
                ((member name '("defun" "defmacro"))
                 (niwa-ptc--fail
                  'preflight
                  "'%s' is only allowed as a top-level form of the script"
                  name))
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
                ((member name macro-names) nil)
                ((or (member name niwa-ptc--special-form-names)
                     (assoc name niwa-ptc--transformers)
                     (member name roster)
                     (member name fn-names))
                 (walk-all args))
                ((assoc name niwa-ptc-pure-primitives)
                 (niwa-ptc--preflight-literal-regexps name args)
                 (walk-all args))
                (t
                 (unless (member name unknown)
                   (push name unknown))
                 (walk-all args)))))))
      (walk form))
    (when unknown
      (let ((known (append niwa-ptc--special-form-names
                           (mapcar #'car niwa-ptc--transformers)
                           (mapcar #'car niwa-ptc-pure-primitives)
                           roster fn-names macro-names)))
        (niwa-ptc--fail
         'preflight
         "Unknown functions: %s.  Callable names are limited to the documented special forms and conveniences, the pure data primitives, this script's definitions, and this request's tools"
         (mapconcat
          (lambda (name)
            (if-let* ((suggestion (niwa-ptc--suggestion name known)))
                (format "%s (try: %s)" name suggestion)
              name))
          (nreverse unknown) ", "))))))

(defun niwa-ptc--collect-definitions (forms roster)
  "Split top-level FORMS into definitions and body forms.
Return (DEFS . BODY), where DEFS is the state's definitions alist.
Definitions are hoisted: they may reference one another regardless of
order, and only the non-definition forms evaluate.  A definition name
must not collide with any closed table, ROSTER tool, or prior
definition, so nothing is ever shadowed."
  (let ((defs nil)
        (body nil))
    (dolist (form forms)
      (let ((op (and (consp form) (niwa-ptc--operator-name form))))
        (if (not (member op '("defun" "defmacro")))
            (push form body)
          (let ((name (nth 1 form))
                (kind (if (equal op "defun") 'function 'macro)))
            (unless (and name (symbolp name)
                         (not (niwa-ptc-keyword-p name))
                         (not (memq name '(nil t))))
              (error "'%s': bad definition name %S" op name))
            (let ((name-string (symbol-name name)))
              (when (or (member name-string '("defun" "defmacro"))
                        (member name-string niwa-ptc--special-form-names)
                        (assoc name-string niwa-ptc--transformers)
                        (assoc name-string niwa-ptc-pure-primitives)
                        (member name-string roster)
                        (assoc name-string defs))
                (error "'%s': %s collides with an existing callable name"
                       op name-string))
              (push (cons name-string
                          (cons kind
                                (niwa-ptc--make-closure
                                 :params (niwa-ptc--check-params
                                          (nth 2 form))
                                 :body (nthcdr 3 form)
                                 :env nil)))
                    defs))))))
    (cons (nreverse defs) (nreverse body))))


;;;; Public interface

;;;###autoload
(defun niwa-ptc-tool-name-p (name)
  "Return non-nil if NAME can identify a tool in the closed language."
  (and (stringp name)
       (not (member name '("ToolCall" "ToolSearch" "defun" "defmacro" "nil" "t")))
       (not (member name niwa-ptc--special-form-names))
       (not (assoc name niwa-ptc--transformers))
       (not (assoc name niwa-ptc-pure-primitives))
       (condition-case nil
           (let* ((parsed (car (niwa-ptc--read (concat "(" name ")"))))
                  (form (cadr parsed)))
             (and (= (length parsed) 2) (consp form) (null (cdr form))
                  (symbolp (car form))
                  (not (niwa-ptc-keyword-p (car form)))
                  (equal name (symbol-name (car form)))))
         (error nil))))

(defun niwa-ptc--pure-argument-p (form)
  "Return non-nil when FORM provably cannot invoke a tool.
Only literals, quoted data and direct pure primitive applications qualify.
Indirect calls and control forms retain the normal script execution path."
  (or (atom form)
      (equal (niwa-ptc--operator-name form) "quote")
      (and (assoc (niwa-ptc--operator-name form)
                  niwa-ptc-pure-primitives)
           (cl-every #'niwa-ptc--pure-argument-p (cdr form)))))

(defun niwa-ptc--direct-tool (forms roster)
  "Return the direct tool name when FORMS is precisely one invocation."
  (let ((form (car forms)))
    (and (null (cdr forms)) (consp form) (symbolp (car form))
         (member (symbol-name (car form)) roster)
         (cl-every #'niwa-ptc--pure-argument-p (cdr form))
         (symbol-name (car form)))))

(defun niwa-ptc-start (script roster &optional standalone-tools)
  "Begin interpreting SCRIPT with tool primitives named in ROSTER.
ROSTER is a list of tool-name strings.  Return an opaque state for
`niwa-ptc-step', or signal if SCRIPT cannot be read or names
operators outside the closed tables.

STANDALONE-TOOLS may only occur as the entire direct invocation.
Guest state is fresh for every call: nothing persists between scripts."
  (let* ((parsed (niwa-ptc--read script))
         (direct (niwa-ptc--direct-tool (cdr (car parsed)) roster))
         (roster (if direct roster
                   (cl-set-difference roster standalone-tools :test #'equal)))
         (split (niwa-ptc--collect-definitions (cdr (car parsed)) roster))
         (defs (car split))
         (body (cdr split))
         (fn-names nil)
         (macro-names nil)
         (now (float-time)))
    (unless body
      (error "Script has no body form after its definitions"))
    (dolist (def defs)
      (if (eq (cadr def) 'function)
          (push (car def) fn-names)
        (push (car def) macro-names)))
    (dolist (def defs)
      (niwa-ptc--preflight
       (cons 'progn (niwa-ptc-closure-body (cddr def)))
       roster fn-names macro-names (unless direct standalone-tools)))
    (niwa-ptc--preflight (cons 'progn body) roster fn-names macro-names
                            (unless direct standalone-tools))
    (niwa-ptc--make-state
     :roster roster
     :direct-tool direct
     :obarray (cdr parsed)
     :control (cons 'progn body)
     :defs defs
     :next-pause (max 1 niwa-ptc-step-slice)
     :slice-deadline (and niwa-ptc-slice-seconds
                          (+ now niwa-ptc-slice-seconds))
     :deadline (and niwa-ptc-max-seconds
                    (+ now niwa-ptc-max-seconds)))))

(defun niwa-ptc-step (state &optional resume-value resume-retained-p)
  "Advance STATE, resuming a suspended call with RESUME-VALUE.

RESUME-RETAINED-P means the driver already charged RESUME-VALUE to STATE.

Return one of:
  (:tool NAME ARGS)  the script is suspended on a tool call
  (:tools CALLS)     suspended on ordered parallel (NAME ARGS) calls
  (:pause)           interpreter fuel; reschedule and step again
  (:done VALUE)      the script finished with VALUE
  (:error KIND MESSAGE) the script failed"
  (condition-case err
      (catch 'niwa-ptc-yield
        (pcase (niwa-ptc-state-mode state)
          ('waiting
           (niwa-ptc--set-return
            state (niwa-ptc-check-value
                   state resume-value (not resume-retained-p))))
          ('done (throw 'niwa-ptc-yield
                        (list :done (niwa-ptc-state-value state))))
          ('error (throw 'niwa-ptc-yield
                         (list :error
                               (niwa-ptc-state-error-kind state)
                               (niwa-ptc-state-value state))))
          ('closed (throw 'niwa-ptc-yield
                          '(:error closed "Interpreter is closed"))))
        (while t
          (cl-incf (niwa-ptc-state-transitions state))
          (when (niwa-ptc--due-for-pause-p state)
            (niwa-ptc--begin-slice state)
            (throw 'niwa-ptc-yield '(:pause)))
          (pcase (niwa-ptc-state-mode state)
            ('eval (niwa-ptc--eval-form state))
            ('return (niwa-ptc--continue state))
            (_ (error "Invalid interpreter state: %S"
                      (niwa-ptc-state-mode state))))))
    ((niwa-ptc-error error)
     (let ((kind (if (eq (car err) 'niwa-ptc-error)
                     (nth 1 err)
                   'script))
           (message (if (eq (car err) 'niwa-ptc-error)
                        (nth 2 err)
                      (error-message-string err))))
       (setf (niwa-ptc-state-control state) nil
             (niwa-ptc-state-env state) nil
             (niwa-ptc-state-stack state) nil
             (niwa-ptc-state-value state) message
             (niwa-ptc-state-error-kind state) kind
             (niwa-ptc-state-mode state) 'error)
       (list :error kind message)))))

(defun niwa-ptc-close (state)
  "Discard STATE and its suspended continuation."
  (setf (niwa-ptc-state-roster state) nil
        (niwa-ptc-state-obarray state) nil
        (niwa-ptc-state-control state) nil
        (niwa-ptc-state-env state) nil
        (niwa-ptc-state-stack state) nil
        (niwa-ptc-state-value state) nil
        (niwa-ptc-state-mode state) 'closed))

(defun niwa-ptc-intern (state name)
  "Intern NAME in STATE's private symbol universe.
The driver uses this to build guest-visible values, such as the keyword
of a nested-call error plist, so the script's own keyword of the same
name matches it."
  (intern name (niwa-ptc-state-obarray state)))

(provide 'niwa-ptc-interpreter)
;;; niwa-ptc-interpreter.el ends here
