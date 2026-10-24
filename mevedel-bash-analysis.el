;;; mevedel-bash-analysis.el --- Conservative Bash analysis -*- lexical-binding: t -*-

;;; Commentary:

;; Produces normalized facts for Bash permission decisions.  A configured
;; Bash Tree-sitter grammar supplies structured parsing when available; a
;; deliberately small scanner provides the same conservative contract when it
;; is not.  This module classifies syntax and commands but grants no authority.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'subr-x)

;; `mevedel-bash-policy'
(declare-function mevedel-bash-policy-read-only-p
                  "mevedel-bash-policy" (argv))
(autoload 'mevedel-bash-policy-read-only-p "mevedel-bash-policy")

;; `mevedel-bash-git'
(declare-function mevedel-bash-git-prepare "mevedel-bash-git"
                  (argv &optional temporary-root))
(autoload 'mevedel-bash-git-prepare "mevedel-bash-git")

;; `treesit'
(declare-function treesit-language-available-p "treesit" (language &optional detail))
(declare-function treesit-node-check "treesit" (node property))
(declare-function treesit-node-child "treesit" (node n &optional named))
(declare-function treesit-node-child-count "treesit" (node &optional named))
(declare-function treesit-node-text "treesit" (node &optional no-property))
(declare-function treesit-node-type "treesit" (node))
(declare-function treesit-parser-create "treesit" (language &optional buffer no-reuse tag))
(declare-function treesit-parser-root-node "treesit" (parser))
(autoload 'treesit-language-available-p "treesit")
(autoload 'treesit-node-check "treesit")
(autoload 'treesit-node-child "treesit")
(autoload 'treesit-node-child-count "treesit")
(autoload 'treesit-node-text "treesit")
(autoload 'treesit-node-type "treesit")
(autoload 'treesit-parser-create "treesit")
(autoload 'treesit-parser-root-node "treesit")


;;
;;; Customization

(defcustom mevedel-bash-dangerous-commands
  '("rm" "sudo" "dd" "mkfs" "fdisk" "parted"
    "chmod" "chown" "chgrp" "chattr"
    "kill" "pkill" "killall"
    "curl" "wget" "nc" "ncat" "telnet"
    "ssh" "scp" "rsync" "sftp"
    "iptables" "systemctl" "service"
    "reboot" "shutdown" "poweroff" "halt")
  "Command names classified as dangerous.
This classification is advisory input to permission policy.  Explicit direct
user authority may still permit a matching command."
  :type '(repeat string)
  :group 'mevedel)

(defconst mevedel-bash-analysis--allowed-node-types
  '("program" "list" "pipeline" "command" "command_name" "word"
    "string" "string_content" "raw_string" "number" "concatenation"
    "variable_assignment" "variable_name"
    "test_command" "binary_expression" "unary_expression" "test_operator"
    "&&" "||" ";" "|" "\"" "'" "[" "]" "=" "!=")
  "Tree-sitter Bash node types accepted as plain command syntax.

A `VAR=value cmd' prefix and a single-bracket `[ ... ]' test are plain
commands to the conservative scanner, so the grammar has to read them the same
way -- otherwise installing the grammar reclassifies them as complex and the
two analyzers disagree about the same string.  Double-bracket `[[ ... ]]' is
deliberately absent: it changes quoting and globbing, so it stays complex
alongside negation and pattern operators.")

(defconst mevedel-bash-analysis--treesit-command-node-types
  '("command" "test_command")
  "Tree-sitter Bash node types whose whole text is one command.")

(defconst mevedel-bash-analysis--control-words
  '("case" "coproc" "do" "done" "elif" "else" "esac" "fi" "for"
    "function" "if" "in" "select" "then" "time" "until" "while")
  "Shell control-flow words rejected by the fallback parser.")

(defun mevedel-bash-analysis--split-command (source)
  "Split one SOURCE segment into literal argv using Bash quoting rules.
Unlike shell completion parsing, this reads the whole segment, including
quoted separator characters.  It does not evaluate expansions."
  (let ((index 0) (length (length source)) quote word started args)
    (cl-labels ((finish ()
                  (when started
                    (push (apply #'string (nreverse word)) args)
                    (setq word nil started nil))))
      (while (< index length)
        (let ((char (aref source index)))
          (cond
           ((eq quote ?')
            (if (eq char ?') (setq quote nil) (push char word)))
           ((and quote (eq char ?\")) (setq quote nil))
           ((and (not quote) (memq char '(?\s ?\t ?\n))) (finish))
           ((and (not quote) (memq char '(?' ?\")))
            (setq quote char started t))
           ((eq char ?\\)
            (when (= (1+ index) length) (error "Incomplete Bash escape"))
            (let ((next (aref source (1+ index))))
              (if (and quote (not (memq next '(?$ ?` ?\" ?\\ ?\n))))
                  (push char word)
                (setq index (1+ index))
                (unless (eq next ?\n) (push next word))))
            (setq started t))
           (t (push char word) (setq started t))))
        (setq index (1+ index)))
      (when quote (error "Unterminated Bash quote"))
      (finish)
      (nreverse args))))


;;
;;; Shared facts

(defun mevedel-bash-analysis--command-name (argv)
  "Return the normalized executable name from ARGV."
  (when-let* ((word (car argv)))
    (file-name-nondirectory word)))

(defun mevedel-bash-analysis--dangerous-p (commands source)
  "Return non-nil when COMMANDS or best-effort tokens in SOURCE are dangerous."
  (or (cl-some
       (lambda (argv)
         (member (mevedel-bash-analysis--command-name argv)
                 mevedel-bash-dangerous-commands))
       commands)
      (cl-some
       (lambda (name)
         (string-match-p
          (concat "\\(?:\\`\\|[^[:alnum:]_-]\\)"
                  (regexp-quote name)
                  "\\(?:\\'\\|[^[:alnum:]_-]\\)")
          source))
       mevedel-bash-dangerous-commands)))

(defun mevedel-bash-analysis--path-like-p (word)
  "Return non-nil when WORD is a literal path-like shell token."
  (and (not (string-prefix-p "-" word))
       (not (string-match-p "[$*?`{}]\\|\\[\\|\\]" word))
       (or (member word '("." ".."))
           (string-prefix-p "/" word)
           (string-prefix-p "./" word)
           (string-prefix-p "../" word)
           (string-prefix-p "~/" word)
           (string-match-p "/" word))))

(defun mevedel-bash-analysis--source-resources (source)
  "Return best-effort literal path resources visible in SOURCE.
This intentionally does not evaluate expansions.  Its purpose is to preserve
protected-path checks even when unsupported syntax prevents argv extraction."
  (let (resources)
    (dolist (fragment
             (cons source (mevedel-bash-analysis--substitution-fragments source)))
      (let ((words (condition-case nil
                       (mevedel-bash-analysis--split-command fragment)
                     (error (split-string fragment "[[:space:]]+" t)))))
        (dolist (raw words)
          (let ((word raw))
            (setq word (replace-regexp-in-string
                        "\\`[0-9]*\\(?:<<-?\\|<>\\|>>\\|>\\|<\\)" "" word))
            (setq word (replace-regexp-in-string "\\`[('\\\"]+" "" word))
            (setq word (replace-regexp-in-string "[)'\\\";|&]+\\'" "" word))
            (when (mevedel-bash-analysis--path-like-p word)
              (push word resources))))))
    (delete-dups (nreverse resources))))

(defun mevedel-bash-analysis--resources (commands source)
  "Return literal path-like arguments identified in COMMANDS and SOURCE."
  (let (resources)
    (dolist (argv commands)
      (dolist (word (cdr argv))
        (when (mevedel-bash-analysis--path-like-p word)
          (push word resources))))
    (delete-dups
     (append (nreverse resources)
             (mevedel-bash-analysis--source-resources source)))))

(defun mevedel-bash-analysis--assignment-word-p (word)
  "Return non-nil when WORD begins with a Bash assignment name."
  (and word
       (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\+?=" word)))

(defun mevedel-bash-analysis--normalize-line-continuations (source)
  "Remove Bash line continuations from SOURCE outside single quotes."
  (let ((index 0)
        quote
        escaped
        output)
    (while (< index (length source))
      (let ((char (aref source index))
            (next (and (< (1+ index) (length source))
                       (aref source (1+ index)))))
        (cond
         ((eq quote ?')
          (push char output)
          (when (eq char ?')
            (setq quote nil)))
         (escaped
          (push char output)
          (setq escaped nil))
         ((and (eq char ?\\) (eq next ?\n))
          (setq index (1+ index)))
         ((eq char ?\\)
          (push char output)
          (setq escaped t))
         ((eq quote ?\")
          (push char output)
          (when (eq char ?\")
            (setq quote nil)))
         (t
          (push char output)
          (when (memq char '(?' ?\"))
            (setq quote char)))))
      (setq index (1+ index)))
    (apply #'string (nreverse output))))

(defun mevedel-bash-analysis--substitution-end (source start)
  "Return the end index of command substitution in SOURCE after START.
START points immediately after the opening `$(' token.  Parentheses inside
quotes or escaped with a backslash do not close the substitution."
  (let ((depth 1)
        (index start)
        quote
        escaped)
    (while (and (< index (length source)) (> depth 0))
      (let ((char (aref source index))
            (next (and (< (1+ index) (length source))
                       (aref source (1+ index)))))
        (cond
         (escaped
          (setq escaped nil))
         ((and (eq char ?\\) (not (eq quote ?')))
          (setq escaped t))
         ((eq quote ?')
          (when (eq char ?')
            (setq quote nil)))
         ((eq quote ?`)
          (when (eq char ?`)
            (setq quote nil)))
         ((eq quote ?\")
          (cond
           ((eq char ?\")
            (setq quote nil))
           ((and (eq char ?$) (eq next ?\())
            (when-let* ((nested-end
                         (mevedel-bash-analysis--substitution-end
                          source (+ index 2))))
              (setq index (1- nested-end))))))
         ((and (eq char ?$) (eq next ?\())
          (when-let* ((nested-end
                       (mevedel-bash-analysis--substitution-end
                        source (+ index 2))))
            (setq index (1- nested-end))))
         ((memq char '(?' ?\" ?`))
          (setq quote char))
         ((eq char ?\()
          (setq depth (1+ depth)))
         ((eq char ?\))
          (setq depth (1- depth)))))
      (setq index (1+ index)))
    (and (= depth 0) index)))

(defun mevedel-bash-analysis--substitution-bodies (source &optional heredoc-p shallow-p)
  "Return best-effort command substitution bodies found in SOURCE.
With HEREDOC-P, outer quotes are data rather than shell quoting.
With SHALLOW-P, omit recursively nested bodies so callers can interpret each
body's here-documents before harvesting its substitutions.  When SHALLOW-P
is `spans', return (BODY START END) entries instead of just body strings."
  (let ((index 0)
        quote
        escaped
        bodies)
    (while (< index (length source))
      (let* ((char (aref source index))
             (next-index (1+ index)))
        (while (and (eq char ?$) (< (1+ next-index) (length source))
                    (eq (aref source next-index) ?\\)
                    (eq (aref source (1+ next-index)) ?\n))
          (setq next-index (+ next-index 2)))
        (let ((next (and (< next-index (length source))
                         (aref source next-index)))
              (after-next (and (< (1+ next-index) (length source))
                               (aref source (1+ next-index)))))
          (cond
           (escaped
            (setq escaped nil))
           ((and (eq char ?\\) (not (eq quote ?')))
            (setq escaped t))
           ((and (not heredoc-p) (eq char ?') (not (eq quote ?\")))
            (setq quote (unless (eq quote ?') ?')))
           ((and (not heredoc-p) (eq char ?\") (not (eq quote ?')))
            (setq quote (unless (eq quote ?\") ?\")))
           ((eq quote ?'))
           ((and (eq char ?$) (eq next #x28) (not (eq after-next #x28)))
            (let* ((start (1+ next-index))
                   (end (mevedel-bash-analysis--substitution-end source start)))
              (when end
                (let ((body (substring source start (1- end))))
                  (push (if (eq shallow-p 'spans) (list body start (1- end)) body)
                        bodies))
                (setq index (1- end)))))
           ((eq char ?`)
            (let ((start (1+ index))
                  (search (1+ index))
                  body-escaped
                  end)
              (while (and (< search (length source)) (not end))
                (let ((body-char (aref source search)))
                  (cond
                   (body-escaped (setq body-escaped nil))
                   ((eq body-char ?\\) (setq body-escaped t))
                   ((eq body-char ?`) (setq end search))))
                (setq search (1+ search)))
              (when end
                (let ((body (string-replace "\\`" "`"
                                            (substring source start end))))
                  (push (if (eq shallow-p 'spans) (list body start end) body) bodies))
                (setq index end)))))
          (setq index (1+ index)))))
    (if shallow-p
        (nreverse bodies)
      (let (expanded)
        (dolist (body (nreverse bodies))
          (push body expanded)
          (dolist (nested (mevedel-bash-analysis--substitution-bodies body))
            (push nested expanded)))
        (delete-dups (nreverse expanded))))))

(defun mevedel-bash-analysis--substitution-fragments (source)
  "Return substitution bodies and their command-chain segments from SOURCE."
  (let ((bodies (mevedel-bash-analysis--substitution-bodies source)))
    (append
     bodies
     (cl-mapcan
      (lambda (body)
        (car (mevedel-bash-analysis--scan-segments body)))
      bodies))))

(defun mevedel-bash-analysis--candidate-from-argv (argv)
  "Return a deny-policy candidate string from ARGV, or nil."
  (when argv
    (let ((normalized (copy-sequence argv)))
      (setcar normalized (file-name-nondirectory (car normalized)))
      (string-join normalized " "))))

(defun mevedel-bash-analysis--candidate-command (fragment)
  "Return a normalized possible command from FRAGMENT, or nil."
  (let ((candidate (string-trim fragment)))
    (setq candidate (replace-regexp-in-string "\\`[({[:space:]]+" "" candidate))
    (setq candidate (replace-regexp-in-string "[)}[:space:]]+\\'" "" candidate))
    (let ((argv (condition-case nil
                    (mevedel-bash-analysis--split-command candidate)
                  (error nil))))
      (while (and argv
                  (mevedel-bash-analysis--assignment-word-p (car argv)))
        (setq argv (cdr argv)))
      (when (member (car argv) '("then" "do" "else" "elif"))
        (setq argv (cdr argv)))
      (while (member (car argv) '("!" "time"))
        (let ((prefix (pop argv)))
          (when (and (equal prefix "time")
                     (equal (car argv) "-p"))
            (setq argv (cdr argv)))))
      (mevedel-bash-analysis--candidate-from-argv argv))))

(defun mevedel-bash-analysis--candidates (source segments commands)
  "Return best-effort command forms from SOURCE, SEGMENTS, and COMMANDS."
  (let* ((fragments
          (append segments
                  (mevedel-bash-analysis--substitution-fragments source)))
         candidates)
    (dolist (fragment fragments)
      (when-let* ((candidate
                   (mevedel-bash-analysis--candidate-command fragment)))
        (push candidate candidates)))
    (dolist (argv commands)
      (push (mevedel-bash-analysis--candidate-from-argv argv) candidates))
    (delete-dups (delq nil (nreverse candidates)))))

(defun mevedel-bash-analysis--classify (commands complex-p source)
  "Classify COMMANDS and COMPLEX-P for SOURCE."
  (cond
   ((mevedel-bash-analysis--dangerous-p commands source) 'dangerous)
   (complex-p 'complex)
   ((and commands
         (cl-every #'mevedel-bash-policy-read-only-p commands))
    'read-only)
   (t 'unknown)))

(defun mevedel-bash-analysis--result
    (source parser segments commands complex-p reasons)
  "Build normalized analysis for SOURCE from PARSER, SEGMENTS, and COMMANDS."
  (let* ((class (mevedel-bash-analysis--classify commands complex-p source))
         (background-p (mevedel-bash-analysis--background-p source)))
    (list :class class
          :complex-p (and complex-p t)
          :background-p (and background-p t)
          :commands commands
          :segments segments
          :candidates (mevedel-bash-analysis--candidates
                       source segments commands)
          :parser parser
          :resources (mevedel-bash-analysis--resources commands source)
          :reasons (or reasons
                       (list (pcase class
                               ('read-only "All commands have read-only policies")
                               ('dangerous "A dangerous command was identified")
                               ('complex "The shell syntax is not supported")
                               (_ "No read-only command policy matched")))))))


;;
;;; Conservative scanner

(defun mevedel-bash-analysis--heredoc-context (source)
  "Return (SHELL BODIES) separating here-document data from SOURCE.
SHELL preserves headers and replaces bodies and terminators with spaces.
BODIES contains only expanding bodies with escaped newlines joined; quoted
delimiters disable expansion and preserve physical lines.
This separation is only for detecting background work, not argv or authority."
  (let ((shell (copy-sequence source))
        (index 0) quote escaped pending bodies)
    (while (< index (length source))
      (let ((char (aref source index)))
        (cond
         (escaped (setq escaped nil))
         ((and (eq char ?\\) (not (eq quote ?'))) (setq escaped t))
         ((eq quote ?') (when (eq char ?') (setq quote nil)))
         ((eq char ?\") (setq quote (unless quote ?\")))
         (quote)
         ((eq char ?') (setq quote ?'))
         ((or (and (eq char ?$) (string-prefix-p "((" (substring source (1+ index))))
              (and (eq char ?\() (string-prefix-p "(" (substring source (1+ index)))))
          ;; Arithmetic shifts are not here-document redirects.  Nested
          ;; command substitutions are inspected separately by the caller.
          (when-let* ((end (mevedel-bash-analysis--substitution-end
                           source (+ index (if (eq char ?$) 2 1)))))
            (setq index (1- end))))
         ((and (eq char ?#)
               (or (= index 0) (memq (aref source (1- index)) '(?\s ?\t ?\n))))
          (setq index (or (string-search "\n" source index) (length source)))
          ;; Process the newline below, including any queued here-documents.
          (setq index (1- index)))
         ((and (eq char ?<)
               (< (1+ index) (length source))
               (eq (aref source (1+ index)) ?<)
               (not (and (> index 0) (eq (aref source (1- index)) ?<)))
               (not (and (< (+ index 2) (length source))
                         (eq (aref source (+ index 2)) ?<))))
          (let* ((start (+ index 2))
                 (tabs (and (< start (length source))
                            (eq (aref source start) ?-)))
                 word-quote word-escaped quoted unsupported)
            (when tabs (setq start (1+ start)))
            (while (and (< start (length source))
                        (memq (aref source start) '(?\s ?\t)))
              (setq start (1+ start)))
            (setq index start)
            (while (and (< index (length source))
                        (or word-quote word-escaped
                            (not (memq (aref source index)
                                       '(?\s ?\t ?\n ?\; ?& ?| ?< ?> ?\( ?\))))))
              (let ((word-char (aref source index)))
                (cond
                 (word-escaped (setq word-escaped nil))
                 ((and (eq word-char ?\\) (not (eq word-quote ?')))
                  (setq word-escaped t quoted t))
                 (word-quote
                  (when (eq word-char word-quote) (setq word-quote nil)))
                 ((memq word-char '(?' ?\"))
                  ;; ANSI-C and locale quoting need expansion semantics, not
                  ;; ordinary quote removal.  Leave them conservatively raw.
                  (when (and (> index start) (eq (aref source (1- index)) ?$))
                    (setq unsupported t))
                  (setq word-quote word-char quoted t))))
              (setq index (1+ index)))
            (when (and (> index start) (not word-quote) (not word-escaped)
                       (not unsupported))
              (push (list (car (mevedel-bash-analysis--split-command
                               (substring source start index))) tabs quoted)
                    pending))
            (setq index (1- index))))
         ((and (eq char ?\n) pending)
          (setq index (1+ index))
          (dolist (document (nreverse pending))
            (pcase-let ((`(,delimiter ,tabs ,quoted) document)
                        (start index)
                        (body nil) (done nil))
              (while (and (< index (length source)) (not done))
                (let* ((line-end (or (string-search "\n" source index) (length source)))
                       (line (substring source index line-end))
                       (joining (not quoted)))
                  ;; In expanding bodies, escaped newlines are removed before
                  ;; delimiter recognition.  Literal quote characters do not
                  ;; change that rule; an even run of backslashes does.
                  (while (and joining (< line-end (length source)))
                    (let ((position (1- (length line))) (slashes 0))
                      (while (and (>= position 0) (eq (aref line position) ?\\))
                        (setq position (1- position) slashes (1+ slashes)))
                      (setq joining (= (% slashes 2) 1))
                      (when joining
                        (let* ((next-start (1+ line-end))
                               (next-end (or (string-search "\n" source next-start)
                                             (length source))))
                          (setq line (concat (substring line 0 -1)
                                             (substring source next-start next-end))
                                line-end next-end)))))
                  (when (equal delimiter (if tabs (string-trim-left line "\t+") line))
                    (setq done t))
                  (unless done
                    (push (concat line (if (< line-end (length source)) "\n" "")) body))
                  (setq index (min (length source) (1+ line-end)))))
              (unless quoted (push (apply #'concat (nreverse body)) bodies))
              (cl-loop for position from start below index
                       do (aset shell position ?\s))))
          (setq pending nil index (1- index)))))
      (setq index (1+ index)))
    (list shell (nreverse bodies))))

(defun mevedel-bash-analysis--background-p (source)
  "Return non-nil when SOURCE contains executable shell background syntax.
Here-document data is not shell syntax.  Command substitutions still execute
in unquoted here-documents, including inside literal quote characters."
  (let ((pending (list source)) seen found)
    (while (and pending (not found))
      (let ((fragment (pop pending)))
        ;; Inspect each distinct body once, after separating its here-documents.
        (unless (member fragment seen)
          (push fragment seen)
          (pcase-let ((`(,shell ,bodies)
                       (mevedel-bash-analysis--heredoc-context fragment)))
            (let ((substitutions
                   (mevedel-bash-analysis--substitution-bodies shell nil 'spans)))
              ;; A nested substitution owns its own quoting and here-documents.
              ;; Hide it before normalizing the outer shell's continuations.
              (dolist (substitution substitutions)
                (cl-loop for position from (nth 1 substitution) below (nth 2 substitution)
                         do (aset shell position ?\s)))
              (setq shell (mevedel-bash-analysis--normalize-line-continuations shell))
              (setq found
                    (member "Background execution is unsupported"
                            (cadr (mevedel-bash-analysis--scan-segments shell))))
              (setq pending
                    (append
                     (mapcar #'car substitutions)
                     (cl-mapcan
                      (lambda (body)
                        ;; Quotes in expanding here-documents are data.
                        (mevedel-bash-analysis--substitution-bodies body t t))
                      bodies)
                     pending)))))))
    found))

(defun mevedel-bash-analysis--scan-segments (source &optional spans-p)
  "Return (SEGMENTS REASONS) from SOURCE using the supported shell subset.
With SPANS-P, append source (START . END) spans for each command."
  (let ((segments nil)
        (current nil)
        (quote nil)
        (escaped nil)
        (reasons nil)
        (index 0)
        (start 0)
        (spans nil)
        (length (length source)))
    (cl-labels ((finish-segment
                 ()
                 (let* ((raw (apply #'string (nreverse current)))
                        (segment (string-trim raw))
                        (candidate
                         (mevedel-bash-analysis--candidate-command segment)))
                   (when (and candidate
                              (string-match-p
                               "\\`coproc\\(?:[[:space:]]\\|\\'\\)"
                               candidate))
                     (push "Background execution is unsupported" reasons))
                   (if (string-empty-p segment)
                       (push "A command separator has an empty operand" reasons)
                     (push segment segments)
                     (push (cons (+ start (- (length raw)
                                             (length (string-trim-left raw))))
                                 (- index (- (length raw)
                                             (length (string-trim-right raw)))))
                           spans))
                   (setq current nil))))
      (while (< index length)
        (let* ((char (aref source index))
               (previous (and (> index 0) (aref source (1- index))))
               (next (and (< (1+ index) length)
                          (aref source (1+ index)))))
          (cond
           (escaped
            (push char current)
            (setq escaped nil))
           ((and (eq char ?\\) (not (eq quote ?')))
            (push char current)
            (setq escaped t))
           ((and (eq char ?') (not (eq quote ?\")))
            (push char current)
            (setq quote (unless (eq quote ?') ?')))
           ((and (eq char ?\") (not (eq quote ?')))
            (push char current)
            (setq quote (unless (eq quote ?\") ?\")))
           (quote
            (when (and (eq quote ?\") (memq char '(?$ ?`)))
              (push "Expansion or substitution is unsupported" reasons))
            (push char current))
           ((or (and (eq char ?$) (eq next #x28)
                     (< (+ index 2) length)
                     (eq (aref source (+ index 2)) #x28))
                (and (eq char #x28) (eq next #x28)))
            (let ((start (if (eq char ?$) (+ index 2) (1+ index))))
              (push "Expansion or substitution is unsupported" reasons)
              (if-let* ((end
                         (mevedel-bash-analysis--substitution-end
                          source start)))
                  (progn
                    (dotimes (offset (- end index))
                      (push (aref source (+ index offset)) current))
                    (setq index (1- end)))
                (push char current))))
           ((and (eq char ?\[)
                 (or (null next) (memq next '(?\s ?\t)))
                 (string-match-p
                  "\\`[[:space:]]*\\'"
                  (apply #'string (reverse current))))
            (push char current))
           ((and (eq char ?\])
                 (memq previous '(?\s ?\t))
                 (let ((remaining
                        (string-trim-left (substring source (1+ index)))))
                   (or (string-empty-p remaining)
                       (string-prefix-p "&&" remaining)
                       (string-prefix-p "||" remaining)
                       (memq (aref remaining 0) '(?\; ?\| ?\n)))))
            (push char current))
           ((memq char '(?$ ?` ?< ?> ?\( ?\) ?{ ?} ?* ?? ?\[ ?\] ?!))
            (push "Expansion, substitution, grouping, or redirection is unsupported"
                  reasons)
            (push char current))
           ((eq char ?\n)
            (push "Newline command separation is unsupported" reasons)
            (finish-segment)
            (setq start (1+ index)))
           ((and (eq char ?&) (eq next ?&))
            (finish-segment)
            (setq start (+ index 2))
            (setq index (1+ index)))
           ((and (eq char ?&)
                 (or (memq previous '(?> ?< ?|))
                     (eq next ?>)))
            (push char current))
           ((eq char ?&)
            (push "Background execution is unsupported" reasons)
            (push char current))
           ((and (eq char ?|) (eq next ?|))
            (finish-segment)
            (setq start (+ index 2))
            (setq index (1+ index)))
           ((eq char ?|)
            (finish-segment)
            (setq start (1+ index)))
           ((eq char ?\;)
            (finish-segment)
            (setq start (1+ index)))
           ((and (eq char ?#)
                 (or (null current) (memq previous '(?\s ?\t))))
            (push "Shell comments are unsupported" reasons)
            (push char current))
           (t
            (push char current))))
        (setq index (1+ index)))
      (when quote
        (push "The command contains an unterminated quote" reasons))
      (when escaped
        (push "The command ends with an incomplete escape" reasons))
      (if current
          (finish-segment)
        (when (and segments
                   (string-match-p "\\(?:&&\\|||\\|[;|]\\)\\s-*\\'" source))
          (push "A command separator has an empty operand" reasons)))
      (append (list (nreverse segments) (delete-dups (nreverse reasons)))
              (when spans-p (list (nreverse spans)))))))

(defun mevedel-bash-analysis--argv (segments)
  "Return (COMMANDS REASONS) parsed from SEGMENTS."
  (let (commands reasons)
    (dolist (segment segments)
      (condition-case nil
          (let ((argv (mevedel-bash-analysis--split-command segment)))
            (cond
             ((null argv)
              (push "A command is empty" reasons))
             ((mevedel-bash-analysis--assignment-word-p (car argv))
              (push "Environment assignments are unsupported" reasons))
             ((member (car argv) mevedel-bash-analysis--control-words)
              (push "Shell control flow is unsupported" reasons))
             (t
              (push argv commands))))
        (error
         (push "The command could not be split into arguments" reasons))))
    (list (nreverse commands) (nreverse reasons))))

(defun mevedel-bash-analysis--heuristic (source)
  "Analyze SOURCE with the conservative scanner."
  (pcase-let* ((`(,segments ,scan-reasons)
                (mevedel-bash-analysis--scan-segments source))
               (`(,commands ,argv-reasons)
                (mevedel-bash-analysis--argv segments))
               (reasons (append scan-reasons argv-reasons)))
    (mevedel-bash-analysis--result
     source 'heuristic segments commands (not (null reasons)) reasons)))


;;
;;; Tree-sitter adapter

(defun mevedel-bash-analysis--treesit-supported-p (node)
  "Return non-nil when NODE contains only the supported Bash subset."
  (and (not (treesit-node-check node 'has-error))
       (member (treesit-node-type node)
               mevedel-bash-analysis--allowed-node-types)
       (let ((supported t)
             (index 0)
             (count (treesit-node-child-count node)))
         (while (and supported (< index count))
           (setq supported
                 (mevedel-bash-analysis--treesit-supported-p
                  (treesit-node-child node index)))
           (setq index (1+ index)))
         supported)))

(defun mevedel-bash-analysis--treesit-command-texts (node)
  "Return command source strings below NODE in source order.

A `test_command' is a command here too: it is one invocation with its own exit
status, and leaving it out would make `[ 1 = 2 ]' analyze as a program with no
commands at all."
  (if (member (treesit-node-type node)
              mevedel-bash-analysis--treesit-command-node-types)
      (list (treesit-node-text node t))
    (let (texts)
      (dotimes (index (treesit-node-child-count node))
        (setq texts
              (append texts
                      (mevedel-bash-analysis--treesit-command-texts
                       (treesit-node-child node index)))))
      texts)))

(defun mevedel-bash-analysis--treesit (source)
  "Analyze SOURCE with the configured Bash Tree-sitter grammar.

The conservative scanner runs alongside the grammar and keeps ownership of
the segments whenever the grammar rejects the program.  Discarding them
would leave `:segments', `:candidates', and `:resources' empty for every
unsupported construct, and those are the surfaces protected-path checks
and explicit deny rules match against: an environment assignment in front
of a denied command would otherwise present nothing to match.  Even supported
nodes must retain every scanner segment: a standalone assignment is not a
command node and must not disappear before read-only classification."
  (with-temp-buffer
    (insert source)
    (pcase-let* ((`(,scan-segments ,scan-reasons)
                  (mevedel-bash-analysis--scan-segments source))
                 (parser (treesit-parser-create 'bash))
                 (root (treesit-parser-root-node parser))
                 (grammar-segments
                  (mevedel-bash-analysis--treesit-command-texts root))
                 (supported
                  (and (null scan-reasons)
                       (mevedel-bash-analysis--treesit-supported-p root)
                       (equal scan-segments grammar-segments)))
                 (segments
                  (if supported grammar-segments scan-segments))
                 (`(,commands ,argv-reasons)
                  (mevedel-bash-analysis--argv segments))
                 (reasons (append
                           (unless supported
                             '("Tree-sitter found unsupported or invalid shell syntax"))
                           argv-reasons)))
      (mevedel-bash-analysis--result
       source 'treesit segments commands
       (or (not supported) (not (null reasons))) reasons))))


;;
;;; Public interface

(defun mevedel-bash-analysis--quote-word (word)
  "Quote literal WORD for Bash, independent of the editor's host shell."
  (concat "'" (replace-regexp-in-string "'" "'\\''" word t t) "'"))

(defun mevedel-bash-analysis-prepare-read-only (source analysis &optional temporary-root)
  "Prepare read-only SOURCE for launch using its original ANALYSIS.
Git and ripgrep command spans use hardened, shell-quoted argv.
TEMPORARY-ROOT supplies the execution target's authorized native temp path.
Connectors and other command source stay intact.  Fail closed if the scanner
and admitted analysis disagree; never execute unhardened Git on failure."
  (unless (eq (plist-get analysis :class) 'read-only)
    (error "Cannot prepare a command without read-only analysis"))
  (setq source (mevedel-bash-analysis--normalize-line-continuations source))
  (pcase-let* ((`(,segments ,reasons ,spans)
                (mevedel-bash-analysis--scan-segments source t))
               (`(,commands ,argv-reasons)
                (mevedel-bash-analysis--argv segments)))
    (unless (and (null reasons) (null argv-reasons)
                 (equal commands (plist-get analysis :commands)))
      (error "Read-only launch preparation disagrees with Bash analysis"))
    (let ((offset 0) pieces)
      (cl-mapc
       (lambda (argv span)
         (push (substring source offset (car span)) pieces)
         (push (cond
                ((equal (car argv) "git")
                 (mapconcat #'mevedel-bash-analysis--quote-word
                            (mevedel-bash-git-prepare argv temporary-root) " "))
                ((member (car argv) '("rg" "ripgrep"))
                 ;; Config can inject --pre or other helper-enabling options
                 ;; not present in the authored argv inspected by policy.
                 (mapconcat #'mevedel-bash-analysis--quote-word
                            (append (list (car argv) "--no-config") (cdr argv)) " "))
                (t (substring source (car span) (cdr span))))
               pieces)
         (setq offset (cdr span)))
       commands spans)
      (push (substring source offset) pieces)
      (apply #'concat (nreverse pieces)))))

;;;###autoload
(defun mevedel-bash-analysis-analyze (source)
  "Return normalized conservative Bash analysis for SOURCE.
The result contains `:class', `:commands', `:parser', `:resources', and
`:reasons'.  Tree-sitter is selected only through normal Emacs grammar
  configuration; no grammar path is added here."
  (let* ((original source)
         (source (mevedel-bash-analysis--normalize-line-continuations source))
         (analysis
          (if (and (fboundp 'treesit-language-available-p)
                   (treesit-language-available-p 'bash))
              (condition-case _err
                  (mevedel-bash-analysis--treesit source)
                (error
                 (let ((result (mevedel-bash-analysis--heuristic source)))
                   (plist-put result :reasons
                              (cons "Tree-sitter analysis failed; used conservative fallback"
                                    (plist-get result :reasons))))))
            (mevedel-bash-analysis--heuristic source))))
    ;; Global continuation normalization does not understand here-documents.
    ;; Their background proof must retain original quoted-body bytes.
    (unless (equal original source)
      (plist-put analysis :background-p
                 (and (mevedel-bash-analysis--background-p original) t)))
    analysis))

(defun mevedel-bash-analysis--snapshot-expansions-safe-p (source)
  "Return non-nil when SOURCE contains only plain parameter expansions.
Even quoted or escaped complex expansions are declined.  Arithmetic, array
subscripts, indirect expansion and parameter operators can execute code."
  (let ((index 0)
        (safe t)
        (case-fold-search nil)
        (parameter "[a-zA-Z_][a-zA-Z_0-9]*"))
    (while (and safe (< index (length source)))
      (let ((char (aref source index)))
        (cond
         ((eq char ?$)
          (if (and (string-match
                    (concat "\\$\\(?:{\\(" parameter "\\)}\\|\\("
                            parameter "\\)\\|\\([@#?]\\)\\)")
                    source index)
                   (= (match-beginning 0) index)
                   (if-let* ((name (or (match-string 1 source) (match-string 2 source))))
                       (and (string-match-p "\\`[a-z_][a-z0-9_]*\\'" name)
                            (not (equal name "_")))
                     (or (not (equal (match-string 3 source) "@"))
                         (and (> index 0) (eq (aref source (1- index)) ?\")
                              (< (match-end 0) (length source))
                              (eq (aref source (match-end 0)) ?\")))))
              (setq index (1- (match-end 0)))
            (setq safe nil)))
         ((memq char '(?` ?{ ?} ?\[ ?\])) (setq safe nil))
         ((and (memq char '(?< ?>)) (< (1+ index) (length source))
               (memq (aref source (1+ index)) '(?\( ?<)))
          (setq safe nil))
         ((and (eq char ?\() (< (1+ index) (length source))
               (eq (aref source (1+ index)) ?\())
          (setq safe nil))))
      (setq index (1+ index)))
    safe))

(defun mevedel-bash-analysis--snapshot-command-safe-p (segment)
  "Return non-nil when SEGMENT cannot visibly run code in the caller shell.
Support plain for loops, but require literal executable names.  Decline
builtins that evaluate code, callbacks or variable subscripts."
  (condition-case nil
      (let* ((case-fold-search nil)
             (text (replace-regexp-in-string "\\`[([:space:]]+" "" segment))
             (text (replace-regexp-in-string "[)[:space:]]+\\'" "" text))
             (argv (mevedel-bash-analysis--split-command text)))
        (cond
         ((equal (car argv) "for")
          (and (string-match-p "\\`[a-z_][a-z0-9_]*\\'" (or (cadr argv) ""))
               (not (equal (cadr argv) "_"))
               (or (null (cddr argv)) (equal (nth 2 argv) "in"))))
         ((equal (car argv) "done") (null (cdr argv)))
         (t
          (when (equal (car argv) "do") (pop argv))
          (let ((name (car argv)))
            (and name
                 (not (cl-some (lambda (char)
                                 (memq char '(?$ ?` ?{ ?} ?< ?> ?* ?? ?\[ ?\] ?\( ?\))))
                               (string-to-list name)))
                 (not (mevedel-bash-analysis--assignment-word-p name))
                 (not (member name mevedel-bash-analysis--control-words))
                 (not (member name
                              '("!" "time" "trap" "eval" "exec" "source" "."
                                "builtin" "command" "alias" "shopt" "mapfile"
                                "readarray" "enable" "bind" "let" "declare"
                                "typeset" "local" "readonly" "unset" "read"
                                "getopts" "wait" "fc" "compgen" "complete" "test" "[")))
                 ;; These builtins can interpret arguments as variable names,
                 ;; including array subscripts evaluated in the caller shell.
                 (not (and (equal name "export")
                           (string-search "$" text)))
                 (or (not (equal name "printf"))
                     (let ((format (cadr argv)))
                       (and format
                            (not (string-prefix-p "-" format))
                            (not (string-match-p "%[-+ #0-9.*hlLqjzt]*n\\|[$]" format))))))))))
    (error nil)))

(defun mevedel-bash-analysis-snapshot-safe-p (source)
  "Return non-nil when SOURCE cannot visibly install a caller EXIT trap.
Plain parameter arguments and for loops are supported, not dynamic executable
names, shell code or function definitions.  Startup-file and inherited-function
traps or namerefs are outside this source proof."
  (setq source (mevedel-bash-analysis--normalize-line-continuations source))
  (pcase-let ((`(,segments ,reasons ,spans)
               (mevedel-bash-analysis--scan-segments source t)))
    (and (mevedel-bash-analysis--snapshot-expansions-safe-p source)
         (cl-every
          (lambda (reason)
            (member reason
                    '("Expansion, substitution, grouping, or redirection is unsupported"
                      "Expansion or substitution is unsupported")))
          reasons)
         (mevedel-bash-analysis--snapshot-command-spans source segments spans)
         (cl-every #'mevedel-bash-analysis--snapshot-command-safe-p segments))))

(defun mevedel-bash-analysis--snapshot-command-spans (source segments spans)
  "Return SPANS with plain for loops in SOURCE grouped as compound stages.
SEGMENTS correspond to SPANS.  Return nil for incomplete or nested loops,
other control flow, or pipelines inside a loop body."
  (let (result loop-start state previous invalid)
    (while (and segments (not invalid))
      (let* ((segment (pop segments))
             (span (pop spans))
             (head (car (mevedel-bash-analysis--split-command segment))))
        (when (and state previous
                   (not (member (string-trim
                                 (substring source (cdr previous) (car span)))
                                '(";" "&&" "||"))))
          (setq invalid t))
        (cond
         ((equal head "for")
          (if state (setq invalid t)
            (setq loop-start (car span) state 'do)))
         ((equal head "do")
          (if (eq state 'do) (setq state 'body) (setq invalid t)))
         ((equal head "done")
          (if (eq state 'body)
              (progn (push (cons loop-start (cdr span)) result)
                     (setq state nil loop-start nil))
            (setq invalid t)))
         ((or (eq state 'do)
              (member head mevedel-bash-analysis--control-words))
          (setq invalid t))
         ((null state) (push span result)))
        (setq previous span)))
    (unless (or invalid state) (nreverse result))))

(defun mevedel-bash-analysis-final-pipeline (source count)
  "Return stage analyses for SOURCE's sole final COUNT-stage outer pipeline.
Return nil when another pipeline could have supplied the observed statuses,
as in conditional lists.  Quoted pipe characters are not connectors."
  (pcase-let* ((`(,segments ,_reasons ,spans)
                (mevedel-bash-analysis--scan-segments
                 (mevedel-bash-analysis--normalize-line-continuations source) t))
               (source (mevedel-bash-analysis--normalize-line-continuations source))
               (spans (mevedel-bash-analysis--snapshot-command-spans source segments spans))
               (segments (mapcar (lambda (span) (substring source (car span) (cdr span)))
                                 spans))
               (pipes 0)
               (trailing 0))
    (while (cdr spans)
      (let ((connector (string-trim
                        (substring source (cdar spans) (caadr spans)))))
        (if (member connector '("|" "|&"))
            (setq pipes (1+ pipes) trailing (1+ trailing))
          (setq trailing 0)))
      (setq spans (cdr spans)))
    (when (and (> count 1) (= pipes (1- count)) (= trailing pipes))
      (mapcar #'mevedel-bash-analysis-analyze (last segments count)))))

(provide 'mevedel-bash-analysis)

;;; mevedel-bash-analysis.el ends here
