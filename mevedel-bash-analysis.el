;;; mevedel-bash-analysis.el --- Conservative Bash analysis -*- lexical-binding: t -*-

;;; Commentary:

;; Produces normalized facts for Bash permission decisions.  A configured
;; Bash Tree-sitter grammar supplies structured parsing when available; a
;; deliberately small scanner provides the same conservative contract when it
;; is not.  This module classifies syntax and commands but grants no authority.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; `treesit'
(declare-function treesit-language-available-p "treesit" (language &optional detail))
(declare-function treesit-node-check "treesit" (node property))
(declare-function treesit-node-child "treesit" (node n &optional named))
(declare-function treesit-node-child-count "treesit" (node &optional named))
(declare-function treesit-node-text "treesit" (node &optional no-property))
(declare-function treesit-node-type "treesit" (node))
(declare-function treesit-parser-create "treesit" (language &optional buffer no-reuse tag))
(declare-function treesit-parser-root-node "treesit" (parser))


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

(defconst mevedel-bash-analysis--read-only-commands
  '("cat" "cd" "cut" "echo" "expr" "false" "grep" "head" "id" "ls"
    "nl" "paste" "pwd" "rev" "seq" "stat" "tail" "tr" "true" "uname"
    "uniq" "wc" "which" "whoami")
  "Commands whose ordinary argument handling is read-only.
Commands with effectful options belong in argument-specific policies rather
than this unconditional list.")

(defconst mevedel-bash-analysis--allowed-node-types
  '("program" "list" "pipeline" "command" "command_name" "word"
    "string" "string_content" "raw_string" "number" "concatenation"
    "&&" "||" ";" "|" "\"" "'")
  "Tree-sitter Bash node types accepted as plain command syntax.")

(defconst mevedel-bash-analysis--control-words
  '("case" "coproc" "do" "done" "elif" "else" "esac" "fi" "for"
    "function" "if" "in" "select" "then" "time" "until" "while")
  "Shell control-flow words rejected by the fallback parser.")


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
       (or (string-prefix-p "/" word)
           (string-prefix-p "./" word)
           (string-prefix-p "../" word)
           (string-prefix-p "~/" word)
           (string-match-p "/" word))))

(defun mevedel-bash-analysis--source-resources (source)
  "Return best-effort literal path resources visible in SOURCE.
This intentionally does not evaluate expansions.  Its purpose is to preserve
protected-path checks even when unsupported syntax prevents argv extraction."
  (require 'shell)
  (let ((words (condition-case nil
                   (split-string-shell-command source)
                 (error (split-string source "[[:space:]]+" t))))
        resources)
    (dolist (raw words)
      (let ((word raw))
        (setq word (replace-regexp-in-string
                    "\\`[0-9]*\\(?:<<-?\\|<>\\|>>\\|>\\|<\\)" "" word))
        (setq word (replace-regexp-in-string "\\`[('\\\"]+" "" word))
        (setq word (replace-regexp-in-string "[)'\\\";|&]+\\'" "" word))
        (when (mevedel-bash-analysis--path-like-p word)
          (push word resources))))
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

(defun mevedel-bash-analysis--substitution-bodies (source)
  "Return best-effort command substitution bodies found in SOURCE."
  (let ((position 0)
        bodies)
    (while (string-match "\\$(" source position)
      (let ((depth 1)
            (index (match-end 0)))
        (while (and (< index (length source)) (> depth 0))
          (pcase (aref source index)
            (?\( (setq depth (1+ depth)))
            (?\) (setq depth (1- depth))))
          (setq index (1+ index)))
        (when (= depth 0)
          (push (substring source (match-end 0) (1- index)) bodies))
        (setq position index)))
    (setq position 0)
    (while (string-match "`\\([^`]*\\)`" source position)
      (push (match-string 1 source) bodies)
      (setq position (match-end 0)))
    (let (expanded)
      (dolist (body (nreverse bodies))
        (push body expanded)
        (dolist (nested (mevedel-bash-analysis--substitution-bodies body))
          (push nested expanded)))
      (delete-dups (nreverse expanded)))))

(defun mevedel-bash-analysis--candidate-command (fragment)
  "Return a normalized possible command from FRAGMENT, or nil."
  (let ((candidate (string-trim fragment)))
    (setq candidate (replace-regexp-in-string "\\`[({[:space:]]+" "" candidate))
    (setq candidate (replace-regexp-in-string "[)}[:space:]]+\\'" "" candidate))
    (while (string-match
            "\\`[A-Za-z_][A-Za-z0-9_]*=[^[:space:]]+[[:space:]]+"
            candidate)
      (setq candidate (substring candidate (match-end 0))))
    (when (string-match
           "\\`\\(?:then\\|do\\|else\\|elif\\)[[:space:]]+" candidate)
      (setq candidate (substring candidate (match-end 0))))
    (unless (string-empty-p candidate)
      candidate)))

(defun mevedel-bash-analysis--candidates (source segments commands)
  "Return best-effort command forms from SOURCE, SEGMENTS, and COMMANDS."
  (let ((fragments (append segments
                           (mevedel-bash-analysis--substitution-bodies source)))
        candidates)
    (dolist (fragment fragments)
      (when-let* ((candidate
                   (mevedel-bash-analysis--candidate-command fragment)))
        (push candidate candidates)))
    (dolist (argv commands)
      (push (string-join argv " ") candidates))
    (delete-dups (nreverse candidates))))

(defun mevedel-bash-analysis--classify (commands complex-p source)
  "Classify COMMANDS and COMPLEX-P for SOURCE."
  (cond
   ((mevedel-bash-analysis--dangerous-p commands source) 'dangerous)
   (complex-p 'complex)
   ((and commands
         (cl-every
          (lambda (argv)
            (let ((executable (car argv)))
              (and (not (string-match-p "/" executable))
                   (member executable
                           mevedel-bash-analysis--read-only-commands))))
          commands))
    'read-only)
   (t 'unknown)))

(defun mevedel-bash-analysis--result
    (source parser segments commands complex-p reasons)
  "Build normalized analysis for SOURCE from PARSER, SEGMENTS, and COMMANDS."
  (let ((class (mevedel-bash-analysis--classify commands complex-p source)))
    (list :class class
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

(defun mevedel-bash-analysis--scan-segments (source)
  "Return (SEGMENTS REASONS) from SOURCE using the supported shell subset."
  (let ((segments nil)
        (current nil)
        (quote nil)
        (escaped nil)
        (reasons nil)
        (index 0)
        (length (length source)))
    (cl-labels ((finish-segment
                 ()
                 (let ((segment (string-trim (apply #'string (nreverse current)))))
                   (if (string-empty-p segment)
                       (push "A command separator has an empty operand" reasons)
                     (push segment segments))
                   (setq current nil))))
      (while (< index length)
        (let* ((char (aref source index))
               (next (and (< (1+ index) length)
                          (aref source (1+ index)))))
          (cond
           (escaped
            (push char current)
            (setq escaped nil))
           ((eq char ?\\)
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
           ((memq char '(?$ ?` ?< ?> ?\( ?\) ?{ ?} ?* ?? ?\[ ?\] ?!))
            (push "Expansion, substitution, grouping, or redirection is unsupported"
                  reasons)
            (push char current))
           ((eq char ?\n)
            (push "Newline command separation is unsupported" reasons)
            (finish-segment))
           ((and (eq char ?&) (eq next ?&))
            (finish-segment)
            (setq index (1+ index)))
           ((eq char ?&)
            (push "Background execution is unsupported" reasons)
            (push char current))
           ((and (eq char ?|) (eq next ?|))
            (finish-segment)
            (setq index (1+ index)))
           ((eq char ?|)
            (finish-segment))
           ((eq char ?\;)
            (finish-segment))
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
      (list (nreverse segments) (delete-dups (nreverse reasons))))))

(defun mevedel-bash-analysis--argv (segments)
  "Return (COMMANDS REASONS) parsed from SEGMENTS."
  (require 'shell)
  (let (commands reasons)
    (dolist (segment segments)
      (condition-case nil
          (let ((argv (split-string-shell-command segment)))
            (cond
             ((null argv)
              (push "A command is empty" reasons))
             ((string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" (car argv))
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
  "Return command source strings below NODE in source order."
  (if (string-equal (treesit-node-type node) "command")
      (list (treesit-node-text node t))
    (let (texts)
      (dotimes (index (treesit-node-child-count node))
        (setq texts
              (append texts
                      (mevedel-bash-analysis--treesit-command-texts
                       (treesit-node-child node index)))))
      texts)))

(defun mevedel-bash-analysis--treesit (source)
  "Analyze SOURCE with the configured Bash Tree-sitter grammar."
  (require 'treesit)
  (with-temp-buffer
    (insert source)
    (let* ((scan (mevedel-bash-analysis--scan-segments source))
           (newline-p
            (member "Newline command separation is unsupported" (cadr scan)))
           (parser (treesit-parser-create 'bash))
           (root (treesit-parser-root-node parser))
           (supported (and (not newline-p)
                           (mevedel-bash-analysis--treesit-supported-p root)))
           (texts (and supported
                       (mevedel-bash-analysis--treesit-command-texts root)))
           (parsed (mevedel-bash-analysis--argv texts))
           (commands (car parsed))
           (reasons (append
                     (unless supported
                       '("Tree-sitter found unsupported or invalid shell syntax"))
                     (cadr parsed))))
      (mevedel-bash-analysis--result
       source 'treesit texts commands
       (or (not supported) (not (null reasons))) reasons))))


;;
;;; Public interface

;;;###autoload
(defun mevedel-bash-analysis-analyze (source)
  "Return normalized conservative Bash analysis for SOURCE.
The result contains `:class', `:commands', `:parser', `:resources', and
`:reasons'.  Tree-sitter is selected only through normal Emacs grammar
configuration; no grammar path is added here."
  (require 'subr-x)
  (if (and (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'bash))
      (condition-case _err
          (mevedel-bash-analysis--treesit source)
        (error
         (let ((result (mevedel-bash-analysis--heuristic source)))
           (plist-put result :reasons
                      (cons "Tree-sitter analysis failed; used conservative fallback"
                            (plist-get result :reasons))))))
    (mevedel-bash-analysis--heuristic source)))

(provide 'mevedel-bash-analysis)

;;; mevedel-bash-analysis.el ends here
