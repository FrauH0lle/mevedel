;;; test-mevedel-tools-bash-permissions.el --- Tests for bash permission system -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the bash command extraction and permission checking system

;;; Code:

(require 'cl-lib)
(require 'mevedel-structs)
(require 'mevedel-tool-exec)
(require 'mevedel-pipeline)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Quote Balancing Tests

(mevedel-deftest mevedel-tools--quotes-balanced-p ()
  ,test
  (test)
  :doc "balanced quotes:
`mevedel-tools--quotes-balanced-p' accepts strings with no quotes"
  (should (equal t (mevedel-tools--quotes-balanced-p "hello world")))
  :doc "balanced quotes:
`mevedel-tools--quotes-balanced-p' accepts balanced double quotes"
  (should (equal t (mevedel-tools--quotes-balanced-p "\"hello\"")))
  :doc "balanced quotes:
`mevedel-tools--quotes-balanced-p' accepts balanced single quotes"
  (should (equal t (mevedel-tools--quotes-balanced-p "'hello'")))
  :doc "balanced quotes:
`mevedel-tools--quotes-balanced-p' accepts nested different quote types"
  (should (equal t (mevedel-tools--quotes-balanced-p "\"hello 'world'\"")))
  :doc "balanced quotes:
`mevedel-tools--quotes-balanced-p' accepts escaped quotes"
  (should (equal t (mevedel-tools--quotes-balanced-p "\"a \\\"b\\\" c\"")))
  :doc "unbalanced quotes:
`mevedel-tools--quotes-balanced-p' rejects unclosed double quote"
  (should (equal nil (mevedel-tools--quotes-balanced-p "\"hello")))
  :doc "unbalanced quotes:
`mevedel-tools--quotes-balanced-p' rejects unclosed single quote"
  (should (equal nil (mevedel-tools--quotes-balanced-p "'hello")))
  :doc "unbalanced quotes:
`mevedel-tools--quotes-balanced-p' rejects mismatched quotes"
  (should (equal nil (mevedel-tools--quotes-balanced-p "\"hello'"))))

;;
;;; Complex Syntax Detection Tests

(mevedel-deftest mevedel-tools--contains-complex-syntax-p ()
  ,test
  (test)
  :doc "simple commands:
`mevedel-tools--contains-complex-syntax-p' allows simple commands"
  (should (equal nil (mevedel-tools--contains-complex-syntax-p "echo hello")))
  :doc "simple commands:
`mevedel-tools--contains-complex-syntax-p' allows commands with arguments"
  (should (equal nil (mevedel-tools--contains-complex-syntax-p "ls -la /tmp")))
  :doc "simple commands:
`mevedel-tools--contains-complex-syntax-p' allows command chains"
  (should (equal nil (mevedel-tools--contains-complex-syntax-p "echo a && echo b")))
  :doc "simple commands:
`mevedel-tools--contains-complex-syntax-p' allows pipes"
  (should (equal nil (mevedel-tools--contains-complex-syntax-p "cat file | grep pattern")))
  :doc "simple commands:
`mevedel-tools--contains-complex-syntax-p' allows command substitution (not complex)"
  (should (equal nil (mevedel-tools--contains-complex-syntax-p "echo $(pwd)")))
  :doc "complex syntax detection:
`mevedel-tools--contains-complex-syntax-p' detects variable expansion"
  (should (equal t (mevedel-tools--contains-complex-syntax-p "echo $VAR")))
  :doc "complex syntax detection:
`mevedel-tools--contains-complex-syntax-p' detects braced variable expansion"
  (should (equal t (mevedel-tools--contains-complex-syntax-p "echo ${VAR}")))
  :doc "complex syntax detection:
`mevedel-tools--contains-complex-syntax-p' detects eval command"
  (should (equal t (mevedel-tools--contains-complex-syntax-p "eval 'dangerous'")))
  :doc "complex syntax detection:
`mevedel-tools--contains-complex-syntax-p' detects exec command"
  (should (equal t (mevedel-tools--contains-complex-syntax-p "exec rm -rf /")))
  :doc "complex syntax detection:
`mevedel-tools--contains-complex-syntax-p' detects here documents"
  (should (equal t (mevedel-tools--contains-complex-syntax-p "cat << EOF")))
  :doc "complex syntax detection:
`mevedel-tools--contains-complex-syntax-p' detects brace expansion"
  (should (equal t (mevedel-tools--contains-complex-syntax-p "echo {a,b,c}")))
  :doc "complex syntax detection:
`mevedel-tools--contains-complex-syntax-p' detects unbalanced quotes"
  (should (equal t (mevedel-tools--contains-complex-syntax-p "echo \"hello"))))

;;
;;; Command Chain Splitting Tests

(mevedel-deftest mevedel-tools--split-command-chain ()
  ,test
  (test)
  :doc "simple commands:
`mevedel-tools--split-command-chain' returns single command as-is"
  (should (equal '("echo hello")
                 (mevedel-tools--split-command-chain "echo hello")))
  :doc "simple commands:
`mevedel-tools--split-command-chain' handles empty string"
  (should (equal '()
                 (mevedel-tools--split-command-chain "")))
  :doc "command chains:
`mevedel-tools--split-command-chain' splits on &&"
  (should (equal '("cmd1" "cmd2")
                 (mevedel-tools--split-command-chain "cmd1 && cmd2")))
  :doc "command chains:
`mevedel-tools--split-command-chain' splits on ||"
  (should (equal '("cmd1" "cmd2")
                 (mevedel-tools--split-command-chain "cmd1 || cmd2")))
  :doc "command chains:
`mevedel-tools--split-command-chain' splits on semicolon"
  (should (equal '("cmd1" "cmd2")
                 (mevedel-tools--split-command-chain "cmd1 ; cmd2")))
  :doc "command chains:
`mevedel-tools--split-command-chain' splits on pipe"
  (should (equal '("cmd1" "cmd2")
                 (mevedel-tools--split-command-chain "cmd1 | cmd2")))
  :doc "command chains:
`mevedel-tools--split-command-chain' splits on newline"
  (should (equal '("cmd1" "cmd2")
                 (mevedel-tools--split-command-chain "cmd1\ncmd2")))
  :doc "command chains:
`mevedel-tools--split-command-chain' handles multiple separators"
  (should (equal '("a" "b" "c" "d" "e")
                 (mevedel-tools--split-command-chain "a && b || c ; d | e")))
  :doc "quote handling:
`mevedel-tools--split-command-chain' does not split inside double quotes"
  (should (equal '("echo \"a && b\"")
                 (mevedel-tools--split-command-chain "echo \"a && b\"")))
  :doc "quote handling:
`mevedel-tools--split-command-chain' does not split inside single quotes"
  (should (equal '("echo 'a || b'")
                 (mevedel-tools--split-command-chain "echo 'a || b'")))
  :doc "quote handling:
`mevedel-tools--split-command-chain' handles quotes with separators outside"
  (should (equal '("echo \"hello\"" "ls")
                 (mevedel-tools--split-command-chain "echo \"hello\" && ls"))))

;;
;;; Substitution Extraction Tests

(mevedel-deftest mevedel-tools--extract-substitutions ()
  ,test
  (test)
  :doc "$() substitutions:
`mevedel-tools--extract-substitutions' extracts simple substitution"
  (should (equal '("pwd")
                 (mevedel-tools--extract-substitutions "echo $(pwd)")))
  :doc "$() substitutions:
`mevedel-tools--extract-substitutions' extracts multiple substitutions"
  (should (equal '("date" "pwd")
                 (mevedel-tools--extract-substitutions "echo $(date) $(pwd)")))
  :doc "$() substitutions:
`mevedel-tools--extract-substitutions' extracts nested substitutions"
  (should (equal '("echo $(pwd)")
                 (mevedel-tools--extract-substitutions "echo $(echo $(pwd))")))
  :doc "$() substitutions:
`mevedel-tools--extract-substitutions' handles empty command string"
  (should (equal '()
                 (mevedel-tools--extract-substitutions "echo hello")))
  :doc "backtick substitutions:
`mevedel-tools--extract-substitutions' extracts backtick substitution"
  (should (equal '("date")
                 (mevedel-tools--extract-substitutions "echo `date`")))
  :doc "backtick substitutions:
`mevedel-tools--extract-substitutions' extracts multiple backtick substitutions"
  (should (equal '("date" "pwd")
                 (mevedel-tools--extract-substitutions "echo `date` `pwd`"))))

;;
;;; Command Name Extraction Tests

(mevedel-deftest mevedel-tools--extract-command-name ()
  ,test
  (test)
  :doc "simple commands:
`mevedel-tools--extract-command-name' extracts simple command"
  (should (equal "ls"
                 (mevedel-tools--extract-command-name "ls")))
  :doc "simple commands:
`mevedel-tools--extract-command-name' extracts command with arguments"
  (should (equal "ls"
                 (mevedel-tools--extract-command-name "ls -la /tmp")))
  :doc "simple commands:
`mevedel-tools--extract-command-name' handles quoted arguments"
  (should (equal "echo"
                 (mevedel-tools--extract-command-name "echo \"hello world\"")))
  :doc "command prefixes:
`mevedel-tools--extract-command-name' extracts command after sudo"
  (should (equal "rm"
                 (mevedel-tools--extract-command-name "sudo rm -rf /")))
  :doc "command prefixes:
`mevedel-tools--extract-command-name' extracts command after env with assignments"
  (should (equal "ls"
                 (mevedel-tools--extract-command-name "env VAR=val ls")))
  :doc "command prefixes:
`mevedel-tools--extract-command-name' extracts command after nice"
  (should (equal "ls"
                 (mevedel-tools--extract-command-name "nice -n 10 ls")))
  :doc "command prefixes:
`mevedel-tools--extract-command-name' extracts command after timeout"
  (should (equal "ls"
                 (mevedel-tools--extract-command-name "timeout 5 ls")))
  :doc "paths:
`mevedel-tools--extract-command-name' extracts basename from absolute path"
  (should (equal "bash"
                 (mevedel-tools--extract-command-name "/bin/bash")))
  :doc "paths:
`mevedel-tools--extract-command-name' extracts basename from relative path"
  (should (equal "script.sh"
                 (mevedel-tools--extract-command-name "./script.sh")))
  :doc "variable assignments:
`mevedel-tools--extract-command-name' skips variable assignments"
  (should (equal "ls"
                 (mevedel-tools--extract-command-name "VAR=value ls")))
  :doc "variable assignments:
`mevedel-tools--extract-command-name' skips multiple variable assignments"
  (should (equal "ls"
                 (mevedel-tools--extract-command-name "A=1 B=2 ls"))))

;;
;;; Full Command Extraction Tests

(mevedel-deftest mevedel-tools--extract-commands ()
  ,test
  (test)
  :doc "simple commands:
`mevedel-tools--extract-commands' extracts single command"
  (let ((result (mevedel-tools--extract-commands "echo hello")))
    (should (equal '("echo") (car result)))
    (should (equal nil (cdr result))))
  :doc "simple commands:
`mevedel-tools--extract-commands' extracts command with path"
  (let ((result (mevedel-tools--extract-commands "/bin/ls -la")))
    (should (equal '("ls") (car result)))
    (should (equal nil (cdr result))))
  :doc "command chains:
`mevedel-tools--extract-commands' extracts from &&"
  (let ((result (mevedel-tools--extract-commands "echo a && ls")))
    (should (equal '("echo" "ls") (car result)))
    (should (equal nil (cdr result))))
  :doc "command chains:
`mevedel-tools--extract-commands' extracts from pipes"
  (let ((result (mevedel-tools--extract-commands "cat file | grep pattern | wc -l")))
    (should (equal '("cat" "grep" "wc") (car result)))
    (should (equal nil (cdr result))))
  :doc "command chains:
`mevedel-tools--extract-commands' extracts from semicolons"
  (let ((result (mevedel-tools--extract-commands "echo a ; ls ; pwd")))
    (should (equal '("echo" "ls" "pwd") (car result)))
    (should (equal nil (cdr result))))
  :doc "command substitutions:
`mevedel-tools--extract-commands' extracts from simple substitution"
  (let ((result (mevedel-tools--extract-commands "echo $(pwd)")))
    (should (equal '("echo" "pwd") (car result)))
    (should (equal nil (cdr result))))
  :doc "command substitutions:
`mevedel-tools--extract-commands' extracts from nested substitutions"
  (let ((result (mevedel-tools--extract-commands "echo $(echo $(pwd))")))
    (should (equal '("echo" "echo" "pwd") (car result)))
    (should (equal nil (cdr result))))
  :doc "command substitutions:
`mevedel-tools--extract-commands' extracts from backticks"
  (let ((result (mevedel-tools--extract-commands "echo `date`")))
    (should (equal '("echo" "date") (car result)))
    (should (equal nil (cdr result))))
  :doc "dangerous command extraction:
`mevedel-tools--extract-commands' detects sudo in chains"
  (let ((result (mevedel-tools--extract-commands "echo hello && sudo rm -rf /")))
    ;; Extracts both sudo and the command after it for better security
    (should (equal '("echo" "sudo" "rm") (car result))))
  :doc "dangerous command extraction:
`mevedel-tools--extract-commands' detects commands in substitutions"
  (let ((result (mevedel-tools--extract-commands "echo $(sudo rm -rf /)")))
    ;; Extracts echo, then recursively extracts sudo and rm from the substitution
    (should (equal '("echo" "sudo" "rm") (car result))))
  :doc "complex syntax handling:
`mevedel-tools--extract-commands' marks variable expansion as unparseable"
  (let ((result (mevedel-tools--extract-commands "echo $VAR")))
    (should (equal t (cdr result))))
  :doc "complex syntax handling:
`mevedel-tools--extract-commands' marks eval as unparseable"
  (let ((result (mevedel-tools--extract-commands "eval 'dangerous'")))
    (should (equal t (cdr result))))
  :doc "complex syntax handling:
`mevedel-tools--extract-commands' marks unbalanced quotes as unparseable"
  (let ((result (mevedel-tools--extract-commands "echo \"hello")))
    (should (equal t (cdr result)))))


;;
;;; Allow pattern suggestions

(mevedel-deftest mevedel-tool-exec--bash-allow-patterns ()
  ,test
  (test)
  :doc "subcommand prefixes:
`mevedel-tool-exec--bash-allow-patterns' generalizes stable subcommands"
  (should (equal '("git log:*")
                 (mevedel-tool-exec--bash-allow-patterns
                  "git log --oneline --graph")))
  :doc "compound commands:
`mevedel-tool-exec--bash-allow-patterns' returns one rule per segment"
  (should (equal '("pwd" "git log:*")
                 (mevedel-tool-exec--bash-allow-patterns
                  "pwd && git log --oneline")))
  :doc "flag arguments:
`mevedel-tool-exec--bash-allow-patterns' keeps exact command when token 2 is a flag"
  (should (equal '("pytest -q test/test-mevedel-tools.el")
                 (mevedel-tool-exec--bash-allow-patterns
                  "pytest -q test/test-mevedel-tools.el")))
  :doc "safe env vars:
`mevedel-tool-exec--bash-allow-patterns' skips safe env assignments"
  (should (equal '("npm run:*")
                 (mevedel-tool-exec--bash-allow-patterns
                  "NODE_ENV=test npm run test")))
  :doc "unsafe env vars:
`mevedel-tool-exec--bash-allow-patterns' keeps exact command with unknown env vars"
  (should (equal '("DOCKER_HOST=tcp://example docker ps")
                 (mevedel-tool-exec--bash-allow-patterns
                  "DOCKER_HOST=tcp://example docker ps")))
  :doc "dangerous commands:
`mevedel-tool-exec--bash-allow-patterns' does not generalize dangerous commands"
  (let ((mevedel-bash-dangerous-commands '("curl")))
    (should (equal '("curl get https://example.com")
                   (mevedel-tool-exec--bash-allow-patterns
                    "curl get https://example.com")))))

;;
;;; Permission Checking Integration Tests

(mevedel-deftest mevedel-tools--check-bash-permission
  (:vars (original-rules original-dangerous original-fail-safe)
   :before-each
   ;; Save original permissions
   (progn
     (setq original-rules mevedel-permission-rules)
     (setq original-dangerous mevedel-bash-dangerous-commands)
     (setq original-fail-safe mevedel-bash-fail-safe-on-complex-syntax))
   :after-each
   ;; Restore original permissions
   (progn
     (setq mevedel-permission-rules original-rules)
     (setq mevedel-bash-dangerous-commands original-dangerous)
     (setq mevedel-bash-fail-safe-on-complex-syntax original-fail-safe)))
  ,test
  (test)
  :doc "allow patterns:
`mevedel-tools--check-bash-permission' allows commands matching allow patterns"
  ;; Specifier-carrying rules override unqualified generic rules
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :action deny)
            ("Bash" :pattern "echo*" :action allow)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'allow (mevedel-tools--check-bash-permission "echo hello"))))
  :doc "allow patterns:
`mevedel-tools--check-bash-permission' allows all commands in chain if all match"
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :action deny)
            ("Bash" :pattern "echo*" :action allow)
            ("Bash" :pattern "ls*" :action allow)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'allow (mevedel-tools--check-bash-permission "echo hello && ls -la"))))
  :doc "allow patterns:
`mevedel-tools--check-bash-permission' allows compound commands with arguments (git status)"
  ;; Matches full command string "git status" against "git status*"
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :action deny)
            ("Bash" :pattern "git status*" :action allow)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'allow (mevedel-tools--check-bash-permission "git status"))))
  :doc "allow patterns:
`mevedel-tools--check-bash-permission' allows compound commands with additional arguments (git log --oneline)"
  ;; Matches full command string "git log --oneline" against "git log*"
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :action deny)
            ("Bash" :pattern "git log*" :action allow)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'allow (mevedel-tools--check-bash-permission "git log --oneline --graph"))))
  :doc "allow prefix patterns:
`mevedel-tools--check-bash-permission' treats PREFIX:* as a word-boundary prefix"
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :action deny)
            ("Bash" :pattern "git log:*" :action allow)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'allow (mevedel-tools--check-bash-permission "git log")))
    (should (equal 'allow (mevedel-tools--check-bash-permission "git log --oneline")))
    (should (equal 'deny (mevedel-tools--check-bash-permission "git lollipop"))))
  :doc "operator detection:
`mevedel-tools--check-bash-permission' detects && operator and checks all commands"
  ;; With &&, should check both extracted commands (echo and rm)
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :pattern "*" :action allow)
            ("Bash" :pattern "rm*" :action deny)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'deny (mevedel-tools--check-bash-permission "echo hello && rm file"))))
  :doc "operator detection:
`mevedel-tools--check-bash-permission' detects || operator and checks all commands"
  ;; With ||, should check both extracted commands
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :pattern "*" :action allow)
            ("Bash" :pattern "rm*" :action deny)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'deny (mevedel-tools--check-bash-permission "ls || rm file"))))
  :doc "operator detection:
`mevedel-tools--check-bash-permission' detects | (pipe) operator and checks all commands"
  ;; With pipe, should check both extracted commands
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :pattern "*" :action allow)
            ("Bash" :pattern "rm*" :action deny)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'deny (mevedel-tools--check-bash-permission "cat file | rm file"))))
  :doc "operator detection:
`mevedel-tools--check-bash-permission' detects ; (semicolon) operator and checks all commands"
  ;; With semicolon, should check both extracted commands
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :pattern "*" :action allow)
            ("Bash" :pattern "rm*" :action deny)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'deny (mevedel-tools--check-bash-permission "echo hello ; rm file"))))
  :doc "operator detection:
`mevedel-tools--check-bash-permission' detects newline operator and checks all commands"
  ;; With newline, should check both extracted commands
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :pattern "*" :action allow)
            ("Bash" :pattern "rm*" :action deny)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'deny (mevedel-tools--check-bash-permission "echo hello\nrm file"))))
  :doc "operator detection:
`mevedel-tools--check-bash-permission' treats commands without operators as specific match"
  ;; Without operators, specific pattern should take precedence
  ;; "echo*" matches, so we trust it (don't check extracted "echo")
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :action deny)
            ("Bash" :pattern "echo*" :action allow)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'allow (mevedel-tools--check-bash-permission "echo hello world"))))
  :doc "deny patterns:
`mevedel-tools--check-bash-permission' denies commands matching deny patterns"
  ;; Later entries override earlier - put specific patterns LAST
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :pattern "*" :action allow)
            ("Bash" :pattern "rm*" :action deny)))
    (should (equal 'deny (mevedel-tools--check-bash-permission "rm -rf /"))))
  :doc "deny patterns:
`mevedel-tools--check-bash-permission' denies chain if any command matches deny pattern"
  ;; Later entries override earlier - put specific patterns LAST
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :pattern "*" :action allow)
            ("Bash" :pattern "rm*" :action deny)))
    (should (equal 'deny (mevedel-tools--check-bash-permission "echo hello && rm file"))))
  :doc "dangerous command blocklist:
`mevedel-tools--check-bash-permission' asks for dangerous commands even if pattern allows"
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :pattern "*" :action allow)))
    (setq mevedel-bash-dangerous-commands '("rm" "sudo"))
    (should (equal 'ask (mevedel-tools--check-bash-permission "rm file"))))
  :doc "dangerous command blocklist:
`mevedel-tools--check-bash-permission' detects dangerous commands in chains"
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :pattern "*" :action allow)))
    (setq mevedel-bash-dangerous-commands '("sudo"))
    (should (equal 'ask (mevedel-tools--check-bash-permission "echo hello && sudo ls"))))
  :doc "dangerous command blocklist:
`mevedel-tools--check-bash-permission' detects dangerous commands after sudo"
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :pattern "*" :action allow)))
    (setq mevedel-bash-dangerous-commands '("rm"))
    (should (equal 'ask (mevedel-tools--check-bash-permission "sudo rm -rf /"))))
  :doc "complex syntax handling:
`mevedel-tools--check-bash-permission' asks for complex syntax when fail-safe is enabled"
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :pattern "*" :action allow)))
    (setq mevedel-bash-fail-safe-on-complex-syntax t)
    (should (equal 'ask (mevedel-tools--check-bash-permission "echo $VAR"))))
  :doc "complex syntax handling:
`mevedel-tools--check-bash-permission' attempts parsing when fail-safe is disabled"
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :pattern "*" :action allow)))
    (setq mevedel-bash-dangerous-commands '())
    (setq mevedel-bash-fail-safe-on-complex-syntax nil)
    (should (equal 'allow (mevedel-tools--check-bash-permission "echo $VAR"))))
  :doc "precedence rules:
`mevedel-tools--check-bash-permission': deny takes precedence over ask"
  ;; Later entries override - put specific patterns LAST
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :pattern "*" :action allow)
            ("Bash" :pattern "echo*" :action ask)
            ("Bash" :pattern "rm*" :action deny)))
    (should (equal 'deny (mevedel-tools--check-bash-permission "echo hello && rm file"))))
  :doc "precedence rules:
`mevedel-tools--check-bash-permission': ask takes precedence over allow"
  ;; Later entries override - put specific patterns LAST
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :pattern "*" :action allow)
            ("Bash" :pattern "rm*" :action ask)))
    (should (equal 'ask (mevedel-tools--check-bash-permission "ls && rm file"))))
  :doc "precedence rules:
`mevedel-tools--check-bash-permission': specifier-carrying rules override unqualified rules"
  ;; Unqualified deny is overridden by the pattern-specifier allow
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :action deny)
            ("Bash" :pattern "echo*" :action allow)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'allow (mevedel-tools--check-bash-permission "echo hello"))))
  :doc "no-rules default:
`mevedel-tools--check-bash-permission' returns ask when no rules match"
  ;; Safety: unknown bash commands always prompt even if trust-all would skip
  (progn
    (setq mevedel-permission-rules nil)
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'ask (mevedel-tools--check-bash-permission "somecmd foo"))))
  :doc "compound commands:
`mevedel-tools--check-bash-permission' accepts reusable segment patterns"
  (progn
    (setq mevedel-permission-rules
          '(("Bash" :pattern "git log *" :action allow)
            ("Bash" :pattern "pwd" :action allow)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'allow
                   (mevedel-tools--check-bash-permission
                    "pwd && git log --oneline")))))


;;
;;; Permission adapter

(mevedel-deftest mevedel-tool-exec--check-permission-async ()
  ,test
  (test)
  :doc "extracts command from input and returns permission"
  (let ((mevedel-permission-rules
         '(("Bash" :pattern "echo*" :action allow)))
        (mevedel-bash-dangerous-commands nil)
        (mevedel-bash-fail-safe-on-complex-syntax t)
        outcome)
    (mevedel-tool-exec--check-permission-async
     nil '(:command "echo hello") (lambda (r) (setq outcome r)))
    (should (eq outcome 'allow)))
  :doc "returns nil when input has no command"
  (let (outcome)
    (mevedel-tool-exec--check-permission-async
     nil '(:other "value") (lambda (r) (setq outcome r)))
    (should (null outcome)))
  :doc "returns deny for denied commands"
  (let ((mevedel-permission-rules
         '(("Bash" :pattern "rm*" :action deny)))
        (mevedel-bash-dangerous-commands nil)
        outcome)
    (mevedel-tool-exec--check-permission-async
     nil '(:command "rm -rf /") (lambda (r) (setq outcome r)))
    (should (eq outcome 'deny)))
  :doc "prompts user and returns allow when pattern says ask and user approves"
  ;; Bash prompts through the queue's 5-button overlay instead of the
  ;; legacy direct prompt primitive.  Mock the queued entry point and
  ;; exercise the allow-once outcome.
  (let ((mevedel-permission-rules
         '(("Bash" :pattern "*" :action allow)))
        (mevedel-bash-dangerous-commands '("sudo"))
        outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry)
                 (funcall (plist-get entry :callback) 'allow-once))))
      (mevedel-tool-exec--check-permission-async
       nil '(:command "sudo ls") (lambda (r) (setq outcome r))))
    (should (eq outcome 'allow)))
  :doc "prompts user and returns deny when pattern says ask and user denies"
  (let ((mevedel-permission-rules
         '(("Bash" :pattern "*" :action allow)))
        (mevedel-bash-dangerous-commands '("sudo"))
        outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry)
                 (funcall (plist-get entry :callback) 'deny-once))))
      (mevedel-tool-exec--check-permission-async
       nil '(:command "sudo ls") (lambda (r) (setq outcome r))))
    (should (eq outcome 'deny)))
  :doc "feedback maps to (deny . REASON) with the historical message"
  ;; Feedback is part of the authoritative queued prompt vocabulary.
  ;; Mock the queue entry point and deliver it directly to the
  ;; adapter's callback.
  (let ((mevedel-permission-rules
         '(("Bash" :pattern "*" :action allow)))
        (mevedel-bash-dangerous-commands '("sudo"))
        outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry)
                 (funcall (plist-get entry :callback)
                          '(feedback . "use git instead")))))
      (mevedel-tool-exec--check-permission-async
       nil '(:command "sudo ls") (lambda (r) (setq outcome r))))
    (should (consp outcome))
    (should (eq 'deny (car outcome)))
    (should (equal "Command cancelled by user. Feedback: use git instead"
                   (cdr outcome))))
  :doc "allow-session stores the suggested reusable Bash prefix pattern"
  (let* ((root (make-temp-file "mevedel-bash-rules-" t))
         (workspace (mevedel-workspace-get-or-create
                     'test root root "test"))
         (session (mevedel-session-create "main" workspace))
         (mevedel--session session)
         (mevedel-permission-rules nil)
         (mevedel-bash-dangerous-commands nil)
         outcome)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                   (lambda (entry)
                     (funcall (plist-get entry :callback)
                              'allow-session))))
          (mevedel-tool-exec--check-permission-async
           nil '(:command "git log --oneline")
           (lambda (r) (setq outcome r)))
          (should (eq outcome 'allow))
          (should (member '("Bash" :pattern "git log:*" :action allow)
                          (mevedel-session-permission-rules session))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))


;;
;;; Bash handler

(mevedel-deftest mevedel-tool-exec--bash ()
  ,test
  (test)
  :doc "errors on missing command"
  (should-error
   (mevedel-tool-exec--bash #'ignore (list))
   :type 'error)
  :doc "executes simple command and returns output"
  (let ((result nil)
        (done nil))
    (mevedel-tool-exec--bash
     (lambda (r) (setq result r done t))
     (list :command "echo hello"))
    ;; Wait for async process
    (with-timeout (5 (error "Timed out"))
      (while (not done)
        (accept-process-output nil 0.1)))
    (should (string-match-p "hello" result)))
  :doc "reports exit code on failure"
  (let ((result nil)
        (done nil))
    (mevedel-tool-exec--bash
     (lambda (r) (setq result r done t))
     (list :command "exit 42"))
    (with-timeout (5 (error "Timed out"))
      (while (not done)
        (accept-process-output nil 0.1)))
    (should (string-match-p "exit code 42" result)))
  :doc "runs from the workspace root even when current buffer is elsewhere"
  (let* ((root (make-temp-file "mevedel-bash-cwd-" t))
         (agent-dir (file-name-concat root ".mevedel" "sessions"
                                      "main" "agents"))
         (workspace (mevedel-workspace-get-or-create
                     'test root root "test"))
         (session (mevedel-session-create "main" workspace))
         (mevedel--session session)
         (default-directory (file-name-as-directory agent-dir))
         result done)
    (make-directory agent-dir t)
    (unwind-protect
        (progn
          (mevedel-tool-exec--bash
           (lambda (r) (setq result r done t))
           (list :command "pwd"))
          (with-timeout (5 (error "Timed out"))
            (while (not done)
              (accept-process-output nil 0.1)))
          (should (equal (file-name-as-directory root)
                         (file-name-as-directory
                          (string-trim result)))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))



;;
;;; Eval check-permission adapter

(mevedel-deftest mevedel-tool-exec--eval-check-permission-async ()
  ,test
  (test)
  :doc "returns deny when input has no expression"
  (let (outcome)
    (mevedel-tool-exec--eval-check-permission-async
     nil '(:other "value") (lambda (r) (setq outcome r)))
    (should (eq outcome 'deny)))
  :doc "returns allow when user approves"
  (let (outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry)
                 (funcall (plist-get entry :callback) 'allow-once))))
      (mevedel-tool-exec--eval-check-permission-async
       nil '(:expression "(+ 1 2)") (lambda (r) (setq outcome r))))
    (should (eq outcome 'allow)))
  :doc "returns deny when user denies"
  (let (outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry)
                 (funcall (plist-get entry :callback) 'deny-once))))
      (mevedel-tool-exec--eval-check-permission-async
       nil '(:expression "(+ 1 2)") (lambda (r) (setq outcome r))))
    (should (eq outcome 'deny)))
  :doc "feedback maps to (deny . REASON) with the historical message"
  (let (outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry)
                 (funcall (plist-get entry :callback)
                          '(feedback . "too dangerous")))))
      (mevedel-tool-exec--eval-check-permission-async
       nil '(:expression "(delete-file \"/etc/passwd\")")
       (lambda (r) (setq outcome r))))
    (should (consp outcome))
    (should (eq 'deny (car outcome)))
    (should (equal "Eval cancelled by user. Feedback: too dangerous"
                   (cdr outcome)))))


;;
;;; Eval handler

(mevedel-deftest mevedel-tool-exec--eval ()
  ,test
  (test)
  :doc "errors on missing expression"
  (should-error
   (mevedel-tool-exec--eval #'ignore (list))
   :type 'error)
  :doc "evaluates simple expression"
  (let (result)
    (mevedel-tool-exec--eval
     (lambda (r) (setq result r))
     (list :expression "(+ 1 2 3)"))
    (should (string-match-p "Result:\n6" result)))
  :doc "captures printed output"
  (let (result)
    (mevedel-tool-exec--eval
     (lambda (r) (setq result r))
     (list :expression "(princ \"hello world\")"))
    (should (string-match-p "STDOUT:\nhello world" result)))
  :doc "reports eval errors"
  (let (result)
    (mevedel-tool-exec--eval
     (lambda (r) (setq result r))
     (list :expression "(error \"test error\")"))
    (should (string-match-p "Error:.*test error" result)))
  :doc "returns string results with %S formatting"
  (let (result)
    (mevedel-tool-exec--eval
     (lambda (r) (setq result r))
     (list :expression "\"hello\""))
    (should (string-match-p "Result:\n\"hello\"" result))))


;;
;;; Renderer

(mevedel-deftest mevedel-tool-exec--render-bash ()
  ,test
  (test)
  :doc "returns nil for non-string result"
  (should (null (mevedel-tool-exec--render-bash
                 "Bash" '(:command "ls") nil nil)))

  :doc "header shows first line of the command; body-mode is sh-mode"
  (let* ((body "file1\nfile2\n")
         (plist (mevedel-tool-exec--render-bash
                 "Bash" '(:command "ls -la\n# more") body nil)))
    (should (equal "Bash: ls -la" (plist-get plist :header)))
    (should (equal body (plist-get plist :body)))
    (should (eq 'sh-mode (plist-get plist :body-mode)))))

(provide 'test-mevedel-tools-bash-permissions)
;;; test-mevedel-tools-bash-permissions.el ends here
