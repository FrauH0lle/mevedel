;;; test-mevedel-tools-bash-permissions.el --- Tests for bash permission system -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the bash command extraction and permission checking system

;;; Code:

(require 'mevedel)
(load (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "helpers"))

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
;;; Permission Checking Integration Tests

(mevedel-deftest mevedel-tools--check-bash-permission
  (:vars (original-permissions original-dangerous original-fail-safe)
   :before-each
   ;; Save original permissions
   (progn
     (setq original-permissions mevedel-bash-permissions)
     (setq original-dangerous mevedel-bash-dangerous-commands)
     (setq original-fail-safe mevedel-tools--bash-fail-safe-on-complex-syntax))
   :after-each
   ;; Restore original permissions
   (progn
     (setq mevedel-bash-permissions original-permissions)
     (setq mevedel-bash-dangerous-commands original-dangerous)
     (setq mevedel-tools--bash-fail-safe-on-complex-syntax original-fail-safe)))
  ,test
  (test)
  :doc "allow patterns:
`mevedel-tools--check-bash-permission' allows commands matching allow patterns"
  ;; Later entries override earlier - put specific patterns LAST
  (progn
    (setq mevedel-bash-permissions '(("*" . deny) ("echo*" . allow)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'allow (mevedel-tools--check-bash-permission "echo hello"))))
  :doc "allow patterns:
`mevedel-tools--check-bash-permission' allows all commands in chain if all match"
  ;; Later entries override earlier - put specific patterns LAST
  (progn
    (setq mevedel-bash-permissions '(("*" . deny) ("echo*" . allow) ("ls*" . allow)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'allow (mevedel-tools--check-bash-permission "echo hello && ls -la"))))
  :doc "allow patterns:
`mevedel-tools--check-bash-permission' allows compound commands with arguments (git status)"
  ;; Matches full command string "git status" against "git status*"
  (progn
    (setq mevedel-bash-permissions '(("*" . deny) ("git status*" . allow)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'allow (mevedel-tools--check-bash-permission "git status"))))
  :doc "allow patterns:
`mevedel-tools--check-bash-permission' allows compound commands with additional arguments (git log --oneline)"
  ;; Matches full command string "git log --oneline" against "git log*"
  (progn
    (setq mevedel-bash-permissions '(("*" . deny) ("git log*" . allow)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'allow (mevedel-tools--check-bash-permission "git log --oneline --graph"))))
  :doc "operator detection:
`mevedel-tools--check-bash-permission' detects && operator and checks all commands"
  ;; With &&, should check both extracted commands (echo and rm)
  (progn
    (setq mevedel-bash-permissions '(("*" . allow) ("rm*" . deny)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'deny (mevedel-tools--check-bash-permission "echo hello && rm file"))))
  :doc "operator detection:
`mevedel-tools--check-bash-permission' detects || operator and checks all commands"
  ;; With ||, should check both extracted commands
  (progn
    (setq mevedel-bash-permissions '(("*" . allow) ("rm*" . deny)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'deny (mevedel-tools--check-bash-permission "ls || rm file"))))
  :doc "operator detection:
`mevedel-tools--check-bash-permission' detects | (pipe) operator and checks all commands"
  ;; With pipe, should check both extracted commands
  (progn
    (setq mevedel-bash-permissions '(("*" . allow) ("rm*" . deny)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'deny (mevedel-tools--check-bash-permission "cat file | rm file"))))
  :doc "operator detection:
`mevedel-tools--check-bash-permission' detects ; (semicolon) operator and checks all commands"
  ;; With semicolon, should check both extracted commands
  (progn
    (setq mevedel-bash-permissions '(("*" . allow) ("rm*" . deny)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'deny (mevedel-tools--check-bash-permission "echo hello ; rm file"))))
  :doc "operator detection:
`mevedel-tools--check-bash-permission' detects newline operator and checks all commands"
  ;; With newline, should check both extracted commands
  (progn
    (setq mevedel-bash-permissions '(("*" . allow) ("rm*" . deny)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'deny (mevedel-tools--check-bash-permission "echo hello\nrm file"))))
  :doc "operator detection:
`mevedel-tools--check-bash-permission' treats commands without operators as specific match"
  ;; Without operators, specific pattern should take precedence
  ;; "echo*" matches, so we trust it (don't check extracted "echo")
  (progn
    (setq mevedel-bash-permissions '(("*" . deny) ("echo*" . allow)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'allow (mevedel-tools--check-bash-permission "echo hello world"))))
  :doc "deny patterns:
`mevedel-tools--check-bash-permission' denies commands matching deny patterns"
  ;; Later entries override earlier - put specific patterns LAST
  (progn
    (setq mevedel-bash-permissions '(("*" . allow) ("rm*" . deny)))
    (should (equal 'deny (mevedel-tools--check-bash-permission "rm -rf /"))))
  :doc "deny patterns:
`mevedel-tools--check-bash-permission' denies chain if any command matches deny pattern"
  ;; Later entries override earlier - put specific patterns LAST
  (progn
    (setq mevedel-bash-permissions '(("*" . allow) ("rm*" . deny)))
    (should (equal 'deny (mevedel-tools--check-bash-permission "echo hello && rm file"))))
  :doc "dangerous command blocklist:
`mevedel-tools--check-bash-permission' asks for dangerous commands even if pattern allows"
  (progn
    (setq mevedel-bash-permissions '(("*" . allow)))
    (setq mevedel-bash-dangerous-commands '("rm" "sudo"))
    (should (equal 'ask (mevedel-tools--check-bash-permission "rm file"))))
  :doc "dangerous command blocklist:
`mevedel-tools--check-bash-permission' detects dangerous commands in chains"
  (progn
    (setq mevedel-bash-permissions '(("*" . allow)))
    (setq mevedel-bash-dangerous-commands '("sudo"))
    (should (equal 'ask (mevedel-tools--check-bash-permission "echo hello && sudo ls"))))
  :doc "dangerous command blocklist:
`mevedel-tools--check-bash-permission' detects dangerous commands after sudo"
  (progn
    (setq mevedel-bash-permissions '(("*" . allow)))
    (setq mevedel-bash-dangerous-commands '("rm"))
    (should (equal 'ask (mevedel-tools--check-bash-permission "sudo rm -rf /"))))
  :doc "complex syntax handling:
`mevedel-tools--check-bash-permission' asks for complex syntax when fail-safe is enabled"
  (progn
    (setq mevedel-bash-permissions '(("*" . allow)))
    (setq mevedel-tools--bash-fail-safe-on-complex-syntax t)
    (should (equal 'ask (mevedel-tools--check-bash-permission "echo $VAR"))))
  :doc "complex syntax handling:
`mevedel-tools--check-bash-permission' attempts parsing when fail-safe is disabled"
  (progn
    (setq mevedel-bash-permissions '(("*" . allow)))
    (setq mevedel-bash-dangerous-commands '())
    (setq mevedel-tools--bash-fail-safe-on-complex-syntax nil)
    (should (equal 'allow (mevedel-tools--check-bash-permission "echo $VAR"))))
  :doc "precedence rules:
`mevedel-tools--check-bash-permission': deny takes precedence over ask"
  ;; Later entries override - put specific patterns LAST
  (progn
    (setq mevedel-bash-permissions '(("*" . allow) ("echo*" . ask) ("rm*" . deny)))
    (should (equal 'deny (mevedel-tools--check-bash-permission "echo hello && rm file"))))
  :doc "precedence rules:
`mevedel-tools--check-bash-permission': ask takes precedence over allow"
  ;; Later entries override - put specific patterns LAST
  (progn
    (setq mevedel-bash-permissions '(("*" . allow) ("rm*" . ask)))
    (should (equal 'ask (mevedel-tools--check-bash-permission "ls && rm file"))))
  :doc "precedence rules:
`mevedel-tools--check-bash-permission': later patterns override earlier ones"
  (progn
    (setq mevedel-bash-permissions '(("*" . deny) ("echo*" . allow)))
    (setq mevedel-bash-dangerous-commands '())
    (should (equal 'allow (mevedel-tools--check-bash-permission "echo hello")))))

(provide 'test-mevedel-tools-bash-permissions)
;;; test-mevedel-tools-bash-permissions.el ends here
