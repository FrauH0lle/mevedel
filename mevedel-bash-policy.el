;;; mevedel-bash-policy.el --- Argument-aware Bash command policies -*- lexical-binding: t -*-

;;; Commentary:

;; Classifies already parsed command argument vectors as read-only or unknown.
;; The policies are intentionally built in and conservative.  Permission rules
;; decide authority; this module only supplies a default safety fact.

;;; Code:

(eval-when-compile
  (require 'cl-lib))


;;
;;; Policies

(defconst mevedel-bash-policy--simple-read-only-commands
  '("cat" "cd" "cut" "echo" "expr" "false" "grep" "head" "id" "ls"
    "nl" "paste" "pwd" "rev" "seq" "stat" "tail" "tr" "true" "uname"
    "uniq" "wc" "which" "whoami")
  "Commands whose argument handling does not introduce child effects.")

(defconst mevedel-bash-policy--unsafe-find-options
  '("-delete" "-exec" "-execdir" "-fls" "-fprint" "-fprint0"
    "-fprintf" "-ok" "-okdir")
  "Find options that mutate, execute commands, or write files.")

(defun mevedel-bash-policy--base64-read-only-p (argv)
  "Return non-nil when base64 ARGV has no output-file option."
  (not
   (cl-some
    (lambda (argument)
      (or (member argument '("-o" "--output"))
          (string-prefix-p "--output=" argument)
          (and (string-prefix-p "-o" argument)
               (not (string-equal argument "-o")))))
    (cdr argv))))

(defun mevedel-bash-policy--find-read-only-p (argv)
  "Return non-nil when find ARGV has no effectful primary."
  (not
   (cl-some
    (lambda (argument)
      (member argument mevedel-bash-policy--unsafe-find-options))
    (cdr argv))))

(defun mevedel-bash-policy--rg-read-only-p (argv)
  "Return non-nil when ripgrep ARGV cannot launch helper programs."
  (not
   (cl-some
    (lambda (argument)
      (or (member argument '("--hostname-bin" "--pre" "--search-zip" "-z"))
          (string-prefix-p "--hostname-bin=" argument)
          (string-prefix-p "--pre=" argument)))
    (cdr argv))))

(defun mevedel-bash-policy--sed-address-p (program)
  "Return non-nil when PROGRAM is one numeric print address."
  (and program
       (string-match-p "\\`[0-9]+\\(?:,[0-9]+\\)?p\\'" program)))

(defun mevedel-bash-policy--sed-read-only-p (argv)
  "Return non-nil when sed ARGV is the recognized numeric print form."
  (and (<= 3 (length argv))
       (<= (length argv) 4)
       (string-equal (nth 1 argv) "-n")
       (mevedel-bash-policy--sed-address-p (nth 2 argv))
       (or (= (length argv) 3)
           (string-equal (nth 3 argv) "-")
           (not (string-prefix-p "-" (nth 3 argv))))))

(defun mevedel-bash-policy--awk-read-only-p (argv)
  "Return non-nil when AWK ARGV uses one effect-free inline program."
  (let ((program (nth 1 argv)))
    (and (<= 2 (length argv))
         program
         (not (string-prefix-p "-" program))
         (not (cl-some
               (lambda (argument)
                 (string-prefix-p "-" argument))
               (cddr argv)))
         (not (string-match-p
               "\\(?:@[[:space:]]*[[:alpha:]_]\\|\\_<\\(?:close\\|getline\\|system\\)\\_>\\|[>|]\\)"
               program)))))


;;
;;; Public interface

;;;###autoload
(defun mevedel-bash-policy-read-only-p (argv)
  "Return non-nil when parsed command ARGV has a read-only built-in policy."
  (let ((command (car argv)))
    (and command
         (not (string-match-p "/" command))
         (cond
          ((member command mevedel-bash-policy--simple-read-only-commands) t)
          ((string-equal command "awk")
           (mevedel-bash-policy--awk-read-only-p argv))
          ((string-equal command "base64")
           (mevedel-bash-policy--base64-read-only-p argv))
          ((string-equal command "find")
           (mevedel-bash-policy--find-read-only-p argv))
          ((string-equal command "rg")
           (mevedel-bash-policy--rg-read-only-p argv))
          ((string-equal command "sed")
           (mevedel-bash-policy--sed-read-only-p argv))))))

(provide 'mevedel-bash-policy)

;;; mevedel-bash-policy.el ends here
