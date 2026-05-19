;;; tests/test-mevedel-utilities.el -- Unit tests for mevedel-utilities.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defun test-mevedel-utilities--raw-bytes (&rest bytes)
  "Return BYTES as an Emacs string of raw byte characters."
  (apply #'string (mapcar #'unibyte-char-to-multibyte bytes)))

(defun test-mevedel-utilities--raw-byte-string-p (string)
  "Return non-nil when STRING contains raw byte characters."
  (catch 'found
    (dotimes (index (length string))
      (when (eq (char-charset (aref string index)) 'eight-bit)
        (throw 'found t)))
    nil))

(mevedel-deftest mevedel--normalize-message-text ()
  ,test
  (test)

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
    (should-not (test-mevedel-utilities--raw-byte-string-p normalized))))

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
        (should-not (get-text-property (point) 'gptel))
        (should-not (get-text-property (point) 'response))
        (should-not (get-text-property (point) 'invisible))
        (should-not (get-text-property (point) 'front-sticky))
        (forward-char 1)))))

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
  (should-error (mevedel--tag-query-prefix-from-infix '(or (or(foo and bar))))))

(provide 'test-mevedel-utilities)
;;; test-mevedel-utilities.el ends here
