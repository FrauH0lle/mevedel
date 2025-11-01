;;; tests/test-mevedel-utilities.el -- Unit tests for mevedel-utilities.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'mevedel)
(load (file-name-concat (file-name-directory (or buffer-file-name load-file-name)) "helpers"))


(describe "mevedel--tag-query-prefix-from-infix:"
  (describe "Valid infix to prefix conversions"
    (it "converts 'foo and not bar or baz'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo and not bar or baz))
              :to-equal '(or (and foo (not bar)) baz)))

    (it "converts 'john or not [jane]'"
      (expect (mevedel--tag-query-prefix-from-infix '(john or not [jane]))
              :to-equal '(or john (not [jane]))))

    (it "converts 'alice and bob and charlie'"
      (expect (mevedel--tag-query-prefix-from-infix '(alice and bob and charlie))
              :to-equal '(and alice bob charlie)))

    (it "converts single tag 'foo'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo))
              :to-equal 'foo))

    (it "converts 'foo bar baz not john'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo bar baz not john))
              :to-equal '(and foo bar baz (not john))))

    (it "converts '((foo))'"
      (expect (mevedel--tag-query-prefix-from-infix '((foo)))
              :to-equal 'foo))

    (it "converts '(((foo)))'"
      (expect (mevedel--tag-query-prefix-from-infix '(((foo))))
              :to-equal 'foo))

    (it "converts '(((foo foo foo)))'"
      (expect (mevedel--tag-query-prefix-from-infix '(((foo foo foo))))
              :to-equal '(and foo foo foo)))

    (it "converts 'not bar and baz'"
      (expect (mevedel--tag-query-prefix-from-infix '(not bar and baz))
              :to-equal '(and (not bar) baz)))

    (it "converts 'bar or bar or baz'"
      (expect (mevedel--tag-query-prefix-from-infix '(bar or bar or baz))
              :to-equal '(or bar bar baz)))

    (it "converts 'bar bar or baz'"
      (expect (mevedel--tag-query-prefix-from-infix '(bar bar or baz))
              :to-equal '(or (and bar bar) baz)))

    (it "converts empty list to nil"
      (expect (mevedel--tag-query-prefix-from-infix '())
              :to-equal nil))

    (it "converts '((()))' to nil"
      (expect (mevedel--tag-query-prefix-from-infix '((())))
              :to-equal nil))

    (it "converts 'danny and (joey and boris)'"
      (expect (mevedel--tag-query-prefix-from-infix '(danny and (joey and boris)))
              :to-equal '(and danny (and joey boris))))

    (it "converts '((danny and (joey and boris)) and (foo or bar))'"
      (expect (mevedel--tag-query-prefix-from-infix '((danny and (joey and boris)) and (foo or bar)))
              :to-equal '(and (and danny (and joey boris)) (or foo bar))))

    (it "converts '((alice or bob) and (charlie or dave))'"
      (expect (mevedel--tag-query-prefix-from-infix '((alice or bob) and (charlie or dave)))
              :to-equal '(and (or alice bob) (or charlie dave))))

    (it "converts '((alice and bob) or (charlie and dave))'"
      (expect (mevedel--tag-query-prefix-from-infix '((alice and bob) or (charlie and dave)))
              :to-equal '(or (and alice bob) (and charlie dave)))))

  (describe "Invalid infix queries"
    (it "rejects '(and)'"
      (expect (mevedel--tag-query-prefix-from-infix '(and))
              :to-throw))

    (it "rejects '(or)'"
      (expect (mevedel--tag-query-prefix-from-infix '(or))
              :to-throw))

    (it "rejects '(not)'"
      (expect (mevedel--tag-query-prefix-from-infix '(not))
              :to-throw))

    (it "rejects '(and foo)'"
      (expect (mevedel--tag-query-prefix-from-infix '(and foo))
              :to-throw))

    (it "rejects '(or foo)'"
      (expect (mevedel--tag-query-prefix-from-infix '(or foo))
              :to-throw))

    (it "rejects '(and foo or bar)'"
      (expect (mevedel--tag-query-prefix-from-infix '(and foo or bar))
              :to-throw))

    (it "rejects '(or and foo bar)'"
      (expect (mevedel--tag-query-prefix-from-infix '(or and foo bar))
              :to-throw))

    (it "rejects '(and (or foo) bar)'"
      (expect (mevedel--tag-query-prefix-from-infix '(and (or foo) bar))
              :to-throw))

    (it "rejects '(foo (or bar))'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo (or bar)))
              :to-throw))

    (it "rejects '(foo or (and bar))'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo or (and bar)))
              :to-throw))

    (it "rejects '(foo bar and (not))'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo bar and (not)))
              :to-throw))

    (it "rejects '((or bar))'"
      (expect (mevedel--tag-query-prefix-from-infix '((or bar)))
              :to-throw))

    (it "rejects '((and foo))'"
      (expect (mevedel--tag-query-prefix-from-infix '((and foo)))
              :to-throw))

    (it "rejects '(foo or (and))'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo or (and)))
              :to-throw))

    (it "rejects '(or ())'"
      (expect (mevedel--tag-query-prefix-from-infix '(or ()))
              :to-throw))

    (it "rejects '(foo or not)'"
      (expect (mevedel--tag-query-prefix-from-infix '(foo or not))
              :to-throw))

    (it "rejects '(and (and foo bar))'"
      (expect (mevedel--tag-query-prefix-from-infix '(and (and foo bar)))
              :to-throw))

    (it "rejects '(or (or(foo and bar)))'"
      (expect (mevedel--tag-query-prefix-from-infix '(or (or(foo and bar))))
              :to-throw))))

(provide 'test-mevedel-utilities)
;;; test-mevedel-utilities.el ends here
