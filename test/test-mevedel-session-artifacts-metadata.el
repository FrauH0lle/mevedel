;;; test-mevedel-session-artifacts-metadata.el --- Leading drawer writes -*- lexical-binding: t -*-

;;; Commentary:

;; The direct property writers rewrite the leading Org drawer without Org
;; parsing.  Replacing a property must not remove and recreate the drawer:
;; that collapsed the transcript body's origin marker onto the buffer start,
;; where the recreated drawer is a body edit to every change proof.  Each case
;; compares final bytes with the original algorithm, which is kept below only
;; as the oracle.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'mevedel-session-artifacts)
(require 'mevedel-transcript-source)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-metadata--old-put (property value)
  "Set PROPERTY to VALUE as the writer did before it kept the drawer."
  (mevedel-session-artifacts-property-delete-direct property)
  (when-let* ((region (mevedel-session-artifacts--ensure-property-drawer)))
    (save-excursion
      (save-restriction
        (widen)
        (let ((inhibit-read-only t))
          (goto-char (car region))
          (forward-line 1)
          (insert (format ":%s: %s\n" property value)))))))

(defconst test-mevedel-metadata--starts
  '("" "body\n"
    ":PROPERTIES:\n:END:\nbody\n"
    ":PROPERTIES:\n:GPTEL_BOUNDS: old\n:END:\nbody\n"
    ":PROPERTIES:\n:OTHER: 1\n:GPTEL_BOUNDS: old\n:KEEP: 2\n:END:\nbody\n"
    ":PROPERTIES:\n:GPTEL_BOUNDS: old\n:GPTEL_BOUNDS+: more\n:END:\nbody\n"
    ":PROPERTIES:\n:OTHER: 1\n:END:\nbody\n")
  "Buffer contents the writers meet.")

(defun test-mevedel-metadata--run (start function)
  "Return the contents after FUNCTION runs in a buffer holding START."
  (with-temp-buffer
    (insert start)
    (funcall function)
    (buffer-string)))

(mevedel-deftest mevedel-session-artifacts-property-delete-direct ()
  ,test
  (test)
  :doc "removes the property with its continuation lines and keeps the others"
  (should (equal ":PROPERTIES:\n:OTHER: 1\n:KEEP: 2\n:END:\nbody\n"
                 (test-mevedel-metadata--run
                  ":PROPERTIES:\n:OTHER: 1\n:GPTEL_BOUNDS: old\n:KEEP: 2\n:END:\nbody\n"
                  (lambda ()
                    (should (mevedel-session-artifacts-property-delete-direct
                             "GPTEL_BOUNDS"))))))
  :doc "removing the last property removes the empty drawer"
  (should (equal "body\n"
                 (test-mevedel-metadata--run
                  ":PROPERTIES:\n:GPTEL_BOUNDS: old\n:END:\nbody\n"
                  (lambda ()
                    (mevedel-session-artifacts-property-delete-direct
                     "GPTEL_BOUNDS")))))
  :doc "KEEP-DRAWER leaves the empty drawer in place"
  (should (equal ":PROPERTIES:\n:END:\nbody\n"
                 (test-mevedel-metadata--run
                  ":PROPERTIES:\n:GPTEL_BOUNDS: old\n:END:\nbody\n"
                  (lambda ()
                    (should (mevedel-session-artifacts-property-delete-direct
                             "GPTEL_BOUNDS" t))))))
  :doc "an absent property or drawer changes nothing and reports nothing"
  (dolist (start '("" "body\n" ":PROPERTIES:\n:OTHER: 1\n:END:\nbody\n"))
    (should (equal start
                   (test-mevedel-metadata--run
                    start
                    (lambda ()
                      (should-not (mevedel-session-artifacts-property-delete-direct
                                   "GPTEL_BOUNDS" t))
                      (should-not (mevedel-session-artifacts-property-delete-direct
                                   "GPTEL_BOUNDS")))))))
  :doc "the drawer stays when a continuation of the property is the only content"
  (should (equal ":PROPERTIES:\n:END:\nbody\n"
                 (test-mevedel-metadata--run
                  ":PROPERTIES:\n:GPTEL_BOUNDS: old\n:GPTEL_BOUNDS+: more\n:END:\nbody\n"
                  (lambda ()
                    (mevedel-session-artifacts-property-delete-direct
                     "GPTEL_BOUNDS" t))))))

(mevedel-deftest mevedel-session-artifacts--property-put-direct ()
  ,test
  (test)
  :doc "final bytes equal the original delete and recreate algorithm"
  (dolist (start test-mevedel-metadata--starts)
    (dolist (value '("((response (1 5)))" "x"))
      (should (equal (test-mevedel-metadata--run
                      start
                      (lambda ()
                        (test-mevedel-metadata--old-put "GPTEL_BOUNDS" value)))
                     (test-mevedel-metadata--run
                      start
                      (lambda ()
                        (mevedel-session-artifacts--property-put-direct
                         "GPTEL_BOUNDS" value)))))))
  :doc "replacing the only property keeps the body origin and the change proof"
  (with-temp-buffer
    (insert ":PROPERTIES:\n:GPTEL_BOUNDS: old\n:END:\nbody text\n")
    (let* ((origin (mevedel-transcript--skip-leading-properties-drawer (point-min)))
           (source (mevedel-transcript-source-create origin)))
      (mevedel-transcript-source-accept source)
      (mevedel-transcript-source-call-with-metadata-change
       (lambda ()
         (mevedel-session-artifacts--property-put-direct
          "GPTEL_BOUNDS" "a much longer replacement value")))
      (should-not (mevedel-transcript-source-change source))
      (should (= (mevedel-transcript--skip-leading-properties-drawer (point-min))
                 (marker-position (mevedel-transcript-source-origin source))))
      (should (equal "body text\n"
                     (buffer-substring (marker-position
                                        (mevedel-transcript-source-origin source))
                                       (point-max))))
      (mevedel-transcript-source-release source)))
  :doc "the original algorithm collapsed the origin, which is the regression"
  (with-temp-buffer
    (insert ":PROPERTIES:\n:GPTEL_BOUNDS: old\n:END:\nbody text\n")
    (let* ((origin (mevedel-transcript--skip-leading-properties-drawer (point-min)))
           (source (mevedel-transcript-source-create origin)))
      (mevedel-transcript-source-accept source)
      (mevedel-transcript-source-call-with-metadata-change
       (lambda () (test-mevedel-metadata--old-put "GPTEL_BOUNDS" "replacement")))
      (should (eql 0 (mevedel-transcript-source-change source)))
      (mevedel-transcript-source-release source)))
  :doc "creating the first drawer is a body edit, as inserting at the origin must be"
  (with-temp-buffer
    (insert "body text\n")
    (let ((source (mevedel-transcript-source-create (point-min))))
      (mevedel-transcript-source-accept source)
      (mevedel-transcript-source-call-with-metadata-change
       (lambda ()
         (mevedel-session-artifacts--property-put-direct "GPTEL_BOUNDS" "v")))
      (should (eql 0 (mevedel-transcript-source-change source)))
      (should (equal ":PROPERTIES:\n:GPTEL_BOUNDS: v\n:END:\nbody text\n"
                     (buffer-string)))
      (mevedel-transcript-source-release source))))

(provide 'test-mevedel-session-artifacts-metadata)

;;; test-mevedel-session-artifacts-metadata.el ends here
