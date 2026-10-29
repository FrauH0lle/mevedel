;;; test-mevedel-memory-store-admission.el -- Authenticated review input -*- lexical-binding: t -*-

;;; Commentary:

;; Actual model admission determines host-generated disposal and survives
;; interrupted publication.  Captured scope is not an admission record.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-journal-claim)
(require 'mevedel-memory-notes)
(require 'mevedel-memory-scope)
(require 'mevedel-memory-store)
(require 'mevedel-session-control-fs)
(require 'mevedel-workspace-identity)

(mevedel-deftest mevedel-memory-store--admitted-notes-p ()
  ,test
  (test)
  :doc "actual input IDs must be a unique list within the captured note scope"
  (let ((scope '(:roots (("notes-root" :kind notes
                         :notes (("a.md" :id "work://shared/a.md")
                                 ("b.md" :id "work://shared/b.md")
                                 ("c.md" :id "work://shared/c.md")))))))
    (should (mevedel-memory-store--admitted-notes-p scope nil))
    (should (mevedel-memory-store--admitted-notes-p nil nil))
    (should (mevedel-memory-store--admitted-notes-p scope '("work://shared/b.md" "work://shared/a.md")))
    (dolist (invalid '("work://shared/a.md" ["work://shared/a.md"]
                       ("work://shared/a.md" . "work://shared/b.md")
                       ("work://shared/a.md" "work://shared/a.md")
                       ("work://shared/a.md" 7) ("foreign/a.md")))
      (should-not (mevedel-memory-store--admitted-notes-p scope invalid)))))

(mevedel-deftest mevedel-memory-store-accept/admission
    (:vars* ((root (file-name-as-directory (file-truename (make-temp-file "memory-admission-" t))))
             (workspace (mevedel-workspace--create :root root))
             (shared (file-name-concat root ".mevedel" "shared"))
             (mevedel-memory-dirs nil)
             (ids '("work://shared/b.md" "work://shared/a.md"))
             scope claim prepared)
     :before-each
     ((mevedel-workspace-identity-ensure root)
      (make-directory shared t)
      (dolist (name '("a.md" "b.md" "c.md"))
        (let ((path (file-name-concat shared name)))
          (write-region "Temporary source.\n" nil path nil 'silent)
          (set-file-times path (seconds-to-time (- (float-time) (* 15 86400))))))
      (setq scope (mevedel-memory-scope-capture workspace))
      (let ((capture (mevedel-memory-notes-capture workspace nil)))
        (should (= 3 (plist-get capture :notes)))
        (push (plist-get capture :root) (plist-get scope :roots)))
      (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)
            prepared (mevedel-memory-store-prepare workspace claim scope nil "")))
     :after-each ((when claim (mevedel-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t)))
  (cl-labels
      ((reply ()
         (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n"
                 "## Remove\n- none\n## Instructions\n- none\n## No action\n- none"))
       (accept (text &optional input)
         (mevedel-memory-store-accept workspace prepared text nil "test:model" nil input))
       (readback () (mevedel-memory-store-accepted workspace (plist-get prepared :id))))
    ,test)
  (test)
  :doc "a successful no-action review disposes exactly the two admitted notes, not all three captured"
  (let* ((accepted (accept (reply) ids))
         (proposals (plist-get accepted :proposals)))
    (should (= 3 (length (mevedel-memory-notes-index (plist-get prepared :scope)))))
    (should (equal ids (plist-get accepted :admitted-notes)))
    (should (= 2 (length proposals)))
    (should (equal ids (mapcar (lambda (item) (plist-get item :note)) proposals)))
    (should (cl-every (lambda (item) (eq 'note-delete (plist-get item :action))) proposals))
    (should-not (plist-member (car proposals) :reason))
    (should-not (plist-member (car proposals) :disposition))
    (should (equal "- none" (plist-get accepted :no-action)))
    (should-not (mevedel-journal-store-entries root))
    ;; Restart-readable immutable input metadata exists before publication.
    (should (equal accepted (readback)))
    (should-not (plist-member (car (plist-get (readback) :proposals)) :reason))
    (should-not (plist-member (car (plist-get (readback) :proposals)) :disposition)))
  :doc "a rejected review cannot create accepted disposal or change any source"
  (progn
    (should-error (accept (string-replace "## Promote\n- none" "## Promote\nMalformed" (reply)) ids))
    (should-not (readback))
    (should-not (mevedel-journal-claim-outcome claim))
    (dolist (name '("a.md" "b.md" "c.md"))
      (should (file-exists-p (file-name-concat shared name)))))
  :doc "successful review retains exact input and generated disposal through publication recovery"
  (let* ((accepted (accept (reply) ids))
         (pass (plist-get prepared :id)))
    (should (= 2 (length (plist-get accepted :proposals))))
    (should (equal ids (plist-get (readback) :admitted-notes)))
    (should-not (mevedel-journal-store-entries root))
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (should (mevedel-memory-store-recover workspace))
    (should (equal ids (plist-get (mevedel-memory-store-accepted workspace pass) :admitted-notes)))
    (should (= 1 (length (mevedel-journal-store-entries root))))
    (dolist (name '("a.md" "b.md" "c.md"))
      (should (file-exists-p (file-name-concat shared name)))))
  :doc "housekeeping nil input is explicit and never inferred from the captured notes"
  (let ((accepted (accept nil)))
    (should (plist-member accepted :admitted-notes))
    (should-not (plist-get accepted :admitted-notes))
    (should-not (plist-get accepted :proposals))
    (should (equal accepted (readback))))
  :doc "duplicate or uncaptured actual input IDs fail before any accepted bundle exists"
  (dolist (invalid '("work://shared/a.md" ["work://shared/a.md"]
                     ("work://shared/a.md" "work://shared/a.md")
                     ("work://shared/a.md" "work://shared/outside.md")))
    (should-error (accept (reply) invalid))
    (should-not (mevedel-journal-claim-outcome claim))
    (should-not (file-exists-p (file-name-concat (mevedel-memory-store--directory workspace (plist-get prepared :id))
                                               "accepted.el")))))

(mevedel-deftest mevedel-memory-store-accept-proposals/admission
    (:vars* ((root (make-temp-file "memory-structured-admission-" t))
             (workspace (mevedel-workspace--create :root root))
             (mevedel-memory-dirs nil)
             (scope (progn (mevedel-workspace-identity-ensure root)
                           (mevedel-memory-scope-capture workspace)))
             (claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
             (prepared (mevedel-memory-store-prepare workspace claim scope nil "")))
     :after-each ((mevedel-journal-claim-settle claim 'cancelled "") (delete-directory root t)))
  ,test
  (test)
  :doc "structured user input cannot manufacture admission through parsed metadata"
  (let ((accepted (mevedel-memory-store-accept-proposals
                   workspace prepared
                   '(:proposals nil :no-action "- User action." :admitted-notes ("work://shared/forged.md"))
                   nil "user" nil)))
    (should (plist-member accepted :admitted-notes))
    (should-not (plist-get accepted :admitted-notes)))
  :doc "parsed metadata cannot manufacture a shared-note disposal"
  (progn
    (should-error
     (mevedel-memory-store-accept-proposals
      workspace prepared '(:proposals ((:action note-delete :note "work://shared/forged.md")))
      nil "user" nil))
    (should-not (mevedel-journal-claim-outcome claim))
    (should-not (mevedel-memory-store-accepted workspace (plist-get prepared :id))))
  :doc "invalid explicit admission cannot create a bundle or settle ownership"
  (let ((parsed '(:proposals nil :no-action "- none")))
    (dolist (notes '(7 ("work://shared/outside.md") ("work://shared/a.md" "work://shared/a.md")))
      (should-error (mevedel-memory-store-accept-proposals workspace prepared parsed nil "test:model" nil notes))
      (should-not (mevedel-journal-claim-outcome claim))
      (should-not (file-exists-p (file-name-concat (mevedel-memory-store--directory workspace (plist-get prepared :id))
                                                 "accepted.el"))))))

(mevedel-deftest mevedel-memory-store-accepted/admission ()
  ,test
  (test)
  :doc "even correctly fenced bundles require current admission schema and valid captured IDs"
  (let* ((root (make-temp-file "memory-admission-loader-" t))
         (workspace (mevedel-workspace--create :root root))
         (mevedel-memory-dirs nil)
         (scope (progn (mevedel-workspace-identity-ensure root)
                       (mevedel-memory-scope-capture workspace)))
         claim)
    (unwind-protect
        (dolist (admission '(missing ("work://shared/outside.md") ["work://shared/outside.md"]
                            ("work://shared/outside.md" "work://shared/outside.md") nil))
          (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
          (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
                 (pass (plist-get prepared :id))
                 (review (list :pass-id pass :workspace (plist-get scope :workspace)
                               :created (mevedel-journal-store-timestamp (float-time))
                               :model "test:model" :focus "" :digests nil :proposals nil :references nil))
                 (data (append (list :prepared-hash (plist-get prepared :hash) :review review
                                     :proposals nil :no-action "- none")
                               (unless (eq admission 'missing) (list :admitted-notes admission))))
                 (text (mevedel-memory-store--print data))
                 (path (file-name-concat (mevedel-memory-store--directory workspace pass) "accepted.el")))
            ;; Create each private fixture once and fence its real byte hash;
            ;; no existing bundle or old format is rewritten or adapted.
            (should (mevedel-session-control-fs-create-file path text))
            (should (mevedel-journal-claim-settle claim 'completed (secure-hash 'sha256 text)))
            (if admission
                (should-error (mevedel-memory-store-accepted workspace pass)
                              :type 'error)
              (should (plist-member (mevedel-memory-store-accepted workspace pass) :admitted-notes)))))
      (when claim (mevedel-journal-claim-settle claim 'cancelled ""))
      (delete-directory root t))))

(provide 'test-mevedel-memory-store-admission)
;;; test-mevedel-memory-store-admission.el ends here
