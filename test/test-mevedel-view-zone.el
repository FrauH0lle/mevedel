;;; test-mevedel-view-zone.el --- Tests for managed view zones -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for fixed managed view zones, including reconciliation,
;; recovery, collapse state, navigation, and region queries.

;;; Code:

(require 'ert)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view-zone)
(require 'mevedel-view)

(defvar-local mevedel-view--agent-transcript-p nil)
(defvar-local mevedel-view--input-marker nil)
(defvar-local mevedel-view--interaction-marker nil)
(defvar-local mevedel-view--status-marker nil)

(defun mevedel-view-zone-test--setup-markers ()
  "Set all view zone markers at point-min in the current buffer."
  (setq-local mevedel-view--status-marker (copy-marker (point-min) t))
  (setq-local mevedel-view--interaction-marker (copy-marker (point-min) t))
  (setq-local mevedel-view--input-marker (copy-marker (point-min) nil)))

(mevedel-deftest mevedel-view-zone-reconcile ()
  ,test
  (test)
  :doc "reconciles a fixed zone while preserving lower marker ordering"
  (with-temp-buffer
    (insert "> draft")
    (mevedel-view-zone-test--setup-markers)
    (mevedel-view-zone-reconcile
     'status (point-min) (point-min)
     '((:namespace status :id tasks :body "Tasks")))
    (should (equal "Tasks\n> draft" (buffer-substring-no-properties
                                      (point-min) (point-max))))
    (should (= (point-min) (marker-position mevedel-view--status-marker)))
    (should (= (length "Tasks\n")
               (- (marker-position mevedel-view--interaction-marker)
                  (point-min))))
    (should (= (marker-position mevedel-view--interaction-marker)
               (marker-position mevedel-view--input-marker))))

  :doc "rejects unknown zones as programming errors"
  (with-temp-buffer
    (should-error
     (mevedel-view-zone-reconcile 'custom (point-min) (point-min) nil)
     :type 'error))

  :doc "preserves point by fragment identity and offset"
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (mevedel-view-zone-reconcile
     'interaction (point-min) (point-min)
     '((:namespace interaction :id prompt :body "abcde")))
    (goto-char (+ (point-min) 3))
    (should (eq (char-after) ?d))
    (mevedel-view-zone-reconcile
     'interaction (point-min) (point-min)
     '((:namespace interaction :id prompt :body "uvwxyz")))
    (should (eq (char-after) ?x)))

  :doc "does not mutate an unchanged managed fragment"
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (mevedel-view-zone-reconcile
     'status (point-min) (point-min)
     '((:namespace status :id tasks :body "Tasks")))
    (let ((changes 0))
      (add-hook 'after-change-functions
                (lambda (&rest _ignore) (cl-incf changes)) nil t)
      (mevedel-view-zone-reconcile
       'status (point-min) (point-min)
       '((:namespace status :id tasks :body "Tasks")))
      (should (= 0 changes))
      (should (equal "Tasks\n" (buffer-substring-no-properties
                                  (point-min) (point-max))))))

  :doc "removes orphaned and duplicate stale zone text"
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (let ((orphan
           (mevedel-view-zone-reconcile
            'status (point-min) (point-min)
            '((:namespace status :id old :body "Old")))))
      (delete-overlay orphan)
      (mevedel-view-zone-clear 'status)
      (should (equal "" (buffer-string))))
    (let ((live
           (mevedel-view-zone-reconcile
            'status (point-min) (point-min)
            '((:namespace status :id live :body "Live")))))
      (goto-char (point-max))
      (insert (propertize "Stale\n"
                          'mevedel-view-zone-namespace 'status
                          'mevedel-view-zone-region (make-overlay 1 1)))
      (set-marker mevedel-view--input-marker (point-max))
      (mevedel-view-zone-reconcile
       'status (point-min) (point-min)
       '((:namespace status :id live :body "Live")))
      (should (eq live (mevedel-view-zone-region 'status)))
      (should (equal "Live\n" (buffer-string)))
      (let ((inhibit-read-only t)
            (tail-start (point-max)))
        (goto-char tail-start)
        (insert "Tail")
        (set-text-properties tail-start (point-max) nil))
      (set-marker mevedel-view--input-marker (point-max))
      (let* ((anchor (copy-marker (point-max) t))
             (bounds (mevedel-view-zone-fragment-bounds 'status 'live))
             (dead-owner (make-overlay 1 1)))
        (let ((inhibit-read-only t))
          (add-text-properties
           (plist-get bounds :start) (plist-get bounds :end)
           `(mevedel-view-zone-region ,dead-owner)))
        (mevedel-view-zone-reconcile
         'status anchor anchor
         '((:namespace status :id live :body "Live")))
        (set-marker anchor nil)
        (should (equal "TailLive\n" (buffer-string)))))))

(mevedel-deftest mevedel-view-zone-collapse-state-set-p
  (:doc "reports whether collapse state was explicitly stored")
  (with-temp-buffer
    (should-not (mevedel-view-zone-collapse-state-set-p 'section))
    (mevedel-view-zone-set-collapse-state 'section nil)
    (should (mevedel-view-zone-collapse-state-set-p 'section))))

(mevedel-deftest mevedel-view-zone-collapse-state
  (:doc "returns stored collapse state or the supplied default")
  (with-temp-buffer
    (should (mevedel-view-zone-collapse-state 'section t))
    (mevedel-view-zone-set-collapse-state 'section nil)
    (should-not (mevedel-view-zone-collapse-state 'section t))))

(mevedel-deftest mevedel-view-zone-set-collapse-state
  (:doc "normalizes stored collapse state to a boolean")
  (with-temp-buffer
    (mevedel-view-zone-set-collapse-state 'section 'yes)
    (should (eq t (mevedel-view-zone-collapse-state 'section)))))

(mevedel-deftest mevedel-view-zone-toggle-collapse-state
  (:doc "toggles and returns collapse state")
  (with-temp-buffer
    (should (mevedel-view-zone-toggle-collapse-state 'section nil))
    (should-not (mevedel-view-zone-toggle-collapse-state 'section nil))))

(mevedel-deftest mevedel-view-zone-toggle-collapsed
  (:doc "toggles the collapsible fragment at point")
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (mevedel-view-zone-reconcile
     'status (point-min) (point-min)
     '((:namespace status :id section :body "Body"
        :collapsible t :collapse-key section)))
    (goto-char (point-min))
    (should (mevedel-view-zone-toggle-collapsed))))

(mevedel-deftest mevedel-view-zone-next
  (:doc "moves to the next navigatable managed fragment")
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (mevedel-view-zone-reconcile
     'status (point-min) (point-min)
     '((:namespace status :id first :body "First" :navigatable t)
       (:namespace status :id second :body "Second" :navigatable t)))
    (goto-char (point-min))
    (mevedel-view-zone-next)
    (should (equal 'second
                   (plist-get (mevedel-view-zone-bounds-at) :id)))))

(mevedel-deftest mevedel-view-zone-previous
  (:doc "moves to the previous navigatable managed fragment")
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (mevedel-view-zone-reconcile
     'status (point-min) (point-min)
     '((:namespace status :id first :body "First" :navigatable t)
       (:namespace status :id second :body "Second" :navigatable t)))
    (goto-char (plist-get (mevedel-view-zone-fragment-bounds
                           'status 'second)
                          :start))
    (mevedel-view-zone-previous)
    (should (equal 'first
                   (plist-get (mevedel-view-zone-bounds-at) :id)))))

(mevedel-deftest mevedel-view-zone-region
  (:doc "returns the identity-backed live region")
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (let ((region
           (mevedel-view-zone-reconcile
            'progress (point-min) (point-min)
            '((:namespace progress :id request :body "Working")))))
      (should (eq region (mevedel-view-zone-region 'progress))))))

(mevedel-deftest mevedel-view-zone-start
  (:doc "returns the live zone start")
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (mevedel-view-zone-reconcile
     'history-live (point-min) (point-min)
     '((:namespace history-live :id call :body "Calling")))
    (should (= (point-min) (mevedel-view-zone-start 'history-live)))))

(mevedel-deftest mevedel-view-zone-fragment-bounds
  (:doc "finds a fragment through the managed-zone interface")
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (mevedel-view-zone-reconcile
     'status (point-min) (point-min)
     '((:namespace status :id tasks :body "Tasks")))
    (let ((bounds (mevedel-view-zone-fragment-bounds 'status 'tasks)))
      (should (= (point-min) (plist-get bounds :start)))
      (should (= (+ (point-min) (length "Tasks\n"))
                 (plist-get bounds :end))))))

(mevedel-deftest mevedel-view-zone-bounds-at
  (:doc "finds the managed fragment containing point")
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (mevedel-view-zone-reconcile
     'status (point-min) (point-min)
     '((:namespace status :id tasks :body "Tasks")))
    (goto-char (point-min))
    (should (equal 'tasks
                   (plist-get (mevedel-view-zone-bounds-at) :id)))))

(mevedel-deftest mevedel-view-zone-clear
  (:doc "removes only the selected managed zone")
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (mevedel-view-zone-reconcile
     'status (point-min) (point-min)
     '((:namespace status :id tasks :body "Tasks")))
    (mevedel-view-zone-clear 'status)
    (should (equal "" (buffer-string)))
    (should-not (mevedel-view-zone-region 'status))))

(mevedel-deftest mevedel-view-zone-forget
  (:doc "drops disposable region state after a larger redraw")
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (mevedel-view-zone-reconcile
     'progress (point-min) (point-min)
     '((:namespace progress :id request :body "Working")))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (mevedel-view-zone-forget 'progress)
    (should-not (mevedel-view-zone-region 'progress))))

(provide 'test-mevedel-view-zone)
;;; test-mevedel-view-zone.el ends here
