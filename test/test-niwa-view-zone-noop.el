;;; test-niwa-view-zone-noop.el --- Unchanged zone tests -*- lexical-binding: t -*-

;;; Commentary:

;; Unchanged rendered text and an absent zone need no viewport transaction,
;; but orphaned text and text-property changes must still take the ordinary
;; reconciliation path.

;;; Code:

(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'niwa-view-viewport)
(require 'niwa-view-zone)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro niwa-view-zone-noop-test--counting (&rest body)
  "Run BODY counting logical viewport captures and restores; return (CAPTURES RESTORES)."
  (declare (indent 0) (debug t))
  `(let ((captures 0) (restores 0)
         (capture (symbol-function 'niwa-view-zone--capture-logical-state))
         (restore (symbol-function 'niwa-view-zone--restore-logical-state)))
     (cl-letf (((symbol-function 'niwa-view-zone--capture-logical-state)
                (lambda (&rest args)
                  (cl-incf captures) (apply capture args)))
               ((symbol-function 'niwa-view-zone--restore-logical-state)
                (lambda (&rest args)
                  (cl-incf restores) (apply restore args))))
       ,@body)
     (list captures restores)))

(defun niwa-view-zone-noop-test--caret-offset ()
  "Return the composer caret offset from its draft start.
The current buffer is the owning view."
  (let ((view (current-buffer)))
    (with-current-buffer (niwa-view-composer-buffer view)
      (- (point) (niwa-view-composer-draft-start view)))))

(niwa-deftest niwa-view-zone-unchanged-viewport ()
  ,test
  (test)
  :doc "unchanged text skips view capture and restore without moving the draft"
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let* ((fragments (list (list :namespace 'status :id 'tasks
                                    :body (propertize "Tasks\n" 'fontified t))))
             (region (niwa-view-zone-reconcile
                      'status niwa-view--status-marker
                      niwa-view--status-marker fragments))
             (text (buffer-substring (point-min) (point-max)))
             (tick (buffer-chars-modified-tick))
             (position (point))
             (start (window-start))
             (vscroll (window-vscroll nil t))
             (captures 0) (restores 0)
             (capture (symbol-function 'niwa-view-zone--capture-logical-state))
             (restore (symbol-function 'niwa-view-zone--restore-logical-state)))
        (cl-letf (((symbol-function 'niwa-view-zone--capture-logical-state)
                   (lambda (&rest args)
                     (cl-incf captures) (apply capture args)))
                  ((symbol-function 'niwa-view-zone--restore-logical-state)
                   (lambda (&rest args)
                     (cl-incf restores) (apply restore args))))
          (should (eq region (niwa-view-zone-reconcile
                              'status niwa-view--status-marker
                              niwa-view--status-marker fragments))))
        (should (equal-including-properties
                 text (buffer-substring (point-min) (point-max))))
        (should (= tick (buffer-chars-modified-tick)))
        (should (= position (point)))
        (should (= start (window-start)))
        (should (= vscroll (window-vscroll nil t)))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
        (should (= captures 0))
        (should (= restores 0)))))

  :doc "property-only updates are not treated as unchanged"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (dolist (face '(bold italic))
        (let ((region (niwa-view-zone-reconcile
                       'status niwa-view--status-marker
                       niwa-view--status-marker
                       (list (list :namespace 'status :id 'tasks
                                   :body (propertize "Tasks" 'face face))))))
          (should (eq face (get-text-property (overlay-start region) 'face)))
          (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
          (should (= 4 (niwa-view-zone-noop-test--caret-offset)))))))

  :doc "unchanged live text does not conceal orphaned text before the region"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let* ((fragments '((:namespace status :id tasks :body "Tasks")))
             (region (niwa-view-zone-reconcile
                      'status niwa-view--status-marker
                      niwa-view--status-marker fragments)))
        (let ((inhibit-read-only t) (inhibit-modification-hooks t))
          (save-excursion
            (goto-char (point-min))
            (insert (propertize "orphan\n" 'niwa-view-zone-namespace 'status
                                'niwa-view-zone-region 'dead))
            (move-overlay region (point) (overlay-end region))))
        (niwa-view-zone-reconcile
         'status niwa-view--status-marker niwa-view--status-marker fragments)
        (should-not (string-match-p "orphan" (buffer-string)))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
        (should (= 4 (niwa-view-zone-noop-test--caret-offset))))))

  :doc "an absent zone skips view capture and restore without changing text"
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (should-not (niwa-view-zone-region 'status))
      (let ((text (buffer-substring (point-min) (point-max)))
            (tick (buffer-modified-tick))
            (position (point))
            (start (window-start)))
        (should (equal '(0 0)
                       (niwa-view-zone-noop-test--counting
                         (should-not (niwa-view-zone-reconcile
                                      'status niwa-view--status-marker
                                      niwa-view--status-marker nil)))))
        (should (equal-including-properties
                 text (buffer-substring (point-min) (point-max))))
        (should (= tick (buffer-modified-tick)))
        (should (= position (point)))
        (should (= start (window-start)))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))))

  :doc "stale text of an absent zone is still removed, with the draft kept"
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
      ;; Lost local state: text survives while the region table forgot it.
      (let ((inhibit-read-only t) (inhibit-modification-hooks t))
        (save-excursion
          (goto-char niwa-view--status-marker)
          (insert (propertize "stale status\n" 'niwa-view-zone-namespace 'status
                              'niwa-view-zone-region 'dead))))
      (should-not (niwa-view-zone-region 'status))
      (should (equal '(1 1)
                     (niwa-view-zone-noop-test--counting
                       (should-not (niwa-view-zone-reconcile
                                    'status niwa-view--status-marker
                                    niwa-view--status-marker nil)))))
      (should-not (string-match-p "stale status" (buffer-string)))
      (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
      (should (= 4 (niwa-view-zone-noop-test--caret-offset))))))

(niwa-deftest niwa-view-zone-absent-stream-update
  (:doc "a stream update runs one viewport transaction without an empty status transaction")
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (niwa-view-test--insert-data data-buf "Earlier answer.\n" 'response)
      (niwa-view--full-rerender)
      (niwa-view-stream-begin-turn
       (niwa-view--history-insertion-marker)
       (with-current-buffer data-buf (point-max)))
      (niwa-view-test--insert-data data-buf "Then" 'response)
      (niwa-view--render-stream-update data-buf)
      (niwa-view-test--insert-composer-draft "> draft" 2)
      (should-not (niwa-view-zone-region 'status))
      (dotimes (_ 3)
        (niwa-view-test--insert-data data-buf " word" 'response)
        (let ((writes 0)
              (preserve (symbol-function 'niwa-view--call-preserving-window-state)))
          (cl-letf (((symbol-function 'niwa-view--call-preserving-window-state)
                     (lambda (&rest args)
                       (cl-incf writes) (apply preserve args))))
            (should (equal '(0 0)
                           (niwa-view-zone-noop-test--counting
                             (niwa-view--render-stream-update data-buf)))))
          (should (= 1 writes))))
      (should (string-search "Then word word word" (buffer-string)))
      (should (equal "> draft" (niwa-view-composer-draft-text))))))

(niwa-deftest niwa-view-zone-absent-proof
  (:doc "one scan proves a zone absent until a region or lost state discards it")
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (niwa-view-test--insert-data data-buf "Earlier answer.\n" 'response)
      (niwa-view--full-rerender)
      (niwa-view-stream-begin-turn
       (niwa-view--history-insertion-marker)
       (with-current-buffer data-buf (point-max)))
      (niwa-view-test--insert-data data-buf "Then" 'response)
      (niwa-view--render-stream-update data-buf)
      (niwa-view-test--insert-composer-draft "> draft" 2)
      (cl-flet ((stale ()
                  (let ((inhibit-read-only t) (inhibit-modification-hooks t))
                    (save-excursion
                      (goto-char niwa-view--status-marker)
                      (insert (propertize "stale status\n"
                                          'niwa-view-zone-namespace 'status
                                          'niwa-view-zone-region 'dead)))))
                (stale-p () (string-search "stale status" (buffer-string)))
                (reconcile ()
                  (niwa-view-zone-reconcile
                   'status niwa-view--status-marker
                   niwa-view--status-marker nil)))
        ;; The stream update's empty status writer proved the zone absent.
        (niwa-view-test--insert-data data-buf " word" 'response)
        (niwa-view--render-stream-update data-buf)
        (should-not (niwa-view-zone-region 'status))
        ;; The proof is reused: text written behind the zone's back is not
        ;; rescanned, which is what keeps the absent check off the buffer size.
        (stale)
        (should-not (reconcile))
        (should (stale-p))
        ;; Losing the table discards the proof, so the stale text goes.
        (setq niwa-view-zone--regions nil)
        (should-not (reconcile))
        (should-not (stale-p))
        ;; A live region replaces the proof; clearing it leaves no zone text.
        (should-not (reconcile))
        (should (niwa-view-zone-reconcile
                 'status niwa-view--status-marker niwa-view--status-marker
                 '((:namespace status :id live :body "Live status"))))
        (should (string-search "Live status" (buffer-string)))
        (stale)
        (should-not (reconcile))
        (should-not (niwa-view-zone-region 'status))
        (should-not (string-search "Live status" (buffer-string)))
        (should-not (stale-p)))
      (should (string-search "Then word" (buffer-string)))
      (should (equal "> draft" (niwa-view-composer-draft-text)))
      (should (= 2 (niwa-view-zone-noop-test--caret-offset))))))

(provide 'test-niwa-view-zone-noop)
;;; test-niwa-view-zone-noop.el ends here
