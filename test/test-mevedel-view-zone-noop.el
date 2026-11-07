;;; test-mevedel-view-zone-noop.el --- Unchanged zone tests -*- lexical-binding: t -*-

;;; Commentary:

;; Unchanged rendered text and an absent zone need no viewport transaction,
;; but orphaned text and text-property changes must still take the ordinary
;; reconciliation path.

;;; Code:

(require 'mevedel-view)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro mevedel-view-zone-noop-test--counting (&rest body)
  "Run BODY counting composer captures and restores; return (CAPTURES RESTORES)."
  (declare (indent 0) (debug t))
  `(let ((captures 0) (restores 0)
         (capture (symbol-function 'mevedel-view--composer-anchors))
         (restore (symbol-function 'mevedel-view--restore-composer-anchors)))
     (cl-letf (((symbol-function 'mevedel-view--composer-anchors)
                (lambda (&rest args)
                  (cl-incf captures) (apply capture args)))
               ((symbol-function 'mevedel-view--restore-composer-anchors)
                (lambda (&rest args)
                  (cl-incf restores) (apply restore args))))
       ,@body)
     (list captures restores)))

(mevedel-deftest mevedel-view-zone-unchanged-viewport ()
  ,test
  (test)
  :doc "unchanged text skips view capture and restore without moving the draft"
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let* ((fragments (list (list :namespace 'status :id 'tasks
                                    :body (propertize "Tasks\n" 'fontified t))))
             (region (mevedel-view-zone-reconcile
                      'status mevedel-view--status-marker
                      mevedel-view--status-marker fragments))
             (text (buffer-substring (point-min) (point-max)))
             (tick (buffer-chars-modified-tick))
             (position (point))
             (start (window-start))
             (vscroll (window-vscroll nil t))
             (captures 0) (restores 0)
             (capture (symbol-function 'mevedel-view--composer-anchors))
             (restore (symbol-function 'mevedel-view--restore-composer-anchors)))
        (cl-letf (((symbol-function 'mevedel-view--composer-anchors)
                   (lambda (&rest args)
                     (cl-incf captures) (apply capture args)))
                  ((symbol-function 'mevedel-view--restore-composer-anchors)
                   (lambda (&rest args)
                     (cl-incf restores) (apply restore args))))
          (should (eq region (mevedel-view-zone-reconcile
                              'status mevedel-view--status-marker
                              mevedel-view--status-marker fragments))))
        (should (equal-including-properties
                 text (buffer-substring (point-min) (point-max))))
        (should (= tick (buffer-chars-modified-tick)))
        (should (= position (point)))
        (should (= start (window-start)))
        (should (= vscroll (window-vscroll nil t)))
        (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
        (should (= captures 0))
        (should (= restores 0)))))

  :doc "property-only updates are not treated as unchanged"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (dolist (face '(bold italic))
        (let ((region (mevedel-view-zone-reconcile
                       'status mevedel-view--status-marker
                       mevedel-view--status-marker
                       (list (list :namespace 'status :id 'tasks
                                   :body (propertize "Tasks" 'face face))))))
          (should (eq face (get-text-property (overlay-start region) 'face)))
          (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
          (should (= (point) (+ 4 (mevedel-view--input-start))))))))

  :doc "unchanged live text does not conceal orphaned text before the region"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let* ((fragments '((:namespace status :id tasks :body "Tasks")))
             (region (mevedel-view-zone-reconcile
                      'status mevedel-view--status-marker
                      mevedel-view--status-marker fragments)))
        (let ((inhibit-read-only t) (inhibit-modification-hooks t))
          (save-excursion
            (goto-char (point-min))
            (insert (propertize "orphan\n" 'mevedel-view-zone-namespace 'status
                                'mevedel-view-zone-region 'dead))
            (move-overlay region (point) (overlay-end region))))
        (mevedel-view-zone-reconcile
         'status mevedel-view--status-marker mevedel-view--status-marker fragments)
        (should-not (string-match-p "orphan" (buffer-string)))
        (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
        (should (= (point) (+ 4 (mevedel-view--input-start)))))))

  :doc "an absent zone skips view capture and restore without changing text"
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (should-not (mevedel-view-zone-region 'status))
      (let ((text (buffer-substring (point-min) (point-max)))
            (tick (buffer-modified-tick))
            (position (point))
            (start (window-start)))
        (should (equal '(0 0)
                       (mevedel-view-zone-noop-test--counting
                         (should-not (mevedel-view-zone-reconcile
                                      'status mevedel-view--status-marker
                                      mevedel-view--status-marker nil)))))
        (should (equal-including-properties
                 text (buffer-substring (point-min) (point-max))))
        (should (= tick (buffer-modified-tick)))
        (should (= position (point)))
        (should (= start (window-start)))
        (should (equal "> draft\nsecond line" (mevedel-view--input-text))))))

  :doc "stale text of an absent zone is still removed, with the draft kept"
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
      ;; Lost local state: text survives while the region table forgot it.
      (let ((inhibit-read-only t) (inhibit-modification-hooks t))
        (save-excursion
          (goto-char mevedel-view--status-marker)
          (insert (propertize "stale status\n" 'mevedel-view-zone-namespace 'status
                              'mevedel-view-zone-region 'dead))))
      (should-not (mevedel-view-zone-region 'status))
      (should (equal '(1 1)
                     (mevedel-view-zone-noop-test--counting
                       (should-not (mevedel-view-zone-reconcile
                                    'status mevedel-view--status-marker
                                    mevedel-view--status-marker nil)))))
      (should-not (string-match-p "stale status" (buffer-string)))
      (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
      (should (= (point) (+ 4 (mevedel-view--input-start)))))))

(mevedel-deftest mevedel-view-zone-absent-stream-update
  (:doc "a stream update with an empty status zone runs one capture/restore pair")
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (mevedel-view-test--insert-data data-buf "Earlier answer.\n" 'response)
      (mevedel-view--full-rerender)
      (mevedel-view-stream-begin-turn
       (mevedel-view--history-insertion-marker)
       (with-current-buffer data-buf (point-max)))
      (mevedel-view-test--insert-data data-buf "Then" 'response)
      (mevedel-view--render-stream-update data-buf)
      (mevedel-view-test--insert-composer-draft "> draft" 2)
      (should-not (mevedel-view-zone-region 'status))
      (dotimes (_ 3)
        (mevedel-view-test--insert-data data-buf " word" 'response)
        ;; The live writer keeps its pair; the empty status writer needs none.
        (should (equal '(1 1)
                       (mevedel-view-zone-noop-test--counting
                         (mevedel-view--render-stream-update data-buf)))))
      (should (string-search "Then word word word" (buffer-string)))
      (should (equal "> draft" (mevedel-view--input-text))))))

(provide 'test-mevedel-view-zone-noop)
;;; test-mevedel-view-zone-noop.el ends here
