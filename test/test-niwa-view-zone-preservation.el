;;; test-niwa-view-zone-preservation.el --- Zone redraw invariants -*- lexical-binding: t; -*-

;;; Commentary:

;; Managed-zone redraws must preserve drafts, semantic positions, neighboring
;; zones and foreign overlays regardless of their text replacement strategy.

;;; Code:

(require 'niwa-mention-bindings)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-interaction)
(require 'niwa-view-stream)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun niwa-view-zone-preservation-test--caret-offset ()
  "Return the composer caret offset from its draft start.
The current buffer is the owning view."
  (let ((view (current-buffer)))
    (with-current-buffer (niwa-view-composer-buffer view)
      (- (point) (niwa-view-composer-draft-start view)))))

(niwa-deftest niwa-view-zone-redraw-invariants ()
  ,test
  (test)
  :doc "property changes preserve semantic anchors, composer point and disabled transcript undo"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let* ((render (lambda (face)
                       (niwa-view-zone-reconcile
                        'status niwa-view--status-marker niwa-view--status-marker
                        (list (list :namespace 'status :id 'row
                                    :body (propertize "status" 'face face
                                                      'niwa-view-zone-anchor 'row))))))
             (region (funcall render 'bold))
             (anchored-position (+ (overlay-start region) 2))
             (anchor (niwa-view-zone-position-anchor anchored-position))
             (position (niwa-view-zone-preservation-test--caret-offset)))
        (funcall render 'italic)
        (should (= anchored-position (niwa-view-zone-anchor-position anchor)))
        (should (eq 'italic (get-text-property anchored-position 'face)))
        (should (= position (niwa-view-zone-preservation-test--caret-offset)))
        (should (eq buffer-undo-list t))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))))

  :doc "an elapsed tick preserves a leading-> multiline draft, undo, zone anchors and foreign overlays"
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (should (niwa-view-composer-display (selected-window)))
      (let ((now (seconds-to-time 1000.25))
            (float (symbol-function 'float-time))
            (niwa-view-spinner-style 'static)
            (niwa-view-tool-spinner-style 'static)
            (niwa-view-spinner-power-policy 'full))
        (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _) t))
                  ((symbol-function 'current-time) (lambda () now))
                  ((symbol-function 'float-time)
                   (lambda (&optional time) (funcall float (or time now)))))
          (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
          (niwa-view-zone-reconcile
           'status niwa-view--status-marker niwa-view--status-marker
           '((:namespace status :id task :body "Task")))
          (niwa-view-zone-reconcile
           'interaction niwa-view--interaction-marker (niwa-view-transcript-tail-marker)
           '((:namespace interaction :id card :body "Question")))
          (niwa-view--start-spinner "Working...")
          (setq niwa-view--spinner-start-time (seconds-to-time 988.25))
          (niwa-view--ensure-request-progress)
          (setq niwa-view--spinner-last-second 1000)
          (let* ((interaction (niwa-view-zone-region 'interaction))
                 (line (make-overlay (overlay-start interaction) (overlay-end interaction)))
                 (position (niwa-view-zone-preservation-test--caret-offset))
                 (composer (niwa-view-composer-buffer view-buf))
                 undo
                 (anchored-position (+ (overlay-start interaction) 2))
                 (anchor (niwa-view-zone-position-anchor anchored-position))
                 (regions (mapcar (lambda (zone)
                                    (let ((other (niwa-view-zone-region zone)))
                                      (list other (overlay-start other) (overlay-end other))))
                                  '(status interaction))))
            (with-current-buffer composer
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (niwa-view-composer-insert "x" (point) view-buf)
              (setq undo buffer-undo-list))
            (setq now (seconds-to-time 1001.25))
            (unless noninteractive (redisplay t))
            (niwa-view--spinner-tick)
            (should (= (1+ position) (niwa-view-zone-preservation-test--caret-offset)))
            (should (equal "> drxaft\nsecond line" (niwa-view-composer-draft-text)))
            (should (= anchored-position (niwa-view-zone-anchor-position anchor)))
            (should (= (overlay-start interaction) (overlay-start line)))
            (should (= (overlay-end interaction) (overlay-end line)))
            (should (string-search "13s" (niwa-view-test--rail-text)))
            (dolist (state regions)
              (should (= (nth 1 state) (overlay-start (car state))))
              (should (= (nth 2 state) (overlay-end (car state)))))
            (with-current-buffer composer
              (should (equal undo buffer-undo-list))
              (undo-start)
              (undo-more 1))
            (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text))))
          (niwa-view--stop-spinner)))))

  :doc "panel text yanked into the composer is draft text, kept with its bindings through zone redraws"
  (dolist (recreate '(nil t))
    (niwa-view-test--with-buffers
      (with-current-buffer view-buf
        (let ((render (lambda (body)
                        (niwa-view-zone-reconcile
                         'status niwa-view--status-marker niwa-view--status-marker
                         (list (list :namespace 'status :id 'tasks :body body)))))
              (binding '(:kind skill :token "$alpha"
                         :source-file "/skills/alpha/SKILL.md")))
          (funcall render "Panel")
          (let ((bounds (niwa-view-zone-fragment-bounds 'status 'tasks)))
            (kill-ring-save (plist-get bounds :start) (1- (plist-get bounds :end))))
          (niwa-view-test--insert-composer-draft "$alpha tail" 7)
          (with-current-buffer (niwa-view-composer-buffer view-buf)
            (let ((start (niwa-view-composer-draft-start view-buf)))
              (niwa-mention-bindings-set start (+ start 6) binding))
            (yank)
            (should-not (text-property-not-all
                         (niwa-view-composer-draft-start view-buf)
                         (niwa-view-composer-draft-end view-buf)
                         'niwa-view-zone-namespace nil)))
          (let ((offset (niwa-view-zone-preservation-test--caret-offset)))
            (when recreate
              (niwa-view-zone-clear 'status))
            (funcall render "Changed")
            (should (equal "$alpha Paneltail" (niwa-view-composer-draft-text)))
            (should (= offset (niwa-view-zone-preservation-test--caret-offset)))
            (with-current-buffer (niwa-view-composer-buffer view-buf)
              (should (equal binding (get-text-property
                                      (niwa-view-composer-draft-start view-buf)
                                      'niwa-mention-binding)))))))))

  :doc "interaction card text yanked into the composer carries no card ownership"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view--interaction-register '(:kind ask :id ask :body "Question"))
      (let ((start (text-property-any (point-min) (point-max)
                                      'niwa-view-interaction-id 'ask)))
        (kill-ring-save start (next-single-property-change
                               start 'niwa-view-interaction-id)))
      (niwa-view-test--insert-composer-draft "" 0)
      (with-current-buffer (niwa-view-composer-buffer view-buf)
        (yank))
      (should (string-search "Question" (niwa-view-composer-draft-text)))
      (with-current-buffer (niwa-view-composer-buffer view-buf)
        (dolist (property (cons 'niwa-view-interaction-id
                                niwa-view-zone-owned-properties))
          (should-not (text-property-not-all
                       (niwa-view-composer-draft-start view-buf)
                       (niwa-view-composer-draft-end view-buf) property nil))))))

  :doc "a status redraw beside a pending approval keeps the card, its row and the draft"
  (niwa-view-test--with-buffers
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer view-buf)
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (let* ((card (niwa-view--interaction-register
                      '(:kind permission :id perm :body "Allow Bash?\n[y] yes [n] no")))
               (render (lambda (body)
                         (niwa-view-zone-reconcile
                          'status niwa-view--status-marker niwa-view--status-marker
                          (list (list :namespace 'status :id 'tasks :body body)))))
               (card-state
                (lambda ()
                  (let* ((start (text-property-any (point-min) (point-max)
                                                   'niwa-view-interaction-id 'perm))
                         (end (next-single-property-change
                               start 'niwa-view-interaction-id nil (point-max))))
                    (list (buffer-substring-no-properties start end)
                          (- (niwa-view-transcript-tail) start)))))
               (offset (niwa-view-zone-preservation-test--caret-offset)))
          (funcall render "one")
          (let ((before (funcall card-state)))
            (funcall render "one\ntwo\nthree")
            (funcall render "x")
            (should (equal before (funcall card-state)))
            (should (eq card (gethash 'perm niwa-view--interaction-overlays)))
            (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
            (should (= offset (niwa-view-zone-preservation-test--caret-offset)))))))))

(provide 'test-niwa-view-zone-preservation)
;;; test-niwa-view-zone-preservation.el ends here
