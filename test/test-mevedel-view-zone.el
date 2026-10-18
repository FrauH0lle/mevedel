;;; test-mevedel-view-zone.el --- Tests for managed view zones -*- lexical-binding: t -*-

;;; Commentary:

;; Contract tests for fixed managed view zones, including reconciliation,
;; recovery, collapse state, navigation, and region queries.

;;; Code:

(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view-zone)

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
  :doc "orders priorities stably on every redraw without changing caller descriptors"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let* ((fragments
              (list (list :namespace 'status :id 'first :priority 2 :body "First")
                    (list :namespace 'status :id 'default :body "Default")
                    (list :namespace 'status :id 'second :priority 2 :body "Second")
                    (list :namespace 'status :id 'no-priority :priority nil :body "Nil")
                    (list :namespace 'status :id 'last :priority -1 :body "Last")))
             (before (copy-tree fragments)))
        (dolist (order (list fragments (reverse fragments)))
          (mevedel-view-zone-reconcile
           'status mevedel-view--status-marker mevedel-view--status-marker order)
          (let ((region (mevedel-view-zone-region 'status)))
            (should (equal
                     (if (eq order fragments)
                         "First\nSecond\nDefault\nNil\nLast\n"
                       "Second\nFirst\nNil\nDefault\nLast\n")
                     (buffer-substring-no-properties
                      (overlay-start region) (overlay-end region)))))
          (should (equal before fragments))
          (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
          (should (= (point) (+ 4 (mevedel-view--input-start))))))))

  :doc "reconciles a fixed zone while preserving lower marker ordering"
  (with-temp-buffer
    (insert "> draft")
    (mevedel-view-zone-test--setup-markers)
    (mevedel-view-zone-reconcile
     'progress (point-min) (point-min)
     '((:namespace progress :id request :body "Working 1s")))
    (let* ((end (overlay-end (mevedel-view-zone-region 'progress)))
           ;; Like `hl-line' on the composer's line: starts at the zone end.
           (line (make-overlay end (point-max))))
      (dolist (body '("Working 2s" "Working... 3s" "W 4s"))
        (mevedel-view-zone-reconcile
         'progress (point-min) (point-min)
         `((:namespace progress :id request :body ,body)))
        (ert-info ((format "after rendering %S" body))
          (should (= (overlay-start line)
                     (overlay-end (mevedel-view-zone-region 'progress))))
          (should (equal "> draft" (buffer-substring-no-properties
                                    (overlay-start line) (overlay-end line))))))))

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

  :doc "history inserted before a managed zone stays outside its bounds"
  (dolist (zone '(status interaction progress))
    (with-temp-buffer
      (insert "> draft")
      (mevedel-view-zone-test--setup-markers)
      (mevedel-view-zone-reconcile
       zone (point-min) (point-min)
       (list (list :namespace zone :id 'test :body "Managed text")))
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (mevedel-view--call-with-render-boundaries-advancing
         (lambda () (insert "New transcript\n"))))
      (should (= (mevedel-view-zone-start zone)
                 (1+ (length "New transcript\n"))))
      (should (= (mevedel-view-zone-start zone)
                 (plist-get (mevedel-view-zone-fragment-bounds zone 'test)
                            :start)))))

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

  :doc "preserves point by anchor identity when anchored text before it changes length"
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (let ((render
           (lambda (first)
             (mevedel-view-zone-reconcile
              'status (point-min) (point-min)
              (list (list :namespace 'status :id 'rows
                          :body (concat
                                 (propertize first 'mevedel-view-zone-anchor '(row a))
                                 (propertize "second row\n" 'mevedel-view-zone-anchor '(row b))
                                 (propertize "[Stop]\n" 'mevedel-view-zone-anchor '(row c)))))))))
      (funcall render "first\n")
      (goto-char (point-min))
      (search-forward "second r")
      (dolist (first '("a much longer first row\n" "1\n"))
        (funcall render first)
        (should (looking-at-p "ow\n"))
        (should (equal '(row b) (get-text-property (point) 'mevedel-view-zone-anchor))))
      (ert-info ("a vanished anchor returns to the fragment start, not to other text")
        (mevedel-view-zone-reconcile
         'status (point-min) (point-min)
         (list (list :namespace 'status :id 'rows
                     :body (propertize "[Stop]\n" 'mevedel-view-zone-anchor '(row c)))))
        (should (= (point) (plist-get (mevedel-view-zone-fragment-bounds 'status 'rows)
                                      :start))))))

  :doc "keeps a window start inside a fragment the same screen rows below its start"
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "> draft")
      (mevedel-view-zone-test--setup-markers)
      (let* ((width (window-body-width))
             (render
              (lambda (first)
                (mevedel-view-zone-reconcile
                 'interaction (point-min) (point-min)
                 (list (list :namespace 'interaction :id 'form
                             :body (concat first "row two\nrow three\nrow four\n"
                                           (mapconcat (lambda (_) "history row\n")
                                                      (number-sequence 1 (* 2 (window-body-height)))
                                                      ""))))))))
        (funcall render "one\n")
        (goto-char (point-max))
        (let ((bounds (mevedel-view-zone-fragment-bounds 'interaction 'form)))
          (set-window-start nil (save-excursion
                                  (goto-char (plist-get bounds :start))
                                  (forward-line 2)
                                  (point))))
        ;; The first line now wraps onto a second screen row.
        (should-not (mevedel-view-test--composer-screen-y (selected-window)))
        (funcall render (concat (make-string (+ width 10) ?x) "\n"))
        (should (= 2 (count-screen-lines
                      (plist-get (mevedel-view-zone-fragment-bounds 'interaction 'form) :start)
                      (window-start))))
        (should (equal "> draft" (mevedel-view--input-text))))))

  :doc "preserves point at a fragment's final character across redraws"
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (mevedel-view-zone-reconcile
     'status (point-min) (point-min)
     '((:namespace status :id tasks :body "Long task status")))
    (let ((bounds (mevedel-view-zone-fragment-bounds 'status 'tasks)))
      (goto-char (1- (plist-get bounds :end)))
      (should (= (plist-get bounds :start)
                 (plist-get (mevedel-view-zone--bounds-at) :start))))
    (mevedel-view-zone-reconcile
     'status (point-min) (point-min)
     '((:namespace status :id tasks :body "Short")))
    (let ((final-position (point)))
      (should (= final-position
                 (1- (plist-get
                      (mevedel-view-zone-fragment-bounds 'status 'tasks)
                      :end))))
      (mevedel-view-zone-reconcile
       'status (point-min) (point-min)
       '((:namespace status :id tasks :body "Short")))
      (should (= final-position (point)))))

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
        (should (equal "TailLive\n" (buffer-string))))))

  :doc "each window anchors its composer, visited card, or reading start independently"
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (forward-line 1)
        (dotimes (n 100) (insert (format "transcript %d\n" n))))
      (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let* ((visitor (selected-window))
             (writer (split-window-right))
             (reader (split-window writer nil 'below))
             (card (list :namespace 'interaction :id 'visited :priority 2
                         :body "question\noption one\noption two\nanswer"))
             (lower (list :namespace 'interaction :id 'lower :priority 1
                          :body (mapconcat (lambda (_) "permission")
                                           (number-sequence 1 30) "\n")))
             (upper (list :namespace 'interaction :id 'upper :priority 3
                          :body "new\nupper\ncard"))
             (render (lambda (fragments)
                       (mevedel-view-zone-reconcile
                        'interaction mevedel-view--interaction-marker
                        mevedel-view--input-marker fragments))))
        (funcall render (list card))
        (goto-char (plist-get (mevedel-view-zone-fragment-bounds
                              'interaction 'visited) :start))
        (forward-line 2)
        (set-window-start visitor
                          (save-excursion (vertical-motion -5 visitor) (point)) t)
        (set-window-point writer (+ 4 (mevedel-view--input-start)))
        (set-window-start writer
                          (save-excursion
                            (goto-char (mevedel-view--input-start))
                            (vertical-motion (- (1- (window-body-height writer)))
                                             writer)
                            (point)) t)
        (let ((reader-start (save-excursion
                              (goto-char (point-min)) (forward-line 10) (point))))
          (set-window-start reader reader-start t)
          (set-window-point reader reader-start)
          (let ((writer-row (mevedel-view-test--composer-screen-y writer)))
            (dolist (fragments (list (list card lower) (list upper card lower)
                                    (list upper card) (list card)))
              (let ((visit-row (count-screen-lines
                                (window-start visitor) (window-point visitor)
                                nil visitor)))
                (funcall render fragments)
                (if (equal fragments (list upper card))
                    ;; Removing the tall lower card exposes NEW blank rows.
                    ;; Fill those rows even though the visited option moves.
                    (progn
                      (should (= (window-start visitor)
                                 (save-excursion
                                   (goto-char (point-max))
                                   (vertical-motion
                                    (- (1- (window-body-height visitor))) visitor)
                                   (point))))
                      (should (< visit-row
                                 (count-screen-lines
                                  (window-start visitor) (window-point visitor)
                                  nil visitor))))
                  (should (= visit-row
                             (count-screen-lines
                              (window-start visitor) (window-point visitor)
                              nil visitor)))))
              (should (eq 'visited (get-text-property
                                   (window-point visitor) 'mevedel-view-zone-id)))
              (should (save-excursion
                        (goto-char (window-point visitor))
                        (looking-at-p "option two")))
              (should (= writer-row (mevedel-view-test--composer-screen-y writer)))
              (should (= (+ 4 (mevedel-view--input-start))
                         (window-point writer)))
              (should (= reader-start (window-start reader)))
              (should (equal "> draft\nsecond line" (mevedel-view--input-text))))
            ;; A different zone's growth preserves the repaired visit row.
            (let ((visit-row (count-screen-lines
                              (window-start visitor) (window-point visitor)
                              nil visitor)))
              (mevedel-view-zone-reconcile
               'status mevedel-view--status-marker mevedel-view--status-marker
               '((:namespace status :id work :body "working\nnew task\nprogress")))
              (should (= visit-row
                         (count-screen-lines
                          (window-start visitor) (window-point visitor)
                          nil visitor))))
            (should (= writer-row (mevedel-view-test--composer-screen-y writer)))
            (should (= reader-start (window-start reader))))))))

  :doc "visitor shrink fills newly exposed rows but preserves existing blanks and option identity"
  (dolist (old-blanks '(0 3))
    (mevedel-view-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (delete-other-windows)
        ;; A finite viewport makes the previously hidden end observable after
        ;; shrink; three deliberate blanks exercise a different repair target.
        (set-window-buffer (split-window (selected-window) 16 'below) data-buf)
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (forward-line 1)
          (dotimes (n 100) (insert (format "transcript %d\n" n))))
        (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (let* ((visitor (selected-window))
               (card '(:namespace interaction :id visited :priority 2
                       :body "question\noption one\noption two\nanswer"))
               (lower (list :namespace 'interaction :id 'lower :priority 1
                            :body (mapconcat (lambda (_) "permission")
                                             (number-sequence
                                              1 (if (zerop old-blanks) 30 3))
                                             "\n")))
               (render (lambda (fragments)
                         (mevedel-view-zone-reconcile
                          'interaction mevedel-view--interaction-marker
                          mevedel-view--input-marker fragments))))
          (funcall render (list card lower))
          (goto-char (plist-get (mevedel-view-zone-fragment-bounds
                                'interaction 'visited) :start))
          (forward-line 2)
          (set-window-start
           visitor (save-excursion
                     (if (zerop old-blanks)
                         (vertical-motion -3 visitor)
                       (goto-char (point-max))
                       (vertical-motion
                        (- (+ 1 old-blanks) (window-body-height visitor)) visitor))
                     (point)) t)
          (let* ((visit-row (count-screen-lines (window-start) (point)))
                 (end-row (- (window-body-height visitor) 1 old-blanks)))
            (should (< visit-row (window-body-height visitor)))
            (if (zerop old-blanks)
                (should-not (mevedel-view--position-screen-y (point-max) visitor))
              (should (= end-row (mevedel-view--position-screen-y
                                 (point-max) visitor))))
            (funcall render (list card))
            ;; This independent row calculation fails both a missing repair
            ;; and an unconditional last-page clamp that consumes old blanks.
            (should (= (window-start visitor)
                       (save-excursion
                         (goto-char (point-max))
                         (vertical-motion (- end-row) visitor)
                         (point))))
            (should (= end-row (mevedel-view--position-screen-y
                               (point-max) visitor)))
            (should (< visit-row (count-screen-lines (window-start) (point))))
            (when (> old-blanks 0)
              (should (= (+ visit-row 3)
                         (count-screen-lines (window-start) (point)))))
            (should (eq 'visited (get-text-property (point) 'mevedel-view-zone-id)))
            (should (looking-at-p "option two"))
            (should (equal "> draft\nsecond line" (mevedel-view--input-text))))))))

  :doc "composer anchoring preserves deliberate blank rows when a card shrinks"
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (forward-line 1)
        (dotimes (n 100) (insert (format "transcript %d\n" n))))
      (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (mevedel-view-zone-reconcile
       'interaction mevedel-view--interaction-marker mevedel-view--input-marker
       ;; Removing these rows must shrink the character tail even after
       ;; composer anchoring exposes three shorter transcript rows.
       (list (list :namespace 'interaction :id 'card
                   :body (mapconcat #'identity
                                    (make-list 3 (make-string 50 ?x)) "\n"))))
      (set-window-start nil
                        (save-excursion
                          (goto-char (mevedel-view--input-start))
                          (vertical-motion -6) (point)) t)
      (mevedel-view-zone-clear 'interaction)
      (should (= 6 (mevedel-view-test--composer-screen-y (selected-window))))
      (should (= (+ 4 (mevedel-view--input-start)) (point)))
      (should (equal "> draft\nsecond line" (mevedel-view--input-text)))))

  :doc "composer anchoring wins over a logical start inside a growing card"
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
      (let ((render (lambda (rows)
                      (mevedel-view-zone-reconcile
                       'interaction mevedel-view--interaction-marker
                       mevedel-view--input-marker
                       (list (list :namespace 'interaction :id 'tall
                                   :body (mapconcat
                                          (lambda (n) (format "row %d" n))
                                          (number-sequence 1 rows) "\n")))))))
        (funcall render 60)
        (set-window-start nil
                          (save-excursion
                            (goto-char (mevedel-view--input-start))
                            (vertical-motion -10) (point)) t)
        (should (mevedel-view-zone-bounds-at (window-start)))
        (should (= 10 (mevedel-view-test--composer-screen-y (selected-window))))
        (funcall render 90)
        (should (= 10 (mevedel-view-test--composer-screen-y (selected-window))))
        (should (= (+ 4 (mevedel-view--input-start)) (point)))
        (should (equal "> draft\nsecond line" (mevedel-view--input-text))))))

  :doc "removed tall cards still fill the last page without a surviving row anchor"
  (dolist (start-in-card '(t nil))
    (mevedel-view-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (delete-other-windows)
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (forward-line 1)
          (dotimes (n 100) (insert (format "transcript %d\n" n))))
        (mevedel-view-test--insert-composer-draft "> draft\nsecond line" 4)
        (mevedel-view-zone-reconcile
         'interaction mevedel-view--interaction-marker mevedel-view--input-marker
         (list (list :namespace 'interaction :id 'tall
                     :body (mapconcat (lambda (_) "option")
                                      (number-sequence 1 (* 2 (window-body-height)))
                                      "\n"))))
        (goto-char (plist-get (mevedel-view-zone-fragment-bounds
                              'interaction 'tall) :start))
        (unless start-in-card (forward-line -5))
        (set-window-start nil (point) t)
        (should-not (mevedel-view-test--composer-screen-y (selected-window)))
        (should (eq (not (null (mevedel-view-zone-bounds-at (window-start))))
                    start-in-card))
        (mevedel-view-zone-clear 'interaction)
        (should (= (window-start)
                   (save-excursion
                     (goto-char (point-max))
                     (vertical-motion (- (1- (window-body-height))))
                     (point))))
        (should (equal "> draft\nsecond line" (mevedel-view--input-text))))))

  :doc "a line's own activation wins over the fragment default in rendered text"
  (with-temp-buffer
    (insert "> draft")
    (mevedel-view-zone-test--setup-markers)
    (let* ((line-action (lambda () 'line))
           (fragment-action (lambda () 'fragment))
           (body (concat
                  (propertize "own\n" 'mevedel-view-zone-activate line-action)
                  "plain\n")))
      (mevedel-view-zone-reconcile
       'status (point-min) (point-min)
       (list (list :namespace 'status :id 'rows :body body
                   :body-suffix "\n" :activate fragment-action)))
      (goto-char (point-min))
      (should (eq line-action
                  (get-text-property (point) 'mevedel-view-zone-activate)))
      (search-forward "plain")
      (should (eq fragment-action
                  (get-text-property (1- (point)) 'mevedel-view-zone-activate)))
      (should (eq fragment-action
                  (get-text-property (1+ (point)) 'mevedel-view-zone-activate))))))

(mevedel-deftest mevedel-view-zone-reconcile-read-only-end ()
  (let ((inspector ,inspector))
    (require 'mevedel-directive-activity)
    (mevedel-view-test--with-buffers
      (save-window-excursion
        (switch-to-buffer view-buf)
        (delete-other-windows)
        (if inspector
            (mevedel-directive-activity-mode)
          (setq-local mevedel-view--agent-transcript-p t))
        ;; Both read-only surfaces keep layout markers at EOF, not a composer.
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          (erase-buffer)
          (dotimes (n 100) (insert (format "history %d\n" n)))
          (setq-local mevedel-view--status-marker (copy-marker (point) t)
                      mevedel-view--interaction-marker (copy-marker (point) t)
                      mevedel-view--input-marker (copy-marker (point) nil)))
        (mevedel-view-zone-reconcile
         'status mevedel-view--status-marker mevedel-view--status-marker
         (list (list :namespace 'status :id 'work
                     :body (mapconcat #'identity
                                      (make-list 3 (make-string 50 ?x)) "\n"))))
        (goto-char (point-max))
        (set-window-start nil (save-excursion (vertical-motion -6) (point)) t)
        (mevedel-view-zone-clear 'status)
        ;; Without an editable composer, net-tail shrink fills the last page.
        ;; Treating the EOF marker as a composer instead preserves six rows.
        ;; Reserve a row for the empty EOF line too: a start one row too early
        ;; leaves EOF outside the viewport, despite filling all history rows.
        (should (= (1- (window-body-height))
                   (save-excursion
                     (goto-char (window-start))
                     (vertical-motion (window-body-height)))))
        (should (= (window-start)
                   (save-excursion
                     (goto-char (point-max))
                     (vertical-motion (- (1- (window-body-height))))
                     (point)))))))
  (inspector)
  :doc "agent transcript EOF is not an editable composer anchor" nil
  :doc "directive inspector EOF is not an editable composer anchor" t)

(mevedel-deftest mevedel-view-zone--anchor-start
  (:doc "returns the start of the anchor run, bounded by the fragment start")
  (with-temp-buffer
    (insert (propertize "aaa" 'mevedel-view-zone-anchor 'a)
            (propertize "bbbb" 'mevedel-view-zone-anchor 'b))
    (should (= 4 (mevedel-view-zone--anchor-start 6 'b 1)))
    (should (= 4 (mevedel-view-zone--anchor-start 4 'b 1)))
    (should (= 2 (mevedel-view-zone--anchor-start 3 'a 2)))))

(mevedel-deftest mevedel-view-zone--anchor-run
  (:doc "finds the run whose anchor is equal, not only eq, to the requested one")
  (with-temp-buffer
    (insert (propertize "aaa" 'mevedel-view-zone-anchor (list 'row 1))
            "plain"
            (propertize "bb" 'mevedel-view-zone-anchor (list 'row 2)))
    (should (equal '(9 . 11) (mevedel-view-zone--anchor-run (list 'row 2) 1 11)))
    (should (equal '(1 . 4) (mevedel-view-zone--anchor-run (list 'row 1) 1 11)))
    (should-not (mevedel-view-zone--anchor-run (list 'row 2) 1 9))
    (should-not (mevedel-view-zone--anchor-run 'missing 1 11))))

(mevedel-deftest mevedel-view-zone-position-anchor
  (:doc "returns nil outside zone text; `mevedel-view-zone-anchor-position' resolves the identity after a redraw and returns nil once the fragment is gone")
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (let ((render (lambda (first)
                    (mevedel-view-zone-reconcile
                     'status (point-min) (point-min)
                     (list (list :namespace 'status :id 'rows
                                 :body (concat
                                        (propertize first 'mevedel-view-zone-anchor 'a)
                                        (propertize "target\n" 'mevedel-view-zone-anchor 'b))))))))
      (funcall render "x\n")
      (goto-char (point-max))
      (insert "plain text")
      (should-not (mevedel-view-zone-position-anchor (- (point-max) 2)))
      (let ((anchor (mevedel-view-zone-position-anchor
                     (+ 2 (save-excursion (goto-char (point-min))
                                          (search-forward "target")
                                          (match-beginning 0))))))
        (funcall render "a much longer first row\n")
        (should (equal "rget" (buffer-substring-no-properties
                               (mevedel-view-zone-anchor-position anchor)
                               (+ 4 (mevedel-view-zone-anchor-position anchor)))))
        (mevedel-view-zone-clear 'status)
        (should-not (mevedel-view-zone-anchor-position anchor))))))

(mevedel-deftest mevedel-view-zone-anchor-position-strict
  (:doc "strict resolution rejects lost sub-anchors and truncated character or screen-row offsets")
  (with-temp-buffer
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (mevedel-view-zone-test--setup-markers)
      (let ((render (lambda (body)
                      (mevedel-view-zone-reconcile
                       'status (point-min) (point-min)
                       (list (list :namespace 'status :id 'rows :body body))))))
        (funcall render (propertize "first\nsecond\nthird\n"
                                    'mevedel-view-zone-anchor 'row))
        (goto-char (point-min))
        (forward-line 2)
        (let ((character (mevedel-view-zone-position-anchor (point)))
              (screen-row (mevedel-view-zone-position-anchor
                           (point) (selected-window))))
          (should (= (point) (mevedel-view-zone-anchor-position character t)))
          (should (= (point) (mevedel-view-zone-anchor-position screen-row t)))
          (funcall render (propertize "short\n" 'mevedel-view-zone-anchor 'row))
          (dolist (anchor (list character screen-row))
            (should (mevedel-view-zone-anchor-position anchor))
            (should-not (mevedel-view-zone-anchor-position anchor t)))
          (funcall render (propertize "other\n" 'mevedel-view-zone-anchor 'replacement))
          (dolist (anchor (list character screen-row))
            (should (= (point-min) (mevedel-view-zone-anchor-position anchor)))
            (should-not (mevedel-view-zone-anchor-position anchor t))))))))

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
     'status (point-min) (point-min)
     '((:namespace status :id tasks :body "Tasks")))
    (should (= (point-min) (mevedel-view-zone-start 'status)))))

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
  (:doc "finds adjacent managed fragments from their interiors")
  (with-temp-buffer
    (mevedel-view-zone-test--setup-markers)
    (mevedel-view-zone-reconcile
     'status (point-min) (point-min)
     '((:namespace status :id tasks :body "Tasks")
       (:namespace status :id agents :body "Agents")))
    (dolist (case '(("Tasks" tasks) ("Agents" agents)))
      (goto-char (point-min))
      (search-forward (car case))
      (backward-char 2)
      (should (equal (cadr case)
                     (plist-get (mevedel-view-zone-bounds-at) :id))))))

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
