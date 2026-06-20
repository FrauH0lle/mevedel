;;; test-mevedel-view-fragment.el --- Tests for view fragments -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view-fragment)


;;
;;; Test helpers

(defun mevedel-view-fragment-test--region (&optional start end)
  "Return a managed fragment region overlay from START to END."
  (make-overlay (or start (point)) (or end (point)) (current-buffer) t nil))

(defun mevedel-view-fragment-test--plain-buffer-string ()
  "Return the current buffer text without properties."
  (buffer-substring-no-properties (point-min) (point-max)))


;;
;;; Rendering

(mevedel-deftest mevedel-view-fragment--render ()
  ,test
  (test)
  :doc "tags label and body with container-scoped fragment metadata"
  (with-temp-buffer
    (let* ((region (mevedel-view-fragment-test--region))
           (fragment '(:namespace interaction
                       :id prompt-1
                       :label-left "Permission"
                       :label-right "Bash"
                       :body "Allow?"
                       :navigatable t))
           (text (mevedel-view-fragment--render region fragment))
           (key (mevedel-view-fragment--key region fragment)))
      (should (equal "Permission Bash\nAllow?\n" text))
      (should (equal key (get-text-property 0 'mevedel-view-fragment-key text)))
      (should (eq region (get-text-property 0 'mevedel-view-fragment-region text)))
      (should (eq 'interaction
                  (get-text-property 0 'mevedel-view-fragment-namespace text)))
      (should (eq 'prompt-1
                  (get-text-property 0 'mevedel-view-fragment-id text)))
      (should (eq 'label
                  (get-text-property 0 'mevedel-view-fragment-section text)))
      (should (eq 'body
                  (get-text-property (string-match-p "Allow" text)
                                     'mevedel-view-fragment-section text)))
      (should (get-text-property 0 'mevedel-view-fragment-navigatable text))
      (should (get-text-property 0 'read-only text))
      (should (get-text-property (string-match-p "Allow" text)
                                 'read-only text))))

  :doc "preserves body-local interactive properties and fills defaults only"
  (with-temp-buffer
    (let* ((region (mevedel-view-fragment-test--region))
           (specific-map (make-sparse-keymap))
           (default-map (make-sparse-keymap))
           (body (concat (propertize "specific" 'keymap specific-map
                                     'help-echo "specific help"
                                     'font-lock-face 'bold
                                     'face 'italic
                                     'mouse-face 'highlight
                                     'follow-link t
                                     'action #'ignore)
                         " plain"))
           (text (mevedel-view-fragment--render
                  region
                  `(:namespace interaction
                    :id prompt-1
                    :body ,body
                    :keymap ,default-map
                    :help-echo "default help"))))
      (should (eq specific-map (get-text-property 0 'keymap text)))
      (should (equal "specific help" (get-text-property 0 'help-echo text)))
      (should (eq 'bold (get-text-property 0 'font-lock-face text)))
      (should (eq 'italic (get-text-property 0 'face text)))
      (should (eq 'highlight (get-text-property 0 'mouse-face text)))
      (should (get-text-property 0 'follow-link text))
      (should (eq #'ignore (get-text-property 0 'action text)))
      (should (eq default-map (get-text-property 9 'keymap text)))
      (should (equal "default help" (get-text-property 9 'help-echo text)))))

  :doc "normalizes non-empty body text to exactly one trailing newline"
  (with-temp-buffer
    (let* ((region (mevedel-view-fragment-test--region))
           (default-map (make-sparse-keymap))
           (text (mevedel-view-fragment--render
                  region `(:namespace interaction
                           :id prompt-1
                           :body "body\n\n"
                           :keymap ,default-map
                           :help-echo "default help"))))
      (should (equal "body\n" text))
      (should (eq default-map (get-text-property 4 'keymap text)))
      (should (equal "default help"
                     (get-text-property 4 'help-echo text)))))

  :doc "appends an optional body suffix after newline normalization"
  (with-temp-buffer
    (let* ((region (mevedel-view-fragment-test--region))
           (default-map (make-sparse-keymap))
           (text (mevedel-view-fragment--render
                  region `(:namespace interaction
                           :id prompt-1
                           :body "body\n\n"
                           :body-suffix "\n"
                           :keymap ,default-map
                           :help-echo "default help")))
           (key (mevedel-view-fragment--key
                 region '(:namespace interaction :id prompt-1))))
      (should (equal "body\n\n" text))
      (should (equal key (get-text-property
                          (1- (length text))
                          'mevedel-view-fragment-key text)))
      (should (eq 'body (get-text-property
                         (1- (length text))
                         'mevedel-view-fragment-section text)))
      (should (eq default-map (get-text-property
                               (1- (length text)) 'keymap text)))
      (should (equal "default help"
                     (get-text-property
                      (1- (length text)) 'help-echo text)))))

  :doc "renders an empty body as an identifiable body fragment"
  (with-temp-buffer
    (let* ((region (mevedel-view-fragment-test--region))
           (text (mevedel-view-fragment--render
                  region '(:namespace interaction :id empty))))
      (should (equal "\n" text))
      (should (eq 'body
                  (get-text-property 0 'mevedel-view-fragment-section text)))
      (should (eq 'empty
                  (get-text-property 0 'mevedel-view-fragment-id text)))))

  :doc "tags action and collapse metadata on rendered text"
  (with-temp-buffer
    (let* ((region (mevedel-view-fragment-test--region))
           (collapse-key '(interaction prompt-1))
           (text (mevedel-view-fragment--render
                  region `(:namespace interaction
                           :id prompt-1
                           :label-left "Prompt"
                           :body "hidden body"
                           :activate ignore
                           :entry (:kind permission)
                           :collapsible t
                           :collapse-key ,collapse-key
                           :collapsed t))))
      (should (equal "Prompt\n" text))
      (should (eq 'ignore
                  (get-text-property 0 'mevedel-view-fragment-activate text)))
      (should (equal '(:kind permission)
                     (get-text-property 0 'mevedel-view-fragment-entry text)))
      (should (get-text-property 0 'mevedel-view-fragment-collapsible text))
      (should (equal collapse-key
                     (get-text-property 0 'mevedel-view-fragment-collapse-key
                                        text)))
      (should (get-text-property 0 'mevedel-view-fragment-collapsed text))))

  :doc "collapse state hides and restores a labeled fragment body across refreshes"
  (with-temp-buffer
    (let* ((region (mevedel-view-fragment-test--region))
           (fragment '(:namespace status
                       :id agents
                       :label-left "Agents"
                       :body "expanded body"
                       :collapsible t
                       :collapse-key (status agents))))
      (mevedel-view-fragment--reconcile region 'status (list fragment))
      (should (string-match-p "expanded body"
                              (mevedel-view-fragment-test--plain-buffer-string)))
      (goto-char (overlay-start region))
      (should (mevedel-view-fragment-toggle-collapsed))
      (mevedel-view-fragment--reconcile
       region 'status
       (list (plist-put (copy-sequence fragment) :body "refreshed body")))
      (should-not (string-match-p
                   "refreshed body"
                   (mevedel-view-fragment-test--plain-buffer-string)))
      (goto-char (overlay-start region))
      (should-not (mevedel-view-fragment-toggle-collapsed))
      (mevedel-view-fragment--reconcile
       region 'status
       (list (plist-put (copy-sequence fragment) :body "restored body")))
      (should (string-match-p "restored body"
                              (mevedel-view-fragment-test--plain-buffer-string))))))


;;
;;; Navigation

(mevedel-deftest mevedel-view-fragment-navigation ()
  ,test
  (test)

  :doc "next and previous skip non-navigatable fragments"
  (with-temp-buffer
    (insert "prefix\n")
    (let ((region (mevedel-view-fragment-test--region (point) (point))))
      (mevedel-view-fragment--reconcile
       region 'status
       '((:namespace status :id first :body "first" :navigatable t)
         (:namespace status :id skipped :body "skipped")
         (:namespace status :id second :body "second" :navigatable t)))
      (goto-char (point-min))
      (should (mevedel-view-fragment-next))
      (should (eq 'first (get-text-property
                          (point) 'mevedel-view-fragment-id)))
      (should (mevedel-view-fragment-next))
      (should (eq 'second (get-text-property
                           (point) 'mevedel-view-fragment-id)))
      (should-not (mevedel-view-fragment-next))
      (should (eq 'second (get-text-property
                           (point) 'mevedel-view-fragment-id)))
      (goto-char (point-max))
      (should (mevedel-view-fragment-previous))
      (should (eq 'second (get-text-property
                           (point) 'mevedel-view-fragment-id)))
      (should (mevedel-view-fragment-previous))
      (should (eq 'first (get-text-property
                          (point) 'mevedel-view-fragment-id)))
      (should-not (mevedel-view-fragment-previous))))

  :doc "navigation respects explicit limits and narrowing"
  (with-temp-buffer
    (insert "prefix\n")
    (let ((region (mevedel-view-fragment-test--region (point) (point))))
      (mevedel-view-fragment--reconcile
       region 'status
       '((:namespace status :id first :body "first" :navigatable t)
         (:namespace status :id second :body "second" :navigatable t)))
      (let* ((first (mevedel-view-fragment--find-bounds region 'status 'first))
             (second (mevedel-view-fragment--find-bounds region 'status 'second))
             (second-start (plist-get second :start)))
        (goto-char (point-min))
        (should (mevedel-view-fragment-next second-start))
        (should (= (point) (plist-get first :start)))
        (should-not (mevedel-view-fragment-next second-start))
        (save-restriction
          (narrow-to-region second-start (point-max))
          (goto-char (point-max))
          (should (mevedel-view-fragment-previous))
          (should (= (point) second-start))
          (should-not (mevedel-view-fragment-previous)))))))


;;
;;; Insertion

(mevedel-deftest mevedel-view-fragment--insert ()
  ,test
  (test)
  :doc "expands an overlay-backed managed region"
  (with-temp-buffer
    (let ((region (mevedel-view-fragment-test--region)))
      (mevedel-view-fragment--insert
       region '(:namespace interaction :id prompt :body "inserted"))
      (should (equal "inserted\n"
                     (buffer-substring-no-properties
                      (overlay-start region) (overlay-end region))))
      (should (mevedel-view-fragment--find-bounds
               region 'interaction 'prompt))))

  :doc "refuses direct insertion inside an existing fragment run"
  (with-temp-buffer
    (let ((region-a (mevedel-view-fragment-test--region)))
      (mevedel-view-fragment--reconcile
       region-a 'interaction
       '((:namespace interaction :id shared :body "abcdef")))
      (let* ((before (mevedel-view-fragment-test--plain-buffer-string))
             (region-b (mevedel-view-fragment-test--region
                        (+ (overlay-start region-a) 2)
                        (+ (overlay-start region-a) 4))))
        (goto-char (overlay-start region-b))
        (should-error
         (mevedel-view-fragment--insert
          region-b '(:namespace status :id other :body "inserted")))
        (should (equal before
                       (mevedel-view-fragment-test--plain-buffer-string)))
        (should (mevedel-view-fragment--find-bounds
                 region-a 'interaction 'shared))))))


;;
;;; Reconciliation

(mevedel-deftest mevedel-view-fragment--reconcile ()
  ,test
  (test)
  :doc "sorts by descending priority and preserves caller order for ties"
  (with-temp-buffer
    (let ((region (mevedel-view-fragment-test--region)))
      (mevedel-view-fragment--reconcile
       region 'status
       '((:namespace status :id low :priority 10 :body "low")
         (:namespace status :id high :priority 30 :body "high")
         (:namespace status :id same-a :priority 20 :body "same-a")
         (:namespace status :id same-b :priority 20 :body "same-b")))
      (should (equal "high\nsame-a\nsame-b\nlow\n"
                     (mevedel-view-fragment-test--plain-buffer-string)))))

  :doc "removes stale fragments in region without touching other regions"
  (with-temp-buffer
    (let (region-a region-b start)
      (insert "prefix\n")
      (setq start (point))
      (insert "region-a\n")
      (setq region-a (mevedel-view-fragment-test--region start (point)))
      (insert "split\n")
      (setq start (point))
      (insert "region-b\n")
      (setq region-b (mevedel-view-fragment-test--region start (point)))
      (mevedel-view-fragment--reconcile
       region-a 'interaction
       '((:namespace interaction :id shared :body "old-a")
         (:namespace interaction :id stale :body "stale-a")))
      (mevedel-view-fragment--reconcile
       region-b 'interaction
       '((:namespace interaction :id shared :body "old-b")))
      (mevedel-view-fragment--reconcile
       region-a 'interaction
       '((:namespace interaction :id shared :body "new-a")))
      (should (string-match-p "new-a" (mevedel-view-fragment-test--plain-buffer-string)))
      (should-not (string-match-p "stale-a" (mevedel-view-fragment-test--plain-buffer-string)))
      (should (string-match-p "old-b" (mevedel-view-fragment-test--plain-buffer-string)))))

  :doc "accepts explicit numeric region bounds"
  (with-temp-buffer
    (insert "placeholder\n")
    (let ((region (cons (point-min) (point-max))))
      (mevedel-view-fragment--reconcile
       region 'interaction
       '((:namespace interaction :id prompt :body "body")))
      (should (equal "body\n"
                     (mevedel-view-fragment-test--plain-buffer-string)))
      (should (= (car region) (point-min)))
      (should (= (cdr region) (point-max)))
      (mevedel-view-fragment--update
       region '(:namespace interaction :id prompt :body "longer body"))
      (should (equal "longer body\n"
                     (mevedel-view-fragment-test--plain-buffer-string)))
      (should (= (car region) (point-min)))
      (should (= (cdr region) (point-max)))
      (should (mevedel-view-fragment--delete region 'interaction 'prompt))
      (should (equal "" (mevedel-view-fragment-test--plain-buffer-string)))
      (should (= (car region) (point-min)))
      (should (= (cdr region) (point-min)))))

  :doc "refuses overlapping numeric regions with foreign fragments"
  (with-temp-buffer
    (let ((region-a (cons (point) (point))))
      (mevedel-view-fragment--reconcile
       region-a 'interaction
       '((:namespace interaction :id shared :body "owned-a")))
      (let ((before (mevedel-view-fragment-test--plain-buffer-string))
            (region-b (cons (car region-a) (cdr region-a))))
        (should-error
         (mevedel-view-fragment--reconcile
          region-b 'interaction
          '((:namespace interaction :id shared :body "owned-b"))))
        (should (equal before
                       (mevedel-view-fragment-test--plain-buffer-string)))
        (should (mevedel-view-fragment--find-bounds
                 region-a 'interaction 'shared)))))

  :doc "refuses zero-width numeric regions inside foreign fragments"
  (with-temp-buffer
    (let ((region-a (cons (point) (point))))
      (mevedel-view-fragment--reconcile
       region-a 'interaction
       '((:namespace interaction :id shared :body "abcdef")))
      (let* ((before (mevedel-view-fragment-test--plain-buffer-string))
             (inside (+ (car region-a) 2))
             (region-b (cons inside inside)))
        (should-error
         (mevedel-view-fragment--reconcile
          region-b 'interaction
          '((:namespace interaction :id owned :body "owned-b"))))
        (should (equal before
                       (mevedel-view-fragment-test--plain-buffer-string)))
        (should (mevedel-view-fragment--find-bounds
                 region-a 'interaction 'shared)))))

  :doc "keeps empty-body fragments addressable after reconcile"
  (with-temp-buffer
    (let ((region (mevedel-view-fragment-test--region)))
      (mevedel-view-fragment--reconcile
       region 'interaction
       '((:namespace interaction :id empty)))
      (should (equal "\n" (mevedel-view-fragment-test--plain-buffer-string)))
      (let ((bounds (mevedel-view-fragment--find-bounds
                     region 'interaction 'empty)))
        (should (= (plist-get bounds :start) (overlay-start region)))
        (should (= (plist-get bounds :end) (overlay-end region))))))

  :doc "preserves other namespaces in the same managed region"
  (with-temp-buffer
    (let ((region (mevedel-view-fragment-test--region)))
      (mevedel-view-fragment--reconcile
       region 'status
       '((:namespace status :id tasks :body "tasks old")))
      (mevedel-view-fragment--reconcile
       region 'interaction
       '((:namespace interaction :id prompt :body "prompt")))
      (mevedel-view-fragment--reconcile
       region 'status
       '((:namespace status :id tasks :body "tasks new")))
      (should (string-match-p "tasks new"
                              (mevedel-view-fragment-test--plain-buffer-string)))
      (should-not (string-match-p "tasks old"
                                  (mevedel-view-fragment-test--plain-buffer-string)))
      (should (string-match-p "prompt"
                              (mevedel-view-fragment-test--plain-buffer-string)))))

  :doc "does not delete fragments owned by another equal-bounds container"
  (with-temp-buffer
    (let ((region-a (mevedel-view-fragment-test--region)))
      (mevedel-view-fragment--reconcile
       region-a 'interaction
       '((:namespace interaction :id shared :body "owned-a")))
      (let ((region-b (mevedel-view-fragment-test--region
                       (overlay-start region-a) (overlay-end region-a))))
        (mevedel-view-fragment--reconcile
         region-b 'interaction
         '((:namespace interaction :id shared :body "owned-b")))
        (should (string-match-p "owned-a"
                                (mevedel-view-fragment-test--plain-buffer-string)))
        (should (string-match-p "owned-b"
                                (mevedel-view-fragment-test--plain-buffer-string)))
        (should (mevedel-view-fragment--find-bounds
                 region-a 'interaction 'shared))
        (should (mevedel-view-fragment--find-bounds
                 region-b 'interaction 'shared)))))

  :doc "refuses to split a foreign fragment run"
  (with-temp-buffer
    (let ((region-a (mevedel-view-fragment-test--region)))
      (mevedel-view-fragment--reconcile
       region-a 'interaction
       '((:namespace interaction :id shared :body "abcdef")))
      (let* ((before (mevedel-view-fragment-test--plain-buffer-string))
             (region-b (mevedel-view-fragment-test--region
                        (+ (overlay-start region-a) 2)
                        (+ (overlay-start region-a) 4))))
        (should-error
         (mevedel-view-fragment--reconcile
          region-b 'interaction
          '((:namespace interaction :id shared :body "owned-b"))))
        (should (equal before
                       (mevedel-view-fragment-test--plain-buffer-string)))
        (should (mevedel-view-fragment--find-bounds
                 region-a 'interaction 'shared)))))

  :doc "finds owned same-namespace spans after a foreign fragment run"
  (with-temp-buffer
    (let ((region-a (mevedel-view-fragment-test--region)))
      (mevedel-view-fragment--reconcile
       region-a 'interaction
       '((:namespace interaction :id foreign :body "abcdef")))
      (let ((region-b (mevedel-view-fragment-test--region
                       (+ (overlay-start region-a) 2)
                       (overlay-end region-a))))
        (mevedel-view-fragment--reconcile
         region-b 'interaction
         '((:namespace interaction :id owned :body "old-b")))
        (mevedel-view-fragment--reconcile
         region-b 'interaction
         '((:namespace interaction :id owned :body "new-b")))
        (should (string-match-p "new-b"
                                (mevedel-view-fragment-test--plain-buffer-string)))
        (should-not (string-match-p "old-b"
                                    (mevedel-view-fragment-test--plain-buffer-string)))
        (should (mevedel-view-fragment--find-bounds
                 region-b 'interaction 'owned)))))

  :doc "keeps marker-pair regions spanning reconciled text"
  (with-temp-buffer
    (insert "placeholder\n")
    (let* ((start (copy-marker (point-min)))
           (end (copy-marker (point-max)))
           (region (cons start end)))
      (mevedel-view-fragment--reconcile
       region 'interaction
       '((:namespace interaction :id prompt :body "body")))
      (should (equal "body\n"
                     (mevedel-view-fragment-test--plain-buffer-string)))
      (should (= (marker-position start) (point-min)))
      (should (= (marker-position end) (point-max)))
      (should (mevedel-view-fragment--find-bounds
               region 'interaction 'prompt))
      (mevedel-view-fragment--update
       region '(:namespace interaction :id prompt :body "new"))
      (should (equal "new\n"
                     (mevedel-view-fragment-test--plain-buffer-string)))
      (should (= (marker-position start) (point-min)))
      (should (= (marker-position end) (point-max))))))


;;
;;; Mutation

(mevedel-deftest mevedel-view-fragment--update ()
  ,test
  (test)
  :doc "updates by id while preserving unrelated surrounding text"
  (with-temp-buffer
    (insert "before\n")
    (let ((start (point)) region)
      (insert "placeholder\n")
      (setq region (mevedel-view-fragment-test--region start (point)))
      (insert "after\n")
      (mevedel-view-fragment--reconcile
       region 'interaction
       '((:namespace interaction :id prompt :body "old body")))
      (mevedel-view-fragment--update
       region '(:namespace interaction :id prompt :body "new body"))
      (should (equal "before\nnew body\nafter\n"
                     (mevedel-view-fragment-test--plain-buffer-string)))))

  :doc "restores point to the nearest equivalent offset inside an updated fragment"
  (with-temp-buffer
    (let ((region (mevedel-view-fragment-test--region)))
      (mevedel-view-fragment--reconcile
       region 'interaction
       '((:namespace interaction :id prompt :body "abcdef")))
      (goto-char (+ (overlay-start region) 3))
      (mevedel-view-fragment--update
       region '(:namespace interaction :id prompt :body "abcXYZdef"))
      (should (= (point) (+ (overlay-start region) 3)))))

  :doc "keeps point inside a fragment when the replacement shrinks"
  (with-temp-buffer
    (let ((region (mevedel-view-fragment-test--region)))
      (mevedel-view-fragment--reconcile
       region 'interaction
       '((:namespace interaction :id first :body "abcdef")
         (:namespace interaction :id second :body "second")))
      (goto-char (+ (overlay-start region) 5))
      (mevedel-view-fragment--update
       region '(:namespace interaction :id first :body "a"))
      (let ((bounds (mevedel-view-fragment--find-bounds
                     region 'interaction 'first)))
        (should (= (point) (1- (plist-get bounds :end))))
        (should (eq 'first
                    (get-text-property (point)
                                       'mevedel-view-fragment-id))))))

  :doc "keeps managed overlay covering siblings after child update and delete"
  (with-temp-buffer
    (let ((region (mevedel-view-fragment-test--region)))
      (mevedel-view-fragment--reconcile
       region 'interaction
       '((:namespace interaction :id first :body "first")
         (:namespace interaction :id second :body "second")))
      (mevedel-view-fragment--update
       region '(:namespace interaction :id first :body "first updated"))
      (should (string-match-p "second"
                              (buffer-substring-no-properties
                               (overlay-start region) (overlay-end region))))
      (should (mevedel-view-fragment--delete region 'interaction 'second))
      (should-not (string-match-p "second"
                                  (mevedel-view-fragment-test--plain-buffer-string)))
      (should (string-match-p "first updated"
                              (buffer-substring-no-properties
                               (overlay-start region) (overlay-end region))))))

  :doc "does not treat the exclusive fragment end as inside the fragment"
  (with-temp-buffer
    (let ((region (mevedel-view-fragment-test--region)))
      (mevedel-view-fragment--reconcile
       region 'interaction
       '((:namespace interaction :id first :body "first")
         (:namespace interaction :id second :body "second")))
      (let ((second-start (plist-get (mevedel-view-fragment--find-bounds
                                      region 'interaction 'second)
                                     :start)))
        (goto-char second-start)
        (mevedel-view-fragment--update
         region '(:namespace interaction :id first :body "first grows"))
        (should (= (point)
                   (plist-get (mevedel-view-fragment--find-bounds
                               region 'interaction 'second)
                              :start)))))))

(mevedel-deftest mevedel-view-fragment--replace-region ()
  ,test
  (test)
  :doc "preserves window point and window start inside an updated fragment"
  (let ((buffer (generate-new-buffer " *fragment-window*"))
        window)
    (unwind-protect
        (progn
          (setq window (split-window (selected-window)))
          (set-window-buffer window buffer)
          (with-current-buffer buffer
            (let ((region (mevedel-view-fragment-test--region)))
              (mevedel-view-fragment--reconcile
               region 'interaction
               '((:namespace interaction :id prompt
                              :body "line-1\nline-2\nline-3\nline-4")))
              (goto-char (+ (overlay-start region) 7))
              (set-window-point window (point))
              (set-window-start window (overlay-start region))
              (mevedel-view-fragment--update
               region '(:namespace interaction :id prompt
                                    :body "line-a\nline-b\nline-c\nline-d"))
              (should (= (window-point window) (+ (overlay-start region) 7)))
              (should (= (window-start window) (overlay-start region))))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))

  :doc "preserves window start when only the scroll anchor is inside"
  (let ((buffer (generate-new-buffer " *fragment-window-start*"))
        window)
    (unwind-protect
        (progn
          (setq window (split-window (selected-window)))
          (set-window-buffer window buffer)
          (with-current-buffer buffer
            (insert "prefix\n")
            (let ((region (mevedel-view-fragment-test--region (point) (point))))
              (mevedel-view-fragment--reconcile
               region 'interaction
               '((:namespace interaction :id prompt
                              :body "line-1\nline-2\nline-3\nline-4")))
              (goto-char (point-max))
              (set-window-point window (point))
              (set-window-start window (+ (overlay-start region) 1))
              (let ((window-point (window-point window))
                    (window-start-offset (- (window-start window)
                                            (overlay-start region))))
                (mevedel-view-fragment--update
                 region '(:namespace interaction :id prompt
                                      :body "line-a\nline-b\nline-c\nline-d"))
                (should (= (window-point window) window-point))
                (should (= (window-start window)
                           (+ (overlay-start region) window-start-offset)))))))
      (when (window-live-p window)
        (delete-window window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))


;;
;;; Deletion and bounds

(mevedel-deftest mevedel-view-fragment--delete ()
  ,test
  (test)
  :doc "deletes a fragment by container-scoped id"
  (with-temp-buffer
    (let ((region (mevedel-view-fragment-test--region)))
      (mevedel-view-fragment--reconcile
       region 'interaction
       '((:namespace interaction :id prompt :body "remove me")
         (:namespace interaction :id keep :body "keep me")))
      (should (mevedel-view-fragment--delete region 'interaction 'prompt))
      (should-not (string-match-p "remove me"
                                  (mevedel-view-fragment-test--plain-buffer-string)))
      (should (string-match-p "keep me"
                              (mevedel-view-fragment-test--plain-buffer-string))))))

(mevedel-deftest mevedel-view-fragment--find-bounds ()
  ,test
  (test)
  :doc "does not conflate distinct containers with equal bounds"
  (with-temp-buffer
    (let* ((region-a (cons (point) (point)))
           (text (mevedel-view-fragment--render
                  region-a '(:namespace interaction
                             :id shared
                             :body "body"))))
      (insert text)
      (setcdr region-a (point))
      (let ((region-b (cons (car region-a) (cdr region-a))))
        (should (equal region-a region-b))
        (should-not (eq region-a region-b))
        (should (mevedel-view-fragment--find-bounds
                 region-a 'interaction 'shared))
        (should-not (mevedel-view-fragment--find-bounds
                     region-b 'interaction 'shared))))))

(mevedel-deftest mevedel-view-fragment--bounds-at ()
  ,test
  (test)
  :doc "returns fragment bounds and container-scoped metadata at point"
  (with-temp-buffer
    (let ((region (mevedel-view-fragment-test--region)))
      (mevedel-view-fragment--reconcile
       region 'interaction
       '((:namespace interaction :id prompt :body "body")))
      (goto-char (overlay-start region))
      (let ((bounds (mevedel-view-fragment--bounds-at)))
        (should (= (plist-get bounds :start) (overlay-start region)))
        (should (= (plist-get bounds :end) (overlay-end region)))
        (should (eq region (plist-get bounds :region)))
        (should (eq 'interaction (plist-get bounds :namespace)))
        (should (eq 'prompt (plist-get bounds :id)))))))

(provide 'test-mevedel-view-fragment)
;;; test-mevedel-view-fragment.el ends here
