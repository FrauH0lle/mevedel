;;; test-mevedel-collaboration-tool-presentation.el --- Tool display tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise browser tool presentation through canonical collaboration records.

;;; Code:

(require 'json)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-collaboration-projection)
(require 'mevedel-skills-invoke)
(require 'mevedel-session-artifacts)
(require 'mevedel-tool-ptc)

(mevedel-deftest mevedel-collaboration-tool-presentation
  ()
  ,test
  (test)
  :doc "direct Skill displays its prepared body without changing envelope identity"
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "Skill" :category "mevedel"
                          :renderer #'mevedel-skills--render-skill-tool))
    (let* ((parsed '(:name "ToolCall" :tool-use-id "call-1"
                     :args (:expression "(Skill :name \"artifact-dashboard\")")
                     :result "<system-reminder>Dependency</system-reminder>\n# Dashboard"
                     :render-data
                     (:kind ptc :direct-tool "Skill" :outcome completed
                      :calls ((:id "call-1/1" :tool "Skill" :status success
                               :args (:name "artifact-dashboard")
                               :render-data (:kind skill-invocation
                                             :prompt "# Dashboard"
                                             :attachments ("artifact")))))))
           (original (copy-tree parsed))
           (record (mevedel-collaboration--tool-record parsed "fixture"))
           (display (plist-get record :presentation)))
      (should (equal "ToolCall" (plist-get record :name)))
      (should (equal "tool-call-1" (plist-get record :id)))
      (should (equal "Skill" (plist-get display :name)))
      (should (equal "artifact-dashboard" (plist-get display :detail)))
      (should (equal "# Dashboard" (plist-get display :body)))
      (should (equal "markdown" (plist-get display :format)))
      (should (equal original parsed))))

  :doc "dependency bodies and composed children survive the JSON allowlist"
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "Skill" :category "mevedel"
                          :renderer #'mevedel-skills--render-skill-tool))
    (mevedel-tool-register
     (mevedel-tool--create :name "ToolCall" :category "mevedel"
                          :renderer #'mevedel-tool-ptc--render))
    (let* ((record (mevedel-collaboration--tool-record
                    '(:name "Skill" :args (:name "dashboard") :result "Model guidance"
                      :render-data (:kind skill-invocation :prompt "# Dashboard"
                                    :attachments ("base" "missing")
                                    :attachment-bodies (("base" . "# Base\nDelivered body"))))
                    "fixture"))
           (wire (json-encode
                  (mevedel-collaboration--json-record record)))
           (display (plist-get record :presentation))
           (dependencies (plist-get display :attachments)))
      (should (string-search "presentation" wire))
      (should (= 2 (length dependencies)))
      (should (equal "# Base\nDelivered body" (plist-get (aref dependencies 0) :body)))
      (should (string-search "unavailable" (plist-get (aref dependencies 1) :body))))
    (let* ((record (mevedel-collaboration--tool-record
                    '(:name "ToolCall" :result "done"
                      :render-data (:kind ptc :outcome completed
                                    :calls ((:id "env/1" :tool "Read" :args (:file_path "a.el")
                                             :status success :batch 0 :result "A")
                                            (:id "env/2" :tool "Read" :args (:file_path "b.el")
                                             :status error :batch 0 :result "Error: missing"))))
                    "fixture"))
           (display (plist-get record :presentation))
           (children (plist-get display :children)))
      (should (equal "ToolCall" (plist-get display :name)))
      (should (= 2 (length children)))
      (should (equal "a.el" (plist-get (aref children 0) :detail)))
      (should (equal "0" (plist-get (aref children 0) :batch)))
      (should (equal "failed" (plist-get (aref children 1) :status)))
      (should (eq :json-false (plist-get (aref children 1) :collapsed)))))

  :doc "budgets the whole display and falls back for malformed metadata"
  (let* ((mevedel-collaboration--max-tool-result-bytes 2048)
         (parsed (list :name "Read" :args '(:file_path "a.el")
                       :result (make-string 10000 ?x)))
         (display (mevedel-collaboration-tool-presentation parsed)))
    (should (plist-get display :truncated))
    (should (< (length (plist-get display :body)) 2048))
    (should (< (string-bytes (json-encode display)) 4096))
    (mevedel-test--with-captured-diagnostics nil
      (should-not (mevedel-collaboration-tool-presentation
                   '(:name "Skill" :result "Visible fallback"
                     :render-data (:attachments malformed))))))

  :doc "envelope failure remains ToolCall and child denial is truthful"
  (let ((mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (mevedel-tool-register
     (mevedel-tool--create :name "ToolCall" :category "mevedel"
                          :renderer #'mevedel-tool-ptc--render))
    (let* ((data '(:kind ptc :direct-tool "Read" :outcome script-error :status error
                   :calls ((:id "env/1" :tool "Read" :status success :result "child OK"))))
           (display (mevedel-collaboration-tool-presentation
                     (list :name "ToolCall" :result "Error: envelope failed" :render-data data))))
      (should (equal "ToolCall" (plist-get display :name)))
      (should (equal "failed" (plist-get display :status)))
      (should (string-search "envelope failed" (plist-get display :body))))
    (let ((display (mevedel-collaboration-tool-presentation
                    '(:name "ToolCall" :result "Permission denied"
                      :render-data (:kind ptc :outcome tool-error
                                    :calls ((:id "env/1" :tool "Read" :status denied
                                             :result "Permission denied")))))))
      (should (equal "denied"
                     (plist-get (aref (plist-get display :children) 0) :status)))))

  :doc "real wrapped patch retains artifact cards and pending execution identity"
  (let* ((root (make-temp-file "mevedel-present-artifact-" t))
         (path (file-name-concat root "artifacts" "example.html")))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session (mevedel-session--create :name "fixture" :save-path root))
          (make-directory (file-name-directory path) t)
          (write-region "<h1>Fixture</h1>" nil path nil 'silent)
          (insert (propertize
                   "(:name \"ToolCall\" :args (:expression \"(ApplyPatch :patch ...)\"))\nApplied patch"
                   'gptel '(tool . "wrapped-patch")))
          (insert (mevedel-tool-render-data-format
                   `(:kind ptc :outcome completed :direct-tool "ApplyPatch"
                     :calls ((:id "wrapped-patch/1" :tool "ApplyPatch" :status success
                              :args (:patch "patch")
                              :render-data (:kind patch :files ((:kind add :added 1 :deleted 0 :diff "" :path ,path))))))
                   "wrapped-patch"))
          (put-text-property (point-min) (point-max)
                             'gptel '(tool . "wrapped-patch"))
          (let* ((room (list :data-buffer (current-buffer)
                             :pending-tools
                             (list (list :id "pending-id" :kind "tool" :name "ToolCall"
                                         :status "running" :pending t
                                         :baseline-tool-count 0 :baseline-record-count 0))))
                 (records (mevedel-collaboration--project-records room)))
            (should (= 1 (length records)))
            (should (equal "pending-id" (plist-get (car records) :id)))
            (should (equal "example.html" (plist-get (car records) :artifact)))
            (should-not (plist-get room :pending-tools))))
      (mevedel-collaboration--artifact-stat-invalidate)
      (delete-directory root t))))

(provide 'test-mevedel-collaboration-tool-presentation)
;;; test-mevedel-collaboration-tool-presentation.el ends here
