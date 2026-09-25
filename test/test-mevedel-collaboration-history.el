;;; test-mevedel-collaboration-history.el --- Archived browser history -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise archived publication through the same room and fetch boundaries.

;;; Code:
(require 'helpers (file-name-concat (file-name-directory load-file-name) "helpers"))
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-task)
(require 'mevedel-collaboration-transport)
(require 'mevedel-collaboration-artifact)
(require 'mevedel-session-artifacts)
(require 'mevedel-tool-render-data)
(require 'org)

(mevedel-deftest mevedel-collaboration-archived-artifacts
  (:doc "publishes and serves archived artifacts after compaction and a cold room start")
  (let* ((directory (make-temp-file "mevedel-history-" t))
         (artifact (file-name-concat directory "artifacts" "design.html"))
         (session (mevedel-session--create :name "history" :save-path directory
                                         :authority-mode 'pid-lock :current-segment 2))
         (data (generate-new-buffer " *history live*"))
         (guests (make-hash-table :test #'eql))
         (room (list :session session :data-buffer data :guests guests :transport 'test))
         sent)
    (unwind-protect
        (progn
          (make-directory (file-name-directory artifact) t)
          (write-region "<h1>Design</h1>" nil artifact nil 'silent)
          (with-temp-buffer
            (org-mode)
            (insert ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\nMake a design\n"
                    "#+begin_tool (ApplyPatch :patch \"patch\")\n"
                    "(:name \"ApplyPatch\" :args (:patch \"patch\"))\n\nApplied patch\n"
                    (mevedel-tool-render-data-format
                     ;; Model-authored relative paths resolve against the artifacts root.
                     '(:kind patch :files ((:kind add :path "design.html" :added 1 :deleted 0 :diff "")))
                     "old-patch")
                    "#+end_tool\n")
            (dotimes (_ 3)
              (goto-char (point-min))
              (search-forward "#+begin_tool")
              (let ((start (match-beginning 0)))
                (search-forward "#+end_tool")
                (org-entry-put (point-min) "GPTEL_BOUNDS"
                               (prin1-to-string `((tool (,start ,(point) "old-patch")))))))
            (write-region (point-min) (point-max)
                          (mevedel-session-artifacts-segment-path directory 1) nil 'silent))
          (with-current-buffer data (setq-local mevedel--session session))
          (puthash 1 (list :ready t :writable nil) guests)
          (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                     (lambda (_transport _peer frame) (push frame sent) t)))
            (mevedel-collaboration--publish room)
            (let* ((index (cl-find "history-index" sent :key (lambda (f) (plist-get f :t)) :test #'equal))
                   (record (car (append (plist-get index :records) nil))))
              (should (equal "design.html" (cdr (assoc "artifact" record))))
              (should-not (assoc "artifact-path" record))
              (let ((id (cdr (assoc "id" record))))
                (should (string-prefix-p "history-1-" id))
                (setq sent nil)
                (mevedel-collaboration--handle-artifact-get room 1 (list :reqId 1 :id id))
                (should (equal "<h1>Design</h1>" (base64-decode-string (plist-get (car sent) :data))))
              ;; Reconnect/cold publication reconstructs the same authority.
              (let ((cold (list :session session :data-buffer data :guests guests :transport 'test)))
                (should (equal (mevedel-collaboration--history-artifacts cold)
                               (mevedel-collaboration--history-artifacts room))))
              ;; Routine publishes do not reread archived transcript bodies.
              (cl-letf (((symbol-function 'mevedel-session-artifacts-read-segment)
                         (lambda (&rest _) (ert-fail "Archive reread during live publication"))))
                (mevedel-collaboration--publish room))
              (setq sent nil)
              (mevedel-collaboration--handle-history-get room 999 '(:reqId 2 :segment 1))
              (should-not sent)
              (dolist (number '(0 -1 2 1.5 "../session.meta.el"))
                (setq sent nil)
                (mevedel-collaboration--handle-history-get room 1 (list :reqId 2 :segment number))
                (should (plist-get (car sent) :error)))
              (setq sent nil)
              (mevedel-collaboration--handle-history-get room 1 '(:reqId 3 :segment 1))
              (should (eq t (plist-get (car sent) :final)))
              (let ((json (json-encode sent)))
                (should (string-match-p "Make a design" json))
                (should (string-match-p "design.html" json))
                (should-not (string-match-p "artifact-path\\|mevedel-render-data" json))
                (should-not (string-match-p (regexp-quote directory) json)))
              ;; Fast repeat requests receive an explicit retry response.
              (mevedel-collaboration--handle-history-get room 1 '(:reqId 4 :segment 1))
              (should (plist-get (car sent) :error))
              (delete-file artifact)
              (mevedel-collaboration--artifact-stat-invalidate)
              (should (plist-get (car (mevedel-collaboration--history-artifacts room)) :missing))
              (delete-file (mevedel-session-artifacts-segment-path directory 1))
              (plist-put (gethash 1 guests) :last-history-fetch nil)
              (mevedel-collaboration--handle-history-get room 1 '(:reqId 5 :segment 1))
              (should (plist-get (car sent) :error))))
          (should-not (plist-get room :records))))
      (kill-buffer data)
      (mevedel-collaboration--artifact-stat-invalidate)
      (delete-directory directory t))))

;;; test-mevedel-collaboration-history.el ends here
