;;; test-niwa-ptc-driver.el -- Tests for the ToolCall driver -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises nested dispatch, retention, audit settlement, and cancellation.

;;; Code:

(require 'niwa-ptc-driver)
(require 'niwa-tool-ptc)
(require 'niwa-ptc-checkpoint)
(require 'niwa-tool-fs)
(require 'niwa-execution-target)
(require 'cl-lib)
(require 'niwa-hooks)
(require 'niwa-permission-queue)
(require 'niwa-telemetry)
(require 'niwa-tool-registry)
(require 'niwa-tool-render-data)
(require 'niwa-tool-task)
(require 'niwa-pipeline)
(require 'niwa-turn)
(require 'niwa-tools)
(require 'niwa-tool-editing)
(require 'niwa-shared-editing)
(require 'niwa-skills-prompt)
(require 'niwa-agents)
(require 'niwa-structs)
(require 'niwa-workspace)
(require 'niwa-session-test-support
         (file-name-concat (file-name-directory
                            (or buffer-file-name load-file-name byte-compile-current-file))
                           "niwa-session-test-support"))
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

(defun test-niwa-ptc-driver--gptel-tools (&rest names)
  "Return the gptel tool structs registered for NAMES."
  (delq nil (mapcar (lambda (name)
                      (when-let* ((tool (ignore-errors (niwa-tool-ensure name))))
                        (niwa-tool-gptel-tool tool)))
                    names)))

(defun test-niwa-ptc-driver--visible (result)
  "Return RESULT without the hidden render-data block the pipeline appends."
  (if (string-match "\n?<!-- niwa-render-data -->" result)
      (substring result 0 (match-beginning 0))
    result))

(defun test-niwa-ptc-driver--register-probe
    (handler &optional async-p check-permission-async)
  "Register a real pipeline tool using HANDLER for driver tests."
  (let* ((args '((value string :required "Value")))
         (tool (niwa-tool--create
                :name "PTCProbe" :category "niwa"
                :handler handler :description "ToolCall test probe"
                :prompt "ToolCall test probe" :args args
                :read-only-p t :async-p async-p
                :check-permission-async check-permission-async))
         (gptel-tool
          (gptel-make-tool
           :name "PTCProbe" :description "ToolCall test probe"
           :function #'ignore :args nil :async t :category "niwa")))
    (setf (niwa-tool-gptel-tool tool) gptel-tool)
    (niwa-tool-register tool)))

(defun test-niwa-ptc-driver--select-probe (buffer)
  "Make PTCProbe the nested roster in BUFFER."
  (with-current-buffer buffer
    (setq-local gptel-tools
                (test-niwa-ptc-driver--gptel-tools "PTCProbe"))))

(defun test-niwa-ptc-driver--run (buffer script &optional raw-p)
  "Run SCRIPT through ToolCall in BUFFER and return its visible result.
With RAW-P, retain the full pipeline result including hidden render data."
  (let ((deadline (+ (float-time) 10.0))
        done result)
    (with-current-buffer buffer
      (niwa-pipeline-run-tool
       (niwa-tool-ensure "ToolCall")
       (lambda (value) (setq result value done t))
       (list :expression script))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should done)
    (if raw-p result (test-niwa-ptc-driver--visible result))))

(defun test-niwa-ptc-driver--telemetry (session)
  "Return persisted and pending telemetry entries for SESSION."
  (let ((path (file-name-concat (niwa-session-save-path session)
                                niwa-telemetry-file-name))
        entries)
    (when (file-readable-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (condition-case nil
            (while t (push (read (current-buffer)) entries))
          (end-of-file))))
    (append (nreverse entries)
            (reverse (niwa-session-telemetry-pending session)))))


;;
;;; Retained child data

(niwa-deftest niwa-ptc-driver--bound-strings ()
  ,test
  (test)
  :doc "truncates every nested string without changing non-string values"
  (should
   (equal '("abc..." (:nested "def..." 7))
          (niwa-ptc-driver--bound-strings
           '("abcdef" (:nested "defghi" 7)) 3))))

(niwa-deftest niwa-ptc-driver--child-args ()
  ,test
  (test)
  :doc "bounds one retained argument string at the configured limit"
  (let* ((text (make-string (1+ niwa-ptc-driver--arg-value-max) ?x))
         (bounded (plist-get (niwa-ptc-driver--child-args
                              (list :value text))
                             :value)))
    (should (= (+ niwa-ptc-driver--arg-value-max 3) (length bounded)))
    (should (string-suffix-p "..." bounded)))
  :doc "drops aggregate or deeply nested arguments before recursive truncation"
  (should-not
   (niwa-ptc-driver--child-args
    (list :value (make-string (1+ niwa-ptc-driver--arg-total-max) ?x))))
  (should-not
   (niwa-ptc-driver--child-args
    (list :value (make-list (1+ niwa-ptc-driver--arg-node-max) "x")))))

(niwa-deftest niwa-ptc-driver--child-render-data ()
  ,test
  (test)
  :doc "retains details at the value limit and drops oversized details"
  (let ((at-limit (make-string niwa-ptc-driver--child-detail-max ?x))
        (over-limit (make-string (1+ niwa-ptc-driver--child-detail-max) ?x)))
    (should (eq at-limit (niwa-ptc-driver--child-render-data at-limit)))
    (should-not (niwa-ptc-driver--child-render-data over-limit))
    (should-not (niwa-ptc-driver--child-render-data nil)))
  :doc "drops guest closures from durable child render data"
  (let ((state (niwa-ptc-start "(lambda () 1)" nil)))
    (unwind-protect
        (let ((closure (cadr (niwa-ptc-step state))))
          (should-not (niwa-ptc-driver--child-render-data closure)))
      (niwa-ptc-close state)))
  :doc "keeps small accepted approval facts when bulky child details are omitted"
  (let* ((approvals '((:source reviewer :outcome allow-once :reason "Exact task")))
         (data (list :kind 'probe :detail (make-string 5000 ?x)
                     :permission-approvals approvals)))
    (should (equal (list :permission-approvals approvals)
                   (niwa-ptc-driver--child-render-data data)))
    (should-not
     (niwa-ptc-driver--child-render-data
      (list :permission-approvals (make-string 5000 ?x))))))



;;
;;; Argument conversion

(niwa-deftest niwa-ptc-driver--convert-args
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)

  :doc "preserves nested declared object keys, arrays, booleans and null for wrapped tools"
  (let* ((universe (obarray-make))
         (tool (niwa-tool--create
                :name "Probe" :args
                '((payload object :required "Payload"
                           :properties (:flag (:type boolean)
                                        :items (:type array :items (:type object
                                                  :properties (:label (:type string))))
                                        :empty (:type object)
                                        :nothing (:type string))))))
         (guest (list (intern ":payload" universe)
                      (list (intern ":flag" universe) (intern ":json-false" universe)
                            (intern ":items" universe)
                            (vector (list (intern ":label" universe) "value"))
                            (intern ":empty" universe) nil
                            (intern ":nothing" universe) (intern ":null" universe))))
         (args (niwa-ptc-driver--convert-args tool guest))
         (payload (plist-get args :payload)))
    (should (eq :json-false (plist-get payload :flag)))
    (should (equal "value" (plist-get (aref (plist-get payload :items) 0) :label)))
    (should (hash-table-p (plist-get payload :empty)))
    (should (eq :null (plist-get payload :nothing)))
    (should (string-search "false" (json-serialize args :null-object :null :false-object :json-false))))

  :doc "converts Ask questions and string-or-object options from keyword plists"
  (progn
    (require 'niwa-tool-ask)
    (niwa-tool-ask-register)
    (let* ((u (obarray-make))
           (k (lambda (name) (intern (concat ":" name) u)))
           (guest (list (funcall k "questions")
                        (vector (list (funcall k "question") "Where?"
                                      (funcall k "options")
                                      (vector "Here (Recommended)"
                                              (list (funcall k "label") "There"
                                                    (funcall k "description") "Far"))))))
           (question (aref (plist-get (niwa-ptc-driver--convert-args
                                       (niwa-tool-get "Ask") guest)
                                      :questions)
                           0))
           (options (plist-get question :options)))
      (ert-info ("Ask fields declared only in nested schemas reach the form")
        (should (equal "Where?" (plist-get question :question)))
        (should (equal "Here (Recommended)" (aref options 0)))
        (should (equal "There" (plist-get (aref options 1) :label)))
        (should (equal "Far" (plist-get (aref options 1) :description))))))

  :doc "normalizes obvious boolean keywords in nested Ask question schemas"
  (progn
    (require 'niwa-tool-ask)
    (niwa-tool-ask-register)
    (dolist (case '((":false" . :json-false) (":true" . t)))
      (let* ((u (obarray-make))
             (args (list (intern ":questions" u)
                         (vector (list (intern ":question" u) "Which?"
                                       (intern ":multiSelect" u) (intern (car case) u)))))
             (converted (niwa-ptc-driver--convert-args (niwa-tool-get "Ask") args)))
        (should (eq (cdr case)
                    (plist-get (aref (plist-get converted :questions) 0) :multiSelect))))))

  :doc "interns declared guest keywords as host keywords"
  (progn
    (niwa-tool-fs--register)
    (let* ((tool (niwa-tool-get "Read"))
           (guest (list (intern ":file_path" (obarray-make)) "/tmp/x")))
      (should (equal '(:file_path "/tmp/x")
                     (niwa-ptc-driver--convert-args tool guest)))))

  :doc "rejects an undeclared argument and names what the tool accepts"
  (progn
    (niwa-tool-fs--register)
    (let* ((tool (niwa-tool-get "Read"))
           (guest (list (intern ":nonesuch" (obarray-make)) 1)))
      ;; Emacs renders ` and ' as curly quotes in error text, so match the
      ;; stable parts rather than the quoting.
      (let ((message (cadr (should-error
                            (niwa-ptc-driver--convert-args tool guest)
                            :type 'error))))
        (should (string-match-p "has no argument" message))
        (should (string-match-p "nonesuch" message))
        (should (string-match-p "file_path" message)))))

:doc "refuses JSON commas and bare true, false or null in data no schema describes"
  (let* ((universe (obarray-make))
         (tool (niwa-tool--create
                :name "Probe" :args '((payload object :required "Payload"))))
         (key (intern ":payload" universe))
         (refused (lambda (value)
                    (error-message-string
                     (should-error (niwa-ptc-driver--convert-args tool (list key value)))))))
    (should (string-match-p "not JSON commas"
                            (funcall refused (list (intern ":points" universe)
                                                   (vector (vector 0 (list (intern "," universe) 1)))))))
    (dolist (name '("true" "false" "null"))
      (should (string-match-p (format "not the bare symbol %s" name)
                              (funcall refused (list (intern ":flag" universe) (intern name universe))))))
    ;; Keywords, t, nil, strings and other symbols pass unchanged.
    (let ((value (list (intern ":a" universe) t (intern ":b" universe) nil
                       (intern ":c" universe) "false" (intern ":d" universe) (intern "falsey" universe))))
      (should (equal value (plist-get (niwa-ptc-driver--convert-args tool (list key value)) :payload)))))

  :doc "rejects arguments that are not keyword and value pairs"
  (progn
    (niwa-tool-fs--register)
    (let ((tool (niwa-tool-get "Read")))
      (dolist (case `((("positional") . "must be :keyword value pairs")
                      ((,(intern ":file_path" (obarray-make))) . "has no value")
                      ((:unknown) . "has no value")
                      ((:unknown "x") . "has no argument")))
        (should (string-match-p
                 (cdr case)
                 (error-message-string
                  (should-error
                   (niwa-ptc-driver--convert-args tool (car case))))))))))


;;
;;; Driving a script through the pipeline

(niwa-deftest niwa-ptc-driver-run
  (:vars ((niwa-ptc-driver--next-envelope-id 0)
          (niwa-shared-editing--runtimes (make-hash-table :test #'equal))
          (niwa-artifact-lease--held (make-hash-table :test #'equal)))
   :vars* ((root (file-name-as-directory
                  (make-temp-file "niwa-ptc-" t)))
           (save-path (make-temp-file "niwa-ptc-save-" t))
           (workspace (niwa-workspace--create
                       :type 'test :id root :root root :name "ptc"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session--create
                     :name "ptc" :workspace workspace
                     :save-path save-path :working-directory root
                     :permission-mode 'full-auto
                     :touched-files (make-hash-table :test #'equal)))
           (buffer (generate-new-buffer " *niwa-ptc-driver*")))
   :before-each
   (progn
     (niwa-tool-clear-registry)
     (niwa-tool-fs--register)
     (niwa-tool-ptc--register)
     (with-temp-file (file-name-concat root "one.txt") (insert "alpha\n"))
     (with-temp-file (file-name-concat root "two.txt") (insert "beta\n"))
     (with-current-buffer buffer
       (setq-local default-directory root
                   niwa--workspace workspace
                   niwa--session session
                   gptel-tools (test-niwa-ptc-driver--gptel-tools "Read"))))
   :after-each
   (progn
     ;; SharedCreate owns a helper and a renewing item lease independently of
     ;; the ToolCall buffer.  Retire both before removing their target store.
     (niwa-shared-editing-stop)
     (niwa-artifact-lease-release-all)
     (maphash (lambda (directory _held) (niwa-artifact-lease--forget directory))
              niwa-artifact-lease--held)
     (niwa-tool-clear-registry)
     (when (buffer-live-p buffer) (kill-buffer buffer))
     (when (niwa-session-lease session)
       (niwa-session-persistence-lock-release save-path session)
       (niwa-workspace-clear-registry))
     (when (file-directory-p root) (delete-directory root t))
     (when (file-directory-p save-path) (delete-directory save-path t))))
  ,test
  (test)

  :doc "TaskWrite accepts task plists in vectors and quoted lists through ToolCall"
  (progn
    (niwa-tool-task--register)
    (with-current-buffer buffer
      (setq-local gptel-tools
                  (test-niwa-ptc-driver--gptel-tools "TaskWrite")))
    (dolist (form '("[(:subject \"First\") (:subject \"Second\" :description \"Details\" :status \"in_progress\" :owner \"Review\" :blockedBy [1] :metadata nil) (:subject \"Third\" :status \"completed\")]"
                    "'((:subject \"First\") (:subject \"Second\" :description \"Details\" :status \"in_progress\" :owner \"Review\" :blockedBy (1) :metadata nil) (:subject \"Third\" :status \"completed\"))"))
      (setf (niwa-session-tasks session) nil)
      (should (string-prefix-p
               "Created 3 tasks:"
               (test-niwa-ptc-driver--run
                buffer (format "(TaskWrite :tasks %s)" form))))
      (let ((tasks (niwa-session-tasks session)))
        (should (equal '(1 2 3) (mapcar #'niwa-task-id tasks)))
        (should (equal '("First" "Second" "Third")
                       (mapcar #'niwa-task-subject tasks)))
        (should (equal '(pending in-progress completed)
                       (mapcar #'niwa-task-status tasks)))
        (should (equal "Details" (niwa-task-description (cadr tasks))))
        (should (equal "Review" (niwa-task-owner (cadr tasks))))
        (should (equal '(1) (niwa-task-blocked-by (cadr tasks))))
        (should (hash-table-p (niwa-task-metadata (cadr tasks)))))))

  :doc "TaskWrite metadata activity reaches the rendered task row on create and update"
  (progn
    (niwa-tool-task--register)
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/reviewer"
                      (niwa-agent-record--create :path "/root/reviewer"))))
    (with-current-buffer buffer
      (setq-local gptel-tools
                  (test-niwa-ptc-driver--gptel-tools "TaskWrite")))
    (should (string-prefix-p
             "Created 1 task"
             (test-niwa-ptc-driver--run
              buffer
              "(TaskWrite :tasks [(:subject \"Review\" :status \"in_progress\" :owner \"/root/reviewer\" :metadata (:activity \"Checking callbacks\"))])")))
    (should (string-match-p
             "Checking callbacks"
             (niwa-tool-task--format-one (car (niwa-session-tasks session)))))
    (should (string-prefix-p
             "Updated 1 task"
             (test-niwa-ptc-driver--run
              buffer "(TaskWrite :tasks [(:id 1 :metadata (:activeForm \"Testing cleanup\"))])")))
    (should (string-match-p
             "Testing cleanup"
             (niwa-tool-task--format-one (car (niwa-session-tasks session))))))

  :doc "SharedEdit accepts nested list and vector arrays through the real ToolCall pipeline"
  (unwind-protect
      (progn
        (niwa-tool-editing--register)
        (with-current-buffer buffer
          (setq-local gptel-tools
                      (test-niwa-ptc-driver--gptel-tools "SharedCreate" "SharedEdit")))
        (let* ((created (test-niwa-ptc-driver--run
                         buffer "(SharedCreate :kind \"whiteboard\" :title \"Arrays\")"))
               (id (plist-get (json-parse-string created :object-type 'plist) :id)))
          (dolist (form '("[(:id \"v\" :after (:id \"v\" :type \"rectangle\" :x 710 :y 143 :width 85 :height 10 :roundness (:type 3)))]"
                          "'((:id \"l\" :after (:id \"l\" :type \"freedraw\" :x 0 :y 0 :width 10 :height 10 :points ((0 0) (10 10)))))"))
            (let (reply)
              (with-current-buffer buffer
                (niwa-ptc-driver-run
                 (lambda (value) (setq reply value))
                 (format "(SharedEdit :id %S :action \"patch\" :changes %s)" id form)
                 '("SharedEdit") '("SharedEdit")))
              (let ((deadline (+ (float-time) 10)))
                (while (and (not reply) (< (float-time) deadline))
                  (accept-process-output nil 0.01)))
              (should (eq (plist-get reply :status) 'success))
              (should (plist-get reply :media))))
          (let* ((state (niwa-shared-editing--read (niwa-session-workspace session) id))
                 (revision (plist-get state :revision)))
            (should (= revision 3)))
          ;; A model's redraw: deletions by hash with null, then shapes with
          ;; nested plists, point vectors and floats, in one patch.
          (let* ((view (let (text)
                         (with-current-buffer buffer
                           (niwa-tool-editing-view (list id) (lambda (v) (setq text (plist-get v :text)))))
                         (let ((deadline (+ (float-time) 10)))
                           (while (and (not text) (< (float-time) deadline))
                             (accept-process-output nil 0.01)))
                         text))
                 (hash (and (string-match "\\([0-9a-f]\\{12\\}\\) {\"id\":\"v\"" view)
                            (match-string 1 view)))
                 reply)
            (should hash)
            (with-current-buffer buffer
              (niwa-ptc-driver-run
               (lambda (value) (setq reply value))
               (format "(SharedEdit :id %S :action \"patch\" :changes [(:id \"v\" :hash %S :after :null) (:id \"box\" :after (:id \"box\" :type \"rectangle\" :x 0 :y 140 :width 300 :height 160 :roundness (:type 3) :roughness 0)) (:id \"link\" :after (:id \"link\" :type \"arrow\" :x 310 :y 220 :width 280 :height 0 :points [[0 0] [280 0]] :startBinding (:elementId \"box\" :fixedPoint [1 0.5] :mode \"orbit\") :startArrowhead :null :endArrowhead \"arrow\"))])"
                       id hash)
               '("SharedEdit") '("SharedEdit")))
            (let ((deadline (+ (float-time) 10)))
              (while (and (not reply) (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (should (equal 'success (plist-get reply :status)))
            (should (= 4 (plist-get (niwa-shared-editing--read (niwa-session-workspace session) id) :revision))))
          ;; Malformed objects say what an object is, and name JSON when written.
          (dolist (case '(("[{\"id\":\"v\",\"after\":null}]" . "not JSON {...} with commas: {")
                          ("[(:id \"v\" :after)]" . "not this value: (:id \"v\" :after)")))
            (let (reply)
              (with-current-buffer buffer
                (niwa-ptc-driver-run
                 (lambda (value) (setq reply value))
                 (format "(SharedEdit :id %S :action \"patch\" :changes %s)" id (car case))
                 '("SharedEdit") '("SharedEdit")))
              (let ((deadline (+ (float-time) 10)))
                (while (and (not reply) (< (float-time) deadline))
                  (accept-process-output nil 0.01)))
              (should (string-search "Objects are keyword plists such as (:id \"a\" :after nil)"
                                     (plist-get reply :result)))
              (should (string-search (cdr case) (plist-get reply :result)))))
          ;; The mistakes a model made drawing a graph: JSON commas between
          ;; points, a bare false, and element fields beside the change.
          (dolist (case '(("[(:id \"g\" :after (:id \"g\" :type \"line\" :x 0 :y 0 :width 10 :height 10 :points [[0.0,0.0],[6.0,29.6]]))]"
                           . "Separate list and vector elements with spaces, not JSON commas")
                          ("[(:id \"g\" :after (:id \"g\" :type \"freedraw\" :x 0 :y 0 :width 10 :height 10 :simulatePressure false :points [[0 0] [6 29.6]]))]"
                           . "Write :json-false or nil for false")
                          ("[(:id \"g\" :type \"line\" :x 0 :y 0 :points [[0 0] [6 29.6]])]"
                           . "put the element's own fields in after")))
            (let (reply)
              (with-current-buffer buffer
                (niwa-ptc-driver-run
                 (lambda (value) (setq reply value))
                 (format "(SharedEdit :id %S :action \"patch\" :changes %s)" id (car case))
                 '("SharedEdit") '("SharedEdit")))
              (let ((deadline (+ (float-time) 10)))
                (while (and (not reply) (< (float-time) deadline))
                  (accept-process-output nil 0.01)))
              (should (eq 'error (plist-get reply :status)))
              (should (string-search (cdr case) (plist-get reply :result)))))
          (should (= 4 (plist-get (niwa-shared-editing--read (niwa-session-workspace session) id)
                                  :revision)))))
    (with-current-buffer buffer (niwa-shared-editing-stop)))

  :doc "indirect argument calls finish before the outer call and retain both audits"
  (let (values)
    (test-niwa-ptc-driver--register-probe
     (lambda (args)
       (push (plist-get args :value) values)
       (list :result (concat "seen-" (plist-get args :value)))))
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (should (equal "seen-seen-inner"
                     (test-niwa-ptc-driver--run
                      buffer "(PTCProbe :value (funcall 'PTCProbe :value \"inner\"))"))))
    (should (equal '("seen-inner" "inner") values)))

  :doc "direct calls preserve long arguments, renderer data and supported media"
  (let ((text (make-string 5000 ?x)) result done)
    (test-niwa-ptc-driver--register-probe
     (lambda (_args) (list :result "done" :render-data (list :kind 'probe :text text)
                           :media '((:kind image :mime "image/png" :data "aGVsbG8=")))))
    (test-niwa-ptc-driver--select-probe buffer)
    (with-current-buffer buffer
      (niwa-ptc-driver-run
       (lambda (value) (setq result value done t))
       (format "(PTCProbe :value %S)" text) '("PTCProbe") '("PTCProbe")))
    (let ((deadline (+ (float-time) 2)))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should done)
    (let ((child (car (plist-get (plist-get result :render-data) :calls))))
      (should (equal text (plist-get (plist-get child :args) :value)))
      (should (equal text (plist-get (plist-get child :render-data) :text))))
    (should (plist-get result :media)))

  :doc "direct calls retain the underlying unlimited output policy"
  (let ((text (make-string 40000 ?x)))
    (test-niwa-ptc-driver--register-probe (lambda (_args) (list :result text)))
    (test-niwa-ptc-driver--select-probe buffer)
    (should (equal text (test-niwa-ptc-driver--run buffer "(PTCProbe :value \"x\")"))))

  :doc "direct text results are returned intact without a second full audit copy"
  (let ((text (make-string 200000 ?x)))
    (test-niwa-ptc-driver--register-probe (lambda (_args) (list :result text)))
    (test-niwa-ptc-driver--select-probe buffer)
    (let* ((result (test-niwa-ptc-driver--run
                    buffer "(PTCProbe :value \"x\")" t))
           (parsed (niwa-tool-render-data-extract result))
           (child (car (plist-get (cdr parsed) :calls))))
      (should (equal text (car parsed)))
      (should (equal (niwa-ptc-driver--preview text) (plist-get child :result)))
      (should (equal "PTCProbe" (plist-get child :tool)))))

  :doc "direct calls preserve non-text child values through post-tool hooks"
  (dolist (raw '(37 nil (:count 3) [1 2]))
    (let (result done observed)
      (test-niwa-ptc-driver--register-probe
       (lambda (_args) (list :result raw)))
      (test-niwa-ptc-driver--select-probe buffer)
      (with-current-buffer buffer
        (setq-local niwa-post-tool-use-functions
                    (list (lambda (event)
                            (when (equal "PTCProbe" (plist-get event :tool-name))
                              (push (plist-get event :result) observed))
                            nil)))
        (niwa-ptc-driver-run
         (lambda (value) (setq result value done t))
         "(PTCProbe :value \"x\")" '("PTCProbe") '("PTCProbe")))
      (let ((deadline (+ (float-time) 2)))
        (while (and (not done) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should done)
      (should (eq 'success (plist-get result :status)))
      (should (equal raw (plist-get result :result)))
      (should (equal (list raw) observed))))

  :doc "direct non-text calls retain child identity in the provider transcript"
  (dolist (raw '(37 nil (:count 3) [1 2]))
    (let (result)
      (test-niwa-ptc-driver--register-probe
       (lambda (_args) (list :result raw)))
      (test-niwa-ptc-driver--select-probe buffer)
      (setq result
            (test-niwa-ptc-driver--run
             buffer "(PTCProbe :value \"x\")" t))
      (let* ((parsed (niwa-tool-render-data-extract result))
             (child (car (plist-get (cdr parsed) :calls))))
        (should (equal (gptel--to-string raw) (car parsed)))
        (should (equal "PTCProbe" (plist-get child :tool)))
        (should (plist-get (cdr parsed) :direct-tool))
        (should (equal raw (plist-get child :result))))))

  :doc "delivers child hook instructions even when composition discards its result"
  (progn
   (test-niwa-ptc-driver--register-probe (lambda (_args) '(:result "discarded")))
  (test-niwa-ptc-driver--select-probe buffer)
  (with-current-buffer buffer
    (setq-local niwa-post-tool-use-functions
                (list (lambda (event)
                        (when (equal "PTCProbe" (plist-get event :tool-name))
                          '(:additional-context ("Required child guidance")))))))
  (let ((niwa-ptc-composable-tools '("PTCProbe")))
    (should (string-search
             "Required child guidance"
             (test-niwa-ptc-driver--run
              buffer "(progn (PTCProbe :value \"x\") \"final\")")))))

  :doc "nested reads deliver path-skill discovery when Skill is only in the catalog"
  (let ((skill (niwa-skill--create :name "path-helper" :description "Inspect Elisp"
                                      :path-patterns '("*.el") :active-p nil))
        (path (file-name-concat root "example.el")))
    (with-temp-file path (insert "(defun example () t)"))
    (niwa-tool-ensure "Skill")
    (setf (niwa-session-skills session) (list skill)
          (niwa-session-tool-catalog session) '((("niwa" "Skill") . "Load guidance")))
    (with-current-buffer buffer
      (setq-local gptel-tools (test-niwa-ptc-driver--gptel-tools "Read" "ToolCall")
                  niwa--current-request (niwa-request--create :id "nested-path" :session session))
      (niwa-skills-install-activation-hook))
    (test-niwa-ptc-driver--run buffer (format "(Read :file_path %S)" path))
    (should (niwa-skill-active-p skill))
    (with-current-buffer buffer
      (should (string-search "path-helper"
                             (prin1-to-string niwa-reminders--turn-events)))))

  :doc "runs scripts with no tool calls and returns their final values"
  (dolist (case '(("(+ 1 2)" . "3") ("nil" . "nil")
                  ("\"text\"" . "text") ("'(1 2)" . "(1 2)")))
    (should (equal (cdr case)
                   (test-niwa-ptc-driver--run buffer (car case)))))

  :doc "boolean keyword aliases reach the pipeline and bare true names the fix"
  (let (received)
    (test-niwa-ptc-driver--register-probe
     (lambda (args) (push (plist-get args :value) received) '(:result "accepted")))
    (setf (niwa-tool-args (niwa-tool-get "PTCProbe"))
          '((value boolean :required "Flag")))
    (test-niwa-ptc-driver--select-probe buffer)
    (dolist (literal '(":false" ":true"))
      (should (equal "accepted" (test-niwa-ptc-driver--run
                                 buffer (format "(PTCProbe :value %s)" literal)))))
    (should (equal '(t :json-false) received))
    (should (string-search "use t for true"
                           (test-niwa-ptc-driver--run buffer "(PTCProbe :value true)")))
    (should (= 2 (length received))))

  :doc "Bash without its required description fails before nested execution"
  (let (called)
    (niwa-tool-ensure "Bash")
    (with-current-buffer buffer
      (setq-local gptel-tools (test-niwa-ptc-driver--gptel-tools "Bash" "ToolCall")))
    (cl-letf (((symbol-function 'niwa-tool-exec--bash)
               (lambda (&rest _) (setq called t))))
      (should (string-search "description"
                             (test-niwa-ptc-driver--run buffer "(Bash :command \"pwd\")"))))
    (should-not called))

  :doc "rejects a closure as the script's final value"
  (should
   (string-match-p
    "closures cannot cross"
    (test-niwa-ptc-driver--run buffer "(lambda () 1)")))

  :doc "rejects a closure before passing it to a nested tool"
  (let (called)
    (test-niwa-ptc-driver--register-probe
     (lambda (_args) (setq called t) '(:result "unexpected")))
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (should
       (string-match-p
        "closures cannot cross"
        (test-niwa-ptc-driver--run
         buffer "(PTCProbe :value (lambda () 1))"))))
    (should-not called))

  :doc "settles errors raised after the timer-driven pump starts"
  (let ((niwa-ptc-composable-tools '("Read")))
    (cl-letf (((symbol-function 'niwa-ptc-driver--child-args)
               (lambda (_args) (error "Audit retention failed"))))
      (should
       (string-match-p
        "Audit retention failed"
        (test-niwa-ptc-driver--run
         buffer
         (format "(progn (Read :file_path %S))"
                 (file-name-concat root "one.txt")))))))

  :doc "settles errors raised by an asynchronous child callback"
  (let ((niwa-ptc-parallelism 1)
        result)
    (test-niwa-ptc-driver--register-probe
     (lambda (child-callback _args)
       (run-at-time 0 nil
                    (lambda ()
                      (funcall child-callback '(:result "done")))))
     t)
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (cl-letf (((symbol-function 'niwa-ptc-driver--classify-outcome)
                 (lambda (&rest _) (error "Async completion failed"))))
        (setq result
              (test-niwa-ptc-driver--run
               buffer "(parallel (PTCProbe :value \"one\")
                                (PTCProbe :value \"two\"))" t))
        (should (string-match-p "Async completion failed" result))
        (let* ((render-data (cdr (niwa-tool-render-data-extract result)))
               (calls (plist-get render-data :calls)))
          (should (= 2 (length calls)))
          (should (equal '(success interrupted)
                         (mapcar (lambda (call)
                                   (plist-get call :status))
                                 calls)))
          (should-not
           (seq-find
            (lambda (call)
              (memq (plist-get call :status) '(queued running)))
            calls))))))

  :doc "settles with an error when final checkpoint persistence fails"
  (let ((durable-session
         (niwa-session--create
          :name "ptc" :workspace workspace :save-path save-path
          :working-directory root
          :execution-target (niwa-execution-target-create root)
          :permission-mode 'full-auto
          :touched-files (make-hash-table :test #'equal))))
    (with-current-buffer buffer
      (setq-local niwa--session durable-session))
    (cl-letf (((symbol-function 'niwa-ptc-checkpoint-start)
               (lambda (&rest _) t))
              ((symbol-function 'niwa-ptc-checkpoint-update)
               (lambda (&rest _) nil)))
      (should
       (string-match-p
        "final audit could not be persisted"
        (test-niwa-ptc-driver--run buffer "(+ 1 2)")))))

  :doc "checkpoints a retained-agent script in its durable root session"
  (let ((durable-session
         (niwa-session--create
          :name "ptc" :workspace workspace :save-path save-path
          :working-directory root
          :execution-target (niwa-execution-target-create root)
          :permission-mode 'full-auto
          :touched-files (make-hash-table :test #'equal)))
        (started nil))
    (with-current-buffer buffer
      (setq-local niwa--session durable-session)
      (setq-local niwa--agent-invocation
                  (niwa-agent-invocation--create :path "/root/worker")))
    (cl-letf (((symbol-function 'niwa-ptc-checkpoint-start)
               (lambda (session source _id _script)
                 (setq started (list session source)) t))
              ((symbol-function 'niwa-ptc-checkpoint-update)
               (lambda (&rest _) t)))
      (should (equal "3" (test-niwa-ptc-driver--run buffer "(+ 1 2)")))
      (should (equal (list durable-session buffer) started))))

  :doc "fallback envelope identities do not depend on a process-local counter"
  (let ((durable-session
         (niwa-session--create
          :name "ptc" :workspace workspace :save-path save-path
          :working-directory root
          :execution-target (niwa-execution-target-create root)
          :permission-mode 'full-auto
          :touched-files (make-hash-table :test #'equal)))
        (entropy 0)
        ids)
    (with-current-buffer buffer
      (setq-local niwa--session durable-session))
    (cl-letf (((symbol-function 'niwa-pipeline-active-tool-use-id) (lambda () nil))
              ((symbol-function 'emacs-pid) (lambda () 11))
              ((symbol-function 'current-time) (lambda () '(1 2 3 4)))
              ((symbol-function 'random) (lambda (&optional _) (cl-incf entropy)))
              ((symbol-function 'niwa-ptc-checkpoint-start)
               (lambda (_session _source id _script) (push id ids) t))
              ((symbol-function 'niwa-ptc-checkpoint-update) (lambda (&rest _) t)))
      (should (equal "3" (test-niwa-ptc-driver--run buffer "(+ 1 2)")))
      (should (equal "3" (test-niwa-ptc-driver--run buffer "(+ 1 2)"))))
    (should (= 2 (length ids)))
    (should-not (equal (car ids) (cadr ids)))
    (dolist (id ids)
      (should (string-match-p "\\`ptc-[[:xdigit:]]\\{32\\}\\'" id))))

  :doc "checks the logical serialized size of a shared final value"
  (let ((niwa-ptc-max-value-bytes 100))
    (should
     (string-match-p
      "value byte budget"
      (test-niwa-ptc-driver--run
       buffer
       "(let* ((x \"abcdefghij\")
               (ys (mapcar (lambda (_) x)
                           (list 1 2 3 4 5 6 7 8 9 10 11 12))))
          ys)"))))

  :doc "dispatches nested calls through the pipeline and aggregates them"
  (let ((result (test-niwa-ptc-driver--run
                 buffer
                 (format "(let ((out nil))
                            (dolist (p (list %S %S))
                              (push (Read :file_path p) out))
                            (length out))"
                         (file-name-concat root "one.txt")
                         (file-name-concat root "two.txt")))))
    (should (equal "2" result)))

  :doc "returns a nested failure as a value the script can branch on"
  (let ((result (test-niwa-ptc-driver--run
                 buffer
                 (format "(let ((r (Read :file_path %S)))
                            (if (plist-get r :error) :handled :unexpected))"
                         (file-name-concat root "missing.txt")))))
    (should (equal ":handled" result)))

  :doc "uses structured child status and assigns synthetic identity"
  (let (child-id)
    (test-niwa-ptc-driver--register-probe
     (lambda (_args)
       (setq child-id (niwa-pipeline-active-tool-use-id))
       '(:result "Error: harmless data" :status success)))
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (should
       (equal "Error: harmless data"
              (test-niwa-ptc-driver--run
               buffer "(PTCProbe :value \"one\")"))))
    (should (string-suffix-p "/1" child-id)))

  :doc "aborts on structured denial with bounded completed-work details"
  (let ((decisions 0)
        result)
    (test-niwa-ptc-driver--register-probe
     (lambda (args) (list :result (plist-get args :value)))
     nil
     (lambda (_tool args cont)
       (setq decisions (1+ decisions))
       (funcall cont (if (equal "two" (plist-get args :value))
                         '(deny . "not allowed")
                       'allow))))
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (setq result
            (test-niwa-ptc-driver--run
             buffer
             "(progn (PTCProbe :value \"one\")
                     (PTCProbe :value \"two\"))")))
    (should (= 2 decisions))
    (should (string-match-p "aborted.*denied" result))
    (should (string-match-p "Nested call audit" result))
    (should (string-match-p "/1 PTCProbe.*one.*success" result))
    (should (string-match-p "/2 PTCProbe.*two.*denied" result)))

  :doc "keeps a queued permission from settling the envelope"
  (let ((niwa-ptc-composable-tools '("PTCProbe")))
    (setf (niwa-session-permission-mode session) 'ask)
    (test-niwa-ptc-driver--register-probe
     (lambda (args) (list :result (plist-get args :value)))
     nil
     (lambda (_tool _args cont) (funcall cont 'ask)))
    (test-niwa-ptc-driver--select-probe buffer)
    (cl-letf (((symbol-function 'niwa-permission-queue--render-head)
               #'ignore))
      (dolist (case '((allow-once . "one")
                      (deny-once . "denied")
                      (aborted . "Error:")))
        (let ((deadline (+ (float-time) 2.0))
              done result)
          (with-current-buffer buffer
            (niwa-pipeline-run-tool
             (niwa-tool-ensure "ToolCall")
             (lambda (value) (setq result value done t))
             '(:expression "(PTCProbe :value \"one\")")))
          (while (and (null (niwa-session-permission-queue session))
                      (< (float-time) deadline))
            (accept-process-output nil 0.01))
          (should (niwa-session-permission-queue session))
          (should-not done)
          (niwa-permission-queue--on-head-outcome
           (car (niwa-session-permission-queue session)) (car case))
          (while (and (not done) (< (float-time) deadline))
            (accept-process-output nil 0.01))
          (should done)
          (should (string-match-p (cdr case)
                                  (test-niwa-ptc-driver--visible result)))))))

  :doc "preserves the recorded malformed Grep calls as script errors"
  (progn
    (with-current-buffer buffer
      (setq-local gptel-tools
                  (test-niwa-ptc-driver--gptel-tools "Grep")))
    (dolist
        (script
         '("(split-string
              (Grep :pattern \"^[(]defcustom[ )]\"
                    :path \".\" :glob \"/*.el\"
                    :output_mode \"count\")
              \"\\n\" t)"
           "(split-string
              (Grep :pattern \"^\\\\\\\\(defcustom\\\\b\"
                    :path \".\" :glob \"*.el\"
                    :output_mode \"count\")
              \"\\n\" t)"))
      (should (string-match-p
               "Wrong type argument: stringp"
               (test-niwa-ptc-driver--run buffer script)))))

  :doc "resumes off-stack, so nested calls do not grow the Lisp stack"
  ;; Most tool callbacks fire synchronously.  Resuming the interpreter
  ;; directly from one recurses per nested call and never returns to the
  ;; command loop, which froze Emacs for the length of the script.  Depth
  ;; must therefore stay flat as the call count rises, not merely finish.
  (let ((depths nil))
    (test-niwa-ptc-driver--register-probe
     (lambda (args)
       (push (length (backtrace-frames)) depths)
       (list :result (plist-get args :value))))
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (should (equal ":ok"
                     (test-niwa-ptc-driver--run
                      buffer
                      (format "(progn (dolist (i (list 1 2 3 4 5 6 7 8 9 10))
                                        (PTCProbe :value %S))
                                      :ok)"
                              (file-name-concat root "one.txt"))))))
    (should (= 10 (length depths)))
    (should (< (- (apply #'max depths) (apply #'min depths)) 50)))

  :doc "runs bounded batches concurrently and preserves source result order"
  (let ((active 0)
        (max-active 0)
        (started nil)
        (niwa-ptc-parallelism 2))
    (test-niwa-ptc-driver--register-probe
     (lambda (child-callback child-args)
       (let* ((value (plist-get child-args :value))
              (delay (pcase value ("a" 0.03) ("b" 0.01) (_ 0))))
         (push value started)
         (setq active (1+ active)
               max-active (max max-active active))
         (run-at-time
          delay nil
          (lambda ()
            (setq active (1- active))
            (funcall child-callback (list :result value))))))
     t)
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (with-current-buffer buffer
        (should (equal '("PTCProbe") (niwa-tool-ptc--roster))))
      (should
       (equal "(\"a\" \"b\" \"c\")"
              (test-niwa-ptc-driver--run
               buffer
               "(parallel (PTCProbe :value \"a\")
                          (PTCProbe :value \"b\")
                          (PTCProbe :value \"c\"))"))))
    (should (= 2 max-active))
    (should (equal '("a" "b" "c") (nreverse started))))

  :doc "rejects oversized child results before retaining their contents"
  (let ((niwa-ptc-max-value-bytes 128))
    (test-niwa-ptc-driver--register-probe
     (lambda (_args) (list :result (make-string 200 ?x))))
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (should
       (equal ":handled"
              (test-niwa-ptc-driver--run
               buffer
               "(let ((r (PTCProbe :value \"large\")))
                  (if (plist-get r :error) :handled :unexpected))")))))

  :doc "charges the guest error wrapper around a nested failure"
  (let ((niwa-ptc-max-retained-bytes 1))
    (test-niwa-ptc-driver--register-probe
     (lambda (_args) '(:result "x" :status error)))
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (should
       (string-match-p
        "retained value budget"
        (test-niwa-ptc-driver--run
         buffer "(PTCProbe :value \"failure\")")))))

  :doc "bounds cumulative parallel results before audit retention"
  (let ((niwa-ptc-max-retained-bytes 200)
        result)
    (test-niwa-ptc-driver--register-probe
     (lambda (_args) (list :result (make-string 120 ?x))))
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (setq result
            (test-niwa-ptc-driver--run
             buffer "(parallel (PTCProbe :value \"a\")
                              (PTCProbe :value \"b\"))" t))
      (let* ((render-data (cdr (niwa-tool-render-data-extract result)))
             (calls (plist-get render-data :calls)))
        (should (= 2 (length calls)))
        (should (string-match-p
                 "retained value budget"
                 (plist-get (nth 1 calls) :result)))
        (should (< (length (plist-get (nth 1 calls) :result)) 200)))))

  :doc "reports only terminal children as completed live work"
  (let (events)
    (test-niwa-ptc-driver--register-probe
     (lambda (args) (list :result (plist-get args :value))))
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (cl-letf (((symbol-function
                  'niwa-view-stream-handle-tool-progress)
                 (lambda (event)
                   (push (copy-tree (plist-get event :facts) t) events))))
        (should
         (equal "one"
                (test-niwa-ptc-driver--run
                 buffer "(PTCProbe :value \"one\")")))))
    (let ((unfinished
           (seq-find
            (lambda (facts)
              (memq (plist-get (car (plist-get facts :calls)) :status)
                    '(queued running)))
            events)))
      (should unfinished)
      (should (= 0 (plist-get unfinished :completed-count)))))

  :doc "does not launch queued siblings after synchronous batch denial"
  (let ((decisions nil)
        (runs nil)
        (phases nil)
        (niwa-ptc-parallelism 2)
        done result)
    (test-niwa-ptc-driver--register-probe
     (lambda (args)
       (push (plist-get args :value) runs)
       (list :result "unexpected"))
     nil
     (lambda (_tool args cont)
       (push (plist-get args :value) decisions)
       (funcall cont '(deny . "not allowed"))))
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (cl-letf (((symbol-function 'niwa-view-stream-handle-tool-progress)
                 (lambda (event)
                   (setq phases
                         (append phases (list (plist-get event :type)))))))
        (with-current-buffer buffer
          (niwa-pipeline-run-tool
           (niwa-tool-ensure "ToolCall")
           (lambda (value) (setq result value done t))
           '(:expression "(parallel (PTCProbe :value \"a\")
                                (PTCProbe :value \"b\")
                                (PTCProbe :value \"c\"))")))
        (let ((deadline (+ (float-time) 2.0)))
          (while (and (not done) (< (float-time) deadline))
            (accept-process-output nil 0.01))))
      (should done)
      (should (string-match-p "batch aborted.*denied" result))
      (let* ((render-data (cdr (niwa-tool-render-data-extract result)))
             (calls (plist-get render-data :calls)))
        (should (equal '(denied cancelled cancelled)
                       (mapcar (lambda (call) (plist-get call :status))
                               calls)))))
    (should (eq 'terminal (car (last phases))))
    (should (equal '("a") (nreverse decisions)))
    (should-not runs))

  :doc "records active siblings cancelled after a batch denial"
  (let ((niwa-ptc-parallelism 2)
        result)
    (test-niwa-ptc-driver--register-probe
     (lambda (_callback _args) nil)
     t
     (lambda (_tool args cont)
       (funcall cont (if (equal "deny" (plist-get args :value))
                         '(deny . "not allowed")
                       'allow))))
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (setq result
            (test-niwa-ptc-driver--run
             buffer "(parallel (PTCProbe :value \"pending\")
                              (PTCProbe :value \"deny\"))" t))
      (let* ((render-data (cdr (niwa-tool-render-data-extract result)))
             (calls (plist-get render-data :calls)))
        (should (equal '(cancelled denied)
                       (mapcar (lambda (call) (plist-get call :status))
                               calls))))))

  :doc "marks the calls of one concurrent join and leaves sequential ones bare"
  (let ((niwa-ptc-parallelism 2)
        result)
    (test-niwa-ptc-driver--register-probe
     (lambda (args) (list :result (plist-get args :value))))
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (setq result
            (test-niwa-ptc-driver--run
             buffer "(progn (PTCProbe :value \"lead\")
                           (parallel (PTCProbe :value \"a\")
                                     (PTCProbe :value \"b\"))
                           (parallel (PTCProbe :value \"lone\")))" t))
      (let* ((render-data (cdr (niwa-tool-render-data-extract result)))
             (batches (mapcar (lambda (call) (plist-get call :batch))
                              (plist-get render-data :calls))))
        ;; The sequential lead call and the one-call join ran nothing
        ;; concurrently, so only the real batch is marked.
        (should (equal '(nil 1 1 nil) batches)))))

  :doc "preserves child media references without payload data"
  (let (result)
    (setq session (test-niwa-session-persistence--leased-session
                   save-path :working-directory root :permission-mode 'full-auto))
    (with-current-buffer buffer
      (setq-local niwa--session session
                  niwa--workspace (niwa-session-workspace session)))
    (test-niwa-ptc-driver--register-probe
     (lambda (_args)
       '(:result "image"
         :media ((:mime "image/png" :kind image :path "image.png"
                  :data "QUJD")))))
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (setq result
            (test-niwa-ptc-driver--run
             buffer "(PTCProbe :value \"image\")" t))
      (let* ((render-data (cdr (niwa-tool-render-data-extract result)))
             (media (plist-get (car (plist-get render-data :calls)) :media)))
        (should (equal '((:mime "image/png" :kind image :path "image.png"))
                       media)))))

  :doc "records script settlement telemetry without script contents"
  (let ((niwa-ptc-composable-tools '("PTCProbe")))
    (test-niwa-ptc-driver--register-probe
     (lambda (args) (list :result (plist-get args :value))))
    (test-niwa-ptc-driver--select-probe buffer)
    (should (equal "one"
                   (test-niwa-ptc-driver--run
                    buffer "(PTCProbe :value \"one\")")))
    (let ((finish
           (seq-find
            (lambda (entry)
              (and (eq 'ptc-script (plist-get entry :event))
                   (eq 'finish (plist-get entry :stage))))
            (test-niwa-ptc-driver--telemetry session))))
      (should finish)
      (should (eq 'final-value (plist-get finish :outcome)))
      (should (= 1 (plist-get finish :nested-call-count)))
      (should (>= (plist-get finish :duration-ms) 0))
      (should-not (string-match-p "PTCProbe\\|one"
                                  (format "%S" finish)))))

  :doc "categorizes budget failures in script telemetry"
  (let ((niwa-ptc-max-steps 1))
    (let ((result (test-niwa-ptc-driver--run buffer "(progn 1 2 3)")))
      (should (string-match-p "step budget" result))
      (should (string-match-p "after 2 steps" result)))
    (let ((finish
           (seq-find
            (lambda (entry)
              (and (eq 'ptc-script (plist-get entry :event))
                   (eq 'finish (plist-get entry :stage))))
            (test-niwa-ptc-driver--telemetry session))))
      (should (eq 'script-error (plist-get finish :outcome)))
      (should (eq 'step (plist-get finish :budget-kind)))))

  :doc "audits the active child before request cancellation settles ToolCall"
  (let* ((request
          (niwa-request--create
           :id "ptc-cancel" :session session
           :file-snapshots (make-hash-table :test #'equal)))
         (started nil)
         (niwa-ptc-parallelism 1)
         done result)
    (test-niwa-ptc-driver--register-probe
     (lambda (_callback _args) (setq started t)) t)
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (with-current-buffer buffer
        (setq-local niwa--current-request request)
        (niwa-pipeline-run-tool
         (niwa-tool-ensure "ToolCall")
         (lambda (value) (setq result value done t))
         '(:expression "(parallel (PTCProbe :value \"active\")
                              (PTCProbe :value \"queued-1\")
                              (PTCProbe :value \"queued-2\"))")))
      (let ((deadline (+ (float-time) 2.0)))
        (while (and (not started) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should started)
      (should-not done)
      (niwa-request-drain-cancellers request)
      (should done)
      (let* ((render-data (cdr (niwa-tool-render-data-extract result)))
             (calls (plist-get render-data :calls)))
        (should (eq 'interrupted (plist-get render-data :outcome)))
        (should (equal '(cancelled cancelled cancelled)
                       (mapcar (lambda (call) (plist-get call :status))
                               calls))))))

  :doc "call cancellation without a request reaches a standalone child's cleanup"
  (let ((cleanups 0) (deliveries 0) started cancel outcome)
    (test-niwa-ptc-driver--register-probe
     (lambda (_callback _args)
       (setq started t)
       (list :cancel (lambda () (cl-incf cleanups))))
     t)
    (test-niwa-ptc-driver--select-probe buffer)
    (with-current-buffer buffer
      (setq cancel
            (niwa-pipeline-run-tool-outcome
             (niwa-tool-ensure "ToolCall")
             (lambda (value) (cl-incf deliveries) (setq outcome value))
             '(:expression "(PTCProbe :value \"wait\")"))))
    (let ((deadline (+ (float-time) 2.0)))
      (while (and (not started) (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (should started)
    (funcall cancel)
    (accept-process-output nil 0.05)
    (should (= 1 cleanups))
    (should (= 1 deliveries))
    (should (eq 'cancelled (plist-get outcome :reason))))

  :doc "ignores its request canceller after successful settlement"
  (let* ((durable-session
          (niwa-session--create
           :name "ptc" :workspace workspace :save-path save-path
           :working-directory root
           :execution-target (niwa-execution-target-create root)
           :permission-mode 'full-auto
           :touched-files (make-hash-table :test #'equal)))
         (request
         (niwa-request--create
          :id "ptc-settled" :session durable-session
          :file-snapshots (make-hash-table :test #'equal)))
        (niwa-telemetry-enabled nil)
        (updates 0)
        last-update)
    (with-current-buffer buffer
      (setq-local niwa--current-request request
                  niwa--session durable-session))
    (cl-letf (((symbol-function 'niwa-ptc-checkpoint-start)
               (lambda (&rest _) t))
              ((symbol-function 'niwa-ptc-checkpoint-update)
               (lambda (_session _buffer _id update)
                 (setq updates (1+ updates)
                       last-update (copy-tree update t))
                 t)))
      (should (equal "3" (test-niwa-ptc-driver--run buffer "(+ 1 2)")))
      (let ((before (copy-tree last-update t)))
        (should (= 1 updates))
        (niwa-request-drain-cancellers request)
        (should (= 1 updates))
        (should (equal before last-update)))))

  :doc "journals child audit in memory and writes durably only at settlement"
  (let* ((durable-session
          (niwa-session--create
           :name "ptc" :workspace workspace :save-path save-path
           :working-directory root
           :execution-target (niwa-execution-target-create root)
           :permission-mode 'full-auto
           :touched-files (make-hash-table :test #'equal)))
         (request
          (niwa-request--create
           :id "ptc-journal" :session durable-session
           :file-snapshots (make-hash-table :test #'equal)))
         (niwa-telemetry-enabled nil)
         (updates 0)
         (notes 0))
    (with-current-buffer buffer
      (setq-local niwa--current-request request
                  niwa--session durable-session))
    (cl-letf (((symbol-function 'niwa-ptc-checkpoint-start)
               (lambda (&rest _) t))
              ((symbol-function 'niwa-ptc-checkpoint-note)
               (lambda (&rest _) (setq notes (1+ notes)) t))
              ((symbol-function 'niwa-ptc-checkpoint-update)
               (lambda (&rest _) (setq updates (1+ updates)) t)))
      (should (equal "2"
                     (test-niwa-ptc-driver--run
                      buffer
                      (format "(length (list (Read :file_path %S) (Read :file_path %S)))"
                              (file-name-concat root "one.txt")
                              (file-name-concat root "two.txt")))))
      ;; Per-child audit progress stays in memory; the sidecar is written
      ;; once, at settlement.
      (should (= 1 updates))
      (should (>= notes 2))))

  :doc "uses child hooks and snapshots from the shared pipeline"
  (let* ((path (file-name-concat root "mutated.txt"))
         (request
          (niwa-request--create
           :id "ptc-mutation" :session session
           :file-snapshots (make-hash-table :test #'equal)))
         (events nil)
         (original-run-event (symbol-function 'niwa-hooks-run-event)))
    (with-temp-file path (insert "before"))
    (test-niwa-ptc-driver--register-probe
     (lambda (args)
       (with-temp-file (plist-get args :value) (insert "after"))
       '(:result "changed")))
    (let ((probe (niwa-tool-ensure "PTCProbe")))
      (setf (niwa-tool-read-only-p probe) nil
            (niwa-tool-snapshot-p probe) t
            (niwa-tool-get-paths probe)
            (lambda (args) (list (plist-get args :value)))))
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (with-current-buffer buffer
        (setq-local niwa--current-request request))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (event event-plist callback &rest rest)
                   (when (equal "PTCProbe" (plist-get event-plist :tool-name))
                     (push event events))
                   (apply original-run-event
                          event event-plist callback rest))))
        (should (equal "changed"
                       (test-niwa-ptc-driver--run
                        buffer (format "(PTCProbe :value %S)" path)))))
      (should (equal "before"
                     (gethash path (niwa-request-file-snapshots request))))
      (should (memq 'PreToolUse events))
      (should (memq 'PostToolUse events))))

  :doc "yields between capped chunks of synchronous batch calls"
  (let ((calls 0)
        observed
        (niwa-ptc-parallelism 2))
    (test-niwa-ptc-driver--register-probe
     (lambda (args)
       (setq calls (1+ calls))
       ;; Queue the observer before the first synchronous completion queues
       ;; the next batch turn.  It should still see the whole first chunk.
       (when (= calls 1)
         (run-at-time 0 nil (lambda () (setq observed calls))))
       (list :result (plist-get args :value))))
    (test-niwa-ptc-driver--select-probe buffer)
    (let ((niwa-ptc-composable-tools '("PTCProbe")))
      (should
       (equal '("a" "b" "c" "d" "e")
              (read
               (test-niwa-ptc-driver--run
                buffer
                "(parallel (PTCProbe :value \"a\")
                           (PTCProbe :value \"b\")
                           (PTCProbe :value \"c\")
                           (PTCProbe :value \"d\")
                           (PTCProbe :value \"e\"))")))))
    (should (= 2 observed)))

  :doc "settles an over-budget script with an error naming the budget"
  ;; The user-visible failure mode: a script that runs away must come back
  ;; as a bounded error, not hang the request.
  (progn
    (let ((niwa-ptc-max-steps 2000)
          (niwa-ptc-step-slice 200))
      (should (string-match-p "step budget"
                              (test-niwa-ptc-driver--run buffer "(while t 1)"))))
    (let ((niwa-ptc-max-tool-calls 2))
      (let ((result
             (test-niwa-ptc-driver--run
              buffer
              (format "(let ((n 0)) (while t (setq n (Read :file_path %S))))"
                      (file-name-concat root "one.txt")))))
        (should (string-match-p "tool calls" result))
        (should (string-match-p "before nested call Read at call 3" result)))))

  :doc "reports an unreadable script without running anything"
  (should (string-match-p "Error:" (test-niwa-ptc-driver--run buffer "(list (a . b))")))

  :doc "reports a forbidden operator as a script error"
  (should (string-match-p "Unknown functions: getenv"
                          (test-niwa-ptc-driver--run buffer "(getenv \"HOME\")")))

  :doc "a standalone tool is rejected inside a composed script"
  (let ((niwa-ptc-composable-tools '("Glob")))
    (should (string-match-p
             "Standalone-only tools"
             (test-niwa-ptc-driver--run
              buffer (format "(progn (Read :file_path %S))" (file-name-concat root "one.txt")))))))


(provide 'test-niwa-ptc-driver)
;;; test-niwa-ptc-driver.el ends here
