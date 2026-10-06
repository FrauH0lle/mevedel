;;; mevedel-mcp-tools.el --- Turn-owned MCP pipeline calls -*- lexical-binding: t -*-

;;; Commentary:
;; Converts registered tool schemas and pipeline outcomes at the MCP boundary.
;; The caller supplies the tool and turn owner captured at admission.  No external
;; session identifier grants execution authority, and no ACP notification
;; executes a tool.  Only calls admitted through this boundary reach handlers.

;;; Code:

(require 'gptel-request)
(require 'mevedel-agents)
(require 'mevedel-pipeline)
(require 'mevedel-tool-registry)
(require 'mevedel-tool-render-data)
(require 'mevedel-tool-media)

(defun mevedel-mcp-tools-schemas (tools)
  "Convert registered gptel TOOLS into MCP tool schemas."
  (vconcat
   (mapcar
    (lambda (entry)
      (let ((function (plist-get entry :function)))
        (list :name (plist-get function :name)
              :description (plist-get function :description)
              :inputSchema (plist-get function :parameters))))
    (append (gptel--parse-tools nil tools) nil))))

(defun mevedel-mcp-tools--result (outcome)
  "Project structured pipeline OUTCOME into an MCP result."
  (let* ((id (plist-get outcome :tool-use-id))
         (text (gptel--to-string
                (mevedel-tool-media-strip-blocks
                 (mevedel-tool-render-data-strip-non-media
                  (plist-get outcome :result) id) id))))
    (list :isError (if (eq 'success (plist-get outcome :status)) :json-false t)
          :content
          (vconcat
           (list (list :type "text" :text text))
           (mapcar
            (lambda (item)
              (let ((mime (plist-get item :mime))
                    (data (plist-get item :data)))
                (if (string-prefix-p "image/" mime)
                    (list :type "image" :data data :mimeType mime)
                  (list :type "resource"
                        :resource
                        (list :uri (concat "urn:sha256:"
                                           (secure-hash 'sha256 data))
                              :mimeType mime :blob data)))))
            (mevedel-tool-media-normalize-items
             (plist-get outcome :media)))))))

(defun mevedel-mcp-tools-call (owner buffer tool args tool-use-id callback)
  "Execute TOOL with ARGS under captured turn OWNER in BUFFER.
OWNER is an admitted root request or a running retained-agent invocation.
TOOL-USE-ID is the native external call identity captured at admission.
CALLBACK receives the MCP result and the structured pipeline outcome with
canonical display metadata.  Return a zero-argument cancellation function.
The caller must select TOOL from its admitted roster, not arbitrary input."
  (let ((finish (lambda (outcome)
                  (funcall callback (mevedel-mcp-tools--result outcome) outcome))))
    (if (not
         (and (buffer-live-p buffer)
              (cond
               ((mevedel-request-p owner)
                (and (eq owner (buffer-local-value 'mevedel--current-request buffer))
                     (not (mevedel-request-cancelled-p owner))))
               ((mevedel-agent-invocation-p owner)
                (and (eq buffer (mevedel-agent-invocation-buffer owner))
                     (eq owner (buffer-local-value 'mevedel--agent-invocation buffer))
                     (eq (mevedel-agent-invocation-parent-session owner)
                         (buffer-local-value 'mevedel--session buffer))
                     (eq (plist-get (mevedel-engine-info owner) :mevedel-request)
                         (buffer-local-value 'mevedel--current-request buffer))
                     (not (when-let* ((request (plist-get (mevedel-engine-info owner) :mevedel-request)))
                            (mevedel-request-cancelled-p request)))
                     (eq 'running (mevedel-agent-invocation-transcript-status owner))
                     (not (mevedel-agent-invocation-runtime-settled-p owner)))))
              (not (plist-get (mevedel-engine-info owner) :mevedel-end-turn))))
        (progn
          (funcall finish
                   (list :status 'error :reason 'obsolete-request
                         :result "Error: The owning request is no longer active."
                         :tool-use-id tool-use-id))
          #'ignore)
      (with-current-buffer buffer
        (let* ((info (list :name (mevedel-tool-name tool) :args args))
               (repair (mevedel-tool-repair-pre-tool-call info tool)))
          (if (plist-member repair :result)
              (let ((result (plist-get repair :result)))
                (mevedel-tool-repair-post-tool-call
                 (plist-put info :result result))
                (funcall finish
                         (list :status 'error :reason 'invalid-arguments
                               :result result :tool-use-id tool-use-id))
                #'ignore)
            (mevedel-pipeline-run-tool-outcome
             tool finish (if (plist-member repair :args)
                             (plist-get repair :args) args)
             (list :tool-use-id tool-use-id :source 'mcp
                   :origin (if (mevedel-request-p owner)
                               (mevedel-request-origin owner)
                             (mevedel-agent-invocation-require-path owner))
                   :engine owner
                   :projection 'provider))))))))

(provide 'mevedel-mcp-tools)
;;; mevedel-mcp-tools.el ends here
