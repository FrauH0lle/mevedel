;;; test-mevedel-resource-mcp.el --- MCP resource provider tests -*- lexical-binding: t -*-

;;; Commentary:

;; Focused tests for the MCP resource-address provider.

;;; Code:

(require 'mcp)
(require 'mcp-hub)
(require 'mevedel-resource)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-resource-mcp-provider ()
  ,test
  (test)
  :doc "lists MCP metadata and reads fresh text through the mention seam"
  (let* ((server "srv/name")
         (uri "file:///a b?x#frag")
         (connection (make-symbol "mcp-connection"))
         (connections (make-hash-table :test #'equal))
         (servers (list (list :name server :status 'connected
                              :resources
                              (list (list :uri uri :name "Example")))))
         (calls 0)
         (address (format "mcp://%s/%s"
                          (mevedel-resource-encode-component server)
                          (mevedel-resource-encode-component uri))))
    (puthash server connection connections)
    (cl-letf (((symbol-function 'mcp-hub-get-servers)
               (lambda () servers))
              ((symbol-function 'mcp-read-resource)
               (lambda (_connection _uri)
                 (cl-incf calls)
                 (list :contents
                       (vector (list :type "text" :text "one")
                               (list :type "text" :text "two"))))))
      (let ((mcp-server-connections connections))
        (let* ((listing
                (mevedel-resource-execute
                 (mevedel-resource-prepare 'read "mcp://" nil)))
               (resources
                (mevedel-resource-execute
                 (mevedel-resource-prepare
                  'read (format "mcp://%s"
                                (mevedel-resource-encode-component server))
                  nil)))
               (attempt (mevedel-resource-prepare 'read address nil)))
          (should (string-match-p "mcp://srv%2Fname"
                                  (plist-get listing :result)))
          (should (string-match-p (regexp-quote address)
                                  (plist-get resources :result)))
          (should (equal "one\ntwo"
                         (plist-get
                          (mevedel-resource-execute attempt) :result)))
          (let ((fresh (mevedel-resource-prepare 'read address nil)))
            (should (equal "one\ntwo"
                           (plist-get
                            (mevedel-resource-execute fresh) :result))))
          (should (= 2 calls))))))
  :doc "rejects unknown MCP servers without falling back to all-server listing"
  (cl-letf (((symbol-function 'mcp-hub-get-servers) (lambda () nil)))
    (should-error
     (mevedel-resource-execute
      (mevedel-resource-prepare 'read "mcp://unknown" nil)))))

(provide 'test-mevedel-resource-mcp)
;;; test-mevedel-resource-mcp.el ends here
