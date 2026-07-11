;;; test-mevedel-specialist-nudges.el --- Tests for specialist prompting -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-agents)
(require 'mevedel-specialist-nudges)
(require 'mevedel-structs)
(require 'mevedel-tool-registry)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

;;
;;; Specialist nudges

(mevedel-deftest mevedel-specialist-nudges-apply
			 ()
			 ,test
			 (test)
			 :doc "Grep identifier searches in code get bounded xref guidance"
			 (let* ((session (mevedel-session--create
					  :name "main"
					  :deferred-set
					  '((("mevedel" "XrefReferences") . "refs")
					    (("mevedel" "XrefDefinitions") . "defs"))))
				(tool (mevedel-tool--create :name "Grep"))
				(ctx (list :tool tool
					   :args '(:pattern "thing-name" :type "elisp")
					   :result "file.el:1:thing-name"
					   :session session))
				(out (mevedel-specialist-nudges-apply ctx)))
			   (should (string-match-p "XrefReferences" (plist-get out :result)))
			   (should (string-match-p "available now" (plist-get out :result)))
			   ;; Same family, same turn: no duplicate nudge.
			   (setq ctx (plist-put ctx :result "file.el:2:thing-name"))
			   (setq out (mevedel-specialist-nudges-apply ctx))
			   (should-not (string-match-p "XrefReferences" (plist-get out :result)))
			   ;; A later turn gets the second allowed nudge; the family
			   ;; is then capped for the context.
			   (setf (mevedel-session-turn-count session) 1)
			   (setq ctx (plist-put ctx :result "file.el:3:thing-name"))
			   (setq out (mevedel-specialist-nudges-apply ctx))
			   (should (string-match-p "XrefReferences" (plist-get out :result)))
			   (setf (mevedel-session-turn-count session) 2)
			   (setq ctx (plist-put ctx :result "file.el:4:thing-name"))
			   (setq out (mevedel-specialist-nudges-apply ctx))
			   (should-not (string-match-p "XrefReferences" (plist-get out :result))))

			 :doc "Grep nudges can update sub-agent invocation state"
				 (let* ((agent (mevedel-agent--create :name "verifier"))
					(invocation (mevedel-agent-invocation--create
						     :agent agent
						     :deferred-set
						     '((("mevedel" "XrefReferences") . "refs"))))
					(session (mevedel-session--create :name "main"))
					(tool (mevedel-tool--create :name "Grep"))
					(ctx (list :tool tool
						   :args '(:pattern "thing-name" :type "elisp")
						   :result "file.el:1:thing-name"
						   :session session
						   :invocation invocation))
					(out (mevedel-specialist-nudges-apply ctx)))
				   (should (string-match-p "XrefReferences" (plist-get out :result)))
				   (should (plist-get (mevedel-agent-invocation-specialist-nudge-state
					       invocation)
					      :xref))
				   (setq ctx (plist-put ctx :result "file.el:2:thing-name"))
				   (setq out (mevedel-specialist-nudges-apply ctx))
				   (should-not (string-match-p "XrefReferences" (plist-get out :result))))

				 :doc "Grep regex searches are not nudged toward xref"
			 (let* ((session (mevedel-session--create
					  :name "main"
					  :deferred-set
					  '((("mevedel" "XrefReferences") . "refs"))))
				(tool (mevedel-tool--create :name "Grep"))
				(ctx (list :tool tool
					   :args '(:pattern "thing.*other" :type "elisp")
					   :result "file.el:1:thing-other"
					   :session session))
				(out (mevedel-specialist-nudges-apply ctx)))
			   (should-not (string-match-p "XrefReferences" (plist-get out :result))))

			 :doc "Grep comment searches are not nudged toward xref"
				 (let* ((session (mevedel-session--create
						  :name "main"
						  :deferred-set
						  '((("mevedel" "XrefReferences") . "refs"))))
					(tool (mevedel-tool--create :name "Grep"))
					(ctx (list :tool tool
						   :args '(:pattern "TODO" :type "elisp")
						   :result "file.el:1:;; TODO fix"
						   :session session))
					(out (mevedel-specialist-nudges-apply ctx)))
				   (should-not (string-match-p "XrefReferences" (plist-get out :result))))

				 :doc "Grep broad single-token searches are not nudged toward xref"
				 (let* ((session (mevedel-session--create
						  :name "main"
						  :deferred-set
						  '((("mevedel" "XrefReferences") . "refs"))))
					(tool (mevedel-tool--create :name "Grep"))
					(ctx (list :tool tool
						   :args '(:pattern "user" :type "elisp")
						   :result "user.el:1:(defvar user-name nil)"
						   :session session))
					(out (mevedel-specialist-nudges-apply ctx)))
				 (should-not (string-match-p "XrefReferences" (plist-get out :result))))

				 :doc "Grep broad code globs do not get one-file Imenu guidance"
					 (let* ((session (mevedel-session--create
							  :name "main"
							  :deferred-set
							  '((("mevedel" "XrefReferences") . "refs")
							    (("mevedel" "XrefDefinitions") . "defs")
							    (("mevedel" "Imenu") . "outline"))))
						(tool (mevedel-tool--create :name "Grep"))
						(ctx (list :tool tool
							   :args '(:pattern "thing-name"
								     :path "."
								     :glob "**/*.el")
							   :result "file.el:1:thing-name"
							   :session session))
						(out (mevedel-specialist-nudges-apply ctx)))
					 (should (string-match-p "XrefReferences" (plist-get out :result)))
					 (should-not (string-match-p "Imenu" (plist-get out :result))))

					 :doc "Grep single code files can get Imenu guidance"
					 (let* ((session (mevedel-session--create
							  :name "main"
							  :deferred-set
							  '((("mevedel" "Imenu") . "outline"))))
						(tool (mevedel-tool--create :name "Grep"))
						(ctx (list :tool tool
							   :args '(:pattern "thing-name"
								     :path "file.el")
							   :result "file.el:1:thing-name"
							   :session session))
						(out (mevedel-specialist-nudges-apply ctx)))
					 (should (string-match-p "Imenu" (plist-get out :result))))

					 :doc "Grep structural code searches get Treesitter guidance"
				 (let* ((session (mevedel-session--create
						  :name "main"
						  :deferred-set
						  '((("mevedel" "Treesitter") . "syntax"))))
					(tool (mevedel-tool--create :name "Grep"))
					(ctx (list :tool tool
						   :args '(:pattern "defun" :type "elisp")
						   :result "file.el:1:(defun thing () nil)"
						   :session session))
					(out (mevedel-specialist-nudges-apply ctx)))
				   (should (string-match-p "Treesitter" (plist-get out :result)))
				   (should (string-match-p "available now" (plist-get out :result))))

				 :doc "Grep no-match and non-code results are not nudged"
				 (let* ((session (mevedel-session--create
						  :name "main"
						  :deferred-set
						  '((("mevedel" "XrefReferences") . "refs")
						    (("mevedel" "Treesitter") . "syntax"))))
					(tool (mevedel-tool--create :name "Grep"))
					(no-match (list :tool tool
							:args '(:pattern "thing-name" :type "elisp")
							:result "No matches found"
							:session session))
					(non-code (list :tool tool
							:args '(:pattern "thing-name"
								:path "README.md")
							:result "README.md:1:thing-name"
							:session session))
					(out (mevedel-specialist-nudges-apply no-match)))
				   (should-not (string-match-p "XrefReferences"
							       (plist-get out :result)))
				   (setq out (mevedel-specialist-nudges-apply non-code))
				   (should-not (string-match-p "XrefReferences"
							       (plist-get out :result)))
				   (should-not (string-match-p "Treesitter"
							       (plist-get out :result))))

				 :doc "Read full code file gets Imenu guidance when Imenu is deferred"
			 (let* ((session (mevedel-session--create
					  :name "main"
					  :deferred-set
					  '((("mevedel" "Imenu") . "outline"))))
				(tool (mevedel-tool--create :name "Read"))
				(ctx (list :tool tool
					   :args '(:file_path "file.el")
					   :result "1\t(defun file () nil)"
					   :session session))
				(out (mevedel-specialist-nudges-apply ctx)))
			   (should (string-match-p "Imenu" (plist-get out :result))))

			 :doc "Read full code file gets Treesitter guidance when Treesitter is deferred"
			 (let* ((session (mevedel-session--create
					  :name "main"
					  :deferred-set
					  '((("mevedel" "Treesitter") . "syntax"))))
				(tool (mevedel-tool--create :name "Read"))
				(ctx (list :tool tool
					   :args '(:file_path "file.el")
					   :result "1\t(defun file () nil)"
					   :session session))
				(out (mevedel-specialist-nudges-apply ctx)))
			   (should (string-match-p "Treesitter" (plist-get out :result))))

			 :doc "Read exact ranges are not nudged"
			 (let* ((session (mevedel-session--create
					  :name "main"
					  :deferred-set
					  '((("mevedel" "Imenu") . "outline")
					    (("mevedel" "Treesitter") . "syntax"))))
				(tool (mevedel-tool--create :name "Read"))
				(ctx (list :tool tool
					   :args '(:file_path "file.el"
							       :offset 10
							       :limit 20)
					   :result "10\t(defun file () nil)"
					   :session session))
				(out (mevedel-specialist-nudges-apply ctx)))
			   (should-not (string-match-p "Imenu" (plist-get out :result))))

			 :doc "Read intentional whole-file ranges are not nudged"
				 (let* ((session (mevedel-session--create
						  :name "main"
						  :deferred-set
						  '((("mevedel" "Imenu") . "outline")
						    (("mevedel" "Treesitter") . "syntax"))))
					(tool (mevedel-tool--create :name "Read"))
					(ctx (list :tool tool
						   :args '(:file_path "file.el"
							     :offset 1
							     :limit 2600)
						   :result "1\t(defun file () nil)"
						   :session session))
					(out (mevedel-specialist-nudges-apply ctx)))
				   (should-not (string-match-p "Imenu" (plist-get out :result)))
				   (should-not (string-match-p "Treesitter" (plist-get out :result))))

				 :doc "Read duplicate, media, and non-code results are not nudged"
			 (let* ((session (mevedel-session--create
					  :name "main"
					  :deferred-set
					  '((("mevedel" "Imenu") . "outline")
					    (("mevedel" "Treesitter") . "syntax"))))
				(tool (mevedel-tool--create :name "Read"))
				(duplicate (list :tool tool
						 :args '(:file_path "file.el")
						 :result "File has already been read"
						 :session session))
				(media (list :tool tool
					     :args '(:file_path "image.png"
						     :max_width 800)
					     :result "<media-file>\n...\n</media-file>"
					     :session session))
				(non-code (list :tool tool
						:args '(:file_path "notes.md")
						:result "1\t# notes"
						:session session))
				out)
			   (dolist (ctx (list duplicate media non-code))
			     (setq out (mevedel-specialist-nudges-apply ctx))
			     (should-not (string-match-p "Imenu"
							 (plist-get out :result)))
			     (should-not (string-match-p "Treesitter"
							 (plist-get out :result)))))

				 :doc "Read default full range values are nudged"
				 (let* ((session (mevedel-session--create
						  :name "main"
						  :deferred-set
						  '((("mevedel" "Imenu") . "outline"))))
					(tool (mevedel-tool--create :name "Read"))
					(ctx (list :tool tool
						   :args '(:file_path "file.el"
								     :offset 0
								     :limit 2000)
						   :result "1\t(defun file () nil)"
						   :session session))
					(out (mevedel-specialist-nudges-apply ctx)))
				   (should (string-match-p "Imenu" (plist-get out :result))))

					 :doc "Read defaulted optional args get specialist nudges"
					 (let* ((session (mevedel-session--create
							  :name "main"
							  :deferred-set
							  '((("mevedel" "Imenu") . "outline")
							    (("mevedel" "XrefReferences") . "refs")
							    (("mevedel" "XrefDefinitions") . "defs")
							    (("mevedel" "Treesitter") . "syntax"))))
						(tool (mevedel-tool--create :name "Read"))
						(ctx (list :tool tool
							   :args '(:file_path "file.el"
								     :offset 0
								     :limit 2000
								     :pages ""
								     :max_width 0
								     :max_height 0
								     :max_tokens 0)
							   :result "1\t(defun file () nil)"
							   :session session))
						(out (mevedel-specialist-nudges-apply ctx)))
					 (should (string-match-p "Imenu" (plist-get out :result)))
					 (should (string-match-p "XrefReferences" (plist-get out :result)))
					 (should (string-match-p "Treesitter" (plist-get out :result))))

  :doc "preserves exact multi-family Grep reminder text and order"
  (let* ((session
          (mevedel-session--create
           :name "main"
           :deferred-set
           '((("mevedel" "XrefReferences") . "refs")
             (("mevedel" "XrefDefinitions") . "defs")
             (("mevedel" "Imenu") . "outline"))))
	         (tool (mevedel-tool--create :name "Grep"))
	         (result
	          (mevedel-specialist-nudges-apply
	           (list :tool tool
	                 :args '(:pattern "thing-name" :path "file.el")
	                 :result "file.el:1:thing-name"
	                 :session session))))
    (should
     (equal
      (concat
       "file.el:1:thing-name\n\n<system-reminder>\n"
       "For precise code symbol references, prefer `XrefReferences(identifier, file_path)'; for definitions or name discovery, prefer `XrefDefinitions(pattern, file_path)'. If the tool is not callable, use ToolSearch(query=\"xref\", load=true); loaded tools are available now for your next tool call.\n"
       "For a symbol outline in one known code file, prefer `Imenu(file_path)' over grepping the file for structure. If the tool is not callable, use ToolSearch(query=\"imenu\", load=true); loaded tools are available now for your next tool call.\n"
       "</system-reminder>")
	      (plist-get result :result))))

  :doc "suppresses media-range Reads even when the path looks like code"
  (let ((session
         (mevedel-session--create
          :name "main"
          :deferred-set '((("mevedel" "Imenu") . "outline")))))
	    (should
	     (equal "contents"
	            (plist-get
	             (mevedel-specialist-nudges-apply
	              (list :tool (mevedel-tool--create :name "Read")
	                    :args '(:file_path "file.el" :max_tokens 100)
	                    :result "contents" :session session))
	             :result)))))


(provide 'test-mevedel-specialist-nudges)
;;; test-mevedel-specialist-nudges.el ends here
