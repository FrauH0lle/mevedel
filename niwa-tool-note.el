;;; niwa-tool-note.el -- Note tool registration -*- lexical-binding: t -*-

;;; Commentary:

;; Register the workspace note writer without loading source presentation.

;;; Code:

(require 'niwa-tool-registry)
(autoload 'niwa-tool-note-write "niwa-note-write")
(autoload 'niwa-tool-note-check "niwa-note-write")

(defun niwa-tool-note-register ()
  "Register the model's workspace note tool."
  (niwa-define-tool
    :name "NoteWrite"
    :description "Create or change workspace notes in one batch, without editing source code."
    :summary "Create, edit, tag, close, delete or send workspace notes."
    :prompt "### When to use
Leave findings on code or manage notes the user asks you to change.

### When NOT to use
Do not close notes, send them or mark them done unless the user asks. Creating a note
does not send it. Read existing notes with Read at notes:// instead.

### How to use
Pass operations in order; one call performs one store sync. Create needs
file_path and start_line (1-based); end_line is inclusive and defaults to
start_line. Omit match for whole lines. To point at an expression or other
exact text, set match to that case-sensitive text, including any line breaks,
within the line range. If it appears more than once, supply occurrence (1-based).
Text defaults to empty. Tags replace the whole tag list; [] clears.
Other actions name an existing note's UUID as id (from notes://UUID.md).
In read-only conversations only create is allowed. Optional prepared_change
captures an ApplyPatch-format suggestion without writing source; the user
applies it. Hard path denials still apply, but preparation needs no approval.
A batch failure reports completed operations; do not repeat those on retry.
Send operations run together after the other operations and do not open a view."
    :handler #'niwa-tool-note-write
    :args ((operations array :required "Note operations, processed in order."
                       :items (:type object
                               :required ["action"]
                               :properties
                               (:action (:type string :enum ["create" "edit" "close" "delete" "done" "send"])
                                :id (:type string)
                                :file_path (:type string)
                                :start_line (:type integer)
                                :end_line (:type integer)
                                :match (:type string :description "Create only: exact case-sensitive text to anchor instead of whole lines; may span lines.")
                                :occurrence (:type integer :description "Create only: 1-based position among exact matches in the line range.")
                                :text (:type string)
                                :tags (:type array :items (:type string))
                                :prepared_change
                                (:type string :description "Create only: an ApplyPatch-format suggestion, using ordinary source paths in this workspace. It waits for the user to apply; source is unchanged.")))))
    :check-permission #'niwa-tool-note-check
    :groups (notes)))

(provide 'niwa-tool-note)
;;; niwa-tool-note.el ends here
