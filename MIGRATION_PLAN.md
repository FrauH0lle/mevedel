# Migration Plan: Remove Macher Dependency

## Overview
This document outlines the plan to remove macher as a core dependency from mevedel and integrate gptel-agent directly.

## Motivation
- **Reduce dependencies**: macher is unmaintained (no activity in 2+ months)
- **Leverage gptel-agent**: Modern, actively developed tooling
- **Simplify architecture**: Remove workspace abstraction layer
- **Add custom tools**: Implement ask_user, directory access requests, etc.

## Current State Analysis

### Macher Integration Depth
- **~50+ integration points** across codebase
- Main files affected:
  - mevedel.el (557 lines): 13 direct macher calls
  - mevedel-instructions.el (2,622 lines): 15+ macher references
  - mevedel-utilities.el (555 lines): 4 workspace calls
  - mevedel-diff-apply.el (665 lines): 2 workspace calls

### Key Macher Features Used
1. **Workspace abstraction** - Project root detection and context
2. **Action execution system** - Async callback-based LLM execution
3. **Patch buffer management** - Per-workspace staging of diffs
4. **Preset system** - Different tool configurations

## Implementation Plan

### Phase 1: Core Infrastructure (mevedel.el)

#### 1.1 Remove macher dependency
**Files:** mevedel.el
**Lines affected:** 32, 517-527
- Remove `(require 'macher)`
- Remove from package-requires
- Remove macher action registrations
- Remove advice on `macher--patch-ready`, `macher--before-action`
- Remove hook on `macher-patch-ready-hook`

#### 1.2 Implement project root allowlist
**New code:** ~50 lines
```elisp
(defcustom mevedel--project-roots nil
  "List of allowed project directories for LLM access."
  :type '(repeat directory)
  :group 'mevedel)

(defun mevedel--project-root ()
  "Get primary project root."
  (or (car mevedel--project-roots)
      (when-let ((proj (project-current)))
        (project-root proj))
      default-directory))

(defun mevedel-add-project-root (directory)
  "Add DIRECTORY to allowed project roots."
  (interactive "DAdd project root: ")
  (add-to-list 'mevedel--project-roots (expand-file-name directory)))

(defun mevedel-remove-project-root (directory)
  "Remove DIRECTORY from allowed project roots."
  (interactive (list (completing-read "Remove: " mevedel--project-roots)))
  (setq mevedel--project-roots (delete directory mevedel--project-roots)))
```

#### 1.3 Buffer management (adapt from macher)
**New code:** ~100 lines
```elisp
(defun mevedel--action-buffer (&optional create)
  "Get or create mevedel action (chat) buffer.
Similar to macher's action buffer concept."
  (or (get-buffer "*mevedel-action*")
      (when create
        (with-current-buffer (get-buffer-create "*mevedel-action*")
          (gptel-mode)
          (current-buffer)))))

(defun mevedel--patch-buffer (&optional create)
  "Get or create mevedel patch staging buffer.
Similar to macher's patch buffer concept."
  (or (get-buffer "*mevedel-patch*")
      (when create
        (with-current-buffer (get-buffer-create "*mevedel-patch*")
          (diff-mode)
          (setq buffer-read-only t)
          (current-buffer)))))

(defun mevedel--append-to-patch-buffer (diff filepath)
  "Append DIFF for FILEPATH to patch buffer."
  (with-current-buffer (mevedel--patch-buffer t)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "\n;; Patch for: %s\n" filepath))
      (insert diff)
      (insert "\n")))
  (mevedel--indicate-patch-ready))

(defun mevedel--indicate-patch-ready ()
  "Provide visual feedback that patch is ready."
  (message "Patch ready in *mevedel-patch* buffer")
  ;; Could add mode-line indicator here
  )

(defun mevedel--clear-patch-buffer ()
  "Clear the patch buffer."
  (interactive)
  (when-let ((buf (mevedel--patch-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))))
```

#### 1.4 Define three gptel presets
**New code:** ~60 lines
```elisp
(gptel-make-preset 'mevedel-discuss
  :description "Read-only tools for context gathering and discussion"
  :tools '(:eval (cl-remove-if
                  (lambda (name)
                    (member name '("edit_files" "write_file" "insert_in_file")))
                  (mapcar #'gptel-tool-name
                          (gptel-get-tool "filesystem"))))
  :system "You are helping analyze code. Use read_file_lines and grep_files to understand the codebase. Do not make any edits."
  :category "mevedel")

(gptel-make-preset 'mevedel-implement
  :parents '(mevedel-discuss)
  :description "Full editing capabilities with patch review workflow"
  :tools '(:append ("edit_files"))
  :system "You are implementing code changes. Use edit_files to make changes, which will be staged for review before applying."
  :category "mevedel")

(gptel-make-preset 'mevedel-revise
  :parents '(mevedel-implement)
  :description "Revise previous implementation with full context"
  :system "You are revising previous changes. The previous patch is included in context."
  :category "mevedel")
```

#### 1.5 Implement patch capture mechanism
**New code:** ~80 lines
```elisp
(defun mevedel--intercept-tool-results (resp info)
  "Intercept tool results from gptel-agent to capture patches.
This is the callback function for gptel-request."
  (pcase resp
    (`(tool-result . ,results)
     ;; Tool execution completed, check for edit_files
     (dolist (tool-result results)
       (cl-destructuring-bind (tool-spec args result) tool-result
         (when (equal (gptel-tool-name tool-spec) "edit_files")
           ;; Extract the diff that was applied
           (when-let ((diff (plist-get args :new_str))
                      (path (plist-get args :path)))
             (mevedel--append-to-patch-buffer diff path))))))

    (`(tool-call . ,_pending-calls)
     ;; Tool confirmation pending, display in action buffer
     (message "Tool call awaiting confirmation"))

    ((pred stringp)
     ;; Normal text response, insert into action buffer
     (when-let ((buf (mevedel--action-buffer)))
       (with-current-buffer buf
         (goto-char (point-max))
         (insert resp))))

    ('abort
     (message "Request aborted"))

    (_
     (message "Unknown response type: %S" resp))))
```

#### 1.6 Define custom tools
**New code:** ~80 lines
```elisp
(gptel-make-tool
 :name "ask_user"
 :function (lambda (callback question &optional options)
             "Ask user a question during LLM execution.
CALLBACK is automatically prepended for async tools.
QUESTION is the question string.
OPTIONS is optional list of choices."
             (let ((answer (if options
                               (completing-read
                                (concat question " ")
                                (append options '("Other")))
                             (read-string (concat question " ")))))
               (funcall callback answer)))
 :description "Ask the user a question and wait for their response. Use this when you need user input to proceed."
 :args '((:name "question"
          :type string
          :description "The question to ask the user")
         (:name "options"
          :type array
          :items (:type string)
          :optional t
          :description "Optional list of predefined choices"))
 :async t
 :confirm nil  ;; Already interactive
 :include t
 :category "mevedel")

(gptel-make-tool
 :name "request_directory_access"
 :function (lambda (callback directory reason)
             "Request access to a new directory.
CALLBACK is for async execution.
DIRECTORY is the path to request.
REASON explains why access is needed."
             (let ((expanded (expand-file-name directory)))
               (if (yes-or-no-p
                    (format "Grant LLM access to %s?\nReason: %s\n\nAllow? "
                            expanded reason))
                   (progn
                     (mevedel-add-project-root expanded)
                     (funcall callback
                              (format "Access granted to %s" expanded)))
                 (funcall callback
                          (format "Access denied to %s" expanded)))))
 :description "Request access to a directory outside the current project roots. Explain why you need access."
 :args '((:name "directory"
          :type string
          :description "Absolute or relative path to the directory")
         (:name "reason"
          :type string
          :description "Explanation of why you need access to this directory"))
 :async t
 :confirm nil
 :include t
 :category "mevedel")
```

#### 1.7 Implement three action commands
**New code:** ~150 lines

Replace existing macher-based commands with:
```elisp
(defun mevedel-implement-directive (directive &optional buffer)
  "Execute DIRECTIVE with full editing tools using gptel-agent.
DIRECTIVE should be a mevedel instruction overlay.
BUFFER is the buffer to execute in (defaults to current)."
  (interactive)
  (let* ((prompt (mevedel--build-prompt-from-directive directive))
         (target-buffer (or buffer (current-buffer)))
         (action-buffer (mevedel--action-buffer t)))
    ;; Clear previous patches
    (mevedel--clear-patch-buffer)
    ;; Display action buffer
    (display-buffer action-buffer)
    ;; Execute with implement preset
    (with-current-buffer action-buffer
      (gptel-request prompt
        :buffer action-buffer
        :callback #'mevedel--intercept-tool-results
        :preset 'mevedel-implement))))

(defun mevedel-revise-directive (directive &optional buffer)
  "Revise previous implementation of DIRECTIVE.
Includes previous patch from instruction undo history in context."
  (interactive)
  (let* ((prev-patch (mevedel--get-instruction-last-patch directive))
         (base-prompt (mevedel--build-prompt-from-directive directive))
         (prompt (if prev-patch
                     (concat "Previous patch applied:\n```diff\n"
                             prev-patch
                             "\n```\n\nRevise the implementation:\n"
                             base-prompt)
                   base-prompt))
         (action-buffer (mevedel--action-buffer t)))
    (mevedel--clear-patch-buffer)
    (display-buffer action-buffer)
    (with-current-buffer action-buffer
      (gptel-request prompt
        :buffer action-buffer
        :callback #'mevedel--intercept-tool-results
        :preset 'mevedel-revise))))

(defun mevedel-discuss-directive (directive &optional buffer)
  "Discuss DIRECTIVE with read-only tools only.
No file modifications will be made."
  (interactive)
  (let* ((prompt (mevedel--build-prompt-from-directive directive))
         (action-buffer (mevedel--action-buffer t)))
    (display-buffer action-buffer)
    (with-current-buffer action-buffer
      (gptel-request prompt
        :buffer action-buffer
        :callback #'mevedel--intercept-tool-results
        :preset 'mevedel-discuss))))

(defun mevedel--build-prompt-from-directive (directive)
  "Build LLM prompt from DIRECTIVE instruction overlay.
Similar to existing mevedel prompt generation."
  ;; This will call existing mevedel prompt building logic
  ;; Just needs to not go through macher
  (mevedel--generate-prompt-for-directive directive))
```

### Phase 2: Update Integration Points

#### 2.1 Update mevedel-instructions.el
**Lines to change:** ~15 locations

Replace:
- `(macher-workspace)` → `(mevedel--project-root)`
- `(macher--workspace-root workspace)` → `(mevedel--project-root)`
- `(macher-patch-buffer ...)` → `(mevedel--patch-buffer ...)`
- Command aliases (lines 1397-1402):
  ```elisp
  (defalias 'mevedel--ov-actions-implement 'mevedel-implement-directive)
  (defalias 'mevedel--ov-actions-revise 'mevedel-revise-directive)
  (defalias 'mevedel--ov-actions-discuss 'mevedel-discuss-directive)
  ```

Enhance undo history:
- Track full patch content in instruction history
- Add `mevedel--get-instruction-last-patch` function

#### 2.2 Update mevedel-utilities.el
**Lines to change:** ~4 locations

Replace:
- `(macher--workspace-root (macher-workspace))` → `(mevedel--project-root)`
- Patch buffer access

#### 2.3 Update mevedel-diff-apply.el
**Lines to change:** ~2 locations

Replace:
- Workspace root logic

### Phase 3: Testing and Documentation

#### 3.1 Rewrite test suite
**Files:** test-mevedel-*.el

- Remove all `cl-letf` mocking of macher functions
- Mock `gptel-request` instead
- Mock tool execution
- Test preset selection
- Test patch capture
- Test custom tools

Example:
```elisp
(describe "mevedel-implement-directive"
  (it "should use mevedel-implement preset"
    (spy-on 'gptel-request)
    (mevedel-implement-directive test-directive)
    (expect 'gptel-request
            :to-have-been-called-with
            :preset 'mevedel-implement)))
```

#### 3.2 Update documentation
**Files:** README.md, mevedel.el (package headers)

Changes:
- Dependency: `macher (>= 0.3.0)` → `gptel-agent (>= 0.1.0)`
- Add project root allowlist documentation
- Document custom tools (ask_user, request_directory_access)
- Update installation instructions
- Add migration notes

## Estimated Effort

| Phase | Task | Lines of Code | Time |
|-------|------|---------------|------|
| 1.1 | Remove macher | -30 | 0.5h |
| 1.2 | Project root allowlist | +50 | 1h |
| 1.3 | Buffer management | +100 | 2h |
| 1.4 | Presets | +60 | 1h |
| 1.5 | Patch capture | +80 | 2h |
| 1.6 | Custom tools | +80 | 1.5h |
| 1.7 | Action commands | +150 | 3h |
| 2.1 | Update instructions.el | ~15 edits | 1.5h |
| 2.2 | Update utilities.el | ~4 edits | 0.5h |
| 2.3 | Update diff-apply.el | ~2 edits | 0.5h |
| 3.1 | Test suite | 27 tests | 4h |
| 3.2 | Documentation | - | 1h |
| **Total** | | **~520 net lines** | **18-20h** |

## Risk Mitigation

1. **Work in separate branch** - Keep master stable
2. **Incremental commits** - One phase at a time
3. **Test after each phase** - Ensure functionality preserved
4. **Keep macher code commented** - Easy rollback if needed

## Success Criteria

- [ ] All tests pass
- [ ] No macher references remain in code
- [ ] All three presets work (implement/revise/discuss)
- [ ] Patch buffer correctly captures edits
- [ ] Custom tools (ask_user, request_directory_access) function
- [ ] Documentation updated
- [ ] Can execute directives from instruction overlays
- [ ] Can work directly from chat buffer

## Open Questions

1. Should we keep action buffer per-project or single global?
   - Decision: Start with single global, add per-project if needed

2. How to handle multiple concurrent directive executions?
   - Decision: Single execution at a time, queue if needed

3. Format for patch buffer - unified diff or separate sections?
   - Decision: Separate sections with file headers (easier to review)
