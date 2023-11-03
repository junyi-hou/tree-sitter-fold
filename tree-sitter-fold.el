;;; tree-sitter-fold.el --- code folding using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Junyi Hou
;;
;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (tree-sitter "0.15.1") (dash "2.19.0"))
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This package provides a code-folding mechanism based on tree-sitter
;; package. Turn on the minor-mode `tree-sitter-fold-mode' to enable
;; this mechanism. Note that all functionalities provided here based on the
;; `tree-sitter-mode', and thus it should be enabled before
;; `tree-sitter-fold-mode' can properly fold codes.

;;; Code:

(require 'tree-sitter)
(require 'dash)

;; =============
;; customization
;; =============

(defgroup tree-sitter-fold nil
  "Code folding using tree-sitter."
  :group 'tree-sitter
  :prefix "tree-sitter-fold-")

(defcustom tree-sitter-fold-foldable-node-alist
  '((python-mode . (function_definition class_definition)) ;
    (go-mode . (type_declaration function_declaration method_declaration))
    (ess-r-mode . (brace_list))
    (rust-mode . (struct_item enum_item function_item trait_item impl_item
		  match_expression mod_item macro_definition macro_invocation))
    (rustic-mode . (struct_item enum_item function_item trait_item impl_item
		    match_expression mod_item macro_definition macro_invocation))
    (nix-mode . (attrset function)))
  "An alist of (mode . (list of tree-sitter-nodes considered foldable in this mode))."
  :type '(alist :key-type symbol :value-type (repeat symbol))
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-range-alist
  '((python-mode . ((function_definition . tree-sitter-fold-range-python)
                    (class_definition . tree-sitter-fold-range-python)))
    (ess-r-mode . ((brace_list . tree-sitter-fold-range-r)))
    (nix-mode . ((attrset . tree-sitter-fold-range-nix-attrset)
                 (function . tree-sitter-fold-range-nix-function)))
    (rust-mode . ((struct_item . tree-sitter-fold-range-rust)
		  (enum_item . tree-sitter-fold-range-rust)
		  (function_item . tree-sitter-fold-range-rust)
		  (trait_item . tree-sitter-fold-range-rust)
		  (impl_item . tree-sitter-fold-range-rust)
		  (match_expression . tree-sitter-fold-range-rust)
		  (mod_item . tree-sitter-fold-range-rust)
		  (macro_definition . tree-sitter-fold-range-rust-macro)
		  (macro_invocation . tree-sitter-fold-range-rust)))
    (rustic-mode . ((struct_item . tree-sitter-fold-range-rust)
		    (enum_item . tree-sitter-fold-range-rust)
		    (function_item . tree-sitter-fold-range-rust)
		    (trait_item . tree-sitter-fold-range-rust)
		    (impl_item . tree-sitter-fold-range-rust)
		    (match_expression . tree-sitter-fold-range-rust)
		    (mod_item . tree-sitter-fold-range-rust)
		    (macro_definition . tree-sitter-fold-range-rust-macro)
		    (macro_invocation . tree-sitter-fold-range-rust)))
    (go-mode . ((type_declaration . tree-sitter-fold-range-go-type-declaration)
                (function_declaration . tree-sitter-fold-range-go-method)
                (method_declaration . tree-sitter-fold-range-go-method))))
  "An alist of (major-mode . (foldable-node-type . function)).
FUNCTION is used to determine where the beginning and end for FOLDABLE-NODE-TYPE
in MAJOR-MODE.  It should take a single argument (the syntax node with type
FOLDABLE-NODE-TYPE) and return the buffer positions of the beginning and end of
the fold in a cons cell.  See `tree-sitter-fold-range-python' for an example."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type function))
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-mode-hook nil
  "Hook to run when enabling `tree-sitter-fold-mode'."
  :type 'hook
  :group 'tree-sitter-fold)

;; ==========
;; minor mode
;; ==========

(define-minor-mode tree-sitter-fold-mode
  "Folding code using tree sitter."
  :init-value nil
  :lighter nil
  (if tree-sitter-fold-mode
      (progn
        (setq-local line-move-ignore-invisible t)
        (add-to-invisibility-spec '(tree-sitter-fold . t))

        ;; evil integration
        (if (bound-and-true-p evil-fold-list)
            (add-to-list 'evil-fold-list
                         '((tree-sitter-fold-mode)
                           :open tree-sitter-fold-open
                           :close tree-sitter-fold-close
                           :open-rec tree-sitter-fold-open-recursively
                           :open-all tree-sitter-fold-open-all
                           :close-all tree-sitter-fold-close-all)))

        (run-hooks 'tree-sitter-fold-mode-hook))
    (remove-from-invisibility-spec '(tree-sitter-fold . t))
    (let ((tree-sitter-mode t))
      (tree-sitter-fold-open-all))))

;; ============================================
;; using `tree-sitter' to determine fold range.
;; ============================================

(defun tree-sitter-fold--foldable-node-at-pos (&optional pos)
  "Return the smallest foldable node at POS.  If POS is nil, use `point'.
Raise `user-error' if no foldable node is found.
This function is borrowed from `tree-sitter-node-at-point'."
  (let* ((pos (or pos (point)))
         (foldable-types (alist-get major-mode tree-sitter-fold-foldable-node-alist))
         (root (tsc-root-node tree-sitter-tree))
         (node (tsc-get-descendant-for-position-range root pos pos)))
    (let ((current node) result)
      (while current
        (if (memq (tsc-node-type current) foldable-types)
            (setq result current
                  current nil)
          (setq current (tsc-get-parent current))))
      (or result (user-error "No foldable node found at POS")))))

(defun tree-sitter-fold--get-fold-range (node)
  "Return the beginning (as buffer position) of fold for NODE."
  (-if-let* ((fold-alist (alist-get major-mode tree-sitter-fold-range-alist))
             (fn (alist-get (tsc-node-type node) fold-alist)))
      (if (functionp fn)
          (funcall fn node)
        (user-error
         (format "Current node is not found in `tree-sitter-fold-range-alist' in %s"
                 major-mode)))))

;; ========
;; overlays
;; ========

(defun tree-sitter-fold--create-overlay (range)
  "Create invisible overlay in RANGE."
  (when (not (null range))
    (let ((ov (make-overlay (car range) (cdr range))))
      (overlay-put ov 'invisible 'tree-sitter-fold)
      (overlay-put ov 'isearch-open-invisible #'tree-sitter-fold--isearch-open))))

(defun tree-sitter-fold--isearch-open (ov)
  "Open overlay OV during `isearch' session."
  (delete-overlay ov))

(defun tree-sitter-fold-overlay-at (node)
  "Return the tree-sitter-fold overlay at NODE if NODE is foldable and folded.  Return nil otherwise."
  (-when-let* ((foldable-types (alist-get major-mode tree-sitter-fold-foldable-node-alist))
               (_ (memq (tsc-node-type node) foldable-types))
               (range (tree-sitter-fold--get-fold-range node)))
    (thread-last (overlays-in (car range) (cdr range))
      (seq-filter (lambda (ov)
                    (and (eq (overlay-get ov 'invisible) 'tree-sitter-fold)
                         (= (overlay-start ov) (car range))
                         (= (overlay-end ov) (cdr range)))))
      car)))

;; ========
;; commands
;; ========

(defmacro tree-sitter-fold--ensure-ts (&rest body)
  "Run BODY only if `tree-sitter-mode' is enabled."
  (declare (indent 0))
  `(if (bound-and-true-p tree-sitter-mode)
       (progn ,@body)
     (user-error "Ignored, tree-sitter-mode is not enable in the current buffer")))

(defun tree-sitter-fold-close (&optional node)
  "Fold the syntax node at `point' if it is foldable.
Foldable nodes are defined in `tree-sitter-fold-foldable-node-alist' for the current
`major-mode'.  If no foldable node is found in point, do nothing."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (let ((node (or node (tree-sitter-fold--foldable-node-at-pos))))
      ;; make sure I do not create multiple overlays for the same fold
      (-when-let* ((ov (tree-sitter-fold-overlay-at node)))
        (delete-overlay ov))
      (tree-sitter-fold--create-overlay (tree-sitter-fold--get-fold-range node)))))

(defun tree-sitter-fold-open ()
  "Open the fold of the syntax node in which `point' resides.
If the current node is not folded or not foldable, do nothing."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (-when-let* ((node (tree-sitter-fold--foldable-node-at-pos))
                 (ov (tree-sitter-fold-overlay-at node)))
      (delete-overlay ov))))

(defun tree-sitter-fold-open-recursively ()
  "Open recursively folded syntax NODE that are contained in the node at `point'."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (when-let* ((node (tree-sitter-fold--foldable-node-at-pos))
                (beg (tsc-node-start-position node))
                (end (tsc-node-end-position node)))
      (thread-last (overlays-in beg end)
        (seq-filter (lambda (ov) (eq (overlay-get ov 'invisible) 'tree-sitter-fold)))
        (mapc #'delete-overlay)))))

(defun tree-sitter-fold-close-all ()
  "Fold all foldable syntax nodes in the buffer."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (let* ((node (tsc-root-node tree-sitter-tree))
           (patterns (seq-mapcat (lambda (type) `(,(list type) @name))
                                 (alist-get major-mode tree-sitter-fold-foldable-node-alist)
                                 'vector))
           (query (tsc-make-query tree-sitter-language patterns))
           (nodes-to-fold (tsc-query-captures query node #'ignore)))
      (thread-last nodes-to-fold
        (mapcar #'cdr)
        (mapc #'tree-sitter-fold-close)))))

(defun tree-sitter-fold-open-all ()
  "Unfold all syntax nodes in the buffer."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (thread-last (overlays-in (point-min) (point-max))
      (seq-filter (lambda (ov) (eq (overlay-get ov 'invisible) 'tree-sitter-fold)))
      (mapc #'delete-overlay))))

(defun tree-sitter-fold-toggle ()
  "Toggle the syntax node at `point'.
If the current syntax node is not foldable, do nothing."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (let ((node (tree-sitter-fold--foldable-node-at-pos (point))))
      (if-let* ((ov (tree-sitter-fold-overlay-at node)))
          (delete-overlay ov)
        (tree-sitter-fold-close node)))))

;; =================
;; language supports
;; =================

(defun tree-sitter-fold-range-python (node)
  "Return the fold range for `function_definition' and `class_definition' NODE in Python."
  (let* ((named-node (or (tsc-get-child-by-field node :superclasses)
                         (tsc-get-child-by-field node :return_type)
                         (tsc-get-child-by-field node :parameters)
                         (tsc-get-child-by-field node :name)))
         ;; the colon is an anonymous node after return_type or parameters node
         (beg (tsc-node-end-position (tsc-get-next-sibling named-node)))
         (end (tsc-node-end-position node)))
    (cons beg end)))

(defun tree-sitter-fold-range-rust (node)
  "Return the fold range for foldable NODE in Rust.
Foldable node-types are `struct-item', `enum_item', `function-item',`trait_item',
`impl_item', `mod_item', `macro_invocation' and `match_expression'."
  (let ((current_node (tsc-get-nth-child node 0))
	(result nil))
    (while (not (eq current_node nil))
      (if (or (eq (tsc-node-type current_node) 'field_declaration_list)
	      (eq (tsc-node-type current_node) 'ordered_first_declaration_list)
	      (eq (tsc-node-type current_node) 'enum_variant_list)
	      (eq (tsc-node-type current_node) 'block)
	      ;; for impl, mod and trait items
	      (eq (tsc-node-type current_node) 'declaration_list)
	      (eq (tsc-node-type current_node) 'token_tree)
	      ;; match block won't fold semicolon in the end, so it makes clear
	      ;; if it is present at the end of the statement even when folded
	      (eq (tsc-node-type current_node) 'match_block))
	  (let ((beg (tsc-node-start-position current_node))
		(end (tsc-node-end-position current_node)))
	    (setq current_node nil)
	    (setq result (cons beg end)))
	(setq current_node (tsc-get-next-sibling current_node))))
    result))

(defun tree-sitter-fold-range-rust-macro (node)
  "Return the fold range for `macro_definition' NODE in Rust."
  (let* ((children (tsc-count-children node))
	(result nil)
	(last_bracket (tsc-get-nth-child node (- children 1)))
	(first_bracket (tsc-get-nth-child node 2)))
    (setq result (cons
		  (tsc-node-start-position first_bracket)
		  (+ 1 (tsc-node-start-position last_bracket))))
    result))

(defun tree-sitter-fold-range-r (node)
  "Return the fold range for `brace_list' NODE in R."
  (let ((beg (tsc-node-end-position (tsc-get-nth-child node 0)))
        (end (1- (tsc-node-end-position node))))
    (cons beg end)))

(defun tree-sitter-fold-range-nix-attrset (node)
  "Return the fold range for `attrset' NODE in Nix express language."
  (let ((beg (tsc-node-end-position (tsc-get-nth-child node 0)))
        (end (1- (tsc-node-end-position node))))
    (cons beg end)))

(defun tree-sitter-fold-range-nix-function (node)
  "Return the fold range for `function' NODE in Nix express language."
  (let ((beg (thread-first node
               (tsc-get-child-by-field :formals)
               (tsc-get-next-sibling)
               (tsc-node-end-position)))
        (end (tsc-node-end-position node)))
    (cons beg end)))

(defun tree-sitter-fold-range-go-type-declaration (node)
  "Return the fold range for `type_declaration' NODE in Go language.
Only `struct_type' and `interface_type' nodes can be folded."
  (when-let* ((type-spec-node (tsc-get-nth-child node 1))
              ;; the type_spec node is not named in the Go grammar
              ;; so ensure that the 1-th child is a type_spec node
              (_ (eq (tsc-node-type type-spec-node) 'type_spec))
              (type-node (tsc-get-child-by-field type-spec-node :type))
              (type-node-type (tsc-node-type type-node)))
    (cond
     ;; only struct and interface types can be folded
     ((or (eq type-node-type 'struct_type)
          (eq type-node-type 'interface_type))
      ;; find the end of the "struct" or "interface" keyword
      (let ((beg (1+ (tsc-node-end-position (tsc-get-nth-child type-node 0))))
            (end (tsc-node-end-position node)))
        (cons beg end)))
     (t nil))))

(defun tree-sitter-fold-range-go-method (node)
  "Return the fold range for `method_declaration' NODE in Go language."
  (let* ((named-node (or (tsc-get-child-by-field node :result)
                        (tsc-get-child-by-field node :parameters)))
         (beg (1+ (tsc-node-end-position named-node)))
        (end (tsc-node-end-position node)))
    (cons beg end)))

(provide 'tree-sitter-fold)
;;; tree-sitter-fold.el ends here
