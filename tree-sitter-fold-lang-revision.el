;;; tree-sitter-fold-lang-revision.el -- revision of the supported language -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Junyi Hou
;;
;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;; Version: 0.3
;; Package-Requires: ((emacs "29.1") (dash "2.19.0") (treesit-auto "0.6.4"))
;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:
(require 'dash)
(require 'treesit)
(require 'treesit-auto)

(defconst tree-sitter-fold-supported-revision
  '((python . "a901729099257aac932d79c60adb5e8a53fa7e6c")
    (yaml . "0e36bed171768908f331ff7dff9d956bae016efb")
    (json . "3fef30de8aee74600f25ec2e319b62a1a870d51e")
    (proto . "42d82fa18f8afe59b5fc0b16c207ee4f84cb185f")
    (java . "2b57cd9541f9fd3a89207d054ce8fbe72657c444")
    (go . "v0.20.0"))
  "An alist of (LANG . REVISION) that specifies the revision for that language.")

(defconst tree-sitter-fold-additional-recipe
  `(,(make-treesit-auto-recipe
      :lang 'nix
      :ts-mode 'nix-ts-mode
      :remap 'nix-mode
      :url "https://github.com/nix-community/tree-sitter-nix"
      :ext "\\.nix\\'"
      :revision "66e3e9ce9180ae08fc57372061006ef83f0abde7"))
  "Additional recipes to be added to `treesit-auto-recipe-list'.")


(defun tree-sitter-fold--set-revision (lang revision)
  "Add REVISION to the treesit-auto-recipe for LANG."
  (-when-let* ((recipe (--first (eq lang (treesit-auto-recipe-lang it)) treesit-auto-recipe-list))
               (_ (not (equal (treesit-auto-recipe-revision recipe) revision))))
    (setf (treesit-auto-recipe-revision recipe) revision)))


(defun tree-sitter-fold--maybe-run-checkout (fn &rest args)
  (-if-let* ((_ (string= (car args) "git"))
             (_ (--filter (string= "-b" it) args))
             (workdir (-last-item args))
             (branch (--first (and (stringp it) (string-match-p "^[a-fA-F0-9]\\{40\\}$" it)) args))
             (args (--remove (member it `("-b" "--depth" "1" ,branch)) args)))
      (progn
        (apply fn args)
        (let ((default-directory workdir))
          (treesit--call-process-signal "git" nil t nil "checkout" branch)))
    (apply fn args)))



;; update treesit-auto-recipe-list to include the revision of the grammar
(with-eval-after-load 'treesit-auto
  ;; add additional recipes
  (setq treesit-auto-recipe-list `(,@treesit-auto-recipe-list ,@tree-sitter-fold-additional-recipe))

  ;; update revision information
  (mapc
   (lambda (info) (pcase info (`(,lang . ,revision) (tree-sitter-fold--set-revision lang revision))))
   tree-sitter-fold-supported-revision)


  ;; support using commit hash in the recipe
  (advice-add #'treesit--call-process-signal :around #'tree-sitter-fold--maybe-run-checkout))

(provide 'tree-sitter-fold-lang-revision)
;;; tree-sitter-fold-lang-revision.el ends here
