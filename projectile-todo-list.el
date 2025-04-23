;;; projectile-todo-list.el --- Project-wide TODO/FIXME scanner using Projectile -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Josh Burns
;; Author: Josh Burns
;; Created: 23 Apr 2025
;; Version: 0.1
;; Package-Requires: ((emacs "27.1")
;;                    (projectile "2.9.1"))
;; Keywords: convenience, tools, project, todo, productivity
;; URL: https://github.com/joshburnsxyz/projectile-todo-list.el

;;; Commentary:

;; This package provides a simple tool to scan your current Projectile project
;; for inline comments like TODO, FIXME, and HACK.  It displays them in a clickable
;; buffer that allows you to jump directly to the corresponding line in the source code.

;; It uses Emacs's syntax parsing where possible to confirm matches occur in
;; comments, and falls back to a customizable map of major modes and comment
;; markers for extra flexibility.

;;; Code:

(require 'projectile)
(require 'subr-x) ;; for string-trim

(defgroup projectile-todo-list nil
  "Scan and list TODO/FIXME style comments in your projectile project."
  :group tools)

(defcustom projectile-todo-list-keywords '("TODO" "FIXME" "HACK" "NOTE")
  "Keywords to search for in code file comments."
  :type '(repeat string)
  :group 'projectile-todo-list)

(defcustom projectile-todo-list-comment-markers
  '((python-mode . "#")
    (emacs-lisp-mode . ";;")
    (js-mode . "//")
    (typescript-mode . "//")
    (go-mode . "//")
    (c-mode . "//")
    (c++-mode . "//")
    (ruby-mode . "#"))
  "Alist mapping major modes to comment line markers. Used as a fallback, in case you're having issues with the automatic detection. You can expand the alist."
  :type '(alist :key-type symbol :value-type string)
  :group 'projectile-todo-list)

(defvar projectile-todo-list-results-buffer "*projectile-todo-list"
  "Name of the buffer used to display results.")

(defun projectile-todo-list--get-project-files ()
  "Return the file tree of the current project."
  (if (projectile-project-p)
      (mapcar (lambda (f) (expand-file-name f (projectile-project-root)))
	      (projectile-current-project-files))
    (error "Not in a valid Projectile project")))

(defun projectile-todo-list--line-starts-with-comment (line mode)
  "Return non-nil if LINE starts with the comment marker for MODE."
  (let ((marker (cdr (assoc mode projectile-todo-list-comment-markers))))
    (when marker
      (string-match-p (concat "^\\s-*" (regexp-quote marker)) line))))

(provide 'projectile-todo-list)

;;; projectile-todo-list.el ends here
