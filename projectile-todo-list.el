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

(provide 'projectile-todo-list)

;;; projectile-todo-list.el ends here
