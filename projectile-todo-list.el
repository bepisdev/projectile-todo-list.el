;;; projectile-todo-list.el --- Project-wide TODO/FIXME scanner using Projectile -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Josh Burns
;; Author: Josh Burns
;; Created: 23 Apr 2025
;; Version: 0.1
;; URL: https://github.com/joshburnsxyz/projectile-todo-list.el
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
      (let ((project-root (projectile-project-root)))
	(mapcar (lambda (f) (expand-file-name f project-root))
		(projectile-project-files project-root)))
    (error "Not in a valid projectile project")))
(defun projectile-todo-list--line-starts-with-comment (line mode)
  "Return non-nil if LINE starts with the comment marker for MODE."
  (let ((marker (cdr (assoc mode projectile-todo-list-comment-markers))))
    (when marker
      (string-match-p (concat "^\\s-*" (regexp-quote marker)) line))))

(defun projectile-todo-list--scan-file (file)
  "Scan FILE for TODO-style comments. Returns a list."
  (let ((matches '()))
    (with-temp-buffer
      (insert-file-contents file)
      ;; Auto detect FILE major mode
      (delay-mode-hooks (set-auto-mode))
      (let ((mode major-mode))
	(goto-char (point-min))
	(while (re-search-forward (regexp-opt projectile-todo-list-keywords 'words) nil t)
	  (if (nth 4 (syntax-ppss))
	      (let ((line (line-number-at-pos))
		    (text (string-trim (buffer-substring (line-beginning-position) (line-end-position)))))
		(push (list :file file :line line :text text) matches))
	    ;; Fallback: Check list of comment markers
	    (let* ((text (buffer-substring (line-beginning-position) (line-end-position)))
		   (line (line-number-at-pos)))
	      (when (projectile-todo-list--line-starts-with-comment text mode)
		(push (list :file file :line line :text text (string-trim text)) matches)))))))
    matches))

(defun projectile-todo-list--collect-todos ()
  "Walk file tree of the current project and collect TODO-style comments."
  (let ((files (projectile-todo-list--get-project-files))
	(results '()))
    (dolist (file files)
      (setq results (append results (projectile-todo-list--scan-file file))))
    results))

(defun projectile-todo-list--display-results (results)
  "Display RESULTS in the results buffer."
  (with-current-buffer (get-buffer-create projectile-todo-list-results-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%-8s %-40s %s\n" "Line" "File" "Text"))
      (insert (make-string 80 ?-) "\n")
      (dolist (item results)
        (let* ((file (plist-get item :file))
               (line (plist-get item :line))
               (text (plist-get item :text)))
          (insert-text-button
           (format "%-8d %-40s %s\n" line (file-name-nondirectory file) text)
           'action (lambda (_) (find-file file) (goto-line line))
           'follow-link t))))
    (read-only-mode 1)
    (goto-char (point-min)))
  (display-buffer projectile-todo-list-results-buffer))

;;;###autoload
(defun projectile-todo-list-run ()
  "Scan and list TODO/FIXME style comments in your projectile project."
  (interactive)
  (if (fboundp 'make-thread)
      (progn
        (message "Scanning project for TODOs...")
        (make-thread
         (lambda ()
           (let ((results (projectile-todo-list--collect-todos)))
             ;; Schedule back on main thread
             (run-at-time 0 nil #'projectile-todo-list--display-results results)))))
    ;; Fallback for older Emacs versions or builds with no thread support
    (let ((results (projectile-todo-list--collect-todos)))
      (projectile-todo-list--display-results results))))

(provide 'projectile-todo-list)

;;; projectile-todo-list.el ends here
