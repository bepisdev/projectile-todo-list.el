# projectile-todo-list.el

A plugin for projectile.el, That scans the current projectile project for inline comment annotations like `TODO`, `FIXME`, `HACK`, and `NOTE`.

It displays results in a clickable buffer, allowing you to jump directly to the source code where each comment appears. It uses Emacs's built-in syntax analysis to detect comments, with fallback support for customizable comment markers per major mode.

---

## Features

- Scans all files in your current Projectile project.
- Detects TODO-style comments using Emacs syntax parsing or user-defined comment markers.
- Presents results in a clickable, read-only buffer for easy navigation.
- Fully customizable keyword list and comment syntax map.

---

## Installation

To install via [`quelpa-use-package`](https://github.com/quelpa/quelpa-use-package), add the following to your Emacs configuration:

```elisp
(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :after quelpa
  :ensure t)

(use-package projectile-todo-list
  :quelpa (:fetcher github :repo "bepisdev/projectile-todo-list.el")
  :commands (projectile-todo-list-run))

```

## Configuration

You can customize the keywords used to search for and indentify TODO comments.

``` elisp
(add-to-list 'projectile-todo-list-keywords '("BUG" "CUSTOM_NOTE_TAG"))
```

`projectile-todo-list` does its best to auto-detect the symbol(s) used to mark a line as a comment in the context of files `major-mode`via `syntax-ppss`. In the case where you have some esoteric use-case, or are experiencing issues with the auto-detection. You can extend the internal map of comment markers to major modes. That is used as the fallback for the auto-detect.

``` elisp
(add-to-list projectile-todo-list-comment-markers
	'((python-mode . "#")
	  (c-mode . "//")))
```

## Usage

While inside a known projectile project.

``` elisp
M-x projectile-todo-list-run
```
