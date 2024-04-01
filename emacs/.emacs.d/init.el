;;;; Emacs setup by Arnout.
;;;;
;;;; It uses as much as is provided since Emacs 29.1 (like
;;;; use-package, project, modus themes, eglot) with a few extra
;;;; conveniences.
;;;;
;;;; Programming support is setup for Clojure and Scala.


;;;----------------------------------------------------------------------
;;; Custom functions
;;;----------------------------------------------------------------------

(defun my/switch-to-prev-buffer ()
  "Switch to the previously active buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) nil)))

(defun my/open-emacs-init ()
  "Open init.el quickly for editing."
  (interactive)
  (find-file-other-window "~/.emacs.d/init.el"))

(defun my/copy-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))


;;;----------------------------------------------------------------------
;;; Package management setup
;;;----------------------------------------------------------------------

(progn					; auto-install missing packages
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))


;;;----------------------------------------------------------------------
;;; General configuration
;;;----------------------------------------------------------------------

(use-package emacs			; emacs built-in customization
  :config
  (column-number-mode t)	        ; show column number in mode-line
  (fido-vertical-mode t)	        ; visual minibuffer completion
  (global-auto-revert-mode t)	        ; auto-revert files from disk
  (load-theme 'modus-vivendi t)
  (menu-bar-mode -1)			; hide menu bar
  (recentf-mode t)			; remember recently opened files
  (savehist-mode t)			; save history of commands
  (show-paren-mode t)			; highlight matching parens

  (when window-system
    (scroll-bar-mode -1)		; hide scroll bar
    (set-face-font 'default "SF Mono 15") ; set font and size
    (toggle-frame-maximized)		; maximize frame
    (tool-bar-mode -1))			; hide tool bar

  :custom
  (inhibit-startup-screen t)		; no emacs startup buffer
  (interprogram-cut-function 'my/copy-to-osx) ; integrate macOS clipboard
  (warning-minimum-level :error)	; don't pop up warning buffer
  (word-wrap t)				; nicely word wrap

  :hook
  (before-save . delete-trailing-whitespace)

  :bind
  ("C-x f" . recentf-open)
  ("M-o"   . other-window)
  ("M-SPC" . completion-at-point)
  ("C-c b" . my/switch-to-prev-buffer)
  ("C-c i" . my/open-emacs-init))


;;;----------------------------------------------------------------------
;;; Scala programming, uses build-in eglot with metals
;;;----------------------------------------------------------------------

(use-package scala-mode)		; scala syntax support

(use-package eglot)			; built-in LSP integration


;;;----------------------------------------------------------------------
;;; Clojure programming
;;;----------------------------------------------------------------------

(use-package clojure-ts-mode)		; clojure syntax support

(use-package cider)			; clojure REPL integration


;;;----------------------------------------------------------------------
;;; Lisp programming (Clojure, Emacs Lisp)
;;;----------------------------------------------------------------------

(use-package paredit			; balance, navigate and move parens
  :hook
  (clojure-ts-mode . paredit-mode)	; enable for clojure
  (emacs-lisp-mode . paredit-mode))	; enable for emacs lisp


;;;----------------------------------------------------------------------
;;; Other modes
;;;----------------------------------------------------------------------

(use-package markdown-mode)		; markdown syntax support

(use-package yaml-mode)			; yaml syntax support


;;;----------------------------------------------------------------------
;;; Utility packages
;;;----------------------------------------------------------------------

(use-package magit)			; superb git support

(use-package undo-tree			; visual and tracking undo/redo
  :diminish undo-tree-mode
  :custom
  (global-undo-tree-mode t)		; available everywhere
  (undo-tree-auto-save-history nil))	; don't save undo tree to files

(use-package goto-chg			; move cursor to last change pos
  :bind
  ("M-l" . goto-last-change))

(use-package which-key			; show shortcuts that can follow
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package exec-path-from-shell	; ensure path is read from shell
  :defer 1)

(use-package diminish)			; hide modes marked with :diminish

(use-package expand-region		; expand selection to enclosing scope
  :bind
  ("<backtab>" . er/expand-region))	; bound to shift-tab

(use-package multiple-cursors		; have multiple cursors feature
  :bind
  ("M-n" . mc/mark-next-like-this)	; mark next line or selection
  ("M-a" . mc/mark-all-like-this-dwim)) ; mark all of selection

(use-package uniquify			; ensure unique buffer names
  :ensure nil)

(use-package diff-hl			; version control in gutter
  :config
  (global-diff-hl-mode)			; enable in all buffers

  (unless window-system			; show in margin in terminal
    (diff-hl-margin-mode)))
