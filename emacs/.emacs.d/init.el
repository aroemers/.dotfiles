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
  (global-hl-line-mode t)		; highlight current line

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
  ("C-c b" . my/switch-to-prev-buffer)
  ("C-c i" . my/open-emacs-init))


;;;----------------------------------------------------------------------
;;; Scala programming, uses build-in eglot with metals
;;;----------------------------------------------------------------------

(use-package scala-mode)		; scala syntax support

(use-package eglot			; built-in LSP integration
  :bind
  (:map eglot-mode-map
   ("C-c c a" . eglot-code-actions)
   ("C-c c o" . eglot-code-action-organize-imports)
   ("C-c c r" . eglot-rename)
   ("C-c c f" . eglot-format))
  (:map flymake-mode-map
   ("C-c ! n" . flymake-goto-next-error)
   ("C-c ! p" . flymake-goto-prev-error)
   ("C-c ! l" . flymake-show-diagnostics-buffer)))


;;;----------------------------------------------------------------------
;;; Clojure programming
;;;----------------------------------------------------------------------

(use-package clojure-mode)		; clojure syntax support

(use-package cider)			; clojure REPL integration


;;;----------------------------------------------------------------------
;;; Lisp programming (Clojure, Emacs Lisp)
;;;----------------------------------------------------------------------

(use-package paredit			; balance, navigate and move parens
  :hook
  (clojure-mode . paredit-mode)		; enable for clojure
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

(use-package vundo			; visual undo/redo
  :bind
  ("C-x u" . vundo))

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

(use-package avy			; quick navigation on screen
  :bind
  ("M-j" . avy-goto-char-timer))

;; Package which-key is incompatible with devil mode.
;; There is a PR here: https://github.com/justbur/emacs-which-key/pull/353.
;; It won't be merged, as which-key will (probably) be part of Emacs 30.
;; Perhaps the incompatibility will solved then.
(use-package devil			; shortcuts using ,
  :config
  (global-devil-mode))
