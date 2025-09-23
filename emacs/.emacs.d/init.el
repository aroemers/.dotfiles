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

(defun my/open-emacs-init ()
  "Open init.el quickly for editing."
  (interactive)
  (find-file-other-window "~/.emacs.d/init.el"))

(defun my/copy-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun my/project-ztree ()
  "Open ztree at project root."
  (interactive)
  (ztree-dir (project-root (project-current t))))

(defun my/toggle-zoom ()
  "Zoom active window or restore windows before zoom."
  (interactive)
  (if (get-register ?Z)
      (save-excursion
	(jump-to-register ?Z)
	(set-register ?Z nil))
    (progn (window-configuration-to-register ?Z)
	   (delete-other-windows))))


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
  (load-theme 'modus-vivendi t)		; set theme
  (set-face-attribute 'tab-bar nil :inherit 'mode-line)	; match tab-bar with theme
  (menu-bar-mode -1)			; hide menu bar
  (recentf-mode t)			; remember recently opened files
  (savehist-mode t)			; save history of commands
  (show-paren-mode t)			; highlight matching parens
  (global-hl-line-mode t)		; highlight current line
  (electric-pair-mode t)		; automatic closing parens
  (save-place-mode t)			; save last location in files
  (global-completion-preview-mode t)	; inline completion suggestion

  (when window-system
    (scroll-bar-mode -1)		  ; hide scroll bar
    (set-face-font 'default "JetBrains Mono 15") ; set font and size
    ;;(toggle-frame-maximized)		; maximize frame
    (set-frame-size nil 200 60)		; set default frame size
    (tool-bar-mode -1))			; hide tool bar

  :custom
  (compilation-scroll-output t)		; auto-scroll *compilation* buffer
  (completion-auto-select t)		; instant active completion buffer
  (inhibit-startup-screen t)		; no emacs startup buffer
  (interprogram-cut-function 'my/copy-to-osx) ; integrate macOS clipboard
  (tab-always-indent 'complete)		; complete-at-point if already indented
  (warning-minimum-level :error)	; don't pop up warning buffer
  (word-wrap t)				; nicely word wrap
  (backward-delete-char-untabify-method 'hungry) ; remove tabs and spaces on backspace
  (tab-bar-tab-hints 1)			; show tab numbers
  (tab-bar-close-button-show nil)	; hide tab close button
  (dired-listing-switches "-alh")	; human readable file sizes in dired
  (xref-search-program 'ripgrep)	; use ripgrep for searching

  :hook
  (before-save . delete-trailing-whitespace)
  (compilation-filter . ansi-color-compilation-filter)

  :bind
  ("C-x f" . recentf-open)
  ("C-c b" . mode-line-other-buffer)
  ("C-c i" . my/open-emacs-init)
  ("C-x z" . my/toggle-zoom)

  (:map prog-mode-map
   ("DEL" . backward-delete-char-untabify)))


;;;----------------------------------------------------------------------
;;; Scala programming, uses build-in eglot with metals
;;;----------------------------------------------------------------------

(use-package scala-mode)		; scala syntax support

(use-package eglot			; built-in LSP integration
  :bind
  ("M->" . xref-find-apropos)

  (:map eglot-mode-map
   ("C-c c a" . eglot-code-actions)
   ("C-c c o" . eglot-code-action-organize-imports)
   ("C-c c r" . eglot-rename)
   ("C-c c f" . eglot-format)
   ("C-c c i" . eglot-find-implementation))
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
;;; Common Lisp programming
;;;----------------------------------------------------------------------

(use-package slime			; common lisp support
  :init
  (setq inferior-lisp-program "sbcl"))


;;;----------------------------------------------------------------------
;;; Lisp programming (Clojure, Common Lisp, Emacs Lisp)
;;;----------------------------------------------------------------------

(use-package paredit			; balance, navigate and move parens
  :hook
  (clojure-mode . paredit-mode)		; enable for clojure
  (lisp-mode . paredit-mode)		; enable for common lisp
  (emacs-lisp-mode . paredit-mode))	; enable for emacs lisp


;;;----------------------------------------------------------------------
;;; Other modes
;;;----------------------------------------------------------------------

(use-package markdown-mode)		; markdown syntax support

(use-package yaml-mode)			; yaml syntax support


;;;----------------------------------------------------------------------
;;; RSS feeds
;;;----------------------------------------------------------------------

(use-package elfeed			; rss reader
  :vc (:url "https://github.com/skeeto/elfeed.git" :rev "3.4.2")

  :custom
  (elfeed-feeds '("https://planet.clojure.in/atom.xml"
                  "https://planet.emacslife.com/atom.xml"
                  "https://lobste.rs/rss")))


;;;----------------------------------------------------------------------
;;; Browsing, eww!
;;;----------------------------------------------------------------------

(use-package eww			; emacs text based browser
  :custom
  (eww-search-prefix "https://www.ecosia.org/search?q="))

(use-package elpher)			; gemini browser


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
  (which-key-mode)

  :custom
  (which-key-idle-delay 0.5))

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
  :custom
  (avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))	; colemak home row
  (avy-style 'pre)			; path before matches

  :bind
  ("M-j"   . avy-goto-char-timer)
  ("M-g j" . avy-goto-line))

;; Package which-key is incompatible with devil mode.
;; There is a PR here: https://github.com/justbur/emacs-which-key/pull/353.
;; It won't be merged, as which-key will (probably) be part of Emacs 30.
;; Perhaps the incompatibility will solved then.
(use-package devil			; shortcuts using ,
  :config
  (global-devil-mode))

(use-package ztree			; file tree navigation
  :bind
  (:map project-prefix-map
   ("t" . my/project-ztree)))

(use-package ace-window			; other-window, but with numbers
  :bind
  ("M-o" . ace-window))

(use-package eat			; emulate a terminal
  :custom
  (eat-term-name "xterm-256color"))	; fix backspace
