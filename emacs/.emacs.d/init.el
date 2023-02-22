;;; arnouts-init --- Arnout's custom Emacs script.
;;;
;;; Commentary:
;;;   This is my custom Emacs init script. Currently it is mostly setup for
;;;   for Clojure and Scala development.
;;;
;;; Code:
;;;   The code is split up in logical sections.


;;;-----------------------------------------------------------------------------
;;; Root initialization.
;;;-----------------------------------------------------------------------------

;; Don't open errer log on warnings on startup.
(setq warning-minimum-level :error)


;;;-----------------------------------------------------------------------------
;;; Package handling.
;;;-----------------------------------------------------------------------------

(require 'package)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(when nil ; debugging tools
  (setq use-package-verbose t)
  (setq use-package-compute-statistics t))


;;;-----------------------------------------------------------------------------
;;; Execution path.
;;;-----------------------------------------------------------------------------

(use-package exec-path-from-shell
  :pin "MELPA"

  :defer 1 ; Load after 1 second idle time, as it has quite some impact on init.

  :config
  (when (memq window-system '(mac ns x))
    (setq shell-file-name "/bin/zsh")
    (exec-path-from-shell-copy-envs '("PATH" "JAVA_HOME"))))


;;;-----------------------------------------------------------------------------
;;; Theming.
;;;-----------------------------------------------------------------------------

;; Load theme package and configure window.
(use-package dracula-theme
  :config
  (load-theme 'dracula t)

  ;; Disable toolbar and menubar.
  (tool-bar-mode -1)
  (menu-bar-mode -1)

  ;; Configure the following on when Emacs is in a window.
  (when window-system
    ;; Disable scrollbar.
    (scroll-bar-mode -1)

    ;; Update fonts and spacing.
    (set-face-attribute 'default nil :height 150 :font "SF Mono")
    (setq-default line-spacing 3)

    ;; Maximize frame on startup.
    (toggle-frame-maximized)

    ;; Set the color of some other parts of Emacs.
    (set-face-background 'cursor "chartreuse1"))

  ;; Configure theme when Emacs is in a terminal.
  (unless window-system
    ;; Reset background from theme to default.
    (set-face-background 'default "unspecified-bg")
    ;; Set a better mode-line background color.
    (set-face-background 'mode-line "#44475a")
    (set-face-background 'mode-line-inactive "unspecified-bg"))

  :custom
  (mouse-wheel-scroll-amount '(2 ((shift) . 1)))
  (mouse-wheel-progressive-speed nil))


;;;-----------------------------------------------------------------------------
;;; LSP support
;;;-----------------------------------------------------------------------------

;; Use the eglot package for LSP support for various languages,
;; including Clojure and Scala.
(use-package eglot
  :defer t)


;;;-----------------------------------------------------------------------------
;;; Clojure development.
;;;-----------------------------------------------------------------------------

;; Require the Clojure mode.
(use-package clojure-mode
  ;; Load when necessary
  :defer t)

;; Enable paredit.
(use-package paredit
  :hook
  (clojure-mode . paredit-mode))

;; Add a simple REPL for Clojure
(use-package inf-clojure
  ;; Load when necessary
  :defer t)

;; Add completion support for Clojure through company (used by eglot)
(use-package company
  :hook
  ((clojure-mode . company-mode)
   (inf-clojure-mode . company-mode))

  :bind
  ("M-SPC" . company-complete))


;;;-----------------------------------------------------------------------------
;;; Ruby development.
;;;-----------------------------------------------------------------------------

;; Automatically close blocks by inserting "end" when typing a
;; block-keyword, followed by a space.
(use-package ruby-end
  :hook
  (ruby-mode . ruby-end-mode))

;; Flycheck with Rubocop.
(use-package flycheck
  :hook
  (ruby-mode . flycheck-mode))

;; Run IRB as a process inside Emacs.
(use-package inf-ruby
  :pin "MELPA"

  :hook
  (ruby-mode . inf-ruby-minor-mode))

;; Automatically close parenthesis and the like.
(use-package smartparens
  :hook
  (ruby-mode . smartparens-strict-mode))

;; Install rspec niceties.
(use-package rspec-mode
  :hook
  (ruby-mode . rspec-mode))


;;;-----------------------------------------------------------------------------
;;; Scala development.
;;;-----------------------------------------------------------------------------

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :commands
  sbt-start
  sbt-command

  :custom
  (sbt:program-options '("-Dsbt.supershell=false")))

(use-package company
  :hook
  (scala-mode . company-mode)

  :bind
  ("M-SPC" . company-complete))


;;;-----------------------------------------------------------------------------
;;; Emacs Lisp development.
;;;-----------------------------------------------------------------------------

;; Enable paredit.
(use-package paredit
  :hook
  (emacs-lisp-mode . paredit-mode))

;; Enable function signatures.
(use-package eldoc
  :hook
  (emacs-lisp-mode . eldoc-mode))


;;;-----------------------------------------------------------------------------
;;; Ido Mode related
;;;-----------------------------------------------------------------------------

;; Enable ido mode. Remember that C-j uses the current selection in
;; the ido minibuffer, which comes in handy when selecting a dired
;; directory (M-x dired).
(use-package ido
  :config
  (ido-mode t)

  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t))

;; Have a better ido-everywhere.
(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode t))

;; Align the options vertically, and make up-down for options, and
;; left-right for history.
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode t)

  :custom
  (ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

;; Have a better fuzzy matching, and highlight the matching characters.
(use-package flx-ido
  :config
  (flx-ido-mode 1))

;; Have ido like completions for M-x (execute-extended-command).
;; It also gives a M-X shortcut to only show the commands from the
;; currend major mode.
(use-package smex
  :config
  (smex-initialize)

  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands))


;;;-----------------------------------------------------------------------------
;;; Common utility packages.
;;;-----------------------------------------------------------------------------

;; Use project package for switching projects, finding files, etc
(use-package project
  :defer t)

;; Browse through the undo tree, using C-x u
(use-package undo-tree
  :config
  (global-undo-tree-mode)

  :custom
  (undo-tree-auto-save-history nil))

;; Show what has changed since the last commit in a file.
(use-package git-gutter
  ;; Load after 2 seconds idle time
  :defer 2

  :config
  (global-git-gutter-mode t))

;; Easily expand a region to the enclosing scope, using backtab (shift+tab).
;; The package advices C-= as key, but it is difficult to get that to work with
;; osx terminals (Terminal.app and iTerm2). Haven't found a solution yet.
;; This package is Clojure compatible.
(use-package expand-region
  :bind
  ("<backtab>" . er/expand-region))

;; Have multiple cursors, based on the current region (or just a cursor on the
;; next line if no region is selected). Use M-n and/or M-p to have a cursor
;; on the next or previous occurence. The package advices to use C-> and C-<,
;; but this won't work with osx terminals (Terminal.app and iTerm2) without
;; extensive abuse of keycodes. Also, M-x mc/mark-more-like-this-extended RET
;; is helpful when quickly adding and skipping occurences.
(use-package multiple-cursors
  :bind
  ("M-p" . mc/mark-previous-like-this)
  ("M-n" . mc/mark-next-like-this)
  ("M-a" . mc/mark-all-like-this-dwim))

;; Go to the last change, using M-l.
(use-package goto-last-change
  :bind
  ("M-l" . goto-last-change))

;; Automatic indention on RET and yanks, and only one space on forward-kills.
;; (use-package auto-indent-mode
;;   ;; Load after delay.
;;   :defer 1

;;   :config
;;   (auto-indent-global-mode t)
;;   ;(add-to-list 'auto-indent-disabled-modes-list 'cider-mode)

;;   :custom
;;   ;; No indenting while moving, it's weird.
;;   (auto-indent-blank-lines-on-move nil))

;; Use git from within emacs.
(use-package magit
  :bind
  ("C-x g" . magit-status))

;; Have too long lines highlighted.
(use-package whitespace
  :hook
  (prog-mode . whitespace-mode)

  :custom
  (whitespace-line-column 120)
  (whitespace-style '(face lines-tail)))

;; Have window numbers for faster switching.
(use-package window-numbering
  :config
  (window-numbering-mode t))

;; Add a markdown mode
(use-package markdown-mode
  :defer t)

;; Add nyan cat.
(use-package nyan-mode
  :if
  window-system

  :config
  (nyan-mode 1)

  :custom
  (nyan-minimum-window-width 175))

;; Install yaml-mode
(use-package yaml-mode
  :defer t)

;; Display bound keys
(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

;; Higlight todo, fixme, etc
(use-package hl-todo
  ;; Load after delay
  :defer 2

  :config
  (global-hl-todo-mode))

;; Add restclient
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)))

;; Support all kinds of web templating, currently set to Django-like templates.
(use-package web-mode
  :mode ("\\.html\\'" "\\.html.erb\\'")

  :custom
  (web-mode-engines-alist '(("django" . "\\.html\\'")))
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t))

;; Have number navigation in eww.
(use-package eww-lnum
  :init (require 'eww)

  :bind (:map eww-mode-map ("f" . eww-lnum-follow)))

;; Have tmux-like zoom ability for frames.
(use-package zoom-window
  :bind (("C-x z" . zoom-window-zoom)))


;;;-----------------------------------------------------------------------------
;;; Tab and spaces handling.
;;;-----------------------------------------------------------------------------

;; Make Emacs ask about missing newline at end of file on save.
(setq require-final-newline 'ask)

;; Remove trailing spaces on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use no tabs by default. Modes that really need tabs should enable
;; indent-tabs-mode explicitly. And if indent-tabs-mode is off, untabify
;; before saving.
(setq-default indent-tabs-mode nil)

;; Set indentation for CSS.
(setq css-indent-offset 2)


;;;-----------------------------------------------------------------------------
;;; Term related settings.
;;;-----------------------------------------------------------------------------

;; Clear ansi-term buffer with C-c C-b (Clear Buffer).
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-c C-b")
              (lambda ()
                (interactive)
                (term-reset-terminal)))))

;; Have UTF-8 encoding in terminals.
(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

;; Set default shell binary.
(setq explicit-shell-file-name "/bin/zsh")

;; Kill buffer on process end.
(defun kill-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'kill-term-exec-hook)


;;;-----------------------------------------------------------------------------
;;; Other niceties.
;;;-----------------------------------------------------------------------------

;; Go straight to the *scratch* buffer, i.e. skip the help message.
;; And set a nice welcoming message.
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Happy hacking, Arnout!\n\n")
;(setq initial-major-mode 'clojure-mode)

;; Show column number next to linenumber in the status bar.
(column-number-mode t)

;; Wrap words instread of breaking them.
(toggle-word-wrap t)

;; Show matching paren.
(show-paren-mode t)

;; Save backups and autosaves in the system's temporary directory.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Switch back to previous buffer, with C-c b.
(defun switch-to-previous-buffer ()
  "Switch to the previously active buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) nil)))
(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)

;; Don't require two spaces between sentences when moving with M-e and M-a.
(setq sentence-end-double-space nil)

;; Have smoother scrolling with keyboard.
(setq scroll-margin 2
      scroll-conservatively 1000)

;; Have emacs reload buffers from the disk when changed. Currently, it does not
;; warn when the buffer is modified _and_ the the file on disk has been
;; modified. Also, watch out for lots of traffic when opening files via a
;; network.
(global-auto-revert-mode t)

;; Open the current file as root.
(defun current-as-root ()
  "Reopen current file as root."
  (interactive)
  (set-visited-file-name (concat "/sudo::" (buffer-file-name)))
  (setq buffer-read-only nil))

;; Have a key for loading the init file quickly.
(defun open-init-el ()
  "Open init.el quickly for editing."
  (interactive)
  (find-file-other-window "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c i") 'open-init-el)

;; Bind M-o to what normally requires C-x o.
(global-set-key (kbd "M-o") 'other-window)

;; Have better buffer names for equally named files.
(require 'uniquify)

;; Bind fill-paragraph to C-c M-q
(global-set-key (kbd "C-c M-q") 'fill-paragraph)

;; Ensure yanking in Emacs integrates with macOS clipboard
(unless window-system
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx))


;;;-----------------------------------------------------------------------------
;;; Emacs automagically managed settings. Clean up once in a while.
;;; -----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(package-selected-packages
   '(protobuf-mode eglot rspec-mode evil web-mode yaml-mode window-numbering which-key use-package undo-tree smex scala-mode sbt-mode restclient nyan-mode magit ido-vertical-mode ido-completing-read+ hl-todo goto-last-change git-gutter flx-ido expand-region exec-path-from-shell dracula-theme auto-indent-mode)))
