;; Load configuration for the local machine if it exists
(load "~/.emacs.d/local.el" t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

(setq use-package-always-ensure t)

;; Set up auto-compile before other packages so they will all be auto-compiled
(use-package auto-compile
  :ensure t
  :demand
  :config
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode))

(use-package ace-jump-mode
  :bind ("C-c C-SPC" . ace-jump-mode) )
(use-package ace-window
  :bind (("C-x o" . ace-window)
	 ("M-o" . ace-window)) )
(use-package adaptive-wrap
  :commands adaptive-wrap-prefix-mode
  :init
  (add-hook 'text-mode-hook 'adaptive-wrap-prefix-mode)
  (add-hook 'prog-mode-hook 'adaptive-wrap-prefix-mode) )
(use-package ag
  :bind ("C-c a" . ag-project)
  :init
  (add-hook 'ag-mode-hook 'toggle-truncate-lines) )
(use-package alan
  :ensure nil
  :load-path "lisp/"
  :demand
  :bind
  (("C-c |" . toggle-window-split)
   ("C-c \\" . toggle-window-split)
   ("C-c d" . toggle-current-window-dedication)
   ("<C-backspace>" . kill-start-of-line)
   ("M-`" . jump-to-mark)
   ("C-`" . push-mark-no-activate)
   ("C-M-q" . indent-whole-buffer)
   ("<S-SPC>" . insert-underscore)
   ("C-c C-s" . switch-to-scratch)
   ("C-S-l" . recenter-horizontal))
  :config
  ;;Indent
  (setq tab-always-indent 'complete)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)

  ;;Startup
  (setq inhibit-startup-screen t)
  (setq initial-major-mode 'fundamental-mode)
  (setq initial-scratch-message nil)
  (setq custom-file "~/.emacs.d/custom.el")

  ;;C Source code variables
  (tool-bar-mode 0)
  (setq delete-by-moving-to-trash t)
  (setq hscroll-margin 0)
  (setq hscroll-step 1)

  ;;Enable off-by-default commands
  (put 'dired-find-alternate-file 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;;Settings that don't belong anywhere else
  (set-face-attribute 'default nil
		      :background "#000000"
		      :foreground "#FFFFFF")
  (defalias 'yes-or-no-p 'y-or-n-p)
  (if (eq system-type 'windows-nt) (setq w32-get-true-file-attributes nil))
  (setq-default display-buffer-reuse-frames t) )
(use-package autorevert
  :bind ("C-c A" . auto-revert-mode)
  :config
  (setq auto-revert-verbose nil) )
(use-package avoid
  :commands mouse-avoidance-mode
  :demand
  :config
  (mouse-avoidance-mode 'exile) )
(use-package calc
  :bind ("C-c c" . calc) )
(use-package compile
  :commands compile recompile
  :bind ("C-c r" . recompile)
  :config
  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (setq compilation-auto-jump-to-first-error t)
  (setq compilation-scroll-output 'first-error) )
(use-package csharp-mode
  :mode ("\\.cs$" . csharp-mode) )
(use-package dired
  :commands dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-recursive-deletes 'always) )
(use-package dired-x
  :demand
  :ensure nil
  :config
  (require 'dired-x))
(use-package drag-stuff
  :commands (drag-stuff-mode turn-off-drag-stuff-mode)
  :diminish drag-stuff-mode
  :init
  (add-hook 'org-mode-hook 'turn-off-drag-stuff-mode)
  (add-hook 'prog-mode-hook 'drag-stuff-mode)
  (add-hook 'text-mode-hook 'drag-stuff-mode) )
(use-package electric
  :commands electric-indent-mode electric-pair-mode
  :init
  (add-hook 'prog-mode-hook 'electric-indent-mode)
  (add-hook 'prog-mode-hook 'electric-pair-mode) )
(use-package expand-region
  :bind ("C-c e" . er/expand-region) )
(use-package files
  :demand
  :ensure nil
  :config
  (setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
  (setq safe-local-variable-values '((encoding . utf-8))) )
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :diminish flyspell-mode
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode) )
(use-package grep
  :commands grep grep-find
  :init
  (add-hook 'grep-mode-hook 'toggle-truncate-lines)
  :config
  (setq grep-highlight-matches 'auto-detect) )
(use-package groovy-mode
  :mode (("\.groovy$" . groovy-mode)
         ("\.gradle$" . groovy-mode))
  :interpreter ("groovy" . groovy-mode) )
(use-package haskell-mode
  :mode (("\.hs$" . haskell-mode)
         ("\.lhs" . haskell-mode))
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc) )
(use-package helm
  :ensure t
  :commands helm-mode
  :init (helm-mode)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-c C-i" . helm-imenu)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-c C-h" . helm-resume)) )
(use-package highlight-symbol
  :commands highlight-symbol-mode
  :diminish highlight-symbol-mode
  :init
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  :config (setq highlight-symbol-idle-delay 0.5) )
(use-package hilit-chg
  :bind
  (("C-c h" . highlight-changes-visible-mode)
   ("C-c H" . highlight-changes-rotate-faces)
   ("C-M-n" . highlight-changes-next-change)
   ("C-M-p" . highlight-changes-previous-change))
  :demand
  :config
  (make-empty-face 'highlight-changes-saved-face)
  (setq highlight-changes-face-list '(highlight-changes-saved-face))
  (add-hook 'write-file-hooks 'highlight-changes-rotate-faces)
  (set-face-attribute 'highlight-changes nil
		      :foreground 'unspecified
		      :background "#2f4f2f")
  (set-face-attribute 'highlight-changes-delete nil
		      :foreground 'unspecified
		      :background "#4f2f2f"
		      :underline nil)
  (global-highlight-changes-mode t) )
(use-package htmlize
  :commands (htmlize-buffer htmlize-file htmlize-many-files htmlize-many-files-dired htmlize-region) )
(use-package ispell
  :bind ("C-c i" . ispell)
  :config
  (setq ispell-program-name "aspell") )
(use-package linum
  :demand
  :config
  (global-linum-mode) )
(use-package lisp
  :bind ("C-S-d" . delete-pair)
  :ensure nil )
(use-package magit
  :bind ("C-c g" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (setq magit-diff-refine-hunk t)
  (setq magit-save-some-buffers nil) )
(use-package menu-bar
  :demand
  :ensure nil
  :config
  (menu-bar-mode 0)
  (menu-bar-showhide-fringe-ind-left) )
(use-package num3-mode
  :commands (num3-mode global-num3-mode)
  :diminish num3-mode
  :defer 1
  :config
  (global-num3-mode)
  (set-face-attribute 'num3-face-even nil
  		      :background "unspecified"
		      :foreground "unspecified"
                      :underline t
                      :weight 'bold) )
(use-package nxml-mode
  :mode ("\.xml" . nxml-mode)
  :ensure nil
  :config
  (setq nxml-auto-insert-xml-declaration-flag nil)
  (setq nxml-sexp-element-flag t) )
(use-package occur-x
  :commands turn-on-occur-x-mode
  :init
  (add-hook 'occur-mode-hook 'turn-on-occur-x-mode)
  :config
  (setq occur-linenumbers-in-margin t) )
(use-package org
  :commands (org-mode orgtbl-mode)
  :config
  (setq org-enforce-todo-dependencies t)
  (setq org-footnote-section nil)
  (setq org-hide-leading-stars t)
  (setq org-highlight-sparse-tree-matches nil)
  (setq org-indirect-buffer-display 'current-window)
  (setq org-startup-indented 'globally)
  (setq org-todo-keywords '((sequence "TODO" "|" "DONE"))) )
(use-package paren
  :demand
  :config
  (setq show-paren-mode 1) )
(use-package projectile
  :commands projectile-global-mode
  :demand
  :config
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'helm)
  (projectile-global-mode) )
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :defer 1
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'text-mode-hook 'rainbow-delimiters-mode)
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "hot pink"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "lime green"))))
  (rainbow-delimiters-depth-4-face ((t (:foreground "royal blue"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "red"))))
  (rainbow-delimiters-depth-6-face ((t (:foreground "aquamarine1"))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "DarkOliveGreen2"))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "dark violet"))))
  (rainbow-delimiters-unmatched-face ((t (:background "magenta" :foreground "#88090B")))) )
(use-package ruby-mode
  :mode (("\.gemspec$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("^[Rr]akefile$" . ruby-mode)
         ("\.rake$" . ruby-mode)) )
(use-package scroll-bar
  :demand
  :ensure nil
  :config
  (scroll-bar-mode 0) )
(use-package simple
  :bind
  (("C-c SPC" . just-one-space)
   ("<C-S-backspace>" . kill-whole-line)
   ("C-c t" . toggle-truncate-lines))
  :ensure nil
  :config
  (setq column-number-mode t)
  (setq next-line-add-newlines t) )
(use-package smartscan
  :commands global-smartscan-mode
  :defer 1
  :config
  (global-smartscan-mode)
  (setq smartscan-symbol-selector "symbol") )
(use-package solar
  :commands calendar-sunrise-sunset
  :ensure nil
  :config
  (setq calendar-latitude 33.45)
  (setq calendar-longitude -112.066667) )
(use-package sort
  :bind ("C-c s" . sort-lines) )
(use-package time
  :defer 1
  :config
  (setq display-time-day-and-date t)
  (setq display-time-default-load-average nil)
  (setq display-time-world-list '(("PST8PDT7" "Pacific") ("ARZ7" "Arizona") ("MTN7MDT6" "Mountain") ("EST5EDT4" "Eastern") ("GMT" "GMT") ("IST-5:30" "Bangalore") ("CST-8" "Beijing")))
  (display-time-mode) )
(use-package unfill
  :bind ("M-Q" . unfill-paragraph) )
(use-package uuidgen
  :commands uuidgen)
(use-package vlf
  :defer 1
  :config (require 'vlf-setup) )
(use-package wgrep
  :defer 2)
(use-package wgrep-ag
  :defer 2)
(use-package window
  :bind
  (("C-S-z" . bury-buffer)
   ("C-}" . enlarge-window-horizontally)
   ("C-{" . shrink-window-horizontally)
   ("C-+" . enlarge-window)
   ("C-_" . shrink-window))
  :ensure nil )
(use-package ws-butler
  :commands ws-butler-mode
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'xml-mode 'ws-butler-mode) )
(use-package yasnippet
  :commands yas-minor-mode
  :init
  (use-package java-snippets)
  (use-package haskell-snippets)
  (yas-global-mode) )
