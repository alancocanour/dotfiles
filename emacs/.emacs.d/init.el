;; Load configuration for the local machine if it exists
(load (expand-file-name "local.el" user-emacs-directory) t)

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

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
  :load-path "lisp/"
  :demand
  :config
  ;;Indent
  (setq tab-always-indent 'complete)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)

  ;;Startup
  (setq inhibit-startup-screen t)
  (setq initial-major-mode 'fundamental-mode)
  (setq initial-scratch-message nil)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

  ;;C Source code variables
  (tool-bar-mode 0)
  (setq delete-by-moving-to-trash t)
  (setq hscroll-margin 0)
  (setq hscroll-step 0.3)
  (setq frame-resize-pixelwise t)

  ;;Enable off-by-default commands
  (put 'dired-find-alternate-file 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  ;;Settings that don't belong anywhere else
  (set-face-attribute 'default nil
		      :family "Fira Code"
		      :weight 'semibold)
  (pixel-scroll-precision-mode)
  (savehist-mode)
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (if (eq system-type 'windows-nt) (setq w32-get-true-file-attributes nil))
  (setq-default display-buffer-reuse-frames t)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'highlight-todo)
  (bind-keys* ("C-c |" . toggle-window-split)
	      ("C-c \\" . toggle-window-split)
	      ("C-c d" . toggle-current-window-dedication)
	      ("<C-backspace>" . kill-start-of-line)
	      ("M-`" . jump-to-mark)
	      ("C-`" . push-mark-no-activate)
	      ("C-M-q" . indent-whole-buffer)
	      ("<S-SPC>" . insert-underscore)
	      ("C-c C-s" . switch-to-scratch)
	      ("C-c C-n" . switch-to-notes)
	      ("C-S-l" . recenter-horizontal)))
(use-package autorevert
  :bind ("C-c A" . auto-revert-mode)
  :config
  (setq auto-revert-verbose nil) )
(use-package avoid
  :commands mouse-avoidance-mode
  :demand
  :config
  (mouse-avoidance-mode 'exile) )
(use-package avy
  :ensure t
  :demand t
  :bind (("C-c C-SPC" . avy-goto-word-or-subword-1)))
(use-package calc
  :bind ("C-c c" . calc) )
(use-package cc-mode
  :commands java-mode
  :init
  (add-hook 'java-mode-hook 'subword-mode))
(use-package compile
  :commands compile recompile
  :bind ("C-c r" . recompile)
  :config
  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (setq compilation-auto-jump-to-first-error nil)
  (setq compilation-scroll-output t) )
(use-package consult
  :ensure t
  :bind (("C-x C-b" . consult-buffer)
         ("C-S-y" . consult-yank-pop)
         ("C-s" . consult-line)
	 ("C-S-s" . consult-line-multi)
	 ("C-c C-i" . consult-imenu)) )
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (setq corfu-quit-at-boundary nil)
  (setq corfu-quit-no-match nil)
  (setq corfu-count 20)
  (keymap-set corfu-map "C-q" #'corfu-quick-insert))
(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  :config
  (corfu-popupinfo-mode))
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))
(use-package csharp-mode
  :mode ("\\.cs$" . csharp-mode) )
(use-package dired
  :commands dired
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-recursive-deletes 'always) )
(use-package dired-x
  :demand
  :config
  (require 'dired-x))
(use-package direnv
  :config
  (direnv-mode) )
(use-package drag-stuff
  :commands (drag-stuff-mode turn-off-drag-stuff-mode)
  :diminish drag-stuff-mode
  :bind (("<M-up>" . drag-stuff-up)
	 ("<M-down>" . drag-stuff-down)) )
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1) )
(use-package electric
  :commands electric-indent-mode electric-pair-mode
  :init
  (add-hook 'prog-mode-hook 'electric-indent-mode)
  (add-hook 'prog-mode-hook 'electric-pair-mode) )
(use-package emacs
  :config
  (load-theme 'modus-vivendi)
  ;; Run treesit-install-language-grammar before using treesitter modes
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
	  (c++-mode . c++-ts-mode)
	  (c-mode . c-ts-mode)
	  (c-or-c++-mode . c-or-c++-ts-mode)
	  (cmake-mode . cmake-ts-mode)
	  (csharp-mode . csharp-ts-mode)
	  (css-mode . css-ts-mode)
	  (dockerfile-mode . dockerfile-ts-mode)
	  (go-mod-mode . go-mod-ts-mode)
	  (go-mode . go-ts-mode)
	  (java-mode . java-ts-mode)
	  (js-mode . js-ts-mode)
	  (json-mode . json-ts-mode)
	  (python-mode . python-ts-mode)
	  (ruby-mode . ruby-ts-mode)
	  (rust-mode . rust-ts-mode)
	  (toml-mode . toml-ts-mode)
	  (tsx-mode . tsx-ts-mode)
	  (typescript-mode . typescript-ts-mode)
	  (yaml-mode . yaml-ts-mode)))
  )
(use-package embark
  :ensure t
  :demand t
  :bind (("C-c a" . embark-act))
  :init
  (setq embark-quit-after-action nil))
(use-package embark-consult
  :ensure t)
(use-package expand-region
  :bind ("C-c e" . er/expand-region) )
(use-package files
  :demand
  :config
  (setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/saves" user-emacs-directory))))
  (setq safe-local-variable-values '((encoding . utf-8))) )
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :diminish flyspell-mode
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode) )
(use-package graphviz-dot-mode
  :mode (("\.dot$" . graphviz-dot-mode)) )
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
(use-package lisp
  :bind ("C-S-d" . delete-pair) )
(use-package lua-mode
  :mode ("\\.lua$" . lua-mode) )
(use-package magit
  :bind ("C-c g" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (setq magit-diff-refine-hunk t)
  (setq magit-save-some-buffers nil) )
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))
(use-package markdown-mode
  :mode (("\\.md" . markdown-mode)) )
(use-package menu-bar
  :demand
  :config
  (menu-bar-mode 0)
  (menu-bar-showhide-fringe-ind-left) )
(use-package nix-mode
  :mode (("\.nix$" . nix-mode) ) )
(use-package num3-mode
  :commands (num3-mode global-num3-mode)
  :diminish num3-mode
  :defer 1
  :custom-face
  (num3-face-even ((t (:underline t :weight bold))))
  :config
  (global-num3-mode) )
(use-package nxml-mode
  :mode ("\.xml" . nxml-mode)
  :config
  (setq nxml-auto-insert-xml-declaration-flag nil)
  (setq nxml-sexp-element-flag t) )
(use-package occur-x
  :commands turn-on-occur-x-mode
  :init
  (add-hook 'occur-mode-hook 'turn-on-occur-x-mode)
  :config
  (setq occur-linenumbers-in-margin t) )
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))
(use-package org
  :commands (org-mode orgtbl-mode)
  :bind
  (("C-c l" . org-store-link))
  :config
  (setq org-archive-location (concat "%s_archive_" (format-time-string "%Y") "::"))
  (setq org-enforce-todo-dependencies t)
  (setq org-footnote-section nil)
  (setq org-highlight-sparse-tree-matches nil)
  (setq org-indirect-buffer-display 'current-window)
  (setq org-todo-keywords '((sequence "TODO" "|" "DONE")))
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t))) )
(use-package paren
  :demand
  :config
  (setq show-paren-mode 1) )
(use-package projectile
  :commands projectile-mode
  :demand
  :config
  (setq projectile-indexing-method 'alien)
  (setq projectile-switch-project-action 'magit-status)
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map) )
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
  :config
  (scroll-bar-mode 0) )
(use-package simple
  :bind
  (("<M-SPC>" . cycle-spacing)
   ("<C-S-backspace>" . kill-whole-line)
   ("C-c t" . toggle-truncate-lines))
  :config
  (setq column-number-mode t)
  (setq next-line-add-newlines t) )
(use-package sort
  :bind ("C-c s" . sort-lines) )
(use-package time
  :defer 1
  :config
  (setq display-time-day-and-date t)
  (setq display-time-default-load-average nil)
  (setq display-time-world-list '(("PST8PDT7" "Pacific") ("ARZ7" "Arizona") ("MTN7MDT6" "Mountain") ("EST5EDT4" "Eastern") ("GMT" "GMT") ("IST-5:30" "Bangalore") ("CST-8" "Beijing")))
  (display-time-mode) )
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil)
  (setq undo-in-region t)
  (setq undo-tree-visualizer-diff t) )
(use-package unfill
  :bind ("M-Q" . unfill-paragraph) )
(use-package uuidgen
  :commands uuidgen)
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (keymap-set vertico-map "C-q" #'vertico-quick-exit))
(use-package wgrep
  :defer 2)
(use-package window
  :bind
  (("C-S-z" . bury-buffer)
   ("C-}" . enlarge-window-horizontally)
   ("C-{" . shrink-window-horizontally)
   ("C-+" . enlarge-window)
   ("C-_" . shrink-window)) )
(use-package ws-butler
  :commands ws-butler-mode
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'xml-mode 'ws-butler-mode) )
(use-package yasnippet
  :commands yas-minor-mode
  :init
  (yas-global-mode) )
