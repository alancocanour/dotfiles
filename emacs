(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package ace-jump-mode
  :bind ("C-c C-SPC" . ace-jump-mode) )
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
  :bind
  (("C-c |" . toggle-window-split)
   ("C-c \\" . toggle-window-split)
   ("C-c d" . toggle-current-window-dedication)
   ("<C-backspace>" . kill-start-of-line)
   ("M-`" . jump-to-mark)
   ("C-`" . push-mark-no-activate)
   ("C-M-q" . indent-whole-buffer)
   ("<S-SPC>" . insert-underscore)
   ("C-c C-s" . switch-to-scratch)) )
(use-package auto-revert-mode
  :bind ("C-c A" . auto-revert-mode) )
(use-package avoid
  :commands mouse-avoidance-mode
  :demand
  :config
  (mouse-avoidance-mode 'exile) )
(use-package calc
  :bind ("C-c c" . calc) )
(use-package cap-words
  :commands capitalized-words-mode
  :init
  (add-hook 'prog-mode-hook 'capitalized-words-mode) )
(use-package compile
  :bind ("C-c r" . recompile) )
(use-package csharp-mode
  :mode ("\\.cs$" . csharp-mode) )
(use-package dired-x
  :demand
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
(use-package fic-mode
  :commands fic-mode
  :diminish fic-mode
  :init
  (add-hook 'prog-mode-hook 'fic-mode) )
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :diminish flyspell-mode
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode) )
(use-package grep
  :commands grep grep-find
  :init
  (add-hook 'grep-mode-hook 'toggle-truncate-lines) )
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
   ("C-c H" . highlight-changes-rotate-faces))
  :demand
  :config
  (make-empty-face 'highlight-changes-saved-face)
  (setq highlight-changes-face-list '(highlight-changes-saved-face))
  (add-hook 'write-file-hooks 'highlight-changes-rotate-faces)
  (set-face-foreground 'highlight-changes nil)
  (set-face-background 'highlight-changes "#2f4f2f")
  (set-face-foreground 'highlight-changes-delete nil)
  (set-face-background 'highlight-changes-delete "#4f2f2f")
  (set-face-underline 'highlight-changes-delete nil)
  (global-highlight-changes-mode t) )
(use-package htmlize
  :commands (htmlize-buffer htmlize-file htmlize-many-files htmlize-many-files-dired htmlize-region) )
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer) )
(use-package ispell
  :bind ("C-c i" . ispell) )
(use-package lisp
  :bind ("C-S-d" . delete-pair) )
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
  :config
  (menu-bar-showhide-fringe-ind-left) )
(use-package multi-eshell
  :bind
  (("<C-tab>" . multi-eshell-switch)
   ("<C-S-tab>" . multi-eshell-go-back))
  :config
  (setq multi-eshell-name "*eshell*")
  (setq multi-eshell-shell-function '(eshell)) )
(use-package num3-mode
  :commands (num3-mode global-num3-mode)
  :diminish num3-mode
  :defer 1
  :config
  (global-num3-mode)
  (set-face-attribute 'num3-face-even nil
                      :underline t
                      :weight 'bold) )
(use-package occur-x
  :commands turn-on-occur-x-mode
  :init
  (add-hook 'occur-mode-hook 'turn-on-occur-x-mode) )
(use-package projectile
  :commands projectile-global-mode
  :demand
  :config
  (setq projectile-indexing-method 'alien)
  (projectile-global-mode) )
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :defer 1
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'text-mode-hook 'rainbow-delimiters-mode) )
(use-package ruby-mode
  :mode (("\.gemspec$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("^[Rr]akefile$" . ruby-mode)
         ("\.rake$" . ruby-mode)) )
(use-package server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t) )
(use-package simple
  :bind
  (("C-c SPC" . just-one-space)
   ("<C-S-backspace>" . kill-whole-line)
   ("C-c t" . toggle-truncate-lines)) )
(use-package smartscan
  :commands global-smartscan-mode
  :defer 1
  :config
  (global-smartscan-mode)
  (setq smartscan-symbol-selector "symbol") )
(use-package sort
  :bind ("C-c s" . sort-lines) )
(use-package unfill
  :bind ("M-Q" . unfill-paragraph) )
(use-package uuidgen
  :commands uuidgen)
(use-package vlf
  :defer 1
  :config (require 'vlf-setup) )
(use-package wgrep
  :defer 2)
(use-package wgrep-agp
  :defer 2)
(use-package win-switch
  :commands win-switch-dispatch
  :bind ("C-x o" . win-switch-dispatch)
  :config
  (setq win-switch-up-keys (quote ("w")))
  (setq win-switch-left-keys (quote ("a")))
  (setq win-switch-down-keys (quote ("s")))
  (setq win-switch-right-keys (quote ("d")))
  (setq win-switch-enlarge-vertically-keys (quote ("W")))
  (setq win-switch-shrink-horizontally-keys (quote ("A")))
  (setq win-switch-shrink-vertically-keys (quote ("S")))
  (setq win-switch-enlarge-horizontally-keys (quote ("D")))
  (setq win-switch-exit-keys (quote ("u" [return] "q"))) )
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

(if (eq system-type 'windows-nt) (setq w32-get-true-file-attributes nil))

;;Always ask y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;;Keep the compilation buffer from opening in every frame
(setq-default display-buffer-reuse-frames t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-verbose nil)
 '(backup-directory-alist (quote (("." . "~/.saves"))))
 '(calendar-latitude 33.45)
 '(calendar-longitude -112.066667)
 '(ccm-step-delay 0.01)
 '(ccm-step-size 20)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output (quote first-error))
 '(delete-by-moving-to-trash t)
 '(dired-listing-switches "-alh")
 '(dired-recursive-deletes (quote always))
 '(display-time-day-and-date t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(display-time-world-list (quote (("PST8PDT7" "Pacific") ("ARZ7" "Arizona") ("MTN7MDT6" "Mountain") ("EST5EDT4" "Eastern") ("GMT" "GMT") ("IST-5:30" "Bangalore") ("CST-8" "Beijing"))))
 '(global-linum-mode t)
 '(grep-highlight-matches (quote auto-detect))
 '(ibuffer-default-sorting-mode (quote filename/process))
 '(ido-auto-merge-work-directories-length -1)
 '(ido-create-new-buffer (quote always))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/todo.org")
 '(initial-major-mode (quote fundamental-mode))
 '(initial-scratch-message nil)
 '(ispell-program-name "aspell")
 '(menu-bar-mode nil)
 '(next-line-add-newlines t)
 '(nxml-auto-insert-xml-declaration-flag nil)
 '(nxml-sexp-element-flag t)
 '(occur-linenumbers-in-margin t)
 '(org-agenda-files "~/.agenda_files")
 '(org-agenda-restore-windows-after-quit t)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (ditaa . t) (sh . t))))
 '(org-ditaa-jar-path "~/bin/ditaa0_9.jar")
 '(org-enforce-todo-dependencies t)
 '(org-export-exclude-tags (quote ("noexport" "bsd")))
 '(org-footnote-section nil)
 '(org-hide-leading-stars t)
 '(org-highlight-sparse-tree-matches nil)
 '(org-indirect-buffer-display (quote current-window))
 '(org-log-done (quote time))
 '(org-log-into-drawer t)
 '(org-stuck-projects (quote ("+LEVEL=2/-DONE-CANCELED" ("TODO" "NEXT" "NEXTACTION") nil "")))
 '(org-todo-keywords (quote ((sequence "BLOCKED" "TODO" "|" "DONE" "CANCELED"))))
 '(rng-schema-locating-files (quote ("~/.emacs.d/schemas/schemas.xml" "schemas.xml")))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(scroll-bar-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(setq inhibit-startup-message t)
 '(show-paren-mode 1)
 '(sort-fold-case t t)
 '(tab-always-indent (quote complete))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t)
 '(warning-suppress-types (quote ((\(undo\ discard-info\)))) nil nil "Disable warning for really big undos"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#000000" :foreground "#FFFFFF"))))
 (put 'dired-find-alternate-file 'disabled nil)
 (put 'narrow-to-region 'disabled nil)
 (put 'upcase-region 'disabled nil)
 (put 'downcase-region 'disabled nil)
 )
