(server-start)

;;Setup packages to automatically download
(setq
 my-packages '(
               ace-jump-mode
               adaptive-wrap
               auctex
               centered-cursor-mode
               company
               csharp-mode
               doc-mode
               drag-stuff
               enclose
               fic-mode
               groovy-mode
               haml-mode
               highlight-symbol
               htmlize
               javadoc-help
               magit
               multi-eshell
               num3-mode
               projectile
               rainbow-delimiters
               rainbow-mode
               rvm
               sass-mode
               wgrep
               win-switch
               ))
;;Enable Marmalade repo
(setq package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/")
                               ("melpa" . "http://melpa.milkbox.net/packages/"))))

;;Intsall missing packages
(require 'cl)
(require 'package)
(package-initialize)
(defun is-package-installed (p)
  (catch 'break
    (loop for ip in package-alist
          do(if (equal p (car ip)) (throw 'break t) ))
    (throw 'break nil)))
(defun install-missing-packages ()
  (interactive)
  (loop for p in my-packages
        do(unless (is-package-installed p) (package-install p))))
;;(install-missing-packages) ;;I'm disabling this to speedup startup. I only need to run install-missing-packages once when using a new emacs install anyway

;;Set up load path
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(if (eq system-type 'windows-nt) (setq w32-get-true-file-attributes nil))

(require 'dired-x);;Enable dired-do-find-marked-files and other fancy dired stuff
(autoload 'centered-cursor-mode "centered-cursor-mode")
(autoload 'adaptive-wrap-prefix-mode "adaptive-wrap")
(autoload 'javadoc-lookup       "javadoc-help" "Look up Java class in Javadoc."   t)
(autoload 'javadoc-help         "javadoc-help" "Open up the Javadoc-help menu."   t)
(autoload 'highlight-symbol-mode         "highlight-symbol" "Highlights the symbol under the cursor"   t)
(add-hook 'text-mode-hook 'adaptive-wrap-prefix-mode)
(add-hook 'text-mode-hook 'drag-stuff-mode)
(add-hook 'org-mode-hook 'turn-off-drag-stuff-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)
(add-hook 'prog-mode-hook 'adaptive-wrap-prefix-mode)
(add-hook 'prog-mode-hook 'drag-stuff-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(add-hook 'prog-mode-hook 'turn-on-fic-mode)
(add-hook 'c-mode-common-hook 'doc-mode)
(add-hook 'grep-mode-hook 'toggle-truncate-lines)
(require 'win-switch)
(semantic-mode 1)
(autoload 'company-mode "company")
(add-hook 'prog-mode-hook 'company-mode)
(electric-pair-mode)
(autoload 'vtl-mode "vtl")
(add-hook 'xml-mode 'company-mode)
(add-hook 'eshell-mode-hook 'company-mode)
(mouse-avoidance-mode 'exile)
(add-hook 'latex-mode-hook 'reftex-mode)
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("^[Rr]akefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.sass$" . sass-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;Show buffer boundaries in left fringe
(menu-bar-showhide-fringe-ind-left)

;;Always ask y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'thingatpt)
(defun describe-at-point ()
  "Uses semantic to describe the class currently under the point"
  (interactive)
  (semantic-ia-describe-class (symbol-name (symbol-at-point)))
  )

(defun indent-whole-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun kill-start-of-line ()
  "kill from point to start of line"
  (interactive)
  (kill-line 0))

(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

(defun insert-underscore ()
  "Insert an underscore at point"
  (interactive)
  (insert-char ?_ 1))

(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; Create a minor mode to hold all of my key bindings which will override bindings in major modes
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;;(define-key my-keys-minor-mode-map (kbd "C-i") 'some-function)
(define-key my-keys-minor-mode-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key my-keys-minor-mode-map (kbd "C-c C-SPC") 'ace-jump-mode)
(define-key my-keys-minor-mode-map (kbd "\C-xo") 'win-switch-dispatch)
(define-key my-keys-minor-mode-map (kbd "M-,") 'describe-at-point)
(define-key my-keys-minor-mode-map (kbd "M-.") 'semantic-ia-fast-jump)
(define-key my-keys-minor-mode-map (kbd "C-SPC") 'company-complete-common)
(define-key my-keys-minor-mode-map (kbd "C-x C-b") 'ibuffer)
(define-key my-keys-minor-mode-map (kbd "C-}") 'enlarge-window-horizontally)
(define-key my-keys-minor-mode-map (kbd "C-{") 'shrink-window-horizontally)
(define-key my-keys-minor-mode-map (kbd "C-+") 'enlarge-window)
(define-key my-keys-minor-mode-map (kbd "C-_") 'shrink-window)
(define-key my-keys-minor-mode-map (kbd "\C-x41") 'maximize-window)
(define-key my-keys-minor-mode-map (kbd "\C-x42") 'minimize-window)
(define-key my-keys-minor-mode-map (kbd "\C-ca") 'org-agenda)
(define-key my-keys-minor-mode-map (kbd "\C-cA") 'auto-revert-mode)
(define-key my-keys-minor-mode-map (kbd "\C-cg") 'magit-status)
(define-key my-keys-minor-mode-map (kbd "\C-cs") 'multi-eshell)
(define-key my-keys-minor-mode-map (kbd "\C-cc") 'calc)
(define-key my-keys-minor-mode-map (kbd "\C-ci") 'ispell)
(define-key my-keys-minor-mode-map (kbd "\C-cr") 'recompile)
(define-key my-keys-minor-mode-map (kbd "\C-c|") 'toggle-window-split)
(define-key my-keys-minor-mode-map (kbd "\C-c\\") 'toggle-window-split)
(define-key my-keys-minor-mode-map (kbd "\C-cd") 'toggle-current-window-dedication)
(define-key my-keys-minor-mode-map (kbd "\C-ct") 'toggle-truncate-lines)
(define-key my-keys-minor-mode-map (kbd "<C-tab>") 'multi-eshell-switch)
(define-key my-keys-minor-mode-map (kbd "<C-S-tab>") 'multi-eshell-go-back)
(define-key my-keys-minor-mode-map (kbd "\C-cj") 'javadoc-lookup)
(define-key my-keys-minor-mode-map (kbd "C-;") 'kill-start-of-line)
(define-key my-keys-minor-mode-map (kbd "M-`") 'jump-to-mark)
(define-key my-keys-minor-mode-map (kbd "C-`") 'push-mark-no-activate)
(define-key my-keys-minor-mode-map (kbd "C-M-q") 'indent-whole-buffer)
(define-key my-keys-minor-mode-map (kbd "M-Q") 'unfill-paragraph)
(define-key my-keys-minor-mode-map (kbd "<S-SPC>") 'insert-underscore)


(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t "" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "~/.saves"))))
 '(calendar-latitude 33.45)
 '(calendar-longitude -112.066667)
 '(ccm-step-delay 0.01)
 '(ccm-step-size 20)
 '(column-number-mode t)
 '(company-backends (quote (company-elisp company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs (company-gtags company-etags company-dabbrev-code company-keywords) company-oddmuse company-files company-dabbrev)))
 '(company-idle-delay nil)
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output (quote first-error))
 '(csharp-want-flymake-fixup nil)
 '(delete-by-moving-to-trash t)
 '(dired-recursive-deletes (quote always))
 '(display-time-day-and-date t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(display-time-world-list (quote (("PST8PDT7" "Pacific") ("ARZ7" "Arizona") ("MTN7MDT6" "Mountain") ("EST5EDT4" "Eastern") ("GMT" "GMT") ("IST-5:30" "Bangalore") ("CST-8" "Beijing"))))
 '(global-centered-cursor-mode t)
 '(global-company-mode t)
 '(global-linum-mode t)
 '(global-num3-mode nil)
 '(global-rainbow-delimiters-mode t)
 '(grep-highlight-matches (quote auto-detect))
 '(highlight-symbol-idle-delay 0.5)
 '(ibuffer-default-sorting-mode (quote filename/process))
 '(ido-auto-merge-work-directories-length -1)
 '(ido-create-new-buffer (quote always))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/todo.org")
 '(initial-scratch-message nil)
 '(ispell-program-name "aspell")
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-diff-refine-hunk t)
 '(magit-save-some-buffers nil)
 '(menu-bar-mode nil)
 '(multi-eshell-name "*eshell*")
 '(multi-eshell-shell-function (quote (eshell)))
 '(next-line-add-newlines t)
 '(nxml-auto-insert-xml-declaration-flag nil)
 '(nxml-sexp-element-flag t)
 '(org-agenda-files "~/.agenda_files")
 '(org-agenda-restore-windows-after-quit t)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (ditaa . t) (sh . t))))
 '(org-ditaa-jar-path "~/bin/ditaa0_9.jar")
 '(org-enforce-todo-dependencies t)
 '(org-export-exclude-tags (quote ("noexport" "bsd")))
 '(org-export-latex-default-packages-alist (quote (("AUTO" "inputenc" t) ("T1" "fontenc" t) ("" "fixltx2e" nil) ("" "graphicx" t) ("" "longtable" nil) ("" "float" nil) ("" "wrapfig" nil) ("" "soul" t) ("" "textcomp" t) ("" "marvosym" t) ("" "wasysym" t) ("" "latexsym" t) ("" "amssymb" t) ("" "hyperref" nil) ("" "fancyhdr" nil) "\\tolerance=1000")))
 '(org-footnote-section nil)
 '(org-hide-leading-stars t)
 '(org-highlight-sparse-tree-matches nil)
 '(org-indirect-buffer-display (quote current-window))
 '(org-log-done (quote time))
 '(org-log-into-drawer t)
 '(org-stuck-projects (quote ("+LEVEL=2/-DONE-CANCELED" ("TODO" "NEXT" "NEXTACTION") nil "")))
 '(org-todo-keywords (quote ((sequence "BLOCKED" "TODO" "|" "DONE" "CANCELED"))))
 '(projectile-global-mode t)
 '(rng-schema-locating-files (quote ("~/.emacs.d/schemas/schemas.xml" "schemas.xml")))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(semantic-idle-scheduler-idle-time 1)
 '(send-mail-function (quote smtpmail-send-it))
 '(setq inhibit-startup-message t)
 '(show-paren-mode 1)
 '(smtpmail-default-smtp-server "relay.lumension.com")
 '(smtpmail-smtp-server "relay.lumension.com")
 '(smtpmail-smtp-service 25)
 '(sort-fold-case t t)
 '(tab-always-indent (quote complete))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t)
 '(warning-suppress-types (quote ((\(undo\ discard-info\)))) nil nil "Disable warning for really big undos")
 '(win-switch-down-keys (quote ("s")))
 '(win-switch-enlarge-horizontally-keys (quote ("D")))
 '(win-switch-enlarge-vertically-keys (quote ("W")))
 '(win-switch-exit-keys (quote ("u" [return] "q")))
 '(win-switch-left-keys (quote ("a")))
 '(win-switch-right-keys (quote ("d")))
 '(win-switch-shrink-horizontally-keys (quote ("A")))
 '(win-switch-shrink-vertically-keys (quote ("S")))
 '(win-switch-up-keys (quote ("w"))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#000000" :foreground "#FFFFFF"))))
 '(num3-face-even ((t (:underline t :weight bold))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "hot pink"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "lime green"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "royal blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "aquamarine1"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "DarkOliveGreen2"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "dark violet"))))
 '(rainbow-delimiters-unmatched-face ((t nil))))
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
