(provide 'alan)

(defun indent-whole-buffer ()
  (interactive)
  (if (editorconfig-core-get-properties)
      (progn (message "Formatting with editorconfig")
             (editorconfig-format-buffer))
      (progn (message "Formatting without editorconfig")
             (delete-trailing-whitespace)
             (indent-region (point-min) (point-max) nil))))

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

(defun switch-to-scratch ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "*scratch*")))

(defvar notes-filename "~/notes.org")

(defun switch-to-notes ()
  (interactive)
  (switch-to-buffer (or (find-buffer-visiting notes-filename)
			(find-file notes-filename))))

(defvar recenter-horizontal-last-op nil)

(defvar recenter-horizontal-positions '(middle left right))

(defun recenter-horizontal ()
  ""
  (interactive)
  (setq recenter-horizontal-last-op
	(if (eq this-command last-command)
	    (car (or (cdr (member recenter-horizontal-last-op recenter-horizontal-positions))
		     recenter-horizontal-positions))
	  (car recenter-horizontal-positions)))
  (let* ((cur (current-column))
	 (width (window-text-width))
	 (half-width (/ width 2)))
    (cond ((eq recenter-horizontal-last-op 'middle)
	   (set-window-hscroll (selected-window) (- cur half-width)))
	  ((eq recenter-horizontal-last-op 'left)
	   (set-window-hscroll (selected-window) cur))
	  ((eq  recenter-horizontal-last-op 'right)
	   (set-window-hscroll (selected-window) (- cur width)))
	  )
    (redraw-frame)))

(defun highlight-todo ()
    (highlight-phrase "TODO"))

(defun list-packages-melpa ()
  (interactive)
  (if (not (alist-get "melpa" package-archives nil))
      (progn
	(require 'package)
	(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
	(package-initialize)))
  (package-list-packages))
