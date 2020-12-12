(require 'cl-lib)
(require 'dired)

(when (eq window-system 'ns)
  (setq-default mac-command-modifier 'control ; Make command act like control in Emacs
    mac-control-modifier 'super))             ; Make control act like command


(unless (display-graphic-p) (menu-bar-mode 0))

(when window-system
  (blink-cursor-mode 1) ; Enable the cursor blinking (should be default), nice to quickly find cursor.
  (scroll-bar-mode 0)   ; Disable the scroll bar
  (tool-bar-mode 0)     ; Disable the tool bar
  (tooltip-mode 0))     ; Disable the tooltips

(setq initial-frame-alist '((top    .  30)
                            (width  . 130)
                            (height .  55)))

;; Make the frame title show full path of buffer file
(setq frame-title-format
      `((buffer-file-name "%f" "%b")
        ,(format " - GNU Emacs %s" emacs-version)))

(progn
  (require 'server)
  (unless (server-running-p) (server-start)))

(setq-default tab-width             2
              fill-column           80
              require-final-newline t
              )

(setq inhibit-startup-screen             t
      make-backup-files                  nil
      auto-save-default                  nil
      custom-file                        (concat user-emacs-directory "custom.el")
      ring-bell-function                 'ignore
      confirm-kill-emacs                 'y-or-n-p
      confirm-nonexistent-file-or-buffer nil
      vc-suppress-confirm                t
      ;; vc-follow-symlinks              t ; macOS: t makes opening git vc files painfully *slow*.
      vc-follow-symlinks                 nil ; macOS: nill should make opening files faster.
      word-wrap                          t
      completions-format                 'vertical ; Easier reading  e.g., `C-x    C-f      TAB TAB`.
      undo-outer-limit                   250000000 ; Reduce warnings until 250     MB.
      large-file-warning-threshold       500000000 ; 500 MB.
      dired-auto-revert-buffer           t
      dired-listing-switches             "-alh"
      initial-frame-alist                '((top . 30) (width . 130) (height . 55))
      recentf-max-saved-items            15
      linum-format                       "%d "
      nlinum-format                      "%d "
      history-length                     1000
      ibuffer-always-show-last-buffer    t         ; Make point start on previous buffer.
      )

(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode t)
(column-number-mode t)
(global-auto-revert-mode t)
(delete-selection-mode t)
(global-visual-line-mode t)
(recentf-mode 1)
(savehist-mode t)
(when (featurep 'saveplace (save-place-mode t))) ; built-in only on newer Emacs.

;; Automatically disply line numbers in programming modes.
;; If Emacs version 26 or above, use the newer, built-in
;; display-line-numbers-mode. Otherwise, see if nlinum mode is installed, and as
;; last resort, use the old, built-in linum-mode.
(if (version<= emacs-version "26.0.50")
    (add-hook 'prog-mode-hook (lambda () (if (featurep 'nlinum) (nlinum-mode) (linum-mode))))
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; Auto-complete: Use built-in Hippie expansion for M-/
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-dabbrev-from-kill))
(global-set-key (kbd "M-/") 'hippie-expand)

;; Open new line with C-return/C-S-return like Sublime Text.
(progn
  (global-set-key (kbd "C-<return>")
      (lambda ()
        (interactive)
        (move-end-of-line 1)
        (newline)))
  (global-set-key (kbd "C-S-<return>")
      (lambda ()
        (interactive)
        (beginning-of-line)
        (electric-newline-and-maybe-indent)
        (previous-line))))

(defun me/ido-recentf-open ()
    "Use `ido-completing-read' to find a recent file."
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " (mapcar 'abbreviate-file-name recentf-list)))
        (message "Opening file...")
      (message "Aborting")))

;;;;;;;;;;;;;;;;;;
;; Key Bindings ;;
;;;;;;;;;;;;;;;;;;

(global-unset-key (kbd "C-q")) ; Reserve C-q prefix for personal bindings (replaces `quoted-insert`).
(global-set-key (kbd "C-q C-q") 'ibuffer)
(global-set-key (kbd "C-q C-e") 'execute-extended-command)
(global-set-key (kbd "C-q C-f") 'find-file)
(global-set-key (kbd "C-q C-t") (lambda () (interactive) (if (featurep 'multi-term) (multi-term-dedicated-toggle) (term "zsh"))))  
(global-set-key (kbd "C-q TAB") (lambda () (interactive) (if (featurep 'evil) (evil-buffer) (previous-buffer))))
(global-set-key (kbd "C-q C-s") 'save-buffer)
(global-set-key (kbd "C-q C-g") 'grep)
(global-set-key (kbd "C-q 1") 'delete-other-windows)
(global-set-key (kbd "C-q 0") 'delete-window)
(global-set-key (kbd "C-q 2") 'split-window-below)
(global-set-key (kbd "C-q 3") 'split-window-right)

(progn (global-unset-key (kbd "C-w")) (global-set-key (kbd "C-w") 'backward-word-kill)) ; Like in terminal.

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "M-<f4>") 'delete-frame) ; Alt-F4 closes frame.
