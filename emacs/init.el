;;;
;;; Package Manager Setup
;;;

;; HACK
(setq evil-toggle-key "C-`") ; I want C-z to do the Emacs default i.e. suspend-emacs


;;; Added by Package.el.  This must come before configurations of
;;; installed packages.  Don't delete this line.  If you don't want it,
;;; just comment it out by adding a semicolon to the start of the line.
;;; You may delete these explanatory comments.
;; (require 'package)
(package-initialize)

;;; Emacs26: fix problem with GUI frames not loading fully with WSL & VcxSrv
;;;   Note: Emacs25 does not have this problem.
;; (modify-frame-parameters nil '((inhibit-double-buffering . t)))
;; (add-hook 'before-make-frame-functions (lambda () (interactive) (modify-frame-parameters nil '((inhibit-double-buffering . t)))))

;;;
;;; PERSONAL FUNCTIONS AND VARIABLES
;;;

(load-file (concat user-emacs-directory "my-functions-and-variables.el"))

;;;
;;; MY SENSIBLE DEFAULTS (include package initialization)
;;;

;; (load-file (concat user-emacs-directory "my-sensible-defaults.el")) ;; not contain package management initialization

(load-file (concat user-emacs-directory "vanilla-sensible-defaults.el")) ;; not contain package management initialization
(load-file (concat user-emacs-directory "third-party-package-sensible-defaults.el")) ;; not contain package management initialization

(require 'dash)
(require 'dash-functional)

;;;
;;; NATIVE MICROSOFT WINDOWS EMACS
;;;

(when (eq system-type 'windows-nt) (load-file (concat user-emacs-directory "windows-nt-config.el")))

;;;
;;; MACOS EMACS
;;;

(when (eq system-type 'darwin) (load-file (concat user-emacs-directory "macos-config.el")))

;;;
;;; WSL EMACS
;;;

(when
    (and (eq system-type 'gnu/linux)
         (string-match "Microsoft" (shell-command-to-string "uname -r")))
  (load-file (concat user-emacs-directory "wsl-config.el")))

;;;
;;; AMAZON-SPECIFIC ELISP
;;;
;;; The amazon-specific.el will be git-ignored by my Emacs repository;
;;; instead it should be kept in a seperate repository and symlinked
;;; to ~/.emacs.d/amazon-specific.el

(let ((file (concat user-emacs-directory "amazon-specific.el"))) (when (file-exists-p file) (load-file file)))

;;;
;;; use-package declarations
;;;

;; (use-package evil
;;   :init
;;   (evil-mode 1)
;;   ;; enable line text object "l", e.g., "vil" and "val" etc.
;;   (load-file (concat user-emacs-directory "lisp/evil-textobj-line.el")))

(use-package evil :config (evil-mode 1))

;; (use-package beacon
;;   :config
;;   (beacon-mode t)
;;   (setq beacon-color "green")
;;   (setq beacon-blink-duration 0.1)
;;   (setq beacon-blink-when-focused t)
;;   )

(use-package helm-swoop
  :after (helm)
  :config
  ;; Locality helps to quickly see the search box and not lose context of what I'm doing
  (setq helm-swoop-split-with-multiple-windows t)
  :bind (:map
         evil-normal-state-map
         ("SPC /" . helm-swoop-without-pre-input)))

(use-package org
  :mode ("\\.org\\.txt\\'" . org-mode)
  :config
  (setq org-todo-keywords '((sequence "TODO" "NEXT" "WAITING"
                                      "|"
                                      "DONE" "CANCELLED" "DELEGATED")))
  (setq org-ellipsis "....") ; alternative: "⤵"
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-log-done 'time)
  (setq org-list-indent-offset 2)
  (setq org-adapt-indentation nil) ; can be annoying
  (setq org-edit-src-content-indentation 4) ; default is 2, but i like some markdown conventions
  (org-babel-do-load-languages 'org-babel-load-languages
                               '(
                                 (emacs-lisp . t)
                                 ;; (sh         . t) ; bourne again shell
                                 (shell      . t) ; whatever $SHELL is
                                 (sql        . t)
                                 (ruby       . t)
                                 (perl       . t)
                                 (clojure    . t)
                                 (python     . t)
                                 ;;; Julia is a WIP (not yet in gnu,
                                 ;;; melpa, or marmalade repos, and can't
                                 ;;; get https://github.com/gjkerns/ob-julia
                                 ;;; to work, it's very old.)
                                 ;; (julia      . t)
                                 ))
  ;;; Taking this out for now b/c of Org 9.2 change:
  ;; (setq org-structure-template-alist
  ;;       (quote
  ;;        ;; Use lowercase templates
  ;;        (("s" "#+begin_src ?\n\n#+end_src")
  ;;         ("e" "#+begin_example\n?\n#+end_example")
  ;;         ("q" "#+begin_quote\n?\n#+end_quote")
  ;;         ("v" "#+begin_verse\n?\n#+end_verse")
  ;;         ("V" "#+begin_verbatim\n?\n#+end_verbatim")
  ;;         ("c" "#+begin_center\n?\n#+end_center")
  ;;         ("C" "#+begin_comment\n?\n#+end_comment")
  ;;         ("l" "#+begin_export latex\n?\n#+end_export")
  ;;         ("L" "#+latex: ")
  ;;         ("h" "#+begin_export html\n?\n#+end_export")
  ;;         ("H" "#+html: ")
  ;;         ("a" "#+begin_export ascii\n?\n#+end_export")
  ;;         ("A" "#+ascii: ")
  ;;         ("i" "#+index: ?")
  ;;         ("I" "#+include: %file ?"))))
  (setq org-directory "~/Org")
  (add-to-list 'org-agenda-files org-directory)
  (setq org-capture-templates
        '(
          ;; :PROPERTIES:
          ;; :CREATED:  [2020-01-28 Tue 15:44]
          ;; :END:
          ("a" "Amazon Note"   plain (file "amazon.org")   "* %u %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")
          ("t" "Tech Note"     plain (file "tech.org")     "* %u %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")
          ("p" "Personal Note" plain (file "personal.org") "* %u %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")
          ))
  (use-package htmlize) ; allow export to html and open in browser via C-c C-e h o
  (defun me/org-set-headings-to-same-font-size ()
    "Stop the org-level headers from increasing in height relative to the other text."
    (dolist (face '(org-level-1
                    org-level-2
                    org-level-3
                    org-level-4
                    org-level-5))
      (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
  (add-hook 'org-mode-hook 'me/org-set-headings-to-same-font-size)
  (add-hook 'org-mode-hook 'flyspell-mode)
  :bind (("<f7>"  . 'org-capture)))

(use-package helm-org-rifle
  :after (helm)
  :config
  (setq helm-org-rifle-show-path t)
  ;; Make indirect buffer the default
  (setq helm-org-rifle-actions
        '(("Show entry in indirect buffer" . helm-org-rifle-show-entry-in-indirect-buffer)
          ("Show entry" . helm-org-rifle--show-candidates)
          ("Show entry in real buffer" . helm-org-rifle-show-entry-in-real-buffer)
          ("Clock in" . helm-org-rifle--clock-in)
          ("Refile" . helm-org-rifle--refile)))
  (setq org-indirect-buffer-display 'current-window)
  :bind (("<f6>" . helm-org-rifle-org-directory)))

;; (use-package ranger)

;; (general-define-key :maps evil-normal-state-map
;;                     :states 'normal
;;                     "SPC SPC" 'helm-mini
;;                     "SPC ;" 'helm-M-x
;;                     "SPC :" 'eshell
;;                     "SPC '" 'multi-term-dedicated-toggle
;;                     )

;;; C-c prefix single-letter keybindings (replace C-q bindings).
;;; Standard Emacs convention bindings that will work in all states.
;; (progn
;;   ;; finding files
;;   (global-set-key (kbd "C-c e v") 'me/find-file-init-el)
;;   ;; buffers
;;   (global-set-key (kbd "C-c b b") 'ivy-switch-buffer)
;;   (global-set-key (kbd "C-c b d") 'kill-this-buffer)
;;   (global-set-key (kbd "C-c d d") 'kill-this-buffer)
;;   (global-set-key (kbd "C-c b r") 'revert-buffer)
;;   (global-set-key (kbd "C-c b H") 'helm-mini)
;;   (global-set-key (kbd "C-c TAB") 'evil-buffer)
;;   (global-set-key (kbd "C-c b n") 'next-buffer)
;;   (global-set-key (kbd "C-c b p") 'previous-buffer)
;;   (global-set-key (kbd "C-c f s") 'save-buffer)
;;   ;; window movement/switching
;;   (global-set-key (kbd "C-c j") 'evil-window-down)
;;   (global-set-key (kbd "C-c k") 'evil-window-up)
;;   (global-set-key (kbd "C-c h") 'evil-window-left)
;;   (global-set-key (kbd "C-c l") 'evil-window-right)
;;   (global-set-key (kbd "C-c w w") 'evil-window-next)
;;   ;; window creation
;;   (global-set-key (kbd "C-c w -") 'me/split-window-below-and-switch)
;;   (global-set-key (kbd "C-c w _") 'split-window-below)
;;   (global-set-key (kbd "C-c w /") 'me/split-window-right-and-switch)
;;   (global-set-key (kbd "C-c w ?") 'split-window-right)
;;   ;; window delete
;;   (global-set-key (kbd "C-c w d") 'delete-window)
;;   (global-set-key (kbd "C-c w o") 'delete-other-windows)
;;   (global-set-key (kbd "C-c o")   'delete-other-windows)
;;   (global-set-key (kbd "C-c w z") 'delete-other-windows) ; ala tmux
;;   ;; window state
;;   (global-set-key (kbd "C-c H") 'winner-undo) ; think: Go Back
;;   (global-set-key (kbd "C-c L") 'winner-redo) ; think: Go Forward
;;   ;; frames
;;   (global-set-key (kbd "C-c f m") 'make-frame)
;;   (global-set-key (kbd "C-c f d") 'delete-frame)
;;   ;; Toggles (prefix: "t")
;;   (global-set-key (kbd "C-c t w") 'toggle-truncate-lines) ; Think: "toggle wrap"
;;   (global-set-key (kbd "C-c t h") 'horizontal-scroll-bar-mode)
;;   ;; Terminals (prefix: "t", sharing with "toggles" prefix)
;;   (global-set-key (kbd "C-c t e") 'eshell)
;;   (global-set-key (kbd "C-c t t") 'multi-term-dedicated-toggle)
;;   ;; eval
;;   (global-set-key (kbd "C-c e b") 'eval-buffer)
;;   (global-set-key (kbd "C-c e r") 'eval-region)
;;   (global-set-key (kbd "C-c e o") 'me/eval-and-replace) ; Think: "Eval Object"
;;   (global-set-key (kbd "C-c e d") 'eval-defun)
;;   ;; editing
;;   (global-set-key (kbd "C-c ]") (lambda () (interactive) (evil-open-below 1) (evil-previous-line 1) (evil-normal-state)))
;;   (global-set-key (kbd "C-c [") (lambda () (interactive) (evil-open-above 1) (evil-next-line 1) (evil-normal-state)))
;;   (global-set-key (kbd "C-c b s") (lambda () (interactive) (switch-to-buffer "*scratch*")))
;;   ;; misc
;;   (global-set-key (kbd "C-c /") 'swiper)
;;   )

;;; Searchable PDFs
;;; Nice defaults ripped from
;;; http://pragmaticemacs.com/emacs/view-and-annotate-pdfs-in-emacs-with-pdf-tools/
;;; Note: buggy. Uncomment and use when needed. Don't VC.
;; (use-package pdf-tools
;;   :pin manual ;; manually update
;;   :config
;;   ;; initialize
;;   (pdf-tools-install) ; [idempotent install]
;;   ;; open pdfs scaled to fit page
;;   (setq-default pdf-view-display-size 'fit-page)
;;   ;; automatically annotate highlights
;;   (setq pdf-annot-activate-created-annotations t)
;;   ;; use normal isearch
;;   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package ido
  :config
  )

;; Don't confirm to kill a buffer with a process
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

(progn
  (tooltip-mode -1)
  (setq tooltip-use-echo-area t))

(use-package diff-hl ; Highlight uncommitted changes using VC.
  ;;; Note: this can cause a major performance problem, especially in
  ;;; Org files. Might be best to keep it off until really needed.
  :config
  ;; (global-diff-hl-mode 1)
  ;; (diff-hl-flydiff-mode 1) ; highlight even when buffer not saved
  )

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 150))

(progn
  (defun me/dired-wsl-open-file ()
    "In dired, open the file named on this line in it's native app in MS
Windows using the wsl-open (August Valera, 2017) shell command."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process "wsl-open" nil 0 nil file)
      (message "Opening %s done" file)))
  (general-define-key :maps 'dired-mode-map
                      :states '(motion normal)
                      "W" 'me/dired-wsl-open-file))

;; Better *frame titles* to see what system Emacs is running on
(if me/wsl?
    ;; Show WSL and Linux Distro info (assumes WSL is running Ubuntu, YMMV for other distros.)
    (let ((extra-info (shell-command-to-string
                       "echo -n $(< /etc/lsb-release grep DISTRIB_DESCRIPTION= | cut -d= -f2 | sed -e 's/\"//g')")))
      (setq frame-title-format `(multiple-frames "%b" ("" invocation-name "@" system-name " [WSL : " ,extra-info "]"))))
  (setq frame-title-format (list "%b - " `default-directory " - %m "))) ; a useful frame title


(use-package dockerfile-mode)

;;; Stack Exchange (Overflow)
;; (use-package sx
;;   :config
;;   (add-to-list 'evil-emacs-state-modes 'sx-question-list-mode)
;;   (add-to-list 'evil-emacs-state-modes 'sx-question-mode)
;;   (add-to-list 'evil-emacs-state-modes 'sx-inbox-mode)
;;   (add-to-list 'evil-emacs-state-modes 'sx-compose-mode)
;;   (bind-keys :prefix "C-c s"
;;              :prefix-map my-sx-map
;;              :prefix-docstring "Global keymap for SX."
;;              ("q" . sx-tab-all-questions)
;;              ("i" . sx-inbox)
;;              ("o" . sx-open-link)
;;              ("u" . sx-tab-unanswered-my-tags)
;;              ("a" . sx-ask)
;;              ("s" . sx-search)))

;; helm-M-x is infinitely more discoverable than the default
;; execute-extended-command
(global-set-key (kbd "M-x") 'helm-M-x)

;; C-x C-m as handy M-x (from Steve Yegge)
(global-set-key (kbd "C-x C-m") 'helm-M-x)

;; Bind to prefixes: C-c, C-q, and (Evil normal) SPC
(->> '(
       "SPC"  counsel-ibuffer
       ;; "SPC"  helm-mini
       "X"    helm-M-x
       "a"    avy-goto-char-timer
       ;; Jumping around (repeating with an alpha so it works in C-c)
       "g o"   helm-mini
       "g a"   avy-goto-char-timer
       "g SPC" helm-M-x
       "g TAB" evil-buffer
       "g ,"   counsel-locate
       "g '"   eshell
       "e ["   me/insert-line-above
       "e ]"   me/insert-line-below
       ;; Window Movement - Single-letter - make sure you don't prefix any other binding to these 4 prefixes
       "j"  evil-window-down
       "k"  evil-window-up
       "h"  evil-window-left
       "l"  evil-window-right
       ;; Windows
       "w o"  delete-other-windows
       "z"    delete-other-windows
       "w w"  evil-window-next
       "w j"  evil-window-down
       "w k"  evil-window-up
       "w h"  evil-window-left
       "w l"  evil-window-right
       "w w"  evil-window-next  ; default emacs: 'other-window
       "w r"  me/rotate-windows
       "w d"  delete-window
       "w /"  me/split-window-right-and-switch
       "w ?"  split-window-right
       "w -"  me/split-window-below-and-switch
       "w _"  split-window-below
       ;;  Buffers
       "f f"  helm-find-files
       "f s"  save-buffer
       "b s"  me/scratchpad-open
       "b S"  me/scratchpad-open-other-date
       "b b"  bury-buffer
       "b d"  kill-this-buffer
       "d d"  kill-this-buffer
       "d D"  kill-this-buffer-and-window
       "q q"  kill-this-buffer
       "w q"  me/evil-wq-intelligently
       ;;  Frames
       "F d"  delete-frame
       "F m"  make-frame
       ;;  Basic Text Editing
       "r y" helm-show-kill-ring
       ;;  Projectile
       "p f"  counsel-projectile-find-file-dwim
       "p p"  counsel-projectile
       "p g"  counsel-projectile-rg
       ;; Paredit
       "P s"  paredit-forward-slurp-sexp
       "P b"  paredit-forward-barf-sexp
       "P )"  paredit-forward-slurp-sexp
       "P ("  paredit-forward-barf-sexp
       "P r"  paredit-raise-sexp
       ;;  Default Elisp Eval Everywhere
       "x x"  eval-defun
       "x e"  eval-last-sexp
       "x r"  eval-region
       "x b"  eval-buffer
       "x i"  me/eval-and-replace ; think: in-place
       ;; Org-Mode
       "o c" org-capture
       "o a" org-agenda
       "o d" helm-org-rifle-org-directory
       "o r" helm-org-rifle
       "o b" helm-org-rifle-current-buffer
       "o v" helm-velocity
       ;; Grepping
       "g g" grep
       "g h" me/helm-ripgrep ; now with prefix arg!
       "g /" swiper
       ;; Toggles
       ;; "t l" nlinum-mode
       ;; "t L" global-nlinum-mode
       "t l" display-line-numbers-mode
       "t L" global-display-line-numbers-mode
       "t t" toggle-truncate-lines
       "t w" toggle-word-wrap
       "t h" horizontal-scroll-bar-mode
       "t d" multi-term-dedicated-toggle
       "t s" whitespace-mode
       "t r" rainbow-delimiters-mode
       "t p" paredit-mode
       "t n" evil-ex-nohighlight
       "T"   multi-term
       ;; Misc
       "r j" jump-to-register ; default is `C-x r j`
       "g r" helm-resume
       "g d" helm-descbinds
       "g f" helm-find
       "g R" helm-recentf ; also C-x C-r
       "e v" me/find-file-init-el
       "e s" (lambda () (interactive) (find-file "~/.emacs.d/my-sensible-defaults.el"))
       ;; More
       "e y" me/xclip-copy
       "e p" me/xclip-paste
       "t m" multi-term
       "e c" helm-flyspell-correct
       ;; Winner
       "H" winner-undo
       "L" winner-redo
       "t f" me/create-today-temp-file
       )
     (-partition 2)
     (mapc (lambda (it)
             (let* ((keys (car it))
                    (func (car (cdr it))))
               (general-define-key :states '(emacs normal insert visual motion) (concat "C-c " keys) func)
               (general-define-key :states '(emacs normal insert visual motion) (concat "C-q " keys) func)
               (general-define-key :states '(normal)                            (concat "SPC " keys) func)
               ))))

;; Bind to prefixes: C-q and Evil normal SPC (but *NOT* C-c)
(->> '(
       "["   me/insert-line-above
       "]"   me/insert-line-below
       "TAB" evil-buffer
       "1"   delete-other-windows ; shadow C-x binding
       "2"   split-window-below ; shadow C-x binding
       "3"   split-window-right ; shadow C-x binding
       "0"   delete-window ; shadow C-x binding
       "C-t" multi-term-dedicated-toggle
       )
     (-partition 2)
     (mapc (lambda (it)
             (let* ((keys (car it))
                    (func (car (cdr it))))
               (general-define-key :states '(emacs normal insert visual motion) (concat "C-q " keys) func)
               (general-define-key :states '(normal)                            (concat "SPC " keys) func)
               ))))

;; Special SPC bindings for Evil Normal not wanted in C-q or C-c
(general-define-key :states '(normal)
                    ;; "SPC /"   'swiper
                    "SPC /"   'helm-swoop-without-pre-input
                    "SPC ;"   'helm-M-x
                    )

;; ;; Special C-q bindings not wanted in C-c or SPC
;; (general-define-key :states '(emacs normal insert visual motion)
;;                     "C-q /"  'me/split-window-right-and-switch
;;                     "C-q ?"  'split-window-right
;;                     "C-q -"  'me/split-window-below-and-switch
;;                     "C-q _"  'split-window-below
;;                     "C-q C-q" 'helm-mini
;;                     "C-q C-w" 'helm-M-x
;;                     )

;; (progn
;;   (setq me/C-q-prefix-only-binding-pairs
;;         '(
;;           "C-q C-e" helm-M-x
;;           "C-q C-b" helm-buffers-list
;;           "C-q C-r" helm-recentf
;;           "C-q C-o" delete-other-windows
;;           "C-q C-w" other-window
;;           "C-q C-t" multi-term-dedicated-toggle
;;           ;; ("C-q C-s" swiper
;;           "C-q C-s" helm-swoop-without-pre-input
;;           "C-q /"   me/split-window-right-and-switch
;;           "C-q ?"   split-window-right
;;           "C-q -"   me/split-window-below-and-switch ; dash
;;           "C-q _"   split-window-below ; underscore
;;           ;; ("C-q C-/" swiper ; underscore
;;           "C-q C-/" helm-swoop-without-pre-input ; underscore
;;           "C-q C-f" counsel-find-file
;;           "C-q C-;" evil-ex
;;           "C-q C-1" (lambda () (interactive) (evil-ex "!")) ; ! (bang) command
;;           ;; ("C-q C-q" counsel-ibuffer ; ! (bang) command
;;           "C-q C-q" helm-mini ; ! (bang) command
;;         ;;; TODO: Set up "C-q C-q"
;;         ;;; Make "C-q C-q" my default key to execute code in a major mode-sepcific way; i.e
;;         ;;;   - elisp -> evil-defun;
;;         ;;;   - ruby -> seeing-is-believing-run-as-xmpfilter;
;;         ;;;   - clojure me/cider-insert-eval-defun-in-repl
;;         ;;;   - sql-mode -> sql-send-paragraph
;;           ))
;;   (->> me/C-q-prefix-only-binding-pairs
;;        (-partition 2)
;;        (mapc (lambda (it)
;;                (let* ((keys (car it))
;;                       (func ((car (cdr it)))))
;;                  (global-set-key (kbd keys) func))))))

;;;; TODO: implement these functions to refactor all the keybinding (super-readable):
;; (mapc 'me/bind-with-prefixes-C-c-C-q-SPC '(
;;                                             ;; ...
;;                                             ))
;; (mapc 'me/bind-with-prefix-SPC '(
;;                                   ;; ...
;;                                   ))
;; (mapc 'me/bind-with-prefixes-C-q-SPC '(
;;                                         ;; ...
;;                                         ))
;; (mapc 'me/bind-with-prefix-C-q '(
;;                                   ;; ...
;;                                   ))

(global-set-key (kbd "C-q /") 'me/split-window-right-and-switch)
(global-set-key (kbd "C-q -") 'me/split-window-below-and-switch)
(global-set-key (kbd "C-q C-q") 'helm-mini)
(global-set-key (kbd "C-q C-r") 'helm-recentf)
(global-set-key (kbd "C-q C-\\") 'me/async-shell-command-on-region-keep-files)

;; (use-package airline-themes
;;   ;; vim-like status line with distinct colors for each Evil state.
;;   :config
;;   (setq airline-display-directory 'airline-directory-shortened)
;;   (setq airline-shortened-directory-length 50) ; Usually can see the tail few dirs.
;;   (load-theme 'airline-molokai t)
;;   ;; Apparently airline-get-vc function gets called in a pretty tight loop over
;;   ;; and i can't find any defcustom variables to disable this. It is making
;;   ;; working on files over Tramp very painful. So just set the function to
;;   ;; return an empty string that gets installed at the mode line. Seems to work
;;   ;; perfectly.
;;   (defun airline-get-vc () ""))

(use-package julia-mode)

(use-package julia-repl)

(use-package julia-shell)

(use-package helm-projectile)

(use-package rust-mode)

;; (use-package helm-youtube) ; doesn't work (returns nils in search results)

(use-package ivy-youtube) ; confirmed it works

(use-package clojure-mode)

(use-package cider
  :config
  (defun me/cider-insert-eval-defun-in-repl ()
    (interactive)
    (cider-insert-defun-in-repl t)
    (cider-switch-to-last-clojure-buffer))
  (defun me/cider-insert-eval-last-sexp-in-repl ()
    (interactive)
    (cider-insert-last-sexp-in-repl t)
    (cider-switch-to-last-clojure-buffer))
  (setq cider-show-error-buffer nil)
  (setq cider-auto-select-error-buffer nil)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-doc-auto-select-buffer nil)
  (setq cider-repl-pretty-print-width 100)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'evil-emacs-state)
  (add-hook 'cider-repl-mode-hook 'end-of-buffer) ; does this even work?
  ;; Emacs & Evil
  (general-define-key :keymaps '(cider-mode-map clojure-mode-map)
                      :states '(emacs normal insert visual)
                      ;; "C-i" 'me/cider-insert-eval-defun-in-repl
                      ;; "C-S-i" 'me/cider-insert-eval-last-sexp-in-repl
                      ;; [?\t] 'cider-repl-indent-and-complete-symbol
                      "C-j" 'me/cider-insert-eval-defun-in-repl
                      "C-S-j" 'me/cider-insert-eval-last-sexp-in-repl
                      "C-c x x"  'me/cider-insert-eval-defun-in-repl
                      "C-c x e"  'me/cider-insert-eval-last-sexp-in-repl
                      "C-c x r"  'cider-eval-region
                      "C-c x b"  'cider-eval-buffer
                      "C-c x i"  'cider-eval-last-sexp-and-replace
                      )
  ;; Evil only
  (general-define-key :keymaps '(cider-mode-map clojure-mode-map)
                      :states '(normal)
                      "SPC x x"  'me/cider-insert-eval-defun-in-repl
                      "SPC x e"  'me/cider-insert-eval-last-sexp-in-repl
                      "SPC x r"  'cider-eval-region
                      "SPC x b"  'cider-eval-buffer
                      "SPC x i"  'cider-eval-last-sexp-and-replace
                      ))

(defun me/async-say ()
  "Use macOS `say` command asynchronously on current paragraph or
   active region. Starts an async process to it does not block Emacs."
  (interactive)
  (save-excursion
    (start-process "say-process" "say-buffer" "say")
    (let ((no-active-region? (not (region-active-p))))
      (when no-active-region? (mark-paragraph))
      (process-send-region "say-process" (region-beginning) (region-end))
      (when no-active-region? (deactivate-mark)))))

(global-set-key (kbd "C-c S") 'me/async-say)
(global-set-key (kbd "C-c v") 'me/async-say)
(global-set-key (kbd "C-q S") 'me/async-say)
(global-set-key (kbd "C-q v") 'me/async-say)

;; The quick brown fox. The slow green turtle.

;; The medium speed dog.


;;; WIP: abstract out what I did in me/async-say to make it work
;;; generically for any command in what to pipe the region to and have
;;; it output to another buffer.

;; (defun me/async-shell-command-on-region ()
;;   (interactive)
;;   (process-send-region
;;    (start-process "*async-shell-on-region-process*"
;;                   "*async-shell-on-region-process*"
;;                   (read-shell-command "Async shell command: "))
;;    (region-beginning)
;;    (region-end))
;;   (display-buffer "*async-shell-on-region-process*")
;;   )

(progn
  (defun me/async-shell-command-on-region ()
    "WIP"
    (interactive)
    (save-excursion
      (start-process "*asorp3*"
                     "*asorp3*"
                     (read-shell-command "Async Shell Command: "))
      (let ((no-active-region? (not (region-active-p))))
        (when no-active-region? (mark-paragraph))
        (process-send-region "*asorp3*" (region-beginning) (region-end))
        (when no-active-region? (deactivate-mark)))
      (stop-process "*asorp3*")
      (display-buffer "*asorp3*")))
  (global-set-key (kbd "C-q |") 'me/async-shell-command-on-region)
  )

(defun me/highlight-sql-keywords ()
  (interactive)
  (highlight-phrase "select"                  'hi-green)
  (highlight-phrase "from\\|join\\|left join" 'hi-pink)
  (highlight-phrase "where"                   'hi-salmon)
  (highlight-phrase "group by"                'hi-aquamarine))

(defun me/unhighlight-all ()
  (interactive)
  (unhighlight-regexp t))

;; FIXME: file this with other key bindings.
;; Make TAB in evil normal act like TAB in Emacs, which is easy for
;; indenting code properly.
;;(use-package evil
  ;;:config
  ;;(use-package general
    ;;;;:config
    ;;;;(general-nmap "TAB" 'indent-for-tab-command)
    ;;))

(defun me/pipe-paragraph-to-shell-command-and-insert-output-below (&optional shell-cmd)
  "Handy for writing a shell command or SQL etc and piping it to the
  interpreter or filter, and having the output from the shell command
  be inserted just below the input paragraph in the current buffer.
  TODO: make this work on the region if it's active, otherwise the
  paragraph."
  (interactive)
  (save-excursion
    (let ((shell-cmd (if shell-cmd shell-cmd (read-shell-command "Shell command:"))))
      ;;; When on the last line, insert a line below to this all works correctly:
      (unless (s-ends-with? "\n" (thing-at-point 'line t))
        (me/insert-line-below)
        (me/insert-line-below))
      ;;;
      (unless (region-active-p) (mark-paragraph))
      (next-line)
      (kill-ring-save (point) (mark)) ; copy
      (yank) ; paste
      (newline)
      (newline) ; put extra newline between SQL and output so that act like "paragraphs"
      (mark-paragraph)
      (message (concat "Running shell command: " shell-cmd))
      (shell-command-on-region (point) (mark) shell-cmd (current-buffer) t)
      (message (concat "Shell command completed: " shell-cmd)))))

(global-set-key (kbd "C-q V")   'me/pipe-paragraph-to-shell-command-and-insert-output-below)
(global-set-key (kbd "C-q C-v") 'me/pipe-paragraph-to-shell-command-and-insert-output-below)

(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

;;; Pretty-printing for elisp code evaluations.
;; Replace the elisp eval functions with pp versions
(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
(defun me/pp-eval-defun ()
  "There is no pp-eval-defun, so here's one"
  (interactive)
  (save-excursion
    (end-of-defun)
    (pp-eval-last-sexp nil)))
(global-set-key [remap eval-defun] 'me/pp-eval-defun)
;; Easier on those Emacs pinkies:
(global-set-key (kbd "C-q C-j") 'me/pp-eval-defun)

;; A simple slug, just replaces spaces with dashes.
(defun me/s-slug (s) (s-replace " " "-" s))

;;
;; me/NOTES - BEGIN
;;
(defun me/notes-build-filename (&optional string)
  (format "%s/%s"
          (format-time-string "%Y/%m/%d")
          (->> (or string (read-string "File name string (spaces will become dashes):"))
               (s-replace " " "-"))))

(defun me/notes-find-file-new-note-personal (&optional string) (interactive) (find-file (concat "~/lrg-notes/personal/" (me/notes-build-filename (read-string "Personal file name string (spaces will become dashes):")))))
(defun me/notes-find-file-new-note-tech     (&optional string) (interactive) (find-file (concat "~/lrg-notes/tech/"     (me/notes-build-filename (read-string "Tech file name string (spaces will become dashes):")))))
(defun me/notes-find-file-new-note-amazon   (&optional string) (interactive) (find-file (concat "~/lrg-notes/amazon/"   (me/notes-build-filename (read-string "Amazon file name string (spaces will become dashes):")))))

(global-set-key (kbd "C-q n p") 'me/notes-find-file-new-note-personal)
(global-set-key (kbd "C-q n t") 'me/notes-find-file-new-note-tech)
(global-set-key (kbd "C-q n a") 'me/notes-find-file-new-note-amazon)
;;
;; End of LRG/NOTES
;;


(defun me/multi-term-run-program (&optional program)
  (interactive)
  (let* ((program (or program (read-shell-command "Shell command:")))
         (multi-term-program program)
         (multi-term-buffer-name program))
    (multi-term)))

(defun me/multi-term-pry ()
  (interactive)
  (me/multi-term-run-program "pry"))

(defun me/multi-term-sqlite ()
  (interactive)
  (me/multi-term-run-program "sqlite"))

(defun me/multi-term-psql ()
  (interactive)
  (me/multi-term-run-program "psql"))


;; https://stackoverflow.com/questions/2091881/emacs-font-sizing-with-ctrl-key-and-mouse-scroll/#7142001
(progn
  (add-to-list 'load-path "~/.emacs.d/lisp")
  (global-set-key [S-mouse-1]   'zoom-in)
  (global-set-key [C-S-mouse-1] 'zoom-out)
  (when (boundp 'mouse-wheel-down-event) (global-set-key (vector (list 'control mouse-wheel-down-event)) 'zoom-in))
  (when (boundp 'mouse-wheel-up-event) (global-set-key (vector (list 'control mouse-wheel-up-event))   'zoom-out)))

(progn
  ;; http://pragmaticemacs.com/emacs/open-a-recent-directory-in-dired-revisited/
  ;; open recent directory, requires ivy (part of swiper)
  ;; borrows from http://stackoverflow.com/questions/23328037/in-emacs-how-to-maintain-a-list-of-recent-directories
  (defun me/ivy-dired-recent-dirs ()
    "Present a list of recently used directories and open the selected one in dired"
    (interactive)
    (let ((recent-dirs
           (delete-dups
            (mapcar (lambda (file)
                      (if (file-directory-p file) file (file-name-directory file)))
                    recentf-list))))

      (let ((dir (ivy-read "Directory: "
                           recent-dirs
                           :re-builder #'ivy--regex
                           :sort nil
                           :initial-input nil)))
        (dired dir))))
  ;; Replace 'show directories briefly' or something like that - a useless default binding.
  (global-set-key (kbd "C-x C-d") 'bjm/ivy-dired-recent-dirs))

(defun me/ffpa-seeing-is-believing-run-as-xmpfilter ()
  (interactive)
  (let* ((seeing-is-believing-executable (concat seeing-is-believing-executable
                                                 " -I /Users/lgillett/repos/amazon-ffpa-star-schema/lib"
                                                 " -x")))
    (seeing-is-believing-mark-current-line-for-xmpfilter)
    (seeing-is-believing-run-as-xmpfilter)))

;; Set the font to something nice for macOS - Menlo height 140!
(when (eq window-system 'ns) (set-face-attribute 'default nil
                                                 ;; :font "Menlo" :height 140
                                                 :font "Andale Mono" :height 120 ;140
                                                 ))

;;; Edit With Emacs - Google Chrome extension.
;; Allow user to edit web-page textareas with Emacs (and other editors).
;; https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh?hl=en
;; C-c C-c to save, hide Emacs, and return to Chrome.
;; C-x C-c to abort an edit.
(use-package edit-server
  :config
  (when window-system
    (setq edit-server-new-frame nil) ; nil = edit
    (when (eq window-system 'ns) (add-hook 'edit-server-done-hook 'ns-do-hide-emacs))
    (edit-server-start)))

(when (file-exists-p "~/repos/fzf.el") (use-package fzf :load-path "~/repos/fzf.el"))

(defun me/pipe-ffpa-ro-pretty () (interactive) (me/pipe-paragraph-to-shell-command-and-insert-output-below "ssh dd3 'dbcli ffpa_ro --table'"))
(global-set-key (kbd "C-c i m") 'me/pipe-ffpa-ro-pretty)
(general-nmap "SPC i m" 'me/pipe-ffpa-ro-pretty)
(general-vmap "SPC i m" 'me/pipe-ffpa-ro-pretty)

(defun me/pipe-ffpa-ro-pretty () (interactive) (me/pipe-paragraph-to-shell-command-and-insert-output-below "ssh dd3 'dbcli ffpa_ro --table'"))
(global-set-key (kbd "C-c i M") 'me/pipe-ffpa-ro-pretty)
(general-nmap "SPC i M" 'me/pipe-ffpa-ro-pretty)
(general-vmap "SPC i M" 'me/pipe-ffpa-ro-pretty)

(defun me/pipe-default-shell () (interactive) (me/pipe-paragraph-to-shell-command-and-insert-output-below (getenv "SHELL")))
(global-set-key (kbd "C-c i s") 'me/pipe-default-shell)
(general-nmap "SPC i s" 'me/pipe-default-shell)
(general-vmap "SPC i s" 'me/pipe-default-shell)

;; minor mode to save buffer at regular intervals (seconds). Fantastic!
(use-package real-auto-save
  :config (setq real-auto-save-interval 60))

;; Use a smaller font in certain modes
(add-hook 'shell-mode-hook 'me/text-scale-adjust-down-2)
(add-hook 'sql-mode-hook 'me/text-scale-adjust-down-1)


(defun me/sql-mode-enable-mysql-hash-style-comments ()
  "Allow # to start line comments in sql-mode. This is MySQL-specific thing."
  (interactive)
  (modify-syntax-entry ?# "< b" sql-mode-syntax-table))

(add-hook 'sql-mode-hook 'me/sql-mode-enable-mysql-hash-style-comments)

(global-set-key (kbd "M-x") 'counsel-M-x)

(add-hook 'before-save-hook 'whitespace-cleanup)

(use-package darkokai-theme :config (load-theme 'darkokai t))

(setq evil-ex-search-persistent-highlight nil)

(use-package org)
(use-package ob-async)

(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a
    graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines
                             (if (display-graphic-p frame)
                                  1 0)))

(add-hook 'after-make-frame-functions 'contextual-menubar)
(add-hook 'sql-mode-hook 'sqlup-mode)


(load-file (concat user-emacs-directory "me-scratchpad.el"))
