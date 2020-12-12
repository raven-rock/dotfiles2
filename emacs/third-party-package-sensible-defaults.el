;; This file loads up nice default packages that must be pulled in
;; from the internet.

(progn
  ;; (package-initialize)
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
  ;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (setq use-package-always-ensure t)
  (require 'use-package))

(use-package diminish)

;; Modern functional programming tools for Elisp.
(use-package dash
  :config
  (use-package dash-functional))

(use-package general) ; Better keybinding manager.

;; When launching GUI from icon, use default shell's environment.
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package which-key :init (which-key-mode t)) ; Heads up display of discoverable menus.

(use-package expand-region :config (global-set-key (kbd "C-=") 'er/expand-region))

;; winner-mode: undo/redo window configuration
(use-package winner
  :config
  (winner-mode t))

(use-package grep
  :after (exec-path-from-shell)
  :config
  (when (executable-find "rg")
    (setq grep-command "rg --hidden -S -z --vimgrep ")
    (setq grep-use-null-device nil)))

(use-package tramp
  :defer 5
  :config
  (add-to-list 'tramp-remote-path (format "/home/%s/.local/bin"     user-login-name))
  (add-to-list 'tramp-remote-path (format "/home/%s/.fzf/bin"       user-login-name))
  (add-to-list 'tramp-remote-path (format "/home/%s/.cargo/bin"     user-login-name))
  (add-to-list 'tramp-remote-path (format "/home/%s/go/bin"         user-login-name))
  (add-to-list 'tramp-remote-path (format "/home/%s/.linuxbrew/bin" user-login-name))
  (add-to-list 'tramp-remote-path (format "/home/%s/bin"            user-login-name))
  (add-to-list 'tramp-remote-path "/home/linuxbrew/.linuxbrew/bin")
  (setq tramp-use-ssh-controlmaster-options nil) ; use what's in ~/.ssh/config instead
  )

;; Allow Shift+arrows to always move around windows
(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package evil
  :config
  (use-package dash)
  (use-package general :config (general-evil-setup))
  ;;; Comment the next line out if you do not turn on by default:
  ;; (evil-mode 1)
  ;;; Save/persist search history between searches and sessions
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;;; Escape acts more like vim (don't have to hit C-g constantly)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  ;;; Use emacs state rather than motion or insert state
  (progn
    (-each evil-motion-state-modes (lambda (it) (add-to-list 'evil-emacs-state-modes it)))
    (-each evil-insert-state-modes (lambda (it) (add-to-list 'evil-emacs-state-modes it)))
    (setq evil-motion-state-modes '())
    (setq evil-insert-state-modes '())
    )
  (define-key dired-mode-map (kbd "SPC") nil)
  (define-key help-mode-map (kbd "SPC") nil)
  (add-hook 'shell-mode-hook 'evil-normal-state)
  ;;; Allow `gc` action to comment (just `gcc` comments current line)
  (use-package evil-commentary
    :config (evil-commentary-mode t)
    :diminish evil-commentary-mode)
  ;;; Allow ysiw and friends
  (use-package evil-surround
    :config (global-evil-surround-mode t))
  (use-package evil-lion
    :config (evil-lion-mode))
  ;;; Handy Add on packages for Evil:
  (use-package evil-surround
    :config
    (global-evil-surround-mode t))
  (use-package evil-lion
    :config
    (evil-lion-mode))
  (use-package evil-commentary
    :config (evil-commentary-mode t)
    :diminish evil-commentary-mode)
  ;;; Evil Line motions: il & al. Yanked from https://github.com/syohex/emacs-evil-textobj-line
  (progn
    (require 'evil)
    (defgroup evil-textobj-line nil
      "Text object line for Evil"
      :group 'evil)
    (defcustom evil-textobj-line-i-key "l"
      "Keys for evil-inner-line"
      :type 'string
      :group 'evil-textobj-line)
    (defcustom evil-textobj-line-a-key "l"
      "Keys for evil-a-line"
      :type 'string
      :group 'evil-textobj-line)
    (defun evil-line-range (count beg end type &optional inclusive)
      (if inclusive
          (evil-range (line-beginning-position) (line-end-position))
        (let ((start (save-excursion
                       (back-to-indentation)
                       (point)))
              (end (save-excursion
                     (goto-char (line-end-position))
                     (skip-syntax-backward " " (line-beginning-position))
                     (point))))
          (evil-range start end))))
    (evil-define-text-object evil-a-line (count &optional beg end type)
      "Select range between a character by which the command is followed."
      (evil-line-range count beg end type t))
    (evil-define-text-object evil-inner-line (count &optional beg end type)
      "Select inner range between a character by which the command is followed."
      (evil-line-range count beg end type))
    (define-key evil-outer-text-objects-map evil-textobj-line-a-key 'evil-a-line)
    (define-key evil-inner-text-objects-map evil-textobj-line-i-key 'evil-inner-line)
    (provide 'evil-textobj-line)
    )
  ;; end of evil line object.
  ;;; Evil Ex Commands:
  (progn
    ;; easy modes
    (evil-ex-define-cmd "wsm" 'whitespace-mode)
    (evil-ex-define-cmd "lim" 'lisp-interaction-mode)
    (evil-ex-define-cmd "mm" 'markdown-mode)
    (evil-ex-define-cmd "om" 'org-mode)
    (evil-ex-define-cmd "fm" 'fundamental-mode)
    (evil-ex-define-cmd "afm" 'auto-fill-mode)
    (evil-ex-define-cmd "rdm" 'rainbow-delimiters-mode)
    (evil-ex-define-cmd "pem" 'paredit-mode)
    (evil-ex-define-cmd "spm" 'smartparens-mode)
    (evil-ex-define-cmd "hig" 'highlight-indent-guides-mode)
    (evil-ex-define-cmd "higm" 'highlight-indent-guides-mode)
    (evil-ex-define-cmd "sbm" 'scroll-bar-mode)
    ;; operations
    (evil-ex-define-cmd "wsc" 'whitespace-cleanup)
    (evil-ex-define-cmd "sib" 'seeing-is-believing-run-as-xmpfilter)
    (evil-ex-define-cmd "xmp" 'seeing-is-believing-run-as-xmpfilter)
    ;; eval'ing
    (evil-ex-define-cmd "er" 'eval-region)
    (evil-ex-define-cmd "eb" 'eval-buffer)
    ;; launch
    (evil-ex-define-cmd "mt" 'multi-term)
    (evil-ex-define-cmd "nt" 'neotree-toggle)
    (evil-ex-define-cmd "ntr" 'neotree-refresh)
    )

  (use-package general ; Misc Evil Stuff
    :config
    (general-define-key
     :states '(normal motion)
     "Y"  (defun me/evil-copy-to-end-of-line () (interactive) (evil-yank (point) (point-at-eol)))
     "gx" 'browse-url-at-point
     )

    ;; When in terminal, correct weird bug where TAB won't org-cycle headers
    ;; (unless window-system
    ;;   (general-define-key
    ;;    :states 'normal
    ;;    :modes 'org-mode-map
    ;;    "TAB" 'org-cycle))
    )

  ;;; Mode that should start up in Evil Normal Mode
  (setq evil-normal-state-modes '(shell-mode
                                  help-mode
                                  Man-mode))

  (evil-mode 0))

(use-package magit
  :config
  (add-to-list 'magit-process-password-prompt-regexps "^Kerberos authentication failed.  Password: ?$"))

(use-package swiper :init (global-set-key (kbd "C-c s") 'swiper))

(use-package ivy
  :init
  ;; (ivy-mode 1)
  ;;; show bookmark-alist in ivy-switch-buffer (already shows buffers & recentf)
  (setq ivy-use-virtual-buffers t)
  ;;; http://oremacs.com/swiper/#ivy--regex-ignore-order
  ;;; https://oremacs.com/2016/01/06/ivy-flx/
  ;;; Completion style functions: ivy--regex (default), ivy--regex-plus, ivy--regex-ignore-order, ivy--regex-fuzzy.
  ;;; Use t to set all ivy functions to use a certain style function.
  ;;; Use a certain ivy tool to be more specific.
  (setq ivy-re-builders-alist
        '(
          ;; (t . ivy--regex-ignore-order) ; Slow; helm-swoop is faster but no font faces
          (t . ivy--regex-plus)
          ;;(swiper . ivy--regex-fuzzy) ; uncomment/modify if you want fuzzy matching for certion tools
          )))

(use-package counsel
  :config
  ;; (counsel-mode t)
  ;; Make counsel-ibuffer window up to 30 lines (files) tall:
  (add-to-list 'ivy-height-alist '(counsel-ibuffer . 30))
  ;;; Set the width of "name" (filename) column in ibuffer and
  ;;; subsequently counsel-ibuffer to 50 to accommodate being able to
  ;;; read long file names (Emacs' default was only 18):
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 50 50 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  :bind (
         ;; ("C-c SPC" . ivy-switch-buffer)
         ;; ("C-c R" . counsel-recentf)
         ;; ("C-c ;" . counsel-M-x)
         :map
         evil-normal-state-map
         ;; ("SPC SPC" . ivy-switch-buffer )
         ;; ("SPC o" . counsel-ibuffer)
         ;; ("SPC R" . counsel-recentf)
         ;; ("SPC ;" . counsel-M-x)
         ))

(use-package counsel-projectile :after counsel)

(use-package helm
  :init
  (helm-mode t)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-c r y") 'helm-show-kill-ring)
  (use-package helm-rg :config (require 'cl-lib))
  (use-package helm-ag))

(use-package sql
  :config
  (add-hook 'sql-login-hook
            (defun me/sql-login-hook ()
              "Custom SQL log-in behaviours. See `sql-login-hook'."
              ;; n.b. If you are looking for a response and need to parse the
              ;; response, use `sql-redirect-value' instead of `comint-send-string'.
              (when (eq sql-product 'postgres)
                (let ((proc (get-buffer-process (current-buffer))))
                  ;; Output each query before executing it. (n.b. this also avoids
                  ;; the psql prompt breaking the alignment of query results.)
                  (comint-send-string proc "\\set ECHO queries\n")
                  (comint-send-string proc "\\timing\n")))))
  (add-hook 'sql-interactive-mode-hook (lambda () (setq truncate-lines t))) ; Dont wrap lines in SQL results buffers
  (push '("*SQL*" nil . ((reusable-frames . t))) display-buffer-alist)) ; Reuse whatever frame the *SQL* buffer is being displayed

;; Auto upcase SQL keywords.
;; docs: https://github.com/trevoke/sqlup-mode.el
;; Instead of automatic, let's make it opt-in via a =C-c u= on a region.
;; (use-package sqlup-mode
;;   :config
;;   (add-to-list 'sqlup-blacklist "name")
;;   :bind (:map
;;          sql-mode-map
;;          ("C-c u" . 'sqlup-capitalize-keywords-in-region)))

(use-package paredit
  :config
  (use-package evil)
  (add-hook 'lisp-mode-hook                'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook          'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook    'enable-paredit-mode)
  (add-hook 'clojure-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook             'enable-paredit-mode)
  :bind (:map
         paredit-mode-map
         ("C-M-s-s" . 'paredit-forward-slurp-sexp)
         ("C-M-s-b" . 'paredit-forward-barf-sexp)
         ("C->"     . 'paredit-forward-slurp-sexp)
         ("C-<"     . 'paredit-forward-barf-sexp)
         :map
         evil-normal-state-map
         ("M-i" . 'paredit-open-round)
         ("M-o" . 'paredit-close-round)
         :map
         evil-insert-state-map
         ("M-i" . 'paredit-open-round)
         ("M-o" . 'paredit-close-round)))

(use-package neotree
  :config
  (setq neo-window-fixed-size t)
  (setq neo-window-width 50)
  (setq neo-reset-size-on-open t)
  (setq neo-smart-open t)
  (setq neo-show-hidden-files t)
  (setq neo-autorefresh nil)
  (setq neo-vc-integration nil)
  (setq neo-toggle-window-keep-p t)
  (when (eq window-system 'ns) (setq neo-default-system-application "open"))
  :bind (("<f8>" . neotree-toggle))
  )

;;;
;; Skin/Theme stuff
;; The following are *order dependent*. Load the theme first, then modify custom
;; faces afterward.
(progn ;; Theme/Skin Stuff
  (defun me/set-region-and-search-colors ()
    "Way better region and search highlighting colors"
    (interactive)
    (set-face-attribute 'region nil :background "green" :foreground "black" :weight 'ultra-bold)
    (set-face-attribute 'isearch nil        :background "orange"    :foreground "black")
    (set-face-attribute 'lazy-highlight nil :background "steelblue" :foreground "black"))
  (defun me/set-custom-background-color ()
    "Changes the default bg color, as well as for fringe and linum."
    (interactive)
    (use-package linum) ; dep
    (use-package nlinum) ; dep
    (let ((color "#191b1c")) ; a nice almost-black
      (set-face-background 'default color)
      (set-face-background 'fringe color)
      (set-face-background 'linum color)))
  (defun me/set-custom-background-color-pitch-black ()
    "Changes the default bg color to pitch black, as well as for fringe and linum."
    (interactive)
    (use-package linum) ; dep
    (use-package nlinum) ; dep
    (let ((color "black")) ; a nice almost-black
      (set-face-background 'default color)
      (set-face-background 'fringe color)
      (set-face-background 'linum color)))
  (defun me/set-linum-foreground-to-dim-gray ()
    (interactive)
    (use-package linum) ; dep
    (use-package nlinum) ; dep
    (set-face-attribute 'linum nil :foreground "gray30"))
  (defun me/set-rainbow-delimiters-colors ()
    "Sets/Resets rainbow delimiters colors. Useful for calling after
     loading some themes which may mess up the nice custom colors."
    (interactive)
    (use-package rainbow-delimiters
      :config
      (set-face-attribute 'rainbow-delimiters-depth-1-face nil :weight 'bold :foreground "chartreuse")
      (set-face-attribute 'rainbow-delimiters-depth-2-face nil :weight 'bold :foreground "deep pink")
      (set-face-attribute 'rainbow-delimiters-depth-3-face nil :weight 'bold :foreground "deep sky blue")
      (set-face-attribute 'rainbow-delimiters-depth-4-face nil :weight 'bold :foreground "dark orange")
      (set-face-attribute 'rainbow-delimiters-depth-5-face nil :weight 'bold :foreground "orchid")
      (set-face-attribute 'rainbow-delimiters-depth-6-face nil :weight 'bold :foreground "yellow")
      (set-face-attribute 'rainbow-delimiters-depth-7-face nil :weight 'bold :foreground "spring green")
      (set-face-attribute 'rainbow-delimiters-depth-8-face nil :weight 'bold :foreground "blue")))
  ;; (defun me/use-hack-font ()
  ;;   "On macOS, first install the Hack font: `brew cask install homebrew/cask-fonts/font-hack`."
  ;;   (interactive)
  ;;   (add-to-list 'default-frame-alist '(font . "-*-Hack-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")))
  (defun me/set-up-theme ()
    (interactive)
    (use-package monokai-theme :config (load-theme 'monokai t))
    ;; Levi's heaven-and-hell version
    (use-package heaven-and-hell
      :ensure t
      :init
      (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
      (setq heaven-and-hell-themes
            '(
              (light . doom-one-light)
              ;; (light . doom-solarized-light)
              (dark . monokai)
              )) ;; Themes can be the list: (dark . (tsdh-dark wombat))
      ;; Optionall, load themes without asking for confirmation.
      (setq heaven-and-hell-load-theme-no-confirm t)
      :hook (after-init . heaven-and-hell-init-hook)
      :bind (("C-c <f6>" . heaven-and-hell-toggle-theme))
      )
    (me/set-region-and-search-colors)
    ;; (me/set-custom-background-color)
    ;; (me/set-custom-background-color-pitch-black)
    ;; (me/set-linum-foreground-to-dim-gray)
    (when (eq window-system 'ns)
      ;; (add-to-list 'default-frame-alist '(font . "Menlo-14"))
      ;; (add-to-list 'default-frame-alist '(font . "Courier New-14"))
      ;; (add-to-list 'default-frame-alist '(font . "Hack-15"))
      (add-to-list 'default-frame-alist '(font . "Andale Mono-14"))
      )
    ;; In monokai, use a *less* dark gray because I can hard see the default #75715E
    (when (member 'monokai custom-enabled-themes)
      (set-face-attribute 'shadow nil :foreground "gray") ; affects org mode =verbatim= text, and probably more
      (set-face-attribute 'font-lock-comment-face nil :foreground "dark gray")) ; affects code comments
    ))

(use-package rainbow-delimiters
  :config
  ;;;; Mode Hooks
  ;;; Emacs Lisp
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  ;;; Clojure
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  ;;; Ruby
  (add-hook 'ruby-mode-hook 'rainbow-delimiters-mode)
  ;;; Better face colors
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :weight 'bold :foreground "chartreuse")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :weight 'bold :foreground "deep pink")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :weight 'bold :foreground "deep sky blue")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :weight 'bold :foreground "dark orange")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :weight 'bold :foreground "orchid")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :weight 'bold :foreground "yellow")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :weight 'bold :foreground "spring green")
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil :weight 'bold :foreground "blue")
  )

(use-package multi-term
  :config
  (setq multi-term-dedicated-window-height 40
        multi-term-dedicated-select-after-open-p t
        )
  ;; Cycle thru open mt's with M-right/left (or create new one if none)
  (global-set-key (kbd "M-<right>") 'multi-term-next)
  (global-set-key (kbd "M-<left>") 'multi-term-prev)
  )

(use-package markdown-mode)

(use-package ruby-mode
  :config
  ;; (let ((file (concat user-emacs-directory "lisp/smartparens-ruby.el"))) (when (file-exists-p file) (load-file file)))
  (use-package smartparens :config (require 'smartparens-ruby)) ; auto complete blocks and stuff
  (add-hook 'ruby-mode-hook 'smartparens-mode)
  (add-hook 'ruby-mode-hook 'me/toggle-show-trailing-whitespace)
  (use-package general)
  (use-package seeing-is-believing
    :config
    (defun me/seeing-is-believing-mark-and-run-as-xmpfilter ()
      (interactive)
      (seeing-is-believing-mark-current-line-for-xmpfilter)
      (seeing-is-believing-run-as-xmpfilter))
    (general-define-key
     :maps 'ruby-mode-map
     :states 'normal
     "SPC x x" 'me/seeing-is-believing-mark-and-run-as-xmpfilter
     "SPC x c" 'seeing-is-believing-clear))
  :bind (("C-j" . 'me/seeing-is-believing-mark-and-run-as-xmpfilter)))

(use-package eshell
  :config
  (global-set-key (kbd "C-c E") 'eshell)
  ;; More legible and informative multi-line prompt, especially when
  ;; working in remote directories.
  (setq eshell-prompt-regexp "^[^#$\n]*[#$] "
        eshell-prompt-function
        (lambda nil
          (concat
           "\n" "[" (user-login-name) "@" (system-name) " "
           (if (string= (eshell/pwd) (getenv "HOME"))
               "~" (eshell/pwd))
           "]" "\n"
           (if (= (user-uid) 0) "# " "$ "))))
  )

(use-package json-mode)

(use-package yaml-mode)

;;; vlf - Very Large File support.
;; Lazy-load big files.
;; Usage:
;;   M-x vlf   -- like find file
;;    -or-
;;   After vlf-setup, just find-file a big file and be prompted how you want to open it.
(use-package vlf
  :config
  (require 'vlf-setup))

;;; avy - Jump to arbitrary positions in visible text and select text quickly.
(use-package avy)

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package dockerfile-mode)

(use-package ido
  :config
  ;; (ido-mode 1)
  ;; (ido-everywhere)
  (setq ido-create-new-buffer 'always)
  (defun me/ido-recentf-open ()
    "Use `ido-completing-read' to find a recent file."
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " (mapcar 'abbreviate-file-name recentf-list)))
        (message "Opening file...")
      (message "Aborting")))
  ;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)
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
  (use-package general
    :config
    (require 'dired)
    (general-define-key :maps 'dired-mode-map
                        :states '(motion normal)
                        "W" 'me/dired-wsl-open-file)))

;; Better *frame titles* to see what system Emacs is running on
(if me/wsl?
    ;; Show WSL and Linux Distro info (assumes WSL is running Ubuntu, YMMV for other distros.)
    (let ((extra-info (shell-command-to-string
                       "echo -n $(< /etc/lsb-release grep DISTRIB_DESCRIPTION= | cut -d= -f2 | sed -e 's/\"//g')")))
      (setq frame-title-format `(multiple-frames "%b" ("" invocation-name "@" system-name " [WSL : " ,extra-info "]"))))
  ;; Show system-name, e.g., gnu/linux or windows-nt
  (setq frame-title-format `(multiple-frames "%b" ("" invocation-name "@" system-name " [" ,(symbol-name system-type) "]"))))

(use-package smex
  :config
  ;; (global-set-key (kbd "M-x") 'smex)
  )

(use-package hl-todo
  :config
  (add-to-list 'hl-todo-keyword-faces
              '("REVIEW" . "#7cb8bb")))

(use-package fic-mode
  :config
  ;; https://github.com/rubocop-hq/ruby-style-guide#comment-annotations
  (add-hook 'prog-mode-hook 'fic-mode)
  (add-to-list 'fic-highlighted-words "REVIEW")
  (add-to-list 'fic-highlighted-words "OPTIMIZE")
  (add-to-list 'fic-highlighted-words "HACK")
  (add-to-list 'fic-highlighted-words "CHECK")
  (add-to-list 'fic-highlighted-words "WIP")
  )

(use-package rainbow-mode)

;; (use-package github-theme)

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  ;; Make other mode effectively inherit from fundamental-mode
  ;; https://github.com/joaotavora/yasnippet/issues/557
  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (yas-activate-extra-mode 'fundamental-mode)))
  (yas-global-mode 1))

(use-package crontab-mode)

;;; Set up the modeline to be more useful
;; (me/show-project-relative-path-in-modeline)
(use-package spaceline :config (spaceline-spacemacs-theme))

;;; Flash the cursor to help your eyes quickly locate it when jumping around
(use-package beacon :config (setq beacon-color "#FF0090"))

;; Display Unicode (😄), Github-style (`:smile:`), and ascii (`:-)`) emoji after Emacs 25.
;; https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :config
  (setq emojify-emoji-styles '(unicode github))
  (setq emojify-download-emojis-p t)
  (global-emojify-mode))

(use-package vterm
  :config
  (setq vterm-max-scrollback 20000)
  (add-to-list 'evil-emacs-state-modes 'vterm-mode))

(use-package pandoc-mode)

(use-package highlight-indent-guides) ; grep keywords: columns
