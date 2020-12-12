(setq me/all-lispy-mode-maps '(clojure-mode-map
                                lisp-interaction-mode-map ; inherits from emacs-lisp-mode-map
                                emacs-lisp-mode-map
                                eshell-mode-map
                                lisp-mode-map
                                scheme-mode-map))

(setq me/all-elisp-mode-maps '(emacs-lisp-mode-map
                                eshell-mode-map
                                lisp-interaction-mode-map))

(setq me/all-lispy-mode-hooks '(clojure-mode-hook
                                 emacs-lisp-mode-hook
                                 eshell-mode-hook
                                 ielm-mode-hook
                                 lisp-mode-hook
                                 cider-repl-mode-hook))

;; wsl? predicate - are we running emacs under Windows Susbsystem for Linux?
;; Use this predicate in a when conditional anywhere there's WSL-specific stuff to do
(setq me/wsl? (and (eq system-type 'gnu/linux)
                    (string-match "Microsoft" (shell-command-to-string "uname -r"))))

(defun me/wsl-open-file (ARG)
  (interactive "FWSL Open File: ")
  (call-process "wsl-open" nil 0 nil ARG))

(defun me/evil-quit-emacs-intelligently ()
  "Kill the current buffer but not the Emacs process."
  (interactive)
  (if (daemonp)
      (save-buffers-kill-terminal)
    (kill-this-buffer)))

(defun me/evil-wq-intelligently ()
  "Write (save) buffer and kill it, but do not kill the Emacs process."
    (interactive)
    (save-buffer)
    (me/evil-quit-emacs-intelligently))

(defun me/kill-dired-buffers ()
  (interactive)
  (let ((dired-buffer? (lambda (buffer)
                         (eq 'dired-mode
                             (buffer-local-value 'major-mode buffer)))))
    (->> (buffer-list)
         (-filter dired-buffer?)
         (-map 'kill-buffer)
         (length)
         (message "%s dired buffers killed"))))

(defun me/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting.
Credit: http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun me/sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun me/find-file-init-el () (interactive) (find-file "~/.emacs.d/init.el"))

(defun me/eval-and-replace ()
  "Replace the preceding sexp with its value. Credit:
http://emacsredux.com/blog/2013/06/21/eval-and-replace/"
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun me/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting.
Credit: http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/"
  (interactive)
  (if (yes-or-no-p (concat "Are you sure you want to delete this file and buffer '" (buffer-file-name) "'?"))
      (let ((filename (buffer-file-name)))
        (when filename
          (if (vc-backend filename)
              (vc-delete-file filename)
            (progn
              (delete-file filename)
              (message "Deleted file %s" filename)
              (kill-buffer)))))
    (message "Aborted deletion of file and buffer")))

(defun me/toggle-window-split ()
  "Toggle a 2-window frame configuration between...
------------------------------------
|           |                      |
| buffer 1  |        buffer 2      |
|           |                      |
------------------------------------
...and...
------------------------------------
|          buffer 1                |
|                                  |
------------------------------------
|           buffer 2               |
|                                  |
------------------------------------
If frame is not in a 2-window configuration, does nothing. Credits:
https://github.com/magnars/.emacs.d/blob/master/defuns/buffer-defuns.el
https://emacs.stackexchange.com/questions/318/switch-window-split-orientation-fastest-way/320#320"
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

(defun me/insert-line-above ()
  (interactive)
  (evil-insert-newline-above)
  (forward-line))

(defun me/insert-line-below ()
  (interactive)
  (evil-insert-newline-below)
  (forward-line -1))

(defun me/find-file-today-scratch-file ()
  "Visit today's scratch buffer at '~/notes/scratch-{YYYYMMDD}.txt'.
Great to bind to H-t"
  (interactive)
  ;; (find-file (me/today-scratch-file-path))
  (find-file (format-time-string "~/scratch/scratch-%Y%m%d.org")))

(defun me/pop-to-scratch-buffer ()
  "Pop to *scratch* buffer in a side window."
  (interactive)
  (pop-to-buffer "*scratch*"))

(defun me/switch-to-scratch-buffer ()
  "Pop to *scratch* buffer in a side window."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun me/create-today-temp-file ()
  (interactive)
  (let* (
         (date (s-chomp (shell-command-to-string "date +%Y-%m-%d")))
         (users-filename (s-replace " " "_" (read-string "Filename:")))
         (filepath (format "~/tmp/%s_%s" date users-filename))
         )
    (find-file filepath)))

(defun youtube ()
  "Search YouTube with a query or region if any. Adapted from
http://emacsredux.com/blog/2013/08/26/search-youtube/"
  (interactive)
  (browse-url
   (concat
    "http://www.youtube.com/results?search_query="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search YouTube: "))))))

(defun google ()
  "Google the selected region if any, display a query prompt
otherwise. Adapted from http://emacsredux.com/blog/2013/03/28/google/"
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?q="
    (url-hexify-string
     (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

(defun ddg ()
  "DuckDuckGo the selected region if any, display a query prompt
otherwise. Adapted from http://emacsredux.com/blog/2013/03/28/google/"
  (interactive)
  (browse-url
   (concat
    "http://duckduckgo.com/?q="
    (url-hexify-string
     (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "DuckDuckGo: "))))))

(defun me/split-window-below-and-switch () (interactive) (split-window-below) (other-window 1))

(defun me/split-window-right-and-switch () (interactive) (split-window-right) (other-window 1))

(defun me/toggle-show-trailing-whitespace ()
  "Toggle the value of buffer-local variable show-trailing-whitespace between
  nil and t. (For some reason, Emacs doesn't have this function build in, nor
  does it have a show-trailing-whitespace-mode.) It's default is nil, so adding
  this function as a hook for a programming mode will have the effect of turning
  it on more easily; e.g.:
      (add-hook 'ruby-mode-hook 'me/toggle-show-trailing-whitespace) ; Just Ruby
      (add-hook 'prog-mode-hook 'me/toggle-show-trailing-whitespace) ; All Programming Modes"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))


(defun me/show-project-relative-path-in-modeline ()
  "Show the name of the buffer relative to any project. Combine with
       (use-package spaceline :config (spaceline-spacemacs-theme)) for a nice
       vim-like modeline from Spacemacs. Adapted from:
       https://www.reddit.com/r/emacs/comments/8xobt3/tip_in_modeline_show_buffer_file_path_relative_to/"
  (interactive)
  (with-eval-after-load 'subr-x
    (setq-default
     mode-line-buffer-identification
     '(:eval
       (format-mode-line
        (propertized-buffer-identification
         (or (when-let*
                 ((buffer-file-truename buffer-file-truename)
                  (prj (cdr-safe (project-current)))
                  (prj-parent (file-name-directory (directory-file-name (expand-file-name prj)))))
               (concat (file-relative-name (file-name-directory buffer-file-truename) prj-parent)
                       (file-name-nondirectory buffer-file-truename)))
             "%b")))))))

(defun me/soft-wrap-this-buffer ()
  "Toggle soft wrap paragraphs/long lines. Does not edit the actual text
in the buffer, just makes it look like it's been filled. Adapted
from https://vlevit.org/en/blog/tech/visual-line-wrap"
  (interactive)
  (if visual-line-mode
      (progn
        (evil-emacs-state)
        (visual-fill-column-mode 0)
        (visual-line-mode 0)
        (evil-normal-state))
    (progn
      (evil-emacs-state)
      (visual-line-mode 1)
      (visual-fill-column-mode 1)
      (evil-normal-state))))

(defun me/async-shell-command-on-region-keep-files ()
  (interactive)
  (let* ((date-string (s-chomp (shell-command-to-string "date +'%Y%m%d-%H%M%S'")))
         ;; (shell-cmd "upcase")
         (shell-cmd (read-shell-command "Async shell command: "))
         (src-file (concat (expand-file-name "~/tmp/async-") date-string "-src"))
         (tgt-file (concat (expand-file-name "~/tmp/async-") date-string "-tgt"))
         (async-shell-command-display-buffer nil)
         (auto-revert-interval 1)
         )
    (shell-command (concat "touch " src-file " " tgt-file))
    (save-excursion
      (unless (region-active-p) (mark-paragraph))
      (write-region (region-beginning) (region-end) src-file))
    (async-shell-command (concat "< " src-file " " shell-cmd " > " tgt-file))
    (delete-other-windows)
    (split-window-below)
    (other-window 1)
    (find-file tgt-file)
    (setq-local truncate-lines t)
    (auto-revert-mode)
    (other-window 1)))

(defun me/disable-all-themes ()
  "Yes, disable all the themes in custom-enabled-themes"
  (interactive)
  (mapc 'disable-theme custom-enabled-themes))

(defun me/dark-theme ()
  (interactive)
  "For working indoors or at night."
  (me/disable-all-themes)
  (load-theme 'monokai t)
  ;;; Cursor colors that indicate Evil state:
  (setq evil-normal-state-cursor '(box       "chartreuse")
        evil-insert-state-cursor '((bar . 2) "yellow")
        evil-visual-state-cursor '(box       "deep pink")
        evil-emacs-state-cursor  '((bar . 3) "purple"))
  (me/set-up-theme)
  )

(defun me/light-theme ()
  "For working outdoors or in a bright room. spacemacs-light is a sepia theme found at https://github.com/nashamri/spacemacs-theme"
  (interactive)
  (me/disable-all-themes)
  (progn
    ;; For some reason (use-package spacemacs-theme...) fails. So doing it this way.
    (package-install 'spacemacs-theme)
    (load-theme 'spacemacs-light t))
  (set-face-attribute 'shadow nil  :foreground "dark blue")
  (set-face-attribute 'font-lock-comment-face nil  :foreground "dark blue")
  (setq evil-normal-state-cursor '(box       "blue")
        evil-insert-state-cursor '((bar . 2) "red")
        evil-visual-state-cursor '(box       "deep pink")
        evil-emacs-state-cursor  '((bar . 3) "purple"))
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :weight 'bold :foreground "deep pink")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :weight 'bold :foreground "forest green")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :weight 'bold :foreground "dark blue")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :weight 'bold :foreground "dark red")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :weight 'bold :foreground "orchid")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :weight 'bold :foreground "blue")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :weight 'bold :foreground "dark green")
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil :weight 'bold :foreground "deep pink"))

(defun me/text-scale-adjust-down-1 ()
  "Quick text scale adjust -1. Only do this when window-system."
  (interactive)
  (when window-system (text-scale-adjust -1)))

(defun me/text-scale-adjust-down-2 ()
  "Quick text scale adjust -2. Only do this when window-system."
  (interactive)
  (when window-system (text-scale-adjust -2)))

(defun me/insert-uuid ()
  "Insert a lowercased uuid without dashes at point. Uses uuidgen
system command."
  (interactive)
  (let* ((uuid (string-trim (shell-command-to-string "uuidgen | tr A-Z a-z | tr -d -"))))
    (insert uuid)))

(defun me/vterm-dd3 ()
  (interactive)
  (let* ((vterm-shell "ssh dd3"))
    (vterm)))

(defun me/vterm-dd3 ()
  (interactive)
  (let* ((vterm-shell "ssh dd3"))
    (vterm)))

(defun me/vterm-dd3-htop ()
  (interactive)
  (let* ((vterm-shell "ssh dd3 -t -- htop"))
    (vterm)))
