(defun counsel-locate-action-extern (x)
  "Pass X to `wsl-open' or equivalent command via the shell."
  (interactive "FFile: ")
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (w32-shell-execute "open" x)
    (start-process-shell-command shell-file-name nil
                                 (format "%s %s"
                                         "wsl-open"
                                         (shell-quote-argument x)))))

(setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
      browse-url-generic-args    '("/c" "start")
      browse-url-browser-function 'browse-url-generic)
