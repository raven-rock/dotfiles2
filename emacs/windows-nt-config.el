;;; Use Unix's \n (LF- Line Feed) and utf instead of Windows \r\n
;;; (CRLF - Carriage Return and Line Feed) as end of line
;;; character. It may not be desirable if most files or project edited
;;; are for Windows or building tools that may fail if the source file
;;; doens't CRLF as line ending.  UTF-8 Everywhere!  Reference:
;;; http://caiorss.github.io/Emacs-Elisp-Programming/Emacs_On_Windows.html
(progn
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (set-terminal-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8))

;; Make Windows programs available to execute from M-& or evil ex.
(setenv  "PATH" (concat
                 ;; "c:/Windows/System32" ";" 
                 "c:/Windows/Microsoft.NET/Framework/v4.0.30319" ";"
                 "C:\\Windows\\Microsoft.NET\\Framework\\v4.0.30319" ";"
                 ;; Unix tools 
                 "C:\\Program Files\\Git\\usr\\bin" ";"
                 ;; User binary files 
                 "C:\\User\\arch\\bin" ";"
                 ;; Mono Installation.
                 "c:\\Program Files\\Mono\\bin" ";"
                 "c:\\Program Files\\Mono\\lib\\mono\\4.5" ";"
                 ;; Whatever was already in PATH
                 (getenv "PATH")
                 ))

(defun run-bash ()
  (interactive)
  (let ((shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe"))
    (shell "*bash*")))

(defun run-cmdexe ()
  (interactive)
  (let ((shell-file-name "cmd.exe"))
    (shell "*cmd.exe*")))

(defun run-powershell ()
  "Run powershell"
  (interactive)
  (async-shell-command "c:/windows/system32/WindowsPowerShell/v1.0/powershell.exe"
                       nil
                       nil))
