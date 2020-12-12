;;; Functions to help me get to current and other dated daily scratchpads quickly

(setq me/scratchpad-directory "~/scratch")

(defun me/scratchpad-open ()
  (interactive)
  (let* ((date-str (format-time-string "%Y-%m-%d_%a"))
         (path (format "%s/%s/pad.txt" me/scratchpad-directory date-str)))
    (find-file path)
    (save-buffer)
    (auto-save-mode 1)
    (message (format "Today's scratchpad, saved, with Auto Save Mode enabled. Path: %s" path))))

(defun me/scratchpad-open-other-date ()
  (interactive)
  (let* ((dated-dirs (reverse (directory-files me/scratchpad-directory)) )
         (chosen-dir (completing-read "Pick date to jump to that scratchpad: " dated-dirs ))
         (path (format "%s/%s/%s" me/scratchpad-directory chosen-dir "pad.txt")))
    (find-file path)))
