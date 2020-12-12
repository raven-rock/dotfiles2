;;; For better ergonomics on macOS GUI Emacs, make Command key send ^Control, and Control key send Command. For the same
;;; effect in terminal Emacs, use iTerm2 configure Preferences > Remap
;;; Modifier Keys

(when (or (eq (window-system) 'ns) (eq (window-system) 'mac))
  (setq mac-control-modifier 'super)
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier 'meta)
  ;; (global-set-key (kbd "C-h") (prefix-command-update))
  )

