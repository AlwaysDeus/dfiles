(display-time-mode 1)
(display-battery-mode 1)

(use-package nyan-mode
  :ensure t
  :init (nyan-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (setq doom-modeline-hight 10
                doom-modeline-time 1
                doom-modeline-time-icon 'nil
                doom-modeline-buffer-file-name-style 'truncate-nil
                doom-modeline-checker-simple-format 'nil))

(use-package hide-mode-line
  :ensure t
  :config
  (global-set-key (kbd "C-M-<return>") 'hide-mode-line-mode))

;; Diminish lets you hide minor modes from showing in the mode line,
;; keeping it minimal.
(use-package diminish
  :ensure t)

;(use-package parrot
;  :config
;  (parrot-mode))

(provide 'vamp-modeline)
