;; NOTE: The first time you load your configuration on a new machine,
;; you'll need to run:
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;;(load-theme 'doom-outrun-electric t)  ; OG PURPLE
  ;(load-theme 'doom-moonlight t)        ; OG Purple
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;(doom-themes-neotree-config)
  ;; or for treemacs users
  ;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package rainbow-mode
  :ensure t
  :config
  (custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
                    '(rainbow-delimiters-depth-2-face ((t (:foreground "yellow"))))
                    '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))))

(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                (list 'org-agenda-mode)))
     (rainbow-mode 1))))
(global-rainbow-mode 1 )

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package centered-window
  :ensure t
  :init (centered-window-mode t)
        (load-theme 'doom-shades-of-purple)   ; OG Purple
  :config
  ;; Alt-Enter toggle
  (global-set-key (kbd "M-<RET>") 'centered-window-mode))
;; Disable centered-window-mode
(dolist (mode '(term-mode-hook
                eshell-mode-hook
                ranger-mode-hook))
  (add-hook mode (lambda () (centered-window-mode 0))))

(provide 'vamp-look)
