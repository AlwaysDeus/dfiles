(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  ;; Set the title
  ;(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; Set the banner
  (setq dashboard-startup-banner "~/Downloads/playboi_carti/narcissist_new.png"
        dashboard-center-content t
        dashboard-image-banner-max-height 500
        dashboard-image-banner-max-width 500
        dashboard-set-heading-icons t
        dashboard-set-file-icons t))

(provide 'vamp-dashboard)
