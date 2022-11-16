;; Treemacs
(use-package treemacs
  :ensure t
  :config
  (setq treemacs-is-never-other-window t)
  (global-set-key (kbd "<f5>") 'treemacs)
  (global-set-key (kbd "C-<f5>") 'treemacs-select-window))

;; Ranger
(use-package ranger
  :ensure t
  :config
  (global-set-key (kbd "C-<return>") 'ranger)
  (setq ranger-show-hidden t))


(provide 'vamp-filebrowser)
