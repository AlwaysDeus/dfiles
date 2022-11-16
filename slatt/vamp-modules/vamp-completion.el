(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill))
         ;:map ivy-reverse-i-search-kill
         ;("C-k" . ivy-previous-line)
         ;("C-d" . ivy-reverse-i-search-kill))
  :config (ivy-mode 1))

;(use-package ivy-rich
;    :init
;    (ivy-rich-mode 1))
(require 'ivy-rich)
(ivy-rich-mode 1)

(use-package counsel
  :ensure t
  :bind ("C-M-j" . 'counsel-switch-buffer)
         ;:map minibuffer-local-map
         ;("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

;; Shows keybindings
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Autocomplete
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook #'global-company-mode))

;; Syntax checking
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Spell checking
(use-package flycheck-aspell
  :ensure t)
;; Ensure `flycheck-aspell' is available
(require 'flycheck-aspell)
;; If you want to check TeX/LaTeX/ConTeXt buffers
(add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
;; If you want to check Markdown/GFM buffers
(add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
;; If you want to check HTML buffers
(add-to-list 'flycheck-checkers 'html-aspell-dynamic)
;; If you want to check XML/SGML buffers
(add-to-list 'flycheck-checkers 'xml-aspell-dynamic)
;; If you want to check Nroff/Troff/Groff buffers
(add-to-list 'flycheck-checkers 'nroff-aspell-dynamic)
;; If you want to check Texinfo buffers
(add-to-list 'flycheck-checkers 'texinfo-aspell-dynamic)
;; If you want to check comments and strings for C-like languages
(add-to-list 'flycheck-checkers 'c-aspell-dynamic)
;; If you want to check message buffers
(add-to-list 'flycheck-checkers 'mail-aspell-dynamic)
;; Because Aspell does not support Org syntax, the user has
;; to define a checker with the desired flags themselves.
(flycheck-aspell-define-checker "org"
  "Org" ("--add-filter" "url")
  (org-mode))
(add-to-list 'flycheck-checkers 'org-aspell-dynamic)

(setq ispell-dictionary "polish")
(setq ispell-program-name "aspell")
(setq ispell-silently-savep t)

;You may also want to advice ispell-pdict-save to refresh flycheck when
;inserting new entries into your local dictionary. This way highlighting
;instantly updates when you add a previously unknown word.
(advice-add #'ispell-pdict-save :after #'flycheck-maybe-recheck)
(defun flycheck-maybe-recheck (_)
  (when (bound-and-true-p flycheck-mode)
   (flycheck-buffer)))

; Cycle through different languages
(let ((langs '("english" "polish")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(global-set-key [f6] 'cycle-ispell-languages)


(provide 'vamp-completion)
