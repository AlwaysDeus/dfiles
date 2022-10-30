;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Font
(setq doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'semi-light))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))
(setq org-hide-emphasis-markers t)

;;; Theme
(setq doom-theme 'doom-one)

;;; Line
;;; Relative line number
(setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)
;;; Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)
;; Also in visual mode
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)

;;; Org mode
(setq org-directory "~/org/")
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;;; Org agenda
(setq org-agenda-files '("~/org/tasks.org"))
;;; Org babel -- hide codeblock info
;;; or https://github.com/amno1/org-babel-hide-markers-mode
;(let ((background-color (face-attribute 'default :background)))
;  (set-face-attribute 'org-block-begin-line nil
;                      :foreground background-color
;                      :background background-color))
;;; Org roam directory
(setq org-roam-directory "~/org/notes")
;;; Org-download
(setq-default org-download-image-dir "~/org/notes/img")

;;; Org tangle
(require 'org-auto-tangle)
(add-hook 'org-mode-hook 'org-auto-tangle-mode)
(setq org-auto-tangle-default t)

;;; Org hide codeblock info
;(let ((background-color (face-attribute 'default :background)))
;  (set-face-attribute 'org-block-begin-line nil
;                      :foreground background-color
;                      :background background-color))

;;; Org headings
;;(use-package org-bullets
;;  :after org
;;  :hook (org-mode . org-bullets-mode)
;;  :custom
;;  (org-bullets-bullet-list '("●" "○" "●" "○" "●" "○" "●")))

;;; Org headline
(after! org
  (set-face-attribute 'org-link nil
                      :weight 'normal
                      :background nil)
  (set-face-attribute 'org-code nil
                      ;;:foreground "#a9a1e1"
                      :background nil)
  (set-face-attribute 'org-date nil
                      :background nil)
  (set-face-attribute 'org-level-1 nil
                      :background nil
                      :height 1.5
                      :weight 'normal)
  (set-face-attribute 'org-level-2 nil
                      :background nil
                      :height 1.2
                      :weight 'normal)
  (set-face-attribute 'org-level-3 nil
                      :background nil
                      :height 1.0
                      :weight 'normal)
  ;;; Move headlines using ALT j/k
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)

  ;;; Types of tasks and customization
  (setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" " CANCELLED(c)"))
        org-todo-keyword-faces
        '(("TODO" :foreground "#ff0000" :weight normal :underline t)
          ("WAITING" :foreground "#9f7efe" :weight normal :underline t)
          ("IN-PROGRESS" :foreground "#551a8b" :weight normal :underline t)
          ("DONE" :foreground "#50a14f" :weight normal :underline t)
          ("CANCELLED" :foreground "#666666" :weight normal :underline t)))
;;  (setq org-todo-keywords
;;        '((sequence "TODO(t)" "WAITING(w)" "IN-PROGRESS(i)"  "|" "DONE(d)" "CANCELLED(c)")))
;;  (setq org-todo-keyword-faces
;;        '(("TODO" . "#ff0000") ("WAITING" . "#9f7efe") ("IN-PROGRESS" . "#551a8b") ("CANCELLED" . "#50a14f") ("DONE" . "#666666"))))
  ;;; Priority colors
  (setq org-priority-faces
        '((?A :foreground "#ff0000")
          (66 :foreground "#ffa500")
          (67 :foreground "#0098dd")
          (68 :foreground "green")))
  ;;; Priority look
  (use-package org-fancy-priorities
    :hook
    (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL"))))

;;; RAINBOW MODE hex values in actual colors
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                (list 'org-agenda-mode)))
     (rainbow-mode 1))))
(global-rainbow-mode 1 )

;;; Centered mode
;(require 'centered-window-mode)
(centered-window-mode t)
;;; Alt-Enter toggle
(global-set-key (kbd "M-<RET>") 'centered-window-mode)

;;; dispaly images (keybind "zi" toggle)
(setq org-startup-with-inline-images t)

;;; Centaur-tabs
(centaur-tabs-mode t)
(setq centaur-tabs-style "bar"
      centaur-tabs-set-icons t
      centaur-tabs-set-modified-marker t
      centaur-tabs-modified-marker "*")
(centaur-tabs-headline-match)
;;; Change tabs Ctrl-PgUp/PgDn
(global-set-key (kbd "C-<prior>") 'centaur-tabs-backward)
(global-set-key (kbd "C-<next>") 'centaur-tabs-forward)
;;; Change tabs Vim sytle
(define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward)
(define-key evil-normal-state-map (kbd "g T") 'centaur-tabs-backward)

;;; Treemacs
(setq treemacs-is-never-other-window t)
(global-set-key (kbd "<f5>") 'treemacs)
(global-set-key (kbd "C-<f5>") 'treemacs-select-window)

;;; Multiple-cursors
;(setq global-evil-mc-mode t)
;(global-set-key (kbd "C->") 'mc/mark-next-like-this)
;(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; RSS
;(load "~/org/rss.el")
