;; Org
(setq org-directory "~/death_note")
(setq org-agenda-files "~/death_note/agenda.org")
(use-package org-download
  :ensure t
  :config
  (setq-default org-download-image-dir "~/death_note/.img"))

;; dispaly images (keybind "zi" toggle)
(setq org-startup-with-inline-images t)

(use-package org-auto-tangle
  :ensure t
  :init (setq org-auto-tangle-default t)
  :hook (org-mode . org-auto-tangle-mode))

;; Org hide codeblock info
;(let ((background-color (face-attribute 'default :background)))
;  (set-face-attribute 'org-block-begin-line nil
;                      :foreground background-color
;                      :background background-color))

;; Org headings
(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "○" "●" "○" "●" "○" "●")))

;; Org headline
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
;; Move headlines using ALT j/k
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "M-k") #'org-metaup)
                     (local-set-key (kbd "M-j") #'org-metadown)))
;; Types of tasks and customization
(setq org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" " CANCELLED(c)"))
      org-todo-keyword-faces
      '(("TODO" :foreground "#ff0000" :weight normal :underline t)
        ("WAITING" :foreground "#9f7efe" :weight normal :underline t)
        ("IN-PROGRESS" :foreground "#551a8b" :weight normal :underline t)
        ("DONE" :foreground "#50a14f" :weight normal :underline t)
        ("CANCELLED" :foreground "#666666" :weight normal :underline t)))
;  (setq org-todo-keywords
;        '((sequence "TODO(t)" "WAITING(w)" "IN-PROGRESS(i)"  "|" "DONE(d)" "CANCELLED(c)")))
;  (setq org-todo-keyword-faces
;        '(("TODO" . "#ff0000") ("WAITING" . "#9f7efe") ("IN-PROGRESS" . "#551a8b") ("CANCELLED" . "#50a14f") ("DONE" . "#666666"))))
;; Priority colors
(setq org-priority-faces
      '((?A :foreground "#ff0000")
        (66 :foreground "#ffa500")
        (67 :foreground "#0098dd")
        (68 :foreground "green")))
;; Priority look
(use-package org-fancy-priorities
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL")))

;;======================================================================================================
;; Org-agenda
;http://www.cachestocaches.com/2016/9/my-workflow-org-agenda/

;; Org-capture
;; Define the custum capture templates
(setq org-capture-templates
       '(("t" "todo" entry (file org-default-notes-file)
          "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
         ("m" "Meeting" entry (file org-default-notes-file)
          "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
         ("d" "Diary" entry (file+datetree "~/death_note/diary.org")
          "* %?\n%U\n" :clock-in t :clock-resume t)
         ("i" "Idea" entry (file org-default-notes-file)
          "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
         ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
          "** NEXT %? \nDEADLINE: %t")))
(local-set-key (kbd "C-c c") 'org-capture)

;; Set default column view headings: Task Total-Time Time-Stamp
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
;Once you've included this, activate org-columns with C-c C-x C-c while on a top-level heading,
;which will allow you to view the time you've spent at the different levels

(provide 'vamp-org)
