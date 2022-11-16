;(setq inhibit-startup-message t)
;(setq inhibit-startup-screen t)

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;; Remove security vulnerability
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(require 'cl-lib)
;(require 'loadhist)
;(file-dependents (feature-file 'cl))

;(require 'server)
;(if (not (server-running-p)) (server-start))

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(tooltip-mode -1)       ; Disable tooltip
(menu-bar-mode -1)      ; Disabale the menu bar
(blink-cursor-mode -1)  ; Disabale cursor blinking
(setq use-dialog-box t) ; only for mouse events


(setq visible-bell t)   ; Set the visible bell
(global-hl-line-mode 1) ; Hilight current line
;; Show line number
(line-number-mode +1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(column-number-mode t)
(size-indication-mode t)
(setq truncate-lines 1)  ; Disable line wrapping

(setq scroll-margin 5
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; Only y/n answers
(setq use-short-answers t)

(global-auto-revert-mode t)

(setq-default tab-width 4
              indent-tabs-mode nil)


;; Clean white spaces
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Buckups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; All variables are for Emacs 28+
(setq package-name-column-width 40)
(setq package-version-column-width 14)
(setq package-status-column-width 12)
(setq package-archive-column-width 8)
(add-hook 'package-menu-mode-hook #'hl-line-mode)

;;======================================================================================================
;; KEYS
;;======================================================================================================
;(recentf-mode 1)
; M-x recentf-open-files ; then press the number

;; Kill current buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(provide 'vamp-essentials)
