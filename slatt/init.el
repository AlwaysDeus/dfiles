;;; init.el --- Personal configuration file -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(dolist (path '("vamp-modules"))
  (add-to-list 'load-path (locate-user-emacs-file path)))
(require 'package)

(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("elpa" . 2)
        ("nongnu" . 1)))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;---------------------------------------------------------------------------------------------------
;;===================================================================================================
;;---------------------------------------------------------------------------------------------------
(require 'vamp-essentials)
; Todo: bookmarks
(require 'vamp-font)
(require 'vamp-evil)
; Todo: evil-redo
(require 'vamp-modeline)
(require 'vamp-look)         ; rainbow-mode, rainbow-delimiters, theme, centered-window, all-the-icons
; ?Todo: change theme colors
(require 'vamp-dashboard)
(require 'vamp-completion)   ; ivy, counsel, company, which-key, flycheck
; Todo: Prots dired easy fiding
(require 'vamp-org)
; Todo: org-agenda customization,
(require 'vamp-filebrowser)  ; ranger, treemacs

;; SCHOOL
;(require 'vamp-school)
    ;https://libreddit.dcs0.hu/r/emacs/comments/jkbvb5/equations_in_emacs/
    ;https://lifeofpenguin.blogspot.com/2021/11/formula-editor-in-gnu-emacs.html
;Circuits
    ;http://tiagoweber.github.io/blog/entry1.html
;Drawing
    ;https://github.com/misohena/el-easydraw
    ;https://lifeofpenguin.blogspot.com/2021/08/scribble-notes-in-gnu-emacs.html

    ;http://127.0.0.1:9010/https://www.youtube.com/playlist?list=PLW9poAEUvGDDxCZX-3xIQ3Wb1HOVcg7N_
;Artist mode
    ;http://127.0.0.1:9010/https://www.youtube.com/watch?v=cIuX87Xo8Fc

;(require 'vamp-rss)


;;; Init.el ends here
