;; Font
(set-face-attribute 'default nil :family "JetBrains Mono")
(set-fontset-font t 'symbol "Symbola")

(use-package unicode-fonts
   :ensure t
   :config
    (unicode-fonts-setup))

(use-package ligature
  :ensure t
  :load-path "path-to-ligature-repo"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
  '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>" ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!==" "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<" "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->" "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<" "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~=" "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|" "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:" ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:" "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!" "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:" "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)" "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do
  ;; it per mode with `ligature-mode'.
  (global-ligature-mode t))

(provide 'vamp-font)
