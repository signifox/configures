;;; Commentary:

;;(use-package dracula-theme
;;  :ensure t
;;  :init
;;  (load-theme 'dracula t))

(use-package powerline :ensure t)

(use-package darkroom :ensure t)

(use-package all-the-icons :ensure t)

(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one t)
  :config
  (progn
    (doom-themes-neotree-config)
    (doom-themes-org-config)))

(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-init))

(provide 'base-theme)
