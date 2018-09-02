;;; Commentary:

(use-package dracula-theme
  :ensure t
  :init
  (load-theme 'dracula t))

(use-package powerline :ensure t)

(use-package darkroom :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
)

(provide 'base-theme)
