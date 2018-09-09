;;; Package --- Summary
;;; Commentary:

;;; Code:
(use-package dracula-theme       :ensure t :defer t)
(use-package atom-one-dark-theme :ensure t :defer t)
(use-package darkokai-theme      :ensure t :defer t)
(use-package github-theme        :ensure t :defer t)
(use-package monokai-theme       :ensure t :defer t)
(use-package paganini-theme      :ensure t :defer t)
(use-package solarized-theme     :ensure t :defer t)
(use-package sublime-themes      :ensure t :defer t)
(use-package zenburn-theme       :ensure t :defer t)

(use-package powerline :ensure t)

(use-package darkroom :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (agenda . 5)
                        (projects . 5))))

(provide 'base-theme)
