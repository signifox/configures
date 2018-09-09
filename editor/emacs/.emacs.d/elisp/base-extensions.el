;;; package --- Summary

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
  ;; allow input not in order
  '((t . ivy--regex-ignore-order))))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x C-r" . counsel-recentf)
  ("C-x j" . counsel-mark-ring)
  ("C-c L" . counsel-load-library)
  ("C-c P" . counsel-package)
  ("C-c f" . counsel-find-library)
  ("C-c g" . counsel-grep)
  ("C-c h" . counsel-command-history)
  ("C-c i" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c l" . counsel-locate)
  ("C-c r" . counsel-rg)
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :config
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)`
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function nil)
  (setq ivy-format-function 'ivy-format-function-arrow)

  (use-package counsel-gtags
  :init
  (add-hook 'c-mode-hook 'counsel-gtags-mode)
  (add-hook 'c++-mode-hook 'counsel-gtags-mode)
  :bind
  ("M-t" . counsel-gtags-find-definition)
  ("M-r" . counsel-gtags-find-reference)
  ("M-s" . counsel-gtags-find-symbol)
  ("M-," . counsel-gtags-go-backward)))

(use-package flycheck
  :ensure t
  :defer t
  :init
  (global-flycheck-mode)
  :config
  (progn
    (add-hook 'c++-mode-hook #'flycheck-mode)
    (add-hook 'c-mode-hook #'flycheck-mode)
    (add-hook 'python-mode-hook #'flycheck-mode)))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode)
  :config
  (yas-reload-all)

  (use-package yasnippet-snippets
   :ensure t))

(use-package hydra
  :ensure t
  :config
  (setq hydra-verbose nil))

(use-package recentf
  :ensure t
  :bind (("C-x f" . recentf-open-files))
  :config
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1)
  (setq recentf-max-saved-items 99)
  (setq recentf-max-menu-items 99)
  (setq recentf-show-file-shortcuts-flag nil)
  (setq recentf-exclude
        '("COMMIT" "autoloads" "archive-contents" "eld" "newsrc"
          ".recentf" "emacs-font-size.conf"))
  (add-hook 'find-file-hook #'recentf-save-list))

(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ;; Magic
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive))

(use-package autorevert
  :ensure t
  :diminish auto-revert-mode
  :config (global-auto-revert-mode))

(use-package avy :ensure t)

(use-package smartparens :ensure t)

(use-package markdown-mode :ensure t)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

(use-package window-numbering
  :ensure t
  :init
  (progn
    (window-numbering-mode t)))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1))

(use-package counsel-projectile
  :ensure t
  :bind
  (("C-c p p" . counsel-projectile-switch-project)
   ("C-c p b" . counsel-projectile-switch-to-buffer)
   ("C-c p f" . counsel-projectile-find-file))
  :config
  (counsel-projectile-on))

(provide 'base-extensions)
