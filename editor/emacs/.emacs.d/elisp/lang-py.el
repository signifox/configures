;;; package --- Summary:
;;; Commentary:

(use-package auto-virtualenv
   :ensure t
   :config
   (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv))

(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup))

(use-package elpy
  :ensure t
  :init
  (setq elpy-rpc-backend "jedi")
  (elpy-enable)
  :config
  (add-hook 'python-mode-hook 'elpy-mode)
  (with-eval-after-load 'elpy
    (add-hook 'elpy-mode-hook 'elpy-use-ipython))

  (setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")
  
  :bind (("M-*" . pop-tag-mark)))

(use-package indent-tools
  :ensure t
  :init
  (add-hook 'python-mode-hook
            (lambda () (define-key python-mode-map (kbd "C-c i") 'indent-tools-hydra/body))))

(provide 'lang-py)
