;;; package --- Summary:
;;; Commentary:

(use-package python
  :ensure t
  :init
  (add-hook 'python-mode-hook 'python-doc)
  (setq python-indent-guess-indent-offset-verbose nil
        python-shell-interpreter "python"))

(use-package anaconda-mode
  :ensure t
  :after python
  :bind (("M-s" . anaconda-mode-find-definitions))
  :config
  ;; trim eldoc to fit the frame
  (setq anaconda-mode-eldoc-as-single-line t)
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'anaconda-mode-hook #'anaconda-eldoc-mode))

(use-package company-anaconda
  :ensure t
  :after
  anaconda-mode
  company
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package pip-requirements
  :ensure t
  :mode ("/requirements.txt$" . pip-requirements-mode))

(use-package pyvenv
  :ensure t
  :config
  (evil-leader/set-key-for-mode 'python-mode
    "pw" 'pyvenv-workon
    "pd" 'pyvenv-deactivate))

(defun python-format-buffer ()
  "Format python buffer using yapify and isort."
  (interactive)
  (yapfify-buffer (point-min) (point-max))
  (py-isort-buffer))

; yapfify
(use-package yapfify :defer t)

; py-isort
(use-package py-isort :defer t)

(evil-leader/set-key-for-mode 'python-mode
  "=" 'python-format-buffer)


(provide 'lang-py)
