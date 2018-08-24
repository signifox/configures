;;; package --- Summary:

(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)
  (add-hook 'c-mode-hook #'company-mode)
  (add-hook 'c++-mode-hook #'company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil))
  :diminish company-mode)

(use-package irony
  :ensure t
  :defer t
  :config
  (add-hook 'irony-mode-hook #'electric-pair-mode)
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands)
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

  (use-package company-irony
      :ensure t
      :config
      (add-to-list 'company-backends 'company-irony))

  (use-package company-irony-c-headers
  :ensure t
  :init
  (add-to-list  'company-backends '(company-irony-c-headers))))

(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style (concat "{BasedOnStyle: Google}"))
  :commands clang-format clang-format-buffer clang-format-region)

(provide 'lang-cc)
