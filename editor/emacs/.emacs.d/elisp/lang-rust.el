
;;; Rust Configure
(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'rustfmt-enable-on-save)

  (use-package cargo
    :ensure t
    :config
    (add-hook 'rust-mode-hook 'cargo-minor-mode))
  
  (use-package flycheck-rust
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))


(provide 'lang-rust)