;;; Package --- Summary
;;; Commentary:

;;; Code:
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(show-paren-mode 1)
(global-hl-line-mode 1)
(display-time-mode 1)
;;(global-linum-mode t)
(setq inhibit-startup-screen t)
(global-auto-revert-mode 1)

(setq column-number-mode t)
(setq line-number-mode t)
(setq display-time-24hr-format t)
(setq require-final-newline t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq show-paren-style 'parenthesis)
(setq show-paren-style 'expression)
(setq show-paren-style 'mixed)

(setq max-mini-window-height 0.8)
(setq-default indent-tabs-mode nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(fset 'yes-or-no-p 'y-or-n-p)
(setq system-time-locale "en_US.utf8")
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;;;; package.el
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")

;;(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(defun vsplit-last-buffer ()
  "Vsplit."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(defun hsplit-last-buffer ()
  "Hsplit."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" "233bb646e100bda00c0af26afe7ab563ef118b9d685f1ac3ca5387856674285d" "7d2e7a9a7944fbde74be3e133fc607f59fdbbab798d13bd7a05e38d35ce0db8d" default)))
 '(package-selected-packages
   (quote
    (window-numbering doom-modeline rainbow-mode darkroom hydra dracula-theme company-irony-c-headers editorconfig ace-window ibuffer-sidebar imenu-anywhere hl-todo yasnippet clang-format avy dired-sidebar yasnippet-snippets flycheck company-irony irony company smex flx gitignore-mode magit counsel-gtags counsel swiper ivy powerline-evil evil-leader evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Hack")
  (set-face-attribute 'default nil :height 165))

(defun vreload()
  "Reload."
  (interactive)
  (load-file user-init-file))

(use-package flx :ensure t)
(use-package smex :ensure t)
(use-package darkroom :ensure t)
(use-package editorconfig :ensure t)
(use-package rainbow-mode :ensure t)
(use-package powerline :ensure t)

;; Evil Mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode t)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "ff" 'find-file
      "fr" 'recentf-open-files
      "bb" 'switch-to-buffer
      "bk" 'kill-buffer
      "gt" 'magit-status
      "dr" 'darkroom-mode
      "w/" 'split-window-right
      "w-" 'split-window-below
      ":"  'counsel-M-x
      "wm" 'delete-other-windows)))

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
  (setq ivy-format-function 'ivy-format-function-arrow))

(use-package counsel-gtags
  :init
  (add-hook 'c-mode-hook 'counsel-gtags-mode)
  (add-hook 'c++-mode-hook 'counsel-gtags-mode)
  :bind
  ("M-t" . counsel-gtags-find-definition)
  ("M-r" . counsel-gtags-find-reference)
  ("M-s" . counsel-gtags-find-symbol)
  ("M-," . counsel-gtags-go-backward))

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
  :bind (("C-x g" . magit-status)))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

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

(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style (concat "{BasedOnStyle: Google}"))
  :commands clang-format clang-format-buffer clang-format-region)

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
  :bind
  ("M-/" . company-complete-common)
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

(use-package flycheck
  :ensure t
  :defer t
  :init
  (global-flycheck-mode)
  :config
  (progn
    (add-hook 'c++-mode-hook #'flycheck-mode)
    (add-hook 'c-mode-hook #'flycheck-mode)))

(use-package avy
  :ensure t
  :bind
  ("C-'" . avy-goto-char)
  ("C-;" . avy-goto-char-2))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode)
  :config
  (yas-reload-all)

  (use-package yasnippet-snippets
   :ensure t))

(use-package dracula-theme
  :ensure t
  :init
  (load-theme 'dracula t))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "-+-")
  (setq dired-sidebar-use-term-integration t))

(use-package ibuffer-sidebar
  :ensure t
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140)))

(defun vsidebar()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

(defun vshell ()
  "Eshell Here."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(use-package term
  :ensure t
  :bind (("C-c t" . term)
         :map term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         :map term-raw-map
         ("M-o" . other-window)
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)))

(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-init))

;;; Rust Configure
(use-package rust-mode
  :ensure t
  )

(use-package cargo
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  )

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  )


;;;; global key bindings
(global-set-key (kbd "<f3>")  'imenu-anywhere)
(global-set-key (kbd "<f4>")  'counsel-find-file)
(global-set-key (kbd "<f5>")  'clang-format-buffer)
(global-set-key (kbd "<f6>")  'counsel-rg)
(global-set-key (kbd "<f7>")  'avy-goto-char-2)
(global-set-key (kbd "<f8>")  'vsidebar)
(global-set-key (kbd "<f9>")  'darkroom-mode)

;;; init.el ends here

