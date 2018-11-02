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
(global-linum-mode t)
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
(setq tramp-verbose 10)
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
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-keys (quote (97 115 100 102 103 104 106 107 108)) t)
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(package-selected-packages
   (quote
    (company-irony flycheck-irony irony counsel-gtags which-key youdao-dictionary powerline-evil evil-leader htmlize monokai-theme rainbow-delimiters ace-window window-numbering projectile smartparens rainbow-mode darkroom editorconfig yasnippet yasnippet-snippets clang-format avy dired-sidebar flycheck company smex magit counsel swiper ivy evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(set-face-attribute 'default nil :height 180)

(use-package smex                :ensure t)
(use-package editorconfig        :ensure t)
(use-package avy                 :ensure t)
(use-package smartparens         :ensure t)
(use-package rainbow-delimiters  :ensure t)
(use-package darkroom            :ensure t)
(use-package monokai-theme       :ensure t)


;; Evil Mode
(use-package evil
  :ensure t
  :config
  (define-key evil-insert-state-map [remap newline] 'newline)
  (define-key evil-insert-state-map [remap newline-and-indent] 'newline-and-indent)
  (define-key evil-ex-map "e " 'find-file)
  (define-key evil-ex-map "b " 'switch-to-buffer)

  (use-package evil-leader
    :ensure t
    :init (global-evil-leader-mode)
    :config
    (progn
      (setq evil-leader/in-all-states t)
      (evil-leader/set-leader "<SPC>")
      (evil-leader/set-key
        "A" 'ag
        "b" 'ivy-switch-buffer
        "g" 'counsel-rg
        "x" 'smex)))
  (evil-mode 1)
  ;; remove all keybindings from insert-state keymap, use emacs-state when editing
  (setcdr evil-insert-state-map nil)

  (use-package powerline-evil
    :ensure t
    :config
    (powerline-evil-vim-color-theme))

  ;; ESC to switch back normal-state
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  ;; esc should always quit: http://stackoverflow.com/a/10166400/61435
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
  (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)
  (global-set-key [escape] 'evil-exit-emacs-state))


;;Extensions
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
     :ensure t
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
    (add-hook 'c-mode-hook #'flycheck-mode)
    (add-hook 'c++-mode-hook #'flycheck-mode)
    (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
    (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
    (add-hook 'python-mode-hook #'flycheck-mode)))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode)
  :config
  (yas-reload-all)

  (use-package yasnippet-snippets
   :ensure t))

(use-package recentf
  :ensure t
  :bind (("C-x f" . recentf-open-files))
  :config
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1)
  (setq recentf-max-saved-items 99)
  (setq recentf-max-menu-items 99)
  (setq recentf-show-file-shortcuts-flag nil)
  (add-hook 'find-file-hook #'recentf-save-list))

(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ("C-x g s" . magit-status)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull))

(use-package autorevert
  :ensure t
  :diminish auto-revert-mode
  :config (global-auto-revert-mode))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

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

(use-package ace-window                 ; Fast window switching
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window)
         ("M-o"   . ace-window)))

(use-package youdao-dictionary
  :ensure t
  :init
  (setq url-automatic-caching t)
  :config
  (global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point+))

(use-package htmlize
  :ensure t
  :commands (htmlize-buffer
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired
             htmlize-region))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

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
  (setq dired-sidebar-subtree-line-prefix "+")
  (setq dired-sidebar-use-term-integration t))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1))


;;Lang-cc
(use-package company
  :ensure t
  :bind (("C-c /". company-complete))
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 4)
  :config
  ;; disables TAB in company-mode, freeing it for yasnippet
  (global-company-mode)
  (add-hook 'c-mode-hook #'company-mode)
  (add-hook 'c++-mode-hook #'company-mode)
  (setq company-backends (delete 'company-semantic company-backends))
  (add-to-list 'company-backends 'company-gtags)
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil))

(use-package clang-format
  :ensure t
  :config
  (setq clang-format-style (concat "{BasedOnStyle: Google}"))
  :commands clang-format clang-format-buffer clang-format-region)

(use-package irony
  :ensure t
  :config
  (add-hook 'irony-mode-hook #'electric-pair-mode)
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands)
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

  (use-package flycheck-irony
    :ensure t
    :commands flycheck-irony-setup
    :init
    (add-hook 'c++-mode-hook 'flycheck-irony-setup)
    (add-hook 'c-mode-hook 'flycheck-irony-setup))

  (use-package company-irony
      :ensure t
      :config
      (add-to-list 'company-backends 'company-irony)))


(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

;;Function
(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

(defun vreload()
  "Reload."
  (interactive)
  (load-file user-init-file))

;;;; global key bindings
(global-set-key (kbd "<f4>")  'counsel-find-file)
(global-set-key (kbd "<f5>")  'clang-format-buffer)
(global-set-key (kbd "<f6>")  'counsel-rg)
(global-set-key (kbd "<f7>")  'avy-goto-char-2)
(global-set-key (kbd "<f8>")  'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "<f9>")  'darkroom-mode)

;;; init.el ends here
