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
 '(ansi-color-names-vector
   ["#1B2229" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#DFDFDF"])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" default)))
 '(fci-rule-color "#5B6268")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (multiple-cursors ace-window counsel-projectile htmlize projectile markdown-mode elpy jedi company-jedi smart-mode-line smartparens window-numbering rainbow-mode darkroom dracula-theme company-irony-c-headers editorconfig ibuffer-sidebar yasnippet clang-format avy dired-sidebar yasnippet-snippets flycheck company-irony irony company smex magit counsel-gtags counsel swiper ivy evil-leader evil use-package)))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

(use-package smex :ensure t)
(use-package editorconfig :ensure t)

;; Evil Mode
(use-package evil
  :ensure t
  :config
  (define-key evil-insert-state-map [remap newline] 'newline)
  (define-key evil-insert-state-map [remap newline-and-indent] 'newline-and-indent)
  (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-ex-map "e " 'find-file)
  (define-key evil-ex-map "b " 'switch-to-buffer)
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
      "ee" 'mc/edit-lines
      "w/" 'split-window-right
      "w-" 'split-window-below
      ":"  'counsel-M-x
      "wm" 'delete-other-windows)))


;;;; global key bindings
(global-set-key (kbd "<f3>")  'imenu-anywhere)
(global-set-key (kbd "<f4>")  'counsel-find-file)
(global-set-key (kbd "<f5>")  'clang-format-buffer)
(global-set-key (kbd "<f6>")  'counsel-rg)
(global-set-key (kbd "<f7>")  'avy-goto-char-2)
(global-set-key (kbd "<f8>")  'vsidebar)
(global-set-key (kbd "<f9>")  'darkroom-mode)


(use-package avy :ensure t)
(use-package smartparens :ensure t)
(use-package markdown-mode :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package darkroom :ensure t)
(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))


;;Theme
(use-package dracula-theme       :ensure t :defer t)
(use-package atom-one-dark-theme :ensure t :defer t)
(use-package monokai-theme       :ensure t :defer t)
(use-package solarized-theme     :ensure t :defer t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  (sml/setup))


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

(use-package multiple-cursors
  :ensure t
  :init
  (progn
    (global-set-key (kbd "C-c .") 'mc/mark-next-like-this)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-c ,") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-l") 'c/mark-all-like-this)))


;;Lang-cc
(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)
  (add-hook 'c-mode-hook #'company-mode)
  (add-hook 'c++-mode-hook #'company-mode)
  :config
  (progn
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


;;Lang-py
;;pip install rope  # refactoring library
;;pip install jedi  # lightweight autocompletion
;;pip install flake8
;;pip install importmagic
;;pip install yapf
(setq python-shell-completion-native-enable nil)

(use-package python
  :mode ("\\.py\\'" . python-mode)
        ("\\.wsgi$" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (setq python-indent-offset 4)
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'python-mode-hook 'color-identifiers-mode))

(use-package jedi
  :ensure t
  :init
  (add-to-list 'company-backends 'company-jedi)
  :config
  (use-package company-jedi
    :ensure t
    :init
    (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
    (setq company-jedi-python-bin "python")))

(use-package elpy
  :ensure t
  :defer 2
  :config
  (progn
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (remove-hook 'elpy-modules 'elpy-module-yasnippet)
      (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    (elpy-enable)
    (setq elpy-rpc-backend "jedi")))


;;Function
(defun vsplit-last-buffer ()
  "Vsplit."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  "Hsplit."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

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


;;;Org-mode
(use-package org
  :ensure t
  :config
  (setq org-agenda-files (list "~/org/todo.org"))
  (setq org-todo-keywords
        '((sequence "TODO" "NEXT" "|" "DONE")
          (sequence "WAITING" "HOLD" "|" "CANCELLED")))
  (setq org-todo-keyword-faces
        '(("NEXT" :foreground "blue" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)))
  ;; 优先级范围和默认任务的优先级
  (setq org-highest-priority ?A)
  (setq org-lowest-priority  ?E)
  (setq org-default-priority ?E)
  ;; 优先级醒目外观
  (setq org-priority-faces
        '((?A . (:background "red" :foreground "white" :weight bold))
          (?B . (:background "DarkOrange" :foreground "white" :weight bold))
          (?C . (:background "yellow" :foreground "DarkGreen" :weight bold))
          (?D . (:background "DodgerBlue" :foreground "black" :weight bold))
          (?E . (:background "SkyBlue" :foreground "black" :weight bold))))
  (setq org-M-RET-may-split-line nil)
  (setq org-src-fontify-natively t)
  (setq org-log-done 'time))

(use-package htmlize
  :ensure t
  :commands (htmlize-buffer
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired
             htmlize-region))

(defun vreload()
  "Reload."
  (interactive)
  (load-file user-init-file))


;;; init.el ends here

