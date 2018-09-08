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
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" default)))
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(package-selected-packages
   (quote
    (elpy jedi company-jedi smart-mode-line smartparens powerline window-numbering rainbow-mode darkroom hydra dracula-theme company-irony-c-headers editorconfig ibuffer-sidebar yasnippet clang-format avy dired-sidebar yasnippet-snippets flycheck company-irony irony company smex magit counsel-gtags counsel swiper ivy evil-leader evil use-package)))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Hack")
  (set-face-attribute 'default nil :height 125))

(use-package smex :ensure t)
(use-package editorconfig :ensure t)
(use-package rainbow-mode :ensure t)

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

;;;; global key bindings
(global-set-key (kbd "<f3>")  'imenu-anywhere)
(global-set-key (kbd "<f4>")  'counsel-find-file)
(global-set-key (kbd "<f5>")  'clang-format-buffer)
(global-set-key (kbd "<f6>")  'counsel-rg)
(global-set-key (kbd "<f7>")  'avy-goto-char-2)
(global-set-key (kbd "<f8>")  'vsidebar)
(global-set-key (kbd "<f9>")  'darkroom-mode)

;;Custom Plugins
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'base-theme)
(require 'base-extensions)
(require 'base-functions)

(require 'lang-cc)
(require 'lang-py)

;;; init.el ends here

