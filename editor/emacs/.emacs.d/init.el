;;; package --- Summary
; Commentary:

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please... jeez
(show-paren-mode 1)
(global-hl-line-mode 1)
(display-time-mode 1)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(setq column-number-mode t)
(setq line-number-mode t)
(global-linum-mode t)
(setq display-time-24hr-format t)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq show-paren-style 'parenthesis)
(setq show-paren-style 'expression)
(setq show-paren-style 'mixed)
(setq max-mini-window-height 0.8)

(set-default-font "Hack 16")

;;;; package.el
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(defun mp-install-packages ()
  "Install only the sweetest of packages."
  (interactive)
  (package-refresh-contents)
  (mapc '(lambda (package)
           (unless (package-installed-p package)
             (package-install package)))
        '(browse-kill-ring
          paredit
          evil
          use-package
          ivy
          counsel
          swiper
          darcula-theme
          undo-tree)))

;;;; macros
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))


;;;; emacs lisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#212121" "#B71C1C" "#558b2f" "#FFA000" "#2196f3" "#4527A0" "#00796b" "#FAFAFA"))
 '(custom-enabled-themes (quote (darcula)))
 '(custom-safe-themes
   (quote
    ("b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "3d5720f488f2ed54dd4e40e9252da2912110948366a16aef503f3e9e7dfe4915" default)))
 '(fci-rule-color "#dadada")
 '(hl-sexp-background-color "#efebe9")
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (ibuffer-sidebar dired-sidebar imenu-anywhere material-theme counsel counsel-gtags swiper ivy use-package yasnippet-snippets airline-themes flycheck darcula-theme clang-format rainbow-delimiters company-irony irony company avy evil undo-tree paredit magit browse-kill-ring)))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun reload-user-init-file()
  (interactive)
  (load-file user-init-file))

(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )
 
(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

(setq clang-format-style (concat "{BasedOnStyle: Google}"))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

(after "rainbow-delimiters-autoloads"
 (add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'company-mode)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(add-hook 'c-mode-hook 'counsel-gtags-mode)
(add-hook 'c++-mode-hook 'counsel-gtags-mode)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'evil)
(evil-mode 1)

(require 'flycheck)
(global-flycheck-mode)

(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

(require 'airline-themes)
(load-theme 'airline-light)

(require 'yasnippet)
(yas-global-mode 1)

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
        '((t   . ivy--regex-ignore-order))))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
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
  :ensure nil
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face `(:family "Helvetica" :height 140)))

(defun sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

;;;; global key bindings
(global-set-key (kbd "<f3>")  'imenu-anywhere)
(global-set-key (kbd "<f4>")  'counsel-find-file)
(global-set-key (kbd "<f5>")  'clang-format-buffer)
(global-set-key (kbd "<f6>")  'counsel-rg)
(global-set-key (kbd "<f7>")  'avy-goto-char-2)
(global-set-key (kbd "<f8>")  'sidebar-toggle)
(global-set-key (kbd "<f9>")  'counsel-gtags-find-definition)
(global-set-key (kbd "<f10>") 'counsel-gtags-go-forward)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-x l") 'counsel-locate)


(after "counsel-gtags"
  (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
  (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
  (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
  (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward))

;;; init.el ends here
