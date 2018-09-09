;;; package --- Summary
;;; Commentary:

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

(defun vreload()
  "Reload."
  (interactive)
  (load-file user-init-file))

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

(use-package google-translate
  :ensure t
  :defines google-translate-translation-directions-alist
  :bind
  ("C-c t" . google-translate-smooth-translate)
  :init
  (setq google-translate-output-destination 'popup)
  (setq google-translate-translation-directions-alist
        '(("en" . "zh-CN") ("zh-CN" . "en"))))


(provide 'base-functions)
