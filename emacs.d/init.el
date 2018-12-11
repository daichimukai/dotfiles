;;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(cd "~/")
(setq inhibit-startup-message t) ; no startup screen
(setq make-backup-files nil)
(setq auto-save-default nil)
(global-set-key (kbd "C-h") 'delete-backward-char)
(scroll-bar-mode -1) ; disable scroll bar

(straight-use-package 'use-package)

(use-package which-key
  :config (which-key-mode)
  :straight t)

(use-package magit
  :defer t
  :straight t)

(use-package evil
  :config
  (define-prefix-command 'my-leader-map) ;; my leader key
  (define-key evil-normal-state-map (kbd "SPC") 'my-leader-map) ;; change the leader key to space
  (define-key my-leader-map "fed" (lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/init.el"))))
  (define-key my-leader-map "ff" 'find-file)
  (define-key my-leader-map (kbd "SPC") 'execute-extended-command)
  (define-key my-leader-map "bb" 'switch-to-buffer)
  (evil-mode)
  :straight t)

;; escape all by "fd"
(use-package evil-escape
  :after evil
  :config (evil-escape-mode)
  :straight t)

(use-package moe-theme
  :config (moe-dark)
  :straight t)
