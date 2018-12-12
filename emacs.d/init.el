;;; straight.el
;;; https://github.com/raxod502/straight.el
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
(tool-bar-mode -1)

;;; use-package
;;; https://github.com/jwiegley/use-package
(straight-use-package 'use-package)

(defun my/open-init-el ()
  "open init.el"
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))

(defun my/reload-init-el ()
  "reload init.el"
  (interactive)
  (eval-buffer (find-file-noselect (expand-file-name "~/.emacs.d/init.el")))
  (message "Reloaded ~/.emacs.d/init.el"))

(defun my/switch-to-scratch-buffer ()
  "switch to a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

;;; leader key
(define-prefix-command 'my-leader-map) ;; my leader key
(bind-key "SPC" 'execute-extended-command my-leader-map)

;;; file map
(define-prefix-command 'my-file-map)
(bind-keys :map my-leader-map
	   :prefix "f" ;; bind to '<leader>f'
	   :prefix-map my-file-map
	   ("f" . find-file))

;;; buffer map
(define-prefix-command 'my-buffer-map)
(bind-keys :map my-leader-map
	   :prefix "b" ;; bind to '<leader>b'
	   :prefix-map my-buffer-map
	   ("b" . switch-to-buffer)
	   ("s" . my/switch-to-scratch-buffer))

;;; git map
(define-prefix-command 'my-git-map)
(bind-keys :map my-leader-map
	   :prefix "g" ;; bind to '<leader>g'
	   :prefix-map my-git-map
	   ("s" . magit-status))

;;; init.el map
(define-prefix-command 'my-init-el-map)
(bind-keys :map my-file-map
	   :prefix "e" ;; bind to '<leader>fe'
	   :prefix-map my-init-el-map
	   ("d" . my/open-init-el))

(use-package which-key
  :config (which-key-mode)
  :straight t)

(use-package magit
  :defer t
  :init
  (setq vc-follow-symlinks t)
  :straight t)

(use-package evil
  :config
  (define-key evil-normal-state-map (kbd "SPC") 'my-leader-map) ;; change the leader key to space
  (evil-mode)
  :straight t)

;; escape all by "fd"
(use-package evil-escape
  :after evil
  :config (evil-escape-mode)
  :straight t)

(use-package evil-magit :straight t
  :after (evil magit)
  :init (setq evil-magit-state 'normal))

(use-package moe-theme
  :config (moe-dark)
  :straight t)

;;; Completion
(use-package ivy :straight t
  :config (ivy-mode 1))

(use-package swiper :straight t
  :bind (("C-s" . swiper)))

;;; company-mode
;;; http://company-mode.github.io/
(use-package company :straight t
  :init (setq company-tooltip-align-annotations t)
  :hook (prog-mode . company-mode)
  :bind)


;;; Language specific configurations

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; R u s t
;;;
;;; TODO: major mode map for rust mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; rust-mode
;;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :straight t
  :mode
  ("\\.rs\\'" . rust-mode)
  :config
  (bind-key "TAB" 'company-indent-or-complete-common rust-mode-map))

;;; racer
;;; https://github.com/racer-rust/emacs-racer
(use-package racer
  :straight t
  :after rust-mode
  :hook ((rust-mode . racer-mode)
	 (racer-mode . eldoc-mode)))

;;; cargo.el
;;; https://github.com/kwrooijen/cargo.el
(use-package cargo
  :straight t
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))
