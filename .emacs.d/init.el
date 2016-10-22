(require 'cl)

(setq-default user-full-name "Daichi Mukai")
(setq-default user-mail-address "mukai.daichi.37e@kyoto-u.jp")

;; package settings
;; using cask
(package-initialize)
(when (if (eq system-type 'gnu/linux)
    (require 'cask "~/.cask/cask.el" t)
    (require 'cask nil t))
  (cask-initialize))

;; general settings
(setq make-backup-files nil)
(setq ring-bell-function 'ignore)
(setq confirm-kill-emacs 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq scroll-step 1)
(setq show-trailing-whitespace t)
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(show-paren-mode)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; theme
(when (require 'moe-theme nil t)
  (moe-dark)
  (moe-theme-set-color 'orange))

;; input method
(when (require 'skk nil t)
  (setq default-input-method "japanese-skk")
  (global-set-key (kbd "C-\\") 'skk-mode))

;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)
(define-key global-map (kbd "M-SPC") 'cua-set-rectangle-mark)
(define-key cua-global-keymap [C-return] nil)

;; ido mode
(when (require 'ido nil t)
  (ido-mode)
  (setq ido-everywhere t)
  (setq ido-create-new-buffer 'always)
  (setq ido-enable-flex-matching t)
  (when (boundp 'confirm-nonexistent-file-or-buffer)
    (setq confirm-nonexistent-file-or-buffer nil))
  (global-set-key (kbd "C-x f") 'ido-find-file-other-window))

;; anzu
(when (require 'anzu nil t)
  (global-anzu-mode 1))

;; git
(when (require 'magit nil t)
  (global-set-key (kbd "C-x g") 'magit-status))
(when (require 'git-gutter nil t)
  (global-git-gutter-mode 1))
(setq vc-follow-symlinks t)

;; undo history
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; snippet
(when (require 'yasnippet nil t)
  (yas-global-mode 1)
  (set-default 'yas-dont-activate
	       #'(lambda()
		   (or (minibufferp)
		       (eq major-mode 'term-mode)))))

;; completion
(when (and (require 'company nil t) (require 'company-math nil t) (require 'company-auctex nil t))
  (setq company-idel-delay 0)
  (setq company-tooltip-align-annotations t)
  (company-auctex-init)
  (define-key global-map (kbd "<C-tab>") 'company-complete)
  (add-to-list 'company-backends 'company-math-symbols-latex)
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-commands)
  (add-hook 'after-init-hook 'global-company-mode))

;; etc
(when (require 'rainbow-delimiters nil t)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;; move to the above window by S-<up> and so on
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)

;; C-h -> backspace
(global-set-key (kbd "C-h") 'delete-backward-char)

;; do not use list-buffers
(global-unset-key (kbd "C-x C-b"))
