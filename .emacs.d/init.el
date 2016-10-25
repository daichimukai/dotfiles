;; always require cl
;; why don't?
(require 'cl)

;; package settings
;;
;; package.el wants to add next S-exp, but I don't know whether
;; it is really needed or not
;; (package-initialize)

;; use cask
(when (require 'cask "~/.cask/cask.el" t)
  (cask-initialize))

;; init-loader
;; directory name `~/.emacs.d/inits' for init-loaders directory conflicts with
;; `~/.emacs.d/init.el' when the completion is working
;; no settings will be loaded if
(when (require 'init-loader nil t)
  (custom-set-variables
   '(init-loader-directory "~/.emacs.d/config/")
   '(init-loader-show-log-after-init "error-only")
   '(init-loader-byte-compile t))
  (init-loader-load "~/.emacs.d/config/"))

;; general settings
(setq make-backup-files nil)
(setq ring-bell-function 'ignore)
(setq confirm-kill-emacs 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
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

;; font settings
;;
;; On the OS X, `size' parameter of the next `let' variable should be an even integer
;; so that the ratio of widths of zenkaku and hankaku characters become 1:2
;;
;; はんかくもじ と ぜんかくもじ の はば の ひ を 1:2 に したい
;; hannkakumoji to zennkakumoji no haba no hi wo 1:2 ni shitai
(let* ((size 12)
       (asciifont "Ricty")
       (jpfont "Ricty")
       (h (* size 10))
       (fontspec (font-spec :family asciifont))
       (jp-fontspec (font-spec :family jpfont)))
  (set-face-attribute 'default nil :family asciifont :height h)
  (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
  (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
  (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
  (set-fontset-font nil '(#x0080 . #x024F) fontspec)
  (set-fontset-font nil '(#x0370 . #x03FF) fontspec))

;; input method
(when (require 'skk nil t)
  (setq default-input-method "japanese-skk")
  (global-set-key (kbd "C-\\") 'skk-mode))

;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)
(define-key global-map (kbd "M-SPC") 'cua-set-rectangle-mark)
(define-key cua-global-keymap [C-return] nil)

;; popwin
(when (require 'popwin nil t)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-position 'bottom)
  (push '("*Buffer List*" :noselect t) popwin:special-display-config)
  (push '("*TeX Help*" :noselect t) popwin:special-display-config))

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

;; rainbow delimiters
(when (require 'rainbow-delimiters nil t)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;; hs-minor-mode
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-#") 'hs-toggle-hiding)

;; move to the above window by S-<up> and so on
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)

;; C-h -> backspace
(global-set-key (kbd "C-h") 'delete-backward-char)

;; do not use list-buffers
(global-unset-key (kbd "C-x C-b"))
