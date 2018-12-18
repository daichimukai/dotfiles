;;; init.el --- My init.el
;;; https://github.com/daichimukai/dotfiles

;;; Commentary:
;;;
;;; How did that?
;;;
;;; Q. How do I open this url in a browser?
;;; A. Push "gx" on the url (`browse-url-at-point`)
;;;


;;; Code:


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
(scroll-bar-mode -1) ; disable scroll bar
(tool-bar-mode -1)

;;; use-package
;;; https://github.com/jwiegley/use-package
(straight-use-package 'use-package)


;;; Font
;;;
;;; hannkaku moji to zennkaku moji no haba no hi wo 1:2 ni shitai
;;; はんかく もじ と ぜんかく もじ の はば の ひ を 1:2 に したい
;;;
;;; (ó﹏ò。) < HELP ME! I cannot explain the accurate reason why this config works as exactly I want
;;;

;;; Hack
;;; https://sourcefoundry.org/hack/
(set-face-attribute 'default 'nil :family "Hack" :height 105)

;;; MyricaM
;;; https://myrica.estable.jp/myricamhistry/
(set-fontset-font t 'japanese-jisx0208 (font-spec :family "MyricaM M" :size 12.0))

;; For macOS
(when (eq system-type 'darwin)
  ;; Anonymous Pro
  ;; https://www.marksimonson.com/fonts/view/anonymous-pro
  (set-face-attribute 'default 'nil :family "Anonymous Pro" :height 160)

  ;; Ricty
  ;; https://www.rs.tus.ac.jp/yyusa/ricty.html
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Ricty" :size 18.0)))

;;; fira-code-mode
(use-package fira-code-mode
  :load-path "lisp"
  :hook (prog-mode . fira-code-mode)
  :config (fira-code-mode--setup))


;;; my utils
(defun my/open-init-el ()
  "Open init.el."
  (interactive)
  (if (get-buffer "init.el")
      (let ((buffer (get-buffer "init.el")))
	(switch-to-buffer buffer))
    (find-file (expand-file-name "~/.emacs.d/init.el"))))

(defun my/reload-init-el ()
  "Reload init.el."
  (interactive)
  (eval-buffer (find-file-noselect (expand-file-name "~/.emacs.d/init.el")))
  (message "Reloaded ~/.emacs.d/init.el"))

(defun my/switch-to-scratch-buffer ()
  "Switch to a scratch buffer."
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


;;; which-key
(use-package which-key
  :config (which-key-mode)
  :straight t)


;;; vcs

;;; magit
(use-package magit
  :defer t
  :init
  (setq vc-follow-symlinks t)
  :straight t)


;;; evil
(use-package evil
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil)
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


;;; UI

;;; moe-theme
(use-package moe-theme
  :config
  (moe-dark)
  (moe-theme-set-color 'orange)
  :straight t)

;;; golden-ratio.el
;;; https://github.com/roman/golden-ratio.el
(use-package golden-ratio
  :straight t
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-exclude-buffer-names '("*goals*" "*response*"))
  (add-to-list 'golden-ratio-extra-commands 'magit-status))

;;; rainbow-delimiters
;;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))


;;; Completion

;;; ivy
;;; https://github.com/abo-abo/swiper
(use-package ivy
  :straight t
  :config
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
	'((t . ivy--regex-plus)))
  (ivy-mode 1))

(use-package counsel
  :straight t
  :config
  (counsel-mode))

(use-package swiper :straight t
  :bind (("C-s" . swiper)))

;;; company-mode
;;; http://company-mode.github.io/
(use-package company :straight t
  :init (setq company-tooltip-align-annotations t)
  :hook (prog-mode . company-mode))


;;; syntax check

;;; flycheck
;;; https://github.com/flycheck/flycheck
(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))


;;; input method

;;; ddskk
;;; https://github.com/skk-dev/ddskk
(use-package ddskk :straight t :defer t)


;;; Utility

;;; quickrun.el
;;; https://github.com/syohex/emacs-quickrun
(use-package quickrun :defer t :straight t)

;;; yasnippet
(use-package yasnippet
  :straight t
  :init
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (use-package yasnippet-snippets :defer t :straight t)
  (yas-global-mode 1))


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

;;; Proof General
;;; https://github.com/ProofGeneral/PG
;;;
;;; See https://github.com/ProofGeneral/PG/issues/385 for the reason of such a strange use-package
(use-package proof-general
  :straight t
  :no-require t)
(use-package proof-site
  :mode ("\\.v\\'" . coq-mode))

;;; company-coq-mode
;;; https://github.com/cpitclaudel/company-coq
(use-package company-coq
  :after (company proof-general)
  :straight t
  :hook (coq-mode . company-coq-mode))


(provide 'init)
;;; init.el ends here
