;;; init.el --- My init.el
;;; https://github.com/daichimukai/dotfiles

;;; Commentary:
;;;
;;; How did that?
;;;
;;; Q. How do I open this url in a browser?
;;; A. M-x browse-url-at-point
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

(setq custom-file (locate-user-emacs-file "custom.el"))
(setq inhibit-startup-message t) ; no startup screen
(setq make-backup-files nil)
(setq auto-save-default nil)
(scroll-bar-mode -1) ; disable scroll bar
(tool-bar-mode -1)
(column-number-mode +1)
(when (eq system-type 'darwin)
  (add-hook 'after-init-hook #'(lambda () (set-frame-parameter nil 'fullscreen 'maximized))))

;;; use-package
;;; https://github.com/jwiegley/use-package
(straight-use-package 'use-package)
(straight-use-package 'diminish)
(eval-when-compile (require 'use-package))
(eval-and-compile (require 'bind-key))
(eval-and-compile (require 'diminish))

(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

(defmacro safe-diminish (file mode &optional newname)
  `(with-eval-after-load ,file
     (diminish ,mode ,newname)))
(safe-diminish "emacs-lisp" 'emacs-lisp-mode "el")
(safe-diminish "undo-tree" 'undo-tree-mode "戻")
(safe-diminish "eldoc" 'eldoc)

;;; For writing elisp

;;; dash.el
;;; https://github.com/magnars/dash.el
(use-package dash :no-require t)

;;; s.el
;;; https://github.com/magnars/s.el
(use-package s :no-require t)

;;; f.el
;;; https://github.com/rejeep/f.el
(use-package f :no-require t)


;;; Font
;;;
;;; hannkaku moji to zennkaku moji no haba no hi wo 1:2 ni shitai
;;; はんかく もじ と ぜんかく もじ の はば の ひ を 1:2 に したい
;;;
;;; (ó﹏ò。) < HELP ME! I cannot explain the accurate reason why this config works as exactly I want
;;;
;;; TODO: write code to switch fonts easily

;;; Hack
;;; https://sourcefoundry.org/hack/
;;; (set-face-attribute 'default 'nil :family "Hack" :height 105)

;;; Anonymous Pro
;;; (set-face-attribute 'default 'nil :family "Anonymous Pro" :height 120)

;;; Sarasa
;;; https://github.com/be5invis/Sarasa-Gothic
(if (eq system-type 'darwin)
      (set-face-attribute 'default 'nil :family "Sarasa Term J" :height 160)
  (set-face-attribute 'default 'nil :family "Sarasa Term J" :height 120))

;;; MyricaM
;;; https://myrica.estable.jp/myricamhistry/

;;; If you want to use Hack font config, uncomment the following line
; (set-fontset-font t 'japanese-jisx0208 (font-spec :family "MyricaM M" :size 12.0))
; (set-fontset-font t 'japanese-jisx0208 (font-spec :family "MyricaM M" :size 13.5))
; (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Sarasa Term J" :size 12.0))

;; For macOS
;(when (eq system-type 'darwin)
  ;; Anonymous Pro
  ;; https://www.marksimonson.com/fonts/view/anonymous-pro
  ;(set-face-attribute 'default 'nil :family "Anonymous Pro" :height 160)

  ;; Ricty
  ;; https://www.rs.tus.ac.jp/yyusa/ricty.html
  ; (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Ricty" :size 18.0)))

;;; fira-code-mode
(use-package fira-code
  :load-path "lisp"
  :no-require t
  :straight nil
  :diminish nil
  :disabled t
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


;;; which-key
(use-package which-key
  :defer nil
  :init
  (setq which-key-enable-extended-define-key t)
  :config (which-key-mode))


;;; leader key
(define-prefix-command 'my-leader-map) ;; my leader key
(bind-key "S-SPC" my-leader-map)
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


;;; vcs

;;; git.el
(use-package git
  :functions git-run
  :no-require t)

;;; magit
(use-package magit
  :defer t
  :diminish "魔"
  :init
  (setq vc-follow-symlinks t)
  :no-require t)


;;; evil
(use-package evil
  :disabled t
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  :config
  (define-key evil-normal-state-map (kbd "SPC") 'my-leader-map) ;; change the leader key to space
  (define-key evil-visual-state-map (kbd "SPC") 'my-leader-map) ;; change the leader key to space
  (evil-mode)
  :demand t)

;; escape all by "fd"
(use-package evil-escape
  :after evil
  :diminish nil
  :config (evil-escape-mode)
  :demand t
  :disabled t
  :no-require t)

(use-package evil-magit
  :after (evil magit)
  :no-require t
  :init
  :disabled t
  (add-hook 'magit-mode-hook #'(lambda () (require 'evil-magit)))
  (setq evil-magit-state 'normal))


;;; UI

;;; moe-theme
(use-package moe-theme
  :demand t
  :config
  (moe-dark)
  (moe-theme-set-color 'orange))

;;; golden-ratio.el
;;; https://github.com/roman/golden-ratio.el
(use-package golden-ratio
  :demand t
  :no-require t
  :diminish "φ"
  :disabled t
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-exclude-buffer-names '("*goals*" "*response*"))
  (add-to-list 'golden-ratio-extra-commands 'magit-status))

;;; rainbow-delimiters
;;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :straight t
  :no-require t
  :hook (prog-mode . rainbow-delimiters-mode))


;;; Completion

;;; ivy
;;; https://github.com/abo-abo/swiper
(use-package ivy
  :demand t
  :no-require t
  :config
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
	'((t . ivy--regex-plus)))
  (ivy-mode 1))

(use-package counsel
  :demand t
  :no-require t
  :config
  (counsel-mode))

(use-package swiper
  :no-require t
  :bind (("C-s" . swiper)))

;;; company-mode
;;; http://company-mode.github.io/
(use-package company
  :no-require t
  :diminish "補"
  :init (setq company-tooltip-align-annotations t)
  :hook (prog-mode . company-mode))

;;; Language Server Protocol
(use-package eglot
  :no-require t)


;;; syntax check

;;; flycheck
;;; https://github.com/flycheck/flycheck
(use-package flycheck
  :no-require t
  :config
  (global-flycheck-mode))


;;; input method

;;; ddskk
;;; https://github.com/skk-dev/ddskk
(use-package ddskk
  :no-require t)


;;; Utility

;;; hydra
;;; https://github.com/abo-abo/hydra
(use-package hydra :no-require)

;;; quickrun.el
;;; https://github.com/syohex/emacs-quickrun
(use-package quickrun
  :no-require t)

;;; yasnippet
(use-package yasnippet
  :no-require t
  :defer nil
  :after which-key
  :init
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (use-package yasnippet-snippets :defer t :straight t)
  (define-key my-leader-map "y" '("YASnippet"))
  (bind-keys  :map my-leader-map
	      :prefix "y"
	      :prefix-map my-yasnippet-map
	      :prefix-docstring "YASnippet"
	      ("s" . yas-insert-snippet)
	      ("n" . yas-new-snippet)
	      ("r" . yas-recompile-all)
	      ("R" . yas-reload-all)
	      ("v" . yas-visit-snippet-file))
  (yas-global-mode 1))

;;; projectile
;;; https://github.com/bbatsov/projectile
(use-package projectile
  :no-require t
  :defer nil
  :config
  (projectile-mode +1))

;;; smartparens
;;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :no-require
  :config
  (require 'smartparens-config)
  :hook (prog-mode . smartparens-mode))

;;; ParEdit
(use-package paredit
  :no-require t)

;;; multiple-cursors.el
;;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :no-require t)

;;; org-mode
;;; https://www.orgmode.org/ja/index.html
;;;
;;; See https://github.com/raxod502/radian/blob/develop/emacs/radian.el for the hack below
(eval-and-compile (require 'subr-x)
		   (require 'git))

(defun org-git-version ()
  "The Git version of `org-mode`.
Inserted by installing `org-mode` or when a release is made."
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
	      "--match=release\*"
	      "--abbrev=6"
	      "HEAD"))))

(defun org-release ()
  "The release version of `org-mode`.
Inserted by installing `org-mode` or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)
(use-package org
  :no-require t
  :config
  (with-eval-after-load 'org-capture
    ;; https://ox-hugo.scripter.co/doc/org-capture-setup/
    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more infomation. "
      (let* ((title (read-from-minibuffer "Post title: "))
	     (fname (org-hugo-slug title))
	     (date (format-time-string "%Y-%m-%d" (current-time))))
	(s-join "\n"
		`(
		  ,(concat "* TODO " title)
		  ":PROPERTIES:"
		  ,(concat ":EXPORT_FILE_NAME: " fname)
		  ,(concat ":EXPORT_DATE: " date)
		  ":END:"
		  "%?\n"))))
    (add-to-list 'org-capture-templates
		 '("b"
		   "Blog post"
		   entry
		   (file+olp "blog.org" "Posts")
		   (function org-hugo-new-subtree-post-capture-template)))))

;;; ox-hugo
;;; https://github.com/kaushalmodi/ox-hugo
(use-package ox-hugo
  :defer t
  :no-require t
  :after ox
  :init
  (setq org-hide-leading-stars t)
  :config
  (require 'ox-hugo-auto-export))


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
  :no-require t
  :mode
  ("\\.rs\\'" . rust-mode)
  :init
  (setq rust-format-on-save t)
  :config
  (bind-key "TAB" 'company-indent-or-complete-common rust-mode-map))

;;; racer
;;; https://github.com/racer-rust/emacs-racer
(use-package racer
  :no-require t
  :after rust-mode
  :hook ((rust-mode . racer-mode)
	 (racer-mode . eldoc-mode)))

;;; cargo.el
;;; https://github.com/kwrooijen/cargo.el
(use-package cargo
  :no-require t
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

;;; Proof General
;;; https://github.com/ProofGeneral/PG
;;;
;;; See https://github.com/ProofGeneral/PG/issues/385 for the reason of such a strange use-package
(use-package proof-site
  :straight proof-general
  :mode ("\\.v\\'" . coq-mode))

;;; company-coq-mode
;;; https://github.com/cpitclaudel/company-coq
(use-package company-coq
  :after (company proof-site)
  :no-require t
  :hook (coq-mode . company-coq-mode))

;;; lean-mode
;;; https://github.com/leanprover/lean-mode
(use-package lean-mode
  :no-require t)
(use-package company-lean
  :no-require t
  :after (company lean-mode))

;;; slime
(use-package slime
  :init
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))
  :no-require)

;;; eros
;;; https://github.com/xiongtx/eros
(use-package eros
  :no-require t)

;;; rg
(use-package rg
  :no-require t)

(provide 'init)
;;; init.el ends here
