;; AUCTeX
(use-package tex
  :config
  (custom-set-variables
   '(font-latex-fontify-sectioning 'color)
   '(TeX-source-correlate-method 'synctex)
   '(TeX-source-correlate-start-server t)
   '(TeX-engine-alist '((pdfuptex "pdfupTeX"
				  "ptex2pdf -u -e -ot la'%S %(mode)'"
				  "ptex2pdf -u -l -ot '%S %(mode)'"
				  "euptex")))
   '(japanese-LaTeX-command-default 'pdfuptex))
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode))
