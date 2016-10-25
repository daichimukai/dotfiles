;; maximize frame after initialization
(defun my/maximize-frame-os-x ()
  "maximize the frame on os x, prepared for after-init-hook"
  (set-frame-parameter nil 'fullscreen 'maximized))
(add-hook 'after-init-hook 'my/maximize-frame-os-x)
