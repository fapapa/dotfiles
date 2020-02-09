;;; init-local.el --- my customizations; not in source control
;;; Commentary:
;;; Code:
;;; Always start fullscreen
(toggle-frame-fullscreen)
(add-hook 'after-make-window-system-frame-hooks
          'toggle-frame-fullscreen)

;;; Highlight the line in the current buffer
(global-hl-line-mode)

;;; C-f in the magit status buffer invokes the magit-gitflow popup. If you
;;; would like to use a different key, set the magit-gitflow-popup-key variable
;;; before loading magit-gitflow
;; (setq magit-gitflow-popup-key "C-n")
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(editorconfig-mode 1)

;; (setq explicit-shell-file-name "/bin/bash")
;; (setq shell-file-name "bash")
;; (setq explicit-bash-args '())
;; (setenv "SHELL" shell-file-name)
;; (setenv "PS1" "\\W> \\[$(tput sgr0)\\]")

;; FROM https://github.com/legoscia/dotemacs/blob/master/dotemacs.org#fix-the-display-of-emoji
;; To fix problems with ZSH displaying properly in shell and emojis displaying properly
;; 'set-fontset-font' is not defined when Emacs is built without a window system
(when (fboundp 'set-fontset-font)
  (defun my-fix-emojis (&optional frame)
    (set-fontset-font "fontset-default" '(#x10000 . #x1ffff) "Fira Code" frame))
  (my-fix-emojis)
  (add-hook 'after-make-frame-functions 'my-fix-emojis))

;; Use a different font in comint mode for better display
(defun my-buffer-face-mode-variable ()
  "Set font to something that displays well for comint mode."
  (interactive)
  (setq buffer-face-mode-face '(:family "MesloLGS NF"))
  (buffer-face-mode))
(add-hook 'comint-mode-hook 'my-buffer-face-mode-variable)

(put 'erase-buffer 'disabled nil)

(dumb-jump-mode)

(yas-global-mode 1)

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(global-set-key (kbd "C-c j") 'emmet-expand-line)

(setq-default css-indent-offset 2)
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'auto-fill-mode)
(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)

;; Prettier
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

;; Auto-insert end keyword
(require 'ruby-end)

(setq alchemist-key-command-prefix (kbd "C-c e"))

(provide `init-local)
;;; init-local.el ends here
