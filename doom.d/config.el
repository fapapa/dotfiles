;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Fabio Papa"
      user-mail-address "fabtheman@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 14))
(setq doom-font (font-spec :name "Operator Mono" :size 14))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Make C-z the doom local-leader alt key
(map! "C-z" nil)
(setq doom-localleader-alt-key "C-z")

(map! "M-m" 'mc/mark-all-like-this-dwim)

(defun +fabio/mark-symbol-or-mark-next-like-this (arg)
  "To mimmick Cmd-d behavior in modern editors. With no region, marks
word/symbol at point
With region marks next occurence of region
With zero ARG, skip the last one and mark next
With negative ARG, search backwards for occurence of region"
  (interactive "p")
  (if (region-active-p)
      (if (< arg 0)
          (mc/mark-previous-like-this 1)
        (mc/mark-next-like-this arg))
    (er/mark-symbol))
  (mc/maybe-multiple-cursors-mode))
(map! "s-d" '+fabio/mark-symbol-or-mark-next-like-this)

(setq-hook! 'js2-mode-hook
  js2-basic-offset 2)

(map! "C-j" 'emmet-expand-line)
(map! "C-S-j" 'emmet-wrap-with-markup)

(setq projectile-project-search-path
      '("~/dev/" "~/dev/lighthouse"))

(use-package! projectile-rails
  :init
  (setq projectile-rails-custom-server-command nil))

;; Cucumber feature files
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
;; Enable file template for *.feature files
;; Stored in .doom.d/snippets/feature-mode/__
(setq +file-templates-alist
  (cons '(feature-mode) +file-templates-alist))

;; Load customize variables
(load! "custom")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
