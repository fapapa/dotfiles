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
(setq doom-font "Operator Mono-12:weight=semilight")
(setq doom-big-font "Operator Mono-28:weight=semilight")
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-string-face :slant italic)
  '(font-lock-keyword-face :weight bold))

(map! (:when IS-MAC
        "s-}" '+workspace/switch-right
        "s-{" '+workspace/switch-left))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-monokai-machine)

;;;;;;;;;;;;;;;;;;;;;;;
;; Org Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq +org-capture-todo-file "inbox.org")
(after! org
  (setq org-capture-templates
        '(("c" "Collect to inbox" entry
           (file +org-capture-todo-file)
           "* [ ] %?\n%i\n%a" :prepend f))))
(setq org-agenda-custom-commands
      '(("c" "Next item to clarify" todo ""
         ((org-agenda-max-entries 1)))))

(setq projectile-project-search-path
      '("~/dev/" "~/dev/lighthouse"))

;;;;;;;;;;;;;;;;;;
;; Ruby & Rails ;;
;;;;;;;;;;;;;;;;;;
(use-package! projectile-rails
  :init
  (setq projectile-rails-custom-server-command nil))

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

;; Cucumber feature files
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
;; Enable file template for *.feature files
;; Stored in .doom.d/snippets/feature-mode/__
(setq +file-templates-alist
  (cons '(feature-mode) +file-templates-alist))

(setq rbenv-installation-dir "/usr/local")

(setq! web-mode-markup-indent-offset 2)
(setq! web-mode-code-indent-offset 2)
(setq-hook! 'js-mode-hook
  js-indent-level 2)
(setq-hook! 'js2-mode-hook
  js2-basic-offset 2)

(with-eval-after-load "ox-latex"
  (setq org-latex-toc-command "\\tableofcontents \\clearpage")
  (add-to-list 'org-latex-classes
               '("koma-article" "\\documentclass[letterpaper]{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("koma-book" "\\documentclass[11pt]{scrbook}"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("koma-report" "\\documentclass[letterpaper]{scrreprt}"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(use-package! prog-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq! format-all-formatters
         '(("Ruby" standardrb)
           ("JavaScript" prettier))))
(setq-hook! 'ruby-mode-hook flycheck-checker 'ruby-standard)

(after! web-mode
  (use-package! lsp-tailwindcss
    :init
    (setq! lsp-tailwindcss-add-on-mode t))
  (add-to-list 'lsp-language-id-configuration '(".*\\.erb$" . "html"))
  (add-hook 'web-mode-local-vars-hook #'lsp!))

(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

(setq epg-pinentry-mode 'loopback)
(pinentry-start)

;; accept completion from copilot and fall back to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

(evil-define-operator fp/evil:explain-code (beg end)
  "Make chatgpt-shell explain-code function into an evil operator."
  :move-point nil
  (deactivate-mark)
  (goto-char end)
  (set-mark (point))
  (goto-char beg)
  (activate-mark)
  (chatgpt-shell-explain-code))
(use-package! chatgpt-shell
  :init
  (setq chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))
  :config
  (map! :nv "g!" #'fp/evil:explain-code
        :leader
        (:prefix ("!" . "AI")
         :desc "ChatGPT minibuffer prompt" "g" #'chatgpt-shell-prompt
         :desc "ChatGPT prompt" "G" #'chatgpt-shell)))
(use-package! dall-e-shell
  :init
  (setq dall-e-shell-openai-key (getenv "OPENAI_API_KEY")))
(after! org
  (require 'ob-chatgpt-shell)
  (require 'ob-dall-e-shell))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
