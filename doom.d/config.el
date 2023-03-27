;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Fabio Papa"
      user-mail-address "fpapa@springhealth.com")

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
(setq doom-big-font "Operator Mono-22:weight=semilight")
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

;; Force emacs to use a specific version of node installed by nvm
(setenv "PATH"
        (concat (getenv "HOME") "/.nvm/versions/node/v18.14.2/bin:"
                (getenv "PATH")))
(setq exec-path
      (append (list (concat (getenv "HOME") "/.nvm/versions/node/v18.14.2/bin")) exec-path))

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
;; let me use C-k to insert a digraph in org-mode
(use-package! evil-org
  :config
  (map! :map evil-org-mode-map
        :i "C-k" #'evil-insert-digraph))
(setq org-roam-directory (file-truename "~/org-roam"))

(setq projectile-project-search-path
      '("~/dev/" "~/SpringCare/"))

;;;;;;;;;;;;;;;;;;
;; Ruby & Rails ;;
;;;;;;;;;;;;;;;;;;
(after! lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("ruby-lsp"))
                    :major-modes '(ruby-mode)
                    :priority 1
                    :server-id 'ruby-lsp))
  (setq lsp-clients-ruby-server-command '("ruby-lsp")))
(use-package! projectile-rails
  :init
  (setq projectile-rails-custom-server-command nil))

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

(setq rbenv-installation-dir "/usr/local")

(setq! web-mode-markup-indent-offset 2)
(setq! web-mode-code-indent-offset 2)
(setq-hook! 'js-mode-hook
  js-indent-level 2)

(after! js2-mode
  (setq-hook! 'js2-mode-hook
    flycheck-checker 'javascript-eslint ; use eslint for linting
    +format-with-lsp nil
    +format-with '(prettier))
  (setq-hook! 'js2-mode-hook
    js2-basic-offset 2))

(with-eval-after-load "ox-latex"
  (setq org-latex-toc-command "\\tableofcontents \\clearpage")
  (add-to-list 'org-latex-classes
               '("scrartcl" "\\documentclass[letterpaper]{scrartcl}"
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

;; Ensure necessary packages
(after! ruby-mode
  ;; Select checker and formatter conditionally
  (defun fp/set-ruby-linter-and-formatter ()
    "Use StandardRB when available, fallback to RuboCop."
    (interactive)
    (if (flycheck-checker-supports-major-mode-p 'ruby-standard 'ruby-mode)
        (progn
          (setq-local flycheck-checker 'ruby-standard)
          (setq-local +format-with-lsp nil)
          (setq-local +format-with '(ruby-standard)))
      (progn
        (setq-local flycheck-checker 'ruby-rubocop)
        (setq-local +format-with-lsp nil)
        (setq-local +format-with '(rubocop)))))

  (add-hook 'ruby-mode-hook #'fp/set-ruby-linter-and-formatter))

(after! apheleia
  (setf (alist-get 'erb-format apheleia-formatters)
        '("erb-format" "--stdin-filename" filepath))

  (add-to-list 'apheleia-mode-alist '("\\.erb\\'" . erb-format)))

(after! web-mode
  (add-to-list 'lsp-language-id-configuration '(".*\\.erb\\'" . "html"))

  (use-package! lsp-tailwindcss
    :init
    (setq! lsp-tailwindcss-add-on-mode t))

  (add-hook! 'web-mode-local-vars-hook
    (lambda ()
      ;; Set formatter based on extension
      (let ((ext (file-name-extension (or buffer-file-name ""))))
        (cond
         ((string= ext "erb")
          (setq-local web-mode-engines-alist '(("erb" . "\\.erb\\'")))
          (setq-local +format-with 'erb-format)
          (setq-local apheleia-formatter 'erb-format))
         ((string= ext "html")
          (setq-local +format-with 'prettier-html)
          (setq-local apheleia-formatter 'prettier-html))))
      (setq-local +format-with-lsp nil)
      (lsp-deferred))))

;; For CSS mode
(add-hook! 'css-mode-hook
  (lambda ()
    (setq-local +format-with '(prettier-css))
    (setq-local +format-with-lsp nil)))

;; (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
;; (setq forge-topic-list-approves-fast-track-messages '(":sheep: it!"))

;; accept completion from copilot and fall back to company
;; (use-package! copilot
;;   :hook (prog-mode . copilot-mode)
;;   :bind (("<backtab>" . 'copilot-accept-completion-by-word)
;;          ("S-<tab>" . 'copilot-accept-completion-by-word)
;;          ("s-<down>" . 'copilot-next-completion)
;;          ("s-<up>" . 'copilot-previous-completion)
;;          ("C-c a" . 'copilot-accept-completion)))

;; (use-package chatgpt-shell
;;   :ensure t
;;   :custom
;;   ((chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))))
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
  (setq chatgpt-shell-anthropic-key (getenv "ANTHROPIC_API_KEY"))
  (setq chatgpt-shell-system-prompt "Programming")
  :config
  (setq chatgpt-shell-system-prompts
        (append
         '(("Bible" . "You are a Bible-believing, reformed Bible scholar that specializes in explaining complex parts of the Bible in an approachable way. You often give illustrations and examples of what you are explaining."))
         chatgpt-shell-system-prompts))
  (evil-define-operator fp/evil-chatgpt-compose (beg end)
    :move-point nil
    (deactivate-mark)
    (goto-char end)
    (set-mark (point))
    (goto-char beg)
    (activate-mark)
    (chatgpt-shell-prompt-compose nil))
  (map! :n "g!" #'fp/evil-chatgpt-compose)
  (map! :leader
        (:prefix ("!" . "AI")
         :desc "Aidermacs Menu" "a" #'aidermacs-transient-menu
         :desc "Open ChatGPT Shell" "G" #'chatgpt-shell
         :desc "ChatGPT prompt in the mini-buffer" "g" #'chatgpt-shell-prompt))
  (after! evil
    (add-hook 'chatgpt-shell-prompt-compose-mode-hook
              (lambda ()
                (evil-local-set-key 'normal "r" #'chatgpt-shell-prompt-compose-reply)
                (evil-local-set-key 'normal "q" #'chatgpt-shell-prompt-compose-quit-and-close-frame))))
  (defun fp/chatgpt-shell-triple-backtick-handler ()
    "Format triple backticks into a multi-line code block."
    (when (and (derived-mode-p 'chatgpt-shell-mode)
               (eq last-command-event ?`)
               (save-excursion
                 (goto-char (point))
                 (looking-back "```" 3)))
      (delete-char -3)
      (insert "```\n\n``")
      (forward-line -2)
      (goto-char (line-beginning-position 2))))
  (defun fp/chatgpt-shell-setup-triple-backticks ()
    "Setup triple backtick handling for chatgpt-shell-mode."
    (add-hook 'post-self-insert-hook #'fp/chatgpt-shell-triple-backtick-handler nil t))
  (add-hook 'chatgpt-shell-mode-hook #'fp/chatgpt-shell-setup-triple-backticks))

;; Aider
(use-package! aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "deepseek"))

(setq ispell-program-name "aspell")
(setq ispell-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

(after! projectile (setq projectile-project-root-files-bottom-up (remove ".git"
                                                                         projectile-project-root-files-bottom-up)))

(use-package! emmet-mode
  :hook ((html-mode css-mode web-mode) . emmet-mode)
  :config
  (setq emmet-expand-jsx-className? t)
  (define-key emmet-mode-keymap
              (kbd "TAB") 'emmet-expand))

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
