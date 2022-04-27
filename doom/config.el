;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Mark Flarup-Jensen"
      user-mail-address "mfj@stibodx.com")

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
;; (setq doom-font (font-spec :family "monospace" :size 24 :weight 'semi-light))
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;; (setq doom-font (font-spec :family "Inconsolata" :size 24))
;; (setq doom-font (font-spec :family "Menlo" :size 14))
(setq doom-font (font-spec :family "MesloLGS NF" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-ayu-mirage)
;; (load-theme 'github t)
;; (load-theme 'gruvbox-dark-soft t)
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-solarized-light)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org-files")



;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq exec-path-from-shell-debug t)
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

;; Set cursor colors on mode change
(unless (display-graphic-p)
  (evil-terminal-cursor-changer-activate))

(when (display-graphic-p)
(setq evil-emacs-state-cursor '("red" box)
    evil-normal-state-cursor '("green" box)
    evil-visual-state-cursor '("orange" box)
    evil-insert-state-cursor '("red" bar)
    evil-replace-state-cursor '("red" bar)
    evil-operator-state-cursor '("red" hollow)))

(defun normal-mode-and-save ()
  "Enter 'evil-normal-state and 'evil-write in sequence."
  (interactive)
  (evil-normal-state)
  (evil-write nil nil))

;; Save buffers with Ctrl+S
(global-set-key (kbd "C-s") 'normal-mode-and-save)
(global-set-key (kbd "C-q") 'evil-quit)

;; Set shell indent to 2 spaces
(add-hook! sh-mode
 (setq sh-basic-offset 2)
 (setq tab-width 2))

;; Override shfmt to use two spaces instead of tabs
(after! format-all
  (set-formatter! 'shfmt
    '("shfmt"
      "-i" "2"
      ;; Mode selection copied from the default config
      ("-ln" "%s" (cl-case (and (boundp 'sh-shell) (symbol-value 'sh-shell))
                    (bash "bash") (mksh "mksh") (t "posix"))))
    :modes 'sh-mode))

;; Set up flycheck checkers combined with lsp
;; https://github.com/hlissner/doom-emacs/issues/1530#issuecomment-725588733
(add-hook! 'lsp-after-initialize-hook
  (run-hooks (intern (format "%s-lsp-hook" major-mode))))

(defun sh-flycheck-setup ()
  (flycheck-add-next-checker 'lsp 'sh-shellcheck))
(add-hook 'sh-mode-lsp-hook #'sh-flycheck-setup)

(defun go-flycheck-setup ()
  (flycheck-add-next-checker 'lsp 'golangci-lint)
  (setq flycheck-golangci-lint-enable-all t)
  (setq flycheck-golangci-lint-disable-linters
     '("gochecknoinits" "gochecknoglobals" "gomnd" "gofumpt" "gci" "exhaustivestruct")))
(add-hook 'go-mode-lsp-hook #'go-flycheck-setup)

;; Disable the golang lsp formatter - it does not clean up imports
;; https://github.com/hlissner/doom-emacs/tree/develop/modules/editor/format#disabling-the-lsp-formatter
(setq-hook! 'go-mode-hook +format-with-lsp nil)

;;(add-hook! go-mode
;;  (setq gofmt-command "goreturns")
;;  (setq lsp-gopls-complete-unimported t))

(add-hook! go-mode
  (setq gofmt-command "goimports")
  (setq lsp-gopls-complete-unimported t)
  (setq lsp-go-env '(GOFLAGS "-tags=integration")))

(use-package dap-mode
  :config
  (dap-mode 1)
  (setq dap-print-io t)
  (require 'dap-hydra)
  (require 'dap-go)
  (dap-go-setup)
  (use-package dap-ui
    :ensure nil
    :config
    (dap-ui-mode 1)
    )
  )

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

(setq doom-modeline-vcs-max-length 50)

;; Lookup reference fix
;; https://github.com/hlissner/doom-emacs/issues/4894
;; (add-hook! lsp-mode
;;   (defalias '+lookup/references 'lsp-find-references))
;; Disable the documentation pop-up
(setq lsp-ui-doc-enable nil)

(use-package anki-editor
  :after org-noter
  :config
  ; I like making decks
  (setq anki-editor-create-decks 't))

;; org-roam specifics:
(use-package org-roam
  :ensure t
  :demand t  ;; Ensure org-roam is loaded by default
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dropbox/org-roam")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(
     ("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t))
   )
 :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(map! :leader
  :desc "Insert (skipping org-capture)"
  "n r I" #'org-roam-node-insert-immediate)
