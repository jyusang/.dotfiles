;; package manager
(setq package-enable-at-startup nil)
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
(straight-use-package 'use-package)

;; standard
(setq dired-listing-switches "-agho --group-directories-first")
(setq inhibit-startup-message t)
(setq visual-bell t)
(column-number-mode)
(global-display-line-numbers-mode t)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :font "Fira Code Light" :height 100)
(set-fringe-mode 8)
(tool-bar-mode -1)
(tooltip-mode -1)

;; eshell
(setq eshell-buffer-maximum-lines 10000
      eshell-history-size 10000
      eshell-scroll-to-bottom-on-input t)
(setq eshell-visual-commands '("less" "more" "top" "vi" "vim")
      eshell-visual-subcommands '(("git" "diff" "log" "show")))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  :straight t)

(use-package dired-single
  :commands
  (dired dired-jump)
  :straight t)

(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
	`((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  :straight t)

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  :config
  (load-theme 'doom-solarized-light t)
  (doom-themes-visual-bell-config)
  :straight t)

(use-package evil
  :init
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  :straight t)

(use-package evil-collection
  :config
  (evil-collection-init)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer)
  :straight t)

(use-package lsp-mode
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (python-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands
  (lsp lsp-deferred)
  :straight t)

(use-package ivy
  :init
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1)
  :straight t)

(use-package counsel
  :config
  (counsel-mode 1)
  :straight t)

(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode 1)
  :straight t)

(use-package projectile
  :config
  (projectile-mode 1)
  :custom
  (projectile-completion-system 'ivy)
  :straight t)

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1)
  :bind-keymap
  (("C-c p" . projectile-command-map))
  :straight t)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :straight t)

(use-package magit
  :straight t)

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  :straight t)

(use-package flycheck
  :init
  (global-flycheck-mode)
  :straight t)

(use-package company
  :config
  (global-company-mode)
  :straight t)

(use-package lsp-ivy
  :commands
  (lsp-ivy-workspace-symbol)
  :straight t)

(use-package lsp-pyright
  :hook
  (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  :straight t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (cd default-directory)
            (eshell)))
