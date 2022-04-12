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
(setq completion-styles '(flex))
(setq dired-listing-switches "-agho")
(setq inhibit-startup-message t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(column-number-mode)
(global-display-line-numbers-mode t)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(menu-bar-mode -1)
(scroll-bar-mode -1)
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

(use-package selectrum
  :config
  (selectrum-mode 1)
  :straight t)

(use-package company
  :config
  (global-company-mode)
  :straight t)

(use-package marginalia
  :config
  (marginalia-mode)
  :straight t)

(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode 1)
  :straight t)

(use-package consult
  :init
  (setq consult-ripgrep-args "rg --color never --glob !.git/ --hidden --line-buffered --line-number --max-columns 1000 --no-heading --smart-case --null .")
  :config
  (defalias 'rg 'consult-ripgrep)
  :straight t)

(use-package magit
  :straight t)

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  :straight t)

(use-package eglot
  :hook
  (python-mode . eglot-ensure)
  :straight t)

(use-package format-all
  :hook
  (prog-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter)
  :straight t)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "startup-time: %s"
		     (format "%.2fs" (float-time (time-subtract
						  after-init-time
						  before-init-time))))))
