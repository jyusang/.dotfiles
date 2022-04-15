(use-package elixir-mode
  :defer 0
  :straight t)

(use-package geiser
  :defer 0
  :straight t)

(use-package go-mode
  :defer 0
  :straight t)

(use-package haskell-mode
  :defer 0
  :straight t)

(use-package json-mode
  :defer 0
  :straight t)

(use-package markdown-mode
  :defer 0
  :straight t)

(use-package nix-mode
  :defer 0
  :straight t)

(use-package rust-mode
  :defer 0
  :straight t)

(use-package tide
  :defer 0
  :hook ((js-mode . (lambda () (tide-setup)))
	 (typescript-mode . (lambda () (tide-setup)))
	 (web-mode . (lambda () (tide-setup))))
  :straight t)

(use-package web-mode
  :mode ("\\.jsx\\'"
         "\\.tsx\\'")
  :straight t)
