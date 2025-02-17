;;; init-clojure.el --- Clojure configuration for Corgi -*- lexical-binding: t -*-
;;; Commentary:
;;; Taken from https://github.com/corgi-emacs/corgi-packages/blob/main/corgi-clojure/corgi-clojure.el
;;
;; Filename: corgi-clojure.el
;; Package-Requires: ((use-package) (cider) (clj-ns-name) (clojure-mode))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package elixir-ts-mode
  :init
    (add-to-list 'exec-path "~/.local/elixir-ls/")
    :ensure t)

(provide 'init-elixir)
;;; init-elixir.el ends here
