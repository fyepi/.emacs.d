;;; init-lisp.el --- Init lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Taken from https://github.com/corgi-emacs/corgi-packages/blob/main/corgi-clojure/corgi-clojure.el
;;
;; Filename: corgi-clojure.el
;; Package-Requires: ((use-package) (cider) (clj-ns-name) (clojure-mode))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; CruxPac
(use-package slime
  :hook (lisp-mode . slime-mode)
  :config
  (setq inferior-lisp-program "sbcl"))
;; -CruxPac

(provide 'init-lisp)
;;; init-lisp.el ends here
