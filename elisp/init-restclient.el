;;; init-restclient.el --- Restclient -*- lexical-binding: t -*-
;;; Commentary:
;;; Taken from https://github.com/corgi-emacs/corgi-packages/blob/main/corgi-clojure/corgi-clojure.el
;;
;; Filename: corgi-clojure.el
;; Package-Requires: ((use-package) (cider) (clj-ns-name) (clojure-mode))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

(provide 'init-restclient)
