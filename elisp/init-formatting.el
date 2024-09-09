;;; init-formatting.el --- Init formatting -*- lexical-binding: t -*-
;;; Commentary:
;;; Taken from https://github.com/corgi-emacs/corgi-packages/blob/main/corgi-clojure/corgi-clojure.el
;;
;; Filename: corgi-clojure.el
;; Package-Requires: ((use-package) (cider) (clj-ns-name) (clojure-mode))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:



(use-package apheleia
  :ensure apheleia
  :diminish ""                          ; Don't show in modeline
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "-i=4" "-sr" "-kp"))
  ;; https://git.genehack.net/os/emacs/issues/2
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (apheleia-global-mode +1))

(use-package prettier-js
  :diminish
  :bind ("C-c C-p" . prettier-js)
  :hook ((js-mode js2-mode  js-ts-mode javascript-ts-modejson-mode web-mode css-mode sgml-mode html-mode typescript-ts-mode)
         .
         prettier-js-mode))

(use-package format-all
  :bind ("C-c C-f" . format-all-buffer))


(provide 'init-formatting)
;;; init-formatting.el ends here
