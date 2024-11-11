;;; init-swift.el --- Init lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Taken from https://github.com/corgi-emacs/corgi-packages/blob/main/corgi-clojure/corgi-clojure.el
;;
;; Filename: corgi-clojure.el
;; Package-Requires: ((use-package) (cider) (clj-ns-name) (clojure-mode))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;; Add MELPA as a package source

;;; Locate sourcekit-lsp
(defun find-sourcekit-lsp ()
  (or (executable-find "sourcekit-lsp")
      (and (eq system-type 'darwin)
           (string-trim (shell-command-to-string "xcrun -f sourcekit-lsp")))
      "/.local/swift6.0.2/usr/bin/sourcekit-lsp"))

;;; Packages we want installed for Swift development

;; .editorconfig file support
(use-package editorconfig
    :ensure t
    :config (editorconfig-mode +1))

;; Swift editing support
(use-package swift-mode
    :ensure t
    :mode "\\.swift\\'"
    :interpreter "swift")

;; Used to interface with swift-lsp.
;; (use-package lsp-mode
;;     :ensure t
;;     :commands lsp
;;     :hook ((swift-mode . lsp)))

;; lsp-mode's UI modules



;; sourcekit-lsp support
(use-package lsp-sourcekit
    :ensure t
    :after lsp-mode
    :custom
    (lsp-sourcekit-executable (find-sourcekit-lsp) "Find sourcekit-lsp"))


;; Spaceline


;;; Don't display the start screen


;;; Disable the toolbar

(provide 'init-swift)
;;; init-lisp.el ends here
