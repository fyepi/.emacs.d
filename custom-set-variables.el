;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values
   '((eval add-hook 'prog-mode-hook (lambda nil (whitespace-mode 1))
           (not :APPEND) :BUFFER-LOCAL)
     (eval let*
           ((x (dir-locals-find-file default-directory))
            (this-directory
             (if (listp x) (car x) (file-name-directory x))))
           (unless
               (or (featurep 'swift-project-settings)
                   (and (fboundp 'tramp-tramp-file-p)
                        (tramp-tramp-file-p this-directory)))
             (add-to-list 'load-path (concat this-directory "utils")
                          :append)
             (defvar swift-project-directory)
             (let ((swift-project-directory this-directory))
               (require 'swift-project-settings)))
           (set (make-local-variable 'swift-project-directory)
                this-directory)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
