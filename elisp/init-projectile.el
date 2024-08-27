;;; init-projectile.el --- -*- lexical-binding: t -*-
;; ProjPac
(use-package projectile

  :bind
  ("C-c p" . projectile-command-map)

  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (when (and *sys/win32*
             (executable-find "tr"))
    (setq projectile-indexing-method 'alien))
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))
;; -ProjPac

(provide 'init-projectile)
