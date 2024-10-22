;;; init-projectile.el --- -*- lexical-binding: t -*-
;; ProjPac
(use-package projectile

  :bind
  ("C-c p" . projectile-command-map)

  :custom
  (projectile-completion-system 'ivy)
  )
;; -ProjPac

(provide 'init-projectile)
