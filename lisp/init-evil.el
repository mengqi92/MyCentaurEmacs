;; init-evil.el --- Initialize Evil configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Evil configurations.
;;

;;; Code:

(setq evil-want-keybinding nil)
(use-package evil
  :config
  (evil-mode 1))

(use-package evil-collection
  :init (evil-collection-init))

(provide 'init-evil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-evil.el ends here
