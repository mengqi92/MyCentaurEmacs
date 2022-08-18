;; init-finance.el --- Initialize Finance configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Finance configurations.
;;

;;; Code:

(use-package beancount
  :mode (("\\.beancount\\'" . beancount-mode))
  :hook (
         (beancount-mode . outline-minor-mode)
         (beancount-mode . (lambda () (setq-local electric-indent-chars nil))))
  :straight `(:type git :host github :repo "beancount/beancount-mode")
  )

(provide 'init-finance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-finance.el ends here
