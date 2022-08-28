;; init-chinese.el --- Initialize Chinese configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Chinese configurations.
;;

;;; Code:
;; (cnfonts-mode 1)

(use-package cnfonts
  :config
  ;; 添加两个字号增大缩小的快捷键
  (define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
  (define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)

  (setq cnfonts-personal-fontnames '(("Roboto" "Roboto Light" "Roboto Mono Light" "Bookerly") ("LXGW Wenkai" "LXGW Wenkai GB Screen") ("HanaMin" "Noto Sans") ("Fira Code Symbol")))

  (setq cnfonts-profiles
        '("Reading" "Org Mode" "Programming"))
  )

(defvar my-line-spacing-alist
  '((9 . 0.1) (10 . 0.9) (11.5 . 0.2)
    (12.5 . 0.2) (14 . 0.2) (16 . 0.2)
    (18 . 0.2) (20 . 1.0) (22 . 0.2)
    (24 . 0.2) (26 . 0.2) (28 . 0.2)
    (30 . 0.2) (32 . 0.2)))

(defun my-line-spacing-setup (fontsizes-list)
  (let ((fontsize (car fontsizes-list))
        (line-spacing-alist (cl-copy-list my-line-spacing-alist)))
    (dolist (list line-spacing-alist)
      (when (= fontsize (car list))
        (setq line-spacing-alist nil)
        (setq-default line-spacing (cdr list))))))

(add-hook 'cnfonts-set-font-finish-hook #'my-line-spacing-setup)

(provide 'init-chinese)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-chinese.el ends here
