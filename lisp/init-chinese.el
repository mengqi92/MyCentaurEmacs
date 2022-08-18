;; init-chinese.el --- Initialize Chinese configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Chinese configurations.
;;

;;; Code:

(use-package cnfonts
  :config
  ;; 让 cnfonts 在 Emacs 启动时自动生效。
  (cnfonts-mode 1)
  ;; 添加两个字号增大缩小的快捷键
  (define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
  (define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)

  (setq cnfonts-personal-fontnames '(("Bookerly") ("LXGW Wenkai") nil ("Fira Code Symbol")))

  (setq cnfonts-profiles
        '("Reading" "Org Mode" "Programming"))
  )

(provide 'init-chinese)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-chinese.el ends here
