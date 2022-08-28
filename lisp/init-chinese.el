;; init-chinese.el --- Initialize Chinese configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Chinese configurations.
;;

;;; Code:

(use-package cnfonts
  :config
  ;; 添加两个字号增大缩小的快捷键
  (define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
  (define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)

  (setq cnfonts-personal-fontnames '(("Roboto" "Roboto Light" "Roboto Mono Light" "Bookerly") ("LXGW Wenkai" "LXGW Wenkai GB Screen") ("HanaMin" "Noto Sans") ("Fira Code Symbol")))

  (setq cnfonts-profiles
        '("Reading" "Org Mode" "Programming"))
  )

(provide 'init-chinese)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-chinese.el ends here
