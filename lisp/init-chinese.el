;; init-chinese.el --- Initialize Chinese configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Chinese configurations.
;;

;;; Code:
(use-package cnfonts
  :ensure t
  :after all-the-icons
  :hook (cnfonts-set-font-finish
         . (lambda (fontsizes-list)
             (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
             (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
             (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
             (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
             (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
             (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append)))
  :config
  ;; 添加两个字号增大缩小的快捷键
  (define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
  (define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)

  (setq cnfonts-personal-fontnames '(("Roboto" "Roboto Light" "Roboto Mono Light" "Bookerly") ("LXGW Wenkai" "LXGW Wenkai GB Screen") ("HanaMin" "Noto Sans") ("Fira Code Symbol")))

  (setq cnfonts-profiles
        '("Reading" "Org Mode" "Programming"))
  (cnfonts-enable))

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

;; RIME input method
(use-package rime
  :custom
  (default-input-method "rime")
  :config
  (setq rime-user-data-dir "~/rime/")
  (setq rime-share-data-dir "/usr/share/rime-data")
  ;; 断言成立时关闭输入法
  ;; (setq rime-disable-predicates t)
  ;; 断言成立时进入 inline ascii 模式
  ;; (setq rime-inline-predicates t)

  (defun +rime-predicate-beancount-p ()
    "Predicate input state in `beancount-mode.'

Determines whether current buffer's `major-mode' is
`beancount-mode', and the cursor is at the beginning of the
line."
    (when (derived-mode-p 'beancount-mode)
      (not (or (nth 3 (syntax-ppss))
               (nth 4 (syntax-ppss))))))

  (setq-default rime-disable-predicates '(rime-predicate-evil-mode-p
                                          rime-predicate-auto-english-p
                                          rime-predicate-punctuation-line-begin-p))

  (add-hook 'beancount-mode-hook
            (lambda () (setq-local rime-disable-predicates
                               '(rime-predicate-evil-mode-p
                                 +rime-predicate-beancount-p
                                 rime-predicate-auto-english-p
                                 rime--punctuation-line-begin-p))))
  )

(provide 'init-chinese)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-chinese.el ends here
