;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; (setq centaur-logo nil)                        ; Logo file or nil (official logo)
(setq centaur-full-name "Mengqi Pei")           ; User full name
(setq centaur-mail-address "mengqipei@gmail.com")   ; Email address
(setq centaur-proxy "127.0.0.1:1080")          ; HTTP/HTTPS proxy
(setq centaur-socks-proxy "127.0.0.1:1081")    ; SOCKS proxy
;; (setq centaur-server nil)                      ; Enable `server-mode' or not: t or nil
;; (setq centaur-icon nil)                        ; Display icons or not: t or nil
(setq centaur-package-archives 'tuna)         ; Package repo: melpa, emacs-cn, netease, ustc, tencent or tuna
(setq centaur-theme 'light)                     ; Color theme: auto, random, system, default, pro, dark, light, warm, cold, day or night
;; (setq centaur-completion-style 'minibuffer)    ; Completion display style: minibuffer or childframe
;; (setq centaur-dashboard nil)                   ; Display dashboard at startup or not: t or nil
(setq centaur-restore-frame-geometry t)      ; Restore the frame's geometry at startup: t or nil
;; (setq centaur-lsp 'eglot)                      ; Set LSP client: lsp-mode, eglot or nil
;; (setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode python-mode markdown-mode)) ; Ignore format on save for some languages
;; (setq centaur-tree-sitter t)                   ; Enable `tree-sitter' or not: t or nil
;; (setq centaur-chinese-calendar t)              ; Support Chinese calendar or not: t or nil
;; (setq centaur-player t)                        ; Enable players or not: t or nil
(setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications. Nil to use font supports ligatures.
;; (setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))


;; Fonts
(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    ;; (cl-loop for font in '("Roboto" "Fira Code" "Bookerly" "Cascadia Code" "Jetbrains Mono"
    ;;                        "SF Mono" "Hack" "Source Code Pro" "Menlo"
    ;;                        "Monaco" "DejaVu Sans Mono" "Consolas")
    ;;          when (font-installedfon-t p)
    ;;          return (set-face-attribute 'nil default
    ;;                                     :family font
    ;;                                     :height (cond (sys/macp 180)
    ;;                                                   (sys/win32p 110)
    ;;                                                   (t 160))))

    ;; ;; Set mode-line font
    ;; ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
    ;; ;;          when (font-installed-p font)
    ;; ;;          return (progn
    ;; ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;; ;;                   (when (facep 'mode-line-active)
    ;; ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;; ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; ;; Specify font for all unicode characters
    ;; (cl-loop for font in '("Noto Sans" "Segoe UI Symbol" "Symbola" "Symbol")
    ;;          when (font-installed-p font)
    ;;          return (set-fontset-font t 'unicode font nil 'prepend))

    ;; ;; Emoji
    ;; (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji")
    ;;          when (font-installed-p font)
    ;;          return (if (>= emacs-major-version 28)
    ;;                     (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend)
    ;;                   (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; ;; Specify font for Chinese characters
    ;; (cl-loop for font in '("Noto Sans CJK SC" "LXGW WenKai" "WenQuanYi Micro Hei" "Microsoft Yahei")
    ;;          when (font-installed-p font)
    ;;          return (set-fontset-font t '(#x4e00 . #x9fff) font))
    ;; (cnfonts-mode 1)
    (nano-modeline-mode)))

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)

;;设置窗口位置为屏幕左上角(0,0)
(set-frame-position (selected-frame) 0 0)

(set-frame-width (selected-frame) 110)
(set-frame-height (selected-frame) 33)

;; 快捷键设置
;; 设置选择区域快捷键
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)

;; Mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;                                    user-mail-address nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; Calendar
;; Set location , then press `S' can show the time of sunrise and sunset
(setq calendar-location-name "Shanghai"
      calendar-latitude 31.230416
      calendar-longitude 121.473701)

;; Misc.
(setq confirm-kill-emacs 'y-or-n-p)

;; Enable proxy
;; (proxy-http-enable)
;; (proxy-socks-enable)

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f0eb51d80f73b247eb03ab216f94e9f86177863fb7e48b44aacaddbfe3357cf1" default)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0))))
 '(aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
 '(cfrs-border-color ((t (:background "#676E95"))))
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
 '(diff-hl-change ((t (:foreground "#82aaff" :background nil))))
 '(diff-hl-delete ((t (:inherit diff-removed :background nil))))
 '(diff-hl-insert ((t (:inherit diff-added :background nil))))
 '(flycheck-posframe-background-face ((t (:inherit tooltip))))
 '(flycheck-posframe-border-face ((t (:inherit posframe-border))))
 '(flycheck-posframe-face ((t (:foreground "#c3e88d"))))
 '(flycheck-posframe-info-face ((t (:foreground "#c3e88d"))))
 '(git-timemachine-minibuffer-author-face ((t (:inherit success))))
 '(git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
 '(hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :underline nil :inverse-video t))))
 '(ivy-minibuffer-match-face-1 ((t (:foreground "dimgray" :distant-foreground nil :background nil))))
 '(ivy-minibuffer-match-face-2 ((t (:distant-foreground nil :background nil))))
 '(ivy-minibuffer-match-face-3 ((t (:distant-foreground nil :background nil))))
 '(ivy-minibuffer-match-face-4 ((t (:distant-foreground nil :background nil))))
 '(ivy-posframe ((t (:inherit tooltip))))
 '(ivy-posframe-border ((t (:inherit posframe-border))))
 '(lsp-headerline-breadcrumb-path-error-face ((t :underline (:style wave :color "#ff5370") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-hint-face ((t :underline (:style wave :color "#c3e88d") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-info-face ((t :underline (:style wave :color "#c3e88d") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-warning-face ((t :underline (:style wave :color "#ffcb6b") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-symbols-error-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#ff5370"))))
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#c3e88d"))))
 '(lsp-headerline-breadcrumb-symbols-info-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#c3e88d"))))
 '(lsp-headerline-breadcrumb-symbols-warning-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#ffcb6b"))))
 '(lsp-ui-sideline-code-action ((t (:inherit warning))))
 '(macrostep-expansion-highlight-face ((t (:inherit tooltip :extend t))))
 '(org-ellipsis ((t (:foreground nil))))
 '(org-pomodoro-mode-line ((t (:inherit warning))))
 '(org-pomodoro-mode-line-break ((t (:inherit success))))
 '(org-pomodoro-mode-line-overtime ((t (:inherit error))))
 '(paradox-archive-face ((t (:inherit font-lock-doc-face))))
 '(paradox-description-face ((t (:inherit completions-annotations))))
 '(pulse-highlight-face ((t (:inherit region :extend t))))
 '(pulse-highlight-start-face ((t (:inherit region))))
 '(symbol-overlay-default-face ((t (:inherit (region bold)))))
 '(transient-posframe ((t (:inherit tooltip))))
 '(transient-posframe-border ((t (:inherit posframe-border))))
 '(which-key-posframe ((t (:inherit tooltip))))
 '(which-key-posframe-border ((t (:inherit posframe-border))))
 '(ztreep-arrow-face ((t (:inherit font-lock-comment-face))))
 '(ztreep-diff-header-face ((t (:inherit (diff-header bold)))))
 '(ztreep-diff-header-small-face ((t (:inherit diff-file-header))))
 '(ztreep-diff-model-add-face ((t (:inherit diff-nonexistent))))
 '(ztreep-diff-model-diff-face ((t (:inherit diff-removed))))
 '(ztreep-diff-model-ignored-face ((t (:inherit font-lock-doc-face :strike-through t))))
 '(ztreep-diff-model-normal-face ((t (:inherit font-lock-doc-face))))
 '(ztreep-expand-sign-face ((t (:inherit font-lock-function-name-face))))
 '(ztreep-header-face ((t (:inherit diff-header))))
 '(ztreep-leaf-face ((t (:inherit diff-index))))
 '(ztreep-node-face ((t (:inherit font-lock-variable-name-face)))))

;;; custom.el ends here
