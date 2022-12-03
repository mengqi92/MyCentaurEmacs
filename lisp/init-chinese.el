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
        '("MonoReading" "Reading" "Programming"))
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

;; Ace-jump-pinyin
;; Jump to Chinese characters
(use-package ace-pinyin
  :diminish
  :hook (after-init . ace-pinyin-global-mode))

;; Evil find (f/F/t/T) pinyin support
(use-package evil-find-char-pinyin
  :config
  (evil-find-char-pinyin-mode +1))

;; RIME input method
(use-package rime
  :custom
  (default-input-method "rime")
  :config
  (setq rime-user-data-dir "~/emacs-rime/")
  (setq rime-share-data-dir "/usr/share/rime-data")
  (when on-macbook (setq rime-librime-root "~/.emacs.d/librime/dist"))
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
                                          rime-predicate-punctuation-line-begin-p))

  (add-hook 'beancount-mode-hook
            (lambda () (setq-local rime-disable-predicates
                              '(rime-predicate-evil-mode-p
                                +rime-predicate-beancount-p
                                rime-predicate-auto-english-p
                                rime-predicate-punctuation-line-begin-p))))

  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq-local rime-disable-predicates
                              '(rime-predicate-evil-mode-p
                                rime-predicate-prog-in-code-p))))

  (add-hook 'org-mode-hook
            (lambda () (setq-local rime-disable-predicates
                              '(rime-predicate-evil-mode-p
                                rime-predicate-auto-english-p
                                rime-predicate-org-latex-mode-p
                                rime-predicate-org-in-src-block-p
                                rime-predicate-punctuation-line-begin-p))))

  (add-hook 'org-roam-mode-hook
            (lambda () (setq-local rime-disable-predicates
                              '(rime-predicate-evil-mode-p
                                rime-predicate-auto-english-p
                                rime-predicate-punctuation-line-begin-p)))))

;; [[https://emacs-china.org/t/org-mode/22313][不用零宽空格在 org-mode 中标记中文的办法]]
(font-lock-add-keywords 'org-mode
                        '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9]*?[/+*_=~]\\( \\)?\\cc?"
                           (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
                          ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9]*?[/+*_=~]\\( \\)\\cc"
                           (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
                        'append)
;; 导出时删掉空格
(with-eval-after-load 'ox
  (defun eli-strip-ws-maybe (text _backend _info)
    (let* ((text (replace-regexp-in-string
                  "\\(\\cc\\) *\n *\\(\\cc\\)"
                  "\\1\\2" text));; remove whitespace from line break
           ;; remove whitespace from `org-emphasis-alist'
           (text (replace-regexp-in-string "\\(\\cc?\\) \\(.*?\\) \\(\\cc\\)"
                                           "\\1\\2\\3" text))
           ;; restore whitespace between English words and Chinese words
           (text (replace-regexp-in-string "\\(\\cc\\)\\(\\(?:<[^>]+>\\)?[a-z0-9A-Z-]+\\(?:<[^>]+>\\)?\\)\\(\\cc\\)"
                                           "\\1 \\2 \\3" text))
           (text (replace-regexp-in-string "\\(\\cc\\) ?\\(\\\\[^{}()]*?\\)\\(\\cc\\)"
                                           "\\1 \\2 \\3" text)))
      text))
  (add-to-list 'org-export-filter-paragraph-functions #'eli-strip-ws-maybe))

;; 限制 emphasis 的范围
(defun org-do-emphasis-faces (limit)
  "Run through the buffer and emphasize strings."
  (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\).*?[~=*/_+]"
    		              (car org-emphasis-regexp-components))))
    (catch :exit
      (while (re-search-forward quick-re limit t)
        (let* ((marker (match-string 2))
               (verbatim? (member marker '("~" "="))))
          (when (save-excursion
    	          (goto-char (match-beginning 0))
    	          (and
    	           ;; Do not match table hlines.
    	           (not (and (equal marker "+")
    		                 (org-match-line
    		                  "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
    	           ;; Do not match headline stars.  Do not consider
    	           ;; stars of a headline as closing marker for bold
    	           ;; markup either.
    	           (not (and (equal marker "*")
    		                 (save-excursion
    		                   (forward-char)
    		                   (skip-chars-backward "*")
    		                   (looking-at-p org-outline-regexp-bol))))
    	           ;; Match full emphasis markup regexp.
    	           (looking-at (if verbatim? org-verbatim-re org-emph-re))
    	           ;; Do not span over paragraph boundaries.
    	           (not (string-match-p org-element-paragraph-separate
    				                    (match-string 2)))
    	           ;; Do not span over cells in table rows.
    	           (not (and (save-match-data (org-match-line "[ \t]*|"))
    		                 (string-match-p "|" (match-string 4))))))
            (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist))
    		            (m (if org-hide-emphasis-markers 4 2)))
              (font-lock-prepend-text-property
               (match-beginning m) (match-end m) 'face face)
              (when verbatim?
    	        (org-remove-flyspell-overlays-in
    	         (match-beginning 0) (match-end 0))
    	        (remove-text-properties (match-beginning 2) (match-end 2)
    				                    '(display t invisible t intangible t)))
              (add-text-properties (match-beginning 2) (match-end 2)
    			                   '(font-lock-multiline t org-emphasis t))
              (when (and org-hide-emphasis-markers
    		             (not (org-at-comment-p)))
    	        (add-text-properties (match-end 4) (match-beginning 5)
    			                     '(invisible t))
    	        (add-text-properties (match-beginning 3) (match-end 3)
    			                     '(invisible t)))
              (throw :exit t))))))))

(provide 'init-chinese)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-chinese.el ends here
