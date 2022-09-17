;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2022 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(use-package org
  :ensure nil
  :commands (org-dynamic-block-define)
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org" :face 'all-the-icons-green :height 1.1 :v-adjust 0.0)
           :color blue :quit-key "q")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c x" . org-capture)
         :map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
         (org-mode . (lambda ()
                       "Beautify org symbols."
                       (when centaur-prettify-org-symbols-alist
                         (if prettify-symbols-alist
                             (push centaur-prettify-org-symbols-alist prettify-symbols-alist)
                           (setq prettify-symbols-alist centaur-prettify-org-symbols-alist)))
                       (prettify-symbols-mode 1)))
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; HACK: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))
  :config
  ;; For hydra
  (defun hot-expand (str &optional mod)
    "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  ;; To speed up startup, don't put to init section
  (setq org-modules nil                 ; Faster loading
        org-directory centaur-org-directory
        org-capture-templates
        `(("i" "Idea" entry (file ,(concat org-directory "/idea.org"))
           "*  %^{Title} %?\n%U\n%a\n")
          ("t" "Todo" entry (file ,(concat org-directory "/gtd.org"))
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "Note" entry (file ,(concat org-directory "/note.org"))
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+olp+datetree
                                ,(concat org-directory "/journal.org"))
           "*  %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
	      ("b" "Book" entry (file+olp+datetree
                             ,(concat org-directory "/book.org"))
	       "* Topic: %^{Description}  %^g %? Added: %U"))

        org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HOLD(h)" "|" "DONE(d)" "CANCELED(c)")
          (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
        org-todo-keyword-faces '(("HOLD" . warning)
                                 ("❓" . warning))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))

        ;; Agenda styling
        org-agenda-files `(,centaur-org-agenda-directory)
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────"

        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?⏷) "\t⏷" nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t)


  ;; Emphasis faces
  (defface my-org-verbatim
    '((default :inherit org-verbatim)
      (((class color) (min-colors 88) (background light))
       :foreground "#aa0000" :underline t :overline t)
      (((class color) (min-colors 88) (background dark))
       :foreground "#ffa059" :underline t :overline t)
      )
    "My custom face for org verbatim.")
  (add-to-list 'org-emphasis-alist
               `("=" my-org-verbatim verbatim))

  ;; (setq org-emphasis-alist
  ;;       '(("*" bold)
  ;;         ("/" italic)
  ;;         ("_" underline)
  ;;         ("=" my-org-verbatim verbatim)
  ;;         ("~" org-code verbatim)
  ;;         ("+" (:strike-through t)))
  ;;       )

  ;; Add new template
  (add-to-list 'org-structure-template-alist '("n" . "note"))

  ;; Use embedded webkit browser if possible
  (when (featurep 'xwidget-internal)
    (push '("\\.\\(x?html?\\|pdf\\)\\'"
            .
            (lambda (file _link)
              (centaur-webkit-browse-url (concat "file://" file) t)))
          org-file-apps))

  ;; Add gfm/md backends
  (use-package ox-gfm)
  (add-to-list 'org-export-backends 'md)

  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

  ;; Prettify UI
  (if emacs/>=27p
      (use-package org-modern
        :hook ((org-mode . org-modern-mode)
               (org-agenda-finalize . org-modern-agenda)
               (org-modern-mode . (lambda ()
                                    "Adapt `org-modern-mode'."
                                    ;; Disable Prettify Symbols mode
                                    (setq prettify-symbols-alist nil)
                                    (prettify-symbols-mode -1)))))
    (progn
      (use-package org-superstar
        :if (and (display-graphic-p) (char-displayable-p ?◉))
        :hook (org-mode . org-superstar-mode)
        :init (setq org-superstar-headline-bullets-list '("◉""○""◈""◇""⁕")))
      (use-package org-fancy-priorities
        :diminish
        :hook (org-mode . org-fancy-priorities-mode)
        :init (setq org-fancy-priorities-list
                    (if (and (display-graphic-p) (char-displayable-p ?🅐))
                        '("🅐" "🅑" "🅒" "🅓")
                      '("HIGH" "MEDIUM" "LOW" "OPTIONAL"))))))

  ;; (use-package valign
  ;;   :hook (org-mode . valign-mode))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defconst load-language-alist
    '((emacs-lisp . t)
      (python     . t)
      (js         . t)
      (css        . t)
      (C          . t)
      (java       . t)
      (plantuml   . t))
    "Alist of org ob languages.")

  ;; ob-sh renamed to ob-shell since 26.1.
  (cl-pushnew '(shell . t) load-language-alist)

  ;; (use-package ob-csharp
  ;;   :straight `(:type git :repo "thomas-villagers/ob-csharp" :host github)
  ;;   :init (cl-pushnew '(csharp . t) load-language-alist))

  (use-package ob-typescript
    :init (cl-pushnew '(typescript . t) load-language-alist))

  ;; Install: npm install -g @mermaid-js/mermaid-cli
  (use-package ob-mermaid
    :init (cl-pushnew '(mermaid . t) load-language-alist))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-alist)

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
                ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Export text/html MIME emails
  (use-package org-mime
    :bind (:map message-mode-map
                ("C-c M-o" . org-mime-htmlize)
                :map org-mode-map
                ("C-c M-o" . org-mime-org-buffer-htmlize)))

  ;; Add graphical view of agenda
  (use-package org-timeline
    :hook (org-agenda-finalize . org-timeline-insert-timeline))

  (when emacs/>=27p
    ;; Auto-toggle Org LaTeX fragments
    (use-package org-fragtog
      :diminish
      :hook (org-mode . org-fragtog-mode))

    ;; Preview
    (use-package org-preview-html
      :diminish
      :bind (:map org-mode-map
                  ("C-c C-h" . org-preview-html-mode))
      :init (when (featurep 'xwidget-internal)
              (setq org-preview-html-viewer 'xwidget))))

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
                ("s-<f7>" . org-tree-slide-mode)
                :map org-tree-slide-mode-map
                ("<left>" . org-tree-slide-move-previous-tree)
                ("<right>" . org-tree-slide-move-next-tree)
                ("S-SPC" . org-tree-slide-move-previous-tree)
                ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :init (setq org-tree-slide-header nil
                org-tree-slide-slide-in-effect t
                org-tree-slide-heading-emphasis nil
                org-tree-slide-cursor-init t
                org-tree-slide-modeline-display 'outside
                org-tree-slide-skip-done nil
                org-tree-slide-skip-comments t
                org-tree-slide-skip-outline-level 3))

  ;; Pomodoro
  (use-package org-pomodoro
    :custom-face
    (org-pomodoro-mode-line ((t (:inherit warning))))
    (org-pomodoro-mode-line-overtime ((t (:inherit error))))
    (org-pomodoro-mode-line-break ((t (:inherit success))))
    :bind (:map org-mode-map
                ("C-c C-x m" . org-pomodoro))
    :init
    (with-eval-after-load 'org-agenda
      (bind-keys :map org-agenda-mode-map
                 ("K" . org-pomodoro)
                 ("C-c C-x m" . org-pomodoro)))))

(use-package writeroom-mode)

;; Roam
(when (executable-find "cc")
  (use-package org-roam
    :diminish
    :hook (after-init . org-roam-db-autosync-enable)
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ;; ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           :map org-mode-map
           ("C-M-i" . completion-at-point)
           ("C-c n n" . org-id-get-create)
           ("C-c n t" . org-roam-tag-add)
           ("C-c n r" . org-roam-ref-add)
           ("C-c n j" . org-roam-dailies-capture-today))
    :custom
    (org-roam-complete-everywhere t)
    :init
    (setq org-roam-directory (expand-file-name "roam" (file-truename centaur-org-directory)))
    :config
    (unless (file-exists-p org-roam-directory)
      (make-directory org-roam-directory))
    ;; If you're using a vertical completion framework, you might want a more informative completion interface
    (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

    (require 'org-roam-export)

    (org-roam-db-autosync-mode)

    (setq org-roam-capture-templates '(("d" "Default" plain "%?"
                                        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                           "#+title: ${title}\n")
                                        :unnarrowed t)
                                       ("f" "Fleeting note" plain "%?"
                                        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                           "#+title: ${title}\n#+filetags: :FleetingNote:\n")
                                        :unnarrowed t)
                                       ("l" "Literature note" plain (file "~/org/roam/templates/literature_note_template.org")
                                        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                           "#+title: ${title}\n#+filetags: :LiteratureNote:\n")
                                        :unnarrowed t)))

    (setq org-roam-dailies-capture-templates
          `(("d" "daily" plain "* %?"
             :target (file+head "%<%Y %m %d>.org" "#+title: %<%Y-%m-%d>\n")
             :unarrowed t)))

    ;; Configure org-roam buffer display.
    (add-to-list 'display-buffer-alist
                 '("\\*org-roam\\*"
                   (display-buffer-in-direction)
                   (direction . right)
                   (window-width . 0.33)
                   (window-height . fit-window-to-buffer)))

    (use-package org-ref
      :config
      (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
      (setq bibtex-completion-bibliography (concat org-roam-directory "/references/ref.bib")
	        bibtex-completion-library-path (concat org-roam-directory "/references/")
	        bibtex-completion-notes-path (concat org-roam-directory "/references/notes/")
	        bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

	        bibtex-completion-additional-search-fields '(keywords)
	        bibtex-completion-display-formats
	        '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	          (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	        bibtex-completion-pdf-open-function
	        (lambda (fpath)
	          (call-process "open" nil 0 nil fpath))))

    (use-package org-roam-bibtex
      :ensure t
      :after org-roam
      :config
      (require 'org-ref))

    (when emacs/>=27p
      (use-package org-roam-ui
        ;; :init
        ;; (when (featurep 'xwidget-internal)
        ;;   (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url))
        :config
        (setq org-roam-ui-sync-theme t
              org-roam-ui-follow t
              org-roam-ui-update-on-save t
              org-roam-ui-open-on-start t)))))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
