(load "~/.emacs.d/minimal.el")

(server-start)

(load "~/.emacs.d/install_packages.el")

(require 'use-package)

(use-package no-littering)

(add-hook 'f90-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

(use-package all-the-icons)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))
(setq global-auto-revert-non-file-buffers t)
(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-modeline
  :ensure t
  :custom ((doom-modeline-height 10))
  :init (doom-modeline-mode 1))
(setq doom-modeline-icon t)

;; these are configured in minimal.el
;; (electric-pair-mode 1)
;; (setq electric-pair-preserve-balance nil)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(dolist (hook '(text-mode-hook LaTeX-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(setq flyspell-sort-corrections nil)
(setq flyspell-issue-message-flag nil)

(use-package reftex
  :ensure auctex
  :after latex)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTex-mode-hook 'flyspell-mode)
(setq reftex-plug-into-AUCTeX t)
;; (setq reftex-default-bibliography '("~/Documents/Research/Biblio_papers/bibtex/master_bibtex.bib"))
(setq reftex-default-bibliography '("~/Documents/Research/Biblio_papers/bibtex/zotero.bib"))
;(setq reftex-bibpath-environment-variables '("~/Documents/Research/Biblio_papers/bibtex/master_bibtex.bib")

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-to-list 'fill-nobreak-predicate 'texmathp)))

(load "~/.emacs.d/emacs_tools/okular-latex.el")
(load "~/.emacs.d/emacs_tools/okular-search.el")

(use-package org
  :pin elpa
  :config
  (define-key org-mode-map (kbd "<S-left>") nil)
  (define-key org-mode-map (kbd "<S-right>") nil)
  (define-key org-mode-map (kbd "<S-down>") nil)
  (define-key org-mode-map (kbd "<S-up>") nil)
  (setq org-ellipsis " ▾ ")
  (setq org-startup-with-inline-images t)
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts t)
  (setq org-use-sub-superscripts "{}")
  (setq org-image-actual-width 400)
  (setq org-hide-emphasis-markers t)
  (setq org-startup-folded t)
  (setq org-capture-templates
	'(("n" "Research note" entry
	   (file+headline "~/Documents/Research/Todos.org" "Research notes")
	   "* %?\n %T")
	  ("p" "Personal note" entry
	   (file+headline "~/Documents/Mathieu/Todos.org" "Personal notes")
	   "* %?\n %T")
	  ("i" "Future project idea" entry
	   (file+headline "~/Documents/Research/Projects/ideas.org" "Future projects ideas")
	   "* %?\n %T")
	  ("j" "Job applications idea" entry
	   (file+headline "~/Documents/Research/Applications/Notes.org" "Application related notes")
	   "* %?\n %T")
	  ("f" "FLASH and PPISN" entry
	   (file+headline "~/Documents/Research/Projects/PP/FLASH/FLASH_notes.org" "FLASH and PPISN notes")
	   "* %?\n %T")
	  ("r" "Random throwaway" entry
	   (file+headline "/tmp/Random_notes.org" "Random throughaway notes")
	   "* %?\n %T")
	  ))
    )

(use-package org-agenda
   :config
   (define-key org-agenda-mode-map (kbd "<S-left>") nil)
   (define-key org-agenda-mode-map (kbd "<S-right>") nil)
   (define-key org-agenda-mode-map (kbd "<S-down>") nil)
   (define-key org-agenda-mode-map (kbd "<S-up>") nil)
)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "●" "○" "●" "○" "●" "○")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  ;; (visual-fill-column-mode 1)
  )

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package org-download
  :config
  (setq-default org-download-image-dir ".org_notes_figures/")
  )

;; (use-package org-roam
;;     :config
;;     (org-roam-db-autosync-mode)
;; )

(use-package elpy
  :ensure t
  :init
  (elpy-enable))
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))

;; Install:
;; pip install black
;; pip install black-macchiato
(use-package python-black
  :demand t
  :after python
  :custom
  (python-black-extra-args '("--line-length=120" "--skip-string-normalization"))
  :bind
  (:map python-mode-map
    ("C-c C-l" . python-black-partial-dwim)))

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

; ein
(setq ein:worksheet-enable-undo t)
(setq ein:output-area-inlined-images t)

(with-eval-after-load "ispell"
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")
  ;; Configure two variants of English, French and Italian
  (setq ispell-dictionary "en_US,en_GB,fr_FR,it_IT")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,en_GB,fr_FR,it_IT")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale
  (setq ispell-personal-dictionary "~/.emacs.d/emacs_tools/hunspell_personal"))

;; (unless (file-exists-p ispell-personal-dictionary)
;; (write-region " " nil ispell-personal-dictionary nil 0))

(setq sentence-end-double-space nil)

;; (use-package recentf
;;   ;; Loads after 1 second of idle time.
;;   :defer 1
;;   :custom
;;   (setq recentf-max-menu-items 40)
;;   (setq recentf-max-saved-items 40)
;; )
(setq recentf-max-menu-items 40)
(setq recentf-max-saved-items 40)
(global-set-key (kbd "M-]") 'recentf-open-files)
(recentf-mode 1)			;
; (global-set-key (kbd "M-]") 'recentf-open-files)

; (add-hook 'recentf-mode-hook 'swiper)

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-capture)
(define-key global-map "\C-ctl" 'org-todo-list)

(defun go-to-column (column)
  (interactive "nColumn: ")
  (move-to-column column t))
(global-set-key (kbd "M-g TAB") 'go-to-column)

(fset 'last-line-which-col
      "\C-[>\C-[OA\C-a\C-[g\C-i\C-u\C-xq[OB")

(put 'last-line-which-col 'kmacro t)

(global-set-key (kbd "C-c C-l") 'last-line-which-col)
