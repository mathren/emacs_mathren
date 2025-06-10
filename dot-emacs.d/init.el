(load "~/.emacs.d/minimal.el")

(server-start)

(load "~/.emacs.d/install_packages.el")

(require 'use-package)

(use-package fzf
  :ensure t
  :bind (("C-c f f" . fzf)               ;; FZF in current directory
	 ("C-c f r" . fzf-recentf)       ;; FZF recent files
	 ("C-c f g" . fzf-git-files))   ;; FZF Git tracked files
  :commands (fzf fzf-directory fzf-git-files fzf-recentf)
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
	fzf/executable "fzf"
	fzf/git-grep-args "-i --line-number %s"
	;; command used for `fzf-grep-*` functions
	;; example usage for ripgrep:
	;; fzf/grep-command "rg --no-heading -nH"
	fzf/grep-command "grep -nrH"
	;; If nil, the fzf buffer will appear at the top of the window
	fzf/position-bottom t
	fzf/window-height 15))

(use-package all-the-icons)

(use-package no-littering)

;; eglot
(use-package eglot :ensure t)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "/usr/bin/clangd-10"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '(f90-mode . ("fortls" "--notify_init" "--nthreads=2")))
(add-hook 'f90-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

(setq company-minimum-prefix-length 1) ;; start at first characted
(setq company-idle-delay 0)            ;; no time delay
(setq company-selection-wrap-around t) ;; wrap around suggestion list
(company-tng-configure-default)        ;; tab cycles through suggestions

(use-package ivy
   :ensure t
   :diminish
   :init (ivy-mode 1)
   :bind (("C-s" . swiper)
	  ;; :map ivy-minibuffer-map
	  ;; ("TAB" . ivy-alt-done)
	  ;; ("C-l" . ivy-alt-done)
	  ;; ("C-j" . ivy-next-line)
	  ;; ("C-k" . ivy-previous-line)
	  ;; :map ivy-switch-buffer-map
	  ;; ("C-k" . ivy-previous-line)
	  ;; ("C-l" . ivy-done)
	  ;; ("C-d" . ivy-switch-buffer-kill)
	  ;; :map ivy-reverse-i-search-map
	  ;; ("C-k" . ivy-previous-line)
	  ;; ("C-d" . ivy-reverse-i-search-kill)
	  )

   )

   (use-package ivy-rich
     :init
     (ivy-rich-mode 1)
     ;; :config
     ;; (setq ivy-format-function #'ivy-format-function-line)
     ;; (setq ivy-rich--display-transformers-list
     ;; 	(plist-put ivy-rich--display-transformers-list
     ;; 		   'ivy-switch-buffer
     ;; 		   '(:columns
     ;; 		     ((ivy-rich-candidate (:width 40))
     ;; 		      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
     ;; 		      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
     ;; 		      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
     ;; 		      ; return file path relative to project root or `default-directory' if project is nil
     ;; 		      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
     ;; 		     :predicate
     ;; 		     (lambda (cand)
     ;; 		       (if-let ((buffer (get-buffer cand)))
     ;; 			   ;; Don't mess with EXWM buffers
     ;; 			   (with-current-buffer buffer
     ;; 			     (not (derived-mode-p 'exwm-mode))))))))
     )

(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode 1))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  ;; Enable auto-revert for dired buffers
  (setq global-auto-revert-non-file-buffers t)
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  ;; Add FZF integration in dired
  (define-key dired-mode-map (kbd "C-c C-f") 'fzf))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

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
  ;; capture templates
  (setq org-capture-templates
	'(("n" "Research note" entry
	   (file+headline "~/Documents/Research/Todos.org" "Research notes")
	   "* %?\n %T")
	  ("p" "Personal note" entry
	   (file+headline "~/Documents/Mathieu/Todos.org" "Personal notes")
	   "* %?\n %T")
	  ("i" "Future project idea" entry
	   (file "~/Documents/Research/Projects/ideas.org")
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
  (setq org-latex-with-hyperref nil)
  )

(use-package org-agenda
   :config
   (define-key org-agenda-mode-map (kbd "<S-left>") nil)
   (define-key org-agenda-mode-map (kbd "<S-right>") nil)
   (define-key org-agenda-mode-map (kbd "<S-down>") nil)
   (define-key org-agenda-mode-map (kbd "<S-up>") nil)
)

(defun reorder-org-headlines-dates ()
  "Extract dates from Org mode headlines, sort them chronologically
  from oldest to newest, and replace them in the headlines."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let* ((date-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \\([A-Za-z]\\{3\\}\\)>")
           (headlines '())
           (dates '())
           (point-min (point-min))
           (point-max (point-max)))

      ;; Extract dates and their positions
      (save-excursion
        (goto-char point-min)
        (while (re-search-forward (concat "^\\*+ " date-regexp) point-max t)
          (let* ((date (match-string 1))
                 (day-of-week (match-string 2))
                 (start (line-beginning-position))
                 (end (save-excursion (end-of-line) (point))))
            (push (list start end date day-of-week) headlines)
            (push date dates)))) ; Store dates as strings

      ;; Sort dates in ascending order
      (setq dates (sort dates 'string<))

      ;; Debugging: Print sorted dates
      ;; (message "Sorted dates: %s" dates)

      ;; Replace old dates with sorted dates
      (save-excursion
        (let ((date-list (reverse dates))) ; Reverse the list to apply oldest date first
          (dolist (headline headlines)
            (let* ((start (car headline))
                   (end (cadr headline))
                   (old-date (nth 2 headline))
                   (day-of-week (nth 3 headline))
                   (new-date (pop date-list))) ; Pop from reversed list
              (goto-char start)
              (re-search-forward date-regexp end t)
              (replace-match (concat "<" new-date " " day-of-week ">")))))))))

(setq org-latex-packages-alist '(("left=25mm, right=25mm, top=25mm, bottom=25mm" "geometry" nil)))
(customize-set-value 'org-latex-hyperref-template
		     "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n  pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L},\n colorlinks=true,\n citecolor=blue,\n linkcolor=blue,\n urlcolor=blue\n}\n")
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
(setq org-export-headline-levels 4)

(defun mr/filter-timestamp (trans back _comm)
  "Remove <> around time-stamps."
  (pcase back
    (`html
     (replace-regexp-in-string "&[lg]t;" "" trans))
    (`latex
     (replace-regexp-in-string "[<>]" "" trans))))

(defun mr/export-odot-html (backend)
  "Custom filter to replace LaTeX \odot with HTML sun symbol `&#9737;`."
  (when (org-export-derived-backend-p backend 'html)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\\\odot" nil t)
        (replace-match "☉" nil t)))))


(add-hook 'org-export-before-processing-hook 'mr/export-odot-html)

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
  (fmakunbound 'org-download-clipboard)
  )

(add-to-list 'auto-mode-alist '("/\.yaml[^/]*$" . yaml-mode))
(add-to-list 'auto-mode-alist '("/\.yml[^/]*$" . yaml-mode))
(add-to-list 'auto-mode-alist '("/Snakefile[^/]*$" . snakemake-mode))
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(use-package elpy
:ensure t
:defer t
:init
(advice-add 'python-mode :before 'elpy-enable))
(add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
(setq elpy-rpc-python-command "python3")

;; Install:
;; pip install black
;; pip install black-macchiato
(use-package python-black
  :demand t
  :after python
  :custom
  (python-black-extra-args '("--line-length=120" "--skip-string-normalization"))
  (setq python-black-command "~/.local/bin/black")
  (setq python-black-macchiato-command "~/.local/bin/black-macchiato")
  :bind
  (:map python-mode-map
    ("C-c C-l" . python-black-partial-dwim)))

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

; ein
(setq ein:worksheet-enable-undo t)
(setq ein:output-area-inlined-images t)

; to see latex in ein markdown cells
(use-package math-preview)

(use-package arxiv-mode
    :ensure t
    :config
    (setq arxiv-default-category "astro-ph")

    (defun mr/arxiv-show-abstract ()
        "Show the abstract window and display appropriate information."
	(unless (buffer-live-p arxiv-abstract-buffer)
	(setq arxiv-abstract-buffer (get-buffer-create "*arXiv-abstract*")))
	(with-current-buffer arxiv-abstract-buffer (arxiv-abstract-mode)
	(visual-line-mode)
	(setq-local prettify-symbols-alist arxiv-abstract-prettify-symbols-alist)
	(prettify-symbols-mode 1)
	(arxiv-format-abstract-page (nth arxiv-current-entry arxiv-entry-list)))
	(unless (window-live-p arxiv-abstract-window)
	(setq arxiv-abstract-window (display-buffer
        "*arXiv-abstract*"t))))

    (advice-add 'arxiv-show-abstract :override #'mr/arxiv-show-abstract)
)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package multiple-cursors
  :ensure t
  :config
  (defun mc/toggle-cursor-at-point ()
      "Add or remove a cursor at point."
      (interactive)
      (if multiple-cursors-mode
	  (message "Cannot toggle cursor at point while `multiple-cursors-mode' is active.")
	(let ((existing (mc/fake-cursor-at-point)))
	  (if existing
	      (mc/remove-fake-cursor existing)
	    (mc/create-fake-cursor-at-point)))))

    (add-to-list 'mc/cmds-to-run-once 'mc/toggle-cursor-at-point)
    (add-to-list 'mc/cmds-to-run-once 'multiple-cursors-mode)
  (define-key mc/keymap (kbd "<return>") nil)
	(global-set-key (kbd "<f1>") 'mc/toggle-cursor-at-point)
    (global-set-key (kbd "<M-s-return>") 'multiple-cursors-mode)
    (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

(use-package magit-lfs
     :ensure t
     :pin melpa)

(use-package ws-butler
       :ensure t
       :pin melpa)
(add-hook 'prog-mode-hook #'ws-butler-mode)

(set-fontset-font "fontset-default" '(#xf000 . #xf23a) "FontAwesome")

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package simple-httpd
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/emacs_tools/okular/")
(require 'okular-search)
(add-hook 'LaTeX-mode-hook (lambda () (local-set-key "\C-x\C-j" 'okular-jump-to-line)))
(add-hook 'tex-mode-hook (lambda () (local-set-key "\C-x\C-j" 'okular-jump-to-line)))

;; (load "~/.emacs.d/emacs_tools/okular/okular-latex.el") --------------------
;; ;; only start server for okular comms when in latex mode
;; (add-hook 'LaTeX-mode-hook 'server-start)
(setq TeX-PDF-mode t) ;; use pdflatex instead of latex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; ;; Enable synctex correlation
;(setq TeX-source-correlate-method 'synctex)
;; Enable synctex generation. Even though the command show as "latex" pdflatex is actually called
(custom-set-variables '(LaTeX-command "latex -synctex=1"))

;; (setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
(setq TeX-view-program-selection  '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list '(("PDF Viewer" "okular --unique %o#src:%n%b")))
;; end okular-latex.el -----------------------------------------------------

(dolist (hook '(text-mode-hook LaTeX-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(setq flyspell-sort-corrections nil)
(setq flyspell-issue-message-flag nil)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-to-list 'fill-nobreak-predicate 'texmathp)))

(use-package reftex
  :ensure auctex
  :after latex)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTex-mode-hook 'flyspell-mode)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-default-bibliography '("~/Documents/Research/Biblio_papers/bibtex/master_bibtex.bib"))
;; (setq reftex-default-bibliography '("~/Documents/Research/Biblio_papers/bibtex/zotero.bib"))
;(setq reftex-bibpath-environment-variables '("~/Documents/Research/Biblio_papers/bibtex/master_bibtex.bib")

(setq tramp-default-method "ssh")
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath=/tmp/master-%%r@%%h:%%p -o ControlPersist=yes")

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

;; Recent buffers in a new Emacs session
(use-package recentf
  :config
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-menu-items 50)
  (setq recentf-max-saved-items 250)
  (recentf-mode t)
  (global-set-key "\M-[" 'recentf-open-files)
  :diminish nil)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-capture)
(define-key global-map "\C-ctl" 'org-todo-list)

(defun go-to-column (column)
  (interactive "Column number: ")
  (move-to-column column t))
(global-set-key (kbd "M-g TAB") 'go-to-column)

(fset 'last-line-which-col
      "\C-[>\C-[OA\C-a\C-[g\C-i\C-u\C-xq[OB")

(put 'last-line-which-col 'kmacro t)

(global-set-key (kbd "C-c C-l") 'last-line-which-col)
