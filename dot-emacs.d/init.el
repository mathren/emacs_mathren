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

(use-package all-the-icons
  :ensure t)

(use-package no-littering
  :ensure t)

;; eglot
(use-package eglot
  :ensure t
  :defer t
  :hook ((python-mode . eglot-ensure)
	 (c-mode . eglot-ensure)
	 (c++-mode . eglot-ensure)
	 (f90-mode . eglot-ensure)
	 (LaTeX-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "/usr/bin/clangd-10"))
  (add-to-list 'eglot-server-programs '(f90-mode . ("fortls" "--notify_init" "--nthreads=2")))

  ;; Configure Python LSP for EIN modes
  (add-to-list 'eglot-server-programs '(ein:notebook-python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs '(ein:notebook-mode . ("pylsp")))

  ;; Configure eglot to work well with flymake
  (setq eglot-send-changes-idle-time 0.5)
  (setq eglot-auto-display-help-buffer nil)
  )

(use-package flymake
  :config
  ;; Show indicators in left fringe instead of underlining
  (setq flymake-fringe-indicator-position 'left-fringe)

  ;; Completely disable all flymake face styling (no underline, background, or foreground changes)
  (set-face-attribute 'flymake-error nil
		      :underline nil
		      :background nil
		      :foreground nil
		      :weight 'normal
		      :inherit nil)
  (set-face-attribute 'flymake-warning nil
		      :underline nil
		      :background nil
		      :foreground nil
		      :weight 'normal
		      :inherit nil)
  (set-face-attribute 'flymake-note nil
		      :underline nil
		      :background nil
		      :foreground nil
		      :weight 'normal
		      :inherit nil)
  ;; Don't show diagnostics at end of line
  (setq flymake-show-diagnostics-at-end-of-line nil)
)

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

(use-package indent-bars
  ;; :ensure t
  :config
    (setq
  indent-bars-pattern "."
  indent-bars-width-frac 0.2
  indent-bars-pad-frac 0.5
  indent-bars-color-by-depth nil
  indent-bars-highlight-current-depth '(:face default :blend 0.5))
)

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

(define-key org-mode-map (kbd "C-c s")
	    (lambda () (interactive)
	      (insert "#+BEGIN_SRC \n\n#+END_SRC")
	      (forward-line -1)))
(define-key org-mode-map (kbd "C-c q")
	    (lambda () (interactive)
	      (insert "#+BEGIN_QUOTE \n\n #+END_QUOTE")
	      (forward-line -1)))

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

(defun mr/remove-bibtex-entries-without-colon ()
  "Remove all BibTeX entries that don't have a colon after the opening brace in the first line.
Entries are assumed to be separated by empty lines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((modified nil))
      (while (re-search-forward "^@\\w+{" nil t)
        (let ((entry-start (match-beginning 0))
              (first-line-end (line-end-position)))
          ;; Check if there's a colon after the opening brace on the first line
          (if (save-excursion
                (goto-char (match-end 0))
                (re-search-forward ":" first-line-end t))
              ;; colon found, move to next entry
              (progn
                ;; Find the end of this entry (next empty line or end of buffer)
                (while (and (forward-line 1)
                           (not (eobp))
                           (not (looking-at "^$"))))
                (when (looking-at "^$")
                  (forward-line 1)))
            ;; No semicolon found, delete this entry
            (let ((entry-end (save-excursion
                              ;; Find the end of this entry
                              (while (and (forward-line 1)
                                         (not (eobp))
                                         (not (looking-at "^$"))))
                              ;; Include the empty line separator if present
                              (when (looking-at "^$")
                                (forward-line 1))
                              (point))))
              (delete-region entry-start entry-end)
              (setq modified t)
              ;; Don't advance position since we deleted text
              (goto-char entry-start)))))
      (when modified
        (message "Removed BibTeX entries without semicolon after opening brace"))
      modified)))

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

(use-package yaml-mode
  :ensure t)
(use-package snakemake-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("/\.yaml[^/]*$" . yaml-mode))
(add-to-list 'auto-mode-alist '("/\.yml[^/]*$" . yaml-mode))
(add-to-list 'auto-mode-alist '("/Snakefile[^/]*$" . snakemake-mode))
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :init
;;   ;; (advice-add 'python-mode :before 'elpy-enable))
;;   (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
;;   (setq elpy-rpc-python-command "python3"))

(use-package ein
  :config
  (setq ein:output-area-inlined-images t)  ;; show inline plots
  (setq ein:worksheet-enable-undo t)
  ; Enable syntax highlighting for Python cells
  (setq ein:completion-backend 'ein:use-ac-backend)
  ;; Set default language mode for cells
  (add-hook 'ein:notebook-mode-hook
	    (lambda ()
	      (setq ein:notebook-lang "python")))
  ;; redefined C-x B conflicts with ein
  (defun pm--visible-buffer-name ()
    "Get visible buffer name - compatibility function for EIN"
    (buffer-name (window-buffer)))

  ;; Enable eglot in Python cells
  (add-hook 'ein:connect-mode-hook #'eglot-ensure)
  ;; ;; Alternative: Enable eglot when entering Python cells
  (add-hook 'ein:notebook-python-mode-hook #'eglot-ensure)
  )

; to see latex in ein markdown cells
 (use-package math-preview
   :ensure t)

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
  (editorconfig-mode 1)
					; exclude tramp
  (add-to-list 'editorconfig-exclude-modes 'tramp-mode))

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
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
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

(use-package citar
 :ensure t
 :bind (("C-c i r" . citar-insert-citation)
	("C-c i o" . citar-open-link))
 :custom
 (citar-bibliography '("~/Documents/Research/Biblio_papers/bibtex/master_bibtex.bib"))
 (citar-symbols
  `((file ,(all-the-icons-faicon "file-pdf-o" :face 'all-the-icons-red) . " ")
    (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue) . " ")
    (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange) . " ")))
 :config
 ;; Load the local bibtool extension
 (load "~/.emacs.d/emacs_tools/citar-bibtool/citar-bibtool.el")

 ;; Configure variables over-writing definition in citar-bibtool.el
 (setq citar-bibtool-master-bibliography "~/Documents/Research/Biblio_papers/bibtex/master_bibtex.bib")
 (setq citar-open-always-create-notes nil)

 ;; Set up hooks
 (add-hook 'latex-mode-hook #'citar-bibtool-setup-local-workflow)
 (add-hook 'LaTeX-mode-hook #'citar-bibtool-setup-local-workflow)

 ;; Ensure citar is loaded in TeX files
 (add-hook 'latex-mode-hook #'citar-mode)
 (add-hook 'LaTeX-mode-hook #'citar-mode)

 ;; Set up key bindings
;; Set up key bindings
 (with-eval-after-load 'latex
   (define-key latex-mode-map (kbd "C-c i r") #'citar-bibtool-insert-key-only) ;; adds to local bib too
   (define-key latex-mode-map (kbd "C-c i R") #'citar-bibtool-sync-all-citations-to-local-bib)
   (define-key latex-mode-map (kbd "C-c i C") #'citar-bibtool-insert-citation-with-local-copy))
 ;; For AUCTeX
 (with-eval-after-load 'tex
   (define-key LaTeX-mode-map (kbd "C-c i r") #'citar-bibtool-insert-key-only)
   (define-key LaTeX-mode-map (kbd "C-c i R") #'citar-bibtool-sync-all-citations-to-local-bib)
   (define-key LaTeX-mode-map (kbd "C-c i C") #'citar-bibtool-insert-citation-with-local-copy))
 )

(use-package tramp
  :custom
  (tramp-remote-path '(tramp-default-remote-path
		       tramp-own-remote-path
		       "/usr/bin/"
		       "/usr/local/bin"
		       "/bin"))
  (tramp-default-remote-shell "/usr/bin/sh")
  )
(setq tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")

(defun count-sloc-region (beg end)
  "Count source lines of code in region (or (narrowed part of)
the buffer when no region is active).  SLOC means that empty
lines and comment-only lines are not taken into consideration."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((count 0))
	(while (not (eobp))
	  (if (not (comment-only-p (line-beginning-position)
				   (line-end-position)))
	      (setq count (1+ count)))
	  (forward-line))
	(message "SLOC in %s: %s."
		 (if (use-region-p) "region" "buffer")
		 count)))))

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
