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
(add-to-list 'eglot-server-programs '(f90-mode . ("fortls" "--notify_init" "--nthreads=2")))
;;   (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)
;; (add-hook 'f90-mode-hook 'eglot-ensure)
;; (add-hook 'python-mode-hook 'eglot-ensure)
;; (add-hook 'c-mode-hook 'my/eglot-ensure-local)
;; (add-hook 'c++-mode-hook 'my/eglot-ensure-local)
;; (add-hook 'f90-mode-hook 'my/eglot-ensure-local)
;; (add-hook 'python-mode-hook 'my/eglot-ensure-local)

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

(use-package reftex
  :ensure auctex
  :after latex)
