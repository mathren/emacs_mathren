(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(setq package-list '(fzf
		     nerd-icons
		     nerd-icons-completion
		     nerd-icons-ibuffer
		     nerd-icons-ivy-rich
		     nerd-icons-dired
		     ;; snakemake-mode
		     auctex
		     company-box
		     counsel
		     dired-icon
		     doom-modeline
		     ein
		     elpy
		     eshell-prompt-extras
		     helpful
		     ivy
		     ivy-rich
		     ivy-prescient
		     jedi
		     jedi-core
		     magit
		     math-preview
		     no-littering
		     org-bullets
		     org-download
		     python-black
		     rainbow-delimiters
		     swiper
		     use-package
		     ;; which-key
		     yaml-mode
		     company-prescient
		     company-fuzzy
		     company-auctex
		     company-bibtex
		     editorconfig
		     arxiv-mode
		     multiple-cursors
		     magit-lfs
		     ;; super-save
		     fontawesome
		     ;; all-the-icons-nerd-fonts
		     ws-butler
		     htmlize
		     simple-httpd
		     sudo-edit
		     ;; all-the-icons
		     ;; all-the-icons-completion
		     ;; all-the-icons-dired
		     ;; all-the-icons-gnus
		     ;; all-the-icons-ibuffer
		     ;; all-the-icons-ivy
		     ;; all-the-icons-ivy-rich
		     auto-complete-auctex
		     markdown-mode
		     indent-bars
		     citar
		     rg
		     vdiff
		     unfill
		     ))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
