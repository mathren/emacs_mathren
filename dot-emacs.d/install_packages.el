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
		     auctex
		     company-box
		     counsel
		     dired-icon
		     doom-modeline
		     ein
		     elpy
		     eshell-prompt-extras
		     helpful
		     orderless
		     vertico
		     corfu
		     embark
		     consult
		     ;; ivy
		     ;; ivy-rich
		     ;; ivy-prescient
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
		     yaml-mode
		     company-prescient
		     company-fuzzy
		     company-auctex
		     company-bibtex
		     editorconfig
		     arxiv-mode
		     multiple-cursors
		     magit-lfs
		     fontawesome
		     ws-butler
		     htmlize
		     simple-httpd
		     sudo-edit
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
