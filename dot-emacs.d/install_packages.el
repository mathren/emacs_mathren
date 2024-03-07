;; Author: Mathieu Renzo
;; Keywords: files
;;
;; Comment: install some packages. Should be manually run first time
;;
;; Copyright (C) 2019-2024 Mathieu Renzo
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; define list of my packages
(setq package-list '(all-the-icons
		     all-the-icons-dired
		     all-the-icons-ibuffer
		     all-the-icons-ivy
		     all-the-icons-ivy-rich
		     snakemake-mode
		     auctex
		     company-box
		     counsel
		     dired-icon
		     dired-single
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
		     major-mode-icons
		     math-preview
		     no-littering
		     org-bullets
		     org-download
		     python-black
		     rainbow-delimiters
		     swiper
		     use-package
		     ;; which-key
		     ;; yaml-mode
		     ;; eglot
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
		     all-the-icons-nerd-fonts
		     ws-butler
		     htmlize
		     ))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
