;; author: Mathieu Renzo

;; Author: Mathieu Renzo <mathren90@gmail.com>
;; Keywords: files

;; Comment: install some packages. Should be manually loaded in emacs at
;; first runtime
;; Copyright (C) 2019-2021 Mathieu Renzo

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
(setq package-list '(which-key
		     use-package
		     swiper
		     rainbow-delimiters
		     org-download
		     org-bullets
		     major-mode-icons
		     magit
		     lsp-ui
		     lsp-treemacs
		     lsp-origami
		     lsp-mode
		     ivy-prescient
		     ivy
		     elpy
		     ein
		     doom-modeline
		     dired-icon
		     counsel
		     company-box
		     auctex
		     all-the-icons-ivy-rich
		     all-the-icons-ivy
		     all-the-icons-ibuffer
		     all-the-icons-dired
		     no-littering))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
