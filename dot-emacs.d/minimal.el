(add-to-list 'auto-mode-alist '("/\.yaml[^/]*$" . yaml-mode))
(add-to-list 'auto-mode-alist '("/\.yml[^/]*$" . yaml-mode))
(add-to-list 'auto-mode-alist '("/Snakefile[^/]*$" . snakemake-mode))
(add-hook 'text-mode-hook 'turn-on-auto-fill)
