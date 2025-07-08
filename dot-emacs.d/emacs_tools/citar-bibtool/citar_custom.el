;; custom configuration for citar
;; https://github.com/emacs-citar/citar


(defvar citar-master-bibliography "~/path/to/master.bib"
  "Path to the master BibTeX file.")

(defvar citar-local-bibliography-cache nil
  "Cache for local bibliography file path per project.")

(defun citar-find-bibliography-in-tex ()
  "Find the bibliography file specified in \\bibliography{} command."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\\\\bibliography{\\([^}]+\\)}" nil t)
      (let ((bib-name (match-string 1)))
        ;; Handle multiple bibliography files (comma-separated)
        (car (split-string bib-name ","))))))

(defun citar-get-local-bib-file ()
  "Get the path to the local BibTeX file for the current project.
First looks for \\bibliography{} command in current buffer, then checks cache."
  (let* ((project-root (or (when (fboundp 'project-root)
                            (project-root (project-current)))
                          (locate-dominating-file default-directory ".git")
                          default-directory))
         (cache-key (expand-file-name "." project-root))
         (cached-path (cdr (assoc cache-key citar-local-bibliography-cache))))

    ;; Return cached path if available
    (if cached-path
        cached-path

      ;; Otherwise, search for bibliography in tex file
      (let* ((bib-basename (citar-find-bibliography-in-tex))
             (bib-path-with-ext (when bib-basename
                                 (expand-file-name (concat bib-basename ".bib") project-root)))
             (bib-path-without-ext (when bib-basename
                                    (expand-file-name bib-basename project-root)))
             (final-path nil))

        (cond
         ;; No \bibliography{} command found
         ((not bib-basename)
          (error "No \\bibliography{} command found in current buffer"))

         ;; Try with .bib extension first
         ((and bib-path-with-ext (file-exists-p bib-path-with-ext))
          (setq final-path bib-path-with-ext))

         ;; Try without .bib extension
         ((and bib-path-without-ext (file-exists-p bib-path-without-ext))
          (setq final-path bib-path-without-ext))

         ;; File not found, but we'll use the path with .bib extension for creation
         (t
          (setq final-path bib-path-with-ext)
          (message "Bibliography file %s not found, will be created"
                   (file-name-nondirectory final-path))))

        ;; Cache the result
        (push (cons cache-key final-path) citar-local-bibliography-cache)
        final-path))))

(defun citar-copy-entry-to-local-bib (key)
  "Copy a BibTeX entry with KEY from master to local bibliography using bibtool."
  (condition-case err
      (let ((local-bib (citar-get-local-bib-file))
            (master-bib citar-master-bibliography))
        (when (and (file-exists-p master-bib) key)
          ;; Create local bib file if it doesn't exist
          (unless (file-exists-p local-bib)
            (write-region "" nil local-bib)
            (message "Created bibliography file: %s" (file-name-nondirectory local-bib)))

          ;; Use bibtool to extract and append the entry
          (let ((temp-file (make-temp-file "bibtool-extract" nil ".bib")))
            (unwind-protect
                (progn
                  ;; Extract the specific entry to temp file
                  (call-process "bibtool" nil nil nil
                              "-s" "-q"
                              "-X" key
                              "-i" master-bib
                              "-o" temp-file)

                  ;; Check if entry was found and temp file has content
                  (when (and (file-exists-p temp-file)
                            (> (file-attribute-size (file-attributes temp-file)) 0))
                    ;; Check if entry already exists in local bib
                    (unless (citar-entry-exists-in-local-bib-p key local-bib)
                      ;; Append to local bib file
                      (with-temp-buffer
                        (insert-file-contents temp-file)
                        (goto-char (point-max))
                        (insert "\n")
                        (append-to-file (point-min) (point-max) local-bib))
                      (message "Added entry '%s' to %s" key (file-name-nondirectory local-bib)))))
              ;; Clean up temp file
              (when (file-exists-p temp-file)
                (delete-file temp-file))))))
    (error (message "Error copying entry '%s': %s" key (error-message-string err)))))

(defun citar-entry-exists-in-local-bib-p (key local-bib)
  "Check if KEY already exists in LOCAL-BIB file."
  (when (file-exists-p local-bib)
    (with-temp-buffer
      (insert-file-contents local-bib)
      (goto-char (point-min))
      (re-search-forward (concat "^@[^{]+{\\s-*" (regexp-quote key) "\\s-*,") nil t))))

(defun citar-insert-citation-with-local-copy (&optional arg)
  "Insert citation and copy entry to local bibliography.
With prefix ARG, insert citation without copying to local bib."
  (interactive "P")
  (condition-case err
      (let* ((keys (citar-select-refs))
             (citation (citar--format-citation keys)))
        ;; Insert the citation
        (insert citation)

        ;; Copy entries to local bib unless prefix arg is given
        (unless arg
          (dolist (key keys)
            (citar-copy-entry-to-local-bib key))))
    (error (message "Error in citar workflow: %s" (error-message-string err)))))

(defun citar-sync-all-citations-to-local-bib ()
  "Scan current buffer for all citations and copy them to local bibliography."
  (interactive)
  (condition-case err
      (save-excursion
        (goto-char (point-min))
        (let ((keys '()))
          ;; Find all citation keys in the buffer
          (while (re-search-forward "\\\\cite[^{]*{\\([^}]+\\)}" nil t)
            (let ((cite-keys (split-string (match-string 1) ",")))
              (dolist (key cite-keys)
                (push (string-trim key) keys))))

          ;; Remove duplicates and copy to local bib
          (setq keys (delete-dups keys))
          (dolist (key keys)
            (citar-copy-entry-to-local-bib key))

          (message "Synchronized %d citations to local bibliography" (length keys))))
    (error (message "Error syncing citations: %s" (error-message-string err)))))

(defun citar-setup-local-workflow ()
  "Set up citar with local bibliography workflow."
  (interactive)
  ;; Add local bib to citar-bibliography if it exists
  (let ((local-bib (citar-get-local-bib-file)))
    (when (file-exists-p local-bib)
      (add-to-list 'citar-bibliography local-bib)))
