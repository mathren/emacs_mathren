(require 'citar)

(defgroup citar-bibtool nil
  "Automatic BibTeX entry copying with citar and bibtool."
  :group 'citar
  :prefix "citar-bibtool-")

(defcustom citar-bibtool-master-bibliography "~/master.bib"
  "Path to the master BibTeX file."
  :type 'file
  :group 'citar-bibtool)

(defvar citar-bibtool-local-bibliography-cache nil
  "Cache for local bibliography file path per project.")

(defun citar-bibtool-find-bibliography-in-tex ()
  "Find the bibliography file specified in \\bibliography{} command."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\\\\bibliography{\\([^}]+\\)}" nil t)
      (let ((bib-name (match-string 1)))
        ;; Handle multiple bibliography files (comma-separated)
        (car (split-string bib-name ","))))))


(defun citar-bibtool-get-local-bib-file ()
  "Get the path to the local BibTeX file from \\bibliography{} command.
First looks for \\bibliography{} command in current buffer, then checks cache."
  (let* ((current-dir default-directory)
         (cache-key (expand-file-name "." current-dir))
         (cached-path (cdr (assoc cache-key citar-bibtool-local-bibliography-cache))))
    ;; Return cached path if available
    (if cached-path
        cached-path
      ;; Otherwise, search for bibliography in tex file
      (let* ((bib-path (citar-bibtool-find-bibliography-in-tex))
             (final-path nil))
        (cond
         ;; No \bibliography{} command found
         ((not bib-path)
          (error "No \\bibliography{} command found in current buffer"))
         ;; Check if the file exists
         ((file-exists-p bib-path)
          (setq final-path bib-path))
         ;; File not found, ask if user wants to create it
         (t
          (setq final-path bib-path)
          (message "Bibliography file %s not found. Create it? (y/n)"
                   bib-path)))
        ;; Cache the result
        (push (cons cache-key final-path) citar-bibtool-local-bibliography-cache)
        final-path))))



(defun citar-bibtool-entry-exists-in-local-bib-p (key local-bib)
  "Check if KEY already exists in LOCAL-BIB file."
  (when (file-exists-p local-bib)
    (with-temp-buffer
      (insert-file-contents local-bib)
      (goto-char (point-min))
      (re-search-forward (concat "^@[^{]+{\\s-*" (regexp-quote key) "\\s-*,") nil t))))

(defun citar-bibtool-copy-entry-to-local-bib (key)
  "Copy a BibTeX entry with KEY from master to local bibliography using bibtool."
  (condition-case err
      (let ((local-bib (citar-bibtool-get-local-bib-file))
            (master-bib citar-bibtool-master-bibliography))
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
                    (unless (citar-bibtool-entry-exists-in-local-bib-p key local-bib)
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

(defun citar-bibtool-format-citation (keys)
  "Format citation for KEYS using LaTeX cite command."
  (if (= (length keys) 1)
      (format "\\cite{%s}" (car keys))
    (format "\\cite{%s}" (string-join keys ","))))

;;;###autoload
(defun citar-bibtool-insert-citation-with-local-copy (&optional arg)
  "Insert citation and copy entry to local bibliography.
With prefix ARG, insert citation without copying to local bib."
  (interactive "P")
  (condition-case err
      (let* ((keys (citar-select-refs))
             (citation (citar-bibtool-format-citation keys)))
        ;; Insert the citation
        (insert citation)

        ;; Copy entries to local bib unless prefix arg is given
        (unless arg
          (dolist (key keys)
            (citar-bibtool-copy-entry-to-local-bib key))))
    (error (message "Error in citar workflow: %s" (error-message-string err)))))

;;;###autoload
(defun citar-bibtool-sync-all-citations-to-local-bib ()
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
            (citar-bibtool-copy-entry-to-local-bib key))

          (message "Synchronized %d citations to local bibliography" (length keys))))
    (error (message "Error syncing citations: %s" (error-message-string err)))))

;;;###autoload
(defun citar-bibtool-setup-local-workflow ()
  "Set up citar with local bibliography workflow."
  (interactive)
  ;; Add local bib to citar-bibliography if it exists
  (condition-case nil
      (let ((local-bib (citar-bibtool-get-local-bib-file)))
        (when (file-exists-p local-bib)
          (add-to-list 'citar-bibliography local-bib)))
    (error nil)) ; Ignore errors if no \bibliography{} command found

  ;; Bind the enhanced insertion command
  (local-set-key (kbd "C-c b") #'citar-bibtool-insert-citation-with-local-copy)
  (local-set-key (kbd "C-c B") #'citar-bibtool-sync-all-citations-to-local-bib))


(defun citar-bibtool-insert-key-only ()
  "Search master bibliography, copy entry to local bib, and insert only the citation key."
  (interactive)
  (let ((keys (citar-select-refs)))
    (dolist (key keys)
      ;; Copy the entry to local bibliography (function only takes key as argument)
      (citar-bibtool-copy-entry-to-local-bib key)
      ;; Insert just the key at point
      (insert key)
      ;; Add comma and space if not the last key
      (unless (equal key (car (last keys)))
        (insert ", ")))))


;; NASA/ADS search
;; Helper functions for ADS API
(defun ads-search-papers (query &optional max-results)
  "Search NASA/ADS for papers matching QUERY. Returns list of papers."
  (let* ((max-results (or max-results 20))
         (url "https://api.adsabs.harvard.edu/v1/search/query")
         (params `(("q" . ,query)
                   ("fl" . "bibcode,title,author,year,pub")
                   ("rows" . ,(number-to-string max-results))))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " ads-api-token))))
         (url-request-data nil))
    (with-current-buffer
        (url-retrieve-synchronously
         (concat url "?" (mapconcat
                          (lambda (pair)
                            (concat (url-hexify-string (car pair))
                                    "="
                                    (url-hexify-string (cdr pair))))
                          params "&")))
      (goto-char (point-min))
      (re-search-forward "^$")
      (let* ((json-response (json-read))
             (docs (cdr (assoc 'docs (cdr (assoc 'response json-response))))))
        (mapcar (lambda (doc)
                  (list (cdr (assoc 'bibcode doc))
                        (cdr (assoc 'title doc))
                        (cdr (assoc 'author doc))
                        (cdr (assoc 'year doc))
                        (cdr (assoc 'pub doc))))
                docs)))))

(defun ads-get-bibtex (bibcode)
  "Retrieve BibTeX entry for given BIBCODE from NASA/ADS."
  (let* ((url "https://api.adsabs.harvard.edu/v1/export/bibtex")
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " ads-api-token))
            ("Content-Type" . "application/json")))
         (url-request-data
          (json-encode `(("bibcode" . (,bibcode))))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let* ((json-response (json-read))
             (export-data (cdr (assoc 'export json-response))))
        export-data))))

(defun ads-format-paper-for-display (paper)
  "Format PAPER data for minibuffer display."
  (let ((authors (nth 2 paper))
        (title (nth 1 paper))
        (year (nth 3 paper))
        (pub (nth 4 paper))
	(bibcode (nth 0 paper))
	)
    (format "%s | %s | %s (%s) | %s"
            bibcode
            (if (vectorp title) (aref title 0) (or title "No title"))
            (if (vectorp authors)
                (concat (aref authors 0)
                        (if (> (length authors) 1) " et al." ""))
              (or authors "Unknown"))
            (or year "Unknown")
            (or pub "Unknown journal"))))

(defun ads-select-paper (papers)
  "Let user select a paper from PAPERS list using completing-read."
  (let* ((formatted-papers (mapcar (lambda (paper)
                                     (cons (ads-format-paper-for-display paper)
                                           paper))
                                   papers))
         (selection (completing-read "Select paper: " formatted-papers nil t)))
    (cdr (assoc selection formatted-papers))))

(defun ads-extract-citation-key-from-bibtex (bibtex-string)
  "Extract citation key from BIBTEX-STRING."
  (when (string-match "@[^{]+{\\([^,\n]+\\)" bibtex-string)
    (match-string 1 bibtex-string)))

(defun ads-replace-citation-key-in-bibtex (bibtex-string old-key new-key)
  "Replace OLD-KEY with NEW-KEY in BIBTEX-STRING."
  (replace-regexp-in-string
   (regexp-quote (concat "{" old-key))
   (concat "{" new-key)
   bibtex-string))

(defun ads-search-and-insert-citation ()
  "Search NASA/ADS, select paper, and insert citation with bibtex entry."
  (interactive)
  (let* ((query (read-string "Search NASA/ADS: "))
         (papers (ads-search-papers query))
         (selected-paper (ads-select-paper papers))
         (bibcode (nth 0 selected-paper))
         (bibtex-entry (ads-get-bibtex bibcode))
         (original-key (ads-extract-citation-key-from-bibtex bibtex-entry))
         (new-key (read-string "Citation key: " original-key))
         (final-bibtex (if (string= original-key new-key)
                           bibtex-entry
                         (ads-replace-citation-key-in-bibtex
                          bibtex-entry original-key new-key)))
         (local-bib-file (concat (car LaTeX-auto-bibliography) ".bib")))

    ;; Insert citation in LaTeX buffer
    (citar-latex-insert-citation (list new-key) nil "cite")

    ;; Add to local bib file
    (with-temp-buffer
      (when (file-exists-p local-bib-file)
        (insert-file-contents local-bib-file))
      (goto-char (point-max))
      (unless (bobp) (insert "\n\n"))
      (insert final-bibtex)
      (write-file local-bib-file))

    (message "Added citation %s to %s" new-key local-bib-file)))

(defun citar-insert-tex-bib (citekeys)
  "Insert CITEKEYS both as citation key in tex and as bibtex entry"
  (interactive (list (citar-select-refs)))
  (let ((local-bib-file (concat (car LaTeX-auto-bibliography) ".bib"))
        (updated nil))
    ;; insert in latex buffer
    (citar-latex-insert-citation citekeys nil "cite")
    ;; update in local .bib
    (with-temp-buffer
      (insert-file-contents local-bib-file)
      (mapcar (lambda (citekey)
                (goto-char (point-min))
                (unless (re-search-forward (concat "@.*?{" citekey) (point-max) t)
                  (setq updated t)
                  (goto-char (point-max))
                  (citar--insert-bibtex citekey)))
              citekeys)
      (when updated (write-file local-bib-file)))))

(provide 'citar-bibtool)
