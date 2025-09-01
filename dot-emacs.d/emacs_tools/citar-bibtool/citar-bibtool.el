;; Citar Bibliography Management Functions
;; A collection of functions for managing citations and bibliography files

(require 'citar)
(require 'json)
(require 'url)

;; Configuration variables -- will be overwritten
(defvar citar-bibtool-master-bibliography "~/master.bib"
  "Path to the master BibTeX file.")

(defvar citar-bibtool-local-bibliography-cache nil
  "Cache for local bibliography file path per project.")

;; Core bibliography functions
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
                   ("fl" . "bibcode,title,author,year,pub,doi,bibstem")
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
                        (cdr (assoc 'pub doc))
                        (cdr (assoc 'doi doc))
                        (cdr (assoc 'bibstem doc))))
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
        (bibcode (nth 0 paper)))
    (concat
     (if (vectorp authors)
         (cond
          ((= (length authors) 1) (aref authors 0))
          ((= (length authors) 2) (concat (aref authors 0) " & " (aref authors 1)))
          ((> (length authors) 2) (concat (aref authors 0) " , " (aref authors 1) " et al.")))
       (or authors "??"))
     " (" (or year "??") ") ⋅"
     (propertize (if (vectorp title) (aref title 0) (or title "No title"))
                 'face 'italic)
     " ⋅ "
     (propertize (or pub "??")
                 'face 'bold)
     " ⋅ " bibcode)))


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


(defun ads-normalize-bibtex-for-comparison (bibtex-string)
  "Normalize BIBTEX-STRING for comparison by removing key and formatting differences."
  (let ((normalized bibtex-string))
    ;; Remove the citation key (replace with placeholder)
    (setq normalized (replace-regexp-in-string
                      "@[^{]+{[^,\n]+"
                      "@article{PLACEHOLDER"
                      normalized))
    ;; Normalize whitespace
    (setq normalized (replace-regexp-in-string "\\s-+" " " normalized))
    ;; Remove leading/trailing whitespace from lines
    (setq normalized (replace-regexp-in-string "^\\s-+\\|\\s-+$" "" normalized))
    ;; Convert to lowercase for case-insensitive comparison
    (downcase normalized)))


(defun ads-find-duplicate-entry-in-local-bib (bibtex-entry local-bib-file)
  "Find if BIBTEX-ENTRY already exists in LOCAL-BIB-FILE.
Returns the existing citation key if found, nil otherwise."
  (when (file-exists-p local-bib-file)
    (let ((normalized-new (ads-normalize-bibtex-for-comparison bibtex-entry))
          (existing-keys '()))
      (with-temp-buffer
        (insert-file-contents local-bib-file)
        (goto-char (point-min))
        ;; Find all bibtex entries in the file
        (while (re-search-forward "@[^{]+{\\([^,\n]+\\)" nil t)
          (let* ((key (match-string 1))
                 (entry-start (match-beginning 0))
                 (entry-end (save-excursion
                              (goto-char entry-start)
                              (forward-sexp)
                              (point)))
                 (entry-content (buffer-substring entry-start entry-end))
                 (normalized-existing (ads-normalize-bibtex-for-comparison entry-content)))
            ;; Compare normalized versions
            (when (string= normalized-new normalized-existing)
              (push key existing-keys)))))
      (car existing-keys)))) ; Return first match


(defun ads-search-and-insert-citation ()
  "Search NASA/ADS, select paper, and insert citation with bibtex entry.
Checks for duplicates and suggests existing keys when found."
  (interactive)
  (let* ((query (read-string "Search NASA/ADS: "))
         (papers (ads-search-papers query))
         (selected-paper (ads-select-paper papers))
         (bibcode (nth 0 selected-paper))
         (bibtex-entry (ads-get-bibtex bibcode))
         (original-key (ads-extract-citation-key-from-bibtex bibtex-entry))
         (local-bib-file (concat (car LaTeX-auto-bibliography) ".bib"))
         (existing-key (ads-find-duplicate-entry-in-local-bib bibtex-entry local-bib-file))
         (final-key nil)
         (final-bibtex nil))

    (cond
     ;; Entry already exists - offer options
     (existing-key
      (let ((choice (completing-read
                     (format "Entry already exists with key '%s'. Choose action: " existing-key)
                     '("use-existing" "rename-existing" "create-new")
                     nil t)))
        (cond
         ((string= choice "use-existing")
          (setq final-key existing-key)
          (setq final-bibtex nil)) ; Don't add to file
         ((string= choice "rename-existing")
          (let ((new-key (read-string "New key for existing entry: " existing-key)))
            ;; Replace key in local bib file
            (with-temp-buffer
              (insert-file-contents local-bib-file)
              (goto-char (point-min))
              (when (re-search-forward (concat "@[^{]+{" (regexp-quote existing-key)) nil t)
                (replace-match (concat "@article{" new-key) nil nil))
              (write-file local-bib-file))
            (setq final-key new-key)
            (setq final-bibtex nil)))
         ((string= choice "create-new")
          (let ((new-key (read-string "Citation key for new entry: " original-key)))
            (setq final-key new-key)
            (setq final-bibtex (if (string= original-key new-key)
                                   bibtex-entry
				 (ads-replace-citation-key-in-bibtex
                                  bibtex-entry original-key new-key))))))))

     ;; New entry
     (t
      (let ((new-key (read-string "Citation key: " original-key)))
        (setq final-key new-key)
        (setq final-bibtex (if (string= original-key new-key)
                               bibtex-entry
                             (ads-replace-citation-key-in-bibtex
                              bibtex-entry original-key new-key))))))

    ;; Insert citation in LaTeX buffer
    (citar-latex-insert-citation (list final-key) nil "cite")

    ;; Add to local bib file if needed
    (when final-bibtex
      (with-temp-buffer
        (when (file-exists-p local-bib-file)
          (insert-file-contents local-bib-file))
        (goto-char (point-max))
        (unless (bobp) (insert "\n\n"))
        (insert final-bibtex)
        (write-file local-bib-file))
      (message "Added citation %s to %s" final-key local-bib-file))

    (when (and existing-key (not final-bibtex))
      (message "Using existing citation %s" final-key))))


(defun citar-bibtool-insert-tex-bib (citekeys)
  "Insert CITEKEYS both as citation key in tex and as bibtex entry."
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


(defun citar-bibtool-insert-citation-from-local-bib ()
  "Insert citation using only entries from the local bibliography file.
Similar to `citar-insert-citation` but reads from local bib instead of global."
  (interactive)
  (condition-case err
      (let* ((local-bib-base (citar-bibtool-get-local-bib-file))
             (local-bib (if (string-suffix-p ".bib" local-bib-base)
                           local-bib-base
                         (concat local-bib-base ".bib")))
             (original-citar-bibliography citar-bibliography)
             (citekeys nil))
        ;; Check if local bib file exists - use full path
        (let ((full-local-bib-path (expand-file-name local-bib)))
          (unless (file-exists-p full-local-bib-path)
            (error "Local bibliography file not found: %s" full-local-bib-path))

          ;; Temporarily override citar-bibliography to use only local file
          (setq citar-bibliography (list full-local-bib-path))

          ;; Force citar to refresh without touching internal cache variables
          (when (fboundp 'citar--bibliography-cache-reset)
            (citar--bibliography-cache-reset))

          ;; Use citar's selection interface with local bibliography
          (unwind-protect
              (progn
                (setq citekeys (citar-select-refs))
                ;; Insert the citation directly
                (insert (citar-bibtool-format-citation citekeys)))
            ;; Always restore original bibliography list
            (setq citar-bibliography original-citar-bibliography)
            (when (fboundp 'citar--bibliography-cache-reset)
              (citar--bibliography-cache-reset)))))
    (error (message "Error inserting citation from local bib: %s" (error-message-string err)))))

(provide 'citar-bibtool)
