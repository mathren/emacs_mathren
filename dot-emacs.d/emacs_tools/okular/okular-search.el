(defvar okular-script "okular"
  "*Name of start script for okular.")

(defun okular-jump-to-line ()
  "Call okular-script to perform a `forward search' for current file and line number.
See contents of okular-script for details.
If AucTeX is used, the value of TeX-master-file is used as filename
for the master .dvi file; else, the return value of okular-master-file-name
is used (which see)."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line 1)
      (let* (;;; current line in file, as found in the documentation
	     ;;; of emacs. Slightly non-intuitive.
	     (current-line (format "%d" (+ 1 (count-lines (point-min) (point)))))
	     ;;; name of the `main' .tex file, which is also used as .dvi basename:
	     (master-file (expand-file-name (if (fboundp 'TeX-master-file)
					     (TeX-master-file t)
					   (okular-get-masterfile (okular-master-file-name)))))
	     ;;; .dvi file name:
	     (pdf-file (concat (file-name-sans-extension master-file) ".pdf"))
	     ;;; current source file name.
	     (filename (expand-file-name (buffer-file-name))))
	(start-process "okular"
		       "okular-output" "okular" ;;; src-args
		      ;;; args for -sourceposition:
		       "--unique" (concat "file:" pdf-file "#src:" current-line filename)
		       )))))


(defun okular-get-masterfile (file)
  "Small helper function for AucTeX compatibility.
Converts the special value t that TeX-master might be set to
into a real file name."
  (if (eq file t)
      (buffer-file-name)
    file))



(defun okular-master-file-name ()
  "Emulate AucTeX's TeX-master-file function.
Partly copied from tex.el's TeX-master-file and TeX-add-local-master."
  (if (boundp 'TeX-master)
      TeX-master
    (let ((master-file (read-file-name "Master file (default this file): ")))
      (if (y-or-n-p "Save info as local variable? ")
	  (progn
	    (goto-char (point-max))
	    (if (re-search-backward "^\\([^\n]+\\)Local Variables:" nil t)
		(let* ((prefix (if (match-beginning 1)
				   (buffer-substring (match-beginning 1) (match-end 1))
				 ""))
		      (start (point)))
		  (re-search-forward (regexp-quote (concat prefix "End:")) nil t)
		  (if (re-search-backward (regexp-quote (concat prefix "TeX-master")) start t)
		      ;;; if TeX-master line exists already, replace it
		      (progn
			(beginning-of-line 1)
			(kill-line 1))
		    (beginning-of-line 1))
		  (insert prefix "TeX-master: " (prin1-to-string master-file) "\n"))
	      (insert "\n%%% Local Variables: "
		      "\n%%% mode: " (substring (symbol-name major-mode) 0 -5)
		      "\n%%% TeX-master: " (prin1-to-string master-file)
		      "\n%%% End: \n"))
	    (save-buffer)
	    (message "(local variables written.)"))
	(message "(nothing written.)"))
      (set (make-local-variable 'TeX-master) master-file))))


(provide 'okular-search)
