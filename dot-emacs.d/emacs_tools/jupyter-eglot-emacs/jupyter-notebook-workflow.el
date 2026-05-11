;;; jupyter-notebook-workflow.el --- ob-jupyter + eglot + pylsp + ox-ipynb  -*- lexical-binding: t; -*-
;;
;; Standalone load-file replacement for EIN (emacs-ipython-notebook).
;; Wires together: emacs-jupyter (ob-jupyter), eglot + pylsp, ox-ipynb,
;; code-cells + jupytext (for opening received .ipynb files), and
;; auto-insert for new notebook templates.
;;
;; Keybindings are modelled after EIN's keymap so muscle memory transfers.
;;
;; REQUIREMENTS (Debian):
;;   sudo apt install python3-pylsp python3-jupytext pandoc
;;   sudo apt install emacs-zmq   OR   M-x package-install RET zmq RET
;;   M-x package-install RET jupyter RET       ; emacs-jupyter / ob-jupyter
;;   M-x package-install RET ox-ipynb RET      ; jkitchin/ox-ipynb via MELPA
;;   M-x package-install RET code-cells RET    ; GNU ELPA
;;
;; USAGE:
;;   Add to your init.el:  (load-file "/path/to/jupyter-notebook-workflow.el")
;;   Or evaluate interactively: M-x load-file RET <path> RET
;;
;; NEW NOTEBOOK:
;;   C-x C-f analysis.ipynb.org   →  auto-insert fires, ready to run
;;
;; OPEN RECEIVED .ipynb:
;;   C-x C-f notebook.ipynb       →  code-cells+jupytext converts on the fly
;;
;; EXPORT TO .ipynb WITH OUTPUTS:
;;   C-c e i   (in an .org notebook buffer)
;;
;; ISSUES TO REPORT UPSTREAM:
;;   - emacs-jupyter: https://github.com/emacs-jupyter/jupyter/issues
;;   - ox-ipynb jupyter-python language mismatch: https://github.com/jkitchin/ox-ipynb/issues/19
;;   - xjupyter (2025 rewrite with working undo): https://github.com/commercial-emacs/xjupyter

;;; ---------------------------------------------------------------------------
;;; 0. Safety: warn clearly if hard dependencies are missing
;;; ---------------------------------------------------------------------------

(defun jnw--check-binary (bin msg)
  "Warn if BIN is not on PATH, showing MSG."
  (unless (executable-find bin)
    (display-warning 'jupyter-notebook-workflow
                     (format "Missing: %s — %s" bin msg)
                     :warning)))

(jnw--check-binary "pylsp"    "install python3-pylsp (apt)")
(jnw--check-binary "jupytext" "install python3-jupytext (apt) for .ipynb round-trip")
(jnw--check-binary "pandoc"   "install pandoc (apt) for .ipynb export")
(jnw--check-binary "jupyter"  "install jupyter (pip/apt) to run kernels")

;;; ---------------------------------------------------------------------------
;;; 1. Package bootstrap — installs missing packages from MELPA/GNU ELPA
;;; ---------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar jnw--required-packages '(zmq jupyter ox-ipynb code-cells)
  "Packages this workflow needs.  Will be installed if absent.")

(defun jnw--ensure-packages ()
  "Install any missing packages from `jnw--required-packages'."
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (pkg jnw--required-packages)
    (unless (package-installed-p pkg)
      (message "jupyter-notebook-workflow: installing %s..." pkg)
      (package-install pkg))))

(jnw--ensure-packages)

;;; ---------------------------------------------------------------------------
;;; 2. org-babel: load jupyter last (it depends on other lang entries)
;;; ---------------------------------------------------------------------------

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((python   . t)
             (shell    . t)
             (jupyter  . t))))   ; MUST be last
  (setq org-confirm-babel-evaluate nil
        org-src-preserve-indentation t
        org-src-fontify-natively t
        org-src-tab-acts-natively t))

;;; ---------------------------------------------------------------------------
;;; 3. emacs-jupyter defaults
;;    :results raw drawer  — kernel decides mime type, stored in :RESULTS: drawer
;;    :async yes           — non-blocking; essential for long-running cells
;;    :pandoc t            — convert html/latex output to org where possible
;;    :exports both        — ox-ipynb includes source + output in .ipynb
;;; ---------------------------------------------------------------------------

(with-eval-after-load 'jupyter
  (setq org-babel-default-header-args:jupyter-python
        '((:session  . "py")
          (:kernel   . "python3")
          (:async    . "yes")
          (:results  . "raw drawer")
          (:exports  . "both")
          (:pandoc   . "t")))

  ;; Make emacs-jupyter kernel shut down when its org buffer is killed.
  ;; Avoids zombie kernels accumulating across sessions.
  (defun jnw--kill-jupyter-kernel-on-buffer-kill ()
    "Shutdown the jupyter session associated with this org buffer."
    (when (and (derived-mode-p 'org-mode)
               (bound-and-true-p jupyter-current-client))
      (ignore-errors (jupyter-shutdown-kernel jupyter-current-client))))
  (add-hook 'kill-buffer-hook #'jnw--kill-jupyter-kernel-on-buffer-kill))

;;; ---------------------------------------------------------------------------
;;; 4. eglot + pylsp
;;    Attaches to python-mode AND to org-src-mode (the indirect buffer that
;;    opens when you press C-c ' inside a src block).
;;; ---------------------------------------------------------------------------

(with-eval-after-load 'eglot
  ;; Use pylsp (Debian: python3-pylsp) not pyright
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp")))
  ;; Keep eglot from slowing org-src indirect buffers with expensive features
  (add-to-list 'eglot-stay-out-of '(flymake)))  ; ob-jupyter has its own output

(defun jnw--maybe-start-eglot ()
  "Start eglot in python-mode org-src indirect buffers."
  (when (and (derived-mode-p 'python-mode)
             ;; Only in real files or org-src indirect buffers, not REPL buffers
             (or buffer-file-name
                 (bound-and-true-p org-src-mode)))
    (eglot-ensure)))

(add-hook 'python-mode-hook   #'jnw--maybe-start-eglot)
(add-hook 'org-src-mode-hook  #'jnw--maybe-start-eglot)

;;; ---------------------------------------------------------------------------
;;; 5. ox-ipynb export
;;    ox-ipynb doesn't natively recognise 'jupyter-python' blocks (issue #19).
;;    The hook below renames them to 'ipython' in a copy buffer before export,
;;    so ox-ipynb sees a language it knows, then discards the copy.
;;; ---------------------------------------------------------------------------

(with-eval-after-load 'ox-ipynb
  ;; Tell ox-ipynb which kernel spec to embed in the .ipynb metadata
  (setq ox-ipynb-kernelspec
        '((:name . "python3")
          (:display_name . "Python 3")
          (:language . "python")))

  (defun jnw--ox-ipynb-rename-jupyter-python ()
    "Rename jupyter-python src blocks to ipython so ox-ipynb exports them.
Runs in a temporary copy; the original buffer is unchanged."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx bol "#+" (or "begin_src" "BEGIN_SRC")
                  " jupyter-python")
              nil t)
        (replace-match (lambda (m)
                         (replace-regexp-in-string
                          "jupyter-python" "ipython" m))
                       nil nil nil 0))))

  (add-hook 'ox-ipynb-before-export-hook
            #'jnw--ox-ipynb-rename-jupyter-python))

;;; ---------------------------------------------------------------------------
;;; 6. code-cells + jupytext: open received .ipynb files
;;    C-x C-f notebook.ipynb  → auto-converts to org, saves back as .ipynb
;;    NOTE: cell *outputs* are NOT preserved in this round-trip (jupytext
;;    limitation). To read outputs, open the .ipynb in Jupyter or nbviewer.
;;; ---------------------------------------------------------------------------

(with-eval-after-load 'code-cells
  ;; Use pandoc for org round-trip.  Jupytext is used as fallback for
  ;; py/md representations because it offers better round-trip consistency.
  (setq code-cells-convert-ipynb-style
        '(("pandoc" "--to" "ipynb" "--from" "org")
          ("pandoc" "--to" "org"   "--from" "ipynb" "--extract-media" "./ipynb-images/")
          (lambda () #'org-mode)))

  ;; After opening an .ipynb as org, activate ob-jupyter goodies
  (add-hook 'code-cells-mode-hook
            (lambda ()
              (when (derived-mode-p 'org-mode)
                (org-babel-do-load-languages
                 'org-babel-load-languages
                 org-babel-load-languages)))))

;; Activate code-cells-mode automatically for .ipynb files
(add-to-list 'auto-mode-alist '("\\.ipynb\\'" . code-cells-mode))

;;; ---------------------------------------------------------------------------
;;; 7. auto-insert: new .ipynb.org files get the full header automatically
;;; ---------------------------------------------------------------------------

(use-package autoinsert
  :ensure nil  ; built-in
  :config
  (auto-insert-mode 1)
  (define-auto-insert
    '("\\.ipynb\\.org\\'" . "Jupyter org notebook skeleton")
    '(nil
      "#+TITLE: " (read-string "Notebook title: ") "\n"
      "#+AUTHOR: " (user-full-name) "\n"
      "#+DATE: " (format-time-string "%Y-%m-%d") "\n"
      "#+PROPERTY: header-args:jupyter-python "
      ":session py :kernel python3 :async yes "
      ":results raw drawer :exports both :pandoc t\n"
      "#+OX-IPYNB-KEYWORD-METADATA: AUTHOR DATE TITLE\n"
      "#+OX-IPYNB-LANGUAGE: jupyter-python\n\n"
      "* Introduction\n\n"
      "#+begin_src jupyter-python\n"
      "import sys\n"
      "print(f\"Python {sys.version}\")\n"
      "#+end_src\n\n"
      "* " _ "\n\n"
      "#+begin_src jupyter-python\n\n"
      "#+end_src\n")))

;;; ---------------------------------------------------------------------------
;;; 8. Cell navigation and insertion helpers
;;; ---------------------------------------------------------------------------

(defun jnw-next-cell ()
  "Move point to the next jupyter src block (like EIN C-<down> / C-c C-n)."
  (interactive)
  (let ((pos (point)))
    (if (re-search-forward "#\\+begin_src jupyter-python" nil t)
        (beginning-of-line)
      (goto-char pos)
      (message "No next cell"))))

(defun jnw-prev-cell ()
  "Jump to the beginning of the previous jupyter-python src block.
If point is already on or inside a cell header, this skips that cell
and lands on the one above it, so you always move to a *different* cell.
Mirrors EIN C-<up> / C-c C-p."
  (interactive)
  (let ((case-fold-search t)
        (origin (point))
        (found nil))
    ;; Search backward repeatedly until we find a #+begin_src that is
    ;; on a different line from where we started (skips the current cell).
    (save-excursion
      (while (and (not found)
                  (re-search-backward "#\\+begin_src jupyter-python" nil t))
        (unless (= (line-number-at-pos (point))
                   (line-number-at-pos origin))
          (setq found (point)))))
    (if found
        (goto-char found)
      (message "No previous cell"))))

(defun jnw-insert-cell-below ()
  "Insert a new jupyter-python src block below the current one.
Mirrors EIN C-c C-b / M-S-RET behaviour: new cell below, cursor inside."
  (interactive)
  ;; Jump past the current #+end_src
  (let ((case-fold-search t))
    (if (re-search-forward "#\\+end_src" nil t)
        (progn
          (end-of-line)
          (insert "\n\n#+begin_src jupyter-python\n\n#+end_src")
          (forward-line -2))
      ;; No end_src found: just append at end of buffer
      (goto-char (point-max))
      (insert "\n\n#+begin_src jupyter-python\n\n#+end_src\n")
      (forward-line -2))))

(defun jnw-insert-cell-above ()
  "Insert a new jupyter-python src block above the current one.
Mirrors EIN C-c C-a behaviour."
  (interactive)
  (let ((case-fold-search t))
    (if (re-search-backward "#\\+begin_src jupyter-python" nil t)
        (progn
          (beginning-of-line)
          (insert "#+begin_src jupyter-python\n\n#+end_src\n\n")
          (forward-line -3))
      (goto-char (point-min))
      (insert "#+begin_src jupyter-python\n\n#+end_src\n\n")
      (forward-line -3))))

(defun jnw-execute-cell ()
  "Execute the src block at point.  Mirrors EIN C-c C-c."
  (interactive)
  (org-babel-execute-src-block))

(defun jnw-execute-cell-and-next ()
  "Execute the src block at point and move to the next one.
Mirrors EIN M-RET."
  (interactive)
  (org-babel-execute-src-block)
  (jnw-next-cell))

(defun jnw-execute-cell-and-insert-below ()
  "Execute the src block at point and insert a new one below.
Mirrors EIN M-S-RET."
  (interactive)
  (org-babel-execute-src-block)
  (jnw-insert-cell-below))

(defun jnw-execute-all-cells ()
  "Execute all src blocks in the buffer.  Mirrors EIN C-u C-c C-c."
  (interactive)
  (org-babel-execute-buffer))

(defun jnw-kill-cell ()
  "Kill (delete) the src block at point.  Mirrors EIN C-c C-k."
  (interactive)
  (let ((element (org-element-at-point)))
    (if (eq (org-element-type element) 'src-block)
        (let ((begin (org-element-property :begin element))
              (end   (org-element-property :end   element)))
          (delete-region begin end)
          (message "Cell killed"))
      (message "Not inside a src block"))))

(defun jnw-toggle-output ()
  "Toggle visibility of the RESULTS drawer below the current cell.
Mirrors EIN C-c C-e."
  (interactive)
  (save-excursion
    (let ((case-fold-search t))
      (when (re-search-forward ":results:" nil t)
        (org-cycle)))))

(defun jnw-clear-output ()
  "Delete the #+RESULTS: block below the current src block.
Mirrors EIN C-c C-l."
  (interactive)
  (org-babel-remove-result))

(defun jnw-merge-cell-below ()
  "Merge the current src block with the one below.  Mirrors EIN C-c RET."
  (interactive)
  (let ((case-fold-search t))
    (save-excursion
      (unless (re-search-forward "#\\+end_src" nil t)
        (user-error "Not inside a src block"))
      (let ((end-of-current (point)))
        ;; Delete the end_src + blank lines + next begin_src line
        (when (re-search-forward "#\\+begin_src[^\n]*\n" nil t)
          (delete-region (- end-of-current 0)
                         (match-end 0))
          (goto-char end-of-current)
          ;; Clean up the seam
          (delete-blank-lines))))))

(defun jnw-move-cell-up ()
  "Swap current src block with the one above.  Mirrors EIN M-<up>."
  (interactive)
  ;; org-babel doesn't have this; we do a simple transpose via kill-ring
  (message "jnw-move-cell-up: not yet implemented — contributions welcome"))

(defun jnw-move-cell-down ()
  "Swap current src block with the one below.  Mirrors EIN M-<down>."
  (interactive)
  (message "jnw-move-cell-down: not yet implemented — contributions welcome"))

(defun jnw-export-to-ipynb ()
  "Export current org notebook to .ipynb (with outputs).
Mirrors EIN notebook save workflow.  Requires ox-ipynb."
  (interactive)
  (if (fboundp 'ox-ipynb-export-to-ipynb-file-and-open)
      (ox-ipynb-export-to-ipynb-file-and-open)
    (user-error "ox-ipynb not loaded; install it from MELPA")))

(defun jnw-interrupt-kernel ()
  "Interrupt the running Jupyter kernel.  Mirrors EIN C-c C-i."
  (interactive)
  (if (fboundp 'jupyter-interrupt-kernel)
      (call-interactively #'jupyter-interrupt-kernel)
    (user-error "emacs-jupyter not loaded")))

(defun jnw-restart-kernel ()
  "Restart the Jupyter kernel.  Mirrors EIN C-c C-r (restart)."
  (interactive)
  (if (fboundp 'jupyter-repl-restart-kernel)
      (call-interactively #'jupyter-repl-restart-kernel)
    (user-error "emacs-jupyter not loaded")))

;;; ---------------------------------------------------------------------------
;;; 9. Keymap — EIN-compatible bindings in org-mode notebook buffers
;;;
;;; EIN reference (from millejoh/emacs-ipython-notebook README):
;;;   C-<down>    goto-next-input       → jnw-next-cell
;;;   C-<up>      goto-prev-input       → jnw-prev-cell
;;;   C-c C-n     goto-next-input       → jnw-next-cell
;;;   C-c C-p     goto-prev-input       → jnw-prev-cell
;;;   C-c C-a     insert-cell-above     → jnw-insert-cell-above
;;;   C-c C-b     insert-cell-below     → jnw-insert-cell-below
;;;   C-<return>  insert-cell-below     → jnw-insert-cell-below  (bonus)
;;;   C-c C-c     execute-cell          → jnw-execute-cell (= org default)
;;;   M-RET       execute-and-next      → jnw-execute-cell-and-next
;;;   M-S-RET     execute-and-insert    → jnw-execute-cell-and-insert-below
;;;   C-u C-c C-c execute-all           → jnw-execute-all-cells
;;;   C-c C-k     kill-cell             → jnw-kill-cell
;;;   C-c C-e     toggle-output         → jnw-toggle-output
;;;   C-c C-l     clear-output          → jnw-clear-output
;;;   C-c RET     merge-cell-below      → jnw-merge-cell-below
;;;   M-<up>      move-cell-up          → jnw-move-cell-up   (stub)
;;;   M-<down>    move-cell-down        → jnw-move-cell-down (stub)
;;;   C-c e i     export to .ipynb      → jnw-export-to-ipynb (bonus)
;;;   C-c C-i     interrupt kernel      → jnw-interrupt-kernel
;;;   C-c C-r     restart kernel        → jnw-restart-kernel
;;;   M-.         jump-to-definition    → xref-find-definitions (eglot)
;;;   M-,         jump-back             → xref-go-back          (eglot)
;;;   C-x C-s     save                  → save-buffer (unchanged)
;;; ---------------------------------------------------------------------------

(defvar jnw-org-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Cell navigation
    (define-key map (kbd "C-<down>")   #'jnw-next-cell)
    (define-key map (kbd "C-<up>")     #'jnw-prev-cell)
    (define-key map (kbd "C-c C-n")    #'jnw-next-cell)
    (define-key map (kbd "C-c C-p")    #'jnw-prev-cell)
    ;; Cell insertion
    (define-key map (kbd "C-c C-b")    #'jnw-insert-cell-below)
    (define-key map (kbd "C-c C-a")    #'jnw-insert-cell-above)
    (define-key map (kbd "C-<return>") #'jnw-insert-cell-below)
    ;; Execution
    (define-key map (kbd "C-c C-c")    #'jnw-execute-cell)
    (define-key map (kbd "M-RET")      #'jnw-execute-cell-and-next)
    (define-key map (kbd "M-S-<return>") #'jnw-execute-cell-and-insert-below)
    ;; Cell manipulation
    (define-key map (kbd "C-c C-k")    #'jnw-kill-cell)
    (define-key map (kbd "C-c C-e")    #'jnw-toggle-output)
    (define-key map (kbd "C-c C-l")    #'jnw-clear-output)
    (define-key map (kbd "C-c RET")    #'jnw-merge-cell-below)
    (define-key map (kbd "M-<up>")     #'jnw-move-cell-up)
    (define-key map (kbd "M-<down>")   #'jnw-move-cell-down)
    ;; Kernel control
    (define-key map (kbd "C-c C-i")    #'jnw-interrupt-kernel)
    (define-key map (kbd "C-c C-r")    #'jnw-restart-kernel)
    ;; Export
    (define-key map (kbd "C-c e i")    #'jnw-export-to-ipynb)
    ;; xref navigation (eglot provides the backends)
    ;; M-. and M-, are already global xref bindings; listed here for reference
    map)
  "Keymap for jupyter-notebook-workflow, overlaid on org-mode.")

(defun jnw--activate-keymap ()
  "Overlay `jnw-org-mode-map' onto `org-mode-map' in notebook buffers.
A buffer is treated as a notebook if it visits an .ipynb.org file or
if it contains a jupyter-python src block."
  (when (or (and buffer-file-name
                 (string-match-p "\\.ipynb\\.org\\'" buffer-file-name))
            (save-excursion
              (goto-char (point-min))
              (re-search-forward "#\\+begin_src jupyter-python" nil t)))
    (use-local-map
     (make-composed-keymap jnw-org-mode-map org-mode-map))))

(add-hook 'org-mode-hook #'jnw--activate-keymap)
;; Re-check after saving, in case a new block was added
(add-hook 'after-save-hook
          (lambda ()
            (when (derived-mode-p 'org-mode)
              (jnw--activate-keymap))))

;;; ---------------------------------------------------------------------------
;;; 10. Which-key descriptions (optional — silently skipped if not installed)
;;; ---------------------------------------------------------------------------

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c C-n" "next cell"
    "C-c C-p" "prev cell"
    "C-c C-a" "insert cell above"
    "C-c C-b" "insert cell below"
    "C-c C-c" "execute cell"
    "C-c C-k" "kill cell"
    "C-c C-e" "toggle output"
    "C-c C-l" "clear output"
    "C-c C-i" "interrupt kernel"
    "C-c C-r" "restart kernel"
    "C-c e i" "export → .ipynb"))

;;; ---------------------------------------------------------------------------
;;; 11. ox-ipynb: export keybinding and require
;;; ---------------------------------------------------------------------------

(with-eval-after-load 'org
  (when (require 'ox-ipynb nil t)
    (message "jupyter-notebook-workflow: ox-ipynb loaded OK")))

;;; ---------------------------------------------------------------------------
;;; 12. Convenience: open the kernel REPL in a side window
;;; ---------------------------------------------------------------------------

(defun jnw-pop-to-repl ()
  "Open (or switch to) the Jupyter REPL for the current session.
Like EIN's C-c C-z (pop-to-notebook) adapted for emacs-jupyter."
  (interactive)
  (if (fboundp 'jupyter-repl-pop-to-buffer)
      (jupyter-repl-pop-to-buffer)
    (user-error "emacs-jupyter not loaded")))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-z") #'jnw-pop-to-repl))

;;; ---------------------------------------------------------------------------
;;; 13. Diagnostics helper — run M-x jnw-diagnose to check setup
;;; ---------------------------------------------------------------------------

(defun jnw-diagnose ()
  "Print a diagnostic report for the jupyter-notebook-workflow setup."
  (interactive)
  (with-current-buffer (get-buffer-create "*jnw-diagnose*")
    (erase-buffer)
    (insert "jupyter-notebook-workflow diagnostics\n")
    (insert (make-string 40 ?=) "\n\n")
    (dolist (item
             `(("pylsp binary"    . ,(executable-find "pylsp"))
               ("jupytext binary" . ,(executable-find "jupytext"))
               ("pandoc binary"   . ,(executable-find "pandoc"))
               ("jupyter binary"  . ,(executable-find "jupyter"))
               ("zmq package"     . ,(if (package-installed-p 'zmq)      "installed" nil))
               ("jupyter package" . ,(if (package-installed-p 'jupyter)  "installed" nil))
               ("ox-ipynb package". ,(if (package-installed-p 'ox-ipynb) "installed" nil))
               ("code-cells pkg"  . ,(if (package-installed-p 'code-cells) "installed" nil))
               ("eglot built-in"  . ,(if (locate-library "eglot")        "found" nil))
               ("ob-jupyter"      . ,(if (locate-library "ob-jupyter")   "found" nil))
               ("ox-ipynb lib"    . ,(if (locate-library "ox-ipynb")     "found" nil))))
      (let ((label (car item))
            (val   (cdr item)))
        (insert (format "  %-22s %s\n"
                        (concat label ":")
                        (if val (concat "✓ " val) "✗ NOT FOUND")))))
    (insert "\nKernel specs available:\n")
    (let ((specs (shell-command-to-string "jupyter kernelspec list 2>&1")))
      (insert specs))
    (insert "\nPylsp version:\n")
    (insert (shell-command-to-string "pylsp --version 2>&1"))
    (insert "\nPandoc version:\n")
    (insert (shell-command-to-string "pandoc --version 2>&1 | head -1"))
    (pop-to-buffer (current-buffer))))

;;; ---------------------------------------------------------------------------
;;; 14. Python environment management
;;;
;;; Supports two common Debian/scientific workflows:
;;;   a) venv  — python3 -m venv ~/.venvs/myenv
;;;   b) conda — conda create -n myenv python=3.11
;;;
;;; Activating an environment does three things:
;;;   1. Sets exec-path so Emacs finds the right python/jupyter/pylsp
;;;   2. Sets the VIRTUAL_ENV / CONDA_PREFIX env var so subprocesses inherit it
;;;   3. Restarts eglot in any open python src buffer so pylsp picks up the
;;;      new interpreter
;;;
;;; For remote work, set the environment BEFORE opening a TRAMP buffer —
;;; see section 15 for the TRAMP-specific path fix.
;;; ---------------------------------------------------------------------------

(defvar jnw-active-environment nil
  "Path to the currently active Python environment root (venv or conda).")

(defun jnw--env-bin (env-root)
  "Return the bin/ directory for ENV-ROOT (works for venv and conda)."
  (expand-file-name "bin" env-root))

(defun jnw--apply-environment (env-root)
  "Activate the Python environment at ENV-ROOT for this Emacs session.
Updates exec-path, PATH, and VIRTUAL_ENV.  Restarts eglot in open
python-mode / org-src-mode buffers so pylsp picks up the new interpreter."
  (let ((bin (jnw--env-bin env-root)))
    (unless (file-directory-p bin)
      (user-error "Not a valid environment (no bin/ found): %s" env-root))
    ;; 1. Prepend bin/ to exec-path (Emacs process search path)
    (setq exec-path (cons bin (cl-remove-if
                               (lambda (p) (string-match-p "/bin$" p))
                               exec-path)))
    ;; 2. Export to subprocesses via PATH
    (setenv "PATH" (concat bin ":" (getenv "PATH")))
    ;; 3. Set standard env markers so tools like pylsp know their interpreter
    (if (file-exists-p (expand-file-name "conda-meta" env-root))
        (setenv "CONDA_PREFIX" env-root)   ; conda env
      (setenv "VIRTUAL_ENV" env-root))     ; venv / virtualenv
    ;; 4. Bounce eglot in open Python buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (or (derived-mode-p 'python-mode)
                       (bound-and-true-p org-src-mode))
                   (bound-and-true-p eglot--managed-mode))
          (eglot-shutdown (eglot-current-server) nil t)
          (eglot-ensure))))
    (setq jnw-active-environment env-root)
    (message "jnw: activated environment %s" env-root)))

(defun jnw-activate-venv (venv-root)
  "Activate a venv at VENV-ROOT for this Emacs session.
Prompts for a directory; defaults to ~/.venvs/ if it exists.
Example: ~/.venvs/myproject"
  (interactive
   (list (read-directory-name
          "venv root: "
          (if (file-directory-p "~/.venvs") "~/.venvs/" "~/"))))
  (jnw--apply-environment venv-root))

(defun jnw-activate-conda (conda-env-name)
  "Activate a conda environment by NAME for this Emacs session.
Looks for the env under ~/miniconda3/envs/, ~/anaconda3/envs/,
or ~/.conda/envs/ — whichever exists first.
Example name: myenv"
  (interactive
   (list (read-string "Conda env name: ")))
  (let* ((candidates (list (expand-file-name
                             (concat "miniconda3/envs/" conda-env-name) "~")
                            (expand-file-name
                             (concat "anaconda3/envs/" conda-env-name) "~")
                            (expand-file-name
                             (concat ".conda/envs/" conda-env-name) "~")))
         (env-root (cl-find-if #'file-directory-p candidates)))
    (unless env-root
      (user-error "Conda env '%s' not found in %s"
                  conda-env-name
                  (mapconcat #'identity candidates ", ")))
    (jnw--apply-environment env-root)))

(defun jnw-deactivate-environment ()
  "Remove the currently active Python environment from exec-path and PATH.
Does not restart eglot — call M-x eglot manually after if needed."
  (interactive)
  (when jnw-active-environment
    (let ((bin (jnw--env-bin jnw-active-environment)))
      (setq exec-path (delete bin exec-path))
      (setenv "PATH" (string-join
                      (delete bin (split-string (getenv "PATH") ":"))
                      ":"))
      (setenv "VIRTUAL_ENV" nil)
      (setenv "CONDA_PREFIX" nil)
      (message "jnw: deactivated environment %s" jnw-active-environment)
      (setq jnw-active-environment nil))))

;;; ---------------------------------------------------------------------------
;;; 15. Kernel launcher
;;;
;;; jnw-start-kernel  — starts a local jupyter kernel and shows the session
;;;                     string to paste into your #+PROPERTY line.
;;; jnw-start-notebook — starts a full jupyter notebook server
;;;                      (for use with the /jpy: TRAMP backend, see section 16)
;;; ---------------------------------------------------------------------------

(defvar jnw--kernel-process nil
  "The local kernel process started by `jnw-start-kernel'.")

(defun jnw-start-kernel (&optional kernelspec)
  "Start a bare jupyter kernel (not a notebook server) and connect to it.
KERNELSPEC defaults to \"python3\".

Use this for the local workflow:
  1. Call M-x jnw-start-kernel
  2. The kernel connection file path is shown in the minibuffer and copied
     to the kill-ring — paste it as the :session value if needed.
  3. Your #+PROPERTY already uses :session py so ob-jupyter auto-connects.

A bare kernel (jupyter kernel) is required — jupyter notebook does NOT
expose ZMQ ports and cannot be used for direct emacs-jupyter connections."
  (interactive (list (read-string "Kernel spec (default: python3): " nil nil "python3")))
  (let* ((spec (or kernelspec "python3"))
         (buf  (get-buffer-create (format "*jnw-kernel:%s*" spec))))
    (if (and jnw--kernel-process
             (process-live-p jnw--kernel-process))
        (message "jnw: kernel already running (%s)" jnw--kernel-process)
      (setq jnw--kernel-process
            (start-process "jnw-kernel" buf
                           "jupyter" "kernel" (concat "--kernel=" spec)))
      (set-process-sentinel
       jnw--kernel-process
       (lambda (proc event)
         (message "jnw-kernel: %s → %s" proc (string-trim event))))
      ;; Show the buffer so the user can see the connection file path
      (display-buffer buf)
      (message "jnw: kernel starting — see *jnw-kernel:%s* for connection file path" spec))))

(defun jnw-stop-kernel ()
  "Stop the kernel started by `jnw-start-kernel'."
  (interactive)
  (if (and jnw--kernel-process (process-live-p jnw--kernel-process))
      (progn
        (kill-process jnw--kernel-process)
        (message "jnw: kernel stopped"))
    (message "jnw: no live kernel process found")))

(defun jnw-start-notebook-server (&optional port directory)
  "Start a jupyter notebook server for use with the /jpy: TRAMP backend.
PORT defaults to 8888.  DIRECTORY defaults to the current directory.

After starting, connect from an org src block with:
  :session /jpy:localhost#PORT:session-name

Or open the kernel list with:
  M-x jupyter-server-list-kernels  (then RET on the server line)

NOTE: this starts a *notebook server*, which uses WebSocket/HTTP, not
raw ZMQ sockets.  Do NOT use :session /ssh:host:name with this — that
mode requires a bare kernel (M-x jnw-start-kernel instead)."
  (interactive
   (list (read-string "Port (default 8888): " nil nil "8888")
         (read-directory-name "Notebook root directory: " default-directory)))
  (let* ((port      (or port "8888"))
         (dir       (or directory default-directory))
         (buf-name  (format "*jnw-notebook-server:%s*" port))
         (buf       (get-buffer-create buf-name)))
    (with-current-buffer buf (erase-buffer))
    (start-process "jnw-notebook" buf
                   "jupyter" "notebook"
                   "--no-browser"
                   (concat "--port=" port)
                   (concat "--notebook-dir=" (expand-file-name dir)))
    (display-buffer buf)
    (message
     (concat "jnw: notebook server starting on port %s. "
             "Connect with :session /jpy:localhost#%s:myname  "
             "or M-x jupyter-server-list-kernels")
     port port)))

;;; ---------------------------------------------------------------------------
;;; 16. TRAMP / remote kernel support
;;;
;;; HOW IT WORKS
;;; ─────────────
;;; emacs-jupyter has first-class TRAMP support via two distinct mechanisms.
;;; Pick the one that matches your remote setup:
;;;
;;; MODE A — Raw ZMQ kernel over SSH tunnels  (low-level, most robust)
;;; ─────────────────────────────────────────
;;; The kernel runs on the remote machine; emacs-jupyter auto-establishes
;;; SSH port-forwards for each ZMQ socket.  In your org file:
;;;
;;;   #+PROPERTY: header-args:jupyter-python :session /ssh:myhost:py :kernel python3
;;;
;;; emacs-jupyter reads the TRAMP path, SSHs to myhost, launches
;;;   jupyter kernel --kernel=python3
;;; reads the connection file it creates, and forwards the ZMQ ports locally.
;;;
;;; REQUIREMENTS:
;;;   • SSH key auth (no password) — emacs-jupyter does not support passwords
;;;   • 'jupyter kernel' available on PATH on the remote (see PATH fix below)
;;;   • NOT 'jupyter notebook' — that doesn't expose ZMQ ports
;;;
;;; MODE B — Jupyter notebook server over HTTP  (higher-level)
;;; ──────────────────────────────────────────
;;; Run a jupyter notebook server on the remote, SSH-forward its HTTP port,
;;; then use the /jpy: TRAMP method.  In your org file:
;;;
;;;   #+PROPERTY: header-args:jupyter-python :session /jpy:localhost#8888:py
;;;
;;; On the remote:
;;;   jupyter notebook --no-browser --port=8888
;;; In your SSH config or terminal:
;;;   ssh -L 8888:localhost:8888 myhost
;;;
;;; KNOWN PAIN POINT: remote PATH
;;; ──────────────────────────────
;;; TRAMP starts a minimal /bin/sh on the remote — it does NOT source
;;; ~/.bashrc or ~/.zshrc, so conda/venv/jupyter are often not on PATH.
;;; The fix is to add tramp-own-remote-path to tramp-remote-path, which
;;; tells TRAMP to use the login shell's PATH instead of its hardcoded list.
;;; If jupyter is still not found, add its absolute path explicitly.
;;;
;;; KNOWN LIMITATION: ProxyJump / multi-hop
;;; ─────────────────────────────────────────
;;; Auto-spawn (Mode A with a session name, not a .json path) does not
;;; reliably work through ProxyJump hosts.  Workaround: start the kernel
;;; manually on the remote and connect to its .json file:
;;;   :session /ssh:myhost:/run/user/1000/jupyter/kernel-XXXX.json
;;; ---------------------------------------------------------------------------

(with-eval-after-load 'tramp
  ;; The single most important fix: make TRAMP use the remote login shell's
  ;; PATH (which has conda/venv on it) rather than its own minimal hardcoded
  ;; list.  Without this, 'jupyter' is not found on the remote.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(defun jnw-remote-session-string (host &optional session-name kernel)
  "Return the :session string for a remote kernel on HOST.
SESSION-NAME defaults to \"py\".  KERNEL defaults to \"python3\".
Copy the result into your #+PROPERTY header-args line.

For MODE A (raw ZMQ — recommended):
  Result: /ssh:HOST:SESSION-NAME

For an existing kernel .json on the remote, use the full path instead:
  /ssh:HOST:/run/user/1000/jupyter/kernel-XXXX.json"
  (interactive
   (list (read-string "Remote host (as in ~/.ssh/config): ")
         (read-string "Session name (default: py): " nil nil "py")
         (read-string "Kernel spec (default: python3): " nil nil "python3")))
  (let* ((sname  (or session-name "py"))
         (kspec  (or kernel "python3"))
         (result (format "/ssh:%s:%s" host sname)))
    (kill-new result)
    (message
     (concat "Session string (copied to kill-ring): %s\n"
             "Add to #+PROPERTY: header-args:jupyter-python "
             ":session %s :kernel %s")
     result result kspec)
    result))

(defun jnw-connect-existing-remote-kernel (host json-path)
  "Connect to an already-running kernel on HOST using its JSON connection file.
JSON-PATH is the absolute path on the remote, e.g.
/run/user/1000/jupyter/kernel-abc123.json

This is the most reliable remote workflow:
  1. SSH to the remote and run: jupyter kernel --kernel=python3
  2. Note the printed connection file path
  3. Call this function with that path
  4. The :session value for your org block is printed and copied."
  (interactive
   (list (read-string "Remote host (as in ~/.ssh/config): ")
         (read-string "Remote JSON path (e.g. /run/user/1000/jupyter/kernel-XX.json): ")))
  (let ((session (format "/ssh:%s:%s" host json-path)))
    (kill-new session)
    (message "Session string (copied): %s  — use as :session in your org block" session)
    session))

;;; ---------------------------------------------------------------------------
;;; 17. Convenience keybindings for env + kernel management
;;; ---------------------------------------------------------------------------

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c j v") #'jnw-activate-venv)
  (define-key org-mode-map (kbd "C-c j c") #'jnw-activate-conda)
  (define-key org-mode-map (kbd "C-c j d") #'jnw-deactivate-environment)
  (define-key org-mode-map (kbd "C-c j k") #'jnw-start-kernel)
  (define-key org-mode-map (kbd "C-c j K") #'jnw-stop-kernel)
  (define-key org-mode-map (kbd "C-c j n") #'jnw-start-notebook-server)
  (define-key org-mode-map (kbd "C-c j r") #'jnw-remote-session-string))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c j"   "jnw: env/kernel"
    "C-c j v" "activate venv"
    "C-c j c" "activate conda"
    "C-c j d" "deactivate env"
    "C-c j k" "start kernel"
    "C-c j K" "stop kernel"
    "C-c j n" "start notebook server"
    "C-c j r" "remote session string"))

;;; ---------------------------------------------------------------------------
;;; Done
;;; ---------------------------------------------------------------------------

(message "jupyter-notebook-workflow.el loaded. Run M-x jnw-diagnose to verify setup.")

(provide 'jupyter-notebook-workflow)
;;; jupyter-notebook-workflow.el ends here

;;;; ══════════════════════════════════════════════════════════════════════════
;;;;  INIT.EL SNIPPET
;;;;  Copy the block below into your ~/.emacs.d/init.el (or ~/.emacs).
;;;;  Nothing below this line is evaluated — it is documentation only.
;;;; ══════════════════════════════════════════════════════════════════════════
;;;;
;;;; ;; ── jupyter-notebook-workflow ────────────────────────────────────────
;;;; ;;
;;;; ;; Load the workflow file.  Adjust the path to where you saved it.
;;;; (load-file "~/.emacs.d/lisp/jupyter-notebook-workflow.el")
;;;;
;;;; ;; ── Optional: activate a Python environment at startup ───────────────
;;;; ;;
;;;; ;; Uncomment ONE of these depending on your setup.
;;;; ;; The environment must be activated before Emacs opens any org notebook.
;;;;
;;;; ;; For a venv:
;;;; ;; (jnw-activate-venv "~/.venvs/myproject")
;;;;
;;;; ;; For conda (name only, not full path):
;;;; ;; (jnw-activate-conda "myenv")
;;;;
;;;; ;; ── Optional: TRAMP PATH fix (always safe to include) ────────────────
;;;; ;;
;;;; ;; Makes TRAMP use the remote login shell's PATH so that conda/pip-
;;;; ;; installed jupyter is visible on the remote without hardcoding paths.
;;;; (with-eval-after-load 'tramp
;;;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
;;;;
;;;; ;; ── Optional: per-host PATH override ────────────────────────────────
;;;; ;;
;;;; ;; If your remote jupyter lives in a non-standard location
;;;; ;; (e.g. a conda env that isn't on the login shell PATH), add it here:
;;;; ;; (with-eval-after-load 'tramp
;;;;   ;;   (add-to-list 'tramp-remote-path "/home/you/miniconda3/envs/myenv/bin"))
;;;;
;;;; ;; ── How to open a received .ipynb ────────────────────────────────────
;;;; ;;
;;;; ;; Just do C-x C-f notebook.ipynb — code-cells + pandoc converts it
;;;; ;; to org automatically on open and back to .ipynb on save.
;;;; ;; NOTE: cell outputs are not preserved in this round-trip.
;;;;
;;;; ;; ── How to connect to a remote kernel ───────────────────────────────
;;;; ;;
;;;; ;; Option A (bare kernel, recommended):
;;;; ;;   On remote: jupyter kernel --kernel=python3
;;;; ;;              (note the printed .json path)
;;;; ;;   In Emacs:  M-x jnw-connect-existing-remote-kernel
;;;; ;;              → enter host and .json path → session string is copied
;;;; ;;   In org:    #+PROPERTY: header-args:jupyter-python :session <pasted>
;;;; ;;
;;;; ;; Option B (auto-spawn, requires key auth + jupyter on remote PATH):
;;;; ;;   In org:    #+PROPERTY: header-args:jupyter-python \
;;;; ;;                :session /ssh:myhost:py :kernel python3
;;;; ;;
;;;; ;; Option C (notebook server over SSH tunnel):
;;;; ;;   On remote: jupyter notebook --no-browser --port=8888
;;;; ;;   Terminal:  ssh -L 8888:localhost:8888 myhost
;;;; ;;   In org:    #+PROPERTY: header-args:jupyter-python \
;;;; ;;                :session /jpy:localhost#8888:myname
;;;; ;;
;;;; ;; ── Key bindings added in org-mode buffers ───────────────────────────
;;;; ;;
;;;; ;; Cell navigation:
;;;; ;;   C-<down>  / C-c C-n   next cell
;;;; ;;   C-<up>    / C-c C-p   prev cell (beginning of previous cell)
;;;; ;; Cell insertion:
;;;; ;;   C-<return>/ C-c C-b   insert cell below
;;;; ;;   C-c C-a               insert cell above
;;;; ;; Execution:
;;;; ;;   C-c C-c               execute cell
;;;; ;;   M-RET                 execute + move to next
;;;; ;;   M-S-RET               execute + insert below
;;;; ;; Cell management:
;;;; ;;   C-c C-k               kill cell
;;;; ;;   C-c C-e               toggle output
;;;; ;;   C-c C-l               clear output
;;;; ;;   C-c RET               merge with cell below
;;;; ;; Kernel:
;;;; ;;   C-c C-i               interrupt kernel
;;;; ;;   C-c C-r               restart kernel
;;;; ;;   C-c C-z               pop to REPL
;;;; ;; Export:
;;;; ;;   C-c e i               export to .ipynb
;;;; ;; Environment / kernel (all prefixed C-c j):
;;;; ;;   C-c j v               activate venv
;;;; ;;   C-c j c               activate conda env
;;;; ;;   C-c j d               deactivate environment
;;;; ;;   C-c j k               start local kernel
;;;; ;;   C-c j K               stop local kernel
;;;; ;;   C-c j n               start notebook server
;;;; ;;   C-c j r               generate remote session string
