;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Caleb Watson"
      user-mail-address "calebdeanwatson@gmail.com")

(require 'random-splash-image)

(setq random-splash-image-dir
      (concat
       (getenv "HOME") "/.doom.d/random-splash-image-dir/chosen-splash-images/src/"))

(with-eval-after-load 'random-splash-image
  (random-splash-image-set))

(setq image-use-external-converter t)

(setq display-line-numbers-type t)

(with-eval-after-load 'org
  (setq org-directory "~/nextcloud/documents/org/"))

(with-eval-after-load 'org
  (setq org-agenda-files
        '("~/nextcloud/documents/org/agenda.org")))

(with-eval-after-load 'org
  (setq +org-capture-journal-file
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/journal.org")))

(with-eval-after-load 'org
  (setq +org-capture-notes-file
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/notes.org")))

(with-eval-after-load 'org
  (setq +org-capture-projects-file
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/projects.org")))

(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

(when (and (not (executable-find "fd"))
           (executable-find "rg"))
  (setq projectile-generic-command
        (let ((rg-cmd ""))
          (dolist (dir projectile-globally-ignored-directories)
            (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
          (setq rg-ignorefile
                (concat "--ignore-file" " "
                        (expand-file-name "rg_ignore" user-emacs-directory)))
          (concat "rg -0 --files --color=never --hidden" rg-cmd " " rg-ignorefile))))

(use-package! org-pandoc-import :after org)

(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("org-plain-latex"
               "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(provide 'org-archive-subtree-hierarchical)
(require 'org-archive)
(defun org-archive-subtree-hierarchical--line-content-as-string ()
  "Returns the content of the current line as a string"
  (save-excursion
    (beginning-of-line)
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))
(defun org-archive-subtree-hierarchical--org-child-list ()
  "This function returns all children of a heading as a list. "
  (interactive)
  (save-excursion
    ;; this only works with org-version > 8.0, since in previous
    ;; org-mode versions the function (org-outline-level) returns
    ;; gargabe when the point is not on a heading.
    (if (= (org-outline-level) 0)
        (outline-next-visible-heading 1)
      (org-goto-first-child))
    (let ((child-list (list (org-archive-subtree-hierarchical--line-content-as-string))))
      (while (org-goto-sibling)
        (setq child-list (cons (org-archive-subtree-hierarchical--line-content-as-string) child-list)))
      child-list)))
(defun org-archive-subtree-hierarchical--org-struct-subtree ()
  "This function returns the tree structure in which a subtree
belongs as a list."
  (interactive)
  (let ((archive-tree nil))
    (save-excursion
      (while (org-up-heading-safe)
        (let ((heading
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))))
          (if (eq archive-tree nil)
              (setq archive-tree (list heading))
            (setq archive-tree (cons heading archive-tree))))))
    archive-tree))
(defun org-archive-subtree-hierarchical ()
  "This function archives a subtree hierarchical"
  (interactive)
  (let ((org-tree (org-archive-subtree-hierarchical--org-struct-subtree))
        (this-buffer (current-buffer))
        (file (abbreviate-file-name
               (or (buffer-file-name (buffer-base-buffer))
                   (error "No file associated to buffer")))))
    (save-excursion
      (setq location org-archive-location
            afile (car (org-archive--compute-location
		                   (or (org-entry-get nil "ARCHIVE" 'inherit) location)))
            ;; heading (org-extract-archive-heading location)
            infile-p (equal file (abbreviate-file-name (or afile ""))))
      (unless afile
        (error "Invalid `org-archive-location'"))
      (if (> (length afile) 0)
          (setq newfile-p (not (file-exists-p afile))
                visiting (find-buffer-visiting afile)
                buffer (or visiting (find-file-noselect afile)))
        (setq buffer (current-buffer)))
      (unless buffer
        (error "Cannot access file \"%s\"" afile))
      (org-cut-subtree)
      (set-buffer buffer)
      (org-mode)
      (goto-char (point-min))
      (while (not (equal org-tree nil))
        (let ((child-list (org-archive-subtree-hierarchical--org-child-list)))
          (if (member (car org-tree) child-list)
              (progn
                (search-forward (car org-tree) nil t)
                (setq org-tree (cdr org-tree)))
            (progn
              (goto-char (point-max))
              (newline)
              (org-insert-struct org-tree)
              (setq org-tree nil)))))
      (newline)
      (org-yank)
      (when (not (eq this-buffer buffer))
        (save-buffer))
      (message "Subtree archived %s"
               (concat "in file: " (abbreviate-file-name afile))))))
(defun org-insert-struct (struct)
  "TODO"
  (interactive)
  (when struct
    (insert (car struct))
    (newline)
    (org-insert-struct (cdr struct))))
(defun org-archive-subtree ()
  (org-archive-subtree-hierarchical))

(setq org-archive-default-command 'org-archive-subtree-hierarchical)

(setq ledger-schedule-file "~/nextcloud/documents/ledger/ledger-schedule.ledger")

(achievements-mode)

(setq org-fold-core-style 'overlays)

(load "/home/chu/.local/share/quicklisp/clhs-use-local.el" t)

(setq browse-url-generic-program "firefox")
(setq browse-url-browser-function "firefox")
(setq browse-url-firefox-program "firefox")
