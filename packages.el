;; -*- no-byte-compile: t; -*-

;; see package.org instead of this file

(package! org-bullets)

(package! org-download
  :recipe (:host github
           :repo "abo-abo/org-download"
           :files ("org-download.el")))

(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

(package! geiser)

(package! macrostep-geiser)

(package! sicp)

(package! random-splash-image)

(package! elcord)

(package! telega)

(package! nov)

(package! rainbow-blocks)

(package! achievements)

(package! clhs)

(package! csv-mode)
