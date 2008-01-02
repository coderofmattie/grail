;;----------------------------------------------------------------------
;; paludis.el
;; Primary Author: Mike Mattie
;;----------------------------------------------------------------------

;; Summary
;;
;; Interface to the paludis package manager typically used with Gentoo
;; Linux.

;; provides a mechanism to search the repositories. From the resulting
;; repository list they can preview or install a package

;; There are three variations on install. The first is simply by name
;; where the package manager chooses a default repository and version.

;; The second is by repository, version is default or latest.

;; the third install variation consists of installing a particular
;; version of a package.

;; The user simply sees hyperlink like names and versions. The three
;; variations should be presented, but should not as variations on
;; the interface. Just more than one place they can execute install
;; related commands.

(require 'auto-overlays)
(require 'auto-overlay-word)

(defface deploy-package-name-face
  `((t (:inherit default :underline t)))
  "deploy face for package names")

(defface deploy-package-stable-face
  `((t (:inherit default :bold t)))
  "deploy face for package names")

(defface deploy-package-beta-face
  `((t (:inherit default :sytle italic)))
  "deploy face for package names")

(defface deploy-package-alpha-face
  `((t (:inherit default :strikethrough t)))
  "deploy face for package names")

(def-sparse-map paludis-keymap
  "paludis package manager keymap"
  ("s" 'paludis-show-at-point)
  ("i" 'paludis-install-at-point))

(defun paludis-show ()
  "show what packages will be installed by paludis assuming the cursor is over
   a package name, repository, or version"
  (interactive)
  (message "show it !")
  )

(defun paludis-install ()
  "install packages with paludis assuming the cursor is over a package name,
   repository name, or version"
  (interactive)
  (message "install it !")
  )

(defun paludis-show-at-point ()
  "show what packages will be installed by paludis"
  (interactive)
  (message "show it !")
  )

(defun paludis-install-at-point ()
  "install packages with paludis, installation is recursive with regards to
   dependencies so you may want to try paludis-show first."
  (interactive)
  (message "install it !")
  )

;; need a local-map
(defun paludis-mode ()
  "turn on paludis mode"
  (interactive)

  (auto-overlay-unload-regexp 'paludis)
  (auto-overlay-load-regexp
    `(word ("^\\\*[[:blank:]]+\\([^[:blank:]]+\\)[[:blank:]]*$" . 1)
       (face . deploy-package-name-face)
       (paludis-package-name . t)
       (local-map . ,paludis-keymap)
       (read-only . t) ;; still doesn't work
       )
    'paludis
    )

  (auto-overlay-start 'paludis)
  )

(provide 'paludis)