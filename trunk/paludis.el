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

(require 'parser)
;; (require 'auto-overlays)
;; (require 'auto-overlay-word)

(defface deploy-package-name-face
  `((t (:inherit default :underline t)))
  "deploy face for package names")

(defface deploy-repository-face
  `((t (:inherit default :foreground "SkyBlue3")))
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
  (overlays-at
  (message "show it !")
  )

(defun paludis-install ()
  "install packages with paludis assuming the cursor is over a package name,
   repository name, or version"
  (interactive)
  (message "install it !")
  )

;; this is where we need to translate the buffer position into a position
;; in our data structure.

(defun paludis-show-at-point ()
  "show what packages will be installed by paludis"
  (interactive)
  (cond
    ((paludis-type-p name) (message "show it !")))
  )

(defun paludis-install-at-point ()
  "install packages with paludis, installation is recursive with regards to
   dependencies so you may want to try paludis-show first."
  (interactive)
  (message "install it !")
  )

;; why can't I name an or ? this is a weakness in define that needs to be addressed.
(parser-compile paludis-query
  (define
    (token whitespace        "[[:blank:]]+" null)
    (token pkg-name          "[^[:blank:]]+" parser-token-string)
    (token repo-name         "\\([^[:blank:]]+\\):" 1)

    (name pkg-version        (or
                               (token pkg-ver-masked "\\\(\\([^[:blank:]]+\\)\\\)[^[:blank:]]+" 1)
                               (token pkg-ver-stable "[^[:blank:]{][^[:blank:]]+" parser-token-string)))
    )

  (and package (token package-record "\\\*") whitespace pkg-name)
  (and repository whitespace repo-name whitespace (+ pkg-version whitespace))
  )

(parser-trace-list paludis-trace
  (package t)
  (repository t))

;; need a local-map
(defun paludis-mode ()
  "turn on paludis mode"
  (interactive)
  (paludis-overlay)
  )

(provide 'paludis)
