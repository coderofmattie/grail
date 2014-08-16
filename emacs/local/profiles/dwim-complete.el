;;----------------------------------------------------------------------
;; dwim-complete.el - advanced global completion system
;;
;; description:
;;
;; a completion system designed to be used as a engine for multiple
;; modules and as a interface that is generally usable throughout Emacs.
;; currently helm is outstanding for this purpose.
;;----------------------------------------------------------------------
(require 'thingatpt)

(grail-load-package 'helm "git" "https://github.com/emacs-helm/helm.git")

(require 'helm-config)

(setq helm-execute-action-at-once-if-one t)

(defun dwim-complete/make-name ( name )
  `(name . ,name))

(defun dwim-complete/make-candidates ( candidates-fn )
  `(candidates .
     (lambda ()
       (sort (mapcar (lambda (x) (identity x)) (,candidates-fn)) 'string-lessp)) ))

(defun dwim-complete/make-action ( &optional fn )
  `(action . ,(if fn fn (lambda (selection) selection))))

(defun dwim-complete/make-source ( name candidates-fn action )
  `(,(dwim-complete/make-name name)
    ,(dwim-complete/make-candidates candidates-fn)
    ,(dwim-complete/make-action action)))

(defun dwim-complete/helm ( prompt input buffer &rest sources-list )
  (helm
    :sources sources-list
    :input input
    :prompt prompt
    :buffer buffer))

(defvar dwim-complete-local-sources nil)

(defvar dwim-complete-stem-start nil)
(defvar dwim-complete-stem-stop nil)

(defun dwim-complete-set-stem ( start end )
  (setq dwim-complete-stem-start start)
  (setq dwim-complete-stem-stop end))

(defun dwim-complete-replace-stem ( completion )
  (when (and
          (and dwim-complete-stem-start dwim-complete-stem-stop)
          (> (- dwim-complete-stem-stop dwim-complete-stem-start) 0))

    (delete-region dwim-complete-stem-start dwim-complete-stem-stop)
    (goto-char dwim-complete-stem-start) )

  (insert (format "%s" completion)))

(defun dwim-complete-delete-stem ()
  (when (and
          (and dwim-complete-stem-start dwim-complete-stem-stop)
          (> (- dwim-complete-stem-stop dwim-complete-stem-start) 0))

    (delete-region dwim-complete-stem-start dwim-complete-stem-stop)
    (goto-char dwim-complete-stem-start)) )

(defvar dwim-complete-mode-sources (make-hash-table))
(defvar dwim-complete-mode-types (make-hash-table))

(defun dwim-complete-mode-add-source ( mode-name source )
  (let
    (( mode-source-list (gethash mode-name dwim-complete-mode-sources) ))

    (setq mode-source-list (cons source mode-source-list))

    (puthash mode-name mode-source-list dwim-complete-mode-sources) ))

(defun dwim-complete-mode-get-sources ( mode-name )
  (gethash mode-name dwim-complete-mode-sources))

(defun dwim-complete-mode-add-type ( mode-name type )
  (let
    (( type-list (gethash mode-name dwim-complete-mode-types) ))

    (setq type-list (cons type type-list))

    (puthash mode-name type-list dwim-complete-mode-types) ))

(defun dwim-complete-mode-check-type ( mode-name type )
  (let
    (( type-list (gethash mode-name dwim-complete-mode-types) ))

    (if type-list
      (member type type-list)
      nil) ))

(defun dwim-complete-behind-point ()
  (interactive)

  (let
    (( stem (thing-at-point 'symbol) )
     ( start nil )
     ( end (point)))

    (if stem
      (progn
        (dwim-complete-set-stem (- (point) (length stem)) end)
        stem)
      "") ))

(defun dwim-complete/buffer ()
  (get-buffer-create "*complete*"))


;; a buffer local value for the mode to use for dwim. nil
;; by default so the major-mode is the actual default.
(defvar dwim-mode nil)

(defun dwim-complete/set-mode ( mode )
  (set (make-local-variable 'dwim-mode) mode))

(defun dwim-complete/get-mode ()
  (or dwim-mode major-mode))

(defun dwim-complete/complete ()
  (interactive)
  (let
    (( completions (dwim-complete-mode-get-sources (dwim-complete/get-mode)) ))

    (if completions
      (apply 'dwim-complete/helm
        "complete: "
        (dwim-complete-behind-point)
        (dwim-complete/buffer)
        completions)
      (message "no completions available for mode: %s" (dwim-complete/get-mode)) ) ))

(defun dwim-complete/for-buffer ()
  (make-local-variable 'dwim-complete-stem-start)
  (make-local-variable 'dwim-complete-stem-stop)

  (local-set-key (kbd "<M-tab>") 'dwim-complete/complete) )

;;----------------------------------------------------------------------
;; keybindings and interfaces.
;;----------------------------------------------------------------------

(defun dwim-complete-vcs-or-file ()
  "dwim-complete vcs for file completion: use <spc> for contents search."
  (interactive)
  (helm-browse-project))

(provide 'profile/dwim-complete)
