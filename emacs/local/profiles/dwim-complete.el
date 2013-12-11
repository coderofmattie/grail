;;----------------------------------------------------------------------
;; dwim-complete
;;----------------------------------------------------------------------
(grail-load 'helm (grail-define-installer "helm"
                    "git"
                    "https://github.com/emacs-helm/helm.git"))

(require 'helm-config)

(setq helm-execute-action-at-once-if-one t)

(defun dwim-complete/make-action ( &optional fn )
  `(action . ,(if fn fn (lambda (selection) selection))))

(defun dwim-complete/make-source ( name candidates action )
  (let
    ((cands     (sort (mapcar (lambda (x) (identity x)) candidates) 'string-lessp)))

    `((name . ,name)
      (candidates . ,cands)
      ,action) ))

(defun dwim-complete/helm ( prompt input buffer &rest sources-list )
  (helm
    :sources sources-list
    :input input
    :prompt prompt
    :buffer buffer))

(defvar dwim-complete-global-sources nil)
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

(defun dwim-complete-global-add-source ( new-source )
  (setq dwim-complete-global-sources (cons new-source dwim-complete-global-sources)) )

(defun dwim-complete-local-add-source ( new-source )
  (setq dwim-complete-local-sources (cons new-source dwim-complete-local-sources)) )

(defun dwim-complete-behind-point ()
  (save-excursion
    (let*
      ((initial-point (point))
       (line-start (progn
                     (beginning-of-line)
                     (point)))
       (behind-point (- initial-point 1)))

      (catch 'abort
        (when (equal initial-point line-start)
          (throw 'abort ""))

        (when (equal behind-point line-start)
          (throw 'abort
            (char-to-string (char-before initial-point))) )

        (let
          (( found (search-backward-regexp "^\\|\\b" line-start t) ))

          (if (eq nil found)
            ""
            (progn
              (dwim-complete-set-stem found initial-point)
              (buffer-substring-no-properties found initial-point)) )) ))))

(defun dwim-complete/buffer ()
  (get-buffer-create "*complete*"))

(defun dwim-complete/complete ()
  (interactive)
  (apply 'dwim-complete/helm "complete: "

    (dwim-complete-behind-point)
    (dwim-complete/buffer)

    (mapcar 'funcall
      (or dwim-complete-local-sources dwim-complete-global-sources)) ))

(defun dwim-complete/for-buffer ( default-source )
  (make-variable-buffer-local 'dwim-complete-local-sources)

  (make-local-variable 'dwim-complete-stem-start)
  (make-local-variable 'dwim-complete-stem-stop)

  (setq dwim-complete-local-sources (list default-source))

  (local-set-key (kbd "<M-tab>") 'dwim-complete/complete) )
