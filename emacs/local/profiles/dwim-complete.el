;;----------------------------------------------------------------------
;; dwim-complete
;;----------------------------------------------------------------------
(grail-load 'helm (grail-define-installer "helm"
                    "git"
                    "https://github.com/emacs-helm/helm.git"))

(require 'helm-config)

(defun dwim-complete/helm (prompt choices &optional complete-buffer)
  "Use helm to select a choice from a list of choices."
  (interactive)

  (let
    ((tmpsource nil)
     (cands     nil)
     (result    nil)
     (rmap      nil)
     (display-fn 'identity))

    (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
    (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))

    (setq tmpsource
      (list
        (cons 'name prompt)
        (cons 'candidates cands)
        '(action . (("Expand" . (lambda (selection) selection)))) ))

    (setq result (helm-other-buffer '(tmpsource) complete-buffer))

    (if (null result)
      (signal 'quit "user quit!")
      (cdr (assoc result rmap))))
    nil)

;; (dwim-complete/helm "crap" '(foo bar baz bing) (get-buffer-create "foo"))

(defun dwim-complete-prefix ( root-string other-string )
  (if (>= (length other-string) (length root-string))
    (let
      ((other-prefix (substring other-string 0 (length root-string))))

      (if (string-equal root-string other-prefix)
        other-string
        nil))
    nil))

(defun dwim-complete-filter-prefix ( root-string other-list )
  (let
    ((candidates nil))

    (mapc
      (lambda ( other )
        (let
          ((check-prefix (dwim-complete-prefix root-string other)))

          (when check-prefix
            (setq candidates (cons check-prefix candidates))) ))
      other-list)

    candidates))
