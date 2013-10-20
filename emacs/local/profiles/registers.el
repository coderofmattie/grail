;;----------------------------------------------------------------------
;; register support
;; written by Mike Mattie
;;----------------------------------------------------------------------
(require 'utilities)
(require 'dwim-tab)

(grail-load 'register-list (grail-define-installer "register-list"
                         "pkg"
                         'register-list))

(defvar reg-keyword-register-map
  "map of keywords to registers" '())

(defun build-map ( list )
  (cons (cons nil (car list))
    (if (eq nil (cdr list))
      nil
      (build-map (cdr list))) ))

(setq reg-keyword-register-map (build-map
  (list ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l
    ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z)))

(defun reg-update-mapping-keyword ( keyword mapping )
  (cons keyword (cdr mapping)))

(defun reg-find-and-apply ( compare mapping-fn )
  (lisp-map (lambda ( mapping-pair )
              (when (funcall compare mapping-pair)
                (funcall mapping-fn mapping-pair)))
    reg-keyword-register-map))

(defun reg-find-and-update ( compare update )
  (setq reg-keyword-register-map
    (lisp-map (lambda ( mapping-pair )
                (if (funcall compare mapping-pair)
                  (funcall update mapping-pair)
                  mapping-pair))
      reg-keyword-register-map) ))

(defun reg-find-register-by-keyword ( keyword )
  (lexical-let
    ((register-found nil))

    (reg-find-and-apply
      (lambda ( mapping-pair )
        (if (string= keyword (car mapping-pair))
          t
          nil))
      (lambda ( mapping-pair )
        (setq register-found (cdr mapping-pair))) )

    register-found))

(defun register-new-keyword ( keyword )
  "copy a region to a keyword mapped register"
  (interactive "sKeyword? ")
  (lexical-let
    ((insert-keyword keyword)
     (use-register nil))

    (reg-find-and-update
      (lambda ( mapping-pair )
        (if (and (eq use-register nil) (eq nil (car mapping-pair)))
          t
          nil))
      (lambda ( mapping-pair )
        (setq use-register (cdr mapping-pair))
        (cons insert-keyword use-register)) )

    (copy-to-register use-register (mark) (point)) ))

(defun register-delete-keyword ( keyword )
  "delete a keyword register mapping"
  (interactive "sKeyword? ")
  (lexical-let
    ((look-for-keyword keyword))

    (reg-find-and-update
      (lambda ( mapping-pair )
        (if (string= look-for-keyword (car mapping-pair))
          t
          nil))
      (lambda ( mapping-pair )
        (message "deleted keyword %s" look-for-keyword)
        (reg-update-mapping-keyword nil mapping-pair)) )))

(defun register-expand-keyword ()
  "expand a register mapping"
  (interactive)
  (let
    ((starting-point (point))
     (travel (re-search-backward "[[:word:]]+" (beginning-of-line) t 0)))

    (if (eq travel nil)
      (progn
        (goto-char starting-point)
        nil)
      (let
        ((register-mapping (reg-find-register-by-keyword
                             (buffer-substring-no-properties travel starting-point))))

        (if (eq nil register-mapping)
          (progn
            (goto-char starting-point)
            nil)
          (progn
            (delete-region travel starting-point)
            (insert-register register-mapping)) ))) ))

(dwim-tab-set-register-expand 'register-expand-keyword)



