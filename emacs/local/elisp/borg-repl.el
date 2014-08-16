;;----------------------------------------------------------------------
;; borg-repl.el
;;----------------------------------------------------------------------
(require 'buffer-ring)

(defun borg-repl/repl-name ( mode-name )
  (concat mode-name "/repl") )


(defun borg-repl/error-msg ( msg &rest info )
  (message "borg-repl Error: %s %s" msg
    (if info
      (concat "[ " (string-join (mapcar
                                 (lambda (x)
                                   (if (stringp x)
                                     x
                                     (pp-to-string x)))
                                 info) ", ") " ]")
      "")))

(defun borg-repl/bind-repl ( repl-name create-fn line-fn region-fn buffer-fn &optional defun-fn )
  (set (make-local-variable 'borg-repl/repl-name) repl-name)
  (set (make-local-variable 'borg-repl/create-repl) create-fn)
  (set (make-local-variable 'borg-repl/eval-line) line-fn)
  (set (make-local-varable  'borg-repl/eval-region) region-fn)
  (set (make-local-varable  'borg-repl/eval-buffer) buffer-fn)

  (when defun-fn
    (set (make-local-varable  'borg-repl/eval-defun) defun-fn)) )

(defun borg-repl/start ()
  "borg-repl/start

   start the REPL this buffer is configured for.
  "
  (interactive)

  (if (boundp 'borg-repl/create-repl)
    (funcall borg-repl/create-repl)
    (borg-repl/error-msg "no REPL start defined here.") ) )

(defun borg-repl/statement ()
  "borg-repl/statement

   eval the last statement.
  "
  (interactive)

  (if (boundp 'borg-repl/eval-line)
    (funcall borg-repl/eval-line)
    (borg-repl/error-msg "no REPL eval line defined here.") ) )

(defun borg-repl/region ()
  "borg-repl/region

   eval the region.
  "
  (interactive)

  (if (boundp 'borg-repl/eval-region)
    (funcall borg-repl/eval-region)
    (borg-repl/error-msg "no REPL eval region defined here.") ) )

(defun borg-repl/buffer ()
  "borg-repl/buffer

   eval the buffer.
  "
  (interactive)

  (if (boundp 'borg-repl/eval-buffer)
    (funcall borg-repl/eval-buffer)
    (borg-repl/error-msg "no REPL eval buffer defined here.") ) )

(defun borg-repl/definition ()
  "borg-repl/definition

   eval the definition.
  "
  (interactive)

  (if (boundp 'borg-repl/eval-defun)
    (funcall borg-repl/eval-defun)
    (borg-repl/error-msg "no REPL eval definition defined here.") ) )

(defun borg-repl/get-buffer ()
  "borg-repl/get-buffer

   get the current repl buffer for this buffer performing all the null
   checks along the way.
  "
  (if (boundp 'borg-repl/repl-name)
    (let
      (( repl-buffer (buffer-torus/get-ring-buffer borg-repl/repl-name) ))

      (if repl-buffer
        (progn
          (setq repl-buffer (get-buffer repl-buffer))

          (if repl-buffer
            repl-buffer
            (progn
              (borg-repl/error-msg "ghost REPL buffer does not exist anymore. will clean REPL ring.")
              (buffer-ring/fix (buffer-torus/search-rings borg-repl/repl-name))
              nil)) )
        (progn
          (borg-repl/error-msg "no REPL in this ring. try creating one.")
          nil)) )
    (progn
      (borg-repl/error-msg "no REPL for this buffer")
      nil) ) )

(defun borg-repl/view ()
  "borg-repl/view

   view the repl without switching to the repl.
  "
  (interactive)

  (let
    (( repl-buffer (borg-repl/get-buffer) ))

    (when repl-buffer
      (pop-to-buffer repl-buffer) ) ))

(defun borg-repl/switch ()
  "borg-repl/switch

   switch to the repl
  "
  (interactive)

  (let
    (( repl-buffer (borg-repl/get-buffer) ))

    (when repl-buffer
      (pop-to-buffer repl-buffer)
      (switch-to-buffer repl-buffer) ) ))

(defun borg-repl/next ()
  "borg-repl/next

   rotate the REPL ring right.
  "
  (interactive)

  (if (boundp 'borg-repl/repl-name)
    (let
      (( repl-ring (buffer-torus/search-rings borg-repl/repl-name) ))

      (if repl-ring
        (buffer-ring/next repl-ring)
        (progn
          (borg-repl/error-msg "no REPL in this ring. try creating one.")
          nil)) )
    (progn
      (borg-repl/error-msg "no REPL for this buffer")
      nil) ) )

(defun borg-repl/prev ()
  "borg-repl/prev

   rotate the REPL ring left.
  "
  (interactive)

  (if (boundp 'borg-repl/repl-name)
    (let
      (( repl-ring (buffer-torus/search-rings borg-repl/repl-name) ))

      (if repl-ring
        (buffer-ring/prev repl-ring)
        (progn
          (borg-repl/error-msg "no REPL in this ring. try creating one.")
          nil)) )
    (progn
      (borg-repl/error-msg "no REPL for this buffer")
      nil) ) )

(custom-key-group "borg repl" "r" t
  ("x" . borg-repl/start)
  ("e" . borg-repl/statement)
  ("r" . borg-repl/region)
  ("b" . borg-repl/buffer)
  ("d" . borg-repl/definition)
  ("v" . borg-repl/view)
  ("s" . borg-repl/switch)
  ("n" . borg-repl/next)
  ("p" . borg-repl/prev) )

(provide 'borg-repl)
