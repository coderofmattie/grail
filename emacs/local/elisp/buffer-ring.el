;;----------------------------------------------------------------------
;; buffer-ring.el
;;
;; A torus for buffer navigation. A ring of buffers, and a ring of buffer
;; rings.
;;----------------------------------------------------------------------

(defconst buffer-ring-version "0.1.1" "buffer-ring version")

(require 'dynamic-ring)
(require 'buffer-status)
(require 'custom-key)

(defvar buffer-ring-torus (make-dyn-ring)
  "a global ring of all the buffer rings. A torus I believe.")

(defvar buffer-ring-default nil
  "The default buffer ring")

;;
;; error handling
;;

(define-error 'buffer-ring/error "buffer-ring error")

(defun buffer-ring/signal-error ( &rest error-info)
  (signal 'buffer-ring/error error-info))

(defmacro buffer-ring/catch-errors ( &rest body )
  `(condition-case trap-error
     (progn ,@body)

     ('buffer-ring/error
       (message "buffer-ring encountered error: %s") (pp-to-string trap-error))
     ('error
       (message "buffer-ring unknown error: %s" (pp-to-string trap-error)) ) ))

(defun buffer-ring/info ( msg &rest info)
  (message "buffer-ring: %s %s" msg
    (if info
      (concat "[ " (string-join (mapcar
                                 (lambda (x)
                                   (if (stringp x)
                                     x
                                     (pp-to-string x)))
                                 info) ", ") " ]")
      "")))

;;
;; buffer tools
;;

(defun buffer-ring/buffer-name ( &optional buffer )
  ;; a mode reload kills or alters somehow the
  ;; buffer object so use names always
  (if (stringp buffer)
    buffer
    (buffer-name buffer)) )

(defmacro buffer-ring/with-buffer ( buffer-name &rest body )
  `(let
     (( buffer-object (get-buffer ,buffer-name) ))

     (unless (stringp ,buffer-name)
       (buffer-ring/signal-error "non-string buffer reference" ,buffer-name))

     (unless buffer-object
       (buffer-ring/signal-error "buffer reference not found" ,buffer-name))

     (with-current-buffer buffer-object
       ,@body) ))

;;
;;  buffer ring structure
;;

(defun buffer-ring/ring-name ( buffer-ring )
  (car (dyn-ring-element-value buffer-ring)))

(defun buffer-ring/buffer-ring ( buffer-ring )
  (cdr (dyn-ring-element-value buffer-ring)))

(defun buffer-ring/make-ring ( name )
  (cons name (make-dyn-ring)))

;;
;;  buffer torus functions.
;;

(defun buffer-torus/find-ring ( name )
  "buffer-torus/find-ring NAME

   Search the buffer torus for a ring NAME and return it if found
   or nil otherwise.
  "
  (lexical-let*
    ((search-name name)
     (found (dyn-ring-find buffer-ring-torus
              (lambda ( found-name )
                (if (string= search-name (car (dyn-ring-element-value found-name)))
                  t
                  nil))) ))
    (when found
      (buffer-ring/buffer-ring (car found))) ))

(defun buffer-torus/get-ring ( name )
  "buffer-torus/get-ring NAME

   Find a existing buffer ring, or create a new buffer ring with name.
   buffer-ring-default is updated. The buffer-ring is returned.
  "
  (let
    ((buffer-ring (buffer-torus/find-ring name)))

    (if buffer-ring
      ;; if it already exists return the ring.
      (progn
        (buffer-status-add (format "Adding to existing ring: %s" name))
        buffer-ring)

      ;; otherwise create a new ring buffer, which is a cons of the
      ;; name and a ring. insert the ring into the global ring.
      (progn
        (buffer-ring/info "Creating a new ring" name)
        (let
          ((new-ring (dyn-ring-make-element (buffer-ring/make-ring name))))

          (dyn-ring-insert buffer-ring-torus new-ring)
          (buffer-ring/buffer-ring new-ring))) ) ))

;;
;; buffer ring variable functions
;;

;; the buffer ring local variables
(defvar buffer-ring nil)

(defvar buffer-ring-id nil)
(defvar buffer-ring-name nil)
(defvar buffer-ring-element nil)

(defvar buffer-ring-modeline nil)

(defun buffer-ring/mk-id ( ring )
  (let
    ((tentative-id nil))

    (catch 'free-id
      (dolist (attempt-count '(1 2 3 4 5))
        (setq tentative-id (number-to-string (random 100000)))

        (unless (buffer-ring/search-ring ring tentative-id)
          (throw 'free-id tentative-id)) )
      (buffer-ring/signal-error "could not find a free id after several attempts") )) )

(defun buffer-ring/buffer-id ( &optional buffer )
  (buffer-ring/with-buffer (buffer-ring/buffer-name buffer)
    (if (boundp 'buffer-ring-id)
      buffer-ring-id
      nil) ))

(defun buffer-ring/local-ring-name ( buffer )
  (buffer-ring/with-buffer (buffer-ring/buffer-name)
    buffer-ring-name))

(defun buffer-ring/search-buffer-list ( id )
  (catch 'found-buffer
    (dolist (search-buffer (buffer-list))
      (let*
        ((search-name (buffer-ring/buffer-name search-buffer))
         (discover-id (buffer-ring/buffer-id search-name)) )

        (when (equal id discover-id)
          (throw 'found-buffer search-name)) ))
    nil))

(defun buffer-ring/search-ring ( buffer-ring id )
  "buffer-ring/search-ring RING ID

   Search buffer RING for ID. return the buffer ring element
   if found, otherwise nil.
  "
  (let
    ((found (dyn-ring-find buffer-ring
              (lambda ( ring-element )
                (when (string-equal (dyn-ring-element-value ring-element) id) t)) )))
    (if found
      (car found)
      nil) ))

(defun buffer-ring/insert-buffer ( ring-name insert-buffer )
  "buffer-ring/insert-buffer RING BUFFER

   Add BUFFER to buffer RING. If the buffer is already in the ring return
   the existing buffer element, or a new one inserted in the buffer RING.
  "
  (when (buffer-ring/buffer-id (buffer-ring/buffer-name insert-buffer))
    (buffer-ring/with-buffer insert-buffer
      (buffer-ring/signal-error "buffer-ring/insert-buffer buffer is already in a ring"
        insert-buffer
        (buffer-ring/local-ring-name)) ))

  (buffer-ring/with-buffer insert-buffer

    ;; create the buffer variables and markers
    (set (make-local-variable 'buffer-ring) (buffer-torus/get-ring ring-name))
    (set (make-local-variable 'buffer-ring-name) ring-name)
    (set (make-local-variable 'buffer-ring-id) (buffer-ring/mk-id buffer-ring))
    (set (make-local-variable 'buffer-ring-modeline) (concat "(" ring-name ")"))

    (set (make-local-variable 'buffer-ring-element)
      (dyn-ring-insert buffer-ring (dyn-ring-make-element buffer-ring-id))) ) )

(defun buffer-ring/ring-size ( &optional buffer-name )
  "buffer-ring/ring-size &optional BUFFER

   Returns the number of buffers in the ring for BUFFER.
   If the buffer is not in a ring it returns -1 so that
   you can always use a numeric operator.
  "
  (buffer-ring/with-buffer (or buffer-name (buffer-ring/buffer-name))
    (if buffer-ring
      (dyn-ring-size buffer-ring)
      -1) ))

;;
;; buffer ring interface
;;

(defun buffer-ring/add ( ring-name )
  "buffer-ring/add

   Add the current buffer to a ring. It will prompt for the ring
   to add the buffer to.
  "
  (interactive "sAdd to ring ? ")

  (if (buffer-ring/buffer-id (buffer-ring/buffer-name))
    (progn
      (buffer-ring/info
        "This buffer is already in a ring, delete it before adding it to another ring")
      nil)
    (progn
      (buffer-ring/insert-buffer ring-name (buffer-ring/buffer-name))
      (add-hook 'kill-buffer-hook 'buffer-ring/delete t t)
      t) ))

(defun buffer-ring/delete ( &optional buffer-name )
  "buffer-ring/delete

   Delete the buffer from the ring. This modifies the ring, it does not
   kill the buffer.
  "
  (interactive)

  (if (buffer-ring/buffer-id buffer-name)
    (progn
      (dyn-ring-delete buffer-ring buffer-ring-element)

      (kill-local-variable 'buffer-ring)

      (kill-local-variable 'buffer-ring-id)
      (kill-local-variable 'buffer-ring-name)
      (kill-local-variable 'buffer-ring-element)

      (kill-local-variable 'buffer-ring-modeline)

      (remove-hook 'kill-buffer-hook 'buffer-ring-delete t))
    (buffer-ring/info "This buffer is not in a ring") ))

(defun buffer-ring/fix ( &optional other-ring )
  "buffer-ring/fix

  fix a buffer ring by pruning any references to buffers that do not exist."
  (interactive)

  (let*
    (( with-ring (or other-ring buffer-ring))
     (count 0))

    (dyn-ring-traverse with-ring
      (lambda ( ring-element )
        (let
          (( search (buffer-ring/search-buffer-list (dyn-ring-element-value ring-element)) ))

          (unless search
            (setq count (+ count 1))
            (dyn-ring-delete with-ring ring-element) ) )) )

    (when (> count 0)
      (message "buffer-ring: fixed %s dead buffer references" count))

    buffer-ring))

(defun buffer-ring/list-buffers ()
  "buffer-ring/list-buffers

   List the buffers in the buffer-ring associated with the current buffer.
  "
  (interactive)

  (if (buffer-ring/buffer-id)
    (let
      ((buffer-list nil)
       (buffer-found nil))

      (dyn-ring-map (buffer-ring/fix)
        (lambda ( buffer-id )
          (setq buffer-found (buffer-name (buffer-ring/search-buffer-list buffer-id)))

          (if buffer-list
            (setq buffer-list (concat buffer-found "," buffer-list))
            (setq buffer-list buffer-found)) ))

      (message "buffer-ring [%s] has buffers: %s" buffer-ring-name buffer-list))
    (message "This buffer is not in a ring.") ))

(defun buffer-ring/switch-to-buffer ( id )
  (let
    ((target-buffer (buffer-ring/search-buffer-list id) ))

    (if target-buffer
      (switch-to-buffer target-buffer)
      (buffer-ring/info "buffer to switch to not found .. very bad")) ))

(defun buffer-ring/rotate ( direction &optional other-ring )
  (let
    (( with-ring (or other-ring buffer-ring) ))

    (if (< (dyn-ring-size (buffer-ring/fix with-ring)) 2)
      (buffer-ring/info "There is only one buffer in the ring.")
      (progn
        (funcall direction with-ring)
        (buffer-ring/switch-to-buffer (dyn-ring-value with-ring)) )) ))

(defun buffer-ring/prev ( &optional other-ring )
  "buffer-ring/prev

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (buffer-ring/rotate 'dyn-ring-rotate-left other-ring))

(defun buffer-ring/next ( &optional other-ring )
  "buffer-ring/next

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (buffer-ring/rotate 'dyn-ring-rotate-right))

;;
;; buffer torus interface
;;

(defun buffer-torus/current-name ()
  (car (dyn-ring-value buffer-ring-torus)))

(defun buffer-torus/current-ring ()
  (cdr (dyn-ring-value buffer-ring-torus)))

(defun buffer-torus/rotate ( direction )
  (if (< (dyn-ring-size buffer-ring-torus) 2)
    (buffer-ring/info "There is only one buffer ring; ignoring the rotate global ring command")
    ;; rotate past any empties
    (if (dyn-ring-rotate-until
          buffer-ring-torus
          direction
          (lambda ( buffer-ring )
            (not (dyn-ring-empty-p (cdr buffer-ring)))))
      (progn
        (buffer-ring/info "switching to ring" (buffer-torus/current-name))
        (let
          ((current-head (dyn-ring-value (cdr (dyn-ring-value buffer-ring-torus)))))

          (buffer-ring/switch-to-buffer current-head) ))
      (buffer-ring/info "All of the buffer rings are empty. Keeping the current ring position")) ))

(defun buffer-torus/next ()
  "buffer-torus/next

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (buffer-torus/rotate 'dyn-ring-rotate-right))

(defun buffer-torus/prev ()
  "buffer-torus/prev

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (buffer-torus/rotate 'dyn-ring-rotate-left))

(defun buffer-torus/list-rings ()
  "buffer-torus/list-rings

   List the buffer rings in the buffer torus.
  "
  (interactive)

  (let
    ((ring-list nil))

    (mapc
      (lambda ( name )
        (setq ring-list
          (if ring-list
            (concat name "," ring-list)
            name)))
      (dyn-ring-map buffer-ring-torus 'car))

    (buffer-ring/info "buffer rings: %s" ring-list) ))

(defun buffer-torus/search-rings ( ring-name )
  "buffer-torus/search-rings RING-NAME

   search the torus for a ring matching RING-NAME
  "
  (interactive)

  (catch 'found
    (dyn-ring-map buffer-ring-torus
      (lambda ( ring )
        (when (string-equal (car ring) ring-name)
          (throw 'found (cdr ring))) ) )
    nil) )

(defun buffer-torus/get-ring-buffer ( ring-name )
  "buffer-torus/get-ring RING-NAME

   return the buffer ring for RING-NAME
  "
  (let
    (( found-ring (buffer-torus/search-rings ring-name) ))

    (when found-ring
      (buffer-ring/search-buffer-list (dyn-ring-value found-ring)) ) ))

(defun buffer-torus/delete ()
  "buffer-torus/delete

   Delete the entire current buffer-ring.
  "
  (interactive)

  (save-excursion
    (mapc
      (lambda ( buffer-name )
        (with-current-buffer buffer-name
          (buffer-ring/delete)))

      (dyn-ring-map (buffer-torus/current-ring) (lambda ( buffer-name )
                                         buffer-name)) )
    (dyn-ring-delete buffer-ring-torus (car buffer-ring-torus)) ))

;;
;; default keymap
;;

(defun buffer-ring/global-keybindings ()
  "buffer-ring-keybindings

   set the global keybindings"

  (global-set-key (kbd "<M-up>")   'buffer-torus/next)
  (global-set-key (kbd "<M-down>") 'buffer-torus/prev)

  (global-set-key (kbd "<M-right>")  'buffer-ring/next)
  (global-set-key (kbd "<M-left>")   'buffer-ring/prev)

  (custom-key-group "buffer ring" "b" t
    ("b" . buffer-ring/list-buffers)
    ("r" . buffer-torus/list-rings)
    ("a" . buffer-ring/add)
    ("d" . buffer-ring/delete)
    ("f" . buffer-ring/fix)
    ("k" . buffer-torus/delete) ) )

(defun buffer-ring/local-keybindings ()
  (local-unset-key (kbd "<M-right>"))
  (local-unset-key (kbd "<M-left>"))

  (local-unset-key (kbd "<M-up>"))
  (local-unset-key (kbd "<M-down>")) )

(buffer-ring/global-keybindings)

(provide 'buffer-ring)

