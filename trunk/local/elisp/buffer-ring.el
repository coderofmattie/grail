;;;----------------------------------------------------------------------
;; buffer-ring.el
;; Primary Author: Mike Mattie
;; Copyright (C) 2009 Mike Mattie
;; License: LGPL-v3
;;;----------------------------------------------------------------------

(defconst buffer-ring-version "0.0.1")

;; TODO:

;; * test the global buffer ring implementation
;; * completion of the ring names

;; * some sort of global default ring so you can simply hit enter when
;;   entering a ring name.

;; * review for bugs and corner cases.

(require 'dynamic-ring)

(defvar the-one-ring (make-dyn-ring)
  "a global ring of all the buffer rings")

(defvar buffer-ring-default nil
  "The default buffer ring")

;;
;;  buffer ring structure
;;

(defun bfr-ring-name ( buffer-ring )
  (car (dyn-ring-get-value buffer-ring)))

(defun bfr-ring-ring ( buffer-ring )
  (cdr (dyn-ring-get-value buffer-ring)))

(defun make-bfr-ring ( name )
  (cons name (make-dyn-ring)))

;;
;;  global buffer ring functions
;;

(defun bfr-find-buffer-ring ( name )
  "bfr-find-buffer-ring NAME

   Search the global buffer ring for ring NAME and return it if
   found or nil otherwise.
  "
  (let
    ((found (dyn-ring-find the-one-ring
              (lambda ( buffer-ring )
                (when (string-equal (bfr-ring-name buffer-ring) name) t)) )))
    (when found
      (car found)) ))

(defun bfr-get-buffer-ring ( name )
  "bfr-get-buffer-ring NAME

   Find a existing buffer ring, or create a new buffer ring with name.
   buffer-ring-default is updated. The buffer-ring is returned.
  "
  (let
    ((buffer-ring (bfr-find-buffer-ring name)))

    (if buffer-ring
      ;; if it already exists return the ring.
      (progn
        (message "Adding to existing ring %s" name)
        buffer-ring)

      ;; otherwise create a new ring buffer, which is a cons of the
      ;; name and a ring. insert the ring into the global ring.
      (progn
        (message "Creating a new ring \"%s\"" name)
        (setq buffer-ring-default name)
        (dyn-ring-insert the-one-ring (dyn-ring-element (make-bfr-ring name))) )) ))

;;
;; buffer ring functions
;;

(defun bfr-buffer-ptr ( buffer )
  "bfring-buffer-ptr BUFFER

   make sure BUFFER is a string so that it will still point to a
   buffer when a buffer is destroyed and re-created.
  "
  (if (bufferp buffer)
    (buffer-name buffer)
    buffer))

(defun bfr-find-buffer ( buffer-ring buffer-name )
  "buffer-ring-find-buffer RING BUFFER

   Search buffer RING for BUFFER. return the buffer ring element
   if found, otherwise nil.
  "
  (let
    ((found (dyn-ring-find buffer-ring
              (lambda ( buffer )
                (when (string-equal (dyn-ring-get-value element) buffer-name) t)) )))
    (when found
      (car found)) ))

(defun bfr-add-buffer ( buffer-ring buffer )
  "bfr-add-buffer RING BUFFER

   Add BUFFER to buffer RING. If the buffer is already in the ring return
   the existing buffer element, or a new one inserted in the buffer RING.
  "
  (let*
    ((buffer-ptr     (bfr-buffer-ptr buffer))
     (buffer-element (bfr-find-buffer (bfr-ring-ring buffer-ring) buffer-ptr)))

    (if buffer-element
      (progn
        (message "buffer %s is already in ring \"%s\"" (buffer-name) (bfr-ring-name buffer-ring))
        buffer-element)
      (dyn-ring-insert (bfr-ring-ring buffer-ring) (dyn-ring-element buffer-ptr))) ))

(defun bfr-ring-size ( &optional buffer )
  "bfr-ring-size BUFFER

   Returns the number of buffers in the ring for BUFFER.
   If the buffer is not in a ring it returns -1 so that
   you can always use a numeric operator.
  "
  (with-current-buffer (or buffer (current-buffer))
    (if (bfr-in-ringp buffer)
      (dyn-ring-size (bfr-ring-ring buffer-ring))
      -1) ))

(defun bfr-in-ringp ( &optional buffer )
  "bfr-in-ringp &optional BUFFER

   return t if BUFFER is in a ring. The argument is optional,
   it defaults to the current buffer.
  "
  (with-current-buffer (or buffer (current-buffer))
    (and (local-variable-p 'buffer-ring) (local-variable-p 'buffer-ring-this-buffer))))

;;
;; buffer ring interface
;;

(defun buffer-ring-add ( name )
  "buffer-ring-add

   Add the current buffer to a ring. It will prompt for the ring
   to add the buffer to.
  "
  (interactive "sAdd to ring ? ")

  (set (make-local-variable 'buffer-ring) (bfr-get-buffer-ring name))
  (set (make-local-variable 'buffer-ring-this-buffer) (bfr-add-buffer buffer-ring (current-buffer)))
  (set (make-local-variable 'buffer-ring-modeline) (concat " Ring (" name ") "))

  (add-hook 'kill-buffer-hook 'buffer-ring-delete t t))

(defun buffer-ring-delete ()
  "buffer-ring-delete

   Delete the buffer from the ring. This modifies the ring, it does not
   kill the buffer.
  "
  (interactive)
  (if (bfr-in-ringp)
    (progn
      (dyn-ring-delete (bfr-ring-ring buffer-ring) buffer-ring-this-buffer)

      (kill-local-variable 'buffer-ring)
      (kill-local-variable 'buffer-ring-this-buffer)
      (kill-local-variable 'buffer-ring-modeline)

      (remove-hook 'kill-buffer-hook 'bfr-del-buffer t))
    (message "This buffer is not in a ring")))

(defun bfr-rotate-buffer-ring ( direction )
  (if (bfr-in-ringp)
    (let
      ((ring (bfr-ring-ring buffer-ring)))

      (if (< (dyn-ring-size ring) 2)
        (message "There is only one ring in the buffer")
        (progn
          (funcall direction ring)
          (switch-to-buffer (dyn-ring-value ring))) ))
    (message "This buffer is not in a ring") ))

(defun bfr-prev-buffer ()
  "bfr-prev-buffer

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (bfr-rotate-buffer-ring 'dyn-ring-rotate-left))

(defun bfr-next-buffer ()
  "bfr-next-buffer

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (bfr-rotate-buffer-ring 'dyn-ring-rotate-right))

(defun cycle-buffers-with-rings ()
  "cycle-buffers-with-rings

   When the buffer is in a ring cycle to the next buffer in the
   ring. If the buffer is not in a ring use other-buffer.
  "
  (interactive)
  (if (> (bfr-ring-size) 0)
    (bfr-next-buffer)
    (switch-to-buffer (other-buffer))))

;;
;; global ring interface
;;

(defun bfr-rotate-global-ring ( direction )
  (if (< (dyn-ring-size the-one-ring) 2)
    (message "There is only one buffer ring; ignoring the rotate command")
    (progn
      (funcall direction the-one-ring)
      (let
        ((buffer-ring (dyn-ring-value ring)))

        (if (< (dyn-ring-size buffer-ring) 1)
          (message "this buffer ring is empty; keeping the current buffer")
          (progn
            (message "switching to ring %s" (bfr-ring-name buffer-ring))
            (switch-to-buffer (dyn-ring-value (bfr-ring-ring buffer-ring)))
            (setq buffer-ring-default (bfr-ring-name buffer-ring)) )) )) ))

(defun bfr-prev-ring ()
  "bfr-prev-ring

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (bfr-rotate-global-ring 'dyn-ring-rotate-left))

(defun bfr-next-ring ()
  "bfr-next-ring

   Switch to the previous buffer in the buffer ring.
  "
  (interactive)
  (bfr-rotate-global-ring 'dyn-ring-rotate-right))

(provide 'buffer-ring)
