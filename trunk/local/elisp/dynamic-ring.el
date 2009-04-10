;;;----------------------------------------------------------------------
;; dynamic-ring.el
;; written by: Mike Mattie
;;;----------------------------------------------------------------------

;;
;; ring structure
;;

(defun make-dyn-ring ()
  "make-dyn-ring

   Return a new dynamic ring stucture.
  "
  (cons nil 0))

(defun dyn-ring-emptyp ( ring-struct )
  "dyn-ring-emptyp RING

   return t if RING has no elements.
  "
  (not (car ring-struct)))

(defun dyn-ring-size ( ring-struct )
  "dyn-ring-size RING

   Return the number of elements in RING.
  "
  (cdr ring-struct))

;;
;; ring elements
;;

(defconst dyn-ring-linkage 0)
(defconst dyn-ring-value   1)

(defun dyn-ring-element ( value )
  "dyn-ring-element VALUE

   Create a new dynamic ring element with VALUE.
  "
  (let
    ((new-elm (make-vector 2 nil)))
    (aset new-elm dyn-ring-value value)
    (aset new-elm dyn-ring-linkage (cons nil nil))
    new-elm))

(defun dyn-ring-get-value ( element  )
  "dyn-ring-get-value ELEMENT

   Return the value of ELEMENT.
   "
  (aref element dyn-ring-value))

(defun dyn-ring-set-value ( element value )
  "dyn-ring-set-value ELEMENT VALUE

   Set the value of ELEMENT to VALUE.
  "
  (aset element dyn-ring-value value))

(defun dyn-ring-value ( ring-struct )
  "dyn-ring-value RING

   Return the value of RING's head.
  "
  (when (car ring-struct)
    (aref (car ring-struct) dyn-ring-value)))

;;
;; ring traversal.
;;

(defun dyn-ring-traverse ( ring-struct fn )
  (let
    ((head    (car ring-struct)))

    (when head
      (funcall fn head)

      (let
        ((current (cdr (aref head dyn-ring-linkage))))

        ;; loop until we return to the head
        (while (and current (not (eq current head)))
          (funcall fn current)
          (setq current (cdr (aref current dyn-ring-linkage))))
        t))))

(defun dyn-ring-find ( ring-struct predicate )
  (lexical-let
    ((found nil)
     (p     predicate))

    (dyn-ring-traverse ring-struct
      (lambda ( element )
        (when (funcall p element)
          (push element found))))

      found))

;;
;; ring modification functions.
;;

(defun dyn-ring-head-linkage ( ring-struct )
  (let
    ((head (car ring-struct)))
    (when head (aref head dyn-ring-linkage))))

(defun dyn-ring-destroy ( ring-struct )
  "dyn-ring-destroy  RING

   Delete the RING. The circular linkage of a ring structure
   makes it doubtful that the garbage collector will be able to
   free a ring.
  "
  (let
    ((linkage (dyn-ring-head-linkage ring-struct)))

    (when linkage

      (if (cdr linkage)
        (progn
          ;; There is more than one element. Break the ring by
          ;; terminating the left element
          (setcdr (cdr linkage) nil)

        (while (cdr linkage)
          (let
            ((right (cdr linkage)))

            ;; delete all the links in the current element
            (setcdr linkage nil)
            (setcar linkage nil)

            ;; move to the right
            (setq linkage (aref right dyn-ring-linkage)) )))
        ;; only one link, so delete the head pointer.
        (setcar ring-struct nil)) )))

(defun dyn-ring-link ( left element right )
  (let
    ((insert-linkage (aref element dyn-ring-linkage)))

    ;; double link the left side
    (setcdr (aref left dyn-ring-linkage) element)
    (setcar insert-linkage left)

    ;; double link the right side
    (setcar (aref right dyn-ring-linkage) insert)
    (setcdr insert-linkage right)))

(defun dyn-ring-insert ( ring-struct insert )
  (let
    ((ring-size (dyn-ring-size ring-struct))
     (head-linkage (dyn-ring-head-linkage ring-struct)))

    (when head-linkage
      (let
        ((insert-linkage (aref insert dyn-ring-linkage)))

        (cond
          ((equal 1 ring-size)
            (progn
              ;; a single element is a special case as both
              ;; the left and right linkage are nil.

              ;; link the existing element to the new element
              (setcar head-linkage insert)
              (setcdr head-linkage insert)

              ;; link the new element to the head
              (setcar insert-linkage (car ring-struct))
              (setcdr insert-linkage (car ring-struct))))

          ((equal 2 ring-size)
            ;; two elements is a special case because both
            ;; elements point to each other.
            (let
              ((other  (car head-linkage))
               (head   (car ring-struct)))

              (dyn-ring-link other insert head)))

          ((> ring-size 2)
            (let
              ((left  (car head-linkage))
               (right (cdr head-linkage)))

              (dyn-ring-link left insert right))) ) )))

  ;; point the head at the new element
  (setcar ring-struct insert)
  ;; update the element count.
  (setcdr ring-struct (+ (cdr ring-struct) 1))

  ;; return the newly inserted element.
  insert)

(defun dyn-ring-link-left-to-right ( left right )
  (setcdr (aref left  dyn-ring-linkage) right)
  (setcar (aref right dyn-ring-linkage) left))

(defun dyn-ring-unlink-element ( element )
  (let
    ((linkage (aref element dyn-ring-linkage)))

    (dyn-ring-link-left-to-right (car linkage) (cdr linkage))
    (cdr linkage) ))

(defun dyn-ring-delete ( ring-struct element )
  (let
    ((ring-size (dyn-ring-size ring-struct)))

    (cond
      ((equal 0 ring-size) nil)
      ((equal 1 ring-size)
        ;; when there is only one element we ignore element and simply
        ;; delete the head and reset the size.
        (progn
          (setcar ring-struct nil)
          (setcdr ring-struct 0)
          t))
      ((equal 2 ring-size)
        ;; if there are two elements nullify the other element's links
        ;; and reset both head and the size.
        (progn
          (let*
            ((other   (car  (aref element dyn-ring-linkage)))
             (linkage (aref other dyn-ring-linkage)))

            (setcar linkage nil)
            (setcdr linkage nil)

            (setcar ring-struct other)
            (setcdr ring-struct 1) )
          t))

      ;; for three or more elements perform a unlink.
      (t
        (let
          ((right (dyn-ring-unlink-element element)))

          ;; if we deleted the head element set the
          ;; head to the right element.
          (when (eq (car ring-struct) element)
            (setcar ring-struct right))

          (setcdr ring-struct (- (cdr ring-struct) 1))
          t)) )))

(defun dyn-ring-rotate ( ring-struct direction )
  "dyn-ring-rotate RING DIRECTION

   This is an internal function. To rotate the ring use
   dyn-ring-rotate-left or dyn-ring-rotate-right.

   Rotate the ring in DIRECTION: left = 'car , right = 'cdr.
   If the ring is empty nil is returned. If the ring
   has a single element the element is returned.

   Otherwise the head of the ring is set to the element
   of DIRECTION, and the element is returned.
  "
  (let
    ((linkage (dyn-ring-head-linkage ring-struct)))

    (when linkage
      (let
        ((link-to (funcall direction linkage)))

        (if link-to
          (setcar ring-struct link-to)) )) ))

(defun dyn-ring-rotate-left ( ring-struct )
  (dyn-ring-rotate ring-struct 'car))

(defun dyn-ring-rotate-right ( ring-struct )
  (dyn-ring-rotate ring-struct 'cdr))

(provide 'dynamic-ring)



