;; misc math hacks

;; handy for the Ms. Henning (math 085) homework assignments phrased in the form
;; of: multiples of x.

(defun multiples-of-n (lower upper x)
  (if (> lower upper)
    nil
    (cons lower (multiples-of-n (+ lower x) upper x))))

(defun multiples-of-f ( lower upper x )
  "a function generator that produces a multiples of iterator"
  (if (> lower upper)
    nil

    (if (not (= 0 (% lower x)))
      (multiples-of-f (+ lower 1) upper x) )

    (lexical-let
      ((f-lower lower)
        (f-upper upper)
        (fx     x))

      (lambda ( y )
        (if (= 0 y)
          f-lower
          (if (> y f-upper)
            nil
            (+ y fx) ))) )))

(defun multiples-of (lower upper x)
  "multiples-of lower upper x

   return a list of the multiples of x in the
   range inclusive of lower to upper.
  "
  (if (> lower upper)
    nil
    (if (= 0 (% lower x))
      (multiples-of-n lower upper x)
      (multiples-of (+ lower 1) upper x) )))

(defun multiples-of-system ( x set-fn )
  "the system version solves for two equations at the same time"
  (let
    ((step 0)
     (answer nil))

    (while (not (null step))
      (when (= 0 (% x (setq step (funcall set-fn step))))
        (setq answer nil))) ))

(defun test-mf ()
  "test the multiples of function generator"
  (let*
    ((mf  (multiples-of-f 3 33 3))
     (x   (funcall mf 0)) )

    (while (not (null x))
      (progn
        (print x)
        (setq x (funcall mf x)) )) ))

(defun solve-system ()
  "this is where it really gets interesting. a general system solver. neat."
  )
