(setq foo (make-dyn-ring))


(setq elm1 (dyn-ring-element 1))

(setq elm2 (dyn-ring-element 2))

(setq elm3 (dyn-ring-element 3))

(princ elm1)

(dyn-ring-traverse foo (lambda ( element )
                         (princ (dyn-ring-get-value element))))


(dyn-ring-insert foo elm1)

(dyn-ring-value foo)

(dyn-ring-insert foo elm2)

(dyn-ring-value foo)

(dyn-ring-rotate-left foo)

(dyn-ring-insert foo elm3)

(dyn-ring-value foo)



(dyn-ring-delete foo elm2)

(dyn-ring-value foo)

(dyn-ring-size foo)


