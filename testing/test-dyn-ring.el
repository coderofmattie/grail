(require 'dny-ring)

(setq foo (make-dyn-ring))

(dyn-ring-destroy foo)

(setq elm1 (dyn-ring-make-element 1))

(setq elm2 (dyn-ring-make-element 2))

(setq elm3 (dyn-ring-make-element 3))

(setq elm4 (dyn-ring-make-element 4))

(dyn-ring-insert foo elm1)

(dyn-ring-insert foo elm2)

(dyn-ring-insert foo elm3)

(dyn-ring-insert foo elm4)

(pp elm1)

(dyn-ring-traverse foo (lambda ( element )
                         (pp element)))

(dyn-ring-traverse foo (lambda ( element )
                         (message "value is %s " (pp-to-string (dyn-ring-element-value element)))))

(dyn-ring-delete foo elm2)


(progn
  (dyn-ring-rotate-left foo)
  (dyn-ring-value foo))

(progn
  (dyn-ring-rotate-right foo)
  (dyn-ring-value foo))


(dyn-ring-size foo)

(dyn-ring-delete foo elm1)

(dyn-ring-delete foo elm3)

(dyn-ring-value foo)

(dyn-ring-insert foo elm2)

(dyn-ring-value foo)


(dyn-ring-insert foo elm3)

(dyn-ring-value foo)




(dyn-ring-value foo)

(dyn-ring-size foo)


