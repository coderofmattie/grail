(progn
  (setq iter-test nil)

  (setq iter (tail-iterator 'iter-test))

  (funcall iter 'foo)
  (funcall iter 'bar)
  (funcall iter 'baz)

  (funcall iter '(woo hoo))

  (pp-to-string (tail-list iter-test)))


(progn
  (setq iter-test nil)

  (setq iter (tail-iterator 'iter-test))
  (funcall iter 'foo)
  (funcall iter (cons 'bar 'baz))

  (pp-to-string (tail-list iter-test)))
