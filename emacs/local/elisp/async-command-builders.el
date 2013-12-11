;;----------------------------------------------------------------------
;; async-command-builders.el
;;----------------------------------------------------------------------
(require 'macros)

(defun async-build-basic ( prefix command callback &optional use-buffer &optional chained )
  (let
    ((grail-async-runner
       (lexical-let
         ((output-buffer (or use-buffer
                             (get-buffer-create (concat prefix "-proc"))))
          (bind-command  command)
          (bind-prefix   prefix)
          (bind-callback callback)
          (bind-next     chained))

         `((lambda ()
             (start-process-shell-command ,bind-prefix ,output-buffer ,bind-command))

            (lambda ()
              (message "%s command %s did not start!" ,bind-prefix ,bind-command)
              (,bind-callback t)
              nil)

            (lambda ( exit-status )
              (message "%s command %s returned error! %s" ,bind-prefix ,bind-command exit-status)
              (,bind-callback exit-status)
              nil)

            (lambda ()
              (message "%s command %s completed." ,bind-prefix ,bind-command)
              (,bind-callback nil)
              t)
            ,bind-next)) ))
    grail-async-runner))

(defun async-build-exp ( body )
  `(async-build-basic ,@(car body)
     ,(if (eq nil (cdr body))
        'nil
        `(lambda ()
           (apply 'grail-process-async-chain ,(async-build-exp (cdr body))) )) ))

(defmacro async-build-chained ( &rest body )
  (macros-symbol-value-recursive
    (async-build-exp body)))

(provide 'async-command-builders)
