;;----------------------------------------------------------------------
;; async-command-builders.el
;;----------------------------------------------------------------------

(defun async-build-chained-fn ( next-builder )
  (lexical-let
    ((builder next-builder))

    (lambda ()
      (apply 'grail-process-async-chain builder)) ))

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

(defmacro async-build-chained ( &rest body )
  (let
    ((this-builder nil))

    (dolist ( builder (reverse body) )
      (let
        ((evaluated (mapcar 'eval builder)))

        (setq this-builder
          (if this-builder
            (apply 'async-build-basic (append evaluated (list (async-build-chained-fn this-builder)) ))
            (apply 'async-build-basic evaluated))) ))

    `',this-builder))

(provide 'async-command-builders)
