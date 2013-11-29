;;----------------------------------------------------------------------
;; async-command-builders.el
;;----------------------------------------------------------------------

(defun async-build-basic ( prefix command callback )
  (let
    ((grail-async-runner
       (lexical-let
         ((output-buffer (get-buffer-create (concat prefix "-proc")))
          (bind-command command)
          (bind-prefix  prefix)
          (bind-callback callback))

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
            nil)) ))
    grail-async-runner))

(provide 'async-command-builders)
