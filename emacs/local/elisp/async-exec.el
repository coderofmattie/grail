;;----------------------------------------------------------------------
;; async-exec
;;----------------------------------------------------------------------

(defun async-exec-chain ( start-process-fn doesnt-start-fn proc-fail-fn
                                   do-after-fn next-fn)
  "grail-process-async-chain START-PROCESS-FN DOESNT-START-FN PROC-FAIL-FN
                             DO-AFTER-FN NEXT-FN

   create asynchronous processes that can be changed. START-PROCESS-FN
   creates a process object. This function generates a process sentinel
   and attaches the sentinel to the process.

   a number of lambdas are supplied in the arguments to fill in the body
   of the process sentinel.

   DOESNT-START-FN: executed if the process does not start.

   PROC-FAIL-FN   : executed if the process returns an error (a non-zero exit code).
   DO-AFTER-FN    : executed when the process exits with success (0 exit code)
   NEXT-FN        : when DO-AFTER-FN returns non-nil this function is executed,
                    typically to chain another async process, but it can do
                    anything.

   With this function processes can be changed by nesting another
   grail-process-async-chain as the tail, or NEXT-FN function for
   a sequence of process execution."
  (lexical-let
    ((async-proc (funcall start-process-fn))
     (no-start   doesnt-start-fn)
     (fail-fn    proc-fail-fn)
     (after-fn   do-after-fn)
     (chain-fn   next-fn))

    (if (or (not (processp async-proc))
            (not (process-status async-proc)))
      (funcall doesnt-start-fn)
      (progn
        ;; setup a lambda process sentinal that does the chaining.
        (set-process-sentinel async-proc
          ;; a sentinal that accepts status-change signals
          (lambda ( bound-proc status-change )
            (when (memq (process-status bound-proc) '(signal exit))
              ;; do something when the process exits
              (if (equal 0 (process-exit-status bound-proc))

                ;; If bound-proc process exits with success call the
                ;; do-after-exit function (do-after-fn).

                ;; If (do-after-fn) returns non-nil, and the (next-fn)
                ;; is non-nil run that function.
                (and (funcall after-fn) (and chain-fn (funcall chain-fn)))

                ;; if the process exits non-zero call (proc-fail-fn)
                (funcall fail-fn (process-exit-status bound-proc)) ))) )) )))

(defun async-exec-sentinel ( proc-buffer on-exit )
  "async-exec-sentinal PROC-BUFFER ON-EXIT

   run ON-EXIT after process in PROC-BUFFER
  "
  (lexical-let*
    ((p-buffer   (current-buffer))
     (p-proc     (get-buffer-process proc-buffer))
     (p-sentinel (make-symbol "async-exec-sentinal"))
     (p-handler  on-exit) )

    (fset
      p-sentinel
      (lambda ( process event )
        (when (equal process p-proc)
          (funcall p-handler p-buffer) ) ))

    (set-process-sentinel p-proc p-sentinel) ))

(provide 'async-exec)
