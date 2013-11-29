;;----------------------------------------------------------------------
;; command-queue
;;
;; run commands in the background in a worker queue like implementation
;;----------------------------------------------------------------------
(require 'grail-profile)
(require 'async-command-builders)

(defconst cmd-queue-idle-start-work 4)

(defvar cmd-queue-tasks nil)
(defvar cmd-queue-task-in-progress nil)
(defvar cmd-queue-run-handle nil)

(defun cmd-queue-start ()
  (message "starting command queue")
  (setq cmd-queue-run-handle
    (run-with-idle-timer cmd-queue-idle-start-work t 'cmd-queue-processor)) )

(defun cmd-queue-stop ()
  (interactive)
  (cancel-timer cmd-queue-run-handle)
  (setq cmd-queue-run-handle nil) )

(defun cmd-queue-reset ()
  (interactive)
  (setq cmd-queue-tasks nil)
  (setq cmd-queue-task-in-progress) )

(defun cmd-queue-processor ()
  (when (cmd-queue-work-runnable-p)
    (cmd-queue-take-task)
    (cmd-queue-run-task)) )

(defun cmd-queue-get-work-buffer ()
  (get-buffer-create "*cmd-queue-work*"))

(defun cmd-queue-task-create ( command callback )
  (list (async-build-basic "cmd-queue" command 'cmd-queue-finish-task) callback command))

(defun cmd-queue-add-task ( command callback )
  (unless cmd-queue-run-handle
    (cmd-queue-start))

  (setq cmd-queue-tasks
    (cons (cmd-queue-task-create command callback) cmd-queue-tasks)) )

(defun cmd-queue-work-runnable-p ()
  (and cmd-queue-tasks (not cmd-queue-task-in-progress)) )

(defun cmd-queue-take-task ()
  (setq cmd-queue-task-in-progress (car cmd-queue-tasks))
  (setq cmd-queue-tasks (cdr cmd-queue-tasks)) )

(defun cmd-queue-run-task ()
  (message "command-queue: starting %s" (caddr cmd-queue-task-in-progress))
  (apply 'grail-process-async-chain (car cmd-queue-task-in-progress) ))

(defun cmd-queue-finish-task ( status )
  (message "command-queue: finished %s " (caddr cmd-queue-task-in-progress))

  (funcall (cadr cmd-queue-task-in-progress) (caddr cmd-queue-task-in-progress) status)
  (setq cmd-queue-task-in-progress nil) )

(provide 'command-queue)
