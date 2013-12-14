;;----------------------------------------------------------------------
;; idle-queue.el - idle queue for managing idle functions
;;
;; description:
;;
;; idle hook functions are very useful. Manage them in a central location
;;----------------------------------------------------------------------

;; how many idle seconds for the idle hook runs.
(defconst idle-queue-time-before-start 2)

;; the list of all hooks to run.
(defvar idle-queue-run-pool nil)

;; the list of hooks currently being processed.
(defvar idle-queue-run-list  nil)

;; timer object for idle queue
(defvar idle-queue-run-handle nil)

(defun idle-queue-start ()
  (unless idle-queue-run-handle
    (message "starting idle queue idle processing")

    (setq idle-queue-run-handle
      (run-with-idle-timer idle-queue-run-handle t 'idle-queue-processor)) ))

(defun idle-queue-stop ()
  (interactive)
  (message "stopping idle queue idle processing")

  (cancel-timer idle-queue-run-handle)
  (setq idle-queue-run-handle nil))

(defun idle-queue-reset ()
  (interactive)

  (setq idle-queue-run-pool nil)
  (setq idle-queue-run-list nil))

(defun idle-queue-processor ()
  (if idle-queue-run-list
    (let
      (( to-run (car idle-queue-run-list) ))

      (setq idle-queue-run-list (cdr idle-queue-run-list))

      (funcall to-run))

    (setq idle-queue-run-list idle-queue-run-pool)) )

(defun idle-queue-add ( hook )
  (idle-queue-start)
  (setq idle-queue-run-pool (cons hook idle-queue-run-pool)) )

(defun idle-queue-remove ( hook )
  (let
    (( updated nil ))

    (mapc
      (lambda ( old-hook )
        (unless (eq hook old-hook)
          (setq updated (cons old-hook updated)) ))
      idle-queue-run-pool)

    (setq idle-queue-run-pool updated) ))

(provide 'idle-queue)
