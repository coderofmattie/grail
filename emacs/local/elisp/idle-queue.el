;;----------------------------------------------------------------------
;; idle-queue.el - idle queue for managing idle functions
;;
;; description:
;;
;; idle hook functions are very useful. Manage them in a central location
;;----------------------------------------------------------------------

;; how many idle seconds for the idle hook runs.
(defconst idle-queue-delay 4)

;; the list of all hooks to run.
(defvar idle-queue-list nil)

;; the list of hooks currently being processed.
(defvar idle-queue-active  nil)

;; timer object for idle queue
(defvar idle-queue-handle nil)

(defun idle-queue-start ()
  (unless idle-queue-handle
    (message "starting idle queue idle processing")

    (idle-queue-reset)

    (setq idle-queue-handle
      (run-with-idle-timer idle-queue-delay t 'idle-queue-processor)) ))

(defun idle-queue-stop ()
  (interactive)
  (message "stopping idle queue idle processing")

  (cancel-timer idle-queue-handle)

  (idle-queue-reset)
  (setq idle-queue-handle nil) )

(defun idle-queue-reset ()
  (interactive)

  (setq idle-queue-active nil)
  (setq idle-queue-list nil))

(defun idle-queue-processor ()
  (if (and idle-queue-list
           (not idle-queue-active))
    (let
      (( to-run (car idle-queue-list) ))

      (setq idle-queue-list (cdr idle-queue-list))

      (funcall to-run)

      (setq to-run nil)) ))

(defun idle-queue-add ( hook )
  (idle-queue-start)
  (setq idle-queue-list (cons hook idle-queue-list)) )

(provide 'idle-queue)
