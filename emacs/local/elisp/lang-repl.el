;;----------------------------------------------------------------------
;; lang-repl.el - repl backend
;;
;; description:
;;
;; a repl backend for languages that provide REPL interfaces.
;;----------------------------------------------------------------------
(require 'subr-x)

(defvar lang-repl-table (make-hash-table :test 'equal))

(defvar lang-repl-create (make-hash-table :test 'equal))

(defun lang-repl-mode-make ( buffer-name )
  (cons buffer-name (list buffer-name)) )

(defun lang-repl-mode-list ( mode-pair )
  (cdr mode-pair))

(defun lang-repl-mode-default ( mode-pair )
  (car mode-pair))

(defun lang-repl-mode-next ( mode-pair )
  (let
    ((buffer-list (lang-repl-mode-list mode-pair))
     (default (lang-repl-mode-default mode-pair))
     (next nil))

    (while (car buffer-list)
      (if (string-equal default (car buffer-list))
        (let
          (( next-buffer (cdr buffer-list)))

          (setq next
            (if next-buffer
              (car next-buffer)
              (car (lang-repl-mode-list mode-pair))) )

          (setq buffer-list nil))
        (setq buffer-list (cdr buffer-list))) )

    (if next
      (progn
        (setcar mode-pair next)
        next)
      nil) ))

(defun lang-repl-mode-add ( mode repl-buffer )
  (let
    ((lookup (gethash mode lang-repl-table)))

    (if lookup
      (progn
        (setcdr lookup (cons repl-buffer (lang-repl-mode-list lookup)))
        (setcar lookup repl-buffer))
      (progn
        (setq lookup (lang-repl-mode-make repl-buffer)) ))

    (puthash mode lookup lang-repl-table)

    (with-current-buffer repl-buffer
      (lang-repl-mode-del-hook-fn mode))

    repl-buffer))

(defun lang-repl-mode-del ( mode repl-buffer )
  (let
    ((lookup (gethash mode lang-repl-table)))

    (when lookup
      (let
        (( deleted (delete repl-buffer (lang-repl-mode-list lookup)) ))

        (when (not (equal (length deleted) (length (lang-repl-mode-list lookup))))
          (setcar lookup (car deleted))
          (setcdr lookup deleted)) )

      (puthash mode lookup lang-repl-table)) ))

(defun lang-repl-mode-del-hook-fn ( mode )
  (add-hook 'kill-buffer-hook
    `(lambda ()
       (lang-repl-mode-del ,mode (buffer-name)))
    t
    t) )

(defun lang-repl-mode-define ( mode create-fn )
  "MODE CREATE-FN

  Define a repl create function for MODE. create
  will be called when a new repl is needed. CREATE-FN
  is called with a single argument: t for first call
  to create a REPL, nil for false.

  REPL creation should create a new REPL and post-creation
  call lang-repl-mode-add with the mode and buffer name.
"
  (puthash mode create-fn lang-repl-create))

(defun lang-repl-mode-create ( repl-mode )
  (let
    (( lookup    (gethash repl-mode lang-repl-table))
     ( create-fn (gethash repl-mode lang-repl-create)))

    (if create-fn
      (funcall create-fn (if lookup nil t))
      nil) ))

(defun lang-repl-mode-display ( repl-buffer )
  (pop-to-buffer repl-buffer)
  (other-window 1))

(defun lang-repl-select-repl-mode ()
  (catch 'abort
    (let
      ((lookup (gethash major-mode lang-repl-table))
       (repl-mode nil))

      (if lookup
        (setq repl-mode major-mode)
        (let
          (( repl-list (hash-table-keys lang-repl-table) ))

          (unless repl-list
            (setq repl-list (hash-table-keys lang-repl-create)))

          (unless repl-list
            (throw 'abort nil))

          (let
            ((input (completing-read
                      "please select a REPL: " repl-list)))

            (when (equal 0 (length input))
              (message "no REPL given, aborting.")
              (throw 'abort nil))

            (setq repl-mode input)) ))

      repl-mode)))

(defun lang-repl-cmd-create ()
  "lang-repl-cmd-create

   Create a new REPL even if there is already a REPL.
  "
  (interactive)

  (let
    (( repl-mode (lang-repl-select-repl-mode)))

    (lang-repl-mode-create repl-mode)))

(defun lang-repl-cmd-next ()
  "lang-repl-cmd-next

   Switch to the next REPL buffer for the mode.
  "
  (interactive)

  (let
    (( repl-mode (lang-repl-select-repl-mode) ))

    (when repl-mode
      (let
        (( lookup (gethash repl-mode lang-repl-table) ))

        (if lookup
          (let
            (( next-buffer (lang-repl-mode-next lookup) ))

            (if next-buffer
              (lang-repl-mode-display next-buffer)
              (message "could not cycle REPL for mode %s" repl-mode)) )

          (message "could not find mode %s REPL" repl-mode)) )) ))

(defun lang-repl-cmd-repl ()
  "lang-repl-cmd-repl

   Pop to the buffer of the REPL for mode or create one
   if no REPL has been started.
  "
  (interactive)

  (let
    (( repl-mode (lang-repl-select-repl-mode))
     (lookup nil))

    (if repl-mode
      (progn
        (setq lookup (gethash repl-mode lang-repl-table))

        (when lookup
          (setq lookup (lang-repl-mode-default lookup)))

        (if lookup
          (lang-repl-mode-display lookup)
          (lang-repl-mode-create repl-mode)) )
      (message "cannot determine repl mode")) ))

(custom-key-group "REPL" "r" t
  ("r" . lang-repl-cmd-repl)
  ("n" . lang-repl-cmd-next)
  ("c" . lang-repl-cmd-create))

(provide 'lang-repl)
