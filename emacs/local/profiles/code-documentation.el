;;----------------------------------------------------------------------
;; code-documentation
;;----------------------------------------------------------------------

(defvar-local documentation-buffer-name nil)
(defvar-local documentation-ring-name nil)
(defvar-local documentation-browse-fn nil)

(defvar-local documentation-browse-default-url nil)
(defvar-local documentation-browse-current-url nil)

;; make tab work like ctrl-x-o

(defun code-documentation-browse-popup ( url )
  (setq documentation-browse-current-url url)
  (browser-profile-make-unique documentation-buffer-name documentation-ring-name)

  (funcall browser-profile-url-command url)

  (pop-to-buffer documentation-buffer-name nil t)

  (other-window)
  )

(defun code-documentation-browse-action ( &optional url )
  (if (get-buffer documentation-buffer-name)
    (if (string-equal url documentation-browse-current-url)
      (pop-to-buffer documentation-buffer-name)
      (progn
        (kill-buffer (get-buffer documentation-buffer-name))
        (code-documentation-browse-popup (or url documentation-browse-default-url))) )

    (code-documentation-browse-popup (or url documentation-browse-default-url)) ))

(defun code-documentation-setup ( buffer-name ring-name default-url )
  (setq
    documentation-buffer-name buffer-name
    documentation-ring-name ring-name
    documentation-browse-fn browse-fn)

  (make-variable-buffer-local 'browse-url-browser-function)
  (setq browse-url-browser-function 'code-documentation-browse-popup))

(defun browser-profile-set-as-default ()
  (make-variable-buffer-local 'browse-url-browser-function)
  (setq browse-url-browser-function browser-profile-url-command))

(defun code-documentation-setup ( buffer-name ring-name default-url )
  (setq
    documentation-buffer-name buffer-name
    documentation-ring-name docs-ring-name
    documentation-browse-default-url default-url)

  (browser-profile-set-as-default code-documentation-browse-action))
