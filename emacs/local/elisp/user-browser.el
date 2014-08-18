;;----------------------------------------------------------------------
;; browser
;;
;; browser support required for documentation usually
;;----------------------------------------------------------------------
(require 'eww)

(defvar browser-profile-buffer-name nil)
(defvar browser-profile-ring-name nil)

(defvar-local browser-profile-url-command 'eww)
(defvar-local browser-profile-file-command 'eww-open-file)

(defun browser-profile-unique-p ()
  (and browser-profile-buffer-name browser-profile-ring-name))

(defun browser-profile-make-unique (new-buffer-name new-ring-name)
  (setq
    browser-profile-buffer-name new-buffer-name
    browser-profile-ring-name new-ring-name))

(defun browser-profile-clear-unique ()
  (setq
    browser-profile-buffer-name nil
    browser-profile-ring-name nil))

(defadvice eww-setup-buffer (after browser-profile-hooks)
  (when (browser-profile-unique-p)
    (rename-buffer browser-profile-buffer-name)

    (with-current-buffer browser-profile-buffer-name
      (buffer-ring/add browser-profile-ring-name)
      (buffer-ring/local-keybindings) )

    (browser-profile-clear-unique))

  ad-return-value)

(defun browser-profile-unique-enable ()
  (interactive)
  (ad-activate 'eww-setup-buffer))

(defun browser-profile-unique-disable ()
  (interactive)
  (ad-deactivate 'eww-setup-buffer))

(browser-profile-unique-enable)

(defun browser-profile-set-as-default ( &optional wrapper-fn )
  (make-variable-buffer-local 'browse-url-browser-function)
  (setq browse-url-browser-function (or wrapper-fn browser-profile-url-command)))

(provide 'user-browser)

