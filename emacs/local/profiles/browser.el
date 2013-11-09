;;----------------------------------------------------------------------
;; browser
;;
;; browser support required for documentation usually
;;----------------------------------------------------------------------
(require 'eww)

(defvar eww-uniquify-buffer-name nil)
(defvar eww-uniquify-ring-name nil)

(defun uniquify-requested-p ()
  (if (and eww-uniquify-buffer-name eww-uniquify-ring-name)
    t
    nil))

(defun uniquify-request-unique (new-buffer-name new-ring-name)
  (setq
    eww-uniquify-buffer-name new-buffer-name
    eww-uniquify-ring-name new-ring-name))

(defun uniquify-clear ()
  (setq
    eww-uniquify-buffer-name nil
    eww-uniquify-ring-name nil))

(defadvice eww-setup-buffer (after uniquify-modify-buffer)
  (when (uniquify-requested-p)
    (buffer-ring-add eww-uniquify-ring-name)
    (rename-buffer eww-uniquify-buffer-name)

    (uniquify-clear))
  ad-return-value)

(defun uniquify-enable()
  (ad-activate 'eww-setup-buffer))

(defun uniquify-disable()
  (ad-deactivate 'eww-setup-buffer))

(defun popup-browse-html ( url &rest args)
  (let
    ((doc-buffer-name (generate-new-buffer-name "browser")))

    (pop-to-buffer doc-buffer-name)
    (eww url) ))

(defun set-popup-browse-html-local ()
  (make-variable-buffer-local 'browse-url-browser-function)
  (setq browse-url-browser-function 'popup-browse-html))

    ;; (with-current-buffer doc-buffer-name
    ;;   (make-variable-buffer-local 'kill-buffer-hook)
    ;;   (add-hook 'kill-buffer-hook
    ;;     (lambda ()
    ;;       (rid-window))
    ;;     t)

;; turn on uniquify by default
(uniquify-enable)



