;;----------------------------------------------------------------------
;; browser
;;
;; browser support required for documentation usually
;;----------------------------------------------------------------------

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




