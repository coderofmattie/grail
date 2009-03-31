(defun deploy-query-inquisitio-search ( package )
  (list "-s" package)
  )

;; within this window can I make hyperlinks where the hyperlink triggers an install
;; method ?

;; can I extract available versions ? , installed versions ? , matching packages ?
(defun deploy-paludis ( package )
  "search for an Emacs package with Paludis's inquisitio search tool."
  (interactive "MPackage? ")

  (lexical-let
    ((search-buffer (generate-new-buffer "deploy-paludis")))

    (with-current-buffer search-buffer
      (unless (= 0 (apply 'call-process "inquisitio"     ;; search program
                     nil                                 ;; no stdin
                     (list (current-buffer) nil)         ;; discard stderr , stdout -> current-buffer
                     nil                                 ;; don't refresh

                     "--category" "app-emacs"            ;; without this constraint inquisitio
                                                         ;; is slow to moribund.
                     (deploy-query-inquisitio-search package) ;; construct search arguments
                     ))
        ;; need an error path here.
        )

      (setq show-trailing-whitespace nil)   ;; disable trailing whitespace

      ;; when we kill the buffer get rid of the window associated so the user
      ;; doesn't have to tediously clean-up.
      (add-hook 'kill-buffer-hook 'rid-window t t)
      )

    (pop-to-buffer search-buffer) ))
