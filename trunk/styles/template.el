;;----------------------------------------------------------------------
;;         else = (E)macs (L)anguage (S)ensitive (E)diting.
;;----------------------------------------------------------------------

;; dummy init defun in case it does not load.
(defun else-xml-init () nil)

(require 'else-mode)

(setq
  else-kill-proceed-to-next-placeholder t)

(load-config "else-xml.el")  ;; until it's made into a proper mode my extensions reside in
                             ;; my config.

;; else-mode is definitely the crown jewel of my input expansion. Sets
;; the standard for macro expansion.

(defun else-xml-init ()
    (if (else-xml-load-language source-language)
      (progn

        ;; localize the current language to the buffer and set it properly
        (else-establish-language source-language)

        (else-mode)

        (else-xml-load-language-alist source-language)

        ;; here is where C-xe will expand templates
        (local-set-key (kbd "C-l e") 'else-expand-placeholder)
        (local-set-key (kbd "C-l n") 'else-next-placeholder)

        (local-set-key (kbd "C-l k") 'else-kill-placeholder)

        (local-set-key (kbd "C-l l") 'else-show-token-names)
        ))
  )
