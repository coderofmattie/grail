;;----------------------------------------------------------------------
;; gui.el
;; Primary Author: Mike Mattie
;;----------------------------------------------------------------------

;; (use-grail-groups "complete")

(setq use-dialog-box nil)                     ;; kill the dialogs before they strike.

(eval-after-load 'flyspell
  '(grail-set-faces
     (flyspell-incorrect (underline t))))

