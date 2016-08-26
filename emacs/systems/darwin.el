;;----------------------------------------------------------------------
;; darwin.el
;;
;; configuration for a darwin platform. Make sure you set up the fonts
;; correctly or emacs barfs on the screen.
;;----------------------------------------------------------------------

;; emacs gets trashed if there is no font specified through
;; the grail system
(setq grail-font-family '("DejaVu Sans Mono" "Courier New"))

;; for some reason when I run emacs out of the dock
;; /usr/local/bin is missing.
(setq exec-path (cons "/usr/local/bin" exec-path))
