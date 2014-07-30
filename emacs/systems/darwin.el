;;----------------------------------------------------------------------
;; darwin.el
;; emacs does not get environment variables set from .profile and like
;; because it is launched by finder. Correct key environment variables.
;;----------------------------------------------------------------------

(setq grail-font-family '("DejaVu LGC Sans Mono" "DejaVu Sans Mono" "Trebuchet MS"))

(mask-grail-profiles "spell")

(defun osx-merge-etc-paths-file ()
  (append
    (split-string (with-temp-buffer
                    (insert-file-contents-literally "/etc/paths")

                    (buffer-substring-no-properties
                      (progn
                        (beginning-of-buffer)
                        (point))
                      (progn
                        (end-of-buffer)
                        (point)) )) )
    exec-path))

;; unfuck osx which has no sane way to handle environment variables
;; or paths. only launchd.conf in /etc which sets variables globally
;; is honored.
;;
;; paths which works better is not honored except in the shell.

(setq exec-path (osx-merge-etc-paths-file))



















