;;----------------------------------------------------------------------
;; darwin.el
;; emacs does not get environment variables set from .profile and like
;; because it is launched by finder. Correct key environment variables.
;;----------------------------------------------------------------------

;; for best font support on a laptop LCD, the font hinting setting of "medium"
;; in the apple control panel works best, along with disabling anti-aliasing.
(setq mac-allow-anti-aliasing nil)

;; (setq ns-use-qd-smoothing nil)

(setq mac-option-modifier 'meta)  ;; oh happy day !!!
                                  ;; no more hellish binding of meta
                                  ;; on the mac key. can use CarbonEmacs again.

(server-start)

















