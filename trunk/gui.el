;;----------------------------------------------------------------------
;; gui.el
;; Primary Author: Mike Mattie
;;----------------------------------------------------------------------

(use-grail-groups "complete")

;; colors for the cursor and the background. Kept in visual.el since
;; the colors need to be harmonious.

(setq use-dialog-box nil)                     ;; kill the dialogs before they strike.

(blink-cursor-mode -1)
(set-cursor-color "yellow")
(setq-default cursor-type 'hollow)

(set-mouse-color "red")

;; DejaVuLisp was modified from DejaVuMono from this link.
;; http://dejavu.sourceforge.net/wiki/index.php/Main_Page

(grail-set-faces
  ;; the default face

  (default
    (background "grey5")
    (foreground "grey70")
    (inverse-video nil)
    (box nil)
    (strike-through nil)
    (overline nil)
    (underline nil)
    (slant 'normal)
    (weight 'normal)
    (height 125)
    (width 'normal)
    (family "DejaVuLisp"))

  ;; comments are set off-tempature to distingiush them better.
  ;; orange was chosen as a red that wasn't harsh.
  (font-lock-comment-face (foreground "orange3"))

  ;; language syntax is the darkest shade of blue
  (font-lock-keyword-face (foreground "DeepSkyBlue4"))

  ;; grammar is the lightest shade of blue
  (font-lock-builtin-face (foreground "SkyBlue3"))

  ;; this should be for any form of literal value in code medium contrast.
  (font-lock-string-face  (foreground "grey50"))
  (font-lock-constant-face (foreground "grey50"))

  ;; decl is dark green
  (font-lock-type-face (foreground "green4"))
  (font-lock-function-name-face (foreground "aquamarine4"))
  (font-lock-variable-name-face (foreground "aquamarine3"))
  )

