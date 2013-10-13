;;----------------------------------------------------------------------
;; display.el
;; written by Mike Mattie
;;----------------------------------------------------------------------

(blink-cursor-mode -1)

;; emacs 24 seems to need this
(set-cursor-color "yellow")

;; parameters that need to be setup before frames are created.

(setq default-frame-alist
  (append '((cursor-color . "yellow")
            (mouse-color  . "red")
            (cursor-type  . "hollow") )
    default-frame-alist))

(grail-set-faces
  ;; the default face

  (default
    (background "grey5")
    (foreground "grey65")
    (inverse-video nil)
    (box nil)
    (strike-through nil)
    (overline nil)
    (underline nil)
    (slant 'normal)
    (weight 'normal)
    (height 140)
    (width 'normal)
    (family "DejaVu Sans Mono"))

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

  (font-lock-warning-face (underline "red") (foreground "grey70"))

  ;; decl is dark green
  (font-lock-type-face (foreground "green4"))
  (font-lock-function-name-face (foreground "aquamarine4"))
  (font-lock-variable-name-face (foreground "aquamarine3"))

  (ediff-current-diff-face-A (background "grey13") (foreground "dark goldenrod"))
  (ediff-current-diff-face-B (background "grey13") (foreground "dark khaki"))
  (ediff-current-diff-face-C (background "grey13") (foreground "olive drab"))

  (ediff-even-diff-face-A (background "grey12") (foreground "light slate grey"))
  (ediff-even-diff-face-B (background "grey12") (foreground "light slate grey"))
  (ediff-even-diff-face-C (background "grey12") (foreground "light slate grey"))

  (ediff-fine-diff-face-A  (underline "orange2"))
  (ediff-fine-diff-face-B  (underline "orange2"))
  (ediff-fine-diff-face-C  (underline "orange2"))
  )

(eval-after-load 'flyspell
  '(progn
     (grail-set-faces
       (flyspell-incorrect (underline "red")))))



