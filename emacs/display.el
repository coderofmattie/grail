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

(defun select-best-font-family ()
  (catch 'best-font
    (dolist (canidate platform-font-family)
      (when (member canidate (font-family-list))
        (throw 'best-font canidate))
      nil)))

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
    (width 'wide)
    (height 170)
    (width 'normal)
    (family (select-best-font-family)))

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
  (font-lock-variable-name-face (foreground "aquamarine3")) )

(set-face-attribute 'default nil
  :font (concat (select-best-font-family) ":spacing=m"))

(eval-after-load 'flyspell
  '(progn
     (grail-set-faces
       (flyspell-incorrect (underline "red")))))

(eval-after-load 'cperl-mode
  '(progn
     (setq cperl-invalid-face nil)

     (set-face-background 'cperl-array-face "grey5")
     (set-face-foreground 'cperl-array-face "aquamarine3")

     (set-face-background 'cperl-hash-face "grey5")
     (set-face-foreground 'cperl-hash-face "aquamarine3")

     (set-face-background 'cperl-nonoverridable-face "grey5")
     (set-face-foreground 'cperl-nonoverridable-face "DeepSkyBlue4") ))

(add-hook 'ediff-mode-hook 'set-custom-ediff-faces t)

(defun set-custom-ediff-faces ()
  (let
    ((diff-bg-color "khaki")
     (diff-bg-selected-color "khaki")
     (diff-fg-color "forest green"))

    (set-face-background 'ediff-current-diff-A diff-bg-selected-color)
    (set-face-foreground 'ediff-current-diff-A diff-fg-color)

    (set-face-background 'ediff-current-diff-B diff-bg-selected-color)
    (set-face-foreground 'ediff-current-diff-B diff-fg-color)

    (set-face-background 'ediff-current-diff-C diff-bg-selected-color)
    (set-face-foreground 'ediff-current-diff-C diff-fg-color)

    (set-face-background 'ediff-even-diff-A diff-bg-color)
    (set-face-foreground 'ediff-even-diff-A diff-fg-color)

    (set-face-background 'ediff-even-diff-B diff-bg-color)
    (set-face-foreground 'ediff-even-diff-B diff-fg-color)

    (set-face-background 'ediff-even-diff-C diff-bg-color)
    (set-face-foreground 'ediff-even-diff-C diff-fg-color)

    (set-face-background 'ediff-odd-diff-A diff-bg-color)
    (set-face-foreground 'ediff-odd-diff-A diff-fg-color)

    (set-face-background 'ediff-odd-diff-B diff-bg-color)
    (set-face-foreground 'ediff-odd-diff-B diff-fg-color)

    (set-face-background 'ediff-odd-diff-C diff-bg-color)
    (set-face-foreground 'ediff-odd-diff-C diff-fg-color)

    (set-face-background 'ediff-fine-diff-A "dark khaki")
    (set-face-foreground 'ediff-fine-diff-A "red")

    (set-face-background 'ediff-fine-diff-B "dark khaki")
    (set-face-foreground 'ediff-fine-diff-B "red")

    (set-face-background 'ediff-fine-diff-C "dark khaki")
    (set-face-foreground 'ediff-fine-diff-C "red") ))

