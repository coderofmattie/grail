;;----------------------------------------------------------------------
;; user-display.el
;;----------------------------------------------------------------------

(defconst codermattie-bg-color "grey5")

(set-face-background 'default codermattie-bg-color)
(set-face-foreground 'default "grey55")

;; comments are set off-tempature to distingiush them better.
;; orange was chosen as a red that wasn't harsh.
(set-face-background 'font-lock-comment-face codermattie-bg-color)
(set-face-foreground 'font-lock-comment-face "orange3")

;; grammar is the lightest shade of blue
(set-face-background 'font-lock-keyword-face codermattie-bg-color)
(set-face-foreground 'font-lock-keyword-face "DeepSkyBlue4")

(set-face-background 'font-lock-builtin-face codermattie-bg-color)
(set-face-foreground 'font-lock-builtin-face "DeepSkyBlue4")

;; this should be for any form of literal value in code medium contrast.
(set-face-background 'font-lock-string-face codermattie-bg-color)
(set-face-foreground 'font-lock-string-face "grey50")

(set-face-background 'font-lock-constant-face codermattie-bg-color)
(set-face-foreground 'font-lock-constant-face "grey50")

(set-face-background 'font-lock-constant-face codermattie-bg-color)
(set-face-foreground 'font-lock-constant-face "grey50")

(set-face-background 'font-lock-warning-face codermattie-bg-color)
;;(set-face-attribute  'font-lock-warning-face :underline "red")
(set-face-foreground 'font-lock-warning-face "grey70")

;; decl is dark green

(set-face-background 'font-lock-type-face codermattie-bg-color)
(set-face-foreground 'font-lock-type-face "green4")

(set-face-background 'font-lock-function-name-face codermattie-bg-color)
(set-face-foreground 'font-lock-function-name-face "aquamarine4")

(set-face-background 'font-lock-variable-name-face codermattie-bg-color)
(set-face-foreground 'font-lock-variable-name-face "aquamarine3")

(defun display-faces-for-cperl ()
  (setq cperl-invalid-face nil)

  (set-face-background 'cperl-array-face codermattie-bg-color)
  (set-face-foreground 'cperl-array-face "aquamarine3")

  (set-face-background 'cperl-hash-face codermattie-bg-color)
  (set-face-foreground 'cperl-hash-face "aquamarine3")

  (set-face-background 'cperl-nonoverridable-face codermattie-bg-color)
  (set-face-foreground 'cperl-nonoverridable-face "DeepSkyBlue4") )

(defun display-faces-for-ediff ()
  (let
    ((diff-bg-color "AntiqueWhite4")
     (diff-bg-selected-color "CadetBlue")

     (diff-fg-color "grey35")

     (diff-fine-bg "SkyBlue4")
     (diff-fine-fg "grey65"))

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

    (set-face-background 'ediff-fine-diff-A diff-fine-bg)
    (set-face-foreground 'ediff-fine-diff-A diff-fine-fg)

    (set-face-background 'ediff-fine-diff-B diff-fine-bg)
    (set-face-foreground 'ediff-fine-diff-B diff-fine-fg)

    (set-face-background 'ediff-fine-diff-C diff-fine-bg)
    (set-face-foreground 'ediff-fine-diff-C diff-fine-fg) ))

(defun display-faces-for-whitespace-mode ()
  (set-face-background 'whitespace-tab "red")
  (set-face-foreground 'whitespace-tab "yellow")

  (set-face-attribute 'whitespace-tab nil :underline t)
  (set-face-attribute 'whitespace-tab nil :inverse-video nil)

  (set-face-background 'whitespace-trailing codermattie-bg-color)
  (set-face-foreground 'whitespace-trailing "yellow")

  (set-face-attribute 'whitespace-trailing nil :underline t)
  (set-face-attribute 'whitespace-trailing nil :inverse-video nil))

(defun display-faces-for-web-mode ()
  (set-face-background 'web-mode-current-element-highlight-face "grey20")

  ;; language syntax is the darkest shade of blue
  (set-face-foreground 'web-mode-doctype-face         "DeepSkyBlue4")
  (set-face-foreground 'web-mode-html-tag-face        "SkyBlue3")
  (set-face-foreground 'web-mode-html-attr-name-face  "aquamarine3")
  (set-face-foreground 'web-mode-html-attr-value-face "grey50") )

(defun display-faces-for-helm ()
  (set-face-background 'helm-source-header "black")
  (set-face-foreground 'helm-source-header "DeepSkyBlue4")

  (set-face-background 'helm-visible-mark "black")
  (set-face-foreground 'helm-visible-mark "orange1")

  (set-face-background 'helm-header "black")
  (set-face-foreground 'helm-header "LightSteelBlue")

  (set-face-background 'helm-candidate-number "black")
  (set-face-foreground 'helm-candidate-number "firebrick1")

  (set-face-background 'helm-selection "grey20")
  (set-face-foreground 'helm-selection "LightSteelBlue"))

(defun display-faces-for-paren ()
  (set-face-background 'paren-face-match "grey15"))

(defun display-faces-for-flyspell ()
  (set-face-attribute  'flyspell-incorrect nil :underline t)
  (set-face-foreground 'flyspell-incorrect "red"))

(defun display-faces-for-term ()
  (set-face-foreground 'term "DarkOrange2") )

(eval-after-load 'cperl-mode '(display-faces-for-cperl))
(eval-after-load 'ediff-mode '(display-faces-for-ediff))
(eval-after-load 'whitespace-mode '(display-faces-for-whitespace-mode))
(eval-after-load 'web-mode '(display-faces-for-web-mode))
(eval-after-load 'helm '(display-faces-for-helm))
(eval-after-load 'mic-paren '(display-faces-for-paren))
(eval-after-load 'flyspell '(display-faces-for-flyspell))
(eval-after-load 'term '(display-faces-for-term))

(provide 'user-display)
