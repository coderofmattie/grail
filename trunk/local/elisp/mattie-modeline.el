;;----------------------------------------------------------------------
;; mattie-modeline
;;----------------------------------------------------------------------

(defface mattie-hacking-mode
   '((t :foreground "forest green" :background "grey15"))
   "face for when it is hacking time.")

(defvar mattie-hacking-warning (cons 20 7) "set the bedtime warning between 8P and 7A")
(defvar mattie-hacking-alert   (cons 22 7) "set the bedtime alert between 10P and 7A")

(defvar mattie-mode nil "default to no mode")

(defun mattie-modeline-modified ()
  "mattie-modeline-modified

   Construct a modified string for the modeline.
  "
  (concat
    (if (or (and (local-variable-p 'view-read-only) view-read-only)
            (and (local-variable-p 'buffer-read-only) buffer-read-only))
      "RO"
      "RW")
    "/"
    (cond
      ((not (verify-visited-file-modtime (current-buffer))) "!DSK")
       ((buffer-modified-p)  "!CHG")
       ((and buffer-file-name (recent-auto-save-p)) "!BKP")
       (t "-"))
      ))

(defun in-time-range ( start end )
  "determine if the current time is imbetween start and end"
  (let
    ((time   (decode-time))
     (hour   nil)
     (minute nil))

    (setq hour   (nth 2 time))
    (setq minute (nth 1 time))

    (when (or (>= hour start) (<= hour end)) t)))

(defun mattie-set-hacking-mode ( alert-start alert-end warning-start warning-end )
  (cond
    ((in-time-range alert-start   alert-end)
      (unless (equal 'alert mattie-mode)
        (set-face-attribute 'mattie-hacking-mode nil
          :foreground "firebrick4"
          :background "black"
          :box "yellow1")
        (setq mattie-mode 'alert)))

    ((in-time-range warning-start warning-end)
      (unless (equal 'warning mattie-mode)
        (set-face-attribute 'mattie-hacking-mode nil
          :foreground "firebrick4"
          :background "black"
          :box nil)
        (setq mattie-mode 'warning)))
    (t
      (unless (equal 'hacking mattie-mode)
        (set-face-attribute 'mattie-hacking-mode nil
          :foreground "forest green"
          :background "grey15"
          :box nil)
        (setq mattie-mode 'hacking))) ))

(defun mattie-update-hacking-mode ()
  "a hook function to update the hacking mode"
  (mattie-set-hacking-mode
    (car mattie-hacking-alert) (cdr mattie-hacking-alert)
    (car mattie-hacking-warning) (cdr mattie-hacking-warning)))

(defvar mattie-mode-line-format
  `("-"
;;    (:eval (make-char 'greek-iso8859-7 107))
    (:propertize display-time-string face mattie-hacking-mode)
    " "
    mode-line-buffer-identification
    ":"
    (line-number-mode "%l")
    ">"
    (column-number-mode "%c")
    " "
    "[" (:eval (mattie-modeline-modified)) vc-mode "]"
    " "
    "(" (:propertize mode-name face (:weight bold)) ":" minor-mode-alist ")"
    buffer-ring-modeline
    " ->" (which-func-mode ("" which-func-format)) ))

(defun setup-mattie-modeline ()
  (setq default-mode-line-format mattie-mode-line-format)

  (display-time)
  (add-hook 'display-time-hook 'mattie-update-hacking-mode))

(provide 'mattie-modeline)

;;  (force-mode-line-update)

vc-mode


