;;----------------------------------------------------------------------
;; scheme support
;;----------------------------------------------------------------------

(require 'scheme)
(require 'cmuscheme)

(defvar scheme-scratch-buffer "*eval-scheme*" "the name of the scheme scratch buffer")
(defvar default-scheme-interpreter "mzscheme")

(grail-load 'quack (grail-define-installer "quack"
                    "file"
                    "http://www.neilvandyke.org/quack/quack.el"))

(setq-default
  quack-dir (grail-garuntee-dir-path (concat grail-state-path "quack/"))
  quack-default-program default-scheme-interpreter

  ;; don't always prompt for the scheme interpreter. Use the default.
  quack-run-scheme-always-prompts-p nil

  ;; don't use customize to save settings.
  quack-options-persist-p nil

  ;; fontify using Emacs faces, don't use a drscheme theme clone.
  quack-fontify-style 'emacs

  ;; tabs are evil
  quack-tabs-are-evil-p t)

;; quack adds all the typical extensions when loaded, so only add non-standard
;; ones.
(setq
  auto-mode-alist (append '(("\\.scheme$"    . scheme-mode)) auto-mode-alist ))

;;----------------------------------------------------------------------
;; scheme
;;----------------------------------------------------------------------

;; the atom definition is tweaked for regex purpose. Without including
;; the list symbols the regex would run over lists in it's quest for
;; whitespace.

(defconst scheme-regex-whitespace "[ \n\t]" "whitespace in scheme")
(defconst scheme-regex-atom "[^ \n\t()]+" "whitespace in scheme")

(defconst scheme-function-regex (concat
                                  "("
                                  scheme-regex-whitespace "*"
                                  "define"
                                  scheme-regex-whitespace "+"
                                  "("
                                  scheme-regex-whitespace "*"
                                  scheme-regex-atom
                                  scheme-regex-whitespace "+"))

(defun scheme-list-fn-signatures ()
  (interactive)
  (occur scheme-function-regex))

;; the actual scheme-mode comes from scheme. the cmuscheme, and quack are layered on top
;; of scheme mode. scheme can get loaded from an auto-mode-alist hit, so make sure that
;; when scheme mode is loaded, that cmuscheme gets layered on too.

(defun scheme ()
  (interactive)

  (split-window-to-buffers
    (or (get-buffer scheme-scratch-buffer)
      (let
        ((scheme-eval-buffer (get-buffer-create scheme-scratch-buffer)))

        (with-current-buffer scheme-eval-buffer
          (scheme-mode)
          (setq scheme-buffer "*scheme*"))
        scheme-eval-buffer))

    (or (get-buffer "*scheme*")
      (save-excursion
        (condition-case nil
          ;; try and switch to the scheme buffer moving the point to the end of the buffer.
          (switch-to-scheme t)
          (current-buffer)
          (error
            (let
              ((pop-up-windows nil))
              (run-scheme default-scheme-interpreter)
              (current-buffer)))))) ))

(defun scheme-send-buffer ()
  (interactive)
  "send the buffer to the scheme interpreter"
  (save-excursion
    (mark-whole-buffer)
    (scheme-send-region)))

(add-hook 'inferior-scheme-mode-hook
  (lambda ()
    (configure-for-programming 'scheme-list-fn-signatures "scheme")
    (lisp-smart-parens-editing) )
  t)

(add-hook 'scheme-mode-hook
  (lambda ()
    (lisp-smart-parens-editing)

    (configure-for-programming 'scheme-list-fn-signatures "scheme")

    (configure-for-evaluation
      'scheme-send-definition
      'scheme-send-last-sexp
      'scheme-send-region
      'scheme-send-buffer))
  t)


