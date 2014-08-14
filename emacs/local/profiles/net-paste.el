;;----------------------------------------------------------------------
;; net-paste.el - various pastebin services
;;
;; description:
;;
;; interface to netpaste services
;;----------------------------------------------------------------------
(grail-load 'dpaste (grail-define-installer "dpaste"
                     "git"
                     "https://github.com/gregnewman/dpaste.el.git"))


(grail-load 'pastebin (grail-define-installer "elpastebin"
                         "git"
                         "https://github.com/nicferrier/elpastebin.git"))

;; title construction

(defconst npaste-default-name (concat user-login-name "@"system-name))

(defun npaste-default-title ()
  (concat npaste-default-name "/"
          (format-time-string "%a(%H:%M:%S)" (current-time))) )

;; pastebin

(defun npaste-pbin-domain ()
  (let
    ((sub-domain (read-string "domain? ")))

    (setq sub-domain (if (> (length sub-domain) 0)
                       `(,sub-domain)
                       '()))
    sub-domain))

(defun npaste-pbin-region ()
  (interactive)
  (apply 'pastebin (mark) (point) (npaste-pbin-domain)) )

(defun npaste-pbin-buffer ()
  (interactive)
  (pastebin-buffer (npaste-pbin-domain)) )

;; dpaste

(defun npaste-dpaste-title ()
  (interactive)
  (let
    ((paste-title (read-string "title? ")))

    (if (< (length paste-title) 1)
      (setq paste-title (npaste-default-title)) )

    paste-title) )

(defun npaste-dpaste-region ()
  (interactive)
  (dpaste-region (mark) (point) (npaste-dpaste-title)))

(defun npaste-dpaste-buffer ()
  (interactive)
  (dpaste-buffer (npaste-dpaste-title)))


;; lookup tables

(defconst npaste-region-table
  `(("pastebin" . npaste-pbin-region)
    ("dpaste" . npaste-dpaste-region) ))

(defconst npaste-buffer-table
  `(("pastebin" . npaste-pbin-buffer)
    ("dpaste" . npaste-dpaste-buffer) ))

(defun npaste-lookup-bin ( name bin )
  (let
    (( lookup (assoc name bin) ))

    (if lookup
      (setq lookup (cdr lookup)) )

    lookup))

;; table names

(defconst npaste-bins '("pastebin" "dpaste"))

;; npaste commands

(defun npaste-region ()
  (interactive)
  (let
    ((destination (npaste-lookup-bin (ido-completing-read "region->to: " npaste-bins)) ))

    (if (commandp destiation)
      (call-interactively destination)
      (message "could not find the pastebin") ) ))

(defun npaste-buffer ()
  (interactive)
  (let
    ((destination (npaste-lookup-bin (ido-completing-read "buffer->to: "
                                      npaste-bins)
                   npaste-buffer-table) ))

    (if (commandp destination)
      (call-interactively destination )
      (message "could not find the pastebin") ) ))

(provide 'grail/paste)
