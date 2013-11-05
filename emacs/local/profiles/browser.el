;;----------------------------------------------------------------------
;; browser
;;
;; browser support required for documentation usually
;;----------------------------------------------------------------------

(grail-load 'eww (grail-define-installer "eww"
                  "file"
                  "http://bzr.savannah.gnu.org/lh/emacs/trunk/download/head:/eww.el-20130610114603-80ap3gwnw4x4m5ix-1/eww.el"))

(grail-load 'w3 (grail-define-installer "w3"
                  "pkg"
                  'w3))


