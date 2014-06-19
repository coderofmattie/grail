;;---------------------------------------------------------------------
;; search-paths
;;----------------------------------------------------------------------
(require 'search-trees)

(let
  ((disney-base  "/home/mattie/src/work/hg/"))

  (search-trees-add-for-host "khan" "python-mode"
    '("common"         (concat disney-base "disney_common"))
    '("django"         (concat disney-base "disney_django"))
    '("dnsrequest"     (concat disney-base "disney_dnsrequest"))
    '("dnstool"        (concat disney-base "disney_dnstool"))
    '("f5sync"         (concat disney-base "disney_f5sync"))
    '("maas"           (concat disney-base "disney_maas"))
    '("counters"       (concat disney-base "disney_monitorable_counters"))
    '("orion"          (concat disney-base "disney_orion_api"))
    '("python"         (concat disney-base "disney_python"))
    '("rest"           (concat disney-base "disney_rest"))
    '("restv1"         (concat disney-base "disney_restv1"))
    '("sdapi"          (concat disney-base "disney_sdapi"))
    '("sdapi_queue"    (concat disney-base "disney_sdapi_queue"))
    '("eat-f5"         (concat disney-base "eat-f5"))
    '("eat-f5-restapi" (concat disney-base "eat-f5-restapi")) ) )

(search-trees-add-for-user "mattie" "emacs-lisp-mode"
  '("emacs" "/home/codermattie/config/emacs/local"))

(provide 'search-paths)
