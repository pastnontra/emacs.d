(local-require 'quelpa)

;; (quelpa '(eaf (:fetcher github
;;                :repo  "manateelazycat/emacs-application-framework"
;;                :files ("*"))))
(quelpa
 '(eaf
   :fetcher git
   :url "https://gitclone.com/github.com/manateelazycat/emacs-application-framework.git"
   :files ("*")))
