(local-require 'quelpa)

;; Speed up
(setq quelpa-self-upgrade-p nil)
(setq quelpa-checkout-melpa-p nil)

(quelpa
 '(eaf
   :fetcher git
   :url "https://gitclone.com/github.com/manateelazycat/emacs-application-framework.git"
   :files ("*")))

(quelpa
 '(awesome-pair
   :fetcher git
   :url "https://github.com/manateelazycat/awesome-pair"
   :files ("*")))

(provide 'init-package)
