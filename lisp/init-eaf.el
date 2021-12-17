;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :init
  (when *wsl* (setq ip_address
                    (shell-command-to-string "grep nameserver /etc/resolv.conf | awk '{printf $2}'")))
  (when *win64* (setq eaf-python-command "python.exe"))
  (setq eaf-proxy-type "http")
  (setq eaf-proxy-host ip_address)
  ;; (setq eaf-proxy-host "172.27.224.1")
  (setq eaf-proxy-port "8889")
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  (eaf-kill-process-after-last-buffer-closed t)
  :config
  (defalias 'browse-web #'eaf-open-browser)
  (require 'eaf)
  (require 'eaf-terminal)
  (require 'eaf-markdown-previewer)
  (require 'eaf-music-player)
  (require 'eaf-pdf-viewer)
  (require 'eaf-file-browser)
  (require 'eaf-netease-cloud-music)
  (require 'eaf-jupyter)
  (require 'eaf-browser)
  (require 'eaf-js-video-player)
  (require 'eaf-image-viewer)
  (require 'eaf-file-manager)
  (require 'eaf-video-player)
  (require 'eaf-file-sender)
  (require 'eaf-org-previewer)
  ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  ;;(eaf-bind-key nil "M-q" eaf-browser-keybinding);; unbind, see more in the Wiki
  )



;; Jupyter

;; (defcustom eaf-jupyter-keybinding
;;   `(,("C-h" ."eaf-send-key-sequence")
;;     ("C-w" . "eaf-send-key-sequence")
;;     ("C-m" . "")))

(provide 'init-eaf)
