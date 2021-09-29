;; -*- coding: utf-8; lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")

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

;; Jupyter

;; (defcustom eaf-jupyter-keybinding
;;   `(,("C-h" ."eaf-send-key-sequence")
;;     ("C-w" . "eaf-send-key-sequence")
;;     ("C-m" . "")))



(provide 'init-eaf)
