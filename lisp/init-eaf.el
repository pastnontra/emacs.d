;; -*- coding: utf-8; lexical-binding: t; -*-

(unless *is-a-mac*
  (require 'eaf))

;; ;; Jupyter
;; (defcustom eaf-jupyter-keybinding
;;   '(("M-x" . "eaf-send-key-sequence")
;;     ("C-w" . "eaf-send-key-sequence")
;;     ("C-m" . "")
;;     )
;;   "Additions to default keybindings of EAF Jupyter."
;;   :type 'cons)

(provide 'init-eaf)