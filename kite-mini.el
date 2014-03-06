
;; TODO: ELPA package format

(require 'json)
(require 'websocket)

(defcustom km-remote-host "127.0.0.1"
  "FIXME: "
  :group 'kite-mini)

(defcustom km-remote-port 9222
  "FIXME: "
  :group 'kite-mini)

(defvar km-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'km-eval)
    (define-key map (kbd "C-x C-u") #'km-update)
    (define-key map (kbd "C-x C-r") #'km-reload))
  "FIXME: ")

(provide 'kite-mini)
