
;; TODO: ELPA package format

(require 'url)
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

(defun km-encode (data)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-encode data)))

(defun km-decode (data)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-read-from-string data)))

(defun km-get-json (url)
  (let* ((url-request-method "GET")
         (url-http-attempt-keepalives nil)
         (json-array-type 'list)
         (json-object-type 'plist))
    (with-current-buffer (url-retrieve-synchronously url)
      (if (not (eq 200 (url-http-parse-response)))
          (error "Unable to connect to host.")
        (goto-char (+ 1 url-http-end-of-headers))
        (json-read)))))

(defun km-page-tab? (tab)
  (string-equal (plist-get tab :type) "page"))

(defun km-get-tabs (host port)
  (let* ((url (url-parse-make-urlobj
               "http" nil nil host port "/json"))
         (tabs (km-get-json url)))
    (-filter #'km-page-tab? tabs)
    tabs))

(defun km-connect (host port)
  (let ((tabs (km-get-tabs host port)))
    ))


(provide 'kite-mini)
