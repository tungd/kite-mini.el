
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

(defvar km-connection-history nil)

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
    (-filter #'km-page-tab? tabs)))

(defun km-tab-completion (tab)
  (let ((title (plist-get tab :title))
        (url (plist-get tab :url)))
    (cons (format "%s" title) tab)))

(defun km-select-tab (host port)
  (let* ((tabs (mapcar #'km-tab-completion
                       (km-get-tabs host port)))
         (selection (completing-read
                     "Tab: " tabs
                     nil t "" 'km-connection-history (caar tabs)))
         (tab (cdr (assoc selection tabs))))
    (plist-get tab :webSocketDebuggerUrl)))

(defun km-connect ()
  (interactive)
  (let ((socket-url (km-select-tab km-remote-host km-remote-port)))
    socket-url))


(provide 'kite-mini)
