
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
    (define-key map (kbd "C-x C-e") #'km-evaluate-region-or-line)
    (define-key map (kbd "C-x C-u") #'km-update)
    (define-key map (kbd "C-x C-r") #'km-reload))
  "FIXME: ")

(defvar km-socket nil
  "FIXME: ")

(defvar km-rpc-id 0
  "FIXME: ")

(defvar km-rpc-callbacks nil
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


(defun km-next-rpc-id ()
  (setq km-rpc-id (+ 1 km-rpc-id)))


(defun km-on-open (socket)
  (message "Kite: connected."))

(defun km-on-message (socket data)
  (message "Kite: %s"  data))

(defun km-on-close (socket)
  (message "Kite: disconnected."))

(defun km-get-tabs (host port)
  (let* ((url (url-parse-make-urlobj
               "http" nil nil host port "/json"))
         (tabs (km-get-json url)))
    (-filter (lambda (tab)
               (and (plist-get tab :webSocketDebuggerUrl)
                    (string-equal (plist-get tab :type) "page")))
             tabs)))

(defun km-tab-completion (tab)
  (let ((title (plist-get tab :title))
        (url (plist-get tab :url)))
    (cons (format "%s" title) tab)))

(defun km-select-tab (host port)
  (let* ((tabs (mapcar #'km-tab-completion
                       (km-get-tabs host port)))
         (selection (completing-read
                     "Tab: " tabs nil t "" nil (caar tabs)))
         (tab (cdr (assoc selection tabs))))
    (plist-get tab :webSocketDebuggerUrl)))

(defun km-call-rpc (method &optional params)
  (websocket-send-text
   km-socket
   (km-encode (list :id (km-next-rpc-id)
                    :method method
                    :params params))))

(defun km-open-socket (url)
  (websocket-open socket-url
                  :on-open #'km-on-open
                  :on-message #'km-on-message))

(defun km-connect ()
  (interactive)
  (let* ((socket-url (km-select-tab km-remote-host
                                    km-remote-port)))
    (setq km-socket (km-open-socket socket-url))
    (km-call-rpc "Console.enable")
    (km-call-rpc "Debugger.enable")
    (km-call-rpc "Network.setCacheDisabled" '(:cacheDisabled t))))

;; Runtime.getProperties

(defun km-send-eval (code)
  (km-call-rpc
   "Runtime.evalue"
   (list :expression code
         :returnByValue t)))

(defun km-update ()
  (km-call-rpc
   "Debugger.setScriptSource"
   (list :scriptId id
         :scriptSource code)))

(defun km-reload ()
  (km-call-rpc
   "Page.reload"
   (list :ignoreCache t)))

(defun km-evaluate-region-or-line ()
  (interactive "*P")
  (let ((start (if (region-active-p)
                   (region-beginning)
                 (line-beginning-position)))
        (end (if (region-active-p)
                 (region-end)
               (line-end-position))))
    (km-evaluate (buffer-substring-no-properties start end))))


(provide 'kite-mini)
