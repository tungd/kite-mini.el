
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

(defvar km-rpc-scripts nil
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

(defun km-register-callback (id fn)
  (let ((hook (intern (number-to-string id) km-rpc-callbacks)))
    (add-hook hook fn t)))

(defun km-dispatch-callback (id data)
  (let ((hook (intern (number-to-string id) km-rpc-callbacks)))
    (when hook
      (run-hook-with-args hook data)
      (unintern hook km-rpc-callbacks))))

(defun km-on-open (socket)
  (message "Kite: connected."))

(defun km--script-parsed (data)
  (let ((extension? (plist-get data :isContentScript))
        (url (plist-get data :url))
        (id (plist-get data :scriptId)))
    (unless extension?
      (add-to-list 'km-rpc-scripts (list :id id :url url)))))

(defun km--message-added (data)
  "We got `source', `level', `text' and the possition that trigger it."
  (let* ((message (plist-get data :message))
         (type (plist-get message :type))
         (text (plist-get message :text)))
    (message "Kite: [%s] %s" type text)))

(defun km--callback (id result)
  )

(defun km-on-message (socket data)
  (let* ((data (km-decode (websocket-frame-payload data)))
         (method (plist-get data :method))
         (params (plist-get data :params)))
    (cond
     ((string-equal method "Debugger.scriptParsed")
      (km--script-parsed params))
     ((string-equal method "Console.messageAdded")
      (km--message-added params))
     ;; TODO: do something usefull here, possibly great for REPL
     ((string-equal method "Console.messageRepeatCountUpdated"))
     ;; These are return messages from RPC calls, not notification
     ((not method)
      (km-dispatch-callback (plist-get data :id) (plist-get data :result)))
     ;; Generic fallback, only used in development
     (t (message "Kite: %s" data)))))

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
                  :on-message #'km-on-message
                  :on-close #'km-on-close))

(defun km-connect ()
  (interactive)
  (km-disconnect)
  (let* ((socket-url (km-select-tab km-remote-host
                                    km-remote-port)))
    (setq km-socket (km-open-socket socket-url))
    (km-call-rpc "Console.enable")
    (km-call-rpc "Debugger.enable")
    (km-call-rpc "Network.setCacheDisabled" '(:cacheDisabled t))))

(defun km-disconnect ()
  (interactive)
  (when (websocket-openp km-socket)
    (websocket-close km-socket)
    (setq km-socket nil
          km-rpc-scripts nil)))

;; Runtime.getProperties

(defun km-send-eval (code)
  (km-call-rpc
   "Runtime.evaluate"
   (list :expression code
         :returnByValue t)))

(defun km-remove-script (id)
  ())

(defun km-script-id (file)
  (let ((result nil)
        (name (file-name-base file)))
    (dolist (script km-rpc-scripts result)
      (let ((id (plist-get script :id))
            (url (plist-get script :url)))
        (when (string-equal name (file-name-base url))
          (if (not (km-script-exists? id))
            (km-remove-script id)
            (setq id (plist-get script :id))))))))

(defun km-update ()
  (interactive)
  (let ((id (km-script-id (buffer-file-name)))
        (source (buffer-substring-no-properties
                 (point-min) (point-max))))
    (if id
        (km-call-rpc
         "Debugger.setScriptSource"
         (list :scriptId id :scriptSource source))
      (message "No matching script for current buffer."))))

(defun km-reload ()
  (interactive)
  (km-call-rpc
   "Page.reload"
   (list :ignoreCache t)))

(defun km-evaluate-region-or-line (&optional args)
  (interactive "*P")
  (let ((start (if (region-active-p)
                   (region-beginning)
                 (line-beginning-position)))
        (end (if (region-active-p)
                 (region-end)
               (line-end-position))))
    (km-send-eval (buffer-substring-no-properties start end))))


(provide 'kite-mini)
