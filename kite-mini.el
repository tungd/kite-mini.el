;;; kite-mini.el --- remote evaluate JavaScript in  WebKit debugger
;;
;; Copyright (c) 2014  Tung Dao <me@tungdao.com>
;;
;; Author: Tung Dao <me@tungdao.com>
;; URL: https://github.com/tungd/kite-mini.el
;; Keywords: webkit
;; Version: 0.1.0
;; Package-Requires: ((dash "1.5.0") (websocket "1.2"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Minor mode for remote evaluate JavaScript in WebKit debugger, with
;; a little icing. Included features are:
;; - Evaluate JavaScript (running at top level)
;; - Modify and update external JavaScript files (live)
;; - Reload
;;
;; Planned features includes:
;; - Live reload stylesheets (without reload)
;; - JavaScript console (REPL)
;;
;;; Code:

(require 'url)
(require 'json)
(require 'dash)
(require 'websocket)


(defcustom km-remote-host "127.0.0.1"
  "Default host for connection to WebKit remote debugging API."
  :group 'kite-mini)

(defcustom km-remote-port 9222
  "Default port for connection to WebKit remote debugging API."
  :group 'kite-mini)

(defvar km-socket nil
  "Websocket connection to WebKit remote debugging API.")

(defvar km-rpc-id 0)
(defvar km-rpc-callbacks nil)
(defvar km-rpc-scripts nil
  "List of JavaScript files available for live editing.")


(defun km-encode (data)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-encode data)))

(defun km-decode (data)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-read-from-string data)))

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

(defun km-on-close (socket)
  (message "Kite: disconnected."))

(defun km-on-script-parsed (data)
  (let ((extension? (plist-get data :isContentScript))
        (url (plist-get data :url))
        (id (plist-get data :scriptId)))
    (when (and url (not extension?))
      (add-to-list 'km-rpc-scripts (list :id id :url url)))))

(defun km-on-message-added (data)
  "We got `source', `level', `text' and the possition that trigger it."
  (let* ((message (plist-get data :message))
         (type (plist-get message :type))
         (text (plist-get message :text)))
    (message "Kite: [%s] %s" type text)))

(defun km-on-message (socket data)
  (let* ((data (km-decode (websocket-frame-payload data)))
         (method (plist-get data :method))
         (params (plist-get data :params)))
    (cond
     ((string-equal method "Debugger.scriptParsed")
      (km-on-script-parsed params))
     ((string-equal method "Console.messageAdded")
      (km-on-message-added params))
     ;; TODO: do something usefull here, possibly great for REPL
     ((string-equal method "Console.messageRepeatCountUpdated"))
     ;; These are return messages from RPC calls, not notification
     ((not method)
      (km-dispatch-callback (plist-get data :id) (plist-get data :result)))
     ;; Generic fallback, only used in development
     (t (message "Kite: %s" data)))))

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


(defun km-send-eval (code)
  (km-call-rpc
   "Runtime.evaluate"
   (list :expression code
         :returnByValue t)))

(defun km-remove-script (script)
  (setq km-rpc-scripts
        (delete script km-rpc-scripts)))

(defun km-script-id (file)
  (let ((result nil)
        (name (file-name-base file)))
    (dolist (script km-rpc-scripts result)
      (let ((id (plist-get script :id))
            (url (plist-get script :url)))
        (when (string-equal name (file-name-base url))
          (if (not (km-script-exists? id))
            (km-remove-script script)
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


(defvar kite-mini-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-x C-e") #'km-evaluate-region-or-line)
      (define-key map (kbd "C-x C-u") #'km-update)
      (define-key map (kbd "C-x C-r") #'km-reload)))
  "Keymap for Kite Mini mode.")

;;;###autoload
(defun turn-on-kite-mini-mode ()
  "Turn on Kite Mini mode.")

;;;###autoload
(defun turn-off-kite-mini-mode ()
  "Turn off Kite Mini mode.")

;;;###autoload
(define-minor-mode kite-mini-mode
  "Minor mode for interact with WebKit remote debugging API."
  :global nil
  :group 'kite-mini
  :init-value nil
  :lighter ""
  :keymap kite-mini-mode-map
  (if kite-mini-mode
      (turn-on-kite-mini-mode)
    (turn-off-kite-mini-mode)))

(provide 'kite-mini)
;;; kite-mini.el ends here
