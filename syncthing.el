;;; syncthing.el --- Emacs client for Syncthing -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, syncthing, sync, client, view
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/KeyWeeUsr/emacs-syncthing

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'widget)
(require 'wid-edit)
(require 'cl-macs)
(require 'cl-lib)
(require 'url)

(defvar syncthing--servers nil)
(defvar-local syncthing-server nil)

(defgroup syncthing nil
  "Customization group for `syncthing'."
  :group 'external
  :group 'communication)

(defcustom syncthing-auto-revert-interval 10
  "Number of seconds to wait before reverting client buffer."
  :type 'number)

(cl-defstruct (syncthing-server (:copier nil) (:named nil)
                                (:constructor syncthing--server))
  "Syncthing server struct."
  name url token data)

;;@TODO: make async
(defun syncthing--request (method url token &rest data)
  "Return METHOD response from URL with TOKEN and DATA."
  (let ((url-request-method method)
        (url-request-data data)
        (url-request-extra-headers `(("X-Api-Key" . ,token))))
    ;;@TODO: handle error responses
    (with-temp-buffer (url-insert-file-contents url)
                      (json-parse-buffer :object-type 'alist
                                         :array-type 'list
                                         :null-object nil
                                         :false-object nil))))

(defun syncthing-request (server method endpoint &rest data)
  "Return SERVER response for METHOD at ENDPOINT for request with DATA."
  (apply #'syncthing--request
         (append (list method
                       (format "%s/%s" (syncthing-server-url server) endpoint)
                       (syncthing-server-token server))
                 data)))

(defun syncthing--server-update (server)
  "Update SERVER data."
  (cl-loop with data = (syncthing-request server "GET" "rest/config")
           initially do
           (setf (alist-get 'system-version data)
                 (syncthing-request server "GET" "rest/system/version")
                 (alist-get 'system-status data)
                 (syncthing-request server "GET" "rest/system/status"))
           with folders = (alist-get 'folders data)
           for i below (length folders)
           for folder = (nth i folders)
           for id = (alist-get 'id folder)
           do (setf (alist-get 'completion folder)
                    (syncthing-request server "GET"
                                       (concat "rest/db/completion?folder=" id))
                    (alist-get 'status folder)
                    (syncthing-request server "GET"
                                       (concat "rest/db/status?folder=" id))
                    (nth i folders) folder)
           (cl-loop with devices = (alist-get 'devices data)
                    for i below (length devices)
                    for device = (nth i devices)
                    for device-id = (alist-get 'deviceID device)
                    for endpoint = (format "rest/db/completion?device=%s&folder=" device-id)
                    do (setf (alist-get id (alist-get 'progress device) nil nil #'equal)
                             (syncthing-request server "GET" (concat endpoint id))
                             (nth i devices) device))
           finally return (setf (syncthing-server-data server) data)))

(defun syncthing-server (name url token)
  "Return server with NAME URL and TOKEN."
  (let ((server (syncthing--server :name name :url url :token token)))
    (syncthing--server-update server)
    server))

(defconst syncthing-gigibyte (expt 1024 3))
(defun syncthing--header-line (server)
  "Return SERVER `header-line-format' string."
  ;;@TODO: Make configurable, Add help text to icons, semantic faces.
  (let ((data (syncthing-server-data server)))
    (string-join
     (list
      ;;@FIX: Hardcoded?
      " 0B/s" " 0B/s"
      (format " %d"
              (cl-reduce #'+ (alist-get 'folders data)
                         :key (lambda (folder)
                                (alist-get 'localFiles (alist-get 'status folder) 0))))
      (format " %d"
              (cl-reduce #'+ (alist-get 'folders data)
                         :key (lambda (folder)
                                (alist-get 'localDirectories (alist-get 'status folder) 0))))
      (format " ~%.1fGiB"
              (/ (cl-reduce #'+ (alist-get 'folders data)
                            :key (lambda (folder)
                                   (alist-get 'localBytes (alist-get 'status folder) 0)))
                 syncthing-gigibyte))
      ;;@FIX: Hardcoded?
      " 3/3" " 4/5"
      "" (substring (alist-get 'myID (alist-get 'system-status data) "n/a") 0 6)
      "" (alist-get 'version (alist-get 'system-version data) "n/a"))
     " ")))

(defun syncthing--draw (server)
  "Draw client view of SERVER."
  (let ((inhibit-read-only t)) (erase-buffer))
  (widget-setup)
  (setq header-line-format (syncthing--header-line server)))

(defun syncthing-revert-buffer (&rest _)
  "Update server and redraw view."
  (unless syncthing-server (user-error "No Syncthing server associated with buffer"))
  (syncthing--server-update syncthing-server)
  (syncthing--draw syncthing-server))

(defvar syncthing-mode-map
  (let ((map (make-keymap))) map))

;; modes for client's session buffer(s)
(define-derived-mode syncthing-mode special-mode "Syncthing"
  "Major mode for Syncthing client.

\\{syncthing-mode-map}"
  (setq-local revert-buffer-function #'syncthing-revert-buffer))

(defun syncthing ()
  "@TODO."
  (interactive)
  (let* ((server (car (or syncthing--servers
                          (push (syncthing-server "Default Localhost"
                                                  "https://127.0.0.1:8384"
                                                  "nAe22LqECtwoDvToDb2hfd2tMMFotf5D")
                                syncthing--servers)))))
    (with-current-buffer (get-buffer-create
                          (format "*syncthing: %s*" (syncthing-server-name server)))
      (unless (derived-mode-p 'syncthing-mode) (syncthing-mode))
      (setq-local syncthing-server server)
      (put 'syncthing-server 'permanent-local t) ;;persist mode change
      (syncthing--draw server)
      (pop-to-buffer (current-buffer)
                     '((display-buffer-reuse-window display-buffer-same-window))))))

(define-minor-mode syncthing-auto-revert-mode
  "Refresh client view every `syncthing-auto-revert-interval' seconds."
  :lighter " syncthing-auto-revert"
  (unless (derived-mode-p 'syncthing-mode) (user-error "Buffer not in syncthing-mode"))
  (setq-local buffer-stale-function (when syncthing-auto-revert-mode #'always)
              auto-revert-interval
              (when syncthing-auto-revert-mode syncthing-auto-revert-interval))
  (auto-revert-mode (if syncthing-auto-revert-mode 1 -1)))

(provide 'syncthing)
;;; syncthing.el ends here
