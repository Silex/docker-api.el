;;; docker-connection.el --- FIXME

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/docker-connection
;; Keywords: filename, convenience
;; Version: 0.1.0
;; Package-Requires: ((s "1.9.0") (dash "1.5.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 's)
(require 'json)

(defgroup docker-connection nil
  ""
  :group 'files
  :group 'convenience)

(defcustom docker-connection-process-name "docker-http"
  "fixme"
  :group 'docker)

(defcustom docker-connection-process-buffer "*docker-http*"
  "fixme"
  :group 'docker)

(defcustom docker-connection-process-host nil
  "host (docker.host.com or nil)"
  :group 'docker)

(defcustom docker-connection-process-family 'local
  "family ('local or 'ipv4)"
  :group 'docker)

(defcustom docker-connection-process-service "/var/run/docker.sock"
  "service (docker.sock or 80)"
  :group 'docker)

(defvar docker-connection--http-data nil
  "fixme")

(defvar docker-connection--request-finished nil
  "fixme")

(defun docker-connection-process-filter(process string)
  "Append received data to `docker-connection--http-data'."
  (setq docker-connection--http-data (concat docker-connection--http-data string)))

(defun docker-connection-process-sentinel (process event)
  "Notify that all data was received with `docker-connection--request-finished'."
  (when (memq (process-status process) '(closed exit signal))
    (setq docker-connection--request-finished t)))

(defun docker-connection-process ()
  (or (get-buffer-process docker-connection-process-buffer)
      (make-network-process
       :name     docker-connection-process-name
       :buffer   docker-connection-process-buffer
       :family   docker-connection-process-family
       :host     docker-connection-process-host
       :service  docker-connection-process-service
       :filter   #'docker-connection-process-filter
       :sentinel #'docker-connection-process-sentinel)))

(defun docker-http-request (method path)
  (let ((request (format "%s %s HTTP/1.0\r\n\r\n" (upcase (symbol-name method)) path)))
    (setq docker-connection--http-data nil)
    (setq docker-connection--request-finished nil)
    (process-send-string (docker-connection-process) request)
    (while (not docker-connection--request-finished)
      (accept-process-output (docker-connection-process) 3))
    (let* ((index (s-index-of "\r\n\r\n" docker-connection--http-data))
           (headers (substring docker-connection--http-data 0 index))
           (data (substring docker-connection--http-data (+ index 4))))
      data)))

(defun docker-json-request (method path)
  (let* ((json (docker-http-request method path))
         (json-object-type 'plist))
    (json-read-from-string json)))

(provide 'docker-connection)

;;; docker-connection.el ends here
