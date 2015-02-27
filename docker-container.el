;;; docker-container.el --- FIXME

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/docker-mode
;; Keywords: docker
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

(require 'json)
(require 'eieio)

(defclass docker-container ()
  ((id         :initarg :id           :initform nil)
   (status     :initarg :status       :initform nil)
   (ports      :initarg :ports        :initform [])
   (names      :initarg :names        :initform [])
   (image      :initarg :image        :initform nil)
   (command    :initarg :command      :initform nil)
   (created    :initarg :created      :initform nil)))

(defun docker-container-from-json (json)
  "Convert JSON to `docker-container'."
  (docker-container
   (plist-get json :Id)
   :id         (plist-get json :Id)
   :status     (plist-get json :Status)
   :ports      (plist-get json :Ports)
   :names      (plist-get json :Names)
   :image      (plist-get json :Image)
   :command    (plist-get json :Command)
   :cretaed    (plist-get json :Created)))

(provide 'docker-container)

;;; docker-container.el ends here
