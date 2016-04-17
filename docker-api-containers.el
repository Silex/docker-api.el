;;; docker-api-containers.el --- Emacs interface to the Docker API

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/docker-api.el

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

(require 'docker-api-connection)

;; TODO add parameters: limit since before filters
(defun docker-api-containers (&optional all size)
  (docker-api-json-request 'get (format "/containers/json?all=%d&size=%d" (if all 1 0) (if size 1 0))))

(defun docker-api-container-remove (id &optional force remove-volumes)
  (docker-api-json-request 'delete (format "/containers/%s?force=%d&v=%d" id (if force 1 0) (if remove-volumes 1 0))))

(provide 'docker-api-containers)

;;; docker-api-containers.el ends here
