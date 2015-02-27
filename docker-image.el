;;; docker-image.el --- FIXME

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

(defclass docker-image ()
  ((id           :initarg :id           :initform nil)
   (parent-id    :initarg :parent-id    :initform nil)
   (repo-tags    :initarg :repo-tags    :initform [])
   (size         :initarg :size         :initform nil)
   (virtual-size :initarg :virtual-size :initform nil)
   (created      :initarg :created      :initform nil)))

(defmethod docker-image-pull (this docker-image)
  (shell-command "docker pull %s" (car (oref this :repo-tags))))

(defun docker-image-from-json (json)
  "Convert JSON to `docker-image'."
  (docker-image
   (plist-get json :Id)
   :id           (plist-get json :Id)
   :parent-id    (plist-get json :ParentId)
   :repo-tags    (plist-get json :RepoTags)
   :size         (plist-get json :Size)
   :virtual-size (plist-get json :VirtualSize)
   :created      (plist-get json :Created)))

(provide 'docker-image)

;;; docker-image.el ends here
