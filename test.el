(add-to-list 'load-path default-directory)

(require 'docker-image)
(require 'docker-container)
(require 'docker-connection)

;; (docker-json-request 'get "/containers/json?all=1")
;; (docker-json-request 'get "/images/json")

(let* ((images-json (docker-json-request 'get "/images/json"))
       (images-eieio (-map #'docker-image-from-json images-json)))
  (--each images-eieio (message "%s" (oref it :repo-tags))))
