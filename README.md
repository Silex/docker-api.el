# Docker Remote API

Emacs interface to the [Docker Remote API](https://docs.docker.com/engine/reference/api/docker_remote_api).

## Examples

### List images

``` emacs-lisp
(--each (docker-api-images)
  (message "image: %s size: %d" (car (assoc-default 'RepoTags it)) (assoc-default 'Size it)))
```

### Different DOCKER_HOST

``` emacs-lisp
(docker-api-with-connection "http://myhost.com:1234"
  (docker-api-images))
```

### TRAMP

``` emacs-lisp
(let ((default-directory "/ssh:user@myhost.com:"))
  (docker-api-images))
```

## Utilities

### docker-api-http-request (method path)

  Make a docker HTTP request using method (`'get`, `'post` or `'delete`) at PATH (`/images/json`, etc).

### docker-api-json-request (method path)

  Same as [docker-api-http-request](#docker-api-http-request-method-path) but parses results as JSON.
