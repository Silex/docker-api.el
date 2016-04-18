# Docker Remote API

Emacs interface to the [Docker Remote API](https://docs.docker.com/engine/reference/api/docker_remote_api).

## Images

### docker-api-images (&optional all digests)

Returns the images list.

### docker-api-image-remove (id &optional force no-prune)

Removes image.

## Containers

### docker-api-containers
### docker-api-container-remove (id)

## Volumes

### docker-api-volumes
### docker-api-volume-remove (id)

## Networks

### docker-api-networks
### docker-api-network-remove (id)

## Utilities

### docker-api-http-request (method path)

  Make a docker HTTP request using method (`'get`, `'post` or `'delete`) at PATH (`/images/json`, etc).

### docker-api-json-request (method path)

  Same as [docker-api-http-request](#docker-api-http-request-method-path) but parses results as JSON.
