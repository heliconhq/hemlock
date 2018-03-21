# hemlock

Simple HTTP and WS test server.

## Supported HTTP endpoints

### /get

Returns JSON encoded information about the request.

### /head

Returns JSON encoded information about the request.

### /options

Returns JSON encoded information about the request.

### /post

Returns JSON encoded information about the request.

### /put

Returns JSON encoded information about the request.

### /patch

Returns JSON encoded information about the request.

### /delete

Returns JSON encoded information about the request.

### /timeout/n

Waits `n` seconds and returns JSON encoded information about the request.

### /redirect/n

Redirect (relative 302) `n` times and returns JSON encoded information about
the request.

## WS endpoints

### /ws

Echoes back what is sent and some additional data in a JSON encoded frame.
