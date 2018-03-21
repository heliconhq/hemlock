# hemlock

Simple HTTP and WS test server suitable for embedding in your test harness.

## Common test example

Install `hemlock` in your test profile (`rebar.conf`, assuming `rebar3`):

    {profiles, [
        {test, [
            {deps, [
                {hemlock, {git, "https://github.com/trelltech/hemlock.git"}}
            ]}
        ]}
    ]}.

Start hemlock with your suite (`your_SUITE.erl`):

    init_per_suite(Config) ->
        application:ensure_all_started(taser),
        application:ensure_all_started(hemlock),
        Config.

    end_per_suite(_Config) ->
        application:stop(hemlock),
        ok.

    my_test(_Config) ->
        {ok, 200, _Headers, _Body} = taser:get("http://127.0.0.1:5000/get"),
        ok.

## Supported HTTP endpoints

### `/get`

Returns JSON encoded information about the request.

### `/head`

Returns JSON encoded information about the request.

### `/options`

Returns JSON encoded information about the request.

### `/post`

Returns JSON encoded information about the request.

### `/put`

Returns JSON encoded information about the request.

### `/patch`

Returns JSON encoded information about the request.

### `/delete`

Returns JSON encoded information about the request.

### `/timeout/<seconds>`

Waits `seconds` seconds and returns JSON encoded information about the request.

### `/redirect/<times>`

Redirects (relative 302) client `times` times and returns JSON encoded
information about the request.

### `/auth/<user>/<password>`

Performs basic auth with `user` and `password` as credentials and returns JSON
encoded information about the request.

## WS endpoints

### `/ws`

Echoes back what is sent and some additional data in a JSON encoded frame.
