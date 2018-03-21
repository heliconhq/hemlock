-module(hemlock_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", hemlock_ws_handler, []},
            {"/timeout/:seconds", hemlock_http_handler, [{route, timeout}]},
            {"/redirect/:times", hemlock_http_handler, [{route, redirect}]},
            {"/:method", hemlock_http_handler, [{route, method}]}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(hemlock_http_listener,
        [{port, 5000}],
        #{env => #{dispatch => Dispatch}}
    ),
    hemlock_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(hemlock_http_listener).
