-module(hemlock_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", hemlock_ws_handler, []},
            {"/:method/[:arg]", hemlock_echo_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 5000}],
        #{env => #{dispatch => Dispatch}}
    ),
    hemlock_sup:start_link().

stop(_State) ->
    ok.
