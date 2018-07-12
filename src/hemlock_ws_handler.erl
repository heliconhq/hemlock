-module(hemlock_ws_handler).

-export([init/2]).
-export([terminate/3]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, _Opts) ->
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
        undefined ->
            {cowboy_websocket, Req, #{req => Req}};
        [Subprotocol|_Rest] ->
            Req1 = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,
                                              Subprotocol, Req),
            {cowboy_websocket, Req1, #{req => Req1}}
    end.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, #{req := Req0} = State) ->
    Obj = #{
        headers => cowboy_req:headers(Req0),
        path => cowboy_req:path(Req0),
        data => Msg,
        query => cowboy_req:parse_qs(Req0)
    },
    {reply, {text, jsx:encode(Obj)}, State};

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.
