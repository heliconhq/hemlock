-module(hemlock_echo_handler).

-export([init/2]).

-define(is_unsafe_http_method(M),
        M =:= <<"POST">> orelse
        M =:= <<"DELETE">> orelse
        M =:= <<"PATCH">> orelse
        M =:= <<"PUT">> orelse
        M =:= <<"PUT">>).

-define(is_safe_http_method(M),
        M =:= <<"GET">> orelse
        M =:= <<"HEAD">> orelse
        M =:= <<"OPTIONS">>).

init(Req0, State) ->
    Obj0 = #{
        headers => cowboy_req:headers(Req0),
        path => cowboy_req:path(Req0),
        method => cowboy_req:method(Req0),
        query => cowboy_req:parse_qs(Req0)
    },

    Method = string:uppercase(cowboy_req:binding(method, Req0)),
    Arg = cowboy_req:binding(arg, Req0),

    {Req1, Obj1, Status, Headers} = dispatch(Method, Arg, Req0, Obj0),

    Req2 = cowboy_req:reply(Status,
        Headers#{<<"content-type">> => <<"application/json">>},
        jsx:encode(Obj1),
        Req1),
    {ok, Req2, State}.

dispatch(Method, _Arg, #{ method := Method } = Req0, Obj)
  when ?is_safe_http_method(Method) ->
    {Req0, Obj, 200, #{}};

dispatch(Method, _Arg, Req0, Obj) when ?is_safe_http_method(Method) ->
    {Req0, Obj, 405, #{}};

dispatch(Method, _Arg, #{ method := Method } = Req0, Obj)
  when ?is_unsafe_http_method(Method) ->
    {ok, Body, Req1} = read_body(Req0),
    {Req1, Obj#{ data => Body }, 200, #{}};

dispatch(Method, _Arg, Req0, Obj) when ?is_unsafe_http_method(Method) ->
    {Req0, Obj, 405, #{}};

dispatch(<<"REDIRECT">>, undefined, Req0, Obj) ->
    {Req0, Obj, 400, #{}};

dispatch(<<"REDIRECT">>, Arg, #{ method := <<"GET">> } = Req0, Obj) ->
    case catch binary_to_integer(Arg) of
        {'EXIT', {badarg, _}} ->
            {Req0, Obj, 400, #{}};
        0 ->
            {Req0, Obj, 302, #{ <<"location">> => <<"/get">> }};
        N ->
            Location = <<"/redirect/", (integer_to_binary(N - 1))/binary>>,
            {Req0, Obj, 302, #{ <<"location">> => Location }}
    end;

dispatch(<<"REDIRECT">>, _Arg, Req0, Obj) ->
    {Req0, Obj, 405, #{}};

dispatch(<<"TIMEOUT">>, undefined, Req0, Obj) ->
    {Req0, Obj, 400, #{}};

dispatch(<<"TIMEOUT">>, Arg, #{ method := Method } = Req0, Obj) ->
    case catch binary_to_integer(Arg) of
        {'EXIT', {badarg, _}} ->
            {Req0, Obj, 400, #{}};
        Timeout when ?is_safe_http_method(Method) ->
            timer:sleep(Timeout * 1000),
            dispatch(Method, Arg, Req0, Obj)
    end;

dispatch(_, _Arg, Req0, Obj) ->
    {Req0, Obj, 404, #{}}.

read_body(Req0) ->
    read_body(Req0, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} -> read_body(Req, <<Acc/binary, Data/binary>>)
    end.
