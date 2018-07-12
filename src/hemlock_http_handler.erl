-module(hemlock_http_handler).

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

init(Req0, [{route, Route}] = State) ->
    Obj0 = #{
        headers => cowboy_req:headers(Req0),
        path => cowboy_req:path(Req0),
        method => cowboy_req:method(Req0),
        query => cowboy_req:parse_qs(Req0)
    },

    {Req1, Obj1, Status, Headers} = dispatch(Route, Req0, Obj0),

    Req2 = cowboy_req:reply(Status,
        Headers#{<<"content-type">> => <<"application/json">>},
        jsx:encode(Obj1),
        Req1),
    {ok, Req2, State}.

dispatch(method, Req0, Obj) ->
    Path = string:uppercase(cowboy_req:binding(method, Req0)),
    handle_method_dispatch(Path, Req0, Obj);

dispatch(redirect, Req0, Obj) ->
    case catch binary_to_integer(cowboy_req:binding(times, Req0)) of
        {'EXIT', {badarg, _}} ->
            {Req0, Obj, 400, #{}};
        1 ->
            {Req0, Obj, 302, #{ <<"location">> => <<"/get">> }};
        N ->
            Location = <<"/redirect/", (integer_to_binary(N - 1))/binary>>,
            {Req0, Obj, 302, #{ <<"location">> => Location }}
    end;

dispatch(timeout, #{ method := Method } = Req0, Obj) ->
    case catch binary_to_integer(cowboy_req:binding(seconds, Req0)) of
        {'EXIT', {badarg, _}} ->
            {Req0, Obj, 400, #{}};
        Timeout ->
            timer:sleep(Timeout * 1000),
            handle_method_dispatch(Method, Req0, Obj)
    end;

dispatch(auth, Req0, Obj) ->
    User = cowboy_req:binding(user, Req0),
    Password = cowboy_req:binding(password, Req0),
    case cowboy_req:parse_header(<<"authorization">>, Req0) of
        {basic, User, Password} ->
            {Req0, Obj, 200, #{}};
        _ ->
            {Req0, Obj, 401, #{}}
    end;

dispatch(status, Req0, Obj) ->
    case catch binary_to_integer(cowboy_req:binding(status_code, Req0)) of
        {'EXIT', {badarg, _}} ->
            {Req0, Obj, 400, #{}};
        Status ->
            {Req0, Obj, Status, #{}}
    end;

dispatch(_, Req0, Obj) ->
    {Req0, Obj, 404, #{}}.

handle_method_dispatch(Path, #{ method := Method } = Req0, Obj) ->
    case Path of
        Method when ?is_safe_http_method(Method) ->
            {Req0, Obj, 200, #{}};
        Method when ?is_unsafe_http_method(Method) ->
            case cowboy_req:parse_header(<<"content-type">>, Req0) of
                {<<"multipart">>, <<"form-data">>, _} ->
                    {Req0, Obj, 415, #{}};
                {<<"application">>, <<"x-www-form-urlencoded">>, _} ->
                    {ok, Form, Req1} = cowboy_req:read_urlencoded_body(Req0),
                    {Req1, Obj#{ form => Form }, 200, #{}};
                _Other ->
                    {ok, Data, Req1} = read_body(Req0),
                    {Req1, Obj#{ data => Data }, 200, #{}}
            end;
        Other when ?is_safe_http_method(Other) ->
            {Req0, Obj, 405, #{}};
        Other when ?is_unsafe_http_method(Other) ->
            {Req0, Obj, 405, #{}};
        _ ->
            {Req0, Obj, 404, #{}}
    end.

read_body(Req0) ->
    read_body(Req0, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} -> read_body(Req, <<Acc/binary, Data/binary>>)
    end.
