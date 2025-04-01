%%% @doc A cowboy middleware that attach the cowboy_otel_h span ctx to itself (the request process).
-module(cowboy_otel_middleware).
-behaviour(cowboy_middleware).

%% Callbacks
-export([execute/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

%%% @doc Attach span ctx to request process. Also rename span and set http.route attribute if we have bindings.
-spec execute(Req, Env) -> {ok, Req, Env} when
    Req :: cowboy_req:req(),
    Env :: cowboy_middleware:env().
execute(#{otel_ctx := Ctx, otel_span_ctx := SpanCtx} = Req, Env) ->
    _ = otel_ctx:attach(Ctx),
    ?set_current_span(SpanCtx),
    case http_route(Req) of
        undefined ->
            ok;
        HttpRoute ->
            HttpMethod = http_method(Req),
            otel_span:set_attribute(SpanCtx, 'http.route', HttpRoute),
            NewName = iolist_to_binary([HttpMethod, " ", HttpRoute]),
            otel_span:update_name(SpanCtx, NewName)
    end,
    {ok, Req, Env};
execute(Req, Env) ->
    {ok, Req, Env}.

http_method(#{method := Method}) -> Method.

http_route(#{path := Path, bindings := Bindings}) ->
    RouteFun = fun
        (_, <<>>, Acc) -> Acc;
        (K, V, Acc) when is_binary(K) -> binary:replace(Acc, V, <<":", K/binary>>);
        (K, V, Acc) when is_atom(K) -> binary:replace(Acc, V, <<":", (atom_to_binary(K, utf8))/binary>>);
        (_, _, Acc) -> Acc
    end,
    maps:fold(RouteFun, Path, Bindings);
http_route(_) ->
    undefined.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test_routes() ->
    [
        {'_', [
            {<<"/metrics">>, no_args, #{}},
            {<<"/user/:uid/stuff">>, uid_args, #{}}
        ]}
    ].

http_route_test() ->
    Dispatch = cowboy_router:compile(test_routes()),
    Req0 = #{host => <<"example.com">>, path => <<"/user/123/stuff">>},
    Env0 = #{dispatch => Dispatch},
    {ok, Req, Env} = cowboy_router:execute(Req0, Env0),
    ?assertMatch(#{handler := uid_args}, Env),
    ?assertMatch(#{path := <<"/user/123/stuff">>}, Req),
    ?assertMatch(#{bindings := #{uid := <<"123">>}}, Req),
    ?assertEqual(<<"/user/:uid/stuff">>, http_route(Req)).

-endif.
