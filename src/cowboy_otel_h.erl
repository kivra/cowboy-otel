%%% @doc A cowboy stream handler that extracts spans from http headers
%%% and starts and ends opentelemetry span.

-module(cowboy_otel_h).
-behavior(cowboy_stream).

-export([
    init/3,
    data/4,
    info/3,
    terminate/3,
    early_error/5
]).

-record(state, {
    next :: any(),
    span :: opentelemetry:span_ctx()
}).

-type state() :: #state{}.

-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

%%% @doc Start the opentelemetry span.
-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts()) ->
    {cowboy_stream:commands(), state()}.
init(StreamID, Req0, Opts) ->
    Headers = maps:get(headers, Req0),
    extract_otel(Headers),
    SpanCtx =
        case ?is_recording() of
            false ->
                maybe_start_span(Req0, Opts);
            true ->
                start_span(Req0)
        end,
    Ctx = otel_ctx:get_current(),
    Req = Req0#{otel_ctx => Ctx, otel_span_ctx => SpanCtx},
    {Commands, Next} = cowboy_stream:init(StreamID, Req, Opts),
    {Commands, #state{next = Next, span = SpanCtx}}.

%%% @doc Does nothing.
-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), State) ->
    {cowboy_stream:commands(), State}
when
    State :: state().
data(StreamID, IsFin, Data, State = #state{next = Next}) ->
    {Commands, Next0} = cowboy_stream:data(StreamID, IsFin, Data, Next),
    {Commands, State#state{next = Next0}}.

%%% @doc Intercept the response command, and then set the status code span attribute.
-spec info(cowboy_stream:streamid(), any(), State) ->
    {cowboy_stream:commands(), State}
when
    State :: state().
info(
    StreamID, {response, Code, _Headers, _Body} = Info, State = #state{next = Next, span = SpanCtx}
) when
    is_integer(Code)
->
    Attributes = #{
        <<"http.response.status_code">> => Code
    },
    otel_span:set_attributes(SpanCtx, Attributes),
    {Commands, Next0} = cowboy_stream:info(StreamID, Info, Next),
    {Commands, State#state{next = Next0}};
info(StreamID, Info, State = #state{next = Next}) ->
    {Commands, Next0} = cowboy_stream:info(StreamID, Info, Next),
    {Commands, State#state{next = Next0}}.

%%% @doc End the opentelemetry span.
-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), state()) -> any().
terminate(StreamID, Reason, #state{next = Next, span = SpanCtx}) ->
    Termination = cowboy_stream:terminate(StreamID, Reason, Next),
    otel_tracer:set_current_span(otel_span:end_span(SpanCtx, undefined)),
    Termination.

%%% @doc Does nothing.
-spec early_error(
    cowboy_stream:streamid(),
    cowboy_stream:reason(),
    cowboy_stream:partial_req(),
    Resp,
    cowboy:opts()
) ->
    Resp
when
    Resp :: cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, {_, _Status, _Headers, _} = Resp, Opts) ->
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

%% Extract and start opentelemetry span
extract_otel(Headers) ->
    Propagator = opentelemetry:get_text_map_extractor(),
    otel_propagator_text_map:extract(
        Propagator, Headers, fun list_header_names/1, fun get_header_value/2
    ).

get_header_value(HeaderName, Headers) ->
    maps:get(HeaderName, Headers, undefined).

list_header_names(Headers) ->
    maps:keys(Headers).

maybe_start_span(#{path := Path} = Req0, #{otel_exclude_paths := ExcludedPaths}) when
    is_list(ExcludedPaths)
->
    case lists:member(Path, ExcludedPaths) of
        true ->
            undefined;
        false ->
            start_span(Req0)
    end;
maybe_start_span(Req0, _Opts) ->
    start_span(Req0).

start_span(#{method := Method, path := Path} = Req0) ->
    #{headers := Headers, host := Host, port := Port, peer := Peer, scheme := Scheme} = Req0,
    {RemoteIP, _Port} = Peer,
    SpanOpts = #{
        kind => ?SPAN_KIND_SERVER,
        attributes =>
            #{
                <<"correlation_id">> => get_header_value(<<"x-correlation-id">>, Headers),
                <<"http.host">> => Host,
                <<"http.host.port">> => Port,
                <<"http.method">> => Method,
                <<"http.scheme">> => Scheme,
                <<"http.target">> => Path,
                <<"http.user_agent">> => get_header_value(<<"user-agent">>, Headers),
                <<"net.host.ip">> => iolist_to_binary(inet:ntoa(RemoteIP))
            }
    },
    SpanName = iolist_to_binary([Method, " ", Path]),
    ?start_span(SpanName, SpanOpts).
