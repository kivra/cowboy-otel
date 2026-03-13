-module(sampler_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

all() ->
    [{group, sampler}].

groups() ->
    [{sampler, [], [sampled_spans]}].

init_per_suite(Config) ->
    application:load(opentelemetry),
    Config.

end_per_suite(_Config) ->
    _ = application:unload(opentelemetry),
    ok.

init_per_group(sampler, Config) ->
    application:set_env(opentelemetry, span_processors, otel_simple_processor),
    application:set_env(opentelemetry, traces_exporter, {otel_exporter_stdout, []}),
    CowboySampler = {cowboy_otel_sampler, #{'http.route' => <<"^(/metrics|/heartbeat)$">>}},
    Sampler = {parent_based, #{root => CowboySampler}},
    application:set_env(opentelemetry, sampler, Sampler),
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config.

end_per_group(sampler, _Config) ->
    _ = application:stop(opentelemetry),
    ok.

init_per_testcase(sampled_spans, Config) ->
    ct:pal("All registerd processes ~p", [erlang:registered()]),
    %%sys:trace(otel_simple_processor_global, true),
    Tid = ets:new(export_tab, [
        public,
        duplicate_bag,
        {keypos, #span.trace_id}
    ]),
    otel_simple_processor:set_exporter(otel_exporter_tab, Tid), 
    [{tid, Tid} | Config].

end_per_testcase(sampled_spans, Config) ->
    Tid = ?config(tid, Config),
    ct:pal("In sampled table ~n~p", [ets:tab2list(Tid)]),
    ets:delete(Tid),
    sys:trace(otel_simple_processor_global, false),
    ok.

sampled_spans(Config) ->
    Tid = ?config(tid, Config),

    SpanCtx1 = mock_http_server_span(<<"GET">>, <<"/pre/unfiltered">>),
    SpanCtxGone1 = mock_http_server_span(<<"GET">>, <<"/metrics">>),
    SpanCtxGone2 = mock_http_server_span(<<"GET">>, <<"/heartbeat">>),
    SpanCtx2 = mock_http_server_span(<<"GET">>, <<"/post/unfiltered">>),

    ?assertEqual(2 * 2, ets:info(Tid, size)),
    ?assert(ets:member(Tid, otel_span:trace_id(SpanCtx1))),
    ?assert(not ets:member(Tid, otel_span:trace_id(SpanCtxGone1))),
    ?assert(not ets:member(Tid, otel_span:trace_id(SpanCtxGone2))),
    ?assert(ets:member(Tid, otel_span:trace_id(SpanCtx2))),

    ok.

mock_http_server_span(Method, Route) ->
    SpanName = <<Method/binary, " ", Route/binary>>,
    SpanOpts =
        #{
            kind => server,
            attributes => #{
                'http.method' => Method,
                'http.route' => Route
            }
        },
    ?with_span(
        SpanName,
        SpanOpts,
        fun mock_server_handler/1
    ).

mock_server_handler(SpanCtx) ->
    ?with_span(
        <<"subspan">>,
        #{},
        fun(_SpanCtx1) ->
            ?set_attributes([{<<"subspan">>, true}]),
            ok
        end
    ),
    ?set_attributes([{<<"http.response.status_code">>, 200}]),
    SpanCtx.

spanReceive(SpanName) ->
    receive
        {span, Span} when Span#span.name == SpanName ->
            Span
    after 1000 ->
        ct:fail("Did not receive the span after 1s")
    end.
