-module(sampler_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

all() ->
    [sampled_spans].

init_per_suite(Config) ->
    application:load(opentelemetry),
    Sampler = {parent_based, #{root => {cowboy_otel_sampler, [<<"GET /exclude">>]}}},
    ExporterSpec = {otel_exporter_stdout, []},
    Processor = {otel_simple_processor, #{exporter => ExporterSpec}},
    application:set_env(opentelemetry, sampler, Sampler),
    application:set_env(opentelemetry, processors, [Processor]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    [
        {exporter, ExporterSpec}
        | Config
    ].

end_per_suite(_Config) ->
    _ = application:stop(opentelemetry),
    _ = application:unload(opentelemetry),
    ok.

init_per_testcase(sampled_spans, Config) ->
    Tid = ets:new(export_tab, [
        public,
        duplicate_bag,
        {keypos, #span.trace_id}
    ]),
    otel_simple_processor:set_exporter(otel_exporter_tab, Tid),
    %% sys:trace(otel_simple_processor_global, true),
    [{tid, Tid} | Config].

end_per_testcase(sampled_spans, Config) ->
    {Exporter, ExporterSpec} = ?config(exporter, Config),
    otel_simple_processor:set_exporter(Exporter, ExporterSpec),
    %% sys:trace(otel_simple_processor_global, false),
    Tid = ?config(tid, Config),
    ets:delete(Tid),
    ok.

sampled_spans(Config) ->
    Tid = ?config(tid, Config),

    SpanCtx1 = mock_http_server_span(<<"GET">>, <<"/pre/success">>),
    SpanCtx2 = mock_http_server_span(<<"GET">>, <<"/exclude">>),
    SpanCtx3 = mock_http_server_span(<<"GET">>, <<"/post/success">>),

    ?assertEqual(2 * 2, ets:info(Tid, size)),
    ?assert(ets:member(Tid, otel_span:trace_id(SpanCtx1))),
    ?assert(not ets:member(Tid, otel_span:trace_id(SpanCtx2))),
    ?assert(ets:member(Tid, otel_span:trace_id(SpanCtx3))),

    ok.

mock_http_server_span(Method, Route) ->
    SpanName = <<Method/binary, " ", Route/binary>>,
    SpanOpts =
        #{
            kind => server,
            attributes => #{
                <<"http.method">> => Method,
                <<"http.route">> => Route
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
