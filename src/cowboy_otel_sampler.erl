-module(cowboy_otel_sampler).

-behavior(otel_sampler).

-export([description/1]).
-export([setup/1]).
-export([should_sample/7]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_sampler.hrl").

setup(BinList) when is_binary(hd(BinList)) ->
    #{exclude => BinList};
setup(String) ->
    #{exclude => string:split(binary:list_to_bin(String), ",")}.

description(#{exclude := List}) ->
    iolist_to_binary(io_lib:format("Exclude spans from set ~p", [List])).

should_sample(_Ctx, _TraceId, _Links, Name, Kind, _Attrs, #{exclude := List}) ->
    case Kind == ?SPAN_KIND_SERVER andalso lists:member(Name, List) of
        true -> {?DROP, [], []};
        _ -> {?RECORD_AND_SAMPLE, [], []}
    end.
