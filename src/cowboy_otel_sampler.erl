-module(cowboy_otel_sampler).

-behavior(otel_sampler).

-export([description/1]).
-export([setup/1]).
-export([should_sample/7]).

-include_lib("opentelemetry/include/otel_sampler.hrl").

setup(BinList) when is_binary(hd(BinList)) ->
    #{exclude => BinList};
setup(String) ->
    #{exclude => string:split(binary:list_to_bin(String), ",")}.

description(#{exclude := List}) ->
    iolist_to_binary(io_lib:format("Exclude spans from set ~p", [List])).

should_sample(_Ctx, _TraceId, _Links, Name, _Kind, _Attrs, #{exclude := List}) ->
    case lists:member(Name, List) of
        false -> {?RECORD_AND_SAMPLE, [], []};
        _ -> {?DROP, [], []}
    end.
