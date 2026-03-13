-module(cowboy_otel_sampler).

-behavior(otel_sampler).

-export([description/1]).
-export([setup/1]).
-export([should_sample/7]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_sampler.hrl").

%% setup/1 expects a map of Attribute Names to Regex string patterns.
%% Example config: #{'http.route' => <<"^(/metrics|/livez)">>}
setup(ExcludeMap) when is_map(ExcludeMap) ->
    %% Pre-compile the regular expressions once for performance
    CompiledRules = maps:fold(
        fun(AttrKey, Pattern, Acc) ->
            {ok, CompiledRegex} = re:compile(Pattern),
            [{AttrKey, CompiledRegex} | Acc]
        end,
        [],
        ExcludeMap
    ),
    #{exclude_rules => CompiledRules}.

description(#{exclude_rules := Rules}) ->
    Keys = [K || {K, _} <- Rules],
    iolist_to_binary(io_lib:format("Exclude spans matching attributes: ~p", [Keys])).

should_sample(_Ctx, _TraceId, _Links, _Name, _Kind, Attrs, #{exclude_rules := Rules}) ->
    %% Check if the span's attributes match any of our exclusion rules
    case matches_any_rule(Attrs, Rules) of
        true ->
            {?DROP, [], []};
        false ->
            {?RECORD_AND_SAMPLE, [], []}
    end.

%% =====================================================================
%% Internal Helper Functions
%% =====================================================================

matches_any_rule(_Attrs, []) ->
    false;
matches_any_rule(Attrs, [{AttrKey, CompiledRegex} | Rest]) ->
    case get_attribute(AttrKey, Attrs) of
        undefined ->
            %% Attribute not present, check the next rule
            matches_any_rule(Attrs, Rest);
        Value ->
            %% Attribute present, check if it matches the regex
            BinValue = to_binary(Value),
            case re:run(BinValue, CompiledRegex) of
                %% Regex matched, we should DROP
                {match, _} -> true;
                %% No match, check next
                nomatch -> matches_any_rule(Attrs, Rest)
            end
    end.

%% Safely extracts the attribute value from the Attrs map.
get_attribute(Key, Attrs) when is_atom(Key) ->
    BinKey = to_binary(Key),
    case Attrs of
        #{Key := Value} ->
            Value;
        #{BinKey := Value} ->
            Value;
        _ ->
            undefined
    end.

%% re:run/2 requires a subject to be a string or binary.
%% We safely cast the attribute value just in case it's an integer or atom.
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_list(V) -> iolist_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) -> iolist_to_binary(io_lib:format("~p", [V])).
