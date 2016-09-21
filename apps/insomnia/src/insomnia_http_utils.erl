%%%-------------------------------------------------------------------
%% @doc insomnia HTTP API utils
%% @end
%%%-------------------------------------------------------------------

-module(insomnia_http_utils).

%% API
-export([parse_search_query/1]).

-export([encode_json/1]).

-include("insomnia.hrl").

%%====================================================================
%% API
%%====================================================================

-spec parse_search_query([{binary(), binary()}]) -> #{}.
parse_search_query(Req) ->
    Keys = [<<"bid">>,
            <<"cur">>,
            <<"geo">>,
            <<"cat">>],
    reconstruct_query(Keys, Req, fun validate/2).

-spec encode_json(#{}) -> binary().
encode_json(Data) ->
    jsx:prettify(jsx:encode(Data)).

%%====================================================================
%% Internal functions
%%====================================================================

-spec reconstruct_query([binary()], [{binary(), binary()}], fun((binary()) -> binary() | float())) -> #{}.
reconstruct_query(Keys, Query, F) ->
    lists:foldl(fun(Key, Acc) -> Value = proplists:get_value(Key, Query), Acc#{Key => F(Key, Value)} end, #{}, Keys).

-spec validate(binary(), 'undefined' | binary()) -> binary() | float().
%% let it be always first
validate(Key, <<"">>) ->
    throw(<<"The value for '", Key/binary, "' is empty.">>);


validate(<<"bid">>, undefined) ->
    ?DEFAULT_BID;
validate(<<"bid">> = Key, Term) ->
    try
        positive_numeric(Term, fun erlang:binary_to_integer/1)
    catch
        _C:_R ->
            try
                positive_numeric(Term, fun erlang:binary_to_float/1)
            catch
                _C:_R ->
                    throw(<<"The value '", Term/binary, "' is not valid for '", Key/binary, "'. Allowed value is: greater then zero numeric.">>)
            end
    end;


validate(<<"cur">>, undefined) ->
    ?DEFAULT_CUR;
validate(<<"cur">>, Term) when Term == <<"USD">> orelse Term == <<"EUR">> orelse Term == <<"RUB">> ->
    Term;
validate(<<"cur">> = Key, Term) ->
    throw(<<"The value '", Term/binary, "' is not valid for '", Key/binary, "'. Allowed value is: USD, EUR or RUB.">>);


validate(<<"geo">>, undefined) ->
    ?DEFAULT_GEO;
validate(<<"geo">>, Term) when Term == <<"US">> orelse Term == <<"EU">> orelse Term == <<"RU">> ->
    Term;
validate(<<"geo">> = Key, Term) ->
    throw(<<"The value '", Term/binary, "' is not valid for '", Key/binary, "'. Allowed value is: US, EU or RU.">>);


validate(<<"cat">>, undefined) ->
    ?DEFAULT_IAB;
validate(<<"cat">>, Term) when Term == <<"IAB1">> orelse Term == <<"IAB7">> orelse Term == <<"IAB8">> ->
    Term;
validate(<<"cat">> = Key, Term) ->
    throw(<<"The value '", Term/binary, "' is not valid for '", Key/binary, "'. Allowed value is: IAB1, IAB7 or IAB8.">>).


-spec positive_numeric(binary(), fun((binary()) -> number())) -> float().
positive_numeric(Term, F) ->
    Bid = float(F(Term)),
    true = Bid > 0,
    Bid.

