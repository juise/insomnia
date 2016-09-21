%%%-------------------------------------------------------------------
%% @doc insomnia — entry point
%% @end
%%%-------------------------------------------------------------------

-module(insomnia).

%% API
-export([start/0,
         stop/0]).

-export([id/0]).

-export([request/1]).

-include("insomnia.hrl").

%%====================================================================
%% API
%%====================================================================

-spec start() -> 'ok'.
start() ->
    {ok, _} = application:ensure_all_started(insomnia),
    ok.

-spec stop() -> 'ok'.
stop() ->
    ok = application:stop(insomnia).

-spec id() -> pos_integer().
id() ->
    erlang:system_time().

-spec request(#{}) -> binary().
request(Query) ->
    Query#{url => lists:nth(rand:uniform(length(?URL)), ?URL)}.

%%====================================================================
%% Internal functions
%%====================================================================

