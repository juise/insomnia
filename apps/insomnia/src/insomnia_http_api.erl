%%%-------------------------------------------------------------------
%% @doc insomnia HTTP API handler
%% @end
%%%-------------------------------------------------------------------

-module(insomnia_http_api).

%% API
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%%====================================================================
%% API
%%====================================================================

init({tcp, http}, Req, State) ->
    {ok, Req, State}.

handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    {ok, Reply} = handle(string:to_upper(binary_to_list(Method)), State, Req),
    {ok, Reply, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

handle("GET", [{action, ping}], Req) ->
    cowboy_req:reply(200, [], <<"pong">>, Req);

handle("GET", [{action, request}], Req) ->
    cowboy_req:reply(200, [], <<"ok">>, Req);

handle(_, _, Req) ->
    cowboy_req:reply(405, Req). %% Method Not Allowed

