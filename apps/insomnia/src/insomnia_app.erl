%%%-------------------------------------------------------------------
%% @doc insomnia public API
%% @end
%%%-------------------------------------------------------------------

-module(insomnia_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%%====================================================================
%% API
%%====================================================================

-spec start(any(), any()) -> no_return().
start(_StartType, _StartArgs) ->
    Pid = insomnia_sup:start_link(),
    ok = start_cowboy(),
    Pid.

-spec stop(any()) -> 'ok'.
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_cowboy() ->
    [{ok, IP}, {ok, Port}, {ok, Backlog}, {ok, MaxConnections}] = [application:get_env(insomnia, Env) || Env <- [ip, port, backlog, max_conn]],
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/v1/ping",        insomnia_http_api, [{action, ping}]},
            {"/api/v1/request",     insomnia_http_api, [{action, request}]}
        ]}
    ]),
    %% backlog it's size of the TCP listen queue (maximum number of simultaneous connections for this listener), the value should be lower or equal then net.core.somaxconn (man 2 listen)
    %% max_connections it's soft maximum number of simultaneous connections for this listener, the value can be lower, equal or greater then backlog
    %% NbAcceptors it's a pool of protocol non blocking acceptors, which accept connections and forward to the given protocol handler, 100 is sufficient
    case cowboy:start_http(http, 100, [{max_connections, MaxConnections}, {backlog, Backlog}, {ip, IP}, {port, Port}], [{env, [{dispatch, Dispatch}]}]) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _Pid}} ->
            ok
    end.
