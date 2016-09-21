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
    {ok, Req, State#{id => insomnia:id()}}.

handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    {ok, Reply} = handle(string:to_upper(binary_to_list(Method)), State, Req),
    {ok, Reply, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

handle("GET", #{action := ping} = _State, Req) ->
    cowboy_req:reply(200, [], <<"pong">>, Req);

handle("GET", #{action := request, id := Id} = _State, Req) ->
    try cowboy_req:qs_vals(Req) of
        {Query, _} ->
            try insomnia:request(insomnia_http_utils:parse_search_query(Query)) of
                Data ->
                    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], insomnia_http_utils:encode_json(#{id => Id, seatbid => Data, message => <<"Ok">>}), Req)
            catch
                throw:Reason ->
                    cowboy_req:reply(400, [{<<"Content-Type">>, <<"application/json">>}], insomnia_http_utils:encode_json(#{id => Id, seatbid => #{}, message => Reason}), Req)
            end
    catch
        _C:_R ->
            cowboy_req:reply(500, [], <<"Internal server error">>, Req)
    end;

handle(_, _, Req) ->
    cowboy_req:reply(405, Req). %% Method Not Allowed

