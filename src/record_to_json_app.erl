%%%-------------------------------------------------------------------
%% @doc record_to_json public API
%% @end
%%%-------------------------------------------------------------------

-module(record_to_json_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    case application:get_env(record_to_json,records_file) of
        {ok,Path} ->
            io:format("Got from environment ~p~n",[Path]),
            try
                Res = rec_to_json_config:set(Path),
                io:format("Result ~p~n",[Res])
            catch
                Exception:Reason  ->
                    io:format("Caught ~p ~p ~n",[Exception,Reason])
            end;

        undefined ->
            io:format("Record Path was not set in environment")

    end,
    record_to_json_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
