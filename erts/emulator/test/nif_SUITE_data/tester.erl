-module(tester).

-include_lib("test_server/include/test_server.hrl").

-export([load_nif_lib/2, run/0]).


load_nif_lib(Config, LibName) ->
    ?line Path = ?config(data_dir, Config),    
    erlang:load_nif(filename:join(Path,LibName), []).

run() ->
    exit({nif_not_loaded,?MODULE,?LINE}).
