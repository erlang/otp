-module(dummy_sup_2).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Dummy = {dummy_server,
             {dummy_server, start_link, []},
             permanent, 5000, worker, [dummy_server]},

    {ok, {{one_for_one, 10, 10}, [Dummy]}}.
