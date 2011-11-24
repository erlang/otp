-module(dummy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    DummySup2 = {dummy_sup_2,
                {dummy_sup_2, start_link, []},
                permanent, 5000, supervisor, [dummy_sup_2]},

    {ok, {{one_for_one, 10, 10}, [DummySup2]}}.
