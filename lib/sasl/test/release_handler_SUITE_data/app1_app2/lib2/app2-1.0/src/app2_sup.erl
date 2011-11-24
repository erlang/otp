-module(app2_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    AChild = {ginny,{app2_server,start_link,[]},
	      permanent,2000,worker,[app2_server]},
    {ok,{{one_for_all,0,1}, [AChild]}}.
