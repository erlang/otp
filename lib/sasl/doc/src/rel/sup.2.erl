-module(sup).
-vsn(2).
-behaviour(supervisor).
-export([init/1]).

init([]) ->
    SupFlags = {one_for_one, 4, 3600},
    GS1 = {gs1, {gs1, start_link, []}, permanent, 2000, worker, [gs1]},  
    GS2 = {gs2, {gs2, start_link, []}, permanent, 2000, worker, [gs2]},  
    {ok, {SupFlags, [GS1, GS2]}}.
