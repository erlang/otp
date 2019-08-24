-module(key_compatibility).
-export([main/0]).

%% Compile like this:
%%  erlc +'{debug_info_key,"an old key"}' key_compatibility.erl

main() ->
    ok.
