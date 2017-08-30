%% Bug reported by Dan Gudmundsson, test shrunk down by Magnus Lång.

%% The problem is that dialyzer_dep generates edges from the fun 
%% application to both of the functions, and then during the warning pass 
%% dialyzer_dataflow:handle_apply_or_call generates warnings for any such 
%% edge that won't return.

%% Since dialyzer_dep is currently supposed to overapproximate rather than
%% underapproximate, the fix was to modify handle_apply_or_call to not generate
%% warnings if some of the possible funs can succeed.

-module(wpc_hlines).

-export([do_export/0]).

do_export() ->
   {Proj, _} =  % The culprit seems to be putting the funs in a tuple
     {fun good/1, fun bad/1},
   Proj(true).

good(_) -> ok.
bad(false) -> ok.
