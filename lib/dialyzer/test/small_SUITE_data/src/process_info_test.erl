%%
%% Tests hardcoded dependent type info for process_info/1
%%
-module(process_info_test).
-export([pinfo/1]).

pinfo(P) when node(P) == node() ->  % On same node
  case process_info(P) of
    undefined ->
      exit(dead);
    Info -> Info
  end;
pinfo(P) ->                         % On different node
  case rpc:call(node(P), erlang, process_info, [P]) of
    {badrpc, _} ->
      exit(badrpc);
    undefined ->           % This does happen
      exit(dead);
    Info -> Info
  end.
