%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2010. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

-module(old_mod).
-compile(r10).

-export([sort_on_old_node/1, sorter/3]).

-include("test_server.hrl").

sorter(Receiver, Ref, List) ->
    Receiver ! {Ref, lists:sort(List)}.

sort_on_old_node(List) when is_list(List) ->
    OldVersion = "r10",
    ?line Pa = filename:dirname(code:which(?MODULE)),
    ?line {X, Y, Z} = now(),
    ?line NodeName = list_to_atom(OldVersion 
				  ++ "_"
				  ++ integer_to_list(X)
				  ++ integer_to_list(Y)
				  ++ integer_to_list(Z)),
    ?line {ok, Node} = ?t:start_node(NodeName,
				     peer,
				     [{args, " -pa " ++ Pa},
				      {erl, [{release, OldVersion++"b_patched"}]}]),
    ?line Ref = make_ref(),
    ?line spawn_link(Node, ?MODULE, sorter, [self(), Ref, List]),
    ?line SortedPids = receive {Ref, SP} -> SP end,
    ?line true = ?t:stop_node(Node),
    ?line SortedPids.
