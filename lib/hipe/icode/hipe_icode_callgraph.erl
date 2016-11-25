%% -*- erlang-indent-level: 2 -*-
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%%-----------------------------------------------------------------------
%% File    : hipe_icode_callgraph.erl
%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%% Purpose : Creates a call graph to find out in what order functions
%%           in a module have to be compiled to gain best information
%%           in hipe_icode_type.erl.
%%
%% Created :  7 Jun 2004 by Tobias Lindahl <tobiasl@it.uu.se>
%%-----------------------------------------------------------------------
-module(hipe_icode_callgraph).

-export([construct/1, 
	 get_called_modules/1,
	 to_list/1,
	 construct_callgraph/1]).

-define(NO_UNUSED, true).

-ifndef(NO_UNUSED).
-export([is_empty/1, take_first/1, pp/1]).
-endif.

-include("hipe_icode.hrl").
-include("hipe_icode_primops.hrl").

%%------------------------------------------------------------------------

-type mfa_icode() :: {mfa(), #icode{}}.

-record(icode_callgraph, {codedict :: dict:dict(), ordered_sccs :: [[mfa()]]}).

%%------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------

-spec construct([mfa_icode()]) -> #icode_callgraph{}.

construct(List) ->
  Calls = get_local_calls(List),
  %% io:format("Calls: ~p\n", [lists:keysort(1, Calls)]),
  Edges = get_edges(Calls),
  %% io:format("Edges: ~p\n", [Edges]),
  DiGraph = hipe_digraph:from_list(Edges),
  Nodes = ordsets:from_list([MFA || {MFA, _} <- List]),
  DiGraph1 = hipe_digraph:add_node_list(Nodes, DiGraph),
  SCCs = hipe_digraph:reverse_preorder_sccs(DiGraph1),
  #icode_callgraph{codedict = dict:from_list(List), ordered_sccs = SCCs}.

-spec construct_callgraph([mfa_icode()]) -> hipe_digraph:hdg().

construct_callgraph(List) ->
  Calls = get_local_calls2(List),
  Edges = get_edges(Calls),  
  hipe_digraph:from_list(Edges).

-spec to_list(#icode_callgraph{}) -> [mfa_icode()].

to_list(#icode_callgraph{codedict = Dict, ordered_sccs = SCCs}) ->
  FlatList = lists:flatten(SCCs),
  [{MFA, dict:fetch(MFA, Dict)} || MFA <- FlatList].

%%------------------------------------------------------------------------

-ifndef(NO_UNUSED).

-spec is_empty(#icode_callgraph{}) -> boolean().

is_empty(#icode_callgraph{ordered_sccs = SCCs}) ->
  SCCs =:= [].

-spec take_first(#icode_callgraph{}) -> {[mfa_icode()], #icode_callgraph{}}.

take_first(#icode_callgraph{codedict = Dict, ordered_sccs = [H|T]} = CG) ->
  SCCCode = [{Mod, dict:fetch(Mod, Dict)} || Mod <- H],
  {SCCCode, CG#icode_callgraph{ordered_sccs = T}}.

-spec pp(#icode_callgraph{}) -> 'ok'.

pp(#icode_callgraph{ordered_sccs = SCCs}) ->
  io:format("Callgraph ~p\n", [SCCs]).
-endif.

%%------------------------------------------------------------------------
%% Get the modules called from this module

-spec get_called_modules([mfa_icode()]) -> ordsets:ordset(atom()).

get_called_modules(List) ->
  get_remote_calls(List, []).

get_remote_calls([{_MFA, Icode}|Left], Acc) ->
  CallSet = get_remote_calls_1(hipe_icode:icode_code(Icode), Acc),
  get_remote_calls(Left, ordsets:union(Acc, CallSet));
get_remote_calls([], Acc) ->
  Acc.

get_remote_calls_1([I|Left], Set) ->
  NewSet =
    case I of
      #icode_call{} ->
	case hipe_icode:call_type(I) of
	  remote ->
	    {M, _F, _A} = hipe_icode:call_fun(I),
	    ordsets:add_element(M, Set);
	  _ ->
	    Set
	end;
      #icode_enter{} ->
	case hipe_icode:enter_type(I) of
	  remote ->
	    {M, _F, _A} = hipe_icode:enter_fun(I),
	    ordsets:add_element(M, Set);
	  _ ->
	    Set
	end;
      _ ->
	Set
    end,
  get_remote_calls_1(Left, NewSet);
get_remote_calls_1([], Set) ->
  Set.

%%------------------------------------------------------------------------
%% Find functions called (or entered) by each function.

get_local_calls(List) ->
  RemoveFun = fun ordsets:del_element/2,
  get_local_calls(List, RemoveFun, []).

get_local_calls2(List) ->
  RemoveFun = fun(_,Set) -> Set end,
  get_local_calls(List, RemoveFun, []).

get_local_calls([{{_M, _F, _A} = MFA, Icode}|Left], RemoveFun, Acc) ->
  CallSet = get_local_calls_1(hipe_icode:icode_code(Icode)),
  %% Exclude recursive calls.
  CallSet1 = RemoveFun(MFA, CallSet),
  get_local_calls(Left, RemoveFun, [{MFA, CallSet1}|Acc]);
get_local_calls([], _RemoveFun, Acc) ->
  Acc.

get_local_calls_1(Icode) ->
  get_local_calls_1(Icode, []).

get_local_calls_1([I|Left], Set) ->
  NewSet =
    case I of
      #icode_call{} ->
	case hipe_icode:call_type(I) of
	  local ->
	    Fun = hipe_icode:call_fun(I),
	    ordsets:add_element(Fun, Set);
	  primop ->
	    case hipe_icode:call_fun(I) of
	      #mkfun{mfa = Fun} ->
		ordsets:add_element(Fun, Set);
	      _ ->
		Set
	    end;
	  remote ->
	    Set
	end;
      #icode_enter{} ->
	case hipe_icode:enter_type(I) of
	  local ->
	    Fun = hipe_icode:enter_fun(I),
	    ordsets:add_element(Fun, Set);
	  primop ->
	    case hipe_icode:enter_fun(I) of
	      #mkfun{mfa = Fun} ->
		ordsets:add_element(Fun, Set);
	      _ ->
		Set
	    end;
	  remote ->
	    Set
	end;
      _ ->
	Set
    end,
  get_local_calls_1(Left, NewSet);
get_local_calls_1([], Set) ->
  Set.

%%------------------------------------------------------------------------
%% Find the edges in the callgraph.

get_edges(Calls) ->
  get_edges(Calls, []).

get_edges([{MFA, Set}|Left], Edges) ->  
  EdgeList = [{MFA, X} || X <- Set],
  EdgeSet = ordsets:from_list(EdgeList),
  get_edges(Left, ordsets:union(EdgeSet, Edges));
get_edges([], Edges) ->
  Edges.
