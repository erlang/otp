%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
%%------------------------------------------------------------
%%
%% Digraph handling for process view GUI. Feeble attempt at data
%% separation. Provides functional interface to the data structures
%% vdata and edata, v for vertex and e for edge.
%%
%%------------------------------------------------------------
-module(appmon_dg).

-include("appmon_dg.hrl").

%% Exports for vertices
-export([get/3, get/2, set/4, av/3, add/4, del/2, visited/3]).

%% Exports for edges
-export([eget/2, eget/3, eset/4, eadd/4, edel/2, ae/3]).

%% Exports for convenience
-export([print_dg/1]).


%%------------------------------------------------------------


eget(all, DG) ->
    digraph:edges(DG).

eget(data, DG, E) ->
    case digraph:edge(DG, E) of
	{_, _V1, _V2, Data} -> Data;
	_Other    -> false
    end;
eget(edge, DG, {V1, V2}) ->
    case digraph:edge(DG, {V1, V2}) of
	{E, W1, W2, ED} -> {E, W1, W2, ED};
	Other ->
	    case digraph:edge(DG, {V2, V1}) of
		{E, W1, W2, ED} -> {E, W1, W2, ED};
		Other -> false
	    end
    end;

%% Weight in edge name
eget(edge, DG, {V1, V2, Weight}) ->
    case digraph:edge(DG, {V1, V2, Weight}) of
	{E, W1, W2, ED} -> {E, W1, W2, ED};
	_Other -> false
    end;
eget(in, DG, V) ->
    efilter(digraph:in_edges(DG, V)).

efilter(Es) -> 
    lists:filter(fun({_V1, _V2, primary}) -> true;
		 (_E) -> false end,
		 Es).

eset(ref, DG, E, Ref) ->
    {E2, _V1, _V2, D} = eget(edge, DG, E),
    update_e(DG, E2, D#edata{ref=Ref});
eset(line, DG, E, Line) ->
    {E2, _V1, _V2, D} = eget(edge, DG, E),
    update_e(DG, E2, D#edata{line=Line}).

edel(DG, E) ->
    digraph:del_edge(DG, E).

eadd(DG, E, D, Ref) ->
    case eget(edge, DG, E) of
	{_, _, _, ED} when is_record(ED, edata), ED#edata.ref == Ref ->
	    known;
	{_, _, _, ED} when is_record(ED, edata), ED#edata.ref /= Ref ->
	    update_e(DG, E, ED#edata{ref=Ref}),
	    updated;
	_Other ->
	    ae(DG, E, D)
    end.

ae(DG, {V1, V2, Weight}, D) ->
    digraph:add_edge(DG, {V1, V2, Weight}, V1, V2, D).

update_e(DG, {V1, V2, Weight}, D) ->
    digraph:del_edge(DG, {V1, V2, Weight}),
    digraph:add_edge(DG, {V1, V2, Weight}, V1, V2, D).

%% Filter destination vertex from a list of edges
vfilter(Vs) ->
    lists:map(fun({_V1, V2, _Weight}) -> V2;
	      ({_V1, V2}) -> V2
	     end, Vs).

get(all, DG) ->
    digraph:vertices(DG).

get(data, DG, {V1, V2}) ->
    case digraph:edge(DG, {V1, V2}) of
	{_,_,_,Data} -> Data;
	_Other    -> false
    end;
get(data, DG, V) ->
    case digraph:vertex(DG, V) of
	{_,Data} -> Data;
	_Other    -> false
    end;

%% Return all children of vertex V (those which V has edges to)
get(out, DG, V) ->
    vfilter(efilter(digraph:out_edges(DG, V)));
get(in, DG, V) ->
    digraph:in_neighbours(DG, V);
get(edges, DG, V) ->
    digraph:edges(DG, V);
get(w, DG, V) ->
    Data = get(data, DG, V),
    Data#vdata.width;
get(x, DG, V) ->
    Data = get(data, DG, V),
    Data#vdata.x.

set(type, DG, V, Type) ->
    D = get(data, DG, V),
    av(DG, V, D#vdata{type=Type});

set(ref, DG, V, Ref) ->
    D = get(data, DG, V),
    av(DG, V, D#vdata{ref=Ref});

set(y, DG, V, Y) ->
    D = get(data, DG, V),
    av(DG, V, D#vdata{y=Y});

set(data, DG, V, D) when is_record(D, vdata)->
    av(DG, V, D);

set(x, DG, V, X) ->
    D = get(data, DG, V),
    if  D#vdata.x /= X -> 
	    av(DG, V, D#vdata{x=X});
	true -> true
    end.

visited(DG, {V1, V2}, Ref) ->			% for edge
    D = eget(data, DG, {V1, V2}),
    if  is_record(D, edata), D#edata.ref == Ref -> true;
	true -> false
    end;
visited(DG, V, Ref) ->
    D = get(data, DG, V),
    if  is_record(D, vdata), D#vdata.ref == Ref -> true;
	true -> false
    end.

add(DG, V, D, Ref) ->
    case get(data, DG, V) of
	D2 when is_record(D2, vdata), D2#vdata.ref==Ref -> 
	    io:format("Ooops in ~p:add vertex~n", [?MODULE]),
	    known;
	D2 when is_record(D2, vdata) -> 
	    %%io:format("~p touch vertex ~p~n", [self(), V]),
	    set(ref, DG, V, Ref), 
	    set(type, DG, V, D#vdata.type), 
	    save_coords(DG, V),
	    updated;
	_Other -> 
	    av(DG, V, D), added
    end.

save_coords(DG, V) ->
    D = get(data, DG, V),
    D2 = D#vdata{origx=D#vdata.x, origy=D#vdata.y},
    av(DG, V, D2).

del(DG, V) ->
    digraph:del_vertex(DG, V).


av(DG, V, D) ->
    digraph:add_vertex(DG, V, D).

print_dg(DG) ->
    io:format("Vertices:~n", []),
    lists:foreach(fun(V) -> io:format("  ~p ~p~n", 
				      [V, get(data, DG, V)]) end,
		  get(all, DG)),
    io:format("Edges:~n", []),
    lists:foreach(fun(V) -> io:format("  ~p ~p~n",
				      [V, eget(edge, DG, V)]) end,
		  eget(all, DG)),
    true.
