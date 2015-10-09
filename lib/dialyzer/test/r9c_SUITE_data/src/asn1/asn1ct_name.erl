%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: asn1ct_name.erl,v 1.1 2008/12/17 09:53:30 mikpe Exp $
%%
-module(asn1ct_name).

%%-compile(export_all).
-export([name_server_loop/1,
	 start/0,
	 stop/0,
	 push/1,
	 pop/1,
	 curr/1,
	 clear/0,
	 delete/1,
	 active/1,
	 prev/1,
	 next/1,
	 all/1,
	 new/1]).

start() ->
    start_server(asn1_ns, asn1ct_name,name_server_loop,[[]]).

stop() -> stop_server(asn1_ns).

name_server_loop(Vars) ->
%%    io:format("name -- ~w~n",[Vars]),
    receive
	{From,{current,Variable}} ->
	    From ! {asn1_ns,get_curr(Vars,Variable)},
	    name_server_loop(Vars);
	{From,{pop,Variable}} ->
	    From ! {asn1_ns,done},
	    name_server_loop(pop_var(Vars,Variable));
	{From,{push,Variable}} ->
	    From ! {asn1_ns,done},
	    name_server_loop(push_var(Vars,Variable));
	{From,{delete,Variable}} ->
	    From ! {asn1_ns,done},
	    name_server_loop(delete_var(Vars,Variable));
	{From,{new,Variable}} ->
	    From ! {asn1_ns,done},
	    name_server_loop(new_var(Vars,Variable));
	{From,{prev,Variable}} ->
	    From ! {asn1_ns,get_prev(Vars,Variable)},
	    name_server_loop(Vars);
	{From,{next,Variable}} ->
	    From ! {asn1_ns,get_next(Vars,Variable)},
	    name_server_loop(Vars);
	{From,stop} ->
	    From ! {asn1_ns,stopped},
	    exit(normal)
    end.

active(V) ->
    case curr(V) of
	nil -> false;
	_ -> true
    end.

req(Req) ->
    asn1_ns ! {self(), Req},
    receive {asn1_ns, Reply} -> Reply end.

pop(V) ->     req({pop,V}).
push(V) ->         req({push,V}).
clear() ->     req(stop), start().
curr(V) ->     req({current,V}).
new(V) ->      req({new,V}).
delete(V) ->   req({delete,V}).
prev(V) ->
    case req({prev,V}) of
	none ->
	    exit('cant get prev of none');
	Rep -> Rep
    end.

next(V) ->
    case req({next,V}) of
	none ->
	    exit('cant get next of none');
	Rep -> Rep
    end.

all(V) ->
    Curr = curr(V),
    if Curr == V -> [];
	true ->
	    lists:reverse(generate(V,last(Curr),[],0))
    end.

generate(V,Number,Res,Pos) ->
    Ell = Pos+1,
    if
	Ell > Number ->
	    Res;
	true ->
	    generate(V,Number,[list_to_atom(lists:concat([V,Ell]))|Res],Ell)
    end.

last(V) ->
    last2(lists:reverse(atom_to_list(V))).

last2(RevL) ->
    list_to_integer(lists:reverse(get_digs(RevL))).


get_digs([H|T]) ->
    if
	H < $9+1,
	H > $0-1 ->
	    [H|get_digs(T)];
	true ->
	    []
    end.

push_var(Vars,Variable) ->
    case lists:keysearch(Variable,1,Vars) of
	false ->
	    [{Variable,[0]}|Vars];
	{value,{Variable,[Digit|Drest]}} ->
	    NewVars = lists:keydelete(Variable,1,Vars),
	    [{Variable,[Digit,Digit|Drest]}|NewVars]
    end.

pop_var(Vars,Variable) ->
    case lists:keysearch(Variable,1,Vars) of
	false ->
	    ok;
	{value,{Variable,[_Dig]}} ->
	    lists:keydelete(Variable,1,Vars);
	{value,{Variable,[_Dig|Digits]}} ->
	    NewVars = lists:keydelete(Variable,1,Vars),
	    [{Variable,Digits}|NewVars]
    end.

get_curr([],Variable) ->
    Variable;
get_curr([{Variable,[0|_Drest]}|_Tail],Variable) ->
    Variable;
get_curr([{Variable,[Digit|_Drest]}|_Tail],Variable) ->
    list_to_atom(lists:concat([Variable,integer_to_list(Digit)]));

get_curr([_|Tail],Variable) ->
    get_curr(Tail,Variable).

new_var(Vars,Variable) ->
    case lists:keysearch(Variable,1,Vars) of
	false ->
	    [{Variable,[1]}|Vars];
	{value,{Variable,[Digit|Drest]}} ->
	    NewVars = lists:keydelete(Variable,1,Vars),
	    [{Variable,[Digit+1|Drest]}|NewVars]
    end.

delete_var(Vars,Variable) ->
    case lists:keysearch(Variable,1,Vars) of
	false ->
	    Vars;
	{value,{Variable,[N]}} when N =< 1  ->
	    lists:keydelete(Variable,1,Vars);
	{value,{Variable,[Digit|Drest]}} ->
	    case Digit of
		0 ->
		    Vars;
		_ ->
		    NewVars = lists:keydelete(Variable,1,Vars),
		    [{Variable,[Digit-1|Drest]}|NewVars]
	    end
    end.

get_prev(Vars,Variable) ->
    case lists:keysearch(Variable,1,Vars) of
	false ->
	    none;
	{value,{Variable,[Digit|_]}} when Digit =< 1 ->
	    Variable;
	{value,{Variable,[Digit|_]}} when Digit > 1 ->
	    list_to_atom(lists:concat([Variable,
				       integer_to_list(Digit-1)]));
	_ ->
	    none
    end.

get_next(Vars,Variable) ->
    case lists:keysearch(Variable,1,Vars) of
	false ->
	    list_to_atom(lists:concat([Variable,"1"]));
	{value,{Variable,[Digit|_]}} when Digit >= 0 ->
	    list_to_atom(lists:concat([Variable,
				       integer_to_list(Digit+1)]));
	_ ->
	    none
    end.


stop_server(Name) ->
    stop_server(Name, whereis(Name)).
stop_server(_Name, undefined) -> stopped;
stop_server(Name, _Pid) ->
    Name  ! {self(), stop},
    receive {Name, _} -> stopped end.


start_server(Name,Mod,Fun,Args) ->
    case whereis(Name) of
	undefined ->
	    register(Name, spawn(Mod,Fun, Args));
	_Pid ->
	    already_started
    end.
