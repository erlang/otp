%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
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
%%
-module(asn1ct_name).

%%-compile(export_all).
-export([start/0,
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
    Parent = self(),
    case get(?MODULE) of
	undefined ->
            put(?MODULE, spawn_link(fun() ->
                            Ref = monitor(process, Parent),
                            name_server_loop({Ref,Parent},[])
                    end)),
            ok;
	_Pid ->
	    already_started
    end.

stop() ->
    req(stop),
    erase(?MODULE).

name_server_loop({Ref, Parent} = Monitor,Vars) ->
%%    io:format("name -- ~w~n",[Vars]),
    receive
	{From,{current,Variable}} ->
	    From ! {?MODULE,get_curr(Vars,Variable)},
	    name_server_loop(Monitor,Vars);
	{From,{pop,Variable}} ->
	    From ! {?MODULE,done},
	    name_server_loop(Monitor,pop_var(Vars,Variable));
	{From,{push,Variable}} ->
	    From ! {?MODULE,done},
	    name_server_loop(Monitor,push_var(Vars,Variable));
	{From,{delete,Variable}} ->
	    From ! {?MODULE,done},
	    name_server_loop(Monitor,delete_var(Vars,Variable));
	{From,{new,Variable}} ->
	    From ! {?MODULE,done},
	    name_server_loop(Monitor,new_var(Vars,Variable));
	{From,{prev,Variable}} ->
	    From ! {?MODULE,get_prev(Vars,Variable)},
	    name_server_loop(Monitor,Vars);
	{From,{next,Variable}} ->
	    From ! {?MODULE,get_next(Vars,Variable)},
	    name_server_loop(Monitor,Vars);
    {'DOWN', Ref, process, Parent, Reason} ->
        exit(Reason);
	{From,stop} ->
	    From ! {?MODULE,stopped}
    end.

active(V) ->
    case curr(V) of
	nil -> false;
	_ -> true
    end.

req(Req) ->
    get(?MODULE) ! {self(), Req},
    receive
        {?MODULE, Reply} -> Reply
    after 5000 ->
            exit(name_server_timeout)
    end.

pop(V) ->     req({pop,V}).
push(V) ->         req({push,V}).
clear() ->     stop(), start().
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
