%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2010. All Rights Reserved.
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
%%% Purpose: Test erlang.xml re autoimports
-module(autoimport_SUITE).

-include_lib("test_server/include/test_server.hrl").
-export([all/1,init_per_testcase/2,fin_per_testcase/2,autoimports/1]).
-define(TEST_TIMEOUT, ?t:seconds(180)).

all(suite) -> [autoimports].

init_per_testcase(_Func, Config) ->
    Dog = test_server:timetrap(?TEST_TIMEOUT),
    [{watchdog, Dog} | Config].

fin_per_testcase(_Func, Config) ->
    Dog = ?config(watchdog, Config),
    catch test_server:timetrap_cancel(Dog),
    ok.


autoimports(suite) ->
    [];
autoimports(doc) ->
    ["Check that erlang.xml documents autoimports correctly"];
autoimports(Config) when is_list(Config) ->
    ?line XML = filename:join([?config(data_dir,Config),"erlang.xml"]),
    ?line case xml(XML) of
	      [] ->
		  ok;
	      List ->
		  lists:foreach(fun({[],F,A}) ->
					io:format("erlang:~s/~p is wrongly "
						  "documented as ~s/~p~n",
						  [F,A,F,A]);
				   ({"erlang",F,A}) ->
					io:format("~s/~p is wrongly "
						  "documented as "
						  "erlang:~s/~p~n",
						  [F,A,F,A])
				end,List),
		  ?t:fail({wrong_autoimports,List})
	  end.

%%
%% Ugly chunk of code to heuristically analyze the erlang.xml
%% documentation file. Don't view this as an example...
%%

xml(XMLFile) ->
    {ok,File} = file:open(XMLFile,[read]),
    DocData = xloop(file:read_line(File),File),
    file:close(File),
    analyze(DocData).

xloop({ok,Line},File) ->
    case re:run(Line,"\\<name\\>",[{capture,none}]) of
	nomatch ->
	    xloop(file:read_line(File),File);
	match ->
	    X = re:replace(Line,"\\).*",")",[{return,list}]),
	    Y = re:replace(X,".*\\>","",[{return,list}]),
	    Mod = get_module(Y),
	    Rest1 = fstrip(Mod++":",Y),
	    Func = get_function(Rest1),
	    Rest2 = fstrip(Func++"(", Rest1),
	    Argc = count_args(Rest2,1,0,false),
	    [{Mod,Func,Argc} |
	     xloop(file:read_line(File),File)]
    end;
xloop(_,_) ->
    [].

analyze([{[],Func,Arity}|T]) ->
    case erl_internal:bif(list_to_atom(Func),Arity) of
	true ->
	    analyze(T);
	false ->
	    [{[],Func,Arity} |
	     analyze(T)]
    end;
analyze([{"erlang",Func,Arity}|T]) ->
    case erl_internal:bif(list_to_atom(Func),Arity) of
	true ->
	    [{"erlang",Func,Arity}|analyze(T)];
	false ->
	    analyze(T)
    end;
analyze([_|T]) ->
   analyze(T);
analyze([]) ->
   [].


count_args([],_,N,false) ->
    N;
count_args([],_,N,true) ->
    N+1;
count_args(_,0,N,true) ->
    N+1;
count_args(_,0,N,false) ->
    N;
count_args([Par|T],Level,N,Got) when (Par =:= 40) or
				 (Par =:= 123) or (Par =:= 91) ->
    count_args(T,Level+1,N,(Level =:= 1) or Got);
count_args([41|T],1,N,true) ->
    count_args(T,0,N+1,false);
count_args([Par|T],Level,N, Got) when (Par =:= 41) or
				 (Par =:= 125) or (Par =:= 93) ->
    count_args(T,Level-1,N,Got);
count_args([$,|T],1,N,true) ->
    count_args(T,1,N+1,false);
count_args([$ |T],A,B,C) ->
    count_args(T,A,B,C);
count_args([_|T],1,N,_) ->
    count_args(T,1,N,true);
count_args([_|T],A,B,C) ->
    count_args(T,A,B,C).

fstrip([],X) ->
    X;
fstrip(_,[]) ->
    [];
fstrip([H|T1],[H|T2]) ->
    fstrip(T1,T2);
fstrip(_,L) ->
    L.

get_module(X) ->
    get_module(X,[]).
get_module([],_) ->
    [];
get_module([$:|_],Acc) ->
    lists:reverse(Acc);
get_module([40|_],_) -> %(
    [];
get_module([H|T],Acc) ->
    get_module(T,[H|Acc]).

get_function(X) ->
    get_function(X,[]).
get_function([],_) ->
    [];
get_function([40|_],Acc) -> %(
    lists:reverse(Acc);
get_function([H|T],Acc) ->
    get_function(T,[H|Acc]).
