%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
%% %CopyrightEnd%
%%
-module(erlsrv).

%% Purpose : Control the external erlsrv program.

%%-compile(export_all).
-export([get_all_services/0,get_service/1,get_service/2,store_service/1,
	 store_service/2,
	 new_service/3, new_service/4, disable_service/2,
	 enable_service/2, disable_service/1, enable_service/1, 
	 remove_service/1, erlsrv/1, rename_service/2,
	 rename_service/3]).

erlsrv(EVer) ->
    Root = code:root_dir(),
    filename:join([Root, "erts-" ++ EVer, "bin", "erlsrv.exe"]).

current_version() ->
    hd(string:tokens(erlang:system_info(version),"_ ")).

%%% Returns {ok, Output} | failed | {error, Reason}
run_erlsrv(Command) ->
    run_erlsrv(current_version(),Command).
run_erlsrv(EVer, Command) ->
    case catch(open_port({spawn, "\"" ++ erlsrv(EVer) ++ "\" " ++ Command},
			 [{line,1000}, in, eof])) of
	{'EXIT',{Reason,_}} ->
	    {port_error, Reason};
	Port ->
	    case read_all_data(Port) of
		[] ->
		    failed;
		 X ->
		    {ok, X}
	    end
    end.

run_erlsrv_interactive(EVer, Commands) ->
    case catch(open_port({spawn, "\""++ erlsrv(EVer) ++ "\" readargs"},
			 [{line,1000}, eof])) of
	{'EXIT',{Reason,_}} ->
	    {port_error, Reason};
	Port ->
	    write_all_data(Port, Commands),
	    case read_all_data(Port) of
		[] ->
		    failed;
		 X ->
		    {ok, X}
	    end
    end.

write_all_data(Port,[]) ->
    Port ! {self(), {command, io_lib:nl()}},
    ok;
write_all_data(Port,[H|T]) ->
    Port ! {self(), {command, unicode:characters_to_binary([H,io_lib:nl()])}},
    write_all_data(Port,T).

read_all_data(Port) ->
    Data0 = lists:reverse(read_all_data(Port,[],[])),
    %% Convert from utf8 to a list of chars
    [unicode:characters_to_list(list_to_binary(Data)) || Data <- Data0].

read_all_data(Port,Line,Lines) ->
    receive
	{Port, {data, {noeol,Data}}} ->
	    read_all_data(Port,Line++Data,Lines);
	{Port, {data, {eol,Data}}} ->
	    read_all_data(Port,[],[Line++Data|Lines]);
	{Port,_Other} ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    case Line of
			[] -> Lines;
			_ -> [Line|Lines]
		    end
	    end
    end.


%%% Get all registered erlsrv services.
get_all_services() ->
    case run_erlsrv("list") of
	failed ->
	    [];
	{ok, [_]} ->
	    [];
	{ok, [_H|T]} ->
	    F = fun(X) ->
			hd(string:tokens(X,"\t "))
		end,
	    lists:map(F,T);
	_ ->
	    {error, external_program_failed}
    end.

disable_service(ServiceName) ->
    disable_service(current_version(), ServiceName).
disable_service(EVer, ServiceName) ->
    run_erlsrv(EVer, "disable " ++ ServiceName).
enable_service(ServiceName) ->
    enable_service(current_version(), ServiceName).
enable_service(EVer, ServiceName) ->
    run_erlsrv(EVer, "enable " ++ ServiceName).
remove_service(ServiceName) ->
    run_erlsrv("remove " ++ ServiceName).
rename_service(FromName, ToName) ->
    rename_service(current_version(), FromName, ToName).
rename_service(EVer, FromName, ToName) ->
    run_erlsrv(EVer, "rename " ++ FromName ++ " " ++ ToName).

%%% Get all information about a service
%%% Returns [{Field,Value | []} ...]
%%% Field is one of:
%%% servicename : The service name (equal to parameter...)
%%% stopaction : The erlang expression that shall stop the node
%%% onfail : Action to take when erlang fails unexpectedly
%%% machine : Full pathname of the erlang machine or start_erl program
%%% workdir : The initial working directory of the erlang machine
%%% sname | name : The short name of the node
%%% priority : The OS priority of the erlang process
%%% args : All arguments correctly parsed into a list of strings
%%% comment : The service description
%%% internalservicename : The windows internal service name
%%% env : A list of environment variables and values [{"VAR", "VALUE"}]
%%% Example:
%%% [{servicename,"kalle_R4A"},
%%%  {stopaction,"erlang:halt()."},
%%%  {args,["-boot", "nisse","--","-reldir", "c:\myapproot"]}
%%%  {env,[{"FOO","BAR"},{"VEGETABLE","TOMATO"}]}]

get_service(ServiceName) ->
    get_service(current_version(), ServiceName).
get_service(EVer, ServiceName) ->   
    case run_erlsrv(EVer, "list " ++ ServiceName) of
	failed ->
	    {error, no_such_service};
	{port_error, Reason} ->
	    {error, {port_error, Reason}};
	{ok, Data} ->
	    Table = [{"Service name",servicename,[]},
		     {"StopAction",stopaction, []},
		     {"OnFail",onfail, "ignore"},
		     {"Machine",machine, []},
		     {"WorkDir",workdir, []},
		     {"SName",sname, []},
		     {"Name",name, []},
		     {"Priority",priority, "default"},
		     {"DebugType",debugtype, "none"},
		     {"Args",args,[]},
		     {"InternalServiceName",internalservicename,[]},
		     {"Comment",comment,[]}], 
	    %% Env has special treatment...
	    F = fun(X) ->
			{Name,Value} = splitline(X),
			case lists:keysearch(Name,1,Table) of
			    {value,{Name,_Atom,Value}} ->
				[];
			    {value,{Name,Atom,_}} ->
				{Atom,Value};
			    _ ->
				[]
			end
		end,
            %%% First split by Env:
	    {Before, After} = split_by_env(Data),
	    FirstPass = lists:flatten(lists:map(F,Before)),
	    %%% If the arguments are there, split them to
	    SecondPass = split_arglist(FirstPass),
	    %%% And now, if After contains anything, that is vwat to
	    %%% have in the environment list...
	    EnvParts = lists:map(
			 fun(S) ->
				 X = string:strip(S,left,$\t),
				 case hd(string:tokens(X,"=")) of
				     X ->
					 %% Can this happen?
					 {X,""};
				     Y ->
					 {Y,
					  lists:sublist(X,length(Y)+2,
							length(X))}
				 end
			 end,
			 After),
	    case EnvParts of
		[] ->
		    SecondPass;
		_ ->
		    lists:append(SecondPass,[{env,EnvParts}])
	    end
    end.


store_service(Service) ->
    store_service(current_version(),Service).
store_service(EmulatorVersion,Service) ->
    case lists:keysearch(servicename,1,Service) of
	false ->
	    {error, no_servicename};
	{value, {_,Name}} ->
	    {Action,Service1} = case get_service(EmulatorVersion,Name) of
			 {error, no_such_service} ->
			     {"add",Service};
			 _ ->
			     {"set",
			      lists:keydelete(internalservicename,1,Service)}
		     end,
	    Commands = [Action | build_commands(Name, Service1)],
	    case run_erlsrv_interactive(EmulatorVersion,Commands) of
		{ok, _} ->
		    ok;
		X ->
		    {error, X}
	    end;
	_ ->
	    {error, malformed_description}
    end.

build_commands(Action, Service) ->
    [ Action | lists:reverse(build_commands2(Service,[]))].

build_commands2([],A) ->
    A;
build_commands2([{env,[]}|T],A) ->
    build_commands2(T,A);
build_commands2([{env,[{Var,Val}|Et]}|T],A) ->
    build_commands2([{env,Et}|T],[Var ++ "=" ++ Val, "-env" | A]);
build_commands2([{servicename,_}|T],A) ->
    build_commands2(T,A);
build_commands2([{Atom,[]} | T],A) ->
    build_commands2(T,["-" ++ atom_to_list(Atom) | A]);
build_commands2([{args,L}|T],A) ->
    build_commands2(T,[concat_args(L),"-args"| A]);
build_commands2([{Atom,Value} | T],A) ->
    build_commands2(T,[Value, "-" ++ atom_to_list(Atom) | A]).

concat_args([H|T]) ->
    H ++ concat_args2(T).
concat_args2([]) ->
    "";
concat_args2([H|T]) ->
    " " ++ H ++ concat_args2(T).


new_service(NewServiceName, OldService, Data) ->
    new_service(NewServiceName, OldService, Data, []).
new_service(NewServiceName, OldService, Data, RestartName) -> 
    Tmp0 = lists:keydelete(internalservicename,1,OldService), %Remove when 
						% creating new service from 
						% old.
    Tmp1 = lists:keyreplace(servicename, 1, Tmp0, 
			    {servicename, NewServiceName}),
    Tmp = case lists:keysearch(env,1,Tmp1) of
	      {value, {env,Env0}} ->
		  Env1 = lists:keydelete("ERLSRV_SERVICE_NAME",1,Env0),
		  lists:keyreplace(env,1,Tmp1,
				   {env, [{"ERLSRV_SERVICE_NAME", 
					   RestartName} | 
					  Env1]});
	      _ ->
		  Tmp1
	  end,

    ArgsTmp = case lists:keysearch(args, 1, Tmp) of
	       false ->
		   [];
	       {value, {args, OldArgs}} ->
		   OldArgs
	   end,
    Args = backstrip(ArgsTmp,"++"), %% Remove trailing ++, has no meaning
    {Found, Tail} = lists:foldr(fun(A,{Flag,AccIn}) -> 
					case {Flag, A} of 
					    {true, _} -> {Flag,AccIn}; 
					    {false, "++"} -> {true, AccIn};
					    _ -> {false, [A|AccIn]} 
					end 
				end, {false,[]}, Args),
    
    {OtherFlags, _DataDir} = case Found of
				true ->
				    check_tail(Tail);
				false ->
				    {[], false} 
			    end,
    NewArgs1 = case Data of
		   [] ->
		       OtherFlags;
		   _ -> 
		       ["-data", Data| OtherFlags]
	       end,
    case Found of
	false ->
	    A = case NewArgs1 of
		    [] ->
			[];
		    _ ->
			["++" | NewArgs1]
		end,
	    case {Args,A} of
		{[],[]} ->
		    Tmp;
		{[],_} ->
		    Tmp ++ [{args, A}];
		{_,_} ->
		    lists:keyreplace(args, 1, Tmp, {args, Args ++ A})
	    end;
	true ->
	    StripArgs = backstrip(Args,["++"|Tail]),
	    NewArgs2 = case NewArgs1 of
			   [] ->
			       [];
			   _ ->
			       ["++" |NewArgs1]
		       end,
	    NewArgs = StripArgs ++ NewArgs2,
	    lists:keyreplace(args, 1, Tmp, {args, NewArgs})
    end.
    

backstrip(List,Tail) ->
    lists:reverse(backstrip2(lists:reverse(List),lists:reverse(Tail))).
backstrip2([A|T1],[A|T2]) ->
    backstrip2(T1,T2);
backstrip2(L,_) ->
    L.

check_tail(Tail) ->
    {A,B} = check_tail(Tail, [], false),
    {lists:reverse(A),B}.

check_tail([], OtherFlags, DataDir) ->
    {OtherFlags, DataDir};
check_tail(["-data", TheDataDir|T], OtherFlags, _DataDir) ->
    check_tail(T, OtherFlags, TheDataDir);
check_tail([H|T],OtherFlags,DataDir) ->
    check_tail(T,[H|OtherFlags],DataDir).




%%% Recursive, The list is small
split_arglist([]) ->
    [];
split_arglist([{args,Str}|T]) ->    
    [{args,parse_arglist(Str)}|T];
split_arglist([H|T]) ->
    [H|split_arglist(T)].

%% Not recursive, may be long...
parse_arglist(Str) ->
    lists:reverse(parse_arglist(Str,[])).
parse_arglist(Str,Accum) ->
    Stripped = string:strip(Str,left),
    case length(Stripped) of
	0 ->
	    Accum;
	_ ->
	    {Next, Rest} = pick_argument(Str),
	    parse_arglist(Rest,[Next | Accum])
    end.

pick_argument(Str) ->
    {Rev,Rest} = pick_argument(normal,Str,[]),
    {lists:reverse(Rev),Rest}.

pick_argument(_,[],Acc) ->
    {Acc, ""};
pick_argument(normal,[$ |T],Acc) ->
    {Acc,T};
pick_argument(normal,[$\\|T],Acc) ->
    pick_argument(normal_escaped,T,[$\\|Acc]);
pick_argument(normal,[$"|T],Acc) ->
    pick_argument(quoted,T,[$"|Acc]);
pick_argument(normal_escaped,[$"|T],Acc) ->
    pick_argument(bquoted,T,[$"|Acc]);
pick_argument(normal_escaped,[A|T],Acc) ->
    pick_argument(normal,T,[A|Acc]);
pick_argument(quoted_escaped,[H|T],Acc) ->
    pick_argument(quoted,T,[H|Acc]);
pick_argument(quoted,[$"|T],Acc) ->
    pick_argument(normal,T,[$"|Acc]);
pick_argument(quoted,[$\\|T],Acc) ->
    pick_argument(quoted_escaped,T,[$\\|Acc]);
pick_argument(quoted,[H|T],Acc) ->
    pick_argument(quoted,T,[H|Acc]);
pick_argument(bquoted_escaped,[$"|T],Acc) ->
    pick_argument(normal,T,[$"|Acc]);
pick_argument(bquoted_escaped,[H|T],Acc) ->
    pick_argument(bquoted,T,[H|Acc]);
pick_argument(bquoted,[$\\|T],Acc) ->
    pick_argument(bquoted_escaped,T,[$\\|Acc]);
pick_argument(bquoted,[H|T],Acc) ->
    pick_argument(bquoted,T,[H|Acc]);
pick_argument(normal,[H|T],Acc) ->
    pick_argument(normal,T,[H|Acc]).

split_helper("Env:",{Where,0}) ->
    {Where + 1, Where};
split_helper(_, {Where,Pos}) ->
    {Where + 1, Pos}.

split_by_env(Data) ->
    %%% Find Env...
    case lists:foldl(fun split_helper/2,{0,0},Data) of
	{_,0} ->
	    %% Not found, hmmmm....
	    {Data,[]};
	{Len,Pos} ->
	    {lists:sublist(Data,Pos),lists:sublist(Data,Pos+2,Len)}
    end.
			

splitline(Line) ->
    case string:chr(Line,$:) of
	0 ->
	    {Line, ""};
	N ->
	    case length(string:substr(Line,N)) of
		1 ->
		    {string:substr(Line,1,N-1),""};
		_ ->
		    {string:substr(Line,1,N-1),string:substr(Line,N+2)}
	    end
    end.
