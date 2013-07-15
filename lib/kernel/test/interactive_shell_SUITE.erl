%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2013. All Rights Reserved.
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
-module(interactive_shell_SUITE).
-include_lib("test_server/include/test_server.hrl").
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 get_columns_and_rows/1, exit_initial/1, job_control_local/1, 
	 job_control_remote/1,
	 job_control_remote_noshell/1,ctrl_keys/1]).

-export([init_per_testcase/2, end_per_testcase/2]).
%% For spawn
-export([toerl_server/3]).

init_per_testcase(_Func, Config) ->
    Dog = test_server:timetrap(test_server:minutes(3)),
    [{watchdog,Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog).


suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [get_columns_and_rows, exit_initial, job_control_local,
     job_control_remote, job_control_remote_noshell,
     ctrl_keys].

groups() -> 
    [].

init_per_suite(Config) ->
    Term = case os:getenv("TERM") of
	       List when is_list(List) ->
		   List;
	       _ ->
		   "dumb"
	   end,
    os:putenv("TERM","vt100"),
    DefShell = get_default_shell(),
    [{default_shell,DefShell},{term,Term}|Config].

end_per_suite(Config) ->
    Term = ?config(term,Config),
    os:putenv("TERM",Term),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%-define(DEBUG,1).
-ifdef(DEBUG).
-define(dbg(Data),erlang:display(Data)).
-else.
-define(dbg(Data),noop).
-endif.

get_columns_and_rows(suite) -> [];
get_columns_and_rows(doc) -> ["Test that the shell can access columns and rows"];
get_columns_and_rows(Config) when is_list(Config) ->
    case proplists:get_value(default_shell,Config) of
	old ->
	    %% Old shell tests
	    ?dbg(old_shell),
	    ?line rtnode([{putline,""},
			  {putline, "2."},
			  {getline, "2"},
			  {putline,"io:columns()."},
			  {getline_re,".*{error,enotsup}"},
			  {putline,"io:rows()."},
			  {getline_re,".*{error,enotsup}"}

			 ],[]),
	    ?line rtnode([{putline,""},
			  {putline, "2."},
			  {getline, "2"},
			  {putline,"io:columns()."},
			  {getline_re,".*{ok,90}"},
			  {putline,"io:rows()."},
			  {getline_re,".*{ok,40}"}],
			 [],
			 "stty rows 40; stty columns 90; ");
	new ->
	    % New shell tests
	    ?dbg(new_shell),
	    ?line rtnode([{putline,""},
			  {putline, "2."},
			  {getline, "2"},
			  {putline,"io:columns()."},
			  %% Behaviour change in R12B-5, returns 80
			  %%		  {getline,"{error,enotsup}"},
			  {getline,"{ok,80}"},
			  {putline,"io:rows()."},
			  %% Behaviour change in R12B-5, returns 24
			  %%		  {getline,"{error,enotsup}"}
			  {getline,"{ok,24}"}
			 ],[]),
	    ?line rtnode([{putline,""},
			  {putline, "2."},
			  {getline, "2"},
			  {putline,"io:columns()."},
			  {getline,"{ok,90}"},
			  {putline,"io:rows()."},
			  {getline,"{ok,40}"}],
			 [],
			 "stty rows 40; stty columns 90; ")
    end.
    
    

exit_initial(suite) -> [];
exit_initial(doc) -> ["Tests that exit of initial shell restarts shell"];
exit_initial(Config) when is_list(Config) ->
    case proplists:get_value(default_shell,Config) of
	old ->
	    rtnode([{putline,""},
		    {putline, "2."},
		    {getline_re, ".*2"},
		    {putline,"exit()."},
		    {getline,""},
		    {getline,"Eshell"},
		    {putline,""},
		    {putline,"35."},
		    {getline_re,".*35"}],[]);
	new ->
	    rtnode([{putline,""},
		    {putline, "2."},
		    {getline, "2"},
		    {putline,"exit()."},
		    {getline,""},
		    {getline,"Eshell"},
		    {putline,""},
		    {putline,"35."},
		    {getline_re,"35"}],[])
    end.

job_control_local(suite) -> [];
job_control_local(doc) -> [ "Tests that local shell can be "
			    "started by means of job control" ];
job_control_local(Config) when is_list(Config) ->
    case proplists:get_value(default_shell,Config) of
	old ->
	    %% Old shell tests
	    {skip,"No new shell found"};
	new ->
	    %% New shell tests
	    ?line rtnode([{putline,""},
			  {putline, "2."},
			  {getline, "2"},
			  {putline,[7]},
			  {sleep,timeout(short)},
			  {putline,""},
			  {getline," -->"},
			  {putline,"s"},
			  {putline,"c"},
			  {putline_raw,""},
			  {getline,"Eshell"},
			  {putline_raw,""},
			  {getline,"1>"},
			  {putline,"35."},
			  {getline,"35"}],[])
    end.

job_control_remote(suite) -> [];
job_control_remote(doc) -> [ "Tests that remote shell can be "
			     "started by means of job control" ];
job_control_remote(Config) when is_list(Config) ->
    case {node(),proplists:get_value(default_shell,Config)} of
	{nonode@nohost,_} ->
	    ?line exit(not_distributed);
	{_,old} ->
	    {skip,"No new shell found"};
	_ ->
	    ?line RNode = create_nodename(),
	    ?line MyNode = atom2list(node()),
	    ?line Pid = spawn_link(fun() ->
					   receive die ->
						   ok
					   end 
				   end),
	    ?line PidStr = pid_to_list(Pid),
	    ?line register(kalaskula,Pid),
	    ?line CookieString = lists:flatten(
				   io_lib:format("~w",
						 [erlang:get_cookie()])),
	    ?line Res = rtnode([{putline,""},
				{putline, "erlang:get_cookie()."},
				{getline, CookieString},
				{putline,[7]},
				{sleep,timeout(short)},
				{putline,""},
				{getline," -->"},
				{putline,"r '"++MyNode++"'"},
				{putline,"c"},
				{putline_raw,""},
				{getline,"Eshell"},
				{sleep,timeout(short)},
				{putline_raw,""},
				{getline,"("++MyNode++")1>"},
				{putline,"whereis(kalaskula)."},
				{getline,PidStr},
				{sleep,timeout(short)}, % Race, known bug.
				{putline_raw,"exit()."},
				{getline,"***"},
				{putline,[7]},
				{putline,""},
				{getline," -->"},
				{putline,"c 1"},
				{putline,""},
				{sleep,timeout(short)},
				{putline_raw,""},
				{getline,"("++RNode++")"}],RNode),
	    ?line Pid ! die,
	    ?line Res
    end.
job_control_remote_noshell(suite) -> [];
job_control_remote_noshell(doc) -> 
    [ "Tests that remote shell can be "
      "started by means of job control to -noshell node" ];
job_control_remote_noshell(Config) when is_list(Config) ->
    case {node(),proplists:get_value(default_shell,Config)} of
	{nonode@nohost,_} ->
	    ?line exit(not_distributed);
	{_,old} ->
	    {skip,"No new shell found"};
	_ ->
	    ?line RNode = create_nodename(),
	    ?line NSNode = start_noshell_node(interactive_shell_noshell),
	    ?line Pid = spawn_link(NSNode, fun() ->
						   receive die ->
							   ok
						   end 
					   end),
	    ?line PidStr = rpc:call(NSNode,erlang,pid_to_list,[Pid]),
	    ?line true = rpc:call(NSNode,erlang,register,[kalaskula,Pid]),
	    ?line NSNodeStr = atom2list(NSNode),
	    ?line CookieString = lists:flatten(
				   io_lib:format("~w",
						 [erlang:get_cookie()])),
	    ?line Res = rtnode([{putline,""},
				{putline, "erlang:get_cookie()."},
				{getline, CookieString},
				{putline,[7]},
				{sleep,timeout(short)},
				{putline,""},
				{getline," -->"},
				{putline,"r '"++NSNodeStr++"'"},
				{putline,"c"},
				{putline_raw,""},
				{getline,"Eshell"},
				{sleep,timeout(short)},
				{putline_raw,""},
				{getline,"("++NSNodeStr++")1>"},
				{putline,"whereis(kalaskula)."},
				{getline,PidStr},
				{sleep,timeout(short)}, % Race, known bug.
				{putline_raw,"exit()."},
				{getline,"***"},
				{putline,[7]},
				{putline,""},
				{getline," -->"},
				{putline,"c 1"},
				{putline,""},
				{sleep,timeout(short)},
				{putline_raw,""},
				{getline,"("++RNode++")"}],RNode),
	    ?line Pid ! die,
	    ?line stop_noshell_node(NSNode),
	    ?line Res
    end.

ctrl_keys(suite) -> [];
ctrl_keys(doc) -> ["Tests various control keys"];
ctrl_keys(_Conf) when is_list(_Conf) ->
    Cu=[$\^u],
    Cw=[$\^w],
    Home=[27,$O,$H],
    End=[27,$O,$F],
    rtnode([{putline,""},
	    {putline,"2."},
	    {getline,"2"},
	    {putline,"\"hello "++Cw++"world\"."},	% test <CTRL>+W
	    {getline,"\"world\""},
	    {putline,"\"hello "++Cu++"\"world\"."},	% test <CTRL>+U
	    {getline,"\"world\""},
	    {putline,"world\"."++Home++"\"hello "},	% test <HOME>
	    {getline,"\"hello world\""},
	    {putline,"world"++Home++"\"hello "++End++"\"."},	% test <END>
	    {getline,"\"hello world\""}]
	    ++wordLeft()++wordRight(),[]).


wordLeft() ->
    L1=[27,27,$[,$D],
    L2=[27]++"[5D",
    L3=[27]++"[1;5D",
    wordLeft(L1)++wordLeft(L2)++wordLeft(L3).

wordLeft(Chars) ->
    End=[27,$O,$F],
    [{putline,"\"world\""++Chars++"hello "++End++"."},
     {getline,"\"hello world\""}].

wordRight() ->
    R1=[27,27,$[,$C],
    R2=[27]++"[5C",
    R3=[27]++"[1;5C",
    wordRight(R1)++wordRight(R2)++wordRight(R3).

wordRight(Chars) ->
    Home=[27,$O,$H],
    [{putline,"world"++Home++"\"hello "++Chars++"\"."},
     {getline,"\"hello world\""}].


rtnode(C,N) ->
    rtnode(C,N,[]).
rtnode(Commands,Nodename,ErlPrefix) ->
    ?line case get_progs() of
	      {error,_Reason} ->
		  ?line {skip,"No runerl present"};
	      {RunErl,ToErl,Erl} ->
		  ?line case create_tempdir() of
			    {error, Reason2} ->
				?line {skip, Reason2};
			    Tempdir ->
				?line SPid = 
				    start_runerl_node(RunErl,ErlPrefix++"\\\""++Erl++"\\\"",
						      Tempdir,Nodename),
				?line CPid = start_toerl_server(ToErl,Tempdir),
				?line erase(getline_skipped),
				?line Res = 
				    (catch get_and_put(CPid, Commands,1)),
				?line case stop_runerl_node(CPid) of
					  {error,_} ->
					      ?line CPid2 = 
						  start_toerl_server
						    (ToErl,Tempdir),
					      ?line erase(getline_skipped),
					      ?line ok = get_and_put
							   (CPid2, 
							    [{putline,[7]},
							     {sleep,
							      timeout(short)},
							     {putline,""},
							     {getline," -->"},
							     {putline,"s"},
							     {putline,"c"},
							     {putline,""}],1),
					      ?line stop_runerl_node(CPid2);
					  _ ->
					      ?line ok
				      end,
				?line wait_for_runerl_server(SPid),
				?line ok = rm_rf(Tempdir),
				?line ok = Res
			end
	  end.

timeout(long) ->
    2 * timeout(normal);
timeout(short) ->
    timeout(normal) div 10;
timeout(normal) ->
    10000 * test_server:timetrap_scale_factor().


start_noshell_node(Name) ->
    PADir =  filename:dirname(code:which(?MODULE)),
    {ok, Node} = test_server:start_node(Name,slave,[{args," -noshell -pa "++
						     PADir++" "}]),
    Node.
stop_noshell_node(Node) ->
    test_server:stop_node(Node).


rm_rf(Dir) ->
    try
      {ok,List} = file:list_dir(Dir),
      Files = [filename:join([Dir,X]) || X <- List],
      [case file:list_dir(Y) of
	   {error, enotdir} ->
	       ok = file:delete(Y);
	   _ ->
	       ok = rm_rf(Y)
       end || Y <- Files],
       ok = file:del_dir(Dir),
       ok
    catch
	_:Exception -> {error, {Exception,Dir}}
    end.
       

get_and_put(_CPid,[],_) ->
    ok;
get_and_put(CPid, [{sleep, X}|T],N) ->
    ?dbg({sleep, X}),
    receive
    after X ->
	    get_and_put(CPid,T,N+1)
    end;
get_and_put(CPid, [{getline, Match}|T],N) ->
    ?dbg({getline, Match}),
    CPid ! {self(), {get_line, timeout(normal)}},
    receive
	{get_line, timeout} ->
	    error_logger:error_msg("~p: getline timeout waiting for \"~s\" "
				   "(command number ~p, skipped: ~p)~n",
				   [?MODULE, Match,N,get(getline_skipped)]),
	    {error, timeout};
	{get_line, Data} ->
	    ?dbg({data,Data}),
	    case lists:prefix(Match, Data) of
		true ->
		    erase(getline_skipped),
		    get_and_put(CPid, T,N+1);
		false ->
		    case get(getline_skipped) of
			undefined ->
			    put(getline_skipped,[Data]);
			List ->
			    put(getline_skipped,List ++ [Data])
		    end,
		    get_and_put(CPid,  [{getline, Match}|T],N)
	    end
    end;

%% Hey ho copy paste from stdlib/io_proto_SUITE
get_and_put(CPid, [{getline_re, Match}|T],N) ->
    ?dbg({getline_re, Match}),
    CPid ! {self(), {get_line, timeout(normal)}},
    receive
	{get_line, timeout} ->
	    error_logger:error_msg("~p: getline_re timeout waiting for \"~s\" "
				   "(command number ~p, skipped: ~p)~n",
				   [?MODULE, Match,N,get(getline_skipped)]),
	    {error, timeout};
	{get_line, Data} ->
	    ?dbg({data,Data}),
	    case re:run(Data, Match,[{capture,none}]) of
		match ->
		    erase(getline_skipped),
		    get_and_put(CPid, T,N+1);
		_ ->
		    case get(getline_skipped) of
			undefined ->
			    put(getline_skipped,[Data]);
			List ->
			    put(getline_skipped,List ++ [Data])
		    end,
		    get_and_put(CPid,  [{getline_re, Match}|T],N)
	    end
    end;

get_and_put(CPid, [{putline_raw, Line}|T],N) ->
    ?dbg({putline_raw, Line}),
    CPid ! {self(), {send_line, Line}},
    Timeout = timeout(normal),
    receive
	{send_line, ok} ->
	     get_and_put(CPid, T,N+1)
    after Timeout ->
	    error_logger:error_msg("~p: putline_raw timeout (~p) sending "
				   "\"~s\" (command number ~p)~n",
				   [?MODULE, Timeout, Line, N]),
	    {error, timeout}
    end;

get_and_put(CPid, [{putline, Line}|T],N) ->
    ?dbg({putline, Line}),
    CPid ! {self(), {send_line, Line}},
    Timeout = timeout(normal),
    receive
	{send_line, ok} ->
	     get_and_put(CPid, [{getline, []}|T],N)
    after Timeout ->
	    error_logger:error_msg("~p: putline timeout (~p) sending "
				   "\"~s\" (command number ~p)~n[~p]~n",
				   [?MODULE, Timeout, Line, N,get()]),
	    {error, timeout}
    end.

wait_for_runerl_server(SPid) ->
    Ref = erlang:monitor(process, SPid), 
    Timeout = timeout(long),
    receive
	{'DOWN', Ref, process, SPid, _} ->
	    ok
    after Timeout ->
	    {error, timeout}
    end.
	
    

stop_runerl_node(CPid) ->
    Ref = erlang:monitor(process, CPid),
    CPid ! {self(), kill_emulator},
    Timeout = timeout(long),
    receive
	{'DOWN', Ref, process, CPid, noproc} ->
	    ok;
	{'DOWN', Ref, process, CPid, normal} ->
	    ok;
	{'DOWN', Ref, process, CPid, {error, Reason}} ->
	    {error, Reason}
    after Timeout ->
	    {error, timeout}
    end.

get_progs() ->
    case os:type() of
	{unix,freebsd} ->
	    {error,"cant use run_erl on freebsd"};
	{unix,openbsd} ->
	    {error,"cant use run_erl on openbsd"};
	{unix,_} ->
	    case os:find_executable("run_erl") of
		RE when is_list(RE) ->
		    case  os:find_executable("to_erl") of
			TE when is_list(TE) ->
			    case os:find_executable("erl") of
				E when is_list(E) ->
				    {RE,TE,E};
				_ ->
				    {error, "Could not find erl command"}
			    end;
			_ ->
			    {error, "Could not find to_erl command"}
		    end;
		_ ->
		    {error, "Could not find run_erl command"}
	    end;
	_ ->
	    {error, "Not a unix OS"}
    end.

create_tempdir() ->
    create_tempdir(filename:join(["/tmp","rtnode"++os:getpid()]),$A).

create_tempdir(Dir,X) when X > $Z, X < $a ->
    create_tempdir(Dir,$a);
create_tempdir(Dir,X) when X > $z -> 
    Estr = lists:flatten(
		     io_lib:format("Unable to create ~s, reason eexist",
				   [Dir++[$z]])),
    {error, Estr};
create_tempdir(Dir0, Ch) ->
    % Expect fairly standard unix.
    Dir = Dir0++[Ch],
    case file:make_dir(Dir) of
	{error, eexist} ->
	    create_tempdir(Dir0, Ch+1);
	{error, Reason} ->
	    Estr = lists:flatten(
		     io_lib:format("Unable to create ~s, reason ~p",
				   [Dir,Reason])),
	    {error,Estr};
	ok ->
	    Dir
    end.

create_nodename() ->
    create_nodename($A).

create_nodename(X) when X > $Z, X < $a ->
    create_nodename($a);
create_nodename(X) when X > $z -> 
    {error,out_of_nodenames};
create_nodename(X) ->
    NN = "rtnode"++os:getpid()++[X],
    case file:read_file_info(filename:join(["/tmp",NN])) of
	{error,enoent} ->
	    Host = lists:nth(2,string:tokens(atom_to_list(node()),"@")),
	    NN++"@"++Host;
	_ ->
	    create_nodename(X+1)
    end.


start_runerl_node(RunErl,Erl,Tempdir,Nodename) ->
    XArg = case Nodename of
	       [] ->
		   [];
	       _ ->
		   " -sname "++(if is_atom(Nodename) -> atom_to_list(Nodename);
				  true -> Nodename 
			       end)++
		       " -setcookie "++atom_to_list(erlang:get_cookie())
	   end,
    spawn(fun() ->
		  os:cmd("\""++RunErl++"\" "++Tempdir++"/ "++Tempdir++" \""++
			 Erl++XArg++"\"")
	  end).

start_toerl_server(ToErl,Tempdir) ->
    Pid = spawn(?MODULE,toerl_server,[self(),ToErl,Tempdir]),
    receive
	{Pid,started} ->
	    Pid;
	{Pid,error,Reason} ->
	    {error,Reason}
    end.

try_to_erl(_Command, 0) ->
    {error, cannot_to_erl};
try_to_erl(Command, N) ->
    ?dbg({?LINE,N}),
    Port = open_port({spawn, Command},[eof,{line,1000}]),
    Timeout = timeout(normal) div 2,
    receive
	{Port, eof} -> 	
	    receive after Timeout ->
			    ok
		    end,
	    try_to_erl(Command, N-1)
    after Timeout ->
	    ?dbg(Port),
	    Port
    end.

toerl_server(Parent,ToErl,Tempdir) ->
    Port = try_to_erl("\""++ToErl++"\" "++Tempdir++"/ 2>/dev/null",8),
    case Port of
	P when is_port(P) ->
	    Parent ! {self(),started};
	{error,Other} ->
	    Parent ! {self(),error,Other},
	    exit(Other)
    end,
    case toerl_loop(Port,[]) of
	normal ->
	    ok;
	{error, Reason} ->
	    error_logger:error_msg("toerl_server exit with reason ~p~n",
				   [Reason]),
	    exit(Reason)
    end.

toerl_loop(Port,Acc) ->
    ?dbg({toerl_loop, Port, Acc}),
    receive
	{Port,{data,{Tag0,Data}}} when is_port(Port) ->
	    ?dbg({?LINE,Port,{data,{Tag0,Data}}}),
	    case Acc of
		[{noeol,Data0}|T0] ->
		    toerl_loop(Port,[{Tag0, Data0++Data}|T0]);
		_ ->
		    toerl_loop(Port,[{Tag0,Data}|Acc])
	    end;
	 {Pid,{get_line,Timeout}} ->
	    case Acc of
		[] ->
		    case get_data_within(Port,Timeout,[]) of
			timeout ->
			    Pid ! {get_line, timeout},
			    toerl_loop(Port,[]);
			{noeol,Data1} ->
			    Pid ! {get_line, timeout},
			    toerl_loop(Port,[{noeol,Data1}]);
			{eol,Data2} ->
			    Pid ! {get_line, Data2}, 
			    toerl_loop(Port,[])
		    end;
		[{noeol,Data3}] ->
		    case get_data_within(Port,Timeout,Data3) of
			timeout ->
			    Pid ! {get_line, timeout},
			    toerl_loop(Port,Acc);
			{noeol,Data4} ->
			    Pid ! {get_line, timeout},
			    toerl_loop(Port,[{noeol,Data4}]);
			{eol,Data5} ->
			    Pid ! {get_line, Data5},
			    toerl_loop(Port,[])
		    end;
		List ->
		    {NewAcc,[{eol,Data6}]} = lists:split(length(List)-1,List),
		    Pid ! {get_line,Data6},
		    toerl_loop(Port,NewAcc)
	    end;
	{Pid, {send_line, Data7}} ->
	    Port ! {self(),{command, Data7++"\n"}},
	    Pid ! {send_line, ok},
	    toerl_loop(Port,Acc);
	{_Pid, kill_emulator} ->
	    Port ! {self(),{command, "init:stop().\n"}},
	    Timeout1 = timeout(long),
	    receive
		{Port,eof} ->
		    normal
	    after Timeout1 ->
		    {error, kill_timeout}
	    end;
	{Port, eof} ->
	    {error, unexpected_eof};
	Other ->
	    {error, {unexpected, Other}}
    end.
	
millistamp() ->
    {Mega, Secs, Micros} = erlang:now(),
    (Micros div 1000) + Secs * 1000 + Mega * 1000000000.
    
get_data_within(Port, X, Acc) when X =< 0 ->
    ?dbg({get_data_within, X, Acc, ?LINE}),
    receive
	{Port,{data,{Tag0,Data}}} ->
	    ?dbg({?LINE,Port,{data,{Tag0,Data}}}),
	    {Tag0, Acc++Data}
    after 0 ->
	    case Acc of
		[] ->
		    timeout;
		Noeol ->
		    {noeol,Noeol}
	    end
    end;


get_data_within(Port, Timeout, Acc) ->	
    ?dbg({get_data_within, Timeout, Acc, ?LINE}),
    T1 = millistamp(),
    receive 
	{Port,{data,{noeol,Data}}} ->
	    ?dbg({?LINE,Port,{data,{noeol,Data}}}),
	    Elapsed = millistamp() - T1 + 1,
	    get_data_within(Port, Timeout - Elapsed, Acc ++ Data); 
	{Port,{data,{eol,Data1}}} ->
	    ?dbg({?LINE,Port,{data,{eol,Data1}}}),
	    {eol, Acc ++ Data1}
    after Timeout ->
	    timeout
    end.
	    
get_default_shell() ->
    try
	rtnode([{putline,""},
		{putline, "whereis(user_drv)."},
		{getline, "undefined"}],[]),
	old
    catch _E:_R ->
	    ?dbg({_E,_R}),
	    new
    end.

atom2list(A) ->
    lists:flatten(io_lib:format("~s", [A])).
