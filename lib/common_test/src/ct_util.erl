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

%%% @doc Common Test Framework Utilities.
%%%
%%% <p>This is a support module for the Common Test Framework. It
%%% implements the process ct_util_server which acts like a data
%%% holder for suite, configuration and connection data.</p>
%%%
-module(ct_util).

-export([start/0,start/1,start/2,stop/1,update_last_run_index/0]).

-export([register_connection/4,unregister_connection/1,
	 does_connection_exist/3,get_key_from_name/1]).

-export([close_connections/0]).

-export([save_suite_data/3, save_suite_data/2, read_suite_data/1, 
	 delete_suite_data/0, delete_suite_data/1, match_delete_suite_data/1,
	 delete_testdata/0, delete_testdata/1, set_testdata/1, get_testdata/1,
	 update_testdata/2]).

-export([override_silence_all_connections/0, override_silence_connections/1, 
	 get_overridden_silenced_connections/0, 
	 delete_overridden_silenced_connections/0, 
	 silence_all_connections/0, silence_connections/1, is_silenced/1, 
	 reset_silent_connections/0]).

-export([get_mode/0, create_table/3, read_opts/0]).

-export([set_cwd/1, reset_cwd/0]).

-export([parse_table/1]).

-export([listenv/1]).

-export([get_target_name/1, get_connections/2]).

-export([is_test_dir/1, get_testdir/2]).

-export([kill_attached/2, get_attached/1, ct_make_ref/0]).

-export([warn_duplicates/1]).

-include("ct_event.hrl").
-include("ct_util.hrl").

-record(suite_data, {key,name,value}).

%%%-----------------------------------------------------------------
%%% @spec start(Mode) -> Pid | exit(Error)
%%%       Mode = normal | interactive
%%%       Pid = pid()
%%%
%%% @doc Start start the ct_util_server process 
%%% (tool-internal use only).
%%%
%%% <p>This function is called from ct_run.erl. It starts and initiates
%%% the <code>ct_util_server</code></p>
%%% 
%%% <p>Returns the process identity of the
%%% <code>ct_util_server</code>.</p>
%%%
%%% @see ct
start() ->
    start(normal,".").

start(LogDir) when is_list(LogDir) ->
    start(normal,LogDir);
start(Mode) ->
    start(Mode,".").

start(Mode,LogDir) ->
    case whereis(ct_util_server) of
	undefined ->
	    S = self(),
	    Pid = spawn_link(fun() -> do_start(S,Mode,LogDir) end),
	    receive 
		{Pid,started} -> Pid;
		{Pid,Error} -> exit(Error)
	    end;
	Pid ->
	    case get_mode() of
		interactive when Mode==interactive ->
		    Pid;
		interactive ->
		    {error,interactive_mode};
		_OtherMode ->
		    Pid
	    end
    end.

do_start(Parent,Mode,LogDir) ->
    process_flag(trap_exit,true),
    register(ct_util_server,self()),
    create_table(?conn_table,#conn.handle),
    create_table(?board_table,2),
    create_table(?suite_table,#suite_data.key),
    {ok,StartDir} = file:get_cwd(),
    case file:set_cwd(LogDir) of
	ok -> ok;
	E -> exit(E)
    end,
    Opts = case read_opts() of
	       {ok,Opts1} ->
		   Opts1;
	       Error ->
		   Parent ! {self(),Error},
		   exit(Error)
	   end,
    %% start an event manager (if not already started by master)
    case ct_event:start_link() of
	{error,{already_started,_}} ->
	    ok;
	_ ->
	    case whereis(vts) of
		undefined ->
		    ct_event:add_handler();
		VtsPid ->
		    ct_event:add_handler([{vts,VtsPid}])
	    end
    end,
    %% start ct_config server
    ct_config:start(Mode),
    %% add user event handlers
    case lists:keysearch(event_handler,1,Opts) of
	{value,{_,Handlers}} ->
	    Add = fun({H,Args}) ->
			  case catch gen_event:add_handler(?CT_EVMGR_REF,H,Args) of
			      ok -> ok;
			      {'EXIT',Why} -> exit(Why);
			      Other -> exit({event_handler,Other})
			  end
		  end,
	    case catch lists:foreach(Add,Handlers) of
		{'EXIT',Reason} ->
		    Parent ! {self(),Reason};
		_ ->
		    ok
	    end;
	false ->
	    ok
    end,
    {StartTime,TestLogDir} = ct_logs:init(Mode),
    ct_event:notify(#event{name=test_start,
			   node=node(),
			   data={StartTime,
				 lists:flatten(TestLogDir)}}),
    Parent ! {self(),started},
    loop(Mode,[],StartDir).

create_table(TableName,KeyPos) ->
    create_table(TableName,set,KeyPos).
create_table(TableName,Type,KeyPos) ->
    catch ets:delete(TableName),
    ets:new(TableName,[Type,named_table,public,{keypos,KeyPos}]).

read_opts() ->
    case file:consult(ct_run:variables_file_name("./")) of
	{ok,Opts} ->
	    {ok,Opts};
	{error,enoent} ->
	    {error,not_installed};
	Error ->
	    {error,{bad_installation,Error}}
    end.

save_suite_data(Key, Value) ->
    call({save_suite_data, {Key, undefined, Value}}).

save_suite_data(Key, Name, Value) ->
    call({save_suite_data, {Key, Name, Value}}).

read_suite_data(Key) ->
    call({read_suite_data, Key}).

delete_suite_data() ->
    call({delete_suite_data, all}).

delete_suite_data(Key) ->
    call({delete_suite_data, Key}).

match_delete_suite_data(KeyPat) ->
    call({match_delete_suite_data, KeyPat}).

delete_testdata() ->
    call(delete_testdata).

delete_testdata(Key) ->
    call({delete_testdata, Key}).

update_testdata(Key, Fun) ->
    call({update_testdata, Key, Fun}).

set_testdata(TestData) ->
    call({set_testdata, TestData}).

get_testdata(Key) ->
    call({get_testdata, Key}).

set_cwd(Dir) ->
    call({set_cwd,Dir}).

reset_cwd() ->
    call(reset_cwd).

loop(Mode,TestData,StartDir) ->
    receive 
	{update_last_run_index,From} ->
	    ct_logs:make_last_run_index(),
	    return(From,ok),
	    loop(Mode,TestData,StartDir);
	{{save_suite_data,{Key,Name,Value}},From} ->
	    ets:insert(?suite_table, #suite_data{key=Key,
						 name=Name,
						 value=Value}),
	    return(From,ok),
	    loop(Mode,TestData,StartDir);
	{{read_suite_data,Key},From} ->
	    case ets:lookup(?suite_table, Key) of
		[#suite_data{key=Key,name=undefined,value=Value}] ->
		    return(From,Value);
		[#suite_data{key=Key,name=Name,value=Value}] ->
		    return(From,{Name,Value});
		_ ->
		    return(From,undefined)
	    end,
	    loop(Mode,TestData,StartDir);
	{{delete_suite_data,Key},From} ->
	    if Key == all ->
		    ets:delete_all_objects(?suite_table);
	       true ->
		    ets:delete(?suite_table, Key)
	    end,
	    return(From,ok),
	    loop(Mode,TestData,StartDir);
	{{match_delete_suite_data,KeyPat},From} ->
	    ets:match_delete(?suite_table, #suite_data{key=KeyPat,
						       name='_',
						       value='_'}),
	    return(From,ok),
	    loop(Mode,TestData,StartDir);
	{delete_testdata,From} ->
	    return(From,ok),
	    loop(From,[],StartDir);	
	{{delete_testdata,Key},From} ->
	    TestData1 = lists:keydelete(Key,1,TestData),
	    return(From,ok),
	    loop(From,TestData1,StartDir);	
	{{set_testdata,New = {Key,_Val}},From} ->
	    TestData1 = lists:keydelete(Key,1,TestData),
	    return(From,ok),
	    loop(Mode,[New|TestData1],StartDir);
	{{get_testdata,Key},From} ->
	    case lists:keysearch(Key,1,TestData) of
		{value,{Key,Val}} ->
		    return(From,Val);
		_ ->
		    return(From,undefined)
	    end,
	    loop(From,TestData,StartDir);
	{{update_testdata,Key,Fun},From} ->
	    TestData1 =
		case lists:keysearch(Key,1,TestData) of
		    {value,{Key,Val}} ->
			NewVal = Fun(Val),
			return(From,NewVal),
			[{Key,NewVal}|lists:keydelete(Key,1,TestData)];
		    _ ->
			return(From,undefined),
			TestData
		end,
	    loop(From,TestData1,StartDir);	    
	{{set_cwd,Dir},From} ->
	    return(From,file:set_cwd(Dir)),
	    loop(From,TestData,StartDir);
	{reset_cwd,From} ->
	    return(From,file:set_cwd(StartDir)),
	    loop(From,TestData,StartDir);
	{{stop,How},From} ->
	    Time = calendar:local_time(),
	    ct_event:sync_notify(#event{name=test_done,
					node=node(),
					data=Time}),
	    close_connections(ets:tab2list(?conn_table)),
	    ets:delete(?conn_table),
	    ets:delete(?board_table),
	    ets:delete(?suite_table),
	    ct_logs:close(How),
	    ct_event:stop(),
	    ct_config:stop(),
	    file:set_cwd(StartDir),
	    return(From,ok);
	{get_mode,From} ->
	    return(From,Mode),
	    loop(Mode,TestData,StartDir);
	{'EXIT',_Pid,normal} ->
	    loop(Mode,TestData,StartDir);
	{'EXIT',Pid,Reason} ->
	    %% Let process crash in case of error, this shouldn't happen!
	    io:format("\n\nct_util_server got EXIT from ~p: ~p\n\n",
		      [Pid,Reason]),
	    file:set_cwd(StartDir),
	    exit(Reason)
    end.


close_connections([#conn{handle=Handle,callback=CB}|Conns]) ->
    CB:close(Handle),
    close_connections(Conns);
close_connections([]) ->
    ok.

get_key_from_name(Name)->
    ct_config:get_key_from_name(Name).

%%%-----------------------------------------------------------------
%%% @spec register_connection(TargetName,Address,Callback,Handle) -> 
%%%                                              ok | {error,Reason}
%%%      TargetName = ct:target_name()
%%%      Address = term()
%%%      Callback = atom()
%%%      Handle = term
%%%
%%% @doc Register a new connection (tool-internal use only).
%%%
%%% <p>This function can be called when a new connection is
%%% established. The connection data is stored in the connection
%%% table, and ct_util will close all registered connections when the
%%% test is finished by calling <code>Callback:close/1</code>.</p>
register_connection(TargetName,Address,Callback,Handle) ->
    TargetRef = 
	case ct_config:get_ref_from_name(TargetName) of
	    {ok,Ref} ->
		Ref;
	    _ ->
		%% no config name associated with connection,
		%% use handle for identification instead
		Handle
	end,
    ets:insert(?conn_table,#conn{handle=Handle,
				 targetref=TargetRef,
				 address=Address,
				 callback=Callback}),
    ok.

%%%-----------------------------------------------------------------
%%% @spec unregister_connection(Handle) -> ok
%%%      Handle = term
%%%
%%% @doc Unregister a connection (tool-internal use only).
%%%
%%% <p>This function should be called when a registered connection is
%%% closed. It removes the connection data from the connection
%%% table.</p>
unregister_connection(Handle) ->
    ets:delete(?conn_table,Handle),
    ok.


%%%-----------------------------------------------------------------
%%% @spec does_connection_exist(TargetName,Address,Callback) -> 
%%%                                              {ok,Handle} | false
%%%      TargetName = ct:target_name()
%%%      Address = address
%%%      Callback = atom()
%%%      Handle = term()
%%%
%%% @doc Check if a connection already exists.
does_connection_exist(TargetName,Address,Callback) ->
    case ct_config:get_ref_from_name(TargetName) of
	{ok,TargetRef} ->
	    case ets:select(?conn_table,[{#conn{handle='$1',
						targetref=TargetRef,
						address=Address,
						callback=Callback},
					  [],
					  ['$1']}]) of
		[Handle] ->
		    {ok,Handle};
		[] ->
		    false
	    end;
	_ ->
	    false
    end.

%%%-----------------------------------------------------------------
%%% @spec get_connections(TargetName,Callback) -> 
%%%                                {ok,Connections} | {error,Reason}
%%%      TargetName = ct:target_name()
%%%      Callback = atom()
%%%      Connections = [Connection]
%%%      Connection = {Handle,Address}
%%%      Handle = term()
%%%      Address = term()
%%%
%%% @doc Return all connections for the <code>Callback</code> on the
%%% given target (<code>TargetName</code>).
get_connections(TargetName,Callback) ->
    case ct_config:get_ref_from_name(TargetName) of
	{ok,Ref} ->
	    {ok,ets:select(?conn_table,[{#conn{handle='$1',
					       address='$2',
					       targetref=Ref,
					       callback=Callback},
					 [],
					 [{{'$1','$2'}}]}])};
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:get_target_name/1
get_target_name(ConnPid) ->
    case ets:select(?conn_table,[{#conn{handle=ConnPid,targetref='$1',_='_'},
				  [],
				  ['$1']}]) of
	[TargetRef] ->
	    ct_config:get_name_from_ref(TargetRef);
	[] ->
	    {error,{unknown_connection,ConnPid}}
    end.

%%%-----------------------------------------------------------------
%%% @spec close_connections() -> ok
%%%
%%% @doc Close all open connections.
close_connections() ->
    close_connections(ets:tab2list(?conn_table)),
    ok.

%%%-----------------------------------------------------------------
%%% @spec 
%%%
%%% @doc 
override_silence_all_connections() ->
    Protocols = [telnet,ftp,rpc,snmp],
    override_silence_connections(Protocols),
    Protocols.

override_silence_connections(Conns) when is_list(Conns) ->
    Conns1 = lists:map(fun({C,B}) -> {C,B};
			  (C)     -> {C,true}
		       end, Conns),
    set_testdata({override_silent_connections,Conns1}).

get_overridden_silenced_connections() ->
    case get_testdata(override_silent_connections) of
	{error,_} ->
	    undefined;
	Conns ->      % list() or undefined
	    Conns
    end.

delete_overridden_silenced_connections() ->    
    delete_testdata(override_silent_connections).

silence_all_connections() ->
    Protocols = [telnet,ftp,rpc,snmp],
    silence_connections(Protocols),
    Protocols.

silence_connections(Conn) when is_tuple(Conn) ->
    silence_connections([Conn]);
silence_connections(Conn) when is_atom(Conn) ->
    silence_connections([{Conn,true}]);
silence_connections(Conns) when is_list(Conns) ->
    Conns1 = lists:map(fun({C,B}) -> {C,B};
			  (C)     -> {C,true}
		       end, Conns),
    set_testdata({silent_connections,Conns1}).

is_silenced(Conn) ->
    case get_testdata(silent_connections) of
	Conns when is_list(Conns) ->
	    case lists:keysearch(Conn,1,Conns) of
		{value,{Conn,true}} ->
		    true;
		_ ->
		    false
	    end;
	_ ->
	    false
    end.
    
reset_silent_connections() ->
    delete_testdata(silent_connections).
    

%%%-----------------------------------------------------------------
%%% @spec stop(How) -> ok
%%%
%%% @doc Stop the ct_util_server and close all existing connections
%%% (tool-internal use only).
%%%
%%% @see ct
stop(How) ->
    case whereis(ct_util_server) of
	undefined -> ok;
	_ -> call({stop,How})
    end.

%%%-----------------------------------------------------------------
%%% @spec update_last_run_index() -> ok
%%%
%%% @doc Update <code>ct_run.&lt;timestamp&gt;/index.html</code> 
%%% (tool-internal use only).
update_last_run_index() ->
    call(update_last_run_index).


%%%-----------------------------------------------------------------
%%% @spec get_mode() -> Mode
%%%   Mode = normal | interactive
%%%
%%% @doc Return the current mode of the ct_util_server
%%% (tool-internal use only).
get_mode() ->
    call(get_mode).

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:listenv/1
listenv(Telnet) ->
    case ct_telnet:send(Telnet,"listenv") of
	ok ->
	    {ok,Data,_} = ct_telnet:expect(Telnet,
					   ["(^.+)=(.*$)"],
					   [{timeout,seconds(3)},
					    repeat]),
	    {ok,[{Name,Val} || [_,Name,Val] <- Data]};
	{error,Reason} ->
	    {error,{could_not_send_command,Telnet,"listenv",Reason}}
    end.

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:parse_table/1
parse_table(Data) ->
    [Heading|Lines]=
	[remove_space(string:tokens(L, "|"),[]) || L <- Data, hd(L)==$|],
    {Heading,Lines}.

remove_space([Str|Rest],Acc) ->
    remove_space(Rest,[string:strip(string:strip(Str),both,$')|Acc]);
remove_space([],Acc) ->
    list_to_tuple(lists:reverse(Acc)).


%%%-----------------------------------------------------------------
%%% @spec 
%%%
%%% @doc
is_test_dir(Dir) ->
    lists:last(string:tokens(filename:basename(Dir), "_")) == "test".

%%%-----------------------------------------------------------------
%%% @spec 
%%%
%%% @doc
get_testdir(Dir, all) ->
    Abs = abs_name(Dir),
    case is_test_dir(Abs) of
	true ->
	    Abs;
	false ->
	    AbsTest = filename:join(Abs, "test"),
	    case filelib:is_dir(AbsTest) of
		true -> AbsTest;
		false -> Abs
	    end		    
    end;

get_testdir(Dir, [Suite | _]) when is_atom(Suite) ->
    get_testdir(Dir, atom_to_list(Suite));

get_testdir(Dir, [Suite | _]) when is_list(Suite) ->
    get_testdir(Dir, Suite);

get_testdir(Dir, Suite) when is_atom(Suite) ->
    get_testdir(Dir, atom_to_list(Suite));

get_testdir(Dir, Suite) when is_list(Suite) ->
    Abs = abs_name(Dir),
    case is_test_dir(Abs) of
	true ->
	    Abs;
	false ->
	    AbsTest = filename:join(Abs, "test"),
	    Mod = case filename:extension(Suite) of
		      ".erl" -> Suite;
		      _ -> Suite ++ ".erl"
		  end,    
	    case filelib:is_file(filename:join(AbsTest, Mod)) of
		true -> AbsTest;
		false -> Abs
	    end		    
    end;

get_testdir(Dir, _) ->
    get_testdir(Dir, all).


%%%-----------------------------------------------------------------
%%% @spec 
%%%
%%% @doc
get_attached(TCPid) ->
    case dbg_iserver:safe_call({get_attpid,TCPid}) of
	{ok,AttPid} when is_pid(AttPid) ->
	    AttPid;
	_ ->
	    undefined
    end.

%%%-----------------------------------------------------------------
%%% @spec 
%%%
%%% @doc
kill_attached(undefined,_AttPid) ->
    ok;
kill_attached(_TCPid,undefined) ->
    ok;
kill_attached(TCPid,AttPid) ->
    case process_info(TCPid) of
	undefined ->
	    exit(AttPid,kill);
	_ ->
	    ok
    end.
	    

%%%-----------------------------------------------------------------
%%% @spec 
%%%
%%% @doc
warn_duplicates(Suites) ->
    Warn = 
	fun(Mod) ->
		case catch apply(Mod,sequences,[]) of
		    {'EXIT',_} ->
			ok;
		    [] ->
			ok;
		    _ ->
			io:format(user,"~nWARNING! Deprecated function: ~w:sequences/0.~n"
				  "         Use group with sequence property instead.~n",[Mod])
		end
	end,
    lists:foreach(Warn, Suites),
    ok.


%%%-----------------------------------------------------------------
%%% Internal functions
call(Msg) ->
    MRef = erlang:monitor(process,whereis(ct_util_server)),
    Ref = make_ref(),
    ct_util_server ! {Msg,{self(),Ref}},
    receive
	{Ref, Result} -> 
	    erlang:demonitor(MRef, [flush]),
	    Result;
	{'DOWN',MRef,process,_,Reason}  -> 
	    {error,{ct_util_server_down,Reason}}
    end.

return({To,Ref},Result) ->
    To ! {Ref, Result}.

seconds(T) ->
    test_server:seconds(T).

ct_make_ref() ->
    Pid = case whereis(ct_make_ref) of
	      undefined -> 
		  spawn_link(fun() -> ct_make_ref_init() end);
	      P -> 
		  P
	  end,
    Pid ! {self(),ref_req},
    receive
	{Pid,Ref} -> Ref
    end.

ct_make_ref_init() ->
    register(ct_make_ref,self()),
    ct_make_ref_loop(0).

ct_make_ref_loop(N) ->
    receive
	{From,ref_req} -> 
	    From ! {self(),N},
	    ct_make_ref_loop(N+1)
    end.
   
abs_name(Dir0) ->
    Abs = filename:absname(Dir0),
    Dir = case lists:reverse(Abs) of
	      [$/|Rest] -> lists:reverse(Rest);
	      _ -> Abs
	  end,
    abs_name1(Dir,[]).

abs_name1([Drv,$:,$/],Acc) ->
    Split = [[Drv,$:,$/]|Acc],
    abs_name2(Split,[]);
abs_name1("/",Acc) ->
    Split = ["/"|Acc],
    abs_name2(Split,[]);
abs_name1(Dir,Acc) ->
    abs_name1(filename:dirname(Dir),[filename:basename(Dir)|Acc]).

abs_name2([".."|T],[_|Acc]) ->
    abs_name2(T,Acc);
abs_name2(["."|T],Acc) ->
    abs_name2(T,Acc);
abs_name2([H|T],Acc) ->
    abs_name2(T,[H|Acc]);
abs_name2([],Acc) ->
    filename:join(lists:reverse(Acc)).
