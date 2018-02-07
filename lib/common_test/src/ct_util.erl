%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2017. All Rights Reserved.
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

%%% @doc Common Test Framework Utilities.
%%%
%%% <p>This is a support module for the Common Test Framework. It
%%% implements the process ct_util_server which acts like a data
%%% holder for suite, configuration and connection data.</p>
%%%
-module(ct_util).

-export([start/0, start/1, start/2, start/3,
	 stop/1, update_last_run_index/0]).

-export([register_connection/4, unregister_connection/1,
	 does_connection_exist/3, get_key_from_name/1]).

-export([get_connections/1, close_connections/0]).

-export([save_suite_data/3, save_suite_data/2,
	 save_suite_data_async/3, save_suite_data_async/2,
	 read_suite_data/1, 
	 delete_suite_data/0, delete_suite_data/1, match_delete_suite_data/1,
	 delete_testdata/0, delete_testdata/1, match_delete_testdata/1,
	 set_testdata/1, get_testdata/1, get_testdata/2,
	 set_testdata_async/1, update_testdata/2, update_testdata/3,
	 set_verbosity/1, get_verbosity/1]).

-export([override_silence_all_connections/0, override_silence_connections/1, 
	 get_overridden_silenced_connections/0, 
	 delete_overridden_silenced_connections/0, 
	 silence_all_connections/0, silence_connections/1,
	 is_silenced/1, is_silenced/2, reset_silent_connections/0]).

-export([get_mode/0, create_table/3, read_opts/0]).

-export([set_cwd/1, reset_cwd/0, get_start_dir/0]).

-export([parse_table/1]).

-export([listenv/1]).

-export([get_target_name/1, get_connection/2]).

-export([is_test_dir/1, get_testdir/2]).

-export([kill_attached/2, get_attached/1]).

-export([warn_duplicates/1]).

-export([mark_process/0, mark_process/1, is_marked/1, is_marked/2,
         remaining_test_procs/0]).

-export([get_profile_data/0, get_profile_data/1,
	 get_profile_data/2, open_url/3]).

-include("ct.hrl").
-include("ct_event.hrl").
-include("ct_util.hrl").

-define(default_verbosity, [{default,?MAX_VERBOSITY},
			    {'$unspecified',?MAX_VERBOSITY}]).

-record(suite_data, {key,name,value}).

%%%-----------------------------------------------------------------
start() ->
    start(normal, ".", ?default_verbosity).
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
start(LogDir) when is_list(LogDir) ->
    start(normal, LogDir, ?default_verbosity);
start(Mode) ->
    start(Mode, ".", ?default_verbosity).

start(LogDir, Verbosity) when is_list(LogDir) ->
    start(normal, LogDir, Verbosity).

start(Mode, LogDir, Verbosity) ->
    case whereis(ct_util_server) of
	undefined ->
	    S = self(),
	    Pid = spawn_link(fun() -> do_start(S, Mode, LogDir, Verbosity) end),
	    receive 
		{Pid,started} -> Pid;
		{Pid,Error} -> exit(Error);
		{_Ref,{Pid,Error}} -> exit(Error)
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

do_start(Parent, Mode, LogDir, Verbosity) ->
    process_flag(trap_exit,true),
    register(ct_util_server,self()),
    mark_process(),
    create_table(?conn_table,#conn.handle),
    create_table(?board_table,2),
    create_table(?suite_table,#suite_data.key),

    create_table(?verbosity_table,1),
    _ = [ets:insert(?verbosity_table,{Cat,Lvl}) || {Cat,Lvl} <- Verbosity],

    {ok,StartDir} = file:get_cwd(),
    case file:set_cwd(LogDir) of
	ok -> ok;
	E -> exit(E)
    end,
    DoExit = fun(Reason) -> ok = file:set_cwd(StartDir), exit(Reason) end,
    Opts = case read_opts() of
	       {ok,Opts1} ->
		   Opts1;
	       Error ->
		   Parent ! {self(),Error},
		   DoExit(Error)
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
    try ct_config:start(Mode) of
	_ -> ok
    catch
	_Class:CfgError ->
	    DoExit(CfgError)
    end,

    %% add user event handlers
    _ = case lists:keysearch(event_handler,1,Opts) of
	{value,{_,Handlers}} ->
	    Add = fun({H,Args}) ->
			  case catch gen_event:add_handler(?CT_EVMGR_REF,H,Args) of
			      ok -> ok;
			      {'EXIT',Why} -> DoExit(Why);
			      Other -> DoExit({event_handler,Other})
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

    ct_default_gl:start_link(group_leader()),

    {StartTime,TestLogDir} = ct_logs:init(Mode, Verbosity),

    ct_event:notify(#event{name=test_start,
			   node=node(),
			   data={StartTime,
				 lists:flatten(TestLogDir)}}),
    %% Initialize ct_hooks
    _ = try ct_hooks:init(Opts) of
	ok ->
	    Parent ! {self(),started};
	{fail,CTHReason} ->
	    ct_logs:tc_print('Suite Callback',CTHReason,[]),
	    self() ! {{stop,{self(),{user_error,CTHReason}}},
		      {Parent,make_ref()}}
    catch
	_:CTHReason:StackTrace ->
	    ErrorInfo = if is_atom(CTHReason) ->
				io_lib:format("{~tp,~tp}",
					      [CTHReason, StackTrace]);
			   true ->
				CTHReason
			end,
	    ct_logs:tc_print('Suite Callback',ErrorInfo,[]),
	    self() ! {{stop,{self(),{user_error,CTHReason}}},
		      {Parent,make_ref()}}
    end,
    loop(Mode, [], StartDir).

create_table(TableName,KeyPos) ->
    create_table(TableName,set,KeyPos).
create_table(TableName,Type,KeyPos) ->
    catch ets:delete(TableName),
    _ = ets:new(TableName,[Type,named_table,public,{keypos,KeyPos}]),
    ok.

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

save_suite_data_async(Key, Value) ->
    save_suite_data_async(Key, undefined, Value).

save_suite_data_async(Key, Name, Value) ->
    cast({save_suite_data, {Key, Name, Value}}).

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

match_delete_testdata(KeyPat) ->
    call({match_delete_testdata, KeyPat}).

update_testdata(Key, Fun) ->
    update_testdata(Key, Fun, []).

update_testdata(Key, Fun, Opts) ->
    call({update_testdata, Key, Fun, Opts}).

set_testdata(TestData) ->
    call({set_testdata, TestData}).

set_testdata_async(TestData) ->
    cast({set_testdata, TestData}).

get_testdata(Key) ->
    call({get_testdata, Key}).

get_testdata(Key, Timeout) ->
    call({get_testdata, Key}, Timeout).

set_cwd(Dir) ->
    call({set_cwd,Dir}).

reset_cwd() ->
    call(reset_cwd).

get_start_dir() ->
    call(get_start_dir).

%% handle verbosity outside ct_util_server (let the client read
%% the verbosity table) to avoid possible deadlock situations
set_verbosity(Elem = {_Category,_Level}) ->
    try ets:insert(?verbosity_table, Elem) of
	_ ->
	    ok
    catch
	_:Reason ->
	    {error,Reason}
    end.
	
get_verbosity(Category) ->
    try ets:lookup(?verbosity_table, Category) of
	[{Category,Level}] -> 
	    Level;
	_ ->
	    undefined
    catch
	_:Reason ->
	    {error,Reason}
    end.

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
	{{match_delete_testdata,{Key1,Key2}},From} ->
	    %% handles keys with 2 elements
	    TestData1 =
		lists:filter(fun({Key,_}) when not is_tuple(Key) ->
				     true;
				({Key,_}) when tuple_size(Key) =/= 2 ->
				     true;
				({{_,KeyB},_}) when Key1 == '_' ->
				     KeyB =/= Key2; 
				({{KeyA,_},_}) when Key2 == '_' ->
				     KeyA =/= Key1; 
				(_) when Key1 == '_' ; Key2 == '_' ->
				     false;
				(_) ->
				     true
			     end, TestData),
	    return(From,ok),
	    loop(From,TestData1,StartDir);
	{{set_testdata,New = {Key,_Val}},From} ->
	    TestData1 = lists:keydelete(Key,1,TestData),
	    return(From,ok),
	    loop(Mode,[New|TestData1],StartDir);
	{{get_testdata, all}, From} ->
	    return(From, TestData),
	    loop(From, TestData, StartDir);
	{{get_testdata,Key},From} ->
	    case lists:keysearch(Key,1,TestData) of
		{value,{Key,Val}} ->
		    return(From,Val);
		_ ->
		    return(From,undefined)
	    end,
	    loop(From,TestData,StartDir);
	{{update_testdata,Key,Fun,Opts},From} ->
	    TestData1 =
		case lists:keysearch(Key,1,TestData) of
		    {value,{Key,Val}} ->
			try Fun(Val) of
			    '$delete' ->
				return(From,deleted),
				lists:keydelete(Key,1,TestData);
			    NewVal ->
				return(From,NewVal),
				[{Key,NewVal}|lists:keydelete(Key,1,TestData)]
			catch
			    _:Error ->
				return(From,{error,Error}),
				TestData
			end;
		    _ ->
			case lists:member(create,Opts) of
			    true ->
				InitVal = Fun(undefined),
				return(From,InitVal),
				[{Key,InitVal}|TestData];
			    false ->
				return(From,undefined),
				TestData
			end
		end,
	    loop(From,TestData1,StartDir);	    
	{{set_cwd,Dir},From} ->
	    return(From,file:set_cwd(Dir)),
	    loop(From,TestData,StartDir);
	{reset_cwd,From} ->
	    return(From,file:set_cwd(StartDir)),
	    loop(From,TestData,StartDir);
	{get_start_dir,From} ->
	    return(From,StartDir),
	    loop(From,TestData,StartDir);
	{{stop,Info},From} ->
	    test_server_io:reset_state(),
	    {MiscIoName,MiscIoDivider,MiscIoFooter} =
		proplists:get_value(misc_io_log,TestData),
	    {ok,MiscIoFd} = file:open(MiscIoName,
				      [append,{encoding,utf8}]),
	    io:put_chars(MiscIoFd, MiscIoDivider),
	    test_server_io:set_fd(unexpected_io, MiscIoFd),

	    Time = calendar:local_time(),
	    ct_event:sync_notify(#event{name=test_done,
					node=node(),
					data=Time}),
	    Callbacks =
		try ets:lookup_element(?suite_table,
				       ct_hooks,
				       #suite_data.value) of
		    CTHMods -> CTHMods
		catch
		    %% this is because ct_util failed in init
		    error:badarg -> []
		end,
	    ct_hooks:terminate(Callbacks),

	    close_connections(ets:tab2list(?conn_table)),
	    ets:delete(?conn_table),
	    ets:delete(?board_table),
	    ets:delete(?suite_table),
	    ets:delete(?verbosity_table),

	    io:put_chars(MiscIoFd, "\n</pre>\n"++MiscIoFooter),
	    test_server_io:stop([unexpected_io]),
	    test_server_io:finish(),

	    ct_logs:close(Info, StartDir),
	    ct_event:stop(),
	    ct_config:stop(),
	    ct_default_gl:stop(),
	    ok = file:set_cwd(StartDir),
	    return(From, Info);
	{Ref, _Msg} when is_reference(Ref) ->
	    %% This clause is used when doing cast operations.
	    loop(Mode,TestData,StartDir);
	{get_mode,From} ->
	    return(From,Mode),
	    loop(Mode,TestData,StartDir);
	{'EXIT',_Pid,normal} ->
	    loop(Mode,TestData,StartDir);
	{'EXIT',Pid,Reason} ->
	    case ets:lookup(?conn_table,Pid) of
		[#conn{address=A,callback=CB}] ->
		    ErrorStr = io_lib:format("~tp", [Reason]),
		    ErrorHtml = ct_logs:escape_chars(ErrorStr),
		    %% A connection crashed - remove the connection but don't die
		    ct_logs:tc_log_async(ct_error_notify,
					 ?MAX_IMPORTANCE,
					 "CT Error Notification",
					 "Connection process died: "
					 "Pid: ~w, Address: ~tp, "
					 "Callback: ~w\n"
					 "Reason: ~ts\n\n",
					 [Pid,A,CB,ErrorHtml]),
		    catch CB:close(Pid),
		    %% in case CB:close failed to do this:
		    unregister_connection(Pid),
		    loop(Mode,TestData,StartDir);
		_ ->
		    %% Let process crash in case of error, this shouldn't happen!
		    io:format("\n\nct_util_server got EXIT "
			      "from ~w: ~tp\n\n", [Pid,Reason]),
		    ok = file:set_cwd(StartDir),
		    exit(Reason)
	    end
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
    %% If TargetName is a registered alias for a config
    %% variable, use it as reference for the connection,
    %% otherwise use the Handle value.
    TargetRef = 
	case ct_config:get_key_from_name(TargetName) of
	    {ok,_Key} ->
		TargetName;
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
    case ct_config:get_key_from_name(TargetName) of
	{ok,_Key} ->
	    case ets:select(?conn_table,[{#conn{handle='$1',
						targetref=TargetName,
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
%%% @spec get_connection(TargetName,Callback) -> 
%%%                                {ok,Connection} | {error,Reason}
%%%      TargetName = ct:target_name()
%%%      Callback = atom()
%%%      Connection = {Handle,Address}
%%%      Handle = term()
%%%      Address = term()
%%%
%%% @doc Return the connection for <code>Callback</code> on the
%%% given target (<code>TargetName</code>).
get_connection(TargetName,Callback) ->
    %% check that TargetName is a registered alias
    case ct_config:get_key_from_name(TargetName) of
	{ok,_Key} ->
	    case ets:select(?conn_table,[{#conn{handle='$1',
						address='$2',
						targetref=TargetName,
						callback=Callback},
					  [],
					  [{{'$1','$2'}}]}]) of
		[Result] ->
		    {ok,Result};
		[] ->
		    {error,no_registered_connection}
	    end;
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @spec get_connections(ConnPid) -> 
%%%                                {ok,Connections} | {error,Reason}
%%%      Connections = [Connection]
%%%      Connection = {TargetName,Handle,Callback,Address}
%%%      TargetName = ct:target_name() | undefined
%%%      Handle = term()
%%%      Callback = atom()
%%%      Address = term()
%%%
%%% @doc Get data for all connections associated with a particular
%%%      connection pid (see Callback:init/3).
get_connections(ConnPid) ->
    Conns = ets:tab2list(?conn_table),
    lists:flatmap(fun(#conn{targetref=TargetName,
			    handle=Handle,
			    callback=Callback,
			    address=Address}) ->
			  case ct_gen_conn:get_conn_pid(Handle) of
			      ConnPid when is_atom(TargetName) ->
				  [{TargetName,Handle,
				    Callback,Address}];
			      ConnPid ->
				  [{undefined,Handle,
				   Callback,Address}];
			      _ ->
				  []
			  end
		  end, Conns).

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:get_target_name/1
get_target_name(Handle) ->
    case ets:select(?conn_table,[{#conn{handle=Handle,targetref='$1',_='_'},
				  [],
				  ['$1']}]) of
	[TargetName] when is_atom(TargetName) ->
	    {ok,TargetName};
	_ ->
	    {error,{unknown_connection,Handle}}
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
    Protocols = [telnet,ftp,rpc,snmp,ssh],
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
    is_silenced(Conn, infinity).

is_silenced(Conn, Timeout) ->
    case get_testdata(silent_connections, Timeout) of
	Conns when is_list(Conns) ->
	    case lists:keysearch(Conn,1,Conns) of
		{value,{Conn,true}} ->
		    true;
		_ ->
		    false
	    end;
	Error = {error,_} ->
	    Error;
	_ ->
	    false
    end.
    
reset_silent_connections() ->
    delete_testdata(silent_connections).
    

%%%-----------------------------------------------------------------
%%% @spec stop(Info) -> ok
%%%
%%% @doc Stop the ct_util_server and close all existing connections
%%% (tool-internal use only).
%%%
%%% @see ct
stop(Info) ->
    case whereis(ct_util_server) of
	undefined -> 
	    ok;
	CtUtilPid ->
	    Ref = monitor(process, CtUtilPid),
	    call({stop,Info}),
	    receive
		{'DOWN',Ref,_,_,_} -> ok
	    end
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
    {Heading, Rest} = get_headings(Data),
    Lines = parse_row(Rest,[],size(Heading)),
    {Heading,Lines}.

get_headings(["|" ++ Headings | Rest]) ->
    {remove_space(string:lexemes(Headings, "|"),[]), Rest};
get_headings([_ | Rest]) ->
    get_headings(Rest);
get_headings([]) ->
    {{},[]}.

parse_row(["|" ++ _ = Row | T], Rows, NumCols) when NumCols > 1 ->
    case string:lexemes(Row, "|") of
	Values when length(Values) =:= NumCols ->
	    parse_row(T,[remove_space(Values,[])|Rows], NumCols);
	Values when length(Values) < NumCols ->
	    parse_row([Row ++"\n"++ hd(T) | tl(T)], Rows, NumCols)
    end;
parse_row(["|" ++ X = Row | T], Rows, 1 = NumCols) ->
    case string:find(X, [$|]) of
	nomatch ->
	    parse_row([Row ++"\n"++hd(T) | tl(T)], Rows, NumCols);
	_Else ->
	    parse_row(T, [remove_space(string:lexemes(Row,"|"),[])|Rows],
		      NumCols)
    end;
parse_row([_Skip | T], Rows, NumCols) ->
    parse_row(T, Rows, NumCols);
parse_row([], Rows, _NumCols) ->
    lists:reverse(Rows).

remove_space([Str|Rest],Acc) ->
    remove_space(Rest,[string:trim(string:trim(Str,both,[$\s]),both,[$'])|Acc]);
remove_space([],Acc) ->
    list_to_tuple(lists:reverse(Acc)).


%%%-----------------------------------------------------------------
%%% @spec 
%%%
%%% @doc
is_test_dir(Dir) ->
    lists:last(string:lexemes(filename:basename(Dir), "_")) == "test".

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
			io:format(?def_gl,
				  "~nWARNING! Deprecated function: ~w:sequences/0.~n"
				  "         Use group with sequence property instead.~n",[Mod])
		end
	end,
    lists:foreach(Warn, Suites),
    ok.

%%%-----------------------------------------------------------------
%%% @spec
%%%
%%% @doc
mark_process() ->
    mark_process(system).

mark_process(Type) ->
    put(ct_process_type, Type).

is_marked(Pid) ->
    is_marked(Pid, system).

is_marked(Pid, Type) ->
    case process_info(Pid, dictionary) of
        {dictionary,List} ->
            Type == proplists:get_value(ct_process_type, List);
        undefined ->
            false
    end.

remaining_test_procs() ->
    Procs = processes(),
    {SharedGL,OtherGLs,Procs2} =
        lists:foldl(
          fun(Pid, ProcTypes = {Shared,Other,Procs1}) ->
                  case is_marked(Pid, group_leader) of
                      true ->
                          if not is_pid(Shared) ->
                                  case test_server_io:get_gl(true) of
                                      Pid ->
                                          {Pid,Other,
                                           lists:delete(Pid,Procs1)};
                                      _ ->
                                          {Shared,[Pid|Other],Procs1}
                                  end;
                             true ->          % SharedGL already found
                                  {Shared,[Pid|Other],Procs1}
                          end;
                      false ->
                          case is_marked(Pid) of
                              true ->
                                  {Shared,Other,lists:delete(Pid,Procs1)};
                              false ->
                                  ProcTypes
                          end
                  end
          end, {undefined,[],Procs}, Procs),

    AllGLs = [SharedGL | OtherGLs],
    TestProcs =
        lists:flatmap(fun(Pid) ->
                              case process_info(Pid, group_leader) of
                                  {group_leader,GL} ->
                                      case lists:member(GL, AllGLs) of
                                          true  -> [{Pid,GL}];
                                          false -> []
                                      end;
                                  undefined ->
                                      []
                              end
                      end, Procs2),
    {TestProcs, SharedGL, OtherGLs}.

%%%-----------------------------------------------------------------
%%% @spec
%%%
%%% @doc
get_profile_data() ->
    get_profile_data(all).

get_profile_data(KeyOrStartDir) ->
    if is_atom(KeyOrStartDir) ->
	    get_profile_data(KeyOrStartDir, get_start_dir());
       is_list(KeyOrStartDir) ->
	    get_profile_data(all, KeyOrStartDir)
    end.

get_profile_data(Key, StartDir) ->
    Profile = case application:get_env(common_test, profile) of
		  {ok,undefined} -> default;
		  {ok,Prof}      -> Prof;
		  _              -> default
	      end,
    get_profile_data(Profile, Key, StartDir).

get_profile_data(Profile, Key, StartDir) ->
    File = case Profile of
	       default ->
		   ?ct_profile_file;
	       _ when is_list(Profile) ->
		   ?ct_profile_file ++ "." ++ Profile;
	       _ when is_atom(Profile) ->
		   ?ct_profile_file ++ "." ++ atom_to_list(Profile)
	   end,
    FullNameWD = filename:join(StartDir, File),
    {WhichFile,Result} =
	case file:consult(FullNameWD) of
	    {error,enoent} ->
		case init:get_argument(home) of
		    {ok,[[HomeDir]]} ->
			FullNameHome = filename:join(HomeDir, File),
			{FullNameHome,file:consult(FullNameHome)};
		    _ ->
			{File,{error,enoent}}
		end;
	    Consulted ->
		{FullNameWD,Consulted}
	end,
    case Result of
	{error,enoent} when Profile /= default ->
	    io:format(?def_gl, "~nERROR! Missing profile file ~tp~n", [File]),
	    undefined;
	{error,enoent} when Profile == default ->
	    undefined;
	{error,Reason} ->
	    io:format(?def_gl,"~nERROR! Error in profile file ~tp: ~tp~n",
		      [WhichFile,Reason]),
	    undefined;
	{ok,Data} ->
	    Data1 = case Data of
			[List] when is_list(List) ->
			    List;
			_ when is_list(Data) ->
			    Data;
			_ ->
			    io:format(?def_gl,
				      "~nERROR! Invalid profile data in ~tp~n",
				      [WhichFile]),
			    []
		    end,
	    if Key == all ->
		    Data1;
	       true ->
		    proplists:get_value(Key, Data)
	    end
    end.

%%%-----------------------------------------------------------------
%%% Internal functions
call(Msg) ->
    call(Msg, infinity).

call(Msg, Timeout) ->
    case {self(),whereis(ct_util_server)} of
	{_,undefined} ->
	    {error,ct_util_server_not_running};
	{Pid,Pid} ->
	    %% the caller is ct_util_server, which must
	    %% be a mistake
	    {error,bad_invocation};
	{Self,Pid} ->
	    MRef = erlang:monitor(process, Pid),
	    Ref = make_ref(),
	    ct_util_server ! {Msg,{Self,Ref}},
	    receive
		{Ref, Result} -> 
		    erlang:demonitor(MRef, [flush]),
		    Result;
		{'DOWN',MRef,process,_,Reason}  -> 
		    {error,{ct_util_server_down,Reason}}
	    after
		Timeout -> {error,timeout}
	    end
    end.

return({To,Ref},Result) ->
    To ! {Ref, Result},
    ok.

cast(Msg) ->
    ct_util_server ! {Msg, {ct_util_server, make_ref()}},
    ok.

seconds(T) ->
    test_server:seconds(T).

abs_name("/") ->
    "/";
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

open_url(iexplore, Args, URL) ->
    {ok,R} = win32reg:open([read]),
    ok = win32reg:change_key(R,"applications\\iexplore.exe\\shell\\open\\command"),
    _ = case win32reg:values(R) of
	{ok, Paths} ->
	    Path = proplists:get_value(default, Paths),
	    [Cmd | _] = string:lexemes(Path, "%"),
	    Cmd1 = Cmd ++ " " ++ Args ++ " " ++ URL,
	    io:format(?def_gl, "~nOpening ~ts with command:~n  ~ts~n", [URL,Cmd1]),
	    open_port({spawn,Cmd1}, []);
	_ ->
	    io:format(?def_gl, "~nNo path to iexplore.exe~n",[])
    end,
    win32reg:close(R),
    ok;

open_url(Prog, Args, URL) ->
    ProgStr = if is_atom(Prog) -> atom_to_list(Prog);
		 is_list(Prog) -> Prog
	      end,
    Cmd = ProgStr ++ " " ++ Args ++ " " ++ URL,
    io:format(?def_gl, "~nOpening ~ts with command:~n  ~ts~n", [URL,Cmd]),
    open_port({spawn,Cmd},[]),
    ok.
