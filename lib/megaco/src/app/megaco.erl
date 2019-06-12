%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2019. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose : Main API for Megaco/H.248 protocol stack
%%----------------------------------------------------------------------

-module(megaco).

%%-----------------------------------------------------------------
%% Public interface
%%-----------------------------------------------------------------

-export([
         start/0,
         stop/0,
        
         start_user/2,
         stop_user/1,

	 info/0, 
         user_info/1, user_info/2,
         update_user_info/3,
         conn_info/1, conn_info/2,
         update_conn_info/3,
         system_info/0, system_info/1,

         connect/4, connect/5, 
         disconnect/2,

         call/3,
         cast/3,
         cancel/2,
         process_received_message/4, process_received_message/5,
         receive_message/4, receive_message/5,

	 encode_actions/3,

	 token_tag2string/1, token_tag2string/2, token_tag2string/3, 

	 parse_digit_map/1,
	 eval_digit_map/1,
	 eval_digit_map/2,
	 report_digit_event/2,
	 test_digit_event/2,

	 encode_binary_term_id/2,
	 decode_binary_term_id/2,

	 encode_sdp/1,
	 decode_sdp/1, 

	 versions1/0, versions2/0, 
	 print_version_info/0, print_version_info/1, 
	 ms/0, nc/0, nc/1, ni/0, ni/1,

	 enable_trace/2, disable_trace/0, set_trace/1,

	 report_event/4, report_event/5,

	 test_request/5,
	 test_reply/5
        ]).

-export([
	 get_stats/0, get_stats/1, get_stats/2,
	 reset_stats/0, reset_stats/1
	]).

%% Deprecated
-export([format_versions/1]).

%% Internal
-export([format_timestamp/1]).

%% This is for XREF
-deprecated([{format_versions, 1, eventually}]).

-export_type([
              void/0
             ]).

-type void() :: term().

-include("megaco_internal.hrl").


%%-----------------------------------------------------------------
%% Starts the Megaco application
%%-----------------------------------------------------------------

start() ->
    application:start(?APPLICATION).


%%-----------------------------------------------------------------
%% Stops the Megaco application
%%-----------------------------------------------------------------

stop() ->
    application:stop(?APPLICATION).


%%-----------------------------------------------------------------
%% Initial configuration of a user
%%-----------------------------------------------------------------

start_user(UserMid, Config) ->
    megaco_config:start_user(UserMid, Config).


%%-----------------------------------------------------------------
%% Delete the configuration of a user
%%-----------------------------------------------------------------

stop_user(UserMid) ->
    megaco_config:stop_user(UserMid).


%%-----------------------------------------------------------------
%% Lookup user information
%%-----------------------------------------------------------------

user_info(UserMid) ->
    [{requests, user_info(UserMid, requests)},
     {replies,  user_info(UserMid, replies)} | user_info(UserMid, all)].

user_info(UserMid, requests) ->
    megaco_messenger:which_requests(UserMid);
user_info(UserMid, replies) ->
    megaco_messenger:which_replies(UserMid);
user_info(UserMid, Item) ->
    megaco_config:user_info(UserMid, Item).


%%-----------------------------------------------------------------
%% Update information about a user
%%-----------------------------------------------------------------

update_user_info(UserMid, Item, Value) ->
    megaco_config:update_user_info(UserMid, Item, Value).


%%-----------------------------------------------------------------
%% Lookup information about an active connection
%%-----------------------------------------------------------------

conn_info(ConnHandle) ->
    [{requests, conn_info(ConnHandle, requests)},
     {replies,  conn_info(ConnHandle, replies)} | conn_info(ConnHandle, all)].

conn_info(ConnHandle, requests) ->
    megaco_messenger:which_requests(ConnHandle);
conn_info(ConnHandle, replies) ->
    megaco_messenger:which_replies(ConnHandle);
conn_info(ConnHandle, Item) ->
    megaco_config:conn_info(ConnHandle, Item).


%%-----------------------------------------------------------------
%% Update information about an active connection
%%-----------------------------------------------------------------

update_conn_info(ConnHandle, Item, Value) ->
    megaco_config:update_conn_info(ConnHandle, Item, Value).


%%-----------------------------------------------------------------
%% All information for the application
%%-----------------------------------------------------------------

info() ->
    Stats = 
	case get_stats() of
	    {ok, Statistics} ->
		Statistics;
	    _ ->
		[]
	end,
    SysInfo = system_info(),
    [{statistics, Stats} | info(SysInfo)].

info(SysInfo) ->
    info(SysInfo, []).

info([], Acc) ->
    lists:reverse(Acc);
info([{connections, Conns} | SysInfo], Acc) ->
    Conns2 = extend_conns_info(Conns),
    info(SysInfo, [{connections, Conns2} | Acc]);
info([{users, Users} | SysInfo], Acc) ->
    Users2 = extend_users_info(Users),
    info(SysInfo, [{users, Users2} | Acc]);
info([Info | SysInfo], Acc) ->
    info(SysInfo, [Info | Acc]).

extend_conns_info(Conns) ->
    extend_conns_info(Conns, []).

extend_conns_info([], Acc) ->
    lists:reverse(Acc);
extend_conns_info([Conn | Conns], Acc) ->
    ConnInfo = conn_info(Conn),
    extend_conns_info(Conns, [{Conn, ConnInfo} | Acc]).

extend_users_info(Users) ->
    extend_users_info(Users, []).

extend_users_info([], Acc) ->
    lists:reverse(Acc);
extend_users_info([User | Users], Acc) ->
    UserInfo = user_info(User),
    extend_users_info(Users, [{User, UserInfo} | Acc]).


%%-----------------------------------------------------------------
%% Lookup system information
%%-----------------------------------------------------------------

system_info_items() ->
    [
     text_config, 
     connections, 
     users, 
     n_active_requests, 
     n_active_replies, 
     n_active_connections,
     pending_counters
    ].

system_info() ->
    [{Item, system_info(Item)} || Item <- system_info_items()].

system_info(Item) ->
    megaco_config:system_info(Item).


%%-----------------------------------------------------------------
%% Establish a "virtual" connection
%%-----------------------------------------------------------------

connect(ReceiveHandle, RemoteMid, SendHandle, ControlPid) ->
    megaco_messenger:connect(ReceiveHandle, RemoteMid, SendHandle, ControlPid).

connect(ReceiveHandle, RemoteMid, SendHandle, ControlPid, Extra) 
  when (Extra =/= ?default_user_callback_extra) ->
    megaco_messenger:connect(ReceiveHandle, RemoteMid, SendHandle, 
			     ControlPid, Extra).


%%-----------------------------------------------------------------
%% Tear down a "virtual" connection
%%-----------------------------------------------------------------

disconnect(ConnHandle, Reason) ->
    megaco_messenger:disconnect(ConnHandle, {user_disconnect, Reason}).


%%-----------------------------------------------------------------
%% Sends a transaction request and waits for a reply
%%-----------------------------------------------------------------

call(ConnHandle, ActionRequests, Options) ->
    megaco_messenger:call(ConnHandle, ActionRequests, Options).


%%-----------------------------------------------------------------
%% Sends a transaction request but does NOT wait for a reply
%%-----------------------------------------------------------------

cast(ConnHandle, ActionRequests, Options) ->
    megaco_messenger:cast(ConnHandle, ActionRequests, Options).


%%-----------------------------------------------------------------
%% Test the validity of the actions
%%-----------------------------------------------------------------
    
test_request(ConnHandle, Version, EncodingMod, EncodingConfig, 
	     ActionRequests) ->
    megaco_messenger:test_request(ConnHandle, ActionRequests, 
				  Version, EncodingMod, EncodingConfig).

%% This tests the actual_reply() type of return from the 
%% handle_trans_request function.
%% 
test_reply(ConnHandle, Version, EncodingMod, EncodingConfig, 
	   Reply) ->
    megaco_messenger:test_reply(ConnHandle, Version, 
				EncodingMod, EncodingConfig, Reply).

%%-----------------------------------------------------------------
%% Func: get_stats/0, get_stats/1, get_stats/2
%% Description: Retreive statistics (counters) for TCP
%%-----------------------------------------------------------------
get_stats() ->
    megaco_messenger:get_stats().

get_stats(SendHandle) ->
    megaco_messenger:get_stats(SendHandle).

get_stats(SendHandle, Counter) ->
    megaco_messenger:get_stats(SendHandle, Counter).


%%-----------------------------------------------------------------
%% Func: reset_stats/0, reaet_stats/1
%% Description: Reset statistics (counters) for TCP
%%-----------------------------------------------------------------
reset_stats() ->
    megaco_messenger:reset_stats().

reset_stats(SendHandle) ->
    megaco_messenger:reset_stats(SendHandle).


%%-----------------------------------------------------------------
%% Cancel all outstanding messages for this connection
%%-----------------------------------------------------------------

cancel(ConnHandle, Reason) ->
    megaco_messenger:cancel(ConnHandle, {user_cancel, Reason}).


%%-----------------------------------------------------------------
%% Process a received message
%%-----------------------------------------------------------------

process_received_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) ->
    megaco_messenger:process_received_message(ReceiveHandle, ControlPid, 
					      SendHandle, BinMsg).

process_received_message(ReceiveHandle, ControlPid, SendHandle, BinMsg, Extra) ->
    megaco_messenger:process_received_message(ReceiveHandle, ControlPid, 
					      SendHandle, BinMsg, 
					      Extra).

receive_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) ->
    megaco_messenger:receive_message(ReceiveHandle, ControlPid, 
				     SendHandle, BinMsg).

receive_message(ReceiveHandle, ControlPid, SendHandle, BinMsg, Extra) ->
    megaco_messenger:receive_message(ReceiveHandle, ControlPid, 
				     SendHandle, BinMsg,
				     Extra).


%%-----------------------------------------------------------------
%% Encode the actions list for one or more transactions.
%%-----------------------------------------------------------------

encode_actions(ConnHandle, ActionRequests, Options) ->
    megaco_messenger:encode_actions(ConnHandle, ActionRequests, Options).


%%-----------------------------------------------------------------
%% Convert the (token) tags found in a decoded message into a 
%% printable string.
%%-----------------------------------------------------------------

token_tag2string(Tag) ->
    token_tag2string(Tag, pretty).

token_tag2string(Tag, pretty) ->
    token_tag2string(Tag, megaco_pretty_text_encoder);
token_tag2string(Tag, compact) ->
    token_tag2string(Tag, megaco_compact_text_encoder);
token_tag2string(Tag, Mod) when is_atom(Tag) and is_atom(Mod) ->
    Mod:token_tag2string(Tag).

token_tag2string(Tag, pretty, V) ->
    token_tag2string(Tag, megaco_pretty_text_encoder, V);
token_tag2string(Tag, compact, V) ->
    token_tag2string(Tag, megaco_compact_text_encoder, V);
token_tag2string(Tag, Mod, V) when is_atom(Tag) and is_atom(Mod) ->
    Mod:token_tag2string(Tag, V).


%%-----------------------------------------------------------------
%% Parses a digit map body
%%-----------------------------------------------------------------

parse_digit_map(DigitMapBody) ->
    megaco_digit_map:parse(DigitMapBody).


%%-----------------------------------------------------------------
%% Collect digit map letters according to the digit map
%%-----------------------------------------------------------------

eval_digit_map(DigitMap) ->
    megaco_digit_map:eval(DigitMap).

eval_digit_map(DigitMap, Timers) ->
    megaco_digit_map:eval(DigitMap, Timers).


%%-----------------------------------------------------------------
%% Send one or more events to event collector process
%%-----------------------------------------------------------------

report_digit_event(DigitMapEvalPid, Event) ->
    megaco_digit_map:report(DigitMapEvalPid, Event).


%%-----------------------------------------------------------------
%% Feed digit map collector with events and return the result
%%-----------------------------------------------------------------

test_digit_event(DigitMap, Events) ->
    megaco_digit_map:test(DigitMap, Events).


%%-----------------------------------------------------------------
%% encode_binary_term_id(Config, MegacoTermId) ->
%% 
%%   {ok, TerminationId} | {error, Reason}
%%
%% Encode the Megaco internal form of a termination id (a
%% megaco_term_id record) into ASN.1'1 internal form of a termination
%% id (a 'TerminationId' record).
%% %%-----------------------------------------------------------------

encode_binary_term_id(Config, TermId) ->
    megaco_binary_term_id:encode(Config, TermId).


%%-----------------------------------------------------------------
%% decode_binary_term_id(Config, TerminationId) ->
%% 
%%   {ok, MegacoTermId} | {error, Reason}
%%
%% Decode ASN.1's internal form of a termination id (a 'TerminationId'
%% record) into the Megaco internal form of a termination id (a
%% megaco_term_id record).
%%-----------------------------------------------------------------

decode_binary_term_id(Config, TermId) ->
    megaco_binary_term_id:decode(Config, TermId).


%%-----------------------------------------------------------------
%% encode_sdp(SDP) ->
%% 
%%   {ok, PP} | {error, Reason}
%%
%% Encode a SDP construct into a property parm construct
%%-----------------------------------------------------------------

encode_sdp(SDP) ->
    megaco_sdp:encode(SDP).


%%-----------------------------------------------------------------
%% decode_sdp(PP) ->
%% 
%%   {ok, SDP} | {error, Reason}
%%
%% Decode a property parm construct into a SDP construct
%%-----------------------------------------------------------------

decode_sdp(PP) ->
    megaco_sdp:decode(PP).


%%-----------------------------------------------------------------
%% {ok, Vs} = megaco:versions1(), megaco:format_versions(Vs).

print_version_info() ->
    {ok, Versions} = megaco:versions1(),
    print_version_info(Versions).

print_version_info(Versions) when is_list(Versions) ->
    print_sys_info(Versions),
    print_os_info(Versions),
    print_mods_info(Versions);
print_version_info(BadVersions) ->
    {error, {bad_versions, BadVersions}}.

format_versions(Versions) ->
    print_version_info(Versions).

print_sys_info(Versions) ->
    case key1search(sys_info, Versions) of
	{value, SysInfo} when is_list(SysInfo) ->
	    {value, Arch} = key1search(arch, SysInfo, "Not found"),
	    {value, Ver}  = key1search(ver, SysInfo, "Not found"),
	    io:format("System info: "
		      "~n   Arch: ~s"
		      "~n   Ver:  ~s"
		      "~n", [Arch, Ver]),
	    ok;
	_ ->
	    io:format("System info: Not found~n", []),
	    not_found
    end.
	    
print_os_info(Versions) ->
    case key1search(os_info, Versions) of
	{value, OsInfo} when is_list(OsInfo) ->
	    Fam = 
		case key1search(fam, OsInfo, "Not found") of
		    {value, F} when is_atom(F) ->
			atom_to_list(F);
		    {value, LF} when is_list(LF) ->
			LF;
		    {value, XF} ->
			lists:flatten(io_lib:format("~p", [XF]))
		end,
	    Name = 
		case key1search(name, OsInfo) of
		    {value, N} when is_atom(N) ->
			"[" ++ atom_to_list(N) ++ "]";
		    {value, LN} when is_list(LN) ->
			"[" ++ LN ++ "]";
		    not_found -> 
			""
		end,
	    Ver = 
		case key1search(ver, OsInfo, "Not found") of
		    {value, T} when is_tuple(T) ->
			tversion(T);
		    {value, LV} when is_list(LV) ->
			LV;
		    {value, XV} ->
			lists:flatten(io_lib:format("~p", [XV]))
		end,
	    io:format("OS info: "
		      "~n   Family: ~s ~s"
		      "~n   Ver:    ~s"
		      "~n", [Fam, Name, Ver]),
	    ok;
	_ ->
	    io:format("OS info:     Not found~n", []),
	    not_found
    end.

%% tversion({A, B, C}) ->
%%     lists:flatten(io_lib:format("~w.~w.~w", [A, B, C]));
tversion(T) ->
    L = tuple_to_list(T),
    lversion(L).

lversion([]) ->
    "";
lversion([A]) ->
    integer_to_list(A);
lversion([A|R]) ->
    integer_to_list(A) ++ "." ++ lversion(R).

print_mods_info(Versions) ->
    case key1search(mod_info, Versions) of
	{value, ModsInfo} when is_list(ModsInfo) ->
	    io:format("Module info: ~n", []),
	    lists:foreach(fun print_mod_info/1, ModsInfo);
	_ ->
	    io:format("Module info: Not found~n", []),
	    not_found
    end.

print_mod_info({Module, Info}) ->
    % Maybe a asn1 generated module
    Asn1Vsn = 
	case (catch Module:info()) of
	    AI when is_list(AI) ->
		case (catch key1search(vsn, AI)) of
		    {value, V} when is_atom(V) ->
			atom_to_list(V);
		    _ ->
			"-"
		end;
	    _ ->
		"-"
	end,
    Vsn = 
	case key1search(vsn, Info) of
	    {value, I} when is_integer(I) ->
		integer_to_list(I);
	    _ ->
		"Not found"
	end,
    AppVsn = 
	case key1search(app_vsn, Info) of
	    {value, S1} when is_list(S1) ->
		S1;
	    _ ->
		"Not found"
	end,
    CompVer = 
	case key1search(compiler_version, Info) of
	    {value, S2} when is_list(S2) ->
		S2;
	    _ ->
		"Not found"
	end,
    CompDate = 
	case key1search(compile_time, Info) of
	    {value, {Year, Month, Day, Hour, Min, Sec}} ->
		lists:flatten(
		  io_lib:format("~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", 
				[Year, Month, Day, Hour, Min, Sec]));
	    _ ->
		"Not found"
	end,
    io:format("   ~w:~n"
	      "      Vsn:          ~s~n"
	      "      App vsn:      ~s~n"
	      "      ASN.1 vsn:    ~s~n"
	      "      Compiler ver: ~s~n"
	      "      Compile time: ~s~n", 
	      [Module, Vsn, AppVsn, Asn1Vsn, CompVer, CompDate]),
    ok.
    
    

key1search(Key, Vals) ->
    case lists:keysearch(Key, 1, Vals) of
        {value, {Key, Val}} ->
            {value, Val};
        false ->
            not_found
    end.

key1search(Key, Vals, Def) ->
    case key1search(Key, Vals) of
	not_found ->
	    {value, Def};
	Value ->
	    Value
    end.


%%-----------------------------------------------------------------

versions1() ->
    case ms1() of
	{ok, Mods} ->
	    {ok, version_info(Mods)};
	Error ->
	    Error
    end.

versions2() ->
    case ms2() of
	{ok, Mods} ->
	    {ok, version_info(Mods)};
	Error ->
	    Error
    end.

version_info(Mods) ->
    SysInfo = sys_info(),
    OsInfo  = os_info(),
    ModInfo = [mod_version_info(Mod) || Mod <- Mods],
    [{sys_info, SysInfo}, {os_info, OsInfo}, {mod_info, ModInfo}].
    
mod_version_info(Mod) ->
    Info = Mod:module_info(),
    {value, {attributes, Attr}}   = lists:keysearch(attributes, 1, Info),
    {value, {vsn,        [Vsn]}}  = lists:keysearch(vsn,        1, Attr),
    {value, {app_vsn,    AppVsn}} = lists:keysearch(app_vsn,    1, Attr),
    {value, {compile,    Comp}}   = lists:keysearch(compile,    1, Info),
    {value, {version,    Ver}}    = lists:keysearch(version,    1, Comp),
    {value, {time,       Time}}   = lists:keysearch(time,       1, Comp),
    {Mod, [{vsn,              Vsn}, 
	   {app_vsn,          AppVsn}, 
	   {compiler_version, Ver}, 
	   {compile_time,     Time}]}.

sys_info() ->
    SysArch = string:strip(erlang:system_info(system_architecture),right,$\n),
    SysVer  = string:strip(erlang:system_info(system_version),right,$\n),
    [{arch, SysArch}, {ver, SysVer}].

os_info() ->
    {OsFam, OsName} = os:type(),
    [{fam, OsFam}, {name, OsName}, {ver, os:version()}].
    
ms() ->    
    ms1().

ms1() ->
    App    = ?APPLICATION,
    LibDir = code:lib_dir(App),
    File   = filename:join([LibDir, "ebin", atom_to_list(App) ++ ".app"]),
    case file:consult(File) of
        {ok, [{application, App, AppFile}]} ->
	    case lists:keysearch(modules, 1, AppFile) of
		{value, {modules, Mods}} ->
		    {ok, Mods};
		_ ->
		    {error, {invalid_format, modules}}
	    end;
        Error ->
            {error, {invalid_format, Error}}
    end.

ms2() ->
    application:get_key(?APPLICATION, modules).

nc() ->
    {ok, Mods} = ms(),
    nc(Mods).

nc(all) ->
    application:load(?APPLICATION),
    case application:get_key(?APPLICATION, modules) of
	{ok, Mods} ->
	    application:unload(?APPLICATION),
	    nc(Mods);
	_ ->
	    {error, not_found}
    end;
nc(Mods) when is_list(Mods) ->
    [Mod || Mod <- Mods, ok /= load(Mod, compile)].

ni() -> 
    case ms() of
	{ok, Mods} ->
	    ni(Mods);
	Error ->
	    Error
    end.

ni(all) -> 
    application:load(?APPLICATION),
    case application:get_key(?APPLICATION, modules) of
	{ok, Mods} ->
	    application:unload(?APPLICATION),
	    ni(Mods);
	_ ->
	    {error, not_found}
    end;
ni(Mods) when is_list(Mods) ->
    [Mod || Mod <- Mods, ok /= load(Mod, interpret)].

load(Mod, How) when is_atom(Mod) ->
    case try_load(Mod, How) of
	ok ->
	    ok;
	_ ->
	    io:format( "~n RETRY ~p FROM: ", [Mod]),
	    ModString = atom_to_list(Mod) ++ ".erl",
	    LibDir = code:lib_dir(?APPLICATION),
	    case find_file([LibDir], ModString) of
		{ok, Abs} ->
		    load(Abs, How);
		{error, Reason} ->
		    io:format( " *** ERROR *** ~p~n", [Reason]),
		    {error, Reason}
	    end
    end;
load(Abs, How) ->
    case try_load(Abs, How) of
	ok ->
	    ok;
	{error, Reason} ->
	    io:format( " *** ERROR *** ~p~n", [Reason]),
	    {error, Reason}
    end.

try_load(Mod, How) ->
    io:format( " ~p ", [Mod]),
    Flags = [{d, debug}],
    case How of
	compile ->
	    case catch c:nc(Mod, Flags) of
		{ok, _} -> ok;
		Other   -> {error, Other}
	    end;
	interpret ->
	    case catch int:ni(Mod, Flags) of
		{module, _} -> ok;
		Other       -> {error, Other}
	    end
    end.

find_file([Dir | Dirs], File) ->
    case file:list_dir(Dir) of
	{ok, List} ->
	    case lists:member(File, List) of
		true ->
		    {ok, filename:join([Dir, File])};
		false ->
		    SubDirs = [filename:join([Dir, Sub]) || Sub <- List],
		    case find_file(SubDirs, File) of
			{ok, Abs} ->
			    {ok, Abs};
			{error, _Reason} ->
			    find_file(Dirs, File)
		    end
	    end;
	{error, _Reason} ->
	    find_file(Dirs, File)
    end;
find_file([], File) ->
    {error, {no_such_file, File}}.


%%-----------------------------------------------------------------

%% -----------------------------
%% These functions can be used instead of the et tool for
%% managing trace of the megaco application.

%%-----------------------------------------------------------------
%% enable_trace(Level, Destination) -> void()
%% 
%% Parameters:
%% Level -> max | min | integer()
%% Destination -> File | Port | io | {io, Verbosity} | HandlerSpec
%% File -> string()
%% Port -> integer()
%% Verbosity -> true | false
%% HandlerSpec = {function(), Data}
%% Data = term()
%%
%% Description:
%% This function is used to start tracing at level Level and send
%% the result either to the file File or the port Port. Note that
%% it starts a tracer server.
%% When Destination is the atom io (or the tuple {io, Verbosity}), 
%% all (printable) megaco trace events (trace_ts events which has 
%% Severity withing Limit) will be written to stdout using io:format. 
%% 
%%-----------------------------------------------------------------
enable_trace(Level, File) when is_list(File) ->
    case file:open(File, [write]) of
	{ok, Fd} ->
	    HandleSpec = {fun handle_trace/2, Fd},
	    dbg:tracer(process, HandleSpec),
	    set_trace(Level);
	Err ->
	    Err
    end;
enable_trace(Level, Port) when is_integer(Port) ->
    dbg:tracer(port, dbg:trace_port(ip, Port)),
    set_trace(Level);
enable_trace(Level, io) ->
    HandleSpec = {fun handle_trace/2, standard_io},
    dbg:tracer(process, HandleSpec),
    set_trace(Level);
enable_trace(Level, {Fun, _Data} = HandleSpec) when is_function(Fun) ->
    dbg:tracer(process, HandleSpec),
    set_trace(Level).


%%-----------------------------------------------------------------
%% disable_trace() -> void()
%% 
%% Description:
%% This function is used to stop tracing.
%%-----------------------------------------------------------------
disable_trace() ->
    %% This is to make handle_trace/2 close the output file (if the
    %% event gets there before dbg closes)
    report_event(stop_trace, stop_trace, stop_trace, stop_trace, stop_trace),
    dbg:stop().


%%-----------------------------------------------------------------
%% set_trace(Level) -> void()
%% 
%% Parameters:
%% Level -> max | min | integer()
%%
%% Description:
%% This function is used to change the trace level when tracing has
%% already been started. 
%%-----------------------------------------------------------------
set_trace(Level) ->
    Pat = et_selector:make_pattern({?MODULE, Level}),
    et_selector:change_pattern(Pat).

report_event(DetailLevel, FromTo, Label, Contents) ->
    %% N.B External call
    ?MODULE:report_event(DetailLevel, FromTo, FromTo, Label, Contents).

report_event(_DetailLevel, _From, _To, _Label, _Contents) ->
    hopefully_traced.
    

%% ----------------------------------------------------------------------
%% handle_trace(Event, Verbosity) -> Verbosity
%% 
%% Parameters:
%% Event -> The trace event (only megaco 'trace_ts' events are printed)
%% Verbosity -> max | min | integer() (see Level above)
%%
%% Description:
%% This function is "receive" and print the trace events. 
%% Events are printed if:
%%   - Verbosity is max
%%   - Severity is =< Verbosity (e.g. Severity = 30, and Verbosity = 40)
%% Events are not printed if:
%%   - Verbosity is min
%%   - Severity is > Verbosity
%%-----------------------------------------------------------------

handle_trace(_, closed_file = Fd) ->
    Fd;
handle_trace({trace_ts, _Who, call, 
	      {?MODULE, report_event, 
	       [stop_trace, stop_trace, stop_trace, stop_trace, stop_trace]}, 
	      _Timestamp}, 
	     standard_io = Fd) ->
    Fd;
handle_trace({trace_ts, _Who, call, 
	      {?MODULE, report_event, 
	       [stop_trace, stop_trace, stop_trace, stop_trace, stop_trace]}, 
	      Timestamp}, 
	     Fd) ->
    (catch io:format(Fd, "stop trace at ~s~n", [format_timestamp(Timestamp)])),
    (catch file:close(Fd)),
    closed_file;
handle_trace({trace_ts, Who, call, 
	      {?MODULE, report_event, 
	       [Sev, From, To, Label, Content]}, Timestamp}, 
	     Fd) ->
    (catch print_megaco_trace(Fd, Sev, Who, Timestamp, Label, From, To, Content)),
    Fd;
handle_trace(Event, Fd) ->
    (catch print_trace(Fd, Event)),
    Fd.


print_megaco_trace(Fd, Sev, Who, Timestamp, Label, From, To, Content) ->
    Ts = format_timestamp(Timestamp),
    io:format(Fd, "[megaco trace ~w ~w ~s] ~s "
	      "~n   From:     ~p"
	      "~n   To:       ~p"
	      "~n   Content:  ~p"
	      "~n", 
	      [Sev, Who, Ts, Label, From, To, Content]).
    
print_trace(Fd, {trace, Who, What, Where}) ->
    io:format(Fd, "[trace]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n", [Who, What, Where]);

print_trace(Fd, {trace, Who, What, Where, Extra}) ->
    io:format(Fd, "[trace]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n   Extra: ~p"
              "~n", [Who, What, Where, Extra]);

print_trace(Fd, {trace_ts, Who, What, Where, When}) ->
    Ts = format_timestamp(When),
    io:format(Fd, "[trace ~s]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n", [Ts, Who, What, Where]);

print_trace(Fd, {trace_ts, Who, What, Where, Extra, When}) ->
    Ts = format_timestamp(When),
    io:format(Fd, "[trace ~s]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n   Extra: ~p"
              "~n", [Ts, Who, What, Where, Extra]);

print_trace(Fd, {seq_trace, What, Where}) ->
    io:format(Fd, "[seq trace]"
              "~n   What:       ~p"
              "~n   Where:      ~p"
              "~n", [What, Where]);

print_trace(Fd, {seq_trace, What, Where, When}) ->
    Ts = format_timestamp(When),
    io:format(Fd, "[seq trace ~s]"
              "~n   What:       ~p"
              "~n   Where:      ~p"
              "~n", [Ts, What, Where]);

print_trace(Fd, {drop, Num}) ->
    io:format(Fd, "[drop trace] ~p~n", [Num]);

print_trace(Fd, Trace) ->
    io:format(Fd, "[trace] "
              "~n   ~p"
              "~n", [Trace]).


format_timestamp({_N1, _N2, N3} = Now) ->
    {Date, Time}   = calendar:now_to_datetime(Now),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate = 
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),  
    lists:flatten(FormatDate).


 
