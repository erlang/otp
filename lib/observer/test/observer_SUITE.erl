%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2023. All Rights Reserved.
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

-module(observer_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("wx/include/wx.hrl").
-include_lib("observer/src/observer_tv.hrl").

-define(ID_LOGVIEW, 5).

%% Test server specific exports
-export([all/0, suite/0,groups/0]).
-export([init_per_testcase/2, end_per_testcase/2,
	 init_per_group/2, end_per_group/2,
	 init_per_suite/1, end_per_suite/1
	]).

%% Test cases
-export([app_file/1, appup_file/1,
	 basic/1, process_win/1, table_win/1,
         port_win_when_tab_not_initiated/1,
         blocking_start/1, remote_node/1
	]).

%% Default timetrap timeout (set in init_per_testcase)
-define(default_timeout, test_server:minutes(2)).

-define(SECS(__S__), timer:seconds(__S__)).

-define(P(F),    print(F)).
-define(P(F, A), print(F, A)).


suite() -> [{timetrap, {minutes, 5}},
            {ct_hooks,[ts_install_cth]}].

all() ->
    [app_file, appup_file, {group, gui}].

groups() ->
    [{gui, [],
      [basic,
       process_win,
       table_win,
       port_win_when_tab_not_initiated,
       blocking_start,
       remote_node
      ]
     }].


init_per_suite(Config) ->
    ?P("init_per_suite -> entry with"
       "~n   Config: ~p", [Config]),
    Config.

end_per_suite(Config) ->
    ?P("end_per_suite -> entry with"
       "~n   Config: ~p", [Config]),
    ok.


%% init_per_testcase(basic = Case, Config) ->
%%     ?P("init_per_testcase(~w) -> entry with"
%%        "~n   Config: ~p", [Case, Config]),
%%     {ok, Host} = inet:gethostname(),
%%     {ok, IFL}  = inet:getifaddrs(),
%%     init_per_testcase2(Case, [{host, Host}, {ifl, IFL} | Config]);
init_per_testcase(Case, Config) ->
    ?P("init_per_testcase(~w) -> entry with"
       "~n   Config: ~p", [Case, Config]),
    init_per_testcase2(Case, Config).

init_per_testcase2(Case, Config) ->
    ?P("init_per_testcase(~w) -> entry", [Case]),
    Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].

%% end_per_testcase(basic = Case, Config) ->
%%     ?P("end_per_testcase(~w) -> entry with"
%%        "~n   Config: ~p", [Case, Config]),
%%     case lists:keysearch(tc_status, 1, Config) of
%%         {value, {tc_status, ok}} ->
%%             ?P("end_per_testcase(~w) -> successful", [Case]),
%%             ok;
%%         {value, _} ->
%%             ?P("end_per_testcase(~w) -> try ensure observer stopped", [Case]),
%%             ensure_observer_stopped();
%%         _ ->
%%             ?P("end_per_testcase(~w) -> nop status", [Case]),
%%             ok
%%     end,
%%     end_per_testcase2(Case, Config);
end_per_testcase(Case, Config) ->
    ?P("end_per_testcase(~w) -> entry with"
       "~n   Config: ~p", [Case, Config]),
    end_per_testcase2(Case, Config).

end_per_testcase2(Case, Config) ->
    ?P("end_per_testcase2(~w) -> entry - try cancel watchdog", [Case]),
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ?P("end_per_testcase2(~w) -> done", [Case]),
    ok.


init_per_group(gui = Group, Config) ->
    ?P("init_per_group(~w) -> entry with"
       "~n   Config: ~p", [Group, Config]),
    try
	case os:type() of
	    {unix,darwin} ->
		?P("init_per_group(~w) -> skip", [Group]),
		exit("Can not test on MacOSX");
            {unix,sunos} ->
                exit("Skip on sunos, for now");
	    {unix, _} ->
                Display = os:getenv("DISPLAY"),
		?P("init_per_group(~w) -> DISPLAY ~s",
                   [Group, Display]),
		case ct:get_config(xserver, none) of
		    none -> ignore;
                    Display -> ok;
		    Server ->
                        os:putenv("DISPLAY", Server), %% Might work if new node is spawned
                        io:format("Config sets other x-server than the DISPLAY\n"
                                  "the DISPLAY variable must be set when starting erlang")
		end;
	    _ -> ignore
	end,
	wx:new(),
	wx:destroy(),
	Config
    catch
	_:undef ->
            ?P("init_per_group(~w) -> undef", [Group]),
	    {skipped, "No wx compiled for this platform"};
	C:Reason:S ->
            ?P("init_per_group(~w) -> "
               "~n   Class:  ~p"
               "~n   Reason: ~p"
               "~n   Stack:  ~p", [Group, C, Reason, S]),
	    SkipReason = io_lib:format("Start wx failed: ~p", [Reason]),
	    {skipped, lists:flatten(SkipReason)}
    end.

end_per_group(_Group, _Config) ->
    ?P("end_per_group(~w) -> entry with"
       "~n   Config: ~p", [_Group, _Config]),
    ok.


app_file(suite) ->
    [];
app_file(doc) ->
    ["Testing .app file"];
app_file(Config) when is_list(Config) ->
    ok = test_server:app_test(observer),
    ok.


%% Testing .appup file
appup_file(Config) when is_list(Config) ->
    ok = test_server:appup_test(observer).


basic(suite) -> [];
basic(doc) -> [""];
basic(Config) when is_list(Config) ->
    ?P("basic -> entry with"
       "~n   Config: ~p", [Config]),

    %% Start these before
    ?P("basic -> try wx new"),
    wx:new(),
    ?P("basic -> try wx destroy"),
    wx:destroy(),
    timer:start(),
    ?P("basic -> try start distribution"),
    {foo, node@machine} ! dummy_msg,  %% start distribution stuff
    %% Otherwise ever lasting servers gets added to procs

    %% It takes some time for all the procs to start,
    %% so give it some time.
    %% Note that this is a problem *only* if this test suite
    %% is run *on its own*. If all the observer suite(s) are run
    %% (ex: ts:run(observer, [batch]), then this problem does
    %% not occcur.
    timer:sleep(5000),

    ?P("basic -> procs before"),
    ProcsBefore = processes(),
    ProcInfoBefore = [{P,process_info(P)} || P <- ProcsBefore],
    NumProcsBefore = length(ProcsBefore),

    ?P("basic -> try start observer"),
    ok = observer:start(),
    Notebook = setup_whitebox_testing(),
    ?P("basic -> Notebook ~p", [Notebook]),

    Count = wxNotebook:getPageCount(Notebook),
    ?P("basic -> page count ~p", [Count]),
    true = Count >= 7,
    ?P("basic -> check selection (=0)"),
    0 = wxNotebook:getSelection(Notebook),
    ?P("basic -> wait some time..."),

    timer:sleep(500),
    Check = fun(N, TestMore) ->
		    TestMore andalso
			test_page(wxNotebook:getPageText(Notebook, N),
				  wxNotebook:getCurrentPage(Notebook)),
		    timer:sleep(200),
		    ok = wxNotebook:advanceSelection(Notebook)
	    end,
    %% Just verify that we can toggle through all pages
    ?P("basic -> try verify that we can toggle through all pages"),
    [_|_] = [Check(N, false) || N <- lists:seq(1, Count)],
    %% Cause it to resize
    ?P("basic -> try resize"),
    Frame = get_top_level_parent(Notebook),
    {W,H} = wxWindow:getSize(Frame),
    wxWindow:setSize(Frame, W+10, H+10),
    ?P("basic -> try verify that we resized"),
    [_|_] = [Check(N, true) || N <- lists:seq(0, Count-1)],

    ?P("basic -> try stop observer"),
    ok = observer:stop(),
    timer:sleep(2000), %% stop is async
    ?P("basic -> try verify observer stopped"),
    ProcsAfter = processes(),
    NumProcsAfter = length(ProcsAfter),
    if NumProcsAfter =/= NumProcsBefore ->
            BeforeNotAfter = ProcsBefore -- ProcsAfter,
            AfterNotBefore = ProcsAfter -- ProcsBefore,
            ?P("basic -> *not* fully stopped:"
               "~n   Number of Procs before: ~p"
               "~n   Number of Procs after:  ~p",
               [NumProcsBefore, NumProcsAfter]),
	    ct:log("Before but not after:~n~p~n",
		   [[{P,I} || {P,I} <- ProcInfoBefore,
                              lists:member(P,BeforeNotAfter)]]),
	    ct:log("After but not before:~n~p~n",
		   [[{P,process_info(P)} || P <- AfterNotBefore]]),
	    ensure_observer_stopped(),
	    ct:fail("leaking processes");
       true ->
	    ok
    end,
    ?P("ensure observer stopped"),
    ensure_observer_stopped(?SECS(2)),
    ?P("basic -> done"),
    ok.

test_page("Load Charts" ++ _, _Window) ->
    ?P("test_page(load charts) -> entry"),
    %% Just let it display some info and hopefully it doesn't crash
    timer:sleep(2000),
    ok;
test_page("Applications" ++ _, _Window) ->
    ?P("test_page(applications) -> entry"),
    ok = application:start(mnesia),
    timer:sleep(1000),  %% Give it time to refresh
    Active = get_active(),
    FakeEv = #wx{event=#wxCommand{type=command_listbox_selected, cmdString="mnesia"}},
    Active ! FakeEv,
    timer:sleep(1000),  %% Give it time to refresh
    ok = application:stop(mnesia),
    timer:sleep(1000),  %% Give it time to refresh
    ok;

test_page("Processes" ++ _, _Window) ->
    ?P("test_page(processes) -> entry"),
    timer:sleep(500),  %% Give it time to refresh
    Active = get_active(),
    Active ! refresh_interval,
    ChangeSort = fun(N) ->
			 FakeEv = #wx{event=#wxList{type=command_list_col_click, col=N}},
			 Active ! FakeEv,
			 timer:sleep(200)
		 end,
    [ChangeSort(N) || N <- lists:seq(1,5) ++ [0]],
    Focus = #wx{event=#wxList{type=command_list_item_focused, itemIndex=2}},
    Active ! Focus,
    Activate = #wx{event=#wxList{type=command_list_item_activated}},
    Active ! Activate,
    timer:sleep(1000),  %% Give it time to refresh
    ok;

test_page("Ports" ++ _, _Window) ->
    ?P("test_page(ports) -> entry"),
    timer:sleep(500),  %% Give it time to refresh
    Active = get_active(),
    Active ! refresh_interval,
    ChangeSort = fun(N) ->
			 FakeEv = #wx{event=#wxList{type=command_list_col_click, col=N}},
			 Active ! FakeEv,
			 timer:sleep(200)
		 end,
    [ChangeSort(N) || N <- lists:seq(1,4) ++ [0]],
    Activate = #wx{event=#wxList{type=command_list_item_activated,
				 itemIndex=2}},
    Active ! Activate,
    timer:sleep(1000),  %% Give it time to refresh
    ok;

test_page("Sockets" ++ _, _Window) ->
    ?P("test_page(sockets) -> entry"),
    %% (maybe) We cannot count on having any 'sockets', so create some.
    %% We don't actually need more then a 4 open *existing* sockets.
    Sockets = sock_create([{[any],         {inet,  stream,    tcp}},
                           {[any],         {inet,  stream,    tcp}},
                           {[any],         {inet,  dgram,     udp}},
                           {[any],         {inet,  dgram,     udp}},
                           {[ipv6],        {inet6, stream,    tcp}},
                           {[ipv6],        {inet6, dgram,     udp}},
                           {[sctp],        {inet,  seqpacket, sctp}},
                           {[ipv6, sctp],  {inet6, seqpacket, sctp}}]),
    timer:sleep(500),  %% Give it time to refresh
    Active = get_active(),
    Active ! refresh_interval,
    %% And this stuff only works if we actually have some (>= 4) sockets
    %% so check that we do
    try socket:number_of() of
        NumSocks when NumSocks >= 4 ->
            ChangeSort =
                fun(N) ->
                        FakeEv = #wx{event=#wxList{type=command_list_col_click, col=N}},
                        Active ! FakeEv,
                        timer:sleep(200)
                end,
            [ChangeSort(N) || N <- lists:seq(1,4) ++ [0]],
            Activate = #wx{event=#wxList{type=command_list_item_activated,
                                         itemIndex=2}},
            Active ! Activate,
            timer:sleep(1000);  %% Give it time to refresh
        _ ->
            ?P("not enough sockets - skip list-col-click test")
    catch
        _:_:_ ->
            ?P("'socket' not supported")
    end,
    %% (maybe) Cleanup
    sock_close(Sockets),
    ok;

test_page("Table" ++ _, _Window) ->
    ?P("test_page(table) -> entry"),
    Tables = [ets:new(list_to_atom("Test-" ++ [C]), [public]) || C <- lists:seq($A, $Z)],
    Table = lists:nth(3, Tables),
    ets:insert(Table, [{N,100-N} || N <- lists:seq(1,100)]),

    Active = get_active(),
    Active ! refresh_interval,
    ChangeSort = fun(N) ->
			 FakeEv = #wx{event=#wxList{type=command_list_col_click, col=N}},
			 Active ! FakeEv,
			 timer:sleep(200)
		 end,
    [ChangeSort(N) || N <- lists:seq(1,5) ++ [0]],
    timer:sleep(1000),
    Focus = #wx{event=#wxList{type=command_list_item_selected, itemIndex=2}},
    Active ! Focus,
    Activate = #wx{event=#wxList{type=command_list_item_activated, itemIndex=2}},
    Active ! Activate,

    Info = 407, %% whitebox...
    Active ! #wx{id=Info},
    timer:sleep(1000),
    ok;

test_page("Trace Overview" ++ _, _Window) ->
    ?P("test_page(trace overview) -> entry"),
    timer:sleep(500),  %% Give it time to refresh
    Active = get_active(),
    Active ! refresh_interval,
    timer:sleep(1000),  %% Give it time to refresh
    ok;

test_page(Title, Window) ->
    ?P("test_page -> entry with"
       "~n   Title:  ~p"
       "~n   Window: ~p", [Title, Window]),
    %% Just let it display some info and hopefully it doesn't crash
    timer:sleep(1000),
    ok.


process_win(suite) -> [];
process_win(doc) -> [""];
process_win(Config) when is_list(Config) ->
    ?P("process_win -> entry"),
    % Stop SASL if already started
    SaslStart = case whereis(sasl_sup) of
                  undefined -> false;
                  _         -> application:stop(sasl),
                               true
                end,
    % Define custom sasl and log_mf_h app vars
    Privdir=?config(priv_dir,Config),
    application:set_env(sasl, sasl_error_logger, tty),
    application:set_env(sasl, error_logger_mf_dir, Privdir),
    application:set_env(sasl, error_logger_mf_maxbytes, 1000),
    application:set_env(sasl, error_logger_mf_maxfiles, 5),
    application:start(sasl),
    ok = observer:start(),
    ObserverNB = setup_whitebox_testing(),
    Parent = get_top_level_parent(ObserverNB),
    % Activate log view
    whereis(observer) ! #wx{id = ?ID_LOGVIEW, event = #wxCommand{type = command_menu_selected}},
    timer:sleep(1000),
    % Process window tests (use sasl_sup for a non empty Log tab)
    Frame = observer_procinfo:start(whereis(sasl_sup), Parent, self()),
    PIPid = wx_object:get_pid(Frame),
    PIPid ! {get_debug_info, self()},
    Notebook = receive {procinfo_debug, NB} -> NB end,
    Count = wxNotebook:getPageCount(Notebook),
    Check = fun(_N) ->
		    ok = wxNotebook:advanceSelection(Notebook),
		    timer:sleep(400)
	    end,
    [_|_] = [Check(N) || N <- lists:seq(1, Count)],
    PIPid ! #wx{event=#wxClose{type=close_window}},
    observer:stop(),
    application:stop(sasl),
    case SaslStart of
         true  -> application:start(sasl);
         false -> ok
    end,
    ?P("ensure observer stopped"),
    ensure_observer_stopped(?SECS(2)),
    ?P("process_win -> done"),
    ok.

table_win(suite) -> [];
table_win(doc) -> [""];
table_win(Config) when is_list(Config) ->
    ?P("table_win -> entry"),
    Tables = [ets:new(list_to_atom("Test-" ++ [C]), [public]) || C <- lists:seq($A, $Z)],
    Table = lists:nth(3, Tables),
    ets:insert(Table, [{N,100-N} || N <- lists:seq(1,100)]),
    ok = observer:start(),
    Notebook = setup_whitebox_testing(),
    Parent = get_top_level_parent(Notebook),
    TObj = observer_tv_table:start_link(Parent, [{node,node()}, {type,ets}, {table,#tab{name=foo, id=Table}}]),
    %% Modal cannot test edit..
    %% TPid = wx_object:get_pid(TObj),
    %% TPid ! #wx{event=#wxList{type=command_list_item_activated, itemIndex=12}},
    timer:sleep(3000),
    wx_object:get_pid(TObj) ! #wx{event=#wxClose{type=close_window}},
    observer:stop(),
    ?P("ensure observer stopped"),
    ensure_observer_stopped(?SECS(3)),
    ?P("table_win -> done"),
    ok.

remote_node(_Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
    ok = observer:start(Node),
    timer:sleep(1000),
    Node = observer_wx:get_active_node(),
    observer:stop(),
    ensure_observer_stopped(?SECS(3)),
    peer:stop(Peer).

blocking_start(_Config) ->
    {Pid, SpawnerRef} = spawn_monitor(fun observer:start_and_wait/0),
    timer:sleep(1000),
    ObserverRef = monitor(process, observer),
    receive
        {'DOWN', ObserverRef, _, _, Reason} ->
            error({observer_stopped_unexpectedly, Reason});
        {'DOWN', SpawnerRef, _, _, Reason} ->
            error({spawner_stopped_unexpectedly, Reason})
    after
        500 ->
            ok
    end,
    observer:stop(),
    ensure_observer_stopped(?SECS(3)),
    receive
        {'DOWN', ObserverRef, _, _, _} ->
            ok
    after
        500 ->
            error(observer_should_have_stopped)
    end,
    false = erlang:is_process_alive(Pid),
    ok.

%% Test PR-1296/OTP-14151
%% Clicking a link to a port before the port tab has been activated the
%% first time crashes observer.
port_win_when_tab_not_initiated(_Config) ->
    ?P("port_win_when_tab_not_initiated -> entry"),
    {ok,Port} = gen_tcp:listen(0,[]),
    ok = observer:start(),
    _Notebook = setup_whitebox_testing(),
    observer ! {open_link,erlang:port_to_list(Port)},
    timer:sleep(1000),
    observer:stop(),
    ?P("ensure observer stopped"),
    ensure_observer_stopped(?SECS(3)),
    ?P("port_win_when_tab_not_initiated -> done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_top_level_parent(Window) ->
    Parent =  wxWindow:getParent(Window),
    case wx:is_null(Parent) of
	true -> Window;
	false -> get_top_level_parent(Parent)
    end.

setup_whitebox_testing() ->
    %% So that if we die observer exists
    link(whereis(observer)),
    {Env, Notebook, _Active} = get_observer_debug(),
    wx:set_env(Env),
    Notebook.

get_active() ->
    {_, _, Active} = get_observer_debug(),
    Active.

get_observer_debug() ->
    observer ! {get_debug_info, self()},
    receive
	{observer_debug, Env, Notebook, Active} ->
	    {Env, Notebook, Active}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ensure_observer_stopped() ->
    ensure_observer_stopped(0).

ensure_observer_stopped(T) when is_integer(T) andalso (T > 0) ->
    case erlang:whereis(observer) of
	undefined ->
	    ?P("observer *not* running"),
	    ok;
	Pid when is_pid(Pid) ->
	    ?P("observer process still running: "
	       "~n   ~p", [erlang:process_info(Pid)]),
	    ct:sleep(?SECS(1)),
	    ensure_observer_stopped(T - 1000),
	    ok
    end;
ensure_observer_stopped(_) ->
    case erlang:whereis(observer) of
	undefined ->
	    ?P("observer *not* running"),
	    ok;
	Pid when is_pid(Pid) ->
	    ?P("observer process still running: kill"
	       "~n   ~p", [erlang:process_info(Pid)]),
	    exit(kill, Pid),
	    ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sock_create(Conds) when is_list(Conds) ->
    sock_create(Conds, []).

sock_create([], Acc) ->
    Acc;
sock_create([{Features, Args}|Conds], Acc) ->
    case sock_cond_action(Features, fun() -> sock_open(Args) end) of
        undefined ->
            sock_create(Conds, Acc);
        Socket ->
            sock_create(Conds, [Socket|Acc])
    end.

sock_cond_action(Features, Action) ->
    sock_cond_action(Features, Action, undefined).

sock_cond_action(Features, Action, Default) ->
    case sock_is_supported(Features) of
        true ->
            Action();
        false ->
            Default
    end.


sock_is_supported([]) ->
    true;
sock_is_supported([Feature|Features]) ->
    sock_is_supported(Feature) andalso sock_is_supported(Features);
sock_is_supported(any) ->
    try socket:supports() of
        Features when is_list(Features) ->
            true
    catch
        _:_:_ ->
            false
    end;
sock_is_supported(Feature) ->
    try socket:is_supported(Feature)
    catch
        _:_:_ ->
            false
    end.


sock_open({Domain, Type, Proto}) ->
    case socket:open(Domain, Type, Proto) of
        {ok, Socket} ->
            ?P("created socket: ~w, ~w, ~w", [Domain, Type, Proto]),
            Socket;
        {error, _} ->
            undefined
    end.


sock_close([]) ->
    ok;
sock_close([Socket|Sockets]) ->
    sock_close(Socket),
    sock_close(Sockets);
sock_close(undefined) ->
    ok;
sock_close(Socket) ->
    (catch socket:close(Socket)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% f(F, A) ->
%%     lists:flatten(io_lib:format(F, A)).

formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp({_N1, _N2, N3} = TS) ->
    {_Date, Time}   = calendar:now_to_local_time(TS),
    {Hour, Min, Sec} = Time,
    FormatTS = io_lib:format("~.2.0w:~.2.0w:~.2.0w.~.3.0w",
                             [Hour, Min, Sec, N3 div 1000]),  
    lists:flatten(FormatTS).

print(F) ->
    print(F, []).

print(F, A) ->
    io:format("~s ~p " ++ F ++ "~n", [formated_timestamp(), self() | A]).

