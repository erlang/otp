%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2014. All Rights Reserved.
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

-module(observer_SUITE).
-include_lib("test_server/include/test_server.hrl").
-include_lib("wx/include/wx.hrl").
-include_lib("observer/src/observer_tv.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0]).
-export([init_per_testcase/2, end_per_testcase/2,
	 init_per_group/2, end_per_group/2,
	 init_per_suite/1, end_per_suite/1
	]).

%% Test cases
-export([app_file/1, appup_file/1,
	 basic/1, process_win/1, table_win/1
	]).

%% Default timetrap timeout (set in init_per_testcase)
-define(default_timeout, ?t:minutes(1)).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [app_file, appup_file, {group, gui}].

groups() ->
    [{gui, [],
      [basic
      %% , process_win, table_win
      ]
     }].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

init_per_group(gui, Config) ->
    try
	case os:type() of
	    {unix,darwin} ->
		exit("Can not test on MacOSX");
	    {unix, _} ->
		io:format("DISPLAY ~s~n", [os:getenv("DISPLAY")]),
		case ct:get_config(xserver, none) of
		    none -> ignore;
		    Server -> os:putenv("DISPLAY", Server)
		end;
	    _ -> ignore
	end,
	wx:new(),
	wx:destroy(),
	Config
    catch
	_:undef ->
	    {skipped, "No wx compiled for this platform"};
	_:Reason ->
	    SkipReason = io_lib:format("Start wx failed: ~p", [Reason]),
	    {skipped, lists:flatten(SkipReason)}
    end.
end_per_group(_, _) ->
    ok.

app_file(suite) ->
    [];
app_file(doc) ->
    ["Testing .app file"];
app_file(Config) when is_list(Config) ->
    ?line ok = ?t:app_test(observer),
    ok.

%% Testing .appup file
appup_file(Config) when is_list(Config) ->
    ok = ?t:appup_test(observer).

-define(DBG(Foo), io:format("~p: ~p~n",[?LINE, catch Foo])).

basic(suite) -> [];
basic(doc) -> [""];
basic(Config) when is_list(Config) ->
    ok = observer:start(),
    Notebook = setup_whitebox_testing(),

    io:format("Notebook ~p~n",[Notebook]),
    Count = wxNotebook:getPageCount(Notebook),
    true = Count >= 6,
    0 = wxNotebook:getSelection(Notebook),
    timer:sleep(500),
    Check = fun(N, TestMore) ->
		    ok = wxNotebook:advanceSelection(Notebook),
		    TestMore andalso
			test_page(wxNotebook:getPageText(Notebook, N),
				  wxNotebook:getCurrentPage(Notebook)),
		    timer:sleep(200)
	    end,
    %% Just verify that we can toogle trough all pages
    [_|_] = [Check(N, false) || N <- lists:seq(1, Count)],
    %% Cause it to resize
    Frame = get_top_level_parent(Notebook),
    {W,H} = wxWindow:getSize(Frame),
    wxWindow:setSize(Frame, W+10, H+10),
    [_|_] = [Check(N, true) || N <- lists:seq(1, Count)],

    ok = observer:stop().

test_page("Load Charts" ++ _, _Window) ->
    %% Just let it display some info and hopefully it doesn't crash
    timer:sleep(2000),
    ok;
test_page("Applications" ++ _, _Window) ->
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
    timer:sleep(500),  %% Give it time to refresh
    Active = get_active(),
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

test_page("Table" ++ _, _Window) ->
    Tables = [ets:new(list_to_atom("Test-" ++ [C]), [public]) || C <- lists:seq($A, $Z)],
    Active = get_active(),
    Active ! refresh_interval,
    ChangeSort = fun(N) ->
			 FakeEv = #wx{event=#wxList{type=command_list_col_click, col=N}},
			 Active ! FakeEv,
			 timer:sleep(200)
		 end,
    [ChangeSort(N) || N <- lists:seq(1,5) ++ [0]],
    timer:sleep(1000),
    Table = lists:nth(3, Tables),
    ets:insert(Table, [{N,100-N} || N <- lists:seq(1,100)]),
    Focus = #wx{event=#wxList{type=command_list_item_selected, itemIndex=2}},
    Active ! Focus,
    Activate = #wx{event=#wxList{type=command_list_item_activated, itemIndex=2}},
    Active ! Activate,

    Info = 407, %% whitebox...
    Active ! #wx{id=Info},
    timer:sleep(1000),
    ok;

test_page(Title, Window) ->
    io:format("Page ~p: ~p~n", [Title, Window]),
    %% Just let it display some info and hopefully it doesn't crash
    timer:sleep(1000),
    ok.


process_win(suite) -> [];
process_win(doc) -> [""];
process_win(Config) when is_list(Config) ->
    ok = observer:start(),
    ObserverNB = setup_whitebox_testing(),
    Parent = get_top_level_parent(ObserverNB),
    Frame = observer_procinfo:start(self(), Parent, self()),
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
    ok.

table_win(suite) -> [];
table_win(doc) -> [""];
table_win(Config) when is_list(Config) ->
    Tables = [ets:new(list_to_atom("Test-" ++ [C]), [public]) || C <- lists:seq($A, $Z)],
    Table = lists:nth(3, Tables),
    ets:insert(Table, [{N,100-N} || N <- lists:seq(1,100)]),
    ok = observer:start(),
    Notebook = setup_whitebox_testing(),
    Parent = get_top_level_parent(Notebook),
    TObj = observer_tv_table:start_link(Parent, [{node,node()}, {type,ets}, {table,#tab{name=foo, id=Table}}]),
    %% Modal can not test edit..
    %% TPid = wx_object:get_pid(TObj),
    %% TPid ! #wx{event=#wxList{type=command_list_item_activated, itemIndex=12}},
    timer:sleep(2000),
    wx_object:get_pid(TObj) ! #wx{event=#wxClose{type=close_window}},
    observer:stop(),
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
