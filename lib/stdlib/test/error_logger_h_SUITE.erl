%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015. All Rights Reserved.
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
-module(error_logger_h_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2]).
-export([logfile/1,tty/1]).

%% Event handler exports.
-export([init/1,handle_event/2,terminate/2]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [logfile,tty].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

logfile(Config) ->
    PrivDir = ?config(priv_dir, Config),
    LogDir = filename:join(PrivDir, ?MODULE),
    Log = filename:join(LogDir, "logfile.log"),
    ok = filelib:ensure_dir(Log),

    Ev = event_templates(),

    error_logger:logfile({open,Log}),
    gen_events(Ev),
    error_logger:logfile(close),
    analyse_events(Log, Ev, []),

    Pa = "-pa " ++ filename:dirname(code:which(?MODULE)),
    {ok,Node} = start_node(logfile, Pa),
    error_logger:logfile({open,Log}),
    ok = rpc:call(Node, erlang, apply, [fun gen_events/1,[Ev]]),
    AtNode = iolist_to_binary(["** at node ",atom_to_list(Node)," **"]),
    error_logger:logfile(close),
    analyse_events(Log, Ev, [AtNode]),

    test_server:stop_node(Node),

    cleanup(Log),
    ok.

tty(Config) ->
    PrivDir = ?config(priv_dir, Config),
    LogDir = filename:join(PrivDir, ?MODULE),
    Log = filename:join(LogDir, "tty.log"),
    ok = filelib:ensure_dir(Log),

    Ev = event_templates(),

    tty_log_open(Log),
    gen_events(Ev),
    tty_log_close(),
    analyse_events(Log, Ev, []),

    Pa = "-pa " ++ filename:dirname(code:which(?MODULE)),
    {ok,Node} = start_node(logfile, Pa),
    tty_log_open(Log),
    ok = rpc:call(Node, erlang, apply, [fun gen_events/1,[Ev]]),
    tty_log_close(),
    AtNode = iolist_to_binary(["** at node ",atom_to_list(Node)," **"]),
    analyse_events(Log, Ev, [AtNode]),

    test_server:stop_node(Node),

    cleanup(Log),
    ok.

tty_log_open(Log) ->
    {ok,Fd} = file:open(Log, [write]),
    error_logger:add_report_handler(?MODULE, Fd),
    Fd.

tty_log_close() ->
    error_logger:delete_report_handler(?MODULE),
    ok.

event_templates() ->
    [{error_msg,["Pure error string\n",[]]},
     {error_msg,["Pure error string with error ~p\n",[]]},
     {error_msg,["Error string with ~p\n", [format]]},
     {error_msg,["Error string with bad format ~p\n", []]},

     {error_report,[error_atom]},
     {error_report,["error string"]},
     {error_report,[[{error_tag,value},error_value]]},

     {info_msg,["Pure info string\n",[]]},
     {info_msg,["Pure info string with error ~p\n",[]]},
     {info_msg,["Pure string with ~p\n", [format]]},
     {info_msg,["Pure string with bad format ~p\n", []]},

     {info_report,[info_atom]},
     {info_report,["info string"]},
     {info_report,[[{info_tag,value},info_value]]},

     {warning_msg,["Pure warning string\n",[]]},
     {warning_msg,["Pure warning string with error ~p\n",[]]},
     {warning_msg,["Warning string with ~p\n", [format]]},
     {warning_msg,["Warning string with bad format ~p\n", []]},

     {warning_report,[warning_atom]},
     {warning_report,["warning string"]},
     {warning_report,[[{warning_tag,value},warning_value]]}].

gen_events(Ev) ->
    io:format("node = ~p\n", [node()]),
    io:format("group leader = ~p\n", [group_leader()]),
    io:format("~p\n", [gen_event:which_handlers(error_logger)]),
    call_error_logger(Ev),

    {Pid,Ref} = spawn_monitor(fun() -> error(ouch) end),
    receive
	{'DOWN',Ref,process,Pid,_} ->
	    ok
    end,

    %% The following calls with a custom type will be ignored.
    error_logger:error_report(ignored, value),
    error_logger:warning_report(ignored, value),
    error_logger:info_report(ignored, value),
    receive after 100 -> ok end,
    ok.

analyse_events(Log, Ev, AtNode) ->
    {ok,Bin} = file:read_file(Log),

    io:format("*** Contents of log file ***\n\n~s\n", [Bin]),

    Lines = binary:split(Bin, <<"\n">>, [global,trim_all]),
    io:format("~p\n", [Lines]),

    Rest = match_output(Ev, Lines, AtNode),
    io:format("~p\n", [Rest]),

    [] = match_emulator_error(Rest),
    ok.


call_error_logger([{F,Args}|T]) ->
    apply(error_logger, F, Args),
    call_error_logger(T);
call_error_logger([]) -> ok.


match_emulator_error([Head,Second,Third,_|Lines]) ->
    match_head(<<"ERROR">>, Head),
    {match,[{0,_}]} = re:run(Second,
			   "^Error in process <\\d+[.]\\d+[.]\\d+> on "
			   "node [^ ]* with exit value:"),
    {match,[{0,_}]} = re:run(Third, "^[{]ouch,"),
    Lines.

match_output([Item|T], Lines0, AtNode) ->
    try match_item(Item, Lines0, AtNode) of
	Lines ->
	    match_output(T, Lines, AtNode)
    catch
	C:E ->
	    Stk = erlang:get_stacktrace(),
	    io:format("ITEM: ~p", [Item]),
	    io:format("LINES: ~p", [Lines0]),
	    erlang:raise(C, E, Stk)
    end;
match_output([], Lines, _) -> Lines.

match_item(Item, Lines, AtNode) ->
    case item_type(Item) of
	{msg,Head,Args} ->
	    match_format(Head, Args, Lines, AtNode);
	{report,Head,Args} ->
	    match_term(Head, Args, Lines, AtNode)
    end.

item_type({error_msg,Args}) ->
    {msg,<<"ERROR">>,Args};
item_type({info_msg,Args}) ->
    {msg,<<"INFO">>,Args};
item_type({warning_msg,Args}) ->
    {msg,<<"WARNING">>,Args};
item_type({error_report,Args}) ->
    {report,<<"ERROR">>,Args};
item_type({info_report,Args}) ->
    {report,<<"INFO">>,Args};
item_type({warning_report,Args}) ->
    {report,<<"WARNING">>,Args}.

match_format(Tag, [Format,Args], [Head|Lines], AtNode) ->
    match_head(Tag, Head),
    Bin = try io_lib:format(Format, Args) of
	      Str ->
		  iolist_to_binary(Str)
    catch
	_:_ ->
	    S = io_lib:format("ERROR: ~p - ~p~n", [Format,Args]),
	    iolist_to_binary(S)
    end,
    Expected0 = binary:split(Bin, <<"\n">>, [trim_all]),
    Expected = Expected0 ++ AtNode,
    match_term_lines(Expected, Lines).

match_term(Tag, [Arg], [Head|Lines], AtNode) ->
    match_head(Tag, Head),
    Expected0 = match_term_get_expected(Arg),
    Expected = Expected0 ++ AtNode,
    match_term_lines(Expected, Lines).

match_term_get_expected(List) when is_list(List) ->
    try iolist_to_binary(List) of
	Bin ->
	    [Bin]
    catch
	_:_ ->
	    format_rep(List)
    end;
match_term_get_expected(Term) ->
    S = io_lib:format("~p", [Term]),
    Bin = iolist_to_binary(S),
    [Bin].

format_rep([{Tag,Data}|Rep]) ->
    [iolist_to_binary(io_lib:format("    ~p: ~p", [Tag,Data]))|
     format_rep(Rep)];
format_rep([Other|Rep]) ->
    [iolist_to_binary(io_lib:format("    ~p", [Other]))|
     format_rep(Rep)];
format_rep([]) -> [].

match_term_lines([Line|T], [Line|Lines]) ->
    match_term_lines(T, Lines);
match_term_lines([], Lines) -> Lines.

match_head(Tag, Head) ->
    Re = <<"^=",Tag/binary,
	   " REPORT==== \\d\\d?-[A-Z][a-z][a-z]-\\d{4}::"
	   "\\d\\d:\\d\\d:\\d\\d ===$">>,
    {match,_} = re:run(Head, Re).

start_node(Name, Args) ->
    case test_server:start_node(Name, slave, [{args,Args}]) of
	{ok,Node} ->
	    {ok,Node};
	Error  ->
	    test_server:fail(Error)
    end.

cleanup(File) ->
    %% The point of this test case is not to test file operations.
    %% Therefore ignore any failures.
    case file:delete(File) of
	ok ->
	    ok;
	{error,Error1} ->
	    io:format("file:delete(~s) failed with error ~p",
		      [File,Error1])
    end,
    Dir = filename:dirname(File),
    case file:del_dir(Dir) of
	ok ->
	    ok;
	{error,Error2} ->
	    io:format("file:del_dir(~s) failed with error ~p",
		      [Dir,Error2])
    end,
    ok.


%%%
%%% Our own event handler. There is no way to intercept the output
%%% from error_logger_tty_h, but we can use the same code by
%%% calling error_logger_tty_h:write_event/2.
%%%

init(Fd) ->
    {ok,Fd}.

handle_event(Event, Fd) ->
    case error_logger_tty_h:write_event(tag_event(Event), io_lib) of
	ok ->
	    ok;
	Str when is_list(Str) ->
	    io:put_chars(Fd, Str)
    end,
    {ok,Fd}.

terminate(_Reason, Fd) ->
    ok = file:close(Fd),
    [].

tag_event(Event) ->
    {erlang:universaltime(),Event}.
