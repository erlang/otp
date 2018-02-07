%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2016. All Rights Reserved.
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
-export([logfile/1,logfile_truncated/1,tty/1,tty_truncated/1]).

%% Event handler exports.
-export([init/1,handle_event/2,terminate/2]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [logfile,logfile_truncated,tty,tty_truncated].

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
    PrivDir = proplists:get_value(priv_dir, Config),
    LogDir = filename:join(PrivDir, ?MODULE),
    Log = filename:join(LogDir, "logfile.log"),
    ok = filelib:ensure_dir(Log),

    Ev = event_templates(),

    do_one_logfile(Log, Ev, unlimited),

    Pa = "-pa " ++ filename:dirname(code:which(?MODULE)),
    {ok,Node} = start_node(logfile, Pa),
    error_logger:logfile({open,Log}),
    ok = rpc:call(Node, erlang, apply, [fun gen_events/1,[Ev]]),
    AtNode = iolist_to_binary(["** at node ",atom_to_list(Node)," **"]),
    error_logger:logfile(close),
    analyse_events(Log, Ev, [AtNode], unlimited),

    %% Make sure that the file_io_server process has been stopped
    [] = lists:filtermap(
           fun(X) ->
                   case {process_info(X, [current_function]),
                         file:pid2name(X)} of
                       {[{current_function, {file_io_server, _, _}}],
                        {ok,P2N = Log}} ->
                           {true, {X, P2N}};
                       _ ->
                           false
                   end
           end, processes()),

    test_server:stop_node(Node),

    cleanup(Log),
    ok.

logfile_truncated(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    LogDir = filename:join(PrivDir, ?MODULE),
    Log = filename:join(LogDir, "logfile_truncated.log"),
    ok = filelib:ensure_dir(Log),

    Ev = event_templates(),

    Depth = 20,
    application:set_env(kernel, error_logger_format_depth, Depth),
    try
	do_one_logfile(Log, Ev, Depth)
    after
	application:unset_env(kernel, error_logger_format_depth)
    end,

    cleanup(Log),
    ok.

do_one_logfile(Log, Ev, Depth) ->
    error_logger:logfile({open,Log}),
    gen_events(Ev),
    error_logger:logfile(close),
    analyse_events(Log, Ev, [], Depth).

tty(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    LogDir = filename:join(PrivDir, ?MODULE),
    Log = filename:join(LogDir, "tty.log"),
    ok = filelib:ensure_dir(Log),

    Ev = event_templates(),

    do_one_tty(Log, Ev, unlimited),

    Pa = "-pa " ++ filename:dirname(code:which(?MODULE)),
    {ok,Node} = start_node(tty, Pa),
    tty_log_open(Log),
    ok = rpc:call(Node, erlang, apply, [fun gen_events/1,[Ev]]),
    tty_log_close(),
    AtNode = iolist_to_binary(["** at node ",atom_to_list(Node)," **"]),
    analyse_events(Log, Ev, [AtNode], unlimited),

    test_server:stop_node(Node),

    cleanup(Log),
    ok.

tty_truncated(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    LogDir = filename:join(PrivDir, ?MODULE),
    Log = filename:join(LogDir, "tty_truncated.log"),
    ok = filelib:ensure_dir(Log),

    Ev = event_templates(),

    Depth = 20,
    application:set_env(kernel, error_logger_format_depth, Depth),
    try
       do_one_tty(Log, Ev, Depth)
    after
	application:unset_env(kernel, error_logger_format_depth)
    end,

    cleanup(Log),
    ok.

do_one_tty(Log, Ev, Depth) ->
    tty_log_open(Log),
    gen_events(Ev),
    tty_log_close(),
    analyse_events(Log, Ev, [], Depth).

tty_log_open(Log) ->
    {ok,Fd} = file:open(Log, [write]),
    Depth = case application:get_env(kernel, error_logger_format_depth) of
		{ok,D} -> D;
		_ -> unlimited
	    end,
    error_logger:add_report_handler(?MODULE, {Fd,Depth,latin1}),
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
     {warning_report,[[{warning_tag,value},warning_value]]},

     %% Bigger terms.
     {error_msg,["fairly big: ~p\n",[lists:seq(1, 128)]]},
     {error_report,[list_to_tuple(lists:seq(1, 100))]},
     {error_report,[lists:seq(32, 126)]},
     {error_report,[[{tag,lists:seq(1, 64)}]]}
    ].

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

analyse_events(Log, Ev, AtNode, Depth) ->
    {ok,Bin} = file:read_file(Log),

    io:format("*** Contents of log file ***\n\n~s\n", [Bin]),

    Lines = binary:split(Bin, <<"\n">>, [global,trim_all]),
    io:format("~p\n", [Lines]),

    Rest = match_output(Ev, Lines, AtNode, Depth),
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

match_output([Item|T], Lines0, AtNode, Depth) ->
    try match_item(Item, Lines0, AtNode, Depth) of
	Lines ->
	    match_output(T, Lines, AtNode, Depth)
    catch
	C:E:Stk ->
	    io:format("ITEM: ~p", [Item]),
	    io:format("LINES: ~p", [Lines0]),
	    erlang:raise(C, E, Stk)
    end;
match_output([], Lines, _, _) -> Lines.

match_item(Item, Lines, AtNode, Depth) ->
    case item_type(Item) of
	{msg,Head,Args} ->
	    match_format(Head, Args, Lines, AtNode, Depth);
	{report,Head,Args} ->
	    match_term(Head, Args, Lines, AtNode, Depth)
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

match_format(Tag, [Format,Args], [Head|Lines], AtNode, Depth) ->
    match_head(Tag, Head),
    Bin = try dl_format(Depth, Format, Args) of
	      Str ->
		  iolist_to_binary(Str)
    catch
	_:_ ->
	    S = dl_format(Depth, "ERROR: ~p - ~p~n", [Format,Args]),
	    iolist_to_binary(S)
    end,
    Expected0 = binary:split(Bin, <<"\n">>, [global,trim]),
    Expected = AtNode ++ Expected0,
    match_term_lines(Expected, Lines).

match_term(Tag, [Arg], [Head|Lines], AtNode, Depth) ->
    match_head(Tag, Head),
    Expected0 = match_term_get_expected(Arg, Depth),
    Expected = AtNode ++ Expected0,
    match_term_lines(Expected, Lines).

match_term_get_expected(List, Depth) when is_list(List) ->
    Bin = try iolist_to_binary(dl_format(Depth, "~s\n", [List])) of
	      Bin0 -> Bin0
	  catch
	      _:_ ->
		  iolist_to_binary(format_rep(List, Depth))
	  end,
    binary:split(Bin, <<"\n">>, [global,trim]);
match_term_get_expected(Term, Depth) ->
    S = dl_format(Depth, "~p\n", [Term]),
    Bin = iolist_to_binary(S),
    binary:split(Bin, <<"\n">>, [global,trim]).

format_rep([{Tag,Data}|Rep], Depth) ->
    [dl_format(Depth, "    ~p: ~p\n", [Tag,Data])|
     format_rep(Rep, Depth)];
format_rep([Other|Rep], Depth) ->
    [dl_format(Depth, "    ~p\n", [Other])|
     format_rep(Rep, Depth)];
format_rep([], _Depth) -> [].

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
	    ct:fail(Error)
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


%% Depth-limited io_lib:format. Intentionally implemented here instead
%% of using io_lib:scan_format/2 to avoid using the same implementation
%% as in the error_logger handlers.

dl_format(unlimited, Format, Args) ->
    io_lib:format(Format, Args);
dl_format(Depth, Format0, Args0) ->
    {Format,Args} = dl_format_1(Format0, Args0, Depth, [], []),
    io_lib:format(Format, Args).

dl_format_1("~p"++Fs, [A|As], Depth, Facc, Acc) ->
    dl_format_1(Fs, As, Depth, [$P,$~|Facc], [Depth,A|Acc]);
dl_format_1("~w"++Fs, [A|As], Depth, Facc, Acc) ->
    dl_format_1(Fs, As, Depth, [$W,$~|Facc], [Depth,A|Acc]);
dl_format_1("~s"++Fs, [A|As], Depth, Facc, Acc) ->
    dl_format_1(Fs, As, Depth, [$s,$~|Facc], [A|Acc]);
dl_format_1([F|Fs], As, Depth, Facc, Aacc) ->
    dl_format_1(Fs, As, Depth, [F|Facc], Aacc);
dl_format_1([], [], _, Facc, Aacc) ->
    {lists:reverse(Facc),lists:reverse(Aacc)}.

%%%
%%% Our own event handler. There is no way to intercept the output
%%% from error_logger_tty_h, but we can use the same code by
%%% calling error_logger_tty_h:write_event/2.
%%%

init({_,_,_}=St) ->
    {ok,St}.

handle_event(Event, {Fd,Depth,Enc}=St) ->
    case error_logger_tty_h:write_event(tag_event(Event), io_lib, {Depth,Enc}) of
	ok ->
	    ok;
	Str when is_list(Str) ->
	    io:put_chars(Fd, Str)
    end,
    {ok,St}.

terminate(_Reason, {Fd,_,_}) ->
    ok = file:close(Fd),
    [].

tag_event(Event) ->
    {erlang:universaltime(),Event}.
