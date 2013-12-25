%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2006-2009 Richard Carlsson
%% @private
%% @see eunit
%% @doc Text-based frontend for EUnit

-module(eunit_tty).

-behaviour(eunit_listener).

-define(NODEBUG, true).
-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([start/0, start/1]).

-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3,
	 terminate/2]).

-record(state, {verbose = false,
                colored = false,
                indent = 0
               }).

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    St = #state{verbose = proplists:get_bool(verbose, Options),
                colored = proplists:get_bool(colored, Options)},
    put(no_tty, proplists:get_bool(no_tty, Options)),
    receive
	{start, _Reference} ->
	    if St#state.verbose -> print_header();
	       true -> ok
	    end,
	    St
    end.

terminate({ok, Data}, #state{colored = Colored} = St) ->
    Pass = proplists:get_value(pass, Data, 0),
    Fail = proplists:get_value(fail, Data, 0),
    Skip = proplists:get_value(skip, Data, 0),
    Cancel = proplists:get_value(cancel, Data, 0),
    if Fail =:= 0, Skip =:= 0, Cancel =:= 0 ->
            if Pass =:= 0 ->
                    ok;
               % fwrite("  There were no tests to run.\n");
               true ->
                    if St#state.verbose -> print_bar();
                       true -> ok
                    end,
                    case {Pass, Colored} of
                        {1, true} ->
                            fwrite("\e[32;1m  Test passed.\e[m\n");
                        {1, false} ->
                            fwrite("  Test passed.\n");
                        {_, true} ->
                            fwrite("\e[32;1m  All ~w tests passed.\e[m\n", [Pass]);
                        {_, false} ->
                            fwrite("  All ~w tests passed.\n", [Pass])
                    end
            end,
            sync_end(ok);
       true ->
            print_bar(),
            if Colored ->
                    fwrite("  Failed: \e[31;1m~w\e[m.  Skipped: \e[33;1m~w\e[m.  Passed: \e[32;1m~w\e[m.\n", [Fail, Skip, Pass]);
               true ->
                    fwrite("  Failed: ~w.  Skipped: ~w.  Passed: ~w.\n", [Fail, Skip, Pass])
            end,
            if Cancel =/= 0 ->
                    if Colored ->
                            fwrite("\e[33;1mOne or more tests were cancelled.\e[m\n");
                       true ->
                            fwrite("One or more tests were cancelled.\n")
                    end;
               true -> ok
            end,
            sync_end(error)
    end;
terminate({error, Reason}, #state{colored = Colored} = _St) ->
    if Colored ->
	    fwrite("\e[31;1mInternal error:\e[m ~P.\n", [Reason, 25]);
	true ->
	    fwrite("Internal error: ~P.\n", [Reason, 25])
    end,
    sync_end(error).

sync_end(Result) ->
    receive
	{stop, Reference, ReplyTo} ->
	    ReplyTo ! {result, Reference, Result},
	    ok
    end.

print_header() ->
    fwrite("======================== EUnit ========================\n").

print_bar() ->
    fwrite("=======================================================\n").


handle_begin(group, Data, St) ->
    ?debugFmt("handle_begin group ~w", [Data]),
    Desc = proplists:get_value(desc, Data),
    if Desc =/= "", Desc =/= undefined, St#state.verbose ->
	    I = St#state.indent,
	    print_group_start(I, Desc),
	    St#state{indent = I + 1};
       true ->
	    St
    end;
handle_begin(test, Data, St) ->
    ?debugFmt("handle_begin test ~w", [Data]),
    if St#state.verbose -> print_test_begin(St#state.indent, Data, St);
       true -> ok
    end,
    St.

handle_end(group, Data, St) ->
    ?debugFmt("handle_end group ~w", [Data]),
    Desc = proplists:get_value(desc, Data),
    if Desc =/= "", Desc =/= undefined, St#state.verbose ->
	    Time = proplists:get_value(time, Data),
	    I = St#state.indent,
	    print_group_end(I, Time),
	    St#state{indent = I - 1};
       true ->
	    St
    end;
handle_end(test, Data, St) ->
    ?debugFmt("handle_end test ~w", [Data]),
    case proplists:get_value(status, Data) of
	ok ->
	    if St#state.verbose -> print_test_end(Data, St);
	       true -> ok
	    end,
	    St;
	Status ->
	    if St#state.verbose -> ok;
	       true -> print_test_begin(St#state.indent, Data, St)
	    end,
	    print_test_error(Status, Data, St),
	    St
    end.

handle_cancel(group, Data, St) ->
    ?debugFmt("handle_cancel group ~w", [Data]),
    I = St#state.indent,
    case proplists:get_value(reason, Data) of
	undefined ->
	    %% "skipped" message is not interesting here
	    St#state{indent = I - 1};
	Reason ->
	    Desc = proplists:get_value(desc, Data),
	    if Desc =/= "", Desc =/= undefined, St#state.verbose ->
		    print_group_cancel(I, Reason, St);
	       true ->
		    print_group_start(I, Desc),
		    print_group_cancel(I, Reason, St)
	    end,
	    St#state{indent = I - 1}
    end;
handle_cancel(test, Data, St) ->
    ?debugFmt("handle_cancel test ~w", [Data]),
    if St#state.verbose -> ok;
       true -> print_test_begin(St#state.indent, Data, St)
    end,
    print_test_cancel(proplists:get_value(reason, Data), St),
    St.


indent(N) when is_integer(N), N >= 1 ->
    fwrite(lists:duplicate(N * 2, $\s));
indent(_N) ->
    ok.

print_group_start(I, Desc) ->
    indent(I),
    fwrite("~s\n", [Desc]).

print_group_end(I, Time) ->
    if Time > 0 ->
            indent(I),
            fwrite("[done in ~.3f s]\n", [Time/1000]);
       true ->
            ok
    end.

print_test_begin(I, Data, #state{colored = Colored}) ->
    Desc = proplists:get_value(desc, Data),
    Line = proplists:get_value(line, Data, 0),
    indent(I),
    L = if Line =:= 0 -> "";
	   true -> io_lib:fwrite(":~w", [Line])
	end,
    D = if Desc =:= "" ; Desc =:= undefined -> "";
	   true -> io_lib:fwrite(" (~s)", [Desc])
	end,
    case proplists:get_value(source, Data) of
	{Module, Name, _Arity} ->
            if
                Colored ->
                    fwrite("\e[36m~s\e[32m~s \e[33m~s\e[m~s...", [Module, L, Name, D]);
                true ->
                    fwrite("~s~s ~s~s...", [Module, L, Name, D])
            end;
	_ ->
            if
                Colored ->
                    fwrite("~s~s...", [L, D]);
                true ->
                    fwrite("\e[32m~s\e[m~s...", [L, D])
            end
    end.

print_test_end(Data, #state{colored = Colored}) ->
    Time = proplists:get_value(time, Data, 0),
    T = if Time > 0 -> io_lib:fwrite("[~.3f s] ", [Time/1000]);
	   true -> ""
	end,
    if
        Colored ->
            fwrite("~s\e[32;1mok\e[m\n", [T]);
        true ->
            fwrite("~sok\n", [T])
    end.

print_test_error({error, Exception}, Data, #state{colored = Colored}) ->
    Output = proplists:get_value(output, Data),
    if
        Colored ->
            fwrite("\e[31;1mfailed\e[m\n~s", [eunit_lib:format_exception(Exception)]);
        true ->
            fwrite("*failed*\n~s", [eunit_lib:format_exception(Exception)])
    end,
    case Output of
	<<>> ->
	    fwrite("\n\n");
	<<Text:800/binary, _:1/binary, _/binary>> ->
	    fwrite("  output:<<\"~s\">>...\n\n", [Text]);
	_ ->
	    fwrite("  output:<<\"~s\">>\n\n", [Output])
    end;
print_test_error({skipped, Reason}, _, #state{colored = Colored}) ->
    if
        Colored ->
            fwrite("\e[33;1mdid not run\e[m\n::~s\n", [format_skipped(Reason)]);
        true ->
            fwrite("*did not run*\n::~s\n", [format_skipped(Reason)])
    end.

format_skipped({module_not_found, M}) ->
    io_lib:fwrite("missing module: ~w", [M]);
format_skipped({no_such_function, {M,F,A}}) ->
    io_lib:fwrite("no such function: ~w:~w/~w", [M,F,A]).

print_test_cancel(Reason, St) ->
    fwrite(format_cancel(Reason, St)).

print_group_cancel(_I, {blame, _}, _St) ->
    ok;
print_group_cancel(I, Reason, St) ->
    indent(I),
    fwrite(format_cancel(Reason, St)).

format_cancel(undefined, #state{colored = Colored}) ->
    if
        Colored ->
            "\e[33;1mskipped\e[m\n";
        true ->
            "*skipped*\n"
    end;
format_cancel(timeout, #state{colored = Colored}) ->
    if
        Colored ->
            "\e[31;1mtimed out\e[m\n";
        true ->
            "*timed out*\n"
    end;
format_cancel({startup, Reason}, #state{colored = Colored}) ->
    if
        Colored ->
            io_lib:fwrite("\e[31;1mcould not start test process\e[m\n::~P\n\n", [Reason, 15]);
        true ->
            io_lib:fwrite("*could not start test process*\n::~P\n\n", [Reason, 15])
    end;
format_cancel({blame, _SubId}, #state{colored = Colored}) ->
    if
        Colored ->
            "\e[31;1mcancelled because of subtask\e[m\n";
        true ->
            "*cancelled because of subtask*\n"
    end;
format_cancel({exit, Reason}, #state{colored = Colored}) ->
    if
        Colored ->
            io_lib:fwrite("\e[31;1munexpected termination of test process\e[m\n::~P\n\n", [Reason, 15]);
        true ->
            io_lib:fwrite("*unexpected termination of test process*\n::~P\n\n", [Reason, 15])
    end;
format_cancel({abort, Reason}, _St) ->
    eunit_lib:format_error(Reason).

fwrite(String) ->
    fwrite(String, []).

fwrite(String, Args) ->
    case get(no_tty) of
        false -> io:fwrite(String, Args);
        true -> ok
    end.
