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
%% $Id: eunit_tty.erl 330 2009-03-01 16:28:02Z rcarlsson $ 
%%
%% @author Richard Carlsson <richardc@it.uu.se>
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
		indent = 0
	       }).

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    St = #state{verbose = proplists:get_bool(verbose, Options)},
    receive
	{start, _Reference} ->
	    if St#state.verbose -> print_header();
	       true -> ok
	    end,
	    St
    end.

terminate({ok, Data}, St) ->
    Pass = proplists:get_value(pass, Data, 0),
    Fail = proplists:get_value(fail, Data, 0),
    Skip = proplists:get_value(skip, Data, 0),
    Cancel = proplists:get_value(cancel, Data, 0),
    if Fail =:= 0, Skip =:= 0, Cancel =:= 0 ->
	    if Pass =:= 0 ->
		    io:fwrite("  There were no tests to run.\n");
	       true ->
		    if St#state.verbose -> print_bar();
		       true -> ok
		    end,
		    if Pass =:= 1 ->
			    io:fwrite("  Test passed.\n");
		       true ->
			    io:fwrite("  All ~w tests passed.\n", [Pass])
		    end
	    end,
	    sync_end(ok);
       true ->
	    print_bar(),
	    io:fwrite("  Failed: ~w.  Skipped: ~w.  Passed: ~w.\n",
		      [Fail, Skip, Pass]),
	    if Cancel =/= 0 ->
		    io:fwrite("One or more tests were cancelled.\n");
	       true -> ok
	    end,
	    sync_end(error)
    end;
terminate({error, Reason}, _St) ->
    io:fwrite("Internal error: ~P.\n", [Reason, 25]),
    sync_end(error).

sync_end(Result) ->
    receive
	{stop, Reference, ReplyTo} ->
	    ReplyTo ! {result, Reference, Result},
	    ok
    end.

print_header() ->
    io:fwrite("======================== EUnit ========================\n").

print_bar() ->
    io:fwrite("=======================================================\n").


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
    if St#state.verbose -> print_test_begin(St#state.indent, Data);
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
	    if St#state.verbose -> print_test_end(Data);
	       true -> ok
	    end,
	    St;
	Status ->
	    if St#state.verbose -> ok;
	       true -> print_test_begin(St#state.indent, Data)
	    end,
	    print_test_error(Status, Data),
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
		    print_group_cancel(I, Reason);
	       true ->
		    print_group_start(I, Desc),
		    print_group_cancel(I, Reason)
	    end,
	    St#state{indent = I - 1}
    end;
handle_cancel(test, Data, St) ->
    ?debugFmt("handle_cancel test ~w", [Data]),
    if St#state.verbose -> ok;
       true -> print_test_begin(St#state.indent, Data)
    end,
    print_test_cancel(proplists:get_value(reason, Data)),
    St.


indent(N) when is_integer(N), N >= 1 ->
    io:put_chars(lists:duplicate(N * 2, $\s));
indent(_N) ->
    ok.

print_group_start(I, Desc) ->
    indent(I),
    io:fwrite("~s\n", [Desc]).

print_group_end(I, Time) ->
    if Time > 0 ->
	    indent(I),
	    io:fwrite("[done in ~.3f s]\n", [Time/1000]);
       true ->
	    ok
    end.

print_test_begin(I, Data) ->
    Desc = proplists:get_value(desc, Data),
    Line = proplists:get_value(line, Data, 0),
    indent(I),
    L = if Line =:= 0 -> "";
	   true -> io_lib:fwrite("~w:", [Line])
	end,
    D = if Desc =:= "" ; Desc =:= undefined -> "";
	   true -> io_lib:fwrite(" (~s)", [Desc])
	end,
    case proplists:get_value(source, Data) of
	{Module, Name, _Arity} ->
	    io:fwrite("~s:~s ~s~s...", [Module, L, Name, D]);
	_ ->
	    io:fwrite("~s~s...", [L, D])
    end.

print_test_end(Data) ->
    Time = proplists:get_value(time, Data, 0),
    T = if Time > 0 -> io_lib:fwrite("[~.3f s] ", [Time/1000]);
	   true -> ""
	end,
    io:fwrite("~sok\n", [T]).

print_test_error({error, Exception}, Data) ->
    Output = proplists:get_value(output, Data),
    io:fwrite("*failed*\n::~s",
	      [eunit_lib:format_exception(Exception)]),
    case Output of
	<<>> ->
	    io:put_chars("\n\n");
	<<Text:800/binary, _:1/binary, _/binary>> ->
	    io:fwrite("  output:<<\"~s\">>...\n\n", [Text]);
	_ ->
	    io:fwrite("  output:<<\"~s\">>\n\n", [Output])
    end;
print_test_error({skipped, Reason}, _) ->
    io:fwrite("*did not run*\n::~s\n", [format_skipped(Reason)]).

format_skipped({module_not_found, M}) ->
    io_lib:format("missing module: ~w", [M]);
format_skipped({no_such_function, {M,F,A}}) ->
    io_lib:format("no such function: ~w:~w/~w", [M,F,A]).    

print_test_cancel(Reason) ->
    io:fwrite(format_cancel(Reason)).

print_group_cancel(_I, {blame, _}) ->
    ok;
print_group_cancel(I, Reason) ->
    indent(I),
    io:fwrite(format_cancel(Reason)).

format_cancel(undefined) ->
    "*skipped*\n";
format_cancel(timeout) ->
    "*timed out*\n";
format_cancel({startup, Reason}) ->
    io_lib:fwrite("*could not start test process*\n::~P\n\n",
		  [Reason, 15]);
format_cancel({blame, _SubId}) ->
    "*cancelled because of subtask*\n";
format_cancel({exit, Reason}) ->
    io_lib:fwrite("*unexpected termination of test process*\n::~P\n\n",
		  [Reason, 15]);
format_cancel({abort, Reason}) ->
    eunit_lib:format_error(Reason).
