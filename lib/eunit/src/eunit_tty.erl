%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
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
		indent = 0
	       }).

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    St = #state{verbose = proplists:get_bool(verbose, Options)},
    put(no_tty, proplists:get_bool(no_tty, Options)),
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
		    fwrite("  There were no tests to run.\n");
	       true ->
		    if St#state.verbose -> print_bar();
		       true -> ok
		    end,
		    if Pass =:= 1 ->
			    fwrite("  Test passed.\n");
		       Pass =:= 2 ->
			    fwrite("  2 tests passed.\n");
		       true ->
			    fwrite("  All ~w tests passed.\n", [Pass])
		    end
	    end,
	    sync_end(ok);
       true ->
	    print_bar(),
	    fwrite("  Failed: ~w.  Skipped: ~w.  Passed: ~w.\n",
                   [Fail, Skip, Pass]),
	    if Cancel =/= 0 ->
		    fwrite("One or more tests were cancelled.\n");
	       true -> ok
	    end,
	    sync_end(error)
    end;
terminate({error, Reason}, _St) ->
    fwrite("Internal error: ~tP.\n", [Reason, 25]),
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
    fwrite(lists:duplicate(N * 2, $\s));
indent(_N) ->
    ok.

print_group_start(I, Desc) ->
    indent(I),
    fwrite("~ts\n", [Desc]).

print_group_end(I, Time) ->
    if Time > 0 ->
	    indent(I),
	    fwrite("[done in ~.3f s]\n", [Time/1000]);
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
	   true -> io_lib:fwrite(" (~ts)", [Desc])
	end,
    case proplists:get_value(source, Data) of
	{Module, Name, _Arity} ->
	    fwrite("~ts:~ts ~ts~ts...", [Module, L, Name, D]);
	_ ->
	    fwrite("~ts~ts...", [L, D])
    end.

print_test_end(Data) ->
    Time = proplists:get_value(time, Data, 0),
    T = if Time > 0 -> io_lib:fwrite("[~.3f s] ", [Time/1000]);
	   true -> ""
	end,
    fwrite("~tsok\n", [T]).

print_test_error({error, Exception}, Data) ->
    Output = proplists:get_value(output, Data),
    fwrite("*failed*\n~ts", [eunit_lib:format_exception(Exception)]),
    case Output of
	<<>> ->
	    fwrite("\n\n");
	<<Text:800/binary, _:1/binary, _/binary>> ->
	    fwrite("  output:<<\"~ts\">>...\n\n", [Text]);
	_ ->
	    fwrite("  output:<<\"~ts\">>\n\n", [Output])
    end;
print_test_error({skipped, Reason}, _) ->
    fwrite("*did not run*\n::~ts\n", [format_skipped(Reason)]).

format_skipped({module_not_found, M}) ->
    io_lib:fwrite("missing module: ~w", [M]);
format_skipped({no_such_function, {M,F,A}}) ->
    io_lib:fwrite("no such function: ~w:~tw/~w", [M,F,A]).

print_test_cancel(Reason) ->
    fwrite(format_cancel(Reason)).

print_group_cancel(_I, {blame, _}) ->
    ok;
print_group_cancel(I, Reason) ->
    indent(I),
    fwrite(format_cancel(Reason)).

format_cancel(undefined) ->
    "*skipped*\n";
format_cancel(timeout) ->
    "*timed out*\n";
format_cancel({startup, Reason}) ->
    io_lib:fwrite("*could not start test process*\n::~tP\n\n",
		  [Reason, 15]);
format_cancel({blame, _SubId}) ->
    "*cancelled because of subtask*\n";
format_cancel({exit, Reason}) ->
    io_lib:fwrite("*unexpected termination of test process*\n::~tP\n\n",
		  [Reason, 15]);
format_cancel({abort, Reason}) ->
    eunit_lib:format_error(Reason).

fwrite(String) ->
    fwrite(String, []).

fwrite(String, Args) ->
    case get(no_tty) of
        false -> io:fwrite(String, Args);
        true -> ok
    end.
