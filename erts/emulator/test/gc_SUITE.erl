%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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

%% Test the garbage collector (or Memory Recycler)

-module(gc_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0]).

-export([
    grow_heap/1,
    grow_stack/1,
    grow_stack_heap/1,
    max_heap_size/1,
    minor_major_gc_option_async/1,
    minor_major_gc_option_self/1
]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [grow_heap, grow_stack, grow_stack_heap, max_heap_size,
    minor_major_gc_option_self,
    minor_major_gc_option_async].


%% Produce a growing list of elements,
%% for X calls, then drop one item per call
%% until the list is empty.
grow_heap(Config) when is_list(Config) ->
    ct:timetrap({minutes, 40}),
    ok  = grow_heap1(256),
    ok  = grow_heap1(512),
    ok  = grow_heap1(1024),
    ok  = grow_heap1(2048),
    ok.

grow_heap1(Len) ->
    io:format("~ngrow_heap with ~p items.",[Len]),
    show_heap("before:"),
    grow_heap1([], Len, 0, up),
    show_heap("after:").

grow_heap1(List, MaxLen, MaxLen, up) ->
    show_heap("top:"),
    grow_heap1(List, MaxLen, MaxLen-1, down);
grow_heap1(List, MaxLen, CurLen, up) ->
    NewList=[make_arbit()|List],
    grow_heap1(NewList, MaxLen, CurLen+1, up);
grow_heap1([], _MaxLen, _, down) ->
    ok;
grow_heap1([_|List], MaxLen, CurLen, down) ->
    C=erlang:unique_integer([positive]),
    Num     = C rem (length(List))+1,
    Elem    = lists:nth(Num, List),
    NewList = lists:delete(Elem, List),
    grow_heap1(NewList, MaxLen, CurLen-1, down).



%% Increase and decrease stack size, and
%% drop off some garbage from time to time.
grow_stack(Config) when is_list(Config) ->
    ct:timetrap({minutes, 80}),
    show_heap("before:"),
    grow_stack1(200, 0),
    show_heap("after:"),
    ok.

grow_stack1(0, _) ->
    ok;
grow_stack1(Recs, 0) ->
%    show_heap("running:"),
    grow_stack1(Recs-1, Recs),
    grow_stack1(0,0);
grow_stack1(Recs, CurRecs) ->
    grow_stack1(Recs, CurRecs-1),
    make_arbit(),
    grow_stack1(1,0),
    ok.


%% Let's see how BEAM handles this one...
%% While growing the heap, bounces the size of the
%% stack, and while reducing the heap, bounces the stack usage.
grow_stack_heap(Config) when is_list(Config) ->
    ct:timetrap({minutes, 40}),
    grow_stack_heap1(16),
    grow_stack_heap1(32),
    ok.

grow_stack_heap1(MaxLen) ->
    io:format("~ngrow_stack_heap with ~p items.",[MaxLen]),
    show_heap("before:"),
    grow_stack_heap1([], MaxLen, 0, up),
    show_heap("after:").

grow_stack_heap1(List, MaxLen, MaxLen, up) ->
    show_heap("top:"),
    grow_stack_heap1(List, MaxLen, MaxLen-1, down);
grow_stack_heap1(List, MaxLen, CurLen, up) ->
    grow_stack1(CurLen*2,0),
    grow_stack_heap1([make_arbit()|List], MaxLen, CurLen+1, up),
    ok;

grow_stack_heap1([], _MaxLen, _, down) -> ok;
grow_stack_heap1([_|List], MaxLen, CurLen, down) ->
    grow_stack1(CurLen*2,0),
    C=erlang:unique_integer([positive]),
    Num=C rem (length(List))+1,
    Elem=lists:nth(Num, List),
    NewList=lists:delete(Elem, List),
    grow_stack_heap1(NewList, MaxLen, CurLen-1, down),
    ok.


%% Create an arbitrary element/term.
make_arbit() ->
    {AA,BB,CC}=erlang:timestamp(),
    A=AA+1, B=BB+1, C=(CC+erlang:unique_integer([positive])) rem 1000000 + 1,
    New =
	case C rem 9 of
	    0 -> make_string((B div C) +5);
	    1 -> C;
	    2 -> make_ref();
	    3 -> self();
	    4 -> list_to_binary(make_string((C div B) + 12));
	    5 -> (C*B)/(A+1);
	    6 -> list_to_tuple(make_string((B div C) +5));
	    7 -> list_to_atom(make_string(((C div B) rem 254) + 2));
	    8 -> fun(X) -> {X,AA,make_string((B div C)+10)} end
	end,
    New.

%% Create an arbitrary string of a certain length.
make_string(Length) ->
    Alph="abcdefghjiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"++
	"0123456789",
    make_string(Alph, Length, []).

make_string(_, 0, Acc) ->
    Acc;
make_string(Alph, Length, Acc) ->
    C=erlang:unique_integer([positive]),
    Pos=1+(Length*C rem length(Alph)),
    make_string(Alph, Length-1, 
		[lists:nth(Pos,Alph)|Acc]).

show_heap(String) ->
    garbage_collect(self()),
    receive after 1 -> ok end,
    {heap_size, HSize}=process_info(self(), heap_size),
    {stack_size, SSize}=process_info(self(), stack_size),
    io:format("Heap/Stack "++String++"~p/~p", [HSize, SSize]).
    
%% Test that doing a remote GC that triggers the max heap size
%% kills the process.
max_heap_size(_Config) ->

    Pid = spawn_opt(fun long_receive/0,[{max_heap_size, 1024},
                                        {message_queue_data, on_heap}]),
    [Pid ! lists:duplicate(I,I) || I <- lists:seq(1,100)],
    Ref = erlang:monitor(process, Pid),

    %% Force messages to be viewed as part of heap
    erlang:process_info(Pid, messages),

    %% Do the GC that triggers max heap
    erlang:garbage_collect(Pid),

    %% Verify that max heap was triggered
    receive
        {'DOWN', Ref, process, Pid, killed} -> ok
    after 5000 ->
            ct:fail({process_did_not_die, Pid, erlang:process_info(Pid)})
    end.

long_receive() ->
    receive
    after 10000 ->
            ok
    end.

minor_major_gc_option_self(_Config) ->
    %% Try as major, the test process will self-trigger GC
    check_gc_tracing_around(
        fun(Pid, Ref) ->
            Pid ! {gc, Ref, major}
        end, [gc_major_start, gc_major_end]),

    %% Try as minor, the test process will self-trigger GC
    check_gc_tracing_around(
        fun(Pid, Ref) ->
            Pid ! {gc, Ref, minor}
        end, [gc_minor_start, gc_minor_end]).

minor_major_gc_option_async(_Config) ->
    %% Try with default option, must be major GC
    check_gc_tracing_around(
        fun(Pid, _Ref) ->
            erlang:garbage_collect(Pid, [])
        end, [gc_major_start, gc_major_end]),

    %% Try with the 'major' type
    check_gc_tracing_around(
        fun(Pid, _Ref) ->
            erlang:garbage_collect(Pid, [{type, major}])
        end, [gc_major_start, gc_major_end]),

    %% Try with 'minor' option, once
    check_gc_tracing_around(
        fun(Pid, _Ref) ->
            erlang:garbage_collect(Pid, [{type, minor}])
        end, [gc_minor_start, gc_minor_end]),

    %% Try with 'minor' option, once, async
    check_gc_tracing_around(
        fun(Pid, Ref) ->
            ?assertEqual(async,
                erlang:garbage_collect(Pid, [{type, minor}, {async, Ref}])),

            receive
                {garbage_collect, Ref, true} ->
                    ok
            after 10000 ->
                ct:fail("Did not receive a completion notification on async GC")
            end
        end, [gc_minor_start, gc_minor_end]).

%% Traces garbage collection around the given operation, and fails the test if
%% it results in any unexpected messages or if the expected trace tags are not
%% received.
check_gc_tracing_around(Fun, ExpectedTraceTags) ->
    Ref = erlang:make_ref(),
    Pid = spawn(
        fun Endless() ->
            receive
                {gc, Ref, Type} ->
                    erlang:garbage_collect(self(), [{type, Type}])
            after 100 ->
                ok
            end,
            Endless()
        end),
    erlang:garbage_collect(Pid, []),
    erlang:trace(Pid, true, [garbage_collection]),
    Fun(Pid, Ref),
    expect_trace_messages(Pid, ExpectedTraceTags),
    erlang:trace(Pid, false, [garbage_collection]),
    erlang:exit(Pid, kill),
    check_no_unexpected_messages().

%% Ensures that trace messages with the provided tags have all been received
%% within a reasonable timeframe.
expect_trace_messages(_Pid, []) ->
    ok;
expect_trace_messages(Pid, [Tag | TraceTags]) ->
    receive
        {trace, Pid, Tag, _Data} ->
            expect_trace_messages(Pid, TraceTags)
    after 4000 ->
        ct:fail("Didn't receive tag ~p within 4000ms", [Tag])
    end.

check_no_unexpected_messages() ->
    receive
        Anything ->
            ct:fail("Unexpected message: ~p", [Anything])
    after 0 ->
        ok
    end.
