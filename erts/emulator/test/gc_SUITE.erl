%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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

%% Test the garbage collector (or Memory Recycler)

-module(gc_SUITE).

-include_lib("test_server/include/test_server.hrl").
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-define(default_timeout, ?t:minutes(10)).

-export([grow_heap/1, grow_stack/1, grow_stack_heap/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [grow_heap, grow_stack, grow_stack_heap].

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


grow_heap(doc) -> ["Produce a growing list of elements, ",
		   "for X calls, then drop one item per call",
		   "until the list is empty."];
grow_heap(Config) when is_list(Config) ->
    ?line Dog=test_server:timetrap(test_server:minutes(40)),
    ?line ok=grow_heap1(256),
    case os:type() of 
	vxworks ->
	    stop_here;
	_ ->
	    ?line ok=grow_heap1(512),
	    ?line ok=grow_heap1(1024),
	    ?line ok=grow_heap1(2048)
    end,
    ?line test_server:timetrap_cancel(Dog),
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
    ?line {_,_,C}=erlang:now(),
    ?line Num=C rem (length(List))+1,
    ?line Elem=lists:nth(Num, List),
    ?line NewList=lists:delete(Elem, List),
    grow_heap1(NewList, MaxLen, CurLen-1, down).



grow_stack(doc) -> ["Increase and decrease stack size, and ",
		    "drop off some garbage from time to time."];
grow_stack(Config) when is_list(Config) ->
    ?line Dog=test_server:timetrap(test_server:minutes(80)),
    show_heap("before:"),
    case os:type() of
	vxworks ->
	    ?line grow_stack1(25, 0);
	_ ->
	    ?line grow_stack1(200, 0)
    end,
    show_heap("after:"),
    ?line test_server:timetrap_cancel(Dog),
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
grow_stack_heap(doc) -> ["While growing the heap, bounces the size ",
			 "of the stack, and while reducing the heap",
			 "bounces the stack usage."];
grow_stack_heap(Config) when is_list(Config) ->
    case os:type() of 
	vxworks ->
	    {comment, "Takes too long to run on VxWorks/cpu32"};
	_ ->
	    ?line Dog=test_server:timetrap(test_server:minutes(40)),
	    ?line grow_stack_heap1(16),
	    ?line grow_stack_heap1(32),
	    ?line test_server:timetrap_cancel(Dog),
	    ok
    end.

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
    ?line {_,_,C}=erlang:now(),
    ?line Num=C rem (length(List))+1,
    ?line Elem=lists:nth(Num, List),
    ?line NewList=lists:delete(Elem, List),
    grow_stack_heap1(NewList, MaxLen, CurLen-1, down),
    ok.


%% Create an arbitrary element/term.
make_arbit() ->
    {AA,BB,CC}=erlang:now(),
    A=AA+1, B=BB+1, C=CC+1,
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
    {_,_,C}=erlang:now(),
    Pos=1+(Length*C rem length(Alph)),
    make_string(Alph, Length-1, 
		[lists:nth(Pos,Alph)|Acc]).

show_heap(String) ->
    garbage_collect(self()),
    receive after 1 -> ok end,
    {heap_size, HSize}=process_info(self(), heap_size),
    {stack_size, SSize}=process_info(self(), stack_size),
    io:format("Heap/Stack "++String++"~p/~p", [HSize, SSize]).
    
