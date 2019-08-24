%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(etp_commands).

-export([file/1]).

file([Fname]) ->
    Result = (catch file_1(Fname)),
    io:format("% ~p~n", [Result]),
    init:stop().

file_1(Fname) ->
    io:format("% Reading ~p...~n", [Fname]),
    {ok,Fd} = file:open(Fname, [read,binary]),
    case read_op(Fd, 128) of
	"chart" ->
            io:format("% Reading heap chart data...~n"),
	    chart_scan(Fd);
	"overlapped-heaps" -> 
            io:format("% Reading overlapped-heaps data...~n"),
	    overlapped_scan(Fd)
    end.

read_op(_Fd, 0) ->
    [];
read_op(Fd, N) ->
    case file:read(Fd, 1) of
	{ok,<<0>>} -> [];
	{ok,<<C>>} -> [C|read_op(Fd, N-1)]
    end.



overlapped_scan(Fd) ->
    overlapped_scan_1(Fd, []).

overlapped_scan_1(Fd, R) ->
    case file:read(Fd, 4*5) of
	eof ->
	    io:format("% Analyzing overlaps...~n"),
	    overlapped_analyze(lists:sort(R));
	{ok,<<Id:32/native,Heap:32/native,Hend:32/native,
	     0:32/native,0:32/native>>} 
	when Heap < Hend ->
	    overlapped_scan_to_0(Fd, [{{Heap,Hend},{Id,heap}}|R], Id, 1);
	{ok,<<Id:32/native,Heap:32/native,Hend:32/native,
	     OldHeap:32/native,OldHend:32/native>>} 
	when Heap < Hend, OldHeap < OldHend->
	    overlapped_scan_to_0(Fd, [{{Heap,Hend},{Id,heap}},
				      {{OldHeap,OldHend},{Id,old_heap}}|R],
				 Id, 1)
    end.

overlapped_scan_to_0(Fd, R, Id, Cnt) ->
    case file:read(Fd, 4*2) of
	{ok,<<0:32/native,0:32/native>>} ->
	    overlapped_scan_1(Fd, R);
	{ok,<<Heap:32/native,Hend:32/native>>}
	when Heap < Hend ->
	    overlapped_scan_to_0(Fd, 
				 [{{Heap,Hend},{Id,{heap_fragment,Cnt}}}|R],
				 Id, Cnt+1);
	eof ->
	    io:format("% Premature end of dump: ~p~n", [Id,Cnt|R])
    end.

overlapped_analyze([]) -> 
    io:format("% Oops! was that file empty?~n");
overlapped_analyze([{{_,Hend1},_}|[{{Heap2,_},_}|_]=R]) 
  when Hend1 =< Heap2 ->
    overlapped_analyze(R);
overlapped_analyze([{Addrs1,Tag1}|[{Addrs2,Tag2}|_]=R]) ->
    io:format("% ~p overlaps ~p (~p,~p)~n", [Tag1,Tag2,Addrs1,Addrs2]),
    overlapped_analyze(R);
overlapped_analyze([_]) -> 
    io:format("% End of overlaps~n").


chart_scan(Fd) ->
    {ok,<<Heap:32/native,HighWater:32/native,Hend:32/native,
	 OldHeap:32/native,OldHend:32/native>>} = file:read(Fd, 4*5),
    chart_scan_1(Fd, 
		 [{Heap,Heap,heap,0},
		  {HighWater,HighWater,high_water,0},
		  {Hend,Hend,hend,0},
		  {OldHeap,OldHeap,old_heap,0},
		  {OldHend,OldHend,old_hend,0}|chart_scan_hdr(Fd)]).

chart_scan_hdr(Fd) ->
    chart_scan_hdr_2(0, chart_scan_hdr_1(Fd)).

chart_scan_hdr_1(Fd) ->
    case file:read(Fd, 4*2) of
	eof -> [];
	{ok,<<0:32/native,0:32/native>>} -> [];
	{ok,<<Start:32/native,Size:32/native>>} ->
	    [{Start,Size}|chart_scan_hdr_1(Fd)]
    end.

chart_scan_hdr_2(_N, []) -> [];
chart_scan_hdr_2(N, [{Start,End}|T]) when Start =< End ->
    [{Start,Start,{heap_frag,N},0},{End,End,{heap_frag_end,N},0}
     |chart_scan_hdr_2(N+1, T)].

chart_scan_1(Fd, R) ->
    case file:read(Fd, 4*4) of
	eof ->
	    io:format("% Analyzing heap chart...~n"),
	    chart_analyze(lists:sort(R));
	{ok,
	 <<Addr:32/native,Size:32/native,Id:32/native,Depth:32/native>>} ->
	    chart_scan_1(Fd, [{Addr,Addr+Size,Id,Depth}|R])
    end.

%-define(raw_chart_dump, 1).
-ifdef(raw_chart_dump).

chart_analyze([]) ->
    io:format("% End of chart~n");
chart_analyze([{S,E,Id,D}|R]) ->
    io:format("% ~.16x-~.16x: ~w[~w]~n", 
	      [S,"0x",E,"0x",Id,D]),
    chart_analyze(R).

-else.

chart_analyze([]) -> 
    io:format("% ***Oops, was chart empty?***~n");
chart_analyze([{S,_,Id,D}=X|R]) ->
    io:format("% ~.16x: ~w[~w", [S,"0x",Id,D]),
    chart_analyze_1(R, X).

chart_analyze_1([{S,E,Id,D}=X|R], {S,E,Id,_}) ->
    io:format(",~w", [D]),
    chart_analyze_1(R, X);
chart_analyze_1([{S,E,Id,D}=X|R], {S,E,_,_}) ->
    io:format("],~w[~w", [Id,D]),
    chart_analyze_1(R, X);
chart_analyze_1(R, X) ->
    io:format("]~n"),
    chart_analyze_2(R, X).

chart_analyze_2([], {_,E,_,_}) ->
    io:format("% ~.16x: End of chart~n", [E,"0x"]);
chart_analyze_2([{S,_,_,_}|_]=R, {_,E,_,_}) ->
    if E == S ->
	    chart_analyze(R);
       E < S ->
	    io:format("% ~.16x:~n", [E,"0x"]),
	    chart_analyze(R);
       true ->
	    io:format("% ~.16x: ***Overlap***~n", [E,"0x"]),
	    chart_analyze(R)
    end.

-endif.
