%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2017. All Rights Reserved.
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

-module(estone_SUITE).
%% Test functions
-export([all/0, suite/0, groups/0,
	 estone/1, estone_bench/1, pgo/0]).

%% Internal exports for EStone tests
-export([lists/1,
	 msgp/1,
	 msgp_medium/1,
	 msgp_huge/1,
	 pattern/1,
	 trav/1,
	 port_io/1,
	 large_dataset_work/1,
	 large_local_dataset_work/1,mk_big_procs/1,big_proc/0, very_big/1,
	 alloc/1,
	 bif_dispatch/1,
	 binary_h/1,echo/1,
	 ets/1,
	 generic/1,req/2,gserv/4,handle_call/3,
	 int_arith/1,
	 float_arith/1,
	 fcalls/1,remote0/1,remote1/1,app0/1,app1/1,
	 timer/1,
	 links/1,lproc/1,
	 run_micro/3,p1/1,ppp/3,macro/2,micros/0]).

-ifndef(PGO).
-include_lib("common_test/include/ct_event.hrl").
-endif.

%% EStone defines
-define(TOTAL, (3000 * 1000 * 100)).   %% 300 secs
-define(BIGPROCS, 2).
-define(BIGPROC_SIZE, 50).
-define(STONEFACTOR, 31000000).   %% Factor to make the reference
                             %% implementation to make 1000 TS_ESTONES.
-record(micro,
	{function, %% The name of the function implementing the micro
	 weight,   %% How important is this in typical applications ??
	 loops = 100,%% initial data
	 tt1,      %% time to do one round
	 str}).    %% Header string


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 4}}].

all() -> 
    [estone].

groups() -> 
    [{estone_bench, [{repeat,50}],[estone_bench]}].


%% EStone Test
estone(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    Mhz=get_cpu_speed(os:type(),DataDir),
    L = ?MODULE:macro(?MODULE:micros(),DataDir),
    {Total, Stones} = sum_micros(L, 0, 0),
    pp(Mhz,Total,Stones,L),
    {comment,Mhz ++ " MHz, " ++ integer_to_list(Stones) ++ " ESTONES"}.

estone_bench(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    L = ?MODULE:macro(?MODULE:micros(),DataDir),
    {Total, Stones} = sum_micros(L, 0, 0),
    notify([[{title,"ESTONES"}, {estones, Stones}] | L]),
    L.

-ifndef(PGO).
notify(Marks) ->
    [ct_event:notify(
       #event{name = benchmark_data,
	      data = [{name,proplists:get_value(title, Mark)},
		      {value,proplists:get_value(estones, Mark)}]})
     || Mark <- Marks].
-else.
notify(_) ->
    ok.
-endif.

%% The benchmarks to run in order to guide PGO (profile guided optimisation)
pgo() ->
    %% We run all benchmarks except the port_io as we don't want to
    %% have to build a custom port.
    Micros = ?MODULE:micros() -- [micro(port_io)],
    ?MODULE:macro(Micros,[]).

%%
%% Calculate CPU speed
%%
%% get_cpu_speed() now returns a string. For multiprocessor
%% machines (at least on Solaris) the format is: <F1>+<F2>[+...]
%%
get_cpu_speed({win32, _},_DataDir) ->
    RegH =
	case catch win32reg:open([read]) of
	    {ok, Handle} ->
		Handle;
	    _ ->
		io:format("Error.~nCannot determine CPU clock"
			  "frequency.~n"
			  "Please set the environment variable"
			  "\"CPU_SPEED\"~n"),
		exit(self(), {error, no_cpu_speed})
	end,
    case win32reg:change_key(RegH,"\\hkey_local_machine\\hardware\\"
			     "description\\system\\centralprocessor"
			     "\\0") of
	ok ->
	    ok;
	_ ->
	    io:format("Error.~nRegistry seems to be damaged or"
		      "unavailable.~n"
		      "Please set the environment variable"
		      "\"CPU_SPEED\",~nor correct your registry"
		      "if possible.~n"),
	    win32reg:close(RegH),
	    exit(self(), {error, no_cpu_speed})
    end,
    case win32reg:value(RegH, "~MHZ") of
	{ok, Speed} ->
	    win32reg:close(RegH),
	    integer_to_list(Speed);
	_ ->
	    io:format("Error.~nRegistry seems to be damaged or "
		      "unavailable.~n"),
	    io:format("Please set the environment variable"
		      "\"CPU_SPEED\"~n"),
	    win32reg:close(RegH),
	    exit(self(), {error, no_cpu_speed})
    end;
get_cpu_speed({unix, sunos},DataDir) ->
    os:cmd(filename:join(DataDir,"sunspeed.sh")) -- "\n";
get_cpu_speed(_Other,_DataDir) ->
    %% Cannot determine CPU speed
    "UNKNOWN".


%%
%% Pretty Print EStone Result
%%
pp(Mhz,Total,Stones,Ms) ->
    io:format("EStone test completed~n",[]),
    io:format("**** CPU speed ~s MHz ****~n",[Mhz]),
    io:format("**** Total time ~w seconds ****~n", [Total / 1000000]),
    io:format("**** ESTONES = ~w ****~n~n", [Stones]),
    io:format("~-31s      ~-12s  ~-10s   %    ~-10s ~n~n",
	      ["    Title", "Millis", "Estone", "Loops"]),
    erlang:display({'ESTONES', Stones}),
    pp2(Ms).

sum_micros([], Tot, Stones) -> {Tot, Stones};
sum_micros([H|T], Tot, Sto) -> 
    sum_micros(T, ks(microsecs, H) + Tot, ks(estones, H) + Sto).

pp2([]) ->   ok;
pp2([R|Tail]) ->
    io:format("~-35s  ~-12w    ~-10w   ~-2w    ~-10w ~n",
	      [ks(title,R), 
	       round(ks(microsecs, R) / 1000), 
	       ks(estones, R),
	       ks(weight_percentage, R),
	       ks(loops, R)]),
    pp2(Tail).

ks(K, L) ->
    {value, {_, V}} = lists:keysearch(K, 1, L),
    V.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EStone test
micro(lists) -> 
     #micro{function = lists,
	    weight = 7, 
	    loops = 6400,
	    str = "list manipulation"};
micro(msgp) ->
    #micro{function = msgp,
	    weight = 10,
	    loops = 1515,
	    str = "small messages"};
micro(msgp_medium) ->
    #micro{function = msgp_medium,
	    weight = 14,
	    loops = 1527,
	    str = "medium messages"};
micro(msgp_huge) ->
    #micro{function = msgp_huge,
	    weight = 4,
	    loops = 52,
	    str = "huge messages"};

micro(pattern) ->
    #micro{function = pattern,
	    weight = 5,
	    loops = 1046,
	    str = "pattern matching"};

micro(trav) ->
    #micro{function = trav,
	    weight = 4,
	    loops = 2834,
	    str = "traverse"};

micro(port_io) ->
    #micro{function = port_io,
	   weight = 12,
	   loops = 4800,
	   str = "Port i/o"};

micro(large_dataset_work) ->
    #micro{function = large_dataset_work,
	   weight = 3,
	   loops = 1193,
	   str = "Work with large dataset"};

micro(large_local_dataset_work) ->
    #micro{function = large_local_dataset_work,
	   weight = 3,
	   loops = 1174,
	   str = "Work with large local dataset"};

micro(alloc) ->
    #micro{function = alloc,
	   weight = 2,
	   loops = 3710,
	   str = "Alloc and dealloc"};

micro(bif_dispatch) ->
    #micro{function = bif_dispatch,
	   weight = 5,
	   loops = 1623,
	   str = "Bif dispatch"};

micro(binary_h) ->
    #micro{function = binary_h,
	   weight = 4,
	   loops = 581,
	   str = "Binary handling"};
micro(ets) ->
    #micro{function = ets,
	   weight = 6,
	   loops = 342,
	   str = "ets datadictionary"};
micro(generic) ->
    #micro{function = generic,
	   weight = 9,
	   loops = 7977,
	   str = "Generic server (with timeout)"};
micro(int_arith) ->
    #micro{function = int_arith,
	   weight = 3,
	   loops = 4157,
	   str = "Small Integer arithmetics"};
micro(float_arith) ->
    #micro{function = float_arith,
	   weight = 1,
	   loops = 5526,
	   str = "Float arithmetics"};
micro(fcalls) ->
    #micro{function = fcalls,
	   weight = 5,
	   loops = 882,
	   str = "Function calls"};

micro(timer) ->
    #micro{function = timer,
	   weight = 2,
	   loops = 2312,
	   str = "Timers"};

micro(links) ->
    #micro{function = links,
	   weight = 1,
	   loops = 30,
	   str = "Links"}.



%% Return a list of micro's
micros() ->
    [
     micro(lists),
     micro(msgp),
     micro(msgp_medium),
     micro(msgp_huge),
     micro(pattern),
     micro(trav),
     micro(port_io),
     micro(large_dataset_work),
     micro(large_local_dataset_work),
     micro(alloc),
     micro(bif_dispatch),
     micro(binary_h),
     micro(ets),
     micro(generic),
     micro(int_arith),
     micro(float_arith),
     micro(fcalls),
     micro(timer),
     micro(links)
    ].

macro(Ms,DataDir) ->
    statistics(reductions),
    statistics(runtime),
    lists(500),  %% fixup cache on first round
    run_micros(Ms,DataDir).

run_micros([],_) -> 
    io:nl(),
    [];
run_micros([H|T],DataDir) ->
    R = run_micro(H,DataDir),
    [R| run_micros(T,DataDir)].

run_micro(M,DataDir) ->
    Pid = spawn(?MODULE, run_micro, [self(),M,DataDir]),
    Res = receive {Pid, Reply} -> Reply end,
    {value,{title,Title}} = lists:keysearch(title,1,Reply),
    {value,{estones,Estones}} = lists:keysearch(estones,1,Reply),
    erlang:display({Title,Estones}),
    Res.
    

run_micro(Top, M, DataDir) ->
    EstoneCat = filename:join(DataDir,"estone_cat"),
    put(estone_cat,EstoneCat),
    Top ! {self(),  apply_micro(M)}.

apply_micro(M) ->
    {GC0, Words0, _} = statistics(garbage_collection),
    statistics(reductions),
    Before = monotonic_time(),
    Compensate = apply_micro(M#micro.function, M#micro.loops),
    After = monotonic_time(),
    {GC1, Words1, _} = statistics(garbage_collection),
    {_, Reds} = statistics(reductions),
    Elapsed = subtr(Before, After),
    MicroSecs = Elapsed - Compensate,
    [{title, M#micro.str},
     {tt1, M#micro.tt1},
     {function, M#micro.function},
     {weight_percentage, M#micro.weight},
     {loops, M#micro.loops},
     {microsecs,MicroSecs},
     {estones, (M#micro.weight * M#micro.weight * ?STONEFACTOR) div max(1,MicroSecs)},
     {gcs, GC1 - GC0},
     {kilo_word_reclaimed, (Words1 - Words0) div 1000},
     {kilo_reductions, Reds div 1000},
     {gc_intensity, gci(max(1,Elapsed), GC1 - GC0, Words1 - Words0)}].

monotonic_time() ->
    try erlang:monotonic_time() catch error:undef -> erlang:now() end.

subtr(Before, After) when is_integer(Before), is_integer(After) ->
    erlang:convert_time_unit(After-Before, native, 1000000);
subtr({_,_,_}=Before, {_,_,_}=After) ->
    timer:now_diff(After, Before).

gci(Micros, Words, Gcs) ->
    ((256 * Gcs) / Micros) + (Words / Micros).

apply_micro(Name, Loops) ->
    io:format("~w(~w)~n", [Name, Loops]),
    apply(?MODULE, Name, [Loops]).

%%%%%%%%%%%% micro bench manipulating lists. %%%%%%%%%%%%%%%%%%%%%%%%%
lists(I) ->
    L1 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    L2 = "aaaaaaaaaa",
    lists(I, L1, L2).

lists(0, _,_) ->
    0;
lists(I, L1, L2) ->
    revt(10, L1),
    appt(10, L1, L2),
    lists(I-1, L1, L2).

revt(0, _) -> 
    done;
revt(I, L) -> 
    reverse(L),
    revt(I-1, L).

reverse(L) ->
    reverse(L, []).
reverse([H|T], Ack) -> reverse(T, [H|Ack]);
reverse([], Ack) -> Ack.

append([H|T], L) ->
    [H | append(T, L)];
append([], L) ->
    L.

appt(0, _L1, _L2) -> ok;
appt(I, L1, L2) ->
    append(L1, L2),
    appt(I-1, L1, L2).


%%%%%%%%%%%%%%% small message passing and ctxt switching %%%%%%%
msgp(I) ->
    msgp(I, small()).

msgp(0, _) -> 
    0;
msgp(I, Msg) ->
    P1 = spawn(?MODULE, p1, [self()]),
    P2 = spawn(?MODULE, p1, [P1]),
    P3 = spawn(?MODULE, p1, [P2]),
    P4 = spawn(?MODULE, p1, [P3]),
    msgp_loop(100, P4, Msg),
    msgp(I-1, Msg).

p1(To) ->
    receive
	{_From, {message, X}} ->
	    To ! {self(), {message, X}},
	    p1(To);
	stop ->
	    To ! stop,
	    exit(normal)
    end.

msgp_loop(0, P, _) ->
    P ! stop,
    receive 
	stop -> ok
    end;
msgp_loop(I, P, Msg) ->
    P ! {self(), {message, Msg}},
    receive
	{_From, {message, _}} ->
	    msgp_loop(I-1, P, Msg)
    end.

%%%%%%%%%%%% large massage passing and ctxt switching %%%%%%%
msgp_medium(I) ->
        msgp_medium(I, big()).

msgp_medium(0, _) -> 
    0;
msgp_medium(I, Msg) ->
    P1 = spawn(?MODULE , p1, [self()]),
    P2 = spawn(?MODULE, p1, [P1]),
    P3 = spawn(?MODULE, p1, [P2]),
    P4 = spawn(?MODULE, p1, [P3]),
    msgp_loop(100, P4, Msg),
    msgp_medium(I-1, Msg).



%%%%%%%%%%%% huge massage passing and ctxt switching %%%%%%%
msgp_huge(I) ->
        msgp_huge(I, very_big(15)).

msgp_huge(0, _) -> 
    0;
msgp_huge(I, Msg) ->
    P1 = spawn(?MODULE , p1, [self()]),
    P4 = spawn(?MODULE, p1, [P1]),
    msgp_loop(100, P4, Msg),
    msgp_huge(I-1, Msg).

    
%%%%%% typical protocol pattern matching %%%%%%%
pattern(0) ->
    0;
pattern(I) ->
    Tail = "aaabbaaababba",
    P1 = [0, 1,2,3,4,5|Tail],
    pat_loop1(100, P1),
    pat_loop2(100, P1),
    pat_loop3(100, P1),
    pat_loop4(100, P1),
    pat_loop5(100, P1),
    pattern(I-1).

pat_loop1(0, _) -> 
    ok;
pat_loop1(_I, [_, _X, _Y, 0 |_T])  ->
    ok;
pat_loop1(_I, [_, _X, _Y, 1| _T]) ->
    ok;
pat_loop1(_I, [_, _X, _Y, 2 | _T]) -> 
    ok;
pat_loop1(I, [_, X, Y, 3 | T]) ->
    pat_loop1(I-1, [0, X,Y,3|T]).

pat_loop2(0, _) ->
    ok;
pat_loop2(_I, [_X, Y | _Tail]) when Y bsl 1 == 0 ->
    ok;
pat_loop2(_I, [_X, Y | _Tail]) when Y bsl 2 == 0 ->
    ok;
pat_loop2(I, [X, Y | Tail]) when Y bsl 2 == 4 ->
    pat_loop2(I-1, [X, Y |Tail]).


pat_loop3(0, _) ->
    ok;
pat_loop3(_I, [{c, h} | _Tail]) -> 
    ok;
pat_loop3(_I, [1, 0 |_T]) ->
    ok;
pat_loop3(_I, [X, _Y |_Tail]) when is_binary(X), size(X) == 1 ->
    ok;
pat_loop3(_I, [no, _Y|_Tail]) -> 
    ok;
pat_loop3(_I, []) ->
    ok;
pat_loop3(_I, [X,_Y|_T]) when X /= 0 ->
    ok;
pat_loop3(_I, [2,3|_T]) ->
    ok;
pat_loop3(_I, [1, 2]) ->
    ok;
pat_loop3(I, [0, 1 |T]) ->
    pat_loop3(I-1, [0,1|T]).


pat_loop4(0, _) ->  ok;
pat_loop4(_I, [20|_T]) -> ok;
pat_loop4(_I, [219|_T]) -> ok;
pat_loop4(_I, [18|_T]) -> ok;
pat_loop4(_I, [17|_T]) -> ok;
pat_loop4(_I, [16|_T]) -> ok;
pat_loop4(_I, [15|_T]) -> ok;
pat_loop4(_I, [14|_T]) -> ok;
pat_loop4(_I, [13|_T]) -> ok;
pat_loop4(_I, [12|_T]) -> ok;
pat_loop4(_I, [11|_T]) -> ok;
pat_loop4(_I, [10|_T]) -> ok;
pat_loop4(_I, [9|_T]) -> ok;
pat_loop4(_I, [8|_T]) -> ok;
pat_loop4(_I, [7|_T]) -> ok;
pat_loop4(_I, [6|_T]) -> ok;
pat_loop4(_I, [5|_T]) -> ok;
pat_loop4(_I, [4|_T]) -> ok;
pat_loop4(_I, [3|_T]) -> ok;
pat_loop4(_I, [1|_T]) -> ok;
pat_loop4(_I, [21|_T]) -> ok;
pat_loop4(_I, [22|_T]) -> ok;
pat_loop4(_I, [23|_T]) -> ok;
pat_loop4(_I, [24|_T]) -> ok;
pat_loop4(_I, [25|_T]) -> ok;
pat_loop4(_I, [26|_T]) -> ok;
pat_loop4(_I, [27|_T]) -> ok;
pat_loop4(I, [0|T]) -> 
    pat_loop4(I-1, [0|T]).

pat_loop5(0, _) -> ok;
pat_loop5(_I, [0, 20|_T]) -> ok;
pat_loop5(_I, [0, 19|_T]) -> ok;
pat_loop5(_I, [0, 18|_T]) -> ok;
pat_loop5(_I, [0, 17|_T]) -> ok;
pat_loop5(_I, [0, 16|_T]) -> ok;
pat_loop5(_I, [0, 15|_T]) -> ok;
pat_loop5(_I, [0, 14|_T]) -> ok;
pat_loop5(_I, [0, 13|_T]) -> ok;
pat_loop5(_I, [0, 12|_T]) -> ok;
pat_loop5(_I, [0, 11|_T]) -> ok;
pat_loop5(_I, [0, 10|_T]) -> ok;
pat_loop5(_I, [0, 9|_T]) -> ok;
pat_loop5(_I, [0, 8|_T]) -> ok;
pat_loop5(_I, [0, 7|_T]) -> ok;
pat_loop5(_I, [0, 6|_T]) -> ok;
pat_loop5(I, [0, 1|T]) -> 
    pat_loop5(I-1, [0,1|T]).

%%%%%%%%%% term traversal representing simple pattern matchhing %%%
%%%%%%%%%                              + some arith
trav(I) ->
    X = very_big(10),
    trav(I, X).

trav(0, _) -> 0;
trav(I, T) ->
    do_trav(T),
    trav(I-1, T).

do_trav(T) when is_tuple(T) ->
    tup_trav(T, 1, 1 + size(T));
do_trav([H|T]) ->
    do_trav(H) + do_trav(T);
do_trav(X) when is_integer(X) -> 1;
do_trav(_X) -> 0.
tup_trav(_T, P, P) -> 0;
tup_trav(T, P, End) ->
    do_trav(element(P, T)) + tup_trav(T, P+1, End).


%% Port I/O
port_io(I) ->
    EstoneCat = get(estone_cat),
    Before = monotonic_time(),
    Pps = make_port_pids(5, I, EstoneCat),  %% 5 ports
    send_procs(Pps, go),
    After = monotonic_time(),
    wait_for_pids(Pps),
    subtr(Before, After).

make_port_pids(0, _, _) -> 
    [];
make_port_pids(NoPorts, J, EstoneCat) ->
    [spawn(?MODULE, ppp, [self(),J,EstoneCat]) | make_port_pids(NoPorts-1, J, EstoneCat)].
ppp(Top, I, EstoneCat) ->
    P = open_port({spawn, EstoneCat}, []),%% cat sits at the other end
    Str = lists:duplicate(200, 88), %% 200 X'es
    Cmd = {self(), {command, Str}},
    receive
	go -> ok
    end,
    ppp_loop(P, I, Cmd),
    Cmd2 = {self(), {command, "abcde"}},
    Res = ppp_loop(P, I, Cmd2),
    P ! {self(), close},
    receive
	{P, closed} ->
	    closed
    end,
    Top ! {self(), Res}.
    
ppp_loop(_P, 0, _) ->
    ok;
ppp_loop(P, I, Cmd) ->
    P ! Cmd,
    receive
	{P, _} ->  %% no match
	    ppp_loop(P, I-1, Cmd)
    end.

%% Working with a very large non-working data set
%% where the passive data resides in remote processes
large_dataset_work(I) ->
    {Minus, Ps} = timer:tc(?MODULE, mk_big_procs, [?BIGPROCS]),
    trav(I),
    lists(I),
    send_procs(Ps, stop),
    Minus. %% Don't count time to create the big procs.

mk_big_procs(0) -> [];
mk_big_procs(I) ->
    [ mk_big_proc()| mk_big_procs(I-1)].

mk_big_proc() ->
    P = spawn(?MODULE, big_proc, []),
    P ! {self(), running},
    receive
	{P, yes} -> P
    end.

big_proc() ->
    X = very_big(?BIGPROC_SIZE), %% creates a big heap
    Y = very_big(?BIGPROC_SIZE),
    Z = very_big(?BIGPROC_SIZE),

    receive
	{From, running} ->
	    From ! {self(), yes}
    end,
    receive
	stop ->
	    {X, Y, Z}  %% Can't be garbed away now by very (not super)
                       %% smart compiler
    end.

%% Working with a large non-working data set
%% where the data resides in the local process.
large_local_dataset_work(I) ->
    {Minus, _Data} = timer:tc(?MODULE, very_big, [?BIGPROC_SIZE]),
    trav(I),
    lists(I),
    Minus.


%% Fast allocation and also deallocation that is gc test
%% Important to not let variable linger on the stack un-necessarily
alloc(0) -> 0;
alloc(I) ->
    _X11 = very_big(),
    _X12 = very_big(),
    _X13 = very_big(),
    _Z = [_X14 = very_big(),
	  _X15 = very_big(),
	  _X16 = very_big()],
    _X17 = very_big(),
    _X18 = very_big(),
    _X19 = very_big(),
    _X20 = very_big(),
    _X21 = very_big(),
    _X22 = very_big(),
    _X23 = very_big(),
    _X24 = very_big(),
    alloc(I-1).

%% Time to call bif's
%% Lot's of element stuff which reflects the record code which
%% is becoming more and more common
bif_dispatch(0) ->
    0;
bif_dispatch(I) ->
    disp(),    disp(),    disp(),    disp(),    disp(),    disp(),
    disp(),    disp(),    disp(),    disp(),    disp(),    disp(),
    bif_dispatch(I-1).

disp() ->
    Tup = {a},
    L = [x],
    self(),self(),self(),self(),self(),self(),self(),self(),self(),
    make_ref(),
    atom_to_list(''),
    _X = list_to_atom([]),
    tuple_to_list({}),
    _X2 = list_to_tuple([]),
    element(1, Tup),
    element(1, Tup),
    _Elem = element(1, Tup),element(1, Tup),element(1, Tup),element(1, Tup),
    element(1, Tup),element(1, Tup),element(1, Tup),element(1, Tup),
    element(1, Tup),element(1, Tup),element(1, Tup),element(1, Tup),
    element(1, Tup),element(1, Tup),element(1, Tup),element(1, Tup),
    setelement(1, Tup,k),
    setelement(1, Tup,k),
    setelement(1, Tup,k),setelement(1, Tup,k),setelement(1, Tup,k),
    setelement(1, Tup,k),setelement(1, Tup,k),setelement(1, Tup,k),
    setelement(1, Tup,k),
    setelement(1, Tup,k),
    setelement(1, Tup,k),
    setelement(1, Tup,k),
    _Y = setelement(1, Tup,k),
    _Date = date(), time(),
    put(a, 1),
    get(a),
    erase(a),
    hd(L),
    tl(L),
    _Len = length(L),length(L),length(L),length(L),
    node(),node(),node(),node(),node(),node(),node(),node(),
    S=self(),
    node(S),node(S),node(S),
    size(Tup),
    _W = whereis(code_server),whereis(code_server),
    whereis(code_server),whereis(code_server),
    whereis(code_server),whereis(code_server),
    _W2 = whereis(code_server).
    
    
%% Generic server like behaviour
generic(I) ->
    register(funky, spawn(?MODULE, gserv, [funky, ?MODULE, [], []])),
    g_loop(I).

g_loop(0) ->
    exit(whereis(funky), kill),
    0;
g_loop(I) ->
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [xyz]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [xyz]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    g_loop(I-1).

req(Name, Req) ->
    R = make_ref(),
    Name ! {self(), R, Req},
    receive
	{Name, R, Reply} -> Reply
    after 2000 ->
	    exit(timeout)
    end.

gserv(Name, Mod, State, Debug) ->
    receive
	{From, Ref, {call, Req}} when Debug == [] ->
	    case catch apply(Mod, handle_call, [From, State, Req]) of
		{reply, Reply, State2} ->
		    From ! {Name, Ref, Reply},
		    gserv(Name, Mod, State2, Debug);
		{noreply, State2} ->
		    gserv(Name, Mod, State2, Debug);
		{'EXIT', Reason} ->
		    exit(Reason)
	    end;
	{_From, _Ref, _Req} when Debug /= [] ->
	    exit(nodebug)
    end.

handle_call(_From, _State, [xyz]) ->
    R = atom_to_list(xyz),
    {reply, R, []};
handle_call(_From, State, [abc]) ->
    R = 1 + 3,
    {reply, R, [R | State]}.

		    

%% Binary handling, creating, manipulating and sending binaries
binary_h(I) ->
    Before = monotonic_time(),
    P = spawn(?MODULE, echo, [self()]),
    B = list_to_binary(lists:duplicate(2000, 5)),
    After = monotonic_time(),
    Compensate = subtr(Before, After),
    binary_h_2(I, P, B),
    Compensate.
    
binary_h_2(0, P, _B) ->
    exit(P, kill);
binary_h_2(I, P, B) ->
    echo_loop(P, 20, B),
    split_loop(B, {abc,1,2222,self(),"ancnd"}, 100),
    binary_h_2(I-1, P, B).

split_loop(_B, _, 0) -> 
    ok;
split_loop(B, Term, I) ->
    {X, Y} = split_binary(B, I),
    size(X),
    binary_to_list(Y, 1, 2),
    binary_to_term(term_to_binary(Term)),
    split_loop(B, Term, I-1).
    

echo_loop(_P, 0, _B) -> 
    k;
echo_loop(P, I, B) ->
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    echo_loop(P, I-1, B).
    

ets(0) -> 
    0;
ets(I) ->
    T1 = ets:new(a, [set]),
    T2 = ets:new(c, [bag, private]),
    L = [T1, T2],
    run_tabs(L, L, 1),
    ets:delete(T1),
    ets:delete(T2),
    ets(I-1).

run_tabs(_, _, 0) ->
    ok;
run_tabs([], L, I) ->
    run_tabs(L, L, I-1);
run_tabs([Tab|Tail], L, I) ->
    Begin = I * 20,
    End = (I+1) * 20,
    run_tab(Tab, Begin, End, I),
    run_tabs(Tail, L, I).

run_tab(_Tab, X, X, _) ->
    ok;
run_tab(Tab, Beg, End, J) ->
    ets:insert(Tab, {Beg, J}),
    ets:insert(Tab, {J, Beg}),
    ets:insert(Tab, {{foo,Beg}, J}),
    ets:insert(Tab, {{foo, J}, Beg}),
    ets:delete(Tab, haha),
    ets:match_delete(Tab, {k, j}),
    ets:match(Tab, {Beg, '$1'}),
    ets:match(Tab, {'$1', J}),
    ets:delete(Tab, Beg),
    K = ets:first(Tab),
    _K2 = ets:next(Tab, K),
    run_tab(Tab, Beg+1, End, J).
    
    
%%%% Integer arith %%%%%
int_arith(0) -> 
    0;
int_arith(I) ->
    do_arith(I) +
    do_arith(I) +
    do_arith(I) +
    do_arith(I) +
    do_arith(I) +
    do_arith(I) +
    do_arith(I) +
    do_arith(I) +
    do_arith(I) +
	66,
    int_arith(I-1).

do_arith(I) ->    
    do_arith2(I) -
    do_arith2(I) -
    do_arith2(I) -
    do_arith2(I) -
    do_arith2(I) -
    do_arith2(I) -
    do_arith2(I) -
	99.

do_arith2(I) ->
    X = 23,
    _Y = 789 + I,
    Z = I + 1,
    U = (X bsl 1 bsr I) * X div 2 bsr 4,
    U1 = Z + Z + Z + Z + X bsl 4 * 2 bsl 2,
    Z - U + U1 div 2.

    
%%%% Float arith %%%%%
float_arith(0) -> 
    0;
float_arith(I) ->
    f_do_arith(I) +
    f_do_arith(I) +
    f_do_arith(I) +
    f_do_arith(I) +
    f_do_arith(I) +
    f_do_arith(I) +
    f_do_arith(I) +
    f_do_arith(I) +
    f_do_arith(I) +
	66,
    float_arith(I-1).

f_do_arith(I) ->    
    X = 23.4,
    _Y = 789.99 + I,
    Z = I + 1.88,
    U = (X * 1 / I) * X / 2 * 4,
    U1 = Z + Z + Z + Z + X * 4 * 2 / 2,
    Z - U + U1 / 2.

%%%% time to do various function calls
fcalls(0) -> 
    0;
fcalls(I) ->
    local0(400),
    remote0(400),
    app0(400),
    local1(400),
    remote1(400),
    app1(400),
    fcalls(I-1).


local0(0) -> 0;
local0(N) ->
    local0(N-1).

local1(0) -> 0;
local1(N) ->
    1+local1(N-1).

remote0(0) -> 0;
remote0(N) ->
    ?MODULE:remote0(N-1).

remote1(0) -> 0;
remote1(N) ->
    1+?MODULE:remote1(N-1).

app0(0) -> 0;
app0(N) ->
    apply(?MODULE, app0, [N-1]).

app1(0) -> 0;
app1(N) ->
    1 + apply(?MODULE, app1, [N-1]).

%%%%%% jog the time queue implementation
timer(I) ->
    L = [50, 50, 50, 100, 1000, 3000, 8000, 50000, 100000],
    timer(I, L).

timer(0, _) -> 0;
timer(N, L) ->
    send_self(100),
    recv(100,L, L),
    timer(N-1).

recv(0, _, _) ->
    ok;
recv(N, [], L) ->
    recv(N, L, L);
recv(N, [Timeout|Tail], L) ->
    receive
        hi_dude ->
            recv(N-1, Tail, L)
    after Timeout ->
            io:format("XXXXX this wasn't supposed to happen???~n", []),
            ok
    end.

send_self(0) ->
    ok;
send_self(N) ->
    self() ! hi_dude,
    send_self(N-1).


%%%%%%%%%%%% managing many links %%%%%
links(I) ->
    L = mk_link_procs(100),
    send_procs(L, {procs, L, I}),
    wait_for_pids(L),
    0.

mk_link_procs(0) -> 
    [];
mk_link_procs(I) ->
    [spawn_link(?MODULE, lproc, [self()]) | mk_link_procs(I-1)].


lproc(Top) ->
    process_flag(trap_exit,true),
    receive
	{procs, Procs, I} ->
	    Top ! {self(), lproc(Procs, Procs, link, I)}
    end.

lproc(_, _, _, 0) ->
    done;
lproc([], Procs, link, I) ->
    lproc(Procs, Procs, unlink, I-1);
lproc([], Procs, unlink, I) ->
    lproc(Procs, Procs, link, I-1);
lproc([Pid|Tail], Procs, unlink, I) ->
    unlink(Pid),
    lproc(Tail, Procs, unlink, I);
lproc([Pid|Tail], Procs, link, I) ->
    link(Pid),
    lproc(Tail, Procs, unlink, I).



%%%%%%%%%%% various utility functions %%%%%%%

echo(Pid) ->
    receive
	X -> Pid ! X,
	     echo(Pid)
    end.

very_big() -> 
    very_big(2).
very_big(0) -> [];
very_big(I) ->
    {1,2,3,a,v,f,r,t,y,u,self(), self(), self(), 
     "22222222222222222", {{"234", self()}}, 
     [[very_big(I-1)]]}.
 
big() ->
    {self(), funky_stuff, baby, {1, [123, true,[]], "abcdef"}}.

small() -> {self(), true}.    
    
%% Wait for a list of children to respond    
wait_for_pids([]) -> 
    ok;
wait_for_pids([P|Tail]) ->
    receive 
	{P, _Res} -> wait_for_pids(Tail)
    end.

send_procs([P|Tail], Msg) -> P ! Msg, send_procs(Tail, Msg);
send_procs([], _) -> ok.
