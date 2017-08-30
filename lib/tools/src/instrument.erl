%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
-module(instrument).

-export([holes/1, mem_limits/1, memory_data/0, read_memory_data/1,
	 sort/1, store_memory_data/1, sum_blocks/1,
	 descr/1, type_descr/2, allocator_descr/2, class_descr/2,
	 type_no_range/1, block_header_size/1, store_memory_status/1,
	 read_memory_status/1, memory_status/1]).


-define(OLD_INFO_SIZE, 32). %% (sizeof(mem_link) in pre R9C utils.c)

-define(IHMARKER(H),  element(1, H)).
-define(VSN(H),       element(2, H)).
-define(INFO_SIZE(H), element(3, H)).
-define(TYPEMAP(H),   element(4, H)).

-define(IHDR(H), is_tuple(H), ?IHMARKER(H) =:= instr_hdr).
-define(IHDRVSN(H, V), ?IHDR(H), ?VSN(H) =:= V).

memory_data() ->
    case catch erlang:system_info(allocated) of
	{'EXIT',{Error,_}} ->
	    erlang:error(Error, []);
	{'EXIT',Error} ->
	    erlang:error(Error, []);
	Res ->
	    Res
    end.

store_memory_data(File) ->
    case catch erlang:system_info({allocated, File}) of
	{'EXIT',{Error,_}} ->
	    erlang:error(Error, [File]);
	{'EXIT',Error} ->
	    erlang:error(Error, [File]);
	Res ->
	    Res
    end.

memory_status(Type) when is_atom(Type) ->
    case catch erlang:system_info({allocated, status, Type}) of
	{'EXIT',{Error,_}} ->
	    erlang:error(Error, [Type]);
	{'EXIT',Error} ->
	    erlang:error(Error, [Type]);
	Res ->
	    Res
    end;
memory_status(Type) ->
    erlang:error(badarg, [Type]).

store_memory_status(File) when is_list(File) ->
    case catch erlang:system_info({allocated, status, File}) of
	{'EXIT',{Error,_}} ->
	    erlang:error(Error, [File]);
	{'EXIT',Error} ->
	    erlang:error(Error, [File]);
	Res ->
	    Res
    end;
store_memory_status(File) ->
    erlang:error(badarg, [File]).

read_memory_data(File) when is_list(File) ->
    case file:consult(File) of
	{ok, [Hdr|MD]} when ?IHDR(Hdr) ->
	    {Hdr, MD};
	{ok, [{T,A,S,undefined}|_] = MD} when is_integer(T),
					      is_integer(A),
					      is_integer(S) ->
	    {{instr_hdr, 1, ?OLD_INFO_SIZE}, MD};
	{ok, [{T,A,S,{X,Y,Z}}|_] = MD} when is_integer(T),
					    is_integer(A),
					    is_integer(S),
					    is_integer(X),
					    is_integer(Y),
					    is_integer(Z) ->
	    {{instr_hdr, 1, ?OLD_INFO_SIZE}, MD};
	{ok, _} ->
	    {error, eio};
	Error ->
	    Error
    end;
read_memory_data(File) ->
    erlang:error(badarg, [File]).

read_memory_status(File) when is_list(File) ->
    case file:consult(File) of
	{ok, [{instr_vsn, _}|Stat]} ->
	    Stat;
	{ok, _} ->
	    {error, eio};
	Error ->
	    Error
    end;
read_memory_status(File) ->
    erlang:error(badarg, [File]).

holes({Hdr, MD}) when ?IHDR(Hdr) ->
    check_holes(?INFO_SIZE(Hdr), MD).

check_holes(_ISz, []) ->
    ok;
check_holes(ISz, [E | L]) ->
    check_holes(ISz, E, L).

check_holes(_ISz, _E1, []) ->
    io:format("~n");
check_holes(ISz, E1, [E2 | Rest]) ->
    check_hole(ISz, E1, E2),
    check_holes(ISz, E2, Rest).

check_hole(ISz, {_,P1,S1,_}, {_,P2,_,_}) ->
    End = P1+S1,
    Hole = P2 - (End + ISz),
    if
	Hole =< 7 ->
	    ok;
	true ->
	    io:format(" ~p", [Hole])
    end.

sum_blocks({Hdr, L}) when ?IHDR(Hdr) ->
    lists:foldl(fun({_,_,S,_}, Sum) -> S+Sum end,
		0,
		L).

mem_limits({Hdr, L}) when ?IHDR(Hdr) ->
    {_, P1, _, _} = hd(L),
    {_, P2, S2, _} = lists:last(L),
    {P1, P2+S2}.

sort({Hdr, MD}) when ?IHDR(Hdr) ->
    {Hdr, lists:keysort(2, MD)}.

descr({Hdr, MD} = ID) when ?IHDR(Hdr) ->
    {Hdr, lists:map(fun ({TN, Addr, Sz, {0, N, S}}) ->
			    {type_descr(ID, TN),
			     Addr,
			     Sz,
			     list_to_pid("<0."
					 ++ integer_to_list(N)
					 ++ "."
					 ++ integer_to_list(S)
					 ++ ">")};
			({TN, Addr, Sz, undefined}) ->
			    {type_descr(ID, TN),
			     Addr,
			     Sz,
			     undefined}
		    end,
		    MD)}.

block_header_size({Hdr, _}) when ?IHDR(Hdr) ->
    ?INFO_SIZE(Hdr).

type_descr({Hdr, _}, TypeNo) when ?IHDRVSN(Hdr, 2),
				  is_integer(TypeNo) ->
    case catch element(1, element(TypeNo, ?TYPEMAP(Hdr))) of
	{'EXIT', _} -> invalid_type;
	Type -> Type
    end;
type_descr({Hdr, _}, TypeNo) when ?IHDRVSN(Hdr, 1),
				  is_integer(TypeNo) ->
    type_string(TypeNo).


allocator_descr({Hdr, _}, TypeNo) when ?IHDRVSN(Hdr, 2), is_integer(TypeNo) ->
    case catch element(2, element(TypeNo, ?TYPEMAP(Hdr))) of
	{'EXIT', _} -> invalid_type;
	Type -> Type
    end;
allocator_descr({Hdr, _}, TypeNo) when ?IHDRVSN(Hdr, 1), is_integer(TypeNo) ->
    "unknown".

class_descr({Hdr, _}, TypeNo) when ?IHDRVSN(Hdr, 2), is_integer(TypeNo) ->
    case catch element(3, element(TypeNo, ?TYPEMAP(Hdr))) of
	{'EXIT', _} -> invalid_type;
	Type -> Type
    end;
class_descr({Hdr, _}, TypeNo) when ?IHDRVSN(Hdr, 1), is_integer(TypeNo) ->
    "unknown".

type_no_range({Hdr, _}) when ?IHDRVSN(Hdr, 2) ->
    {1, tuple_size(?TYPEMAP(Hdr))};
type_no_range({Hdr, _}) when ?IHDRVSN(Hdr, 1) ->
    {-1, 1000}.

type_string(-1) ->
    "unknown";
type_string(1) ->
    "atom text";
type_string(11) ->
    "atom desc";
type_string(2) ->
    "bignum (big_to_list)";
type_string(31) ->
    "fixalloc";
type_string(32) ->
    "unknown fixalloc block";
type_string(33) ->
    "message buffer";
type_string(34) ->
    "message link";
type_string(4) ->
    "estack";
type_string(40) ->
    "db table vec";
type_string(41) ->
    "db tree select buffer";
type_string(43) ->
    "db hash select buffer";
type_string(44) ->
    "db hash select list";
type_string(45) ->
    "db match prog stack";
type_string(46) ->
    "db match prog heap data";
type_string(47) ->
    "db temp buffer";
type_string(48) ->
    "db error";
type_string(49) ->
    "db error info";
type_string(50) ->
    "db trans tab";
type_string(51) ->
    "db segment";
type_string(52) ->
    "db term";
type_string(53) ->
    "db add_counter";
type_string(54) ->
    "db segment table";
type_string(55) ->
    "db table (fix)";
type_string(56) ->
    "db bindings";
type_string(57) ->
    "db counter";
type_string(58) ->
    "db trace vec";
type_string(59) ->
    "db fixed deletion";
type_string(60) ->
    "binary (external.c)";
type_string(61) ->
    "binary";
type_string(62) ->
    "procbin (fix)";
type_string(70) ->
    "driver alloc (io.c)";
type_string(71) ->
    "binary (io.c)";
type_string(72) ->
    "binary vec (io.c)";
type_string(73) ->
    "binary vec 2 (io.c)";
type_string(74) ->
    "io vec (io.c)";
type_string(75) ->
    "io vec 2 (io.c)";
type_string(76) ->
    "temp io buffer (io.c)";
type_string(77) ->
    "temp io buffer 2 (io.c)";
type_string(78) ->
    "line buffer (io.c)";
type_string(8) ->
    "heap";
type_string(801) ->
    "heap (1)";
type_string(802) ->
    "heap (2)";
type_string(803) ->
    "heap (3)";
type_string(804) ->
    "heap (4)";
type_string(805) ->
    "heap (5)";
type_string(821) ->
    "heap fragment (1)";
type_string(822) ->
    "heap fragment (2)";
type_string(830) ->
    "sequential store buffer (for vectors)";
type_string(91) ->
    "process table";
type_string(92) ->
    "process desc";
type_string(110) ->
    "hash buckets";
type_string(111) ->
    "hash table";
type_string(120) ->
    "index init";
type_string(121) ->
    "index table";
type_string(130) ->
    "temp buffer";
type_string(140) ->
    "timer wheel";
type_string(150) ->
    "distribution cache";
type_string(151) ->
    "dmem";
type_string(152) ->
    "distribution table";
type_string(153) ->
    "distribution table buckets";
type_string(154) ->
    "distribution table entry";
type_string(155) ->
    "node table";
type_string(156) ->
    "node table buckets";
type_string(157) ->
    "node table entry";
type_string(160) ->
    "port table";
type_string(161) ->
    "driver entry";
type_string(162) ->
    "port setup";
type_string(163) ->
    "port wait";
type_string(170) ->
    "module";
type_string(171) ->
    "fundef";
type_string(180) ->
    "file table";
type_string(181) ->
    "driver table";
type_string(182) ->
    "poll struct";
type_string(190) ->
    "inet driver";
type_string(200) ->
    "efile driver";
type_string(210) ->
    "gc root set";
type_string(220) ->
    "breakpoint data";
type_string(230) ->
    "async queue";
type_string(231) ->
    "async (exit)";
type_string(232) ->
    "async (driver)";
type_string(240) ->
    "bits buffer";
type_string(241) ->
    "bits temp buffer";
type_string(250) ->
    "modules (loader)";
type_string(251) ->
    "code (loader)";
type_string(252) ->
    "atom tab (loader)";
type_string(253) ->
    "import tab (loader)";
type_string(254) ->
    "export tab (loader)";
type_string(255) ->
    "lable tab (loader)";
type_string(256) ->
    "gen op (loader)";
type_string(257) ->
    "gen op args (loader)";
type_string(258) ->
    "gen op args 2 (loader)";
type_string(259) ->
    "gen op args 3 (loader)";
type_string(260) ->
    "lambdas (loader)";
type_string(261) ->
    "temp int buffer (loader)";
type_string(262) ->
    "temp heap (loader)";
type_string(280) ->
    "dist ctrl msg buffer";
type_string(281) ->
    "dist_buf";
type_string(290) ->
    "call trace buffer";
type_string(300) ->
    "bif timer rec";
type_string(310) ->
    "argument registers";
type_string(320) ->
    "compressed binary temp buffer";
type_string(330) ->
    "term_to_binary temp buffer";
type_string(340) ->
    "proc dict";
type_string(350) ->
    "trace to port temp buffer";
type_string(360) ->
    "lists subtract temp buffer";
type_string(370) ->
    "link (lh)";
type_string(380) ->
    "port call buffer";
type_string(400) ->
    "definite_alloc block";
type_string(_) ->
    invalid_type.

