%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
-module(nc_SUITE).


-include_lib("common_test/include/ct.hrl").

-define(VERSION_MAGIC,       131).

-define(ATOM_EXT,            100).
-define(REFERENCE_EXT,       101).
-define(PORT_EXT,            102).
-define(PID_EXT,             103).
-define(NEW_REFERENCE_EXT,   114).
-define(ATOM_UTF8_EXT,       118).
-define(SMALL_ATOM_UTF8_EXT, 119).
-define(NEW_PID_EXT,         $X).
-define(NEW_PORT_EXT,        $Y).
-define(NEWER_REFERENCE_EXT, $Z).


-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_suite/1,
	 end_per_suite/1,
	 init_per_testcase/2,
	 end_per_testcase/2]).

-export([pid_roundtrip/1,
	 port_roundtrip/1,
	 ref_roundtrip/1,
	 new_float/1,
	 old_stuff/1,
	 binary_roundtrip/1,
	 decompress_roundtrip/1,
	 compress_roundtrip/1,
	 integer_roundtrip/1,
	 fun_roundtrip/1,
	 lists_roundtrip/1,
	 lists_roundtrip_2/1,
	 lists_iterator/1,
	 unicode/1,
	 unicode_list_to_string/1,
	 unicode_string_to_list/1,
	 utf8_atom/1,
	 utf8_pid/1,
	 utf8_port/1,
	 utf8_ref/1,
	 connect/1]).


%% Top of cases

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [pid_roundtrip, port_roundtrip, ref_roundtrip,
     new_float, old_stuff, binary_roundtrip,
     decompress_roundtrip, compress_roundtrip,
     integer_roundtrip, fun_roundtrip, lists_roundtrip,
     lists_roundtrip_2, lists_iterator, unicode,
     unicode_list_to_string, unicode_string_to_list,
     utf8_atom, utf8_pid, utf8_port, utf8_ref,
     connect].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_suite(Config) when is_list(Config) ->
    case case code:priv_dir(jinterface) of
	     {error,bad_name} -> false;
	     P -> filelib:is_dir(P) end of
	true ->
	    jitu:init_all(Config);
	false ->
	    {skip,"No jinterface application"}
    end.

end_per_suite(Config) ->
    jitu:finish_all(Config).



%% Add/remove watchdog before/after each test case.
%%
init_per_testcase(Case, Config) ->
    T = case atom_to_list(Case) of
	    "unicode"++_ -> 240;
	    _ -> 120
	end,
    WatchDog = test_server:timetrap(test_server:seconds(T)),
    [{watchdog, WatchDog}| Config].

end_per_testcase(_Case, Config) ->
    jitu:kill_all_jnodes(),
    WatchDog = ?config(watchdog, Config),
    test_server:timetrap_cancel(WatchDog).


%%
%% Test cases
%%

pid_roundtrip(doc) -> [];
pid_roundtrip(suite) -> [];
pid_roundtrip(Config) when is_list(Config)->
    ThisNode = {node(), erlang:system_info(creation)},
    RemPids = [mk_pid({gurka@sallad, Cr}, Num, Ser)
	       || Cr <- [1,2,3,4,16#adec0ded],
		  {Num, Ser} <- [{4711,4711},{32767, 8191}]],
    do_echo([self(),
	     mk_pid(ThisNode, 4711, 4711),
	     mk_pid(ThisNode, 32767, 8191)
	     | RemPids],
	    Config).

fun_roundtrip(doc) -> [];
fun_roundtrip(suite) -> [];
fun_roundtrip(Config) when is_list(Config)->
    do_echo([fun(A, B) -> A + B end,
	     fun(A) -> lists:reverse(A) end,
	     fun() -> ok end,
	     fun fun_roundtrip/1],
	    Config).

port_roundtrip(doc) -> [];
port_roundtrip(suite) -> [];
port_roundtrip(Config) when is_list(Config)->
    ThisNode = {node(), erlang:system_info(creation)},
    RemPorts = [mk_port({gurka@sallad, Cr}, Num)
		|| Cr <- [1,2,3,4,16#adec0ded],
		   Num <- [4711, 268435455]],
    do_echo([hd(erlang:ports()),
	     mk_port(ThisNode, 4711),
	     mk_port(ThisNode, 268435455)
	     | RemPorts],
	    Config).

ref_roundtrip(doc) -> [];
ref_roundtrip(suite) -> [];
ref_roundtrip(Config) when is_list(Config)->
    ThisNode = {node(), erlang:system_info(creation)},
    RemRefs = [mk_ref({gurka@sallad, Cr}, Words)
	       || Cr <- [1,2,3,4,16#adec0ded],
		  Words <- [[4711],
			    [4711, 4711, 4711],
			    [262143, 4294967295, 4294967295]]],
    do_echo([make_ref(),
	     mk_ref(ThisNode, [4711]),
	     mk_ref(ThisNode, [4711, 4711, 4711]),
	     mk_ref(ThisNode, [262143, 4294967295, 4294967295])
	     | RemRefs],
	    Config).

new_float(doc) -> [];
new_float(suite) -> [];
new_float(Config) when is_list(Config)->
    Two16 = float(1 bsl 16),
    X = math:sqrt(2),
    Floats = lists:reverse(seq(1/X, 63, fun(Y) -> Y / Two16 end),
			   [0.0|seq(X, 63, fun(Y) -> Y * Two16 end)]),
    io:format("~w", [Floats]),
    do_echo(Floats, Config).

old_stuff(doc) -> [];
old_stuff(suite) -> [];
old_stuff(Config) when is_list(Config)->
    Terms = [0.0,math:sqrt(2)],
    OutTrans =
	fun (D) ->
		{self(),term_to_binary(D, [{minor_version,0}]),binary}
	end,
    InTrans =
	fun (Echoer, D, {Echoer,D,binary}) ->
		ok
	end,
    do_echo(Terms, Config, OutTrans, InTrans).

binary_roundtrip(doc) -> [];
binary_roundtrip(suite) -> [];
binary_roundtrip(Config) when is_list(Config) ->
    do_echo([<<17>>,
	     <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17>>,
	     <<3:2>>,
	     <<3,7:3>>,
	     <<>>],
	    Config).

decompress_roundtrip(doc) -> [];
decompress_roundtrip(suite) -> [];
decompress_roundtrip(Config) when is_list(Config) ->
	RandomBin = erlang:term_to_binary(lists:seq(1, 5 * 1024 * 1024)), % roughly 26MB
	<<RandomBin1k:1024/binary,_/binary>> = RandomBin,
	<<RandomBin1M:1048576/binary,_/binary>> = RandomBin,
	<<RandomBin10M:10485760/binary,_/binary>> = RandomBin,
    Terms =
	[{},
	 {a,b,c},
	 [],
	 0.0,
	 math:sqrt(2),
	 <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,31:5>>,
	 "{}",
	 RandomBin1k,
	 RandomBin1M,
	 RandomBin10M,
	 RandomBin,
	 make_ref()],
    OutTrans =
	fun (D) ->
		{self(),term_to_binary(D, [compressed]),binary}
	end,
    InTrans =
	fun (Echoer, D, {Echoer,D,binary}) ->
		ok
	end,
    do_echo(Terms, Config, OutTrans, InTrans).

compress_roundtrip(doc) -> [];
compress_roundtrip(suite) -> [];
compress_roundtrip(Config) when is_list(Config) ->
	RandomBin = erlang:term_to_binary(lists:seq(1, 5 * 1024 * 1024)), % roughly 26MB
	<<RandomBin1k:1024/binary,_/binary>> = RandomBin,
	<<RandomBin1M:1048576/binary,_/binary>> = RandomBin,
	<<RandomBin10M:10485760/binary,_/binary>> = RandomBin,
    Terms =
	[{},
	 {a,b,c},
	 [],
	 0.0,
	 math:sqrt(2),
	 <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,31:5>>,
	 "{}",
	 RandomBin1k,
	 RandomBin1M,
	 RandomBin10M,
	 RandomBin,
	 make_ref()],
    OutTrans =
	fun (D) ->
		{self(),D,compress}
	end,
    InTrans =
	fun (Echoer, D, {Echoer,B,compress}) ->
		D = binary_to_term(B)
	end,
    do_echo(Terms, Config, OutTrans, InTrans).



integer_roundtrip(doc) -> [];
integer_roundtrip(suite) -> [];
integer_roundtrip(Config) when is_list(Config) ->
    Xs = [1 bsl X || X <- [26,27,28,29,30,31,32,33,
			   62,63,64,65,
			   126,127,128,129]],
    Terms = [0,1,-1,-2]
	++lists:flatmap(fun (X) -> [X-2,X-1,X,-X+1,-X,-X-1] end,
			Xs),
    io:format("~w", [Terms]),
    OutTrans =
	fun (V) ->
		{self(),V,bigint}
	end,
    InTrans =
	fun (Echoer, V, {Echoer,{V,W,X,L,U},bigint}) ->
		Bitlength = bitlength(V),
		{w,W} = {w,signum(V) * Bitlength},
		Y = V band 16#FFFFffffFFFFffff,
		{y,Y} = {y,X band 16#FFFFffffFFFFffff},
		{l,L} = {l,if  V =:= X, Bitlength < 64 -> 1;
			       true                    -> 0  end},
		{u,U} = {u,if  V =:= Y, V >= 0, Bitlength =< 64 -> 1;
			       true                             -> 0  end}
	end,
    do_echo(Terms, Config, OutTrans, InTrans).

signum(V) when is_integer(V), V > 0 ->  1;
signum(V) when is_integer(V), V < 0 -> -1;
signum(0) ->                            0.

bitlength(0) ->
    0;
bitlength(-1) ->
    0;
bitlength(V) when is_integer(V) ->
    1 + bitlength(V bsr 1).



lists_roundtrip(doc) -> [];
lists_roundtrip(suite) -> [];
lists_roundtrip(Config) when is_list(Config) ->
    Ls = [lists:seq(1,10),
	  lists:seq(11,17)++last_tail,
	  [{default}],
	  [car|cdr],
	  [[]],
	  []],
    do_echo(Ls, Config).



lists_roundtrip_2(doc) -> [];
lists_roundtrip_2(suite) -> [];
lists_roundtrip_2(Config) when is_list(Config) ->
    Ls = [{[a,b],tail},
	  {[c,d,e],tail},
	  {[],tail},
	  {[f],tail},
	  {[g,h|i],tail},
	  {[j,k,l|m],tail},
	  {[n|o],tail},
	  {[z,1,2,3,4],tail3},
	  {[z,5,6,7],tail3},
	  {[z,8,9],tail3},
	  {[z,10],tail3},
	  {[],tail3},
	  {[z,11,12,13,14|15],tail3},
	  {[z,16,17,18|19],tail3},
	  {[z,20,21|22],tail3},
	  {[z,23|24],tail3},
	  {[z|25],tail3},
	  {"abc123",sub3atom},
	  {"abc",sub3atom},
	  {"abcdefg",codepointBug}
	 ],
    Trans =
	fun ([_|T], tail) ->
		T;
	    (L, tail) when is_list(L) ->
		null;
	    ([_,_,_|T], tail3) ->
		T;
	    (L, tail3) when is_list(L) ->
		null;
	    ([_,_,_|L], sub3atom) ->
		list_to_atom(L);
	    (L, codepointBug) ->
		L
	end,
    OutTrans =
	fun ({L,Twist}) ->
		{self(),L,Twist}
	end,
    InTrans =
	fun (Echoer, {L,Twist}, {Echoer,X,Twist}) ->
		Y = Trans(L, Twist),
		io:format("## ~w ~w ~w ~w~n", [L,Twist,X,Y]),
		X = Y
	end,
    do_echo(Ls, Config, OutTrans, InTrans).




lists_iterator(doc) -> [];
lists_iterator(suite) -> [];
lists_iterator(Config) when is_list(Config) ->
    Ls = [["able ","was ","I ","ere ","I ","saw ","elba"]],
    do_echo(Ls, Config,
	    fun (L) -> {self(),L,strcat} end,
	    fun (Echoer, L, {Echoer,X,strcat}) ->
		    io:format("## ~p ~p~n", [L,X]),
		    X = lists:flatten(L)
	    end).



unicode(doc) -> [];
unicode(suite) -> [];
unicode(Config) when is_list(Config) ->
    S1 = "plain ascii",
    S2 = "iso-latin åäö ñ",
    S3 = "Codepoints... åäö \x{1000}",
    S4 = [0,1,31,32,63,64,127,128,255],
    S5 = [0,1,127,128,255,256,16#d7ff,
	  16#e000,16#fffd,16#10000,16#10ffff],
    Ss = [S1,S2,S3,S4,S5],
    do_echo(unicode_cp_gen([{S,valid} || S <- Ss] ++ cp_gen(71)), Config,
	    fun ({L,invalid}) -> {self(),L,utf8};
		({L,Tag}) -> {self(),L,Tag};
		({L})     -> {self(),L}
	    end,
	    fun (Echoer, {L,invalid}=Out, {Echoer,X,utf8}=In) ->
		    case L of
			X -> ok;
			_ ->
			    ?t:fail({mismatch,Out,In})
		    end;
		(Echoer, {L,Tag}=Out, {Echoer,X,Tag}=In) ->
		    case unicode:characters_to_binary(L, utf8) of
			X -> ok;
			_ ->
			    ?t:fail({mismatch,Out,In})
		    end;
		(Echoer, {L}=Out, {Echoer,X}=In) ->
		    case L of
			X -> ok;
			_ ->
			    ?t:fail({mismatch,Out,In})
		    end
	    end, ["unicode"]).

%% Lazy wrapper to lazy list
unicode_cp_gen([{S,valid}|Ss]) ->
    [{S,utf8},{S}|unicode_cp_gen(Ss)];
unicode_cp_gen([{S,invalid}=St|Ss]) ->
    [St,{S}|unicode_cp_gen(Ss)];
unicode_cp_gen([]) ->
    [];
unicode_cp_gen(Cont) when is_function(Cont, 0) ->
    fun () ->
	    unicode_cp_gen(Cont())
    end.



unicode_list_to_string(doc) -> [];
unicode_list_to_string(suite) -> [];
unicode_list_to_string(Config) when is_list(Config) ->
    do_echo(cp_gen(73), Config,
	    fun ({L,_}) -> {self(),L,to_string_neg_int_list} end,
	    fun (Echoer, {L,invalid}=Out, {Echoer,X,_}=In) ->
		    case L of
			X -> ok;
			_ ->
			    ?t:fail({mismatch,Out,In})
		    end;
		(Echoer, {L,valid}=Out, {Echoer,X,_}=In) ->
		    B = unicode:characters_to_binary(L, unicode, {utf16,big}),
		    case [-D || <<D:16/big>> <= B] of
			X -> ok;
			_ ->
			    ?t:fail({mismatch,Out,In})
		    end
	    end).



unicode_string_to_list(doc) -> [];
unicode_string_to_list(suite) -> [];
unicode_string_to_list(Config) when is_list(Config) ->
    do_echo(cp_gen(79), Config,
	    fun ({L,_}) -> {self(),L,to_neg_int_list} end,
	    fun (Echoer, {L,invalid}=Out, {Echoer,X,_}=In) ->
		    case L of
			X -> ok;
			_ ->
			    ?t:fail({mismatch,Out,In})
		    end;
		(Echoer, {L,valid}=Out, {Echoer,X,_}=In) ->
		    case [-C || C <- L] of
			X -> ok;
			_ ->
			    ?t:fail({mismatch,Out,In})
		    end
	    end, ["unicode"]).


evil_smiley() ->
    <<240,159,152,136>>.

evil_smileys(0) ->
    [];
evil_smileys(N) ->
    [evil_smiley() | evil_smileys(N-1)].

utf8_atom(Config) when is_list(Config) ->
    ES = evil_smiley(),
    SmallUA = binary_to_term(list_to_binary([?VERSION_MAGIC,
					     ?SMALL_ATOM_UTF8_EXT,
					     size(ES),
					     ES])),
    true = is_atom(SmallUA),
    NoESs = 300 div size(ES),
    ESs = evil_smileys(NoESs),
    LargeUA = binary_to_term(list_to_binary([?VERSION_MAGIC,
					     ?ATOM_UTF8_EXT,
					     uint16_be(NoESs*size(ES)),
					     ESs])),
    true = is_atom(LargeUA),
    erlang:display({atom, SmallUA, LargeUA}),
    do_echo([SmallUA, LargeUA], Config).

utf8_nodenames_ext() ->
    H = "@host",
    ES = evil_smiley(),
    SmallUANodeExt = list_to_binary([?SMALL_ATOM_UTF8_EXT,
				     size(ES)+length(H),
				     ES,
				     H]),
    NoESs = 300 div size(ES),
    ESs = evil_smileys(NoESs),
    LargeUANodeExt = list_to_binary([?ATOM_UTF8_EXT,
				     uint16_be(NoESs*size(ES)+length(H)),
				     ESs,
				     H]),
    {SmallUANodeExt, LargeUANodeExt}.

utf8_pid(Config) when is_list(Config) ->
    {SmallUANodeExt, LargeUANodeExt} = utf8_nodenames_ext(),
    SmallPid = mk_pid({SmallUANodeExt, 2}, 4711, 4711),
    LargePid = mk_pid({LargeUANodeExt, 2}, 4711, 4711),
    erlang:display({pid, SmallPid, node(SmallPid)}),
    erlang:display({pid, LargePid, node(LargePid)}),
    do_echo([SmallPid, LargePid], Config).

utf8_port(Config) when is_list(Config) ->
    {SmallUANodeExt, LargeUANodeExt} = utf8_nodenames_ext(),
    SmallPort = mk_port({SmallUANodeExt, 2}, 4711),
    erlang:display({port, SmallPort, node(SmallPort)}),
    LargePort = mk_port({LargeUANodeExt, 2}, 4711),
    erlang:display({port, LargePort, node(LargePort)}),
    do_echo([SmallPort, LargePort], Config).

utf8_ref(Config) when is_list(Config) ->
    {SmallUANodeExt, LargeUANodeExt} = utf8_nodenames_ext(),
    SmallRef = mk_ref({SmallUANodeExt, 2}, [4711, 4711, 4711]),
    erlang:display({ref, SmallRef, node(SmallRef)}),
    LargeRef = mk_ref({LargeUANodeExt, 2}, [4711, 4711, 4711]),
    erlang:display({ref, LargeRef, node(LargeRef)}),
    do_echo([SmallRef, LargeRef], Config).
    

%% Lazy list
cp_gen(N) ->
    cp_gen(N, -1, 16#110000).

cp_gen(N, Start, End) ->
    cp_gen(N, Start, End, cp_validity(Start), [], 0, [], 0).

cp_gen(N, U, End, PrevValidity, Acc, Len, Ss, Ls) when Len >= N ->
    cp_gen(N, U, End, PrevValidity, [], 0, [{Acc,PrevValidity}|Ss], Ls+1);
cp_gen(N, U, End, PrevValidity, Acc, Len, Ss, Ls) when Ls >= N ->
    Ss ++ fun () ->
		  cp_gen(N, U, End, PrevValidity, Acc, Len, [], 0)
	  end;
cp_gen(_, U, End, _, Acc, _, Ss, _) when U > End ->
    [{Acc,valid}|Ss];
cp_gen(N, U, End, PrevValidity, Acc, Len, Ss, Ls) ->
    Validity = cp_validity(U),
    NextU = U+1,
    {NextAcc,NextLen} =	case Validity of
			    valid -> {[U|Acc],Len+1};
			    invalid -> {Acc,Len}
			end,
    {NextSs,NextLs} = case Validity of
			  PrevValidity -> {Ss,Ls};
			  valid -> {[{[U-1],PrevValidity}|Ss],Ls+1};
			  invalid -> {[{[U],Validity}|Ss],Ls+1}
		      end,
    cp_gen(N, NextU, End, Validity, NextAcc, NextLen, NextSs, NextLs).

cp_validity(UnicodeCP) ->
    try <<UnicodeCP/big-utf32>> of
	_ -> valid
    catch
	error:_ -> invalid
    end.



connect(doc) -> [];
connect(suite) -> [];
connect(Config) when is_list(Config) ->
    WD = filename:dirname(code:which(?MODULE)),
    {ok,Other} = ?t:start_node(make_name(), slave, [{args,"-pa "++WD}]),
    Action =
	fun (Pid) ->
		JName = node(Pid),
		Hidden = [JName],
		Pid ! {self(),Other},
		receive
		    {Pid,Other,true} ->
			ok;
		    Unexpected1 ->
			?t:fail({result,Unexpected1})
		end,
		Hidden = erlang:nodes(hidden),
		Hidden = rpc:call(Other, erlang, nodes, [hidden]),
		true =
		    rpc:call(Other, erlang, disconnect_node, [JName]),
		[] =
		    rpc:call(Other, erlang, nodes, [hidden]),
		Hidden = erlang:nodes(hidden),
		%% Again
		receive after 2000 -> ok end,
		%% We have no way of knowing when the Java node
		%% detects the nodedown.
		Pid ! {self(),Other},
		receive
		    {Pid,Other,true} ->
			ok;
		    Unexpected2->
			?t:fail({result,Unexpected2})
		end,
		Hidden = rpc:call(Other, erlang, nodes, [hidden])
	end,
    run_server(connection_server, Config, Action, []).



seq(_, 0, _) ->
    [];
seq(X, N, Fun) ->
    [X|seq(Fun(X), N-1, Fun)].

do_echo(DataList, Config) ->
    do_echo(DataList, Config,
	    fun (D) -> % OutTrans
		    {self(),D}
	    end,
	    fun (Echoer, D, {Echoer,D}) -> % InTrans
		    ok
	    end, []).

do_echo(DataList, Config, OutTrans, InTrans) ->
    do_echo(DataList, Config, OutTrans, InTrans, []).

do_echo(DataList, Config, OutTrans, InTrans, ExtraArgs)
  when is_list(DataList), is_list(Config) ->
    run_server(echo_server, Config,
	       fun (Echoer) ->
		       echo_loop(DataList, Echoer, OutTrans, InTrans, [])
	       end,
	       ExtraArgs).

echo_loop([D|Ds], Echoer, OutTrans, InTrans, TermAcc) ->
    OutMsg = OutTrans(D),
    Echoer ! OutMsg,
    %%io:format("echo_server ~p: ~p ! ~P~n", [self(),Echoer,OutMsg,10]),
    receive
	Reply ->
	    %%io:format("echo_server ~p: receive ~P~n", [self(),Reply,10]),
	    InTrans(Echoer, D, Reply)
    end,
    Term = case OutMsg of
	       {_, T, _} -> T;
	       {_, T} -> T
	   end,
    echo_loop(Ds, Echoer, OutTrans, InTrans, [Term | TermAcc]);
echo_loop([], Echoer, _, _, TermAcc) ->
    check_terms(Echoer, TermAcc);
%% Lazy list
echo_loop(Cont, Echoer, OutTrans, InTrans, TermAcc)
  when is_function(Cont, 0) ->
    check_terms(Echoer, TermAcc),
    OutMsg = Echoer ! {self(),undefined,hash_clear},
    %%io:format("echo_server ~p: ~p ! ~P~n", [self(),Echoer,OutMsg,10]),
    receive
	{Echoer,hash_cleared,hash_clear}=Reply ->
	    %%io:format("echo_server ~p: receive ~P~n", [self(),Reply,10]),
	    ok;
	Other ->
	    io:format("echo_server_terms unexpected ~p: receive ~P~n",
		      [self(),Other,10]),
	    ?t:fail({unexpected, Other})
    end,
    echo_loop(Cont(), Echoer, OutTrans, InTrans, []).

check_terms(Echoer, [Term | Rest]) ->
    OutMsg = {self(),Term,hash_lookup},
    Echoer ! OutMsg,
    %%io:format("check_terms ~p: ~p ! ~P~n", [self(),Echoer,OutMsg,10]),
    receive
	{Echoer,true,hash_lookup} = ReplyMsg ->
	    %%io:format("check_terms ~p: receive ~P~n", [self(),ReplyMsg,10]),
	    check_terms(Echoer, Rest);
	Other ->
	    io:format("check_terms unexpected ~p: receive ~P~n",
		      [self(),Other,10]),
	    ?t:fail({unexpected, Other})
    end;
check_terms(_, []) ->
    ok.

run_server(Server, Config, Action, ExtraArgs) ->
    Name = make_name(),
    true = register(Name, self()),
    JName = make_name(),
    spawn_link(fun () ->
		       %% Setting max memory to 256. This is due to
		       %% echo_server sometimes failing with
		       %% OutOfMemoryException one some test machines.
		       ok = jitu:java(?config(java, Config),
				      ?config(data_dir, Config),
				      atom_to_list(Server),
				      [JName,
				       erlang:get_cookie(),
				       node(),
				       Name]++ExtraArgs,
				      " -Xmx256m"),
				      %% " -Xmx256m -DOtpConnection.trace=3"),
		       Name ! {done, JName}
	       end),
    receive
	{Server, JName, Pid} ->
	    ?t:format("~w: ~p (~p)~n",
		      [Server, Pid, node(Pid)]),
	    ?t:format("nodes(hidden): ~p~n",
		      [nodes(hidden)]),
	    Action(Pid),
	    Pid ! bye,
	    receive
		{done, JName} ->
		    ok
	    end;
	Other ->
	    ?t:fail({unexpected,Other})
    end.

%%
%% Utils...
%%

make_name() ->
    {A, B, C} = now(),
    list_to_atom(atom_to_list(?MODULE)
		 ++ "-" ++ integer_to_list(A)
		 ++ "-" ++ integer_to_list(B)
		 ++ "-" ++ integer_to_list(C)).

uint32_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 32 ->
    [(Uint bsr 24) band 16#ff,
     (Uint bsr 16) band 16#ff,
     (Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint32_be(Uint) ->
    exit({badarg, uint32_be, [Uint]}).


uint16_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 16 ->
    [(Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint16_be(Uint) ->
    exit({badarg, uint16_be, [Uint]}).

uint8(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 8 ->
    Uint band 16#ff;
uint8(Uint) ->
    exit({badarg, uint8, [Uint]}).


pid_tag(Creation) when Creation =< 3 -> ?PID_EXT;
pid_tag(_Creation) -> ?NEW_PID_EXT.

enc_creation(Creation) when Creation =< 3 -> uint8(Creation);
enc_creation(Creation) -> uint32_be(Creation).

mk_pid({NodeName, Creation}, Number, Serial) when is_atom(NodeName) ->
    <<?VERSION_MAGIC, NodeNameExt/binary>> = term_to_binary(NodeName),
    mk_pid({NodeNameExt, Creation}, Number, Serial);
mk_pid({NodeNameExt, Creation}, Number, Serial) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      pid_tag(Creation),
					      NodeNameExt,
					      uint32_be(Number),
					      uint32_be(Serial),
					      enc_creation(Creation)])) of
	Pid when is_pid(Pid) ->
	    Pid;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_pid, [{NodeNameExt, Creation}, Number, Serial]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

port_tag(Creation) when Creation =< 3 -> ?PORT_EXT;
port_tag(_Creation) -> ?NEW_PORT_EXT.

mk_port({NodeName, Creation}, Number) when is_atom(NodeName) ->
    <<?VERSION_MAGIC, NodeNameExt/binary>> = term_to_binary(NodeName),
    mk_port({NodeNameExt, Creation}, Number);
mk_port({NodeNameExt, Creation}, Number) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      port_tag(Creation),
					      NodeNameExt,
					      uint32_be(Number),
					      enc_creation(Creation)])) of
	Port when is_port(Port) ->
	    Port;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_port, [{NodeNameExt, Creation}, Number]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

ref_tag(Creation) when Creation =< 3 -> ?NEW_REFERENCE_EXT;
ref_tag(_Creation) -> ?NEWER_REFERENCE_EXT.

mk_ref({NodeName, Creation}, [Number] = NL) when is_atom(NodeName),
						 is_integer(Creation),
						 is_integer(Number) ->
    <<?VERSION_MAGIC, NodeNameExt/binary>> = term_to_binary(NodeName),
    mk_ref({NodeNameExt, Creation}, NL);
mk_ref({NodeNameExt, Creation}, [Number]) when is_integer(Creation),
					       Creation =< 3,
					       is_integer(Number) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?REFERENCE_EXT,
					      NodeNameExt,
					      uint32_be(Number),
					      uint8(Creation)])) of
	Ref when is_reference(Ref) ->
	    Ref;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_ref, [{NodeNameExt, Creation}, [Number]]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end;
mk_ref({NodeName, Creation}, Numbers) when is_atom(NodeName),
					   is_integer(Creation),
					   is_list(Numbers) ->
    <<?VERSION_MAGIC, NodeNameExt/binary>> = term_to_binary(NodeName),
    mk_ref({NodeNameExt, Creation}, Numbers);
mk_ref({NodeNameExt, Creation}, Numbers) when is_integer(Creation),
					      is_list(Numbers) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ref_tag(Creation),
					      uint16_be(length(Numbers)),
					      NodeNameExt,
					      enc_creation(Creation),
					      lists:map(fun (N) ->
								uint32_be(N)
							end,
							Numbers)])) of
	Ref when is_reference(Ref) ->
	    Ref;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_ref, [{NodeNameExt, Creation}, Numbers]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.
