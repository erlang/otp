%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
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

-module(bif_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 display/1, display_huge/0,
	 types/1,
	 t_list_to_existing_atom/1,os_env/1,otp_7526/1,
	 binary_to_atom/1,binary_to_existing_atom/1,
	 atom_to_binary/1,min_max/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [types, t_list_to_existing_atom, os_env, otp_7526,
     display,
     atom_to_binary, binary_to_atom, binary_to_existing_atom,
     min_max].

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


init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog=?t:timetrap(?t:minutes(1)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).


display(suite) ->
    [];
display(doc) ->
    ["Uses erlang:display to test that erts_printf does not do deep recursion"];
display(Config) when is_list(Config) ->
    Pa = filename:dirname(code:which(?MODULE)),
    {ok, Node} = test_server:start_node(display_huge_term,peer,
					[{args, "-pa "++Pa}]),
    true = rpc:call(Node,?MODULE,display_huge,[]),
    test_server:stop_node(Node),
    ok.

display_huge() ->
    erlang:display(deeep(100000)).

deeep(0,Acc) ->
    Acc;
deeep(N,Acc) ->
    deeep(N-1,[Acc|[]]).

deeep(N) ->
    deeep(N,[hello]).


types(Config) when is_list(Config) ->
    c:l(erl_bif_types),
    case erlang:function_exported(erl_bif_types, module_info, 0) of
	false ->
	    %% Fail cleanly.
	    ?line ?t:fail("erl_bif_types not compiled");
	true ->
	    types_1()
    end.

types_1() ->
    ?line List0 = erlang:system_info(snifs),

    %% Ignore missing type information for hipe BIFs.
    ?line List = [MFA || {M,_,_}=MFA <- List0, M =/= hipe_bifs],

    case [MFA || MFA <- List, not known_types(MFA)] of
	[] ->
	    types_2(List);
	BadTypes ->
	    io:put_chars("No type information:\n"),
	    io:format("~p\n", [lists:sort(BadTypes)]),
	    ?line ?t:fail({length(BadTypes),bifs_without_types})
    end.

types_2(List) ->
    BadArity = [MFA || {M,F,A}=MFA <- List,
		       begin
			   Types = erl_bif_types:arg_types(M, F, A),
			   length(Types) =/= A
		       end],
    case BadArity of
	[] ->
	    types_3(List);
	[_|_] ->
	    io:put_chars("Bifs with bad arity\n"),
	    io:format("~p\n", [BadArity]),
	    ?line ?t:fail({length(BadArity),bad_arity})
    end.

types_3(List) ->
    BadSmokeTest = [MFA || {M,F,A}=MFA <- List,
			   begin
			       try erl_bif_types:type(M, F, A) of
				   Type ->
				       %% Test that type is returned.
				       not erl_types:is_erl_type(Type)
			       catch
				   Class:Error ->
				       io:format("~p: ~p ~p\n",
						 [MFA,Class,Error]),
				       true
			       end
			   end],
    case BadSmokeTest of
	[] ->
	    ok;
	[_|_] ->
	    io:put_chars("Bifs with failing calls to erlang_bif_types:type/3 "
			 "(or with bogus return values):\n"),
	    io:format("~p\n", [BadSmokeTest]),
	    ?line ?t:fail({length(BadSmokeTest),bad_smoke_test})
    end.

known_types({M,F,A}) ->
    erl_bif_types:is_known(M, F, A).

t_list_to_existing_atom(Config) when is_list(Config) ->
    ?line all = list_to_existing_atom("all"),
    ?line ?MODULE = list_to_existing_atom(?MODULE_STRING),
    ?line UnlikelyStr = "dsfj923874390867er869fds9864y97jhg3973qerueoru",
    try
	?line list_to_existing_atom(UnlikelyStr),
	?line ?t:fail()
    catch
	error:badarg -> ok
    end,

    %% The compiler has become smarter! We need the call to id/1 in
    %% the next line.
    ?line UnlikelyAtom = list_to_atom(id(UnlikelyStr)),
    ?line UnlikelyAtom = list_to_existing_atom(UnlikelyStr),
    ok.

os_env(doc) ->
    [];
os_env(suite) ->
    [];
os_env(Config) when is_list(Config) ->
    ?line EnvVar1 = "MjhgvFDrresdCghN mnjkUYg vfrD",
    ?line false = os:getenv(EnvVar1),
    ?line true = os:putenv(EnvVar1, "mors"),
    ?line "mors" = os:getenv(EnvVar1),
    ?line true = os:putenv(EnvVar1, ""),
    ?line case os:getenv(EnvVar1) of
	      "" -> ?line ok;
	      false -> ?line ok;
	      BadVal -> ?line ?t:fail(BadVal)
	  end,
    %% os:putenv and os:getenv currently uses a temp buf of size 1024
    %% for storing key+value
    ?line os_env_long(1010, 1030, "hej hopp").
    
os_env_long(Min, Max, _Value) when Min > Max ->
    ?line ok;
os_env_long(Min, Max, Value) ->
    ?line EnvVar = lists:duplicate(Min, $X),
    ?line true = os:putenv(EnvVar, Value),
    ?line Value = os:getenv(EnvVar),
    ?line true = os:putenv(EnvVar, ""),
    ?line os_env_long(Min+1, Max, Value).

otp_7526(doc) ->    
    ["Test that string:to_integer does not Halloc in wrong order."];
otp_7526(Config) when is_list(Config) ->
    ok = test_7526(256).

iterate_7526(0, Acc) -> Acc;
iterate_7526(N, Acc) ->
    iterate_7526(N - 1,
		 [case string:to_integer("9223372036854775808,\n") of
		      {Int, _Foo} -> Int
		  end | Acc]).

do_test_7526(N,M) ->
    {Self, Ref} = {self(), make_ref()},
    T = erlang:make_tuple(M,0),
    spawn_opt(fun()->
		      L = iterate_7526(N, []),
		      BadList = [X || X <- L, X =/= 9223372036854775808],
		      BadLen = length(BadList),
		      M = length(tuple_to_list(T)),
		      %%io:format("~b bad conversions: ~p~n", [BadLen, BadList]),
		      Self ! {done, Ref, BadLen}
	      end,
	      [link,{fullsweep_after,0}]),
	receive {done, Ref, Len} -> Len end.


test_7526(0) ->
    ok;
test_7526(N) ->
    case do_test_7526(1000,N) of
	0 -> test_7526(N-1);
	Other ->
	    {error,N,Other}
    end.

-define(BADARG(E), {'EXIT',{badarg,_}} = (catch E)).
-define(SYS_LIMIT(E), {'EXIT',{system_limit,_}} = (catch E)).

binary_to_atom(Config) when is_list(Config) ->
    HalfLong = lists:seq(0, 127),
    HalfLongAtom = list_to_atom(HalfLong),
    HalfLongBin = list_to_binary(HalfLong),
    Long = lists:seq(0, 254),
    LongAtom = list_to_atom(Long),
    LongBin = list_to_binary(Long),

    %% latin1
    ?line '' = test_binary_to_atom(<<>>, latin1),
    ?line '\377' = test_binary_to_atom(<<255>>, latin1),
    ?line HalfLongAtom = test_binary_to_atom(HalfLongBin, latin1),
    ?line LongAtom = test_binary_to_atom(LongBin, latin1),

    %% utf8
    ?line '' = test_binary_to_atom(<<>>, utf8),
    ?line HalfLongAtom = test_binary_to_atom(HalfLongBin, utf8),
    ?line HalfLongAtom = test_binary_to_atom(HalfLongBin, unicode),
    ?line [] = [C || C <- lists:seq(128, 255),
		     begin
			 list_to_atom([C]) =/=
			     test_binary_to_atom(<<C/utf8>>, utf8)
		     end],

    %% badarg failures.
    ?line fail_binary_to_atom(atom),
    ?line fail_binary_to_atom(42),
    ?line fail_binary_to_atom({a,b,c}),
    ?line fail_binary_to_atom([1,2,3]),
    ?line fail_binary_to_atom([]),
    ?line fail_binary_to_atom(42.0),
    ?line fail_binary_to_atom(self()),
    ?line fail_binary_to_atom(make_ref()),
    ?line fail_binary_to_atom(<<0:7>>),
    ?line fail_binary_to_atom(<<42:13>>),
    ?line ?BADARG(binary_to_atom(id(<<>>), blurf)),
    ?line ?BADARG(binary_to_atom(id(<<>>), [])),

    %% Bad UTF8 sequences.
    ?line ?BADARG(binary_to_atom(id(<<255>>), utf8)),
    ?line ?BADARG(binary_to_atom(id(<<255,0>>), utf8)),
    ?line ?BADARG(binary_to_atom(id(<<0:512/unit:8,255>>), utf8)),
    ?line ?BADARG(binary_to_atom(id(<<0:512/unit:8,255,0>>), utf8)),
    ?line ?BADARG(binary_to_atom(id(<<16#C0,16#80>>), utf8)), %Overlong 0.
    ?line [?BADARG(binary_to_atom(<<C/utf8>>, utf8)) ||
	      C <- lists:seq(256, 16#D7FF)],
    ?line [?BADARG(binary_to_atom(<<C/utf8>>, utf8)) ||
	      C <- lists:seq(16#E000, 16#FFFD)],
    ?line [?BADARG(binary_to_atom(<<C/utf8>>, utf8)) ||
	      C <- lists:seq(16#10000, 16#8FFFF)],
    ?line [?BADARG(binary_to_atom(<<C/utf8>>, utf8)) ||
	      C <- lists:seq(16#90000, 16#10FFFF)],

    %% system_limit failures.
    ?line ?SYS_LIMIT(binary_to_atom(<<0:256/unit:8>>, latin1)),
    ?line ?SYS_LIMIT(binary_to_atom(<<0:257/unit:8>>, latin1)),
    ?line ?SYS_LIMIT(binary_to_atom(<<0:512/unit:8>>, latin1)),
    ?line ?SYS_LIMIT(binary_to_atom(<<0:256/unit:8>>, utf8)),
    ?line ?SYS_LIMIT(binary_to_atom(<<0:257/unit:8>>, utf8)),
    ?line ?SYS_LIMIT(binary_to_atom(<<0:512/unit:8>>, utf8)),
    ok.

test_binary_to_atom(Bin0, Encoding) ->
    Res = binary_to_atom(Bin0, Encoding),
    Res = binary_to_existing_atom(Bin0, Encoding),
    Bin1 = id(<<7:3,Bin0/binary,32:5>>),
    Sz = byte_size(Bin0),
    <<_:3,UnalignedBin:Sz/binary,_:5>> = Bin1,
    Res = binary_to_atom(UnalignedBin, Encoding).

fail_binary_to_atom(Bin) ->
    try
	binary_to_atom(Bin, latin1)
    catch
	error:badarg ->
	    ok
    end,
    try
	binary_to_atom(Bin, utf8)
    catch
	error:badarg ->
	    ok
    end,
    try
	binary_to_existing_atom(Bin, latin1)
    catch
	error:badarg ->
	    ok
    end,
    try
	binary_to_existing_atom(Bin, utf8)
    catch
	error:badarg ->
	    ok
    end.
	

binary_to_existing_atom(Config) when is_list(Config) ->
    ?line UnlikelyBin = <<"ou0897979655678dsfj923874390867er869fds973qerueoru">>,
    try
	?line binary_to_existing_atom(UnlikelyBin, latin1),
	?line ?t:fail()
    catch
	error:badarg -> ok
    end,

    try
	?line binary_to_existing_atom(UnlikelyBin, utf8),
	?line ?t:fail()
    catch
	error:badarg -> ok
    end,

    ?line UnlikelyAtom = binary_to_atom(id(UnlikelyBin), latin1),
    ?line UnlikelyAtom = binary_to_existing_atom(UnlikelyBin, latin1),
    ok.


atom_to_binary(Config) when is_list(Config) ->
    HalfLong = lists:seq(0, 127),
    HalfLongAtom = list_to_atom(HalfLong),
    HalfLongBin = list_to_binary(HalfLong),
    Long = lists:seq(0, 254),
    LongAtom = list_to_atom(Long),
    LongBin = list_to_binary(Long),

    %% latin1
    ?line <<>> = atom_to_binary('', latin1),
    ?line <<"abc">> = atom_to_binary(abc, latin1),
    ?line <<127>> = atom_to_binary('\177', latin1),
    ?line HalfLongBin = atom_to_binary(HalfLongAtom, latin1),
    ?line LongBin = atom_to_binary(LongAtom, latin1),

    %% utf8.
    ?line <<>> = atom_to_binary('', utf8),
    ?line <<>> = atom_to_binary('', unicode),
    ?line <<127>> = atom_to_binary('\177', utf8),
    ?line <<"abcdef">> = atom_to_binary(abcdef, utf8),
    ?line HalfLongBin = atom_to_binary(HalfLongAtom, utf8),
    ?line LongAtomBin = atom_to_binary(LongAtom, utf8),
    ?line verify_long_atom_bin(LongAtomBin, 0),

    %% Failing cases.
    ?line fail_atom_to_binary(<<1>>),
    ?line fail_atom_to_binary(42),
    ?line fail_atom_to_binary({a,b,c}),
    ?line fail_atom_to_binary([1,2,3]),
    ?line fail_atom_to_binary([]),
    ?line fail_atom_to_binary(42.0),
    ?line fail_atom_to_binary(self()),
    ?line fail_atom_to_binary(make_ref()),
    ?line ?BADARG(atom_to_binary(id(a), blurf)),
    ?line ?BADARG(atom_to_binary(id(b), [])),
    ok.

verify_long_atom_bin(<<I/utf8,T/binary>>, I) ->
    verify_long_atom_bin(T, I+1);
verify_long_atom_bin(<<>>, 255) -> ok.

fail_atom_to_binary(Term) ->
    try
	atom_to_binary(Term, latin1)
    catch
	error:badarg ->
	    ok
    end,
    try
	atom_to_binary(Term, utf8)
    catch
	error:badarg ->
	    ok
    end.

min_max(Config) when is_list(Config) ->	
    ?line a = erlang:min(id(a), a),
    ?line a = erlang:min(id(a), b),
    ?line a = erlang:min(id(b), a),
    ?line b = erlang:min(id(b), b),
    ?line a = erlang:max(id(a), a),
    ?line b = erlang:max(id(a), b),
    ?line b = erlang:max(id(b), a),
    ?line b = erlang:max(id(b), b),

    ?line 42.0 = erlang:min(42.0, 42),
    ?line 42.0 = erlang:max(42.0, 42),
    %% And now (R14) they are also autoimported!
    ?line a = min(id(a), a),
    ?line a = min(id(a), b),
    ?line a = min(id(b), a),
    ?line b = min(id(b), b),
    ?line a = max(id(a), a),
    ?line b = max(id(a), b),
    ?line b = max(id(b), a),
    ?line b = max(id(b), b),

    ?line 42.0 = min(42.0, 42),
    ?line 42.0 = max(42.0, 42),

    ok.

%% Helpers
    
id(I) -> I.

