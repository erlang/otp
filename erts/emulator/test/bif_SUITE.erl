%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2024. All Rights Reserved.
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

-module(bif_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-export([all/0, suite/0, init_per_testcase/2, end_per_testcase/2]).

-export([display/1, display_huge/0, display_string/1,
	 erl_bif_types/1,guard_bifs_in_erl_bif_types/1,
	 shadow_comments/1,list_to_utf8_atom/1,
	 specs/1,improper_bif_stubs/1,
	 t_list_to_existing_atom/1,os_env/1,otp_7526/1,
	 t_binary_to_atom/1,t_binary_to_existing_atom/1,
	 t_atom_to_binary/1,min_max/1, erlang_halt/1,
         erl_crash_dump_bytes/1,
	 is_builtin/1, error_stacktrace/1,
	 error_stacktrace_during_call_trace/1,
         group_leader_prio/1, group_leader_prio_dirty/1,
         is_process_alive/1,
         is_process_alive_signal_from/1,
         process_info_blast/1,
         os_env_case_sensitivity/1,
         verify_middle_queue_save/1,
         test_length/1,
         fixed_apply_badarg/1,
         external_fun_apply3/1,
         node_1/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    [erl_bif_types, guard_bifs_in_erl_bif_types, shadow_comments,
     specs, improper_bif_stubs,
     t_list_to_existing_atom, os_env, otp_7526,
     display, display_string, list_to_utf8_atom,
     t_atom_to_binary, t_binary_to_atom, t_binary_to_existing_atom,
     erl_crash_dump_bytes, min_max, erlang_halt, is_builtin,
     error_stacktrace, error_stacktrace_during_call_trace,
     group_leader_prio, group_leader_prio_dirty,
     is_process_alive, is_process_alive_signal_from,
     process_info_blast, os_env_case_sensitivity,
     verify_middle_queue_save, test_length,fixed_apply_badarg,
     external_fun_apply3, node_1].

init_per_testcase(guard_bifs_in_erl_bif_types, Config) when is_list(Config) ->
    skip_missing_erl_bif_types(Config);
init_per_testcase(erl_bif_types, Config) when is_list(Config) ->
    skip_missing_erl_bif_types(Config);
init_per_testcase(shadow_comments, Config) when is_list(Config) ->
    skip_missing_erl_bif_types(Config);
init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Config.

end_per_testcase(_Func, Config) ->
    erts_test_utils:ept_check_leaked_nodes(Config).

%% erl_bif_types comes from dialyzer which some test runs skip building, so
%% we'll skip the tests that use it as the result shouldn't vary based on
%% platform. The majority build it so we'll have plenty coverage either way.
skip_missing_erl_bif_types(Config) ->
    c:l(erl_bif_types),
    case erlang:function_exported(erl_bif_types, module_info, 0) of
        false -> {skip, "erl_bif_types not compiled"};
        true -> Config
    end.

%% Uses erlang:display to test that erts_printf does not do deep recursion
display(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
    true = rpc:call(Node,?MODULE,display_huge,[]),
    peer:stop(Peer),
    ok.

display_huge() ->
    erlang:display(deeep(100000)).

deeep(0,Acc) ->
    Acc;
deeep(N,Acc) ->
    deeep(N-1,[Acc|[]]).

deeep(N) ->
    deeep(N,[hello]).

display_string(Config) when is_list(Config) ->
    true = erlang:display_string("hej"),
    true = erlang:display_string(""),
    true = erlang:display_string("hopp"),
    true = erlang:display_string("\n"),
    true = erlang:display_string(lists:seq(1100,1200)),
    {error,badarg} = try
                         erlang:display_string(atom),
                         ok
                     catch
                         T0:E0 ->
                             {T0, E0}
                     end,
    {error,badarg} = try
                         erlang:display_string(make_ref()),
                         ok
                     catch
                         T1:E1 ->
                             {T1, E1}
                     end,
    ok.

erl_bif_types(Config) when is_list(Config) ->
    List = erlang:system_info(snifs),

    KnownTypes = [MFA || MFA <- List, known_types(MFA)],
    io:format("There are ~p BIFs with type information in erl_bif_types.",
	      [length(KnownTypes)]),
    erl_bif_types_2(KnownTypes).

erl_bif_types_2(List) ->
    BadArity = [MFA || {M,F,A}=MFA <- List,
		       begin
			   Types = erl_bif_types:arg_types(M, F, A),
			   length(Types) =/= A
		       end],
    case BadArity of
	[] ->
	    erl_bif_types_3(List);
	[_|_] ->
	    io:put_chars("Bifs with bad arity\n"),
	    io:format("~p\n", [BadArity]),
	    ct:fail({length(BadArity),bad_arity})
    end.

erl_bif_types_3(List) ->
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
	    ct:fail({length(BadSmokeTest),bad_smoke_test})
    end.

guard_bifs_in_erl_bif_types(_Config) ->
    List0 = erlang:system_info(snifs),
    List = [{F,A} || {erlang,F,A} <- List0,
		     erl_internal:guard_bif(F, A)],
    Not = [FA || {F,A}=FA <- List,
		 not erl_bif_types:is_known(erlang, F, A)],
    case Not of
	[] ->
	    ok;
	[_|_] ->
	    io:put_chars(
	      ["Dialyzer requires that all guard BIFs "
	       "have type information in erl_bif_types.\n\n"
	       "The following guard BIFs have no type information "
	       "in erl_bif_types:\n\n",
	       [io_lib:format("  ~p/~p\n", [F,A]) || {F,A} <- Not]]),
	    ct:fail(erl_bif_types)
    end.

shadow_comments(_Config) ->
    ErlangList = [{erlang,F,A} || {F,A} <- erlang:module_info(exports),
				  not is_operator(F,A)],
    List0 = erlang:system_info(snifs),
    List1 = [MFA || {M,_,_}=MFA <- List0, M =/= erlang],
    List = List1 ++ ErlangList,
    HasTypes = [MFA || {M,F,A}=MFA <- List,
		       erl_bif_types:is_known(M, F, A)],
    Path = get_code_path(),
    BifRel = sofs:relation(HasTypes, [{m,f,a}]),
    BifModules = sofs:to_external(sofs:projection(1, BifRel)),
    AbstrByModule = [extract_abstract(Mod, Path) || Mod <- BifModules],
    Specs0 = [extract_specs(Mod, Abstr) ||
		 {Mod,Abstr} <- AbstrByModule],
    Specs = lists:append(Specs0),
    SpecFuns0 = [F || {F,_} <- Specs],
    SpecFuns = sofs:relation(SpecFuns0, [{m,f,a}]),
    HasTypesAndSpecs = sofs:intersection(BifRel, SpecFuns),
    Commented0 = lists:append([extract_comments(Mod, Path) ||
				  Mod <- BifModules]),
    Commented = sofs:relation(Commented0, [{m,f,a}]),
    {NoComments0,_,NoBifSpecs0} =
	sofs:symmetric_partition(HasTypesAndSpecs, Commented),
    NoComments = sofs:to_external(NoComments0),
    NoBifSpecs = sofs:to_external(NoBifSpecs0),

    case NoComments of
	[] ->
	    ok;
	[_|_] ->
	    io:put_chars(
	      ["If a BIF stub has both a spec and has type information in "
	       "erl_bif_types, there *must*\n"
	       "be a comment in the source file to make that immediately "
	       "obvious.\n\nThe following comments are missing:\n\n",
	       [io_lib:format("%% Shadowed by erl_bif_types: ~p:~p/~p\n",
			      [M,F,A]) || {M,F,A} <- NoComments]]),
	    ct:fail(bif_stub)
    end,

    case NoBifSpecs of
	[] ->
	    ok;
	[_|_] ->
	    io:put_chars(
	      ["The following functions have \"shadowed\" comments "
	       "claiming that there is type information in erl_bif_types,\n"
	       "but actually there is no such type information.\n\n"
	       "Therefore, the following comments should be removed:\n\n",
	       [io_lib:format("%% Shadowed by erl_bif_types: ~p:~p/~p\n",
			      [M,F,A]) || {M,F,A} <- NoBifSpecs]]),
	    ct:fail(erl_bif_types)
    end.

extract_comments(Mod, Path) ->
    Beam = which(Mod, Path),
    SrcDir = filename:join(filename:dirname(filename:dirname(Beam)), "src"),
    Src = filename:join(SrcDir, atom_to_list(Mod) ++ ".erl"),
    {ok,Bin} = file:read_file(Src),
    Lines0 = binary:split(Bin, <<"\n">>, [global]),
    Lines1 = [T || <<"%% Shadowed by erl_bif_types: ",T/binary>> <- Lines0],
    {ok,ReMFA} = re:compile("([^:]*):([^/]*)/(\\d*)"),
    Lines = [L || L <- Lines1, re:run(L, ReMFA, [{capture,[]}]) =:= match],
    [begin
	 {match,[M,F,A]} = re:run(L, ReMFA, [{capture,all_but_first,list}]),
	 {list_to_atom(M),list_to_atom(F),list_to_integer(A)}
     end || L <- Lines].

known_types({M,F,A}) ->
    erl_bif_types:is_known(M, F, A).

specs(_) ->
    List0 = erlang:system_info(snifs),

    %% Ignore all operators.
    List = [MFA || MFA <- List0, not is_operator(MFA)],

    %% Extract specs from the abstract code for all BIFs.
    Path = get_code_path(),
    BifRel = sofs:relation(List, [{m,f,a}]),
    BifModules = sofs:to_external(sofs:projection(1, BifRel)),
    AbstrByModule = [extract_abstract(Mod, Path) || Mod <- BifModules],
    Specs0 = [extract_specs(Mod, Abstr) ||
		 {Mod,Abstr} <- AbstrByModule],
    Specs = lists:append(Specs0),
    BifSet = sofs:set(List, [function]),
    SpecRel0 = sofs:relation(Specs, [{function,spec}]),
    SpecRel = sofs:restriction(SpecRel0, BifSet),

    %% Find BIFs without specs.
    NoSpecs0 = sofs:difference(BifSet, sofs:domain(SpecRel)),
    NoSpecs = sofs:to_external(NoSpecs0),
    case NoSpecs of
	[] ->
	    ok;
	[_|_] ->
	    io:put_chars("The following BIFs don't have specs:\n"),
	    [print_mfa(MFA) || MFA <- NoSpecs],
	    ct:fail(no_spec)
    end.

is_operator({erlang,F,A}) ->
    is_operator(F,A);
is_operator(_) -> false.

is_operator(F,A) ->
    erl_internal:arith_op(F, A) orelse
	erl_internal:bool_op(F, A) orelse
	erl_internal:comp_op(F, A) orelse
	erl_internal:list_op(F, A) orelse
	erl_internal:send_op(F, A).
    
extract_specs(M, Abstr) ->
    [{make_mfa(M, Name),Spec} || {attribute,_,spec,{Name,Spec}} <- Abstr].

make_mfa(M, {F,A}) -> {M,F,A};
make_mfa(M, {M,_,_}=MFA) -> MFA.

improper_bif_stubs(_) ->
    Bifs = erlang:system_info(snifs),
    Path = get_code_path(),
    BifRel = sofs:relation(Bifs, [{m,f,a}]),
    BifModules = sofs:to_external(sofs:projection(1, BifRel)),
    AbstrByModule = [extract_abstract(Mod, Path) || Mod <- BifModules],
    Funcs0 = [extract_functions(Mod, Abstr) ||
		 {Mod,Abstr} <- AbstrByModule],
    Funcs = lists:append(Funcs0),
    BifSet = sofs:set(Bifs, [function]),
    FuncRel0 = sofs:relation(Funcs, [{function,code}]),
    FuncRel = sofs:restriction(FuncRel0, BifSet),
    [check_stub(MFA, Body) || {MFA,Body} <- sofs:to_external(FuncRel)],
    ok.

extract_functions(M, Abstr) ->
    [{{M,F,A},Body} || {function,_,F,A,Body} <- Abstr].

check_stub({_,F,2}, _B) when F =:= min; F =:= max ->
    %% In Erlang/OTP 26, min/2 and max/2 are guard BIFs. For backward
    %% compatibility with code compiled with an earlier version, the
    %% Erlang implementation of them is kept.
    ok;
check_stub({_,F,A}, B) ->
    try
	[{clause,_,Args,[],Body}] = B,
	A = length(Args),
	[{call,_,{remote,_,{atom,_,erlang},{atom,_,nif_error}},[_]}] = Body
    catch
	_:_ ->
	    io:put_chars("Invalid body for the following BIF stub:\n"),
	    Func = {function,0,F,A,B},
	    io:put_chars(erl_pp:function(Func)),
	    io:nl(),
	    io:put_chars("The body should be: erlang:nif_error(undef)"),
	    ct:fail(invalid_body)
    end.

list_to_utf8_atom(Config) when is_list(Config) ->
    'hello' = atom_roundtrip("hello"),
    'こんにちは' = atom_roundtrip("こんにちは"),

    %% Test all edge cases.
    _ = atom_roundtrip([16#80]),
    _ = atom_roundtrip([16#7F]),
    _ = atom_roundtrip([16#FF]),
    _ = atom_roundtrip([16#100]),
    _ = atom_roundtrip([16#7FF]),
    _ = atom_roundtrip([16#800]),
    _ = atom_roundtrip([16#D7FF]),
    atom_badarg([16#D800]),
    atom_badarg([16#DFFF]),
    _ = atom_roundtrip([16#E000]),
    _ = atom_roundtrip([16#FFFF]),
    _ = atom_roundtrip([16#1000]),
    _ = atom_roundtrip([16#10FFFF]),
    atom_badarg([16#110000]),

    atom_badarg([-1]),
    [atom_badarg([(-1 bsl N) + $A]) || N <- lists:seq(8,16)],
    ok.

atom_roundtrip(String) ->
    Atom = list_to_atom(String),
    Atom = list_to_existing_atom(String),
    String = atom_to_list(Atom),
    Atom.

atom_badarg(String) ->
    {'EXIT',{badarg,_}} = (catch list_to_atom(String)),
    {'EXIT',{badarg,_}} = (catch list_to_existing_atom(String)),
    ok.

t_list_to_existing_atom(Config) when is_list(Config) ->
    all = list_to_existing_atom("all"),
    ?MODULE = list_to_existing_atom(?MODULE_STRING),
    UnlikelyStr = "dsfj923874390867er869fds9864y97jhg3973qerueoru",
    try
	list_to_existing_atom(UnlikelyStr),
	ct:fail(atom_exists)
    catch
	error:badarg -> ok
    end,

    %% The compiler has become smarter! We need the call to id/1 in
    %% the next line.
    UnlikelyAtom = list_to_atom(id(UnlikelyStr)),
    UnlikelyAtom = list_to_existing_atom(UnlikelyStr),
    ok.

os_env(Config) when is_list(Config) ->
    EnvVar1 = "MjhgvFDrresdCghN mnjkUYg vfrD",
    false = os:getenv(EnvVar1),
    true = os:putenv(EnvVar1, "mors"),
    "mors" = os:getenv(EnvVar1),
    true = os:putenv(EnvVar1, ""),
    case os:getenv(EnvVar1) of
	      "" -> ok;
	      false -> ok;
	      BadVal -> ct:fail(BadVal)
	  end,
    true = os:putenv(EnvVar1, "mors"),
    true = os:unsetenv(EnvVar1),
    false = os:getenv(EnvVar1),
    true = os:unsetenv(EnvVar1), % unset unset variable
    %% os:putenv, os:getenv and os:unsetenv currently use a temp
    %% buffer of size 1024 for storing key+value
    os_env_long(1010, 1030, "hej hopp").
    
os_env_long(Min, Max, _Value) when Min > Max ->
    ok;
os_env_long(Min, Max, Value) ->
    EnvVar = lists:duplicate(Min, $X),
    true = os:putenv(EnvVar, Value),
    Value = os:getenv(EnvVar),
    true = os:unsetenv(EnvVar),
    os_env_long(Min+1, Max, Value).

os_env_case_sensitivity(Config) when is_list(Config) ->
    %% The keys in os:getenv/putenv must be case-insensitive on Windows, and
    %% case-sensitive elsewhere.
    true = os:putenv("os_env_gurka", "gaffel"),
    Expected = case os:type() of
                   {win32, _} -> "gaffel";
                   _ -> false
               end,
    Expected = os:getenv("OS_ENV_GURKA"),
    ok.

%% Test that string:to_integer does not Halloc in wrong order.
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

t_binary_to_atom(Config) when is_list(Config) ->
    HalfLong = lists:seq(0, 127),
    HalfLongAtom = list_to_atom(HalfLong),
    HalfLongBin = list_to_binary(HalfLong),
    Long = lists:seq(0, 254),
    LongAtom = list_to_atom(Long),
    LongBin = list_to_binary(Long),
    UnicodeLongAtom = list_to_atom([$é || _ <- lists:seq(0, 254)]),
    UnicodeLongBin = << <<"é"/utf8>> || _ <- lists:seq(0, 254)>>,

    %% latin1
    '' = test_binary_to_atom(<<>>, latin1),
    '\377' = test_binary_to_atom(<<255>>, latin1),
    HalfLongAtom = test_binary_to_atom(HalfLongBin, latin1),
    LongAtom = test_binary_to_atom(LongBin, latin1),

    %% utf8
    '' = test_binary_to_atom(<<>>, utf8),
    HalfLongAtom = test_binary_to_atom(HalfLongBin, utf8),
    HalfLongAtom = test_binary_to_atom(HalfLongBin, unicode),
    UnicodeLongAtom = test_binary_to_atom(UnicodeLongBin, utf8),
    UnicodeLongAtom = test_binary_to_atom(UnicodeLongBin, unicode),
    [] = [C || C <- lists:seq(128, 255),
		     begin
			 list_to_atom([C]) =/=
			     test_binary_to_atom(<<C/utf8>>, utf8)
		     end],

    ExoticBin = <<"こんにちは"/utf8>>,
    ExoticAtom = test_binary_to_atom(ExoticBin, utf8),
    ExoticBin = atom_to_binary(ExoticAtom, utf8),
    ExoticBin = atom_to_binary(ExoticAtom),

    %% badarg failures.
    fail_binary_to_atom(atom),
    fail_binary_to_atom(42),
    fail_binary_to_atom({a,b,c}),
    fail_binary_to_atom([1,2,3]),
    fail_binary_to_atom([]),
    fail_binary_to_atom(42.0),
    fail_binary_to_atom(self()),
    fail_binary_to_atom(make_ref()),
    fail_binary_to_atom(<<0:7>>),
    fail_binary_to_atom(<<42:13>>),
    ?BADARG(binary_to_atom(id(<<>>), blurf)),
    ?BADARG(binary_to_atom(id(<<>>), [])),

    %% Bad UTF8 sequences.
    ?BADARG(binary_to_atom(id(<<255>>), utf8)),
    ?BADARG(binary_to_atom(id(<<255>>))),
    ?BADARG(binary_to_atom(id(<<255,0>>), utf8)),
    ?BADARG(binary_to_atom(id(<<16#C0,16#80>>), utf8)), %Overlong 0.
    <<B:1/binary, _/binary>> = id(<<194, 163>>), %Truncated character ERL-474
    ?BADARG(binary_to_atom(B, utf8)),

    %% system_limit failures.
    ?SYS_LIMIT(binary_to_atom(id(<<0:512/unit:8,255>>), utf8)),
    ?SYS_LIMIT(binary_to_atom(id(<<0:512/unit:8,255>>))),
    ?SYS_LIMIT(binary_to_atom(id(<<0:512/unit:8,255,0>>), utf8)),
    ?SYS_LIMIT(binary_to_atom(<<0:256/unit:8>>, latin1)),
    ?SYS_LIMIT(binary_to_atom(<<0:257/unit:8>>, latin1)),
    ?SYS_LIMIT(binary_to_atom(<<0:512/unit:8>>, latin1)),
    ?SYS_LIMIT(binary_to_atom(<<0:256/unit:8>>, utf8)),
    ?SYS_LIMIT(binary_to_atom(<<0:257/unit:8>>, utf8)),
    ?SYS_LIMIT(binary_to_atom(<<0:512/unit:8>>, utf8)),
    ok.

test_binary_to_atom(Bin0, Encoding) ->
    Res = binary_to_atom(Bin0, Encoding),
    Res = binary_to_existing_atom(Bin0, Encoding),
    if
        Encoding =:= utf8;
        Encoding =:= unicode ->
            Res = binary_to_atom(Bin0),
            Res = binary_to_existing_atom(Bin0);
       true ->
            ok
    end,
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
        binary_to_atom(Bin)
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
    end,
    try
        binary_to_existing_atom(Bin)
    catch
        error:badarg ->
            ok
    end.
	

t_binary_to_existing_atom(Config) when is_list(Config) ->
    UnlikelyBin = <<"ou0897979655678dsfj923874390867er869fds973qerueoru">>,
    try
	binary_to_existing_atom(UnlikelyBin, latin1),
	ct:fail(atom_exists)
    catch
	error:badarg -> ok
    end,

    try
	binary_to_existing_atom(UnlikelyBin, utf8),
	ct:fail(atom_exists)
    catch
	error:badarg -> ok
    end,
    try
	binary_to_existing_atom(UnlikelyBin),
	ct:fail(atom_exists)
    catch
	error:badarg -> ok
    end,

    UnlikelyAtom = binary_to_atom(id(UnlikelyBin), latin1),
    UnlikelyAtom = binary_to_existing_atom(UnlikelyBin, latin1),

    %% ERL-944; a binary that was too large would overflow the latin1-to-utf8
    %% conversion buffer.
    OverflowAtom = <<0:511/unit:8,
                     196, 133, 196, 133, 196, 133, 196, 133, 196, 133,
                     196, 133, 196, 133, 196, 133, 196, 133, 196, 133,
                     196, 133, 196, 133, 196, 133, 196, 133, 196, 133,
                     196, 133, 196, 133, 196, 133, 196, 133, 196, 133>>,
    {'EXIT', _} = (catch binary_to_existing_atom(OverflowAtom, latin1)),

    ok.


t_atom_to_binary(Config) when is_list(Config) ->
    HalfLong = lists:seq(0, 127),
    HalfLongAtom = list_to_atom(HalfLong),
    HalfLongBin = list_to_binary(HalfLong),
    Long = lists:seq(0, 254),
    LongAtom = list_to_atom(Long),
    LongBin = list_to_binary(Long),

    %% latin1
    <<>> = atom_to_binary('', latin1),
    <<"abc">> = atom_to_binary(abc, latin1),
    <<127>> = atom_to_binary('\177', latin1),
    HalfLongBin = atom_to_binary(HalfLongAtom, latin1),
    LongBin = atom_to_binary(LongAtom, latin1),

    %% utf8.
    <<>> = atom_to_binary(''),
    <<>> = atom_to_binary('', utf8),
    <<>> = atom_to_binary('', unicode),
    <<127>> = atom_to_binary('\177', utf8),
    <<"abcdef">> = atom_to_binary(abcdef, utf8),
    HalfLongBin = atom_to_binary(HalfLongAtom, utf8),
    HalfLongBin = atom_to_binary(HalfLongAtom),
    LongAtomBin = atom_to_binary(LongAtom, utf8),
    LongAtomBin = atom_to_binary(LongAtom),
    verify_long_atom_bin(LongAtomBin, 0),

    %% Failing cases.
    fail_atom_to_binary(<<1>>),
    fail_atom_to_binary(42),
    fail_atom_to_binary({a,b,c}),
    fail_atom_to_binary([1,2,3]),
    fail_atom_to_binary([]),
    fail_atom_to_binary(42.0),
    fail_atom_to_binary(self()),
    fail_atom_to_binary(make_ref()),
    ?BADARG(atom_to_binary(id(a), blurf)),
    ?BADARG(atom_to_binary(id(b), [])),
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
    end,
    try
        atom_to_binary(Term)
    catch
        error:badarg ->
            ok
    end.


min_max(Config) when is_list(Config) ->
    Self = self(),
    Port = hd(erlang:ports()),
    Ref = make_ref(),
    a = min(id(a), a),
    a = min(id(a), b),
    a = min(id(b), a),
    b = min(id(b), b),
    Ref = min(id(Self), id(Ref)),

    -3 = min(id(5), -3),
    -3 = min(-3, id(5)),
    -3 = min(0, id(-3)),
    -3 = min(id(-3), 0),
    0 = min(0, id(17)),

    a = max(id(a), a),
    b = max(id(a), b),
    b = max(id(b), a),
    b = max(id(b), b),
    Self = max(id(Self), id(Ref)),

    5 = max(id(5), -3),
    5 = max(-3, id(5)),
    0 = max(0, id(-3)),
    0 = max(id(-3), 0),
    17 = max(0, id(17)),

    %% Return the first argument when arguments are equal.
    42.0 = min(id(42.0), 42),
    42.0 = max(id(42.0), 42),

    Min = id(min),
    Max = id(max),

    "abc" = erlang:Min("abc", "def"),
    <<"def">> = erlang:Max(<<"abc">>, <<"def">>),

    %% Make sure that the JIT doesn't do any unsafe optimizations.
    {0, 0} = min_max_zero(0),
    {-7, 0} = min_max_zero(-7),
    {0, 555} = min_max_zero(555),
    {0, 1 bsl 64} = min_max_zero(1 bsl 64),
    {-1 bsl 64, 0} = min_max_zero(-1 bsl 64),

    {-99, 23} = do_min_max(-99, 23),
    {-10, 0} = do_min_max(0, -10),
    {0, 77} = do_min_max(77, 0),
    {1, 2} = do_min_max(1, 2),
    {42, 99} = do_min_max(99, 42),
    {100, 1 bsl 64} = do_min_max(100, 1 bsl 64),
    {-1 bsl 64, 77} = do_min_max(77, -1 bsl 64),
    {-1 bsl 64, 1 bsl 64} = do_min_max(1 bsl 64, -1 bsl 64),
    {42.0, 43} = do_min_max(42.0, 43),
    {42.0, 50.0} = do_min_max(42.0, 50.0),
    {42.0, 42.0} = do_min_max(42.0, id(40.0 + 2.0)),
    {{1,2}, {a,b}} = do_min_max({id(a), id(b)}, {id(1), id(2)}),
    {{a,b}, [a,b]} = do_min_max({a,id(b)}, [a,id(b)]),
    {{1.0,b}, {1.0,b}} = do_min_max({id(1.0), id(b)}, {id(1), id(b)}),
    {{7,b}, {7,b}} = do_min_max({id(7), id(b)}, {id(7.0), id(b)}),

    {42,Self} = do_min_max(42, Self),
    {42,Self} = do_min_max(Self, 42),
    {42,Port} = do_min_max(42, Port),
    {42,Port} = do_min_max(Port, 42),

    ok.

min_max_zero(A0) ->
    Result = {min(A0, 0), max(A0, 0)},
    Result = {min(0, A0), max(0, A0)},
    A = id(A0),
    Result = {min(A, 0), max(A, 0)},
    Result = {min(0, A), max(0, A)}.

do_min_max(A0, B0) ->
    Result = {min(A0, B0), max(A0, B0)},

    A0 = min(id(A0), A0),
    A0 = max(id(A0), A0),
    B0 = min(id(B0), B0),
    B0 = max(id(B0), B0),

    A = id(A0),
    B = id(B0),
    Result = {min(A, B), max(A, B)},

    if
        is_integer(A), is_atom(node(B)) orelse is_integer(B) ->
            _ = id(0),
            Result = {min(A, B),max(A, B)};
        is_atom(node(A)) orelse is_integer(A), is_integer(B) ->
            _ = id(0),
            Result = {min(A, B),max(A, B)};
        true ->
            ok
    end,

    if
        is_integer(A), 0 =< A, A =< 1000, is_atom(node(B)) orelse is_integer(B) ->
            _ = id(0),
            Result = {min(A, B),max(A, B)};
        is_atom(node(A)) orelse is_integer(A), is_integer(B), 0 =< B, B =< 1000 ->
            _ = id(0),
            Result = {min(A, B),max(A, B)};
        true ->
            ok
    end,

    Result = do_min_max_1(1, 2, 3, 4, 5, A, B).

do_min_max_1(_, _, _, _, _, A, B) ->
    if
        is_integer(A), 0 =< A, A < 16#1_0000,
        is_integer(B), 0 =< B, B < 16#1_0000 ->
            Result = {min(A, B),max(A, B)},
            Result = {min(B, A),max(B, A)},
            _ = id(0),
            Result = {min(A, B),max(A, B)},
            Result = {min(B, A),max(B, A)};
        is_integer(A), is_integer(B) ->
            Result = {min(A, B),max(A, B)},
            Result = {min(B, A),max(B, A)},
            _ = id(0),
            Result = {min(A, B),max(A, B)},
            Result = {min(B, A),max(B, A)};
        is_float(A), is_float(B) ->
            Result = {min(A, B),max(A, B)},
            Result = {min(B, A),max(B, A)},
            _ = id(0),
            Result = {min(A, B),max(A, B)},
            Result = {min(B, A),max(B, A)};
        is_number(A), is_number(B) ->
            Result = {min(A, B),max(A, B)},
            _ = id(0),
            Result = {min(A, B),max(A, B)};
        true ->
            Result = {min(A, B),max(A, B)},
            _ = id(0),
            Result = {min(A, B),max(A, B)}
    end.

erlang_halt(Config) when is_list(Config) ->
    try erlang:halt(undefined) of
	_-> ct:fail({erlang,halt,{undefined}})
    catch error:badarg -> ok end,
    try halt(undefined) of
	_-> ct:fail({halt,{undefined}})
    catch error:badarg -> ok end,
    try erlang:halt(undefined, []) of
	_-> ct:fail({erlang,halt,{undefined,[]}})
    catch error:badarg -> ok end,
    try halt(undefined, []) of
	_-> ct:fail({halt,{undefined,[]}})
    catch error:badarg -> ok end,
    try halt(0, undefined) of
	_-> ct:fail({halt,{0,undefined}})
    catch error:badarg -> ok end,
    try halt(0, [undefined]) of
	_-> ct:fail({halt,{0,[undefined]}})
    catch error:badarg -> ok end,
    try halt(0, [{undefined,true}]) of
	_-> ct:fail({halt,{0,[{undefined,true}]}})
    catch error:badarg -> ok end,
    try halt(0, [{flush,undefined}]) of
	_-> ct:fail({halt,{0,[{flush,undefined}]}})
    catch error:badarg -> ok end,
    try halt(0, [{flush,true,undefined}]) of
	_-> ct:fail({halt,{0,[{flush,true,undefined}]}})
    catch error:badarg -> ok end,
    {ok, _, N1} = ?CT_PEER(),
    {badrpc,nodedown} = rpc:call(N1, erlang, halt, []),
    {ok, _, N2} = ?CT_PEER(),
    {badrpc,nodedown} = rpc:call(N2, erlang, halt, [0]),
    {ok, _, N3} = ?CT_PEER(),
    {badrpc,nodedown} = rpc:call(N3, erlang, halt, [0,[]]),
    {ok, _, N4} = ?CT_PEER(),
    {badrpc,nodedown} = rpc:call(N4, erlang, halt, [lists:duplicate(300,$x)]),
    %% Test unicode slogan
    {ok, _, N5} = ?CT_PEER(),
    {badrpc,nodedown} = rpc:call(N5, erlang, halt, [[339,338,254,230,198,295,167,223,32,12507,12531,12480]]),

    % This test triggers a segfault when dumping a crash dump
    % to make sure that we can handle it properly.

    %% Prevent address sanitizer from catching SEGV in slave node
    AsanOpts = add_asan_opt("handle_segv=0"),
    {ok, _, N6} = ?CT_PEER(),
    reset_asan_opts(AsanOpts),

    CrashDump = filename:join(proplists:get_value(priv_dir,Config),
                              "segfault_erl_crash.dump"),
    true = rpc:call(N6, os, putenv, ["ERL_CRASH_DUMP",CrashDump]),
    false = rpc:call(N6, erts_debug, set_internal_state,
                     [available_internal_state, true]),
    {badrpc,nodedown} = rpc:call(N6, erts_debug, set_internal_state,
                                 [broken_halt, "Validate correct crash dump"]),
    {ok,_} = wait_until_stable_size(CrashDump,-1),
    {ok, Bin} = file:read_file(CrashDump),
    case {string:find(Bin, <<"\n=end\n">>),
          string:find(Bin, <<"\r\n=end\r\n">>)} of
        {nomatch,nomatch} ->
            ct:fail("Could not find end marker in crash dump");
        {_,_} ->
            ok
    end.

add_asan_opt(Opt) ->
    case test_server:is_asan() of
	true ->
	    case os:getenv("ASAN_OPTIONS") of
		false ->
		    os:putenv("ASAN_OPTIONS", Opt),
		    undefined;
		AO ->
		    os:putenv("ASAN_OPTIONS", AO ++ [$: | Opt]),
		    AO
	    end;
	_ ->
	    false
    end.

reset_asan_opts(false) -> ok;
reset_asan_opts(undefined) -> os:unsetenv("ASAN_OPTIONS");
reset_asan_opts(AO) -> os:putenv("ASAN_OPTIONS", AO).

wait_until_stable_size(_File,-10) ->
    {error,enoent};
wait_until_stable_size(File,PrevSz) ->
    timer:sleep(250),
    case file:read_file_info(File) of
        {error,enoent} ->
            wait_until_stable_size(File,PrevSz-1);
        {ok,#file_info{size = PrevSz }} when PrevSz /= -1 ->
            io:format("Crashdump file size was: ~p (~s)~n",[PrevSz,File]),
            {ok,PrevSz};
        {ok,#file_info{size = NewSz }} ->
            wait_until_stable_size(File,NewSz)
    end.

% Test erlang:halt with ERL_CRASH_DUMP_BYTES
erl_crash_dump_bytes(Config) when is_list(Config) ->
    Bytes = 1000,
    CrashDump = do_limited_crash_dump(Config, Bytes),
    {ok,ActualBytes} = wait_until_stable_size(CrashDump,-1),
    true = ActualBytes < (Bytes + 100),

    NoDump = do_limited_crash_dump(Config,0),
    {error,enoent} = wait_until_stable_size(NoDump,-8),
    ok.

do_limited_crash_dump(Config, Bytes) ->
    {ok, _, N} = ?CT_PEER(),
    BytesStr = integer_to_list(Bytes),
    CrashDump = filename:join(proplists:get_value(priv_dir,Config),
                              "erl_crash." ++ BytesStr ++ ".dump"),
    true = rpc:call(N, os, putenv, ["ERL_CRASH_DUMP",CrashDump]),
    true = rpc:call(N, os, putenv, ["ERL_CRASH_DUMP_BYTES",BytesStr]),
    {badrpc,nodedown} = rpc:call(N, erlang, halt, ["Testing ERL_CRASH_DUMP_BYTES"]),
    CrashDump.


is_builtin(_Config) ->
    Exp0 = [{M,F,A} || {M,_} <- code:all_loaded(),
		       {F,A} <- M:module_info(exports)],
    Exp = ordsets:from_list(Exp0),

    %% Built-ins implemented as special instructions.
    Instructions = [{erlang,apply,2},{erlang,apply,3},{erlang,yield,0}],

    Builtins0 = Instructions ++ erlang:system_info(snifs),
    Builtins = ordsets:from_list(Builtins0),

    Fakes = [{M,F,42} || {M,F,_} <- Instructions],
    All = ordsets:from_list(Fakes ++ Exp),
    NotBuiltin = ordsets:subtract(All, Builtins),

    _ = [{true,_} = {erlang:is_builtin(M, F, A),MFA} ||
            {M,F,A}=MFA <- Builtins],
    _ = [{false,_} = {erlang:is_builtin(M, F, A),MFA} ||
            {M,F,A}=MFA <- NotBuiltin],

    ok.

error_stacktrace(Config) when is_list(Config) ->
    error_stacktrace_test().

error_stacktrace_during_call_trace(Config) when is_list(Config) ->
    Tracer = spawn_link(fun () ->
				receive after infinity -> ok end
			end),
    Mprog = [{'_',[],[{exception_trace}]}],
    erlang:trace_pattern({?MODULE,'_','_'}, Mprog, [local]),
    1 = erlang:trace_pattern({erlang,error,2}, Mprog, [local]),
    1 = erlang:trace_pattern({erlang,error,1}, Mprog, [local]),
    erlang:trace(all, true, [call,return_to,timestamp,{tracer, Tracer}]),
    try
	error_stacktrace_test()
    after
	erlang:trace(all, false, [call,return_to,timestamp,{tracer, Tracer}]),
	erlang:trace_pattern({erlang,error,2}, false, [local]),
	erlang:trace_pattern({erlang,error,1}, false, [local]),
	erlang:trace_pattern({?MODULE,'_','_'}, false, [local]),
	unlink(Tracer),
	exit(Tracer, kill),
	Mon = erlang:monitor(process, Tracer),
	receive
	    {'DOWN', Mon, process, Tracer, _} -> ok
	end
    end,
    ok.

error_stacktrace_test() ->
    Types = [apply_const_last, apply_const, apply_last,
	     apply, double_apply_const_last, double_apply_const,
	     double_apply_last, double_apply, multi_apply_const_last,
             apply_const_only, apply_only,
	     multi_apply_const, multi_apply_last, multi_apply,
	     call_const_last, call_last, call_const, call],
    lists:foreach(fun (Type) ->
			  {Pid, Mon} = spawn_monitor(
					 fun () ->
						 stk([a,b,c,d], Type, error_2)
					 end),
			  receive
			      {'DOWN', Mon, process, Pid, Reason} ->
				  {oops, Stack} = Reason,
%%				  io:format("Type: ~p Stack: ~p~n",
%%					    [Type, Stack]),
				  [{?MODULE, do_error_2, [Type], _},
				   {?MODULE, stk, 3, _},
				   {?MODULE, stk, 3, _}] = Stack
			  end
		  end,
		  Types),
    lists:foreach(fun (Type) ->
			  {Pid, Mon} = spawn_monitor(
					 fun () ->
						 stk([a,b,c,d], Type, error_1)
					 end),
			  receive
			      {'DOWN', Mon, process, Pid, Reason} ->
				  {oops, Stack} = Reason,
%%				  io:format("Type: ~p Stack: ~p~n",
%%					    [Type, Stack]),
				  [{?MODULE, do_error_1, 1, _},
				   {?MODULE, stk, 3, _},
				   {?MODULE, stk, 3, _}] = Stack
			  end
		  end,
		  Types),
    ok.

stk([_|L], Type, Func) ->
    stk(L, Type, Func),
    %% Force the compiler to keep this body-recursive. We want the stack trace
    %% to have one entry here and another in the base case to test that
    %% multiple frames in the same function aren't removed unless they're
    %% identical.
    id(ok);
stk([], Type, Func) ->
    put(erlang, erlang),
    put(tail, []),
    tail(Type, Func, jump),
    id(ok).

tail(Type, Func, jump) ->
    tail(Type, Func, do);
tail(Type, error_1, do) ->
    do_error_1(Type);
tail(Type, error_2, do) ->
    do_error_2(Type).

do_error_2(apply_const_only) ->
    apply(erlang, error, [oops, [apply_const_only]]);
do_error_2(apply_only) ->
    Erlang = get(erlang),
    Tail = get(tail),
    apply(Erlang, error, [oops, [apply_only|Tail]]);
do_error_2(apply_const_last) ->
    erlang:apply(erlang, error, [oops, [apply_const_last]]);
do_error_2(apply_const) ->
    erlang:apply(erlang, error, [oops, [apply_const]]),
    ok;
do_error_2(apply_last) ->
    erlang:apply(id(erlang), id(error), id([oops, [apply_last]]));
do_error_2(apply) ->
    erlang:apply(id(erlang), id(error), id([oops, [apply]])),
    ok;
do_error_2(double_apply_const_last) ->
    erlang:apply(erlang, apply, [erlang, error, [oops, [double_apply_const_last]]]);
do_error_2(double_apply_const) ->
    erlang:apply(erlang, apply, [erlang, error, [oops, [double_apply_const]]]),
    ok;
do_error_2(double_apply_last) ->
    erlang:apply(id(erlang), id(apply), [id(erlang), id(error), id([oops, [double_apply_last]])]);
do_error_2(double_apply) ->
    erlang:apply(id(erlang), id(apply), [id(erlang), id(error), id([oops, [double_apply]])]),
    ok;
do_error_2(multi_apply_const_last) ->
    erlang:apply(erlang, apply, [erlang, apply, [erlang, apply, [erlang, error, [oops, [multi_apply_const_last]]]]]);
do_error_2(multi_apply_const) ->
    erlang:apply(erlang, apply, [erlang, apply, [erlang, apply, [erlang, error, [oops, [multi_apply_const]]]]]),
    ok;
do_error_2(multi_apply_last) ->
    erlang:apply(id(erlang), id(apply), [id(erlang), id(apply), [id(erlang), id(apply), [id(erlang), id(error), id([oops, [multi_apply_last]])]]]);
do_error_2(multi_apply) ->
    erlang:apply(id(erlang), id(apply), [id(erlang), id(apply), [id(erlang), id(apply), [id(erlang), id(error), id([oops, [multi_apply]])]]]),
    ok;
do_error_2(call_const_last) ->
    erlang:error(oops, [call_const_last]);
do_error_2(call_last) ->
    erlang:error(id(oops), id([call_last]));
do_error_2(call_const) ->
    erlang:error(oops, [call_const]),
    ok;
do_error_2(call) ->
    erlang:error(id(oops), id([call])).


do_error_1(apply_const_only) ->
    apply(erlang, error, [oops]);
do_error_1(apply_only) ->
    Erlang = get(erlang),
    Tail = get(tail),
    apply(Erlang, error, [oops|Tail]);
do_error_1(apply_const_last) ->
    erlang:apply(erlang, error, [oops]);
do_error_1(apply_const) ->
    erlang:apply(erlang, error, [oops]),
    ok;
do_error_1(apply_last) ->
    erlang:apply(id(erlang), id(error), id([oops]));
do_error_1(apply) ->
    erlang:apply(id(erlang), id(error), id([oops])),
    ok;
do_error_1(double_apply_const_last) ->
    erlang:apply(erlang, apply, [erlang, error, [oops]]);
do_error_1(double_apply_const) ->
    erlang:apply(erlang, apply, [erlang, error, [oops]]),
    ok;
do_error_1(double_apply_last) ->
    erlang:apply(id(erlang), id(apply), [id(erlang), id(error), id([oops])]);
do_error_1(double_apply) ->
    erlang:apply(id(erlang), id(apply), [id(erlang), id(error), id([oops])]),
    ok;
do_error_1(multi_apply_const_last) ->
    erlang:apply(erlang, apply, [erlang, apply, [erlang, apply, [erlang, error, [oops]]]]);
do_error_1(multi_apply_const) ->
    erlang:apply(erlang, apply, [erlang, apply, [erlang, apply, [erlang, error, [oops]]]]),
    ok;
do_error_1(multi_apply_last) ->
    erlang:apply(id(erlang), id(apply), [id(erlang), id(apply), [id(erlang), id(apply), [id(erlang), id(error), id([oops])]]]);
do_error_1(multi_apply) ->
    erlang:apply(id(erlang), id(apply), [id(erlang), id(apply), [id(erlang), id(apply), [id(erlang), id(error), id([oops])]]]),
    ok;
do_error_1(call_const_last) ->
    erlang:error(oops);
do_error_1(call_last) ->
    erlang:error(id(oops));
do_error_1(call_const) ->
    erlang:error(oops),
    ok;
do_error_1(call) ->
    erlang:error(id(oops)).


group_leader_prio(Config) when is_list(Config) ->
    group_leader_prio_test(false).

group_leader_prio_dirty(Config) when is_list(Config) ->
    group_leader_prio_test(true).

group_leader_prio_test(Dirty) ->
    %%
    %% Unfortunately back in the days node local group_leader/2 was not
    %% implemented as sending an asynchronous signal to the process to change
    %% group leader for. Instead it has always been synchronously changed, and
    %% nothing in the documentation have hinted otherwise... Therefore I do not
    %% dare the change this.
    %%
    %% In order to prevent priority inversion, the priority of the receiver of
    %% the group leader signal is elevated while handling incoming signals if
    %% the sender has a higher priority than the receiver. This test tests that
    %% the priority elevation actually works...
    %%
    Tester = self(),
    Init = erlang:whereis(init),
    GL = erlang:group_leader(),
    process_flag(priority, max),
    {TestProcFun, NTestProcs}
        = case Dirty of
              false ->
                  %% These processes will handle all incoming signals
                  %% by them selves...
                  {fun () ->
                           Tester ! {alive, self()},
                           receive after infinity -> ok end
                   end,
                   100};
              true ->
                  %% These processes won't handle incoming signals by
                  %% them selves since they are stuck on dirty schedulers
                  %% when we try to change group leader. A dirty process
                  %% signal handler process (system process) will be notified
                  %% of the need to handle incoming signals for these processes,
                  %% and will instead handle the signal for these processes...
                  {fun () ->
                           %% The following sends the message '{alive, self()}'
                           %% to Tester once on a dirty io scheduler, then wait
                           %% there until the process terminates...
                           erts_debug:dirty_io(alive_waitexiting, Tester)
                   end,
                   erlang:system_info(dirty_io_schedulers)}
          end,
    TPs = lists:map(fun (_) ->
                            spawn_opt(TestProcFun,
                                      [link, {priority, normal}])
                    end, lists:seq(1, NTestProcs)),
    lists:foreach(fun (TP) -> receive {alive, TP} -> ok end end, TPs),
    TLs = lists:map(fun (_) ->
                            spawn_opt(fun () -> tok_loop() end,
                                      [link, {priority, high}])
                    end,
                    lists:seq(1, 2*erlang:system_info(schedulers))),
    %% Wait to ensure distribution of high prio processes over schedulers...
    receive after 1000 -> ok end,
    %%
    %% Test that we can get group-leader signals through to normal prio
    %% processes from a max prio process even though all schedulers are filled
    %% with executing high prio processes.
    %%
    lists:foreach(fun (_) ->
                          lists:foreach(fun (TP) ->
                                                erlang:yield(),
                                                %% whitebox -- Enqueue some signals on it
                                                %% preventing us from hogging its main lock
                                                %% and set group-leader directly....
                                                erlang:demonitor(erlang:monitor(process, TP)),
                                                true = erlang:group_leader(Init, TP),
                                                {group_leader, Init} = process_info(TP, group_leader),
                                                erlang:demonitor(erlang:monitor(process, TP)),
                                                true = erlang:group_leader(GL, TP),
                                                {group_leader, GL} = process_info(TP, group_leader)
                                        end,
                                        TPs)
                  end,
                  lists:seq(1,100)),
    %%
    %% Also test when it is exiting...
    %%
    lists:foreach(fun (TP) ->
                          erlang:yield(),
                          M = erlang:monitor(process, TP),
                          unlink(TP),
                          exit(TP, bang),
                          badarg = try
                                       true = erlang:group_leader(Init, TP)
                                   catch
                                       error : What -> What
                                   end,
                          receive
                              {'DOWN', M, process, TP, Reason} ->
                                  bang = Reason
                          end
                  end,
                  TPs),
    lists:foreach(fun (TL) ->
                          M = erlang:monitor(process, TL),
                          unlink(TL),
                          exit(TL, bang),
                          receive
                              {'DOWN', M, process, TL, Reason} ->
                                  bang = Reason
                          end
                  end,
                  TLs),
    ok.

is_process_alive(Config) when is_list(Config) ->
    process_flag(priority, max),
    Ps = lists:map(fun (_) ->
                           spawn_opt(fun () -> tok_loop() end,
                                     [{priority, high}, link])
                   end,
                   lists:seq(1, 2*erlang:system_info(schedulers))),
    receive after 1000 -> ok end, %% Wait for load to spread
    lists:foreach(fun (P) ->
                          %% Ensure that signal order is preserved
                          %% and that we are not starved due to
                          %% priority inversion
                          true = erlang:is_process_alive(P),
                          unlink(P),
                          true = erlang:is_process_alive(P),
                          exit(P, kill),
                          false = erlang:is_process_alive(P)
                  end,
                  Ps),
    ok.

is_process_alive_signal_from(Config) when is_list(Config) ->
    process_flag(priority, high),
    process_flag(scheduler, 1),
    Schdlr = case erlang:system_info(schedulers_online) of
                 1 -> 1;
                 _ -> 2
             end,
    X = is_process_alive_signal_from_test(100000, 0, Schdlr),
    erlang:display({exits_detected, X}),
    {comment, integer_to_list(X) ++ " exited processes detected"}.

is_process_alive_signal_from_test(0, X, _Schdlr) ->
    X;
is_process_alive_signal_from_test(N, X, Schdlr) ->
    Tester = self(),
    {Testee, TMon} = spawn_opt(fun () ->
                                       Mon = erlang:monitor(process, Tester),
                                       Tester ! {self(), ready},
                                       busy_wait_go(),
                                       _ = erlang:demonitor(Mon),
                                       exit(normal)
                               end,
                               [link,
                                monitor,
                                {priority, high},
                                {scheduler, Schdlr}]),
    receive {Testee, ready} -> ok end,
    {monitored_by, MBList1} = process_info(self(), monitored_by),
    true = lists:member(Testee, MBList1),
    erlang:yield(),
    Testee ! {go, ok},
    erlang:yield(),
    NewX = case erlang:is_process_alive(Testee) of
               true ->
                   X;
               false ->
                   %% Demonitor signal should have reached us before the
                   %% is-process-alive reply...
                   {monitored_by, MBList2} = process_info(self(), monitored_by),
                   false = lists:member(Testee, MBList2),
                   X+1
           end,
    receive {'DOWN', TMon, process, Testee, normal} -> ok end,
    is_process_alive_signal_from_test(N-1, NewX, Schdlr).

process_info_blast(Config) when is_list(Config) ->
    Tester = self(),
    NoAttackers = 1000,
    NoAL = lists:seq(1, NoAttackers),
    Consume = make_ref(),
    Victim = spawn_link(fun () ->
                                receive
                                    Consume ->
                                        ok
                                end,
                                consume_msgs()
                        end),
    AFun = fun () ->
                   Victim ! hej,
                   Res = process_info(Victim, message_queue_len),
                   Tester ! {self(), Res}
           end,
    Attackers0 = lists:map(fun (_) ->
                                   spawn_link(AFun)
                           end,
                           NoAL),
    lists:foreach(fun (A) ->
                          receive
                              {A, Res} ->
                                  case Res of
                                      {message_queue_len, Len} when Len > 0, Len =< NoAttackers ->
                                          Len;
                                      Error ->
                                          exit({unexpected, Error})
                                  end
                          end
                  end,
                  Attackers0),
    Attackers1 = lists:map(fun (_) ->
                                   spawn_link(AFun)
                           end,
                           NoAL),
    Victim ! Consume,
    lists:foreach(fun (A) ->
                          receive
                              {A, Res} ->
                                  case Res of
                                      {message_queue_len, Len} when Len >= 0, Len =< 2*NoAttackers+1 ->
                                          ok;
                                      undefined ->
                                          ok;
                                      Error ->
                                          exit({unexpected, Error})
                                  end
                          end
                  end,
                  Attackers1),
    KillFun = fun (P) ->
                      unlink(P),
                      exit(P, kill),
                      false = erlang:is_process_alive(P)
              end,
    lists:foreach(fun (A) -> KillFun(A) end, Attackers0),
    lists:foreach(fun (A) -> KillFun(A) end, Attackers1),
    KillFun(Victim),
    ok.

consume_msgs() ->
    receive
        _ ->
            consume_msgs()
    after 0 ->
              ok
    end.


%% Test that process_info(Pid, [message_queue_len]) works correctly when
%%  fetching part of the middle signal queue into inner queue.
verify_middle_queue_save(Config) when is_list(Config) ->
    Control = self(),
    ProcessToHang = spawn_link(
        fun () ->
            Single = self(),
            put(count, 0),
            Doubles = [spawn_link(fun () -> message_queue_len_retrievers(Single, 0) end) || _ <- lists:seq(1, 2)],
            Control ! {doubles, Doubles},
            process_that_hangs(Control, 0, Doubles)
        end),
    ensure_not_hanging(ProcessToHang, [], 50000).

process_that_hangs(Control, Total, Doubles) ->
    put(count, Total),
    %% fetch something innocent, like 'priority' of the process
    [process_info(Pid, [priority]) || Pid <- Doubles],
    Control ! alive,
    process_that_hangs(Control, Total + 1, Doubles).

message_queue_len_retrievers(Control, PrevCount) ->
    %% need to fetch dictionary for test reasons, but actual trigger is 'message_queue_len',
    %%  or 'memory', or 'total_heap_size' - anything that needs to fetch external message queue
    %%  via ERTS_PI_FLAG_NEED_MSGQ_LEN internal flag to process_info
    [_, {dictionary, [{count, Count}]}] = erlang:process_info(Control, [message_queue_len, dictionary]),
    Count > PrevCount andalso begin Control ! count end,
    message_queue_len_retrievers(Control, Count).

ensure_not_hanging(Proc, _Doubles, 0) ->
    unlink(Proc),
    exit(Proc, kill);
ensure_not_hanging(Proc, Doubles, Remaining) ->
    receive
        alive ->
            ensure_not_hanging(Proc, Doubles, Remaining - 1);
        {doubles, NewDoubles} ->
            ensure_not_hanging(Proc, NewDoubles, Remaining)
    after 1000 ->
        Reason = {Proc, "hung", erlang:process_info(Proc, backtrace)},
        unlink(Proc),
        exit(Proc, kill),
        ct:fail(Reason)
    end.

%% Test that length/1 returns the correct result after trapping, and
%% also that the argument is correct in the stacktrace for a badarg
%% exception.

test_length(_Config) ->
    {Start,Inc} = case test_server:timetrap_scale_factor() of
                      1 -> {16*4000,3977};
                      _ -> {100,1}
            end,
    Good = lists:reverse(lists:seq(1, Start)),
    Bad = Good ++ [bad|cons],
    test_length(Start, 10*Start, Inc, Good, Bad),

    %% Test that calling length/1 from a match spec works.
    MsList = lists:seq(1, 2*Start),
    MsInput = [{tag,Good},{tag,MsList}],
    Ms0 = [{{tag,'$1'},[{'>',{length,'$1'},Start}],['$1']}],
    Ms = ets:match_spec_compile(Ms0),
    [MsList] = ets:match_spec_run(MsInput, Ms),
    ok.

test_length(I, N, Inc, Good, Bad) when I < N ->
    Length = id(length),
    I = length(Good),
    I = erlang:Length(Good),

    %% Test length/1 in guards.
    if
        length(Good) =:= I ->
            ok
    end,
    if
        length(Bad) =:= I ->
            error(should_fail);
        true ->
            ok
    end,

    {'EXIT',{badarg,[{erlang,length,[[I|_]],_}|_]}} = (catch length(Bad)),
    {'EXIT',{badarg,[{erlang,length,[[I|_]],_}|_]}} = (catch erlang:Length(Bad)),
    IncSeq = lists:seq(I + 1, I + Inc),
    test_length(I+Inc, N, Inc,
                lists:reverse(IncSeq, Good),
                lists:reverse(IncSeq, Bad));
test_length(_, _, _, _, _) -> ok.

%% apply/3 with a fixed number of arguments didn't include all arguments on
%% badarg exceptions.
fixed_apply_badarg(Config) when is_list(Config) ->
    Bad = id({}),

    {'EXIT',{badarg, [{erlang,apply,[{},baz,[a,b]],[{error_info,_}]} | _]}} =
        (catch Bad:baz(a,b)),
    {'EXIT',{badarg, [{erlang,apply,[baz,{},[c,d]],[{error_info,_}]} | _]}} =
        (catch baz:Bad(c,d)),

    {'EXIT',{badarg, [{erlang,apply,[{},baz,[e,f]],[{error_info,_}]} | _]}} =
        (catch apply(Bad,baz,[e,f])),
    {'EXIT',{badarg, [{erlang,apply,[baz,{},[g,h]],[{error_info,_}]} | _]}} =
        (catch apply(baz,Bad,[g,h])),

    ok.

external_fun_apply3(_Config) ->
    %% erlang:apply/3 would always badarg when called through an external fun.

    Apply = id(fun erlang:apply/3),
    Self = Apply(erlang, self, []),
    true = is_pid(Self),

    {'EXIT',{undef,_}} = (catch Apply(does, 'not', [exist])),

    ok.

node_1(_Config) ->
    {ok, Peer, Node} = ?CT_PEER(),

    local_node(self()),
    LocalPort = lists:last(erlang:ports()),
    local_node(LocalPort),
    local_node(make_ref()),

    external_node(erpc:call(Node, erlang, self, []), Node),
    ExtPort = hd(erpc:call(Node, erlang, ports, [])),
    external_node(ExtPort, Node),
    external_node(erpc:call(Node, erlang, make_ref, []), Node),

    node_error(a),
    node_error(42),
    node_error({a,b,c}),
    node_error({tag,self()}),
    node_error([self()]),
    node_error(1 bsl 133),
    node_error(#{}),
    node_error(#{id(a) => b}),
    node_error(<<"binary">>),

    peer:stop(Peer),
    ok.

local_node(E) ->
    test_node(E, node()).

external_node(E, Node) ->
    test_node(E, Node).

test_node(E0, Node) ->
    true = node(id(E0)) =:= Node,
    E = id(E0),
    if
        node(E) =:= Node ->
            ok
    end,
    test_node_2(id(E), Node).

test_node_2(E, Node) when is_pid(E); is_port(E); is_reference(E) ->
    true = node(E) =:= Node,
    if
        node(E) =:= Node ->
            ok
    end,
    test_node_3(id(E), Node),
    ok.

test_node_3(E, Node) when is_pid(E) ->
    true = node(E) =:= Node;
test_node_3(E, Node) when is_port(E) ->
    true = node(E) =:= Node;
test_node_3(E, Node) when is_reference(E) ->
    true = node(E) =:= Node.

node_error(E0) ->
    E = id(E0),
    {'EXIT',{badarg,[{erlang,node,[E],_}|_]}} = catch node(E),
    if
        node(E) ->
            ct:fail(should_fail);
        true ->
            ok
    end.

%% helpers

busy_wait_go() ->
    receive
        {go, Info} ->
            Info
    after
        0 ->
            busy_wait_go()
    end.

id(I) -> I.

%% Get code path, including the path for the erts application.
get_code_path() ->
    Erts = filename:join([code:root_dir(),"erts","preloaded","ebin"]),
    case filelib:is_dir(Erts) of
	true->
	    [Erts|code:get_path()];
	_ ->
	    code:get_path()
    end.

which(Mod, Path) ->
    which_1(atom_to_list(Mod) ++ ".beam", Path).

which_1(Base, [D|Ds]) ->
    Path = filename:join(D, Base),
    case filelib:is_regular(Path) of
	true -> Path;
	false -> which_1(Base, Ds)
    end.
print_mfa({M,F,A}) ->
    io:format("~p:~p/~p", [M,F,A]).

extract_abstract(Mod, Path) ->
    Beam = which(Mod, Path),
    {ok,{Mod,[{abstract_code,{raw_abstract_v1,Abstr}}]}} =
	beam_lib:chunks(Beam, [abstract_code]),
    {Mod,Abstr}.


tok_loop() ->
    tok_loop(hej).

tok_loop(hej) ->
    tok_loop(hopp);
tok_loop(hopp) ->
    tok_loop(hej).
