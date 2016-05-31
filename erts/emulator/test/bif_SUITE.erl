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

-module(bif_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-export([all/0, suite/0,
	 display/1, display_huge/0,
	 erl_bif_types/1,guard_bifs_in_erl_bif_types/1,
	 shadow_comments/1,
	 specs/1,improper_bif_stubs/1,auto_imports/1,
	 t_list_to_existing_atom/1,os_env/1,otp_7526/1,
	 binary_to_atom/1,binary_to_existing_atom/1,
	 atom_to_binary/1,min_max/1, erlang_halt/1,
	 is_builtin/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    [erl_bif_types, guard_bifs_in_erl_bif_types, shadow_comments,
     specs, improper_bif_stubs, auto_imports,
     t_list_to_existing_atom, os_env, otp_7526,
     display,
     atom_to_binary, binary_to_atom, binary_to_existing_atom,
     min_max, erlang_halt, is_builtin].

%% Uses erlang:display to test that erts_printf does not do deep recursion
display(Config) when is_list(Config) ->
    Pa = filename:dirname(code:which(?MODULE)),
    {ok, Node} = test_server:start_node(display_huge_term,peer,
					[{args, "-pa \""++Pa++"\""}]),
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

erl_bif_types(Config) when is_list(Config) ->
    ensure_erl_bif_types_compiled(),

    List0 = erlang:system_info(snifs),

    %% Ignore missing type information for hipe BIFs.
    List = [MFA || {M,_,_}=MFA <- List0, M =/= hipe_bifs],

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
    ensure_erl_bif_types_compiled(),

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
    ensure_erl_bif_types_compiled(),

    ErlangList = [{erlang,F,A} || {F,A} <- erlang:module_info(exports),
				  not is_operator(F,A)],
    List0 = erlang:system_info(snifs),
    List1 = [MFA || {M,_,_}=MFA <- List0, M =/= hipe_bifs, M =/= erlang],
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

ensure_erl_bif_types_compiled() ->
    c:l(erl_bif_types),
    case erlang:function_exported(erl_bif_types, module_info, 0) of
	false ->
	    %% Fail cleanly.
	    ct:fail("erl_bif_types not compiled");
	true ->
	    ok
    end.

known_types({M,F,A}) ->
    erl_bif_types:is_known(M, F, A).

specs(_) ->
    List0 = erlang:system_info(snifs),

    %% Ignore missing type information for hipe BIFs.
    List1 = [MFA || {M,_,_}=MFA <- List0, M =/= hipe_bifs],

    %% Ignore all operators.
    List = [MFA || MFA <- List1, not is_operator(MFA)],

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
    Bifs0 = erlang:system_info(snifs),
    Bifs = [MFA || {M,_,_}=MFA <- Bifs0, M =/= hipe_bifs],
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

auto_imports(_Config) ->
    Path = get_code_path(),
    {erlang,Abstr} = extract_abstract(erlang, Path),
    SpecFuns = [Name || {attribute,_,spec,{Name,_}} <- Abstr],
    auto_imports(SpecFuns, 0).

auto_imports([{F,A}|T], Errors) ->
    case erl_internal:bif(F, A) of
	false ->
	    io:format("~p/~p: not auto-imported, but spec claims it "
		      "is auto-imported", [F,A]),
	    auto_imports(T, Errors+1);
	true ->
	    auto_imports(T, Errors)
    end;
auto_imports([{erlang,F,A}|T], Errors) ->
    case erl_internal:bif(F, A) of
	false ->
	    auto_imports(T, Errors);
	true ->
	    io:format("~p/~p: auto-imported, but "
		      "spec claims it is *not* auto-imported", [F,A]),
	    auto_imports(T, Errors+1)
    end;
auto_imports([], 0) ->
    ok;
auto_imports([], Errors) ->
    ct:fail({Errors,inconsistencies}).

extract_functions(M, Abstr) ->
    [{{M,F,A},Body} || {function,_,F,A,Body} <- Abstr].

check_stub({erlang,apply,3}, _) ->
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

binary_to_atom(Config) when is_list(Config) ->
    HalfLong = lists:seq(0, 127),
    HalfLongAtom = list_to_atom(HalfLong),
    HalfLongBin = list_to_binary(HalfLong),
    Long = lists:seq(0, 254),
    LongAtom = list_to_atom(Long),
    LongBin = list_to_binary(Long),

    %% latin1
    '' = test_binary_to_atom(<<>>, latin1),
    '\377' = test_binary_to_atom(<<255>>, latin1),
    HalfLongAtom = test_binary_to_atom(HalfLongBin, latin1),
    LongAtom = test_binary_to_atom(LongBin, latin1),

    %% utf8
    '' = test_binary_to_atom(<<>>, utf8),
    HalfLongAtom = test_binary_to_atom(HalfLongBin, utf8),
    HalfLongAtom = test_binary_to_atom(HalfLongBin, unicode),
    [] = [C || C <- lists:seq(128, 255),
		     begin
			 list_to_atom([C]) =/=
			     test_binary_to_atom(<<C/utf8>>, utf8)
		     end],

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
    ?BADARG(binary_to_atom(id(<<255,0>>), utf8)),
    ?BADARG(binary_to_atom(id(<<16#C0,16#80>>), utf8)), %Overlong 0.
    [?BADARG(binary_to_atom(<<C/utf8>>, utf8)) || C <- lists:seq(256, 16#D7FF)],
    [?BADARG(binary_to_atom(<<C/utf8>>, utf8)) || C <- lists:seq(16#E000, 16#FFFD)],
    [?BADARG(binary_to_atom(<<C/utf8>>, utf8)) || C <- lists:seq(16#10000, 16#8FFFF)],
    [?BADARG(binary_to_atom(<<C/utf8>>, utf8)) || C <- lists:seq(16#90000, 16#10FFFF)],

    %% system_limit failures.
    ?SYS_LIMIT(binary_to_atom(id(<<0:512/unit:8,255>>), utf8)),
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

    UnlikelyAtom = binary_to_atom(id(UnlikelyBin), latin1),
    UnlikelyAtom = binary_to_existing_atom(UnlikelyBin, latin1),
    ok.


atom_to_binary(Config) when is_list(Config) ->
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
    <<>> = atom_to_binary('', utf8),
    <<>> = atom_to_binary('', unicode),
    <<127>> = atom_to_binary('\177', utf8),
    <<"abcdef">> = atom_to_binary(abcdef, utf8),
    HalfLongBin = atom_to_binary(HalfLongAtom, utf8),
    LongAtomBin = atom_to_binary(LongAtom, utf8),
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
    end.

min_max(Config) when is_list(Config) ->	
    a = erlang:min(id(a), a),
    a = erlang:min(id(a), b),
    a = erlang:min(id(b), a),
    b = erlang:min(id(b), b),
    a = erlang:max(id(a), a),
    b = erlang:max(id(a), b),
    b = erlang:max(id(b), a),
    b = erlang:max(id(b), b),

    42.0 = erlang:min(42.0, 42),
    42.0 = erlang:max(42.0, 42),
    %% And now (R14) they are also autoimported!
    a = min(id(a), a),
    a = min(id(a), b),
    a = min(id(b), a),
    b = min(id(b), b),
    a = max(id(a), a),
    b = max(id(a), b),
    b = max(id(b), a),
    b = max(id(b), b),

    42.0 = min(42.0, 42),
    42.0 = max(42.0, 42),
    ok.



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
    H = hostname(),
    {ok,N1} = slave:start(H, halt_node1),
    {badrpc,nodedown} = rpc:call(N1, erlang, halt, []),
    {ok,N2} = slave:start(H, halt_node2),
    {badrpc,nodedown} = rpc:call(N2, erlang, halt, [0]),
    {ok,N3} = slave:start(H, halt_node3),
    {badrpc,nodedown} = rpc:call(N3, erlang, halt, [0,[]]),
    {ok,N4} = slave:start(H, halt_node4),
    {badrpc,nodedown} = rpc:call(N4, erlang, halt, [lists:duplicate(300,$x)]),

    % This test triggers a segfault when dumping a crash dump
    % to make sure that we can handle it properly.
    {ok,N4} = slave:start(H, halt_node4),
    CrashDump = filename:join(proplists:get_value(priv_dir,Config),
                              "segfault_erl_crash.dump"),
    true = rpc:call(N4, os, putenv, ["ERL_CRASH_DUMP",CrashDump]),
    false = rpc:call(N4, erts_debug, set_internal_state,
                     [available_internal_state, true]),
    {badrpc,nodedown} = rpc:call(N4, erts_debug, set_internal_state,
                                 [broken_halt, "Validate correct crash dump"]),
    ok = wait_until_stable_size(CrashDump,-1),
    {ok, Bin} = file:read_file(CrashDump),
    case {string:str(binary_to_list(Bin),"\n=end\n"),
          string:str(binary_to_list(Bin),"\r\n=end\r\n")} of
        {0,0} -> ct:fail("Could not find end marker in crash dump");
        _ -> ok
    end.

wait_until_stable_size(_File,-10) ->
    {error,enoent};
wait_until_stable_size(File,PrevSz) ->
    timer:sleep(250),
    case file:read_file_info(File) of
        {error,enoent} ->
            wait_until_stable_size(File,PrevSz-1);
        {ok,#file_info{size = PrevSz }} when PrevSz /= -1 ->
            io:format("Crashdump file size was: ~p (~s)~n",[PrevSz,File]),
            ok;
        {ok,#file_info{size = NewSz }} ->
            wait_until_stable_size(File,NewSz)
    end.

is_builtin(_Config) ->
    Exp0 = [{M,F,A} || {M,_} <- code:all_loaded(),
		       {F,A} <- M:module_info(exports)],
    Exp = ordsets:from_list(Exp0),

    %% erlang:apply/3 is considered to be built-in, but is not
    %% implemented as other BIFs.

    Builtins0 = [{erlang,apply,3}|erlang:system_info(snifs)],
    Builtins = ordsets:from_list(Builtins0),
    NotBuiltin = ordsets:subtract(Exp, Builtins),
    _ = [true = erlang:is_builtin(M, F, A) || {M,F,A} <- Builtins],
    _ = [false = erlang:is_builtin(M, F, A) || {M,F,A} <- NotBuiltin],

    ok.


%% Helpers
    
id(I) -> I.

%% Get code path, including the path for the erts application.
get_code_path() ->
    case code:lib_dir(erts) of
	{error,bad_name} ->
	    Erts = filename:join([code:root_dir(),"erts","preloaded","ebin"]),
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


hostname() ->
    hostname(atom_to_list(node())).

hostname([$@ | Hostname]) ->
    list_to_atom(Hostname);
hostname([_C | Cs]) ->
    hostname(Cs).
