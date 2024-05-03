%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2024. All Rights Reserved.
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

-include_lib("syntax_tools/include/merl.hrl").

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
         unsafe_get_list/1,
	 beam_validator/1,trunc_and_friends/1,cover_safe_and_pure_bifs/1,
         cover_trim/1,
         head_tail/1,
         min_max/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,test_lib:parallel(),
      [beam_validator,
       unsafe_get_list,
       trunc_and_friends,
       cover_safe_and_pure_bifs,
       cover_trim,
       head_tail,
       min_max
      ]}].

init_per_suite(Config) ->
    _ = id(Config),
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

unsafe_get_list(_Config) ->
    [[1], [1], [1]] = create_rows(id(3)),
    ok.

create_rows(Num) -> create_rows(Num, [[1]]).

create_rows(1, Rows) ->
    Rows;
create_rows(Num, [PrevRow | _] = Rows) ->
    [_PrevRowH | PrevRowT] = PrevRow,
    [] = first(PrevRowT, PrevRow),
    create_rows(Num - 1, [[1] | Rows]).

first(Fst, _Snd) -> Fst.

%% Cover code in beam_validator.

beam_validator(Config) ->
    [false,Config] = food(Config),

    true = is_number(42.0),
    false = is_port(Config),

    ok.

food(Curriculum) ->
    [try
	 is_bitstring(functions)
     catch _ ->
	     0
     end, Curriculum].

%% Test trunc/1, round/1, floor/1, ceil/1.
trunc_and_friends(_Config) ->
    Bifs = [trunc,round,floor,ceil],
    Fs = trunc_and_friends_1(Bifs),
    Mod = ?FUNCTION_NAME,
    Calls = [begin
		 Atom = erl_syntax:function_name(N),
		 ?Q("'@Atom'()")
	     end || N <- Fs],
    Tree = ?Q(["-module('@Mod@').",
	       "-export([test/0]).",
	       "test() -> _@Calls, ok.",
	       "id(I) -> I."]) ++ Fs,
    merl:print(Tree),
    Opts = test_lib:opt_opts(?MODULE),
    {ok,_Bin} = merl:compile_and_load(Tree, Opts),
    Mod:test(),
    ok.

trunc_and_friends_1([F|Fs]) ->
    Func = list_to_atom("f"++integer_to_list(length(Fs))),
    [trunc_template(Func, F)|trunc_and_friends_1(Fs)];
trunc_and_friends_1([]) -> [].

trunc_template(Func, Bif) ->
    Val = 42.77,
    Res = erlang:Bif(Val),
    FloatRes = float(Res),
    ?Q("'@Func@'() ->
        Var = id(_@Val@),
        if _@Bif@(Var) =:= _@Res@ -> ok end,
	if _@Bif@(Var) == _@FloatRes@ -> ok end,
	if _@Bif@(Var) == _@Res@ -> ok end,
        _@Res@ = _@Bif@(Var),
        try begin _@Bif@(a), ok end
        catch error:badarg -> ok end,
        ok.").

cover_safe_and_pure_bifs(Config) ->
    _ = get(),
    _ = get_keys(a),
    _ = group_leader(),
    _ = is_alive(),
    _ = min(Config, []),
    _ = nodes(),
    _ = erlang:ports(),
    _ = pre_loaded(),
    _ = processes(),
    _ = registered(),
    _ = term_to_binary(Config),
    42 = list_to_integer("2A", 16),
    a = binary_to_atom(atom_to_binary(a)),

    ok.

cover_trim(_Config) ->
    ok = cover_trim_1(<<"abc">>, id([42])),
    ok = cover_trim_1({a,b,c}, id([42])),

    true = cover_trim_2("keep-alive", "1"),
    false = cover_trim_2("keep-alive", "0"),
    false = cover_trim_2("other", "1"),
    false = cover_trim_2("other", "0"),

    true = cover_trim_3("keep-alive", -1),
    false = cover_trim_3("keep-alive", 100),
    false = cover_trim_3("other", -10),
    false = cover_trim_3("other", -100),

    ok.

cover_trim_1(Something, V) ->
    id(Something),
    id(Something),
    if
        hd(V) =:= 42 ->
            ok
    end.

cover_trim_2(Header, NList)->
    id(0),
    case id(Header) of
        "keep-alive" when hd(NList) >= $1 ->
            true;
        _Connect ->
            false
    end.

cover_trim_3(Header, N)->
    id(0),
    case id(Header) of
        "keep-alive" when abs(N) < 42 ->
            true;
        _Connect ->
            false
    end.

%% GH-7024: The loader transformations for hd/1 and tl/1 were incorrect and
%% failed when certain optimizations were turned off.
head_tail(_Config) ->
    {1, ok} = head_case(),
    {1, ok} = tail_case(),

    1 = hd(id([1])),
    [] = tl(id([1])),

    ok.

head_case() ->
    case 1 of
        X when hd(X) -> blurf;
        X -> {X, ok}
    end.

tail_case() ->
    case 1 of
        X when tl(X) -> blurf;
        X -> {X, ok}
    end.

min_max(_Config) ->
    False = id(false),
    True = id(true),

    false = bool_min_false(False, False),
    false = bool_min_false(False, True),
    false = bool_min_false(True, False),
    true = bool_min_true(True, True),

    false = bool_max_false(False, False),
    true = bool_max_true(False, True),
    true = bool_max_true(True, False),
    true = bool_max_true(True, True),

    11 = min_increment(100),
    11 = min_increment(10),
    10 = min_increment(9),
    1 = min_increment(0),
    0 = min_increment(-1),
    11 = min_increment(a),

    {42,42} = max_number(id(42)),
    {42,42.0} = max_number(id(42.0)),
    {-1,1} = max_number(id(-1)),
    {-1,1} = max_number(id(-1.0)),

    100 = int_clamped_add(-1),
    100 = int_clamped_add(0),
    105 = int_clamped_add(5),
    110 = int_clamped_add(10),
    110 = int_clamped_add(11),

    100 = num_clamped_add(-1),
    100 = num_clamped_add(0),
    105 = num_clamped_add(5),
    110 = num_clamped_add(10),
    110 = num_clamped_add(11),

    105 = num_clamped_add(5),
    105.0 = num_clamped_add(5.0),
    110 = num_clamped_add(a),
    110 = num_clamped_add({a,b,c}),
    110 = num_clamped_add({a,b,c}),

    ok.

%% GH-7170: The following functions would cause a crash in
%% beam_ssa_codegen.

bool_min_false(A, B) when is_boolean(A), is_boolean(B) ->
    false = min(A, B).

bool_min_true(A, B) when is_boolean(A), is_boolean(B) ->
    true = min(A, B).

bool_max_false(A, B) when is_boolean(A), is_boolean(B) ->
    false = max(A, B).

bool_max_true(A, B) when is_boolean(B) ->
    true = max(A, B),
    if
        is_boolean(A) ->
            true = max(A, B)
    end.

max_number(A) ->
    Res = {trunc(A), max(A, 1)},
    Res = {trunc(A), max(1, A)}.

min_increment(A) ->
    Res = min(10, A) + 1,
    Res = min(A, 10) + 1,
    Res = min(id(A), 10) + 1.

int_clamped_add(A) when is_integer(A) ->
    min(max(A, 0), 10) + 100.

num_clamped_add(A) ->
    min(max(A, 0), 10) + 100.

%%%
%%% Common utilities.
%%%

id(I) ->
    I.
