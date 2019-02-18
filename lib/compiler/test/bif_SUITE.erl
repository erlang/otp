%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2018. All Rights Reserved.
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
	 beam_validator/1,trunc_and_friends/1,cover_safe_and_pure_bifs/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,[parallel],
      [beam_validator,
       trunc_and_friends,
       cover_safe_and_pure_bifs
      ]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

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

    ok.
