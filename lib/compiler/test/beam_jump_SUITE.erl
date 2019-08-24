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
-module(beam_jump_SUITE).

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 undefined_label/1,ambiguous_catch_try_state/1,
         unsafe_move_elimination/1,build_tuple/1,
         coverage/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,[parallel],
      [undefined_label,
       ambiguous_catch_try_state,
       unsafe_move_elimination,
       build_tuple,
       coverage
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

undefined_label(_Config) ->
    {'EXIT',{function_clause,_}} = (catch flights(0, [], [])),
    ok.

%% Would lose a label when compiled with no_copt.

flights(0, [], []) when [], 0; 0.0, [], false ->
    clark;
flights(_, Reproduction, introduction) when false, Reproduction ->
    responsible.

%% [ERL-209] beam_jump would share 'catch' blocks, causing an
%% ambiguous_catch_try_state error in beam_validator.

ambiguous_catch_try_state(_Config) ->
    {{'EXIT',{{case_clause,song},_}},{'EXIT',{{case_clause,song},_}}} =
	checks(42),
    ok.

river() -> song.

checks(Wanted) ->
    %% Must be one line to cause the unsafe optimization.
    {catch case river() of sheet -> begin +Wanted, if "da" -> Wanted end end end, catch case river() of sheet -> begin + Wanted, if "da" -> Wanted end end end}.

unsafe_move_elimination(_Config) ->
    {{left,right,false},false} = unsafe_move_elimination_1(left, right, false),
    {{false,right,false},false} = unsafe_move_elimination_1(false, right, true),
    {{true,right,right},right} = unsafe_move_elimination_1(true, right, true),
    [ok = unsafe_move_elimination_2(I) || I <- lists:seq(0,16)],
    ok.

unsafe_move_elimination_1(Left, Right, Simple0) ->
    id(1),

    %% The move at label 29 would be removed by beam_jump, which is unsafe because
    %% the two select_val instructions have different source registers.
    %%
    %%   {select_val,{y,0},{f,25},{list,[{atom,true},{f,27},{atom,false},{f,29}]}}.
    %%               ^^^^^                                  ^^^^^^^^^^^^^^^^^^^
    %% {label,27}.
    %%   {kill,{y,0}}.
    %%   {move,{y,2},{x,0}}.
    %%   {line,...}.
    %%   {call,1,{f,31}}.
    %%   {select_val,{x,0},{f,33},{list,[{atom,true},{f,35},{atom,false},{f,29}]}}.
    %%               ^^^^^                                  ^^^^^^^^^^^^^^^^^^^
    %% {label,29}.
    %%   {move,{atom,false},{y,0}}.  <=== REMOVED (unsafely).
    %%   {jump,{f,37}}.

    Simple = case case Simple0 of
                      false -> false;
                      true -> id(Left)
                  end
             of
                 false ->
                     false;
                 true ->
                     id(Right)
             end,
    {id({Left,Right,Simple}),Simple}.

unsafe_move_elimination_2(Int) ->
    %% The type optimization pass would recognize that TagInt can only be
    %% [0 .. 7], so the first 'case' would select_val over [0 .. 6] and swap
    %% out the fail label with the block for 7.
    %%
    %% A later optimization would merge this block with 'expects_h' in the
    %% second case, as the latter is only reachable from the former.
    %%
    %% ... but this broke down when the move elimination optimization didn't
    %% take the fail label of the first select_val into account. This caused it
    %% to believe that the only way to reach 'expects_h' was through the second
    %% case when 'Tag' =:= 'h', which made it remove the move instruction
    %% added in the first case, passing garbage to expects_h/2.
    TagInt = Int band 2#111,
    Tag = case TagInt of
              0 -> a;
              1 -> b;
              2 -> c;
              3 -> d;
              4 -> e;
              5 -> f;
              6 -> g;
              7 -> h
          end,
    case Tag of
        g -> expects_g(TagInt, Tag);
        h -> expects_h(TagInt, Tag);
        _ -> Tag = id(Tag), ok
    end.

expects_g(6, Atom) ->
    Atom = id(g),
    ok.

expects_h(7, Atom) ->
    Atom = id(h),
    ok.

-record(message2, {id, p1}).
-record(message3, {id, p1, p2}).

build_tuple(_Config) ->
    {'EXIT',{{badrecord,message3},_}} = (catch do_build_tuple(#message2{})),
    ok.

do_build_tuple(Message) ->
    if is_record(Message, message2) ->
	    Res = {res, rand:uniform(100)},
	    {Message#message3.id, Res}
    end.

coverage(_Config) ->
    ok = coverage_1(ok),
    {error,badarg} = coverage_1({error,badarg}),

    gt = coverage_2(100, 42),
    le = coverage_2(100, 999),
    le = coverage_2([], []),
    gt = coverage_2([], xxx),

    ok.

coverage_1(Var) ->
    case id(Var) of
	ok -> ok;
	Error -> Error
    end.

%% Cover beam_jump:invert_test(is_ne_exact).
coverage_2(Pre1, Pre2) ->
    case
        case Pre1 == [] of
            false ->
                false;
            true ->
                Pre2 /= []
        end
    of
        true ->
            gt;
        false ->
            case Pre1 > Pre2 of
                true ->
                    gt;
                false ->
                    le
            end
    end.


id(I) ->
    I.
