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
-module(beam_jump_SUITE).

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 undefined_label/1,ambiguous_catch_try_state/1,
         unsafe_move_elimination/1,build_tuple/1,
         coverage/1,call_sharing/1,undecided_allocation/1]).

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
       coverage,
       call_sharing,
       undecided_allocation
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

ambiguous_catch_try_state(Config) ->
    {{'EXIT',{{case_clause,song},_}},{'EXIT',{{case_clause,song},_}}} =
	checks(42),

    {'EXIT',{{try_clause,42},_}} = (catch unsafe_sharing()),

    {'EXIT',{{badmatch,b},_}} = (catch ambiguous_catch_try_state_1(<<>>)),
    {'EXIT',{{badmatch,b},_}} = (catch ambiguous_catch_try_state_1(Config)),

    {'EXIT',{{badmatch,0},_}} = (catch ambiguous_catch_try_state_2()),
    {'EXIT',{{badmatch,0},_}} = (catch ambiguous_catch_try_state_3()),

    {'EXIT',{badarg,_}} = catch ambiguous_catch_try_state_4(),

    ok.

river() -> song.

checks(Wanted) ->
    %% Must be one line to cause the unsafe optimization.
    {catch case river() of sheet -> begin +Wanted, if "da" -> Wanted end end end, catch case river() of sheet -> begin + Wanted, if "da" -> Wanted end end end}.

%% Must be one line to cause the unsafe optimization. Would cause beam_validator to reject the function.
unsafe_sharing() -> try try id(42) catch parent:215 -> []; education:17 -> try 12 catch _:_ -> a end /= if false -> fy end end of [] -> if false -> a end catch _:_ -> name end.

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

%% When compiled with +no_copt, beam_validator would complain about
%% ambiguous try/catch state.
ambiguous_catch_try_state_1(<<42:false>>) ->
    %% The beam_ssa_bsm pass will duplicate the entire second clause.
    %% beam_jump will share the blocks with the build_stacktrace
    %% instructions.
    [];
ambiguous_catch_try_state_1(V0) ->
    try
        try
            receive after bad -> timeout end
        catch
            _:V0 ->
                error
        after
            ok
        end
    of
        true ->
            ok
    catch
        month:power:V2 ->
            %% A build_stacktrace instruction would be shared, causing
            %% an ambiguous try/catch state.
            V2
    after
        a = b
    end.

ambiguous_catch_try_state_2() ->
    case
        try
            case false = 0 of
                   false ->
                       hand
               end
        catch
            idea:[]:V1 ->
                V1;
            country:42 ->
                %% if_end would be shared in an unsafe way.
                if 0 -> way end after [] end of [] -> if $X -> "D" end
    end.

ambiguous_catch_try_state_3() ->
    case
        try
            case false = 0 of
                   false ->
                       hand
               end
        catch
            idea:[]:V1 ->
                V1;
            country:42 ->
                %% case_end would be shared in an unsafe way.
                case x of y -> way end after [] end of [] -> case x of $X -> "D" end
    end.


ambiguous_catch_try_state_4() ->
    0.0 = try binary_to_float(garbage_collect() orelse ((1.0 = tuple_to_list(ok)) -- ok))
          after
              ok
          end.

-record(message2, {id, p1}).
-record(message3, {id, p1, p2}).

build_tuple(_Config) ->
    Message2 = #message2{},
    {'EXIT',{{badrecord,Message2},_}} = (catch do_build_tuple(#message2{})),
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

    error = coverage_3(#{key => <<"child">>}),
    error = coverage_3(#{}),

    ok = coverage_4(whatever),
    -0.5 = coverage_4(any),

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

coverage_3(#{key := <<child>>}) when false ->
    ok;
coverage_3(#{}) ->
    error.

%% Cover beam_jump:value_to_literal/1.
coverage_4(whatever) ->
    maybe
        coverage_4(ok),
        ok
    end;
coverage_4(_) ->
    (bnot 0) / 2.

%% ERIERL-478: The validator failed to validate argument types when calls were
%% shared and the types at the common block turned out wider than the join of
%% each individual call site.
call_sharing(_Config) ->
    A_2 = {a, 1},
    A_3 = {a, 1, 2},

    A_2 = cs_1(id(A_2)),
    A_3 = cs_1(id(A_3)),

    B_2 = {b, 1},
    B_3 = {b, 1, 2},
    B_2 = cs_1(id(B_2)),
    B_3 = cs_1(id(B_3)),

    C_2 = {c, 1},
    C_3 = {c, 1, 2},
    {'EXIT',_} = (catch (cs_1(id(C_2)))),
    {'EXIT',_} = (catch (cs_1(id(C_3)))),

    ok.

cs_1(Key) ->
    A = case Key of
            %% Must be a single line to trigger the bug.
            {Tag, _, _} when Tag == a; Tag == b -> cs_2(Key); {Tag, _} when Tag == a; Tag == b -> cs_2(Key)
        end,
    id(A).

cs_2(I) -> I.

undecided_allocation(_Config) ->
    ok = catch undecided_allocation_1(<<10:(3*7)>>),
    {'EXIT',{{badrecord,<<0>>},_}} = catch undecided_allocation_1(8),

    {bar,1} = undecided_allocation_2(id(<<"bar">>)),
    {foo,2} = undecided_allocation_2(id(<<"foo">>)),
    {'EXIT',_} = catch undecided_allocation_2(id(<<"foobar">>)),
    {'EXIT',{{badmatch,error},_}} = catch undecided_allocation_2(id("foo,bar")),
    {'EXIT',_} = catch undecided_allocation_2(id(foobar)),
    {'EXIT',_} = catch undecided_allocation_2(id(make_ref())),

    ok = undecided_allocation_3(id(<<0>>), gurka),
    {'EXIT', {badarith, _}} = catch undecided_allocation_3(id(<<>>), gurka),

    ok.

-record(rec, {}).
undecided_allocation_1(<<10:3/integer-unit:7>>) ->
    ok;
undecided_allocation_1(V) ->
    %% The record update operation would be duplicated by the beam_ssa_bssm
    %% pass, and beam_jump would incorrectly share the resulting calls to
    %% error/1, causing beam_validator to issue the following diagnostic
    %% when this module was compiled with the no_type_opt option:
    %%
    %%  Internal consistency check failed - please report this bug.
    %%  Instruction: {call_ext,1,{extfunc,erlang,error,1}}
    %%  Error:       {allocated,undecided}:

    <<
      <<0>> || <<0:V>> <= <<0>>
    >>#rec{},
    if whatever -> [] end.

undecided_allocation_2(Order) ->
    {_, _} =
        case Order of
            <<"bar">> ->
                {bar, 1};
            <<"foo">> ->
                {foo, 2};
            S ->
                case string:split("foo", "o") of
                    [] ->
                        ok;
                    _ ->
                        %% The beam_ssa_bsm pass will duplicate this code,
                        %% and beam_jump would undo the duplication by sharing
                        %% the code that calls lists:flatten/1. The problem was
                        %% that the stack frames had different sizes for the
                        %% two calls to lists:flatten/1. The diagnostic would be:
                        %%
                        %%  Internal consistency check failed - please report this bug.
                        %%  Instruction: {call_ext,1,{extfunc,lists,flatten,1}}
                        %%  Error:       {allocated,undecided}:

                        lists:flatten(
                            case S of
                                Y when is_binary(Y) -> Y;
                                Y -> string:split(Y, ",")
                            end
                        )
                end,
                error
        end.

%% GH-6571: bs_init_writable can only be shared when the stack frame size is
%% known.
undecided_allocation_3(<<_>>, _) ->
    ok;
undecided_allocation_3(X, _) ->
    case 0 + get_keys() of
        X ->
            ok;
        _ ->
            (node() orelse garbage_collect()) =:=
                case <<0 || false>> of
                    #{} ->
                        ok
                end
    end.

id(I) ->
    I.
