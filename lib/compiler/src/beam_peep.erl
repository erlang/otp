%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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

-module(beam_peep).

-export([module/2]).

-import(lists, [reverse/1,member/2,usort/1]).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,_}, _Opts) ->
    %% First coalesce adjacent labels.
    {Fs1,Lc} = beam_clean:clean_labels(Fs0),

    %% Do the peep hole optimizations.
    Fs = [function(F) || F <- Fs1],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    try
	Is1 = peep(Is0),
	Is = beam_jump:remove_unused_labels(Is1),
	{function,Name,Arity,CLabel,Is}
    catch
        Class:Error:Stack ->
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.


%% Peep-hole optimizations suitable to perform when most of the
%% optimations passes have been run.
%%
%% (1) In a sequence of tests, we can remove any test instruction
%%     that has been previously seen, because it will certainly
%%     succeed.
%%
%%     For instance, in the following code sequence
%%
%%       is_eq_exact _Fail SomeRegister SomeLiteral
%%       is_ne_exact _Fail SomeOtherRegister SomeOtherLiteral
%%       is_eq_exact _Fail SomeRegister SomeLiteral
%%       is_ne_exact _Fail SomeOtherRegister StillSomeOtherLiteral
%%
%%     the third test is redundant. The code sequence will be produced
%%     by a combination of semicolon and command guards, such as
%%  
%%      InEncoding =:= latin1, OutEncoding =:= unicode; 
%%      InEncoding =:= latin1, OutEncoding =:= utf8 ->
%%

peep(Is) ->
    peep(Is, gb_sets:empty(), []).

peep([{bif,tuple_size,_,[_]=Ops,Dst}=I|Is], SeenTests0, Acc) ->
    %% Pretend that we have seen {test,is_tuple,_,Ops}.
    SeenTests1 = gb_sets:add({is_tuple,Ops}, SeenTests0),
    %% Kill all remembered tests that depend on the destination register.
    SeenTests = kill_seen(Dst, SeenTests1),
    peep(Is, SeenTests, [I|Acc]);
peep([{bif,map_get,_,[Key,Map],Dst}=I|Is], SeenTests0, Acc) ->
    %% Pretend that we have seen {test,has_map_fields,_,[Map,Key]}
    SeenTests1 = gb_sets:add({has_map_fields,[Map,Key]}, SeenTests0),
    %% Kill all remembered tests that depend on the destination register.
    SeenTests = kill_seen(Dst, SeenTests1),
    peep(Is, SeenTests, [I|Acc]);
peep([{bif,_,_,_,Dst}=I|Is], SeenTests0, Acc) ->
    %% Kill all remembered tests that depend on the destination register.
    SeenTests = kill_seen(Dst, SeenTests0),
    peep(Is, SeenTests, [I|Acc]);
peep([{gc_bif,_,_,_,_,Dst}=I|Is], SeenTests0, Acc) ->
    %% Kill all remembered tests that depend on the destination register.
    SeenTests = kill_seen(Dst, SeenTests0),
    peep(Is, SeenTests, [I|Acc]);
peep([{jump,{f,L}},{label,L}=I|Is], _, Acc) ->
    %% Sometimes beam_jump has missed this optimization.
    peep(Is, gb_sets:empty(), [I|Acc]);
peep([{select,select_val,R,F,Vls0}|Is], SeenTests0, Acc0) ->
    case prune_redundant_values(Vls0, F) of
	[] ->
	    %% No values left. Must convert to plain jump.
	    I = {jump,F},
	    peep([I|Is], gb_sets:empty(), Acc0);
        [{atom,_}=Value,Lbl] ->
            %% Single value left. Convert to regular test.
            Is1 = [{test,is_eq_exact,F,[R,Value]},{jump,Lbl}|Is],
            peep(Is1, SeenTests0, Acc0);
        [{integer,_}=Value,Lbl] ->
            %% Single value left. Convert to regular test.
            Is1 = [{test,is_eq_exact,F,[R,Value]},{jump,Lbl}|Is],
            peep(Is1, SeenTests0, Acc0);
	[{atom,B1},Lbl,{atom,B2},Lbl] when B1 =:= not B2 ->
            %% Replace with is_boolean test.
            Is1 = [{test,is_boolean,F,[R]},{jump,Lbl}|Is],
            peep(Is1, SeenTests0, Acc0);
	[_|_]=Vls ->
	    I = {select,select_val,R,F,Vls},
	    peep(Is, gb_sets:empty(), [I|Acc0])
    end;
peep([{get_map_elements,Fail,Src,List}=I|Is], _SeenTests, Acc0) ->
    SeenTests = gb_sets:empty(),
    case simplify_get_map_elements(Fail, Src, List, Acc0) of
        {ok,Acc} ->
            peep(Is, SeenTests, Acc);
        error ->
            peep(Is, SeenTests, [I|Acc0])
    end;
peep([{test,has_map_fields,Fail,Ops}=I|Is], SeenTests, Acc0) ->
    case simplify_has_map_fields(Fail, Ops, Acc0) of
        {ok,Acc} ->
            peep(Is, SeenTests, Acc);
        error ->
            peep(Is, SeenTests, [I|Acc0])
    end;
peep([{test,Op,_,Ops}=I|Is], SeenTests0, Acc) ->
    case beam_utils:is_pure_test(I) of
	false ->
	    %% Bit syntax matching, which may modify registers and/or
	    %% match state. Clear all information about tests that
	    %% has succeeded.
	    peep(Is, gb_sets:empty(), [I|Acc]);
	true ->
	    case is_test_redundant(Op, Ops, SeenTests0) of
		true ->
		    %% This test or a similar test has already succeeded and
		    %% is therefore redundant.
		    peep(Is, SeenTests0, Acc);
		false ->
		    %% Remember that we have seen this test.
		    Test = {Op,Ops},
		    SeenTests = gb_sets:insert(Test, SeenTests0),
		    peep(Is, SeenTests, [I|Acc])
	    end
    end;
peep([I|Is], _, Acc) ->
    %% An unknown instruction. Throw away all information we
    %% have collected about test instructions.
    peep(Is, gb_sets:empty(), [I|Acc]);
peep([], _, Acc) -> reverse(Acc).

is_test_redundant(Op, Ops, Seen) ->
    gb_sets:is_element({Op,Ops}, Seen) orelse
	is_test_redundant_1(Op, Ops, Seen).

is_test_redundant_1(is_boolean, [R], Seen) ->
    gb_sets:is_element({is_eq_exact,[R,{atom,false}]}, Seen) orelse
	gb_sets:is_element({is_eq_exact,[R,{atom,true}]}, Seen);
is_test_redundant_1(_, _, _) -> false.

kill_seen(Dst, Seen0) ->
    gb_sets:from_ordset(kill_seen_1(gb_sets:to_list(Seen0), Dst)).

kill_seen_1([{_,Ops}=Test|T], Dst) ->
    case member(Dst, Ops) of
	true -> kill_seen_1(T, Dst);
	false -> [Test|kill_seen_1(T, Dst)]
    end;
kill_seen_1([], _) -> [].

prune_redundant_values([_Val,F|Vls], F) ->
    prune_redundant_values(Vls, F);
prune_redundant_values([Val,Lbl|Vls], F) ->
    [Val,Lbl|prune_redundant_values(Vls, F)];
prune_redundant_values([], _) -> [].

simplify_get_map_elements(Fail, Src, {list,[Key,Dst]},
                          [{get_map_elements,Fail,Src,{list,List1}}|Acc]) ->
    case are_keys_literals([Key]) andalso are_keys_literals(List1) of
        true ->
            case member(Key, List1) of
                true ->
                    %% The key is already in the other list. That is
                    %% very unusual, because there are optimizations to get
                    %% rid of duplicate keys. Therefore, don't try to
                    %% do anything smart here; just keep the
                    %% get_map_elements instructions separate.
                    error;
                false ->
                    List = [Key,Dst|List1],
                    {ok,[{get_map_elements,Fail,Src,{list,List}}|Acc]}
            end;
        false ->
            error
    end;
simplify_get_map_elements(_, _, _, _) -> error.

simplify_has_map_fields(Fail, [Src|Keys0],
                        [{test,has_map_fields,Fail,[Src|Keys1]}|Acc]) ->
    case are_keys_literals(Keys0) andalso are_keys_literals(Keys1) of
        true ->
            Keys = usort(Keys0 ++ Keys1),
            {ok,[{test,has_map_fields,Fail,[Src|Keys]}|Acc]};
        false ->
            error
    end;
simplify_has_map_fields(_, _, _) -> error.

are_keys_literals([{x,_}|_]) -> false;
are_keys_literals([{y,_}|_]) -> false;
are_keys_literals([_|_]) -> true.
