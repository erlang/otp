%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

-module(beam_peep).

-export([module/2]).

-import(lists, [reverse/1,member/2]).

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
	Class:Error ->
	    Stack = erlang:get_stacktrace(),
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
%% (2) A select_val/4 instruction that only verifies that
%%     its argument is either 'true' or 'false' can be
%%     be replaced with an is_boolean/2 instruction. That is:
%%
%%          select_val Reg Fail   [ true Next false Next ]
%%        Next: ...
%%         
%%     can be rewritten to
%%
%%          is_boolean Fail Reg
%%        Next: ...
%%

peep(Is) ->
    peep(Is, gb_sets:empty(), []).

peep([{bif,tuple_size,_,[_]=Ops,Dst}=I|Is], SeenTests0, Acc) ->
    %% Pretend that we have seen {test,is_tuple,_,Ops}.
    SeenTests1 = gb_sets:add({is_tuple,Ops}, SeenTests0),
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
peep([{test,is_boolean,{f,Fail},Ops}|_]=Is, SeenTests,
     [{test,is_atom,{f,Fail},Ops}|Acc]) ->
    %% The previous is_atom/2 test (with the same failure label) is redundant.
    %% (If is_boolean(Src) is true, is_atom(Src) is also true, so it is
    %% OK to still remember that we have seen is_atom/1.)
    peep(Is, SeenTests, Acc);
peep([{test,Op,_,Ops}=I|Is], SeenTests0, Acc) ->
    case beam_utils:is_pure_test(I) of
	false ->
	    %% Bit syntax matching, which may modify registers and/or
	    %% match state. Clear all information about tests that
	    %% has succeeded.
	    peep(Is, gb_sets:empty(), [I|Acc]);
	true ->
	    Test = {Op,Ops},
	    case gb_sets:is_element(Test, SeenTests0) of
		true ->
		    %% This test has already succeeded and
		    %% is therefore redundant.
		    peep(Is, SeenTests0, Acc);
		false ->
		    %% Remember that we have seen this test.
		    SeenTests = gb_sets:insert(Test, SeenTests0),
		    peep(Is, SeenTests, [I|Acc])
	    end
    end;
peep([{select_val,Src,Fail,
	{list,[{atom,false},{f,L},{atom,true},{f,L}]}}|
      [{label,L}|_]=Is], SeenTests, Acc) ->
    I = {test,is_boolean,Fail,[Src]},
    peep([I|Is], SeenTests, Acc);
peep([{select_val,Src,Fail,
       {list,[{atom,true},{f,L},{atom,false},{f,L}]}}|
      [{label,L}|_]=Is], SeenTests, Acc) ->
    I = {test,is_boolean,Fail,[Src]},
    peep([I|Is], SeenTests, Acc);
peep([I|Is], _, Acc) ->
    %% An unknown instruction. Throw away all information we
    %% have collected about test instructions.
    peep(Is, gb_sets:empty(), [I|Acc]);
peep([], _, Acc) -> reverse(Acc).

kill_seen(Dst, Seen0) ->
    gb_sets:from_ordset(kill_seen_1(gb_sets:to_list(Seen0), Dst)).

kill_seen_1([{_,Ops}=Test|T], Dst) ->
    case member(Dst, Ops) of
	true -> kill_seen_1(T, Dst);
	false -> [Test|kill_seen_1(T, Dst)]
    end;
kill_seen_1([], _) -> [].
