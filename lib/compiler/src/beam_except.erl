%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2018. All Rights Reserved.
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

-module(beam_except).
-export([module/2]).

%%% Rewrite certain calls to erlang:error/{1,2} to specialized
%%% instructions:
%%%
%%% erlang:error({badmatch,Value})       => badmatch Value
%%% erlang:error({case_clause,Value})    => case_end Value
%%% erlang:error({try_clause,Value})     => try_case_end Value
%%% erlang:error(if_clause)              => if_end
%%% erlang:error(function_clause, Args)  => jump FuncInfoLabel
%%%

-import(lists, [reverse/1,reverse/2,seq/2,splitwith/2]).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc}, _Opt) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    try
	Is = function_1(Is0),
	{function,Name,Arity,CLabel,Is}
    catch
        Class:Error:Stack ->
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

-record(st,
	{lbl :: beam_asm:label(),              %func_info label
	 loc :: [_],                           %location for func_info
	 arity :: arity()                      %arity for function
	 }).

function_1(Is0) ->
    case Is0 of
	[{label,Lbl},{line,Loc},{func_info,_,_,Arity}|_] ->
	    St = #st{lbl=Lbl,loc=Loc,arity=Arity},
	    translate(Is0, St, []);
	[{label,_}|_] ->
	    %% No line numbers. The source must be a .S file.
	    %% There is no need to do anything.
	    Is0
    end.

translate([{call_ext,Ar,{extfunc,erlang,error,Ar}}=I|Is], St, Acc) ->
    translate_1(Ar, I, Is, St, Acc);
translate([I|Is], St, Acc) ->
    translate(Is, St, [I|Acc]);
translate([], _, Acc) ->
    reverse(Acc).

translate_1(Ar, I, Is, #st{arity=Arity}=St, [{line,_}=Line|Acc1]=Acc0) ->
    case dig_out(Ar, Arity, Acc1) of
	no ->
	    translate(Is, St, [I|Acc0]);
	{yes,function_clause,Acc2} ->
	    case {Is,Line,St} of
		{[return|_],{line,Loc},#st{lbl=Fi,loc=Loc}} ->
		    Instr = {jump,{f,Fi}},
		    translate(Is, St, [Instr|Acc2]);
		{_,_,_} ->
                    %% Not a call_only instruction, or not the same
                    %% location information as in in the line instruction
                    %% before the func_info instruction. Not safe
                    %% to translate to a jump.
		    translate(Is, St, [I|Acc0])
	    end;
	{yes,Instr,Acc2} ->
	    translate(Is, St, [Instr,Line|Acc2])
    end.

dig_out(1, _Arity, Is) ->
    dig_out(Is);
dig_out(2, Arity, Is) ->
    dig_out_fc(Arity, Is);
dig_out(_, _, _) -> no.

dig_out([{block,Bl0}|Is]) ->
    case dig_out_block(reverse(Bl0)) of
	no -> no;
	{yes,What,[]} ->
	    {yes,What,Is};
	{yes,What,Bl} ->
	    {yes,What,[{block,Bl}|Is]}
    end;
dig_out(_) -> no.

dig_out_block([{set,[{x,0}],[{atom,if_clause}],move}]) ->
    {yes,if_end,[]};
dig_out_block([{set,[{x,0}],[{literal,{Exc,Value}}],move}|Is]) ->
    translate_exception(Exc, {literal,Value}, Is, 0);
dig_out_block([{set,[{x,0}],[{atom,Exc},Value],put_tuple2}|Is]) ->
    translate_exception(Exc, Value, Is, 3);
dig_out_block(_) -> no.

translate_exception(badmatch, Val, Is, Words) ->
    {yes,{badmatch,Val},fix_block(Is, Words)};
translate_exception(case_clause, Val, Is, Words) ->
    {yes,{case_end,Val},fix_block(Is, Words)};
translate_exception(try_clause, Val, Is, Words) ->
    {yes,{try_case_end,Val},fix_block(Is, Words)};
translate_exception(_, _, _, _) -> no.

fix_block(Is, 0) ->
    reverse(Is);
fix_block(Is, Words) ->
    reverse(fix_block_1(Is, Words)).

fix_block_1([{set,[],[],{alloc,Live,{F1,F2,Needed0,F3}}}|Is], Words)
  when is_integer(Needed0) ->
    case Needed0 - Words of
        0 ->
            Is;
        Needed ->
            true = Needed >= 0,				%Assertion.
            [{set,[],[],{alloc,Live,{F1,F2,Needed,F3}}}|Is]
    end;
fix_block_1([I|Is], Words) ->
    [I|fix_block_1(Is, Words)];
fix_block_1([], _Words) ->
    %% Rare. The heap allocation was probably done by a binary
    %% construction instruction.
    [].

dig_out_fc(Arity, Is0) ->
    Regs0 = maps:from_list([{{x,X},{arg,X}} || X <- seq(0, Arity-1)]),
    {Is,Acc0} = splitwith(fun({label,_}) -> false;
                             ({test,_,_,_}) -> false;
                             (_) -> true
                          end, Is0),
    {Regs,Acc} = dig_out_fc_1(reverse(Is), Arity, Regs0, Acc0),
    case Regs of
        #{{x,0}:={atom,function_clause},{x,1}:=Args} ->
            case moves_from_stack(Args, 0, []) of
                {Moves,Arity} ->
                    {yes,function_clause,reverse(Moves, Acc)};
                {_,_} ->
                    no
            end;
        #{} ->
            no
    end.

dig_out_fc_1([{block,Bl}|Is], Arity, Regs0, Acc) ->
    Regs = dig_out_fc_block(Bl, Regs0),
    dig_out_fc_1(Is, Arity, Regs, Acc);
dig_out_fc_1([{bs_set_position,_,_}=I|Is], Arity, Regs, Acc) ->
    dig_out_fc_1(Is, Arity, Regs, [I|Acc]);
dig_out_fc_1([{bs_get_tail,Src,Dst,Live0}|Is], Arity, Regs0, Acc) ->
    case Src of
        {x,X} when X < Arity ->
            %% The heuristic for determining the number of live
            %% registers is likely to give an incorrect result.
            %% Give up.
            {#{},[]};
        _ ->
            Regs = prune_xregs(Live0, Regs0),
            Live = dig_out_stack_live(Regs, Live0),
            I = {bs_get_tail,Src,Dst,Live},
            dig_out_fc_1(Is, Arity, Regs, [I|Acc])
    end;
dig_out_fc_1([_|_], _Arity, _Regs, _Acc) ->
    {#{},[]};
dig_out_fc_1([], _Arity, Regs, Acc) ->
    {Regs,Acc}.

dig_out_fc_block([{set,[],[],{alloc,Live,_}}|Is], Regs0) ->
    Regs = prune_xregs(Live, Regs0),
    dig_out_fc_block(Is, Regs);
dig_out_fc_block([{set,[Dst],[Hd,Tl],put_list}|Is], Regs0) ->
    Regs = Regs0#{Dst=>{cons,get_reg(Hd, Regs0),get_reg(Tl, Regs0)}},
    dig_out_fc_block(Is, Regs);
dig_out_fc_block([{set,[Dst],[Src],move}|Is], Regs0) ->
    Regs = Regs0#{Dst=>get_reg(Src, Regs0)},
    dig_out_fc_block(Is, Regs);
dig_out_fc_block([{set,_,_,_}|_], _Regs) ->
    %% Unknown instruction. Fail.
    #{};
dig_out_fc_block([], Regs) -> Regs.

dig_out_stack_live(Regs, Default) ->
    Reg = {x,2},
    case Regs of
        #{Reg:=List} ->
            dig_out_stack_live_1(List, Default);
        #{} ->
            Default
    end.

dig_out_stack_live_1({cons,{arg,N},T}, Live) ->
    dig_out_stack_live_1(T, max(N + 1, Live));
dig_out_stack_live_1({cons,_,T}, Live) ->
    dig_out_stack_live_1(T, Live);
dig_out_stack_live_1(nil, Live) ->
    Live;
dig_out_stack_live_1(_, Live) -> Live.

prune_xregs(Live, Regs) ->
    maps:filter(fun({x,X}, _) -> X < Live end, Regs).

moves_from_stack({cons,{arg,N},_}, I, _Acc) when N =/= I ->
    %% Wrong argument. Give up.
    {[],-1};
moves_from_stack({cons,H,T}, I, Acc) ->
    case H of
        {arg,I} ->
            moves_from_stack(T, I+1, Acc);
        _ ->
            moves_from_stack(T, I+1, [{move,H,{x,I}}|Acc])
    end;
moves_from_stack(nil, I, Acc) ->
    {reverse(Acc),I};
moves_from_stack({literal,[H|T]}, I, Acc) ->
    Cons = {cons,tag_literal(H),tag_literal(T)},
    moves_from_stack(Cons, I, Acc);
moves_from_stack(_, _, _) ->
    %% Not understood. Give up.
    {[],-1}.


get_reg(R, Regs) ->
    case Regs of
        #{R:=Val} -> Val;
        #{} -> R
    end.

tag_literal([]) -> nil;
tag_literal(T) when is_atom(T) -> {atom,T};
tag_literal(T) when is_float(T) -> {float,T};
tag_literal(T) when is_integer(T) -> {integer,T};
tag_literal(T) -> {literal,T}.
