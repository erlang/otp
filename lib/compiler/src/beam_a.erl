%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2012-2025. All Rights Reserved.
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
%% Purpose: Run directly after code generation to do any normalization
%%          or preparation to simplify the optimization passes.
%%          (Mandatory.)

-module(beam_a).
-moduledoc false.

-export([module/2]).

-include("beam_asm.hrl").

-spec module(beam_asm:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc}, _Opt) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    try
	%% Rename certain operations to simplify the optimization passes.
	Is1 = rename_instrs(Is0),

	%% Remove unusued labels for cleanliness and to help
	%% optimization passes.
	Is2 = beam_jump:remove_unused_labels(Is1),

        %% Some optimization passes can't handle consecutive labels.
        %% Coalesce multiple consecutive labels.
        Is = coalesce_consecutive_labels(Is2, [], []),

        {function,Name,Arity,CLabel,Is}
    catch
        Class:Error:Stack ->
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

rename_instrs([{test,is_eq_exact,_,[Dst,Src]}=Test,
               {move,Src,Dst}|Is]) ->
    %% The move instruction is not needed.
    rename_instrs([Test|Is]);
rename_instrs([{test,is_eq_exact,_,[Same,Same]}|Is]) ->
    %% Same literal or same register. Will always succeed.
    rename_instrs(Is);
rename_instrs([{apply_last,A,N}|Is]) ->
    [{apply,A},{deallocate,N},return|rename_instrs(Is)];
rename_instrs([{call_last,A,F,N}|Is]) ->
    [{call,A,F},{deallocate,N},return|rename_instrs(Is)];
rename_instrs([{call_ext_last,A,F,N}|Is]) ->
    [{call_ext,A,F},{deallocate,N},return|rename_instrs(Is)];
rename_instrs([{call_only,A,F}|Is]) ->
    [{call,A,F},return|rename_instrs(Is)];
rename_instrs([{call_ext_only,A,F}|Is]) ->
    [{call_ext,A,F},return|rename_instrs(Is)];
rename_instrs([{'%live',_}|Is]) ->
    %% Ignore old type of live annotation. Only happens when compiling
    %% from very old .S files.
    rename_instrs(Is);
rename_instrs([{get_list,S,D1,D2}|Is]) ->
    %% Only happens when compiling from old .S files.
    if
        D1 =:= S ->
            [{get_tl,S,D2},{get_hd,S,D1}|rename_instrs(Is)];
        true ->
            [{get_hd,S,D1},{get_tl,S,D2}|rename_instrs(Is)]
    end;
rename_instrs([I|Is]) ->
    [rename_instr(I)|rename_instrs(Is)];
rename_instrs([]) -> [].

rename_instr({bif,Bif,Fail,[A,B],Dst}=I) ->
    case Bif of
        '=<' ->
            {bif,'>=',Fail,[B,A],Dst};
        '<' ->
            case [A,B] of
                [{integer,N},{tr,_,#t_integer{}}=Src] ->
                    {bif,'>=',Fail,[Src,{integer,N+1}],Dst};
                [{tr,_,#t_integer{}}=Src,{integer,N}] ->
                    {bif,'>=',Fail,[{integer,N-1},Src],Dst};
                [_,_] ->
                    I
            end;
        '>' ->
            rename_instr({bif,'<',Fail,[B,A],Dst});
        _ ->
            I
    end;
rename_instr({put_map_assoc,Fail,S,D,R,L}) ->
    {put_map,Fail,assoc,S,D,R,L};
rename_instr({put_map_exact,Fail,S,D,R,L}) ->
    {put_map,Fail,exact,S,D,R,L};
rename_instr({test,has_map_fields,Fail,Src,{list,List}}) ->
    {test,has_map_fields,Fail,[Src|List]};
rename_instr({test,is_nil,Fail,[Src]}) ->
    {test,is_eq_exact,Fail,[Src,nil]};
rename_instr({select_val=I,Reg,Fail,{list,List}}) ->
    {select,I,Reg,Fail,List};
rename_instr({select_tuple_arity=I,Reg,Fail,{list,List}}) ->
    {select,I,Reg,Fail,List};
rename_instr(send) ->
    {call_ext,2,send};
rename_instr(I) -> I.

coalesce_consecutive_labels([{label,L}=Lbl,{label,Alias}|Is], Replace, Acc) ->
    coalesce_consecutive_labels([Lbl|Is], [{Alias,L}|Replace], Acc);
coalesce_consecutive_labels([I|Is], Replace, Acc) ->
    coalesce_consecutive_labels(Is, Replace, [I|Acc]);
coalesce_consecutive_labels([], Replace, Acc) ->
    D = maps:from_list(Replace),
    beam_utils:replace_labels(Acc, [], D, fun(L) -> L end).
