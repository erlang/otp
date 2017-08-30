%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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

-export([module/2]).

module({Mod,Exp,Attr,Fs0,Lc}, _Opt) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    try
	%% Rename certain operations to simplify the optimization passes.
	Is1 = rename_instrs(Is0),

	%% Remove unusued labels for cleanliness and to help
	%% optimization passes and HiPE.
	Is = beam_jump:remove_unused_labels(Is1),
	{function,Name,Arity,CLabel,Is}
    catch
	Class:Error ->
	    Stack = erlang:get_stacktrace(),
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

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
    %% When compiling from old .S files.
    rename_instrs(Is);
rename_instrs([I|Is]) ->
    [rename_instr(I)|rename_instrs(Is)];
rename_instrs([]) -> [].

rename_instr({bs_put_binary=I,F,Sz,U,Fl,Src}) ->
    {bs_put,F,{I,U,Fl},[Sz,Src]};
rename_instr({bs_put_float=I,F,Sz,U,Fl,Src}) ->
    {bs_put,F,{I,U,Fl},[Sz,Src]};
rename_instr({bs_put_integer=I,F,Sz,U,Fl,Src}) ->
    {bs_put,F,{I,U,Fl},[Sz,Src]};
rename_instr({bs_put_utf8=I,F,Fl,Src}) ->
    {bs_put,F,{I,Fl},[Src]};
rename_instr({bs_put_utf16=I,F,Fl,Src}) ->
    {bs_put,F,{I,Fl},[Src]};
rename_instr({bs_put_utf32=I,F,Fl,Src}) ->
    {bs_put,F,{I,Fl},[Src]};
rename_instr({bs_put_string,_,_}=I) ->
    {bs_put,{f,0},I,[]};
rename_instr({bs_add=I,F,[Src1,Src2,U],Dst}) when is_integer(U) ->
    {bif,I,F,[Src1,Src2,{integer,U}],Dst};
rename_instr({bs_utf8_size=I,F,Src,Dst}) ->
    {bif,I,F,[Src],Dst};
rename_instr({bs_utf16_size=I,F,Src,Dst}) ->
    {bif,I,F,[Src],Dst};
rename_instr({bs_init2=I,F,Sz,Extra,Live,Flags,Dst}) ->
    {bs_init,F,{I,Extra,Flags},Live,[Sz],Dst};
rename_instr({bs_init_bits=I,F,Sz,Extra,Live,Flags,Dst}) ->
    {bs_init,F,{I,Extra,Flags},Live,[Sz],Dst};
rename_instr({bs_append=I,F,Sz,Extra,Live,U,Src,Flags,Dst}) ->
    {bs_init,F,{I,Extra,U,Flags},Live,[Sz,Src],Dst};
rename_instr({bs_private_append=I,F,Sz,U,Src,Flags,Dst}) ->
    {bs_init,F,{I,U,Flags},none,[Sz,Src],Dst};
rename_instr(bs_init_writable=I) ->
    {bs_init,{f,0},I,1,[{x,0}],{x,0}};
rename_instr({test,Op,F,[Ctx,Bits,{string,Str}]}) ->
    %% When compiling from a .S file.
    <<Bs:Bits/bits,_/bits>> = list_to_binary(Str),
    {test,Op,F,[Ctx,Bs]};
rename_instr({put_map_assoc,Fail,S,D,R,L}) ->
    {put_map,Fail,assoc,S,D,R,L};
rename_instr({put_map_exact,Fail,S,D,R,L}) ->
    {put_map,Fail,exact,S,D,R,L};
rename_instr({test,has_map_fields,Fail,Src,{list,List}}) ->
    {test,has_map_fields,Fail,[Src|List]};
rename_instr({select_val=I,Reg,Fail,{list,List}}) ->
    {select,I,Reg,Fail,List};
rename_instr({select_tuple_arity=I,Reg,Fail,{list,List}}) ->
    {select,I,Reg,Fail,List};
rename_instr(send) ->
    {call_ext,2,send};
rename_instr(I) -> I.
