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
%% Purpose: Run right before beam_asm to do any final fix-ups or clean-ups.
%%          (Mandatory.)

-module(beam_z).

-export([module/2]).

-import(lists, [dropwhile/2]).

module({Mod,Exp,Attr,Fs0,Lc}, _Opt) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    try
	Is = undo_renames(Is0),
	{function,Name,Arity,CLabel,Is}
    catch
	Class:Error ->
	    Stack = erlang:get_stacktrace(),
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

undo_renames([{call_ext,2,send}|Is]) ->
    [send|undo_renames(Is)];
undo_renames([{apply,A},{deallocate,N},return|Is]) ->
    [{apply_last,A,N}|undo_renames(Is)];
undo_renames([{call,A,F},{deallocate,N},return|Is]) ->
    [{call_last,A,F,N}|undo_renames(Is)];
undo_renames([{call_ext,A,F},{deallocate,N},return|Is]) ->
    [{call_ext_last,A,F,N}|undo_renames(Is)];
undo_renames([{call,A,F},return|Is]) ->
    [{call_only,A,F}|undo_renames(Is)];
undo_renames([{call_ext,A,F},return|Is]) ->
    [{call_ext_only,A,F}|undo_renames(Is)];
undo_renames([{bif,raise,_,_,_}=I|Is0]) ->
    %% A minor optimization. Done here because:
    %%  (1) beam_jump may move or share 'raise' instructions, and that
    %%      may confuse beam_validator.
    %%  (2) beam_trim cannot do its optimization if the 'deallocate'
    %%      instruction after 'raise' has been removed.
    Is = dropwhile(fun({label,_}) -> false;
		      (_) -> true
		   end, Is0),
    [I|undo_renames(Is)];
undo_renames([I|Is]) ->
    [undo_rename(I)|undo_renames(Is)];
undo_renames([]) -> [].

undo_rename({bs_put,F,{I,U,Fl},[Sz,Src]}) ->
    {I,F,Sz,U,Fl,Src};
undo_rename({bs_put,F,{I,Fl},[Src]}) ->
    {I,F,Fl,Src};
undo_rename({bs_put,{f,0},{bs_put_string,_,_}=I,[]}) ->
    I;
undo_rename({bif,bs_add=I,F,[Src1,Src2,{integer,U}],Dst}) ->
    {I,F,[Src1,Src2,U],Dst};
undo_rename({bif,bs_utf8_size=I,F,[Src],Dst}) ->
    {I,F,Src,Dst};
undo_rename({bif,bs_utf16_size=I,F,[Src],Dst}) ->
    {I,F,Src,Dst};
undo_rename({bs_init,F,{I,U,Flags},none,[Sz,Src],Dst}) ->
    {I,F,Sz,U,Src,Flags,Dst};
undo_rename({bs_init,F,{I,Extra,Flags},Live,[Sz],Dst}) ->
    {I,F,Sz,Extra,Live,Flags,Dst};
undo_rename({bs_init,F,{I,Extra,U,Flags},Live,[Sz,Src],Dst}) ->
    {I,F,Sz,Extra,Live,U,Src,Flags,Dst};
undo_rename({bs_init,_,bs_init_writable=I,_,_,_}) ->
    I;
undo_rename({test,bs_match_string=Op,F,[Ctx,Bin0]}) ->
    Bits = bit_size(Bin0),
    Bin = case Bits rem 8 of
	      0 -> Bin0;
	      Rem -> <<Bin0/bitstring,0:(8-Rem)>>
	  end,
    {test,Op,F,[Ctx,Bits,{string,binary_to_list(Bin)}]};
undo_rename({put_map,Fail,assoc,S,D,R,L}) ->
    {put_map_assoc,Fail,S,D,R,L};
undo_rename({put_map,Fail,exact,S,D,R,L}) ->
    {put_map_exact,Fail,S,D,R,L};
undo_rename({test,has_map_fields,Fail,[Src|List]}) ->
    {test,has_map_fields,Fail,Src,{list,List}};
undo_rename({get_map_elements,Fail,Src,{list,List}}) ->
    {get_map_elements,Fail,Src,{list,List}};
undo_rename({select,I,Reg,Fail,List}) ->
    {I,Reg,Fail,{list,List}};
undo_rename(I) -> I.
