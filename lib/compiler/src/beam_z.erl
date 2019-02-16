%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2018. All Rights Reserved.
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

-import(lists, [dropwhile/2,map/2]).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_asm:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc}, Opts) ->
    NoGetHdTl = proplists:get_bool(no_get_hd_tl, Opts),
    Fs = [function(F, NoGetHdTl) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}, NoGetHdTl) ->
    try
	Is1 = undo_renames(Is0),
        Is = maybe_eliminate_get_hd_tl(Is1, NoGetHdTl),
	{function,Name,Arity,CLabel,Is}
    catch
        Class:Error:Stack ->
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
undo_renames([{get_hd,Src,Dst1},{get_tl,Src,Dst2}|Is]) ->
    [{get_list,Src,Dst1,Dst2}|undo_renames(Is)];
undo_renames([{get_tl,Src,Dst2},{get_hd,Src,Dst1}|Is]) ->
    [{get_list,Src,Dst1,Dst2}|undo_renames(Is)];
undo_renames([{bs_put,_,{bs_put_binary,1,_},
               [{atom,all},{literal,<<>>}]}|Is]) ->
    undo_renames(Is);
undo_renames([{bs_put,Fail,{bs_put_binary,1,_Flags},
               [{atom,all},{literal,BinString}]}|Is0]) ->
    Bits = bit_size(BinString),
    Bytes = Bits div 8,
    case Bits rem 8 of
        0 ->
            I = {bs_put_string,byte_size(BinString),
                 {string,BinString}},
            [undo_rename(I)|undo_renames(Is0)];
        Rem ->
            <<Binary:Bytes/bytes,Int:Rem>> = BinString,
            PutInt = {bs_put_integer,Fail,{integer,Rem},1,
                      {field_flags,[unsigned,big]},{integer,Int}},
            Is = [PutInt|undo_renames(Is0)],
            case Binary of
                <<>> ->
                    Is;
                _ ->
                    [{bs_put_string,byte_size(Binary),
                      {string,Binary}}|Is]
            end
    end;
undo_renames([I|Is]) ->
    [undo_rename(I)|undo_renames(Is)];
undo_renames([]) -> [].

undo_rename({bs_put,F,{I,U,Fl},[Sz,Src]}) ->
    {I,F,Sz,U,Fl,Src};
undo_rename({bs_put,F,{I,Fl},[Src]}) ->
    {I,F,Fl,Src};
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
    {test,Op,F,[Ctx,Bits,{string,Bin}]};
undo_rename({put_map,Fail,assoc,S,D,R,L}) ->
    {put_map_assoc,Fail,S,D,R,L};
undo_rename({put_map,Fail,exact,S,D,R,L}) ->
    {put_map_exact,Fail,S,D,R,L};
undo_rename({test,has_map_fields,Fail,[Src|List]}) ->
    {test,has_map_fields,Fail,Src,{list,List}};
undo_rename({get_map_elements,Fail,Src,{list,List}}) ->
    {get_map_elements,Fail,Src,{list,List}};
undo_rename({test,is_eq_exact,Fail,[Src,nil]}) ->
    {test,is_nil,Fail,[Src]};
undo_rename({select,I,Reg,Fail,List}) ->
    {I,Reg,Fail,{list,List}};
undo_rename(I) -> I.

%%%
%%% Eliminate get_hd/get_tl instructions if requested by
%%% the no_get_hd_tl option.
%%%

maybe_eliminate_get_hd_tl(Is, true) ->
    map(fun({get_hd,Cons,Hd}) ->
                {get_list,Cons,Hd,{x,1022}};
           ({get_tl,Cons,Tl}) ->
                {get_list,Cons,{x,1022},Tl};
           (I) -> I
        end, Is);
maybe_eliminate_get_hd_tl(Is, false) -> Is.
