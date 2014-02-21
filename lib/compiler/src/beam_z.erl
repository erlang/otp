%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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
%% Purpose: Run right before beam_asm to do any final fix-ups or clean-ups.
%%          (Mandatory.)

-module(beam_z).

-export([module/2]).

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
undo_rename({put_map,Fail,assoc,S,D,R,L}) ->
    {put_map_assoc,Fail,S,D,R,L};
undo_rename({put_map,Fail,exact,S,D,R,L}) ->
    {put_map_exact,Fail,S,D,R,L};
undo_rename({test,has_map_fields,Fail,[Src|List]}) ->
    {test,has_map_fields,Fail,Src,{list,[to_typed_literal(V)||V<-List]}};
undo_rename({get_map_elements,Fail,Src,{list, List}}) ->
    {get_map_elements,Fail,Src,{list,[to_typed_literal(V)||V<-List]}};
undo_rename({select,I,Reg,Fail,List}) ->
    {I,Reg,Fail,{list,List}};
undo_rename(I) -> I.

%% to_typed_literal(Arg)
%% transform Arg to specific literal i.e. float | integer | atom if applicable
to_typed_literal({literal, V}) when is_float(V) -> {float, V};
to_typed_literal({literal, V}) when is_atom(V) -> {atom, V};
to_typed_literal({literal, V}) when is_integer(V) -> {integer, V};
to_typed_literal({literal, []}) -> nil;
to_typed_literal(V) -> V.
