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
	Is1 = [rename_instr(I) || I <- Is0],

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
%% rename_instr({bs_put_string,_,_}=I) ->
%%     {bs_put,{f,0},I,[]};
rename_instr(I) -> I.
