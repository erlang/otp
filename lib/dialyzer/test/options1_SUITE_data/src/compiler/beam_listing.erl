%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: beam_listing.erl,v 1.1 2008/12/17 09:53:41 mikpe Exp $
%%
-module(beam_listing).

-export([module/2]).

-include("v3_life.hrl").

-import(lists, [foreach/2]).

module(File, Core) when element(1, Core) == c_module ->
    %% This is a core module.
    io:put_chars(File, core_pp:format(Core));
module(File, Kern) when element(1, Kern) == k_mdef ->
    %% This is a kernel module.
    io:put_chars(File, v3_kernel_pp:format(Kern));
    %%io:put_chars(File, io_lib:format("~p~n", [Kern]));
module(File, {Mod,Exp,Attr,Kern}) ->
    %% This is output from beam_life (v3).
    io:fwrite(File, "~w.~n~p.~n~p.~n", [Mod,Exp,Attr]),
    foreach(fun (F) -> function(File, F) end, Kern);
module(Stream, {Mod,Exp,Attr,Code,NumLabels}) ->
    %% This is output from beam_codegen.
    io:format(Stream, "{module, ~s}.  %% version = ~w\n",
	      [Mod, beam_opcodes:format_number()]),
    io:format(Stream, "\n{exports, ~p}.\n", [Exp]),
    io:format(Stream, "\n{attributes, ~p}.\n", [Attr]),
    io:format(Stream, "\n{labels, ~p}.\n", [NumLabels]),
    foreach(
      fun ({function,Name,Arity,Entry,Asm}) ->
	      io:format(Stream, "\n\n{function, ~w, ~w, ~w}.\n",
			[Name, Arity, Entry]),
	      foreach(fun(Op) -> print_op(Stream, Op) end, Asm) end,
      Code);
module(Stream, {Mod,Exp,Inter}) ->
    %% Other kinds of intermediate formats.
    io:fwrite(Stream, "~w.~n~p.~n", [Mod,Exp]),
    foreach(fun (F) -> io:format(Stream, "~p.\n", [F]) end, Inter);
module(Stream, [_|_]=Fs) ->
    %% Form-based abstract format.
    foreach(fun (F) -> io:format(Stream, "~p.\n", [F]) end, Fs).

print_op(Stream, Label) when element(1, Label) == label ->
    io:format(Stream, "  ~p.\n", [Label]);
print_op(Stream, Op) ->
    io:format(Stream, "    ~p.\n", [Op]).

function(File, {function,Name,Arity,Args,Body,Vdb}) ->
    io:nl(File),
    io:format(File, "function ~p/~p.\n", [Name,Arity]),
    io:format(File, " ~p.\n", [Args]),
    print_vdb(File, Vdb),
    put(beam_listing_nl, true),
    foreach(fun(F) -> format(File, F, []) end, Body),
    nl(File),
    erase(beam_listing_nl).

format(File, #l{ke=Ke,i=I,vdb=Vdb}, Ind) ->
    nl(File),
    ind_format(File, Ind, "~p ", [I]),
    print_vdb(File, Vdb),
    nl(File),
    format(File, Ke, Ind);
format(File, Tuple, Ind) when is_tuple(Tuple) ->
    ind_format(File, Ind, "{", []),
    format_list(File, tuple_to_list(Tuple), [$\s|Ind]),
    ind_format(File, Ind, "}", []);
format(File, List, Ind) when is_list(List) ->
    ind_format(File, Ind, "[", []),
    format_list(File, List, [$\s|Ind]),
    ind_format(File, Ind, "]", []);
format(File, F, Ind) ->
    ind_format(File, Ind, "~p", [F]).

format_list(File, [F], Ind) ->
    format(File, F, Ind);
format_list(File, [F|Fs], Ind) ->
    format(File, F, Ind),
    ind_format(File, Ind, ",", []),
    format_list(File, Fs, Ind);
format_list(_, [], _) -> ok.


print_vdb(File, [{Var,F,E}|Vs]) ->
    io:format(File, "~p:~p..~p ", [Var,F,E]),
    print_vdb(File, Vs);
print_vdb(_, []) -> ok.

ind_format(File, Ind, Format, Args) ->
    case get(beam_listing_nl) of
	true ->
	    put(beam_listing_nl, false),
	    io:put_chars(File, Ind);
	false -> ok
    end,
    io:format(File, Format, Args).

nl(File) ->
    case put(beam_listing_nl, true) of
	true -> ok;
	false -> io:nl(File)
    end.
