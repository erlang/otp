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
%%     $Id: beam_dict.erl,v 1.1 2008/12/17 09:53:41 mikpe Exp $
%%
%% Purpose : Maintain atom, import, and export tables for assembler.

-module(beam_dict).

-export([new/0, opcode/2, highest_opcode/1,
	 atom/2, local/4, export/4, import/4, string/2, lambda/5,
	 atom_table/1, local_table/1, export_table/1, import_table/1,
	 string_table/1,lambda_table/1]).

-record(asm_dict,
	{atoms = [],				% [{Index, Atom}]
	 exports = [],				% [{F, A, Label}]
	 locals = [],				% [{F, A, Label}]
	 imports = [],				% [{Index, {M, F, A}]
	 strings = [],				% Deep list of characters
	 lambdas = [],				% [{...}]
	 next_atom = 1,
	 next_import = 0,
	 string_offset = 0,
	 highest_opcode = 0
	}).

new() ->
    #asm_dict{}.

%% Remembers highest opcode.

opcode(Op, Dict) when Dict#asm_dict.highest_opcode > Op -> Dict;
opcode(Op, Dict) -> Dict#asm_dict{highest_opcode=Op}.

%% Returns the highest opcode encountered.

highest_opcode(#asm_dict{highest_opcode=Op}) -> Op.

%% Returns the index for an atom (adding it to the atom table if necessary).
%%    atom(Atom, Dict) -> {Index, Dict'}

atom(Atom, Dict) when atom(Atom) ->
    NextIndex = Dict#asm_dict.next_atom,
    case lookup_store(Atom, Dict#asm_dict.atoms, NextIndex) of
	{Index, _, NextIndex} ->
	    {Index, Dict};
	{Index, Atoms, NewIndex} ->
	    {Index, Dict#asm_dict{atoms=Atoms, next_atom=NewIndex}}
    end.

%% Remembers an exported function.
%%    export(Func, Arity, Label, Dict) -> Dict'

export(Func, Arity, Label, Dict0) when atom(Func), integer(Arity), integer(Label) ->
    {Index, Dict1} = atom(Func, Dict0),
    Dict1#asm_dict{exports = [{Index, Arity, Label}| Dict1#asm_dict.exports]}.

%% Remembers a local function.
%%    local(Func, Arity, Label, Dict) -> Dict'

local(Func, Arity, Label, Dict0) when atom(Func), integer(Arity), integer(Label) ->
    {Index,Dict1} = atom(Func, Dict0),
    Dict1#asm_dict{locals = [{Index,Arity,Label}| Dict1#asm_dict.locals]}.

%% Returns the index for an import entry (adding it to the import table if necessary).
%%    import(Mod, Func, Arity, Dict) -> {Index, Dict'}

import(Mod, Func, Arity, Dict) when atom(Mod), atom(Func), integer(Arity) ->
    NextIndex = Dict#asm_dict.next_import,
    case lookup_store({Mod, Func, Arity}, Dict#asm_dict.imports, NextIndex) of
	{Index, _, NextIndex} ->
	    {Index, Dict};
	{Index, Imports, NewIndex} ->
	    {_, D1} = atom(Mod, Dict#asm_dict{imports=Imports, next_import=NewIndex}),
	    {_, D2} = atom(Func, D1),
	    {Index, D2}
    end.

%% Returns the index for a string in the string table (adding the string to the
%% table if necessary).
%%    string(String, Dict) -> {Offset, Dict'}

string(Str, Dict) when list(Str) ->
    #asm_dict{strings = Strings, string_offset = NextOffset} = Dict,
    case old_string(Str, Strings) of
	{true, Offset} ->
	    {Offset, Dict};
	false ->
	    NewDict = Dict#asm_dict{strings = Strings++Str,
				    string_offset = NextOffset+length(Str)},
	    {NextOffset, NewDict}
    end.

%% Returns the index for a funentry (adding it to the table if necessary).
%%    lambda(Dict, Lbl, Index, Uniq, NumFree) -> {Index,Dict'}

lambda(Lbl, Index, OldUniq, NumFree, #asm_dict{lambdas=Lambdas0}=Dict) ->
    OldIndex = length(Lambdas0),
    Lambdas = [{Lbl,{OldIndex,Lbl,Index,NumFree,OldUniq}}|Lambdas0],
    {OldIndex,Dict#asm_dict{lambdas=Lambdas}}.

%% Returns the atom table.
%%    atom_table(Dict) -> [Length,AtomString...]

atom_table(#asm_dict{atoms=Atoms, next_atom=NumAtoms}) ->
    Sorted = lists:sort(Atoms),
    Fun = fun({_, A}) ->
		  L = atom_to_list(A),
		  [length(L)|L]
	  end,
    {NumAtoms-1, lists:map(Fun, Sorted)}.

%% Returns the table of local functions.
%%    local_table(Dict) -> {NumLocals, [{Function, Arity, Label}...]}

local_table(#asm_dict{locals = Locals}) ->
    {length(Locals),Locals}.

%% Returns the export table.
%%    export_table(Dict) -> {NumExports, [{Function, Arity, Label}...]}

export_table(#asm_dict{exports = Exports}) ->
    {length(Exports), Exports}.

%% Returns the import table.
%%    import_table(Dict) -> {NumImports, [{Module, Function, Arity}...]}

import_table(Dict) ->
    #asm_dict{imports = Imports, next_import = NumImports} = Dict,
    Sorted = lists:sort(Imports),
    Fun = fun({_, {Mod, Func, Arity}}) ->
		  {Atom0, _} = atom(Mod, Dict),
		  {Atom1, _} = atom(Func, Dict),
		  {Atom0, Atom1, Arity}
	  end,
    {NumImports, lists:map(Fun, Sorted)}.

string_table(#asm_dict{strings = Strings, string_offset = Size}) ->
    {Size, Strings}.

lambda_table(#asm_dict{locals=Loc0,lambdas=Lambdas0}) ->
    Lambdas1 = sofs:relation(Lambdas0),
    Loc = sofs:relation([{Lbl,{F,A}} || {F,A,Lbl} <- Loc0]),
    Lambdas2 = sofs:relative_product1(Lambdas1, Loc),
    Lambdas = [<<F:32,A:32,Lbl:32,Index:32,NumFree:32,OldUniq:32>> ||
		  {{_,Lbl,Index,NumFree,OldUniq},{F,A}} <- sofs:to_external(Lambdas2)],
    {length(Lambdas),Lambdas}.

%%% Local helper functions.

lookup_store(Key, Dict, NextIndex) ->
    case catch lookup_store1(Key, Dict, NextIndex) of
	Index when integer(Index) ->
	    {Index, Dict, NextIndex};
	{Index, NewDict} ->
	    {Index, NewDict, NextIndex+1}
    end.

lookup_store1(Key, [Pair|Dict], NextIndex) when Key > element(2, Pair) ->
    {Index, NewDict} = lookup_store1(Key, Dict, NextIndex),
    {Index, [Pair|NewDict]};
lookup_store1(Key, [{Index, Key}|_Dict], _NextIndex) ->
    throw(Index);
lookup_store1(Key, Dict, NextIndex) ->
    {NextIndex, [{NextIndex, Key}|Dict]}.

%% Search for string Str in the string pool Pool.
%%   old_string(Str, Pool) -> false | {true, Offset}

old_string(Str, Pool) ->
    old_string(Str, Pool, 0).

old_string([C|Str], [C|Pool], Index) ->
    case lists:prefix(Str, Pool) of
	true ->
	    {true, Index};
	false ->
	    old_string([C|Str], Pool, Index+1)
    end;
old_string(Str, [_|Pool], Index) ->
    old_string(Str, Pool, Index+1);
old_string(_Str, [], _Index) ->
    false.
