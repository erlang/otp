%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
%% Purpose: Maintain atom, import, export, and other tables for assembler.

-module(beam_dict).

-export([new/0,opcode/2,highest_opcode/1,
	 atom/2,local/4,export/4,import/4,
	 string/2,lambda/3,literal/2,line/2,fname/2,
	 atom_table/1,local_table/1,export_table/1,import_table/1,
	 string_table/1,lambda_table/1,literal_table/1,
	 line_table/1]).

-type label() :: non_neg_integer().

-type index() :: non_neg_integer().

-type atom_tab()   :: #{atom() => index()}.
-type import_tab() :: gb_trees:tree(mfa(), index()).
-type fname_tab()  :: #{Name :: term() => index()}.
-type line_tab()   :: #{{Fname :: index(), Line :: term()} => index()}.
-type literal_tab() :: dict:dict(Literal :: term(), index()).

-record(asm,
	{atoms = #{}                :: atom_tab(),
	 exports = []		    :: [{label(), arity(), label()}],
	 locals = []		    :: [{label(), arity(), label()}],
	 imports = gb_trees:empty() :: import_tab(),
	 strings = <<>>		    :: binary(),	%String pool
	 lambdas = {0,[]},				%[{...}]
	 literals = dict:new()	    :: literal_tab(),
	 fnames = #{}               :: fname_tab(),
	 lines = #{}                :: line_tab(),
	 num_lines = 0		    :: non_neg_integer(), %Number of line instructions
	 next_import = 0	    :: non_neg_integer(),
	 string_offset = 0	    :: non_neg_integer(),
	 next_literal = 0	    :: non_neg_integer(),
	 highest_opcode = 0	    :: non_neg_integer()
	}).
-type bdict() :: #asm{}.

%%-----------------------------------------------------------------------------

-spec new() -> bdict().

new() ->
    #asm{}.

%% Remember the highest opcode.
-spec opcode(non_neg_integer(), bdict()) -> bdict().

opcode(Op, Dict) when Dict#asm.highest_opcode >= Op -> Dict;
opcode(Op, Dict) -> Dict#asm{highest_opcode=Op}.

%% Returns the highest opcode encountered.
-spec highest_opcode(bdict()) -> non_neg_integer().

highest_opcode(#asm{highest_opcode=Op}) -> Op.

%% Returns the index for an atom (adding it to the atom table if necessary).
%%    atom(Atom, Dict) -> {Index,Dict'}
-spec atom(atom(), bdict()) -> {pos_integer(), bdict()}.

atom(Atom, #asm{atoms=Atoms}=Dict) when is_atom(Atom) ->
    case Atoms of
        #{ Atom := Index} -> {Index,Dict};
        _ ->
            NextIndex = maps:size(Atoms) + 1,
            {NextIndex,Dict#asm{atoms=Atoms#{Atom=>NextIndex}}}
    end.

%% Remembers an exported function.
%%    export(Func, Arity, Label, Dict) -> Dict'
-spec export(atom(), arity(), label(), bdict()) -> bdict().

export(Func, Arity, Label, Dict0) when is_atom(Func),
				       is_integer(Arity),
				       is_integer(Label) ->
    {Index, Dict1} = atom(Func, Dict0),
    Dict1#asm{exports = [{Index, Arity, Label}| Dict1#asm.exports]}.

%% Remembers a local function.
%%    local(Func, Arity, Label, Dict) -> Dict'
-spec local(atom(), arity(), label(), bdict()) -> bdict().

local(Func, Arity, Label, Dict0) when is_atom(Func),
				      is_integer(Arity),
				      is_integer(Label) ->
    {Index,Dict1} = atom(Func, Dict0),
    Dict1#asm{locals=[{Index,Arity,Label}|Dict1#asm.locals]}.

%% Returns the index for an import entry (adding it to the import table if necessary).
%%    import(Mod, Func, Arity, Dict) -> {Index,Dict'}
-spec import(atom(), atom(), arity(), bdict()) -> {non_neg_integer(), bdict()}.

import(Mod0, Name0, Arity, #asm{imports=Imp0,next_import=NextIndex}=D0)
  when is_atom(Mod0), is_atom(Name0), is_integer(Arity) ->
    {Mod,D1} = atom(Mod0, D0),
    {Name,D2} = atom(Name0, D1),
    MFA = {Mod,Name,Arity},
    case gb_trees:lookup(MFA, Imp0) of
	{value,Index} ->
	    {Index,D2};
	none ->
	    Imp = gb_trees:insert(MFA, NextIndex, Imp0),
	    {NextIndex,D2#asm{imports=Imp,next_import=NextIndex+1}}
    end.

%% Returns the index for a string in the string table (adding the string to the
%% table if necessary).
%%    string(String, Dict) -> {Offset, Dict'}
-spec string(string(), bdict()) -> {non_neg_integer(), bdict()}.

string(Str, Dict) when is_list(Str) ->
    #asm{strings=Strings,string_offset=NextOffset} = Dict,
    StrBin = list_to_binary(Str),
    case old_string(StrBin, Strings) of
	none ->
	    NewDict = Dict#asm{strings = <<Strings/binary,StrBin/binary>>,
			       string_offset=NextOffset+byte_size(StrBin)},
	    {NextOffset,NewDict};
	Offset when is_integer(Offset) ->
	    {NextOffset-Offset,Dict}
    end.

%% Returns the index for a fun entry.
%%    lambda(Lbl, NumFree, Dict) -> {Index,Dict'}
-spec lambda(label(), non_neg_integer(), bdict()) ->
        {non_neg_integer(), bdict()}.

lambda(Lbl, NumFree, #asm{lambdas={OldIndex,Lambdas0}}=Dict) ->
    %% Set Index the same as OldIndex.
    Index = OldIndex,
    %% Initialize OldUniq to 0. It will be set to an unique value
    %% based on the MD5 checksum of the BEAM code for the module.
    OldUniq = 0,
    Lambdas = [{Lbl,{OldIndex,Lbl,Index,NumFree,OldUniq}}|Lambdas0],
    {OldIndex,Dict#asm{lambdas={OldIndex+1,Lambdas}}}.

%% Returns the index for a literal (adding it to the literal table if necessary).
%%    literal(Literal, Dict) -> {Index,Dict'}
-spec literal(term(), bdict()) -> {non_neg_integer(), bdict()}.

literal(Lit, #asm{literals=Tab0,next_literal=NextIndex}=Dict) ->
    case dict:find(Lit, Tab0) of
	{ok,Index} ->
	    {Index,Dict};
	error ->
	    Tab = dict:store(Lit, NextIndex, Tab0),
	    {NextIndex,Dict#asm{literals=Tab,next_literal=NextIndex+1}}
    end.

%% Returns the index for a line instruction (adding information
%% to the location information table).
-spec line(list(), bdict()) -> {non_neg_integer(), bdict()}.

line([], #asm{num_lines=N}=Dict) ->
    %% No location available. Return the special pre-defined
    %% index 0.
    {0,Dict#asm{num_lines=N+1}};
line([{location,Name,Line}], #asm{lines=Lines,num_lines=N}=Dict0) ->
    {FnameIndex,Dict1} = fname(Name, Dict0),
    Key = {FnameIndex,Line},
    case Lines of
        #{Key := Index} -> {Index,Dict1#asm{num_lines=N+1}};
        _ ->
	    Index = maps:size(Lines) + 1,
            {Index, Dict1#asm{lines=Lines#{Key=>Index},num_lines=N+1}}
    end.

fname(Name, #asm{fnames=Fnames}=Dict) ->
    case Fnames of
        #{Name := Index} -> {Index,Dict};
        _ ->
            Index = maps:size(Fnames),
	    {Index,Dict#asm{fnames=Fnames#{Name=>Index}}}
    end.

%% Returns the atom table.
%%    atom_table(Dict) -> {LastIndex,[Length,AtomString...]}
-spec atom_table(bdict()) -> {non_neg_integer(), [[non_neg_integer(),...]]}.

atom_table(#asm{atoms=Atoms}) ->
    NumAtoms = maps:size(Atoms),
    Sorted = lists:keysort(2, maps:to_list(Atoms)),
    {NumAtoms,[begin
                   L = atom_to_list(A),
                   [length(L)|L]
               end || {A,_} <- Sorted]}.

%% Returns the table of local functions.
%%    local_table(Dict) -> {NumLocals, [{Function, Arity, Label}...]}
-spec local_table(bdict()) -> {non_neg_integer(), [{label(),arity(),label()}]}.

local_table(#asm{locals = Locals}) ->
    {length(Locals),Locals}.

%% Returns the export table.
%%    export_table(Dict) -> {NumExports, [{Function, Arity, Label}...]}
-spec export_table(bdict()) -> {non_neg_integer(), [{label(),arity(),label()}]}.

export_table(#asm{exports = Exports}) ->
    {length(Exports),Exports}.

%% Returns the import table.
%%    import_table(Dict) -> {NumImports, [{Module, Function, Arity}...]}
-spec import_table(bdict()) -> {non_neg_integer(), [{label(),label(),arity()}]}.

import_table(#asm{imports=Imp,next_import=NumImports}) ->
    Sorted = lists:keysort(2, gb_trees:to_list(Imp)),
    ImpTab = [MFA || {MFA,_} <- Sorted],
    {NumImports,ImpTab}.

-spec string_table(bdict()) -> {non_neg_integer(), binary()}.

string_table(#asm{strings=Strings,string_offset=Size}) ->
    {Size,Strings}.

-spec lambda_table(bdict()) -> {non_neg_integer(), [<<_:192>>]}.

lambda_table(#asm{locals=Loc0,lambdas={NumLambdas,Lambdas0}}) ->
    Lambdas1 = sofs:relation(Lambdas0),
    Loc = sofs:relation([{Lbl,{F,A}} || {F,A,Lbl} <- Loc0]),
    Lambdas2 = sofs:relative_product1(Lambdas1, Loc),
    Lambdas = [<<F:32,A:32,Lbl:32,Index:32,NumFree:32,OldUniq:32>> ||
		  {{_,Lbl,Index,NumFree,OldUniq},{F,A}} <- sofs:to_external(Lambdas2)],
    {NumLambdas,Lambdas}.

%% Returns the literal table.
%%    literal_table(Dict) -> {NumLiterals, [<<TermSize>>,TermInExternalFormat]}
-spec literal_table(bdict()) -> {non_neg_integer(), [[binary(),...]]}.

literal_table(#asm{literals=Tab,next_literal=NumLiterals}) ->
    L0 = dict:fold(fun(Lit, Num, Acc) ->
			   [{Num,my_term_to_binary(Lit)}|Acc]
		   end, [], Tab),
    L1 = lists:sort(L0),
    L = [[<<(byte_size(Term)):32>>,Term] || {_,Term} <- L1],
    {NumLiterals,L}.

my_term_to_binary(Term) ->
    term_to_binary(Term, [{minor_version,1}]).

%% Return the line table.
-spec line_table(bdict()) ->
    {non_neg_integer(),				%Number of line instructions.
     non_neg_integer(),[string()],
     non_neg_integer(),[{non_neg_integer(),non_neg_integer()}]}.

line_table(#asm{fnames=Fnames0,lines=Lines0,num_lines=NumLineInstrs}) ->
    NumFnames = maps:size(Fnames0),
    Fnames1 = lists:keysort(2, maps:to_list(Fnames0)),
    Fnames = [Name || {Name,_} <- Fnames1],
    NumLines = maps:size(Lines0),
    Lines1 = lists:keysort(2, maps:to_list(Lines0)),
    Lines = [L || {L,_} <- Lines1],
    {NumLineInstrs,NumFnames,Fnames,NumLines,Lines}.

%% Search for binary string Str in the binary string pool Pool.
%%    old_string(Str, Pool) -> none | Index
-spec old_string(binary(), binary()) -> 'none' | pos_integer().

old_string(Str, Pool) ->
    case binary:match(Pool, Str) of
        nomatch -> none;
        {Start,_Length} -> byte_size(Pool) - Start
    end.
