%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2023. All Rights Reserved.
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
-module(gen_bs_size_unit_checks).

-export([generate/2]).

-import(lists, [reverse/1, seq/2, uniq/1, unzip/1, zip/2]).
-import(string, [join/2]).

generate(File, _Config) ->
    InitialSizes = seq(0, 4) ++ [15, 16, 17, 31, 32, 33, 63, 64, 65,
				 127, 128, 129, 255, 256, 257],
    AddedSizes = seq(0, 4) ++ [15, 16 ,17, 31, 32, 33, 63, 64, 65,
			       127, 128, 129, 255, 256, 257],
    NoofElements = seq(1, 5),

    _ = rand:seed(exro928ss, {123, 123534, 345345}),

    Cases1 = [{Initial,Increment,Elements,FixedSize} ||
		 Initial <- InitialSizes,
		 Increment <- AddedSizes,
		 Elements <- NoofElements,
		 FixedSize <- [fixed, variable, mixed],
		 Elements =< Increment,
		 Increment > 0],
    Cases2 = uniq(Cases1),
    Cases = zip(seq(1, length(Cases2)), Cases2),

    Out = [format_case(["bsu_test_", integer_to_list(Id)],
                       Initial, Increment, Elements, FixedSize)
	   || {Id, {Initial,Increment,Elements,FixedSize}} <- Cases],

    Exports = [["-export([", Export, "]).\n"] || {Export, _} <- Out],
    Bodies = [Body || {_, Body} <- Out],
    Module = ["-module(bsu_tests).\n",
	      "\n",
	      Exports,
	      "\n",
	      Bodies
	     ],
    ok = file:write_file(File, Module).


format_case(Name, InitialSize, AddedSize, NoofElements, fixed) ->
    Expected = gcd(gcd(InitialSize, 256), AddedSize),
    Es = make_elements(AddedSize, NoofElements, 0),
    {Vars, Elements} = unzip(Es),
    Appendable = appendable(InitialSize),
    Comment = [io_lib:format("%% Adding ~p bits to initial accumulator"
                             " of ~p bits using ~p elements.~n",
                             [AddedSize, InitialSize, NoofElements]),
               io_lib:format("%% Expected unit size is ~p~n", [Expected])],
    {[Name, "/1"],
     body(Comment, Name, Vars, Expected, InitialSize, Elements, Appendable)};
format_case(Name, InitialSize, _, NoofElements, variable) ->
    InitialUnit = gcd(InitialSize, 256),
    {Vars, Elements, Unit} =
	make_varsize_elements(NoofElements, 0, InitialUnit),
    Appendable = appendable(InitialSize),
    Comment = [io_lib:format("%% Adding a variable number of bits to initial"
                             " accumulator of ~p bits using~n%% both fixed size"
                             " and variable sized elements.~n",
                             [InitialSize]),
               io_lib:format("%% Expected unit size is ~p~n", [Unit])],
    {[Name, "/1"],
     body(Comment, Name, Vars, Unit, InitialSize, Elements, Appendable)};
format_case(Name, InitialSize, AddedSize, NoofElements, mixed) ->
    InitialUnit = gcd(InitialSize, 256),
    Es = make_elements(AddedSize, NoofElements, 0),
    {FixedVars, FixedElements} = unzip(Es),
    {VaribleVars, VariableElements, Unit1} =
	make_varsize_elements(NoofElements, NoofElements, InitialUnit),
    Unit = gcd(gcd(InitialUnit, AddedSize), Unit1),
    Elements = shuffle(FixedElements ++ VariableElements),
    Appendable = appendable(InitialSize),
    Vars = VaribleVars ++ FixedVars,
    Comment = [io_lib:format("%% Adding a variable number of bits to initial"
                             " accumulator of ~p bits~n%% using ~p elements.~n",
                             [InitialSize, NoofElements]),
               io_lib:format("%% Expected unit size is ~p~n", [Unit])],
    {[Name, "/1"],
     body(Comment, Name, Vars, Unit, InitialSize, Elements, Appendable)}.

body(Comment, Name, Vars, Unit, InitialSize, Elements, Appendable) ->
    [Comment,
     Name, "(Xs) ->\n",
     "%%ssa% (_) when post_ssa_opt ->\n",
     "%%ssa% _ = call(fun ", Name, "/2, _, A)"
     " { result_type => {t_bitstring,",
     integer_to_list(Unit), Appendable, "} }.\n",
     "    ", Name, "(Xs, ", "<<0:", integer_to_list(InitialSize), ">>", ").\n\n",
     Name, "([_", Vars, "|Xs], Acc) ->\n",
     "    ", Name, "(Xs, ", "<<Acc/bitstring", Elements, ">>", ");\n",
     Name, "([], Acc) ->\n",
     "    Acc.\n\n"].

make_elements(0, 0, _) ->
    [];
make_elements(Size, 1, Id) ->
    [format_element_of_size(Size, Id)];
make_elements(Size, NoofElements, Id) when Size =:= NoofElements ->
    [format_element_of_size(1, Id)|
     make_elements(Size - 1, NoofElements - 1, Id + 1)];
make_elements(Size, NoofElements, Id) when Size > NoofElements ->
    MaxSize = Size - NoofElements,
    ElemSize = rand:uniform(MaxSize),
    [format_element_of_size(ElemSize, Id)
    |make_elements(Size - ElemSize, NoofElements - 1, Id + 1)].

appendable(InitialSize) ->
    case InitialSize of
	0 -> ", true";
	_ -> ", false"
    end.

format_element_of_size(Size, Id) ->
    case rand:uniform(2) of
	1 ->
	    format_bitstring_element(Size, Id);
	2 ->
	    format_integer_element(Size, Id)
    end.

format_integer_element(Size, Id) ->
    case rand:uniform(2) of
	1 ->
	    literal_integer_element(Size);
	2 ->
	    variable_integer_element(Size, Id)
    end.

variable_integer_element(Size=32, Id) ->
    Unit = make_unit(Size),
    E = case rand:uniform(2) of
	    1 ->
		io_lib:format(", V~p:~p~s", [Id, Size, Unit]);
	    2 ->
		io_lib:format(", V~p/utf32", [Id])
	end,
    V = io_lib:format(", V~p", [Id]),
    {V, E};
variable_integer_element(Size, Id) ->
    Unit = make_unit(Size),
    E = io_lib:format(", V~p:~p~s", [Id, Size, Unit]),
    V = io_lib:format(", V~p", [Id]),
    {V, E}.

literal_integer_element(Size) ->
    %% We're not doing utf32 here as it is too hard to generate a
    %% valid code point.
    V = rand:uniform(1 bsl Size) - 1,
    Unit = make_unit(Size),
    E = io_lib:format(", ~p:~p~s", [V, Size, Unit]),
    {[], E}.

format_bitstring_element(Size, Id) ->
    case rand:uniform(2) of
	1 ->
	    literal_bitstring_element(Size);
	2 ->
	    variable_bitstring_element(Size, Id)
    end.

variable_bitstring_element(Size, Id) ->
    Unit = make_unit(Size),
    E = io_lib:format(", V~p:~p/bitstring~s", [Id, Size, Unit]),
    V = io_lib:format(", V~p", [Id]),
    {V, E}.

literal_bitstring_element(Size) ->
    V = rand:uniform(1 bsl Size) - 1,
    Unit = make_unit(Size),
    E = io_lib:format(", <<~p:~p>>/bitstring~s", [V, Size, Unit]),
    {[], E}.


make_unit(Size) ->
    case rand:uniform(1) of
	1 ->
	    [];
	2 ->
	    io_lib:format("-unit:~p", [Size])
    end.

make_varsize_elements(NoofElements, Id, Unit0) ->
    make_varsize_elements(NoofElements, Id, Unit0, [], []).

make_varsize_elements(0, _, Unit, Vars, Elements) ->
    {reverse(Vars), reverse(Elements), Unit};
make_varsize_elements(NoofElements, Id, Unit0, Vars, Elements) ->
    Var = io_lib:format(", V~p", [Id]),
    Es = #{ 1 => {binary, 8},
	    2 => {bitstring, 1},
	    3 => {utf8, 8},
	    4 => {utf16, 16} },

    {T,U} = map_get(rand:uniform(4), Es),
    Unit = gcd(U, Unit0),
    Element = io_lib:format(", V~p/~p", [Id, T]),
    make_varsize_elements(NoofElements - 1, Id + 1, Unit,
			  [Var|Vars], [Element|Elements]).

gcd(0, Other) -> Other;
gcd(Other, 0) -> Other;
gcd(A, B) ->
    case A rem B of
        0 -> B;
        X -> gcd(B, X)
    end.

shuffle(Ls) ->
    %% à la Knuth
    A = array:from_list(Ls),
    shuffle(A, array:size(A)).

shuffle(A, 0) ->
    array:to_list(A);
shuffle(A0, Size) ->
    A = swap(rand:uniform(Size) - 1, Size - 1, A0),
    shuffle(A, Size - 1).

swap(X, Y, A) ->
    Xv = array:get(X, A),
    Yv = array:get(Y, A),
    array:set(X, Yv, array:set(Y, Xv, A)).
