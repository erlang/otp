%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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

%%
%% Purpose: Simple example of NIFs using resource objects to implement functions
%%          for matrix calculations.

-module(matrix_nif).

-export([create/3, pos/3, add/2, size_of/1, to_term/1]).

-define(nif, nif_error(?LINE)). 

-on_load(on_load/0).

on_load() ->
    erlang:load_nif("./matrix_nif", 0).

%% NIFs
create(_Rows, _Cols, _RowList) -> ?nif.
pos(_Mx, _Row, _Col) -> ?nif.
add(_MxA, _MxB) -> ?nif.
size_of(_Mx) -> ?nif.
to_term(_Mx) -> ?nif.

nif_error(Line) ->
    erlang:error({"NIF not implemented in matrix_nif at line", Line}).

