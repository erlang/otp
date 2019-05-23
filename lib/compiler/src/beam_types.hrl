%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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

%% Common term types for passes operating on beam SSA and assembly. Helper
%% functions for wrangling these can be found in beam_types.erl
%%
%% The type lattice is as follows:
%%
%%  any                  Any Erlang term (top element).
%%
%%    - atom             An atom.
%%    - {binary,Unit}    A bitstring aligned to unit Unit.
%%    - #t_bs_match{}    A match context.
%%    - #t_fun{}         A fun.
%%    - map              A map.
%%    - number           A number.
%%       -- float        Floating point number.
%%       -- integer      Integer.
%%    - list             A list.
%%       -- cons         Cons (nonempty list).
%%       -- nil          The empty list.
%%    - #t_tuple{}       Tuple.
%%
%%  none                 No type (bottom element).

-define(ATOM_SET_SIZE, 5).

-record(t_atom, {elements=any :: 'any' | [atom()]}).
-record(t_fun, {arity=any :: arity() | 'any'}).
-record(t_integer, {elements=any :: 'any' | {integer(),integer()}}).
-record(t_bs_match, {type :: type()}).
-record(t_tuple, {size=0 :: integer(),
                  exact=false :: boolean(),
                  %% Known element types (1-based index), unknown elements are
                  %% are assumed to be 'any'.
                  elements=#{} :: #{ non_neg_integer() => type() }}).

-type type() :: 'any' | 'none' |
                #t_atom{} | #t_bs_match{} | #t_fun{} | #t_integer{} |
                #t_tuple{} | {'binary',pos_integer()} | 'cons' | 'float' |
                'list' | 'map' | 'nil' | 'number'.
