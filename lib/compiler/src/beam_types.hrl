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
%%  any                      Any Erlang term (top element).
%%
%%    - #t_atom{}            Atom, or a set thereof.
%%    - #t_bs_matchable{}    Binary-matchable types.
%%        - #t_bitstring{}   Bitstring.
%%        - #t_bs_context{}  Match context.
%%    - #t_fun{}             Fun.
%%    - #t_map{}             Map.
%%    - number               Any number.
%%       -- #t_float{}       Floating point number.
%%       -- #t_integer{}     Integer.
%%    - list                 Any list.
%%       -- cons             Cons (nonempty list).
%%       -- nil              The empty list.
%%    - #t_tuple{}           Tuple.
%%
%%  none                     No type (bottom element).
%%
%% We also use #t_union{} to represent conflicting types produced by certain
%% expressions, e.g. the "#t_atom{} or #t_tuple{}" of lists:keyfind/3, which is
%% very useful for preserving type information when we would otherwise have
%% reduced it to 'any'. Since few operations can make direct use of this extra
%% type information, types should generally be normalized to one of the above
%% before use.

-define(ATOM_SET_SIZE, 5).

-record(t_atom, {elements=any :: 'any' | [atom()]}).
-record(t_float, {elements=any :: 'any' | {float(),float()}}).
-record(t_fun, {arity=any :: arity() | 'any'}).
-record(t_integer, {elements=any :: 'any' | {integer(),integer()}}).
-record(t_bitstring, {size_unit=1 :: pos_integer()}).
-record(t_bs_context, {tail_unit=1 :: pos_integer(),
                       slots=0 :: non_neg_integer(),
                       valid=0 :: non_neg_integer()}).
-record(t_bs_matchable, {tail_unit=1}).
-record(t_map, {}).
-record(t_tuple, {size=0 :: integer(),
                  exact=false :: boolean(),
                  elements=#{} :: tuple_elements()}).

%% Known element types, where the key is a 1-based integer index. Unknown
%% elements are assumed to be 'any', and indexes above ?TUPLE_ELEMENT_LIMIT are
%% ignored for performance reasons.

-define(TUPLE_ELEMENT_LIMIT, 12).
-type tuple_elements() :: #{ Key :: pos_integer() => type() }.

-type normal_type() :: any | none |
                       list | cons | nil |
                       number | #t_float{} | #t_integer{} |
                       #t_atom{} |
                       #t_bitstring{} | #t_bs_context{} | #t_bs_matchable{} |
                       #t_fun{} |
                       #t_map{} |
                       #t_tuple{}.

-type record_key() :: {Arity :: integer(), Tag :: normal_type() }.
-type record_set() :: ordsets:ordset({record_key(), #t_tuple{}}).
-type tuple_set() :: #t_tuple{} | record_set().

-record(t_union, {atom=none :: none | #t_atom{},
                  list=none :: none | list | cons | nil,
                  number=none :: none | number | #t_float{} | #t_integer{},
                  tuple_set=none :: none | tuple_set(),
                  other=none :: normal_type()}).

-type type() :: #t_union{} | normal_type().
