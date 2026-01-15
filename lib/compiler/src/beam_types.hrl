%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2019-2025. All Rights Reserved.
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

%% Type version, must be bumped whenever the external type format changes.
-define(BEAM_TYPES_VERSION, 3).

%% Common term types for passes operating on beam SSA and assembly. Helper
%% functions for wrangling these can be found in beam_types.erl
%%
%% The type lattice is as follows:
%%
%%  any                      Any Erlang term (top element).
%%
%%    - #t_atom{}            Atom, or a set thereof.
%%    - #t_number{}          Any number.
%%       -- #t_float{}       Floating point number.
%%       -- #t_integer{}     Integer.
%%    - #t_list{}            Any list.
%%       -- #t_cons{}        Cons (nonempty list).
%%       -- nil              The empty list.
%%    - #t_tuple{}           Tuple.
%%    - other                Other types.
%%       -- #t_fun{}          Fun.
%%       -- #t_map{}          Map.
%%       -- identifier
%%         -- pid
%%         -- port
%%         -- reference
%%       -- #t_bs_matchable{} Binary-matchable types.
%%         -- #t_bitstring{}    Bitstring.
%%         -- #t_bs_context{}   Match context.
%%
%%  none                     No type (bottom element).
%%
%% We also use #t_union{} to represent conflicting types produced by certain
%% expressions, e.g. the "#t_atom{} or #t_tuple{}" of lists:keyfind/3, which is
%% very useful for preserving type information when we would otherwise have
%% reduced it to 'any'. Since few operations can make direct use of this extra
%% type information, types should generally be normalized to one of the above
%% before use.
%%
%% When adding a new type it's important that the lattice stays consistent [1].
%% In brief, the following properties must hold:
%%
%% * All types must be unambiguous; any given value must narrow down to a
%%   single type, and multiple supertypes are not allowed.
%%
%% * `meet` is used when we know more about a value (e.g. type tests), so it
%%   must not return a more general type than either of its arguments. In other
%%   words, we're only allowed to *add* knowledge in a `meet`.
%%
%% * `join` is used when we know less about a value (e.g. phi node), so it
%%   must not return a more specific type than either of its arguments. In
%%   other words we're only allowed to *remove* knowledge in a `join`.
%%
%% * Both `join` and `meet` must be commutative, associative, and idempotent.
%%
%% Maintaining the above may seem trivial but subtle errors can creep in when
%% adding fields or restrictions to a type. ?TUPLE_ELEMENT_LIMIT is a great
%% example of this.
%%
%% The property test suite ensures that the above holds, so don't forget to
%% add your new types there. You should also consider increasing ?REPETITIONS
%% during development to ensure it hits all nooks and crannies.
%%
%% [1] https://en.wikipedia.org/wiki/Lattice_(order)#General_lattice

-define(ATOM_SET_SIZE, 5).

%% Documented limits
-define(MAX_FUNC_ARGS, 255).
-define(MAX_TUPLE_SIZE, (1 bsl 24) - 1).

-type float_range() :: 'any' | {'-inf',float()} | {float(),'+inf'}.

-record(t_atom, {elements=any :: 'any' | ordsets:ordset(atom())}).
-record(t_bitstring, {size_unit=1 :: pos_integer(),
                      %% The appendable flag indicates whether the bitstring
                      %% originated as <<>> and has only been appended to by
                      %% `bs_create_bin` with the bitstring as the leftmost
                      %% fragment.
                      appendable=false :: boolean()}).
-record(t_bs_context, {tail_unit=1 :: pos_integer()}).
-record(t_bs_matchable, {tail_unit=1 :: pos_integer()}).
-record(t_float, {elements=any :: float_range()}).
-record(t_fun, {arity=any :: arity() | 'any',
                target=any :: {atom(), non_neg_integer()} | 'any',
                type=any :: type() }).
-record(t_integer, {elements=any :: 'any' | beam_bounds:range()}).
-record(t_number, {elements=any :: 'any' | beam_bounds:range()}).

%% `super_key` and `super_value` are the join of all key and value types.
%%
%% Note that we don't track specific elements as we have no obvious way to
%% limit them. See ?TUPLE_ELEMENT_LIMIT for details.
-record(t_map, {super_key=any :: type(),
                super_value=any :: type()}).

%% `type` is the join of all list elements, and `terminator` is the tail of the
%% last cons cell ('nil' for proper lists).
%%
%% Note that `type` may not be updated unless the entire list is known, and
%% that the terminator being known is not a guarantee that the rest of the list
%% is.
-record(t_cons, {type=any :: type(), terminator=any :: type()}).
-record(t_list, {type=any :: type(), terminator=any :: type()}).

-record(t_tuple, {size=0 :: integer(),
                  exact=false :: boolean(),
                  elements=#{} :: tuple_elements()}).

%% Known element types, where the key is a 1-based integer index. Unknown
%% elements are assumed to be 'any', and indexes above ?TUPLE_ELEMENT_LIMIT are
%% ignored for performance reasons.
%%
%% Cutting off all indexes above a certain limit may seem strange, but is
%% required to ensure that a meet of two types always returns a type that's at
%% least as specific as either type. Consider the following types:
%%
%%    A = #t_tuple{elements=#{ ... elements 1 .. 6 ... }}
%%    B = #t_tuple{elements=#{ ... elements 7 .. 13 ... }}
%%
%% If we'd collapse types once a tuple has more than 12 elements, meet(A, B)
%% would suddenly be less specific than either A or B. Ignoring all elements
%% above a certain index avoids this problem, at the small price of losing type
%% information in huge tuples.

-define(TUPLE_ELEMENT_LIMIT, 12).
-type tuple_elements() :: #{ Key :: pos_integer() => type() }.

-type normal_type() :: 'any' | 'none' |
                       #t_number{} | #t_float{} | #t_integer{} |
                       #t_atom{} |
                       #t_bitstring{} | #t_bs_context{} | #t_bs_matchable{} |
                       #t_fun{} |
                       #t_list{} | #t_cons{} | 'nil' |
                       'other' |
                       #t_map{} |
                       'identifier' |
                       'pid' |
                       'port' |
                       'reference' |
                       #t_tuple{}.

-type other_type() :: 'none' | #t_fun{} | #t_map{} |
                      'pid' | 'port' | 'reference' | 'identifier' |
                      #t_bitstring{} | #t_bs_context{} |
                      #t_bs_matchable{}.

-type record_key() :: {Arity :: integer(), Tag :: normal_type() }.
-type record_set() :: ordsets:ordset({record_key(), #t_tuple{}}).
-type tuple_set() :: #t_tuple{} | record_set().

%% The fields in the union must not overlap. In particular, that means
%% that the type `any` is not allowed in any field.
-record(t_union, {atom=none :: 'none' | #t_atom{},
                  list=none :: 'none' | #t_list{} | #t_cons{} | nil,
                  number=none :: 'none' | #t_number{} | #t_float{} | #t_integer{},
                  tuple_set=none :: 'none' | tuple_set(),
                  other=none :: 'other' | other_type()}).

-type type() :: #t_union{} | normal_type().

-ifdef(BEAM_TYPES_INTERNAL).
%% Internal constants used by beam_types.erl and its whitebox tests
-define(TUPLE_SET_LIMIT, 12).
-define(MAX_TYPE_DEPTH, 4).
-endif.
