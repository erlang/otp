%%% Copyright 2010-2013 Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>
%%%
%%% This file is part of PropEr.
%%%
%%% PropEr is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% PropEr is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

%%% @copyright 2010-2016 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Manolis Papadakis
%%% @doc Internal header file: This header is included in all PropEr source
%%%      files.

-include("compile_flags.hrl").
-include("proper_common.hrl").


%%------------------------------------------------------------------------------
%% Activate strip_types parse transform
%%------------------------------------------------------------------------------

-ifdef(NO_TYPES).
-compile({parse_transform, strip_types}).
-endif.

%%------------------------------------------------------------------------------
%% Random generator selection
%%------------------------------------------------------------------------------

-ifdef(USE_SFMT).
-define(RANDOM_MOD, sfmt).
-define(SEED_NAME, sfmt_seed).
-else.
-define(RANDOM_MOD, random).
-define(SEED_NAME, random_seed).
-endif.

%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------

-define(PROPERTY_PREFIX, "prop_").


%%------------------------------------------------------------------------------
%% Constants
%%------------------------------------------------------------------------------

-define(SEED_RANGE, 4294967296).
-define(MAX_ARITY, 20).
-define(MAX_TRIES_FACTOR, 5).
-define(ANY_SIMPLE_PROB, 3).
-define(ANY_BINARY_PROB, 1).
-define(ANY_EXPAND_PROB, 8).
-define(SMALL_RANGE_THRESHOLD, 16#FFFF).


%%------------------------------------------------------------------------------
%% Common type aliases
%%------------------------------------------------------------------------------

%% TODO: Perhaps these should be moved inside modules.
-type mod_name() :: atom().
-type fun_name() :: atom().
-type size() :: non_neg_integer().
-type length() :: non_neg_integer().
-type position() :: pos_integer().
-type frequency() :: pos_integer().
-type seed() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-type abs_form()   :: erl_parse:abstract_form().
-type abs_expr()   :: erl_parse:abstract_expr().
-type abs_clause() :: erl_parse:abstract_clause().

%% TODO: Replace these with the appropriate types from stdlib.
-ifdef(AT_LEAST_19).
-type abs_type() :: erl_parse:abstract_type().
-type abs_rec_field() :: term().	% erl_parse:af_field_decl().
-else.
-type abs_type() :: term().
-type abs_rec_field() :: term().
-endif.

-type loose_tuple(T) :: {} | {T} | {T,T} | {T,T,T} | {T,T,T,T} | {T,T,T,T,T}
		      | {T,T,T,T,T,T} | {T,T,T,T,T,T,T} | {T,T,T,T,T,T,T,T}
		      | {T,T,T,T,T,T,T,T,T} | {T,T,T,T,T,T,T,T,T,T} | tuple().
