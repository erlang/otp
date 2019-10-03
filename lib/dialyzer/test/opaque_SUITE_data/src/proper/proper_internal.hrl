%%% Copyright 2010-2013 Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>
%%%
%%% This file is part of PropEr.
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%

%%% @copyright 2010-2013 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Manolis Papadakis
%%% @doc Internal header file: This header is included in all PropEr source
%%%      files.

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
-type abs_type() :: term().
-type abs_rec_field() :: term().

-type loose_tuple(T) :: {} | {T} | {T,T} | {T,T,T} | {T,T,T,T} | {T,T,T,T,T}
		      | {T,T,T,T,T,T} | {T,T,T,T,T,T,T} | {T,T,T,T,T,T,T,T}
		      | {T,T,T,T,T,T,T,T,T} | {T,T,T,T,T,T,T,T,T,T} | tuple().
