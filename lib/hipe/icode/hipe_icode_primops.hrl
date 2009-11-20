%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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
%%=======================================================================
%% File        : hipe_icode_primops.hrl
%% Author      : Kostis Sagonas
%% Description : Contains definitions for HiPE's primitive operations.
%%=======================================================================
%% $Id$
%%=======================================================================

-record(apply_N, {arity :: arity()}).

-record(closure_element, {n :: arity()}).

-record(element, {typeinfo :: list()}). %% XXX: refine?

-record(gc_test, {need :: non_neg_integer()}).

-record(mkfun, {mfa :: mfa(), magic_num :: integer(), index :: integer()}).

-record(unsafe_element, {index :: non_neg_integer()}).

-record(unsafe_update_element, {index :: non_neg_integer()}).
