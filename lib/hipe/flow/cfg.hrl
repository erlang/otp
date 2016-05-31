%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
%%============================================================================
%% File    : cfg.hrl
%% Author  : Kostis Sagonas <kostis@it.uu.se>
%% Purpose : Contains typed record declarations for the CFG data structures
%%
%% $Id$
%%============================================================================

-type cfg_lbl() :: non_neg_integer().

%%
%% This is supposed to be local but appears here for the time being
%% just so that it is used below
%%
-record(cfg_info, {'fun'         :: mfa(),
                   start_label   :: cfg_lbl(),
                   %% TODO: merge is_closure and closure_arity into one field
                   is_closure    :: boolean(),
                   closure_arity = none :: 'none' | arity(),
                   is_leaf       :: boolean(),
                   params        :: list(),    %% XXX: refine
                   info = []     :: list()}).  %% seems not needed; take out??
-type cfg_info() :: #cfg_info{}.

%%
%% Data is a triple with a dict of constants, a list of labels and an integer
%%
-type cfg_data() :: {dict:dict(), [cfg_lbl()], non_neg_integer()}.

%%
%% The following is to be used by other modules
%%
-record(cfg, {table = gb_trees:empty() :: gb_trees:tree(),
              info                     :: cfg_info(),
              data                     :: cfg_data()}).
-type cfg() :: #cfg{}.
