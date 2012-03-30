%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2012. All Rights Reserved.
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
                   is_closure    :: boolean(),
                   closure_arity :: arity(),
                   is_leaf       :: boolean(),
                   params,     % :: list()
                   info = []}).  %% this field seems not needed; take out??

%%
%% Data is a triple with a dict of constants, a list of labels and an integer
%%
-type cfg_data() :: {dict(), [cfg_lbl()], non_neg_integer()}.

%%
%% The following is to be used by other modules
%%
-record(cfg, {table = gb_trees:empty() :: gb_tree(),
              info                     :: #cfg_info{},
              data                     :: cfg_data()}).
-type cfg() :: #cfg{}.
