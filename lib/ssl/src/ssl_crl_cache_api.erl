%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2015. All Rights Reserved.
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

-module(ssl_crl_cache_api).

-include_lib("public_key/include/public_key.hrl"). 

-type db_handle() :: term(). 

-callback lookup(#'DistributionPoint'{}, db_handle()) -> not_available | [public_key:der_encoded()].
-callback select(term(), db_handle()) ->  [public_key:der_encoded()].
-callback fresh_crl(#'DistributionPoint'{}, public_key:der_encoded()) -> public_key:der_encoded().
