%%--------------------------------------------------------------------
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
-module(ct_userconfig_callback).

-export([check_parameter/1, read_config/1]).

read_config(Str) ->
    KeyVals = string:tokens(Str, " "),
    {ok,read_config1(KeyVals)}.

read_config1([Key,Val | KeyVals]) ->
    [{list_to_atom(Key),Val} | read_config1(KeyVals)];
read_config1([]) ->
    [].

check_parameter(Str) ->
    {ok,{config,Str}}.
