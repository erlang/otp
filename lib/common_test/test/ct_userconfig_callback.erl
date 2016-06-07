%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
