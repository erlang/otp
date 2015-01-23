%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014. All Rights Reserved.
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

-module(standard_error_SUITE).

-export([all/0,suite/0]).
-export([badarg/1,getopts/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [badarg,getopts].

badarg(Config) when is_list(Config) ->
    {'EXIT',{badarg,_}} = (catch io:put_chars(standard_error, [oops])),
    true = erlang:is_process_alive(whereis(standard_error)),
    ok.

getopts(Config) when is_list(Config) ->
    [{encoding,latin1}] = io:getopts(standard_error),
    ok.
