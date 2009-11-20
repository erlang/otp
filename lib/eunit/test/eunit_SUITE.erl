%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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
-module(eunit_SUITE).

-export([all/1,eunit_test/1]).
	 
-include("test_server.hrl").

all(suite) ->
    [eunit_test].

eunit_test(Config) when is_list(Config) ->
    ok = file:set_cwd(code:lib_dir(eunit)),
    ok = eunit:test(eunit).

