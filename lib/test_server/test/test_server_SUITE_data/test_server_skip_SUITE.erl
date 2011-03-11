%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2011. All Rights Reserved.
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
-module(test_server_skip_SUITE).

-export([all/1, init_per_suite/1, end_per_suite/1]).
-export([dummy/1]).

-include_lib("test_server/include/test_server.hrl").
-include_lib("test_server/include/test_server_line.hrl").

all(suite) ->
    [dummy].

init_per_suite(Config) when is_list(Config) ->
    {skip,"Skipping init_per_suite - check that \'dummy\' and"
     " \'end_per_suite\' are also skipped"}.

dummy(suite) -> [];
dummy(doc) -> ["This testcase should never be executed"];
dummy(Config) when is_list(Config) ->
    ?t:fail("This testcase should be executed since"
	    " init_per_suite/1 is skipped").

end_per_suite(doc) -> ["This testcase should never be executed"];
end_per_suite(Config) when is_list(Config) ->
    ?t:fail("end_per_suite/1 should not be executed when"
	    " init_per_suite/1 is skipped").
