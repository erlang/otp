%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2013. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Verify the application specifics of the Inets application
%%----------------------------------------------------------------------
-module(inets_appup_test).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").


%% Test server callbacks
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    [appup].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(suite) -> [];
init_per_suite(doc) -> [];
init_per_suite(Config) when is_list(Config) ->
    Config.
    

end_per_suite(suite) -> [];
end_per_suite(doc) -> [];
end_per_suite(Config) when is_list(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

appup() ->
    [{doc, "Perform a simple check of the inets appup file"}].
appup(Config) when is_list(Config) ->
    ok = ?t:appup_test(inets).
