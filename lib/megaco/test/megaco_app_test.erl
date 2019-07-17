%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2019. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Verify the application specifics of the Megaco application
%%----------------------------------------------------------------------

-module(megaco_app_test).

-export([
         all/0,

         app/0,   app/1,
         appup/0, appup/1
        ]).

-include_lib("common_test/include/ct.hrl").


%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

all() -> 
    [
     app, 
     appup
    ].


%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

app() ->
    [{doc, "Test that the megaco app file is ok"}].
app(Config) when is_list(Config) ->
    ok = test_server:app_test(megaco).


%%--------------------------------------------------------------------

appup() ->
    [{doc, "Test that the megaco appup file is ok"}].
appup(Config) when is_list(Config) ->
    ok = test_server:appup_test(megaco).
