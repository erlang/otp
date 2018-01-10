%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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


-module(ct_match_state_cth).


-include_lib("common_test/src/ct_util.hrl").
-include_lib("common_test/include/ct_event.hrl").

-compile(export_all).

id(Opts) ->
    empty_cth:id(Opts).

post_groups(Suite, Groups) ->
    empty_cth:post_groups(Suite, Groups).

post_all(Suite, Tests, Groups) ->
    empty_cth:post_all(Suite, Tests, Groups).

init(Id, Opts) ->
    empty_cth:init(Id, Opts),
    {ok,mystate}.

%% In the following, always match against the state value, to ensure
%% that init has indeed been called before the rest of the hooks.
pre_init_per_suite(Suite,Config,mystate) ->
    empty_cth:pre_init_per_suite(Suite,Config,mystate).

post_init_per_suite(Suite,Config,Return,mystate) ->
    empty_cth:post_init_per_suite(Suite,Config,Return,mystate).

pre_end_per_suite(Suite,Config,mystate) ->
    empty_cth:pre_end_per_suite(Suite,Config,mystate).

post_end_per_suite(Suite,Config,Return,mystate) ->
    empty_cth:post_end_per_suite(Suite,Config,Return,mystate).

terminate(mystate) ->
    empty_cth:terminate(mystate).
