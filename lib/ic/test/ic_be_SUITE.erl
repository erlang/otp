%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
%%
%%%----------------------------------------------------------------------
%%% Purpose : Test suite for the backends of the IDL compiler
%%%----------------------------------------------------------------------

-module(ic_be_SUITE).
-include_lib("common_test/include/ct.hrl").


-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,plain/1]).     


-define(OUT(X), filename:join([proplists:get_value(priv_dir, Config), gen, to_list(X)])).


%% Top of cases

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [plain].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% Checking code for the plain backend.
plain(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, plain),
    ok = ic:gen(File,stdopts(OutDir)++[{be,erl_plain}]),
    ok.

%%--------------------------------------------------------------------
%%
%% Utilities
stdopts(OutDir) ->
    [{outdir, OutDir}, {maxerrs, infinity}].

to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) -> X.

