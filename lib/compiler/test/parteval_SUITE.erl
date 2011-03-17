%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2011. All Rights Reserved.
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
-module(parteval_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, pe2/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [pe2].

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


%% (This is more general than needed, since we once compiled the same
%% source code with and without a certain option.)
compile_and_load(Srcname, Outdir, Module, Options) ->
    ?line Objname = filename:join(Outdir, "t1") ++ code:objfile_extension(),
    ?line {ok, Module} =
	compile:file(Srcname,
		     [{d, 'M', Module}, {outdir, Outdir}] ++ Options),
    ?line {ok, B} = file:read_file(Objname),
    ?line {module, Module} = code:load_binary(Module, Objname, B),
    B.

pe2(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Srcname = filename:join(DataDir, "t1.erl"),
    ?line compile_and_load(Srcname, PrivDir, t1, []),

    ?line {Correct, Actual} = t1:run(),
    ?line Correct = Actual,
    ok.
