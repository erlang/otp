%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
-module(debugger_SUITE).

%% Test break points.

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 app_test/1,appup_test/1,erts_debug/1,encrypted_debug_info/1,
	 no_abstract_code/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [app_test, appup_test, erts_debug, no_abstract_code,
     encrypted_debug_info].

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


init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

app_test(Config) when is_list(Config) ->
    test_server:app_test(debugger),
    ok.

appup_test(Config) when is_list(Config) ->
    ok = test_server:appup_test(debugger).

erts_debug(Config) when is_list(Config) ->
    c:l(erts_debug),
    ok.

no_abstract_code(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Simple = filename:join(PrivDir, "simple"),
    Source = Simple ++ ".erl",
    BeamFile = Simple ++ ".beam",
    simple_file(Source),

    %% Compile module without abstract code.
    CompileFlags = [{outdir,PrivDir}],
    {ok,_} = compile:file(Source, CompileFlags),
    error = int:i(Simple),

    %% Cleanup.
    ok = file:delete(Source),
    ok = file:delete(BeamFile),

    ok.

encrypted_debug_info(Config) when is_list(Config) ->
    try	begin crypto:start(), crypto:stop(), ok end of
	ok ->
	    encrypted_debug_info_1(Config)
    catch
	error:_ ->
	    {skip,"The crypto application is missing or broken"}
    end.

encrypted_debug_info_1(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Simple = filename:join(PrivDir, "simple"),
    Source = Simple ++ ".erl",
    BeamFile = Simple ++ ".beam",
    simple_file(Source),

    %% Compile module.
    Key = "_This a Crypto Key_",
    CompileFlags = [{outdir,PrivDir},debug_info,{debug_info_key,Key}],
    {ok,_} = compile:file(Source, CompileFlags),

    %% Interpret module
    ok = beam_lib:crypto_key_fun(simple_crypto_fun(Key)),
    {module,simple} = int:i(Simple),

    %% Remove key.
    {ok,_} = beam_lib:clear_crypto_key_fun(),
    error = int:i(Simple),

    %% Cleanup.
    ok = file:delete(Source),
    ok = file:delete(BeamFile),

    ok.

simple_crypto_fun(Key) ->
    fun(init) -> ok;
       ({debug_info, des3_cbc, simple, _}) -> Key
    end.


simple_file(File) ->
    simple_file(File, simple).

simple_file(File, Module) ->
    simple_file(File, Module, member).

simple_file(File, Module, F) ->
    B = list_to_binary(["-module(", atom_to_list(Module), "). "
			"-export([t/0]). "
			"t() -> "
			"    t([]). "
			"t(L) -> "
			"    lists:",
			atom_to_list(F), "(a, L). "]),
    ok = file:write_file(File, B).
