%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2011. All Rights Reserved.
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
-module(debugger_SUITE).

%% Test break points.

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 app_test/1,appup_test/1,erts_debug/1,encrypted_debug_info/1,
	 no_abstract_code/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

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
    Dog=test_server:timetrap(?t:minutes(0.5)),
    [{watchdog, Dog}|Config].
end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

app_test(Config) when is_list(Config) ->
    ?line ?t:app_test(debugger),
    ok.

appup_test(Config) when is_list(Config) ->
    ok = ?t:appup_test(debugger).

erts_debug(Config) when is_list(Config) ->
    c:l(erts_debug),
    ok.

no_abstract_code(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Simple = filename:join(PrivDir, "simple"),
    ?line Source = Simple ++ ".erl",
    ?line BeamFile = Simple ++ ".beam",
    ?line simple_file(Source),

    %% Compile module without abstract code.
    CompileFlags = [{outdir,PrivDir}],
    ?line {ok,_} = compile:file(Source, CompileFlags),
    ?line error = int:i(Simple),

    %% Cleanup.
    ?line ok = file:delete(Source),
    ?line ok = file:delete(BeamFile),

    ok.

encrypted_debug_info(Config) when is_list(Config) ->
    try	begin crypto:start(), crypto:info(), crypto:stop(), ok end of
	ok ->
	    encrypted_debug_info_1(Config)
    catch
	error:_ ->
	    {skip,"The crypto application is missing or broken"}
    end.

encrypted_debug_info_1(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Simple = filename:join(PrivDir, "simple"),
    ?line Source = Simple ++ ".erl",
    ?line BeamFile = Simple ++ ".beam",
    ?line simple_file(Source),

    %% Compile module.
    Key = "_This a Crypto Key_",
    CompileFlags = [{outdir,PrivDir},debug_info,{debug_info_key,Key}],
    ?line {ok,_} = compile:file(Source, CompileFlags),

    %% Interpret module
    ?line ok = beam_lib:crypto_key_fun(simple_crypto_fun(Key)),
    ?line {module,simple} = int:i(Simple),

    %% Remove key.
    ?line {ok,_} = beam_lib:clear_crypto_key_fun(),
    ?line error = int:i(Simple),

    %% Cleanup.
    ?line ok = file:delete(Source),
    ?line ok = file:delete(BeamFile),

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
