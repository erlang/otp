%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
-module(make_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include_lib("kernel/include/file.hrl").

%% in ./make_SUITE_data there are test-files used by this
%% test suite. There are 4 files named test1.erl ... test5.erl.
%% The test files are attacked in various ways in order to put make on trial.
%% 
%% Also, and Emakefile exists in ./make_SUITE_data. This file specifies
%% that the file :"test5.erl" shall be compiled with the 'S' option,
%% i.e. produce "test5.S" instead of "test5.<objext>"

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [make_all, make_files, load, netload, recompile_on_changed_include,
     emake_opts, {group, otp_6057}].

groups() -> 
    [{otp_6057,[],[otp_6057_a, otp_6057_b,
                   otp_6057_c]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    otp_6057_init(Config).

end_per_group(_GroupName, Config) ->
    otp_6057_end(Config).

init_per_testcase(_,Config) ->
    Config.

end_per_testcase(netload,_Config) ->
    %% Stop slave - in case of failure
    Nodes = nodes(),
    case [N || N <- Nodes,
               "make_SUITE_netload" == hd(string:lexemes(atom_to_list(N),"@"))] of
        [Node] ->
            ct_slave:stop(Node);
        _ ->
            ok
    end;
end_per_testcase(_,_Config) ->
    ok.

test_files() -> ["test1", "test2", "test3", "test4"].

make_all(Config) when is_list(Config) ->
    Current = prepare_data_dir(Config),
    up_to_date = make:all(),
    ok = ensure_exists(test_files()),
    ok = ensure_exists(["test5"],".S"), % Emakefile: [{test5,['S']}
    file:set_cwd(Current),
    ensure_no_messages(),
    ok.

make_files(Config) when is_list(Config) ->
    Current = prepare_data_dir(Config),

    %% Make files that exist.

    Files = [test1, test2],
    up_to_date = make:files(Files), % ok files
    ok = ensure_exists(Files),

    error = make:files([test1,test7]), % non existing file
    up_to_date = make:files([test1,test2],[debug_info]), % with option

    file:set_cwd(Current),
    ensure_no_messages(),
    ok.

load(Config) ->
    Current = prepare_data_dir(Config),
    code:purge(test1),
    code:delete(test1),
    false = code:is_loaded(test1),
    up_to_date = make:files([test1], [load]),
    {file,_} = code:is_loaded(test1),
    file:set_cwd(Current),
    ensure_no_messages(),
    ok.

netload(Config) ->
    Current = prepare_data_dir(Config),
    code:purge(test1),
    code:delete(test1),
    false = code:is_loaded(test1),
    {ok,Node} = ct_slave:start(make_SUITE_netload),
    up_to_date = make:files([test1], [netload]),
    timer:sleep(1000), % async, so give some time
    {file,F} = code:is_loaded(test1),
    {file,F} = rpc:call(Node,code,is_loaded,[test1]),
    ct_slave:stop(Node),
    file:set_cwd(Current),
    ensure_no_messages(),
    ok.

recompile_on_changed_include(Config) ->
    Current = prepare_data_dir(Config),

    Files = [test_incl1,"incl_src/test_incl2"],
    up_to_date = make:files(Files),
    ok = ensure_exists([test_incl1,test_incl2]),

    {ok, FileInfo11} = file:read_file_info("test_incl1.beam"),
    Date11 = FileInfo11#file_info.mtime,
    {ok, FileInfo21} = file:read_file_info("test_incl2.beam"),
    Date21 = FileInfo21#file_info.mtime,
    timer:sleep(2000),

    %% Touch the include file
    {ok,Bin} = file:read_file("test_incl.hrl"),
    ok = file:delete("test_incl.hrl"),
    ok = file:write_file("test_incl.hrl",Bin),

    up_to_date = make:files(Files),

    {ok, FileInfo12} = file:read_file_info("test_incl1.beam"),
    case FileInfo12#file_info.mtime of
        Date11 -> ct:fail({"file not recompiled", "test_incl1.beam"});
        _Date12 -> ok
    end,
    {ok, FileInfo22} = file:read_file_info("test_incl2.beam"),
    case FileInfo22#file_info.mtime of
        Date21 -> ct:fail({"file not recompiled", "test_incl2.beam"});
        _Date22 -> ok
    end,

    file:set_cwd(Current),
    ensure_no_messages(),
    ok.

emake_opts(Config) when is_list(Config) ->
    Current = prepare_data_dir(Config),

    %% prove that emake is used in opts instead of local Emakefile
    Opts = [{emake, [test8, test9]}],
    error = make:all(Opts),
    error = make:files([test9], Opts),
    "test8.beam" = ensure_exists([test8]),
    "test9.beam" = ensure_exists([test9]),
    "test5.S" = ensure_exists(["test5"],".S"),

    file:set_cwd(Current),
    ensure_no_messages(),
    ok.

%% Moves to the data directory of this suite, clean it from any object
%% files (*.jam for a JAM emulator).  Returns the previous directory.
prepare_data_dir(Config) ->
    {ok, Current} = file:get_cwd(),
    {value, {data_dir, Dir}} = lists:keysearch(data_dir, 1, Config),
    file:set_cwd(Dir),
    {ok, Files} = file:list_dir("."),
    delete_obj(Files, code:objfile_extension()),
    ensure_no_messages(),
    Current.

delete_obj([File|Rest], ObjExt) ->
    case filename:extension(File) of
        ObjExt -> file:delete(File);
        ".S" -> file:delete(File);
        _ -> ok
    end,
    delete_obj(Rest, ObjExt);
delete_obj([], _) ->
    ok.



%% Ensure that the given object files exists.
ensure_exists(Names) ->
    ensure_exists(Names, code:objfile_extension()).

ensure_exists([Name|Rest], ObjExt) when is_atom(Name) ->
    ensure_exists([atom_to_list(Name)|Rest], ObjExt);
ensure_exists([Name|Rest], ObjExt) ->
    case filelib:is_regular(Name++ObjExt) of
        true ->
            ensure_exists(Rest, ObjExt);
        false ->
            Name++ObjExt
    end;
ensure_exists([], _) ->
    ok.

otp_6057_init(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),

    %% Create the directories PrivDir/otp_6057/src1, /src2 and /ebin
    Src1 = filename:join([PrivDir, otp_6057, src1]),
    Src2 = filename:join([PrivDir, otp_6057, src2]),
    Ebin = filename:join([PrivDir, otp_6057, ebin]),
    ok = file:make_dir(filename:join(PrivDir, otp_6057)),
    ok = file:make_dir(Src1),
    ok = file:make_dir(Src2),
    ok = file:make_dir(Ebin),

    %% Copy test1.erl and test2.erl to src1, and test3.erl to src2
    Test1orig = filename:join(DataDir, "test1.erl"),
    Test2orig = filename:join(DataDir, "test2.erl"),
    Test3orig = filename:join(DataDir, "test3.erl"),
    Test1 = filename:join(Src1, "test1.erl"),
    Test2 = filename:join(Src1, "test2.erl"),
    Test3 = filename:join(Src2, "test3.erl"),
    {ok, _} = file:copy(Test1orig, Test1),
    {ok, _} = file:copy(Test2orig, Test2),
    {ok, _} = file:copy(Test3orig, Test3),

    %% Create an Emakefile in src1
    Emakefile = filename:join(Src1, "Emakefile"),
    {ok, Fd} = file:open(Emakefile, write),
    ok = io:write(Fd, {["test1.erl","test2","../src2/test3"],
                       [{outdir,"../ebin"}]}),
    ok = io:fwrite(Fd, ".~n", []),
    ok = file:close(Fd),

    ensure_no_messages(),
    Config.

%% Test that make:all/0, suite/0 looks for object file in correct place
otp_6057_a(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),

    %% Go to src1, saving old CWD
    {ok, CWD} = file:get_cwd(),
    Src1 = filename:join([PrivDir, otp_6057, src1]),
    ok = file:set_cwd(Src1),

    %% Call make:all()
    up_to_date = make:all(),

    %% Ensure that all beam files are created in the ebin directory
    Ebin = filename:join([PrivDir, otp_6057, ebin]),
    Test1 = filename:join(Ebin, test1),
    Test2 = filename:join(Ebin, test2),
    Test3 = filename:join(Ebin, test3),
    case ensure_exists([Test1, Test2, Test3]) of
        ok -> ok;
        Missing ->
            ct:fail({"missing beam file", Missing})
    end,

    %% Check creation date of test1.beam and make sure it is not
    %% recompiled if make:all() is called again.
    %% (Sleep a while, if the file is recompiled within a second then
    %%  mtime will be the same).
    {ok, FileInfo1} = file:read_file_info(Test1++".beam"),
    Date1 = FileInfo1#file_info.mtime,
    timer:sleep(2000),
    up_to_date = make:all(),
    {ok, FileInfo2} = file:read_file_info(Test1++".beam"),
    case FileInfo2#file_info.mtime of
        Date1 -> ok;
        _Date2 ->
            ct:fail({"recompiled beam file", Test1++".beam"})
    end,

    %% Remove the beam files
    ok =
    ensure_removed([Test1++".beam",Test2++".beam",Test2++".beam"]),

    %% Return to original CWD
    ok = file:set_cwd(CWD),

    ensure_no_messages(),
    ok.

%% Test that make:files/1 can handle a file in another directory
otp_6057_b(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),

    %% Go to src1, saving old CWD
    {ok, CWD} = file:get_cwd(),
    Src1 = filename:join([PrivDir, otp_6057, src1]),
    ok = file:set_cwd(Src1),

    %% Ensure there is no beam file already
    Ebin = filename:join([PrivDir, otp_6057, ebin]),
    Test3 = filename:join(Ebin, "test3"),
    ok = ensure_removed([Test3++".beam"]),

    %% Call make:files/1
    up_to_date = make:files(["../src2/test3"]),

    %% Ensure that the beam file is created in the ebin directory
    case ensure_exists([Test3]) of
        ok -> ok;
        Missing ->
            ct:fail({"missing beam file", Missing})
    end,

    %% Remove the beam file
    ok = ensure_removed([Test3++".beam"]),

    %% Return to original CWD
    ok = file:set_cwd(CWD),

    ensure_no_messages(),
    ok.

%% Test that make:files/1 find options in Emakefile if a file is
%% given with the .erl extension there
otp_6057_c(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),

    %% Go to src1, saving old CWD
    {ok, CWD} = file:get_cwd(),
    Src1 = filename:join([PrivDir, otp_6057, src1]),
    ok = file:set_cwd(Src1),

    %% Ensure there are no beam files already
    Ebin = filename:join([PrivDir, otp_6057, ebin]),
    Test1 = filename:join(Ebin, "test1"),
    Test2 = filename:join(Ebin, "test2"),
    ok = ensure_removed([Test1++".beam",Test2++".beam"]),

    %% Call make:files/1
    up_to_date = make:files([test1, test2]),

    %% Ensure that the beam files are created in the ebin directory
    Ebin = filename:join([PrivDir, otp_6057, ebin]),
    case ensure_exists([Test1, Test2]) of
        ok -> ok;
        Missing ->
            ct:fail({"missing beam file", Missing})
    end,

    %% Remove the beam files
    ok = ensure_removed([Test1++".beam", Test2++".beam"]),

    %% Return to original CWD
    ok = file:set_cwd(CWD),

    ensure_no_messages(),
    ok.

otp_6057_end(Config) when is_list(Config) ->
    Config.

ensure_removed([File|Files]) ->
    file:delete(File),
    ensure_removed(Files);
ensure_removed([]) ->
    ok.

ensure_no_messages() ->
    ensure_no_messages(0).

ensure_no_messages(N) ->
    receive
        Any ->
            io:format("Unexpected message: ~p", [Any]),
            ensure_no_messages(N+1)
    after 0 ->
              case N of
                  0 -> ok;
                  N -> ct:fail(failed)
              end
    end.
