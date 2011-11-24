%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
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
-module(make_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, make_all/1, make_files/1]).
-export([otp_6057_init/1,
	 otp_6057_a/1, otp_6057_b/1, otp_6057_c/1,
	 otp_6057_end/1]).

-include_lib("test_server/include/test_server.hrl").

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
    [make_all, make_files, {group, otp_6057}].

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


test_files() -> ["test1", "test2", "test3", "test4"].

make_all(suite) -> [];
make_all(Config) when is_list(Config) ->
    ?line Current = prepare_data_dir(Config),
    ?line up_to_date = make:all(),
    ?line ok = ensure_exists(test_files()),
    ?line ok = ensure_exists(["test5"],".S"), % Emakefile: [{test5,['S']}
    ?line file:set_cwd(Current),
    ?line ensure_no_messages(),
    ok.

make_files(suite) -> [];
make_files(Config) when is_list(Config) ->
    ?line Current = prepare_data_dir(Config),

    %% Make files that exist.

    ?line Files = [test1, test2],
    ?line up_to_date = make:files(Files), % ok files
    ?line ok = ensure_exists(Files),

    ?line error = make:files([test1,test7]), % non existing file
    ?line up_to_date = make:files([test1,test2],[debug_info]), % with option

    ?line file:set_cwd(Current),
    ?line ensure_no_messages(),
    ok.


%% Moves to the data directory of this suite, clean it from any object
%% files (*.jam for a JAM emulator).  Returns the previous directory.
prepare_data_dir(Config) ->
    ?line {ok, Current} = file:get_cwd(),
    ?line {value, {data_dir, Dir}} = lists:keysearch(data_dir, 1, Config),
    ?line file:set_cwd(Dir),
    ?line {ok, Files} = file:list_dir("."),
    ?line delete_obj(Files, code:objfile_extension()),
    ?line ensure_no_messages(),
    Current.

delete_obj([File|Rest], ObjExt) ->
    ?line case filename:extension(File) of
	      ObjExt -> file:delete(File);
	      ".S" -> file:delete(File);
	      _ -> ok
	  end,
    ?line delete_obj(Rest, ObjExt);
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
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),

    %% Create the directories PrivDir/otp_6057/src1, /src2 and /ebin
    Src1 = filename:join([PrivDir, otp_6057, src1]),
    Src2 = filename:join([PrivDir, otp_6057, src2]),
    Ebin = filename:join([PrivDir, otp_6057, ebin]),
    ?line ok = file:make_dir(filename:join(PrivDir, otp_6057)),
    ?line ok = file:make_dir(Src1), 
    ?line ok = file:make_dir(Src2), 
    ?line ok = file:make_dir(Ebin), 

    %% Copy test1.erl and test2.erl to src1, and test3.erl to src2
    Test1orig = filename:join(DataDir, "test1.erl"),
    Test2orig = filename:join(DataDir, "test2.erl"),
    Test3orig = filename:join(DataDir, "test3.erl"),
    Test1 = filename:join(Src1, "test1.erl"),
    Test2 = filename:join(Src1, "test2.erl"),
    Test3 = filename:join(Src2, "test3.erl"),
    ?line {ok, _} = file:copy(Test1orig, Test1),
    ?line {ok, _} = file:copy(Test2orig, Test2),
    ?line {ok, _} = file:copy(Test3orig, Test3),

    %% Create an Emakefile in src1
    Emakefile = filename:join(Src1, "Emakefile"),
    ?line {ok, Fd} = file:open(Emakefile, write),
    ?line ok = io:write(Fd, {["test1.erl","test2","../src2/test3"],
			     [{outdir,"../ebin"}]}),
    ?line ok = io:fwrite(Fd, ".~n", []),
    ?line ok = file:close(Fd),

    ?line ensure_no_messages(),
    Config.

otp_6057_a(suite) ->
    [];
otp_6057_a(doc) ->
    ["Test that make:all/0, suite/0 looks for object file in correct place"];
otp_6057_a(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),

    %% Go to src1, saving old CWD
    ?line {ok, CWD} = file:get_cwd(),
    Src1 = filename:join([PrivDir, otp_6057, src1]),
    ?line ok = file:set_cwd(Src1),

    %% Call make:all()
    ?line up_to_date = make:all(),

    %% Ensure that all beam files are created in the ebin directory
    Ebin = filename:join([PrivDir, otp_6057, ebin]),
    Test1 = filename:join(Ebin, test1),
    Test2 = filename:join(Ebin, test2),
    Test3 = filename:join(Ebin, test3),
    case ensure_exists([Test1, Test2, Test3]) of
	ok -> ok;
	Missing ->
	    ?line ?t:fail({"missing beam file", Missing})
    end,

    %% Check creation date of test1.beam and make sure it is not
    %% recompiled if make:all() is called again.
    %% (Sleep a while, if the file is recompiled within a second then
    %%  mtime will be the same).
    ?line {ok, FileInfo1} = file:read_file_info(Test1++".beam"),
    Date1 = FileInfo1#file_info.mtime,
    ?t:sleep(?t:seconds(2)),
    ?line up_to_date = make:all(),
    ?line {ok, FileInfo2} = file:read_file_info(Test1++".beam"),
    case FileInfo2#file_info.mtime of
	Date1 -> ok;
	_Date2 ->
	    ?line ?t:fail({"recompiled beam file", Test1++".beam"})
    end,

    %% Remove the beam files
    ?line ok =
	ensure_removed([Test1++".beam",Test2++".beam",Test2++".beam"]),

    %% Return to original CWD
    ?line ok = file:set_cwd(CWD),

    ?line ensure_no_messages(),
    ok.

otp_6057_b(suite) ->
    [];
otp_6057_b(doc) ->
    ["Test that make:files/1 can handle a file in another directory"];
otp_6057_b(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),

    %% Go to src1, saving old CWD
    ?line {ok, CWD} = file:get_cwd(),
    Src1 = filename:join([PrivDir, otp_6057, src1]),
    ?line ok = file:set_cwd(Src1),

    %% Ensure there is no beam file already
    Ebin = filename:join([PrivDir, otp_6057, ebin]),
    Test3 = filename:join(Ebin, "test3"),
    ?line ok = ensure_removed([Test3++".beam"]),

    %% Call make:files/1
    ?line up_to_date = make:files(["../src2/test3"]),
    
    %% Ensure that the beam file is created in the ebin directory
    case ensure_exists([Test3]) of
	ok -> ok;
	Missing ->
	    ?line ?t:fail({"missing beam file", Missing})
    end,

    %% Remove the beam file
    ?line ok = ensure_removed([Test3++".beam"]),

    %% Return to original CWD
    ?line ok = file:set_cwd(CWD),

    ?line ensure_no_messages(),
    ok.

otp_6057_c(suite) ->
    [];
otp_6057_c(doc) ->
    ["Test that make:files/1 find options in Emakefile if a file is "
     "given with the .erl extension there"];
otp_6057_c(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),

    %% Go to src1, saving old CWD
    ?line {ok, CWD} = file:get_cwd(),
    Src1 = filename:join([PrivDir, otp_6057, src1]),
    ?line ok = file:set_cwd(Src1),

    %% Ensure there are no beam files already
    Ebin = filename:join([PrivDir, otp_6057, ebin]),
    Test1 = filename:join(Ebin, "test1"),
    Test2 = filename:join(Ebin, "test2"),
    ?line ok = ensure_removed([Test1++".beam",Test2++".beam"]),

    %% Call make:files/1
    ?line up_to_date = make:files([test1, test2]),
    
    %% Ensure that the beam files are created in the ebin directory
    Ebin = filename:join([PrivDir, otp_6057, ebin]),
    case ensure_exists([Test1, Test2]) of
	ok -> ok;
	Missing ->
	    ?line ?t:fail({"missing beam file", Missing})
    end,

    %% Remove the beam files
    ?line ok = ensure_removed([Test1++".beam", Test2++".beam"]),

    %% Return to original CWD
    ?line ok = file:set_cwd(CWD),

    ?line ensure_no_messages(),
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
		N -> ?t:fail()
	    end
    end.
	    
