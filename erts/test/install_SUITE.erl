%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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


%%%-------------------------------------------------------------------
%%% File    : install_SUITE.erl
%%% Author  : Rickard Green
%%% Description : 
%%%
%%% Created : 12 Jan 2010 by Rickard Green
%%%-------------------------------------------------------------------
-module(install_SUITE).

-export([all/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([bin_default/1,
	 bin_default_dirty/1,
	 bin_outside_eprfx/1,
	 bin_outside_eprfx_dirty/1,
	 bin_unreasonable_path/1,
	 bin_not_abs/1,
	 'bin white space'/1,
	 bin_no_srcfile/1,
	 bin_unreachable_absolute/1,
	 bin_unreachable_relative/1,
	 bin_same_dir/1,
	 bin_ok_symlink/1,
	 bin_dirname_fail/1,
	 bin_no_use_dirname_fail/1]).

-define(JOIN(A,B,C), filename:join(A, B, C)).

-include_lib("common_test/include/ct.hrl").

-record(inst, {mkdirs = true,
	       symlinks = true,
	       cmd_prefix = "",
	       ln_s = "ln -s",
	       test_prefix = "",
	       destdir = "",
	       extra_prefix = "",
	       exec_prefix = "",
	       bindir = "",
	       erlang_bindir = "",
	       bindir_symlinks = ""}).

need_symlink_cases() -> 
    [bin_unreachable_absolute, bin_unreachable_relative,
     bin_same_dir, bin_ok_symlink, bin_dirname_fail,
     bin_no_use_dirname_fail].

dont_need_symlink_cases() -> 
    [bin_default, bin_default_dirty, bin_outside_eprfx,
     bin_outside_eprfx_dirty, bin_not_abs,
     bin_unreasonable_path, 'bin white space',
     bin_no_srcfile].

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    dont_need_symlink_cases() ++ need_symlink_cases().


%%
%% The test cases
%%

bin_default(Config) when is_list(Config) ->
    E = "/usr/local",
    Bs = "/usr/local/bin",
    Be = Bs,
    EBs = "/usr/local/lib/erlang/bin",
    EBe = EBs,
    RP = "../lib/erlang/bin",
    ChkRes = fun (Res, #inst{test_prefix = TP,
			     destdir = D,
			     extra_prefix = EP,
			     bindir_symlinks = BSL,
			     symlinks = SL}) ->
		     B = join([TP, D, EP, Be]),
		     Expct = case {SL, BSL} of
				 {false, _} when BSL == "relative";
						 BSL == "absolute" ->
				     {error, no_ln_s};
				 {false, _} ->
				     {ok,{absolute,
						B,join([TP,D,EP,EBe])}};
				 {true, "absolute"} ->
				     {ok,{absolute,B,join([TP,EP,EBe])}};
				 {true, _} ->
				     {ok,{relative,B,RP}}
			       end,
		     expect(Expct, Res)
	     end,
    install_bin(Config, #inst{exec_prefix = E,
			      bindir = Bs,
			      erlang_bindir = EBs}, ChkRes).

bin_default_dirty(Config) when is_list(Config) ->
    E = "/usr/./local/lib/..",
    Bs = "/usr/local//lib/../lib/erlang/../../bin",
    Be = "/usr/local/lib/../lib/erlang/../../bin",
    EBs = "/usr/local/lib/../lib/erlang/../erlang/bin/x/y/../..//",
    EBe = "/usr/local/lib/../lib/erlang/../erlang/bin/x/y/../..",
    RP = "../lib/erlang/bin",
    ChkRes = fun (Res, #inst{test_prefix = TP,
			     destdir = D,
			     extra_prefix = EP,
			     bindir_symlinks = BSL,
			     symlinks = SL}) ->
		     B = join([TP, D, EP, Be]),
		     Expct = case {SL, BSL} of
				 {false, _} when BSL == "relative";
						 BSL == "absolute" ->
				     {error, no_ln_s};
				 {false, _} ->
				     {ok,{absolute,
						B,join([TP,D,EP,EBe])}};
				 {true, "absolute"} ->
				     {ok,{absolute,
						B,join([TP,EP,EBe])}};
				 {true, _} ->
				     {ok,{relative,B,RP}}
			       end,
		     expect(Expct, Res)
	     end,
    install_bin(Config, #inst{exec_prefix = E,
			      bindir = Bs,
			      erlang_bindir = EBs}, ChkRes).


bin_outside_eprfx(Config) when is_list(Config) ->
    E = "/usr/local",
    Bs = "/usr/bin",
    Be = Bs,
    EBs = "/usr/local/lib/erlang/bin",
    EBe = EBs,
    RP = "../local/lib/erlang/bin",
    ChkRes = fun (Res, #inst{test_prefix = TP,
			     destdir = D,
			     extra_prefix = EP,
			     bindir_symlinks = BSL,
			     symlinks = SL}) ->
		     B = join([TP, D, EP, Be]),
		     Expct = case {SL, BSL} of
				 {false, _} when BSL == "relative";
						 BSL == "absolute" ->
				     {error, no_ln_s};
				 {false, _} ->
				     {ok,{absolute,
						B,join([TP,D,EP,EBe])}};
				 {true, "relative"} ->
				     {ok,{relative,B,RP}};
				 {true, _} ->
				     {ok,{absolute,B,join([TP,EP,EBe])}}
			       end,
		     expect(Expct, Res)
	     end,
    install_bin(Config, #inst{exec_prefix = E,
			      bindir = Bs,
			      erlang_bindir = EBs}, ChkRes).


bin_outside_eprfx_dirty(Config) when is_list(Config) ->
    E = "/usr/local/lib/..",
    Bs = "/usr/local/lib/../../bin",
    Be = Bs,
    EBs = "/usr/local/lib/erlang/bin",
    EBe = EBs,
    RP = "../local/lib/erlang/bin",
    ChkRes = fun (Res, #inst{test_prefix = TP,
			     destdir = D,
			     extra_prefix = EP,
			     bindir_symlinks = BSL,
			     symlinks = SL}) ->
		     B = join([TP, D, EP, Be]),
		     Expct = case {SL, BSL} of
				 {false, _} when BSL == "relative";
						 BSL == "absolute" ->
				     {error, no_ln_s};
				 {false, _} ->
				     {ok,{absolute,
						B,join([TP,D,EP,EBe])}};
				 {true, "relative"} ->
				     {ok,{relative,B,RP}};
				 {true, _} ->
				     {ok,{absolute,B,join([TP,EP,EBe])}}
			       end,
		     expect(Expct, Res)
	     end,
    install_bin(Config, #inst{exec_prefix = E,
			      bindir = Bs,
			      erlang_bindir = EBs}, ChkRes).

bin_unreasonable_path(Config) when is_list(Config) ->
    E = "/usr/local/../../..",
    Bs = "/usr/local/../../../bin",
    Be = Bs,
    EBs = "/usr/local/../../../bin_unreasonable_path/usr/local/lib/erlang/bin",
    EBe = EBs,
    RP = "../bin_unreasonable_path/usr/local/lib/erlang/bin",
    ChkRes = fun (Res, #inst{test_prefix = TP,
			     destdir = D,
			     extra_prefix = EP,
			     bindir_symlinks = BSL,
			     symlinks = SL}) ->
		     B = join([TP, D, EP, Be]),
		     Expct = case {TP, SL, BSL} of
				 {_, false, _} when BSL == "relative";
						 BSL == "absolute" ->
				     {error, no_ln_s};
				 {_, false, _} ->
				     {ok,{absolute,
						B,join([TP,D,EP,EBe])}};
				 {"", true, "relative"} ->
				     {error, unreasonable_path};
				 {"", true, _} ->
				     {ok,{absolute,B,join([TP,EP,EBe])}};
				 {_, true, "absolute"} ->
				     {ok,{absolute,B,join([TP,EP,EBe])}};
				 _ ->
				     {ok,{relative,B,RP}}
			       end,
		     expect(Expct, Res)
	     end,
    install_bin(Config, #inst{exec_prefix = E,
			      bindir = Bs,
			      erlang_bindir = EBs}, ChkRes).

bin_unreachable_absolute(Config) when is_list(Config) ->
    TDir = proplists:get_value(test_dir, Config),
    make_dirs(TDir, "/opt/local/lib/erlang/usr/bin"),
    make_dirs(TDir, "/opt/local/lib/erlang/bin"),
    Erl = join([TDir, "/opt/local/lib/erlang/bin/erl"]),
    Erlc = join([TDir, "/opt/local/lib/erlang/bin/erlc"]),
    make_dirs(TDir, "/usr/local/lib"),
    make_dirs(TDir, "/usr/local/bin"),
    ok = file:write_file(Erl, "erl"),
    ok = file:write_file(Erlc, "erlc"),
    ok = file:make_symlink("../../../opt/local/lib/erlang/usr",
			   join([TDir, "/usr/local/lib/erlang"])),
    E = "/usr/local",
    Bs = "/usr/local/bin",
    Be = Bs,
    EBs = "/usr/local/lib/erlang/../bin",
    EBe = EBs,
    ChkRes = fun (Res, #inst{test_prefix = TP,
			     destdir = D,
			     extra_prefix = EP,
			     bindir_symlinks = BSL,
			     symlinks = SL}) ->
		     B = join([TP, D, EP, Be]),
		     Expct = case {SL, BSL} of
				 {false, _} when BSL == "relative";
						 BSL == "absolute" ->
				     {error, no_ln_s};
				 {false, _} ->
				     {ok,{absolute,
						B,join([TP,D,EP,EBe])}};
				 {true, "relative"} ->
				     {error, unreachable_absolute};
				 {true, _} ->
				     {ok,{absolute,B,join([TP,EP,EBe])}}
			       end,
		     expect(Expct, Res)
	     end,
    install_bin(Config, #inst{exec_prefix = E,
			      bindir = Bs,
			      erlang_bindir = EBs}, ChkRes).

bin_unreachable_relative(Config) when is_list(Config) ->
    TDir = proplists:get_value(test_dir, Config),
    make_dirs(TDir, "/opt/local/lib/erlang/bin"),
    make_dirs(TDir, "/opt/local/bin"),
    make_dirs(TDir, "/usr/local/lib/erlang/bin"),
    Erl = join([TDir, "/usr/local/lib/erlang/bin/erl"]),
    Erlc = join([TDir, "/usr/local/lib/erlang/bin/erlc"]),
    ok = file:write_file(Erl, "erl"),
    ok = file:write_file(Erlc, "erlc"),
    ok = file:make_symlink("../../opt/local/bin",
			   join([TDir, "/usr/local/bin"])),

    E = "/usr/local",
    Bs = "/usr/local/bin",
    Be = Bs,
    EBs = "/usr/local/lib/erlang/bin",
    EBe = EBs,
    ChkRes = fun (Res, #inst{test_prefix = TP,
			     destdir = D,
			     extra_prefix = EP,
			     bindir_symlinks = BSL,
			     symlinks = SL}) ->
		     B = join([TP, D, EP, Be]),
		     Expct = case {SL, BSL} of
				 {false, _} when BSL == "relative";
						 BSL == "absolute" ->
				     {error, no_ln_s};
				 {false, _} ->
				     {ok,{absolute,
						B,join([TP,D,EP,EBe])}};
				 {true, "relative"} ->
				     {error, unreachable_relative};
				 {true, _} ->
				     {ok,{absolute,B,join([TP,EP,EBe])}}
			       end,
		     expect(Expct, Res)
	     end,
    install_bin(Config, #inst{exec_prefix = E,
			      bindir = Bs,
			      erlang_bindir = EBs}, ChkRes).

bin_ok_symlink(Config) when is_list(Config) ->
    TDir = proplists:get_value(test_dir, Config),
    make_dirs(TDir, "/usr/local/bin"),
    make_dirs(TDir, "/opt/local/lib/erlang/bin"),
    Erl = join([TDir, "/opt/local/lib/erlang/bin/erl"]),
    Erlc = join([TDir, "/opt/local/lib/erlang/bin/erlc"]),
    ok = file:write_file(Erl, "erl"),
    ok = file:write_file(Erlc, "erlc"),
    ok = file:make_symlink("../../opt/local/lib",
			   join([TDir, "/usr/local/lib"])),
    E = "/usr/local",
    Bs = "/usr/local/bin",
    Be = Bs,
    EBs = "/usr/local/lib/erlang/bin",
    EBe = EBs,
    RP = "../lib/erlang/bin",
    ChkRes = fun (Res, #inst{test_prefix = TP,
			     destdir = D,
			     extra_prefix = EP,
			     bindir_symlinks = BSL,
			     symlinks = SL}) ->
		     B = join([TP, D, EP, Be]),
		     Expct = case {SL, BSL} of
				 {false, _} when BSL == "relative";
						 BSL == "absolute" ->
				     {error, no_ln_s};
				 {false, _} ->
				     {ok,{absolute,
						B,join([TP,D,EP,EBe])}};
				 {true, "absolute"} ->
				     {ok,{absolute,B,join([TP,EP,EBe])}};
				 {true, _} ->
				     {ok,{relative,B,RP}}
			       end,
		     expect(Expct, Res)
	     end,
    install_bin(Config, #inst{exec_prefix = E,
			      bindir = Bs,
			      erlang_bindir = EBs}, ChkRes).

bin_same_dir(Config) when is_list(Config) ->
    TDir = proplists:get_value(test_dir, Config),
    make_dirs(TDir, "/usr/local/bin"),
    make_dirs(TDir, "/usr/local/lib"),
    ok = file:make_symlink("..", join([TDir, "/usr/local/lib/erlang"])),
    Erl = join([TDir, "/usr/local/lib/erlang/bin/erl"]),
    Erlc = join([TDir, "/usr/local/lib/erlang/bin/erlc"]),
    ok = file:write_file(Erl, "erl"),
    ok = file:write_file(Erlc, "erlc"),
    ChkRes = fun (Res, _) ->
		     expect({error, target_and_source_same_dir}, Res)
	     end,
    install_bin(Config,
		#inst{mkdirs = false,
		      exec_prefix = "/usr/local",
		      bindir = "/usr/local/bin",
		      erlang_bindir = "/usr/local/lib/erlang/bin"},
		ChkRes).

bin_not_abs(Config) when is_list(Config) ->
    ChkRes = fun (Res, #inst{test_prefix = TP}) ->
		     case TP of
			 "" ->
			     expect({error, {not_abs, 'bindir'}}, Res);
			 _ ->
			     B = join([TP, "/usr/local/bin"]),
			     {ok, {relative, B, "../lib/erlang/bin"}}
		     end
	     end,
    install_bin(Config,
		#inst{exec_prefix = "/usr/local",
		      bindir = "usr/local/bin",
		      erlang_bindir = "/usr/local/lib/erlang/bin"},
		ChkRes).


'bin white space'(Config) when is_list(Config) ->
    E = "/u s r/local",
    Bs = "/u s r/local/b	i	n",
    Be = Bs,
    EBs = "/u s r/local/lib/erl	ang/bin",
    EBe = EBs,
    RP = "../lib/erl	ang/bin",
    ChkRes = fun (Res, #inst{test_prefix = TP,
			     destdir = D,
			     extra_prefix = EP,
			     bindir_symlinks = BSL,
			     symlinks = SL}) ->
		     B = join([TP, D, EP, Be]),
		     Expct = case {SL, BSL} of
				 {false, _} when BSL == "relative";
						 BSL == "absolute" ->
				     {error, no_ln_s};
				 {false, _} ->
				     {ok,{absolute,
						B,join([TP,D,EP,EBe])}};
				 {true, "absolute"} ->
				     {ok,{absolute,B,join([TP,EP,EBe])}};
				 {true, _} ->
				     {ok,{relative,B,RP}}
			       end,
		     expect(Expct, Res)
	     end,
    install_bin(Config, #inst{exec_prefix = E,
			      bindir = Bs,
			      erlang_bindir = EBs}, ChkRes).

bin_dirname_fail(Config) when is_list(Config) ->
    E = "/opt",
    Bs = "/opt/lib/../bin",
    Be = Bs,
    EBs = "/opt/lib/erlang/otp/bin",
    EBe = EBs,
    CMDPRFX = "PATH=\""++proplists:get_value(data_dir,Config)++":"++os:getenv("PATH")++"\"",
    ChkRes = fun (Res, #inst{test_prefix = TP,
			     destdir = D,
			     extra_prefix = EP,
			     bindir_symlinks = BSL,
			     symlinks = SL}) ->
		     B = join([TP, D, EP, Be]),
		     Expct = case {SL, BSL} of
				 {false, _} when BSL == "relative";
						 BSL == "absolute" ->
				     {error, no_ln_s};
				 {false, _} ->
				     {ok,{absolute,
						B,join([TP,D,EP,EBe])}};
				 {true, "relative"} ->
				     {error, dirname_failed};
				 {true, _} ->
				     {ok,{absolute,B,join([TP,EP,EBe])}}
			       end,
		     expect(Expct, Res)
	     end,
    install_bin(Config, #inst{cmd_prefix = CMDPRFX,
			      exec_prefix = E,
			      bindir = Bs,
			      erlang_bindir = EBs}, ChkRes).

bin_no_use_dirname_fail(Config) when is_list(Config) ->
    E = "/opt",
    Bs = "/opt/bin",
    Be = Bs,
    EBs = "/opt/lib/erlang/otp/bin",
    EBe = EBs,
    RP = "../lib/erlang/otp/bin",
    CMDPRFX = "PATH=\""++proplists:get_value(data_dir,Config)++":"++os:getenv("PATH")++"\"",
    ChkRes = fun (Res, #inst{test_prefix = TP,
			     destdir = D,
			     extra_prefix = EP,
			     bindir_symlinks = BSL,
			     symlinks = SL}) ->
		     B = join([TP, D, EP, Be]),
		     Expct = case {SL, BSL} of
				 {false, _} when BSL == "relative";
						 BSL == "absolute" ->
				     {error, no_ln_s};
				 {false, _} ->
				     {ok,{absolute,
						B,join([TP,D,EP,EBe])}};
				 {true, "absolute"} ->
				     {ok,{absolute,B,join([TP,EP,EBe])}};
				 {true, _} ->
				     {ok,{relative,B,RP}}
			       end,
		     expect(Expct, Res)
	     end,
    install_bin(Config, #inst{cmd_prefix = CMDPRFX,
			      exec_prefix = E,
			      bindir = Bs,
			      erlang_bindir = EBs}, ChkRes).

bin_no_srcfile(Config) when is_list(Config) ->
    TDir = proplists:get_value(test_dir, Config),
    make_dirs(TDir, "/opt/local/bin"),
    make_dirs(TDir, "/opt/local/lib/erlang/bin"),
    Erl = join([TDir, "/opt/local/lib/erlang/bin/erl"]),
    ok = file:write_file(Erl, "erl"),
    Erlc = join([TDir, "/opt/local/lib/erlang/bin/erlc"]),
    RP_Erlc = "../lib/erlang/bin/erlc",
    ChkRes = fun (Res,  #inst{bindir_symlinks = BSL,
			      symlinks = SL}) ->
		     Expct = case {SL, BSL} of
				 {false, _} when BSL == "relative";
						 BSL == "absolute" ->
				     {error, no_ln_s};
				 {false,_} ->
				     {error,{no_srcfile, Erlc}};
				 {true, "absolute"} ->
				     {error,{no_srcfile, Erlc}};
				 {true, _} ->
				     {error,{no_srcfile, RP_Erlc}}
			     end,
		     expect(Expct, Res)
	     end,
    install_bin(Config,
		#inst{mkdirs = false,
		      exec_prefix = "/opt/local",
		      bindir = "/opt/local/bin",
		      erlang_bindir = "/opt/local/lib/erlang/bin"},
		ChkRes).

%%
%%
%% Auxiliary functions
%%
%%

expect(X, X) ->
    io:format("result: ~p~n", [X]),
    io:format("-----------------------------------------------~n", []),
    ok;
expect(X, Y) ->
    io:format("expected: ~p~n", [X]),
    io:format("got     : ~p~n", [Y]),
    io:format("-----------------------------------------------~n", []),
    ct:fail({X,Y}).
    
init_per_suite(Config) ->
    PD = proplists:get_value(priv_dir, Config),
    SymLinks = case os:type() of
                   {win32, _} -> false;
                   _ ->
                       case file:make_symlink("nothing",
                                              filename:join(PD, "symlink_test")) of
                           ok -> true;
                           _ -> false
                       end
               end,
    [{symlinks, SymLinks} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(Case, Config) ->
    init_per_testcase_aux(proplists:get_value(symlinks,Config),
                          os:type(),Case,Config).

init_per_testcase_aux(_, {win32, _}, _Case, _Config) ->
    {skip, "Not on windows"};
init_per_testcase_aux(false, OsType, Case, Config) ->
    case lists:member(Case, need_symlink_cases()) of
	false -> init_per_testcase_aux(true, OsType, Case, Config);
	true -> {skip, "Cannot create symbolic links"}
    end;
init_per_testcase_aux(true, _OsType, Case, Config) ->
    [{testcase, Case},
     {test_dir, make_dirs(proplists:get_value(priv_dir, Config), atom_to_list(Case))}
     | Config].

end_per_testcase(_Case, _Config) ->
    ok.

make_dirs(Root, Suffix) ->
    do_make_dirs(Root, string:tokens(Suffix, [$/])).

do_make_dirs(_Root, []) ->
    "";
do_make_dirs(Root, [D|Ds]) ->
    Dir = filename:join(Root, D),
    case file:make_dir(Dir) of
	{error, eexist} -> ok;
	ok -> ok;
	Err -> exit({make_dir, Err})
    end,
    filename:join(Dir, do_make_dirs(Dir, Ds)).

install_bin(Config, #inst{mkdirs = MkDirs,
			  exec_prefix = EXEC_PREFIX,
			  bindir = BINDIR,
			  erlang_bindir = ERLANG_BINDIR} = Inst, ChkRes) ->
    PDir = proplists:get_value(priv_dir, Config),
    TDir = proplists:get_value(test_dir, Config),
    TD = atom_to_list(proplists:get_value(testcase, Config)),
    case MkDirs of
	false -> ok;
	true ->
	    make_dirs(TDir, EXEC_PREFIX),
	    make_dirs(TDir, BINDIR),
	    make_dirs(TDir, ERLANG_BINDIR),
	    Erl = join([TDir, ERLANG_BINDIR, "/erl"]),
	    Erlc = join([TDir, ERLANG_BINDIR, "/erlc"]),
	    ok = file:write_file(Erl, "erl"),
	    ok = file:write_file(Erlc, "erlc")
    end,

    install_bin2(Config, Inst#inst{destdir = TDir}, ChkRes),
    install_bin2(Config, Inst#inst{extra_prefix = TDir}, ChkRes),
    install_bin2(Config, Inst#inst{destdir = PDir,
				   extra_prefix = "/"++TD}, ChkRes),
    install_bin2(Config,
		 Inst#inst{test_prefix = TDir,
			   exec_prefix = join([TDir, EXEC_PREFIX]),
			   bindir = join([TDir, BINDIR]),
			   erlang_bindir = join([TDir, ERLANG_BINDIR])},
		 ChkRes),
    case proplists:get_value(symlinks, Config) of
	true -> ok;
	false -> {comment, "No symlink tests run, since symlinks not working"}
    end.
				   
    
install_bin2(Config, Inst, ChkRes) ->
    install_bin3(Config, Inst#inst{symlinks = false,
				   ln_s = "ln"}, ChkRes),
    install_bin3(Config, Inst#inst{symlinks = false,
				   ln_s = "ln",
				   bindir_symlinks = "relative"}, ChkRes),
    install_bin3(Config, Inst#inst{symlinks = false,
				   ln_s = "ln",
				   bindir_symlinks = "absolute"}, ChkRes),
    install_bin3(Config, Inst#inst{symlinks = false,
				   ln_s = "cp -p"}, ChkRes),
    install_bin3(Config, Inst#inst{symlinks = false,
				   ln_s = "cp -p",
				   bindir_symlinks = "relative"}, ChkRes),
    install_bin3(Config, Inst#inst{symlinks = false,
				   ln_s = "cp -p",
				   bindir_symlinks = "absolute"}, ChkRes),
    case proplists:get_value(symlinks, Config) of
	true ->
	    install_bin3(Config, Inst#inst{symlinks = true,
					   ln_s = "ln -s"}, ChkRes),
	    install_bin3(Config, Inst#inst{symlinks = true,
					   ln_s = "ln -s",
					   bindir_symlinks = "relative"}, ChkRes),
	    install_bin3(Config, Inst#inst{symlinks = true,
					   ln_s = "ln -s",
					   bindir_symlinks = "absolute"}, ChkRes);
	false ->
	    ok
    end.
    
    

install_bin3(Config,
		 #inst{cmd_prefix = CMD_PRFX,
		       ln_s = LN_S,
		       destdir = DESTDIR,
		       extra_prefix = EXTRA_PREFIX,
		       exec_prefix = EXEC_PREFIX,
		       bindir = BINDIR,
		       erlang_bindir = ERLANG_BINDIR,
		       bindir_symlinks = BINDIR_SYMLINKS} = Inst,
		 ChkRes) ->
    Test = proplists:get_value(testcase, Config),
    DDir = proplists:get_value(data_dir, Config),
    TDir = proplists:get_value(test_dir, Config),
    InstallBin = filename:join(DDir, "install_bin"),
    ResFile = filename:join(TDir, atom_to_list(Test) ++ "-result.txt"),
    Cmd = CMD_PRFX ++ " "
	++ InstallBin ++ " --ln_s \"" ++ LN_S
	++ "\" --destdir \"" ++ DESTDIR
	++ "\" --extra-prefix \"" ++ EXTRA_PREFIX
	++ "\" --bindir-symlinks \"" ++ BINDIR_SYMLINKS
	++ "\" --bindir \"" ++ BINDIR
	++ "\" --erlang-bindir \"" ++ ERLANG_BINDIR
	++ "\" --exec-prefix \"" ++ EXEC_PREFIX
	++ "\" --test-file \"" ++ ResFile ++ "\" erl erlc",

    io:format("CMD_PRFX        = \"~s\"~n"
	      "LN_S            = \"~s\"~n" 
	      "BINDIR_SYMLINKS = \"~s\"~n"
	      "exec_prefix     = \"~s\"~n"
	      "bindir          = \"~s\"~n"
	      "erlang_bindir   = \"~s\"~n"
	      "EXTRA_PREFIX    = \"~s\"~n"
	      "DESTDIR         = \"~s\"~n",
	      [CMD_PRFX, LN_S, BINDIR_SYMLINKS, EXEC_PREFIX, BINDIR,
	       ERLANG_BINDIR, EXTRA_PREFIX, DESTDIR]),

    io:format("$ ~s~n", [Cmd]),
    CmdOutput = os:cmd(Cmd),
    io:format("~s~n", [CmdOutput]),
    ChkRes(case file:consult(ResFile) of
	       {ok, [Res]} -> Res;
	       Err -> exit({result, Err})
	   end,
	   Inst).

join("") ->
    "";
join([""|Ds]) ->
    join(Ds);
join([D|Ds]) ->
    "/" ++ string:strip(D, both, $/) ++ join(Ds).
