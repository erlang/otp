%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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

%%% Purpose : Autoconf for Windows.

-module(ts_autoconf_win32).
-export([configure/0]).

-include("ts.hrl").

configure() ->
    case variables() of
	{ok, Vars} ->
	    ts_lib:subst_file("conf_vars.in", "conf_vars", Vars);
	Error ->
	    Error
    end.

variables() ->
    run_tests(tests(), []).

run_tests([{Prompt, Tester}|Rest], Vars) ->
    io:format("checking ~s... ", [Prompt]),
    case catch Tester(Vars) of
	{'EXIT', Reason} ->
	    io:format("FAILED~nExit status: ~p~n", [Reason]),
	    {error, auto_conf_failed};
	{Result, NewVars} ->
	    io:format("~s~n", [lists:concat([Result])]),
	    run_tests(Rest, NewVars)
    end;
run_tests([], Vars) ->
    {ok, Vars}.

%%% The tests.

tests() ->
    [{"host system type", fun system_type/1},
     {"CPU type", fun cpu/1},
     {"for C compiler", fun c_compiler/1},
     {"for make program", fun make/1},
     {"for location of SSL libraries", fun ssl/1},
     {"for location of Java compiler", fun javac/1}].

system_type(Vars) ->
    Os = case os:type() of
	     {win32, nt} ->
		 case os:version() of
		     {4,_,_} -> "Windows NT";
		     {5,0,_} -> "Windows 2000";
		     {5,1,_} -> "Windows XP";
		     {5,2,_} -> "Windows 2003";
		     {6,0,_} -> "Windows Vista";
		     {6,1,_} -> "Windows 7";
		     {_,_,_} -> "Windows NCC-1701-D"
		 end;
	     {win32, windows} -> 
		 case os:version() of
		     {4,0,_} ->  "Windows 95";
		     {4,10,_} -> "Windows 98"
		 end;
	     {win32, _} -> "Windows"
	 end,
    {Os, [{host_os, Os}, {host, "win32"}|Vars]}.

cpu(Vars) ->
    Arch = os:getenv("PROCESSOR_ARCHITECTURE"),
    Level0 = os:getenv("PROCESSOR_Level"),
    Cpu = case {Arch, Level0} of
	      {"x86", Level} when is_list(Level) ->
		  "i" ++ Level ++ "86";
	      {Other, _Level} when is_list(Other) ->
		  Other;
	      {false, _} ->
		  "i386"
	  end,
    {Cpu, [{host_cpu, Cpu}|Vars]}.

c_compiler(Vars) ->
    try
	CompTests = [{msc, fun visual_cxx/1},
		     {gnuc, fun mingw32/1}],
	%% First try to find the same compiler that the system
	%% was built with...
	UsedCompiler = case erlang:system_info(c_compiler_used) of
			   {UsedCmplr, _} ->
			       case lists:keysearch(UsedCmplr, 1, CompTests) of
				   {value, {UsedCmplr, CompTest}} ->
				       CompTest(Vars);
				   _ -> 
				       ok
			       end,
			       UsedCmplr;
			   undefined ->
			       undefined
		       end,
	%% ... then try to find a compiler...
	lists:foreach(fun ({Cmplr, _CmplrTst}) when Cmplr =:= UsedCompiler ->
			      ok; % Have already checked for this one
			  ({_Cmplr, CmplrTst}) ->
			      CmplrTst(Vars)
		      end,
		      CompTests),
	{no, Vars}
    catch
	throw:{_Path, _NewVars} = Res -> Res
    end.

visual_cxx(Vars) ->
    case os:find_executable("cl") of
	false ->
	    {no, Vars};
	Path when is_list(Path) ->
	    {DEFAULT_THR_LIB,
	     ERTS_THR_LIB,
	     DLL,
	     DBG_LINK,
	     DBG_COMP,
	     OPT} =
		case is_debug_build() of
		    true ->
			{"-MTd ",
			 "-MDd ",
			 "-LDd ",
			 "-link -debug -pdb:none ",
			 "-Z7 -DDEBUG",
			 " "};
		    false ->
			{"-MT ",
			 "-MD ",
			 "-LD ",
			 "-Zi -link ",
			 "-Zi ",
			 "-Ox "}
		end,
	    WIN32 = "-D__WIN32__ ",
	    ERTS_CFLAGS = ERTS_THR_LIB ++ WIN32 ++ OPT ++ DBG_COMP,
	    LIBS = "ws2_32.lib",
	    CC = "cl -nologo",
	    throw({Path, [{'CC', CC},
			  {'LD', CC},
			  {'SHLIB_LD', CC},
			  {'SHLIB_LDFLAGS', ERTS_THR_LIB ++ DLL},
			  {'SHLIB_LDLIBS', DBG_LINK ++ "kernel32.lib"},
			  {'SHLIB_EXTRACT_ALL', ""},
			  {'CFLAGS', DEFAULT_THR_LIB ++ WIN32 ++ DBG_COMP},
			  {'EI_CFLAGS', DEFAULT_THR_LIB ++ WIN32 ++ DBG_COMP},
			  {'ERTS_CFLAGS', ERTS_CFLAGS},
			  {'SHLIB_CFLAGS', ERTS_CFLAGS++DLL},
			  {'CROSSLDFLAGS', ""},
			  {'DEFS', common_c_defs()},
			  {'SHLIB_SUFFIX', ".dll"},
			  {'ERTS_LIBS', ERTS_THR_LIB ++ LIBS},
			  {'LIBS', DEFAULT_THR_LIB ++ DBG_LINK ++ LIBS},
			  {obj,".obj"},
			  {exe, ".exe"},
			  {test_c_compiler, "{msc, undefined}"}
			  | Vars]})
    end.
	
mingw32(Vars) ->
    Gcc = "mingw32-gcc",
    case os:find_executable(Gcc) of
	false ->
	    {no, Vars};
	Path when is_list(Path) ->
	    {DBG_COMP,
	     OPT} =
		case is_debug_build() of
		    true ->
			{"-DDEBUG",
			 " "};
		    false ->
			{" ",
			 "-O2 "}
		end,
	    WIN32 = "-D__WIN32__ ",
	    ERTS_CFLAGS = WIN32 ++ "-g " ++ OPT ++ DBG_COMP,
	    LIBS = "-lws2_32",
	    CC = Gcc,
	    throw({Path, [{'CC', CC},
			  {'LD', CC},
			  {'SHLIB_LD', CC},
			  {'SHLIB_LDFLAGS', "-shared "},
			  {'SHLIB_LDLIBS', " -lkernel32"},
			  {'SHLIB_EXTRACT_ALL', ""},
			  {'CFLAGS', WIN32 ++ DBG_COMP},
			  {'EI_CFLAGS', WIN32 ++ DBG_COMP},
			  {'ERTS_CFLAGS', ERTS_CFLAGS},
			  {'SHLIB_CFLAGS', ERTS_CFLAGS},
			  {'CROSSLDFLAGS', ""},
			  {'DEFS', common_c_defs()},
			  {'SHLIB_SUFFIX', ".dll"},
			  {'ERTS_LIBS', LIBS},
			  {'LIBS', LIBS},
			  {obj,".o"},
			  {exe, ".exe"},
			  {test_c_compiler, "{gnuc, undefined}"}
			  | Vars]})
    end.

common_c_defs() ->
    "-DHAVE_STRERROR=1".

make(Vars) ->
    try
	find_make("nmake -nologo", Vars),
	find_make("mingw32-make", Vars)
    catch
	throw:{_Path, _NewVars} = Res -> Res
    end.

find_make(MakeCmd, Vars) ->
    [Make|_] = string:lexemes(MakeCmd, " \t"),
    case os:find_executable(Make) of
	false ->
	    {no, Vars};
	Path when is_list(Path) ->
	    throw({Path, [{make_command, MakeCmd} | Vars]})
    end.

ssl(Vars) ->
    {"win32",[{'SSLEAY_ROOT',"win32"}|Vars]}.

javac(Vars) ->
    case os:find_executable("javac") of
	false ->
	    {no, Vars};
	Path when is_list(Path) ->
	    {Path, [{'JAVAC', "javac"} | Vars]}
    end.

is_debug_build() ->
    case catch string:find(erlang:system_info(system_version), "debug") of
        nomatch ->
            false;
	_Else ->
	    true
    end.
