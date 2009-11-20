%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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

%%% Purpose : Autoconf for cross environments.

-module(ts_autoconf_vxworks).
-export([configure/1]).
%%% Supported cross platforms:
-define(PLATFORMS, ["vxworks_cpu32", "vxworks_ppc860", "vxworks_ppc603", 
		    "vxworks_sparc", "vxworks_ppc750", "vxworks_simso"]).
-include("ts.hrl").

%% takes an argument {Target_arch, Target_host} (e.g. {vxworks_ppc860, thorin}).
configure({Target_arch, Target_host}) ->
    case variables({Target_arch, Target_host}) of
	{ok, Vars} ->
	    ts_lib:subst_file("conf_vars.in", "conf_vars", Vars);
	Error ->
	    Error
    end.

variables(Cross_spec) ->
    run_tests(Cross_spec, tests(), []).

run_tests(Cross_spec, [{Prompt, Tester}|Rest], Vars) ->
    io:format("checking ~s... ", [Prompt]),
    case catch Tester(Cross_spec, Vars) of
	{'EXIT', Reason} ->
	    io:format("FAILED~nExit status: ~p~n", [Reason]),
	    {error, auto_conf_failed};
	{Result, NewVars} ->
	    io:format("~s~n", [lists:concat([Result])]),
	    run_tests(Cross_spec, Rest, NewVars)
    end;
run_tests(_Cross_spec, [], Vars) ->
    {ok, Vars}.


%%% The tests.

tests() ->
    [{"supported target architecture", fun target_architecture/2},
     {"cross target host to run tests on", fun target_host/2},
     {"CPU type", fun cpu/2},
     {"for cross-compiling gcc", fun find_gcc/2},
     {"for cross-linker", fun find_ld/2},
     {"for object extension", fun find_obj/2},
     {"for shared libraries extension", fun find_dll/2},
     {"for executables extension", fun find_exe/2},
     {"for make", fun find_make/2}].

target_architecture({Architecture, _Target_host}, Vars) ->
    case lists:member(Architecture, ?PLATFORMS) of
	true ->
	    {Architecture, [{host_os, os_type(Architecture)}, {host,  Architecture}|Vars]};    
	false ->
	    {"unsupported_platform", Vars} 
    end.

target_host({_Architecture, Target_host}, Vars) ->
    {Target_host, [{target_host, Target_host} | Vars]}.

cpu({Arch, _Target_host}, Vars) ->
    Cpu = processor(Arch),
    {Cpu, [{host_cpu, Cpu}|Vars]}.
		
find_gcc({Arch, _Target_host}, Vars) ->
    Gcc = "cc" ++ gnu_suffix(Arch),
    case os:find_executable(Gcc) of
	false ->
	    {no, Vars};
	Path when is_list(Path) ->
	    Cflags = cflags(Arch),
	    {Path, [{'CC', Gcc},
		    {'CFLAGS', Cflags},
		    {'EI_CFLAGS', Cflags},
		    {'ERTS_CFLAGS', Cflags},
		    {'DEFS', ""},
		    {'ERTS_LIBS', ""},
		    {'LIBS', ""},
		    {'SHLIB_CFLAGS', Cflags},
		    {test_c_compiler, "{gnuc, undefined}"} | Vars]}
    end.

find_ld({Arch, _Target_host}, Vars) ->
    Linker = "ld" ++ gnu_suffix(Arch),
    case os:find_executable(Linker) of
	false ->
	    {no, Vars};
	Path when is_list(Path) ->
	    {Path, [{'LD', Linker},
		    {'CROSSLDFLAGS', ldflags(Arch)},
		    {'SHLIB_EXTRACT_ALL', ""},
		    {'SHLIB_LD', Linker},
		    {'SHLIB_LDFLAGS', ""},
		    {'SHLIB_LDLIBS', ""} | Vars]}
    end.

find_obj({Arch, _Target_host}, Vars) ->
    Obj = obj_ext(Arch),
    {Obj, [{obj, Obj}|Vars]}.

find_dll({Arch, _Target_host}, Vars) ->
    Dll = dll_ext(Arch),
    {Dll, [{'SHLIB_SUFFIX', Dll}|Vars]}.

find_exe({Arch, _Target_host}, Vars) ->
    Exe = exe_ext(Arch),
    {Exe, [{exe, Exe}|Vars]}.

find_make(_, Vars) ->
    {"make", [{make_command, "make"} | Vars]}.

%%% some utility functions 
gnu_suffix(Arch) ->
    {_, _, _, _, Suffix, _Cpu, _Cflags, _} = cross_data(Arch),
    Suffix.

processor(Arch) ->
    {_, _, _, _, _Suffix, Cpu, _Cflags, _} = cross_data(Arch),
    Cpu.

cflags(Arch) ->
    {_, _, _, _, _Suffix, _Cpu, Cflags, _} = cross_data(Arch),
    Cflags.

ldflags(Arch) ->
    {_, _, _, _, _Suffix, _Cpu, _Cflags, Ldflags} = cross_data(Arch),
    Ldflags.

os_type(Arch) ->
    {Os_type, _, _, _, _, _, _, _} = cross_data(Arch),
    Os_type.

obj_ext(Arch) ->
    {_, _, Obj, _, _, _, _, _} = cross_data(Arch),
    Obj.

dll_ext(Arch) ->
    {_, _, _, Dll, _, _, _, _} = cross_data(Arch),
    Dll.

exe_ext(Arch) ->
    {_, Exe, _, _, _, _, _, _} = cross_data(Arch),
    Exe.

cross_data(Arch) ->
    case Arch of
	"vxworks_cpu32" ->
	    {"VxWorks", "", ".o", ".eld", "68k", "cpu32",
	    "-DCPU=CPU32 -DVXWORKS -I$(WIND_BASE)/target/h -mnobitfield -fno-builtin -nostdinc -fvolatile -msoft-float",
	    "-r -d"};
	"vxworks_ppc860" ->
	    {"VxWorks", "", ".o", ".eld", "ppc", "ppc860",
	     "-DCPU=PPC860 -DVXWORKS -I$(WIND_BASE)/target/h -mcpu=860 -fno-builtin -fno-for-scope -msoft-float -D_GNU_TOOL -nostdinc",
	    "-r -d"};
	"vxworks_ppc603" ->
	    {"VxWorks", "", ".o", ".eld", "ppc", "ppc603",
	     "-DCPU=PPC603 -DVXWORKS -I$(WIND_BASE)/target/h -fno-builtin -fno-for-scope -D_GNU_TOOL -nostdinc",
	    "-r -d"};
	"vxworks_sparc" ->
	    %%% The Sparc Architecture is included for private use (i.e. not Tornado 1.0.1 compatible).
	    {"VxWorks", "", ".o", ".eld", "sparc", "sparc",
	    "-DCPU=SPARC -DVXWORKS  -I/home/gandalf/bsproj/BS.2/UOS/vw/5.2/h -fno-builtin -nostdinc",
	    "-r -d"};
	"vxworks_ppc750" ->
	    {"VxWorks", "", ".o", ".eld", "ppc", "ppc604",
	     "-DCPU=PPC604 -DVXWORKS -DTOOL_FAMILY=gnu -DTOOL=gnu -I$(WIND_BASE)/target/h -fno-builtin -fno-for-scope -D_GNU_TOOL",
	    "-r -d"};
	"vxworks_simso" ->
	    {"VxWorks", "", ".o", ".eld", "simso", "simso",
	     "-DCPU=SIMSPARCSOLARIS -DVXWORKS -DTOOL_FAMILY=gnu -DTOOL=gnu -I$(WIND_BASE)/target/h -I$(WIND_GCC_INCLUDE) -fno-builtin -fno-for-scope -D_GNU_TOOL",
	    "-r -d"}
	
    end.
