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
-module(ts_install).


-export([install/2, platform_id/1]).

-include("ts.hrl").

install(install_local, Options) ->
    install(os:type(), Options);

install(TargetSystem, Options) ->
    io:format("Running configure for cross architecture, network target name~n"
	      "~p~n", [TargetSystem]),
    case autoconf(TargetSystem) of
	{ok, Vars0} ->
	    OsType = os_type(TargetSystem),
	    Vars1 = ts_erl_config:variables(merge(Vars0,Options),OsType),
	    {Options1, Vars2} = add_vars(Vars1, Options),
	    Vars3 = lists:flatten([Options1|Vars2]),
	    write_terms(?variables, Vars3);
	{error, Reason} ->
	    {error, Reason}
    end.

os_type({unix,_}=OsType) -> OsType;
os_type({win32,_}=OsType) -> OsType;
os_type(_Other) -> vxworks.

merge(Vars,[]) ->
    Vars;
merge(Vars,[{crossroot,X}| Tail]) ->
    merge([{crossroot, X} | Vars], Tail);
merge(Vars,[_X | Tail]) ->
    merge(Vars,Tail).

%% Autoconf for various platforms.
%% unix uses the configure script
%% win32 uses ts_autoconf_win32
%% VxWorks uses ts_autoconf_vxworks.

autoconf(TargetSystem) ->
    case autoconf1(TargetSystem) of
	ok ->
	    autoconf2(file:read_file("conf_vars"));
	Error ->
	    Error
    end.

autoconf1({win32, _}) ->
    ts_autoconf_win32:configure();
autoconf1({unix, _}) ->
    unix_autoconf();
autoconf1(Other) ->
    ts_autoconf_vxworks:configure(Other).

autoconf2({ok, Bin}) ->
    get_vars(binary_to_list(Bin), name, [], []);
autoconf2(Error) ->
    Error.

get_vars([$:|Rest], name, Current, Result) ->
    Name = list_to_atom(lists:reverse(Current)),
    get_vars(Rest, value, [], [Name|Result]);
get_vars([$\r|Rest], value, Current, Result) ->
    get_vars(Rest, value, Current, Result);
get_vars([$\n|Rest], value, Current, [Name|Result]) ->
    Value = lists:reverse(Current),
    get_vars(Rest, name, [], [{Name, Value}|Result]);
get_vars([C|Rest], State, Current, Result) ->
    get_vars(Rest, State, [C|Current], Result);
get_vars([], name, [], Result) ->
    {ok, Result};
get_vars(_, _, _, _) ->
    {error, fatal_bad_conf_vars}.

unix_autoconf() ->
    Configure = filename:absname("configure"),
    Args = case catch erlang:system_info(threads) of
	       false -> "";
	       _ -> " --enable-shlib-thread-safety"
	   end
	++ case catch string:str(erlang:system_info(system_version),
				 "debug") > 0 of
	       false -> "";
	       _ -> " --enable-debug-mode"
	   end,
    case filelib:is_file(Configure) of
	true ->
	    Env = macosx_cflags(),
	    Port = open_port({spawn, Configure ++ Args},
			     [stream, eof, {env,Env}]),
	    ts_lib:print_data(Port);
	false ->
	    {error, no_configure_script}
    end.

macosx_cflags() ->
    case os:type() of
	{unix, darwin} ->
	    %% To ensure that the drivers we build can be loaded
	    %% by the emulator, add either -m32 or -m64 to CFLAGS.
	    WordSize = erlang:system_info(wordsize),
	    Mflag = "-m" ++ integer_to_list(8*WordSize),
	    [{"CFLAGS", Mflag},{"LDFLAGS", Mflag}];
	_ ->
	    []
    end.

write_terms(Name, Terms) ->
    case file:open(Name, [write]) of
	{ok, Fd} ->
	    Result = write_terms1(Fd, Terms),
	    file:close(Fd),
	    Result;
	{error, Reason} ->
	    {error, Reason}
    end.

write_terms1(Fd, [Term|Rest]) ->
    ok = io:format(Fd, "~p.\n", [Term]),
    write_terms1(Fd, Rest);
write_terms1(_, []) ->
    ok.

add_vars(Vars0, Opts0) ->
    {Opts,LongNames} =
	case lists:keymember(longnames, 1, Opts0) of
	    true ->
		{lists:keydelete(longnames, 1, Opts0),true};
	    false ->
		{Opts0,false}
	end,
    {PlatformId, PlatformLabel, PlatformFilename, Version} =
	platform([{longnames, LongNames}|Vars0]),
    {Opts, [{longnames, LongNames},
	    {platform_id, PlatformId},
	    {platform_filename, PlatformFilename},
	    {rsh_name, get_rsh_name()},
	    {platform_label, PlatformLabel},
	    {erl_flags, []},
	    {erl_release, Version},
	    {ts_testcase_callback, get_testcase_callback()} | Vars0]}.

get_testcase_callback() ->
    case os:getenv("TS_TESTCASE_CALLBACK") of
	ModFunc when is_list(ModFunc), ModFunc /= "" ->
	    case string:tokens(ModFunc, " ") of
		[_Mod,_Func] -> ModFunc;
		_ -> ""
	    end;
	_ ->
	    case init:get_argument(ts_testcase_callback) of
		{ok,[[Mod,Func]]} -> Mod ++ " " ++ Func;
		_ -> ""
	    end
    end.

get_rsh_name() ->
    case os:getenv("ERL_RSH") of
	false ->
	    case ts_lib:erlang_type() of
		{clearcase, _} ->
		    "ctrsh";
		{_, _} ->
		    "rsh"
	    end;
	Str ->
	    Str
    end.

platform_id(Vars) ->
    {Id,_,_,_} = platform(Vars),
    Id.
    
platform(Vars) ->
    Hostname = hostname(),

    {Type,Version} = ts_lib:erlang_type(),
    Cpu = ts_lib:var('CPU', Vars),
    Os = ts_lib:var(os, Vars),

    ErlType = to_upper(atom_to_list(Type)),
    OsType = ts_lib:initial_capital(Os),
    CpuType = ts_lib:initial_capital(Cpu),
    LinuxDist = linux_dist(),
    ExtraLabel = extra_platform_label(),
    Schedulers = schedulers(),
    BindType = bind_type(),
    KP = kernel_poll(),
    IOTHR = io_thread(),
    LC = lock_checking(),
    MT = modified_timing(),
    AsyncThreads = async_threads(),
    HeapType = heap_type_label(),
    Debug = debug(),
    CpuBits = word_size(),
    Common = lists:concat([Hostname,"/",OsType,"/",CpuType,CpuBits,LinuxDist,
			   Schedulers,BindType,KP,IOTHR,LC,MT,AsyncThreads,
			   HeapType,Debug,ExtraLabel]),
    PlatformId = lists:concat([ErlType, " ", Version, Common]),
    PlatformLabel = ErlType ++ Common,
    PlatformFilename = platform_as_filename(PlatformId),
    {PlatformId, PlatformLabel, PlatformFilename, Version}.

platform_as_filename(Label) ->
    lists:map(fun($ ) -> $_;
		 ($/) -> $_;
		 (C) when $A =< C, C =< $Z -> C - $A + $a;
		 (C) -> C end,
	      Label).

to_upper(String) ->
    lists:map(fun(C) when $a =< C, C =< $z -> C - $a + $A;
		 (C) -> C end,
	      String).

word_size() ->
    case erlang:system_info(wordsize) of
	4 -> "";
	8 -> "/64"
    end.

linux_dist() ->
    case os:type() of
	{unix,linux} ->
	    linux_dist_1([fun linux_dist_suse/0]);
	_ -> ""
    end.

linux_dist_1([F|T]) ->
    case F() of
	"" -> linux_dist_1(T);
	Str -> Str
    end;
linux_dist_1([]) -> "".

linux_dist_suse() ->
    case filelib:is_file("/etc/SuSE-release") of
	false -> "";
	true ->
	    Ver0 = os:cmd("awk '/^VERSION/ {print $3}' /etc/SuSE-release"),
	    [_|Ver1] = lists:reverse(Ver0),
	    Ver = lists:reverse(Ver1),
	    "/Suse" ++ Ver
    end.

hostname() ->
    case catch inet:gethostname() of
	{ok, Hostname} when is_list(Hostname) ->
	    "/" ++ lists:takewhile(fun (C) -> C /= $. end, Hostname);
	_ ->
	    "/localhost"
    end.

heap_type_label() ->
    case catch erlang:system_info(heap_type) of
	hybrid -> "/Hybrid";
	_ -> "" %private
    end.

async_threads() ->
    case catch erlang:system_info(threads) of
	true -> "/A"++integer_to_list(erlang:system_info(thread_pool_size));
	_ -> ""
    end.

schedulers() ->
    case catch erlang:system_info(smp_support) of
	true ->
	    case {erlang:system_info(schedulers),
		  erlang:system_info(schedulers_online)} of
		{S,S} ->
		    "/S"++integer_to_list(S);
		{S,O} ->
		    "/S"++integer_to_list(S) ++ ":" ++
			integer_to_list(O)
	    end;
	_ -> ""
    end.

bind_type() ->
    case catch erlang:system_info(scheduler_bind_type) of
	thread_no_node_processor_spread -> "/sbttnnps";
	no_node_processor_spread -> "/sbtnnps";
	no_node_thread_spread -> "/sbtnnts";
	processor_spread -> "/sbtps";
	thread_spread -> "/sbtts";
	no_spread -> "/sbtns";
	_ -> ""
    end.
					

debug() ->
    case string:str(erlang:system_info(system_version), "debug") of
	0 -> "";
	_ -> "/Debug"
    end.

lock_checking() ->
    case catch erlang:system_info(lock_checking) of
	true -> "/LC";
	_ -> ""
    end.

modified_timing() ->
    case catch erlang:system_info(modified_timing_level) of
	N when is_integer(N) ->
	    "/T" ++ integer_to_list(N);
	_ -> ""
    end.

kernel_poll() ->
    case catch erlang:system_info(kernel_poll) of
	true -> "/KP";
	_ -> ""
    end.

io_thread() ->
    case catch erlang:system_info(io_thread) of
	true -> "/IOTHR";
	_ -> ""
    end.

extra_platform_label() ->
    case os:getenv("TS_EXTRA_PLATFORM_LABEL") of
	[] -> "";
	[_|_]=Label -> "/" ++ Label;
	false -> ""
    end.

