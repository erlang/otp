%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(ts_install).

-export([install/2, platform_id/1]).

-include("ts.hrl").
-include_lib("kernel/include/file.hrl").

install(install_local, Options) ->
    install(os:type(), Options);

install(TargetSystem, Options) ->
    case file:consult(?variables) of
	{ok, Vars} ->
	    case proplists:get_value(cross,Vars) of
		"yes" when Options == []->
		    target_install(Vars);
		_ ->
		    build_install(TargetSystem, Options)
	    end;
	_ ->
	    build_install(TargetSystem, Options)
    end.


build_install(TargetSystem, Options) ->
    XComp = parse_xcomp_file(proplists:get_value(xcomp,Options)),
    case autoconf(TargetSystem, XComp++Options) of
	{ok, Vars0} ->
	    OsType = os_type(TargetSystem),
	    Vars1 = ts_erl_config:variables(Vars0++XComp++Options,OsType),
	    {Options1, Vars2} = add_vars(Vars1, Options),
	    Vars3 = lists:flatten([Options1|Vars2]),
	    write_terms(?variables, Vars3);
	{error, Reason} ->
	    {error, Reason}
    end.

os_type({unix,_}=OsType) -> OsType;
os_type({win32,_}=OsType) -> OsType.

target_install(CrossVars) ->
    io:format("Cross installation detected, skipping configure and data_dir make~n"),
    case file:rename(?variables,?cross_variables) of
	ok ->
	    ok;
	_ ->
	    io:format("Could not find variables file from cross make~n"),
	    throw(cross_installation_failed)
    end,
    CPU = proplists:get_value('CPU',CrossVars),
    OS = proplists:get_value(os,CrossVars),
    {Options,Vars} = add_vars([{cross,"yes"},{'CPU',CPU},{os,OS}],[]),
    Variables = lists:flatten([Options|Vars]),
    write_terms(?variables, Variables).

%% Autoconf for various platforms.
%% unix uses the configure script
%% win32 uses ts_autoconf_win32

autoconf(TargetSystem, XComp) ->
    case autoconf1(TargetSystem, XComp) of
	ok ->
	    autoconf2(file:read_file("conf_vars"));
	Error ->
	    Error
    end.

autoconf1({win32, _},[{cross,"no"}]) ->
    ts_autoconf_win32:configure();
autoconf1({unix, _},XCompFile) ->
    unix_autoconf(XCompFile);
autoconf1(_,_) ->
    io:format("cross compilation not supported for that this platform~n"),
    throw(cross_installation_failed).

autoconf2({ok, Bin}) ->
    get_vars(ts_lib:b2s(Bin), name, [], []);
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

unix_autoconf(XConf) ->
    Configure = filename:absname("configure"),
    Flags = proplists:get_value(crossflags,XConf,[]),
    Env = proplists:get_value(crossenv,XConf,[]),
    Host = get_xcomp_flag("host", Flags),
    Build = get_xcomp_flag("build", Flags),
    Threads = [" --enable-shlib-thread-safety" ||
		  erlang:system_info(threads) /= false],
    Debug = [" --enable-debug-mode" ||
		string:find(erlang:system_info(system_version),"debug") =/= nomatch],
    MXX_Build = [Y || Y <- string:lexemes(os:getenv("CONFIG_FLAGS", ""), " \t\n"),
		      Y == "--enable-m64-build"
			  orelse Y == "--enable-m32-build"],
    Args = Host ++ Build ++ Threads ++ Debug ++ " " ++ MXX_Build,
    case filelib:is_file(Configure) of
	true ->
	    OSXEnv = macosx_cflags(),
	    UnQuotedEnv = assign_vars(unquote(Env++OSXEnv)),
	    io:format("Running ~ts~nEnv: ~p~n",
		      [lists:flatten(Configure ++ Args),UnQuotedEnv]),
	    Port = open_port({spawn, lists:flatten(["\"",Configure,"\"",Args])},
			     [stream, eof, {env,UnQuotedEnv}]),
	    ts_lib:print_data(Port);
	false ->
	    {error, no_configure_script}
    end.

unquote([{Var,Val}|T]) ->
    [{Var,unquote(Val)}|unquote(T)];
unquote([]) ->
    [];
unquote("\""++Rest) ->
    lists:reverse(tl(lists:reverse(Rest)));
unquote(String) ->
    String.

assign_vars([]) ->
    [];
assign_vars([{VAR,FlagsStr} | VARs]) ->
    [{VAR,assign_vars(FlagsStr)} | assign_vars(VARs)];
assign_vars(FlagsStr) ->
    Flags = [assign_all_vars(Str,[]) || Str <- string:lexemes(FlagsStr, [$\s])],
    lists:flatten(lists:join(" ", Flags)).

assign_all_vars([$$ | Rest], FlagSoFar) ->
    {VarName,Rest1} = get_var_name(Rest, []),
    assign_all_vars(Rest1, FlagSoFar ++ os:getenv(VarName, ""));
assign_all_vars([Char | Rest], FlagSoFar) ->
    assign_all_vars(Rest, FlagSoFar ++ [Char]);
assign_all_vars([], Flag) ->
    Flag.

get_var_name([Ch | Rest] = Str, VarR) ->
    case valid_char(Ch) of
	true  -> get_var_name(Rest, [Ch | VarR]);
	false -> {lists:reverse(VarR),Str}
    end;
get_var_name([], VarR) ->
    {lists:reverse(VarR),[]}.

valid_char(Ch) when Ch >= $a, Ch =< $z -> true;
valid_char(Ch) when Ch >= $A, Ch =< $Z -> true;
valid_char(Ch) when Ch >= $0, Ch =< $9 -> true;
valid_char($_)                         -> true;
valid_char(_)                          -> false.

get_xcomp_flag(Flag, Flags) ->
    get_xcomp_flag(Flag, Flag, Flags).
get_xcomp_flag(Flag, Tag, Flags) ->
    case proplists:get_value(Flag,Flags) of
	undefined -> "";
	"guess" -> [" --",Tag,"=",os:cmd("$ERL_TOP/erts/autoconf/config.guess")];
	HostVal -> [" --",Tag,"=",HostVal]
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

parse_xcomp_file(undefined) ->
    [{cross,"no"}];
parse_xcomp_file(Filepath) ->
    {ok,Bin} = file:read_file(Filepath),
    Lines = binary:split(Bin,<<"\n">>,[global,trim]),
    {Envs,Flags} = parse_xcomp_file(Lines,[],[]),
    [{cross,"yes"},{crossroot,os:getenv("ERL_TOP")},
     {crossenv,Envs},{crossflags,Flags}].

parse_xcomp_file([<<A:8,_/binary>> = Line|R],Envs,Flags)
  when $A =< A, A =< $Z ->
    [Var,Value] = binary:split(Line,<<"=">>),
    parse_xcomp_file(R,[{ts_lib:b2s(Var),
			 ts_lib:b2s(Value)}|Envs],Flags);
parse_xcomp_file([<<"erl_xcomp_",Line/binary>>|R],Envs,Flags) ->
    [Var,Value] = binary:split(Line,<<"=">>),
    parse_xcomp_file(R,Envs,[{ts_lib:b2s(Var),
			      ts_lib:b2s(Value)}|Flags]);
parse_xcomp_file([_|R],Envs,Flags) ->
    parse_xcomp_file(R,Envs,Flags);
parse_xcomp_file([],Envs,Flags) ->
    {lists:reverse(Envs),lists:reverse(Flags)}.

write_terms(Name, Terms) ->
    case file:open(Name, [write]) of
	{ok, Fd} ->
	    Result = write_terms1(Fd, remove_duplicates(Terms)),
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

remove_duplicates(List) ->
    lists:reverse(
      lists:foldl(fun({Key,Val},Acc) ->
			  R = make_ref(),
			  case proplists:get_value(Key,Acc,R) of
			      R -> [{Key,Val}|Acc];
			      _Else ->
				  Acc
			  end
		  end,[],List)).

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
    NetDir = lists:concat(["/net", hostname()]),
    Mounted = case file:read_file_info(NetDir) of
		  {ok, #file_info{type = directory}} -> NetDir;
		  _ -> ""
	      end,
    {Opts, [{longnames, LongNames},
	    {platform_id, PlatformId},
	    {platform_filename, PlatformFilename},
	    {rsh_name, os:getenv("ERL_RSH", "ssh")},
	    {platform_label, PlatformLabel},
	    {ts_net_dir, Mounted},
	    {erl_flags, []},
	    {erl_release, Version},
	    {ts_testcase_callback, get_testcase_callback()} | Vars0]}.

get_testcase_callback() ->
    case os:getenv("TS_TESTCASE_CALLBACK") of
	ModFunc when is_list(ModFunc), ModFunc /= "" ->
	    case string:lexemes(ModFunc, " ") of
		[_Mod,_Func] -> ModFunc;
		_ -> ""
	    end;
	_ ->
	    case init:get_argument(ts_testcase_callback) of
		{ok,[[Mod,Func]]} -> Mod ++ " " ++ Func;
		_ -> ""
	    end
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
    OffHeapMsgQ = off_heap_msgq(),
    Debug = debug(),
    CpuBits = word_size(),
    Common = lists:concat([Hostname,"/",OsType,"/",CpuType,CpuBits,LinuxDist,
			   Schedulers,BindType,KP,IOTHR,LC,MT,AsyncThreads,
			   OffHeapMsgQ,Debug,ExtraLabel]),
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
    case {erlang:system_info({wordsize,external}),
	  erlang:system_info({wordsize,internal})} of
	{4,4} -> "";
	{8,8} -> "/64";
	{8,4} -> "/Halfword"
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

async_threads() ->
    case catch erlang:system_info(threads) of
	true -> "/A"++integer_to_list(erlang:system_info(thread_pool_size));
	_ -> ""
    end.

off_heap_msgq() ->
    case catch erlang:system_info(message_queue_data) of
	off_heap -> "/OffHeapMsgQ";
	_ -> ""
    end.

schedulers() ->
    case {erlang:system_info(schedulers),
          erlang:system_info(schedulers_online)} of
        {S,S} ->
            "/S"++integer_to_list(S);
        {S,O} ->
            "/S"++integer_to_list(S) ++ ":" ++
                integer_to_list(O)
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
    case string:find(erlang:system_info(system_version), "debug") of
	nomatch -> "";
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
