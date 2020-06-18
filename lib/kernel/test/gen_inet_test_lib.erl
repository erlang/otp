%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2020-2020. All Rights Reserved.
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

-module(gen_inet_test_lib).

-export([init_per_suite/1,
         end_per_suite/1]).
-export([tc_try/3]).
-export([f/2,
         print/1, print/2]).

-include("gen_inet_test_lib.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite([{allow_skip, Allow}|Config]) ->
    init_per_suite(Allow, Config);
init_per_suite(Config) ->
    init_per_suite(true, Config).

init_per_suite(AllowSkip, Config) when is_boolean(AllowSkip) ->

    ct:timetrap(timer:minutes(2)),

    try analyze_and_print_host_info() of
        {Factor, HostInfo} when (AllowSkip =:= true) andalso
                                is_integer(Factor) ->
            try maybe_skip(HostInfo) of
                true ->
                    {skip, "Unstable host and/or os (or combo thererof)"};
                false ->
                    [{gen_inet_factor, Factor} | Config]
            catch
                throw:{skip, _} = SKIP ->
                    SKIP
            end;

        {Factor, _HostInfo} when (AllowSkip =:= false) andalso
                                 is_integer(Factor) ->
            [{gen_inet_factor, Factor} | Config]

    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.


end_per_suite(Config) when is_list(Config) ->
    Config.

analyze_and_print_host_info() ->
    {OsFam, OsName} = os:type(),
    Version         =
        case os:version() of
            {Maj, Min, Rel} ->
                f("~w.~w.~w", [Maj, Min, Rel]);
            VStr ->
                VStr
        end,
    case {OsFam, OsName} of
        {unix, linux} ->
            analyze_and_print_linux_host_info(Version);
        {unix, openbsd} ->
            analyze_and_print_openbsd_host_info(Version);
        {unix, freebsd} ->
            analyze_and_print_freebsd_host_info(Version);           
        {unix, netbsd} ->
            analyze_and_print_netbsd_host_info(Version);           
        {unix, darwin} ->
            analyze_and_print_darwin_host_info(Version);
        {unix, sunos} ->
            analyze_and_print_solaris_host_info(Version);
        {win32, nt} ->
            analyze_and_print_win_host_info(Version);
        _ ->
            io:format("OS Family: ~p"
                      "~n   OS Type:        ~p"
                      "~n   Version:        ~p"
                      "~n   Num Schedulers: ~s"
                      "~n", [OsFam, OsName, Version, str_num_schedulers()]),
            {num_schedulers_to_factor(), []}
    end.

linux_which_distro(Version) ->
    case file:read_file_info("/etc/issue") of
        {ok, _} ->
            case [string:trim(S) ||
                     S <- string:tokens(os:cmd("cat /etc/issue"), [$\n])] of
                [DistroStr|_] ->
                    io:format("Linux: ~s"
                              "~n   ~s"
                              "~n",
                              [Version, DistroStr]),
                    case DistroStr of
                        "Wind River Linux" ++ _ ->
                            wind_river;
                        "MontaVista" ++ _ ->
                            montavista;
                        "Yellow Dog" ++ _ ->
                            yellow_dog;
                        _ ->
                            other
                    end;
                X ->
                    io:format("Linux: ~s"
                              "~n   ~p"
                              "~n",
                              [Version, X]),
                    other
            end;
        _ ->
            io:format("Linux: ~s"
                      "~n", [Version]),
            other
    end.
    
analyze_and_print_linux_host_info(Version) ->
    Distro =
        case file:read_file_info("/etc/issue") of
            {ok, _} ->
                linux_which_distro(Version);
            _ ->
                io:format("Linux: ~s"
                          "~n", [Version]),
                other
        end,
    Factor =
        case (catch linux_which_cpuinfo(Distro)) of
            {ok, {CPU, BogoMIPS}} ->
                io:format("CPU: "
                          "~n   Model:          ~s"
                          "~n   BogoMIPS:       ~w"
                          "~n   Num Schedulers: ~s"
                          "~n", [CPU, BogoMIPS, str_num_schedulers()]),
                if
                    (BogoMIPS > 20000) ->
                        1;
                    (BogoMIPS > 10000) ->
                        2;
                    (BogoMIPS > 5000) ->
                        3;
                    (BogoMIPS > 2000) ->
                        5;
                    (BogoMIPS > 1000) ->
                        8;
                    true ->
                        10
                end;
            {ok, CPU} ->
                io:format("CPU: "
                          "~n   Model:          ~s"
                          "~n   Num Schedulers: ~s"
                          "~n", [CPU, str_num_schedulers()]),
                NumChed = erlang:system_info(schedulers),
                if
                    (NumChed > 2) ->
                        2;
                    true ->
                        5
                end;
            _ ->
                5
        end,
    %% Check if we need to adjust the factor because of the memory
    try linux_which_meminfo() of
        AddFactor ->
            io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
            {Factor + AddFactor, []}
    catch
        _:_:_ ->
            io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
            {Factor, []}
    end.

linux_cpuinfo_lookup(Key) when is_list(Key) ->
    linux_info_lookup(Key, "/proc/cpuinfo").

linux_cpuinfo_cpu() ->
    case linux_cpuinfo_lookup("cpu") of
        [Model] ->
            Model;
        _ ->
            "-"
    end.

linux_cpuinfo_motherboard() ->
    case linux_cpuinfo_lookup("motherboard") of
        [MB] ->
            MB;
        _ ->
            "-"
    end.

linux_cpuinfo_bogomips() ->
    case linux_cpuinfo_lookup("bogomips") of
        BMips when is_list(BMips) ->
            try lists:sum([bogomips_to_int(BM) || BM <- BMips])
            catch
                _:_:_ ->
                    "-"
            end;
        _X ->
            "-"
    end.

linux_cpuinfo_total_bogomips() ->
    case linux_cpuinfo_lookup("total bogomips") of
        [TBM] ->
            try bogomips_to_int(TBM)
            catch
                _:_:_ ->
                    "-"
            end;
        _ ->
            "-"
    end.

bogomips_to_int(BM) ->
    try list_to_float(BM) of
        F ->
            floor(F)
    catch
        _:_:_ ->
            try list_to_integer(BM) of
                I ->
                    I
            catch
                _:_:_ ->
                    throw(noinfo)
            end
    end.

linux_cpuinfo_model() ->
    case linux_cpuinfo_lookup("model") of
        [M] ->
            M;
        _X ->
            "-"
    end.

linux_cpuinfo_platform() ->
    case linux_cpuinfo_lookup("platform") of
        [P] ->
            P;
        _ ->
            "-"
    end.

linux_cpuinfo_model_name() ->
    case linux_cpuinfo_lookup("model name") of
        [P|_] ->
            P;
        _ ->
            "-"
    end.

linux_cpuinfo_processor() ->
    case linux_cpuinfo_lookup("Processor") of
        [P] ->
            P;
        _ ->
            "-"
    end.

linux_which_cpuinfo(montavista) ->
    CPU =
        case linux_cpuinfo_cpu() of
            "-" ->
                throw(noinfo);
            Model ->
                case linux_cpuinfo_motherboard() of
                    "-" ->
                        Model;
                    MB ->
                        Model ++ " (" ++ MB ++ ")"
                end
        end,
    case linux_cpuinfo_bogomips() of
        "-" ->
            {ok, CPU};
        BMips ->
            {ok, {CPU, BMips}}
    end;

linux_which_cpuinfo(yellow_dog) ->
    CPU =
        case linux_cpuinfo_cpu() of
            "-" ->
                throw(noinfo);
            Model ->
                case linux_cpuinfo_motherboard() of
                    "-" ->
                        Model;
                    MB ->
                        Model ++ " (" ++ MB ++ ")"
                end
        end,
    {ok, CPU};

linux_which_cpuinfo(wind_river) ->
    CPU =
        case linux_cpuinfo_model() of
            "-" ->
                throw(noinfo);
            Model ->
                case linux_cpuinfo_platform() of
                    "-" ->
                        Model;
                    Platform ->
                        Model ++ " (" ++ Platform ++ ")"
                end
        end,
    case linux_cpuinfo_total_bogomips() of
        "-" ->
            {ok, CPU};
        BMips ->
            {ok, {CPU, BMips}}
    end;

%% Check for x86 (Intel or AMD)
linux_which_cpuinfo(other) ->
    CPU =
        case linux_cpuinfo_model_name() of
            "-" ->
                %% ARM (at least some distros...)
                case linux_cpuinfo_processor() of
                    "-" ->
			case linux_cpuinfo_model() of
			    "-" ->
				%% Ok, we give up
				throw(noinfo);
			    Model ->
				Model
			end;
                    Proc ->
                        Proc
                end;
            ModelName ->
                ModelName
        end,
    case linux_cpuinfo_bogomips() of
        "-" ->
            {ok, CPU};
        BMips ->
            {ok, {CPU, BMips}}
    end.

linux_meminfo_lookup(Key) when is_list(Key) ->
    linux_info_lookup(Key, "/proc/meminfo").

linux_meminfo_memtotal() ->
    case linux_meminfo_lookup("MemTotal") of
        [X] ->
            X;
        _ ->
            "-"
    end.
            
%% We *add* the value this return to the Factor.
linux_which_meminfo() ->
    case linux_meminfo_memtotal() of
        "-" ->
            0;
        MemTotal ->
            io:format("Memory:"
                      "~n   ~s"
                      "~n", [MemTotal]),
            case string:tokens(MemTotal, [$ ]) of
                [MemSzStr, MemUnit] ->
                    MemSz2 = list_to_integer(MemSzStr),
                    MemSz3 = 
                        case string:to_lower(MemUnit) of
                            "kb" ->
                                MemSz2;
                            "mb" ->
                                MemSz2*1024;
                            "gb" ->
                                MemSz2*1024*1024;
                            _ ->
                                throw(noinfo)
                        end,
                    if
                        (MemSz3 >= 8388608) ->
                            0;
                        (MemSz3 >= 4194304) ->
                            1;
                        (MemSz3 >= 2097152) ->
                            3;
                        true ->
                            5
                    end;
                _X ->
                    0
            end
    end.

%% Just to be clear: This is ***not*** scientific...
analyze_and_print_openbsd_host_info(Version) ->
    io:format("OpenBSD:"
              "~n   Version: ~p"
              "~n", [Version]),
    Extract =
        fun(Key) -> 
                string:tokens(string:trim(os:cmd("sysctl " ++ Key)), [$=])
        end,
    try
        begin
            CPU =
                case Extract("hw.model") of
                    ["hw.model", Model] ->
                        string:trim(Model);
                    _ ->
                        "-"
                end,
            CPUSpeed =
                case Extract("hw.cpuspeed") of
                    ["hw.cpuspeed", Speed] ->
                        list_to_integer(Speed);
                    _ ->
                        -1
                end,
            NCPU =
                case Extract("hw.ncpufound") of
                    ["hw.ncpufound", N] ->
                        list_to_integer(N);
                    _ ->
                        -1
                end,
            Memory =
                case Extract("hw.physmem") of
                    ["hw.physmem", PhysMem] ->
                        list_to_integer(PhysMem) div 1024;
                    _ ->
                        -1
                end,
            io:format("CPU:"
                      "~n   Model: ~s"
                      "~n   Speed: ~w"
                      "~n   N:     ~w"
                      "~nMemory:"
                      "~n   ~w KB"
                      "~n", [CPU, CPUSpeed, NCPU, Memory]),
            io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
            CPUFactor =
                if
                    (CPUSpeed =:= -1) ->
                        1;
                    (CPUSpeed >= 2000) ->
                        if
                            (NCPU >= 4) ->
                                1;
                            (NCPU >= 2) ->
                                2;
                            true ->
                                3
                        end;
                    true ->
                        if
                            (NCPU >= 4) ->
                                2;
                            (NCPU >= 2) ->
                                3;
                            true ->
                                4
                        end
                end,
            MemAddFactor =
                if
                    (Memory =:= -1) ->
                        0;
                    (Memory >= 8388608) ->
                        0;
                    (Memory >= 4194304) ->
                        1;
                    (Memory >= 2097152) ->
                        2;
                    true ->
                        3
                end,
            {CPUFactor + MemAddFactor, []}
        end
    catch
        _:_:_ ->
            io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
            {2, []}
    end.

analyze_and_print_freebsd_host_info(Version) ->
    io:format("FreeBSD:"
              "~n   Version: ~p"
              "~n", [Version]),
    %% This test require that the program 'sysctl' is in the path.
    %% First test with 'which sysctl', if that does not work
    %% try with 'which /sbin/sysctl'. If that does not work either,
    %% we skip the test...
    try
        begin
            SysCtl =
                case string:trim(os:cmd("which sysctl")) of
                    [] ->
                        case string:trim(os:cmd("which /sbin/sysctl")) of
                            [] ->
                                throw(sysctl);
                            SC2 ->
                                SC2
                        end;
                    SC1 ->
                        SC1
                end,
            Extract =
                fun(Key) ->
                        string:tokens(string:trim(os:cmd(SysCtl ++ " " ++ Key)),
                                      [$:])
                end,
            CPU      = analyze_freebsd_cpu(Extract),
            CPUSpeed = analyze_freebsd_cpu_speed(Extract),
            NCPU     = analyze_freebsd_ncpu(Extract),
            Memory   = analyze_freebsd_memory(Extract),
            io:format("CPU:"
                      "~n   Model:          ~s"
                      "~n   Speed:          ~w"
                      "~n   N:              ~w"
                      "~n   Num Schedulers: ~s"
                      "~nMemory:"
                      "~n   ~w KB"
                      "~n",
                      [CPU, CPUSpeed, NCPU, str_num_schedulers(), Memory]),
            io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
            CPUFactor =
                if
                    (CPUSpeed =:= -1) ->
                        1;
                    (CPUSpeed >= 2000) ->
                        if
                            (NCPU >= 4) ->
                                1;
                            (NCPU >= 2) ->
                                2;
                            true ->
                                3
                        end;
                    true ->
                        if
                            (NCPU =:= -1) ->
                                1;
                            (NCPU >= 4) ->
                                2;
                            (NCPU >= 2) ->
                                3;
                            true ->
                                4
                        end
                end,
            MemAddFactor =
                if
                    (Memory =:= -1) ->
                        0;
                    (Memory >= 8388608) ->
                        0;
                    (Memory >= 4194304) ->
                        1;
                    (Memory >= 2097152) ->
                        2;
                    true ->
                        3
                end,
            {CPUFactor + MemAddFactor, []}
        end
    catch
        _:_:_ ->
            io:format("CPU:"
                      "~n   Num Schedulers: ~s"
                      "~n", [str_num_schedulers()]),
            io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
            Factor = case erlang:system_info(schedulers) of
                         1 ->
                             10;
                         2 ->
                             5;
                         _ ->
                             2
                     end,
            {Factor, []}
    end.


analyze_freebsd_cpu(Extract) ->
    analyze_freebsd_item(Extract, "hw.model", fun(X) -> X end, "-").

analyze_freebsd_cpu_speed(Extract) ->
    analyze_freebsd_item(Extract,
                         "hw.clockrate",
                         fun(X) -> list_to_integer(X) end,
                         -1).

analyze_freebsd_ncpu(Extract) ->
    analyze_freebsd_item(Extract,
                         "hw.ncpu",
                         fun(X) -> list_to_integer(X) end,
                         -1).

analyze_freebsd_memory(Extract) ->
    analyze_freebsd_item(Extract,
                         "hw.physmem",
                         fun(X) -> list_to_integer(X) div 1024 end,
                         -1).

analyze_freebsd_item(Extract, Key, Process, Default) ->
    try
        begin
            case Extract(Key) of
                [Key, Model] ->
                    Process(string:trim(Model));
                _ ->
                    Default
            end
        end
    catch
        _:_:_ ->
            Default
    end.

analyze_and_print_netbsd_host_info(Version) ->
    io:format("NetBSD:"
              "~n   Version: ~p"
              "~n", [Version]),
    %% This test require that the program 'sysctl' is in the path.
    %% First test with 'which sysctl', if that does not work
    %% try with 'which /sbin/sysctl'. If that does not work either,
    %% we skip the test...
    try
        begin
            SysCtl =
                case string:trim(os:cmd("which sysctl")) of
                    [] ->
                        case string:trim(os:cmd("which /sbin/sysctl")) of
                            [] ->
                                throw(sysctl);
                            SC2 ->
                                SC2
                        end;
                    SC1 ->
                        SC1
                end,
            Extract =
                fun(Key) ->
                        [string:trim(S) ||
                            S <-
                                string:tokens(string:trim(os:cmd(SysCtl ++ " " ++ Key)),
                                              [$=])]
                end,
            CPU      = analyze_netbsd_cpu(Extract),
            Machine  = analyze_netbsd_machine(Extract),
            Arch     = analyze_netbsd_machine_arch(Extract),
            CPUSpeed = analyze_netbsd_cpu_speed(Extract),
            NCPU     = analyze_netbsd_ncpu(Extract),
            Memory   = analyze_netbsd_memory(Extract),
            io:format("CPU:"
                      "~n   Model:          ~s (~s, ~s)"
                      "~n   Speed:          ~w MHz"
                      "~n   N:              ~w"
                      "~n   Num Schedulers: ~w"
                      "~nMemory:"
                      "~n   ~w KB"
                      "~n",
                      [CPU, Machine, Arch, CPUSpeed, NCPU,
                       erlang:system_info(schedulers), Memory]),
            CPUFactor =
                if
                    (CPUSpeed =:= -1) ->
                        1;
                    (CPUSpeed >= 2000) ->
                        if
                            (NCPU >= 4) ->
                                1;
                            (NCPU >= 2) ->
                                2;
                            true ->
                                3
                        end;
                    true ->
                        if
                            (NCPU =:= -1) ->
                                1;
                            (NCPU >= 4) ->
                                2;
                            (NCPU >= 2) ->
                                3;
                            true ->
                                4
                        end
                end,
            MemAddFactor =
                if
                    (Memory =:= -1) ->
                        0;
                    (Memory >= 8388608) ->
                        0;
                    (Memory >= 4194304) ->
                        1;
                    (Memory >= 2097152) ->
                        2;
                    true ->
                        3
                end,
            {CPUFactor + MemAddFactor, []}
        end
    catch
        _:_:_ ->
            io:format("CPU:"
                      "~n   Num Schedulers: ~w"
                      "~n", [erlang:system_info(schedulers)]),
            Factor = case erlang:system_info(schedulers) of
                         1 ->
                             10;
                         2 ->
                             5;
                         _ ->
                             2
                     end,
            {Factor, []}
    end.

analyze_netbsd_cpu(Extract) ->
    analyze_netbsd_item(Extract, "hw.model", fun(X) -> X end, "-").

analyze_netbsd_machine(Extract) ->
    analyze_netbsd_item(Extract, "hw.machine", fun(X) -> X end, "-").

analyze_netbsd_machine_arch(Extract) ->
    analyze_netbsd_item(Extract, "hw.machine_arch", fun(X) -> X end, "-").

analyze_netbsd_cpu_speed(Extract) ->
    analyze_netbsd_item(Extract, "machdep.dmi.processor-frequency", 
                        fun(X) -> case string:tokens(X, [$\ ]) of
                                      [MHz, "MHz"] ->
                                          list_to_integer(MHz);
                                      _ ->
                                          -1
                                  end
                        end, "-").

analyze_netbsd_ncpu(Extract) ->
    analyze_netbsd_item(Extract,
                        "hw.ncpu",
                        fun(X) -> list_to_integer(X) end,
                        -1).

analyze_netbsd_memory(Extract) ->
    analyze_netbsd_item(Extract,
                        "hw.physmem64",
                        fun(X) -> list_to_integer(X) div 1024 end,
                        -1).

analyze_netbsd_item(Extract, Key, Process, Default) ->
    analyze_freebsd_item(Extract, Key, Process, Default).

%% Model Identifier: Macmini7,1
%%       Processor Name: Intel Core i5
%%       Processor Speed: 2,6 GHz
%%       Number of Processors: 1
%%       Total Number of Cores: 2
%%       L2 Cache (per Core): 256 KB
%%       L3 Cache: 3 MB
%%       Hyper-Threading Technology: Enabled
%%       Memory: 16 GB

analyze_and_print_darwin_host_info(Version) ->
    %% This stuff is for macOS.
    %% If we ever tested on a pure darwin machine,
    %% we need to find some other way to find some info...
    %% Also, I suppose its possible that we for some other
    %% reason *fail* to get the info...
    case analyze_darwin_software_info() of
        [] ->
            io:format("Darwin:"
                      "~n   Version:        ~s"
                      "~n   Num Schedulers: ~s"
                      "~n", [Version, str_num_schedulers()]),
            {num_schedulers_to_factor(), []};
        SwInfo when  is_list(SwInfo) ->
            SystemVersion = analyze_darwin_sw_system_version(SwInfo),
            KernelVersion = analyze_darwin_sw_kernel_version(SwInfo),
            HwInfo        = analyze_darwin_hardware_info(),
            ModelName     = analyze_darwin_hw_model_name(HwInfo),
            ModelId       = analyze_darwin_hw_model_identifier(HwInfo),
            ProcName      = analyze_darwin_hw_processor_name(HwInfo),
            ProcSpeed     = analyze_darwin_hw_processor_speed(HwInfo),
            NumProc       = analyze_darwin_hw_number_of_processors(HwInfo),
            NumCores      = analyze_darwin_hw_total_number_of_cores(HwInfo),
            Memory        = analyze_darwin_hw_memory(HwInfo),
            io:format("Darwin:"
                      "~n   System Version: ~s"
                      "~n   Kernel Version: ~s"
                      "~n   Model:          ~s (~s)"
                      "~n   Processor:      ~s (~s, ~s, ~s)"
                      "~n   Memory:         ~s"
                      "~n   Num Schedulers: ~s"
                      "~n", [SystemVersion, KernelVersion,
                             ModelName, ModelId,
                             ProcName, ProcSpeed, NumProc, NumCores, 
                             Memory,
                             str_num_schedulers()]),
            CPUFactor = analyze_darwin_cpu_to_factor(ProcName,
                                                     ProcSpeed,
                                                     NumProc,
                                                     NumCores),
            MemFactor = analyze_darwin_memory_to_factor(Memory),
            if (MemFactor =:= 1) ->
                    {CPUFactor, []};
               true ->
                    {CPUFactor + MemFactor, []}
            end
    end.

analyze_darwin_sw_system_version(SwInfo) ->
    proplists:get_value("system version", SwInfo, "-").

analyze_darwin_sw_kernel_version(SwInfo) ->
    proplists:get_value("kernel version", SwInfo, "-").

analyze_darwin_software_info() ->
    analyze_darwin_system_profiler("SPSoftwareDataType").

analyze_darwin_hw_model_name(HwInfo) ->
    proplists:get_value("model name", HwInfo, "-").

analyze_darwin_hw_model_identifier(HwInfo) ->
    proplists:get_value("model identifier", HwInfo, "-").

analyze_darwin_hw_processor_name(HwInfo) ->
    proplists:get_value("processor name", HwInfo, "-").

analyze_darwin_hw_processor_speed(HwInfo) ->
    proplists:get_value("processor speed", HwInfo, "-").

analyze_darwin_hw_number_of_processors(HwInfo) ->
    proplists:get_value("number of processors", HwInfo, "-").

analyze_darwin_hw_total_number_of_cores(HwInfo) ->
    proplists:get_value("total number of cores", HwInfo, "-").

analyze_darwin_hw_memory(HwInfo) ->
    proplists:get_value("memory", HwInfo, "-").

analyze_darwin_hardware_info() ->
    analyze_darwin_system_profiler("SPHardwareDataType").

%% This basically has the structure: "Key: Value"
%% But could also be (for example):
%%    "Something:" (which we ignore)
%%    "Key: Value1:Value2"
analyze_darwin_system_profiler(DataType) ->
    %% First, make sure the program actually exist:
    case os:cmd("which system_profiler") of
        [] ->
            [];
        _ ->
            D0 = os:cmd("system_profiler " ++ DataType),
            D1 = string:tokens(D0, [$\n]),
            D2 = [string:trim(S1) || S1 <- D1],
            D3 = [string:tokens(S2, [$:]) || S2 <- D2],
            analyze_darwin_system_profiler2(D3)
    end.

analyze_darwin_system_profiler2(L) ->
    analyze_darwin_system_profiler2(L, []).
    
analyze_darwin_system_profiler2([], Acc) ->
    [{string:to_lower(K), V} || {K, V} <- lists:reverse(Acc)];
analyze_darwin_system_profiler2([[_]|T], Acc) ->
    analyze_darwin_system_profiler2(T, Acc);
analyze_darwin_system_profiler2([[H1,H2]|T], Acc) ->
    analyze_darwin_system_profiler2(T, [{H1, string:trim(H2)}|Acc]);
analyze_darwin_system_profiler2([[H|TH0]|T], Acc) ->
    %% Some value parts has ':' in them, so put them together
    TH1 = colonize(TH0),
    analyze_darwin_system_profiler2(T, [{H, string:trim(TH1)}|Acc]).

%% This is only called if the length is at least 2
colonize([L1, L2]) ->
    L1 ++ ":" ++ L2;
colonize([H|T]) ->
    H ++ ":" ++ colonize(T).

%% The memory looks like this "<size> <unit>". Example: "2 GB" 
analyze_darwin_memory_to_factor(Mem) ->
    case [string:to_lower(S) || S <- string:tokens(Mem, [$\ ])] of
        [_SzStr, "tb"] ->
            1;
        [SzStr, "gb"] ->
            try list_to_integer(SzStr) of
                Sz when Sz < 2 ->
                    20;
                Sz when Sz < 4 ->
                    10;
                Sz when Sz < 8 ->
                    5;
                Sz when Sz < 16 ->
                    2;
                _ ->
                    1
            catch
                _:_:_ ->
                    20
            end;
        [_SzStr, "mb"] ->
            20;
        _ ->
            20
    end.

%% The speed is a string: "<speed> <unit>"
%% the speed may be a float, which we transforms into an integer of MHz.
%% To calculate a factor based on processor speed, number of procs
%% and number of cores is ... not an exact ... science ...
analyze_darwin_cpu_to_factor(_ProcName,
                             ProcSpeedStr, NumProcStr, NumCoresStr) ->
    Speed = 
        case [string:to_lower(S) || S <- string:tokens(ProcSpeedStr, [$\ ])] of
            [SpeedStr, "mhz"] ->
                try list_to_integer(SpeedStr) of
                    SpeedI ->
                        SpeedI
                catch
                    _:_:_ ->
                        try list_to_float(SpeedStr) of
                            SpeedF ->
                                trunc(SpeedF)
                        catch
                            _:_:_ ->
                                -1
                        end
                end;
            [SpeedStr, "ghz"] ->
                try list_to_float(SpeedStr) of
                    SpeedF ->
                        trunc(1000*SpeedF)
                catch
                    _:_:_ ->
                        try list_to_integer(SpeedStr) of
                            SpeedI ->
                                1000*SpeedI
                        catch
                            _:_:_ ->
                                -1
                        end
                end;
            _ ->
                -1
        end,
    NumProc = try list_to_integer(NumProcStr) of
                  NumProcI ->
                      NumProcI
              catch
                  _:_:_ ->
                      1
              end,
    NumCores = try list_to_integer(NumCoresStr) of
                   NumCoresI ->
                       NumCoresI
               catch
                   _:_:_ ->
                       1
               end,
    if
        (Speed > 3000) ->
            if
                (NumProc =:= 1) ->
                    if
                        (NumCores < 2) ->
                            5;
                        (NumCores < 4) ->
                            3;
                        (NumCores < 6) ->
                            2;
                        true ->
                            1
                    end;
                true ->
                    if
                        (NumCores < 4) ->
                            2;
                        true ->
                            1
                    end
            end;
        (Speed > 2000) ->
            if
                (NumProc =:= 1) ->
                    if
                        (NumCores < 2) ->
                            8;
                        (NumCores < 4) ->
                            5;
                        (NumCores < 6) ->
                            3;
                        true ->
                            1
                    end;
                true ->
                    if
                        (NumCores < 4) ->
                            5;
                        (NumCores < 8) ->
                            2;
                        true ->
                            1
                    end
            end;
        true ->
            if
                (NumProc =:= 1) ->
                    if
                        (NumCores < 2) ->
                            10;
                        (NumCores < 4) ->
                            7;
                        (NumCores < 6) ->
                            5;
                        (NumCores < 8) ->
                            3;
                        true ->
                            1
                    end;
                true ->
                    if
                        (NumCores < 4) ->
                            8;
                        (NumCores < 8) ->
                            4;
                        true ->
                            1
                    end
            end
    end.

analyze_and_print_solaris_host_info(Version) ->
    Release =
        case file:read_file_info("/etc/release") of
            {ok, _} ->
                case [string:trim(S) || S <- string:tokens(os:cmd("cat /etc/release"), [$\n])] of
                    [Rel | _] ->
                        Rel;
                    _ ->
                        "-"
                end;
            _ ->
                "-"
        end,
    %% Display the firmware device tree root properties (prtconf -b)
    Props = [list_to_tuple([string:trim(PS) || PS <- Prop]) ||
                Prop <- [string:tokens(S, [$:]) ||
                            S <- string:tokens(os:cmd("prtconf -b"), [$\n])]],
    BannerName = case lists:keysearch("banner-name", 1, Props) of
                     {value, {_, BN}} ->
                         string:trim(BN);
                     _ ->
                         "-"
                 end,
    InstructionSet =
        case string:trim(os:cmd("isainfo -k")) of
            "Pseudo-terminal will not" ++ _ ->
                "-";
            IS ->
                IS
        end,
    PtrConf = [list_to_tuple([string:trim(S) || S <- Items]) || Items <- [string:tokens(S, [$:]) || S <- string:tokens(os:cmd("prtconf"), [$\n])], length(Items) > 1],
    SysConf =
        case lists:keysearch("System Configuration", 1, PtrConf) of
            {value, {_, SC}} ->
                SC;
            _ ->
                "-"
        end,
    %% Because we count the lines of the output (which may contain
    %% any number of extra crap lines) we need to ensure we only
    %% count the "proper" stdout. So send it to a tmp file first
    %% and then count its number of lines...
    NumPhysCPU =
       try
            begin
                File1 = f("/tmp/psrinfo_p.~s.~w", [os:getpid(), os:system_time()]),
                os:cmd("psrinfo -p > " ++ File1),
                string:trim(os:cmd("cat " ++ File1))
            end
        catch
                _:_:_ ->
                    "-"
        end,
    %% Because we count the lines of the output (which may contain
    %% any number of extra crap lines) we need to ensure we only
    %% count the "proper" stdout. So send it to a tmp file first
    %% and then count its number of lines...
    NumVCPU =
        try
            begin
                File2 = f("/tmp/psrinfo.~s.~w", [os:getpid(), os:system_time()]),
                os:cmd("psrinfo > " ++ File2),
                [NumVCPUStr | _] = string:tokens(os:cmd("wc -l " ++ File2), [$\ ]),
                NumVCPUStr
            end
        catch
            _:_:_ ->
                "-"
        end,
    MemSz =
        case lists:keysearch("Memory size", 1, PtrConf) of
            {value, {_, MS}} ->
                MS;
            _ ->
                "-"
        end,
    io:format("Solaris: ~s"
              "~n   Release:         ~s"
              "~n   Banner Name:     ~s"
              "~n   Instruction Set: ~s"
              "~n   CPUs:            ~s (~s)"
              "~n   System Config:   ~s"
              "~n   Memory Size:     ~s"
              "~n   Num Schedulers:  ~s"
              "~n~n", [Version, Release, BannerName, InstructionSet,
                       NumPhysCPU, NumVCPU,
                       SysConf, MemSz,
                       str_num_schedulers()]),
    io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
    MemFactor =
        try string:tokens(MemSz, [$ ]) of
            [SzStr, "Mega" ++ _] ->
                try list_to_integer(SzStr) of
                    Sz when Sz > 8192 ->
                        0;
                    Sz when Sz > 4096 ->
                        1;
                    Sz when Sz > 2048 ->
                        2;
                    _ -> 
                        5
                catch
                    _:_:_ ->
                        10
                end;
            [SzStr, "Giga" ++ _] ->
                try list_to_integer(SzStr) of
                    Sz when Sz > 8 ->
                        0;
                    Sz when Sz > 4 ->
                        1;
                    Sz when Sz > 2 ->
                        2;
                    _ -> 
                        5
                catch
                    _:_:_ ->
                        10
                end;
            _ ->
                10
        catch
            _:_:_ ->
                10
        end,
    {try erlang:system_info(schedulers) of
         1 ->
             10;
         2 ->
             5;
         N when (N =< 6) ->
             2;
         _ ->
             1
     catch
         _:_:_ ->
             10
     end + MemFactor, []}.    

analyze_and_print_win_host_info(Version) ->
    SysInfo    = which_win_system_info(),
    OsName     = win_sys_info_lookup(os_name,             SysInfo),
    OsVersion  = win_sys_info_lookup(os_version,          SysInfo),
    SysMan     = win_sys_info_lookup(system_manufacturer, SysInfo),
    SysMod     = win_sys_info_lookup(system_model,        SysInfo),
    NumProcs   = win_sys_info_lookup(num_processors,      SysInfo),
    TotPhysMem = win_sys_info_lookup(total_phys_memory,   SysInfo),
    io:format("Windows: ~s"
              "~n   OS Version:             ~s (~p)"
              "~n   System Manufacturer:    ~s"
              "~n   System Model:           ~s"
              "~n   Number of Processor(s): ~s"
              "~n   Total Physical Memory:  ~s"
              "~n   Num Schedulers:         ~s"
              "~n~n", [OsName, OsVersion, Version,
                       SysMan, SysMod, NumProcs, TotPhysMem,
                       str_num_schedulers()]),
    io:format("TS Scale Factor: ~w~n", [timetrap_scale_factor()]),
    MemFactor =
        try
            begin
                [MStr, MUnit|_] =
                    string:tokens(lists:delete($,, TotPhysMem), [$\ ]),
                case string:to_lower(MUnit) of
                    "gb" ->
                        try list_to_integer(MStr) of
                            M when M > 8 ->
                                0;
                            M when M > 4 ->
                                1;
                            M when M > 2 ->
                                2;
                            _ -> 
                                5
                        catch
                            _:_:_ ->
                                10
                        end;
                    "mb" ->
                        try list_to_integer(MStr) of
                            M when M > 8192 ->
                                0;
                            M when M > 4096 ->
                                1;
                            M when M > 2048 ->
                                2;
                            _ -> 
                                5
                        catch
                            _:_:_ ->
                                10
                        end;
                    _ ->
                        10
                end
            end
        catch
            _:_:_ ->
                10
        end,
    CPUFactor = 
        case erlang:system_info(schedulers) of
            1 ->
                10;
            2 ->
                5;
            _ ->
                2
        end,
    {CPUFactor + MemFactor, SysInfo}.

win_sys_info_lookup(Key, SysInfo) ->
    win_sys_info_lookup(Key, SysInfo, "-").

win_sys_info_lookup(Key, SysInfo, Def) ->
    case lists:keysearch(Key, 1, SysInfo) of
        {value, {Key, Value}} ->
            Value;
        false ->
            Def
    end.

%% This function only extracts the prop we actually care about!
which_win_system_info() ->
    F = fun() ->
                try
                    begin
                        SysInfo = os:cmd("systeminfo"),
                        process_win_system_info(
                          string:tokens(SysInfo, [$\r, $\n]), [])
                    end
                catch
                    C:E:S ->
                        io:format("Failed get or process System info: "
                                  "   Error Class: ~p"
                                  "   Error:       ~p"
                                  "   Stack:       ~p"
                                  "~n", [C, E, S]),
                        []
                end
        end,
    proxy_call(F, timer:minutes(1), []).

process_win_system_info([], Acc) ->
    Acc;
process_win_system_info([H|T], Acc) ->
    case string:tokens(H, [$:]) of
        [Key, Value] ->
            case string:to_lower(Key) of
                "os name" ->
                    process_win_system_info(T,
                                            [{os_name, string:trim(Value)}|Acc]);
                "os version" ->
                    process_win_system_info(T,
                                            [{os_version, string:trim(Value)}|Acc]);
                "system manufacturer" ->
                    process_win_system_info(T,
                                            [{system_manufacturer, string:trim(Value)}|Acc]);
                "system model" ->
                    process_win_system_info(T,
                                            [{system_model, string:trim(Value)}|Acc]);
                "processor(s)" ->
                    [NumProcStr|_] = string:tokens(Value, [$\ ]),
                    T2 = lists:nthtail(list_to_integer(NumProcStr), T),
                    process_win_system_info(T2,
                                            [{num_processors, NumProcStr}|Acc]);
                "total physical memory" ->
                    process_win_system_info(T,
                                            [{total_phys_memory, string:trim(Value)}|Acc]);
                _ ->
                    process_win_system_info(T, Acc)
            end;
        _ ->
            process_win_system_info(T, Acc)
    end.

str_num_schedulers() ->
    try erlang:system_info(schedulers) of
        N -> f("~w", [N])
    catch
        _:_:_ -> "-"
    end.

num_schedulers_to_factor() ->
    try erlang:system_info(schedulers) of
        1 ->
            10;
        2 ->
            5;
        N when (N =< 6) ->
            2;
        _ ->
            1
    catch
        _:_:_ ->
            10
    end.


linux_info_lookup(Key, File) ->
    LKey = string:to_lower(Key),
    try [string:trim(S) || S <- string:tokens(os:cmd("grep -i " ++ "\"" ++ LKey ++ "\"" ++ " " ++ File), [$:,$\n])] of
        Info ->
            linux_info_lookup_collect(LKey, Info, [])
    catch
        _:_:_ ->
            "-"
    end.

linux_info_lookup_collect(_Key, [], Values) ->
    lists:reverse(Values);
linux_info_lookup_collect(Key, [Key, Value|Rest], Values) ->
    linux_info_lookup_collect(Key, Rest, [Value|Values]);
linux_info_lookup_collect(Key1, [Key2, Value|Rest], Values) ->
    case string:to_lower(Key2) of
	Key1 ->
	    linux_info_lookup_collect(Key1, Rest, [Value|Values]);
	_ ->
	    lists:reverse(Values)
    end;
linux_info_lookup_collect(_, _, Values) ->
    lists:reverse(Values).

maybe_skip(HostInfo) ->

    %% We have some crap machines that causes random test case failures
    %% for no obvious reason. So, attempt to identify those without actually
    %% checking for the host name...

    LinuxVersionVerify =
        fun(V) when (V > {3,6,11}) ->
                false; % OK - No skip
           (V) when (V =:= {3,6,11}) ->
                case string:trim(os:cmd("cat /etc/issue")) of
                    "Fedora release 16 " ++ _ -> % Stone age Fedora => Skip
                        true;
                    _ ->
                        false
                end;
           (V) when (V =:= {3,4,20}) ->
                case string:trim(os:cmd("cat /etc/issue")) of
                    "Wind River Linux 5.0.1.0" ++ _ -> % *Old* Wind River => skip
                        true;
                    _ ->
                        false
                end;
           (V) when (V =:= {2,6,32}) ->
                case string:trim(os:cmd("cat /etc/issue")) of
                    "Debian GNU/Linux 6.0 " ++ _ -> % Stone age Debian => Skip
                        true;
                    _ ->
                        false
                end;
           (V) when (V > {2,6,24}) ->
                false; % OK - No skip
           (V) when (V =:= {2,6,10}) ->
                case string:trim(os:cmd("cat /etc/issue")) of
                    "MontaVista" ++ _ -> % Stone age MontaVista => Skip
                        %% The real problem is that the machine is *very* slow
                        true;
                    _ ->
                        false
                end;
           (_) ->
                %% We are specifically checking for
                %% a *really* old gento...
                case string:find(string:strip(os:cmd("uname -a")), "gentoo") of
                    nomatch ->
                        false;
                    _ -> % Stone age gentoo => Skip
                        true
                end
        end,
    DarwinVersionVerify =
        fun(V) when (V > {9, 8, 0}) ->
                %% This version is OK: No Skip
                false;
           (_V) ->
                %% This version is *not* ok: Skip
                true
        end,
    SkipWindowsOnVirtual =
        fun() ->
                SysMan = win_sys_info_lookup(system_manufacturer, HostInfo),
                case string:to_lower(SysMan) of
                    "vmware" ++ _ ->
                        true;
                    _ ->
                        false
                end
        end,
    COND = [{unix,  [{linux, LinuxVersionVerify}, 
                     {darwin, DarwinVersionVerify}]},
            {win32, SkipWindowsOnVirtual}],
    os_cond_skip(COND).

os_cond_skip(any) ->
    true;
os_cond_skip(Skippable) when is_list(Skippable) ->
    os_cond_skip(Skippable, os:type());
os_cond_skip(_Crap) ->
    false.

os_cond_skip(Skippable, {OsFam, OsName}) ->
    os_cond_skip(Skippable, OsFam, OsName);
os_cond_skip(Skippable, OsFam) ->
    os_cond_skip(Skippable, OsFam, undefined).

os_cond_skip(Skippable, OsFam, OsName) -> 
    %% Check if the entire family is to be skipped
    %% Example: [win32, unix]
    case lists:member(OsFam, Skippable) of
        true ->
            true;
        false ->
            %% Example: [{unix, freebsd}] | [{unix, [freebsd, darwin]}]
            case lists:keysearch(OsFam, 1, Skippable) of
                {value, {OsFam, OsName}} ->
                    true;
                {value, {OsFam, OsNames}} when is_list(OsNames) ->
                    %% OsNames is a list of: 
                    %%    [atom()|{atom(), function/0 | function/1}]
                    case lists:member(OsName, OsNames) of
                        true ->
                            true;
                        false ->
                            os_cond_skip_check(OsName, OsNames)
                    end;
                {value, {OsFam, Check}} when is_function(Check, 0) ->
                    Check();
                {value, {OsFam, Check}} when is_function(Check, 1) ->
                    Check(os:version());
                _ ->
                    false
            end
    end.

%% Performs a check via a provided fun with arity 0 or 1.
%% The argument is the result of os:version().
os_cond_skip_check(OsName, OsNames) ->
    case lists:keysearch(OsName, 1, OsNames) of
        {value, {OsName, Check}} when is_function(Check, 0) ->
            Check();
        {value, {OsName, Check}} when is_function(Check, 1) ->
            Check(os:version());
        _ ->
            false
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_tc_name(N) when is_atom(N) ->
    set_tc_name(atom_to_list(N));
set_tc_name(N) when is_list(N) ->
    put(tc_name, N).

%% get_tc_name() ->
%%     get(tc_name).

tc_begin(TC) ->
    OldVal = process_flag(trap_exit, true),
    put(old_trap_exit, OldVal),
    set_tc_name(TC),
    tc_print("begin ***",
             "~n----------------------------------------------------~n", "").
    
tc_end(Result) when is_list(Result) ->
    OldVal = erase(old_trap_exit),
    process_flag(trap_exit, OldVal),
    tc_print("done: ~s", [Result], 
             "", "----------------------------------------------------~n~n"),
    ok.

%% *** tc_try/2,3 ***
%% Case: Basically the test case name
%% Cond: A fun that is evaluated before the actual test case
%%       The point of this is that it can performs checks to
%%       see if we shall run the test case at all.
%%       For instance, the test case may only work in specific
%%       conditions.
%% TC:   The test case fun
tc_try(Case, Cond, TC) 
  when is_atom(Case) andalso
       is_function(Cond, 0) andalso
       is_function(TC, 0) ->
    tc_begin(Case),
    try Cond() of
        ok ->
            try 
                begin
                    TC(),
                    ?SLEEP(?SECS(1)),
                    tc_end("ok")
                end
            catch
                C:{skip, _} = SKIP when ((C =:= throw) orelse (C =:= exit)) ->
                    %% i("catched[tc] (skip): "
                    %%   "~n   C:    ~p"
                    %%   "~n   SKIP: ~p"
                    %%   "~n", [C, SKIP]),
                    tc_end( f("skipping(catched,~w,tc)", [C]) ),
                    SKIP;
                C:E:S ->
                    %% i("catched[tc]: "
                    %%   "~n   C: ~p"
                    %%   "~n   E: ~p"
                    %%   "~n   S: ~p"
                    %%    "~n", [C, E, S]),
                    tc_end( f("failed(catched,~w,tc)", [C]) ),
                    erlang:raise(C, E, S)
            end;
        {skip, _} = SKIP ->
            tc_end("skipping(tc)"),
            SKIP;
        {error, Reason} ->
            tc_end("failed(tc)"),
            exit({tc_cond_failed, Reason})
    catch
        C:{skip, _} = SKIP when ((C =:= throw) orelse (C =:= exit)) ->
            %% i("catched[cond] (skip): "
            %%   "~n   C:    ~p"
            %%   "~n   SKIP: ~p"
            %%   "~n", [C, SKIP]),
            tc_end( f("skipping(catched,~w,cond)", [C]) ),
            SKIP;
        C:E:S ->
            %% i("catched[cond]: "
            %%   "~n   C: ~p"
            %%   "~n   E: ~p"
            %%   "~n   S: ~p"
            %%   "~n", [C, E, S]),
            tc_end( f("failed(catched,~w,cond)", [C]) ),
            erlang:raise(C, E, S)
    end.


tc_print(F, Before, After) ->
    tc_print(F, [], Before, After).

tc_print(F, A, Before, After) ->
    Name = tc_which_name(),
    FStr = f("*** [~s][~s][~p] " ++ F ++ "~n", 
             [formated_timestamp(),Name,self()|A]),
    io:format(user, Before ++ FStr ++ After, []).

tc_which_name() ->
    case get(tc_name) of
        undefined ->
            case get(sname) of
                undefined ->
                    "";
                SName when is_list(SName) ->
                    SName
            end;
        Name when is_list(Name) ->
            Name
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timetrap_scale_factor() ->
    case (catch test_server:timetrap_scale_factor()) of
        {'EXIT', _} ->
            1;
        N ->
            N
    end.


proxy_call(F, Timeout, Default)
  when is_function(F, 0) andalso is_integer(Timeout) andalso (Timeout > 0) ->
    {P, M} = erlang:spawn_monitor(fun() -> exit(F()) end),
    receive
        {'DOWN', M, process, P, Reply} ->
            Reply
    after Timeout ->
            erlang:demonitor(M, [flush]),
            exit(P, kill),
            Default
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp({_N1, _N2, N3} = TS) ->
    {_Date, Time}   = calendar:now_to_local_time(TS),
    {Hour, Min, Sec} = Time,
    FormatTS = io_lib:format("~.2.0w:~.2.0w:~.2.0w.~.3.0w",
                             [Hour, Min, Sec, N3 div 1000]),  
    lists:flatten(FormatTS).

print(F) ->
    print(F, []).

print(F, A) ->
    io:format("~s ~p " ++ F ++ "~n", [formated_timestamp(), self() | A]).
