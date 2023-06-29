%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2020-2023. All Rights Reserved.
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

-module(kernel_test_lib).

-export([init_per_suite/1,
         end_per_suite/1]).
-export([tc_try/2, tc_try/3, tc_try/4, tc_try/5]).
-export([socket_type/1,
	 listen/3,
         connect/4, connect/5,
         open/3,
         is_socket_backend/1,
         inet_backend_opts/1,
         explicit_inet_backend/0,
         test_inet_backends/0,
         which_inet_backend/1]).
-export([start_node/2, start_node/3,
         stop_node/1]).
-export([f/2,
         print/1, print/2,
         formated_timestamp/0]).
-export([good_hosts/1,
         lookup/3]).
-export([os_cmd/1, os_cmd/2]).

-export([
         proxy_call/3,

         %% Generic 'has support' test function(s)
         is_socket_supported/0,
         has_support_ipv4/0,
         has_support_ipv6/0,
	 has_support_unix_domain_socket/0,

         which_local_host_info/1, which_local_host_info/2,
         which_local_addr/1, which_link_local_addr/1,

         %% Skipping
         not_yet_implemented/0,
         skip/1
        ]).

%% Convenient exports...
-export([analyze_and_print_host_info/0]).

-include("kernel_test_lib.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite([{allow_skip, Allow}|Config]) ->
    init_per_suite(Allow, Config);
init_per_suite(Config) ->
    init_per_suite(true, Config).

init_per_suite(AllowSkip, Config) when is_boolean(AllowSkip) ->

    print("kernel environment: "
          "~n   (kernel) app:  ~p"
          "~n   (all)    init: ~p"
          "~n   (kernel) init: ~p",
          [application:get_all_env(kernel),
           init:get_arguments(),
           case init:get_argument(kernel) of
               {ok, Args} -> Args;
               error -> undefined
           end]),

    ct:timetrap(timer:minutes(2)),

    try analyze_and_print_host_info() of
        {Factor, HostInfo} when (AllowSkip =:= true) andalso
                                is_integer(Factor) ->
            try maybe_skip(HostInfo) of
                true ->
                    {skip, "Unstable host and/or os (or combo thererof)"};
                false ->
                    print("try start (global) system monitor"),
                    case kernel_test_global_sys_monitor:start() of
                        {ok, _} ->
                            print("(global) system monitor started"),
                            case lists:keysearch(label, 1, HostInfo) of
                                {value, Label} ->
                                    [{kernel_factor, Factor}, Label | Config];
                                false ->
                                    [{kernel_factor, Factor} | Config]
                            end;
                        {error, Reason} ->
                            print("Failed start (global) system monitor:"
                                  "~n      ~p", [Reason]),
                            {skip, "Failed start (global) system monitor"}
                    end
            catch
                throw:{skip, _} = SKIP ->
                    SKIP
            end;

        {Factor, HostInfo} when (AllowSkip =:= false) andalso
                                 is_integer(Factor) ->
            print("try start (global) system monitor"),
            case kernel_test_global_sys_monitor:start() of
                {ok, _} ->
                    print("(global) system monitor started"),
                    case lists:keysearch(label, 1, HostInfo) of
                        {value, Label} ->
                            [{kernel_factor, Factor}, Label | Config];
                        false ->
                            [{kernel_factor, Factor} | Config]
                    end;
                {error, Reason} ->
                    print("Failed start (global) system monitor:"
                          "~n      ~p", [Reason]),
                    [{kernel_factor, Factor} | Config]
            end
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.


end_per_suite(Config) when is_list(Config) ->
    kernel_test_global_sys_monitor:stop(),
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
                      "~n   OS Type:               ~p"
                      "~n   Version:               ~p"
                      "~n   Num Online Schedulers: ~s"
                      "~n", [OsFam, OsName, Version, str_num_schedulers()]),
            {num_schedulers_to_factor(), []}
    end.

linux_which_distro(Version) ->
    try do_linux_which_distro(Version)
    catch
        throw:{distro, DistroAndLabel} ->
            DistroAndLabel
    end.

do_linux_which_distro(Version) ->
    Label = ts_extra_platform_label(),

    %% Many (linux) distro's use the /etc/issue file, so try that first.
    %% Then we just keep going until we are "done".
    DistroStr = do_linux_which_distro_issue(Version, Label),

    %% Still not sure; try fedora
    _ = do_linux_which_distro_fedora(Version, Label),

    %% Still not sure; try suse
    _ = do_linux_which_distro_suse(Version, Label),

    %% Still not sure; try os-release
    _ = do_linux_which_distro_os_release(Version, Label),

    %% And the fallback
    io:format("Linux: ~s"
              "~n   Distro:       ~s"
              "~n   Label:        ~s"
              "~n   Product Name: ~s"
              "~n",
              [Version, DistroStr, Label,
               linux_product_name()]),
    {other, simplify_label(Label)}.

do_linux_which_distro_issue(Version, Label) ->
    case file:read_file_info("/etc/issue") of
        {ok, _} ->
            case [string:trim(S) ||
                     S <- string:tokens(os:cmd("cat /etc/issue"), [$\n])] of
                [DistroStr | _] ->
                    case DistroStr of
                        "Wind River Linux" ++ _ ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, DistroStr, Label,
                                       linux_product_name()]),
                            throw({distro,
                                   {wind_river, simplify_label(Label)}});
                        "MontaVista" ++ _ ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, DistroStr, Label,
                                       linux_product_name()]),
                            throw({distro,
                                   {montavista, simplify_label(Label)}});
                        "Yellow Dog" ++ _ ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, DistroStr, Label,
                                       linux_product_name()]),
                            throw({distro,
                                   {yellow_dog, simplify_label(Label)}});
                        "Ubuntu" ++ _ ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, DistroStr, Label,
                                       linux_product_name()]),
                            throw({distro,
                                   {ubuntu, simplify_label(Label)}});
                        "Linux Mint" ++ _ ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, DistroStr, Label,
                                       linux_product_name()]),
                            throw({distro,
                                   {linux_mint, simplify_label(Label)}});
                        _ ->
                            DistroStr
                    end;
                X ->
                    X
            end;
        _ ->
            "Unknown"
    end.
                            
do_linux_which_distro_fedora(Version, Label) ->
    %% Check if fedora
    case file:read_file_info("/etc/fedora-release") of
        {ok, _} ->
            case [string:trim(S) ||
                     S <- string:tokens(os:cmd("cat /etc/fedora-release"),
                                        [$\n])] of
                [DistroStr | _] ->
                    io:format("Linux: ~s"
                              "~n   Distro:                  ~s"
                              "~n   TS Extra Platform Label: ~s"
                              "~n   Product Name:            ~s"
                              "~n",
                              [Version, DistroStr, Label,
                               linux_product_name()]);
                _ ->
                    io:format("Linux: ~s"
                              "~n   Distro:                  ~s"
                              "~n   TS Extra Platform Label: ~s"
                              "~n   Product Name:            ~s"
                              "~n",
                              [Version, "Fedora", Label,
                               linux_product_name()])
            end,
            throw({distro, {fedora, simplify_label(Label)}});
        _ ->
            ignore
    end.

do_linux_which_distro_suse(Version, Label) ->
    %% Check if its a SuSE
    case file:read_file_info("/etc/SUSE-brand") of
        {ok, _} ->
            case file:read_file_info("/etc/SuSE-release") of
                {ok, _} ->
                    case [string:trim(S) ||
                             S <- string:tokens(os:cmd("cat /etc/SuSE-release"),
                                                [$\n])] of
                        ["SUSE Linux Enterprise Server" ++ _ = DistroStr | _] ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, DistroStr, Label,
                                       linux_product_name()]),
                            throw({distro, {sles, simplify_label(Label)}});
                        [DistroStr | _] ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, DistroStr, Label,
                                       linux_product_name()]),
                            throw({distro, {suse, simplify_label(Label)}});
                        _ ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, "SuSE", Label,
                                       linux_product_name()]),
                            throw({distro, {suse, simplify_label(Label)}})
                    end;
                _ ->
                    case string:tokens(os:cmd("cat /etc/SUSE-brand"), [$\n]) of
                        ["SLE" = DistroStr, VERSION | _] ->
                            case [string:strip(S) ||
                                     S <- string:tokens(VERSION, [$=])] of
                                ["VERSION", VersionNo] ->
                                    io:format("Linux: ~s"
                                              "~n   Distro:                  ~s"
                                              "~n   Distro Version:          ~s"
                                              "~n   TS Extra Platform Label: ~s"
                                              "~n   Product Name:            ~s"
                                              "~n",
                                              [Version,
                                               DistroStr, VersionNo,
                                               Label,
                                               linux_product_name()]),
                                    throw({distro,
                                           {sles, simplify_label(Label)}});
                                _ ->
                                    io:format("Linux: ~s"
                                              "~n   Distro:                  ~s"
                                              "~n   TS Extra Platform Label: ~s"
                                              "~n   Product Name:            ~s"
                                              "~n",
                                              [Version, DistroStr, Label,
                                               linux_product_name()]),
                                    throw({distro,
                                           {sles, simplify_label(Label)}})
                            end;
                        ["openSUSE" = DistroStr, VERSION | _] ->
                            case [string:strip(S) ||
                                     S <- string:tokens(VERSION, [$=])] of
                                ["VERSION", VersionNo] ->
                                    io:format("Linux: ~s"
                                              "~n   Distro:                  ~s"
                                              "~n   Distro Version:          ~s"
                                              "~n   TS Extra Platform Label: ~s"
                                              "~n   Product Name:            ~s"
                                              "~n",
                                              [Version,
                                               DistroStr, VersionNo,
                                               Label,
                                               linux_product_name()]),
                                    throw({distro,
                                           {suse, simplify_label(Label)}});
                                _ ->
                                    io:format("Linux: ~s"
                                              "~n   Distro:                  ~s"
                                              "~n   TS Extra Platform Label: ~s"
                                              "~n   Product Name:            ~s"
                                              "~n",
                                              [Version, DistroStr, Label,
                                               linux_product_name()]),
                                    throw({distro,
                                           {suse, simplify_label(Label)}})
                            end;
                        _ ->
                            io:format("Linux: ~s"
                                      "~n   Distro:                  ~s"
                                      "~n   TS Extra Platform Label: ~s"
                                      "~n   Product Name:            ~s"
                                      "~n",
                                      [Version, "Unknown SUSE", Label,
                                       linux_product_name()]),
                            throw({distro, {suse, simplify_label(Label)}})
                    end
            end;
        _ ->
            ignore
    end.

do_linux_which_distro_os_release(Version, Label) ->
    case file:read_file_info("/etc/os-release") of
        {ok, _} ->
            %% We want to 'catch' if our processing is wrong,
            %% that's why we catch and re-throw the distro.
            %% Actual errors will be returned as 'ignore'.
            try
                begin
                    Info = linux_process_os_release(),
                    {value, {_, DistroStr}} = lists:keysearch(name, 1, Info),
                    {value, {_, VersionNo}} = lists:keysearch(version, 1, Info),
                    io:format("Linux: ~s"
                              "~n   Distro:                  ~s"
                              "~n   Distro Version:          ~s"
                              "~n   TS Extra Platform Label: ~s"
                              "~n   Product Name:            ~s"
                              "~n",
                              [Version, DistroStr, VersionNo, Label,
                               linux_product_name()]),
                    throw({distro,
                           {linux_distro_str_to_distro_id(DistroStr),
                            simplify_label(Label)}})
                end
            catch
                throw:{distro, _} = DISTRO ->
                    throw(DISTRO);
                _:_ ->
                    ignore
            end;
        _ ->
            ignore
    end.

linux_process_os_release() ->
    %% Read the "raw" file
    Raw = os:cmd("cat /etc/os-release"),
    %% Split it into lines
    Lines1 = string:tokens(Raw, [$\n]),
    %% Just in case, skip any lines starting with '#'.
    Lines2 = linux_process_os_release1(Lines1),
    %% Each (remaining) line *should* be: <TAG>=<VALUE>
    %% Both sides will be strings, the value side will be a quoted string...
    %% Convert those into a 2-tuple list: [{Tag, Value}]
    linux_process_os_release2(Lines2).

linux_process_os_release1(Lines) ->
    linux_process_os_release1(Lines, []).

linux_process_os_release1([], Acc) ->
    lists:reverse(Acc);
linux_process_os_release1([H|T], Acc) ->
    case H of
        "#" ++ _ ->
            linux_process_os_release1(T, Acc);
        _ ->
            linux_process_os_release1(T, [H|Acc])
    end.

linux_process_os_release2(Lines) ->
    linux_process_os_release2(Lines, []).

linux_process_os_release2([], Acc) ->
    lists:reverse(Acc);
linux_process_os_release2([H|T], Acc) ->
    case linux_process_os_release3(H) of
        {value, Value} ->
            linux_process_os_release2(T, [Value|Acc]);
        false ->
            linux_process_os_release2(T, Acc)
    end.

linux_process_os_release3(H) ->
    case [string:strip(S) || S <- string:tokens(H, [$=])] of
        [Tag, Value] ->
            Tag2   = list_to_atom(string:to_lower(Tag)),
            Value2 = string:strip(Value, both, $"),
            linux_process_os_release4(Tag2, Value2);
        _ ->
            false
    end.

linux_process_os_release4(name = Tag, Value) ->
    {value, {Tag, Value}};
linux_process_os_release4(version = Tag, Value) ->
    {value, {Tag, Value}};
linux_process_os_release4(version_id = Tag, Value) ->
    {value, {Tag, Value}};
linux_process_os_release4(id = Tag, Value) ->
    {value, {Tag, Value}};
linux_process_os_release4(pretty_name = Tag, Value) ->
    {value, {Tag, Value}};
linux_process_os_release4(_Tag, _Value) ->
    false.

linux_distro_str_to_distro_id("Debian" ++ _) ->
    debian;
linux_distro_str_to_distro_id("Fedora" ++ _) ->
    fedora;
linux_distro_str_to_distro_id("Linux Mint" ++ _) ->
    linux_mint;
linux_distro_str_to_distro_id("MontaVista" ++ _) ->
    montavista;
linux_distro_str_to_distro_id("openSUSE" ++ _) ->
    suse;
linux_distro_str_to_distro_id("SLES" ++ _) ->
    sles;
linux_distro_str_to_distro_id("Ubuntu" ++ _) ->
    ubuntu;
linux_distro_str_to_distro_id("Wind River Linux" ++ _) ->
    wind_river;
linux_distro_str_to_distro_id("Yellow Dog" ++ _) ->
    yellow_dog;
linux_distro_str_to_distro_id(X) ->
    X.


analyze_and_print_linux_host_info(Version) ->
    {Distro, Label} =
        case file:read_file_info("/etc/issue") of
            {ok, _} ->
                linux_which_distro(Version);
            _ ->
                L = ts_extra_platform_label(),
                io:format("Linux: ~s"
                          "~n   TS Extra Platform Label: ~s"
                          "~n", [Version, L]),
                {other, simplify_label(L)}
        end,
    %% 'VirtFactor' will be 0 unless virtual
    VirtFactor = linux_virt_factor(),
    Factor =
        case (catch linux_which_cpuinfo(Distro)) of
            {ok, {CPU, BogoMIPS}} ->
                io:format("CPU: "
                          "~n   Model:                 ~s"
                          "~n   BogoMIPS:              ~w"
                          "~n   Num Online Schedulers: ~s"
                          "~n", [CPU, BogoMIPS, str_num_schedulers()]),
                if
                    (BogoMIPS > 50000) ->
                        1;
                    (BogoMIPS > 40000) ->
                        2;
                    (BogoMIPS > 30000) ->
                        3;
                    (BogoMIPS > 20000) ->
                        4;
                    (BogoMIPS > 10000) ->
                        5;
                    (BogoMIPS > 5000) ->
                        8;
                    (BogoMIPS > 3000) ->
                        12;
                    true ->
                        10
                end;
            {ok, "POWER9" ++ _ = CPU} ->
                %% For some reason this host is really slow
                %% Consider the CPU, it really should not be...
                %% But, to not fail a bunch of test cases, we add 5
                case linux_cpuinfo_clock() of
                    Clock when is_integer(Clock) andalso (Clock > 0) ->
                        io:format("CPU: "
                                  "~n   Model:                 ~s"
                                  "~n   CPU Speed:             ~w"
                                  "~n   Num Online Schedulers: ~s"
                                  "~n", [CPU, Clock, str_num_schedulers()]),
                        if
                            (Clock > 2000) ->
                                5 + num_schedulers_to_factor();
                            true ->
                                10 + num_schedulers_to_factor()
                        end;
                    _ ->
                        num_schedulers_to_factor()
                end;
            {ok, CPU} ->
                io:format("CPU: "
                          "~n   Model:                 ~s"
                          "~n   Num Online Schedulers: ~s"
                          "~n", [CPU, str_num_schedulers()]),
                num_schedulers_to_factor();
            _ ->
                5
        end,
    AddLabelFactor = label2factor(Label),
    %% Check if we need to adjust the factor because of the memory
    AddMemFactor = try linux_which_meminfo()
                   catch _:_:_ -> 0
                   end,
    TSScaleFactor = case timetrap_scale_factor() of
                        N when is_integer(N) andalso (N > 0) ->
                            N - 1;
                        _ ->
                            0
                    end,
    io:format("Factor calc:"
              "~n      Base Factor:     ~w"
              "~n      Label Factor:    ~w"
              "~n      Mem Factor:      ~w"
              "~n      Virtual Factor:  ~w"
              "~n      TS Scale Factor: ~w"
              "~n", [Factor, AddLabelFactor, AddMemFactor, VirtFactor,
                     TSScaleFactor]),
    {Factor + AddLabelFactor + AddMemFactor + VirtFactor + TSScaleFactor,
     [{label, Label}]}.

linux_virt_factor() ->
    linux_virt_factor(linux_product_name()).

linux_virt_factor("VMware" ++ _) ->
    2;
linux_virt_factor("VirtualBox" ++ _) ->
    4;
linux_virt_factor(_) ->
    0.


linux_cpuinfo_clock() ->
    %% This is written as: "3783.000000MHz"
    %% So, check unit MHz (handle nothing else).
    %% Also, check for both float and integer
    %% Also,  the freq is per core, and can vary...
    case linux_cpuinfo_lookup("clock") of
        [C|_] when is_list(C) ->
            case lists:reverse(string:to_lower(C)) of
                "zhm" ++ CRev ->
                    try trunc(list_to_float(lists:reverse(CRev))) of
                        I ->
                            I
                    catch
                        _:_:_ ->
                            try list_to_integer(lists:reverse(CRev)) of
                                I ->
                                    I
                            catch
                                _:_:_ ->
                                    0
                            end
                    end;
                _ ->
                    0
            end;
        _ ->
            0
    end.

linux_cpuinfo_lookup(Key) when is_list(Key) ->
    linux_info_lookup(Key, "/proc/cpuinfo").

linux_cpuinfo_cpu() ->
    case linux_cpuinfo_lookup("cpu") of
        [Model] ->
            Model;
        ["POWER9" ++ _ = CPU|_] ->
            CPU;
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
	[] ->
	    "-";
        BMips when is_list(BMips) ->
            BMScale = 1.0,
            try round(BMScale * lists:sum([bogomips_to_int(BM) || BM <- BMips]))
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

linux_cpuinfo_machine() ->
    case linux_cpuinfo_lookup("machine") of
        [M] ->
            M;
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
linux_which_cpuinfo(Other) when (Other =:= debian) orelse
                                (Other =:= fedora) orelse
                                (Other =:= ubuntu) orelse
                                (Other =:= linux_mint) orelse
                                (Other =:= sles) orelse
                                (Other =:= suse) orelse
                                (Other =:= other) ->
    CPU =
        case linux_cpuinfo_model_name() of
            "-" ->
		%% This is for POWER9
		case linux_cpuinfo_cpu() of
		    "POWER9" ++ _ = PowerCPU ->
			Machine =
			    case linux_cpuinfo_machine() of
				"-" ->
				    "";
				M ->
				    " (" ++ M ++ ")"
			    end,
			PowerCPU ++ Machine;
		    _X ->
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
			end
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
    Label          = ts_extra_platform_label(),
    AddLabelFactor = label2factor(simplify_label(Label)),
    io:format("OpenBSD:"
              "~n   Version: ~s"
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
            CPUFactor =
                if
                    (CPUSpeed >= 3000) ->
                        if
                            (NCPU >= 8) ->
                                1;
                            (NCPU >= 6) ->
                                2;
                            (NCPU >= 4) ->
                                3;
                            (NCPU >= 2) ->
                                4;
                            true ->
                                10
                        end;
                    (CPUSpeed >= 2000) ->
                        if
                            (NCPU >= 8) ->
                                2;
                            (NCPU >= 6) ->
                                3;
                            (NCPU >= 4) ->
                                4;
                            (NCPU >= 2) ->
                                5;
                            true ->
                                12
                        end;
                    (CPUSpeed >= 1000) ->
                        if
                            (NCPU >= 8) ->
                                3;
                            (NCPU >= 6) ->
                                4;
                            (NCPU >= 4) ->
                                5;
                            (NCPU >= 2) ->
                                6;
                            true ->
                                14
                        end;
                    true ->
                        if
                            (NCPU >= 8) ->
                                4;
                            (NCPU >= 6) ->
                                6;
                            (NCPU >= 4) ->
                                8;
                            (NCPU >= 2) ->
                                10;
                            true ->
                                20
                        end
                end,
            MemAddFactor =
                if
                    (Memory >= 16777216) ->
                        0;
                    (Memory >= 8388608) ->
                        1;
                    (Memory >= 4194304) ->
                        3;
                    (Memory >= 2097152) ->
                        5;
                    true ->
                        10
                end,
            io:format("TS Scale Factor:         ~w~n"
                      "TS Extra Platform Label: ~s~n",
                      [timetrap_scale_factor(), Label]),
            {CPUFactor + MemAddFactor + AddLabelFactor, []}
        end
    catch
        _:_:_ ->
            io:format("TS Scale Factor:         ~w~n"
                      "TS Extra Platform Label: ~s~n",
                      [timetrap_scale_factor(), Label]),
            {2 + AddLabelFactor, []}
    end.

which_freebsd_version() ->
    case string:trim(os:cmd("which freebsd-version")) of
        [] ->
            "-";
        FreeBSDVersion ->
            case string:trim(os:cmd(FreeBSDVersion)) of
                [] ->
                    undefined;
                V ->
                    V
            end
    end.
    
analyze_and_print_freebsd_host_info(Version) ->
    Label          = ts_extra_platform_label(),
    AddLabelFactor = label2factor(simplify_label(Label)),
    FreeBSDVersion = which_freebsd_version(),
    case FreeBSDVersion of
        undefined ->
            io:format("FreeBSD:"
                      "~n   Version: ~s"
                      "~n", [Version]),
            "";
        _ ->
            io:format("FreeBSD:"
                      "~n   Version: ~s (~s)"
                      "~n", [Version, FreeBSDVersion])
    end,
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
                      "~n   Model:                 ~s"
                      "~n   Speed:                 ~w"
                      "~n   N:                     ~w"
                      "~n   Num Online Schedulers: ~s"
                      "~nMemory:"
                      "~n   ~w KB"
                      "~n",
                      [CPU, CPUSpeed, NCPU, str_num_schedulers(), Memory]),
            io:format("TS Scale Factor:         ~w~n"
                      "TS Extra Platform Label: ~s~n",
                      [timetrap_scale_factor(), Label]),
            CPUFactor =
                if
                    (CPUSpeed =:= -1) ->
                        1;
                    (CPUSpeed >= 3000) ->
                        if
                            (NCPU >= 8) ->
                                1;
                            (NCPU >= 6) ->
                                2;
                            (NCPU >= 4) ->
                                3;
                            (NCPU >= 2) ->
                                4;
                            true ->
                                5
                        end;
                    (CPUSpeed >= 2000) ->
                        if
                            (NCPU >= 12) ->
                                1;
                            (NCPU >= 8) ->
                                2;
                            (NCPU >= 6) ->
                                4;
                            (NCPU >= 4) ->
                                6;
                            (NCPU >= 2) ->
                                8;
                            true ->
                                10
                        end;
                    true ->
                        if
                            (NCPU =:= -1) ->
                                2;
                            (NCPU >= 12) ->
                                2;
                            (NCPU >= 8) ->
                                3;
                            (NCPU >= 6) ->
                                5;
                            (NCPU >= 4) ->
                                7;
                            (NCPU >= 2) ->
                                9;
                            true ->
                                12
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
                      "~n   Num Online Schedulers: ~s"
                      "~n", [str_num_schedulers()]),
            io:format("TS Scale Factor:         ~w~n"
                      "TS Extra Platform Label: ~s~n",
                      [timetrap_scale_factor(), Label]),
            {num_schedulers_to_factor() ++ AddLabelFactor, []}
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
    Label          = ts_extra_platform_label(),
    AddLabelFactor = label2factor(simplify_label(Label)),
    io:format("NetBSD:"
              "~n   Version: ~s"
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
            io:format("TS Scale Factor:         ~w~n"
                      "TS Extra Platform Label: ~s~n",
                      [timetrap_scale_factor(), Label]),
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
            {CPUFactor + MemAddFactor + AddLabelFactor, []}
        end
    catch
        _:_:_ ->
            io:format("CPU:"
                      "~n   Num Schedulers: ~w"
                      "~n", [erlang:system_info(schedulers)]),
            io:format("TS Scale Factor:         ~w~n"
                      "TS Extra Platform Label: ~s~n",
                      [timetrap_scale_factor(), Label]),
            Factor = num_schedulers_to_factor(),
            {Factor + AddLabelFactor, []}
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

%%       Model Identifier: Macmini7,1
%%       Processor Name: Intel Core i5
%%       Processor Speed: 2,6 GHz
%%       Number of Processors: 1
%%       Total Number of Cores: 2
%%       L2 Cache (per Core): 256 KB
%%       L3 Cache: 3 MB
%%       Hyper-Threading Technology: Enabled
%%       Memory: 16 GB

%% Hardware:
%%
%%   Hardware Overview:
%%
%%    Model Name: MacBook Pro
%%    Model Identifier: MacBookPro18,1
%%    Chip: Apple M1 Pro
%%    Total Number of Cores: 10 (8 performance and 2 efficiency)
%%    Memory: 32 GB
%%    System Firmware Version: 7459.101.2
%%    OS Loader Version: 7459.101.2
%%    Serial Number (system): THF4W05C97
%%    Hardware UUID: 7C9AB2E1-73B1-5AD6-9BC8-7229DE7A748C
%%    Provisioning UDID: 00006000-000259042662801E
%%    Activation Lock Status: Enabled


analyze_and_print_darwin_host_info(Version) ->
    Label          = ts_extra_platform_label(),
    AddLabelFactor = label2factor(simplify_label(Label)),
    %% This stuff is for macOS.
    %% If we ever tested on a pure darwin machine,
    %% we need to find some other way to find some info...
    %% Also, I suppose its possible that we for some other
    %% reason *fail* to get the info...
    Label  = ts_extra_platform_label(),
    {BaseFactor, MemFactor} =
        case analyze_darwin_software_info() of
            [] ->
                io:format("Darwin:"
                          "~n   Version:               ~s"
                          "~n   Num Online Schedulers: ~s"
                          "~n   TS Extra Platform Label: ~s"
                          "~n", [Version, str_num_schedulers(), Label]),
                {num_schedulers_to_factor(), 1};
            SwInfo when is_list(SwInfo) ->
                SystemVersion = analyze_darwin_sw_system_version(SwInfo),
                KernelVersion = analyze_darwin_sw_kernel_version(SwInfo),
                HwInfo        = analyze_darwin_hardware_info(),
                ModelName     = analyze_darwin_hw_model_name(HwInfo),
                ModelId       = analyze_darwin_hw_model_identifier(HwInfo),
                {Processor, CPUFactor} = analyze_darwin_hw_processor(HwInfo),
                Memory        = analyze_darwin_hw_memory(HwInfo),
                Memory        = analyze_darwin_hw_memory(HwInfo),
                io:format("Darwin:"
                          "~n   System Version:          ~s"
                          "~n   Kernel Version:          ~s"
                          "~n   Model:                   ~s (~s)"
                          "~n   Processor:               ~s"
                          "~n   Memory:                  ~s"
                          "~n   Num Online Schedulers:   ~s"
                          "~n   TS Extra Platform Label: ~s"
                          "~n~n",
                          [SystemVersion, KernelVersion,
                           ModelName, ModelId,
                           Processor, 
                           Memory,
                           str_num_schedulers(), Label]),
                {CPUFactor, analyze_darwin_memory_to_factor(Memory)}
        end,
    AddLabelFactor = label2factor(simplify_label(Label)),
    AddMemFactor = if
                       (MemFactor > 0) ->
                           MemFactor - 1;
                       true ->
                           0
                   end,
    TSScaleFactor = ts_scale_factor(),
    io:format("Factor calc:"
              "~n      Base Factor:     ~w"
              "~n      Label Factor:    ~w"
              "~n      Mem Factor:      ~w"
              "~n      TS Scale Factor: ~w"
              "~n~n",
              [BaseFactor, AddLabelFactor, AddMemFactor, TSScaleFactor]),
    {BaseFactor + AddLabelFactor + AddMemFactor + TSScaleFactor,
     [{label, Label}]}.

analyze_darwin_sw_system_version(SwInfo) ->
    proplists:get_value("system version", SwInfo, "-").

analyze_darwin_sw_kernel_version(SwInfo) ->
    proplists:get_value("kernel version", SwInfo, "-").

analyze_darwin_software_info() ->
    analyze_darwin_system_profiler("SPSoftwareDataType").

analyze_darwin_hw_chip(HwInfo) ->
    proplists:get_value("chip", HwInfo, "-").

analyze_darwin_hw_model_name(HwInfo) ->
    proplists:get_value("model name", HwInfo, "-").

analyze_darwin_hw_model_identifier(HwInfo) ->
    proplists:get_value("model identifier", HwInfo, "-").

analyze_darwin_hw_processor(HwInfo) ->
    case analyze_darwin_hw_processor_name(HwInfo) of
        "-" -> % Maybe Apple Chip
            case analyze_darwin_hw_chip(HwInfo) of
                "-" ->
                    "-";
                Chip ->
                    NumCores = analyze_darwin_hw_total_number_of_cores(HwInfo),
                    CPUFactor = analyze_darwin_cpu_to_factor(Chip, NumCores),
                    {f("~s [~s]", [Chip, NumCores]), CPUFactor}
            end;
        ProcName ->
            ProcSpeed = analyze_darwin_hw_processor_speed(HwInfo),
            NumProc   = analyze_darwin_hw_number_of_processors(HwInfo),
            NumCores  = analyze_darwin_hw_total_number_of_cores(HwInfo),
            CPUFactor = analyze_darwin_cpu_to_factor(ProcName,
                                                     ProcSpeed,
                                                     NumProc,
                                                     NumCores),
            {f("~s [~s, ~s, ~s]",
               [ProcName, ProcSpeed, NumProc, NumCores]), CPUFactor}
    end.

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
    %% First, make sure the program actually exist (with the current PATH):
    Prog0 = "system_profiler",
    case os:cmd("which " ++ Prog0) of
        [] ->
            %% Ok, as a last resource, check if is /usr/sbin/system_profiler?
            Prog1 = "/usr/sbin/system_profiler",
            case os:cmd("which " ++ Prog1) of
                [] ->
                    [];
                _ ->
                    analyze_darwin_system_profiler(Prog1, DataType)
            end;
        _ ->
            analyze_darwin_system_profiler(Prog0, DataType)
    end.

analyze_darwin_system_profiler(Prog, DataType) ->
    D0 = os:cmd(Prog ++ " " ++ DataType),
    D1 = string:tokens(D0, [$\n]),
    D2 = [string:trim(S1) || S1 <- D1],
    D3 = [string:tokens(S2, [$:]) || S2 <- D2],
    analyze_darwin_system_profiler2(D3).

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

%% This is for the M1 family of chips
%% We don't actually know *how* fast it is, only that its fast.
%% the speed may be a float, which we transforms into an integer of MHz.
%% To calculate a factor based on processor "class" and number of cores
%% is ... not an exact ... science ...
analyze_darwin_cpu_to_factor("Apple M" ++ _ = _Chip, _NumCoresStr) ->
    %% We know that pretty much every M processor is *fast*,
    %% so there is no real need to "calculate" anything...
    1.

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
    Label          = ts_extra_platform_label(),
    AddLabelFactor = label2factor(simplify_label(Label)),

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
              "~n   Release:                ~s"
              "~n   Banner Name:            ~s"
              "~n   Instruction Set:        ~s"
              "~n   CPUs:                   ~s (~s)"
              "~n   System Config:          ~s"
              "~n   Memory Size:            ~s"
              "~n   Num Online Schedulers:  ~s"
              "~n~n", [Version, Release, BannerName, InstructionSet,
                       NumPhysCPU, NumVCPU,
                       SysConf, MemSz,
                       str_num_schedulers()]),
    AddMemFactor =
        try string:tokens(MemSz, [$ ]) of
            [SzStr, "Mega" ++ _] ->
                try list_to_integer(SzStr) of
                    Sz when Sz > 16384 ->
                        0;
                    Sz when Sz > 8192 ->
                        1;
                    Sz when Sz > 4096 ->
                        4;
                    Sz when Sz > 2048 ->
                        8;
                    _ -> 
                        12
                catch
                    _:_:_ ->
                        10
                end;
            [SzStr, "Giga" ++ _] ->
                try list_to_integer(SzStr) of
                    Sz when Sz > 16 ->
                        0;
                    Sz when Sz > 8 ->
                        1;
                    Sz when Sz > 4 ->
                        4;
                    Sz when Sz > 2 ->
                        8;
                    _ -> 
                        12
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
    %% We don't really have enough info about the CPU to calculate the
    %% base factor based on that, so we just use the number of schedulers.
    BaseFactor =
        try erlang:system_info(schedulers) of
            1 ->
                12;
            2 ->
                8;
            N when (N =:= 3) orelse (N =:= 4) ->
	        4;
            N when (N =< 6) ->
                3;
            _ ->
                2
        catch
            _:_:_ ->
               12
        end,
    TSScaleFactor = ts_scale_factor(),
    io:format("Factor calc:"
              "~n      Base Factor:             ~w"
              "~n      Label Factor:            ~w"
              "~n      Mem Factor:              ~w"
              "~n      TS Scale Factor:         ~w"
              "~n      TS Extra Platform Label: ~s"
              "~n~n",
              [BaseFactor, AddLabelFactor, AddMemFactor,
	       TSScaleFactor, Label]),
    {BaseFactor + AddMemFactor + AddLabelFactor + TSScaleFactor,
     [{label, Label}]}.    

analyze_and_print_win_host_info(Version) ->
    Label          = ts_extra_platform_label(),
    AddLabelFactor = label2factor(simplify_label(Label)),

    SysInfo    = which_win_system_info(),
    OsName     = win_sys_info_lookup(os_name,             SysInfo),
    OsVersion  = win_sys_info_lookup(os_version,          SysInfo),
    SysMan     = win_sys_info_lookup(system_manufacturer, SysInfo),
    SysMod     = win_sys_info_lookup(system_model,        SysInfo),
    SysType    = win_sys_info_lookup(system_type,         SysInfo),
    NumProcs   = win_sys_info_lookup(num_processors,      SysInfo),
    TotPhysMem = win_sys_info_lookup(total_phys_memory,   SysInfo),
    io:format("Windows: ~s"
              "~n   OS Version:             ~s (~p)"
              "~n   System Manufacturer:    ~s"
              "~n   System Model:           ~s"
              "~n   System Type:            ~s"
              "~n   Number of Processor(s): ~s"
              "~n   Total Physical Memory:  ~s"
              "~n   (Erlang) WordSize:      ~w"
              "~n   Num Online Schedulers:  ~s"
              "~n~n", [OsName, OsVersion, Version,
                       SysMan, SysMod, SysType,
                       NumProcs, TotPhysMem,
                       erlang:system_info(wordsize),
                       str_num_schedulers()]),
    io:format("TS: "
              "~n   TimeTrap Factor:      ~w"
              "~n   Extra Platform Label: ~s"
              "~n~n",
              [timetrap_scale_factor(), Label]),
    %% 'VirtFactor' will be 0 unless virtual
    VirtFactor = win_virt_factor(SysMod),

    %% On some machines this is a badly formated string
    %% (contains a char of 255), so we need to do some nasty stuff...
    MemFactor =
        try
            begin
                %% "Normally" this looks like this: "16,123 MB"
                %% But sometimes the "," is replaced by a
		%% 255 or 160 char, which I assume must be some
		%% unicode screwup...
                %% Anyway, filter out both of them!
                TotPhysMem1 = lists:delete($,, TotPhysMem),
                TotPhysMem2 = lists:delete(255, TotPhysMem1),
                TotPhysMem3 = lists:delete(160, TotPhysMem2),
                [MStr, MUnit|_] = string:tokens(TotPhysMem3, [$\ ]),
                case string:to_lower(MUnit) of
                    "gb" ->
                        try list_to_integer(MStr) of
                            M when M >= 16 ->
                                0;
                            M when M >= 8 ->
                                1;
                            M when M >= 4 ->
                                3;
                            M when M >= 2 ->
                                6;
                            _ -> 
                                10
                        catch
                            _:_:_ ->
                                %% For some reason the string contains
                                %% "unusual" characters...
                                %% ...so print the string as a list...
                                io:format("Bad memory string: "
                                          "~n   [gb] ~w"
                                          "~n", [MStr]),
                                10
                        end;
                    "mb" ->
                        try list_to_integer(MStr) of
                            M when M >= 16384 ->
                                0;
                            M when M >= 8192 ->
                                1;
                            M when M >= 4096 ->
                                3;
                            M when M >= 2048 ->
                                6;
                            _ -> 
                                10
                        catch
                            _:_:_ ->
                                %% For some reason the string contains
                                %% "unusual" characters...
                                %% ...so print the string as a list...
                                io:format("Bad memory string: "
                                          "~n   [mb] ~w"
                                          "~n", [MStr]),
                                10
                        end;
                    _ ->
                        io:format("Bad memory string: "
                                  "~n   ~w"
                                  "~n", [MStr]),
                        10
                end
            end
        catch
            _:_:_ ->
                %% For some reason the string contains
                %% "unusual" characters...
                %% ...so print the string as a list...
                io:format("Bad memory string: "
                          "~n   (y) ~w"
                          "~n", [TotPhysMem]),
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
    io:format("Factor calc:"
              "~n      CPU Factor:     ~w"
              "~n      Mem Factor:     ~w"
              "~n      Label Factor:   ~w"
              "~n      Virtual Factor: ~w"
              "~n~n",
              [CPUFactor, MemFactor, AddLabelFactor, VirtFactor]),
    {CPUFactor + MemFactor + AddLabelFactor + VirtFactor, SysInfo}.

win_virt_factor("VMware" ++ _) ->
    2;
win_virt_factor(_) ->
    0.


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
                "system type" ->
                    process_win_system_info(T,
                                            [{system_type, string:trim(Value)}|Acc]);
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
    try erlang:system_info(schedulers_online) of
        N -> f("~w", [N])
    catch
        _:_:_ -> "-"
    end.

num_schedulers_to_factor() ->
    try erlang:system_info(schedulers_online) of
        1 ->
            10;
        2 ->
            8;
        3 ->
            6;
        4 ->
            4;
        N when (N =< 5) ->
            2;
        _ ->
            1
    catch
        _:_:_ ->
            10
    end.


ts_extra_platform_label() ->
    case os:getenv("TS_EXTRA_PLATFORM_LABEL") of
        false -> "-";
        Val   -> Val
    end.

ts_scale_factor() ->
    case timetrap_scale_factor() of
        N when is_integer(N) andalso (N > 0) ->
            N - 1;
        _ ->
            0
    end.

simplify_label("Systemtap" ++ _) ->
    {host, systemtap};
simplify_label("Meamax" ++ _) ->
    {host, meamax};
simplify_label("Cover" ++ _) ->
    {host, cover};
simplify_label(Label) ->
    case string:find(string:to_lower(Label), "docker") of
        "docker" ++ _ ->
            docker;
        _ ->
            {host, undefined}
    end.

label2factor(docker) ->
    4;
label2factor({host, meamax}) ->
    2;
label2factor({host, cover}) ->
    6;
label2factor({host, _}) ->
    0.

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


linux_product_name() ->
    ProductNameFile = "/sys/devices/virtual/dmi/id/product_name",
    case file:read_file_info(ProductNameFile) of
        {ok, _} ->
            case os:cmd("cat " ++ ProductNameFile) of
                false ->
                    "-";
                Info ->
                    string:trim(Info)
            end;
        _ ->
            "-"
    end.


maybe_skip(_HostInfo) ->

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
        %% fun() ->
        %%         SysMan = win_sys_info_lookup(system_manufacturer, HostInfo),
        %%         case string:to_lower(SysMan) of
        %%             "vmware" ++ _ ->
        %%                 true;
        %%             _ ->
        %%                 false
        %%         end
        %% end,
        fun() ->
                %% The host has been replaced and the VM has been reinstalled
                %% so for now we give it a chance...
                false
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

lookup(Key, Config, Default) ->
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, Val}} ->
            Val;
        _ ->
            Default
    end.


%% Extract N number of hosts from the config.
%% Prepend with the own host.
%% If not N number of hosts are available, issue a skip exit.
good_hosts(N) when is_integer(N) andalso (N > 0) ->
    GoodHosts         = ct:get_config(test_hosts, []),
    {ok, CurrentHost} = inet:gethostname(),
    GoodHosts2        = [CurrentHost] ++ (GoodHosts -- [CurrentHost]),
    good_hosts2(GoodHosts2, N).

good_hosts2(GoodHosts, N) ->
    good_hosts2(GoodHosts, N, []).

good_hosts2(_GoodHosts, N, Acc) when (N =:= 0) ->
    lists:reverse(Acc);
good_hosts2([], _N, _Acc) ->
    ?SKIPE("Not enough good hosts");
good_hosts2([H|T], N, Acc) ->
    case inet:gethostbyname(H) of
        {ok, _} ->
            good_hosts2(T, N-1, [H|Acc]); 
        {error, _} ->
            ?P("Host not found: ~p", [H]),
            good_hosts2(T, N, Acc)
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


%% *** tc_try/2,3,4,5 ***
%% Case:   Basically the test case name
%% TCCond: A fun that is evaluated before the actual test case
%%         The point of this is that it can performs checks to
%%         see if we shall run the test case at all.
%%         For instance, the test case may only work in specific
%%         conditions.
%% Pre:    A fun that is nominally part of the test case
%%         but is an initiation that must be "undone". This is
%%         done by the Post fun (regardless if the TC is successfull
%%         or not). Example: Starts a couple of nodes,
%% TC:     The test case fun
%% Post:   A fun that undo what was done by the Pre fun.
%%         Example: Stops the nodes created by the Pre function.
tc_try(Case, TC) ->
    tc_try(Case, fun() -> ok end, TC).

tc_try(Case, TCCond, TC0) when is_function(TC0, 0) ->
    Pre  = fun()  -> undefined end,
    TC   = fun(_) -> TC0() end,
    Post = fun(_) -> ok end,
    tc_try(Case, TCCond, Pre, TC, Post).

tc_try(Case, Pre, TC, Post)
  when is_atom(Case) andalso
       is_function(Pre, 0) andalso
       is_function(TC, 1) andalso
       is_function(Post, 1) ->
    TCCond = fun() -> ok end,
    tc_try(Case, TCCond, Pre, TC, Post).

tc_try(Case, TCCond, Pre, TC, Post)
  when is_atom(Case) andalso
       is_function(TCCond, 0) andalso
       is_function(Pre, 0) andalso
       is_function(TC, 1) andalso
       is_function(Post, 1) ->
    tc_begin(Case),
    try TCCond() of
        ok ->
            tc_print("starting: try pre"),
            try Pre() of
                State ->
                    tc_print("pre done: try test case"),
                    try
                        begin
                            TCRes = TC(State),
                            ?SLEEP(?SECS(1)),
                            tc_print("test case done: try post"),
                            (catch Post(State)),
                            tc_end("ok"),
                            TCRes
                        end
                    catch
                        C:{skip, _} = SKIP when (C =:= throw) orelse
                                                (C =:= exit) ->
                            tc_print("test case (~w) skip: try post", [C]),
                            (catch Post(State)),
                            tc_end( f("skipping(catched,~w,tc)", [C]) ),
                            SKIP;
                        C:E:S ->
                            %% We always check the system events
                            %% before we accept a failure.
                            %% We do *not* run the Post here because it might
                            %% generate sys events itself...
                            case kernel_test_global_sys_monitor:events() of
                                [] ->
                                    tc_print("test case failed: try post"),
                                    (catch Post(State)),
                                    tc_end( f("failed(caught,~w,tc)", [C]) ),
                                    erlang:raise(C, E, S);
                                SysEvs ->
                                    tc_print(
                                      "System Events received during tc: "
                                      "~n   ~p"
                                      "~nwhen tc failed:"
                                      "~n   C: ~p"
                                      "~n   E: ~p"
                                      "~n   S: ~p",
                                      [SysEvs, C, E, S]),
                                    (catch Post(State)),
                                    tc_end( f("skipping(catched-sysevs,~w,tc)",
                                              [C]) ),
                                    SKIP = {skip,
                                            "TC failure with system events"},
                                    SKIP
                            end
                    end
            catch
                C:{skip, _} = SKIP when (C =:= throw) orelse
                                        (C =:= exit) ->
                    tc_end( f("skipping(catched,~w,tc-pre)", [C]) ),
                    SKIP;
                C:E:S ->
                    %% We always check the system events
                    %% before we accept a failure
                    case kernel_test_global_sys_monitor:events() of
                        [] ->
                            tc_print("tc-pre failed: auto-skip"
                                     "~n   C: ~p"
                                     "~n   E: ~p"
                                     "~n   S: ~p",
                                     [C, E, S]),
                            tc_end( f("auto-skip(caught,~w,tc-pre)", [C]) ),
                            SKIP = {skip, f("TC-Pre failure (~w)", [C])},
                            SKIP;
                        SysEvs ->
                            tc_print("System Events received: "
                                     "~n   ~p"
                                     "~nwhen tc-pre failed:"
                                     "~n   C: ~p"
                                     "~n   E: ~p"
                                     "~n   S: ~p",
                                     [SysEvs, C, E, S], "", ""),
                            tc_end( f("skipping(catched-sysevs,~w,tc-pre)",
                                      [C]) ),
                            SKIP = {skip, "TC-Pre failure with system events"},
                            SKIP
                    end
            end;
        {skip, _} = SKIP ->
            tc_end("skipping(cond)"),
            SKIP;
        {error, Reason} ->
            tc_end("failed(cond)"),
            exit({tc_cond_failed, Reason})
    catch
        C:{skip, _} = SKIP when ((C =:= throw) orelse (C =:= exit)) ->
            tc_end( f("skipping(catched,~w,cond)", [C]) ),
            SKIP;
        C:E:S ->
            tc_end( f("failed(catched,~w,cond)", [C]) ),
            erlang:raise(C, E, S)
    end.


tc_print(F) ->
    tc_print(F, [], "", "").

tc_print(F, A) ->
    tc_print(F, A, "", "").

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
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_node(Name, Args) ->
    start_node(Name, Args, []).

start_node(Name, Args, Opts) ->
    Pa = filename:dirname(code:which(?MODULE)),
    A = Args ++
        " -pa " ++ Pa ++ 
        " -s " ++ atom_to_list(kernel_test_sys_monitor) ++ " start" ++ 
        " -s global sync",
    case test_server:start_node(Name, peer, [{args, A}|Opts]) of
        {ok, _Node} = OK ->
            global:sync(),
	    OK;
        ERROR ->
            ERROR
    end.

stop_node(Node) ->
    test_server:stop_node(Node).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timetrap_scale_factor() ->
    case (catch test_server:timetrap_scale_factor()) of
        {'EXIT', _} ->
            1;
        N ->
            N
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

os_cmd(Cmd) ->
    os_cmd(Cmd, infinity).

os_cmd(Cmd, infinity) ->
    {ok, os:cmd(Cmd)};
os_cmd(Cmd, Timeout) when is_integer(Timeout) andalso (Timeout > 0) ->
    proxy_call(fun() -> {ok, os:cmd(Cmd)} end, Timeout, {error, timeout}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

socket_type(Config) ->
    case is_socket_backend(Config) of
	true ->
	    socket;
	false ->
	    port
    end.

%% gen_tcp wrappers

listen(Config, Port, Opts) ->
    InetBackendOpts = inet_backend_opts(Config),
    gen_tcp:listen(Port, InetBackendOpts ++ Opts).

connect(Config, Host, Port, Opts) ->
    InetBackendOpts = inet_backend_opts(Config),
    gen_tcp:connect(Host, Port, InetBackendOpts ++ Opts).

connect(Config, Host, Port, Opts, Timeout) ->
    InetBackendOpts = inet_backend_opts(Config),
    gen_tcp:connect(Host, Port, InetBackendOpts ++ Opts, Timeout).


%% gen_udp wrappers

open(Config, Port, Opts) ->
    InetBackendOpts = inet_backend_opts(Config),
    gen_udp:open(Port, InetBackendOpts ++ Opts).


inet_backend_opts(Config) when is_list(Config) ->
    case lists:keysearch(socket_create_opts, 1, Config) of
        {value, {socket_create_opts, InetBackendOpts}} ->
            InetBackendOpts;
        false ->
            []
    end.

is_socket_backend(Config) when is_list(Config) ->
    case lists:keysearch(socket_create_opts, 1, Config) of
        {value, {socket_create_opts, [{inet_backend, socket}]}} ->
            true;
        _ ->
            false
    end.


explicit_inet_backend() ->
    case application:get_all_env(kernel) of
        Env when is_list(Env) ->
            case lists:keysearch(inet_backend, 1, Env) of
                {value, {inet_backend, _}} ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end.


test_inet_backends() ->
    case application:get_all_env(kernel) of
        Env when is_list(Env) ->
            case lists:keysearch(test_inet_backends, 1, Env) of
                {value, {test_inet_backends, true}} ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false 
    end.

which_inet_backend(Config) ->
    case lists:keysearch(socket_create_opts, 1, Config) of
        {value, {socket_create_opts, [{inet_backend, Backend}]}} ->
            Backend;
        _ ->
            default
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



%% This is an extremely simple check...
has_support_ipv4() ->
    case which_local_addr(inet) of
        {ok, _Addr} ->
            ok;
        {error, _R1} ->
            skip("IPv4 Not Supported")
    end.

has_support_ipv6() ->
    case which_local_addr(inet6) of
        {ok, _Addr} ->
            ok;
        {error, _R1} ->
            skip("IPv6 Not Supported")
    end.

is_socket_supported() ->
    try socket:info() of
        #{} ->
            true
    catch
        error : notsup ->
            false;
        error : undef ->
            false
    end.

has_support_unix_domain_socket() ->
    socket:is_supported(local).

%% This gets the local "proper" address
%% (not {127, ...} or {169,254, ...} or {0, ...} or {16#fe80, ...})
%% We should really implement this using the (new) net module,
%% but until that gets the necessary functionality...
which_local_addr(Domain) ->
    case which_local_host_info(false, Domain) of
        {ok, [#{addr := Addr}|_]} ->
            {ok, Addr};
        {error, _Reason} = ERROR ->
            ERROR
    end.


%% This function returns the link local address of the local host
%% IPv4: 169.254.0.0/16 (169.254.0.0 - 169.254.255.255)
%% IPv6: fe80::/10
which_link_local_addr(Domain) ->
    case which_local_host_info(true, Domain) of
        {ok, [#{addr := Addr}|_]} ->
            {ok, Addr};
        {error, _Reason} = ERROR ->
            ERROR
    end.


%% Returns the interface (name), flags and address (not 127...,
%% or the equivalent IPv6 address) of the local host.
which_local_host_info(Domain) ->
    which_local_host_info(false, Domain).


which_local_host_info(LinkLocal, Domain)
  when is_boolean(LinkLocal) andalso
       ((Domain =:= inet) orelse (Domain =:= inet6)) ->
    case inet:getifaddrs() of
        {ok, IFL} ->
            which_local_host_info(LinkLocal, Domain, IFL, []);
        {error, _} = ERROR ->
            ERROR
    end.

%% There are a bunch of "special" interfaces that we exclude:
%% Here are some MacOS interfaces:
%%   lo      (skip) is the loopback interface
%%   en0     (keep) is your hardware interfaces (usually Ethernet and WiFi)
%%   p2p0    (skip) is a point to point link (usually VPN)
%%   stf0    (skip) is a "six to four" interface (IPv6 to IPv4)
%%   gif01   (skip) is a software interface
%%   bridge0 (skip) is a software bridge between other interfaces
%%   utun0   (skip) is used for "Back to My Mac"
%%   XHC20   (skip) is a USB network interface
%%   awdl0   (skip) is Apple Wireless Direct Link (Bluetooth) to iOS devices
%% What are these:
%%   ap0
%%   anpi0
%%.  vmenet0
%% On Mac, List hw: networksetup -listallhardwareports
which_local_host_info(_LinkLocal, _Domain, [], []) ->
    {error, no_address};
which_local_host_info(_LinkLocal, _Domain, [], Acc) ->
    {ok, lists:reverse(Acc)};
which_local_host_info(LinkLocal, Domain, [{"tun" ++ _, _}|IFL], Acc) ->
    which_local_host_info(LinkLocal, Domain, IFL, Acc);
which_local_host_info(LinkLocal, Domain, [{"docker" ++ _, _}|IFL], Acc) ->
    which_local_host_info(LinkLocal, Domain, IFL, Acc);
which_local_host_info(LinkLocal, Domain, [{"br-" ++ _, _}|IFL], Acc) ->
    which_local_host_info(LinkLocal, Domain, IFL, Acc);
which_local_host_info(LinkLocal, Domain, [{"ap" ++ _, _}|IFL], Acc) ->
    which_local_host_info(LinkLocal, Domain, IFL, Acc);
which_local_host_info(LinkLocal, Domain, [{"anpi" ++ _, _}|IFL], Acc) ->
    which_local_host_info(LinkLocal, Domain, IFL, Acc);
which_local_host_info(LinkLocal, Domain, [{"vmenet" ++ _, _}|IFL], Acc) ->
    which_local_host_info(LinkLocal, Domain, IFL, Acc);
which_local_host_info(LinkLocal, Domain, [{"utun" ++ _, _}|IFL], Acc) ->
    which_local_host_info(LinkLocal, Domain, IFL, Acc);
which_local_host_info(LinkLocal, Domain, [{"bridge" ++ _, _}|IFL], Acc) ->
    which_local_host_info(LinkLocal, Domain, IFL, Acc);
which_local_host_info(LinkLocal, Domain, [{"llw" ++ _, _}|IFL], Acc) ->
    which_local_host_info(LinkLocal, Domain, IFL, Acc);
which_local_host_info(LinkLocal, Domain, [{"awdl" ++ _, _}|IFL], Acc) ->
    which_local_host_info(LinkLocal, Domain, IFL, Acc);
which_local_host_info(LinkLocal, Domain, [{"p2p" ++ _, _}|IFL], Acc) ->
    which_local_host_info(LinkLocal, Domain, IFL, Acc);
which_local_host_info(LinkLocal, Domain, [{"stf" ++ _, _}|IFL], Acc) ->
    which_local_host_info(LinkLocal, Domain, IFL, Acc);
which_local_host_info(LinkLocal, Domain, [{"XHCZ" ++ _, _}|IFL], Acc) ->
    which_local_host_info(LinkLocal, Domain, IFL, Acc);
which_local_host_info(LinkLocal, Domain, [{Name, IFO}|IFL], Acc) ->
    case if_is_running_and_not_loopback(IFO) of
        true ->
            try which_local_host_info2(LinkLocal, Domain, IFO) of
                Info ->
                    which_local_host_info(LinkLocal, Domain, IFL,
                                          [Info#{name => Name}|Acc])
            catch
                throw:_E:_ ->
                    which_local_host_info(LinkLocal, Domain, IFL, Acc)
            end;
        false ->
            which_local_host_info(LinkLocal, Domain, IFL, Acc)
    end;
which_local_host_info(LinkLocal, Domain, [_|IFL], Acc) ->
    which_local_host_info(LinkLocal, Domain, IFL, Acc).

if_is_running_and_not_loopback(If) ->
    lists:keymember(flags, 1, If) andalso
        begin
            {value, {flags, Flags}} = lists:keysearch(flags, 1, If),
            (not lists:member(loopback, Flags)) andalso
                lists:member(running, Flags)
        end.


which_local_host_info2(LinkLocal, inet = _Domain, IFO) ->
    Addr      = which_local_host_info3(
                  addr,  IFO,
                  fun({A, _, _, _}) when (A =:= 127) -> false;
                     ({A, B, _, _}) when (A =:= 169) andalso 
                                         (B =:= 254) -> LinkLocal;
                     ({_, _, _, _}) -> not LinkLocal;
                     (_) -> false
                  end),
    NetMask   = try which_local_host_info3(netmask,  IFO,
					   fun({_, _, _, _}) -> true;
					      (_) -> false
					   end)
		catch
		    throw:{error, no_address} ->
			undefined
		end,
    BroadAddr = try which_local_host_info3(broadaddr,  IFO,
					   fun({_, _, _, _}) -> true;
					      (_) -> false
					   end)
		catch
		    throw:{error, no_address} ->
			undefined
		end,
    Flags     = try which_local_host_info3(flags, IFO, fun(_) -> true end)
		catch
		    throw:{error, no_address} ->
			[]
		end,
    #{flags     => Flags,
      addr      => Addr,
      broadaddr => BroadAddr,
      netmask   => NetMask};
which_local_host_info2(LinkLocal, inet6 = _Domain, IFO) ->
    Addr    = which_local_host_info3(addr,  IFO,
                                     fun({A, _, _, _, _, _, _, _}) 
                                           when (A =:= 0) -> false;
                                        ({A, _, _, _, _, _, _, _})
                                           when (A =:= 16#fe80) -> LinkLocal;
                                        ({_, _, _, _, _, _, _, _}) -> not LinkLocal;
                                        (_) -> false
                                     end),
    NetMask = which_local_host_info3(netmask,  IFO,
                                       fun({_, _, _, _, _, _, _, _}) -> true;
                                          (_) -> false
                                       end),
    Flags   = which_local_host_info3(flags, IFO, fun(_) -> true end),
    #{flags   => Flags,
      addr    => Addr,
      netmask => NetMask}.

which_local_host_info3(_Key, [], _) ->
    throw({error, no_address});
which_local_host_info3(Key, [{Key, Val}|IFO], Check) ->
    case Check(Val) of
        true ->
            Val;
        false ->
            which_local_host_info3(Key, IFO, Check)
    end;
which_local_host_info3(Key, [_|IFO], Check) ->
    which_local_host_info3(Key, IFO, Check).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_yet_implemented() ->
    skip("not yet implemented").

skip(Reason) ->
    throw({skip, Reason}).


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
