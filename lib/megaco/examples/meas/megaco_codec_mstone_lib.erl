%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose: Misc utility functions for the mstone modules
%%----------------------------------------------------------------------

-module(megaco_codec_mstone_lib).


%% API
%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([start_flex_scanner/0, stop_flex_scanner/1,
	 expanded_messages/2, expanded_messages/3, 
	 display_os_info/0, 
	 display_system_info/0, 
	 display_alloc_info/0, 
	 display_app_info/0,
	 detect_version/3]).

%% Internal exports
-export([flex_scanner_handler/1]).

-include_lib("kernel/include/file.hrl").


%%----------------------------------------------------------------------
%% 
%% D e t e c t   V e r s i o n
%% 
%%----------------------------------------------------------------------

detect_version(Codec, Conf, Bin) ->
    case (catch Codec:version_of(Conf, Bin)) of
	{ok, V} ->
	    case (catch Codec:decode_message(Conf, V, Bin)) of
		{ok, M} ->
		    case (catch Codec:encode_message(Conf, V, M)) of
			{ok, NewBin} ->
			    {V, NewBin};
			Error1 ->
			    error({encode_failed, Error1, Codec, Conf, M})
		    end;
		Error2 ->
		    error({decode_failed, Error2, Codec, Conf, Bin})
	    end;
	Error3 ->
	    error({version_of_failed, Error3, Codec, Conf, Bin})
    end.


%%----------------------------------------------------------------------
%% 
%% D i s p l a y   O s   I n f o 
%% 
%%----------------------------------------------------------------------

display_os_info() ->
    V = case os:version() of
	    {Major, Minor, Release} ->
		lists:flatten(
		  io_lib:format("~w.~w.~w", [Major, Minor, Release]));
	    Str ->
		Str
	end,
    case os:type() of
	{OsFam, OsName} ->
	    io:format("OS:                  ~p-~p: ~s~n", [OsFam, OsName, V]);
	OsFam ->
	    io:format("OS:                  ~p: ~s~n", [OsFam, V])
    end.
	    

%%----------------------------------------------------------------------
%% 
%% D i s p l a y   S y s t e m   I n f o 
%% 
%%----------------------------------------------------------------------

display_system_info() ->
    SysArch       = system_architecture(),
    OtpRel        = otp_release(),
    SysVer        = system_version(),
    SysHT         = heap_type(),
    SysSMP        = smp_support(),
    SysNumSched   = schedulers(),
    SysProcLimit  = process_limit(),
    SysThreads    = threads(),
    SysTPSz       = thread_pool_size(),
    SchedBindings = scheduler_bindings(),
    SchedBindType = scheduler_bind_type(),
    CpuTopology   = cpu_topology(), 
    io:format("System architecture: ~s~n", [SysArch]),
    io:format("OTP release:         ~s~n", [OtpRel]),
    io:format("System version:      ~s~n", [SysVer]),
    io:format("Heap type:           ~s~n", [SysHT]),
    io:format("Thread support:      ~s~n", [SysThreads]),
    io:format("Thread pool size:    ~s~n", [SysTPSz]),
    io:format("Process limit:       ~s~n", [SysProcLimit]),
    io:format("SMP support:         ~s~n", [SysSMP]),
    io:format("Num schedulers:      ~s~n", [SysNumSched]),
    io:format("Scheduler bindings:  ~s~n", [SchedBindings]),
    io:format("Scheduler bind type: ~s~n", [SchedBindType]),
    io:format("Cpu topology:        ~s~n", [CpuTopology]),
    ok.


system_architecture() ->
    string:strip(system_info(system_architecture, string),right,$\n).

otp_release() ->
    system_info(otp_release, string).

system_version() ->
    string:strip(system_info(system_version, string),right,$\n).

heap_type() ->
    system_info(heap_type, any).

smp_support() ->
    system_info(smp_support, any).

schedulers() ->
    system_info(schedulers, any).

process_limit() ->
    system_info(process_limit, any).

threads() ->
    system_info(threads, any).

thread_pool_size() ->
    system_info(thread_pool_size, any).

scheduler_bindings() ->
    system_info(scheduler_bindings, any).

scheduler_bind_type() ->
    system_info(scheduler_bind_type, any).

cpu_topology() ->
    system_info(cpu_topology, any).

system_info(Tag, Type) ->
    case (catch erlang:system_info(Tag)) of
	{'EXIT', _} ->
	    "-";
	Info when is_list(Info) andalso (Type =:= string) ->
	    Info;
	Info ->
	    lists:flatten(io_lib:format("~w", [Info]))
    end.



%%----------------------------------------------------------------------
%% 
%% D i s p l a y   A l l o c a t o r   I n f o 
%% 
%%----------------------------------------------------------------------

display_alloc_info() ->
    io:format("Allocator memory information:~n", []),
    AllocInfo = alloc_info(),
    display_alloc_info(AllocInfo).

display_alloc_info([]) ->
    ok;
display_alloc_info([{Alloc, Mem}|AllocInfo]) ->
    io:format("  ~15w: ~10w~n", [Alloc, Mem]),
    display_alloc_info(AllocInfo).

alloc_info() ->
    case erlang:system_info(allocator) of
        {_Allocator, _Version, Features, _Settings} ->
            alloc_info(Features);
        _ ->
            []
    end.

alloc_info(Allocators) ->
    Allocs = [temp_alloc, sl_alloc, std_alloc, ll_alloc, eheap_alloc,
              ets_alloc, binary_alloc, driver_alloc],
    alloc_info(Allocators, Allocs, []).

alloc_info([], _, Acc) ->
    lists:reverse(Acc);
alloc_info([Allocator | Allocators], Allocs, Acc) ->
    case lists:member(Allocator, Allocs) of
        true ->
            Instances0 = erlang:system_info({allocator, Allocator}),
            Instances =
                if
                    is_list(Instances0) ->
                        [Instance || Instance <- Instances0,
                                     element(1, Instance) =:= instance];
                    true ->
                        []
                end,
            AllocatorMem = alloc_mem_info(Instances),
            alloc_info(Allocators, Allocs, [{Allocator, AllocatorMem} | Acc]);

        false ->
            alloc_info(Allocators, Allocs, Acc)
    end.


alloc_mem_info(Instances) ->
    alloc_mem_info(Instances, []).

alloc_mem_info([], Acc) ->
    lists:sum([Mem || {instance, _, Mem} <- Acc]);
alloc_mem_info([{instance, N, Info}|Instances], Acc) ->
    InstanceMemInfo = alloc_instance_mem_info(Info),
    alloc_mem_info(Instances, [{instance, N, InstanceMemInfo} | Acc]).

alloc_instance_mem_info(InstanceInfo) ->
    MBCS = alloc_instance_mem_info(mbcs, InstanceInfo),
    SBCS = alloc_instance_mem_info(sbcs, InstanceInfo),
    MBCS + SBCS.

alloc_instance_mem_info(Key, InstanceInfo) ->
    case lists:keysearch(Key, 1, InstanceInfo) of
        {value, {Key, Info}} ->
            case lists:keysearch(blocks_size, 1, Info) of
                {value, {blocks_size, Mem, _, _}} ->
                    Mem;
                _ ->
                    0
            end;
        _ ->
            0
    end.


%%----------------------------------------------------------------------
%% 
%% D i s p l a y   A p p   I n f o 
%% 
%%----------------------------------------------------------------------

display_app_info() ->
    display_megaco_info(),
    display_asn1_info().

display_megaco_info() ->
    MI = megaco:module_info(),
    {value, {attributes, Attr}} = lists:keysearch(attributes, 1, MI),
    {value, {app_vsn,    Ver}}  = lists:keysearch(app_vsn, 1, Attr),
    io:format("Megaco version:      ~s~n", [Ver]).

display_asn1_info() ->
    AI = megaco_ber__media_gateway_control_v1:info(),
    Vsn = 
	case lists:keysearch(vsn, 1, AI) of
	    {value, {vsn, V}} when is_atom(V) ->
		atom_to_list(V);
	    {value, {vsn, V}} when is_list(V) ->
		V;
	    _ ->
		"unknown"
	end,
    io:format("ASN.1 version:       ~s~n", [Vsn]).


%%----------------------------------------------------------------------
%% 
%% E x p a n d   M e s s a g e s
%% 
%%----------------------------------------------------------------------

expanded_messages(Codecs, DrvInclude) ->
    MessagePackage = time_test, 
    expanded_messages(MessagePackage, Codecs, DrvInclude).

expanded_messages(MessagePackage, Codecs, DrvInclude) ->
    ECodecs  = expand_codecs(Codecs, DrvInclude), 
    Messages = megaco_codec_transform:messages(MessagePackage), 
    expanded_messages2(ECodecs, Messages, []).

expanded_messages2([], _Messages, EMessages) ->
    lists:reverse(EMessages);
expanded_messages2([{Codec, Mod, Conf}|ECodecs], Messages, EMessages) ->
    case lists:keysearch(Codec, 1, Messages) of
	{value, {Codec, Msgs}} ->
	    expanded_messages2(ECodecs, Messages, 
			       [{Codec, Mod, Conf, Msgs}|EMessages]);
	false ->
	    exit({error, {no_such_codec_data, Codec}})
    end.


%%----------------------------------------------------------------------
%% 
%% E x p a n d   C o d e c s
%% 
%%----------------------------------------------------------------------

expand_codecs(Codecs, DrvInclude) ->
    expand_codecs(Codecs, DrvInclude, []).

expand_codecs([], _, ECodecs) ->
    lists:reverse(lists:flatten(ECodecs));
expand_codecs([Codec|Codecs], DrvInclude, ECodecs) when is_atom(Codec) ->
    ECodec = expand_codec(Codec, DrvInclude),
    expand_codecs(Codecs, DrvInclude, [ECodec|ECodecs]).

expand_codec(Codec, flex) ->
    case Codec of
	pretty ->
	    [{Codec, megaco_pretty_text_encoder, [flex_scanner]},
	     {Codec, megaco_pretty_text_encoder, [flex_scanner]},
	     {Codec, megaco_pretty_text_encoder, [flex_scanner]},
	     {Codec, megaco_pretty_text_encoder, [flex_scanner]},
	     {Codec, megaco_pretty_text_encoder, [flex_scanner]},
	     {Codec, megaco_pretty_text_encoder, [flex_scanner]},
	     {Codec, megaco_pretty_text_encoder, [flex_scanner]},
	     {Codec, megaco_pretty_text_encoder, [flex_scanner]}];
	compact ->
	    [{Codec, megaco_compact_text_encoder, [flex_scanner]},
	     {Codec, megaco_compact_text_encoder, [flex_scanner]},
	     {Codec, megaco_compact_text_encoder, [flex_scanner]},
	     {Codec, megaco_compact_text_encoder, [flex_scanner]},
	     {Codec, megaco_compact_text_encoder, [flex_scanner]},
	     {Codec, megaco_compact_text_encoder, [flex_scanner]},
	     {Codec, megaco_compact_text_encoder, [flex_scanner]},
	     {Codec, megaco_compact_text_encoder, [flex_scanner]}];
	ber ->
	    [];
	per ->
	    [];
	erlang ->
	    [];
	Else ->
	    error({invalid_codec, Else})
    end;
expand_codec(Codec, only_drv) ->
    case Codec of
	pretty ->
	    [{Codec, megaco_pretty_text_encoder, [flex_scanner]},
	     {Codec, megaco_pretty_text_encoder, [flex_scanner]}];
	compact ->
	    [{Codec, megaco_compact_text_encoder, [flex_scanner]},
	     {Codec, megaco_compact_text_encoder, [flex_scanner]}];
	ber ->
	    [{Codec, megaco_ber_encoder, [native]},
	     {Codec, megaco_ber_encoder, []}];
	per ->
	    [{Codec, megaco_per_encoder, [native]},
	     {Codec, megaco_per_encoder, []}];
	erlang ->
	    Encoder = megaco_erl_dist_encoder,
	    [
	     {Codec, Encoder, [megaco_compressed,compressed]},
	     {Codec, Encoder, [compressed]},
	     {Codec, Encoder, [megaco_compressed,compressed]},
	     {Codec, Encoder, [compressed]}
	    ];
	Else ->
	    error({invalid_codec, Else})
    end;
expand_codec(Codec, no_drv) ->
    case Codec of
	pretty ->
	    [{Codec, megaco_pretty_text_encoder, []},
	     {Codec, megaco_pretty_text_encoder, []}];
	compact ->
	    [{Codec, megaco_compact_text_encoder, []},
	     {Codec, megaco_compact_text_encoder, []}];
	ber ->
	    [{Codec, megaco_ber_encoder, [native]},
	     {Codec, megaco_ber_encoder, []}];
	per ->
	    [{Codec, megaco_per_encoder, [native]},
	     {Codec, megaco_per_encoder, []}];
	erlang ->
	    Encoder = megaco_erl_dist_encoder,
	    [
	     {Codec, Encoder, [megaco_compressed]},
 	     {Codec, Encoder, []},
	     {Codec, Encoder, [megaco_compressed]},
 	     {Codec, Encoder, []}
	    ];
	Else ->
	    error({invalid_codec, Else})
    end;
expand_codec(Codec, _) ->
    case Codec of
	pretty ->
	    [{Codec, megaco_pretty_text_encoder, [flex_scanner]},
	     {Codec, megaco_pretty_text_encoder, []}];
	compact ->
	    [{Codec, megaco_compact_text_encoder, [flex_scanner]},
	     {Codec, megaco_compact_text_encoder, []}];
	ber ->
	    [{Codec, megaco_ber_encoder, [native]},
	     {Codec, megaco_ber_encoder, []}];
	per ->
	    [{Codec, megaco_per_encoder, [native]},
	     {Codec, megaco_per_encoder, []}];
	erlang ->
	    Encoder = megaco_erl_dist_encoder,
	    [
	     {Codec, Encoder, [megaco_compressed,compressed]},
	     {Codec, Encoder, [compressed]},
	     {Codec, Encoder, [megaco_compressed]},
 	     {Codec, Encoder, []}
	    ];
	Else ->
	    error({invalid_codec, Else})
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%----------------------------------------------------------------------
%% 
%% S t a r t   F l e x   S c a n n e r   H a n d l e r
%% 
%%----------------------------------------------------------------------

start_flex_scanner() ->
    Pid = proc_lib:spawn(?MODULE, flex_scanner_handler, [self()]),
    receive
        {flex_scanner_started, Pid, Conf} ->
            {Pid, [Conf]};
        {flex_scanner_error, {failed_loading_flex_scanner_driver, Reason}} ->
            error({failed_loading_flex_scanner_driver, Reason});
        {flex_scanner_error, Reason} ->
            error({failed_loading_flex_scanner_driver, Reason})
    after 10000 ->
            exit(Pid, kill),
            error({failed_starting_flex_scanner, timeout})
    end.

%%----------------------------------------------------------------------
%% 
%% S t o p   F l e x   S c a n n e r   H a n d l e r
%% 
%%----------------------------------------------------------------------

stop_flex_scanner(Pid) ->
    Pid ! stop_flex_scanner.

flex_scanner_handler(Pid) ->
    case (catch megaco_flex_scanner:start()) of
        {ok, PortOrPorts} ->
            Pid ! {flex_scanner_started, self(), {flex, PortOrPorts}},
            flex_scanner_handler_loop(Pid, PortOrPorts);
        {error, {load_driver, {open_error, Reason}}} ->
            Error = {failed_loading_flex_scanner_driver, Reason},
            Pid ! {flex_scanner_error, Error},
            exit(Error);
        Else ->
            Error = {unknown_result_from_start_flex_scanner, Else},
            Pid ! {flex_scanner_error, Error},
            exit(Error)
    end.

flex_scanner_handler_loop(Pid, PortOrPorts) ->
    receive
        {ping, Pinger} ->
            Pinger ! {pong, self()},
            flex_scanner_handler_loop(Pid, PortOrPorts);
        {'EXIT', Port, Reason} when (Port =:= PortOrPorts) ->
            Pid ! {flex_scanner_exit, Reason},
            exit({flex_scanner_exit, Reason});
        {'EXIT', Port, Reason} when is_port(Port) ->
	    case megaco_flex_scanner:is_scanner_port(Port, PortOrPorts) of
		true ->
		    Pid ! {flex_scanner_exit, Reason},
		    exit({flex_scanner_exit, Reason});
		false ->
		    %% Just ignore this crap
		    flex_scanner_handler_loop(Pid, PortOrPorts)
	    end;
        stop_flex_scanner ->
            megaco_flex_scanner:stop(PortOrPorts),
            exit(normal);
        _Other ->
            flex_scanner_handler_loop(Pid, PortOrPorts)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

error(Reason) ->
    throw({error, Reason}).

