%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2025-2025. All Rights Reserved.
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

%% Note that some of the functions exported by prim_net only works on unnix
%% and others only on Windows.

%% SUITE = prim_net_SUITE
%% Run the entire test suite: 
%% ts:run(kernel, prim_net_SUITE, [batch]).
%%
%% Run a specific group:
%% ts:run(kernel, prim_net_SUITE, {group, foo}, [batch]).
%%
%% Run a specific test case:
%% ts:run(kernel, prim_net_SUITE, foo, [batch]).
%%
%% (cd /mnt/c/$LOCAL_TESTS/26/kernel_test/ && $ERL_TOP/bin/win32/erl.exe -sname kernel-26-tester -pa c:$LOCAL_TESTS/26/test_server)
%% application:set_env(kernel, test_inet_backends, true).
%%
%% S = fun() -> ts:run(kernel, prim_net_SUITE, [batch]) end.
%% S = fun(SUITE) -> ts:run(kernel, SUITE, [batch]) end.
%% G = fun(GROUP) -> ts:run(kernel, prim_net_SUITE, {group, GROUP}, [batch]) end.
%% G = fun(SUITE, GROUP) -> ts:run(kernel, SUITE, {group, GROUP}, [batch]) end.
%% T = fun(TC) -> ts:run(kernel, prim_net_SUITE, TC, [batch]) end.
%%
%% S = fun() -> ct:run_test([{suite, prim_net_SUITE}]) end.
%% S = fun(SUITE) -> ct:run_test([{suite, SUITE}]) end.
%% G = fun(GROUP) -> ct:run_test([{suite, prim_net_SUITE}, {group, GROUP}]) end.
%% G = fun(SUITE, GROUP) -> ct:run_test([{suite, SUITE}, {group, GROUP}]) end.
%% T = fun(TC) -> ct:run_test([{suite, prim_net_SUITE}, {testcase, TC}]) end.
%% T = fun(S, TC) -> ct:run_test([{suite, S}, {testcase, TC}]) end.
%% T = fun(G, TC) -> ct:run_test([{suite, prim_net_SUITE}, {group, G}, {testcase, TC}]) end.
%% T = fun(S, G, TC) -> ct:run_test([{suite, S}, {group, G}, {testcase, TC}]) end.
%%
%% Some official info about AF_UNIX
%% https://devblogs.microsoft.com/commandline/windowswsl-interop-with-af_unix/



-module(prim_net_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").
-include("socket_test_evaluator.hrl").
-include("kernel_test_lib.hrl").

%% Suite exports
-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1,    end_per_suite/1,
         init_per_group/2,    end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
         get_adapters_addresses/1,
         get_if_entry/1,
	 get_interface_info/1
        ]).


%% Internal exports
%% -export([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [{ct_hooks, [ts_install_cth]},
     {timetrap, {minutes,1}}].

all() -> 
    [
     {group, misc}
    ].


groups() ->     
    [
     {misc, [], misc_cases()}
    ].


misc_cases() ->
    [
     get_adapters_addresses,
     get_if_entry,
     get_interface_info
    ].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(Config0) ->
    ?P("init_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),
    
    try net:info() of
        #{} ->
            case ?LIB:init_per_suite(Config0) of
                {skip, _} = SKIP ->
                    SKIP;

                Config1 when is_list(Config1) ->

                    ?P("init_per_suite -> end when "
                       "~n      Config: ~p", [Config1]),

                    ?ENSURE_NOT_DOG_SLOW(Config1, 15),

                    %% We need a monitor on this node also
                    kernel_test_sys_monitor:start(),

                    Config1
            end
    catch
        error : notsup ->
            {skip, "enet not supported"};
        error : undef ->
            {skip, "enet not configured"}
    end.

end_per_suite(Config0) ->

    ?P("end_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    %% Stop the local monitor
    kernel_test_sys_monitor:stop(),

    Config1 = ?LIB:end_per_suite(Config0),

    ?P("end_per_suite -> "
       "~n      Nodes: ~p", [erlang:nodes()]),

    Config1.


init_per_group(_GroupName, Config) ->
    io:format("init_per_group(~w) -> entry with"
              "~n   Config: ~p"
              "~n", [_GroupName, Config]),
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_TC, Config) ->
    io:format("init_per_testcase(~w) -> entry with"
              "~n   Config: ~p"
              "~n", [_TC, Config]),
    Config.

end_per_testcase(_TC, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                               MISC                                  %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% There is no way we can test every possible combination of args,
%% so we just test a new simple examples.

get_adapters_addresses(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(?FUNCTION_NAME,
	   fun() -> ok end,
           fun() ->
                   ok = do_get_adapters_addresses()
           end).


do_get_adapters_addresses() ->
    L = [
	 #{},
         default,
         no_skips_all_includes,
	 no_skips_no_includes,
         all_skips_no_includes,
	 all_skips_all_includes,
	 %% We only test this one thing; *one* flag
	 %% In the case of addresses, we also add family
	 #{flags => #{skip_unicast => true}},
	 #{flags => #{skip_unicast => false}},
	 #{family => unspec,
	   flags => #{skip_unicast => false}},
	 #{family => inet,
	   flags => #{skip_unicast => false}},
	 #{family => inet6,
	   flags => #{skip_unicast => false}},
	 #{flags => #{skip_anycast => true}},
	 #{flags => #{skip_anycast => false}},
	 #{family => unspec,
	   flags => #{skip_anycast => false}},
	 #{family => inet,
	   flags => #{skip_anycast => false}},
	 #{family => inet6,
	   flags => #{skip_anycast => false}},
	 #{flags => #{skip_multicast => true}},
	 #{flags => #{skip_multicast => false}},
	 #{flags => #{skip_dns_server => true}},
	 #{flags => #{skip_dns_server => false}},
	 #{family => unspec,
	   flags => #{skip_dns_server => false}},
	 #{family => inet,
	   flags => #{skip_dns_server => false}},
	 #{family => inet6,
	   flags => #{skip_dns_server => false}},
	 #{flags => #{include_prefix => true}},
	 #{flags => #{include_prefix => false}},
	 #{family => unspec,
	   flags => #{include_prefix => false}},
	 #{family => inet,
	   flags => #{include_prefix => false}},
	 #{family => inet6,
	   flags => #{include_prefix => false}}%% ,
	 %% #{flags => #{include_wins_info => true}},
	 %% #{flags => #{include_gateways => true}}
	],
    try
        do_get_adapters_addresses(L)
    catch
        error:notsup = NOTSUP ->
            skip(NOTSUP)
    end.

do_get_adapters_addresses([]) ->
    ok;
do_get_adapters_addresses([H | T]) ->
    ?P("try gaa with: "
       "~n   Args: ~p", [H]),    
    case prim_net:get_adapters_addresses(H) of
        {ok, Adapters} when is_list(Adapters) ->
	    gaa_verify_adapter_addrs(H, Adapters),
            do_get_adapters_addresses(T);

        {error, Reason} ->
	    ?P("unexpected failure: "
	       "~n   Flags:  ~p"
	       "~n   Reason: ~p", [H, Reason]),
            exit({unexpected_failure, Reason})
    end.


%% This is **very** basic
gaa_verify_adapter_addrs(default = _Filter,
			 Adapters)
  when is_list(Adapters) andalso (Adapters =/= []) ->
    ok;
gaa_verify_adapter_addrs(no_skips_all_includes = _Filter,
			 Adapters)
  when is_list(Adapters) andalso (Adapters =/= []) ->
    ok;
gaa_verify_adapter_addrs(no_skips_no_includes = _Filter,
			 Adapters)
  when is_list(Adapters) andalso (Adapters =/= []) ->
    ok;
gaa_verify_adapter_addrs(all_skips_no_includes = _Filter,
			 Adapters)
  when is_list(Adapters) andalso (Adapters =/= []) ->
    ok;
gaa_verify_adapter_addrs(all_skips_all_includes = _Filter,
			 Adapters)
  when is_list(Adapters) andalso (Adapters =/= []) ->
    ok;
gaa_verify_adapter_addrs(#{family := Fam,
			   flags  := Flags},
			 Adapters) ->
    gaa_verify_adapter_addrs_flags(Adapters, Flags, Fam);
gaa_verify_adapter_addrs(#{flags  := Flags},
			 Adapters) ->
    gaa_verify_adapter_addrs_flags(Adapters, Flags, undefined);
gaa_verify_adapter_addrs(_, _) ->
    ok.

gaa_verify_adapter_addrs_flags(Adapters, Flags, Fam) ->
    case Flags of
	#{skip_anycast := SkipAC} ->
	    gaa_verify_anycast(Adapters, SkipAC, Fam);
	#{skip_unicast := SkipUC} ->
	    gaa_verify_unicast(Adapters, SkipUC, Fam);
	#{skip_multicast := SkipMC} ->
	    gaa_verify_multicast(Adapters, SkipMC, Fam);
	#{skip_dns_server := SkipDS} ->
	    gaa_verify_dns_servers(Adapters, SkipDS, Fam);
	#{include_prefix := IncPref} ->
	    gaa_verify_include_prefix(Adapters, IncPref, Fam)%% ;
	%% #{include_wins_info := true} ->
	%%     ?P("include_wins_info ->"
	%%        "~n   ~p", [Adapters]),
	%%     ok;
	%% #{include_gateways := true} ->
	%%     ?P("include_gateways ->"
	%%        "~n   ~p", [Adapters]),
	%%     ok
    end.
    

gaa_verify_anycast([], __SkipAC, _Fam) ->
    ok;
gaa_verify_anycast([#{name          := Name,
		      anycast_addrs := []} | Adapters],
		   SkipAC, Fam) when (SkipAC =:= true) ->
    ?P("(no) anycast addrs verified for ~p", [Name]),
    gaa_verify_anycast(Adapters, SkipAC, Fam);
gaa_verify_anycast([#{name          := Name,
		      anycast_addrs := Addrs} | Adapters],
		   SkipAC, Fam) ->
    gaa_verify_addrs(Addrs, Fam),
    ?P("anycast addrs (~w) verified for ~p:"
       "~n   ~p", [Fam, Name, Addrs]),
    gaa_verify_anycast(Adapters, SkipAC, Fam);
gaa_verify_anycast([#{name := Name} = Adapter | _Adapters], _SkipAC, _Fam) ->
    ?P("anycast verification failed for ~p: "
       "~n   ~p", [Name, Adapter]),
    exit({unexpected_anycast, Name}).


gaa_verify_unicast([], _SkipUC, _Fam) ->
    ok;
gaa_verify_unicast([#{name          := Name,
		      unicast_addrs := []} | Adapters],
		   SkipUC, Fam) when (SkipUC =:= true) ->
    ?P("(no) unicast addrs verified for ~p", [Name]),
    gaa_verify_unicast(Adapters, SkipUC, Fam);
gaa_verify_unicast([#{name          := Name,
		      unicast_addrs := Addrs} | Adapters],
		   SkipAC, Fam) ->
    gaa_verify_addrs(Addrs, Fam),
    ?P("unicast addrs (~w) verified for ~p:"
       "~n   ~p", [Fam, Name, Addrs]),
    gaa_verify_unicast(Adapters, SkipAC, Fam);
gaa_verify_unicast([#{name := Name} = Adapter| _Adapters],
		   _SkipUC, _Fam) ->
    ?P("unicast verification failed for ~p: "
       "~n   ~p", [Name, Adapter]),
    exit({unexpected_unicast, Name}).


gaa_verify_multicast([], _SkipMC, _Fam) ->
    ok;
gaa_verify_multicast([#{name            := Name,
			multicast_addrs := []} | Adapters],
		     SkipMC, Fam) when (SkipMC =:= true) ->
    ?P("(no) multicast addrs verified for ~p", [Name]),
    gaa_verify_multicast(Adapters, SkipMC, Fam);
gaa_verify_multicast([#{name            := Name,
			multicast_addrs := Addrs} | Adapters],
		     SkipMC, Fam) ->
    gaa_verify_addrs(Addrs, Fam),
    ?P("multicast addrs (~w) verified for ~p:"
       "~n   ~p", [Fam, Name, Addrs]),
    gaa_verify_multicast(Adapters, SkipMC, Fam);
gaa_verify_multicast([#{name := Name} = Adapter | _Adapters],
		     _SkipMC, Fam) ->
    ?P("multicast (~w) verification failed for ~p: "
       "~n   ~p", [Fam, Name, Adapter]),
    exit({unexpected_multicast, Name}).


gaa_verify_dns_servers([], _SkipMC, _Fam) ->
    ok;
gaa_verify_dns_servers([#{name        := Name,
			  dns_server_addrs := []} | Adapters],
		       SkipDS, Fam) when (SkipDS =:= true) ->
    ?P("(no) dns-servers verified for ~p", [Name]),
    gaa_verify_dns_servers(Adapters, SkipDS, Fam);
gaa_verify_dns_servers([#{name             := Name,
			  dns_server_addrs := Addrs} | Adapters],
		       SkipDS, Fam) ->
    gaa_verify_addrs(Addrs, Fam),
    ?P("dns-servers (~w) verified for ~p:"
       "~n   ~p", [Fam, Name, Addrs]),
    gaa_verify_dns_servers(Adapters, SkipDS, Fam);
gaa_verify_dns_servers([#{name := Name} = Adapter | _Adapters],
		       SkipDS, Fam) ->
    ?P("dns-servers (~w) verification failed for ~p: "
       "~n   SkipDS:  ~p"
       "~n   Adapter: ~p", [Fam, Name, SkipDS, Adapter]),
    exit({unexpected_dns_servers, Name}).


gaa_verify_include_prefix([], _IncPref, _Fam) ->
    ok;
gaa_verify_include_prefix([#{name     := Name,
			     prefixes := []} | Adapters],
			  IncPref, Fam) when (IncPref =:= false) ->
    ?P("(do not) include-prefix verified for ~p", [Name]),
    gaa_verify_include_prefix(Adapters, IncPref, Fam);
gaa_verify_include_prefix([#{name     := Name,
			     prefixes := Prefs} | Adapters],
			  IncPref, Fam) ->
    %% Prefixes are not the same as addresses,
    %% but content is similar enough that we can use the same function...
    gaa_verify_addrs(Prefs, Fam),
    ?P("include-prefix (~w) verified for ~p:"
       "~n   ~p", [Fam, Name, Prefs]),
    gaa_verify_include_prefix(Adapters, IncPref, Fam);
gaa_verify_include_prefix([#{name := Name} = Adapter | _Adapters],
			  IncPref, Fam) ->
    ?P("include-prefix (~w) verification failed for ~p: "
       "~n   IncPref: ~p"
       "~n   Adapter: ~p", [Fam, Name, IncPref, Adapter]),
    exit({unexpected_include_prefix, Name}).


gaa_verify_addrs([], _Fam) ->
    ok;
gaa_verify_addrs([Addr|Addrs], FilterFam) ->
    gaa_verify_addr(Addr, FilterFam),
    gaa_verify_addrs(Addrs, FilterFam).

gaa_verify_addr(#{addr := #{addr   := _,
			    family := _}},
		undefined = _Fam) ->
    ok;
gaa_verify_addr(#{addr := #{addr   := Addr,
			    family := Fam}},
		FilterFam)
  when (FilterFam =:= inet) andalso
       (FilterFam =:= Fam) andalso
       is_tuple(Addr) andalso (tuple_size(Addr) =:= 4) ->
    ok;
gaa_verify_addr(#{addr := #{addr   := Addr,
			    family := Fam}},
		FilterFam)
  when (FilterFam =:= inet6) andalso
       (FilterFam =:= Fam) andalso
       is_tuple(Addr) andalso (tuple_size(Addr) =:= 8) ->
    ok;
%% If filter-fam is unspec, the address family can be *either* inet *or* inet6
gaa_verify_addr(#{addr := #{addr   := Addr,
			    family := Fam}},
		FilterFam)
  when (FilterFam =:= unspec) andalso
       (((Fam =:= inet) andalso
	 is_tuple(Addr) andalso (tuple_size(Addr) =:= 4)) orelse
	((Fam =:= inet6) andalso
	 is_tuple(Addr) andalso (tuple_size(Addr) =:= 8))) ->
    ok;
gaa_verify_addr(Addr, FilterFam) ->
    ?P("Failed verify address: "
       "~n   Filter Family: ~p"
       "~n   Address:       ~p", [Addr, FilterFam]),
    exit({unexpected_address, FilterFam}).
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% There is no way we can test every possible combination of args,
%% so we just test a new simple examples.

get_if_entry(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(?FUNCTION_NAME,
	   fun() -> ok end,
           fun() ->
                   ok = do_get_if_entry()
           end).

do_get_if_entry() ->
    %% Just ensure we get an entry for every if
    ?P("try validate known interfaces"),
    {ok, IfNames} = net:if_names(),
    Idxs =
	case [Idx || {Idx, _} <- IfNames] of
	    [] ->
		exit({skip, no_ifs});
	    L ->
		L
	end,
    ok = do_get_if_entry(Idxs),

    %% Ask for an index that we know does not exit
    ?P("try validate *not* known interface"),
    [MaxIdx | _] = lists:reverse(lists:sort(Idxs)),
    InvalidIdx = MaxIdx + 99,
    case prim_net:get_if_entry(#{index => InvalidIdx}) of
	{error, Reason} ->
	    ?P("expected failure get if entry for index ~w:"
	       "~n   ~p", [InvalidIdx, Reason]),
	    ok;
	{ok, Entry} ->
	    ?P("unexpected success requesting non-existing if entry ~w:"
	       "~n   ~p", [InvalidIdx, Entry]),
	    exit(unexpected_success)
    end.


do_get_if_entry([]) ->
    ok;
do_get_if_entry([Idx|Idxs]) ->
    case prim_net:get_if_entry(#{index => Idx}) of
	{ok, Entry} ->
	    ?P("expected success retreiving if entry ~w: "
	       "~n   ~p", [Idx, Entry]),
	    do_get_if_entry(Idxs);
	{error, Reason} ->
	    ?P("unexpected failure retreiving if entry ~w: "
	       "~n   ~p", [Idx, Reason]),
	    exit({unexpected_failure, Idx, Reason})
    end.

   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This function gives a list of IPv4 interfaces, so its very simple.
%% We only have to ensure that the host actually supports IPv4.

get_interface_info(_Config) when is_list(_Config) ->
    ?TT(?SECS(10)),
    tc_try(?FUNCTION_NAME,
	   fun() -> ?HAS_SUPPORT_IPV4() end,
           fun() ->
                   ok = do_get_interface_info()
           end).

do_get_interface_info() ->
    case prim_net:get_interface_info(#{debug => false}) of
	{ok, IFs} ->
	    gii_verify_result(IFs);
	{error, Reason} ->
	    exit(Reason)
    end.


gii_verify_result([]) ->
    ok;
gii_verify_result([IF | IFs]) ->
    case IF of
	#{index := Idx, name := Name} ->
	    ?P("verified: "
	       "~n   Index: ~p"
	       "~n   Name:  ~p", [Idx, Name]),
	    gii_verify_result(IFs);
	_ ->
	    ?P("unexpected interface info: "
	       "~n   ~p", [IF]),
	    exit(unpexpected_interface_info)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% local_host() ->
%%     try net_adm:localhost() of
%%         Host when is_list(Host) ->
%% 	    %% Convert to shortname if long
%% 	    case string:tokens(Host, [$.]) of
%% 		[H|_] ->
%% 		    list_to_atom(H)
%% 	    end
%%     catch
%%         C:E:S ->
%%             erlang:raise(C, E, S)
%%     end.


%% This gets the local address (not 127.0...)
%% We should really implement this using the (new) net module,
%% but until that gets the necessary functionality...
%% which_local_addr(Domain) ->
%%     ?LIB:which_local_addr(Domain).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% monitored_by() ->
%%     monitored_by(self()).
%% monitored_by(Pid) ->	
%%     {monitored_by, Refs} = erlang:process_info(Pid, monitored_by),
%%     Refs.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% not_supported(What) ->
%%     skip({not_supported, What}).

%% not_yet_implemented() ->
%%     skip("not yet implemented").

skip(Reason) ->
    throw({skip, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% *** tc_try/2,3 ***
%% Case:      Basically the test case name
%% TCCondFun: A fun that is evaluated before the actual test case
%%            The point of this is that it can performs checks to
%%            see if we shall run the test case at all.
%%            For instance, the test case may only work in specific
%%            conditions.
%% FCFun:     The test case fun
%% tc_try(Case, TCFun) ->
%%     ?TC_TRY(Case, TCFun).

tc_try(Case, TCCondFun, TCFun) ->
    ?TC_TRY(Case, TCCondFun, TCFun).
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% i(F) ->
%%     i(F, []).

%% i(F, A) ->
%%     FStr = ?F("[~s] " ++ F, [?FTS()|A]),
%%     io:format(user, FStr ++ "~n", []),
%%     io:format(FStr, []).

