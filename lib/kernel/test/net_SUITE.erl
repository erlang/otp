%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2019-2022. All Rights Reserved.
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
%% This test suite is basically a "placeholder" for a proper test suite...
%% Also we should really call prim_net directly, and not net (since that does
%% not even reside here).
%%

%% Run the entire test suite: 
%% ts:run(emulator, net_SUITE, [batch]).
%%
%% Run a specific group:
%% ts:run(emulator, net_SUITE, {group, foo}, [batch]).
%%
%% Run a specific test case:
%% ts:run(emulator, net_SUITE, foo, [batch]).

-module(net_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").
-include("kernel_test_lib.hrl").

%% Suite exports
-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1,    end_per_suite/1,
         init_per_group/2,    end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
         %% *** API Basic ***
         api_b_gethostname/1,
         api_b_getifaddrs/1,
         api_b_name_and_addr_info/1,
         
         api_b_name_and_index/1,

         %% *** API Misc ***
         api_m_getaddrinfo_v4/0,
         api_m_getaddrinfo_v4/1,
         api_m_getaddrinfo_v6/0,
         api_m_getaddrinfo_v6/1,

         api_m_getnameinfo_v4/0,
         api_m_getnameinfo_v4/1,
         api_m_getnameinfo_v6/0,
         api_m_getnameinfo_v6/1

         %% Tickets
        ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(FAIL(R), exit(R)).
-define(SKIP(R), throw({skip, R})).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    Groups = [{api, "ENET_TEST_API", include}],
    [use_group(Group, Env, Default) || {Group, Env, Default} <- Groups].

use_group(Group, Env, Default) ->
	case os:getenv(Env) of
	    false when (Default =:= include) ->
		[{group, Group}];
	    false ->
		[];
	    Val ->
		case list_to_atom(string:to_lower(Val)) of
		    Use when (Use =:= include) orelse 
			     (Use =:= enable) orelse 
			     (Use =:= true) ->
			[{group, Group}];
		    _ ->
			[]
		end
	end.
    

groups() -> 
    [{api,       [], api_cases()},
     {api_basic, [], api_basic_cases()},
     {api_misc,  [], api_misc_cases()}

     %% {tickets, [], ticket_cases()}
    ].
     
api_cases() ->
    [
     {group, api_basic},
     {group, api_misc}
    ].

api_basic_cases() ->
    [
     api_b_gethostname,
     api_b_getifaddrs,
     api_b_name_and_addr_info,
     api_b_name_and_index
    ].

api_misc_cases() ->
    [
     api_m_getaddrinfo_v4,
     api_m_getaddrinfo_v6,
     api_m_getnameinfo_v4,
     api_m_getnameinfo_v6
    ].

%% ticket_cases() ->
%%     [].



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

                    %% We need a monitor on this node also
                    kernel_test_sys_monitor:start(),

                    Config1
            end

    catch
        error : notsup ->
            {skip, "net not supported"}
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

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.


init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, Config) ->
    Config.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%                           API BASIC                                 %%
%%                                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Get the hostname of the host.
api_b_gethostname(suite) ->
    [];
api_b_gethostname(doc) ->
    [];
api_b_gethostname(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(api_b_gethostname,
           fun() ->
                   ok = api_b_gethostname()
           end).


api_b_gethostname() ->
    case net:gethostname() of
        {ok, Hostname} ->
            i("hostname: ~s", [Hostname]),
            ok;
        {error, enotsup = Reason} ->
            i("gethostname not supported - skipping"),
            skip(Reason);
        {error, Reason} ->
            ?FAIL(Reason)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a *very* basic test. It simply calls the function and expect
%% it to succeed...
api_b_getifaddrs(suite) ->
    [];
api_b_getifaddrs(doc) ->
    [];
api_b_getifaddrs(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(api_b_getifaddrs,
           fun() ->
                   ok = api_b_getifaddrs()
           end).


api_b_getifaddrs() ->
    try net:getifaddrs() of
        {ok, IfAddrs} ->
            i("IfAddrs: "
              "~n   ~p", [IfAddrs]),
            ok;
        {error, enotsup = Reason} ->
            i("getifaddrs not supported - skipping"),
            skip(Reason);
        {error, Reason} ->
            ?FAIL(Reason)
    catch
        error : notsup = CReason ->
            Fun     = fun(F) when is_function(F, 0) ->
                              try F()
                              catch C:E:S -> {catched, {C, E, S}}
                              end
                      end,
            Res2Str = fun({ok, Res})         -> ?F("ok: ~p", [Res]);
                         ({error, Reason})   -> ?F("error: ~p", [Reason]);
                         ({catched, {C, E}}) -> ?F("catched: ~w, ~p", [C, E])
                      end,
            IIRes    = Fun(fun() -> prim_net:get_interface_info(#{}) end),
            ATRes    = Fun(fun() -> prim_net:get_ip_address_table(#{}) end),
            AARes    = Fun(fun() -> prim_net:get_adapters_addresses(#{}) end),
            IIResStr = Res2Str(IIRes),
            ATResStr = Res2Str(ATRes),
            IFERes   = win_getifaddrs_ife(IIRes, ATRes),
            AAResStr = Res2Str(AARes),
            %% Note that the prim_net module is *not* intended to 
            %% be called directly. This is just a temporary thing.
            i("~w => skipping"
              "~n   Interface Info: "
              "~n      ~s"
              "~n   IP Address Table: "
              "~n      ~s"
              "~n   MIB If Table: "
              "~n      ~p"
              "~n   Adapters Addresses: "
              "~n      ~p",
              [CReason, IIResStr, ATResStr, IFERes, AAResStr]),
            skip(CReason)
    end.

win_getifaddrs_ife({ok, II}, {ok, AT}) ->
    IDX1 = [IDX || #{index := IDX} <- II],
    IDX2 = [IDX || #{index := IDX} <- AT],
    MergedIDX = merge(IDX1, IDX2),
    MibIfTable =
        [try prim_net:get_if_entry(#{index => IDX}) of
             {ok, Entry} ->
                 Entry;
             {error, _} = ERROR ->
                 {IDX, ERROR}
         catch
             %% This is *very* unlikely since we got here because of
             %% a previous 'notsup'. But just in case...
             error : notsup = CReason ->
                 {IDX, CReason};
             C:E ->
                 {IDX, {C, E}}
         end || IDX <- MergedIDX],
    MibIfTable;
win_getifaddrs_ife(_, _) ->
    undefined.

    
merge([], L) ->
    lists:sort(L);
merge([H|T], L) ->
    case lists:member(H, L) of
        true ->
            merge(T, L);
        false ->
            merge(T, [H|L])
    end.

            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Get name and address info.
api_b_name_and_addr_info(suite) ->
    [];
api_b_name_and_addr_info(doc) ->
    [];
api_b_name_and_addr_info(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(api_b_name_and_addr_info,
           fun() ->
                   ok = api_b_name_and_addr_info()
           end).


api_b_name_and_addr_info() ->
    Domain = inet,
    Addr   = which_local_addr(Domain),
    SA     = #{family => Domain, addr => Addr},
    try
        begin
            i("try getnameinfo for"
              "~n   ~p", [SA]),
            Hostname =
                case net:getnameinfo(SA) of
                    {ok, #{host := Name, service := Service} = NameInfo} 
                      when is_list(Name) andalso is_list(Service) ->
                        i("getnameinfo: "
                          "~n   ~p", [NameInfo]),
                        Name;
                    {ok, BadNameInfo} ->
                        ?FAIL({getnameinfo, SA, BadNameInfo});
                    {error, enotsup = ReasonNI} ->
                        i("getnameinfo not supported - skipping"),
                        ?SKIP({getnameinfo, ReasonNI});
                    {error, Reason1} ->
                        ?FAIL({getnameinfo, SA, Reason1})
                end,
            i("try getaddrinfo for"
              "~n   ~p", [Hostname]),
            case net:getaddrinfo(Hostname) of
                {ok, AddrInfos} when is_list(AddrInfos) ->
                    i("getaddrinfo: "
                      "~n   ~p", [AddrInfos]),
                    verify_addr_info(AddrInfos, Domain);
                {ok, BadAddrInfo} ->
                    ?FAIL({getaddrinfo, Hostname, BadAddrInfo});
                {error, enotsup = ReasonAI} ->
                    i("getaddrinfo not supported - skipping"),
                    ?SKIP({getaddrinfo, ReasonAI});
                {error, Reason2} ->
                    ?FAIL({getaddrinfo, Hostname, Reason2})
            end
        end
    catch
        error : notsup = Reason ->
            i("~w => skipping", [Reason]),
            skip(Reason)
    end.


verify_addr_info(AddrInfos, Domain) when (AddrInfos =/= []) ->
    verify_addr_info2(AddrInfos, Domain).
    
verify_addr_info2([], _Domain) ->
    ok;
verify_addr_info2([#{addr     := #{addr   := Addr,
				   family := Domain,
				   port   := Port},
                     family   := Domain,
                     type     := _Type,
                     protocol := _Proto}|T], Domain) 
  when is_integer(Port) andalso
       (((Domain =:= inet) andalso is_tuple(Addr) andalso (size(Addr) =:= 4)) orelse
        ((Domain =:= inet6) andalso is_tuple(Addr) andalso (size(Addr) =:= 8))) -> 
    verify_addr_info2(T, Domain);
verify_addr_info2([#{family := DomainA}|T], DomainB) 
  when (DomainA =/= DomainB) ->
    %% Ignore entries for other domains
    verify_addr_info2(T, DomainB);
verify_addr_info2([BadAddrInfo|_], Domain) ->
    ?FAIL({bad_address_info, BadAddrInfo, Domain}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Verify (interface) name and index functions.
%% if_names/0,
%% if_name2index/1
%% if_index2name/1
api_b_name_and_index(suite) ->
    [];
api_b_name_and_index(doc) ->
    [];
api_b_name_and_index(_Config) when is_list(_Config) ->
    ?TT(?SECS(5)),
    tc_try(api_b_name_and_index,
           fun() ->
                   ok = api_b_name_and_index()
           end).


api_b_name_and_index() ->
    try
        begin
            Names =
                case net:if_names() of
                    {ok, N} when is_list(N) andalso (N =/= []) ->
                        N;
                    {error, enotsup = Reason} ->
                        i("if_names not supported - skipping"),
                        ?SKIP({if_names, Reason});
                    {error, Reason} ->
                        ?FAIL({if_names, Reason})
                end,
            verify_if_names(Names)
        end
    catch
        error : notsup = CReason ->
            i("~w => skipping", [CReason]),
            skip(CReason)
    end.
        

verify_if_names([]) ->
    ok;
verify_if_names([{Index, Name}|T]) ->
    case net:if_name2index(Name) of
        {ok, Index} ->
            ok;
        {ok, BadIndex} ->
            ?FAIL({if_name2index, Name, Index, BadIndex});
        {error, enotsup = Reason_N2I1} ->
            i("if_name2index not supported - skipping"),
            ?SKIP({if_name2index, Reason_N2I1});
        {error, Reason_N2I2} ->
            ?FAIL({if_name2index, Name, Reason_N2I2})
    end,
    case net:if_index2name(Index) of
        {ok, Name} ->
            ok;
        {ok, BadName} ->
            ?FAIL({if_index2name, Index, Name, BadName});
        {error, enotsup = Reason_I2N1} ->
            i("if_index2name not supported - skipping"),
            ?SKIP({if_index2name, Reason_I2N1});
        {error, Reason_I2N2} ->
            ?FAIL({if_index2name, Index, Reason_I2N2})
    end,
    verify_if_names(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_m_getaddrinfo_v4() ->
    required(v4).

api_m_getaddrinfo_v4(suite) ->
    [];
api_m_getaddrinfo_v4(doc) ->
    [];
api_m_getaddrinfo_v4(Config) when is_list(Config) ->
    ?TT(?SECS(5)),
    Pre  = fun() ->
                   {Name, FullName, IPStr, IP, Aliases,_,_} =
                       ct:get_config(test_host_ipv4_only),
                   #{name      => Name,
                     full_name => FullName,
                     ip_string => IPStr,
                     ip        => IP,
                     aliases   => Aliases,
                     family    => inet}
           end,
    Case = fun(Info) ->
                   ok = api_m_getaddrinfo(Info)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME,
           Pre, Case, Post).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_m_getaddrinfo_v6() ->
    required(v6).

api_m_getaddrinfo_v6(suite) ->
    [];
api_m_getaddrinfo_v6(doc) ->
    [];
api_m_getaddrinfo_v6(Config) when is_list(Config) ->
    ?TT(?SECS(5)),
    Pre  = fun() ->
                   {Name, FullName, IPStr, IP, Aliases} =
                       ct:get_config(test_host_ipv6_only),
                   #{name      => Name,
                     full_name => FullName,
                     ip_string => IPStr,
                     ip        => IP,
                     aliases   => Aliases,
                     family    => inet6}
           end,
    Case = fun(Info) ->
                   ok = api_m_getaddrinfo(Info)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME,
           Pre, Case, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_m_getaddrinfo(#{name   := Name,
                    family := Domain,
                    ip     := IP}) ->
    i("Check address info for ~p with"
      "~n   Domain: ~p"
      "~n   IP:     ~p", [Name, Domain, IP]),
    try net:getaddrinfo(Name) of
        {ok, AddrInfos} ->
            %% Check that we can actually find this IP in the list
            api_m_getaddrinfo_verify(AddrInfos, Name, Domain, IP);
        {error, enotsup = ReasonAI} ->
            i("getaddrinfo not supported - skipping"),
            ?SKIP({getaddrinfo, ReasonAI});
        {error, Reason} ->
            ?FAIL({gethaddrinfo, Name, Reason})
    catch
        error : notsup = Reason ->
            i("~w => skipping", [Reason]),
            skip(Reason)
    end.



%% First we filter out the address info of the correct domain (family), then:
%% 1) If there is no address info left: SKIP
%% 2) If there are address info, check for the selected address

api_m_getaddrinfo_verify(AddrInfos, Name, Domain, IP) ->
    i("Attempt to verify from ~w address-infos", [length(AddrInfos)]),
    AddrInfos2 = [AI || #{family := D} = AI <- AddrInfos, D =:= Domain],
    case AddrInfos2 of
        [] ->
            i("No address info of correct domain (~w) available", [Domain]),
            ?SKIP({no_ai_of_domain, Domain});
        _ ->
            api_m_getaddrinfo_verify2(AddrInfos, Name, Domain, IP)
    end.

api_m_getaddrinfo_verify2([], Name, Domain, IP) ->
    i("No match found for ~p: "
      "~n   Domain:   ~p"
      "~n   IP:       ~p", [Name, Domain, IP]),
    ?FAIL({not_found, Name, Domain, IP});
api_m_getaddrinfo_verify2([#{family := Domain,
                            addr   := #{addr   := IP, 
                                        family := Domain} = _SockAddr} = 
                              AddrInfo|_],
                         Name, Domain, IP) ->
    i("Found match for ~p: "
      "~n   AddrInfo: ~p"
      "~n   Domain:   ~p"
      "~n   IP:       ~p", [Name, AddrInfo, Domain, IP]),
    ok;
api_m_getaddrinfo_verify2([AddrInfo|AddrInfos], Name, Domain, IP) ->
    i("No match: "
      "~n   AddrInfo: ~p"
      "~n   Domain: ~p"
      "~n   IP:     ~p", [AddrInfo, Domain, IP]),
    api_m_getaddrinfo_verify2(AddrInfos, Name, Domain, IP).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_m_getnameinfo_v4() ->
    required(v4).

api_m_getnameinfo_v4(suite) ->
    [];
api_m_getnameinfo_v4(doc) ->
    [];
api_m_getnameinfo_v4(Config) when is_list(Config) ->
    ?TT(?SECS(5)),
    Pre  = fun() ->
                   {Name, FullName, IPStr, IP, Aliases,_,_} =
                       ct:get_config(test_host_ipv4_only),
                   #{name      => Name,
                     full_name => FullName,
                     ip_string => IPStr,
                     ip        => IP,
                     aliases   => Aliases,
                     family    => inet}
           end,
    Case = fun(Info) ->
                   ok = api_m_getnameinfo(Info)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME,
           Pre, Case, Post).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_m_getnameinfo_v6() ->
    required(v6).

api_m_getnameinfo_v6(suite) ->
    [];
api_m_getnameinfo_v6(doc) ->
    [];
api_m_getnameinfo_v6(Config) when is_list(Config) ->
    ?TT(?SECS(5)),
    Pre  = fun() ->
                   {Name, FullName, IPStr, IP, Aliases} =
                       ct:get_config(test_host_ipv6_only),
                   #{name      => Name,
                     full_name => FullName,
                     ip_string => IPStr,
                     ip        => IP,
                     aliases   => Aliases,
                     family    => inet6}
           end,
    Case = fun(Info) ->
                   ok = api_m_getnameinfo(Info)
           end,
    Post = fun(_) -> ok end,
    tc_try(?FUNCTION_NAME,
           Pre, Case, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_m_getnameinfo(#{name      := Name,
                    full_name := FName,
                    family    := Domain,
                    ip        := IP}) ->
    i("Check name info for ~p with"
      "~n   Domain: ~p"
      "~n   IP:     ~p", [Name, Domain, IP]),
    SA = #{family => Domain,
           addr   => IP},
    try net:getnameinfo(SA) of
        {ok, NameInfo} ->
            %% Check that we can actually find this IP in the list
            api_m_getnameinfo_verify(NameInfo, Name, FName, IP);
        {error, enotsup = ReasonAI} ->
            i("getaddrinfo not supported - skipping"),
            ?SKIP({getnameinfo, ReasonAI});
        {error, Reason} ->
            ?FAIL({getnameinfo, Name, Reason})
    catch
        error : notsup = Reason ->
            i("~w => skipping", [Reason]),
            skip(Reason)
    end.


api_m_getnameinfo_verify(#{host := Name} = NameInfo, Name, _FName, _IP) ->
    i("Found (name) match for ~p: "
      "~n   NameInfo: ~p", [Name, NameInfo]),
    ok;
api_m_getnameinfo_verify(#{host := FName} = NameInfo, _Name, FName, _IP) ->
    i("Found (full name) match for ~p: "
      "~n   NameInfo: ~p", [FName, NameInfo]),
    ok;
api_m_getnameinfo_verify(#{host := IPStr} = NameInfo, Name, FName, IP)
  when (size(IP) =:= 8) ->
    %% On some hosts we get back the IPv6 address as a string.
    %% Exampole: 
    %%     {65216,0,0,0,2560,8447,65202,46249} -> "fec0::a00:20ff:feb2:b4a9"
    %% This is possibly because of bad config of the host.
    case inet_parse:ipv6_address(IPStr) of
        {ok, IP} ->
            i("Found (IP string) \"match\" for ~p: "
              "~n   NameInfo: ~p", [IP, NameInfo]),
            ok;
        _ ->
            ?FAIL({not_found, NameInfo, Name, FName})
    end;
api_m_getnameinfo_verify(NameInfo, Name, FName, _IP) ->
    i("No match found for ~p (~p): "
      "~n   NameInfo: ~p", [Name, FName, NameInfo]),
    ?FAIL({not_found, NameInfo, Name, FName}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This gets the local address (not 127.0...)
%% We should really implement this using the (new) net module,
%% but until that gets the necessary functionality...
which_local_addr(Domain) ->
    case ?LIB:which_local_addr(Domain) of
        {ok, Addr} ->
            Addr;
        {error, _} = ERROR ->
            skip(ERROR)
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% not_yet_implemented() ->
%%     skip("not yet implemented").

skip(Reason) ->
    throw({skip, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp({_N1, _N2, _N3} = TS) ->
    {_Date, Time}   = calendar:now_to_local_time(TS),
    %% {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    %% FormatTS = 
    %%     io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w.~w",
    %%                   [YYYY, MM, DD, Hour, Min, Sec, N3]),  
    FormatTS = io_lib:format("~.2.0w:~.2.0w:~.2.0w", [Hour, Min, Sec]),  
    lists:flatten(FormatTS).

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tc_try(Case, TC) ->
    ?TC_TRY(Case, TC).

tc_try(Case, Pre, TC, Post) ->
    ?TC_TRY(Case, Pre, TC, Post).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Required configuration
required(v4) ->
    [{require, test_host_ipv4_only}];
required(v6) ->
    [{require, test_host_ipv6_only}].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% l2a(S) when is_list(S) ->
%%     list_to_atom(S).

%% l2b(L) when is_list(L) ->
%%     list_to_binary(L).

%% b2l(B) when is_binary(B) ->
%%     binary_to_list(B).

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).


i(F) ->
    i(F, []).

i(F, A) ->
    FStr = f("[~s] " ++ F, [formated_timestamp()|A]),
    io:format(user, FStr ++ "~n", []),
    io:format(FStr, []).

