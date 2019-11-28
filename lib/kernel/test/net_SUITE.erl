%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
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
         
         api_b_name_and_index/1

         %% Tickets
        ]).


%% -include("socket_test_evaluator.hrl").

%% Internal exports
%% -export([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SLEEP(T), receive after T -> ok end).

-define(FAIL(R), exit(R)).

-define(MINS(M), timer:minutes(M)).
-define(SECS(S), timer:seconds(S)).

-define(TT(T),   ct:timetrap(T)).


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
     {api_basic, [], api_basic_cases()}

     %% {tickets, [], ticket_cases()}
    ].
     
api_cases() ->
    [
     {group, api_basic}
    ].

api_basic_cases() ->
    [
     api_b_gethostname,
     api_b_getifaddrs,
     api_b_name_and_addr_info,
     api_b_name_and_index
    ].

%% ticket_cases() ->
%%     [].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(Config) ->
    %% We test on the socket module for simplicity
    case lists:member(socket, erlang:loaded()) of
        true ->
            case os:type() of
                {win32, _} ->
                    not_yet_implemented();
                _ ->
                    %% ?LOGGER:start(),
                    Config
            end;
        false ->
            {skip, "esock disabled"}
    end.

end_per_suite(_) ->
    %% ?LOGGER:stop(),
    ok.

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
    case net:getifaddrs() of
        {ok, IfAddrs} ->
            i("IfAddrs: "
              "~n   ~p", [IfAddrs]),
            ok;
        {error, Reason} ->
            ?FAIL(Reason)
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
    Hostname =
        case net:getnameinfo(SA) of
            {ok, #{host := Name, service := Service} = NameInfo} 
              when is_list(Name) andalso is_list(Service) ->
                i("getnameinfo: "
                  "~n   ~p", [NameInfo]),
                Name;
            {ok, BadNameInfo} ->
                ?FAIL({getnameinfo, SA, BadNameInfo});
            {error, Reason1} ->
                ?FAIL({getnameinfo, SA, Reason1})
        end,
    case net:getaddrinfo(Hostname) of
        {ok, AddrInfos} when is_list(AddrInfos) ->
            i("getaddrinfo: "
              "~n   ~p", [AddrInfos]),
            verify_addr_info(AddrInfos, Domain);
        {ok, BadAddrInfo} ->
            ?FAIL({getaddrinfo, Hostname, BadAddrInfo});
        {error, Reason2} ->
            ?FAIL({getaddrinfo, Hostname, Reason2})
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
    Names =
        case net:if_names() of
            {ok, N} when is_list(N) andalso (N =/= []) ->
                N;
            {error, Reason} ->
                ?FAIL({if_names, Reason})
        end,
    verify_if_names(Names).

verify_if_names([]) ->
    ok;
verify_if_names([{Index, Name}|T]) ->
    case net:if_name2index(Name) of
        {ok, Index} ->
            ok;
        {ok, BadIndex} ->
            ?FAIL({name2index, Name, Index, BadIndex});
        {error, ReasonN2I} ->
            ?FAIL({name2index, Name, ReasonN2I})
    end,
    case net:if_index2name(Index) of
        {ok, Name} ->
            ok;
        {ok, BadName} ->
            ?FAIL({index2name, Index, Name, BadName});
        {error, ReasonI2N} ->
            ?FAIL({index2name, Index, ReasonI2N})
    end,
    verify_if_names(T).



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
which_local_addr(Domain) ->
    case inet:getifaddrs() of
        {ok, IFL} ->
            which_addr(Domain, IFL);
        {error, Reason} ->
            ?FAIL({inet, getifaddrs, Reason})
    end.

which_addr(_Domain, []) ->
    skip(no_address);
which_addr(Domain, [{"lo" ++ _, _}|IFL]) ->
    which_addr(Domain, IFL);
which_addr(Domain, [{_Name, IFO}|IFL]) ->
    case which_addr2(Domain, IFO) of
        {ok, Addr} ->
            Addr;
        {error, no_address} ->
            which_addr(Domain, IFL)
    end;
which_addr(Domain, [_|IFL]) ->
    which_addr(Domain, IFL).

which_addr2(_Domain, []) ->
    {error, no_address};
which_addr2(inet = _Domain, [{addr, Addr}|_IFO]) when (size(Addr) =:= 4) ->
    {ok, Addr};
which_addr2(inet6 = _Domain, [{addr, Addr}|_IFO]) when (size(Addr) =:= 8) ->
    {ok, Addr};
which_addr2(Domain, [_|IFO]) ->
    which_addr2(Domain, IFO).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_yet_implemented() ->
    skip("not yet implemented").

skip(Reason) ->
    throw({skip, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% t() ->
%%     os:timestamp().


%% tdiff({A1, B1, C1} = _T1x, {A2, B2, C2} = _T2x) ->
%%     T1 = A1*1000000000+B1*1000+(C1 div 1000), 
%%     T2 = A2*1000000000+B2*1000+(C2 div 1000), 
%%     T2 - T1.


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

set_tc_name(N) when is_atom(N) ->
    set_tc_name(atom_to_list(N));
set_tc_name(N) when is_list(N) ->
    put(tc_name, N).

%% get_tc_name() ->
%%     get(tc_name).

tc_begin(TC) ->
    set_tc_name(TC),
    tc_print("begin ***",
             "~n----------------------------------------------------~n", "").
    
tc_end(Result) when is_list(Result) ->
    tc_print("done: ~s", [Result], 
             "", "----------------------------------------------------~n~n"),
    ok.


tc_try(Case, Fun) when is_atom(Case) andalso is_function(Fun, 0) ->
    tc_begin(Case),
    try 
        begin
            Fun(),
            ?SLEEP(?SECS(1)),
            tc_end("ok")
        end
    catch
        throw:{skip, _} = SKIP ->
            tc_end("skipping"),
            SKIP;
        Class:Error:Stack ->
            tc_end("failed"),
            erlang:raise(Class, Error, Stack)
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
    
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% l2a(S) when is_list(S) ->
%%     list_to_atom(S).

%% l2b(L) when is_list(L) ->
%%     list_to_binary(L).

%% b2l(B) when is_binary(B) ->
%%     binary_to_list(B).

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).


%% i(F) ->
%%     i(F, []).

i(F, A) ->
    FStr = f("[~s] " ++ F, [formated_timestamp()|A]),
    io:format(user, FStr ++ "~n", []),
    io:format(FStr, []).

