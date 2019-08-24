%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2019. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Basic module for reading and verifying config files
%%----------------------------------------------------------------------
-module(snmp_conf).


%% External exports
%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([read_files/2, no_gen/2, no_order/2, no_filter/1, keyorder/4]).
-export([read/2, read/3]).

%% Basic (type) check functions
-export([check_mandatory/2,
	 check_integer/1, check_integer/2,

	 check_string/1, check_string/2,

	 check_atom/2,

	 check_timer/1,

	 all_domains/0,
	 check_domain/1,
	 domain_to_name/1,
	 all_tdomains/0,
	 check_tdomain/1,
	 mk_tdomain/0, mk_tdomain/1,
	 tdomain_to_family/1, tdomain_to_domain/1,
	 which_domain/1,
	 mk_addr_string/1,
	 check_ip/1, check_ip/2,
	 check_port/1,
%%	 ip_port_to_domaddr/2,
	 check_address/2, check_address/3,
	 check_taddress/2,
	 mk_taddress/1, mk_taddress/2,

	 check_packet_size/1,

	 check_oid/1,
	 check_imask/1, check_emask/1,

	 check_mp_model/1,
	 check_sec_model/1, check_sec_model/2, check_sec_model/3,
	 check_sec_level/1,

	 all_integer/1
	]).


-define(SNMP_USE_V3, true).
-include_lib("snmp/include/snmp_types.hrl").
-include_lib("snmp/include/SNMP-FRAMEWORK-MIB.hrl").
-include_lib("snmp/include/TRANSPORT-ADDRESS-MIB.hrl").
-include_lib("snmp/include/SNMPv2-TM.hrl").

-define(VMODULE,"CONF").
-include("snmp_verbosity.hrl").


-define(is_word(P), (((P) band (bnot 65535)) =:= 0)).
-define(is_word(P0, P1), ((((P0) bor (P1)) band (bnot 255)) =:= 0)).

mk_word(B0, B1) -> ((B0) bsl 8) bor (B1).
mk_bytes(W) -> [(W) bsr 8,(W) band 255].

-define(
   is_ipv4_addr(A0, A1, A2, A3),
   ((((A0) bor (A1) bor (A2) bor (A3)) band (bnot 255)) =:= 0)).

-define(
   is_ipv6_addr(A0, A1, A2, A3, A4, A5, A6, A7),
   ((((A0) bor (A1) bor (A2) bor (A3) bor (A4) bor (A5) bor (A6) bor (A7))
	 band (bnot 65535)) =:= 0)).
-define(
   is_ipv6_addr(
     A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15),
   ((((A0) bor (A1) bor (A2) bor (A3) bor
	  (A4) bor (A5) bor (A6) bor (A7) bor
	  (A8) bor (A9) bor (A10) bor (A11) bor
	  (A12) bor (A13) bor (A14) bor (A15))
	 band (bnot 65535)) =:= 0)).


%%-----------------------------------------------------------------

%% read_files(Dir, Files) -> Configs
%% Dir      - string()   - Full path to the config dir.
%% Files    - [{FileName, Gen, Order, Check, Filter}]
%% FileName - string()   - Name of the config file.
%% Gen      - function/2 - In case of failure when reading the config file, 
%%                         this function is called to either generate a
%%                         default file or issue the error.
%%                         Returns a generated config list corresponding
%%                         to the written file.
%%                         (Dir, Error) -> Configs.
%% Order    - function/2 - An ordering function that is used to process
%%                         the read config entries using lists:sort/2.
%%                         Returns true if arg 1 compares less than or
%%                         equal to arg 2, false otherwise.
%% Check    - function/2 - Check each entry as they are read from the file.
%%                         (Entry, State) ->
%%                             {ok,NewState} | {{ok,NewEntry},NewState} |
%%                             throw(Error)
%%                         State =:= 'undefined' the first time.
%% Filter   - function/1 - Process all the config entries read from the file
%%                         (Configs) -> [config_entry()].
%% Configs  - [config_entry()]
%% config_entry() - term()

read_files(Dir, Files) when is_list(Dir) andalso is_list(Files) ->
    read_files(Dir, Files, []).

read_files(_Dir, [], Res) ->
    lists:reverse(Res);
read_files(Dir, [{FileName, Gen, Order, Check, Filter}|Files], Res)
  when is_list(FileName),
       is_function(Gen),
       is_function(Order),
       is_function(Check),
       is_function(Filter) ->
    ?vdebug("read_files -> entry with~n"
	    "   FileName: ~p", [FileName]),
    File = filename:join(Dir, FileName),
    Confs =
	case file:read_file_info(File) of
	    {ok,_} ->
		read(File, Order, Check);
	    {error, R} ->
		?vlog("failed reading file info for ~s: ~n"
		      "   ~p", [FileName, R]),
		Gen(Dir, R)
	end,
    read_files(Dir, Files, [Filter(Confs)|Res]).



no_gen(_Dir, _R) -> [].
no_order(_, _) -> true.
no_filter(X) -> X.

%% Order tuples on element N with Keys first in appearence order.
%%
%% An ordering function (A, B) shall return true iff
%% A is less than or equal to B i.e shall return
%% false iff A is to be ordered after B.

-spec keyorder(N, A, B, Keys) ->
                      boolean() when
      N    :: integer(),
      A    :: tuple(),
      B    :: tuple(),
      Keys :: maybe_improper_list().

keyorder(N, A, B, _) when element(N, A) == element(N, B) ->
    true;
keyorder(N, A, B, [Key | _])
  when tuple_size(A) >= 1, element(N, B) == Key ->
    false;
keyorder(N, A, B, [Key | _])
  when element(N, A) == Key, tuple_size(B) >= 1 ->
    true;
keyorder(N, A, B, [_ | Keys]) ->
    keyorder(N, A, B, Keys);
keyorder(_, A, B, []) when tuple_size(A) >= 1, tuple_size(B) >= 1 ->
    %% Do not order other keys
    true;
keyorder(N, A, B, sort) ->
    %% Order other keys according to standard sort order
    element(N, A) =< element(N, B).


read(File, Verify) ->
    Check = fun (Row, State) -> {Verify(Row), State} end,
    read(File, fun no_order/2, Check).

%% Ret. Res | exit(Reason)
read(File, Order, Check) when is_function(Order), is_function(Check) ->
    ?vdebug("read -> entry with~n"
	"   File: ~p", [File]),
    Fd = open_file(File),
    read_fd(File, Order, Check, Fd, 1, []).

read_fd(File, Order, Check, Fd, StartLine, Res) ->
    case do_read(Fd, "", StartLine) of
	{ok, Row, EndLine} ->
	    ?vtrace("read_fd ->~n"
		    "   Row:     ~p~n"
		    "   EndLine: ~p", [Row,EndLine]),
	    read_fd(
	      File, Order, Check, Fd, EndLine,
	      [{StartLine, Row, EndLine}|Res]);
	{error, Error, EndLine} ->
	    ?vtrace("read_fd -> read failure:~n"
		    "   Error: ~p~n"
		    "   EndLine: ~p", [Error,EndLine]),
	    file:close(Fd),
	    error({failed_reading, File, StartLine, EndLine, Error});
	{eof, _EndLine} ->
	    Lines =
		lists:sort(
		  fun ({_, RowA, _}, {_, RowB, _}) ->
			  Order(RowA, RowB)
		  end,
		  lists:reverse(Res)),
	    ?vtrace("read_fd to read_check ->~n"
		    "   Lines: ~p", [Lines]),
	    read_check(File, Check, Lines, undefined, [])
    end.

read_check(_, _, [], _, Res) ->
    lists:reverse(Res);
read_check(File, Check, [{StartLine, Row, EndLine}|Lines], State, Res) ->
    try Check(Row, State) of
	{Rows, NewState} when is_list(Rows) ->
	    ?vtrace("read_check -> ok:~n"
		    "   Rows: ~p~n", [Rows]),
	    read_check(File, Check, Lines, NewState, Rows ++ Res);
	{ok, NewState} ->
	    ?vtrace("read_check -> ok", []),
	    read_check(File, Check, Lines, NewState, [Row | Res]);
	{{ok, NewRow}, NewState} ->
	    ?vtrace("read_check -> ok:~n"
		    "   NewRow: ~p~n", [NewRow]),
	    read_check(File, Check, Lines, NewState, [NewRow | Res])
    catch
	throw:{error, Reason} ->
	    ?vtrace("read_check -> error:"
		    "~n   Reason: ~p", [Reason]),
	    error({failed_check, File, StartLine, EndLine, Reason});
	C:E:S ->
	    ?vtrace("read_check -> failure:"
                    "~n   Class: ~p"
		    "~n   Error: ~p"
		    "~n   Stack: ~p", [C, E, S]),
	    error({failed_check, File, StartLine, EndLine, {C, E, S}})
    end.

open_file(File) ->
    case file:open(File, [read]) of
	{ok, Fd} ->
	    Fd;
	{error, Reason} ->
	    error({failed_open, File, Reason})
    end.

do_read(Io, Prompt, StartLine) ->
    case io:request(Io, {get_until,Prompt,erl_scan,tokens,[StartLine]}) of
	{ok, Toks, EndLine} ->
	    case erl_parse:parse_term(Toks) of
		{ok, Term} ->
		    {ok, Term, EndLine};
		{error, {Line, erl_parse, Error}} ->
		    {error, {parse_error, Error}, Line}
	    end;
	Other ->
	    Other
    end.



%%-----------------------------------------------------------------


check_mandatory(L, [{Key, Value}|T]) ->
    case lists:keymember(Key, 1, L) of
	true -> 
	    check_mandatory(L, T);
	false when Value == mandatory -> 
	    error({missing_mandatory, Key});
	false ->
	    {value, V} = Value,
	    check_mandatory([{Key, V} | L], T)
    end;
check_mandatory(L, []) -> 
    {ok, L}.


%% ---------

check_integer(I) -> check_integer(I, any).

check_integer(I, any) when is_integer(I) -> ok;
check_integer(I, pos) when is_integer(I), I > 0 -> ok;
check_integer(I, neg) when is_integer(I), I < 0 -> ok;
check_integer(I1, {gt,  I2}) 
  when is_integer(I1) andalso is_integer(I2) andalso (I1 > I2) -> ok;
check_integer(I1, {gte, I2}) 
  when is_integer(I1) andalso is_integer(I2) andalso (I1 >= I2) -> ok;
check_integer(I1, {lt,  I2}) 
  when is_integer(I1) andalso is_integer(I2) andalso (I1 < I2) -> ok;
check_integer(I1, {lte, I2}) 
  when is_integer(I1) andalso is_integer(I2) andalso (I1 =< I2) -> ok;
check_integer(I1, {eq,  I1}) 
  when is_integer(I1) -> ok;
check_integer(I,  {range, L, U}) 
  when (is_integer(I) andalso 
	is_integer(L) andalso 
	is_integer(U) andalso 
	(I >= L) andalso (I =< U)) -> ok;
check_integer(I, _)  -> error({invalid_integer, I}).

check_packet_size(S) ->
    case (catch check_integer(S, {range, 484, 2147483647})) of
	ok ->
	    ok;
	{error, _} ->
	    error({invalid_packet_size, S})
    end.

%% ---------

check_string(X) when is_list(X) -> ok;
check_string(X) -> error({invalid_string, X}).

check_string(X, any) 
  when is_list(X) -> ok;
check_string(X, {gt, Len})   
  when is_list(X) andalso (length(X) > Len) -> ok;
check_string(X, {gt, _Len})  
  when is_list(X) -> error({invalid_length, X});
check_string(X, {gte, Len})  
  when is_list(X) andalso (length(X) >= Len) -> ok;
check_string(X, {gte, _Len}) 
  when is_list(X) -> error({invalid_length, X});
check_string(X, {lt, Len})   
  when is_list(X) andalso (length(X) < Len) -> ok;
check_string(X, {lt, _Len})  
  when is_list(X) -> error({invalid_length, X});
check_string(X, {lte, Len})  
  when is_list(X) andalso (length(X) =< Len) -> ok;
check_string(X, {lte, _Len}) 
  when is_list(X) -> error({invalid_length, X});
check_string(X, Len)       
  when is_list(X) andalso is_integer(Len) andalso (length(X) =:= Len) -> ok;
check_string(X, _Len)      when is_list(X) -> error({invalid_length, X});
check_string(X, _Len)                   -> error({invalid_string, X}).


check_atom(X, Atoms) ->
    case lists:keysearch(X, 1, Atoms) of
        {value, {X, Val}} -> 
	    {ok, Val};
        _ -> 
	    error({invalid_atom, X, Atoms})
    end.


%% ---------

check_mp_model(MPModel) when is_atom(MPModel) ->
    All = [{v1,  ?MP_V1}, {v2c, ?MP_V2C}, {v3,  ?MP_V3}],
    check_atom(MPModel, All);
check_mp_model(?MP_V1) ->
    {ok, ?MP_V1};
check_mp_model(?MP_V2C) ->
    {ok, ?MP_V2C};
check_mp_model(?MP_V3) ->
    {ok, ?MP_V3};
check_mp_model(BadMpModel) ->
    error({invalid_mp_model, BadMpModel}).


%% ---------

check_sec_model(SecModel) when is_atom(SecModel) ->
    check_sec_model(SecModel, []);
check_sec_model(?SEC_ANY) ->
    {ok, ?SEC_ANY};
check_sec_model(?SEC_V1) ->
    {ok, ?SEC_V1};
check_sec_model(?SEC_V2C) ->
    {ok, ?SEC_V2C};
check_sec_model(?SEC_USM) ->
    {ok, ?SEC_USM};
check_sec_model(BadSecModel) ->
    error({invalid_sec_model, BadSecModel}).

check_sec_model(SecModel, Exclude) when is_atom(SecModel) ->
    All = [{any, ?SEC_ANY},
           {v1,  ?SEC_V1},
           {v2c, ?SEC_V2C},
           {usm, ?SEC_USM}],
    Alt = [{X, Y} || {X, Y} <- All, not lists:member(X, Exclude)],
    case (catch check_atom(SecModel, Alt) ) of
	{error, _} ->
	    error({invalid_sec_model, SecModel});
	OK ->
	    OK
    end;
check_sec_model(BadSecModel, _Exclude) ->
    error({invalid_sec_model, BadSecModel}).

check_sec_model(v1, v1, Exclude) ->
    check_sec_model2(v1, ?SEC_V1, Exclude);
check_sec_model(v1, SecModel, _Exclude) ->
    error({invalid_sec_model, v1, SecModel});
check_sec_model(v2c, v2c, Exclude) ->
    check_sec_model2(v2c, ?SEC_V2C, Exclude);
check_sec_model(v2c, SecModel, _Exclude) ->
    error({invalid_sec_model, v2c, SecModel});
check_sec_model(v3, usm, Exclude) ->
    check_sec_model2(v3, ?SEC_USM, Exclude);
check_sec_model(v3, SecModel, _Exclude) ->
    error({invalid_sec_model, v3, SecModel});
check_sec_model(M1, M2, _Exclude) ->
    error({invalid_sec_model, M1, M2}).

check_sec_model2(SecModel, SM, Exclude) ->
    case lists:member(SecModel, Exclude) of
	false ->
	    {ok, SM};
	true ->
	    error({invalid_sec_model, SecModel})
    end.


%% ---------

check_sec_level(SecLevel) when is_atom(SecLevel) ->
    All = [{noAuthNoPriv, ?'SnmpSecurityLevel_noAuthNoPriv'}, 
	   {authNoPriv,   ?'SnmpSecurityLevel_authNoPriv'}, 
	   {authPriv,     ?'SnmpSecurityLevel_authPriv'}], 
    case (catch check_atom(SecLevel, All)) of
	{error, _} ->
	    error({invalid_sec_level, SecLevel});
	OK ->
	    OK
    end;
check_sec_level(?'SnmpSecurityLevel_noAuthNoPriv' = SL) ->
    {ok, SL};
check_sec_level(?'SnmpSecurityLevel_authNoPriv' = SL) ->
    {ok, SL};
check_sec_level(?'SnmpSecurityLevel_authPriv' = SL) ->
    {ok, SL};
check_sec_level(BadSecLevel) ->
    error({invalid_sec_level, BadSecLevel}).


%% ---------
all_tdomains() ->
    [
     ?transportDomainUdpIpv4, 
     ?transportDomainUdpIpv6, 
     ?transportDomainUdpIpv4z, 
     ?transportDomainUdpIpv6z, 
     ?transportDomainTcpIpv4, 
     ?transportDomainTcpIpv6, 
     ?transportDomainTcpIpv4z, 
     ?transportDomainTcpIpv6z, 
     ?transportDomainSctpIpv4, 
     ?transportDomainSctpIpv6, 
     ?transportDomainSctpIpv4z, 
     ?transportDomainSctpIpv6z, 
     ?transportDomainLocal, 
     ?transportDomainUdpDns, 
     ?transportDomainTcpDns,
     ?transportDomainSctpDns
    ].

check_tdomain(TDomain) ->
    SupportedTDomains = 
	[
	 ?snmpUDPDomain, % Legacy
	 ?transportDomainUdpIpv4,
	 ?transportDomainUdpIpv6			 
	],
    AllTDomains = all_tdomains(), 
    case lists:member(TDomain, SupportedTDomains) of
	true ->
	    ok;
	false ->
	    case lists:member(TDomain, AllTDomains) of
		true ->
		    error({unsupported_tdomain, TDomain});
		false ->
		    error({unknown_tdomain, TDomain})
	    end
    end.


%% ---------

mk_tdomain() ->
    mk_tdomain(snmpUDPDomain).

mk_tdomain(snmpUDPDomain) ->
    mk_tdomain(transportDomainUdpIpv4);
mk_tdomain(transportDomainUdpIpv4) ->
    ?transportDomainUdpIpv4;
mk_tdomain(transportDomainUdpIpv6) ->
    ?transportDomainUdpIpv6;
mk_tdomain(BadDomain) ->
    error({bad_domain, BadDomain}).


%% ---------

tdomain_to_family(snmpUDPDomain) ->
    inet;
tdomain_to_family(transportDomainUdpIpv4) ->
    inet;
tdomain_to_family(transportDomainUdpIpv6) ->
    inet6;
tdomain_to_family(?snmpUDPDomain) ->
    inet;
tdomain_to_family(?transportDomainUdpIpv4) ->
    inet;
tdomain_to_family(?transportDomainUdpIpv6) ->
    inet6;
tdomain_to_family(BadDomain) ->
    error({bad_domain, BadDomain}).


%% ---------

tdomain_to_domain(?snmpUDPDomain) ->
    snmpUDPDomain;
tdomain_to_domain(?transportDomainUdpIpv4) ->
    transportDomainUdpIpv4;
tdomain_to_domain(?transportDomainUdpIpv6) ->
    transportDomainUdpIpv6;
tdomain_to_domain(BadTDomain) ->
    error({bad_tdomain, BadTDomain}).


%% ---------

check_taddress(?snmpUDPDomain, X) ->
    check_taddress(transportDomainUdpIpv4, X);
check_taddress(snmpUDPDomain, X) ->
    check_taddress(transportDomainUdpIpv4, X);
%%
check_taddress(?transportDomainUdpIpv4, X) ->
    check_taddress(transportDomainUdpIpv4, X);
check_taddress(transportDomainUdpIpv4, X) ->
    case X of
	[A0,A1,A2,A3,P0,P1]
	  when ?is_ipv4_addr(A0, A1, A2, A3), ?is_word(P0, P1) ->
	    ok;
	_ ->
	    error({invalid_taddress, X})
    end;
%%
check_taddress(?transportDomainUdpIpv6, X) ->
    check_taddress(transportDomainUdpIpv6, X);
check_taddress(transportDomainUdpIpv6, X) ->
    case X of
	[A0,A1,A2,A3,A4,A5,A6,A7,P0,P1]
	when ?is_ipv6_addr(A0, A1, A2, A3, A4, A5, A6, A7),
	     ?is_word(P0, P1) ->
	    ok;
	_ ->
	    error({invalid_taddress, X})
    end;
%%
check_taddress(BadDomain, _X) ->
    error({invalid_tdomain, BadDomain}).



%% ---------

check_timer(infinity) ->
    {ok, infinity};
check_timer(T) when is_record(T, snmp_incr_timer) ->
    {ok, T};
check_timer({WaitFor, Factor, Incr, Retry} = T) ->
    case (catch do_check_timer(WaitFor, Factor, Incr, Retry)) of
	ok ->
	    {ok, #snmp_incr_timer{wait_for    = WaitFor,
				  factor      = Factor,
				  incr        = Incr,
				  max_retries = Retry}};
	_Err ->
	    error({invalid_timer, T})
    end;
check_timer(Timeout) ->
    case (catch check_integer(Timeout, {gt, 0})) of
	ok ->
	    {ok, #snmp_incr_timer{wait_for    = Timeout,
				  factor      = 1,
				  incr        = 0,
				  max_retries = 0}};
	_Err ->
	    error({invalid_timer, Timeout})
    end.

do_check_timer(WaitFor, Factor, Incr, Retry) ->
    check_integer(WaitFor, {gt,  0}),
    check_integer(Factor,  {gt,  0}),
    check_integer(Incr,    {gte, 0}),
    check_integer(Retry,   {gte, 0}),
    ok.


%% ---------

all_domains() ->
    [
     transportDomainUdpIpv4, 
     transportDomainUdpIpv6, 
     transportDomainUdpIpv4z, 
     transportDomainUdpIpv6z, 
     transportDomainTcpIpv4, 
     transportDomainTcpIpv6, 
     transportDomainTcpIpv4z, 
     transportDomainTcpIpv6z, 
     transportDomainSctpIpv4, 
     transportDomainSctpIpv6, 
     transportDomainSctpIpv4z, 
     transportDomainSctpIpv6z, 
     transportDomainLocal, 
     transportDomainUdpDns, 
     transportDomainTcpDns,
     transportDomainSctpDns
    ].

check_domain(snmpUDPDomain) -> ok;
check_domain(transportDomainUdpIpv4) -> ok;
check_domain(transportDomainUdpIpv6) -> ok;
check_domain(Domain) ->
    case lists:member(Domain, all_domains()) of
	true ->
	    error({unsupported_domain, Domain});
	false ->
	    error({unknown_domain, Domain})
    end.

domain_to_name(snmpUDPDomain) ->
    undefined;
domain_to_name(transportDomainUdpIpv4) ->
    udpIpv4;
domain_to_name(transportDomainUdpIpv6) ->
    udpIpv6;
domain_to_name(transportDomainUdpIpv4z) ->
    udpIpv4z;
domain_to_name(transportDomainUdpIpv6z) ->
    udpIpv6z;
domain_to_name(transportDomainTcpIpv4) ->
    tcpIpv4;
domain_to_name(transportDomainTcpIpv6) ->
    tcpIpv6;
domain_to_name(transportDomainTcpIpv4z) ->
    tcpIpv4z;
domain_to_name(transportDomainTcpIpv6z) ->
    tcpIpv6z;
domain_to_name(transportDomainSctpIpv4) ->
    sctpIpv4;
domain_to_name(transportDomainSctpIpv6) ->
    sctpIpv6;
domain_to_name(transportDomainSctpIpv4z) ->
    sctpIpv4z;
domain_to_name(transportDomainSctpIpv6z) ->
    sctpIpv6z;
domain_to_name(transportDomainLocal) ->
    local;
domain_to_name(transportDomainUdpDns) ->
    udpDns;
domain_to_name(transportDomainTcpDns) ->
    tcpDns;
domain_to_name(transportDomainSctpDns) ->
    sctpDns;
domain_to_name(BadDomain) ->
    error({bad_domain, BadDomain}).

%% ---------

mk_taddress(Address) ->
    mk_taddress(snmpUDPDomain, Address).

%% The values of Domain, Ip and Port has both been checked at this
%% point, so we dont need to do that again, but this function is
%% also used on incoming packets from net_if so a little
%% check that net_if does not supply bad arguments is in order.
%%
%% These are just for convenience
mk_taddress(?snmpUDPDomain, Address) ->
    mk_taddress(snmpUDPDomain, Address);
mk_taddress(?transportDomainUdpIpv4, Address) ->
    mk_taddress(transportDomainUdpIpv4, Address);
mk_taddress(?transportDomainUdpIpv6, Address) ->
    mk_taddress(transportDomainUdpIpv6, Address);
%%
mk_taddress(snmpUDPDomain, Address) -> % Legacy
    mk_taddress(transportDomainUdpIpv4, Address);
mk_taddress(transportDomainUdpIpv4 = Domain, Address) ->
    case Address of
	[] -> % Empty mask
	    [];
	{Ip, Port} when tuple_size(Ip) =:= 4, is_integer(Port) ->
	    tuple_to_list(Ip) ++ mk_bytes(Port);
	_ ->
	    erlang:error(badarg, [Domain,Address])
    end;
mk_taddress(transportDomainUdpIpv6 = Domain, Address) ->
    case Address of
	[] -> % Empty mask
	    [];
	{{A, B, C, D, E, F, G, H}, Port} ->
	    [A bsr 8, A band 255,
	     B bsr 8, B band 255,
	     C bsr 8, C band 255,
	     D bsr 8, D band 255,
	     E bsr 8, E band 255,
	     F bsr 8, F band 255,
	     G bsr 8, G band 255,
	     H bsr 8, H band 255,
	     Port bsr 8, Port band 255];
	_ ->
	    erlang:error(badarg, [Domain,Address])
    end;
%% Bad domain
mk_taddress(BadDomain, _) ->
    error({bad_domain, BadDomain}).


%% ---------

%% XXX remove, when net_if handles one socket per transport domain

which_domain([A0,A1,A2,A3])
  when ?is_ipv4_addr(A0, A1, A2, A3) ->
    transportDomainUdpIpv4;
which_domain({A0, A1, A2, A3})
  when ?is_ipv4_addr(A0, A1, A2, A3) ->
    transportDomainUdpIpv4;
which_domain([A0,A1,A2,A3,A4,A5,A6,A7])
  when ?is_ipv6_addr(A0, A1, A2, A3, A4, A5, A6, A7) ->
    transportDomainUdpIpv6;
which_domain([A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15])
  when ?is_ipv6_addr(
	  A0, A1, A2, A3, A4, A5, A6, A7,
	  A8, A9, A10, A11, A12, A13, A14, A15) ->
    transportDomainUdpIpv6;
which_domain({A0, A1, A2, A3, A4, A5, A6, A7})
  when ?is_ipv6_addr(A0, A1, A2, A3, A4, A5, A6, A7) ->
    transportDomainUdpIpv6.
    
%% ---------

mk_addr_string({Domain, Addr}) when is_atom(Domain) ->
    %% XXX There is only code for IP domains here
    case check_address_ip(Domain, Addr) of
	false ->
	    case check_address_ip_port(Domain, Addr) of
		false ->
		    error({bad_address, {Domain, Addr}});
		true ->
		    {IP, Port} = Addr,
		    mk_addr_string_ntoa(Domain, IP, Port);
		{IP, Port} ->
		    mk_addr_string_ntoa(Domain, IP, Port)
	    end;
	true ->
	    mk_addr_string_ntoa(Domain, Addr);
	IP ->
	    mk_addr_string_ntoa(Domain, IP)
    end;
mk_addr_string({_IP, Port} = Addr) when is_integer(Port) ->
    mk_addr_string({snmpUDPDomain, Addr});
mk_addr_string(Strange) ->
    lists:flatten(io_lib:format("~w", [Strange])).


mk_addr_string_ntoa({_, _, _, _} = IP) ->
    inet:ntoa(IP);
mk_addr_string_ntoa(IP) ->
    lists:flatten(io_lib:format("[~s]", [inet:ntoa(IP)])).

mk_addr_string_ntoa(Domain, IP) ->
    case domain_to_name(Domain) of
	undefined ->
	    mk_addr_string_ntoa(IP);
	Name ->
	    lists:flatten(
	      io_lib:format("~w://~s", [Name, mk_addr_string_ntoa(IP)]))
    end.

mk_addr_string_ntoa(Domain, IP, Port) ->
    lists:flatten(
      io_lib:format(
	"~s:~w", [mk_addr_string_ntoa(Domain, IP), Port])).

%% ---------

check_ip(X) ->
    check_ip(snmpUDPDomain, X).

check_ip(Domain, IP) ->
    %% XXX There is only code for IP domains here
    case check_address_ip(Domain, IP) of
	false ->
	    error({bad_address, {Domain, IP}});
	true ->
	    ok;
	FixedIP ->
	    {ok, FixedIP}
    end.


%% ---------

check_port(Port) when ?is_word(Port) ->
    ok;
check_port(Port) ->
    error({bad_port, Port}).

%% ip_port_to_domaddr(IP, Port) when ?is_word(Port) ->
%%     %% XXX There is only code for IP domains here
%%     case check_address_ip(transportDomainUdpIpv4, IP) of
%% 	false ->
%% 	    case check_address_ip(transportDomainUdpIpv6, IP) of
%% 		false ->
%% 		    error({bad_address, {transportDomainUdpIpv4, {IP, Port}}});
%% 		true ->
%% 		    {transportDomainUdpIpv6, {IP, Port}};
%% 		FixedIP ->
%% 		    {transportDomainUdpIpv6, {FixedIP, Port}}
%% 	    end;
%% 	true ->
%% 	    {transportDomainUdpIpv4, {IP, Port}};
%% 	FixedIP ->
%% 	    {transportDomainUdpIpv4, {FixedIP, Port}}
%%     end;
%% ip_port_to_domaddr(IP, Port) ->
%%     error({bad_address, {transportDomainUdpIpv4, {IP, Port}}}).

%% Check a configuration term field from a file to see if it
%% can be fixed to be fed to mk_taddress/2.

check_address(Domain, Address, DefaultPort) ->
    %% If Address does not contain Port or contains Port =:= 0
    %% create an address containing DefaultPort
    %%
    %% XXX There is only code for IP domains here
    case check_address_ip(Domain, Address) of
	false ->
	    case check_address_ip_port(Domain, Address) of
		false ->
		    error({bad_address, {Domain, Address}});
		true ->
		    case Address of
			{IP, 0} ->
			    {ok, {IP, DefaultPort}};
			_ ->
			    ok
		    end;
		{FixedIP, 0} ->
		    {ok, {FixedIP, DefaultPort}};
		FixedAddress ->
		    {ok, FixedAddress}
	    end;
	true ->
	    {ok, {Address, DefaultPort}};
	FixedIP ->
	    {ok, {FixedIP, DefaultPort}}
    end.

check_address(Domain, Address) ->
    %% Address has to contain Port
    %%
    %% XXX There is only code for IP domains here
    case check_address_ip_port(Domain, Address) of
	false ->
	    error({bad_address, {Domain, Address}});
	true ->
	    ok;
	FixedAddress ->
	    {ok, FixedAddress}
    end.

%% -> IP
check_address_ip(Domain, Address)
  when Domain =:= snmpUDPDomain;
       Domain =:= transportDomainUdpIpv4 ->
    case Address of
	%% Erlang native format
	{A0, A1, A2, A3}
	  when ?is_ipv4_addr(A0, A1, A2, A3) ->
	    true;
	%% Erlangish format
	[A0,A1,A2,A3]
	  when ?is_ipv4_addr(A0, A1, A2, A3) ->
	    {A0, A1, A2, A3};
	_ ->
	    false
    end;
check_address_ip(transportDomainUdpIpv6, Address) ->
    case Address of
	%% Erlang native format
	{A0, A1, A2, A3, A4, A5, A6, A7}
	  when ?is_ipv6_addr(A0, A1, A2, A3, A4, A5, A6, A7) ->
	    true;
	%% Erlangish format
	[A0,A1,A2,A3,A4,A5,A6,A7]
	  when ?is_ipv6_addr(A0, A1, A2, A3, A4, A5, A6, A7) ->
	    {A0, A1, A2, A3, A4, A5, A6, A7};
	%% SNMP standards format
	[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15]
	  when ?is_ipv6_addr(
		  A0, A1, A2, A3, A4, A5, A6, A7,
		  A8, A9, A10, A11, A12, A13, A14, A15) ->
	    {mk_word(A0, A1), mk_word(A2, A3),
	     mk_word(A4, A5), mk_word(A6, A7),
	     mk_word(A8, A9), mk_word(A10, A11),
	     mk_word(A12, A13), mk_word(A14, A15)};
	_ ->
	    false
    end;
check_address_ip(BadDomain, _) ->
    error({bad_domain, BadDomain}).

%% -> {IP, Port}
check_address_ip_port(Domain, Address)
  when Domain =:= snmpUDPDomain;
       Domain =:= transportDomainUdpIpv4 ->
    case Address of
	{IP, Port} when ?is_word(Port) ->
	    case check_address_ip(Domain, IP) of
		false ->
		    false;
		true ->
		    true;
		FixedIP ->
		    {FixedIP, Port}
	    end;
	%% SNMP standards format
	[A0,A1,A2,A3,P0,P1]
	  when ?is_ipv4_addr(A0, A1, A2, A3), ?is_word(P0, P1) ->
	    {{A0, A1, A2, A3}, mk_word(P0, P1)};
	_ ->
	    false
    end;
check_address_ip_port(transportDomainUdpIpv6 = Domain, Address) ->
    case Address of
	{IP, Port} when ?is_word(Port) ->
	    case check_address_ip(Domain, IP) of
		false ->
		    false;
		true ->
		    true;
		FixedIP ->
		    {FixedIP, Port}
	    end;
	%% Erlang friendly list format
	[A0,A1,A2,A3,A4,A5,A6,A7,P]
	  when ?is_ipv6_addr(A0, A1, A2, A3, A4, A5, A6, A7),
	       ?is_word(P) ->
	    {{A0, A1, A2, A3, A4, A5, A6, A7}, P};
	%% Strange hybrid format with port as bytes
	[A0,A1,A2,A3,A4,A5,A6,A7,P0,P1]
	  when ?is_ipv6_addr(A0, A1, A2, A3, A4, A5, A6, A7),
	       ?is_word(P0, P1) ->
	    {{A0, A1, A2, A3, A4, A5, A6, A7}, mk_word(P0, P1)};
	%% SNMP standards format
	[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,P0,P1]
	  when ?is_ipv6_addr(
		  A0, A1, A2, A3, A4, A5, A6, A7,
		  A8, A9, A10, A11, A12, A13, A14, A15),
	       ?is_word(P0, P1) ->
	    {{mk_word(A0, A1), mk_word(A2, A3),
	      mk_word(A4, A5), mk_word(A6, A7),
	      mk_word(A8, A9), mk_word(A10, A11),
	      mk_word(A12, A13), mk_word(A14, A15)},
	     mk_word(P0, P1)};
	_ ->
	    false
    end;
check_address_ip_port(BadDomain, _) ->
    error({bad_domain, BadDomain}).



%% ---------

check_oid([E1,E2|_] = X) when E1 * 40 + E2 =< 255 ->
    case all_integer(X) of
	true -> 
	    ok;
	_ -> 
	    error({invalid_object_identifier, X})
    end;
check_oid(X) -> 
    error({invalid_object_identifier, X}).


%% ---------

%% Check a (view) mask in the internal form (all 0 and 1): 
check_imask(null) ->
    {ok, []};
check_imask(IMask) when is_list(IMask) ->
    do_check_imask(IMask), 
    {ok, IMask}.

do_check_imask([]) ->
    ok;
do_check_imask([0|IMask]) ->
    do_check_imask(IMask);
do_check_imask([1|IMask]) ->
    do_check_imask(IMask);
do_check_imask([X|_]) ->
    error({invalid_internal_mask_element, X}).


%% Check a (view) mask in the external form (according to MIB, 
%% an OCTET STRING of at most length 16). 
check_emask(EMask) when is_list(EMask) andalso (length(EMask) =< 16) ->
    do_check_emask(EMask).

do_check_emask([]) ->
    ok;
do_check_emask([X|EMask]) 
  when is_integer(X) andalso (X >= 16#00) andalso (X =< 16#FF) ->
    do_check_emask(EMask);
do_check_emask([X|_]) ->
    error({invalid_external_mask_element, X}).


%% ---------

all_integer([H|T]) when is_integer(H) -> all_integer(T);
all_integer([_H|_T]) -> false;
all_integer([]) -> true.


%% ---------

error(Reason) ->
    throw({error, Reason}).

