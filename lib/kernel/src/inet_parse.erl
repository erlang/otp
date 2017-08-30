%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(inet_parse).

%% Parser for all kinds of ineternet configuration files

%% Avoid warning for local function error/2 clashing with autoimported BIF.
-compile({no_auto_import,[error/2]}).
-export([hosts/1, hosts/2]).
-export([protocols/1, protocols/2]).
-export([netmasks/1, netmasks/2]).
-export([networks/1, networks/2]).
-export([services/1, services/2]).
-export([rpc/1, rpc/2]).
-export([resolv/1, resolv/2]).
-export([host_conf_linux/1, host_conf_linux/2]).
-export([host_conf_freebsd/1, host_conf_freebsd/2]).
-export([host_conf_bsdos/1, host_conf_bsdos/2]).
-export([nsswitch_conf/1, nsswitch_conf/2]).

-export([ipv4_address/1, ipv6_address/1]).
-export([ipv4strict_address/1, ipv6strict_address/1]).
-export([address/1, strict_address/1]).
-export([visible_string/1, domain/1]).
-export([ntoa/1, dots/1]).
-export([split_line/1]).

-import(lists, [reverse/1]).

-include_lib("kernel/include/file.hrl").

%% --------------------------------------------------------------------------
%% Parse services internet style
%% Syntax: 
%%      Name   Port/Protocol    [Aliases]  \n
%%      # comment
%% --------------------------------------------------------------------------

services(File) ->
    services(noname, File).

services(Fname, File) ->
    Fn = fun([Name, PortProto | Aliases]) ->
		 {Proto,Port} = port_proto(PortProto, 0),
		 {Name,Proto,Port,Aliases}
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%% Parse rpc program names
%% Syntax:
%%      Name   Program  [Aliases]  \n |
%%      # comment
%% --------------------------------------------------------------------------

rpc(File) ->
    rpc(noname, File).

rpc(Fname, File) ->
    Fn = fun([Name,Program | Aliases]) ->
		 Prog = list_to_integer(Program),
		 {Name,Prog,Aliases}
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%% Parse hosts file unix style
%% Syntax:
%%      IP Name [Aliases]  \n |
%%      # comment
%% --------------------------------------------------------------------------
hosts(File) ->
    hosts(noname,File).

hosts(Fname,File) ->
    Fn = fun([Address, Name | Aliases]) ->
		 %% XXX Fix for link-local IPv6 addresses that specify
		 %% interface with a %if suffix. These kind of
		 %% addresses maybe need to be gracefully handled
		 %% throughout inet* and inet_drv.
		 case string:tokens(Address, "%") of
		     [Addr,_] ->
			 {ok,_} = address(Addr),
			 skip;
		     _ ->
			 {ok,IP} = address(Address),
			 {IP, Name, Aliases}
		 end
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%% Parse resolv file unix style
%% Syntax:
%%      domain Domain \n
%%      nameserver IP \n
%%      search Dom1 Dom2 ... \n
%%      lookup Method1 Method2 Method3 \n
%%      # comment
%% --------------------------------------------------------------------------

resolv(File) ->
    resolv(noname,File).

resolv(Fname, File) ->
    Fn = fun(["domain", Domain]) ->
		 {domain, Domain};
	    (["nameserver", Address]) ->
		 {ok,IP} = address(Address),
		 {nameserver,IP};
	    (["search" | List]) ->
		 {search, List};
	    (["lookup" | Types]) ->
		 {lookup, Types};
	    (_) ->
		 skip  %% there are too many local options, we MUST skip
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%%
%% Parse Linux host.conf file
%% find "order" only.
%%
%% --------------------------------------------------------------------------
host_conf_linux(File) ->
    host_conf_linux(noname,File).

host_conf_linux(Fname, File) ->
    Fn = fun(["order" | Order]) ->
		 %% XXX remove ',' between entries
		 {lookup, split_comma(Order)}; 
	    (_) ->
		 skip
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%%
%% Parse Freebsd/Netbsd host.conf file
%% find "order" only.
%%
%% --------------------------------------------------------------------------
host_conf_freebsd(File) ->
    host_conf_freebsd(noname,File).

host_conf_freebsd(Fname, File) ->
    Fn = fun([Type]) -> Type end,
    case parse_file(Fname, File, Fn) of
	{ok, Ls} -> {ok, [{lookup, Ls}]};
	Error -> Error
    end.



%% --------------------------------------------------------------------------
%%
%% Parse BSD/OS irs.conf file
%% find "hosts" only and ignore options.
%%
%% Syntax: 
%%      Map AccessMethod [,AccessMethod] [continue|merge [,merge|,continue]] \n
%%      # comment

%% --------------------------------------------------------------------------
host_conf_bsdos(File) ->
    host_conf_bsdos(noname,File).

host_conf_bsdos(Fname, File) ->
    Fn = fun(["hosts" | List]) ->
		 delete_options(split_comma(List));
	    (_) ->
		 skip
	 end,
    case parse_file(Fname, File, Fn) of
	{ok, Ls} ->
	    {ok, [{lookup, lists:append(Ls)}]};
	Error -> Error
    end.

delete_options(["continue"|T]) ->
    delete_options(T);
delete_options(["merge"|T]) ->
    delete_options(T);
delete_options([H|T]) ->
    [H|delete_options(T)];
delete_options([]) ->
    [].


%% --------------------------------------------------------------------------
%%
%% Parse Solaris nsswitch.conf
%% find "hosts:" only
%%
%% --------------------------------------------------------------------------

nsswitch_conf(File) ->
    nsswitch_conf(noname,File).

nsswitch_conf(Fname, File) ->
    Fn = fun(["hosts:" | Types]) ->
		 {lookup, Types};
	    (_) -> skip
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%% Parse protocol file unix style
%% Syntax:
%%      name protocol number name \n
%%      # comment
%% --------------------------------------------------------------------------

protocols(File) ->
    protocols(noname,File).

protocols(Fname, File) ->
    Fn = fun([Name, Number, DName]) ->
		 {list_to_atom(Name), list_to_integer(Number), DName}
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%% Parse netmasks file unix style
%% Syntax:
%%      Network  Subnetmask
%%      # comment
%% --------------------------------------------------------------------------

netmasks(File) ->
    netmasks(noname, File).

netmasks(Fname, File) ->
    Fn = fun([Net, Subnetmask]) ->
		 {ok, NetIP} = address(Net),
		 {ok, Mask} =  address(Subnetmask),
		 {NetIP, Mask}
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%% Parse networks file unix style
%% Syntax:
%%      network-name  network-number aliases ...
%%      # comment
%% --------------------------------------------------------------------------

networks(File) ->
    networks(noname, File).

networks(Fname, File) ->
    Fn = fun([NetName, NetNumber]) ->
		 Number = list_to_integer(NetNumber),
		 {NetName, Number}
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%%
%% Simple Line by Line parser
%%
%% --------------------------------------------------------------------------

parse_file(Fname, {fd,Fd}, Fn) ->
    parse_fd(Fname,Fd, 1, Fn, []);
parse_file(Fname, {chars,Cs}, Fn) when is_list(Cs) ->
    parse_cs(Fname, Cs, 1, Fn, []);
parse_file(Fname, {chars,Cs}, Fn) when is_binary(Cs) ->
    parse_cs(Fname, binary_to_list(Cs), 1, Fn, []);
parse_file(_, File, Fn) ->
    case file:open(File, [read]) of
	{ok, Fd} ->
	    Result = parse_fd(File,Fd, 1, Fn, []),
	    _ = file:close(Fd),
	    Result;
	Error -> Error
    end.

parse_fd(Fname,Fd, Line, Fun, Ls) ->
    case read_line(Fd) of
	eof -> {ok, reverse(Ls)};
	Cs ->
	    case split_line(Cs) of
		[] -> parse_fd(Fname, Fd, Line+1, Fun, Ls);
		Toks ->
		    case catch Fun(Toks) of
			{'EXIT',_} ->
			    error("~p:~p: erroneous line, SKIPPED~n",[Fname,Line]),
			    parse_fd(Fname, Fd,Line+1,Fun,Ls);
			{warning,Wlist,Val} ->
			    warning("~p:~p: warning! strange domain name(s) ~p ~n",[Fname,Line,Wlist]),
			    parse_fd(Fname, Fd,Line+1,Fun,[Val|Ls]);

			skip -> 
			    parse_fd(Fname, Fd, Line+1, Fun, Ls);
			Val -> parse_fd(Fname, Fd, Line+1, Fun, [Val|Ls])
		    end
	    end
    end.

parse_cs(Fname, Chars, Line, Fun, Ls) ->
    case get_line(Chars) of
	eof -> {ok, reverse(Ls)};
	{Cs,Chars1} ->
	    case split_line(Cs) of
		[] -> parse_cs(Fname, Chars1, Line+1, Fun, Ls);
		Toks ->
		    case catch Fun(Toks) of
			{'EXIT',_} -> 
			    error("~p:~p: erroneous line, SKIPPED~n",[Fname,Line]),
 			    parse_cs(Fname, Chars1, Line+1, Fun, Ls);
			{warning,Wlist,Val} ->
			    warning("~p:~p: warning! strange domain name(s) ~p ~n",[Fname,Line,Wlist]),
			    parse_cs(Fname, Chars1, Line+1, Fun, [Val|Ls]);

			skip -> parse_cs(Fname, Chars1, Line+1, Fun, Ls);
			Val -> parse_cs(Fname, Chars1, Line+1, Fun, [Val|Ls])
		    end
	    end
    end.

get_line([]) -> eof;
get_line(Chars) -> get_line(Chars,[]).

get_line([], Acc) -> {reverse(Acc), []};
get_line([$\r, $\n | Cs], Acc) -> {reverse([$\n|Acc]), Cs};
get_line([$\n | Cs], Acc) -> {reverse([$\n|Acc]), Cs};
get_line([C | Cs], Acc) -> get_line(Cs, [C|Acc]).

%%
%% Read a line
%%
read_line(Fd) when is_pid(Fd) -> io:get_line(Fd, '');
read_line(Fd = #file_descriptor{}) ->
    collect_line(Fd, []).

collect_line(Fd, Cs) ->
    case file:read(Fd, 80) of
	{ok, Line} when is_binary(Line) ->
	    collect_line(Fd, byte_size(Line), binary_to_list(Line), Cs);
	{ok, Line} ->
	    collect_line(Fd, length(Line), Line, Cs);
	eof when Cs =:= [] ->
	    eof;
	eof -> reverse(Cs)
    end.    

collect_line(Fd, N, [$\r, $\n|_], Cs) ->
    {ok, _} = file:position(Fd, {cur,-(N-2)}),
    reverse([$\n|Cs]);
collect_line(Fd, N, [$\n|_], Cs) ->
    {ok, _} = file:position(Fd, {cur,-(N-1)}),
    reverse([$\n|Cs]);
collect_line(Fd, _, [], Cs) ->
    collect_line(Fd, Cs);
collect_line(Fd, N, [X|Xs], Cs) ->
    collect_line(Fd, N-1, Xs, [X|Cs]).


%% split Port/Proto -> {Port, Proto}
port_proto([X|Xs], N) when X >= $0, X =< $9 -> 
    port_proto(Xs, N*10 + (X - $0));
port_proto([$/ | Proto], Port) when Port =/= 0 -> 
    {list_to_atom(Proto), Port}.

%%
%% Check if a String is a string with visible characters #21..#7E
%% visible_string(String) -> Bool
%%
visible_string([H|T]) ->
    is_vis1([H|T]);
visible_string(_) ->
    false.

is_vis1([C | Cs]) when C >= 16#21, C =< 16#7e -> is_vis1(Cs);
is_vis1([]) -> true;
is_vis1(_) -> false.

%%
%% Check if a String is a domain name according to RFC XXX.
%% domain(String) -> Bool
%%
domain([H|T]) ->
    is_dom1([H|T]);
domain(_) ->
    false.

is_dom1([C | Cs]) when C >= $a, C =< $z -> is_dom_ldh(Cs);
is_dom1([C | Cs]) when C >= $A, C =< $Z -> is_dom_ldh(Cs);
is_dom1([C | Cs]) when C >= $0, C =< $9 -> 
    case is_dom_ldh(Cs) of
	true  -> is_dom2(string:tokens([C | Cs],"."));
	false -> false
    end;
is_dom1(_) -> false.

is_dom_ldh([C | Cs]) when C >= $a, C =< $z -> is_dom_ldh(Cs);
is_dom_ldh([C | Cs]) when C >= $A, C =< $Z -> is_dom_ldh(Cs);
is_dom_ldh([C | Cs]) when C >= $0, C =< $9 -> is_dom_ldh(Cs);
is_dom_ldh([$-,$. | _]) -> false;
is_dom_ldh([$_,$. | _]) -> false;
is_dom_ldh([$_ | Cs]) -> is_dom_ldh(Cs);
is_dom_ldh([$- | Cs]) -> is_dom_ldh(Cs);
is_dom_ldh([$. | Cs]) -> is_dom1(Cs);
is_dom_ldh([]) -> true;
is_dom_ldh(_) -> false.

%%% Check that we don't get a IP-address as a domain name.

-define(L2I(L), (catch list_to_integer(L))).

is_dom2([A,B,C,D]) ->
    case ?L2I(D) of
	Di when is_integer(Di) ->
	    case {?L2I(A),?L2I(B),?L2I(C)} of
		{Ai,Bi,Ci} when is_integer(Ai),
                                is_integer(Bi),
                                is_integer(Ci) -> false;
		_ -> true
	    end;
	_ -> true
    end;
is_dom2(_) ->
    true.



%%
%% Parse ipv4 address or ipv6 address
%% Return {ok, Address} | {error, Reason}
%%
address(Cs) when is_list(Cs) ->
    case ipv4_address(Cs) of
	{ok,IP} ->
	    {ok,IP};
	_ ->
	    ipv6strict_address(Cs)
    end;
address(_) -> 
    {error, einval}.

%%Parse ipv4 strict address or ipv6 strict address
strict_address(Cs) when is_list(Cs) ->
    case ipv4strict_address(Cs) of
	{ok,IP} ->
	    {ok,IP};
	_ ->
	    ipv6strict_address(Cs)
    end;
strict_address(_) ->
    {error, einval}.

%%
%% Parse IPv4 address: 
%%    d1.d2.d3.d4
%%    d1.d2.d4
%%    d1.d4
%%    d4
%% Any d may be octal, hexadecimal or decimal by C language standards.
%% d4 fills all LSB bytes. This is legacy behaviour from Solaris
%% and FreeBSD. And partly Linux that behave the same except
%% it does not accept hexadecimal.
%%
%% Return {ok, IP} | {error, einval}
%%
ipv4_address(Cs) ->
    try ipv4_addr(Cs) of
	Addr ->
	    {ok,Addr}
    catch
	error:badarg ->
	    {error,einval}
    end.

ipv4_addr(Cs) ->
    case ipv4_addr(Cs, []) of
	[D] when D < (1 bsl 32) ->
	    <<D1,D2,D3,D4>> = <<D:32>>,
	    {D1,D2,D3,D4};
	[D,D1] when D < (1 bsl 24), D1 < 256 ->
	    <<D2,D3,D4>> = <<D:24>>,
	    {D1,D2,D3,D4};
	[D,D2,D1] when D < (1 bsl 16), (D2 bor D1) < 256 ->
	    <<D3,D4>> = <<D:16>>,
	    {D1,D2,D3,D4};
	[D4,D3,D2,D1] when (D4 bor D3 bor D2 bor D1) < 256 ->
	    {D1,D2,D3,D4};
	_ ->
	    erlang:error(badarg)
    end.

ipv4_addr([_|_], [_,_,_,_]) ->
    %% Early bailout for extra characters
    erlang:error(badarg);
ipv4_addr("0x"++Cs, Ds) ->
    ipv4_addr(strip0(Cs), Ds, [], 16, 8);
ipv4_addr("0X"++Cs, Ds) ->
    ipv4_addr(strip0(Cs), Ds, [], 16, 8);
ipv4_addr("0"++Cs, Ds) ->
    ipv4_addr(strip0(Cs), Ds, [$0], 8, 11);
ipv4_addr(Cs, Ds) when is_list(Cs) ->
    ipv4_addr(Cs, Ds, [], 10, 10).

ipv4_addr(Cs0, Ds, Rs, Base, N) ->
    case ipv4_field(Cs0, N, Rs, Base) of
	{D,""} ->
	    [D|Ds];
	{D,[$.|[_|_]=Cs]} ->
	    ipv4_addr(Cs, [D|Ds]);
	{_,_} ->
	    erlang:error(badarg)
    end.

strip0("0"++Cs) ->
    strip0(Cs);
strip0(Cs) when is_list(Cs) ->
    Cs.


%%
%% Parse IPv4 strict dotted decimal address, no leading zeros:
%%    d1.d2.d3.d4
%%
%% Return {ok, IP} | {error, einval}
%%
ipv4strict_address(Cs) ->
    try ipv4strict_addr(Cs) of
	Addr ->
	    {ok,Addr}
    catch
	error:badarg ->
	    {error,einval}
    end.

ipv4strict_addr(Cs) ->
    case ipv4strict_addr(Cs, []) of
	[D4,D3,D2,D1] when (D4 bor D3 bor D2 bor D1) < 256 ->
	    {D1,D2,D3,D4};
	_ ->
	    erlang:error(badarg)
    end.

ipv4strict_addr([_|_], [_,_,_,_]) ->
    %% Early bailout for extra characters
    erlang:error(badarg);
ipv4strict_addr("0", Ds) ->
    [0|Ds];
ipv4strict_addr("0."++Cs, Ds) ->
    ipv4strict_addr(Cs, [0|Ds]);
ipv4strict_addr(Cs0, Ds) when is_list(Cs0) ->
    case ipv4_field(Cs0, 3, [], 10) of
	{D,""} ->
	    [D|Ds];
	{D,[$.|[_|_]=Cs]} ->
	    ipv4strict_addr(Cs, [D|Ds]);
	{_,_} ->
	    erlang:error(badarg)
    end.



ipv4_field("", _, Rs, Base) ->
    {ipv4_field(Rs, Base),""};
ipv4_field("."++_=Cs, _, Rs, Base) ->
    {ipv4_field(Rs, Base),Cs};
ipv4_field("0"++_, _, [], _) ->
    erlang:error(badarg);
ipv4_field([C|Cs], N, Rs, Base) when N > 0 ->
    ipv4_field(Cs, N-1, [C|Rs], Base);
ipv4_field(Cs, _, _, _) when is_list(Cs) ->
    erlang:error(badarg).

ipv4_field(Rs, Base) ->
    V = erlang:list_to_integer(lists:reverse(Rs), Base),
    if  V < 0 ->
	    erlang:error(badarg);
	true ->
	    V
    end.



%%
%% Forgiving IPv6 address
%%
%% Accepts IPv4 address and returns it as a IPv4 compatible IPv6 address
%%
ipv6_address(Cs) ->
    case ipv4_address(Cs) of
	{ok,{D1,D2,D3,D4}} ->
	    {ok,{0,0,0,0,0,16#ffff,(D1 bsl 8) bor D2,(D3 bsl 8) bor D4}};
	_ ->
	    ipv6strict_address(Cs)
    end.

%%
%% Parse IPv6 address according to RFC 4291:
%%     x1:x2:x3:x4:x5:x6:x7:x8
%%     x1:x2::x7:x8
%%     ::x7:x8
%%     x1:x2::
%%     ::
%%     x1:x2:x3:x4:x5:x6:d7a.d7b.d8a.d8b
%%     x1:x2::x5:x6:d7a.d7b.d8a.d8b
%%     ::x5:x6:d7a.d7b.d8a.d8b
%%     x1:x2::d7a.d7b.d8a.d8b
%%     ::d7a.d7b.d8a.d8b
%%     etc
%%
%% Return {ok, IP} | {error, einval}
%%
ipv6strict_address(Cs) ->
    try ipv6_addr(Cs) of
	Addr ->
	    {ok,Addr}
    catch
	error:badarg ->
	    {error,einval}
    end.

ipv6_addr("::") ->
    ipv6_addr_done([], [], 0);
ipv6_addr("::"++Cs) ->
    ipv6_addr(hex(Cs), [], [], 0);
ipv6_addr(Cs) ->
    ipv6_addr(hex(Cs), [], 0).

%% Before "::"
ipv6_addr({Cs0,[]}, A, N) when N == 7 ->
    ipv6_addr_done([hex_to_int(Cs0)|A]);
ipv6_addr({Cs0,"::"}, A, N) when N =< 6 ->
    ipv6_addr_done([hex_to_int(Cs0)|A], [], N+1);
ipv6_addr({Cs0,"::"++Cs1}, A, N) when N =< 5 ->
    ipv6_addr(hex(Cs1), [hex_to_int(Cs0)|A], [], N+1);
ipv6_addr({Cs0,":"++Cs1}, A, N) when N =< 6 ->
    ipv6_addr(hex(Cs1), [hex_to_int(Cs0)|A], N+1);
ipv6_addr({Cs0,"."++_=Cs1}, A, N) when N == 6 ->
    ipv6_addr_done(A, [], N, ipv4strict_addr(Cs0++Cs1));
ipv6_addr(_, _, _) ->
    erlang:error(badarg).

%% After "::"
ipv6_addr({Cs0,[]}, A, B, N) when N =< 6 ->
    ipv6_addr_done(A, [hex_to_int(Cs0)|B], N+1);
ipv6_addr({Cs0,":"++Cs1}, A, B, N) when N =< 5 ->
    ipv6_addr(hex(Cs1), A, [hex_to_int(Cs0)|B], N+1);
ipv6_addr({Cs0,"."++_=Cs1}, A, B, N) when N =< 5 ->
    ipv6_addr_done(A, B, N, ipv4strict_addr(Cs0++Cs1));
ipv6_addr(_, _, _, _) ->
    erlang:error(badarg).

ipv6_addr_done(Ar, Br, N, {D1,D2,D3,D4}) ->
    ipv6_addr_done(Ar, [((D3 bsl 8) bor D4),((D1 bsl 8) bor D2)|Br], N+2).

ipv6_addr_done(Ar, Br, N) ->
    ipv6_addr_done(Br++dup(8-N, 0, Ar)).

ipv6_addr_done(Ar) ->
    list_to_tuple(lists:reverse(Ar)).

%% Collect 1-4 Hex digits
hex(Cs) -> hex(Cs, [], 4).
%%
hex([C|Cs], R, N) when C >= $0, C =< $9, N > 0 ->
    hex(Cs, [C|R], N-1);
hex([C|Cs], R, N) when C >= $a, C =< $f, N > 0 ->
    hex(Cs, [C|R], N-1);
hex([C|Cs], R, N) when C >= $A, C =< $F, N > 0 ->
    hex(Cs, [C|R], N-1);
hex(Cs, [_|_]=R, _) when is_list(Cs) ->
    {lists:reverse(R),Cs};
hex(_, _, _) ->
    erlang:error(badarg).

%% Hex string to integer
hex_to_int(Cs) -> erlang:list_to_integer(Cs, 16).

%% Dup onto head of existing list
dup(0, _, L) ->
    L;
dup(N, E, L) when is_integer(N), N >= 1 ->
    dup(N-1, E, [E|L]).



%% Convert IPv4 adress to ascii
%% Convert IPv6 / IPV4 adress to ascii (plain format)
ntoa({A,B,C,D}) ->
    integer_to_list(A) ++ "." ++ integer_to_list(B) ++ "." ++ 
	integer_to_list(C) ++ "." ++ integer_to_list(D);
%% ANY
ntoa({0,0,0,0,0,0,0,0}) -> "::";
%% LOOPBACK
ntoa({0,0,0,0,0,0,0,1}) -> "::1";
%% IPV4 ipv6 host address
ntoa({0,0,0,0,0,0,A,B}) -> "::" ++ dig_to_dec(A) ++ "." ++ dig_to_dec(B);
%% IPV4 non ipv6 host address
ntoa({0,0,0,0,0,16#ffff,A,B}) -> 
    "::FFFF:" ++ dig_to_dec(A) ++ "." ++ dig_to_dec(B);
ntoa({_,_,_,_,_,_,_,_}=T) ->
    %% Find longest sequence of zeros, at least 2, to replace with "::"
    ntoa(tuple_to_list(T), []);
ntoa(_) ->
    {error, einval}.

%% Find first double zero
ntoa([], R) ->
    ntoa_done(R);
ntoa([0,0|T], R) ->
    ntoa(T, R, 2);
ntoa([D|T], R) ->
    ntoa(T, [D|R]).

%% Count consecutive zeros
ntoa([], R, _) ->
    ntoa_done(R, []);
ntoa([0|T], R, N) ->
    ntoa(T, R, N+1);
ntoa([D|T], R, N) ->
    ntoa(T, R, N, [D]).

%% Find alternate double zero
ntoa([], R1, _N1, R2) ->
    ntoa_done(R1, R2);
ntoa([0,0|T], R1, N1, R2) ->
    ntoa(T, R1, N1, R2, 2);
ntoa([D|T], R1, N1, R2) ->
    ntoa(T, R1, N1, [D|R2]).

%% Count consecutive alternate zeros
ntoa(T, R1, N1, R2, N2) when N2 > N1 ->
    %% Alternate zero sequence is longer - use it instead
    ntoa(T, R2++dup(N1, 0, R1), N2);
ntoa([], R1, _N1, R2, N2) ->
    ntoa_done(R1, dup(N2, 0, R2));
ntoa([0|T], R1, N1, R2, N2) ->
    ntoa(T, R1, N1, R2, N2+1);
ntoa([D|T], R1, N1, R2, N2) ->
    ntoa(T, R1, N1, [D|dup(N2, 0, R2)]).

ntoa_done(R1, R2) ->
    lists:append(
      separate(":", lists:map(fun dig_to_hex/1, lists:reverse(R1)))++
      ["::"|separate(":", lists:map(fun dig_to_hex/1, lists:reverse(R2)))]).

ntoa_done(R) ->
    lists:append(separate(":", lists:map(fun dig_to_hex/1, lists:reverse(R)))).

separate(_E, []) ->
    [];
separate(E, [_|_]=L) ->
    separate(E, L, []).

separate(E, [H|[_|_]=T], R) ->
    separate(E, T, [E,H|R]);
separate(_E, [H], R) ->
    lists:reverse(R, [H]).

%% convert to A.B decimal form
dig_to_dec(0) -> "0.0";
dig_to_dec(X) -> 
    integer_to_list((X bsr 8) band 16#ff) ++ "." ++
	integer_to_list(X band 16#ff).

%% Convert a integer to hex string
dig_to_hex(X) ->
    erlang:integer_to_list(X, 16).

%%
%% Count number of '.' in a name
%% return {Number of non-terminating dots, has-terminating dot?}
%%        {integer, bool}
%%
dots(Name) -> dots(Name, 0).

dots([$.], N) -> {N, true};
dots([$. | T], N) -> dots(T, N+1);
dots([_C | T], N) -> dots(T, N);
dots([], N) -> {N, false}.


split_line(Line) ->
    split_line(Line, []).

split_line([$# | _], Tokens) ->  reverse(Tokens);
split_line([$\s| L], Tokens) ->  split_line(L, Tokens);
split_line([$\t | L], Tokens) -> split_line(L, Tokens);
split_line([$\n | L], Tokens) -> split_line(L, Tokens);
split_line([], Tokens) -> reverse(Tokens);
split_line([C|Cs], Tokens) -> split_mid(Cs, [C], Tokens).

split_mid([$# | _Cs], Acc, Tokens) -> split_end(Acc, Tokens);
split_mid([$\s | Cs], Acc, Tokens) -> split_line(Cs, [reverse(Acc) | Tokens]);
split_mid([$\t | Cs], Acc, Tokens) -> split_line(Cs, [reverse(Acc) | Tokens]);
split_mid([$\r, $\n | Cs], Acc, Tokens) -> split_line(Cs, [reverse(Acc) | Tokens]);
split_mid([$\n | Cs], Acc, Tokens) -> split_line(Cs, [reverse(Acc) | Tokens]);
split_mid([], Acc, Tokens) -> split_end(Acc, Tokens);
split_mid([C|Cs], Acc, Tokens) -> split_mid(Cs, [C|Acc], Tokens).

split_end(Acc, Tokens) -> reverse([reverse(Acc) | Tokens]).


%% Split a comma separated tokens. Because we already have split on
%% spaces we may have the cases
%%
%%        ",foo"
%%        "foo,"
%%        "foo,bar..."
 
split_comma([]) ->
    [];
split_comma([Token | Tokens]) ->
    split_comma(Token, []) ++ split_comma(Tokens).
 
split_comma([], Tokens) ->       reverse(Tokens);
split_comma([$, | L], Tokens) -> split_comma(L, Tokens);
split_comma([C|Cs], Tokens) ->   split_mid_comma(Cs, [C], Tokens).
 
split_mid_comma([$, | Cs], Acc, Tokens) ->
    split_comma(Cs, [reverse(Acc) | Tokens]);
split_mid_comma([], Acc, Tokens) ->
    split_end(Acc, Tokens);
split_mid_comma([C|Cs], Acc, Tokens) ->
    split_mid_comma(Cs, [C|Acc], Tokens).

%%

warning(Fmt, Args) ->
    case application:get_env(kernel,inet_warnings) of
	{ok,on} -> 
	    error_logger:info_msg("inet_parse:" ++ Fmt, Args);
	_ ->
	    ok
    end.

error(Fmt, Args) ->
    error_logger:info_msg("inet_parse:" ++ Fmt, Args).

