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
-module(inet_parse).

%% Parser for all kinds of ineternet configuration files

-export([hosts/1, hosts/2]).
-export([hosts_vxworks/1]).
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
-export([address/1]).
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
%% Parse hostShow vxworks style
%% Syntax:
%%      Name          IP                [Aliases]  \n 
%% --------------------------------------------------------------------------
hosts_vxworks(Hosts) ->
    Fn = fun([Name, Address | Aliases]) ->
		 {ok,IP} = address(Address),
		 {IP, Name, Aliases}
	 end,
    parse_file(Hosts, Fn).

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

parse_file(File, Fn) ->
    parse_file(noname, File, Fn).

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
	    file:close(Fd),
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
%% Test ipv4 address or ipv6 address
%% Return {ok, Address} | {error, Reason}
%%
address(Cs) when is_list(Cs) ->
    case ipv4_address(Cs) of
	{ok,IP} -> {ok,IP};
	_ ->
	    case ipv6_address(Cs) of
		{ok, IP} -> {ok, IP};
		Error -> Error
	    end
    end;
address(_) -> 
    {error, einval}.

%%
%% Parse IPv4 address: 
%%    d1.d2.d3.d4
%%    d1.d2.d4
%%    d1.d4
%%    d4
%%
%% Return {ok, IP} | {error, einval}
%%
ipv4_address(Cs) ->
    case catch ipv4_addr(Cs) of
	{'EXIT',_} -> {error,einval};
	Addr -> {ok,Addr}
    end.

ipv4_addr(Cs) -> 
    ipv4_addr(d3(Cs), []).

ipv4_addr({Cs0,[]}, A) when length(A) =< 3 ->
    case [tod(Cs0)|A] of
	[D4,D3,D2,D1] ->
	    {D1,D2,D3,D4};
	[D4,D2,D1] ->
	    {D1,D2,0,D4};
	[D4,D1] ->
	    {D1,0,0,D4};
	[D4] ->
	    {0,0,0,D4}
    end;
ipv4_addr({Cs0,"."++Cs1}, A) when length(A) =< 2 ->
    ipv4_addr(d3(Cs1), [tod(Cs0)|A]).

d3(Cs) -> d3(Cs, []).

d3([C|Cs], R) when C >= $0, C =< $9, length(R) =< 2 ->
    d3(Cs, [C|R]);
d3(Cs, [_|_]=R) ->
    {lists:reverse(R),Cs}.

tod(Cs) ->
    case erlang:list_to_integer(Cs) of
	D when D >= 0, D =< 255 ->
	    D;
	_ ->
	    erlang:error(badarg, [Cs])
    end.

%%
%% Parse IPv6 address: 
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
%%
%% Return {ok, IP} | {error, einval}
%%
ipv6_address(Cs) ->
    case catch ipv6_addr(Cs) of
	{'EXIT',_} -> {error,einval};
	Addr -> {ok,Addr}
    end.

ipv6_addr("::") ->
    ipv6_addr_done([], []);
ipv6_addr("::"++Cs) ->
    ipv6_addr(x4(Cs), [], []);
ipv6_addr(Cs) ->
    ipv6_addr(x4(Cs), []).

%% Before "::"
ipv6_addr({Cs0,[]}, A) when length(A) =:= 7 ->
    ipv6_addr_done([tox(Cs0)|A]);
ipv6_addr({Cs0,"::"}, A) when length(A) =< 6 ->
    ipv6_addr_done([tox(Cs0)|A], []);
ipv6_addr({Cs0,"::"++Cs1}, A) when length(A) =< 5 ->
    ipv6_addr(x4(Cs1), [tox(Cs0)|A], []);
ipv6_addr({Cs0,":"++Cs1}, A) when length(A) =< 6 ->
    ipv6_addr(x4(Cs1), [tox(Cs0)|A]);
ipv6_addr({Cs0,"."++Cs1}, A) when length(A) =:= 6 ->
    ipv6_addr(d3(Cs1), A, [], [tod(Cs0)]).

%% After "::"
ipv6_addr({Cs0,[]}, A, B) when length(A)+length(B) =< 6 ->
    ipv6_addr_done(A, [tox(Cs0)|B]);
ipv6_addr({Cs0,":"++Cs1}, A, B) when length(A)+length(B) =< 5 ->
    ipv6_addr(x4(Cs1), A, [tox(Cs0)|B]);
ipv6_addr({Cs0,"."++Cs1}, A, B) when length(A)+length(B) =< 5 ->
    ipv6_addr(x4(Cs1), A, B, [tod(Cs0)]).

%% After "."
ipv6_addr({Cs0,[]}, A, B, C) when length(C) =:= 3 ->
    ipv6_addr_done(A, B, [tod(Cs0)|C]);
ipv6_addr({Cs0,"."++Cs1}, A, B, C) when length(C) =< 2 ->
    ipv6_addr(d3(Cs1), A, B, [tod(Cs0)|C]).

ipv6_addr_done(Ar, Br, [D4,D3,D2,D1]) ->
    ipv6_addr_done(Ar, [((D3 bsl 8) bor D4),((D1 bsl 8) bor D2)|Br]).

ipv6_addr_done(Ar, Br) ->
    ipv6_addr_done(Br++dup(8-length(Ar)-length(Br), 0, Ar)).

ipv6_addr_done(Ar) ->
    list_to_tuple(lists:reverse(Ar)).

x4(Cs) -> x4(Cs, []).

x4([C|Cs], R) when C >= $0, C =< $9, length(R) =< 3 ->
    x4(Cs, [C|R]);
x4([C|Cs], R) when C >= $a, C =< $f, length(R) =< 3 ->
    x4(Cs, [C|R]);
x4([C|Cs], R) when C >= $A, C =< $F, length(R) =< 3 ->
    x4(Cs, [C|R]);
x4(Cs, [_|_]=R) ->
    {lists:reverse(R),Cs}.

tox(Cs) ->
    erlang:list_to_integer(Cs, 16).

dup(0, _, L) ->
    L;
dup(N, E, L) when is_integer(N), N >= 1 ->
    dup(N-1, E, [E|L]);
dup(N, E, L) ->
    erlang:error(badarg, [N,E,L]).

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
    ntoa(tuple_to_list(T), []).

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
dig_to_dec(0) -> [$0,$.,$0];
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

