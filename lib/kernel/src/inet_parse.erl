%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2026. All Rights Reserved.
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
-moduledoc false.

-compile([{nowarn_possibly_unsafe_function, {erlang, list_to_atom, 1}}]).

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
-include("inet_int.hrl").

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
		 case string:lexemes(Address, "%") of
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
                    try Fun(Toks) of
                        {warning,Wlist,Val} ->
                            warning("~p:~p: warning! strange domain name(s) ~p ~n",[Fname,Line,Wlist]),
                            parse_fd(Fname, Fd,Line+1,Fun,[Val|Ls]);
                        skip ->
                            parse_fd(Fname, Fd, Line+1, Fun, Ls);
                        Val -> parse_fd(Fname, Fd, Line+1, Fun, [Val|Ls])
                    catch error : _ ->
                            error("~p:~p: erroneous line, SKIPPED~n",[Fname,Line]),
                            parse_fd(Fname, Fd,Line+1,Fun,Ls)
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
                    try Fun(Toks) of
                        {warning,Wlist,Val} ->
                            warning("~p:~p: warning! strange domain name(s) ~p ~n",[Fname,Line,Wlist]),
                            parse_cs(Fname, Chars1, Line+1, Fun, [Val|Ls]);

                        skip -> parse_cs(Fname, Chars1, Line+1, Fun, Ls);
                        Val -> parse_cs(Fname, Chars1, Line+1, Fun, [Val|Ls])
                    catch error : _ ->
                            error("~p:~p: erroneous line, SKIPPED~n",[Fname,Line]),
                            parse_cs(Fname, Chars1, Line+1, Fun, Ls)
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
visible_string([C | Cs]) when C >= 16#21, C =< 16#7e -> visible_string(Cs);
visible_string([]) -> true;
visible_string(_) -> false.

%%
%% Check if a String is a domain name according to RFC XXX.
%% domain(String) -> Bool
%%
%% We regard the empty domain name and domain names ending in a dot
%% as not valid domain names.  That can be debated.
domain([])                      -> false;
domain([_|_] = Cs) ->
    is_dom1(Cs) andalso
    %%
    %% Also check that we don't get a IP-address as a domain name
    %% (A valid domain name cannot be an IPv6 address
    %%  since the latter has to contain a `:`)
        try ipv4_addr(Cs) of
            _Addr               -> false
        catch throw : error     -> true
        end.

%% Each DNS label starts with letter or number and cannot be empty
is_dom1([C | Cs]) ->
    if
        is_integer(C, $a, $z);
        is_integer(C, $A, $Z);
        is_integer(C, $0, $9)   -> is_dom_ldh(Cs);
        true                    -> false
    end.

%% Within a DNS label, but not at the end, `-` and `_` are also allowed.
%% A `.` ends the label.
is_dom_ldh([])                  -> true;
is_dom_ldh([$.])                -> false;
is_dom_ldh([$_])                -> false;
is_dom_ldh([$-])                -> false;
is_dom_ldh([$_,$. | _])         -> false;
is_dom_ldh([$-,$. | _])         -> false;
is_dom_ldh([$. | Cs])           -> is_dom1(Cs);
is_dom_ldh([$_ | Cs])           -> is_dom_ldh(Cs);
is_dom_ldh([$- | Cs])           -> is_dom_ldh(Cs);
is_dom_ldh([_|_] = Cs)          -> is_dom1(Cs).


%%
%% Parse ipv4 address or ipv6 address
%% Return {ok, Address} | {error, Reason}
%%
address(Bin) when is_binary(Bin) ->
    %% Try binary parsing first, strict
    try ipv4s_c1b(Bin) of                   IP  -> {ok, IP}
    catch throw : error ->
            Cs = binary_to_list(Bin),
            try ipv4_addr(Cs) of            IP  -> {ok, IP}
            catch throw : error ->
                    try ipv6_addr(Cs) of    IP  -> {ok, IP}
                    catch throw : error         -> {error, einval}
                    end
            end
    end;
address(Cs) when is_list(Cs) ->
    case ipv4_address(Cs) of
        {ok, IP} ->
            {ok, IP};
        _ ->
            ipv6strict_address(Cs)
    end;
address(_) ->
    {error, einval}.

%%Parse ipv4 strict address or ipv6 strict address
strict_address(Addr) when is_list(Addr); is_binary(Addr) ->
    case ipv4strict_address(Addr) of
        {ok, IP} ->
            {ok, IP};
        _ ->
            ipv6strict_address(Addr)
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
ipv4_address(Bin) when is_binary(Bin) ->
    ipv4_address(binary_to_list(Bin));
ipv4_address(Cs) ->
    try ipv4_addr(Cs) of
	Addr ->
	    {ok,Addr}
    catch throw : error ->
	    {error,einval}
    end.

ipv4_addr(Cs) ->
    case ipv4_addr(Cs, []) of
        [D] when is_integer(D, 0, 16#ffff_ffff) ->
            D4 = D band 16#ff,
            Da = D bsr 8,
            D3 = Da band 16#ff,
            Db = Da bsr 8,
            D2 = Db band 16#ff,
            D1 = Db bsr 8,
            {D1,D2,D3,D4};
        [D,D1] when is_integer(D, 0, 16#ff_ffff), is_integer(D1, 0, 16#ff) ->
            D4 = D band 16#ff,
            Da = D bsr 8,
            D3 = Da band 16#ff,
            D2 = Da bsr 8,
            {D1,D2,D3,D4};
        [D,D2,D1] when
              is_integer(D, 0, 16#ffff),
              is_integer(D2, 0, 16#ff),
              is_integer(D1, 0, 16#ff) ->
            D4 = D band 16#ff,
            D3 = D bsr 8,
            {D1,D2,D3,D4};
        [D4,D3,D2,D1] when
              is_integer(D4, 0, 16#ff),
              is_integer(D3, 0, 16#ff),
              is_integer(D2, 0, 16#ff),
              is_integer(D1, 0, 16#ff) ->
            {D1,D2,D3,D4};
        _ ->
            throw(error)
    end.

ipv4_addr([_|_], [_,_,_,_]) ->
    %% Early bailout for extra characters
    throw(error);
%% 8 hex, 11 octal, or 10 decimal chars is maximum
%% needed to represent 16#ffff_ffff
ipv4_addr("0x"++Cs, Ds) ->
    ipv4_addr(Cs, Ds, [], 16, 8);
ipv4_addr("0X"++Cs, Ds) ->
    ipv4_addr(Cs, Ds, [], 16, 8);
ipv4_addr("0"++Cs, Ds) ->
    ipv4_addr(Cs, Ds, [$0], 8, 11);
ipv4_addr([C|_]=Cs, Ds) when is_integer(C, $0, $9) ->
    ipv4_addr(Cs, Ds, [], 10, 10);
%% The field does not start with a decimal digit
ipv4_addr(_, _) ->
    throw(error).


ipv4_addr(Cs0, Ds, Rs, Base, N) ->
    case ipv4_field(Cs0, N, Rs, Base) of
	{D,""} ->
	    [D|Ds];
	{D,[$.|[_|_]=Cs]} ->
	    ipv4_addr(Cs, [D|Ds]);
	{_,_} ->
            throw(error)
    end.

ipv4_field("", _, Rs, Base) ->
    {ipv4_field(Rs, Base),""};
ipv4_field("."++_=Cs, _, Rs, Base) ->
    {ipv4_field(Rs, Base),Cs};
ipv4_field([C|Cs], N, Rs, Base) when N > 0 ->
    ipv4_field(Cs, N-1, [C|Rs], Base);
ipv4_field(Cs, _, _, _) when is_list(Cs) ->
    throw(error).

ipv4_field(Rs, Base) when
      Base =:= 8;
      Base =:= 10;
      Base =:= 16 ->
    case lists:reverse(Rs) of
        [C | _] when
              C =:= $+;
              C =:= $- ->
            throw(error);
        Cs when is_list(Cs) ->
            try erlang:list_to_integer(Cs, Base) of
                V when is_integer(V, 0, 16#ffff_ffff) ->
                    V;
                _ ->
                    throw(error)
            catch error : badarg ->
                    throw(error)
            end
    end.


%%
%% Parse IPv4 strict dotted decimal address, no leading zeros:
%%    d1.d2.d3.d4
%%
%% Return {ok, IP} | {error, einval}
%%
ipv4strict_address(Bin) when is_binary(Bin) ->
    try ipv4s_c1b(Bin) of
        IP ->
            {ok, IP}
    catch throw : error ->
            {error, einval}
    end;
ipv4strict_address(Cs) when is_list(Cs) ->
    try ipv4s_c1(Cs) of
        IP ->
            {ok, IP}
    catch throw : error ->
            {error, einval}
    end.


%% Validate and create an octet from 1, 2 or 3 decimal characters
%% "0".."9", "10".."99", "100..255"
-compile({inline, [ipv4s_octet/1,ipv4s_octet/2,ipv4s_octet/3]}).
%%
%% 0..9
ipv4s_octet(C1) when is_integer((C1), $0, $9) ->
    C1 - $0;
ipv4s_octet(_) ->
    throw(error).
%%
%% 10..99
ipv4s_octet(C1, C2) when is_integer(C1, $1, $9), is_integer(C2, $0, $9) ->
    C1*10 + C2 - $0*11;
ipv4s_octet(_, _) ->
    throw(error).
%%
%% 100..199
ipv4s_octet($1, C2, C3) when is_integer(C2, $0, $9), is_integer(C3, $0, $9) ->
    (100 - $0*11) + C2*10 + C3;
%% 200..249
ipv4s_octet($2, C2, C3) when is_integer(C2, $0, $4), is_integer(C3, $0, $9) ->
    (200 - $0*11) + C2*10 + C3;
%% 250..255
ipv4s_octet($2, $5, C3) when is_integer(C3, $0, $5) ->
    (250 - $0) + C3;
ipv4s_octet(_, _, _) ->
    throw(error).

%% Single-pass charlist parser for strict IPv4 addresses.
%% Four functions, one per octet — no packed accumulator, no dot
%% counter, no bit unpacking.

ipv4s_c1(Cs) ->
    case Cs of
        [C1, $. | T]            -> ipv4s_c2(T, ipv4s_octet(C1));
        [C1, C2, $. | T]        -> ipv4s_c2(T, ipv4s_octet(C1, C2));
        [C1, C2, C3, $. | T]    -> ipv4s_c2(T, ipv4s_octet(C1, C2, C3));
        _                       -> throw(error)
    end.

ipv4s_c2(Cs, A) ->
    case Cs of
        [C1, $. | T]            -> ipv4s_c3(T, A, ipv4s_octet(C1));
        [C1, C2, $. | T]        -> ipv4s_c3(T, A, ipv4s_octet(C1, C2));
        [C1, C2, C3, $. | T]    -> ipv4s_c3(T, A, ipv4s_octet(C1, C2, C3));
        _                       -> throw(error)
    end.

ipv4s_c3(Cs, A, B) ->
    case Cs of
        [C1, $. | T]            -> ipv4s_c4(T, A, B, ipv4s_octet(C1));
        [C1, C2, $. | T]        -> ipv4s_c4(T, A, B, ipv4s_octet(C1, C2));
        [C1, C2, C3, $. | T]    -> ipv4s_c4(T, A, B, ipv4s_octet(C1, C2, C3));
        _                       -> throw(error)
    end.

ipv4s_c4(Cs, A, B, C) ->
    case Cs of
        [C1]                    -> {A, B, C, ipv4s_octet(C1)};
        [C1, C2]                -> {A, B, C, ipv4s_octet(C1, C2)};
        [C1, C2, C3]            -> {A, B, C, ipv4s_octet(C1, C2, C3)};
        _                       -> throw(error)
    end.


%% Single-pass binary parser for strict IPv4 addresses.
%% Four functions, one per octet.  Each matches 1-3 digits and
%% passes the parsed value forward as a plain parameter — no
%% packed-integer accumulator, no dot counter, no bit unpacking.
%% BEAM reuses the match context throughout.

ipv4s_c1b(Bin) ->
    case Bin of
        <<C1,$.,R/binary>>       -> ipv4s_c2b(R, ipv4s_octet(C1));
        <<C1,C2,$.,R/binary>>    -> ipv4s_c2b(R, ipv4s_octet(C1, C2));
        <<C1,C2,C3,$.,R/binary>> -> ipv4s_c2b(R, ipv4s_octet(C1, C2, C3));
        <<_/binary>>             -> throw(error)
    end.

ipv4s_c2b(Bin, A) ->
    case Bin of
        <<C1,$.,R/binary>>       -> ipv4s_c3b(R, A, ipv4s_octet(C1));
        <<C1,C2,$.,R/binary>>    -> ipv4s_c3b(R, A, ipv4s_octet(C1, C2));
        <<C1,C2,C3,$.,R/binary>> -> ipv4s_c3b(R, A, ipv4s_octet(C1, C2, C3));
        <<_/binary>>             -> throw(error)
    end.

ipv4s_c3b(Bin, A, B) ->
    case Bin of
        <<C,$.,R/binary>>        -> ipv4s_c4b(R, A, B, ipv4s_octet(C));
        <<C1,C2,$.,R/binary>>    -> ipv4s_c4b(R, A, B, ipv4s_octet(C1, C2));
        <<C1,C2,C3,$.,R/binary>> -> ipv4s_c4b(R, A, B, ipv4s_octet(C1, C2, C3));
        <<_/binary>>             -> throw(error)
    end.

ipv4s_c4b(Bin, A, B, C) ->
    case Bin of
        <<C1>>          -> {A,B,C,ipv4s_octet(C1)};
        <<C1, C2>>      -> {A,B,C,ipv4s_octet(C1, C2)};
        <<C1, C2, C3>>  -> {A,B,C,ipv4s_octet(C1, C2, C3)};
        <<_/binary>>             -> throw(error)
    end.


%%
%% Forgiving IPv6 address
%%
%% Accepts IPv4 address and returns it as a IPv4 compatible IPv6 address
%%
ipv6_address(Bin) when is_binary(Bin) ->
    try ipv4s_c1b(Bin) of
        {D1, D2, D3, D4} ->
            {ok, {0, 0, 0, 0, 0, 16#ffff,
                  (D1 bsl 8) bor D2, (D3 bsl 8) bor D4}}
    catch throw : error ->
            case ipv6strict_address(Bin) of
                {ok, _} = Ok -> Ok;
                {error, _} -> ipv6_address(binary_to_list(Bin))
            end
    end;
ipv6_address(Cs) ->
    case ipv4_address(Cs) of
        {ok, {D1, D2, D3, D4}} ->
            {ok, {0, 0, 0, 0, 0, 16#ffff, (D1 bsl 8) bor D2, (D3 bsl 8) bor D4}};
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
ipv6strict_address(Bin) when is_binary(Bin) ->
    ipv6strict_address(binary_to_list(Bin));
ipv6strict_address(Cs) when is_list(Cs) ->
    try ipv6_addr(Cs) of
        Addr ->
            {ok, Addr}
    catch throw : error ->
            {error, einval}
    end;
ipv6strict_address(_) ->
    {error, einval}.

ipv6_addr("") ->
    throw(error); % Null string
ipv6_addr("::") ->
    ipv6_addr_done(0, [], [], true);
ipv6_addr([$: | Cs]) ->
    ipv6_addr(Cs, [], [], false, 0);
ipv6_addr([_ | _] = Cs) ->
    ipv6_addr(Cs, [], [], false, 0).

-compile({inline, [ipv6_hex_digit/1]}).
ipv6_hex_digit(C) ->
    if  is_integer(C, $0, $9) -> C - $0;
        is_integer(C, $a, $f) -> C - ($a-10);
        is_integer(C, $A, $F) -> C - ($A-10);
        true -> throw(error) % Invalid character
    end.

%% 1..4 hex characters in a field, convert to field value, X
-define(ipv6_hex_field(C), (ipv6_hex_digit(C))).
-define(ipv6_hex_field(C1, C2),
        ((ipv6_hex_digit(C1) bsl 4) bor ipv6_hex_digit(C2))).
-define(ipv6_hex_field(C1, C2, C3),
        ((((ipv6_hex_digit(C1) bsl 4) bor ipv6_hex_digit(C2)) bsl 4)
             bor ipv6_hex_digit(C3))).
-define(ipv6_hex_field(C1, C2, C3, C4),
        ((((((ipv6_hex_digit(C1) bsl 4) bor ipv6_hex_digit(C2)) bsl 4)
               bor ipv6_hex_digit(C3)) bsl 4) bor ipv6_hex_digit(C4))).

%% At start of a field, or rather;
%% first in the string or after the separator char of the previous field.
%% Only the : separator comes back to this loop; the . and the %
%% jumps out to parsing the tail.
%%
%% Cs: Characters to parse
%% Ar: Reverse list of IPv6 address words
%% Br: Reverse list of IPv6 address words
%% N:  The number of address words seen
%% Compr :: bool(), true if we have seen ::, i.e compressed zeros
%%
%% Before we see a ::, Ar has all words.  When we see a :: we swap Ar and Br
%% so Br becomes the words before the ::, and Ar the words after.
%%
%% Characters after the eight field
ipv6_addr(Cs, _Ar, _Br, _Compr, N) when is_list(Cs), N >= 8 ->
    throw(error); % Missing end
%%
%% Empty field
ipv6_addr([$: | Cs], Ar, Br, Compr, N) ->
    case Compr of
        false -> % This is the first ::
            case Cs of
                [] ->
                    ipv6_addr_done(N, Br, Ar, true);
                [$% | _] ->
                    ipv6_addr_scope(tl(Cs), Br, Ar, true, N, 0);
                [_ | _] ->
                    ipv6_addr(Cs, Br, [0 | Ar], true, N+1)
            end;
        true  ->
            throw(error) % More than one ::, or :::
    end;
ipv6_addr([$% | _], _Ar, _Br, _Compr, _N) ->
    throw(error); % Empty field before scope id suffix
ipv6_addr([$. | _], _Ar, _Br, _Compr, _N) ->
    throw(error); % Empty decimal field
%%
%% One character field
ipv6_addr([C1], Ar, Br, Compr, N) ->
    ipv6_addr_done(?ipv6_hex_field(C1), Ar, Br, Compr, N);
ipv6_addr([C1, $: | Cs], Ar, Br, Compr, N) ->
    ipv6_addr_field(Cs, Ar, Br, Compr, N, ?ipv6_hex_field(C1));
ipv6_addr([C1, $% | Cs], Ar, Br, Compr, N) ->
    ipv6_addr_scope(Cs, Ar, Br, Compr, N, ?ipv6_hex_field(C1));
ipv6_addr([C1, $. | Cs], Ar, Br, Compr, N) ->
    ipv6_addr_v4(Cs, Ar, Br, Compr, N, ipv4s_octet(C1));
%%
%% Two characters field
ipv6_addr([C1, C2], Ar, Br, Compr, N) ->
    ipv6_addr_done(?ipv6_hex_field(C1, C2), Ar, Br, Compr, N);
ipv6_addr([C1, C2, $: | Cs], Ar, Br, Compr, N) ->
    ipv6_addr_field(Cs, Ar, Br, Compr, N, ?ipv6_hex_field(C1, C2));
ipv6_addr([C1, C2, $% | Cs], Ar, Br, Compr, N) ->
    ipv6_addr_scope(Cs, Ar, Br, Compr, N, ?ipv6_hex_field(C1, C2));
ipv6_addr([C1, C2, $. | Cs], Ar, Br, Compr, N) ->
    ipv6_addr_v4(Cs, Ar, Br, Compr, N, ipv4s_octet(C1, C2));
%%
%% Three characters field
ipv6_addr([C1, C2, C3], Ar, Br, Compr, N) ->
    ipv6_addr_done(?ipv6_hex_field(C1, C2, C3), Ar, Br, Compr, N);
ipv6_addr([C1, C2, C3, $: | Cs], Ar, Br, Compr, N) ->
    ipv6_addr_field(Cs, Ar, Br, Compr, N, ?ipv6_hex_field(C1, C2, C3));
ipv6_addr([C1, C2, C3, $% | Cs], Ar, Br, Compr, N) ->
    ipv6_addr_scope(Cs, Ar, Br, Compr, N, ?ipv6_hex_field(C1, C2, C3));
ipv6_addr([C1, C2, C3, $. | Cs], Ar, Br, Compr, N) ->
    ipv6_addr_v4(Cs, Ar, Br, Compr, N, ipv4s_octet(C1, C2, C3));
%%
%% Four characters field
ipv6_addr([C1, C2, C3, C4], Ar, Br, Compr, N) ->
    ipv6_addr_done(?ipv6_hex_field(C1, C2, C3, C4), Ar, Br, Compr, N);
ipv6_addr([C1, C2, C3, C4, $: | Cs], Ar, Br, Compr, N) ->
    ipv6_addr_field(Cs, Ar, Br, Compr, N, ?ipv6_hex_field(C1, C2, C3, C4));
ipv6_addr([C1, C2, C3, C4, $% | Cs], Ar, Br, Compr, N) ->
    ipv6_addr_scope(Cs, Ar, Br, Compr, N, ?ipv6_hex_field(C1, C2, C3, C4));
%%
%% More than four characters field,
%% or first IPv4 suffix field with more than three characters
ipv6_addr([_ | _], _Ar, _Br, _Compr, _N) ->
    throw(error). % Too wide field

ipv6_addr_done(X, Ar, Br, Compr, N) ->
    ipv6_addr_done(N+1, [X | Ar], Br, Compr).
%%
ipv6_addr_done(N, Ar, Br, Compr) ->
    list_to_tuple(ipv6_addr_fill_zeros(N, Ar, Br, Compr)).

%% IPv6 hex field
ipv6_addr_field([], _Ar, _Br, _Compr, _N, _X) ->
    throw(error); % Truncated after :
ipv6_addr_field(Cs, Ar, Br, Compr, N, X) when is_list(Cs) ->
    ipv6_addr(Cs, [X | Ar], Br, Compr, N+1).

%% After %
ipv6_addr_scope([], _Ar, _Br, _Compr, _N, _X) ->
    throw(error); %<zone_id> has to be a non-null string (RFC 4007)
ipv6_addr_scope([C | Cs], Ar, Br, Compr, N, X) ->
    if
        is_integer(C, $0, $9) ->
            ipv6_addr_scope_dec(Cs, [X | Ar], Br, Compr, N+1, C - $0);
        true ->
            %% We ignore string scope id for now
            ipv6_addr_done(X, Ar, Br, Compr, N)
    end.

ipv6_addr_scope_dec([], Ar, Br, Compr, N, ScopeId) ->
    ipv6_addr_scope_done(ScopeId, Ar, Br, Compr, N);
ipv6_addr_scope_dec([C|Cs], Ar, Br, Compr, N, ScopeId) ->
    if
        is_integer(C, $0, $9) ->
            ScopeId_1 = ScopeId*10 + C - $0,
            if  is_integer(ScopeId_1, 0, 16#ffff) ->
                    ipv6_addr_scope_dec(Cs, Ar, Br, Compr, N, ScopeId_1);
                true ->
                    throw(error) % 16-bit overflow
            end;
       true ->
            %% Non-numerical <zone_id> - ignore it
            ipv6_addr_done(N, Ar, Br, Compr)
    end.

ipv6_addr_scope_done(ScopeId, Ar, Br, Compr, N) when is_integer(ScopeId) ->
    %% FreeBSD kernel style piggy-back the Scope ID into the second word
    %% of the address for link-local and site-local addresses
    %% which is always 0.
    case ipv6_addr_fill_zeros(N, Ar, Br, Compr) of
        [X1, 0 | Xs] when
              X1 =:= 16#fe80;
              X1 =:= 16#ff02 ->
            list_to_tuple([X1, ScopeId | Xs]);
        Xs when length(Xs) == 8 ->
            throw(error) % Inappropriate address for scope id
    end.

%% IPv4 suffix, first value in A
ipv6_addr_v4(Cs, Ar, Br, Compr, N, A) ->
    N =< 6 orelse throw(error), % Too many fields
    case list_pos($%, Cs) of
        0 ->
            %% No scope id suffix
            {X6, X7} = ipv6_addr_v4(Cs, A),
            ipv6_addr_done(X7, [X6 | Ar], Br, Compr, N+1);
        P ->
            %% Split into IPv4 suffix and scope id suffix
            {IPv4Tail, [$% | ScopeIdSuffix]} = lists:split(P-1, Cs),
            {X6, X7} = ipv6_addr_v4(IPv4Tail, A),
            ipv6_addr_scope(
              ScopeIdSuffix, [X6 | Ar], Br, Compr, N+1, X7)
    end.

ipv6_addr_v4(Cs, A) ->
    {A, B, C, D} = ipv4s_c2(Cs, A), % Parse the remaining 3 suffix fields
    X6 = (A bsl 8) bor B,
    X7 = (C bsl 8) bor D,
    {X6, X7}.

ipv6_addr_fill_zeros(N, Ar, [], false) ->
    if  N == 8 ->
            lists:reverse(Ar);
        N < 8 ->
            throw(error) % Too few fields
    end;
ipv6_addr_fill_zeros(N, Ar, Br, true) ->
    if  N == 8 ->
            lists:reverse(Br, lists:reverse(Ar));
        N < 8 ->
            %% Fill in the gap with zeros
            lists:reverse(Br, dup(8-N, 0, lists:reverse(Ar)))
    end.

%% Return the position of element X in list L,
%% or 0 if X does not match any element in L.
%% First position is 1.
%%
list_pos(X, L) -> list_pos(X, L, 1).
%%
list_pos(_, [],      _) -> 0;
list_pos(X, [X | _], N) -> N;
list_pos(X, [_ | L], N) when is_integer(N), N >= 1 ->
    list_pos(X, L, N+1).

%% Duplicate E N times onto head of L
dup(0, _, L) ->
    L;
dup(N, E, L) when is_integer(N), N >= 1 ->
    dup(N-1, E, [E|L]).



%% Convert IPv4 address to ascii
%% Convert IPv6 / IPV4 address to ascii (plain format)
ntoa({A,B,C,D}) when ?ip(A,B,C,D) ->
    integer_to_list(A) ++ "." ++ integer_to_list(B) ++ "." ++ 
	integer_to_list(C) ++ "." ++ integer_to_list(D);
%% ANY
ntoa({0,0,0,0,0,0,0,0}) -> "::";
%% LOOPBACK
ntoa({0,0,0,0,0,0,0,1}) -> "::1";
%% IPV4 ipv6 host address
ntoa({0,0,0,0,0,0,A,B}) when ?ip6(0,0,0,0,0,0,A,B) ->
    "::" ++ dig_to_dec(A) ++ "." ++ dig_to_dec(B);
%% IPV4 non ipv6 host address
ntoa({0,0,0,0,0,16#ffff=X,A,B}) when ?ip6(0,0,0,0,0,X,A,B) ->
    "::ffff:" ++ dig_to_dec(A) ++ "." ++ dig_to_dec(B);
ntoa({A,B,C,D,E,F,G,H}) when ?ip6(A,B,C,D,E,F,G,H) ->
    if
        A =:= 16#fe80, B =/= 0;
        A =:= 16#ff02, B =/= 0 ->
            %% Find longest sequence of zeros, at least 2,
            %% to replace with "::"
            ntoa([A,0,C,D,E,F,G,H], []) ++ "%" ++ integer_to_list(B);
        true ->
            %% Find longest sequence of zeros, at least 2,
            %% to replace with "::"
            ntoa([A,B,C,D,E,F,G,H], [])
    end;
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

%% Convert a integer to hex string (lowercase)
dig_to_hex(0) -> "0";
dig_to_hex(X) when is_integer(X), 0 < X ->
    dig_to_hex(X, "").
%%
dig_to_hex(0, Acc) -> Acc;
dig_to_hex(X, Acc) ->
    dig_to_hex(
      X bsr 4,
      [case X band 15 of
           D when D < 10 -> D + $0;
           D -> D - 10 + $a
       end|Acc]).

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

