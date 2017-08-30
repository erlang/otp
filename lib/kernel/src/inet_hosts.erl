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
-module(inet_hosts).

%% Implement gethostbyname gethostbyaddr for inet_hosts table

-export([gethostbyname/1, gethostbyname/2, gethostbyaddr/1]).

-include("inet.hrl").
-include("inet_int.hrl").

gethostbyname(Name) when is_list(Name) ->
    gethostbyname(Name,
		  case inet_db:res_option(inet6) of
		      true -> inet6;
		      false -> inet
		  end);
gethostbyname(Name) when is_atom(Name) ->
    gethostbyname(atom_to_list(Name));
gethostbyname(_) -> {error, formerr}.



gethostbyname(Name, Type) when is_list(Name), is_atom(Type) ->
    %% Byname has lowercased names while Byaddr keep the name casing.
    %% This is to be able to reconstruct the original /etc/hosts entry.
    N = inet_db:tolower(Name),
    case gethostbyname(N, Type, inet_hosts_byname, inet_hosts_byaddr) of
	false ->
	    case gethostbyname(N, Type,
			       inet_hosts_file_byname,
			       inet_hosts_file_byaddr) of
		false -> {error,nxdomain};
		Hostent -> {ok,Hostent}
	    end;
	Hostent -> {ok,Hostent}
    end;
gethostbyname(Name, Type) when is_atom(Name), is_atom(Type) ->
    gethostbyname(atom_to_list(Name), Type);
gethostbyname(_, _) -> {error, formerr}.

gethostbyname(Name, Type, Byname, Byaddr) ->
    inet_db:res_update_hosts(),
    case [I || [I] <- ets:match(Byname, {Name,Type,'$1'})] of
	[] -> false;
	[IP|_]=IPs ->
	    %% Use the primary IP address to generate aliases
	    [Nm|As] = [N || [N] <- ets:match(Byaddr,
					     {'$1',Type,IP})],
	    make_hostent(Nm, IPs, As, Type)
    end.




gethostbyaddr({A,B,C,D}=IP) when ?ip(A,B,C,D) ->
    gethostbyaddr(IP, inet);
%% ipv4  only ipv6 address
gethostbyaddr({0,0,0,0,0,16#ffff=F,G,H}) when ?ip6(0,0,0,0,0,F,G,H) ->
    gethostbyaddr({G bsr 8, G band 255, H bsr 8, H band 255});
gethostbyaddr({A,B,C,D,E,F,G,H}=IP) when ?ip6(A,B,C,D,E,F,G,H) ->
    gethostbyaddr(IP, inet6);
gethostbyaddr(Addr) when is_list(Addr) ->
    case inet_parse:address(Addr) of
	{ok,IP} -> gethostbyaddr(IP);
	_Error -> {error, formerr}
    end;
gethostbyaddr(Addr) when is_atom(Addr) ->
    gethostbyaddr(atom_to_list(Addr));
gethostbyaddr(_) -> {error, formerr}.



gethostbyaddr(IP, Type) ->
    case gethostbyaddr(IP, Type, inet_hosts_byaddr) of
	false ->
	    case gethostbyaddr(IP, Type, inet_hosts_file_byaddr) of
		false -> {error,nxdomain};
		Hostent -> {ok,Hostent}
	    end;
	Hostent -> {ok,Hostent}
    end.

gethostbyaddr(IP, Type, Byaddr) ->
    inet_db:res_update_hosts(),
    case [N || [N] <- ets:match(Byaddr, {'$1',Type,IP})] of
	[] -> false;
	[Nm|As] -> make_hostent(Nm, [IP], As, Type)
    end.



make_hostent(Name, Addrs, Aliases, inet) ->
    #hostent {
	      h_name = Name,
	      h_addrtype = inet,
	      h_length = 4,
	      h_addr_list = Addrs,
	      h_aliases = Aliases
	     };
make_hostent(Name, Addrs, Aliases, inet6) ->
    #hostent {
	      h_name = Name,
	      h_addrtype = inet6,
	      h_length = 16,
	      h_addr_list = Addrs,
	      h_aliases = Aliases
	     }.


