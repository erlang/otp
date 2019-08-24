%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(inet_config).

-include("inet_config.hrl").
-include("inet.hrl").

-import(lists, [foreach/2, member/2, reverse/1]).

%% Avoid warning for local function error/2 clashing with autoimported BIF.
-compile({no_auto_import,[error/2]}).
-export([init/0]).

-export([do_load_resolv/2]).

%%
%% Must be called after inet_db:start
%%
%% Order in which to load inet_db data:
%% 1. Hostname  (possibly derive domain and search)
%% 2. OS default  /etc/resolv.conf,  Windows registry etc
%%    a) Hosts database
%%    b) Resolver options 
%% 3. Config (kernel app)
%% 4. Root   (otp root)
%% 5. Home   (user inetrc)
%%
%%
-spec init() -> 'ok'.
init() ->
    set_hostname(),

    %% Note: In shortnames (or non-distributed) mode we don't need to know
    %% our own domain name. In longnames mode we do and we can't rely on 
    %% the user to provide it (by means of inetrc), so we need to look 
    %% for it ourselves.

    OsType = os:type(),
    do_load_resolv(OsType, erl_dist_mode()),

    case OsType of
	{unix,Type} ->
	    if Type =:= linux ->
		    %% It may be the case that the domain name was not set
		    %% because the hostname was short. But NOW we can look it
		    %% up and get the long name and the domain name from it.
		    
		    %% FIXME: The second call to set_hostname will insert
		    %% a duplicate entry in the search list.
		    
		    case inet_db:res_option(domain) of
			"" ->
			    case inet:gethostbyname(inet_db:gethostname()) of
				{ok,#hostent{h_name = []}} ->
				    ok;
				{ok,#hostent{h_name = HostName}} ->
				    set_hostname({ok,HostName});
				_ ->
				    ok
			    end;
			_ ->
			    ok
		    end;
	       true -> ok
	    end,    
	    add_dns_lookup(inet_db:res_option(lookup));
	_ ->
	    ok
    end,

    %% Read inetrc file, if it exists.
    {RcFile,CfgFiles,CfgList} = read_rc(),

    %% Possibly read config files or system registry
    lists:foreach(fun({file,hosts,File}) ->
			  load_hosts(File, unix);
		     ({file,Func,File}) ->
			  load_resolv(File, Func);
		     ({registry,win32}) ->
			  case OsType of
			      {win32,WinType} ->
				  win32_load_from_registry(WinType);
			      _ ->
				  error("cannot read win32 system registry~n", [])
			  end
		  end, CfgFiles),

    %% Add inetrc config entries
    case inet_db:add_rc_list(CfgList) of
	ok -> ok;
	_  -> error("syntax error in ~ts~n", [RcFile])
    end,

    %% Set up a resolver configuration file for inet_res,
    %% unless that already has been done
    case OsType of
	{unix,_} ->
	    %% The Etc variable enables us to run tests with other 
	    %% configuration files than the normal ones 
	    Etc = os:getenv("ERL_INET_ETC_DIR", ?DEFAULT_ETC),
	    case inet_db:res_option(resolv_conf) of
		undefined ->
		    inet_db:res_option(
		      resolv_conf_name,
		      filename:join(Etc, ?DEFAULT_RESOLV));
		_ -> ok
	    end,
	    case inet_db:res_option(hosts_file) of
		undefined ->
		    inet_db:res_option(
		      hosts_file_name,
		      filename:join(Etc, ?DEFAULT_HOSTS));
		_ -> ok
	    end;
	_ -> ok
    end.



erl_dist_mode() ->
    case init:get_argument(sname) of
	{ok,[[_SName]]} -> shortnames;
	_ ->
	    case init:get_argument(name) of
		{ok,[[_Name]]} -> longnames;
		_ -> nonames
	    end
    end.

do_load_resolv({unix,Type}, longnames) ->
    %% The Etc variable enables us to run tests with other 
    %% configuration files than the normal ones 
    Etc = os:getenv("ERL_INET_ETC_DIR", ?DEFAULT_ETC),
    load_resolv(filename:join(Etc, ?DEFAULT_RESOLV), resolv),
    case Type of
	freebsd ->	    %% we may have to check version (2.2.2)
	    load_resolv(filename:join(Etc,"host.conf"), host_conf_freebsd);
	'bsd/os' ->
	    load_resolv(filename:join(Etc,"irs.conf"), host_conf_bsdos);
	sunos ->
	    case os:version() of
		{Major,_,_} when Major >= 5 ->
		    load_resolv(filename:join(Etc,"nsswitch.conf"), 
				nsswitch_conf);
		_ -> 
		    ok
	    end;
	netbsd ->
	    case os:version() of
		{Major,Minor,_} when Major >= 1, Minor >= 4 ->
		    load_resolv(filename:join(Etc,"nsswitch.conf"), 
				nsswitch_conf);
		_ ->
		    ok
	    end;		
	linux ->
	    case load_resolv(filename:join(Etc,"host.conf"),
			     host_conf_linux) of
		ok ->
		    ok;
		_ ->
		    load_resolv(filename:join(Etc,"nsswitch.conf"), 
				nsswitch_conf)
	    end;
	_ ->
	    ok
    end,
    inet_db:set_lookup([native]);

do_load_resolv({win32,Type}, longnames) ->	
    win32_load_from_registry(Type),
    inet_db:set_lookup([native]);

do_load_resolv(_, _) ->
    inet_db:set_lookup([native]).

add_dns_lookup(L) ->
    case lists:member(dns,L) of
	true -> ok;
	_ ->
	    case application:get_env(kernel,inet_dns_when_nis) of
		{ok,true} -> 
		    add_dns_lookup(L,[]);
		_ ->
		    ok
	    end
    end.

add_dns_lookup([yp|T],Acc) ->
    add_dns_lookup(T,[yp,dns|Acc]);
add_dns_lookup([H|T],Acc) ->
    add_dns_lookup(T,[H|Acc]);
add_dns_lookup([],Acc) ->
    inet_db:set_lookup(reverse(Acc)).

%%
%% Set the hostname (SHORT)
%% If hostname is long use the suffix as default domain
%% and initalize the search option with the parts of domain
%%
set_hostname() ->
    case inet_udp:open(0,[]) of
	{ok,U} ->
	    Res = inet:gethostname(U),
	    inet_udp:close(U),
	    set_hostname(Res);
	_ ->
	    set_hostname({ok, []})
    end.

set_hostname({ok,Name}) when length(Name) > 0 ->
    {Host, Domain} = lists:splitwith(fun($.) -> false;
					(_)  -> true
				     end, Name),
    inet_db:set_hostname(Host),
    set_search_dom(Domain);
set_hostname({ok,[]}) ->
    inet_db:set_hostname("nohost"),
    set_search_dom("nodomain").

set_search_dom([$.|Domain]) ->
    %% leading . not removed by dropwhile above.
    inet_db:set_domain(Domain),
    inet_db:ins_search(Domain),
    ok;
set_search_dom([]) ->
    ok;
set_search_dom(Domain) ->
    inet_db:set_domain(Domain),
    inet_db:ins_search(Domain),
    ok.

%%
%% Load resolver data
%%
load_resolv(File, Func) ->
    case get_file(File) of
	{ok,Bin} ->
            case inet_parse:Func(File, {chars, Bin}) of
		{ok, Ls} ->
		    inet_db:add_rc_list(Ls);
		{error, Reason} ->
		    error("parse error in file ~ts: ~p", [File, Reason])
	    end;
	Error ->
	    warning("file not found ~ts: ~p~n", [File, Error])
    end.

%%
%% Load a UNIX hosts file
%%
load_hosts(File,Os) ->
    case get_file(File) of
	{ok,Bin} ->
	    case inet_parse:hosts(File,{chars,Bin}) of
		{ok, Ls} ->
		    foreach(
		      fun({IP, Name, Aliases}) -> 
			      inet_db:add_host(IP, [Name|Aliases]) end,
		      Ls);
		{error, Reason} ->
		    error("parse error in file ~ts: ~p", [File, Reason])
	    end;
	Error ->
	    case Os of
		unix ->
		    error("file not found ~ts: ~p~n", [File, Error]);
		_ -> 
		    %% for windows or nt the hosts file is not always there
		    %% and we don't require it
		    ok
	    end
    end.

%%
%% Load resolver data from Windows registry
%%
win32_load_from_registry(Type) ->
    %% The TcpReg variable enables us to run tests with other registry configurations than
    %% the normal ones 
    TcpReg = os:getenv("ERL_INET_ETC_DIR", ""),
    {ok, Reg} = win32reg:open([read]),
    {TcpIp,HFileKey} =
    case Type of
	nt ->
	    case TcpReg of
		[] -> 
		    {"\\hklm\\system\\CurrentControlSet\\Services\\TcpIp\\Parameters",
		     "DataBasePath" };
		Other ->
		    {Other,"DataBasePath"}
	    end;
	windows ->
	    case TcpReg of 
		[] ->
		    {"\\hklm\\system\\CurrentControlSet\\Services\\VxD\\MSTCP",
		     "LMHostFile" };
		Other ->
		    {Other,"LMHostFile"}
	    end
    end,
    Result = 
	case win32reg:change_key(Reg,TcpIp) of
	    ok ->
		win32_load1(Reg,Type,HFileKey);
	    {error, _Reason} ->
		error("Failed to locate TCP/IP parameters (is TCP/IP installed)?",
		      [])
	end,
    win32reg:close(Reg),
    Result.

win32_load1(Reg,Type,HFileKey) ->
    Names = [HFileKey, "Domain", "DhcpDomain", 
	     "EnableDNS", "NameServer", "SearchList"],
    case win32_get_strings(Reg, Names) of
	[DBPath0, Domain, DhcpDomain, 
	 _EnableDNS, NameServers0, Search] ->
	    inet_db:set_domain(
	      case Domain of "" -> DhcpDomain; _ -> Domain end),
	    NameServers = win32_split_line(NameServers0,Type),
	    AddNs = fun(Addr) ->
			    case inet_parse:address(Addr) of
				{ok, Address} ->
				    inet_db:add_ns(Address);
				{error, _} ->
				    error("Bad TCP/IP address in registry", [])
			    end
		    end,
	    foreach(AddNs, NameServers),
	    Searches0 = win32_split_line(Search,Type),
	    Searches = case member(Domain, Searches0) of
			   true  -> Searches0;
			   false -> [Domain|Searches0]
		       end,
	    foreach(fun(D) -> inet_db:add_search(D) end, Searches),
	    if Type =:= nt ->
		    DBPath = win32reg:expand(DBPath0),
		    load_hosts(filename:join(DBPath, "hosts"),nt);
		Type =:= windows ->
		    load_hosts(filename:join(DBPath0,""),windows)
	    end,
%% Maybe activate this later as an optimization
%% For now we use native always as the SAFE way
%%	    case NameServers of
%%		[] -> inet_db:set_lookup([native, file]);
%%		_  -> inet_db:set_lookup([dns, file, native])
%%	    end;
	    true;
	{error, _Reason} ->
	    error("Failed to read TCP/IP parameters from registry", [])
    end.

win32_split_line(Line,nt) -> inet_parse:split_line(Line);
win32_split_line(Line,windows) -> string:lexemes(Line, ",").

win32_get_strings(Reg, Names) ->
    win32_get_strings(Reg, Names, []).

win32_get_strings(Reg, [Name|Rest], Result) ->
    case win32reg:value(Reg, Name) of
	{ok, Value} when is_list(Value) ->
	    win32_get_strings(Reg, Rest, [Value|Result]);
	{ok, _NotString} ->
	    {error, not_string};
	{error, _Reason} ->
	    win32_get_strings(Reg, Rest, [""|Result])
    end;
win32_get_strings(_, [], Result) ->
    lists:reverse(Result).

read_rc() ->
    {RcFile,CfgList} = read_inetrc(),
    case extract_cfg_files(CfgList, [], []) of
	{CfgFiles,CfgList1} ->
	    {RcFile,CfgFiles,CfgList1};
	error ->
	    {error,[],[]}
    end.



extract_cfg_files([E = {file,Type,_File} | Es], CfgFiles, CfgList) ->
    extract_cfg_files1(Type, E, Es, CfgFiles, CfgList);
extract_cfg_files([E = {registry,Type} | Es], CfgFiles, CfgList) ->
    extract_cfg_files1(Type, E, Es, CfgFiles, CfgList);
extract_cfg_files([E | Es], CfgFiles, CfgList) ->
    extract_cfg_files(Es, CfgFiles, [E | CfgList]);
extract_cfg_files([], CfgFiles, CfgList) ->
    {reverse(CfgFiles),reverse(CfgList)}.

extract_cfg_files1(Type, E, Es, CfgFiles, CfgList) ->
    case valid_type(Type) of
	true ->
	    extract_cfg_files(Es, [E | CfgFiles], CfgList);
	false ->
	    error("invalid config value ~w in inetrc~n", [Type]),
	    error
    end.

valid_type(resolv) ->            true;
valid_type(host_conf_freebsd) -> true;
valid_type(host_conf_bsdos) ->   true;
valid_type(host_conf_linux) ->   true;
valid_type(nsswitch_conf) ->     true;
valid_type(hosts) ->             true;
valid_type(win32) ->             true;
valid_type(_) ->                 false.

read_inetrc() ->
   case application:get_env(inetrc) of
       {ok,File} ->
	   try_get_rc(File);
       _ ->
	   case os:getenv("ERL_INETRC") of
	       false ->
		   {nofile,[]};
	       File ->
		   try_get_rc(File)
	   end
   end.

try_get_rc(File) ->
    case get_rc(File) of
	error -> {nofile,[]};
	Ls ->    {File,Ls}
    end.    

get_rc(File) ->
    case get_file(File) of
	{ok,Bin} ->
	    case parse_inetrc(Bin) of
		{ok,Ls} -> 
		    Ls;
		_Error -> 
		    error("parse error in ~ts~n", [File]),
		    error
	    end;
	_Error -> 
	    error("file ~ts not found~n", [File]),
	    error
    end.

%% XXX Check if we really need to prim load the stuff
get_file(File) ->
    case erl_prim_loader:get_file(File) of
	{ok,Bin,_} -> {ok,Bin};
	Error -> Error
    end.

error(Fmt, Args) ->
    error_logger:error_msg("inet_config: " ++ Fmt, Args).

warning(Fmt, Args) ->
    case application:get_env(kernel,inet_warnings) of
	%{ok,silent} -> ok;
	{ok,on} -> 
	    error_logger:info_msg("inet_config:" ++ Fmt, Args);
	_ ->
	    ok
    end.

%% 
%% Parse inetrc, i.e. make a binary of a term list.
%% The extra newline is to let the user ignore the whitespace !!!
%% Ignore leading whitespace before a token (due to bug in erl_scan) !
%% 
parse_inetrc(Bin) ->
    case file_binary_to_list(Bin) of
        {ok, String} ->
            parse_inetrc(String ++ "\n", 1, []);
        error ->
            {error, 'bad_encoding'}
    end.

parse_inetrc_skip_line([], _Line, Ack) ->
    {ok, reverse(Ack)};
parse_inetrc_skip_line([$\n|Str], Line, Ack) ->
    parse_inetrc(Str, Line+1, Ack);
parse_inetrc_skip_line([_|Str], Line, Ack) ->
    parse_inetrc_skip_line(Str, Line, Ack).

parse_inetrc([$%|Str], Line, Ack) ->
    parse_inetrc_skip_line(Str, Line, Ack);
parse_inetrc([$\s|Str], Line, Ack) ->
    parse_inetrc(Str, Line, Ack);
parse_inetrc([$\n |Str], Line, Ack) ->
    parse_inetrc(Str, Line+1, Ack);
parse_inetrc([$\t|Str], Line, Ack) ->
    parse_inetrc(Str, Line, Ack);
parse_inetrc([], _, Ack) ->
    {ok, reverse(Ack)};


%% The clauses above are here due to a bug in erl_scan (OTP-1449).

parse_inetrc(Str, Line, Ack) ->
    case erl_scan:tokens([], Str, Line) of
	{done, {ok, Tokens, EndLine}, MoreChars} ->
	    case erl_parse:parse_term(Tokens) of
		{ok, Term} ->
		    parse_inetrc(MoreChars, EndLine, [Term|Ack]);
		Error ->
		    {error, {'parse_inetrc', Error}}
	    end;
	{done, {eof, _}, _} ->
	    {ok, reverse(Ack)};
	{done, Error, _} ->
	    {error, {'scan_inetrc', Error}};
	{more, _} -> %% Bug in erl_scan !!
	    {error, {'scan_inetrc', {eof, Line}}}
    end.

file_binary_to_list(Bin) ->
    Enc = case epp:read_encoding_from_binary(Bin) of
              none -> epp:default_encoding();
              Encoding -> Encoding
          end,
    case catch unicode:characters_to_list(Bin, Enc) of
        String when is_list(String) ->
            {ok, String};
        _ ->
            error
    end.

