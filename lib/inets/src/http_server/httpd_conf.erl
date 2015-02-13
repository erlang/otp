%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2015. All Rights Reserved.
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
%%
-module(httpd_conf).

%% EWSAPI 
-export([is_directory/1, is_file/1, make_integer/1, clean/1, 
	 custom_clean/3, check_enum/2]).

%% Application internal API
-export([load/1, load/2, load_mime_types/1, store/1, store/2,
	 remove/1, remove_all/1, get_config/2, get_config/3,
	 lookup_socket_type/1, 
	 lookup/2, lookup/3, lookup/4, 
	 validate_properties/1]).

-define(VMODULE,"CONF").
-include("httpd_internal.hrl").
-include("httpd.hrl").
-include_lib("inets/src/http_lib/http_internal.hrl").


%%%=========================================================================
%%%  EWSAPI
%%%=========================================================================
%%-------------------------------------------------------------------------
%%  is_directory(FilePath) -> Result
%%	FilePath = string()
%%      Result = {ok,Directory} | {error,Reason}
%%      Directory = string()
%%      Reason = string() | enoent | eacces | enotdir | FileInfo
%%      FileInfo = File info record
%%
%% Description: Checks if FilePath is a directory in which case it is
%% returned. 
%%-------------------------------------------------------------------------
is_directory(Directory) ->
    case file:read_file_info(Directory) of
	{ok,FileInfo} ->
	    #file_info{type = Type, access = Access} = FileInfo,
	    is_directory(Type,Access,FileInfo,Directory);
	{error,Reason} ->
	    {error,Reason}
    end.
is_directory(directory,read,_FileInfo,Directory) ->
    {ok,Directory};
is_directory(directory,read_write,_FileInfo,Directory) ->
    {ok,Directory};
is_directory(_Type,_Access,FileInfo,_Directory) ->
    {error,FileInfo}.


%%-------------------------------------------------------------------------
%% is_file(FilePath) -> Result
%%	FilePath = string()
%%      Result = {ok,File} | {error,Reason}
%%      File = string()
%%      Reason = string() | enoent | eacces | enotdir | FileInfo
%%      FileInfo = File info record
%%
%% Description: Checks if FilePath is a regular file in which case it
%% is returned.
%%-------------------------------------------------------------------------
is_file(File) ->
    case file:read_file_info(File) of
	{ok,FileInfo} ->
	    #file_info{type = Type, access = Access} = FileInfo,
	    is_file(Type,Access,FileInfo,File);
	{error,Reason} ->
	    {error,Reason}
    end.
is_file(regular,read,_FileInfo,File) ->
    {ok,File};
is_file(regular,read_write,_FileInfo,File) ->
    {ok,File};
is_file(_Type,_Access,FileInfo,_File) ->
    {error,FileInfo}.


%%-------------------------------------------------------------------------
%% make_integer(String) -> Result
%% String = string()
%% Result = {ok,integer()} | {error,nomatch}
%%
%% Description: make_integer/1 returns an integer representation of String. 
%%-------------------------------------------------------------------------
make_integer(String) ->
    case inets_regexp:match(clean(String),"[0-9]+") of
	{match, _, _} ->
	    {ok, list_to_integer(clean(String))};
	nomatch ->
	    {error, nomatch}
    end.


%%-------------------------------------------------------------------------
%% clean(String) -> Stripped
%% String = Stripped = string()
%%
%% Description:clean/1 removes leading and/or trailing white spaces
%% from String.
%%-------------------------------------------------------------------------
clean(String) ->
    {ok,CleanedString,_} = 
	inets_regexp:gsub(String, "^[ \t\n\r\f]*|[ \t\n\r\f]*\$",""),
    CleanedString.


%%-------------------------------------------------------------------------
%% custom_clean(String,Before,After) -> Stripped
%% Before = After = regexp()
%% String = Stripped = string()
%%
%% Description: custom_clean/3 removes leading and/or trailing white
%% spaces and custom characters from String. 
%%-------------------------------------------------------------------------
custom_clean(String,MoreBefore,MoreAfter) ->
    {ok,CleanedString,_} = inets_regexp:gsub(String,"^[ \t\n\r\f"++MoreBefore++
				       "]*|[ \t\n\r\f"++MoreAfter++"]*\$",""),
    CleanedString.


%%-------------------------------------------------------------------------
%% check_enum(EnumString,ValidEnumStrings) -> Result
%%	EnumString = string()
%%      ValidEnumStrings = [string()]
%%      Result = {ok,atom()} | {error,not_valid}
%%
%% Description: check_enum/2 checks if EnumString is a valid
%% enumeration of ValidEnumStrings in which case it is returned as an
%% atom.
%%-------------------------------------------------------------------------
check_enum(_Enum,[]) ->
    {error, not_valid};
check_enum(Enum,[Enum|_Rest]) ->
    {ok, list_to_atom(Enum)};
check_enum(Enum, [_NotValid|Rest]) ->
    check_enum(Enum, Rest).


%%%=========================================================================
%%%  Application internal API
%%%=========================================================================
%% The configuration data is handled in three (3) phases:
%% 1. Parse the config file and put all directives into a key-vale
%%    tuple list (load/1). 
%% 2. Traverse the key-value tuple list store it into an ETS table.
%%    Directives depending on other directives are taken care of here
%%    (store/1).
%% 3. Traverse the ETS table and do a complete clean-up (remove/1).

%% Phase 1: Load
load(ConfigFile) ->
    ?hdrv("load config", [{config_file, ConfigFile}]),
    case read_config_file(ConfigFile) of
	{ok, Config} ->
	    ?hdrt("config read", []),
	    case bootstrap(Config) of
		{error, Reason} ->
		    ?hdri("bootstrap failed", [{reason, Reason}]),
		    {error, Reason};
		{ok, Modules} ->
                    ?hdrd("config bootstrapped", [{modules, Modules}]),
		    load_config(Config, lists:append(Modules, [?MODULE]))
	    end;
	{error, Reason} ->
	    ?hdri("failed reading config file", [{reason, Reason}]),
	    {error, ?NICE("Error while reading config file: "++Reason)}
    end.

load(eof, []) ->
    eof;

load("MaxHeaderSize " ++ MaxHeaderSize, []) ->
    case make_integer(MaxHeaderSize) of
        {ok, Integer} ->
            {ok, [], {max_header_size,Integer}};
        {error, _} ->
            {error, ?NICE(clean(MaxHeaderSize)++
                          " is an invalid number of MaxHeaderSize")}
    end;

load("MaxURISize " ++ MaxHeaderSize, []) ->
    case make_integer(MaxHeaderSize) of
        {ok, Integer} ->
            {ok, [], {max_uri_size, Integer}};
        {error, _} ->
            {error, ?NICE(clean(MaxHeaderSize)++
                          " is an invalid number of MaxHeaderSize")}
    end;

load("MaxContentLength " ++ Max, []) ->
    case make_integer(Max) of
        {ok, Integer} ->
            {ok, [], {max_content_length, Integer}};
        {error, _} ->
            {error, ?NICE(clean(Max) ++
			      " is an invalid number of MaxContentLength")}
    end;

load("ServerName " ++ ServerName, []) ->
    {ok,[], {server_name, clean(ServerName)}};

load("ServerTokens " ++ ServerTokens, []) ->
    %% These are the valid *plain* server tokens: 
    %%     sprod, major, minor, minimum, os, full
    %% It can also be a "private" server token: private:<any string>
    case string:tokens(ServerTokens, [$:]) of
	["private", Private] ->
	    {ok,[], {server_tokens, clean(Private)}};
	[TokStr] ->
	    Tok = list_to_atom(clean(TokStr)),
	    case lists:member(Tok, [prod, major, minor, minimum, os, full]) of
		true ->
		    {ok,[], {server_tokens, Tok}};
		false ->
		    {error, ?NICE(clean(ServerTokens) ++ 
				  " is an invalid ServerTokens")}
	    end;
	_ ->
	    {error, ?NICE(clean(ServerTokens) ++ " is an invalid ServerTokens")}
    end;

load("SocketType " ++ SocketType, []) ->
    %% ssl is the same as HTTP_DEFAULT_SSL_KIND
    %% essl is the pure Erlang-based ssl (the "new" ssl)
    case check_enum(clean(SocketType), ["ssl", "essl", "ip_comm"]) of
	{ok, ValidSocketType} ->
	    {ok, [], {socket_type, ValidSocketType}};
	{error,_} ->
	    {error, ?NICE(clean(SocketType) ++ " is an invalid SocketType")}
    end;

load("Port " ++ Port, []) ->
    case make_integer(Port) of
	{ok, Integer} ->
	    {ok, [], {port, Integer}};
	{error, _} ->
	    {error, ?NICE(clean(Port)++" is an invalid Port")}
    end;

load("BindAddress " ++ Address0, []) ->
    %% If an ipv6 address is provided in URL-syntax strip the
    %% url specific part e.i. "[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]"
    %% -> "FEDC:BA98:7654:3210:FEDC:BA98:7654:3210"

    try
	begin
	    ?hdrv("load BindAddress", [{address0, Address0}]),
	    {Address, IpFamily} = 
		case string:tokens(Address0, [$|]) of
		    [Address1] ->
			?hdrv("load BindAddress", [{address1, Address1}]),
			{clean_address(Address1), inet6fb4};
		    [Address1, IpFamilyStr] ->
			?hdrv("load BindAddress", 
			      [{address1, Address1}, 
			       {ipfamily_str, IpFamilyStr}]),
			{clean_address(Address1), make_ipfamily(IpFamilyStr)};
		    _Bad ->
			?hdrv("load BindAddress - bad address", 
			      [{bad_address, _Bad}]),
			throw({error, {bad_bind_address, Address0}})
		end,

	    ?hdrv("load BindAddress - address and ipfamily separated", 
		  [{address, Address}, {ipfamily, IpFamily}]),

	    case Address of
		"*" ->
		    {ok, [], [{bind_address, any}, {ipfamily, IpFamily}]};
		_ ->
		    case httpd_util:ip_address(Address, IpFamily) of
			{ok, IPAddr} ->
			    ?hdrv("load BindAddress - checked", 
				  [{ip_address, IPAddr}]),
			    Entries = [{bind_address, IPAddr}, 
				       {ipfamily,     IpFamily}], 
			    {ok, [], Entries};
			{error, _} ->
			    {error, ?NICE(Address ++ " is an invalid address")}
		    end
	    end
	end
    catch
	throw:{error, {bad_bind_address, _}} ->
	    ?hdrv("load BindAddress - bad bind address", []),
	    {error, ?NICE(Address0 ++ " is an invalid address")};
	throw:{error, {bad_ipfamily, _}} ->
	    ?hdrv("load BindAddress - bad ipfamily", []),
	    {error, ?NICE(Address0 ++ " has an invalid ipfamily")}
    end;

load("KeepAlive " ++ OnorOff, []) ->
    case list_to_atom(clean(OnorOff)) of
	off ->
	    {ok, [], {keep_alive, false}};
	_ ->
	    {ok, [], {keep_alive, true}}
    end;

load("MaxKeepAliveRequests " ++  MaxRequests, []) ->
    case make_integer(MaxRequests) of
	{ok, Integer} ->
	    {ok, [], {max_keep_alive_request, Integer}};
	{error, _} ->
	    {error, ?NICE(clean(MaxRequests) ++
			  " is an invalid MaxKeepAliveRequests")}
    end;

%% This clause is kept for backwards compatibility
load("MaxKeepAliveRequest " ++  MaxRequests, []) ->
    case make_integer(MaxRequests) of
	{ok, Integer} ->
	    {ok, [], {max_keep_alive_request, Integer}};
	{error, _} ->
	    {error, ?NICE(clean(MaxRequests) ++
			  " is an invalid MaxKeepAliveRequest")}
    end;

load("KeepAliveTimeout " ++ Timeout, []) ->
    case make_integer(Timeout) of
	{ok, Integer} ->
	    {ok, [], {keep_alive_timeout, Integer}};
	{error, _} ->
	    {error, ?NICE(clean(Timeout)++" is an invalid KeepAliveTimeout")}
    end;

load("Modules " ++ Modules, []) ->
    {ok, ModuleList} = inets_regexp:split(Modules," "),
    {ok, [], {modules,[list_to_atom(X) || X <- ModuleList]}};

load("ServerAdmin " ++ ServerAdmin, []) ->
    {ok, [], {server_admin,clean(ServerAdmin)}};

load("ServerRoot " ++ ServerRoot, []) ->
    case is_directory(clean(ServerRoot)) of
	{ok, Directory} ->
	    {ok, [], [{server_root,string:strip(Directory,right,$/)}]};
	{error, _} ->
	    {error, ?NICE(clean(ServerRoot)++" is an invalid ServerRoot")}
    end;

load("MimeTypes " ++ MimeTypes, []) ->
    case load_mime_types(clean(MimeTypes)) of
	{ok, MimeTypesList} ->
	    {ok, [], [{mime_types, MimeTypesList}]};
	{error, Reason} ->
	    {error, Reason}
    end;

load("MaxClients " ++ MaxClients, []) ->
    case make_integer(MaxClients) of
	{ok, Integer} ->
	    {ok, [], {max_clients,Integer}};
	{error, _} ->
	    {error, ?NICE(clean(MaxClients) ++
			  " is an invalid number of MaxClients")}
    end;
load("DocumentRoot " ++ DocumentRoot,[]) ->
    case is_directory(clean(DocumentRoot)) of
	{ok, Directory} ->
	    {ok, [], {document_root,string:strip(Directory,right,$/)}};
	{error, _} ->
	    {error, ?NICE(clean(DocumentRoot)++" is an invalid DocumentRoot")}
    end;
load("DefaultType " ++ DefaultType, []) ->
    {ok, [], {default_type,clean(DefaultType)}};
load("SSLCertificateFile " ++ SSLCertificateFile, []) ->
    case is_file(clean(SSLCertificateFile)) of
	{ok, File} ->
	    {ok, [], {ssl_certificate_file,File}};
    {error, _} ->
	    {error, ?NICE(clean(SSLCertificateFile)++
			  " is an invalid SSLCertificateFile")}
    end;
load("SSLLogLevel " ++ SSLLogAlert, []) ->
    case SSLLogAlert of
	"none" ->
	    {ok, [], {ssl_log_alert, false}};
	_ ->
	    {ok, [], {ssl_log_alert, true}}
    end;
load("SSLCertificateKeyFile " ++ SSLCertificateKeyFile, []) ->
    case is_file(clean(SSLCertificateKeyFile)) of
	{ok, File} ->
	    {ok, [], {ssl_certificate_key_file,File}};
	{error, _} ->
	    {error, ?NICE(clean(SSLCertificateKeyFile)++
			  " is an invalid SSLCertificateKeyFile")}
    end;
load("SSLVerifyClient " ++ SSLVerifyClient, []) ->
    case make_integer(clean(SSLVerifyClient)) of
	{ok, Integer} when (Integer >=0) andalso (Integer =< 2) ->
	    {ok, [], {ssl_verify_client,Integer}};
	{ok, _Integer} ->
	    {error,?NICE(clean(SSLVerifyClient) ++
			 " is an invalid SSLVerifyClient")};
	{error, nomatch} ->
	    {error,?NICE(clean(SSLVerifyClient) ++ 
			 " is an invalid SSLVerifyClient")}
    end;
load("SSLVerifyDepth " ++ SSLVerifyDepth, []) ->
    case make_integer(clean(SSLVerifyDepth)) of
	{ok, Integer} when Integer > 0 ->
	    {ok, [], {ssl_verify_client_depth,Integer}};
	{ok, _Integer} ->
	    {error,?NICE(clean(SSLVerifyDepth) ++
			 " is an invalid SSLVerifyDepth")};
	{error, nomatch} ->
	    {error,?NICE(clean(SSLVerifyDepth) ++
			 " is an invalid SSLVerifyDepth")}
    end;
load("SSLCiphers " ++ SSLCiphers, []) ->
    {ok, [], {ssl_ciphers, clean(SSLCiphers)}};
load("SSLCACertificateFile " ++ SSLCACertificateFile, []) ->
    case is_file(clean(SSLCACertificateFile)) of
	{ok, File} ->
	    {ok, [], {ssl_ca_certificate_file,File}};
	{error, _} ->
	    {error, ?NICE(clean(SSLCACertificateFile)++
			  " is an invalid SSLCACertificateFile")}
    end;
load("SSLPasswordCallbackModule " ++ SSLPasswordCallbackModule, []) ->
    {ok, [], {ssl_password_callback_module,
	      list_to_atom(clean(SSLPasswordCallbackModule))}};
load("SSLPasswordCallbackFunction " ++ SSLPasswordCallbackFunction, []) ->
    {ok, [], {ssl_password_callback_function,
	      list_to_atom(clean(SSLPasswordCallbackFunction))}};
load("SSLPasswordCallbackArguments " ++ SSLPasswordCallbackArguments, []) ->
    {ok, [], {ssl_password_callback_arguments, 
	                         SSLPasswordCallbackArguments}};
load("DisableChunkedTransferEncodingSend " ++ TrueOrFalse, []) ->
    case list_to_atom(clean(TrueOrFalse)) of
	true ->
	    {ok, [], {disable_chunked_transfer_encoding_send, true}};
	_ ->
	    {ok, [], {disable_chunked_transfer_encoding_send, false}}
    end;
load("LogFormat " ++ LogFormat, []) ->
    {ok,[],{log_format, list_to_atom(httpd_conf:clean(LogFormat))}};
load("ErrorLogFormat " ++ LogFormat, []) ->
    {ok,[],{error_log_format, list_to_atom(httpd_conf:clean(LogFormat))}}.


clean_address(Addr) ->
    string:strip(string:strip(clean(Addr), left, $[), right, $]).


make_ipfamily(IpFamilyStr) ->
    IpFamily = list_to_atom(IpFamilyStr),
    case lists:member(IpFamily, [inet, inet6, inet6fb4]) of
	true ->
	    IpFamily;
	false ->
	    throw({error, {bad_ipfamily, IpFamilyStr}})
    end.


%%
%% load_mime_types/1 -> {ok, MimeTypes} | {error, Reason}
%%
load_mime_types(MimeTypesFile) ->
    case file:open(MimeTypesFile, [read]) of
	{ok, Stream} ->
	    parse_mime_types(Stream, []);
	{error, _} ->
	    {error, ?NICE("Can't open " ++ MimeTypesFile)}
    end.


validate_properties(Properties) ->
    %% First, check that all mandatory properties are present
    case mandatory_properties(Properties) of
	ok -> 
	    %% Second, check that property dependency are ok
	    {ok, check_minimum_bytes_per_second(validate_properties2(Properties))};
	Error ->
	    throw(Error)
    end.

%% This function is used to validate inter-property dependencies.
%% That is, if property A depends on property B.
%% The only sunch preperty at this time is bind_address that depends 
%% on ipfamily.
validate_properties2(Properties) ->
    case proplists:get_value(bind_address, Properties) of
	undefined ->
	    case proplists:get_value(sock_type, Properties, ip_comm) of
		ip_comm ->
		    case proplists:get_value(ipfamily, Properties) of
			undefined ->
			    [{bind_address, any}, 
			     {ipfamily, inet6fb4} | Properties];
			_ ->
			    [{bind_address, any} | Properties]
		    end;
		_ ->
		    [{bind_address, any} | Properties]
	    end;
	any ->
	    Properties;
	Address0 ->
	    IpFamily = proplists:get_value(ipfamily, Properties, inet6fb4),
	    case httpd_util:ip_address(Address0, IpFamily) of
		{ok, Address} ->
		    Properties1 = proplists:delete(bind_address, Properties),
		    [{bind_address, Address} | Properties1];
		{error, Reason} ->
		    Error = {error, 
			     {failed_determine_ip_address, 
			      Address0, IpFamily, Reason}}, 
		    throw(Error)
	    end
    end.
check_minimum_bytes_per_second(Properties) ->
    case proplists:get_value(minimum_bytes_per_second, Properties, false) of
	false ->
	    Properties;
	Nr ->
	    case is_integer(Nr) of
		false ->
		    throw({error, {minimum_bytes_per_second, is_not_integer}});
		_ ->
		    Properties
	    end
    end.
mandatory_properties(ConfigList) ->
    a_must(ConfigList, [server_name, port, server_root, document_root]).

a_must(_ConfigList, []) ->
    ok;
a_must(ConfigList, [Prop | Rest]) ->
    case proplists:get_value(Prop, ConfigList) of
	undefined ->
	    {error, {missing_property, Prop}};
	_ ->
	    a_must(ConfigList, Rest)
    end.


validate_config_params([]) ->
    ok;
validate_config_params([{max_header_size, Value} | Rest]) 
  when is_integer(Value) andalso (Value > 0) ->
    validate_config_params(Rest);
validate_config_params([{max_header_size, Value} | _]) ->
    throw({max_header_size, Value});

validate_config_params([{max_body_size, Value} | Rest]) 
  when is_integer(Value) andalso (Value > 0) ->
    validate_config_params(Rest);
validate_config_params([{max_body_size, Value} | _]) -> 
    throw({max_body_size, Value});

validate_config_params([{max_content_length, Value} | Rest]) 
  when is_integer(Value) andalso (Value > 0) ->
    validate_config_params(Rest);
validate_config_params([{max_content_length, Value} | _]) -> 
    throw({max_content_length, Value});

validate_config_params([{server_name, Value} | Rest])  
  when is_list(Value) ->
    validate_config_params(Rest);
validate_config_params([{server_name, Value} | _]) ->
    throw({server_name, Value});

validate_config_params([{server_tokens, Value} | Rest])  
  when is_atom(Value) ->
    case lists:member(Value, plain_server_tokens()) of
	true ->
	    validate_config_params(Rest);
	false ->
	    throw({server_tokens, Value})
    end;
validate_config_params([{server_tokens, {private, Value}} | Rest])  
  when is_list(Value) ->
    validate_config_params(Rest);
validate_config_params([{server_tokens, Value} | _]) ->
    throw({server_tokens, Value});

validate_config_params([{socket_type, ip_comm} | Rest]) ->
    validate_config_params(Rest);

validate_config_params([{socket_type, Value} | Rest]) 
  when Value == ssl; Value == essl ->
    validate_config_params(Rest);

validate_config_params([{socket_type, {Value, _}} | Rest])
  when Value == essl orelse Value == ssl ->
    validate_config_params(Rest);

validate_config_params([{socket_type, Value} | _]) ->
    throw({socket_type, Value});

validate_config_params([{port, Value} | Rest]) 
  when is_integer(Value) andalso (Value >= 0) ->
    validate_config_params(Rest);
validate_config_params([{port, Value} | _]) -> 
    throw({port, Value});

validate_config_params([{bind_address, Value} | Rest])  ->
    case is_bind_address(Value) of
	true ->
	    validate_config_params(Rest);
	false ->
	    throw({bind_address, Value})
    end;

validate_config_params([{ipfamily, Value} | Rest]) 
  when ((Value =:= inet)  orelse 
	(Value =:= inet6) orelse 
	(Value =:= inet6fb4)) ->
    validate_config_params(Rest);
validate_config_params([{ipfamily, Value} | _]) -> 
    throw({ipfamily, Value});

validate_config_params([{keep_alive, Value} | Rest])  
  when (Value =:= true) orelse (Value =:= false) ->
    validate_config_params(Rest);
validate_config_params([{keep_alive, Value} | _]) ->
    throw({keep_alive, Value});

validate_config_params([{max_keep_alive_request, Value} | Rest]) 
  when is_integer(Value) andalso (Value > 0) ->
    validate_config_params(Rest);
validate_config_params([{max_keep_alive_request, Value} | _]) ->
    throw({max_keep_alive_request, Value});

validate_config_params([{keep_alive_timeout, Value} | Rest]) 
  when is_integer(Value) andalso (Value >= 0) ->
    validate_config_params(Rest);
validate_config_params([{keep_alive_timeout, Value} | _]) ->
    throw({keep_alive_timeout, Value});

validate_config_params([{modules, Value} | Rest]) ->
    ok = httpd_util:modules_validate(Value),
    validate_config_params(Rest);
	  
validate_config_params([{server_admin, Value} | Rest]) when is_list(Value) ->
    validate_config_params(Rest);
validate_config_params([{server_admin, Value} | _]) ->
    throw({server_admin, Value});

validate_config_params([{server_root, Value} | Rest]) ->
    ok = httpd_util:dir_validate(server_root, Value),
    validate_config_params(Rest);

validate_config_params([{mime_types, Value} | Rest]) ->
    ok = httpd_util:mime_types_validate(Value),
    validate_config_params(Rest);

validate_config_params([{max_clients, Value} | Rest]) 
  when is_integer(Value) andalso (Value > 0) ->
    validate_config_params(Rest);
validate_config_params([{max_clients, Value} | _]) ->
    throw({max_clients, Value});

validate_config_params([{document_root, Value} | Rest]) ->
    ok = httpd_util:dir_validate(document_root, Value),
    validate_config_params(Rest);

validate_config_params([{default_type, Value} | Rest]) when is_list(Value) ->
    validate_config_params(Rest);
validate_config_params([{default_type, Value} | _]) ->
    throw({default_type, Value});

validate_config_params([{ssl_certificate_file = Key, Value} | Rest]) ->
    ok = httpd_util:file_validate(Key, Value),
    validate_config_params(Rest);

validate_config_params([{ssl_certificate_key_file = Key, Value} | Rest]) ->
    ok = httpd_util:file_validate(Key, Value),
    validate_config_params(Rest);

validate_config_params([{ssl_verify_client, Value} | Rest]) 
  when (Value =:= 0) orelse (Value =:= 1) orelse (Value =:= 2) ->
    validate_config_params(Rest);

validate_config_params([{ssl_verify_client_depth, Value} | Rest]) 
  when is_integer(Value) andalso (Value >= 0) ->
    validate_config_params(Rest);
validate_config_params([{ssl_verify_client_depth, Value} | _]) ->
    throw({ssl_verify_client_depth, Value});

validate_config_params([{ssl_ciphers, Value} | Rest]) when is_list(Value) ->
    validate_config_params(Rest);
validate_config_params([{ssl_ciphers, Value} | _]) ->
    throw({ssl_ciphers, Value});

validate_config_params([{ssl_ca_certificate_file = Key, Value} | Rest]) ->
    ok = httpd_util:file_validate(Key, Value),
    validate_config_params(Rest);

validate_config_params([{ssl_password_callback_module, Value} | Rest]) 
  when is_atom(Value) ->
    validate_config_params(Rest);
validate_config_params([{ssl_password_callback_module, Value} | _]) ->
    throw({ssl_password_callback_module, Value});

validate_config_params([{ssl_password_callback_function, Value} | Rest]) 
  when is_atom(Value) ->
    validate_config_params(Rest);
validate_config_params([{ssl_password_callback_function, Value} | _]) ->
    throw({ssl_password_callback_function, Value});

validate_config_params([{ssl_password_callback_arguments, Value} | Rest]) 
  when is_list(Value) ->
    validate_config_params(Rest);
validate_config_params([{ssl_password_callback_arguments, Value} | _]) ->
    throw({ssl_password_callback_arguments, Value});

validate_config_params([{disable_chunked_transfer_encoding_send, Value} |
			Rest])  
  when (Value =:= true) orelse (Value =:= false) ->
    validate_config_params(Rest);
validate_config_params([{disable_chunked_transfer_encoding_send, Value} |
			_ ]) ->
    throw({disable_chunked_transfer_encoding_send, Value});
validate_config_params([_| Rest]) ->
    validate_config_params(Rest).

%% It is actually pointless to check bind_address in this way since
%% we need ipfamily to do it properly...
is_bind_address(any) ->
    true;
is_bind_address(Value) ->
    case httpd_util:ip_address(Value, inet6fb4) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

store(ConfigList0) -> 
    ?hdrd("store", []),
    try validate_config_params(ConfigList0) of
	ok ->
	    Modules = 
		proplists:get_value(modules, ConfigList0, ?DEFAULT_MODS),
	    ?hdrt("store", [{modules, Modules}]),
	    Port = proplists:get_value(port, ConfigList0),
	    Addr = proplists:get_value(bind_address, ConfigList0, any),
	    ConfigList = fix_mime_types(ConfigList0),
	    Name = httpd_util:make_name("httpd_conf", Addr, Port),
	    ConfigDB = ets:new(Name, [named_table, bag, protected]),
	    store(ConfigDB, ConfigList, 
		  lists:append(Modules, [?MODULE]), 
		  ConfigList)
    catch
	throw:Error ->
	    ?hdri("store - config parameter validation failed", 
		  [{error, Error}]),
	    {error, {invalid_option, Error}}
    end.

fix_mime_types(ConfigList0) ->
    case proplists:get_value(mime_types, ConfigList0) of
	undefined ->
	    ServerRoot = proplists:get_value(server_root, ConfigList0),
		MimeTypesFile = 
		filename:join([ServerRoot,"conf", "mime.types"]),
		case filelib:is_file(MimeTypesFile) of
		    true ->
			{ok, MimeTypesList} = load_mime_types(MimeTypesFile),
			[{mime_types, MimeTypesList} | ConfigList0];
		    false ->
			[{mime_types,
			  [{"html","text/html"},{"htm","text/html"}]} 
			 | ConfigList0]
		end;
	_ ->
	    ConfigList0
    end.

store({mime_types,MimeTypesList},ConfigList) ->
    Port = proplists:get_value(port, ConfigList),
    Addr = proplists:get_value(bind_address, ConfigList),
    Name = httpd_util:make_name("httpd_mime",Addr,Port),
    {ok, MimeTypesDB} = store_mime_types(Name,MimeTypesList),
    {ok, {mime_types,MimeTypesDB}};
store({log_format, LogFormat}, _ConfigList) 
  when (LogFormat =:= common) orelse (LogFormat =:= combined) ->
    {ok,{log_format, LogFormat}};
store({log_format, LogFormat}, _ConfigList) 
  when (LogFormat =:= compact) orelse (LogFormat =:= pretty) ->
    {ok, {log_format, LogFormat}};
store({server_tokens, ServerTokens} = Entry, _ConfigList) ->
    Server = server(ServerTokens), 
    {ok, [Entry, {server, Server}]};
store({keep_alive_timeout, KeepAliveTimeout}, _ConfigList) ->
    {ok, {keep_alive_timeout, KeepAliveTimeout}};
store(ConfigListEntry, _ConfigList) ->
    {ok, ConfigListEntry}.


%% The SERVER_SOFTWARE macro has the following structure: 
%%       <product>/<version>
%% Example: "inets/1.2.3"
%% So, with this example (on a linux machine, with OTP R15B), 
%% this will result in:
%%   prod:    "inets"
%%   major:   "inets/1"
%%   minor:   "inets/1.2"
%%   minimal: "inets/1.2.3"
%%   os:      "inets/1.2.3 (unix)
%%   full:    "inets/1.2.3 (unix/linux) OTP/R15B"
%% Note that the format of SERVER_SOFTWARE is that of 'minimal'.
%% Also, there will always be atleast two digits in a version: 
%% Not just 1 but 1.0
%% 
%% We have already checked that the value is valid, 
%% so there is no need to check enything here.
%% 
server(prod = _ServerTokens) ->
    [Prod|_Version] = string:tokens(?SERVER_SOFTWARE, [$/]),
    Prod;
server(major = _ServerTokens) ->
    [Prod|Version] = string:tokens(?SERVER_SOFTWARE, [$/]), 
    [Major|_]      = string:tokens(Version, [$.]), 
    Prod ++ "/" ++ Major;
server(minor = _ServerTokens) ->
    [Prod|Version]  = string:tokens(?SERVER_SOFTWARE, [$/]), 
    [Major,Minor|_] = string:tokens(Version, [$.]), 
    Prod ++ "/" ++ Major ++ "." ++ Minor;
server(minimal = _ServerTokens) ->
    %% This is the default
    ?SERVER_SOFTWARE;
server(os = _ServerTokens) ->
    OS = os_info(partial), 
    lists:flatten(io_lib:format("~s ~s", [?SERVER_SOFTWARE, OS]));
server(full = _ServerTokens) ->
    OTPRelease = otp_release(),
    OS = os_info(full), 
    lists:flatten(
      io_lib:format("~s ~s OTP/~s", [?SERVER_SOFTWARE, OS, OTPRelease]));
server({private, Server} = _ServerTokens) when is_list(Server) -> 
    %% The user provide its own 
    Server;
server(_) -> 
    ?SERVER_SOFTWARE.

os_info(Info) ->
    case os:type() of
	{OsFamily, _OsName} when Info =:= partial ->
	    lists:flatten(io_lib:format("(~w)", [OsFamily]));
	{OsFamily, OsName} ->
	    lists:flatten(io_lib:format("(~w/~w)", [OsFamily, OsName]))
    end.

otp_release() ->
    erlang:system_info(otp_release).


%% Phase 3: Remove
remove_all(ConfigDB) ->
    Modules = httpd_util:lookup(ConfigDB,modules,[]),
    remove_traverse(ConfigDB, lists:append(Modules,[?MODULE])).

remove(ConfigDB) ->
    ets:delete(ConfigDB),
    ok.

%% config(ConfigDB) ->
%%     case httpd_util:lookup(ConfigDB, socket_type, ip_comm) of
%% 	ssl ->
%% 	    case ssl_certificate_file(ConfigDB) of
%% 		undefined ->
%% 		    {error,
%% 		     "Directive SSLCertificateFile "
%% 		     "not found in the config file"};
%% 		SSLCertificateFile ->
%% 		    {ssl,
%% 		     SSLCertificateFile++
%% 		     ssl_certificate_key_file(ConfigDB)++
%% 		     ssl_verify_client(ConfigDB)++
%% 		     ssl_ciphers(ConfigDB)++
%% 		     ssl_password(ConfigDB)++
%% 		     ssl_verify_depth(ConfigDB)++
%% 		     ssl_ca_certificate_file(ConfigDB)}
%% 	    end;
%% 	ip_comm ->
%% 	    ip_comm
%%     end.


get_config(Address, Port) ->    
    Tab = httpd_util:make_name("httpd_conf", Address, Port),
    Properties =  ets:tab2list(Tab),
    MimeTab = proplists:get_value(mime_types, Properties),
    NewProperties = proplists:delete(mime_types, Properties),
    [{mime_types, ets:tab2list(MimeTab)} | NewProperties].
     
get_config(Address, Port, Properties) ->    
    Tab = httpd_util:make_name("httpd_conf", Address, Port),
    Config = 
	lists:map(fun(Prop) -> {Prop, httpd_util:lookup(Tab, Prop)} end,
		  Properties),
    [{Proporty, Value} || {Proporty, Value} <- Config, Value =/= undefined].  
	
		   
lookup(Tab, Key) ->
    httpd_util:lookup(Tab, Key).

lookup(Tab, Key, Default) when is_atom(Key) ->
    httpd_util:lookup(Tab, Key, Default);

lookup(Address, Port, Key) when is_integer(Port) ->
    Tab = table(Address, Port),
    lookup(Tab, Key).

lookup(Address, Port, Key, Default) when is_integer(Port) ->
    Tab = table(Address, Port),
    lookup(Tab, Key, Default).

table(Address, Port) ->
    httpd_util:make_name("httpd_conf", Address, Port).


lookup_socket_type(ConfigDB) ->
    case httpd_util:lookup(ConfigDB, socket_type, ip_comm) of
	ip_comm ->
	    ip_comm;
	{Tag, Conf} ->
	    {Tag, Conf};
	SSL when (SSL =:= ssl) orelse (SSL =:= essl) ->
	    SSLTag = 
		if
		    (SSL =:= ssl) ->
			?HTTP_DEFAULT_SSL_KIND;
		    true ->
			SSL
		end,
	    case ssl_certificate_file(ConfigDB) of
		undefined ->
		    Reason = "Directive SSLCertificateFile "
			"not found in the config file", 
		    throw({error, Reason}); 
		SSLCertificateFile ->
		    {SSLTag, SSLCertificateFile ++ ssl_config(ConfigDB)}
	    end
    end.

ssl_config(ConfigDB) ->
    ssl_certificate_key_file(ConfigDB) ++
	ssl_verify_client(ConfigDB) ++
	ssl_ciphers(ConfigDB) ++
	ssl_password(ConfigDB) ++
	ssl_verify_depth(ConfigDB) ++
	ssl_ca_certificate_file(ConfigDB) ++
	ssl_log_level(ConfigDB).
	    
    

%%%========================================================================
%%% Internal functions
%%%========================================================================
%%% Phase 1 Load:
bootstrap([]) ->
    {ok, ?DEFAULT_MODS};
bootstrap([Line|Config]) ->
    case Line of
	"Modules " ++ Modules ->
	    {ok, ModuleList} = inets_regexp:split(Modules," "),
	    TheMods = [list_to_atom(X) || X <- ModuleList],
	    case verify_modules(TheMods) of
		ok ->
		    {ok, TheMods};
		{error, Reason} ->
		    {error, Reason}
	    end;
	_ ->
	    bootstrap(Config)
    end.

load_config(Config, Modules) ->
    %% Create default contexts for all modules
    Contexts = lists:duplicate(length(Modules), []),
    load_config(Config, Modules, Contexts, []).

load_config([], _Modules, _Contexts, ConfigList) ->
    ?hdrv("config loaded", []),
    {ok, ConfigList};
	
load_config([Line|Config], Modules, Contexts, ConfigList) ->
    ?hdrt("load config", [{config_line, Line}]),
    case load_traverse(Line, Contexts, Modules, [], ConfigList, no) of
	{ok, NewContexts, NewConfigList} ->
	    load_config(Config, Modules, NewContexts, NewConfigList);
	{error, Reason} -> 
	    {error, Reason}
    end.


%% This loads the config file into each module specified by Modules
%% Each module has its own context that is passed to and (optionally)
%% returned by the modules load function. The module can also return
%% a ConfigEntry, which will be added to the global configuration
%% list.
%% All configuration directives are guaranteed to be passed to all
%% modules. Each module only implements the function clauses of
%% the load function for the configuration directives it supports,
%% it's ok if an apply returns {'EXIT', {function_clause, ..}}.
load_traverse(Line, [], [], _NewContexts, _ConfigList, no) ->
    {error, ?NICE("Configuration directive not recognized: "++Line)};
load_traverse(_Line, [], [], NewContexts, ConfigList, yes) ->
    {ok, lists:reverse(NewContexts), ConfigList};
load_traverse(Line, [Context|Contexts], [Module|Modules], NewContexts,
	      ConfigList, State) ->
    ?hdrt("load config traverse",
          [{context, Context}, {httpd_module, Module}, {state, State}]),
    case catch apply(Module, load, [Line, Context]) of
	{'EXIT', {function_clause, _FC}} ->
	    ?hdrt("does not handle load config", 
		  [{config_line, Line}, {fc, _FC}]),
	    load_traverse(Line, Contexts, Modules, 
			  [Context|NewContexts], ConfigList, State);

	{'EXIT', {undef, _}} ->
	    ?hdrt("does not implement load", []),
	    load_traverse(Line, Contexts, Modules,
			  [Context|NewContexts], ConfigList, yes);

	{'EXIT', Reason} ->
	    error_logger:error_report({'EXIT', Reason}),
	    load_traverse(Line, Contexts, Modules, 
			  [Context|NewContexts], ConfigList, State);

	ok ->
	    ?hdrt("line processed", []),
	    load_traverse(Line, Contexts, Modules, 
			  [Context|NewContexts], ConfigList, yes);

	{ok, NewContext} ->
	    ?hdrt("line processed", [{new_context, NewContext}]),
	    load_traverse(Line, Contexts, Modules, 
			  [NewContext|NewContexts], ConfigList, yes);

	{ok, NewContext, ConfigEntry} when is_tuple(ConfigEntry) ->
	     ?hdrt("line processed",
                  [{new_context, NewContext}, {config_entry, ConfigEntry}]),
	    load_traverse(Line, Contexts, 
			  Modules, [NewContext|NewContexts],
			  [ConfigEntry|ConfigList], yes);

	{ok, NewContext, ConfigEntry} when is_list(ConfigEntry) ->
	    ?hdrt("line processed",
                  [{new_context, NewContext}, {config_entry, ConfigEntry}]),
	    load_traverse(Line, Contexts, Modules, [NewContext|NewContexts],
			  lists:append(ConfigEntry, ConfigList), yes);

	{error, Reason} ->
	    ?hdrv("line processing failed", [{reason, Reason}]),
	    {error, Reason}
    end.
	
%% Verifies that all specified modules are available.
verify_modules([]) ->
    ok;
verify_modules([Mod|Rest]) ->
    case code:which(Mod) of
	non_existing ->
	    {error, ?NICE(atom_to_list(Mod)++" does not exist")};
	_Path ->
	    verify_modules(Rest)
    end.

%% Reads the entire configuration file and returns list of strings or
%% and error.
read_config_file(FileName) ->
    case file:open(FileName, [read]) of
	{ok, Stream} ->
	    read_config_file(Stream, []);
	{error, _Reason} ->
	    {error, ?NICE("Cannot open "++FileName)}
    end.
read_config_file(Stream, SoFar) ->
    case io:get_line(Stream, []) of
	eof ->
	    file:close(Stream),
	    {ok, lists:reverse(SoFar)};
	{error, Reason} ->
	    file:close(Stream),
	    {error, Reason};
	[$#|_Rest] ->
	    %% Ignore commented lines for efficiency later ..
	    read_config_file(Stream, SoFar);
	Line ->
	    {ok, NewLine, _}=inets_regexp:sub(clean(Line),"[\t\r\f ]"," "),
	    case NewLine of
		[] ->
		    %% Also ignore empty lines ..
		    read_config_file(Stream, SoFar);
		_Other ->
		    read_config_file(Stream, [NewLine|SoFar])
	    end
    end.

parse_mime_types(Stream,MimeTypesList) ->
    Line=
	case io:get_line(Stream,'') of
	    eof ->
		eof;
	    String ->
		clean(String)
	end,
    parse_mime_types(Stream, MimeTypesList, Line).
parse_mime_types(Stream, MimeTypesList, eof) ->
    file:close(Stream),
    {ok, MimeTypesList};
parse_mime_types(Stream, MimeTypesList, "") ->
    parse_mime_types(Stream, MimeTypesList);
parse_mime_types(Stream, MimeTypesList, [$#|_]) ->
    parse_mime_types(Stream, MimeTypesList);
parse_mime_types(Stream, MimeTypesList, Line) ->
    case inets_regexp:split(Line, " ") of
	{ok, [NewMimeType|Suffixes]} ->
	    parse_mime_types(Stream,
			     lists:append(suffixes(NewMimeType,Suffixes),
					  MimeTypesList));
	{ok, _} ->
	    {error, ?NICE(Line)}
    end.

suffixes(_MimeType,[]) ->
    [];
suffixes(MimeType,[Suffix|Rest]) ->
    [{Suffix,MimeType}|suffixes(MimeType,Rest)].


%% Phase 2: store
store(ConfigDB, _ConfigList, _Modules, []) ->
    {ok, ConfigDB};
store(ConfigDB, ConfigList, Modules, [ConfigListEntry|Rest]) ->
    ?hdrt("store", [{entry, ConfigListEntry}]),
    case store_traverse(ConfigListEntry, ConfigList, Modules) of
	{ok, ConfigDBEntry} when is_tuple(ConfigDBEntry) ->
	    ets:insert(ConfigDB, ConfigDBEntry),
	    store(ConfigDB, ConfigList, Modules, Rest);
	{ok, ConfigDBEntry} when is_list(ConfigDBEntry) ->
	    lists:foreach(fun(Entry) ->
				  ets:insert(ConfigDB,Entry)
			  end,ConfigDBEntry),
	    store(ConfigDB, ConfigList, Modules, Rest);
	{error, Reason} ->
	    {error,Reason}
    end.

store_traverse(_ConfigListEntry, _ConfigList,[]) ->
    {error, ?NICE("Unable to store configuration...")};
store_traverse(ConfigListEntry, ConfigList, [Module|Rest]) ->
    ?hdrt("store traverse",
          [{httpd_module, Module}, {entry, ConfigListEntry}]),
    case catch apply(Module, store, [ConfigListEntry, ConfigList]) of
	{'EXIT',{function_clause,_}} ->
	    ?hdrt("does not handle store config", []),
	    store_traverse(ConfigListEntry,ConfigList,Rest);
	{'EXIT',{undef, _}} ->
	    ?hdrt("does not implement store", []),
	    store_traverse(ConfigListEntry,ConfigList,Rest);
	{'EXIT', Reason} ->
	    error_logger:error_report({'EXIT',Reason}),
	    store_traverse(ConfigListEntry,ConfigList,Rest);
	Result ->
	    ?hdrt("config entry processed", [{result, Result}]),
	    Result
    end.

store_mime_types(Name,MimeTypesList) ->
    %% Make sure that the ets table is not duplicated
    %% when reloading configuration
    catch ets:delete(Name),
    MimeTypesDB = ets:new(Name, [named_table, set, protected]),
    store_mime_types1(MimeTypesDB, MimeTypesList).
store_mime_types1(MimeTypesDB,[]) ->
    {ok, MimeTypesDB};
store_mime_types1(MimeTypesDB,[Type|Rest]) ->
    ets:insert(MimeTypesDB, Type),
    store_mime_types1(MimeTypesDB, Rest).


%% Phase 3: remove
remove_traverse(_ConfigDB,[]) ->
    ok;
remove_traverse(ConfigDB,[Module|Rest]) ->
    case (catch apply(Module,remove,[ConfigDB])) of
	{'EXIT',{undef,_}} ->
	    remove_traverse(ConfigDB,Rest);
	{'EXIT',{function_clause,_}} ->
	    remove_traverse(ConfigDB,Rest);
	{'EXIT',Reason} ->
	    error_logger:error_report({'EXIT',Reason}),
	    remove_traverse(ConfigDB,Rest);
	{error,Reason} ->
	    error_logger:error_report(Reason),
	    remove_traverse(ConfigDB,Rest);
	_ ->
	    remove_traverse(ConfigDB,Rest)
    end.

ssl_certificate_file(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_certificate_file) of
	undefined ->
	    undefined;
	SSLCertificateFile ->
	    [{certfile,SSLCertificateFile}]
    end.

ssl_certificate_key_file(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_certificate_key_file) of
	undefined ->
	    [];
	SSLCertificateKeyFile ->
	    [{keyfile,SSLCertificateKeyFile}]
    end.

ssl_log_level(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_log_alert) of
	undefined ->
	    [];
	SSLLogLevel ->
	    [{log_alert,SSLLogLevel}]
    end.

ssl_verify_client(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_verify_client) of
	undefined ->
	    [];
	SSLVerifyClient ->
	    [{verify,SSLVerifyClient}]
    end.

ssl_ciphers(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_ciphers) of
	undefined ->
	    [];
	Ciphers ->
	    [{ciphers, Ciphers}]
    end.

ssl_password(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_password_callback_module) of
	undefined ->
	    [];
	Module ->
	    case httpd_util:lookup(ConfigDB, 
				   ssl_password_callback_function) of
		undefined ->
		    [];
		Function ->
		    Args = case httpd_util:lookup(ConfigDB, 
					   ssl_password_callback_arguments) of
			       undefined ->
				   [];
			       Arguments  ->
				   [Arguments]
			   end,
	       
		    case catch apply(Module, Function, Args) of
			Password when is_list(Password) ->
			    [{password, Password}];
			Error ->
			    error_report(ssl_password,Module,Function,Error),
			    []
		    end
	    end
    end.

ssl_verify_depth(ConfigDB) ->
    case httpd_util:lookup(ConfigDB, ssl_verify_client_depth) of
	undefined ->
	    [];
	Depth ->
	    [{depth, Depth}]
    end.

ssl_ca_certificate_file(ConfigDB) ->
    case httpd_util:lookup(ConfigDB, ssl_ca_certificate_file) of
	undefined ->
	    [];
	File ->
	    [{cacertfile, File}]
    end.

plain_server_tokens() ->
    [prod, major, minor, minimum, os, full].

error_report(Where,M,F,Error) ->
    error_logger:error_report([{?MODULE, Where}, 
			       {apply, {M, F, []}}, Error]).

