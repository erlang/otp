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
%%
-module(httpd_conf).

%% Application internal API
-export([load_mime_types/1, store/1, store/2,
	 remove/1, remove_all/1, get_config/3, get_config/4,
	 lookup_socket_type/1, 
	 lookup/2, lookup/3, lookup/4, 
	 validate_properties/1, white_space_clean/1]).

-define(VMODULE,"CONF").
-include("httpd_internal.hrl").
-include("httpd.hrl").
-include_lib("inets/src/http_lib/http_internal.hrl").

%%%=========================================================================
%%%  Application internal API
%%%=========================================================================
%% The configuration data is handled in three (2) phases:
%% 1. Traverse the key-value tuple list store it into an ETS table.
%%    Directives depending on other directives are taken care of here
%%    (store/1).
%% 3. Traverse the ETS table and do a complete clean-up (remove/1).

validate_ipfamily(inet) ->
    inet;
validate_ipfamily(inet6) ->
    inet6;
%% Backwards compatibility wrapper, 
%% fallback to the default, IPV4,
%% as it will most proably work.
%% IPv6 standard moved away from 
%% beeing able to fallback to ipv4
validate_ipfamily(inet6fb4) ->
    inet;
validate_ipfamily(IpFamilyStr) ->
    throw({error, {bad_ipfamily, IpFamilyStr}}).

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
validate_properties2(Properties0) ->
    Properties = fix_ipfamily(Properties0),
    case proplists:get_value(bind_address, Properties) of
	undefined ->
	    case proplists:get_value(sock_type, Properties, ip_comm) of
		ip_comm ->
		   add_inet_defaults(Properties);
		{ip_comm, _} ->
		    add_inet_defaults(Properties);
		_ ->
		    [{bind_address, any} | Properties]
	    end;
	any ->
	    Properties;
	Address0 ->
	    IpFamily = proplists:get_value(ipfamily, Properties, inet),
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

fix_ipfamily(Properties) ->
    case proplists:get_value(ipfamily, Properties) of
	undefined ->
	    Properties;
	IpFamily ->
	    NewProps = proplists:delete(ipfamily, Properties),
	    [{ipfamily, validate_ipfamily(IpFamily)} | NewProps]
    end.

add_inet_defaults(Properties) ->
    case proplists:get_value(ipfamily, Properties) of
	undefined ->
	    [{bind_address, any}, 
	     {ipfamily, inet} | Properties];
	_ ->
	    [{bind_address, any} | Properties]
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

validate_config_params([{socket_type, {Value, Opts}} | Rest]) when Value == ip_comm; 
								   Value == ssl; 
								   Value == essl ->
    %% Make sure not to set socket values used internaly
    validate_config_params(Opts), 
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

validate_config_params([{logger, Value} | Rest]) when is_list(Value) ->
    true = validate_logger(Value),
    validate_config_params(Rest);
validate_config_params([{logger, Value} | _]) ->
    throw({logger, Value});

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
validate_config_params([{Name, _} = Opt | _]) when Name == packet;
						   Name == mode;
						   Name == active;
						   Name == reuseaddr ->
    throw({internaly_handled_opt_can_not_be_set, Opt});
validate_config_params([_| Rest]) ->
    validate_config_params(Rest).

is_bind_address(any) ->
    true;
is_bind_address(Value) ->
    case is_bind_address(Value, inet) of
	false ->
	    is_bind_address(Value, inet6);
	True ->
	    True
    end.

is_bind_address(Value, IpFamily) ->
    case httpd_util:ip_address(Value, IpFamily) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.
 
store(ConfigList0) -> 
    try validate_config_params(ConfigList0) of
	ok ->
	    Modules = 
		proplists:get_value(modules, ConfigList0, ?DEFAULT_MODS),
	    Port = proplists:get_value(port, ConfigList0),
	    Addr = proplists:get_value(bind_address, ConfigList0, any),
	    Profile = proplists:get_value(profile, ConfigList0, default),
	    ConfigList = fix_mime_types(ConfigList0),
	    Name = httpd_util:make_name("httpd_conf", Addr, Port, Profile),
	    ConfigDB = ets:new(Name, [named_table, bag, protected]),
	    store(ConfigDB, ConfigList, 
		  lists:append(Modules, [?MODULE]), 
		  ConfigList)
    catch
	throw:Error ->
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
    MimeTypes ->
        case filelib:is_file(MimeTypes) of
            true ->
                {ok, MimeTypesList} = load_mime_types(MimeTypes),
                ConfigList = proplists:delete(mime_types, ConfigList0),
                [{mime_types, MimeTypesList} | ConfigList];
            false ->
                ConfigList0
        end
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
server(none = _ServerTokens) ->
    "";
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


get_config(Address, Port, Profile) ->    
    Tab = httpd_util:make_name("httpd_conf", Address, Port, Profile),
    Properties =  ets:tab2list(Tab),
    MimeTab = proplists:get_value(mime_types, Properties),
    NewProperties = proplists:delete(mime_types, Properties),
    [{mime_types, ets:tab2list(MimeTab)} | NewProperties].
     
get_config(Address, Port, Profile, Properties) ->    
    Tab = httpd_util:make_name("httpd_conf", Address, Port, Profile),
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
	{ip_comm, _} = Type ->
	    Type;
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
parse_mime_types(Stream,MimeTypesList) ->
    Line=
	case io:get_line(Stream,'') of
	    eof ->
		eof;
	    String ->
		re:replace(white_space_clean(String), "[\t\r\f ]"," ", [{return,list}, global])	
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
    case re:split(Line, " ", [{return, list}]) of
	[NewMimeType|Suffixes] ->
	    parse_mime_types(Stream,
			     lists:append(suffixes(NewMimeType,Suffixes),
					  MimeTypesList));
	_ ->
	    {error, ?NICE(Line)}
    end.

suffixes(_MimeType,[]) ->
    [];
suffixes(MimeType,[""|Rest]) ->
    suffixes(MimeType, Rest);
suffixes(MimeType,[Suffix|Rest]) ->
    [{Suffix,MimeType}|suffixes(MimeType,Rest)].


%% Phase 2: store
store(ConfigDB, _ConfigList, _Modules, []) ->
    {ok, ConfigDB};
store(ConfigDB, ConfigList, Modules, [ConfigListEntry|Rest]) ->
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
    case catch apply(Module, store, [ConfigListEntry, ConfigList]) of
	{'EXIT',{function_clause,_}} ->
	    store_traverse(ConfigListEntry,ConfigList,Rest);
	{'EXIT',{undef, _}} ->
	    store_traverse(ConfigListEntry,ConfigList,Rest);
	{'EXIT', Reason} ->
	    error_logger:error_report({'EXIT',Reason}),
	    store_traverse(ConfigListEntry,ConfigList,Rest);
	Result ->
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
    [none, prod, major, minor, minimum, os, full].

error_report(Where,M,F,Error) ->
    error_logger:error_report([{?MODULE, Where}, 
			       {apply, {M, F, []}}, Error]).
white_space_clean(String) ->
    re:replace(String, "^[ \t\n\r\f]*|[ \t\n\r\f]*\$","", 
	       [{return,list}, global]).

validate_logger([{error, Domain}]) when is_atom(Domain) ->
    true;
validate_logger(List) ->
    throw({logger, List}).





