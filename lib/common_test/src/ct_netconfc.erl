%%----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
%% File: ct_netconfc.erl
%%
%% Description:
%%    This file contains the Netconf client interface
%%
%% @author Support
%%
%% @doc Netconf client module.
%%
%% <p>The Netconf client is compliant with RFC4741 and RFC4742.</p>
%%
%% <p> For each server to test against, the following entry can be
%% added to a configuration file:</p>
%%
%% <p>`{server_id(),options()}.'</p>
%%
%% <p> The `server_id()' or an associated `target_name()' (see
%% {@link ct}) shall then be used in calls to {@link open/2}.</p>
%%
%% <p>If no configuration exists for a server, a session can still be
%% opened by calling {@link open/2} with all necessary options given
%% in the call. The first argument to {@link open/2} can then be any
%% atom.</p>
%%
%% == Logging ==
%%
%% The netconf server uses the `error_logger' for logging of netconf
%% traffic. A special purpose error handler is implemented in
%% `ct_conn_log_h'. To use this error handler, add the `cth_conn_log'
%% hook in your test suite, e.g.
%%
%% ```
%% suite() ->
%%    [{ct_hooks, [{cth_conn_log, [{conn_mod(),hook_options()}]}]}].
%%'''
%%
%% The `conn_mod()' is the name of the common_test module implementing
%% the connection protocol, e.g. `ct_netconfc'.
%%
%% The hook option `log_type' specifies the type of logging:
%%
%% <dl>
%%   <dt>`raw'</dt>
%%   <dd>The sent and received netconf data is logged to a separate
%%   text file as is without any formatting. A link to the file is
%%   added to the test case HTML log.</dd>
%%
%%   <dt>`pretty'</dt>
%%   <dd>The sent and received netconf data is logged to a separate
%%   text file with XML data nicely indented. A link to the file is
%%   added to the test case HTML log.</dd>
%%
%%   <dt>`html (default)'</dt>
%%   <dd>The sent and received netconf traffic is pretty printed
%%   directly in the test case HTML log.</dd>
%%
%%   <dt>`silent'</dt>
%%   <dd>Netconf traffic is not logged.</dd>
%% </dl>
%%
%% By default, all netconf traffic is logged in one single log
%% file. However, it is possible to have different connections logged
%% in separate files. To do this, use the hook option `hosts' and
%% list the names of the servers/connections that will be used in the
%% suite. Note that the connections must be named for this to work,
%% i.e. they must be opened with {@link open/2}.
%%
%% The `hosts' option has no effect if `log_type' is set to `html' or
%% `silent'.
%%
%% The hook options can also be specified in a configuration file with
%% the configuration variable `ct_conn_log':
%%
%% ```
%% {ct_conn_log,[{conn_mod(),hook_options()}]}.
%% '''
%%
%% For example:
%%
%% ```
%% {ct_conn_log,[{ct_netconfc,[{log_type,pretty},
%%                             {hosts,[key_or_name()]}]}]}
%% '''
%%
%% <b>Note</b> that hook options specified in a configuration file
%% will overwrite the hardcoded hook options in the test suite.
%%
%% === Logging example 1 ===
%%
%% The following `ct_hooks' statement will cause pretty printing of
%% netconf traffic to separate logs for the connections named
%% `nc_server1' and `nc_server2'. Any other connections will be logged
%% to default netconf log.
%%
%% ```
%% suite() ->
%%    [{ct_hooks, [{cth_conn_log, [{ct_netconfc,[{log_type,pretty}},
%%                                               {hosts,[nc_server1,nc_server2]}]}
%%                                ]}]}].
%%'''
%%
%% Connections must be opened like this:
%%
%% ```
%% open(nc_server1,[...]),
%% open(nc_server2,[...]).
%% '''
%%
%% === Logging example 2 ===
%%
%% The following configuration file will cause raw logging of all
%% netconf traffic into one single text file.
%%
%% ```
%% {ct_conn_log,[{ct_netconfc,[{log_type,raw}]}]}.
%% '''
%%
%% The `ct_hooks' statement must look like this:
%%
%% ```
%% suite() ->
%%    [{ct_hooks, [{cth_conn_log, []}]}].
%% '''
%%
%% The same `ct_hooks' statement without the configuration file would
%% cause HTML logging of all netconf connections into the test case
%% HTML log.
%%
%% == Notifications ==
%%
%% The netconf client is also compliant with RFC5277 NETCONF Event
%% Notifications, which defines a mechanism for an asynchronous
%% message notification delivery service for the netconf protocol.
%%
%% Specific functions to support this are {@link
%% create_subscription/6} and {@link get_event_streams/3}. (The
%% functions also exist with other arities.)
%%
%% @end
%%----------------------------------------------------------------------
-module(ct_netconfc).

-include("ct_netconfc.hrl").
-include("ct_util.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([open/1,
	 open/2,
	 only_open/1,
	 only_open/2,
	 hello/1,
	 hello/2,
	 hello/3,
	 close_session/1,
	 close_session/2,
	 kill_session/2,
	 kill_session/3,
	 send/2,
	 send/3,
	 send_rpc/2,
	 send_rpc/3,
	 lock/2,
	 lock/3,
	 unlock/2,
	 unlock/3,
	 get/2,
	 get/3,
	 get_config/3,
	 get_config/4,
	 edit_config/3,
	 edit_config/4,
	 edit_config/5,
	 delete_config/2,
	 delete_config/3,
	 copy_config/3,
	 copy_config/4,
	 action/2,
	 action/3,
	 create_subscription/1,
	 create_subscription/2,
	 create_subscription/3,
	 create_subscription/4,
	 create_subscription/5,
	 create_subscription/6,
	 get_event_streams/2,
	 get_event_streams/3,
	 get_capabilities/1,
	 get_capabilities/2,
	 get_session_id/1,
	 get_session_id/2]).

%%----------------------------------------------------------------------
%% Exported types
%%----------------------------------------------------------------------
-export_type([notification/0]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
%% ct_gen_conn callbacks
-export([init/3,
	 handle_msg/3,
	 handle_msg/2,
	 terminate/2,
	 close/1]).

%% ct_conn_log callback
-export([format_data/2]).

%%----------------------------------------------------------------------
%% Internal defines
%%----------------------------------------------------------------------
-define(APPLICATION,?MODULE).
-define(DEFAULT_STREAM,"NETCONF").

-define(error(ConnName,Report),
	error_logger:error_report([{ct_connection,ConnName},
				   {client,self()},
				   {module,?MODULE},
				   {line,?LINE} |
				   Report])).

-define(is_timeout(T), (is_integer(T) orelse T==infinity)).
-define(is_filter(F),
	(?is_simple_xml(F)
	 orelse (F==[])
	 orelse (is_list(F) andalso ?is_simple_xml(hd(F))))).
-define(is_simple_xml(Xml),
	(is_atom(Xml) orelse (is_tuple(Xml) andalso is_atom(element(1,Xml))))).
-define(is_string(S), (is_list(S) andalso is_integer(hd(S)))).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
%% Client state
-record(state, {host,
		port,
		connection,      % #connection
		capabilities,
		session_id,
		msg_id = 1,
		hello_status,
		no_end_tag_buff = <<>>,
		buff = <<>>,
		pending = [],    % [#pending]
		event_receiver}).% pid

%% Run-time client options.
-record(options, {ssh = [], % Options for the ssh application
		  host,
		  port = ?DEFAULT_PORT,
		  timeout = ?DEFAULT_TIMEOUT,
		  name}).

%% Connection reference
-record(connection, {reference, % {CM,Ch}
		     host,
		     port,
		     name}).

%% Pending replies from server
-record(pending, {tref,    % timer ref (returned from timer:xxx)
		  ref,     % pending ref
		  msg_id,
		  op,
		  caller}).% pid which sent the request

%%----------------------------------------------------------------------
%% Type declarations
%%----------------------------------------------------------------------
-type client() :: handle() | ct_gen_conn:server_id() | ct_gen_conn:target_name().
-type handle() :: term().
%% An opaque reference for a connection (netconf session). See {@link
%% ct} for more information.

-type options() :: [option()].
%% Options used for setting up ssh connection to a netconf server.

-type option() :: {ssh,host()} | {port,inet:port_number()} | {user,string()} |
		  {password,string()} | {user_dir,string()} |
		  {timeout,timeout()}.
-type host() :: inet:hostname() | inet:ip_address().

-type notification() :: {notification, xml_attributes(), notification_content()}.
-type notification_content() :: [event_time()|simple_xml()].
-type event_time() :: {eventTime,xml_attributes(),[xs_datetime()]}.

-type stream_name() :: string().
-type streams() :: [{stream_name(),[stream_data()]}].
-type stream_data() :: {description,string()} |
		       {replaySupport,string()} |
		       {replayLogCreationTime,string()} |
		       {replayLogAgedTime,string()}.
%% See XML Schema for Event Notifications found in RFC5277 for further
%% detail about the data format for the string values.

%-type error_handler() :: module().
-type error_reason() :: term().

-type simple_xml() :: {xml_tag(), xml_attributes(), xml_content()} |
		      {xml_tag(), xml_content()} |
		      xml_tag().
%% <p>This type is further described in the documentation for the
%% <tt>Xmerl</tt> application.</p>
-type xml_tag() :: atom().
-type xml_attributes() :: [{xml_attribute_tag(),xml_attribute_value()}].
-type xml_attribute_tag() :: atom().
-type xml_attribute_value() :: string().
-type xml_content() :: [simple_xml() | iolist()].
-type xpath() :: {xpath,string()}.

-type netconf_db() :: running | startup | candidate.
-type xs_datetime() :: string().
%% This date and time identifyer has the same format as the XML type
%% dateTime and compliant to RFC3339. The format is
%% ```[-]CCYY-MM-DDThh:mm:ss[.s][Z|(+|-)hh:mm]'''

%%----------------------------------------------------------------------
%% External interface functions
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
-spec open(Options) -> Result when
      Options :: options(),
      Result :: {ok,handle()} | {error,error_reason()}.
%% @doc Open a netconf session and exchange `hello' messages.
%%
%% If the server options are specified in a configuration file, or if
%% a named client is needed for logging purposes (see {@section
%% Logging}) use {@link open/2} instead.
%%
%% The opaque `handler()' reference which is returned from this
%% function is required as client identifier when calling any other
%% function in this module.
%%
%% The `timeout' option (milli seconds) is used when setting up
%% the ssh connection and when waiting for the hello message from the
%% server. It is not used for any other purposes during the lifetime
%% of the connection.
%%
%% @end
%%----------------------------------------------------------------------
open(Options) ->
    open(Options,#options{},[],true).

%%----------------------------------------------------------------------
-spec open(KeyOrName, ExtraOptions) -> Result when
      KeyOrName :: ct_gen_conn:key_or_name(),
      ExtraOptions :: options(),
      Result :: {ok,handle()} | {error,error_reason()}.
%% @doc Open a named netconf session and exchange `hello' messages.
%%
%% If `KeyOrName' is a configured `server_id()' or a
%% `target_name()' associated with such an ID, then the options
%% for this server will be fetched from the configuration file.
%
%% The `ExtraOptions' argument will be added to the options found in
%% the configuration file. If the same options are given, the values
%% from the configuration file will overwrite `ExtraOptions'.
%%
%% If the server is not specified in a configuration file, use {@link
%% open/1} instead.
%%
%% The opaque `handle()' reference which is returned from this
%% function can be used as client identifier when calling any other
%% function in this module. However, if `KeyOrName' is a
%% `target_name()', i.e. if the server is named via a call to
%% `ct:require/2' or a `require' statement in the test
%% suite, then this name may be used instead of the `handle()'.
%%
%% The `timeout' option (milli seconds) is used when setting up
%% the ssh connection and when waiting for the hello message from the
%% server. It is not used for any other purposes during the lifetime
%% of the connection.
%%
%% @see ct:require/2
%% @end
%%----------------------------------------------------------------------
open(KeyOrName, ExtraOpts) ->
    open(KeyOrName, ExtraOpts, true).

open(KeyOrName, ExtraOpts, Hello) ->
    SortedExtra = lists:keysort(1,ExtraOpts),
    SortedConfig = lists:keysort(1,ct:get_config(KeyOrName,[])),
    AllOpts = lists:ukeymerge(1,SortedConfig,SortedExtra),
    open(AllOpts,#options{name=KeyOrName},[{name,KeyOrName}],Hello).

open(OptList,InitOptRec,NameOpt,Hello) ->
    case check_options(OptList,undefined,undefined,InitOptRec) of
	{Host,Port,Options} ->
	    case ct_gen_conn:start({Host,Port},Options,?MODULE,
				   NameOpt ++ [{reconnect,false},
					       {use_existing_connection,false},
					       {forward_messages,true}]) of
		{ok,Client} when Hello==true ->
		    case hello(Client,Options#options.timeout) of
			ok ->
			    {ok,Client};
			Error ->
			    Error
		    end;
		Other ->
		    Other
	    end;
	Error ->
	    Error
    end.


%%----------------------------------------------------------------------
-spec only_open(Options) -> Result when
      Options :: options(),
      Result :: {ok,handle()} | {error,error_reason()}.
%% @doc Open a netconf session, but don't send `hello'.
%%
%% As {@link open/1} but does not send a `hello' message.
%%
%% @end
%%----------------------------------------------------------------------
only_open(Options) ->
    open(Options,#options{},[],false).

%%----------------------------------------------------------------------
-spec only_open(KeyOrName,ExtraOptions) -> Result when
      KeyOrName :: ct_gen_conn:key_or_name(),
      ExtraOptions :: options(),
      Result :: {ok,handle()} | {error,error_reason()}.
%% @doc Open a name netconf session, but don't send `hello'.
%%
%% As {@link open/2} but does not send a `hello' message.
%%
%% @end
%%----------------------------------------------------------------------
only_open(KeyOrName, ExtraOpts) ->
    open(KeyOrName, ExtraOpts, false).

%%----------------------------------------------------------------------
%% @spec hello(Client) -> Result
%% @equiv hello(Client, [], infinity)
hello(Client) ->
    hello(Client,[],?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec hello(Client,Timeout) -> Result when
      Client :: handle(),
      Timeout :: timeout(),
      Result :: ok | {error,error_reason()}.
%% @spec hello(Client, Timeout) -> Result
%% @equiv hello(Client, [], Timeout)
hello(Client,Timeout) ->
    hello(Client,[],Timeout).

%%----------------------------------------------------------------------
-spec hello(Client,Options,Timeout) -> Result when
      Client :: handle(),
      Options :: [{capability, [string()]}],
      Timeout :: timeout(),
      Result :: ok | {error,error_reason()}.
%% @doc Exchange `hello' messages with the server.
%%
%% Adds optional capabilities and sends a `hello' message to the
%% server and waits for the return.
%% @end
%%----------------------------------------------------------------------
hello(Client,Options,Timeout) ->
    call(Client, {hello, Options, Timeout}).


%%----------------------------------------------------------------------
%% @spec get_session_id(Client) -> Result
%% @equiv get_session_id(Client, infinity)
get_session_id(Client) ->
    get_session_id(Client, ?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec get_session_id(Client, Timeout) -> Result when
      Client :: client(),
      Timeout :: timeout(),
      Result :: pos_integer() | {error,error_reason()}.
%% @doc Returns the session id associated with the given client.
%%
%% @end
%%----------------------------------------------------------------------
get_session_id(Client, Timeout) ->
    call(Client, get_session_id, Timeout).

%%----------------------------------------------------------------------
%% @spec get_capabilities(Client) -> Result
%% @equiv get_capabilities(Client, infinity)
get_capabilities(Client) ->
    get_capabilities(Client, ?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec get_capabilities(Client, Timeout) -> Result when
      Client :: client(),
      Timeout :: timeout(),
      Result :: [string()] | {error,error_reason()}.
%% @doc Returns the server side capabilities
%%
%% The following capability identifiers, defined in RFC 4741, can be returned:
%%
%% <ul>
%%   <li>`"urn:ietf:params:netconf:base:1.0"'</li>
%%   <li>`"urn:ietf:params:netconf:capability:writable-running:1.0"'</li>
%%   <li>`"urn:ietf:params:netconf:capability:candidate:1.0"'</li>
%%   <li>`"urn:ietf:params:netconf:capability:confirmed-commit:1.0"'</li>
%%   <li>`"urn:ietf:params:netconf:capability:rollback-on-error:1.0"'</li>
%%   <li>`"urn:ietf:params:netconf:capability:startup:1.0"'</li>
%%   <li>`"urn:ietf:params:netconf:capability:url:1.0"'</li>
%%   <li>`"urn:ietf:params:netconf:capability:xpath:1.0"'</li>
%% </ul>
%%
%% Note, additional identifiers may exist, e.g. server side namespace.
%%
%% @end
%%----------------------------------------------------------------------
get_capabilities(Client, Timeout) ->
    call(Client, get_capabilities, Timeout).

%%----------------------------------------------------------------------
%% @spec send(Client, SimpleXml) -> Result
%% @equiv send(Client, SimpleXml, infinity)
send(Client, SimpleXml) ->
    send(Client, SimpleXml, ?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec send(Client, SimpleXml, Timeout) -> Result when
      Client :: client(),
      SimpleXml :: simple_xml(),
      Timeout :: timeout(),
      Result :: simple_xml() | {error,error_reason()}.
%% @doc Send an XML document to the server.
%%
%% The given XML document is sent as is to the server. This function
%% can be used for sending XML documents that can not be expressed by
%% other interface functions in this module.
send(Client, SimpleXml, Timeout) ->
    call(Client,{send, Timeout, SimpleXml}).

%%----------------------------------------------------------------------
%% @spec send_rpc(Client, SimpleXml) -> Result
%% @equiv send_rpc(Client, SimpleXml, infinity)
send_rpc(Client, SimpleXml) ->
    send_rpc(Client, SimpleXml, ?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec send_rpc(Client, SimpleXml, Timeout) -> Result when
      Client :: client(),
      SimpleXml :: simple_xml(),
      Timeout :: timeout(),
      Result :: [simple_xml()] | {error,error_reason()}.
%% @doc Send a Netconf <code>rpc</code> request to the server.
%%
%% The given XML document is wrapped in a valid Netconf
%% <code>rpc</code> request and sent to the server. The
%% <code>message-id</code> and namespace attributes are added to the
%% <code>rpc</code> element.
%%
%% This function can be used for sending <code>rpc</code> requests
%% that can not be expressed by other interface functions in this
%% module.
send_rpc(Client, SimpleXml, Timeout) ->
    call(Client,{send_rpc, SimpleXml, Timeout}).

%%----------------------------------------------------------------------
%% @spec lock(Client, Target) -> Result
%% @equiv lock(Client, Target, infinity)
lock(Client, Target) ->
    lock(Client, Target,?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec lock(Client, Target, Timeout) -> Result when
      Client :: client(),
      Target :: netconf_db(),
      Timeout :: timeout(),
      Result :: ok | {error,error_reason()}.
%% @doc Unlock configuration target.
%%
%% Which target parameters that can be used depends on if
%% `:candidate' and/or `:startup' are supported by the
%% server. If successfull, the configuration system of the device is
%% not available to other clients (Netconf, CORBA, SNMP etc). Locks
%% are intended to be short-lived.
%%
%% The operations {@link kill_session/2} or {@link kill_session/3} can
%% be used to force the release of a lock owned by another Netconf
%% session. How this is achieved by the server side is implementation
%% specific.
%%
%% @end
%%----------------------------------------------------------------------
lock(Client, Target, Timeout) ->
    call(Client,{send_rpc_op,lock,[Target],Timeout}).

%%----------------------------------------------------------------------
%% @spec unlock(Client, Target) -> Result
%% @equiv unlock(Client, Target, infinity)
unlock(Client, Target) ->
    unlock(Client, Target,?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec unlock(Client, Target, Timeout) -> Result when
      Client :: client(),
      Target :: netconf_db(),
      Timeout :: timeout(),
      Result :: ok | {error,error_reason()}.
%% @doc Unlock configuration target.
%%
%% If the client earlier has aquired a lock, via {@link lock/2} or
%% {@link lock/3}, this operation release the associated lock.  To be
%% able to access another target than `running', the server must
%% support `:candidate' and/or `:startup'.
%%
%% @end
%%----------------------------------------------------------------------
unlock(Client, Target, Timeout) ->
    call(Client, {send_rpc_op, unlock, [Target], Timeout}).

%%----------------------------------------------------------------------
%% @spec get(Client, Filter) -> Result
%% @equiv get(Client, Filter, infinity)
get(Client, Filter) ->
    get(Client, Filter, ?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec get(Client, Filter, Timeout) -> Result when
      Client :: client(),
      Filter :: simple_xml() | xpath(),
      Timeout :: timeout(),
      Result :: {ok,[simple_xml()]} | {error,error_reason()}.
%% @doc Get data.
%%
%% This operation returns both configuration and state data from the
%% server.
%%
%% Filter type `xpath' can only be used if the server supports
%% `:xpath'.
%%
%% @end
%%----------------------------------------------------------------------
get(Client, Filter, Timeout) ->
    call(Client,{send_rpc_op, get, [Filter], Timeout}).

%%----------------------------------------------------------------------
%% @spec get_config(Client, Source, Filter) -> Result
%% @equiv get_config(Client, Source, Filter, infinity)
get_config(Client, Source, Filter) ->
    get_config(Client, Source, Filter, ?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec get_config(Client, Source, Filter, Timeout) -> Result when
      Client :: client(),
      Source :: netconf_db(),
      Filter :: simple_xml() | xpath(),
      Timeout :: timeout(),
      Result :: {ok,[simple_xml()]} | {error,error_reason()}.
%% @doc Get configuration data.
%%
%% To be able to access another source than `running', the server
%% must advertise `:candidate' and/or `:startup'.
%%
%% Filter type `xpath' can only be used if the server supports
%% `:xpath'.
%%
%%
%% @end
%%----------------------------------------------------------------------
get_config(Client, Source, Filter, Timeout) ->
    call(Client, {send_rpc_op, get_config, [Source, Filter], Timeout}).

%%----------------------------------------------------------------------
%% @spec edit_config(Client, Target, Config) -> Result
%% @equiv edit_config(Client, Target, Config, [], infinity)
edit_config(Client, Target, Config) ->
    edit_config(Client, Target, Config, ?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec edit_config(Client, Target, Config, OptParamsOrTimeout) -> Result when
      Client :: client(),
      Target :: netconf_db(),
      Config :: simple_xml(),
      OptParamsOrTimeout :: [simple_xml()] | timeout(),
      Result :: ok | {error,error_reason()}.
%% @doc
%%
%% If `OptParamsOrTimeout' is a timeout value, then this is
%% equivalent to {@link edit_config/5. edit_config(Client, Target,
%% Config, [], Timeout)}.
%%
%% If `OptParamsOrTimeout' is a list of simple XML, then this is
%% equivalent to {@link edit_config/5. edit_config(Client, Target,
%% Config, OptParams, infinity)}.
%%
%% @end
edit_config(Client, Target, Config, Timeout) when ?is_timeout(Timeout) ->
    edit_config(Client, Target, Config, [], Timeout);
edit_config(Client, Target, Config, OptParams) when is_list(OptParams) ->
    edit_config(Client, Target, Config, OptParams, ?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec edit_config(Client, Target, Config, OptParams, Timeout) -> Result when
      Client :: client(),
      Target :: netconf_db(),
      Config :: simple_xml(),
      OptParams :: [simple_xml()],
      Timeout :: timeout(),
      Result :: ok | {error,error_reason()}.
%% @doc Edit configuration data.
%%
%% Per default only the running target is available, unless the server
%% include `:candidate' or `:startup' in its list of
%% capabilities.
%%
%% `OptParams' can be used for specifying optional parameters
%% (`default-operation', `test-option' or `error-option') that will be
%% added to the `edit-config' request. The value must be a list
%% containing valid simple XML, for example
%%
%% ```
%% [{'default-operation', ["none"]},
%%  {'error-option', ["rollback-on-error"]}]
%%'''
%%
%% @end
%%----------------------------------------------------------------------
edit_config(Client, Target, Config, OptParams, Timeout) ->
    call(Client, {send_rpc_op, edit_config, [Target,Config,OptParams], Timeout}).


%%----------------------------------------------------------------------
%% @spec delete_config(Client, Target) -> Result
%% @equiv delete_config(Client, Target, infinity)
delete_config(Client, Target) ->
    delete_config(Client, Target, ?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec delete_config(Client, Target, Timeout) -> Result when
      Client :: client(),
      Target :: startup | candidate,
      Timeout :: timeout(),
      Result :: ok | {error,error_reason()}.
%% @doc Delete configuration data.
%%
%% The running configuration cannot be deleted and `:candidate'
%% or `:startup' must be advertised by the server.
%%
%% @end
%%----------------------------------------------------------------------
delete_config(Client, Target, Timeout) when Target == startup;
					    Target == candidate ->
    call(Client,{send_rpc_op, delete_config, [Target], Timeout}).

%%----------------------------------------------------------------------
%% @spec copy_config(Client, Source, Target) -> Result
%% @equiv copy_config(Client, Source, Target, infinity)
copy_config(Client, Source, Target) ->
    copy_config(Client, Source, Target, ?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec copy_config(Client, Target, Source, Timeout) -> Result when
      Client :: client(),
      Target :: netconf_db(),
      Source :: netconf_db(),
      Timeout :: timeout(),
      Result :: ok | {error,error_reason()}.
%% @doc Copy configuration data.
%%
%% Which source and target options that can be issued depends on the
%% capabilities supported by the server. I.e. `:candidate' and/or
%% `:startup' are required.
%%
%% @end
%%----------------------------------------------------------------------
copy_config(Client, Target, Source, Timeout) ->
    call(Client,{send_rpc_op, copy_config, [Target, Source], Timeout}).

%%----------------------------------------------------------------------
%% @spec action(Client, Action) -> Result
%% @equiv action(Client, Action, infinity)
action(Client,Action) ->
    action(Client,Action,?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec action(Client, Action, Timeout) -> Result when
      Client :: client(),
      Action :: simple_xml(),
      Timeout :: timeout(),
      Result :: ok | {ok,[simple_xml()]} | {error,error_reason()}.
%% @doc Execute an action. If the return type is void, <c>ok</c> will
%%      be returned instead of <c>{ok,[simple_xml()]}</c>.
%%
%% @end
%%----------------------------------------------------------------------
action(Client,Action,Timeout) ->
    call(Client,{send_rpc_op, action, [Action], Timeout}).

%%----------------------------------------------------------------------
create_subscription(Client) ->
    create_subscription(Client,?DEFAULT_STREAM,?DEFAULT_TIMEOUT).

create_subscription(Client,Timeout)
  when ?is_timeout(Timeout) ->
    create_subscription(Client,?DEFAULT_STREAM,Timeout);
create_subscription(Client,Stream)
  when ?is_string(Stream) ->
    create_subscription(Client,Stream,?DEFAULT_TIMEOUT);
create_subscription(Client,Filter)
  when ?is_filter(Filter) ->
    create_subscription(Client,?DEFAULT_STREAM,Filter,
			?DEFAULT_TIMEOUT).

create_subscription(Client,Stream,Timeout)
  when ?is_string(Stream) andalso
       ?is_timeout(Timeout) ->
    call(Client,{send_rpc_op,{create_subscription,self()},
		 [Stream,undefined,undefined,undefined],
		 Timeout});
create_subscription(Client,StartTime,StopTime)
  when ?is_string(StartTime) andalso
       ?is_string(StopTime) ->
    create_subscription(Client,?DEFAULT_STREAM,StartTime,StopTime,
			?DEFAULT_TIMEOUT);
create_subscription(Client,Filter,Timeout)
  when ?is_filter(Filter) andalso
       ?is_timeout(Timeout) ->
    create_subscription(Client,?DEFAULT_STREAM,Filter,Timeout);
create_subscription(Client,Stream,Filter)
  when ?is_string(Stream) andalso
       ?is_filter(Filter) ->
    create_subscription(Client,Stream,Filter,?DEFAULT_TIMEOUT).

create_subscription(Client,StartTime,StopTime,Timeout)
  when ?is_string(StartTime) andalso
       ?is_string(StopTime) andalso
       ?is_timeout(Timeout) ->
    create_subscription(Client,?DEFAULT_STREAM,StartTime,StopTime,Timeout);
create_subscription(Client,Stream,StartTime,StopTime)
  when ?is_string(Stream) andalso
       ?is_string(StartTime) andalso
       ?is_string(StopTime) ->
    create_subscription(Client,Stream,StartTime,StopTime,?DEFAULT_TIMEOUT);
create_subscription(Client,Filter,StartTime,StopTime)
  when ?is_filter(Filter) andalso
       ?is_string(StartTime) andalso
       ?is_string(StopTime) ->
    create_subscription(Client,?DEFAULT_STREAM,Filter,
			StartTime,StopTime,?DEFAULT_TIMEOUT);
create_subscription(Client,Stream,Filter,Timeout)
  when ?is_string(Stream) andalso
       ?is_filter(Filter) andalso
       ?is_timeout(Timeout) ->
    call(Client,{send_rpc_op,{create_subscription,self()},
		 [Stream,Filter,undefined,undefined],
		 Timeout}).

create_subscription(Client,Stream,StartTime,StopTime,Timeout)
  when ?is_string(Stream) andalso
       ?is_string(StartTime) andalso
       ?is_string(StopTime) andalso
       ?is_timeout(Timeout) ->
    call(Client,{send_rpc_op,{create_subscription,self()},
		 [Stream,undefined,StartTime,StopTime],
		 Timeout});
create_subscription(Client,Stream,Filter,StartTime,StopTime)
  when ?is_string(Stream) andalso
       ?is_filter(Filter) andalso
       ?is_string(StartTime) andalso
       ?is_string(StopTime) ->
    create_subscription(Client,Stream,Filter,StartTime,StopTime,?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec create_subscription(Client, Stream, Filter,StartTime, StopTime, Timeout) ->
				 Result when
      Client :: client(),
      Stream :: stream_name(),
      Filter :: simple_xml() | [simple_xml()],
      StartTime :: xs_datetime(),
      StopTime :: xs_datetime(),
      Timeout :: timeout(),
      Result :: ok | {error,error_reason()}.
%% @doc Create a subscription for event notifications.
%%
%% This function sets up a subscription for netconf event
%% notifications of the given stream type, matching the given
%% filter. The calling process will receive notifications as messages
%% of type `notification()'.
%%
%% <dl>
%%   <dt>Stream:</dt>
%%   <dd> An optional parameter that indicates which stream of events
%%   is of interest.  If not present, events in the default NETCONF
%%   stream will be sent.</dd>
%%
%%   <dt>Filter:</dt>
%%   <dd>An optional parameter that indicates which subset of all
%%   possible events is of interest.  The format of this parameter is
%%   the same as that of the filter parameter in the NETCONF protocol
%%   operations.  If not present, all events not precluded by other
%%   parameters will be sent.</dd>
%%
%%   <dt>StartTime:</dt>
%%   <dd>An optional parameter used to trigger the replay feature and
%%   indicate that the replay should start at the time specified.  If
%%   `StartTime' is not present, this is not a replay subscription.
%%   It is not valid to specify start times that are later than the
%%   current time.  If the `StartTime' specified is earlier than the
%%   log can support, the replay will begin with the earliest
%%   available notification.  This parameter is of type dateTime and
%%   compliant to [RFC3339].  Implementations must support time
%%   zones.</dd>
%%
%%   <dt>StopTime:</dt>
%%   <dd>An optional parameter used with the optional replay feature
%%   to indicate the newest notifications of interest.  If `StopTime'
%%   is not present, the notifications will continue until the
%%   subscription is terminated.  Must be used with and be later than
%%   `StartTime'.  Values of `StopTime' in the future are valid.  This
%%   parameter is of type dateTime and compliant to [RFC3339].
%%   Implementations must support time zones.</dd>
%% </dl>
%%
%% See RFC5277 for further details about the event notification
%% mechanism.
%%
%% @end
%%----------------------------------------------------------------------
create_subscription(Client,Stream,Filter,StartTime,StopTime,Timeout) ->
    call(Client,{send_rpc_op,{create_subscription, self()},
		 [Stream,Filter,StartTime,StopTime],
		 Timeout}).

%%----------------------------------------------------------------------
%% @spec get_event_streams(Client, Timeout) -> Result
%% @equiv get_event_streams(Client, [], Timeout)
get_event_streams(Client,Timeout) when is_integer(Timeout); Timeout==infinity ->
    get_event_streams(Client,[],Timeout);

%%----------------------------------------------------------------------
%% @spec get_event_streams(Client, Streams) -> Result
%% @equiv get_event_streams(Client, Streams, infinity)
get_event_streams(Client,Streams) when is_list(Streams) ->
    get_event_streams(Client,Streams,?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec get_event_streams(Client, Streams, Timeout)
		       -> Result when
      Client :: client(),
      Streams :: [stream_name()],
      Timeout :: timeout(),
      Result :: {ok,streams()} | {error,error_reason()}.
%% @doc Send a request to get the given event streams.
%%
%% `Streams' is a list of stream names. The following filter will
%% be sent to the netconf server in a `get' request:
%%
%% ```
%% <netconf xmlns="urn:ietf:params:xml:ns:netmod:notification">
%%   <streams>
%%     <stream>
%%       <name>StreamName1</name>
%%     </stream>
%%     <stream>
%%       <name>StreamName2</name>
%%     </stream>
%%     ...
%%   </streams>
%% </netconf>
%% '''
%%
%% If `Streams' is an empty list, ALL streams will be requested
%% by sending the following filter:
%%
%% ```
%% <netconf xmlns="urn:ietf:params:xml:ns:netmod:notification">
%%   <streams/>
%% </netconf>
%% '''
%%
%% If more complex filtering is needed, a use {@link get/2} or {@link
%% get/3} and specify the exact filter according to XML Schema for
%% Event Notifications found in RFC5277.
%%
%% @end
%%----------------------------------------------------------------------
get_event_streams(Client,Streams,Timeout) ->
    call(Client,{get_event_streams,Streams,Timeout}).


%%----------------------------------------------------------------------
%% @spec close_session(Client) -> Result
%% @equiv close_session(Client, infinity)
close_session(Client) ->
    close_session(Client, ?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec close_session(Client, Timeout) -> Result when
      Client :: client(),
      Timeout :: timeout(),
      Result :: ok | {error,error_reason()}.
%% @doc Request graceful termination of the session associated with the client.
%%
%% When a netconf server receives a `close-session' request, it
%% will gracefully close the session.  The server will release any
%% locks and resources associated with the session and gracefully
%% close any associated connections.  Any NETCONF requests received
%% after a `close-session' request will be ignored.
%%
%% @end
%%----------------------------------------------------------------------
close_session(Client, Timeout) ->
    call(Client,{send_rpc_op, close_session, [], Timeout}, true).


%%----------------------------------------------------------------------
%% @spec kill_session(Client, SessionId) -> Result
%% @equiv kill_session(Client, SessionId, infinity)
kill_session(Client, SessionId) ->
    kill_session(Client, SessionId, ?DEFAULT_TIMEOUT).

%%----------------------------------------------------------------------
-spec kill_session(Client, SessionId, Timeout) -> Result when
      Client :: client(),
      SessionId :: pos_integer(),
      Timeout :: timeout(),
      Result :: ok | {error,error_reason()}.
%% @doc Force termination of the session associated with the supplied
%% session id.
%%
%% The server side shall abort any operations currently in process,
%% release any locks and resources associated with the session, and
%% close any associated connections.
%%
%% Only if the server is in the confirmed commit phase, the
%% configuration will be restored to its state before entering the
%% confirmed commit phase. Otherwise, no configuration roll back will
%% be performed.
%%
%% If the given `SessionId' is equal to the current session id,
%% an error will be returned.
%%
%% @end
%% ----------------------------------------------------------------------
kill_session(Client, SessionId, Timeout) ->
    call(Client,{send_rpc_op, kill_session, [SessionId], Timeout}).


%%----------------------------------------------------------------------
%% Callback functions
%%----------------------------------------------------------------------

%% @private
init(_KeyOrName,{_Host,_Port},Options) ->
    case ssh_open(Options) of
	{ok, Connection} ->
	    log(Connection,open),
	    {ConnPid,_} = Connection#connection.reference,
	    {ok, ConnPid, #state{connection = Connection}};
	{error,Reason}->
	    {error,Reason}
    end.

%% @private
terminate(_, #state{connection=Connection}) ->
    ssh_close(Connection),
    log(Connection,close),
    ok.

%% @private
handle_msg({hello, Options, Timeout}, From,
	   #state{connection=Connection,hello_status=HelloStatus} = State) ->
    case do_send(Connection, client_hello(Options)) of
	ok ->
	    case HelloStatus of
		undefined ->
		    {Ref,TRef} = set_request_timer(Timeout),
		    {noreply, State#state{hello_status=#pending{tref=TRef,
								ref=Ref,
								caller=From}}};
		received ->
		    {reply, ok, State#state{hello_status=done}};
		{error,Reason} ->
		    {stop, {error,Reason}, State}
	    end;
	Error ->
	    {stop, Error, State}
    end;
handle_msg(_, _From, #state{session_id=undefined} = State) ->
    %% Hello is not yet excanged - this shall never happen
    {reply,{error,waiting_for_hello},State};
handle_msg(get_capabilities, _From, #state{capabilities = Caps} = State) ->
    {reply, Caps, State};
handle_msg(get_session_id, _From, #state{session_id = Id} = State) ->
    {reply, Id, State};
handle_msg({send, Timeout, SimpleXml}, From,
	    #state{connection=Connection,pending=Pending} = State) ->
    case do_send(Connection, SimpleXml) of
	ok ->
	    {Ref,TRef} = set_request_timer(Timeout),
	    {noreply, State#state{pending=[#pending{tref=TRef,
						    ref=Ref,
						    caller=From} | Pending]}};
	Error ->
	    {reply, Error, State}
    end;
handle_msg({send_rpc, SimpleXml, Timeout}, From, State) ->
    do_send_rpc(undefined, SimpleXml, Timeout, From, State);
handle_msg({send_rpc_op, Op, Data, Timeout}, From, State) ->
    SimpleXml = encode_rpc_operation(Op,Data),
    do_send_rpc(Op, SimpleXml, Timeout, From, State);
handle_msg({get_event_streams=Op,Streams,Timeout}, From, State) ->
    Filter = {netconf,?NETMOD_NOTIF_NAMESPACE_ATTR,
             [{streams,[{stream,[{name,[Name]}]} || Name <- Streams]}]},
    SimpleXml = encode_rpc_operation(get,[Filter]),
    do_send_rpc(Op, SimpleXml, Timeout, From, State).

%% @private
handle_msg({ssh_cm, CM, {data, Ch, _Type, Data}}, State) ->
    ssh_connection:adjust_window(CM,Ch,size(Data)),
    handle_data(Data, State);
handle_msg({ssh_cm, _CM, _SshCloseMsg}, State) ->
    %% _SshCloseMsg can probably be one of
    %% {eof,Ch}
    %% {exit_status,Ch,Status}
    %% {exit_signal,Ch,ExitSignal,ErrorMsg,LanguageString}
    %% {signal,Ch,Signal}

    %% This might e.g. happen if the server terminates the connection,
    %% as in kill-session (or if ssh:close is called from somewhere
    %% unexpected).

    %%! Log this??
    %%! Currently the log will say that the client closed the
    %%! connection - due to terminate/2

    {stop, State};
handle_msg({Ref,timeout},
	   #state{hello_status=#pending{ref=Ref,caller=Caller}} = State) ->
    ct_gen_conn:return(Caller,{error,{hello_session_failed,timeout}}),
    {stop,State#state{hello_status={error,timeout}}};
handle_msg({Ref,timeout},#state{pending=Pending} = State) ->
    {value,#pending{op=Op,caller=Caller},Pending1} =
	lists:keytake(Ref,#pending.ref,Pending),
    ct_gen_conn:return(Caller,{error,timeout}),
    R = case Op of
	    close_session -> stop;
	    _ -> noreply
	end,
    %% Halfhearted try to get in correct state, this matches
    %% the implementation before this patch
    {R,State#state{pending=Pending1, no_end_tag_buff= <<>>, buff= <<>>}}.

%% @private
%% Called by ct_util_server to close registered connections before terminate.
close(Client) ->
    case get_handle(Client) of
	{ok,Pid} ->
	    case ct_gen_conn:stop(Pid) of
		{error,{process_down,Pid,noproc}} ->
		    {error,already_closed};
		Result ->
		    Result
	    end;
	Error ->
	    Error
    end.


%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
call(Client, Msg) ->
    call(Client, Msg, infinity, false).
call(Client, Msg, Timeout) when is_integer(Timeout); Timeout==infinity ->
    call(Client, Msg, Timeout, false);
call(Client, Msg, WaitStop) when is_boolean(WaitStop) ->
    call(Client, Msg, infinity, WaitStop).
call(Client, Msg, Timeout, WaitStop) ->
    case get_handle(Client) of
	{ok,Pid} ->
	    case ct_gen_conn:call(Pid,Msg,Timeout) of
		{error,{process_down,Pid,noproc}} ->
		    {error,no_such_client};
		{error,{process_down,Pid,normal}} when WaitStop ->
		    %% This will happen when server closes connection
		    %% before client received rpc-reply on
		    %% close-session.
		    ok;
		{error,{process_down,Pid,normal}} ->
		    {error,closed};
		{error,{process_down,Pid,Reason}} ->
		    {error,{closed,Reason}};
		Other when WaitStop ->
		    MRef = erlang:monitor(process,Pid),
		    receive
			{'DOWN',MRef,process,Pid,Normal} when Normal==normal;
							      Normal==noproc ->
			    Other;
			{'DOWN',MRef,process,Pid,Reason} ->
			    {error,{{closed,Reason},Other}}
		    after Timeout ->
			    erlang:demonitor(MRef, [flush]),
			    {error,{timeout,Other}}
		    end;
		Other ->
		    Other
	    end;
	Error ->
	    Error
    end.

get_handle(Client) when is_pid(Client) ->
    {ok,Client};
get_handle(Client) ->
    case ct_util:get_connection(Client, ?MODULE) of
	{ok,{Pid,_}} ->
	    {ok,Pid};
	{error,no_registered_connection} ->
	    {error,{no_connection_found,Client}};
	Error ->
	    Error
    end.

check_options([], undefined, _Port, _Options) ->
    {error, no_host_address};
check_options([], _Host, undefined, _Options) ->
    {error, no_port};
check_options([], Host, Port, Options) ->
    {Host,Port,Options};
check_options([{ssh, Host}|T], _, Port, #options{} = Options) ->
    check_options(T, Host, Port, Options#options{host=Host});
check_options([{port,Port}|T], Host, _, #options{} = Options) ->
    check_options(T, Host, Port, Options#options{port=Port});
check_options([{timeout, Timeout}|T], Host, Port, Options)
  when is_integer(Timeout); Timeout==infinity ->
    check_options(T, Host, Port, Options#options{timeout = Timeout});
check_options([{timeout, _} = Opt|_T], _Host, _Port, _Options) ->
    {error, {invalid_option, Opt}};
check_options([Opt|T], Host, Port, #options{ssh=SshOpts}=Options) ->
    %% Option verified by ssh
    check_options(T, Host, Port, Options#options{ssh=[Opt|SshOpts]}).

%%%-----------------------------------------------------------------
set_request_timer(infinity) ->
    {undefined,undefined};
set_request_timer(T) ->
    Ref = make_ref(),
    {ok,TRef} = timer:send_after(T,{Ref,timeout}),
    {Ref,TRef}.

%%%-----------------------------------------------------------------
cancel_request_timer(undefined,undefined) ->
    ok;
cancel_request_timer(Ref,TRef) ->
    _ = timer:cancel(TRef),
    receive {Ref,timeout} -> ok
    after 0 -> ok
    end.

%%%-----------------------------------------------------------------
client_hello(Options) when is_list(Options) ->
    UserCaps = [{capability, UserCap} ||
		   {capability, UserCap} <- Options,
		   is_list(hd(UserCap))],
    {hello, ?NETCONF_NAMESPACE_ATTR,
     [{capabilities,
       [{capability,[?NETCONF_BASE_CAP++?NETCONF_BASE_CAP_VSN]}|
	UserCaps]}]}.

%%%-----------------------------------------------------------------

encode_rpc_operation(Lock,[Target]) when Lock==lock; Lock==unlock ->
    {Lock,[{target,[Target]}]};
encode_rpc_operation(get,[Filter]) ->
    {get,filter(Filter)};
encode_rpc_operation(get_config,[Source,Filter]) ->
    {'get-config',[{source,[Source]}] ++ filter(Filter)};
encode_rpc_operation(edit_config,[Target,Config,OptParams]) ->
    {'edit-config',[{target,[Target]}] ++ OptParams ++ [{config,[Config]}]};
encode_rpc_operation(delete_config,[Target]) ->
    {'delete-config',[{target,[Target]}]};
encode_rpc_operation(copy_config,[Target,Source]) ->
    {'copy-config',[{target,[Target]},{source,[Source]}]};
encode_rpc_operation(action,[Action]) ->
    {action,?ACTION_NAMESPACE_ATTR,[{data,[Action]}]};
encode_rpc_operation(kill_session,[SessionId]) ->
    {'kill-session',[{'session-id',[integer_to_list(SessionId)]}]};
encode_rpc_operation(close_session,[]) ->
    'close-session';
encode_rpc_operation({create_subscription,_},
		     [Stream,Filter,StartTime,StopTime]) ->
    {'create-subscription',?NETCONF_NOTIF_NAMESPACE_ATTR,
     [{stream,[Stream]}] ++
	 filter(Filter) ++
	 maybe_element(startTime,StartTime) ++
	 maybe_element(stopTime,StopTime)}.

filter(undefined) ->
    [];
filter({xpath,Filter}) when ?is_string(Filter) ->
    [{filter,[{type,"xpath"},{select, Filter}],[]}];
filter(Filter) when is_list(Filter) ->
    [{filter,[{type,"subtree"}],Filter}];
filter(Filter) ->
    filter([Filter]).

maybe_element(_,undefined) ->
    [];
maybe_element(Tag,Value) ->
    [{Tag,[Value]}].

%%%-----------------------------------------------------------------
%%% Send XML data to server
do_send_rpc(PendingOp,SimpleXml,Timeout,Caller,
	    #state{connection=Connection,msg_id=MsgId,pending=Pending} = State) ->
    case do_send_rpc(Connection, MsgId, SimpleXml) of
	ok ->
	    {Ref,TRef} = set_request_timer(Timeout),
	    {noreply, State#state{msg_id=MsgId+1,
				  pending=[#pending{tref=TRef,
						    ref=Ref,
						    msg_id=MsgId,
						    op=PendingOp,
						    caller=Caller} | Pending]}};
	Error ->
	    {reply, Error, State#state{msg_id=MsgId+1}}
    end.

do_send_rpc(Connection, MsgId, SimpleXml) ->
    do_send(Connection,
	    {rpc,
	     [{'message-id',MsgId} | ?NETCONF_NAMESPACE_ATTR],
	     [SimpleXml]}).

do_send(Connection, SimpleXml) ->
    Xml=to_xml_doc(SimpleXml),
    log(Connection,send,Xml),
    ssh_send(Connection, Xml).

to_xml_doc(Simple) ->
    Prolog = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    Xml = unicode:characters_to_binary(
	    xmerl:export_simple([Simple],
				xmerl_xml,
				[#xmlAttribute{name=prolog,
					       value=Prolog}])),
    <<Xml/binary,?END_TAG/binary>>.

%%%-----------------------------------------------------------------
%%% Parse and handle received XML data
%%% Two buffers are used:
%%%   * 'no_end_tag_buff' contains data that is checked and does not
%%%     contain any (part of an) end tag.
%%%   * 'buff' contains all other saved data - it may or may not
%%%     include (a part of) an end tag.
%%% The reason for this is to avoid running binary:split/3 multiple
%%% times on the same data when it does not contain an end tag. This
%%% can be a considerable optimation in the case when a lot of data is
%%% received (e.g. when fetching all data from a node) and the data is
%%% sent in multiple ssh packages.
handle_data(NewData,#state{connection=Connection} = State0) ->
    log(Connection,recv,NewData),
    NoEndTag0 = State0#state.no_end_tag_buff,
    Buff0 = State0#state.buff,
    Data = <<Buff0/binary, NewData/binary>>,
    case binary:split(Data,?END_TAG,[]) of
	[_NoEndTagFound] ->
	    NoEndTagSize = case byte_size(Data) of
			       Sz when Sz<5 -> 0;
			       Sz -> Sz-5
			   end,
	    <<NoEndTag1:NoEndTagSize/binary,Buff/binary>> = Data,
	    NoEndTag = <<NoEndTag0/binary,NoEndTag1/binary>>,
	    {noreply, State0#state{no_end_tag_buff=NoEndTag, buff=Buff}};
	[FirstMsg0,Buff1] ->
	    FirstMsg = remove_initial_nl(<<NoEndTag0/binary,FirstMsg0/binary>>),
	    SaxArgs = [{event_fun,fun sax_event/3}, {event_state,[]}],
	    case xmerl_sax_parser:stream(FirstMsg, SaxArgs) of
		{ok, Simple, _Thrash} ->
		    case decode(Simple, State0#state{no_end_tag_buff= <<>>,
						     buff=Buff1}) of
			{noreply, #state{buff=Buff} = State} when Buff =/= <<>> ->
			    %% Recurse if we have more data in buffer
			    handle_data(<<>>, State);
			Other ->
			    Other
		    end;
		{fatal_error,_Loc,Reason,_EndTags,_EventState} ->
		    ?error(Connection#connection.name,
			   [{parse_error,Reason},
			    {buffer, Buff0},
			    {new_data,NewData}]),
		    handle_error(Reason, State0#state{no_end_tag_buff= <<>>,
						      buff= <<>>})
	    end
    end.


%% xml does not accept a leading nl and some netconf server add a nl after
%% each ?END_TAG, ignore them
remove_initial_nl(<<"\n", Data/binary>>) ->
    remove_initial_nl(Data);
remove_initial_nl(Data) ->
    Data.

handle_error(Reason, State) ->
    Pending1 = case State#state.pending of
		   [] -> [];
		   Pending ->
		       %% Assuming the first request gets the
		       %% first answer
		       P=#pending{tref=TRef,ref=Ref,caller=Caller} =
			   lists:last(Pending),
		       cancel_request_timer(Ref,TRef),
		       Reason1 = {failed_to_parse_received_data,Reason},
		       ct_gen_conn:return(Caller,{error,Reason1}),
		       lists:delete(P,Pending)
	       end,
    {noreply, State#state{pending=Pending1}}.

%% Event function for the sax parser. It builds a simple XML structure.
%% Care is taken to keep namespace attributes and prefixes as in the original XML.
sax_event(Event,_Loc,State) ->
    sax_event(Event,State).

sax_event({startPrefixMapping, Prefix, Uri},Acc) ->
    %% startPrefixMapping will always come immediately before the
    %% startElement where the namespace is defined.
    [{xmlns,{Prefix,Uri}}|Acc];
sax_event({startElement,_Uri,_Name,QN,Attrs},Acc) ->
    %% Pick out any namespace attributes inserted due to a
    %% startPrefixMapping event.The rest of Acc will then be only
    %% elements.
    {NsAttrs,NewAcc} = split_attrs_and_elements(Acc,[]),
    Tag = qn_to_tag(QN),
    [{Tag,NsAttrs ++ parse_attrs(Attrs),[]}|NewAcc];
sax_event({endElement,_Uri,_Name,_QN},[{Name,Attrs,Cont},{Parent,PA,PC}|Acc]) ->
    [{Parent,PA,[{Name,Attrs,lists:reverse(Cont)}|PC]}|Acc];
sax_event(endDocument,[{Tag,Attrs,Cont}]) ->
    {Tag,Attrs,lists:reverse(Cont)};
sax_event({characters,String},[{Name,Attrs,Cont}|Acc]) ->
    [{Name,Attrs,[String|Cont]}|Acc];
sax_event(_Event,State) ->
    State.

split_attrs_and_elements([{xmlns,{Prefix,Uri}}|Rest],Attrs) ->
    split_attrs_and_elements(Rest,[{xmlnstag(Prefix),Uri}|Attrs]);
split_attrs_and_elements(Elements,Attrs) ->
    {Attrs,Elements}.

xmlnstag([]) ->
    xmlns;
xmlnstag(Prefix) ->
    list_to_atom("xmlns:"++Prefix).

qn_to_tag({[],Name}) ->
    list_to_atom(Name);
qn_to_tag({Prefix,Name}) ->
    list_to_atom(Prefix ++ ":" ++ Name).

parse_attrs([{_Uri, [], Name, Value}|Attrs]) ->
    [{list_to_atom(Name),Value}|parse_attrs(Attrs)];
parse_attrs([{_Uri, Prefix, Name, Value}|Attrs]) ->
    [{list_to_atom(Prefix ++ ":" ++ Name),Value}|parse_attrs(Attrs)];
parse_attrs([]) ->
    [].


%%%-----------------------------------------------------------------
%%% Decoding of parsed XML data
decode({Tag,Attrs,_}=E, #state{connection=Connection,pending=Pending}=State) ->
    ConnName = Connection#connection.name,
    case get_local_name_atom(Tag) of
	'rpc-reply' ->
	    case get_msg_id(Attrs) of
		undefined ->
		    case Pending of
			[#pending{msg_id=MsgId}] ->
			    ?error(ConnName,[{warning,rpc_reply_missing_msg_id},
					       {assuming,MsgId}]),
			    decode_rpc_reply(MsgId,E,State);
			_ ->
			    ?error(ConnName,[{error,rpc_reply_missing_msg_id}]),
			    {noreply,State}
		    end;
		MsgId ->
		    decode_rpc_reply(MsgId,E,State)
	    end;
	hello ->
	    case State#state.hello_status of
		undefined ->
		    case decode_hello(E) of
			{ok,SessionId,Capabilities} ->
			    {noreply,State#state{session_id = SessionId,
						 capabilities = Capabilities,
						 hello_status = received}};
			{error,Reason} ->
			    {noreply,State#state{hello_status = {error,Reason}}}
		    end;
		#pending{tref=TRef,ref=Ref,caller=Caller} ->
		    cancel_request_timer(Ref,TRef),
		    case decode_hello(E) of
			{ok,SessionId,Capabilities} ->
			    ct_gen_conn:return(Caller,ok),
			    {noreply,State#state{session_id = SessionId,
						 capabilities = Capabilities,
						 hello_status = done}};
			{error,Reason} ->
			    ct_gen_conn:return(Caller,{error,Reason}),
			    {stop,State#state{hello_status={error,Reason}}}
		    end;
		Other ->
		    ?error(ConnName,[{got_unexpected_hello,E},
				       {hello_status,Other}]),
		    {noreply,State}
	    end;
	notification ->
	    EventReceiver = State#state.event_receiver,
	    EventReceiver ! E,
	    {noreply,State};
	Other ->
	    %% Result of send/2, when not sending an rpc request - or
	    %% if netconf server sends noise. Can handle this only if
	    %% there is just one pending that matches (i.e. has
	    %% undefined msg_id and op)
	    case [P || P = #pending{msg_id=undefined,op=undefined} <- Pending] of
		[#pending{tref=TRef,ref=Ref,caller=Caller}] ->
		    cancel_request_timer(Ref,TRef),
		    ct_gen_conn:return(Caller,E),
		    {noreply,State#state{pending=[]}};
		_ ->
		    ?error(ConnName,[{got_unexpected_msg,Other},
				       {expecting,Pending}]),
		    {noreply,State}
	    end

    end.

get_msg_id(Attrs) ->
    case lists:keyfind('message-id',1,Attrs) of
	{_,Str} ->
	    list_to_integer(Str);
	false ->
	    undefined
    end.

decode_rpc_reply(MsgId,{_,Attrs,Content0}=E,#state{pending=Pending} = State) ->
    case lists:keytake(MsgId,#pending.msg_id,Pending) of
	{value, #pending{tref=TRef,ref=Ref,op=Op,caller=Caller}, Pending1} ->
	    cancel_request_timer(Ref,TRef),
	    Content = forward_xmlns_attr(Attrs,Content0),
	    {CallerReply,{ServerReply,State2}} =
		do_decode_rpc_reply(Op,Content,State#state{pending=Pending1}),
	    ct_gen_conn:return(Caller,CallerReply),
	    {ServerReply,State2};
	false ->
	    %% Result of send/2, when receiving a correct
	    %% rpc-reply. Can handle this only if there is just one
	    %% pending that matches (i.e. has undefined msg_id and op)
	    case [P || P = #pending{msg_id=undefined,op=undefined} <- Pending] of
		[#pending{tref=TRef,
			  ref=Ref,
			  msg_id=undefined,
			  op=undefined,
			  caller=Caller}] ->
		    cancel_request_timer(Ref,TRef),
		    ct_gen_conn:return(Caller,E),
		    {noreply,State#state{pending=[]}};
		_ ->
		    ConnName = (State#state.connection)#connection.name,
		    ?error(ConnName,[{got_unexpected_msg_id,MsgId},
						   {expecting,Pending}]),
		    {noreply,State}
	    end
    end.

do_decode_rpc_reply(Op,Result,State)
  when Op==lock; Op==unlock; Op==edit_config; Op==delete_config;
       Op==copy_config; Op==kill_session ->
    {decode_ok(Result),{noreply,State}};
do_decode_rpc_reply(Op,Result,State)
  when Op==get; Op==get_config; Op==action ->
    {decode_data(Result),{noreply,State}};
do_decode_rpc_reply(close_session,Result,State) ->
    case decode_ok(Result) of
	ok -> {ok,{stop,State}};
	Other -> {Other,{noreply,State}}
    end;
do_decode_rpc_reply({create_subscription,Caller},Result,State) ->
    case decode_ok(Result) of
	ok ->
	    {ok,{noreply,State#state{event_receiver=Caller}}};
	Other ->
	    {Other,{noreply,State}}
    end;
do_decode_rpc_reply(get_event_streams,Result,State) ->
    {decode_streams(decode_data(Result)),{noreply,State}};
do_decode_rpc_reply(undefined,Result,State) ->
    {Result,{noreply,State}}.



decode_ok([{Tag,Attrs,Content}]) ->
    case get_local_name_atom(Tag) of
	ok ->
	    ok;
	'rpc-error' ->
	    {error,forward_xmlns_attr(Attrs,Content)};
	_Other ->
	    {error,{unexpected_rpc_reply,[{Tag,Attrs,Content}]}}
    end;
decode_ok(Other) ->
    {error,{unexpected_rpc_reply,Other}}.

decode_data([{Tag,Attrs,Content}]) ->
    case get_local_name_atom(Tag) of
	ok ->
	    %% when action has return type void
	    ok;	
	data ->
	    %% Since content of data has nothing from the netconf
	    %% namespace, we remove the parent's xmlns attribute here
	    %% - just to make the result cleaner
	    {ok,forward_xmlns_attr(remove_xmlnsattr_for_tag(Tag,Attrs),Content)};
	'rpc-error' ->
	    {error,forward_xmlns_attr(Attrs,Content)};
	_Other ->
	    {error,{unexpected_rpc_reply,[{Tag,Attrs,Content}]}}
    end;
decode_data(Other) ->
    {error,{unexpected_rpc_reply,Other}}.

get_qualified_name(Tag) ->
    case string:tokens(atom_to_list(Tag),":") of
	[TagStr] -> {[],TagStr};
	[PrefixStr,TagStr] -> {PrefixStr,TagStr}
    end.

get_local_name_atom(Tag) ->
    {_,TagStr} = get_qualified_name(Tag),
    list_to_atom(TagStr).


%% Remove the xmlns attr that points to the tag. I.e. if the tag has a
%% prefix, remove {'xmlns:prefix',_}, else remove default {xmlns,_}.
remove_xmlnsattr_for_tag(Tag,Attrs) ->
    {Prefix,_TagStr} = get_qualified_name(Tag),
    XmlnsTag = xmlnstag(Prefix),
    case lists:keytake(XmlnsTag,1,Attrs) of
	{value,_,NoNsAttrs} ->
	    NoNsAttrs;
	false ->
	    Attrs
    end.

%% Take all xmlns attributes from the parent's attribute list and
%% forward into all childrens' attribute lists. But do not overwrite
%% any.
forward_xmlns_attr(ParentAttrs,Children) ->
    do_forward_xmlns_attr(get_all_xmlns_attrs(ParentAttrs,[]),Children).

do_forward_xmlns_attr(XmlnsAttrs,[{ChT,ChA,ChC}|Children]) ->
    ChA1 = add_xmlns_attrs(XmlnsAttrs,ChA),
    [{ChT,ChA1,ChC} | do_forward_xmlns_attr(XmlnsAttrs,Children)];
do_forward_xmlns_attr(_XmlnsAttrs,[]) ->
    [].

add_xmlns_attrs([{Key,_}=A|XmlnsAttrs],ChA) ->
    case lists:keymember(Key,1,ChA) of
	true ->
	    add_xmlns_attrs(XmlnsAttrs,ChA);
	false ->
	    add_xmlns_attrs(XmlnsAttrs,[A|ChA])
    end;
add_xmlns_attrs([],ChA) ->
    ChA.

get_all_xmlns_attrs([{xmlns,_}=Default|Attrs],XmlnsAttrs) ->
    get_all_xmlns_attrs(Attrs,[Default|XmlnsAttrs]);
get_all_xmlns_attrs([{Key,_}=Attr|Attrs],XmlnsAttrs) ->
    case atom_to_list(Key) of
	"xmlns:"++_Prefix ->
	    get_all_xmlns_attrs(Attrs,[Attr|XmlnsAttrs]);
	_ ->
	    get_all_xmlns_attrs(Attrs,XmlnsAttrs)
    end;
get_all_xmlns_attrs([],XmlnsAttrs) ->
    XmlnsAttrs.


%% Decode server hello to pick out session id and capabilities
decode_hello({hello,_Attrs,Hello}) ->
    case lists:keyfind('session-id',1,Hello) of
	{'session-id',_,[SessionId]} ->
	    case lists:keyfind(capabilities,1,Hello) of
		{capabilities,_,Capabilities} ->
		    case decode_caps(Capabilities,[],false) of
			{ok,Caps} ->
			    {ok,list_to_integer(SessionId),Caps};
			Error ->
			    Error
		    end;
		false ->
		    {error,{incorrect_hello,capabilities_not_found}}
	    end;
	false ->
	    {error,{incorrect_hello,no_session_id_found}}
    end.

decode_caps([{capability,[],[?NETCONF_BASE_CAP++Vsn=Cap]} |Caps], Acc, _) ->
    case Vsn of
	?NETCONF_BASE_CAP_VSN ->
	    decode_caps(Caps, [Cap|Acc], true);
	_ ->
	    {error,{incompatible_base_capability_vsn,Vsn}}
    end;
decode_caps([{capability,[],[Cap]}|Caps],Acc,Base) ->
    decode_caps(Caps,[Cap|Acc],Base);
decode_caps([H|_T],_,_) ->
    {error,{unexpected_capability_element,H}};
decode_caps([],_,false) ->
    {error,{incorrect_hello,no_base_capability_found}};
decode_caps([],Acc,true) ->
    {ok,lists:reverse(Acc)}.


%% Return a list of {Name,Data}, where data is a {Tag,Value} list for each stream
decode_streams({error,Reason}) ->
    {error,Reason};
decode_streams({ok,[{netconf,_,Streams}]}) ->
    {ok,decode_streams(Streams)};
decode_streams([{streams,_,Streams}]) ->
    decode_streams(Streams);
decode_streams([{stream,_,Stream} | Streams]) ->
    {name,_,[Name]} = lists:keyfind(name,1,Stream),
    [{Name,[{Tag,Value} || {Tag,_,[Value]} <- Stream, Tag /= name]}
     | decode_streams(Streams)];
decode_streams([]) ->
    [].


%%%-----------------------------------------------------------------
%%% Logging

log(Connection,Action) ->
    log(Connection,Action,<<>>).
log(#connection{host=Host,port=Port,name=Name},Action,Data) ->
    error_logger:info_report(#conn_log{client=self(),
				       address={Host,Port},
				       name=Name,
				       action=Action,
				       module=?MODULE},
			     Data).


%% Log callback - called from the error handler process
%% @private
format_data(How,Data) ->
    %% Assuming that the data is encoded as UTF-8.  If it is not, then
    %% the printout might be wrong, but the format function will not
    %% crash!
    %% FIXME: should probably read encoding from the data and do
    %% unicode:characters_to_binary(Data,InEncoding,utf8) when calling
    %% log/3 instead of assuming utf8 in as done here!
    do_format_data(How,unicode:characters_to_binary(Data)).

do_format_data(raw,Data) ->
    io_lib:format("~n~ts~n",[hide_password(Data)]);
do_format_data(pretty,Data) ->
    maybe_io_lib_format(indent(Data));
do_format_data(html,Data) ->
    maybe_io_lib_format(html_format(Data)).

maybe_io_lib_format(<<>>) ->
    [];
maybe_io_lib_format(String) ->
    io_lib:format("~n~ts~n",[String]).

%%%-----------------------------------------------------------------
%%% Hide password elements from XML data
hide_password(Bin) ->
    re:replace(Bin,<<"(<password[^>]*>)[^<]*(</password>)">>,<<"\\1*****\\2">>,
	       [global,{return,binary},unicode]).

%%%-----------------------------------------------------------------
%%% HTML formatting
html_format(Bin) ->
    binary:replace(indent(Bin),<<"<">>,<<"&lt;">>,[global]).

%%%-----------------------------------------------------------------
%%% Indentation of XML code
indent(Bin) ->
    String = normalize(hide_password(Bin)),
    IndentedString =
	case erase(part_of_line) of
	    undefined ->
		indent1(String,[]);
	    Part ->
		indent1(lists:reverse(Part)++String,erase(indent))
	end,
    unicode:characters_to_binary(IndentedString).

%% Normalizes the XML document by removing all space and newline
%% between two XML tags.
%% Returns a list, no matter if the input was a list or a binary.
normalize(Bin) ->
    re:replace(Bin,<<">[ \r\n\t]+<">>,<<"><">>,[global,{return,list},unicode]).


indent1("<?"++Rest1,Indent1) ->
    %% Prolog
    {Line,Rest2,Indent2} = indent_line(Rest1,Indent1,[$?,$<]),
    Line++indent1(Rest2,Indent2);
indent1("</"++Rest1,Indent1) ->
    %% Stop tag
    case indent_line1(Rest1,Indent1,[$/,$<]) of
	{[],[],_} ->
	    [];
	{Line,Rest2,Indent2} ->
	    "\n"++Line++indent1(Rest2,Indent2)
    end;
indent1("<"++Rest1,Indent1) ->
    %% Start- or empty tag
    put(tag,get_tag(Rest1)),
    case indent_line(Rest1,Indent1,[$<]) of
	{[],[],_} ->
	    [];
	{Line,Rest2,Indent2} ->
	    "\n"++Line++indent1(Rest2,Indent2)
    end;
indent1([H|T],Indent) ->
    [H|indent1(T,Indent)];
indent1([],_Indent) ->
    [].

indent_line("?>"++Rest,Indent,Line) ->
    %% Prolog
    {lists:reverse(Line)++"?>",Rest,Indent};
indent_line("/></"++Rest,Indent,Line) ->
    %% Empty tag, and stop of parent tag -> one step out in indentation
    {Indent++lists:reverse(Line)++"/>","</"++Rest,Indent--"  "};
indent_line("/>"++Rest,Indent,Line) ->
    %% Empty tag, then probably next tag -> keep indentation
    {Indent++lists:reverse(Line)++"/>",Rest,Indent};
indent_line("></"++Rest,Indent,Line) ->
    LastTag = erase(tag),
    case get_tag(Rest) of
	LastTag ->
	    %% Start and stop tag, but no content
	    indent_line1(Rest,Indent,[$/,$<,$>|Line]);
	_ ->
	    %% Stop tag completed, and then stop tag of parent -> one step out
	    {Indent++lists:reverse(Line)++">","</"++Rest,Indent--"  "}
    end;
indent_line("><"++Rest,Indent,Line) ->
    %% Stop tag completed, and new tag comming -> keep indentation
    {Indent++lists:reverse(Line)++">","<"++Rest,"  "++Indent};
indent_line("</"++Rest,Indent,Line) ->
    %% Stop tag starting -> search for end of this tag
    indent_line1(Rest,Indent,[$/,$<|Line]);
indent_line([H|T],Indent,Line) ->
    indent_line(T,Indent,[H|Line]);
indent_line([],Indent,Line) ->
    %% The line is not complete - will be continued later
    put(part_of_line,Line),
    put(indent,Indent),
    {[],[],Indent}.

indent_line1("></"++Rest,Indent,Line) ->
    %% Stop tag completed, and then stop tag of parent -> one step out
    {Indent++lists:reverse(Line)++">","</"++Rest,Indent--"  "};
indent_line1(">"++Rest,Indent,Line) ->
    %% Stop tag completed -> keep indentation
    {Indent++lists:reverse(Line)++">",Rest,Indent};
indent_line1([H|T],Indent,Line) ->
    indent_line1(T,Indent,[H|Line]);
indent_line1([],Indent,Line) ->
    %% The line is not complete - will be continued later
    put(part_of_line,Line),
    put(indent,Indent),
    {[],[],Indent}.

get_tag("/>"++_) ->
    [];
get_tag(">"++_) ->
    [];
get_tag([H|T]) ->
    [H|get_tag(T)];
get_tag([]) ->
    %% The line is not complete - will be continued later.
    [].


%%%-----------------------------------------------------------------
%%% SSH stuff

ssh_open(#options{host=Host,timeout=Timeout,port=Port,ssh=SshOpts,name=Name}) ->
    case ssh:connect(Host, Port,
		     [{user_interaction,false},
		      {silently_accept_hosts, true}|SshOpts]) of
	{ok,CM} ->
	    case ssh_connection:session_channel(CM, Timeout) of
		{ok,Ch} ->
		    case ssh_connection:subsystem(CM, Ch, "netconf", Timeout) of
			success ->
			    {ok, #connection{reference = {CM,Ch},
					     host = Host,
					     port = Port,
					     name = Name}};
			failure ->
			    ssh:close(CM),
			    {error,{ssh,could_not_execute_netconf_subsystem}};
			{error,timeout} ->
			    {error,{ssh,could_not_execute_netconf_subsystem,timeout}}
		    end;
		{error, Reason} ->
		    ssh:close(CM),
		    {error,{ssh,could_not_open_channel,Reason}}
	    end;
	{error,Reason} ->
	    {error,{ssh,could_not_connect_to_server,Reason}}
    end.

ssh_send(#connection{reference = {CM,Ch}}, Data) ->
    case ssh_connection:send(CM, Ch, Data) of
	ok -> ok;
	{error,Reason} -> {error,{ssh,failed_to_send_data,Reason}}
    end.

ssh_close(#connection{reference = {CM,_Ch}}) ->
    ssh:close(CM).


%%----------------------------------------------------------------------
%% END OF MODULE
%%----------------------------------------------------------------------
