%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2023. All Rights Reserved.
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

%% Description:
%%% This version of the HTTP/1.1 client supports:
%%%      - RFC 2616 HTTP 1.1 client part
%%%      - RFC 2818 HTTP Over TLS

-module(httpc).

-behaviour(inets_service).

%% API
-export([
	 request/1, request/2, request/4, request/5,
	 cancel_request/1, cancel_request/2,
	 set_option/2,  set_option/3,
	 set_options/1, set_options/2,
	 get_option/1,  get_option/2,
	 get_options/1, get_options/2,
	 store_cookies/2, store_cookies/3, 
	 cookie_header/1, cookie_header/2, cookie_header/3, 
	 which_cookies/0, which_cookies/1, 
	 reset_cookies/0, reset_cookies/1, 
	 which_sessions/0, which_sessions/1, 
	 stream_next/1,
	 default_profile/0, 
	 profile_name/1, profile_name/2,
         ssl_verify_host_options/1,
	 info/0, info/1
	]).

%% Behavior callbacks
-export([start_standalone/1, start_service/1, 
	 stop_service/1, 
	 services/0, service_info/1]).


-include_lib("inets/src/http_lib/http_internal.hrl").
-include("httpc_internal.hrl").

-define(DEFAULT_PROFILE, default).

%%%=========================================================================
%%%  API
%%%=========================================================================

default_profile() ->
    ?DEFAULT_PROFILE.

-spec profile_name(pid()) -> pid();
                  (atom()) -> pid() | atom().
profile_name(?DEFAULT_PROFILE) ->
    httpc_manager;
profile_name(Profile) when is_pid(Profile) -> 
    Profile;
profile_name(Profile) -> 
    Prefix = lists:flatten(io_lib:format("~w_", [?MODULE])),
    profile_name(Prefix, Profile).


profile_name(Prefix, Profile) when is_atom(Profile) ->
    list_to_atom(Prefix ++ atom_to_list(Profile));
profile_name(_Prefix, Profile) when is_pid(Profile) ->
    Profile.


-spec request(uri_string:uri_string()) -> {ok, Result} | {error, term()} when
      Result :: {StatusLine :: { HttpVersion, StatusCode, string()}
                , [HttpHeader]
                , HttpBodyResult}
              | { StatusCode
                , HttpBodyResult}
              | RequestId
              | saved_to_file,
      HttpBodyResult :: uri_string:uri_string() | binary(),
      HttpVersion :: uri_string:uri_string(),
      StatusCode :: non_neg_integer(),
      HttpHeader :: { Field :: [byte()]
                    , Value :: binary() | iolist()},
      RequestId :: any().
request(Url) ->
    request(Url, default_profile()).

-spec request(Url, Profile) -> {ok, Result} | {error, term()} when
      Url :: uri_string:uri_string(),
      Profile :: atom() | pid(),
      Result ::{ StatusLine, [HttpHeader], HttpBodyResult}
              | { StatusCode, HttpBodyResult}
              | RequestId
              | saved_to_file,
      HttpHeader :: { Field :: [byte()]
                    , Value :: binary() | iolist()},
      HttpBodyResult :: uri_string:uri_string() | binary(),
      StatusLine :: { HttpVersion
                    , StatusCode
                    , string()},
      HttpVersion :: uri_string:uri_string(),
      StatusCode  :: non_neg_integer(),
      RequestId :: any().
request(Url, Profile) ->
    request(get, {Url, []}, [], [], Profile).

%% @doc Sends a HTTP-request. The function can be both synchronous and
%% asynchronous in the later case the function will return `{ok, RequestId}'
%% and later on a message will be sent to the
%% calling process on the format {http, {RequestId, {StatusLine,
%% Headers, Body}}} or {http, {RequestId, {error, Reason}}}.
%% Only octects are accepted in header fields and values.
-spec request(Method, Request, HttpOptions, Options) -> {ok, Result} | {error, term()} when
      Method :: head | get | put | patch | post | trace | options | delete,
      Request :: { uri_string:uri_string()
                 , [HttpHeader] }
               | { uri_string:uri_string()
                 , [ HttpHeader ]
                 , ContentType::uri_string:uri_string()
                 , HttpBody},
      HttpBody :: iolist()
                | binary()
                | { fun((Accumulator::term()) ->
                      eof | {ok, iolist(), Accumulator::term()}), Accumulator::term()}
                | { chunkify
                  , fun((Accumulator::term()) ->
                      eof | {ok, iolist(), Accumulator::term()})
                  , Accumulator::term() },
      HttpOptions :: [HttpOption],
      HttpOption :: {timeout, timeout()}
                  | {connect_timeout, timeout()}
                  | {ssl, [ssl:tls_option()]}
                  | {autoredirect, boolean()}
                  | {proxy_auth, {string(), string()}}
                  | {version, HttpVersion} | {relaxed, boolean()},
      Options :: [OptionRequest],
      OptionRequest :: {sync, boolean()}
                        | {stream, StreamTo}
                        | {body_format, BodyFormat}
                        | {full_result, boolean()}
                        | {headers_as_is, boolean()}
                        | {socket_opts, [SocketOpt]}
                        | {receiver, Receiver}
                        | {ipv6_host_with_brackets, boolean()},
      StreamTo :: none | self | {self, once} | file:name_all(),
      SocketOpt :: term(),
      BodyFormat  :: string() | binary() | atom(),
      Receiver :: pid()
                  | fun((term()) -> term())
                  | { ReceiverModule::atom()
                    , ReceiverFunction::atom()
                    , ReceiverArgs::list()},
      Result :: { StatusLine , [HttpHeader], HttpBodyResult}
              | { StatusCode, HttpBodyResult}
              | RequestId
              | saved_to_file,
      StatusCode::non_neg_integer(),
      StatusLine :: { HttpVersion
                    , StatusCode
                    , string()},
      HttpVersion :: uri_string:uri_string(),
      HttpHeader :: { Field :: [byte()]
                    , Value :: binary() | iolist()},
      HttpBodyResult :: uri_string:uri_string() | binary(),
      RequestId :: any().
request(Method, Request, HttpOptions, Options) ->
    request(Method, Request, HttpOptions, Options, default_profile()).


-define(WITH_BODY, [post, put, patch, delete]).
-define(WITHOUT_BODY, [get, head, options, trace, put, delete]).

-spec request(Method, Request, HttpOptions, Options, Profile) -> {ok, Result} | {error, term()} when
      Method :: head | get | put | patch | post | trace | options | delete,
      Request :: { uri_string:uri_string()
                 , [HttpHeader] }
               | { uri_string:uri_string()
                 , [ HttpHeader ]
                 , ContentType::uri_string:uri_string()
                 , HttpBody},
      HttpBody :: iolist()
                | binary()
                | { fun((Accumulator::term()) ->
                      eof | {ok, iolist(), Accumulator::term()}), Accumulator::term()}
                | { chunkify
                  , fun((Accumulator::term()) ->
                      eof | {ok, iolist(), Accumulator::term()})
                  , Accumulator::term() },
      HttpHeader :: { Field :: [byte()]
                    , Value :: binary() | iolist()},
      HttpOptions :: [HttpOption],
      HttpOption :: {timeout, timeout()}
                  | {connect_timeout, timeout()}
                  | {ssl, [ssl:tls_option()]}
                  | {autoredirect, boolean()}
                  | {proxy_auth, {string(), string()}}
                  | {version, HttpVersion} | {relaxed, boolean()},
      Options :: [OptionRequest],
      OptionRequest :: {sync, boolean()}
                     | {stream, StreamTo}
                     | {body_format, BodyFormat}
                     | {full_result, boolean()}
                     | {headers_as_is, boolean()}
                     | {socket_opts, [SocketOpt]}
                     | {receiver, Receiver}
                     | {ipv6_host_with_brackets, boolean()},
      StreamTo :: none | self | {self, once} | file:name_all(),
      BodyFormat  :: string() | binary() | atom(),
      SocketOpt :: term(),
      Receiver :: pid()
                  | fun((term()) -> term())
                  | { ReceiverModule::atom()
                    , ReceiverFunction::atom()
                    , ReceiverArgs::list()},
      Profile :: atom() | pid(),
      HttpVersion :: uri_string:uri_string(),
      Result :: {StatusLine
                , [HttpHeader]
                , HttpBodyResult}
              | { StatusCode
                , HttpBodyResult}
              | RequestId
              | saved_to_file,
      StatusLine :: { HttpVersion, StatusCode, string()},
      StatusCode  :: non_neg_integer(),
      HttpBodyResult :: uri_string:uri_string() | binary(),
      RequestId :: any().
request(Method, Request, HTTPOptions, Options, Profile)
  when is_atom(Profile) orelse is_pid(Profile) ->
    WithBody = lists:member(Method, ?WITH_BODY),
    WithoutBody = lists:member(Method, ?WITHOUT_BODY),
    case check_request(WithBody, WithoutBody, Request) of
        ok ->
            do_request(Method, Request,
                       HTTPOptions, Options, Profile);
        {error, _} = Error ->
            Error
    end.

do_request(Method, {Url, Headers}, HTTPOptions, Options, Profile) ->
    do_request(Method, {Url, Headers, [], []}, HTTPOptions, Options, Profile);
do_request(Method, {Url, Headers, ContentType, Body}, HTTPOptions, Options, Profile) ->
    case normalize_and_parse_url(Url) of
	{error, Reason, _} ->
	    {error, Reason};
	ParsedUrl ->
	    case header_parse(Headers) of
            {error, Reason} ->
                {error, Reason};
            ok ->
                handle_request(Method, Url,
                               ParsedUrl, Headers, ContentType, Body,
                               HTTPOptions, Options, Profile)
            end
    end.

%% Check combination of method and presence of body
check_request(false, false, _Request) ->
    {error, invalid_method};
check_request(_, true, {_URL, _Headers}) ->
    ok;
check_request(true, _, {_URL, _Headers, ContentType, Body})
  when is_list(ContentType)
       andalso (is_list(Body) orelse is_binary(Body)) ->
    ok;
check_request(true, _, {_URL, _Headers, ContentType, Body})
  when is_list(ContentType) andalso is_tuple(Body) ->
    check_body_gen(Body);
check_request(_, _, _Request) ->
    {error, invalid_request}.

%%
%% @doc Description: Cancels a HTTP-request.
%%
-spec cancel_request(RequestId) -> ok when
      RequestId :: any().
cancel_request(RequestId) ->
    cancel_request(RequestId, default_profile()).

%%
%% @doc Cancels an asynchronous HTTP request. Notice that this does not
%% guarantee that the request response is not delivered. Because it is
%% asynchronous, the request can already have been completed when the
%% cancellation arrives.
%%
-spec cancel_request(RequestId, Profile) -> ok when
      RequestId :: any(),
      Profile :: atom() | pid().
cancel_request(RequestId, Profile)
  when is_atom(Profile) orelse is_pid(Profile) ->
    httpc_manager:cancel_request(RequestId, profile_name(Profile)).
   

%% @doc Sets options to be used for subsequent requests.
%% @see set_options/2. Informs the httpc_manager of the new settings.
-spec set_options(Options) -> ok | {error, Reason} when
      Options :: [Option],
      Option :: {proxy, {Proxy, NoProxy}}
              | {https_proxy, {Proxy, NoProxy}}
              | {max_sessions, MaxSessions}
              | {max_keep_alive_length, MaxKeepAlive}
              | {keep_alive_timeout, KeepAliveTimeout}
              | {max_pipeline_length, MaxPipeline}
              | {pipeline_timeout, PipelineTimeout}
              | {cookies, CookieMode}
              | {ipfamily, IpFamily}
              | {ip, IpAddress}
              | {port, Port}
              | {socket_opts, SocketOpts}
              | {verbose, VerboseMode}
              | {unix_socket, UnixSocket},
      Proxy :: {HostName, Port},
      Port :: non_neg_integer(),
      NoProxy :: [DomainDesc | HostName | IpAddressDesc],
      MaxSessions :: integer(),
      MaxKeepAlive :: integer(),
      KeepAliveTimeout :: integer(),
      MaxPipeline :: integer(),
      PipelineTimeout :: integer(),
      CookieMode :: enabled | disabled | verify,
      IpFamily :: inet | inet6 | local | inet6fb4,
      IpAddressDesc :: uri_string:uri_string(),
      IpAddress :: inet:ip_address(),
      VerboseMode :: false | verbose | debug | trace,
      SocketOpts :: [SocketOpt],
      SocketOpt :: term(),
      UnixSocket :: file:name_all(),
      Reason :: term(),
      DomainDesc :: string(),
      HostName :: uri_string:uri_string().
set_options(Options) ->
    set_options(Options, default_profile()).

%% @doc Sets options to be used for subsequent requests.
-spec set_options(Options, Profile) -> ok | {error, Reason} when
      Options :: [Option],
      Option :: {proxy, {Proxy, NoProxy}}
              | {https_proxy, {Proxy, NoProxy}}
              | {max_sessions, MaxSessions}
              | {max_keep_alive_length, MaxKeepAlive}
              | {keep_alive_timeout, KeepAliveTimeout}
              | {max_pipeline_length, MaxPipeline}
              | {pipeline_timeout, PipelineTimeout}
              | {cookies, CookieMode}
              | {ipfamily, IpFamily}
              | {ip, IpAddress}
              | {port, Port}
              | {socket_opts, [SocketOpt]}
              | {verbose, VerboseMode}
              | {unix_socket, UnixSocket},
      Profile :: atom() | pid(),
      SocketOpt :: term(),
      Proxy :: {HostName, Port},
      Port :: non_neg_integer(),
      NoProxy :: [DomainDesc | HostName | IpAddressDesc],
      MaxSessions :: integer(),
      MaxKeepAlive :: integer(),
      KeepAliveTimeout :: integer(),
      MaxPipeline :: integer(),
      PipelineTimeout :: integer(),
      CookieMode :: enabled | disabled | verify,
      IpFamily :: inet | inet6 | local | inet6fb4,
      IpAddressDesc :: uri_string:uri_string(),
      IpAddress :: inet:ip_address(),
      VerboseMode :: false | verbose | debug | trace,
      UnixSocket :: string(),
      Reason :: term(),
      DomainDesc :: string(),
      HostName :: uri_string:uri_string().
set_options(Options, Profile) when is_atom(Profile) orelse is_pid(Profile) ->
    case validate_options(Options) of
	{ok, Opts} ->
	    httpc_manager:set_options(Opts, profile_name(Profile));
	{error, Reason} ->
	    {error, Reason}
    end.

-spec set_option(atom(), term()) -> ok | {error, term()}.
set_option(Key, Value) ->
    set_option(Key, Value, default_profile()).

-spec set_option(atom(), term(), atom()) -> ok | {error, term()}.
set_option(Key, Value, Profile) ->
    set_options([{Key, Value}], Profile).


-spec get_options() -> term().
get_options() ->
    record_info(fields, options).

%%
%% @doc Retrieves the options currently used by the client.
%%
-spec get_options(OptionItems) -> {ok, Values} | {error, Reason} when
      OptionItems :: all | [OptionItem],
      OptionItem :: proxy | https_proxy | max_sessions | keep_alive_timeout
                  | max_keep_alive_length | pipeline_timeout | max_pipeline_length | cookies
                  | ipfamily | ip | port | socket_opts | verbose | unix_socket,
      Values :: [{OptionItem, term()}],
      Reason :: term().
get_options(Options) ->
    get_options(Options, default_profile()).

%%
%% @doc Retrieves the options currently used by the client.
%%
-spec get_options(OptionItems, Profile) -> {ok, Values} | {error, Reason} when
      OptionItems :: all | [OptionItem],
      OptionItem :: proxy | https_proxy | max_sessions | keep_alive_timeout
                  | max_keep_alive_length | pipeline_timeout | max_pipeline_length | cookies
                  | ipfamily | ip | port | socket_opts | verbose | unix_socket,
      Values :: [{OptionItem, term()}],
      Profile :: atom() | pid(),
      Reason :: term().
get_options(all = _Options, Profile) ->
    get_options(get_options(), Profile);
get_options(Options, Profile) 
  when (is_list(Options) andalso 
	(is_atom(Profile) orelse is_pid(Profile))) ->
    case Options -- get_options() of
	[] ->
	    try 
		begin
		    {ok, httpc_manager:get_options(Options, 
						   profile_name(Profile))}
		end
	    catch
		exit:{noproc, _} ->
		    {error, inets_not_started}
	    end;
	InvalidGetOptions ->
	    {error, {invalid_options, InvalidGetOptions}}
    end.

get_option(Key) ->
    get_option(Key, default_profile()).

get_option(Key, Profile) ->
    case get_options([Key], Profile) of
	{ok, [{Key, Value}]} ->
	    {ok, Value};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------------
%% Default client ssl options to verify server
%%
%%  WildcardHostName=true  does wildcard matching on the hostname check
%%--------------------------------------------------------------------------
-spec ssl_verify_host_options(WildcardHostName) -> list() when
      WildcardHostName :: boolean().
ssl_verify_host_options(WildcardHostName) ->
    WildCard = case WildcardHostName of
                   true ->
                       Fun = public_key:pkix_verify_hostname_match_fun(https),
                       [{customize_hostname_check,[{match_fun, Fun}]}];
                   false ->
                       []
               end,
    [{verify, verify_peer}, {cacerts, public_key:cacerts_get()} | WildCard].


-spec store_cookies(SetCookieHeaders, Url) -> ok | {error, Reason} when
      SetCookieHeaders :: [HttpHeader],
      HttpHeader       :: { Field :: [byte()], Value :: binary() | iolist()},
      Url              :: term(),
      Reason           :: term().
store_cookies(SetCookieHeaders, Url) ->
    store_cookies(SetCookieHeaders, Url, default_profile()).

%%
%% @doc Saves the cookies defined in `SetCookieHeaders; in the client profile
%% cookie database. Call this function if option `cookies' is set to `verify'. If no
%% profile is specified, the default profile is used.
-spec store_cookies(SetCookieHeaders, Url, Profile) -> ok | {error, Reason} when
      SetCookieHeaders :: [HttpHeader],
      HttpHeader       :: { Field :: [byte()], Value :: binary() | iolist()},
      Url              :: term(),
      Profile          :: atom() | pid(),
      Reason           :: term().
store_cookies(SetCookieHeaders, Url, Profile) 
  when is_atom(Profile) orelse is_pid(Profile) ->
    case normalize_and_parse_url(Url) of
        {error, Bad, _} ->
            {error, {parse_failed, Bad}};
        URI ->
            Scheme = scheme_to_atom(maps:get(scheme, URI, undefined)),
            Host = maps:get(host, URI, ""),
            Port = maps:get(port, URI, default_port(Scheme)),
            Path = uri_string:recompose(#{path => maps:get(path, URI, "")}),
	    %% Since the Address part is not actually used
	    %% by the manager when storing cookies, we dont
	    %% care about ipv6-host-with-brackets.
	    Address     = {Host, Port}, 
	    ProfileName = profile_name(Profile),
	    Cookies     = httpc_cookie:cookies(SetCookieHeaders, Path, Host),
	    httpc_manager:store_cookies(Cookies, Address, ProfileName), 
	    ok
    end.

default_port(http) ->
    80;
default_port(https) ->
    443.


-spec cookie_header(Url) -> HttpHeader | {error, Reason} when
      Url        :: uri_string:uri_string(),
      HttpHeader :: { Field :: [byte()], Value :: binary() | iolist()},
      Reason     :: term().
cookie_header(Url) ->
    cookie_header(Url, default_profile()).

-spec cookie_header(Url, ProfileOrOpts) -> HttpHeader | {error, Reason} when
      Url        :: uri_string:uri_string(),
      HttpHeader :: { Field :: [byte()], Value :: binary() | iolist()},
      ProfileOrOpts :: Profile | Opts,
      Profile    :: atom() | pid(),
      Opts       :: [CookieHeaderOpt],
      CookieHeaderOpt :: {ipv6_host_with_brackets, boolean()},
      Reason     :: term().
cookie_header(Url, Profile) when is_atom(Profile) orelse is_pid(Profile) ->
    cookie_header(Url, [], Profile);
cookie_header(Url, Opts) when is_list(Opts) ->
    cookie_header(Url, Opts, default_profile()).

%%
%% @doc Returns the cookie header that would have been sent when making a
%% request to Url using profile Profile. If no profile is specified, the default
%% profile is used.
%%
-spec cookie_header(Url, Opts, Profile) -> HttpHeader | {error, Reason} when
      Url        :: uri_string:uri_string(),
      HttpHeader :: { Field :: [byte()], Value :: binary() | iolist()},
      Profile    :: atom() | pid(),
      Opts       :: [CookieHeaderOpt],
      CookieHeaderOpt :: {ipv6_host_with_brackets, boolean()},
      Reason     :: term().
cookie_header(Url, Opts, Profile)
  when (is_list(Opts) andalso (is_atom(Profile) orelse is_pid(Profile))) ->
    try 
	begin
	    httpc_manager:which_cookies(Url, Opts, profile_name(Profile))
	end
    catch 
	exit:{noproc, _} ->
	    {error, {not_started, Profile}}
    end.
    

-spec which_cookies() -> [CookieStores] when
      CookieStores :: {cookies, Cookies} | {session_cookies, Cookies},
      Cookies :: [term()].
which_cookies() ->
    which_cookies(default_profile()).

%%
%% @doc Produces a list of the entire cookie database. Intended for
%% debugging/testing purposes. If no profile is specified, the default profile
%% is used.
%%
-spec which_cookies(Profile) -> [CookieStores] when
      Profile :: atom() | pid(),
      CookieStores :: {cookies, Cookies} | {session_cookies, Cookies},
      Cookies :: [term()].
which_cookies(Profile) ->
    ?hcrt("which cookies", [{profile, Profile}]),
    try 
	begin
	    httpc_manager:which_cookies(profile_name(Profile))
	end
    catch 
	exit:{noproc, _} ->
	    {error, {not_started, Profile}}
    end.


-spec which_sessions() -> SessionInfo when
      SessionInfo :: {GoodSession, BadSessions, NonSessions},
      GoodSession :: [Session],
      BadSessions :: [term()],
      NonSessions :: [term()],
      Session :: term().
which_sessions() ->
    which_sessions(default_profile()).

%%
%% @doc This function is intended for debugging only. It produces a slightly
%% processed dump of the session database. The first list of the session
%% information tuple will contain session information on an internal format. The
%% last two lists of the session information tuple should always be empty if the
%% code is working as intended. If no profile is specified, the default profile
%% is used. The dumped sessions database is sorted into three groups
%% (Good-, Bad- and Non-sessions).
%%
-spec which_sessions(Profile) -> SessionInfo when
      Profile :: atom() | pid(),
      SessionInfo :: {GoodSession, BadSessions, NonSessions},
      GoodSession :: [Session],
      BadSessions :: [term()],
      NonSessions :: [term()],
      Session :: term().
which_sessions(Profile) ->
    try 
	begin
	    httpc_manager:which_sessions(profile_name(Profile))
	end
    catch 
	exit:{noproc, _} ->
	    {[], [], []}
    end.



%%
%% @doc Produces a list of miscellaneous information. Intended for debugging. If
%% no profile is specified, the default profile is used.
%%
-spec info() -> list() | {error, Reason} when
      Reason :: term().
info() ->
    info(default_profile()).

%%
%% @doc Produces a list of miscellaneous information. Intended for debugging. If
%% no profile is specified, the default profile is used.
%%
-spec info(Profile) -> list() | {error, Reason} when
      Reason :: term(),
      Profile :: atom() | pid().
info(Profile) ->
    try 
	begin
	    httpc_manager:info(profile_name(Profile))
	end
    catch 
	exit:{noproc, _} ->
	    {error, {not_started, Profile}}
    end.


%%
%% @doc Debug function. Resets (clears) the cookie database for the default profile
%%
-spec reset_cookies() -> Void when
      Void :: term().
reset_cookies() ->
    reset_cookies(default_profile()).

%%
%% @doc Debug function. Resets (clears) the cookie database for the specified
%% `Profile'. If no profile is specified the default profile is used.
%%
-spec reset_cookies(Profile) -> Void when
      Profile :: atom() | pid(),
      Void :: term().
reset_cookies(Profile) ->
    try 
	begin
	    httpc_manager:reset_cookies(profile_name(Profile))
	end
    catch 
	exit:{noproc, _} ->
	    {error, {not_started, Profile}}
    end.


%%
%% @doc Triggers the next message to be streamed, that is, the same behavior as
%% active ones for sockets.
%%
-spec stream_next(Pid) -> ok when
      Pid :: pid().
stream_next(Pid) ->
    httpc_handler:stream_next(Pid).


%%%========================================================================
%%% Behaviour callbacks
%%%========================================================================
start_standalone(PropList) ->
    case proplists:get_value(profile, PropList) of
	undefined ->
	    {error, no_profile};
	Profile ->
	    Dir = 
		proplists:get_value(data_dir, PropList, only_session_cookies),
	    httpc_manager:start_link(Profile, Dir, stand_alone)
    end.

start_service(Config) ->
    httpc_profile_sup:start_child(Config).

stop_service(Profile) when is_atom(Profile) ->
    httpc_profile_sup:stop_child(Profile);
stop_service(Pid) when is_pid(Pid) ->
    case service_info(Pid) of
	{ok, [{profile, Profile}]} ->
	    stop_service(Profile);
	Error ->
	    Error
    end.

services() ->
    [{httpc, Pid} || {_, Pid, _, _} <- 
			 supervisor:which_children(httpc_profile_sup)].
service_info(Pid) ->
    try [{ChildName, ChildPid} || 
	    {ChildName, ChildPid, _, _} <- 
		supervisor:which_children(httpc_profile_sup)] of
	Children ->
	    child_name2info(child_name(Pid, Children))
    catch
	exit:{noproc, _} ->
	    {error, service_not_available} 
    end.


%%%========================================================================
%%% Internal functions
%%%========================================================================
-spec normalize_and_parse_url(Characters) -> NormalizedURI when
      Characters    :: uri_string:uri_string(),
      NormalizedURI :: uri_string:uri_string() | uri_string:uri_map()
                     | uri_string:error().
normalize_and_parse_url(Url) ->
    uri_string:normalize(unicode:characters_to_list(Url), [return_map]).

handle_request(Method, Url, 
               URI,
	       Headers0, ContentType, Body0,
	       HTTPOptions0, Options0, Profile) ->

    Started     = http_util:timestamp(), 
    NewHeaders0 = [{http_util:to_lower(Key), Val} || {Key, Val} <- Headers0],

    try
	begin
	    {NewHeaders, Body} = 
		case Body0 of
		    {chunkify, ProcessBody, Acc} 
		      when is_function(ProcessBody, 1) ->
			NewHeaders1 = ensure_chunked_encoding(NewHeaders0), 
			Body1       = {mk_chunkify_fun(ProcessBody), Acc}, 
			{NewHeaders1, Body1};
		    {ProcessBody, _} 
		      when is_function(ProcessBody, 1) ->
			{NewHeaders0, Body0};
		    _ when is_list(Body0) orelse is_binary(Body0) ->
			{NewHeaders0, Body0};
		    _ ->
			throw({error, {bad_body, Body0}})
		end,

            HTTPOptions   = http_options(HTTPOptions0),
            Options       = request_options(Options0),
            Sync          = proplists:get_value(sync,   Options),
            Stream        = proplists:get_value(stream, Options),
            Receiver      = proplists:get_value(receiver, Options),
            SocketOpts    = proplists:get_value(socket_opts, Options),
	    UnixSocket    = proplists:get_value(unix_socket, Options),
            BracketedHost = proplists:get_value(ipv6_host_with_brackets,
                                                Options),

            Scheme        = scheme_to_atom(maps:get(scheme, URI, undefined)),
            Userinfo      = maps:get(userinfo, URI, ""),
            Host          = http_util:maybe_add_brackets(maps:get(host, URI, ""), BracketedHost),
            Port          = maps:get(port, URI, default_port(Scheme)),
            Host2         = http_request:normalize_host(Scheme, Host, Port),
            Path          = uri_string:recompose(#{path => maps:get(path, URI, "")}),
            Query         = add_question_mark(maps:get(query, URI, "")),
            HeadersRecord = header_record(NewHeaders, Host2, HTTPOptions),

	    Request = #request{from          = Receiver,
			       scheme        = Scheme,
			       address       = {Host, Port},
			       path          = Path,
			       pquery        = Query,
			       method        = Method,
			       headers       = HeadersRecord, 
			       content       = {ContentType, Body},
			       settings      = HTTPOptions, 
			       abs_uri       = Url,
			       userinfo      = Userinfo,
			       stream        = Stream, 
			       headers_as_is = headers_as_is(Headers0, Options),
			       socket_opts   = SocketOpts, 
			       started       = Started,
			       unix_socket   = UnixSocket,
			       ipv6_host_with_brackets = BracketedHost},
	    case httpc_manager:request(Request, profile_name(Profile)) of
		{ok, RequestId} ->
		    handle_answer(RequestId, Sync, Options);
		{error, Reason} ->
		    {error, Reason}
	    end
	end
    catch
	error:{noproc, _} ->
	    {error, {not_started, Profile}};
	throw:Error ->
	    Error
    end.


add_question_mark(<<>>) ->
    <<>>;
add_question_mark([]) ->
    [];
add_question_mark(Comp) when is_binary(Comp) ->
    <<$?, Comp/binary>>;
add_question_mark(Comp) when is_list(Comp) ->
    [$?|Comp].


scheme_to_atom("http") ->
    http;
scheme_to_atom("https") ->
    https;
scheme_to_atom(undefined) ->
    throw({error, {no_scheme}});
scheme_to_atom(Scheme) ->
    throw({error, {bad_scheme, Scheme}}).


ensure_chunked_encoding(Hdrs) ->
    Key = "transfer-encoding",
    lists:keystore(Key, 1, Hdrs, {Key, "chunked"}).


mk_chunkify_fun(ProcessBody) ->
    fun(eof_body) ->
	    eof;
       (Acc) ->
	    case ProcessBody(Acc) of
		eof ->
		    {ok, <<"0\r\n\r\n">>, eof_body};
		{ok, Data, NewAcc} ->
		    Chunk = [
			     integer_to_list(iolist_size(Data), 16), 
			     "\r\n",
			     Data,
			     "\r\n"],
		    {ok, Chunk, NewAcc}
	    end
    end.


handle_answer(RequestId, false, _) ->
    {ok, RequestId};
handle_answer(RequestId, true, Options) ->
    receive
	{http, {RequestId, saved_to_file}} ->
	    {ok, saved_to_file};
	{http, {RequestId, {_,_,_} = Result}} ->
	    return_answer(Options, Result);
	{http, {RequestId, {error, Reason}}} ->
	    {error, Reason}
    end.

return_answer(Options, {StatusLine, Headers, BinBody}) ->
    Body = maybe_format_body(BinBody, Options),
    case proplists:get_value(full_result, Options, true) of
	true ->
	    {ok, {StatusLine, Headers, Body}};
	false ->
	    {_, Status, _} = StatusLine,
	    {ok, {Status, Body}}
    end.

maybe_format_body(BinBody, Options) ->
    case proplists:get_value(body_format, Options, string) of
	string ->
	    binary_to_list(BinBody);
	_ ->
	    BinBody
    end.

-spec headers_as_is(HeaderRequest, OptionsRequest) -> HeaderRequest when
      HeaderRequest :: [{list(), list() | binary()}] | [tuple()],
      OptionsRequest :: [OptionRequest],
      OptionRequest :: {sync, boolean()}
                        | {stream, StreamTo}
                        | {body_format, BodyFormat}
                        | {full_result, boolean()}
                        | {headers_as_is, boolean()}
                        | {socket_opts, [SocketOpt]}
                        | {receiver, Receiver}
                        | {ipv6_host_with_brackets, boolean()},
      BodyFormat  :: string() | binary() | atom(),
      StreamTo :: none | self | {self, once} | file:name_all(),
      SocketOpt :: term(),
      Receiver :: pid()
                  | fun((term()) -> term())
                  | { ReceiverModule::atom()
                    , ReceiverFunction::atom()
                    , ReceiverArgs::list()}.
%% This options is a workaround for http servers that do not follow the 
%% http standard and have case sensitive header parsing. Should only be
%% used if there is no other way to communicate with the server or for
%% testing purpose.
headers_as_is(Headers, Options) ->
     case proplists:get_value(headers_as_is, Options, false) of
	 false ->
	     [];
	 true  ->
	     Headers
     end.

http_options(HttpOptions) ->
    HttpOptionsDefault = http_options_default(),
    http_options(HttpOptionsDefault, HttpOptions, #http_options{}).

http_options([], [], Acc) ->
    Acc;
http_options([], HttpOptions, Acc) ->
    Fun = fun(BadOption) ->
		    Report = io_lib:format("Invalid option ~p ignored ~n", 
					   [BadOption]),
		    error_logger:info_report(Report)
	  end,
    lists:foreach(Fun, HttpOptions),
    Acc;
http_options([{Tag, Default, Idx, Post} | Defaults], HttpOptions, Acc) ->
    case lists:keysearch(Tag, 1, HttpOptions) of
	{value, {Tag, Val0}} ->
	    case Post(Val0) of
		{ok, Val} ->
		    Acc2 = setelement(Idx, Acc, Val),
		    HttpOptions2 = lists:keydelete(Tag, 1, HttpOptions),
		    http_options(Defaults, HttpOptions2, Acc2);
		error ->
		    Report = io_lib:format("Invalid option ~p:~p ignored ~n", 
					   [Tag, Val0]),
		    error_logger:info_report(Report),
		    HttpOptions2 = lists:keydelete(Tag, 1, HttpOptions),
		    http_options(Defaults, HttpOptions2, Acc)
	    end;
	false ->
	    DefaultVal = 
		case Default of
		    {value, Val} ->
			Val;
		    {field, DefaultIdx} ->
			element(DefaultIdx, Acc)
		end,
	    Acc2 = setelement(Idx, Acc, DefaultVal),
	    http_options(Defaults, HttpOptions, Acc2)
    end.

http_options_default() ->
    VersionPost = 
	fun(Value) when is_atom(Value) ->
		{ok, http_util:to_upper(atom_to_list(Value))};
	   (Value) when is_list(Value) ->
		{ok, http_util:to_upper(Value)};
	   (_) ->
		error
	end,
    TimeoutPost = fun(Value) when is_integer(Value) andalso (Value >= 0) ->
			  {ok, Value};
		     (infinity = Value) ->
			  {ok, Value};
		     (_) ->
			  error
		  end,
    AutoRedirectPost =  boolfun(),

    SslPost = fun(Value) when is_list(Value) ->
                      {ok, {ssl, Value}};
                 ({ssl, SslOptions}) when is_list(SslOptions) ->
		      {ok, {ssl, SslOptions}};
                 %% backwards compat
		 ({essl, SslOptions}) when is_list(SslOptions) ->
		      {ok, {ssl, SslOptions}};
		 (_) ->
		      error
	      end,
    ProxyAuthPost = fun({User, Passwd} = Value) when is_list(User) andalso 
						     is_list(Passwd) ->
			    {ok, Value};
		       (_) ->
			    error
		    end,
    RelaxedPost =  boolfun(),

    ConnTimeoutPost = 
	fun(Value) when is_integer(Value) andalso (Value >= 0) ->
		{ok, Value};
	   (infinity = Value) ->
		{ok, Value};
	   (_) ->
		error
	end,

    SslOpts = ssl_verify_host_options(true),

    UrlDecodePost =  boolfun(),
    [
     {version,         {value, "HTTP/1.1"},            #http_options.version,         VersionPost}, 
     {timeout,         {value, ?HTTP_REQUEST_TIMEOUT}, #http_options.timeout,         TimeoutPost},
     {autoredirect,    {value, true},                  #http_options.autoredirect,    AutoRedirectPost},
     {ssl,             {value, {ssl, SslOpts}},        #http_options.ssl,             SslPost},
     {proxy_auth,      {value, undefined},             #http_options.proxy_auth,      ProxyAuthPost},
     {relaxed,         {value, false},                 #http_options.relaxed,         RelaxedPost},
     {url_encode,      {value, false},                 #http_options.url_encode,      UrlDecodePost},
     %% this field has to be *after* the timeout option (as that field is used for the default value)
     {connect_timeout, {field, #http_options.timeout}, #http_options.connect_timeout, ConnTimeoutPost}
    ].

-spec boolfun() -> fun(((true | false)) -> {ok, true | false}) | fun((term()) -> error).
boolfun() ->
    fun(Value) when (Value =:= true) orelse
		    (Value =:= false) ->
	    {ok, Value};
       (_) ->
	    error
    end.

request_options_defaults() ->
    VerifyBoolean = boolfun(),

    VerifySync = VerifyBoolean,

    VerifyStream = 
	fun(none = _Value) -> 
		ok;
	   (self = _Value) -> 
		ok;
	   ({self, once} = _Value) -> 
		ok;
	   (Value) when is_list(Value) -> 
		ok;
	   (_) -> 
		error
	end,

    VerifyBodyFormat = 
	fun(string = _Value) ->
		ok;
	   (binary = _Value) ->
		ok;
	   (_) ->
		error
	end,
    
    VerifyFullResult = VerifyBoolean,

    VerifyHeaderAsIs = VerifyBoolean,

    VerifyReceiver = 
	fun(Value) when is_pid(Value) ->
		ok;
	   ({M, F, A}) when (is_atom(M) andalso 
			     is_atom(F) andalso 
			     is_list(A)) ->
		ok;
	   (Value) when is_function(Value, 1) ->
		ok;
	   (_) ->
		error
	end,

    VerifySocketOpts = 
	fun([]) ->
		{ok, undefined};
	   (Value) when is_list(Value) ->
		ok;
	   (_) ->
		error
	end,

    VerifyBrackets = VerifyBoolean,

    [
     {sync,                    true,      VerifySync}, 
     {stream,                  none,      VerifyStream},
     {body_format,             string,    VerifyBodyFormat},
     {full_result,             true,      VerifyFullResult},
     {headers_as_is,           false,     VerifyHeaderAsIs},
     {receiver,                self(),    VerifyReceiver},
     {socket_opts,             undefined, VerifySocketOpts},
     {ipv6_host_with_brackets, false,     VerifyBrackets}
    ]. 

request_options(Options) ->
    Defaults = request_options_defaults(), 
    request_options(Defaults, Options, []).

request_options([], [], Acc) ->
    request_options_sanity_check(Acc),
    lists:reverse(Acc);
request_options([], Options, Acc) ->
    Fun = fun(BadOption) ->
		    Report = io_lib:format("Invalid option ~p ignored ~n", 
					   [BadOption]),
		    error_logger:info_report(Report)
	  end,
    lists:foreach(Fun, Options),
    Acc;
request_options([{Key, DefaultVal, Verify} | Defaults], Options, Acc) ->
    case lists:keysearch(Key, 1, Options) of
	{value, {Key, Value}} ->
	    case Verify(Value) of
		ok ->
		    Options2 = lists:keydelete(Key, 1, Options),
		    request_options(Defaults, Options2, [{Key, Value} | Acc]);
		{ok, Value2} ->
		    Options2 = lists:keydelete(Key, 1, Options),
		    request_options(Defaults, Options2, [{Key, Value2} | Acc]);
		error ->
		    Report = io_lib:format("Invalid option ~p:~p ignored ~n", 
					   [Key, Value]),
		    error_logger:info_report(Report),
		    Options2 = lists:keydelete(Key, 1, Options),
		    request_options(Defaults, Options2, Acc)
	    end;
	false ->
	    request_options(Defaults, Options, [{Key, DefaultVal} | Acc])
    end.

-spec request_options_sanity_check([OptionRequest]) -> ok | no_return() when
      OptionRequest :: {sync, boolean()}
                     | {stream, StreamTo}
                     | {body_format, BodyFormat}
                     | {full_result, boolean()}
                     | {headers_as_is, boolean()}
                     | {socket_opts, [SocketOpt]}
                     | {receiver, Receiver}
                     | {ipv6_host_with_brackets, boolean()},
      StreamTo :: none | self | {self, once} | file:name_all(),
      BodyFormat  :: string() | binary() | atom(),
      SocketOpt :: term(),
      Receiver :: pid()
                  | fun((term()) -> term())
                  | { ReceiverModule::atom()
                    , ReceiverFunction::atom()
                    , ReceiverArgs::list()}.
request_options_sanity_check(Opts) ->
    case proplists:get_value(sync, Opts) of
	Sync when (Sync =:= true) ->
	    case proplists:get_value(receiver, Opts) of
		Pid when is_pid(Pid) andalso (Pid =:= self()) ->
		    ok;
		BadReceiver ->
		    throw({error, {bad_options_combo, 
				   [{sync, true}, {receiver, BadReceiver}]}})
	    end,
	    case proplists:get_value(stream, Opts) of
		Stream when (Stream =:= self) orelse 
			    (Stream =:= {self, once}) ->
		    throw({error, streaming_error});
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end,
    ok.

validate_ipfamily_unix_socket(Options0) ->
    IpFamily = proplists:get_value(ipfamily, Options0, inet),
    UnixSocket = proplists:get_value(unix_socket, Options0, undefined),
    Options1 = proplists:delete(ipfamily, Options0),
    Options2 = proplists:delete(ipfamily, Options1),
    validate_ipfamily_unix_socket(IpFamily, UnixSocket, Options2,
                                  [{ipfamily, IpFamily}, {unix_socket, UnixSocket}]).
%%
validate_ipfamily_unix_socket(local, undefined, _Options, _Acc) ->
    bad_option(unix_socket, undefined);
validate_ipfamily_unix_socket(IpFamily, UnixSocket, _Options, _Acc)
  when IpFamily =/= local, UnixSocket =/= undefined ->
    bad_option(ipfamily, IpFamily);
validate_ipfamily_unix_socket(IpFamily, UnixSocket, Options, Acc) ->
    validate_ipfamily(IpFamily),
    validate_unix_socket(UnixSocket),
    {Options, Acc}.

validate_options(Options0) ->
    try
        {Options, Acc} = validate_ipfamily_unix_socket(Options0),
        validate_options(Options, Acc)
    catch
        error:Reason ->
            {error, Reason}
    end.
%%
validate_options([], ValidOptions) ->
    {ok, lists:reverse(ValidOptions)};

validate_options([{proxy, Proxy} = Opt| Tail], Acc) ->
    validate_proxy(Proxy),
    validate_options(Tail, [Opt | Acc]);

validate_options([{https_proxy, Proxy} = Opt| Tail], Acc) ->
    validate_https_proxy(Proxy),
    validate_options(Tail, [Opt | Acc]);

validate_options([{max_sessions, Value} = Opt| Tail], Acc) ->
    validate_max_sessions(Value),
    validate_options(Tail, [Opt | Acc]);

validate_options([{keep_alive_timeout, Value} = Opt| Tail], Acc) ->
    validate_keep_alive_timeout(Value),
    validate_options(Tail, [Opt | Acc]);

validate_options([{max_keep_alive_length, Value} = Opt| Tail], Acc) ->
    validate_max_keep_alive_length(Value),
    validate_options(Tail, [Opt | Acc]);

validate_options([{pipeline_timeout, Value} = Opt| Tail], Acc) ->
    validate_pipeline_timeout(Value),
    validate_options(Tail, [Opt | Acc]);

validate_options([{max_pipeline_length, Value} = Opt| Tail], Acc) ->
    validate_max_pipeline_length(Value), 
    validate_options(Tail, [Opt | Acc]);

validate_options([{cookies, Value} = Opt| Tail], Acc) ->
    validate_cookies(Value),
    validate_options(Tail, [Opt | Acc]);

validate_options([{ipfamily, Value} = Opt| Tail], Acc) ->
    validate_ipfamily(Value), 
    validate_options(Tail, [Opt | Acc]);

%% For backward compatibillity
validate_options([{ipv6, Value}| Tail], Acc) ->
    NewValue = validate_ipv6(Value), 
    Opt = {ipfamily, NewValue},
    validate_options(Tail, [Opt | Acc]);

validate_options([{ip, Value} = Opt| Tail], Acc) ->
    validate_ip(Value),
    validate_options(Tail, [Opt | Acc]);

validate_options([{port, Value} = Opt| Tail], Acc) ->
    validate_port(Value), 
    validate_options(Tail, [Opt | Acc]);

validate_options([{socket_opts, Value} = Opt| Tail], Acc) ->
    validate_socket_opts(Value), 
    validate_options(Tail, [Opt | Acc]);

validate_options([{verbose, Value} = Opt| Tail], Acc) ->
    validate_verbose(Value), 
    validate_options(Tail, [Opt | Acc]);

validate_options([{unix_socket, Value} = Opt| Tail], Acc) ->
    validate_unix_socket(Value),
    validate_options(Tail, [Opt | Acc]);

validate_options([{_, _} = Opt| _], _Acc) ->
    {error, {not_an_option, Opt}}.


validate_proxy({{ProxyHost, ProxyPort}, NoProxy} = Proxy)
  when is_list(ProxyHost) andalso 
       is_integer(ProxyPort) andalso 
       is_list(NoProxy) ->
    Proxy;
validate_proxy(BadProxy) ->
    bad_option(proxy, BadProxy).

validate_https_proxy({{ProxyHost, ProxyPort}, NoProxy} = Proxy) 
  when is_list(ProxyHost) andalso 
       is_integer(ProxyPort) andalso 
       is_list(NoProxy) ->
    Proxy;
validate_https_proxy(BadProxy) ->
    bad_option(https_proxy, BadProxy).

validate_max_sessions(Value) when is_integer(Value) andalso (Value >= 0) ->
    Value;
validate_max_sessions(BadValue) ->
    bad_option(max_sessions, BadValue).

validate_keep_alive_timeout(Value) when is_integer(Value) andalso (Value >= 0) ->
    Value;
validate_keep_alive_timeout(infinity = Value) ->
    Value;
validate_keep_alive_timeout(BadValue) ->
    bad_option(keep_alive_timeout, BadValue).

validate_max_keep_alive_length(Value) when is_integer(Value) andalso (Value >= 0) ->
    Value;
validate_max_keep_alive_length(BadValue) ->
    bad_option(max_keep_alive_length, BadValue).

validate_pipeline_timeout(Value) when is_integer(Value) ->
    Value;
validate_pipeline_timeout(infinity = Value) ->
    Value;
validate_pipeline_timeout(BadValue) ->
    bad_option(pipeline_timeout, BadValue).

validate_max_pipeline_length(Value) when is_integer(Value) ->
    Value;
validate_max_pipeline_length(BadValue) ->
    bad_option(max_pipeline_length, BadValue).

validate_cookies(Value) 
  when ((Value =:= enabled)  orelse 
	(Value =:= disabled) orelse 
	(Value =:= verify)) ->
    Value;
validate_cookies(BadValue) ->
    bad_option(cookies, BadValue).

validate_ipv6(Value) when (Value =:= enabled) orelse (Value =:= disabled) ->
    case Value of
	enabled ->
	    inet6fb4;
	disabled ->
	    inet
    end;  
validate_ipv6(BadValue) ->
    bad_option(ipv6, BadValue).

validate_ipfamily(Value) 
  when (Value =:= inet) orelse (Value =:= inet6) orelse
       (Value =:= inet6fb4) orelse (Value =:= local) ->
    Value;
validate_ipfamily(BadValue) ->
    bad_option(ipfamily, BadValue).

validate_ip(Value) 
  when tuple_size(Value) =:= 4; tuple_size(Value) =:= 8 ->
    Value;
validate_ip(BadValue) ->
    bad_option(ip, BadValue).
    
validate_port(Value) when is_integer(Value) ->
    Value;
validate_port(BadValue) ->
    bad_option(port, BadValue).

validate_socket_opts(Value) when is_list(Value) ->
    Value;
validate_socket_opts(BadValue) ->
    bad_option(socket_opts, BadValue).

validate_verbose(Value) 
  when ((Value =:= false) orelse 
	(Value =:= verbose) orelse 
	(Value =:= debug) orelse 
	(Value =:= trace)) ->
    ok;
validate_verbose(BadValue) ->
    bad_option(verbose, BadValue).

validate_unix_socket(Value)
  when (Value =:= undefined) ->
    Value;
validate_unix_socket(Value)
  when is_list(Value) andalso length(Value) > 0 ->
    Value;
validate_unix_socket(BadValue) ->
    bad_option(unix_socket, BadValue).

bad_option(Option, BadValue) ->
    throw({error, {bad_option, Option, BadValue}}).


header_record(NewHeaders, Host, #http_options{version = Version}) ->
    header_record(NewHeaders, #http_request_h{}, Host, Version).

header_record([], RequestHeaders, Host, Version) ->
    validate_headers(RequestHeaders, Host, Version);
header_record([{"cache-control", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'cache-control' = Val},
		  Host, Version);  
header_record([{"connection", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{connection = Val}, Host,
		 Version);
header_record([{"date", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{date = Val}, Host, 
		  Version);  
header_record([{"pragma", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{pragma = Val}, Host,
		  Version);  
header_record([{"trailer", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{trailer = Val}, Host,
		  Version);  
header_record([{"transfer-encoding", Val} | Rest], RequestHeaders, Host, 
	      Version) ->
    header_record(Rest, 
		  RequestHeaders#http_request_h{'transfer-encoding' = Val},
		  Host, Version);  
header_record([{"upgrade", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{upgrade = Val}, Host,
		  Version);  
header_record([{"via", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{via = Val}, Host, 
		  Version);  
header_record([{"warning", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{warning = Val}, Host,
		  Version);  
header_record([{"accept", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{accept = Val}, Host,
		  Version);  
header_record([{"accept-charset", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'accept-charset' = Val}, 
		  Host, Version);  
header_record([{"accept-encoding", Val} | Rest], RequestHeaders, Host, 
	      Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'accept-encoding' = Val},
		  Host, Version);  
header_record([{"accept-language", Val} | Rest], RequestHeaders, Host, 
	      Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'accept-language' = Val},
		  Host, Version);  
header_record([{"authorization", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{authorization = Val}, 
		  Host, Version);  
header_record([{"expect", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{expect = Val}, Host,
		  Version);
header_record([{"from", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{from = Val}, Host, 
		  Version);  
header_record([{"host", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{host = Val}, Host, 
		  Version);
header_record([{"if-match", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'if-match' = Val},
		  Host, Version);  
header_record([{"if-modified-since", Val} | Rest], RequestHeaders, Host, 
	      Version) ->
    header_record(Rest, 
		  RequestHeaders#http_request_h{'if-modified-since' = Val},
		  Host, Version);  
header_record([{"if-none-match", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'if-none-match' = Val}, 
		  Host, Version);  
header_record([{"if-range", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'if-range' = Val}, 
		  Host, Version);  

header_record([{"if-unmodified-since", Val} | Rest], RequestHeaders, Host, 
	      Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'if-unmodified-since' 
						      = Val}, Host, Version);  
header_record([{"max-forwards", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'max-forwards' = Val}, 
		  Host, Version);  
header_record([{"proxy-authorization", Val} | Rest], RequestHeaders, Host, 
	      Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'proxy-authorization' 
						      = Val}, Host, Version);  
header_record([{"range", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{range = Val}, Host, 
		  Version);  
header_record([{"referer", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{referer = Val}, Host, 
		  Version);  
header_record([{"te", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{te = Val}, Host, 
		  Version);  
header_record([{"user-agent", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'user-agent' = Val}, 
		  Host, Version);  
header_record([{"allow", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{allow = Val}, Host, 
		  Version);  
header_record([{"content-encoding", Val} | Rest], RequestHeaders, Host, 
	      Version) ->
    header_record(Rest, 
		  RequestHeaders#http_request_h{'content-encoding' = Val},
		  Host, Version);  
header_record([{"content-language", Val} | Rest], RequestHeaders, 
	      Host, Version) ->
    header_record(Rest, 
		  RequestHeaders#http_request_h{'content-language' = Val}, 
		  Host, Version);  
header_record([{"content-length", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'content-length' = Val},
		  Host, Version);  
header_record([{"content-location", Val} | Rest], RequestHeaders, 
	      Host, Version) ->
    header_record(Rest, 
		  RequestHeaders#http_request_h{'content-location' = Val},
		  Host, Version);  
header_record([{"content-md5", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'content-md5' = Val}, 
		  Host, Version);  
header_record([{"content-range", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'content-range' = Val},
		  Host, Version);  
header_record([{"content-type", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'content-type' = Val}, 
		  Host, Version);  
header_record([{"expires", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{expires = Val}, Host, 
		  Version);  
header_record([{"last-modified", Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{'last-modified' = Val},
		  Host, Version);  
header_record([{Key, Val} | Rest], RequestHeaders, Host, Version) ->
    header_record(Rest, RequestHeaders#http_request_h{
			  other = [{Key, Val} |
				   RequestHeaders#http_request_h.other]}, 
		  Host, Version).

validate_headers(RequestHeaders = #http_request_h{te = undefined}, Host, 
		 "HTTP/1.1" = Version) ->
    validate_headers(RequestHeaders#http_request_h{te = ""}, Host, 
		     "HTTP/1.1" = Version);
validate_headers(RequestHeaders = #http_request_h{host = undefined}, 
		 Host, "HTTP/1.1" = Version) ->
    validate_headers(RequestHeaders#http_request_h{host = Host}, Host, Version);
validate_headers(RequestHeaders, _, _) ->
    RequestHeaders.

%%--------------------------------------------------------------------------
%% These functions are just simple wrappers to parse specifically HTTP URIs
%%--------------------------------------------------------------------------

-spec header_parse(HeaderRequest) -> ok | InvalidHeaderParsed when
      HeaderRequest       :: [{list(), list() | binary()}] | [tuple()],
      InvalidHeaderParsed :: {error, {headers_error, invalid_field | invalid_value}}.
header_parse([]) ->
    ok;
header_parse([{Field, Value}|T])
  when is_list(Field)
       andalso (is_list(Value) orelse is_binary(Value)) ->
    header_parse(T);
header_parse([{Field, _Value}| _ ])
  when not is_list(Field) ->
    {error, {headers_error, invalid_field}};
header_parse([{_, _}| _]) ->
    {error, {headers_error, invalid_value}}.

child_name2info(undefined) ->
    {error, no_such_service};
child_name2info(httpc_manager) ->
    {ok, [{profile, default}]};
child_name2info({httpc, Profile}) ->
    {ok, [{profile, Profile}]}.

child_name(_, []) ->
    undefined;
child_name(Pid, [{Name, Pid} | _]) ->
    Name;
child_name(Pid, [_ | Children]) ->
    child_name(Pid, Children).


check_body_gen({Fun, _}) when is_function(Fun, 1) ->
    ok;
check_body_gen({chunkify, Fun, _}) when is_function(Fun, 1) ->
    ok;
check_body_gen(Gen) ->
    {error, {bad_body_generator, Gen}}.
