%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Handles ssl sessions
%%----------------------------------------------------------------------

-module(ssl_session).

-include("ssl_handshake.hrl").
-include("ssl_internal.hrl").
-include("ssl_api.hrl").

%% Internal application API
-export([is_new/2, client_select_session/4, server_select_session/7, valid_session/2]).

-type seconds()   :: integer(). 

%%--------------------------------------------------------------------
-spec is_new(ssl:session_id(), ssl:session_id()) -> boolean().
%%
%% Description: Checks if the session id decided by the server is a
%%              new or resumed sesion id.
%%--------------------------------------------------------------------
is_new(<<>>, _) ->
    true;
is_new(SessionId, SessionId) ->
    false;
is_new(_ClientSuggestion, _ServerDecision) ->
    true.

%%--------------------------------------------------------------------
-spec client_select_session({ssl:host(), inet:port_number(), map()}, db_handle(), atom(),
	 #session{}) -> #session{}.
%%
%% Description: Should be called by the client side to get an id
%%              for the client hello message.
%%--------------------------------------------------------------------
client_select_session({_, _, #{versions := Versions,
                               protocol := Protocol}} = ClientInfo, 
                      Cache, CacheCb, NewSession) ->
    
    RecordCb = record_cb(Protocol),
    Version = RecordCb:lowest_protocol_version(Versions),
    
    case Version of
        {3, N} when N >= 4 ->
          NewSession#session{session_id = crypto:strong_rand_bytes(32)};
        _ ->
            do_client_select_session(ClientInfo, Cache, CacheCb, NewSession)  
    end.  

%%--------------------------------------------------------------------
-spec server_select_session(ssl_record:ssl_version(), inet:port_number(), binary(), map(), 
                            binary(),db_handle(), atom())  -> {binary(), #session{} | undefined}. 
%%
%% Description: Should be called by the server side to get an id
%%              for the client hello message.
%%--------------------------------------------------------------------
server_select_session({_, Minor}, Port, <<>>, _SslOpts, _Cert, _, _) when Minor >= 4 ->
    {ssl_manager:new_session_id(Port), undefined};
server_select_session(_, Port, <<>>, _SslOpts, _Cert, _, _) ->
    {ssl_manager:new_session_id(Port), undefined};
server_select_session(_, Port, SuggestedId, Options, Cert, Cache, CacheCb) ->
    LifeTime = case application:get_env(ssl, session_lifetime) of
		   {ok, Time} when is_integer(Time) -> Time;
		   _ -> ?'24H_in_sec'
	       end,
    case is_resumable(SuggestedId, Port, Options,
		      Cache, CacheCb, LifeTime, Cert)
    of
	{true, Resumed} ->
	    {SuggestedId, Resumed};
	{false, undefined} ->
	    {ssl_manager:new_session_id(Port), undefined}
    end.

-spec valid_session(#session{}, seconds() | {invalidate_before, integer()}) -> boolean().
%%
%% Description: Check that the session has not expired
%%--------------------------------------------------------------------
valid_session(#session{time_stamp = TimeStamp}, {invalidate_before, Before}) ->
    TimeStamp > Before;
valid_session(#session{time_stamp = TimeStamp}, LifeTime) ->
    Now = erlang:monotonic_time(),
    Lived = erlang:convert_time_unit(Now-TimeStamp, native, seconds),
    Lived < LifeTime.
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

do_client_select_session({Host, Port, #{reuse_session := SessionId}}, Cache, CacheCb, NewSession) when is_binary(SessionId)->
    case CacheCb:lookup(Cache, {{Host, Port}, SessionId}) of
        undefined ->
	    NewSession#session{session_id = <<>>};
	#session{} = Session->
	    Session
    end;
do_client_select_session(ClientInfo, 
                      Cache, CacheCb, #session{own_certificate = OwnCert} = NewSession) ->
    case select_session(ClientInfo, Cache, CacheCb, OwnCert) of
        no_session ->
            NewSession#session{session_id = <<>>};
        Session ->
            Session
    end.

select_session({_, _, #{reuse_sessions := Reuse}}, _Cache, _CacheCb, _OwnCert) when Reuse =/= true ->
    %% If reuse_sessions == false | save a new session should be created
    no_session;
select_session({HostIP, Port, SslOpts}, Cache, CacheCb, OwnCert) ->
    Sessions = CacheCb:select_session(Cache, {HostIP, Port}),
    select_session(Sessions, SslOpts, OwnCert).

select_session([], _, _) ->
    no_session;
select_session(Sessions, #{ciphers := Ciphers}, OwnCert) ->
    IsNotResumable =
	fun(Session) ->
		not (resumable(Session#session.is_resumable) andalso
		     lists:member(Session#session.cipher_suite, Ciphers)
		     andalso (OwnCert == Session#session.own_certificate))
 	end,
    case lists:dropwhile(IsNotResumable, Sessions) of
	[] ->   no_session;
	[Session | _] -> Session
    end.

is_resumable(_, _, #{reuse_sessions := false}, _, _, _, _) ->
    {false, undefined};
is_resumable(SuggestedSessionId, Port, #{reuse_session := ReuseFun} = Options, Cache,
	     CacheCb, SecondLifeTime, OwnCert) ->
    case CacheCb:lookup(Cache, {Port, SuggestedSessionId}) of
	#session{cipher_suite = CipherSuite,
		 own_certificate = SessionOwnCert,
		 compression_method = Compression,
		 is_resumable = IsResumable,
		 peer_certificate = PeerCert} = Session ->
	    case resumable(IsResumable)
		andalso (OwnCert == SessionOwnCert)
		andalso valid_session(Session, SecondLifeTime)
		andalso reusable_options(Options, Session)
		andalso ReuseFun(SuggestedSessionId, PeerCert,
				 Compression, CipherSuite)
	    of
		true  -> {true, Session};
		false -> {false, undefined}
	    end;
	undefined ->
 	    {false, undefined}
    end.

resumable(new) ->
    false;
resumable(IsResumable) ->
    IsResumable.

reusable_options(#{fail_if_no_peer_cert := true,
                   verify := verify_peer}, Session) ->
    (Session#session.peer_certificate =/= undefined);
reusable_options(_,_) ->
    true.

record_cb(tls) ->
    tls_record;
record_cb(dtls) ->
    dtls_record.
