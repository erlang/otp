%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Handles ssl sessions
%%----------------------------------------------------------------------

-module(ssl_session).

-include("ssl_handshake.hrl").
-include("ssl_internal.hrl").

%% Internal application API
-export([is_new/2, id/3, id/6, valid_session/2]).

-define(GEN_UNIQUE_ID_MAX_TRIES, 10).

-type seconds()   :: integer(). 

%%--------------------------------------------------------------------
-spec is_new(session_id(), session_id()) -> boolean().
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
-spec id({host(), port_num(), #ssl_options{}}, cache_ref(), atom()) -> binary().
%%
%% Description: Should be called by the client side to get an id 
%%              for the client hello message.
%%--------------------------------------------------------------------
id(ClientInfo, Cache, CacheCb) ->
    case select_session(ClientInfo, Cache, CacheCb) of
	no_session ->
	    <<>>;
	SessionId ->
	    SessionId
    end.

%%--------------------------------------------------------------------
-spec id(port_num(), binary(), #ssl_options{}, cache_ref(), 
	 atom(), seconds()) -> binary().
%%
%% Description: Should be called by the server side to get an id 
%%              for the server hello message.
%%--------------------------------------------------------------------
id(Port, <<>>, _, Cache, CacheCb, _) ->
    new_id(Port, ?GEN_UNIQUE_ID_MAX_TRIES, Cache, CacheCb);

id(Port, SuggestedSessionId, #ssl_options{reuse_sessions = ReuseEnabled,
					  reuse_session = ReuseFun}, 
   Cache, CacheCb, SecondLifeTime) ->
    case is_resumable(SuggestedSessionId, Port, ReuseEnabled, 
		      ReuseFun, Cache, CacheCb, SecondLifeTime) of
	true ->
	    SuggestedSessionId;
	false ->
	    new_id(Port, ?GEN_UNIQUE_ID_MAX_TRIES, Cache, CacheCb)
    end.
%%--------------------------------------------------------------------
-spec valid_session(#session{}, seconds()) -> boolean().
%%
%% Description: Check that the session has not expired
%%--------------------------------------------------------------------
valid_session(#session{time_stamp = TimeStamp}, LifeTime) ->
    Now =  calendar:datetime_to_gregorian_seconds({date(), time()}),
    Now - TimeStamp < LifeTime.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
select_session({HostIP, Port, SslOpts}, Cache, CacheCb) ->    
    Sessions = CacheCb:select_session(Cache, {HostIP, Port}),
    select_session(Sessions, SslOpts).

select_session([], _) ->
    no_session;

select_session(Sessions, #ssl_options{ciphers = Ciphers,
				      reuse_sessions = ReuseSession}) ->
    IsResumable = 
 	fun(Session) -> 
 		ReuseSession andalso (Session#session.is_resumable) andalso  
 		    lists:member(Session#session.cipher_suite, Ciphers)
 	end,
    case [Id || [Id, Session] <- Sessions, IsResumable(Session)] of
 	[] ->
 	    no_session;
 	List ->
 	    hd(List)
    end.

%% If we can not generate a not allready in use session ID in
%% ?GEN_UNIQUE_ID_MAX_TRIES we make the new session uncacheable The
%% value of ?GEN_UNIQUE_ID_MAX_TRIES is stolen from open SSL which
%% states : "If we can not find a session id in
%% ?GEN_UNIQUE_ID_MAX_TRIES either the RAND code is broken or someone
%% is trying to open roughly very close to 2^128 (or 2^256) SSL
%% sessions to our server"
new_id(_, 0, _, _) ->
    <<>>;
new_id(Port, Tries, Cache, CacheCb) ->
    Id = crypto:rand_bytes(?NUM_OF_SESSION_ID_BYTES),
    case CacheCb:lookup(Cache, {Port, Id}) of
	undefined ->
	    Now =  calendar:datetime_to_gregorian_seconds({date(), time()}),
	    %% New sessions can not be set to resumable
	    %% until handshake is compleate and the
	    %% other session values are set.
	    CacheCb:update(Cache, {Port, Id}, #session{session_id = Id,
						       is_resumable = false,
						       time_stamp = Now}),
	    Id;
	_ ->
	    new_id(Port, Tries - 1, Cache, CacheCb)
    end.

is_resumable(SuggestedSessionId, Port, ReuseEnabled, ReuseFun, Cache, 
	     CacheCb, SecondLifeTime) ->
    case CacheCb:lookup(Cache, {Port, SuggestedSessionId}) of
	#session{cipher_suite = CipherSuite,
		 compression_method = Compression,
		 is_resumable = Is_resumable,
		 peer_certificate = PeerCert} = Session ->
	    ReuseEnabled 
		andalso Is_resumable  
		andalso valid_session(Session, SecondLifeTime) 
		andalso ReuseFun(SuggestedSessionId, PeerCert, 
				 Compression, CipherSuite);
	undefined ->
	    false
    end.
