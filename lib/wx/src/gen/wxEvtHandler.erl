%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

%% This module is actually handwritten see ../api_gen/wx_extra/wxEvtHandler.erl
%%
%% @doc The Event handler.
%%
%% To get events from wxwidgets objects you subscribe to them by
%% calling connect/[2-3].  Events are sent as messages, if no callback
%% was supplied  These messages will be {@link wx(). #wx{}} where
%% EventRecord is a record that depends on the {@link
%% wxEventType(). event type}.  The records are defined in:
%% wx/include/wx.hrl.
%%
%% If a callback was supplied to connect, the callback will be invoked
%% (in another process) to handle the event. The callback should be of
%% arity 2.  fun(EventRecord::wx(), EventObject::wxObject()).
%%
%% Beware that the callback will be in executed in new process each time.
%%
%% <a href="http://www.wxwidgets.org/manuals/stable/wx_wxevthandler.html">
%% The orginal documentation</a>. 
%%
%% @headerfile "../../include/wx.hrl"
%%
%%@type wxEvtHandler().  An object reference

-module(wxEvtHandler).
-include("wxe.hrl").
-include("../include/wx.hrl").

%% API
-export([connect/2, connect/3, disconnect/1, disconnect/2, disconnect/3]).

%% internal exports
-export([connect_impl/3, disconnect_impl/2, disconnect_impl/3, 
	 new_evt_listener/0, destroy_evt_listener/1, 
	 get_callback/1, replace_fun_with_id/2]).

-record(evh, {et=null,id=?wxID_ANY,lastId=?wxID_ANY,skip=undefined,userdata=[],cb=0}).



%% @spec (This::wxEvtHandler(), EventType::wxEventType()) -> ok
%% @doc Equivalent to {@link connect/3. connect(This, EventType, [])}

connect(This, EventType) ->
    connect(This, EventType, []).

%% @spec (This::wxEvtHandler(), EventType::wxEventType(), [Options]) -> ok
%% @doc This function subscribes the to events of EventType,
%% in the range id, lastId. The events will be received as messages 
%% if no callback is supplied.
%%
%% Options: 
%%    {id, integer()},      The identifier (or first of the identifier range) to be 
%%                            associated with this event handler. 
%%                          Default ?wxID_ANY
%%    {lastId, integer()},  The second part of the identifier range. 
%%                          If used 'id' must be set as the starting identifier range.
%%                          Default ?wxID_ANY
%%    {skip,  boolean()},   If skip is true further event_handlers will be called.
%%                          This is not used if the 'callback' option is used. 
%%                          Default false.
%%    {callback, function()} Use a callback fun(EventRecord::wx(), EventObject::wxObject()) 
%%                          to process the event. Default not specfied i.e. a message will
%%                          be delivered to the process calling this function.
%%    {userData, term()}    An erlang term that will be sent with the event. Default: [].
connect(This=#wx_ref{type=ThisT}, EventType, Options) ->
    EvH = parse_opts(Options, #evh{et=EventType}),
    ?CLASS(ThisT,wxEvtHandler),
    case wxe_util:connect_cb(This, EvH) of
	ok -> ok;
	{badarg, event_type} ->
	    erlang:error({badarg,EventType})
    end.

parse_opts([{callback,Fun}|R], Opts) when is_function(Fun) ->
    %% Check Fun Arity?
    parse_opts(R, Opts#evh{cb=Fun});
parse_opts([callback|R], Opts) ->
    parse_opts(R, Opts#evh{cb=1});
parse_opts([{userData, UserData}|R],Opts) ->
    parse_opts(R, Opts#evh{userdata=UserData});
parse_opts([{skip, Skip}|R],Opts) when is_boolean(Skip) ->
    parse_opts(R, Opts#evh{skip=Skip});
parse_opts([{id, Id}|R],Opts) when is_integer(Id) ->
    parse_opts(R, Opts#evh{id=Id});
parse_opts([{lastId, Id}|R],Opts) when is_integer(Id) ->
    parse_opts(R, Opts#evh{lastId=Id});
parse_opts([_BadArg|R], Opts) ->
    parse_opts(R, Opts);
parse_opts([], Opts = #evh{id=Id,lastId=Lid,skip=Skip, cb=CB}) -> 
    if 
	Skip =/= undefined andalso CB =/= 0 -> 
	    erlang:error({badarg, {combined, skip, callback}});
	Lid =/= ?wxID_ANY andalso Id =:= ?wxID_ANY  ->
	    erlang:error({badarg, no_start_identifier_range});
	Skip =:= undefined ->  %% Default
	    Opts#evh{skip=false};
	true ->
	    Opts
    end.

%% @spec (This::wxEvtHandler()) -> true | false
%% @doc Equivalent to {@link disconnect/3. disconnect(This, null, [])}
%% Can also have an optional callback Fun() as an additional last argument.

disconnect(This=#wx_ref{type=ThisT,ref=_ThisRef}) ->
    ?CLASS(ThisT,wxEvtHandler),
    disconnect(This, null, []).

%% @spec (This::wxEvtHandler(), EventType::wxEventType()) -> true | false
%% @doc Equivalent to {@link disconnect/3. disconnect(This, EventType, [])}
disconnect(This=#wx_ref{type=ThisT,ref=_ThisRef}, EventType) when is_atom(EventType) ->
    ?CLASS(ThisT,wxEvtHandler),
    disconnect(This, EventType, []).

%% @spec (This::wxEvtHandler(), EventType::wxEventType(), Opts) -> true | false
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxevthandler.html#wxevthandlerdisconnect">external documentation</a>
%% This function unsubscribes the process or callback fun from the event handler.
%% EventType may be the atom 'null' to match any eventtype.
%% Notice that the options skip and userdata is not used to match the eventhandler.
disconnect(This=#wx_ref{type=ThisT,ref=_ThisRef}, EventType, Opts)  ->
    ?CLASS(ThisT,wxEvtHandler),
    EvH = parse_opts(Opts, #evh{et=EventType}),
    case wxe_util:disconnect_cb(This, EvH) of
	{badarg, event_type} ->
	    erlang:error({badarg,EventType});
	Bool ->
	    Bool
    end.


%% @hidden
connect_impl(#wx_ref{type=wxeEvtListener,ref=EvtList}, 
	     #wx_ref{type=ThisT,ref=ThisRef}, 
	     #evh{id=Winid, lastId=LastId, et=EventType, 
		  skip=Skip, userdata=Userdata, cb=FunID}) 
  when is_integer(FunID)->
    EventTypeBin = list_to_binary([atom_to_list(EventType)|[0]]),
    ThisTypeBin = list_to_binary([atom_to_list(ThisT)|[0]]),
    UD = if Userdata =:= [] -> 0;
	    true -> 
		 wxe_util:send_bin(term_to_binary(Userdata)),
		 1
	 end,
    wxe_util:call(100, <<EvtList:32/?UI,ThisRef:32/?UI,
			Winid:32/?UI,LastId:32/?UI,
			(wxe_util:from_bool(Skip)):32/?UI,
			UD:32/?UI,
			FunID:32/?UI,
			(size(EventTypeBin)):32/?UI,
			(size(ThisTypeBin)):32/?UI, 
			%% Note no alignment
			EventTypeBin/binary,ThisTypeBin/binary>>).

%% @hidden
disconnect_impl(Listener, Object) ->
    disconnect_impl(Listener, Object, #evh{}).
%% @hidden
disconnect_impl(#wx_ref{type=wxeEvtListener,ref=EvtList}, 
		#wx_ref{type=_ThisT,ref=ThisRef}, 
		#evh{id=Winid, lastId=LastId, et=EventType}) ->
    EventTypeBin = list_to_binary([atom_to_list(EventType)|[0]]),
    wxe_util:call(101, <<EvtList:32/?UI, 
			ThisRef:32/?UI,Winid:32/?UI,LastId:32/?UI,
			(size(EventTypeBin)):32/?UI,
			%% Note no alignment
			EventTypeBin/binary>>).

%% @hidden
new_evt_listener() ->
    wxe_util:call(98, <<>>).

%% @hidden
destroy_evt_listener(#wx_ref{type=wxeEvtListener,ref=EvtList}) ->
    wxe_util:call(99, <<EvtList:32/?UI>>).

%% @hidden
get_callback(#evh{cb=Callback}) ->
    Callback.

%% @hidden
replace_fun_with_id(Evh, Id) ->
    Evh#evh{cb=Id}.

