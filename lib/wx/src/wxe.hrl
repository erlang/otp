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
%%%-------------------------------------------------------------------
%%% File    : wxe.hrl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description :  Some internal macros and record definitions
%%%
%%% Created :  8 Feb 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------

-record(wx_ref, {ref, type, state=[]}).

-record(wx_env, {port, sv, debug=0}).

-record(wx_mem, {bin, size}).

-record(evh, {et=null,id=-1,lastId=-1,cb=0,
	      skip=undefined,userdata=[], % temp
	      handler=undefined % added after connect
	     }).

-define(CLASS(Type,Class), ((Type) =:= Class) orelse (Type):parent_class(Class)).

-define(CLASS_T(Type,Class),
	try (((Type) =:= Class) orelse (Type):parent_class(Class)) catch _:_ -> false end).

-define(UI, unsigned-native).
-define(I,  signed-native).
-define(F,  float-native).

-define(WXE_IDENTIFIER, wx_env).
-define(BATCH_BEGIN,    0).
-define(BATCH_END,      1).
%%-define(CREATE_PORT,  2).  %% Not used in erlang
%%-define(REMOVE_PORT,  3).
-define(DESTROY_OBJECT, 4).
-define(WXE_CB_RETURN,  5).  %% Used for callback return buffer
%%-define(WXE_SHUTDOWN, 6).  %% Not used in erlang
-define(WXE_REGISTER_OBJECT,  7).  %% Used for object monitoring
-define(WXE_CB_START,   8).  %% Used for event-callback start
-define(WXE_DEBUG_DRIVER,  9).    %% Set debug
%%-define(WXE_DEBUG_PING,  10).    %% debug ping (when using debugger it's needed)
-define(WXE_BIN_INCR,    11). %% Binary refc incr 
-define(WXE_BIN_DECR,    12). %% Binary refc decr
-define(WXE_INIT_OPENGL, 13). %% Binary refc decr

-include("gen/wxe_funcs.hrl").
