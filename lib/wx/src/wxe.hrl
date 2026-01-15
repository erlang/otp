%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : wxe.hrl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description :  Some internal macros and record definitions
%%%
%%% Created :  8 Feb 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------

-record(wx_ref, {ref, type, state=[]}).

-record(wx_env, {ref, sv, debug=0}).

-record(wx_mem, {bin, size}).

-record(evh, {et=null,id=-1,lastId=-1,cb=0,
	      skip=undefined,userdata=[], % temp
	      handler=undefined % added after connect
	     }).

-define(WXE_IDENTIFIER, wx_env).

-define(CLASS(Type,Class), ((Type) =:= Class) orelse (Type):parent_class(Class)).

-define(CLASS_T(Type,Class),
	try (((Type) =:= Class) orelse (Type):parent_class(Class)) catch _:_ -> false end).

-define(get_env(),
        case erlang:get(?WXE_IDENTIFIER) of
            #wx_env{ref=_wx_Ref_} -> _wx_Ref_;
            _ -> erlang:error({wx,unknown_env})
        end).

-define(UI, unsigned-native).
-define(I,  signed-native).
-define(F,  float-native).

-define(is_chardata(String), (is_list(String) orelse is_binary(String))).
-define(is_colordata(Col), ( (tuple_size(Col) =:= 3 orelse tuple_size(Col) =:= 4)
                             andalso is_integer(element(1, Col)) ) ).

-define(BATCH_BEGIN,    5).
-define(BATCH_END,      6).
%%-define(CREATE_PORT,  2).  %% Not used in erlang
%%-define(REMOVE_PORT,  3).

-define(WXE_CB_START,   9).  %% Used for event-callback start
-define(WXE_CB_RETURN,  11).  %% Used for callback return buffer

-define(WXE_DEBUG_DRIVER,  8).    %% Set debug

%%-define(WXE_SHUTDOWN, 13).  %% Not used in erlang
-define(WXE_DEBUG_PING,  14). %% debug ping (when using debugger it's needed)
-define(WXE_DELETE_ENV,  15).
-define(WXE_GET_CONSTS,  16).

-define(DESTROY_OBJECT,  50).
-define(WXE_REGISTER_OBJECT,  51).  %% Used for object monitoring
%% -define(WXE_INIT_OPENGL, 52).

-include("gen/wxe_funcs.hrl").
