%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

-module(wxPageSetupDialog).
-include("wxe.hrl").
-export([destroy/1,getPageSetupData/1,new/1,new/2,showModal/1]).

%% inherited exports
-export([parent_class/1]).

-type wxPageSetupDialog() :: wx:wx_object().
-export_type([wxPageSetupDialog/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new(Parent, [])
-spec new(Parent) -> wxPageSetupDialog() when
	Parent::wxWindow:wxWindow().

new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialog.html#wxpagesetupdialogwxpagesetupdialog">external documentation</a>.
-spec new(Parent, [Option]) -> wxPageSetupDialog() when
	Parent::wxWindow:wxWindow(),
	Option :: {'data', wxPageSetupDialogData:wxPageSetupDialogData()}.
new(#wx_ref{type=ParentT}=Parent, Options)
 when is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({data, #wx_ref{type=DataT}} = Arg) ->   ?CLASS(DataT,wxPageSetupDialogData),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent, Opts,?get_env(),?wxPageSetupDialog_new),
  wxe_util:rec(?wxPageSetupDialog_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialog.html#wxpagesetupdialoggetpagesetupdata">external documentation</a>.
-spec getPageSetupData(This) -> wxPageSetupDialogData:wxPageSetupDialogData() when
	This::wxPageSetupDialog().
getPageSetupData(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialog_GetPageSetupData),
  wxe_util:rec(?wxPageSetupDialog_GetPageSetupData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpagesetupdialog.html#wxpagesetupdialogshowmodal">external documentation</a>.
-spec showModal(This) -> integer() when
	This::wxPageSetupDialog().
showModal(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPageSetupDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxPageSetupDialog_ShowModal),
  wxe_util:rec(?wxPageSetupDialog_ShowModal).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxPageSetupDialog()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPageSetupDialog),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
