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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialog.html">wxPageSetupDialog</a>.
%% @type wxPageSetupDialog().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxPageSetupDialog).
-include("wxe.hrl").
-export([destroy/1,getPageSetupData/1,new/1,new/2,showModal/1]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (Parent::wxWindow:wxWindow()) -> wxPageSetupDialog()
%% @equiv new(Parent, [])
new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

%% @spec (Parent::wxWindow:wxWindow(), [Option]) -> wxPageSetupDialog()
%% Option = {data, wxPageSetupDialogData:wxPageSetupDialogData()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialog.html#wxpagesetupdialogwxpagesetupdialog">external documentation</a>.
new(#wx_ref{type=ParentT,ref=ParentRef}, Options)
 when is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({data, #wx_ref{type=DataT,ref=DataRef}}, Acc) ->   ?CLASS(DataT,wxPageSetupDialogData),[<<1:32/?UI,DataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxPageSetupDialog_new,
  <<ParentRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxPageSetupDialog()) -> wxPageSetupDialogData:wxPageSetupDialogData()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialog.html#wxpagesetupdialoggetpagesetupdata">external documentation</a>.
getPageSetupData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialog),
  wxe_util:call(?wxPageSetupDialog_GetPageSetupData,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPageSetupDialog()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxpagesetupdialog.html#wxpagesetupdialogshowmodal">external documentation</a>.
showModal(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPageSetupDialog),
  wxe_util:call(?wxPageSetupDialog_ShowModal,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPageSetupDialog()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPageSetupDialog),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
