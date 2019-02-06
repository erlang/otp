%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprinter.html">wxPrinter</a>.
%% @type wxPrinter().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxPrinter).
-include("wxe.hrl").
-export([createAbortWindow/3,destroy/1,getAbort/1,getLastError/0,getPrintDialogData/1,
  new/0,new/1,print/3,print/4,printDialog/2,reportError/4,setup/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxPrinter/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxPrinter() :: wx:wx_object().
%% @equiv new([])
-spec new() -> wxPrinter().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprinter.html#wxprinterwxprinter">external documentation</a>.
-spec new([Option]) -> wxPrinter() when
	Option :: {'data', wxPrintDialogData:wxPrintDialogData()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({data, #wx_ref{type=DataT,ref=DataRef}}, Acc) ->   ?CLASS(DataT,wxPrintDialogData),[<<1:32/?UI,DataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxPrinter_new,
  <<BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprinter.html#wxprintercreateabortwindow">external documentation</a>.
-spec createAbortWindow(This, Parent, Printout) -> wxWindow:wxWindow() when
	This::wxPrinter(), Parent::wxWindow:wxWindow(), Printout::wxPrintout:wxPrintout().
createAbortWindow(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},#wx_ref{type=PrintoutT,ref=PrintoutRef}) ->
  ?CLASS(ThisT,wxPrinter),
  ?CLASS(ParentT,wxWindow),
  ?CLASS(PrintoutT,wxPrintout),
  wxe_util:call(?wxPrinter_CreateAbortWindow,
  <<ThisRef:32/?UI,ParentRef:32/?UI,PrintoutRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprinter.html#wxprintergetabort">external documentation</a>.
-spec getAbort(This) -> boolean() when
	This::wxPrinter().
getAbort(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrinter),
  wxe_util:call(?wxPrinter_GetAbort,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprinter.html#wxprintergetlasterror">external documentation</a>.
%%<br /> Res = ?wxPRINTER_NO_ERROR | ?wxPRINTER_CANCELLED | ?wxPRINTER_ERROR
-spec getLastError() -> wx:wx_enum().
getLastError() ->
  wxe_util:call(?wxPrinter_GetLastError,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprinter.html#wxprintergetprintdialogdata">external documentation</a>.
-spec getPrintDialogData(This) -> wxPrintDialogData:wxPrintDialogData() when
	This::wxPrinter().
getPrintDialogData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrinter),
  wxe_util:call(?wxPrinter_GetPrintDialogData,
  <<ThisRef:32/?UI>>).

%% @equiv print(This,Parent,Printout, [])
-spec print(This, Parent, Printout) -> boolean() when
	This::wxPrinter(), Parent::wxWindow:wxWindow(), Printout::wxPrintout:wxPrintout().

print(This,Parent,Printout)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_record(Printout, wx_ref) ->
  print(This,Parent,Printout, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprinter.html#wxprinterprint">external documentation</a>.
-spec print(This, Parent, Printout, [Option]) -> boolean() when
	This::wxPrinter(), Parent::wxWindow:wxWindow(), Printout::wxPrintout:wxPrintout(),
	Option :: {'prompt', boolean()}.
print(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},#wx_ref{type=PrintoutT,ref=PrintoutRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxPrinter),
  ?CLASS(ParentT,wxWindow),
  ?CLASS(PrintoutT,wxPrintout),
  MOpts = fun({prompt, Prompt}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Prompt)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxPrinter_Print,
  <<ThisRef:32/?UI,ParentRef:32/?UI,PrintoutRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprinter.html#wxprinterprintdialog">external documentation</a>.
-spec printDialog(This, Parent) -> wxDC:wxDC() when
	This::wxPrinter(), Parent::wxWindow:wxWindow().
printDialog(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef}) ->
  ?CLASS(ThisT,wxPrinter),
  ?CLASS(ParentT,wxWindow),
  wxe_util:call(?wxPrinter_PrintDialog,
  <<ThisRef:32/?UI,ParentRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprinter.html#wxprinterreporterror">external documentation</a>.
-spec reportError(This, Parent, Printout, Message) -> 'ok' when
	This::wxPrinter(), Parent::wxWindow:wxWindow(), Printout::wxPrintout:wxPrintout(), Message::unicode:chardata().
reportError(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},#wx_ref{type=PrintoutT,ref=PrintoutRef},Message)
 when ?is_chardata(Message) ->
  ?CLASS(ThisT,wxPrinter),
  ?CLASS(ParentT,wxWindow),
  ?CLASS(PrintoutT,wxPrintout),
  Message_UC = unicode:characters_to_binary([Message,0]),
  wxe_util:cast(?wxPrinter_ReportError,
  <<ThisRef:32/?UI,ParentRef:32/?UI,PrintoutRef:32/?UI,(byte_size(Message_UC)):32/?UI,(Message_UC)/binary, 0:(((8- ((0+byte_size(Message_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprinter.html#wxprintersetup">external documentation</a>.
-spec setup(This, Parent) -> boolean() when
	This::wxPrinter(), Parent::wxWindow:wxWindow().
setup(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef}) ->
  ?CLASS(ThisT,wxPrinter),
  ?CLASS(ParentT,wxWindow),
  wxe_util:call(?wxPrinter_Setup,
  <<ThisRef:32/?UI,ParentRef:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxPrinter()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrinter),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
