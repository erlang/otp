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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprinter.html">wxPrinter</a>.
%% @type wxPrinter().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxPrinter).
-include("wxe.hrl").
-export([createAbortWindow/3,destroy/1,getAbort/1,getLastError/0,getPrintDialogData/1,
  new/0,new/1,print/3,print/4,printDialog/2,reportError/4,setup/2]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxPrinter()
%% @equiv new([])
new() ->
  new([]).

%% @spec ([Option]) -> wxPrinter()
%% Option = {data, wxPrintDialogData:wxPrintDialogData()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprinter.html#wxprinterwxprinter">external documentation</a>.
new(Options)
 when is_list(Options) ->
  MOpts = fun({data, #wx_ref{type=DataT,ref=DataRef}}, Acc) ->   ?CLASS(DataT,wxPrintDialogData),[<<1:32/?UI,DataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxPrinter_new,
  <<BinOpt/binary>>).

%% @spec (This::wxPrinter(), Parent::wxWindow:wxWindow(), Printout::wxPrintout:wxPrintout()) -> wxWindow:wxWindow()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprinter.html#wxprintercreateabortwindow">external documentation</a>.
createAbortWindow(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},#wx_ref{type=PrintoutT,ref=PrintoutRef}) ->
  ?CLASS(ThisT,wxPrinter),
  ?CLASS(ParentT,wxWindow),
  ?CLASS(PrintoutT,wxPrintout),
  wxe_util:call(?wxPrinter_CreateAbortWindow,
  <<ThisRef:32/?UI,ParentRef:32/?UI,PrintoutRef:32/?UI>>).

%% @spec (This::wxPrinter()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprinter.html#wxprintergetabort">external documentation</a>.
getAbort(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrinter),
  wxe_util:call(?wxPrinter_GetAbort,
  <<ThisRef:32/?UI>>).

%% @spec () -> WxPrinterError
%% WxPrinterError = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprinter.html#wxprintergetlasterror">external documentation</a>.
%%<br /> WxPrinterError is one of ?wxPRINTER_NO_ERROR | ?wxPRINTER_CANCELLED | ?wxPRINTER_ERROR
getLastError() ->
  wxe_util:call(?wxPrinter_GetLastError,
  <<>>).

%% @spec (This::wxPrinter()) -> wxPrintDialogData:wxPrintDialogData()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprinter.html#wxprintergetprintdialogdata">external documentation</a>.
getPrintDialogData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrinter),
  wxe_util:call(?wxPrinter_GetPrintDialogData,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrinter(), Parent::wxWindow:wxWindow(), Printout::wxPrintout:wxPrintout()) -> bool()
%% @equiv print(This,Parent,Printout, [])
print(This,Parent,Printout)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_record(Printout, wx_ref) ->
  print(This,Parent,Printout, []).

%% @spec (This::wxPrinter(), Parent::wxWindow:wxWindow(), Printout::wxPrintout:wxPrintout(), [Option]) -> bool()
%% Option = {prompt, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprinter.html#wxprinterprint">external documentation</a>.
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

%% @spec (This::wxPrinter(), Parent::wxWindow:wxWindow()) -> wxDC:wxDC()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprinter.html#wxprinterprintdialog">external documentation</a>.
printDialog(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef}) ->
  ?CLASS(ThisT,wxPrinter),
  ?CLASS(ParentT,wxWindow),
  wxe_util:call(?wxPrinter_PrintDialog,
  <<ThisRef:32/?UI,ParentRef:32/?UI>>).

%% @spec (This::wxPrinter(), Parent::wxWindow:wxWindow(), Printout::wxPrintout:wxPrintout(), Message::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprinter.html#wxprinterreporterror">external documentation</a>.
reportError(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},#wx_ref{type=PrintoutT,ref=PrintoutRef},Message)
 when is_list(Message) ->
  ?CLASS(ThisT,wxPrinter),
  ?CLASS(ParentT,wxWindow),
  ?CLASS(PrintoutT,wxPrintout),
  Message_UC = unicode:characters_to_binary([Message,0]),
  wxe_util:cast(?wxPrinter_ReportError,
  <<ThisRef:32/?UI,ParentRef:32/?UI,PrintoutRef:32/?UI,(byte_size(Message_UC)):32/?UI,(Message_UC)/binary, 0:(((8- ((0+byte_size(Message_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxPrinter(), Parent::wxWindow:wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprinter.html#wxprintersetup">external documentation</a>.
setup(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef}) ->
  ?CLASS(ThisT,wxPrinter),
  ?CLASS(ParentT,wxWindow),
  wxe_util:call(?wxPrinter_Setup,
  <<ThisRef:32/?UI,ParentRef:32/?UI>>).

%% @spec (This::wxPrinter()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrinter),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
