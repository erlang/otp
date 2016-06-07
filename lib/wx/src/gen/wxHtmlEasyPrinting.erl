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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html">wxHtmlEasyPrinting</a>.
%% @type wxHtmlEasyPrinting().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxHtmlEasyPrinting).
-include("wxe.hrl").
-export([destroy/1,getPageSetupData/1,getPrintData/1,new/0,new/1,pageSetup/1,
  previewFile/2,previewText/2,previewText/3,printFile/2,printText/2,
  printText/3,setFonts/3,setFonts/4,setFooter/2,setFooter/3,setHeader/2,
  setHeader/3]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxHtmlEasyPrinting/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxHtmlEasyPrinting() :: wx:wx_object().
%% @equiv new([])
-spec new() -> wxHtmlEasyPrinting().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingwxhtmleasyprinting">external documentation</a>.
-spec new([Option]) -> wxHtmlEasyPrinting() when
	Option :: {'name', unicode:chardata()}
		 | {'parentWindow', wxWindow:wxWindow()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({name, Name}, Acc) ->   Name_UC = unicode:characters_to_binary([Name,0]),[<<1:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({parentWindow, #wx_ref{type=ParentWindowT,ref=ParentWindowRef}}, Acc) ->   ?CLASS(ParentWindowT,wxWindow),[<<2:32/?UI,ParentWindowRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxHtmlEasyPrinting_new,
  <<BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintinggetprintdata">external documentation</a>.
-spec getPrintData(This) -> wxPrintData:wxPrintData() when
	This::wxHtmlEasyPrinting().
getPrintData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  wxe_util:call(?wxHtmlEasyPrinting_GetPrintData,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintinggetpagesetupdata">external documentation</a>.
-spec getPageSetupData(This) -> wxPageSetupDialogData:wxPageSetupDialogData() when
	This::wxHtmlEasyPrinting().
getPageSetupData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  wxe_util:call(?wxHtmlEasyPrinting_GetPageSetupData,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingpreviewfile">external documentation</a>.
-spec previewFile(This, Htmlfile) -> boolean() when
	This::wxHtmlEasyPrinting(), Htmlfile::unicode:chardata().
previewFile(#wx_ref{type=ThisT,ref=ThisRef},Htmlfile)
 when is_list(Htmlfile) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Htmlfile_UC = unicode:characters_to_binary([Htmlfile,0]),
  wxe_util:call(?wxHtmlEasyPrinting_PreviewFile,
  <<ThisRef:32/?UI,(byte_size(Htmlfile_UC)):32/?UI,(Htmlfile_UC)/binary, 0:(((8- ((0+byte_size(Htmlfile_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @equiv previewText(This,Htmltext, [])
-spec previewText(This, Htmltext) -> boolean() when
	This::wxHtmlEasyPrinting(), Htmltext::unicode:chardata().

previewText(This,Htmltext)
 when is_record(This, wx_ref),is_list(Htmltext) ->
  previewText(This,Htmltext, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingpreviewtext">external documentation</a>.
-spec previewText(This, Htmltext, [Option]) -> boolean() when
	This::wxHtmlEasyPrinting(), Htmltext::unicode:chardata(),
	Option :: {'basepath', unicode:chardata()}.
previewText(#wx_ref{type=ThisT,ref=ThisRef},Htmltext, Options)
 when is_list(Htmltext),is_list(Options) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Htmltext_UC = unicode:characters_to_binary([Htmltext,0]),
  MOpts = fun({basepath, Basepath}, Acc) ->   Basepath_UC = unicode:characters_to_binary([Basepath,0]),[<<1:32/?UI,(byte_size(Basepath_UC)):32/?UI,(Basepath_UC)/binary, 0:(((8- ((0+byte_size(Basepath_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxHtmlEasyPrinting_PreviewText,
  <<ThisRef:32/?UI,(byte_size(Htmltext_UC)):32/?UI,(Htmltext_UC)/binary, 0:(((8- ((0+byte_size(Htmltext_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingprintfile">external documentation</a>.
-spec printFile(This, Htmlfile) -> boolean() when
	This::wxHtmlEasyPrinting(), Htmlfile::unicode:chardata().
printFile(#wx_ref{type=ThisT,ref=ThisRef},Htmlfile)
 when is_list(Htmlfile) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Htmlfile_UC = unicode:characters_to_binary([Htmlfile,0]),
  wxe_util:call(?wxHtmlEasyPrinting_PrintFile,
  <<ThisRef:32/?UI,(byte_size(Htmlfile_UC)):32/?UI,(Htmlfile_UC)/binary, 0:(((8- ((0+byte_size(Htmlfile_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @equiv printText(This,Htmltext, [])
-spec printText(This, Htmltext) -> boolean() when
	This::wxHtmlEasyPrinting(), Htmltext::unicode:chardata().

printText(This,Htmltext)
 when is_record(This, wx_ref),is_list(Htmltext) ->
  printText(This,Htmltext, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingprinttext">external documentation</a>.
-spec printText(This, Htmltext, [Option]) -> boolean() when
	This::wxHtmlEasyPrinting(), Htmltext::unicode:chardata(),
	Option :: {'basepath', unicode:chardata()}.
printText(#wx_ref{type=ThisT,ref=ThisRef},Htmltext, Options)
 when is_list(Htmltext),is_list(Options) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Htmltext_UC = unicode:characters_to_binary([Htmltext,0]),
  MOpts = fun({basepath, Basepath}, Acc) ->   Basepath_UC = unicode:characters_to_binary([Basepath,0]),[<<1:32/?UI,(byte_size(Basepath_UC)):32/?UI,(Basepath_UC)/binary, 0:(((8- ((0+byte_size(Basepath_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxHtmlEasyPrinting_PrintText,
  <<ThisRef:32/?UI,(byte_size(Htmltext_UC)):32/?UI,(Htmltext_UC)/binary, 0:(((8- ((0+byte_size(Htmltext_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingpagesetup">external documentation</a>.
-spec pageSetup(This) -> 'ok' when
	This::wxHtmlEasyPrinting().
pageSetup(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  wxe_util:cast(?wxHtmlEasyPrinting_PageSetup,
  <<ThisRef:32/?UI>>).

%% @equiv setFonts(This,Normal_face,Fixed_face, [])
-spec setFonts(This, Normal_face, Fixed_face) -> 'ok' when
	This::wxHtmlEasyPrinting(), Normal_face::unicode:chardata(), Fixed_face::unicode:chardata().

setFonts(This,Normal_face,Fixed_face)
 when is_record(This, wx_ref),is_list(Normal_face),is_list(Fixed_face) ->
  setFonts(This,Normal_face,Fixed_face, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingsetfonts">external documentation</a>.
-spec setFonts(This, Normal_face, Fixed_face, [Option]) -> 'ok' when
	This::wxHtmlEasyPrinting(), Normal_face::unicode:chardata(), Fixed_face::unicode:chardata(),
	Option :: {'sizes', [integer()]}.
setFonts(#wx_ref{type=ThisT,ref=ThisRef},Normal_face,Fixed_face, Options)
 when is_list(Normal_face),is_list(Fixed_face),is_list(Options) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Normal_face_UC = unicode:characters_to_binary([Normal_face,0]),
  Fixed_face_UC = unicode:characters_to_binary([Fixed_face,0]),
  MOpts = fun({sizes, Sizes}, Acc) -> [<<1:32/?UI,(length(Sizes)):32/?UI,
        (<< <<C:32/?I>> || C <- Sizes>>)/binary, 0:(((0+length(Sizes)) rem 2)*32)>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxHtmlEasyPrinting_SetFonts,
  <<ThisRef:32/?UI,(byte_size(Normal_face_UC)):32/?UI,(Normal_face_UC)/binary, 0:(((8- ((0+byte_size(Normal_face_UC)) band 16#7)) band 16#7))/unit:8,(byte_size(Fixed_face_UC)):32/?UI,(Fixed_face_UC)/binary, 0:(((8- ((4+byte_size(Fixed_face_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @equiv setHeader(This,Header, [])
-spec setHeader(This, Header) -> 'ok' when
	This::wxHtmlEasyPrinting(), Header::unicode:chardata().

setHeader(This,Header)
 when is_record(This, wx_ref),is_list(Header) ->
  setHeader(This,Header, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingsetheader">external documentation</a>.
-spec setHeader(This, Header, [Option]) -> 'ok' when
	This::wxHtmlEasyPrinting(), Header::unicode:chardata(),
	Option :: {'pg', integer()}.
setHeader(#wx_ref{type=ThisT,ref=ThisRef},Header, Options)
 when is_list(Header),is_list(Options) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Header_UC = unicode:characters_to_binary([Header,0]),
  MOpts = fun({pg, Pg}, Acc) -> [<<1:32/?UI,Pg:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxHtmlEasyPrinting_SetHeader,
  <<ThisRef:32/?UI,(byte_size(Header_UC)):32/?UI,(Header_UC)/binary, 0:(((8- ((0+byte_size(Header_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @equiv setFooter(This,Footer, [])
-spec setFooter(This, Footer) -> 'ok' when
	This::wxHtmlEasyPrinting(), Footer::unicode:chardata().

setFooter(This,Footer)
 when is_record(This, wx_ref),is_list(Footer) ->
  setFooter(This,Footer, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingsetfooter">external documentation</a>.
-spec setFooter(This, Footer, [Option]) -> 'ok' when
	This::wxHtmlEasyPrinting(), Footer::unicode:chardata(),
	Option :: {'pg', integer()}.
setFooter(#wx_ref{type=ThisT,ref=ThisRef},Footer, Options)
 when is_list(Footer),is_list(Options) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Footer_UC = unicode:characters_to_binary([Footer,0]),
  MOpts = fun({pg, Pg}, Acc) -> [<<1:32/?UI,Pg:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxHtmlEasyPrinting_SetFooter,
  <<ThisRef:32/?UI,(byte_size(Footer_UC)):32/?UI,(Footer_UC)/binary, 0:(((8- ((0+byte_size(Footer_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxHtmlEasyPrinting()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxHtmlEasyPrinting),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
