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

-module(wxHtmlEasyPrinting).
-include("wxe.hrl").
-export([destroy/1,getPageSetupData/1,getPrintData/1,new/0,new/1,pageSetup/1,
  previewFile/2,previewText/2,previewText/3,printFile/2,printText/2,
  printText/3,setFonts/3,setFonts/4,setFooter/2,setFooter/3,setHeader/2,
  setHeader/3]).

%% inherited exports
-export([parent_class/1]).

-type wxHtmlEasyPrinting() :: wx:wx_object().
-export_type([wxHtmlEasyPrinting/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

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
  MOpts = fun({name, Name}) ->   Name_UC = unicode:characters_to_binary(Name),{name,Name_UC};
          ({parentWindow, #wx_ref{type=ParentWindowT}} = Arg) ->   ?CLASS(ParentWindowT,wxWindow),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxHtmlEasyPrinting_new),
  wxe_util:rec(?wxHtmlEasyPrinting_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintinggetprintdata">external documentation</a>.
-spec getPrintData(This) -> wxPrintData:wxPrintData() when
	This::wxHtmlEasyPrinting().
getPrintData(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlEasyPrinting_GetPrintData),
  wxe_util:rec(?wxHtmlEasyPrinting_GetPrintData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintinggetpagesetupdata">external documentation</a>.
-spec getPageSetupData(This) -> wxPageSetupDialogData:wxPageSetupDialogData() when
	This::wxHtmlEasyPrinting().
getPageSetupData(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlEasyPrinting_GetPageSetupData),
  wxe_util:rec(?wxHtmlEasyPrinting_GetPageSetupData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingpreviewfile">external documentation</a>.
-spec previewFile(This, Htmlfile) -> boolean() when
	This::wxHtmlEasyPrinting(), Htmlfile::unicode:chardata().
previewFile(#wx_ref{type=ThisT}=This,Htmlfile)
 when ?is_chardata(Htmlfile) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Htmlfile_UC = unicode:characters_to_binary(Htmlfile),
  wxe_util:queue_cmd(This,Htmlfile_UC,?get_env(),?wxHtmlEasyPrinting_PreviewFile),
  wxe_util:rec(?wxHtmlEasyPrinting_PreviewFile).

%% @equiv previewText(This,Htmltext, [])
-spec previewText(This, Htmltext) -> boolean() when
	This::wxHtmlEasyPrinting(), Htmltext::unicode:chardata().

previewText(This,Htmltext)
 when is_record(This, wx_ref),?is_chardata(Htmltext) ->
  previewText(This,Htmltext, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingpreviewtext">external documentation</a>.
-spec previewText(This, Htmltext, [Option]) -> boolean() when
	This::wxHtmlEasyPrinting(), Htmltext::unicode:chardata(),
	Option :: {'basepath', unicode:chardata()}.
previewText(#wx_ref{type=ThisT}=This,Htmltext, Options)
 when ?is_chardata(Htmltext),is_list(Options) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Htmltext_UC = unicode:characters_to_binary(Htmltext),
  MOpts = fun({basepath, Basepath}) ->   Basepath_UC = unicode:characters_to_binary(Basepath),{basepath,Basepath_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Htmltext_UC, Opts,?get_env(),?wxHtmlEasyPrinting_PreviewText),
  wxe_util:rec(?wxHtmlEasyPrinting_PreviewText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingprintfile">external documentation</a>.
-spec printFile(This, Htmlfile) -> boolean() when
	This::wxHtmlEasyPrinting(), Htmlfile::unicode:chardata().
printFile(#wx_ref{type=ThisT}=This,Htmlfile)
 when ?is_chardata(Htmlfile) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Htmlfile_UC = unicode:characters_to_binary(Htmlfile),
  wxe_util:queue_cmd(This,Htmlfile_UC,?get_env(),?wxHtmlEasyPrinting_PrintFile),
  wxe_util:rec(?wxHtmlEasyPrinting_PrintFile).

%% @equiv printText(This,Htmltext, [])
-spec printText(This, Htmltext) -> boolean() when
	This::wxHtmlEasyPrinting(), Htmltext::unicode:chardata().

printText(This,Htmltext)
 when is_record(This, wx_ref),?is_chardata(Htmltext) ->
  printText(This,Htmltext, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingprinttext">external documentation</a>.
-spec printText(This, Htmltext, [Option]) -> boolean() when
	This::wxHtmlEasyPrinting(), Htmltext::unicode:chardata(),
	Option :: {'basepath', unicode:chardata()}.
printText(#wx_ref{type=ThisT}=This,Htmltext, Options)
 when ?is_chardata(Htmltext),is_list(Options) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Htmltext_UC = unicode:characters_to_binary(Htmltext),
  MOpts = fun({basepath, Basepath}) ->   Basepath_UC = unicode:characters_to_binary(Basepath),{basepath,Basepath_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Htmltext_UC, Opts,?get_env(),?wxHtmlEasyPrinting_PrintText),
  wxe_util:rec(?wxHtmlEasyPrinting_PrintText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingpagesetup">external documentation</a>.
-spec pageSetup(This) -> 'ok' when
	This::wxHtmlEasyPrinting().
pageSetup(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlEasyPrinting_PageSetup).

%% @equiv setFonts(This,Normal_face,Fixed_face, [])
-spec setFonts(This, Normal_face, Fixed_face) -> 'ok' when
	This::wxHtmlEasyPrinting(), Normal_face::unicode:chardata(), Fixed_face::unicode:chardata().

setFonts(This,Normal_face,Fixed_face)
 when is_record(This, wx_ref),?is_chardata(Normal_face),?is_chardata(Fixed_face) ->
  setFonts(This,Normal_face,Fixed_face, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingsetfonts">external documentation</a>.
-spec setFonts(This, Normal_face, Fixed_face, [Option]) -> 'ok' when
	This::wxHtmlEasyPrinting(), Normal_face::unicode:chardata(), Fixed_face::unicode:chardata(),
	Option :: {'sizes', [integer()]}.
setFonts(#wx_ref{type=ThisT}=This,Normal_face,Fixed_face, Options)
 when ?is_chardata(Normal_face),?is_chardata(Fixed_face),is_list(Options) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Normal_face_UC = unicode:characters_to_binary(Normal_face),
  Fixed_face_UC = unicode:characters_to_binary(Fixed_face),
  MOpts = fun({sizes, _sizes} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Normal_face_UC,Fixed_face_UC, Opts,?get_env(),?wxHtmlEasyPrinting_SetFonts).

%% @equiv setHeader(This,Header, [])
-spec setHeader(This, Header) -> 'ok' when
	This::wxHtmlEasyPrinting(), Header::unicode:chardata().

setHeader(This,Header)
 when is_record(This, wx_ref),?is_chardata(Header) ->
  setHeader(This,Header, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingsetheader">external documentation</a>.
-spec setHeader(This, Header, [Option]) -> 'ok' when
	This::wxHtmlEasyPrinting(), Header::unicode:chardata(),
	Option :: {'pg', integer()}.
setHeader(#wx_ref{type=ThisT}=This,Header, Options)
 when ?is_chardata(Header),is_list(Options) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Header_UC = unicode:characters_to_binary(Header),
  MOpts = fun({pg, _pg} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Header_UC, Opts,?get_env(),?wxHtmlEasyPrinting_SetHeader).

%% @equiv setFooter(This,Footer, [])
-spec setFooter(This, Footer) -> 'ok' when
	This::wxHtmlEasyPrinting(), Footer::unicode:chardata().

setFooter(This,Footer)
 when is_record(This, wx_ref),?is_chardata(Footer) ->
  setFooter(This,Footer, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmleasyprinting.html#wxhtmleasyprintingsetfooter">external documentation</a>.
-spec setFooter(This, Footer, [Option]) -> 'ok' when
	This::wxHtmlEasyPrinting(), Footer::unicode:chardata(),
	Option :: {'pg', integer()}.
setFooter(#wx_ref{type=ThisT}=This,Footer, Options)
 when ?is_chardata(Footer),is_list(Options) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Footer_UC = unicode:characters_to_binary(Footer),
  MOpts = fun({pg, _pg} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Footer_UC, Opts,?get_env(),?wxHtmlEasyPrinting_SetFooter).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxHtmlEasyPrinting()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxHtmlEasyPrinting),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
