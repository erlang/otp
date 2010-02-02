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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhtmleasyprinting.html">wxHtmlEasyPrinting</a>.
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

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxHtmlEasyPrinting()
%% @equiv new([])
new() ->
  new([]).

%% @spec ([Option]) -> wxHtmlEasyPrinting()
%% Option = {name, string()} | {parentWindow, wxWindow:wxWindow()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhtmleasyprinting.html#wxhtmleasyprintingwxhtmleasyprinting">external documentation</a>.
new(Options)
 when is_list(Options) ->
  MOpts = fun({name, Name}, Acc) ->   Name_UC = unicode:characters_to_binary([Name,0]),[<<1:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({parentWindow, #wx_ref{type=ParentWindowT,ref=ParentWindowRef}}, Acc) ->   ?CLASS(ParentWindowT,wxWindow),[<<2:32/?UI,ParentWindowRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxHtmlEasyPrinting_new,
  <<BinOpt/binary>>).

%% @spec (This::wxHtmlEasyPrinting()) -> wxPrintData:wxPrintData()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhtmleasyprinting.html#wxhtmleasyprintinggetprintdata">external documentation</a>.
getPrintData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  wxe_util:call(?wxHtmlEasyPrinting_GetPrintData,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxHtmlEasyPrinting()) -> wxPageSetupDialogData:wxPageSetupDialogData()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhtmleasyprinting.html#wxhtmleasyprintinggetpagesetupdata">external documentation</a>.
getPageSetupData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  wxe_util:call(?wxHtmlEasyPrinting_GetPageSetupData,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxHtmlEasyPrinting(), Htmlfile::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhtmleasyprinting.html#wxhtmleasyprintingpreviewfile">external documentation</a>.
previewFile(#wx_ref{type=ThisT,ref=ThisRef},Htmlfile)
 when is_list(Htmlfile) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Htmlfile_UC = unicode:characters_to_binary([Htmlfile,0]),
  wxe_util:call(?wxHtmlEasyPrinting_PreviewFile,
  <<ThisRef:32/?UI,(byte_size(Htmlfile_UC)):32/?UI,(Htmlfile_UC)/binary, 0:(((8- ((0+byte_size(Htmlfile_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxHtmlEasyPrinting(), Htmltext::string()) -> bool()
%% @equiv previewText(This,Htmltext, [])
previewText(This,Htmltext)
 when is_record(This, wx_ref),is_list(Htmltext) ->
  previewText(This,Htmltext, []).

%% @spec (This::wxHtmlEasyPrinting(), Htmltext::string(), [Option]) -> bool()
%% Option = {basepath, string()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhtmleasyprinting.html#wxhtmleasyprintingpreviewtext">external documentation</a>.
previewText(#wx_ref{type=ThisT,ref=ThisRef},Htmltext, Options)
 when is_list(Htmltext),is_list(Options) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Htmltext_UC = unicode:characters_to_binary([Htmltext,0]),
  MOpts = fun({basepath, Basepath}, Acc) ->   Basepath_UC = unicode:characters_to_binary([Basepath,0]),[<<1:32/?UI,(byte_size(Basepath_UC)):32/?UI,(Basepath_UC)/binary, 0:(((8- ((0+byte_size(Basepath_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxHtmlEasyPrinting_PreviewText,
  <<ThisRef:32/?UI,(byte_size(Htmltext_UC)):32/?UI,(Htmltext_UC)/binary, 0:(((8- ((0+byte_size(Htmltext_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxHtmlEasyPrinting(), Htmlfile::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhtmleasyprinting.html#wxhtmleasyprintingprintfile">external documentation</a>.
printFile(#wx_ref{type=ThisT,ref=ThisRef},Htmlfile)
 when is_list(Htmlfile) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Htmlfile_UC = unicode:characters_to_binary([Htmlfile,0]),
  wxe_util:call(?wxHtmlEasyPrinting_PrintFile,
  <<ThisRef:32/?UI,(byte_size(Htmlfile_UC)):32/?UI,(Htmlfile_UC)/binary, 0:(((8- ((0+byte_size(Htmlfile_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxHtmlEasyPrinting(), Htmltext::string()) -> bool()
%% @equiv printText(This,Htmltext, [])
printText(This,Htmltext)
 when is_record(This, wx_ref),is_list(Htmltext) ->
  printText(This,Htmltext, []).

%% @spec (This::wxHtmlEasyPrinting(), Htmltext::string(), [Option]) -> bool()
%% Option = {basepath, string()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhtmleasyprinting.html#wxhtmleasyprintingprinttext">external documentation</a>.
printText(#wx_ref{type=ThisT,ref=ThisRef},Htmltext, Options)
 when is_list(Htmltext),is_list(Options) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Htmltext_UC = unicode:characters_to_binary([Htmltext,0]),
  MOpts = fun({basepath, Basepath}, Acc) ->   Basepath_UC = unicode:characters_to_binary([Basepath,0]),[<<1:32/?UI,(byte_size(Basepath_UC)):32/?UI,(Basepath_UC)/binary, 0:(((8- ((0+byte_size(Basepath_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxHtmlEasyPrinting_PrintText,
  <<ThisRef:32/?UI,(byte_size(Htmltext_UC)):32/?UI,(Htmltext_UC)/binary, 0:(((8- ((0+byte_size(Htmltext_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxHtmlEasyPrinting()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhtmleasyprinting.html#wxhtmleasyprintingpagesetup">external documentation</a>.
pageSetup(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  wxe_util:cast(?wxHtmlEasyPrinting_PageSetup,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxHtmlEasyPrinting(), Normal_face::string(), Fixed_face::string()) -> ok
%% @equiv setFonts(This,Normal_face,Fixed_face, [])
setFonts(This,Normal_face,Fixed_face)
 when is_record(This, wx_ref),is_list(Normal_face),is_list(Fixed_face) ->
  setFonts(This,Normal_face,Fixed_face, []).

%% @spec (This::wxHtmlEasyPrinting(), Normal_face::string(), Fixed_face::string(), [Option]) -> ok
%% Option = {sizes, [integer()]}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhtmleasyprinting.html#wxhtmleasyprintingsetfonts">external documentation</a>.
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

%% @spec (This::wxHtmlEasyPrinting(), Header::string()) -> ok
%% @equiv setHeader(This,Header, [])
setHeader(This,Header)
 when is_record(This, wx_ref),is_list(Header) ->
  setHeader(This,Header, []).

%% @spec (This::wxHtmlEasyPrinting(), Header::string(), [Option]) -> ok
%% Option = {pg, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhtmleasyprinting.html#wxhtmleasyprintingsetheader">external documentation</a>.
setHeader(#wx_ref{type=ThisT,ref=ThisRef},Header, Options)
 when is_list(Header),is_list(Options) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Header_UC = unicode:characters_to_binary([Header,0]),
  MOpts = fun({pg, Pg}, Acc) -> [<<1:32/?UI,Pg:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxHtmlEasyPrinting_SetHeader,
  <<ThisRef:32/?UI,(byte_size(Header_UC)):32/?UI,(Header_UC)/binary, 0:(((8- ((0+byte_size(Header_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxHtmlEasyPrinting(), Footer::string()) -> ok
%% @equiv setFooter(This,Footer, [])
setFooter(This,Footer)
 when is_record(This, wx_ref),is_list(Footer) ->
  setFooter(This,Footer, []).

%% @spec (This::wxHtmlEasyPrinting(), Footer::string(), [Option]) -> ok
%% Option = {pg, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxhtmleasyprinting.html#wxhtmleasyprintingsetfooter">external documentation</a>.
setFooter(#wx_ref{type=ThisT,ref=ThisRef},Footer, Options)
 when is_list(Footer),is_list(Options) ->
  ?CLASS(ThisT,wxHtmlEasyPrinting),
  Footer_UC = unicode:characters_to_binary([Footer,0]),
  MOpts = fun({pg, Pg}, Acc) -> [<<1:32/?UI,Pg:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxHtmlEasyPrinting_SetFooter,
  <<ThisRef:32/?UI,(byte_size(Footer_UC)):32/?UI,(Footer_UC)/binary, 0:(((8- ((0+byte_size(Footer_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxHtmlEasyPrinting()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxHtmlEasyPrinting),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
