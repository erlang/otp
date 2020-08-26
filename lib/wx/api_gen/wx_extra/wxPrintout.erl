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
%%
<<EXPORT:wxPrintout new/2,new/3 wxPrintout:EXPORT>>

<<wxPrintout
%% @spec (Title::string(), OnPrintPage::function()) -> wxPrintout:wxPrintout()
%% @doc @equiv new(Title, OnPrintPage, [])
new(Title, OnPrintPage) ->
    new(Title, OnPrintPage, []).

%% @spec (Title::string(), OnPrintPage::function(), [Option]) -> wxPrintout:wxPrintout()
%% Option = {onPreparePrinting, OnPreparePrinting::function()} | 
%%          {onBeginPrinting,   OnBeginPrinting::function()} | 
%%          {onEndPrinting,     OnEndPrinting::function()} | 
%%          {onBeginDocument,   OnBeginDocument::function()} | 
%%          {onEndDocument,     OnEndDocument::function()} | 
%%          {hasPage,           HasPage::function()} | 
%%          {getPageInfo,       GetPageInfo::function()}
%% @doc Creates a wxPrintout object with a callback fun and optionally other callback funs.<br />
%%   <pre>OnPrintPage(This,Page) -> boolean() </pre>
%%   <pre>OnPreparePrinting(This) -> ok  </pre>
%%   <pre>OnBeginPrinting(This) -> ok   </pre>
%%   <pre>OnEndPrinting(This) -> ok  </pre>
%%   <pre>OnBeginDocument(This,StartPage,EndPage) -> boolean()  </pre>
%%   <pre>OnEndDocument(This) -> ok  </pre>
%%   <pre>HasPage(This,Page)} -> boolean()   </pre>
%%   <pre>GetPageInfo(This) -> {MinPage::integer(), MaxPage::integer(),
%%                              PageFrom::integer(), PageTo::integer()}  </pre>
%%  The <b>This</b> argument is the wxPrintout object reference to this object
%%  <br /> NOTE: The callbacks may not call other processes.
new(Title, OnPrintPage, Opts) when is_list(Title), is_function(OnPrintPage), is_list(Opts) ->
    OnPrintPageId = wxe_util:get_cbId(OnPrintPage),
    MOpts = fun({onPreparePrinting, F},Acc) when is_function(F) ->
		    [{onPreparePrinting, wxe_util:get_cbId(F)}|Acc];
	       ({onBeginPrinting, F},Acc) when is_function(F) ->
		    [{onBeginPrinting, wxe_util:get_cbId(F)}|Acc];
	       ({onEndPrinting, F},Acc) when is_function(F) ->
		    [{onEndPrinting, wxe_util:get_cbId(F)}|Acc];
	       ({onBeginDocument, F},Acc) when is_function(F) ->
		    [{onBeginDocument, wxe_util:get_cbId(F)}|Acc];
	       ({onEndDocument, F},Acc) when is_function(F) ->
		    [{onEndDocument, wxe_util:get_cbId(F)}|Acc];
	       ({hasPage, F},Acc) when is_function(F) ->
		    [{hasPage, wxe_util:get_cbId(F)}|Acc];
	       ({getPageInfo, F},Acc) when is_function(F) ->
		    [{getPageInfo,wxe_util:get_cbId(F)}|Acc]
	    end,
    OptsMod = lists:foldl(MOpts, [], Opts),
    Title_UC = unicode:characters_to_binary(Title),
    Op = ~s,
    wxe_util:queue_cmd(Title_UC, OnPrintPageId, OptsMod, ?get_env(), Op),
    wxe_util:rec(Op).

wxPrintout>>
