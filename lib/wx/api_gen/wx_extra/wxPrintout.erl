%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2021. All Rights Reserved.
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
%% @doc @equiv new(Title, OnPrintPage, [])
-spec new(Title::string(), OnPrintPage::function()) -> wxPrintout:wxPrintout().
new(Title, OnPrintPage) ->
    new(Title, OnPrintPage, []).

-spec new(Title::string(), OnPrintPage, [Option]) ->
          wxPrintout:wxPrintout() when
      OnPrintPage :: fun((wxPrintout(), Page::integer()) -> boolean()),
      Option ::{onPreparePrinting, fun((wxPrintout()) -> ok)} |
               {onBeginPrinting,   fun((wxPrintout()) -> ok)} |
               {onEndPrinting,     fun((wxPrintout()) -> ok)} |
               {onBeginDocument,   fun((wxPrintout(), StartPage::integer(), EndPage::integer()) -> boolean())} |
               {onEndDocument,     fun((wxPrintout()) -> ok)} |
               {hasPage,           fun((wxPrintout(), Page::integer()) -> ok)} |
               {getPageInfo,       fun((wxPrintout()) ->
                                              {MinPage::integer(), MaxPage::integer(),
                                               PageFrom::integer(), PageTo::integer()})}.

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
