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
%%   <pre>OnPreparePrinting(This) -> term()   </pre>
%%   <pre>OnBeginPrinting(This) -> term()   </pre>
%%   <pre>OnEndPrinting(This) -> term()   </pre>
%%   <pre>OnBeginDocument(This,StartPage,EndPage) -> boolean()  </pre>
%%   <pre>OnEndDocument(This) -> term()  </pre>
%%   <pre>HasPage(This,Page)} -> boolean()   </pre>
%%   <pre>GetPageInfo(This) -> {MinPage::integer(), MaxPage::integer(),
%%                              PageFrom::integer(), PageTo::integer()}  </pre>
%%  The <b>This</b> argument is the wxPrintout object reference to this object
%%  <br /> NOTE: The callbacks may not call other processes. 
new(Title, OnPrintPage, Opts) when is_list(Title), is_function(OnPrintPage), is_list(Opts) ->
    OnPrint = fun([This,Page]) -> 
		      Bool = OnPrintPage(This,Page), 
		      <<(wxe_util:from_bool(Bool)):32/?UI>>
	      end,
    OnPrintPageId = wxe_util:get_cbId(OnPrint),
    MOpts = fun({onPreparePrinting, F},Acc) when is_function(F) ->
		    Fun = fun([This]) -> 
				  F(This), 
				  <<>> 
			  end,
		    [<<1:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({onBeginPrinting, F},Acc) when is_function(F) -> 
		    Fun = fun([This]) -> 
				  F(This), 
				  <<>>
			  end,
		    [<<2:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({onEndPrinting, F},Acc) when is_function(F) -> 
		    Fun = fun([This]) -> 
				  F(This), 
				  <<>> 
			  end,
		    [<<3:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({onBeginDocument, F},Acc) when is_function(F) -> 
		    Fun = fun([This,S,E]) -> 
				  BegD = F(This,S,E), 
				  <<(wxe_util:from_bool(BegD)):32/?UI>>
			  end,
		    [<<4:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({onEndDocument, F},Acc) when is_function(F) -> 
		    Fun = fun([This]) -> 
				  F(This), 
				  <<>> 
			  end,
		    [<<5:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({hasPage, F},Acc) when is_function(F) -> 
		    Fun = fun([This,Page]) -> 
				  HasP = F(This,Page),
				  <<(wxe_util:from_bool(HasP)):32/?UI>>
			  end,
		    [<<6:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({getPageInfo, F},Acc) when is_function(F) -> 
		    Fun = fun([This]) ->
				  {Min,Max,PF,PT} = F(This),
				  <<Min:32/?UI,Max:32/?UI,PF:32/?UI,PT:32/?UI>>
			  end,
		    [<<7:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc]
	    end,
    BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Opts)),
    Title_UC = unicode:characters_to_binary([Title,0]),
    wxe_util:call(~s, << (byte_size(Title_UC)):32/?UI,Title_UC/binary,
			  0:(((8- ((4+byte_size(Title_UC)) band 16#7)) band 16#7))/unit:8,
			  OnPrintPageId:32/?UI,
			  BinOpt/binary>>).

wxPrintout>>
