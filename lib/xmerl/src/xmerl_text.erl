%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2024. All Rights Reserved.
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

%% Description  : Callback module for exporting as raw text.


-module(xmerl_text).
-moduledoc false.

-export(['#xml-inheritance#'/0]).

-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1,
	 '#cdata#'/1,
	 '#comment#'/1]).

-include("xmerl.hrl").


'#xml-inheritance#'() -> [].


%% The '#text#' function is called for every text segment of type text.
'#text#'(Text) -> Text.

%% The '#cdata#' function is called for every text segment of type cdata.
%% Handled the same as text.
'#cdata#'(Text) -> Text.

%% The '#comment#' function is called for every comment element.
%% Comment value is not exported since there is no markup.
'#comment#'(_Text) -> [].

%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.

'#root#'(Data, _Attrs, [], _E) -> Data.


%% The '#element#' function is the default handler for XML elements.

'#element#'(_Tag, Data, _Attrs, _Parents, _E) -> Data.
