%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2018. All Rights Reserved.
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
%%

-ifndef(inets_internal_hrl).
-define(inets_internal_hrl, true).

%% Various trace macros

-define(report(Severity, Label, Service, Content), 
	inets_trace:report_event(Severity, Label, Service, 
				 [{module, ?MODULE}, {line, ?LINE} | Content])).
-define(report_important(Label, Service, Content), 
	?report(20, Label, Service, Content)).
-define(report_verbose(Label, Service, Content),   
	?report(40, Label, Service, Content)).
-define(report_debug(Label, Service, Content),     
	?report(60, Label, Service, Content)).
-define(report_trace(Label, Service, Content),     
	?report(80, Label, Service, Content)).


-define(CR, $\r).
-define(LF, $\n).
-define(CRLF, [$\r,$\n]).
-define(SP, $\s).
-define(TAB, $\t).
-define(LEFT_PAREN, $().
-define(RIGHT_PAREN, $)).
-define(WHITE_SPACE, $ ).
-define(DOUBLE_QUOTE, $"). 

-endif. % -ifdef(inets_internal_hrl).
