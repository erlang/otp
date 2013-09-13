%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
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
%%
%%

-ifndef(inets_internal_hrl).
-define(inets_internal_hrl, true).

-define(STACK(), erlang:get_stacktrace()).

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
