%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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

-module(hdlt_random_html). 
-export([page/3]). 

page(SessionID, _Env, Input) -> 
%%     log("page(~p) -> deliver content-type when"
%% 	"~n   SessionID: ~p"
%% 	"~n   Env:       ~p"
%% 	"~n   Input:     ~p", [self(), SessionID, Env, Input]),
    [WorkSimStr, SzSimStr] = string:tokens(Input, [$:]),
    WorkSim = list_to_integer(WorkSimStr),
    SzSim   = list_to_integer(SzSimStr),
    mod_esi:deliver(SessionID,  "Content-Type:text/html\r\n\r\n"),  
    mod_esi:deliver(SessionID, start("Random test page")),  
    mod_esi:deliver(SessionID, content(WorkSim, SzSim)),  
    mod_esi:deliver(SessionID, stop()),
    ok.

start(Title) ->  
    "<HTML>  
<HEAD>  
<TITLE>" ++ Title ++ "</TITLE> 
 </HEAD>  
<BODY>\n".  

stop() ->  
    "</BODY>  
</HTML>
". 
 
content(WorkSim, SzSim) ->  
    {A, B, C} = now(),  
    random:seed(A, B, C),  
    lists:sort([random:uniform(X) || X <- lists:seq(1, WorkSim)]),  
    lists:flatten(lists:duplicate(SzSim, "Dummy data ")). 

%% log(F, A) ->
%%     hdlt_logger:set_name("HDLT RANDOM-HTML"),
%%     hdlt_logger:set_level(debug),
%%     hdlt_logger:log(F, A).
