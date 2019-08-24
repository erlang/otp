%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2015. All Rights Reserved.
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
    {A, B, C} = {erlang:phash2([node()]),
                 inets_time_compat:monotonic_time(),
                 inets_time_compat:unique_integer()},

    random:seed(A, B, C),  
    lists:sort([random:uniform(X) || X <- lists:seq(1, WorkSim)]),  
    lists:flatten(lists:duplicate(SzSim, "Dummy data ")). 

%% log(F, A) ->
%%     hdlt_logger:set_name("HDLT RANDOM-HTML"),
%%     hdlt_logger:set_level(debug),
%%     hdlt_logger:log(F, A).
