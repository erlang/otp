%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
%% ------------------------------------------------------------
%%  Focus Demo
%% ------------------------------------------------------------

-module(focus_demo).
-compile([{nowarn_deprecated_function,{gs,config,2}},
          {nowarn_deprecated_function,{gs,create,4}},
          {nowarn_deprecated_function,{gs,read,2}},
          {nowarn_deprecated_function,{gs,start,0}},
          {nowarn_deprecated_function,{gs,window,2}}]).

-export([start/0,init/0]).


%% ----- File Selection ----
start() ->
    spawn(focus_demo,init,[]).


init() ->
    S=gs:start(),
    Font = case gs:read(S,{choose_font,{screen,12}}) of
	       {screen,_,12} = Screen ->
		   Screen;
	       _ ->
		   gs:read(S,{choose_font,{courier,12}})
	   end,
    Win=gs:window(S,[{title,"Focus Demo"},{width,200},{height,150}]),
    gs:create(entry,e1,Win,[{y,0},{focus,true}]),
    gs:create(entry,e2,Win,[{y,30},{focus,true}]),
    gs:create(entry,e3,Win,[{y,60},{focus,true}]),
    gs:create(entry,e4,Win,[{y,90},{focus,true}]),
    gs:create(button,b1,Win,[{x,100},{width,30},
			     {label,{text,"e1"}},{font,Font}]),
    gs:create(button,b2,Win,[{y,30},{x,100},{width,30},
			     {label,{text,"e2"}},{font,Font}]),
    gs:create(button,b3,Win,[{y,60},{x,100},{width,30},
			     {label,{text,"e3"}},{font,Font}]),
    gs:create(button,b4,Win,[{y,90},{x,100},{width,30},
			     {label,{text,"e4"}},{font,Font}]),
    gs:create(button,clear,Win,[{y,120},{x,35},{width,50},
				{label,{text,"Clear"}},{font,Font}]),
    gs:create(button,ask,Win,[{y,120},{x,85},{width,30},
			      {label,{text,"?"}},{font,Font}]),
    gs:create(button,quit,Win,[{y,120},{x,115},{width,50},
			       {label,{text,"Quit"}},{font,Font}]),
    gs:config(Win,{map,true}),
    loop().

loop() ->
    receive
	{gs,quit,_,_,_} -> exit(normal);
	{gs,ask,_,_,_} -> 
	    R1=gs:read(e1,setfocus),R2=gs:read(e2,setfocus),
	    R3=gs:read(e3,setfocus),R4=gs:read(e4,setfocus),
	    R= if R1==true -> e1; 
		   R2==true -> e2;
		   R3==true -> e3;
		   R4==true -> e4;
		   true -> nobody
	       end,
	    io:format("Focus status: ~w has focus.~n",[R]);
	{gs,clear,_,_,_} -> 
	    gs:config(clear,{setfocus,true}),
	    io:format("Focus is cleared.~n",[]);
	{gs,b1,_,_,_} -> gs:config(e1,{setfocus,true});
	{gs,b2,_,_,_} -> gs:config(e2,{setfocus,true});
	{gs,b3,_,_,_} -> gs:config(e3,{setfocus,true});
	{gs,b4,_,_,_} -> gs:config(e4,{setfocus,true});
	{gs,Id,focus,_,[0|_]} -> 
	    io:format("~w lost focus.~n",[Id]);
	{gs,Id,focus,_,[1|_]} -> 
	    io:format("~w gained focus.~n",[Id]);
	{gs,_,destroy,_,_} -> 
	    exit(normal);
	X ->
	    io:format("Got X=~w~n",[X])
    end,
    loop().

%% ----------------------------------------
%% done
