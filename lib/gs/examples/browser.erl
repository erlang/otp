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
%% Simple Browser
%% ------------------------------------------------------------

-module(browser).
-compile([{nowarn_deprecated_function,{gs,button,2}},
          {nowarn_deprecated_function,{gs,config,2}},
          {nowarn_deprecated_function,{gs,entry,2}},
          {nowarn_deprecated_function,{gs,label,2}},
          {nowarn_deprecated_function,{gs,listbox,2}},
          {nowarn_deprecated_function,{gs,read,2}},
          {nowarn_deprecated_function,{gs,start,0}},
          {nowarn_deprecated_function,{gs,window,2}}]).

-export([start/0,start/2,init/3]).

start() ->
    spawn(browser,init,[self(),text(),items()]),	
    receive
	{browser,Result} -> Result
    end.

text() -> "Pick an erlangian: ".

items() ->
    lists:sort(["marcus","bjorn","anders","dalle","henrik","mk",
		"keisu","klas","patric","ola","torpvret","lelle",
		"eklas","mbj","janne","martin","kent","pippi",
		"gunilla","uwiger","macr"]).


start(Text,Items) ->
    spawn(browser,init,[self(),Text,Items]),	
    receive
	{browser,Result} -> Result
    end.

init(Pid,Text,Items) ->
    S=gs:start(),
    Win=gs:window(S,[{width,250},{height,270},{title,"Browser"}]),
    gs:label(Win,[{label,{text,Text}},{width,250}]),
    Entry=gs:entry(Win,[{y,35},{width,240},{x,5},
			{keypress,true},{setfocus,true}]),
    Lb=gs:listbox(Win,[{x,5},{y,65},{width,160},{height,195},
		       {vscroll,right},{click,true},{doubleclick,true}]),
    Ok=gs:button(Win,[{label,{text,"OK"}},{width,40},{x,185},{y,175}]),
    Cancel=gs:button(Win,[{label,{text,"Cancel"}},{x,175},{y,225},{width,65}]),
    gs:config(Lb,[{items,Items}]),
    gs:config(Win,{map,true}),
    browser_loop(Pid,Ok,Cancel,Entry,Lb).

browser_loop(Pid,Ok,Cancel,Entry,Lb) ->
    receive
	{gs,Ok,click,_,_} ->
	    Txt=gs:read(Entry,text),
	    Pid ! {browser,{ok,Txt}};
	{gs,Cancel,click,_,_} ->
	    Pid ! {browser,cancel};
	{gs,Entry,keypress,_,['Return'|_]} ->
	    Txt=gs:read(Entry,text),
	    Pid ! {browser,{ok,Txt}};
	{gs,Entry,keypress,_,_} ->
	    browser_loop(Pid,Ok,Cancel,Entry,Lb);
	{gs,Lb,click,_,[_Idx, Txt|_]} ->
	    gs:config(Entry,{text,Txt}),
	    browser_loop(Pid,Ok,Cancel,Entry,Lb);
	{gs,Lb,doubleclick,_,[_Idx, Txt|_]} ->
	    Pid ! {browser,{ok,Txt}};
	{gs,_,destroy,_,_} ->
	    Pid ! {browser,cancel};
	X ->
	    io:format("Got X=~w~n",[X]),
	    browser_loop(Pid,Ok,Cancel,Entry,Lb)
    end.

%% ------------------------------------------------------------
%% end of browser.erl
