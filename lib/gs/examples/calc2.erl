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
%% A Simple Calculator demo in Erlang
%% Describes how to match against the data field.
%% ------------------------------------------------------------

-module(calc2).
-compile([{nowarn_deprecated_function,{gs,button,2}},
          {nowarn_deprecated_function,{gs,config,2}},
          {nowarn_deprecated_function,{gs,label,2}},
          {nowarn_deprecated_function,{gs,start,0}},
          {nowarn_deprecated_function,{gs,window,2}}]).

-export([start/0,calc/0]).

start() ->
    spawn(calc2,calc,[]).

calc() ->
    I = gs:start(),
    Win = gs:window(I,[{title,"Calc2"},{width,120},{height,150}]),
    Lbl = gs:label(Win,[{label,{text,"0"}},{width,120}]),
    gs:button(Win,[{label,{text,"1"}},{width,30},{x,0},{y,30},{data,1}]),
    gs:button(Win,[{label,{text,"2"}},{width,30},{x,30},{y,30},{data,2}]),
    gs:button(Win,[{label,{text,"3"}},{width,30},{x,60},{y,30},{data,3}]),
    gs:button(Win,[{label,{text,"4"}},{width,30},{x,0},{y,60},{data,4}]),
    gs:button(Win,[{label,{text,"5"}},{width,30},{x,30},{y,60},{data,5}]),
    gs:button(Win,[{label,{text,"6"}},{width,30},{x,60},{y,60},{data,6}]),
    gs:button(Win,[{label,{text,"7"}},{width,30},{x,0},{y,90},{data,7}]),
    gs:button(Win,[{label,{text,"8"}},{width,30},{x,30},{y,90},{data,8}]),
    gs:button(Win,[{label,{text,"9"}},{width,30},{x,60},{y,90},{data,9}]),
    gs:button(Win,[{label,{text,"0"}},{width,60},{x,0},{y,120},{data,0}]),
    gs:button(Win,[{label,{text,"C"}},{width,30},{x,60},{y,120},{data,'C'}]),
    gs:button(Win,[{label,{text,"AC"}},{width,30},{x,90},{y,120},{data,'AC'},
		   {fg,red}]),
    gs:button(Win,[{label,{text,"+"}},{width,30},{x,90},{y,30},{data,'+'}]),
    gs:button(Win,[{label,{text,"*"}},{width,30},{x,90},{y,60},{data,'*'}]),
    gs:button(Win,[{label,{text,"-"}},{width,30},{x,90},{y,90},{data,'-'}]),
    gs:config(Win,{map,true}),
    calc_loop(Lbl,0,0,'+').

calc_loop(Lbl,M,V,Op) ->
    receive
	{gs,_,click,D,_} when is_integer(D) -> 
	    digit_press(Lbl,M,V*10+D,Op);
	{gs,_,click,'C',_} -> 
	    c(Lbl,M,V,Op);
	{gs,_,click,'AC',_} -> 
	    ac(Lbl,M,V,Op);
	{gs,_,click,NewOp,_} ->  
	    calc(Lbl,Op,M,V,NewOp);
	{gs,_,destroy,_,_} ->
	    exit(normal);
	_Other -> 
	    calc_loop(Lbl,M,V,Op)
    end.

digit_press(Lbl,M,V,Op) ->
    gs:config(Lbl,[{label,{text,V}}]),
    calc_loop(Lbl,M,V,Op).

calc(Lbl,'+',M,V,Op) ->
    NewM = M + V,
    gs:config(Lbl,[{label,{text,NewM}}]),
    calc_loop(Lbl,NewM,0,Op);
calc(Lbl,'-',M,V,Op) ->
    NewM = M - V,
    gs:config(Lbl,[{label,{text,NewM}}]),
    calc_loop(Lbl,NewM,0,Op);
calc(Lbl,'*',M,V,Op) ->
    NewM = M * V,
    gs:config(Lbl,[{label,{text,NewM}}]),
    calc_loop(Lbl,NewM,0,Op).

c(Lbl,M,_V,Op) ->
    gs:config(Lbl,[{label,{text,0}}]),
    calc_loop(Lbl,M,0,Op).

ac(Lbl,_M,_V,_Op) ->
    gs:config(Lbl,[{label,{text,0}}]),
    calc_loop(Lbl,0,0,'+').

%% ------------------------------------------------------------
