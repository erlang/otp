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
%% ------------------------------------------------------------

-module(calc).
-compile([{nowarn_deprecated_function,{gs,button,2}},
          {nowarn_deprecated_function,{gs,config,2}},
          {nowarn_deprecated_function,{gs,label,2}},
          {nowarn_deprecated_function,{gs,start,0}},
          {nowarn_deprecated_function,{gs,window,2}}]).

-export([start/0,calc/0]).

start() ->
    spawn(calc,calc,[]).

calc() ->
    I = gs:start(),
    Win = gs:window(I,[{title,"Calc1"},{width,120},{height,150}]),
    Label = gs:label(Win,[{label,{text,"0"}},{width,120}]),
    B1 = gs:button(Win,[{label,{text,"1"}},{width,30},{x,0},{y,30}]),
    B2 = gs:button(Win,[{label,{text,"2"}},{width,30},{x,30},{y,30}]),
    B3 = gs:button(Win,[{label,{text,"3"}},{width,30},{x,60},{y,30}]),
    B4 = gs:button(Win,[{label,{text,"4"}},{width,30},{x,0},{y,60}]),
    B5 = gs:button(Win,[{label,{text,"5"}},{width,30},{x,30},{y,60}]),
    B6 = gs:button(Win,[{label,{text,"6"}},{width,30},{x,60},{y,60}]),
    B7 = gs:button(Win,[{label,{text,"7"}},{width,30},{x,0},{y,90}]),
    B8 = gs:button(Win,[{label,{text,"8"}},{width,30},{x,30},{y,90}]),
    B9 = gs:button(Win,[{label,{text,"9"}},{width,30},{x,60},{y,90}]),
    B0 = gs:button(Win,[{label,{text,"0"}},{width,60},{x,0},{y,120}]),
    C = gs:button(Win,[{label,{text,"C"}},{width,30},{x,60},{y,120}]),
    AC = gs:button(Win,[{label,{text,"AC"}},{width,30},{x,90},{y,120},{fg,red}]),
    Plus = gs:button(Win,[{label,{text,"+"}},{width,30},{x,90},{y,30}]),
    Times = gs:button(Win,[{label,{text,"*"}},{width,30},{x,90},{y,60}]),
    Minus = gs:button(Win,[{label,{text,"-"}},{width,30},{x,90},{y,90}]),
    gs:config(Win,{map,true}),
    Ids = [Label,B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,Minus,Plus,Times,C,AC],
    calc_loop(Ids,0,0,'+').

calc_loop(Ids,M,V,Op) ->
    [_Label,B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,Minus,Plus,Times,C,AC] = Ids,
    receive
	{gs,B0,click,_,_} -> digit_press(Ids,M,V*10+0,Op);
	{gs,B1,click,_,_} -> digit_press(Ids,M,V*10+1,Op);
	{gs,B2,click,_,_} -> digit_press(Ids,M,V*10+2,Op);
	{gs,B3,click,_,_} -> digit_press(Ids,M,V*10+3,Op);
	{gs,B4,click,_,_} -> digit_press(Ids,M,V*10+4,Op);
	{gs,B5,click,_,_} -> digit_press(Ids,M,V*10+5,Op);
	{gs,B6,click,_,_} -> digit_press(Ids,M,V*10+6,Op);
	{gs,B7,click,_,_} -> digit_press(Ids,M,V*10+7,Op);
	{gs,B8,click,_,_} -> digit_press(Ids,M,V*10+8,Op);
	{gs,B9,click,_,_} -> digit_press(Ids,M,V*10+9,Op);
	{gs,Minus,click,_,_} -> calc(Ids,Op,M,V,'-');
	{gs,Plus,click,_,_} ->  calc(Ids,Op,M,V,'+');
	{gs,Times,click,_,_} -> calc(Ids,Op,M,V,'*');
	{gs,AC,click,_,_} -> ac(Ids,M,V,Op);
	{gs,C,click,_,_} -> c(Ids,M,V,Op);
	{gs,_,destroy,_,_} -> exit(normal);
	_Other -> calc_loop(Ids,M,V,Op)
    end.

digit_press(Ids,M,V,Op) ->
    [Label|_]=Ids,
    gs:config(Label,[{label,{text,V}}]),
    calc_loop(Ids,M,V,Op).

calc(Ids,'+',M,V,Op) ->
    NewM = M + V,
    [Label|_]=Ids,
    gs:config(Label,[{label,{text,NewM}}]),
    calc_loop(Ids,NewM,0,Op);
calc(Ids,'-',M,V,Op) ->
    NewM = M - V,
    [Label|_]=Ids,
    gs:config(Label,[{label,{text,NewM}}]),
    calc_loop(Ids,NewM,0,Op);
calc(Ids,'*',M,V,Op) ->
    NewM = M * V,
    [Label|_]=Ids,
    gs:config(Label,[{label,{text,NewM}}]),
    calc_loop(Ids,NewM,0,Op).

c(Ids,M,_V,Op) ->
    [Label|_]=Ids,
    gs:config(Label,[{label,{text,0}}]),
    calc_loop(Ids,M,0,Op).

ac(Ids,_M,_V,_Op) ->
    [Label|_]=Ids,
    gs:config(Label,[{label,{text,0}}]),
    calc_loop(Ids,0,0,'+').

%% ------------------------------------------------------------
