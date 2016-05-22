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
%% Purpose : Fractal trees

-module(frac).
-compile([{nowarn_deprecated_function,{gs,config,2}},
          {nowarn_deprecated_function,{gs,create,3}},
          {nowarn_deprecated_function,{gs,start,0}}]).

-export([start/0, go/0, test/0, grow/2, expand/3, subst/2]).

%% 0L - grammer -- context insensitive lindenmayer grammer

start() ->
    spawn(frac,go,[]).

go() ->
    draw(),
    receive
	_X -> true
    end.

draw() ->
    S=gs:start(),
    Width = 800,
    Ht = 520,
    Title="Context Insensitive Lindenmayer Grammer (L0) Trees",
    Win=gs:create(window,S,[{title,Title},{width,Width},{height,Ht}]),
    Canvas=gs:create(canvas,Win,[{width,Width},{height,Ht},{bg,{237,224,189}}]),
    gs:config(Win,[{iconname,"Plants"},{map,true}]),
    draw(Canvas, 1, Width, Ht),
    draw(Canvas, 2, Width, Ht),
    draw(Canvas, 3, Width, Ht),
    draw(Canvas, 4, Width, Ht).

draw(Graph, Index, Width, Ht) ->
    draw_frac(Graph, Index, Width, Ht).
    
test() ->
    grow(3,1).

grow(NGens, RuleNumber) ->
    lists:flatten(expand(NGens, RuleNumber, [a])).

rule(1,a) -> [b,'[',a,']',b,'(',a,')',a];
rule(1,b) -> [b,b];

rule(2,a) -> [b,'[',a,'[',b,a,']',']'];
rule(2,b) -> [b,'(','(',b,')',a,')',c];
rule(2,c) -> [c,d];

rule(3,a) -> [d,'[',d,b,e,']','(',d,c,e,')'];
rule(3,b) -> [d,'[',d,a,f,']','(',d,c,f,')',f];
rule(3,c) -> [d,'[',d,b,g,']','(',d,a,g,')',g];

rule(4,a) -> [c,'(',b,a,'(',b,')',')',c,'[',b,a,'[',b,']',']'];
rule(4,b) -> [c,'(',b,e,')',c,'[',b,f,']'];
rule(4,c) -> [g,c,c];

rule(_,X) -> X.


step(a) -> 1.0;
step(b) -> 0.8;
step(c) -> 0.6;
step(d) -> 0.7;
step(e) -> 0.6;
step(f) -> 0.65;
step(g) -> 0.75;
step(_) -> 1.0.

start_coords(1) -> {0.8,0.8};
start_coords(2) -> {0.6,0.8};
start_coords(3) -> {0.4,0.8};
start_coords(4) -> {0.2,0.8};
start_coords(_) -> {0.5, 0.5}.

gens(1) -> 5;
gens(_) -> 5.

scale(1) -> 5;
scale(2) -> 40;
scale(3) -> 40;
scale(4) -> 4;
scale(_) -> 5.

expand(0,_,X) ->
    X;
expand(N,Index,X) ->
    expand(N - 1, Index, lists:flatten(subst(X, Index))).


subst([],_) -> [];
subst([H|T],Index) ->
    [rule(Index,H)|subst(T,Index)].


draw_frac(Id, Index, Width, Ht) ->
    X0 = 100,
    Y0 = 100,
    {XScale,YScale} = start_coords(Index),
    Xstart = trunc(X0 + Width*XScale),
    Ystart = trunc(Y0 + Ht*YScale),
    Angle = 270.0 * 3.14159 / 180.0,
    Scale = scale(Index),
    N = gens(Index),
    Tree = grow(N,Index),
    drawit(Tree, Id, Xstart, Ystart, Angle, Scale, []).

% drawit(Tree,S0,S).

drawit([],_,_,_,_,_,_) ->
    true;
drawit(['('|T],Id,X0,Y0,Ang,Scale,Stack) ->
    Ang1 =  Ang + (20.0 * 3.14159 / 180.0),
    Scale1 = Scale * 0.8,
    drawit(T,Id,X0,Y0,Ang1,Scale1,[{X0,Y0,Ang,Scale}|Stack]);
drawit(['['|T],Id,X0,Y0,Ang,Scale,Stack) ->
    Ang1 = Ang - (40.0 * 3.14159 / 180.0),
    Scale1 = Scale * 0.8,
    drawit(T,Id,X0,Y0,Ang1,Scale1,[{X0,Y0,Ang,Scale}|Stack]);
drawit([')'|T],Id,_,_,_,_,[{X1,Y1,Ang1,Scale1}|Stack]) ->
    drawit(T,Id,X1,Y1,Ang1,Scale1,Stack);
drawit([']'|T],Id,_,_,_,_,[{X1,Y1,Ang1,Scale1}|Stack]) ->
    drawit(T,Id,X1,Y1,Ang1,Scale1,Stack);
drawit([Symbol|T],Id,X0,Y0,Ang,Scale,Stack) ->
    Size = step(Symbol),
    L = Size * Scale,
    {X1, Y1} = plotit(Id,X0,Y0,L,Ang),
    drawit(T,Id,X1,Y1,Ang,Scale,Stack).

plotit(Id,X0,Y0,L,A) ->
    CosA = math:cos(A),
    SinA = math:sin(A),
    X = trunc(X0 + L*CosA),
    Y = trunc(Y0 + L*SinA),
    gs:create(line,Id,[{coords,[{X0,Y0},{X,Y}]}]),
    {X,Y}.
	
