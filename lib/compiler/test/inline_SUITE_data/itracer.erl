%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
-module(itracer).
-export([itracer/0]).

%%%---------------------------------------------------------------------------
%%% 
%%%  This is a little raytracer.
%%% 
%%%---------------------------------------------------------------------------


%%----------------------------------------------------------------------------
%% Constructors.
%%----------------------------------------------------------------------------


%%----------------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------------

itracer() ->
	C1 = ccreate(),
	C2 = set_width(C1,100),
	C3 = set_height(C2,100),
	C4 = initialize(C3),
	Sphere1 = screate(40,vcreate(35,10,0),{1,0,0}),
	Sphere2 = screate(35,vcreate(-25,-25,50),{0,1,0}),
	PL = traceloop(C4,50,50,[Sphere1,Sphere2]).


%%----------------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------------

overflow_prevent(A) when A<1 -> A;
overflow_prevent(_) -> 1.


%%----------------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------------

traceloop(Camera,Width,Height,Scene) ->
	traceloop(Camera,Width,Height,0,0,Scene,[]).


traceloop(_,_,Height,_,Y,_,PL) when Height=<Y ->
	PL;

traceloop(Camera,Width,Height,X,Y,Scene,PL) when Width=<X ->
	traceloop(Camera,Width,Height,0,Y+1,Scene,PL);

traceloop(Camera,Width,Height,X,Y,Scene,PL) ->
	Ray = ray(Camera,X/Width,Y/Height),
	{R1,G1,B1} = traceray(Ray,Scene,1),
	R2 = overflow_prevent(R1),
	G2 = overflow_prevent(G1),
	B2 = overflow_prevent(B1),
	P = {trunc(R2*255), trunc(G2*255), trunc(B2*255)},
	traceloop(Camera,Width,Height,X+1,Y,Scene,[{X,Y,P}|PL]).


%%----------------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------------

traceray(Ray,Scene,Level) ->
	Hit = findintersection(Ray,Scene,Level),
	case Hit of
	  nohit -> {0,0,0};
	  {[T|Ts],Object} -> shaderay(Ray,Scene,Level,T,Object)
	end.


%%----------------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------------

% Here we loop through all the objects in the scene to find the
% closest intersection.

findintersection(_,[],_) -> nohit;

findintersection(Ray,[Object|Objects],Level) ->
	Ts = intersection(Object,Ray),
	Hit1 = findintersection(Ray,Objects,Level),
	Hit2 = closesthit(Ts,Object,Hit1).


closesthit(nohit,_,nohit) -> nohit;
closesthit(nohit,_,{[T|Ts],Obj}) when T>0 -> {[T|Ts],Obj};
closesthit(nohit,_,_) -> nohit;
closesthit([T|Ts],Obj,nohit) when T>0 -> {[T|Ts],Obj};
closesthit(_,_,nohit) -> nohit;
closesthit([T1|Ts1],Obj1,{[T2|Ts2],Obj2}) when T1>0,T1<T2 -> {[T1|Ts1],Obj1};
closesthit([T1|Ts1],Obj1,{[T2|Ts2],Obj2}) when T2>0,T2<T1 -> {[T2|Ts2],Obj2};
closesthit(_,_,_) -> nohit.



%%----------------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------------

shaderay(Ray,Scene,Level,T,Object) ->
	Direction = get_direction(Ray),
	Origin = get_origin(Ray),
	Point = add(Origin, mul(T, Direction)),
	Normal = calcnormal(Object, Point),
	Diffuse = -dot(Normal, Direction),
	ReflectionVector = reflection(Ray,Normal),
	NewOrigin = add(Point, mul(0.0001, Normal)),
	ReflectionRay = rcreate(NewOrigin,ReflectionVector),
	{Red1,Green1,Blue1} = get_color(Object),
	if
	  Level<4, Diffuse>0 ->
		{Red2,Green2,Blue2} = traceray(ReflectionRay,Scene,Level+1),
		{Diffuse*Red1 + 0.5*Red2,
		 Diffuse*Green1 + 0.5*Green2,
		 Diffuse*Blue1 + 0.5*Blue2};
	  Level<4, Diffuse<0 ->
		{0,0,0};
	  true ->
		{0,0,0}
	end.


%%----------------------------------------------------------------------------
%% Har nedan foljer bara ett gang testfunktioner....
%%----------------------------------------------------------------------------
-record(camera,{width,height,zoom,position,lookat,up,right,down,corner}).

%%%---------------------------------------------------------------------------
%%% 
%%%  Useful camera operations.
%%% 
%%%---------------------------------------------------------------------------


%%----------------------------------------------------------------------------
%% Constructors.
%%----------------------------------------------------------------------------

ccreate() ->
	#camera{width=100, height=100, zoom=256,
		position = vcreate(0,0,-256),
		lookat = vcreate(0,0,0),
		up = vcreate(0,1,0)}.



%%----------------------------------------------------------------------------
%% Selectors and modifiers.
%%----------------------------------------------------------------------------

set_width(C,Width) ->   C#camera{width=Width}.
set_height(C,Height) -> C#camera{height=Height}.
set_zoom(C,Zoom) ->     C#camera{zoom=Zoom}.
cset_position(C,Pos) ->  C#camera{position=Pos}.
set_lookat(C,Lookat) -> C#camera{lookat=Lookat}.
set_up(C,Up) ->         C#camera{up=Up}.

get_width(C) ->    C#camera.width.
get_height(C) ->   C#camera.height.
get_zoom(C) ->     C#camera.zoom.
cget_position(C) -> C#camera.position.
get_lookat(C) ->   C#camera.lookat.
get_up(C) ->       C#camera.up.



%%----------------------------------------------------------------------------
%% Operators.
%%----------------------------------------------------------------------------

initialize(C) ->
	Dir = normalize(sub(C#camera.lookat, C#camera.position)),
	Up1 = normalize(C#camera.up),
	D = dot(Up1, Dir),
	Up2 = normalize(sub(Up1, mul(D, Dir))),
	Down = mul(-1, Up2),
	Right = normalize(cross(Up2,Dir)),
	Corner1 = mul(C#camera.zoom, Dir),
	Corner2 = add(Corner1, mul(-C#camera.width/2, Right)),
	Corner3 = add(Corner2, mul(-C#camera.height/2, Down)),
	C2 = C#camera{down=Down, right=Right, corner=Corner3}.


%
% X och Y ska ligga i intervallet [0..1]
%
ray(C,X,Y) ->
	Right = mul(C#camera.width*X, C#camera.right),
	Down = mul(C#camera.height*Y, C#camera.down),
	Point = add(C#camera.corner, add(Right,Down)),
	rcreate(C#camera.position,normalize(Point)).



%%----------------------------------------------------------------------------
%%          E   N   D           O   F           F   I   L   E
%%----------------------------------------------------------------------------
-record(vector,{x,y,z}).


%%%---------------------------------------------------------------------------
%%% 
%%%  Useful vector operations.
%%% 
%%%---------------------------------------------------------------------------


%%----------------------------------------------------------------------------
%% Constructors.
%%----------------------------------------------------------------------------

vcreate() ->
	#vector{x=0,y=0,z=0}.


vcreate(X,Y,Z) ->
	#vector{x=X,y=Y,z=Z}.



%%----------------------------------------------------------------------------
%% Selectors and modifiers.
%%----------------------------------------------------------------------------

set_x(V,X) -> V#vector{x=X}.
set_y(V,Y) -> V#vector{y=Y}.
set_z(V,Z) -> V#vector{z=Z}.

get_x(V) -> V#vector.x.
get_y(V) -> V#vector.y.
get_z(V) -> V#vector.z.



%%----------------------------------------------------------------------------
%% Operators.
%%----------------------------------------------------------------------------

add(A,B) ->
	#vector{x=A#vector.x+B#vector.x,
		y=A#vector.y+B#vector.y, 
		z=A#vector.z+B#vector.z}.


sub(A,B) ->
	#vector{x=A#vector.x-B#vector.x,
		y=A#vector.y-B#vector.y, 
		z=A#vector.z-B#vector.z}.


mul(T,A) ->
	#vector{x=A#vector.x * T,
		y=A#vector.y * T,
		z=A#vector.z * T}.


dot(A,B) ->
	A#vector.x*B#vector.x +
	A#vector.y*B#vector.y +
	A#vector.z*B#vector.z.


normalize(A) ->
	S = 1 / math:sqrt(dot(A,A)),
	vcreate(A#vector.x * S, A#vector.y * S, A#vector.z * S).


cross(A,B) ->
	#vector{x = A#vector.y*B#vector.z - A#vector.z*B#vector.y,
		y = A#vector.z*B#vector.x - A#vector.x*B#vector.z,
		z = A#vector.x*B#vector.y - A#vector.y*B#vector.x}.


%%----------------------------------------------------------------------------
%%          E   N   D           O   F           F   I   L   E
%%----------------------------------------------------------------------------
-record(ray,{origin,direction}).

%%%---------------------------------------------------------------------------
%%% 
%%%  Useful ray stuff.
%%% 
%%%---------------------------------------------------------------------------


%%----------------------------------------------------------------------------
%% Constructors.
%%----------------------------------------------------------------------------

rcreate() ->
	#ray{origin=vcreate(0,0,0), direction=vcreate(0,0,1)}.


rcreate(Origin,Direction) ->
	#ray{origin=Origin, direction=Direction}.



%%----------------------------------------------------------------------------
%% Selectors and modifiers.
%%----------------------------------------------------------------------------

set_origin(R,Origin) ->		R#ray{origin=Origin}.
set_direction(R,Direction) ->	R#ray{direction=Direction}.

get_origin(R) ->		R#ray.origin.
get_direction(R) ->		R#ray.direction.



%%----------------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------------

reflection(R,N) ->
	A = mul(2*dot(N, R#ray.direction), N),
	normalize(sub(R#ray.direction, A)).



%%----------------------------------------------------------------------------
%%          E   N   D           O   F           F   I   L   E
%%----------------------------------------------------------------------------
-record(sphere,{radius,position,color}).

%%%---------------------------------------------------------------------------
%%% 
%%%  Useful sphere operations.
%%% 
%%%---------------------------------------------------------------------------


%%----------------------------------------------------------------------------
%% Constructors.
%%----------------------------------------------------------------------------

screate() ->
	#sphere{radius=1, position=vcreate(0,0,0), color={1,1,1}}.


screate(Radius,Position,Color) ->
	#sphere{radius=Radius, position=Position, color=Color}.



%%----------------------------------------------------------------------------
%% Selectors and modifiers.
%%----------------------------------------------------------------------------

set_radius(S,Radius) ->		S#sphere{radius=Radius}.
sset_position(S,Position) ->	S#sphere{position=Position}.
set_color(S,Color) ->		S#sphere{color=Color}.

get_radius(S) ->		S#sphere.radius.
sget_position(S) ->		S#sphere.position.
get_color(S) ->			S#sphere.color.



%%----------------------------------------------------------------------------
%% Calculates the intersection between a ray and the sphere.
%%----------------------------------------------------------------------------

intersection(S,Ray) ->
	SR = sub(S#sphere.position,get_origin(Ray)),
	B = dot(SR,get_direction(Ray)),
	C = dot(SR,SR),
	Root = B*B-C + S#sphere.radius * S#sphere.radius,
	if
	  Root>0 ->
	    SquareRoot = math:sqrt(Root),
	    [B-SquareRoot,B+SquareRoot];
	  true ->
	    nohit
	end.

calcnormal(S,P) ->
	normalize(sub(P, S#sphere.position)).
	%mul(1/S#sphere.radius, sub(P, S#sphere.position)).



%%----------------------------------------------------------------------------
%%          E   N   D           O   F           F   I   L   E
%%----------------------------------------------------------------------------
