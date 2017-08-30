%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

-module(reltool_fgraph).

-export([
	step/2,
	step/3
	]).

-export([
	new/0,
	
	add/3,
	set/3,
	del/2,

	is_defined/2,
	get/2,
	size/1,

	foreach/2,
	map/2,
	foldl/3,
	mapfoldl/3
	]).

-compile(inline).
-compile({inline_size, 128}).

-include("reltool_fgraph.hrl").

%% KEY-VALUE STORAGE Process dictionary
new() -> [].

is_defined(Key, _Fg) ->
    case get(Key) of
	undefined -> false;
	_ -> true
    end.

get(K, _Fg) ->
    case get(K) of
	{_, V} -> V;
	_ -> undefined
    end.

add(Key, Value, Fg) ->
    put(Key, {Key, Value}),
    [Key|Fg].

set(Key, Value, Fg) ->
    put(Key, {Key, Value}),
    Fg.

size(Fg) -> length(Fg).

del(Key, Fg) ->
    erase(Key),
    lists:delete(Key, Fg).

foreach(Fun, Fg) ->
    lists:foreach(fun
	(Key) -> Fun(get(Key))
    end, Fg),
    Fg.

map(Fun, Fg) ->
    lists:foreach(fun
	(Key) -> put(Key,Fun(get(Key)))
    end, Fg),
    Fg.

foldl(Fun, I, Fg) ->
    lists:foldl(fun
	(Key, Out) ->
	    Fun(get(Key), Out)
	end, I, Fg).

mapfoldl(Fun, I, Fg) ->
    Acc = lists:foldl(fun
	(Key, Out) ->
	    {Value, Acc} = Fun(get(Key), Out),
	    put(Key, Value),
	    Acc
	end, I, Fg),
    {Fg, Acc}.

step(Vs, Es) -> step(Vs, Es, {0,0}).
step(Vs, Es, Pa) ->
    ?MODULE:map(fun
	(Node = {_, #fg_v{ type = static }}) -> Node;
	({Key, Value = #fg_v{ p = {Px, Py}, v = {Vx, Vy}, type = dynamic}})
		      when is_float(Px), is_float(Py),
			   is_float(Vx), is_float(Vy) ->
	    F0 = {0.0,0.0},
	    F1 = coulomb_repulsion(Key, Value, Vs, F0),
	    F2 = hooke_attraction(Key, Value, Vs, Es, F1),
	    F3 = point_attraction(Key, Value, Pa, F2),

	    {Fx, Fy} = F3,

	    Vx1 = (Vx + ?fg_th*Fx)*?fg_damp,
	    Vy1 = (Vy + ?fg_th*Fy)*?fg_damp,

	    Px1 = Px + ?fg_th*Vx1,
	    Py1 = Py + ?fg_th*Vy1,

    	    {Key, Value#fg_v{ p = {Px1, Py1}, v = {Vx1, Vy1}}};
	(Node) -> Node
    end, Vs).

point_attraction(_, #fg_v{ p = P0 }, Pa, {Fx, Fy})
  when is_float(Fx), is_float(Fy) ->
    K = 20,
    L = 150,
    {R, {Cx,Cy}} = composition(P0, Pa),
    F = -K*?fg_stretch*(R - L),
    {Fx + Cx*F, Fy + Cy*F}.

coulomb_repulsion(K0, #fg_v{ p = P0, q = Q0}, Vs, {Fx0, Fy0})
  when is_float(Fx0), is_float(Fy0) ->
    ?MODULE:foldl(fun
	({K1, _}, F) when K1 == K0 -> F;
	({_, #fg_v{ p = P1, q = Q1}}, {Fx, Fy}) ->
	    {R, {Cx, Cy}} = composition(P0, P1),
	    F = ?fg_kc*(Q1*Q0)/(R*R+0.0001),
	    {Fx + Cx*F, Fy + Cy*F};
	(_, F) -> F
    end, {Fx0, Fy0}, Vs).

hooke_attraction(Key0, #fg_v{ p = P0 }, Vs, Es, {Fx0, Fy0})
  when is_float(Fx0), is_float(Fy0) ->
    ?MODULE:foldl(fun
	({{Key1,Key1}, _}, F) -> F;
	({{Key1,Key2}, #fg_e{ l = L, k = K}}, {Fx, Fy}) when Key1 =:= Key0->
	    #fg_v{ p = P1} = ?MODULE:get(Key2, Vs),
	    {R, {Cx,Cy}} = composition(P0, P1),
	    F = -K*?fg_stretch*(R - L),
	    {Fx + Cx*F, Fy + Cy*F};
	({{Key2,Key1}, #fg_e{ l = L, k = K}}, {Fx, Fy}) when Key1 =:= Key0->
	    #fg_v{ p = P1} = ?MODULE:get(Key2, Vs),
	    {R, {Cx,Cy}} = composition(P0, P1),
	    F = -K*?fg_stretch*(R - L),
	    {Fx + Cx*F, Fy + Cy*F};
	(_, F) -> F
    end, {Fx0, Fy0}, Es).

composition({Px1, Py1}, {Px0, Py0})
  when is_float(Px1), is_float(Py1), is_float(Px0), is_float(Py0) ->
    Dx  = Px1 - Px0,
    Dy  = Py1 - Py0,
    R   = math:sqrt(Dx*Dx + Dy*Dy + 0.001),
    {R, {Dx/R, Dy/R}}.
