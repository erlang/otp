%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%% @doc egd_render 
%%

-module(egd_render).

-export([binary/1, binary/2]).
-export([eps/1]).
-compile(inline).

-export([line_to_linespans/3]).

-include("egd.hrl").
-define('DummyC',0).

binary(Image) ->
    binary(Image, opaque).

binary(Image, Type) ->
    parallel_binary(precompile(Image),Type).

parallel_binary(Image = #image{ height = Height },Type) ->
    case erlang:min(erlang:system_info(schedulers), Height) of
        1 ->
	    % if the height or the number of schedulers is 1
	    % do the scanlines in this process.
	    W  = Image#image.width,
	    Bg = Image#image.background,
	    Os = Image#image.objects,
	    erlang:list_to_binary([scanline(Y, Os, {0,0,W - 1, Bg}, Type)
				   || Y <- lists:seq(1, Height)]);
	Np ->
	    Pids    = start_workers(Np, Type),
    	    Handler = handle_workers(Height, Pids),
    	    init_workers(Image, Handler, Pids),
	    Res = receive_binaries(Height),
    	    finish_workers(Pids),
	    Res
    end.

start_workers(Np, Type) ->
    start_workers(Np, Type, []).

start_workers( 0,    _, Pids) -> Pids;
start_workers(Np, Type, Pids) when Np > 0 -> 
    start_workers(Np - 1, Type, [spawn_link(fun() -> worker(Type) end)|Pids]).

worker(Type) ->
    receive
	{Pid, data, #image{ objects = Os, width = W, background = Bg }} -> 
	    worker(Os, W, Bg, Type, Pid)
    end.

worker(Objects, Width, Bg, Type, Collector) ->
    receive
    	{Pid, scan, {Ys, Ye}} ->
	    lists:foreach(fun
		(Y) ->
		    Bin = erlang:list_to_binary(scanline(Y, Objects, {0,0,Width - 1, Bg}, Type)),
		    Collector ! {scan, Y, Bin}
		end, lists:seq(Ys,Ye)),
	    Pid ! {self(), scan_complete},
	    worker(Objects, Width, Bg, Type, Collector);
    	{Pid, scan, Y} ->
	    Bin = erlang:list_to_binary(scanline(Y, Objects, {0,0,Width - 1, Bg}, Type)),
	    Collector ! {scan, Y, Bin},
	    Pid ! {self(), scan_complete},
	    worker(Objects, Width, Bg, Type, Collector);
	{_, done} ->
	 ok
    end.

init_workers(_Image, _Handler, []) -> ok;
init_workers(Image, Handler, [Pid|Pids]) ->
    Pid ! {self(), data, Image}, 
    Handler ! {Pid, scan_complete},
    init_workers(Image, Handler, Pids).

handle_workers(H, Pids) ->
    spawn_link(fun() -> handle_workers(H, H, length(Pids)) end).

handle_workers(_, 0, _) -> ok;
handle_workers(H, Hi, Np) when H > 0 ->
    N = trunc(Hi/(2*Np)),
    receive 
	{Pid, scan_complete} -> 
	    if N < 2 ->
	    	Pid ! {self(), scan, Hi},
		handle_workers(H, Hi - 1, Np);
	    true ->
	    	Pid ! {self(), scan, {Hi - N, Hi}},
	   	handle_workers(H, Hi - 1 - N, Np)
	end
    end.

finish_workers([]) -> ok;
finish_workers([Pid|Pids]) ->
    Pid ! {self(), done},
    finish_workers(Pids).

receive_binaries(H) ->
    receive_binaries(H, []).

receive_binaries(0, Bins) -> erlang:list_to_binary(Bins);
receive_binaries(H, Bins) when H > 0 ->
    receive
        {scan, H, Bin} -> 
	    receive_binaries(H - 1, [Bin|Bins])
    end.

scanline(Y, Os, {_,_,Width,_}=LSB, Type) ->
    OLSs = parse_objects_on_line(Y-1, Width, Os),
    RLSs = resulting_line_spans([LSB|OLSs],Type),
    [ lists:duplicate(Xr - Xl + 1, <<(trunc(R*255)):8,(trunc(G*255)):8,(trunc(B*255)):8>>) || {_,Xl, Xr, {R,G,B,_}} <- RLSs ].

resulting_line_spans(LSs,Type) ->
    %% Build a list of "transitions" from left to right.
    Trans = line_spans_to_trans(LSs),
    %% Convert list of "transitions" to linespans.
    trans_to_line_spans(Trans,Type).

line_spans_to_trans(LSs) ->
    line_spans_to_trans(LSs,[],0).

line_spans_to_trans([],Db,_) ->
    lists:sort(Db);
line_spans_to_trans([{_,L,R,C}|LSs],Db,Z) ->
    line_spans_to_trans(LSs,[{{L,Z,start},C},{{R+1,Z,stop},C}|Db],Z+1).

trans_to_line_spans(Trans,Type) ->
    trans_to_line_spans(simplify_trans(Trans,Type,[],{0.0,0.0,0.0,0.0},[])).

trans_to_line_spans(SimpleTrans) ->
    trans_to_line_spans1(SimpleTrans,[]).

trans_to_line_spans1([],Spans) ->
    Spans;
trans_to_line_spans1([_],Spans) ->
    Spans;
trans_to_line_spans1([{L1,_},{L2,C2}|SimpleTrans],Spans) ->
    %% We are going backwards now...
    trans_to_line_spans1([{L2,C2}|SimpleTrans],[{?DummyC,L2,L1-1,C2}|Spans]).

simplify_trans([],_,_,_,Acc) ->
    Acc;
simplify_trans([{{L,_,_},_}|_] = Trans,Type,Layers,OldC,Acc) ->
    {NextTrans,RestTrans} =
	lists:splitwith(fun({{L1,_,_},_}) when L1 == L ->
				true;
			   (_) ->
				false
			end, Trans),
    {C,NewLayers} = color(NextTrans,Layers,Type,OldC),
    case OldC of
        C -> %% No change in color, so transition unnecessary.
            simplify_trans(RestTrans,Type,NewLayers,OldC,Acc);
        _ ->
            simplify_trans(RestTrans,Type,NewLayers,C,[{L,C}|Acc])
    end.

color(Trans,Layers,Type,OldC) ->
    case modify_layers(Layers,Trans) of
        Layers ->
            {OldC,Layers};
        NewLayers ->
            {color(NewLayers,Type),NewLayers}
    end.

color([],_) -> {0.0,0.0,0.0,0.0};
color([{_,C}|_],opaque) -> C;    
color(Layers,alpha) -> color1({0.0,0.0,0.0,0.0},Layers).

color1(Color,[]) -> Color;
color1(Color,[{_,C}|Layers]) -> color1(alpha_blend(Color,C),Layers).

modify_layers(Layers,[]) -> Layers;
modify_layers(Layers,[{{_,Z,start},C}|Trans]) ->
    modify_layers(add_layer(Layers, Z, C), Trans);
modify_layers(Layers,[{{_,Z,stop },C}|Trans]) ->
    modify_layers(remove_layer(Layers, Z, C), Trans).

add_layer([{Z1,_}=H|Layers],Z,C) when Z1 > Z ->
    [H|add_layer(Layers,Z,C)];
add_layer(Layers,Z,C) ->
    [{Z,C}|Layers].

remove_layer(Layers,Z,C) ->
    Layers -- [{Z,C}].

alpha_blend({R1,G1,B1,A1}, {R2,G2,B2,A2}) when is_float(A1), is_float(A2)->
  Beta = A2*(1.0 - A1),
  A = A1 + Beta,
  R = R1*A1 + R2*Beta,
  G = G1*A1 + G2*Beta,
  B = B1*A1 + B2*Beta,
  {R,G,B,A}.

parse_objects_on_line(Y, Width, Objects) ->
    parse_objects_on_line(Y, 1, Width, Objects, []).
parse_objects_on_line(_Y, _Z, _, [], Out) -> lists:flatten(Out);
parse_objects_on_line(Y, Z, Width, [O|Os], Out) ->
    case is_object_on_line(O, Y) of
    	false ->
	    parse_objects_on_line(Y, Z + 1, Width, Os, Out);
	true ->
	    OLs  = object_line_data(O,Y,Z),
	    TOLs = trim_object_line_data(OLs, Width),
	    parse_objects_on_line(Y, Z + 1, Width, Os, [TOLs|Out])
    end.

trim_object_line_data(OLs, Width) ->
    trim_object_line_data(OLs, Width, []).
trim_object_line_data([], _, Out) -> Out;

trim_object_line_data([{_, Xl, _, _}|OLs], Width, Out) when Xl > Width ->
    trim_object_line_data(OLs, Width, Out);
trim_object_line_data([{_, _, Xr, _}|OLs], Width, Out) when Xr < 0 ->
    trim_object_line_data(OLs, Width, Out);
trim_object_line_data([{Z, Xl, Xr, C}|OLs], Width, Out) ->
    trim_object_line_data(OLs, Width, [{Z, erlang:max(0,Xl), erlang:min(Xr,Width), C}|Out]).

% object_line_data
% In:
%	Object :: image_object()
%	Y :: index of height
%	Z :: index of depth
% Out:
%	OLs = [{Z, Xl, Xr, Color}]
%	Z = index of height
%	Xl = left X index
%	Xr = right X index 
% Purpose:
%	Calculate the length (start and finish index) of an objects horizontal
%	line given the height index.

object_line_data(#image_object{type=rectangle,
                               span={X0,Y0,X1,Y1}, color=C}, Y, Z) ->
    if
        Y0 =:= Y ; Y1 =:= Y ->
            [{Z, X0, X1, C}];
        true ->
            [{Z, X0, X0, C},
             {Z, X1, X1, C}]
    end;

object_line_data(#image_object{type=filled_rectangle,
                               span={X0, _, X1, _}, color=C}, _Y, Z) ->
    [{Z, X0, X1, C}];

object_line_data(#image_object{type=filled_ellipse,
                               internals={Xr,Yr,Yr2}, span={X0,Y0,X1,Y1}, color=C}, Y, Z) ->
    if 
        X1 - X0 =:= 0; Y1 - Y0 =:= 0 ->
            [{Z, X0, X1, C}];
        true ->
            Yo  = trunc(Y - Y0 - Yr),
            Yo2 = Yo*Yo,
            Xo  = math:sqrt((1 - Yo2/Yr2))*Xr,
            [{Z, round(X0 - Xo + Xr), round(X0 + Xo + Xr), C}]
    end;

object_line_data(#image_object{type=filled_triangle,
                               intervals=Is, color=C}, Y, Z) ->
    case lists:keyfind(Y, 1, Is) of
   	{Y, Xl, Xr} -> [{Z, Xl, Xr, C}];
	false -> []
    end;    

object_line_data(#image_object{type=line,
                               intervals=M, color={R,G,B,_}}, Y, Z) ->
    case M of
        #{Y := Ls} -> [{Z, Xl, Xr, {R,G,B,1.0-C/255}}||{Xl,Xr,C} <- Ls];
	_ -> []
    end;

object_line_data(#image_object{type=polygon,
                               color=C, intervals=Is}, Y, Z) ->
    [{Z, Xl, Xr, C} || {Yp, Xl, Xr} <- Is, Yp =:= Y];

object_line_data(#image_object{type=text_horizontal,
                               color=C, intervals=Is}, Y, Z) ->
    [{Z, Xl, Xr, C} || {Yg, Xl, Xr} <- Is, Yg =:= Y];

object_line_data(#image_object{type=pixel,
                               span={X0,_,X1,_}, color=C}, _, Z) ->
    [{Z, X0, X1, C}].

is_object_on_line(#image_object{span={_,Y0,_,Y1}}, Y) ->
    if Y < Y0; Y > Y1 -> false;
       true -> true
    end.
    
%%% primitives to line_spans

%% compile objects to linespans

precompile(#image{objects = Os}=I) ->
    I#image{objects = precompile_objects(Os)}.

precompile_objects([]) -> [];
precompile_objects([#image_object{type=line, internals=W, points=[P0,P1]}=O|Os]) ->
    [O#image_object{intervals = linespans_to_map(line_to_linespans(P0,P1,W))}|precompile_objects(Os)];
precompile_objects([#image_object{type=filled_triangle, points=[P0,P1,P2]}=O|Os]) ->
    [O#image_object{intervals = triangle_ls(P0,P1,P2)}|precompile_objects(Os)];
precompile_objects([#image_object{type=polygon, points=Pts}=O|Os]) ->
    [O#image_object{intervals = polygon_ls(Pts)}|precompile_objects(Os)];
precompile_objects([#image_object{type=filled_ellipse, span={X0,Y0,X1,Y1}}=O|Os]) ->
    Xr  = (X1 - X0)/2,
    Yr  = (Y1 - Y0)/2,
    Yr2 = Yr*Yr,
    [O#image_object{internals={Xr,Yr,Yr2}}|precompile_objects(Os)];
precompile_objects([#image_object{type=arc, points=[P0,P1], internals=D}=O|Os]) ->
    Es = egd_primitives:arc_to_edges(P0, P1, D),
    Ls = lists:foldl(fun ({Ep0,Ep1},M) ->
                             linespans_to_map(line_to_linespans(Ep0,Ep1,1),M)
                     end, #{}, Es),
    [O#image_object{type=line, intervals=Ls}|precompile_objects(Os)];
precompile_objects([#image_object{type=text_horizontal,
                                  points=[P0], internals={Font,Text}}=O|Os]) ->
    [O#image_object{intervals=text_horizontal_ls(P0,Font,Text)}|precompile_objects(Os)];
precompile_objects([O|Os]) ->
    [O|precompile_objects(Os)].

% triangle 

triangle_ls(P1,P2,P3) ->
    % Find top point (or left most top point),
    % From that point, two lines will be drawn to the 
    % other points.
    % For each Y step, 
  	% bresenham_line_interval for each of the two lines
	% Find the left most and the right most for those lines
    % At an end point, a new line to the point already being drawn
    % repeat same procedure as above
    [Sp1, Sp2, Sp3] = tri_pt_ysort([P1,P2,P3]),   
    triangle_ls_lp(tri_ls_ysort(line_to_linespans(Sp1,Sp2,1)), Sp2,
                   tri_ls_ysort(line_to_linespans(Sp1,Sp3,1)), Sp3, []).

% There will be Y mismatches between the two lists since bresenham is not perfect.
% I can be remedied with checking intervals this could however be costly and
% it may not be necessary, depending on how exact we need the points to be.
% It should at most differ by one and endpoints should be fine.

triangle_ls_lp([],_,[],_,Out) -> Out;
triangle_ls_lp(LSs1, P1, [], P2, Out) -> 
    SLSs = tri_ls_ysort(line_to_linespans(P2,P1,1)),
    N2 = length(SLSs),
    N1 = length(LSs1),
    if 
	N1 > N2 ->
	    [_|ILSs] = LSs1,
    	    triangle_ls_lp(ILSs, SLSs, Out);
	N2 > N1 ->
	    [_|ILSs] = SLSs,
    	    triangle_ls_lp(LSs1, ILSs, Out);
	true ->
    	    triangle_ls_lp(LSs1, SLSs, Out)
    end;
triangle_ls_lp([], P1, LSs2, P2, Out) ->
    SLSs = tri_ls_ysort(line_to_linespans(P1,P2,1)),
    N1 = length(SLSs),
    N2 = length(LSs2),
    if 
	N1 > N2 ->
	    [_|ILSs] = SLSs,
    	    triangle_ls_lp(ILSs, LSs2, Out);
	N2 > N1 ->
	    [_|ILSs] = LSs2,
    	    triangle_ls_lp(SLSs, ILSs, Out);
	true ->
	    triangle_ls_lp(SLSs, LSs2, Out)
    end;
triangle_ls_lp([LS1|LSs1],P1,[LS2|LSs2],P2, Out) ->
    {Y, Xl1, Xr1,_Ca1} = LS1,
    {_, Xl2, Xr2,_Ca2} = LS2,
    Xr = lists:max([Xl1,Xr1,Xl2,Xr2]),
    Xl = lists:min([Xl1,Xr1,Xl2,Xr2]),
    triangle_ls_lp(LSs1,P1,LSs2,P2,[{Y,Xl,Xr}|Out]).

triangle_ls_lp([],[],Out) -> Out;
triangle_ls_lp([],_,Out) -> Out;
triangle_ls_lp(_,[],Out) -> Out;
triangle_ls_lp([LS1|LSs1], [LS2|LSs2], Out) ->
    {Y, Xl1, Xr1, _Ca1} = LS1,
    {_, Xl2, Xr2, _Ca2} = LS2,
    Xr = lists:max([Xl1,Xr1,Xl2,Xr2]),
    Xl = lists:min([Xl1,Xr1,Xl2,Xr2]),
    triangle_ls_lp(LSs1,LSs2,[{Y,Xl,Xr}|Out]).
       
tri_pt_ysort(Pts) ->
    % {X,Y}
    lists:sort(
	fun ({_,Y1},{_,Y2}) ->
	   if Y1 > Y2 -> false; true -> true end
	end, Pts).

tri_ls_ysort(LSs) ->
    % {Y, Xl, Xr, Ca}
    lists:sort(
	fun ({Y1,_,_,_},{Y2,_,_,_}) ->
	   if Y1 > Y2 -> false; true -> true end
	end, LSs).

% polygon_ls
% In:
%	Pts :: [{X,Y}]
% Out:
% 	LSs :: [{Y,Xl,Xr}]
% Purpose:
%	Make polygon line spans
% Algorithm:
%	1. Find the left most (lm) point
%	2. Find the two points adjacent to that point
%		The tripplet will make a triangle
%	3. Ensure no points lies within the triangle
%	4a.No points within triangle, 
%		make triangle,
%	   	remove lm point
%		1.
%	4b.point(s) within triangle,
%				


polygon_ls(Pts) ->
    % Make triangles
    Tris = polygon_tri(Pts),
    % interval triangles
    lists:flatten(polygon_tri_ls(Tris, [])).

polygon_tri_ls([], Out) -> Out;
polygon_tri_ls([{P1,P2,P3}|Tris], Out) ->
    polygon_tri_ls(Tris, [triangle_ls(P1,P2,P3)|Out]).

polygon_tri(Pts) ->
    polygon_tri(polygon_lm_pt(Pts), []).


polygon_tri([P1,P2,P3],Tris) -> [{P1,P2,P3}|Tris];
polygon_tri([P2,P1,P3|Pts], Tris) ->
    case polygon_tri_test(P1,P2,P3,Pts) of
	false -> polygon_tri(polygon_lm_pt([P2,P3|Pts]), [{P1,P2,P3}|Tris]);
	[LmPt|Ptsn] -> polygon_tri([P2,P1,LmPt,P3|Ptsn], Tris)
    end.

polygon_tri_test(P1,P2,P3, Pts) ->
    polygon_tri_test(P1,P2,P3, Pts, []).
    
polygon_tri_test(_,_,_, [], _) -> false;
polygon_tri_test(P1,P2,P3,[Pt|Pts], Ptsr) ->
    case point_inside_triangle(Pt, P1,P2,P3) of
    	false -> polygon_tri_test(P1,P2,P3, Pts, [Pt|Ptsr]);
    	true -> [Pt|Pts] ++ lists:reverse(Ptsr) 
    end.

% polygon_lm_pt
% In:
%	Pts ::  [{X,Y}]
% Out
%	LmPts = [{X0,Y0},{Xmin,Y0},{X1,Y1},...]
% Purpose:
%	 The order of the list is important
%	 rotate the elements until Xmin is first
%	 This is not extremly fast.

polygon_lm_pt(Pts) ->
    Xs = [X||{X,_}<-Pts],
    polygon_lm_pt(Pts, lists:min(Xs), []).

polygon_lm_pt([Pt0,{X,_}=Ptm | Pts], Xmin, Ptsr) when X > Xmin ->
    polygon_lm_pt([Ptm|Pts], Xmin, [Pt0|Ptsr]);
polygon_lm_pt(Pts, _,  Ptsr) ->
    Pts ++ lists:reverse(Ptsr).


% return true if P is inside triangle (p1,p2,p3),
% otherwise false.

points_same_side({P1x,P1y}, {P2x,P2y}, {L1x,L1y}, {L2x,L2y}) ->
    ((P1x - L1x)*(L2y - L1y) - (L2x - L1x)*(P1y - L1y) *
     (P2x - L1x)*(L2y - L1y) - (L2x - L1x)*(P2y - L1y)) >= 0.

point_inside_triangle(P, P1, P2, P3) ->
    points_same_side(P, P1, P2, P3) and 
    points_same_side(P, P2, P1, P3) and 
    points_same_side(P, P3, P1, P2).
   
%% [{Y, Xl, Xr}] -> #{Y := [{Xl,Xr}]}
%% Reorganize linspans to a map with Y as key.

linespans_to_map(Ls) ->
    linespans_to_map(Ls,#{}).
linespans_to_map([{Y,Xl,Xr,C}|Ls], M) ->
    case M of
        #{Y := Spans} -> linespans_to_map(Ls, M#{Y := [{Xl,Xr,C}|Spans]});
        _ -> linespans_to_map(Ls, M#{Y => [{Xl,Xr,C}]})
    end;
linespans_to_map([], M) ->
    M.


%% line_to_linespans
%%   Anti-aliased thick line
%%   Do it CPS style
%% In:
%%	P1 :: point()
%%	P2 :: point()
%% Out:
%%      [{Y,Xl,Xr}]
%%
line_to_linespans({X0,Y0},{X1,Y1},Wd) ->
    Dx = abs(X1-X0),
    Dy = abs(Y1-Y0),
    Sx = if X0 < X1 -> 1; true -> -1 end,
    Sy = if Y0 < Y1 -> 1; true -> -1 end,
    E0 = Dx - Dy,
    Ed = if Dx + Dy =:= 0 -> 1; true -> math:sqrt(Dx*Dx + Dy*Dy) end,
    line_to_ls(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E0,Ed,(Wd+1)/2,[]).

line_to_ls(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls0) ->
    C = max(0, 255*(abs(E - Dx+Dy)/Ed - Wd + 1)),
    Ls1 = [{Y0,X0,X0,C}|Ls0],
    line_to_ls_sx(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls1,E).

line_to_ls_sx(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls,E2) when 2*E2 > -Dx ->
    line_to_ls_sx_do(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls,E2+Dy,Y0);
line_to_ls_sx(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls,E2) ->
    line_to_ls_sy(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls,E2,X0).

line_to_ls_sx_do(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls0,E2,Y) when E2 < Ed*Wd andalso
                                                                (Y1 =/= Y orelse Dx > Dy) ->
    Y2 = Y + Sy,
    C = max(0,255*(abs(E2)/Ed-Wd+1)),
    Ls = [{Y2,X0,X0,C}|Ls0],
    line_to_ls_sx_do(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls,E2+Dx,Y2);
line_to_ls_sx_do(X0,_Y0,X1,_Y1,_Dx,_Dy,_Sx,_Sy,_E,_Ed,_Wd,Ls,_E2,_Y) when X0 =:= X1 ->
    Ls;
line_to_ls_sx_do(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls,_E2,_Y) ->
    line_to_ls_sy(X0+Sx,Y0,X1,Y1,Dx,Dy,Sx,Sy,E-Dy,Ed,Wd,Ls,E,X0).

line_to_ls_sy(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls0,E2,X) when 2*E2 =< Dy ->
    line_to_ls_sy_do(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls0,Dx-E2,X);
line_to_ls_sy(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls0,_E2,_X) ->
    line_to_ls(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls0).

line_to_ls_sy_do(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls0,E2,X) when E2 < Ed*Wd andalso
                                                                (X1 =/= X orelse Dx < Dy) ->
    X2 = X + Sx,
    C = max(0,255*(abs(E2)/Ed-Wd+1)),
    Ls = [{Y0,X2,X2,C}|Ls0],
    line_to_ls_sy_do(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls,E2+Dy,X2);
line_to_ls_sy_do(_X0,Y0,_X1,Y1,_Dx,_Dy,_Sx,_Sy,_E,_Ed,_Wd,Ls,_E2,_X) when Y0 =:= Y1 ->
    Ls;
line_to_ls_sy_do(X0,Y0,X1,Y1,Dx,Dy,Sx,Sy,E,Ed,Wd,Ls0,_E2,_X) ->
    line_to_ls(X0,Y0+Sy,X1,Y1,Dx,Dy,Sx,Sy,E+Dx,Ed,Wd,Ls0).

% Text

text_horizontal_ls(Point, Font, Chars) ->
    {_Fw,Fh} = egd_font:size(Font),
    text_intervals(Point, Fh, Font, Chars, []).
    
% This is stupid. The starting point is the top left (Ptl) but the font
% offsets is relative to the bottom right origin,
%  {Xtl,Ytl} -------------------------
%            |                       |
%            |    Glyph BoundingBox  |
%            |     --------          |
%            |     |Bitmap| Gh       |
%       FH   |-Gx0-|Data  |          |
%            |     --------          |
%            |        |              |
%            |       Gy0             |
%            |        |              |
% Glyph (0,0)------------------------- Gxm (Glyph X move)
%                     FW
% Therefore, we need Yo, which is Yo = FH - Gy0 - Gh,
% Font height minus Glyph Y offset minus Glyph bitmap data boundingbox
% height.

text_intervals( _, _, _, [], Out) -> lists:flatten(Out);
text_intervals({Xtl,Ytl}, Fh, Font, [Code|Chars], Out) ->
    {{_Gw, Gh, Gx0, Gy0, Gxm}, LSs} = egd_font:glyph(Font, Code),
    % Set offset points from translation matrix to point in TeInVe.
    Yo = Fh - Gh + Gy0,
    GLSs = text_intervals_vertical({Xtl+Gx0,Ytl+Yo},LSs, []),
    text_intervals({Xtl+Gxm,Ytl}, Fh, Font, Chars, [GLSs|Out]).

text_intervals_vertical( _, [], Out) -> Out;
text_intervals_vertical({Xtl, Ytl}, [LS|LSs], Out) -> 
    H = lists:foldl( 
	fun ({Xl,Xr}, RLSs) ->
	    [{Ytl, Xl + Xtl, Xr + Xtl}|RLSs]
	end, [], LS),
    text_intervals_vertical({Xtl, Ytl+1}, LSs, [H|Out]).


%%% E. PostScript implementation

eps(#image{ objects = Os, width = W, height = H}) ->
    list_to_binary([eps_header(W,H),eps_objects(H,Os),eps_footer()]).

eps_objects(H,Os) -> eps_objects(H,Os, []).
eps_objects(_,[], Out) -> lists:flatten(Out);
eps_objects(H,[O|Os], Out) -> eps_objects(H,Os, [eps_object(H,O)|Out]).

eps_object(H,#image_object{ type = text_horizontal, internals = {_Font,Text}, points = [{X,Y}], color={R,G,B,_}}) ->
    s("/Times-Roman findfont\n14 scalefont\nsetfont\n~.4f ~.4f ~.4f setrgbcolor\nnewpath\n~p ~p moveto\n(~s) show~n",
	[R,G,B,X,H-(Y + 10), Text]);
eps_object(H,#image_object{ type = filled_ellipse, points = [{X1,Y1p},{X2,Y2p}], color={R,G,B,_}}) ->
    Y1 = H - Y1p,
    Y2 = H - Y2p,
    Xr = trunc((X2-X1)/2),
    Yr = trunc((Y2-Y1)/2),
    Cx = X1 + Xr,
    Cy = Y1 + Yr,
    s("~.4f ~.4f ~.4f setrgbcolor\nnewpath\n~p ~p ~p ~p 0 360 ellipse fill\n", 
	[R,G,B,Cx,Cy,Xr,Yr]);
eps_object(H,#image_object{ type = arc, points = [P0, P1], internals = D, color={R,G,B,_}}) ->
    Es = egd_primitives:arc_to_edges(P0, P1, D),
    [s("~.4f ~.4f ~.4f setrgbcolor\n", [R,G,B])|lists:foldl(fun
    	({{X1,Y1},{X2,Y2}}, Eps) ->
    	    [s("newpath\n~p ~p moveto\n~p ~p lineto\n1 setlinewidth\nstroke\n", [X1,H-Y1,X2,H-Y2])|Eps]
    end, [], Es)];
 
eps_object(H,#image_object{ type = line, points = [{X1,Y1}, {X2,Y2}], color={R,G,B,_}}) ->
    s("~.4f ~.4f ~.4f setrgbcolor\nnewpath\n~p ~p moveto\n~p ~p lineto\n1 setlinewidth\nstroke\n", 
	[R,G,B,X1,H-Y1,X2,H-Y2]);
eps_object(H,#image_object{ type = rectangle, points = [{X1,Y1}, {X2,Y2}], color={R,G,B,_}}) ->
    s("~.4f ~.4f ~.4f setrgbcolor\nnewpath\n~p ~p moveto\n~p ~p lineto\n~p ~p lineto\n~p ~p lineto\n~p ~p lineto\n1 setlinewidth\nstroke\n", 
	[R,G,B,X1,H-Y1,X2,H-Y1,X2,H-Y2,X1,H-Y2,X1,H-Y1]);
eps_object(H,#image_object{ type = filled_rectangle, points = [{X1,Y1}, {X2,Y2}], color={R,G,B,_}}) ->
    s("~.4f ~.4f ~.4f setrgbcolor\nnewpath\n~p ~p moveto\n~p ~p lineto\n~p ~p lineto\n~p ~p lineto\n~p ~p lineto\nclosepath\nfill\n", 
	[R,G,B,X1,H-Y1,X2,H-Y1,X2,H-Y2,X1,H-Y2,X1,H-Y1]);
eps_object(_,_) -> "".

s(Format, Terms) -> lists:flatten(io_lib:format(Format, Terms)).

eps_header(W,H) ->
    s("%!PS-Adobe-3.0 EPSF-3.0\n%%Creator: Created by egd\n%%BoundingBox: 0 0 ~p ~p\n%%LanguageLevel: 2\n%%Pages: 1\n%%DocumentData: Clean7Bit\n",[W,H]) ++ 
    "%%BeginProlog\n/ellipse {7 dict begin\n/endangle exch def\n/startangle exch def\n/yradius exch def\n/xradius exch def\n/yC exch def\n/xC exch def\n"
    "/savematrix matrix currentmatrix def\nxC yC translate\nxradius yradius scale\n0 0 1 startangle endangle arc\nsavematrix setmatrix\nend\n} def\n"
    "%%EndProlog\n".

eps_footer() -> 
    "%%EOF\n".
