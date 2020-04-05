% file: "barnes.erl"

-module(barnes2).
-export([?MODULE/0]).

?MODULE() ->
    Stars = create_scenario(1000, 1.0),
    R = hd(loop(10,1000.0,Stars,0)),
    Str = lists:flatten(io_lib:format("~p", [R])),
    {R,Str =:= {1.00000,-1.92269e+4,-1.92269e+4,2.86459e-2,2.86459e-2}}.

create_scenario(N, M) ->
  create_scenario0(0, 0, trunc(math:sqrt(N)), M).

create_scenario0(_X, _SN, _SN, _M) -> 
  [];
create_scenario0(SN, Y, SN, M) -> 
  create_scenario0(0, Y+1, SN, M);
create_scenario0(X, Y, SN, M) ->
  XPos0 = (((20000.0 * 2) / SN) * X) - 20000.0,
  YPos0 = (((20000.0 * 2) / SN) * Y) - 20000.0,
  Calibrate = ((20000.0 * 2) / SN) / 2,
  XPos = XPos0 + Calibrate,
  YPos = YPos0 + Calibrate,
  [{M, XPos, YPos, 0.0, 0.0} | create_scenario0(X+1, Y, SN, M)].

relpos_to_quadrant(DX, DY) when DX >= 0 ->
  if
    DY >= 0 -> 0;
    true -> 3
  end;
relpos_to_quadrant(_, DY) -> 
  if
    DY >= 0 -> 1;
    true -> 2 
  end.

quadrant_to_dx(0, D) -> D;
quadrant_to_dx(1, D) -> -D;
quadrant_to_dx(2, D) -> -D;
quadrant_to_dx(3, D) -> D.

quadrant_to_dy(Q,D) ->
  if
    Q < 2 -> D;
    true -> -D
  end.

create_tree(Stars) ->
  create_tree0(Stars, empty).

create_tree0([],Tree) -> 
  Tree;
create_tree0([{M,X,Y,_,_} | Stars], Tree) ->
  create_tree0(Stars, insert_tree_element(Tree, M, X, Y, 0.0, 0.0, 20000.0)).

insert_tree_element(empty, M, X, Y, _OX, _OY, _D) ->
  {body,M,X,Y};
insert_tree_element({branch,M0,SubTree}, M, X, Y, OX, OY, D) ->
  Q = relpos_to_quadrant(X-OX,Y-OY),
  D2 = D / 2,
  DX = quadrant_to_dx(Q,D2),
  DY = quadrant_to_dy(Q,D2),
  {branch,M0+M,setelement(Q+1,SubTree,
                          insert_tree_element(element(Q+1,SubTree),
                                              M, X, Y, OX+DX, OY+DY,D2))};
insert_tree_element({body,M0,X0,Y0},M,X,Y,OX,OY,D) ->
  resolve_body_conflict(M,X,Y,M0,X0,Y0,OX,OY,D).

resolve_body_conflict(M0, X0, Y0, M1, X1, Y1, OX, OY, D) ->
  T = {empty,empty,empty,empty},
  Q0 = relpos_to_quadrant(X0-OX,Y0-OY),
  Q1 = relpos_to_quadrant(X1-OX,Y1-OY),
  D2 = D / 2,
  if
    Q0 == Q1 -> DX = quadrant_to_dx(Q0,D2),
                DY = quadrant_to_dy(Q1,D2),
                {branch,M0+M1,setelement(Q0+1,T,
                                         resolve_body_conflict(M0,X0,Y0,
                                                               M1,X1,Y1,
                                                               OX+DX,OY+DY,
                                                               D2))} ;
    true -> {branch,M0+M1, setelement(Q1+1,
                                      setelement(Q0+1,T,{body,M0,X0,Y0}),
                                      {body,M1,X1,Y1})}
  end.

compute_acceleration(empty, _, _, _, _, _,L) -> 
  {{0.0, 0.0},L+1};
compute_acceleration({body,BM,BX,BY}, _D, _OX, _OY, X, Y,L) ->
  DX = BX - X,
  DY = BY - Y,
  R2 = (DX * DX) + (DY * DY),
  Divisor = R2 * math:sqrt(R2),
  if
    Divisor < 0.000001 -> % was: Divisor < ?EPSILON -> 
      {{0.0, 0.0},L+1};
    true -> 
      Expr = BM / Divisor,
      {{DX * Expr, DY * Expr},L+1}
  end;
compute_acceleration({branch,M,SubTree}, D, OX, OY, X, Y,L) ->
  DX = OX - X,
  DY = OY - Y,
  R2 = (DX * DX) + (DY * DY),
  DD = D*D,
  R2_THETA2 = 0.09 * R2, % TRY 2.0 *R2, !!!  was: R2_THETA2 = ?THETA2*R2,
  if
    % Ok to approximate?
    DD < R2_THETA2 -> 
      Divisor = R2 * math:sqrt(R2),
      if
        Divisor < 0.000001 ->
          {{0.0,0.0},L};
        true -> 
          Expr = M / Divisor,
          {{DX*Expr, DY*Expr},L+1}
        end;
    % Not ok to approximate...
    true -> 
      D2 = D / 2,
      {{AX0, AY0},L1} = compute_acceleration(element(1,SubTree),
                                        D2, OX + quadrant_to_dx(0,D2),
                                        OY + quadrant_to_dy(0,D2),X,Y,L),
      {{AX1, AY1},L2} = compute_acceleration(element(2,SubTree),
                                        D2, OX + quadrant_to_dx(1,D2),
                                        OY + quadrant_to_dy(1,D2),X,Y,L1),
      {{AX2, AY2},L3} = compute_acceleration(element(3,SubTree),
                                        D2,OX + quadrant_to_dx(2,D2),
                                        OY + quadrant_to_dy(2,D2),X,Y,L2),
      {{AX3, AY3},L4} = compute_acceleration(element(4,SubTree),
                                        D2, OX + quadrant_to_dx(3,D2),
                                        OY + quadrant_to_dy(3,D2),X,Y,L3),
      {{AX0+AX1+AX2+AX3, AY0+AY1+AY2+AY3},L4+1}
  end.

compute_star_accelerations(_Tree,[],L) -> 
  {[],L};
compute_star_accelerations(Tree,[{_,X, Y,_,_}|Stars],L) ->
  {A,AL} = compute_acceleration(Tree, 20000.0, 0.0, 0.0, X, Y,L),
  {B,BL} = compute_star_accelerations(Tree, Stars,AL),
  {[A | B],BL}.

compute_next_state([],_,_) -> 
  [];
compute_next_state([{M,X,Y,VX,VY}|Stars],[{AX,AY}|Accs],Time) ->
  VX0 = VX + (AX * Time),
  VY0 = VY + (AY * Time),
  [{M,X+(VX*Time),Y+(VY*Time),VX0,VY0} | compute_next_state(Stars,Accs,Time)].

advance_time(Time,Stars,L) ->
  Tree = create_tree(Stars),
  {Acc,NL} = compute_star_accelerations(Tree, Stars,L),
  {compute_next_state(Stars, Acc, Time),NL}.

loop(0,_Time,Stars,_L) -> 
  Stars;
loop(N,Time,Stars,L) -> 
  {NS,NL} = advance_time(Time,Stars,L),
  loop(N-1,Time,NS,NL).
