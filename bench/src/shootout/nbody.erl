% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
%
% contributed by Paulo Sergio Almeida
% minor optimizations by Ulf Wiger (2007-06-17)

-module(nbody).
-export([main/1]).
-export([small/0,medium/0,big/0]).

-define(pi, 3.14159265358979323).
-define(solar_mass, (4 * ?pi * ?pi)).
-define(days_per_year, 365.24).
-define(f(X), is_float(X)).

%% Small, medium, big
small() -> 10.
medium() -> 50000000. % <-- default (161.24 sec)
big() -> 100000000. % untested.

main(N) ->
  Bodies = offset_momentum(bodies()),
  energy(Bodies),
  energy(advance(N, Bodies, 0.01)).

offset_momentum(Bodies = [{X, Y, Z, _, _, _, Ms} | T])
    when ?f(X),?f(Y),?f(Z),?f(Ms) ->
  {Px, Py, Pz} = lists:foldl(fun({_, _, _, Vx, Vy, Vz, M}, {Px, Py, Pz})
                                     when ?f(Vx),?f(Vy),?f(M),?f(Px),?f(Py),
                                     ?f(Pz) ->
                               {Px + Vx * M, Py + Vy * M, Pz + Vz * M}
                             end,
                             {0.0, 0.0, 0.0},
                             Bodies),
  [{X, Y, Z, -Px/?solar_mass, -Py/?solar_mass, -Pz/?solar_mass, Ms} | T].

energy(Bodies) -> energy(Bodies, 0.0).

energy([], E) -> E;
energy([{X, Y, Z, Vx, Vy, Vz, M} | T], E)
  when ?f(X),?f(Y), ?f(Z), ?f(Vx), ?f(Vy), ?f(Vz), ?f(M), ?f(E) ->
  energy(T, lists:foldl(fun({X2, Y2, Z2, _, _, _, M2}, Ea)
                           when ?f(X2),?f(Y2),?f(Z2),?f(M2),?f(Ea) ->
                          Dx = X - X2,
                          Dy = Y - Y2,
                          Dz = Z - Z2,
                          Dist = math:sqrt(Dx*Dx + Dy*Dy + Dz*Dz),
                          Ea - M * M2 / Dist
                        end,
                        E + 0.5 * M * (Vx * Vx + Vy * Vy + Vz * Vz),
                        T)).

advance(0, Bodies, _Dt) -> Bodies;
advance(N, Bodies, Dt) -> advance(N - 1, adv2(adv1(Bodies, Dt), Dt), Dt).

%%% adv1([B], _) -> [B];
%%% adv1([B | T], Dt) ->
%%%   {B1, T1} = adv1(B, T, [], Dt),
%%%   [B1 | adv1(T1, Dt)].
adv1(Bs, Dt) ->
    adv1(Bs, Dt, []).
adv1([], _, Acc) -> Acc;
adv1([B | T], Dt, Acc) ->
  {B1, T1} = adv1(B, T, [], Dt),
    adv1(T1, Dt, [B1|Acc]).

%%%adv1(B, [],  L, _Dt) -> {B, lists:reverse(L)};
adv1(B, [],  L, _Dt) -> {B, L};
adv1({X, Y, Z, Vx, Vy, Vz, M}, [{X2, Y2, Z2, Vx2, Vy2, Vz2, M2} | T], L, Dt)
  when ?f(X), ?f(Y), ?f(Z), ?f(Vx), ?f(Vy), ?f(Vz), ?f(M), ?f(Dt),
       ?f(X2), ?f(Y2), ?f(Z2), ?f(Vx2), ?f(Vy2), ?f(Vz2), ?f(M2) ->
  Dx = X - X2,
  Dy = Y - Y2,
  Dz = Z - Z2,
  D = math:sqrt(Dx*Dx + Dy*Dy + Dz*Dz),
  Mag = Dt / (D*D*D),
  Bmm = M *Mag,
  B2mm = M2 *Mag,
  Bnew = {X, Y, Z, Vx - Dx * B2mm, Vy - Dy * B2mm, Vz - Dz * B2mm, M},
  B2new = {X2, Y2, Z2, Vx2 + Dx * Bmm, Vy2 + Dy * Bmm, Vz2 + Dz * Bmm, M2},
  adv1(Bnew, T, [B2new | L], Dt).

adv2(Bs, Dt) ->
    adv2(Bs, Dt, []).
adv2([], _, Acc) -> Acc;
adv2([{X, Y, Z, Vx, Vy, Vz, M} | T], Dt, Acc)
  when ?f(X), ?f(Y), ?f(Z), ?f(Vx), ?f(Vy), ?f(Vz), ?f(M), ?f(Dt) ->
    adv2(T, Dt, [{X + Dt * Vx, Y + Dt * Vy, Z + Dt * Vz, Vx, Vy, Vz, M}|Acc]).

bodies() ->
[
  { % sun
  0.0,
  0.0,
  0.0,
  0.0,
  0.0,
  0.0,
  ?solar_mass
  },

  { % jupiter
  4.84143144246472090e+00,
  -1.16032004402742839e+00,
  -1.03622044471123109e-01,
  1.66007664274403694e-03 * ?days_per_year,
  7.69901118419740425e-03 * ?days_per_year,
  -6.90460016972063023e-05 * ?days_per_year,
  9.54791938424326609e-04 * ?solar_mass
  },

  { % saturn
  8.34336671824457987e+00,
  4.12479856412430479e+00,
  -4.03523417114321381e-01,
  -2.76742510726862411e-03 * ?days_per_year,
  4.99852801234917238e-03 * ?days_per_year,
  2.30417297573763929e-05 * ?days_per_year,
  2.85885980666130812e-04 * ?solar_mass
  },

  { % uranus
  1.28943695621391310e+01,
  -1.51111514016986312e+01,
  -2.23307578892655734e-01,
  2.96460137564761618e-03 * ?days_per_year,
  2.37847173959480950e-03 * ?days_per_year,
  -2.96589568540237556e-05 * ?days_per_year,
  4.36624404335156298e-05 * ?solar_mass
  },

  { % neptune
  1.53796971148509165e+01,
  -2.59193146099879641e+01,
  1.79258772950371181e-01,
  2.68067772490389322e-03 * ?days_per_year,
  1.62824170038242295e-03 * ?days_per_year,
  -9.51592254519715870e-05 * ?days_per_year,
  5.15138902046611451e-05 * ?solar_mass
  }
].
