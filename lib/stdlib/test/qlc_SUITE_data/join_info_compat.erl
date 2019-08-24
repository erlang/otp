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
-module(join_info_compat).

-compile(export_all).

create_handle() ->
    H1 = qlc:sort([{192.0,1,a},{192.0,2,b},{192.0,3,c}]),
    qlc:q({qlc_lc,
           % fun-info: {23,109048965,'-create_handle/0-fun-23-'}
           fun() ->
                  {qlc_v1,
                   % fun-info: {2,105724313,'-create_handle/0-fun-2-'}
                   fun(S01_0_1, RL01_0_1, Go01_0_1) ->
                          Fun1_0_1 =
                              % fun-info: {1,131900588,'-create_handle/0-fun-1-'}
                              fun(0, RL1_0_1, _, _, _, _, _, _, _)
                                     when is_list(RL1_0_1) ->
                                     lists:reverse(RL1_0_1);
                                 (0, _, _, _, _, _, _, _, _) ->
                                     [];
                                 (1,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  C1_3_1,
                                  Y1,
                                  C1_1_1,
                                  B1,
                                  X1)
                                     when is_list(RL1_0_1) ->
                                     Fun1_0_1(element(1, Go1_0_1),
                                              [{X1,Y1}|RL1_0_1],
                                              Fun1_0_1,
                                              Go1_0_1,
                                              C1_3_1,
                                              Y1,
                                              C1_1_1,
                                              B1,
                                              X1);
                                 (1,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  C1_3_1,
                                  Y1,
                                  C1_1_1,
                                  B1,
                                  X1) ->
                                     [{X1,Y1}|
                                      % fun-info: {0,27702789,'-create_handle/0-fun-0-'}
                                      fun() ->
                                             Fun1_0_1(element(1,
                                                              Go1_0_1),
                                                      RL1_0_1,
                                                      Fun1_0_1,
                                                      Go1_0_1,
                                                      C1_3_1,
                                                      Y1,
                                                      C1_1_1,
                                                      B1,
                                                      X1)
                                      end];
                                 (2,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  C1_3_1,
                                  Y1,
                                  _,
                                  B1,
                                  X1) ->
                                     Fun1_0_1(3,
                                              RL1_0_1,
                                              Fun1_0_1,
                                              Go1_0_1,
                                              C1_3_1,
                                              Y1,
                                              element(4, Go1_0_1),
                                              B1,
                                              X1);
                                 (3,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  C1_3_1,
                                  Y1,
                                  [{B1,X1,_}|C1_0_1],
                                  _,
                                  _) ->
                                     Fun1_0_1(element(3, Go1_0_1),
                                              RL1_0_1,
                                              Fun1_0_1,
                                              Go1_0_1,
                                              C1_3_1,
                                              Y1,
                                              C1_0_1,
                                              B1,
                                              X1);
                                 (3,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  C1_3_1,
                                  Y1,
                                  [_|C1_0_1],
                                  _,
                                  _) ->
                                     Fun1_0_1(3,
                                              RL1_0_1,
                                              Fun1_0_1,
                                              Go1_0_1,
                                              C1_3_1,
                                              Y1,
                                              C1_0_1,
                                              [],
                                              []);
                                 (3,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  C1_3_1,
                                  Y1,
                                  [],
                                  _,
                                  _) ->
                                     Fun1_0_1(element(2, Go1_0_1),
                                              RL1_0_1,
                                              Fun1_0_1,
                                              Go1_0_1,
                                              C1_3_1,
                                              Y1,
                                              [],
                                              [],
                                              []);
                                 (3,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  C1_3_1,
                                  Y1,
                                  C1_1_1,
                                  _,
                                  _) ->
                                     case C1_1_1() of
                                         [{B1,X1,_}|C1_0_1] ->
                                             Fun1_0_1(element(3,
                                                              Go1_0_1),
                                                      RL1_0_1,
                                                      Fun1_0_1,
                                                      Go1_0_1,
                                                      C1_3_1,
                                                      Y1,
                                                      C1_0_1,
                                                      B1,
                                                      X1);
                                         [_|C1_0_1] ->
                                             Fun1_0_1(3,
                                                      RL1_0_1,
                                                      Fun1_0_1,
                                                      Go1_0_1,
                                                      C1_3_1,
                                                      Y1,
                                                      C1_0_1,
                                                      [],
                                                      []);
                                         [] ->
                                             Fun1_0_1(element(2,
                                                              Go1_0_1),
                                                      RL1_0_1,
                                                      Fun1_0_1,
                                                      Go1_0_1,
                                                      C1_3_1,
                                                      Y1,
                                                      [],
                                                      [],
                                                      []);
                                         E1_0_1 ->
                                             E1_0_1
                                     end;
                                 (4,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  C1_3_1,
                                  Y1,
                                  C1_1_1,
                                  B1,
                                  X1) ->
                                     if
                                         B1 =:= 192.0 ->
                                             Fun1_0_1(element(6,
                                                              Go1_0_1),
                                                      RL1_0_1,
                                                      Fun1_0_1,
                                                      Go1_0_1,
                                                      C1_3_1,
                                                      Y1,
                                                      C1_1_1,
                                                      B1,
                                                      X1);
                                         true ->
                                             Fun1_0_1(element(5,
                                                              Go1_0_1),
                                                      RL1_0_1,
                                                      Fun1_0_1,
                                                      Go1_0_1,
                                                      C1_3_1,
                                                      Y1,
                                                      C1_1_1,
                                                      B1,
                                                      X1)
                                     end;
                                 (5,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  _,
                                  Y1,
                                  C1_1_1,
                                  B1,
                                  X1) ->
                                     Fun1_0_1(6,
                                              RL1_0_1,
                                              Fun1_0_1,
                                              Go1_0_1,
                                              element(9, Go1_0_1),
                                              Y1,
                                              C1_1_1,
                                              B1,
                                              X1);
                                 (6,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  [{Y1}|C1_0_1],
                                  _,
                                  C1_1_1,
                                  B1,
                                  X1) ->
                                     Fun1_0_1(element(8, Go1_0_1),
                                              RL1_0_1,
                                              Fun1_0_1,
                                              Go1_0_1,
                                              C1_0_1,
                                              Y1,
                                              C1_1_1,
                                              B1,
                                              X1);
                                 (6,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  [_|C1_0_1],
                                  _,
                                  C1_1_1,
                                  B1,
                                  X1) ->
                                     Fun1_0_1(6,
                                              RL1_0_1,
                                              Fun1_0_1,
                                              Go1_0_1,
                                              C1_0_1,
                                              [],
                                              C1_1_1,
                                              B1,
                                              X1);
                                 (6,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  [],
                                  _,
                                  C1_1_1,
                                  B1,
                                  X1) ->
                                     Fun1_0_1(element(7, Go1_0_1),
                                              RL1_0_1,
                                              Fun1_0_1,
                                              Go1_0_1,
                                              [],
                                              [],
                                              C1_1_1,
                                              B1,
                                              X1);
                                 (6,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  C1_3_1,
                                  _,
                                  C1_1_1,
                                  B1,
                                  X1) ->
                                     case C1_3_1() of
                                         [{Y1}|C1_0_1] ->
                                             Fun1_0_1(element(8,
                                                              Go1_0_1),
                                                      RL1_0_1,
                                                      Fun1_0_1,
                                                      Go1_0_1,
                                                      C1_0_1,
                                                      Y1,
                                                      C1_1_1,
                                                      B1,
                                                      X1);
                                         [_|C1_0_1] ->
                                             Fun1_0_1(6,
                                                      RL1_0_1,
                                                      Fun1_0_1,
                                                      Go1_0_1,
                                                      C1_0_1,
                                                      [],
                                                      C1_1_1,
                                                      B1,
                                                      X1);
                                         [] ->
                                             Fun1_0_1(element(7,
                                                              Go1_0_1),
                                                      RL1_0_1,
                                                      Fun1_0_1,
                                                      Go1_0_1,
                                                      [],
                                                      [],
                                                      C1_1_1,
                                                      B1,
                                                      X1);
                                         E1_0_1 ->
                                             E1_0_1
                                     end;
                                 (7,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  C1_3_1,
                                  Y1,
                                  C1_1_1,
                                  B1,
                                  X1) ->
                                     if
                                         X1 == Y1 ->
                                             Fun1_0_1(element(11,
                                                              Go1_0_1),
                                                      RL1_0_1,
                                                      Fun1_0_1,
                                                      Go1_0_1,
                                                      C1_3_1,
                                                      Y1,
                                                      C1_1_1,
                                                      B1,
                                                      X1);
                                         true ->
                                             Fun1_0_1(element(10,
                                                              Go1_0_1),
                                                      RL1_0_1,
                                                      Fun1_0_1,
                                                      Go1_0_1,
                                                      C1_3_1,
                                                      Y1,
                                                      C1_1_1,
                                                      B1,
                                                      X1)
                                     end;
                                 (8,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  C1_3_1,
                                  Y1,
                                  _,
                                  B1,
                                  X1) ->
                                     Fun1_0_1(9,
                                              RL1_0_1,
                                              Fun1_0_1,
                                              Go1_0_1,
                                              C1_3_1,
                                              Y1,
                                              element(14, Go1_0_1),
                                              B1,
                                              X1);
                                 (9,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  C1_3_1,
                                  _,
                                  [[{B1,X1,_}|{Y1}]|C1_0_1],
                                  _,
                                  _) ->
                                     Fun1_0_1(element(13, Go1_0_1),
                                              RL1_0_1,
                                              Fun1_0_1,
                                              Go1_0_1,
                                              C1_3_1,
                                              Y1,
                                              C1_0_1,
                                              B1,
                                              X1);
                                 (9,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  C1_3_1,
                                  _,
                                  [_|C1_0_1],
                                  _,
                                  _) ->
                                     Fun1_0_1(9,
                                              RL1_0_1,
                                              Fun1_0_1,
                                              Go1_0_1,
                                              C1_3_1,
                                              [],
                                              C1_0_1,
                                              [],
                                              []);
                                 (9,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  C1_3_1,
                                  _,
                                  [],
                                  _,
                                  _) ->
                                     Fun1_0_1(element(12, Go1_0_1),
                                              RL1_0_1,
                                              Fun1_0_1,
                                              Go1_0_1,
                                              C1_3_1,
                                              [],
                                              [],
                                              [],
                                              []);
                                 (9,
                                  RL1_0_1,
                                  Fun1_0_1,
                                  Go1_0_1,
                                  C1_3_1,
                                  _,
                                  C1_1_1,
                                  _,
                                  _) ->
                                     case C1_1_1() of
                                         [[{B1,X1,_}|{Y1}]|C1_0_1] ->
                                             Fun1_0_1(element(13,
                                                              Go1_0_1),
                                                      RL1_0_1,
                                                      Fun1_0_1,
                                                      Go1_0_1,
                                                      C1_3_1,
                                                      Y1,
                                                      C1_0_1,
                                                      B1,
                                                      X1);
                                         [_|C1_0_1] ->
                                             Fun1_0_1(9,
                                                      RL1_0_1,
                                                      Fun1_0_1,
                                                      Go1_0_1,
                                                      C1_3_1,
                                                      [],
                                                      C1_0_1,
                                                      [],
                                                      []);
                                         [] ->
                                             Fun1_0_1(element(12,
                                                              Go1_0_1),
                                                      RL1_0_1,
                                                      Fun1_0_1,
                                                      Go1_0_1,
                                                      C1_3_1,
                                                      [],
                                                      [],
                                                      [],
                                                      []);
                                         E1_0_1 ->
                                             E1_0_1
                                     end
                              end,
                          Fun1_0_1(S01_0_1,
                                   RL01_0_1,
                                   Fun1_0_1,
                                   Go01_0_1,
                                   [],
                                   [],
                                   [],
                                   [],
                                   [])
                   end,
                   % fun-info: {3,41816426,'-create_handle/0-fun-3-'}
                   fun() ->
                          {<<$\203:8/integer-unit:1-unsigned-big,
                             $P:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $.:8/integer-unit:1-unsigned-big,
                             $x:8/integer-unit:1-unsigned-big,
                             $\234:8/integer-unit:1-unsigned-big,
                             $Ë:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $N:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $-:8/integer-unit:1-unsigned-big,
                             $):8/integer-unit:1-unsigned-big,
                             $-:8/integer-unit:1-unsigned-big,
                             $È:8/integer-unit:1-unsigned-big,
                             $I:8/integer-unit:1-unsigned-big,
                             $M:8/integer-unit:1-unsigned-big,
                             $ä:8/integer-unit:1-unsigned-big,
                             $Ê:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $Ê:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\n:8/integer-unit:1-unsigned-big,
                             $0:8/integer-unit:1-unsigned-big,
                             $\227:8/integer-unit:1-unsigned-big,
                             $%:8/integer-unit:1-unsigned-big,
                             $\026:8/integer-unit:1-unsigned-big,
                             $%:8/integer-unit:1-unsigned-big,
                             $r:8/integer-unit:1-unsigned-big,
                             $¥:8/integer-unit:1-unsigned-big,
                             $0:8/integer-unit:1-unsigned-big,
                             $0:8/integer-unit:1-unsigned-big,
                             $F:8/integer-unit:1-unsigned-big,
                             $ :8/integer-unit:1-unsigned-big,
                             $ð:8/integer-unit:1-unsigned-big,
                             $":8/integer-unit:1-unsigned-big,
                             $³:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $þ:8/integer-unit:1-unsigned-big,
                             $Ì:8/integer-unit:1-unsigned-big,
                             $\n:8/integer-unit:1-unsigned-big,
                             $É:8/integer-unit:1-unsigned-big>>,
                           <<$\203:8/integer-unit:1-unsigned-big,
                             $P:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $<:8/integer-unit:1-unsigned-big,
                             $x:8/integer-unit:1-unsigned-big,
                             $\234:8/integer-unit:1-unsigned-big,
                             $Ë:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $N:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $-:8/integer-unit:1-unsigned-big,
                             $):8/integer-unit:1-unsigned-big,
                             $-:8/integer-unit:1-unsigned-big,
                             $È:8/integer-unit:1-unsigned-big,
                             $I:8/integer-unit:1-unsigned-big,
                             $M:8/integer-unit:1-unsigned-big,
                             $ä:8/integer-unit:1-unsigned-big,
                             $Ê:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $Î:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\n:8/integer-unit:1-unsigned-big,
                             $0:8/integer-unit:1-unsigned-big,
                             $\227:8/integer-unit:1-unsigned-big,
                             $%:8/integer-unit:1-unsigned-big,
                             $\026:8/integer-unit:1-unsigned-big,
                             $%:8/integer-unit:1-unsigned-big,
                             $r:8/integer-unit:1-unsigned-big,
                             $¥:8/integer-unit:1-unsigned-big,
                             $0:8/integer-unit:1-unsigned-big,
                             $0:8/integer-unit:1-unsigned-big,
                             $::8/integer-unit:1-unsigned-big,
                             $¡:8/integer-unit:1-unsigned-big,
                             $ð:8/integer-unit:1-unsigned-big,
                             $":8/integer-unit:1-unsigned-big,
                             $P:8/integer-unit:1-unsigned-big,
                             $x:8/integer-unit:1-unsigned-big,
                             $ñ:8/integer-unit:1-unsigned-big,
                             $Y:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $ª:8/integer-unit:1-unsigned-big,
                             $9:8/integer-unit:1-unsigned-big,
                             $\r:8/integer-unit:1-unsigned-big,
                             $ý:8/integer-unit:1-unsigned-big>>,
                           <<$\203:8/integer-unit:1-unsigned-big,
                             $P:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $I:8/integer-unit:1-unsigned-big,
                             $x:8/integer-unit:1-unsigned-big,
                             $\234:8/integer-unit:1-unsigned-big,
                             $Ë:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $M:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $Ê:8/integer-unit:1-unsigned-big,
                             $/:8/integer-unit:1-unsigned-big,
                             $H:8/integer-unit:1-unsigned-big,
                             $ä:8/integer-unit:1-unsigned-big,
                             $N:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $¶:8/integer-unit:1-unsigned-big,
                             $µ:8/integer-unit:1-unsigned-big,
                             $²:8/integer-unit:1-unsigned-big,
                             $Í:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $\006:8/integer-unit:1-unsigned-big,
                             $Ò:8/integer-unit:1-unsigned-big,
                             $e:8/integer-unit:1-unsigned-big,
                             $\211:8/integer-unit:1-unsigned-big,
                             $E:8/integer-unit:1-unsigned-big,
                             $\s:8/integer-unit:1-unsigned-big,
                             $>:8/integer-unit:1-unsigned-big,
                             $£:8/integer-unit:1-unsigned-big,
                             $\023:8/integer-unit:1-unsigned-big,
                             $\210:8/integer-unit:1-unsigned-big,
                             $Ç:8/integer-unit:1-unsigned-big,
                             $\232:8/integer-unit:1-unsigned-big,
                             $\226:8/integer-unit:1-unsigned-big,
                             $\223:8/integer-unit:1-unsigned-big,
                             $\237:8/integer-unit:1-unsigned-big,
                             $X:8/integer-unit:1-unsigned-big,
                             $\222:8/integer-unit:1-unsigned-big,
                             $È:8/integer-unit:1-unsigned-big,
                             $\235:8/integer-unit:1-unsigned-big,
                             $l:8/integer-unit:1-unsigned-big,
                             $¨:8/integer-unit:1-unsigned-big,
                             $g:8/integer-unit:1-unsigned-big,
                             $i:8/integer-unit:1-unsigned-big,
                             $d:8/integer-unit:1-unsigned-big,
                             $\200:8/integer-unit:1-unsigned-big,
                             $\001:8/integer-unit:1-unsigned-big,
                             $R:8/integer-unit:1-unsigned-big,
                             $µ:8/integer-unit:1-unsigned-big,
                             $\r:8/integer-unit:1-unsigned-big,
                             $\214:8/integer-unit:1-unsigned-big,
                             $\030:8/integer-unit:1-unsigned-big,
                             $@:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $c:8/integer-unit:1-unsigned-big,
                             $Ö:8/integer-unit:1-unsigned-big,
                             $\017:8/integer-unit:1-unsigned-big,
                             $=:8/integer-unit:1-unsigned-big>>,
                           <<$\203:8/integer-unit:1-unsigned-big,
                             $h:8/integer-unit:1-unsigned-big,
                             $\003:8/integer-unit:1-unsigned-big,
                             $d:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\005:8/integer-unit:1-unsigned-big,
                             $t:8/integer-unit:1-unsigned-big,
                             $u:8/integer-unit:1-unsigned-big,
                             $p:8/integer-unit:1-unsigned-big,
                             $l:8/integer-unit:1-unsigned-big,
                             $e:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $\f:8/integer-unit:1-unsigned-big,
                             $l:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\001:8/integer-unit:1-unsigned-big,
                             $h:8/integer-unit:1-unsigned-big,
                             $\003:8/integer-unit:1-unsigned-big,
                             $d:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\003:8/integer-unit:1-unsigned-big,
                             $v:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $r:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $\f:8/integer-unit:1-unsigned-big,
                             $d:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\001:8/integer-unit:1-unsigned-big,
                             $Y:8/integer-unit:1-unsigned-big,
                             $j:8/integer-unit:1-unsigned-big>>,
                           <<$\203:8/integer-unit:1-unsigned-big,
                             $P:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $*:8/integer-unit:1-unsigned-big,
                             $x:8/integer-unit:1-unsigned-big,
                             $\234:8/integer-unit:1-unsigned-big,
                             $Ë:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $M:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $Ê:8/integer-unit:1-unsigned-big,
                             $/:8/integer-unit:1-unsigned-big,
                             $H:8/integer-unit:1-unsigned-big,
                             $ä:8/integer-unit:1-unsigned-big,
                             $\005:8/integer-unit:1-unsigned-big,
                             $R:8/integer-unit:1-unsigned-big,
                             $¶:8/integer-unit:1-unsigned-big,
                             $¶:8/integer-unit:1-unsigned-big,
                             $\031:8/integer-unit:1-unsigned-big,
                             $Ì:8/integer-unit:1-unsigned-big,
                             $):8/integer-unit:1-unsigned-big,
                             $\f:8/integer-unit:1-unsigned-big,
                             $Ì:8/integer-unit:1-unsigned-big,
                             $e:8/integer-unit:1-unsigned-big,
                             $\211:8/integer-unit:1-unsigned-big,
                             $E:8/integer-unit:1-unsigned-big,
                             $\s:8/integer-unit:1-unsigned-big,
                             $.:8/integer-unit:1-unsigned-big,
                             $c:8/integer-unit:1-unsigned-big,
                             $\004:8/integer-unit:1-unsigned-big,
                             $\n:8/integer-unit:1-unsigned-big,
                             $/:8/integer-unit:1-unsigned-big,
                             $\022:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $Ì:8/integer-unit:1-unsigned-big,
                             $\205:8/integer-unit:1-unsigned-big,
                             $\t:8/integer-unit:1-unsigned-big,
                             $\216:8/integer-unit:1-unsigned-big>>,
                           <<$\203:8/integer-unit:1-unsigned-big,
                             $P:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $j:8/integer-unit:1-unsigned-big,
                             $x:8/integer-unit:1-unsigned-big,
                             $\234:8/integer-unit:1-unsigned-big,
                             $Ë:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $I:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $I:8/integer-unit:1-unsigned-big,
                             $Î:8/integer-unit:1-unsigned-big,
                             $Ï:8/integer-unit:1-unsigned-big,
                             $+:8/integer-unit:1-unsigned-big,
                             $N:8/integer-unit:1-unsigned-big,
                             $ú:8/integer-unit:1-unsigned-big,
                             $ÿ:8/integer-unit:1-unsigned-big,
                             $ÿ:8/integer-unit:1-unsigned-big,
                             $ÿ:8/integer-unit:1-unsigned-big,
                             $·:8/integer-unit:1-unsigned-big,
                             $\f:8/integer-unit:1-unsigned-big,
                             $æ:8/integer-unit:1-unsigned-big,
                             $\024:8/integer-unit:1-unsigned-big,
                             $\006:8/integer-unit:1-unsigned-big,
                             $Ö:8/integer-unit:1-unsigned-big,
                             $\222:8/integer-unit:1-unsigned-big,
                             $Ò:8/integer-unit:1-unsigned-big,
                             $\202:8/integer-unit:1-unsigned-big,
                             $\234:8/integer-unit:1-unsigned-big,
                             $Ô:8/integer-unit:1-unsigned-big,
                             $D:8/integer-unit:1-unsigned-big,
                             $®:8/integer-unit:1-unsigned-big,
                             $\034:8/integer-unit:1-unsigned-big,
                             $\006:8/integer-unit:1-unsigned-big,
                             $\006:8/integer-unit:1-unsigned-big,
                             $\006:8/integer-unit:1-unsigned-big,
                             $f:8/integer-unit:1-unsigned-big,
                             $\220:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $s:8/integer-unit:1-unsigned-big,
                             $Y:8/integer-unit:1-unsigned-big,
                             $b:8/integer-unit:1-unsigned-big,
                             $Q:8/integer-unit:1-unsigned-big,
                             $":8/integer-unit:1-unsigned-big,
                             $W:8/integer-unit:1-unsigned-big,
                             $\n:8/integer-unit:1-unsigned-big,
                             $\003:8/integer-unit:1-unsigned-big,
                             $£:8/integer-unit:1-unsigned-big,
                             $\023:8/integer-unit:1-unsigned-big,
                             $\n:8/integer-unit:1-unsigned-big,
                             $/:8/integer-unit:1-unsigned-big,
                             $\002:8/integer-unit:1-unsigned-big,
                             $\205:8/integer-unit:1-unsigned-big,
                             $\027:8/integer-unit:1-unsigned-big,
                             $\237:8/integer-unit:1-unsigned-big,
                             $\205:8/integer-unit:1-unsigned-big,
                             $¤:8/integer-unit:1-unsigned-big,
                             $\227:8/integer-unit:1-unsigned-big,
                             $\007:8/integer-unit:1-unsigned-big,
                             $¤:8/integer-unit:1-unsigned-big,
                             $\227:8/integer-unit:1-unsigned-big,
                             $\021:8/integer-unit:1-unsigned-big,
                             $.:8/integer-unit:1-unsigned-big,
                             $Ï:8/integer-unit:1-unsigned-big,
                             $\003:8/integer-unit:1-unsigned-big,
                             $\224:8/integer-unit:1-unsigned-big,
                             $\217:8/integer-unit:1-unsigned-big,
                             $Ì:8/integer-unit:1-unsigned-big,
                             $\002:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\203:8/integer-unit:1-unsigned-big,
                             $>:8/integer-unit:1-unsigned-big,
                             $\034:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big>>}
                   end,
                   [{1,
                     2,
                     2,
                     {gen,
                      % fun-info: {4,131674517,'-create_handle/0-fun-4-'}
                      fun() ->
                             H1
                      end}},
                    {2,5,4,fil},
                    {3,
                     7,
                     5,
                     {gen,
                      % fun-info: {5,108000324,'-create_handle/0-fun-5-'}
                      fun() ->
                             [{0},{1},{2}]
                      end}},
                    {4,10,7,fil},
                    {5,
                     12,
                     8,
                     {gen,
                      {join,
                       '==',
                       1,
                       3,
                       % fun-info: {9,59718458,'-create_handle/0-fun-9-'}
                       fun(H1_0_1) ->
                              F1_0_1 =
                                  % fun-info: {7,779460,'-create_handle/0-fun-7-'}
                                  fun(_, []) ->
                                         [];
                                     (F1_0_1, [O1_0_1|C1_0_1]) ->
                                         case O1_0_1 of
                                             {_,_,_}
                                                 when
                                                     192.0
                                                     =:=
                                                     element(1, O1_0_1) ->
                                                 [O1_0_1|
                                                  % fun-info: {6,23729943,'-create_handle/0-fun-6-'}
                                                  fun() ->
                                                         F1_0_1(F1_0_1,
                                                                C1_0_1)
                                                  end];
                                             _ ->
                                                 F1_0_1(F1_0_1, C1_0_1)
                                         end;
                                     (F1_0_1, C1_0_1)
                                         when is_function(C1_0_1) ->
                                         F1_0_1(F1_0_1, C1_0_1());
                                     (_, C1_0_1) ->
                                         C1_0_1
                                  end,
                              % fun-info: {8,43652904,'-create_handle/0-fun-8-'}
                              fun() ->
                                     F1_0_1(F1_0_1, H1_0_1)
                              end
                       end,
                       % fun-info: {13,102310144,'-create_handle/0-fun-13-'}
                       fun(H1_0_1) ->
                              F1_0_1 =
                                  % fun-info: {11,74362432,'-create_handle/0-fun-11-'}
                                  fun(_, []) ->
                                         [];
                                     (F1_0_1, [O1_0_1|C1_0_1]) ->
                                         case O1_0_1 of
                                             {_} ->
                                                 [O1_0_1|
                                                  % fun-info: {10,23729943,'-create_handle/0-fun-10-'}
                                                  fun() ->
                                                         F1_0_1(F1_0_1,
                                                                C1_0_1)
                                                  end];
                                             _ ->
                                                 F1_0_1(F1_0_1, C1_0_1)
                                         end;
                                     (F1_0_1, C1_0_1)
                                         when is_function(C1_0_1) ->
                                         F1_0_1(F1_0_1, C1_0_1());
                                     (_, C1_0_1) ->
                                         C1_0_1
                                  end,
                              % fun-info: {12,43652904,'-create_handle/0-fun-12-'}
                              fun() ->
                                     F1_0_1(F1_0_1, H1_0_1)
                              end
                       end,
                       % fun-info: {14,17838355,'-create_handle/0-fun-14-'}
                       fun() ->
                              {[{1,[192.0]}],[],[]}
                       end}}}],
                   % fun-info: {22,31304647,'-create_handle/0-fun-22-'}
                   fun(join) ->
                          {[[{1,"\002"},{3,"\001"}]],[]};
                      (size) ->
                          % fun-info: {15,31963143,'-create_handle/0-fun-15-'}
                          fun(0) ->
                                 2;
                             (1) ->
                                 3;
                             (3) ->
                                 1;
                             (_) ->
                                 undefined
                          end;
                      (template) ->
                          % fun-info: {16,113413274,'-create_handle/0-fun-16-'}
                          fun({1,2}, '=:=') ->
                                 "\001";
                             ({1,2}, '==') ->
                                 "\001\002";
                             ({3,1}, '=:=') ->
                                 "\002";
                             ({3,1}, '==') ->
                                 "\001\002";
                             (_, _) ->
                                 []
                          end;
                      (constants) ->
                          % fun-info: {18,52148739,'-create_handle/0-fun-18-'}
                          fun(1) ->
                                 % fun-info: {17,5864387,'-create_handle/0-fun-17-'}
                                 fun(1) ->
                                        {values,[192.0],{some,[2]}};
                                    (_) ->
                                        false
                                 end;
                             (_) ->
                                 no_column_fun
                          end;
                      (n_leading_constant_columns) ->
                          % fun-info: {19,82183172,'-create_handle/0-fun-19-'}
                          fun(1) ->
                                 1;
                             (_) ->
                                 0
                          end;
                      (constant_columns) ->
                          % fun-info: {20,80910005,'-create_handle/0-fun-20-'}
                          fun(1) ->
                                 "\001";
                             (_) ->
                                 []
                          end;
                      (match_specs) ->
                          % fun-info: {21,91764346,'-create_handle/0-fun-21-'}
                          fun(1) ->
                                 {[{{'$1','$2','_'},
                                    [{'=:=','$1',192.0}],
                                    ['$_']}],
                                  "\002"};
                             (_) ->
                                 undefined
                          end;
                      (_) ->
                          undefined
                   end}
           end,
           undefined}).

lookup_handle() ->
    E = qlc_SUITE:table([{1,a},{2,b},{3,c}], 1, [1]),
    qlc:q({qlc_lc,
           % fun-info: {46,120768015,'-lookup_handle/0-fun-22-'}
           fun() ->
                  {qlc_v1,
                   % fun-info: {26,82970908,'-lookup_handle/0-fun-2-'}
                   fun(S02_0_1, RL02_0_1, Go02_0_1) ->
                          Fun2_0_1 =
                              % fun-info: {25,75235357,'-lookup_handle/0-fun-1-'}
                              fun(0, RL2_0_1, _, _, _, _, _, _)
                                     when is_list(RL2_0_1) ->
                                     lists:reverse(RL2_0_1);
                                 (0, _, _, _, _, _, _, _) ->
                                     [];
                                 (1,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  C2_2_1,
                                  Y2,
                                  C2_1_1,
                                  X2)
                                     when is_list(RL2_0_1) ->
                                     Fun2_0_1(element(1, Go2_0_1),
                                              [{X2,Y2}|RL2_0_1],
                                              Fun2_0_1,
                                              Go2_0_1,
                                              C2_2_1,
                                              Y2,
                                              C2_1_1,
                                              X2);
                                 (1,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  C2_2_1,
                                  Y2,
                                  C2_1_1,
                                  X2) ->
                                     [{X2,Y2}|
                                      % fun-info: {24,124255471,'-lookup_handle/0-fun-0-'}
                                      fun() ->
                                             Fun2_0_1(element(1,
                                                              Go2_0_1),
                                                      RL2_0_1,
                                                      Fun2_0_1,
                                                      Go2_0_1,
                                                      C2_2_1,
                                                      Y2,
                                                      C2_1_1,
                                                      X2)
                                      end];
                                 (2,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  C2_2_1,
                                  Y2,
                                  _,
                                  X2) ->
                                     Fun2_0_1(3,
                                              RL2_0_1,
                                              Fun2_0_1,
                                              Go2_0_1,
                                              C2_2_1,
                                              Y2,
                                              element(4, Go2_0_1),
                                              X2);
                                 (3,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  C2_2_1,
                                  Y2,
                                  [{X2,_}|C2_0_1],
                                  _) ->
                                     Fun2_0_1(element(3, Go2_0_1),
                                              RL2_0_1,
                                              Fun2_0_1,
                                              Go2_0_1,
                                              C2_2_1,
                                              Y2,
                                              C2_0_1,
                                              X2);
                                 (3,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  C2_2_1,
                                  Y2,
                                  [_|C2_0_1],
                                  _) ->
                                     Fun2_0_1(3,
                                              RL2_0_1,
                                              Fun2_0_1,
                                              Go2_0_1,
                                              C2_2_1,
                                              Y2,
                                              C2_0_1,
                                              []);
                                 (3,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  C2_2_1,
                                  Y2,
                                  [],
                                  _) ->
                                     Fun2_0_1(element(2, Go2_0_1),
                                              RL2_0_1,
                                              Fun2_0_1,
                                              Go2_0_1,
                                              C2_2_1,
                                              Y2,
                                              [],
                                              []);
                                 (3,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  C2_2_1,
                                  Y2,
                                  C2_1_1,
                                  _) ->
                                     case C2_1_1() of
                                         [{X2,_}|C2_0_1] ->
                                             Fun2_0_1(element(3,
                                                              Go2_0_1),
                                                      RL2_0_1,
                                                      Fun2_0_1,
                                                      Go2_0_1,
                                                      C2_2_1,
                                                      Y2,
                                                      C2_0_1,
                                                      X2);
                                         [_|C2_0_1] ->
                                             Fun2_0_1(3,
                                                      RL2_0_1,
                                                      Fun2_0_1,
                                                      Go2_0_1,
                                                      C2_2_1,
                                                      Y2,
                                                      C2_0_1,
                                                      []);
                                         [] ->
                                             Fun2_0_1(element(2,
                                                              Go2_0_1),
                                                      RL2_0_1,
                                                      Fun2_0_1,
                                                      Go2_0_1,
                                                      C2_2_1,
                                                      Y2,
                                                      [],
                                                      []);
                                         E2_0_1 ->
                                             E2_0_1
                                     end;
                                 (4,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  _,
                                  Y2,
                                  C2_1_1,
                                  X2) ->
                                     Fun2_0_1(5,
                                              RL2_0_1,
                                              Fun2_0_1,
                                              Go2_0_1,
                                              element(7, Go2_0_1),
                                              Y2,
                                              C2_1_1,
                                              X2);
                                 (5,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  [{Y2}|C2_0_1],
                                  _,
                                  C2_1_1,
                                  X2) ->
                                     Fun2_0_1(element(6, Go2_0_1),
                                              RL2_0_1,
                                              Fun2_0_1,
                                              Go2_0_1,
                                              C2_0_1,
                                              Y2,
                                              C2_1_1,
                                              X2);
                                 (5,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  [_|C2_0_1],
                                  _,
                                  C2_1_1,
                                  X2) ->
                                     Fun2_0_1(5,
                                              RL2_0_1,
                                              Fun2_0_1,
                                              Go2_0_1,
                                              C2_0_1,
                                              [],
                                              C2_1_1,
                                              X2);
                                 (5,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  [],
                                  _,
                                  C2_1_1,
                                  X2) ->
                                     Fun2_0_1(element(5, Go2_0_1),
                                              RL2_0_1,
                                              Fun2_0_1,
                                              Go2_0_1,
                                              [],
                                              [],
                                              C2_1_1,
                                              X2);
                                 (5,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  C2_2_1,
                                  _,
                                  C2_1_1,
                                  X2) ->
                                     case C2_2_1() of
                                         [{Y2}|C2_0_1] ->
                                             Fun2_0_1(element(6,
                                                              Go2_0_1),
                                                      RL2_0_1,
                                                      Fun2_0_1,
                                                      Go2_0_1,
                                                      C2_0_1,
                                                      Y2,
                                                      C2_1_1,
                                                      X2);
                                         [_|C2_0_1] ->
                                             Fun2_0_1(5,
                                                      RL2_0_1,
                                                      Fun2_0_1,
                                                      Go2_0_1,
                                                      C2_0_1,
                                                      [],
                                                      C2_1_1,
                                                      X2);
                                         [] ->
                                             Fun2_0_1(element(5,
                                                              Go2_0_1),
                                                      RL2_0_1,
                                                      Fun2_0_1,
                                                      Go2_0_1,
                                                      [],
                                                      [],
                                                      C2_1_1,
                                                      X2);
                                         E2_0_1 ->
                                             E2_0_1
                                     end;
                                 (6,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  C2_2_1,
                                  Y2,
                                  C2_1_1,
                                  X2) ->
                                     if
                                         X2 =:= Y2 ->
                                             Fun2_0_1(element(9,
                                                              Go2_0_1),
                                                      RL2_0_1,
                                                      Fun2_0_1,
                                                      Go2_0_1,
                                                      C2_2_1,
                                                      Y2,
                                                      C2_1_1,
                                                      X2);
                                         true ->
                                             Fun2_0_1(element(8,
                                                              Go2_0_1),
                                                      RL2_0_1,
                                                      Fun2_0_1,
                                                      Go2_0_1,
                                                      C2_2_1,
                                                      Y2,
                                                      C2_1_1,
                                                      X2)
                                     end;
                                 (7,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  C2_2_1,
                                  Y2,
                                  _,
                                  X2) ->
                                     Fun2_0_1(8,
                                              RL2_0_1,
                                              Fun2_0_1,
                                              Go2_0_1,
                                              C2_2_1,
                                              Y2,
                                              element(12, Go2_0_1),
                                              X2);
                                 (8,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  C2_2_1,
                                  _,
                                  [[{X2,_}|{Y2}]|C2_0_1],
                                  _) ->
                                     Fun2_0_1(element(11, Go2_0_1),
                                              RL2_0_1,
                                              Fun2_0_1,
                                              Go2_0_1,
                                              C2_2_1,
                                              Y2,
                                              C2_0_1,
                                              X2);
                                 (8,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  C2_2_1,
                                  _,
                                  [_|C2_0_1],
                                  _) ->
                                     Fun2_0_1(8,
                                              RL2_0_1,
                                              Fun2_0_1,
                                              Go2_0_1,
                                              C2_2_1,
                                              [],
                                              C2_0_1,
                                              []);
                                 (8,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  C2_2_1,
                                  _,
                                  [],
                                  _) ->
                                     Fun2_0_1(element(10, Go2_0_1),
                                              RL2_0_1,
                                              Fun2_0_1,
                                              Go2_0_1,
                                              C2_2_1,
                                              [],
                                              [],
                                              []);
                                 (8,
                                  RL2_0_1,
                                  Fun2_0_1,
                                  Go2_0_1,
                                  C2_2_1,
                                  _,
                                  C2_1_1,
                                  _) ->
                                     case C2_1_1() of
                                         [[{X2,_}|{Y2}]|C2_0_1] ->
                                             Fun2_0_1(element(11,
                                                              Go2_0_1),
                                                      RL2_0_1,
                                                      Fun2_0_1,
                                                      Go2_0_1,
                                                      C2_2_1,
                                                      Y2,
                                                      C2_0_1,
                                                      X2);
                                         [_|C2_0_1] ->
                                             Fun2_0_1(8,
                                                      RL2_0_1,
                                                      Fun2_0_1,
                                                      Go2_0_1,
                                                      C2_2_1,
                                                      [],
                                                      C2_0_1,
                                                      []);
                                         [] ->
                                             Fun2_0_1(element(10,
                                                              Go2_0_1),
                                                      RL2_0_1,
                                                      Fun2_0_1,
                                                      Go2_0_1,
                                                      C2_2_1,
                                                      [],
                                                      [],
                                                      []);
                                         E2_0_1 ->
                                             E2_0_1
                                     end
                              end,
                          Fun2_0_1(S02_0_1,
                                   RL02_0_1,
                                   Fun2_0_1,
                                   Go02_0_1,
                                   [],
                                   [],
                                   [],
                                   [])
                   end,
                   % fun-info: {27,111349661,'-lookup_handle/0-fun-3-'}
                   fun() ->
                          {<<$\203:8/integer-unit:1-unsigned-big,
                             $P:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $.:8/integer-unit:1-unsigned-big,
                             $x:8/integer-unit:1-unsigned-big,
                             $\234:8/integer-unit:1-unsigned-big,
                             $Ë:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $N:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $-:8/integer-unit:1-unsigned-big,
                             $):8/integer-unit:1-unsigned-big,
                             $-:8/integer-unit:1-unsigned-big,
                             $È:8/integer-unit:1-unsigned-big,
                             $I:8/integer-unit:1-unsigned-big,
                             $M:8/integer-unit:1-unsigned-big,
                             $\024:8/integer-unit:1-unsigned-big,
                             $Ì:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $Ê:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\n:8/integer-unit:1-unsigned-big,
                             $0:8/integer-unit:1-unsigned-big,
                             $\227:8/integer-unit:1-unsigned-big,
                             $%:8/integer-unit:1-unsigned-big,
                             $\026:8/integer-unit:1-unsigned-big,
                             $%:8/integer-unit:1-unsigned-big,
                             $\n:8/integer-unit:1-unsigned-big,
                             $¦:8/integer-unit:1-unsigned-big,
                             $0:8/integer-unit:1-unsigned-big,
                             $0:8/integer-unit:1-unsigned-big,
                             $F:8/integer-unit:1-unsigned-big,
                             $ :8/integer-unit:1-unsigned-big,
                             $ð:8/integer-unit:1-unsigned-big,
                             $":8/integer-unit:1-unsigned-big,
                             $³:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\206:8/integer-unit:1-unsigned-big,
                             $\n:8/integer-unit:1-unsigned-big,
                             $Þ:8/integer-unit:1-unsigned-big>>,
                           <<$\203:8/integer-unit:1-unsigned-big,
                             $P:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $.:8/integer-unit:1-unsigned-big,
                             $x:8/integer-unit:1-unsigned-big,
                             $\234:8/integer-unit:1-unsigned-big,
                             $Ë:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $N:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $-:8/integer-unit:1-unsigned-big,
                             $):8/integer-unit:1-unsigned-big,
                             $-:8/integer-unit:1-unsigned-big,
                             $È:8/integer-unit:1-unsigned-big,
                             $I:8/integer-unit:1-unsigned-big,
                             $M:8/integer-unit:1-unsigned-big,
                             $\024:8/integer-unit:1-unsigned-big,
                             $Ì:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $Ê:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\n:8/integer-unit:1-unsigned-big,
                             $0:8/integer-unit:1-unsigned-big,
                             $\227:8/integer-unit:1-unsigned-big,
                             $%:8/integer-unit:1-unsigned-big,
                             $\026:8/integer-unit:1-unsigned-big,
                             $%:8/integer-unit:1-unsigned-big,
                             $\n:8/integer-unit:1-unsigned-big,
                             $¦:8/integer-unit:1-unsigned-big,
                             $0:8/integer-unit:1-unsigned-big,
                             $0:8/integer-unit:1-unsigned-big,
                             $F:8/integer-unit:1-unsigned-big,
                             $ :8/integer-unit:1-unsigned-big,
                             $ð:8/integer-unit:1-unsigned-big,
                             $â:8/integer-unit:1-unsigned-big,
                             $³:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\222:8/integer-unit:1-unsigned-big,
                             $\n:8/integer-unit:1-unsigned-big,
                             $ä:8/integer-unit:1-unsigned-big>>,
                           <<$\203:8/integer-unit:1-unsigned-big,
                             $h:8/integer-unit:1-unsigned-big,
                             $\003:8/integer-unit:1-unsigned-big,
                             $d:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\005:8/integer-unit:1-unsigned-big,
                             $t:8/integer-unit:1-unsigned-big,
                             $u:8/integer-unit:1-unsigned-big,
                             $p:8/integer-unit:1-unsigned-big,
                             $l:8/integer-unit:1-unsigned-big,
                             $e:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $\022:8/integer-unit:1-unsigned-big,
                             $l:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\001:8/integer-unit:1-unsigned-big,
                             $h:8/integer-unit:1-unsigned-big,
                             $\003:8/integer-unit:1-unsigned-big,
                             $d:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\003:8/integer-unit:1-unsigned-big,
                             $v:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $r:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $\022:8/integer-unit:1-unsigned-big,
                             $d:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\001:8/integer-unit:1-unsigned-big,
                             $Y:8/integer-unit:1-unsigned-big,
                             $j:8/integer-unit:1-unsigned-big>>,
                           <<$\203:8/integer-unit:1-unsigned-big,
                             $P:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $+:8/integer-unit:1-unsigned-big,
                             $x:8/integer-unit:1-unsigned-big,
                             $\234:8/integer-unit:1-unsigned-big,
                             $Ë:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $M:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $Ê:8/integer-unit:1-unsigned-big,
                             $/:8/integer-unit:1-unsigned-big,
                             $H:8/integer-unit:1-unsigned-big,
                             $\024:8/integer-unit:1-unsigned-big,
                             $N:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $¶:8/integer-unit:1-unsigned-big,
                             $µ:8/integer-unit:1-unsigned-big,
                             $²:8/integer-unit:1-unsigned-big,
                             $Í:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $\006:8/integer-unit:1-unsigned-big,
                             $Ò:8/integer-unit:1-unsigned-big,
                             $e:8/integer-unit:1-unsigned-big,
                             $\211:8/integer-unit:1-unsigned-big,
                             $E:8/integer-unit:1-unsigned-big,
                             $\s:8/integer-unit:1-unsigned-big,
                             $>:8/integer-unit:1-unsigned-big,
                             $c:8/integer-unit:1-unsigned-big,
                             $\004:8/integer-unit:1-unsigned-big,
                             $\n:8/integer-unit:1-unsigned-big,
                             $/:8/integer-unit:1-unsigned-big,
                             $\022:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $×:8/integer-unit:1-unsigned-big,
                             $\227:8/integer-unit:1-unsigned-big,
                             $\t:8/integer-unit:1-unsigned-big,
                             $Û:8/integer-unit:1-unsigned-big>>,
                           <<$\203:8/integer-unit:1-unsigned-big,
                             $P:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\\:8/integer-unit:1-unsigned-big,
                             $x:8/integer-unit:1-unsigned-big,
                             $\234:8/integer-unit:1-unsigned-big,
                             $Ë:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $I:8/integer-unit:1-unsigned-big,
                             $a:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $I:8/integer-unit:1-unsigned-big,
                             $Î:8/integer-unit:1-unsigned-big,
                             $Ï:8/integer-unit:1-unsigned-big,
                             $+:8/integer-unit:1-unsigned-big,
                             $N:8/integer-unit:1-unsigned-big,
                             $ú:8/integer-unit:1-unsigned-big,
                             $ÿ:8/integer-unit:1-unsigned-big,
                             $ÿ:8/integer-unit:1-unsigned-big,
                             $ÿ:8/integer-unit:1-unsigned-big,
                             $û:8/integer-unit:1-unsigned-big,
                             $\f:8/integer-unit:1-unsigned-big,
                             $æ:8/integer-unit:1-unsigned-big,
                             $\024:8/integer-unit:1-unsigned-big,
                             $\006:8/integer-unit:1-unsigned-big,
                             $Ö:8/integer-unit:1-unsigned-big,
                             $\222:8/integer-unit:1-unsigned-big,
                             $Ò:8/integer-unit:1-unsigned-big,
                             $\202:8/integer-unit:1-unsigned-big,
                             $\234:8/integer-unit:1-unsigned-big,
                             $Ô:8/integer-unit:1-unsigned-big,
                             $D:8/integer-unit:1-unsigned-big,
                             $Á:8/integer-unit:1-unsigned-big,
                             $\034:8/integer-unit:1-unsigned-big,
                             $\006:8/integer-unit:1-unsigned-big,
                             $\006:8/integer-unit:1-unsigned-big,
                             $\006:8/integer-unit:1-unsigned-big,
                             $&:8/integer-unit:1-unsigned-big,
                             $\220:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $s:8/integer-unit:1-unsigned-big,
                             $Y:8/integer-unit:1-unsigned-big,
                             $b:8/integer-unit:1-unsigned-big,
                             $Q:8/integer-unit:1-unsigned-big,
                             $¢:8/integer-unit:1-unsigned-big,
                             $`:8/integer-unit:1-unsigned-big,
                             $\n:8/integer-unit:1-unsigned-big,
                             $\003:8/integer-unit:1-unsigned-big,
                             $c:8/integer-unit:1-unsigned-big,
                             $\004:8/integer-unit:1-unsigned-big,
                             $\n:8/integer-unit:1-unsigned-big,
                             $/:8/integer-unit:1-unsigned-big,
                             $>:8/integer-unit:1-unsigned-big,
                             $\v:8/integer-unit:1-unsigned-big,
                             $I:8/integer-unit:1-unsigned-big,
                             $µ:8/integer-unit:1-unsigned-big,
                             $\020:8/integer-unit:1-unsigned-big,
                             $H:8/integer-unit:1-unsigned-big,
                             $5:8/integer-unit:1-unsigned-big,
                             $#:8/integer-unit:1-unsigned-big,
                             $\\:8/integer-unit:1-unsigned-big,
                             $^:8/integer-unit:1-unsigned-big,
                             $\b:8/integer-unit:1-unsigned-big,
                             $(:8/integer-unit:1-unsigned-big,
                             $\037:8/integer-unit:1-unsigned-big,
                             $\231:8/integer-unit:1-unsigned-big,
                             $\005:8/integer-unit:1-unsigned-big,
                             $\000:8/integer-unit:1-unsigned-big,
                             $\024:8/integer-unit:1-unsigned-big,
                             $Ù:8/integer-unit:1-unsigned-big,
                             $\031:8/integer-unit:1-unsigned-big,
                             $M:8/integer-unit:1-unsigned-big>>}
                   end,
                   [{1,
                     2,
                     2,
                     {gen,
                      % fun-info: {28,75197307,'-lookup_handle/0-fun-4-'}
                      fun() ->
                             E
                      end}},
                    {2,
                     5,
                     4,
                     {gen,
                      % fun-info: {29,86826511,'-lookup_handle/0-fun-5-'}
                      fun() ->
                             [{0},{1},{2}]
                      end}},
                    {3,8,6,fil},
                    {4,
                     10,
                     7,
                     {gen,
                      {join,
                       '==',
                       1,
                       2,
                       % fun-info: {33,129609919,'-lookup_handle/0-fun-9-'}
                       fun(H2_0_1) ->
                              F2_0_1 =
                                  % fun-info: {31,45768082,'-lookup_handle/0-fun-7-'}
                                  fun(_, []) ->
                                         [];
                                     (F2_0_1, [O2_0_1|C2_0_1]) ->
                                         case O2_0_1 of
                                             {_,_} ->
                                                 [O2_0_1|
                                                  % fun-info: {30,28136696,'-lookup_handle/0-fun-6-'}
                                                  fun() ->
                                                         F2_0_1(F2_0_1,
                                                                C2_0_1)
                                                  end];
                                             _ ->
                                                 F2_0_1(F2_0_1, C2_0_1)
                                         end;
                                     (F2_0_1, C2_0_1)
                                         when is_function(C2_0_1) ->
                                         F2_0_1(F2_0_1, C2_0_1());
                                     (_, C2_0_1) ->
                                         C2_0_1
                                  end,
                              % fun-info: {32,48059625,'-lookup_handle/0-fun-8-'}
                              fun() ->
                                     F2_0_1(F2_0_1, H2_0_1)
                              end
                       end,
                       % fun-info: {37,63676968,'-lookup_handle/0-fun-13-'}
                       fun(H2_0_1) ->
                              F2_0_1 =
                                  % fun-info: {35,129320532,'-lookup_handle/0-fun-11-'}
                                  fun(_, []) ->
                                         [];
                                     (F2_0_1, [O2_0_1|C2_0_1]) ->
                                         case O2_0_1 of
                                             {_} ->
                                                 [O2_0_1|
                                                  % fun-info: {34,28136696,'-lookup_handle/0-fun-10-'}
                                                  fun() ->
                                                         F2_0_1(F2_0_1,
                                                                C2_0_1)
                                                  end];
                                             _ ->
                                                 F2_0_1(F2_0_1, C2_0_1)
                                         end;
                                     (F2_0_1, C2_0_1)
                                         when is_function(C2_0_1) ->
                                         F2_0_1(F2_0_1, C2_0_1());
                                     (_, C2_0_1) ->
                                         C2_0_1
                                  end,
                              % fun-info: {36,48059625,'-lookup_handle/0-fun-12-'}
                              fun() ->
                                     F2_0_1(F2_0_1, H2_0_1)
                              end
                       end,
                       % fun-info: {38,3236543,'-lookup_handle/0-fun-14-'}
                       fun() ->
                              {[],[],[]}
                       end}}}],
                   % fun-info: {45,56361026,'-lookup_handle/0-fun-21-'}
                   fun(join) ->
                          [[{1,"\001"},{2,"\001"}]];
                      (size) ->
                          % fun-info: {39,40607542,'-lookup_handle/0-fun-15-'}
                          fun(0) ->
                                 2;
                             (1) ->
                                 2;
                             (2) ->
                                 1;
                             (_) ->
                                 undefined
                          end;
                      (template) ->
                          % fun-info: {40,34907048,'-lookup_handle/0-fun-16-'}
                          fun({1,1}, _) ->
                                 "\001\002";
                             ({2,1}, _) ->
                                 "\001\002";
                             (_, _) ->
                                 []
                          end;
                      (constants) ->
                          % fun-info: {41,11686091,'-lookup_handle/0-fun-17-'}
                          fun(_) ->
                                 no_column_fun
                          end;
                      (n_leading_constant_columns) ->
                          % fun-info: {42,21492441,'-lookup_handle/0-fun-18-'}
                          fun(_) ->
                                 0
                          end;
                      (constant_columns) ->
                          % fun-info: {43,55297177,'-lookup_handle/0-fun-19-'}
                          fun(_) ->
                                 []
                          end;
                      (match_specs) ->
                          % fun-info: {44,55081557,'-lookup_handle/0-fun-20-'}
                          fun(_) ->
                                 undefined
                          end;
                      (_) ->
                          undefined
                   end}
           end,
           undefined}).
