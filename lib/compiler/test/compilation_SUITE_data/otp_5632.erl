%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
-module(otp_5632).
-export([?MODULE/0]).
-export([pstnproxy_add_headers/2,macosx_workaround/0,fast_cut/3,test/1,test2/1,
	 create_int_jumper_cables/1]).

-import(lists, [foldl/3,last/1,member/2,reverse/1,reverse/2,seq/2,sort/1]).

?MODULE() ->
    ok.

-record(request, {
	  header
	 }).


-record(siporigin, {
	  proto
	 }).


%% Test a problem in beam_jump.erl.
pstnproxy_add_headers(Request, Origin) when is_record(Request, request),
                                            is_record(Origin, siporigin) ->
    NewHeaders1 = Request#request.header,
    NewHeaders2 =
        case (Origin#siporigin.proto == tls) or (Origin#siporigin.proto == tls6) of
            true ->
		keylist:delete("X-Foo2", NewHeaders1);
	    false ->
                keylist:delete("X-Foo2", NewHeaders1)
        end,
    NewHeaders3 =
      case (Origin#siporigin.proto == tls) or (Origin#siporigin.proto == tls6) of
          true ->
              keylist:delete("X-Foo3", NewHeaders2);
          false ->
              keylist:delete("X-Foo3", NewHeaders2)
      end,
    Request#request{header = NewHeaders3}.

%% Test a problem in beam_validator.erl.
macosx_workaround() ->
    try 1.0/zero()
    catch
	error:_ -> ok
    end.

zero() -> 0.0.

-record(we, {id,
             perm = 0,
             name,
             es,
             fs,
             he,
             vc,
             vp,
             mat = default,
             next_id,
             mode,
             mirror = none,
             light = none,
             has_shape = true}).

-record(edge, {vs,
               ve,
               a = none,
               b = none,
               lf,
               rf,
               ltpr,
               ltsu,
               rtpr,
               rtsu}).


fast_cut(Edge,Pos0,We0) ->
    {NewEdge = NewV,We} = wings_we:new_ids(1,We0),
    #we{es = Etab0,
        vc = Vct0,
        vp = Vtab0,
        he = Htab0} = We,
    Template = gb_trees:get(Edge,Etab0),
    #edge{vs = Vstart,
          ve = Vend,
          a = ACol,
          b = BCol,
          lf = Lf,
          rf = Rf,
          ltpr = EdgeA,
          rtsu = EdgeB,
          rtpr = NextBCol} = Template,
    VendPos = gb_trees:get(Vend,Vtab0),
    Vct1 = gb_trees:update(Vend,NewEdge,Vct0),
    VstartPos = wings_vertex:pos(Vstart,Vtab0),
    if
        Pos0 =:= default ->
            NewVPos0 = e3d_vec:average([VstartPos,VendPos]);
        true ->
            NewVPos0 = Pos0
    end,
    NewVPos = wings_util:share(NewVPos0),
    Vct = gb_trees:insert(NewV,NewEdge,Vct1),
    Vtab = gb_trees:insert(NewV,NewVPos,Vtab0),
    AColOther = ?MODULE:get_vtx_color(EdgeA,Lf,Etab0),
    BColOther = ?MODULE:get_vtx_color(NextBCol,Rf,Etab0),
    Weight = if
                 Pos0 == default ->
                     0.500000;
                 true ->
                     ADist = e3d_vec:dist(Pos0,VstartPos),
                     BDist = e3d_vec:dist(Pos0,VendPos),
                     try
                         ADist / (ADist + BDist)
                     catch
                         error:badarith ->
                             0.500000
                     end
             end,
    NewColA = wings_color:mix(Weight,AColOther,ACol),
    NewColB = wings_color:mix(Weight,BCol,BColOther),
    NewEdgeRec = Template#edge{vs = NewV,
                               a = NewColA,
                               ltsu = Edge,
                               rtpr = Edge},
    Etab1 = gb_trees:insert(NewEdge,NewEdgeRec,Etab0),
    Etab2 = ?MODULE:patch_edge(EdgeA,NewEdge,Edge,Etab1),
    Etab3 = ?MODULE:patch_edge(EdgeB,NewEdge,Edge,Etab2),
    EdgeRec = Template#edge{ve = NewV,
                            b = NewColB,
                            rtsu = NewEdge,
                            ltpr = NewEdge},
    Etab = gb_trees:update(Edge,EdgeRec,Etab3),
    Htab = case gb_sets:is_member(Edge,Htab0) of
               false ->
                   Htab0;
               true ->
                   gb_sets:insert(NewEdge,Htab0)
           end,
    {We#we{es = Etab,
           vc = Vct,
           vp = Vtab,
           he = Htab},NewV}.

%% A problem in beam_bool.

-record(a, {a,b,c}).

test(As) ->
     case As of
     A when A#a.b == []; A#a.b == undefined ->
         true;
     _ ->
         false
     end.

test2(As) ->
     case As of
     A when A#a.b == {a,b,c}; A#a.b == undefined ->
         true;
     _ ->
         false
     end.

%% Record updating problems.

-record(int_jumper_cable, {id,
                           connectionFieldRef,
                           aiuPlugInUnitRef,
                           connFieldConnector,
                           aiuConnector,
                           dlAttenuation = 1,
                           ulAttenuation = 1,
                           electricalDlDelay = 100,
                           electricalUlDelay = 100,
                           optionals = []}).

create_int_jumper_cables(_Config) ->
    ct_line:line({{ccl_setup_SUITE,create_int_jumper_cables},637}),
    ct:comment("Create IntJumperCable MO"),
    ct_line:line({{ccl_setup_SUITE,create_int_jumper_cables},639}),
    Parent = "ManagedElement=1,Equipment=1",
    ct_line:line({{ccl_setup_SUITE,create_int_jumper_cables},641}),
    I1 = #int_jumper_cable{id = 1,
                           connectionFieldRef = Parent ++ ",ConnectionField=1",
                           aiuPlugInUnitRef = Parent ++ ",Subrack=2,Slot=6,PlugInUnit=1",
                           connFieldConnector = "J1",
                           aiuConnector = 1},
    ct_line:line({{ccl_setup_SUITE,create_int_jumper_cables},650}),
    I2 = I1#int_jumper_cable{id = 2,
                             connFieldConnector = "H1",
                             aiuConnector = 2},
    ct_line:line({{ccl_setup_SUITE,create_int_jumper_cables},657}),
    I3 = I1#int_jumper_cable{id = 3,
                             aiuPlugInUnitRef = Parent ++ ",Subrack=2,Slot=9,PlugInUnit=1",
                             connFieldConnector = "J2"},
    ct_line:line({{ccl_setup_SUITE,create_int_jumper_cables},663}),
    I4 = I3#int_jumper_cable{id = 4,
                             connFieldConnector = "H2",
                             aiuConnector = 2},
    ct_line:line({{ccl_setup_SUITE,create_int_jumper_cables},670}),
    I5 = I1#int_jumper_cable{id = 5,
                             aiuPlugInUnitRef = Parent ++ ",Subrack=2,Slot=12,PlugInUnit=1",
                             connFieldConnector = "J3"},
    ct_line:line({{ccl_setup_SUITE,create_int_jumper_cables},677}),
    I6 = I5#int_jumper_cable{id = 6,
                             connFieldConnector = "H3",
                             aiuConnector = 2},
    ct_line:line({{ccl_setup_SUITE,create_int_jumper_cables},684}),
    [_MO1,_MO2,_MO3,_MO4,_MO5,_MO6] = mub_util:create(mp_mub,Parent,[I1,I2,I3,I4,I5,I6]),
    ct_line:line({{ccl_setup_SUITE,create_int_jumper_cables},686}),
    ok.
