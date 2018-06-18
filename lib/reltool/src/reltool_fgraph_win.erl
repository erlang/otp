%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2018. All Rights Reserved.
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

-module(reltool_fgraph_win).

-export([
         new/2,
         add_node/2,
         add_node/3,
         del_node/2,
         change_node/3,
         add_link/2,
         del_link/2,
         set_dbl_click/2,
         stop/2
        ]).

-include_lib("wx/include/wx.hrl").
-include("reltool_fgraph.hrl").

-record(state,
        {
          parent_pid,
          frame,
          window,
          width,
          height,
          q_slider,
          l_slider,
          k_slider,
          mouse_act,
          is_frozen,
          ticker
         }).

-record(graph,
        {
          pen,
          brush,
          font,
          select = none,
          offset = {0,0},
          offset_state = false,
          ke = 0,
          vs = [],
          es = []
         }).

-define(BRD,10).
-define(ARC_R, 10).

-define(reset, 80).
-define(lock, 81).
-define(unlock, 82).
-define(move, 83).
-define(select, 84).
-define(delete, 85).
-define(freeze, 86).

-define(q_slider, 90).
-define(l_slider, 91).
-define(k_slider, 92).

-define(default_q, 20).
-define(default_l, 20).
-define(default_k, 20).

-define(color_bg, {45,50,95}).
-define(color_fg, {235,245,230}).
-define(color_default, {10,220,20}).
-define(color_default_bg, {20,230,30}).
-define(color_alternate, {220,10,20}).
-define(color_alternate_bg, {230,20,30}).

add_node(Pid, Key) -> add_node(Pid, Key, default).
add_node(Pid, Key, Color) -> Pid ! {add_node, Key, Color}.
del_node(Pid, Key) -> Pid ! {del_node, Key}.
change_node(Pid, Key, Color) ->  Pid ! {change_node, Key, Color}.

add_link(Pid, {FromKey, ToKey}) -> Pid ! {add_link, {FromKey, ToKey}}.
del_link(Pid, {FromKey, ToKey}) -> Pid ! {del_link, {FromKey, ToKey}}.

stop(Pid, Reason) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {stop, Reason},
    receive
        {'DOWN', Ref, _, _, _} ->
            ok
    end.

set_dbl_click(Pid, Fun) -> Pid ! {set_dbl_click, Fun}.

new(Parent, Options) ->
    Env = wx:get_env(),
    Me  = self(),
    Pid = spawn_link(fun() -> init([Parent, Me, Env, Options]) end),
    receive {Pid, {?MODULE, Panel}} -> {Pid,Panel} end.

init([ParentWin, Pid, Env, Options]) ->
    wx:set_env(Env),

    BReset  = wxButton:new(ParentWin, ?reset,  [{label,"Reset"}]),
    BFreeze = wxButton:new(ParentWin, ?freeze, [{label,"Freeze"}]),
    BLock   = wxButton:new(ParentWin, ?lock,   [{label,"Lock"}]),
    BUnlock = wxButton:new(ParentWin, ?unlock, [{label,"Unlock"}]),
    BDelete = wxButton:new(ParentWin, ?delete, [{label,"Delete"}]),

    SQ  = wxSlider:new(ParentWin, ?q_slider, ?default_q, 1, 500,
		       [{style, ?wxVERTICAL}]),
    SL  = wxSlider:new(ParentWin, ?l_slider, ?default_l, 1, 500,
		       [{style, ?wxVERTICAL}]),
    SK  = wxSlider:new(ParentWin, ?k_slider, ?default_k, 1, 500,
		       [{style, ?wxVERTICAL}]),
    Win = wxWindow:new(ParentWin, ?wxID_ANY, Options),

    ButtonSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(ButtonSizer, BReset),
    wxSizer:add(ButtonSizer, BFreeze),
    wxSizer:add(ButtonSizer, BLock),
    wxSizer:add(ButtonSizer, BUnlock),
    wxSizer:add(ButtonSizer, BDelete),

    SliderSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(SliderSizer, SQ, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(SliderSizer, SL, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(SliderSizer, SK, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(ButtonSizer, SliderSizer, [{flag, ?wxEXPAND}, {proportion, 1}]),

    WindowSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(WindowSizer, ButtonSizer, [{flag, ?wxEXPAND}, {proportion, 0}]),
    wxSizer:add(WindowSizer, Win, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxButton:setToolTip(BReset, "Remove selection and unlock all nodes."),
    wxButton:setToolTip(BFreeze, "Start/stop redraw of screen."),
    wxButton:setToolTip(BLock, "Lock all selected nodes."),
    wxButton:setToolTip(BUnlock, "Unlock all selected nodes."),
    wxButton:setToolTip(BDelete, "Delete all selected nodes."),

    wxButton:setToolTip(SQ, "Control repulsive force. This can also be"
			" controlled with the mouse wheel on the canvas."),
    wxButton:setToolTip(SL, "Control link length."),
    wxButton:setToolTip(SK, "Control attractive force. Use with care."),
    wxButton:setToolTip(Win,
			"Drag mouse while left mouse button is pressed "
			"to perform various operations. "
			"Combine with control key to select. Combine "
			"with shift key to lock single node."),

    wxButton:connect(BReset,  command_button_clicked),
    wxButton:connect(BFreeze, command_button_clicked),
    wxButton:connect(BLock,   command_button_clicked),
    wxButton:connect(BUnlock, command_button_clicked),
    wxButton:connect(BDelete, command_button_clicked),

    wxWindow:connect(SQ, command_slider_updated),
    wxWindow:connect(SL, command_slider_updated),
    wxWindow:connect(SK, command_slider_updated),

    wxWindow:connect(Win, enter_window),
    wxWindow:connect(Win, move),
    wxWindow:connect(Win, motion),
    wxWindow:connect(Win, mousewheel),
    wxWindow:connect(Win, key_up),
    wxWindow:connect(Win, left_down),
    wxWindow:connect(Win, left_up),
    wxWindow:connect(Win, right_down),
    wxWindow:connect(Win, paint,  [{skip, true}]),

    Pen   = wxPen:new({0,0,0}, [{width, 3}]),
    Font  = wxFont:new(12, ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[]),
    Brush = wxBrush:new({0,0,0}),

    Pid ! {self(), {?MODULE, WindowSizer}},

    wxWindow:setFocus(Win), %% Get keyboard focus

    Vs = reltool_fgraph:new(),
    Es = reltool_fgraph:new(),

    Me = self(),
    Ticker = spawn_link(fun() -> ticker_init(Me) end),

    loop( #state{ parent_pid = Pid,
                  q_slider = SQ,
		  l_slider = SL,
		  k_slider = SK,
                  mouse_act = ?move,
		  frame = ParentWin,
		  window = Win,
                  is_frozen = false,
		  ticker = Ticker},
          #graph{ vs = Vs,
		  es = Es,
		  pen = Pen,
		  font = Font,
		  brush = Brush}).

graph_add_node_unsure(Key, State, G = #graph{ vs = Vs }) ->
    case reltool_fgraph:is_defined(Key, Vs) of
        true  -> G;
        false -> graph_add_node(Key, State, G)
    end.

graph_add_node(Key, Color, G = #graph{ vs = Vs}) ->
    Q  = 20.0,   % repulsive force
    M  = 0.5,    % mass
    P  = {float(450 + rand:uniform(100)),
	  float(450 + rand:uniform(100))},
    G#graph{ vs = reltool_fgraph:add(Key,
				     #fg_v{ p = P, m = M, q = Q, color = Color},
				     Vs)}.

graph_change_node(Key, Color, G) ->
    case reltool_fgraph:get(Key, G#graph.vs) of
	undefined ->
	    G;
	V ->
	    G#graph{ vs = reltool_fgraph:set(Key, V#fg_v{ color = Color },
					     G#graph.vs)}
    end.

graph_del_node(Key, G = #graph{ vs = Vs0, es = Es0}) ->
    Vs = reltool_fgraph:del(Key, Vs0),
    Es = delete_edges(Es0, [Key]),
    G#graph{ vs = Vs, es = Es }.

graph_add_link(Key0, Key1, G = #graph{ es = Es}) ->
    K  = 60.0,   % attractive force
    L  =  5.0,   % spring length
    G#graph{ es = reltool_fgraph:add({Key0, Key1}, #fg_e{ k = K, l = L}, Es) }.

graph_del_link(Key0, Key1, G = #graph{ es = Es}) ->
    G#graph{ es = reltool_fgraph:del({Key0, Key1}, Es) }.

ticker_init(Pid) ->
    ticker_loop(Pid, 50).
ticker_loop(Pid, Time) ->
    receive after Time ->
        Pid ! {self(), redraw},
        T0 = erlang:monotonic_time(),
        receive {Pid, ok} -> ok end,
        T1 = erlang:monotonic_time(),
        D = erlang:convert_time_unit(T1-T0, native, milli_seconds),
        case round(40 - D) of
            Ms when Ms < 0 ->
                %io:format("ticker: wait is   0 ms [fg ~7s ms] [fps ~7s]~n",
		%          [s(D), s(1000/D)]),
                ticker_loop(Pid, 0);
            Ms ->
                %io:format("ticker: wait is ~3s ms [fg ~7s ms] [fps ~7s]~n",
		%          [s(Ms), s(D), s(1000/40)]),
                ticker_loop(Pid, Ms)
        end
    end.

delete_edges(Es, []) ->
    Es;
delete_edges(Es, [Key|Keys]) ->
    Edges = reltool_fgraph:foldl(fun
        ({{K1, K2}, _}, Out) when K1 =:= Key -> [{K1,K2}|Out];
        ({{K1, K2}, _}, Out) when K2 =:= Key -> [{K1,K2}|Out];
        (_, Out) -> Out
    end, [], Es),
    Es1 = lists:foldl(fun
        (K, Esi) -> reltool_fgraph:del(K, Esi)
    end, Es, Edges),
    delete_edges(Es1, Keys).


set_charge(Q, Vs) -> % Repulsive force
    F = fun({Key, Value}) -> {Key, Value#fg_v{ q = Q}} end,
    reltool_fgraph:map(F, Vs).

set_length(L, Es) -> % Spring length
    F  = fun({Ps, E}) -> {Ps, E#fg_e{ l = L}} end,
    reltool_fgraph:map(F, Es).

set_spring(K, Es) -> % Attractive force
    F  = fun({Ps, E}) -> {Ps, E#fg_e{ k = K}} end,
    reltool_fgraph:map(F, Es).

loop(S, G) ->
    receive
        #wx{id = ?reset, event = #wxCommand{type=command_button_clicked}} ->
	    %% Remove selection and unlock all nodes
            Q = ?default_q,
            L = ?default_l,
            K = ?default_k,
            wxSlider:setValue(S#state.q_slider, Q),
            wxSlider:setValue(S#state.l_slider, L),
            wxSlider:setValue(S#state.k_slider, K),
            Es = set_length(L, G#graph.es),
            Es2 = set_spring(K, Es),

            Vs2 =
		reltool_fgraph:map(fun({Key, V}) ->
					   {Key, V#fg_v{selected = false,
							type = dynamic,
							q = Q}}
				   end,
				   G#graph.vs),

            {Xs, Ys} =
		reltool_fgraph:foldl(fun({_Key,
					  #fg_v{p = {X, Y}}}, {Xs, Ys}) ->
					     {[X| Xs], [Y | Ys]}
				     end,
				     {[], []},
				     Vs2),
	   %% io:format("Before: ~p\n", [G#graph.offset]),
	    Offset =
                case length(Xs) of
                    0 ->
                        {0, 0};
                    N ->
			MeanX = (lists:sum(Xs) / N),
			MeanY = (lists:sum(Ys) / N),
			{SizeX, SizeY} = wxWindow:getSize(S#state.window),
			%% io:format("Min: ~p\n",
			%%           [{lists:min(Xs), lists:min(Ys)}]),
			%% io:format("Mean: ~p\n",
			%%           [{MeanX, MeanY}]),
			%% io:format("Max: ~p\n",
			%%           [{lists:max(Xs), lists:max(Ys)}]),
			%% io:format("Size: ~p\n", [{SizeX, SizeY}]),
			%% {XM - (XS / 2), YM - (YS / 2)}
			%% {0 - lists:min(Xs) + 20, 0 - lists:min(Ys) + 20}
			{0 - MeanX + (SizeX / 2), 0 - MeanY + (SizeY / 2)}
                end,
	    %% io:format("After: ~p\n", [Offset]),
	    loop(S, G#graph{vs = Vs2,
			    es = Es2,
			    offset = Offset,
			    offset_state = false});
        #wx{id = ?freeze, event = #wxCommand{type=command_button_clicked}} ->
	    %% Start/stop redraw of screen
            IsFrozen =
                case S#state.is_frozen of
                    true ->
                        S#state.ticker ! {self(), ok},
                        false;
                    false ->
                        true
                end,
            loop(S#state{is_frozen = IsFrozen}, G);
        #wx{id = ?lock, event = #wxCommand{type=command_button_clicked}} ->
	    %% Lock all selected nodes
            Vs = reltool_fgraph:map(fun
                            ({Key, V = #fg_v{selected = true}}) ->
				   {Key, V#fg_v{ type = static  }};
                            (KV) -> KV
                           end, G#graph.vs),
            loop(S, G#graph{ vs = Vs });
        #wx{id = ?unlock, event = #wxCommand{type=command_button_clicked}} ->
	    %% Unlock all selected nodes
            Vs = reltool_fgraph:map(fun
                            ({Key, V = #fg_v{selected = true}}) ->
				   {Key, V#fg_v{ type = dynamic }};
                            (KV) -> KV
                           end, G#graph.vs),
            loop(S, G#graph{ vs = Vs });
        #wx{id = ?delete, event = #wxCommand{type=command_button_clicked}} ->
	    %% Delete all selected nodes
            {Vs1, Keys} =
		reltool_fgraph:foldl(fun
					 ({Key,
					   #fg_v{ selected = true}},
					  {Vs, Ks}) ->
					     {reltool_fgraph:del(Key,Vs),
					      [Key|Ks]};
					 (_, {Vs, Ks}) ->
					     {Vs, Ks}
                                      end, {G#graph.vs,[]}, G#graph.vs),
            Es = delete_edges(G#graph.es, Keys),
            loop(S, G#graph{ vs = Vs1, es = Es});

        #wx{id = ?select, event = #wxCommand{type=command_button_clicked}} ->
            loop(S#state{ mouse_act = ?select }, G);

        #wx{id = ?move, event = #wxCommand{type=command_button_clicked}} ->
            loop(S#state{ mouse_act = ?move }, G);

        #wx{id = ?q_slider, event = #wxCommand{type=command_slider_updated,
					       commandInt = Q}} ->
            loop(S, G#graph{ vs = set_charge(Q, G#graph.vs)});
        #wx{id = ?l_slider, event = #wxCommand{type=command_slider_updated,
					       commandInt = L}} ->
            loop(S, G#graph{ es = set_length(L, G#graph.es)});
        #wx{id = ?k_slider, event = #wxCommand{type=command_slider_updated,
					       commandInt = K}} ->
            loop(S, G#graph{ es = set_spring(K, G#graph.es)});
        #wx{event=#wxKey{type=key_up, keyCode = 127}} -> % delete
            {Vs1, Keys} =
		reltool_fgraph:foldl(fun({Key,
					  #fg_v{ selected = true}},
					 {Vs, Ks}) ->
					     {reltool_fgraph:del(Key,Vs),
					      [Key|Ks]};
					(_, {Vs, Ks}) ->
					     {Vs, Ks}
				     end,
				     {G#graph.vs,[]}, G#graph.vs),
            Es = delete_edges(G#graph.es, Keys),
            loop(S, G#graph{ vs = Vs1, es = Es});
        #wx{event=#wxKey{type=key_up}} ->
            loop(S, G);
        #wx{event=#wxKey{type=key_down}} ->
            loop(S, G);

        %% mouse
        #wx{event=#wxMouse{type=left_down,
			   shiftDown=Shift,
			   controlDown=Ctrl,
			   x=X,
			   y=Y}} ->
            if
                Shift ->
                    loop(S, mouse_left_down_move(G, {X,Y}));
                Ctrl ->
                    loop(S, mouse_left_down_select(G, {X,Y}));
                S#state.mouse_act =:= ?move ->
                    loop(S, mouse_left_down_move(G, {X,Y}));
                S#state.mouse_act =:= ?select ->
                    loop(S, mouse_left_down_select(G, {X,Y}))
            end;
        #wx{event=#wxMouse{type=motion,
			   shiftDown=Shift,
			   controlDown=Ctrl,
			   x=X,
			   y=Y}} ->
            if
                Shift ->
                    loop(S, mouse_motion_move(G, {X,Y}));
                Ctrl ->
                    loop(S, mouse_motion_select(G, {X,Y}));
                S#state.mouse_act =:= ?move ->
                    loop(S, mouse_motion_move(G, {X,Y}));
                S#state.mouse_act =:= ?select ->
                    loop(S, mouse_motion_select(G, {X,Y}))
            end;
        #wx{event=#wxMouse{type=left_up,
			   shiftDown=Shift,
			   controlDown=Ctrl, x=X, y=Y}} ->
            if
                Shift ->
                    loop(S, mouse_left_up_move(G, {X,Y}, Shift));
                Ctrl ->
                    loop(S, mouse_left_up_select(G, {X,Y}));
                S#state.mouse_act =:= ?move ->
                    loop(S, mouse_left_up_move(G, {X,Y}, Shift));
                S#state.mouse_act =:= ?select ->
                    loop(S, mouse_left_up_select(G, {X,Y}))
            end;

        #wx{event=#wxMouse{type=right_down,x=_X,y=_Y}} ->
            loop(S, G);
        %% mouse wheel
        #wx{event=#wxMouse{type=mousewheel, wheelRotation=Rotation}} ->
            Q = wxSlider:getValue(S#state.q_slider),
            if
		Rotation > 0, Q > 5 ->
                    wxSlider:setValue(S#state.q_slider, Q - 4),
                    loop(S, G#graph{ vs = set_charge(Q - 4, G#graph.vs) });
		Rotation < 0 ->
                    wxSlider:setValue(S#state.q_slider, Q + 4),
                    loop(S, G#graph{ vs = set_charge(Q + 4, G#graph.vs) });
                true ->
                    loop(S, G)
            end;

        %% #wx{event=#wxClose{}} ->
        %%     catch wxWindow:'Destroy'(S#state.frame);
        %% #wx{id=?wxID_EXIT, event=#wxCommand{type=command_menu_selected}} ->
        %%     wxWindow:close(S#state.frame,[]);
        #wx{obj=_Win,event=#wxPaint{}} ->
            redraw(S, G),
            loop(S, G);
        #wx{obj=Win,event=#wxMouse{type=enter_window}} ->
            wxWindow:setFocus(Win),
            loop(S, G);

        %% Graph manipulation
        {add_node, Key, State} ->
	    loop(S, graph_add_node_unsure(Key, State, G));
        {del_node, Key} ->
	    loop(S, graph_del_node(Key, G));
        {change_node, Key, Color} ->
	    loop(S, graph_change_node(Key, Color, G));
        {add_link, {K0,K1}} ->
	    loop(S, graph_add_link(K0, K1, G));
        {del_link, {K0,K1}} ->
	    loop(S, graph_del_link(K0, K1, G));

        {Req, redraw} ->
	    {SizeX, SizeY} = wxWindow:getSize(S#state.window),
            Vs = reltool_fgraph:step(G#graph.vs,
				     G#graph.es,
				     {SizeX/2.0 - 20.0, SizeY/2.0}),
            case S#state.is_frozen of
                false ->
                    Req ! {self(), ok};
                true ->
                    ignore
            end,
            redraw(S, G),
            loop(S, G#graph{ vs = Vs} );

        {stop, Reason} ->
	    unlink(S#state.parent_pid),
	    exit(Reason);

        Other ->
            error_logger:format("~w~w got unexpected message:\n\t~tp\n",
                                [?MODULE, self(), Other]),
            loop(S, G)
    end.

mouse_left_down_select(G, {X0,Y0}) ->
    G#graph{ select = {{X0,Y0}, {X0,Y0}} }.

mouse_left_down_move(#graph{vs = Vs} = G, {X, Y}) ->
    % point on node?
    case coord_to_key(G, {X, Y}) of
        false ->
            G#graph{ offset_state = {X,Y}};
        {true, Key} ->
            V = #fg_v{ type = Type} = reltool_fgraph:get(Key, Vs),
            G#graph{ vs = reltool_fgraph:set(Key,
					     V#fg_v{ type = moving}, Vs),
		     select = {node, Key, Type, X, Y} }
    end.

coord_to_key(#graph{vs = Vs, offset = {Xo, Yo}}, {X, Y}) ->
    Xr = X - Xo,
    Yr = Y - Yo,
    reltool_fgraph:foldl(fun({Key, #fg_v{ p = {Px, Py}}}, _)
			       when abs(Px - Xr) < 10,
				    abs(Py - Yr) < 10 ->
				 {true, Key};
			    (_, Out) ->
				 Out
			 end, false, Vs).

mouse_left_up_select(G, {_X,_Y}) ->
    case G#graph.select of
        {{X0,Y0}, {X1, Y1}} ->
            {Xo, Yo} = G#graph.offset,
            Xmin = lists:min([X0,X1]) - Xo,
            Xmax = lists:max([X1,X0]) - Xo,
            Ymin = lists:min([Y0,Y1]) - Yo,
            Ymax = lists:max([Y1,Y0]) - Yo,
            Vs = reltool_fgraph:map(fun
                ({Key, Value = #fg_v{ p = {Px, Py}}})
   		   when Px > Xmin, Px < Xmax, Py > Ymin, Py < Ymax ->
                    {Key, Value#fg_v{ selected = true }};
                ({Key, Value}) -> {Key, Value#fg_v{ selected = false }}
            end, G#graph.vs),
            G#graph{ select = none, vs = Vs};
        _ ->
            G#graph{ select = none}
    end.

mouse_left_up_move(G = #graph{ select = Select, vs = Vs} = G, {X,Y}, Shift) ->
    case Select of
        {node, Key, _, X, Y} ->
            io:format("click: ~p\n", [Key]),
            G#graph{ select = none, offset_state = false };
        {node, Key, Type, _, _} ->
            V = reltool_fgraph:get(Key, Vs),
            Type2 =
                case Shift of
                    true -> static;
                    false -> Type
                end,
            G#graph{ select = none,
		     vs = reltool_fgraph:set(Key, V#fg_v{ type = Type2}, Vs),
		     offset_state = false };
        _ ->
            G#graph{ select = none, offset_state = false }
    end.

mouse_motion_select(G, {X,Y}) ->
    case G#graph.select of
        {P0, _P1} -> G#graph{ select = {P0, {X,Y}}};
        _        -> G
    end.

mouse_motion_move(G = #graph{ select = {node, Key, _, _, _}, vs = Vs}, {X,Y}) ->
    {Xo, Yo} = G#graph.offset,
    V = reltool_fgraph:get(Key, Vs),
    V2 = V#fg_v{ p = {float(X - Xo), float(Y - Yo)}},
    G#graph{ vs = reltool_fgraph:set(Key, V2, Vs) };
mouse_motion_move(G, {X,Y}) ->
    case G#graph.offset_state of
        {X1,Y1} ->
            {X0, Y0} = G#graph.offset,
            G#graph{ offset_state = {X,Y},
		     offset = {X0 - (X1 - X), Y0 - (Y1 - Y)} };
            _ ->
                G
    end.

redraw(#state{window=Win}, G) ->
    DC0  = wxClientDC:new(Win),
    DC   = wxBufferedDC:new(DC0),
    Size = wxWindow:getSize(Win),
    redraw(DC, Size, G),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(DC0),
    ok.

redraw(DC, _Size, G) ->
    wx:batch(fun() ->

        Pen   = G#graph.pen,
        Font  = G#graph.font,
        Brush = G#graph.brush,
        wxDC:setTextForeground(DC,?color_fg),
        wxBrush:setColour(Brush, ?color_bg),
        wxDC:setBrush(DC, Brush),
        wxDC:setBackground(DC, Brush),
        wxPen:setWidth(Pen, 1),
        wxDC:clear(DC),

        % draw vertices and edges
        wxPen:setColour(Pen, ?color_fg),
        wxDC:setPen(DC,Pen),

        %draw_es(DC, G#graph.es_pts, G#graph.offset),
        draw_es(DC, G#graph.vs, G#graph.es, G#graph.offset, Pen, Brush),
        draw_vs(DC, G#graph.vs, G#graph.offset, Pen, Brush),

        % draw selection box
        wxPen:setColour(Pen, ?color_fg),
        wxDC:setPen(DC,Pen),
        draw_select_box(DC, G#graph.select),

        % draw information text
        wxFont:setWeight(Font,?wxNORMAL),
        draw_text(DC,
		  reltool_fgraph:'size'(G#graph.vs),
		  reltool_fgraph:'size'(G#graph.es), G#graph.ke),
        ok
    end).

draw_select_box(DC, {{X0,Y0}, {X1,Y1}}) ->
    draw_line(DC, {X0,Y0}, {X1,Y0}, {0,0}),
    draw_line(DC, {X1,Y1}, {X1,Y0}, {0,0}),
    draw_line(DC, {X1,Y1}, {X0,Y1}, {0,0}),
    draw_line(DC, {X0,Y0}, {X0,Y1}, {0,0}),
    ok;
draw_select_box(_DC, _) ->
    ok.

draw_es(DC, Vs, Es, Po, Pen, Brush) ->
    reltool_fgraph:foreach(fun
        ({{K1, K2}, _}) ->
            #fg_v{ p = P1} = reltool_fgraph:'get'(K1, Vs),
            #fg_v{ p = P2} = reltool_fgraph:'get'(K2, Vs),
            draw_arrow(DC, P1, P2, Po, Pen, Brush)
        end, Es).

draw_arrow(DC, {X0,Y0}, {X1, Y1}, {X, Y}, Pen, Brush) ->
    Xdiff = (X0 - X1) / 4,
    Ydiff = (Y0 - Y1) / 4,
    X2 = X1 + Xdiff + X,
    Y2 = Y1 + Ydiff + Y,
    wxDC:setPen(DC, Pen),
    wxDC:setBrush(DC, Brush),

    draw_line(DC, {X0,Y0}, {X1, Y1}, {X, Y}),

    %% Draw arrow head
    Radians = calc_angle({X0, Y0}, {X1, Y1}),
    Len = 10,
    %% Angle = 30,
    %% Degrees = radians_to_degrees(Radians),
    %% Radians2 = degrees_to_radians(Degrees + Angle + 180),
    %% Radians3 = degrees_to_radians(Degrees - Angle + 180),
    Radians2 = Radians + 3.665191429188092,
    Radians3 = Radians + 2.617993877991494,
    {X3, Y3} = calc_point({X2, Y2}, Len, Radians2),
    {X4, Y4} = calc_point({X2, Y2}, Len, Radians3),
    Points = [{round(X2), round(Y2)},
	      {round(X3), round(Y3)},
	      {round(X4), round(Y4)}],
    wxDC:drawPolygon(DC, Points, []).

draw_line(DC, {X0,Y0}, {X1, Y1}, {X, Y}) ->
    wxDC:drawLine(DC,
		  {round(X0 + X), round(Y0 + Y)},
		  {round(X1 + X), round(Y1 + Y)}).

draw_vs(DC, Vs, {Xo, Yo}, Pen, Brush) ->
    reltool_fgraph:foreach(fun({Key,
				#fg_v{p ={X, Y},
				      color = Color,
				      selected = Sel}}) ->
				   String = s(Key),
				   case Sel of
				       true ->
					   wxPen:setColour(Pen, ?color_fg),
					   wxBrush:setColour(Brush, ?color_bg),
					   wxDC:setPen(DC,Pen),
					   wxDC:setBrush(DC, Brush),
					   SelProps = {round(X-12 + Xo),
						       round(Y-12 + Yo),
						       24,
						       24},
					   wxDC:drawRoundedRectangle(DC,
								     SelProps,
								     float(?ARC_R)),
					   ok;
				       false ->
					   ok
				   end,
				   case Color of
				       default ->
					   wxPen:setColour(Pen, ?color_default),
					   wxBrush:setColour(Brush,
							     ?color_default_bg);
				       alternate ->
					   wxPen:setColour(Pen,
							   ?color_alternate),
					   wxBrush:setColour(Brush,
							     ?color_alternate_bg);
				       {FgColor, BgColor} ->
					   wxPen:setColour(Pen, FgColor),
					   wxBrush:setColour(Brush, BgColor);
				       Color ->
					   wxPen:setColour(Pen, Color),
					   wxBrush:setColour(Brush, Color)
				   end,
				   wxDC:setPen(DC,Pen),
				   wxDC:setBrush(DC, Brush),
				   NodeProps = {round(X-8 + Xo),
						round(Y-8 + Yo),17,17},
				   wxDC:drawRoundedRectangle(DC,
							     NodeProps,
							     float(?ARC_R)),
				   wxDC:drawText(DC,
						 String,
						 {round(X + Xo),
						  round(Y + Yo)}),
				   ok;
			      (_) ->
				   ok
			   end,
			   Vs).

draw_text(DC, Nvs, Nes, _KE) ->
    VsString = "#nodes: " ++ integer_to_list(Nvs),
    EsString = "#links: " ++ integer_to_list(Nes),
    %% KEString = " ke: " ++ s(KE),
    wxDC:drawText(DC, VsString, {10,10}),
    wxDC:drawText(DC, EsString, {10,25}),
    %% wxDC:drawText(DC, KEString, {10,40}),
    ok.

s(Format, Terms) -> lists:flatten(io_lib:format(Format, Terms)).
s(Term) when is_float(Term) -> s("~.2f", [Term]);
s(Term) when is_integer(Term) -> integer_to_list(Term);
s(Term) when is_atom(Term) -> atom_to_list(Term);
s(Term) -> s("~p", [Term]).

%% Calclulate angle in radians for a line between two points
calc_angle({X1, Y1}, {X2, Y2}) ->
    math:atan2((Y2 - Y1), (X2 - X1)).

%% Calc new point at a given distance and angle from another point
calc_point({X, Y}, Length, Radians) ->
    X2 = round(X + Length * math:cos(Radians)),
    Y2 = round(Y + Length * math:sin(Radians)),
    {X2, Y2}.

%% %% Convert from an angle in radians to degrees
%% radians_to_degrees(Radians) ->
%%     Radians * 180 / math:pi().
%%
%% %% Convert from an angle in degrees to radians
%% degrees_to_radians(Degrees) ->
%%     Degrees * math:pi() / 180.
