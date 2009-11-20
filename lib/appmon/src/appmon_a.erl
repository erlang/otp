%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(appmon_a).

%%----------------------------------------------------------------------
%%
%% Monitors an application, i.e its supervision tree.
%%
%%----------------------------------------------------------------------
%%
%%
%%		INTRODUCTION
%%		------------
%%
%%		This file contains a description of the files involved
%%		and the communication between the appmon_a display
%%		manager and the appmon_a2 information gatherer. Further
%%		information on the placement algorithm can be found in
%%		the place.erl file.
%%
%%
%%		FILES
%%		-----
%%
%%		The supervision tree graphical software consists of
%%		the following files:
%%
%% appmon_a	Gen server driving the process display window.
%%		Responsible for assigning gs identifiers to all 
%%		processes and process link
%% appmon_a2	The process information gathering routines. 
%%		Works by following the process links from application 
%%		master once every second
%% dg		The process database is implemented as a shared 
%%		digraph (see manual pages for digraph) and this is 
%%		the routines handling this digraph. Since the digraph 
%%		is shared appmon_a2 will put some info into it that the 
%%		appmon_a later will modify. The structures used are 
%%		described in dg.hrl
%% place	Places a tree, decides the x and y coordinates (not 
%%		necessarily corresponding to window coordinates) of 
%%		processes (or vertices to be specific). Note that 
%%		special routines are used to transform the possibly 
%%		cyclic digraph into a strict tree before trying to 
%%		place it.
%%
%%
%%
%%		IMPLEMENTATION DETAIL
%%		---------------------
%%
%%		The appmon_a module will follow links between processes,
%%		starting with the application master. A unique
%%		reference is used to prevent infinite recursion. Note
%%		that this process and link gathering is done in the
%%		live digraph so that already known processes are
%%		updated with the reference and new ones are added to
%%		the digraph. After all processes and links have been
%%		added or updated a search is made for those processes
%%		and links that have an old reference. These are those
%%		processes and links that are not present in the
%%		application any more. Those are extracted from the
%%		digraph and then deleted and the extracts are then
%%		used (by appmon_a) to delete the appropriate gs
%%		objects. The responsibilities of appmon_a is thus 1) add
%%		all new processes and links to the digraph and 2) make
%%		a list of all those objects from the digraph that have
%%		been deleted.
%%
%%		When appmon_a2 has gathered all necessary information it
%%		notifies the appmon_a display manager. Note that this is
%%		implemented as a call (as opposed to a cast) to
%%		prevent appmon_a2 from changing the digraph while appmon_a
%%		uses it. appmon_a places all processes using the place
%%		module. place will place the processes in the x y
%%		planes, hopefully in a nice way, re-forming the
%%		digraph during the process into a strict tree using
%%		some simple heuristics, some links that makes the
%%		graph cyclic will be considered secondary and later
%%		coloured red. Note that the process links are not
%%		placed since their coordinates are those of the
%%		processes that they are links between. The place
%%		module is only concerned at a fairly high level of
%%		abstraction. Currently its x coordinates are used as
%%		real coordinates while the y coordinates must be
%%		scaled to correct values, thus the x plane is
%%		continous and the y plane is disctrete.
%%
%%		Having placed processes the new ones are drawn on the
%%		display along with all new process links, then all
%%		processes and process links are moved to their
%%		possibly new positions. The place module is not
%%		sensitive to changes in position and therefore has no
%%		concept of which nodes will have to be moved. hence
%%		all nodes are moved (but most of them probably to the
%%		same position as before)
%%
%%
%%
%%
%%----------------------------------------------------------------------



-export([start/2, start/3, stop/0]).


-record(astate, {app, name, client, digraph}).

-import(lists, [foreach/2]).

%% gen server stuff
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_info/2, terminate/2]).
-export([handle_call/3, code_change/3]).


-define(APPSPACE, 10).				% The space between apps
-define(NODEAREA_H, 90).			% The height of a node
-define(BUTTAREA_H, 80).			% The button area height
-define(APPBUTT_H, 20).				% Height of appl button
-define(EDITORW, 260).

-define(MAXWIDTH, 800).
-define(MINWIDTH, 382).
-define(MAXHEIGHT, 450).
-define(MINHEIGHT, 325).

-define(SUPVIEWTXT, "Sup. view").
-define(PROCVIEWTXT, "Proc. view").
-define(CLOSETXT, "Close").
-define(REFRESHTXT, "Refresh").
-define(SAVEOPTSTXT, "Save options").
-define(HELPTXT, "Help").

-define(CHARWIDTH, 7).				%Should use GS primitives

-define( darkkhaki, {189, 183, 107}).
-define( palegoldenrod, {238, 232, 170}).
-define( peachpuff4, {139, 119, 101}).
-define( red, red).
-define( darkgrey, {169, 169, 169}).
-define( lightgrey, {211, 211, 211}).
-define( royalblue, {65, 105, 225}).
-define( aquamarine4, {69, 139, 116}).
-define( palegreen4, {84, 139,  84}).
-define( darkseagreen, {105, 139, 105}).
-define( f_line_col, {150, 150, 255}).


-include("appmon_dg.hrl").


%%------------------------------------------------------------
%%------------------------------------------------------------


start(NodeName, AppName) ->
    gen_server:start_link(?MODULE, {NodeName, AppName, AppName}, []).

start(NodeName, AppName, AppId) ->
    gen_server:start_link(?MODULE, {NodeName, AppName, AppId}, []).


stop() ->
    ok.



%%------------------------------------------------------------
%% Public interface


%%------------------------------------------------------------
%% Administration

%% AppName is the name of the application, usually an atom like sasl
%% or kernel, AppId is the application pid or the application name,
%% either goes.
init({NodeName, AppName, AppId}) ->
    process_flag(trap_exit, true),
    {ok, Client} = appmon_info:start_link(NodeName, self(), []),
    init_ref(),
    init_foreign_places(),
    DG = digraph:new([cyclic, private]),
    State = #astate{app=AppId, name=AppName, client=Client, digraph=DG},
    refresh(State),
    setup_base_win(NodeName, AppName),
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(norequest, _From, State) ->
    {reply, null, State}.

%%------------------------------------------------------------
%% handle casts

handle_cast({ping, _Node, _From}, State) ->
    {noreply, State};
handle_cast(_Other, State) ->
    {noreply, State}.



%%------------------------------------------------------------
%% handle info

handle_info({gs, _, click, _, [?CLOSETXT|_]}, State) ->
    {stop, normal, State};
handle_info({gs, _, destroy, _, _}, State) ->
    {stop, normal, State};
handle_info({gs, _, click, _, [?REFRESHTXT|_]}, State) ->
    refresh(State),
    {noreply, State};
handle_info({gs, _, click, _, [?HELPTXT|_]}, State) ->
    HelpFile = filename:join([code:lib_dir(appmon),
			      "doc", "html", "part_frame.html"]),
    tool_utils:open_help(win(), HelpFile),
    {noreply, State};
handle_info({gs, Id, click, {mode, Mode}, _}, State) ->
    %%io:format("handle_info: Setting mode: ~p~n", [Mode]),
    set_mode(Id, Mode),
    {noreply, State};
handle_info({gs, _, click, _, [?SUPVIEWTXT|_]}, State) ->
    refresh(State, [{info_type, sup}]),
    {noreply, State};
handle_info({gs, _, click, _, [?PROCVIEWTXT|_]}, State) ->
    refresh(State, [{info_type, link}]),
    {noreply, State};
handle_info({gs, Id, buttonpress, _,[1, X, Y|_]}, State) ->
    %%io:format("Id clicked: ~p~n", [gs:read(Id, {find, {X, Y}})]),
    catch find_pid(State, Id, X, Y),
    set_default_mode(),
    {noreply, State};
handle_info({gs, Win, configure, _Data, [W, H|_]}, State) ->
    case win() of Win -> user_driven_resize(W, H);
	_-> ok
    end,
    {noreply, State};

handle_info({delivery, _S, pinfo, _N, Res}, State) ->
    appmon_txt:print(Res),
    {noreply, State};
handle_info({delivery, S, app, N, Res}, State) ->
    {delivery, _Serv, app, _Name, {Root, Vs, Ls, SecLs}} = 
	flush({delivery, S, app, N, Res}),
    update2(Vs, Root, Ls, SecLs, State),
    {noreply, State};

handle_info({kill}, State) ->
    {stop, normal, State};
handle_info({state}, State) ->
    {noreply, State};
handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};
handle_info(_Other, State) ->
    {noreply, State}.


%% Refresh sets new options for the request and forces an update of
%% the screen ant status.
refresh(State) ->
    refresh(State, []).
refresh(State, Opts) ->
    appmon_info:app(State#astate.client,
		    State#astate.name, true, Opts).



%% find_pid finds the pid of the clicked object. The scenario is that
%% the user clicks on an item in his window, that ObjId is searched
%% for among all nodes (vertices) and if found action is taken
%% depending on the current mode (see handle_info)
find_pid(State, Id, X, Y) ->
    %% Try to manage both versions of GS, remove first case later.
    ObjList = case gs:read(Id, {find, {X, Y}}) of
		  {error, _} ->
		      gs:read(Id, {hit, {X, Y}}); % Try new format
		  Num when is_integer(Num) -> [Num];
		  _Other -> []
	      end,
    DG = State#astate.digraph,
    All = appmon_dg:get(all, DG),
    find_pid2(ObjList, All, DG, State).

find_pid2([Id | Ids], All, DG, State) ->
    case search_for_pid(All, DG, Id) of
	{ok, _KeyStr, Pid} ->
	    handle_proc_press(mode(), Pid, State);
	_ -> find_pid2(Ids, All, DG, State)
    end;
find_pid2([], _All, _DG, _State) -> ok.

search_for_pid([V|Vs], DG, ObjId) ->
    VD = appmon_dg:get(data, DG, V),
    if  ObjId==VD#vdata.txt_obj ->
	    {ok, V, VD#vdata.type};
	true -> search_for_pid(Vs, DG, ObjId)
    end;
search_for_pid([], _DG, _ObjId) -> false.


%%
%% called when a process has been clicked on.
%%
handle_proc_press(info, Pid, State) -> 
    appmon_info:pinfo(State#astate.client, Pid, true, 
		      [{timeout, at_most_once}]);
handle_proc_press(send, Pid, _State) -> 
    {P, RawStr} = two_entries(winroot(), 250, 70,
			      "Send", "To: ", "Msg: ", 
			      pid_to_list(Pid), "", bg()),
    Str = case lists:last(RawStr) of
	       46 -> RawStr;
	       _ -> RawStr++"."
	   end,
    case erl_scan:string(Str) of
	{ok, Tokens, _} ->
	    case erl_parse:parse_term(Tokens) of
		{ok, Term} ->
		    case catch list_to_pid(P) of
			To when is_pid(To) -> To ! Term;
			_ -> error
		    end;
		_Error -> error
	    end;
	_Error -> error
    end;
handle_proc_press(trace, Pid, _State) -> 
    case trace_state(Pid) of
	true ->
	    io:format("Removing trace on ~p~n", [Pid]),
	    sys:trace(Pid, false),
	    set_trace_state(Pid, false);
	_Other ->
	    io:format("Putting trace on ~p~n", [Pid]),
	    sys:trace(Pid, true, 1000),
	    set_trace_state(Pid, true)
    end;
handle_proc_press(kill, Pid, _State) -> 
    exit(Pid, kill).


trace_state(Pid) -> get({trace_state, Pid}).
set_trace_state(Pid, State) -> put({trace_state, Pid}, State).

set_default_mode() ->
    {Id, Mode} = get(default_mode),
    case mode() of
	Mode -> true;
	_Other -> set_mode(Id, Mode)
    end.
set_default_mode(Id, Mode) ->
    put(default_mode, {Id, Mode}),
    select(Id),					%Dirty workaround
    set_default_mode().

set_mode(Id, Mode) ->
    %%io:format("mode=~p~n", [Mode]),
    set_mode(Mode),
    deselect(),
    select(Id).

set_mode(Mode)	-> put(mode, Mode).
mode()		-> get(mode).

flush({delivery, S, A, N, R}) ->
    receive
	{delivery, S, A, N, R2} ->
	    flush({delivery, S, A, N, R2})
    after 0 ->
	    {delivery, S, A, N, R}
    end.


%%------------------------------------------------------------
%% Real stuff
%% 
update2(Vs, Root, Ls, SecLs, State) ->
    DG = State#astate.digraph,
    Ref = mk_ref(),
    Added = add_procs(Vs, DG, Ref), 
    AddedLs = add_links(Ls, DG, Ref, primary), 
    AddedLs2 = add_links(SecLs, DG, Ref, secondary),
    DelLs = del_links(appmon_dg:eget(all, DG), DG, Ref),
    Dels = del_procs(appmon_dg:get(all, DG), DG, Ref),

    LastX = appmon_place:place(DG, Root),
    Width = lists:max(LastX),
    Height = length(LastX),

    %% Delete things from screen
    del(Dels), del(DelLs),
    
    %% Add vertices to screen
    foreach(fun(V) ->  draw_node(DG, V) end, Added),
    
    %% Add edges to screen
    foreach(fun(E) ->  draw_edge(DG, E) end, AddedLs),
    foreach(fun(E) ->  draw_edge(DG, E) end, AddedLs2),
    
    %% Move vertices on screen
    foreach(fun(V) ->  move_vertex(DG, V) end, appmon_dg:get(all, DG)),

    tree_driven_resize(Width, Height),

    gs:config(win(), {map, true}),		%Make win visible
    ok.

%% Make an integer reference, could have used make_ref BIF but didn't
mk_ref() -> put(reference, get(reference)+1).
init_ref() -> put(reference, 0).


%% Add processes (vertices) to digraph, use the string repr of pid as
%% key in digraph.
add_procs([{Pid, Str}|Vs], DG, Ref) ->
    case appmon_dg:add(DG, Str, mk_vdata(Str, Pid, Ref), Ref) of
	known	-> add_procs(Vs, DG, Ref);
	updated -> add_procs(Vs, DG, Ref);
	_	-> 
	    case lists:member(32, Str) of
		true -> 
		    appmon_dg:set(x, DG, Str, foreign), % UNHOLY!
		    add_procs(Vs, DG, Ref);	% Don't add foreign
		_ -> [Str | add_procs(Vs, DG, Ref)]
	    end
    end;
add_procs([], _DG, _Ref) -> [].


%% Add links to digraph. NOTE that foreign links get a special weight
%% and that no link is added if it goes to a process not in the set of
%% vertices.
%%
%% OTP-1970: Check that linked-to processes really exist.
%%
add_links([{V1, V2}|Ls], DG, Ref, Weight) ->
    L = case lists:member(32, V2) of
	    true -> {V1, V2, foreign};
	    _ ->    {V1, V2, Weight}
	end,
    case appmon_dg:get(data, DG, V2) of
	false -> add_links(Ls, DG, Ref, Weight);
	VD ->
	    if  VD#vdata.ref == Ref ->		% OTP-1970
		    case appmon_dg:eadd(DG, L, mk_edata(L, Ref), Ref) of
			known -> add_links(Ls, DG, Ref, Weight);
			updated -> add_links(Ls, DG, Ref, Weight);
			_Other -> [L | add_links(Ls, DG, Ref, Weight)]
		    end;
		true ->
		    add_links(Ls, DG, Ref, Weight)
	    end
    end;
add_links([], _DG, _Ref, _Weight) -> [].

%% Make an edge data structure
mk_edata(_L, Ref) ->
    #edata{ref=Ref}.

%% Make a vertex data structure, note that pid can be either a pid or
%% a port, we're not picky here.
mk_vdata(P, Pid, Ref) ->
    #vdata{ref=Ref, type=Pid, txt=P, width=width(P)}.
width(Txt) -> ?CHARWIDTH*length(Txt)+10.	% Should use GS stuff instead


%% Delete those processes that have the wrong reference from the
%% digraph. Returns a list of deleted procs and their data (to be able
%% to erase things on screen).
del_procs([V|Vs], DG, Ref) ->
    VD = appmon_dg:get(data, DG, V),
    if  VD#vdata.ref /= Ref -> appmon_dg:del(DG, V),
			       [{V, VD} | del_procs(Vs, DG, Ref)];
	true -> del_procs(Vs, DG, Ref)
    end;
del_procs([], _DG, _Ref) -> [].


%% Deletes links that have the wrong reference from the digraph, note
%% that the weight of the edge is not considered here. Returns a list
%% of deleted links and their data (to be able to erase things on
%% screen).
del_links([L | Ls], DG, Ref) ->
    ED = appmon_dg:eget(data, DG, L),
    if  ED#edata.ref /= Ref -> appmon_dg:edel(DG, L),
			       [{L, ED} | del_links(Ls, DG, Ref)];
	true -> del_links(Ls, DG, Ref)
    end;
del_links([], _DG, _Ref) -> [].

%% Del deletes the GS objects of the list of should-be-deleted
%% items. Returns nothing particular.
del(L) ->
    lists:foreach(fun({{V1, V2, Weight}, D}) when is_record(D, edata) ->
			  if Weight== foreign ->
				  dealloc_foreign({V1, V2, Weight});
			     true -> ok end,
			  destroy(D#edata.line);
		     ({_I, D}) when is_record(D, vdata) ->
			  destroy(D#vdata.sym_obj),
			  destroy(D#vdata.txt_obj)
		  end, L).


move_vertex(DG, V) ->
    VData = appmon_dg:get(data, DG, V),
%%    io:format("Vertex ~p data: x:~p, oldx:~p, y:~p, oldy:~p offs:~p~n",
%%	      [V, VData#vdata.x, VData#vdata.origx, 
%%	       VData#vdata.y, VData#vdata.origy, offsetx()]),
    if  VData#vdata.x == foreign -> ok;
	true ->
	    X = VData#vdata.x,
	    Y = scaley(VData#vdata.y),
	    OldX = VData#vdata.origx,
	    OldY = scaley(VData#vdata.origy),
	    if  X==OldX, Y==OldY -> true;
		true ->
		    %%io:format("Moving vertex: ~p~n", [V]),
		    
		    W = VData#vdata.width,
		    {BoxC, TxtC} = calc_box_coords(X, Y, W),
		    
		    %% move the symbol and text
		    gs:config(VData#vdata.sym_obj, [{coords, BoxC}]),
		    gs:config(VData#vdata.txt_obj, [{coords, TxtC}]),
		    foreach(fun(E) -> move_edge(DG, E) end, 
			    appmon_dg:get(edges, DG, V))
	    end
    end.


move_edge(DG, E) ->
    {{V1, V2, Weight}, V1, V2, ED} = appmon_dg:eget(edge, DG, E),
    VD1 = appmon_dg:get(data, DG, V1),
    VD2 = appmon_dg:get(data, DG, V2),
    Line = ED#edata.line,
    move_line(DG, VD1, VD2, Line, Weight).
move_line(DG, VD1, VD2, Line, Weight) when is_list(Line) ->
    move_line(DG, VD1, VD2, hd(Line), Weight);
move_line(_DG, VD1, VD2, Line, Weight) ->
    Coords = calc_coords(VD1, VD2, Weight),
    gs:config(Line, [{coords, Coords} | line_opts(Weight)]).

%% Draw the vertex on the canvas
draw_node(DG, V) ->
    %%io:format("Drawing~n",[]),
    Data = appmon_dg:get(data, DG, V),

    X      = Data#vdata.x,
    Y      = scaley(Data#vdata.y),

    {Sym, Txt} = draw(rectangle, sup_col(), Data#vdata.txt, X, Y,
		      Data#vdata.width),
    
    appmon_dg:av(DG, V, Data#vdata{sym_obj=Sym, txt_obj=Txt}),
    true.

%% Draws a symbol (rectangle for instance) on the canvas.
draw(Symbol, Col, Txt, X, Y, W) ->
    {BoxC, TxtC} = calc_box_coords(X, Y, W),
    Box = gs:create(Symbol, canvas(), [{coords, BoxC}, {fill, Col}]),
    
    TxtObj = gs:create(text, canvas(), [{coords, TxtC},
					{anchor, c},
					%%{buttonpress, true},
					{text, Txt}]),
    {Box, TxtObj}.

%% Returns {BoxCoords, TextCoords}
calc_box_coords(X, Y, W) ->
    {[{X, Y-radius()}, {X+W, Y+radius()}],   [{X+trunc(W/2), Y}]}.


%% Draw a line on the canvas
draw_edge(DG, E) ->
    {V1, V2, Weight} = E,
    Line = draw_line(DG, V1, V2, Weight),
%%    io:format("Line: ~p~n", [Line]),
    appmon_dg:eset(line, DG, E, Line).


%% From is parent, To is child. If To is not a record then we are
%% dealing with a link to a process on another node. Find a suitable
%% place at the left margin and write the process name there and draw
%% a line to it.
%%
draw_line(DG, From, To, foreign) ->
    VD1 = appmon_dg:get(data, DG, From),
    Y = find_foreign_place(VD1#vdata.y+0.5, foreign_places()),
    add_foreign_place({From, To, foreign}, Y),
%%    io:format("New Y: ~p~n", [Y]),
    appmon_dg:set(x, DG, To, 0),
    appmon_dg:set(y, DG, To, Y),
    VD2 = appmon_dg:get(data, DG, To),
    Coords = calc_coords(VD1, VD2, foreign),
%%    io:format("Node coords: ~p~n", [Coords]),
    L = gs:create(line, canvas(), [{coords, Coords} | line_opts(foreign)]),
    T = gs:create(text, canvas(), [{coords, [{0, 5+scaley(Y)}]},
				   {anchor, nw}, {fg, f_line_col()},
%%				   {font, {screen, 10}}, 
				   {text, To}]),
    [L, T];

draw_line(DG, From, To, Weight) ->
    VD1 = appmon_dg:get(data, DG, From),
    VD2 = appmon_dg:get(data, DG, To),
    Coords = calc_coords(VD1, VD2, Weight),
    gs:create(line, canvas(), [{coords, Coords} | line_opts(Weight)]).

%%----------------------------------------------------------------------
%%
%% Line coordinate calculation
%%
%% Calculate coordinates for edges (links, lines). Primaries have a
%% nice knee and secondaries are oriented differently. If weight is
%% foreign then we will calculate a nice line to the left margin.
%%
calc_coords(From, To, foreign) ->
    Y = scaley(To#vdata.y),
    X1 = From#vdata.x+trunc(From#vdata.width/2),
    Y1 = scaley(From#vdata.y)+radius(),
    [{0, Y}, {X1-40, Y}, {X1, Y1}];
calc_coords(From, To, primary) ->
    X1 = From#vdata.x+trunc(From#vdata.width/2),
    Y1 = scaley(From#vdata.y)+radius(),
    
    X2 = To#vdata.x+trunc(To#vdata.width/2),
    Y2 = scaley(To#vdata.y)-radius(),
    
    Y3 = trunc((Y1+Y2)/2),
    [{X1, Y1}, {X1, Y3}, {X2, Y3}, {X2, Y2}];

calc_coords(V1, V2, _Weight) ->
    Y1  = scaley(V1#vdata.y),
    X1  = V1#vdata.x,
    X1w = X1+V1#vdata.width,
    Y2  = scaley(V2#vdata.y),
    X2  = V2#vdata.x,
    X2w = X2+V2#vdata.width,

    if  Y1 == Y2 -> calc_u(X1, X1w, Y1, X2, X2w);
	X1w < X2 -> calc_s(X1w, Y1, X2, Y2);
	X2w < X1 -> calc_s(X1, Y1, X2w, Y2);
	true ->
	    D1 = abs(X1-X2), D2 = abs(X1w-X2w),
	    if  D1 > D2 -> calc_rbrack(X1w, Y1, X2w, Y2);
		true -> calc_lbrack(X1, Y1, X2, Y2)
	    end
    end.

%% Calculates line coordinates that will go from bottom of one node to
%% bottom of another on the same level. The line will form a nice "U".
calc_u(X1, X1w, Y, X2, X2w) ->
    X3 = trunc((X1+X1w)/2),
    X4 = trunc((X2+X2w)/2),
    Y2 = Y+radius(),
    Y3 = Y2+20,
    [{X3, Y2}, {X3, Y3}, {X4, Y3}, {X4, Y2}].

%% Calculates line coordinates that will go from right (or left) side
%% to right (or left) side, thus forming a "[" or a "]" (bracket).
calc_rbrack(X1, Y1, X2, Y2) ->
    X3 = 20 + if  X1 > X2 -> X1;
		  true -> X2
	      end,
    [{X1, Y1}, {X3, Y1}, {X3, Y2}, {X2, Y2}].
calc_lbrack(X1, Y1, X2, Y2) ->
    X3 = -20 + if  X1 < X2 -> X1;
		   true -> X2
	      end,
    [{X1, Y1}, {X3, Y1}, {X3, Y2}, {X2, Y2}].

%% Calculates line coordinates that will form a nice "S"
calc_s(X1, Y1, X2, Y2) ->
    X3 = trunc((X1+X2)/2),
    [{X1, Y1}, {X3, Y1}, {X3, Y2}, {X2, Y2}].


%% Options for lines (edges, links)
line_opts(foreign)	-> [{width, 2}, {smooth, true},  {fg, f_line_col()}];
line_opts(primary)	-> [{width, 2}, {smooth, false}, {fg, line_col()}];
line_opts(_)		-> [{width, 2}, {smooth, true}, {fg, sec_line_col()}].



%%----------------------------------------------------------------------
%%
%% Handling of links to foreign processes
%%
%%----------------------------------------------------------------------
dealloc_foreign(L) ->
%%    io:format("deallocing foreign: ~p~n", [L]),
    put(foreign_places, lists:keydelete(L, 1, foreign_places())).
add_foreign_place(V, Y) ->
%%    io:format("Adding foreign: ~p~n", [V]),
    put(foreign_places, [{V, Y} | foreign_places()]).
foreign_places() ->
    get(foreign_places).
init_foreign_places() ->
    put(foreign_places, []).

%% Find a good place for the foreign node
find_foreign_place(StartY, L) ->
    case lists:keysearch(StartY, 2, L) of
	{value, _} -> find_foreign_place(StartY + 1, L);
	_ -> StartY
    end.
    

%%------------------------------------------------------------
%%
%% Graphical stuff
%%

setup_base_win(NodeName, AppName) ->
    set_winroot(gs:start([{kernel,true}])),

    W = ?MINWIDTH, H = ?MINHEIGHT,
    
    Name = "APPMON: " ++ atom_to_list(AppName) ++ " on " ++
	atom_to_list(NodeName),

    set_win(gs:create(window, winroot(), [{title, Name}, %%{bg, red},
					  {x, 250}, {y, 100},
					  {width, W}, {bg, win_col()},
					  {height, H+?BUTTAREA_H}])),
    %% standard buttons
    mk_std_butts(win(), W),
    set_canvas(gs:create(canvas, win(),[{x,0}, {y,?BUTTAREA_H}, 
					{width, W}, {height, H}, 
					{bg, bg()}, 
					{buttonpress, true}])),
    
    set_old_win_size(width, gs:read(win(), width)),
    set_old_win_size(height, gs:read(win(), height)),

%%    gs:config(win(), {map, true}),		%Make win visible
    ok.


nice_line_coords(W, H) ->
    [{0,H-10}, {W,H-10}].

%%------------------------------
%% Button stuff

mk_butt_area(Win, W) ->
    H = ?BUTTAREA_H,
    F = gs:create(frame, Win,[{x,0}, {y,0}, %%{bg, frame_col()},
			      {width,W}, {height,H}]),
    C = gs:create(canvas,F,[{x,0}, {y,0}, {width, W}, {height, H-9}, 
			    {bg, bg()}]),
    L = gs:create(line,C,[{coords,nice_line_coords(W, H)}]),

    MB = gs:create(menubar, Win, []),

    FMB = gs:create(menubutton, MB, [{label, {text, "File"}}]),
    FM = gs:create(menu, FMB, []),
    gs:create(menuitem, FM, [{label, {text, ?CLOSETXT}}]),
    
    OMB = gs:create(menubutton, MB, [{label, {text, "Options"}}]),
    OM = gs:create(menu, OMB, []),
    gs:create(menuitem, OM, [{label, {text, ?REFRESHTXT}}]),
    Group = now(),
    gs:create(menuitem, OM, [{itemtype, separator}]),
    gs:create(menuitem, OM, [{label, {text, ?SUPVIEWTXT}}, {itemtype, radio},
			     {group, Group}]),
    gs:create(menuitem, OM, [{label, {text, ?PROCVIEWTXT}}, {select, true}, 
			     {group, Group}, {itemtype, radio}]),

    HMB = gs:create(menubutton, MB, [{label, {text, "Help"}}, {side, right}]),
    HM = gs:create(menu, HMB, []),
    gs:create(menuitem, HM, [{label, {text, ?HELPTXT}}]),
  
    {F, C, L}.

mk_std_butts(Win, W) ->
    {F, C, L} = mk_butt_area(Win, W),
    set_bframe(F), set_bcanvas(C), set_bline(L),

    IButt = mk_mode_butt({text, "Info"}, {mode, info}, 10),
    mk_mode_butt({text, "Send"}, {mode, send}, 90),
    mk_mode_butt({text, "Trace"}, {mode, trace}, 170),
    mk_mode_butt({text, "Kill"}, {mode, kill}, 250),

    set_default_mode(IButt, info),
    
    true.

select(Id) ->
    gs:config(Id, {bg, sel_col()}),
    set_selected(Id).

deselect() ->
    gs:config(selected(), {bg, de_sel_col()}).

mk_mode_butt(Label, Data, X) ->
    gs:create(button, bframe(), [{label, Label}, {x, X}, {y, 35},
				 {data, Data}, {width, 70}, {height, 25}]).

%%------------------------------------------------------------
%% Graphical utilities

mk_frame(P, X, Y, W, H, BG) ->
    gs:create(frame, P, [{x, X}, {y, Y}, {width, W}, {height, H}, {bg, BG}]).
    
mk_butt(P, X, Y, W, H, Txt) ->
    gs:create(button, P, [{x, X}, {y, Y}, {height, H}, {width, W}, 
			  {label, {text, Txt}}]).

mk_butt(P, X, Y, Txt) ->
    mk_butt(P, X, Y, 70, 20, Txt).

mk_label(P, X, Y, W, H, Txt, BG) ->
    gs:create(label, P, [{x, X}, {y, Y}, {height, H}, {width, W}, 
			 {label, {text, Txt}}, {bg, BG}]).

mk_entry(P, X, Y, W, H, Txt, BG) ->
    gs:create(entry, P, [{x, X}, {y, Y}, {height, H}, {width, W}, {text, Txt},
			 {bg, BG}, {keypress, true}]).


two_entries(Root, W, H, Name, LTxt1, LTxt2, StartTxt1, StartTxt2, BG) ->
    Win = gs:create(window, Root, [{title, Name}, %%{bg, red},
				   %%{x, X}, {y, Y},
				   {width, W}, {bg, BG},
				   {height, H}]),
    F = mk_frame(Win, 0, 0, W, H, BG),

    mk_label(F, 10, 10, 30, 20, LTxt1, BG),
    mk_label(F, 10, 40, 30, 20, LTxt2, BG),

    E1 = mk_entry(F, 40, 10, 120, 20, StartTxt1, BG),
    E2 = mk_entry(F, 40, 40, 120, 20, StartTxt2, BG),

    Ok = mk_butt(F, 170, 10, "Ok"),
    Cn = mk_butt(F, 170, 40, "Cancel"),
    gs:config(Win, {map, true}),

    Ret = case catch two_entries_loop(E1, E2, Ok, Cn) of
	      {P2, Msg} -> {P2, Msg};
	      _Other ->
		  false
	  end,
    gs:destroy(Win),
    Ret.


two_entries_loop(E1, E2, Ok, Cn) ->
    receive
	{gs, Ok, click, _, _} ->
	    {gs:read(E1, text),
	     gs:read(E2, text)};
	{gs, E1, keypress, _, ['Return'|_]} ->
	    {gs:read(E1, text),
	     gs:read(E2, text)};
	{gs, E2, keypress, _, ['Return'|_]} ->
	    {gs:read(E1, text),
	     gs:read(E2, text)};
	{gs, _, keypress, _, _} ->
	    two_entries_loop(E1, E2, Ok, Cn);
	{gs, Cn, click, _, _} ->
	    true
    end.

%%--------------------------------------------------------------------
%%
%% Resizing routines.
%%
%%	Resizing deals with a number of different interdependent
%%	sizes. Top size is the window size. From window size all other
%%	sizes are calculated, we call this the "leader" size. The
%%	canvas is usually the same size as the window, except for the
%%	row of buttons at the top of the windoww. The canvas is also
%%	displaced when the tree is smaller than the minimum window
%%	size.
%%
%%
%%	Window size - the size of the outer window. Note that
%%	provisions must be made for the button area at the top of the
%%	window, this is called WinAdj. this is the only item taht
%%	changes when the user manually resizes the window.
%%
%%	Canvas size - The size of the canvas, should be equal to
%%	window size less the button area. Must be adjusted when the
%%	window has been manually resized. The canvas also has a
%%	scrollregion which must be maintained. Note that we could have
%%	used the canvas size as "leading" size, but this did not work
%%	since the canvas doesn't fill the complete window when the
%%	tree is smaller than the window.
%%
%%	Tree size - The size of the tree. This may change whenever a
%%	new tree is delivered from the info routine.
%%
%%	Dim - All these size adjustments are done in some dimension
%%	(width or height).
%%
%%	Max, Min - The outmost window may not become larger than Max
%%	size or smaller than Min size when resized by the tree
%%	size. The user resizing is not restricted to these sizes.
%%
%% Scrollbars:
%%
%%	Scrollbars are used whenever necessary, whenever the tree size
%%	is bigger than canvas size (in any dimension).
%%
%% Invariants:
%%
%%	The three sizes are not varied at the same time. When the
%%	window is resized because of a new tree, then window and
%%	canvas must be updated. When the user has resized, then only
%%	the canvas must be changed (to fit in the window)
%%
%% Tree driven resize
%%
%%	This occurs when the tree has been updated. The window may
%%	grow and shrink to fit the tree, but may not be smaller than
%%	Min and not bigger than Max (scrollbars will be used instead)
%%

tree_driven_resize(TWidth, THeight) ->
    gs:config(win(), {configure, false}),
    Width = TWidth+20,
    Height = scaley(THeight+1),
    put({width, tree}, Width),
    put({height, tree}, Height),
    adjust_win(width, Width),
    adjust_win(height, Height),
    fit_tree_to_win(width, Width),
    fit_tree_to_win(height, Height),
    check_scroll_region(Width, Height, gs:read(canvas(), scrollregion)),
    gs:config(win(), {configure, true}),
    ok.


%% Will adjust the window size to the tree size (given the max and min
%% restrictions.
adjust_win(Dim, TreeSize) ->
    case get({Dim, user_resize}) of
	true -> ok;
	_ ->
	    WinSize = gs:read(win(), Dim),%%get_dim(Dim, win()),
	    case get_wanted_winsize(Dim, TreeSize) + winadj(Dim) of
		WinSize -> ok;
		NewSize -> 
		    %%set(Dim, win(), NewSize+winadj(Dim))
		    set_old_win_size(Dim, NewSize),
		    gs:config(win(), {Dim, NewSize})
	    end
    end.

get_wanted_winsize(Dim, Size) ->
    Max = maxsize(Dim), Min = minsize(Dim),
    if  Size > Max	-> Max;
	Size < Min	-> Min;
	true		-> Size
    end.

set_old_win_size(Dim, Size) -> put({Dim, winsize}, Size).
old_win_size(Dim) -> get({Dim, winsize}).


%%--------------------------------------------------------------------
%%
%% user_driven_resize
%%
%%	This is when the user drags the window to some size. This is
%%	basically the same as a tree resize, only this time the window
%%	itself must not be fiddled with. When the window has been
%%	resized this way then normal tree driven resize is not allow
%%	to alter the size in that dimension. User overrides.
%%
user_driven_resize(W, H) ->
    gs:config(win(), {configure, false}),
    check_user_resize(width, W),
    check_user_resize(height, H),
    check_scroll_region(get({width, tree}), get({height, tree}),
			gs:read(canvas(), scrollregion)),
    gs:config(win(), {configure, true}).

check_user_resize(Dim, Size) ->
    case old_win_size(Dim) of
	Size -> false;
	_ ->
	    put({Dim, user_resize}, true),
	    set_old_win_size(Dim, Size),
	    fit_tree_to_win(Dim, get({Dim, tree}))
    end.



%%--------------------------------------------------------------------
%%
%% General resizing routines
%%
%% fit_tree_to_win - Will fit the canvas into a pre-sized window in
%% one dimension.
%%
fit_tree_to_win(Dim, TreeSize) ->
    Size = gs:read(win(), Dim) - winadj(Dim),
    set_canvas_offset(Dim, Size, TreeSize),
    set_button_width(Dim, Size),
    if  TreeSize > Size ->
	    gs:config(canvas(), {trans_dim2vh(Dim), trans_dim2enable(Dim)});
	TreeSize < Size ->
	    gs:config(canvas(), {trans_dim2vh(Dim), false});
	true ->
	    gs:config(canvas(), {trans_dim2vh(Dim), false})
    end.


%%------------------------------
%% Set the canvas width and displacement in x.
set_canvas_offset(height, Size, _) -> 
    gs:config(canvas(), {height, Size});
set_canvas_offset(width, Size, Size) -> 
    gs:config(canvas(), [{x, 0}, {width, Size}]);
set_canvas_offset(width, Size, TreeSize) when Size<TreeSize -> 
    gs:config(canvas(), [{x, 0}, {width, Size}]);
set_canvas_offset(width, Size, TreeSize) when Size>TreeSize-> 
    Val = trunc((Size-TreeSize)/2),
    gs:config(canvas(), [{x, Val}, {width, Size-Val}]).

%%------------------------------
%% Set the button area width
set_button_width(height,_) -> ok;
set_button_width(width, W) ->
    gs:config(bcanvas(), [{width, W}]),
    gs:config(bframe(), [{width, W}]),
    gs:config(bline(), [{coords, nice_line_coords(W, ?BUTTAREA_H)}]).
    

%%------------------------------
%% Update the scrollregion size if needed.
check_scroll_region(W, H, {_, _, W, H}) -> ok;
check_scroll_region(W, H, {_, _, _, _}) -> 
    gs:config(canvas(), {scrollregion, {0, 0, W, H}}).


%% Window sizing primitives
winadj(width)	-> 0;
winadj(height)	-> ?BUTTAREA_H.
maxsize(width)	-> ?MAXWIDTH;
maxsize(height)	-> ?MAXHEIGHT.
minsize(width)	-> ?MINWIDTH;
minsize(height)	-> ?MINHEIGHT.



trans_dim2vh(width) -> hscroll;
trans_dim2vh(height) -> vscroll.
trans_dim2enable(width) -> bottom;
trans_dim2enable(height) -> right.





%%------------------------------------------------------------
%% Global Window info

winroot() ->		get(winroot).
win() ->		get(win).
canvas() ->		get(canvas).
bframe() ->		get(bframe).
bcanvas() ->		get(bcanvas).
bline() ->              get(bline).
set_winroot(X) ->	put(winroot, X).
set_win(X) ->		put(win, X).
set_canvas(X) ->	put(canvas, X).
set_bframe(X) ->	put(bframe, X).
set_bcanvas(X) ->	put(bcanvas, X).
set_bline(X) ->		put(bline, X).

sup_col()	-> ?darkkhaki.
%%work_col()	-> ?orange.
bg()		-> ?palegoldenrod.
line_col()	-> ?peachpuff4. %% saddlebrown.darkgoldenrod
f_line_col()	-> ?royalblue. %% saddlebrown.darkgoldenrod
sec_line_col()	-> ?red.
win_col()	-> bg(). %%darkolivegreen.

sel_col()	-> ?darkgrey.
de_sel_col()	-> ?lightgrey.
set_selected(Id)-> put(selected, Id).
selected()	-> get(selected).

scaley(Y)	-> 55*Y.
radius()	-> 10.

destroy(undefined)	-> true;
destroy(L) when is_list(L) -> lists:foreach(fun(X) -> destroy(X) end , L);	
destroy(Win)		-> gs:destroy(Win).

