%%
%%  wings.hrl --
%%
%%     Global record definition and defines.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings.hrl,v 1.1 2009/01/25 18:55:33 kostis Exp $
%%

-include("wings_intl.hrl").

-ifdef(NEED_ESDL).
-include_lib("esdl/include/sdl.hrl").
-include_lib("esdl/include/sdl_events.hrl").
-include_lib("esdl/include/sdl_video.hrl").
-include_lib("esdl/include/sdl_keyboard.hrl").
-include_lib("esdl/include/sdl_mouse.hrl").
-include_lib("esdl/src/sdl_util.hrl").
-define(CTRL_BITS, ?KMOD_CTRL).
-define(ALT_BITS, ?KMOD_ALT).
-define(SHIFT_BITS, ?KMOD_SHIFT).
-define(META_BITS, ?KMOD_META).
-endif.

-define(WINGS_VERSION, ?wings_version).

-define(CHAR_HEIGHT, wings_text:height()).
-define(CHAR_WIDTH, wings_text:width()).

-define(LINE_HEIGHT, (?CHAR_HEIGHT+2)).
-define(GROUND_GRID_SIZE, 1).
-define(CAMERA_DIST, (8.0*?GROUND_GRID_SIZE)).
-define(NORMAL_LINEWIDTH, 1.0).
-define(DEGREE, 176).				%Degree character.

-define(HIT_BUF_SIZE, (1024*1024)).

-define(PANE_COLOR, {0.52,0.52,0.52}).
-define(BEVEL_HIGHLIGHT, {0.9,0.9,0.9}).
-define(BEVEL_LOWLIGHT, {0.3,0.3,0.3}).
-define(BEVEL_HIGHLIGHT_MIX, 0.5).
-define(BEVEL_LOWLIGHT_MIX, 0.5).

-define(SLOW(Cmd), begin wings_io:hourglass(), Cmd end).
-define(TC(Cmd), wings_util:tc(fun() -> Cmd end, ?MODULE, ?LINE)).

-ifdef(DEBUG).
-define(ASSERT(E), case E of
		       true -> ok;
		       _ ->
			   erlang:error({assertion_failed,?MODULE,?LINE})
		   end).
-define(CHECK_ERROR(), wings_gl:check_error(?MODULE, ?LINE)).
-else.
-define(ASSERT(E),ok).
-define(CHECK_ERROR(), ok).
-endif.

%% Display lists per object.
%%  Important: Plain integers and integers in lists will be assumed to
%%  be display lists. Arbitrary integers must be stored inside a tuple
%%  or record to not be interpreted as a display list.
-record(dlo,
	{work=none,				%Workmode faces.
	 smooth=none,				%Smooth-shaded faces.
	 edges=none,				%Edges and wire-frame.
	 vs=none,				%Unselected vertices.
	 hard=none,				%Hard edges.
	 sel=none,				%Selected items.
	 orig_sel=none,				%Original selection.
	 normals=none,				%Normals.
	 pick=none,				%For picking.
	 proxy_faces=none,			%Smooth proxy faces.
	 proxy_edges=none,			%Smooth proxy edges.

	 %% Miscellanous.
	 hilite=none,				%Hilite display list.
	 mirror=none,				%Virtual mirror data.
	 ns=none,				%Normals/positions per face.

	 %% Source for display lists.
	 src_we=none,				%Source object.
	 src_sel=none,				%Source selection.
	 orig_mode=none,			%Original selection mode.
	 split=none,				%Split data.
	 drag=none,				%For dragging.
	 transparent=false,			%Object includes transparancy.
	 proxy_data=none,			%Data for smooth proxy.
	 open=false,				%Open (has hole).

	 %% List of display lists known to be needed only based
	 %% on display modes, not whether the lists themselves exist.
	 %% Example: [work,edges]
	 needed=[]
	}).

%% Main state record containing all objects and other important state.
-record(st,
	{shapes,				%All visible shapes
	 selmode,				%Selection mode:
						% vertex, edge, face, body
	 sh=false,				%Smart highlight active: true|false
	 sel=[],				%Current sel: [{Id,GbSet}]
	 ssels=[],				%Saved selections:
						%  [{Name,Mode,GbSet}]
	 temp_sel=none,			        %Selection only temporary?

	 mat,					%Defined materials (GbTree).
	 pal=[],                                %Palette
	 file,					%Current filename.
	 saved,					%True if model has been saved.
	 onext,					%Next object id to use.
	 bb=none,				%Saved bounding box.
	 edge_loop=none,			%Previous edge loop.
	 views={0,{}},				%{Current,TupleOfViews}
	 pst=gb_trees:empty(),                  %Plugin State Info
						%   gb_tree where key is plugin	module

	 %% Previous commands.
	 repeatable,			        %Last repeatable command.
	 ask_args,				%Ask arguments.
	 drag_args,			        %Drag arguments for command.
	 def,					%Default operations.

	 %% Undo information.
	 top,					%Top of stack.
	 bottom,				%Bottom of stack.
	 next_is_undo,				%State of undo/redo toggle.
	 undone				        %States that were undone.
	}).

%% The Winged-Edge data structure.
%% See http://www.cs.mtu.edu/~shene/COURSES/cs3621/NOTES/model/winged-e.html
-record(we,
	{id,					%Shape id.
	 perm=0,				%Permissions:
						% 0 - Everything allowed.
						% 1 - Visible, can't select.
						% [] or {Mode,GbSet} -
						%  Invisible, can't select.
						%  The GbSet contains the
						%  object's selection.
	 name,					%Name.
	 es,					%gb_tree containing edges
	 fs,					%gb_tree containing faces
	 he,					%gb_sets containing hard edges
	 vc,					%Connection info (=incident edge)
						% for vertices.
	 vp,					%Vertex positions.
	 pst=gb_trees:empty(),                  %Plugin State Info,
						%   gb_tree where key is plugin module
	 mat=default,				%Materials.
	 next_id,				%Next free ID for vertices,
						% edges, and faces.
						% (Needed because we never re-use
						%  IDs.)
	 mode,					%'vertex'/'material'/'uv'
	 mirror=none,				%Mirror: none|Face
	 light=none,				%Light data: none|Light
	 has_shape=true				%true|false
	}).

-define(IS_VISIBLE(Perm), (Perm =< 1)).
-define(IS_NOT_VISIBLE(Perm), (Perm > 1)).
-define(IS_SELECTABLE(Perm), (Perm == 0)).
-define(IS_NOT_SELECTABLE(Perm), (Perm =/= 0)).

-define(IS_LIGHT(We), ((We#we.light =/= none) and (not We#we.has_shape))).
-define(IS_ANY_LIGHT(We), (We#we.light =/= none)).
-define(HAS_SHAPE(We), (We#we.has_shape)).
%-define(IS_LIGHT(We), (We#we.light =/= none)).
%-define(IS_NOT_LIGHT(We), (We#we.light =:= none)).

%% Edge in a winged-edge shape.
-record(edge,
	{vs,					%Start vertex for edge
	 ve,					%End vertex for edge
	 a=none,			        %Color or UV coordinate.
	 b=none,			        %Color or UV coordinate.
	 lf,					%Left face
	 rf,					%Right face
	 ltpr,					%Left traversal predecessor
	 ltsu,					%Left traversal successor
	 rtpr,					%Right traversal predecessor
	 rtsu					%Right traversal successor
	}).

%% The current view/camera.
-record(view,
	{origin,
	 distance,				% From origo.
	 azimuth,
	 elevation,
	 pan_x,					%Panning in X direction.
	 pan_y,					%Panning in Y direction.
	 along_axis=none,			%Which axis viewed along.
	 fov,					%Field of view.
	 hither,				%Near clipping plane.
	 yon					%Far clipping plane.
	}).
