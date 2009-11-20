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
%% ------------------------------------------------------------
%% Purpose:  window management and the gs interface
%% ------------------------------------------------------------

-module(pman_win).

%% ---------------------------------------------------------------
%% The user interface exports 
%% ---------------------------------------------------------------

-export([pman_window/3, window/1, module_data/1, display/1, format/2,
	 dialog_window/2, configeditor/2, configwin/3,
	 update/1, update/3,
	 msg_win/1, title/1,
	 remove_menu/1, add_menu/3,
	 change_colour/3, links_menus/1, calc_columnwidths/1]).
-export([font/0, font/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Constants
%%
-include("pman_win.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pman_window/3 - Create a GS window and components for the
%%   Pman overview window, the main window.
%%
%% Arguments:
%%   Size		number of processes
%%   HiddenModules	list of modules 
%%   Nodes		list of supervised nodes
%%
%% Return: 
%%   {Win, Grid, Frame, Procnum, W, H} where
%%   Win	The GS top window
%%   Grid	The GS grid
%%   Procnum	Number of displayed processes
%%

pman_window(Size, _HiddenModules, Nodes) ->
    GS = gs:start([{kernel,true}]),
    Font = font(GS),
    Win_Options = [{title, lists:concat(["Pman: Overview on ",node()])},
		   {width, ?WIN_WIDTH}, {height, ?WIN_HEIGHT},
		   {destroy, true},
		   {keypress,true}],
    Win = gs:create(window, GS, Win_Options),

    %% Menu bar
    MenuBar = gs:create(menubar, Win, []),
    MBFile = gs:create(menubutton, MenuBar, [{label,{text," File "}},
					     {font,Font},
					     {underline,1}]),
    MBView = gs:create(menubutton, MenuBar, [{label,{text, " View "}},
					     {font,Font},
					     {underline,1}]),
    MBTrace = gs:create(menubutton, MenuBar, [{label,{text, " Trace "}},
					      {font,Font},
					      {underline,1}]),
    MBHelp = gs:create(menubutton, MenuBar, [{label, {text, " Help "}},
					     {font,Font},
					     {side,right},
					     {underline,1}]),

    %% Addition of a menu for distribution
    add_node_menu(MenuBar, Nodes, Font),

    %% All menu buttons
    MenuFile = gs:create(menu, MBFile, []),
    MenuView = gs:create(menu, MBView, []),
    MenuTrace = gs:create(menu, MBTrace, []),
    MenuHelp = gs:create(menu, MBHelp, []),

    %% File menu
    gse:named_menuitem('Default Options', MenuFile,
		       [{label,{text,"Options..."}}, {font,Font},
			{underline,0}]),
    gse:named_menuitem('Save Options',MenuFile,
		       [{label,{text,"Save Options"}}, {font,Font}]),
    gse:named_menuitem('Exit', MenuFile,
		       [{label,{text,"Exit"}}, {font,Font},
			{underline,0}]),

    %% View menu
    gse:named_menuitem('Hide All',MenuView,
		       [{label, {text, "Hide All Processes"}},
			{font,Font},
			{underline,1}]),

    gse:named_menuitem('Hide Modules', MenuView,
		       [{label, {text, "Hide Modules..."}},
			{font,Font},
			{underline,8}]),

    gse:named_menuitem('Hide Selected Process', MenuView,
		       [{label, {text, "Hide Selected Process"}},
			{font,Font},
			{underline,2}]),

    gse:named_menuitem('Module',MenuView,
		       [{label, {text, "Module Info..."}}, {font,Font},
			{underline,7}]),

    gse:named_menuitem('Refresh', MenuView,
		       [{label, {text, "Refresh"}}, {font,Font},
			{underline,0}]),

    gse:named_menuitem('Show All',MenuView,
		       [{label, {text, "Show All Processes"}},
			{font,Font}]),

    gse:named_menuitem('Show Selected',MenuView,
		       [{label, {text, "Show Processes..."}},
			{font,Font}]),

    %% Trace menu
    gs:create(menuitem, 'Kill', MenuTrace, [{label,{text, "Kill"}},
					    {font,Font},
					    {underline,0}]),

    gs:create(menuitem, 'Trace Process', MenuTrace,
	      [{label, {text, "Trace Selected Process"}}, {font,Font},
	       {underline,0}]),

    gs:create(menuitem,'Trace Shell', MenuTrace,
	      [{label, {text,"Shell Process"}}, {font,Font},
	       {underline,0}]),

    %% Help menu
    gs:create(menuitem,'Help', MenuHelp,  [{label, {text, "Help" }},
					   {font,Font},
					   {underline,0}]),

    %% Window contents

    %% Geometry managing frame
    Frame = gse:frame(Win, [{y,?MENU_HEIGHT},
			    {packer_x,[{stretch, 1}]},
			    {packer_y,[{stretch,10},
				       {fixed,?CHECKBAR_HEIGHT}]}]),



    %% Grid 
    Grid_Options = [
		    {pack_x,1}, {pack_y,1},
		    {fg,black}, 
		    {vscroll,right},{hscroll,bottom},
		    calc_columnwidths(739),
		    {rows, {1,Size}}], 
    Grid = gse:grid(Frame,Grid_Options),


    %% Checkbutton bar at the bottom of the window

    CheckBar = gse:frame(Frame, [{pack_x,1},
				 {pack_y,2},
				 {packer_x,[{stretch, 2, 100,300},
					    {stretch, 2, 100,300},
					    {stretch,1},
					    {stretch, 2,100,300}]},
				 {packer_y,[{stretch,1}]}]),
    gse:named_checkbutton('Hide System',CheckBar,
			  [{pack_xy,{1,1}},
			   {justify, left},
			   {align,w},
			   {width, 200},
			   {font, Font},
			   {label, {text, "Hide System Processes" }}]),
    
    gse:named_checkbutton('Auto Hide New',CheckBar,
			  [{pack_xy,{2,1}},
			   {width, 200},
			   {justify, left},
			   {align,w},
			   {font, Font},
			   {label, {text, "Auto-Hide New" }}]),
    
    gse:named_label('Number Hidden',CheckBar,
			  [{pack_xy,{4,1}},
			   {justify, left},
			   {align,w},
			   {width, 200},
			   {font, Font},
			   {label, {text, ?CPIDHIDDENTEXT }}]),

    %% Finalize it!
    gse:map(Win),
    gse:config(Win,[raise]),
    gse:config(Win,[{configure,true}]),


    {Win, Grid, Frame, length(processes())+1, ?WIN_WIDTH, ?WIN_HEIGHT}.


%% Calculate columnwidths in respect to the size of the window.

calc_columnwidths(Width) ->
    if
	Width =< 739 -> 
	    {columnwidths,[75,215,146,90,105,105]};
       true -> 
	    S = (Width - 75)/(215+146+90+105+105),
	    {columnwidths,[75,round(215*S),round(146*S),round(90*S),
			   round(105*S),round(105*S)]}
    end.

%% ---------------------------------------------------------------
%% Create a trace window
%%
%% Process, a process id or an atom
%%
%% Return: A window and a editor
%% ---------------------------------------------------------------


window(Process) ->
    GS = gs:start([{kernel,true}]),
    Font = font(GS),
    Win_Options = [{title,title(Process)}, {width,550}, {keypress,true}, 
		   {configure,true},
		   {destroy,true},{height, 400}],
    Win = gs:create(window,GS,Win_Options),
    
    MenuBar = gs:create(menubar, Win, []),

    %% File menu
    MBFile =  gs:create(menubutton,MenuBar,[{label,{text," File "}},
					    {font,Font},
					    {underline, 1}]),
    MenuFile = gs:create(menu, MBFile, []),
    make_menus(pman_process:is_running(Process), MenuBar, MenuFile,
	       Font),

    gse:named_menuitem('Save buffer',MenuFile,
		       [{label,{text, "Save buffer..."}},
			{font,Font},
			{underline,0}]),
    gse:named_menuitem('Close',MenuFile,
		       [{label, {text, "Close"}},
			{font,Font},
			{underline,0}]),


    Editor = gs:create(editor,Win,[{x,3}, {y,40},
				   {width,546}, {height,348},
				   {font,Font}]),
    gs:config(Editor, [{keypress, true},{insert, {'end', display(Process)}}]),
    gs:config(Editor, [{enable, false},{vscroll, right}, {hscroll, bottom},
		       {wrap,none}]),
    gs:config(Win, [{map, true}]),
    {Win, Editor}.

%% ---------------------------------------------------------------------
%% Menu Help Fuctions
%% ---------------------------------------------------------------------


links_menus(Links) ->
    gs:destroy('Links'),
    gs:create(menu,'Links','LinksMenu',[]),
    Flag =  case links_menus(Links,[]) of
		[] -> false;
		Pids ->
		    add_menu('Links', Pids, "Trace"),
		    true
	    end,
    gse:config('LinksMenu',[{enable,Flag}]).
	    
links_menus([],Pids) -> Pids;
links_menus([Pid|Links],Pids) when is_pid(Pid) ->
    links_menus(Links,[Pid|Pids]);
links_menus([_Port|Links],Pids) ->
    links_menus(Links,Pids).


%% Create the node menu. 

add_node_menu(MenuBar, Nodes, Font) ->
    MBNode = gs:create(menubutton, MenuBar, [{label,{text, " Nodes "}},
					     {font,Font},
					     {underline, 1}]),
    gs:create(menu, node, MBNode, []),
    add_menu(node, Nodes, "Show", Font),
    gse:disable(node()).


%% ---------------------------------------------------------------------
%% Add Menus in the list under Menu menuitem.

add_menu(Menu, Names, Tag) ->
    add_menu(Menu, Names, Tag, font()).

add_menu(_Menu, [], _Tag, _Font) -> ok;
add_menu(Menu, [Name|Names], Tag, Font) ->
    Title = io_lib:format("~s ~p",[Tag, Name]),
    gs:create(menuitem,Name,Menu,[{label,{text,Title}},
				  {font,Font},
				  {data,{Menu,Name}}]),
    add_menu(Menu, Names, Tag, Font).

%% ---------------------------------------------------------------------
%% Remove a specific menu item, or a whole menu, or a list of both.
%%

remove_menu(List) when is_list(List)->
    lists:foreach(fun(X) -> gs:destroy(X) end, List);

remove_menu(Object) ->
    gse:destroy(Object).


%% ---------------------------------------------------------------------
%% If the trace window opened is supposed to trace a real pid, let us
%% add the trace menu, and other items specific to tracing. If not,
%% the only menus available are the ones in the default defined in
%% window(Pid).

make_menus(false, _, _, _) -> ok;
make_menus({true,Pid}, MenuBar, MenuFile, Font) ->
    MBView = gs:create(menubutton,'ViewMenu',MenuBar,
				[{underline,1},
				 {label,{text," View "}}, {font,Font},
				 {side,left}]),
    MenuView = gs:create(menu, MBView, []),

    MBTrace = gs:create(menubutton,'TraceMenu',MenuBar,
			      [{underline,1},
			       {label,{text," Trace "}}, {font,Font},
			       {side,left}]),
    MenuTrace = gs:create(menu, MBTrace, []),


    MBHelp =  gs:create(menubutton,'HelpMenu',MenuBar,
			      [{underline,1},
			       {label,{text," Help "}}, {font,Font},
			       {side,right}]),
    MenuHelp = gs:create(menu, MBHelp, []),

    %% File menu
    gse:named_menuitem('Options', MenuFile,
		       [{label, {text, "Options..."}}, {font,Font},
			{underline,0}]),

    %% Trace menu
    gse:named_menuitem('All Links', MenuTrace,
		       [{label, {text, "All Linked Processes"}},
			{font,Font},
			{underline,0}]),
    gse:named_menuitem('LinksMenu', MenuTrace,
		       [{underline,0},
			{label, {text, "Linked Process..."}},
			{font,Font},
			{itemtype, cascade},
			{enable,false}]),
    gs:create(menu,'Links','LinksMenu',[]),
    case pman_process:pinfo(Pid, links) of
	Links when is_list(Links) ->
	    links_menus(Links);
	undefined ->
	    lists:foreach(fun(X) -> gse:disable(X) end,['LinksMenu'])
    end,
    gse:named_menuitem('Kill', MenuTrace,
		       [{label, {text, "Kill"}}, {font,Font},
			{underline,0}]),

    %% View menu
    gse:named_menuitem('Clear', MenuView,
		       [{label, {text, "Clear buffer"}}, {font,Font},
			{underline,0}]),

    gse:named_menuitem('Module', MenuView,
		       [{label, {text, "Module Info"}}, {font,Font},
			{underline,0}]),

    %% Help menu
    gse:named_menuitem('Help', MenuHelp,
		       [{label, {text, "Help"}}, {font,Font},
			{underline,0}]).

%% ---------------------------------------------------------------------
%% Configurate the actual editor
%%
%% Editor, actual editor
%% Options, actual options for the editor
%%
%% Return: A configurated editor with the actual options
%% ---------------------------------------------------------------------

configeditor(Editor, Options) ->
    gs:config(Editor, Options).

%% ---------------------------------------------------------------------
%% Configure the actual window after it has been resized.
%% ---------------------------------------------------------------------

configwin(Object, W, H) ->
    Dx = abs(W - gs:read(Object,width) - 4),
    Dy = abs(H - gs:read(Object,height) - 42),
    if
	Dx + Dy =/= 0 ->
	    gs:config(Object,[{width,W - 4}]);
	true -> ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% update/1, 3
update(NoOfHidden) ->
    Str = lists:flatten(io_lib:format(?CPIDHIDDENTEXT++"~w",
				      [NoOfHidden])),
    gse:config('Number Hidden', [{label, {text,Str}}]).
    
update(Grid, ShowInfoR, NoOfHidden) ->

    %% We reverse the list because we want the processes to appear with
    %% the newest (=highest) pid first in the list.
    ShowInfo = lists:reverse(ShowInfoR),

    %% Set the length of the grid
    CGridline = length(ShowInfo) + 1,
    gs:config(Grid, [{rows, {1,CGridline}}]),

    %% Add the header line 
    add_gridline(Grid,
		 1,
		 {'Pid','Current Function','Name','Msgs','Reds','Size'},
		 []),

    update(NoOfHidden),

    %% Recurse through the ordset of pids
    update_r(Grid, ShowInfo, 2).

update_r(Grid, [], Row) ->
    delete_gridlines(Grid, Row);
update_r(Grid, [{Pid,Func,Name,Msgs,Reds,Psize}|ShowInfo], Row)  -> 
    {M, F, A} = Func,
    FuncText = lists:flatten(io_lib:format("~w:~w/~w", [M, F, A])),
    add_gridline(Grid,
		 Row, 
		 {Pid, FuncText, Name, Msgs, Reds, Psize},
		 [{data,{pidfunc,Pid,Func}}]),
    update_r(Grid, ShowInfo, Row+1).

add_gridline(Grid, Row, Tuple, LIOptSpec) ->
    {Pid, FuncText, Name, Msgs, Reds, Psize} = Tuple,
    LIOpt = [{click,true},
	     {doubleclick,true},
	     {fg, colour(Row)},
	     {text,{1,Pid}},
	     {text,{2,FuncText}},
	     {text,{3,Name}},
	     {text,{4,Msgs}},
	     {text,{5,Reds}},
	     {text,{6,Psize}} |LIOptSpec],
    case gs:read(Grid, {obj_at_row, Row}) of
	undefined ->
	    gse:gridline(Grid,[{row, Row}|LIOpt]);
	GridLine ->
	    gs:config(GridLine,LIOpt)
    end.
    
delete_gridlines(Grid, Row) -> 
    case gs:read(Grid, {obj_at_row, Row}) of
	undefined ->
	    ok;
	GridLine ->
	    gs:destroy(GridLine),
	    delete_gridlines(Grid, Row+1)
    end.

colour(1) ->
    ?HEADER_COLOUR;
colour(_Row) ->
    ?UNSELECTED_COLOUR.

%% Interchange colours between two rows
change_colour(Grid, Row, Row) ->
    Gitem = gs:read(Grid, {obj_at_row,Row}),
    gs:config(Gitem, {fg,?SELECTED_COLOUR});
change_colour(Grid, From, To) ->
    Gitem_to = gs:read(Grid, {obj_at_row,To}),
    Gitem_fr = gs:read(Grid, {obj_at_row,From}),
    gs:config(Gitem_to, {fg,?SELECTED_COLOUR}),
    gs:config(Gitem_fr, {fg,colour(From)}).
    
%% --------------------------------------------------------------
%% Create a title for the window
%% Return: the title
%% --------------------------------------------------------------

title({module, Mod}) ->
    lists:flatten([io_lib:format("Pman: Module info ~p", [Mod])]);

title({shell,  Sh} ) ->
    lists:flatten([io_lib:format("Pman: Shell process ~p on ~p",
				 [Sh,node(Sh)])]);

title(Sh) ->
    lists:flatten([io_lib:format("Pman: Process ~p on ~p",
				 [Sh, node(Sh)]),name(Sh)]).
name(Pid) ->
    case pman_process:pinfo(Pid, registered_name) of
	undefined -> "";
	Name ->
	    lists:flatten([io_lib:format("[~p]", [Name])])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module_data/1 - %% Returns the module information for a
%%   module, on a format suitable to insert into a GS editor.
%%
%% Arguments:
%%   ModuleName		The module
%%
%% Returns:
%%   A string with module information.
%%

module_data(ModuleName) ->
    vformat("", catch apply({ModuleName, module_info},[])).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% display/1 - 
%%

display({_,Pid,_}) -> display(Pid);
display({_,Pid}) -> display(Pid);
display(Pid) when is_pid(Pid) ->
    case pman_process:pinfo(Pid) of
	undefined ->
	    format('Process is dead~n',[]);
	Other ->
	    proc_format(Other)
    end.

%% --------------------------------------------------------------
%% Format functions for the shell and help window.
%% --------------------------------------------------------------

vformat(Pad, {M,F,A}) when is_atom(F) -> 
    Pad2 = lists:append(Pad,mkpad(io_lib:format("~w:~w",[M,F]))),
    lists:flatten([format("~p:~p", [M,F]),argformat(Pad2, A)]);

vformat(Pad, [H|T]) ->
    kvformat(Pad, [H|T],"[");

vformat(_Pad, X) -> format("~p~n", [X]).

format(Format) -> format(Format, []).

format(Format, Args) ->
   io_lib:format(Format, Args).
   

kvformat(S, [Item],Buff) ->
    lists:reverse([format("\n~s~p]\n",[S,Item])|Buff]);

kvformat(S,[H|T],Buff) ->
    kvformat(S,T,[format("\n~s~p, ",[S,H])|Buff]);

kvformat(_,[],Buff) -> 
    lists:reverse(["]\n"|Buff]).

argformat(_Pad,A) when is_integer(A) ->
    format("/~p\n", [A]);
argformat(_,A) ->
    lists:flatten([format("/~p\n", [length(A)]),
		   format("args: \n"),
		   argformat2("      ", A)]).

argformat2(Pad, Arglist) ->
    Chars = lists:flatten(io_lib:format("~p",[Arglist])),
    if
	length(Chars) < (70 - length(Pad)) ->
	    format("~s~s\n", [Pad, Chars]);
	true ->
	    argformat3(Pad, Arglist)
    end.

argformat3(_,[]) -> format("\n");
argformat3(Pad, [H|T]) ->
    Chars = truncate(65,io_lib:format("~s~p",[Pad, H])),
    format("~s,\n", [Chars]),
     argformat3(Pad, T).

pformat(false) -> [];
pformat({value,{_, 0}}) -> [];
pformat({value,{_, []}}) -> [];
pformat({value, {Key, Vals}}) ->
    Pad = mkpad(io_lib:format("~p ",[Key])),
    format(lists:flatten(["~p: " ,vformat(Pad, Vals), "~n"]), [Key]).
   
truncate(0, _Chars) -> ".....";
truncate(I, [H|T]) -> [H|truncate(I-1, T)];
truncate(_I, []) -> [].

mkpad([_|T]) -> [32|mkpad(T)];
mkpad([]) -> [].

proc_format(Pi) ->  %% process_info struct
    X1 = pformat(lists:keysearch(initial_call, 1, Pi)),
    X2 = pformat(lists:keysearch(current_function, 1,Pi)),
    X3 = pformat(lists:keysearch(messages, 1,Pi)),
    X4 = pformat(lists:keysearch(dictionary,1, Pi)),
    X5 = pformat(lists:keysearch(heap_size, 1,Pi)),
    X6 = pformat(lists:keysearch(stack_size, 1,Pi)),
    X7 = pformat(lists:keysearch(reductions, 1,Pi)),
    X8 = pformat(lists:keysearch(links, 1,Pi)),
    X9 = pformat(lists:keysearch(trap_exit, 1,Pi)),
    lists:flatten([X1, X2, X3, X4, X5,X6,X7,X8,X9]).


%% Using the tool_utils function for presenting messages.
dialog_window(GSParent, Text) ->
    spawn_link(tool_utils, notify, [GSParent, Text]).

%% Create a window with a dismiss button.
msg_win(Text) ->
    spawn_link(fun() -> display_msg_win(Text) end).

display_msg_win(Text) ->
    GS = gs:start([{kernel,true}]),
    Font = font(GS),
    Win = gs:window(GS, [{width,200}, {height,75}, {destroy,true},
			 {title,"Pman Message"}]),
    Can = gs:canvas(Win, [{width,200}, {height, 75},{x,0},{y,0}]),
    gs:text(Can, [{text,Text}, {coords,[{10,0}]}, {justify,center}]),
    Btn = gs:button(Win, [{label,{text,"Dismiss"}}, {font,Font},
			  {width,100}, {x,50}, {y,40}]),
    gs:config(Win, {map,true}),
    receive
	{gs, Btn, click, _, _} ->
	    ok
    end.

%% Choose default font
font() ->
    font(gs:start([{kernel,true}])).

font(GS) ->
    case gs:read(GS, {choose_font, {screen,[],12}}) of
	Font when element(1, Font)==screen ->
	    Font;
	_ ->
	    gs:read(GS, {choose_font, {courier,[],12}})
    end.
