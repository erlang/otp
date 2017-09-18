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
%%%
%%% Description : Testing and demo xrc's
%%%               This mimics the xrc demo from wxwidgets.
%%% Created :  4 Dec 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------

-module(xrc).
-export([start/0]).

-include("../../include/wx.hrl").


%% I've put all "classes" in the same file,  but you can see the follow
%% the code in xrcdemo from the samples directory in wxWidgets src package

start() ->
    %% Starts wxwidgets
    WX = wx:new(),
    %% In erlang wx, all image handlers are initilized already.
    %% wxImage::AddHandler(new wxXPMHandler);
    
    %% Initialize all the XRC handlers. Always required (unless you feel like
    %% going through and initializing a handler of each control type you will
    %% be using (ie initialize the spinctrl handler, initialize the textctrl
    %% handler). However, if you are only using a few control types, it will
    %% save some space to only initialize the ones you will be using. See
    %% wxXRC docs for details.

    Xrc = wxXmlResource:get(),
    wxXmlResource:initAllHandlers(Xrc),
    true = wxXmlResource:load(Xrc, rc_dir("menu.xrc")),
    true = wxXmlResource:load(Xrc, rc_dir("toolbar.xrc")),
    true = wxXmlResource:load(Xrc, rc_dir("basicdlg.xrc")),
    true = wxXmlResource:load(Xrc, rc_dir("derivdlg.xrc")),
    true = wxXmlResource:load(Xrc, rc_dir("controls.xrc")),
    true = wxXmlResource:load(Xrc, rc_dir("frame.xrc")),
    true = wxXmlResource:load(Xrc, rc_dir("uncenter.xrc")),
    true = wxXmlResource:load(Xrc, rc_dir("custclas.xrc")),
    true = wxXmlResource:load(Xrc, rc_dir("artprov.xrc")),
    true = wxXmlResource:load(Xrc, rc_dir("platform.xrc")),
    true = wxXmlResource:load(Xrc, rc_dir("variable.xrc")),
    Frame = wxFrame:new(),
    myframe(WX,Frame),
    wxFrame:show(Frame),
    loop(Frame),
    wx:destroy().

rc_dir(File) ->
  SelfDir = filename:dirname(code:which(?MODULE)),
  filename:join([SelfDir,rc,File]).

loop(Frame) ->
    receive 
	#wx{id=Id, event=#wxCommand{}} ->
	    handle_cmd(get(Id), Id, Frame),
	    loop(Frame);
	#wx{event=#wxClose{}} ->
	    catch wxWindows:'Destroy'(Frame),
	    ok;
	Ev = #wx{} ->
	    io:format("Got ~p ~n", [Ev]),
	    loop(Frame)
    end.

myframe(Parent, Frame) ->
    Xrc = wxXmlResource:get(),
    wxXmlResource:loadFrame(Xrc, Frame, Parent, "main_frame"),
    %% wxTopLevelWindow:setIcon(Frame, wxXmlResource:loadIcon(Xrc,"appicon")),
    %% Load and setup menubar
    wxFrame:setMenuBar(Frame, wxXmlResource:loadMenuBar(Xrc, "main_menu")),
    %% hmm wxSystemOptions::SetOption ( wxT("msw.remap"), 0 );
    wxFrame:setToolBar(Frame, wxXmlResource:loadToolBar(Xrc, Frame, "main_toolbar")),
    wxFrame:createStatusBar(Frame, [{number,1}]),
    ok = wxFrame:connect(Frame, close_window), 
    connect(Frame).
  
connect(Frame) ->    
    Menues = [unload_resource_menuitem, reload_resource_menuitem,
	      non_derived_dialog_tool_or_menuitem, derived_tool_or_menuitem,
	      controls_tool_or_menuitem, uncentered_tool_or_menuitem,
	      custom_class_tool_or_menuitem, platform_property_tool_or_menuitem,
	      art_provider_tool_or_menuitem, variable_expansion_tool_or_menuitem
	     ],
    wxFrame:connect(Frame,command_menu_selected, [{id, ?wxID_EXIT}]),
    wxFrame:connect(Frame,command_menu_selected, [{id, ?wxID_ABOUT}]),
    [connect_xrcid(Str,Frame) || Str <- Menues],
    ok.

connect_xrcid(Name,Frame) ->
    ID = wxXmlResource:getXRCID(atom_to_list(Name)),
    put(ID, Name),
    wxFrame:connect(Frame,command_menu_selected,[{id,ID}]).

%% Handle commands
    
handle_cmd(unload_resource_menuitem, _, _Frame) ->
    Xrc = wxXmlResource:get(),
    case wxXmlResource:unload(Xrc, "rc/basicdlg") of
	true -> 
	    io:format("Basic dialog unloaded~n",[]);
	false ->
	    io:format("Failed to unload basic dialog~n",[])
    end;

handle_cmd(reload_resource_menuitem, _, _Frame) ->
    Xrc = wxXmlResource:get(),
    case wxXmlResource:reload(Xrc, "rc/basicdlg") of
	true -> 
	    io:format("Basic dialog reloaded~n",[]);
	false ->
	    io:format("Failed to reload basic dialog~n",[])
    end;

handle_cmd(_, ?wxID_EXIT, Frame) ->
    wxFrame:close(Frame);

handle_cmd(non_derived_dialog_tool_or_menuitem, _, Frame) ->
    Xrc = wxXmlResource:get(),
    Dlg = wxDialog:new(),
    %% "non_derived_dialog" is the name of the wxDialog XRC node that should
    %% be loaded.
    case wxXmlResource:loadDialog(Xrc, Dlg, Frame, "non_derived_dialog") of
	true ->
	    wxDialog:showModal(Dlg);
	false ->
	    io:format("Failed to load non_derived_dialog~n",[])
    end,    
    %% In Erlang you should delete the dialog afterwards
    wxDialog:destroy(Dlg);

handle_cmd(derived_tool_or_menuitem, _, Frame) ->
    Pref = prefDialog(Frame),
    wxDialog:showModal(Pref);

handle_cmd(animation_ctrl_play, _, _Frame) ->
    %% Not yet implemented
    ok;

handle_cmd(controls_tool_or_menuitem,_,Frame) ->
    Xrc = wxXmlResource:get(),
    Dlg = wxDialog:new(),
    true = wxXmlResource:loadDialog(Xrc, Dlg, Frame, "controls_dialog"),
    
    LCtrl = wxXmlResource:xrcctrl(Dlg, "controls_listctrl", wxListCtrl),
    wxListCtrl:insertColumn(LCtrl, 0, "Name", [{width, 200}]), 
    wxListCtrl:insertItem(LCtrl, 0, "Todd Hope"),
    wxListCtrl:insertItem(LCtrl, 1, "Kim Wynd"),
    wxListCtrl:insertItem(LCtrl, 2, "Leon Li"),

    TCtrl = wxXmlResource:xrcctrl(Dlg, "controls_treectrl", wxTreeCtrl),
    wxTreeCtrl:addRoot(TCtrl, "Godfather"),
    TRoot = wxTreeCtrl:getRootItem(TCtrl),
    wxTreeCtrl:appendItem(TCtrl,TRoot, "Evil henchmen 1"), 
    wxTreeCtrl:appendItem(TCtrl,TRoot, "Evil henchmen 2"), 
    wxTreeCtrl:appendItem(TCtrl,TRoot, "Evil accountant"), 

    wxDialog:showModal(Dlg),
    wxDialog:destroy(Dlg);

handle_cmd(uncentered_tool_or_menuitem,_,Frame) ->
    Xrc = wxXmlResource:get(),
    Dlg = wxDialog:new(),
    true = wxXmlResource:loadDialog(Xrc, Dlg, Frame, "uncentered_dialog"),
    wxDialog:showModal(Dlg),
    wxDialog:destroy(Dlg);

handle_cmd(custom_class_tool_or_menuitem,_,Frame) ->
    Xrc = wxXmlResource:get(),
    Dlg = wxDialog:new(),
    true = wxXmlResource:loadDialog(Xrc, Dlg, Frame, "custom_class_dialog"),

    ResizeableLC = myResizeableListCtrl(Dlg, ?wxID_ANY, {-1,-1}, {-1,-1},?wxLC_REPORT),
    %% "custom_control_placeholder" is the name of the "unknown" tag in the
    %% custctrl.xrc XRC file.
    wxXmlResource:attachUnknownControl(Xrc, "custom_control_placeholder", ResizeableLC),
    wxDialog:showModal(Dlg),
    wxDialog:destroy(Dlg);

handle_cmd(platform_property_tool_or_menuitem, _, Frame) ->
    Xrc = wxXmlResource:get(),
    Dlg = wxDialog:new(),
    true = wxXmlResource:loadDialog(Xrc, Dlg, Frame, "platform_property_dialog"),
    wxDialog:showModal(Dlg),
    wxDialog:destroy(Dlg);

handle_cmd(art_provider_tool_or_menuitem, _, Frame) ->
    Xrc = wxXmlResource:get(),
    Dlg = wxDialog:new(),
    true = wxXmlResource:loadDialog(Xrc, Dlg, Frame, "art_provider_dialog"),
    wxDialog:showModal(Dlg),
    wxDialog:destroy(Dlg);

handle_cmd(variable_expansion_tool_or_menuitem, _, Frame) ->
    Xrc = wxXmlResource:get(),
    Dlg = wxDialog:new(),
    true = wxXmlResource:loadDialog(Xrc, Dlg, Frame, "variable_expansion_dialog"),
    wxDialog:showModal(Dlg),
    wxDialog:destroy(Dlg);
handle_cmd(_, ?wxID_ABOUT, Frame) ->
    Msg = "This is the about dialog of XML resources demo.\n",
    MD = wxMessageDialog:new(Frame,Msg,
			     [{style, ?wxOK bor ?wxICON_INFORMATION}, 
			      {caption, "About"}]),
    wxDialog:showModal(MD),
    wxDialog:destroy(MD);
handle_cmd(Dialog, Id, _) ->
    io:format("Not implemented yet ~p (~p) ~n",[Dialog, Id]).
   

%%%%%%%%%%%%%%%%
%% Trying to mimic the derived dialog example
%%%%%%%%%%%%%%%%
 
prefDialog(Parent) ->    
    Xrc = wxXmlResource:get(),
    Dlg = wxDialog:new(),
    true = wxXmlResource:loadDialog(Xrc, Dlg, Parent, "derived_dialog"),

    %% Shows that callbacks can be used it doesn't need to though.
    OnMyButtonClicked = fun(_EvRec, _wxEvent) ->
				MD = wxMessageDialog:new(Dlg, "You clicked on My Button"),
				wxMessageDialog:showModal(MD),
				wxMessageDialog:destroy(MD)
			end,
    wxDialog:connect(Dlg, command_button_clicked, 
		     [{id,wxXmlResource:getXRCID("my_button")},
		      {callback,OnMyButtonClicked}]),

    OnMyCheckBox = fun(_EvRec, _Event) ->
			   CheckB = wxXmlResource:xrcctrl(Dlg, "my_checkbox", wxCheckBox),
			   Text = wxXmlResource:xrcctrl(Dlg, "my_textctrl", wxTextCtrl),
			   Bool = wxCheckBox:isChecked(CheckB),
			   wxTextCtrl:enable(Text, [{enable,Bool}])
		   end,
    wxDialog:connect(Dlg,update_ui,[{id,wxXmlResource:getXRCID("my_checkbox")},
				    {callback,OnMyCheckBox}]),

    %% Keep updateUI event interval at 250ms
    wxUpdateUIEvent:setUpdateInterval(250),
    
    OnOk = fun(_,_) ->
		   Str = 
		       "Press Ok to close derived dialog, or Cancel to abort"
		       "Overriding base class ok button handler",
		   MD = wxMessageDialog:new(Dlg, Str, [{style, ?wxOK bor ?wxCANCEL bor ?wxCENTER}]),
		   case wxMessageDialog:showModal(MD) of
		       ?wxID_OK -> 
			   wxMessageDialog:endModal(Dlg, ?wxID_OK);
		       _R ->
			   ignore
		   end,
		   wxMessageDialog:destroy(MD)
	   end,
    wxDialog:connect(Dlg,command_button_clicked,[{id,?wxID_OK},{callback,OnOk}]),
    Dlg.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(RECORD_COLUMN,  0).
-define(ACTION_COLUMN,  1).
-define(PRIORITY_COLUMN,2).

-define(PU_ADD_RECORD,     ?wxID_HIGHEST+1).
-define(PU_EDIT_RECORD,    ?wxID_HIGHEST+2).
-define(PU_DELETE_RECORD,  ?wxID_HIGHEST+3).

myResizeableListCtrl(Parent,Id,Pos,Size,Style) -> 
    LC = wxListCtrl:new(Parent, [{winid,Id}, {pos,Pos}, {size,Size}, {style,Style}]),
    wxListCtrl:insertColumn(LC,?RECORD_COLUMN, "Record", [{width, 140}]),
    wxListCtrl:insertColumn(LC,?ACTION_COLUMN, "Action", [{width, 70}]),
    wxListCtrl:insertColumn(LC,?PRIORITY_COLUMN, "Priority", [{width, 70}]),
    wxListCtrl:connect(LC, right_down, [{id,Id}, {callback, fun lc_onRightClick/2}]),
    wxListCtrl:connect(LC, size, [{id,Id}, {callback, fun lc_onSize/2}]),
    LC.
    
lc_onRightClick(#wx{obj=ListCtrl, event=#wxMouse{x=X,y=Y}},_Ev) ->
    Menu = wxMenu:new(),
    wxMenu:append(Menu, ?PU_ADD_RECORD, "Add a new record"),
    wxMenu:append(Menu, ?PU_EDIT_RECORD,"Edit selected record"),
    wxMenu:append(Menu, ?PU_DELETE_RECORD, "Delete selected record"),
    case wxListCtrl:getSelectedItemCount(ListCtrl) of
	0 ->
	    wxMenu:enable(Menu, ?PU_EDIT_RECORD, false),
	    wxMenu:enable(Menu, ?PU_DELETE_RECORD, false);
	_ ->
	    ignore
    end,
    MenuCB = fun(_,_) -> io:format("Menu selected~n",[]) end,
    wxWindow:connect(ListCtrl, command_menu_selected, [{callback, MenuCB}]),
    wxWindow:popupMenu(ListCtrl, Menu, [{pos, {X,Y}}]),
    wxMenu:destroy(Menu).
    
lc_onSize(#wx{obj=ListCtrl},EvObj) ->
    {LeftMostColW0,_} = wxListCtrl:getSize(ListCtrl),
    LeftMostColW1 = LeftMostColW0 - wxListCtrl:getColumnWidth(ListCtrl, ?ACTION_COLUMN),
    LeftMostColW2 = LeftMostColW1 - wxListCtrl:getColumnWidth(ListCtrl, ?PRIORITY_COLUMN),
    %% Hmm missing wxSystemSettings::GetMetric( wxSYS_VSCROLL_X );
    LeftMostColW = LeftMostColW2 - 5,  
    wxListCtrl:setColumnWidth(ListCtrl, ?RECORD_COLUMN, LeftMostColW),
    %% REQURED event.Skip() call to allow this event to propagate
    %% upwards so others can do what they need to do in response to
    %% this size event.
    wxEvent:skip(EvObj),
    io:format("Successfully set column width~n").

%%%%%
