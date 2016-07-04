%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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

-module(observer_traceoptions_wx).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-export([process_trace/2, port_trace/2, trace_pattern/4, select_nodes/2,
	 output/2, select_matchspec/4]).

process_trace(Parent, Default) ->
    Dialog = wxDialog:new(Parent, ?wxID_ANY, "Process Options",
			  [{style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER}]),
    Panel = wxPanel:new(Dialog),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    PanelSz = wxBoxSizer:new(?wxHORIZONTAL),
    LeftSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel,  [{label, "Tracing options"}]),
    RightSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Inheritance options:"}]),

    FuncBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace function call", []),
    check_box(FuncBox, lists:member(functions, Default)),
    ArityBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace arity instead of arguments", []),
    check_box(ArityBox, lists:member(functions, Default)),
    SendBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace send message", []),
    check_box(SendBox, lists:member(send, Default)),
    RecBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace receive message", []),
    check_box(RecBox, lists:member('receive', Default)),
    EventBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace process events", []),
    check_box(EventBox, lists:member(events, Default)),
    SchedBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace scheduling of processes", []),
    check_box(SchedBox, lists:member(running_procs, Default)),
    ExitBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace scheduling of exiting processes", []),
    check_box(ExitBox, lists:member(exiting, Default)),
    GCBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace garbage collections", []),
    check_box(GCBox, lists:member(garbage_collection, Default)),

    {SpawnBox, SpwnAllRadio, SpwnFirstRadio} =
	optionpage_top_right(Panel, RightSz, [{flag, ?wxBOTTOM},{border, 5}], "spawn"),
    {LinkBox, LinkAllRadio, LinkFirstRadio} =
	optionpage_top_right(Panel, RightSz, [{flag, ?wxBOTTOM},{border, 5}], "link"),
    SpawnBool = lists:member(on_spawn, Default) orelse lists:member(on_first_spawn, Default),
    LinkBool = lists:member(on_link, Default) orelse lists:member(on_first_link, Default),
    check_box(SpawnBox, SpawnBool),
    check_box(LinkBox,  LinkBool),
    enable(SpawnBox, [SpwnAllRadio, SpwnFirstRadio]),
    enable(LinkBox, [LinkAllRadio, LinkFirstRadio]),
    [wxRadioButton:setValue(Radio, lists:member(Opt, Default)) ||
	{Radio, Opt} <- [{SpwnAllRadio, on_spawn}, {SpwnFirstRadio, on_first_spawn},
			 {LinkAllRadio, on_link},  {LinkFirstRadio, on_first_link}]],

    [wxSizer:add(LeftSz, CheckBox, []) || CheckBox <- [FuncBox,ArityBox,SendBox,RecBox,EventBox,SchedBox,ExitBox,GCBox]],
    wxSizer:add(LeftSz, 150, -1),

    wxSizer:add(PanelSz, LeftSz, [{flag, ?wxEXPAND}, {proportion,1}]),
    wxSizer:add(PanelSz, RightSz,[{flag, ?wxEXPAND}, {proportion,1}]),
    wxPanel:setSizer(Panel, PanelSz),
    wxSizer:add(MainSz, Panel, [{flag, ?wxEXPAND}, {proportion,1}]),
    Buttons = wxDialog:createButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
    wxSizer:add(MainSz, Buttons, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxWindow:setSizerAndFit(Dialog, MainSz),
    wxSizer:setSizeHints(MainSz, Dialog),
    wxCheckBox:connect(SpawnBox, command_checkbox_clicked,
		       [{callback, fun(#wx{event=#wxCommand{}},_) ->
					   enable(SpawnBox, [SpwnAllRadio, SpwnFirstRadio])
				   end}]),
    wxCheckBox:connect(LinkBox, command_checkbox_clicked,
		       [{callback, fun(#wx{event=#wxCommand{}},_) ->
					   enable(LinkBox, [LinkAllRadio, LinkFirstRadio])
				   end}]),

    case wxDialog:showModal(Dialog) of
	?wxID_OK ->
	    All = [{SendBox, send}, {RecBox, 'receive'},
		   {FuncBox, functions}, {ArityBox, arity},
		   {EventBox, events}, {SchedBox, running_procs},
		   {ExitBox, exiting}, {GCBox, garbage_collection},
		   {{SpawnBox, SpwnAllRadio}, on_spawn},
		   {{SpawnBox,SpwnFirstRadio}, on_first_spawn},
		   {{LinkBox, LinkAllRadio}, on_link},
		   {{LinkBox, LinkFirstRadio}, on_first_link}],
	    Check = fun({Box, Radio}) ->
			    wxCheckBox:getValue(Box) andalso wxRadioButton:getValue(Radio);
		       (Box) ->
			    wxCheckBox:getValue(Box)
		    end,
	    Opts = [Id || {Tick, Id} <- All, Check(Tick)],
	    wxDialog:destroy(Dialog),
	    lists:reverse(Opts);
	?wxID_CANCEL ->
	    wxDialog:destroy(Dialog),
	    throw(cancel)
    end.

port_trace(Parent, Default) ->
    Dialog = wxDialog:new(Parent, ?wxID_ANY, "Port Options",
			  [{style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER}]),
    Panel = wxPanel:new(Dialog),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    OptsSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel,  [{label, "Tracing options"}]),

    SendBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace send message", []),
    check_box(SendBox, lists:member(send, Default)),
    RecBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace receive message", []),
    check_box(RecBox, lists:member('receive', Default)),
    EventBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace port events", []),
    check_box(EventBox, lists:member(events, Default)),
    SchedBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace scheduling of ports", []),
    check_box(SchedBox, lists:member(running_ports, Default)),

    [wxSizer:add(OptsSz, CheckBox, []) || CheckBox <- [SendBox,RecBox,EventBox,SchedBox]],
    wxSizer:add(OptsSz, 150, -1),

    wxPanel:setSizer(Panel, OptsSz),
    wxSizer:add(MainSz, Panel, [{flag, ?wxEXPAND}, {proportion,1}]),
    Buttons = wxDialog:createButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
    wxSizer:add(MainSz, Buttons, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxWindow:setSizerAndFit(Dialog, MainSz),
    wxSizer:setSizeHints(MainSz, Dialog),

    case wxDialog:showModal(Dialog) of
	?wxID_OK ->
	    All = [{SendBox, send}, {RecBox, 'receive'},
		   {EventBox, events}, {SchedBox, running_ports}],
	    Opts = [Id || {Tick, Id} <- All, wxCheckBox:getValue(Tick)],
	    wxDialog:destroy(Dialog),
	    lists:reverse(Opts);
	?wxID_CANCEL ->
	    wxDialog:destroy(Dialog),
	    throw(cancel)
    end.

trace_pattern(ParentPid, Parent, Node, MatchSpecs) ->
    try
	{Module,MFAs,MatchSpec} =
	    case module_selector(Parent, Node) of
		{'$trace_event',Event} ->
		    MS = select_matchspec(ParentPid, Parent, MatchSpecs, Event),
		    {'Events',[{'Events',Event}],MS};
		Mod ->
		    MFAs0 = function_selector(Parent, Node, Mod),
		    MS = select_matchspec(ParentPid, Parent, MatchSpecs, funcs),
		    {Mod,MFAs0,MS}
	    end,
	{Module, [#tpattern{m=M,fa=FA,ms=MatchSpec} || {M,FA} <- MFAs]}
    catch cancel -> cancel
    end.

select_nodes(Parent, Nodes) ->
    Choices = [{atom_to_list(X), X} || X <- Nodes],
    check_selector(Parent, Choices).

module_selector(Parent, Node) ->
    Dialog = wxDialog:new(Parent, ?wxID_ANY, "Select Module or Event",
			  [{style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER},
			   {size, {400, 400}}]),
    Panel = wxPanel:new(Dialog),
    PanelSz = wxBoxSizer:new(?wxVERTICAL),
    MainSz  = wxBoxSizer:new(?wxVERTICAL),

    TxtCtrl = wxTextCtrl:new(Panel, ?wxID_ANY),
    ListBox = wxListBox:new(Panel, ?wxID_ANY, [{style, ?wxLB_SINGLE}]),
    wxSizer:add(PanelSz, TxtCtrl, [{flag, ?wxEXPAND}]),
    wxSizer:add(PanelSz, ListBox, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxPanel:setSizer(Panel, PanelSz),
    wxSizer:add(MainSz, Panel, [{flag, ?wxEXPAND bor ?wxALL},
				{border, 5}, {proportion, 1}]),
    Buttons = wxDialog:createButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
    wxSizer:add(MainSz, Buttons, [{flag, ?wxEXPAND bor ?wxALL},
				  {border, 5}, {proportion, 0}]),
    wxWindow:setSizer(Dialog, MainSz),
    OkId = wxDialog:getAffirmativeId(Dialog),
    OkButt = wxWindow:findWindowById(OkId),
    wxWindow:disable(OkButt),
    wxWindow:setFocus(TxtCtrl),
    %% init data
    Modules = get_modules(Node),
    Events = [{"Messages sent",{'$trace_event',send}},
	      {"Messages received",{'$trace_event','receive'}}],
    AllModules = Events ++ [{atom_to_list(X), X} || X <- Modules],
    filter_listbox_data("", AllModules, ListBox),
    wxTextCtrl:connect(TxtCtrl, command_text_updated,
		       [{callback, fun(#wx{event=#wxCommand{cmdString=Input}}, _) ->
					   filter_listbox_data(Input, AllModules, ListBox)
				   end}]),
    wxListBox:connect(ListBox, command_listbox_doubleclicked,
		      [{callback, fun(_, _) -> wxDialog:endModal(Dialog, ?wxID_OK) end}]),
    wxListBox:connect(ListBox, command_listbox_selected,
		      [{callback, fun(#wx{event=#wxCommand{commandInt=Id}}, _) ->
					  Id >= 0 andalso wxWindow:enable(OkButt)
				  end}]),

    case wxDialog:showModal(Dialog) of
	?wxID_OK ->
	    SelId  = wxListBox:getSelection(ListBox),
	    case SelId >= 0 of
		true ->
		    Module = wxListBox:getClientData(ListBox, SelId),
		    wxDialog:destroy(Dialog),
		    Module;
		false ->
		    wxDialog:destroy(Dialog),
		    throw(cancel)
	    end;
	?wxID_CANCEL ->
	    wxDialog:destroy(Dialog),
	    throw(cancel)
    end.

function_selector(Parent, Node, Module) ->
    Functions = observer_wx:try_rpc(Node, Module, module_info, [functions]),
    Externals = observer_wx:try_rpc(Node, Module, module_info, [exports]),

    Choices = lists:usort([{Name, Arity} || {Name, Arity} <- Externals ++ Functions,
					    not(erl_internal:guard_bif(Name, Arity))]),
    ParsedChoices = parse_function_names(Choices),
    case check_selector(Parent, ParsedChoices) of
	[] -> [{Module, {'_', '_'}}];
	FAs ->
	    [{Module, {F, A}} || {F,A} <- FAs]
    end.

check_selector(Parent, ParsedChoices) ->
    Dialog = wxDialog:new(Parent, ?wxID_ANY, "Trace Functions",
			  [{style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER},
			   {size, {400, 400}}]),

    Panel = wxPanel:new(Dialog),
    PanelSz = wxBoxSizer:new(?wxVERTICAL),
    MainSz  = wxBoxSizer:new(?wxVERTICAL),

    TxtCtrl = wxTextCtrl:new(Panel, ?wxID_ANY),
    ListBox = wxCheckListBox:new(Panel, ?wxID_ANY, [{style, ?wxLB_EXTENDED}]),
    wxSizer:add(PanelSz, TxtCtrl, [{flag, ?wxEXPAND}]),
    wxSizer:add(PanelSz, ListBox, [{flag, ?wxEXPAND}, {proportion, 1}]),
    SelAllBtn   = wxButton:new(Panel, ?wxID_ANY,   [{label, "Check Visible"}]),
    DeSelAllBtn = wxButton:new(Panel, ?wxID_ANY, [{label, "Uncheck Visible"}]),
    ButtonSz = wxBoxSizer:new(?wxHORIZONTAL),
    [wxSizer:add(ButtonSz, Button, []) || Button <- [SelAllBtn, DeSelAllBtn]],
    wxSizer:add(PanelSz, ButtonSz, [{flag, ?wxEXPAND bor ?wxALL},
				    {border, 5}, {proportion, 0}]),
    wxPanel:setSizer(Panel, PanelSz),
    wxSizer:add(MainSz, Panel, [{flag, ?wxEXPAND bor ?wxALL},
				{border, 5}, {proportion, 1}]),

    Buttons = wxDialog:createButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
    wxSizer:add(MainSz, Buttons, [{flag, ?wxEXPAND bor ?wxALL},
				  {border, 5}, {proportion, 0}]),
    wxWindow:setSizer(Dialog, MainSz),
    wxWindow:setFocus(TxtCtrl),
    %% Init
    filter_listbox_data("", ParsedChoices, ListBox, false),
    %% Setup Event handling
    wxTextCtrl:connect(TxtCtrl, command_text_updated,
		       [{callback,
			 fun(#wx{event=#wxCommand{cmdString=Input}}, _) ->
				 filter_listbox_data(Input, ParsedChoices, ListBox, false)
			 end}]),
    Self = self(),

    %% Sigh clientdata in checklistbox crashes on windows, wx-bug I presume.
    %% Don't have time to investigate now, workaround file bug report later
    GetClientData = fun(LB, N) ->
			    String = wxListBox:getString(LB, N),
			    {_, Data} = lists:keyfind(String, 1, ParsedChoices),
			    Data
		    end,
    wxCheckListBox:connect(ListBox, command_checklistbox_toggled,
			   [{callback,
			     fun(#wx{event=#wxCommand{commandInt=N}}, _) ->
				     Self ! {ListBox, wxCheckListBox:isChecked(ListBox, N),
					     GetClientData(ListBox, N)}
			     end}]),
    Check = fun(Id, Bool) ->
    		    wxCheckListBox:check(ListBox, Id, [{check, Bool}]),
    		    Self ! {ListBox, Bool, GetClientData(ListBox, Id)}
    	    end,
    wxButton:connect(SelAllBtn, command_button_clicked,
    		     [{callback, fun(#wx{}, _) ->
    					 Count = wxListBox:getCount(ListBox),
    					 [Check(SelId, true) ||
    					     SelId <- lists:seq(0, Count-1),
    					     not wxCheckListBox:isChecked(ListBox, SelId)]
    				 end}]),
    wxButton:connect(DeSelAllBtn, command_button_clicked,
    		     [{callback, fun(#wx{}, _) ->
    					 Count = wxListBox:getCount(ListBox),
    					 [Check(SelId, false) ||
    					     SelId <- lists:seq(0, Count-1),
    					     wxCheckListBox:isChecked(ListBox, SelId)]
    				 end}]),
    case wxDialog:showModal(Dialog) of
	?wxID_OK ->
	    wxDialog:destroy(Dialog),
	    get_checked(ListBox, []);
	?wxID_CANCEL ->
	    wxDialog:destroy(Dialog),
	    get_checked(ListBox, []),
	    throw(cancel)
    end.

get_checked(ListBox, Acc) ->
    receive
	{ListBox, true, FA} ->
	    get_checked(ListBox, [FA|lists:delete(FA,Acc)]);
	{ListBox, false, FA} ->
	    get_checked(ListBox, lists:delete(FA,Acc))
    after 0 ->
	    lists:reverse(Acc)
    end.

select_matchspec(Pid, Parent, AllMatchSpecs, Key) ->
    {MatchSpecs,RestMS} =
	case lists:keytake(Key,1,AllMatchSpecs) of
	    {value,{Key,MSs0},Rest} -> {MSs0,Rest};
	    false -> {[],AllMatchSpecs}
	end,
    Dialog = wxDialog:new(Parent, ?wxID_ANY, "Trace Match Specifications",
			  [{style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER},
			   {size, {400, 400}}]),

    Panel = wxPanel:new(Dialog),
    PanelSz = wxBoxSizer:new(?wxVERTICAL),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    TxtSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Match specification:"}]),
    BtnSz = wxBoxSizer:new(?wxHORIZONTAL),
    SavedSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Saved match specifications:"}]),

    TextCtrl = create_styled_txtctrl(Panel),
    wxSizer:add(TxtSz, TextCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),

    AddMsBtn  = wxButton:new(Panel, ?wxID_ANY, [{label, "New"}]),
    EditMsBtn  = wxButton:new(Panel, ?wxID_ANY, [{label, "Edit"}]),
    DelMsBtn  = wxButton:new(Panel, ?wxID_ANY, [{label, "Delete"}]),
    wxSizer:add(BtnSz, AddMsBtn),
    wxSizer:add(BtnSz, EditMsBtn),
    wxSizer:add(BtnSz, DelMsBtn),

    ListBox = wxListBox:new(Panel, ?wxID_ANY, []),
    wxSizer:add(SavedSz, ListBox, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(PanelSz, TxtSz, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(PanelSz, BtnSz),
    wxSizer:add(PanelSz, SavedSz, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxWindow:setSizer(Panel, PanelSz),
    wxSizer:add(MainSz, Panel, [{flag, ?wxEXPAND bor ?wxALL},
				{border, 5}, {proportion, 1}]),
    Buttons = wxDialog:createButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
    wxSizer:add(MainSz, Buttons, [{flag, ?wxEXPAND bor ?wxALL},
				  {border, 5}, {proportion, 0}]),
    wxWindow:setSizer(Dialog, MainSz),
    OkId = wxDialog:getAffirmativeId(Dialog),
    OkButt = wxWindow:findWindowById(OkId),
    wxWindow:disable(OkButt),
    wxWindow:disable(EditMsBtn),
    wxWindow:disable(DelMsBtn),

    Choices = ms_names(MatchSpecs),
    filter_listbox_data("", Choices, ListBox),

    Add = fun(_,_) ->
		  case edit_ms(TextCtrl, new, Dialog) of
		      Ms = #match_spec{} ->
			  add_and_select(-1, Ms, ListBox),
			  wxWindow:enable(OkButt),
			  wxWindow:enable(EditMsBtn),
			  wxWindow:enable(DelMsBtn);
		      Else -> Else
		  end
	  end,
    Edit = fun(_,_) ->
		   SelId = wxListBox:getSelection(ListBox),
		   case SelId >= 0 of
		       true ->
			   #match_spec{name=Name} = wxListBox:getClientData(ListBox,SelId),
			   case edit_ms(TextCtrl, Name, Dialog) of
			       Ms = #match_spec{} ->
				   add_and_select(SelId, Ms, ListBox),
				   wxWindow:enable(OkButt),
				   wxWindow:enable(EditMsBtn),
				   wxWindow:enable(DelMsBtn);
			       Else -> Else
			   end;
		       false ->
			   ok
		   end
	   end,
    Del = fun(_,_) ->
		  SelId = wxListBox:getSelection(ListBox),
		  case SelId >= 0 of
		      true ->
			  wxListBox:delete(ListBox, SelId);
		      false ->
			  ok
		  end
	  end,
    Sel = fun(#wx{event=#wxCommand{commandInt=Id}}, _) ->
		  case Id >= 0 of
		      true ->
			  wxWindow:enable(OkButt),
			  wxWindow:enable(EditMsBtn),
			  wxWindow:enable(DelMsBtn),
			  #match_spec{func=Str} = wxListBox:getClientData(ListBox,Id),
			  wxStyledTextCtrl:setText(TextCtrl, Str);
		      false ->
			  try
			      wxWindow:disable(OkButt),
			      wxWindow:disable(EditMsBtn),
			      wxWindow:disable(DelMsBtn)
			  catch _:_ -> ok
			  end
		  end
	  end,
    wxButton:connect(AddMsBtn,  command_button_clicked,  [{callback,Add}]),
    wxButton:connect(EditMsBtn,  command_button_clicked, [{callback,Edit}]),
    wxButton:connect(DelMsBtn,  command_button_clicked,  [{callback,Del}]),
    wxListBox:connect(ListBox, command_listbox_selected, [{callback, Sel}]),
    case wxDialog:showModal(Dialog) of
	?wxID_OK ->
	    SelId  = wxListBox:getSelection(ListBox),
	    Count = wxListBox:getCount(ListBox),
	    MSs = [wxListBox:getClientData(ListBox, Id) ||
		      Id <- lists:seq(0, Count-1)],
	    Pid ! {update_ms, [{Key,MSs}|RestMS]},
	    MS = lists:nth(SelId+1, MSs),
	    wxDialog:destroy(Dialog),
	    MS;
	?wxID_CANCEL ->
	    wxDialog:destroy(Dialog),
	    throw(cancel)
    end.

output(Parent, Default) ->
    Dialog = wxDialog:new(Parent, ?wxID_ANY, "Process Options",
			  [{style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER}]),
    Panel = wxPanel:new(Dialog),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    PanelSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel,  [{label, "Output"}]),

    %% Output select
    WinB = wxCheckBox:new(Panel, ?wxID_ANY, "Window", []),
    check_box(WinB, proplists:get_value(window, Default, true)),
    ShellB = wxCheckBox:new(Panel, ?wxID_ANY, "Shell", []),
    check_box(ShellB, proplists:get_value(shell, Default, false)),
    [wxSizer:add(PanelSz, CheckBox, []) || CheckBox <- [WinB, ShellB]],
    GetFileOpts = ttb_file_options(Panel, PanelSz, Default),
    %% Set sizer and show dialog
    wxPanel:setSizer(Panel, PanelSz),
    wxSizer:add(MainSz, Panel, [{flag, ?wxEXPAND bor ?wxALL}, {proportion,1}, {border, 3}]),
    Buttons = wxDialog:createButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
    wxSizer:add(MainSz, Buttons, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxWindow:setSizerAndFit(Dialog, MainSz),
    wxSizer:setSizeHints(MainSz, Dialog),
    case wxDialog:showModal(Dialog) of
	?wxID_OK ->
	    Res = [{window, wxCheckBox:getValue(WinB)},
		   {shell, wxCheckBox:getValue(ShellB)} | GetFileOpts()],
	    wxDialog:destroy(Dialog),
	    Res;
	?wxID_CANCEL ->
	    wxDialog:destroy(Dialog),
	    throw(cancel)
    end.

edit_ms(TextCtrl, Label0, Parent) ->
    Str = ensure_last_is_dot(wxStyledTextCtrl:getText(TextCtrl)),
    try
	MatchSpec = ms_from_string(Str),
	Label = case Label0 == new of
		    true -> get_label(Parent);
		    _ -> Label0
		end,
	#match_spec{name=Label, term=MatchSpec,
		    str=io_lib:format("~w",[MatchSpec]),
		    func=Str}
    catch
	throw:cancel ->
	    ok;
	throw:Error ->
	    observer_wx:create_txt_dialog(Parent, Error, "Error", ?wxICON_ERROR),
	    ok
    end.

get_label(Frame) ->
    Dialog = wxTextEntryDialog:new(Frame, "Enter alias: "),
    case wxDialog:showModal(Dialog) of
	?wxID_OK ->
	    wxTextEntryDialog:getValue(Dialog);
	?wxID_CANCEL ->
	    throw(cancel)
    end.

ms_from_string(Str) ->
    try
	Tokens = case erl_scan:string(Str) of
		     {ok, Ts, _} -> Ts;
		     {error, {SLine, SMod, SError}, _} ->
			 throw(io_lib:format("~w: ~s", [SLine,SMod:format_error(SError)]))
		 end,
	Exprs  = case erl_parse:parse_exprs(Tokens) of
		     {ok, T} -> T;
		     {error, {PLine, PMod, PError}} ->
			 throw(io_lib:format("~w: ~s", [PLine,PMod:format_error(PError)]))
		 end,
	Term = case Exprs of
		   [{'fun', _, {clauses, Clauses}}|_] ->
		       case ms_transform:transform_from_shell(dbg,Clauses,orddict:new()) of
			   {error, [{_,[{MSLine,Mod,MSInfo}]}],_} ->
			       throw(io_lib:format("~w: ~p", [MSLine,Mod:format_error(MSInfo)]));
			   {error, _} ->
			       throw("Could not convert fun() to match spec");
			   Ms ->
			       Ms
		       end;
		   [Expr|_] ->
		       erl_parse:normalise(Expr)
	       end,
	case erlang:match_spec_test([], Term, trace) of
	    {ok, _, _, _} -> Term;
	    {error, List} -> throw([[Error, $\n] || {_, Error} <- List])
	end
    catch error:_Reason ->
	    %% io:format("Bad term: ~s~n ~p in ~p~n", [Str, _Reason, erlang:get_stacktrace()]),
	    throw("Invalid term")
    end.

add_and_select(Id, MS0, ListBox) ->
    [{Str,User}] = ms_names([MS0]),
    Sel = case Id >= 0 of
	     true ->
		  wxListBox:setString(ListBox, Id, Str),
		  wxListBox:setClientData(ListBox, Id, User),
		  Id;
	      false ->
		  wxListBox:append(ListBox, Str, User)
	  end,
    wxListBox:setSelection(ListBox, Sel).

filter_listbox_data(Input, Data, ListBox) ->
    filter_listbox_data(Input, Data, ListBox, true).

filter_listbox_data(Input, Data, ListBox, AddClientData) ->
    FilteredData = [X || X = {Str, _} <- Data, re:run(Str, Input) =/= nomatch],
    wxListBox:clear(ListBox),
    wxListBox:appendStrings(ListBox, [Str || {Str,_} <- FilteredData]),
    AddClientData andalso
	wx:foldl(fun({_, Term}, N) ->
			 wxListBox:setClientData(ListBox, N, Term),
			 N+1
		 end, 0, FilteredData),
    FilteredData.

get_modules(Node) ->
    lists:sort([Module || {Module, _} <- observer_wx:try_rpc(Node, code, all_loaded, [])]).

optionpage_top_right(Panel, TopRightSz, Options, Text) ->
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    ChkBox = wxCheckBox:new(Panel, ?wxID_ANY, "Inherit on " ++ Text, []),
    RadioSz = wxBoxSizer:new(?wxVERTICAL),
    Radio1 = wxRadioButton:new(Panel, ?wxID_ANY, "All " ++ Text, [{style, ?wxRB_GROUP}]),
    Radio2 = wxRadioButton:new(Panel, ?wxID_ANY, "First " ++ Text ++ " only", []),
    wxSizer:add(Sizer, ChkBox, []),
    wxSizer:add(RadioSz, Radio1, []),
    wxSizer:add(RadioSz, Radio2, []),
    wxSizer:add(Sizer, RadioSz, [{flag, ?wxLEFT},{border, 20}]),
    wxSizer:add(TopRightSz, Sizer, Options),
    {ChkBox, Radio1, Radio2}.


create_styled_txtctrl(Parent) ->
    FixedFont = observer_wx:get_attrib({font, fixed}),
    Ed = wxStyledTextCtrl:new(Parent),
    wxStyledTextCtrl:styleClearAll(Ed),
    wxStyledTextCtrl:styleSetFont(Ed, ?wxSTC_STYLE_DEFAULT, FixedFont),
    wxStyledTextCtrl:setLexer(Ed, ?wxSTC_LEX_ERLANG),
    wxStyledTextCtrl:setMarginType(Ed, 1, ?wxSTC_MARGIN_NUMBER),
    wxStyledTextCtrl:setSelectionMode(Ed, ?wxSTC_SEL_LINES),
    wxStyledTextCtrl:setUseHorizontalScrollBar(Ed, false),

    Styles =  [{?wxSTC_ERLANG_DEFAULT,  {0,0,0}},
	       {?wxSTC_ERLANG_COMMENT,  {160,53,35}},
	       {?wxSTC_ERLANG_VARIABLE, {150,100,40}},
	       {?wxSTC_ERLANG_NUMBER,   {5,5,100}},
	       {?wxSTC_ERLANG_KEYWORD,  {130,40,172}},
	       {?wxSTC_ERLANG_STRING,   {170,45,132}},
	       {?wxSTC_ERLANG_OPERATOR, {30,0,0}},
	       {?wxSTC_ERLANG_ATOM,     {0,0,0}},
	       {?wxSTC_ERLANG_FUNCTION_NAME, {64,102,244}},
	       {?wxSTC_ERLANG_CHARACTER,{236,155,172}},
	       {?wxSTC_ERLANG_MACRO,    {40,144,170}},
	       {?wxSTC_ERLANG_RECORD,   {40,100,20}},
	       {?wxSTC_ERLANG_SEPARATOR,{0,0,0}},
	       {?wxSTC_ERLANG_NODE_NAME,{0,0,0}}],
    SetStyle = fun({Style, Color}) ->
		       wxStyledTextCtrl:styleSetFont(Ed, Style, FixedFont),
		       wxStyledTextCtrl:styleSetForeground(Ed, Style, Color)
	       end,
    [SetStyle(Style) || Style <- Styles],
    wxStyledTextCtrl:setKeyWords(Ed, 0, keyWords()),
    Ed.


keyWords() ->
    L = ["after","begin","case","try","cond","catch","andalso","orelse",
	 "end","fun","if","let","of","query","receive","when","bnot","not",
	 "div","rem","band","and","bor","bxor","bsl","bsr","or","xor"],
    lists:flatten([K ++ " " || K <- L] ++ [0]).


enable(CheckBox, Radio) ->
    case wxCheckBox:isChecked(CheckBox) of
	false ->
	    [wxWindow:disable(R) || R <- Radio];
	true ->
	    [wxWindow:enable(R) || R <- Radio]
    end.


check_box(ChkBox, Bool) ->
    case Bool of
	true ->
	    wxCheckBox:set3StateValue(ChkBox, ?wxCHK_CHECKED);
	false ->
	    ignore
    end.

parse_function_names(Choices) ->
    StrList = [{atom_to_list(Name) ++ "/" ++ integer_to_list(Arity), Term}
	       || Term = {Name, Arity} <- Choices],
    parse_function_names(StrList, []).

parse_function_names([], Acc) ->
    lists:reverse(Acc);
parse_function_names([{H, Term}|T], Acc) ->
    IsFun = re:run(H, ".*-fun-\\d*?-"),
    IsLc = re:run(H, ".*-lc\\$\\^\\d*?/\\d*?-\\d*?-"),
    IsLbc = re:run(H, ".*-lbc\\$\\^\\d*?/\\d*?-\\d*?-"),
    Parsed =
	if IsFun =/= nomatch -> "Fun: " ++ H;
	   IsLc =/= nomatch -> "List comprehension: " ++ H;
	   IsLbc =/= nomatch -> "Bit comprehension: " ++ H;
	   true ->
		H
	end,
    parse_function_names(T, [{Parsed, Term} | Acc]).

ms_names(MatchSpecList) ->
    MsOrAlias = fun(#match_spec{name = A, str = M}) ->
			case A of
			    "" -> M;
			    _ -> A ++ "    " ++ M
			end
		end,
    [{MsOrAlias(X), X} || X <- MatchSpecList].

ensure_last_is_dot([]) ->
    ".";
ensure_last_is_dot(String) ->
    case lists:last(String) =:= $. of
	true ->
	    String;
	false ->
	    String ++ "."
    end.

ttb_file_options(Panel, Sizer, Default) ->
    Top   = wxBoxSizer:new(?wxVERTICAL),
    NameS = wxBoxSizer:new(?wxHORIZONTAL),
    FileBox = wxCheckBox:new(Panel, ?wxID_ANY, "File (Using ttb file tracer)", []),
    check_box(FileBox, proplists:get_value(file, Default, false)),
    wxSizer:add(Sizer, FileBox),
    Desc  = wxStaticText:new(Panel, ?wxID_ANY, "File"),
    FileName = proplists:get_value(filename, Default, "ttb"),
    FileT = wxTextCtrl:new(Panel, ?wxID_ANY, [{size, {150,-1}}, {value, FileName}]),
    FileB = wxButton:new(Panel, ?wxID_ANY, [{label, "Browse"}]),
    wxSizer:add(NameS, Desc,  [{proportion, 0}, {flag, ?wxALIGN_CENTER_VERTICAL}]),
    wxSizer:add(NameS, FileT, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL}]),
    wxSizer:add(NameS, FileB, [{proportion, 0}, {flag, ?wxALIGN_CENTER_VERTICAL}]),

    WrapB = wxCheckBox:new(Panel, ?wxID_ANY, "Wrap logs"),
    WrapSz = wxSlider:new(Panel, ?wxID_ANY, proplists:get_value(wrap_sz, Default, 128),
			  64, 10*1024, [{style, ?wxSL_HORIZONTAL bor ?wxSL_LABELS}]),
    WrapC = wxSlider:new(Panel, ?wxID_ANY, proplists:get_value(wrap_c, Default, 8),
			 2, 100, [{style, ?wxSL_HORIZONTAL bor ?wxSL_LABELS}]),

    wxSizer:add(Top, NameS, [{flag, ?wxEXPAND}]),
    wxSizer:add(Top, WrapB, []),
    wxSizer:add(Top, WrapSz,[{flag, ?wxEXPAND}]),
    wxSizer:add(Top, WrapC, [{flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, Top, [{flag, ?wxEXPAND bor ?wxLEFT},{border, 20}]),

    Enable = fun(UseFile, UseWrap0) ->
		     UseWrap = UseFile andalso UseWrap0,
		     [wxWindow:enable(W, [{enable, UseFile}]) || W <- [Desc,FileT,FileB,WrapB]],
		     [wxWindow:enable(W, [{enable, UseWrap}]) || W <- [WrapSz, WrapC]],
		     check_box(WrapB, UseWrap0)
	     end,
    Enable(proplists:get_value(file, Default, false), proplists:get_value(wrap, Default, false)),
    wxPanel:connect(FileBox, command_checkbox_clicked,
		    [{callback, fun(_,_) ->
					Enable(wxCheckBox:getValue(FileBox),
					       wxCheckBox:getValue(WrapB))
				end}]),
    wxPanel:connect(WrapB, command_checkbox_clicked,
		    [{callback, fun(_,_) ->
					Enable(true, wxCheckBox:getValue(WrapB))
				end}]),
    wxPanel:connect(FileB, command_button_clicked,
		    [{callback, fun(_,_) ->  get_file(FileT) end}]),
    fun() ->
	    [{file, wxCheckBox:getValue(FileBox)},
	     {filename, wxTextCtrl:getValue(FileT)},
	     {wrap, wxCheckBox:getValue(WrapB)},
	     {wrap_sz, wxSlider:getValue(WrapSz)},
	     {wrap_c, wxSlider:getValue(WrapC)}]
    end.

get_file(Text) ->
    Str = wxTextCtrl:getValue(Text),
    Dialog = wxFileDialog:new(Text,
			      [{message, "Select a file"},
			       {defaultFile, Str}]),
    case wxDialog:showModal(Dialog) of
	?wxID_OK ->
	    Dir = wxFileDialog:getDirectory(Dialog),
	    File = wxFileDialog:getFilename(Dialog),
	    wxTextCtrl:setValue(Text, filename:join(Dir, File));
	_ -> ok
    end,
    wxFileDialog:destroy(Dialog).
