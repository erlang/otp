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

%%
-module(dbg_wx_break_win).

%% External exports
-export([create_win/5,
	 update_functions/2,
	 handle_event/2]).

-include_lib("wx/include/wx.hrl").

-record(winInfo, {type,            % line | conditional | function
		  win,             % wxobj()
		  entries,         % [{atom|integer, wxobj()}]
		  trigger,         % [{wxobj(),enable | disable | delete}]
		  listbox,         % wxobj()
		  text,            % wxobj()
		  ok,              % wxobj()
		  funcs=[]         % [[Name, Arity]]
		 }).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% create_win(Win, Pos, Type, Mod, Line) -> #winInfo{}
%%   Win = Top level window
%%   Pos = {X, Y}
%%     X = Y = integer()
%%   Type =  line | conditional | function
%%   Mod = atom() | ""
%%   Line = integer() | ""
%%--------------------------------------------------------------------

create_win(Parent, Pos, function, Mod, _Line) ->
    Win = wxDialog:new(Parent, ?wxID_ANY, "Function Break",
		       [{pos, Pos}, 
			{style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER}]),
    MainS = wxBoxSizer:new(?wxVERTICAL),  
    Label = wxStaticText:new(Win, ?wxID_ANY, "Module:"),
    Int = int:interpreted(),
    IntStrs = [atom_to_list(M) || M <- Int],
    Text  = wxComboBox:new(Win, ?wxID_ANY, 
			   [{value, dbg_wx_win:to_string(Mod)}, 
			    {choices, IntStrs}]),
    
    Expand = [{border, 5}, {flag,?wxLEFT bor ?wxRIGHT bor ?wxEXPAND}],
    _ = wxSizer:add(MainS, Label, [{border,5},
			       {flag,?wxTOP bor ?wxLEFT bor ?wxRIGHT}]),
    _ = wxSizer:add(MainS, Text, Expand),
    FunLabel = wxStaticText:new(Win, ?wxID_ANY, "Function:"),
    LB = wxListBox:new(Win, ?wxID_ANY, [{size,{-1, 100}},{style,?wxLB_MULTIPLE}]),
    _ = wxSizer:add(MainS, FunLabel, Expand),
    _ = wxSizer:add(MainS, LB, [{proportion,1}|Expand]),
    wxSizer:setMinSize(MainS, 300, 400),
    OK = wxDialog:createStdDialogButtonSizer(Win, ?wxOK bor ?wxCANCEL),
    _ = wxSizer:add(MainS, OK, [{border,5},{flag,?wxALL}]),
    wxDialog:setSizer(Win,MainS),
    _ = wxSizer:fit(MainS, Win),
    wxSizer:setSizeHints(MainS,Win),
    wxComboBox:setFocus(Text),
    wxDialog:connect(Win,    command_button_clicked),
    wxComboBox:connect(Text, command_text_updated),
    wxListBox:connect(LB, command_listbox_selected),
    wxListBox:connect(LB, command_listbox_doubleclicked),
    OKId   = wxDialog:getAffirmativeId(Win),
    OKButt = wxWindow:findWindowById(OKId, [{parent, Win}]),
    wxWindow:disable(OKButt),
    wxDialog:centreOnParent(Win),
    wxDialog:show(Win),

    #winInfo{type=function, win=Win, text=Text, ok=OKButt,
	     entries=[], trigger=enable,
	     listbox=LB, funcs=[]};

create_win(Parent, Pos, Type, Mod, Line) ->
    Title = case Type of
		line -> "Line Break";
		conditional -> "Conditional Break"
	    end,
    Style = ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER,
    Win = wxDialog:new(Parent, ?wxID_ANY, Title,
		       [{pos, Pos}, 
			{style, Style}]),
    %% Create Sizers
    MainS = wxBoxSizer:new(?wxVERTICAL),      
    
    %% Add module
    Int = int:interpreted(),
    IntStrs = [atom_to_list(M) || M <- Int],
    ModT  = wxComboBox:new(Win, ?wxID_ANY, [{choices,IntStrs}]),
    ModSz = create_label_of_control(Win, "Module:", ModT, Mod),
    _ = wxSizer:add(MainS,ModSz,[{flag, ?wxEXPAND}]),
    %% Create rest of text input fields
    Add = fun({IType, Label, Def}) ->
		  {Sz, Text} = create_sizer_with_text(Win, Label, Def),
		  _ = wxSizer:add(MainS, Sz, [{flag, ?wxEXPAND}]),
		  {Text, IType}
	  end,
    Inputs = case Type of
		 line ->
		     [{integer,"Line:",Line}];
		 conditional ->
		     [{integer,"Line:",Line},
		      {atom,"C-Module:",""}, 
		      {atom,"C-Function:",""}]
	     end,    
    %% Add the rest of the entries
    Entries = wx:map(Add, Inputs),    
    %% Create and add radio box
    {TriggerBox,Trigger} = create_trigger_box(Win),
    _ = wxSizer:add(MainS, TriggerBox, [{border,5},{flag,?wxALL bor ?wxEXPAND}]),

    _ = wxSizer:addStretchSpacer(MainS),
    %% Put it together
    OK = wxDialog:createStdDialogButtonSizer(Win, ?wxOK bor ?wxCANCEL),
    _ = wxSizer:add(MainS, OK, [{border,5},{flag,?wxALL}]),
    wxSizer:setMinSize(MainS, 300, -1),
    wxDialog:setSizer(Win,MainS),
    _ = wxSizer:fit(MainS, Win),
    wxSizer:setSizeHints(MainS,Win),
    wxComboBox:setFocus(ModT),
    wxDialog:connect(Win, command_button_clicked),
    wxDialog:connect(Win, command_text_updated),
    OKId   = wxDialog:getAffirmativeId(Win),
    OKButt = wxWindow:findWindowById(OKId),
    wxWindow:disable(OKButt),
    wxDialog:centreOnParent(Win),
    wxDialog:show(Win),
    #winInfo{type=Type, win=Win, text=ModT, 
	     entries=Entries, trigger=Trigger, ok=OKButt}.
    
%%--------------------------------------------------------------------
%% update_functions(WinInfo, Funcs) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Funcs = [{Name, Arity}]
%%     Name = atom()
%%     Arity = integer()
%%--------------------------------------------------------------------
update_functions(WinInfo, Funcs) ->
    Items = lists:map(fun([N, A]) -> 
			      lists:flatten(io_lib:format("~p/~p", [N,A]))
		      end,
		      Funcs),
    wxListBox:set(WinInfo#winInfo.listbox, Items),
    WinInfo#winInfo{funcs=Funcs}.

%%--------------------------------------------------------------------
%% handle_event(WxEvent, WinInfo) -> Command
%% WxEvent = #wx{}
%% WinInfo = #winInfo{}
%% Command = ignore
%%         | stopped
%%         | {win, WinInfo}
%%         | {module, Mod}
%%         | {break, [[Mod, Line]], Action}
%%         | {break, [[Mod, Line, CMod, CFunc]], Action}
%%         | {break, [[Mod, Func, Arity]], Action}
%%--------------------------------------------------------------------
handle_event(#wx{id=?wxID_CANCEL}, #winInfo{win=Win}) ->
    wxDialog:destroy(Win),
    stopped;
handle_event(#wx{event=#wxCommand{type=command_text_updated}}, 
	     #winInfo{type=function, text=Text, ok=OK}) ->
    Module = wxComboBox:getValue(Text),
    wxWindow:disable(OK),
    {module, list_to_atom(Module)};
handle_event(#wx{event=#wxCommand{type=command_text_updated}}, 
	     #winInfo{text=Text, ok=OK, entries=Es}) ->
    Module = wxComboBox:getValue(Text),
    case check_input(Es) of
	error -> wxWindow:disable(OK);
	_Data when Module =/= "" -> wxWindow:enable(OK);
	_ -> wxWindow:disable(OK)
    end,
    ignore;
handle_event(#wx{event=#wxCommand{type=command_listbox_selected}}, 
	     #winInfo{type=function, listbox=LB, ok=OK}) ->
    case wxListBox:getSelections(LB) of
	{N,_} when N > 0 -> wxWindow:enable(OK);
	_ -> wxWindow:disable(OK)
    end,
    ignore;
handle_event(#wx{id=OKorListBox, event=#wxCommand{type=OKorDoubleClick}},
	     #winInfo{type=function,win=Win,listbox=LB,funcs=Funcs,text=Text})
  when OKorListBox =:= ?wxID_OK; 
       OKorDoubleClick =:= command_listbox_doubleclicked ->
    Mod = wxComboBox:getValue(Text),
    {_, IndexL} = wxListBox:getSelections(LB),
    Breaks = [[list_to_atom(Mod)|lists:nth(Index+1, Funcs)] || Index <- IndexL],
    wxDialog:destroy(Win),
    {break, Breaks, enable};
handle_event(#wx{id=?wxID_OK},#winInfo{win=Win,text=Text, entries=Es, trigger=Trigger}) ->
    %% Non function box
    Mod = wxComboBox:getValue(Text),
    Data = check_input(Es),
    Trigged = get_trigger(Trigger),
    wxDialog:destroy(Win),    
    {break, [[list_to_atom(Mod)|Data]], Trigged};

handle_event(_WxEvent, _WinInfo) ->
    %% io:format("Ev: ~p ~n", [_WxEvent]),
    ignore.

check_input(Entries) ->
    check_input(Entries, []).
check_input([{Entry, Type} | Entries], Data) ->
    Str = wxTextCtrl:getValue(Entry),
    case erl_scan:string(Str) of
	{ok, [{Type, _Line, Val}], _EndLine} ->
	    check_input(Entries, [Val|Data]);
	_Error -> error
    end;
check_input([], Data) -> lists:reverse(Data).

create_sizer_with_text(Parent,Label,Def) ->    
    Text  = wxTextCtrl:new(Parent, ?wxID_ANY), 
    Sz = create_label_of_control(Parent, Label, Text, Def),
    {Sz, Text}.

create_label_of_control(Parent, Label, Control, Def) ->
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    Text  = wxStaticText:new(Parent, ?wxID_ANY, Label),
    Border = {border, 5},
    Flag   = ?wxRIGHT bor ?wxLEFT bor ?wxALIGN_CENTRE_VERTICAL,
    _ = wxSizer:add(Sizer, Text, [{proportion,1}, {flag,Flag}, Border]),
    _ = wxSizer:add(Sizer, Control, [{proportion,3}, {flag,Flag bor ?wxEXPAND}, Border]),
    wxControl:setLabel(Control, dbg_wx_win:to_string(Def)),
    Sizer.
    
create_trigger_box(Win) ->
    SBox = wxStaticBox:new(Win, ?wxID_ANY, "Trigger Action:"),
    SBS  = wxStaticBoxSizer:new(SBox, ?wxVERTICAL),
    Ebtn = wxRadioButton:new(Win, ?wxID_ANY, "Enable"),
    _ = wxSizer:add(SBS,Ebtn),
    Dibtn = wxRadioButton:new(Win, ?wxID_ANY, "Disable"),
    _ = wxSizer:add(SBS,Dibtn),
    Debtn = wxRadioButton:new(Win, ?wxID_ANY, "Delete"),
    _ = wxSizer:add(SBS,Debtn),
    wxRadioButton:setValue(Ebtn, true),
    {SBS, [{Ebtn,enable},{Dibtn,disable},{Debtn,delete}]}.

get_trigger([{Btn,Op}|R]) ->
    case wxRadioButton:getValue(Btn) of
	true -> Op;
	false -> get_trigger(R)
    end.
	     
    
