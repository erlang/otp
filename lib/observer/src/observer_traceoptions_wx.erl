%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011. All Rights Reserved.
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

-module(observer_traceoptions_wx).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-export([process_trace/2, trace_pattern/4]).

-compile(export_all).

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
    SendBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace send message", []),
    check_box(SendBox, lists:member(send, Default)),
    RecBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace receive message", []),
    check_box(RecBox, lists:member('receive', Default)),
    EventBox = wxCheckBox:new(Panel, ?wxID_ANY, "Trace process events", []),
    check_box(EventBox, lists:member(events, Default)),

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
    wxRadioButton:setValue(SpwnAllRadio, lists:member(on_spawn, Default)),
    wxRadioButton:setValue(SpwnFirstRadio, lists:member(on_first_spawn, Default)),
    wxRadioButton:setValue(LinkAllRadio, lists:member(on_link, Default)),
    wxRadioButton:setValue(LinkFirstRadio, lists:member(on_first_link, Default)),

    wxSizer:add(LeftSz, FuncBox, []),
    wxSizer:add(LeftSz, SendBox, []),
    wxSizer:add(LeftSz, RecBox, []),
    wxSizer:add(LeftSz, EventBox, []),
    wxSizer:add(LeftSz, 150, -1),

    wxSizer:add(PanelSz, LeftSz, [{flag, ?wxEXPAND}]),
    wxSizer:add(PanelSz, RightSz,[{flag, ?wxEXPAND}]),
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

    Res = case wxDialog:showModal(Dialog) of
	      ?wxID_OK ->
		  All = [{SendBox, send}, {RecBox, 'receive'},
			 {FuncBox, functions}, {EventBox, events},
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
		  {ok, lists:reverse(Opts)};
	      ?wxID_CANCEL ->
		  cancel
	  end,
    wxDialog:destroy(Dialog),
    Res.

trace_pattern(ParentPid, Parent, Node, MatchSpecs) ->
    try
	Module = module_selector(Parent, Node),
	MFAs  = function_selector(Parent, Node, Module),
	MatchSpec = select_matchspec(ParentPid, Parent, MatchSpecs),
	{Module, [#tpattern{m=M,fa={F,A},ms=MatchSpec} || {M,F,A} <- MFAs]}
    catch cancel -> cancel
    end.

module_selector(Parent, Node) ->
    Dialog = wxDialog:new(Parent, ?wxID_ANY, "Select Module",
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
    AllModules = [{atom_to_list(X), X} || X <- Modules],
    filter_listbox_data("", AllModules, ListBox),
    wxTextCtrl:connect(TxtCtrl, command_text_updated,
		       [{callback, fun(#wx{event=#wxCommand{cmdString=Input}}, _) ->
					   filter_listbox_data(Input, AllModules, ListBox)
				   end}]),
    wxListBox:connect(ListBox, command_listbox_doubleclicked,
		      [{callback, fun(_, _) -> wxDialog:endModal(Dialog, ?wxID_OK) end}]),
    wxListBox:connect(ListBox, command_listbox_selected,
		      [{callback, fun(_, _) -> wxWindow:enable(OkButt) end}]),

    case wxDialog:showModal(Dialog) of
	?wxID_OK ->
	    SelId  = wxListBox:getSelection(ListBox),
	    Module = wxListBox:getClientData(ListBox, SelId),
	    wxDialog:destroy(Dialog),
	    Module;
	?wxID_CANCEL ->
	    wxDialog:destroy(Dialog),
	    throw(cancel)
    end.

function_selector(Parent, Node, Module) ->
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
    Functions = observer_wx:try_rpc(Node, Module, module_info, [functions]),
    Choices = lists:sort([{Name, Arity} || {Name, Arity} <- Functions,
					   not(erl_internal:guard_bif(Name, Arity))]),
    ParsedChoices = parse_function_names(Choices),
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
	    case get_checked_funcs(ListBox, []) of
		[] -> [{Module, '_', '_'}];
		FAs ->
		    [{Module, F, A} || {F,A} <- FAs]
	    end;
	?wxID_CANCEL ->
	    wxDialog:destroy(Dialog),
	    throw(cancel)
    end.

get_checked_funcs(ListBox, Acc) ->
    receive
	{ListBox, true, FA} ->
	    get_checked_funcs(ListBox, [FA|lists:delete(FA,Acc)]);
	{ListBox, false, FA} ->
	    get_checked_funcs(ListBox, lists:delete(FA,Acc))
    after 0 ->
	    lists:reverse(Acc)
    end.

select_matchspec(Pid, Parent, MatchSpecs) ->
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
		  case edit_ms(TextCtrl, new, Parent) of
		      Ms = #match_spec{} -> add_and_select(-1, Ms, ListBox);
		      Else -> Else
		  end
	  end,
    Edit = fun(_,_) ->
		   SelId = wxListBox:getSelection(ListBox),
		   case SelId >= 0 of
		       true ->
			   #match_spec{name=Name} = wxListBox:getClientData(ListBox,SelId),
			   case edit_ms(TextCtrl, Name, Parent) of
			       Ms = #match_spec{} -> add_and_select(SelId, Ms, ListBox);
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
	    Pid ! {update_ms, MSs},
	    MS = lists:nth(SelId+1, MSs),
	    wxDialog:destroy(Dialog),
	    MS;
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
    FixedFont = observer_wx:get_attrib({font, modern}),
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


%% find_and_format_ms(Selection, [ #match_spec{str_ms = Spec, alias = Alias, fun2ms = Fun} | T ]) ->
%%     case ((Selection =:= Spec) or (Selection =:= Alias)) or (Selection =:= Fun) of
%% 	true ->
%% 	    if Selection =:= Alias ->
%% 		    Spec;
%% 	       true ->
%% 		    Selection
%% 	    end;
%% 	false ->
%% 	    find_and_format_ms(Selection, T)
%%     end.

%% find_ms(_, []) ->
%%     {nomatch, #match_spec{}};
%% find_ms(Str, [ #match_spec{str_ms = Spec, alias = Alias, fun2ms = Fun} = MS | T ]) ->
%%     case ((Str =:= Spec) or (Str =:= Alias)) or (Str =:= Fun) of
%% 	true ->
%% 	    {match, MS};
%% 	false ->
%% 	    find_ms(Str, T)
%%     end.

%% apply_matchspec(MatchSpec, TracedDict, root) ->
%%     UpdateMS = fun(_Key, RecordList) ->
%% 		       [X#traced_func{match_spec = MatchSpec} || X <- RecordList]
%% 	       end,
%%     {ok, dict:map(UpdateMS, TracedDict)};
%% apply_matchspec(MatchSpec, TracedDict, {module, Module}) ->
%%     RecordList = dict:fetch(Module, TracedDict),
%%     RecordList2 = [X#traced_func{match_spec = MatchSpec} || X <- RecordList],
%%     {ok, dict:store(Module, RecordList2, TracedDict)};
%% apply_matchspec(MatchSpec, TracedDict, {function, Module, TracedFuncRec}) ->
%%     RecordList = dict:fetch(Module, TracedDict),
%%     NewFunc = TracedFuncRec#traced_func{match_spec = MatchSpec},
%%     RecordList2 = [NewFunc | [X || X <- RecordList, X =/= TracedFuncRec]],
%%     {NewFunc, dict:store(Module, RecordList2, TracedDict)}.

%% create_matchspec_page(Parent, MatchSpecs, UserData) ->
%%     Panel = wxPanel:new(Parent),
%%     MainSz = wxBoxSizer:new(?wxVERTICAL),
%%     TxtSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Match specification:"}]),
%%     BtnSz = wxBoxSizer:new(?wxHORIZONTAL),
%%     SavedSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Saved match specifications:"}]),

%%     TxtCtrl = create_styled_txtctrl(Panel),
%%     wxSizer:add(TxtSz, TxtCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),

%%     AddMsBtn = wxButton:new(Panel, ?MATCHPAGE_ADDMS, [{label, "Add"}]),
%%     AddMsAliasBtn = wxButton:new(Panel, ?MATCHPAGE_ADDMS_ALIAS, [{label, "Add with alias"}]),
%%     Fun2MSBtn = wxButton:new(Panel, ?MATCHPAGE_ADDFUN, [{label, "Add fun"}]),
%%     wxSizer:add(BtnSz, AddMsBtn),
%%     wxSizer:add(BtnSz, AddMsAliasBtn),
%%     wxSizer:add(BtnSz, Fun2MSBtn),

%%     Choices = show_ms_in_savedlistbox(MatchSpecs),
%%     SavedMSListBox = wxListBox:new(Panel, ?MATCHPAGE_LISTBOX, [{choices, Choices}]),
%%     wxSizer:add(SavedSz, SavedMSListBox, [{flag, ?wxEXPAND}, {proportion, 1}]),

%%     wxButton:connect(AddMsBtn, command_button_clicked, [{userData, UserData}]),
%%     wxButton:connect(AddMsAliasBtn, command_button_clicked, [{userData, UserData}] ),
%%     wxButton:connect(Fun2MSBtn, command_button_clicked, [{userData, UserData}] ),
%%     wxListBox:connect(SavedMSListBox, command_listbox_selected, [{userData, UserData}] ),
%%     wxSizer:add(MainSz, TxtSz, [{flag, ?wxEXPAND}, {proportion, 1}]),
%%     wxSizer:add(MainSz, BtnSz),
%%     wxSizer:add(MainSz, SavedSz, [{flag, ?wxEXPAND}, {proportion, 1}]),

%%     wxWindow:setSizer(Panel, MainSz),
%%     {Panel, MainSz, TxtCtrl, SavedMSListBox}.



%% update_tree(Tree, Dict) ->
%%     RootId = wxTreeCtrl:getRootItem(Tree),
%%     wxTreeCtrl:deleteChildren(Tree, RootId),

%%     FillTree = fun(KeyAtom, RecordList, acc_in) ->
%% 		       ParsedList = parse_record_function_names(RecordList),
%% 		       Module = wxTreeCtrl:appendItem(Tree, RootId, atom_to_list(KeyAtom)),
%% 		       lists:foldl(fun(TracedFuncRecord, N) ->
%% 					   FNameStr = lists:nth(N, ParsedList),
%% 					   wxTreeCtrl:appendItem(Tree, Module, FNameStr,
%% 								 [{data, TracedFuncRecord}]),
%% 					   N+1
%% 				   end,
%% 				   1, RecordList),
%% 		       wxTreeCtrl:sortChildren(Tree, Module),
%% 		       acc_in
%% 	       end,
%%     dict:fold(FillTree, acc_in, Dict),
%%     wxTreeCtrl:sortChildren(Tree, RootId),
%%     wxTreeCtrl:expand(Tree, RootId).




%% create_module_popup(Parent, ModuleName, TracedDict) ->
%%     Module = list_to_atom(ModuleName),
%%     Value = dict:find(Module, TracedDict),
%%     TracedModRecs =
%% 	case Value of
%% 	    {ok, V} ->
%% 		V;
%% 	    error ->
%% 		[]
%% 	end,
%%     Functions = Module:module_info(functions),
%%     Choices = lists:sort([{Name, Arity} || {Name, Arity} <- Functions, not(erl_internal:guard_bif(Name, Arity))]),
%%     ParsedChoices = parse_function_names(Choices),

%%     Dialog = wxDialog:new(Parent, ?MODULEPOPUP_DIALOG, ModuleName,
%% 			  [{style, ?wxDEFAULT_FRAME_STYLE}]),
%%     Panel = wxPanel:new(Dialog),
%%     MainSz = wxBoxSizer:new(?wxVERTICAL),

%%     SelBtnSz = wxBoxSizer:new(?wxHORIZONTAL),
%%     TxtCtrl = wxTextCtrl:new(Panel, ?MODULEPOPUP_TXTCTRL),
%%     SelBtn = wxButton:new(Panel, ?MODULEPOPUP_SELECT, [{label, "Select"}]),
%%     DeSelBtn = wxButton:new(Panel, ?MODULEPOPUP_SELECT, [{label, "Deselect"}]),
%%     SelAllBtn = wxButton:new(Panel, ?MODULEPOPUP_SELALL, [{label, "Select all"}]),
%%     DeSelAllBtn = wxButton:new(Panel, ?MODULEPOPUP_SELALL, [{label, "Deselect all"}]),
%%     CheckListBox = wxCheckListBox:new(Panel, ?MODULEPOPUP_CHECKLISTBOX, [{choices, ParsedChoices}, {style, ?wxLB_EXTENDED}]),
%%     Indices = find_index(TracedModRecs, Choices),
%%     lists:foreach(fun(X) ->  wxCheckListBox:check(CheckListBox, X) end, Indices),
%%     Selections = [wxControlWithItems:getString(CheckListBox, I) || I <- Indices],

%%     OKBtn = wxButton:new(Panel, ?wxID_OK, []),
%%     CancelBtn = wxButton:new(Panel, ?wxID_CANCEL, []),
%%     DialogBtnSz = wxStdDialogButtonSizer:new(),
%%     wxStdDialogButtonSizer:addButton(DialogBtnSz, OKBtn),
%%     wxStdDialogButtonSizer:addButton(DialogBtnSz, CancelBtn),
%%     wxStdDialogButtonSizer:realize(DialogBtnSz),

%%     wxSizer:add(SelBtnSz, SelBtn),
%%     wxSizer:add(SelBtnSz, DeSelBtn),
%%     wxSizer:add(SelBtnSz, SelAllBtn),
%%     wxSizer:add(SelBtnSz, DeSelAllBtn),
%%     wxSizer:add(MainSz, TxtCtrl, [{flag, ?wxEXPAND}]),
%%     wxSizer:add(MainSz, CheckListBox, [{flag, ?wxEXPAND}, {proportion, 1}]),
%%     wxSizer:add(MainSz, SelBtnSz, [{flag, ?wxEXPAND}]),
%%     wxSizer:add(MainSz, DialogBtnSz),
%%     wxWindow:setSizer(Panel, MainSz),

%%     wxButton:connect(SelBtn, command_button_clicked, [{userData, true}]),
%%     wxButton:connect(DeSelBtn, command_button_clicked, [{userData, false}]),
%%     wxButton:connect(SelAllBtn, command_button_clicked, [{userData, true}]),
%%     wxButton:connect(DeSelAllBtn, command_button_clicked, [{userData, false}]),
%%     wxButton:connect(OKBtn, command_button_clicked, [{userData, {module_popup, Module, ParsedChoices, Choices}}]),
%%     wxButton:connect(CancelBtn, command_button_clicked, [{userData, module_popup}]),
%%     wxTextCtrl:connect(TxtCtrl, command_text_updated, [{userData, ParsedChoices}]),
%%     wxCheckListBox:connect(CheckListBox, command_checklistbox_toggled),
%%     wxDialog:connect(Dialog, close_window),
%%     wxDialog:show(Dialog),
%%     {Dialog, CheckListBox, Selections}.

%% get_selections(Selections, FunctionList) ->
%%     get_selections(Selections, FunctionList, []).
%% get_selections([], _, Acc) ->
%%     Acc;
%% get_selections([Int|T], FuncList, Acc) ->
%%     get_selections(T, FuncList, [lists:nth(Int, FuncList) | Acc]).

%% find_index(Selections, FunctionList) ->
%%     find_index(Selections, FunctionList, 1, []).
%% find_index(Selections, FunctionList, N, Acc) when N > length(FunctionList); Selections =:= [] ->
%%     Acc;

%% find_index([#traced_func{func_name = Name, arity = Arity} |STail] = Selections,
%% 	   FunctionList, N, Acc) ->
%%     {Fname, A} = lists:nth(N, FunctionList),
%%     case (Fname =:= Name) and (A =:= Arity) of
%% 	true ->
%% 	    find_index(STail, FunctionList, 1, [N-1|Acc]);
%% 	false ->
%% 	    find_index(Selections, FunctionList, N+1, Acc)
%%     end;

%% find_index([Sel|STail] = Selections, FunctionList, N, Acc) when is_list(Sel) ->
%%     case lists:nth(N, FunctionList) =:= Sel of
%% 	true ->
%% 	    find_index(STail, FunctionList, 1, [N-1|Acc]);
%% 	false ->
%% 	    find_index(Selections, FunctionList, N+1, Acc)
%%     end.

%% atomlist_to_stringlist(Modules) ->
%%     [atom_to_list(X) || X <- Modules].

ensure_last_is_dot([]) ->
    ".";
ensure_last_is_dot(String) ->
    case lists:last(String) =:= $. of
	true ->
	    String;
	false ->
	    String ++ "."
    end.


%% dbg_from_string(Str0) ->
%%     Str = unicode:characters_to_list(Str0),
%%     case erl_scan:string(Str) of
%% 	{ok, Tokens,_} ->
%% 	    case erl_parse:parse_exprs(Tokens) of
%% 		{ok,[{'fun',_,{clauses, Cl}}]} ->
%% 		    case ms_transform:
%% 			transform_from_shell(dbg,Cl,orddict:new()) of
%% 			{error, [{_,[{Line,ms_transform,Info}]}],_} ->
%% 			    {error,{Line,ms_transform,Info}};
%% 			{error, _} = ET1 ->
%% 			    ET1;
%% 			Else ->
%% 			    {ok, Else, "[" ++ lists:flatten(io_lib:format("~p", Else)) ++ "]"}
%% 		    end;
%% 		{ok,_} ->
%% 		    {error, {1,ms_transform,1}};
%% 		{error,Reason} ->
%% 		    {error,Reason}
%% 	    end;
%% 	{error,Reason2,_} ->
%% 	    {error,Reason2}
%%     end.

%% get_correct_matchspec_components(From, State) ->
%%     case From of
%% 	matchpage ->
%% 	    {State#traceopts_state.matchpage_styled_txtctrl,
%% 	     State#traceopts_state.frame};
%% 	matchpopup ->
%% 	    {State#traceopts_state.matchspec_popup_styled_txtctrl,
%% 	     State#traceopts_state.matchspec_popup_dialog}
%%     end.



%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 						%Trace option window

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% All pages

%% handle_event(#wx{id = ?wxID_OK,
%% 		 event = #wxCommand{type = command_button_clicked},
%% 		 userData = trace_options},
%% 	     #traceopts_state{boxes = Boxes,
%% 			      trace_options = TraceOpts,
%% 			      match_specs = MatchSpecs,
%% 			      traced_funcs = TracedFuncs,
%% 			      parent = Parent} = State) ->
%%     UpdTraceOpts = wx:batch(fun() ->
%% 				    read_trace_boxes(Boxes, TraceOpts)
%% 			    end),
%%     Parent ! {updated_traceopts,
%% 	      UpdTraceOpts,
%% 	      MatchSpecs,
%% 	      TracedFuncs},
%%     {stop, shutdown, State};

%% handle_event(#wx{id = ?wxID_CANCEL,
%% 		 event = #wxCommand{type = command_button_clicked},
%% 		 userData = trace_options},
%% 	     #traceopts_state{parent = Parent} = State) ->
%%     Parent ! traceopts_closed,
%%     {stop, shutdown, State};

%% handle_event(#wx{id = ?TRACEOPTS_FRAME,
%% 		 event = #wxClose{type = close_window}},
%% 	     #traceopts_state{parent = Parent} = State) ->
%%     Parent ! traceopts_closed,
%%     {stop, shutdown, State};


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Page - Tracing

%% handle_event(#wx{event = #wxCommand{type = command_checkbox_clicked}, userData = Boxgroup},
%% 	     State) ->
%%     enable(Boxgroup),
%%     {noreply, State};


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Page - Functions

%% handle_event(#wx{id = ?FUNCTIONPAGE_LISTBOX,
%% 		 event = #wxCommand{type = command_listbox_doubleclicked,
%% 				    cmdString = ChosenModule}},
%% 	     #traceopts_state{frame = Frame,
%% 			      traced_funcs = TracedDict,
%% 			      popup_open = false} = State) ->
%%     {Dialog, CheckListBox, CheckedFuncs} = create_module_popup(Frame, ChosenModule, TracedDict),
%%     {noreply, State#traceopts_state{popup_open = true,
%% 				    module_popup_dialog = Dialog,
%% 				    module_popup_checklistbox = CheckListBox,
%% 				    checked_funcs = CheckedFuncs}};

%% handle_event(#wx{id = ?FUNCTIONPAGE_TXTCTRL,
%% 		 event = #wxCommand{type = command_text_updated,
%% 				    cmdString = Input},
%% 		 userData = Data},
%% 	     #traceopts_state{functionpage_listbox = ListBox} = State) ->
%%     filter_listbox_data(Input, Data, ListBox),
%%     {noreply, State};

%% handle_event(#wx{event = #wxTree{type = command_tree_item_activated,
%% 				 item = Item}},
%% 	     #traceopts_state{frame = Frame,
%% 			      match_specs = MatchSpecs,
%% 			      popup_open = false} = State) ->

%%     Dialog = wxDialog:new(Frame, ?MATCH_POPUP_DIALOG, "Match specification",
%% 			  [{style, ?wxDEFAULT_FRAME_STYLE}]),
%%     {MatchPanel, MatchSz, StyledTxtCtrl, ListBox} = create_matchspec_page(Dialog, MatchSpecs, matchpopup),
%%     ApplyBtn = wxButton:new(MatchPanel, ?wxID_APPLY),
%%     CancelBtn = wxButton:new(MatchPanel, ?wxID_CANCEL, []),
%%     wxButton:connect(ApplyBtn, command_button_clicked, [{userData, Item}]),
%%     wxButton:connect(CancelBtn, command_button_clicked, [{userData, matchspec_popup}]),
%%     DialogBtnSz = wxStdDialogButtonSizer:new(),
%%     wxStdDialogButtonSizer:addButton(DialogBtnSz, ApplyBtn),
%%     wxStdDialogButtonSizer:addButton(DialogBtnSz, CancelBtn),
%%     wxStdDialogButtonSizer:realize(DialogBtnSz),
%%     wxSizer:add(MatchSz, DialogBtnSz),

%%     wxDialog:connect(Dialog, close_window),
%%     wxDialog:show(Dialog),
%%     {noreply, State#traceopts_state{matchspec_popup_dialog = Dialog,
%% 				    matchspec_popup_listbox = ListBox,
%% 				    matchspec_popup_styled_txtctrl = StyledTxtCtrl,
%% 				    popup_open = true}};

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Page - Match specs

%% handle_event(#wx{event = #wxCommand{type = command_listbox_selected,
%% 				    cmdString = Txt}},
%% 	     State) when Txt =:= [] ->
%%     {noreply, State};

%% handle_event(#wx{id = ?MATCHPAGE_LISTBOX,
%% 		 event = #wxCommand{type = command_listbox_selected,
%% 				    cmdString = SavedTxt},
%% 		 userData = From},
%% 	     #traceopts_state{match_specs = MatchSpecs} = State) ->
%%     {StyledTxtCtrl, _} = get_correct_matchspec_components(From, State),
%%     MsOrFun = find_and_format_ms(SavedTxt, MatchSpecs),
%%     wxStyledTextCtrl:setText(StyledTxtCtrl, MsOrFun),
%%     {noreply, State};

%% handle_event(#wx{id = ?MATCHPAGE_ADDFUN,
%% 		 event = #wxCommand{type = command_button_clicked},
%% 		 userData = From},
%% 	     #traceopts_state{match_specs = MatchSpecs,
%% 			      matchpage_listbox = PageListBox,
%% 			      matchspec_popup_listbox = PopupListBox} = State) ->

%%     {StyledTxtCtrl, Frame} = get_correct_matchspec_components(From, State),
%%     StrFun = ensure_last_is_dot(wxStyledTextCtrl:getText(StyledTxtCtrl)),

%%     MatchSpecs2 = case dbg_from_string(StrFun) of
%% 		      {ok, TermMS, StrMS} ->
%% 			  FunMS = #match_spec{str_ms = StrMS, term_ms = TermMS, fun2ms = StrFun},
%% 			  case lists:member(FunMS, MatchSpecs) of
%% 			      true ->
%% 				  observer_wx:create_txt_dialog(Frame, StrFun ++ "\nalready exists",
%% 								"Error", ?wxICON_ERROR),
%% 				  MatchSpecs;
%% 			      false ->
%% 				  wxStyledTextCtrl:setText(StyledTxtCtrl, StrMS),
%% 				  update_matchspec_listbox(StrFun, {PopupListBox, PageListBox}, From),
%% 				  lists:reverse([FunMS | MatchSpecs])
%% 			  end;
%% 		      {error, {_, Module, What}} ->
%% 			  FailMsg = Module:format_error(What),
%% 			  observer_wx:create_txt_dialog(Frame, FailMsg, "Error", ?wxICON_ERROR),
%% 			  MatchSpecs
%% 		  end,
%%     {noreply, State#traceopts_state{match_specs = MatchSpecs2}};

%% handle_event(#wx{id = ?MATCHPAGE_ADDMS,
%% 		 event = #wxCommand{type = command_button_clicked},
%% 		 userData = From},
%% 	     #traceopts_state{match_specs = MatchSpecs,
%% 			      matchpage_listbox = PageListBox,
%% 			      matchspec_popup_listbox = PopupListBox} = State) ->

%%     {StyledTxtCtrl, Frame} = get_correct_matchspec_components(From, State),
%%     StrMS = ensure_last_is_dot(wxStyledTextCtrl:getText(StyledTxtCtrl)),
%%     MatchSpecs2 = case check_correct_MS(StrMS) of
%% 		      {true, TermMS} ->
%% 			  MS = #match_spec{str_ms = StrMS, term_ms = TermMS},
%% 			  case lists:member(MS, MatchSpecs) of
%% 			      true ->
%% 				  observer_wx:create_txt_dialog(Frame, StrMS ++ "\nalready exists",
%% 								"Error", ?wxICON_ERROR),
%% 				  MatchSpecs;
%% 			      false ->
%% 				  update_matchspec_listbox(StrMS, {PopupListBox, PageListBox}, From),
%% 				  lists:reverse([MS | MatchSpecs])
%% 			  end;
%% 		      {false, Reason} ->
%% 			  observer_wx:create_txt_dialog(Frame, Reason, "Error", ?wxICON_ERROR),
%% 			  MatchSpecs
%% 		  end,
%%     {noreply, State#traceopts_state{match_specs = MatchSpecs2}};


%% handle_event(#wx{id = ?MATCHPAGE_ADDMS_ALIAS,
%% 		 event = #wxCommand{type = command_button_clicked},
%% 		 userData = From},
%% 	     #traceopts_state{match_specs = MatchSpecs,
%% 			      matchpage_listbox = PageListBox,
%% 			      matchspec_popup_listbox = PopupListBox} = State) ->

%%     {StyledTxtCtrl, Frame} = get_correct_matchspec_components(From, State),
%%     StrMS = ensure_last_is_dot(wxStyledTextCtrl:getText(StyledTxtCtrl)),

%%     MatchSpecs2 = case check_correct_MS(StrMS) of
%% 		      {true, TermMS} ->
%% 			  Dialog = wxTextEntryDialog:new(Frame, "Enter ms alias: "),
%% 			  Alias = case wxDialog:showModal(Dialog) of
%% 				      ?wxID_OK ->
%% 					  wxTextEntryDialog:getValue(Dialog);
%% 				      ?wxID_CANCEL ->
%% 					  ""
%% 				  end,
%% 			  wxDialog:destroy(Dialog),

%% 			  case Alias of
%% 			      "" ->
%% 				  observer_wx:create_txt_dialog(Frame, "Bad alias", "Syntax error",
%% 								?wxICON_ERROR),
%% 				  MatchSpecs;

%% 			      _ ->
%% 				  MS = #match_spec{alias = Alias, str_ms = StrMS,
%% 						   term_ms = TermMS},
%% 				  {OccupiedAlias, _} = find_ms(Alias, MatchSpecs),

%% 				  if
%% 				      OccupiedAlias =:= match ->
%% 					  observer_wx:create_txt_dialog(Frame, "Alias " ++ Alias ++ " already exists",
%% 									"Error", ?wxICON_ERROR),
%% 					  MatchSpecs;
%% 				      true ->
%% 					  update_matchspec_listbox(Alias, {PopupListBox, PageListBox}, From),
%% 					  lists:reverse([MS | MatchSpecs])
%% 				  end
%% 			  end;
%% 		      {false, Reason} ->
%% 			  observer_wx:create_txt_dialog(Frame, Reason, "Error", ?wxICON_ERROR),
%% 			  MatchSpecs
%% 		  end,
%%     {noreply, State#traceopts_state{match_specs = MatchSpecs2}};


%% handle_event(#wx{id = ?wxID_APPLY,
%% 		 event = #wxCommand{type = command_button_clicked},
%% 		 userData = Item},
%% 	     #traceopts_state{matchspec_popup_dialog = Dialog,
%% 			      matchspec_popup_listbox = ListBox,
%% 			      tree = Tree,
%% 			      match_specs = MatchSpecs,
%% 			      traced_funcs = TracedDict} = State) ->
%%     IntSelection = wxListBox:getSelection(ListBox),
%%     StrSelection =
%% 	case IntSelection >= 0 of
%% 	    true ->
%% 		wxControlWithItems:getString(ListBox, IntSelection);
%% 	    false ->
%% 		[]
%% 	end,
%%     {_, MS} = find_ms(StrSelection, MatchSpecs),
%%     RootId = wxTreeCtrl:getRootItem(Tree),
%%     ItemParent = wxTreeCtrl:getItemParent(Tree, Item),

%%     TracedDict2 =
%% 	if (Item =:= RootId) ->
%% 		{ok, NewDict} = apply_matchspec(MS, TracedDict, root),
%% 		NewDict;
%% 	   (ItemParent =:= RootId) ->
%% 		Module = list_to_atom(wxTreeCtrl:getItemText(Tree, Item)),
%% 		{ok, NewDict} = apply_matchspec(MS, TracedDict, {module, Module}),
%% 		NewDict;
%% 	   true ->
%% 		TracedFuncRec = wxTreeCtrl:getItemData(Tree, Item),
%% 		Module = list_to_atom(wxTreeCtrl:getItemText(Tree, ItemParent)),
%% 		{NewTracedFuncRecord, NewDict} =
%% 		    apply_matchspec(MS,
%% 				    TracedDict,
%% 				    {function,
%% 				     Module,
%% 				     TracedFuncRec}),
%% 		wxTreeCtrl:setItemData(Tree, Item, NewTracedFuncRecord),
%% 		NewDict
%% 	end,
%%     wxDialog:destroy(Dialog),
%%     {noreply, State#traceopts_state{traced_funcs = TracedDict2,
%% 				    popup_open = false}};

%% handle_event(#wx{id = ?wxID_CANCEL,
%% 		 event = #wxCommand{type = command_button_clicked},
%% 		 userData = matchspec_popup},
%% 	     #traceopts_state{matchspec_popup_dialog = Dialog} = State) ->
%%     wxDialog:destroy(Dialog),
%%     {noreply, State#traceopts_state{popup_open = false}};

%% handle_event(#wx{id = ?MATCH_POPUP_DIALOG,
%% 		 event = #wxClose{type = close_window}},
%% 	     #traceopts_state{matchspec_popup_dialog = Dialog} = State) ->
%%     wxDialog:destroy(Dialog),
%%     {noreply, State#traceopts_state{popup_open = false}};

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 						%Module Popup

%% handle_event(#wx{id = ?wxID_OK,
%% 		 event = #wxCommand{type = command_button_clicked},
%% 		 userData = {module_popup, Module,
%% 			     ParsedChoices, Choices}},
%% 	     #traceopts_state{
%% 		  module_popup_dialog = Dialog,
%% 		  traced_funcs = TracedDict,
%% 		  tree = Tree,
%% 		  checked_funcs = CheckedFuncs} = State) ->

%%     Indices = [I+1 || I <- find_index(CheckedFuncs, ParsedChoices)],
%%     Selections = get_selections(Indices, Choices),
%%     TracedDict2 = case Selections of
%% 		      [] ->
%% 			  dict:erase(Module, TracedDict);
%% 		      _ ->
%% 			  Traced = [#traced_func{arity = Arity,
%% 						 func_name = Function}
%% 				    || {Function, Arity} <- Selections],
%% 			  dict:store(Module, Traced, TracedDict)
%% 		  end,

%%     update_tree(Tree, TracedDict2),
%%     wxDialog:destroy(Dialog),
%%     {noreply, State#traceopts_state{traced_funcs = TracedDict2,
%% 				    checked_funcs = [],
%% 				    popup_open = false}};

%% handle_event(#wx{id = ?wxID_CANCEL,
%% 		 event = #wxCommand{type = command_button_clicked},
%% 		 userData = module_popup},
%% 	     #traceopts_state{module_popup_dialog = Dialog} = State) ->
%%     wxDialog:destroy(Dialog),
%%     {noreply, State#traceopts_state{popup_open = false,
%% 				    checked_funcs = []}};

%% handle_event(#wx{id = ?MODULEPOPUP_SELECT,
%% 		 event = #wxCommand{type = command_button_clicked},
%% 		 userData = Bool},
%% 	     #traceopts_state{module_popup_checklistbox = CheckListBox,
%% 			      checked_funcs = CheckedFuncs} = State) ->
%%     {_, Selections} = wxListBox:getSelections(CheckListBox),
%%     lists:foreach(fun(Index) -> wxCheckListBox:check(CheckListBox, Index, [{check, Bool}]) end, Selections),
%%     StrSelections = [wxControlWithItems:getString(CheckListBox, N) || N <- Selections],
%%     CheckedFuncs2 = case Bool of
%% 			true ->
%% 			    [X || X <- StrSelections,
%% 				  not(lists:member(X, CheckedFuncs))] ++ CheckedFuncs;
%% 			false ->
%% 			    CheckedFuncs -- StrSelections
%% 		    end,
%%     {noreply, State#traceopts_state{checked_funcs = CheckedFuncs2}};

%% handle_event(#wx{id = ?MODULEPOPUP_SELALL,
%% 		 event = #wxCommand{type = command_button_clicked},
%% 		 userData = Bool},
%% 	     #traceopts_state{module_popup_checklistbox = CheckListBox} = State) ->
%%     lists:foreach(fun(Index) ->
%% 			  wxCheckListBox:check(CheckListBox, Index, [{check, Bool}])
%% 		  end,
%% 		  lists:seq(0, wxControlWithItems:getCount(CheckListBox))),
%%     CheckedFuncs = case Bool of
%% 		       true ->
%% 			   [wxControlWithItems:getString(CheckListBox, N)
%% 			    || N <- lists:seq(0, wxControlWithItems:getCount(CheckListBox))];
%% 		       false ->
%% 			   []
%% 		   end,
%%     {noreply, State#traceopts_state{checked_funcs = CheckedFuncs}};

%% handle_event(#wx{id = ?MODULEPOPUP_CHECKLISTBOX,
%% 		 obj = CheckListBox,
%% 		 event = #wxCommand{type = command_checklistbox_toggled,
%% 				    commandInt = Index}},
%% 	     #traceopts_state{checked_funcs = CheckedFuncs} = State) ->

%%     UpdCheckedFuncs = case
%% 			  wxCheckListBox:isChecked(CheckListBox, Index) of
%% 			  true ->
%% 			      [wxControlWithItems:getString(CheckListBox, Index) | CheckedFuncs];
%% 			  false ->
%% 			      lists:delete(wxControlWithItems:getString(CheckListBox, Index), CheckedFuncs)
%% 		      end,
%%     {noreply, State#traceopts_state{checked_funcs = UpdCheckedFuncs}};

%% handle_event(#wx{id = ?MODULEPOPUP_TXTCTRL,
%% 		 event = #wxCommand{type = command_text_updated,
%% 				    cmdString = Input},
%% 		 userData = Data},
%% 	     #traceopts_state{module_popup_checklistbox = CListBox,
%% 			      checked_funcs = CheckedFuncs} = State) ->
%%     FilteredData = filter_listbox_data(Input, Data, CListBox),
%%     lists:foreach(fun(Index) ->
%% 			  wxCheckListBox:check(CListBox, Index, [{check, true}])
%% 		  end,
%% 		  [wxControlWithItems:findString(CListBox, X) || X <- CheckedFuncs, lists:member(X, FilteredData)]),
%%     {noreply, State};

%% handle_event(#wx{id = ?MODULEPOPUP_DIALOG,
%% 		 event = #wxClose{type = close_window}},
%% 	     #traceopts_state{module_popup_dialog = Dialog} = State) ->
%%     wxDialog:destroy(Dialog),
%%     {noreply, State#traceopts_state{popup_open = false,
%% 				    checked_funcs = []}};

%% handle_event(#wx{event = What}, State) ->
%%     io:format("~p~p: Unhandled event: ~p ~n", [?MODULE, self(), What]),
%%     {noreply, State}.



%% terminate(Reason, #traceopts_state{frame = Frame}) ->
%%     io:format("~p terminating traceopts. Reason: ~p~n", [?MODULE, Reason]),
%%     wxFrame:destroy(Frame),
%%     ok.

%% code_change(_, _, State) ->
%%     {stop, not_yet_implemented, State}.

%% handle_info(Any, State) ->
%%     io:format("~p~p: received unexpected message: ~p\n", [?MODULE, self(), Any]),
%%     {noreply, State}.

%% handle_call(Msg, _From, State) ->
%%     io:format("~p~p: Got Call ~p~n",[?MODULE, ?LINE, Msg]),
%%     {reply, ok, State}.

%% handle_cast(Msg, State) ->
%%     io:format("~p ~p: Unhandled cast ~p~n", [?MODULE, ?LINE, Msg]),
%%     {noreply, State}.
