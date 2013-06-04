%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2013. All Rights Reserved.
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

-module(observer_lib).

-export([get_wx_parent/1,
	 display_info_dialog/1, user_term/3, user_term_multiline/3,
	 interval_dialog/4, start_timer/1, stop_timer/1,
	 display_info/2, fill_info/2, update_info/2, to_str/1,
	 create_menus/3, create_menu_item/3,
	 create_attrs/0,
	 set_listctrl_col_size/2
	]).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

get_wx_parent(Window) ->
    Parent = wxWindow:getParent(Window),
    case wx:is_null(Parent) of
	true -> Window;
	false -> get_wx_parent(Parent)
    end.

interval_dialog(Parent0, {Timer, Value}, Min, Max) ->
    Parent = get_wx_parent(Parent0),
    Dialog = wxDialog:new(Parent, ?wxID_ANY, "Update Interval",
			  [{style, ?wxDEFAULT_DIALOG_STYLE bor
				?wxRESIZE_BORDER}]),
    Panel = wxPanel:new(Dialog),
    Check = wxCheckBox:new(Panel, ?wxID_ANY, "Periodical refresh"),
    wxCheckBox:setValue(Check, Timer /= false),
    Style = ?wxSL_HORIZONTAL bor ?wxSL_AUTOTICKS bor ?wxSL_LABELS,
    Slider = wxSlider:new(Panel, ?wxID_ANY, Value, Min, Max,
			  [{style, Style}, {size, {200, -1}}]),
    wxWindow:enable(Slider, [{enable, Timer /= false}]),
    InnerSizer = wxBoxSizer:new(?wxVERTICAL),
    Buttons = wxDialog:createButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),
    Flags = [{flag, ?wxEXPAND bor ?wxALL}, {border, 2}],
    wxSizer:add(InnerSizer, Check,  Flags),
    wxSizer:add(InnerSizer, Slider, Flags),
    wxPanel:setSizer(Panel, InnerSizer),
    TopSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(TopSizer, Panel, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(TopSizer, Buttons, [{flag, ?wxEXPAND}]),
    wxWindow:setSizerAndFit(Dialog, TopSizer),
    wxSizer:setSizeHints(TopSizer, Dialog),
    wxCheckBox:connect(Check, command_checkbox_clicked,
		       [{callback, fun(#wx{event=#wxCommand{commandInt=Enable0}},_) ->
					   Enable = Enable0 > 0,
					   wxWindow:enable(Slider, [{enable, Enable}])
				   end}]),
    Res = case wxDialog:showModal(Dialog) of
	      ?wxID_OK ->
		  Enabled = wxCheckBox:isChecked(Check),
		  setup_timer(Enabled, {Timer, wxSlider:getValue(Slider)});
	      ?wxID_CANCEL ->
		  {Timer, Value}
	  end,
    wxDialog:destroy(Dialog),
    Res.

stop_timer(Timer = {false, _}) -> Timer;
stop_timer(Timer = {true, _}) -> Timer;
stop_timer(Timer = {_, Intv}) ->
    setup_timer(false, Timer),
    {true, Intv}.
start_timer(Intv) when is_integer(Intv) ->
    setup_timer(true, {true, Intv});
start_timer(Timer) ->
    setup_timer(true, Timer).

setup_timer(false, {Timer, Value})
  when is_boolean(Timer) ->
    {false, Value};
setup_timer(true,  {false, Value}) ->
    {ok, Timer} = timer:send_interval(Value * 1000, refresh_interval),
    {Timer, Value};
setup_timer(Bool, {Timer, Old}) ->
    timer:cancel(Timer),
    setup_timer(Bool, {false, Old}).

display_info_dialog(Str) ->
    Dlg = wxMessageDialog:new(wx:null(), Str),
    wxMessageDialog:showModal(Dlg),
    wxMessageDialog:destroy(Dlg),
    ok.

%% display_info(Parent, [{Title, [{Label, Info}]}]) -> {Panel, Sizer, InfoFieldsToUpdate}
display_info(Frame, Info) ->
    Panel = wxPanel:new(Frame),
    wxWindow:setBackgroundColour(Panel, {255,255,255}),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:addSpacer(Sizer, 5),
    Add = fun(BoxInfo) ->
		  {Box, InfoFs} = create_box(Panel, BoxInfo),
		  wxSizer:add(Sizer, Box, [{flag, ?wxEXPAND bor ?wxALL},
					   {border, 5}]),
		  wxSizer:addSpacer(Sizer, 5),
		  InfoFs
	  end,
    InfoFs = [Add(I) || I <- Info],
    wxWindow:setSizerAndFit(Panel, Sizer),
    {Panel, Sizer, InfoFs}.

fill_info([{Str, Key}|Rest], Data) when is_atom(Key); is_function(Key) ->
    [{Str, get_value(Key, Data)} | fill_info(Rest, Data)];
fill_info([{Str, {Format, Key}}|Rest], Data)
  when is_atom(Key); is_function(Key), is_atom(Format) ->
    case get_value(Key, Data) of
	undefined -> [{Str, undefined} | fill_info(Rest, Data)];
	Value -> [{Str, {Format, Value}} | fill_info(Rest, Data)]
    end;
fill_info([{Str,SubStructure}|Rest], Data) when is_list(SubStructure) ->
    [{Str, fill_info(SubStructure, Data)}|fill_info(Rest,Data)];
fill_info([{Str,Attrib,SubStructure}|Rest], Data) ->
    [{Str, Attrib, fill_info(SubStructure, Data)}|fill_info(Rest,Data)];
fill_info([], _) -> [].

get_value(Key, Data) when is_atom(Key) ->
    proplists:get_value(Key,Data);
get_value(Fun, Data) when is_function(Fun) ->
    Fun(Data).

update_info([Fields|Fs], [{_Header, SubStructure}| Rest]) ->
    update_info2(Fields, SubStructure),
    update_info(Fs, Rest);
update_info([Fields|Fs], [{_Header, _Attrib, SubStructure}| Rest]) ->
    update_info2(Fields, SubStructure),
    update_info(Fs, Rest);
update_info([], []) ->
    ok.

update_info2([Field|Fs], [{_Str, Value}|Rest]) ->
    wxStaticText:setLabel(Field, to_str(Value)),
    update_info2(Fs, Rest);
update_info2([], []) -> ok.


to_str(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_str({bytes, B}) ->
    KB = B div 1024,
    MB = KB div 1024,
    GB = MB div 1024,
    if
	GB > 10 -> integer_to_list(GB) ++ " gB";
	MB > 10 -> integer_to_list(MB) ++ " mB";
	KB >  0 -> integer_to_list(KB) ++ " kB";
	true -> integer_to_list(B) ++ " B "
    end;
to_str({time_ms, MS}) ->
    S = MS div 1000,
    Min = S div 60,
    Hours = Min div 60,
    Days = Hours div 24,
    if
	Days > 0 -> integer_to_list(Days) ++ " Days";
	Hours > 0 -> integer_to_list(Hours) ++ " Hours";
	Min > 0 -> integer_to_list(Min) ++ " Mins";
	true -> integer_to_list(S) ++ " Secs"
    end;

to_str({func, {F,A}}) when is_atom(F), is_integer(A) ->
    lists:concat([F, "/", A]);
to_str({func, {F,'_'}}) when is_atom(F) ->
    atom_to_list(F);
to_str({A, B}) when is_atom(A), is_atom(B) ->
    lists:concat([A, ":", B]);
to_str({M,F,A}) when is_atom(M), is_atom(F), is_integer(A) ->
    lists:concat([M, ":", F, "/", A]);
to_str(Value) when is_list(Value) ->
    case lists:all(fun(X) -> is_integer(X) end, Value) of
	true -> Value;
	false ->
	    lists:foldl(fun(X, Acc) ->
				to_str(X) ++ " " ++ Acc end,
			"", Value)
    end;
to_str(Port) when is_port(Port) ->
    erlang:port_to_list(Port);
to_str(Pid) when is_pid(Pid) ->
    pid_to_list(Pid);
to_str(No) when is_integer(No) ->
    integer_to_list(No);
to_str(Term) ->
    io_lib:format("~w", [Term]).

create_menus([], _MenuBar, _Type) -> ok;
create_menus(Menus, MenuBar, Type) ->
    Add = fun({Tag, Ms}, Index) ->
		  create_menu(Tag, Ms, Index, MenuBar, Type)
	  end,
    [{First, _}|_] = Menus,
    OnMac = os:type() =:= {unix, darwin},
    Index = if Type =:= default -> 0;
	       First =:= "File" -> 0;
	       OnMac -> 0;
	       true -> 1
	    end,
    wx:foldl(Add, Index, Menus),
    ok.

create_menu("File", MenuItems, Index, MenuBar, Type) ->
    OnMac = os:type() =:= {unix, darwin},
    if OnMac, Type =:= default ->
	    Index;
       not OnMac, Type =:= plugin ->
	    MenuId = wxMenuBar:findMenu(MenuBar, "File"),
	    Menu = wxMenuBar:getMenu(MenuBar, MenuId),
	    lists:foldl(fun(Record, N) ->
				create_menu_item(Record, Menu, N)
			end, 0, MenuItems),
	    Index + 1;
       true ->
	    Menu = wxMenu:new(),
	    lists:foldl(fun(Record, N) ->
				create_menu_item(Record, Menu, N)
			end, 0, MenuItems),
	    wxMenuBar:insert(MenuBar, Index, Menu, "File"),
	    Index+1
    end;
create_menu(Name, MenuItems, Index, MenuBar, _Type) ->
    Menu = wxMenu:new(),
    lists:foldl(fun(Record, N) ->
			create_menu_item(Record, Menu, N)
		end, 0, MenuItems),
    wxMenuBar:insert(MenuBar, Index, Menu, Name),
    Index+1.

create_menu_item(#create_menu{id = ?wxID_HELP=Id}, Menu, Index) ->
    wxMenu:insert(Menu, Index, Id),
    Index+1;
create_menu_item(#create_menu{id=Id, text=Text, help=Help, type=Type, check=Check},
		 Menu, Index) ->
    Opts = case Help of
	       [] -> [];
	       _ -> [{help, Help}]
	   end,
    case Type of
	append ->
	    wxMenu:insert(Menu, Index, Id,
			  [{text, Text}|Opts]);
	check ->
	    wxMenu:insertCheckItem(Menu, Index, Id, Text, Opts),
	    wxMenu:check(Menu, Id, Check);
	radio ->
	    wxMenu:insertRadioItem(Menu, Index, Id, Text, Opts),
	    wxMenu:check(Menu, Id, Check);
	separator ->
	    wxMenu:insertSeparator(Menu, Index)
    end,
    Index+1;
create_menu_item(separator, Menu, Index) ->
    wxMenu:insertSeparator(Menu, Index),
    Index+1.

create_attrs() ->
    Font = wxSystemSettings:getFont(?wxSYS_DEFAULT_GUI_FONT),
    Text = case wxSystemSettings:getColour(?wxSYS_COLOUR_LISTBOXTEXT) of
	       {255,255,255,_} -> {10,10,10};  %% Is white on Mac for some reason
	       Color -> Color
	   end,
    #attrs{even = wxListItemAttr:new(Text, ?BG_EVEN, Font),
	   odd  = wxListItemAttr:new(Text, ?BG_ODD, Font),
	   deleted = wxListItemAttr:new(?FG_DELETED, ?BG_DELETED, Font),
	   changed = wxListItemAttr:new(Text, ?BG_CHANGED, Font),
	   searched = wxListItemAttr:new(Text, ?BG_SEARCHED, Font)
	  }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_box_info({Title, List}) when is_list(List) -> {Title, ?wxALIGN_LEFT, List};
get_box_info({Title, left, List}) -> {Title, ?wxALIGN_LEFT, List};
get_box_info({Title, right, List}) -> {Title, ?wxALIGN_RIGHT, List}.

create_box(Panel, Data) ->
    {Title, Align, Info} = get_box_info(Data),
    Box = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, Title}]),
    Left  = wxBoxSizer:new(?wxVERTICAL),
    Right = wxBoxSizer:new(?wxVERTICAL),
    Expand    = [{flag, ?wxEXPAND}],
    ExpAlign  = [{flag, Align}],
    AddRow = fun({Desc, Value}) ->
		     wxSizer:add(Left, wxStaticText:new(Panel, ?wxID_ANY, Desc ++ ":"), Expand),
		     Field = wxStaticText:new(Panel, ?wxID_ANY, to_str(Value)),
		     wxSizer:add(Right, Field, ExpAlign),
		     Field
	     end,
    InfoFields = [AddRow(Entry) || Entry <- Info],
    wxSizer:add(Box, Left),
    wxSizer:addSpacer(Box, 10),
    wxSizer:add(Box, Right),
    wxSizer:addSpacer(Box, 30),
    {Box, InfoFields}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_listctrl_col_size(LCtrl, Total) ->
    wx:batch(fun() -> calc_last(LCtrl, Total) end).

calc_last(LCtrl, _Total) ->
    Cols = wxListCtrl:getColumnCount(LCtrl),
    {Total, _} = wxWindow:getClientSize(LCtrl),
    SBSize = scroll_size(LCtrl),
    Last = lists:foldl(fun(I, Last) ->
			       Last - wxListCtrl:getColumnWidth(LCtrl, I)
		       end, Total-SBSize, lists:seq(0, Cols - 2)),
    Size = max(150, Last),
    wxListCtrl:setColumnWidth(LCtrl, Cols-1, Size).

scroll_size(LCtrl) ->
    case os:type() of
	{win32, nt} -> 0;
	{unix, darwin} ->
	    %% I can't figure out is there is a visible scrollbar
	    %% Always make room for it
	    wxSystemSettings:getMetric(?wxSYS_VSCROLL_X);
	_ ->
	    case wxWindow:hasScrollbar(LCtrl, ?wxVERTICAL) of
		true -> wxSystemSettings:getMetric(?wxSYS_VSCROLL_X);
		false -> 0
	    end
    end.


user_term(Parent, Title, Default) ->
    Dialog = wxTextEntryDialog:new(Parent, Title, [{value, Default}]),
    case wxTextEntryDialog:showModal(Dialog) of
	?wxID_OK ->
	    Str = wxTextEntryDialog:getValue(Dialog),
	    wxTextEntryDialog:destroy(Dialog),
	    parse_string(ensure_last_is_dot(Str));
	?wxID_CANCEL ->
	    wxTextEntryDialog:destroy(Dialog),
	    cancel
    end.

user_term_multiline(Parent, Title, Default) ->
    Dialog = wxDialog:new(Parent, ?wxID_ANY, Title,
			  [{style, ?wxDEFAULT_DIALOG_STYLE bor
			    ?wxRESIZE_BORDER}]),
    Panel = wxPanel:new(Dialog),

    TextCtrl = wxTextCtrl:new(Panel, ?wxID_ANY,
			      [{value, Default},
			       {style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
    Line = wxStaticLine:new(Panel, [{style, ?wxLI_HORIZONTAL}]),

    Buttons = wxDialog:createButtonSizer(Dialog, ?wxOK bor ?wxCANCEL),

    InnerSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(InnerSizer, TextCtrl,
		[{flag, ?wxEXPAND bor ?wxALL},{proportion, 1},{border, 5}]),
    wxSizer:add(InnerSizer, Line,
		[{flag, ?wxEXPAND},{proportion, 0},{border, 5}]),
    wxPanel:setSizer(Panel, InnerSizer),

    TopSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(TopSizer, Panel,
		[{flag, ?wxEXPAND bor ?wxALL},{proportion, 1},{border, 5}]),
    wxSizer:add(TopSizer, Buttons,
		[{flag, ?wxEXPAND bor ?wxBOTTOM bor ?wxRIGHT},{border, 10}]),

    % calculate the size of TopSizer when the whole user_term
    % fits in the TextCtrl
    DC = wxClientDC:new(Panel),
    W = wxDC:getCharWidth(DC),
    H = wxDC:getCharHeight(DC),
    {EW, EH} = wxDC:getMultiLineTextExtent(DC, Default),
    wxSizer:setItemMinSize(InnerSizer, 0, EW+2*W, EH+H),
    TopSize = wxSizer:getMinSize(TopSizer),
    % reset min size of TextCtrl to 40 chararacters * 4 lines
    wxSizer:setItemMinSize(InnerSizer, 0, 40*W, 4*H),

    wxWindow:setSizerAndFit(Dialog, TopSizer),
    wxSizer:setSizeHints(TopSizer, Dialog),

    wxWindow:setClientSize(Dialog, TopSize),

    case wxDialog:showModal(Dialog) of
	?wxID_OK ->
	    Str = wxTextCtrl:getValue(TextCtrl),
	    wxDialog:destroy(Dialog),
	    parse_string(ensure_last_is_dot(Str));
	?wxID_CANCEL ->
	    wxDialog:destroy(Dialog),
	    cancel
    end.

parse_string(Str) ->
    try
	Tokens = case erl_scan:string(Str) of
		     {ok, Ts, _} -> Ts;
		     {error, {_SLine, SMod, SError}, _} ->
			 throw(io_lib:format("~s", [SMod:format_error(SError)]))
		 end,
	case erl_parse:parse_term(Tokens) of
	    {error, {_PLine, PMod, PError}} ->
		throw(io_lib:format("~s", [PMod:format_error(PError)]));
	    Res -> Res
	end
    catch
	throw:ErrStr ->
	    {error, ErrStr};
	_:_Err ->
	    {error, ["Syntax error in: ", Str]}
    end.

ensure_last_is_dot([]) ->
    ".";
ensure_last_is_dot(String) ->
    case lists:last(String) =:= $. of
	true ->
	    String;
	false ->
	    String ++ "."
    end.
