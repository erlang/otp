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

-module(observer_lib).

-export([get_wx_parent/1,
	 display_info_dialog/1, display_yes_no_dialog/1,
	 display_progress_dialog/2, destroy_progress_dialog/0,
	 wait_for_progress/0, report_progress/1,
	 user_term/3, user_term_multiline/3,
	 interval_dialog/4, start_timer/1, stop_timer/1,
	 display_info/2, fill_info/2, update_info/2, to_str/1,
	 create_menus/3, create_menu_item/3,
	 create_attrs/0,
	 set_listctrl_col_size/2,
	 create_status_bar/1,
	 html_window/1, html_window/2
	]).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-define(SINGLE_LINE_STYLE, ?wxBORDER_NONE bor ?wxTE_READONLY bor ?wxTE_RICH2).
-define(MULTI_LINE_STYLE, ?SINGLE_LINE_STYLE bor ?wxTE_MULTILINE).


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
    display_info_dialog("",Str).
display_info_dialog(Title,Str) ->
    Dlg = wxMessageDialog:new(wx:null(), Str, [{caption,Title}]),
    wxMessageDialog:showModal(Dlg),
    wxMessageDialog:destroy(Dlg),
    ok.

display_yes_no_dialog(Str) ->
    Dlg = wxMessageDialog:new(wx:null(), Str, [{style,?wxYES_NO}]),
    R = wxMessageDialog:showModal(Dlg),
    wxMessageDialog:destroy(Dlg),
    R.

%% display_info(Parent, [{Title, [{Label, Info}]}]) -> {Panel, Sizer, InfoFieldsToUpdate}
display_info(Frame, Info) ->
    Panel = wxPanel:new(Frame),
    wxWindow:setBackgroundStyle(Panel, ?wxBG_STYLE_SYSTEM),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:addSpacer(Sizer, 5),
    Add = fun(BoxInfo) ->
		  case create_box(Panel, BoxInfo) of
		      {Box, InfoFs} ->
			  wxSizer:add(Sizer, Box,
				      [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
			  wxSizer:addSpacer(Sizer, 5),
			  InfoFs;
		      undefined ->
			  []
		  end
	  end,
    InfoFs = [Add(I) || I <- Info],
    wxWindow:setSizerAndFit(Panel, Sizer),
    {Panel, Sizer, InfoFs}.

fill_info([{dynamic, Key}|Rest], Data)
  when is_atom(Key); is_function(Key) ->
    %% Special case used by crashdump_viewer when the value decides
    %% which header to use
    case get_value(Key, Data) of
	undefined -> [undefined | fill_info(Rest, Data)];
	{Str,Value} -> [{Str, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Key}|Rest], Data) when is_atom(Key); is_function(Key) ->
    case get_value(Key, Data) of
	undefined -> [undefined | fill_info(Rest, Data)];
	Value -> [{Str, Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str,Attrib,Key}|Rest], Data) when is_atom(Key); is_function(Key) ->
    case get_value(Key, Data) of
	undefined -> [undefined | fill_info(Rest, Data)];
	Value -> [{Str,Attrib,Value} | fill_info(Rest, Data)]
    end;
fill_info([{Str, {Format, Key}}|Rest], Data)
  when is_atom(Key); is_function(Key), is_atom(Format) ->
    case get_value(Key, Data) of
	undefined -> [undefined | fill_info(Rest, Data)];
	Value -> [{Str, {Format, Value}} | fill_info(Rest, Data)]
    end;
fill_info([{Str, Attrib, {Format, Key}}|Rest], Data)
  when is_atom(Key); is_function(Key), is_atom(Format) ->
    case get_value(Key, Data) of
	undefined -> [undefined | fill_info(Rest, Data)];
	Value -> [{Str, Attrib, {Format, Value}} | fill_info(Rest, Data)]
    end;
fill_info([{Str,SubStructure}|Rest], Data) when is_list(SubStructure) ->
    [{Str, fill_info(SubStructure, Data)}|fill_info(Rest,Data)];
fill_info([{Str,Attrib,SubStructure}|Rest], Data) ->
    [{Str, Attrib, fill_info(SubStructure, Data)}|fill_info(Rest,Data)];
fill_info([{Str, Key = {K,N}}|Rest], Data) when is_atom(K), is_integer(N) ->
    case get_value(Key, Data) of
	undefined -> [undefined | fill_info(Rest, Data)];
	Value -> [{Str, Value} | fill_info(Rest, Data)]
    end;
fill_info([], _) -> [].

get_value(Fun, Data) when is_function(Fun) ->
    Fun(Data);
get_value(Key, Data) ->
    proplists:get_value(Key,Data).

update_info([Fields|Fs], [{_Header, SubStructure}| Rest]) ->
    update_info2(Fields, SubStructure),
    update_info(Fs, Rest);
update_info([Fields|Fs], [{_Header, _Attrib, SubStructure}| Rest]) ->
    update_info2(Fields, SubStructure),
    update_info(Fs, Rest);
update_info([], []) ->
    ok.

update_info2([undefined|Fs], [_|Rest]) ->
    update_info2(Fs, Rest);
update_info2([Scroll = {_, _, _}|Fs], [{_, NewInfo}|Rest]) ->
    update_scroll_boxes(Scroll, NewInfo),
    update_info2(Fs, Rest);
update_info2([Field|Fs], [{_Str, {click, Value}}|Rest]) ->
    wxStaticText:setLabel(Field, to_str(Value)),
    update_info2(Fs, Rest);
update_info2([Field|Fs], [{_Str, Value}|Rest]) ->
    wxStaticText:setLabel(Field, to_str(Value)),
    update_info2(Fs, Rest);
update_info2([Field|Fs], [undefined|Rest]) ->
    wxStaticText:setLabel(Field, ""),
    update_info2(Fs, Rest);
update_info2([], []) -> ok.

update_scroll_boxes({_, _, 0}, {_, []}) -> ok;
update_scroll_boxes({Win, Sizer, _}, {Type, List}) ->
    [wxSizerItem:deleteWindows(Child) ||  Child <- wxSizer:getChildren(Sizer)],
    Cursor = wxCursor:new(?wxCURSOR_HAND),
    add_entries(Type, List, Win, Sizer, Cursor),
    wxCursor:destroy(Cursor),
    wxSizer:recalcSizes(Sizer),
    wxWindow:refresh(Win),
    ok.

to_str(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_str({Unit, X}) when (Unit==bytes orelse Unit==time_ms) andalso is_list(X) ->
    try list_to_integer(X) of
	B -> to_str({Unit,B})
    catch error:badarg -> X
    end;
to_str({bytes, B}) ->
    KB = B div 1024,
    MB = KB div 1024,
    GB = MB div 1024,
    if
	GB > 10 -> integer_to_list(GB) ++ " GB";
	MB > 10 -> integer_to_list(MB) ++ " MB";
	KB >  0 -> integer_to_list(KB) ++ " kB";
	true -> integer_to_list(B) ++ " B"
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
to_str({{format,Fun},Value}) when is_function(Fun) ->
    Fun(Value);
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
to_str(Float) when is_float(Float) ->
    io_lib:format("~.3f", [Float]);
to_str(Term) ->
    io_lib:format("~w", [Term]).

create_menus([], _MenuBar, _Type) -> ok;
create_menus(Menus, MenuBar, Type) ->
    Add = fun({Tag, Ms}, Index) ->
		  create_menu(Tag, Ms, Index, MenuBar, Type)
	  end,
    [{First, _}|_] = Menus,
    Index = if Type =:= default -> 0;
	       First =:= "File" -> 0;
	       true -> 1
	    end,
    wx:foldl(Add, Index, Menus),
    ok.

create_menu("File", MenuItems, Index, MenuBar, Type) ->
    if
	Type =:= plugin ->
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
	   changed_even = wxListItemAttr:new(Text, mix(?BG_CHANGED,?BG_EVEN), Font),
	   changed_odd  = wxListItemAttr:new(Text, mix(?BG_CHANGED,?BG_ODD), Font),
	   new_even = wxListItemAttr:new(Text, mix(?BG_NEW,?BG_EVEN), Font),
	   new_odd  = wxListItemAttr:new(Text, mix(?BG_NEW, ?BG_ODD), Font),
	   searched = wxListItemAttr:new(Text, ?BG_SEARCHED, Font)
	  }.

mix(RGB,_) -> RGB.

%% mix({R,G,B},{MR,MG,MB}) ->
%%     {trunc(R*MR/255), trunc(G*MG/255), trunc(B*MB/255)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_box_info({Title, List}) when is_list(List) -> {Title, ?wxALIGN_LEFT, List};
get_box_info({Title, left, List}) -> {Title, ?wxALIGN_LEFT, List};
get_box_info({Title, right, List}) -> {Title, ?wxALIGN_RIGHT, List}.

add_box(Panel, OuterBox, Cursor, Title, Proportion, {Format, List}) ->
    Box = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, Title}]),
    Scroll = wxScrolledWindow:new(Panel),
    wxScrolledWindow:enableScrolling(Scroll,true,true),
    wxScrolledWindow:setScrollbars(Scroll,1,1,0,0),
    ScrollSizer  = wxBoxSizer:new(?wxVERTICAL),
    wxScrolledWindow:setSizer(Scroll, ScrollSizer),
    wxWindow:setBackgroundStyle(Scroll, ?wxBG_STYLE_SYSTEM),
    add_entries(Format, List, Scroll, ScrollSizer, Cursor),
    wxSizer:add(Box,Scroll,[{proportion,1},{flag,?wxEXPAND}]),
    wxSizer:add(OuterBox,Box,[{proportion,Proportion},{flag,?wxEXPAND}]),
    {Scroll,ScrollSizer,length(List)}.

add_entries(click, List, Scroll, ScrollSizer, Cursor) ->
    Add = fun(Link) ->
		  TC = link_entry(Scroll, Link, Cursor),
                  wxWindow:setBackgroundStyle(TC, ?wxBG_STYLE_SYSTEM),
		  wxSizer:add(ScrollSizer,TC, [{flag,?wxEXPAND}])
	  end,
    [Add(Link) || Link <- List];
add_entries(plain, List, Scroll, ScrollSizer, _) ->
    Add = fun(String) ->
		  TC = wxStaticText:new(Scroll, ?wxID_ANY, String),
		  wxSizer:add(ScrollSizer,TC,[{flag,?wxEXPAND}])
	  end,
    [Add(String) || String <- List].


create_box(_Panel, {scroll_boxes,[]}) ->
    undefined;
create_box(Panel, {scroll_boxes,Data}) ->
    OuterBox = wxBoxSizer:new(?wxHORIZONTAL),
    Cursor = wxCursor:new(?wxCURSOR_HAND),
    AddBox = fun({Title,Proportion,Format = {_,_}}) ->
		     add_box(Panel, OuterBox, Cursor, Title, Proportion, Format);
		({Title, Format = {_,_}}) ->
		     add_box(Panel, OuterBox, Cursor, Title, 1, Format);
		(undefined) ->
		     undefined
	     end,
    Boxes = [AddBox(Entry) || Entry <- Data],
    wxCursor:destroy(Cursor),

    MaxL = lists:foldl(fun({_,_,L},Max) when L>Max -> L;
			  (_,Max) -> Max
		       end,
		       0,
		       Boxes),

    Dummy = wxTextCtrl:new(Panel, ?wxID_ANY, [{style, ?SINGLE_LINE_STYLE}]),
    {_,H} = wxWindow:getSize(Dummy),
    wxTextCtrl:destroy(Dummy),

    MaxH = if MaxL > 8 -> 8*H;
	      true -> MaxL*H
	   end,
    [wxWindow:setMinSize(B,{0,MaxH}) || {B,_,_} <- Boxes],
    wxSizer:layout(OuterBox),
    {OuterBox, Boxes};

create_box(Parent, Data) ->
    {Title, _Align, Info} = get_box_info(Data),
    Top = wxStaticBoxSizer:new(?wxVERTICAL, Parent, [{label, Title}]),
    Panel = wxPanel:new(Parent),
    Box = wxBoxSizer:new(?wxVERTICAL),
    LeftSize = 30 + get_max_width(Panel,Info),
    RightProportion = [{flag, ?wxEXPAND}],
    AddRow = fun({Desc0, Value0}) ->
		     Desc = Desc0++":",
		     Line = wxBoxSizer:new(?wxHORIZONTAL),
		     Label = wxStaticText:new(Panel, ?wxID_ANY, Desc),
		     wxSizer:add(Line, 5, 0),
		     wxSizer:add(Line, Label),
		     wxSizer:setItemMinSize(Line, Label, LeftSize, -1),
		     Field =
			 case Value0 of
			     {click,"unknown"} ->
				 wxStaticText:new(Panel, ?wxID_ANY,"unknown");
			     {click,Value} ->
				 link_entry(Panel,Value);
			     _ ->
				 Value = to_str(Value0),
				 case length(Value) > 100 of
				     true ->
					 Shown = lists:sublist(Value, 80),
					 TCtrl = wxStaticText:new(Panel, ?wxID_ANY, [Shown,"..."]),
					 wxWindow:setToolTip(TCtrl,wxToolTip:new(Value)),
					 TCtrl;
				     false ->
					 wxStaticText:new(Panel, ?wxID_ANY, Value)
				 end
			 end,
		     wxSizer:add(Line, 10, 0), % space of size 10 horisontally
		     wxSizer:add(Line, Field, RightProportion),
		     wxSizer:add(Box, Line, [{proportion,1}]),
		     Field;
		(undefined) ->
		     undefined
	     end,
    InfoFields = [AddRow(Entry) || Entry <- Info],
    wxWindow:setSizer(Panel, Box),
    wxSizer:add(Top, Panel, [{proportion,1},{flag,?wxEXPAND}]),
    {Top, InfoFields}.

link_entry(Panel, Link) ->
    Cursor = wxCursor:new(?wxCURSOR_HAND),
    TC = link_entry2(Panel, to_link(Link), Cursor),
    wxCursor:destroy(Cursor),
    TC.
link_entry(Panel, Link, Cursor) ->
    link_entry2(Panel, to_link(Link), Cursor).

link_entry2(Panel,{Target,Str},Cursor) ->
    TC = wxStaticText:new(Panel, ?wxID_ANY, Str),
    wxWindow:setForegroundColour(TC,?wxBLUE),
    wxWindow:setCursor(TC, Cursor),
    wxWindow:connect(TC, left_down, [{userData,Target}]),
    wxWindow:connect(TC, enter_window),
    wxWindow:connect(TC, leave_window),
    ToolTip = wxToolTip:new("Click to see properties for " ++ Str),
    wxWindow:setToolTip(TC, ToolTip),
    TC.

to_link(RegName={Name, Node}) when is_atom(Name), is_atom(Node) ->
    Str = io_lib:format("{~p,~p}", [Name, Node]),
    {RegName, Str};
to_link(TI = {_Target, _Identifier}) ->
    TI;
to_link(Target0) ->
    Target=to_str(Target0),
    {Target, Target}.

html_window(Panel) ->
    Win = wxHtmlWindow:new(Panel, [{style, ?wxHW_SCROLLBAR_AUTO}]),
    %% wxHtmlWindow:setFonts(Win, "", FixedName),
    wxHtmlWindow:connect(Win,command_html_link_clicked),
    Win.

html_window(Panel, Html) ->
    Win = html_window(Panel),
    wxHtmlWindow:setPage(Win, Html),
    Win.

get_max_width(Parent,Info) ->
    lists:foldl(fun({Desc,_}, Max) ->
			{W, _, _, _} = wxWindow:getTextExtent(Parent, Desc),
			max(W,Max);
		   (_, Max) -> Max
		end, 0, Info).

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
	{unix, darwin} -> 0; %% Always 0 in wxWidgets-3.0
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

%%%-----------------------------------------------------------------
%%% Status bar for warnings
create_status_bar(Panel) ->
    StatusStyle = ?wxTE_MULTILINE bor ?wxTE_READONLY bor ?wxTE_RICH2,
    Red = wxTextAttr:new(?wxRED),

    %% wxTextCtrl:setSize/3 does not work, so we must create a dummy
    %% text ctrl first to get the size of the text, then set it when
    %% creating the real text ctrl.
    Dummy = wxTextCtrl:new(Panel, ?wxID_ANY,[{style,StatusStyle}]),
    {X,Y,_,_} = wxTextCtrl:getTextExtent(Dummy,"WARNING"),
    wxTextCtrl:destroy(Dummy),
    StatusBar = wxTextCtrl:new(Panel, ?wxID_ANY,
			 [{style,StatusStyle},
			  {size,{X,Y+2}}]), % Y+2 to avoid scrollbar
    wxTextCtrl:setDefaultStyle(StatusBar,Red),
    wxTextAttr:destroy(Red),
    StatusBar.

%%%-----------------------------------------------------------------
%%% Progress dialog
-define(progress_handler,cdv_progress_handler).
display_progress_dialog(Title,Str) ->
    Caller = self(),
    Env = wx:get_env(),
    spawn_link(fun() ->
		       progress_handler(Caller,Env,Title,Str)
	       end),
    ok.

wait_for_progress() ->
    receive
	continue ->
	    ok;
	Error ->
	    Error
    end.

destroy_progress_dialog() ->
    report_progress(finish).

report_progress(Progress) ->
    case whereis(?progress_handler) of
	Pid when is_pid(Pid) ->
	    Pid ! {progress,Progress},
	    ok;
	_ ->
	    ok
    end.

progress_handler(Caller,Env,Title,Str) ->
    register(?progress_handler,self()),
    wx:set_env(Env),
    PD = progress_dialog(Env,Title,Str),
    try progress_loop(Title,PD,Caller)
    catch closed -> normal end.

progress_loop(Title,PD,Caller) ->
    receive
	{progress,{ok,done}} -> % to make wait_for_progress/0 return
	    Caller ! continue,
	    progress_loop(Title,PD,Caller);
	{progress,{ok,Percent}} when is_integer(Percent) ->
	    update_progress(PD,Percent),
	    progress_loop(Title,PD,Caller);
	{progress,{ok,Msg}} ->
	    update_progress_text(PD,Msg),
	    progress_loop(Title,PD,Caller);
	{progress,{error, Reason}} ->
	    finish_progress(PD),
	    FailMsg =
		if is_list(Reason) -> Reason;
		   true -> file:format_error(Reason)
		end,
	    display_info_dialog("Crashdump Viewer Error",FailMsg),
	    Caller ! error,
	    unregister(?progress_handler),
	    unlink(Caller);
	{progress,finish} ->
	    finish_progress(PD),
	    unregister(?progress_handler),
	    unlink(Caller)
    end.

progress_dialog(_Env,Title,Str) ->
    PD = wxProgressDialog:new(Title,Str,
			      [{maximum,101},
			       {style,
				?wxPD_APP_MODAL bor
				    ?wxPD_SMOOTH bor
				    ?wxPD_AUTO_HIDE}]),
    wxProgressDialog:setMinSize(PD,{200,-1}),
    PD.

update_progress(PD,Value) ->
    try wxProgressDialog:update(PD,Value)
    catch _:_ -> throw(closed) %% Port or window have died
    end.
update_progress_text(PD,Text) ->
    try wxProgressDialog:update(PD,0,[{newmsg,Text}])
    catch _:_ -> throw(closed) %% Port or window have died
    end.
finish_progress(PD) ->
    wxProgressDialog:destroy(PD).
