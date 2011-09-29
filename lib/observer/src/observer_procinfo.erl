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

-module(observer_procinfo).

-behaviour(wx_object).

-export([start/4]).

-export([init/1, handle_event/2, handle_cast/2, terminate/2, code_change/3,
	 handle_call/3, handle_info/2]).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-define(CLOSE, 601).
-define(REFRESH, 602).
-define(SELECT_ALL, 603).
-define(ID_NOTEBOOK, 604).

-record(procinfo_state, {parent,
			 frame,
			 node,
			 pid,
			 module,
			 procinfo_stc,
			 modinfo_stc,
			 modcode_stc,
			 checklistbox,
			 current_view, % proc_info::atom | module_info::atom | module_code::atom
			 itemlist = [{backtrace, false},
				     {binary, false},
				     {catchlevel, false},
				     {current_function, false},
				     {dictionary, false},
				     {error_handler, true},
				     {garbage_collection, true},
				     {group_leader, true},
				     {heap_size, true},
				     {initial_call, false},
				     {last_calls, false},
				     {links, true},
				     {memory, false},
				     {message_queue_len, true},
				     {messages, false},
				     {monitored_by, false},
				     {monitors, false},
				     {priority, true},
				     {reductions, false},
				     {registered_name, false},
				     {sequential_trace_token, false},
				     {stack_size, false},
				     {status, false},
				     {suspending, false},
				     {total_heap_size, false},
				     {trace, false},
				     {trap_exit,true}]
			}).




start(Node, Process, ParentFrame, Parent) ->
    wx_object:start(?MODULE, [Node, Process, ParentFrame, Parent], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Node, Process, ParentFrame, Parent]) ->
    try
	State = #procinfo_state{parent = Parent,
				node = Node,
				pid = Process,
				current_view = proc_info
			       },
	ItemList = State#procinfo_state.itemlist,
	Name = case observer_wx:try_rpc(Node, erlang, process_info, [Process, registered_name]) of
		   [] ->
		       undefined;
		   {registered_name, M} ->
		       M
	       end,
	{initial_call, {Module, _, _}} = observer_wx:try_rpc(Node, erlang, process_info, [Process, initial_call]),
	{Frame, ProcStc, CheckListBox, ModInfoStc, ModCodeStc} = setup(ParentFrame, Node, Process, ItemList, Module, Name),
	{Frame, State#procinfo_state{frame = Frame,
				     module = Module,
				     procinfo_stc = ProcStc,
				     modinfo_stc = ModInfoStc,
				     modcode_stc = ModCodeStc,
				     checklistbox = CheckListBox}}
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(ParentFrame, Node),
	    {stop, badrpc, #procinfo_state{parent = Parent,
					   pid = Process}}
    end.


setup(ParentFrame, Node, Pid, ItemList, Module, Name) ->
    Title = case Name of
		undefined ->
		    atom_to_list(Node) ++ ":" ++ atom_to_list(Module);
		Name ->
		    atom_to_list(Node) ++ ":" ++ atom_to_list(Name)
	    end,
    Frame = wxFrame:new(ParentFrame, ?wxID_ANY, Title,
			[{style, ?wxDEFAULT_FRAME_STYLE},
			 {size, {900,900}}]),
    Panel = wxPanel:new(Frame, []),
    MainSz = wxBoxSizer:new(?wxHORIZONTAL),
    Notebook = wxNotebook:new(Panel, ?ID_NOTEBOOK, [{style, ?wxBK_DEFAULT}]),
    wxNotebook:connect(Notebook, command_notebook_page_changed),
    {CodePanel, CheckListBox, CodeStc} = create_procinfo_page(Notebook, Node, Pid, ItemList),
    {ModInfoPanel, ModInfoStc} = create_page(Notebook, Node, Module, module_info),
    {ModCodePanel, ModCodeStc} = create_page(Notebook, Node, Module, module_code),
    wxNotebook:addPage(Notebook, CodePanel, "Process information", []),
    wxNotebook:addPage(Notebook, ModInfoPanel, "Module information", []),
    wxNotebook:addPage(Notebook, ModCodePanel, "Module code", []),
    MenuBar = wxMenuBar:new(),
    create_menus(MenuBar),
    wxWindow:setSizer(Panel, MainSz),
    wxSizer:add(MainSz, Notebook, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:show(Frame),
    wxFrame:connect(Frame, close_window),
    wxMenu:connect(Frame, command_menu_selected),
    {Frame, CodeStc, CheckListBox, ModInfoStc, ModCodeStc}.


create_procinfo_page(Notebook, Node, Pid, ItemList) ->
    Panel = wxPanel:new(Notebook),
    MainSz = wxBoxSizer:new(?wxHORIZONTAL),
    CheckSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "View"}]),
    BtnSz = wxBoxSizer:new(?wxHORIZONTAL),

    Stc = create_styled_txtctrl(Panel, proc_info),
    Txt = get_formatted_values(Node, Pid, ItemList),
    set_text(Stc, Txt, text),
    Choices = [atom_to_list(Tag) || {Tag, _} <- ItemList],
    CheckListBox = wxCheckListBox:new(Panel, ?wxID_ANY, [{choices, Choices},
							 {style, ?wxLB_EXTENDED},
							 {style, ?wxLB_SORT},
							 {style, ?wxLB_NEEDED_SB}]),
    check_boxes(CheckListBox, ItemList),
    wxCheckListBox:connect(CheckListBox, command_checklistbox_toggled),

    SelAllBtn = wxButton:new(Panel, ?SELECT_ALL, [{label, "Select all"}]),
    DeSelAllBtn = wxButton:new(Panel, ?SELECT_ALL, [{label, "Deselect all"}]),

    wxButton:connect(SelAllBtn, command_button_clicked, [{userData, true}]),
    wxButton:connect(DeSelAllBtn, command_button_clicked, [{userData, false}]),
    wxWindow:setSizer(Panel, MainSz),
    wxSizer:add(MainSz, Stc, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(CheckSz, CheckListBox, [{proportion, 1}]),
    wxSizer:add(BtnSz, SelAllBtn),
    wxSizer:add(BtnSz, DeSelAllBtn),
    wxSizer:add(CheckSz, BtnSz),
    wxSizer:add(MainSz, CheckSz, [{flag, ?wxEXPAND}]),
    {Panel, CheckListBox, Stc}.

create_page(Notebook, Node, Module, What) ->
    Panel = wxPanel:new(Notebook, []),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    Stc = create_styled_txtctrl(Panel, What),
    {Sort, Txt} = case What of
		      module_info ->
			  {text, get_formatted_modinfo(Node, Module)};
		      module_code ->
			  case get_src_file(Node, Module) of
			      {ok, File} ->
				  {file, File};
			      error->
				  {text, "Error! Could not read sourcefile"}
			  end
		  end,
    set_text(Stc, Txt, Sort),
    wxWindow:setSizer(Panel, Sizer),
    wxSizer:add(Sizer, Stc, [{flag, ?wxEXPAND}, {proportion, 1}]),
    {Panel, Stc}.

create_menus(MenuBar) ->
    observer_wx:create_menu(
      [
       {"File", [#create_menu{id = ?CLOSE, text = "Close"}]},
       {"View", [#create_menu{id = ?REFRESH, text = "Refresh"}]}
      ],
      MenuBar).

check_boxes(CheckListBox, Bool, all) ->
    lists:foreach(fun(Index) ->
			  wxCheckListBox:check(CheckListBox, Index, [{check, Bool}])
		  end,
		  lists:seq(0, wxControlWithItems:getCount(CheckListBox))).
check_boxes(CheckListBox, ItemList) ->
    lists:foldl(fun({_, Bool}, Index) ->
			wxCheckListBox:check(CheckListBox, Index, [{check, Bool}]),
			Index+1
		end,
		0, ItemList).

create_styled_txtctrl(Parent, View) ->
    FixedFont = wxFont:new(11, ?wxFONTFAMILY_TELETYPE, ?wxFONTSTYLE_NORMAL, ?wxNORMAL,[]),
    Stc = wxStyledTextCtrl:new(Parent),
    wxStyledTextCtrl:styleClearAll(Stc),
    wxStyledTextCtrl:styleSetFont(Stc, ?wxSTC_STYLE_DEFAULT, FixedFont),
    wxStyledTextCtrl:setLexer(Stc, ?wxSTC_LEX_ERLANG),
    wxStyledTextCtrl:setMarginType(Stc, 2, ?wxSTC_MARGIN_NUMBER),
    W = wxStyledTextCtrl:textWidth(Stc, ?wxSTC_STYLE_LINENUMBER, "9"),
    wxStyledTextCtrl:setMarginWidth(Stc, 2, W*3),

    wxStyledTextCtrl:setSelectionMode(Stc, ?wxSTC_SEL_LINES),
    wxStyledTextCtrl:setUseHorizontalScrollBar(Stc, false),

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
		       wxStyledTextCtrl:styleSetFont(Stc, Style, FixedFont),
		       wxStyledTextCtrl:styleSetForeground(Stc, Style, Color)
	       end,
    [SetStyle(Style) || Style <- Styles],

    KeyWords = case View of
		   proc_info ->
		       get_procinfo_keywords();
		   module_info ->
		       get_modinfo_keywords();
		   module_code ->
		       get_erl_keywords()
	       end,
    wxStyledTextCtrl:setKeyWords(Stc, 0, KeyWords),
    Stc.

get_erl_keywords() ->
    L = ["after","begin","case","try","cond","catch","andalso","orelse",
	 "end","fun","if","let","of","query","receive","when","bnot","not",
	 "div","rem","band","and","bor","bxor","bsl","bsr","or","xor"],
    lists:flatten([K ++ " "|| K <- L] ++ [0]).
get_procinfo_keywords() ->
    L = ["backtrace","binary","catchlevel","current_function","dictionary",
	 "error_handler","garbage_collection","group_leader", "heap_size",
	 "initial_call","last_calls","links","memory","message_queue_len",
	 "messages","monitored_by","monitors", "priority","reductions",
	 "registered_name", "sequential_trace_token","stack_size","status",
	 "suspending", "total_heap_size","trace","trap_exit"],
    lists:flatten([K ++ " "|| K <- L] ++ [0]).
get_modinfo_keywords() ->
    L = ["exports", "imports", "attributes", "compile"],
    lists:flatten([K ++ " "|| K <- L] ++ [0]).

get_formatted_values(Node, Process, ItemList) ->
    TagList = [Tag || {Tag, Bool} <- ItemList, Bool =:= true],
    Values = observer_wx:try_rpc(Node, erlang, process_info, [Process, TagList]),
    lists:flatten(format_value(Values, [])).

format_value([], Acc) ->
    lists:reverse(Acc);
format_value([{backtrace, Bin} | T], Acc) ->
    format_value(T, [io_lib:format("{backtrace,~s}~n", [binary_to_list(Bin)]) | Acc]);
format_value([H|T], Acc) ->
    format_value(T, [io_lib:format("~p~n", [H]) | Acc]).

get_formatted_modinfo(Node, Module) ->
    Info = observer_wx:try_rpc(Node, Module, module_info, []),
    lists:flatten([io_lib:format("~p~n", [I]) || I <- Info]).
get_src_remote(Node, Module) ->
    case observer_wx:try_rpc(Node, filename, find_src, [Module]) of
	{error, _} ->
	    error;
	{SrcFile, _} ->
	    case observer_wx:try_rpc(Node, file, read_file_info, [SrcFile ++ ".erl"]) of
		{error, _} ->
		    error;
		{ok, _} ->
		    {ok, SrcFile ++ ".erl"}
	    end
    end.

get_src_local(Module) ->
    case filename:find_src(Module) of
	{error, _} ->
	    error;
        {SrcFile, _} ->
            case file:read_file_info(SrcFile ++ ".erl") of
                {error, _} ->
		    error;
                {ok, _} ->
                    {ok, SrcFile ++ ".erl"}
            end
    end.

get_src_file(Node, Module) ->
    case get_src_remote(Node, Module) of
	{ok, SrcFile} ->
	    {ok, SrcFile};
	error ->
	    get_src_local(Module)
    end.


set_text(Stc, Text, text) ->
    wxStyledTextCtrl:setReadOnly(Stc, false),
    wxStyledTextCtrl:setText(Stc, Text),
    wxStyledTextCtrl:setReadOnly(Stc, true);
set_text(Stc, File, file) ->
    wxStyledTextCtrl:setReadOnly(Stc, false),
    wxStyledTextCtrl:loadFile(Stc, File),
    wxStyledTextCtrl:setReadOnly(Stc, true).

update_procinfo_page(Stc, Node, Process, ItemList) ->
    Txt = get_formatted_values(Node, Process, ItemList),
    set_text(Stc, Txt, text).
update_modinfo_page(Stc, Node, Module) ->
    Txt = get_formatted_modinfo(Node, Module),
    set_text(Stc, Txt, text).
update_modcode_page(Stc, Node, Module) ->
    case get_src_file(Node, Module) of
	{ok, File} ->
	    set_text(Stc, File, file);
	error ->
	    set_text(Stc, "Error! Could not read sourcefile", text)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Callbacks%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(#wx{event = #wxClose{type = close_window}},
	     State) ->
    {stop, shutdown, State};

handle_event(#wx{id = ?CLOSE,
		 event = #wxCommand{type = command_menu_selected}},
	     State) ->
    {stop, shutdown, State};

handle_event(#wx{id = ?REFRESH,
		 event = #wxCommand{type = command_menu_selected}},
	     #procinfo_state{current_view = Current,
			     frame = Frame,
			     node = Node,
			     pid = Pid,
			     procinfo_stc = Stc,
			     itemlist = ItemList} = State) when Current =:= proc_info ->
    try
	update_procinfo_page(Stc, Node, Pid, ItemList),
	{noreply, State}
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Frame, Node),
	    {stop, badrpc, State}
    end;

handle_event(#wx{id = ?REFRESH,
		 event = #wxCommand{type = command_menu_selected}},
	     #procinfo_state{current_view = Current,
			     frame = Frame,
			     node = Node,
			     modinfo_stc = Stc,
			     module = Module} = State) when Current =:= module_info ->
    try
	update_modinfo_page(Stc, Node, Module),
	{noreply, State}
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Frame, Node),
	    {stop, badrpc, State}
    end;

handle_event(#wx{id = ?REFRESH,
		 event = #wxCommand{type = command_menu_selected}},
	     #procinfo_state{current_view = Current,
			     modcode_stc = Stc,
			     frame = Frame,
			     node = Node,
			     module = Module} = State) when Current =:= module_code ->
    try
	update_modcode_page(Stc, Node, Module),
	{noreply, State}
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Frame, Node),
	    {stop, badrpc, State}
    end;


handle_event(#wx{obj = Notebook, id = ?ID_NOTEBOOK,
		 event = #wxNotebook{type = command_notebook_page_changed}},
	     #procinfo_state{frame = Frame,
			     module = Module,
			     procinfo_stc = ProcStc,
			     modcode_stc = CodeStc,
			     modinfo_stc = ModInfoStc,
			     node = Node,
			     pid = Pid,
			     itemlist = ItemList} = State) ->
    try
	Current = case observer_wx:check_page_title(Notebook) of
		      "Process information" ->
			  update_procinfo_page(ProcStc, Node, Pid, ItemList),
			  proc_info;
		      "Module information" ->
			  update_modinfo_page(ModInfoStc, Node, Module),
			  module_info;
		      "Module code" ->
			  update_modcode_page(CodeStc, Node, Module),
			  module_code
		  end,
	{noreply, State#procinfo_state{current_view = Current}}
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Frame, Node),
	    {stop, badrpc, State}
    end;

handle_event(#wx{event = #wxCommand{type = command_checklistbox_toggled,
				    commandInt = Index},
		 obj = CheckListbox},
	     #procinfo_state{frame = Frame,
			     node = Node,
			     pid = Process,
			     procinfo_stc = Stc,
			     itemlist = ItemList} = State) ->
    try
	{Tag, _} = lists:nth(Index+1, ItemList),
	ItemList2 = case wxCheckListBox:isChecked(CheckListbox, Index) of
			true ->
			    lists:keyreplace(Tag, 1, ItemList, {Tag, true});
			false ->
			    lists:keyreplace(Tag, 1, ItemList, {Tag, false})
		    end,
	Txt = get_formatted_values(Node, Process, ItemList2),
	set_text(Stc, Txt, text),
	{noreply, State#procinfo_state{itemlist = ItemList2}}

    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Frame, Node),
	    {stop, badrpc, State}
    end;

handle_event(#wx{id = ?SELECT_ALL,
		 event = #wxCommand{type = command_button_clicked},
		 userData = Bool},
	     #procinfo_state{frame = Frame,
			     node = Node,
			     pid = Process,
			     itemlist = ItemList,
			     procinfo_stc = Stc,
			     checklistbox = CheckListBox} = State) ->
    try
	check_boxes(CheckListBox, Bool, all),
	ItemList2 = lists:keymap(fun(_) ->
					 Bool
				 end,
				 2, ItemList),
	Txt = get_formatted_values(Node, Process, ItemList2),
	set_text(Stc, Txt, text),
	{noreply, State#procinfo_state{itemlist = ItemList2}}
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Frame, Node),
	    {stop, badrpc, State}
    end;

handle_event(Event, State) ->
    io:format("~p: ~p, Handle event: ~p~n", [?MODULE, ?LINE, Event]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("~p: ~p, Handle info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

handle_call(Call, _From, State) ->
    io:format("~p ~p: Got call ~p~n",[?MODULE, ?LINE, Call]),
    {reply, ok, State}.

handle_cast(Cast, State) ->
    io:format("~p ~p: Got cast ~p~n", [?MODULE, ?LINE, Cast]),
    {noreply, State}.

terminate(Reason, #procinfo_state{parent = Parent,
				  pid = Pid,
				  frame = Frame}) ->
    io:format("~p terminating. Reason: ~p~n", [?MODULE, Reason]),
    Parent ! {procinfo_menu_closed, Pid},
    case Frame of
	undefined ->
	    ok;
	_ ->
	    wxFrame:destroy(Frame)
    end,
    ok.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.
