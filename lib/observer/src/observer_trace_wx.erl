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

-module(observer_trace_wx).

-export([start_link/2, add_processes/2]).
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-define(OPTIONS, 301).
-define(SAVE_BUFFER, 302).
-define(CLOSE, 303).
-define(CLEAR, 304).
-define(SAVE_TRACEOPTS, 305).
-define(LOAD_TRACEOPTS, 306).
-define(TOGGLE_TRACE, 307).
-define(ADD_NEW, 308).
-define(ADD_TP, 309).
-define(PROCESSES, 350).
-define(MODULES, 351).
-define(FUNCTIONS, 352).

-record(state,
	{parent,
	 panel,
	 p_view,
	 m_view,
	 f_view,
	 nodes = [],
	 toggle_button,
	 tpids = [],  %% #tpid
	 def_trace_opts = [],
	 tpatterns = dict:new(), % Key =:= Module::atom, Value =:= {M, F, A, MatchSpec}
	 match_specs = []}). % [ #match_spec{} ]

-record(tpid, {pid, opts}).

start_link(Notebook, ParentPid) ->
    wx_object:start_link(?MODULE, [Notebook, ParentPid], []).

add_processes(Tracer, Pids) when is_list(Pids) ->
    wx_object:cast(Tracer, {add_processes, Pids}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Notebook, ParentPid]) ->
    wx:batch(fun() -> create_window(Notebook, ParentPid) end).

create_window(Notebook, ParentPid) ->
    %% Create the window
    Panel = wxPanel:new(Notebook, [{size, wxWindow:getClientSize(Notebook)}]),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    Splitter = wxSplitterWindow:new(Panel, [{size, wxWindow:getClientSize(Panel)}]),
    ProcessView = create_process_view(Splitter),
    {MatchSpecView,ModView,FuncView} = create_matchspec_view(Splitter),
    wxSplitterWindow:setSashGravity(Splitter, 0.5),
    wxSplitterWindow:setMinimumPaneSize(Splitter,50),
    wxSplitterWindow:splitHorizontally(Splitter, ProcessView, MatchSpecView),
    wxSizer:add(Sizer, Splitter, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}, {proportion, 1}]),
    %% Buttons
    Buttons = wxBoxSizer:new(?wxHORIZONTAL),
    ToggleButton = wxToggleButton:new(Panel, ?TOGGLE_TRACE, "Start Trace", []),
    wxSizer:add(Buttons, ToggleButton),
    New = wxButton:new(Panel, ?ADD_NEW, [{label, "Trace New Processes"}]),
    wxSizer:add(Buttons, New),
    ATP = wxButton:new(Panel, ?ADD_TP, [{label, "Add Trace Pattern"}]),
    wxSizer:add(Buttons, ATP),
    wxMenu:connect(Panel, command_togglebutton_clicked, []),
    wxMenu:connect(Panel, command_button_clicked, []),
    wxSizer:add(Sizer, Buttons, [{flag, ?wxALL},{border, 2}, {proportion,0}]),
    wxWindow:setSizer(Panel, Sizer),
    {Panel, #state{parent=ParentPid, panel=Panel,
		   p_view=ProcessView, m_view=ModView, f_view=FuncView,
		   match_specs=default_matchspecs()}}.

default_matchspecs() ->
    Ms = [{"Return Trace", [{'_', [], [{return_trace}]}], "fun(_) -> return_trace() end"},
	  {"Exception Trace", [{'_', [], [{exception_trace}]}], "fun(_) -> exception_trace() end"},
	  {"Message Caller", [{'_', [], [{message,{caller}}]}], "fun(_) -> message(caller()) end"},
	  {"Message Dump", [{'_', [], [{message,{process_dump}}]}], "fun(_) -> message(process_dump()) end"}],
    [make_ms(Name,Term,FunStr) || {Name,Term,FunStr} <- Ms].

create_process_view(Parent) ->
    Style = ?wxLC_REPORT bor ?wxLC_SINGLE_SEL bor ?wxLC_HRULES,
    Grid = wxListCtrl:new(Parent, [{winid, ?PROCESSES}, {style, Style}]),
    Li = wxListItem:new(),
    AddListEntry = fun({Name, Align, DefSize}, Col) ->
			   wxListItem:setText(Li, Name),
			   wxListItem:setAlign(Li, Align),
			   wxListCtrl:insertColumn(Grid, Col, Li),
			   wxListCtrl:setColumnWidth(Grid, Col, DefSize),
			   Col + 1
		   end,
    ListItems = [{"Process Id",    ?wxLIST_FORMAT_CENTER,  120},
		 {"Trace Options", ?wxLIST_FORMAT_LEFT, 300}],
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),

    %% wxListCtrl:connect(Grid, command_list_item_activated),
    %% wxListCtrl:connect(Grid, command_list_item_selected),
    wxListCtrl:connect(Grid, size, [{skip, true}]),

    wxWindow:setFocus(Grid),
    Grid.

create_matchspec_view(Parent) ->
    Panel  = wxPanel:new(Parent),
    MainSz = wxBoxSizer:new(?wxHORIZONTAL),
    Style = ?wxLC_REPORT bor ?wxLC_SINGLE_SEL bor ?wxLC_HRULES,
    Splitter = wxSplitterWindow:new(Panel, []),
    Modules = wxListCtrl:new(Splitter, [{winid, ?MODULES}, {style, Style}]),
    Funcs   = wxListCtrl:new(Splitter, [{winid, ?FUNCTIONS}, {style, Style}]),
    Li = wxListItem:new(),
    wxListItem:setText(Li, "Modules"),
    wxListCtrl:insertColumn(Modules, 0, Li),
    wxListItem:setText(Li, "Functions"),
    wxListCtrl:insertColumn(Funcs, 0, Li),
    wxListCtrl:setColumnWidth(Funcs, 0, 150),
    wxListItem:setText(Li, "Match Spec"),
    wxListCtrl:insertColumn(Funcs, 1, Li),
    wxListCtrl:setColumnWidth(Funcs, 1, 300),
    wxListItem:destroy(Li),
    wxSplitterWindow:setSashGravity(Splitter, 0.0),
    wxSplitterWindow:setMinimumPaneSize(Splitter,50),
    wxSplitterWindow:splitVertically(Splitter, Modules, Funcs, [{sashPosition, 150}]),
    wxSizer:add(MainSz, Splitter,   [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxListCtrl:connect(Modules, size, [{skip, true}]),
    wxListCtrl:connect(Funcs,   size, [{skip, true}]),
    wxListCtrl:connect(Modules, command_list_item_selected),
    %% wxListCtrl:connect(Funcs, command_list_item_selected),
    wxPanel:setSizer(Panel, MainSz),
    {Panel, Modules, Funcs}.

create_menues(Parent) ->
    Menus = [{"File", [#create_menu{id = ?LOAD_TRACEOPTS, text = "Load settings"},
		       #create_menu{id = ?SAVE_TRACEOPTS, text = "Save settings"}]
	     }],
    observer_wx:create_menus(Parent, Menus).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Main window
handle_event(#wx{obj=Obj, event=#wxSize{size={W,_}}}, State) ->
    case wx:getObjectType(Obj) =:= wxListCtrl of
	true ->
	    wx:batch(fun() ->
			     Cols = wxListCtrl:getColumnCount(Obj),
			     Last = lists:foldl(fun(I, Last) ->
							Last - wxListCtrl:getColumnWidth(Obj, I)
						end, W-?LCTRL_WDECR, lists:seq(0, Cols - 2)),
			     Size = max(150, Last),
			     wxListCtrl:setColumnWidth(Obj, Cols-1, Size)
		     end);
	false ->
	    ok
    end,
    {noreply, State};

handle_event(#wx{id=?ADD_NEW}, State = #state{panel=Parent, def_trace_opts=TraceOpts}) ->
    case observer_traceoptions_wx:process_trace(Parent, TraceOpts) of
	{ok, Opts} ->
	    Process = #tpid{pid=new, opts=Opts},
	    {noreply, do_add_processes([Process], State#state{def_trace_opts=Opts})};
	cancel ->
	    {noreply, State}
    end;

handle_event(#wx{id=?ADD_TP},
	     State = #state{panel=Parent, nodes=Nodes, match_specs=Ms}) ->
    Node = case Nodes of
	       [N|_] -> N;
	       [] -> node()
	   end,
    case observer_traceoptions_wx:trace_pattern(self(), Parent, Node, Ms) of
	cancel ->
	    {noreply, State};
	Patterns ->
	    {noreply, do_add_patterns(Patterns, State)}
    end;

handle_event(#wx{id=?MODULES, event=#wxList{type=command_list_item_selected, itemIndex=Row}},
	     State = #state{tpatterns=TPs, m_view=Mview, f_view=Fview}) ->
    Module = list_to_atom(wxListCtrl:getItemText(Mview, Row)),
    update_functions_view(dict:fetch(Module, TPs), Fview),
    {noreply, State};

%% handle_event(#wx{id = ?CLEAR, event = #wxCommand{type = command_menu_selected}},
%% 	     #state{text_ctrl = TxtCtrl} = State) ->
%%     wxTextCtrl:clear(TxtCtrl),
%%     {noreply, State};

%% handle_event(#wx{id = ?SAVE_BUFFER, event = #wxCommand{type = command_menu_selected}},
%% 	     #state{frame = Frame, text_ctrl = TxtCtrl} = State) ->
%%     Dialog = wxFileDialog:new(Frame, [{style, ?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT}]),
%%     case wxFileDialog:showModal(Dialog) of
%% 	?wxID_OK ->
%% 	    Path = wxFileDialog:getPath(Dialog),
%% 	    wxDialog:destroy(Dialog),
%% 	    case filelib:is_file(Path) of
%% 		true ->
%% 		    observer_wx:create_txt_dialog(Frame, "File already exists: " ++ Path ++ "\n",
%% 						  "Error", ?wxICON_ERROR);
%% 		false ->
%% 		    wxTextCtrl:saveFile(TxtCtrl, [{file, Path}])
%% 	    end;
%% 	_ ->
%% 	    wxDialog:destroy(Dialog),
%% 	    ok
%%     end,
%%     {noreply, State};

handle_event(#wx{id = ?SAVE_TRACEOPTS,
		 event = #wxCommand{type = command_menu_selected}},
	     #state{panel = Panel,
		    def_trace_opts = TraceOpts,
		    match_specs = MatchSpecs,
		    tpatterns = TracePatterns
		   } = State) ->
    Dialog = wxFileDialog:new(Panel, [{style, ?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT}]),
    case wxFileDialog:showModal(Dialog) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(Dialog),
	    write_file(Panel, Path, TraceOpts, MatchSpecs, dict:to_list(TracePatterns));
	_ ->
	    ok
    end,
    wxDialog:destroy(Dialog),
    {noreply, State};

handle_event(#wx{id = ?LOAD_TRACEOPTS,
		 event = #wxCommand{type = command_menu_selected}},
	     #state{panel = Panel} = State) ->
    Dialog = wxFileDialog:new(Panel, [{style, ?wxFD_FILE_MUST_EXIST}]),
    State2 = case wxFileDialog:showModal(Dialog) of
		 ?wxID_OK ->
		     Path = wxFileDialog:getPath(Dialog),
		     read_settings(Path, State);
		 _ ->
		     State
	     end,
    wxDialog:destroy(Dialog),
    {noreply, State2};


%% handle_event(#wx{event = #wxCommand{type = command_togglebutton_clicked, commandInt = 1}},
%% 	     #state{node = Node,
%% 		    traced_procs = TracedProcs,
%% 		    traced_funcs = TracedDict,
%% 		    trace_options = TraceOpts,
%% 		    text_ctrl = TextCtrl,
%% 		    toggle_button = ToggleBtn} = State) ->

%%     start_trace(Node, TracedProcs, TracedDict, TraceOpts),
%%     wxTextCtrl:appendText(TextCtrl, "Start Trace:\n"),
%%     wxToggleButton:setLabel(ToggleBtn, "Stop Trace"),
%%     {noreply, State};

%% handle_event(#wx{event = #wxCommand{type = command_togglebutton_clicked, commandInt = 0}}, %%Stop tracing
%% 	     #state{text_ctrl = TxtCtrl,
%% 		    toggle_button = ToggleBtn} = State) ->
%%     dbg:stop_clear(),
%%     wxTextCtrl:appendText(TxtCtrl, "Stop Trace.\n"),
%%     wxToggleButton:setLabel(ToggleBtn, "Start Trace"),
%%     {noreply, State};

handle_event(#wx{id=ID, event = What}, State) ->
    io:format("~p:~p: Unhandled event: ~p, ~p ~n", [?MODULE, self(), ID, What]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(Msg, _From, State) ->
    io:format("~p~p: Got Call ~p~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({add_processes, Pids}, State = #state{panel=Parent, def_trace_opts=TraceOpts}) ->
    case observer_traceoptions_wx:process_trace(Parent, TraceOpts) of
	{ok, Opts} ->
	    POpts = [#tpid{pid=Pid, opts=Opts} || Pid <- Pids],
	    {noreply, do_add_processes(POpts, State#state{def_trace_opts=Opts})};
	cancel ->
	    {noreply, State}
    end;
handle_cast(Msg, State) ->
    io:format("~p ~p: Unhandled cast ~p~n", [?MODULE, ?LINE, Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({active, _Node}, State=#state{parent=Parent}) ->
    create_menues(Parent),
    {noreply, State};

handle_info(not_active, State) ->
    {noreply, State};

handle_info({update_ms, NewMs}, State) ->
    {noreply, State#state{match_specs=NewMs}};

handle_info(Any, State) ->
    io:format("~p~p: received unexpected message: ~p\n", [?MODULE, self(), Any]),
    {noreply, State}.

terminate(Reason, #state{nodes=Nodes}) ->
    %% case observer_wx:try_rpc(Node, erlang, whereis, [dbg]) of
    %% 	undefined -> fine;
    %% 	Pid -> exit(Pid, kill)
    %% end,
    ok.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_add_processes(POpts, S0=#state{p_view=LCtrl, tpids=OldPids, nodes=Ns0}) ->
    case merge_pids(POpts, OldPids) of
	{OldPids, [], []} ->
	    S0;
	{Pids, New, Changed} ->
	    update_process_view(Pids, LCtrl),
	    Ns1 = lists:usort([node(Pid) || #tpid{pid=Pid} <- New, is_pid(Pid)]),
	    Nodes = case ordsets:subtract(Ns1, Ns0) of
			[] -> Ns0; %% No new Nodes
			NewNs ->
			    %% Handle new nodes
			    %% BUGBUG add trace patterns for new nodes
			    ordsets:union(NewNs, Ns0)
		    end,
	    S0#state{tpids=Pids, nodes=Nodes}
    end.

update_process_view(Pids, LCtrl) ->
    wxListCtrl:deleteAllItems(LCtrl),
    wx:foldl(fun(#tpid{pid=Pid, opts=Opts}, Row) ->
		     _Item = wxListCtrl:insertItem(LCtrl, Row, ""),
		     ?EVEN(Row) andalso
			 wxListCtrl:setItemBackgroundColour(LCtrl, Row, ?BG_EVEN),
		     wxListCtrl:setItem(LCtrl, Row, 0, observer_lib:to_str(Pid)),
		     wxListCtrl:setItem(LCtrl, Row, 1, observer_lib:to_str(Opts)),
		     Row+1
	     end, 0, Pids).

do_add_patterns({Module, NewPs}, State=#state{tpatterns=TPs0, m_view=Mview, f_view=Fview}) ->
    Old = case dict:find(Module, TPs0) of
	      {ok, Prev}  -> Prev;
	      error -> []
	  end,
    case merge_patterns(NewPs, Old) of
	{Old, [], []} ->
	    State;
	{MPatterns, New, Changed} ->
	    TPs = dict:store(Module, MPatterns, TPs0),
	    update_modules_view(lists:sort(dict:fetch_keys(TPs)), Module, Mview),
	    update_functions_view(dict:fetch(Module, TPs), Fview),
	    State#state{tpatterns=TPs}
    end.

update_modules_view(Mods, Module, LCtrl) ->
    wxListCtrl:deleteAllItems(LCtrl),
    wx:foldl(fun(Mod, Row) ->
		     _Item = wxListCtrl:insertItem(LCtrl, Row, ""),
		     ?EVEN(Row) andalso
			 wxListCtrl:setItemBackgroundColour(LCtrl, Row, ?BG_EVEN),
		     wxListCtrl:setItem(LCtrl, Row, 0, observer_lib:to_str(Mod)),
		     (Mod =:= Module) andalso
			 wxListCtrl:setItemState(LCtrl, Row, 16#FFFF, ?wxLIST_STATE_SELECTED),
		     Row+1
	     end, 0, Mods).

update_functions_view(Funcs, LCtrl) ->
    wxListCtrl:deleteAllItems(LCtrl),
    wx:foldl(fun(#tpattern{fa=FA, ms=#match_spec{str=Ms}}, Row) ->
		     _Item = wxListCtrl:insertItem(LCtrl, Row, ""),
		     ?EVEN(Row) andalso wxListCtrl:setItemBackgroundColour(LCtrl, Row, ?BG_EVEN),
		     wxListCtrl:setItem(LCtrl, Row, 0, observer_lib:to_str({func,FA})),
		     wxListCtrl:setItem(LCtrl, Row, 1, Ms),
		     Row+1
	     end, 0, Funcs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge_pids([N1=#tpid{pid=new}|Ns], [N2=#tpid{pid=new}|Old]) ->
    {Pids, New, Changed} = merge_pids_1(Ns,Old),
    {[N1|Pids], New, [{N2,N2}|Changed]};
merge_pids([N1=#tpid{pid=new}|Ns], Old) ->
    {Pids, New, Changed} = merge_pids_1(Ns,Old),
    {[N1|Pids], [N1|New], Changed};
merge_pids(Ns, [N2=#tpid{pid=new}|Old]) ->
    {Pids, New, Changed} = merge_pids_1(Ns,Old),
    {[N2|Pids], New, Changed};
merge_pids(New, Old) ->
    merge_pids_1(New, Old).

merge_pids_1(New, Old) ->
    merge(lists:sort(New), Old, #tpid.pid, [], [], []).

merge_patterns(New, Old) ->
    merge(lists:sort(New), Old, #tpattern.fa, [], [], []).


merge([N|Ns], [N|Os], El, New, Ch, All) ->
    merge(Ns, Os, El, New, Ch, [N|All]);
merge([N|Ns], [O|Os], El, New, Ch, All)
  when element(El, N) == element(El, O) ->
    merge(Ns, Os, El, New, [{O,N}|Ch], [N|All]);
merge([N|Ns], Os=[O|_], El, New, Ch, All)
  when element(El, N) < element(El, O) ->
    merge(Ns, Os, El, [N|New], Ch, [N|All]);
merge(Ns=[N|_], [O|Os], El, New, Ch, All)
  when element(El, N) > element(El, O) ->
    merge(Ns, Os, El, New, Ch, [O|All]);
merge([], Os, _El, New, Ch, All) ->
    {lists:reverse(All, Os), New, Ch};
merge(Ns, [], _El, New, Ch, All) ->
    {lists:reverse(All, Ns), Ns++New, Ch}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_trace(Node, TracedProcs, TracedDict,
	    #trace_options{send = Send, treceive = Receive, functions = Functions,
			   events = Events, on_1st_spawn = On1Spawn,
			   on_all_spawn = AllSpawn, on_1st_link = On1Link,
			   on_all_link = AllLink}) ->
    dbg:stop_clear(),
    MyPid = self(),
    HandlerFun = fun(NewMsg, _) ->
			 MyPid ! NewMsg
		 end,
    dbg:tracer(process, {HandlerFun, []}),

    case Node =:= node() of
	true ->
	    ok;
	false ->
	    dbg:n(Node)
    end,

    Recs = [{Send, send},
	    {Receive, 'receive'},
	    {Functions, call},
	    {Events, procs},
	    {On1Spawn, set_on_first_spawn},
	    {AllSpawn, set_on_spawn},
	    {On1Link, set_on_first_link},
	    {AllLink, set_on_link}],
    Flags = [Assoc || {true, Assoc} <- Recs],

    case TracedProcs of
	all ->
	    dbg:p(all, Flags);
	new ->
	    dbg:p(new, Flags);
	_Pids ->
	    lists:foreach(fun(Pid) -> dbg:p(Pid, Flags) end, TracedProcs)
    end,

    case Functions of
	true ->
	    trace_functions(TracedDict);
	false ->
	    ok
    end.

textformat({died, Pid}) ->
    io_lib:format("~w Process died.~n",[Pid]);
textformat({shell_died, Old, New}) ->
    io_lib:format("~w Shell Process died. Restarted as ~w~n~n",[Old,New]);
textformat({trace, From, 'receive', Msg}) ->
    io_lib:format("~w: rec   ~s~n", [From,
				     tuple_space(Msg)]);
textformat({trace, From, send, Msg, To}) ->
    io_lib:format("~w:  !    To: ~w Msg: ~s~n", [From,
						 To,
						 tuple_space(Msg)]);
textformat({trace, From, call, Func}) ->
    io_lib:format("~w: call  ~s~n",[From, ffunc(Func)]);
textformat({trace, From, spawn, Data}) ->
    io_lib:format("~w: spawn ~p~n", [From, Data]);
textformat({trace, From, link, Data}) ->
    io_lib:format("~w: link  ~p~n", [From,  Data]);
textformat({trace, From, unlink, Data}) ->
    io_lib:format("~w: U-lnk ~p~n", [From,  Data]);

textformat({trace, From, Op, Data}) ->
    io_lib:format("~w: ~w   ~p~n", [From, Op, Data]);

textformat({print, Format, Args}) ->
    io_lib:format(Format, Args);
textformat(Other) ->
    io_lib:format("~p~n",[Other]).


tuple_space(X) when is_tuple(X) -> print(tuple_size(X), X, "}");
tuple_space(X)                  -> io_lib:format("~p",[X]).


ffunc({M,F, Argl}) ->
    io_lib:format("~w:~w(~s)", [M, F, fargs(Argl)]);
ffunc(X) -> tuple_space(X).

fargs([]) -> [];
fargs([A]) -> tuple_space(A);  %% last arg
fargs([A|Args]) -> [tuple_space(A),", "|fargs(Args)].

print(0 , _X, Buff) -> ["{"|Buff];
print(1 , X, Buff) ->
    Str =  tuple_space(element(1, X)),
    ["{",Str|Buff];
print(Num, X, Buff) ->
    Str =  tuple_space(element(Num, X)),
    print(Num-1, X, [", ",Str|Buff]).

trace_functions(TracedDict) ->
    Trace = fun(KeyAtom, RecordList, acc_in) ->
		    lists:foreach(fun(#traced_func{func_name = Function,
						   arity = Arity,
						   match_spec = #match_spec{term = MS}}) ->
					  dbg:tpl({KeyAtom, Function, Arity}, MS)
				  end,
				  RecordList),
		    acc_in
	    end,
    dict:fold(Trace, acc_in, TracedDict).

write_file(Frame, Filename, TraceOps, MatchSpecs, TPs) ->
    FormatMS = fun(#match_spec{name=Id, term=T, func=F}) ->
		       io_lib:format("[{name,\"~s\"}, {term, ~w}, {func, \"~s\"}]",
				     [Id, T, F])
	       end,
    FormatTP = fun({Module, FTPs}) ->
		       List = format_ftp(FTPs, FormatMS),
		       io_lib:format("{tp, ~w, [~s]}.~n",[Module, List])
	       end,
    Str =
	["%%%\n%%% This file is generated by Observer\n",
	 "%%%\n%%% DO NOT EDIT!\n%%%\n",
	 [["{ms, ", FormatMS(Ms), "}.\n"] || Ms <- MatchSpecs],
	 "{traceopts, ", io_lib:format("~w",[TraceOps]) ,"}.\n",
	 [FormatTP(TP) || TP <- TPs]
	],
    case file:write_file(Filename, list_to_binary(Str)) of
	ok ->
	    success;
	{error, Reason} ->
	    FailMsg = file:format_error(Reason),
	    observer_wx:create_txt_dialog(Frame, FailMsg, "Error", ?wxICON_ERROR)
    end.

format_ftp([#tpattern{fa={F,A}, ms=Ms}], FormatMS) ->
    io_lib:format("{~w, ~w, ~s}", [F,A,FormatMS(Ms)]);
format_ftp([#tpattern{fa={F,A}, ms=Ms}|Rest], FormatMS) ->
    [io_lib:format("{~w, ~w, ~s},~n     ", [F,A,FormatMS(Ms)]),
     format_ftp(Rest, FormatMS)].

read_settings(Filename, #state{match_specs=Ms0, def_trace_opts=TO0} = State) ->
    case file:consult(Filename) of
	{ok, Terms} ->
	    Ms  = lists:usort(Ms0 ++ [parse_ms(MsList) || {ms, MsList} <- Terms]),
	    TOs = lists:usort(TO0 ++ proplists:get_value(traceopts, Terms, [])),
	    lists:foldl(fun parse_tp/2,
			State#state{match_specs=Ms, def_trace_opts=TOs},
			Terms);
	{error, _} ->
	    observer_wx:create_txt_dialog(State#state.panel, "Could not load settings",
					  "Error", ?wxICON_ERROR),
	    State
    end.

parse_ms(Opts) ->
    Name = proplists:get_value(name, Opts, "TracePattern"),
    Term = proplists:get_value(term, Opts, [{'_',[],[ok]}]),
    FunStr = proplists:get_value(term, Opts, "fun(_) -> ok end"),
    make_ms(Name, Term, FunStr).

make_ms(Name, Term, FunStr) ->
    #match_spec{name=Name, term=Term, str=io_lib:format("~w", Term), func = FunStr}.

parse_tp({tp, Mod, FAs}, State) ->
    Patterns = [#tpattern{m=Mod,fa={F,A}, ms=parse_ms(List)} ||
		   {F,A,List} <- FAs],
    do_add_patterns({Mod, Patterns}, State);
parse_tp(_, State) ->
    State.
