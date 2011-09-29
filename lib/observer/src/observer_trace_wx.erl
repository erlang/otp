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

-export([start/6]).
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


-record(state, {
	  parent,
	  frame,
	  text_ctrl,
	  trace_options,
	  toggle_button,
	  node,
	  traceoptions_open,
	  traced_procs,
	  traced_funcs = dict:new(), % Key =:= Module::atom, Value =:= [ #traced_func  ]
	  match_specs = []}). % [ #match_spec{} ]


start(Node, TracedProcs, TraceOpts, MatchSpecs, ParentFrame, ParentPid) ->
    wx_object:start(?MODULE, [Node, TracedProcs, TraceOpts, MatchSpecs, ParentFrame, ParentPid], []).

init([Node, TracedProcs, TraceOpts, MatchSpecs, ParentFrame, ParentPid]) ->
    State =
	wx:batch(fun() ->
			 create_window(ParentFrame, TraceOpts)
		 end),

    Frame = State#state.frame,
    TraceOpts2 = State#state.trace_options,
    TracedFuncs = State#state.traced_funcs,

    wx_object:start(observer_traceoptions_wx,
		    [Frame, self(), Node, TraceOpts2, TracedFuncs, MatchSpecs],
		    []),

    {Frame, State#state{parent = ParentPid,
			node = Node,
			traced_procs = TracedProcs,
			match_specs = MatchSpecs,
			traceoptions_open = true}}.


create_window(ParentFrame, TraceOpts) ->
    %% Create the window
    Frame = wxFrame:new(ParentFrame, ?wxID_ANY, "Tracer",
			[{style, ?wxDEFAULT_FRAME_STYLE},
			 {size, {900, 900}}]),
    wxFrame:connect(Frame, close_window,[{skip,true}]),
    Panel = wxPanel:new(Frame, []),
    Sizer = wxBoxSizer:new(?wxVERTICAL),

    %% Menues
    MenuBar = wxMenuBar:new(),
    create_menues(MenuBar),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxMenu:connect(Frame, command_menu_selected, []),

    %% Buttons
    ToggleButton = wxToggleButton:new(Panel, ?wxID_ANY, "Start Trace", []),
    wxSizer:add(Sizer, ToggleButton, [{flag, ?wxALL},
				      {border, 5}]),
    wxMenu:connect(ToggleButton, command_togglebutton_clicked, []),

    TxtCtrl =  wxTextCtrl:new(Panel, ?wxID_ANY,
			      [{style,?wxTE_READONLY bor
				    ?wxTE_MULTILINE},
			       {size, {400, 300}}]),

    wxSizer:add(Sizer, TxtCtrl, [{proportion, 1},
				 {flag, ?wxEXPAND}]),

    %% Display window
    wxWindow:setSizer(Panel, Sizer),
    wxFrame:show(Frame),
    #state{frame = Frame,
	   text_ctrl = TxtCtrl,
	   toggle_button = ToggleButton,
	   trace_options = TraceOpts#trace_options{main_window = false}}.

create_menues(MenuBar) ->
    observer_wx:create_menu(
      [
       {"File", [
		 #create_menu{id = ?LOAD_TRACEOPTS, text = "Load settings"},
		 #create_menu{id = ?SAVE_TRACEOPTS, text = "Save settings"},
		 separator,
		 #create_menu{id = ?SAVE_BUFFER, text = "Save buffer"},
		 separator,
		 #create_menu{id = ?CLOSE, text = "Close"}
		]},
       {"View", [
		 #create_menu{id = ?CLEAR, text = "Clear buffer"}
		]},
       {"Options", [
		    #create_menu{id = ?OPTIONS, text = "Trace options"}
		   ]}
      ],
      MenuBar).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
						%Main window

handle_event(#wx{id = ?CLOSE, event = #wxCommand{type = command_menu_selected}},
	     #state{parent = Parent,
		    trace_options = TraceOpts,
		    match_specs = MatchSpecs} = State) ->
    Parent ! {tracemenu_closed, TraceOpts, MatchSpecs},
    {stop, shutdown, State};

handle_event(#wx{id = ?OPTIONS, event = #wxCommand{type = command_menu_selected}},
	     #state{frame = Frame, trace_options = TraceOpts,
		    traced_funcs = TracedFuncs,
		    node = Node,
		    match_specs = MatchSpecs,
		    traceoptions_open = false} = State) ->

    wx_object:start(observer_traceoptions_wx,
		    [Frame, self(),  Node, TraceOpts, TracedFuncs, MatchSpecs],
		    []),

    {noreply, State#state{traceoptions_open = true}};

handle_event(#wx{id = ?CLEAR, event = #wxCommand{type = command_menu_selected}},
	     #state{text_ctrl = TxtCtrl} = State) ->
    wxTextCtrl:clear(TxtCtrl),
    {noreply, State};

handle_event(#wx{id = ?SAVE_BUFFER, event = #wxCommand{type = command_menu_selected}},
	     #state{frame = Frame, text_ctrl = TxtCtrl} = State) ->
    Dialog = wxFileDialog:new(Frame, [{style, ?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT}]),
    case wxFileDialog:showModal(Dialog) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(Dialog),
	    wxDialog:destroy(Dialog),
	    case filelib:is_file(Path) of
		true ->
		    observer_wx:create_txt_dialog(Frame, "File already exists: " ++ Path ++ "\n",
						  "Error", ?wxICON_ERROR);
		false ->
		    wxTextCtrl:saveFile(TxtCtrl, [{file, Path}])
	    end;
	_ ->
	    wxDialog:destroy(Dialog),
	    ok
    end,
    {noreply, State};

handle_event(#wx{id = ?SAVE_TRACEOPTS,
		 event = #wxCommand{type = command_menu_selected}},
	     #state{frame = Frame,
		    trace_options = TraceOpts,
		    match_specs = MatchSpecs} = State) ->
    Dialog = wxFileDialog:new(Frame, [{style, ?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT}]),
    case wxFileDialog:showModal(Dialog) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(Dialog),
	    write_file(Frame, Path, TraceOpts, MatchSpecs);
	_ ->
	    ok
    end,
    wxDialog:destroy(Dialog),
    {noreply, State};

handle_event(#wx{id = ?LOAD_TRACEOPTS,
		 event = #wxCommand{type = command_menu_selected}},
	     #state{frame = Frame} = State) ->
    Dialog = wxFileDialog:new(Frame, [{style, ?wxFD_FILE_MUST_EXIST}]),
    State2 = case wxFileDialog:showModal(Dialog) of
		 ?wxID_OK ->
		     Path = wxFileDialog:getPath(Dialog),
		     read_settings(Path, State);
		 _ ->
		     State
	     end,
    wxDialog:destroy(Dialog),
    {noreply, State2};


handle_event(#wx{event = #wxClose{type = close_window}},
	     #state{parent = Parent,
		    trace_options = TraceOpts,
		    match_specs = MatchSpecs} = State) ->
    Parent ! {tracemenu_closed, TraceOpts, MatchSpecs},
    {stop, shutdown, State};

handle_event(#wx{event = #wxCommand{type = command_togglebutton_clicked, commandInt = 1}},
	     #state{node = Node,
		    traced_procs = TracedProcs,
		    traced_funcs = TracedDict,
		    trace_options = TraceOpts,
		    text_ctrl = TextCtrl,
		    toggle_button = ToggleBtn} = State) ->

    start_trace(Node, TracedProcs, TracedDict, TraceOpts),
    wxTextCtrl:appendText(TextCtrl, "Start Trace:\n"),
    wxToggleButton:setLabel(ToggleBtn, "Stop Trace"),
    {noreply, State};

handle_event(#wx{event = #wxCommand{type = command_togglebutton_clicked, commandInt = 0}}, %%Stop tracing
	     #state{text_ctrl = TxtCtrl,
		    toggle_button = ToggleBtn} = State) ->
    dbg:stop_clear(),
    wxTextCtrl:appendText(TxtCtrl, "Stop Trace.\n"),
    wxToggleButton:setLabel(ToggleBtn, "Start Trace"),
    {noreply, State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{event = What}, State) ->
    io:format("~p~p: Unhandled event: ~p ~n", [?MODULE, self(), What]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({updated_traceopts,
	     TraceOpts,
	     MatchSpecs,
	     TracedFuncs}, State) ->
    {noreply, State#state{trace_options = TraceOpts,
			  match_specs = MatchSpecs,
			  traced_funcs = TracedFuncs,
			  traceoptions_open = false}};

handle_info(traceopts_closed, State) ->
    {noreply, State#state{traceoptions_open = false}};

handle_info(Tuple, #state{text_ctrl = TxtCtrl} = State) when is_tuple(Tuple) ->
    Text = textformat(Tuple),
    wxTextCtrl:appendText(TxtCtrl, lists:flatten(Text)),
    {noreply, State};

handle_info(Any, State) ->
    io:format("~p~p: received unexpected message: ~p\n", [?MODULE, self(), Any]),
    {noreply, State}.


terminate(Reason, #state{node = Node,
			 frame = Frame}) ->
    try
	case observer_wx:try_rpc(Node, erlang, whereis, [dbg]) of
	    undefined -> fine;
	    Pid -> exit(Pid, kill)
	end,
	io:format("~p terminating tracemenu. Reason: ~p~n", [?MODULE, Reason]),
	wxFrame:destroy(Frame),
	ok
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(Frame, Node),
	    wxFrame:destroy(Frame)
    end.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

handle_call(Msg, _From, State) ->
    io:format("~p~p: Got Call ~p~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("~p ~p: Unhandled cast ~p~n", [?MODULE, ?LINE, Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


tuple_space(X) when is_tuple(X) -> print(size(X), X, "}");
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
						   match_spec = #match_spec{term_ms = MS}}) ->
					  dbg:tpl({KeyAtom, Function, Arity}, MS)
				  end,
				  RecordList),
		    acc_in
	    end,
    dict:fold(Trace, acc_in, TracedDict).


write_file(Frame, Filename, #trace_options{send = Send,
					   treceive = Receive,
					   functions = Functions,
					   events = Events,
					   on_1st_spawn = On1stSpawn,
					   on_all_spawn = OnAllSpawn,
					   on_1st_link = On1stLink,
					   on_all_link = OnAllLink},
	   MatchSpecs) ->

    FormattedMatchSpecs = lists:flatten(lists:foldl(
					  fun(#match_spec{alias = A, term_ms = T, fun2ms = F}, Acc) ->
						  [io_lib:format("{alias, ~p, term_ms, ~p, fun2ms, ~p}.\n",
								 [A, T, F]) | Acc]
					  end, [], MatchSpecs)),

    Binary =
	list_to_binary("%%%\n%%% This file is generated by Observer\n"
		       "%%%\n%%% DO NOT EDIT!\n%%%\n"
		       "{send, " ++ atom_to_list(Send) ++ "}.\n"
		       "{treceive, " ++ atom_to_list(Receive) ++ "}.\n"
		       "{functions, " ++ atom_to_list(Functions) ++ "}.\n"
		       "{events, " ++ atom_to_list(Events) ++ "}.\n"
		       "{on_1st_spawn, " ++ atom_to_list(On1stSpawn) ++ "}.\n"
		       "{on_all_spawn, " ++ atom_to_list(OnAllSpawn) ++ "}.\n"
		       "{on_1st_link, " ++ atom_to_list(On1stLink) ++ "}.\n"
		       "{on_all_link, " ++ atom_to_list(OnAllLink) ++ "}.\n"
		       ++ FormattedMatchSpecs),

    case file:write_file(Filename, Binary) of
	ok ->
	    success;
	{error, Reason} ->
	    FailMsg = file:format_error(Reason),
	    observer_wx:create_txt_dialog(Frame, FailMsg, "Error", ?wxICON_ERROR)
    end.


read_settings(Filename, #state{frame = Frame} = State) ->
    case file:consult(Filename) of
	{ok, Terms} ->
	    {TraceOpts, MatchSpecs} = parse_settings(Terms, {#trace_options{}, []}),
	    State#state{trace_options = TraceOpts, match_specs = MatchSpecs};
	{error, _} ->
	    observer_wx:create_txt_dialog(Frame, "Could not load settings", "Error", ?wxICON_ERROR),
	    State
    end.


parse_settings([], {TraceOpts, MatchSpecs}) ->
    {TraceOpts, MatchSpecs};
parse_settings([{send, Bool} | T], {Opts, MS}) ->
    parse_settings(T, {Opts#trace_options{send = Bool}, MS});
parse_settings([{treceive, Bool} | T], {Opts, MS}) ->
    parse_settings(T, {Opts#trace_options{treceive = Bool}, MS});
parse_settings([{functions, Bool} | T], {Opts, MS}) ->
    parse_settings(T, {Opts#trace_options{functions = Bool}, MS});
parse_settings([{events, Bool} | T], {Opts, MS}) ->
    parse_settings(T, {Opts#trace_options{events = Bool}, MS});
parse_settings([{on_1st_spawn, Bool} | T], {Opts, MS}) ->
    parse_settings(T, {Opts#trace_options{on_1st_spawn = Bool}, MS});
parse_settings([{on_all_spawn, Bool} | T], {Opts, MS}) ->
    parse_settings(T, {Opts#trace_options{on_all_spawn = Bool}, MS});
parse_settings([{on_1st_link, Bool} | T], {Opts, MS}) ->
    parse_settings(T, {Opts#trace_options{on_1st_link = Bool}, MS});
parse_settings([{on_all_link, Bool} | T], {Opts, MS}) ->
    parse_settings(T, {Opts#trace_options{on_all_link = Bool}, MS});
parse_settings([{alias, A, term_ms, TermMS, fun2ms, F} | T], {Opts, MatchSpecs}) ->
    Alias = case A of
		undefined -> A;
		_ -> lists:flatten(io_lib:format("~s", [A]))
	    end,
    Fun2MS = case F of
		 undefined -> F;
		 _ -> lists:flatten(io_lib:format("~s", [F]))
	     end,
    parse_settings(T, {Opts, [#match_spec{alias = Alias,
					  term_ms = TermMS,
					  str_ms = lists:flatten(io_lib:format("~p", [TermMS])),
					  fun2ms = Fun2MS}
			      | MatchSpecs]}).
