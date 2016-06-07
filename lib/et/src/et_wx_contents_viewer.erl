%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Displays details of a trace event
%%----------------------------------------------------------------------

-module(et_wx_contents_viewer).

-behaviour(wx_object).

%% External exports
-export([start_link/1, 
         stop/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2,
	 handle_event/2]).

-include("../include/et.hrl").
-include("et_internal.hrl").
-include_lib("wx/include/wx.hrl").

-record(state, {parent_pid,     % Pid of parent process
                viewer_pid,     % Pid of viewer process
                event_order,    % Field to be used as primary key
                event,          % The original event
                filtered_event, % Event processed by active filter
                active_filter,  % Name of the active filter
                filters,        % List of possible filters
                win,            % GUI: Frame object
                frame,          % GUI: Frame object
                panel,          % GUI: Panel object
                width,          % GUI: Window width
                height,
		editor,
		menu_data,      % GUI: Window height
		wx_debug,       % GUI: WX debug level
		trap_exit}).    % trap_exit process flag

%%%----------------------------------------------------------------------
%%% Client side
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% start_link(Options) -> {ok, ContentsPid} | {error, Reason}
%%
%% Start a viewer for the event contents as window in GS
%%
%% Options = [option()]
%% 
%% option() =
%% 
%%   {parent_pid, pid()}          |  % Pid of parent process
%%   {viewer_pid, pid()}          |  % Pid of viewer process
%%   {event_order, event_order()} |  % Field to be used as primary key 
%%   {active_filter, atom()}      |  % Name of the active filter
%%   {filter, atom(), fun()}         % A named filter fun
%%   
%% event_order() = 'trace_ts' | 'event_ts'
%% ContentsPid = pid()
%% Reason = term()
%%----------------------------------------------------------------------

start_link(Options) ->
    case parse_opt(Options, default_state()) of
        {ok, S} ->
	    try
		WxRef = wx_object:start_link(?MODULE, [S], []),
		Pid = wx_object:get_pid(WxRef),
		if
		    S#state.parent_pid =/= self() ->
			unlink(Pid);
		    true ->
			ok
		end,
		{ok, Pid}
	    catch
		error:Reason ->
		    {error, {'EXIT', Reason, erlang:get_stacktrace()}}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

default_state() ->
    #state{parent_pid    = self(),
           viewer_pid    = undefined,
           active_filter = ?DEFAULT_FILTER_NAME,
           filters       = [?DEFAULT_FILTER],
           width         = 600,
           height        = 300,
	   wx_debug      = 0,
	   trap_exit     = true}.

parse_opt([], S) ->
    Name = S#state.active_filter,
    Filters = S#state.filters,
    if
        S#state.event =:= undefined ->
            {error, {badarg, no_event}};
        is_atom(Name) ->
            case lists:keysearch(Name, #filter.name, Filters) of
                {value, F} when is_record(F, filter) ->
                    {ok, S#state{active_filter = Name}};
                false ->
                    {error, {badarg, {no_such_filter, Name, Filters}}}
            end
    end;
parse_opt([H | T], S) ->
    case H of
        {parent_pid, ParentPid} when is_pid(ParentPid); ParentPid =:= undefined ->
            parse_opt(T, S#state{parent_pid = ParentPid});
        {viewer_pid, ViewerPid} when is_pid(ViewerPid) ->
            parse_opt(T, S#state{viewer_pid = ViewerPid});
	{wx_debug, Level} ->
            parse_opt(T, S#state{wx_debug = Level});
	{trap_exit, Bool} when Bool =:= true; Bool =:= false->
            parse_opt(T, S#state{trap_exit = Bool});
        {event_order, trace_ts} ->
            parse_opt(T, S#state{event_order = trace_ts});
        {event_order, event_ts} ->
            parse_opt(T, S#state{event_order = event_ts});
        {event, Event} when is_record(Event, event) ->
            parse_opt(T, S#state{event = Event});
        {active_filter, Name} when is_atom(Name) ->
            parse_opt(T, S#state{active_filter = Name});
        F when is_record(F, filter),
               is_atom(F#filter.name),
               is_function(F#filter.function) ->
            Filters = lists:keydelete(F#filter.name, #filter.name, S#state.filters),
            Filters2 = lists:keysort(#filter.name, [F | Filters]),
            parse_opt(T, S#state{filters = Filters2});
        {width, Width} when is_integer(Width), Width > 0 ->
            parse_opt(T, S#state{width = Width});
        {height, Height} when is_integer(Height), Height > 0 ->
            parse_opt(T, S#state{height = Height});
        Bad ->
            {error, {bad_option, Bad}}
    end;
parse_opt(BadList, _S) ->
    {error, {bad_option_list, BadList}}.

%%----------------------------------------------------------------------
%% stop(ContentsPid) -> ok
%%
%% Stops a contents viewer process
%%
%% ContentsPid = pid()
%%----------------------------------------------------------------------

stop(ContentsPid) when is_pid(ContentsPid) ->
    Type = process,
    MonitorRef = erlang:monitor(Type, ContentsPid),
    ContentsPid ! {stop, self()},
    receive
	{'DOWN', MonitorRef, Type, ContentsPid, shutdown} ->
	    ok;
	{'DOWN', MonitorRef, Type, ContentsPid, Reason} ->
	    {error, Reason}
    end.

%% call(Frame, Request) ->
%%     wx_object:call(Frame, Request, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------

init([S]) when is_record(S, state) ->
    process_flag(trap_exit, S#state.trap_exit),
    case S#state.parent_pid of
	undefined -> ok;
	ParentPid -> link(ParentPid)
    end,
    wx:debug(S#state.wx_debug),
    S2 = create_window(S),
    {S2#state.frame, S2}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call(Request, From, S) ->
    ok = error_logger:format("~p(~p): handle_call(~p, ~p, ~p)~n",
                             [?MODULE, self(), Request, From, S]),
    Reply = {error, {bad_request, Request}},
    {reply, Reply, S}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_cast(Msg, S) ->
    ok = error_logger:format("~p(~p): handle_cast(~p, ~p)~n",
                             [?MODULE, self(), Msg, S]),
    {noreply, S}.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_event(#wx{id = Id,
		 event = #wxCommand{type = command_menu_selected}}, 
	     S) ->
    case proplists:get_value(Id, S#state.menu_data) of
	undefined ->
	    ok;
        Data when is_record(Data, filter) ->
            F = Data,
            ChildState = S#state{active_filter = F#filter.name},
            _ = wx_object:start_link(?MODULE, [ChildState], []),
            ok;
        {hide, Actors} ->
            send_viewer_event(S, {delete_actors, Actors});
        {show, Actors} ->
            send_viewer_event(S, {insert_actors, Actors});
        {mode, Mode} ->
            send_viewer_event(S, {mode, Mode});
        Nyi ->
            ok = error_logger:format("~p: click ~p ignored (nyi)~n",
                                     [?MODULE, Nyi])
    end,
    case Id of
	?wxID_EXIT ->
	    wxFrame:destroy(S#state.frame),
	    opt_unlink(S#state.parent_pid),
	    {stop, shutdown, S};
        ?wxID_SAVE ->
            Event = S#state.event,
            TimeStamp = 
                case S#state.event_order of
                    trace_ts -> Event#event.trace_ts;
                    event_ts -> Event#event.event_ts
                end,
            FileName = lists:flatten(["et_contents_viewer_", now_to_string(TimeStamp), ".txt"]),
	    Style = ?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT,
	    Msg = "Select a file to the events to",
	    case select_file(S#state.frame, Msg, filename:absname(FileName), Style) of
		{ok, FileName2} ->
		    Bin = list_to_binary(event_to_string(Event, S#state.event_order)),
		    ok = file:write_file(FileName2, Bin);
		cancel ->
		    ok
	    end,
            {noreply, S};
	?wxID_PRINT ->
	    Html = wxHtmlEasyPrinting:new([{parentWindow, S#state.win}]),
	    Text =  "<pre>" ++ wxTextCtrl:getValue(S#state.editor) ++ "</pre>",
	    wxHtmlEasyPrinting:previewText(Html, Text),
	    {noreply, S};
	_ ->
	    {noreply, S}
    end;
handle_event(#wx{event = #wxKey{rawCode = KeyCode}}, S) ->
    case KeyCode of
        $c ->
	    wxFrame:destroy(S#state.frame),
	    opt_unlink(S#state.parent_pid),
            {stop, normal, S};
        $f ->
            E    = S#state.filtered_event,
            From = E#event.from,
            send_viewer_event(S, {delete_actors, [From]}),
            {noreply, S};
        $t ->
            E  = S#state.filtered_event,
            To = E#event.to,
            send_viewer_event(S, {delete_actors, [To]}),
            {noreply, S};
        $b ->
            E    = S#state.filtered_event,
            From = E#event.from,
            To   = E#event.to,
            send_viewer_event(S, {delete_actors, [From, To]}),
            {noreply, S};
        
        $F ->
            E    = S#state.filtered_event,
            From = E#event.from,
            send_viewer_event(S, {insert_actors, [From]}),
            {noreply, S};
        $T ->
            E  = S#state.filtered_event,
            To = E#event.to,
            send_viewer_event(S, {insert_actors, [To]}),
            {noreply, S};
        $B ->
            E    = S#state.filtered_event,
            From = E#event.from,
            To   = E#event.to,
            send_viewer_event(S, {insert_actors, [From, To]}),
            {noreply, S};

        $s ->
            E     = S#state.filtered_event,
            From  = E#event.from,
            To    = E#event.to,
            First = et_collector:make_key(S#state.event_order, E),
            Mode  = {search_actors, forward, First, [From, To]},
            send_viewer_event(S, {mode, Mode}),
            {noreply, S};
        $r ->
            E     = S#state.filtered_event,
            From  = E#event.from,
            To    = E#event.to,
            First = et_collector:make_key(S#state.event_order, E),
            Mode  = {search_actors, reverse, First, [From, To]},
            send_viewer_event(S, {mode, Mode}),
            {noreply, S};
        $a ->
            send_viewer_event(S, {mode, all}),
            {noreply, S};

        $0 ->
            case lists:keysearch(?DEFAULT_FILTER_NAME, #filter.name, S#state.filters) of
                {value, F} when is_record(F, filter) ->
                    ChildState = S#state{active_filter = F#filter.name},
                    _ = wx_object:start_link(?MODULE, [ChildState], []),
                    ok;
                false ->
                    ok
            end,
            {noreply, S};
        Int when is_integer(Int), Int > $0, Int =< $9 ->
            case catch lists:nth(Int-$0, S#state.filters) of
                F when is_record(F, filter) ->
                    ChildState = S#state{active_filter = F#filter.name},
                    _ = wx_object:start_link(?MODULE, [ChildState], []),
                    ok;
                {'EXIT', _} ->
                    ok
            end,
            {noreply, S};

        _ ->
            io:format("~p: ignored: ~p~n", [?MODULE, KeyCode]),
            {noreply, S}
    end;
handle_event(#wx{event = #wxClose{}}, S) ->
    opt_unlink(S#state.parent_pid),
    {stop, shutdown, S};
handle_event(#wx{event = #wxSize{size = {W, H}}}, S) ->
    S2 = S#state{width = W, height = H},
    {noreply, S2};
handle_event(Wx = #wx{}, S) ->
    io:format("~p got an unexpected event: ~p\n", [self(), Wx]),
    {noreply, S}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info({stop, _From}, S) ->
    wxFrame:destroy(S#state.frame),
    opt_unlink(S#state.parent_pid),
    {stop, shutdown, S};
handle_info({'EXIT', Pid, Reason}, S) ->
    if
        Pid =:= S#state.parent_pid ->
	    wxFrame:destroy(S#state.frame),
	    opt_unlink(S#state.parent_pid),
            {stop, Reason, S};
        true ->
            {noreply, S}
    end;
handle_info(Info, S) ->
    ok = error_logger:format("~p(~p): handle_info(~p, ~p)~n",
                             [?MODULE, self(), Info, S]),
    {noreply, S}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------

terminate(_Reason, _S) ->
    ignore.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%----------------------------------------------------------------------
%%% Handle graphics
%%%----------------------------------------------------------------------

opt_unlink(Pid) ->
    if
	Pid =:= undefined ->
	    ignore;
	true ->
	    unlink(Pid)
    end.

create_window(S) ->
    H = S#state.height,
    W = S#state.width,
    Name = S#state.active_filter,
    Title = lists:concat([?MODULE, " (filter: ", Name, ")"]),
    WinOpt = [{size, {W,H}}],
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, Title, WinOpt),
    _ = wxFrame:createStatusBar(Frame),

    Panel = wxPanel:new(Frame, []),
    Bar = wxMenuBar:new(),
    _ = wxFrame:setMenuBar(Frame,Bar),
    create_file_menu(Bar),
    Editor = wxTextCtrl:new(Panel, ?wxID_ANY, [{style, 0
						bor ?wxDEFAULT
						bor ?wxTE_RICH2 %% Needed on Windows
						bor ?wxTE_MULTILINE
						bor ?wxTE_READONLY
						bor ?wxTE_DONTWRAP}]),
    Font = wxFont:new(10, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[]),
    TextAttr = wxTextAttr:new(?wxBLACK, [{font, Font}]),
    _ = wxTextCtrl:setDefaultStyle(Editor, TextAttr),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    _ = wxSizer:add(Sizer, Editor, [{flag, ?wxEXPAND}, {proportion, 1}]),
    FilteredEvent = config_editor(Editor, S),
    S2 = S#state{win = Frame, panel = Panel, filtered_event = FilteredEvent},
    HideData = create_hide_menu(Bar, S2),
    SearchData = create_search_menu(Bar, S2),
    FilterData = create_filter_menu(Bar, S#state.filters),
    _ = wxFrame:connect(Frame, command_menu_selected, []),
    _ = wxFrame:connect(Frame, key_up),
    _ = wxFrame:connect(Frame, close_window, [{skip,true}]),
    _ = wxFrame:setFocus(Frame),
    _ = wxPanel:setSizer(Panel, Sizer),
    _ = wxSizer:fit(Sizer, Panel),
    _ = wxFrame:show(Frame),
    S2#state{menu_data = HideData++SearchData++FilterData, editor = Editor, frame = Frame}.

menuitem(Menu, Id, Text, UserData) ->
    Item = wxMenu:append(Menu, Id, Text),
    {wxMenuItem:getId(Item), UserData}.

create_file_menu(Bar) ->
    Menu = wxMenu:new([]),
    _ = wxMenu:append(Menu, ?wxID_SAVE, "Save"),
    _ = wxMenu:append(Menu, ?wxID_PRINT,"Print"),
    _ = wxMenu:appendSeparator(Menu),
    _ = wxMenu:append(Menu, ?wxID_EXIT, "Close"),
    _ = wxMenuBar:append(Bar, Menu, "File"),
    ok.

create_filter_menu(Bar, Filters) ->
    Menu  = wxMenu:new([]),
    _ = wxMenuItem:enable(wxMenu:append(Menu, ?wxID_ANY, "Select Filter"), [{enable, false}]),
    _ = wxMenu:appendSeparator(Menu),
    Item = fun(F, {N,Acc}) when F#filter.name =:= ?DEFAULT_FILTER_NAME->
                   Label = lists:concat([pad_string(F#filter.name, 20, $\ , right), "(0)"]),
                   MenuItem = menuitem(Menu, ?wxID_ANY, Label, F),
                   {N + 1, [MenuItem|Acc]};
              (F, {N, Acc}) ->
                   Name = F#filter.name,
                   Label = lists:concat([pad_string(Name, 20, $\ , right), "(", N, ")"]),
                   MenuItem = menuitem(Menu, ?wxID_ANY, Label, F),
                   {N + 1, [MenuItem|Acc]}
           end,
    Filters2 = lists:keysort(#filter.name, Filters),
    {_,MenuData} = lists:foldl(Item, {1, []}, Filters2),
    _ = wxMenuBar:append(Bar, Menu, "Filters"),
    MenuData.

create_hide_menu(Bar, S) ->
    Menu   = wxMenu:new([]),
    E      = S#state.filtered_event,
    From   = E#event.from,
    To     = E#event.to,
    MenuData =
	if
	    S#state.viewer_pid =:= undefined ->
		ignore;
	    From =:= To ->
		_ = wxMenuItem:enable(wxMenu:append(Menu, ?wxID_ANY, "Hide actor in Viewer "),
                                      [{enable, false}]),
		_ = wxMenu:appendSeparator(Menu),
		Hide = menuitem(Menu, ?wxID_ANY, "From=To (f|t|b)", {hide, [From]}),
		_ = wxMenu:appendSeparator(Menu),
		_ = wxMenuItem:enable(wxMenu:append(Menu, ?wxID_ANY, "Show actor in Viewer "),
                                      [{enable, false}]),
		_ = wxMenu:appendSeparator(Menu),
		Show = menuitem(Menu, ?wxID_ANY, "From=To (F|T|B)", {show, [From]}),
		[Show,Hide];
	    true ->
		_ = wxMenuItem:enable(wxMenu:append(Menu, ?wxID_ANY, "Hide actor in Viewer "),
                                      [{enable, false}]),
                _ = wxMenu:appendSeparator(Menu),
		Hide = [menuitem(Menu, ?wxID_ANY, "From (f)", {hide, [From]}),
			menuitem(Menu, ?wxID_ANY, "To   (t)", {hide, [To]}),
			menuitem(Menu, ?wxID_ANY, "Both (b)", {hide, [From, To]})],
		_ = wxMenu:appendSeparator(Menu),
		_ = wxMenuItem:enable(wxMenu:append(Menu, ?wxID_ANY, "Show actor in Viewer "),
                                      [{enable, false}]),
		_ = wxMenu:appendSeparator(Menu),
		Show = [menuitem(Menu, ?wxID_ANY, "From (F)", {show, [From]}),
			menuitem(Menu, ?wxID_ANY, "To   (T)", {show, [To]}),
			menuitem(Menu, ?wxID_ANY, "Both (B)", {show, [From, To]})],
		Show++Hide
	end,
    _ = wxMenuBar:append(Bar, Menu, "Hide"),
    MenuData.

create_search_menu(Bar, S) ->
    Menu   = wxMenu:new([]),
    E      = S#state.filtered_event,
    From   = E#event.from,
    To     = E#event.to,
    _ = wxMenuItem:enable(wxMenu:append(Menu, ?wxID_ANY, "Search in Viewer "),
                          [{enable, false}]),
    _ = wxMenu:appendSeparator(Menu),
    MenuData =
	if
	    S#state.viewer_pid =:= undefined ->
		[menuitem(Menu, ?wxID_ANY, "Abort search. Display all (a)", {mode, all})];
	    From =:= To  ->
		Key = et_collector:make_key(S#state.event_order, E),
		ModeS = {search_actors, forward, Key, [From]},
		ModeR = {search_actors, reverse, Key, [From]},
		[menuitem(Menu, ?wxID_ANY, "Forward from this event   (s)", {mode, ModeS}),
		 menuitem(Menu, ?wxID_ANY, "Reverse from this event   (r)", {mode, ModeR}),
		 menuitem(Menu, ?wxID_ANY, "Abort search. Display all (a)", {mode, all})];
	    true ->
		Key = et_collector:make_key(S#state.event_order, E),
		ModeS = {search_actors, forward, Key, [From, To]},
		ModeR = {search_actors, reverse, Key, [From, To]},
		[menuitem(Menu, ?wxID_ANY, "Forward from this event   (s)", {mode, ModeS}),
		 menuitem(Menu, ?wxID_ANY, "Reverse from this event   (r)", {mode, ModeR}),
		 menuitem(Menu, ?wxID_ANY, "Abort search. Display all (a)", {mode, all})]
	end,
    _ = wxMenuBar:append(Bar, Menu, "Search"),
    MenuData.

config_editor(Editor, S) ->
    Event = S#state.event,
    Name = S#state.active_filter,
    {value, F} = lists:keysearch(Name, #filter.name, S#state.filters),
    FilterFun = F#filter.function,
    case catch FilterFun(Event) of
        true ->
            do_config_editor(Editor, Event, lightblue, S#state.event_order);
        {true, Event2} when is_record(Event2, event) ->
            do_config_editor(Editor, Event2, lightblue, S#state.event_order);
        false ->
            do_config_editor(Editor, Event, red, S#state.event_order);
        Bad ->
            Contents = {bad_filter, Name, Bad},
            BadEvent = Event#event{contents = Contents},
            do_config_editor(Editor, BadEvent, red, S#state.event_order)
    end.

do_config_editor(Editor, Event, _Colour, TsKey) ->
    String = event_to_string(Event, TsKey),
    wxTextCtrl:appendText(Editor, String),
    Event.

%%%----------------------------------------------------------------------
%%% String handling
%%%----------------------------------------------------------------------

term_to_string(Term) ->
    case catch io_lib:format("~s", [Term]) of
        {'EXIT', _} -> io_lib:format("~p", [Term]);
        GoodString  -> GoodString
    end.

now_to_string({Mega, Sec, Micro} = Now)
  when is_integer(Mega), is_integer(Sec), is_integer(Micro) ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_universal_time(Now),
    lists:concat([Y, "-", 
		  pad_string(Mo, 2, $0, left), "-", 
		  pad_string(D, 2, $0, left),
		  "T",
		  pad_string(H, 2, $0, left), ":",
		  pad_string(Mi, 2, $0, left), ":",
		  pad_string(S, 2, $0, left), ".", 
		  Micro]);
now_to_string(Other) ->
    term_to_string(Other).

event_to_string(Event, TsKey) ->
    ReportedTs = Event#event.trace_ts,
    ParsedTs   = Event#event.event_ts,
    Deep = 
        ["DETAIL LEVEL: ", term_to_string(Event#event.detail_level),
         "\nLABEL:        ", term_to_string(Event#event.label),
         case Event#event.from =:= Event#event.to of
             true ->
                 ["\nACTOR:        ", term_to_string(Event#event.from)];
             false ->
                 ["\nFROM:         ", term_to_string(Event#event.from),
                  "\nTO:           ", term_to_string(Event#event.to)]
         end,
         case ReportedTs =:= ParsedTs of
             true ->
                 ["\nPARSED:       ", now_to_string(ParsedTs)];
             false ->
                 case TsKey of
                     trace_ts ->
                         ["\nTRACE_TS:     ", now_to_string(ReportedTs),
                          "\nEVENT_TS:     ", now_to_string(ParsedTs)];
                     event_ts ->
                         ["\nEVENT_TS:     ", now_to_string(ParsedTs),
                          "\nTRACE_TS:     ", now_to_string(ReportedTs)]
                 end
         end,
         "\nCONTENTS:\n\n", term_to_string(Event#event.contents)],
    lists:flatten(Deep).

pad_string(Int, MinLen, Char, Dir) when is_integer(Int) ->
    pad_string(integer_to_list(Int), MinLen, Char, Dir);
pad_string(Atom, MinLen, Char, Dir) when is_atom(Atom) ->
    pad_string(atom_to_list(Atom), MinLen, Char, Dir);
pad_string(String, MinLen, Char, Dir) when is_integer(MinLen), MinLen >= 0 ->
    Len = length(String),
    case {Len >= MinLen, Dir} of
        {true, _} ->
            String;
        {false, right} ->
            String ++ lists:duplicate(MinLen - Len, Char);
        {false, left} ->
	    lists:duplicate(MinLen - Len, Char) ++ String
    end.

send_viewer_event(S, Event)  ->
    case S#state.viewer_pid of
        ViewerPid when is_pid(ViewerPid) ->
            ViewerPid ! {et, Event},
            ok;
        undefined  ->
            ok
    end.

select_file(Frame, Message, DefaultFile, Style) ->
    Dialog = wxFileDialog:new(Frame,
                              [{message, Message},
                               {defaultDir, filename:dirname(DefaultFile)},
                               {defaultFile, filename:basename(DefaultFile)},
                               {style, Style}]),
    Choice = 
        case wxMessageDialog:showModal(Dialog) of
            ?wxID_CANCEL ->  cancel;
            ?wxID_OK -> {ok, wxFileDialog:getPath(Dialog)}
        end,
    wxFileDialog:destroy(Dialog),
    Choice.
