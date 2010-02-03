%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2010. All Rights Reserved.
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
%%
%%----------------------------------------------------------------------
%% Purpose: Displays details of a trace event
%%----------------------------------------------------------------------

-module(et_gs_contents_viewer).

-behaviour(gen_server).

%% External exports
-export([start_link/1, 
         stop/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-include("../include/et.hrl").
-include("et_internal.hrl").

-record(state, {parent_pid,     % Pid of parent process
                viewer_pid,     % Pid of viewer process
                event_order,    % Field to be used as primary key
                event,          % The original event
                filtered_event, % Event processed by active filter
                active_filter,  % Name of the active filter
                filters,        % List of possible filters
                win,            % GUI: Window object
                packer,         % GUI: Packer object
                width,          % GUI: Window width
                height}).       % GUI: Window height

%%%----------------------------------------------------------------------
%%% Client side
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% start_link(Options) -> {ok, ContentsPid} | {error, Reason}
%%
%% Start an viewer for the event contents as window in GS
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
            case gen_server:start_link(?MODULE, [S], []) of
                {ok, ContentsPid} when S#state.parent_pid =/= self() ->
                    unlink(ContentsPid),
                    {ok, ContentsPid};
                Other ->
                    Other
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
           height        = 300}.

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
        {parent_pid, ParentPid} when is_pid(ParentPid) ->
            parse_opt(T, S#state{parent_pid = ParentPid});
        {viewer_pid, ViewerPid} when is_pid(ViewerPid) ->
            parse_opt(T, S#state{viewer_pid = ViewerPid});
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

stop(ContentsPid) ->
    unlink(ContentsPid),
    call(ContentsPid, stop).

call(ContentsPid, Request) ->
    gen_server:call(ContentsPid, Request, infinity).

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
    process_flag(trap_exit, true),
    S2 = create_window(S),
    {ok, S2}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call(stop, _From, S) ->
    unlink(S#state.parent_pid),
    {stop, shutdown, ok, S};
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
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info({gs, Button, click, Data, _Other}, S) ->
    case Button of
        close ->
            gs:destroy(S#state.win),
            {stop, normal, S};
        save ->
            Event = S#state.event,
            Bin = list_to_binary(event_to_string(Event, S#state.event_order)),
            TimeStamp = 
                case S#state.event_order of
                    trace_ts -> Event#event.trace_ts;
                    event_ts   -> Event#event.event_ts
                end,
            FileName = ["et_contents_viewer_", now_to_string(TimeStamp), ".save"],
            file:write_file(lists:flatten(FileName), Bin),
            {noreply, S};
        _PopupMenuItem when is_record(Data, filter) ->
            F = Data,
            ChildState= S#state{active_filter = F#filter.name},
            case gen_server:start_link(?MODULE, [ChildState], []) of
                {ok, Pid} when S#state.parent_pid =/= self() ->
                    unlink(Pid),
                    {noreply, S};
                _ ->
                    {noreply, S}
            end;
        {hide, Actors} ->
            send_viewer_event(S, {delete_actors, Actors}),
            {noreply, S};
        {show, Actors} ->
            send_viewer_event(S, {insert_actors, Actors}),
            {noreply, S};
        {mode, Mode} ->
            send_viewer_event(S, {mode, Mode}),
            {noreply, S};
        Nyi ->
            ok = error_logger:format("~p: click ~p ignored (nyi)~n",
                                     [?MODULE, Nyi]),
            {noreply, S}
    end;
handle_info({gs, _Obj, destroy,_, _}, S) ->
    unlink(S#state.parent_pid),
    gs:destroy(S#state.win),
    {stop, normal, S};
handle_info({gs, _Obj, keypress, _, [KeySym, _Keycode, _Shift, _Control | _]}, S) ->
    case KeySym of
        'c' ->
            gs:destroy(S#state.win),
            {stop, normal, S};

        'f' ->
            E    = S#state.filtered_event,
            From = E#event.from,
            send_viewer_event(S, {delete_actors, [From]}),
            {noreply, S};
        't' ->
            E  = S#state.filtered_event,
            To = E#event.to,
            send_viewer_event(S, {delete_actors, [To]}),
            {noreply, S};
        'b' ->
            E    = S#state.filtered_event,
            From = E#event.from,
            To   = E#event.to,
            send_viewer_event(S, {delete_actors, [From, To]}),
            {noreply, S};
        
        'F' ->
            E    = S#state.filtered_event,
            From = E#event.from,
            send_viewer_event(S, {insert_actors, [From]}),
            {noreply, S};
        'T' ->
            E  = S#state.filtered_event,
            To = E#event.to,
            send_viewer_event(S, {insert_actors, [To]}),
            {noreply, S};
        'B' ->
            E    = S#state.filtered_event,
            From = E#event.from,
            To   = E#event.to,
            send_viewer_event(S, {insert_actors, [From, To]}),
            {noreply, S};

        's' ->
            E     = S#state.filtered_event,
            From  = E#event.from,
            To    = E#event.to,
            First = et_collector:make_key(S#state.event_order, E),
            Mode  = {search_actors, forward, First, [From, To]},
            send_viewer_event(S, {mode, Mode}),
            {noreply, S};
        'r' ->
            E     = S#state.filtered_event,
            From  = E#event.from,
            To    = E#event.to,
            First = et_collector:make_key(S#state.event_order, E),
            Mode  = {search_actors, reverse, First, [From, To]},
            send_viewer_event(S, {mode, Mode}),
            {noreply, S};
        'a' ->
            send_viewer_event(S, {mode, all}),
            {noreply, S};

        0 ->
            case lists:keysearch(?DEFAULT_FILTER_NAME, #filter.name, S#state.filters) of
                {value, F} when is_record(F, filter) ->
                    ChildState= S#state{active_filter = F#filter.name},
                    case gen_server:start_link(?MODULE, [ChildState], []) of
                        {ok, Pid} when S#state.parent_pid =/= self() ->
                            unlink(Pid);
                        _ ->
                            ignore
                    end;
                false ->
                    ignore
            end,
            {noreply, S};
        Int when is_integer(Int), Int > 0, Int =< 9 ->
            case catch lists:nth(Int, S#state.filters) of
                F when is_record(F, filter) ->
                    ChildState= S#state{active_filter = F#filter.name},
                    case gen_server:start_link(?MODULE, [ChildState], []) of
                        {ok, Pid} when S#state.parent_pid =/= self() ->
                            unlink(Pid);
                        _ ->
                            ignore
                    end;
                {'EXIT', _} ->
                    ignore
            end,
            {noreply, S};

        'Shift_L' ->
            {noreply, S};
        'Shift_R' ->
            {noreply, S};
        'Caps_Lock' ->
            {noreply, S};
        _ ->
            io:format("~p: ignored: ~p~n", [?MODULE, KeySym]),
            {noreply, S}
    end;
handle_info({gs, _Obj, configure, [], [W, H | _]}, S) ->
    gs:config(S#state.packer, [{width, W},{height, H}]),
    S2 = S#state{width = W, height = H},
    {noreply, S2};
handle_info({'EXIT', Pid, Reason}, S) ->
    if
        Pid =:= S#state.parent_pid ->
            unlink(Pid),
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

create_window(S) ->
    H = S#state.height,
    W = S#state.width,
    Name = S#state.active_filter,
    Title = lists:concat([?MODULE, " (filter: ", Name, ")"]),
    WinOpt = [{title, Title}, {configure, true},
              {width, W}, {height, H}],
    GS  = gs:start(),
    Win = gs:window(GS, WinOpt),
    Bar = gs:menubar(Win, []),
    create_file_menu(Bar),
    PackerOpt = [{packer_x, [{stretch, 1}]},
                 {packer_y, [{stretch, 1}, {fixed, 25}]},
                 {x, 0}, {y, 25}],
    Packer = gs:frame(Win, PackerOpt),
    EditorOpt = [{pack_xy, {1, 1}}, {vscroll, right}, {hscroll, bottom},
                 {wrap, none},
                 {bg, lightblue},  {font, {courier, 12}}],
    Editor = gs:editor(Packer, EditorOpt),
    FilteredEvent = config_editor(Editor, S),
    S2 = S#state{win = Win, packer = Packer, filtered_event = FilteredEvent},
    create_hide_menu(Bar, S2),
    create_search_menu(Bar, S2),
    create_filter_menu(Bar, S#state.filters),
    gs:config(Packer, [{width, W}, {height, H}]),
    gs:config(Win, [{map,true}, {keypress, true}]),
    S2.

create_file_menu(Bar) ->
    Button = gs:menubutton(Bar,  [{label, {text, "File"}}]),
    Menu  = gs:menu(Button, []),
    gs:menuitem(close, Menu,     [{label, {text,"Close (c)"}}]),
    gs:menuitem(save, Menu,      [{label, {text,"Save"}}]).

create_filter_menu(Bar, Filters) ->
    Button = gs:menubutton(Bar, [{label, {text, "Filters"}}]),
    Menu  = gs:menu(Button, []),
    gs:menuitem(Menu, [{label, {text, "Select Filter"}}, {bg, lightblue}, {enable, false}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    Item = fun(F, N) when F#filter.name =:= ?DEFAULT_FILTER_NAME->
                   Label = lists:concat([pad_string(F#filter.name, 20), "(0)"]),
                   gs:menuitem(Menu, [{label, {text, Label}}, {data, F}]),
                   N + 1;
              (F, N) ->
                   Name = F#filter.name,
                   Label = lists:concat([pad_string(Name, 20), "(", N, ")"]),
                   gs:menuitem(Menu, [{label, {text, Label}}, {data, F}]),
                   N + 1
           end,
    Filters2 = lists:keysort(#filter.name, Filters),
    lists:foldl(Item, 1, Filters2),
    Menu.

create_hide_menu(Bar, S) ->
    Button = gs:menubutton(Bar,  [{label, {text, "Hide"}}]),
    Menu   = gs:menu(Button, []),
    E      = S#state.filtered_event,
    From   = E#event.from,
    To     = E#event.to,
    if
        S#state.viewer_pid =:= undefined ->
            ignore;
        From =:= To ->
            gs:menuitem(Menu, [{label, {text, "Hide actor in Viewer "}}, {bg, lightblue}, {enable, false}]),
            gs:menuitem(Menu, [{itemtype, separator}]),
            gs:menuitem({hide, [From]},     Menu, [{label, {text,"From=To (f|t|b)"}}]),
            gs:menuitem(Menu, [{itemtype, separator}]),
            gs:menuitem(Menu, [{label, {text, "Show actor in Viewer "}}, {bg, lightblue}, {enable, false}]),
            gs:menuitem(Menu, [{itemtype, separator}]),
            gs:menuitem({show, [From]},     Menu, [{label, {text,"From=To (F|T|B)"}}]);
        true ->
            gs:menuitem(Menu, [{label, {text, "Hide actor in Viewer "}}, {bg, lightblue}, {enable, false}]),
            gs:menuitem(Menu, [{itemtype, separator}]),
            gs:menuitem({hide, [From]},     Menu, [{label, {text,"From (f)"}}]),
            gs:menuitem({hide, [To]},       Menu, [{label, {text,"To   (t)"}}]),
            gs:menuitem({hide, [From, To]}, Menu, [{label, {text,"Both (b)"}}]),
            gs:menuitem(Menu, [{itemtype, separator}]),
            gs:menuitem(Menu, [{label, {text, "Show actor in Viewer "}}, {bg, lightblue}, {enable, false}]),
            gs:menuitem(Menu, [{itemtype, separator}]),
            gs:menuitem({show, [From]},     Menu, [{label, {text,"From (F)"}}]),
            gs:menuitem({show, [To]},       Menu, [{label, {text,"To   (T)"}}]),
            gs:menuitem({show, [From, To]}, Menu, [{label, {text,"Both (B)"}}])
    end.

create_search_menu(Bar, S) ->
    Button = gs:menubutton(Bar,  [{label, {text, "Search"}}]),
    Menu   = gs:menu(Button, []),
    E      = S#state.filtered_event,
    From   = E#event.from,
    To     = E#event.to,
    gs:menuitem(Menu, [{label, {text, "Search in Viewer "}},
                       {bg, lightblue}, {enable, false}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    if
        S#state.viewer_pid =:= undefined ->
            S;
        From =:= To  ->
            Key = et_collector:make_key(S#state.event_order, E),
            ModeS = {search_actors, forward, Key, [From]},
            ModeR = {search_actors, reverse, Key, [From]},
            gs:menuitem({mode, ModeS}, Menu, [{label, {text,"Forward from this event   (s)"}}]),
            gs:menuitem({mode, ModeR}, Menu, [{label, {text,"Reverse from this event   (r)"}}]);
        true ->
            Key = et_collector:make_key(S#state.event_order, E),
            ModeS = {search_actors, forward, Key, [From, To]},
            ModeR = {search_actors, reverse, Key, [From, To]},
            gs:menuitem({mode, ModeS}, Menu, [{label, {text,"Forward from this event   (s)"}}]),
            gs:menuitem({mode, ModeR}, Menu, [{label, {text,"Reverse from this event   (r)"}}])
    end,
    gs:menuitem({mode, all}, Menu,   [{label, {text,"Abort search. Display all (a)"}}]).

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

do_config_editor(Editor, Event, Colour, TsKey) ->
    String = event_to_string(Event, TsKey),
    gs:config(Editor, {insert, {'end', String}}),
    gs:config(Editor, {enable, false}),
    gs:config(Editor, {bg, Colour}),
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
    lists:concat([Y, "-", Mo, "-", D, " ", H, ".", Mi, ".", S, ".", Micro]);
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

pad_string(Atom, MinLen) when is_atom(Atom) ->
    pad_string(atom_to_list(Atom), MinLen);
pad_string(String, MinLen) when is_integer(MinLen), MinLen >= 0 ->
    Len = length(String),
    case Len >= MinLen of
        true ->
            String;
        false ->
            String ++ lists:duplicate(MinLen - Len, $ )
    end.

send_viewer_event(S, Event)  ->
    case S#state.viewer_pid of
        ViewerPid when is_pid(ViewerPid) ->
            ViewerPid ! {et, Event};
        undefined  ->
            ignore
    end.
