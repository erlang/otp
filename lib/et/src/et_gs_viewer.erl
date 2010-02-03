%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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
%% Purpose: Displays a sequence chart for trace events (messages/actions)
%%----------------------------------------------------------------------

-module(et_gs_viewer).

-behaviour(gen_server).

%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-include("../include/et.hrl").
-include("et_internal.hrl").

-define(unknown, "UNKNOWN").

-record(state,
        {parent_pid,       % Pid of parent process
         collector_pid,    % Pid of collector process
         event_order,      % Field to be used as primary key
         trace_pattern,    % Collector trace pattern
         active_filter,    % Name of the active filter
         filters,          % List of possible filters
         selected_actor,   % Actor selected by user
         first_event,      % Key of first event (regardless of visibility)
         last_event,       % Key of last event (regardless of visibility)
         max_events,       % Maximum number of shown events
         events,           % Queue containg all event keys (regardless of visibility)
         max_actors,       % Maximum number of shown actors
         actors,           % List of known actors
         refresh_needed,   % Refresh is needed in order to show all actors
         display_mode,     % Display all or only matching actors
         detail_level,     % Show only events with lesser detail level
         hide_actions,     % Hide/show events where to == from actor (bool)
         hide_unknown,     % Hide/show events with unknown actor (bool)
         is_suspended,     % Suspend viewer updates (bool)
         title,            % GUI: Window title
         win,              % GUI: Window object
         menubar,          % GUI: Menu bar object
         packer,           % GUI: Packer object
         width,            % GUI: Window width
         height,           % GUI: Window height
         scale,            % GUI: Scaling factor on canvas
         font,             % GUI: Font to be used on text labels
         canvas_width,     % GUI: Canvas width
         canvas_height,    % GUI: Canvas height
         canvas,           % GUI: Canvas object
         y_pos}).          % GUI: Current y position on canvas

-record(actor, {name, string}).

-define(initial_x, 10).
-define(incr_x,    60).
-define(initial_y, 15).
-define(incr_y,    15).

%%%----------------------------------------------------------------------
%%% Client side
%%%----------------------------------------------------------------------

start_link(Options) -> 
    case parse_opt(Options, default_state(), []) of
        {ok, S, CollectorOpt} ->
            case S#state.collector_pid of
                CollectorPid when is_pid(CollectorPid) ->
                    case gen_server:start_link(?MODULE, [S], []) of
                        {ok, Pid} when S#state.parent_pid =/= self() ->
                            unlink(Pid),
                            {ok, Pid};
                        Other ->
                            Other
                    end;
                undefined ->
                    case et_collector:start_link(CollectorOpt) of
                        {ok, CollectorPid} ->
                            S2 = S#state{collector_pid = CollectorPid},
                            case gen_server:start_link(?MODULE, [S2], []) of
                                {ok, Pid} when S#state.parent_pid =/= self() ->
                                    unlink(Pid),
                                    {ok, Pid};
                                Other ->
                                    Other
                            end;
                        {error, Reason} ->
                            {error, {et_collector, Reason}}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

default_state() ->
    #state{parent_pid     = self(),
           collector_pid  = undefined,
           detail_level   = ?detail_level_max,
           active_filter  = ?DEFAULT_FILTER_NAME,
           filters        = [?DEFAULT_FILTER],
           event_order    = trace_ts,
           is_suspended   = false,
           max_events     = 100,
           first_event    = first,
           last_event     = first,
           events         = queue_new(),
           max_actors     = 5,
           actors         = [create_actor(?unknown)],
           selected_actor = ?unknown,
           hide_actions   = false,
           hide_unknown   = false,
           refresh_needed = false,
           display_mode   = all,
           scale          = 2,
           canvas_height  = 0,
           canvas_width   = 0,
           width          = 800,
           height         = 600}.

parse_opt([], S, CollectorOpt) ->
    {ok, S, [{parent_pid, S#state.parent_pid} | CollectorOpt]};
parse_opt([H | T], S, CollectorOpt) ->
    case H of
        {parent_pid, Parent} when Parent =:= undefined ->
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S#state{parent_pid = Parent}, CollectorOpt2);
        {parent_pid, Parent} when is_pid(Parent) ->
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S#state{parent_pid = Parent}, CollectorOpt2);
        {title, Title} ->
            parse_opt(T, S#state{title = name_to_string(Title)}, CollectorOpt);
        {detail_level, Level} when is_integer(Level),
                                   Level >= ?detail_level_min,
                                   Level =< ?detail_level_max -> 
            parse_opt(T, S#state{detail_level = Level}, CollectorOpt);
        {detail_level, max} ->
            parse_opt(T, S#state{detail_level = ?detail_level_max}, CollectorOpt);
        {detail_level, min} ->
            parse_opt(T, S#state{detail_level = ?detail_level_min}, CollectorOpt);
        {is_suspended, true} ->
            parse_opt(T, S#state{is_suspended = true}, CollectorOpt);
        {is_suspended, false} ->
            parse_opt(T, S#state{is_suspended = false}, CollectorOpt);
        {scale, Scale} when is_integer(Scale), Scale > 0 ->
            parse_opt(T, S#state{scale = Scale}, CollectorOpt);
        {width, W} when is_integer(W), W > 0 ->
            parse_opt(T, S#state{width = W, canvas_width = W}, CollectorOpt);
        {height, WH} when is_integer(WH), WH > 0 ->
            parse_opt(T, S#state{height = WH, canvas_height = WH}, CollectorOpt);
        {collector_pid, Pid} when is_pid(Pid) -> 
            parse_opt(T, S#state{collector_pid = Pid}, CollectorOpt);
        {collector_pid, undefined} -> 
            parse_opt(T, S#state{collector_pid = undefined}, CollectorOpt);
        {active_filter, Name} when is_atom(Name) ->
            parse_opt(T, S#state{active_filter = Name}, CollectorOpt);
        {event_order, trace_ts} -> %% BUGBUG: Verify event_order with collector
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S#state{event_order = trace_ts}, CollectorOpt2);
        {event_order, event_ts} -> %% BUGBUG: Verify event_order with collector
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S#state{event_order = event_ts}, CollectorOpt2);
        {trace_port, _Port} -> 
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S, CollectorOpt2);
        {trace_max_queue, _Queue} -> 
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S, CollectorOpt2);
        {trace_pattern, _Pattern} -> 
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S, CollectorOpt2);
        {trace_global, _Boolean} -> 
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S, CollectorOpt2);
        {trace_client, _Client} -> 
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S, CollectorOpt2);
        {dict_insert, {filter, Name}, Fun} ->
	    if
		is_atom(Name), is_function(Fun) ->
		    F = #filter{name = Name, function = Fun},
		    Filters = lists:keydelete(Name, #filter.name, S#state.filters),
		    CollectorOpt2 = [H | CollectorOpt],
		    parse_opt(T, S#state{filters = Filters ++ [F]}, CollectorOpt2);
		true ->
	            {error, {bad_option, H}}
	    end;
        {dict_insert, {subscriber, Pid}, _Val} ->
	    if
		is_pid(Pid) ->
		    CollectorOpt2 = [H | CollectorOpt],
		    parse_opt(T, S, CollectorOpt2);
		true ->
	            {error, {bad_option, H}}
	    end;
        {dict_insert, _Key, _Val} ->
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S, CollectorOpt2);
        {dict_delete, {filter, Name}} ->
            Filters = lists:keydelete(Name, #filter.name, S#state.filters),
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S#state{filters = Filters}, CollectorOpt2);
        {dict_delete, _Key} ->
            CollectorOpt2 = [H | CollectorOpt],
            parse_opt(T, S, CollectorOpt2);
        {max_events, Max} when is_integer(Max), Max > 0->
            parse_opt(T,  S#state{max_events = Max}, CollectorOpt);
        {max_events, Max} when Max =:= infinity ->
            parse_opt(T,  S#state{max_events = Max}, CollectorOpt);
        {max_actors, Max} when is_integer(Max), Max >= 0->
            parse_opt(T,  S#state{max_actors = Max}, CollectorOpt);
        {max_actors, Max} when Max =:= infinity ->
            parse_opt(T,  S#state{max_actors = Max}, CollectorOpt);
        {actors, ActorNames} when is_list(ActorNames) ->
            ActorNames2 = 
                case lists:member(?unknown, ActorNames) of
                    false -> [?unknown | ActorNames];
                    true  -> ActorNames
                end,
            Actors = [create_actor(Name) || Name <- ActorNames2],
            parse_opt(T, S#state{actors = Actors}, CollectorOpt);
        {first_event, First} ->
            parse_opt(T,  S#state{first_event = First}, CollectorOpt);
        {hide_unknown, Bool} when Bool =:= false ->
            parse_opt(T,  S#state{hide_unknown = Bool}, CollectorOpt);
        {hide_unknown, Bool} when Bool =:= true ->
            parse_opt(T,  S#state{hide_unknown = Bool}, CollectorOpt);
        {hide_actions, Bool} when Bool =:= false ->
            parse_opt(T,  S#state{hide_actions = Bool}, CollectorOpt);
        {hide_actions, Bool} when Bool =:= true ->
            parse_opt(T,  S#state{hide_actions = Bool}, CollectorOpt);
        {display_mode, Mode = all} ->
            parse_opt(T,  S#state{display_mode = Mode}, CollectorOpt);
        {display_mode, Mode = {search_actors, Dir, _Key, Actors}} when is_list(Actors), Dir =:= forward ->
            parse_opt(T,  S#state{display_mode = Mode}, CollectorOpt);
        {display_mode, Mode = {search_actors, Dir, _Key, Actors}} when is_list(Actors), Dir =:= reverse ->
            parse_opt(T,  S#state{display_mode = Mode}, CollectorOpt);

        Bad ->
            {error, {bad_option, Bad}}
    end;
parse_opt(BadList, _S, _CollectorOpt) ->
    {error, {bad_option_list, BadList}}.

do_dict_insert({filter, Name}, Fun, S) when is_atom(Name), is_function(Fun) ->
    F = #filter{name = Name, function = Fun},
    Filters = lists:keydelete(Name, #filter.name, S#state.filters),
    Filters2 = lists:keysort(#filter.name, [F | Filters]),
    gs:destroy(filter_menu),
    create_filter_menu(S#state.active_filter, Filters2),    
    S#state{filters = Filters2};
do_dict_insert(_Key, _Val, S) ->
    %% ok = error_logger:format("~p(~p): handle_info({et, {dict_insert, ~p, ~p}})~n",
    %%                          [?MODULE, self(), Key, Val]),
    S.

do_dict_delete({filter, Name}, S) when is_atom(Name), Name =/= S#state.active_filter ->
    Filters = lists:keydelete(Name, #filter.name, S#state.filters),
    gs:destroy(filter_menu),
    create_filter_menu(S#state.active_filter, Filters),    
    S#state{filters = Filters};
do_dict_delete(_Key, S) ->
    %% ok = error_logger:format("~p(~p): handle_info({et, {dict_delete, ~p}})~n",
    %%                          [?MODULE, self(), Key]),
    S.

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
    InitialTimeout = 0,
    case S#state.parent_pid of
	undefined ->
	    ignore;
	Pid when is_pid(Pid) ->
	    link(Pid)
    end,
    et_collector:dict_insert(S#state.collector_pid,
                             {subscriber, self()},
                             ?MODULE),
    {ok, create_main_window(S), InitialTimeout}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call(get_collector_pid, _From, S) ->
    Reply = S#state.collector_pid,
    reply(Reply, S);
handle_call(stop, _From, S) ->
    gs:destroy(S#state.win),
    {stop, shutdown, ok, S};
handle_call(Request, From, S) ->
    ok = error_logger:format("~p(~p): handle_call(~p, ~p, ~p)~n",
                             [?MODULE, self(), Request, From, S]),
    Reply = {error, {bad_request, Request}},
    reply(Reply, S).

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_cast(Msg, S) ->
    ok = error_logger:format("~p(~p): handle_cast(~p, ~p)~n",
                             [?MODULE, self(), Msg, S]),
    noreply(S).

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info({et, {more_events, _Size}}, S) ->
    noreply(S);
handle_info({et, {insert_actors, ActorNames}}, S) when is_list(ActorNames) ->
    Fun = fun(N, Actors) ->
                  case lists:keymember(N, #actor.name, Actors) of
                      true  -> Actors;
                      false -> Actors ++ [create_actor(N)]
                  end
          end,
    Actors = lists:foldl(Fun, S#state.actors, ActorNames),
    S2 = refresh_main_window(S#state{actors = Actors}),
    noreply(S2);
handle_info({et, {delete_actors, ActorNames}}, S) when is_list(ActorNames)->
    Fun = fun(N, Actors) when N =:= ?unknown ->
                  Actors;
             (N, Actors) ->
                  lists:keydelete(N, #actor.name, Actors)
          end,
    New = lists:foldl(Fun, S#state.actors, ActorNames),
    S2 = refresh_main_window(S#state{actors = New}),
    noreply(S2);
handle_info({et, {dict_insert, Key, Val}}, S) ->
    S2 = do_dict_insert(Key, Val, S),
    noreply(S2);
handle_info({et, {dict_delete, Key}}, S) ->
    S2 = do_dict_delete(Key, S),
    noreply(S2);
handle_info({et, first}, S) ->
    S2 = scroll_first(S),
    noreply(S2);
handle_info({et, prev}, S) ->
    S2 = scroll_prev(S),
    noreply(S2);
handle_info({et, next}, S) ->
    S2 = scroll_next(S),
    noreply(S2);
handle_info({et, last}, S) ->
    S2 = scroll_last(S),
    noreply(S2);
handle_info({et, refresh}, S) ->
    S2 = refresh_main_window(S),
    noreply(S2);
handle_info({et, {display_mode, Mode}}, S) ->
    S2 = change_display_mode(Mode, S),
    noreply(S2);
handle_info({et, close}, S) ->
    gs:destroy(S#state.win),
    {stop, shutdown, S};
handle_info({gs, Button, click, Data, Other} = Click, S) ->
    CollectorPid = S#state.collector_pid,
    case Button of
        close ->
            gs:destroy(S#state.win),
            {stop, shutdown, S};
        suspended ->
            case Other of
                [_Text, _Group, Bool | _] when Bool =:= true ->
                    S2 = do_suspend(S),
                    noreply(S2);    
                [_Text, _Group, Bool | _] when Bool =:= false ->
                    S2 = do_resume(S),
                    noreply(S2);
                _ ->
                    click_error(Click, S),
                    noreply(S)
            end;
        hide_actions ->
            case Other of
                [_Text, _Group, Bool | _] when Bool =:= true ->
                    S2 = refresh_main_window(S#state{hide_actions = Bool}),
                    noreply(S2);    
                [_Text, _Group, Bool | _] when Bool =:= false ->
                    S2 = refresh_main_window(S#state{hide_actions = Bool}),
                    noreply(S2);
                _ -> 
                    click_error(Click, S),
                    noreply(S)
            end;
        hide_unknown ->
            case Other of
                [_Text, _Group, Bool | _] when Bool =:= true ->
                    S2 = refresh_main_window(S#state{hide_unknown = Bool}),
                    noreply(S2);    
                [_Text, _Group, Bool | _] when Bool =:= false ->
                    S2 = refresh_main_window(S#state{hide_unknown = Bool}),
                    noreply(S2);
                _ -> 
                    click_error(Click, S),
                    noreply(S)
            end;
        up ->
            S2 = scroll_up(S),
            noreply(S2);
        down ->
            S2 = scroll_down(S),
            noreply(S2);
        first ->
            S2 = scroll_first(S),
            noreply(S2);
        prev ->
            S2 = scroll_prev(S),
            noreply(S2);
        next ->
            S2 = scroll_next(S),
            noreply(S2);
        last ->
            S2 = scroll_last(S),
            noreply(S2);
        refresh ->
            S2 = refresh_main_window(S),
            noreply(S2);
        {display_mode, Mode} ->
            S2 = change_display_mode(Mode, S),
            noreply(S2);
        close_all ->
            close_all(S);
        close_all_others ->
            close_all_others(S);
        first_all ->
            et_collector:multicast(CollectorPid, first),
            noreply(S);
        prev_all ->
            et_collector:multicast(CollectorPid, prev),
            noreply(S);
        next_all ->
            et_collector:multicast(CollectorPid, next),
            noreply(S);
        last_all ->
            et_collector:multicast(CollectorPid, last),
            noreply(S);
        refresh_all ->
            et_collector:multicast(CollectorPid, refresh),
            noreply(S);
        clear_all ->
            et_collector:clear_table(CollectorPid),
            et_collector:multicast(CollectorPid, refresh),
            noreply(S);
        load_all ->
            et_collector:start_trace_client(CollectorPid, event_file, "et_viewer.log"),
            noreply(S);
        save_all ->
            et_collector:save_event_file(CollectorPid,
                                         "et_viewer.log",
                                         [existing, write, keep]),
            noreply(S);
        {open_viewer, Scale} ->
            Actors = [A#actor.name || A <- S#state.actors],
            open_viewer(Scale, S#state.active_filter, Actors, S),
            noreply(S);
        _Level when Data =:= detail_level, is_integer(hd(Other)),
                    hd(Other) >= ?detail_level_min,
                    hd(Other) =< ?detail_level_max ->
            S2 = S#state{detail_level = hd(Other)},
            noreply(S2);
        _PopupMenuItem when is_record(Data, filter) ->
            open_viewer(S#state.scale, Data#filter.name, [?unknown], S),
            noreply(S);
        _ ->
            click_error(Click, S),
            noreply(S)
    end;
handle_info({gs, _Obj, destroy,_, _}, S) ->
    gs:destroy(S#state.win),
    {stop, shutdown, S};
handle_info({gs, _Obj, buttonpress, _, [_Button, X, Y | _]}, S) ->
    S3 =
        case y_to_n(Y, S) of
            actor ->
                %% Actor click
                case S#state.actors of
                    [] ->
                        S;
                    _ ->
                        N = x_to_n(X, S),
                        A = lists:nth(N, S#state.actors),
                        S#state{selected_actor = A}
                end;
            {event, N} ->
                %% Event click
                List = queue_to_list(S#state.events),
                S2 = S#state{events = list_to_queue(List)},
                
                Key = lists:nth(N, List),
                Pid = S#state.collector_pid,
                Fun = fun create_contents_window/2,
		case et_collector:iterate(Pid, Key, -1) of
		    Prev when Prev =:= Key ->
			et_collector:iterate(Pid, first, 1, Fun, S2);
		    Prev ->
			et_collector:iterate(Pid, Prev, 1, Fun, S2)
		end
        end,
    noreply(S3);
handle_info({gs, _Obj, buttonrelease, _, [_Button, X, Y | _]}, S) ->
    S2 =
        case y_to_n(Y, S) of
            actor ->
                %% Actor click
                case S#state.actors of
                    [] ->
                        S;
                    Actors ->
                        N = x_to_n(X, S),
                        New = lists:nth(N, S#state.actors),
                        Old = S#state.selected_actor,
                        case New#actor.name =:= Old#actor.name of
                            true ->
                                A = S#state.selected_actor,
                                toggle_search_for_actor(A#actor.name, S);
                            false ->
                                move_actor(Old, New, Actors, S)
                        end
                end;
            {event, _N} ->
                %% Event click ignored
                S
        end,
    noreply(S2);
handle_info({gs, _Obj, keypress, _, [KeySym, _Keycode, _Shift, _Control | _]} = Key, S) ->
    case KeySym of
        'c' ->
            close_all_others(S);
        'C' ->
            close_all(S);
        'Up' ->
            S2 = scroll_up(S),
            noreply(S2);
        'Down' ->
            S2 = scroll_down(S),
            noreply(S2);
        'f' ->
            S2 = scroll_first(S),
            noreply(S2);
        'p' ->
            S2 = scroll_prev(S),
            noreply(S2);
        'Prior' ->
            S2 = scroll_prev(S),
            noreply(S2);
        'n' ->
            S2 = scroll_next(S),
            noreply(S2);
        'Next' ->
            S2 = scroll_next(S),
            noreply(S2);
        'l' ->
            S2 = scroll_last(S),
            noreply(S2);
        'r' ->
            S2 = refresh_main_window(S),
            noreply(S2);
        'F' ->
            et_collector:multicast(S#state.collector_pid, first),
            noreply(S);
        'P' ->
            et_collector:multicast(S#state.collector_pid, prev),
            noreply(S);
        'N' ->
            et_collector:multicast(S#state.collector_pid, next),
            noreply(S);
        'L' ->
            et_collector:multicast(S#state.collector_pid, last),
            noreply(S);
        'R' ->
            et_collector:multicast(S#state.collector_pid, refresh),
            noreply(S);

        'a' ->
            S2 = S#state{display_mode = all},
            S3 = refresh_main_window(S2),
            noreply(S3);

        'equal' ->
            Scale = S#state.scale,
            Actors = [A#actor.name || A <- S#state.actors],
            open_viewer(Scale, S#state.active_filter, Actors, S),
            noreply(S);
        'plus' ->
            Scale = S#state.scale + 1,
            Actors = [A#actor.name || A <- S#state.actors],
            open_viewer(Scale, S#state.active_filter, Actors, S),
            noreply(S);
        'minus' ->
            case S#state.scale of
                1 ->
                    gs:config(S#state.canvas, beep);
                Scale ->
                    Actors = [A#actor.name || A <- S#state.actors],
                    open_viewer(Scale - 1, S#state.active_filter, Actors, S)
            end,
            noreply(S);
        0 ->
            case lists:keysearch(?DEFAULT_FILTER_NAME, #filter.name, S#state.filters) of
                {value, F} when is_record(F, filter) ->
                    open_viewer(S#state.scale, F#filter.name, [?unknown], S);
                false ->
                    gs:config(S#state.canvas, beep)
            end,
            noreply(S);
        Int when is_integer(Int), Int > 0, Int =< 9 ->
            case catch lists:nth(Int, S#state.filters) of
                F when is_record(F, filter) ->
                    open_viewer(S#state.scale, F#filter.name, [?unknown], S);
                {'EXIT', _} ->
                    gs:config(S#state.canvas, beep)
            end,
            noreply(S);

        'Shift_L' ->
            noreply(S);
        'Shift_R' ->
            noreply(S);
        'Caps_Lock' ->
            noreply(S);
            
        _ ->
            click_error(Key, S),
            noreply(S)
    end;
handle_info({gs, _Obj,configure, [], [W, H | _]}, S) ->
    gs:config(S#state.packer, [{width, W}, {height, H}]),
    S2 = S#state{width = W, height = H},
    noreply(S2);
handle_info(timeout, S) ->
    Try =
        case S#state.display_mode of
            {search_actors, reverse, _, _} ->
                -10;
            _ ->
                10
        end,
    if
        S#state.is_suspended =:= true ->
            {noreply, S, infinity};
        S#state.max_events =:= infinity ->
            display_more_events(Try, S);
        true ->
            Needed = S#state.max_events - queue_length(S#state.events),
            if
                Needed =< 0  -> {noreply, S, infinity};
                Needed > 10  -> display_more_events(Try, S);
                Needed =< 10 -> display_more_events(Needed, S)
            end
    end;

handle_info({'EXIT', Pid, Reason}, S) ->
    if
        Pid =:= S#state.collector_pid ->
            unlink(Pid),
            gs:destroy(S#state.win),
            {stop, Reason, S};
        Pid =:= S#state.parent_pid ->
            unlink(Pid),
            gs:destroy(S#state.win),
            {stop, Reason, S};
        true ->
            noreply(S)
    end;
handle_info(Info, S) ->
    ok = error_logger:format("~p(~p): handle_info(~p, ~p)~n",
                             [?MODULE, self(), Info, S]),
    noreply(S).

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
%%% Handle suspend/resume
%%%----------------------------------------------------------------------

reply(Reply, S) ->
    case queue_length(S#state.events) of
        _ when S#state.is_suspended =:= true ->
            {reply, Reply, S, infinity};
        _ when S#state.max_events =:= infinity ->
            {reply, Reply, S, 500};
        N when N >= S#state.max_events ->
            {reply, Reply, S, infinity};
        _ ->
            {reply, Reply, S, 0}
    end.

noreply(S) ->
    case queue_length(S#state.events) of
        _ when S#state.is_suspended =:= true ->
            {noreply, S, infinity};
        _ when S#state.max_events =:= infinity ->
            {noreply, S, 500};
        N when N >= S#state.max_events ->
            {noreply, S, infinity};
        _ ->
            {noreply, S, 0}
    end.

do_suspend(S) ->
    config_suspend(S#state{is_suspended = true}).

do_resume(S) ->
    config_suspend(S#state{is_suspended = false}).

config_suspend(S) ->
    Suspended = S#state.is_suspended,
    gs:config(refresh,     [{enable, not Suspended}]),
    gs:config(refresh_all, [{enable, not Suspended}]),
    gs:config(clear_all,   [{enable, not Suspended}]),
    S.

refresh_main_window(S) ->
    Pid = S#state.collector_pid,
    Key = S#state.first_event,
    case et_collector:iterate(Pid, Key, -1) of
	Prev when Prev =:= Key ->
	    scroll_first(S);
	_Prev ->
	    S2 = S#state{last_event = S#state.first_event},
	    clear_canvas(S2)
    end.    

scroll_first(S) ->
    S2 = S#state{first_event = first, last_event  = first},
    clear_canvas(S2).

scroll_prev(S) ->
    Try =
        case S#state.max_events of
            infinity -> -10;
            Max      -> -Max
        end,
    Key = et_collector:iterate(S#state.collector_pid, S#state.first_event, Try),
    S2 = S#state{first_event = Key, last_event  = Key},
    clear_canvas(S2).

scroll_next(S) ->
    S2 = S#state{first_event = S#state.last_event},
    clear_canvas(S2).

scroll_up(S) ->
    Key = et_collector:iterate(S#state.collector_pid, S#state.first_event, -5),
    S2 = S#state{first_event = Key, last_event  = Key},
    clear_canvas(S2).

scroll_down(S) ->
    Key = et_collector:iterate(S#state.collector_pid, S#state.first_event, 5),
    S2 = S#state{first_event = Key, last_event  = Key},
    clear_canvas(S2).

scroll_last(S) ->
    S2 = S#state{first_event = last, last_event  = last},
    clear_canvas(S2).

change_display_mode(Mode, S) ->
    case Mode of
        all ->
            S2 = S#state{display_mode = Mode},
            refresh_main_window(S2);
        {search_actors, _Dir, _Key, []}  ->
            S2 = S#state{display_mode = all},
            refresh_main_window(S2);
        {search_actors, _Dir, Key, Actors} when is_list(Actors) ->
            Pid = S#state.collector_pid,
            Prev = et_collector:iterate(Pid, Key, -1),
            S2 = S#state{first_event  = Prev,
                         last_event   = Prev,
                         display_mode = Mode},
            clear_canvas(S2)
    end.

close_all(S) ->
    et_collector:multicast(S#state.collector_pid, close),
    timer:sleep(timer:seconds(1)),
    spawn(et_collector, stop, [S#state.collector_pid]),
    gs:destroy(S#state.win),
    {stop, shutdown, S}.

close_all_others(S) ->
    Fun =
        fun({{subscriber, Pid}, _}) ->
                if
                    Pid =:= self() ->
                        ignore;
                    true ->
                        unlink(Pid),
                        Pid ! {et, close}
                end
        end,
    All = et_collector:dict_match(S#state.collector_pid,
                                  {{subscriber, '_'}, '_'}),
    lists:foreach(Fun, All),
    noreply(S).

click_error(Click, S) ->
    gs:config(S#state.canvas, beep),
    io:format("~p: ignored: ~p~n", [?MODULE, Click]).

%%%----------------------------------------------------------------------
%%% Clone viewer
%%%----------------------------------------------------------------------

open_viewer(Scale, FilterName, Actors, S) ->
    Filters = [{dict_insert, {filter, F#filter.name}, F#filter.function}
               || F <- S#state.filters],
    Options = 
        [{parent_pid,    S#state.parent_pid},
         {title,         S#state.title},
         {collector_pid, S#state.collector_pid},
         {is_suspended,  S#state.is_suspended},
         {detail_level,  S#state.detail_level},
         {active_filter, FilterName},
         {event_order,   S#state.event_order},
         {first_event,   S#state.first_event},
         {max_events,    S#state.max_events},
         {max_actors,    S#state.max_actors},
         {hide_actions,  S#state.hide_actions},
         {hide_unknown,  S#state.hide_unknown},
         {is_suspended,  S#state.is_suspended},
         {actors,        Actors},
         {scale,         Scale},
         {width,         S#state.width},
         {height,        S#state.height} | Filters],
    case start_link(Options) of
        {ok, ViewerPid} ->
            unlink(ViewerPid),
            ok;
        {error, Reason} ->
            ok = error_logger:format("~p: Failed to start a new window: ~p~n",
                                     [?MODULE, Reason])
    end.

%%%----------------------------------------------------------------------
%%% Handle graphics
%%%----------------------------------------------------------------------

create_main_window(S) ->
    Font    = select_font(S#state.scale),
    GS      = gs:start(),
    Name    = name_to_string(S#state.active_filter),
    Title   = case S#state.title of
                  undefined -> atom_to_list(?MODULE);
                  Explicit  -> name_to_string(Explicit)
              end,
    WinOpt  = [{title, Title ++ " (filter: " ++ Name ++  ")"},
               {configure, true},
               {width, S#state.width},
               {height, S#state.height}],
    Win     = gs:window(GS, WinOpt),
    Bar     = gs:menubar(Win, []),

    create_file_menu(Bar),
    create_viewer_menu(Bar),
    create_collector_menu(Bar),
    gs:menubutton(filter_button, Bar, [{label, {text, "Filter"}}]),
    create_filter_menu(S#state.active_filter, S#state.filters),
    create_help_menu(Bar),
   
    config_suspend(S),

    PackerOpt = [{packer_x, [{fixed, 5}, {fixed, 40}, {fixed, 40},
                             {stretch, 1}, {fixed, 5}]},
                 {packer_y, [{fixed, 30}, {fixed, 30},
                             {stretch, 1}, {fixed, 30}]},
                 {x, 0}, {y, 30}],
    Packer = gs:frame(Win, PackerOpt),
    gs:checkbutton(suspended, Packer, [{label,{text,"Freeze"}},
                                       {x, 10}, {y, 0},
                                       {width, 120}, {align, w},
                                       {select, S#state.is_suspended}]),
    gs:checkbutton(hide_actions, Packer, [{label,{text,"Hide From=To"}},
                                          {x, 10}, {y, 20},
                                          {width, 120}, {align, w},
                                          {select, S#state.hide_actions}]),
    gs:checkbutton(hide_unknown, Packer, [{label,{text,"Hide Unknown"}},
                                          {x, 10}, {y, 40},
                                          {width, 120}, {align, w},
                                          {select, S#state.hide_unknown}]),
    gs:scale(Packer, [{text,"Detail Level"},
                      {range, {?detail_level_min, ?detail_level_max}},
                      {orient, horizontal},
                      {x, 150}, {y, 0}, {height, 65}, {width, 200},
                      {pos, S#state.detail_level}, {data, detail_level}]),
    CanvasW = calc_canvas_width(S),
    CanvasH = calc_canvas_height(S),
    CanOpt = [{pack_xy, {{2, 4}, 3}}, {vscroll, right}, {hscroll, bottom},
              {scrollregion, {2, 2, CanvasW, CanvasH}}],
    Canvas = gs:canvas(Packer, CanOpt),
    gs:config(Canvas, [{buttonpress, true}, {buttonrelease, true}]),
    gs:config(Packer, [{width, S#state.width}, {height, S#state.height}]),
    gs:config(Win, [{map, true}, {keypress, true}]),
    S2 = S#state{title = Title,
                 win = Win, font = Font, packer = Packer,
                 canvas_width = CanvasW, canvas_height = CanvasH,
                 canvas = Canvas,
                 y_pos = ?initial_y * S#state.scale},
    draw_all_actors(S2).

select_font(Scale) when is_integer(Scale) ->
    case Scale of
        1 -> {courier,  7};
        2 -> {courier, 10};
        3 -> {courier, 12};
        4 -> {courier, 14};
        S -> {courier, S * 4}
    end.

create_file_menu(Bar) ->
    Button = gs:menubutton(Bar,  [{label, {text, "File"}}]),
    Menu   = gs:menu(Button, []),
    gs:menuitem(close_all, Menu,        [{label, {text, "Close Collector and all Viewers         (C) "}}]),
    gs:menuitem(close_all_others, Menu, [{label, {text, "Close other Viewers, but keep Collector (c)"}}]),
    gs:menuitem(close, Menu,            [{label, {text, "Close this  Viewer,  but keep Collector"}}]),
    gs:menuitem(Menu, [{itemtype, separator}]),

    gs:menuitem(clear_all, Menu, [{label, {text, "Clear Collector"}}]),
    gs:menuitem(load_all, Menu,  [{label, {text, "Load  Collector from the file \"et_viewer.log\""}}]),
    gs:menuitem(save_all, Menu,  [{label, {text, "Save  Collector to   the file \"et_viewer.log\""}}]).

create_viewer_menu(Bar) ->
    Button = gs:menubutton(Bar,  [{label, {text, "Viewer"}}]),
    Menu   = gs:menu(Button, []),
    gs:menuitem(Menu, [{label, {text, "Scroll this Viewer"}}, {bg, lightblue}, {enable,false}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    gs:menuitem(first, Menu,   [{label, {text, "First                     (f)"}}]),
    gs:menuitem(prev, Menu,    [{label, {text, "Prev                      (p)"}}]),
    gs:menuitem(next, Menu,    [{label, {text, "Next                      (n)"}}]),
    gs:menuitem(last, Menu,    [{label, {text, "Last                      (l)"}}]),
    gs:menuitem(refresh, Menu, [{label, {text, "Refresh                   (r)"}}]),
    gs:menuitem(Menu, [{itemtype, separator}]),                  
    gs:menuitem(up,   Menu,    [{label, {text, "Up   5                    (Up)"}}]),
    gs:menuitem(down, Menu,    [{label, {text, "Down 5                    (Down)"}}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    gs:menuitem(Menu, [{label, {text, "Search in this Viewer"}}, {bg, lightblue}, {enable,false}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    gs:menuitem({mode, all}, Menu,    [{label, {text, "Abort search. Display all (a)"}}]).

create_collector_menu(Bar) ->
    Button = gs:menubutton(Bar,  [{label, {text, "Collector"}}]),
    Menu = gs:menu(Button, []),
    gs:menuitem(Menu, [{label, {text, "Scroll all Viewers"}}, {bg, lightblue}, {enable,false}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    gs:menuitem(first_all, Menu,   [{label, {text, "First   (F)"}}]),
    gs:menuitem(prev_all, Menu,    [{label, {text, "Prev    (P)"}}]),
    gs:menuitem(next_all, Menu,    [{label, {text, "Next    (N)"}}]),
    gs:menuitem(last_all, Menu,    [{label, {text, "Last    (L)"}}]),
    gs:menuitem(refresh_all, Menu, [{label, {text, "Refresh (R)"}}]).

create_filter_menu(ActiveFilterName, Filters) ->
    Menu = gs:menu(filter_menu, filter_button, []),
    Item = fun(F, N) when F#filter.name =:= collector ->
                   Label = lists:concat([pad_string(F#filter.name, 20), "(0)"]),
                   gs:menuitem(Menu, [{label, {text, Label}}, {data, F}]),
                   N + 1;
              (F, N) ->
                   Label = lists:concat([pad_string(F#filter.name, 20), "(", N, ")"]),
                   gs:menuitem(Menu, [{label, {text, Label}}, {data, F}]),
                   N + 1
           end,
    gs:menuitem(Menu, [{label, {text, "Same Filter New Scale"}}, {bg, lightblue}, {enable,false}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    {value, Filter} = lists:keysearch(ActiveFilterName, #filter.name, Filters),
    Same    = lists:concat([pad_string(ActiveFilterName, 20), "(=)"]),
    Larger  = lists:concat([pad_string(ActiveFilterName, 20), "(+)"]),
    Smaller = lists:concat([pad_string(ActiveFilterName, 20), "(-)"]),
    gs:menuitem(Menu, [{label, {text, Same}}, {data, Filter}]),
    gs:menuitem(Menu, [{label, {text, Smaller}}, {data, Filter}]),
    gs:menuitem(Menu, [{label, {text, Larger}}, {data, Filter}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    gs:menuitem(Menu, [{label, {text, "New Filter Same Scale"}}, {bg, lightblue}, {enable,false}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    lists:foldl(Item, 1, Filters).

create_help_menu(Bar) ->
    Button = gs:menubutton(Bar,  [{label, {text, "Help"}}]),
    Menu = gs:menu(Button, []),
    gs:menuitem(Menu, [{label, {text, "Display details of an event"}},
                       {bg, lightblue}, {enable,false}]),
    gs:menuitem(Menu, [{label, {text, "    Single click on the name tag or the arrow (Mouse-1)"}},
                       {enable,false}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    gs:menuitem(Menu, [{label, {text, "Toggle actor search"}},
                       {bg, lightblue}, {enable,false}]),
    gs:menuitem(Menu, [{label, {text, "    Single click on the name tag (Mouse-1)"}},
                       {enable,false}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    gs:menuitem(Menu, [{label, {text, "Move actor"}},
                       {bg, lightblue}, {enable,false}]),
    gs:menuitem(Menu, [{label, {text, "    se drag and drop on name tag (Mouse-1)"}},
                       {enable,false}]).

clear_canvas(S) ->
    gs:destroy(S#state.canvas),
    CanvasW = calc_canvas_width(S),
    CanvasH = calc_canvas_height(S),
    CanOpt = [{pack_xy, {{2, 4}, 3}}, {vscroll, right}, {hscroll, bottom},
              {scrollregion, {2, 2, CanvasW, CanvasH}}],
    Canvas = gs:canvas(S#state.packer, CanOpt),
    gs:config(S#state.packer, [{width, S#state.width}, {height, S#state.height}]), 
    gs:config(Canvas, [{buttonpress, true}, {buttonrelease, true}]),
    S2 = S#state{refresh_needed = false,
                 y_pos          = ?initial_y * S#state.scale,
                 canvas         = Canvas,
                 canvas_width   = CanvasW, 
                 canvas_height  = CanvasH,
                 events         = queue_new()},
    draw_all_actors(S2).

calc_canvas_width(S) ->
    Min = calc_min_actors(S),
    CanvasW = ((2 * ?initial_x) + (Min * ?incr_x)) * S#state.scale,
    lists:max([CanvasW, S#state.width - (15 * S#state.scale), S#state.canvas_width]).

calc_canvas_height(S) ->
    Min = calc_min_events(S),
    CanvasH = ((2 * ?initial_y) + (Min * ?incr_y)) * S#state.scale,
    lists:max([CanvasH, S#state.height - (4 * 30), S#state.canvas_height]).

calc_min_actors(S) ->
    Max = S#state.max_actors,
    N   = length(S#state.actors),
    if
        Max =:= infinity ->
            N * 2;
        Max < N ->
            N;
        true ->
            Max
    end.
    
calc_min_events(S) ->
    Max = S#state.max_events,
    N   = queue_length(S#state.events),
    if
        Max =:= infinity ->
            N * 2;
        Max < N ->
            N;
        true ->
            Max
    end.

display_more_events(Try, S) ->
    Name = S#state.active_filter,
    {value, F} = lists:keysearch(Name, #filter.name, S#state.filters),
    FilterFun = F#filter.function,
    Fun = fun(Event, State) ->
              case catch FilterFun(Event) of
                  true ->
                      State2 = ensure_key(Event, State),
                      opt_display_event(Event, State2);
                  {true, Event2} -> 
                      State2 = ensure_key(Event2, State),
                      opt_display_event(Event2, State2);
                  false ->
                      ensure_key(Event, State);
                  Bad ->
                      Contents = {bad_filter, Name, Bad, Event},
                      Event2 = Event#event{contents = Contents,
                                           from     = bad_filter,
                                           to       = bad_filter},
                      State2 = ensure_key(Event2, State),
                      opt_display_event(Event2, State2)
              end
          end,
    Pid = S#state.collector_pid,
    S2 = et_collector:iterate(Pid, S#state.last_event, Try, Fun, S),
    case queue_length(S2#state.events) - queue_length(S#state.events) of
        Diff when Diff =:= Try ->
            %% Got as much as requested, look for more
            %% io:format("Done: ~p~n", [{Try, Diff}]),
            {noreply, S2, 0};
        _Diff when S2#state.first_event =:= S#state.first_event,
                  S2#state.last_event  =:= S#state.last_event ->
            %% Got lesser than requested, wait a while before looking for more
            %% io:format("More: ~p~n", [{Try, Diff}]),
            {noreply, S2, 500};
        _Diff ->
            %% Got lesser than requested, look for more
            %% io:format("More2: ~p~n", [{Try, Diff}]),
            {noreply, S2, 0}
    end.

ensure_key(E, S) when is_record(E, event), is_record(S, state)  ->
    Key  = et_collector:make_key(S#state.event_order, E),
    case S#state.first_event of
             first ->
                 S#state{first_event = Key, last_event = Key};
             last ->
                 S#state{first_event = Key, last_event = Key};
             _     -> 
                 S#state{last_event = Key}
    end.

opt_display_event(E, S) ->
    case S#state.display_mode of
        all ->
            display_event(E, S);
        {search_actors, _Dir, _FirstKey, Actors} ->
            %% Key = S#state.last_event,
            From = select_actor_name(E#event.from, S),
            case lists:member(From, Actors) of
                true ->
                    display_event(E, S);
                false ->
                    To = select_actor_name(E#event.to, S),
                    case lists:member(To, Actors) of
                        true ->
                            display_event(E, S);
                        false ->
                            S
                    end
            end
    end.

select_actor_name(Name, S) ->
    case lists:keymember(Name, #actor.name, S#state.actors) of
        true  ->  Name;
        false ->  ?unknown
    end.

display_event(E, S) when E#event.detail_level < S#state.detail_level ->
    {FromRefresh, From} = ensure_actor(E#event.from, S),
    {FromName, FromPos, S2} = From,
    {ToRefresh, To} = ensure_actor(E#event.to, S2),
    {ToName, ToPos, S3} = To,
    if
        FromRefresh =/= false, ToRefresh =/= false ->
            Key = S#state.last_event,
            refresh_beep(S),
            S3#state{refresh_needed = true,
                     events = queue_in(Key, S3#state.events)};
        FromName =:= ToName ->
            case S#state.hide_actions of
                true  -> 
                    S3;
                false -> 
                    Label = name_to_string(E#event.label),
                    draw_named_arrow(Label, FromName, FromPos, ToName, ToPos, S3)
            end;
        true ->
            Label = name_to_string(E#event.label),
            draw_named_arrow(Label, FromName, FromPos, ToName, ToPos, S3)
    end;
display_event(_, S) ->
    S.

draw_named_arrow(Label, FromName, FromPos, ToName, ToPos, S) ->
    Key = S#state.last_event,
    case S#state.y_pos + (?incr_y *  S#state.scale) of
        _ when S#state.hide_unknown =:= true, FromName =:= ?unknown ->
            S;
        _ when S#state.hide_unknown =:= true, ToName =:= ?unknown ->
            S;
        Y when  Y > S#state.canvas_height ->
            refresh_beep(S),
            S#state{refresh_needed = true,
                    events = queue_in(Key, S#state.events)};
        Y ->
            S2 = S#state{y_pos  = Y, events = queue_in(Key, S#state.events)},
            S3 = draw_arrow(FromPos, ToPos, S2),
            draw_label(Label, FromName, ToName, FromPos, ToPos, S3)
    end.

refresh_beep(S) ->
    case S#state.refresh_needed of
        false  -> 
            gs:config(S#state.canvas, beep),
            gs:config(S#state.canvas, beep),
            gs:config(S#state.canvas, beep);
        true -> 
            ignore
    end.

draw_arrow(Pos, Pos, S) ->
    S;
draw_arrow(FromPos, ToPos, S) ->
    Y = S#state.y_pos,
    CanOpts = [{coords, [{FromPos , Y}, {ToPos, Y}]},
               {arrow, last},{width, 1}, {fg, black}],
    gs:line(S#state.canvas, CanOpts),
    S.

draw_label(Label, FromName, ToName, FromPos, ToPos, S) ->
    Colour =
        if
            FromName =:= ?unknown, 
            ToName   =:= ?unknown -> blue; %turquoise;
            FromName =:= ?unknown -> orange;
            ToName   =:= ?unknown -> orange;
            FromPos  =:= ToPos    -> blue;
            true                  -> red
        end,
    Scale = S#state.scale,
    X = lists:min([FromPos, ToPos]) + (6 * Scale),
    Y = S#state.y_pos,
    write_text(Label, X, Y, Colour, S),
    S.

draw_all_actors(State) ->
    Scale = State#state.scale,
    Fun = fun(A, X) ->
                  draw_actor(A, X, State),
                  X + (?incr_x * Scale)
          end,
    lists:foldl(Fun, ?initial_x * Scale, State#state.actors),
    State.

%% Returns: {NeedsRefreshBool, {ActorPos, NewsS, NewActors}}
ensure_actor(Name, S) ->
    do_ensure_actor(Name, S, S#state.actors, 0).

do_ensure_actor(Name, S, [H | _], N) when H#actor.name =:= Name ->
    Pos = (?initial_x + (N * ?incr_x)) * S#state.scale,
    {false, {Name, Pos, S}};
do_ensure_actor(Name, S, [_ | T], N) ->
    do_ensure_actor(Name, S, T, N + 1);
do_ensure_actor(Name, S, [], N) ->
    %% A brand new actor, let's see if it does fit
    Pos = (?initial_x + (N * ?incr_x)) * S#state.scale,
    MaxActors = S#state.max_actors,
    if
        is_integer(MaxActors), N > MaxActors ->
            %% Failed on max_actors limit, put into unknown
            %% Assume that unknown always is in actor list
            ensure_actor(?unknown, S);
        Pos > (S#state.canvas_width - ((?initial_x - 15) * S#state.scale)) ->
            %% New actor does not fit in canvas, refresh needed
            A = create_actor(Name),
            draw_actor(A, Pos, S),
            {true, {Name, Pos, S#state{actors = S#state.actors ++ [A]}}};
        true ->
            %% New actor fits in canvas. Draw the new actor.
            A = create_actor(Name),
            draw_actor(A, Pos, S),
            {false, {Name, Pos, S#state{actors = S#state.actors ++ [A]}}}
    end.

draw_actor(A, LineX, S) ->
    Scale    = S#state.scale,
    TextX    = LineX - (5 * Scale),
    TextY    = ?initial_y * Scale,
    LineTopY = TextY + ((?incr_y  / 2) * Scale),
    LineBotY = S#state.canvas_height - ((?incr_y / 2) * Scale),
    Colour   = case A#actor.name of
                   ?unknown -> orange;
                   _        -> red
               end,
    write_text(A#actor.string, TextX, TextY, Colour, S),
    LineOpt = [{coords, [{LineX, LineTopY}, {LineX, LineBotY}]},
               {width, 1}, {fg, Colour}],
    gs:line(S#state.canvas, LineOpt).

toggle_search_for_actor(ActorName,S) ->    
    case S#state.display_mode of
        all ->
            io:format("~p: search for: ~p ++ ~p~n", [?MODULE, [], [ActorName]]),
            %% Search for this actor
            Key = S#state.first_event,
            Actors = [ActorName],
            Mode = {search_actors, forward, Key, Actors},
            change_display_mode(Mode, S);
        {search_actors, Dir, Key, Actors}->
            Actors2 = 
                case lists:member(ActorName, Actors) of
                    true ->
                        io:format("~p: search for: ~p -- ~p~n", [?MODULE, Actors, [ActorName]]),
                        %% Remove actor from search list
                        Actors -- [ActorName];
                    false ->
                        io:format("~p: search for: ~p ++ ~p~n", [?MODULE, Actors, [ActorName]]),
                        %% Add actor from search list
                        [ActorName | Actors]
                end,
            Mode2 = {search_actors, Dir, Key, Actors2},
            change_display_mode(Mode2, S)
    end.

move_actor(From, To, Actors, S) ->
    Pos      = #actor.name,
    ToName   = To#actor.name,
    FromName = From#actor.name,
    ToIx     = actor_index(ToName, Pos, Actors),
    FromIx   = actor_index(FromName, Pos, Actors),
    if
        FromIx =/= 0, ToIx =/= 0, ToIx > FromIx ->
            Actors2 = lists:keydelete(FromName, Pos, Actors),
            Actors3 = insert_actor_after(From, To, Actors2),
            S2 = S#state{actors = Actors3},
            refresh_main_window(S2);
        FromIx =/= 0, ToIx =/= 0 ->
            Actors2 = lists:keydelete(FromName, Pos, Actors),
            Actors3 = insert_actor_before(From, To, Actors2),
            S2 = S#state{actors = Actors3},
            refresh_main_window(S2);
        true ->
            %% Ignore
            S
    end.

insert_actor_after(From, To, [H | T]) ->
    case To#actor.name =:= H#actor.name of
        true  -> [H, From | T];
        false -> [H | insert_actor_after(From, To, T)]
    end;
insert_actor_after(_From, _To, []) ->
    [].

insert_actor_before(From, To, [H | T]) ->
    case To#actor.name =:= H#actor.name of
        true  -> [From, H | T];
        false -> [H | insert_actor_before(From, To, T)]
    end;
insert_actor_before(_From, _To, []) ->
    [].

actor_index(_Key, _Pos, []) ->
    0;
actor_index(Key, Pos, [H | T]) ->
    case Key =:= element(Pos, H) of
        false -> actor_index(Key, Pos, T) + 1;
        true  -> 1
    end.

y_to_n(Y, S) ->
    Y2   = ((Y / S#state.scale) - ?initial_y + (?incr_y / 2)),
    N    = round(Y2 / ?incr_y - 0.2),
    MaxN = queue_length(S#state.events),
    if
        N =< 0   -> actor;
        N > MaxN -> actor;
        true     -> {event, N}
    end.

x_to_n(X, S) ->
    Scale   = S#state.scale,
    Len = length(S#state.actors),
    X2 = X - (?initial_x * Scale),
    N  = X2 / (?incr_x * Scale),
    N2 = trunc(N + 1.5),
    if
        N2 > Len -> Len;
        N2 < 1   -> 1;
        true     -> N2
    end.

write_text(Text, X, Y, Colour, S) ->
    Opt = [{coords, [{X, Y - (?incr_y * S#state.scale / 2)}]},
           {font, S#state.font}, {fg, Colour}, {text, Text}],
    gs:text(S#state.canvas, Opt).

create_contents_window(Event, S) ->
    Options = [{viewer_pid, self()},
               {event, Event},
               {event_order, S#state.event_order},
               {active_filter, S#state.active_filter}
               | S#state.filters],
    case et_gs_contents_viewer:start_link(Options) of
        {ok, _Pid} ->
            S;
        {error, Reason} ->
            ok = error_logger:format("~p(~p): create_contents_window(~p) ->~n     ~p~n",
                                     [?MODULE, self(), Options, Reason]),
            S
    end.

%%%----------------------------------------------------------------------
%%% String padding of actors
%%%----------------------------------------------------------------------

create_actor(Name) ->
    String = name_to_string(Name),
    PaddedString = pad_string(String, 8),
    #actor{name = Name, string = PaddedString}.

name_to_string(Name) ->
    case catch io_lib:format("~s", [Name]) of
        {'EXIT', _} -> lists:flatten(io_lib:format("~w", [Name]));
        GoodString  -> lists:flatten(GoodString)
    end.

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

%%%----------------------------------------------------------------------
%%% Queue management
%%%----------------------------------------------------------------------

queue_new() ->
    {0, [], []}.

queue_in(X, {Size, In, Out}) ->
    {Size + 1, [X | In], Out}.

%% queue_out(Q) ->
%%     case Q of
%%         {Size, In, [H | Out]} -> {{value, H}, {Size - 1, In, Out}};
%%         {Size, [], []}        -> {empty, {Size, [], []}};
%%         {Size, In, _}         -> queue_out({Size, [], lists:reverse(In)})
%%     end.

queue_to_list({_Size, [], Out}) ->
    Out;
queue_to_list({_Size, In, Out}) ->
    Out ++ lists:reverse(In).

queue_length({Size, _In, _Out}) ->
    Size.

list_to_queue(List) when is_list(List) ->
    {length(List), [], List}.
