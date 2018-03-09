%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2017. All Rights Reserved.
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

-module(observer_procinfo).

-behaviour(wx_object).

-export([start/3]).

-export([init/1, handle_event/2, handle_cast/2, terminate/2, code_change/3,
	 handle_call/3, handle_info/2]).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-define(REFRESH, 601).
-define(SELECT_ALL, 603).
-define(ID_NOTEBOOK, 604).

-record(state, {parent,
		frame,
		notebook,
		pid,
		pages=[],
		expand_table,
		expand_wins=[]
	       }).

-record(worker, {panel, callback}).

-record(io, {rdata=""}).

start(Process, ParentFrame, Parent) ->
    wx_object:start_link(?MODULE, [Process, ParentFrame, Parent], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Pid, ParentFrame, Parent]) ->
    try
	Table = ets:new(observer_expand,[set,public]),
	Title=case observer_wx:try_rpc(node(Pid), erlang, process_info, [Pid, registered_name]) of
		  [] -> io_lib:format("~p",[Pid]);
		  {registered_name, Registered} -> io_lib:format("~tp (~p)",[Registered, Pid]);
		  undefined -> throw(process_undefined)
	      end,
	Frame=wxFrame:new(ParentFrame, ?wxID_ANY, [atom_to_list(node(Pid)), $:, Title],
			  [{style, ?wxDEFAULT_FRAME_STYLE}, {size, {850,600}}]),
	MenuBar = wxMenuBar:new(),
	create_menus(MenuBar),
	wxFrame:setMenuBar(Frame, MenuBar),

	Notebook = wxNotebook:new(Frame, ?ID_NOTEBOOK, [{style, ?wxBK_DEFAULT}]),

	ProcessPage = init_panel(Notebook, "Process Information", [Pid], fun init_process_page/2),
	MessagePage = init_panel(Notebook, "Messages", [Pid,Table], fun init_message_page/3),
	DictPage    = init_panel(Notebook, "Dictionary", [Pid,Table], fun init_dict_page/3),
	StackPage   = init_panel(Notebook, "Stack Trace", [Pid], fun init_stack_page/2),
	StatePage   = init_panel(Notebook, "State", [Pid,Table], fun init_state_page/3),
        Ps = case gen_server:call(observer, log_status) of
		 true  -> [init_panel(Notebook, "Log", [Pid,Table], fun init_log_page/3)];
		 false -> []
	     end,

	wxFrame:connect(Frame, close_window),
	wxMenu:connect(Frame, command_menu_selected),
	%% wxNotebook:connect(Notebook, command_notebook_page_changed, [{skip,true}]),
	wxFrame:show(Frame),
	{Frame, #state{parent=Parent,
		       pid=Pid,
		       frame=Frame,
		       notebook=Notebook,
		       pages=[ProcessPage,MessagePage,DictPage,StackPage,StatePage|Ps],
		       expand_table=Table
		      }}
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(ParentFrame, node(Pid)),
	    {stop, badrpc};
	  process_undefined ->
	    observer_lib:display_info_dialog(ParentFrame,"No such alive process"),
	    {stop, normal}
    end.

init_panel(Notebook, Str, FunArgs, Fun) ->
    Panel  = wxPanel:new(Notebook),
    Sizer  = wxBoxSizer:new(?wxHORIZONTAL),
    {Window,Callback} = apply(Fun,[Panel|FunArgs]),
    wxSizer:add(Sizer, Window, [{flag, ?wxEXPAND bor ?wxALL}, {proportion, 1}, {border, 5}]),
    wxPanel:setSizer(Panel, Sizer),
    true = wxNotebook:addPage(Notebook, Panel, Str),
    #worker{panel=Panel, callback=Callback}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Callbacks%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(#wx{event=#wxClose{type=close_window}}, State) ->
    {stop, normal, State};

handle_event(#wx{id=?wxID_CLOSE, event=#wxCommand{type=command_menu_selected}}, State) ->
    {stop, normal, State};

handle_event(#wx{id=?REFRESH}, #state{frame=Frame, pid=Pid, pages=Pages, expand_table=T}=State) ->
    ets:delete_all_objects(T),
    try [(W#worker.callback)() || W <- Pages]
    catch process_undefined ->
	    wxFrame:setTitle(Frame, io_lib:format("*DEAD* ~p",[Pid]))
    end,
    {noreply, State};

handle_event(#wx{obj=MoreEntry,event=#wxMouse{type=left_down},userData={more,More}}, State) ->
    observer_lib:add_scroll_entries(MoreEntry,More),
    {noreply, State};

handle_event(#wx{event=#wxMouse{type=left_down}, userData=TargetPid}, State) ->
    observer ! {open_link, TargetPid},
    {noreply, State};

handle_event(#wx{obj=Obj, event=#wxMouse{type=enter_window}}, State) ->
    wxStaticText:setForegroundColour(Obj,{0,0,100,255}),
    {noreply, State};

handle_event(#wx{obj=Obj, event=#wxMouse{type=leave_window}}, State) ->
    wxStaticText:setForegroundColour(Obj,?wxBLUE),
    {noreply, State};

handle_event(#wx{event=#wxHtmlLink{linkInfo=#wxHtmlLinkInfo{href=Href}}},
	     #state{frame=Frame,expand_table=T,expand_wins=Opened0}=State) ->
    {Type, Rest} = case Href of
		       "#Term?"++Keys   -> {cdv_term_cb, Keys};
		       "#OBSBinary?"++Keys -> {cdv_bin_cb, Keys};
		       _ -> {other, Href}
		   end,
    case Type of
	other ->
	    observer ! {open_link, Href},
	    {noreply, State};
	Callback ->
	    [{"key1",Key1},{"key2",Key2},{"key3",Key3}] = uri_string:dissect_query(Rest),
	    Id = {obs, {T,{list_to_integer(Key1),
			   list_to_integer(Key2),
			   list_to_integer(Key3)}}},
	    Opened =
		case lists:keyfind(Id,1,Opened0) of
		    false ->
			Win = cdv_detail_wx:start_link(Id,[],Frame,Callback,obs),
			[{Id,Win}|Opened0];
		    {_,Win} ->
			wxFrame:raise(Win),
			Opened0
		end,
	    {noreply,State#state{expand_wins=Opened}}
    end;

handle_event(#wx{event=#wxHtmlLink{linkInfo=#wxHtmlLinkInfo{href=Info}}}, State) ->
    observer ! {open_link, Info},
    {noreply, State};

handle_event(Event, _State) ->
    error({unhandled_event, Event}).

handle_info({get_debug_info, From}, State = #state{notebook=Notebook}) ->
    From ! {procinfo_debug, Notebook},
    {noreply, State};
handle_info(_Info, State) ->
    %% io:format("~p: ~p, Handle info: ~tp~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

handle_call(Call, From, _State) ->
    error({unhandled_call, Call, From}).

handle_cast({detail_win_closed,Id}, #state{expand_wins=Opened0}=State) ->
    Opened = lists:keydelete(Id,1,Opened0),
    {noreply,State#state{expand_wins=Opened}};

handle_cast(Cast, _State) ->
    error({unhandled_cast, Cast}).

terminate(_Reason, #state{parent=Parent,pid=Pid,frame=Frame,expand_table=T}) ->
    T=/=undefined andalso ets:delete(T),
    Parent ! {procinfo_menu_closed, Pid},
    case Frame of
	undefined ->  ok;
	_ -> wxFrame:destroy(Frame)
    end,
    ok.

code_change(_, _, State) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_process_page(Panel, Pid) ->
    WSz = observer_wx:try_rpc(node(Pid), erlang, system_info,[wordsize]),
    Fields0 = process_info_fields(Pid, WSz),
    {FPanel, _, UpFields} = observer_lib:display_info(Panel, Fields0),
    {FPanel, fun() ->
		     Fields = process_info_fields(Pid, WSz),
		     observer_lib:update_info(UpFields, Fields)
	     end}.


init_message_page(Parent, Pid, Table) ->
    Win = observer_lib:html_window(Parent),
    Update = fun() ->
		     case observer_wx:try_rpc(node(Pid), erlang, process_info,
					      [Pid, messages])
		     of
			 {messages, Messages} ->
			     Html = observer_html_lib:expandable_term("Message Queue", Messages, Table),
			     wxHtmlWindow:setPage(Win, Html);
			 _ ->
			     throw(process_undefined)
		     end
	     end,
    Update(),
    {Win, Update}.

init_dict_page(Parent, Pid, Table) ->
    Win = observer_lib:html_window(Parent),
    Update = fun() ->
		     case observer_wx:try_rpc(node(Pid), erlang, process_info, [Pid, dictionary])
		     of
			 {dictionary,Dict} ->
			     Html = observer_html_lib:expandable_term("Dictionary", Dict, Table),
			     wxHtmlWindow:setPage(Win, Html);
			 _ ->
			     throw(process_undefined)
		     end
	     end,
    Update(),
    {Win, Update}.

init_stack_page(Parent, Pid) ->
    LCtrl = wxListCtrl:new(Parent, [{style, ?wxLC_REPORT bor ?wxLC_HRULES}]),
    Li = wxListItem:new(),
    wxListItem:setText(Li, "Module:Function/Arg"),
    wxListCtrl:insertColumn(LCtrl, 0, Li),
    wxListCtrl:setColumnWidth(LCtrl, 0, 300),
    wxListItem:setText(Li, "File:LineNumber"),
    wxListCtrl:insertColumn(LCtrl, 1, Li),
    wxListCtrl:setColumnWidth(LCtrl, 1, 300),
    wxListItem:destroy(Li),
    Update = fun() ->
		     case observer_wx:try_rpc(node(Pid), erlang, process_info,
					      [Pid, current_stacktrace])
		     of
			 {current_stacktrace,RawBt} ->
			     wxListCtrl:deleteAllItems(LCtrl),
			     wx:foldl(fun({M, F, A, Info}, Row) ->
					      _Item = wxListCtrl:insertItem(LCtrl, Row, ""),
					      ?EVEN(Row) andalso
						  wxListCtrl:setItemBackgroundColour(LCtrl, Row, ?BG_EVEN),
					      wxListCtrl:setItem(LCtrl, Row, 0, observer_lib:to_str({M,F,A})),
					      FileLine = case Info of
							     [{file,File},{line,Line}] ->
								 io_lib:format("~ts:~w", [File,Line]);
							     _ ->
								 []
							 end,
					      wxListCtrl:setItem(LCtrl, Row, 1, FileLine),
					      Row+1
				      end, 0, RawBt);
			 _ ->
			     throw(process_undefined)
		     end
	     end,
    Resize = fun(#wx{event=#wxSize{size={W,_}}},Ev) ->
		     wxEvent:skip(Ev),
		     observer_lib:set_listctrl_col_size(LCtrl, W)
	     end,
    wxListCtrl:connect(LCtrl, size, [{callback, Resize}]),
    Update(),
    {LCtrl, Update}.

init_state_page(Parent, Pid, Table) ->
    Win = observer_lib:html_window(Parent),
    Update = fun() ->
		     StateInfo = fetch_state_info(Pid),
		     Html = observer_html_lib:expandable_term("ProcState", StateInfo, Table),
		     wxHtmlWindow:setPage(Win, Html)
	     end,
    Update(),
    {Win, Update}.

fetch_state_info(Pid) ->
    %% First, test if sys:get_status/2 have any chance to return an answer
    case rpc:call(node(Pid), proc_lib, translate_initial_call, [Pid]) of
	%% Not a gen process
	{proc_lib,init_p,5} -> [];
	%% May be a gen process
	{M, _F, _A} -> fetch_state_info2(Pid, M);
	_ -> throw(process_undefined)
    end.

fetch_state_info2(Pid, M) ->
    %% Get the behavio(u)r
    I = rpc:call(node(Pid), M, module_info, [attributes]),
    case lists:keyfind(behaviour, 1, I) of
	false -> case lists:keyfind(behavior, 1, I) of
		     false		-> B = undefined;
		     {behavior, [B]}	-> B
		 end;
	{behaviour, [B]} -> B
    end,
    %% but not sure that system messages are treated by this process
    %% so using a rpc with a small timeout in order not to lag the display
    case rpc:call(node(Pid), sys, get_status, [Pid, 200])
    of
	{status, _, {module, _},
	 [_PDict, _SysState, _Parent, _Dbg,
	  [Header,{data, First},{data, Second}|_]]} ->
	    [{"Behaviour", B}, Header] ++ First ++ Second;
	{status, _, {module, _},
	 [_PDict, _SysState, _Parent, _Dbg,
	  [Header,{data, First}, OtherFormat]]} ->
	    [{"Behaviour", B}, Header] ++ First ++ [{"State",OtherFormat}];
	{status, _, {module, _},
	 [_PDict, _SysState, _Parent, _Dbg, OtherFormat]} ->
	    %% Formatted status ?
	    case lists:keyfind(format_status, 1, rpc:call(node(Pid), M, module_info, [exports])) of
		false	-> Opt = {"Format", unknown};
		_	-> Opt = {"Format", overriden}
	    end,
	    [{"Behaviour", B}, Opt, {"State",OtherFormat}];
	{badrpc,{'EXIT',{timeout, _}}} -> []
    end.

init_log_page(Parent, Pid, Table) ->
    Win = observer_lib:html_window(Parent),
    Update = fun() ->
		     Fd = spawn_link(fun() -> io_server() end),
		     rpc:call(node(Pid), rb, rescan, [[{start_log, Fd}]]),
		     rpc:call(node(Pid), rb, grep, [local_pid_str(Pid)]),
		     Logs = io_get_data(Fd),
		     %% Replace remote local pid notation to global notation
		     Pref = global_pid_node_pref(Pid),
		     ExpPid = re:replace(Logs,"<0\.","<" ++ Pref ++ ".",[global, {return, list}]),
		     %% Try to keep same look by removing blanks at right of rewritten PID
		     NbBlanks = length(Pref) - 1,
		     Re = "(<" ++ Pref ++ "\.[^>]{1,}>)[ ]{"++ integer_to_list(NbBlanks) ++ "}",
		     Look = re:replace(ExpPid, Re, "\\1", [global, {return, list}]),
		     Html = observer_html_lib:expandable_term("SaslLog", Look, Table),
		     wxHtmlWindow:setPage(Win, Html)
	     end,
    Update(),
    {Win, Update}.

create_menus(MenuBar) ->
    Menus = [{"File", [#create_menu{id=?wxID_CLOSE, text="Close"}]},
	     {"View", [#create_menu{id=?REFRESH, text="Refresh\tCtrl-R"}]}],
    observer_lib:create_menus(Menus, MenuBar, new_window).

process_info_fields(Pid, WSz) ->
    Struct = [{"Overview",
	       [{"Initial Call",     initial_call},
		{"Current Function", current_function},
		{"Registered Name",  registered_name},
		{"Status",           status},
		{"Message Queue Len",message_queue_len},
		{"Group Leader",     {click, group_leader}},
		{"Priority",         priority},
		{"Trap Exit",        trap_exit},
		{"Reductions",       reductions},
		{"Binary",           fun(Data) -> stringify_bins(Data) end},
		{"Last Calls",       last_calls},
		{"Catch Level",      catchlevel},
		{"Trace",            trace},
		{"Suspending",       suspending},
		{"Sequential Trace Token", sequential_trace_token},
		{"Error Handler",    error_handler}]},
	      {scroll_boxes,
	       [{"Links",            {click, links}},
		{"Monitors",         {click, filter_monitor_info()}},
		{"Monitored by",     {click, monitored_by}}]},
	      {"Memory and Garbage Collection", right,
	       [{"Memory",           {bytes, memory}},
		{"Stack and Heaps",  {{words,WSz}, total_heap_size}},
		{"Heap Size",        {{words,WSz}, heap_size}},
		{"Stack Size",       {{words,WSz}, stack_size}},
		{"GC Min Heap Size", {{words,WSz}, get_gc_info(min_heap_size)}},
		{"GC FullSweep After", get_gc_info(fullsweep_after)}
	       ]}],
    case observer_wx:try_rpc(node(Pid), erlang, process_info, [Pid, item_list()]) of
	RawInfo when is_list(RawInfo) ->
	    observer_lib:fill_info(Struct, RawInfo);
	_ ->
	    throw(process_undefined)
    end.

item_list() ->
    [ %% backtrace,
      binary,
      catchlevel,
      current_function,
      %% dictionary,
      error_handler,
      garbage_collection,
      group_leader,
      heap_size,
      initial_call,
      last_calls,
      links,
      memory,
      message_queue_len,
      %% messages,
      monitored_by,
      monitors,
      priority,
      reductions,
      registered_name,
      sequential_trace_token,
      stack_size,
      status,
      suspending,
      total_heap_size,
      trace,
      trap_exit].

get_gc_info(Arg) ->
    fun(Data) ->
	    GC = proplists:get_value(garbage_collection, Data),
	    proplists:get_value(Arg, GC)
    end.

filter_monitor_info() ->
    fun(Data) ->
	    Ms = proplists:get_value(monitors, Data),
	    [Id || {_Type, Id} <- Ms] % Type is process or port
    end.

stringify_bins(Data) ->
    Bins = proplists:get_value(binary, Data),
    [lists:flatten(io_lib:format("<< ~s, refc ~w>>", [observer_lib:to_str({bytes,Sz}),Refc]))
     || {_Ptr, Sz, Refc} <- Bins].

local_pid_str(Pid) ->
    %% observer can observe remote nodes
    %% There is no function to get the local
    %% pid from the remote pid ...
    %% So grep will fail to find remote pid in remote local log.
    %% i.e. <4589.42.1> will not be found, but <0.42.1> will
    %% Let's replace first integer by zero
    "<0" ++ re:replace(pid_to_list(Pid),"\<([0-9]{1,})","",[{return, list}]).

global_pid_node_pref(Pid) ->
    %% Global PID node prefix : X of <X.Y.Z>
    [NodePrefix|_] = string:lexemes(pid_to_list(Pid),"<."),
    NodePrefix.

io_get_data(Pid) ->
    Pid ! {self(), get_data_and_close},
    receive
	{Pid, data, Data} ->  lists:flatten(Data)
    end.

io_server() ->
    io_server(#io{}).

io_server(State) ->
    receive
	{io_request, From, ReplyAs, Request} ->
	    {_, Reply, NewState} =  io_request(Request,State),
	    From ! {io_reply, ReplyAs, Reply},
	    io_server(NewState);
	{Pid, get_data_and_close} ->
	    Pid ! {self(), data, lists:reverse(State#io.rdata)},
	    normal;
	_Unknown ->
	    io_server(State)
    end.

io_request({put_chars, _Encoding, Chars}, State = #io{rdata=Data}) ->
    {ok, ok, State#io{rdata=[Chars|Data]}};
io_request({put_chars, Encoding, Module, Function, Args}, State) ->
    try
	io_request({put_chars, Encoding, apply(Module, Function, Args)}, State)
    catch _:_ ->
	    {error, {error, Function}, State}
    end;
io_request(_Req, State) ->
    %% io:format("~p: Unknown req: ~tp ~n",[?LINE, _Req]),
    {ok, {error, request}, State}.
