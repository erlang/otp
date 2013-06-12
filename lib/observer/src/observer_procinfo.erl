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
		pid,
		pages=[]
	       }).

-record(worker, {panel, callback}).

start(Process, ParentFrame, Parent) ->
    wx_object:start_link(?MODULE, [Process, ParentFrame, Parent], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Pid, ParentFrame, Parent]) ->
    try
	Title=case observer_wx:try_rpc(node(Pid), erlang, process_info, [Pid, registered_name]) of
		  [] -> io_lib:format("~p",[Pid]);
		  {registered_name, Registered} -> io_lib:format("~p (~p)",[Registered, Pid]);
		  undefined -> throw(process_undefined)
	      end,
	Frame=wxFrame:new(ParentFrame, ?wxID_ANY, [atom_to_list(node(Pid)), $:, Title],
			  [{style, ?wxDEFAULT_FRAME_STYLE}, {size, {850,600}}]),
	MenuBar = wxMenuBar:new(),
	create_menus(MenuBar),
	wxFrame:setMenuBar(Frame, MenuBar),

	Notebook = wxNotebook:new(Frame, ?ID_NOTEBOOK, [{style, ?wxBK_DEFAULT}]),

	ProcessPage = init_panel(Notebook, "Process Information", Pid, fun init_process_page/2),
	MessagePage = init_panel(Notebook, "Messages", Pid, fun init_message_page/2),
	DictPage    = init_panel(Notebook, "Dictionary", Pid, fun init_dict_page/2),
	StackPage   = init_panel(Notebook, "Stack Trace", Pid, fun init_stack_page/2),
	StatePage   = init_panel(Notebook, "State", Pid, fun init_state_page/2),

	wxFrame:connect(Frame, close_window),
	wxMenu:connect(Frame, command_menu_selected),
	%% wxNotebook:connect(Notebook, command_notebook_page_changed, [{skip,true}]),
	wxFrame:show(Frame),
	{Frame, #state{parent=Parent,
		       pid=Pid,
		       frame=Frame,
		       pages=[ProcessPage,MessagePage,DictPage,StackPage,StatePage]
		      }}
    catch error:{badrpc, _} ->
	    observer_wx:return_to_localnode(ParentFrame, node(Pid)),
	    {stop, badrpc};
	  process_undefined ->
	    observer_lib:display_info_dialog("No such alive process"),
	    {stop, normal}
    end.

init_panel(Notebook, Str, Pid, Fun) ->
    Panel  = wxPanel:new(Notebook),
    Sizer  = wxBoxSizer:new(?wxHORIZONTAL),
    {Window,Callback} = Fun(Panel, Pid),
    wxSizer:add(Sizer, Window, [{flag, ?wxEXPAND bor ?wxALL}, {proportion, 1}, {border, 5}]),
    wxPanel:setSizer(Panel, Sizer),
    true = wxNotebook:addPage(Notebook, Panel, Str),
    #worker{panel=Panel, callback=Callback}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Callbacks%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(#wx{event=#wxClose{type=close_window}}, State) ->
    {stop, normal, State};

handle_event(#wx{id=?wxID_CLOSE, event=#wxCommand{type=command_menu_selected}}, State) ->
    {stop, normal, State};

handle_event(#wx{id=?REFRESH}, #state{frame=Frame, pid=Pid, pages=Pages}=State) ->
    try [(W#worker.callback)() || W <- Pages]
    catch process_undefined ->
	    wxFrame:setTitle(Frame, io_lib:format("*DEAD* ~p",[Pid]))
    end,
    {noreply, State};

handle_event(Event, _State) ->
    error({unhandled_event, Event}).

handle_info(_Info, State) ->
    %% io:format("~p: ~p, Handle info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

handle_call(Call, From, _State) ->
    error({unhandled_call, Call, From}).

handle_cast(Cast, _State) ->
    error({unhandled_cast, Cast}).

terminate(_Reason, #state{parent=Parent,pid=Pid,frame=Frame}) ->
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
    Fields0 = process_info_fields(Pid),
    {FPanel, _, UpFields} = observer_lib:display_info(Panel, Fields0),
    {FPanel, fun() ->
		     Fields = process_info_fields(Pid),
		     observer_lib:update_info(UpFields, Fields)
	     end}.

init_text_page(Parent) ->
    Style = ?wxTE_MULTILINE bor ?wxTE_RICH2 bor ?wxTE_READONLY,
    Text = wxTextCtrl:new(Parent, ?wxID_ANY, [{style, Style}]),
    Font = observer_wx:get_attrib({font, fixed}),
    Attr = wxTextAttr:new(?wxBLACK, [{font, Font}]),
    true = wxTextCtrl:setDefaultStyle(Text, Attr),
    wxTextAttr:destroy(Attr),
    Text.

init_message_page(Parent, Pid) ->
    Text = init_text_page(Parent),
    Format = fun(Message, Number) ->
		     {io_lib:format("~-4.w ~p~n", [Number, Message]),
		      Number+1}
	     end,
    Update = fun() ->
		     case observer_wx:try_rpc(node(Pid), erlang, process_info,
					      [Pid, messages])
		     of
			 {messages,RawMessages} ->
			     {Messages,_} = lists:mapfoldl(Format, 1, RawMessages),
			     Last = wxTextCtrl:getLastPosition(Text),
			     wxTextCtrl:remove(Text, 0, Last),
			     case Messages =:= [] of
				 true ->
				     wxTextCtrl:writeText(Text, "No messages");
				 false ->
				     wxTextCtrl:writeText(Text, Messages)
			     end;
			 _ ->
			     throw(process_undefined)
		     end
	     end,
    Update(),
    {Text, Update}.

init_dict_page(Parent, Pid) ->
    Text = init_text_page(Parent),
    Update = fun() ->
		     case observer_wx:try_rpc(node(Pid), erlang, process_info, [Pid, dictionary])
		     of
			 {dictionary,RawDict} ->
			     Dict = [io_lib:format("~-20.w ~p~n", [K, V]) || {K, V} <- RawDict],
			     Last = wxTextCtrl:getLastPosition(Text),
			     wxTextCtrl:remove(Text, 0, Last),
			     wxTextCtrl:writeText(Text, Dict);
			 _ ->
			     throw(process_undefined)
		     end
	     end,
    Update(),
    {Text, Update}.

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
			     observer_wx:try_rpc(node(Pid), erlang, process_info,
						 [Pid, current_stacktrace]),
			     wxListCtrl:deleteAllItems(LCtrl),
			     wx:foldl(fun({M, F, A, Info}, Row) ->
					      _Item = wxListCtrl:insertItem(LCtrl, Row, ""),
					      ?EVEN(Row) andalso
						  wxListCtrl:setItemBackgroundColour(LCtrl, Row, ?BG_EVEN),
					      wxListCtrl:setItem(LCtrl, Row, 0, observer_lib:to_str({M,F,A})),
					      FileLine = case Info of
							     [{file,File},{line,Line}] ->
								 io_lib:format("~s:~w", [File,Line]);
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


init_state_page(Parent, Pid) ->
    Text = init_text_page(Parent),
    Update = fun() ->
		     %% First, test if sys:get_status/2 have any chance to return an answer
		     case rpc:call(node(Pid), proc_lib, translate_initial_call, [Pid])
		     of
			 %% Not a gen process
			 {proc_lib,init_p,5} -> Misc = [{"Information", "Not available"}];
			 %% May be a gen process
			 {M, _F, _A} ->
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
				 {status, _, {module, _}, [_PDict, _SysState, _Parent, _Dbg,
							   [Header,{data, First},{data, Second}]]} ->
				     Misc = [{"Behaviour", B}] ++ [Header] ++ First ++ Second;
				 {status, _, {module, _}, [_PDict, _SysState, _Parent, _Dbg,
							   [Header,{data, First}, OtherFormat]]} ->
				     Misc = [{"Behaviour", B}] ++ [Header] ++ First ++ [{"State",OtherFormat}];
				 {status, _, {module, _}, [_PDict, _SysState, _Parent, _Dbg,
							   OtherFormat]} ->
				     %% Formatted status ?
				     case lists:keyfind(format_status, 1, rpc:call(node(Pid), M, module_info, [exports])) of
					 false	-> Opt = {"Format", unknown};
					 _	-> Opt = {"Format", overriden}
				     end,
				     Misc = [{"Behaviour", B}] ++ [Opt, {"State",OtherFormat}];
				 {badrpc,{'EXIT',{timeout, _}}} ->
				     Misc = [{"Information","Timed out"},
					     {"Tip","system messages are probably not treated by this process"}]
			     end;
			 _ -> Misc=[], throw(process_undefined)
		     end,
		     Dict = [io_lib:format("~-20.s ~tp~n", [K, V]) || {K, V} <- Misc],
		     Last = wxTextCtrl:getLastPosition(Text),
		     wxTextCtrl:remove(Text, 0, Last),
		     wxTextCtrl:writeText(Text, Dict)
	     end,
    Update(),
    {Text, Update}.


create_menus(MenuBar) ->
    Menus = [{"File", [#create_menu{id=?wxID_CLOSE, text="Close"}]},
	     {"View", [#create_menu{id=?REFRESH, text="Refresh\tCtrl-R"}]}],
    observer_lib:create_menus(Menus, MenuBar, new_window).

process_info_fields(Pid) ->
    Struct = [{"Overview",
	       [{"Initial Call",     initial_call},
		{"Current Function", current_function},
		{"Registered Name",  registered_name},
		{"Status",           status},
		{"Message Queue Len",message_queue_len},
		{"Priority",         priority},
		{"Trap Exit",        trap_exit},
		{"Reductions",       reductions},
		{"Binary",           binary},
		{"Last Calls",       last_calls},
		{"Catch Level",      catchlevel},
		{"Trace",            trace},
		{"Suspending",       suspending},
		{"Sequential Trace Token", sequential_trace_token},
		{"Error Handler",    error_handler}]},
	      {"Connections",
	       [{"Group Leader",     group_leader},
		{"Links",            links},
		{"Monitors",         monitors},
		{"Monitored by",     monitored_by}]},
	      {"Memory and Garbage Collection", right,
	       [{"Memory",           {bytes, memory}},
		{"Stack and Heaps",  {bytes, total_heap_size}},
		{"Heap Size",        {bytes, heap_size}},
		{"Stack Size",       {bytes, stack_size}},
		{"GC Min Heap Size", {bytes, get_gc_info(min_heap_size)}},
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
