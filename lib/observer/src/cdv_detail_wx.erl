%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2017. All Rights Reserved.
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
-module(cdv_detail_wx).

-behaviour(wx_object).

-export([start_link/5]).

-export([init/1, handle_event/2, handle_cast/2, terminate/2, code_change/3,
	 handle_call/3, handle_info/2]).

-include_lib("wx/include/wx.hrl").
-include("crashdump_viewer.hrl").
-include("observer_defs.hrl").

-record(state, {parent,
		frame,
		id,
		pages=[]
	       }).

%% Defines
-define(ID_NOTEBOOK, 604).

%% Detail view
start_link(Id, Data, ParentFrame, Callback, App) ->
    wx_object:start_link(?MODULE,[Id,Data,ParentFrame,Callback,App,self()],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Id, Data, ParentFrame, Callback, App, Parent]) ->
    display_progress(ParentFrame,App),
    case Callback:get_details(Id, Data) of
	{ok,Details} ->
            display_progress_pulse(Callback,Id),
	    init(Id,ParentFrame,Callback,App,Parent,Details);
	{yes_no, Info, Fun} ->
            destroy_progress(App),
	    case observer_lib:display_yes_no_dialog(Info) of
		?wxID_YES -> Fun();
		?wxID_NO -> ok
	    end,
	    {stop,normal};
	{info,Info} ->
            destroy_progress(App),
	    observer_lib:display_info_dialog(ParentFrame,Info),
	    {stop,normal}
    end.

%% Display progress bar only if the calling app is crashdump_viewer
display_progress(ParentFrame,cdv) ->
    observer_lib:display_progress_dialog(ParentFrame,
                                         "Crashdump Viewer",
                                         "Reading data");
display_progress(_,_) ->
    ok.

%% Display pulse while creating process detail page with much data
display_progress_pulse(cdv_proc_cb,Pid) ->
    observer_lib:report_progress({ok,"Displaying data for "++Pid}),
    observer_lib:report_progress({ok,start_pulse});
display_progress_pulse(_,_) ->
    ok.

destroy_progress(cdv) ->
    observer_lib:sync_destroy_progress_dialog();
destroy_progress(_) ->
    ok.

init(Id,ParentFrame,Callback,App,Parent,{Title,Info,TW}) ->
    Frame=wxFrame:new(ParentFrame, ?wxID_ANY, [Title],
		      [{style, ?wxDEFAULT_FRAME_STYLE}, {size, {850,600}}]),
    MenuBar = wxMenuBar:new(),
    create_menus(MenuBar),
    wxFrame:setMenuBar(Frame, MenuBar),

    Panel = wxPanel:new(Frame, []),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    {InfoPanel,Pages} = create_pages(Panel,Callback:detail_pages(),[Info]),
    wxSizer:add(Sizer, InfoPanel, [{proportion, 1}, {flag, ?wxEXPAND}]),

    case TW of
	[] ->
	    undefined;
	_ ->
	    StatusBar = observer_lib:create_status_bar(Panel),
	    wxSizer:add(Sizer, StatusBar, [{flag, ?wxEXPAND bor ?wxALL},
					   {proportion, 0},
					   {border,4}]),
	    wxTextCtrl:writeText(StatusBar, TW),
	    StatusBar
    end,

    wxPanel:setSizer(Panel, Sizer),

    wxFrame:connect(Frame, close_window),
    wxMenu:connect(Frame, command_menu_selected),
    wxFrame:show(Frame),
    destroy_progress(App),
    {Frame, #state{parent=Parent,
		   id=Id,
		   frame=Frame,
		   pages=Pages
		  }}.

create_pages(Panel,[{_PageTitle,Fun}],FunArgs) ->
    %% Only one page - don't create notebook
    Page = init_panel(Panel, Fun, FunArgs),
    {Page,[Page]};
create_pages(Panel,PageSpecs,FunArgs) ->
    Notebook = wxNotebook:new(Panel, ?ID_NOTEBOOK, [{style, ?wxBK_DEFAULT}]),
    Pages = [init_tab(Notebook, PageTitle, Fun, FunArgs)
	     || {PageTitle,Fun} <- PageSpecs],
    {Notebook, Pages}.

init_tab(Notebook,Title,Fun,FunArgs) ->
    Panel = init_panel(Notebook,Fun,FunArgs),
    true = wxNotebook:addPage(Notebook, Panel, Title),
    Panel.

init_panel(ParentWin, Fun, FunArgs) ->
    Panel  = wxScrolledWindow:new(ParentWin),
    wxScrolledWindow:enableScrolling(Panel,true,true),
    wxScrolledWindow:setScrollbars(Panel,1,1,0,0),
    Sizer  = wxBoxSizer:new(?wxHORIZONTAL),
    Window = apply(Fun, [Panel | FunArgs]),
    wxSizer:add(Sizer, Window, [{flag, ?wxEXPAND bor ?wxALL},
				{proportion, 1},
				{border, 5}]),
    wxPanel:setSizer(Panel, Sizer),
    Panel.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Callbacks%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(#wx{event=#wxClose{type=close_window}}, State) ->
    {stop, normal, State};

handle_event(#wx{id=?wxID_CLOSE, event=#wxCommand{type=command_menu_selected}},
	     State) ->
    {stop, normal, State};

handle_event(Event, _State) ->
    error({unhandled_event, Event}).

handle_info(_Info, State) ->
    %% io:format("~p: ~p, Handle info: ~tp~n", [?MODULE, ?LINE, _Info]),
    {noreply, State}.

handle_call(Call, From, _State) ->
    error({unhandled_call, Call, From}).

handle_cast(Cast, _State) ->
    error({unhandled_cast, Cast}).

terminate(_Reason, #state{parent=Parent,id=Id,frame=Frame}) ->
    wx_object:cast(Parent,{detail_win_closed, Id}),
    case Frame of
	undefined ->  ok;
	_ -> wxFrame:destroy(Frame)
    end,
    ok.

code_change(_, _, State) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_menus(MenuBar) ->
    Menus = [{"File", [#create_menu{id=?wxID_CLOSE, text="Close"}]}],
    observer_lib:create_menus(Menus, MenuBar, new_window).
