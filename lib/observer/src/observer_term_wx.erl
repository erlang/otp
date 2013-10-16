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

-module(observer_term_wx).

-behaviour(wx_object).

-export([start/2]).

-export([init/1, handle_event/2, handle_cast/2, terminate/2, code_change/3,
	 handle_call/3, handle_info/2]).

-include_lib("wx/include/wx.hrl").
-include("observer_defs.hrl").

-define(REFRESH, 601).

-record(state, {parent,
		frame,
		id
	       }).

start(Id, ParentFrame) ->
    wx_object:start_link(?MODULE, [Id, ParentFrame, self()], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([{T,Key}=Id, ParentFrame, Parent]) ->
    case ets:lookup(T,Key) of
	[{Key,Term}] ->
	    init(Id, ParentFrame, Parent, Term);
	[] ->
	    observer_lib:display_info_dialog(
	      "The term does no longer exist.\n"
	      "Please refresh the process window!"),
	    {stop, normal}
    end.



init(Id, ParentFrame, Parent, Term) ->
    Frame=wxFrame:new(ParentFrame, ?wxID_ANY, ["Expanded Term"],
		      [{style, ?wxDEFAULT_FRAME_STYLE}, {size, {850,600}}]),
    MenuBar = wxMenuBar:new(),
    Menus = [{"File", [#create_menu{id=?wxID_CLOSE, text="Close"}]}],
    observer_lib:create_menus(Menus, MenuBar, new_window),
    wxFrame:setMenuBar(Frame, MenuBar),
    Panel  = wxPanel:new(Frame),
    Sizer  = wxBoxSizer:new(?wxHORIZONTAL),

    Window = observer_lib:html_window(Panel),
    Html = crashdump_viewer_html:plain_page(io_lib:format("~p~n",[Term])),
    wxHtmlWindow:setPage(Window, Html),

    wxSizer:add(Sizer, Window, [{flag, ?wxEXPAND bor ?wxALL},
					{proportion, 1},
				{border, 5}]),
    wxPanel:setSizer(Panel, Sizer),
    wxFrame:show(Frame),
    {Frame, #state{parent=Parent,
		   frame=Frame,
		   id=Id
		  }}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Callbacks%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(#wx{event=#wxClose{type=close_window}}, State) ->
    {stop, normal, State};

handle_event(#wx{id=?wxID_CLOSE, event=#wxCommand{type=command_menu_selected}}, State) ->
    {stop, normal, State};

handle_event(#wx{event=#wxHtmlLink{linkInfo=#wxHtmlLinkInfo{href=Info}}}, State) ->
    observer ! {open_link, Info},
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

terminate(_Reason, #state{parent=Parent,id=Id,frame=Frame}) ->
    Parent ! {expand_win_closed, Id},
    case Frame of
	undefined ->  ok;
	_ -> wxFrame:destroy(Frame)
    end,
    ok.

code_change(_, _, State) ->
    {ok, State}.
