%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

-module(ex_listCtrl).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

-export([start/1, init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-record(state,
	{
	  parent,
	  config,
	  notebook
	 }).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
        wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
    Parent = proplists:get_value(parent, Config),
    Panel = wxPanel:new(Parent, []),

    %% Setup sizers
    MainSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
				     [{label, "wxListCtrl"}]),

    Notebook = wxNotebook:new(Panel, 1, [{style, ?wxBK_DEFAULT}]),


    ListCtrl1 = wxListCtrl:new(Notebook, [{style, ?wxLC_LIST}]),
    [wxListCtrl:insertItem(ListCtrl1, Int, "Item "++integer_to_list(Int)) ||
	Int <- lists:seq(0,50)],
    ListCtrl2 = create_list_ctrl(Notebook, [{style, ?wxLC_REPORT bor
					     ?wxLC_SINGLE_SEL}]),
    IL = wxImageList:new(16,16),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_COPY", [{size, {16,16}}])),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_MISSING_IMAGE", [{size, {16,16}}])),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_QUESTION", [{size, {16,16}}])),
    wxImageList:add(IL, wxArtProvider:getBitmap("wxART_WARNING", [{size, {16,16}}])),
    wxListCtrl:assignImageList(ListCtrl2, IL, ?wxIMAGE_LIST_SMALL),
    Fun =
	fun(Item) ->
		case Item rem 4 of
		    0 ->
			wxListCtrl:setItemBackgroundColour(ListCtrl2, Item, {240,240,240,255}),
			wxListCtrl:setItemImage(ListCtrl2, Item, 0);
		    1 -> wxListCtrl:setItemImage(ListCtrl2, Item, 1);
		    2 -> wxListCtrl:setItemImage(ListCtrl2, Item, 2),
			 wxListCtrl:setItemBackgroundColour(ListCtrl2, Item, {240,240,240,255});
		    _ -> wxListCtrl:setItemImage(ListCtrl2, Item, 3)
		end
	end,
    wx:foreach(Fun, lists:seq(0,50)),

    ListCtrl3 = create_list_ctrl(Notebook, [{style, ?wxLC_REPORT}]),
    wxListCtrl:setTextColour(ListCtrl3, ?wxBLUE),
    wxListCtrl:setItemBackgroundColour(ListCtrl3,5,?wxRED),
    wxListCtrl:setItemBackgroundColour(ListCtrl3,3,?wxGREEN),
    wxListCtrl:setItemBackgroundColour(ListCtrl3,0,?wxCYAN),

    IA = wxListItemAttr:new(),
    wxListItemAttr:setTextColour(IA, {190, 25, 25}),
    LC4Opts = [{style, ?wxLC_REPORT bor ?wxLC_VIRTUAL},
	       {onGetItemText, fun(_This, Item, 0) ->
				       "Row " ++ integer_to_list(Item);
				  (_, Item, 1) when Item rem 5 == 0 ->
				       "Column 2";
				  (_, _, _) -> ""
			       end},
	       {onGetItemAttr, fun(_This, Item) when Item rem 3 == 0 ->
				       IA;
				  (_This, _Item)  ->
				       wx:typeCast(wx:null(), wxListItemAttr)
			       end},
	       {onGetItemColumnImage, fun(_This, Item, 1) ->
					      Item rem 4;
					 (_, _, _) ->
					      -1
				      end}
	      ],
    ListCtrl4 = wxListCtrl:new(Notebook, LC4Opts),
    wxListCtrl:setImageList(ListCtrl4, IL, ?wxIMAGE_LIST_SMALL),

    wxListCtrl:insertColumn(ListCtrl4, 0, "Column 1"),
    wxListCtrl:insertColumn(ListCtrl4, 1, "Column 2"),
    wxListCtrl:setColumnWidth(ListCtrl4, 0, 200),
    wxListCtrl:setColumnWidth(ListCtrl4, 1, 200),
    wxListCtrl:setItemCount(ListCtrl4, 1000000),


    wxListCtrl:connect(ListCtrl1, command_list_item_selected, []),
    wxListCtrl:connect(ListCtrl2, command_list_item_selected, []),
    wxListCtrl:connect(ListCtrl3, command_list_item_selected, []),
    wxListCtrl:connect(ListCtrl4, command_list_item_selected, []),

    %% Add to sizers
    wxNotebook:addPage(Notebook, ListCtrl1, "List", []),
    wxNotebook:addPage(Notebook, ListCtrl2, "Report", []),
    wxNotebook:addPage(Notebook, ListCtrl3, "Colored multiselect", []),
    wxNotebook:addPage(Notebook, ListCtrl4, "Virtual Report", []),

    wxSizer:add(MainSizer, Notebook, [{proportion, 1},
				      {flag, ?wxEXPAND}]),

    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config,
		   notebook = Notebook}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{obj = _ListCtrl,
		 event = #wxList{itemIndex = Item}},
	     State = #state{}) ->
    demo:format(State#state.config,"Item ~p selected.\n",[Item]),
    {noreply,State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p\n",[Msg]),
    {noreply,State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    demo:format(State#state.config,"Got Call ~p\n",[Msg]),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.


code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(FIRST_COL, 0).
-define(SECOND_COL, 1).
-define(THIRD_COL, 2).

create_list_ctrl(Win, Options) ->
    ListCtrl = wxListCtrl:new(Win, Options),
    wxListCtrl:insertColumn(ListCtrl, ?FIRST_COL, "First Col", []),
    wxListCtrl:insertColumn(ListCtrl, ?SECOND_COL, "Second Col", []),
    wxListCtrl:insertColumn(ListCtrl, ?THIRD_COL, "Third Col", []),
    Fun =
	fun(Int) ->
		Name = integer_to_list(Int),
		wxListCtrl:insertItem(ListCtrl, Int, ""),
		wxListCtrl:setItem(ListCtrl, Int, ?FIRST_COL, "First "++Name),
		wxListCtrl:setItem(ListCtrl, Int, ?SECOND_COL, "Second "++Name),
		wxListCtrl:setItem(ListCtrl, Int, ?THIRD_COL, "Third "++Name)
	end,
    wx:foreach(Fun, lists:seq(0,50)),

    ListCtrl.


