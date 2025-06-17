%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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

%% This is example of the widgets and usage of Wx.
%% Hopefully it will contain all implemented widgets, it's event handling
%% and some tutorials of how to use sizers and other stuff.

-module(ex_webview).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).
-export([start/1, init/1,
	 terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-record(state, {buttons, config, parent, webview, texts, timer}).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
    wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
    Parent = wxScrolledWindow:new(proplists:get_value(parent, Config)),
    wxScrolledWindow:setScrollRate(Parent, 5, 5),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),

    os:type() == {win32,nt} andalso wx_misc:mSWSetEmulationLevel(?wxWEBVIEWIE_EMU_IE11),
    try wxWebView:new(Parent, ?wxID_ANY, [{url, "https://www.erlang.org/"}]) of
    WebView ->
        {ok, Timer} = timer:send_interval(100, update_passive_boxes),
        Events = [webview_navigating, webview_navigated, webview_loaded, webview_error,
            webview_newwindow, webview_title_changed],
        [wxWebView:connect(WebView, Event) || Event <- Events],

        {Buttons, Texts} = create_buttons(Parent, WebView),
        wxSizer:add(Sizer, Buttons, [{flag, ?wxEXPAND}]),
        wxSizer:add(Sizer, WebView, [{proportion, 1}, {flag, ?wxEXPAND}]),
        wxFrame:setSizer(Parent, Sizer),
        {Parent, #state{
            buttons=Buttons,
            config=Config,
            parent=Parent,
            webview=WebView, 
            texts=Texts,
            timer=Timer
        }}
    catch
    error:{undefined_function,_} ->
        Sorry = wxStaticText:new(Parent, ?wxID_ANY, "wxWebView Support is missing in this installation.", []),
        wxSizer:add(Sizer, Sorry, [{proportion, 1}, {flag, ?wxEXPAND}]),
        {Parent, #state{
            config=Config,
            parent=Parent,
            webview=Sorry
        }}
    end.

create_buttons(Parent, WebView) ->
    Buttons = wxScrolledWindow:new(Parent, [{size, {350, 400}}]),
    wxScrolledWindow:enableScrolling(Buttons, false, true),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxScrolledWindow:setSizer(Buttons, Sizer),

    create_label(Buttons, "Navigation"),
    Text = wxTextCtrl:new(Buttons, ?wxID_ANY, [{value, wxWebView:getCurrentURL(WebView)}]),
    wxSizer:add(Sizer, Text, [{flag, ?wxEXPAND}]),
    create_pair(Buttons,    
        fun (P) -> create_button(P, "getCurrentURL()") end,
        fun (P) -> create_button(P, "loadURL()") end),
    wxSizer:addSpacer(Sizer, 5),

    create_label(Buttons, "Search"),
    Search = create_flex_panel(Buttons, 2, 0),
    Find = wxTextCtrl:new(Search, ?wxID_ANY, [{value, "search term"}]),
    wxSizer:add(wxFrame:getSizer(Search), Find, [{flag, ?wxEXPAND}]),
    create_button(Search, "find()"),

    create_label(Buttons, "Other Actions"),
    Panel = create_flex_panel(Buttons, 3),
    [create_button(Panel, Label) || Label <- [
        "goBack()", "goForward()", "clearHistory()",
        "undo()", "redo()", "selectAll()",
        "reload()", "stop()", "print()",
        "copy()", "paste()", "cut()"
    ]],
    Panel2 = create_flex_panel(Buttons, 2),
    [create_button(Panel2, Label) || Label <- [
        "reload(NO_CACHE)",
        "setEditable()","setPage()",
        "setZoom()","setZoomType(TEXT)",
        "deleteSelection()", "setZoomType(LAYOUT)",
        "clearSelection()", "enableContextMenu()",
        "enableHistory()","setZoomFactor(1)"
    ]],
    wxSizer:addSpacer(Sizer, 5),

    create_label(Buttons, "Console output only"),
    PrintPanel = create_flex_panel(Buttons, 2),
    Names = [
        "getCurrentTitle()","getPageSource()",
        "getPageText()","getSelectedSource()",
        "getZoom()","getZoomType()","getZoomFactor()",
        "getSelectedText()","runScript()"
    ],
    [create_button(PrintPanel, Label) || Label <- Names],
    wxSizer:addSpacer(Sizer, 5),

    CtrlPanel = create_flex_panel(Buttons, 2),
    Controls = lists:foldl(fun (Label, Map) ->
        maps:put(Label, create_passive_checkbox(CtrlPanel, Label), Map)
    end, #{url => Text, find => Find}, passive_boxes()),

    wxSizer:layout(Sizer),
    wxScrolledWindow:fitInside(Buttons),
    wxScrolledWindow:setScrollRate(Buttons, 5, 15),
    {Buttons, Controls}.

create_label(Parent, Label) ->
    Text = wxStaticText:new(Parent, ?wxID_ANY, Label),
    wxSizer:add(wxFrame:getSizer(Parent), Text),
    Text.

create_flex_panel(Parent, Cols) -> 
    create_flex_panel(Parent, Cols, Cols-1).
create_flex_panel(Parent, Cols, GrowCol) ->
    Panel = wxPanel:new(Parent),
    Flex = wxFlexGridSizer:new(Cols),
    wxPanel:setSizer(Panel, Flex),
    wxFlexGridSizer:addGrowableCol(Flex, GrowCol),
    wxSizer:add(wxFrame:getSizer(Parent), Panel, [{flag, ?wxEXPAND}]),
    Panel.

create_button(Parent, Label) ->
    Sizer = wxFrame:getSizer(Parent),
    Button = wxButton:new(Parent, ?wxID_ANY, [{label, Label}]),
    wxButton:connect(Button, command_button_clicked),
    wxSizer:add(Sizer, Button, [{flag, ?wxEXPAND}]).

create_pair(Parent, P1, P2) ->
    Panel = wxPanel:new(Parent),
    wxPanel:setSizer(Panel, wxBoxSizer:new(?wxHORIZONTAL)),
    wxSizerItem:setProportion(P1(Panel), 1),
    wxSizerItem:setProportion(P2(Panel), 1),
    wxSizer:add(wxFrame:getSizer(Parent), Panel, [{flag, ?wxEXPAND}]).

create_passive_checkbox(Parent, Label) ->
    Sizer = wxFrame:getSizer(Parent),
    Checkbox = wxCheckBox:new(Parent, ?wxID_ANY, Label),
    wxCheckBox:disable(Checkbox),
    wxSizer:add(Sizer, Checkbox, [{flag, ?wxEXPAND}]),
    Checkbox.

button("getCurrentURL()", #state{webview=WebView, texts=#{url := Text}}) ->
    wxTextCtrl:setValue(Text, wxWebView:getCurrentURL(WebView));
button("loadURL()", #state{webview=WebView, texts=#{url := Text}}) ->
    wxWebView:loadURL(WebView, wxTextCtrl:getValue(Text));
button("find()", #state{webview=WebView, texts=#{find := Text}}) ->
    wxWebView:find(WebView, wxTextCtrl:getValue(Text));
button("enableContextMenu()", #state{webview=WebView}) ->
    Context = not wxWebView:isContextMenuEnabled(WebView),
    wxWebView:enableContextMenu(WebView, [{enable, Context}]);
button("setEditable()", #state{webview=WebView}) ->
    Context = not wxWebView:isEditable(WebView),
    wxWebView:setEditable(WebView, [{enable, Context}]);
button("enableHistory()", #state{webview=WebView}) ->
    History = case get(history) of
        undefined -> true;
        true -> true;
        false -> false
    end,
    put(history, not History),
    wxWebView:enableHistory(WebView, [{enable, History}]);
button("reload(NO_CACHE)", #state{webview=WebView}) ->
    wxWebView:reload(WebView, [{flags, ?wxWEBVIEW_RELOAD_NO_CACHE}]);
button("setPage()", #state{webview=WebView}) ->
    wxWebView:setPage(WebView, "<html><body>Hello World</body></html>", "");
button("setZoom()", #state{webview=WebView}) ->
    Zoom = case get(zoom) of
        undefined -> ?wxWEBVIEW_ZOOM_TINY;
        ?wxWEBVIEW_ZOOM_TINY -> ?wxWEBVIEW_ZOOM_SMALL;
        ?wxWEBVIEW_ZOOM_SMALL -> ?wxWEBVIEW_ZOOM_MEDIUM;
        ?wxWEBVIEW_ZOOM_MEDIUM -> ?wxWEBVIEW_ZOOM_LARGE;
        ?wxWEBVIEW_ZOOM_LARGE -> ?wxWEBVIEW_ZOOM_LARGEST;
        ?wxWEBVIEW_ZOOM_LARGEST -> ?wxWEBVIEW_ZOOM_TINY
    end,
    put(zoom, Zoom),
    wxWebView:setZoom(WebView, Zoom);
button("setZoomType(TEXT)", #state{webview=WebView}) ->
    wxWebView:setZoomType(WebView, ?wxWEBVIEW_ZOOM_TYPE_TEXT);
button("setZoomType(LAYOUT)", #state{webview=WebView}) ->
    wxWebView:setZoomType(WebView, ?wxWEBVIEW_ZOOM_TYPE_LAYOUT);
button("setZoomFactor(1)", #state{webview=WebView}) ->
    wxWebView:setZoomFactor(WebView, 1);
button("runScript()", #state{webview=WebView}) ->
    wxWebView:runScript(WebView, "alert('This is from Javascript!');");
button(Other, #state{webview=WebView}) ->
    Singles = ["goBack()", "goForward()", "print()","redo()","undo()",
    "stop()","reload()","paste()","copy()","cut()","deleteSelection()",
    "clearSelection()","clearHistory()", "selectAll()", 
    "getCurrentTitle()", "getPageSource()", "getPageText()", 
    "getSelectedSource()", "getSelectedText()", "getZoom()", "getZoomType()",
    "getZoomFactor()"],
    case lists:member(Other, Singles) of
        true ->
            Call = string:left(Other, string:len(Other) - 2),
            Atom = list_to_atom(Call),
            apply(wxWebView, Atom, [WebView]);

        false -> io:format("Unhandled button press: ~s ~n",[Other])
    end.

passive_boxes() ->
    ["canGoForward()", "canCut()", "canGoBack()", "canCopy()", 
        "canRedo()", "canPaste()", "canUndo()", "isBusy()",
        "isContextMenuEnabled()", "isEditable()", "canSetZoomType(TEXT)",
        "hasSelection()", "canSetZoomType(LAYOUT)"].

update_passive_boxes([], _) ->
    ok;
update_passive_boxes(["canSetZoomType(TEXT)" = Label | Rest], State) ->
    #state{webview=WebView, texts=#{Label := Checkbox}} = State,
    Value = wxWebView:canSetZoomType(WebView, ?wxWEBVIEW_ZOOM_TYPE_TEXT),
    wxCheckBox:setValue(Checkbox, Value),
    update_passive_boxes(Rest, State);
update_passive_boxes(["canSetZoomType(LAYOUT)" = Label | Rest], State) ->
    #state{webview=WebView, texts=#{Label := Checkbox}} = State,
    Value = wxWebView:canSetZoomType(WebView, ?wxWEBVIEW_ZOOM_TYPE_LAYOUT),
    wxCheckBox:setValue(Checkbox, Value),
    update_passive_boxes(Rest, State);
update_passive_boxes([Label | Rest], State) ->
    #state{webview=WebView, texts=#{Label := Checkbox}} = State,
    Call = string:left(Label, string:len(Label) - 2),
    Atom = list_to_atom(Call),
    wxCheckBox:setValue(Checkbox, apply(wxWebView, Atom, [WebView])),
    update_passive_boxes(Rest, State).

%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{obj=Button, event=#wxCommand{type=command_button_clicked}}, State) ->
    Label = wxButton:getLabel(Button),
    Ret = try button(Label, State)
    catch
        error:{undefined_function,_} -> "Function only supported starting wxWidgets 3.1"
    end,
    demo:format(State#state.config,"Got button press: ~s => ~p~n",[Label, Ret]),
    {noreply, State};

handle_event(#wx{event=Ev=#wxWebView{}}, State) ->
    demo:format(State#state.config,"WebView Event ~180p~n",[Ev]),
    {noreply, State};

handle_event(Ev = #wx{}, State = #state{}) ->
    demo:format(State#state.config,"Got Event ~p~n",[Ev]),
    {noreply,State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(update_passive_boxes, State) ->
    update_passive_boxes(passive_boxes(), State),
    {noreply,State};

handle_info({'_wxe_error_' , _Code, What, undefined_function}, State) ->
    demo:format(State#state.config, "Function is only supported starting wxWidgets 3.1 (~p)~n",[What]),
    {noreply,State};

handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_call(shutdown, _From, State=#state{parent=Panel, timer=Timer}) ->
    timer:cancel(Timer),
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    demo:format(State#state.config,"Got Call ~p~n",[Msg]),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    demo:format(State#state.config,"Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _) ->
    ok.

