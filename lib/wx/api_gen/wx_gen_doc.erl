%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2020-2025. All Rights Reserved.
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
%% Api wrapper generator

-module(wx_gen_doc).
-export([module_doc/1]).

-compile([export_all, nowarn_export_all]).

-include("wx_gen.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-import(gen_util, [lowercase/1, lowercase_all/1, uppercase/1,
		   open_write/1, close/0, erl_copyright/0, w/2,
		   args/3, args/4]).

-import(wx_gen, [drop_empty/1]).

-define(LOG(F,A),
        fun() -> CurrF = case get(current_func) of undefined -> ?LINE; ElSe -> ElSe end,
                 io:format("~w:~s:~p: " ++ F, [?LINE,get(current_class), CurrF|A])
        end()).

-define(DBGCF(Class, Func, Format, Args),
	case {get(current_class), get(current_func)} of
	    {Class, Func} -> ?LOG(Format, Args);
	    _ -> ok
	end).

module_doc(#class{name=Name,parent=Parent,event=Evs}) ->
    w(~s'-moduledoc """\n', []),
    %% io:format("~w: Gen ~p~n", [?LINE, Name]),
    class_brief(Name),
    class_description(Name, Parent, Evs),
    w(~s'""".\n', []),
    ok.

gen_header(Name) ->
    Legal = "Licensed under the wxWindows Free Documentation Licence, Version 3",
    {Year, _, _} = date(),

    [nl(2), {copyright,
             [nl(4), {year, ["2020"]}, {year,[integer_to_list(Year)]},
              nl(4), {holder, ["wxWidgets team."]}]},
     nl(2), {legalnotice, [Legal, nl(2)]},
     nl(2), {title, [Name]},
     nl(0)].

class_brief(Name) ->
    [{_,Doc}] = ets:lookup(docs, Name),
    Brief = case fsummary(Doc) of
                [] -> ["Functions for " ++ Name ++ " class"];
                Docs -> [to_text(Docs)]
            end,
    try w("~s\n\n", [unicode:characters_to_binary(Brief)])
    catch Type:Err:ST ->
            io:format("Error ~p~n ~p~n ~p~n", [Err, Brief, fsummary(Doc)]),
            erlang:raise(Type,Err,ST)
    end.

class_description(Name, Parent, Evs) ->
    [{_,Doc}] = ets:lookup(docs, Name),
    D0 = doc(detailed, Doc),
    D1 = flatten_p(D0),
    D2 = remove_doxy_link(D1),
    {Events, D3} = make_events(D2, Evs),
    %% ?DBGCF("wxCheckBox", undefined, "~p~n", [p(D2)]),
    Docs = translate(D3),
    Parents = wx_gen_erl:parents(Parent),
    MRef = fun(M) -> {url, "`m:" ++ M ++ "`", none} end,
    PRef = case [MRef(P) || P <- Parents, P =/= root, P =/= object] of
               [] -> [];
               Ps ->
                   Derived = {p, "This class is derived, and can use functions, from: "},
                   Items = [{list_item, [P]} || P <- Ps],
                   [Derived, {list, Items}]
           end,

    Url = "https://docs.wxwidgets.org/3.2/class" ++
        camelcase_to_underscore(Name) ++ ".html",
    WxRef = {p, [{text, "wxWidgets docs: "}, {url, Url, Name}]},
    try
        Everything = Docs ++ PRef ++ [WxRef|Events],
        Desc = to_text(Everything),
        %% ?DBGCF("wxListView", undefined, "~p~n", [Docs]),
        w("~s\n", [unicode:characters_to_binary(Desc)]),
        ok
    catch Type:Err:ST ->
            io:format("Error ~p~n ~p~n ~p~n", [Err, Name, fsummary(Doc)]),
            erlang:raise(Type,Err,ST)
    end.

func(Ms) ->
    try
        Xml = gen_func(Ms, []),
        case Xml of
            [] ->
                ignore;
            _ ->
                %% ?DBGCF("wxAuiManager", "InsertPane", "~p~n", [Xml]),
                FuncDoc = to_text(Xml),
                NoQuotes = string:find(FuncDoc, "\"") == nomatch,
                case is_single_line(FuncDoc) andalso NoQuotes of
                    true ->
                        w(~s'-doc "~s".\n', [unicode:characters_to_binary(FuncDoc)]);
                    false ->
                        w(~s'-doc """\n', []),
                        FuncDoc = to_text(Xml),
                        w("~s", [unicode:characters_to_binary(FuncDoc)]),
                        w(~s'\n""".\n', [])
                end,
                ok
        end
    catch Err:Reason:St ->
            io:format("Err ~p: ~P~n ~P~n",[Err, Reason, 20, St, 30]),
            exit(gen_doc)
    end.

gen_func(Ms, Acc0) ->
    Last = length(Ms),
    {_, Fs} = lists:foldl(
                fun(M, {N,Acc}) ->
                        {N+1, [gen_func_1(M,N =/= Last,N)|Acc]}
                end, {1,[]}, Ms),
    [lists:append(lists:reverse(Fs)) | Acc0].

gen_func_1(#method{name=N,alias=A,params=Ps,where=erl_no_opt,method_type=MT}, SkipDesc, Clause) ->
    put(current_func, N),
    Name = erl_func_name(N,A,MT),
    As = wx_gen_erl:erl_args_count(Ps, erl_no_opt),
    Impl = io_lib:format("~s/~w",[Name,As+1]),
    %% ?DBGCF("wxBitmap", "Create", "~p~n", [Ps]),
    Desc = case SkipDesc of
               true -> [];
               false -> [{text, "Equivalent to: "}, {c, [Impl]}]
           end,
    Desc;
gen_func_1(#method{name=N,alias=A,params=Ps,where=erl_alias,method_type=MT}, SkipDesc, Clause) ->
    put(current_func, N),
    As = wx_gen_erl:erl_args_count(Ps, erl_alias),
    Impl = io_lib:format("~s/~w",[wx_gen_erl:erl_func_name(N,undefined),As]),
    %%    ?DBGCF("wxBitmap", "Create", "~p~n", [Ps]),
    Desc = case SkipDesc of
               true  -> [];
               false -> [{text, "Equivalent to: "}, {c, [Impl]}]
           end,
    Desc;
gen_func_1(#method{name=N,id=Id,alias=A,params=Ps,method_type=MT}, SkipDesc, Clause) ->
    put(current_func, N),
    Name = erl_func_name(N,A,MT),
    As = wx_gen_erl:erl_args_count(Ps, erl_alias),
    Docs = case (not SkipDesc) andalso ets:lookup(docs, Id) of
               false ->
                   %% ?DBGCF("wxBitmap", "Create", "~p~n", [Ps]),
                   [];
               [] when Name =:= "destroy" ->
                   [
                    [{text, "Destroys the object."}]
                   ];
               [] ->
                   ?LOG(" /~w (~p ~p) is missing docs~n",[As, SkipDesc, Clause]),
                   [];
               [{_,Doc}] ->
                   case {fsummary(Doc), desc(Doc)} of
                       {[], Desc} ->
                           Desc;
                       {Desc, []} ->
                           Desc;
                       {Sum, Desc} ->
                           [Sum, nl(0), Desc]
                   end
           end,
    Docs.
%% Remove paragraph

fsummary(Docs) ->
    Doc = doc(brief, Docs),
    case flatten_p(Doc) of
        [] ->
            %% Det = doc(detailed, Doc);
            [];
        Head ->
            %% ?DBGCF("wxBitmap", "wxBitmap", "'~p'~n",[Doc]),
            %% ?DBGCF("wxHtmlWindow", "GetOpenedPageTitle", "~p~n", [Docs]),
            [{p, [{text_nocut, to_text(translate(fsummary_1(Head)))}]}]
    end.

desc(Doc) ->
    Docs = doc(detailed, Doc),
    %% ?DBGCF("wx_misc", "wxNewId", "~100p~n", [p(Docs)]),
    Flat = flatten_p(Docs),
    %% ?DBGCF("wxArtProvider", "GetBitmap", "~100p~n", [p(Flat)]),
    Clean = remove_doxy_link(Flat),
    Res = translate(Clean),
    %% ?DBGCF("wxArtProvider", "GetBitmap", "~100p~n", [p(Res)]),
    Res.

%%%%%%%%%%%%%%

make_events(D1, Evs) ->
    make_events(D1, Evs, []).

make_events([#xmlElement{name=heading}=E|Es], Evs, Acc) ->
    %% ?DBGCF("wxTopLevelWindow", undefined, "~100p~n", [p(E)]),
    case is_event_heading(E) of
        false ->
            make_events(Es, Evs, [E|Acc]);
        true ->
            {Events, Rest} = make_events_sect(Es, Evs),
            {Events, lists:reverse(Acc, Rest)}
    end;
make_events([#xmlElement{name=sect1, content=[Title|Cs]}=E|Es], Evs, Acc) ->
    %% ?DBGCF("wxTopLevelWindow", undefined, "~100p~n", [p(Title)]),
    case is_event_heading(Title) of
        false ->
            make_events(Es, Evs, [E|Acc]);
        true ->
            {Events, Rest} = make_events_sect(Cs++Es, Evs),
            {Events, lists:reverse(Acc, Rest)}
    end;
make_events([E|Es], Evs, Acc) ->
    make_events(Es, Evs, [E|Acc]);
make_events([], _, Acc) ->
    {[], lists:reverse(Acc)}.

is_event_heading(#xmlElement{name=Name, content=Cs})
  when Name =:= title; Name =:= heading ->
    case get_text(Cs) of
        %% "Default event" ++ _ -> true;
        "Events" ++ _ -> true;
        _ -> false
    end;
is_event_heading(_) ->
    false.

make_events_sect(Cs, false) ->
    %% ?DBGCF("wxTopLevelWindow", undefined, "~tp~n",[p(Cs)]),
    {Evs, Rest} = get_event_list(Cs),
    Refs = [{list_item, [Ev]} || Item <- Evs, Ev <- get_event_class(Item), Ev =/= ignore],
    case Refs of
        [] -> {[], Rest};
        _ ->
            EventDoc = [{p, ["Event types emitted from this class: "]}|
                        [{list, Refs}]],
            {[{title, ["Events"]}|EventDoc], Rest}
    end;
make_events_sect(Cs, [_|_]) ->
    {_Refs, Rest} = get_event_list(Cs),
    EvtMarker = "wxEvtHandler#connect/3",
    EvtFunc = "wxEvtHandler:connect/3",
    EvtHRef = #xmlElement{name=seemfa, attributes=[{marker,EvtMarker}],
                          content=[{c, [{text, EvtFunc}]}]},
    EvType = get(current_class) ++ "Type",
    TypeRef = #xmlElement{name=seetype, attributes=[{marker,"#" ++ EvType}],
                          content=[{c, [{text, EvType}]}]},
    EventDoc = [{p, ["Use ", EvtHRef, " with ", TypeRef,
                     " to subscribe to events of this type."]}],
    EventSect = [{title, ["Events"]}|EventDoc],
    case get(current_class) of
        "wxStyledTextEvent" -> %% Broken xml
            {EventSect, []};
        _ ->
            {EventSect, Rest}
    end.

get_event_list([#xmlElement{name=para, content=Cs}|Rest]) ->
    case get_event_list(Cs) of
        false ->
            get_event_list(Rest);
        {Res,Cont} ->
            {Res, Cont ++ Rest}
    end;
get_event_list([#xmlElement{name=itemizedlist, content=Evs}|R]) ->
    {Evs,R};
get_event_list([_|R]) ->
    get_event_list(R);
get_event_list([]) ->
    false.

get_event_class(#xmlText{}) -> [];
get_event_class(#xmlElement{name=listitem, content=[#xmlElement{name=para, content=Cs}]}) ->
    [#xmlText{value = "EVT_" ++ EventMacro}|_] = Cs,
    [WxName|_R] = string:split(EventMacro, "("),
    Map = get(ev2class),
    EvType0 = string:lowercase(WxName),
    case maps:get(EvType0, Map, undefined) of
        undefined ->
            EvType = event_name(WxName),
            case maps:get(EvType, Map, undefined) of
                undefined ->
                    case check_missing_event(WxName) of
                        ok -> [ignore];
                        "JOY_*" -> make_event_refs("wxJoystickEvent");
                        "MOUSE_*" -> make_event_refs("wxMouseEvent");
                        "SCROLLWIN_*" -> make_event_refs("wxScrollWinEvent");
                        _ ->
                            ?LOG("Missing event: ~s ~s .. ~s ~n", [EvType, WxName, _R]),
                            [ignore]
                    end;
                Class ->
                    %% [#xmlElement{name=seeerl, attributes=[{marker,Class}], content=[{c, [EvType]}]}]
                    [{url, "`m:"++Class ++ "`", "`" ++ EvType ++ "`"}]
            end;
        Class ->
            %% [#xmlElement{name=seeerl, attributes=[{marker,Class}], content=[{c, [EvType0]}]}]
            [{url,  "`m:"++Class ++ "`", "`" ++ EvType0 ++ "`"}]
    end;
get_event_class(#xmlElement{name=Name, content=Cs}) ->
    io:format("~w: ~P~n",[Name, p(Cs), 20]).


make_event_refs(Class) ->
    #class{event=Evs} = get({class, Class}),
    Fun = fun(Ev) ->
                  EvType = wx_gen_erl:event_type_name(Ev),
                  %% #xmlElement{name=seeerl, attributes=[{marker,Class}],content=[{c, [EvType]}]}
                  [{url,  "`m:"++Class ++ "`", "`" ++ EvType ++ "`"}]
          end,
    [Fun(Ev) || Ev <- Evs].

event_name(Ev = "AUINOTEBOOK_" ++ _) ->
    string:lowercase("COMMAND_" ++ Ev);
event_name("BUTTON") ->
    string:lowercase("COMMAND_BUTTON_CLICKED");
event_name("CHECKBOX") ->
    string:lowercase("COMMAND_CHECKBOX_CLICKED");
event_name("CHECKLISTBOX") ->
    string:lowercase("COMMAND_CHECKLISTBOX_TOGGLED");
event_name("CHOICE") ->
    string:lowercase("COMMAND_CHOICE_SELECTED");
event_name("CLOSE") ->
    string:lowercase("CLOSE_WINDOW");
event_name("COMBOBOX") ->
    string:lowercase("COMMAND_COMBOBOX_SELECTED");
event_name(Ev = "COLOURPICKER_" ++ _) ->
    string:lowercase("COMMAND_" ++ Ev);
event_name(Ev = "DIRPICKER_" ++ _) ->
    string:lowercase("COMMAND_" ++ Ev);
event_name(Ev = "FILEPICKER_" ++ _) ->
    string:lowercase("COMMAND_" ++ Ev);
event_name(Ev = "FONTPICKER_" ++ _) ->
    string:lowercase("COMMAND_" ++ Ev);
event_name(Ev = "HTML_" ++ _) ->
    string:lowercase("COMMAND_" ++ Ev);
event_name("LISTBOX") ->
    string:lowercase("COMMAND_LISTBOX_SELECTED");
event_name("LISTBOX_DCLICK") ->
    string:lowercase("COMMAND_LISTBOX_DOUBLECLICKED");
event_name(Ev = "LIST_" ++ _) ->
    string:lowercase("COMMAND_" ++ Ev);
%% event_name("NOTEBOOK_PAGE_" ++ Ev) ->
%%     string:lowercase("BOOKCTRL_PAGE_" ++ Ev);
event_name(Ev = "NOTEBOOK_" ++ _) ->
    string:lowercase("COMMAND_" ++ Ev);
event_name(Ev = "SPLITTER_" ++ _) ->
    string:lowercase("COMMAND_" ++ Ev);
event_name(Ev = "SPINCTRL") ->
    string:lowercase("COMMAND_" ++ Ev ++ "_UPDATED");
event_name(Ev = "SLIDER") ->
    string:lowercase("COMMAND_" ++ Ev ++ "_UPDATED");
event_name(Ev = "TEXT_" ++ _) ->
    string:lowercase("COMMAND_" ++ Ev);
event_name("TOGGLEBUTTON") ->
    string:lowercase("COMMAND_TOGGLEBUTTON_CLICKED");
event_name(Ev = "TOOL_" ++ _) ->
    string:lowercase("COMMAND_" ++ Ev);
%% event_name("TREEBOOK_" ++ Ev) ->
%%     string:lowercase("BOOKCTRL_" ++ Ev);
%% event_name("TOOLBOOK_" ++ Ev) ->
%%     string:lowercase("BOOKCTRL_" ++ Ev);
%% event_name("LISTBOOK_" ++ Ev) ->
%%     string:lowercase("BOOKCTRL_" ++ Ev);
event_name("RADIOBOX") ->
    string:lowercase("COMMAND_RADIOBOX_SELECTED");
event_name("RADIOBUTTON") ->
    string:lowercase("COMMAND_RADIOBUTTON_SELECTED");
event_name(Ev = "TREE_" ++ _) ->
    string:lowercase("COMMAND_" ++ Ev);
event_name("TEXT") ->
    string:lowercase("COMMAND_TEXT_UPDATED");
event_name("COMMAND_SCROLL_" ++ Ev) ->
    string:lowercase("SCROLL_" ++ Ev);
event_name(Ev) ->
    string:lowercase(Ev).

%% Alias and groups of events
check_missing_event("MENU") -> ok;
check_missing_event("TOOL") -> ok;
check_missing_event("CALENDAR" ++ _) -> ok;
check_missing_event("COLOURPICKER_CURRENT_CHANGED") -> ok;  %% Only windows ?
check_missing_event("COLOURPICKER_DIALOG_CANCELLED") -> ok;  %% Only windows ?
check_missing_event("TREEBOOK_NODE_" ++ _) -> ok; %% New stuff
check_missing_event("COMMAND_SCROLL") -> ok;
check_missing_event("SCROLL") -> ok;
check_missing_event("MOVE_" ++ _) -> ok;  %% Win only
check_missing_event("TASKBAR_CLICK") -> ok;
check_missing_event("SPLITTER_DCLICK") -> ok;
check_missing_event("SCROLLWIN") -> ok;
check_missing_event("LIST_ITEM_CHECKED") -> ok;
check_missing_event("LIST_ITEM_UNCHECKED") -> ok;
check_missing_event("MENU_HIGHLIGHT_ALL") -> ok;
check_missing_event("POWER" ++ _) -> ok;
check_missing_event("QUERY_LAYOUT_INFO") -> ok;
check_missing_event("CALCULATE_LAYOUT") -> ok;
check_missing_event(Ev) ->
    case string:find(Ev, "RANGE") of
        nomatch -> Ev;
        "RANGE" -> ok
    end.

%%%%%%%%

remove_doxy_link([#xmlElement{name=para, content=Cs0}=E|Es]) ->
    case remove_doxy_link(Cs0) of
        [] -> remove_doxy_link(Es);
        Cs -> [E#xmlElement{content=Cs}|remove_doxy_link(Es)]
    end;
remove_doxy_link([#xmlText{value=_},
                  #xmlElement{name=nonbreakablespace},
                  #xmlElement{name=nonbreakablespace},
                  #xmlText{value="page_libs_" ++ _}|Es]) ->
    lists:dropwhile(fun(#xmlElement{name=para}) -> false; (_) -> true end, Es);
remove_doxy_link([#xmlElement{name=heading, content=[]}|Es]) ->
    remove_doxy_link(Es);
remove_doxy_link([E|Es]) ->
    %% ?DBGCF("wxAuiPaneInfo", undefined, "~p~n",[p(Es)]),
    [E|remove_doxy_link(Es)];
remove_doxy_link([]) ->
    [].


%% tools

xml_func_name(Name, As, Clause) ->
    [{name,Name}, {arity, integer_to_list(As)}, {clause_i, integer_to_list(Clause)}, {since,""}].

nl(Indent) ->
    NL = [$\n, lists:duplicate(Indent, $\s)],
    {text, NL}.

doc(_, undefined) ->
    [];
doc(Key, MapXml) ->
    case maps:get(Key, MapXml, undefined) of
        undefined -> [];
        Xml -> Xml
    end.

translate(Docs) ->
    R = translate(Docs, []),
    R.

translate([Doc|Docs], Acc) ->
    case t(Doc) of
        ignore ->
            translate(Docs, Acc);
        List when is_list(List) ->
            translate(Docs, lists:reverse(List) ++ Acc);
        Xml ->
            translate(Docs, [Xml|Acc])
    end;
translate([], Acc) ->
    lists:reverse(Acc).

t(#xmlText{value = Txt} = Xml) ->
    case is_include([Xml]) of
        true -> ignore;
        false -> {text, Txt}
    end;
t(#xmlElement{name=para, content=Cs}) ->
    Docs = translate(Cs),
    case is_empty(Docs) orelse is_include(Cs) of
        true -> ignore;
        false -> {p, Docs}
    end;
t(#xmlElement{name=simplesect, attributes=As, content=Cs}) ->
    Split = case As of
                [#xmlAttribute{value="example"}] ->
                    {ignore, ignore, []};
                [#xmlAttribute{value="see"}] ->
                    case see_sect(Cs) of
                        [One] ->
                            {combine, "See: ", One};
                        List ->
                            {list, {text, "See: \n"}, List}
                    end;
                [#xmlAttribute{value="deprecated"}] ->
                    [{p, Doc}] = translate(Cs),
                    {combine, "Deprecated: ", Doc};
                [#xmlAttribute{value="return"}] ->
                    [{p, Doc}] = translate(Cs),
                            {combine, "Return: ", Doc};
                [#xmlAttribute{value="remark"}] ->
                    case translate(Cs) of
                        [] ->
                            {ignore, ignore, []};
                        [{p, Doc}] ->
                            {combine, "Remark: ", Doc}
                    end;
                [#xmlAttribute{value="note"}] ->
                    [{p, Doc}] = translate(Cs),
                    {combine, "Note: ", Doc};
                [#xmlAttribute{value="since"}] ->
                    [{p, Doc}] = translate(Cs),
                    {combine, "Since: ", Doc};
                [#xmlAttribute{value=V}] ->
                    %% ?DBGCF("wxBitmap", "SetHeight", "~p~n", [V]),
                    {other, {p, string:titlecase(V) ++ ": "}, translate(Cs)}
            end,
    case Split of
        {_, _, []} -> ignore;
        {list, Intro, Refs} ->
            Items = [{list_item, Ref} || Ref <- Refs],
            [Intro, {list, Items}];
        {other, Intro, Text} ->
            [Intro|Text];
        {combine, Intro, Text} ->
            [{p, [Intro|Text]}]
    end;

t(#xmlElement{name=sect1, content=[#xmlElement{name=title, content=Title}|Desc]}) ->
    case {get(current_class), get_text(Title)} of
        {"wxStyledTextCtrl", "Index of" ++ _} ->
            ignore;
        _ ->
            [{p, [get_text(Title)]} | translate(Desc)]
    end;

t(#xmlElement{name=xrefsect,
              content=[#xmlElement{name=xreftitle, content=Title},
                       #xmlElement{name=xrefdescription, content=Desc}]}) ->
    Intro = case get_text(Title) of
                [] -> [];
                "Todo" -> skip;
                T -> [{text, T ++ ": "}]
            end,
    case Intro of
        skip -> ignore;
        _ ->
            Docs = translate(Intro ++ Desc),
            case is_empty(Docs) of
                true -> ignore;
                false -> Docs
            end
    end;

t(#xmlElement{name=ref}=Ref) ->
    see(Ref);
t(#xmlElement{name=ulink, attributes=[#xmlAttribute{value=Url}], content=Cs}) ->
    {url, Url, get_text(Cs)};
t(#xmlElement{name=onlyfor, content=Cs}) ->
    {p, ["Only for:" | translate(Cs)]};
t(#xmlElement{name=C, content=Txt})
  when C =:= emphasis; C =:= computeroutput; C =:= bold; C =:= verbatim ->
    Trans = translate(Txt),
    case is_list(Trans) andalso is_text(Trans) of
        true when Trans =:= [] -> ignore;
        true -> #xmlElement{name=c, content=Trans};
        false -> Trans
    end;


%% Fixme (lists and tables)
t(#xmlElement{name=parameterlist, content=_C}) ->
    ignore;
t(#xmlElement{name=itemizedlist, content=C}) ->
    {p, [{list, translate(C)}]};
t(#xmlElement{name=orderedlist, content=_C}) ->
    %% {list, translate(C)};
    ignore;
t(#xmlElement{name=table, content=_C}) ->
    ignore;
t(#xmlElement{name=listitem, content=C}) ->
    {list_item, translate(C)};
t(#xmlElement{name=entry, content=_C}) ->
    ignore;

t(#xmlElement{name=linebreak}) ->
    ignore;
t(#xmlElement{name=hruler}) ->
    ignore;

t(#xmlElement{name=ndash, content=[]}) ->
    {text, "-"};
t(#xmlElement{name=mdash, content=[]}) ->
    {text, "-"};

t(#xmlElement{name=heading, content=Cs}) ->
    case Cs == [] orelse "" == get_text(Cs) of
        true -> ignore;
        false -> {title, translate(Cs)}
    end;
t(#xmlElement{name=nonbreakablespace}) ->
    ignore;
t(#xmlElement{name=programlisting}) ->
    ignore;
t(#xmlElement{name=image}) ->
    ignore;
t(#xmlElement{name=native}) ->
    ignore;
t(#xmlElement{name=anchor, content=[]}) ->
    ignore;
t(#xmlElement{name=htmlonly}) ->
    ignore;
t(#xmlElement{name=What, content=Cs}) ->
    ?LOG("xml unhand: ~p~n  ~P~n", [What,p(Cs),15]),
    ignore;
t({url, _, _} = AlreadyTranslated) ->
    AlreadyTranslated;
t({_, _} = AlreadyTranslated) ->
    AlreadyTranslated.

see(#xmlElement{name=ref, attributes=As, content=Cs}) ->
    #xmlAttribute{value=RefType} = lists:keyfind(kindref, #xmlAttribute.name, As),
    see(RefType, As,Cs).

see("member", As, Cs) ->
    #xmlAttribute{value=RefId} = lists:keyfind(refid, #xmlAttribute.name, As),
    case lookup(RefId) of
        {M,F,A0} ->
            A = integer_to_list(A0),
            {Marker,Func} = case get(current_class) of
                                M -> {"#" ++ F ++ "/" ++ A, F ++ "/" ++ A};
                                _ -> {M ++ "#" ++ F ++ "/" ++ A, M ++ ":" ++ F ++ "/" ++ A}
                            end,
            #xmlElement{name=seemfa, attributes=[{marker,Marker}], content=[{c, [{text, Func}]}]};
        enum ->
            EnumP = markdown_qoute(get_text(Cs)),
            [{text, [$?|EnumP]}];
        not_found ->
            Func = get_text(Cs),
            [{c, [Func]}, {text, " (not implemented in wx)"}]
    end;
see("compound", As, _Cs) ->
    #xmlAttribute{value=RefId} = lists:keyfind(refid, #xmlAttribute.name, As),
    case RefId of
        "classwxPoint" -> [{text, "{X,Y}"}];
        "classwxRect" -> [{text, "{X,Y,W,H}"}];
        "classwxSize" -> [{text, "{Width,Height}"}];
        "classwxColor" ->
            #xmlElement{name=seetype, attributes=[{marker,"wx#wx_colour"}], content=[{c, ["wx_color()"]}]};
        "classwxColour" ->
            #xmlElement{name=seetype, attributes=[{marker,"wx#wx_colour"}], content=[{c, ["wx_color()"]}]};
        "classwxDateTime" ->
            #xmlElement{name=seetype, attributes=[{marker,"wx#wx_datetime"}], content=[{c, ["wx_datetime()"]}]};
        "classwxMouseState" ->
            #xmlElement{name=seetype, attributes=[{marker,"wx#wx_wxMouseState"}], content=[{c, ["wx_wxMouseState()"]}]};
        "classwxHtmlLinkInfo" ->
            #xmlElement{name=seetype, attributes=[{marker,"wx#wx_wxHtmlLinkInfo"}], content=[{c, ["wx_wxHtmlLinkInfo()"]}]};
        "class" ++ Class ->
            case get({class, Class}) of
                undefined ->
                    [{c, [Class]}, {text, " (not implemented in wx)"}];
                _ ->
                    %% AppModule = Class,
                    {url, "`m:" ++ Class ++ "`", none}
            end;
        "group__group" ++ _ ->
            ignore;
        _ ->
            %% ?LOG("Not supported ~p ~p~n",[RefId,_Cs])
            ignore
    end.

see_sect([#xmlElement{content=Cs}]) ->
    see_sect2(Cs, []);
see_sect(Refs) ->
    Res = see_sect2(Refs, []),
    Res.

see_sect2([#xmlText{value=Txt}|Rest], Acc0) ->
    %% ?DBGCF("wxArtProvider", undefined, "~p~n", [Txt]),
    MakeRef = fun(TextRef, Acc) ->
                      Stripped = string:trim(TextRef, both, " ."),
                      Split = string:split(Stripped, "_", all),
                      case see_sect3(Split, Stripped) of
                          ignore -> Acc;
                          no_ref -> Acc;
                          Ref -> [Ref|Acc]
                      end
              end,
    Acc = lists:foldl(MakeRef, Acc0, string:lexemes(Txt, ",")),
    see_sect2(Rest, Acc);
see_sect2([Ref|Rest], Acc) ->
    %% ?DBGCF("wxArtProvider", undefined, "~p~n", [t(Ref)]),
    case t(Ref) of
        ignore -> see_sect2(Rest, Acc);
        [{c,_}, {text, " (not implemented" ++ _}] -> see_sect2(Rest, Acc);
        Res when is_list(Res) -> see_sect2(Rest, [Res|Acc]);
        Res -> see_sect2(Rest, [[Res]|Acc])
    end;
see_sect2([], Acc) ->
    lists:reverse(Acc).

see_sect3(_, "") -> ignore;
see_sect3(["overview", Part], Stripped) ->
    see_sect4("Overview " ++ Part, "overview_" ++ Part, Stripped);
see_sect3(["overview", Part, _], Stripped) ->
    see_sect4("Overview " ++ Part, "overview_" ++ Part, Stripped);
see_sect3(["page", "samples" ++ _, _], Stripped) ->
    see_sect4("Examples", "page_samples", Stripped);
see_sect3(_Pre, _Text) ->
    no_ref.

see_sect4(Name, Pre, Stripped0) ->
    [Stripped| _] = string:split(Stripped0, " "),
    Url0 = "https://docs.wxwidgets.org/3.2/",
    Url = Url0 ++ Pre ++ ".html#" ++ Stripped,
    [{url, Url, Name}].

get_text([#xmlText{value=Val}]) ->
    Val;
get_text(_Debug) ->
    {_, ST} = process_info(self(), current_stacktrace),
    {not_single_text, ST, _Debug}. %% return atom so we crash

to_text(Txt) ->
    %% ?DBGCF("wxWindow", undefined, "~p~n", [p(Txt)]),
    tighten_whitespace(to_text(Txt, 0)).

to_text([{text, Txt1}, {text, Txt2}|Rest], Sz0) ->
    to_text([{text, Txt1 ++ Txt2}|Rest], Sz0);
to_text([{text, Txt}|Rest], Sz0) ->
    {Val, Sz} = split_line(Txt, Sz0),
    [Val|to_text(Rest, Sz)];
to_text([{text_nocut, Txt}|Rest], Sz0) ->
    [Txt|to_text(Rest, string:length(Txt) + Sz0)];

to_text([#xmlElement{name=p, content=Val}|Rest], _) ->
    case Rest of
        [] ->
            ["\n\n", to_text(Val, 0)];
        _ ->
            ["\n\n", to_text(Val, 0),"\n\n" | to_text(Rest, 0)]
    end;
to_text([{p,Val}|Rest], _) ->
    case Rest of
        [] ->
            ["\n\n", to_text(Val, 0)];
        _ ->
            ["\n\n", to_text(Val, 0),"\n\n" | to_text(Rest, 0)]
    end;
to_text([#xmlElement{name=c, content=Val}|Rest], Sz) ->
    Part1 = ["`", to_text(Val, 0), "`"],
    [Part1 | to_text(Rest, string:length(Part1) + Sz)];
to_text([{c,Val}|Rest], Sz) ->
    Part1 = case Val of
                [{c, _}|_] ->  %% c in c don't double qoute
                    to_text(Val, 0);
                _ ->
                    ["`", to_text(Val, 0), "`"]
            end,
    [Part1 | to_text(Rest, string:length(Part1) + Sz)];
to_text([#xmlElement{name=seemfa, content=Val}|Rest], Sz) ->
    [to_text(Val, 0) | to_text(Rest, Sz)];
to_text([#xmlElement{name=seetype, content=Val}|Rest], Sz) ->
    [to_text(Val, 0) | to_text(Rest, Sz)];
to_text([{url, Url, none}|Rest], Sz) ->
    UrlString = [Url],
    [ UrlString | to_text(Rest, Sz + string:length(UrlString))];
to_text([{url, Url, String}|Rest], Sz) ->
    UrlString = ["[", String, "](", Url, ")"],
    [ UrlString | to_text(Rest, Sz + string:length(UrlString))];
to_text([{title, Title}|Rest], _) ->
    [  "\n\n## ", to_text(Title,0), "\n\n"| to_text(Rest,0)];
to_text([{list, List}|Rest], _Sz) ->
    ListString = [["* ", list_item(Item), "\n\n"] || {list_item, Item} <- List],
    [ ListString | to_text(Rest, 0)];
to_text([$\s], _Sz) ->
    [];
to_text([C|Rest], Sz) when is_integer(C) ->
    [C|to_text(Rest, Sz+1)];
to_text([L|Rest], Sz) when is_list(L) ->
    to_text(L ++ Rest, Sz);
to_text([], _Sz) ->
    [].


tighten_whitespace(String) ->
    case string:next_codepoint(String) of
        [$\n|Rest] ->
            tighten_whitespace(Rest);
        [$\s|Rest] ->
            tighten_whitespace(Rest);
        [Char|Rest] ->
            [Char|t_wsp_char(Rest)];
        [] ->
            []
    end.

t_wsp_char(String) ->
    case string:next_codepoint(String) of
        [$\n|Rest] ->
            t_wsp_nl(Rest);
        [$\s|Rest] ->
            t_wsp_space(Rest);
        [Char|Rest] ->
            [Char|t_wsp_char(Rest)];
        [] ->
            []
    end.

t_wsp_nl(String) ->
    case string:next_codepoint(String) of
        [$\n|Rest] ->
            t_wsp_para(Rest);
        [$\s|Rest] ->
            t_wsp_nl(Rest);
        [Char|Rest] ->
            ["\n", Char|t_wsp_char(Rest)];
        [] ->
            []
    end.

t_wsp_para(String) ->
    case string:next_codepoint(String) of
        [$\n|Rest] ->
            t_wsp_para(Rest);
        [$\s|Rest] ->
            t_wsp_para(Rest);
        [Char|Rest] ->
            ["\n\n", Char|t_wsp_char(Rest)];
        [] ->
            []
    end.

t_wsp_space(String) ->
    case string:next_codepoint(String) of
        [$\n|Rest] ->
            t_wsp_nl(Rest);
        [$\s|Rest] ->
            t_wsp_space(Rest);
        [Char|Rest] ->
            [$\s, Char|t_wsp_char(Rest)];
        [] ->
            []
    end.

add_space([]) ->
    [];
add_space(Words) ->
    case string:next_codepoint(Words) of
        [$\n|_] -> Words;
        _ -> [$\s|Words]
    end.

split_line(Str, BefSz) ->
    %% ?DBGCF("wxAcceleratorEntry", undefined, "~w ~p~n", [BefSz, Str]),
    Split = string:split(Str, " ", all),
    %% ?DBGCF("wxAcceleratorEntry", undefined, "~p~n", [Split]),
    split_line_1(Split, BefSz).

split_line_1([[Char]|Rest], Len) when Char =:= $,; Char =:= $.; Char =:= $:; Char =:= $) ->
    {Cont, Sz} = split_line_1(Rest, Len+2),
    {[Char|add_space(Cont)], Sz};
split_line_1([[Char]|Rest], Len) when Char =:= $; ; Char =:= $( ->
    {Cont, Sz} = split_line_1(Rest, Len+1),
    {[Char|Cont], Sz};
split_line_1([[Char, Char2]|Rest], Len) when Char =:= $; ; Char =:= $) ->
    {Cont, Sz} = split_line_1(Rest, Len+1),
    {[Char, Char2, $\s|Cont], Sz};
split_line_1([Word|Rest], Len) ->
    WordSz = string:length(Word),
    NewLen = WordSz + Len + 1,
    if Rest == [] ->
            {[Word], Len};
       NewLen > 90 ->
            {Cont, Sz} = split_line_1(Rest, WordSz),
            {["\n",Word | add_space(Cont)], Sz};
       true ->
            {Cont, Sz} = split_line_1(Rest, NewLen),
            {[Word | add_space(Cont)], Sz}
    end;
split_line_1([], Sz) ->
    {[], Sz}.

list_item([{p, Text}]) ->
    to_text(Text);
list_item(Text) ->
    to_text(Text).

is_text([#xmlText{}|R]) ->
    is_text(R);
is_text([{text, _}|R]) ->
    is_text(R);
is_text([C|R]) when is_integer(C) ->
    is_text(R);
is_text([]) -> true;
is_text(_) -> false.

is_text_element(#xmlText{}) -> true;
is_text_element({text, _}) -> true;
is_text_element(#xmlElement{name=E}) when
      E =:= ndash; E =:= emphasis; E =:= bold;
      E =:= computeroutput; E =:= nonbreakablespace;
      E =:= verbatim; E =:= linebreak; E =:= ref ->
    true;
is_text_element(_) ->
    false.

is_single_line(Text) ->
    case string:find(Text, "\n") of
        nomatch -> true;
        _ -> false
    end.

%%%%%%%%%%%%%%%%

fsummary_1([#xmlElement{name=para, content=Cs}|_Fs]) ->
    fsummary_1(Cs);
fsummary_1([#xmlText{value=Val}|Fs]) ->
    [{text, markdown_qoute(Val)}|fsummary_1(Fs)];
fsummary_1([{text, Val}|Fs]) ->
    [{text, Val}|fsummary_1(Fs)];
fsummary_1([#xmlElement{name=E, content=Cs}|Fs])
  when E =:= emphasis; E =:= bold; E=:= computeroutput ->
    [{c, fsummary_1(Cs)}|fsummary_1(Fs)];
fsummary_1([#xmlElement{name=ndash, content=[]}|Fs]) ->
    [{text, "--"}|fsummary_1(Fs)];
fsummary_1([#xmlElement{name=ndash}=E|Fs]) ->
    [E|fsummary_1(Fs)];
fsummary_1([#xmlElement{name=ref}=Ref|Fs]) ->
    case t(Ref) of
        #xmlElement{name=seemfa, content=Cs} ->
            Cs ++ fsummary_1(Fs);
        #xmlElement{name=seeerl, content=Cs} ->
            Cs ++ fsummary_1(Fs);
        #xmlElement{name=seetype, content=Cs} ->
            Cs ++ fsummary_1(Fs);
        #xmlText{value = Cs} ->
            [Cs | fsummary_1(Fs)];
        {text, Cs} ->
            [Cs | fsummary_1(Fs)];
        Cs when is_list(Cs) ->
            Cs ++ fsummary_1(Fs);
        Cs ->
            [Cs | fsummary_1(Fs)]
    end;
fsummary_1([]) ->
    [].

%%%%%%%%%%%%%%%%

lookup(RefId) ->
    Type = case wx_gen:strip_id(RefId) of
               {"class" ++ Class, _} -> Class;
               _ -> other
           end,
    case ets:lookup(defs, RefId) of
        [] when Type =:= other ->
            enum;  % ?
        [] ->
            %% ?LOG("Doc: Ref class Not found ~p ~n",[RefId]),
            not_found;
        Refs ->
            case lookup2(Refs, [get(current_class), Type]) of
                {"wx"++_, _, _} = MFA ->
                    MFA;
                {_, F, A} ->
                    {"wx_misc", F, A};
                not_found ->
                    [{_,{_C,_F}}|_] = Refs,
                    %% ?LOG("Doc: Ref func Not found ~p::~p~n",[_C, _F]),
                    not_found
            end
    end.

lookup2(CFs, [Search|Rest]) ->
    case [{C,Func} || {_, {C, Func}} <- CFs, C =:= Search] of
        [] ->
            lookup2(CFs, Rest);
        [{C,F}|_] ->
            case lookup(C,F) of
                not_found ->
                    lookup2(CFs, Rest);
                Res ->
                    Res
            end
    end;
lookup2(CFs, []) ->
    lookup3(CFs, CFs).

lookup3([{_,{C,F}}|Rest], All) ->
    case lookup(C,F) of
        not_found -> lookup3(Rest, All);
        Res -> Res
    end;
lookup3([], [{_,{_,F}}|_All]) ->
    case get(current_class) of
        "wx_misc" ->  %% Decrease debug printout
            not_found;
        Class ->
            Parents = wx_gen_erl:parents(Class),
            lookup4(Parents,F)
    end.

lookup4([], _Func) ->
    not_found;
lookup4(_, "Create") ->
    %% We don't want unimplemented 'create' to point to base class..
    not_found;
lookup4([Class|R], Func) ->
    case lookup(Class, Func) of
        not_found -> lookup4(R, Func);
        Res ->
            %% ?LOG("FOUND: ~p ~p~n", [Class, Func]),
            Res
    end.


lookup(root, _) -> not_found;
lookup(object, _) -> not_found;
lookup("static", Func) ->
    %% ?LOG("Doc: ~p ~p~n", [get(current_class), _Func]),
    lookup("utils", Func);
lookup(Class, Func) ->
    case get({class, Class}) of
        #class{methods=Ms} ->
            case [M || [#method{name=N, alias=A}=M|_] <- Ms, N =:= Func orelse A =:= Func] of
                [#method{name=N,alias=A,params=Ps, method_type=MT}|_] ->
                    Name = erl_func_name(N,A,MT),
                    As = wx_gen_erl:erl_args_count(Ps, erl_alias),
                    {Class, Name, As};
                [] ->
                    not_found
            end;
        undefined ->
            ?LOG("Doc: Ref class Not found ~p::~p~n",[Class, Func]),
            not_found
    end.

%%%%%%%%%%%%%

%% The xml doesn't close sections correctly due to the markdown
%% used in wxWidgets doesn't close the sections or is not indented
%% correctly, so we need to move sections to the top.

flatten_p(Docs) ->
%%    ?DBGCF("wxWindow", undefined, "~p~n",[Docs]),
    {_Stack, Res} = flatten_p(Docs, [], []),
    %% ?DBGCF("wxWindow", undefined, "~p~n",[Res]),
    try
        true = is_list(Res),
        Res
    catch _:_ ->
            ?LOG("Failed: ~p ~n ~p~n", [Res, Docs]),
            exit(failed)
    end.

flatten_p([Doc|Docs], Stack, Acc) ->
    case f(Doc) of
        #xmlText{value=_Val} = Txt ->
            case is_empty([Txt]) of
                true when Acc =:= []; Docs =:= [] ->
                    flatten_p(Docs, Stack, Acc);
                _ ->
                    flatten_p(Docs, Stack, [Txt|Acc])
            end;
        break when Stack == [] ->
            #xmlElement{name=Type, content=Cs} = Doc,
            {NewStack, Res} = flatten_p(Cs, [Doc#xmlElement{name=Type}], []),
            flatten_2(Docs, NewStack, lists:reverse(Res) ++ Acc);
        break ->
            %% ?DBGCF("wxCheckBox", undefined, "~p ~p~n",[Type, [Name || #xmlElement{name = Name} <- Stack]]),
            [Top|Stack1] = Stack,
            New = Top#xmlElement{content=lists:reverse(Acc)},
            {{break, Stack1, [Doc|Docs]}, [New]};
        {para, Deep} when (hd(Stack))#xmlElement.name =:= para ->
            %% Flatten para in para
            {NewStack, Res} = flatten_p(Deep, Stack, []),  %% ??
            flatten_2(Docs, NewStack, lists:reverse(Res) ++ Acc);
        {Type, Deep} ->
            %% ?DBGCF("wxFrame", undefined, "~p ~p~n",[Type, [Name || #xmlElement{name = Name} <- Stack]]),
            {NewStack, Res} = flatten_p(Deep, [Doc#xmlElement{name=Type}|Stack], []),
            flatten_2(Docs, NewStack, lists:reverse(Res) ++ Acc)
    end;
flatten_p([], [], Acc) ->
    {[], lists:reverse(Acc)};
flatten_p([], [Top|Stack], Acc) ->
    %% ?DBGCF("wxWindow", undefined, "~p ~p ~p~n",
    %%         [Top#xmlElement.name, [Name || #xmlElement{name = Name} <- Stack], p(lists:reverse(Acc))]),
    {Stack, [Top#xmlElement{content=lists:reverse(Acc)}]}.

flatten_2(Cont, {break, [], Cont2}, Acc) ->
    flatten_p(Cont2 ++ Cont, [], Acc);
flatten_2(Cont, {break, [Top|Stack], Cont2}, Acc) ->
    {{break, Stack, Cont2 ++ Cont}, [Top#xmlElement{content=lists:reverse(Acc)}]};
flatten_2(Cont, Stack, Acc) when is_list(Stack) ->
    flatten_p(Cont, Stack, Acc).

f(#xmlElement{name=heading}) ->
    break;
f(#xmlElement{name=para, content=Cs}) ->
    {para,Cs};
f(#xmlElement{name=sect1, content=_Cs}) ->
    break;
%% f(#xmlElement{name=simplesect=T, content=Cs}) ->
%%     break;
f(#xmlElement{name=T, content=Cs}) ->
    {T,Cs};
f(#xmlText{}=T) ->
    T.

camelcase_to_underscore(Name) ->
    Split = split_cc(Name, [], []),
    lists:append(lists:join([$_], Split)).

markdown_qoute([$_|Rest]) ->
    [$\\, $_ | markdown_qoute(Rest)];
markdown_qoute([$*|Rest]) ->
    [$\\, $* | markdown_qoute(Rest)];
markdown_qoute([$<|Rest]) ->
    [ "*<" | markdown_qoute(Rest)];
markdown_qoute([$>|Rest]) ->
    [ ">*" | markdown_qoute(Rest)];
markdown_qoute([Char|Rest]) ->
    [Char|markdown_qoute(Rest)];
markdown_qoute([]) ->
    [].

split_cc([C|Cs], Word, Str)
  when C >= $A, C =< $W ->
    split_cc(Cs, [C+($a-$A)], [lists:reverse(Word)|Str]);
split_cc([C|Cs], Word, Str) ->
    split_cc(Cs, [C|Word], Str);
split_cc([], Word, Str) ->
    lists:reverse([lists:reverse(Word)|Str]).

erl_func_name(_N,_A,constructor) ->
    "new";
erl_func_name(_N,_A,destructor) ->
    "destroy";
erl_func_name(N,A,_) ->
    wx_gen_erl:erl_func_name(N,A).

is_empty(List) ->
    lists:all(fun(#xmlText{value=Text}) -> string:trim(Text) =:= "";
                 (_) -> false
              end, List).

is_include([#xmlText{value=Txt}|_]) ->
    case string:trim(Txt) of
        "Include file:" -> true;
        "#include" ++ _ -> true;
        _ -> false
    end;
is_include(_) -> false.

%% Dbg help
p(List) when is_list(List) ->
    [p(E) || E <- List];
p(#xmlElement{name=itemizedlist=What, content=List} = _DBG) ->
    {What, [p(E) || E <- List]};
p(#xmlElement{name=programlisting=What}) ->
    {What, [code_example]};
p(#xmlElement{name=What, content=Cs}) ->
    {What, p(Cs)};
p(#xmlAttribute{name=What, value=V}) ->
    {attr, What, V};
p(#xmlText{value=Txt}) ->
    case string:strip(Txt) of
        [] -> %% io:format("Ignore: '~s'~n", [Txt]),
            ' ';
        _ ->
            {text, lists:flatten(io_lib:format("~-45.s",[Txt]))}
    end;
p({break, Done, Cont}) ->
    {break, p(Done), p(Cont)};
p({C, List}) ->
    {C, p(List)};
p(Char) ->
    Char.


