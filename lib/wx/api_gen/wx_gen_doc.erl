%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2022. All Rights Reserved.
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
-export([gen/1]).

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

gen(Defs) ->
    %% Fail = ["wxTopLevelWindow", "wxFrame"],
    Fail = [],
    Ev2ClassL = [{wx_gen_erl:event_type_name(Ev),Name} ||
                   #class{name=Name, event=Evs} <- Defs, Evs =/= false, Ev <- Evs],
    Ev2Class = maps:from_list(Ev2ClassL),
    true = length(Ev2ClassL) =:= maps:size(Ev2Class),
    put(ev2class, Ev2Class),
    [gen_class(Class) || #class{parent=Parent, name=Name,options=Opts} = Class <- Defs,
                         not (Parent =:= "static"),
                         not lists:member("ignore", Opts),
                         Fail =:= [] orelse lists:member(Name, Fail)],
    Static = [Class || #class{parent="static", options=Opts} = Class <- Defs,
                       not lists:member("ignore", Opts)],
    gen_misc(Static),
    ok.

gen_class(Class) ->
    try	gen_class1(Class)
    catch throw:skipped ->
	    Class;
          error:Reason:ST ->
            ?LOG("Error: ~P in~n  ~P~n",[Reason, 20, ST, 20])
    end.

gen_class1(#class{name=Name,parent=Parent,methods=Ms, event=Evs}) ->
    put(current_class, Name),
    Funcs = case gen_funcs(Ms) of
                [] -> [nl(0)];
                Fs -> [nl(0), {funcs, Fs}, nl(0)]
            end,
    erase(current_func),

    Types = case Evs of
                false when Name =:= "wxEvtHandler" ->
                    [nl(4), {datatype, [{name, [{name, Name}], []}]},
                     nl(4), {datatype, [{name, [{name, "wxEventType"}], []}]},
                     nl(4), {datatype, [{name, [{name, "wx"}], []}]},
                     nl(4), {datatype, [{name, [{name, "event"}], []}]}
                    ];
                false ->
                    [{datatype, [{name, [{name, Name}], []}]}];
                [_|_] ->
                    [nl(4), {datatype, [{name, [{name, Name}], []}]},
                     nl(4), {datatype, [{name, [{name, wx_gen_erl:event_rec_name(Name)}], []}]},
                     nl(4), {datatype, [{name, [{name, Name++"Type"}], []}]}]
            end,
    ErlRef = {erlref,
              [nl(0), {header, gen_header(Name)},
               nl(0), {module, [Name]},
               nl(0), {modulesummary, class_brief(Name)},
               nl(0) | class_description(Name, Parent, Evs)]
              ++ [nl(0), {datatypes, Types},
                  nl(0) | Funcs]
             },
    open_write("../doc/src/"++Name++".xml"),
    Intro = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
        "<!DOCTYPE erlref SYSTEM \"erlref.dtd\">\n"
        "\n<!-- THIS FILE IS GENERATED DO NOT EDIT -->\n\n",
    Root = [#xmlAttribute{name=prolog, value=Intro}],
    Export = xmerl:export_simple([nl(0), ErlRef],xmerl_xml, Root),
    w("~s~n",[unicode:characters_to_binary(Export)]),
    close(),
    erase(current_class),
    ok.

gen_misc(Files) ->
    Ms = lists:append([Ms || #class{methods=Ms} <- Files]),
    Name = "wx_misc",
    put(current_class, Name),
    Funcs = gen_funcs(Ms),
    erase(current_func),
    open_write("../doc/src/wx_misc.xml"),
    Intro = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
        "<!DOCTYPE erlref SYSTEM \"erlref.dtd\">\n"
        "\n<!-- THIS FILE IS GENERATED DO NOT EDIT -->\n\n",
    Root = [#xmlAttribute{name=prolog, value=Intro}],
    ErlRef = {erlref,
              [nl(0),{header, gen_header(Name)},
               nl(0),{module, [Name]},
               nl(0),{modulesummary, ["Miscellaneous functions."]},
               nl(0),{description, [{p,["Miscellaneous functions."]}]},
               nl(0),{funcs, Funcs},
               nl(0)
              ]},
    Export = xmerl:export_simple([nl(0), ErlRef],xmerl_xml, Root),
    w("~s~n",[unicode:characters_to_binary(Export)]),
    close(),
    erase(current_class),
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
    case fsummary(Doc) of
        [] -> ["Functions for " ++ Name ++ " class"];
        Docs -> [to_text(Docs)]
    end.

class_description(Name, Parent, Evs) ->
    [{_,Doc}] = ets:lookup(docs, Name),
    D0 = doc(detailed, Doc),
    %% ?DBGCF("wxStyledTextEvent", undefined, "~tp~n",[p(D0)]),
    D1 = flatten_p(D0),
    %% ?DBGCF("wxStyledTextEvent", undefined, "~tp~n",[p(D1)]),
    D2 = remove_doxy_link(D1),
    {Events, D3} = make_events(D2, Evs),
    %% ?DBGCF("wxBrush", undefined, "~tp~n",[p(D2)]),
    Docs = translate(D3),

    Parents = wx_gen_erl:parents(Parent),
    MRef = fun(M) -> #xmlElement{name=seeerl, attributes=[{marker,M}],
                                 content=[{c, [#xmlText{value=M}]}]}
           end,
    PRef = case [MRef(P) || P <- Parents, P =/= root, P =/= object] of
               [] -> [];
               Ps ->
                   Derived = {p, ["This class is derived (and can use functions) from: ",
                                  nl(4) | lists:join(" ", Ps)] },
                   [nl(2),Derived, nl(2)]
           end,

    Url = "https://docs.wxwidgets.org/3.1/class" ++
        camelcase_to_underscore(Name) ++ ".html",
    WxRef = {p, ["wxWidgets docs: ", {url, [{href, Url}], [Name]}]},
    [{description, Docs ++ PRef ++ [nl(2),WxRef,nl(2)]}|Events].

gen_funcs(Ms) ->
    lists:foldl(fun(M, Acc) -> gen_func(M, Acc) end, [], Ms).

gen_func(Ms, Acc0) ->
    Last = length(Ms),
    {_, Fs} = lists:foldl(
                fun(M, {N,Acc}) ->
                        {N+1, [gen_func_1(M,N =/= Last,N)|Acc]}
                end, {1,[]}, Ms),
    [nl(2), {func, lists:append(lists:reverse(Fs))}, nl(2) | Acc0].

gen_func_1(#method{name=N,alias=A,params=Ps,where=erl_no_opt,method_type=MT}, SkipDesc, Clause) ->
    put(current_func, N),
    Name = erl_func_name(N,A,MT),
    As = wx_gen_erl:erl_args_count(Ps, erl_no_opt),
    Impl = io_lib:format("~s/~w",[Name,As+1]),
    Desc = case SkipDesc of
               true -> [nl(2)];
               false -> [nl(4),{fsummary, ["See: ", {c, [Impl]}]}, nl(2)]
           end,
    [nl(4),{name, xml_func_name(Name,As,Clause), []}|Desc];
gen_func_1(#method{name=N,alias=A,params=Ps,where=erl_alias,method_type=MT}, SkipDesc, Clause) ->
    put(current_func, N),
    Name = erl_func_name(N,A,MT),
    As = wx_gen_erl:erl_args_count(Ps, erl_alias),
    Impl = io_lib:format("~s/~w",[wx_gen_erl:erl_func_name(N,undefined),As]),
    Desc = case SkipDesc of
               true -> [nl(2)];
               false ->
                   [nl(4),{fsummary, ["See: ", {c, [Impl]}]},
                    nl(4),{desc, [{p, ["See: ", {seemfa, [{marker, [$#|Impl]}], [{c, [Impl]}]},"."]}, nl(4)]},
                    nl(2)]
           end,
    [nl(4),{name, xml_func_name(Name,As,Clause), []}|Desc];
gen_func_1(#method{name=N,id=Id,alias=A,params=Ps,method_type=MT}, SkipDesc, Clause) ->
    put(current_func, N),
    Name = erl_func_name(N,A,MT),
    As = wx_gen_erl:erl_args_count(Ps, erl_alias),
    Desc = case (not SkipDesc) andalso ets:lookup(docs, Id) of
               false ->
                   [nl(2)];
               [] when Name =:= "destroy" ->
                   [
                    nl(4),{fsummary, ["Destructor"]},
                    nl(4),{desc, [{p, ["Destroys the object."]}]},
                    nl(2)];
               [] ->
                   ?LOG(" /~w (~p ~p) is missing docs~n",[As, SkipDesc, Clause]),
                   [nl(4),{fsummary, [""]}, {desc, [""]},
                    nl(2)];
               [{_,Doc}] ->
                   [nl(4),{fsummary, fsummary(Doc)},
                    nl(4),{desc, desc(Doc)},
                    nl(2)]
           end,
    [nl(4),{name, xml_func_name(Name,As,Clause), []}|Desc].
%% Remove paragraph

fsummary(Doc) ->
    case flatten_p(doc(brief, Doc)) of
        [] ->
            %% Det = doc(detailed, Doc);
            [];
        [#xmlElement{name=para, content=Cs}|_] ->
            %% ?DBGCF("wxXmlResource", "ClearHandlers", "~p~n",[p(Cs)]),
            Res = fsummary_1(Cs),
            try xmerl:export_simple_content(Res, xmerl_xml)
            catch _:Reason:ST ->
                    ?LOG("ERROR: ~p~n~p ~p~n ~p~n",[p(Cs), Reason, Res, ST])
            end,
            Res
    end.

desc(Doc) ->
    Docs = doc(brief, Doc) ++ doc(detailed, Doc),
    %% ?DBGCF("wx_misc", "wxNewId", "~100p~n", [p(Docs)]),
    Flat = flatten_p(Docs),
    Clean = remove_doxy_link(Flat),
    %% ?DBGCF("wxSlider", "SetThumbLength", "~100p~n", [p(Flat)]),
    Res = translate(Clean),
    Res.

%%%%%%%%%%%%%%

make_events(D1, Evs) ->
    make_events(D1, Evs, []).

make_events([#xmlElement{name=heading}=E|Es], Evs, Acc) ->
    case is_event_heading(E) of
        false ->
            make_events(Es, Evs, [E|Acc]);
        true ->
            {Events, Rest} = make_events_sect(Es, Evs),
            {Events, lists:reverse(Acc, Rest)}
    end;
make_events([#xmlElement{name=sect1, content=[Title|Cs]}=E|Es], Evs, Acc) ->
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
    end.


make_events_sect(Cs, false) ->
    {Evs, Rest} = get_event_list(Cs),
    Refs = [Ev || Item <- Evs, Ev <- get_event_class(Item), Ev =/= ignore],
    %% ?DBGCF("wxSlider", undefined, "~100p~n",[Refs]),
    EventRefs = lists:join(", ", Refs),
    case EventRefs of
        [] -> {[], Rest};
        _ ->
            EventDoc = [{p, ["Event types emitted from this class: "|EventRefs]}],
            {[{section, [{title, ["Events"]}|EventDoc]}], Rest}
    end;
make_events_sect(Cs, [_|_]) ->
    %% ?DBGCF("wxStyledTextEvent", undefined, "~tp~n",[p(Cs)]),
    {_Refs, Rest} = get_event_list(Cs),
    EvtMarker = "wxEvtHandler#connect/3",
    EvtFunc = "wxEvtHandler:connect/3",
    EvtHRef = #xmlElement{name=seemfa, attributes=[{marker,EvtMarker}],
                          content=[{c, [#xmlText{value=EvtFunc}]}]},
    EvType = get(current_class) ++ "Type",
    TypeRef = #xmlElement{name=seetype, attributes=[{marker,"#" ++ EvType}],
                          content=[{c, [#xmlText{value=EvType}]}]},
    EventDoc = [{p, ["Use ", EvtHRef, " with ", TypeRef,
                     " to subscribe to events of this type."]}],
    EventSect = [{section, [{title, ["Events"]}|EventDoc]}],
    case get(current_class) of
        "wxStyledTextEvent" -> %% Broken xml
            {EventSect, []};
        _ ->
            {EventSect, Rest}
    end.

get_event_list(Cs) ->
    [EvL|R] = lists:dropwhile(fun(#xmlElement{name=itemizedlist}) -> false; (_) -> true end, Cs),
    #xmlElement{name=itemizedlist, content=Evs} = EvL,
    {Evs, R}.

get_event_class(#xmlText{}) -> [];
get_event_class(#xmlElement{name=listitem, content=[#xmlElement{name=para, content=Cs}]}) ->
    [#xmlText{value="EVT_" ++ EventMacro}|_] = Cs,
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
                    [#xmlElement{name=seeerl, attributes=[{marker,Class}], content=[{c, [EvType]}]}]
            end;
        Class ->
            [#xmlElement{name=seeerl, attributes=[{marker,Class}], content=[{c, [EvType0]}]}]
    end.

make_event_refs(Class) ->
    #class{event=Evs} = get({class, Class}),
    Fun = fun(Ev) ->
                  EvType = wx_gen_erl:event_type_name(Ev),
                  #xmlElement{name=seeerl, attributes=[{marker,Class}],
                              content=[{c, [EvType]}]}
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
    #xmlText{value=NL}.

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

t(#xmlText{}=Txt) ->
    Txt;
t(#xmlElement{name=para, content=Cs}) ->
    Docs = translate(Cs),
    case is_empty(Docs) orelse is_include(Cs) of
        true -> ignore;
        false -> #xmlElement{name=p, content=Docs ++ [nl(6)]}
    end;
t(#xmlElement{name=simplesect, attributes=As, content=Cs}) ->
    Split = case As of
                [#xmlAttribute{value="see"}] ->
                    {"See: ", see_sect(Cs)};
                [#xmlAttribute{value=V}] ->
                    {string:titlecase(V) ++ ": ", translate(Cs)}
            end,
    case Split of
        {_, []} -> ignore;
        {Intro, Docs} ->
            #xmlElement{name=p, content=[Intro|Docs ++ [nl(6)]]}
    end;

t(#xmlElement{name=sect1, content=[#xmlElement{name=title, content=Title}|Desc]}) ->
    [{p, [get_text(Title)]} | translate(Desc)];

t(#xmlElement{name=xrefsect,
              content=[#xmlElement{name=xreftitle, content=Title},
                       #xmlElement{name=xrefdescription, content=Desc}]}) ->
    Intro = case get_text(Title) of
                [] -> [];
                "Todo" -> skip;
                T -> [#xmlText{value=T ++ ": "}]
            end,
    case Intro of
        skip -> ignore;
        _ ->
            Docs = translate(Intro ++ Desc),
            case is_empty(Docs) of
                true -> ignore;
                false -> Docs ++ [nl(6)]
            end
    end;

t(#xmlElement{name=ref}=Ref) ->
    see(Ref);
t(#xmlElement{name=ulink, attributes=[#xmlAttribute{value=Url}], content=Cs}) ->
    {url, [{href, Url}], Cs};
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
t(#xmlElement{name=itemizedlist, content=_C}) ->
    ignore;
t(#xmlElement{name=orderedlist, content=_C}) ->
    ignore;
t(#xmlElement{name=table, content=_C}) ->
    ignore;
t(#xmlElement{name=listitem, content=_C}) ->
    ignore;
t(#xmlElement{name=entry, content=_C}) ->
    ignore;

t(#xmlElement{name=linebreak}) ->
    ignore;
t(#xmlElement{name=hruler}) ->
    ignore;

t(#xmlElement{name=ndash, content=[]}) ->
    #xmlText{value="-"};
t(#xmlElement{name=mdash, content=[]}) ->
    #xmlText{value="-"};

t(#xmlElement{name=heading, content=Cs}) ->
    case Cs == [] orelse "" == get_text(Cs) of
        true -> ignore;
        false -> {p, Cs}
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
t(#xmlElement{name=What, content=Cs}) ->
    ?LOG("xml unhand: ~p~n  ~P~n", [What,p(Cs),15]),
    ignore.

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
            #xmlElement{name=seemfa, attributes=[{marker,Marker}], content=[{c, [#xmlText{value=Func}]}]};
        enum ->
            EnumP = get_text(Cs),
            #xmlText{value=[$?|EnumP]};
        not_found ->
            Func = get_text(Cs),
            [{c, [Func]}, " (not implemented in wx)"]
    end;
see("compound", As, _Cs) ->
    #xmlAttribute{value=RefId} = lists:keyfind(refid, #xmlAttribute.name, As),
    case RefId of
        "classwxPoint" -> ["{X,Y}"];
        "classwxRect" -> ["{X,Y,W,H}"];
        "classwxSize" -> ["{Width,Height}"];
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
                    [{c, [Class]}, " (not implemented in wx)"];
                _ ->
                    %% AppModule = Class,
                    #xmlElement{name=seeerl, attributes=[{marker,Class}], content=[{c, [Class]}]}
            end;
        "group__group" ++ _ ->
            ignore;
        _ ->
            %% ?LOG("Not supported ~p ~p~n",[RefId,_Cs])
            ignore
    end.

see_sect(Refs) ->
    List = see_sect2(Refs, []),
    WithComma = lists:join([", "], List),
    %% ?LOG("~p~n",[WithComma]),
    lists:append(WithComma).

see_sect2([#xmlText{value=Txt}|Rest], Acc0) ->
    MakeRef = fun(TextRef, Acc) ->
                      Stripped = string:trim(TextRef, both, " ."),
                      Split = string:split(Stripped, "_", all),
                      see_sect3(Split, Stripped, Acc)
              end,
    Acc = lists:foldl(MakeRef, Acc0, string:lexemes(Txt, ",")),
    see_sect2(Rest, Acc);
see_sect2([Ref|Rest], Acc) ->
    case t(Ref) of
        ignore -> see_sect2(Rest, Acc);
        Res when is_list(Res) -> see_sect2(Rest, [Res|Acc]);
        Res -> see_sect2(Rest, [[Res]|Acc])
    end;
see_sect2([], Acc) ->
    lists:reverse(Acc).

see_sect3(_, "", Acc) -> Acc;
see_sect3(["overview", Part], Stripped, Acc) ->
    [see_sect4("Overview " ++ Part, "overview_" ++ Part, Stripped)|Acc];
see_sect3(["overview", Part, _], Stripped, Acc) ->
    [see_sect4("Overview " ++ Part, "overview_" ++ Part, Stripped)|Acc];
see_sect3(["page", "samples" ++ _, _], Stripped, Acc) ->
    [see_sect4("Examples", "page_samples", Stripped)|Acc];
see_sect3(_Pre, Text, Acc) ->
    %% ?LOG("~p ~p~n", [_Pre, Text]),
    [[Text]|Acc].

see_sect4(Name, Pre, Stripped) ->
    Url0 = "https://docs.wxwidgets.org/3.1/",
    Url = Url0 ++ Pre ++ ".html#" ++ Stripped,
    [{url, [{href, Url}], [Name]}].

get_text([#xmlText{value=Val}]) ->
    Val;
get_text(_) ->
    not_single_text. %% return atom so we crash if is not expected that the string is empty

to_text([#xmlText{value=Val}|Rest]) ->
    Val ++ to_text(Rest);
to_text([{c,Txt}|Rest]) ->
    Txt ++ to_text(Rest);
to_text([C|Rest]) when is_integer(C) ->
    [C|to_text(Rest)];
to_text([L|Rest]) when is_list(L) ->
    to_text(L) ++ to_text(Rest);
to_text([]) ->
    [].

is_text([#xmlText{}|R]) ->
    is_text(R);
is_text([C|R]) when is_integer(C) ->
    is_text(R);
is_text([]) -> true;
is_text(_) -> false.

is_text_element(#xmlText{}) -> true;
is_text_element(#xmlElement{name=E}) when
      E =:= ndash; E =:= emphasis; E =:= bold;
      E =:= computeroutput; E =:= nonbreakablespace;
      E =:= verbatim; E =:= linebreak; E =:= ref ->
    true;
is_text_element(_) ->
    false.

%%%%%%%%%%%%%%%%

fsummary_1([#xmlText{value=Val}|Fs]) ->
    [Val|fsummary_1(Fs)];
fsummary_1([#xmlElement{name=E, content=Cs}|Fs])
  when E =:= emphasis; E =:= bold; E=:= computeroutput ->
    [{c, Cs}|fsummary_1(Fs)];
fsummary_1([#xmlElement{name=ndash, content=[]}|Fs]) ->
    ["--"|fsummary_1(Fs)];

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
        #xmlText{value=Cs} ->
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

flatten_p(Docs) ->
    R = flatten_p(Docs, top, []),
    R.

flatten_p([Doc|Docs], Parent, Acc0) ->
    case f(Doc) of
        break when Parent =/= top ->
            {break, [Doc|Docs], Acc0};
        break ->
            flatten_p(Docs, Parent, [Doc|Acc0]);
        {Type,Deep} when is_list(Deep), Parent =:= Type ->
            case flatten_p(Deep, Type, Acc0) of
                {break, Rest, Acc} ->
                    {break, Rest++Docs, Acc};
                Acc ->
                    flatten_p(Docs, Parent, Acc)
            end;
        {_Type,Deep} when is_list(Deep), Parent =:= para ->
            %% list or simplsect inside para breakout
            {break, [Doc|Docs], Acc0};
        {Type,Deep} when is_list(Deep) ->
            Cont = [simplesect, xrefdescription, parameterdescription],
            case Type =:= para andalso lists:member(Parent, Cont) of
                true ->
                    flatten_p(Deep ++ Docs, Parent, Acc0);
                false ->
                    case flatten_p(Deep, Type, []) of
                        {break, Rest, Cs} ->
                            Acc = lists:reverse(Cs),
                            case Parent of
                                top ->
                                    flatten_p(Rest++Docs, top, [Doc#xmlElement{name=Type, content=Acc}|Acc0]);
                                _ ->
                                    {break, Rest++Docs, [Doc#xmlElement{name=Type, content=Acc}|Acc0]}
                            end;
                        Cs ->
                            flatten_p(Docs, Parent, [Doc#xmlElement{name=Type, content=Cs}|Acc0])
                    end
            end;
        Xml when Parent =:= top ->
            %% We don't want text outside of para's
            case lists:splitwith(fun is_text_element/1, [Doc|Docs]) of
                {[], _Cont} ->
                    flatten_p(Docs, Parent, [Xml|Acc0]);
                {Cs, Cont} ->
                    flatten_p([#xmlElement{name=para, content=Cs}|Cont], top, Acc0)
            end;
        #xmlText{} = Txt ->
            case is_empty([Txt]) of
                true when Acc0 =:= []; Docs =:= [] ->
                    flatten_p(Docs, Parent, Acc0);
                _ ->
                    flatten_p(Docs, Parent, [Txt|Acc0])
            end;
        Xml ->
                    flatten_p(Docs, Parent, [Xml|Acc0])
    end;
flatten_p([], _, Acc) ->
    lists:reverse(Acc).

f(#xmlElement{name=heading}) ->
    break;
f(#xmlElement{name=para, content=Cs}) ->
    {para,Cs};
f(#xmlElement{name=sect1=T, content=Cs}) ->
    {T,Cs};
f(#xmlElement{name=simplesect=T, content=Cs}) ->
    {T,Cs};
f(#xmlElement{name=itemizedlist=T, content=Cs}) ->
    {T,Cs};
f(#xmlElement{name=orderedlist=T, content=Cs}) ->
    {T,Cs};
f(#xmlElement{name=parameterlist=T, content=Cs}) ->
    {T,Cs};
f(#xmlElement{name=listitem=T, content=Cs}) ->
    {T,Cs};
f(#xmlElement{name=parameterdescription=T, content=Cs}) ->
    {T,Cs};
f(#xmlElement{name=entry=T, content=Cs}) ->
    {T,Cs};
f(#xmlElement{name=row=T, content=Cs}) ->
    {T,Cs};
f(#xmlElement{name=table=T, content=Cs}) ->
    {T,Cs};
f(#xmlElement{name=onlyfor=T, content=Cs}) ->
    {T,Cs};

f(#xmlText{}=T) ->
    T;
f(#xmlElement{name=Type, content=Cs0}=E) ->
    case flatten_p(Cs0, Type, []) of
        Cs when is_list(Cs) -> E#xmlElement{content=Cs};
        _Hmm ->
            ?LOG("Break ~p ~P~n", [Type, p(E), 20]),
            exit(break)
    end.

camelcase_to_underscore(Name) ->
    Split = split_cc(Name, [], []),
    lists:append(lists:join([$_], Split)).

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
        _ -> false
    end;
is_include(_) -> false.

%% Dbg help
p(List) when is_list(List) ->
    [p(E) || E <- List];
p(#xmlElement{name=itemizedlist=What} = _DBG) ->
    {What, [long_list]};
p(#xmlElement{name=programlisting=What}) ->
    {What, [code_example]};
p(#xmlElement{name=What, content=Cs}) ->
    {What, p(Cs)};
p(#xmlAttribute{name=What, value=V}) ->
    {attr, What, V};
p(#xmlText{value=Txt}) ->
    {text, lists:flatten(io_lib:format("~-15.s",[Txt]))};
p({break, Done, Cont}) ->
    {break, p(Done), p(Cont)};
p({C, List}) ->
    {C, p(List)};
p(Char) ->
    Char.


