#!/bin/env escript

-mode(compile).

-record(f, {c,f,t,a,os,w,p}).


main([]) ->
    {ok, A280} = file:consult("wx_28_api.dump"),
    {ok, A300} = file:consult("wx_30_api.dump"),
    A28 = [#f{c=C,f=F,t=T,a=A,os=Os,w=W,p=P} || {C,F,T,A,Os,W,P} <- A280],
    A30 = [#f{c=C,f=F,t=T,a=A,os=Os,w=W,p=P} || {C,F,T,A,Os,W,P} <- A300],
    Diff = A28 -- A30,
    Problems = diff(Diff, A30, []),
    {Remain,Incomp} = write(Problems, 0, 0),
    io:format("~n Remaining Problems: ~p~n",[Remain]),
    io:format(" Known incompatibles: ~p~n",[Incomp]),
    ok.

diff([A|R], A30, Acc) ->
    Potential = find(A, A30),
    case is_ok(A, Potential, A30) of
        true -> diff(R, A30, Acc);
        {opt_differ, _} = Diff ->
            diff(R, A30, [{A, Diff}|Acc]);
        false ->
            diff(R, A30, [{A,Potential}|Acc])
    end;
diff([], _, Acc) ->
    Acc.

find(#f{c=C,f=Search}, New) ->
    Parents = parents(C,New),
    lists:append(find([C|Parents], Search, New)).

find([C|R], M, New) ->
    [[F || #f{c=C2,f=M2} = F <- New, C =:= C2, M =:= M2] | find(R, M, New)];
find([], _, _) ->
    [].

parents(C, All) ->
    case lists:keyfind(C, #f.c, All) of
        #f{p=P} when P =/= root; P =/= object ->
            [P|parents(P,All)];
        _ ->
            []
    end.

is_ok(A,Bs,All) ->
    is_ok(A,Bs,All,false).

is_ok(#f{f=destroy}, _, _, _) ->
    %% Ignore destroy
    true;
is_ok(#f{f='\'Destroy\''}, _, _, _) ->
    %% Ignore destroy
    true;
is_ok(#f{f=M,t=T,a=As,os=O}, [#f{f=M,t=T,a=As,os=O}|_], _, _) ->
    true;
is_ok(#f{f=M,t=T,a=As,os=O,w=Info}=A, [#f{f=M,t=T,a=As,os=O2}|R], All, Acc) ->
    %% Only opts
    case O -- O2 of
        [] when Info == erl_no_opt -> is_ok(A, R, All, Acc);
        [] -> true;
        _ when Info == taylormade -> true;
        _Changed -> is_ok(A, R, All, {opt_differ, O2 -- O})
    end;
is_ok(#f{f=M,t=T,a=As,os=O}=A, [#f{f=M,t=T,a=Bs,os=O}|R], All, Acc) ->
    %% Only args check if merged
    check_args(As,Bs,All) orelse is_ok(A,R,All,Acc);
is_ok(#f{f=M,t=T,a=As,os=O}=A, [#f{f=M,t=_T2, a=As,os=O}|R], All, Acc) ->
    case T of %% Was cast now returns something
        ok ->
            %% io:format("Return type diff: ~p => ~p~n",[A, _T2]),
            true;
        _ ->
            is_ok(A,R,All,Acc)
    end;
is_ok(A, [_|R], All, Acc) ->
    is_ok(A, R, All, Acc);
is_ok(_, [], _All, Acc) ->
    Acc.

check_args([A|As], [A|Bs], All) ->
    check_args(As,Bs, All);
check_args([A|As], [B|Bs], All) when is_list(A), is_list(B) ->
    lists:all(fun(M) -> lists:member(M, B) end, B) andalso check_args(As,Bs,All);
check_args([A|As], [B|Bs], All) when is_list(B) ->
    lists:member(A, B) andalso check_args(As,Bs,All);
check_args([], [], _) ->
    true;
check_args([A|As], [B|Bs], All) when is_atom(A) ->
    Parents = parents(A,All),
    %% io:format("A ~p is ~p? ~p ~p~n", [A, B, Parents, lists:member(B, Parents)]),
    lists:member(B, Parents) andalso check_args(As,Bs, All);
check_args(_, _, _) ->
    false.

%% Known API changes
write([{#f{c=wxGLCanvas},_}|Rest0], P, K) ->
    io:format("wxGLCanvas API is incompatible~n",[]),
    Rest = [F || {#f{c=Mod},_} = F <- Rest0, Mod =/= wxGLCanvas],
    write(Rest, P, K+1);
write([{#f{f=clientDC, a=[], os=[]},_}|Rest], P, K) ->
    io:format("wxClientDC default creators have been removed~n",[]),
    write(Rest, P, K+1);
write([{#f{f=paintDC, a=[], os=[]},_}|Rest], P, K) ->
    io:format("wxPaintDC default creators have been removed~n",[]),
    write(Rest, P, K+1);
write([{#f{f=windowDC, a=[], os=[]},_}|Rest], P, K) ->
    io:format("wxWindowDC default creators have been removed~n",[]),
    write(Rest, P, K+1);
write([{#f{c=wxBitmapButton,f=setBitmapSelected},_}|Rest0], P, K) ->
    io:format("wxBitmapButton:[g|s]etBitmapSelected have been removed~n",[]),
    [{#f{f=getBitmapSelected},_}|Rest] = Rest0,
    write(Rest, P, K+1);
write([{#f{c=wxCalendarDateAttr,f=calendarDateAttr},_}|Rest0], P, K) ->
    io:format("wxCalendarDateAttr:new(ColText [,OptList]) have been removed~n",[]),
    [{#f{f=calendarDateAttr},_}|Rest] = Rest0,
    write(Rest, P, K+1);
write([{#f{c=wxColourData,f=colourData},_}|Rest], P, K) ->
    write(Rest, P, K+1);
write([{#f{c=wxColourPickerCtrl,f=setColour, t=bool},_}|Rest], P, K) ->
    %% Changed type from bool to void
    write(Rest, P, K+1);
write([{#f{c=wxCursor, f=cursor, a=[binary|_]},_}|Rest], P, K) ->
    %% Removed in 2.9
    write(Rest, P, K);
write([{#f{c=wxDC,f=computeScaleAndOrigin},_}|Rest], P, K) ->
    %% Implementation detail?
    write(Rest, P, K+1);
write([{#f{c=wxDC,f=setClippingRegion, a=[_,wxRegion]},_}|Rest], P, K) ->
    %% Implementation detail?
    write(Rest, P, K+1);
write([{#f{c=wxFindReplaceData,f=findReplaceData, a=[_]},_}|Rest], P, K) ->
    %% Changed with default value
    write(Rest, P, K+1);
write([{#f{c=wxGraphicsRenderer,f=GradBrush},_}|Rest], P, K)
  when GradBrush =:= createRadialGradientBrush; GradBrush =:= createLinearGradientBrush ->
    io:format("wxGraphicsRenderer:create*GradientBrush(..) uses GradientStops now~n",[]),
    write(Rest, P, K+1);
write([{#f{c=wxGraphicsRenderer,f=createPen},_}|Rest], P, K) ->
    io:format("wxGraphicsRenderer:createPen(..) have been removed~n",[]),
    write(Rest, P, K+1);

%% wxGrid API have been reworked
write([{#f{c=wxGrid,f=canDragRowSize},_}|Rest], P, K) ->
    io:format("wxGrid API have many changes~n",[]),
    write(Rest, P, K+1);
write([{#f{c=wxGrid,f=canDragColSize},_}|Rest], P, K) -> write(Rest, P, K);
write([{#f{c=wxGrid,f=getViewWidth},_}|Rest], P, K) -> write(Rest, P, K);
write([{#f{c=wxGrid,f=grid, a=[wxWindow,int,int]},_}|Rest], P, K) -> write(Rest, P, K);
write([{#f{c=wxGrid,f=setCellAlignment},_}|Rest], P, K) -> write(Rest, P, K);
write([{#f{c=wxGrid,f=setCellBackgroundColour, a=[_,{int,int,int,int}|_]},_}|Rest], P, K) -> write(Rest, P, K);
write([{#f{c=wxGrid,f=setCellTextColour, a=[_,{int,int,int,int}|_]},_}|Rest], P, K) -> write(Rest, P, K);
write([{#f{c=wxGrid,f=setCellValue, a=[_,list|_]},_}|Rest], P, K) -> write(Rest, P, K);
write([{#f{c=wxGridCellEditor,f=paintBackground},_}|Rest], P, K) -> write(Rest, P, K);
write([{#f{c=wxGridCellEditor,f=beginEdit},_}|Rest], P, K) -> write(Rest, P, K);
write([{#f{c=wxGridCellEditor,f=endEdit},_}|Rest], P, K) -> write(Rest, P, K);

write([{#f{c=wxIcon,f=icon, a=[wx]},_}|Rest], P, K) ->
    write(Rest, P, K+1);
write([{#f{c=wxIconBundle,f=icon, a=[wx]},_}|Rest], P, K) ->
    %% Removed strange
    write(Rest, P, K+1);
write([{#f{c=wxIconizeEvent,f=iconized},_}|Rest], P, K) ->
    %% Changed to isIconized
    write(Rest, P, K+1);
write([{#f{c=wxIdleEvent,f=canSend},_}|Rest], P, K) ->
    %% removed
    write(Rest, P, K+1);
write([{#f{c=wxListCtrl,f=setItem, t=int},_}|Rest], P, K) ->
    io:format("wxListCtrl:setItem/4 changed return value~n",[]),
    write(Rest, P, K+1);
write([{#f{c=wxListCtrl,f=sortItems},_}|Rest], P, K) ->
    write(Rest, P, K);
write([{#f{c=wxMDIClientWindow,f=mDIClientWindow},_}|Rest], P, K) ->
    write(Rest, P, K);
write([{#f{c=wxMenu,f=prepend,a=[_,_,_,_,bool]},_}|Rest], P, K) ->
    write(Rest, P, K);
write([{#f{c=wxMenu,f=insert,a=[_,_,_,_,_,bool]},_}|Rest], P, K) ->
    write(Rest, P, K);
write([{#f{c=wxMenu,f=append,a=[_,_,_,_,bool]},_}|Rest], P, K) ->
    io:format("wxMenu append/insert/prepend have changed return value and lost IsCheckable argument~n",[]),
    write(Rest, P, K+1);
write([{#f{c=wxMultiChoiceDialog,f=multiChoiceDialog,a=[]},_}|Rest], P, K) ->
    write(Rest, P, K+1);
write([{#f{c=wxNotebookEvent},_}|Rest0], P, K) ->
    io:format("wxNotebookEvent have been replaced by the wxBookctrlEvent~n",[]),
    Rest = [F || {#f{c=Mod},_} = F <- Rest0, Mod =/= wxNotebookEvent],
    write(Rest, P, K+1);
write([{#f{c=wxPageSetupDialogData,f=setPaperSize},_}|Rest], P, K) ->
    write(Rest, P, K+1);
write([{#f{c=wxPanel,f=panel},_}|Rest], P, K) ->
    write(Rest, P, K+1);
write([{#f{c=wxPostScriptDC,f=Resolution},_}|Rest], P, K)
  when Resolution =:= setResolution; Resolution =:= getResolution ->
    %% Deprecated in 2.9
    write(Rest, P, K+1);
write([{#f{c=wxPrinter,f=createAbortWindow},_}|Rest], P, K) ->
    write(Rest, P, K+1);
write([{#f{c=wxRegion,f=subtract},_}|Rest], P, K) ->
    write(Rest, P, K+1);
write([{#f{c=wxShowEvent,f=getShow},_}|Rest], P, K) ->
    %% Deprecated in 2.9
    write(Rest, P, K+1);
write([{#f{c=wxSingleChoiceDialog,f=singleChoiceDialog, a=[]},_}|Rest], P, K) ->
    write(Rest, P, K+1);
write([{#f{c=wxSizer,f=recalcSizes},_}|Rest], P, K) ->
    io:format("Removed wxSizer:recalcSizer() was an wxWidgets internal function now deprecated~n", []),
    write(Rest, P, K+1);
write([{#f{c=wxSizerItem,f=setWindow},_}|Rest], P, K) ->
    io:format("Removed depr wxSizerItem:setWindow() use assignWindow~n", []),
    write(Rest, P, K+1);
write([{#f{c=wxSizerItem,f=setSpacer},_}|Rest], P, K) ->
    io:format("Removed depr wxSizerItem:setSpacer() use assignSpacer~n", []),
    write(Rest, P, K+1);
write([{#f{c=wxSizerItem,f=setSizer},_}|Rest], P, K) ->
    io:format("Removed depr wxSizerItem:setSizer() use assignSizer~n", []),
    write(Rest, P, K+1);
write([{#f{c=wxSizerItem, f=sizerItem},_}|Rest0], P, K) ->
    io:format("wxSizerItem:new() Some args have become options ~n",[]),
    Rest = [F || {#f{c=Mod, f=M},_} = F <- Rest0, Mod =/= wxSizerItem orelse M =/= sizerItem],
    write(Rest, P, K+1);
write([{#f{c=wxSplashScreen,f=splashScreen, a=[]},_}|Rest], P, K) ->
    write(Rest, P, K+1);
write([{#f{c=wxSplitterWindow,f=setSashSize},_}|Rest], P, K) ->
    %% Deprecated No-op
    write(Rest, P, K+1);
write([{#f{c=wxStyledTextCtrl,f=addStyledText},_}|Rest], P, K) ->
    %% Unusable API couldn't have been used
    write(Rest, P, K);
write([{#f{c=wxStyledTextCtrl,f=getStyledText},_}|Rest], P, K) ->
    %% Unusable API couldn't have been used
    write(Rest, P, K);
write([{#f{c=wxStyledTextCtrl,f=getCaretSticky},_}|Rest], P, K) ->
    write(Rest, P, K);     %% Changed from bool => int
write([{#f{c=wxStyledTextCtrl,f=setCaretSticky},_}|Rest], P, K) ->
    write(Rest, P, K);     %% Changed from bool => int
write([{#f{c=wxStyledTextCtrl,f=getIndentationGuides},_}|Rest], P, K) ->
    write(Rest, P, K);    %% Changed from bool => int
write([{#f{c=wxStyledTextCtrl,f=setIndentationGuides},_}|Rest], P, K) ->
    write(Rest, P, K);    %% Changed from bool => int
write([{#f{c=wxStyledTextCtrl,f=startStyling},_}|Rest], P, K) ->
    write(Rest, P, K);    %% Remove mask arg?
write([{#f{c=wxStyledTextCtrl,f=usePopUp},_}|Rest], P, K) ->
    io:format("wxStyledTextCtrl some functions have changed args from boolean to int~n"),
    write(Rest, P, K+1);
write([{#f{c=wxToolBar,f=insertTool},_}|Rest], P, K) ->
    write(Rest, P, K);
write([{#f{c=wxToolBar,f=addTool},_}|Rest0], P, K) ->
    io:format("wxToolBar add/insertTool without label have been deprecated~n",[]),
    Rest = [F || {#f{c=Mod, f=M},_} = F <- Rest0, Mod =/= wxToolBar orelse M =/= addTool],
    write(Rest, P, K+1);
write([{#f{c=wxWindow,f=makeModal},_}|Rest], P, K) ->
    io:format("wxWindow:makeModal(..) have been deprecated~n",[]),
    write(Rest, P, K+1);
write([{#f{c=wxWindow,f=setVirtualSizeHints},_}|Rest0], P, K) ->
    io:format("wxWindow:setVirtualSizeHints(..) have been deprecated~n",[]),
    Rest = [F || {#f{c=Mod, f=M},_} = F <- Rest0, Mod =/= wxWindow orelse M =/= setVirtualSizeHints],
    write(Rest, P, K+1);
write([{#f{c=wx_misc,f=genericFindWindowAtPoint},_}|Rest], P, K) ->
    write(Rest, P, K+1);
write([{#f{c=wx_misc,f=shutdown},_}|Rest], P, K) ->
    write(Rest, P, K+1);


%% Options
write([{#f{c=wxDisplay, f=display},{opt_differ,_}}|Rest], P, K) ->
    io:format("wxDisplay::wxDisplay opts incompatible~n",[]),
    write(Rest, P, K+1);
write([{#f{c=wxFlexGridSizer, f=flexGridSizer},{opt_differ,_}}|Rest], P, K) ->
    io:format("wxFlexGridSizer::wxFlexGridSizer opts incompatible~n",[]),
    write(Rest, P, K+1);
write([{#f{c=wxGridSizer, f=gridSizer},{opt_differ,_}}|Rest], P, K) ->
    io:format("wxGridSizer::wxGridSizer opts incompatible~n",[]),
    write(Rest, P, K+1);
write([{#f{c=wxHtmlWindow, f=setFonts},{opt_differ,_}}|Rest], P, K) ->
    %% Bug in old api??
    write(Rest, P, K);
write([{#f{c=wxImage, f=convertToGreyscale},{opt_differ,_}}|Rest], P, K) ->
    io:format("wxImage::convertToGreyscale opts incompatible~n",[]),
    write(Rest, P, K+1);

%% Unknown

write([{A, {opt_differ, Opts}}|Rest], P, K) ->
    io:format("28: ~0p ",[A]),
    io:format(" Changed Opts: ~p~n",[Opts]),
    write(Rest, P+1, K);
write([{A,Ps}|Rest], P, K) ->
    io:format("28: ~0p~n",[A]),
    [io:format("  30: ~0p~n",[B]) || B <- Ps],
    io:nl(),
    write(Rest, P+1, K);
%% write([_|Rest], P, K) ->
%%     write(Rest, P+1, K);
write([], P, K) ->
    {P,K}.

