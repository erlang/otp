-module(ex13).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $ ').

-export([start/0,init/0]).

start() -> spawn(ex13, init, []).

init() ->
    I=gs:start(),
    Win=gs:window(I, [{width,200},{height,100},
                      {title,"menu"},{map, true}]),
    Bar = gs:create(menubar, Win, []),
    Fmb = gs:create(menubutton, Bar,
                    [{label,{text,"File"}}]),
    Emb = gs:create(menubutton, Bar,
                    [{label,{text,"Edit"}}]),
    Hmb = gs:create(menubutton, Bar,
                    [{label,{text,"Help"}},{side,right}]),
    Fmnu = gs:create(menu, Fmb, []),
    Emnu = gs:create(menu, Emb, []),
    Hmnu = gs:create(menu, Hmb, []),
    gs:create(menuitem, load, Fmnu,
              [{label,{text, "Load"}}]),
    gs:create(menuitem, save, Fmnu,
              [{label,{text, "Save"}}]),
    Exit = gs:create(menuitem, Fmnu,
                     [{label,{text, "Exit"}}]),
    Color = gs:create(menuitem, Emnu,
                      [{label,{text, "Color"}},
                       {itemtype, cascade}]),
    Cmnu = gs:create(menu, Color, [{disabledfg,gray}]),
    gs:create(menuitem, Cmnu, [{label, {text,"Red"}},
                               {data, {new_color, red}},
                               {itemtype,radio},{group,gr1}]),
    gs:create(menuitem, Cmnu, [{label, {text,"Blue"}},
                               {data, {new_color, blue}},
                               {itemtype,radio},{group,gr1}]),
    gs:create(menuitem,Cmnu, [{label, {text,"Black"}},
                              {data, {new_color, black}},
                              {itemtype,radio},{group,gr1}]),
    Y = gs:create(menuitem, Hmnu, [{label, {text,"You"}},
                                   {itemtype, check}]),
    M = gs:create(menuitem, me, Hmnu, [{label, {text, "Me"}},
                                       {itemtype, check}]),
    gs:create(menuitem, Hmnu, [{itemtype, separator}]),
    gs:create(menuitem, Hmnu, [{label, {text, "Other"}},
                               {itemtype, check},
                               {enable,false}]),
    gs:create(menuitem, doit, Hmnu, [{label, {text, "Doit!"}},
                                     {data, {doit, Y, M}}]),
    loop(Exit, Win).

loop(Exit, Win) ->
    receive
        {gs, save, click, _Data, [Txt, Index | Rest]} ->
            io:format("Save~n");
        {gs, load, click, _Data, [Txt, Index | Rest]} ->
            io:format("Load~n");
        {gs, Exit, click, _Data, [Txt, Index | Rest]} ->
            io:format("Exit~n"),
            exit(normal);
        {gs, _MnuItem, click, {new_color, Color}, Args} ->
            io:format("Change color to ~w. Args:~p~n",
                      [Color, Args]),
            gs:config(Win, [{bg, Color}]);
        {gs, doit, click, {doit, YouId, MeId}, Args} ->
            HelpMe = gs:read(MeId, select),
            HelpYou = gs:read(YouId, select),
            io:format("Doit. HelpMe:~w, HelpYou:~w, Args:~p~n",
                      [HelpMe, HelpYou, Args]);
        Other -> io:format("Other:~p~n",[Other])
    end,
    loop(Exit, Win).
