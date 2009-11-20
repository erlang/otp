-module(ex14).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $ ').

-export([start/0,init/0]).

start() -> spawn(ex14, init, []).

init() ->
    Y = [{y,0},{height, 30},{width, 90}],
    R=[{window, [{width, 400},{height, 300}, {title,"editor"},{map, true}],
       [{editor,editor,[{x,0},{y, 35},{width,300},{height,250},
			{insert,{'end',"Edit this text!"}},{vscroll,right}]},
	{button, clear, [{label, {text, "Clear"}},{x,0} | Y]},
	{checkbutton,enable,[{label,{text,"Enable"}},{select,false},{x,100}|Y]},
	{button, time, [{label, {text, "Insert Time"}},{x,200} | Y]},
	{button, quit,  [{label, {text, "Quit"}},{x,300} | Y]}]}],
    gs:create_tree(gs:start(),R),
    gs:config(editor,{enable,false}),
    loop().

loop() ->
    receive
        {gs, clear, _, _, _} ->
            io:format("clear editor~n"),
            Enable = gs:read(editor, enable),
            gs:config(editor,{enable, true}),
            gs:config(editor,clear),
            gs:config(editor,{enable, Enable});
        {gs, enable, _, _, [_Txt, _Grp, Enable|_]} ->
            io:format("Enable: ~w~n", [Enable]),
            gs:config(editor,{enable, Enable});
        {gs, time, _, _, _} ->
            TimeStr = io_lib:format("Hr:Min:Sec is now ~w:~w:~w~n",
                                    tuple_to_list(time())),
            io:format("Insert Time: ~s~n", [TimeStr]),
            Enable = gs:read(editor, enable),
            gs:config(editor,{enable, true}),
            gs:config(editor,{insert, {insert, TimeStr}}),
            gs:config(editor,{enable, Enable});
        {gs, quit, _, _, _} ->
            exit(normal);
        Other ->
            io:format("Other:~w~n",[Other])
    end,
    loop().
