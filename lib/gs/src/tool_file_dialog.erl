%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

%%
-module(tool_file_dialog).
-compile([{nowarn_deprecated_function,{gs,button,3}},
          {nowarn_deprecated_function,{gs,config,2}},
          {nowarn_deprecated_function,{gs,entry,3}},
          {nowarn_deprecated_function,{gs,frame,3}},
          {nowarn_deprecated_function,{gs,label,3}},
          {nowarn_deprecated_function,{gs,listbox,3}},
          {nowarn_deprecated_function,{gs,read,2}},
          {nowarn_deprecated_function,{gs,start,0}},
          {nowarn_deprecated_function,{gs,window,3}}]).

-export([start/1]).

-record(opts, {type,          % open | save | multiselect
	       dir,           % string()  Current directory
	       file,          % string()  Filename, no path
	       extensions,    % [string()]  Filtered file extensions
	       hidden}).      % [{Dir, [File]}]  Hidden files per dir.

-define(WIDTH,  250).
-define(HEIGHT, 400).
-define(BTNW,   65).
-define(BTNH,   30).

%% start(Opts) -> {ok, AbsFile, Dir} | {error,cancel} | pid()
%%   Opts = [Opt]
%%     Opt = {type, open|save|multiselect}
%%         | {extensions, [string()]}  % For example ".erl"
%%         | {dir, string()}           % Absolute path
%%         ! {file, string()           % Filename (no path)
%%   AbsFile = string()
%%   Dir = string()
%% An open/save dialog returns {ok, AbsFile, Dir} or {error,cancel}
%% (the latter, ridiculous, return value is kept for backwards
%%  compatibility reasons only).
%%
%% A multiselect box returns a pid and delivers messages on the form:
%%   {select, AbsFile} | {close, Dir}
%%
%% Dir is the current directory displayed and can be used to start a
%% a new filedialog with the same directory.

start(Opts0) ->
    Opts = parse_opts(Opts0),
    Self = self(),
    case Opts#opts.type of
	multiselect ->
	    spawn_link(fun() -> init(Self, Opts) end);
	_Type -> % open | save
	    spawn_link(fun() -> init(Self, Opts) end),
	    receive
		{fd_result, Res} ->
		    Res
	    end
    end.

parse_opts(Opts) ->
    {ok, CWD} = file:get_cwd(),
    DefOpts = #opts{type=open, dir=CWD, file="NoName",
		    extensions=[], hidden=[]},
    parse_opts(Opts, DefOpts).

parse_opts([{type, Type}|Opts], DefOpts) ->
    if
	Type==open; Type==save; Type==multiselect ->
	    parse_opts(Opts, DefOpts#opts{type=Type});
	true ->
	    erlang:error(badarg, [{type,Type}])
    end;
parse_opts([{extensions, Exts}|Opts], DefOpts) ->
    case lists:all(fun(Ext) -> is_list(Ext) end, Exts) of
	true ->
	    parse_opts(Opts, DefOpts#opts{extensions=Exts});
	false ->
	    erlang:error(badarg, [{extension, Exts}])
    end;
parse_opts([{dir, Dir}|Opts], DefOpts) ->
    case filelib:is_dir(Dir) of
	true ->
	    case filename:pathtype(Dir) of
		absolute ->
		    parse_opts(Opts, DefOpts#opts{dir=Dir});
		_ ->
		    parse_opts(Opts,
			       DefOpts#opts{dir=filename:absname(Dir)})
	    end;
	false ->
	    erlang:error(badarg, [{dir, Dir}])
    end;
parse_opts([{file, Name}|Opts], DefOpts) ->
    if
	is_list(Name) ->
	    parse_opts(Opts, DefOpts#opts{file=Name});
	true ->
	    erlang:error(badarg, [{file, Name}])
    end;
parse_opts([_|Opts], DefOpts) -> % ignore unknown options
    parse_opts(Opts, DefOpts);
parse_opts([], DefOpts) ->
    DefOpts.

%%--Loop----------------------------------------------------------------

init(From, Opts) ->
    make_window(Opts),
    loop(From, {?WIDTH,?HEIGHT}, Opts).

loop(From, {OldW,OldH}=Size, Opts) ->
    receive

	%% Window is closed
	{gs, win, destroy, _, _} when Opts#opts.type==multiselect ->
	    From ! {close, Opts#opts.dir};
	{gs, win, destroy, _, _} ->
	    From ! {fd_result, {error, cancel}};

	%% Window is moved or resized
	{gs, win, configure, _, [OldW,OldH|_]} ->
	    loop(From, Size, Opts);
	{gs, win, configure, _, [W,H|_]} ->
	    gs:config(resizer, [{width,W},{height,H}]),
	    loop(From, {W,H}, Opts);

	%% Up button is selected
	{gs, up, click, _, _} ->
	    Opts2 = set_dir(up, Opts),
	    loop(From, Size, Opts2);

	%% A listbox item (dir or file) is selected
	{gs, lb, click, _, [_I,Item|_]} ->
	    Entry = case lists:last(Item) of
			$/ -> "";
			_Ch -> Item
		    end,
	    gs:config(entry, {text,Entry}),
	    loop(From, Size, Opts);

	%% A listbox item (dir or file) is double-clicked
	{gs, lb, doubleclick, _, [_I,Item|_]} ->
	    case lists:last(Item) of
		$/ ->  do_select({dir, Item}, From, Size, Opts);
		_Ch -> do_select({file, Item}, From, Size, Opts)
	    end;

	%% Open/Save/Select button is selected
	{gs, select, click, _, _} ->
	    case gs:read(entry, text) of
		"" ->
		    case gs:read(lb, selection) of
			[] ->
			    gs:config(select, beep),
			    loop(From, Size, Opts);
			[I] ->
			    Item = gs:read(lb, {get,I}),
			    case lists:last(Item) of
				$/ ->
				    do_select({dir, Item},
					      From, Size, Opts);
				_Ch ->
				    do_select({file, Item},
					      From, Size, Opts)
			    end
		    end;
		Item -> do_select(Item, From, Size, Opts)
	    end;
			   
	%% 'Return' is pressed
	{gs, entry, keypress, _, ['Return'|_]} ->
	    case gs:read(entry, text) of
		"" ->
		    gs:config(select, beep),
		    loop(From, Size, Opts);
		Item ->
		    do_select(Item, From, Size, Opts)
	    end;

	%% All button is selected (multiselect dialog)
	{gs, all, click, _, _} ->
	    {_Dirs, Files} = select_all(),
	    lists:foreach(fun(File) ->
				  AbsFile = filename:join(Opts#opts.dir,
							  File),
				  From ! {select, AbsFile}
			  end,
			  Files),
	    From ! {close, Opts#opts.dir};

	%% Cancel button is selected (open/save dialog)
	{gs, cancel, click, _, _} ->
	    From ! {fd_result, {error, cancel}};

	%% Close button is selected (multiselect dialog)
	{gs, close, click, _, _} ->
	    From ! {close, Opts#opts.dir};

	Msg ->
	    io:format("GOT: ~p~n", [Msg]),
	    loop(From, Size, Opts)
    end.

do_select({dir, Name}, From, Size, Opts) ->
    do_select_dir(filename:join(Opts#opts.dir, Name), From, Size, Opts);
do_select({file, Name}, From, Size, Opts) ->
    do_select_file(filename:join(Opts#opts.dir, Name), From, Size,Opts);
do_select(Entry, From, Size, Opts) ->
    AbsName = case filename:pathtype(Entry) of
		  absolute -> Entry;
		  _ -> filename:join(Opts#opts.dir, Entry)
	      end,
    case filelib:is_dir(AbsName) of
	true -> do_select_dir(AbsName, From, Size, Opts);
	false -> do_select_file(AbsName, From, Size, Opts)
    end.
	    
do_select_dir(Dir, From, Size, Opts) ->
    Opts2 = set_dir(Dir, Opts),
    loop(From, Size, Opts2).

do_select_file(File, From, Size, Opts) ->
    case filelib:is_file(File) of
	true when Opts#opts.type==multiselect ->
	    From ! {select, File},
	    Opts2 = update(File, Opts),
	    loop(From, Size, Opts2);
	true -> % open | save
	    From ! {fd_result, {ok, File, Opts#opts.dir}};
	false when Opts#opts.type==save ->
	    case filelib:is_dir(filename:dirname(File)) of
		true ->
		    From ! {fd_result, {ok, File, Opts#opts.dir}};
		false ->
		    gs:config(select, beep),
		    loop(From, Size, Opts)
	    end;
	false -> % multiselect | open
	    gs:config(select, beep),
	    loop(From, Size, Opts)
    end.

%%--Common GUI functions------------------------------------------------

-define(UPW, 35).
-define(UPH, 30).
-define(ENTRYH, 30).

make_window(Opts) ->
    GS = gs:start(),

    Title = case Opts#opts.type of
		open -> "Open File";
		save -> "Save File";
		multiselect -> "Select Files"
	    end,

    Font = case gs:read(GS, {choose_font,{screen,[],12}}) of
	       Font0 when element(1, Font0)==screen ->
		   Font0;
	       _ ->
		   gs:read(GS, {choose_font,{courier,[],12}})
	   end,

    gs:window(win, GS, [{title,Title},
			{width,?WIDTH}, {height,?HEIGHT},
			{configure,true}]),

    Marg = {fixed,5},
    Parent = gs:frame(resizer, win, [{packer_x,[Marg,{stretch,1},Marg]},
				     {packer_y,[Marg,
						{stretch,10},
						{stretch,1,2*?BTNH},
						Marg]}]),
    gs:frame(btnframe, resizer, [{packer_x, [{stretch,1},
					     {fixed,?BTNW},
					     {stretch,1},
					     {fixed,?BTNW},
					     {stretch,1},
					     {fixed,?BTNW},
					     {stretch,1}]},
				 {packer_y, [{stretch,1},
					     {fixed,?BTNH},
					     {stretch,1}]},
				 {pack_x,2}, {pack_y,3}]),

    gs:frame(frame, Parent, [{packer_x,[{fixed,?UPW},{stretch,1}]},
			     {packer_y,[{fixed,?UPH},{fixed,?ENTRYH},
					{stretch,1}]},
			     {pack_x,2}, {pack_y,2}]),

    Fup = filename:join([code:priv_dir(gs),"bitmap","fup.bm"]),
    gs:button(up, frame, [{label,{image, Fup}},
			  {pack_x,1}, {pack_y,1}]),
    gs:label(infodir, frame, [{label,{text," Dir:"}}, {font,Font},
			      {pack_x,2}, {pack_y,1}, {align,w}]),
    gs:label(l1, frame, [{label,{text,"File:"}}, {font,Font}, {align,e},
			 {pack_x,1}, {pack_y,2}]),

    gs:entry(entry, frame, [{font,Font}, {keypress,true},
			    {pack_x,2}, {pack_y,2}]),
    gs:listbox(lb, frame, [{font,Font}, {pack_x,{1,2}}, {pack_y,3},
			   {selectmode,single},
			   {vscroll,right},
			   {click,true}, {doubleclick,true}]),

    set_dir(Opts#opts.dir, Opts),

    case Opts#opts.type of
	multiselect ->
	    gs:button(select, btnframe, [{label,{text,"Select"}},
					 {font,Font},
					 {pack_x,2}, {pack_y,2}]),
	    gs:button(all, btnframe, [{label,{text,"All"}}, {font,Font},
				      {pack_x,4}, {pack_y,2}]),
	    gs:button(close,btnframe,[{label,{text,"Done"}},
				      {font,Font},
				      {pack_x,6}, {pack_y,2}]);
	Type ->
	    Text = case Type of
		       open -> "Open";
		       save -> "Save"
		   end,
	    gs:button(select, btnframe, [{label,{text,Text}},
					 {font,Font},
					 {pack_x,2}, {pack_y,2}]),
	    gs:button(cancel, btnframe, [{label,{text,"Cancel"}},
					 {font,Font},
					 {pack_x,6}, {pack_y,2}])
    end,

    gs:config(resizer, [{width,?WIDTH}, {height,?HEIGHT}]),
    gs:config(win, {map,true}).

%% update(AbsFile, Opts) -> Opts'
update(AbsFile, Opts) ->
    Dir = filename:dirname(AbsFile),
    File = filename:basename(AbsFile),

    %% Hide the file
    Hidden0 = Opts#opts.hidden,
    Hidden = case lists:keysearch(Dir, 1, Hidden0) of
		 {value, {_Dir, Files}} ->
		     lists:keyreplace(Dir, 1, Hidden0,
				      {Dir, [File|Files]});
		 false ->
		     [{Dir, [File]} | Hidden0]
	     end,
    Opts2 = Opts#opts{hidden=Hidden},
    set_dir(Dir, Opts2).

%% select_all() -> {Dirs, Files}
select_all() ->
    Is = lists:seq(0, gs:read(lb, size)-1),
    sort_selected(Is, [], []).

sort_selected([I|Is], Dirs, Files) ->
    FileOrDir = gs:read(lb, {get,I}),
    case lists:last(FileOrDir) of
	$/ ->
	    sort_selected(Is, [drop_last(FileOrDir)|Dirs], Files);
	_Ch ->
	    sort_selected(Is, Dirs, [FileOrDir|Files])
    end;
sort_selected([], Dirs, Files) ->
    {Dirs, Files}.

drop_last(Str) ->
    lists:sublist(Str, length(Str)-1).

%% set_dir(Dir0, Opts) -> Opts'
%%   Dir0 = up | string() absolute path only
set_dir(Dir0, Opts) ->
    Dir = if
	      Dir0==up -> filename:dirname(Opts#opts.dir);
	      true ->Dir0
	  end,

    case filelib:is_dir(Dir) of
	true ->
	    gs:config(frame, {cursor,busy}),
	    gs:config(lb, clear),
	    Items = get_files(Dir, Opts#opts.hidden,
			      Opts#opts.extensions),
	    case Opts#opts.type of
		save ->
		    gs:config(entry, {text,Opts#opts.file});
		_ ->
		    gs:config(entry, {text,""})
	    end,
	    gs:config(lb, [{items,Items}]),
	    gs:config(lb, {selection, clear}),
	    gs:config(infodir, {label,{text,["Dir: "|Dir]}}),
	    gs:config(frame, {cursor,parent}),
	    Opts#opts{dir=Dir};
	false ->
	    gs:config(select, beep),
	    Opts
    end.

get_files(Dir, Hidden, Exts) ->
    {ok, Items0} = file:list_dir(Dir),
    
    Items = case lists:keysearch(Dir, 1, Hidden) of
		{value, {_Dir, HiddenHere}} ->
		    lists:filter(fun(Item0) ->
					 not lists:member(Item0,
							  HiddenHere)
				 end,
				 Items0);
		false ->
		    Items0
	    end,

    get_files(Dir, Items, [], [], Exts).

get_files(Dir, [Item0|Items], Dirs, Files, Exts) ->
    Item = filename:join(Dir, Item0),
    case filelib:is_dir(Item) of
	true ->
	    get_files(Dir, Items, [Item0++"/"|Dirs], Files, Exts);
	false ->
	    case filelib:is_regular(Item) of
		true when Exts==[] ->
		    get_files(Dir, Items, Dirs, [Item0|Files], Exts);
		true ->
		    case lists:member(filename:extension(Item), Exts) of
			true ->
			    get_files(Dir,Items,Dirs,[Item0|Files],Exts);
			false ->
			    get_files(Dir, Items, Dirs, Files, Exts)
		    end;
		false ->
		    get_files(Dir, Items, Dirs, Files, Exts)
	    end
    end;
get_files(_Dir, [], Dirs, Files, _Exts) ->
    lists:sort(Dirs) ++ lists:sort(Files).
