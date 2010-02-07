%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
%%

-module(dbg_ui_filedialog_win).

%% External exports
-export([create_win/6, create_win/7, get_window/1,
	 tag/2,
	 handle_event/2]).

-record(winInfo, {window,        % gsobj()
		  extra,         % fun()
		  cwd,           % string()
		  pattern        % string()
		 }).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% create_win(GS, Title, Pos, Mode, Filter, Extra)
%% create_win(GS, Title, Pos, Mode, Filter, Extra, FileName) -> #winInfo{}
%%   GS  = term()
%%   Title = string()
%%   Pos = {X,Y}
%%   Mode = normal | multiselect
%%   Filter = string() File name that may include * symbols.
%%   Extra = fun(File) -> {true, tag} | true | {error, term()}
%%   FileName = string() Suggested file name when saving
%%--------------------------------------------------------------------
create_win(GS, Title, {X,Y}, Mode, Filter, Extra) ->
    create_win(GS, Title, {X,Y}, Mode, Filter, Extra, null).
create_win(GS, Title, {X,Y}, Mode, Filter, Extra, FileName) ->
    Pad = 8,
    Wlb = 480, Hlb = 130,

    Font = dbg_ui_win:font(normal),

    {Wlbl, Hlbl} = dbg_ui_win:min_size(["Directories"], 80, 20),
    {Wbtn, Hbtn} = dbg_ui_win:min_size(["Filter","Cancel"], 70, 30),

    %% Window
    Win = gs:window(GS, [{title,Title}, {x, X}, {y,Y}, {destroy,true}]),

    %% 'Filter' label and entry (for selecting directory)
    gs:label(Win, [{label, {text,"Filter"}}, {font, Font}, {align, sw},
		   {x, Pad+2}, {y, Pad}, {width,Wlbl}, {height,Hlbl}]),
    gs:entry('Filter', Win, [{x, Pad}, {y, Pad+Hlbl},
			     {width, Wlb}, {height, Hbtn},
			     {keypress, true}]),

    %% Listboxes (showing directories and files)
    Xmid = Pad + Wlb/2,
    Y2 = Pad + Hlbl + Hbtn + Pad,
    gs:label(Win, [{label, {text,"Directories"}},
		   {font, Font}, {align, sw},
		   {x, Pad+2}, {y, Y2},
		   {width, Wlbl}, {height, Hlbl}]),
    gs:label(Win, [{label, {text,"Files"}},
		   {font, Font}, {align, sw},
		   {x, Xmid+Pad/2+2}, {y, Y2},
		   {width, Wlbl}, {height, Hlbl}]),
    gs:listbox('Dirs', Win, [{x, Pad}, {y, Y2+Hlbl},
			     {width, Wlb/2-Pad/2}, {height, Hlb},
			     {vscroll, right},
			     {click, true}, {doubleclick, true}]),
    gs:listbox('Files', Win, [{x, Xmid+Pad/2}, {y, Y2+Hlbl},
			      {width, Wlb/2-Pad/2}, {height, Hlb},
			      {vscroll, right},
			      {click, true}, {doubleclick, true}]),

    %% 'Selection' label and entry (for selecting file)
    Y3 = Y2 + Hlbl + Hlb,
    gs:label(Win, [{label, {text,"Selection"}}, {font,Font}, {align,sw},
		   {x, Pad+2}, {y, Y3}, {width, Wlbl}, {height, Hlbl}]),
    gs:entry('Selection', Win, [{x, Pad}, {y, Y3+Hlbl},
				{width, Wlb}, {height, Hbtn},
				{keypress, true}]),

    %% Buttons
    Y4 = Y3 + Hlbl + Hbtn + Pad,
    Wb = Wlb - Wbtn,
    Opts = [{y, Y4}, {width, Wbtn}, {height, Hbtn}, {font, Font}],
    case Mode of
	normal ->
	    gs:button(Win, [{label, {text,"Ok"}}, {x, Pad},
			    {data, select} | Opts]),
	    gs:button(Win, [{label, {text,"Filter"}}, {x, Wlb/2-Wbtn/2},
			    {data, filter} | Opts]),
	    gs:button(Win, [{label, {text,"Cancel"}}, {x, Pad+Wb},
			    {data, done} | Opts]);
	multiselect ->
	    gs:button(Win, [{label, {text,"Choose"}}, {x, Pad},
			    {data, select} | Opts]),
	    gs:button(Win, [{label, {text,"All"}}, {x, Pad+Wb/3},
			    {data, multiselect} | Opts]),
	    gs:button(Win, [{label, {text,"Filter"}}, {x, Pad+2*Wb/3},
			    {data, filter} | Opts]),
	    gs:button(Win, [{label, {text,"Done"}}, {x, Pad+Wb},
			    {data, done} | Opts])
    end,

    %% Insert contents
    {ok, Home} = file:get_cwd(),
    {Cwd, Pattern} = update_win(Filter, Extra, Home),
    if
	is_list(FileName) ->
	    gs:config('Selection', {text, filename:join(Cwd,FileName)});
	true -> ignore
    end,

    Wwin = Pad + Wlb + Pad,
    Hwin = Y4 + Hbtn + Pad,
    gs:config(Win, [{width, Wwin}, {height, Hwin}, {map, true}]),

    #winInfo{window=Win, extra=Extra, cwd=Cwd, pattern=Pattern}.

%%--------------------------------------------------------------------
%% get_window(WinInfo) -> Window
%%   WinInfo = #winInfo{}
%%   Window = gsobj()
%%--------------------------------------------------------------------
get_window(WinInfo) ->
    WinInfo#winInfo.window.

%%--------------------------------------------------------------------
%% tag(WinInfo, File)
%%   WinInfo = #winInfo{}
%%   File = string()
%%--------------------------------------------------------------------
tag(WinInfo, File0) ->
    File = relfile(WinInfo#winInfo.cwd, File0),
    case member(File, gs:read('Files', items)) of
	{true, Index} -> gs:config('Files', {change, {Index, tag(File)}});
	false -> ignore
    end.

tag(Str) -> [$*|Str].
untag([$*|Str]) -> Str;
untag([$(|Str]) -> [$)|Rts] = lists:reverse(Str),lists:reverse(Rts);
untag(Str) -> Str.

member(E, L) ->        member(E, L, 0).
member(E, [E|_], I) -> {true, I};
member(E, [_|T], I) -> member(E, T, I+1);
member(_E, [], _I) ->  false.

%%--------------------------------------------------------------------
%% handle_event(GSEvent, WinInfo) -> Command
%%   GSEvent = {gs, Id, Event, Data, Arg}
%%   WinInfo = #winInfo{}
%%   Command = ignore
%%           | {stopped, Dir}
%%           | {win, WinInfo}
%%           | {select, File} | {multiselect, Dir, FileNames}
%%--------------------------------------------------------------------
handle_event({gs, _Id, destroy, _Data, _Args}, WinInfo) ->
    {stopped, WinInfo#winInfo.cwd};

handle_event({gs, 'Filter', keypress, _Data, ['Return'|_]}, WinInfo) ->
    handle_event({gs, null, click, filter, null}, WinInfo);
handle_event({gs, 'Selection', keypress, _Data, ['Return'|_]}, WinInfo) ->
    handle_event({gs, null, click, select, null}, WinInfo);

handle_event({gs, 'Dirs', click, _Data, [0,"..",true|_]}, WinInfo) ->
    Filter = filename:join(filename:dirname(WinInfo#winInfo.cwd),
			   WinInfo#winInfo.pattern),
    gs:config('Filter', {text, Filter}),
    ignore;
handle_event({gs, 'Dirs', click, _Data, [_Index,Str,true|_]}, WinInfo) ->
    Filter = filename:join([WinInfo#winInfo.cwd, Str,
			    WinInfo#winInfo.pattern]),
    gs:config('Filter', {text, Filter}),
    ignore;
handle_event({gs, 'Dirs', doubleclick, _Data, _Arg}, WinInfo) ->
    handle_event({gs, null, click, filter, null}, WinInfo);

handle_event({gs, 'Files', click, _Data, [_Index,Str,true|_]}, WinInfo) ->
    Selection = filename:join(WinInfo#winInfo.cwd, untag(Str)),
    gs:config('Selection', {text, Selection}),
    ignore;
handle_event({gs, 'Files', doubleclick, _Data, _Arg}, WinInfo) ->
    handle_event({gs, null, click, select, null}, WinInfo);
  
handle_event({gs, _Id, click, select, _Arg}, _WinInfo) ->
    {select, gs:read('Selection', text)};
handle_event({gs, _Id, click, multiselect, _Arg}, WinInfo) ->
    Files = [untag(File) || File <- gs:read('Files', items)],
    {multiselect, WinInfo#winInfo.cwd, Files};
handle_event({gs, _Id, click, filter, _Arg}, WinInfo) ->
    {Cwd, Pattern} = update_win(gs:read('Filter', text),
				WinInfo#winInfo.extra,
				WinInfo#winInfo.cwd),
    {win, WinInfo#winInfo{cwd=Cwd, pattern=Pattern}};
handle_event({gs, _Id, click, done, _Arg}, WinInfo) ->
    {stopped, WinInfo#winInfo.cwd};
    
handle_event(_GSEvent, _WinInfo) ->
    ignore.

%%====================================================================
%% Internal functions
%%====================================================================

update_win(Filter, ExtraFilter, Prev) ->
    {Res, {Filter2, Cwd, FilePattern}} = check_filter(Filter, Prev),

    Dirs = [".." | get_subdirs(Cwd)],

    gs:config('Filter', {text, Filter2}),
    gs:config('Dirs', {items, Dirs}),
    gs:config('Selection', {text, Cwd}),

    case Res of
	ok ->
	    Matching = lists:sort(filelib:wildcard(Filter2, erl_prim_loader)),
	    Files = extra_filter(Matching, Cwd, ExtraFilter),
	    gs:config('Files', {items, Files});
	error ->
	    gs:config('Files', beep)
    end,

    {Cwd, FilePattern}.

%% check_filter(Filter, Prev) -> {ok, Res} | {error, Res}
%%   Res = {Filter, Cwd, FilePattern}
%%   Filter = Prev = Cwd = FilePattern = string()
check_filter(Filter0, Prev) ->
    Filter = case filename:pathtype(Filter0) of
		 absolute -> Filter0;
		 _Relative -> filename:absname(Filter0, Prev)
	     end,
    Comps = filename:split(Filter),
    Last = lists:last(Comps),
    FilePattern = case is_pattern(Last) of
		      true -> Last;
		      false -> "*"
		  end,
    {Cwd, Rest} = max_existing(Comps),
    case Rest of
	[] ->
	    %% Filter = existing file or directory
	    Res = case filelib:is_dir(Filter, erl_prim_loader) of
		      true -> {filename:join(Filter, "*"), Filter, "*"};
		      false -> {Filter, filename:dirname(Filter),
				filename:basename(Filter)}
		  end,
	    {ok, Res};
	[FilePattern] ->
	    %% Filter = existing dir and valid pattern
	    {ok, {Filter, Cwd, FilePattern}}; 
	Comps ->
	    %% Filter = garbage
	    {error, {Prev, Prev, "*"}};
	[Name|_Names] ->
	    %% Filter = existing dir ++ pattern or non-existing file/dir
	    case is_pattern(Name) of
		true -> {ok, {Filter, Cwd, FilePattern}};
		false -> {error, {Cwd, Cwd, ""}}
	    end
    end.

max_existing([Name | Names]) ->
    case filelib:is_file(Name, erl_prim_loader) of
	true -> max_existing(Name, Names);
	false -> {[], [Name | Names]}
    end.
max_existing(Dir, [Name | Names]) ->
    Dir2 = filename:join(Dir, Name),
    case filelib:is_file(Dir2, erl_prim_loader) of
	true when Names =:= [] -> {Dir2, []};
	true -> max_existing(Dir2, Names);
	false -> {Dir, [Name | Names]}
    end.

is_pattern(Str) ->
    lists:member($*, Str).

extra_filter([File|Files], Dir, Fun) ->
    case Fun(File) of
	true ->
	    [relfile(Dir, File) | extra_filter(Files, Dir, Fun)];
	{true,tag} ->
	    [[$*|relfile(Dir,File)] | extra_filter(Files, Dir, Fun)];
	{true,disable} ->
	    [[$(|relfile(Dir,File)]++[$)] | extra_filter(Files, Dir, Fun)];
	{error, _Reason} -> extra_filter(Files, Dir, Fun)
    end;
extra_filter([], _Dir, _Fun) -> [].

get_subdirs(Dir) ->
    case erl_prim_loader:list_dir(Dir) of
	{ok, FileNames} ->
	    X = [FN || FN <- FileNames,
		       filelib:is_dir(filename:join(Dir, FN), erl_prim_loader)],
	    lists:sort(X);
	_Error ->
	    []
    end.

%% Return the "remainder" of a file name relative a dir name, examples:
%%   relfile("/home/gunilla", "/home/gunilla/m.erl") -> "m.erl"
%%   relfile("/home/gunilla/dir", "/home/gunilla/dir/m.erl") -> "dir/m.erl"
%%   relfile("/home/gunilla", "/home/arne/m.erl") -> "/home/arne/m.erl"
relfile(Dir, File) ->
    case compare(Dir, File) of
	error -> File;
	RelFile -> RelFile
    end.

compare([_|Dir], [_|File]) ->
    compare(Dir, File);
compare([], [$/|File]) ->
    File;
compare(_, _) ->
    error.
