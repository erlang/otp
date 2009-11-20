%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(pman_options).

%% Window with trace options settings (File->Options...)

-export([dialog/3,
	 read_from_file/1, save_to_file/2]).

-include("pman_options.hrl").

-define(WIN_WIDTH,  350).
-define(WIN_HEIGHT, 350).

-define(TOP_WINDOW, xx_pman_option_window_xx).
-define(TOP_FRAME,  xx_pman_top_frame_xx).

-record(state, {resize_frame,   % GS identifier for the main frame
		parent}).       % Pid of parent

%%--dialog/3------------------------------------------------------------
%% Create a window, or return a value indicating that is is already
%% created.

dialog(ParentWin, Title, Options) ->
    Self = self(),
    Pid = spawn(fun() -> dialog(Self, ParentWin, Title, Options) end),
    receive
	{Pid, Value} ->
	    Value % Options2 | {error,destroyed} | {error,cancelled}
    end.

dialog(Parent, ParentWin, Title, Options) ->

    %% Check if the dialog has already been created, in that 
    %% case, we can reuse it. Otherwise a new dialog is created.
    case gse:name_occupied(?TOP_WINDOW) of
	false -> make_window(ParentWin, Title);
	true -> ok
    end,

    %% Window has now been created or may be re-used
    update_window_from_options(Options),

    gse:resize(?TOP_FRAME, ?WIN_WIDTH, ?WIN_HEIGHT),
    gse:map(?TOP_WINDOW),

    loop(#state{resize_frame=?TOP_FRAME, parent=Parent}).

loop(State) ->
    receive
	{gs, _Id, destroy, _Data, _Arg} -> 
	    State#state.parent ! {self(), {error,destroyed}};

	{gs, ?TOP_WINDOW, configure, _Data, [W, H |_]} ->
	    gse:config(State#state.resize_frame,
		       [{width,W},{height,H}]), % repack
	    loop(State);

	{gs, ok_button, click, _Data, _Arg} ->
	    Options = get_options_from_window(),
	    gse:unmap(?TOP_WINDOW),
	    State#state.parent ! {self(), Options};

	{gs, cancel_button, click, _Data, _Arg} ->
	    gse:unmap(?TOP_WINDOW),
	    State#state.parent ! {self(), {error,cancelled}};

	{gs, trace_spawn, click, _Data, [_Text,_,Value]} ->
	    group_radio(Value, trace_spawn_all, [trace_spawn_all,
						 trace_spawn_first]),
	    loop(State);

	{gs, trace_link, click, _Data, [_Text,_,Value]} ->
	    group_radio(Value, trace_link_all, [trace_link_all,
						trace_link_first]),
	    loop(State);

	{gs, trace_in_window, click, _Data, _Arg} ->
	    lists:foreach(fun(X) -> gse:disable(X) end,
			  [trace_file, trace_file_browse]),
	    loop(State);

	{gs, trace_to_file, click, _Data, [_Text,_,_Value]} ->
	    lists:foreach(fun(X) -> gse:enable(X) end,
			  [trace_file, trace_file_browse]),
	    loop(State);

	{gs, trace_file_browse, click, _Data, _Arg} ->
	    Result = tool_utils:file_dialog([{type,save},
					     {file, "Untitled.log"}]),
	    case Result of
		{error, _Reason} ->
		    loop(State);
		{ok, Name,_State} ->
		    gse:config(trace_file, [{text, Name}]),
		    loop(State)
	    end
    end.

-define(LBLOPTS, [{justify,left}, {align,w}]).
-define(BTNOPTS, [{justify,left}, {align,w}]).

make_window(ParentWin, Title) ->

    Font = pman_win:font(),

    gse:named_window(?TOP_WINDOW, ParentWin, [{title,Title},
					      {configure,true},
					      {width, ?WIN_WIDTH},
					      {height, ?WIN_HEIGHT}]),

    gse:named_frame(?TOP_FRAME, ?TOP_WINDOW,
		    [{bw,3},
		     {packer_x,[{stretch,1,175}, {stretch,1,175}]},
		     {packer_y,[{stretch,3},{stretch,2},{stretch,1}]}]),

    F11 = gse:frame(?TOP_FRAME, [{bw,3},
				 {pack_xy,{1,1}},
				 {packer_x,[{stretch,1},
					    {stretch,20},
					    {stretch,2}]},
				 {packer_y,[{stretch,2},
					    {stretch,1},
					    {stretch,1},
					    {stretch,1},
					    {stretch,1},
					    {stretch,1},
					    {stretch,1},
					    {stretch,1}]}]),

    gse:label(F11,[{pack_xy,{2,1}},
		   {label,{text,"Trace output options:"}},
		   {font,Font} | ?LBLOPTS]),

    gse:named_checkbutton(trace_send, F11,
			  [{pack_xy,{2,2}},
			   {label,{text,"Trace send"}},
			   {font,Font} | ?BTNOPTS]),
    gse:named_checkbutton(trace_receive, F11,
			  [{pack_xy,{2,3}},
			   {label,{text, "Trace receive"}},
			   {font,Font} | ?BTNOPTS]),
    gse:named_checkbutton(trace_functions,F11,
			  [{pack_xy,{2,4}},
			   {label,{text, "Trace functions"}},
			   {font,Font} | ?BTNOPTS]),
    gse:named_checkbutton(trace_events,F11,
			  [{pack_xy,{2,5}},
			   {label,{text, "Trace events"}},
			   {font,Font} | ?BTNOPTS]),

    F21 = gse:frame(?TOP_FRAME, [{bw,3},
				 {pack_xy,{2,1}},
				 {packer_x,[{stretch,1},
					    {stretch,2},
					    {stretch,2},
					    {stretch,20},
					    {stretch,1}]},
				 {packer_y,[{stretch,2},
					    {stretch,1},
					    {stretch,1},
					    {stretch,1},
					    {stretch,1},
					    {stretch,1},
					    {stretch,1},
					    {stretch,1},
					    {stretch,1}]}]),

    gse:label(F21, [{pack_xy,{{2,4},1}},
		    {label,{text,"Inheritance options:"}},
		    {font,Font} | ?LBLOPTS]),

    gse:named_checkbutton(trace_spawn, F21,
			  [{pack_xy,{{2,4},2}},
			   {data,trace_send},
			   {label,{text,"Inherit on spawn"}},
			   {font,Font} | ?BTNOPTS]),
    gse:named_radiobutton(trace_spawn_all, F21,
			  [{pack_xy,{{3,4},3}},
			   {group,spawn},
			   {data,trace_receive},
			   {label,{text, "All spawns"}},
			   {font,Font} | ?BTNOPTS]),
    gse:named_radiobutton(trace_spawn_first, F21,
			  [{pack_xy,{{3,4},4}},
			   {group,spawn},
			   {data,trace_receive},
			   {label,{text,"First spawn only"}},
			   {font,Font} | ?BTNOPTS]),
    gse:named_checkbutton(trace_link, F21,
			  [{pack_xy,{{2,4},6}},
			   {data,trace_send},
			   {label,{text,"Inherit on link"}},
			   {font,Font} | ?BTNOPTS]),
    gse:named_radiobutton(trace_link_all, F21,
			  [{pack_xy,{{3,4},7}},
			   {group,link},
			   {data,trace_receive},
			   {label,{text,"All links"}},
			   {font,Font} | ?BTNOPTS]),

    gse:named_radiobutton(trace_link_first, F21,
			  [{pack_xy,{{3,4},8}},
			   {group,link},
			   {data,trace_receive},
			   {label,{text,"First link only"}},
			   {font,Font} | ?BTNOPTS]),

    F12 = gse:frame(?TOP_FRAME, [{bw,3},
				 {pack_xy,{{1,2},2}},
				 {packer_x,[{stretch,1},
					    {stretch,5},  % Label
					    {stretch,1},
					    {stretch,10}, % Field
					    {stretch,1},
					    {stretch,5},  % Button
					    {stretch,1}]},
				 {packer_y,[{stretch,2},
					    {stretch,1},
					    {stretch,1},
					    {stretch,1}]}]),
    
    gse:label(F12, [{pack_xy,{{2,6},1}},
		    {label,{text,"Trace output options:"}},
		    {font,Font} | ?LBLOPTS]),
    gse:named_radiobutton(trace_in_window, F12,
			  [{pack_xy,{{2,6},2}},
			   {group, trace_dest},
			   {label,{text,"In window"}},
			   {font,Font} | ?BTNOPTS]),
    gse:named_radiobutton(trace_to_file, F12,
			  [{pack_xy,{2,3}},
			   {group, trace_dest},
			   {label,{text,"To file"}},
			   {font,Font} | ?BTNOPTS]),
    gse:named_entry(trace_file, F12, [{pack_xy,{4,3}}, {font,Font}]),
    gse:named_button(trace_file_browse, F12,
		     [{pack_xy,{6,3}},
		      {label,{text," Browse..."}},
		      {font,Font} | ?BTNOPTS]),

    F13 = gse:frame(?TOP_FRAME, [{bw,3},
				 {pack_xy,{{1,2},3}},
				 {packer_x,[{stretch, 1},
					    {fixed, 60},
					    {stretch, 1},
					    {fixed, 60},
					    {stretch, 1}]},
				 {packer_y,[{stretch,1},
					    {fixed, 30},
					    {stretch,1}]}]),
    
    gse:named_button(ok_button, F13, [{pack_xy,{2,2}},
				      {label,{text,"OK"}},
				      {font,Font}]),
    gse:named_button(cancel_button, F13, [{pack_xy,{4,2}},
					  {label,{text,"Cancel"}},
					  {font,Font}]).

update_window_from_options(Options) ->

    %% Trace output
    gse:config(trace_send, [{select,Options#trace_options.send}]),
    gse:config(trace_receive,
	       [{select,Options#trace_options.treceive}]),
    gse:config(trace_functions,
	       [{select,Options#trace_options.functions}]),
    gse:config(trace_events, [{select,Options#trace_options.events}]),

    %% Trace inheritance
    case (Options#trace_options.inherit_on_all_spawn or
	  Options#trace_options.inherit_on_1st_spawn) of
	true ->
	    gse:select(trace_spawn),
	    gse:config(trace_spawn_all,
		       [{select,Options#trace_options.inherit_on_all_spawn}]),
	    gse:config(trace_spawn_first,
		       [{select,Options#trace_options.inherit_on_1st_spawn}]);
	false ->
	    lists:foreach(fun(X) -> gse:disable(X) end,
			  [trace_spawn_all,trace_spawn_first])
    end,

    case (Options#trace_options.inherit_on_all_link or
	  Options#trace_options.inherit_on_1st_link) of
	true -> gse:select(trace_link),
		gse:config(trace_link_all,
			   [{select,Options#trace_options.inherit_on_all_link}]),
		gse:config(trace_link_first,
			   [{select, Options#trace_options.inherit_on_1st_link}]);
	false ->
	    lists:foreach(fun(X) -> gse:disable(X) end,
			  [trace_link_all,trace_link_first])
    end,

    %% Trace ouput destinations
    gse:config(trace_in_window,
	       [{select,(not Options#trace_options.to_file)}]),

    gse:config(trace_to_file, [{select,Options#trace_options.to_file}]),
    gse:config(trace_file, [{text,Options#trace_options.file}]),
    case Options#trace_options.to_file of
	true ->
	    ok;
	false ->
	    lists:foreach(fun(X) -> gse:disable(X) end,
			  [trace_file, trace_file_browse])
    end.

get_options_from_window() ->
    #trace_options{send = gse:read(trace_send,select),
		   treceive = gse:read(trace_receive,select),
		   functions = gse:read(trace_functions,select),
		   events = gse:read(trace_events,select),
		   inherit_on_1st_spawn = gse:read(trace_spawn_first,select),
		   inherit_on_all_spawn = gse:read(trace_spawn_all,select),
		   inherit_on_1st_link = gse:read(trace_link_first,select),
		   inherit_on_all_link = gse:read(trace_link_all,select),
		   to_file = gse:read(trace_to_file,select),
		   file = gse:read(trace_file,text)}.

group_radio(Value, Default, GroupList) ->
    case Value of
	true ->
	    gse:select(Default),
	    lists:foreach(fun(X) -> gse:enable(X) end, GroupList);
	false ->
	    lists:foreach(fun(X) -> gse:deselect(X) end, GroupList),
	    lists:foreach(fun(X) -> gse:disable(X) end, GroupList)
    end.

%%--read_from_file/(File)-----------------------------------------------
%% Returns the options saved in File.
%% If no options can be found, then the default options are
%% returned.

read_from_file(File) ->
    case file:consult(File) of
	{ok, [Term]} ->
	    if
		is_record(Term, trace_options) ->
		    {ok, Term};
		true ->
		    {error, "unexpected contents", #trace_options{}}
	    end;
	{ok, _Terms} ->
	    {error, "unexpected contents", #trace_options{}};
	{error, Tuple} when is_tuple(Tuple) -> % {Line,Mod,Term}
	    {error, "erroneous contents", #trace_options{}};
	{error, _Posix} ->
	    %% The most probable reason is that the file does not
	    %% exist, this is not an error so we simply return
	    %% the default trace options instead
	    {ok, #trace_options{}}
    end.

%%--save_to_file(Options, File)-----------------------------------------

save_to_file(Options, File) ->
    case file:open(File, [write]) of
	{ok, Fd} ->
	    {{Year,Month,Day},{H,M,S}} = calendar:local_time(),
	    io:format(Fd, "%%%~n", []),
	    io:format(Fd, "%%% File: ~s~n", [File]),
	    io:format(Fd, "%%% Date: ~w-~2..0w-~2..0w, ~2..0w:~2..0w:~2..0w~n",
		      [Year,Month,Day,H,M,S]),
	    io:format(Fd, "%%%~n", []),
	    io:format(Fd, "%%% This file was created by Pman. ~n", []),
	    io:format(Fd, "%%%~n", []),
	    io:format(Fd, "%%% DO NOT EDIT! ~n", []),
	    io:format(Fd, "%%%~n", []),
	    io:format(Fd, "%%%~n", []),
	    io:format(Fd, "~p.~n", [Options]),
	    file:close(Fd),
	    ok;
	{error, Posix} ->
	    {error, file:format_error(Posix)}
    end.
