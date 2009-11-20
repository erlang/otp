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
-module(pman_tool).

%% Listbox selection window

-export([select/3]).

-record(state, {topwin,
		frame,
		listbox}).

%% Constants
-define(WIN_WIDTH, 350).
-define(WIN_HEIGHT, 350).

select(Win, Title, Choices) ->
    Self = self(),
    Pid = spawn_link(fun() -> init(Self, Win, Title, Choices) end),
    receive 
	{Pid, Result} ->
	    Result
    end.

init(Pid, Win, Title, Choices) ->

    %% Create window
    State = create_window(Win, Title, Choices),

    gse:map(State#state.topwin),
    
    %% enter event loop
    loop(Pid, Choices, State).

loop(Pid, Choices, State) ->
    receive
	{gs, _, destroy, _Data, _Args} ->
	    Pid ! {self(), cancelled};
	{gs, _, configure, _Data, [W, H|_]} ->
	    gse:resize(State#state.frame, W, H),
	    loop(Pid, Choices, State);
	{gs, _, click, ok, _Args} ->
	    case gs:read(State#state.listbox, selection) of
		[] ->
		    Pid ! {self(), cancelled};
		Indices ->
		    Selection = selection(Indices, Choices),
		    Pid ! {self(), Selection}
	    end;
	{gs, _, click, cancel, _Args} ->
	    Pid ! {self(), cancelled};
	{gs, Obj, doubleclick, _Data, _Args} ->
	    self() ! {gs, Obj, click, ok, []},
	    loop(Pid, Choices, State);
	_GSEvent ->
	    loop(Pid, Choices, State)
    end.

selection(Indices, Choices) ->
    selection(0, Indices, Choices).

selection(I, [I|Is], [{Val,_Str}|Vals]) ->
    [Val | selection(I+1, Is, Vals)];
selection(I, [I|Is], [Val|Vals]) ->
    [Val | selection(I+1, Is, Vals)];
selection(_I, [], _Vals) ->
    [];
selection(I, Is, [_Val|Vals]) ->
    selection(I+1, Is, Vals).
    
create_window(Win, Title, Choices) ->
    Font = pman_win:font(Win),

    %% Top window and a frame that covers it entirely, to allow
    %% usage of the packer for geometry management.
    Topwin = gse:window(Win, [{width, ?WIN_WIDTH},
			      {height,?WIN_HEIGHT},
			      {configure, true},
			      {title, Title}]),
    Frame = gse:frame(Topwin, [{packer_x,[{stretch,1},
					  {stretch,1}]},
			       {packer_y,[{stretch,1},
					  {stretch,5},
					  {stretch,1}]}]),

    %% Caption above the list of items
    CaptionTxt = "Select one or more of the following:",
    gse:label(Frame, [{pack_x,{1,2}},
		      {pack_y,{1,1}},
		      {label,{text,CaptionTxt}}, {font,Font}]),

    %% List of selectable items
    Listbox = gse:listbox(Frame, [{pack_x,{1,2}},
				  {pack_y,{2,2}},
				  {selectmode,multiple},
				  {doubleclick, true},
				  {font,Font},
				  {items, str_choices(Choices)}]),

    %% OK and Cancel buttons in a separate frame.
    F13 = gse:frame(Frame, [{bw,1},
			    {pack_xy,{{1,2},3}},
			    {packer_x,[{stretch,1},
				       {fixed, 60},
				       {stretch,1},
				       {fixed, 60},
				       {stretch,1}]},
			    {packer_y,[{stretch,1},
				       {fixed, 30},
				       {stretch,1}]}]),

    gse:button(F13, [{pack_xy,{2,2}},
		     {label,{text,"OK"}}, {font,Font},
		     {data,ok}]),
    gse:button(F13, [{pack_xy,{4,2}},
		     {label,{text,"Cancel"}}, {font,Font},
		     {data,cancel}]),

    gse:resize(Frame, ?WIN_WIDTH, ?WIN_HEIGHT),
    #state{topwin=Topwin, frame=Frame, listbox=Listbox}.

str_choices(Choices) ->
    lists:map(
      fun({Val, Str}) ->
	      lists:flatten(io_lib:format("~p: ~s", [Val, Str]));
	 (Term) ->
	      lists:flatten(io_lib:format("~p", [Term]))
      end,
      Choices).
