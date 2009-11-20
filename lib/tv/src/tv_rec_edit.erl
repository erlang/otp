%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
-module(tv_rec_edit).



-export([start/5,
	 start/6,
	 init/8
	]).


-include("tv_int_def.hrl").



-define(DEFAULT_BG_COLOR, {217,217,217}).

-define(WIN_WIDTH, 375).
-define(WIN_HEIGHT, 341).
-define(ETS_WIN_HEIGHT, 154).

-define(FRAME_WIDTH, 375).
-define(FRAME_HEIGHT, 265).
-define(ETS_FRAME_HEIGHT, 74).

-define(MAX_LABEL_WIDTH, 165).
-define(X0, 15).
-define(Y0, 20).
-define(LABEL_HEIGHT, 30).
-define(ENTRY_HEIGHT, 30).
-define(FONT, {screen,12}).
-define(NEXT_BTN_WIDTH, 57).
-define(NEXT_BTN_HEIGHT, 22).
-define(NEXT_BTN_FG, {178,34,34}).
-define(INSERT_BTN_WIDTH, 80).
-define(INSERT_BTN_HEIGHT, 30).
-define(INSERT_BTN_DIST_BETWEEN, 23).
-define(INSERT_BTN_DIST_FROM_BOTTOM, 23).





start(TableType, TableName, AttributeList, ListsAsStr, ErrMsgMode) ->
    AttributeValues = lists:duplicate(length(AttributeList), undefined),
    spawn_link(?MODULE, init, [TableType, TableName, AttributeList, 
			       AttributeValues, ListsAsStr, ErrMsgMode, self(), true]).



start(TableType, TableName, AttributeList, AttributeValues, ListsAsStr, ErrMsgMode) ->
    spawn_link(?MODULE, init, [TableType, TableName, AttributeList, 
			       AttributeValues, ListsAsStr, ErrMsgMode, self(), false]).




init(TableType,TableName,AttributeList,AttributeValues,ListsAsStr,ErrMsgMode,MasterPid,Insert) ->
    process_flag(trap_exit, true),
    put(error_msg_mode, ErrMsgMode),
    Frames = create_window(TableType, TableName, AttributeList, AttributeValues, 
			   ListsAsStr, Insert),
    loop(TableType, TableName, Frames, AttributeList, AttributeValues, MasterPid, ListsAsStr).





loop(TabType, TabName, Frames, AttrList, AttrVals, MPid, ListsAsStr) ->
    receive
		
	{gs, insert, click, Insert, _Args} ->
	    gs:config(win, [{cursor, busy}]),
	    case get_record(TabType, TabName, AttrList, AttrList, Frames) of
		{ok, NewRec} ->
		    case Insert of
			insert ->
			    MPid ! {new_object, NewRec};
			change ->
			    MPid ! {updated_object, NewRec}
		    end;
		error ->
		    done
	    end,
	    gs:config(win, [{cursor, arrow}]),	    
	    loop(TabType, TabName, Frames, AttrList, AttrVals, MPid, ListsAsStr);
	

	{gs, cancel, click, _Data, _Args} ->
	    exit(normal);
	

	{gs, reset, click, _Data, _Args} ->
	    gs:config(win, [{cursor, busy}]),
	    set_entry_values(TabType, AttrList, AttrVals, ListsAsStr),
	    gs:config(win, [{cursor, arrow}]),
	    loop(TabType, TabName, Frames, AttrList, AttrVals, MPid, ListsAsStr);



	{gs, EntryId, keypress, _Data, ['Tab', _No, 0 | _T]} ->
	    {_Term, {NextEntry, NextFrame}} = 
		check_entry_content(EntryId, AttrList, Frames, forward),
	    case NextEntry of
		EntryId ->
		    gs:config(NextEntry, [{setfocus, true}]);
		_OtherId ->
		    gs:config(NextFrame, [raise]),
		    gs:config(NextEntry, [{setfocus, true},
					  {select, {0,100000000}}])
	    end,
	    loop(TabType, TabName, Frames, AttrList, AttrVals, MPid, ListsAsStr);
	

	{gs, EntryId, keypress, _Data, ['Down' | _T]} ->
	    {_Term, {NextEntry, NextFrame}} = 
		check_entry_content(EntryId, AttrList, Frames, forward),
	    case NextEntry of
		EntryId ->
		    gs:config(NextEntry, [{setfocus, true}]);
		_OtherId ->
		    gs:config(NextFrame, [raise]),
		    gs:config(NextEntry, [{setfocus, true},
					  {select, {0,100000000}}])
	    end,
	    loop(TabType, TabName, Frames, AttrList, AttrVals, MPid, ListsAsStr);
	

	{gs, EntryId, keypress, _Data, ['Tab', _No, 1 | _T]} ->
	    {_Term, {NextEntry, NextFrame}} = 
		check_entry_content(EntryId, AttrList, Frames, backward),
	    gs:config(NextFrame, [raise]),
	    case NextEntry of
		EntryId ->
		    gs:config(NextEntry, [{setfocus, true}]);
		_OtherId ->
		    gs:config(NextFrame, [raise]),
		    gs:config(NextEntry, [{setfocus, true},
					  {select, {0,100000000}}])
	    end,
	    loop(TabType, TabName, Frames, AttrList, AttrVals, MPid, ListsAsStr);
	
	
	{gs, EntryId, keypress, _Data, ['Up' | _T]} ->
	    {_Term, {NextEntry, NextFrame}} = 
		check_entry_content(EntryId, AttrList, Frames, backward),
	    gs:config(NextFrame, [raise]),
	    case NextEntry of
		EntryId ->
		    gs:config(NextEntry, [{setfocus, true}]);
		_OtherId ->
		    gs:config(NextFrame, [raise]),
		    gs:config(NextEntry, [{setfocus, true},
					  {select, {0,100000000}}])
	    end,
	    loop(TabType, TabName, Frames, AttrList, AttrVals, MPid, ListsAsStr);
	
	
	{gs, Id, keypress, _Data, ['Return' | _T]} ->
	    OldCursor = gs:read(Id, cursor),
	    gs:config(Id, [{cursor, busy}]),
	    gs:config(win, [{cursor, busy}]),
	    Insert = gs:read(insert, data),
	    case get_record(TabType, TabName, AttrList, AttrList, Frames) of
		{ok, NewRec} ->
		    case Insert of
			insert ->
			    MPid ! {new_object, NewRec};
			change ->
			    MPid ! {updated_object, NewRec}
		    end;
		error ->
		    done
	    end,
	    gs:config(win, [{cursor, arrow}]),
	    gs:config(Id, [{cursor, OldCursor}]),
	    loop(TabType, TabName, Frames, AttrList, AttrVals, MPid, ListsAsStr);


	
	{gs, _Id, click, FrameNo, _Args} ->
	    gs:config(lists:nth(FrameNo, Frames), [raise]),
	    loop(TabType, TabName, Frames, AttrList, AttrVals, MPid, ListsAsStr);


	{gs, win, configure, _Data, [Width | _T]} ->
	    resize_window(TabType, lists:max([Width, ?WIN_WIDTH]), Frames, AttrList),
	    loop(TabType, TabName, Frames, AttrList, AttrVals, MPid, ListsAsStr);
	    

	{gs, win, destroy, _Data, _Args} ->
	    exit(normal);
	

	insert_mode ->
	    NewAttrVals = lists:duplicate(length(AttrList), undefined),
	    set_entry_values(TabType, AttrList, NewAttrVals, ListsAsStr),
	    loop(TabType, TabName, Frames, AttrList, NewAttrVals, MPid, ListsAsStr);

	
	{update_mode, Obj} ->
	    NewAttrVals = 
		case TabType of
		    mnesia ->
			case Obj of
			    undefined ->
				lists:duplicate(length(AttrList), undefined);
			    _AnyRec ->
				tl(tuple_to_list(Obj))
			end;
		    ets ->
			[Obj]
		end,
	    set_entry_values(TabType, AttrList, NewAttrVals, ListsAsStr),
	    loop(TabType, TabName, Frames, AttrList, NewAttrVals, MPid, ListsAsStr);
	    
	    
	{reset_info, Obj} ->
	       %% Info to use, instead of old info, when reset button is pressed.
	    NewAttrVals = 
		case TabType of
		    mnesia ->
			case Obj of
			    undefined ->
				lists:duplicate(length(AttrList), undefined);
			    _AnyRec ->
				tl(tuple_to_list(Obj))
			end;
		    ets ->
			[Obj]
		end,
	    loop(TabType, TabName, Frames, AttrList, NewAttrVals, MPid, ListsAsStr);
	    

	    raise ->
	    gs:config(win, [raise]),
	    loop(TabType, TabName, Frames, AttrList,AttrVals, MPid, ListsAsStr);


	{'EXIT', _Pid, _Reason} ->
	    exit(normal);


	_Other ->
	    loop(TabType, TabName, Frames, AttrList,AttrVals, MPid, ListsAsStr)
    end.




resize_window(TabType, WinWidth, Frames, AttrList) ->
    WinHeight = 
	case TabType of 
	    mnesia ->
		get_window_height(length(AttrList));
	    ets ->
		?ETS_WIN_HEIGHT
	end,
    gs:config(win, [{width, WinWidth},
		    {height, WinHeight}
		   ]),
    FrameWidth = WinWidth,
    LblL = lists:map(fun(H) ->
			     gs:config(H, [{width, FrameWidth}]),
			     {LblW, BId, NId} = gs:read(H, data),
			     XNext = get_next_btn_xpos(FrameWidth),
			     XBack = XNext - ?NEXT_BTN_WIDTH,
			     gs:config(BId, [{x, XBack}]),
			     gs:config(NId, [{x, XNext}]),
			     LblW
		     end, 
		     Frames),
    LblW = hd(LblL),
    EntryW = get_entry_width(TabType, FrameWidth, LblW),
    lists:foreach(fun(H) ->
			  gs:config(H, [{width, EntryW}])
		  end,
		  AttrList),
    gs:config(btnframe, [{width, FrameWidth}]),
    {XInsert, XCancel, XReset} = get_insert_btn_coords(WinWidth),
    gs:config(insert, [{x, XInsert}]),
    gs:config(cancel, [{x, XCancel}]),
    gs:config(reset, [{x, XReset}]).




check_entry_content(EntryId, AttributeList, Frames, Direction) ->
    EditedStr = gs:read(EntryId, text),
    case tv_db_search:string_to_term(EditedStr) of
	{error, {_Reason, Msg}} ->
	    gs:config(EntryId, [beep]),
	    tv_utils:notify(gs:start(), "TV Notification", Msg),
	    {error, {EntryId, no_matter}};
	{ok, NewTerm} ->
	    {{ok,NewTerm}, get_next_entry_id(EntryId, AttributeList, Frames, Direction)}
    end.

    


get_next_entry_id(EntryId, AttributeList, Frames, Direction) ->
    OldPos = get_pos(EntryId, AttributeList),
    MaxPos = length(AttributeList),
    NewPos = case Direction of
		 forward when OldPos < MaxPos ->
		     OldPos + 1;
		 forward ->
		     1;
		 backward when OldPos > 1 ->
		     OldPos - 1;
		 backward ->
		     MaxPos;
		 stationary ->
		     OldPos
	     end,
    FramePos = get_next_frame_id(NewPos),
    {lists:nth(NewPos, AttributeList), lists:nth(FramePos, Frames)}.




get_next_frame_id(Pos) ->
    case Pos rem 5 of
	0 ->
	    Pos div 5;
	_Other ->
	    (Pos div 5) + 1
    end.
    



get_record(TabType, TabName, AttrList, AttrList, Frames) ->
    case get_record(AttrList, AttrList, Frames, []) of
	{ok, RecList} ->
	    case TabType of
		mnesia ->
		    NewRecList = [TabName | RecList],
		    {ok, list_to_tuple(NewRecList)};
		ets ->
		    {ok, hd(RecList)}   %% Only one element, a tuple!
	    end;
	error ->
	    error
    end.




get_record([H | T], AttrList, Frames, Acc) ->
    case check_entry_content(H, AttrList, Frames, forward) of
	{{ok, NewTerm}, _PosTuple} ->
	    get_record(T, AttrList, Frames, [NewTerm | Acc]);
	{error, _PosTuple} ->
	    {EntryId, FrameId} = get_next_entry_id(H, AttrList, Frames, stationary),
	    gs:config(FrameId, [raise]),
	    gs:config(EntryId, [{setfocus, true}]),
	    error
    end;
get_record([], _AttrList, _Frames, Acc) ->
    {ok, lists:reverse(Acc)}.
	    
		




get_pos(Elem, L) ->
    get_pos(Elem, L, 1).


get_pos(Elem, [Elem | _T], N) ->
    N;
get_pos(Elem, [_H | T], N) ->
    get_pos(Elem, T, N + 1).




create_window(mnesia, TableName, AttrList, AttrValues, ListsAsStr, Insert) ->
    NofAttr       = length(AttrList),
    NofFrames =
	case NofAttr rem 5 of
	    0 ->
		NofAttr div 5;
	    _Rem ->
		(NofAttr div 5) + 1
	end,

    WinHeight   = get_window_height(NofAttr),
    FrameHeight = get_frame_height(NofAttr),

    Attr = get_longest_attribute_name(AttrList),
    LabelWidth = lists:min([?MAX_LABEL_WIDTH,
			    element(1, gs:read(gs:start(), 
					       {font_wh, {?FONT, atom_to_list(Attr)}}))]),
    
    gs:window(win, gs:start(), [{width, ?WIN_WIDTH},
				{height, WinHeight},
				{title, "[TV]   Record Editor:   '" ++ 
				 atom_to_list(TableName) ++ "'"},
				{bg, ?DEFAULT_BG_COLOR},
				{configure, true},
				{destroy, true},
				{cursor, arrow}
			       ]),
    
    create_insert_and_cancel_btns(Insert, WinHeight, FrameHeight),
    FrameList = create_frames(NofFrames, LabelWidth, AttrList, AttrValues, NofFrames, 
			      ListsAsStr, FrameHeight),
    gs:config(hd(FrameList), [raise]),
    gs:config(hd(AttrList), [{setfocus, true},
			     {select, {0,100000000}}]),
    gs:config(win, [{map,true}]),
    FrameList;
create_window(ets, TableName, [Attr], [AttrVal], ListsAsStr, Insert) ->
    gs:window(win, gs:start(), [{width, ?WIN_WIDTH},
				{height, ?ETS_WIN_HEIGHT},
				{title, "[TV]   Tuple Editor, table  '" ++ 
				 atom_to_list(TableName) ++ "'"},
				{bg, ?DEFAULT_BG_COLOR},
				{configure, true},
				{destroy, true},
				{cursor, arrow}
			       ]),
    
    F = gs:frame(win, [{width, ?FRAME_WIDTH},
		       {height, ?ETS_FRAME_HEIGHT},
		       {x, 0},
		       {y, 0},
		       {bg, ?DEFAULT_BG_COLOR},
		       {bw,2},
		       {data, {0, undefined, undefined}}
		      ]),
    
    create_insert_and_cancel_btns(Insert, ?ETS_WIN_HEIGHT, ?ETS_FRAME_HEIGHT),

    EntryW = get_entry_width(ets, ?FRAME_WIDTH, 0),
    EntryX = ?X0 - 2,
    
    EntryText = 
	case AttrVal of
	    undefined ->
		"";
	    _OtherVal ->
		case ListsAsStr of
		    true ->
			tv_io_lib:format("~p", [AttrVal]);
		    false ->
			lists:flatten(io_lib:write(AttrVal))
		end
	end,
    gs:entry(Attr, F, [{width, EntryW},
		       {height, ?LABEL_HEIGHT},
		       {x, EntryX},
		       {y, ?Y0},
		       {bg, {255,255,255}},
		       {fg, {0,0,0}},
		       {bw, 1},
		       {font, ?FONT},
		       {justify, left},
		       {text, EntryText},
		       {cursor, text},
		       {setfocus, true},
		       {enable, true},
		       {keypress,true},
		       {select, {0,100000000}}
		      ]),
    gs:config(win, [{map,true}]),
    [F].
    
    
    

get_insert_btn_coords(WinWidth) ->
    Middle        = round(WinWidth / 2),
    XInsert       = Middle - round(1.5 * ?INSERT_BTN_WIDTH) - ?INSERT_BTN_DIST_BETWEEN,
    XCancel       = Middle - round(0.5 * ?INSERT_BTN_WIDTH),
    XReset        = Middle + round(0.5 * ?INSERT_BTN_WIDTH) + ?INSERT_BTN_DIST_BETWEEN,
    {XInsert, XCancel, XReset}.




create_insert_and_cancel_btns(Insert, WinHeight, FrameHeight) ->
    LowerFrameHeight           = WinHeight - FrameHeight,
    Y                          = ?INSERT_BTN_DIST_FROM_BOTTOM,
    {XInsert, XCancel, XReset} = get_insert_btn_coords(?WIN_WIDTH),
    
    {InsertBtnText, InsertBtnData} = 
	case Insert of
	    true ->
		{"Insert", insert};
	    false ->
		{"Change", change}
	end,

    gs:frame(btnframe, win, [{width, ?FRAME_WIDTH},
			     {height, LowerFrameHeight},
			     {x, 0},
			     {y, FrameHeight},
			     {bg, ?DEFAULT_BG_COLOR},
			     {bw,2}
			    ]),
    gs:button(insert, btnframe, [{width, ?INSERT_BTN_WIDTH},
				 {height, ?INSERT_BTN_HEIGHT},
				 {x, XInsert},
				 {y, Y},
				 {bg, ?DEFAULT_BG_COLOR},
				 {fg, {0,0,0}},
				 {font, ?FONT},
				 {label, {text, InsertBtnText}},
				 {align, center},
				 {data, InsertBtnData}
				]),
    gs:button(cancel, btnframe, [{width, ?INSERT_BTN_WIDTH},
				 {height, ?INSERT_BTN_HEIGHT},
				 {x, XCancel},
				 {y, Y},
				 {bg, ?DEFAULT_BG_COLOR},
				 {fg, {0,0,0}},
				 {font, ?FONT},
				 {label, {text, "Cancel"}},
				 {align, center}
				]),
    gs:button(reset, btnframe, [{width, ?INSERT_BTN_WIDTH},
				{height, ?INSERT_BTN_HEIGHT},
				{x, XReset},
				{y, Y},
				{bg, ?DEFAULT_BG_COLOR},
				{fg, {0,0,0}},
				{font, ?FONT},
				{label, {text, "Reset"}},
				{align, center}
			       ]).
    




create_frames(0, _LblW, _AttrList, _AttrValues, _NofFrames, _ListsAsStr, _FrameHeight) ->    
    [];
create_frames(N, LblW, AttrList, AttrValues, NofFrames, ListsAsStr, FrameHeight) ->
    F = gs:frame(win, [{width, ?FRAME_WIDTH},
		       {height, FrameHeight},
		       {x, 0},
		       {y, 0},
		       {bg, ?DEFAULT_BG_COLOR},
		       {bw,2}
		      ]),
    {BId, NId} = create_back_and_next_btns(F, 5, N, NofFrames),
    gs:config(F, [{data, {LblW, BId, NId}}]),
    {RemAttrList, RemAttrValues} = 
	create_labels_and_entries(5, AttrList, AttrValues, LblW, F, ListsAsStr),
    [F | create_frames(N - 1,LblW,RemAttrList,RemAttrValues,NofFrames,ListsAsStr,FrameHeight)].






create_back_and_next_btns(FrameId, NofEntries, FrameNo, NofFrames) ->
    Y        = ?Y0 + NofEntries * (?LABEL_HEIGHT + 10) + 8,
    XNext    = get_next_btn_xpos(?FRAME_WIDTH),
    XBack    = XNext - ?NEXT_BTN_WIDTH,
    DataNext = (NofFrames - FrameNo + 1) + 1,
    DataBack = (NofFrames - FrameNo + 1) - 1,
    BId = 
	if 
	    DataBack =< 0 ->
		undefined;
	    true ->
		gs:button(FrameId, [{width, ?NEXT_BTN_WIDTH},
				    {height, ?NEXT_BTN_HEIGHT},
				    {x, XBack},
				    {y, Y},
				    {bg, ?DEFAULT_BG_COLOR},
				    {fg, ?NEXT_BTN_FG},
				    {font, ?FONT},
				    {align, center},
				    {label, {text, "< Back"}},
				    %% {underline, 2},
				    {data, DataBack}
				   ])
	end,
    NId = 
	if
	    DataNext > NofFrames ->
		undefined;
	    true ->
		gs:button(FrameId, [{width, ?NEXT_BTN_WIDTH},
				    {height, ?NEXT_BTN_HEIGHT},
				    {x, XNext},
				    {y, Y},
				    {bg, ?DEFAULT_BG_COLOR},
				    {fg, ?NEXT_BTN_FG},
				    {font, ?FONT},
				    {align, center},
				    {label, {text, " Next >"}},
				    %% {underline, 1},
				    {data, DataNext}
				   ])
	end,
    {BId, NId}.




get_next_btn_xpos(FrameWidth) ->
    FrameWidth - ?X0 - ?NEXT_BTN_WIDTH.



get_entry_width(TableType, FrameWidth, LblWidth) ->
    HorizontalSpacing = 
	case TableType of
	    mnesia ->
		10;
	    ets ->
		0
	end,
    FrameWidth - LblWidth - 2 * ?X0 - HorizontalSpacing.
    


create_labels_and_entries(N, [H | T], [VH | VT], LblW, F, ListsAsStr) when N > 0 ->
    Y      = ?Y0 + (5 - N) * (?LABEL_HEIGHT + 10),
    EntryW = get_entry_width(mnesia, ?FRAME_WIDTH, LblW),
    EntryX = ?FRAME_WIDTH - EntryW - ?X0 - 2,

    EntryText = 
	case ListsAsStr of
	    true ->
		tv_io_lib:format("~p", [VH]);
	    false ->
		lists:flatten(io_lib:write(VH))
	end,
    gs:label(F, [{width, LblW},
		 {height, ?LABEL_HEIGHT},
		 {x, ?X0},
		 {y, Y},
		 {bg, ?DEFAULT_BG_COLOR},
		 {fg, {0,0,0}},
		 {align,w},
		 {font, ?FONT},
		 {label, {text, atom_to_list(H)}}
		]),
    gs:entry(H, F, [{width, EntryW},
		    {height, ?LABEL_HEIGHT},
		    {x, EntryX},
		    {y, Y},
		    {bg, {255,255,255}},
		    {fg, {0,0,0}},
		    {bw, 1},
		    {font, ?FONT},
		    {justify, left},
		    {text, EntryText},
		    {cursor, text},
		    {setfocus, false},
		    {enable, true},
		    {keypress,true}
		   ]),
    create_labels_and_entries(N - 1, T, VT, LblW, F, ListsAsStr);
create_labels_and_entries(0, RemAttrList, RemAttrValues, _LblW, _F, _ListsAsStr) ->
    {RemAttrList, RemAttrValues};
create_labels_and_entries(_N, [], [], _LblW, _F, _ListsAsStr) ->
    {[], []}.




get_longest_attribute_name(AttrList) ->
    get_longest_attribute_name(AttrList, 0, undefined).


get_longest_attribute_name([H | T], Max, Attr) ->
    CurrLength = length(atom_to_list(H)),
    if
	CurrLength >= Max ->
	    get_longest_attribute_name(T, CurrLength, H);
	true ->
	    get_longest_attribute_name(T, Max, Attr)
    end;
get_longest_attribute_name([], _Max, Attr) ->
    Attr.




get_window_height(N) ->
    if 
	N >= 5 ->
	    ?WIN_HEIGHT;
	true ->
	    ?WIN_HEIGHT - ((5 - N) * (?LABEL_HEIGHT + 10) + ?NEXT_BTN_HEIGHT + 8)
    end.



get_frame_height(N) ->
    if 
	N >= 5 ->
	    ?FRAME_HEIGHT;
	true ->
	    ?FRAME_HEIGHT - ((5 - N) * (?LABEL_HEIGHT + 10) + ?NEXT_BTN_HEIGHT + 8)
    end.
	    
    


set_entry_values(TabType, [H | T], [VH | VT], ListsAsStr) ->
    EntryText = 
	case VH of
	    undefined when TabType =:= ets ->
		"";
	    _AnyValue ->
		case ListsAsStr of
		    true ->
			tv_io_lib:format("~p", [VH]);
		    false ->
			lists:flatten(io_lib:write(VH))
		end
	end,
    gs:config(H, [{text, EntryText}]),
    set_entry_values(TabType, T, VT, ListsAsStr);
set_entry_values(_TabType, [], [], _ListsAsStr) ->
    done.
