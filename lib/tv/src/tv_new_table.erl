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
%% %CopyrightEnd%k
-module(tv_new_table).



-export([start/2,
	 init/3
	]).



-define(DEFAULT_BG_COLOR, {217, 217, 217}).
-define(FONT, {screen, 12}).

-define(WIN_WIDTH, 400).
-define(WIN_HEIGHT, 555).   %% 510

-define(FRAME_WIDTH, 400).
-define(FRAME1_HEIGHT, 170).
-define(FRAME2_HEIGHT, 260).
-define(FRAME3_HEIGHT, 125). %% 80
-define(BW, 2).

-define(FRAME_X, 0).
-define(FRAME1_Y, 0).
-define(FRAME2_Y, 170).
-define(FRAME3_Y, 430).


-define(LBL_HEIGHT, 30).
-define(NODE_LBL_WIDTH, 45).
-define(NAME_LBL_WIDTH, 85).
-define(TYPE_LBL_WIDTH, 45).
-define(PROT_LBL_WIDTH, 85).
-define(KEYPOS_LBL_WIDTH, 95).

-define(LBL_X, 10).
-define(NODE_LBL_Y, 20).
-define(NAME_LBL_Y, 80).
-define(TYPE_LBL_Y, 10).
-define(PROT_LBL_Y, 100).
-define(KEYPOS_LBL_Y, 200).


-define(ENTRY_HEIGHT, 30).
-define(NODE_ENTRY_WIDTH, 275).
-define(NAME_ENTRY_WIDTH, 275).
-define(KEYPOS_ENTRY_WIDTH, 50).

-define(ENTRY_X1, 110).
-define(ENTRY_X2, 110).
-define(NODE_ENTRY_Y, 20).
-define(NAME_ENTRY_Y, 80).
-define(KEYPOS_ENTRY_Y, 200).

-define(RBTN_HEIGHT, 30).
-define(RBTN_WIDTH1, 105).
-define(RBTN_WIDTH2, 115).

-define(RBTN_X1, 60).
-define(RBTN_X2, 165).
-define(RBTN_X3, 270).
-define(RBTN_Y1, 40).
-define(RBTN_Y1PLUS, 70).
-define(RBTN_Y2, 130).


-define(CBTN_HEIGHT, 30).
-define(NAMED_TABLE_CBTN_WIDTH, 100).
-define(OPEN_BROWSER_CBTN_WIDTH, 105).

-define(NAMED_TABLE_CBTN_X, 110).
-define(NAMED_TABLE_CBTN_Y, 120).

-define(OPEN_BROWSER_CBTN_X, 85).  %% 215
-define(OPEN_BROWSER_CBTN_Y, 10).  %% 200


-define(BTN_WIDTH, 100).
-define(BTN_HEIGHT, 30).

-define(BTN_X1, 85). 
-define(BTN_X2, 225).
-define(BTN_Y, 65).   %% 30


-define(VLINE_LBL_WIDTH, (380 - 2 * ?BW)).
-define(VLINE_LBL_HEIGHT, 1).
-define(HLINE_LBL_WIDTH, 1).
-define(HLINE_LBL_HEIGHT, 70).

-define(VLINE_LBL_X, (10 - ?BW)).
-define(VLINE_LBL_Y1, 85).
-define(VLINE_LBL_Y2, 180).
-define(HLINE_LBL_X, 188).
-define(HLINE_LBL_Y, 180).


-define(DEFAULT_NAME, my_table).
-define(DEFAULT_TYPE, set).
-define(DEFAULT_PROT, public).
-define(DEFAULT_KEYPOS, 1).




start(Node, ErrMsgMode) ->
    spawn_link(?MODULE, init, [Node, ErrMsgMode, self()]).





init(Node, ErrMsgMode, MPid) ->
    process_flag(trap_exit, true),
    put(error_msg_mode, ErrMsgMode),
    create_window(Node),
    loop(false, ?DEFAULT_TYPE, ?DEFAULT_PROT, true, MPid).





loop(NamedTab, Type, Prot, OpenBrowser, MPid) ->
    receive
	
	{gs, ok, click, _Data, _Args} ->
	    gs:config(win, [{cursor, busy}]),
	    case create_table(NamedTab, Type, Prot, OpenBrowser, MPid) of
		ok ->
		    exit(normal);
		error ->
		    gs:config(win, [{cursor, arrow}]),
		    loop(NamedTab, Type, Prot, OpenBrowser, MPid)
	    end;


	{gs, cancel, click, _Data, _Args} ->
	    exit(normal);


	{gs, set, click, _Data, _Args} ->
	    loop(NamedTab, set, Prot, OpenBrowser, MPid);


	{gs, ordered_set, click, _Data, _Args} ->
	    loop(NamedTab, ordered_set, Prot, OpenBrowser, MPid);


	{gs, bag, click, _Data, _Args} ->
	    loop(NamedTab, bag, Prot, OpenBrowser, MPid);


	{gs, duplicate_bag, click, _Data, _Args} ->
	    loop(NamedTab, duplicate_bag, Prot, OpenBrowser, MPid);


	{gs, public, click, _Data, _Args} ->
	    gs:config(open_browser, [{enable, true}, {select, OpenBrowser}]),
	    loop(NamedTab, Type, public, OpenBrowser, MPid);


	{gs, protected, click, _Data, _Args} ->
	    gs:config(open_browser, [{enable, true}, {select, OpenBrowser}]),
	    loop(NamedTab, Type, protected, OpenBrowser, MPid);

	
	{gs, private, click, _Data, _Args} ->
	    gs:config(open_browser, [{select, false}, {enable, false}]),
	    loop(NamedTab, Type, private, OpenBrowser, MPid);


	{gs, named_table, click, Data, _Args} ->
	    gs:config(named_table, [{data, not(Data)}]),
	    loop(Data, Type, Prot, OpenBrowser, MPid);
	    

	{gs, open_browser, click, Data, _Args} ->
	    gs:config(open_browser, [{data, not(Data)}]),
	    loop(Data, Type, Prot, Data, MPid);
	    

	{gs, EntryId, keypress, _Data, ['Tab', _No, 0 | _T]} ->
	    case get_entry_term(EntryId) of
		{ok, _Term} ->
		    gs:config(next_entry(EntryId, forward), [{setfocus, true},
							     {select, {0, 100000000}}]);
		error ->
		    done
	    end,
	    loop(NamedTab, Type, Prot, OpenBrowser, MPid);
	

	{gs, EntryId, keypress, _Data, ['Tab', _No, 1 | _T]} ->
	    case get_entry_term(EntryId) of
		{ok, _Term} ->
		    gs:config(next_entry(EntryId, backward), [{setfocus, true},
							      {select, {0, 100000000}}]);
		error ->
		    done
	    end,
	    loop(NamedTab, Type, Prot, OpenBrowser, MPid);
	

	{gs, EntryId, keypress, _Data, ['Down' | _T]} ->
	    case get_entry_term(EntryId) of
		{ok, _Term} ->
		    gs:config(next_entry(EntryId, forward), [{setfocus, true},
							     {select, {0, 100000000}}]);
		error ->
		    done
	    end,
	    loop(NamedTab, Type, Prot, OpenBrowser, MPid);
	

	{gs, EntryId, keypress, _Data, ['Up' | _T]} ->
	    case get_entry_term(EntryId) of
		{ok, _Term} ->
		    gs:config(next_entry(EntryId, backward), [{setfocus, true},
							      {select, {0, 100000000}}]);
		error ->
		    done
	    end,
	    loop(NamedTab, Type, Prot, OpenBrowser, MPid);
	

	{gs, _EntryId, keypress, _Data, ['Return' | _T]} ->
	    gs:config(win, [{cursor, busy}]),
	    case create_table(NamedTab, Type, Prot, OpenBrowser, MPid) of
		ok ->
		    exit(normal);
		error ->
		    gs:config(win, [{cursor, arrow}]),
		    loop(NamedTab, Type, Prot, OpenBrowser, MPid)
	    end;


	{gs, win, configure, _Data, _Args} ->
	    gs:config(win, [{width, ?WIN_WIDTH},
			    {height, ?WIN_HEIGHT}]),
	    loop(NamedTab, Type, Prot, OpenBrowser, MPid);

	
	{gs, win, destroy, _Data, _Args} ->
	    exit(normal);
	

	raise ->
	    gs:config(win, [raise]),
	    loop(NamedTab, Type, Prot, OpenBrowser, MPid);	    


	{error_msg_mode, ErrMsgMode} ->
	    put(error_msg_mode, ErrMsgMode),
	    loop(NamedTab, Type, Prot, OpenBrowser, MPid);
	

	{'EXIT', _Pid, _Reason} ->
	    exit(normal);


	_Other ->
    	    loop(NamedTab, Type, Prot, OpenBrowser, MPid)
    
    end.




create_table(NamedTab, Type, Prot, OpenBrowser, MPid) ->
    case get_entry_term(node_entry) of
	error ->
	    error;
	{ok, Node} ->
	    case get_entry_term(name_entry) of
		error ->
		    error;
		{ok, TabName} ->
		    case get_entry_term(keypos_entry) of
			error ->
			    error;
			{ok, KeyPos} ->
			    Options = 
				[Type, Prot, {keypos, KeyPos}] ++
				case NamedTab of
				    true ->
					[named_table];
				    false ->
					[]
				end,
			    {Readable, NewOpenBrowser} = 
				case Prot of
				    private ->
					{false, false};
				    _Other ->
					{true, OpenBrowser}
				end,
			    MPid ! {tv_new_table, self(), Node, TabName, Options, ets,
				    Readable, NewOpenBrowser},
			    receive
				ok ->
				    ok;
				error ->
				    show_error_msg(),
				    error
			    after 
				5000 ->
				    show_error_msg(),
				    error
			    end
		    end
	    end
    end.





show_error_msg() ->
    Msg = 
	case get(error_msg_mode) of
	    normal ->
		["Couldn't create a table using",
		 "the specified settings!"];
	    haiku ->
		["The table you want",
		 "Could maybe be created.",
		 "But I don't know how."]
	end,
    tv_utils:notify(win, "TV Notification", Msg).

    





get_entry_term(Id) ->
    EditedStr = gs:read(Id, text),
    case tv_db_search:string_to_term(EditedStr) of
	{ok, NewTerm} when Id =:= node_entry, is_atom(NewTerm) ->
	    {ok,NewTerm};
	{ok, NewTerm} when Id =:= name_entry, is_atom(NewTerm) ->
	    {ok,NewTerm};
	{ok, NewTerm} when Id =:= keypos_entry, is_integer(NewTerm), NewTerm > 0 ->
	    {ok,NewTerm};
	_Other ->
	    NewMsg =
		case get(error_msg_mode) of
		    normal ->
			case Id of
			    node_entry ->
				["Please enter a valid node name!"];
			    name_entry ->
				["Please enter a valid table name!"];
			    keypos_entry ->
				["Please enter a valid key position!"]
			end;
		    haiku ->
			E1 = "Aborted effort",
			L  =
			    case Id of 
				node_entry ->
				    ["Reflect, repent and retype:",
				     "Enter valid node."];
				name_entry ->
				    ["Reflect, repent and retype:",
				     "Enter valid name."];
				keypos_entry ->
				    ["Reflect, repent and retype",
				     "Key position, please."]
			    end,
			[E1 | L]
		end,
	    gs:config(Id, [beep, {select, {0, 100000000}}, {setfocus, true}]),
	    tv_utils:notify(win, "TV Notification", NewMsg),
	    error
    end.


    


next_entry(node_entry, forward) ->
    name_entry;
next_entry(node_entry, backward) ->
    keypos_entry;
next_entry(name_entry, forward) ->
    keypos_entry;
next_entry(name_entry, backward) ->
    node_entry;
next_entry(keypos_entry, forward) ->
    node_entry;
next_entry(keypos_entry, backward) ->
    name_entry.




create_window(Node) ->
    gs:window(win, gs:start(), [{width, ?WIN_WIDTH},
				{height, ?WIN_HEIGHT},
				{bg, ?DEFAULT_BG_COLOR},
				{title, "[TV]   Create New ETS Table"},
				{configure, true},
				{destroy, true},
				{cursor, arrow}
			       ]),
    
    gs:frame(frame1, win, [{width, ?FRAME_WIDTH},
			   {height, ?FRAME1_HEIGHT},
			   {x, ?FRAME_X},
			   {y, ?FRAME1_Y},
			   {bg, ?DEFAULT_BG_COLOR},
			   {bw, ?BW}]),
    gs:frame(frame2, win, [{width, ?FRAME_WIDTH},
			   {height, ?FRAME2_HEIGHT},
			   {x, ?FRAME_X},
			   {y, ?FRAME2_Y},
			   {bg, ?DEFAULT_BG_COLOR},
			   {bw, ?BW}]),
    gs:frame(frame3, win, [{width, ?FRAME_WIDTH},
			   {height, ?FRAME3_HEIGHT},
			   {x, ?FRAME_X},
			   {y, ?FRAME3_Y},
			   {bg, ?DEFAULT_BG_COLOR},
			   {bw, ?BW}]),

    gs:label(frame1, [{width, ?NODE_LBL_WIDTH},
		      {height, ?LBL_HEIGHT},
		      {x, ?LBL_X},
		      {y, ?NODE_LBL_Y},
		      {bg, ?DEFAULT_BG_COLOR},
		      {fg, {0,0,0}},
		      {align, w},
		      {font, ?FONT},
		      {label, {text, "Node:"}}
		     ]),
    gs:label(frame1, [{width, ?NAME_LBL_WIDTH},
		      {height, ?LBL_HEIGHT},
		      {x, ?LBL_X},
		      {y, ?NAME_LBL_Y},
		      {bg, ?DEFAULT_BG_COLOR},
		      {fg, {0,0,0}},
		      {align, w},
		      {font, ?FONT},
		      {label, {text, "Table name:"}}
		     ]),
    gs:label(frame2, [{width, ?TYPE_LBL_WIDTH},
		      {height, ?LBL_HEIGHT},
		      {x, ?LBL_X},
		      {y, ?TYPE_LBL_Y},
		      {bg, ?DEFAULT_BG_COLOR},
		      {fg, {0,0,0}},
		      {align, w},
		      {font, ?FONT},
		      {label, {text, "Type:"}}
		     ]),
    gs:label(frame2, [{width, ?PROT_LBL_WIDTH},
		      {height, ?LBL_HEIGHT},
		      {x, ?LBL_X},
		      {y, ?PROT_LBL_Y},
		      {bg, ?DEFAULT_BG_COLOR},
		      {fg, {0,0,0}},
		      {align, w},
		      {font, ?FONT},
		      {label, {text, "Protection:"}}
		     ]),
    gs:label(frame2, [{width, ?KEYPOS_LBL_WIDTH},
		      {height, ?LBL_HEIGHT},
		      {x, ?LBL_X},
		      {y, ?KEYPOS_LBL_Y},
		      {bg, ?DEFAULT_BG_COLOR},
		      {fg, {0,0,0}},
		      {align, w},
		      {font, ?FONT},
		      {label, {text, "Key position:"}}
		     ]),
        
    gs:entry(node_entry, frame1, [{width, ?NODE_ENTRY_WIDTH},
				  {height, ?ENTRY_HEIGHT},
				  {x, ?ENTRY_X1},
				  {y, ?NODE_ENTRY_Y},
				  {bg, {255,255,255}},
				  {fg, {0,0,0}},
				  {font, ?FONT},
				  {enable, true},
				  {text, "'" ++ atom_to_list(Node) ++ "'"},
				  {keypress, true}
				 ]),
    gs:entry(name_entry, frame1, [{width, ?NAME_ENTRY_WIDTH},
				  {height, ?ENTRY_HEIGHT},
				  {x, ?ENTRY_X1},
				  {y, ?NAME_ENTRY_Y},
				  {bg, {255,255,255}},
				  {fg, {0,0,0}},
				  {font, ?FONT},
				  {enable, true},
				  {text, atom_to_list(?DEFAULT_NAME)},
				  {keypress, true},
				  {setfocus, true},
				  {select, {0,100000000}}
				 ]),
    gs:entry(keypos_entry, frame2, [{width, ?KEYPOS_ENTRY_WIDTH},
				    {height, ?ENTRY_HEIGHT},
				    {x, ?ENTRY_X2},
				    {y, ?KEYPOS_ENTRY_Y},
				    {bg, {255,255,255}},
				    {fg, {0,0,0}},
				    {font, ?FONT},
				    {enable, true},
				    {keypress, true},
				    {text, integer_to_list(?DEFAULT_KEYPOS)}
				   ]),
    
    gs:radiobutton(set, frame2, [{width, ?RBTN_WIDTH1},
				 {height, ?RBTN_HEIGHT},
				 {x, ?RBTN_X1},
				 {y, ?RBTN_Y1},
				 {align, w},
				 {label, {text, "set"}},
				 {group, type}
				 ]),
    gs:radiobutton(ordered_set, frame2, [{width, ?RBTN_WIDTH1},
					 {height, ?RBTN_HEIGHT},
					 {x, ?RBTN_X2},
					 {y, ?RBTN_Y1},
					 {align, w},
					 {label, {text, "ordered_set"}},
					 {group, type}
					]),
    gs:radiobutton(bag, frame2, [{width, ?RBTN_WIDTH1},
				 {height, ?RBTN_HEIGHT},
				 {x, ?RBTN_X1},
				 {y, ?RBTN_Y1PLUS},
				 {align, w},
				 {label, {text, "bag"}},
				 {group, type}
				 ]),
    gs:radiobutton(duplicate_bag, frame2, [{width, ?RBTN_WIDTH2},
					   {height, ?RBTN_HEIGHT},
					   {x, ?RBTN_X2},
					   {y, ?RBTN_Y1PLUS},
					   {align, w},
					   {label, {text, "duplicate_bag"}},
					   {group, type}
					  ]),
    
    gs:radiobutton(public, frame2, [{width, ?RBTN_WIDTH1},
				    {height, ?RBTN_HEIGHT},
				    {x, ?RBTN_X1},
				    {y, ?RBTN_Y2},
				    {align, w},
				    {label, {text, "public"}},
				    {group, protection}
				   ]),
    gs:radiobutton(protected, frame2, [{width, ?RBTN_WIDTH1},
				       {height, ?RBTN_HEIGHT},
				       {x, ?RBTN_X2},
				       {y, ?RBTN_Y2},
				       {align, w},
				       {label, {text, "protected"}},
				       {group, protection}
				      ]),
    gs:radiobutton(private, frame2, [{width, ?RBTN_WIDTH2},
				     {height, ?RBTN_HEIGHT},
				     {x, ?RBTN_X3},
				     {y, ?RBTN_Y2},
				     {align, w},
				     {label, {text, "private"}},
				     {group, protection}
				    ]),
    
    gs:checkbutton(named_table, frame1, [{width, ?NAMED_TABLE_CBTN_WIDTH},
					 {height, ?CBTN_HEIGHT},
					 {x, ?NAMED_TABLE_CBTN_X},
					 {y, ?NAMED_TABLE_CBTN_Y},
					 {align, w},
					 {label, {text, "Named table"}},
					 {select, false},
					 {data, true}
					]),

    gs:checkbutton(open_browser, frame3, [{width, ?OPEN_BROWSER_CBTN_WIDTH},
					  {height, ?CBTN_HEIGHT},
					  {x, ?OPEN_BROWSER_CBTN_X},
					  {y, ?OPEN_BROWSER_CBTN_Y},
					  {align, w},
					  {label, {text, "Open browser"}},
					  {select, true},
					  {data, false}
					 ]),

%%    gs:label(frame2, [{width, ?VLINE_LBL_WIDTH},
%%		      {height, ?VLINE_LBL_HEIGHT},
%%		      {x, ?VLINE_LBL_X},
%%		      {y, ?VLINE_LBL_Y1},
%%		      {bg, {0,0,0}}
%%		     ]),
%%    gs:label(frame2, [{width, ?VLINE_LBL_WIDTH},
%%		      {height, ?VLINE_LBL_HEIGHT},
%%		      {x, ?VLINE_LBL_X},
%%		      {y, ?VLINE_LBL_Y2},
%%		      {bg, {0,0,0}}
%%		     ]),
%%    gs:label(frame2, [{width, ?HLINE_LBL_WIDTH},
%%		      {height, ?HLINE_LBL_HEIGHT},
%%		      {x, ?HLINE_LBL_X},
%%		      {y, ?HLINE_LBL_Y},
%%		      {bg, {0,0,0}}
%%		     ]),
%%    
    gs:button(ok, frame3, [{width, ?BTN_WIDTH},
			   {height, ?BTN_HEIGHT},
			   {x, ?BTN_X1},
			   {y, ?BTN_Y},
			   {bg, ?DEFAULT_BG_COLOR},
			   {fg, {0,0,0}},
			   {label, {text, "OK"}}
			  ]),
    gs:button(cancel, frame3, [{width, ?BTN_WIDTH},
			       {height, ?BTN_HEIGHT},
			       {x, ?BTN_X2},
			       {y, ?BTN_Y},
			       {bg, ?DEFAULT_BG_COLOR},
			       {fg, {0,0,0}},
			       {label, {text, "Cancel"}}
			      ]),

    gs:config(?DEFAULT_TYPE, [{select, true}]),
    gs:config(?DEFAULT_PROT, [{select, true}]),
    
    gs:config(win, [{map, true}]).
    






