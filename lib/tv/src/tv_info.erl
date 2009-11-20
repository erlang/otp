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
-module(tv_info).



-export([info/6
	]).


-include("tv_int_def.hrl").
-include("tv_int_msg.hrl").




-define(DEFAULT_BG_COLOR, {217, 217, 217}).


-record(card_field_ids, {parent_pid,
			 window_id,
			 window_frame,
			 table_id,
			 table_type,
			 table_name,
			 named_table,
			 owner_pid,
			 owner_name,
			 bag_or_set,
			 arity,
			 attributes,
			 wild_pattern,
			 keypos,
			 index,
			 snmp,
			 protection,
			 size, 
			 memory,
			 storage_type,
			 disc_copies,
			 where_to_read,
			 ram_copies,
			 disc_only_copies,
			 where_to_write,
			 checkpoints,
			 node
			}).
	       


-define(WINDOW_WIDTH, 580).
-define(WINDOW_HEIGHT, 430).



-define(MNESIA_INFO_ITEMS, [type,
			    arity,
			    attributes,
			    index,
			    size,
			    memory,
			    storage_type,
			    where_to_read,
			    disc_copies,
			    disc_only_copies,
			    ram_copies,
			    where_to_write,
			    checkpoints
			   ]).




info(Master, Node, LocalNode, TabId, TabType, ErrMsgMode) ->
    process_flag(trap_exit,true),
    WinId = create_window(),
    {CardIds, MaskLabel} = init(Master, Node, LocalNode, TabId, TabType, WinId),
    put(error_msg_mode, ErrMsgMode),
    gs:config(WinId, [{map, true}]),
    loop(CardIds, MaskLabel, Node, LocalNode, TabId, TabType).






create_window() ->
    WinWidth = ?WINDOW_WIDTH,
    WinHeight = ?WINDOW_HEIGHT,
    Win = gs:window(win, gs:start(), [{width, WinWidth},
				      {height, WinHeight},
				      {bg, ?DEFAULT_BG_COLOR},
				      {destroy, true},
				      {configure, true},
				      {keypress, true}
				     ]),

    MenubarId = gs:create(menubar, Win, [{bg, ?DEFAULT_BG_COLOR}
					]),
    Mbutt = gs:create(menubutton, MenubarId, [{bg, ?DEFAULT_BG_COLOR},
					      {fg, {178, 34, 34}},  % firebrick
					      {label, {text, " File "}},
					      {underline, 1}
					     ]),
    Obutt = gs:create(menubutton, MenubarId, [{bg, ?DEFAULT_BG_COLOR},
					      {fg, {178, 34, 34}},  % firebrick
					      {label, {text, " Options "}},
					      {underline, 1}
					     ]),

       % Create the actual menu!
    FMenu = gs:create(menu, Mbutt, [{bg, ?DEFAULT_BG_COLOR},
				   {fg, {178, 34, 34}}]), 
    OMenu = gs:create(menu, Obutt, [{bg, ?DEFAULT_BG_COLOR},
				   {fg, {178, 34, 34}}]), 
    gs:create(menuitem, FMenu, [{bg, ?DEFAULT_BG_COLOR},
				{fg, {178, 34, 34}},
				{label, {text, " Close    Ctrl-C "}},
				{data, close_menu},
				{underline, 1}
			       ]),
    gs:create(menuitem, OMenu, [{bg, ?DEFAULT_BG_COLOR},
				{fg, {178, 34, 34}},
				{label, {text, " Refresh    Ctrl-R "}},
				{data, update},
				{underline, 1}
			       ]),
    Win.

    






init(Master, Node, LocalNode, TabId, TabType, WinId) ->
    WinWidth  = ?WINDOW_WIDTH,
    WinHeight = ?WINDOW_HEIGHT,

    WinFrame = gs:frame(WinId, [{width, WinWidth},
				{height, WinHeight},
				{x, 0},
				{y, 30},
				{bg, ?DEFAULT_BG_COLOR},
				{bw, 0}
			       ]),

    TableIdFlap       = create_flap(1, "Table Id", WinFrame),
    BasicSettingsFlap = create_flap(2, "Basic Settings", WinFrame),
    SizeFlap          = create_flap(3, "Size", WinFrame),
    StorageFlap       = create_flap(4, "Storage", WinFrame),

    TableIdCard       = create_card(WinFrame, TableIdFlap),
    BasicSettingsCard = create_card(WinFrame, BasicSettingsFlap),
    SizeCard          = create_card(WinFrame, SizeFlap),
    StorageCard       = create_card(WinFrame, StorageFlap),
        

    set_flap_label(TableIdFlap, "Table Id"),
    set_flap_label(BasicSettingsFlap, "Basic Settings"),
    set_flap_label(SizeFlap, "Size"),
    set_flap_label(StorageFlap, "Storage"),


    gs:config(TableIdCard, [raise]),

    CardIds = print_cards(TabType, TableIdCard, BasicSettingsCard, SizeCard, StorageCard),

    {_CardId, FirstMaskXpos} = gs:read(TableIdFlap, data),
    Mask = gs:label(WinFrame, [{width, gs:read(TableIdFlap, width) - 2 * gs:read(TableIdFlap, bw) + 1},
			       {height, gs:read(TableIdCard, bw)},
			       {x, FirstMaskXpos},
			       {y, gs:read(TableIdCard, y)},
			       {bg, ?DEFAULT_BG_COLOR}
			      ]),

    update_info_flaps(TabType, Node, LocalNode, TabId, CardIds, Master),
    {CardIds#card_field_ids{parent_pid   = Master,
			    window_id    = WinId, 
			    window_frame = WinFrame}, Mask}.

    



check_node(OldNode, LocalNode) ->
    HomeNode = node(),
    case net_adm:ping(OldNode) of
	pong ->
	    OldNode;
	pang when LocalNode ->  
	       %% The system has gone either distributed or undistributed.
	       %% No matter which, HomeNode tells the current correct node.
	    HomeNode;
	pang ->
	    OldNode
    end.






update_data_field(notext, {label, Id}) ->
    gs:config(Id, [{label, {text, "" }}]);
update_data_field(notext, {listbox, Id}) ->
    gs:config(Id, [{items, []}]);
update_data_field({Data}, {label, Id}) ->
    gs:config(Id, [{label, {text, " " ++ lists:flatten(io_lib:write(Data))}}]);
update_data_field({Data}, {listbox, Id}) ->
    gs:config(Id, [{items, lists:map(fun(E) -> " " ++ lists:flatten(io_lib:write(E)) 
				     end, Data)}]).

    

    
print_info(mnesia, Node, LocalNode, TabId, CardIds) ->
    update_data_field({mnesia}, 
		      CardIds#card_field_ids.table_type),
    update_data_field({TabId}, 
		      CardIds#card_field_ids.table_name),
    update_data_field({tv_mnesia_rpc:table_info(Node, LocalNode, TabId, type)}, 
		      CardIds#card_field_ids.bag_or_set),
    update_data_field({tv_mnesia_rpc:table_info(Node, LocalNode, TabId, arity) - 1}, 
		      CardIds#card_field_ids.arity),

    AttributesList = tv_mnesia_rpc:table_info(Node, LocalNode, TabId, attributes),
    update_data_field({AttributesList}, 
		      CardIds#card_field_ids.attributes),
    update_data_field({lists:map(fun(N) -> 
					 lists:nth(N - 1, AttributesList) 
				 end, 
				 [2] ++ tv_mnesia_rpc:table_info(Node, 
								 LocalNode, 
								 TabId, 
								 index)
				)
		      }, 
		      CardIds#card_field_ids.index),

    update_data_field({tv_mnesia_rpc:table_info(Node, LocalNode, TabId, size)}, 
		      CardIds#card_field_ids.size),
    update_data_field({tv_mnesia_rpc:table_info(Node, LocalNode, TabId, memory)}, 
		      CardIds#card_field_ids.memory),
    update_data_field({tv_mnesia_rpc:table_info(Node, LocalNode, TabId, storage_type)}, 
		      CardIds#card_field_ids.storage_type),
    update_data_field({tv_mnesia_rpc:table_info(Node, LocalNode, TabId, where_to_read)}, 
		      CardIds#card_field_ids.where_to_read),
    update_data_field({tv_mnesia_rpc:table_info(Node, LocalNode, TabId, disc_copies)}, 
		      CardIds#card_field_ids.disc_copies),
    update_data_field({tv_mnesia_rpc:table_info(Node, LocalNode, TabId, disc_only_copies)}, 
		      CardIds#card_field_ids.disc_only_copies),
    update_data_field({tv_mnesia_rpc:table_info(Node, LocalNode, TabId, ram_copies)}, 
		      CardIds#card_field_ids.ram_copies),
    update_data_field({tv_mnesia_rpc:table_info(Node, LocalNode, TabId, where_to_write)}, 
		      CardIds#card_field_ids.where_to_write),
    update_data_field({tv_mnesia_rpc:table_info(Node, LocalNode, TabId, checkpoints)}, 
		      CardIds#card_field_ids.checkpoints),
    {ok, TabId};
print_info(ets, Node, LocalNode, TabId, CardIds) ->
    update_data_field({ets}, 
		      CardIds#card_field_ids.table_type),
    update_data_field({TabId}, 
		      CardIds#card_field_ids.table_id),
    TabName = tv_ets_rpc:info(Node, LocalNode, TabId, name),
    update_data_field({TabName}, 
		      CardIds#card_field_ids.table_name),
    update_data_field({tv_ets_rpc:info(Node, LocalNode, TabId, named_table)}, 
		      CardIds#card_field_ids.named_table),

    OwnerPid              = tv_ets_rpc:info(Node, LocalNode, TabId, owner),
    OwnerNameSearchResult = lists:keysearch(registered_name, 
					    1, 
					    rpc:block_call(Node, 
							   erlang, 
							   process_info, 
							   [OwnerPid])),
    OwnerName             = case OwnerNameSearchResult of
				false ->
				    notext;
				{value, {registered_name, WantedName}} ->
				    {WantedName}
			    end,
    update_data_field({OwnerPid}, 
		      CardIds#card_field_ids.owner_pid),
    update_data_field(OwnerName, 
		      CardIds#card_field_ids.owner_name),
    
    update_data_field({tv_ets_rpc:info(Node, LocalNode, TabId, keypos)},  
		      CardIds#card_field_ids.keypos),
    update_data_field({tv_ets_rpc:info(Node, LocalNode, TabId, type)},  
		      CardIds#card_field_ids.bag_or_set),
    update_data_field({tv_ets_rpc:info(Node, LocalNode, TabId, protection)},  
		      CardIds#card_field_ids.protection),
    update_data_field({tv_ets_rpc:info(Node, LocalNode, TabId, size)}, 
		      CardIds#card_field_ids.size),
    update_data_field({tv_ets_rpc:info(Node, LocalNode, TabId, memory)},  
		      CardIds#card_field_ids.memory),
    update_data_field({tv_ets_rpc:info(Node, LocalNode, TabId, node)}, 
		      CardIds#card_field_ids.node),
    {ok, TabName}.
    





print_cards(mnesia, Card1, Card2, Card3, Card4) ->
    create_card_text(1, "Table Type:", Card1),
    create_card_text(2, "Table Name:", Card1),
    
    create_card_text(1, "Table Type:", Card2),
    create_card_text(2, "Number of Attributes:", Card2),
%    create_card_text(3, "Attribute Names:", Card2),
%    create_card_text(4, "Index Positions:", Card2),

    create_card_text(1, "Number of Elements Stored:", Card3),
    create_card_text(2, "Number of Words Allocated:", Card3),

    create_card_text(1, "Local Storage Type:", Card4),
    create_card_text(2, "Table Readable at Node:", Card4),
%    create_card_text(3, "Disc Copy Nodes:", Card4),
%    create_card_text(4, "Disc Copy Only Nodes:", Card4),
%    create_card_text(5, "RAM Copy Nodes:", Card4),
%    create_card_text(6, "Active Table Replica Nodes:", Card4),
%    create_card_text(7, "Active Checkpoints:", Card4),
    
    {AttributesId, IndexId} = create_special_fields(mnesia, size_card, Card2),

    
    {DiscCopiesId, DiscOnlyCopiesId, RamCopiesId, WhereToWriteId, CheckpointsId} = 
	create_special_fields(mnesia, storage_card, Card4),

    #card_field_ids{table_name       = {label, create_card_data_field(2, Card1)},
		    table_type       = {label, create_card_data_field(1, Card1)},
		    bag_or_set       = {label, create_card_data_field(1, Card2)},
		    arity            = {label, create_card_data_field(2, Card2)},
		    attributes       = AttributesId,
		    index            = IndexId,
		    size             = {label, create_card_data_field(1, Card3)},
		    memory           = {label, create_card_data_field(2, Card3)},
		    storage_type     = {label, create_card_data_field(1, Card4)},
		    where_to_read    = {label, create_card_data_field(2, Card4)},
		    disc_copies      = DiscCopiesId,
		    disc_only_copies = DiscOnlyCopiesId,
		    ram_copies       = RamCopiesId,
		    where_to_write   = WhereToWriteId,
		    checkpoints      = CheckpointsId
		   };
print_cards(ets, Card1, Card2, Card3, Card4) ->
    create_card_text(1, "Table Type:", Card1),
    create_card_text(2, "Table Id:", Card1),
    create_card_text(3, "Table Name:", Card1),
    create_card_text(4, "Table Name Registered:", Card1),
    create_card_text(5, "Process Owning the Table:", Card1),
    create_card_text(6, "Name of Owning Process:", Card1),

    create_card_text(1, "Index Position:", Card2),
    create_card_text(2, "Table Type:", Card2),
    create_card_text(3, "Protection Mode:", Card2),
    
    create_card_text(1, "Number of Elements Stored:", Card3),
    create_card_text(2, "Number of Words Allocated:", Card3),
    
    create_card_text(1, "Table Stored at Node:", Card4),
    
    #card_field_ids{table_id    = {label, create_card_data_field(2, Card1)},
		    table_type  = {label, create_card_data_field(1, Card1)},
		    table_name  = {label, create_card_data_field(3, Card1)},
		    named_table = {label, create_card_data_field(4, Card1)},
		    owner_pid   = {label, create_card_data_field(5, Card1)},
		    owner_name  = {label, create_card_data_field(6, Card1)},
		    	       			      
		    keypos      = {label, create_card_data_field(1, Card2)},
		    bag_or_set  = {label, create_card_data_field(2, Card2)},
		    protection  = {label, create_card_data_field(3, Card2)},
			       			      
		    size        = {label, create_card_data_field(1, Card3)},
		    memory      = {label, create_card_data_field(2, Card3)},
			       			      
		    node        = {label, create_card_data_field(1, Card4)}
		   }.



    


create_special_fields(mnesia, size_card, CardId) ->
    LabelWidth      = 195,
    LabelHeight     = 24,
    ListboxWidth    = 210,
    ListboxHeight   = 160,
    VerticalSpacing = 20,
    LXpos           = 30,
    RXpos           = 330,
    Ypos            = 40 + (LabelHeight + VerticalSpacing) * 2 + 25,
    gs:label(CardId, [{width, LabelWidth},
		      {height, LabelHeight},
		      {x, LXpos},
		      {y, Ypos},
		      {bg, ?DEFAULT_BG_COLOR},
		      {fg, {0, 0, 0}},
		      {align, center},
		      {label, {text, "Attribute Names:"}}
		    ]),
    
    gs:label(CardId, [{width, LabelWidth},
		      {height, LabelHeight},
		      {x, RXpos},
		      {y, Ypos},
		      {bg, ?DEFAULT_BG_COLOR},
		      {fg, {0, 0, 0}},
		      {align, center},
		      {label, {text, "Attributes Used as Indices:"}}
		     ]),
    
    AttributesId = gs:listbox(CardId, [{width, ListboxWidth},
				       {height, ListboxHeight},
				       {x, LXpos},
				       {y, Ypos + LabelHeight - 3},
				       {bg, {255, 255, 255}},
				       {fg, {0, 0, 0}},
				       {hscroll, bottom},
				       {vscroll, right},
				       {selectmode, single},
				       {click, true},
				       {doubleclick, true},
				       {data, listbox}
				      ]),
    
    IndexId = gs:listbox(CardId, [{width, ListboxWidth},
				  {height, ListboxHeight},
				  {x, RXpos},
				  {y, Ypos + LabelHeight - 3},
				  {bg, {255, 255, 255}},
				  {fg, {0, 0, 0}},
				  {hscroll, bottom},
				  {vscroll, right},
				  {selectmode, single},
				  {click, true},
				  {doubleclick, true},
				  {data, listbox}				  
				]),
    
    {{listbox, AttributesId}, 
     {listbox, IndexId}
    };
create_special_fields(mnesia, storage_card, CardId) ->
    LabelWidth      = 155,
    LabelHeight     = 24,
    ListboxHeight   = 80,
    ListboxWidth    = 170,
    VerticalSpacing = 20,
    LXpos           = 10,
    MXpos           = 197,
    RXpos           = 385,
    % Y-positions for upper and lower row.
    UYpos           = 40 + (LabelHeight + VerticalSpacing) * 2,
    LYpos           = UYpos +  ListboxHeight + 37,
    gs:label(CardId, [{width, LabelWidth},
		      {height, LabelHeight},
		      {x, LXpos},
		      {y, UYpos},
		      {bg, ?DEFAULT_BG_COLOR},
		      {fg, {0, 0, 0}},
		      {align, center},
		      {label, {text, "Disc Copy Nodes:"}}
		     ]),

    gs:label(CardId, [{width, LabelWidth},
		      {height, LabelHeight},
		      {x, MXpos},
		      {y, UYpos},
		      {bg, ?DEFAULT_BG_COLOR},
		      {fg, {0, 0, 0}},
		      {align, center},
		      {label, {text, "Disc Only Copy Nodes:"}}
		     ]),
    
    gs:label(CardId, [{width, LabelWidth},
		      {height, LabelHeight},
		      {x, RXpos},
		      {y, UYpos},
		      {bg, ?DEFAULT_BG_COLOR},
		      {fg, {0, 0, 0}},
		      {align, center},
		      {label, {text, "RAM Copy Nodes:"}}
		     ]),


    gs:label(CardId, [{width, LabelWidth},
		      {height, LabelHeight},
		      {x, LXpos},
		      {y, LYpos},
		      {bg, ?DEFAULT_BG_COLOR},
		      {fg, {0, 0, 0}},
		      {align, center},
		      {label, {text, "Table Replica Nodes:"}}
		     ]),

    gs:label(CardId, [{width, LabelWidth},
		      {height, LabelHeight},
		      {x, MXpos},
		      {y, LYpos},
		      {bg, ?DEFAULT_BG_COLOR},
		      {fg, {0, 0, 0}},
		      {align, center},
		      {label, {text, "Active Checkpoints:"}}
		    ]),
    

    DiscCopiesId = gs:listbox(CardId, [{width, ListboxWidth},
				       {height, ListboxHeight},
				       {x, LXpos},
				       {y, UYpos + LabelHeight - 3},
				       {bg, {255, 255, 255}},
				       {fg, {0, 0, 0}},
				       {hscroll, bottom},
				       {vscroll, right},
				       {selectmode, single},
				       {click, true},
				       {doubleclick, true},
				       {data, listbox}				       
				      ]),
    
    DiscCopiesOnlyId = gs:listbox(CardId, [{width, ListboxWidth},
					   {height, ListboxHeight},
					   {x, MXpos},
					   {y, UYpos + LabelHeight - 3},
					   {bg, {255, 255, 255}},
					   {fg, {0, 0, 0}},
					   {hscroll, bottom},
					   {vscroll, right},
					   {selectmode, single},
					   {click, true},
					   {doubleclick, true},
					   {data, listbox}
					  ]),

    RamCopiesId = gs:listbox(CardId, [{width, ListboxWidth},
				      {height, ListboxHeight},
				      {x, RXpos},
				      {y, UYpos + LabelHeight - 3},
				      {bg, {255, 255, 255}},
				      {fg, {0, 0, 0}},
				      {hscroll, bottom},
				      {vscroll, right},
				      {selectmode, single},
				      {click, true},
				      {doubleclick, true},
				      {data, listbox}
				     ]),
    


    WhereToWriteId = gs:listbox(CardId, [{width, ListboxWidth},
					 {height, ListboxHeight},
					 {x, LXpos},
					 {y, LYpos + LabelHeight - 3},
					 {bg, {255, 255, 255}},
					 {fg, {0, 0, 0}},
					 {hscroll, bottom},
					 {vscroll, right},
					 {selectmode, single},
					 {click, true},
					 {doubleclick, true},
					 {data, listbox}
					]),
    
    CheckpointsId = gs:listbox(CardId, [{width, ListboxWidth},
					{height, ListboxHeight},
					{x, MXpos},
					{y, LYpos + LabelHeight - 3},
					{bg, {255, 255, 255}},
					{fg, {0, 0, 0}},
					{hscroll, bottom},
					{vscroll, right},
					{selectmode, single},
					{click, true},
					{doubleclick, true},
					{data, listbox}
				       ]),
    
    {{listbox, DiscCopiesId}, 
     {listbox, DiscCopiesOnlyId}, 
     {listbox, RamCopiesId}, 
     {listbox, WhereToWriteId}, 
     {listbox, CheckpointsId}
    }.







create_card_data_field(N, ParentId) ->
    Width           = 345,
    Height          = 24,
    VerticalSpacing = 20,
    Xpos            = 210,
    Ypos            = 40 + (Height + VerticalSpacing) * (N - 1),

    BgFrame = gs:frame(ParentId, [{width, Width},
				  {height, Height},
				  {x, Xpos},
				  {y, Ypos},
				  {bg, {0, 0, 0}},
				  {bw, 0}
				 ]),
    gs:label(BgFrame, [{width, Width - 2},
		       {height, Height - 2},
		       {x, 1},
		       {y, 1},
		       {bg, {255, 255, 255}},
		       {fg, {0, 0, 0}},
		       {align, w}
		      ]).






create_card_text(N, Text, ParentId) ->
    LabelWidth      = 205,
    LabelHeight     = 24,
    VerticalSpacing = 20,
    Xpos            = 10,
    Ypos            = 40 + (LabelHeight + VerticalSpacing) * (N - 1),
    gs:label(ParentId, [{width, LabelWidth},
			{height, LabelHeight},
			{x, Xpos},
			{y, Ypos},
			{bg, ?DEFAULT_BG_COLOR},
			{fg, {0, 0, 0}},
			{align, w},
			{label, {text, Text}}
		       ]).
    



create_card(ParentId, FlapId) ->
    CardId = gs:frame(ParentId, [{width, 570},
				 {height, 360},
				 {x, 5},
				 {y, 35},
				 {bg, ?DEFAULT_BG_COLOR},
				 {bw, 2}
				]),
    FlapXpos = gs:read(FlapId, data),
    gs:config(FlapId, [{data, {CardId, FlapXpos}}
		      ]),
    CardId.
    
    



set_flap_label(ParentId, Text) ->
    Bw     = gs:read(ParentId, bw),     % It is assumed that the parent is a frame!  :-)
    Width  = gs:read(ParentId, width) - 2 * Bw - 2,
    Height = gs:read(ParentId, height) - 2 * Bw - 6,
    Xpos   = 0,
    Ypos   = 0,
    Data   = gs:read(ParentId, data),
    
    gs:label(ParentId, [{width, Width},
			{height, Height},
			{x, Xpos},
			{y, Ypos},
			% {fg, {178, 34, 34}},
			{bg, ?DEFAULT_BG_COLOR},
			{fg, {0, 0, 0}},
			{label, {text, Text}},
			{align, center},
			{buttonpress, true},
			{data, Data}
		       ]).



create_flap(N, _Text, ParentId) ->
    Width       = 120,
    Height      = 40,
    Spacing     = 2,
    FirstXpos   = 5,
    Xpos        = FirstXpos + ((Width + Spacing) * (N - 1)),
    Ypos        = 5,
    BorderWidth = 2,
    
    gs:frame(ParentId, [{width, Width},
			{height, Height},
			{x, Xpos},
			{y, Ypos},
			{bg, ?DEFAULT_BG_COLOR},
			{bw, BorderWidth},
			{cursor, hand},
			{buttonpress, true},
			{data, Xpos + BorderWidth}
		       ]).
				   


update_info_flaps(TabType, Node, LocalNode, TabId, CardIds, MasterPid) ->
    case catch print_info(TabType, Node, LocalNode, TabId, CardIds) of
	{ok, TabName} ->
	    WinTitle = tv_pc_menu_handling:get_window_title(TabType,Node,TabId,TabName),
	    gs:config(win, [{title, "[TV]   " ++ WinTitle}]),
	    done;
	nodedown ->
	    nodedown;
	no_table ->
	    gs:config(win, [beep]),
	    case get(error_msg_mode) of
		normal ->
		    Msg = ["The table " ++ lists:flatten(io_lib:write(TabId)) ++ " on node",
			   lists:flatten(io_lib:write(Node)) ++ " no longer exists!"],
		    tv_utils:notify(win, "TV Notification", Msg);
		haiku ->
		    Msg = ["Three things are certain:",
			   "Death, taxes, and lost tables.",
			   "Guess which has occurred."],
		    tv_utils:notify(win, "TV Notification", Msg)
	    end,
	    MasterPid ! #ip_dead_table{sender = self()};
	mnesia_not_started ->
	    gs:config(win, [beep]),
	    case get(error_msg_mode) of
		normal ->
		    Msg = ["The table " ++ lists:flatten(io_lib:write(TabId)) ++ " on node",
			   lists:flatten(io_lib:write(Node)) ++ " no longer exists!"],
		    tv_utils:notify(win, "TV Notification", Msg);
		haiku ->
		    Msg = ["A table that big?",
			   "It might be very useful.",
			   "But now it is gone."],
		    tv_utils:notify(win, "TV Notification", Msg)
	    end,
	    MasterPid ! #ip_dead_table{sender = self()};
	{unexpected_error,Reason} ->
	    io:format("Unexpected error:  ~p~n", [Reason]);
	_Other ->
	    io:format("Unexpected return value: ~p~n", [_Other]),
	    done
    end.



    
loop(CardIds, MaskLabel, Node, LocalNode, TabId, TabType) ->				 
    receive
	#info_update_table_info{sender = Sender} ->
	    NewNode = check_node(Node, LocalNode),
	    update_info_flaps(TabType, NewNode, LocalNode, TabId, CardIds, Sender),
	    loop(CardIds, MaskLabel, NewNode, LocalNode, TabId, TabType);


	#info_raise_window{sender = Sender} ->
	    gs:config(CardIds#card_field_ids.window_id, [raise]),
	    NewNode = check_node(Node, LocalNode),
	    chk(update_info_flaps(TabType, NewNode, LocalNode, TabId, CardIds, Sender)),
	    loop(CardIds, MaskLabel, NewNode, LocalNode, TabId, TabType);
	

	#info_quit{} ->
	    exit(normal);

	{gs, _FlapId, buttonpress, {CardId, Xpos}, [1 | _]} ->
	    gs:config(CardId, [raise
			      ]),
	    gs:config(MaskLabel, [raise,
				  {x, Xpos}
				 ]),
	    loop(CardIds, MaskLabel, Node, LocalNode, TabId, TabType);

	{gs, _Id, buttonpress, {_CardId, _Xpos}, _Args} ->
	    loop(CardIds, MaskLabel, Node, LocalNode, TabId, TabType);

	{gs, _LblId, enter, _Data, _Args} ->
	    loop(CardIds, MaskLabel, Node, LocalNode, TabId, TabType);
	
	{gs, WinId, configure, _Data, _Args} ->
	    gs:config(WinId, [{width, ?WINDOW_WIDTH},
			      {height, ?WINDOW_HEIGHT}
			     ]),
	    loop(CardIds, MaskLabel, Node, LocalNode, TabId, TabType);

	{gs, ListboxId, click, listbox, _Args} ->
	    gs:config(ListboxId, [{selection, clear}]),
	    loop(CardIds, MaskLabel, Node, LocalNode, TabId, TabType);

	{gs, ListboxId, doubleclick, listbox, _Args} ->
	    gs:config(ListboxId, [{selection, clear}]),
	    loop(CardIds, MaskLabel, Node, LocalNode, TabId, TabType);

	{gs, _Id, click, update, _Args} ->
	    NewNode = check_node(Node, LocalNode),
	    chk(update_info_flaps(TabType,NewNode,LocalNode,TabId,CardIds,
				  CardIds#card_field_ids.parent_pid)),
	    loop(CardIds, MaskLabel, NewNode, LocalNode, TabId, TabType);

	{gs, _Id, keypress, _, [r, _, 0, 1 | _]} ->
	    NewNode = check_node(Node, LocalNode),
	    chk(update_info_flaps(TabType,NewNode,LocalNode,TabId,CardIds,
				  CardIds#card_field_ids.parent_pid)),
	    loop(CardIds, MaskLabel, NewNode, LocalNode, TabId, TabType);

	{gs, _Id, keypress, _, ['R', _, 1, 1 | _]} ->
	    NewNode = check_node(Node, LocalNode),
	    chk(update_info_flaps(TabType,NewNode,LocalNode,TabId,CardIds,
				  CardIds#card_field_ids.parent_pid)),
	    loop(CardIds, MaskLabel, NewNode, LocalNode, TabId, TabType);

	{gs, _Id, click, close_menu, _Args} ->
	    exit(normal);

	{gs, _Id, keypress, _, [c, _, 0, 1 | _]} ->
	    exit(normal);
	
	{gs, _Id, keypress, _, ['C', _, 1, 1 | _]} ->
	    exit(normal);

	{gs, _Id, destroy, _Data, _Args} ->
	    exit(normal);

	{'EXIT', _Pid, _Reason} ->
	    exit(normal);

	{error_msg_mode, Mode} ->
	    put(error_msg_mode, Mode),
	    loop(CardIds, MaskLabel, Node, LocalNode, TabId, TabType);

	_Other ->
	    loop(CardIds, MaskLabel, Node, LocalNode, TabId, TabType)  
    end.
    





chk(nodedown) ->
    gs:config(win, [beep]),
    case get(error_msg_mode) of
	normal ->
	    tv_utils:notify(win, "TV Notification", 
			    ["The node is down, and the",
			     "table cannot be reached."]);
	haiku ->
	    ErrMsg1 = ["With searching comes loss",
		       "And the presence of absence:",
		       "Node is down."],
	    tv_utils:notify(win, "TV Notification", ErrMsg1)
    end;
chk(_Other) ->
    done.
    
