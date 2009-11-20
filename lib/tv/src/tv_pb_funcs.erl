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
-module(tv_pb_funcs).



-export([init_btns/10, 
	 update_hbtns/3, 
	 update_vbtns/5, 
	 update_keys/2, 
	 set_new_sort_col/2]).


-include("tv_int_def.hrl").
-include("tv_pb_int_def.hrl").







%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************




%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================



init_btns(ParentId, Ypos, HbtnH, 
	  VbtnW, ResbtnW, FirstColShown, ColsShown, NofRows, RowH, ProcVars) ->

    #process_variables{key_numbers = KeyNos,
		       key_ids     = KeyIds} = ProcVars,

%    C = gs:canvas(ParentId, [{width, VbtnW - 1},
%			     {height, HbtnH},
%			     {x, 0},
%			     {y, HbtnH + 1},
%			     {bg, white}
%			    ]),
%    gs:create(image, C, [{load_gif, "erlang.gif"}]),

    {HbtnsShown, ResBtnsShown} = update_hbtns(ColsShown, [], [], 
					      FirstColShown, ParentId, Ypos,
					      HbtnH, ResbtnW, VbtnW),

    NewKeyIds = update_keys(KeyNos, KeyIds, FirstColShown, 
			    FirstColShown + length(ColsShown) - 1, HbtnsShown, 
			    ParentId, []),

    VbtnsShown = create_vbtns(ParentId, Ypos, NofRows, RowH, VbtnW, HbtnH),
    ProcVars#process_variables{grid_frame_id   = ParentId,
			       ypos            = Ypos,
			       hbtn_height     = HbtnH,
			       vbtn_width      = VbtnW,
			       resbtn_width    = ResbtnW,
			       first_col_shown = FirstColShown,
			       hbtns_shown     = HbtnsShown,
			       resbtns_shown   = ResBtnsShown,
			       vbtns_shown     = VbtnsShown,
			       cols_shown      = ColsShown,
			       key_ids         = NewKeyIds
			      }.










%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_hbtns(FirstColShown, ColsShown, ProcVars) ->
    #process_variables{grid_frame_id   = ParentId,
		       first_col_shown = OldFirstColShown,
		       cols_shown      = OldColsShown,
		       ypos            = Ypos,
		       hbtn_height     = HbtnH,
		       vbtn_width      = VbtnW,
		       resbtn_width    = ResbtnW,
		       hbtns_shown     = HbtnsShown,
		       resbtns_shown   = ResbtnsShown,
		       key_numbers     = KeyNos,
		       key_ids         = KeyIds,
		       col_mark_params = ColMarkP}  = ProcVars,

       % Only if the grid has been scrolled horizontally need we move the
       % col mark!
    case FirstColShown of
	OldFirstColShown ->
	    done;
	_NewValue ->
	    #col_mark_params{col_btn_id         = MarkedBtnId,
			     virtual_col_marked = ColMarked,
			     sort_btn_id        = SortBtnId,
			     virtual_sort_col   = SortCol}  = ColMarkP,
	    unmark_marked_col(MarkedBtnId, ColMarked, SortCol),
	    unmark_sort_col(SortBtnId, ColMarked, SortCol)
    end,

    {NewHbtns, NewResbtns, NewKeys} = 
	case {FirstColShown, ColsShown} of
	    {OldFirstColShown, OldColsShown} ->
		{HbtnsShown, ResbtnsShown, KeyIds};
	    _Other ->
		{NewHbtnsShown, NewResbtnsShown} = update_hbtns(ColsShown, 
								HbtnsShown, 
								ResbtnsShown, 
								FirstColShown, 
								ParentId, 
								Ypos, 
								HbtnH, 
								ResbtnW,
								VbtnW),
		NewKeyIds = update_keys(KeyNos, KeyIds, FirstColShown, 
					FirstColShown + length(ColsShown) - 1, 
					NewHbtnsShown, ParentId, []),
		{NewHbtnsShown, NewResbtnsShown, NewKeyIds}
	end,
	    
       % Now mark the marked column again!
    NewColMarkP = mark_marked_col(NewHbtns, FirstColShown, ColMarkP),

    ProcVars#process_variables{first_col_shown = FirstColShown,
			       hbtns_shown     = NewHbtns,
			       resbtns_shown   = NewResbtns,
			       cols_shown      = ColsShown,
			       key_ids         = NewKeys,
			       col_mark_params = NewColMarkP
			      }.
    
    
		       






%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_vbtns(NofRowsShown, FirstRowShown, Colors, BlinkEnabled, ProcVars) ->
    #process_variables{vbtns_shown      = Vbtns,
		       blink_color_list = BlinkList} = ProcVars,
    
    update_vbtns(1, NofRowsShown, FirstRowShown, Vbtns, Colors, BlinkEnabled, BlinkList),
    NewProcVars = update_sort_btn_mark(ProcVars),
    NewProcVars.










%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


set_new_sort_col(SortCol, ProcVars) ->
    #process_variables{hbtns_shown     = HbtnsShown,
		       col_mark_params = ColMarkP} = ProcVars,
    
    #col_mark_params{col_btn_id         = MarkedColBtnId,
		     sort_btn_id        = OldSortBtnId}  = ColMarkP,
    
       % Set the new color of the sort btn, and remove the mark, if it is the same
       % column!

    case MarkedColBtnId of
	undefined ->
	    done;
	_AnyId ->
	    gs:config(MarkedColBtnId, [{bg, ?DEFAULT_BG_COLOR},
				       {fg, {0, 0, 0}}
				      ])
    end,
    
    SortBtnId = get_btn_id(SortCol, HbtnsShown),
    case SortBtnId of 
	undefined ->
	       % The btn isn't visible, or no sorting shall be performed!
	    gs:config(OldSortBtnId, [{bg, ?DEFAULT_BG_COLOR},
				     {fg, {0, 0, 0}}
				    ]);
	_Other ->
	       % Unmark the old sort btn id!
	    gs:config(OldSortBtnId, [{bg, ?DEFAULT_BG_COLOR},
				     {fg, {0, 0, 0}}
				    ]),
	    gs:config(SortBtnId, [{bg, ?SORT_MARK_COLOR},
				  {fg, {0, 0, 0}}
				 ])
    end,
    
    NewColMarkP = ColMarkP#col_mark_params{col_btn_id         = undefined,
					   virtual_col_marked = undefined,
					   sort_btn_id        = SortBtnId,
					   virtual_sort_col   = SortCol
					  },
    ProcVars#process_variables{col_mark_params = NewColMarkP}.
    










%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_keys(KeyList, ProcVars) ->
    #process_variables{key_numbers     = OldKeyList,
		       key_ids         = KeyIds,
		       first_col_shown = FirstColShown,
		       cols_shown      = ColsShown,
		       hbtns_shown     = HbtnsShown,
		       grid_frame_id   = ParentId} = ProcVars,

    NewKeyIds = case KeyList of
		    OldKeyList ->
			KeyIds;
		    NewKeyList ->
			update_keys(NewKeyList, KeyIds, FirstColShown, 
				    FirstColShown + length(ColsShown) - 1, 
				    HbtnsShown, ParentId, [])
		end,

    ProcVars#process_variables{key_numbers = KeyList,
			       key_ids     = NewKeyIds
			      }.








%%%*********************************************************************
%%% INTERNAL FUNCTIONS
%%%*********************************************************************




%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


unmark_sort_col(undefined, _ColMarked, _SortCol) ->
    done;
unmark_sort_col(SortBtnId, _ColMarked, _SortCol) ->
    gs:config(SortBtnId, [{bg, ?DEFAULT_BG_COLOR},
			  {fg, {0, 0, 0}}
			 ]).







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


mark_marked_col(HbtnsShown, _FirstColShown, ColMarkP) ->
    #col_mark_params{virtual_col_marked = VirtualCol,
		     virtual_sort_col   = SortCol} = ColMarkP,

    {NewMarkBtnId, NewSortBtnId} = 
	case VirtualCol of
	    SortCol ->
		   % Same btn!
		BtnId = get_btn_id(VirtualCol, 
				   HbtnsShown),
		gs:config(BtnId, [{bg, ?SORT_MARK_COLOR},
				  {fg, {0, 0, 0}}
				 ]),
		{BtnId, BtnId};
	    _OtherCol ->
		MarkBtnId = get_btn_id(VirtualCol, HbtnsShown),
		case MarkBtnId of
		    undefined ->
			done;
		    _Else ->
			gs:config(MarkBtnId, [{bg, ?COL_MARK_COLOR},
					      {fg, {255, 255, 255}}
					     ])
		end,

		SortBtnId = get_btn_id(SortCol, HbtnsShown),
		case SortBtnId of
		    undefined ->
			done;
		    _OtherId ->
			gs:config(SortBtnId, [{bg, ?SORT_MARK_COLOR},
					      {fg, {0, 0, 0}}
					     ])
		end,

		{MarkBtnId, SortBtnId}
	end,
    
    ColMarkP#col_mark_params{col_btn_id = NewMarkBtnId,
			     sort_btn_id = NewSortBtnId}.
    





%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


unmark_marked_col(undefined, _ColMarked, _SortCol) ->
    done;
unmark_marked_col(BtnId, _ColMarked, _SortCol) ->
    gs:config(BtnId, [{bg, ?DEFAULT_BG_COLOR},
		      {fg, {0,0,0}}
		     ]).







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_sort_btn_mark(ProcVars) ->
    #process_variables{hbtns_shown     = HbtnsShown,
		       col_mark_params = ColMarkP}  = ProcVars,

    #col_mark_params{col_btn_id         = MarkedColBtnId,
		     virtual_col_marked = ColMarked,
		     sort_btn_id        = OldSortBtnId,
		     virtual_sort_col   = SortCol}  = ColMarkP,
    
    {NewMarkedColBtnId, NewColMarked} = case ColMarked of
					    SortCol ->
						{undefined, undefined};
					    _Other ->
						{MarkedColBtnId, ColMarked}
					end,

    NewSortBtnId = set_sort_btn_color(OldSortBtnId, SortCol, HbtnsShown),

    NewColMarkP = ColMarkP#col_mark_params{col_btn_id         = NewMarkedColBtnId,
					   virtual_col_marked = NewColMarked,
					   sort_btn_id        = NewSortBtnId},

    ProcVars#process_variables{col_mark_params = NewColMarkP}.
    









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


get_btn_id(VirtualCol, HbtnsShown) ->
    case lists:keysearch(VirtualCol, #hbtn.virtual_col, HbtnsShown) of
	false ->
	    undefined;
	{value, HbtnRec} ->
	    HbtnRec#hbtn.id
    end.
    








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


set_sort_btn_color(undefined, SortCol, HbtnsShown) ->
    case lists:keysearch(SortCol, #hbtn.virtual_col, HbtnsShown) of
	false ->
	    undefined;
	{value, HbtnRec} ->
	    BtnId = HbtnRec#hbtn.id,
	    gs:config(BtnId, [{bg, ?SORT_MARK_COLOR}]),
	    BtnId
    end;
set_sort_btn_color(BtnId, undefined, _HbtnsShown) ->
    gs:config(BtnId, [{bg, ?DEFAULT_BG_COLOR}]);
set_sort_btn_color(OldSortBtnId, SortCol, HbtnsShown) ->
    case gs:read(OldSortBtnId, bg) of 
	SortCol ->
	       % Btn is already marked!
	    OldSortBtnId;
	_OtherColor ->
	       % Unmark old btn, mark new btn, if visible.
	    gs:config(OldSortBtnId, [{bg, ?DEFAULT_BG_COLOR}]),
	    case lists:keysearch(SortCol, #hbtn.virtual_col, HbtnsShown) of
		false ->
		    undefined;
		{value, HbtnRec} ->
		    BtnId = HbtnRec#hbtn.id,
		    gs:config(BtnId, [{bg, ?SORT_MARK_COLOR}]),
		    BtnId
	    end
    end.









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_vbtns(N, NofRowsShown, _VirtualRowNo,
	     _Vbtns, _Colors, _BlinkEnabled, _BlinkList) when N > NofRowsShown ->
    done;
update_vbtns(_N, _NofRowsShown, _VirtualRowNo, [], [], _BlinkEnabled, _BlinkList) ->
    done;
update_vbtns(_N, _NofRowsShown, _VirtualRowNo, [], _Colors, _BlinkEnabled, _BlinkList) ->
       % Right now we don't bother with dynamically creating row buttons:
       % we ought too know in advance the maximum number of rows that can
       % be visible.
    io:format("Configuration error: too few rows in grid.~n"),
    done;
update_vbtns(N, NofRowsShown, 
	     VirtualRowNo, [VbtnRec | VT], [], BlinkEnabled, BlinkList) ->
    VbtnId = VbtnRec#vbtn.id,
    gs:config(VbtnId, [{bg, ?DEFAULT_BG_COLOR},
		       {fg, ?BLACK},
		       {label, {text, integer_to_list(VirtualRowNo)}},
		       {data, {vbtn, N, VirtualRowNo}}      % Real row + virtual row
		      ]),
    update_vbtns(N + 1, NofRowsShown, VirtualRowNo + 1,VT, [], BlinkEnabled, 
		 BlinkList);
update_vbtns(N, NofRowsShown, 
	     VirtualRowNo, [VbtnRec | VT], [Color | CT], true, BlinkList) ->
    VbtnId = VbtnRec#vbtn.id,
    {Text, TextColor} = get_vbtn_text_and_textcolor(Color, VirtualRowNo),
    case lists:member(Color, BlinkList) of
	true ->
	    gs:config(VbtnId, [{bg, Color}, 
			       {fg, TextColor},
			       {label, {text, Text}},
			       {data, {vbtn, N, VirtualRowNo}}, % Real + virtual row
			       flash
			      ]);
	false ->
	    gs:config(VbtnId, [{bg, Color},
			       {fg, TextColor},
			       {label, {text, Text}},
			       {data, {vbtn, N, VirtualRowNo}} % Real + virtual row
			      ])
    end,
    update_vbtns(N + 1, NofRowsShown, VirtualRowNo + 1, VT, CT, true, BlinkList);
update_vbtns(N, NofRowsShown, 
	     VirtualRowNo, [VbtnRec | VT], [Color | CT], false, BlinkList) ->
    VbtnId = VbtnRec#vbtn.id,
    {Text, TextColor} = get_vbtn_text_and_textcolor(Color, VirtualRowNo),
    gs:config(VbtnId, [{bg, Color},
		       {fg, TextColor},
		       {label, {text, Text}},
		       {data, {vbtn, N, VirtualRowNo}}  % Real row + virtual row
		      ]),
    update_vbtns(N + 1, NofRowsShown, VirtualRowNo + 1, VT, CT, false, BlinkList).
    
    
    







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


get_vbtn_text_and_textcolor(?BLACK, N) ->
    {integer_to_list(N), ?WHITE};
get_vbtn_text_and_textcolor(?RED1, N) ->
    {integer_to_list(N), ?BLACK};
get_vbtn_text_and_textcolor(?RED2, N) ->
    {integer_to_list(N), ?BLACK};
get_vbtn_text_and_textcolor(?RED3, N) ->
    {integer_to_list(N), ?BLACK};
get_vbtn_text_and_textcolor(?RED4, N) ->
    {integer_to_list(N), ?BLACK};
get_vbtn_text_and_textcolor(?RED5, N) ->
    {integer_to_list(N), ?BLACK};
get_vbtn_text_and_textcolor(?GREEN1, N) ->
    {integer_to_list(N), ?BLACK};
get_vbtn_text_and_textcolor(?GREEN2, N) ->
    {integer_to_list(N), ?BLACK};
get_vbtn_text_and_textcolor(?GREEN3, N) ->
    {integer_to_list(N), ?BLACK};
get_vbtn_text_and_textcolor(?GREEN4, N) ->
    {integer_to_list(N), ?BLACK};
get_vbtn_text_and_textcolor(?GREEN5, N) ->
    {integer_to_list(N), ?BLACK};
get_vbtn_text_and_textcolor(_AnyOtherColor, N) ->
    {integer_to_list(N), ?BLACK}.









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


create_vbtns(ParentId, Ypos, NofRows, RowHeight, VbtnW, HbtnH) ->
    create_vbtns(1, NofRows, RowHeight, ParentId, VbtnW, Ypos + HbtnH, []).








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


create_vbtns(N, NofRows, _RowHeight, _ParId, _VbtnW, _Ypos, VAcc) when N > NofRows ->
    lists:reverse(VAcc);
create_vbtns(N, NofRows, RowHeight, ParId, VbtnW, Ypos, VAcc) ->
    VHeight = RowHeight + 1,
    VInfo   = create_one_vbtn(ParId, VHeight, VbtnW, Ypos, N),
    create_vbtns(N + 1, NofRows, RowHeight, ParId, VbtnW, Ypos + VHeight,
		 [VInfo | VAcc]).
    






%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


create_one_vbtn(ParentId, Height, VbtnW, Ypos, N) ->
    Id = gs:button(ParentId, [{width, VbtnW},
			      {height, Height},
			      {x, 0},
			      {y, Ypos},
			      {font, ?BTN_FONT},
			      {bg, ?DEFAULT_BG_COLOR},
			      {align, center},
			      {label, {text,integer_to_list(N)}},
			      {data, {vbtn, N, N}}      % Real row + virtual row
			     ]),
    #vbtn{virtual_row = N,
	  real_row    = N,
	  id          = Id,
	  height      = Height,
	  ypos        = Ypos}.









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_hbtns([], _HBtnsShown, 
	     _ResBtns, _VirtualColNo, _FrId, _Ypos, _HbtnH, _ResBtnW, _VbtnW) ->
    {[], []};
update_hbtns(ColsShown, HBtns, 
	     ResBtns, VirtualColNo, FrId, Ypos, HbtnH, ResBtnW, VbtnW) ->
    update_hbtns(1, ColsShown, HBtns, ResBtns, HbtnH, ResBtnW, VbtnW, 
		 VirtualColNo, FrId, 0, Ypos, [], []).








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_hbtns(_N, [], 
	     [], [], _HbtnH, _ResBtnW, _VbtnW, _ColNo, _FrId, _Xpos, _Ypos, HAcc, RAcc) ->
    {lists:reverse(HAcc), lists:reverse(RAcc)};

update_hbtns(N, [], [HInfo | HT], [RInfo | RT],  
	     HbtnH, ResBtnW, VbtnW, ColNo, FrId, Xpos, Ypos, HAcc, RAcc) ->
       % If too many buttons, i.e., if the ColsShown list 
       % has become empty.
    gs:destroy(HInfo#hbtn.id),
    gs:destroy(RInfo#resbtn.id),
    update_hbtns(N, [], HT, RT,  HbtnH, ResBtnW, VbtnW, ColNo, FrId, 
		 Xpos, Ypos, HAcc, RAcc);

update_hbtns(1, [ColW | T], [], [],  
	     HbtnH, ResBtnW, VbtnW, ColNo, FrId, _Xpos, Ypos, HAcc, RAcc) ->
       % The first button has to be bigger than the others.
    {HInfo, RInfo} = create_one_hbtn_and_resbtn(FrId, ColW - 2, 
						HbtnH, VbtnW - 1, 
						Ypos, ResBtnW, 1, ColNo),
    update_hbtns(2, T, [], [],  HbtnH, ResBtnW, VbtnW, ColNo + 1, 
		 FrId, VbtnW - 1 + ColW - 2 + ResBtnW, Ypos, [HInfo | HAcc], 
		 [RInfo | RAcc]);

update_hbtns(N, [ColW | T], [], [],  
	     HbtnH, ResBtnW, VbtnW, ColNo, FrId, Xpos, Ypos, HAcc, RAcc) ->
    {HInfo, RInfo} = create_one_hbtn_and_resbtn(FrId, ColW - 4, 
						HbtnH, Xpos, 
						Ypos, ResBtnW, N, ColNo),
    update_hbtns(N + 1, T, [], [],  HbtnH, ResBtnW, VbtnW, ColNo + 1, 
		 FrId, Xpos + ColW - 4 + ResBtnW, Ypos, [HInfo | HAcc], 
		 [RInfo | RAcc]);

update_hbtns(1, [ColW | T], [HInfo | HT], [RInfo | RT], 
	     HbtnH, ResBtnW, VbtnW, ColNo, FrId, _Xpos, Ypos, HAcc, RAcc) ->
    {NewHInfo, NewRInfo} = config_one_hbtn_and_resbtn(HInfo, RInfo, 
						      ColW - 2, 
						      VbtnW - 1, 
						      1, ColNo),
    update_hbtns(2, T, HT, RT,  HbtnH, ResBtnW, VbtnW, ColNo + 1, 
		 FrId, VbtnW - 1 + ColW - 2 + ResBtnW, Ypos, 
		 [NewHInfo | HAcc], [NewRInfo | RAcc]);

update_hbtns(N, [ColW | T], [HInfo | HT], [RInfo | RT],  
	     HbtnH, ResBtnW, VbtnW, ColNo, FrId, Xpos, Ypos, HAcc, RAcc) ->
    {NewHInfo, NewRInfo} = config_one_hbtn_and_resbtn(HInfo, RInfo, 
						      ColW - 4, 
						      Xpos, N, 
						      ColNo),
    update_hbtns(N + 1, T, HT, RT,  HbtnH, ResBtnW, VbtnW, ColNo + 1, 
		 FrId, Xpos + ColW - 4 + ResBtnW, Ypos, [NewHInfo | HAcc], 
		 [NewRInfo | RAcc]).












%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


create_one_hbtn_and_resbtn(ParId, HWidth, HHeight, HXpos, Ypos, RWidth, N, ColNo) ->
    HId = gs:button(ParId, [{width, HWidth},
			    {height, HHeight},
			    {x, HXpos},
			    {y, Ypos},
			    {font, ?BTN_FONT},
			    {bg, ?DEFAULT_BG_COLOR},
			    {data, {hbtn, N, ColNo}},
			    {label, {text, integer_to_list(ColNo)}}
			   ]),
    RId = gs:button(ParId, [{width, RWidth},
			    {height, HHeight},
			    {x, HXpos + HWidth},
			    {y, Ypos},
			    {cursor, resize},
			    {buttonpress, true},
			    {buttonrelease, true},
			    {data, {resbtn, N, ColNo, (HXpos + HWidth + RWidth div 2)}},
			    {bg, ?BLACK}
			   ]),
    HInfo = #hbtn{virtual_col = ColNo,
		  real_col    = N,
		  id          = HId,
		  width       = HWidth,
		  xpos        = HXpos},
    RInfo = #resbtn{virtual_col = ColNo,
		    real_col    = N,
		    id          = RId,
		    width       = RWidth,
		    xpos        = HXpos + HWidth},
    {HInfo, RInfo}.












%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


config_one_hbtn_and_resbtn(HInfo, RInfo, HWidth, HXpos, N, ColNo) ->
    gs:config(HInfo#hbtn.id, [{width, HWidth},
			      {x, HXpos},
			      {data, {hbtn, N, ColNo}},
			      {label, {text, integer_to_list(ColNo)}}
			     ]),
    gs:config(RInfo#resbtn.id, [{x, HXpos + HWidth},
				{data, {resbtn, N, ColNo, 
					(HXpos + HWidth + RInfo#resbtn.width div 2)}}
			]),
    NewHInfo = HInfo#hbtn{virtual_col = ColNo,
			  width       = HWidth,
			  xpos        = HXpos},
    NewRInfo = RInfo#resbtn{virtual_col = ColNo,
			    xpos        = HXpos + HWidth},
    {NewHInfo, NewRInfo}.









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_keys([], [], _FirstCol, _LastCol, _HBtns, _ParentId, KeyIdsAcc) ->
    lists:reverse(KeyIdsAcc);

update_keys([], [KeyId | IdT], FirstCol, LastCol, HBtns, ParentId, KeyIdsAcc) ->
    gs:config(KeyId, [{x, 1200}]),
    update_keys([], IdT, FirstCol, LastCol, HBtns, ParentId, 
		[KeyId | KeyIdsAcc]);

update_keys([KeyNo | KT], [], FirstCol, LastCol, 
	    HBtns,ParentId,  KeyIdsAcc) when KeyNo >= FirstCol, KeyNo =< LastCol ->
    {_Width, Xpos} = get_keywidth_and_pos(KeyNo, FirstCol, HBtns),
    NewKeyId = create_key(ParentId, Xpos, 1),
    update_keys(KT, [], FirstCol, LastCol, HBtns, ParentId, 
		[NewKeyId | KeyIdsAcc]);

update_keys([_KeyNo | KT], [], FirstCol, LastCol, HBtns, ParentId, KeyIdsAcc) ->
    update_keys(KT, [], FirstCol, LastCol, HBtns, ParentId, 
		KeyIdsAcc);

update_keys([KeyNo | KT], [KeyId | IdT], FirstCol, LastCol, 
	    HBtns, ParentId, KeyIdsAcc) when KeyNo >= FirstCol, KeyNo =< LastCol ->
    {Width, Xpos} = get_keywidth_and_pos(KeyNo, FirstCol, HBtns),
    gs:config(KeyId, [{width, Width},
		      {x, Xpos}
		     ]),
    update_keys(KT, IdT, FirstCol, LastCol, HBtns, ParentId, 
		[KeyId | KeyIdsAcc]);

update_keys([_KeyNo | KT], 
	    [KeyId | IdT], FirstCol, LastCol, HBtns, ParentId, KeyIdsAcc) ->
    update_keys(KT, [KeyId | IdT], FirstCol, LastCol, HBtns, ParentId, 
		KeyIdsAcc).









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


get_keywidth_and_pos(VirtualCol, FirstCol, HBtns) ->
    RealColNo = VirtualCol - FirstCol + 1,
    HBtnR = lists:nth(RealColNo, HBtns),
    #hbtn{width = Width,
	  xpos  = Xpos}  = HBtnR,
    KeyWidth = 10,
       % Compute the x position for the key!
    KeyXpos = (Xpos + (Width div 2) - (KeyWidth div 2)),
    {KeyWidth, KeyXpos}.
    






%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


create_key(ParentId, Xpos, Ypos) ->
    PicDir = code:priv_dir(tv),
    C = gs:canvas(ParentId, [{width, 10},
			     {height, 18},
			     {x, Xpos},
			     {y, Ypos},
			     {bg, ?DEFAULT_BG_COLOR}
			    ]),
    gs:create(image, C, [{bitmap, PicDir ++ "/key.xbm"}]),
    C.







    













