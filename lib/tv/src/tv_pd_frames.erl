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
-module(tv_pd_frames).



-export([create_display_frames/4, resize_display_frames/3]).




-include("tv_int_def.hrl").
-include("tv_pd_int_def.hrl").







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


create_display_frames(WindowId, WindowWidth, WindowHeight, FrameP) ->
    {DisplayId, DisplayWidth, DisplayHeight} = 
	create_frame(WindowId, 
		     get_display_coords(WindowWidth, WindowHeight),
		     ?DEFAULT_BG_COLOR,
		     0),

    {ToolbarId, ToolbarWidth, ToolbarHeight} = create_toolbar_frame(DisplayId, 
								    DisplayWidth),

    {SheetFrameId, SheetBgFrameId, SheetFrameWidth, SheetFrameHeight} = 
	create_sheet_frames(DisplayId, 
			    DisplayWidth, 
			    DisplayHeight),

    {GridFrameId, GridBgFrameId, GridFrameWidth, GridFrameHeight} = 
	create_grid_frames(SheetFrameId, 
			   SheetFrameWidth, 
			   SheetFrameHeight),
    
    
    FrameP#frame_params{display_id           = DisplayId,
			toolbar_frame_id     = ToolbarId,
			toolbar_frame_width  = ToolbarWidth,
			toolbar_frame_height = ToolbarHeight,
			sheet_frame_id       = SheetFrameId,
			sheet_frame_width    = SheetFrameWidth,
			sheet_frame_height   = SheetFrameHeight,
			sheet_bgframe_id     = SheetBgFrameId,
			grid_frame_id        = GridFrameId,
			grid_frame_width     = GridFrameWidth,
			grid_frame_height    = GridFrameHeight,
			grid_bgframe_id      = GridBgFrameId
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


resize_display_frames(NewW, NewH, FrameP) ->
    #frame_params{display_id         = DispId,
		  toolbar_frame_id   = ToolbarId,
		  sheet_frame_id     = SheetFgId,
		  sheet_bgframe_id   = SheetBgId,
		  grid_frame_id      = GridFgId,
		  grid_bgframe_id    = GridBgId}  = FrameP,
    
    {NewDispW, NewDispH} = config_frame(DispId, get_display_coords(NewW, NewH)),
    {NewToolW, NewToolH} = resize_toolbar(ToolbarId, NewDispW),
    {NewSheetFgW, NewSheetFgH} = resize_sheet_frames(SheetFgId, SheetBgId, NewDispW,
						     NewDispH),

    {NewGridFgW, NewGridFgH} = resize_grid_frames(GridFgId, GridBgId, NewSheetFgW, 
						  NewSheetFgH),
    
    FrameP#frame_params{toolbar_frame_width  = NewToolW,
			toolbar_frame_height = NewToolH,
			sheet_frame_width    = NewSheetFgW,
			sheet_frame_height   = NewSheetFgH,
			grid_frame_width     = NewGridFgW,
			grid_frame_height    = NewGridFgH
			}.








%%%********************************************************************
%%% INTERNAL FUNCTIONS
%%%********************************************************************





%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


config_frame(Id, {Width, Height, Xpos, Ypos}) ->
    gs:config(Id, [{width, Width},
		   {height, Height},
		   {x, Xpos},
		   {y, Ypos}
		  ]),
    {Width, Height}.







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


create_frame(ParentId, {Width, Height, Xpos, Ypos}, Color, BorderWidth) ->
    Id = gs:frame(ParentId, [{width, Width},
			     {height, Height},
			     {x, Xpos},
			     {y, Ypos},
			     {bw, BorderWidth},
			     {bg, Color}
			    ]),
    {Id, Width, Height}.









%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


create_grid_frames(SheetFrameId, SheetFrameWidth, SheetFrameHeight) ->
    {BgId, _W, _H} = 
	create_frame(SheetFrameId,
		     get_grid_frame_coords(bg, SheetFrameWidth, SheetFrameHeight),
		     ?BLACK, 
		     0),
    {FgId, FgWidth, FgHeight} = 
	create_frame(SheetFrameId, 
		     get_grid_frame_coords(fg, SheetFrameWidth, SheetFrameHeight),
		     ?DEFAULT_BG_COLOR, 
		     0),
    {FgId, BgId, FgWidth, FgHeight}.
    
    






%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


create_sheet_frames(DispId, DispWidth, DispHeight) ->
    {BgId, _W, _H} = create_frame(DispId, 
				  get_sheet_frame_coords(bg, DispWidth, DispHeight),
				  ?BLACK, 
				  0),
    {FgId, FgWidth, FgHeight} = 
	create_frame(DispId, 
		     get_sheet_frame_coords(fg, DispWidth, DispHeight), 
		     ?DEFAULT_BG_COLOR, 
		     0),
    {FgId, BgId, FgWidth, FgHeight}.







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


create_toolbar_frame(DispId, DispWidth) ->
    create_frame(DispId, get_toolbar_coords(DispWidth), ?DEFAULT_BG_COLOR, 0).







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


get_display_coords(WindowWidth, WindowHeight) ->
    Xpos = 4,
    {WindowWidth - 2 * Xpos, WindowHeight - ?MENUBAR_HEIGHT - Xpos, Xpos, ?MENUBAR_HEIGHT}.








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


get_grid_frame_coords(bg, FrameWidth, FrameHeight) ->
    get_grid_frame_coords2(FrameWidth, FrameHeight, 0);
get_grid_frame_coords(fg, FrameWidth, FrameHeight) ->
    get_grid_frame_coords2(FrameWidth, FrameHeight, 1).








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


get_grid_frame_coords2(FrameWidth, FrameHeight, BorderWidth) ->
    Xpos   = 0,
    Ypos   = 0,
    Width  = FrameWidth - ?VSCALE_WIDTH - Xpos - BorderWidth,
    Height = FrameHeight - ?HSCALE_HEIGHT - Ypos - BorderWidth,
    {Width, Height, Xpos, Ypos}.
    





%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


get_sheet_frame_coords(bg, FrameWidth, FrameHeight) ->
    get_sheet_frame_coords2(FrameWidth, FrameHeight, 0);
get_sheet_frame_coords(fg, FrameWidth, FrameHeight) ->
    get_sheet_frame_coords2(FrameWidth, FrameHeight, 1).








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


get_sheet_frame_coords2(FrameWidth, FrameHeight, BorderWidth) ->
    Xpos   = BorderWidth,
    Ypos   = ?TOOLBAR_HEIGHT + BorderWidth,
    Width  = FrameWidth - 2 * BorderWidth,
    Height = FrameHeight - Ypos - ?MISC_AREA_HEIGHT - BorderWidth,
    {Width, Height, Xpos, Ypos}.


    





%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


get_toolbar_coords(DispWidth) ->
    Xpos = 0,
    {DispWidth - 2 * Xpos, ?TOOLBAR_HEIGHT, Xpos, 0}.





    



%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


resize_grid_frames(FgId, BgId, ParentWidth, ParentHeight) ->
    config_frame(BgId, get_grid_frame_coords(bg, ParentWidth, ParentHeight)),
    config_frame(FgId, get_grid_frame_coords(fg, ParentWidth, ParentHeight)).

    








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


resize_sheet_frames(FgId, BgId, ParentWidth, ParentHeight) ->
    config_frame(BgId, get_sheet_frame_coords(bg, ParentWidth, ParentHeight)),
    config_frame(FgId, get_sheet_frame_coords(fg, ParentWidth, ParentHeight)).
		 
    







%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


resize_toolbar(Id, DispWidth) ->
    config_frame(Id, get_toolbar_coords(DispWidth)).



