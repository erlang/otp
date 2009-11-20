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
%%%*********************************************************************
%%% 
%%%   Description:      Part of pd controlling the scale, i.e., the scrollbar
%%%                     imitation.
%%%
%%%*********************************************************************


-module(tv_pd_scale).



-export([init_scale/2, 
	 resize_scale/2, 
	 set_scale_range/3, 
	 set_scale_pos/3]).



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


init_scale(FrameP, ScaleP) ->
    #frame_params{sheet_frame_id     = SheetFrameId,
		  sheet_frame_width  = SheetFrameWidth,
		  sheet_frame_height = SheetFrameHeight,
		  grid_frame_width   = GridFrameWidth,
		  grid_frame_height  = GridFrameHeight} = FrameP,

    VScaleId = create_scale(vscale, SheetFrameId, SheetFrameWidth, GridFrameHeight),
    HScaleId = create_scale(hscale, SheetFrameId, GridFrameWidth, SheetFrameHeight),
    
    ScaleP#scale_params{vscale_id  = VScaleId,
			vscale_pos = 0,
			hscale_id  = HScaleId,
			hscale_pos = 0
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


resize_scale(FrameP, ScaleP) ->
    #frame_params{sheet_frame_width  = SheetFrameWidth,
		  sheet_frame_height = SheetFrameHeight,
		  grid_frame_width   = GridFrameWidth,
		  grid_frame_height  = GridFrameHeight} = FrameP,

    #scale_params{vscale_id    = VScaleId,
		  hscale_id    = HScaleId} = ScaleP,

    config_scale(vscale, VScaleId, SheetFrameWidth, GridFrameHeight),
    config_scale(hscale, HScaleId, GridFrameWidth, SheetFrameHeight),
    ScaleP.







    

%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


set_scale_range(vscale, Range, ScaleP) ->
    {Lo, Hi} = Range,
    NewRange = if
		   Lo > Hi ->
		       {Hi, Hi};
		   true ->
		       Range
	       end,
    VScaleId = ScaleP#scale_params.vscale_id,
    gs:config(VScaleId, [{range, NewRange}]);
set_scale_range(hscale, Range, ScaleP) ->
    {Lo, Hi} = Range,
    NewRange = if
		   Lo > Hi ->
		       {Hi, Hi};
		   true ->
		       Range
	       end,
    HScaleId = ScaleP#scale_params.hscale_id,
    gs:config(HScaleId, [{range, NewRange}]).








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


set_scale_pos(_ScaleName, undefined, ScaleP) ->
    ScaleP;
set_scale_pos(vscale, NewPos, ScaleP) ->
    ScaleId = ScaleP#scale_params.vscale_id,
    gs:config(ScaleId, [{pos, NewPos}]),
    ScaleP#scale_params{vscale_pos = NewPos};
set_scale_pos(hscale, NewPos, ScaleP) ->
    ScaleId = ScaleP#scale_params.hscale_id,
    gs:config(ScaleId, [{pos, NewPos}]),
    ScaleP#scale_params{hscale_pos = NewPos}.







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


config_scale(ScaleName, ScaleId, FrameWidth, FrameHeight) ->
    {Width, Height, Xpos, Ypos} = get_scale_coords(ScaleName, 
						   FrameWidth, 
						   FrameHeight),
    gs:config(ScaleId, [{height, Height},
			{width, Width},
			{x, Xpos},
			{y, Ypos}
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


create_scale(ScaleName, FrameId, FrameWidth, FrameHeight) ->
    {Width, Height, Xpos, Ypos} = get_scale_coords(ScaleName, 
						   FrameWidth, 
						   FrameHeight),
    {Orientation, Range}  = case ScaleName of 
				vscale ->
				    {vertical, {1, 1}};
				hscale ->
				    {horizontal, {1, 1}}
			    end,
    gs:scale(FrameId, [{data, ScaleName},
		       {orient, Orientation},
		       {buttonpress, true},
		       {buttonrelease, true},
		       {height, Height},
		       {width, Width},
		       {x, Xpos},
		       {y, Ypos},
		       {bg, ?DEFAULT_BG_COLOR},
		       {fg, {0, 0, 0}},
		       {range, Range}
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


get_scale_coords(hscale, FrameWidth, FrameHeight) ->
    Height = ?HSCALE_HEIGHT,
    Xpos   = ?VBTN_WIDTH - 3,  % Subtracting 3 makes it look better!
    Ypos   = FrameHeight - Height,
    Width  = FrameWidth - Xpos + 5,  % Adding 5 for better look!
    {Width, Height, Xpos, Ypos};
get_scale_coords(vscale, FrameWidth, FrameHeight) ->
    Width  = ?VSCALE_WIDTH,
    Xpos   = (FrameWidth - Width),
    Ypos   = ?HBTN_HEIGHT - 3,       % Subtracting 3 makes it look better!
    Height = FrameHeight - Ypos + 5,   % Adding 5 for better look!
    {Width, Height, Xpos, Ypos}.
    


















