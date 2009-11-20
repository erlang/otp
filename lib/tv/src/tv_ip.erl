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
-module(tv_ip).



-export([ip/1]).



-include("tv_int_msg.hrl").


-define(NOF_LABELS, 25).

-define(DEFAULT_BG_COLOR, {217, 217, 217}).







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


ip(_Master) ->
    W = gs:window(win, gs:start(), [{width, 302},
				    {height, 38},
				    {bg, ?DEFAULT_BG_COLOR},
				    {title, "Launching..."}
				   ]),
    C = gs:canvas(W, [{width, 40},
		      {height, 35},
		      {x, 0},
		      {bg, {255, 255, 255}}
		     ]),
    gs:create(image, C, [{load_gif, code:priv_dir(tv) ++ "/erlang.gif"}]),
    gs:label(W, [{width, 252},
		 {height, 12}, 
		 {x, 47},
		 {y, 23},
		 {bg, {0, 0, 0}},
		 {cursor, arrow}
		]),
    
    LabelList = create_labels(?NOF_LABELS, W, 48),
    
    L = gs:label(W, [{width, 250},
		     {height, 18},
		     {x, 47},
		     {y, 0},
		     {bg, ?DEFAULT_BG_COLOR},
		     {fg, {0, 0, 0}},
		     {align, w}
		    ]),
    gs:config(win, [{map, true}]),
    loop(1, LabelList, L).









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


create_labels(0, _WinId, _Xpos) ->
    [];
create_labels(N, WinId, Xpos) ->
    Width = 10,
    Xdiff = Width,
    LabelId = gs:label(WinId, [{width, Width},
			       {height, 10}, 
			       {x, Xpos},
			       {y, 24},
			       {bg, {235, 235, 235}},
			       {cursor, arrow}
			      ]),

    [LabelId | create_labels(N - 1, WinId, Xpos + Xdiff)].











%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


loop(N, LabelList, L) ->
    receive
	Msg ->
	    case Msg of

		#ip_update{nof_elements_to_mark = X, text = Text} ->
		    update_window(LabelList, N, N + X, L, Text),
		    loop(N + X, LabelList, L);

		#ip_quit{} ->
		    update_labels(LabelList, N, ?NOF_LABELS),
		    receive
		    after 1000 ->
			    done
		    end,
		    done;

		_Other ->
		    loop(N, LabelList, L)
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


update_window(LabelList, N, Hi, LblId, Text) ->
    gs:config(win, [raise]),
    gs:config(LblId, [{label, {text, Text}}]),
    update_labels(LabelList, N, Hi).








%%======================================================================
%% Function:      
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================


update_labels(_LabelList, N, _Hi) when N > ?NOF_LABELS ->
    done;
update_labels(_LabelList, N, Hi) when N >= Hi ->
    done;
update_labels(LabelList, N, Hi) ->
    LabelId = lists:nth(N, LabelList),
    gs:config(LabelId, [{bg, {0, 0, 255}}]),
    update_labels(LabelList, N + 1, Hi).
    











