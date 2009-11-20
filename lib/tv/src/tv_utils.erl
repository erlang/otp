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
-module(tv_utils).



-export([notify/3]).



   %% Minimum size of help windows
-define(wwin, 300).
-define(hwin, 180).

   %% Button sizes
-define(wbut, 60).
-define(hbut, 30).

-define(pad, 10).


%----------------------------------------
% notify(S,Strings) -> ok
%   S       = pid() GS
%   Strings = string() | [string()]
% A notification window contains a message to the user.
% Will lock the GUI until the user confirms the message by
% pressing the 'Ok' button.
%----------------------------------------
notify(S,Title,Strings) ->
    W    = required_width(Strings, ?wwin),
    Htop = round(2 * ?hwin / 3),
    Hbot = ?hwin - Htop,

    %% Open a new window
    Win = gs:create(window,S,[{width, W},
			      {height, ?hwin},
                              {title,  Title},
                              {data, notifywin}
			     ]),

    %% Top frame containing a label
    Top = gs:create(frame,Win,[{width, W},
			       {height, Htop},
			       {x, 0},
			       {y, 0},
                               {data, notifywin},
			       {keypress, true}
			      ]),
    
    Lbl = gs:create(label,Top,[{width,W},
			       {height, Htop - 2 * ?pad},
			       {x, 0},
			       {y, ?pad},
                               {align, c},
			       {justify, center},
                               {data, notifywin},
			       {keypress, true}
			      ]),
  
    gs:config(Lbl, {label, {text, insert_newlines(Strings)}}),

    %% Bottom frame containing an 'Ok' button
    Bot = gs:create(frame,Win,[{width, W},
			       {height, Hbot},
			       {x, 0},
			       {y, Htop}
			      ]),
    gs:create(button,Bot,[{width, ?wbut},
			  {height, ?hbut},
                          {x, W / 2 - ?wbut/2},
			  {y, Hbot / 2 - ?hbut / 2},
                          {label, {text, "OK"}},
                          {data, notifywin},
			  {keypress, true}]),

    gs:config(Win, [{map,true}]),
    
    event_loop(Win,null).




insert_newlines([String|Rest]) when is_list(String), Rest=/=[]->
    String ++ "\n" ++ insert_newlines(Rest);
insert_newlines([Last]) ->
    [Last];
insert_newlines(Other) ->
    Other.




event_loop(Win,Entry) ->
    receive

        %%
        %% Notify window
        %%

        %% 'Ok' pressed in notify window
        {gs,_Obj,_Event,notifywin,["OK"|_]} ->
            gs:destroy(Win),
            ok;

        %% 'Window manager destroy' received in notify window
        {gs,_Obj,destroy,notifywin,_} ->
            gs:destroy(Win),
            ok;

        %% 'Return' pressed in notify or confirm window
        {gs,_Obj,_Event,helpwin,['Return'|_]} ->
            gs:destroy(Win),
            ok;


        %%
        %% Common or partly common events
        %%

        %% 'Window manager destroy' received in notify, 
        %% confirm,confirm_exit or request window
        {gs,_Obj,destroy,_,_} ->
            gs:destroy(Win),
            cancel;

        %% Flush any other GS events
        {gs,_Obj,_Event,_Data,_Arg} ->
            event_loop(Win,Entry)
    end.




%----------------------------------------
% required_width(Strings,Min) -> Req
%   Strings   = string() | [string()]
%   Min = Req = integer()
% Returns the minimum required width in pixels for a help window,
% which is the maximum of Min and the required width for Strings.
% NOTE: Font dependant really!
%----------------------------------------
required_width([First|Rest],Min) when is_list(First) ->
    Req = 7*length(First), % 7 pixels per character
    if
        Req>Min ->
            required_width(Rest,Req);
        true ->
            required_width(Rest,Min)
    end;
required_width([],Min) ->
    Min;
required_width(String,Min) ->
    Req = 7*length(String),
    if
        Req>Min ->
            Req;
        true ->
            Min
    end.

