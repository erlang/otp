%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2018. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
%%%-------------------------------------------------------------------
%%% File    : menus.erl
%%% Author  : Matthew Harrison <harryhuk at users.sourceforge.net>
%%% Description : Test of menus
%%%
%%% Created :  18 Sep 2008 by  Matthew Harrison <harryhuk at users.sourceforge.net>
%%%-------------------------------------------------------------------
-module(taskbar).

-include_lib("wx/include/wx.hrl").

-export([start/0]).

%%%Lots of IDs to declare!
-define(menuID_FILE_QUIT,           ?wxID_EXIT).
-define(menuID_FILE_CLEAR_LOG,      100).

-define(menuID_MENUBAR_TOGGLE,      200).
-define(menuID_MENUBAR_APPEND,      201).
-define(menuID_MENUBAR_INSERT,      202).
-define(menuID_MENUBAR_DELETE,      203).
-define(menuID_MENUBAR_ENABLE,      204).
-define(menuID_MENUBAR_GET_LABEL,   206).
-define(menuID_MENUBAR_SET_LABEL,   207).
-define(menuID_MENUBAR_FIND_MENU,   208).

-define(menuID_MENU_APPEND,         300).
-define(menuID_MENU_APPEND_SUB,     301).
-define(menuID_MENU_INSERT,         302).
-define(menuID_MENU_DELETE,         303).
-define(menuID_MENU_ENABLE,         304).
-define(menuID_MENU_CHECK,          305).
-define(menuID_MENU_GET_LABEL,      306).
-define(menuID_MENU_SET_LABEL,      307).
-define(menuID_MENU_GET_INFO,       308).
-define(menuID_MENU_FIND_ITEM,      309).

-define(menuID_TEST_NORMAL,         400).
-define(menuID_TEST_CHECK,          401).
-define(menuID_TEST_RADIO_1,        402).
-define(menuID_TEST_RADIO_2,        403).
-define(menuID_TEST_RADIO_3,        404).

-define(menuID_SUBMENU,             450).
-define(menuID_SUBMENU_NORMAL,      451).
-define(menuID_SUBMENU_CHECK,       452).
-define(menuID_SUBMENU_RADIO_1,     453).
-define(menuID_SUBMENU_RADIO_2,     454).
-define(menuID_SUBMENU_RADIO_3,     455).

-define(menuID_DUMMY_FIRST,         500).
-define(menuID_DUMMY_SECOND,        501).
-define(menuID_DUMMY_THIRD,         502).
-define(menuID_DUMMY_FOURTH,        503).
-define(menuID_DUMMY_LAST,          504).

-define(menuID_HELP_ABOUT,          ?wxID_ABOUT).

-define(menuID_POPUP_TO_BE_DELETED, 2000).
-define(menuID_POPUP_TO_BE_GREYED,  2001).
-define(menuID_POPUP_TO_BE_CHECKED, 2002).
-define(menuID_POPUP_TO_BE_SUBMENU, 2003).

-define(wID_LOG_TEXT_CTRL, 3000).

-record(state, {dummyMenuCount=0, fileMenu=undefined}).

%%    
%%    
%%    
start() ->
    Wx = wx:new(),
    wx:batch(fun() -> create_frame(Wx) end),
    Taskbar = wxTaskBarIcon:new(fun() ->
        create_menu()
    end),
    Path = filename:dirname(code:which(?MODULE)),
    wxTaskBarIcon:setIcon(Taskbar,  wxIcon:new(filename:join(Path,"sample.xpm"), [{type, ?wxBITMAP_TYPE_XPM}])),
    
    loop(#state{}),
    ok.


%%    
%%    
%%    
create_frame(Wx) ->
    Frame = wxFrame:new(Wx, -1, "wxErlang menu sample", [{size, {600,400}}]),

    Path = filename:dirname(code:which(?MODULE)),    
    wxFrame:setIcon(Frame,  wxIcon:new(filename:join(Path,"sample.xpm"), [{type, ?wxBITMAP_TYPE_XPM}])),

    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window),

    Frame.

%%    
%%    
%%    
create_menu() ->
    % unlike wxwidgets the stock menu items still need text to be given, 
    % although help text does appear
    StockSubMenu = wxMenu:new(),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_ADD      }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_CANCEL   }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_CLEAR    }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_CLOSE    }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_COPY     }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_CUT      }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_DELETE   }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_FIND     }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_REPLACE  }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_BACKWARD }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_DOWN     }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_FORWARD  }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_UP       }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_HELP     }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_HOME     }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_INDENT   }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_INDEX    }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_ITALIC   }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_NEW              }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_NO               }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_OK               }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_OPEN             }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_PASTE            }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_PREFERENCES      }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_PRINT            }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_EXIT             }])),

    StockSubMenu.

loop(State) ->
    receive 
  	#wx{event=#wxClose{}, obj=Frame} ->
  	    io:format("~p Closing window ~n",[self()]),
  	    wxWindow:destroy(Frame); %onClose(Frame); 
	#wx{id=?menuID_FILE_QUIT, obj=Frame, event=#wxCommand{type=command_menu_selected}} ->
  	    io:format("~p Closing window ~n",[self()]),
  	    wxWindow:destroy(Frame); %onClose(Frame); 
  	    
	Msg ->
	    io:format("Got ~p ~n", [Msg]),
	    loop(State)
    after 5000 ->
	    loop(State)
    end.
    
