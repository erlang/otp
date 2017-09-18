%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
-module(menu).

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
    Frame = wx:batch(fun() -> create_frame(Wx) end),
    wxWindow:show(Frame),
    
    State = #state{},
    
    loop(State),
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

    FileMenu   = create_file_menu(),
    MenuBarMenu = create_menubar_menu(),
    MenuMenu = create_menu_menu(),
    TestMenu = create_test_menu(),
    HelpMenu = create_help_menu(),
            
    MenuBar    = wxMenuBar:new(?wxMB_DOCKABLE),


    wxMenuBar:append(MenuBar, FileMenu,     "&File"),
    wxMenuBar:append(MenuBar, MenuBarMenu,  "Menu&bar"),
    wxMenuBar:append(MenuBar, MenuMenu,     "&Menu"),
    wxMenuBar:append(MenuBar, TestMenu,     "&Test"),
    wxMenuBar:append(MenuBar, HelpMenu,     "&Help"),
    
    wxFrame:setMenuBar(Frame, MenuBar),
    
    LogTextCtrl = wxTextCtrl:new(Frame, ?wID_LOG_TEXT_CTRL, 
				 [{value, ""},
						%{pos, ?wxDefaultPosition}, 
						%{size, ?wxDefaultSize}, 
				  {style, ?wxTE_MULTILINE}]),
    wxTextCtrl:setEditable(LogTextCtrl, false),

    ok = wxFrame:setStatusText(Frame, "Welcome to wxErlang menu sample",[]),
    
    ok = wxFrame:connect(Frame, command_menu_selected), 
    
    String = "Brief explanations:~n"
            "the commands from the \"Menu\" menu append/insert/delete items to/from the last menu.~n"
            "The commands from \"Menubar\" menu work with the menubar itself.~n~n",
           %% "Right click the band below to test popup menus.~n",
    logMessage(Frame, String),
    Frame.

%%    
%%    
%%    
create_file_menu() ->
    FileMenu  = wxMenu:new(),
    
    wxMenu:append(FileMenu, wxMenuItem:new([
            {id,        ?menuID_SUBMENU},
            {subMenu,   create_stock_menu()},
            {text,      "&Standard items demo"}
            ])),
    ClearLogBitmap = wxBitmap:new("copy.xpm"),
    ClearLogItem = wxMenuItem:new([
            {id,    ?menuID_FILE_CLEAR_LOG},
            {text,  "Clear &log\tCtrl-L"}   %% note mnemonic and accelerator
            ]),
    wxMenuItem:setBitmap(ClearLogItem, ClearLogBitmap),
            
    wxMenu:append(FileMenu, ClearLogItem ),
    wxMenu:appendSeparator(FileMenu),  
    wxMenu:append(FileMenu, wxMenuItem:new([
            {id, ?menuID_FILE_QUIT} %,
            %{text, "E&xit\tAlt-X"}
            ])),
    FileMenu.

%%    
%%    
%%    
create_menubar_menu() ->
    MenuBarMenu   = wxMenu:new(),
    wxMenu:append(MenuBarMenu, wxMenuItem:new([
            {id,    ?menuID_MENUBAR_APPEND},
            {text,  "&Append menu\tCtrl-A"},
            {help,  "Append a menu to the menubar"}
            ])),
    wxMenu:append(MenuBarMenu, wxMenuItem:new([
            {id,    ?menuID_MENUBAR_INSERT},
            {text,  "&Insert menu\tCtrl-I"},
            {help,  "Insert a menu into the menubar"}
            ])),
    wxMenu:append(MenuBarMenu, wxMenuItem:new([
            {id,    ?menuID_MENUBAR_DELETE},
            {text,  "&Delete menu\tCtrl-D"},
            {help,  "Insert a menu into the menubar"}
            ])),
    wxMenu:append(MenuBarMenu, wxMenuItem:new([
            {id,    ?menuID_MENUBAR_TOGGLE},
            {text,  "&Toggle menu\tCtrl-T"},
            {help,  "Toggle the first menu in the menubar"},
            {kind,  ?wxITEM_CHECK}
            ])),   
    wxMenu:appendSeparator(MenuBarMenu), %% --------------------------
    wxMenu:append(MenuBarMenu, wxMenuItem:new([
            {id,    ?menuID_MENUBAR_ENABLE},
            {text,  "&Enable menu\tCtrl-E"},
            {help,  "Enable or disable the last menu"},
            {kind,  ?wxITEM_CHECK}
            ])),
    wxMenu:appendSeparator(MenuBarMenu), %% --------------------------
    wxMenu:append(MenuBarMenu, wxMenuItem:new([
            {id,    ?menuID_MENUBAR_GET_LABEL},
            {text,  "&Get menu label\tCtrl-G"},   
            {help,  "Get the label of the last menu"}
            ])),
    wxMenu:append(MenuBarMenu, wxMenuItem:new([
            {id,    ?menuID_MENUBAR_SET_LABEL},
            {text,  "&Set menu label\tCtrl-S"},   
            {help,  "Change the label of the last menu"}
            ])),
    wxMenu:appendSeparator(MenuBarMenu), %% --------------------------
    wxMenu:append(MenuBarMenu, wxMenuItem:new([
            {id,    ?menuID_MENUBAR_FIND_MENU},
            {text,  "&Find menu from label\tCtrl-F"},   
            {help,  "Find a menu by searching for its label"}
            ])),
    MenuBarMenu.


%%    
%%    
%%    
create_menu_menu() ->
    MenuMenu   = wxMenu:new(),
    wxMenu:append(MenuMenu, wxMenuItem:new([
            {id,    ?menuID_MENU_APPEND},
            {text,  "&Append menu item\tAlt-A"},
            {help,  "Append a menu item to the last menu"}
            ])),
    wxMenu:append(MenuMenu, wxMenuItem:new([
            {id,    ?menuID_MENU_APPEND_SUB},
            {text,  "&Append sub menu\tAlt-S"},
            {help,  "Append a sub menu to the last menu"}
            ])),
    wxMenu:append(MenuMenu, wxMenuItem:new([
            {id,    ?menuID_MENU_INSERT},
            {text,  "&Insert menu item\tAlt-I"},   
            {help,  "Insert a menu item in head of the last menu"}
            ])),
    wxMenu:append(MenuMenu, wxMenuItem:new([
            {id,    ?menuID_MENU_DELETE},
            {text,  "&Delete menu item\tAlt-D"},   
            {help,  "Delete the last menu item from the last menu"}
            ])),
    wxMenu:appendSeparator(MenuMenu), %% --------------------------
    wxMenu:append(MenuMenu, wxMenuItem:new([
            {id,    ?menuID_MENU_ENABLE},
            {text,  "&Enable menu item\tAlt-E"},   
            {help,  "Enable or disable the last menu item"},
            {kind,  ?wxITEM_CHECK}
            ])),
    wxMenu:append(MenuMenu, wxMenuItem:new([
            {id,    ?menuID_MENU_CHECK},
            {text,  "&Check menu item\tAlt-C"},   
            {help,  "Check or uncheck the last menu item"},
            {kind,  ?wxITEM_CHECK}
            ])),   
    wxMenu:appendSeparator(MenuMenu), %% --------------------------
    wxMenu:append(MenuMenu, wxMenuItem:new([
            {id,    ?menuID_MENU_GET_INFO},
            {text,  "Get menu item in&fo\tAlt-F"},   
            {help,  "Show the state of the last menu item"}
            ])),
    wxMenu:append(MenuMenu, wxMenuItem:new([
            {id,    ?menuID_MENU_SET_LABEL},
            {text,  "&Set menu label\tCtrl-S"},   
            {help,  "Change the label of the last menu"}
            ])),
    wxMenu:appendSeparator(MenuMenu), %% --------------------------
    wxMenu:append(MenuMenu, wxMenuItem:new([
            {id,    ?menuID_MENU_FIND_ITEM},
            {text,  "Find menu item from label"},
            {help,  "Find a menu item by searching for its label"}
            ])),
    MenuMenu.


%%    
%%    
%%    
create_test_menu() ->
    TestMenu   = wxMenu:new(),
    wxMenu:append(TestMenu, wxMenuItem:new([
            {id,    ?menuID_TEST_NORMAL},
            {text,  "&Normal submenu item"},
            {help,  "Disabled submenu item"}
            ])),
    wxMenu:appendSeparator(TestMenu), %% --------------------------
    %% note different way of adding check menu item
    wxMenu:appendCheckItem(TestMenu, ?menuID_TEST_CHECK,    "&Check item"),
    wxMenu:appendSeparator(TestMenu), %% --------------------------
    wxMenu:appendRadioItem(TestMenu, ?menuID_TEST_RADIO_1,  "Radio item &1"),
    wxMenu:appendRadioItem(TestMenu, ?menuID_TEST_RADIO_2,  "Radio item &2"),
    wxMenu:appendRadioItem(TestMenu, ?menuID_TEST_RADIO_3,  "Radio item &3"),
    TestMenu.

%%    
%%    
%%    
create_help_menu() ->
    HelpMenu =  wxMenu:new(),
    % unlike wxwidgets the stock menu items still need text to be given, 
    % although help text does appear
    % Note the keybord accelerator
    wxMenu:append(HelpMenu, wxMenuItem:new([
            {id,    ?menuID_HELP_ABOUT},
            %{text,  "&About\tF1"},
            {help,  "About menu sample"}
            ])),
    HelpMenu.

%%    
%%    
%%    
create_stock_menu() ->
    % unlike wxwidgets the stock menu items still need text to be given, 
    % although help text does appear
    StockSubMenu = wxMenu:new(),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_ADD      }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_APPLY    }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_BOLD     }])),
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
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_JUSTIFY_CENTER   }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_JUSTIFY_FILL     }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_JUSTIFY_LEFT     }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_JUSTIFY_RIGHT    }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_NEW              }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_NO               }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_OK               }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_OPEN             }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_PASTE            }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_PREFERENCES      }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_PRINT            }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_PREVIEW          }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_PROPERTIES       }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_EXIT             }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_REDO             }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_REFRESH          }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_REMOVE           }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_REVERT_TO_SAVED  }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_SAVE             }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_SAVEAS           }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_STOP             }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_UNDELETE         }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_UNDERLINE        }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_UNDO             }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_UNINDENT         }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_YES              }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_ZOOM_100         }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_ZOOM_FIT         }])),
    wxMenu:append(StockSubMenu,  wxMenuItem:new([{id,    ?wxID_ZOOM_OUT         }])),

    StockSubMenu.

create_dummy_menu() ->
    DummyMenu = wxMenu:new(),
    wxMenu:append(DummyMenu, ?menuID_DUMMY_FIRST, "&First item\tCtrl-F1"),
    wxMenu:appendSeparator(DummyMenu), %% --------------------------
    wxMenu:append(DummyMenu, ?menuID_DUMMY_SECOND, "&Second item\tCtrl-F2"),
    DummyMenu.

loop(State) ->
    receive 
  	#wx{event=#wxClose{}, obj=Frame} ->
  	    io:format("~p Closing window ~n",[self()]),
  	    wxWindow:destroy(Frame); %onClose(Frame); 
	#wx{id=?menuID_FILE_QUIT, obj=Frame, event=#wxCommand{type=command_menu_selected}} ->
  	    io:format("~p Closing window ~n",[self()]),
  	    wxWindow:destroy(Frame); %onClose(Frame); 
  	    
	#wx{obj=Frame, userData=UserData, event=#wxCommand{type=command_menu_selected}} = Wx->
	    logMessage(Frame, "got wx:~p ud:~p~n", [Wx, UserData]),
  	    State1 = onMenuAction(Wx, State),
	    loop(State1);
	Msg ->
	    io:format("Got ~p ~n", [Msg]),
	    loop(State)
    after 5000 ->
	    loop(State)
    end.
    

onMenuAction(#wx{id=?menuID_FILE_CLEAR_LOG, obj=Frame}, #state{} = State) ->
    wxTextCtrl:clear(findLogger(Frame)), 
    State;
    
    
onMenuAction(#wx{id=?menuID_MENUBAR_APPEND, obj=Frame},  #state{} = State) ->
    MenuBar = wxFrame:getMenuBar(Frame),
    wxMenuBar:append(MenuBar, create_dummy_menu(), "DummyMenu"),
    State;

onMenuAction(#wx{id=?menuID_MENUBAR_INSERT, obj=Frame},  #state{} = State) ->
    MenuBar = wxFrame:getMenuBar(Frame),
    wxMenuBar:insert(MenuBar, 0, create_dummy_menu(), "DummyMenu"),
    State;

onMenuAction(#wx{id=?menuID_MENUBAR_DELETE, obj=Frame},  #state{} = State) ->
    MenuBar = wxFrame:getMenuBar(Frame),
    Count = wxMenuBar:getMenuCount(MenuBar),
    if
        (Count > 2) ->
            wxMenuBar:remove(MenuBar, Count -1);
        true ->
            logMessage(Frame, "Cannot delete any more menus~n")
    end,
    State;

onMenuAction(#wx{id=?menuID_MENUBAR_TOGGLE, obj=Frame}, #state{fileMenu=FileMenu} = State) ->
    logMessage(Frame, "onMenubarToggle ~p ~n", [FileMenu]),
    MenuBar = wxFrame:getMenuBar(Frame),
    if 
        FileMenu =:= undefined ->    
            Menu = wxMenuBar:remove(MenuBar, 0), 
            State#state{fileMenu=Menu};
        true ->
            wxMenuBar:insert(MenuBar, 0, FileMenu, "&File"),
            State#state{fileMenu=undefined}
    end;
        
onMenuAction( #wx{id=?menuID_MENUBAR_ENABLE=Id, obj=Frame}, #state{} = State) ->
%%     logMessage(Frame, "onMenubarEnable ~n"),
    MenuBar = wxFrame:getMenuBar(Frame),
    MenuItem = wxMenuBar:findItem(MenuBar, Id),
    Enable = (not wxMenuItem:isCheckable(MenuItem)) orelse wxMenuItem:isChecked(MenuItem),
    
    Count = wxMenuBar:getMenuCount(MenuBar),
    wxMenuBar:enableTop(MenuBar, (Count - 1), Enable), 
    State;


onMenuAction( #wx{id=?menuID_MENUBAR_GET_LABEL, obj=Frame}, #state{} = State) ->
    logMessage(Frame, "onMenubarGetLabel ~n"),
    MenuBar = wxFrame:getMenuBar(Frame),
    Count = wxMenuBar:getMenuCount(MenuBar),
    Label = wxMenuBar:getLabelTop(MenuBar, (Count - 1)),
    logMessage(Frame, "The label of the last menu item is ~p", [Label]),
    State;
    
onMenuAction( #wx{id=?menuID_MENUBAR_SET_LABEL, obj=Frame}, #state{} = State) ->
%%    logMessage(Frame, "onMenubarGetLabel ~n"),
    MenuBar = wxFrame:getMenuBar(Frame),
    Count = wxMenuBar:getMenuCount(MenuBar),
    Label = wxMenuBar:getLabelTop(MenuBar, (Count - 1)),
    TextDialog = wxTextEntryDialog:new(Frame, 
            "Enter new label: ", 
            [{caption, "Change last menu text"}, {value, Label}]),
    wxDialog:showModal(TextDialog),
    NewLabel = wxTextEntryDialog:getValue(TextDialog),
    wxMenuBar:setLabelTop(MenuBar, (Count - 1), NewLabel),
    wxDialog:destroy(TextDialog),
    State;
    
onMenuAction( #wx{id=?menuID_MENUBAR_FIND_MENU, obj=Frame}, #state{} = State) ->
%%    logMessage(Frame, "onMenubarFindMenu ~n"),
    MenuBar = wxFrame:getMenuBar(Frame),
    _Count = wxMenuBar:getMenuCount(MenuBar),
%    Label = wxMenuBar:getLabelTop(MenuBar, (Count - 1)),
    TextDialog = wxTextEntryDialog:new(Frame, 
            "Enter label to search for: ", 
            [{caption, "Find menu"}]),
    wxDialog:showModal(TextDialog),
    Label = wxTextEntryDialog:getValue(TextDialog),
    Len = string:len(Label),
    if
        (Len > 0) ->
            Index = wxMenuBar:findMenu(MenuBar, Label),
            if 
                (Index =:= ?wxNOT_FOUND) ->
                    logMessage(Frame, "Warning: No menu with label ~p", [Label]);
                true ->
                    logMessage(Frame, "Menu ~p has label ~p", [Index, Label])
            end;
        true -> true
    end,
            
    State;
   
onMenuAction(#wx{id=?menuID_MENU_APPEND, obj=Frame}, #state{} = State) ->
    MenuBar = wxFrame:getMenuBar(Frame),
    Count   = wxMenuBar:getMenuCount(MenuBar),
    Menu    = wxMenuBar:getMenu(MenuBar, Count - 1),
    io:format("MenuBar ~p Menu ~p Count ~p ~n", [MenuBar, Menu, Count]),
    wxMenu:appendSeparator(Menu),
    wxMenu:append(Menu, wxMenuItem:new([
            {id,    ?menuID_DUMMY_THIRD},
            {text,  "&Third dummy item\tCtrl-F3"},   
            {kind,  ?wxITEM_CHECK}
            ])),   

    State;

onMenuAction(#wx{id=?menuID_MENU_APPEND_SUB, obj=Frame}, #state{} = State) ->
    MenuBar = wxFrame:getMenuBar(Frame),
    Count   = wxMenuBar:getMenuCount(MenuBar),
    Menu    = wxMenuBar:getMenu(MenuBar, Count - 2),
    wxMenu:appendSeparator(Menu),
    wxMenu:append(Menu, wxMenuItem:new([
            {id,    ?menuID_DUMMY_LAST},
            {text,  "&Dummy sub menu"},   
            {help,  "Dummy sub menu help"},   
            {subMenu,  create_dummy_menu()}
            ])),   

    State;

onMenuAction(#wx{id=Id, obj=Frame}, #state{}=State) when ((Id >= ?menuID_DUMMY_FIRST) and (Id =< ?menuID_DUMMY_LAST)) ->
    logMessage(Frame, "Dummy item #~p ~n", [Id - ?menuID_DUMMY_FIRST + 1]),
    State;

onMenuAction( #wx{id=?menuID_HELP_ABOUT=Id, obj=Frame},  #state{} = State) ->
    showDialog(Id,  Frame),
    State;

onMenuAction( #wx{obj=Frame} = Wx,  State) ->
    logMessage(Frame, "unimplemented menu item event ~p ~n", [Wx]),
    State.
    
logMessage(Frame, Msg) ->
    logMessage(Frame, Msg, []).
    
logMessage(Frame, Msg, ArgList) ->
    String  = lists:flatten(io_lib:format(Msg, ArgList)),
    
    wxTextCtrl:appendText(findLogger(Frame), String).
    
findLogger(Frame) ->
    LogWin = wxWindow:findWindowById(?wID_LOG_TEXT_CTRL, [{parent, Frame}]),
    wx:typeCast(LogWin, wxTextCtrl).
    
   
showDialog(?menuID_HELP_ABOUT,  Frame) ->
    String = lists:flatten(io_lib:format("Welcome to wxErlang 0.97.5.26!~n~n"
		       "This is the minimal wxErlang sample~n"
		       "running under ~p.",
		       [wx_misc:getOsDescription()])),
    MessageDialog = wxMessageDialog:new(Frame,
   			     String,
   			     [{style, ?wxOK bor ?wxICON_INFORMATION}, 
   			      {caption, "About wxErlang minimal sample"}]),

    wxDialog:showModal(MessageDialog),
    wxDialog:destroy(MessageDialog);
    
showDialog(Id,  Frame) ->
    String = lists:flatten(io_lib:format("Unimplemented Dialog ~p", [Id])),
    MessageDialog = wxMessageDialog:new(Frame,
   			     String,
   			     [{style, ?wxOK bor ?wxICON_INFORMATION}, 
   			      {caption, "wxErlang minimal sample"}]),

    wxDialog:showModal(MessageDialog),
    wxDialog:destroy(MessageDialog).
    
