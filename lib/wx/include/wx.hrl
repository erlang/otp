%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

%%  All event messages are encapsulated in a wx record
%%  they contain the widget id and a specialized event record.
%%  Each event record may be sent for one or more event types.
%%  The mapping to wxWidgets is one record per class.

-record(wx, {id   :: integer(),         %% Integer Identity of object.
             obj  :: wx:wx_object(),    %% Object reference that was used in the connect call.
             userData :: term(),        %% User data specified in the connect call.
             event :: event()           %% The event record
            }).

-type wx() :: #wx{}. %% wx event record 
%% Here comes the definitions of all event records.
%% they contain the event type and possible some extra information.

-record(wxActivate,{type :: wxActivateEventType(), %% Callback event: {@link wxActivateEvent}
	active :: boolean()}).
-type wxActivateEventType() :: 'activate' | 'activate_app' | 'hibernate'.
-type wxActivate() :: #wxActivate{}. %% Callback event: {@link wxActivateEvent}

-record(wxAuiManager,{type :: wxAuiManagerEventType(), %% Callback event: {@link wxAuiManagerEvent}
	manager :: wxAuiManager:wxAuiManager(),
	pane :: wxAuiPaneInfo:wxAuiPaneInfo(),
	button :: integer(),
	veto_flag :: boolean(),
	canveto_flag :: boolean(),
	dc :: wxDC:wxDC()}).
-type wxAuiManagerEventType() :: 'aui_pane_button' | 'aui_pane_close' | 'aui_pane_maximize' | 'aui_pane_restore' | 'aui_pane_activated' | 'aui_render' | 'aui_find_manager'.
-type wxAuiManager() :: #wxAuiManager{}. %% Callback event: {@link wxAuiManagerEvent}

-record(wxAuiNotebook,{type :: wxAuiNotebookEventType(), %% Callback event: {@link wxAuiNotebookEvent}
	old_selection :: integer(),
	selection :: integer(),
	drag_source :: wxAuiNotebook:wxAuiNotebook()}).
-type wxAuiNotebookEventType() :: 'command_auinotebook_page_close' | 'command_auinotebook_page_changed' | 'command_auinotebook_page_changing' | 'command_auinotebook_button' | 'command_auinotebook_begin_drag' | 'command_auinotebook_end_drag' | 'command_auinotebook_drag_motion' | 'command_auinotebook_allow_dnd' | 'command_auinotebook_tab_middle_down' | 'command_auinotebook_tab_middle_up' | 'command_auinotebook_tab_right_down' | 'command_auinotebook_tab_right_up' | 'command_auinotebook_page_closed' | 'command_auinotebook_drag_done' | 'command_auinotebook_bg_dclick'.
-type wxAuiNotebook() :: #wxAuiNotebook{}. %% Callback event: {@link wxAuiNotebookEvent}

-record(wxCalendar,{type :: wxCalendarEventType(), %% Callback event: {@link wxCalendarEvent}
	wday :: wx:wx_enum(),
	date :: wx:wx_datetime()}).
-type wxCalendarEventType() :: 'calendar_sel_changed' | 'calendar_day_changed' | 'calendar_month_changed' | 'calendar_year_changed' | 'calendar_doubleclicked' | 'calendar_weekday_clicked'.
-type wxCalendar() :: #wxCalendar{}. %% Callback event: {@link wxCalendarEvent}

-record(wxChildFocus, {type :: wxChildFocusEventType()}). %% Callback event: {@link wxChildFocusEvent}
-type wxChildFocusEventType() :: 'child_focus'.
-type wxChildFocus() :: #wxChildFocus{}. %% Callback event: {@link wxChildFocusEvent}

-record(wxClipboardText, {type :: wxClipboardTextEventType()}). %% Callback event: {@link wxClipboardTextEvent}
-type wxClipboardTextEventType() :: 'command_text_copy' | 'command_text_cut' | 'command_text_paste'.
-type wxClipboardText() :: #wxClipboardText{}. %% Callback event: {@link wxClipboardTextEvent}

-record(wxClose, {type :: wxCloseEventType()}). %% Callback event: {@link wxCloseEvent}
-type wxCloseEventType() :: 'close_window' | 'end_session' | 'query_end_session'.
-type wxClose() :: #wxClose{}. %% Callback event: {@link wxCloseEvent}

-record(wxColourPicker,{type :: wxColourPickerEventType(), %% Callback event: {@link wxColourPickerEvent}
	colour :: wx:wx_colour()}).
-type wxColourPickerEventType() :: 'command_colourpicker_changed'.
-type wxColourPicker() :: #wxColourPicker{}. %% Callback event: {@link wxColourPickerEvent}

-record(wxCommand,{type :: wxCommandEventType(), %% Callback event: {@link wxCommandEvent}
	cmdString :: unicode:chardata(),
	commandInt :: integer(),
	extraLong :: integer()}).
-type wxCommandEventType() :: 'command_button_clicked' | 'command_checkbox_clicked' | 'command_choice_selected' | 'command_listbox_selected' | 'command_listbox_doubleclicked' | 'command_text_updated' | 'command_text_enter' | 'command_menu_selected' | 'command_slider_updated' | 'command_radiobox_selected' | 'command_radiobutton_selected' | 'command_scrollbar_updated' | 'command_vlbox_selected' | 'command_combobox_selected' | 'command_tool_rclicked' | 'command_tool_enter' | 'command_checklistbox_toggled' | 'command_togglebutton_clicked' | 'command_left_click' | 'command_left_dclick' | 'command_right_click' | 'command_set_focus' | 'command_kill_focus' | 'command_enter'.
-type wxCommand() :: #wxCommand{}. %% Callback event: {@link wxCommandEvent}

-record(wxContextMenu,{type :: wxContextMenuEventType(), %% Callback event: {@link wxContextMenuEvent}
	pos :: {X::integer(), Y::integer()}}).
-type wxContextMenuEventType() :: 'context_menu'.
-type wxContextMenu() :: #wxContextMenu{}. %% Callback event: {@link wxContextMenuEvent}

-record(wxDate,{type :: wxDateEventType(), %% Callback event: {@link wxDateEvent}
	date :: wx:wx_datetime()}).
-type wxDateEventType() :: 'date_changed'.
-type wxDate() :: #wxDate{}. %% Callback event: {@link wxDateEvent}

-record(wxDisplayChanged, {type :: wxDisplayChangedEventType()}). %% Callback event: {@link wxDisplayChangedEvent}
-type wxDisplayChangedEventType() :: 'display_changed'.
-type wxDisplayChanged() :: #wxDisplayChanged{}. %% Callback event: {@link wxDisplayChangedEvent}

-record(wxDropFiles,{type :: wxDropFilesEventType(), %% Callback event: {@link wxDropFilesEvent}
	noFiles :: integer(),
	pos :: {X::integer(), Y::integer()},
	files :: [unicode:chardata()]}).
-type wxDropFilesEventType() :: 'drop_files'.
-type wxDropFiles() :: #wxDropFiles{}. %% Callback event: {@link wxDropFilesEvent}

-record(wxErase,{type :: wxEraseEventType(), %% Callback event: {@link wxEraseEvent}
	dc :: wxDC:wxDC()}).
-type wxEraseEventType() :: 'erase_background'.
-type wxErase() :: #wxErase{}. %% Callback event: {@link wxEraseEvent}

-record(wxFileDirPicker,{type :: wxFileDirPickerEventType(), %% Callback event: {@link wxFileDirPickerEvent}
	path :: unicode:chardata()}).
-type wxFileDirPickerEventType() :: 'command_filepicker_changed' | 'command_dirpicker_changed'.
-type wxFileDirPicker() :: #wxFileDirPicker{}. %% Callback event: {@link wxFileDirPickerEvent}

-record(wxFocus,{type :: wxFocusEventType(), %% Callback event: {@link wxFocusEvent}
	win :: wxWindow:wxWindow()}).
-type wxFocusEventType() :: 'set_focus' | 'kill_focus'.
-type wxFocus() :: #wxFocus{}. %% Callback event: {@link wxFocusEvent}

-record(wxFontPicker,{type :: wxFontPickerEventType(), %% Callback event: {@link wxFontPickerEvent}
	font :: wxFont:wxFont()}).
-type wxFontPickerEventType() :: 'command_fontpicker_changed'.
-type wxFontPicker() :: #wxFontPicker{}. %% Callback event: {@link wxFontPickerEvent}

-record(wxGrid,{type :: wxGridEventType(), %% Callback event: {@link wxGridEvent}
	row :: integer(),
	col :: integer(),
	x :: integer(),
	y :: integer(),
	selecting :: boolean(),
	control :: boolean(),
	meta :: boolean(),
	shift :: boolean(),
	alt :: boolean()}).
-type wxGridEventType() :: 'grid_cell_left_click' | 'grid_cell_right_click' | 'grid_cell_left_dclick' | 'grid_cell_right_dclick' | 'grid_label_left_click' | 'grid_label_right_click' | 'grid_label_left_dclick' | 'grid_label_right_dclick' | 'grid_row_size' | 'grid_col_size' | 'grid_range_select' | 'grid_cell_change' | 'grid_select_cell' | 'grid_editor_shown' | 'grid_editor_hidden' | 'grid_editor_created' | 'grid_cell_begin_drag'.
-type wxGrid() :: #wxGrid{}. %% Callback event: {@link wxGridEvent}

-record(wxHelp, {type :: wxHelpEventType()}). %% Callback event: {@link wxHelpEvent}
-type wxHelpEventType() :: 'help' | 'detailed_help'.
-type wxHelp() :: #wxHelp{}. %% Callback event: {@link wxHelpEvent}

-record(wxHtmlLink,{type :: wxHtmlLinkEventType(), %% Callback event: {@link wxHtmlLinkEvent}
	linkInfo :: wx:wx_wxHtmlLinkInfo()}).
-type wxHtmlLinkEventType() :: 'command_html_link_clicked'.
-type wxHtmlLink() :: #wxHtmlLink{}. %% Callback event: {@link wxHtmlLinkEvent}

-record(wxIconize,{type :: wxIconizeEventType(), %% Callback event: {@link wxIconizeEvent}
	iconized :: boolean()}).
-type wxIconizeEventType() :: 'iconize'.
-type wxIconize() :: #wxIconize{}. %% Callback event: {@link wxIconizeEvent}

-record(wxIdle, {type :: wxIdleEventType()}). %% Callback event: {@link wxIdleEvent}
-type wxIdleEventType() :: 'idle'.
-type wxIdle() :: #wxIdle{}. %% Callback event: {@link wxIdleEvent}

-record(wxInitDialog, {type :: wxInitDialogEventType()}). %% Callback event: {@link wxInitDialogEvent}
-type wxInitDialogEventType() :: 'init_dialog'.
-type wxInitDialog() :: #wxInitDialog{}. %% Callback event: {@link wxInitDialogEvent}

-record(wxJoystick,{type :: wxJoystickEventType(), %% Callback event: {@link wxJoystickEvent}
	pos :: {X::integer(), Y::integer()},
	zPosition :: integer(),
	buttonChange :: integer(),
	buttonState :: integer(),
	joyStick :: integer()}).
-type wxJoystickEventType() :: 'joy_button_down' | 'joy_button_up' | 'joy_move' | 'joy_zmove'.
-type wxJoystick() :: #wxJoystick{}. %% Callback event: {@link wxJoystickEvent}

-record(wxKey,{type :: wxKeyEventType(), %% Callback event: {@link wxKeyEvent}
	x :: integer(),
	y :: integer(),
	keyCode :: integer(),
	controlDown :: boolean(),
	shiftDown :: boolean(),
	altDown :: boolean(),
	metaDown :: boolean(),
	scanCode :: boolean(),
	uniChar :: integer(),
	rawCode :: integer(),
	rawFlags :: integer()}).
-type wxKeyEventType() :: 'char' | 'char_hook' | 'key_down' | 'key_up'.
-type wxKey() :: #wxKey{}. %% Callback event: {@link wxKeyEvent}

-record(wxList,{type :: wxListEventType(), %% Callback event: {@link wxListEvent}
	code :: integer(),
	oldItemIndex :: integer(),
	itemIndex :: integer(),
	col :: integer(),
	pointDrag :: {X::integer(), Y::integer()}}).
-type wxListEventType() :: 'command_list_begin_drag' | 'command_list_begin_rdrag' | 'command_list_begin_label_edit' | 'command_list_end_label_edit' | 'command_list_delete_item' | 'command_list_delete_all_items' | 'command_list_key_down' | 'command_list_insert_item' | 'command_list_col_click' | 'command_list_col_right_click' | 'command_list_col_begin_drag' | 'command_list_col_dragging' | 'command_list_col_end_drag' | 'command_list_item_selected' | 'command_list_item_deselected' | 'command_list_item_right_click' | 'command_list_item_middle_click' | 'command_list_item_activated' | 'command_list_item_focused' | 'command_list_cache_hint'.
-type wxList() :: #wxList{}. %% Callback event: {@link wxListEvent}

-record(wxMaximize, {type :: wxMaximizeEventType()}). %% Callback event: {@link wxMaximizeEvent}
-type wxMaximizeEventType() :: 'maximize'.
-type wxMaximize() :: #wxMaximize{}. %% Callback event: {@link wxMaximizeEvent}

-record(wxMenu,{type :: wxMenuEventType(), %% Callback event: {@link wxMenuEvent}
	menuId :: integer(),
	menu :: wxMenu:wxMenu()}).
-type wxMenuEventType() :: 'menu_open' | 'menu_close' | 'menu_highlight'.
-type wxMenu() :: #wxMenu{}. %% Callback event: {@link wxMenuEvent}

-record(wxMouseCaptureChanged, {type :: wxMouseCaptureChangedEventType()}). %% Callback event: {@link wxMouseCaptureChangedEvent}
-type wxMouseCaptureChangedEventType() :: 'mouse_capture_changed'.
-type wxMouseCaptureChanged() :: #wxMouseCaptureChanged{}. %% Callback event: {@link wxMouseCaptureChangedEvent}

-record(wxMouseCaptureLost, {type :: wxMouseCaptureLostEventType()}). %% Callback event: {@link wxMouseCaptureLostEvent}
-type wxMouseCaptureLostEventType() :: 'mouse_capture_lost'.
-type wxMouseCaptureLost() :: #wxMouseCaptureLost{}. %% Callback event: {@link wxMouseCaptureLostEvent}

-record(wxMouse,{type :: wxMouseEventType(), %% Callback event: {@link wxMouseEvent}
	x :: integer(),
	y :: integer(),
	leftDown :: boolean(),
	middleDown :: boolean(),
	rightDown :: boolean(),
	controlDown :: boolean(),
	shiftDown :: boolean(),
	altDown :: boolean(),
	metaDown :: boolean(),
	wheelRotation :: integer(),
	wheelDelta :: integer(),
	linesPerAction :: integer()}).
-type wxMouseEventType() :: 'left_down' | 'left_up' | 'middle_down' | 'middle_up' | 'right_down' | 'right_up' | 'motion' | 'enter_window' | 'leave_window' | 'left_dclick' | 'middle_dclick' | 'right_dclick' | 'mousewheel'.
-type wxMouse() :: #wxMouse{}. %% Callback event: {@link wxMouseEvent}

-record(wxMove,{type :: wxMoveEventType(), %% Callback event: {@link wxMoveEvent}
	pos :: {X::integer(), Y::integer()},
	rect :: {X::integer(), Y::integer(), W::integer(), H::integer()}}).
-type wxMoveEventType() :: 'move'.
-type wxMove() :: #wxMove{}. %% Callback event: {@link wxMoveEvent}

-record(wxNavigationKey,{type :: wxNavigationKeyEventType(), %% Callback event: {@link wxNavigationKeyEvent}
	flags :: integer(),
	focus :: wxWindow:wxWindow()}).
-type wxNavigationKeyEventType() :: 'navigation_key'.
-type wxNavigationKey() :: #wxNavigationKey{}. %% Callback event: {@link wxNavigationKeyEvent}

-record(wxNotebook,{type :: wxNotebookEventType(), %% Callback event: {@link wxNotebookEvent}
	nSel :: integer(),
	nOldSel :: integer()}).
-type wxNotebookEventType() :: 'command_notebook_page_changed' | 'command_notebook_page_changing'.
-type wxNotebook() :: #wxNotebook{}. %% Callback event: {@link wxNotebookEvent}

-record(wxPaint, {type :: wxPaintEventType()}). %% Callback event: {@link wxPaintEvent}
-type wxPaintEventType() :: 'paint'.
-type wxPaint() :: #wxPaint{}. %% Callback event: {@link wxPaintEvent}

-record(wxPaletteChanged, {type :: wxPaletteChangedEventType()}). %% Callback event: {@link wxPaletteChangedEvent}
-type wxPaletteChangedEventType() :: 'palette_changed'.
-type wxPaletteChanged() :: #wxPaletteChanged{}. %% Callback event: {@link wxPaletteChangedEvent}

-record(wxQueryNewPalette, {type :: wxQueryNewPaletteEventType()}). %% Callback event: {@link wxQueryNewPaletteEvent}
-type wxQueryNewPaletteEventType() :: 'query_new_palette'.
-type wxQueryNewPalette() :: #wxQueryNewPalette{}. %% Callback event: {@link wxQueryNewPaletteEvent}

-record(wxSash,{type :: wxSashEventType(), %% Callback event: {@link wxSashEvent}
	edge :: wx:wx_enum(),
	dragRect :: {X::integer(), Y::integer(), W::integer(), H::integer()},
	dragStatus :: wx:wx_enum()}).
-type wxSashEventType() :: 'sash_dragged'.
-type wxSash() :: #wxSash{}. %% Callback event: {@link wxSashEvent}

-record(wxScroll,{type :: wxScrollEventType(), %% Callback event: {@link wxScrollEvent}
	commandInt :: integer(),
	extraLong :: integer()}).
-type wxScrollEventType() :: 'scroll_top' | 'scroll_bottom' | 'scroll_lineup' | 'scroll_linedown' | 'scroll_pageup' | 'scroll_pagedown' | 'scroll_thumbtrack' | 'scroll_thumbrelease' | 'scroll_changed'.
-type wxScroll() :: #wxScroll{}. %% Callback event: {@link wxScrollEvent}

-record(wxScrollWin,{type :: wxScrollWinEventType(), %% Callback event: {@link wxScrollWinEvent}
	commandInt :: integer(),
	extraLong :: integer()}).
-type wxScrollWinEventType() :: 'scrollwin_top' | 'scrollwin_bottom' | 'scrollwin_lineup' | 'scrollwin_linedown' | 'scrollwin_pageup' | 'scrollwin_pagedown' | 'scrollwin_thumbtrack' | 'scrollwin_thumbrelease'.
-type wxScrollWin() :: #wxScrollWin{}. %% Callback event: {@link wxScrollWinEvent}

-record(wxSetCursor,{type :: wxSetCursorEventType(), %% Callback event: {@link wxSetCursorEvent}
	x :: integer(),
	y :: integer(),
	cursor :: wxCursor:wxCursor()}).
-type wxSetCursorEventType() :: 'set_cursor'.
-type wxSetCursor() :: #wxSetCursor{}. %% Callback event: {@link wxSetCursorEvent}

-record(wxShow,{type :: wxShowEventType(), %% Callback event: {@link wxShowEvent}
	show :: boolean()}).
-type wxShowEventType() :: 'show'.
-type wxShow() :: #wxShow{}. %% Callback event: {@link wxShowEvent}

-record(wxSize,{type :: wxSizeEventType(), %% Callback event: {@link wxSizeEvent}
	size :: {W::integer(), H::integer()},
	rect :: {X::integer(), Y::integer(), W::integer(), H::integer()}}).
-type wxSizeEventType() :: 'size'.
-type wxSize() :: #wxSize{}. %% Callback event: {@link wxSizeEvent}

-record(wxSpin,{type :: wxSpinEventType(), %% Callback event: {@link wxSpinEvent}
	commandInt :: integer()}).
-type wxSpinEventType() :: 'command_spinctrl_updated' | 'spin_up' | 'spin_down' | 'spin'.
-type wxSpin() :: #wxSpin{}. %% Callback event: {@link wxSpinEvent}

-record(wxSplitter, {type :: wxSplitterEventType()}). %% Callback event: {@link wxSplitterEvent}
-type wxSplitterEventType() :: 'command_splitter_sash_pos_changed' | 'command_splitter_sash_pos_changing' | 'command_splitter_doubleclicked' | 'command_splitter_unsplit'.
-type wxSplitter() :: #wxSplitter{}. %% Callback event: {@link wxSplitterEvent}

-record(wxStyledText,{type :: wxStyledTextEventType(), %% Callback event: {@link wxStyledTextEvent}
	position :: integer(),
	key :: integer(),
	modifiers :: integer(),
	modificationType :: integer(),
	text :: unicode:chardata(),
	length :: integer(),
	linesAdded :: integer(),
	line :: integer(),
	foldLevelNow :: integer(),
	foldLevelPrev :: integer(),
	margin :: integer(),
	message :: integer(),
	wParam :: integer(),
	lParam :: integer(),
	listType :: integer(),
	x :: integer(),
	y :: integer(),
	dragText :: unicode:chardata(),
	dragAllowMove :: boolean(),
	dragResult :: wx:wx_enum()}).
-type wxStyledTextEventType() :: 'stc_change' | 'stc_styleneeded' | 'stc_charadded' | 'stc_savepointreached' | 'stc_savepointleft' | 'stc_romodifyattempt' | 'stc_key' | 'stc_doubleclick' | 'stc_updateui' | 'stc_modified' | 'stc_macrorecord' | 'stc_marginclick' | 'stc_needshown' | 'stc_painted' | 'stc_userlistselection' | 'stc_uridropped' | 'stc_dwellstart' | 'stc_dwellend' | 'stc_start_drag' | 'stc_drag_over' | 'stc_do_drop' | 'stc_zoom' | 'stc_hotspot_click' | 'stc_hotspot_dclick' | 'stc_calltip_click' | 'stc_autocomp_selection'.
-type wxStyledText() :: #wxStyledText{}. %% Callback event: {@link wxStyledTextEvent}

-record(wxSysColourChanged, {type :: wxSysColourChangedEventType()}). %% Callback event: {@link wxSysColourChangedEvent}
-type wxSysColourChangedEventType() :: 'sys_colour_changed'.
-type wxSysColourChanged() :: #wxSysColourChanged{}. %% Callback event: {@link wxSysColourChangedEvent}

-record(wxTaskBarIcon, {type :: wxTaskBarIconEventType()}). %% Callback event: {@link wxTaskBarIconEvent}
-type wxTaskBarIconEventType() :: 'taskbar_move' | 'taskbar_left_down' | 'taskbar_left_up' | 'taskbar_right_down' | 'taskbar_right_up' | 'taskbar_left_dclick' | 'taskbar_right_dclick'.
-type wxTaskBarIcon() :: #wxTaskBarIcon{}. %% Callback event: {@link wxTaskBarIconEvent}

-record(wxTree,{type :: wxTreeEventType(), %% Callback event: {@link wxTreeEvent}
	item :: integer(),
	itemOld :: integer(),
	pointDrag :: {X::integer(), Y::integer()}}).
-type wxTreeEventType() :: 'command_tree_begin_drag' | 'command_tree_begin_rdrag' | 'command_tree_begin_label_edit' | 'command_tree_end_label_edit' | 'command_tree_delete_item' | 'command_tree_get_info' | 'command_tree_set_info' | 'command_tree_item_expanded' | 'command_tree_item_expanding' | 'command_tree_item_collapsed' | 'command_tree_item_collapsing' | 'command_tree_sel_changed' | 'command_tree_sel_changing' | 'command_tree_key_down' | 'command_tree_item_activated' | 'command_tree_item_right_click' | 'command_tree_item_middle_click' | 'command_tree_end_drag' | 'command_tree_state_image_click' | 'command_tree_item_gettooltip' | 'command_tree_item_menu'.
-type wxTree() :: #wxTree{}. %% Callback event: {@link wxTreeEvent}

-record(wxUpdateUI, {type :: wxUpdateUIEventType()}). %% Callback event: {@link wxUpdateUIEvent}
-type wxUpdateUIEventType() :: 'update_ui'.
-type wxUpdateUI() :: #wxUpdateUI{}. %% Callback event: {@link wxUpdateUIEvent}

-record(wxWindowCreate, {type :: wxWindowCreateEventType()}). %% Callback event: {@link wxWindowCreateEvent}
-type wxWindowCreateEventType() :: 'create'.
-type wxWindowCreate() :: #wxWindowCreate{}. %% Callback event: {@link wxWindowCreateEvent}

-record(wxWindowDestroy, {type :: wxWindowDestroyEventType()}). %% Callback event: {@link wxWindowDestroyEvent}
-type wxWindowDestroyEventType() :: 'destroy'.
-type wxWindowDestroy() :: #wxWindowDestroy{}. %% Callback event: {@link wxWindowDestroyEvent}

-type event() :: wxActivate() | wxAuiManager() | wxAuiNotebook() | wxCalendar() | wxChildFocus() | wxClipboardText() | wxClose() | wxColourPicker() | wxCommand() | wxContextMenu() | wxDate() | wxDisplayChanged() | wxDropFiles() | wxErase() | wxFileDirPicker() | wxFocus() | wxFontPicker() | wxGrid() | wxHelp() | wxHtmlLink() | wxIconize() | wxIdle() | wxInitDialog() | wxJoystick() | wxKey() | wxList() | wxMaximize() | wxMenu() | wxMouse() | wxMouseCaptureChanged() | wxMouseCaptureLost() | wxMove() | wxNavigationKey() | wxNotebook() | wxPaint() | wxPaletteChanged() | wxQueryNewPalette() | wxSash() | wxScroll() | wxScrollWin() | wxSetCursor() | wxShow() | wxSize() | wxSpin() | wxSplitter() | wxStyledText() | wxSysColourChanged() | wxTaskBarIcon() | wxTree() | wxUpdateUI() | wxWindowCreate() | wxWindowDestroy().
-type wxEventType() :: wxActivateEventType() | wxAuiManagerEventType() | wxAuiNotebookEventType() | wxCalendarEventType() | wxChildFocusEventType() | wxClipboardTextEventType() | wxCloseEventType() | wxColourPickerEventType() | wxCommandEventType() | wxContextMenuEventType() | wxDateEventType() | wxDisplayChangedEventType() | wxDropFilesEventType() | wxEraseEventType() | wxFileDirPickerEventType() | wxFocusEventType() | wxFontPickerEventType() | wxGridEventType() | wxHelpEventType() | wxHtmlLinkEventType() | wxIconizeEventType() | wxIdleEventType() | wxInitDialogEventType() | wxJoystickEventType() | wxKeyEventType() | wxListEventType() | wxMaximizeEventType() | wxMenuEventType() | wxMouseCaptureChangedEventType() | wxMouseCaptureLostEventType() | wxMouseEventType() | wxMoveEventType() | wxNavigationKeyEventType() | wxNotebookEventType() | wxPaintEventType() | wxPaletteChangedEventType() | wxQueryNewPaletteEventType() | wxSashEventType() | wxScrollEventType() | wxScrollWinEventType() | wxSetCursorEventType() | wxShowEventType() | wxSizeEventType() | wxSpinEventType() | wxSplitterEventType() | wxStyledTextEventType() | wxSysColourChangedEventType() | wxTaskBarIconEventType() | wxTreeEventType() | wxUpdateUIEventType() | wxWindowCreateEventType() | wxWindowDestroyEventType().

%% Hardcoded Records
-record(wxMouseState, {x :: integer(), y :: integer(),
          leftDown :: boolean(), middleDown :: boolean(), rightDown :: boolean(), 
          controlDown :: boolean(), shiftDown :: boolean(),
          altDown :: boolean(), metaDown :: boolean(), cmdDown :: boolean()
        }).
-record(wxHtmlLinkInfo, {
          href :: unicode:chardata(), target :: unicode:chardata()
        }).

%% Hardcoded Defines
-define(wxDefaultSize, {-1,-1}).
-define(wxDefaultPosition, {-1,-1}).

%% Global Variables
-define(wxBLACK,  wxe_util:get_const(wxBLACK)).
-define(wxBLACK_BRUSH,  wxe_util:get_const(wxBLACK_BRUSH)).
-define(wxBLACK_DASHED_PEN,  wxe_util:get_const(wxBLACK_DASHED_PEN)).
-define(wxBLACK_PEN,  wxe_util:get_const(wxBLACK_PEN)).
-define(wxBLUE,  wxe_util:get_const(wxBLUE)).
-define(wxBLUE_BRUSH,  wxe_util:get_const(wxBLUE_BRUSH)).
-define(wxCROSS_CURSOR,  wxe_util:get_const(wxCROSS_CURSOR)).
-define(wxCYAN,  wxe_util:get_const(wxCYAN)).
-define(wxCYAN_BRUSH,  wxe_util:get_const(wxCYAN_BRUSH)).
-define(wxCYAN_PEN,  wxe_util:get_const(wxCYAN_PEN)).
-define(wxGREEN,  wxe_util:get_const(wxGREEN)).
-define(wxGREEN_BRUSH,  wxe_util:get_const(wxGREEN_BRUSH)).
-define(wxGREEN_PEN,  wxe_util:get_const(wxGREEN_PEN)).
-define(wxGREY_BRUSH,  wxe_util:get_const(wxGREY_BRUSH)).
-define(wxGREY_PEN,  wxe_util:get_const(wxGREY_PEN)).
-define(wxHOURGLASS_CURSOR,  wxe_util:get_const(wxHOURGLASS_CURSOR)).
-define(wxITALIC_FONT,  wxe_util:get_const(wxITALIC_FONT)).
-define(wxLIGHT_GREY,  wxe_util:get_const(wxLIGHT_GREY)).
-define(wxLIGHT_GREY_BRUSH,  wxe_util:get_const(wxLIGHT_GREY_BRUSH)).
-define(wxLIGHT_GREY_PEN,  wxe_util:get_const(wxLIGHT_GREY_PEN)).
-define(wxMEDIUM_GREY_BRUSH,  wxe_util:get_const(wxMEDIUM_GREY_BRUSH)).
-define(wxMEDIUM_GREY_PEN,  wxe_util:get_const(wxMEDIUM_GREY_PEN)).
-define(wxNORMAL_FONT,  wxe_util:get_const(wxNORMAL_FONT)).
-define(wxNullBitmap,  wxe_util:get_const(wxNullBitmap)).
-define(wxNullBrush,  wxe_util:get_const(wxNullBrush)).
-define(wxNullCursor,  wxe_util:get_const(wxNullCursor)).
-define(wxNullFont,  wxe_util:get_const(wxNullFont)).
-define(wxNullIcon,  wxe_util:get_const(wxNullIcon)).
-define(wxNullPalette,  wxe_util:get_const(wxNullPalette)).
-define(wxNullPen,  wxe_util:get_const(wxNullPen)).
-define(wxRED,  wxe_util:get_const(wxRED)).
-define(wxRED_BRUSH,  wxe_util:get_const(wxRED_BRUSH)).
-define(wxRED_PEN,  wxe_util:get_const(wxRED_PEN)).
-define(wxSMALL_FONT,  wxe_util:get_const(wxSMALL_FONT)).
-define(wxSTANDARD_CURSOR,  wxe_util:get_const(wxSTANDARD_CURSOR)).
-define(wxSWISS_FONT,  wxe_util:get_const(wxSWISS_FONT)).
-define(wxTRANSPARENT_BRUSH,  wxe_util:get_const(wxTRANSPARENT_BRUSH)).
-define(wxTRANSPARENT_PEN,  wxe_util:get_const(wxTRANSPARENT_PEN)).
-define(wxWHITE,  wxe_util:get_const(wxWHITE)).
-define(wxWHITE_BRUSH,  wxe_util:get_const(wxWHITE_BRUSH)).
-define(wxWHITE_PEN,  wxe_util:get_const(wxWHITE_PEN)).

%% Enum and defines
% From class wxAuiManager
-define(wxAuiManager_actionNone, 0).
-define(wxAuiManager_actionResize, 1).
-define(wxAuiManager_actionClickButton, 2).
-define(wxAuiManager_actionClickCaption, 3).
-define(wxAuiManager_actionDragToolbarPane, 4).
-define(wxAuiManager_actionDragFloatingPane, 5).
% From class wxAuiPaneInfo::wxAuiPaneState
-define(wxAuiPaneInfo_optionFloating, 1).
-define(wxAuiPaneInfo_optionHidden, 2).
-define(wxAuiPaneInfo_optionLeftDockable, 4).
-define(wxAuiPaneInfo_optionRightDockable, 8).
-define(wxAuiPaneInfo_optionTopDockable, 16).
-define(wxAuiPaneInfo_optionBottomDockable, 32).
-define(wxAuiPaneInfo_optionFloatable, 64).
-define(wxAuiPaneInfo_optionMovable, 128).
-define(wxAuiPaneInfo_optionResizable, 256).
-define(wxAuiPaneInfo_optionPaneBorder, 512).
-define(wxAuiPaneInfo_optionCaption, 1024).
-define(wxAuiPaneInfo_optionGripper, 2048).
-define(wxAuiPaneInfo_optionDestroyOnClose, 4096).
-define(wxAuiPaneInfo_optionToolbar, 8192).
-define(wxAuiPaneInfo_optionActive, 16384).
-define(wxAuiPaneInfo_optionGripperTop, 32768).
-define(wxAuiPaneInfo_optionMaximized, 65536).
-define(wxAuiPaneInfo_optionDockFixed, 131072).
-define(wxAuiPaneInfo_buttonClose, 2097152).
-define(wxAuiPaneInfo_buttonMaximize, 4194304).
-define(wxAuiPaneInfo_buttonMinimize, 8388608).
-define(wxAuiPaneInfo_buttonPin, 16777216).
-define(wxAuiPaneInfo_buttonCustom1, 67108864).
-define(wxAuiPaneInfo_buttonCustom2, 134217728).
-define(wxAuiPaneInfo_buttonCustom3, 268435456).
-define(wxAuiPaneInfo_savedHiddenState, 1073741824).
-define(wxAuiPaneInfo_actionPane, 2147483648).
% From class wxBitmap::Representation
-define(wxBitmap_Pixmap, 0).
-define(wxBitmap_Pixbuf, 1).
% From class wxChoicebook
-define(wxChoicebook_SetSelection_SendEvent, 1).
% From class wxDateTime::Calendar
-define(wxDateTime_Gregorian, 0).
-define(wxDateTime_Julian, 1).
% From class wxDateTime::Country
-define(wxDateTime_Country_Unknown, 0).
-define(wxDateTime_Country_Default, 1).
-define(wxDateTime_Country_WesternEurope_Start, 2).
-define(wxDateTime_Country_EEC, ?wxDateTime_Country_WesternEurope_Start).
-define(wxDateTime_France, (?wxDateTime_Country_WesternEurope_Start+1)).
-define(wxDateTime_Germany, (?wxDateTime_Country_WesternEurope_Start+2)).
-define(wxDateTime_UK, (?wxDateTime_Country_WesternEurope_Start+3)).
-define(wxDateTime_Country_WesternEurope_End, ?wxDateTime_UK).
-define(wxDateTime_Russia, (?wxDateTime_UK+1)).
-define(wxDateTime_USA, (?wxDateTime_UK+2)).
% From class wxDateTime::GregorianAdoption
-define(wxDateTime_Gr_Unknown, 0).
-define(wxDateTime_Gr_Standard, 1).
-define(wxDateTime_Gr_Alaska, 2).
-define(wxDateTime_Gr_Albania, 3).
-define(wxDateTime_Gr_Austria, ?wxDateTime_Gr_Unknown).
-define(wxDateTime_Gr_Austria_Brixen, (?wxDateTime_Gr_Unknown+1)).
-define(wxDateTime_Gr_Austria_Salzburg, ?wxDateTime_Gr_Austria_Brixen).
-define(wxDateTime_Gr_Austria_Tyrol, ?wxDateTime_Gr_Austria_Brixen).
-define(wxDateTime_Gr_Austria_Carinthia, (?wxDateTime_Gr_Austria_Brixen+1)).
-define(wxDateTime_Gr_Austria_Styria, ?wxDateTime_Gr_Austria_Carinthia).
-define(wxDateTime_Gr_Belgium, (?wxDateTime_Gr_Austria_Carinthia+1)).
-define(wxDateTime_Gr_Bulgaria, ?wxDateTime_Gr_Unknown).
-define(wxDateTime_Gr_Bulgaria_1, (?wxDateTime_Gr_Unknown+1)).
-define(wxDateTime_Gr_Bulgaria_2, (?wxDateTime_Gr_Unknown+2)).
-define(wxDateTime_Gr_Bulgaria_3, (?wxDateTime_Gr_Unknown+3)).
-define(wxDateTime_Gr_Canada, ?wxDateTime_Gr_Unknown).
-define(wxDateTime_Gr_China, ?wxDateTime_Gr_Unknown).
-define(wxDateTime_Gr_China_1, (?wxDateTime_Gr_Unknown+1)).
-define(wxDateTime_Gr_China_2, (?wxDateTime_Gr_Unknown+2)).
-define(wxDateTime_Gr_Czechoslovakia, (?wxDateTime_Gr_Unknown+3)).
-define(wxDateTime_Gr_Denmark, (?wxDateTime_Gr_Unknown+4)).
-define(wxDateTime_Gr_Egypt, (?wxDateTime_Gr_Unknown+5)).
-define(wxDateTime_Gr_Estonia, (?wxDateTime_Gr_Unknown+6)).
-define(wxDateTime_Gr_Finland, (?wxDateTime_Gr_Unknown+7)).
-define(wxDateTime_Gr_France, (?wxDateTime_Gr_Unknown+8)).
-define(wxDateTime_Gr_France_Alsace, (?wxDateTime_Gr_Unknown+9)).
-define(wxDateTime_Gr_France_Lorraine, (?wxDateTime_Gr_Unknown+10)).
-define(wxDateTime_Gr_France_Strasbourg, (?wxDateTime_Gr_Unknown+11)).
-define(wxDateTime_Gr_Germany, ?wxDateTime_Gr_Unknown).
-define(wxDateTime_Gr_Germany_Catholic, (?wxDateTime_Gr_Unknown+1)).
-define(wxDateTime_Gr_Germany_Prussia, (?wxDateTime_Gr_Unknown+2)).
-define(wxDateTime_Gr_Germany_Protestant, (?wxDateTime_Gr_Unknown+3)).
-define(wxDateTime_Gr_GreatBritain, (?wxDateTime_Gr_Unknown+4)).
-define(wxDateTime_Gr_Greece, (?wxDateTime_Gr_Unknown+5)).
-define(wxDateTime_Gr_Hungary, (?wxDateTime_Gr_Unknown+6)).
-define(wxDateTime_Gr_Ireland, ?wxDateTime_Gr_GreatBritain).
-define(wxDateTime_Gr_Italy, ?wxDateTime_Gr_Standard).
-define(wxDateTime_Gr_Japan, ?wxDateTime_Gr_Unknown).
-define(wxDateTime_Gr_Japan_1, (?wxDateTime_Gr_Unknown+1)).
-define(wxDateTime_Gr_Japan_2, (?wxDateTime_Gr_Unknown+2)).
-define(wxDateTime_Gr_Japan_3, (?wxDateTime_Gr_Unknown+3)).
-define(wxDateTime_Gr_Latvia, (?wxDateTime_Gr_Unknown+4)).
-define(wxDateTime_Gr_Lithuania, (?wxDateTime_Gr_Unknown+5)).
-define(wxDateTime_Gr_Luxemburg, (?wxDateTime_Gr_Unknown+6)).
-define(wxDateTime_Gr_Netherlands, ?wxDateTime_Gr_Belgium).
-define(wxDateTime_Gr_Netherlands_Groningen, (?wxDateTime_Gr_Belgium+1)).
-define(wxDateTime_Gr_Netherlands_Gelderland, (?wxDateTime_Gr_Belgium+2)).
-define(wxDateTime_Gr_Netherlands_Utrecht, (?wxDateTime_Gr_Belgium+3)).
-define(wxDateTime_Gr_Netherlands_Friesland, (?wxDateTime_Gr_Belgium+4)).
-define(wxDateTime_Gr_Norway, ?wxDateTime_Gr_Denmark).
-define(wxDateTime_Gr_Poland, ?wxDateTime_Gr_Standard).
-define(wxDateTime_Gr_Portugal, ?wxDateTime_Gr_Standard).
-define(wxDateTime_Gr_Romania, (?wxDateTime_Gr_Standard+1)).
-define(wxDateTime_Gr_Russia, (?wxDateTime_Gr_Standard+2)).
-define(wxDateTime_Gr_Scotland, ?wxDateTime_Gr_GreatBritain).
-define(wxDateTime_Gr_Spain, ?wxDateTime_Gr_Standard).
-define(wxDateTime_Gr_Sweden, ?wxDateTime_Gr_Finland).
-define(wxDateTime_Gr_Switzerland, ?wxDateTime_Gr_Unknown).
-define(wxDateTime_Gr_Switzerland_Catholic, (?wxDateTime_Gr_Unknown+1)).
-define(wxDateTime_Gr_Switzerland_Protestant, (?wxDateTime_Gr_Unknown+2)).
-define(wxDateTime_Gr_Turkey, (?wxDateTime_Gr_Unknown+3)).
-define(wxDateTime_Gr_USA, ?wxDateTime_Gr_GreatBritain).
-define(wxDateTime_Gr_Wales, ?wxDateTime_Gr_GreatBritain).
-define(wxDateTime_Gr_Yugoslavia, (?wxDateTime_Gr_GreatBritain+1)).
% From class wxDateTime::Month
-define(wxDateTime_Jan, 0).
-define(wxDateTime_Feb, 1).
-define(wxDateTime_Mar, 2).
-define(wxDateTime_Apr, 3).
-define(wxDateTime_May, 4).
-define(wxDateTime_Jun, 5).
-define(wxDateTime_Jul, 6).
-define(wxDateTime_Aug, 7).
-define(wxDateTime_Sep, 8).
-define(wxDateTime_Oct, 9).
-define(wxDateTime_Nov, 10).
-define(wxDateTime_Dec, 11).
-define(wxDateTime_Inv_Month, 12).
% From class wxDateTime::NameFlags
-define(wxDateTime_Name_Full, 1).
-define(wxDateTime_Name_Abbr, 2).
% From class wxDateTime::TZ
-define(wxDateTime_Local, 0).
-define(wxDateTime_GMT_12, 1).
-define(wxDateTime_GMT_11, 2).
-define(wxDateTime_GMT_10, 3).
-define(wxDateTime_GMT_9, 4).
-define(wxDateTime_GMT_8, 5).
-define(wxDateTime_GMT_7, 6).
-define(wxDateTime_GMT_6, 7).
-define(wxDateTime_GMT_5, 8).
-define(wxDateTime_GMT_4, 9).
-define(wxDateTime_GMT_3, 10).
-define(wxDateTime_GMT_2, 11).
-define(wxDateTime_GMT_1, 12).
-define(wxDateTime_GMT0, 13).
-define(wxDateTime_GMT1, 14).
-define(wxDateTime_GMT2, 15).
-define(wxDateTime_GMT3, 16).
-define(wxDateTime_GMT4, 17).
-define(wxDateTime_GMT5, 18).
-define(wxDateTime_GMT6, 19).
-define(wxDateTime_GMT7, 20).
-define(wxDateTime_GMT8, 21).
-define(wxDateTime_GMT9, 22).
-define(wxDateTime_GMT10, 23).
-define(wxDateTime_GMT11, 24).
-define(wxDateTime_GMT12, 25).
-define(wxDateTime_GMT13, 26).
-define(wxDateTime_WET, ?wxDateTime_GMT0).
-define(wxDateTime_WEST, ?wxDateTime_GMT1).
-define(wxDateTime_CET, ?wxDateTime_GMT1).
-define(wxDateTime_CEST, ?wxDateTime_GMT2).
-define(wxDateTime_EET, ?wxDateTime_GMT2).
-define(wxDateTime_EEST, ?wxDateTime_GMT3).
-define(wxDateTime_MSK, ?wxDateTime_GMT3).
-define(wxDateTime_MSD, ?wxDateTime_GMT4).
-define(wxDateTime_AST, ?wxDateTime_GMT_4).
-define(wxDateTime_ADT, ?wxDateTime_GMT_3).
-define(wxDateTime_EST, ?wxDateTime_GMT_5).
-define(wxDateTime_EDT, ?wxDateTime_GMT_4).
-define(wxDateTime_CST, ?wxDateTime_GMT_6).
-define(wxDateTime_CDT, ?wxDateTime_GMT_5).
-define(wxDateTime_MST, ?wxDateTime_GMT_7).
-define(wxDateTime_MDT, ?wxDateTime_GMT_6).
-define(wxDateTime_PST, ?wxDateTime_GMT_8).
-define(wxDateTime_PDT, ?wxDateTime_GMT_7).
-define(wxDateTime_HST, ?wxDateTime_GMT_10).
-define(wxDateTime_AKST, ?wxDateTime_GMT_9).
-define(wxDateTime_AKDT, ?wxDateTime_GMT_8).
-define(wxDateTime_A_WST, ?wxDateTime_GMT8).
-define(wxDateTime_A_CST, (?wxDateTime_GMT13+1)).
-define(wxDateTime_A_EST, ?wxDateTime_GMT10).
-define(wxDateTime_A_ESST, ?wxDateTime_GMT11).
-define(wxDateTime_NZST, ?wxDateTime_GMT12).
-define(wxDateTime_NZDT, ?wxDateTime_GMT13).
-define(wxDateTime_UTC, ?wxDateTime_GMT0).
% From class wxDateTime::WeekDay
-define(wxDateTime_Sun, 0).
-define(wxDateTime_Mon, 1).
-define(wxDateTime_Tue, 2).
-define(wxDateTime_Wed, 3).
-define(wxDateTime_Thu, 4).
-define(wxDateTime_Fri, 5).
-define(wxDateTime_Sat, 6).
-define(wxDateTime_Inv_WeekDay, 7).
% From class wxDateTime::WeekFlags
-define(wxDateTime_Default_First, 0).
-define(wxDateTime_Monday_First, 1).
-define(wxDateTime_Sunday_First, 2).
% From class wxDateTime::Year
% From class wxDialog
-define(wxDialog_ButtonSizerFlags, (?wxOK bor ?wxCANCEL bor ?wxYES bor ?wxNO bor ?wxHELP bor ?wxNO_DEFAULT)).
% From class wxGrid
-define(wxGrid_wxGRID_CELLCTRL, 2000).
-define(wxGrid_wxGRID_TOPCTRL, 2001).
% From class wxGrid
-define(wxGrid_wxGRID_TEXTCTRL, 2100).
-define(wxGrid_wxGRID_CHECKBOX, 2101).
-define(wxGrid_wxGRID_CHOICE, 2102).
-define(wxGrid_wxGRID_COMBOBOX, 2103).
% From class wxGrid::CursorMode
-define(wxGrid_WXGRID_CURSOR_SELECT_CELL, 0).
-define(wxGrid_WXGRID_CURSOR_RESIZE_ROW, 1).
-define(wxGrid_WXGRID_CURSOR_RESIZE_COL, 2).
-define(wxGrid_WXGRID_CURSOR_SELECT_ROW, 3).
-define(wxGrid_WXGRID_CURSOR_SELECT_COL, 4).
-define(wxGrid_WXGRID_CURSOR_MOVE_COL, 5).
% From class wxGrid::wxGridSelectionModes
-define(wxGrid_wxGridSelectCells, 0).
-define(wxGrid_wxGridSelectRows, 1).
-define(wxGrid_wxGridSelectColumns, 2).
% From class wxGridCellAttr::wxAttrKind
-define(wxGridCellAttr_Any, 0).
-define(wxGridCellAttr_Default, 1).
-define(wxGridCellAttr_Cell, 2).
-define(wxGridCellAttr_Row, 3).
-define(wxGridCellAttr_Col, 4).
-define(wxGridCellAttr_Merged, 5).
% From class wxGridCellAttr::wxAttrOverflowMode
-define(wxGridCellAttr_UnsetOverflow, -1).
-define(wxGridCellAttr_Overflow, 0).
-define(wxGridCellAttr_SingleCell, 1).
% From class wxGridCellAttr::wxAttrReadMode
-define(wxGridCellAttr_Unset, -1).
-define(wxGridCellAttr_ReadWrite, 0).
-define(wxGridCellAttr_ReadOnly, 1).
% From class wxHelpEvent::Origin
-define(wxHelpEvent_Origin_Unknown, 0).
-define(wxHelpEvent_Origin_Keyboard, 1).
-define(wxHelpEvent_Origin_HelpButton, 2).
% From class wxHtmlEasyPrinting::FontMode
-define(wxHtmlEasyPrinting_FontMode_Explicit, 0).
-define(wxHtmlEasyPrinting_FontMode_Standard, 1).
% From class wxHtmlWindow::ClipboardType
-define(wxHtmlWindow_Primary, 0).
-define(wxHtmlWindow_Secondary, 1).
% From class wxListbook
-define(wxListbook_SetSelection_SendEvent, 1).
% From class wxNavigationKeyEvent
-define(wxNavigationKeyEvent_IsBackward, 0).
-define(wxNavigationKeyEvent_IsForward, 1).
-define(wxNavigationKeyEvent_WinChange, 2).
-define(wxNavigationKeyEvent_FromTab, 4).
% From class wxNotebook
-define(wxNotebook_SetSelection_SendEvent, 1).
% From class wxProgressDialog
-define(wxProgressDialog_Uncancelable, -1).
-define(wxProgressDialog_Canceled, 0).
-define(wxProgressDialog_Continue, 1).
-define(wxProgressDialog_Finished, 2).
% From class wxSizerItem
-define(wxSizerItem_Item_None, 0).
-define(wxSizerItem_Item_Window, 1).
-define(wxSizerItem_Item_Sizer, 2).
-define(wxSizerItem_Item_Spacer, 3).
-define(wxSizerItem_Item_Max, 4).
% From class wxTextCtrl
-define(wxTextCtrl_SetValue_SendEvent, 1).
-define(wxTextCtrl_SetValue_SelectionOnly, 2).
% From class wxToolbook
-define(wxToolbook_SetSelection_SendEvent, 1).
% From class wxTreebook
-define(wxTreebook_SetSelection_SendEvent, 1).
% From class wxWindow::MoveKind
-define(wxWindow_MoveBefore, 0).
-define(wxWindow_MoveAfter, 1).
% From class wxWindowGTK::ScrollDir
-define(wxWindowGTK_ScrollDir_Horz, 0).
-define(wxWindowGTK_ScrollDir_Vert, 1).
-define(wxWindowGTK_ScrollDir_Max, 2).
% From class wxWindowGTK::ScrollUnit
-define(wxWindowGTK_ScrollUnit_Line, 0).
-define(wxWindowGTK_ScrollUnit_Page, 1).
-define(wxWindowGTK_ScrollUnit_Max, 2).
% From "accel.h"
-define(wxACCEL_NORMAL, 0).
-define(wxACCEL_ALT, 1).
-define(wxACCEL_CTRL, 2).
-define(wxACCEL_SHIFT, 4).
-define(wxACCEL_CMD, ?wxACCEL_CTRL).
% From "app.h"
-define(wxPRINT_WINDOWS, 1).
-define(wxPRINT_POSTSCRIPT, 2).
% From "auibook.h": wxAuiNotebookOption
-define(wxAUI_NB_TOP, 1).
-define(wxAUI_NB_LEFT, 2).
-define(wxAUI_NB_RIGHT, 4).
-define(wxAUI_NB_BOTTOM, 8).
-define(wxAUI_NB_TAB_SPLIT, 16).
-define(wxAUI_NB_TAB_MOVE, 32).
-define(wxAUI_NB_TAB_EXTERNAL_MOVE, 64).
-define(wxAUI_NB_TAB_FIXED_WIDTH, 128).
-define(wxAUI_NB_SCROLL_BUTTONS, 256).
-define(wxAUI_NB_WINDOWLIST_BUTTON, 512).
-define(wxAUI_NB_CLOSE_BUTTON, 1024).
-define(wxAUI_NB_CLOSE_ON_ACTIVE_TAB, 2048).
-define(wxAUI_NB_CLOSE_ON_ALL_TABS, 4096).
-define(wxAUI_NB_MIDDLE_CLICK_CLOSE, 8192).
-define(wxAUI_NB_DEFAULT_STYLE, (?wxAUI_NB_TOP bor ?wxAUI_NB_TAB_SPLIT bor ?wxAUI_NB_TAB_MOVE bor ?wxAUI_NB_SCROLL_BUTTONS bor ?wxAUI_NB_CLOSE_ON_ACTIVE_TAB bor ?wxAUI_NB_MIDDLE_CLICK_CLOSE)).
% From "bookctrl.h"
-define(wxBK_HITTEST_NOWHERE, 1).
-define(wxBK_HITTEST_ONICON, 2).
-define(wxBK_HITTEST_ONLABEL, 4).
-define(wxBK_HITTEST_ONITEM, (?wxBK_HITTEST_ONICON bor ?wxBK_HITTEST_ONLABEL)).
-define(wxBK_HITTEST_ONPAGE, 8).
% From "bookctrl.h"
-define(wxBK_ALIGN_MASK, (?wxBK_TOP bor ?wxBK_BOTTOM bor ?wxBK_LEFT bor ?wxBK_RIGHT)).
-define(wxBK_RIGHT, 128).
-define(wxBK_LEFT, 64).
-define(wxBK_BOTTOM, 32).
-define(wxBK_TOP, 16).
-define(wxBK_DEFAULT, 0).
% From "bugs.h": wxSashDragStatus
-define(wxSASH_STATUS_OK, 0).
-define(wxSASH_STATUS_OUT_OF_RANGE, 1).
% From "button.h"
-define(wxBU_EXACTFIT, 1).
-define(wxBU_AUTODRAW, 4).
-define(wxBU_NOAUTODRAW, 0).
-define(wxBU_ALIGN_MASK, (?wxBU_LEFT bor ?wxBU_TOP bor ?wxBU_RIGHT bor ?wxBU_BOTTOM)).
-define(wxBU_BOTTOM, 512).
-define(wxBU_RIGHT, 256).
-define(wxBU_TOP, 128).
-define(wxBU_LEFT, 64).
% From "calctrl.h"
-define(wxCAL_SUNDAY_FIRST, 0).
-define(wxCAL_MONDAY_FIRST, 1).
-define(wxCAL_SHOW_HOLIDAYS, 2).
-define(wxCAL_NO_YEAR_CHANGE, 4).
-define(wxCAL_NO_MONTH_CHANGE, 12).
-define(wxCAL_SEQUENTIAL_MONTH_SELECTION, 16).
-define(wxCAL_SHOW_SURROUNDING_WEEKS, 32).
% From "calctrl.h": wxCalendarDateBorder
-define(wxCAL_BORDER_NONE, 0).
-define(wxCAL_BORDER_SQUARE, 1).
-define(wxCAL_BORDER_ROUND, 2).
% From "calctrl.h": wxCalendarHitTestResult
-define(wxCAL_HITTEST_NOWHERE, 0).
-define(wxCAL_HITTEST_HEADER, 1).
-define(wxCAL_HITTEST_DAY, 2).
-define(wxCAL_HITTEST_INCMONTH, 3).
-define(wxCAL_HITTEST_DECMONTH, 4).
-define(wxCAL_HITTEST_SURROUNDING_WEEK, 5).
% From "checkbox.h"
-define(wxCHK_ALLOW_3RD_STATE_FOR_USER, 8192).
-define(wxCHK_3STATE, 4096).
-define(wxCHK_2STATE, 0).
% From "checkbox.h": wxCheckBoxState
-define(wxCHK_UNCHECKED, 0).
-define(wxCHK_CHECKED, 1).
-define(wxCHK_UNDETERMINED, 2).
% From "choicdgg.h"
-define(wxCHOICEDLG_STYLE, (?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER bor ?wxOK bor ?wxCANCEL bor ?wxCENTRE)).
-define(wxCHOICE_WIDTH, 200).
-define(wxCHOICE_HEIGHT, 150).
% From "choicebk.h"
-define(wxCHB_ALIGN_MASK, ?wxBK_ALIGN_MASK).
-define(wxCHB_RIGHT, ?wxBK_RIGHT).
-define(wxCHB_LEFT, ?wxBK_LEFT).
-define(wxCHB_BOTTOM, ?wxBK_BOTTOM).
-define(wxCHB_TOP, ?wxBK_TOP).
-define(wxCHB_DEFAULT, ?wxBK_DEFAULT).
% From "clntdata.h": wxClientDataType
-define(wxClientData_None, 0).
-define(wxClientData_Object, 1).
-define(wxClientData_Void, 2).
% From "clrpicker.h"
-define(wxCLRP_DEFAULT_STYLE, 0).
-define(wxCLRP_USE_TEXTCTRL, ?wxPB_USE_TEXTCTRL).
-define(wxCLRP_SHOW_LABEL, 8).
% From "cmndata.h"
% From "cmndata.h": wxPrintBin
-define(wxPRINTBIN_DEFAULT, 0).
-define(wxPRINTBIN_ONLYONE, 1).
-define(wxPRINTBIN_LOWER, 2).
-define(wxPRINTBIN_MIDDLE, 3).
-define(wxPRINTBIN_MANUAL, 4).
-define(wxPRINTBIN_ENVELOPE, 5).
-define(wxPRINTBIN_ENVMANUAL, 6).
-define(wxPRINTBIN_AUTO, 7).
-define(wxPRINTBIN_TRACTOR, 8).
-define(wxPRINTBIN_SMALLFMT, 9).
-define(wxPRINTBIN_LARGEFMT, 10).
-define(wxPRINTBIN_LARGECAPACITY, 11).
-define(wxPRINTBIN_CASSETTE, 12).
-define(wxPRINTBIN_FORMSOURCE, 13).
-define(wxPRINTBIN_USER, 14).
% From "colour.h"
-define(wxC2S_HTML_SYNTAX, 4).
-define(wxC2S_CSS_SYNTAX, 2).
-define(wxC2S_NAME, 1).
% From "confbase.h"
-define(wxCONFIG_CASE_SENSITIVE, 0).
% From "cpp.h"
-define(wxEMPTY_PARAMETER_VALUE, ()).
% From "datectrl.h"
-define(wxDP_DEFAULT, 0).
-define(wxDP_SPIN, 1).
-define(wxDP_DROPDOWN, 2).
-define(wxDP_SHOWCENTURY, 4).
-define(wxDP_ALLOWNONE, 8).
% From "datetime.h"
% From "dcbuffer.h"
-define(wxBUFFER_CLIENT_AREA, 2).
-define(wxBUFFER_VIRTUAL_AREA, 1).
-define(wxALWAYS_NATIVE_DOUBLE_BUFFER, wxe_util:get_const(wxALWAYS_NATIVE_DOUBLE_BUFFER)).
% From "defs.h"
-define(wxDefaultCoord, -1).
% From "defs.h"
-define(wxID_NONE, -3).
-define(wxID_SEPARATOR, -2).
-define(wxID_ANY, -1).
-define(wxID_LOWEST, 4999).
-define(wxID_OPEN, 5000).
-define(wxID_CLOSE, 5001).
-define(wxID_NEW, 5002).
-define(wxID_SAVE, 5003).
-define(wxID_SAVEAS, 5004).
-define(wxID_REVERT, 5005).
-define(wxID_EXIT, 5006).
-define(wxID_UNDO, 5007).
-define(wxID_REDO, 5008).
-define(wxID_HELP, 5009).
-define(wxID_PRINT, 5010).
-define(wxID_PRINT_SETUP, 5011).
-define(wxID_PAGE_SETUP, 5012).
-define(wxID_PREVIEW, 5013).
-define(wxID_ABOUT, 5014).
-define(wxID_HELP_CONTENTS, 5015).
-define(wxID_HELP_INDEX, 5016).
-define(wxID_HELP_SEARCH, 5017).
-define(wxID_HELP_COMMANDS, 5018).
-define(wxID_HELP_PROCEDURES, 5019).
-define(wxID_HELP_CONTEXT, 5020).
-define(wxID_CLOSE_ALL, 5021).
-define(wxID_PREFERENCES, 5022).
-define(wxID_EDIT, 5030).
-define(wxID_CUT, 5031).
-define(wxID_COPY, 5032).
-define(wxID_PASTE, 5033).
-define(wxID_CLEAR, 5034).
-define(wxID_FIND, 5035).
-define(wxID_DUPLICATE, 5036).
-define(wxID_SELECTALL, 5037).
-define(wxID_DELETE, 5038).
-define(wxID_REPLACE, 5039).
-define(wxID_REPLACE_ALL, 5040).
-define(wxID_PROPERTIES, 5041).
-define(wxID_VIEW_DETAILS, 5042).
-define(wxID_VIEW_LARGEICONS, 5043).
-define(wxID_VIEW_SMALLICONS, 5044).
-define(wxID_VIEW_LIST, 5045).
-define(wxID_VIEW_SORTDATE, 5046).
-define(wxID_VIEW_SORTNAME, 5047).
-define(wxID_VIEW_SORTSIZE, 5048).
-define(wxID_VIEW_SORTTYPE, 5049).
-define(wxID_FILE, 5050).
-define(wxID_FILE1, 5051).
-define(wxID_FILE2, 5052).
-define(wxID_FILE3, 5053).
-define(wxID_FILE4, 5054).
-define(wxID_FILE5, 5055).
-define(wxID_FILE6, 5056).
-define(wxID_FILE7, 5057).
-define(wxID_FILE8, 5058).
-define(wxID_FILE9, 5059).
-define(wxID_OK, 5100).
-define(wxID_CANCEL, 5101).
-define(wxID_APPLY, 5102).
-define(wxID_YES, 5103).
-define(wxID_NO, 5104).
-define(wxID_STATIC, 5105).
-define(wxID_FORWARD, 5106).
-define(wxID_BACKWARD, 5107).
-define(wxID_DEFAULT, 5108).
-define(wxID_MORE, 5109).
-define(wxID_SETUP, 5110).
-define(wxID_RESET, 5111).
-define(wxID_CONTEXT_HELP, 5112).
-define(wxID_YESTOALL, 5113).
-define(wxID_NOTOALL, 5114).
-define(wxID_ABORT, 5115).
-define(wxID_RETRY, 5116).
-define(wxID_IGNORE, 5117).
-define(wxID_ADD, 5118).
-define(wxID_REMOVE, 5119).
-define(wxID_UP, 5120).
-define(wxID_DOWN, 5121).
-define(wxID_HOME, 5122).
-define(wxID_REFRESH, 5123).
-define(wxID_STOP, 5124).
-define(wxID_INDEX, 5125).
-define(wxID_BOLD, 5126).
-define(wxID_ITALIC, 5127).
-define(wxID_JUSTIFY_CENTER, 5128).
-define(wxID_JUSTIFY_FILL, 5129).
-define(wxID_JUSTIFY_RIGHT, 5130).
-define(wxID_JUSTIFY_LEFT, 5131).
-define(wxID_UNDERLINE, 5132).
-define(wxID_INDENT, 5133).
-define(wxID_UNINDENT, 5134).
-define(wxID_ZOOM_100, 5135).
-define(wxID_ZOOM_FIT, 5136).
-define(wxID_ZOOM_IN, 5137).
-define(wxID_ZOOM_OUT, 5138).
-define(wxID_UNDELETE, 5139).
-define(wxID_REVERT_TO_SAVED, 5140).
-define(wxID_SYSTEM_MENU, 5200).
-define(wxID_CLOSE_FRAME, 5201).
-define(wxID_MOVE_FRAME, 5202).
-define(wxID_RESIZE_FRAME, 5203).
-define(wxID_MAXIMIZE_FRAME, 5204).
-define(wxID_ICONIZE_FRAME, 5205).
-define(wxID_RESTORE_FRAME, 5206).
-define(wxID_FILEDLGG, 5900).
-define(wxID_HIGHEST, 5999).
% From "defs.h"
-define(wxDEFAULT, 70).
-define(wxDECORATIVE, 71).
-define(wxROMAN, 72).
-define(wxSCRIPT, 73).
-define(wxSWISS, 74).
-define(wxMODERN, 75).
-define(wxTELETYPE, 76).
-define(wxVARIABLE, 80).
-define(wxFIXED, 81).
-define(wxNORMAL, 90).
-define(wxLIGHT, 91).
-define(wxBOLD, 92).
-define(wxITALIC, 93).
-define(wxSLANT, 94).
-define(wxSOLID, 100).
-define(wxDOT, 101).
-define(wxLONG_DASH, 102).
-define(wxSHORT_DASH, 103).
-define(wxDOT_DASH, 104).
-define(wxUSER_DASH, 105).
-define(wxTRANSPARENT, 106).
-define(wxSTIPPLE_MASK_OPAQUE, 107).
-define(wxSTIPPLE_MASK, 108).
-define(wxSTIPPLE, 110).
-define(wxBDIAGONAL_HATCH, 111).
-define(wxCROSSDIAG_HATCH, 112).
-define(wxFDIAGONAL_HATCH, 113).
-define(wxCROSS_HATCH, 114).
-define(wxHORIZONTAL_HATCH, 115).
-define(wxVERTICAL_HATCH, 116).
-define(wxFIRST_HATCH, ?wxBDIAGONAL_HATCH).
-define(wxLAST_HATCH, ?wxVERTICAL_HATCH).
-define(wxJOIN_BEVEL, 120).
-define(wxJOIN_MITER, 121).
-define(wxJOIN_ROUND, 122).
-define(wxCAP_ROUND, 130).
-define(wxCAP_PROJECTING, 131).
-define(wxCAP_BUTT, 132).
% From "defs.h"
-define(wxFLOOD_SURFACE, 1).
-define(wxFLOOD_BORDER, 2).
% From "defs.h"
-define(wxODDEVEN_RULE, 1).
-define(wxWINDING_RULE, 2).
% From "defs.h"
-define(wxTOOL_TOP, 1).
-define(wxTOOL_BOTTOM, 2).
-define(wxTOOL_LEFT, 3).
-define(wxTOOL_RIGHT, 4).
% From "defs.h"
-define(wxMM_TEXT, 1).
-define(wxMM_LOMETRIC, 2).
-define(wxMM_HIMETRIC, 3).
-define(wxMM_LOENGLISH, 4).
-define(wxMM_HIENGLISH, 5).
-define(wxMM_TWIPS, 6).
-define(wxMM_ISOTROPIC, 7).
-define(wxMM_ANISOTROPIC, 8).
-define(wxMM_POINTS, 9).
-define(wxMM_METRIC, 10).
% From "defs.h"
-define(wxPRINT_QUALITY_DRAFT, -4).
-define(wxPRINT_QUALITY_LOW, -3).
-define(wxPRINT_QUALITY_MEDIUM, -2).
-define(wxPRINT_QUALITY_HIGH, -1).
-define(wxLANDSCAPE, 2).
-define(wxPORTRAIT, 1).
-define(wxSIZE_FORCE, 16).
-define(wxSIZE_NO_ADJUSTMENTS, 8).
-define(wxSIZE_ALLOW_MINUS_ONE, 4).
-define(wxSIZE_USE_EXISTING, 0).
-define(wxSIZE_AUTO, (?wxSIZE_AUTO_WIDTH bor ?wxSIZE_AUTO_HEIGHT)).
-define(wxSIZE_AUTO_HEIGHT, 2).
-define(wxSIZE_AUTO_WIDTH, 1).
-define(wxSETUP, 131072).
-define(wxMORE, 65536).
-define(wxHELP, 32768).
-define(wxRESET, 16384).
-define(wxBACKWARD, 8192).
-define(wxFORWARD, 4096).
-define(wxICON_MASK, (16#00000100 bor 16#00000200 bor 16#00000400 bor 16#00000800)).
-define(wxICON_ASTERISK, ?wxICON_INFORMATION).
-define(wxICON_STOP, ?wxICON_HAND).
-define(wxICON_INFORMATION, 2048).
-define(wxICON_QUESTION, 1024).
-define(wxICON_ERROR, ?wxICON_HAND).
-define(wxICON_WARNING, ?wxICON_EXCLAMATION).
-define(wxICON_HAND, 512).
-define(wxICON_EXCLAMATION, 256).
-define(wxNO_DEFAULT, 128).
-define(wxYES_DEFAULT, 0).
-define(wxCANCEL, 16).
-define(wxYES_NO, (?wxYES bor ?wxNO)).
-define(wxNO, 8).
-define(wxOK, 4).
-define(wxYES, 2).
-define(wxLI_VERTICAL, ?wxVERTICAL).
-define(wxLI_HORIZONTAL, ?wxHORIZONTAL).
-define(wxBI_EXPAND, ?wxEXPAND).
-define(wxST_DOTS_END, 4).
-define(wxST_DOTS_MIDDLE, 2).
-define(wxST_NO_AUTORESIZE, 1).
-define(wxST_SIZEGRIP, 16).
-define(wxTC_OWNERDRAW, 1024).
-define(wxTC_MULTILINE, 512).
-define(wxTC_BOTTOM, 128).
-define(wxTC_RIGHT, 64).
-define(wxTC_LEFT, 32).
-define(wxTC_TOP, 0).
-define(wxTC_FIXEDWIDTH, 32).
-define(wxTC_RIGHTJUSTIFY, 16).
-define(wxSP_WRAP, 8192).
-define(wxSP_ARROW_KEYS, 4096).
-define(wxSP_VERTICAL, ?wxVERTICAL).
-define(wxSP_HORIZONTAL, ?wxHORIZONTAL).
-define(wxSB_VERTICAL, ?wxVERTICAL).
-define(wxSB_HORIZONTAL, ?wxHORIZONTAL).
-define(wxRB_USE_CHECKBOX, 16).
-define(wxRB_SINGLE, 8).
-define(wxRB_GROUP, 4).
-define(wxRA_USE_CHECKBOX, 16).
-define(wxRA_VERTICAL, ?wxVERTICAL).
-define(wxRA_HORIZONTAL, ?wxHORIZONTAL).
-define(wxRA_SPECIFY_ROWS, ?wxVERTICAL).
-define(wxRA_SPECIFY_COLS, ?wxHORIZONTAL).
-define(wxRA_TOPTOBOTTOM, 2).
-define(wxRA_LEFTTORIGHT, 1).
-define(wxCB_DROPDOWN, 32).
-define(wxCB_READONLY, 16).
-define(wxCB_SORT, 8).
-define(wxCB_SIMPLE, 4).
-define(wxLB_INT_HEIGHT, 2048).
-define(wxLB_HSCROLL, ?wxHSCROLL).
-define(wxLB_ALWAYS_SB, 1024).
-define(wxLB_NEEDED_SB, 512).
-define(wxLB_OWNERDRAW, 256).
-define(wxLB_EXTENDED, 128).
-define(wxLB_MULTIPLE, 64).
-define(wxLB_SINGLE, 32).
-define(wxLB_SORT, 16).
-define(wxFIXED_LENGTH, 1024).
-define(wxCOLOURED, 2048).
-define(wxMENU_TEAROFF, 1).
-define(wxMB_DOCKABLE, 1).
-define(wxFRAME_NO_WINDOW_MENU, 256).
-define(wxFRAME_DRAWER, 32).
-define(wxDIALOG_EX_CONTEXTHELP, ?wxWS_EX_CONTEXTHELP).
-define(wxFRAME_EX_CONTEXTHELP, ?wxWS_EX_CONTEXTHELP).
-define(wxWS_EX_CONTEXTHELP, 128).
-define(wxDIALOG_EX_METAL, 64).
-define(wxFRAME_EX_METAL, 64).
-define(wxWS_EX_PROCESS_UI_UPDATES, 32).
-define(wxWS_EX_PROCESS_IDLE, 16).
-define(wxWS_EX_THEMED_BACKGROUND, 8).
-define(wxWS_EX_TRANSIENT, 4).
-define(wxWS_EX_BLOCK_EVENTS, 2).
-define(wxWS_EX_VALIDATE_RECURSIVELY, 1).
-define(wxWINDOW_STYLE_MASK, (?wxVSCROLL bor ?wxHSCROLL bor ?wxBORDER_MASK bor ?wxALWAYS_SHOW_SB bor ?wxCLIP_CHILDREN bor ?wxCLIP_SIBLINGS bor ?wxTRANSPARENT_WINDOW bor ?wxTAB_TRAVERSAL bor ?wxWANTS_CHARS bor ?wxRETAINED bor ?wxPOPUP_WINDOW bor ?wxFULL_REPAINT_ON_RESIZE)).
-define(wxNO_FULL_REPAINT_ON_RESIZE, 0).
-define(wxFULL_REPAINT_ON_RESIZE, 65536).
-define(wxPOPUP_WINDOW, 131072).
-define(wxBACKINGSTORE, ?wxRETAINED).
-define(wxRETAINED, wxe_util:get_const(wxRETAINED)).
-define(wxWANTS_CHARS, 262144).
-define(wxTAB_TRAVERSAL, 524288).
-define(wxTRANSPARENT_WINDOW, 1048576).
-define(wxCLIP_SIBLINGS, 536870912).
-define(wxCLIP_CHILDREN, 4194304).
-define(wxALWAYS_SHOW_SB, 8388608).
-define(wxNO_BORDER, ?wxBORDER_NONE).
-define(wxSTATIC_BORDER, ?wxBORDER_STATIC).
-define(wxSIMPLE_BORDER, ?wxBORDER_SIMPLE).
-define(wxBORDER, ?wxBORDER_SIMPLE).
-define(wxRAISED_BORDER, ?wxBORDER_RAISED).
-define(wxSUNKEN_BORDER, ?wxBORDER_SUNKEN).
-define(wxDOUBLE_BORDER, ?wxBORDER_DOUBLE).
-define(wxCAPTION, 536870912).
-define(wxHSCROLL, 1073741824).
-define(wxVSCROLL, 2147483648).
-define(wxDEFAULT_CONTROL_BORDER, wxe_util:get_const(wxDEFAULT_CONTROL_BORDER)).
-define(wxCENTER_ON_SCREEN, ?wxCENTRE_ON_SCREEN).
-define(wxCENTRE_ON_SCREEN, 2).
-define(wxCENTER_FRAME, 0).
-define(wxBYTE_ORDER, wxe_util:get_const(wxBYTE_ORDER)).
-define(wxPDP_ENDIAN, 3412).
-define(wxLITTLE_ENDIAN, 1234).
-define(wxBIG_ENDIAN, 4321).
-define(wxHAS_INT64, wxe_util:get_const(wxHAS_INT64)).
-define(wxNOT_FOUND, -1).
% From "defs.h": form_ops_t
-define(wxCLEAR, 0).
-define(wxROP_BLACK, ?wxCLEAR).
-define(wxBLIT_BLACKNESS, ?wxCLEAR).
-define(wxXOR, (?wxCLEAR+1)).
-define(wxROP_XORPEN, ?wxXOR).
-define(wxBLIT_SRCINVERT, ?wxXOR).
-define(wxINVERT, (?wxXOR+1)).
-define(wxROP_NOT, ?wxINVERT).
-define(wxBLIT_DSTINVERT, ?wxINVERT).
-define(wxOR_REVERSE, (?wxINVERT+1)).
-define(wxROP_MERGEPENNOT, ?wxOR_REVERSE).
-define(wxBLIT_00DD0228, ?wxOR_REVERSE).
-define(wxAND_REVERSE, (?wxOR_REVERSE+1)).
-define(wxROP_MASKPENNOT, ?wxAND_REVERSE).
-define(wxBLIT_SRCERASE, ?wxAND_REVERSE).
-define(wxCOPY, (?wxAND_REVERSE+1)).
-define(wxROP_COPYPEN, ?wxCOPY).
-define(wxBLIT_SRCCOPY, ?wxCOPY).
-define(wxAND, (?wxCOPY+1)).
-define(wxROP_MASKPEN, ?wxAND).
-define(wxBLIT_SRCAND, ?wxAND).
-define(wxAND_INVERT, (?wxAND+1)).
-define(wxROP_MASKNOTPEN, ?wxAND_INVERT).
-define(wxBLIT_00220326, ?wxAND_INVERT).
-define(wxNO_OP, (?wxAND_INVERT+1)).
-define(wxROP_NOP, ?wxNO_OP).
-define(wxBLIT_00AA0029, ?wxNO_OP).
-define(wxNOR, (?wxNO_OP+1)).
-define(wxROP_NOTMERGEPEN, ?wxNOR).
-define(wxBLIT_NOTSRCERASE, ?wxNOR).
-define(wxEQUIV, (?wxNOR+1)).
-define(wxROP_NOTXORPEN, ?wxEQUIV).
-define(wxBLIT_00990066, ?wxEQUIV).
-define(wxSRC_INVERT, (?wxEQUIV+1)).
-define(wxROP_NOTCOPYPEN, ?wxSRC_INVERT).
-define(wxBLIT_NOTSCRCOPY, ?wxSRC_INVERT).
-define(wxOR_INVERT, (?wxSRC_INVERT+1)).
-define(wxROP_MERGENOTPEN, ?wxOR_INVERT).
-define(wxBLIT_MERGEPAINT, ?wxOR_INVERT).
-define(wxNAND, (?wxOR_INVERT+1)).
-define(wxROP_NOTMASKPEN, ?wxNAND).
-define(wxBLIT_007700E6, ?wxNAND).
-define(wxOR, (?wxNAND+1)).
-define(wxROP_MERGEPEN, ?wxOR).
-define(wxBLIT_SRCPAINT, ?wxOR).
-define(wxSET, (?wxOR+1)).
-define(wxROP_WHITE, ?wxSET).
-define(wxBLIT_WHITENESS, ?wxSET).
% From "defs.h": wxAlignment
-define(wxALIGN_NOT, 0).
-define(wxALIGN_CENTER_HORIZONTAL, 256).
-define(wxALIGN_CENTRE_HORIZONTAL, ?wxALIGN_CENTER_HORIZONTAL).
-define(wxALIGN_LEFT, ?wxALIGN_NOT).
-define(wxALIGN_TOP, ?wxALIGN_NOT).
-define(wxALIGN_RIGHT, 512).
-define(wxALIGN_BOTTOM, 1024).
-define(wxALIGN_CENTER_VERTICAL, 2048).
-define(wxALIGN_CENTRE_VERTICAL, ?wxALIGN_CENTER_VERTICAL).
-define(wxALIGN_CENTER, (?wxALIGN_CENTER_HORIZONTAL bor ?wxALIGN_CENTER_VERTICAL)).
-define(wxALIGN_CENTRE, ?wxALIGN_CENTER).
-define(wxALIGN_MASK, 3840).
% From "defs.h": wxBackgroundStyle
-define(wxBG_STYLE_SYSTEM, 0).
-define(wxBG_STYLE_COLOUR, 1).
-define(wxBG_STYLE_CUSTOM, 2).
% From "defs.h": wxBorder
-define(wxBORDER_DEFAULT, 0).
-define(wxBORDER_NONE, 2097152).
-define(wxBORDER_STATIC, 16777216).
-define(wxBORDER_SIMPLE, 33554432).
-define(wxBORDER_RAISED, 67108864).
-define(wxBORDER_SUNKEN, 134217728).
-define(wxBORDER_DOUBLE, 268435456).
-define(wxBORDER_THEME, 268435456).
-define(wxBORDER_MASK, 522190848).
% From "defs.h": wxDataFormatId
-define(wxDF_INVALID, 0).
-define(wxDF_TEXT, 1).
-define(wxDF_BITMAP, 2).
-define(wxDF_METAFILE, 3).
-define(wxDF_SYLK, 4).
-define(wxDF_DIF, 5).
-define(wxDF_TIFF, 6).
-define(wxDF_OEMTEXT, 7).
-define(wxDF_DIB, 8).
-define(wxDF_PALETTE, 9).
-define(wxDF_PENDATA, 10).
-define(wxDF_RIFF, 11).
-define(wxDF_WAVE, 12).
-define(wxDF_UNICODETEXT, 13).
-define(wxDF_ENHMETAFILE, 14).
-define(wxDF_FILENAME, 15).
-define(wxDF_LOCALE, 16).
-define(wxDF_PRIVATE, 20).
-define(wxDF_HTML, 30).
-define(wxDF_MAX, 31).
% From "defs.h": wxDirection
-define(wxLEFT, 16).
-define(wxRIGHT, 32).
-define(wxUP, 64).
-define(wxDOWN, 128).
-define(wxTOP, ?wxUP).
-define(wxBOTTOM, ?wxDOWN).
-define(wxNORTH, ?wxUP).
-define(wxSOUTH, ?wxDOWN).
-define(wxWEST, ?wxLEFT).
-define(wxEAST, ?wxRIGHT).
-define(wxALL, (?wxUP bor ?wxDOWN bor ?wxRIGHT bor ?wxLEFT)).
% From "defs.h": wxDuplexMode
-define(wxDUPLEX_SIMPLEX, 0).
-define(wxDUPLEX_HORIZONTAL, 1).
-define(wxDUPLEX_VERTICAL, 2).
% From "defs.h": wxGeometryCentre
-define(wxCENTRE, 1).
-define(wxCENTER, ?wxCENTRE).
% From "defs.h": wxHitTest
-define(wxHT_NOWHERE, 0).
-define(wxHT_SCROLLBAR_FIRST, ?wxHT_NOWHERE).
-define(wxHT_SCROLLBAR_ARROW_LINE_1, (?wxHT_NOWHERE+1)).
-define(wxHT_SCROLLBAR_ARROW_LINE_2, (?wxHT_NOWHERE+2)).
-define(wxHT_SCROLLBAR_ARROW_PAGE_1, (?wxHT_NOWHERE+3)).
-define(wxHT_SCROLLBAR_ARROW_PAGE_2, (?wxHT_NOWHERE+4)).
-define(wxHT_SCROLLBAR_THUMB, (?wxHT_NOWHERE+5)).
-define(wxHT_SCROLLBAR_BAR_1, (?wxHT_NOWHERE+6)).
-define(wxHT_SCROLLBAR_BAR_2, (?wxHT_NOWHERE+7)).
-define(wxHT_SCROLLBAR_LAST, (?wxHT_NOWHERE+8)).
-define(wxHT_WINDOW_OUTSIDE, (?wxHT_NOWHERE+9)).
-define(wxHT_WINDOW_INSIDE, (?wxHT_NOWHERE+10)).
-define(wxHT_WINDOW_VERT_SCROLLBAR, (?wxHT_NOWHERE+11)).
-define(wxHT_WINDOW_HORZ_SCROLLBAR, (?wxHT_NOWHERE+12)).
-define(wxHT_WINDOW_CORNER, (?wxHT_NOWHERE+13)).
-define(wxHT_MAX, (?wxHT_NOWHERE+14)).
% From "defs.h": wxItemKind
-define(wxITEM_SEPARATOR, -1).
-define(wxITEM_NORMAL, 0).
-define(wxITEM_CHECK, 1).
-define(wxITEM_RADIO, 2).
-define(wxITEM_MAX, 3).
% From "defs.h": wxKeyCode
-define(WXK_BACK, 8).
-define(WXK_TAB, 9).
-define(WXK_RETURN, 13).
-define(WXK_ESCAPE, 27).
-define(WXK_SPACE, 32).
-define(WXK_DELETE, 127).
-define(WXK_START, 300).
-define(WXK_LBUTTON, 301).
-define(WXK_RBUTTON, 302).
-define(WXK_CANCEL, 303).
-define(WXK_MBUTTON, 304).
-define(WXK_CLEAR, 305).
-define(WXK_SHIFT, 306).
-define(WXK_ALT, 307).
-define(WXK_CONTROL, 308).
-define(WXK_MENU, 309).
-define(WXK_PAUSE, 310).
-define(WXK_CAPITAL, 311).
-define(WXK_END, 312).
-define(WXK_HOME, 313).
-define(WXK_LEFT, 314).
-define(WXK_UP, 315).
-define(WXK_RIGHT, 316).
-define(WXK_DOWN, 317).
-define(WXK_SELECT, 318).
-define(WXK_PRINT, 319).
-define(WXK_EXECUTE, 320).
-define(WXK_SNAPSHOT, 321).
-define(WXK_INSERT, 322).
-define(WXK_HELP, 323).
-define(WXK_NUMPAD0, 324).
-define(WXK_NUMPAD1, 325).
-define(WXK_NUMPAD2, 326).
-define(WXK_NUMPAD3, 327).
-define(WXK_NUMPAD4, 328).
-define(WXK_NUMPAD5, 329).
-define(WXK_NUMPAD6, 330).
-define(WXK_NUMPAD7, 331).
-define(WXK_NUMPAD8, 332).
-define(WXK_NUMPAD9, 333).
-define(WXK_MULTIPLY, 334).
-define(WXK_ADD, 335).
-define(WXK_SEPARATOR, 336).
-define(WXK_SUBTRACT, 337).
-define(WXK_DECIMAL, 338).
-define(WXK_DIVIDE, 339).
-define(WXK_F1, 340).
-define(WXK_F2, 341).
-define(WXK_F3, 342).
-define(WXK_F4, 343).
-define(WXK_F5, 344).
-define(WXK_F6, 345).
-define(WXK_F7, 346).
-define(WXK_F8, 347).
-define(WXK_F9, 348).
-define(WXK_F10, 349).
-define(WXK_F11, 350).
-define(WXK_F12, 351).
-define(WXK_F13, 352).
-define(WXK_F14, 353).
-define(WXK_F15, 354).
-define(WXK_F16, 355).
-define(WXK_F17, 356).
-define(WXK_F18, 357).
-define(WXK_F19, 358).
-define(WXK_F20, 359).
-define(WXK_F21, 360).
-define(WXK_F22, 361).
-define(WXK_F23, 362).
-define(WXK_F24, 363).
-define(WXK_NUMLOCK, 364).
-define(WXK_SCROLL, 365).
-define(WXK_PAGEUP, 366).
-define(WXK_PAGEDOWN, 367).
-define(WXK_NUMPAD_SPACE, 368).
-define(WXK_NUMPAD_TAB, 369).
-define(WXK_NUMPAD_ENTER, 370).
-define(WXK_NUMPAD_F1, 371).
-define(WXK_NUMPAD_F2, 372).
-define(WXK_NUMPAD_F3, 373).
-define(WXK_NUMPAD_F4, 374).
-define(WXK_NUMPAD_HOME, 375).
-define(WXK_NUMPAD_LEFT, 376).
-define(WXK_NUMPAD_UP, 377).
-define(WXK_NUMPAD_RIGHT, 378).
-define(WXK_NUMPAD_DOWN, 379).
-define(WXK_NUMPAD_PAGEUP, 380).
-define(WXK_NUMPAD_PAGEDOWN, 381).
-define(WXK_NUMPAD_END, 382).
-define(WXK_NUMPAD_BEGIN, 383).
-define(WXK_NUMPAD_INSERT, 384).
-define(WXK_NUMPAD_DELETE, 385).
-define(WXK_NUMPAD_EQUAL, 386).
-define(WXK_NUMPAD_MULTIPLY, 387).
-define(WXK_NUMPAD_ADD, 388).
-define(WXK_NUMPAD_SEPARATOR, 389).
-define(WXK_NUMPAD_SUBTRACT, 390).
-define(WXK_NUMPAD_DECIMAL, 391).
-define(WXK_NUMPAD_DIVIDE, 392).
-define(WXK_WINDOWS_LEFT, 393).
-define(WXK_WINDOWS_RIGHT, 394).
-define(WXK_WINDOWS_MENU, 395).
-define(WXK_COMMAND, 396).
-define(WXK_SPECIAL1, 193).
-define(WXK_SPECIAL2, 194).
-define(WXK_SPECIAL3, 195).
-define(WXK_SPECIAL4, 196).
-define(WXK_SPECIAL5, 197).
-define(WXK_SPECIAL6, 198).
-define(WXK_SPECIAL7, 199).
-define(WXK_SPECIAL8, 200).
-define(WXK_SPECIAL9, 201).
-define(WXK_SPECIAL10, 202).
-define(WXK_SPECIAL11, 203).
-define(WXK_SPECIAL12, 204).
-define(WXK_SPECIAL13, 205).
-define(WXK_SPECIAL14, 206).
-define(WXK_SPECIAL15, 207).
-define(WXK_SPECIAL16, 208).
-define(WXK_SPECIAL17, 209).
-define(WXK_SPECIAL18, 210).
-define(WXK_SPECIAL19, 211).
-define(WXK_SPECIAL20, 212).
% From "defs.h": wxKeyModifier
-define(wxMOD_NONE, 0).
-define(wxMOD_ALT, 1).
-define(wxMOD_CONTROL, 2).
-define(wxMOD_ALTGR, (?wxMOD_ALT bor ?wxMOD_CONTROL)).
-define(wxMOD_SHIFT, 4).
-define(wxMOD_META, 8).
-define(wxMOD_WIN, ?wxMOD_META).
-define(wxMOD_CMD, wxe_util:get_const(wxMOD_CMD)).
-define(wxMOD_ALL, 65535).
% From "defs.h": wxNotificationOptions
-define(wxNOTIFY_NONE, 0).
-define(wxNOTIFY_ONCE, 1).
-define(wxNOTIFY_REPEAT, 2).
% From "defs.h": wxOrientation
-define(wxHORIZONTAL, 4).
-define(wxVERTICAL, 8).
-define(wxBOTH, (?wxVERTICAL bor ?wxHORIZONTAL)).
% From "defs.h": wxPaperSize
-define(wxPAPER_NONE, 0).
-define(wxPAPER_LETTER, 1).
-define(wxPAPER_LEGAL, 2).
-define(wxPAPER_A4, 3).
-define(wxPAPER_CSHEET, 4).
-define(wxPAPER_DSHEET, 5).
-define(wxPAPER_ESHEET, 6).
-define(wxPAPER_LETTERSMALL, 7).
-define(wxPAPER_TABLOID, 8).
-define(wxPAPER_LEDGER, 9).
-define(wxPAPER_STATEMENT, 10).
-define(wxPAPER_EXECUTIVE, 11).
-define(wxPAPER_A3, 12).
-define(wxPAPER_A4SMALL, 13).
-define(wxPAPER_A5, 14).
-define(wxPAPER_B4, 15).
-define(wxPAPER_B5, 16).
-define(wxPAPER_FOLIO, 17).
-define(wxPAPER_QUARTO, 18).
-define(wxPAPER_10X14, 19).
-define(wxPAPER_11X17, 20).
-define(wxPAPER_NOTE, 21).
-define(wxPAPER_ENV_9, 22).
-define(wxPAPER_ENV_10, 23).
-define(wxPAPER_ENV_11, 24).
-define(wxPAPER_ENV_12, 25).
-define(wxPAPER_ENV_14, 26).
-define(wxPAPER_ENV_DL, 27).
-define(wxPAPER_ENV_C5, 28).
-define(wxPAPER_ENV_C3, 29).
-define(wxPAPER_ENV_C4, 30).
-define(wxPAPER_ENV_C6, 31).
-define(wxPAPER_ENV_C65, 32).
-define(wxPAPER_ENV_B4, 33).
-define(wxPAPER_ENV_B5, 34).
-define(wxPAPER_ENV_B6, 35).
-define(wxPAPER_ENV_ITALY, 36).
-define(wxPAPER_ENV_MONARCH, 37).
-define(wxPAPER_ENV_PERSONAL, 38).
-define(wxPAPER_FANFOLD_US, 39).
-define(wxPAPER_FANFOLD_STD_GERMAN, 40).
-define(wxPAPER_FANFOLD_LGL_GERMAN, 41).
-define(wxPAPER_ISO_B4, 42).
-define(wxPAPER_JAPANESE_POSTCARD, 43).
-define(wxPAPER_9X11, 44).
-define(wxPAPER_10X11, 45).
-define(wxPAPER_15X11, 46).
-define(wxPAPER_ENV_INVITE, 47).
-define(wxPAPER_LETTER_EXTRA, 48).
-define(wxPAPER_LEGAL_EXTRA, 49).
-define(wxPAPER_TABLOID_EXTRA, 50).
-define(wxPAPER_A4_EXTRA, 51).
-define(wxPAPER_LETTER_TRANSVERSE, 52).
-define(wxPAPER_A4_TRANSVERSE, 53).
-define(wxPAPER_LETTER_EXTRA_TRANSVERSE, 54).
-define(wxPAPER_A_PLUS, 55).
-define(wxPAPER_B_PLUS, 56).
-define(wxPAPER_LETTER_PLUS, 57).
-define(wxPAPER_A4_PLUS, 58).
-define(wxPAPER_A5_TRANSVERSE, 59).
-define(wxPAPER_B5_TRANSVERSE, 60).
-define(wxPAPER_A3_EXTRA, 61).
-define(wxPAPER_A5_EXTRA, 62).
-define(wxPAPER_B5_EXTRA, 63).
-define(wxPAPER_A2, 64).
-define(wxPAPER_A3_TRANSVERSE, 65).
-define(wxPAPER_A3_EXTRA_TRANSVERSE, 66).
-define(wxPAPER_DBL_JAPANESE_POSTCARD, 67).
-define(wxPAPER_A6, 68).
-define(wxPAPER_JENV_KAKU2, 69).
-define(wxPAPER_JENV_KAKU3, 70).
-define(wxPAPER_JENV_CHOU3, 71).
-define(wxPAPER_JENV_CHOU4, 72).
-define(wxPAPER_LETTER_ROTATED, 73).
-define(wxPAPER_A3_ROTATED, 74).
-define(wxPAPER_A4_ROTATED, 75).
-define(wxPAPER_A5_ROTATED, 76).
-define(wxPAPER_B4_JIS_ROTATED, 77).
-define(wxPAPER_B5_JIS_ROTATED, 78).
-define(wxPAPER_JAPANESE_POSTCARD_ROTATED, 79).
-define(wxPAPER_DBL_JAPANESE_POSTCARD_ROTATED, 80).
-define(wxPAPER_A6_ROTATED, 81).
-define(wxPAPER_JENV_KAKU2_ROTATED, 82).
-define(wxPAPER_JENV_KAKU3_ROTATED, 83).
-define(wxPAPER_JENV_CHOU3_ROTATED, 84).
-define(wxPAPER_JENV_CHOU4_ROTATED, 85).
-define(wxPAPER_B6_JIS, 86).
-define(wxPAPER_B6_JIS_ROTATED, 87).
-define(wxPAPER_12X11, 88).
-define(wxPAPER_JENV_YOU4, 89).
-define(wxPAPER_JENV_YOU4_ROTATED, 90).
-define(wxPAPER_P16K, 91).
-define(wxPAPER_P32K, 92).
-define(wxPAPER_P32KBIG, 93).
-define(wxPAPER_PENV_1, 94).
-define(wxPAPER_PENV_2, 95).
-define(wxPAPER_PENV_3, 96).
-define(wxPAPER_PENV_4, 97).
-define(wxPAPER_PENV_5, 98).
-define(wxPAPER_PENV_6, 99).
-define(wxPAPER_PENV_7, 100).
-define(wxPAPER_PENV_8, 101).
-define(wxPAPER_PENV_9, 102).
-define(wxPAPER_PENV_10, 103).
-define(wxPAPER_P16K_ROTATED, 104).
-define(wxPAPER_P32K_ROTATED, 105).
-define(wxPAPER_P32KBIG_ROTATED, 106).
-define(wxPAPER_PENV_1_ROTATED, 107).
-define(wxPAPER_PENV_2_ROTATED, 108).
-define(wxPAPER_PENV_3_ROTATED, 109).
-define(wxPAPER_PENV_4_ROTATED, 110).
-define(wxPAPER_PENV_5_ROTATED, 111).
-define(wxPAPER_PENV_6_ROTATED, 112).
-define(wxPAPER_PENV_7_ROTATED, 113).
-define(wxPAPER_PENV_8_ROTATED, 114).
-define(wxPAPER_PENV_9_ROTATED, 115).
-define(wxPAPER_PENV_10_ROTATED, 116).
% From "defs.h": wxPrintMode
-define(wxPRINT_MODE_NONE, 0).
-define(wxPRINT_MODE_PREVIEW, 1).
-define(wxPRINT_MODE_FILE, 2).
-define(wxPRINT_MODE_PRINTER, 3).
-define(wxPRINT_MODE_STREAM, 4).
% From "defs.h": wxStretch
-define(wxSTRETCH_NOT, 0).
-define(wxSHRINK, 4096).
-define(wxGROW, 8192).
-define(wxEXPAND, ?wxGROW).
-define(wxSHAPED, 16384).
-define(wxFIXED_MINSIZE, 32768).
-define(wxRESERVE_SPACE_EVEN_IF_HIDDEN, 2).
-define(wxTILE, 49152).
-define(wxADJUST_MINSIZE, 0).
% From "defs.h": wxUpdateUI
-define(wxUPDATE_UI_NONE, 0).
-define(wxUPDATE_UI_RECURSE, 1).
-define(wxUPDATE_UI_FROMIDLE, 2).
% From "dialog.h"
-define(wxDEFAULT_DIALOG_STYLE, (?wxCAPTION bor ?wxSYSTEM_MENU bor ?wxCLOSE_BOX)).
-define(wxDIALOG_NO_PARENT, 1).
% From "dirctrlg.h"
-define(wxDIRCTRL_DIR_ONLY, 16).
-define(wxDIRCTRL_SELECT_FIRST, 32).
-define(wxDIRCTRL_SHOW_FILTERS, 64).
-define(wxDIRCTRL_3D_INTERNAL, 128).
-define(wxDIRCTRL_EDIT_LABELS, 256).
% From "dirctrlg.h"
-define(wxID_FILTERLISTCTRL, 7001).
-define(wxID_TREECTRL, 7000).
% From "dirdlg.h"
-define(wxDD_DEFAULT_STYLE, (?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER)).
-define(wxDD_NEW_DIR_BUTTON, 0).
-define(wxDD_DIR_MUST_EXIST, 512).
-define(wxDD_CHANGE_DIR, 256).
% From "dirdlgg.h"
% From "dnd.h"
-define(wxDrag_CopyOnly, 0).
-define(wxDrag_AllowMove, 1).
-define(wxDrag_DefaultMove, 3).
% From "dnd.h": wxDragResult
-define(wxDragError, 0).
-define(wxDragNone, 1).
-define(wxDragCopy, 2).
-define(wxDragMove, 3).
-define(wxDragLink, 4).
-define(wxDragCancel, 5).
% From "event.h"
-define(wxMOUSE_BTN_ANY, -1).
-define(wxMOUSE_BTN_NONE, 0).
-define(wxMOUSE_BTN_LEFT, 1).
-define(wxMOUSE_BTN_MIDDLE, 2).
-define(wxMOUSE_BTN_RIGHT, 3).
% From "event.h"
-define(wxJOYSTICK1, 0).
-define(wxJOYSTICK2, 1).
% From "event.h"
-define(wxJOY_BUTTON_ANY, -1).
-define(wxJOY_BUTTON1, 1).
-define(wxJOY_BUTTON2, 2).
-define(wxJOY_BUTTON3, 4).
-define(wxJOY_BUTTON4, 8).
% From "event.h"
% From "event.h": Propagation_state
-define(wxEVENT_PROPAGATE_NONE, 0).
% From "event.h": wxIdleMode
-define(wxIDLE_PROCESS_ALL, 0).
-define(wxIDLE_PROCESS_SPECIFIED, 1).
% From "event.h": wxUpdateUIMode
-define(wxUPDATE_UI_PROCESS_ALL, 0).
-define(wxUPDATE_UI_PROCESS_SPECIFIED, 1).
% From "fdrepdlg.h"
% From "fdrepdlg.h": wxFindReplaceDialogStyles
-define(wxFR_REPLACEDIALOG, 1).
-define(wxFR_NOUPDOWN, 2).
-define(wxFR_NOMATCHCASE, 4).
-define(wxFR_NOWHOLEWORD, 8).
% From "fdrepdlg.h": wxFindReplaceFlags
-define(wxFR_DOWN, 1).
-define(wxFR_WHOLEWORD, 2).
-define(wxFR_MATCHCASE, 4).
% From "filedlg.h"
-define(wxFD_OPEN, 1).
-define(wxFD_SAVE, 2).
-define(wxFD_OVERWRITE_PROMPT, 4).
-define(wxFD_FILE_MUST_EXIST, 16).
-define(wxFD_MULTIPLE, 32).
-define(wxFD_CHANGE_DIR, 128).
-define(wxFD_PREVIEW, 256).
% From "filedlg.h"
-define(wxFD_DEFAULT_STYLE, ?wxFD_OPEN).
% From "filepicker.h"
-define(wxDIRP_DEFAULT_STYLE, ?wxDIRP_DIR_MUST_EXIST).
-define(wxDIRP_USE_TEXTCTRL, ?wxPB_USE_TEXTCTRL).
-define(wxFLP_DEFAULT_STYLE, (?wxFLP_OPEN bor ?wxFLP_FILE_MUST_EXIST)).
-define(wxFLP_USE_TEXTCTRL, ?wxPB_USE_TEXTCTRL).
-define(wxDIRP_CHANGE_DIR, 16).
-define(wxDIRP_DIR_MUST_EXIST, 8).
-define(wxFLP_CHANGE_DIR, 16384).
-define(wxFLP_FILE_MUST_EXIST, 8192).
-define(wxFLP_OVERWRITE_PROMPT, 4096).
-define(wxFLP_SAVE, 2048).
-define(wxFLP_OPEN, 1024).
% From "font.h"
-define(wxFONTFLAG_DEFAULT, 0).
-define(wxFONTFLAG_ITALIC, 1).
-define(wxFONTFLAG_SLANT, 2).
-define(wxFONTFLAG_LIGHT, 4).
-define(wxFONTFLAG_BOLD, 8).
-define(wxFONTFLAG_ANTIALIASED, 16).
-define(wxFONTFLAG_NOT_ANTIALIASED, 32).
-define(wxFONTFLAG_UNDERLINED, 64).
-define(wxFONTFLAG_STRIKETHROUGH, 128).
-define(wxFONTFLAG_MASK, (?wxFONTFLAG_ITALIC bor ?wxFONTFLAG_SLANT bor ?wxFONTFLAG_LIGHT bor ?wxFONTFLAG_BOLD bor ?wxFONTFLAG_ANTIALIASED bor ?wxFONTFLAG_NOT_ANTIALIASED bor ?wxFONTFLAG_UNDERLINED bor ?wxFONTFLAG_STRIKETHROUGH)).
% From "font.h": wxFontFamily
-define(wxFONTFAMILY_DEFAULT, ?wxDEFAULT).
-define(wxFONTFAMILY_DECORATIVE, ?wxDECORATIVE).
-define(wxFONTFAMILY_ROMAN, ?wxROMAN).
-define(wxFONTFAMILY_SCRIPT, ?wxSCRIPT).
-define(wxFONTFAMILY_SWISS, ?wxSWISS).
-define(wxFONTFAMILY_MODERN, ?wxMODERN).
-define(wxFONTFAMILY_TELETYPE, ?wxTELETYPE).
-define(wxFONTFAMILY_MAX, (?wxTELETYPE+1)).
-define(wxFONTFAMILY_UNKNOWN, ?wxFONTFAMILY_MAX).
% From "font.h": wxFontStyle
-define(wxFONTSTYLE_NORMAL, ?wxNORMAL).
-define(wxFONTSTYLE_ITALIC, ?wxITALIC).
-define(wxFONTSTYLE_SLANT, ?wxSLANT).
-define(wxFONTSTYLE_MAX, (?wxSLANT+1)).
% From "font.h": wxFontWeight
-define(wxFONTWEIGHT_NORMAL, ?wxNORMAL).
-define(wxFONTWEIGHT_LIGHT, ?wxLIGHT).
-define(wxFONTWEIGHT_BOLD, ?wxBOLD).
-define(wxFONTWEIGHT_MAX, (?wxBOLD+1)).
% From "fontenc.h": wxFontEncoding
-define(wxFONTENCODING_SYSTEM, -1).
-define(wxFONTENCODING_DEFAULT, 0).
-define(wxFONTENCODING_ISO8859_1, 1).
-define(wxFONTENCODING_ISO8859_2, 2).
-define(wxFONTENCODING_ISO8859_3, 3).
-define(wxFONTENCODING_ISO8859_4, 4).
-define(wxFONTENCODING_ISO8859_5, 5).
-define(wxFONTENCODING_ISO8859_6, 6).
-define(wxFONTENCODING_ISO8859_7, 7).
-define(wxFONTENCODING_ISO8859_8, 8).
-define(wxFONTENCODING_ISO8859_9, 9).
-define(wxFONTENCODING_ISO8859_10, 10).
-define(wxFONTENCODING_ISO8859_11, 11).
-define(wxFONTENCODING_ISO8859_12, 12).
-define(wxFONTENCODING_ISO8859_13, 13).
-define(wxFONTENCODING_ISO8859_14, 14).
-define(wxFONTENCODING_ISO8859_15, 15).
-define(wxFONTENCODING_ISO8859_MAX, 16).
-define(wxFONTENCODING_KOI8, 17).
-define(wxFONTENCODING_KOI8_U, 18).
-define(wxFONTENCODING_ALTERNATIVE, 19).
-define(wxFONTENCODING_BULGARIAN, 20).
-define(wxFONTENCODING_CP437, 21).
-define(wxFONTENCODING_CP850, 22).
-define(wxFONTENCODING_CP852, 23).
-define(wxFONTENCODING_CP855, 24).
-define(wxFONTENCODING_CP866, 25).
-define(wxFONTENCODING_CP874, 26).
-define(wxFONTENCODING_CP932, 27).
-define(wxFONTENCODING_CP936, 28).
-define(wxFONTENCODING_CP949, 29).
-define(wxFONTENCODING_CP950, 30).
-define(wxFONTENCODING_CP1250, 31).
-define(wxFONTENCODING_CP1251, 32).
-define(wxFONTENCODING_CP1252, 33).
-define(wxFONTENCODING_CP1253, 34).
-define(wxFONTENCODING_CP1254, 35).
-define(wxFONTENCODING_CP1255, 36).
-define(wxFONTENCODING_CP1256, 37).
-define(wxFONTENCODING_CP1257, 38).
-define(wxFONTENCODING_CP12_MAX, 39).
-define(wxFONTENCODING_UTF7, 40).
-define(wxFONTENCODING_UTF8, 41).
-define(wxFONTENCODING_EUC_JP, 42).
-define(wxFONTENCODING_UTF16BE, 43).
-define(wxFONTENCODING_UTF16LE, 44).
-define(wxFONTENCODING_UTF32BE, 45).
-define(wxFONTENCODING_UTF32LE, 46).
-define(wxFONTENCODING_MACROMAN, 47).
-define(wxFONTENCODING_MACJAPANESE, 48).
-define(wxFONTENCODING_MACCHINESETRAD, 49).
-define(wxFONTENCODING_MACKOREAN, 50).
-define(wxFONTENCODING_MACARABIC, 51).
-define(wxFONTENCODING_MACHEBREW, 52).
-define(wxFONTENCODING_MACGREEK, 53).
-define(wxFONTENCODING_MACCYRILLIC, 54).
-define(wxFONTENCODING_MACDEVANAGARI, 55).
-define(wxFONTENCODING_MACGURMUKHI, 56).
-define(wxFONTENCODING_MACGUJARATI, 57).
-define(wxFONTENCODING_MACORIYA, 58).
-define(wxFONTENCODING_MACBENGALI, 59).
-define(wxFONTENCODING_MACTAMIL, 60).
-define(wxFONTENCODING_MACTELUGU, 61).
-define(wxFONTENCODING_MACKANNADA, 62).
-define(wxFONTENCODING_MACMALAJALAM, 63).
-define(wxFONTENCODING_MACSINHALESE, 64).
-define(wxFONTENCODING_MACBURMESE, 65).
-define(wxFONTENCODING_MACKHMER, 66).
-define(wxFONTENCODING_MACTHAI, 67).
-define(wxFONTENCODING_MACLAOTIAN, 68).
-define(wxFONTENCODING_MACGEORGIAN, 69).
-define(wxFONTENCODING_MACARMENIAN, 70).
-define(wxFONTENCODING_MACCHINESESIMP, 71).
-define(wxFONTENCODING_MACTIBETAN, 72).
-define(wxFONTENCODING_MACMONGOLIAN, 73).
-define(wxFONTENCODING_MACETHIOPIC, 74).
-define(wxFONTENCODING_MACCENTRALEUR, 75).
-define(wxFONTENCODING_MACVIATNAMESE, 76).
-define(wxFONTENCODING_MACARABICEXT, 77).
-define(wxFONTENCODING_MACSYMBOL, 78).
-define(wxFONTENCODING_MACDINGBATS, 79).
-define(wxFONTENCODING_MACTURKISH, 80).
-define(wxFONTENCODING_MACCROATIAN, 81).
-define(wxFONTENCODING_MACICELANDIC, 82).
-define(wxFONTENCODING_MACROMANIAN, 83).
-define(wxFONTENCODING_MACCELTIC, 84).
-define(wxFONTENCODING_MACGAELIC, 85).
-define(wxFONTENCODING_MACKEYBOARD, 86).
-define(wxFONTENCODING_MAX, 87).
-define(wxFONTENCODING_MACMIN, ?wxFONTENCODING_MACROMAN).
-define(wxFONTENCODING_MACMAX, ?wxFONTENCODING_MACKEYBOARD).
-define(wxFONTENCODING_UTF16, wxe_util:get_const(wxFONTENCODING_UTF16)).
-define(wxFONTENCODING_UTF32, wxe_util:get_const(wxFONTENCODING_UTF32)).
-define(wxFONTENCODING_UNICODE, ?wxFONTENCODING_UTF32).
-define(wxFONTENCODING_GB2312, ?wxFONTENCODING_CP936).
-define(wxFONTENCODING_BIG5, ?wxFONTENCODING_CP950).
-define(wxFONTENCODING_SHIFT_JIS, ?wxFONTENCODING_CP932).
% From "fontpicker.h"
-define(wxFNTP_MAXPOINT_SIZE, 100).
-define(wxFNTP_DEFAULT_STYLE, (?wxFNTP_FONTDESC_AS_LABEL bor ?wxFNTP_USEFONT_FOR_LABEL)).
-define(wxFNTP_USE_TEXTCTRL, ?wxPB_USE_TEXTCTRL).
-define(wxFNTP_USEFONT_FOR_LABEL, 16).
-define(wxFNTP_FONTDESC_AS_LABEL, 8).
% From "frame.h"
-define(wxFRAME_SHAPED, 16).
-define(wxFRAME_FLOAT_ON_PARENT, 8).
-define(wxFRAME_TOOL_WINDOW, 4).
-define(wxFRAME_NO_TASKBAR, 2).
% From "framemanager.h": wxAuiButtonId
-define(wxAUI_BUTTON_CLOSE, 101).
-define(wxAUI_BUTTON_MAXIMIZE_RESTORE, 102).
-define(wxAUI_BUTTON_MINIMIZE, 103).
-define(wxAUI_BUTTON_PIN, 104).
-define(wxAUI_BUTTON_OPTIONS, 105).
-define(wxAUI_BUTTON_WINDOWLIST, 106).
-define(wxAUI_BUTTON_LEFT, 107).
-define(wxAUI_BUTTON_RIGHT, 108).
-define(wxAUI_BUTTON_UP, 109).
-define(wxAUI_BUTTON_DOWN, 110).
-define(wxAUI_BUTTON_CUSTOM1, 201).
-define(wxAUI_BUTTON_CUSTOM2, 202).
-define(wxAUI_BUTTON_CUSTOM3, 203).
% From "framemanager.h": wxAuiManagerDock
-define(wxAUI_DOCK_NONE, 0).
-define(wxAUI_DOCK_TOP, 1).
-define(wxAUI_DOCK_RIGHT, 2).
-define(wxAUI_DOCK_BOTTOM, 3).
-define(wxAUI_DOCK_LEFT, 4).
-define(wxAUI_DOCK_CENTER, 5).
-define(wxAUI_DOCK_CENTRE, ?wxAUI_DOCK_CENTER).
% From "framemanager.h": wxAuiManagerOption
-define(wxAUI_MGR_ALLOW_FLOATING, 1).
-define(wxAUI_MGR_ALLOW_ACTIVE_PANE, 2).
-define(wxAUI_MGR_TRANSPARENT_DRAG, 4).
-define(wxAUI_MGR_TRANSPARENT_HINT, 8).
-define(wxAUI_MGR_VENETIAN_BLINDS_HINT, 16).
-define(wxAUI_MGR_RECTANGLE_HINT, 32).
-define(wxAUI_MGR_HINT_FADE, 64).
-define(wxAUI_MGR_NO_VENETIAN_BLINDS_FADE, 128).
-define(wxAUI_MGR_LIVE_RESIZE, 256).
-define(wxAUI_MGR_DEFAULT, (?wxAUI_MGR_ALLOW_FLOATING bor ?wxAUI_MGR_TRANSPARENT_HINT bor ?wxAUI_MGR_HINT_FADE bor ?wxAUI_MGR_NO_VENETIAN_BLINDS_FADE)).
% From "framemanager.h": wxAuiPaneButtonState
-define(wxAUI_BUTTON_STATE_NORMAL, 0).
-define(wxAUI_BUTTON_STATE_HOVER, 2).
-define(wxAUI_BUTTON_STATE_PRESSED, 4).
-define(wxAUI_BUTTON_STATE_DISABLED, 8).
-define(wxAUI_BUTTON_STATE_HIDDEN, 16).
-define(wxAUI_BUTTON_STATE_CHECKED, 32).
% From "framemanager.h": wxAuiPaneDockArtGradients
-define(wxAUI_GRADIENT_NONE, 0).
-define(wxAUI_GRADIENT_VERTICAL, 1).
-define(wxAUI_GRADIENT_HORIZONTAL, 2).
% From "framemanager.h": wxAuiPaneDockArtSetting
-define(wxAUI_DOCKART_SASH_SIZE, 0).
-define(wxAUI_DOCKART_CAPTION_SIZE, 1).
-define(wxAUI_DOCKART_GRIPPER_SIZE, 2).
-define(wxAUI_DOCKART_PANE_BORDER_SIZE, 3).
-define(wxAUI_DOCKART_PANE_BUTTON_SIZE, 4).
-define(wxAUI_DOCKART_BACKGROUND_COLOUR, 5).
-define(wxAUI_DOCKART_SASH_COLOUR, 6).
-define(wxAUI_DOCKART_ACTIVE_CAPTION_COLOUR, 7).
-define(wxAUI_DOCKART_ACTIVE_CAPTION_GRADIENT_COLOUR, 8).
-define(wxAUI_DOCKART_INACTIVE_CAPTION_COLOUR, 9).
-define(wxAUI_DOCKART_INACTIVE_CAPTION_GRADIENT_COLOUR, 10).
-define(wxAUI_DOCKART_ACTIVE_CAPTION_TEXT_COLOUR, 11).
-define(wxAUI_DOCKART_INACTIVE_CAPTION_TEXT_COLOUR, 12).
-define(wxAUI_DOCKART_BORDER_COLOUR, 13).
-define(wxAUI_DOCKART_GRIPPER_COLOUR, 14).
-define(wxAUI_DOCKART_CAPTION_FONT, 15).
-define(wxAUI_DOCKART_GRADIENT_TYPE, 16).
% From "framemanager.h": wxAuiPaneInsertLevel
-define(wxAUI_INSERT_PANE, 0).
-define(wxAUI_INSERT_ROW, 1).
-define(wxAUI_INSERT_DOCK, 2).
% From "gauge.h"
-define(wxGAUGE_EMULATE_INDETERMINATE_MODE, wxe_util:get_const(wxGAUGE_EMULATE_INDETERMINATE_MODE)).
-define(wxGA_SMOOTH, 32).
-define(wxGA_VERTICAL, ?wxVERTICAL).
-define(wxGA_HORIZONTAL, ?wxHORIZONTAL).
% From "gdicmn.h"
% From "gdicmn.h": wxBitmapType
-define(wxBITMAP_TYPE_INVALID, 0).
-define(wxBITMAP_TYPE_BMP, 1).
-define(wxBITMAP_TYPE_BMP_RESOURCE, 2).
-define(wxBITMAP_TYPE_RESOURCE, ?wxBITMAP_TYPE_BMP_RESOURCE).
-define(wxBITMAP_TYPE_ICO, (?wxBITMAP_TYPE_BMP_RESOURCE+1)).
-define(wxBITMAP_TYPE_ICO_RESOURCE, (?wxBITMAP_TYPE_BMP_RESOURCE+2)).
-define(wxBITMAP_TYPE_CUR, (?wxBITMAP_TYPE_BMP_RESOURCE+3)).
-define(wxBITMAP_TYPE_CUR_RESOURCE, (?wxBITMAP_TYPE_BMP_RESOURCE+4)).
-define(wxBITMAP_TYPE_XBM, (?wxBITMAP_TYPE_BMP_RESOURCE+5)).
-define(wxBITMAP_TYPE_XBM_DATA, (?wxBITMAP_TYPE_BMP_RESOURCE+6)).
-define(wxBITMAP_TYPE_XPM, (?wxBITMAP_TYPE_BMP_RESOURCE+7)).
-define(wxBITMAP_TYPE_XPM_DATA, (?wxBITMAP_TYPE_BMP_RESOURCE+8)).
-define(wxBITMAP_TYPE_TIF, (?wxBITMAP_TYPE_BMP_RESOURCE+9)).
-define(wxBITMAP_TYPE_TIF_RESOURCE, (?wxBITMAP_TYPE_BMP_RESOURCE+10)).
-define(wxBITMAP_TYPE_GIF, (?wxBITMAP_TYPE_BMP_RESOURCE+11)).
-define(wxBITMAP_TYPE_GIF_RESOURCE, (?wxBITMAP_TYPE_BMP_RESOURCE+12)).
-define(wxBITMAP_TYPE_PNG, (?wxBITMAP_TYPE_BMP_RESOURCE+13)).
-define(wxBITMAP_TYPE_PNG_RESOURCE, (?wxBITMAP_TYPE_BMP_RESOURCE+14)).
-define(wxBITMAP_TYPE_JPEG, (?wxBITMAP_TYPE_BMP_RESOURCE+15)).
-define(wxBITMAP_TYPE_JPEG_RESOURCE, (?wxBITMAP_TYPE_BMP_RESOURCE+16)).
-define(wxBITMAP_TYPE_PNM, (?wxBITMAP_TYPE_BMP_RESOURCE+17)).
-define(wxBITMAP_TYPE_PNM_RESOURCE, (?wxBITMAP_TYPE_BMP_RESOURCE+18)).
-define(wxBITMAP_TYPE_PCX, (?wxBITMAP_TYPE_BMP_RESOURCE+19)).
-define(wxBITMAP_TYPE_PCX_RESOURCE, (?wxBITMAP_TYPE_BMP_RESOURCE+20)).
-define(wxBITMAP_TYPE_PICT, (?wxBITMAP_TYPE_BMP_RESOURCE+21)).
-define(wxBITMAP_TYPE_PICT_RESOURCE, (?wxBITMAP_TYPE_BMP_RESOURCE+22)).
-define(wxBITMAP_TYPE_ICON, (?wxBITMAP_TYPE_BMP_RESOURCE+23)).
-define(wxBITMAP_TYPE_ICON_RESOURCE, (?wxBITMAP_TYPE_BMP_RESOURCE+24)).
-define(wxBITMAP_TYPE_ANI, (?wxBITMAP_TYPE_BMP_RESOURCE+25)).
-define(wxBITMAP_TYPE_IFF, (?wxBITMAP_TYPE_BMP_RESOURCE+26)).
-define(wxBITMAP_TYPE_TGA, (?wxBITMAP_TYPE_BMP_RESOURCE+27)).
-define(wxBITMAP_TYPE_MACCURSOR, (?wxBITMAP_TYPE_BMP_RESOURCE+28)).
-define(wxBITMAP_TYPE_MACCURSOR_RESOURCE, (?wxBITMAP_TYPE_BMP_RESOURCE+29)).
-define(wxBITMAP_TYPE_ANY, 50).
% From "gdicmn.h": wxStockCursor
-define(wxCURSOR_NONE, 0).
-define(wxCURSOR_ARROW, 1).
-define(wxCURSOR_RIGHT_ARROW, 2).
-define(wxCURSOR_BULLSEYE, 3).
-define(wxCURSOR_CHAR, 4).
-define(wxCURSOR_CROSS, 5).
-define(wxCURSOR_HAND, 6).
-define(wxCURSOR_IBEAM, 7).
-define(wxCURSOR_LEFT_BUTTON, 8).
-define(wxCURSOR_MAGNIFIER, 9).
-define(wxCURSOR_MIDDLE_BUTTON, 10).
-define(wxCURSOR_NO_ENTRY, 11).
-define(wxCURSOR_PAINT_BRUSH, 12).
-define(wxCURSOR_PENCIL, 13).
-define(wxCURSOR_POINT_LEFT, 14).
-define(wxCURSOR_POINT_RIGHT, 15).
-define(wxCURSOR_QUESTION_ARROW, 16).
-define(wxCURSOR_RIGHT_BUTTON, 17).
-define(wxCURSOR_SIZENESW, 18).
-define(wxCURSOR_SIZENS, 19).
-define(wxCURSOR_SIZENWSE, 20).
-define(wxCURSOR_SIZEWE, 21).
-define(wxCURSOR_SIZING, 22).
-define(wxCURSOR_SPRAYCAN, 23).
-define(wxCURSOR_WAIT, 24).
-define(wxCURSOR_WATCH, 25).
-define(wxCURSOR_BLANK, 26).
-define(wxCURSOR_DEFAULT, wxe_util:get_const(wxCURSOR_DEFAULT)).
-define(wxCURSOR_ARROWWAIT, wxe_util:get_const(wxCURSOR_ARROWWAIT)).
-define(wxCURSOR_MAX, wxe_util:get_const(wxCURSOR_MAX)).
% From "generic_2laywin.h"
-define(wxLAYOUT_QUERY, 256).
-define(wxLAYOUT_MRU_LENGTH, 16).
-define(wxLAYOUT_LENGTH_X, 0).
-define(wxLAYOUT_LENGTH_Y, 8).
% From "generic_2laywin.h": wxLayoutAlignment
-define(wxLAYOUT_NONE, 0).
-define(wxLAYOUT_TOP, 1).
-define(wxLAYOUT_LEFT, 2).
-define(wxLAYOUT_RIGHT, 3).
-define(wxLAYOUT_BOTTOM, 4).
% From "generic_2laywin.h": wxLayoutOrientation
-define(wxLAYOUT_HORIZONTAL, 0).
-define(wxLAYOUT_VERTICAL, 1).
% From "generic_2sashwin.h"
-define(wxSW_3D, (?wxSW_3DSASH bor ?wxSW_3DBORDER)).
-define(wxSW_3DBORDER, 128).
-define(wxSW_3DSASH, 64).
-define(wxSW_BORDER, 32).
-define(wxSW_NOBORDER, 0).
-define(wxSASH_DRAG_LEFT_DOWN, 2).
-define(wxSASH_DRAG_DRAGGING, 1).
-define(wxSASH_DRAG_NONE, 0).
% From "generic_2sashwin.h": wxSashEdgePosition
-define(wxSASH_TOP, 0).
-define(wxSASH_RIGHT, 1).
-define(wxSASH_BOTTOM, 2).
-define(wxSASH_LEFT, 3).
-define(wxSASH_NONE, 100).
% From "generic_2splash.h"
-define(wxSPLASH_NO_TIMEOUT, 0).
-define(wxSPLASH_TIMEOUT, 4).
-define(wxSPLASH_NO_CENTRE, 0).
-define(wxSPLASH_CENTRE_ON_SCREEN, 2).
-define(wxSPLASH_CENTRE_ON_PARENT, 1).
% From "generic_2splitter.h"
-define(wxSPLIT_DRAG_NONE, 0).
-define(wxSPLIT_DRAG_DRAGGING, 1).
-define(wxSPLIT_DRAG_LEFT_DOWN, 2).
% From "generic_2splitter.h": wxSplitMode
-define(wxSPLIT_HORIZONTAL, 1).
-define(wxSPLIT_VERTICAL, 2).
% From "glcanvas.h"
-define(WX_GL_RGBA, 1).
-define(WX_GL_BUFFER_SIZE, 2).
-define(WX_GL_LEVEL, 3).
-define(WX_GL_DOUBLEBUFFER, 4).
-define(WX_GL_STEREO, 5).
-define(WX_GL_AUX_BUFFERS, 6).
-define(WX_GL_MIN_RED, 7).
-define(WX_GL_MIN_GREEN, 8).
-define(WX_GL_MIN_BLUE, 9).
-define(WX_GL_MIN_ALPHA, 10).
-define(WX_GL_DEPTH_SIZE, 11).
-define(WX_GL_STENCIL_SIZE, 12).
-define(WX_GL_MIN_ACCUM_RED, 13).
-define(WX_GL_MIN_ACCUM_GREEN, 14).
-define(WX_GL_MIN_ACCUM_BLUE, 15).
-define(WX_GL_MIN_ACCUM_ALPHA, 16).
% From "hash.h"
-define(wxHASH_SIZE_DEFAULT, 1000).
% From "htmlwin.h"
-define(wxHW_DEFAULT_STYLE, ?wxHW_SCROLLBAR_AUTO).
-define(wxHW_NO_SELECTION, 8).
-define(wxHW_SCROLLBAR_AUTO, 4).
-define(wxHW_SCROLLBAR_NEVER, 2).
% From "htmlwin.h": wxHtmlOpeningStatus
-define(wxHTML_OPEN, 0).
-define(wxHTML_BLOCK, 1).
-define(wxHTML_REDIRECT, 2).
% From "htmprint.h"
-define(wxPAGE_ODD, 0).
-define(wxPAGE_EVEN, 1).
-define(wxPAGE_ALL, 2).
% From "imagbmp.h"
-define(wxBMP_24BPP, 24).
-define(wxBMP_8BPP, 8).
-define(wxBMP_8BPP_GREY, 9).
-define(wxBMP_8BPP_GRAY, ?wxBMP_8BPP_GREY).
-define(wxBMP_8BPP_RED, 10).
-define(wxBMP_8BPP_PALETTE, 11).
-define(wxBMP_4BPP, 4).
-define(wxBMP_1BPP, 1).
-define(wxBMP_1BPP_BW, 2).
% From "image.h"
-define(wxIMAGE_RESOLUTION_INCHES, 1).
-define(wxIMAGE_RESOLUTION_CM, 2).
% From "image.h"
-define(wxIMAGE_QUALITY_NORMAL, 0).
-define(wxIMAGE_QUALITY_HIGH, 1).
% From "imaglist.h"
-define(wxIMAGE_LIST_NORMAL, 0).
-define(wxIMAGE_LIST_SMALL, 1).
-define(wxIMAGE_LIST_STATE, 2).
% From "imaglist.h"
-define(wxIMAGELIST_DRAW_FOCUSED, 8).
-define(wxIMAGELIST_DRAW_SELECTED, 4).
-define(wxIMAGELIST_DRAW_TRANSPARENT, 2).
-define(wxIMAGELIST_DRAW_NORMAL, 1).
% From "intl.h": wxLanguage
-define(wxLANGUAGE_DEFAULT, wxe_util:get_const(wxLANGUAGE_DEFAULT)).
-define(wxLANGUAGE_UNKNOWN, wxe_util:get_const(wxLANGUAGE_UNKNOWN)).
-define(wxLANGUAGE_ABKHAZIAN, wxe_util:get_const(wxLANGUAGE_ABKHAZIAN)).
-define(wxLANGUAGE_AFAR, wxe_util:get_const(wxLANGUAGE_AFAR)).
-define(wxLANGUAGE_AFRIKAANS, wxe_util:get_const(wxLANGUAGE_AFRIKAANS)).
-define(wxLANGUAGE_ALBANIAN, wxe_util:get_const(wxLANGUAGE_ALBANIAN)).
-define(wxLANGUAGE_AMHARIC, wxe_util:get_const(wxLANGUAGE_AMHARIC)).
-define(wxLANGUAGE_ARABIC, wxe_util:get_const(wxLANGUAGE_ARABIC)).
-define(wxLANGUAGE_ARABIC_ALGERIA, wxe_util:get_const(wxLANGUAGE_ARABIC_ALGERIA)).
-define(wxLANGUAGE_ARABIC_BAHRAIN, wxe_util:get_const(wxLANGUAGE_ARABIC_BAHRAIN)).
-define(wxLANGUAGE_ARABIC_EGYPT, wxe_util:get_const(wxLANGUAGE_ARABIC_EGYPT)).
-define(wxLANGUAGE_ARABIC_IRAQ, wxe_util:get_const(wxLANGUAGE_ARABIC_IRAQ)).
-define(wxLANGUAGE_ARABIC_JORDAN, wxe_util:get_const(wxLANGUAGE_ARABIC_JORDAN)).
-define(wxLANGUAGE_ARABIC_KUWAIT, wxe_util:get_const(wxLANGUAGE_ARABIC_KUWAIT)).
-define(wxLANGUAGE_ARABIC_LEBANON, wxe_util:get_const(wxLANGUAGE_ARABIC_LEBANON)).
-define(wxLANGUAGE_ARABIC_LIBYA, wxe_util:get_const(wxLANGUAGE_ARABIC_LIBYA)).
-define(wxLANGUAGE_ARABIC_MOROCCO, wxe_util:get_const(wxLANGUAGE_ARABIC_MOROCCO)).
-define(wxLANGUAGE_ARABIC_OMAN, wxe_util:get_const(wxLANGUAGE_ARABIC_OMAN)).
-define(wxLANGUAGE_ARABIC_QATAR, wxe_util:get_const(wxLANGUAGE_ARABIC_QATAR)).
-define(wxLANGUAGE_ARABIC_SAUDI_ARABIA, wxe_util:get_const(wxLANGUAGE_ARABIC_SAUDI_ARABIA)).
-define(wxLANGUAGE_ARABIC_SUDAN, wxe_util:get_const(wxLANGUAGE_ARABIC_SUDAN)).
-define(wxLANGUAGE_ARABIC_SYRIA, wxe_util:get_const(wxLANGUAGE_ARABIC_SYRIA)).
-define(wxLANGUAGE_ARABIC_TUNISIA, wxe_util:get_const(wxLANGUAGE_ARABIC_TUNISIA)).
-define(wxLANGUAGE_ARABIC_UAE, wxe_util:get_const(wxLANGUAGE_ARABIC_UAE)).
-define(wxLANGUAGE_ARABIC_YEMEN, wxe_util:get_const(wxLANGUAGE_ARABIC_YEMEN)).
-define(wxLANGUAGE_ARMENIAN, wxe_util:get_const(wxLANGUAGE_ARMENIAN)).
-define(wxLANGUAGE_ASSAMESE, wxe_util:get_const(wxLANGUAGE_ASSAMESE)).
-define(wxLANGUAGE_AYMARA, wxe_util:get_const(wxLANGUAGE_AYMARA)).
-define(wxLANGUAGE_AZERI, wxe_util:get_const(wxLANGUAGE_AZERI)).
-define(wxLANGUAGE_AZERI_CYRILLIC, wxe_util:get_const(wxLANGUAGE_AZERI_CYRILLIC)).
-define(wxLANGUAGE_AZERI_LATIN, wxe_util:get_const(wxLANGUAGE_AZERI_LATIN)).
-define(wxLANGUAGE_BASHKIR, wxe_util:get_const(wxLANGUAGE_BASHKIR)).
-define(wxLANGUAGE_BASQUE, wxe_util:get_const(wxLANGUAGE_BASQUE)).
-define(wxLANGUAGE_BELARUSIAN, wxe_util:get_const(wxLANGUAGE_BELARUSIAN)).
-define(wxLANGUAGE_BENGALI, wxe_util:get_const(wxLANGUAGE_BENGALI)).
-define(wxLANGUAGE_BHUTANI, wxe_util:get_const(wxLANGUAGE_BHUTANI)).
-define(wxLANGUAGE_BIHARI, wxe_util:get_const(wxLANGUAGE_BIHARI)).
-define(wxLANGUAGE_BISLAMA, wxe_util:get_const(wxLANGUAGE_BISLAMA)).
-define(wxLANGUAGE_BRETON, wxe_util:get_const(wxLANGUAGE_BRETON)).
-define(wxLANGUAGE_BULGARIAN, wxe_util:get_const(wxLANGUAGE_BULGARIAN)).
-define(wxLANGUAGE_BURMESE, wxe_util:get_const(wxLANGUAGE_BURMESE)).
-define(wxLANGUAGE_CAMBODIAN, wxe_util:get_const(wxLANGUAGE_CAMBODIAN)).
-define(wxLANGUAGE_CATALAN, wxe_util:get_const(wxLANGUAGE_CATALAN)).
-define(wxLANGUAGE_CHINESE, wxe_util:get_const(wxLANGUAGE_CHINESE)).
-define(wxLANGUAGE_CHINESE_SIMPLIFIED, wxe_util:get_const(wxLANGUAGE_CHINESE_SIMPLIFIED)).
-define(wxLANGUAGE_CHINESE_TRADITIONAL, wxe_util:get_const(wxLANGUAGE_CHINESE_TRADITIONAL)).
-define(wxLANGUAGE_CHINESE_HONGKONG, wxe_util:get_const(wxLANGUAGE_CHINESE_HONGKONG)).
-define(wxLANGUAGE_CHINESE_MACAU, wxe_util:get_const(wxLANGUAGE_CHINESE_MACAU)).
-define(wxLANGUAGE_CHINESE_SINGAPORE, wxe_util:get_const(wxLANGUAGE_CHINESE_SINGAPORE)).
-define(wxLANGUAGE_CHINESE_TAIWAN, wxe_util:get_const(wxLANGUAGE_CHINESE_TAIWAN)).
-define(wxLANGUAGE_CORSICAN, wxe_util:get_const(wxLANGUAGE_CORSICAN)).
-define(wxLANGUAGE_CROATIAN, wxe_util:get_const(wxLANGUAGE_CROATIAN)).
-define(wxLANGUAGE_CZECH, wxe_util:get_const(wxLANGUAGE_CZECH)).
-define(wxLANGUAGE_DANISH, wxe_util:get_const(wxLANGUAGE_DANISH)).
-define(wxLANGUAGE_DUTCH, wxe_util:get_const(wxLANGUAGE_DUTCH)).
-define(wxLANGUAGE_DUTCH_BELGIAN, wxe_util:get_const(wxLANGUAGE_DUTCH_BELGIAN)).
-define(wxLANGUAGE_ENGLISH, wxe_util:get_const(wxLANGUAGE_ENGLISH)).
-define(wxLANGUAGE_ENGLISH_UK, wxe_util:get_const(wxLANGUAGE_ENGLISH_UK)).
-define(wxLANGUAGE_ENGLISH_US, wxe_util:get_const(wxLANGUAGE_ENGLISH_US)).
-define(wxLANGUAGE_ENGLISH_AUSTRALIA, wxe_util:get_const(wxLANGUAGE_ENGLISH_AUSTRALIA)).
-define(wxLANGUAGE_ENGLISH_BELIZE, wxe_util:get_const(wxLANGUAGE_ENGLISH_BELIZE)).
-define(wxLANGUAGE_ENGLISH_BOTSWANA, wxe_util:get_const(wxLANGUAGE_ENGLISH_BOTSWANA)).
-define(wxLANGUAGE_ENGLISH_CANADA, wxe_util:get_const(wxLANGUAGE_ENGLISH_CANADA)).
-define(wxLANGUAGE_ENGLISH_CARIBBEAN, wxe_util:get_const(wxLANGUAGE_ENGLISH_CARIBBEAN)).
-define(wxLANGUAGE_ENGLISH_DENMARK, wxe_util:get_const(wxLANGUAGE_ENGLISH_DENMARK)).
-define(wxLANGUAGE_ENGLISH_EIRE, wxe_util:get_const(wxLANGUAGE_ENGLISH_EIRE)).
-define(wxLANGUAGE_ENGLISH_JAMAICA, wxe_util:get_const(wxLANGUAGE_ENGLISH_JAMAICA)).
-define(wxLANGUAGE_ENGLISH_NEW_ZEALAND, wxe_util:get_const(wxLANGUAGE_ENGLISH_NEW_ZEALAND)).
-define(wxLANGUAGE_ENGLISH_PHILIPPINES, wxe_util:get_const(wxLANGUAGE_ENGLISH_PHILIPPINES)).
-define(wxLANGUAGE_ENGLISH_SOUTH_AFRICA, wxe_util:get_const(wxLANGUAGE_ENGLISH_SOUTH_AFRICA)).
-define(wxLANGUAGE_ENGLISH_TRINIDAD, wxe_util:get_const(wxLANGUAGE_ENGLISH_TRINIDAD)).
-define(wxLANGUAGE_ENGLISH_ZIMBABWE, wxe_util:get_const(wxLANGUAGE_ENGLISH_ZIMBABWE)).
-define(wxLANGUAGE_ESPERANTO, wxe_util:get_const(wxLANGUAGE_ESPERANTO)).
-define(wxLANGUAGE_ESTONIAN, wxe_util:get_const(wxLANGUAGE_ESTONIAN)).
-define(wxLANGUAGE_FAEROESE, wxe_util:get_const(wxLANGUAGE_FAEROESE)).
-define(wxLANGUAGE_FARSI, wxe_util:get_const(wxLANGUAGE_FARSI)).
-define(wxLANGUAGE_FIJI, wxe_util:get_const(wxLANGUAGE_FIJI)).
-define(wxLANGUAGE_FINNISH, wxe_util:get_const(wxLANGUAGE_FINNISH)).
-define(wxLANGUAGE_FRENCH, wxe_util:get_const(wxLANGUAGE_FRENCH)).
-define(wxLANGUAGE_FRENCH_BELGIAN, wxe_util:get_const(wxLANGUAGE_FRENCH_BELGIAN)).
-define(wxLANGUAGE_FRENCH_CANADIAN, wxe_util:get_const(wxLANGUAGE_FRENCH_CANADIAN)).
-define(wxLANGUAGE_FRENCH_LUXEMBOURG, wxe_util:get_const(wxLANGUAGE_FRENCH_LUXEMBOURG)).
-define(wxLANGUAGE_FRENCH_MONACO, wxe_util:get_const(wxLANGUAGE_FRENCH_MONACO)).
-define(wxLANGUAGE_FRENCH_SWISS, wxe_util:get_const(wxLANGUAGE_FRENCH_SWISS)).
-define(wxLANGUAGE_FRISIAN, wxe_util:get_const(wxLANGUAGE_FRISIAN)).
-define(wxLANGUAGE_GALICIAN, wxe_util:get_const(wxLANGUAGE_GALICIAN)).
-define(wxLANGUAGE_GEORGIAN, wxe_util:get_const(wxLANGUAGE_GEORGIAN)).
-define(wxLANGUAGE_GERMAN, wxe_util:get_const(wxLANGUAGE_GERMAN)).
-define(wxLANGUAGE_GERMAN_AUSTRIAN, wxe_util:get_const(wxLANGUAGE_GERMAN_AUSTRIAN)).
-define(wxLANGUAGE_GERMAN_BELGIUM, wxe_util:get_const(wxLANGUAGE_GERMAN_BELGIUM)).
-define(wxLANGUAGE_GERMAN_LIECHTENSTEIN, wxe_util:get_const(wxLANGUAGE_GERMAN_LIECHTENSTEIN)).
-define(wxLANGUAGE_GERMAN_LUXEMBOURG, wxe_util:get_const(wxLANGUAGE_GERMAN_LUXEMBOURG)).
-define(wxLANGUAGE_GERMAN_SWISS, wxe_util:get_const(wxLANGUAGE_GERMAN_SWISS)).
-define(wxLANGUAGE_GREEK, wxe_util:get_const(wxLANGUAGE_GREEK)).
-define(wxLANGUAGE_GREENLANDIC, wxe_util:get_const(wxLANGUAGE_GREENLANDIC)).
-define(wxLANGUAGE_GUARANI, wxe_util:get_const(wxLANGUAGE_GUARANI)).
-define(wxLANGUAGE_GUJARATI, wxe_util:get_const(wxLANGUAGE_GUJARATI)).
-define(wxLANGUAGE_HAUSA, wxe_util:get_const(wxLANGUAGE_HAUSA)).
-define(wxLANGUAGE_HEBREW, wxe_util:get_const(wxLANGUAGE_HEBREW)).
-define(wxLANGUAGE_HINDI, wxe_util:get_const(wxLANGUAGE_HINDI)).
-define(wxLANGUAGE_HUNGARIAN, wxe_util:get_const(wxLANGUAGE_HUNGARIAN)).
-define(wxLANGUAGE_ICELANDIC, wxe_util:get_const(wxLANGUAGE_ICELANDIC)).
-define(wxLANGUAGE_INDONESIAN, wxe_util:get_const(wxLANGUAGE_INDONESIAN)).
-define(wxLANGUAGE_INTERLINGUA, wxe_util:get_const(wxLANGUAGE_INTERLINGUA)).
-define(wxLANGUAGE_INTERLINGUE, wxe_util:get_const(wxLANGUAGE_INTERLINGUE)).
-define(wxLANGUAGE_INUKTITUT, wxe_util:get_const(wxLANGUAGE_INUKTITUT)).
-define(wxLANGUAGE_INUPIAK, wxe_util:get_const(wxLANGUAGE_INUPIAK)).
-define(wxLANGUAGE_IRISH, wxe_util:get_const(wxLANGUAGE_IRISH)).
-define(wxLANGUAGE_ITALIAN, wxe_util:get_const(wxLANGUAGE_ITALIAN)).
-define(wxLANGUAGE_ITALIAN_SWISS, wxe_util:get_const(wxLANGUAGE_ITALIAN_SWISS)).
-define(wxLANGUAGE_JAPANESE, wxe_util:get_const(wxLANGUAGE_JAPANESE)).
-define(wxLANGUAGE_JAVANESE, wxe_util:get_const(wxLANGUAGE_JAVANESE)).
-define(wxLANGUAGE_KANNADA, wxe_util:get_const(wxLANGUAGE_KANNADA)).
-define(wxLANGUAGE_KASHMIRI, wxe_util:get_const(wxLANGUAGE_KASHMIRI)).
-define(wxLANGUAGE_KASHMIRI_INDIA, wxe_util:get_const(wxLANGUAGE_KASHMIRI_INDIA)).
-define(wxLANGUAGE_KAZAKH, wxe_util:get_const(wxLANGUAGE_KAZAKH)).
-define(wxLANGUAGE_KERNEWEK, wxe_util:get_const(wxLANGUAGE_KERNEWEK)).
-define(wxLANGUAGE_KINYARWANDA, wxe_util:get_const(wxLANGUAGE_KINYARWANDA)).
-define(wxLANGUAGE_KIRGHIZ, wxe_util:get_const(wxLANGUAGE_KIRGHIZ)).
-define(wxLANGUAGE_KIRUNDI, wxe_util:get_const(wxLANGUAGE_KIRUNDI)).
-define(wxLANGUAGE_KONKANI, wxe_util:get_const(wxLANGUAGE_KONKANI)).
-define(wxLANGUAGE_KOREAN, wxe_util:get_const(wxLANGUAGE_KOREAN)).
-define(wxLANGUAGE_KURDISH, wxe_util:get_const(wxLANGUAGE_KURDISH)).
-define(wxLANGUAGE_LAOTHIAN, wxe_util:get_const(wxLANGUAGE_LAOTHIAN)).
-define(wxLANGUAGE_LATIN, wxe_util:get_const(wxLANGUAGE_LATIN)).
-define(wxLANGUAGE_LATVIAN, wxe_util:get_const(wxLANGUAGE_LATVIAN)).
-define(wxLANGUAGE_LINGALA, wxe_util:get_const(wxLANGUAGE_LINGALA)).
-define(wxLANGUAGE_LITHUANIAN, wxe_util:get_const(wxLANGUAGE_LITHUANIAN)).
-define(wxLANGUAGE_MACEDONIAN, wxe_util:get_const(wxLANGUAGE_MACEDONIAN)).
-define(wxLANGUAGE_MALAGASY, wxe_util:get_const(wxLANGUAGE_MALAGASY)).
-define(wxLANGUAGE_MALAY, wxe_util:get_const(wxLANGUAGE_MALAY)).
-define(wxLANGUAGE_MALAYALAM, wxe_util:get_const(wxLANGUAGE_MALAYALAM)).
-define(wxLANGUAGE_MALAY_BRUNEI_DARUSSALAM, wxe_util:get_const(wxLANGUAGE_MALAY_BRUNEI_DARUSSALAM)).
-define(wxLANGUAGE_MALAY_MALAYSIA, wxe_util:get_const(wxLANGUAGE_MALAY_MALAYSIA)).
-define(wxLANGUAGE_MALTESE, wxe_util:get_const(wxLANGUAGE_MALTESE)).
-define(wxLANGUAGE_MANIPURI, wxe_util:get_const(wxLANGUAGE_MANIPURI)).
-define(wxLANGUAGE_MAORI, wxe_util:get_const(wxLANGUAGE_MAORI)).
-define(wxLANGUAGE_MARATHI, wxe_util:get_const(wxLANGUAGE_MARATHI)).
-define(wxLANGUAGE_MOLDAVIAN, wxe_util:get_const(wxLANGUAGE_MOLDAVIAN)).
-define(wxLANGUAGE_MONGOLIAN, wxe_util:get_const(wxLANGUAGE_MONGOLIAN)).
-define(wxLANGUAGE_NAURU, wxe_util:get_const(wxLANGUAGE_NAURU)).
-define(wxLANGUAGE_NEPALI, wxe_util:get_const(wxLANGUAGE_NEPALI)).
-define(wxLANGUAGE_NEPALI_INDIA, wxe_util:get_const(wxLANGUAGE_NEPALI_INDIA)).
-define(wxLANGUAGE_NORWEGIAN_BOKMAL, wxe_util:get_const(wxLANGUAGE_NORWEGIAN_BOKMAL)).
-define(wxLANGUAGE_NORWEGIAN_NYNORSK, wxe_util:get_const(wxLANGUAGE_NORWEGIAN_NYNORSK)).
-define(wxLANGUAGE_OCCITAN, wxe_util:get_const(wxLANGUAGE_OCCITAN)).
-define(wxLANGUAGE_ORIYA, wxe_util:get_const(wxLANGUAGE_ORIYA)).
-define(wxLANGUAGE_OROMO, wxe_util:get_const(wxLANGUAGE_OROMO)).
-define(wxLANGUAGE_PASHTO, wxe_util:get_const(wxLANGUAGE_PASHTO)).
-define(wxLANGUAGE_POLISH, wxe_util:get_const(wxLANGUAGE_POLISH)).
-define(wxLANGUAGE_PORTUGUESE, wxe_util:get_const(wxLANGUAGE_PORTUGUESE)).
-define(wxLANGUAGE_PORTUGUESE_BRAZILIAN, wxe_util:get_const(wxLANGUAGE_PORTUGUESE_BRAZILIAN)).
-define(wxLANGUAGE_PUNJABI, wxe_util:get_const(wxLANGUAGE_PUNJABI)).
-define(wxLANGUAGE_QUECHUA, wxe_util:get_const(wxLANGUAGE_QUECHUA)).
-define(wxLANGUAGE_RHAETO_ROMANCE, wxe_util:get_const(wxLANGUAGE_RHAETO_ROMANCE)).
-define(wxLANGUAGE_ROMANIAN, wxe_util:get_const(wxLANGUAGE_ROMANIAN)).
-define(wxLANGUAGE_RUSSIAN, wxe_util:get_const(wxLANGUAGE_RUSSIAN)).
-define(wxLANGUAGE_RUSSIAN_UKRAINE, wxe_util:get_const(wxLANGUAGE_RUSSIAN_UKRAINE)).
-define(wxLANGUAGE_SAMOAN, wxe_util:get_const(wxLANGUAGE_SAMOAN)).
-define(wxLANGUAGE_SANGHO, wxe_util:get_const(wxLANGUAGE_SANGHO)).
-define(wxLANGUAGE_SANSKRIT, wxe_util:get_const(wxLANGUAGE_SANSKRIT)).
-define(wxLANGUAGE_SCOTS_GAELIC, wxe_util:get_const(wxLANGUAGE_SCOTS_GAELIC)).
-define(wxLANGUAGE_SERBIAN, wxe_util:get_const(wxLANGUAGE_SERBIAN)).
-define(wxLANGUAGE_SERBIAN_CYRILLIC, wxe_util:get_const(wxLANGUAGE_SERBIAN_CYRILLIC)).
-define(wxLANGUAGE_SERBIAN_LATIN, wxe_util:get_const(wxLANGUAGE_SERBIAN_LATIN)).
-define(wxLANGUAGE_SERBO_CROATIAN, wxe_util:get_const(wxLANGUAGE_SERBO_CROATIAN)).
-define(wxLANGUAGE_SESOTHO, wxe_util:get_const(wxLANGUAGE_SESOTHO)).
-define(wxLANGUAGE_SETSWANA, wxe_util:get_const(wxLANGUAGE_SETSWANA)).
-define(wxLANGUAGE_SHONA, wxe_util:get_const(wxLANGUAGE_SHONA)).
-define(wxLANGUAGE_SINDHI, wxe_util:get_const(wxLANGUAGE_SINDHI)).
-define(wxLANGUAGE_SINHALESE, wxe_util:get_const(wxLANGUAGE_SINHALESE)).
-define(wxLANGUAGE_SISWATI, wxe_util:get_const(wxLANGUAGE_SISWATI)).
-define(wxLANGUAGE_SLOVAK, wxe_util:get_const(wxLANGUAGE_SLOVAK)).
-define(wxLANGUAGE_SLOVENIAN, wxe_util:get_const(wxLANGUAGE_SLOVENIAN)).
-define(wxLANGUAGE_SOMALI, wxe_util:get_const(wxLANGUAGE_SOMALI)).
-define(wxLANGUAGE_SPANISH, wxe_util:get_const(wxLANGUAGE_SPANISH)).
-define(wxLANGUAGE_SPANISH_ARGENTINA, wxe_util:get_const(wxLANGUAGE_SPANISH_ARGENTINA)).
-define(wxLANGUAGE_SPANISH_BOLIVIA, wxe_util:get_const(wxLANGUAGE_SPANISH_BOLIVIA)).
-define(wxLANGUAGE_SPANISH_CHILE, wxe_util:get_const(wxLANGUAGE_SPANISH_CHILE)).
-define(wxLANGUAGE_SPANISH_COLOMBIA, wxe_util:get_const(wxLANGUAGE_SPANISH_COLOMBIA)).
-define(wxLANGUAGE_SPANISH_COSTA_RICA, wxe_util:get_const(wxLANGUAGE_SPANISH_COSTA_RICA)).
-define(wxLANGUAGE_SPANISH_DOMINICAN_REPUBLIC, wxe_util:get_const(wxLANGUAGE_SPANISH_DOMINICAN_REPUBLIC)).
-define(wxLANGUAGE_SPANISH_ECUADOR, wxe_util:get_const(wxLANGUAGE_SPANISH_ECUADOR)).
-define(wxLANGUAGE_SPANISH_EL_SALVADOR, wxe_util:get_const(wxLANGUAGE_SPANISH_EL_SALVADOR)).
-define(wxLANGUAGE_SPANISH_GUATEMALA, wxe_util:get_const(wxLANGUAGE_SPANISH_GUATEMALA)).
-define(wxLANGUAGE_SPANISH_HONDURAS, wxe_util:get_const(wxLANGUAGE_SPANISH_HONDURAS)).
-define(wxLANGUAGE_SPANISH_MEXICAN, wxe_util:get_const(wxLANGUAGE_SPANISH_MEXICAN)).
-define(wxLANGUAGE_SPANISH_MODERN, wxe_util:get_const(wxLANGUAGE_SPANISH_MODERN)).
-define(wxLANGUAGE_SPANISH_NICARAGUA, wxe_util:get_const(wxLANGUAGE_SPANISH_NICARAGUA)).
-define(wxLANGUAGE_SPANISH_PANAMA, wxe_util:get_const(wxLANGUAGE_SPANISH_PANAMA)).
-define(wxLANGUAGE_SPANISH_PARAGUAY, wxe_util:get_const(wxLANGUAGE_SPANISH_PARAGUAY)).
-define(wxLANGUAGE_SPANISH_PERU, wxe_util:get_const(wxLANGUAGE_SPANISH_PERU)).
-define(wxLANGUAGE_SPANISH_PUERTO_RICO, wxe_util:get_const(wxLANGUAGE_SPANISH_PUERTO_RICO)).
-define(wxLANGUAGE_SPANISH_URUGUAY, wxe_util:get_const(wxLANGUAGE_SPANISH_URUGUAY)).
-define(wxLANGUAGE_SPANISH_US, wxe_util:get_const(wxLANGUAGE_SPANISH_US)).
-define(wxLANGUAGE_SPANISH_VENEZUELA, wxe_util:get_const(wxLANGUAGE_SPANISH_VENEZUELA)).
-define(wxLANGUAGE_SUNDANESE, wxe_util:get_const(wxLANGUAGE_SUNDANESE)).
-define(wxLANGUAGE_SWAHILI, wxe_util:get_const(wxLANGUAGE_SWAHILI)).
-define(wxLANGUAGE_SWEDISH, wxe_util:get_const(wxLANGUAGE_SWEDISH)).
-define(wxLANGUAGE_SWEDISH_FINLAND, wxe_util:get_const(wxLANGUAGE_SWEDISH_FINLAND)).
-define(wxLANGUAGE_TAGALOG, wxe_util:get_const(wxLANGUAGE_TAGALOG)).
-define(wxLANGUAGE_TAJIK, wxe_util:get_const(wxLANGUAGE_TAJIK)).
-define(wxLANGUAGE_TAMIL, wxe_util:get_const(wxLANGUAGE_TAMIL)).
-define(wxLANGUAGE_TATAR, wxe_util:get_const(wxLANGUAGE_TATAR)).
-define(wxLANGUAGE_TELUGU, wxe_util:get_const(wxLANGUAGE_TELUGU)).
-define(wxLANGUAGE_THAI, wxe_util:get_const(wxLANGUAGE_THAI)).
-define(wxLANGUAGE_TIBETAN, wxe_util:get_const(wxLANGUAGE_TIBETAN)).
-define(wxLANGUAGE_TIGRINYA, wxe_util:get_const(wxLANGUAGE_TIGRINYA)).
-define(wxLANGUAGE_TONGA, wxe_util:get_const(wxLANGUAGE_TONGA)).
-define(wxLANGUAGE_TSONGA, wxe_util:get_const(wxLANGUAGE_TSONGA)).
-define(wxLANGUAGE_TURKISH, wxe_util:get_const(wxLANGUAGE_TURKISH)).
-define(wxLANGUAGE_TURKMEN, wxe_util:get_const(wxLANGUAGE_TURKMEN)).
-define(wxLANGUAGE_TWI, wxe_util:get_const(wxLANGUAGE_TWI)).
-define(wxLANGUAGE_UIGHUR, wxe_util:get_const(wxLANGUAGE_UIGHUR)).
-define(wxLANGUAGE_UKRAINIAN, wxe_util:get_const(wxLANGUAGE_UKRAINIAN)).
-define(wxLANGUAGE_URDU, wxe_util:get_const(wxLANGUAGE_URDU)).
-define(wxLANGUAGE_URDU_INDIA, wxe_util:get_const(wxLANGUAGE_URDU_INDIA)).
-define(wxLANGUAGE_URDU_PAKISTAN, wxe_util:get_const(wxLANGUAGE_URDU_PAKISTAN)).
-define(wxLANGUAGE_UZBEK, wxe_util:get_const(wxLANGUAGE_UZBEK)).
-define(wxLANGUAGE_UZBEK_CYRILLIC, wxe_util:get_const(wxLANGUAGE_UZBEK_CYRILLIC)).
-define(wxLANGUAGE_UZBEK_LATIN, wxe_util:get_const(wxLANGUAGE_UZBEK_LATIN)).
-define(wxLANGUAGE_VIETNAMESE, wxe_util:get_const(wxLANGUAGE_VIETNAMESE)).
-define(wxLANGUAGE_VOLAPUK, wxe_util:get_const(wxLANGUAGE_VOLAPUK)).
-define(wxLANGUAGE_WELSH, wxe_util:get_const(wxLANGUAGE_WELSH)).
-define(wxLANGUAGE_WOLOF, wxe_util:get_const(wxLANGUAGE_WOLOF)).
-define(wxLANGUAGE_XHOSA, wxe_util:get_const(wxLANGUAGE_XHOSA)).
-define(wxLANGUAGE_YIDDISH, wxe_util:get_const(wxLANGUAGE_YIDDISH)).
-define(wxLANGUAGE_YORUBA, wxe_util:get_const(wxLANGUAGE_YORUBA)).
-define(wxLANGUAGE_ZHUANG, wxe_util:get_const(wxLANGUAGE_ZHUANG)).
-define(wxLANGUAGE_ZULU, wxe_util:get_const(wxLANGUAGE_ZULU)).
-define(wxLANGUAGE_USER_DEFINED, wxe_util:get_const(wxLANGUAGE_USER_DEFINED)).
-define(wxLANGUAGE_VALENCIAN, wxe_util:get_const(wxLANGUAGE_VALENCIAN)).
-define(wxLANGUAGE_SAMI, wxe_util:get_const(wxLANGUAGE_SAMI)).
% From "intl.h": wxLayoutDirection
-define(wxLayout_Default, 0).
-define(wxLayout_LeftToRight, 1).
-define(wxLayout_RightToLeft, 2).
% From "intl.h": wxLocaleCategory
-define(wxLOCALE_CAT_NUMBER, 0).
-define(wxLOCALE_CAT_DATE, 1).
-define(wxLOCALE_CAT_MONEY, 2).
-define(wxLOCALE_CAT_MAX, 3).
% From "intl.h": wxLocaleInfo
-define(wxLOCALE_THOUSANDS_SEP, 0).
-define(wxLOCALE_DECIMAL_POINT, 1).
% From "intl.h": wxLocaleInitFlags
-define(wxLOCALE_LOAD_DEFAULT, 1).
-define(wxLOCALE_CONV_ENCODING, 2).
% From "layout.h"
-define(wxLAYOUT_DEFAULT_MARGIN, 0).
% From "layout.h": wxEdge
-define(wxLeft, 0).
-define(wxTop, 1).
-define(wxRight, 2).
-define(wxBottom, 3).
-define(wxWidth, 4).
-define(wxHeight, 5).
-define(wxCentre, 6).
-define(wxCenter, ?wxCentre).
-define(wxCentreX, (?wxCentre+1)).
-define(wxCentreY, (?wxCentre+2)).
% From "layout.h": wxRelationship
-define(wxUnconstrained, 0).
-define(wxAsIs, 1).
-define(wxPercentOf, 2).
-define(wxAbove, 3).
-define(wxBelow, 4).
-define(wxLeftOf, 5).
-define(wxRightOf, 6).
-define(wxSameAs, 7).
-define(wxAbsolute, 8).
% From "list.h": wxKeyType
-define(wxKEY_NONE, 0).
-define(wxKEY_INTEGER, 1).
-define(wxKEY_STRING, 2).
% From "listbase.h"
-define(wxLIST_NEXT_ABOVE, 0).
-define(wxLIST_NEXT_ALL, 1).
-define(wxLIST_NEXT_BELOW, 2).
-define(wxLIST_NEXT_LEFT, 3).
-define(wxLIST_NEXT_RIGHT, 4).
% From "listbase.h"
-define(wxLIST_ALIGN_DEFAULT, 0).
-define(wxLIST_ALIGN_LEFT, 1).
-define(wxLIST_ALIGN_TOP, 2).
-define(wxLIST_ALIGN_SNAP_TO_GRID, 3).
% From "listbase.h"
-define(wxLIST_AUTOSIZE, -1).
-define(wxLIST_AUTOSIZE_USEHEADER, -2).
% From "listbase.h"
-define(wxLIST_RECT_BOUNDS, 0).
-define(wxLIST_RECT_ICON, 1).
-define(wxLIST_RECT_LABEL, 2).
% From "listbase.h"
-define(wxLIST_FIND_UP, 0).
-define(wxLIST_FIND_DOWN, 1).
-define(wxLIST_FIND_LEFT, 2).
-define(wxLIST_FIND_RIGHT, 3).
% From "listbase.h"
-define(wxLIST_HITTEST_ONITEM, (?wxLIST_HITTEST_ONITEMICON bor ?wxLIST_HITTEST_ONITEMLABEL bor ?wxLIST_HITTEST_ONITEMSTATEICON)).
-define(wxLIST_HITTEST_TORIGHT, 2048).
-define(wxLIST_HITTEST_TOLEFT, 1024).
-define(wxLIST_HITTEST_ONITEMSTATEICON, 512).
-define(wxLIST_HITTEST_ONITEMRIGHT, 256).
-define(wxLIST_HITTEST_ONITEMLABEL, 128).
-define(wxLIST_HITTEST_ONITEMICON, 32).
-define(wxLIST_HITTEST_NOWHERE, 4).
-define(wxLIST_HITTEST_BELOW, 2).
-define(wxLIST_HITTEST_ABOVE, 1).
-define(wxLIST_STATE_SOURCE, 256).
-define(wxLIST_STATE_PICKED, 128).
-define(wxLIST_STATE_INUSE, 64).
-define(wxLIST_STATE_FILTERED, 32).
-define(wxLIST_STATE_DISABLED, 16).
-define(wxLIST_STATE_CUT, 8).
-define(wxLIST_STATE_SELECTED, 4).
-define(wxLIST_STATE_FOCUSED, 2).
-define(wxLIST_STATE_DROPHILITED, 1).
-define(wxLIST_STATE_DONTCARE, 0).
-define(wxLIST_MASK_FORMAT, 64).
-define(wxLIST_MASK_WIDTH, 32).
-define(wxLIST_SET_ITEM, 16).
-define(wxLIST_MASK_DATA, 8).
-define(wxLIST_MASK_IMAGE, 4).
-define(wxLIST_MASK_TEXT, 2).
-define(wxLIST_MASK_STATE, 1).
-define(wxLC_USER_TEXT, ?wxLC_VIRTUAL).
-define(wxLC_MASK_SORT, (?wxLC_SORT_ASCENDING bor ?wxLC_SORT_DESCENDING)).
-define(wxLC_MASK_ALIGN, (?wxLC_ALIGN_TOP bor ?wxLC_ALIGN_LEFT)).
-define(wxLC_MASK_TYPE, (?wxLC_ICON bor ?wxLC_SMALL_ICON bor ?wxLC_LIST bor ?wxLC_REPORT)).
-define(wxLC_SORT_DESCENDING, 32768).
-define(wxLC_SORT_ASCENDING, 16384).
-define(wxLC_SINGLE_SEL, 8192).
-define(wxLC_NO_SORT_HEADER, 4096).
-define(wxLC_NO_HEADER, 2048).
-define(wxLC_EDIT_LABELS, 1024).
-define(wxLC_VIRTUAL, 512).
-define(wxLC_AUTOARRANGE, 256).
-define(wxLC_ALIGN_LEFT, 128).
-define(wxLC_ALIGN_TOP, 64).
-define(wxLC_REPORT, 32).
-define(wxLC_LIST, 16).
-define(wxLC_SMALL_ICON, 8).
-define(wxLC_ICON, 4).
-define(wxLC_HRULES, 2).
-define(wxLC_VRULES, 1).
% From "listbase.h": wxListColumnFormat
-define(wxLIST_FORMAT_LEFT, 0).
-define(wxLIST_FORMAT_RIGHT, 1).
-define(wxLIST_FORMAT_CENTRE, 2).
-define(wxLIST_FORMAT_CENTER, ?wxLIST_FORMAT_CENTRE).
% From "listbook.h"
-define(wxLB_ALIGN_MASK, ?wxBK_ALIGN_MASK).
-define(wxLB_RIGHT, ?wxBK_RIGHT).
-define(wxLB_LEFT, ?wxBK_LEFT).
-define(wxLB_BOTTOM, ?wxBK_BOTTOM).
-define(wxLB_TOP, ?wxBK_TOP).
-define(wxLB_DEFAULT, ?wxBK_DEFAULT).
% From "log.h"
-define(wxTraceRefCount, 8).
-define(wxTraceResAlloc, 4).
-define(wxTraceMessages, 2).
-define(wxTraceMemAlloc, 1).
% From "notebook.h"
-define(wxNB_HITTEST_NOWHERE, ?wxBK_HITTEST_NOWHERE).
-define(wxNB_HITTEST_ONICON, ?wxBK_HITTEST_ONICON).
-define(wxNB_HITTEST_ONLABEL, ?wxBK_HITTEST_ONLABEL).
-define(wxNB_HITTEST_ONITEM, ?wxBK_HITTEST_ONITEM).
-define(wxNB_HITTEST_ONPAGE, ?wxBK_HITTEST_ONPAGE).
% From "notebook.h"
-define(wxNB_FLAT, 2048).
-define(wxNB_NOPAGETHEME, 1024).
-define(wxNB_MULTILINE, 512).
-define(wxNB_FIXEDWIDTH, 256).
-define(wxNB_RIGHT, ?wxBK_RIGHT).
-define(wxNB_LEFT, ?wxBK_LEFT).
-define(wxNB_BOTTOM, ?wxBK_BOTTOM).
-define(wxNB_TOP, ?wxBK_TOP).
-define(wxNB_DEFAULT, ?wxBK_DEFAULT).
% From "pickerbase.h"
-define(wxPB_USE_TEXTCTRL, 2).
% From "prntbase.h"
-define(wxID_PREVIEW_GOTO, 8).
-define(wxID_PREVIEW_LAST, 7).
-define(wxID_PREVIEW_FIRST, 6).
-define(wxID_PREVIEW_ZOOM, 5).
-define(wxID_PREVIEW_PRINT, 4).
-define(wxID_PREVIEW_PREVIOUS, 3).
-define(wxID_PREVIEW_NEXT, 2).
-define(wxID_PREVIEW_CLOSE, 1).
-define(wxPREVIEW_DEFAULT, (?wxPREVIEW_PREVIOUS bor ?wxPREVIEW_NEXT bor ?wxPREVIEW_ZOOM bor ?wxPREVIEW_FIRST bor ?wxPREVIEW_GOTO bor ?wxPREVIEW_LAST)).
-define(wxPREVIEW_GOTO, 64).
-define(wxPREVIEW_LAST, 32).
-define(wxPREVIEW_FIRST, 16).
-define(wxPREVIEW_ZOOM, 8).
-define(wxPREVIEW_NEXT, 4).
-define(wxPREVIEW_PREVIOUS, 2).
-define(wxPREVIEW_PRINT, 1).
% From "prntbase.h": wxPrinterError
-define(wxPRINTER_NO_ERROR, 0).
-define(wxPRINTER_CANCELLED, 1).
-define(wxPRINTER_ERROR, 2).
% From "progdlg.h"
-define(wxPD_CAN_SKIP, 128).
-define(wxPD_REMAINING_TIME, 64).
-define(wxPD_SMOOTH, 32).
-define(wxPD_ESTIMATED_TIME, 16).
-define(wxPD_ELAPSED_TIME, 8).
-define(wxPD_AUTO_HIDE, 4).
-define(wxPD_APP_MODAL, 2).
-define(wxPD_CAN_ABORT, 1).
% From "region.h": wxRegionContain
-define(wxOutRegion, 0).
-define(wxPartRegion, 1).
-define(wxInRegion, 2).
% From "region.h": wxRegionOp
-define(wxRGN_AND, 0).
-define(wxRGN_COPY, 1).
-define(wxRGN_DIFF, 2).
-define(wxRGN_OR, 3).
-define(wxRGN_XOR, 4).
% From "scrolwin.h"
-define(wxScrolledWindowStyle, (?wxHSCROLL bor ?wxVSCROLL)).
% From "settings.h": wxSystemColour
-define(wxSYS_COLOUR_SCROLLBAR, 0).
-define(wxSYS_COLOUR_BACKGROUND, 1).
-define(wxSYS_COLOUR_DESKTOP, ?wxSYS_COLOUR_BACKGROUND).
-define(wxSYS_COLOUR_ACTIVECAPTION, (?wxSYS_COLOUR_BACKGROUND+1)).
-define(wxSYS_COLOUR_INACTIVECAPTION, (?wxSYS_COLOUR_BACKGROUND+2)).
-define(wxSYS_COLOUR_MENU, (?wxSYS_COLOUR_BACKGROUND+3)).
-define(wxSYS_COLOUR_WINDOW, (?wxSYS_COLOUR_BACKGROUND+4)).
-define(wxSYS_COLOUR_WINDOWFRAME, (?wxSYS_COLOUR_BACKGROUND+5)).
-define(wxSYS_COLOUR_MENUTEXT, (?wxSYS_COLOUR_BACKGROUND+6)).
-define(wxSYS_COLOUR_WINDOWTEXT, (?wxSYS_COLOUR_BACKGROUND+7)).
-define(wxSYS_COLOUR_CAPTIONTEXT, (?wxSYS_COLOUR_BACKGROUND+8)).
-define(wxSYS_COLOUR_ACTIVEBORDER, (?wxSYS_COLOUR_BACKGROUND+9)).
-define(wxSYS_COLOUR_INACTIVEBORDER, (?wxSYS_COLOUR_BACKGROUND+10)).
-define(wxSYS_COLOUR_APPWORKSPACE, (?wxSYS_COLOUR_BACKGROUND+11)).
-define(wxSYS_COLOUR_HIGHLIGHT, (?wxSYS_COLOUR_BACKGROUND+12)).
-define(wxSYS_COLOUR_HIGHLIGHTTEXT, (?wxSYS_COLOUR_BACKGROUND+13)).
-define(wxSYS_COLOUR_BTNFACE, (?wxSYS_COLOUR_BACKGROUND+14)).
-define(wxSYS_COLOUR_3DFACE, ?wxSYS_COLOUR_BTNFACE).
-define(wxSYS_COLOUR_BTNSHADOW, (?wxSYS_COLOUR_BTNFACE+1)).
-define(wxSYS_COLOUR_3DSHADOW, ?wxSYS_COLOUR_BTNSHADOW).
-define(wxSYS_COLOUR_GRAYTEXT, (?wxSYS_COLOUR_BTNSHADOW+1)).
-define(wxSYS_COLOUR_BTNTEXT, (?wxSYS_COLOUR_BTNSHADOW+2)).
-define(wxSYS_COLOUR_INACTIVECAPTIONTEXT, (?wxSYS_COLOUR_BTNSHADOW+3)).
-define(wxSYS_COLOUR_BTNHIGHLIGHT, (?wxSYS_COLOUR_BTNSHADOW+4)).
-define(wxSYS_COLOUR_BTNHILIGHT, ?wxSYS_COLOUR_BTNHIGHLIGHT).
-define(wxSYS_COLOUR_3DHIGHLIGHT, ?wxSYS_COLOUR_BTNHIGHLIGHT).
-define(wxSYS_COLOUR_3DHILIGHT, ?wxSYS_COLOUR_BTNHIGHLIGHT).
-define(wxSYS_COLOUR_3DDKSHADOW, (?wxSYS_COLOUR_BTNHIGHLIGHT+1)).
-define(wxSYS_COLOUR_3DLIGHT, (?wxSYS_COLOUR_BTNHIGHLIGHT+2)).
-define(wxSYS_COLOUR_INFOTEXT, (?wxSYS_COLOUR_BTNHIGHLIGHT+3)).
-define(wxSYS_COLOUR_INFOBK, (?wxSYS_COLOUR_BTNHIGHLIGHT+4)).
-define(wxSYS_COLOUR_LISTBOX, (?wxSYS_COLOUR_BTNHIGHLIGHT+5)).
-define(wxSYS_COLOUR_HOTLIGHT, (?wxSYS_COLOUR_BTNHIGHLIGHT+6)).
-define(wxSYS_COLOUR_GRADIENTACTIVECAPTION, (?wxSYS_COLOUR_BTNHIGHLIGHT+7)).
-define(wxSYS_COLOUR_GRADIENTINACTIVECAPTION, (?wxSYS_COLOUR_BTNHIGHLIGHT+8)).
-define(wxSYS_COLOUR_MENUHILIGHT, (?wxSYS_COLOUR_BTNHIGHLIGHT+9)).
-define(wxSYS_COLOUR_MENUBAR, (?wxSYS_COLOUR_BTNHIGHLIGHT+10)).
-define(wxSYS_COLOUR_LISTBOXTEXT, (?wxSYS_COLOUR_BTNHIGHLIGHT+11)).
-define(wxSYS_COLOUR_LISTBOXHIGHLIGHTTEXT, (?wxSYS_COLOUR_BTNHIGHLIGHT+12)).
-define(wxSYS_COLOUR_MAX, (?wxSYS_COLOUR_BTNHIGHLIGHT+13)).
% From "settings.h": wxSystemFeature
-define(wxSYS_CAN_DRAW_FRAME_DECORATIONS, 1).
-define(wxSYS_CAN_ICONIZE_FRAME, 2).
-define(wxSYS_TABLET_PRESENT, 3).
% From "settings.h": wxSystemFont
-define(wxSYS_OEM_FIXED_FONT, 10).
-define(wxSYS_ANSI_FIXED_FONT, 11).
-define(wxSYS_ANSI_VAR_FONT, 12).
-define(wxSYS_SYSTEM_FONT, 13).
-define(wxSYS_DEVICE_DEFAULT_FONT, 14).
-define(wxSYS_DEFAULT_PALETTE, 15).
-define(wxSYS_SYSTEM_FIXED_FONT, 16).
-define(wxSYS_DEFAULT_GUI_FONT, 17).
-define(wxSYS_ICONTITLE_FONT, ?wxSYS_DEFAULT_GUI_FONT).
% From "settings.h": wxSystemMetric
-define(wxSYS_MOUSE_BUTTONS, 1).
-define(wxSYS_BORDER_X, 2).
-define(wxSYS_BORDER_Y, 3).
-define(wxSYS_CURSOR_X, 4).
-define(wxSYS_CURSOR_Y, 5).
-define(wxSYS_DCLICK_X, 6).
-define(wxSYS_DCLICK_Y, 7).
-define(wxSYS_DRAG_X, 8).
-define(wxSYS_DRAG_Y, 9).
-define(wxSYS_EDGE_X, 10).
-define(wxSYS_EDGE_Y, 11).
-define(wxSYS_HSCROLL_ARROW_X, 12).
-define(wxSYS_HSCROLL_ARROW_Y, 13).
-define(wxSYS_HTHUMB_X, 14).
-define(wxSYS_ICON_X, 15).
-define(wxSYS_ICON_Y, 16).
-define(wxSYS_ICONSPACING_X, 17).
-define(wxSYS_ICONSPACING_Y, 18).
-define(wxSYS_WINDOWMIN_X, 19).
-define(wxSYS_WINDOWMIN_Y, 20).
-define(wxSYS_SCREEN_X, 21).
-define(wxSYS_SCREEN_Y, 22).
-define(wxSYS_FRAMESIZE_X, 23).
-define(wxSYS_FRAMESIZE_Y, 24).
-define(wxSYS_SMALLICON_X, 25).
-define(wxSYS_SMALLICON_Y, 26).
-define(wxSYS_HSCROLL_Y, 27).
-define(wxSYS_VSCROLL_X, 28).
-define(wxSYS_VSCROLL_ARROW_X, 29).
-define(wxSYS_VSCROLL_ARROW_Y, 30).
-define(wxSYS_VTHUMB_Y, 31).
-define(wxSYS_CAPTION_Y, 32).
-define(wxSYS_MENU_Y, 33).
-define(wxSYS_NETWORK_PRESENT, 34).
-define(wxSYS_PENWINDOWS_PRESENT, 35).
-define(wxSYS_SHOW_SOUNDS, 36).
-define(wxSYS_SWAP_BUTTONS, 37).
% From "settings.h": wxSystemScreenType
-define(wxSYS_SCREEN_NONE, 0).
-define(wxSYS_SCREEN_TINY, 1).
-define(wxSYS_SCREEN_PDA, 2).
-define(wxSYS_SCREEN_SMALL, 3).
-define(wxSYS_SCREEN_DESKTOP, 4).
% From "sizer.h": wxFlexSizerGrowMode
-define(wxFLEX_GROWMODE_NONE, 0).
-define(wxFLEX_GROWMODE_SPECIFIED, 1).
-define(wxFLEX_GROWMODE_ALL, 2).
% From "slider.h"
-define(wxSL_INVERSE, 4096).
-define(wxSL_SELRANGE, 2048).
-define(wxSL_BOTH, 1024).
-define(wxSL_BOTTOM, 512).
-define(wxSL_RIGHT, 256).
-define(wxSL_TOP, 128).
-define(wxSL_LEFT, 64).
-define(wxSL_LABELS, wxe_util:get_const(wxSL_LABELS)).
-define(wxSL_AUTOTICKS, ?wxSL_TICKS).
-define(wxSL_TICKS, 16).
-define(wxSL_VERTICAL, ?wxVERTICAL).
-define(wxSL_HORIZONTAL, ?wxHORIZONTAL).
% From "splitter.h"
-define(wxSP_3D, (?wxSP_3DBORDER bor ?wxSP_3DSASH)).
-define(wxSP_BORDER, ?wxSP_3DBORDER).
-define(wxSP_NO_XP_THEME, 1024).
-define(wxSP_3DBORDER, 512).
-define(wxSP_3DSASH, 256).
-define(wxSP_LIVE_UPDATE, 128).
-define(wxSP_PERMIT_UNSPLIT, 64).
-define(wxSP_NOSASH, 16).
-define(wxSP_NOBORDER, 0).
% From "statusbr.h"
-define(wxSB_RAISED, 2).
-define(wxSB_FLAT, 1).
-define(wxSB_NORMAL, 0).
% From "stc.h"
-define(wxSTC_CMD_WORDRIGHTENDEXTEND, 2442).
-define(wxSTC_CMD_WORDRIGHTEND, 2441).
-define(wxSTC_CMD_WORDLEFTENDEXTEND, 2440).
-define(wxSTC_CMD_WORDLEFTEND, 2439).
-define(wxSTC_CMD_STUTTEREDPAGEDOWNEXTEND, 2438).
-define(wxSTC_CMD_STUTTEREDPAGEDOWN, 2437).
-define(wxSTC_CMD_STUTTEREDPAGEUPEXTEND, 2436).
-define(wxSTC_CMD_STUTTEREDPAGEUP, 2435).
-define(wxSTC_CMD_PAGEDOWNRECTEXTEND, 2434).
-define(wxSTC_CMD_PAGEUPRECTEXTEND, 2433).
-define(wxSTC_CMD_LINEENDRECTEXTEND, 2432).
-define(wxSTC_CMD_VCHOMERECTEXTEND, 2431).
-define(wxSTC_CMD_HOMERECTEXTEND, 2430).
-define(wxSTC_CMD_CHARRIGHTRECTEXTEND, 2429).
-define(wxSTC_CMD_CHARLEFTRECTEXTEND, 2428).
-define(wxSTC_CMD_LINEUPRECTEXTEND, 2427).
-define(wxSTC_CMD_LINEDOWNRECTEXTEND, 2426).
-define(wxSTC_CMD_PARAUPEXTEND, 2416).
-define(wxSTC_CMD_PARAUP, 2415).
-define(wxSTC_CMD_PARADOWNEXTEND, 2414).
-define(wxSTC_CMD_PARADOWN, 2413).
-define(wxSTC_CMD_DELLINERIGHT, 2396).
-define(wxSTC_CMD_DELLINELEFT, 2395).
-define(wxSTC_CMD_WORDPARTRIGHTEXTEND, 2393).
-define(wxSTC_CMD_WORDPARTRIGHT, 2392).
-define(wxSTC_CMD_WORDPARTLEFTEXTEND, 2391).
-define(wxSTC_CMD_WORDPARTLEFT, 2390).
-define(wxSTC_CMD_LINECOPY, 2455).
-define(wxSTC_CMD_VCHOMEWRAPEXTEND, 2454).
-define(wxSTC_CMD_VCHOMEWRAP, 2453).
-define(wxSTC_CMD_LINEENDWRAPEXTEND, 2452).
-define(wxSTC_CMD_LINEENDWRAP, 2451).
-define(wxSTC_CMD_HOMEWRAPEXTEND, 2450).
-define(wxSTC_CMD_HOMEWRAP, 2349).
-define(wxSTC_CMD_LINEENDDISPLAYEXTEND, 2348).
-define(wxSTC_CMD_LINEENDDISPLAY, 2347).
-define(wxSTC_CMD_HOMEDISPLAYEXTEND, 2346).
-define(wxSTC_CMD_HOMEDISPLAY, 2345).
-define(wxSTC_CMD_DELETEBACKNOTLINE, 2344).
-define(wxSTC_CMD_LINESCROLLUP, 2343).
-define(wxSTC_CMD_LINESCROLLDOWN, 2342).
-define(wxSTC_CMD_UPPERCASE, 2341).
-define(wxSTC_CMD_LOWERCASE, 2340).
-define(wxSTC_CMD_LINEDUPLICATE, 2404).
-define(wxSTC_CMD_LINETRANSPOSE, 2339).
-define(wxSTC_CMD_LINEDELETE, 2338).
-define(wxSTC_CMD_LINECUT, 2337).
-define(wxSTC_CMD_DELWORDRIGHT, 2336).
-define(wxSTC_CMD_DELWORDLEFT, 2335).
-define(wxSTC_CMD_ZOOMOUT, 2334).
-define(wxSTC_CMD_ZOOMIN, 2333).
-define(wxSTC_CMD_VCHOMEEXTEND, 2332).
-define(wxSTC_CMD_VCHOME, 2331).
-define(wxSTC_CMD_FORMFEED, 2330).
-define(wxSTC_CMD_NEWLINE, 2329).
-define(wxSTC_CMD_BACKTAB, 2328).
-define(wxSTC_CMD_TAB, 2327).
-define(wxSTC_CMD_DELETEBACK, 2326).
-define(wxSTC_CMD_CANCEL, 2325).
-define(wxSTC_CMD_EDITTOGGLEOVERTYPE, 2324).
-define(wxSTC_CMD_PAGEDOWNEXTEND, 2323).
-define(wxSTC_CMD_PAGEDOWN, 2322).
-define(wxSTC_CMD_PAGEUPEXTEND, 2321).
-define(wxSTC_CMD_PAGEUP, 2320).
-define(wxSTC_CMD_DOCUMENTENDEXTEND, 2319).
-define(wxSTC_CMD_DOCUMENTEND, 2318).
-define(wxSTC_CMD_DOCUMENTSTARTEXTEND, 2317).
-define(wxSTC_CMD_DOCUMENTSTART, 2316).
-define(wxSTC_CMD_LINEENDEXTEND, 2315).
-define(wxSTC_CMD_LINEEND, 2314).
-define(wxSTC_CMD_HOMEEXTEND, 2313).
-define(wxSTC_CMD_HOME, 2312).
-define(wxSTC_CMD_WORDRIGHTEXTEND, 2311).
-define(wxSTC_CMD_WORDRIGHT, 2310).
-define(wxSTC_CMD_WORDLEFTEXTEND, 2309).
-define(wxSTC_CMD_WORDLEFT, 2308).
-define(wxSTC_CMD_CHARRIGHTEXTEND, 2307).
-define(wxSTC_CMD_CHARRIGHT, 2306).
-define(wxSTC_CMD_CHARLEFTEXTEND, 2305).
-define(wxSTC_CMD_CHARLEFT, 2304).
-define(wxSTC_CMD_LINEUPEXTEND, 2303).
-define(wxSTC_CMD_LINEUP, 2302).
-define(wxSTC_CMD_LINEDOWNEXTEND, 2301).
-define(wxSTC_CMD_LINEDOWN, 2300).
-define(wxSTC_CMD_CLEAR, 2180).
-define(wxSTC_CMD_PASTE, 2179).
-define(wxSTC_CMD_COPY, 2178).
-define(wxSTC_CMD_CUT, 2177).
-define(wxSTC_CMD_UNDO, 2176).
-define(wxSTC_CMD_SELECTALL, 2013).
-define(wxSTC_CMD_REDO, 2011).
-define(wxSTC_SPICE_COMMENTLINE, 8).
-define(wxSTC_SPICE_VALUE, 7).
-define(wxSTC_SPICE_DELIMITER, 6).
-define(wxSTC_SPICE_NUMBER, 5).
-define(wxSTC_SPICE_KEYWORD3, 4).
-define(wxSTC_SPICE_KEYWORD2, 3).
-define(wxSTC_SPICE_KEYWORD, 2).
-define(wxSTC_SPICE_IDENTIFIER, 1).
-define(wxSTC_SPICE_DEFAULT, 0).
-define(wxSTC_OPAL_DEFAULT, 32).
-define(wxSTC_OPAL_BOOL_CONST, 8).
-define(wxSTC_OPAL_PAR, 7).
-define(wxSTC_OPAL_STRING, 6).
-define(wxSTC_OPAL_SORT, 5).
-define(wxSTC_OPAL_KEYWORD, 4).
-define(wxSTC_OPAL_INTEGER, 3).
-define(wxSTC_OPAL_COMMENT_LINE, 2).
-define(wxSTC_OPAL_COMMENT_BLOCK, 1).
-define(wxSTC_OPAL_SPACE, 0).
-define(wxSTC_INNO_IDENTIFIER, 12).
-define(wxSTC_INNO_STRING_SINGLE, 11).
-define(wxSTC_INNO_STRING_DOUBLE, 10).
-define(wxSTC_INNO_KEYWORD_USER, 9).
-define(wxSTC_INNO_KEYWORD_PASCAL, 8).
-define(wxSTC_INNO_COMMENT_PASCAL, 7).
-define(wxSTC_INNO_PREPROC_INLINE, 6).
-define(wxSTC_INNO_PREPROC, 5).
-define(wxSTC_INNO_SECTION, 4).
-define(wxSTC_INNO_PARAMETER, 3).
-define(wxSTC_INNO_KEYWORD, 2).
-define(wxSTC_INNO_COMMENT, 1).
-define(wxSTC_INNO_DEFAULT, 0).
-define(wxSTC_CSOUND_STRINGEOL, 15).
-define(wxSTC_CSOUND_GLOBAL_VAR, 14).
-define(wxSTC_CSOUND_IRATE_VAR, 13).
-define(wxSTC_CSOUND_KRATE_VAR, 12).
-define(wxSTC_CSOUND_ARATE_VAR, 11).
-define(wxSTC_CSOUND_PARAM, 10).
-define(wxSTC_CSOUND_COMMENTBLOCK, 9).
-define(wxSTC_CSOUND_USERKEYWORD, 8).
-define(wxSTC_CSOUND_HEADERSTMT, 7).
-define(wxSTC_CSOUND_OPCODE, 6).
-define(wxSTC_CSOUND_IDENTIFIER, 5).
-define(wxSTC_CSOUND_INSTR, 4).
-define(wxSTC_CSOUND_OPERATOR, 3).
-define(wxSTC_CSOUND_NUMBER, 2).
-define(wxSTC_CSOUND_COMMENT, 1).
-define(wxSTC_CSOUND_DEFAULT, 0).
-define(wxSTC_FS_BINNUMBER, 23).
-define(wxSTC_FS_HEXNUMBER, 22).
-define(wxSTC_FS_ERROR, 21).
-define(wxSTC_FS_LABEL, 20).
-define(wxSTC_FS_ASM, 19).
-define(wxSTC_FS_CONSTANT, 18).
-define(wxSTC_FS_STRINGEOL, 17).
-define(wxSTC_FS_DATE, 16).
-define(wxSTC_FS_IDENTIFIER, 15).
-define(wxSTC_FS_OPERATOR, 14).
-define(wxSTC_FS_PREPROCESSOR, 13).
-define(wxSTC_FS_STRING, 12).
-define(wxSTC_FS_NUMBER, 11).
-define(wxSTC_FS_KEYWORD4, 10).
-define(wxSTC_FS_KEYWORD3, 9).
-define(wxSTC_FS_KEYWORD2, 8).
-define(wxSTC_FS_KEYWORD, 7).
-define(wxSTC_FS_COMMENTDOCKEYWORDERROR, 6).
-define(wxSTC_FS_COMMENTDOCKEYWORD, 5).
-define(wxSTC_FS_COMMENTLINEDOC, 4).
-define(wxSTC_FS_COMMENTDOC, 3).
-define(wxSTC_FS_COMMENTLINE, 2).
-define(wxSTC_FS_COMMENT, 1).
-define(wxSTC_FS_DEFAULT, 0).
-define(wxSTC_ST_SPEC_SEL, 16).
-define(wxSTC_ST_CHARACTER, 15).
-define(wxSTC_ST_ASSIGN, 14).
-define(wxSTC_ST_KWSEND, 13).
-define(wxSTC_ST_SPECIAL, 12).
-define(wxSTC_ST_RETURN, 11).
-define(wxSTC_ST_GLOBAL, 10).
-define(wxSTC_ST_NIL, 9).
-define(wxSTC_ST_SUPER, 8).
-define(wxSTC_ST_SELF, 7).
-define(wxSTC_ST_BOOL, 6).
-define(wxSTC_ST_BINARY, 5).
-define(wxSTC_ST_SYMBOL, 4).
-define(wxSTC_ST_COMMENT, 3).
-define(wxSTC_ST_NUMBER, 2).
-define(wxSTC_ST_STRING, 1).
-define(wxSTC_ST_DEFAULT, 0).
-define(wxSTC_SQL_QUOTEDIDENTIFIER, 23).
-define(wxSTC_SQL_USER4, 22).
-define(wxSTC_SQL_USER3, 21).
-define(wxSTC_SQL_USER2, 20).
-define(wxSTC_SQL_USER1, 19).
-define(wxSTC_SQL_COMMENTDOCKEYWORDERROR, 18).
-define(wxSTC_SQL_COMMENTDOCKEYWORD, 17).
-define(wxSTC_SQL_WORD2, 16).
-define(wxSTC_SQL_COMMENTLINEDOC, 15).
-define(wxSTC_SQL_SQLPLUS_COMMENT, 13).
-define(wxSTC_SQL_IDENTIFIER, 11).
-define(wxSTC_SQL_OPERATOR, 10).
-define(wxSTC_SQL_SQLPLUS_PROMPT, 9).
-define(wxSTC_SQL_SQLPLUS, 8).
-define(wxSTC_SQL_CHARACTER, 7).
-define(wxSTC_SQL_STRING, 6).
-define(wxSTC_SQL_WORD, 5).
-define(wxSTC_SQL_NUMBER, 4).
-define(wxSTC_SQL_COMMENTDOC, 3).
-define(wxSTC_SQL_COMMENTLINE, 2).
-define(wxSTC_SQL_COMMENT, 1).
-define(wxSTC_SQL_DEFAULT, 0).
-define(wxSTC_REBOL_WORD8, 28).
-define(wxSTC_REBOL_WORD7, 27).
-define(wxSTC_REBOL_WORD6, 26).
-define(wxSTC_REBOL_WORD5, 25).
-define(wxSTC_REBOL_WORD4, 24).
-define(wxSTC_REBOL_WORD3, 23).
-define(wxSTC_REBOL_WORD2, 22).
-define(wxSTC_REBOL_WORD, 21).
-define(wxSTC_REBOL_IDENTIFIER, 20).
-define(wxSTC_REBOL_TIME, 19).
-define(wxSTC_REBOL_DATE, 18).
-define(wxSTC_REBOL_URL, 17).
-define(wxSTC_REBOL_EMAIL, 16).
-define(wxSTC_REBOL_FILE, 15).
-define(wxSTC_REBOL_TAG, 14).
-define(wxSTC_REBOL_ISSUE, 13).
-define(wxSTC_REBOL_MONEY, 12).
-define(wxSTC_REBOL_BINARY, 11).
-define(wxSTC_REBOL_TUPLE, 10).
-define(wxSTC_REBOL_PAIR, 9).
-define(wxSTC_REBOL_NUMBER, 8).
-define(wxSTC_REBOL_BRACEDSTRING, 7).
-define(wxSTC_REBOL_QUOTEDSTRING, 6).
-define(wxSTC_REBOL_CHARACTER, 5).
-define(wxSTC_REBOL_OPERATOR, 4).
-define(wxSTC_REBOL_PREFACE, 3).
-define(wxSTC_REBOL_COMMENTBLOCK, 2).
-define(wxSTC_REBOL_COMMENTLINE, 1).
-define(wxSTC_REBOL_DEFAULT, 0).
-define(wxSTC_T3_USER3, 19).
-define(wxSTC_T3_USER2, 18).
-define(wxSTC_T3_USER1, 17).
-define(wxSTC_T3_HTML_STRING, 16).
-define(wxSTC_T3_HTML_DEFAULT, 15).
-define(wxSTC_T3_HTML_TAG, 14).
-define(wxSTC_T3_MSG_PARAM, 13).
-define(wxSTC_T3_LIB_DIRECTIVE, 12).
-define(wxSTC_T3_X_STRING, 11).
-define(wxSTC_T3_D_STRING, 10).
-define(wxSTC_T3_S_STRING, 9).
-define(wxSTC_T3_IDENTIFIER, 8).
-define(wxSTC_T3_NUMBER, 7).
-define(wxSTC_T3_KEYWORD, 6).
-define(wxSTC_T3_OPERATOR, 5).
-define(wxSTC_T3_LINE_COMMENT, 4).
-define(wxSTC_T3_BLOCK_COMMENT, 3).
-define(wxSTC_T3_PREPROCESSOR, 2).
-define(wxSTC_T3_X_DEFAULT, 1).
-define(wxSTC_T3_DEFAULT, 0).
-define(wxSTC_HA_COMMENTBLOCK3, 16).
-define(wxSTC_HA_COMMENTBLOCK2, 15).
-define(wxSTC_HA_COMMENTBLOCK, 14).
-define(wxSTC_HA_COMMENTLINE, 13).
-define(wxSTC_HA_INSTANCE, 12).
-define(wxSTC_HA_OPERATOR, 11).
-define(wxSTC_HA_IMPORT, 10).
-define(wxSTC_HA_DATA, 9).
-define(wxSTC_HA_CAPITAL, 8).
-define(wxSTC_HA_MODULE, 7).
-define(wxSTC_HA_CLASS, 6).
-define(wxSTC_HA_CHARACTER, 5).
-define(wxSTC_HA_STRING, 4).
-define(wxSTC_HA_NUMBER, 3).
-define(wxSTC_HA_KEYWORD, 2).
-define(wxSTC_HA_IDENTIFIER, 1).
-define(wxSTC_HA_DEFAULT, 0).
-define(wxSTC_CAML_COMMENT3, 15).
-define(wxSTC_CAML_COMMENT2, 14).
-define(wxSTC_CAML_COMMENT1, 13).
-define(wxSTC_CAML_COMMENT, 12).
-define(wxSTC_CAML_STRING, 11).
-define(wxSTC_CAML_CHAR, 9).
-define(wxSTC_CAML_NUMBER, 8).
-define(wxSTC_CAML_OPERATOR, 7).
-define(wxSTC_CAML_LINENUM, 6).
-define(wxSTC_CAML_KEYWORD3, 5).
-define(wxSTC_CAML_KEYWORD2, 4).
-define(wxSTC_CAML_KEYWORD, 3).
-define(wxSTC_CAML_TAGNAME, 2).
-define(wxSTC_CAML_IDENTIFIER, 1).
-define(wxSTC_CAML_DEFAULT, 0).
-define(wxSTC_VHDL_USERWORD, 14).
-define(wxSTC_VHDL_STDTYPE, 13).
-define(wxSTC_VHDL_STDPACKAGE, 12).
-define(wxSTC_VHDL_STDFUNCTION, 11).
-define(wxSTC_VHDL_ATTRIBUTE, 10).
-define(wxSTC_VHDL_STDOPERATOR, 9).
-define(wxSTC_VHDL_KEYWORD, 8).
-define(wxSTC_VHDL_STRINGEOL, 7).
-define(wxSTC_VHDL_IDENTIFIER, 6).
-define(wxSTC_VHDL_OPERATOR, 5).
-define(wxSTC_VHDL_STRING, 4).
-define(wxSTC_VHDL_NUMBER, 3).
-define(wxSTC_VHDL_COMMENTLINEBANG, 2).
-define(wxSTC_VHDL_COMMENT, 1).
-define(wxSTC_VHDL_DEFAULT, 0).
-define(wxSTC_ASN1_OPERATOR, 10).
-define(wxSTC_ASN1_TYPE, 9).
-define(wxSTC_ASN1_DESCRIPTOR, 8).
-define(wxSTC_ASN1_ATTRIBUTE, 7).
-define(wxSTC_ASN1_KEYWORD, 6).
-define(wxSTC_ASN1_SCALAR, 5).
-define(wxSTC_ASN1_OID, 4).
-define(wxSTC_ASN1_STRING, 3).
-define(wxSTC_ASN1_IDENTIFIER, 2).
-define(wxSTC_ASN1_COMMENT, 1).
-define(wxSTC_ASN1_DEFAULT, 0).
-define(wxSTC_SH_HERE_Q, 13).
-define(wxSTC_SH_HERE_DELIM, 12).
-define(wxSTC_SH_BACKTICKS, 11).
-define(wxSTC_SH_PARAM, 10).
-define(wxSTC_SH_SCALAR, 9).
-define(wxSTC_SH_IDENTIFIER, 8).
-define(wxSTC_SH_OPERATOR, 7).
-define(wxSTC_SH_CHARACTER, 6).
-define(wxSTC_SH_STRING, 5).
-define(wxSTC_SH_WORD, 4).
-define(wxSTC_SH_NUMBER, 3).
-define(wxSTC_SH_COMMENTLINE, 2).
-define(wxSTC_SH_ERROR, 1).
-define(wxSTC_SH_DEFAULT, 0).
-define(wxSTC_APDL_FUNCTION, 12).
-define(wxSTC_APDL_ARGUMENT, 11).
-define(wxSTC_APDL_STARCOMMAND, 10).
-define(wxSTC_APDL_SLASHCOMMAND, 9).
-define(wxSTC_APDL_COMMAND, 8).
-define(wxSTC_APDL_PROCESSOR, 7).
-define(wxSTC_APDL_WORD, 6).
-define(wxSTC_APDL_OPERATOR, 5).
-define(wxSTC_APDL_STRING, 4).
-define(wxSTC_APDL_NUMBER, 3).
-define(wxSTC_APDL_COMMENTBLOCK, 2).
-define(wxSTC_APDL_COMMENT, 1).
-define(wxSTC_APDL_DEFAULT, 0).
-define(wxSTC_AU3_UDF, 15).
-define(wxSTC_AU3_COMOBJ, 14).
-define(wxSTC_AU3_EXPAND, 13).
-define(wxSTC_AU3_SPECIAL, 12).
-define(wxSTC_AU3_PREPROCESSOR, 11).
-define(wxSTC_AU3_SENT, 10).
-define(wxSTC_AU3_VARIABLE, 9).
-define(wxSTC_AU3_OPERATOR, 8).
-define(wxSTC_AU3_STRING, 7).
-define(wxSTC_AU3_MACRO, 6).
-define(wxSTC_AU3_KEYWORD, 5).
-define(wxSTC_AU3_FUNCTION, 4).
-define(wxSTC_AU3_NUMBER, 3).
-define(wxSTC_AU3_COMMENTBLOCK, 2).
-define(wxSTC_AU3_COMMENT, 1).
-define(wxSTC_AU3_DEFAULT, 0).
-define(wxSTC_SN_USER, 19).
-define(wxSTC_SN_SIGNAL, 14).
-define(wxSTC_SN_REGEXTAG, 13).
-define(wxSTC_SN_STRINGEOL, 12).
-define(wxSTC_SN_IDENTIFIER, 11).
-define(wxSTC_SN_OPERATOR, 10).
-define(wxSTC_SN_PREPROCESSOR, 9).
-define(wxSTC_SN_WORD3, 8).
-define(wxSTC_SN_WORD2, 7).
-define(wxSTC_SN_STRING, 6).
-define(wxSTC_SN_WORD, 5).
-define(wxSTC_SN_NUMBER, 4).
-define(wxSTC_SN_COMMENTLINEBANG, 3).
-define(wxSTC_SN_COMMENTLINE, 2).
-define(wxSTC_SN_CODE, 1).
-define(wxSTC_SN_DEFAULT, 0).
-define(wxSTC_GC_OPERATOR, 9).
-define(wxSTC_GC_STRING, 8).
-define(wxSTC_GC_COMMAND, 7).
-define(wxSTC_GC_CONTROL, 6).
-define(wxSTC_GC_ATTRIBUTE, 5).
-define(wxSTC_GC_EVENT, 4).
-define(wxSTC_GC_GLOBAL, 3).
-define(wxSTC_GC_COMMENTBLOCK, 2).
-define(wxSTC_GC_COMMENTLINE, 1).
-define(wxSTC_GC_DEFAULT, 0).
-define(wxSTC_KIX_IDENTIFIER, 31).
-define(wxSTC_KIX_OPERATOR, 9).
-define(wxSTC_KIX_FUNCTIONS, 8).
-define(wxSTC_KIX_KEYWORD, 7).
-define(wxSTC_KIX_MACRO, 6).
-define(wxSTC_KIX_VAR, 5).
-define(wxSTC_KIX_NUMBER, 4).
-define(wxSTC_KIX_STRING2, 3).
-define(wxSTC_KIX_STRING1, 2).
-define(wxSTC_KIX_COMMENT, 1).
-define(wxSTC_KIX_DEFAULT, 0).
-define(wxSTC_V_USER, 19).
-define(wxSTC_V_STRINGEOL, 12).
-define(wxSTC_V_IDENTIFIER, 11).
-define(wxSTC_V_OPERATOR, 10).
-define(wxSTC_V_PREPROCESSOR, 9).
-define(wxSTC_V_WORD3, 8).
-define(wxSTC_V_WORD2, 7).
-define(wxSTC_V_STRING, 6).
-define(wxSTC_V_WORD, 5).
-define(wxSTC_V_NUMBER, 4).
-define(wxSTC_V_COMMENTLINEBANG, 3).
-define(wxSTC_V_COMMENTLINE, 2).
-define(wxSTC_V_COMMENT, 1).
-define(wxSTC_V_DEFAULT, 0).
-define(wxSTC_MSSQL_COLUMN_NAME_2, 16).
-define(wxSTC_MSSQL_DEFAULT_PREF_DATATYPE, 15).
-define(wxSTC_MSSQL_STORED_PROCEDURE, 14).
-define(wxSTC_MSSQL_FUNCTION, 13).
-define(wxSTC_MSSQL_GLOBAL_VARIABLE, 12).
-define(wxSTC_MSSQL_SYSTABLE, 11).
-define(wxSTC_MSSQL_DATATYPE, 10).
-define(wxSTC_MSSQL_STATEMENT, 9).
-define(wxSTC_MSSQL_COLUMN_NAME, 8).
-define(wxSTC_MSSQL_VARIABLE, 7).
-define(wxSTC_MSSQL_IDENTIFIER, 6).
-define(wxSTC_MSSQL_OPERATOR, 5).
-define(wxSTC_MSSQL_STRING, 4).
-define(wxSTC_MSSQL_NUMBER, 3).
-define(wxSTC_MSSQL_LINE_COMMENT, 2).
-define(wxSTC_MSSQL_COMMENT, 1).
-define(wxSTC_MSSQL_DEFAULT, 0).
-define(wxSTC_ERLANG_UNKNOWN, 31).
-define(wxSTC_ERLANG_NODE_NAME, 13).
-define(wxSTC_ERLANG_SEPARATOR, 12).
-define(wxSTC_ERLANG_RECORD, 11).
-define(wxSTC_ERLANG_MACRO, 10).
-define(wxSTC_ERLANG_CHARACTER, 9).
-define(wxSTC_ERLANG_FUNCTION_NAME, 8).
-define(wxSTC_ERLANG_ATOM, 7).
-define(wxSTC_ERLANG_OPERATOR, 6).
-define(wxSTC_ERLANG_STRING, 5).
-define(wxSTC_ERLANG_KEYWORD, 4).
-define(wxSTC_ERLANG_NUMBER, 3).
-define(wxSTC_ERLANG_VARIABLE, 2).
-define(wxSTC_ERLANG_COMMENT, 1).
-define(wxSTC_ERLANG_DEFAULT, 0).
-define(wxSTC_METAPOST_EXTRA, 6).
-define(wxSTC_METAPOST_TEXT, 5).
-define(wxSTC_METAPOST_COMMAND, 4).
-define(wxSTC_METAPOST_SYMBOL, 3).
-define(wxSTC_METAPOST_GROUP, 2).
-define(wxSTC_METAPOST_SPECIAL, 1).
-define(wxSTC_METAPOST_DEFAULT, 0).
-define(wxSTC_TEX_TEXT, 5).
-define(wxSTC_TEX_COMMAND, 4).
-define(wxSTC_TEX_SYMBOL, 3).
-define(wxSTC_TEX_GROUP, 2).
-define(wxSTC_TEX_SPECIAL, 1).
-define(wxSTC_TEX_DEFAULT, 0).
-define(wxSTC_YAML_ERROR, 8).
-define(wxSTC_YAML_TEXT, 7).
-define(wxSTC_YAML_DOCUMENT, 6).
-define(wxSTC_YAML_REFERENCE, 5).
-define(wxSTC_YAML_NUMBER, 4).
-define(wxSTC_YAML_KEYWORD, 3).
-define(wxSTC_YAML_IDENTIFIER, 2).
-define(wxSTC_YAML_COMMENT, 1).
-define(wxSTC_YAML_DEFAULT, 0).
-define(wxSTC_LOT_ABORT, 6).
-define(wxSTC_LOT_FAIL, 5).
-define(wxSTC_LOT_PASS, 4).
-define(wxSTC_LOT_SET, 3).
-define(wxSTC_LOT_BREAK, 2).
-define(wxSTC_LOT_HEADER, 1).
-define(wxSTC_LOT_DEFAULT, 0).
-define(wxSTC_CLW_DEPRECATED, 16).
-define(wxSTC_CLW_ERROR, 15).
-define(wxSTC_CLW_STANDARD_EQUATE, 14).
-define(wxSTC_CLW_ATTRIBUTE, 13).
-define(wxSTC_CLW_STRUCTURE_DATA_TYPE, 12).
-define(wxSTC_CLW_BUILTIN_PROCEDURES_FUNCTION, 11).
-define(wxSTC_CLW_RUNTIME_EXPRESSIONS, 10).
-define(wxSTC_CLW_COMPILER_DIRECTIVE, 9).
-define(wxSTC_CLW_KEYWORD, 8).
-define(wxSTC_CLW_PICTURE_STRING, 7).
-define(wxSTC_CLW_REAL_CONSTANT, 6).
-define(wxSTC_CLW_INTEGER_CONSTANT, 5).
-define(wxSTC_CLW_USER_IDENTIFIER, 4).
-define(wxSTC_CLW_STRING, 3).
-define(wxSTC_CLW_COMMENT, 2).
-define(wxSTC_CLW_LABEL, 1).
-define(wxSTC_CLW_DEFAULT, 0).
-define(wxSTC_MMIXAL_INCLUDE, 17).
-define(wxSTC_MMIXAL_SYMBOL, 16).
-define(wxSTC_MMIXAL_OPERATOR, 15).
-define(wxSTC_MMIXAL_HEX, 14).
-define(wxSTC_MMIXAL_REGISTER, 13).
-define(wxSTC_MMIXAL_STRING, 12).
-define(wxSTC_MMIXAL_CHAR, 11).
-define(wxSTC_MMIXAL_REF, 10).
-define(wxSTC_MMIXAL_NUMBER, 9).
-define(wxSTC_MMIXAL_OPERANDS, 8).
-define(wxSTC_MMIXAL_OPCODE_POST, 7).
-define(wxSTC_MMIXAL_OPCODE_UNKNOWN, 6).
-define(wxSTC_MMIXAL_OPCODE_VALID, 5).
-define(wxSTC_MMIXAL_OPCODE_PRE, 4).
-define(wxSTC_MMIXAL_OPCODE, 3).
-define(wxSTC_MMIXAL_LABEL, 2).
-define(wxSTC_MMIXAL_COMMENT, 1).
-define(wxSTC_MMIXAL_LEADWS, 0).
-define(wxSTC_NSIS_COMMENTBOX, 18).
-define(wxSTC_NSIS_FUNCTIONDEF, 17).
-define(wxSTC_NSIS_PAGEEX, 16).
-define(wxSTC_NSIS_SECTIONGROUP, 15).
-define(wxSTC_NSIS_NUMBER, 14).
-define(wxSTC_NSIS_STRINGVAR, 13).
-define(wxSTC_NSIS_MACRODEF, 12).
-define(wxSTC_NSIS_IFDEFINEDEF, 11).
-define(wxSTC_NSIS_SUBSECTIONDEF, 10).
-define(wxSTC_NSIS_SECTIONDEF, 9).
-define(wxSTC_NSIS_USERDEFINED, 8).
-define(wxSTC_NSIS_LABEL, 7).
-define(wxSTC_NSIS_VARIABLE, 6).
-define(wxSTC_NSIS_FUNCTION, 5).
-define(wxSTC_NSIS_STRINGRQ, 4).
-define(wxSTC_NSIS_STRINGLQ, 3).
-define(wxSTC_NSIS_STRINGDQ, 2).
-define(wxSTC_NSIS_COMMENT, 1).
-define(wxSTC_NSIS_DEFAULT, 0).
-define(wxSTC_PS_BADSTRINGCHAR, 15).
-define(wxSTC_PS_BASE85STRING, 14).
-define(wxSTC_PS_HEXSTRING, 13).
-define(wxSTC_PS_TEXT, 12).
-define(wxSTC_PS_PAREN_PROC, 11).
-define(wxSTC_PS_PAREN_DICT, 10).
-define(wxSTC_PS_PAREN_ARRAY, 9).
-define(wxSTC_PS_IMMEVAL, 8).
-define(wxSTC_PS_LITERAL, 7).
-define(wxSTC_PS_KEYWORD, 6).
-define(wxSTC_PS_NAME, 5).
-define(wxSTC_PS_NUMBER, 4).
-define(wxSTC_PS_DSC_VALUE, 3).
-define(wxSTC_PS_DSC_COMMENT, 2).
-define(wxSTC_PS_COMMENT, 1).
-define(wxSTC_PS_DEFAULT, 0).
-define(wxSTC_ESCRIPT_WORD3, 11).
-define(wxSTC_ESCRIPT_WORD2, 10).
-define(wxSTC_ESCRIPT_BRACE, 9).
-define(wxSTC_ESCRIPT_IDENTIFIER, 8).
-define(wxSTC_ESCRIPT_OPERATOR, 7).
-define(wxSTC_ESCRIPT_STRING, 6).
-define(wxSTC_ESCRIPT_WORD, 5).
-define(wxSTC_ESCRIPT_NUMBER, 4).
-define(wxSTC_ESCRIPT_COMMENTDOC, 3).
-define(wxSTC_ESCRIPT_COMMENTLINE, 2).
-define(wxSTC_ESCRIPT_COMMENT, 1).
-define(wxSTC_ESCRIPT_DEFAULT, 0).
-define(wxSTC_LOUT_STRINGEOL, 10).
-define(wxSTC_LOUT_IDENTIFIER, 9).
-define(wxSTC_LOUT_OPERATOR, 8).
-define(wxSTC_LOUT_STRING, 7).
-define(wxSTC_LOUT_WORD4, 6).
-define(wxSTC_LOUT_WORD3, 5).
-define(wxSTC_LOUT_WORD2, 4).
-define(wxSTC_LOUT_WORD, 3).
-define(wxSTC_LOUT_NUMBER, 2).
-define(wxSTC_LOUT_COMMENT, 1).
-define(wxSTC_LOUT_DEFAULT, 0).
-define(wxSTC_POV_WORD8, 16).
-define(wxSTC_POV_WORD7, 15).
-define(wxSTC_POV_WORD6, 14).
-define(wxSTC_POV_WORD5, 13).
-define(wxSTC_POV_WORD4, 12).
-define(wxSTC_POV_WORD3, 11).
-define(wxSTC_POV_WORD2, 10).
-define(wxSTC_POV_BADDIRECTIVE, 9).
-define(wxSTC_POV_DIRECTIVE, 8).
-define(wxSTC_POV_STRINGEOL, 7).
-define(wxSTC_POV_STRING, 6).
-define(wxSTC_POV_IDENTIFIER, 5).
-define(wxSTC_POV_OPERATOR, 4).
-define(wxSTC_POV_NUMBER, 3).
-define(wxSTC_POV_COMMENTLINE, 2).
-define(wxSTC_POV_COMMENT, 1).
-define(wxSTC_POV_DEFAULT, 0).
-define(wxSTC_CSS_ATTRIBUTE, 16).
-define(wxSTC_CSS_IDENTIFIER2, 15).
-define(wxSTC_CSS_SINGLESTRING, 14).
-define(wxSTC_CSS_DOUBLESTRING, 13).
-define(wxSTC_CSS_DIRECTIVE, 12).
-define(wxSTC_CSS_IMPORTANT, 11).
-define(wxSTC_CSS_ID, 10).
-define(wxSTC_CSS_COMMENT, 9).
-define(wxSTC_CSS_VALUE, 8).
-define(wxSTC_CSS_UNKNOWN_IDENTIFIER, 7).
-define(wxSTC_CSS_IDENTIFIER, 6).
-define(wxSTC_CSS_OPERATOR, 5).
-define(wxSTC_CSS_UNKNOWN_PSEUDOCLASS, 4).
-define(wxSTC_CSS_PSEUDOCLASS, 3).
-define(wxSTC_CSS_CLASS, 2).
-define(wxSTC_CSS_TAG, 1).
-define(wxSTC_CSS_DEFAULT, 0).
-define(wxSTC_F_CONTINUATION, 14).
-define(wxSTC_F_LABEL, 13).
-define(wxSTC_F_OPERATOR2, 12).
-define(wxSTC_F_PREPROCESSOR, 11).
-define(wxSTC_F_WORD3, 10).
-define(wxSTC_F_WORD2, 9).
-define(wxSTC_F_WORD, 8).
-define(wxSTC_F_IDENTIFIER, 7).
-define(wxSTC_F_OPERATOR, 6).
-define(wxSTC_F_STRINGEOL, 5).
-define(wxSTC_F_STRING2, 4).
-define(wxSTC_F_STRING1, 3).
-define(wxSTC_F_NUMBER, 2).
-define(wxSTC_F_COMMENT, 1).
-define(wxSTC_F_DEFAULT, 0).
-define(wxSTC_ASM_EXTINSTRUCTION, 14).
-define(wxSTC_ASM_STRINGEOL, 13).
-define(wxSTC_ASM_CHARACTER, 12).
-define(wxSTC_ASM_COMMENTBLOCK, 11).
-define(wxSTC_ASM_DIRECTIVEOPERAND, 10).
-define(wxSTC_ASM_DIRECTIVE, 9).
-define(wxSTC_ASM_REGISTER, 8).
-define(wxSTC_ASM_MATHINSTRUCTION, 7).
-define(wxSTC_ASM_CPUINSTRUCTION, 6).
-define(wxSTC_ASM_IDENTIFIER, 5).
-define(wxSTC_ASM_OPERATOR, 4).
-define(wxSTC_ASM_STRING, 3).
-define(wxSTC_ASM_NUMBER, 2).
-define(wxSTC_ASM_COMMENT, 1).
-define(wxSTC_ASM_DEFAULT, 0).
-define(wxSTC_SCRIPTOL_PREPROCESSOR, 15).
-define(wxSTC_SCRIPTOL_CLASSNAME, 14).
-define(wxSTC_SCRIPTOL_TRIPLE, 13).
-define(wxSTC_SCRIPTOL_IDENTIFIER, 12).
-define(wxSTC_SCRIPTOL_OPERATOR, 11).
-define(wxSTC_SCRIPTOL_KEYWORD, 10).
-define(wxSTC_SCRIPTOL_STRINGEOL, 9).
-define(wxSTC_SCRIPTOL_CHARACTER, 8).
-define(wxSTC_SCRIPTOL_STRING, 7).
-define(wxSTC_SCRIPTOL_NUMBER, 6).
-define(wxSTC_SCRIPTOL_COMMENTBLOCK, 5).
-define(wxSTC_SCRIPTOL_CSTYLE, 4).
-define(wxSTC_SCRIPTOL_PERSISTENT, 3).
-define(wxSTC_SCRIPTOL_COMMENTLINE, 2).
-define(wxSTC_SCRIPTOL_WHITE, 1).
-define(wxSTC_SCRIPTOL_DEFAULT, 0).
-define(wxSTC_MATLAB_DOUBLEQUOTESTRING, 8).
-define(wxSTC_MATLAB_IDENTIFIER, 7).
-define(wxSTC_MATLAB_OPERATOR, 6).
-define(wxSTC_MATLAB_STRING, 5).
-define(wxSTC_MATLAB_KEYWORD, 4).
-define(wxSTC_MATLAB_NUMBER, 3).
-define(wxSTC_MATLAB_COMMAND, 2).
-define(wxSTC_MATLAB_COMMENT, 1).
-define(wxSTC_MATLAB_DEFAULT, 0).
-define(wxSTC_FORTH_LOCALE, 11).
-define(wxSTC_FORTH_STRING, 10).
-define(wxSTC_FORTH_NUMBER, 9).
-define(wxSTC_FORTH_PREWORD2, 8).
-define(wxSTC_FORTH_PREWORD1, 7).
-define(wxSTC_FORTH_DEFWORD, 6).
-define(wxSTC_FORTH_KEYWORD, 5).
-define(wxSTC_FORTH_CONTROL, 4).
-define(wxSTC_FORTH_IDENTIFIER, 3).
-define(wxSTC_FORTH_COMMENT_ML, 2).
-define(wxSTC_FORTH_COMMENT, 1).
-define(wxSTC_FORTH_DEFAULT, 0).
-define(wxSTC_NNCRONTAB_IDENTIFIER, 10).
-define(wxSTC_NNCRONTAB_ENVIRONMENT, 9).
-define(wxSTC_NNCRONTAB_STRING, 8).
-define(wxSTC_NNCRONTAB_NUMBER, 7).
-define(wxSTC_NNCRONTAB_ASTERISK, 6).
-define(wxSTC_NNCRONTAB_MODIFIER, 5).
-define(wxSTC_NNCRONTAB_KEYWORD, 4).
-define(wxSTC_NNCRONTAB_SECTION, 3).
-define(wxSTC_NNCRONTAB_TASK, 2).
-define(wxSTC_NNCRONTAB_COMMENT, 1).
-define(wxSTC_NNCRONTAB_DEFAULT, 0).
-define(wxSTC_EIFFEL_STRINGEOL, 8).
-define(wxSTC_EIFFEL_IDENTIFIER, 7).
-define(wxSTC_EIFFEL_OPERATOR, 6).
-define(wxSTC_EIFFEL_CHARACTER, 5).
-define(wxSTC_EIFFEL_STRING, 4).
-define(wxSTC_EIFFEL_WORD, 3).
-define(wxSTC_EIFFEL_NUMBER, 2).
-define(wxSTC_EIFFEL_COMMENTLINE, 1).
-define(wxSTC_EIFFEL_DEFAULT, 0).
-define(wxSTC_LISP_MULTI_COMMENT, 12).
-define(wxSTC_LISP_SPECIAL, 11).
-define(wxSTC_LISP_OPERATOR, 10).
-define(wxSTC_LISP_IDENTIFIER, 9).
-define(wxSTC_LISP_STRINGEOL, 8).
-define(wxSTC_LISP_STRING, 6).
-define(wxSTC_LISP_SYMBOL, 5).
-define(wxSTC_LISP_KEYWORD_KW, 4).
-define(wxSTC_LISP_KEYWORD, 3).
-define(wxSTC_LISP_NUMBER, 2).
-define(wxSTC_LISP_COMMENT, 1).
-define(wxSTC_LISP_DEFAULT, 0).
-define(wxSTC_BAAN_WORD2, 10).
-define(wxSTC_BAAN_STRINGEOL, 9).
-define(wxSTC_BAAN_IDENTIFIER, 8).
-define(wxSTC_BAAN_OPERATOR, 7).
-define(wxSTC_BAAN_PREPROCESSOR, 6).
-define(wxSTC_BAAN_STRING, 5).
-define(wxSTC_BAAN_WORD, 4).
-define(wxSTC_BAAN_NUMBER, 3).
-define(wxSTC_BAAN_COMMENTDOC, 2).
-define(wxSTC_BAAN_COMMENT, 1).
-define(wxSTC_BAAN_DEFAULT, 0).
-define(wxSTC_ADA_ILLEGAL, 11).
-define(wxSTC_ADA_COMMENTLINE, 10).
-define(wxSTC_ADA_LABEL, 9).
-define(wxSTC_ADA_STRINGEOL, 8).
-define(wxSTC_ADA_STRING, 7).
-define(wxSTC_ADA_CHARACTEREOL, 6).
-define(wxSTC_ADA_CHARACTER, 5).
-define(wxSTC_ADA_DELIMITER, 4).
-define(wxSTC_ADA_NUMBER, 3).
-define(wxSTC_ADA_IDENTIFIER, 2).
-define(wxSTC_ADA_WORD, 1).
-define(wxSTC_ADA_DEFAULT, 0).
-define(wxSTC_AVE_WORD6, 16).
-define(wxSTC_AVE_WORD5, 15).
-define(wxSTC_AVE_WORD4, 14).
-define(wxSTC_AVE_WORD3, 13).
-define(wxSTC_AVE_WORD2, 12).
-define(wxSTC_AVE_WORD1, 11).
-define(wxSTC_AVE_OPERATOR, 10).
-define(wxSTC_AVE_IDENTIFIER, 9).
-define(wxSTC_AVE_STRINGEOL, 8).
-define(wxSTC_AVE_ENUM, 7).
-define(wxSTC_AVE_STRING, 6).
-define(wxSTC_AVE_WORD, 3).
-define(wxSTC_AVE_NUMBER, 2).
-define(wxSTC_AVE_COMMENT, 1).
-define(wxSTC_AVE_DEFAULT, 0).
-define(wxSTC_CONF_DIRECTIVE, 9).
-define(wxSTC_CONF_IP, 8).
-define(wxSTC_CONF_OPERATOR, 7).
-define(wxSTC_CONF_STRING, 6).
-define(wxSTC_CONF_PARAMETER, 5).
-define(wxSTC_CONF_EXTENSION, 4).
-define(wxSTC_CONF_IDENTIFIER, 3).
-define(wxSTC_CONF_NUMBER, 2).
-define(wxSTC_CONF_COMMENT, 1).
-define(wxSTC_CONF_DEFAULT, 0).
-define(wxSTC_DIFF_ADDED, 6).
-define(wxSTC_DIFF_DELETED, 5).
-define(wxSTC_DIFF_POSITION, 4).
-define(wxSTC_DIFF_HEADER, 3).
-define(wxSTC_DIFF_COMMAND, 2).
-define(wxSTC_DIFF_COMMENT, 1).
-define(wxSTC_DIFF_DEFAULT, 0).
-define(wxSTC_MAKE_IDEOL, 9).
-define(wxSTC_MAKE_TARGET, 5).
-define(wxSTC_MAKE_OPERATOR, 4).
-define(wxSTC_MAKE_IDENTIFIER, 3).
-define(wxSTC_MAKE_PREPROCESSOR, 2).
-define(wxSTC_MAKE_COMMENT, 1).
-define(wxSTC_MAKE_DEFAULT, 0).
-define(wxSTC_BAT_OPERATOR, 7).
-define(wxSTC_BAT_IDENTIFIER, 6).
-define(wxSTC_BAT_COMMAND, 5).
-define(wxSTC_BAT_HIDE, 4).
-define(wxSTC_BAT_LABEL, 3).
-define(wxSTC_BAT_WORD, 2).
-define(wxSTC_BAT_COMMENT, 1).
-define(wxSTC_BAT_DEFAULT, 0).
-define(wxSTC_ERR_JAVA_STACK, 20).
-define(wxSTC_ERR_TIDY, 19).
-define(wxSTC_ERR_ABSF, 18).
-define(wxSTC_ERR_IFORT, 17).
-define(wxSTC_ERR_IFC, 16).
-define(wxSTC_ERR_ELF, 15).
-define(wxSTC_ERR_PHP, 14).
-define(wxSTC_ERR_DIFF_MESSAGE, 13).
-define(wxSTC_ERR_DIFF_DELETION, 12).
-define(wxSTC_ERR_DIFF_ADDITION, 11).
-define(wxSTC_ERR_DIFF_CHANGED, 10).
-define(wxSTC_ERR_CTAG, 9).
-define(wxSTC_ERR_LUA, 8).
-define(wxSTC_ERR_NET, 7).
-define(wxSTC_ERR_PERL, 6).
-define(wxSTC_ERR_BORLAND, 5).
-define(wxSTC_ERR_CMD, 4).
-define(wxSTC_ERR_MS, 3).
-define(wxSTC_ERR_GCC, 2).
-define(wxSTC_ERR_PYTHON, 1).
-define(wxSTC_ERR_DEFAULT, 0).
-define(wxSTC_LUA_WORD8, 19).
-define(wxSTC_LUA_WORD7, 18).
-define(wxSTC_LUA_WORD6, 17).
-define(wxSTC_LUA_WORD5, 16).
-define(wxSTC_LUA_WORD4, 15).
-define(wxSTC_LUA_WORD3, 14).
-define(wxSTC_LUA_WORD2, 13).
-define(wxSTC_LUA_STRINGEOL, 12).
-define(wxSTC_LUA_IDENTIFIER, 11).
-define(wxSTC_LUA_OPERATOR, 10).
-define(wxSTC_LUA_PREPROCESSOR, 9).
-define(wxSTC_LUA_LITERALSTRING, 8).
-define(wxSTC_LUA_CHARACTER, 7).
-define(wxSTC_LUA_STRING, 6).
-define(wxSTC_LUA_WORD, 5).
-define(wxSTC_LUA_NUMBER, 4).
-define(wxSTC_LUA_COMMENTDOC, 3).
-define(wxSTC_LUA_COMMENTLINE, 2).
-define(wxSTC_LUA_COMMENT, 1).
-define(wxSTC_LUA_DEFAULT, 0).
-define(wxSTC_L_COMMENT, 4).
-define(wxSTC_L_MATH, 3).
-define(wxSTC_L_TAG, 2).
-define(wxSTC_L_COMMAND, 1).
-define(wxSTC_L_DEFAULT, 0).
-define(wxSTC_PROPS_KEY, 5).
-define(wxSTC_PROPS_DEFVAL, 4).
-define(wxSTC_PROPS_ASSIGNMENT, 3).
-define(wxSTC_PROPS_SECTION, 2).
-define(wxSTC_PROPS_COMMENT, 1).
-define(wxSTC_PROPS_DEFAULT, 0).
-define(wxSTC_B_BINNUMBER, 18).
-define(wxSTC_B_HEXNUMBER, 17).
-define(wxSTC_B_ERROR, 16).
-define(wxSTC_B_LABEL, 15).
-define(wxSTC_B_ASM, 14).
-define(wxSTC_B_CONSTANT, 13).
-define(wxSTC_B_KEYWORD4, 12).
-define(wxSTC_B_KEYWORD3, 11).
-define(wxSTC_B_KEYWORD2, 10).
-define(wxSTC_B_STRINGEOL, 9).
-define(wxSTC_B_DATE, 8).
-define(wxSTC_B_IDENTIFIER, 7).
-define(wxSTC_B_OPERATOR, 6).
-define(wxSTC_B_PREPROCESSOR, 5).
-define(wxSTC_B_STRING, 4).
-define(wxSTC_B_KEYWORD, 3).
-define(wxSTC_B_NUMBER, 2).
-define(wxSTC_B_COMMENT, 1).
-define(wxSTC_B_DEFAULT, 0).
-define(wxSTC_RB_UPPER_BOUND, 41).
-define(wxSTC_RB_STDERR, 40).
-define(wxSTC_RB_STDOUT, 31).
-define(wxSTC_RB_STDIN, 30).
-define(wxSTC_RB_WORD_DEMOTED, 29).
-define(wxSTC_RB_STRING_QW, 28).
-define(wxSTC_RB_STRING_QR, 27).
-define(wxSTC_RB_STRING_QX, 26).
-define(wxSTC_RB_STRING_QQ, 25).
-define(wxSTC_RB_STRING_Q, 24).
-define(wxSTC_RB_HERE_QX, 23).
-define(wxSTC_RB_HERE_QQ, 22).
-define(wxSTC_RB_HERE_Q, 21).
-define(wxSTC_RB_HERE_DELIM, 20).
-define(wxSTC_RB_DATASECTION, 19).
-define(wxSTC_RB_BACKTICKS, 18).
-define(wxSTC_RB_CLASS_VAR, 17).
-define(wxSTC_RB_INSTANCE_VAR, 16).
-define(wxSTC_RB_MODULE_NAME, 15).
-define(wxSTC_RB_SYMBOL, 14).
-define(wxSTC_RB_GLOBAL, 13).
-define(wxSTC_RB_REGEX, 12).
-define(wxSTC_RB_IDENTIFIER, 11).
-define(wxSTC_RB_OPERATOR, 10).
-define(wxSTC_RB_DEFNAME, 9).
-define(wxSTC_RB_CLASSNAME, 8).
-define(wxSTC_RB_CHARACTER, 7).
-define(wxSTC_RB_STRING, 6).
-define(wxSTC_RB_WORD, 5).
-define(wxSTC_RB_NUMBER, 4).
-define(wxSTC_RB_POD, 3).
-define(wxSTC_RB_COMMENTLINE, 2).
-define(wxSTC_RB_ERROR, 1).
-define(wxSTC_RB_DEFAULT, 0).
-define(wxSTC_PL_POD_VERB, 31).
-define(wxSTC_PL_STRING_QW, 30).
-define(wxSTC_PL_STRING_QR, 29).
-define(wxSTC_PL_STRING_QX, 28).
-define(wxSTC_PL_STRING_QQ, 27).
-define(wxSTC_PL_STRING_Q, 26).
-define(wxSTC_PL_HERE_QX, 25).
-define(wxSTC_PL_HERE_QQ, 24).
-define(wxSTC_PL_HERE_Q, 23).
-define(wxSTC_PL_HERE_DELIM, 22).
-define(wxSTC_PL_DATASECTION, 21).
-define(wxSTC_PL_BACKTICKS, 20).
-define(wxSTC_PL_LONGQUOTE, 19).
-define(wxSTC_PL_REGSUBST, 18).
-define(wxSTC_PL_REGEX, 17).
-define(wxSTC_PL_VARIABLE_INDEXER, 16).
-define(wxSTC_PL_SYMBOLTABLE, 15).
-define(wxSTC_PL_HASH, 14).
-define(wxSTC_PL_ARRAY, 13).
-define(wxSTC_PL_SCALAR, 12).
-define(wxSTC_PL_IDENTIFIER, 11).
-define(wxSTC_PL_OPERATOR, 10).
-define(wxSTC_PL_PREPROCESSOR, 9).
-define(wxSTC_PL_PUNCTUATION, 8).
-define(wxSTC_PL_CHARACTER, 7).
-define(wxSTC_PL_STRING, 6).
-define(wxSTC_PL_WORD, 5).
-define(wxSTC_PL_NUMBER, 4).
-define(wxSTC_PL_POD, 3).
-define(wxSTC_PL_COMMENTLINE, 2).
-define(wxSTC_PL_ERROR, 1).
-define(wxSTC_PL_DEFAULT, 0).
-define(wxSTC_HPHP_OPERATOR, 127).
-define(wxSTC_HPHP_HSTRING_VARIABLE, 126).
-define(wxSTC_HPHP_COMMENTLINE, 125).
-define(wxSTC_HPHP_COMMENT, 124).
-define(wxSTC_HPHP_VARIABLE, 123).
-define(wxSTC_HPHP_NUMBER, 122).
-define(wxSTC_HPHP_WORD, 121).
-define(wxSTC_HPHP_SIMPLESTRING, 120).
-define(wxSTC_HPHP_HSTRING, 119).
-define(wxSTC_HPHP_DEFAULT, 118).
-define(wxSTC_HPA_IDENTIFIER, 117).
-define(wxSTC_HPA_OPERATOR, 116).
-define(wxSTC_HPA_DEFNAME, 115).
-define(wxSTC_HPA_CLASSNAME, 114).
-define(wxSTC_HPA_TRIPLEDOUBLE, 113).
-define(wxSTC_HPA_TRIPLE, 112).
-define(wxSTC_HPA_WORD, 111).
-define(wxSTC_HPA_CHARACTER, 110).
-define(wxSTC_HPA_STRING, 109).
-define(wxSTC_HPA_NUMBER, 108).
-define(wxSTC_HPA_COMMENTLINE, 107).
-define(wxSTC_HPA_DEFAULT, 106).
-define(wxSTC_HPA_START, 105).
-define(wxSTC_HPHP_COMPLEX_VARIABLE, 104).
-define(wxSTC_HP_IDENTIFIER, 102).
-define(wxSTC_HP_OPERATOR, 101).
-define(wxSTC_HP_DEFNAME, 100).
-define(wxSTC_HP_CLASSNAME, 99).
-define(wxSTC_HP_TRIPLEDOUBLE, 98).
-define(wxSTC_HP_TRIPLE, 97).
-define(wxSTC_HP_WORD, 96).
-define(wxSTC_HP_CHARACTER, 95).
-define(wxSTC_HP_STRING, 94).
-define(wxSTC_HP_NUMBER, 93).
-define(wxSTC_HP_COMMENTLINE, 92).
-define(wxSTC_HP_DEFAULT, 91).
-define(wxSTC_HP_START, 90).
-define(wxSTC_HBA_STRINGEOL, 87).
-define(wxSTC_HBA_IDENTIFIER, 86).
-define(wxSTC_HBA_STRING, 85).
-define(wxSTC_HBA_WORD, 84).
-define(wxSTC_HBA_NUMBER, 83).
-define(wxSTC_HBA_COMMENTLINE, 82).
-define(wxSTC_HBA_DEFAULT, 81).
-define(wxSTC_HBA_START, 80).
-define(wxSTC_HB_STRINGEOL, 77).
-define(wxSTC_HB_IDENTIFIER, 76).
-define(wxSTC_HB_STRING, 75).
-define(wxSTC_HB_WORD, 74).
-define(wxSTC_HB_NUMBER, 73).
-define(wxSTC_HB_COMMENTLINE, 72).
-define(wxSTC_HB_DEFAULT, 71).
-define(wxSTC_HB_START, 70).
-define(wxSTC_HJA_REGEX, 67).
-define(wxSTC_HJA_STRINGEOL, 66).
-define(wxSTC_HJA_SYMBOLS, 65).
-define(wxSTC_HJA_SINGLESTRING, 64).
-define(wxSTC_HJA_DOUBLESTRING, 63).
-define(wxSTC_HJA_KEYWORD, 62).
-define(wxSTC_HJA_WORD, 61).
-define(wxSTC_HJA_NUMBER, 60).
-define(wxSTC_HJA_COMMENTDOC, 59).
-define(wxSTC_HJA_COMMENTLINE, 58).
-define(wxSTC_HJA_COMMENT, 57).
-define(wxSTC_HJA_DEFAULT, 56).
-define(wxSTC_HJA_START, 55).
-define(wxSTC_HJ_REGEX, 52).
-define(wxSTC_HJ_STRINGEOL, 51).
-define(wxSTC_HJ_SYMBOLS, 50).
-define(wxSTC_HJ_SINGLESTRING, 49).
-define(wxSTC_HJ_DOUBLESTRING, 48).
-define(wxSTC_HJ_KEYWORD, 47).
-define(wxSTC_HJ_WORD, 46).
-define(wxSTC_HJ_NUMBER, 45).
-define(wxSTC_HJ_COMMENTDOC, 44).
-define(wxSTC_HJ_COMMENTLINE, 43).
-define(wxSTC_HJ_COMMENT, 42).
-define(wxSTC_HJ_DEFAULT, 41).
-define(wxSTC_HJ_START, 40).
-define(wxSTC_H_SGML_BLOCK_DEFAULT, 31).
-define(wxSTC_H_SGML_1ST_PARAM_COMMENT, 30).
-define(wxSTC_H_SGML_COMMENT, 29).
-define(wxSTC_H_SGML_ENTITY, 28).
-define(wxSTC_H_SGML_SPECIAL, 27).
-define(wxSTC_H_SGML_ERROR, 26).
-define(wxSTC_H_SGML_SIMPLESTRING, 25).
-define(wxSTC_H_SGML_DOUBLESTRING, 24).
-define(wxSTC_H_SGML_1ST_PARAM, 23).
-define(wxSTC_H_SGML_COMMAND, 22).
-define(wxSTC_H_SGML_DEFAULT, 21).
-define(wxSTC_H_XCCOMMENT, 20).
-define(wxSTC_H_VALUE, 19).
-define(wxSTC_H_QUESTION, 18).
-define(wxSTC_H_CDATA, 17).
-define(wxSTC_H_ASPAT, 16).
-define(wxSTC_H_ASP, 15).
-define(wxSTC_H_SCRIPT, 14).
-define(wxSTC_H_XMLEND, 13).
-define(wxSTC_H_XMLSTART, 12).
-define(wxSTC_H_TAGEND, 11).
-define(wxSTC_H_ENTITY, 10).
-define(wxSTC_H_COMMENT, 9).
-define(wxSTC_H_OTHER, 8).
-define(wxSTC_H_SINGLESTRING, 7).
-define(wxSTC_H_DOUBLESTRING, 6).
-define(wxSTC_H_NUMBER, 5).
-define(wxSTC_H_ATTRIBUTEUNKNOWN, 4).
-define(wxSTC_H_ATTRIBUTE, 3).
-define(wxSTC_H_TAGUNKNOWN, 2).
-define(wxSTC_H_TAG, 1).
-define(wxSTC_H_DEFAULT, 0).
-define(wxSTC_TCL_BLOCK_COMMENT, 21).
-define(wxSTC_TCL_COMMENT_BOX, 20).
-define(wxSTC_TCL_WORD8, 19).
-define(wxSTC_TCL_WORD7, 18).
-define(wxSTC_TCL_WORD6, 17).
-define(wxSTC_TCL_WORD5, 16).
-define(wxSTC_TCL_WORD4, 15).
-define(wxSTC_TCL_WORD3, 14).
-define(wxSTC_TCL_WORD2, 13).
-define(wxSTC_TCL_WORD, 12).
-define(wxSTC_TCL_EXPAND, 11).
-define(wxSTC_TCL_MODIFIER, 10).
-define(wxSTC_TCL_SUB_BRACE, 9).
-define(wxSTC_TCL_SUBSTITUTION, 8).
-define(wxSTC_TCL_IDENTIFIER, 7).
-define(wxSTC_TCL_OPERATOR, 6).
-define(wxSTC_TCL_IN_QUOTE, 5).
-define(wxSTC_TCL_WORD_IN_QUOTE, 4).
-define(wxSTC_TCL_NUMBER, 3).
-define(wxSTC_TCL_COMMENTLINE, 2).
-define(wxSTC_TCL_COMMENT, 1).
-define(wxSTC_TCL_DEFAULT, 0).
-define(wxSTC_C_GLOBALCLASS, 19).
-define(wxSTC_C_COMMENTDOCKEYWORDERROR, 18).
-define(wxSTC_C_COMMENTDOCKEYWORD, 17).
-define(wxSTC_C_WORD2, 16).
-define(wxSTC_C_COMMENTLINEDOC, 15).
-define(wxSTC_C_REGEX, 14).
-define(wxSTC_C_VERBATIM, 13).
-define(wxSTC_C_STRINGEOL, 12).
-define(wxSTC_C_IDENTIFIER, 11).
-define(wxSTC_C_OPERATOR, 10).
-define(wxSTC_C_PREPROCESSOR, 9).
-define(wxSTC_C_UUID, 8).
-define(wxSTC_C_CHARACTER, 7).
-define(wxSTC_C_STRING, 6).
-define(wxSTC_C_WORD, 5).
-define(wxSTC_C_NUMBER, 4).
-define(wxSTC_C_COMMENTDOC, 3).
-define(wxSTC_C_COMMENTLINE, 2).
-define(wxSTC_C_COMMENT, 1).
-define(wxSTC_C_DEFAULT, 0).
-define(wxSTC_P_DECORATOR, 15).
-define(wxSTC_P_WORD2, 14).
-define(wxSTC_P_STRINGEOL, 13).
-define(wxSTC_P_COMMENTBLOCK, 12).
-define(wxSTC_P_IDENTIFIER, 11).
-define(wxSTC_P_OPERATOR, 10).
-define(wxSTC_P_DEFNAME, 9).
-define(wxSTC_P_CLASSNAME, 8).
-define(wxSTC_P_TRIPLEDOUBLE, 7).
-define(wxSTC_P_TRIPLE, 6).
-define(wxSTC_P_WORD, 5).
-define(wxSTC_P_CHARACTER, 4).
-define(wxSTC_P_STRING, 3).
-define(wxSTC_P_NUMBER, 2).
-define(wxSTC_P_COMMENTLINE, 1).
-define(wxSTC_P_DEFAULT, 0).
-define(wxSTC_LEX_AUTOMATIC, 1000).
-define(wxSTC_LEX_SPICE, 78).
-define(wxSTC_LEX_OPAL, 77).
-define(wxSTC_LEX_INNOSETUP, 76).
-define(wxSTC_LEX_FREEBASIC, 75).
-define(wxSTC_LEX_CSOUND, 74).
-define(wxSTC_LEX_FLAGSHIP, 73).
-define(wxSTC_LEX_SMALLTALK, 72).
-define(wxSTC_LEX_REBOL, 71).
-define(wxSTC_LEX_TADS3, 70).
-define(wxSTC_LEX_PHPSCRIPT, 69).
-define(wxSTC_LEX_HASKELL, 68).
-define(wxSTC_LEX_PUREBASIC, 67).
-define(wxSTC_LEX_BLITZBASIC, 66).
-define(wxSTC_LEX_CAML, 65).
-define(wxSTC_LEX_VHDL, 64).
-define(wxSTC_LEX_ASN1, 63).
-define(wxSTC_LEX_BASH, 62).
-define(wxSTC_LEX_APDL, 61).
-define(wxSTC_LEX_AU3, 60).
-define(wxSTC_LEX_SPECMAN, 59).
-define(wxSTC_LEX_GUI4CLI, 58).
-define(wxSTC_LEX_KIX, 57).
-define(wxSTC_LEX_VERILOG, 56).
-define(wxSTC_LEX_MSSQL, 55).
-define(wxSTC_LEX_OCTAVE, 54).
-define(wxSTC_LEX_ERLANG, 53).
-define(wxSTC_LEX_FORTH, 52).
-define(wxSTC_LEX_POWERBASIC, 51).
-define(wxSTC_LEX_METAPOST, 50).
-define(wxSTC_LEX_TEX, 49).
-define(wxSTC_LEX_YAML, 48).
-define(wxSTC_LEX_LOT, 47).
-define(wxSTC_LEX_CLWNOCASE, 46).
-define(wxSTC_LEX_CLW, 45).
-define(wxSTC_LEX_MMIXAL, 44).
-define(wxSTC_LEX_NSIS, 43).
-define(wxSTC_LEX_PS, 42).
-define(wxSTC_LEX_ESCRIPT, 41).
-define(wxSTC_LEX_LOUT, 40).
-define(wxSTC_LEX_POV, 39).
-define(wxSTC_LEX_CSS, 38).
-define(wxSTC_LEX_F77, 37).
-define(wxSTC_LEX_FORTRAN, 36).
-define(wxSTC_LEX_CPPNOCASE, 35).
-define(wxSTC_LEX_ASM, 34).
-define(wxSTC_LEX_SCRIPTOL, 33).
-define(wxSTC_LEX_MATLAB, 32).
-define(wxSTC_LEX_BAAN, 31).
-define(wxSTC_LEX_VBSCRIPT, 28).
-define(wxSTC_LEX_BULLANT, 27).
-define(wxSTC_LEX_NNCRONTAB, 26).
-define(wxSTC_LEX_TCL, 25).
-define(wxSTC_LEX_EIFFELKW, 24).
-define(wxSTC_LEX_EIFFEL, 23).
-define(wxSTC_LEX_RUBY, 22).
-define(wxSTC_LEX_LISP, 21).
-define(wxSTC_LEX_ADA, 20).
-define(wxSTC_LEX_AVE, 19).
-define(wxSTC_LEX_PASCAL, 18).
-define(wxSTC_LEX_CONF, 17).
-define(wxSTC_LEX_DIFF, 16).
-define(wxSTC_LEX_LUA, 15).
-define(wxSTC_LEX_LATEX, 14).
-define(wxSTC_LEX_XCODE, 13).
-define(wxSTC_LEX_BATCH, 12).
-define(wxSTC_LEX_MAKEFILE, 11).
-define(wxSTC_LEX_ERRORLIST, 10).
-define(wxSTC_LEX_PROPERTIES, 9).
-define(wxSTC_LEX_VB, 8).
-define(wxSTC_LEX_SQL, 7).
-define(wxSTC_LEX_PERL, 6).
-define(wxSTC_LEX_XML, 5).
-define(wxSTC_LEX_HTML, 4).
-define(wxSTC_LEX_CPP, 3).
-define(wxSTC_LEX_PYTHON, 2).
-define(wxSTC_LEX_NULL, 1).
-define(wxSTC_LEX_CONTAINER, 0).
-define(wxSTC_SCMOD_ALT, 4).
-define(wxSTC_SCMOD_CTRL, 2).
-define(wxSTC_SCMOD_SHIFT, 1).
-define(wxSTC_SCMOD_NORM, 0).
-define(wxSTC_KEY_DIVIDE, 312).
-define(wxSTC_KEY_SUBTRACT, 311).
-define(wxSTC_KEY_ADD, 310).
-define(wxSTC_KEY_RETURN, 13).
-define(wxSTC_KEY_TAB, 9).
-define(wxSTC_KEY_BACK, 8).
-define(wxSTC_KEY_ESCAPE, 7).
-define(wxSTC_KEY_INSERT, 309).
-define(wxSTC_KEY_DELETE, 308).
-define(wxSTC_KEY_NEXT, 307).
-define(wxSTC_KEY_PRIOR, 306).
-define(wxSTC_KEY_END, 305).
-define(wxSTC_KEY_HOME, 304).
-define(wxSTC_KEY_RIGHT, 303).
-define(wxSTC_KEY_LEFT, 302).
-define(wxSTC_KEY_UP, 301).
-define(wxSTC_KEY_DOWN, 300).
-define(wxSTC_MODEVENTMASKALL, 8191).
-define(wxSTC_MULTILINEUNDOREDO, 4096).
-define(wxSTC_MOD_BEFOREDELETE, 2048).
-define(wxSTC_MOD_BEFOREINSERT, 1024).
-define(wxSTC_MOD_CHANGEMARKER, 512).
-define(wxSTC_LASTSTEPINUNDOREDO, 256).
-define(wxSTC_MULTISTEPUNDOREDO, 128).
-define(wxSTC_PERFORMED_REDO, 64).
-define(wxSTC_PERFORMED_UNDO, 32).
-define(wxSTC_PERFORMED_USER, 16).
-define(wxSTC_MOD_CHANGEFOLD, 8).
-define(wxSTC_MOD_CHANGESTYLE, 4).
-define(wxSTC_MOD_DELETETEXT, 2).
-define(wxSTC_MOD_INSERTTEXT, 1).
-define(wxSTC_KEYWORDSET_MAX, 8).
-define(wxSTC_ALPHA_NOALPHA, 256).
-define(wxSTC_ALPHA_OPAQUE, 255).
-define(wxSTC_ALPHA_TRANSPARENT, 0).
-define(wxSTC_SEL_LINES, 2).
-define(wxSTC_SEL_RECTANGLE, 1).
-define(wxSTC_SEL_STREAM, 0).
-define(wxSTC_CARET_EVEN, 8).
-define(wxSTC_CARET_JUMPS, 16).
-define(wxSTC_CARET_STRICT, 4).
-define(wxSTC_CARET_SLOP, 1).
-define(wxSTC_VISIBLE_STRICT, 4).
-define(wxSTC_VISIBLE_SLOP, 1).
-define(wxSTC_CURSORWAIT, 4).
-define(wxSTC_CURSORNORMAL, -1).
-define(wxSTC_EDGE_BACKGROUND, 2).
-define(wxSTC_EDGE_LINE, 1).
-define(wxSTC_EDGE_NONE, 0).
-define(wxSTC_CACHE_DOCUMENT, 3).
-define(wxSTC_CACHE_PAGE, 2).
-define(wxSTC_CACHE_CARET, 1).
-define(wxSTC_CACHE_NONE, 0).
-define(wxSTC_WRAPVISUALFLAGLOC_START_BY_TEXT, 2).
-define(wxSTC_WRAPVISUALFLAGLOC_END_BY_TEXT, 1).
-define(wxSTC_WRAPVISUALFLAGLOC_DEFAULT, 0).
-define(wxSTC_WRAPVISUALFLAG_START, 2).
-define(wxSTC_WRAPVISUALFLAG_END, 1).
-define(wxSTC_WRAPVISUALFLAG_NONE, 0).
-define(wxSTC_WRAP_CHAR, 2).
-define(wxSTC_WRAP_WORD, 1).
-define(wxSTC_WRAP_NONE, 0).
-define(wxSTC_TIME_FOREVER, 10000000).
-define(wxSTC_FOLDFLAG_BOX, 1).
-define(wxSTC_FOLDFLAG_LEVELNUMBERS, 64).
-define(wxSTC_FOLDFLAG_LINEAFTER_CONTRACTED, 16).
-define(wxSTC_FOLDFLAG_LINEAFTER_EXPANDED, 8).
-define(wxSTC_FOLDFLAG_LINEBEFORE_CONTRACTED, 4).
-define(wxSTC_FOLDFLAG_LINEBEFORE_EXPANDED, 2).
-define(wxSTC_FOLDLEVELNUMBERMASK, 4095).
-define(wxSTC_FOLDLEVELUNINDENT, 131072).
-define(wxSTC_FOLDLEVELCONTRACTED, 65536).
-define(wxSTC_FOLDLEVELBOXFOOTERFLAG, 32768).
-define(wxSTC_FOLDLEVELBOXHEADERFLAG, 16384).
-define(wxSTC_FOLDLEVELHEADERFLAG, 8192).
-define(wxSTC_FOLDLEVELWHITEFLAG, 4096).
-define(wxSTC_FOLDLEVELBASE, 1024).
-define(wxSTC_FIND_POSIX, 4194304).
-define(wxSTC_FIND_REGEXP, 2097152).
-define(wxSTC_FIND_WORDSTART, 1048576).
-define(wxSTC_FIND_MATCHCASE, 4).
-define(wxSTC_FIND_WHOLEWORD, 2).
-define(wxSTC_PRINT_COLOURONWHITEDEFAULTBG, 4).
-define(wxSTC_PRINT_COLOURONWHITE, 3).
-define(wxSTC_PRINT_BLACKONWHITE, 2).
-define(wxSTC_PRINT_INVERTLIGHT, 1).
-define(wxSTC_PRINT_NORMAL, 0).
-define(wxSTC_INDICS_MASK, 224).
-define(wxSTC_INDIC2_MASK, 128).
-define(wxSTC_INDIC1_MASK, 64).
-define(wxSTC_INDIC0_MASK, 32).
-define(wxSTC_INDIC_ROUNDBOX, 7).
-define(wxSTC_INDIC_BOX, 6).
-define(wxSTC_INDIC_HIDDEN, 5).
-define(wxSTC_INDIC_STRIKE, 4).
-define(wxSTC_INDIC_DIAGONAL, 3).
-define(wxSTC_INDIC_TT, 2).
-define(wxSTC_INDIC_SQUIGGLE, 1).
-define(wxSTC_INDIC_PLAIN, 0).
-define(wxSTC_INDIC_MAX, 7).
-define(wxSTC_CASE_LOWER, 2).
-define(wxSTC_CASE_UPPER, 1).
-define(wxSTC_CASE_MIXED, 0).
-define(wxSTC_CHARSET_8859_15, 1000).
-define(wxSTC_CHARSET_THAI, 222).
-define(wxSTC_CHARSET_VIETNAMESE, 163).
-define(wxSTC_CHARSET_ARABIC, 178).
-define(wxSTC_CHARSET_HEBREW, 177).
-define(wxSTC_CHARSET_JOHAB, 130).
-define(wxSTC_CHARSET_TURKISH, 162).
-define(wxSTC_CHARSET_SYMBOL, 2).
-define(wxSTC_CHARSET_SHIFTJIS, 128).
-define(wxSTC_CHARSET_CYRILLIC, 1251).
-define(wxSTC_CHARSET_RUSSIAN, 204).
-define(wxSTC_CHARSET_OEM, 255).
-define(wxSTC_CHARSET_MAC, 77).
-define(wxSTC_CHARSET_HANGUL, 129).
-define(wxSTC_CHARSET_GREEK, 161).
-define(wxSTC_CHARSET_GB2312, 134).
-define(wxSTC_CHARSET_EASTEUROPE, 238).
-define(wxSTC_CHARSET_CHINESEBIG5, 136).
-define(wxSTC_CHARSET_BALTIC, 186).
-define(wxSTC_CHARSET_DEFAULT, 1).
-define(wxSTC_CHARSET_ANSI, 0).
-define(wxSTC_STYLE_MAX, 127).
-define(wxSTC_STYLE_LASTPREDEFINED, 39).
-define(wxSTC_STYLE_CALLTIP, 38).
-define(wxSTC_STYLE_INDENTGUIDE, 37).
-define(wxSTC_STYLE_CONTROLCHAR, 36).
-define(wxSTC_STYLE_BRACEBAD, 35).
-define(wxSTC_STYLE_BRACELIGHT, 34).
-define(wxSTC_STYLE_LINENUMBER, 33).
-define(wxSTC_STYLE_DEFAULT, 32).
-define(wxSTC_MARGIN_FORE, 3).
-define(wxSTC_MARGIN_BACK, 2).
-define(wxSTC_MARGIN_NUMBER, 1).
-define(wxSTC_MARGIN_SYMBOL, 0).
-define(wxSTC_MASK_FOLDERS, 4261412864).
-define(wxSTC_MARKNUM_FOLDEROPEN, 31).
-define(wxSTC_MARKNUM_FOLDER, 30).
-define(wxSTC_MARKNUM_FOLDERSUB, 29).
-define(wxSTC_MARKNUM_FOLDERTAIL, 28).
-define(wxSTC_MARKNUM_FOLDERMIDTAIL, 27).
-define(wxSTC_MARKNUM_FOLDEROPENMID, 26).
-define(wxSTC_MARKNUM_FOLDEREND, 25).
-define(wxSTC_MARK_CHARACTER, 10000).
-define(wxSTC_MARK_FULLRECT, 26).
-define(wxSTC_MARK_PIXMAP, 25).
-define(wxSTC_MARK_ARROWS, 24).
-define(wxSTC_MARK_DOTDOTDOT, 23).
-define(wxSTC_MARK_BACKGROUND, 22).
-define(wxSTC_MARK_CIRCLEMINUSCONNECTED, 21).
-define(wxSTC_MARK_CIRCLEMINUS, 20).
-define(wxSTC_MARK_CIRCLEPLUSCONNECTED, 19).
-define(wxSTC_MARK_CIRCLEPLUS, 18).
-define(wxSTC_MARK_TCORNERCURVE, 17).
-define(wxSTC_MARK_LCORNERCURVE, 16).
-define(wxSTC_MARK_BOXMINUSCONNECTED, 15).
-define(wxSTC_MARK_BOXMINUS, 14).
-define(wxSTC_MARK_BOXPLUSCONNECTED, 13).
-define(wxSTC_MARK_BOXPLUS, 12).
-define(wxSTC_MARK_TCORNER, 11).
-define(wxSTC_MARK_LCORNER, 10).
-define(wxSTC_MARK_VLINE, 9).
-define(wxSTC_MARK_PLUS, 8).
-define(wxSTC_MARK_MINUS, 7).
-define(wxSTC_MARK_ARROWDOWN, 6).
-define(wxSTC_MARK_EMPTY, 5).
-define(wxSTC_MARK_SHORTARROW, 4).
-define(wxSTC_MARK_SMALLRECT, 3).
-define(wxSTC_MARK_ARROW, 2).
-define(wxSTC_MARK_ROUNDRECT, 1).
-define(wxSTC_MARK_CIRCLE, 0).
-define(wxSTC_MARKER_MAX, 31).
-define(wxSTC_CP_DBCS, 1).
-define(wxSTC_CP_UTF8, 65001).
-define(wxSTC_EOL_LF, 2).
-define(wxSTC_EOL_CR, 1).
-define(wxSTC_EOL_CRLF, 0).
-define(wxSTC_WS_VISIBLEAFTERINDENT, 2).
-define(wxSTC_WS_VISIBLEALWAYS, 1).
-define(wxSTC_WS_INVISIBLE, 0).
-define(wxSTC_LEXER_START, 4000).
-define(wxSTC_OPTIONAL_START, 3000).
-define(wxSTC_START, 2000).
-define(wxSTC_INVALID_POSITION, -1).
-define(wxSTC_USE_POPUP, 1).
% From "tbarbase.h": wxToolBarToolStyle
-define(wxTOOL_STYLE_BUTTON, 1).
-define(wxTOOL_STYLE_SEPARATOR, 2).
-define(wxTOOL_STYLE_CONTROL, 3).
% From "textctrl.h"
-define(wxTEXT_ATTR_TABS, 1024).
-define(wxTEXT_ATTR_RIGHT_INDENT, 512).
-define(wxTEXT_ATTR_LEFT_INDENT, 256).
-define(wxTEXT_ATTR_ALIGNMENT, 128).
-define(wxTEXT_ATTR_FONT, (?wxTEXT_ATTR_FONT_FACE bor ?wxTEXT_ATTR_FONT_SIZE bor ?wxTEXT_ATTR_FONT_WEIGHT bor ?wxTEXT_ATTR_FONT_ITALIC bor ?wxTEXT_ATTR_FONT_UNDERLINE)).
-define(wxTEXT_ATTR_FONT_UNDERLINE, 64).
-define(wxTEXT_ATTR_FONT_ITALIC, 32).
-define(wxTEXT_ATTR_FONT_WEIGHT, 16).
-define(wxTEXT_ATTR_FONT_SIZE, 8).
-define(wxTEXT_ATTR_FONT_FACE, 4).
-define(wxTEXT_ATTR_BACKGROUND_COLOUR, 2).
-define(wxTEXT_ATTR_TEXT_COLOUR, 1).
-define(wxTEXT_TYPE_ANY, 0).
-define(wxTE_CAPITALIZE, 0).
-define(wxTE_RICH2, 32768).
-define(wxTE_BESTWRAP, 0).
-define(wxTE_WORDWRAP, 1).
-define(wxTE_CHARWRAP, 16384).
-define(wxTE_DONTWRAP, ?wxHSCROLL).
-define(wxTE_NOHIDESEL, 8192).
-define(wxTE_AUTO_URL, 4096).
-define(wxTE_PASSWORD, 2048).
-define(wxTE_PROCESS_ENTER, 1024).
-define(wxTE_RICH, 128).
-define(wxTE_CENTRE, ?wxTE_CENTER).
-define(wxTE_RIGHT, ?wxALIGN_RIGHT).
-define(wxTE_CENTER, ?wxALIGN_CENTER_HORIZONTAL).
-define(wxTE_LEFT, 0).
-define(wxTE_PROCESS_TAB, 64).
-define(wxTE_MULTILINE, 32).
-define(wxTE_READONLY, 16).
-define(wxTE_AUTO_SCROLL, 8).
-define(wxTE_NO_VSCROLL, 2).
-define(wxHAS_TEXT_WINDOW_STREAM, 0).
% From "textctrl.h": wxTextAttrAlignment
-define(wxTEXT_ALIGNMENT_DEFAULT, 0).
-define(wxTEXT_ALIGNMENT_LEFT, 1).
-define(wxTEXT_ALIGNMENT_CENTRE, 2).
-define(wxTEXT_ALIGNMENT_CENTER, ?wxTEXT_ALIGNMENT_CENTRE).
-define(wxTEXT_ALIGNMENT_RIGHT, (?wxTEXT_ALIGNMENT_CENTRE+1)).
-define(wxTEXT_ALIGNMENT_JUSTIFIED, (?wxTEXT_ALIGNMENT_CENTRE+2)).
% From "textctrl.h": wxTextCtrlHitTestResult
-define(wxTE_HT_UNKNOWN, -2).
-define(wxTE_HT_BEFORE, -1).
-define(wxTE_HT_ON_TEXT, 0).
-define(wxTE_HT_BELOW, 1).
-define(wxTE_HT_BEYOND, 2).
% From "textdlgg.h"
-define(wxTextEntryDialogStyle, (?wxOK bor ?wxCANCEL bor ?wxCENTRE bor ?wxWS_EX_VALIDATE_RECURSIVELY)).
% From "toolbar.h"
-define(wxTB_HORIZONTAL, ?wxHORIZONTAL).
-define(wxTB_TOP, ?wxTB_HORIZONTAL).
-define(wxTB_VERTICAL, ?wxVERTICAL).
-define(wxTB_LEFT, ?wxTB_VERTICAL).
-define(wxTB_3DBUTTONS, 16).
-define(wxTB_FLAT, 32).
-define(wxTB_DOCKABLE, 64).
-define(wxTB_NOICONS, 128).
-define(wxTB_TEXT, 256).
-define(wxTB_NODIVIDER, 512).
-define(wxTB_NOALIGN, 1024).
-define(wxTB_HORZ_LAYOUT, 2048).
-define(wxTB_HORZ_TEXT, (?wxTB_HORZ_LAYOUT bor ?wxTB_TEXT)).
-define(wxTB_NO_TOOLTIPS, 4096).
-define(wxTB_BOTTOM, 8192).
-define(wxTB_RIGHT, 16384).
% From "toolbook.h"
-define(wxBK_BUTTONBAR, 256).
% From "toplevel.h"
-define(wxFULLSCREEN_NOMENUBAR, 1).
-define(wxFULLSCREEN_NOTOOLBAR, 2).
-define(wxFULLSCREEN_NOSTATUSBAR, 4).
-define(wxFULLSCREEN_NOBORDER, 8).
-define(wxFULLSCREEN_NOCAPTION, 16).
-define(wxFULLSCREEN_ALL, (?wxFULLSCREEN_NOMENUBAR bor ?wxFULLSCREEN_NOTOOLBAR bor ?wxFULLSCREEN_NOSTATUSBAR bor ?wxFULLSCREEN_NOBORDER bor ?wxFULLSCREEN_NOCAPTION)).
% From "toplevel.h"
-define(wxUSER_ATTENTION_INFO, 1).
-define(wxUSER_ATTENTION_ERROR, 2).
% From "toplevel.h"
-define(wxTOPLEVEL_EX_DIALOG, 8).
-define(wxDEFAULT_FRAME_STYLE, (?wxSYSTEM_MENU bor ?wxRESIZE_BORDER bor ?wxMINIMIZE_BOX bor ?wxMAXIMIZE_BOX bor ?wxCLOSE_BOX bor ?wxCAPTION bor ?wxCLIP_CHILDREN)).
-define(wxRESIZE_BORDER, 64).
-define(wxTINY_CAPTION_VERT, 128).
-define(wxTINY_CAPTION_HORIZ, 256).
-define(wxMAXIMIZE_BOX, 512).
-define(wxMINIMIZE_BOX, 1024).
-define(wxSYSTEM_MENU, 2048).
-define(wxCLOSE_BOX, 4096).
-define(wxMAXIMIZE, 8192).
-define(wxMINIMIZE, ?wxICONIZE).
-define(wxICONIZE, 16384).
-define(wxSTAY_ON_TOP, 32768).
% From "treebase.h"
-define(wxTR_DEFAULT_STYLE, wxe_util:get_const(wxTR_DEFAULT_STYLE)).
-define(wxTR_FULL_ROW_HIGHLIGHT, 8192).
-define(wxTR_HIDE_ROOT, 2048).
-define(wxTR_ROW_LINES, 1024).
-define(wxTR_EDIT_LABELS, 512).
-define(wxTR_HAS_VARIABLE_ROW_HEIGHT, 128).
-define(wxTR_EXTENDED, 64).
-define(wxTR_MULTIPLE, 32).
-define(wxTR_SINGLE, 0).
-define(wxTR_TWIST_BUTTONS, 16).
-define(wxTR_LINES_AT_ROOT, 8).
-define(wxTR_NO_LINES, 4).
-define(wxTR_HAS_BUTTONS, 1).
-define(wxTR_NO_BUTTONS, 0).
% From "treebase.h": wxTreeItemIcon
-define(wxTreeItemIcon_Normal, 0).
-define(wxTreeItemIcon_Selected, 1).
-define(wxTreeItemIcon_Expanded, 2).
-define(wxTreeItemIcon_SelectedExpanded, 3).
-define(wxTreeItemIcon_Max, 4).
% From "utils.h"
-define(wxEXEC_ASYNC, 0).
-define(wxEXEC_SYNC, 1).
-define(wxEXEC_NOHIDE, 2).
-define(wxEXEC_MAKE_GROUP_LEADER, 4).
-define(wxEXEC_NODISABLE, 8).
% From "utils.h"
-define(wxBROWSER_NEW_WINDOW, 1).
% From "utils.h"
-define(wxStrip_Mnemonics, 1).
-define(wxStrip_Accel, 2).
-define(wxStrip_All, (?wxStrip_Mnemonics bor ?wxStrip_Accel)).
% From "utils.h": wxKillError
-define(wxKILL_OK, 0).
-define(wxKILL_BAD_SIGNAL, 1).
-define(wxKILL_ACCESS_DENIED, 2).
-define(wxKILL_NO_PROCESS, 3).
-define(wxKILL_ERROR, 4).
% From "utils.h": wxKillFlags
-define(wxKILL_NOCHILDREN, 0).
-define(wxKILL_CHILDREN, 1).
% From "utils.h": wxShutdownFlags
-define(wxSHUTDOWN_POWEROFF, 0).
-define(wxSHUTDOWN_REBOOT, 1).
% From "utils.h": wxSignal
-define(wxSIGNONE, 0).
-define(wxSIGHUP, 1).
-define(wxSIGINT, 2).
-define(wxSIGQUIT, 3).
-define(wxSIGILL, 4).
-define(wxSIGTRAP, 5).
-define(wxSIGABRT, 6).
-define(wxSIGIOT, ?wxSIGABRT).
-define(wxSIGEMT, (?wxSIGABRT+1)).
-define(wxSIGFPE, (?wxSIGABRT+2)).
-define(wxSIGKILL, (?wxSIGABRT+3)).
-define(wxSIGBUS, (?wxSIGABRT+4)).
-define(wxSIGSEGV, (?wxSIGABRT+5)).
-define(wxSIGSYS, (?wxSIGABRT+6)).
-define(wxSIGPIPE, (?wxSIGABRT+7)).
-define(wxSIGALRM, (?wxSIGABRT+8)).
-define(wxSIGTERM, (?wxSIGABRT+9)).
% From "valtext.h"
-define(wxFILTER_EXCLUDE_CHAR_LIST, 128).
-define(wxFILTER_INCLUDE_CHAR_LIST, 64).
-define(wxFILTER_EXCLUDE_LIST, 32).
-define(wxFILTER_INCLUDE_LIST, 16).
-define(wxFILTER_NUMERIC, 8).
-define(wxFILTER_ALPHANUMERIC, 4).
-define(wxFILTER_ALPHA, 2).
-define(wxFILTER_ASCII, 1).
-define(wxFILTER_NONE, 0).
% From "version.h"
-define(wxBETA_NUMBER, wxe_util:get_const(wxBETA_NUMBER)).
-define(wxSUBRELEASE_NUMBER, wxe_util:get_const(wxSUBRELEASE_NUMBER)).
-define(wxRELEASE_NUMBER, wxe_util:get_const(wxRELEASE_NUMBER)).
-define(wxMINOR_VERSION, wxe_util:get_const(wxMINOR_VERSION)).
-define(wxMAJOR_VERSION, wxe_util:get_const(wxMAJOR_VERSION)).
% From "window.h"
% From "window.h": wxWindowVariant
-define(wxWINDOW_VARIANT_NORMAL, 0).
-define(wxWINDOW_VARIANT_SMALL, 1).
-define(wxWINDOW_VARIANT_MINI, 2).
-define(wxWINDOW_VARIANT_LARGE, 3).
-define(wxWINDOW_VARIANT_MAX, 4).
% From "xmlres.h": wxXmlResourceFlags
-define(wxXRC_USE_LOCALE, 1).
-define(wxXRC_NO_SUBCLASSING, 2).
-define(wxXRC_NO_RELOADING, 4).
