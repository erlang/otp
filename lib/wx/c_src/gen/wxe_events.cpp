/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2022. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
*/

/***** This file is generated do not edit ****/

#include <wx/wx.h>
#include "../wxe_impl.h"

#include "wxe_macros.h"
#include "../wxe_events.h"

#include "../wxe_return.h"

WX_DECLARE_HASH_MAP(int, wxeEtype*, wxIntegerHash, wxIntegerEqual, wxeETmap );

wxeETmap etmap;

int wxeEventTypeFromAtom(ERL_NIF_TERM etype_atom) {
  wxeETmap::iterator it;
  for(it = etmap.begin(); it != etmap.end(); ++it) {
    wxeEtype * value = it->second;
    if(enif_is_identical(value->evName, etype_atom)) {
      if(it->first > wxEVT_USER_FIRST) {
        return it->first - wxEVT_USER_FIRST;
      } else {
        return it->first;
      }
    }
  }
  return -1;
}

void initEventTable()
{
  wxe_evInfo event_types[] =
    {
     {wxEVT_NULL, 0, "null", "wxWXENullEvent", "wxWXENull"},
     {wxEVT_COMMAND_BUTTON_CLICKED, 168, "command_button_clicked", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_CHECKBOX_CLICKED, 168, "command_checkbox_clicked", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_CHOICE_SELECTED, 168, "command_choice_selected", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_LISTBOX_SELECTED, 168, "command_listbox_selected", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_LISTBOX_DOUBLECLICKED, 168, "command_listbox_doubleclicked", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_TEXT_UPDATED, 168, "command_text_updated", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_TEXT_ENTER, 168, "command_text_enter", "wxCommandEvent", "wxCommand"},
     {wxEVT_TEXT_MAXLEN, 168, "text_maxlen", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_MENU_SELECTED, 168, "command_menu_selected", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_SLIDER_UPDATED, 168, "command_slider_updated", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_RADIOBOX_SELECTED, 168, "command_radiobox_selected", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_RADIOBUTTON_SELECTED, 168, "command_radiobutton_selected", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_SCROLLBAR_UPDATED, 168, "command_scrollbar_updated", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_VLBOX_SELECTED, 168, "command_vlbox_selected", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_COMBOBOX_SELECTED, 168, "command_combobox_selected", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMBOBOX_DROPDOWN, 168, "combobox_dropdown", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMBOBOX_CLOSEUP, 168, "combobox_closeup", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_TOOL_RCLICKED, 168, "command_tool_rclicked", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_TOOL_ENTER, 168, "command_tool_enter", "wxCommandEvent", "wxCommand"},
     {wxEVT_TOOL_DROPDOWN, 168, "tool_dropdown", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_CHECKLISTBOX_TOGGLED, 168, "command_checklistbox_toggled", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_TOGGLEBUTTON_CLICKED, 168, "command_togglebutton_clicked", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_LEFT_CLICK, 168, "command_left_click", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_LEFT_DCLICK, 168, "command_left_dclick", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_RIGHT_CLICK, 168, "command_right_click", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_SET_FOCUS, 168, "command_set_focus", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_KILL_FOCUS, 168, "command_kill_focus", "wxCommandEvent", "wxCommand"},
     {wxEVT_COMMAND_ENTER, 168, "command_enter", "wxCommandEvent", "wxCommand"},
#if wxCHECK_VERSION(3,1,0)
     {wxEVT_NOTIFICATION_MESSAGE_CLICK, 168, "notification_message_click", "wxCommandEvent", "wxCommand"},
#endif
#if wxCHECK_VERSION(3,1,0)
     {wxEVT_NOTIFICATION_MESSAGE_DISMISSED, 168, "notification_message_dismissed", "wxCommandEvent", "wxCommand"},
#endif
#if wxCHECK_VERSION(3,1,0)
     {wxEVT_NOTIFICATION_MESSAGE_ACTION, 168, "notification_message_action", "wxCommandEvent", "wxCommand"},
#endif
     {wxEVT_SCROLL_TOP, 169, "scroll_top", "wxScrollEvent", "wxScroll"},
     {wxEVT_SCROLL_BOTTOM, 169, "scroll_bottom", "wxScrollEvent", "wxScroll"},
     {wxEVT_SCROLL_LINEUP, 169, "scroll_lineup", "wxScrollEvent", "wxScroll"},
     {wxEVT_SCROLL_LINEDOWN, 169, "scroll_linedown", "wxScrollEvent", "wxScroll"},
     {wxEVT_SCROLL_PAGEUP, 169, "scroll_pageup", "wxScrollEvent", "wxScroll"},
     {wxEVT_SCROLL_PAGEDOWN, 169, "scroll_pagedown", "wxScrollEvent", "wxScroll"},
     {wxEVT_SCROLL_THUMBTRACK, 169, "scroll_thumbtrack", "wxScrollEvent", "wxScroll"},
     {wxEVT_SCROLL_THUMBRELEASE, 169, "scroll_thumbrelease", "wxScrollEvent", "wxScroll"},
     {wxEVT_SCROLL_CHANGED, 169, "scroll_changed", "wxScrollEvent", "wxScroll"},
     {wxEVT_SCROLLWIN_TOP, 170, "scrollwin_top", "wxScrollWinEvent", "wxScrollWin"},
     {wxEVT_SCROLLWIN_BOTTOM, 170, "scrollwin_bottom", "wxScrollWinEvent", "wxScrollWin"},
     {wxEVT_SCROLLWIN_LINEUP, 170, "scrollwin_lineup", "wxScrollWinEvent", "wxScrollWin"},
     {wxEVT_SCROLLWIN_LINEDOWN, 170, "scrollwin_linedown", "wxScrollWinEvent", "wxScrollWin"},
     {wxEVT_SCROLLWIN_PAGEUP, 170, "scrollwin_pageup", "wxScrollWinEvent", "wxScrollWin"},
     {wxEVT_SCROLLWIN_PAGEDOWN, 170, "scrollwin_pagedown", "wxScrollWinEvent", "wxScrollWin"},
     {wxEVT_SCROLLWIN_THUMBTRACK, 170, "scrollwin_thumbtrack", "wxScrollWinEvent", "wxScrollWin"},
     {wxEVT_SCROLLWIN_THUMBRELEASE, 170, "scrollwin_thumbrelease", "wxScrollWinEvent", "wxScrollWin"},
     {wxEVT_LEFT_DOWN, 171, "left_down", "wxMouseEvent", "wxMouse"},
     {wxEVT_LEFT_UP, 171, "left_up", "wxMouseEvent", "wxMouse"},
     {wxEVT_MIDDLE_DOWN, 171, "middle_down", "wxMouseEvent", "wxMouse"},
     {wxEVT_MIDDLE_UP, 171, "middle_up", "wxMouseEvent", "wxMouse"},
     {wxEVT_RIGHT_DOWN, 171, "right_down", "wxMouseEvent", "wxMouse"},
     {wxEVT_RIGHT_UP, 171, "right_up", "wxMouseEvent", "wxMouse"},
     {wxEVT_MOTION, 171, "motion", "wxMouseEvent", "wxMouse"},
     {wxEVT_ENTER_WINDOW, 171, "enter_window", "wxMouseEvent", "wxMouse"},
     {wxEVT_LEAVE_WINDOW, 171, "leave_window", "wxMouseEvent", "wxMouse"},
     {wxEVT_LEFT_DCLICK, 171, "left_dclick", "wxMouseEvent", "wxMouse"},
     {wxEVT_MIDDLE_DCLICK, 171, "middle_dclick", "wxMouseEvent", "wxMouse"},
     {wxEVT_RIGHT_DCLICK, 171, "right_dclick", "wxMouseEvent", "wxMouse"},
     {wxEVT_MOUSEWHEEL, 171, "mousewheel", "wxMouseEvent", "wxMouse"},
     {wxEVT_AUX1_DOWN, 171, "aux1_down", "wxMouseEvent", "wxMouse"},
     {wxEVT_AUX1_UP, 171, "aux1_up", "wxMouseEvent", "wxMouse"},
     {wxEVT_AUX1_DCLICK, 171, "aux1_dclick", "wxMouseEvent", "wxMouse"},
     {wxEVT_AUX2_DOWN, 171, "aux2_down", "wxMouseEvent", "wxMouse"},
     {wxEVT_AUX2_UP, 171, "aux2_up", "wxMouseEvent", "wxMouse"},
     {wxEVT_AUX2_DCLICK, 171, "aux2_dclick", "wxMouseEvent", "wxMouse"},
     {wxEVT_SET_CURSOR, 172, "set_cursor", "wxSetCursorEvent", "wxSetCursor"},
     {wxEVT_CHAR, 173, "char", "wxKeyEvent", "wxKey"},
     {wxEVT_CHAR_HOOK, 173, "char_hook", "wxKeyEvent", "wxKey"},
     {wxEVT_KEY_DOWN, 173, "key_down", "wxKeyEvent", "wxKey"},
     {wxEVT_KEY_UP, 173, "key_up", "wxKeyEvent", "wxKey"},
     {wxEVT_SIZE, 174, "size", "wxSizeEvent", "wxSize"},
     {wxEVT_MOVE, 175, "move", "wxMoveEvent", "wxMove"},
     {wxEVT_PAINT, 176, "paint", "wxPaintEvent", "wxPaint"},
     {wxEVT_ERASE_BACKGROUND, 177, "erase_background", "wxEraseEvent", "wxErase"},
     {wxEVT_SET_FOCUS, 178, "set_focus", "wxFocusEvent", "wxFocus"},
     {wxEVT_KILL_FOCUS, 178, "kill_focus", "wxFocusEvent", "wxFocus"},
     {wxEVT_CHILD_FOCUS, 179, "child_focus", "wxChildFocusEvent", "wxChildFocus"},
     {wxEVT_MENU_OPEN, 180, "menu_open", "wxMenuEvent", "wxMenu"},
     {wxEVT_MENU_CLOSE, 180, "menu_close", "wxMenuEvent", "wxMenu"},
     {wxEVT_MENU_HIGHLIGHT, 180, "menu_highlight", "wxMenuEvent", "wxMenu"},
     {wxEVT_CLOSE_WINDOW, 181, "close_window", "wxCloseEvent", "wxClose"},
     {wxEVT_END_SESSION, 181, "end_session", "wxCloseEvent", "wxClose"},
     {wxEVT_QUERY_END_SESSION, 181, "query_end_session", "wxCloseEvent", "wxClose"},
     {wxEVT_SHOW, 182, "show", "wxShowEvent", "wxShow"},
     {wxEVT_ICONIZE, 183, "iconize", "wxIconizeEvent", "wxIconize"},
     {wxEVT_MAXIMIZE, 184, "maximize", "wxMaximizeEvent", "wxMaximize"},
     {wxEVT_JOY_BUTTON_DOWN, 185, "joy_button_down", "wxJoystickEvent", "wxJoystick"},
     {wxEVT_JOY_BUTTON_UP, 185, "joy_button_up", "wxJoystickEvent", "wxJoystick"},
     {wxEVT_JOY_MOVE, 185, "joy_move", "wxJoystickEvent", "wxJoystick"},
     {wxEVT_JOY_ZMOVE, 185, "joy_zmove", "wxJoystickEvent", "wxJoystick"},
     {wxEVT_UPDATE_UI, 186, "update_ui", "wxUpdateUIEvent", "wxUpdateUI"},
     {wxEVT_SYS_COLOUR_CHANGED, 187, "sys_colour_changed", "wxSysColourChangedEvent", "wxSysColourChanged"},
     {wxEVT_MOUSE_CAPTURE_CHANGED, 188, "mouse_capture_changed", "wxMouseCaptureChangedEvent", "wxMouseCaptureChanged"},
     {wxEVT_DISPLAY_CHANGED, 189, "display_changed", "wxDisplayChangedEvent", "wxDisplayChanged"},
     {wxEVT_PALETTE_CHANGED, 190, "palette_changed", "wxPaletteChangedEvent", "wxPaletteChanged"},
     {wxEVT_QUERY_NEW_PALETTE, 191, "query_new_palette", "wxQueryNewPaletteEvent", "wxQueryNewPalette"},
     {wxEVT_NAVIGATION_KEY, 192, "navigation_key", "wxNavigationKeyEvent", "wxNavigationKey"},
     {wxEVT_CREATE, 193, "create", "wxWindowCreateEvent", "wxWindowCreate"},
     {wxEVT_DESTROY, 194, "destroy", "wxWindowDestroyEvent", "wxWindowDestroy"},
     {wxEVT_HELP, 195, "help", "wxHelpEvent", "wxHelp"},
     {wxEVT_DETAILED_HELP, 195, "detailed_help", "wxHelpEvent", "wxHelp"},
     {wxEVT_CONTEXT_MENU, 196, "context_menu", "wxContextMenuEvent", "wxContextMenu"},
     {wxEVT_IDLE, 197, "idle", "wxIdleEvent", "wxIdle"},
     {wxEVT_GRID_CELL_LEFT_CLICK, 198, "grid_cell_left_click", "wxGridEvent", "wxGrid"},
     {wxEVT_GRID_CELL_RIGHT_CLICK, 198, "grid_cell_right_click", "wxGridEvent", "wxGrid"},
     {wxEVT_GRID_CELL_LEFT_DCLICK, 198, "grid_cell_left_dclick", "wxGridEvent", "wxGrid"},
     {wxEVT_GRID_CELL_RIGHT_DCLICK, 198, "grid_cell_right_dclick", "wxGridEvent", "wxGrid"},
     {wxEVT_GRID_LABEL_LEFT_CLICK, 198, "grid_label_left_click", "wxGridEvent", "wxGrid"},
     {wxEVT_GRID_LABEL_RIGHT_CLICK, 198, "grid_label_right_click", "wxGridEvent", "wxGrid"},
     {wxEVT_GRID_LABEL_LEFT_DCLICK, 198, "grid_label_left_dclick", "wxGridEvent", "wxGrid"},
     {wxEVT_GRID_LABEL_RIGHT_DCLICK, 198, "grid_label_right_dclick", "wxGridEvent", "wxGrid"},
     {wxEVT_GRID_CELL_CHANGED, 198, "grid_cell_changed", "wxGridEvent", "wxGrid"},
     {wxEVT_GRID_SELECT_CELL, 198, "grid_select_cell", "wxGridEvent", "wxGrid"},
     {wxEVT_GRID_CELL_BEGIN_DRAG, 198, "grid_cell_begin_drag", "wxGridEvent", "wxGrid"},
     {wxEVT_GRID_EDITOR_SHOWN, 198, "grid_editor_shown", "wxGridEvent", "wxGrid"},
     {wxEVT_GRID_EDITOR_HIDDEN, 198, "grid_editor_hidden", "wxGridEvent", "wxGrid"},
     {wxEVT_GRID_COL_MOVE, 198, "grid_col_move", "wxGridEvent", "wxGrid"},
     {wxEVT_GRID_COL_SORT, 198, "grid_col_sort", "wxGridEvent", "wxGrid"},
     {wxEVT_GRID_TABBING, 198, "grid_tabbing", "wxGridEvent", "wxGrid"},
     {wxEVT_SASH_DRAGGED, 200, "sash_dragged", "wxSashEvent", "wxSash"},
     {wxEVT_COMMAND_LIST_BEGIN_DRAG, 201, "command_list_begin_drag", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_BEGIN_RDRAG, 201, "command_list_begin_rdrag", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_BEGIN_LABEL_EDIT, 201, "command_list_begin_label_edit", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_END_LABEL_EDIT, 201, "command_list_end_label_edit", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_DELETE_ITEM, 201, "command_list_delete_item", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_DELETE_ALL_ITEMS, 201, "command_list_delete_all_items", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_KEY_DOWN, 201, "command_list_key_down", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_INSERT_ITEM, 201, "command_list_insert_item", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_COL_CLICK, 201, "command_list_col_click", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_COL_RIGHT_CLICK, 201, "command_list_col_right_click", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_COL_BEGIN_DRAG, 201, "command_list_col_begin_drag", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_COL_DRAGGING, 201, "command_list_col_dragging", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_COL_END_DRAG, 201, "command_list_col_end_drag", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_ITEM_SELECTED, 201, "command_list_item_selected", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_ITEM_DESELECTED, 201, "command_list_item_deselected", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_ITEM_RIGHT_CLICK, 201, "command_list_item_right_click", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_ITEM_MIDDLE_CLICK, 201, "command_list_item_middle_click", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_ITEM_ACTIVATED, 201, "command_list_item_activated", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_ITEM_FOCUSED, 201, "command_list_item_focused", "wxListEvent", "wxList"},
     {wxEVT_COMMAND_LIST_CACHE_HINT, 201, "command_list_cache_hint", "wxListEvent", "wxList"},
     {wxEVT_DATE_CHANGED, 202, "date_changed", "wxDateEvent", "wxDate"},
     {wxEVT_CALENDAR_SEL_CHANGED, 203, "calendar_sel_changed", "wxCalendarEvent", "wxCalendar"},
     {wxEVT_CALENDAR_DAY_CHANGED, 203, "calendar_day_changed", "wxCalendarEvent", "wxCalendar"},
     {wxEVT_CALENDAR_MONTH_CHANGED, 203, "calendar_month_changed", "wxCalendarEvent", "wxCalendar"},
     {wxEVT_CALENDAR_YEAR_CHANGED, 203, "calendar_year_changed", "wxCalendarEvent", "wxCalendar"},
     {wxEVT_CALENDAR_DOUBLECLICKED, 203, "calendar_doubleclicked", "wxCalendarEvent", "wxCalendar"},
     {wxEVT_CALENDAR_WEEKDAY_CLICKED, 203, "calendar_weekday_clicked", "wxCalendarEvent", "wxCalendar"},
     {wxEVT_COMMAND_FILEPICKER_CHANGED, 204, "command_filepicker_changed", "wxFileDirPickerEvent", "wxFileDirPicker"},
     {wxEVT_COMMAND_DIRPICKER_CHANGED, 204, "command_dirpicker_changed", "wxFileDirPickerEvent", "wxFileDirPicker"},
     {wxEVT_COMMAND_COLOURPICKER_CHANGED, 205, "command_colourpicker_changed", "wxColourPickerEvent", "wxColourPicker"},
     {wxEVT_COMMAND_FONTPICKER_CHANGED, 206, "command_fontpicker_changed", "wxFontPickerEvent", "wxFontPicker"},
     {wxEVT_STC_AUTOCOMP_CANCELLED, 207, "stc_autocomp_cancelled", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_AUTOCOMP_CHAR_DELETED, 207, "stc_autocomp_char_deleted", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_AUTOCOMP_SELECTION, 207, "stc_autocomp_selection", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_CALLTIP_CLICK, 207, "stc_calltip_click", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_CHANGE, 207, "stc_change", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_CHARADDED, 207, "stc_charadded", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_DO_DROP, 207, "stc_do_drop", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_DOUBLECLICK, 207, "stc_doubleclick", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_DRAG_OVER, 207, "stc_drag_over", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_DWELLEND, 207, "stc_dwellend", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_DWELLSTART, 207, "stc_dwellstart", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_HOTSPOT_CLICK, 207, "stc_hotspot_click", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_HOTSPOT_DCLICK, 207, "stc_hotspot_dclick", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_HOTSPOT_RELEASE_CLICK, 207, "stc_hotspot_release_click", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_INDICATOR_CLICK, 207, "stc_indicator_click", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_INDICATOR_RELEASE, 207, "stc_indicator_release", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_MACRORECORD, 207, "stc_macrorecord", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_MARGINCLICK, 207, "stc_marginclick", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_MODIFIED, 207, "stc_modified", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_NEEDSHOWN, 207, "stc_needshown", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_PAINTED, 207, "stc_painted", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_ROMODIFYATTEMPT, 207, "stc_romodifyattempt", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_SAVEPOINTLEFT, 207, "stc_savepointleft", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_SAVEPOINTREACHED, 207, "stc_savepointreached", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_START_DRAG, 207, "stc_start_drag", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_STYLENEEDED, 207, "stc_styleneeded", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_UPDATEUI, 207, "stc_updateui", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_USERLISTSELECTION, 207, "stc_userlistselection", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_STC_ZOOM, 207, "stc_zoom", "wxStyledTextEvent", "wxStyledText"},
     {wxEVT_COMMAND_TREE_BEGIN_DRAG, 213, "command_tree_begin_drag", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_BEGIN_RDRAG, 213, "command_tree_begin_rdrag", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_BEGIN_LABEL_EDIT, 213, "command_tree_begin_label_edit", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_END_LABEL_EDIT, 213, "command_tree_end_label_edit", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_DELETE_ITEM, 213, "command_tree_delete_item", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_GET_INFO, 213, "command_tree_get_info", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_SET_INFO, 213, "command_tree_set_info", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_ITEM_EXPANDED, 213, "command_tree_item_expanded", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_ITEM_EXPANDING, 213, "command_tree_item_expanding", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_ITEM_COLLAPSED, 213, "command_tree_item_collapsed", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_ITEM_COLLAPSING, 213, "command_tree_item_collapsing", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_SEL_CHANGED, 213, "command_tree_sel_changed", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_SEL_CHANGING, 213, "command_tree_sel_changing", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_KEY_DOWN, 213, "command_tree_key_down", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_ITEM_ACTIVATED, 213, "command_tree_item_activated", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_ITEM_RIGHT_CLICK, 213, "command_tree_item_right_click", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_ITEM_MIDDLE_CLICK, 213, "command_tree_item_middle_click", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_END_DRAG, 213, "command_tree_end_drag", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_STATE_IMAGE_CLICK, 213, "command_tree_state_image_click", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_ITEM_GETTOOLTIP, 213, "command_tree_item_gettooltip", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_TREE_ITEM_MENU, 213, "command_tree_item_menu", "wxTreeEvent", "wxTree"},
     {wxEVT_DIRCTRL_SELECTIONCHANGED, 213, "dirctrl_selectionchanged", "wxTreeEvent", "wxTree"},
     {wxEVT_DIRCTRL_FILEACTIVATED, 213, "dirctrl_fileactivated", "wxTreeEvent", "wxTree"},
     {wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGED, 214, "command_notebook_page_changed", "wxBookCtrlEvent", "wxBookCtrl"},
     {wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGING, 214, "command_notebook_page_changing", "wxBookCtrlEvent", "wxBookCtrl"},
     {wxEVT_CHOICEBOOK_PAGE_CHANGED, 214, "choicebook_page_changed", "wxBookCtrlEvent", "wxBookCtrl"},
     {wxEVT_CHOICEBOOK_PAGE_CHANGING, 214, "choicebook_page_changing", "wxBookCtrlEvent", "wxBookCtrl"},
     {wxEVT_TREEBOOK_PAGE_CHANGED, 214, "treebook_page_changed", "wxBookCtrlEvent", "wxBookCtrl"},
     {wxEVT_TREEBOOK_PAGE_CHANGING, 214, "treebook_page_changing", "wxBookCtrlEvent", "wxBookCtrl"},
     {wxEVT_TOOLBOOK_PAGE_CHANGED, 214, "toolbook_page_changed", "wxBookCtrlEvent", "wxBookCtrl"},
     {wxEVT_TOOLBOOK_PAGE_CHANGING, 214, "toolbook_page_changing", "wxBookCtrlEvent", "wxBookCtrl"},
     {wxEVT_LISTBOOK_PAGE_CHANGED, 214, "listbook_page_changed", "wxBookCtrlEvent", "wxBookCtrl"},
     {wxEVT_LISTBOOK_PAGE_CHANGING, 214, "listbook_page_changing", "wxBookCtrlEvent", "wxBookCtrl"},
     {wxEVT_COMMAND_TEXT_COPY, 220, "command_text_copy", "wxClipboardTextEvent", "wxClipboardText"},
     {wxEVT_COMMAND_TEXT_CUT, 220, "command_text_cut", "wxClipboardTextEvent", "wxClipboardText"},
     {wxEVT_COMMAND_TEXT_PASTE, 220, "command_text_paste", "wxClipboardTextEvent", "wxClipboardText"},
     {wxEVT_COMMAND_SPINCTRL_UPDATED, 221, "command_spinctrl_updated", "wxSpinEvent", "wxSpin"},
     {wxEVT_SCROLL_LINEUP + wxEVT_USER_FIRST, 169, "spin_up", "wxSpinEvent", "wxSpin"},
     {wxEVT_SCROLL_LINEDOWN + wxEVT_USER_FIRST, 169, "spin_down", "wxSpinEvent", "wxSpin"},
     {wxEVT_SCROLL_THUMBTRACK + wxEVT_USER_FIRST, 169, "spin", "wxSpinEvent", "wxSpin"},
     {wxEVT_COMMAND_SPLITTER_SASH_POS_CHANGED, 223, "command_splitter_sash_pos_changed", "wxSplitterEvent", "wxSplitter"},
     {wxEVT_COMMAND_SPLITTER_SASH_POS_CHANGING, 223, "command_splitter_sash_pos_changing", "wxSplitterEvent", "wxSplitter"},
     {wxEVT_COMMAND_SPLITTER_DOUBLECLICKED, 223, "command_splitter_doubleclicked", "wxSplitterEvent", "wxSplitter"},
     {wxEVT_COMMAND_SPLITTER_UNSPLIT, 223, "command_splitter_unsplit", "wxSplitterEvent", "wxSplitter"},
     {wxEVT_COMMAND_HTML_LINK_CLICKED, 225, "command_html_link_clicked", "wxHtmlLinkEvent", "wxHtmlLink"},
     {wxEVT_HTML_CELL_CLICKED, 225, "html_cell_clicked", "wxHtmlLinkEvent", "wxHtmlLink"},
     {wxEVT_HTML_CELL_HOVER, 225, "html_cell_hover", "wxHtmlLinkEvent", "wxHtmlLink"},
     {wxEVT_COMMAND_AUINOTEBOOK_PAGE_CLOSE, 228, "command_auinotebook_page_close", "wxAuiNotebookEvent", "wxAuiNotebook"},
     {wxEVT_COMMAND_AUINOTEBOOK_PAGE_CHANGED, 228, "command_auinotebook_page_changed", "wxAuiNotebookEvent", "wxAuiNotebook"},
     {wxEVT_COMMAND_AUINOTEBOOK_PAGE_CHANGING, 228, "command_auinotebook_page_changing", "wxAuiNotebookEvent", "wxAuiNotebook"},
     {wxEVT_COMMAND_AUINOTEBOOK_BUTTON, 228, "command_auinotebook_button", "wxAuiNotebookEvent", "wxAuiNotebook"},
     {wxEVT_COMMAND_AUINOTEBOOK_BEGIN_DRAG, 228, "command_auinotebook_begin_drag", "wxAuiNotebookEvent", "wxAuiNotebook"},
     {wxEVT_COMMAND_AUINOTEBOOK_END_DRAG, 228, "command_auinotebook_end_drag", "wxAuiNotebookEvent", "wxAuiNotebook"},
     {wxEVT_COMMAND_AUINOTEBOOK_DRAG_MOTION, 228, "command_auinotebook_drag_motion", "wxAuiNotebookEvent", "wxAuiNotebook"},
     {wxEVT_COMMAND_AUINOTEBOOK_ALLOW_DND, 228, "command_auinotebook_allow_dnd", "wxAuiNotebookEvent", "wxAuiNotebook"},
     {wxEVT_COMMAND_AUINOTEBOOK_TAB_MIDDLE_DOWN, 228, "command_auinotebook_tab_middle_down", "wxAuiNotebookEvent", "wxAuiNotebook"},
     {wxEVT_COMMAND_AUINOTEBOOK_TAB_MIDDLE_UP, 228, "command_auinotebook_tab_middle_up", "wxAuiNotebookEvent", "wxAuiNotebook"},
     {wxEVT_COMMAND_AUINOTEBOOK_TAB_RIGHT_DOWN, 228, "command_auinotebook_tab_right_down", "wxAuiNotebookEvent", "wxAuiNotebook"},
     {wxEVT_COMMAND_AUINOTEBOOK_TAB_RIGHT_UP, 228, "command_auinotebook_tab_right_up", "wxAuiNotebookEvent", "wxAuiNotebook"},
     {wxEVT_COMMAND_AUINOTEBOOK_PAGE_CLOSED, 228, "command_auinotebook_page_closed", "wxAuiNotebookEvent", "wxAuiNotebook"},
     {wxEVT_COMMAND_AUINOTEBOOK_DRAG_DONE, 228, "command_auinotebook_drag_done", "wxAuiNotebookEvent", "wxAuiNotebook"},
     {wxEVT_COMMAND_AUINOTEBOOK_BG_DCLICK, 228, "command_auinotebook_bg_dclick", "wxAuiNotebookEvent", "wxAuiNotebook"},
     {wxEVT_AUI_PANE_BUTTON, 229, "aui_pane_button", "wxAuiManagerEvent", "wxAuiManager"},
     {wxEVT_AUI_PANE_CLOSE, 229, "aui_pane_close", "wxAuiManagerEvent", "wxAuiManager"},
     {wxEVT_AUI_PANE_MAXIMIZE, 229, "aui_pane_maximize", "wxAuiManagerEvent", "wxAuiManager"},
     {wxEVT_AUI_PANE_RESTORE, 229, "aui_pane_restore", "wxAuiManagerEvent", "wxAuiManager"},
     {wxEVT_AUI_PANE_ACTIVATED, 229, "aui_pane_activated", "wxAuiManagerEvent", "wxAuiManager"},
     {wxEVT_AUI_RENDER, 229, "aui_render", "wxAuiManagerEvent", "wxAuiManager"},
     {wxEVT_AUI_FIND_MANAGER, 229, "aui_find_manager", "wxAuiManagerEvent", "wxAuiManager"},
     {wxEVT_TASKBAR_MOVE, 232, "taskbar_move", "wxTaskBarIconEvent", "wxTaskBarIcon"},
     {wxEVT_TASKBAR_LEFT_DOWN, 232, "taskbar_left_down", "wxTaskBarIconEvent", "wxTaskBarIcon"},
     {wxEVT_TASKBAR_LEFT_UP, 232, "taskbar_left_up", "wxTaskBarIconEvent", "wxTaskBarIcon"},
     {wxEVT_TASKBAR_RIGHT_DOWN, 232, "taskbar_right_down", "wxTaskBarIconEvent", "wxTaskBarIcon"},
     {wxEVT_TASKBAR_RIGHT_UP, 232, "taskbar_right_up", "wxTaskBarIconEvent", "wxTaskBarIcon"},
     {wxEVT_TASKBAR_LEFT_DCLICK, 232, "taskbar_left_dclick", "wxTaskBarIconEvent", "wxTaskBarIcon"},
     {wxEVT_TASKBAR_RIGHT_DCLICK, 232, "taskbar_right_dclick", "wxTaskBarIconEvent", "wxTaskBarIcon"},
     {wxEVT_INIT_DIALOG, 233, "init_dialog", "wxInitDialogEvent", "wxInitDialog"},
     {wxEVT_ACTIVATE, 235, "activate", "wxActivateEvent", "wxActivate"},
     {wxEVT_ACTIVATE_APP, 235, "activate_app", "wxActivateEvent", "wxActivate"},
     {wxEVT_HIBERNATE, 235, "hibernate", "wxActivateEvent", "wxActivate"},
     {wxEVT_MOUSE_CAPTURE_LOST, 238, "mouse_capture_lost", "wxMouseCaptureLostEvent", "wxMouseCaptureLost"},
     {wxEVT_DROP_FILES, 241, "drop_files", "wxDropFilesEvent", "wxDropFiles"},
#if WXE_WEBVIEW
     {wxEVT_WEBVIEW_NAVIGATING, 246, "webview_navigating", "wxWebViewEvent", "wxWebView"},
#endif
#if WXE_WEBVIEW
     {wxEVT_WEBVIEW_NAVIGATED, 246, "webview_navigated", "wxWebViewEvent", "wxWebView"},
#endif
#if WXE_WEBVIEW
     {wxEVT_WEBVIEW_LOADED, 246, "webview_loaded", "wxWebViewEvent", "wxWebView"},
#endif
#if WXE_WEBVIEW
     {wxEVT_WEBVIEW_ERROR, 246, "webview_error", "wxWebViewEvent", "wxWebView"},
#endif
#if WXE_WEBVIEW
     {wxEVT_WEBVIEW_NEWWINDOW, 246, "webview_newwindow", "wxWebViewEvent", "wxWebView"},
#endif
#if WXE_WEBVIEW
     {wxEVT_WEBVIEW_TITLE_CHANGED, 246, "webview_title_changed", "wxWebViewEvent", "wxWebView"},
#endif
     {-1, 0, "", "", ""}
  };

  ErlNifEnv *env = enif_alloc_env();
  for(int i=0; event_types[i].ev_type != -1; i++) {
     if(NULL == etmap[event_types[i].ev_type]) {
       etmap[event_types[i].ev_type] =
         new wxeEtype(env, &event_types[i]);
     } else {
       wxeEtype *prev = etmap[event_types[i].ev_type];
       wxString msg(wxT("Duplicate event defs: "));
       msg += wxString::FromAscii(event_types[i].ev_name);
       msg += wxString::Format(wxT(" %d "), event_types[i].class_id);
       msg += wxString::FromAscii(prev->evName);
       msg += wxString::Format(wxT(" %d"), prev->cID);
       send_msg("internal_error", &msg);
     }
  }
  enif_free_env(env);
}

bool sendevent(wxEvent *event, wxeMemEnv *memenv)
{
  int send_res ;
  wxMBConvUTF32 UTFconverter;
  wxeEtype *Etype = etmap[event->GetEventType()];
  wxeEvtListener *cb = (wxeEvtListener *)event->m_callbackUserData;
  WxeApp * app = (WxeApp *) wxTheApp;
  if(!memenv) return 0;

  wxeReturn rt = wxeReturn(memenv, cb->listener, false);
  ERL_NIF_TERM ev_term;
  switch(Etype->cID) {
  case 168: {// wxCommandEvent
    wxCommandEvent * ev = (wxCommandEvent *) event;
    ev_term = enif_make_tuple5(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make(ev->GetString()),
        rt.make_int(ev->GetInt()),
        rt.make_int(ev->GetExtraLong()));
    break;
  }
  case 169: {// wxScrollEvent or wxSpinEvent
    if(event->IsKindOf(CLASSINFO(wxScrollEvent))) {
    wxScrollEvent * ev = (wxScrollEvent *) event;
    ev_term = enif_make_tuple4(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_int(ev->GetPosition()),
        rt.make_int(ev->GetOrientation()));
    } else {
      Etype = etmap[event->GetEventType() + wxEVT_USER_FIRST];
    wxSpinEvent * ev = (wxSpinEvent *) event;
    ev_term = enif_make_tuple3(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_int(ev->GetPosition()));
  }
    break;
  }
  case 170: {// wxScrollWinEvent
    wxScrollWinEvent * ev = (wxScrollWinEvent *) event;
    ev_term = enif_make_tuple4(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_int(ev->GetPosition()),
        rt.make_int(ev->GetOrientation()));
    break;
  }
  case 171: {// wxMouseEvent
    wxMouseEvent * ev = (wxMouseEvent *) event;
    ev_term = enif_make_tuple(rt.env,14,
        Etype->evRecord,
        Etype->evName,
        rt.make_int(ev->GetX()),
        rt.make_int(ev->GetY()),
        rt.make_bool(ev->LeftIsDown()),
        rt.make_bool(ev->MiddleIsDown()),
        rt.make_bool(ev->RightIsDown()),
        rt.make_bool(ev->ControlDown()),
        rt.make_bool(ev->ShiftDown()),
        rt.make_bool(ev->AltDown()),
        rt.make_bool(ev->MetaDown()),
        rt.make_int(ev->GetWheelRotation()),
        rt.make_int(ev->GetWheelDelta()),
        rt.make_int(ev->GetLinesPerAction()));
    break;
  }
  case 172: {// wxSetCursorEvent
    wxSetCursorEvent * ev = (wxSetCursorEvent *) event;
    const  wxCursor * GetCursor = &ev->GetCursor();
    ev_term = enif_make_tuple5(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_int(ev->GetX()),
        rt.make_int(ev->GetY()),
        rt.make_ref(app->getRef((void *)GetCursor,memenv), "wxCursor"));
    break;
  }
  case 173: {// wxKeyEvent
    wxKeyEvent * ev = (wxKeyEvent *) event;
    ev_term = enif_make_tuple(rt.env,12,
        Etype->evRecord,
        Etype->evName,
        rt.make_int(ev->GetX()),
        rt.make_int(ev->GetY()),
        rt.make_int(ev->GetKeyCode()),
        rt.make_bool(ev->ControlDown()),
        rt.make_bool(ev->ShiftDown()),
        rt.make_bool(ev->AltDown()),
        rt.make_bool(ev->MetaDown()),
        rt.make_int(ev->GetUnicodeKey()),
        rt.make_uint(ev->GetRawKeyCode()),
        rt.make_uint(ev->GetRawKeyFlags()));
    break;
  }
  case 174: {// wxSizeEvent
    wxSizeEvent * ev = (wxSizeEvent *) event;
    ev_term = enif_make_tuple4(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make(ev->GetSize()),
        rt.make(ev->GetRect()));
    break;
  }
  case 175: {// wxMoveEvent
    wxMoveEvent * ev = (wxMoveEvent *) event;
    ev_term = enif_make_tuple4(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make(ev->GetPosition()),
        rt.make(ev->GetRect()));
    break;
  }
  case 176: {// wxPaintEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 177: {// wxEraseEvent
    wxEraseEvent * ev = (wxEraseEvent *) event;
      wxDC * GetDC = ev->GetDC();
    ev_term = enif_make_tuple3(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_ref(app->getRef((void *)GetDC,memenv), "wxDC"));
    break;
  }
  case 178: {// wxFocusEvent
    wxFocusEvent * ev = (wxFocusEvent *) event;
      wxWindow * GetWindow = ev->GetWindow();
    ev_term = enif_make_tuple3(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_ref(app->getRef((void *)GetWindow,memenv), "wxWindow"));
    break;
  }
  case 179: {// wxChildFocusEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 180: {// wxMenuEvent
    wxMenuEvent * ev = (wxMenuEvent *) event;
      wxMenu * GetMenu = ev->GetMenu();
    ev_term = enif_make_tuple4(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_int(ev->GetMenuId()),
        rt.make_ref(app->getRef((void *)GetMenu,memenv), "wxMenu"));
    break;
  }
  case 181: {// wxCloseEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 182: {// wxShowEvent
    wxShowEvent * ev = (wxShowEvent *) event;
    ev_term = enif_make_tuple3(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_bool(ev->IsShown()));
    break;
  }
  case 183: {// wxIconizeEvent
    wxIconizeEvent * ev = (wxIconizeEvent *) event;
    ev_term = enif_make_tuple3(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_bool(ev->IsIconized()));
    break;
  }
  case 184: {// wxMaximizeEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 185: {// wxJoystickEvent
    wxJoystickEvent * ev = (wxJoystickEvent *) event;
    ev_term = enif_make_tuple7(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make(ev->GetPosition()),
        rt.make_int(ev->GetZPosition()),
        rt.make_int(ev->GetButtonChange()),
        rt.make_int(ev->GetButtonState()),
        rt.make_int(ev->GetJoystick()));
    break;
  }
  case 186: {// wxUpdateUIEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 187: {// wxSysColourChangedEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 188: {// wxMouseCaptureChangedEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 189: {// wxDisplayChangedEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 190: {// wxPaletteChangedEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 191: {// wxQueryNewPaletteEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 192: {// wxNavigationKeyEvent
    wxNavigationKeyEvent * ev = (wxNavigationKeyEvent *) event;
      wxWindow * GetCurrentFocus = ev->GetCurrentFocus();
    ev_term = enif_make_tuple4(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_bool(ev->GetDirection()),
        rt.make_ref(app->getRef((void *)GetCurrentFocus,memenv), "wxWindow"));
    break;
  }
  case 193: {// wxWindowCreateEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 194: {// wxWindowDestroyEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 195: {// wxHelpEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 196: {// wxContextMenuEvent
    wxContextMenuEvent * ev = (wxContextMenuEvent *) event;
    ev_term = enif_make_tuple3(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make(ev->GetPosition()));
    break;
  }
  case 197: {// wxIdleEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 198: {// wxGridEvent
    wxGridEvent * ev = (wxGridEvent *) event;
    ev_term = enif_make_tuple(rt.env,10,
        Etype->evRecord,
        Etype->evName,
        rt.make_int(ev->GetRow()),
        rt.make_int(ev->GetCol()),
        rt.make(ev->GetPosition()),
        rt.make_bool(ev->Selecting()),
        rt.make_bool(ev->ControlDown()),
        rt.make_bool(ev->MetaDown()),
        rt.make_bool(ev->ShiftDown()),
        rt.make_bool(ev->AltDown()));
    break;
  }
  case 200: {// wxSashEvent
    wxSashEvent * ev = (wxSashEvent *) event;
    ev_term = enif_make_tuple5(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_int(ev->GetEdge()),
        rt.make(ev->GetDragRect()),
        rt.make_int(ev->GetDragStatus()));
    break;
  }
  case 201: {// wxListEvent
    wxListEvent * ev = (wxListEvent *) event;
    ev_term = enif_make_tuple7(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_int(ev->GetKeyCode()),
        rt.make_int(ev->GetCacheFrom()),
        rt.make_int(ev->GetIndex()),
        rt.make_int(ev->GetColumn()),
        rt.make(ev->GetPoint()));
    break;
  }
  case 202: {// wxDateEvent
    wxDateEvent * ev = (wxDateEvent *) event;
    ev_term = enif_make_tuple3(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make(ev->GetDate()));
    break;
  }
  case 203: {// wxCalendarEvent
    wxCalendarEvent * ev = (wxCalendarEvent *) event;
    ev_term = enif_make_tuple4(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_int(ev->GetWeekDay()),
        rt.make(ev->GetDate()));
    break;
  }
  case 204: {// wxFileDirPickerEvent
    wxFileDirPickerEvent * ev = (wxFileDirPickerEvent *) event;
    ev_term = enif_make_tuple3(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make(ev->GetPath()));
    break;
  }
  case 205: {// wxColourPickerEvent
    wxColourPickerEvent * ev = (wxColourPickerEvent *) event;
    ev_term = enif_make_tuple3(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make(ev->GetColour()));
    break;
  }
  case 206: {// wxFontPickerEvent
    wxFontPickerEvent * ev = (wxFontPickerEvent *) event;
    wxFont * GetFont = new wxFont(ev->GetFont());
    app->newPtr((void *) GetFont,3, memenv);
    ev_term = enif_make_tuple3(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_ref(app->getRef((void *)GetFont,memenv), "wxFont"));
    break;
  }
  case 207: {// wxStyledTextEvent
    wxStyledTextEvent * ev = (wxStyledTextEvent *) event;
    ev_term = enif_make_tuple(rt.env,22,
        Etype->evRecord,
        Etype->evName,
        rt.make_int(ev->GetPosition()),
        rt.make_int(ev->GetKey()),
        rt.make_int(ev->GetModifiers()),
        rt.make_int(ev->GetModificationType()),
        rt.make(ev->GetText()),
        rt.make_int(ev->GetLength()),
        rt.make_int(ev->GetLinesAdded()),
        rt.make_int(ev->GetLine()),
        rt.make_int(ev->GetFoldLevelNow()),
        rt.make_int(ev->GetFoldLevelPrev()),
        rt.make_int(ev->GetMargin()),
        rt.make_int(ev->GetMessage()),
        rt.make_int(ev->GetWParam()),
        rt.make_int(ev->GetLParam()),
        rt.make_int(ev->GetListType()),
        rt.make_int(ev->GetX()),
        rt.make_int(ev->GetY()),
        rt.make(ev->GetDragText()),
        rt.make_bool(ev->GetDragAllowMove()),
        rt.make_int(ev->GetDragResult()));
    break;
  }
  case 213: {// wxTreeEvent
    wxTreeEvent * ev = (wxTreeEvent *) event;
    ev_term = enif_make_tuple5(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make((wxUIntPtr *) ev->GetItem().m_pItem),
        rt.make((wxUIntPtr *) ev->GetOldItem().m_pItem),
        rt.make(ev->GetPoint()));
    break;
  }
  case 214: {// wxBookCtrlEvent
    wxBookCtrlEvent * ev = (wxBookCtrlEvent *) event;
    ev_term = enif_make_tuple4(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_int(ev->GetSelection()),
        rt.make_int(ev->GetOldSelection()));
    break;
  }
  case 220: {// wxClipboardTextEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 221: {// wxSpinEvent
    wxSpinEvent * ev = (wxSpinEvent *) event;
    ev_term = enif_make_tuple3(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_int(ev->GetPosition()));
    break;
  }
  case 223: {// wxSplitterEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 225: {// wxHtmlLinkEvent
    wxHtmlLinkEvent * ev = (wxHtmlLinkEvent *) event;
    ev_term = enif_make_tuple3(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make(ev->GetLinkInfo()));
    break;
  }
  case 228: {// wxAuiNotebookEvent
    wxAuiNotebookEvent * ev = (wxAuiNotebookEvent *) event;
      wxAuiNotebook * GetDragSource = ev->GetDragSource();
    ev_term = enif_make_tuple5(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_int(ev->GetOldSelection()),
        rt.make_int(ev->GetSelection()),
        rt.make_ref(app->getRef((void *)GetDragSource,memenv), "wxAuiNotebook"));
    break;
  }
  case 229: {// wxAuiManagerEvent
    wxAuiManagerEvent * ev = (wxAuiManagerEvent *) event;
      wxAuiManager * GetManager = ev->GetManager();
      wxAuiPaneInfo * GetPane = ev->GetPane();
      wxDC * GetDC = ev->GetDC();
    ev_term = enif_make_tuple(rt.env,8,
        Etype->evRecord,
        Etype->evName,
        rt.make_ref(app->getRef((void *)GetManager,memenv), "wxAuiManager"),
        rt.make_ref(app->getRef((void *)GetPane,memenv), "wxAuiPaneInfo"),
        rt.make_int(ev->GetButton()),
        rt.make_bool(ev->GetVeto()),
        rt.make_bool(ev->CanVeto()),
        rt.make_ref(app->getRef((void *)GetDC,memenv), "wxDC"));
    break;
  }
  case 232: {// wxTaskBarIconEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 233: {// wxInitDialogEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 235: {// wxActivateEvent
    wxActivateEvent * ev = (wxActivateEvent *) event;
    ev_term = enif_make_tuple3(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make_bool(ev->GetActive()));
    break;
  }
  case 238: {// wxMouseCaptureLostEvent
    ev_term = enif_make_tuple2(rt.env,
        Etype->evRecord,
        Etype->evName
);
    break;
  }
  case 241: {// wxDropFilesEvent
    wxDropFilesEvent * ev = (wxDropFilesEvent *) event;
    ev_term = enif_make_tuple4(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make(ev->GetPosition()),
        rt.make_list_strings(ev->m_noFiles, ev->GetFiles())
);
    break;
  }
#if WXE_WEBVIEW
  case 246: {// wxWebViewEvent
    wxWebViewEvent * ev = (wxWebViewEvent *) event;
    ev_term = enif_make_tuple6(rt.env,
        Etype->evRecord,
        Etype->evName,
        rt.make(ev->GetString()),
        rt.make_int(ev->GetInt()),
        rt.make(ev->GetTarget()),
        rt.make(ev->GetURL()));
    break;
  }
#endif // "WXE_WEBVIEW"

  }

  ERL_NIF_TERM wx_ev =
    enif_make_tuple5(rt.env,
                     WXE_ATOM_wx,
                     rt.make_int((int) event->GetId()),
                     rt.make_ref(cb->obj, cb->class_name),
                     rt.make_ext2term(cb->user_data),
                     ev_term);

  if(cb->fun_id) {
    ERL_NIF_TERM wx_cb =
      enif_make_tuple4(rt.env,
                       WXE_ATOM__wx_invoke_cb_,
                       rt.make_int(cb->fun_id),
                       wx_ev,
                       rt.make_ref(app->getRef((void *)event,memenv), Etype->evClass)
                       );
    pre_callback();
    send_res =  rt.send(wx_cb);
    if(send_res) handle_event_callback(memenv->me_ref, cb->listener);
    app->clearPtr((void *) event);
  } else {
    send_res =  rt.send(wx_ev);
    if(cb->skip) event->Skip();
    if(app->recurse_level < 1 && (Etype->cID == 174 || Etype->cID == 175)) {
      app->recurse_level++;
      app->dispatch_cmds();
      app->recurse_level--;
    }
  };
  return send_res;
}
