/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2023. All Rights Reserved.
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

#include <wx/wx.h>
#include "../wxe_impl.h"

extern void wxe_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxe_registerPid(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxEvtHandler_Connect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxEvtHandler_Disconnect_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_CacheBestSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_CaptureMouse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Centre(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_CentreOnParent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_ClearBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_ClientToScreen_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_ClientToScreen_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Close(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_ConvertDialogToPixels(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_ConvertPixelsToDialog(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_DestroyChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Disable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_DragAcceptFiles(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Enable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_FindFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_FindWindow_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_FindWindow_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_FindWindowById(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_FindWindowByName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_FindWindowByLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Fit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_FitInside(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Freeze(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetAcceleratorTable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetBackgroundStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetBestSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetCaret(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetCapture(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetCharHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetCharWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetClientSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetContainingSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetDropTarget(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if wxCHECK_VERSION(3,1,4)
extern void wxWindow_GetDPIScaleFactor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxCHECK_VERSION(3,1,4)
extern void wxWindow_GetExtraStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetForegroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetGrandParent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetHandle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetHelpText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetMaxSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetMinSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetParent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetScreenPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetScreenRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetScrollPos(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetScrollRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetScrollThumb(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetTextExtent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetThemeEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetToolTip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetUpdateRegion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetVirtualSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetWindowStyleFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_GetWindowVariant(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_HasCapture(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_HasScrollbar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_HasTransparentBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Hide(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_InheritAttributes(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_InitDialog(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_InvalidateBestSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_IsFrozen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_IsEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_IsExposed_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_IsExposed_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_IsExposed_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_IsExposed_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_IsRetained(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_IsShown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_IsTopLevel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_IsShownOnScreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Layout(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_LineDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_LineUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Lower(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Move_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Move_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_MoveAfterInTabOrder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_MoveBeforeInTabOrder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Navigate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_PageDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_PageUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_PopupMenu_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_PopupMenu_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Raise(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Refresh(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_RefreshRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_ReleaseMouse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_RemoveChild(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Reparent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_ScreenToClient_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_ScreenToClient_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_ScrollLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_ScrollPages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_ScrollWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetAcceleratorTable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetAutoLayout(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetBackgroundStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetCaret(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetClientSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetClientSize_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetClientSize_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetContainingSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetMaxSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetMinSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetOwnBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetOwnFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetOwnForegroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetDropTarget(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetExtraStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetFocusFromKbd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetForegroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetHelpText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetPalette(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetScrollbar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetScrollPos(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetSize_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetSize_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetSize_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetSizeHints_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetSizeHints_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetSizerAndFit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetThemeEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetToolTip_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetToolTip_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetVirtualSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetVirtualSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetWindowStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetWindowStyleFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetWindowVariant(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_ShouldInheritColours(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Thaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_TransferDataFromWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_TransferDataToWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Update(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_UpdateWindowUI(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Validate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_WarpPointer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_SetTransparent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_CanSetTransparent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_IsDoubleBuffered(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if wxCHECK_VERSION(3,1,0) || (!defined(__WXMAC__) && wxCHECK_VERSION(3,0,0))
extern void wxWindow_SetDoubleBuffered(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxCHECK_VERSION(3,1,0) || (!defined(__WXMAC__) && wxCHECK_VERSION(3,0,0))
extern void wxWindow_GetContentScaleFactor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if wxCHECK_VERSION(3,1,3)
extern void wxWindow_GetDPI(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxCHECK_VERSION(3,1,3)
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_FromDIP_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxCHECK_VERSION(3,1,0)
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_FromDIP_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxCHECK_VERSION(3,1,0)
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_FromDIP_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxCHECK_VERSION(3,1,0)
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_FromDIP_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxCHECK_VERSION(3,1,0)
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_ToDIP_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxCHECK_VERSION(3,1,0)
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_ToDIP_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxCHECK_VERSION(3,1,0)
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_ToDIP_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxCHECK_VERSION(3,1,0)
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_ToDIP_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxCHECK_VERSION(3,1,0)
extern void wxTopLevelWindow_GetIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTopLevelWindow_GetIcons(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTopLevelWindow_GetTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTopLevelWindow_IsActive(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTopLevelWindow_Iconize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTopLevelWindow_IsFullScreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTopLevelWindow_IsIconized(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTopLevelWindow_IsMaximized(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTopLevelWindow_Maximize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTopLevelWindow_RequestUserAttention(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTopLevelWindow_SetIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTopLevelWindow_SetIcons(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTopLevelWindow_CentreOnScreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTopLevelWindow_SetShape(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTopLevelWindow_SetTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTopLevelWindow_ShowFullScreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_CreateStatusBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_CreateToolBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_GetClientAreaOrigin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_GetMenuBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_GetStatusBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_GetStatusBarPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_GetToolBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_ProcessCommand(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_SendSizeEvent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_SetMenuBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_SetStatusBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_SetStatusBarPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_SetStatusText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_SetStatusWidths(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFrame_SetToolBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMiniFrame_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMiniFrame_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMiniFrame_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplashScreen_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplashScreen_GetSplashStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplashScreen_GetTimeout(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPanel_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPanel_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPanel_InitDialog(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPanel_SetFocusIgnoringChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_CalcScrolledPosition_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_CalcScrolledPosition_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_CalcUnscrolledPosition_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_CalcUnscrolledPosition_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_EnableScrolling(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_GetScrollPixelsPerUnit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_GetViewStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_DoPrepareDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_PrepareDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_Scroll_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_Scroll_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_SetScrollbars(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_SetScrollRate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrolledWindow_SetTargetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashWindow_GetSashVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashWindow_GetMaximumSizeX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashWindow_GetMaximumSizeY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashWindow_GetMinimumSizeX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashWindow_GetMinimumSizeY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashWindow_SetMaximumSizeX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashWindow_SetMaximumSizeY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashWindow_SetMinimumSizeX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashWindow_SetMinimumSizeY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashWindow_SetSashVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashLayoutWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashLayoutWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashLayoutWindow_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashLayoutWindow_GetAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashLayoutWindow_GetOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashLayoutWindow_SetAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashLayoutWindow_SetDefaultSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashLayoutWindow_SetOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_AppendCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_AppendRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_AutoSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_AutoSizeColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_AutoSizeColumns(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_AutoSizeRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_AutoSizeRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_BeginBatch(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_BlockToDeviceRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_CanDragCell(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_CanDragColMove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if wxCHECK_VERSION(3,1,4)
extern void wxGrid_CanDragGridRowEdges(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxCHECK_VERSION(3,1,4)
extern void wxGrid_CanDragColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_CanDragRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_CanDragGridSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_CanEnableCellControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_CellToRect_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_CellToRect_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_ClearGrid(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_ClearSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_CreateGrid(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_DeleteCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_DeleteRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_DisableCellEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_DisableDragColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_DisableDragGridSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_DisableDragRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_EnableCellEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_EnableDragColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_EnableDragGridSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_EnableDragRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_EnableEditing(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_EnableGridLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_EndBatch(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_Fit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_ForceRefresh(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetBatchCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetCellAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetCellBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetCellEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetCellFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetCellRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetCellTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetCellValue_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetCellValue_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetColLabelAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetColLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetColLabelValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetColMinimalAcceptableWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetDefaultCellAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetDefaultCellBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetDefaultCellFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetDefaultCellTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetDefaultColLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetDefaultColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetDefaultEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetDefaultEditorForCell_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetDefaultEditorForCell_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetDefaultEditorForType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetDefaultRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetDefaultRendererForCell(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetDefaultRendererForType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetDefaultRowLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetDefaultRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetGridCursorCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetGridCursorRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetGridLineColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GridLinesEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetLabelBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetLabelFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetLabelTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetNumberCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetNumberRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetOrCreateCellAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetRowMinimalAcceptableHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetRowLabelAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetRowLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetRowLabelValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetScrollLineX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetScrollLineY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetSelectedCells(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetSelectedCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetSelectedRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetSelectionBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetSelectionBlockTopLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetSelectionBlockBottomRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetSelectionForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetGridWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetGridRowLabelWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetGridColLabelWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_GetGridCornerLabelWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_HideCellEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_InsertCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_InsertRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_IsCellEditControlEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_IsCurrentCellReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_IsEditable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_IsInSelection_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_IsInSelection_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_IsReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_IsSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_IsVisible_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_IsVisible_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_MakeCellVisible_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_MakeCellVisible_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_MoveCursorDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_MoveCursorLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_MoveCursorRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_MoveCursorUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_MoveCursorDownBlock(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_MoveCursorLeftBlock(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_MoveCursorRightBlock(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_MoveCursorUpBlock(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_MovePageDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_MovePageUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_RegisterDataType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SaveEditControlValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SelectAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SelectBlock_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SelectBlock_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SelectCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SelectRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetCellAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetCellBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetCellEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetCellFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetCellRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetCellTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetCellValue_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetCellValue_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetColAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetColFormatBool(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetColFormatNumber(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetColFormatFloat(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetColFormatCustom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetColLabelAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetColLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetColLabelValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetColMinimalWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetColMinimalAcceptableWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetDefaultCellAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetDefaultCellBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetDefaultCellFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetDefaultCellTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetDefaultEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetDefaultRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetDefaultColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetDefaultRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetGridCursor_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetGridCursor_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetGridLineColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetLabelBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetLabelFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetLabelTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetRowAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetRowLabelAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetRowLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetRowLabelValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetRowMinimalHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetRowMinimalAcceptableHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetScrollLineX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetScrollLineY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetSelectionBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetSelectionForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_SetSelectionMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_ShowCellEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_XToCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_XToEdgeOfCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_YToEdgeOfRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGrid_YToRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellRenderer_Draw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellRenderer_GetBestSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellEditor_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellEditor_IsCreated(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellEditor_SetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellEditor_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellEditor_Reset(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellEditor_StartingKey(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellEditor_StartingClick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellEditor_HandleReturn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellBoolRenderer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellBoolRenderer_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellBoolEditor_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellBoolEditor_IsTrueValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellBoolEditor_UseStringValues(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellBoolEditor_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellFloatRenderer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellFloatRenderer_GetPrecision(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellFloatRenderer_GetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellFloatRenderer_SetParameters(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellFloatRenderer_SetPrecision(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellFloatRenderer_SetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellFloatRenderer_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellFloatEditor_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellFloatEditor_SetParameters(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellFloatEditor_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellStringRenderer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellStringRenderer_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellTextEditor_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellTextEditor_SetParameters(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellTextEditor_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellChoiceEditor_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellChoiceEditor_SetParameters(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellChoiceEditor_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellNumberRenderer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellNumberRenderer_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellNumberEditor_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellNumberEditor_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellNumberEditor_SetParameters(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellNumberEditor_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_SetAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_SetReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_SetRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_SetEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_HasTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_HasBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_HasFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_HasAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_HasRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_HasEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_GetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_GetAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_GetRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_GetEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_IsReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridCellAttr_SetDefAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_Blit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_CalcBoundingBox(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_CrossHair(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DestroyClippingRegion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DeviceToLogicalX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DeviceToLogicalXRel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DeviceToLogicalY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DeviceToLogicalYRel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawArc(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawCheckMark(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawCircle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawEllipse_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawEllipse_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawEllipticArc(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawPolygon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawRectangle_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawRectangle_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawRotatedText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawRoundedRectangle_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawRoundedRectangle_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_DrawText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_EndDoc(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_EndPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_FloodFill(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetBackgroundMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetCharHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetCharWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetClippingBox(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetLayoutDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetLogicalFunction(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetMapMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetMultiLineTextExtent_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetMultiLineTextExtent_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetPartialTextExtents(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetPen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetPixel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetPPI(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetSizeMM(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetTextBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetTextExtent_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetTextExtent_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetTextForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GetUserScale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GradientFillConcentric_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GradientFillConcentric_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_GradientFillLinear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_LogicalToDeviceX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_LogicalToDeviceXRel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_LogicalToDeviceY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_LogicalToDeviceYRel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_MaxX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_MaxY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_MinX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_MinY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_ResetBoundingBox(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetAxisOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetBackgroundMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetClippingRegion_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetClippingRegion_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetDeviceOrigin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetLayoutDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetLogicalFunction(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetMapMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetPalette(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetPen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetTextBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetTextForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_SetUserScale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_StartDoc(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDC_StartPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMirrorDC_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScreenDC_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if wxUSE_POSTSCRIPT
extern void wxPostScriptDC_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_POSTSCRIPT
#if wxUSE_POSTSCRIPT
extern void wxPostScriptDC_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_POSTSCRIPT
#if wxUSE_POSTSCRIPT
#endif // wxUSE_POSTSCRIPT
extern void wxWindowDC_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxClientDC_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPaintDC_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMemoryDC_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMemoryDC_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMemoryDC_SelectObject(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMemoryDC_SelectObjectAsSource(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBufferedDC_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBufferedDC_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBufferedDC_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBufferedDC_Init_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBufferedDC_Init_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBufferedPaintDC_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBufferedPaintDC_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if wxUSE_GRAPHICS_CONTEXT
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsObject_GetRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsObject_IsNull(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_Create_STAT_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_Create_STAT_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_CreatePen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_CreateBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_CreateRadialGradientBrush_7(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_CreateRadialGradientBrush_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_CreateLinearGradientBrush_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_CreateLinearGradientBrush_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_CreateFont_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_CreateFont_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_CreateMatrix(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_CreatePath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_Clip_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_Clip_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_ResetClip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_DrawBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_DrawEllipse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_DrawIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_DrawLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_DrawPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_DrawRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_DrawRoundedRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_DrawText_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_DrawText_4_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_DrawText_4_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_DrawText_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_FillPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_StrokePath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_GetPartialTextExtents(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_GetTextExtent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_Rotate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_Scale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_Translate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_GetTransform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_SetTransform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_ConcatTransform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_SetBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_SetFont_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_SetFont_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_SetPen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_StrokeLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsContext_StrokeLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsMatrix_Concat(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsMatrix_Get(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsMatrix_Invert(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsMatrix_IsEqual(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsMatrix_IsIdentity(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsMatrix_Rotate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsMatrix_Scale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsMatrix_Translate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsMatrix_Set(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsMatrix_TransformPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsMatrix_TransformDistance(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_MoveToPoint_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_MoveToPoint_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_AddArc_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_AddArc_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_AddArcToPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_AddCircle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_AddCurveToPoint_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_AddCurveToPoint_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_AddEllipse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_AddLineToPoint_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_AddLineToPoint_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_AddPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_AddQuadCurveToPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_AddRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_AddRoundedRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_CloseSubpath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_Contains_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_Contains_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_GetBox(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_GetCurrentPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsPath_Transform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsRenderer_GetDefaultRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsRenderer_CreateContext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsRenderer_CreateBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsRenderer_CreateLinearGradientBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsRenderer_CreateRadialGradientBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsRenderer_CreateFont_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsRenderer_CreateFont_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsRenderer_CreateMatrix(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsRenderer_CreatePath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsGradientStops_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsGradientStops_Item(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsGradientStops_GetCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsGradientStops_SetStartColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsGradientStops_GetStartColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsGradientStops_SetEndColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsGradientStops_GetEndColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGraphicsGradientStops_Add(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
#endif // wxUSE_GRAPHICS_CONTEXT
extern void wxMenuBar_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_Append(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_Check(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_Enable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_EnableTop(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_FindMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_FindMenuItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_FindItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_GetHelpString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_GetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_GetMenuLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_GetMenuLabelText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_GetMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_GetMenuCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_Insert(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_IsChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if defined(__WXMAC__)
extern void wxMenuBar_SetAutoWindowMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // defined(__WXMAC__)
#if defined(__WXMAC__)
extern void wxMenuBar_GetAutoWindowMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // defined(__WXMAC__)
#if defined(__WXMAC__)
extern void wxMenuBar_OSXGetAppleMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // defined(__WXMAC__)
#if defined(__WXMAC__)
extern void wxMenuBar_MacGetCommonMenuBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // defined(__WXMAC__)
#if defined(__WXMAC__)
extern void wxMenuBar_MacSetCommonMenuBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // defined(__WXMAC__)
extern void wxMenuBar_IsEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_Remove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_Replace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_SetHelpString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_SetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuBar_SetMenuLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControl_GetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControl_SetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_Append_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_Append_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_appendStrings_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_appendStrings_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_Delete(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_FindString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_getClientData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_setClientData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_GetCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_GetString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_GetStringSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_Insert_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_Insert_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_insertStrings_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_insertStrings_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_IsEmpty(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_Select(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_SetString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxControlWithItems_SetStringSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Append_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Append_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Append_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_AppendCheckItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_AppendRadioItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_AppendSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Break(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Check(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Delete_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Delete_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Destroy_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Destroy_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Enable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_FindItem_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_FindItem_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_FindItemByPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_GetHelpString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_GetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_GetMenuItemCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_GetMenuItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_GetTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Insert_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Insert_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Insert_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_InsertCheckItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_InsertRadioItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_InsertSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_IsChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_IsEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Prepend_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Prepend_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Prepend_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_PrependCheckItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_PrependRadioItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_PrependSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Remove_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_Remove_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_SetHelpString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_SetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenu_SetTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_Check(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_Enable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_GetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_GetHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_GetId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_GetKind(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_GetLabelText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_GetItemLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_GetItemLabelText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_GetMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_GetSubMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_IsCheckable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_IsChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_IsEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_IsSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_IsSubMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_SetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_SetHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_SetMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_SetSubMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuItem_SetItemLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_AddControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_AddSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_AddTool_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_AddTool_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_AddTool_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_AddCheckTool(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_AddRadioTool(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_AddStretchableSpace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_InsertStretchableSpace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_DeleteTool(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_DeleteToolByPos(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_EnableTool(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_FindById(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_FindControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_FindToolForPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_GetToolSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_GetToolBitmapSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_GetMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_GetToolEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_GetToolLongHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_GetToolPacking(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_GetToolPos(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_GetToolSeparation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_GetToolShortHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_GetToolState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_InsertControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_InsertSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_InsertTool_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_InsertTool_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_Realize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_RemoveTool(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_SetMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_SetToolBitmapSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_SetToolLongHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_SetToolPacking(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_SetToolShortHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_SetToolSeparation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolBar_ToggleTool(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStatusBar_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStatusBar_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStatusBar_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStatusBar_GetFieldRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStatusBar_GetFieldsCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStatusBar_GetStatusText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStatusBar_PopStatusText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStatusBar_PushStatusText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStatusBar_SetFieldsCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStatusBar_SetMinHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStatusBar_SetStatusText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStatusBar_SetStatusWidths(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStatusBar_SetStatusStyles(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_new_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_new_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_new_2_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_new_2_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_ConvertToImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_CopyFromIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_Create_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_Create_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_Create_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_GetDepth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_GetHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_GetPalette(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_GetMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_GetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_GetSubBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_LoadFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_SaveFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_SetDepth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_SetHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_SetMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_SetPalette(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmap_SetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIcon_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIcon_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIcon_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIcon_CopyFromBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIconBundle_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIconBundle_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIconBundle_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIconBundle_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIconBundle_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIconBundle_AddIcon_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIconBundle_AddIcon_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIconBundle_AddIcon_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIconBundle_GetIcon_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIconBundle_GetIcon_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCursor_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCursor_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCursor_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCursor_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCursor_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMask_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMask_new_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMask_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMask_new_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMask_Create_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMask_Create_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMask_Create_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_2_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_3_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_3_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Blur(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_BlurHorizontal(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_BlurVertical(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_ConvertAlphaToMask_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_ConvertAlphaToMask_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_ConvertToGreyscale_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_ConvertToGreyscale_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_ConvertToMono(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Copy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Create_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Create_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Create_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Create_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Create_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Create_3_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_FindFirstUnusedColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetImageExtWildcard(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetAlpha_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetAlpha_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetBlue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetGreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetImageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetMaskBlue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetMaskGreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetMaskRed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetOrFindMaskColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetPalette(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetRed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetSubImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_HasAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_HasMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetOption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_GetOptionInt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_HasOption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_InitAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_InitStandardHandlers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_IsTransparent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_LoadFile_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_LoadFile_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_RemoveHandler(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Mirror(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Replace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Rescale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Resize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Rotate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_RotateHue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Rotate90(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SaveFile_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SaveFile_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SaveFile_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Scale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Size(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SetAlpha_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SetAlpha_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SetData_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SetData_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SetMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SetMaskColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SetMaskFromImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SetOption_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SetOption_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SetPalette(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SetRGB_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SetRGB_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBrush_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBrush_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBrush_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBrush_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBrush_GetStipple(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBrush_GetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBrush_IsHatch(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBrush_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBrush_SetColour_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBrush_SetColour_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBrush_SetStipple(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBrush_SetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPen_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPen_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPen_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPen_GetCap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPen_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPen_GetJoin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPen_GetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPen_GetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPen_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPen_SetCap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPen_SetColour_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPen_SetColour_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPen_SetJoin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPen_SetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPen_SetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Contains_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Contains_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Contains_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Contains_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_ConvertToBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_GetBox(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Intersect_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Intersect_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Intersect_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_IsEmpty(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Subtract_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Subtract_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Offset_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Offset_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Union_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Union_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Union_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Union_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Xor_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Xor_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRegion_Xor_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAcceleratorTable_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAcceleratorTable_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAcceleratorTable_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAcceleratorEntry_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAcceleratorEntry_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAcceleratorEntry_GetCommand(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAcceleratorEntry_GetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAcceleratorEntry_GetKeyCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAcceleratorEntry_Set(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAcceleratorEntry_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_Create_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_Create_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_GetBlinkTime(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_GetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_GetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_Hide(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_IsVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_Move_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_Move_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_SetBlinkTime(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_SetSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_SetSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCaret_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Add_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Add_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Add_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Add_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_AddSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_AddStretchSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_CalcMin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Detach_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Detach_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Fit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_FitInside(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_GetChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_GetItem_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_GetItem_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_GetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_GetMinSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Hide_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Hide_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Insert_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Insert_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Insert_4_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Insert_4_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Insert_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_InsertSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_InsertStretchSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_IsShown_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_IsShown_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Layout(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Prepend_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Prepend_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Prepend_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Prepend_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Prepend_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_PrependSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_PrependStretchSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Remove_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Remove_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Replace_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Replace_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_SetDimension_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_SetDimension_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_SetMinSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_SetMinSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_SetItemMinSize_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_SetItemMinSize_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_SetItemMinSize_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_SetItemMinSize_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_SetSizeHints(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Show_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Show_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_Show_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizer_ShowItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerFlags_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerFlags_Align(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerFlags_Border_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerFlags_Border_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerFlags_Center(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerFlags_Expand(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerFlags_Left(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerFlags_Proportion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerFlags_Right(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerFlags_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_new_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_new_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_CalcMin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_DeleteWindows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_DetachSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_GetBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_GetFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_GetMinSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_GetProportion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_GetRatio(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_GetRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_GetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_GetSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_GetSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_GetUserData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_GetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_IsSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_IsShown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_IsSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_IsWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_SetBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_SetDimension(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_SetFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_SetInitSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_SetMinSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_SetMinSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_SetProportion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_SetRatio_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_SetRatio_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_SetRatio_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_AssignSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_AssignSpacer_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_AssignSpacer_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_AssignWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizerItem_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBoxSizer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBoxSizer_GetOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticBoxSizer_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticBoxSizer_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticBoxSizer_GetStaticBox(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridSizer_new_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridSizer_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridSizer_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridSizer_new_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridSizer_GetCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridSizer_GetHGap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridSizer_GetRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridSizer_GetVGap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridSizer_SetCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridSizer_SetHGap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridSizer_SetRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridSizer_SetVGap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFlexGridSizer_new_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFlexGridSizer_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFlexGridSizer_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFlexGridSizer_new_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFlexGridSizer_AddGrowableCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFlexGridSizer_AddGrowableRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFlexGridSizer_GetFlexibleDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFlexGridSizer_GetNonFlexibleGrowMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFlexGridSizer_RemoveGrowableCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFlexGridSizer_RemoveGrowableRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFlexGridSizer_SetFlexibleDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFlexGridSizer_SetNonFlexibleGrowMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_Add_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_Add_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_Add_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_CalcMin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_CheckForIntersection_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_CheckForIntersection_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_FindItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_FindItemAtPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_FindItemAtPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_FindItemWithData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_GetCellSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_GetEmptyCellSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_GetItemPosition_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_GetItemPosition_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_GetItemSpan_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_GetItemSpan_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_SetEmptyCellSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_SetItemPosition_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_SetItemPosition_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_SetItemSpan_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridBagSizer_SetItemSpan_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStdDialogButtonSizer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStdDialogButtonSizer_AddButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStdDialogButtonSizer_Realize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStdDialogButtonSizer_SetAffirmativeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStdDialogButtonSizer_SetCancelButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStdDialogButtonSizer_SetNegativeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_new_5_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_new_5_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_IsFixedWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_GetDefaultEncoding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_GetFaceName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_GetFamily(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_GetNativeFontInfoDesc(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_GetNativeFontInfoUserDesc(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_GetPointSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_GetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_GetUnderlined(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_GetWeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_SetDefaultEncoding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_SetFaceName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_SetFamily(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_SetPointSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_SetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_SetUnderlined(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFont_SetWeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolTip_Enable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolTip_SetDelay(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolTip_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolTip_SetTip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolTip_GetTip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolTip_GetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxButton_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxButton_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxButton_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxButton_GetDefaultSize_STAT_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if wxCHECK_VERSION(3,1,3)
extern void wxButton_GetDefaultSize_STAT_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxCHECK_VERSION(3,1,3)
extern void wxButton_SetDefault(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxButton_SetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxButton_GetBitmapDisabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxButton_GetBitmapFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxButton_GetBitmapLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxButton_SetBitmapDisabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxButton_SetBitmapFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxButton_SetBitmapLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmapButton_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmapButton_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmapButton_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmapButton_NewCloseButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToggleButton_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToggleButton_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToggleButton_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToggleButton_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToggleButton_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_SetDate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_GetDate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if !wxCHECK_VERSION(2,9,0)
extern void wxCalendarCtrl_EnableYearChange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // !wxCHECK_VERSION(2,9,0)
extern void wxCalendarCtrl_EnableMonthChange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_EnableHolidayDisplay(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_SetHeaderColours(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_GetHeaderColourFg(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_GetHeaderColourBg(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_SetHighlightColours(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_GetHighlightColourFg(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_GetHighlightColourBg(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_SetHolidayColours(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_GetHolidayColourFg(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_GetHolidayColourBg(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_GetAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_SetAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_SetHoliday(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_ResetAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarCtrl_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_SetBorderColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_SetBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_SetHoliday(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_HasTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_HasBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_HasBorderColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_HasFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_HasBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_IsHoliday(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_GetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_GetBorderColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_GetBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarDateAttr_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCheckBox_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCheckBox_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCheckBox_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCheckBox_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCheckBox_Get3StateValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCheckBox_Is3rdStateAllowedForUser(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCheckBox_Is3State(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCheckBox_IsChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCheckBox_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCheckBox_Set3StateValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCheckListBox_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCheckListBox_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCheckListBox_Check(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCheckListBox_IsChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoice_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoice_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoice_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoice_Delete(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoice_GetColumns(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoice_SetColumns(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_CanCopy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_CanCut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_CanPaste(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_CanRedo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_CanUndo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_Copy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_Cut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_GetInsertionPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_GetLastPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_Paste(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_Redo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_Replace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_Remove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_SetInsertionPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_SetInsertionPointEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_SetSelection_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_SetSelection_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxComboBox_Undo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGauge_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGauge_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGauge_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGauge_GetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGauge_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGauge_IsVertical(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGauge_SetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGauge_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGauge_Pulse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_Init(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_CollapseTree(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_ExpandPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_GetDefaultPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_GetPath_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_GetPath_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_GetFilePath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_GetFilter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_GetFilterIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_GetRootId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_GetTreeCtrl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_ReCreateTree(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_SetDefaultPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_SetFilter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_SetFilterIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGenericDirCtrl_SetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticBox_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticBox_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticBox_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticLine_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticLine_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticLine_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticLine_IsVertical(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticLine_GetDefaultSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListBox_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListBox_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListBox_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListBox_Deselect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListBox_GetSelections(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListBox_InsertItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListBox_IsSelected(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListBox_Set(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListBox_HitTest_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListBox_HitTest_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListBox_SetFirstItem_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListBox_SetFirstItem_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_Arrange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_ClearAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_DeleteAllItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_DeleteColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_DeleteItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_EditLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_EnsureVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_FindItem_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_FindItem_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetColumnCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetColumnWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetCountPerPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetItemBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetItemCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetItemData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetItemFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetItemPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetItemRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetItemSpacing(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetItemState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetItemText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetItemTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetNextItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetSelectedItemCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetTopItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_GetViewRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_InsertColumn_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_InsertColumn_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_InsertItem_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_InsertItem_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_InsertItem_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_InsertItem_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_RefreshItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_RefreshItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_ScrollList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetColumnWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetItem_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetItem_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetItemBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetItemCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetItemData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetItemFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetItemImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetItemColumnImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetItemPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetItemState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetItemText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetItemTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetSingleStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SetWindowStyleFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListCtrl_SortItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListView_ClearColumnImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListView_Focus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListView_GetFirstSelected(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListView_GetFocusedItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListView_GetNextSelected(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListView_IsSelected(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListView_Select(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListView_SetColumnImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_GetAlign(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_GetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_GetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_GetId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_GetImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_GetMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_GetState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_GetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_GetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_SetAlign(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_SetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_SetId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_SetImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_SetMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_SetState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_SetStateMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_SetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItem_SetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItemAttr_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItemAttr_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItemAttr_GetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItemAttr_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItemAttr_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItemAttr_HasBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItemAttr_HasFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItemAttr_HasTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItemAttr_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItemAttr_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItemAttr_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListItemAttr_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_Add_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_Add_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_Add_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_Draw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_GetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_GetIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_GetImageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_GetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_Remove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_RemoveAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_Replace_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_Replace_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_GetAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_GetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_GetFontEncoding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_GetFontFaceName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_GetFontSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_GetFontStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_GetFontUnderlined(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_GetFontWeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_GetLeftIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_GetLeftSubIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_GetRightIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_GetTabs(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_HasBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_HasFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_HasTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_GetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_IsDefault(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetFontEncoding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetFontFaceName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetFontFamily(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetFontSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetFontPointSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetFontPixelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetFontStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetFontUnderlined(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetFontWeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetLeftIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetRightIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetTabs(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextAttr_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_AppendText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_CanCopy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_CanCut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_CanPaste(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_CanRedo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_CanUndo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_Copy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_Cut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_DiscardEdits(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_ChangeValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_EmulateKeyPress(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_GetDefaultStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_GetInsertionPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_GetLastPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_GetLineLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_GetLineText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_GetNumberOfLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_GetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_GetStringSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_GetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_IsEditable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_IsModified(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_IsMultiLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_IsSingleLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_LoadFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_MarkDirty(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_Paste(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_PositionToXY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_Redo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_Remove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_Replace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_SaveFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_SetDefaultStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_SetEditable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_SetInsertionPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_SetInsertionPointEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_SetMaxLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_SetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_ShowPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_Undo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_WriteText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextCtrl_XYToPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlBase_AddPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlBase_InsertPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlBase_DeletePage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlBase_RemovePage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlBase_DeleteAllPages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlBase_GetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlBase_GetPageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlBase_GetCurrentPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlBase_AdvanceSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlBase_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlBase_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlBase_ChangeSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlBase_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlBase_GetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlBase_SetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotebook_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotebook_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotebook_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotebook_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotebook_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotebook_GetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotebook_GetRowCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotebook_GetThemeBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotebook_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotebook_SetPadding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotebook_SetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotebook_SetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_AddPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_AdvanceSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_DeleteAllPages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_GetCurrentPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_GetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_GetPageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_GetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_GetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_InsertPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_SetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_SetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_SetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChoicebook_ChangeSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_AddPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_AdvanceSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_DeleteAllPages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_GetCurrentPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_GetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_GetPageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_GetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_GetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_InsertPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_SetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_SetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_SetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxToolbook_ChangeSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_AddPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_AdvanceSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_DeleteAllPages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_GetCurrentPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_GetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_GetPageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_GetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_GetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_InsertPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_SetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_SetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_SetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListbook_ChangeSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_AddPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_AdvanceSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_DeleteAllPages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_GetCurrentPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_GetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_GetPageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_GetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_GetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_ExpandNode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_IsNodeExpanded(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_InsertPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_InsertSubPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_SetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_SetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_SetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreebook_ChangeSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_AddRoot(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_AppendItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_AssignStateImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_Collapse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_CollapseAndReset(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_Delete(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_DeleteAllItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_DeleteChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_EditLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_EnsureVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_Expand(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetBoundingRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetChildrenCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetFirstChild(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetNextChild(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetFirstVisibleItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetItemBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetItemData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetItemFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetItemImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetItemText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetItemTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetLastChild(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetNextSibling(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetNextVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetItemParent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetPrevSibling(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetPrevVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetRootItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetSelections(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_GetStateImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_InsertItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_IsBold(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_IsExpanded(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_IsSelected(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_IsVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_ItemHasChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_IsTreeItemIdOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_PrependItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_ScrollTo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_SelectItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_SetIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_SetItemBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_SetItemBold(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_SetItemData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_SetItemDropHighlight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_SetItemFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_SetItemHasChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_SetItemImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_SetItemText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_SetItemTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_SetStateImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_SetWindowStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_SortChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_Toggle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_ToggleItemSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_Unselect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_UnselectAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeCtrl_UnselectItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrollBar_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrollBar_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrollBar_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrollBar_GetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrollBar_GetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrollBar_GetThumbPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrollBar_GetThumbSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrollBar_SetThumbPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrollBar_SetScrollbar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinButton_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinButton_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinButton_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinButton_GetMax(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinButton_GetMin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinButton_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinButton_SetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinButton_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinCtrl_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinCtrl_SetValue_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinCtrl_SetValue_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinCtrl_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinCtrl_SetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinCtrl_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinCtrl_GetMin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinCtrl_GetMax(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticText_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticText_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticText_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticText_GetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticText_SetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticText_Wrap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticBitmap_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticBitmap_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticBitmap_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticBitmap_GetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStaticBitmap_SetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_Enable_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_Enable_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_GetString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_GetColumnCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_GetItemHelpText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_GetItemToolTip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_GetItemFromPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_GetRowCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_IsItemEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_IsItemShown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_SetItemHelpText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioBox_SetItemToolTip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioButton_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioButton_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioButton_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioButton_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxRadioButton_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSlider_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSlider_new_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSlider_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSlider_GetLineSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSlider_GetMax(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSlider_GetMin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSlider_GetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSlider_GetThumbLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSlider_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSlider_SetLineSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSlider_SetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSlider_SetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSlider_SetThumbLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSlider_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDialog_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDialog_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDialog_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDialog_CreateButtonSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDialog_CreateStdDialogButtonSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDialog_EndModal(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDialog_GetAffirmativeId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDialog_GetReturnCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDialog_IsModal(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDialog_SetAffirmativeId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDialog_SetReturnCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDialog_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDialog_ShowModal(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourDialog_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourDialog_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourDialog_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourDialog_GetColourData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourData_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourData_GetChooseFull(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourData_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourData_GetCustomColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourData_SetChooseFull(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourData_SetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourData_SetCustomColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPalette_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPalette_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPalette_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPalette_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPalette_GetColoursCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPalette_GetPixel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPalette_GetRGB(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPalette_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDirDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDirDialog_GetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDirDialog_GetMessage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDirDialog_SetMessage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDirDialog_SetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDialog_GetDirectory(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDialog_GetFilename(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDialog_GetFilenames(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDialog_GetFilterIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDialog_GetMessage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDialog_GetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDialog_GetPaths(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDialog_GetWildcard(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDialog_SetDirectory(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDialog_SetFilename(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDialog_SetFilterIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDialog_SetMessage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDialog_SetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDialog_SetWildcard(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPickerBase_SetInternalMargin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPickerBase_GetInternalMargin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPickerBase_SetTextCtrlProportion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPickerBase_SetPickerCtrlProportion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPickerBase_GetTextCtrlProportion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPickerBase_GetPickerCtrlProportion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPickerBase_HasTextCtrl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPickerBase_GetTextCtrl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPickerBase_IsTextCtrlGrowable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPickerBase_SetPickerCtrlGrowable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPickerBase_SetTextCtrlGrowable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPickerBase_IsPickerCtrlGrowable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFilePickerCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFilePickerCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFilePickerCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFilePickerCtrl_GetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFilePickerCtrl_SetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDirPickerCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDirPickerCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDirPickerCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDirPickerCtrl_GetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDirPickerCtrl_SetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourPickerCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourPickerCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourPickerCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourPickerCtrl_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourPickerCtrl_SetColour_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourPickerCtrl_SetColour_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDatePickerCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDatePickerCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDatePickerCtrl_GetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDatePickerCtrl_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDatePickerCtrl_SetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDatePickerCtrl_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontPickerCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontPickerCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontPickerCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontPickerCtrl_GetSelectedFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontPickerCtrl_SetSelectedFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontPickerCtrl_GetMaxPointSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontPickerCtrl_SetMaxPointSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFindReplaceDialog_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFindReplaceDialog_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFindReplaceDialog_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFindReplaceDialog_GetData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFindReplaceData_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFindReplaceData_GetFindString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFindReplaceData_GetReplaceString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFindReplaceData_GetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFindReplaceData_SetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFindReplaceData_SetFindString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFindReplaceData_SetReplaceString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMultiChoiceDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMultiChoiceDialog_GetSelections(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMultiChoiceDialog_SetSelections(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSingleChoiceDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSingleChoiceDialog_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSingleChoiceDialog_GetStringSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSingleChoiceDialog_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextEntryDialog_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextEntryDialog_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextEntryDialog_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextEntryDialog_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPasswordEntryDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontData_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontData_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontData_EnableEffects(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontData_GetAllowSymbols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontData_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontData_GetChosenFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontData_GetEnableEffects(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontData_GetInitialFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontData_GetShowHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontData_SetAllowSymbols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontData_SetChosenFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontData_SetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontData_SetInitialFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontData_SetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontData_SetShowHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontDialog_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontDialog_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontDialog_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontDialog_GetFontData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxProgressDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxProgressDialog_Resume(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxProgressDialog_Update(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMessageDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialog_GetPageSetupData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialog_ShowModal(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_EnableHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_EnableMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_EnableOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_EnablePaper(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_EnablePrinter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_GetDefaultMinMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_GetEnableMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_GetEnableOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_GetEnablePaper(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_GetEnablePrinter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_GetEnableHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_GetDefaultInfo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_GetMarginTopLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_GetMarginBottomRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_GetMinMarginTopLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_GetMinMarginBottomRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_GetPaperId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_GetPaperSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_GetPrintData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_SetDefaultInfo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_SetDefaultMinMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_SetMarginTopLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_SetMarginBottomRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_SetMinMarginTopLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_SetMinMarginBottomRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_SetPaperId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_SetPaperSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPageSetupDialogData_SetPrintData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialog_new_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialog_new_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialog_GetPrintDialogData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialog_GetPrintDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_EnableHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_EnablePageNumbers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_EnablePrintToFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_EnableSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_GetAllPages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_GetCollate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_GetFromPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_GetMaxPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_GetMinPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_GetNoCopies(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_GetPrintData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_GetPrintToFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_GetToPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_SetCollate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_SetFromPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_SetMaxPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_SetMinPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_SetNoCopies(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_SetPrintData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_SetPrintToFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintDialogData_SetToPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_GetCollate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_GetBin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_GetDuplex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_GetNoCopies(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_GetOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_GetPaperId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_GetPrinterName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_GetQuality(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_SetBin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_SetCollate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_SetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_SetDuplex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_SetNoCopies(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_SetOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_SetPaperId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_SetPrinterName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintData_SetQuality(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_GetCanvas(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_GetCurrentPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_GetFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_GetMaxPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_GetMinPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_GetPrintout(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_GetPrintoutForPrinting(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_PaintPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_Print(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_RenderPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_SetCanvas(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_SetCurrentPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_SetFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_SetPrintout(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintPreview_SetZoom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPreviewFrame_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPreviewFrame_CreateControlBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPreviewFrame_CreateCanvas(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPreviewFrame_Initialize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPreviewFrame_OnCloseWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPreviewControlBar_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPreviewControlBar_CreateButtons(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPreviewControlBar_GetPrintPreview(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPreviewControlBar_GetZoomControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPreviewControlBar_SetZoomControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrinter_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrinter_CreateAbortWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrinter_GetAbort(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrinter_GetLastError(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrinter_GetPrintDialogData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrinter_Print(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrinter_PrintDialog(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrinter_ReportError(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrinter_Setup(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_AttachUnknownControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_ClearHandlers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_CompareVersion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_Get(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_GetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_GetVersion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_GetXRCID(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_InitAllHandlers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_Load(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_LoadBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_LoadDialog_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_LoadDialog_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_LoadFrame_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_LoadFrame_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_LoadIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_LoadMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_LoadMenuBar_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_LoadMenuBar_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_LoadPanel_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_LoadPanel_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_LoadToolBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_Set(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_SetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxXmlResource_Unload(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlEasyPrinting_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlEasyPrinting_GetPrintData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlEasyPrinting_GetPageSetupData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlEasyPrinting_PreviewFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlEasyPrinting_PreviewText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlEasyPrinting_PrintFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlEasyPrinting_PrintText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlEasyPrinting_PageSetup(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlEasyPrinting_SetFonts(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlEasyPrinting_SetHeader(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlEasyPrinting_SetFooter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if wxUSE_GLCANVAS
extern void wxGLCanvas_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GLCANVAS
#if wxUSE_GLCANVAS
extern void wxGLCanvas_SetCurrent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GLCANVAS
#if wxUSE_GLCANVAS && wxUSE_GLCANVAS_EGL
extern void wxGLCanvas_CreateSurface(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GLCANVAS && wxUSE_GLCANVAS_EGL
#if wxUSE_GLCANVAS
extern void wxGLCanvas_IsDisplaySupported(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GLCANVAS
#if wxUSE_GLCANVAS
extern void wxGLCanvas_SwapBuffers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GLCANVAS
#if wxUSE_GLCANVAS
#endif // wxUSE_GLCANVAS
#if wxUSE_GLCANVAS
extern void wxGLContext_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GLCANVAS
#if wxUSE_GLCANVAS
extern void wxGLContext_SetCurrent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GLCANVAS
#if wxUSE_GLCANVAS && wxCHECK_VERSION(3,1,0)
extern void wxGLContext_IsOK(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GLCANVAS && wxCHECK_VERSION(3,1,0)
#if wxUSE_GLCANVAS
#endif // wxUSE_GLCANVAS
#if wxUSE_AUI
extern void wxAuiManager_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_AddPane_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_AddPane_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_AddPane_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_DetachPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_GetAllPanes(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_GetArtProvider(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_GetDockSizeConstraint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_GetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_GetManagedWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_GetManager(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_GetPane_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_GetPane_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_HideHint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_InsertPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_LoadPaneInfo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_LoadPerspective(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_SavePaneInfo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_SavePerspective(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_SetArtProvider(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_SetDockSizeConstraint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_SetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_SetManagedWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_ShowHint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_UnInit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiManager_Update(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_BestSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_BestSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Bottom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_BottomDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Caption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_CaptionVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Centre(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_CentrePane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_CloseButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_DefaultPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_DestroyOnClose(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Direction(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Dock(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Dockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Fixed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Float(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Floatable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_FloatingPosition_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_FloatingPosition_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_FloatingSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_FloatingSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Gripper(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_GripperTop(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_HasBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_HasCaption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_HasCloseButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_HasFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_HasGripper(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_HasGripperTop(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_HasMaximizeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_HasMinimizeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_HasPinButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Hide(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_IsBottomDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_IsDocked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_IsFixed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_IsFloatable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_IsFloating(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_IsLeftDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_IsMovable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_IsResizable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_IsRightDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_IsShown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_IsToolbar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_IsTopDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Layer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Left(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_LeftDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_MaxSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_MaxSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_MaximizeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_MinSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_MinSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_MinimizeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Movable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Name(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_PaneBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_PinButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Position(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Resizable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Right(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_RightDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Row(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_SafeSet(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_SetFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_ToolbarPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Top(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_TopDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_Window(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_GetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_GetFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_GetDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_GetLayer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_GetRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_GetFloatingPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_GetFloatingSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiPaneInfo_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_AddPage_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_AddPage_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_Create_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_Create_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_DeletePage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_GetArtProvider(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_GetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_GetPageBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_GetPageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_GetPageIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_GetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_InsertPage_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_InsertPage_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_RemovePage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_SetArtProvider(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_SetPageBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_SetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_SetTabCtrlHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiNotebook_SetUniformBitmapSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiTabArt_SetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiTabArt_SetMeasuringFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiTabArt_SetNormalFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiTabArt_SetSelectedFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiTabArt_SetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiTabArt_SetActiveColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiDockArt_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiDockArt_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiDockArt_GetMetric(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiDockArt_SetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiDockArt_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiDockArt_SetMetric(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiSimpleTabArt_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
#if wxUSE_AUI
extern void wxAuiSimpleTabArt_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_AUI
extern void wxMDIParentFrame_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIParentFrame_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIParentFrame_ActivateNext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIParentFrame_ActivatePrevious(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIParentFrame_ArrangeIcons(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIParentFrame_Cascade(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIParentFrame_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIParentFrame_GetActiveChild(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIParentFrame_GetClientWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIParentFrame_Tile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIChildFrame_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIChildFrame_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIChildFrame_Activate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIChildFrame_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIChildFrame_Maximize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIChildFrame_Restore(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIClientWindow_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMDIClientWindow_CreateClient(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLayoutAlgorithm_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLayoutAlgorithm_LayoutFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLayoutAlgorithm_LayoutMDIFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLayoutAlgorithm_LayoutWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxEvent_GetId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxEvent_GetSkipped(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxEvent_GetTimestamp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxEvent_IsCommandEvent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxEvent_ResumePropagation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxEvent_ShouldPropagate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxEvent_Skip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxEvent_StopPropagation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCommandEvent_getClientData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCommandEvent_GetExtraLong(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCommandEvent_GetInt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCommandEvent_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCommandEvent_GetString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCommandEvent_IsChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCommandEvent_IsSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCommandEvent_SetInt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCommandEvent_SetString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrollEvent_GetOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrollEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrollWinEvent_GetOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxScrollWinEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_AltDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_Button(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_ButtonDClick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_ButtonDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_ButtonUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_CmdDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_ControlDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_Dragging(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_Entering(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_GetButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_GetLogicalPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_GetLinesPerAction(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_GetWheelRotation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_GetWheelDelta(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_GetX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_GetY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_IsButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_IsPageScroll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_Leaving(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_LeftDClick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_LeftDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_LeftIsDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_LeftUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_MetaDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_MiddleDClick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_MiddleDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_MiddleIsDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_MiddleUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_Moving(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_RightDClick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_RightDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_RightIsDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_RightUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_ShiftDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_GetWheelAxis(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_Aux1DClick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_Aux1Down(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_Aux1Up(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_Aux2DClick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_Aux2Down(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseEvent_Aux2Up(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSetCursorEvent_GetCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSetCursorEvent_GetX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSetCursorEvent_GetY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSetCursorEvent_HasCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSetCursorEvent_SetCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxKeyEvent_AltDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxKeyEvent_CmdDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxKeyEvent_ControlDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxKeyEvent_GetKeyCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxKeyEvent_GetModifiers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxKeyEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxKeyEvent_GetRawKeyCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxKeyEvent_GetRawKeyFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxKeyEvent_GetUnicodeKey(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxKeyEvent_GetX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxKeyEvent_GetY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxKeyEvent_HasModifiers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxKeyEvent_MetaDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxKeyEvent_ShiftDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizeEvent_GetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSizeEvent_GetRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMoveEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMoveEvent_GetRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxEraseEvent_GetDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFocusEvent_GetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxChildFocusEvent_GetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuEvent_GetMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuEvent_GetMenuId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMenuEvent_IsPopup(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCloseEvent_CanVeto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCloseEvent_GetLoggingOff(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCloseEvent_SetCanVeto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCloseEvent_SetLoggingOff(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCloseEvent_Veto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxShowEvent_SetShow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxShowEvent_IsShown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIconizeEvent_IsIconized(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxJoystickEvent_ButtonDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxJoystickEvent_ButtonIsDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxJoystickEvent_ButtonUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxJoystickEvent_GetButtonChange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxJoystickEvent_GetButtonState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxJoystickEvent_GetJoystick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxJoystickEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxJoystickEvent_GetZPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxJoystickEvent_IsButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxJoystickEvent_IsMove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxJoystickEvent_IsZMove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_CanUpdate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_Check(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_Enable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_GetChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_GetEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_GetShown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_GetSetChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_GetSetEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_GetSetShown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_GetSetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_GetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_GetMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_GetUpdateInterval(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_ResetUpdateTime(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_SetMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_SetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxUpdateUIEvent_SetUpdateInterval(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxMouseCaptureChangedEvent_GetCapturedWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPaletteChangedEvent_SetChangedWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPaletteChangedEvent_GetChangedWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxQueryNewPaletteEvent_SetPaletteRealized(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxQueryNewPaletteEvent_GetPaletteRealized(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNavigationKeyEvent_GetDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNavigationKeyEvent_SetDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNavigationKeyEvent_IsWindowChange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNavigationKeyEvent_SetWindowChange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNavigationKeyEvent_IsFromTab(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNavigationKeyEvent_SetFromTab(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNavigationKeyEvent_GetCurrentFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNavigationKeyEvent_SetCurrentFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHelpEvent_GetOrigin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHelpEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHelpEvent_SetOrigin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHelpEvent_SetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxContextMenuEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxContextMenuEvent_SetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIdleEvent_GetMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIdleEvent_RequestMore(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIdleEvent_MoreRequested(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxIdleEvent_SetMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridEvent_AltDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridEvent_ControlDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridEvent_GetCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridEvent_GetRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridEvent_MetaDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridEvent_Selecting(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGridEvent_ShiftDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotifyEvent_Allow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotifyEvent_IsAllowed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotifyEvent_Veto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashEvent_GetEdge(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashEvent_GetDragRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSashEvent_GetDragStatus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListEvent_GetCacheFrom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListEvent_GetCacheTo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListEvent_GetKeyCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListEvent_GetIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListEvent_GetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListEvent_GetPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListEvent_GetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListEvent_GetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListEvent_GetImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListEvent_GetData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListEvent_GetMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListEvent_GetItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxListEvent_IsEditCancelled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDateEvent_GetDate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarEvent_GetWeekDay(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxCalendarEvent_GetDate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDirPickerEvent_GetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxColourPickerEvent_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFontPickerEvent_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetKey(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetModifiers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetModificationType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetLinesAdded(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetFoldLevelNow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetFoldLevelPrev(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetMargin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetMessage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetWParam(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetLParam(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetListType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetDragText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetDragAllowMove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetDragResult(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetShift(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextEvent_GetAlt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxGetKeyState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxGetMousePosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxGetMouseState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxSetDetectableAutoRepeat(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxBell(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxFindMenuItemId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxFindWindowAtPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxBeginBusyCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxEndBusyCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxIsBusy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxShutdown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxShell(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxLaunchDefaultBrowser(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxGetEmailAddress(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxGetUserId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxGetHomeDir(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxNewId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxRegisterId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxGetCurrentId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxGetOsDescription(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxIsPlatformLittleEndian(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void utils_wxIsPlatform64Bit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void gdicmn_wxDisplaySize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void gdicmn_wxSetCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_GetDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_GetPageSizeMM(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_GetPageSizePixels(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_GetPaperRectPixels(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_GetPPIPrinter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_GetPPIScreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_GetTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_IsPreview(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_FitThisSizeToPaper(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_FitThisSizeToPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_FitThisSizeToPageMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_MapScreenSizeToPaper(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_MapScreenSizeToPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_MapScreenSizeToPageMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_MapScreenSizeToDevice(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_GetLogicalPaperRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_GetLogicalPageRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_GetLogicalPageMarginsRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_SetLogicalOrigin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPrintout_OffsetLogicalOrigin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AddText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_InsertText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ClearAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ClearDocumentStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetCharAt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetCurrentPos(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetAnchor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetStyleAt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_Redo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetUndoCollection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SelectAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetSavePoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CanRedo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_MarkerLineFromHandle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_MarkerDeleteHandle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetUndoCollection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetViewWhiteSpace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetViewWhiteSpace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_PositionFromPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_PositionFromPointClose(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GotoLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GotoPos(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetAnchor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetCurLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetEndStyled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ConvertEOLs(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetEOLMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetEOLMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StartStyling(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetStyling(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetBufferedDraw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetBufferedDraw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetTabWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetTabWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetCodePage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_MarkerDefine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_MarkerSetForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_MarkerSetBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_MarkerAdd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_MarkerDelete(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_MarkerDeleteAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_MarkerGet(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_MarkerNext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_MarkerPrevious(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_MarkerDefineBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_MarkerAddSet(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_MarkerSetAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetMarginType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetMarginType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetMarginWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetMarginWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetMarginMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetMarginMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetMarginSensitive(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetMarginSensitive(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleClearAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetBold(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetItalic(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetFaceName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetEOLFilled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleResetDefault(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetUnderline(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetCase(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetHotSpot(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetSelForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetSelBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetSelAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetSelAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetCaretForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CmdKeyAssign(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CmdKeyClear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CmdKeyClearAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetStyleBytes(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetCaretPeriod(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetCaretPeriod(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetWordChars(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_BeginUndoAction(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_EndUndoAction(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_IndicatorSetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_IndicatorGetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_IndicatorSetForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_IndicatorGetForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetWhitespaceForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetWhitespaceBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetStyleBits(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetLineState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetLineState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetMaxLineState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetCaretLineVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetCaretLineVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetCaretLineBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetCaretLineBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompShow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompCancel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompActive(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompPosStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompComplete(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompStops(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompSetSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompGetSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompSelect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompSetCancelAtStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompGetCancelAtStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompSetFillUps(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompSetChooseSingle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompGetChooseSingle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompSetIgnoreCase(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompGetIgnoreCase(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_UserListShow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompSetAutoHide(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompGetAutoHide(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompSetDropRestOfWord(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompGetDropRestOfWord(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_RegisterImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ClearRegisteredImages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompGetTypeSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompSetTypeSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompSetMaxWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompGetMaxWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompSetMaxHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompGetMaxHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetUseTabs(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetUseTabs(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetLineIndentation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetLineIndentation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetLineIndentPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetUseHorizontalScrollBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetUseHorizontalScrollBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetIndentationGuides(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetIndentationGuides(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetHighlightGuide(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetHighlightGuide(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetLineEndPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetCodePage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetCaretForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetCurrentPos(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetSelectionStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetSelectionStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetSelectionEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetSelectionEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetPrintMagnification(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetPrintMagnification(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetPrintColourMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetPrintColourMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_FindText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_FormatRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetFirstVisibleLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetLineCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetMarginLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetMarginLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetMarginRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetMarginRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetModify(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetSelectedText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetTextRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_HideSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineFromPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_PositionFromLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineScroll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_EnsureCaretVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ReplaceSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CanPaste(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CanUndo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_EmptyUndoBuffer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_Undo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_Cut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_Copy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_Paste(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetTextLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetOvertype(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetCaretWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetCaretWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetTargetStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetTargetStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetTargetEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetTargetEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ReplaceTarget(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SearchInTarget(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetSearchFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetSearchFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CallTipShow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CallTipCancel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CallTipActive(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CallTipPosAtStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CallTipSetHighlight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CallTipSetBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CallTipSetForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CallTipSetForegroundHighlight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CallTipUseStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_VisibleFromDocLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_DocLineFromVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_WrapCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetFoldLevel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetFoldLevel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetLastChild(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetFoldParent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ShowLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_HideLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetLineVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetFoldExpanded(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetFoldExpanded(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ToggleFold(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_EnsureVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetFoldFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_EnsureVisibleEnforcePolicy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetTabIndents(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetTabIndents(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetBackSpaceUnIndents(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetBackSpaceUnIndents(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetMouseDwellTime(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetMouseDwellTime(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_WordStartPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_WordEndPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetWrapMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetWrapMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetWrapVisualFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetWrapVisualFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetWrapVisualFlagsLocation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetWrapVisualFlagsLocation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetWrapStartIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetWrapStartIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetLayoutCache(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetLayoutCache(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetScrollWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetScrollWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_TextWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetEndAtLastLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_TextHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetUseVerticalScrollBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetUseVerticalScrollBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AppendText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetTwoPhaseDraw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetTwoPhaseDraw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_TargetFromSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LinesJoin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LinesSplit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetFoldMarginColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetFoldMarginHiColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineDownExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineUpExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CharLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CharLeftExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CharRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CharRightExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_WordLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_WordLeftExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_WordRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_WordRightExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_Home(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_HomeExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineEndExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_DocumentStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_DocumentStartExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_DocumentEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_DocumentEndExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_PageUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_PageUpExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_PageDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_PageDownExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_EditToggleOvertype(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_Cancel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_DeleteBack(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_Tab(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_BackTab(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_NewLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_FormFeed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_VCHome(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_VCHomeExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ZoomIn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ZoomOut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_DelWordLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_DelWordRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineCut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineDelete(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineTranspose(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineDuplicate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LowerCase(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_UpperCase(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineScrollDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineScrollUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_DeleteBackNotLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_HomeDisplay(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_HomeDisplayExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineEndDisplay(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineEndDisplayExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_HomeWrapExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineEndWrap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineEndWrapExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_VCHomeWrap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_VCHomeWrapExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineCopy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_MoveCaretInsideView(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_BraceHighlight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_BraceBadLight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_BraceMatch(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetViewEOL(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetViewEOL(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetModEventMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetEdgeColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetEdgeColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetEdgeMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetEdgeMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetEdgeColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetEdgeColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SearchAnchor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SearchNext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SearchPrev(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LinesOnScreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_UsePopUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SelectionIsRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetZoom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetZoom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetModEventMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetSTCFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetSTCFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetStatus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetStatus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetMouseDownCaptures(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetMouseDownCaptures(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetSTCCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetSTCCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetControlCharSymbol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetControlCharSymbol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_WordPartLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_WordPartLeftExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_WordPartRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_WordPartRightExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetVisiblePolicy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_DelLineLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_DelLineRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetXOffset(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ChooseCaretX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetXCaretPolicy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetYCaretPolicy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetPrintWrapMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetHotspotActiveForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetHotspotActiveBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetHotspotActiveUnderline(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetHotspotSingleLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ParaDownExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ParaUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ParaUpExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_PositionBefore(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_PositionAfter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CopyRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CopyText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetSelectionMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetSelectionMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineDownRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineUpRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CharLeftRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CharRightRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_HomeRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_VCHomeRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LineEndRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_PageUpRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_PageDownRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StutteredPageUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StutteredPageUpExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StutteredPageDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StutteredPageDownExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_WordLeftEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_WordLeftEndExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_WordRightEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_WordRightEndExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetWhitespaceChars(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetCharsDefault(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AutoCompGetCurrent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_Allocate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_FindColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetCaretSticky(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetCaretSticky(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ToggleCaretSticky(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetPasteConvertEndings(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetPasteConvertEndings(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SelectionDuplicate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetCaretLineBackAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetCaretLineBackAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StartRecord(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StopRecord(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetLexer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetLexer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_Colourise(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetProperty(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetKeyWords(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetLexerLanguage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetProperty(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetStyleBitsNeeded(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetCurrentLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetSpec(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetFontAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetCharacterSet(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_StyleSetFontEncoding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_CmdKeyExecute(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_PointFromPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ScrollToLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_ScrollToColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetVScrollBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetHScrollBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetLastKeydownProcessed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetLastKeydownProcessed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SaveFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_LoadFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_DoDragOver(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_DoDropText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetUseAntiAliasing(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AddTextRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_InsertTextRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetCurLineRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetLineRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetSelectedTextRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetTextRangeRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_SetTextRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_GetTextRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxStyledTextCtrl_AppendTextRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxArtProvider_GetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxArtProvider_GetIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeEvent_GetKeyCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeEvent_GetItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeEvent_GetKeyEvent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeEvent_GetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeEvent_GetOldItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeEvent_GetPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeEvent_IsEditCancelled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTreeEvent_SetToolTip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlEvent_GetOldSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlEvent_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlEvent_SetOldSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBookCtrlEvent_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDataObject_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDataObject_AddFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDataObject_GetFilenames(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxFileDataObject_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextDataObject_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextDataObject_GetTextLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextDataObject_GetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextDataObject_SetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTextDataObject_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmapDataObject_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmapDataObject_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmapDataObject_GetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmapDataObject_SetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxBitmapDataObject_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxClipboard_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxClipboard_AddData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxClipboard_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxClipboard_Close(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxClipboard_Flush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxClipboard_GetData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxClipboard_IsOpened(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxClipboard_Open(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxClipboard_SetData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxClipboard_UsePrimarySelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxClipboard_IsSupported(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxClipboard_Get(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSpinEvent_SetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_GetMinimumPaneSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_GetSashGravity(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_GetSashPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_GetSplitMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_GetWindow1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_GetWindow2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_Initialize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_IsSplit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_ReplaceWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_SetSashGravity(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_SetSashPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_SetMinimumPaneSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_SetSplitMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_SplitHorizontally(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_SplitVertically(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_Unsplit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterWindow_UpdateSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterEvent_GetSashPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterEvent_GetX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterEvent_GetY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterEvent_GetWindowBeingRemoved(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSplitterEvent_SetSashPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_AppendToPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_GetOpenedAnchor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_GetOpenedPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_GetOpenedPageTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_GetRelatedFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_HistoryBack(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_HistoryCanBack(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_HistoryCanForward(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_HistoryClear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_HistoryForward(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_LoadFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_LoadPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_SelectAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_SelectionToText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_SelectLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_SelectWord(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_SetBorders(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_SetFonts(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_SetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_SetRelatedFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_SetRelatedStatusBar_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_SetRelatedStatusBar_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlWindow_ToText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxHtmlLinkEvent_GetLinkInfo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSystemSettings_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSystemSettings_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSystemSettings_GetMetric(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSystemSettings_GetScreenType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSystemOptions_GetOption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSystemOptions_GetOptionInt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSystemOptions_HasOption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSystemOptions_IsFalse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSystemOptions_SetOption_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxSystemOptions_SetOption_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebookEvent_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebookEvent_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebookEvent_SetOldSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebookEvent_GetOldSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebookEvent_SetDragSource(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebookEvent_GetDragSource(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManagerEvent_SetManager(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManagerEvent_GetManager(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManagerEvent_SetPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManagerEvent_GetPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManagerEvent_SetButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManagerEvent_GetButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManagerEvent_SetDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManagerEvent_GetDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManagerEvent_Veto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManagerEvent_GetVeto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManagerEvent_SetCanVeto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManagerEvent_CanVeto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLogNull_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLogNull_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTaskBarIcon_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTaskBarIcon_PopupMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTaskBarIcon_RemoveIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxTaskBarIcon_SetIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_new_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_new_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_Init_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_Init_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_AddCatalog_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_AddCatalog_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_AddCatalog_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_AddCatalogLookupPathPrefix(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_GetCanonicalName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_GetLanguage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_GetLanguageName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_GetLocale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_GetName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_GetString_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_GetString_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_GetHeaderValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_GetSysName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_GetSystemEncoding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_GetSystemEncodingName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_GetSystemLanguage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_IsLoaded(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxLocale_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxActivateEvent_GetActive(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if wxUSE_POPUPWIN
extern void wxPopupWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
extern void wxPopupWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
extern void wxPopupWindow_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
extern void wxPopupWindow_Position(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
extern void wxPopupTransientWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
extern void wxPopupTransientWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
extern void wxPopupTransientWindow_Popup(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
extern void wxPopupTransientWindow_Dismiss(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
#endif // wxUSE_POPUPWIN
extern void wxOverlay_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxOverlay_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxOverlay_Reset(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDCOverlay_new_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDCOverlay_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDCOverlay_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDCOverlay_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDropFilesEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDropFilesEvent_GetNumberOfFiles(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDropFilesEvent_GetFiles(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if wxUSE_DISPLAY
extern void wxDisplay_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
extern void wxDisplay_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY && wxCHECK_VERSION(3,1,3)
extern void wxDisplay_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_DISPLAY && wxCHECK_VERSION(3,1,3)
#if wxUSE_DISPLAY
extern void wxDisplay_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
extern void wxDisplay_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
extern void wxDisplay_GetClientArea(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
extern void wxDisplay_GetGeometry(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
extern void wxDisplay_GetName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
extern void wxDisplay_IsPrimary(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
extern void wxDisplay_GetCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
extern void wxDisplay_GetFromPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
extern void wxDisplay_GetFromWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY && wxCHECK_VERSION(3,1,2)
extern void wxDisplay_GetPPI(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_DISPLAY && wxCHECK_VERSION(3,1,2)
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGCDC_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGCDC_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGCDC_GetGraphicsContext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
extern void wxGCDC_SetGraphicsContext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxUSE_GRAPHICS_CONTEXT
extern void wxNotificationMessage_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotificationMessage_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if wxCHECK_VERSION(3,1,0)
extern void wxNotificationMessage_AddAction(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxCHECK_VERSION(3,1,0)
extern void wxNotificationMessage_Close(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotificationMessage_SetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if wxCHECK_VERSION(3,1,0)
extern void wxNotificationMessage_SetIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // wxCHECK_VERSION(3,1,0)
extern void wxNotificationMessage_SetMessage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotificationMessage_SetParent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotificationMessage_SetTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxNotificationMessage_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if __WXMSW__ 
extern void wxNotificationMessage_UseTaskBarIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // __WXMSW__ 
#if __WXMSW__ && wxCHECK_VERSION(3,1,0)
extern void wxNotificationMessage_MSWUseToasts(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // __WXMSW__ && wxCHECK_VERSION(3,1,0)
#if WXE_WEBVIEW
extern void wxWebView_New(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_GetCurrentTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_GetCurrentURL(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_GetPageSource(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_GetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_IsBusy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_IsEditable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_LoadURL(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_Print(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_Reload(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW && wxCHECK_VERSION(3,1,1)
extern void wxWebView_RunScript(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW && wxCHECK_VERSION(3,1,1)
#if WXE_WEBVIEW
extern void wxWebView_SetEditable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_SetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_Stop(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_CanCopy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_CanCut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_CanPaste(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_Copy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_Cut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_Paste(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_EnableContextMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_IsContextMenuEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_CanGoBack(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_CanGoForward(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_ClearHistory(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_EnableHistory(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_GoBack(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_GoForward(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_ClearSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_DeleteSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_GetSelectedSource(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_GetSelectedText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_HasSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_SelectAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_CanRedo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_CanUndo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_Redo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_Undo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_Find(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_CanSetZoomType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_GetZoom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_GetZoomType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_SetZoom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebView_SetZoomType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW && wxCHECK_VERSION(3,1,4)
extern void wxWebView_GetZoomFactor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW && wxCHECK_VERSION(3,1,4)
#if WXE_WEBVIEW && wxCHECK_VERSION(3,1,4)
extern void wxWebView_SetZoomFactor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW && wxCHECK_VERSION(3,1,4)
#if WXE_WEBVIEW && wxCHECK_VERSION(3,1,4)
extern void wxWebView_IsBackendAvailable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW && wxCHECK_VERSION(3,1,4)
#if WXE_WEBVIEW
extern void wxWebViewEvent_GetString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebViewEvent_GetInt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebViewEvent_GetTarget(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
extern void wxWebViewEvent_GetURL(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif // WXE_WEBVIEW

wxe_fns_t wxe_fns[] =
{
  {NULL, "", "", 0}, // 0
  {NULL, "", "", 0}, // 1
  {NULL, "", "", 0}, // 2
  {NULL, "", "", 0}, // 3
  {NULL, "", "", 0}, // 4
  {NULL, "", "", 0}, // 5
  {NULL, "", "", 0}, // 6
  {NULL, "", "", 0}, // 7
  {NULL, "", "", 0}, // 8
  {NULL, "", "", 0}, // 9
  {NULL, "", "", 0}, // 10
  {NULL, "", "", 0}, // 11
  {NULL, "", "", 0}, // 12
  {NULL, "", "", 0}, // 13
  {NULL, "", "", 0}, // 14
  {NULL, "", "", 0}, // 15
  {NULL, "", "", 0}, // 16
  {NULL, "", "", 0}, // 17
  {NULL, "", "", 0}, // 18
  {NULL, "", "", 0}, // 19
  {NULL, "", "", 0}, // 20
  {NULL, "", "", 0}, // 21
  {NULL, "", "", 0}, // 22
  {NULL, "", "", 0}, // 23
  {NULL, "", "", 0}, // 24
  {NULL, "", "", 0}, // 25
  {NULL, "", "", 0}, // 26
  {NULL, "", "", 0}, // 27
  {NULL, "", "", 0}, // 28
  {NULL, "", "", 0}, // 29
  {NULL, "", "", 0}, // 30
  {NULL, "", "", 0}, // 31
  {NULL, "", "", 0}, // 32
  {NULL, "", "", 0}, // 33
  {NULL, "", "", 0}, // 34
  {NULL, "", "", 0}, // 35
  {NULL, "", "", 0}, // 36
  {NULL, "", "", 0}, // 37
  {NULL, "", "", 0}, // 38
  {NULL, "", "", 0}, // 39
  {NULL, "", "", 0}, // 40
  {NULL, "", "", 0}, // 41
  {NULL, "", "", 0}, // 42
  {NULL, "", "", 0}, // 43
  {NULL, "", "", 0}, // 44
  {NULL, "", "", 0}, // 45
  {NULL, "", "", 0}, // 46
  {NULL, "", "", 0}, // 47
  {NULL, "", "", 0}, // 48
  {NULL, "", "", 0}, // 49
  {wxe_destroy, "wxe_util", "destroy", 1}, // 50
  {wxe_registerPid, "wxe_util", "registerPid", 1}, // 51
  {NULL, "", "", 0}, // 52
  {NULL, "", "", 0}, // 53
  {NULL, "", "", 0}, // 54
  {NULL, "", "", 0}, // 55
  {NULL, "", "", 0}, // 56
  {NULL, "", "", 0}, // 57
  {NULL, "", "", 0}, // 58
  {NULL, "", "", 0}, // 59
  {NULL, "", "", 0}, // 60
  {NULL, "", "", 0}, // 61
  {NULL, "", "", 0}, // 62
  {NULL, "", "", 0}, // 63
  {NULL, "", "", 0}, // 64
  {NULL, "", "", 0}, // 65
  {NULL, "", "", 0}, // 66
  {NULL, "", "", 0}, // 67
  {NULL, "", "", 0}, // 68
  {NULL, "", "", 0}, // 69
  {NULL, "", "", 0}, // 70
  {NULL, "", "", 0}, // 71
  {NULL, "", "", 0}, // 72
  {NULL, "", "", 0}, // 73
  {NULL, "", "", 0}, // 74
  {NULL, "", "", 0}, // 75
  {NULL, "", "", 0}, // 76
  {NULL, "", "", 0}, // 77
  {NULL, "", "", 0}, // 78
  {NULL, "", "", 0}, // 79
  {NULL, "", "", 0}, // 80
  {NULL, "", "", 0}, // 81
  {NULL, "", "", 0}, // 82
  {NULL, "", "", 0}, // 83
  {NULL, "", "", 0}, // 84
  {NULL, "", "", 0}, // 85
  {NULL, "", "", 0}, // 86
  {NULL, "", "", 0}, // 87
  {NULL, "", "", 0}, // 88
  {NULL, "", "", 0}, // 89
  {NULL, "", "", 0}, // 90
  {NULL, "", "", 0}, // 91
  {NULL, "", "", 0}, // 92
  {NULL, "", "", 0}, // 93
  {NULL, "", "", 0}, // 94
  {NULL, "", "", 0}, // 95
  {NULL, "", "", 0}, // 96
  {NULL, "", "", 0}, // 97
  {NULL, "", "", 0}, // 98
  {NULL, "", "", 0}, // 99
  {wxEvtHandler_Connect, "wxEvtHandler", "connect", 3}, // 100
  {wxEvtHandler_Disconnect_2, "wxEvtHandler", "disconnect", 3}, // 101
  {NULL, "wxEvtHandler", "disconnect", 2}, // 102 TaylorMade erl only wxEvtHandler_Disconnect_1
  {NULL, "wxEvtHandler", "disconnect", 1}, // 103 TaylorMade erl only wxEvtHandler_Disconnect_0
  {wxWindow_new_0, "wxWindow", "new", 0}, // 104
  {wxWindow_new_3, "wxWindow", "new", 3}, // 105
  {NULL, "wxWindow", "destroy", 1}, // 106 obj destructor wxWindow_destruct
  {wxWindow_Create, "wxWindow", "create", 4}, // 107
  {wxWindow_CacheBestSize, "wxWindow", "cacheBestSize", 2}, // 108
  {wxWindow_CaptureMouse, "wxWindow", "captureMouse", 1}, // 109
  {wxWindow_Centre, "wxWindow", "centre", 2}, // 110
  {wxWindow_CentreOnParent, "wxWindow", "centreOnParent", 2}, // 111
  {wxWindow_ClearBackground, "wxWindow", "clearBackground", 1}, // 112
  {wxWindow_ClientToScreen_2, "wxWindow", "clientToScreen", 3}, // 113
  {wxWindow_ClientToScreen_1, "wxWindow", "clientToScreen", 2}, // 114
  {wxWindow_Close, "wxWindow", "close", 2}, // 115
  {NULL, "", "", 0}, // 116
  {wxWindow_ConvertDialogToPixels, "wxWindow", "convertDialogToPixels", 2}, // 117
  {NULL, "", "", 0}, // 118
  {wxWindow_ConvertPixelsToDialog, "wxWindow", "convertPixelsToDialog", 2}, // 119
  {wxWindow_Destroy, "wxWindow", "'Destroy'", 1}, // 120
  {wxWindow_DestroyChildren, "wxWindow", "destroyChildren", 1}, // 121
  {wxWindow_Disable, "wxWindow", "disable", 1}, // 122
  {wxWindow_DragAcceptFiles, "wxWindow", "dragAcceptFiles", 2}, // 123
  {wxWindow_Enable, "wxWindow", "enable", 2}, // 124
  {wxWindow_FindFocus, "wxWindow", "findFocus", 0}, // 125
  {wxWindow_FindWindow_1_0, "wxWindow", "findWindow", 2}, // 126
  {wxWindow_FindWindow_1_1, "wxWindow", "findWindow", 2}, // 127
  {wxWindow_FindWindowById, "wxWindow", "findWindowById", 2}, // 128
  {wxWindow_FindWindowByName, "wxWindow", "findWindowByName", 2}, // 129
  {wxWindow_FindWindowByLabel, "wxWindow", "findWindowByLabel", 2}, // 130
  {wxWindow_Fit, "wxWindow", "fit", 1}, // 131
  {wxWindow_FitInside, "wxWindow", "fitInside", 1}, // 132
  {wxWindow_Freeze, "wxWindow", "freeze", 1}, // 133
  {wxWindow_GetAcceleratorTable, "wxWindow", "getAcceleratorTable", 1}, // 134
  {wxWindow_GetBackgroundColour, "wxWindow", "getBackgroundColour", 1}, // 135
  {wxWindow_GetBackgroundStyle, "wxWindow", "getBackgroundStyle", 1}, // 136
  {wxWindow_GetBestSize, "wxWindow", "getBestSize", 1}, // 137
  {wxWindow_GetCaret, "wxWindow", "getCaret", 1}, // 138
  {wxWindow_GetCapture, "wxWindow", "getCapture", 0}, // 139
  {wxWindow_GetCharHeight, "wxWindow", "getCharHeight", 1}, // 140
  {wxWindow_GetCharWidth, "wxWindow", "getCharWidth", 1}, // 141
  {NULL, "", "", 0}, // 142
  {wxWindow_GetChildren, "wxWindow", "getChildren", 1}, // 143
  {NULL, "", "", 0}, // 144
  {wxWindow_GetClientSize, "wxWindow", "getClientSize", 1}, // 145
  {wxWindow_GetContainingSizer, "wxWindow", "getContainingSizer", 1}, // 146
  {wxWindow_GetCursor, "wxWindow", "getCursor", 1}, // 147
  {wxWindow_GetDropTarget, "wxWindow", "getDropTarget", 1}, // 148
#if wxCHECK_VERSION(3,1,4)
  {wxWindow_GetDPIScaleFactor, "wxWindow", "getDPIScaleFactor", 1}, // 149
#else
  {NULL, "wxWindow", "getDPIScaleFactor", 0}, // 149
#endif // wxCHECK_VERSION(3,1,4)
  {wxWindow_GetExtraStyle, "wxWindow", "getExtraStyle", 1}, // 150
  {wxWindow_GetFont, "wxWindow", "getFont", 1}, // 151
  {wxWindow_GetForegroundColour, "wxWindow", "getForegroundColour", 1}, // 152
  {wxWindow_GetGrandParent, "wxWindow", "getGrandParent", 1}, // 153
  {wxWindow_GetHandle, "wxWindow", "getHandle", 1}, // 154
  {wxWindow_GetHelpText, "wxWindow", "getHelpText", 1}, // 155
  {wxWindow_GetId, "wxWindow", "getId", 1}, // 156
  {wxWindow_GetLabel, "wxWindow", "getLabel", 1}, // 157
  {wxWindow_GetMaxSize, "wxWindow", "getMaxSize", 1}, // 158
  {wxWindow_GetMinSize, "wxWindow", "getMinSize", 1}, // 159
  {wxWindow_GetName, "wxWindow", "getName", 1}, // 160
  {wxWindow_GetParent, "wxWindow", "getParent", 1}, // 161
  {NULL, "", "", 0}, // 162
  {wxWindow_GetPosition, "wxWindow", "getPosition", 1}, // 163
  {wxWindow_GetRect, "wxWindow", "getRect", 1}, // 164
  {NULL, "", "", 0}, // 165
  {wxWindow_GetScreenPosition, "wxWindow", "getScreenPosition", 1}, // 166
  {wxWindow_GetScreenRect, "wxWindow", "getScreenRect", 1}, // 167
  {wxWindow_GetScrollPos, "wxWindow", "getScrollPos", 2}, // 168
  {wxWindow_GetScrollRange, "wxWindow", "getScrollRange", 2}, // 169
  {wxWindow_GetScrollThumb, "wxWindow", "getScrollThumb", 2}, // 170
  {NULL, "", "", 0}, // 171
  {wxWindow_GetSize, "wxWindow", "getSize", 1}, // 172
  {wxWindow_GetSizer, "wxWindow", "getSizer", 1}, // 173
  {wxWindow_GetTextExtent, "wxWindow", "getTextExtent", 3}, // 174
  {wxWindow_GetThemeEnabled, "wxWindow", "getThemeEnabled", 1}, // 175
  {wxWindow_GetToolTip, "wxWindow", "getToolTip", 1}, // 176
  {wxWindow_GetUpdateRegion, "wxWindow", "getUpdateRegion", 1}, // 177
  {wxWindow_GetVirtualSize, "wxWindow", "getVirtualSize", 1}, // 178
  {NULL, "", "", 0}, // 179
  {wxWindow_GetWindowStyleFlag, "wxWindow", "getWindowStyleFlag", 1}, // 180
  {wxWindow_GetWindowVariant, "wxWindow", "getWindowVariant", 1}, // 181
  {wxWindow_HasCapture, "wxWindow", "hasCapture", 1}, // 182
  {wxWindow_HasScrollbar, "wxWindow", "hasScrollbar", 2}, // 183
  {wxWindow_HasTransparentBackground, "wxWindow", "hasTransparentBackground", 1}, // 184
  {wxWindow_Hide, "wxWindow", "hide", 1}, // 185
  {wxWindow_InheritAttributes, "wxWindow", "inheritAttributes", 1}, // 186
  {wxWindow_InitDialog, "wxWindow", "initDialog", 1}, // 187
  {wxWindow_InvalidateBestSize, "wxWindow", "invalidateBestSize", 1}, // 188
  {wxWindow_IsFrozen, "wxWindow", "isFrozen", 1}, // 189
  {wxWindow_IsEnabled, "wxWindow", "isEnabled", 1}, // 190
  {wxWindow_IsExposed_2, "wxWindow", "isExposed", 3}, // 191
  {wxWindow_IsExposed_1_0, "wxWindow", "isExposed", 2}, // 192
  {wxWindow_IsExposed_4, "wxWindow", "isExposed", 5}, // 193
  {wxWindow_IsExposed_1_1, "wxWindow", "isExposed", 2}, // 194
  {wxWindow_IsRetained, "wxWindow", "isRetained", 1}, // 195
  {wxWindow_IsShown, "wxWindow", "isShown", 1}, // 196
  {wxWindow_IsTopLevel, "wxWindow", "isTopLevel", 1}, // 197
  {wxWindow_IsShownOnScreen, "wxWindow", "isShownOnScreen", 1}, // 198
  {wxWindow_Layout, "wxWindow", "layout", 1}, // 199
  {wxWindow_LineDown, "wxWindow", "lineDown", 1}, // 200
  {wxWindow_LineUp, "wxWindow", "lineUp", 1}, // 201
  {wxWindow_Lower, "wxWindow", "lower", 1}, // 202
  {wxWindow_Move_3, "wxWindow", "move", 4}, // 203
  {wxWindow_Move_2, "wxWindow", "move", 3}, // 204
  {wxWindow_MoveAfterInTabOrder, "wxWindow", "moveAfterInTabOrder", 2}, // 205
  {wxWindow_MoveBeforeInTabOrder, "wxWindow", "moveBeforeInTabOrder", 2}, // 206
  {wxWindow_Navigate, "wxWindow", "navigate", 2}, // 207
  {wxWindow_PageDown, "wxWindow", "pageDown", 1}, // 208
  {wxWindow_PageUp, "wxWindow", "pageUp", 1}, // 209
  {wxWindow_PopupMenu_2, "wxWindow", "popupMenu", 3}, // 210
  {wxWindow_PopupMenu_3, "wxWindow", "popupMenu", 4}, // 211
  {wxWindow_Raise, "wxWindow", "raise", 1}, // 212
  {wxWindow_Refresh, "wxWindow", "refresh", 2}, // 213
  {wxWindow_RefreshRect, "wxWindow", "refreshRect", 3}, // 214
  {wxWindow_ReleaseMouse, "wxWindow", "releaseMouse", 1}, // 215
  {wxWindow_RemoveChild, "wxWindow", "removeChild", 2}, // 216
  {wxWindow_Reparent, "wxWindow", "reparent", 2}, // 217
  {wxWindow_ScreenToClient_2, "wxWindow", "screenToClient", 1}, // 218
  {wxWindow_ScreenToClient_1, "wxWindow", "screenToClient", 2}, // 219
  {wxWindow_ScrollLines, "wxWindow", "scrollLines", 2}, // 220
  {wxWindow_ScrollPages, "wxWindow", "scrollPages", 2}, // 221
  {wxWindow_ScrollWindow, "wxWindow", "scrollWindow", 4}, // 222
  {wxWindow_SetAcceleratorTable, "wxWindow", "setAcceleratorTable", 2}, // 223
  {wxWindow_SetAutoLayout, "wxWindow", "setAutoLayout", 2}, // 224
  {wxWindow_SetBackgroundColour, "wxWindow", "setBackgroundColour", 2}, // 225
  {wxWindow_SetBackgroundStyle, "wxWindow", "setBackgroundStyle", 2}, // 226
  {wxWindow_SetCaret, "wxWindow", "setCaret", 2}, // 227
  {wxWindow_SetClientSize_2, "wxWindow", "setClientSize", 3}, // 228
  {wxWindow_SetClientSize_1_0, "wxWindow", "setClientSize", 2}, // 229
  {wxWindow_SetClientSize_1_1, "wxWindow", "setClientSize", 2}, // 230
  {wxWindow_SetContainingSizer, "wxWindow", "setContainingSizer", 2}, // 231
  {wxWindow_SetCursor, "wxWindow", "setCursor", 2}, // 232
  {wxWindow_SetMaxSize, "wxWindow", "setMaxSize", 2}, // 233
  {wxWindow_SetMinSize, "wxWindow", "setMinSize", 2}, // 234
  {wxWindow_SetOwnBackgroundColour, "wxWindow", "setOwnBackgroundColour", 2}, // 235
  {wxWindow_SetOwnFont, "wxWindow", "setOwnFont", 2}, // 236
  {wxWindow_SetOwnForegroundColour, "wxWindow", "setOwnForegroundColour", 2}, // 237
  {wxWindow_SetDropTarget, "wxWindow", "setDropTarget", 2}, // 238
  {wxWindow_SetExtraStyle, "wxWindow", "setExtraStyle", 2}, // 239
  {wxWindow_SetFocus, "wxWindow", "setFocus", 1}, // 240
  {wxWindow_SetFocusFromKbd, "wxWindow", "setFocusFromKbd", 1}, // 241
  {wxWindow_SetFont, "wxWindow", "setFont", 2}, // 242
  {wxWindow_SetForegroundColour, "wxWindow", "setForegroundColour", 2}, // 243
  {wxWindow_SetHelpText, "wxWindow", "setHelpText", 2}, // 244
  {wxWindow_SetId, "wxWindow", "setId", 2}, // 245
  {wxWindow_SetLabel, "wxWindow", "setLabel", 2}, // 246
  {wxWindow_SetName, "wxWindow", "setName", 2}, // 247
  {wxWindow_SetPalette, "wxWindow", "setPalette", 2}, // 248
  {wxWindow_SetScrollbar, "wxWindow", "setScrollbar", 6}, // 249
  {wxWindow_SetScrollPos, "wxWindow", "setScrollPos", 4}, // 250
  {wxWindow_SetSize_5, "wxWindow", "setSize", 6}, // 251
  {wxWindow_SetSize_2_1, "wxWindow", "setSize", 3}, // 252
  {wxWindow_SetSize_1, "wxWindow", "setSize", 2}, // 253
  {wxWindow_SetSize_2_0, "wxWindow", "setSize", 3}, // 254
  {wxWindow_SetSizeHints_2, "wxWindow", "setSizeHints", 3}, // 255
  {wxWindow_SetSizeHints_3, "wxWindow", "setSizeHints", 4}, // 256
  {wxWindow_SetSizer, "wxWindow", "setSizer", 3}, // 257
  {wxWindow_SetSizerAndFit, "wxWindow", "setSizerAndFit", 3}, // 258
  {wxWindow_SetThemeEnabled, "wxWindow", "setThemeEnabled", 2}, // 259
  {wxWindow_SetToolTip_1_0, "wxWindow", "setToolTip", 2}, // 260
  {wxWindow_SetToolTip_1_1, "wxWindow", "setToolTip", 2}, // 261
  {wxWindow_SetVirtualSize_2, "wxWindow", "setVirtualSize", 3}, // 262
  {wxWindow_SetVirtualSize_1, "wxWindow", "setVirtualSize", 2}, // 263
  {wxWindow_SetWindowStyle, "wxWindow", "setWindowStyle", 2}, // 264
  {wxWindow_SetWindowStyleFlag, "wxWindow", "setWindowStyleFlag", 2}, // 265
  {wxWindow_SetWindowVariant, "wxWindow", "setWindowVariant", 2}, // 266
  {wxWindow_ShouldInheritColours, "wxWindow", "shouldInheritColours", 1}, // 267
  {wxWindow_Show, "wxWindow", "show", 2}, // 268
  {wxWindow_Thaw, "wxWindow", "thaw", 1}, // 269
  {wxWindow_TransferDataFromWindow, "wxWindow", "transferDataFromWindow", 1}, // 270
  {wxWindow_TransferDataToWindow, "wxWindow", "transferDataToWindow", 1}, // 271
  {wxWindow_Update, "wxWindow", "update", 1}, // 272
  {wxWindow_UpdateWindowUI, "wxWindow", "updateWindowUI", 2}, // 273
  {wxWindow_Validate, "wxWindow", "validate", 1}, // 274
  {wxWindow_WarpPointer, "wxWindow", "warpPointer", 3}, // 275
  {wxWindow_SetTransparent, "wxWindow", "setTransparent", 2}, // 276
  {wxWindow_CanSetTransparent, "wxWindow", "canSetTransparent", 1}, // 277
  {wxWindow_IsDoubleBuffered, "wxWindow", "isDoubleBuffered", 1}, // 278
#if wxCHECK_VERSION(3,1,0) || (!defined(__WXMAC__) && wxCHECK_VERSION(3,0,0))
  {wxWindow_SetDoubleBuffered, "wxWindow", "setDoubleBuffered", 2}, // 279
#else
  {NULL, "wxWindow", "setDoubleBuffered", 0}, // 279
#endif // wxCHECK_VERSION(3,1,0) || (!defined(__WXMAC__) && wxCHECK_VERSION(3,0,0))
  {wxWindow_GetContentScaleFactor, "wxWindow", "getContentScaleFactor", 1}, // 280
#if wxCHECK_VERSION(3,1,3)
  {wxWindow_GetDPI, "wxWindow", "getDPI", 1}, // 281
#else
  {NULL, "wxWindow", "getDPI", 0}, // 281
#endif // wxCHECK_VERSION(3,1,3)
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_FromDIP_1_1, "wxWindow", "fromDIP", 2}, // 282
#else
  {NULL, "wxWindow", "fromDIP", 0}, // 282
#endif // wxCHECK_VERSION(3,1,0)
  {NULL, "", "", 0}, // 283
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_FromDIP_1_0, "wxWindow", "fromDIP", 2}, // 284
#else
  {NULL, "wxWindow", "fromDIP", 0}, // 284
#endif // wxCHECK_VERSION(3,1,0)
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_FromDIP_2_1, "wxWindow", "fromDIP", 2}, // 285
#else
  {NULL, "wxWindow", "fromDIP", 0}, // 285
#endif // wxCHECK_VERSION(3,1,0)
  {NULL, "", "", 0}, // 286
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_FromDIP_2_0, "wxWindow", "fromDIP", 2}, // 287
#else
  {NULL, "wxWindow", "fromDIP", 0}, // 287
#endif // wxCHECK_VERSION(3,1,0)
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_ToDIP_1_1, "wxWindow", "toDIP", 2}, // 288
#else
  {NULL, "wxWindow", "toDIP", 0}, // 288
#endif // wxCHECK_VERSION(3,1,0)
  {NULL, "", "", 0}, // 289
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_ToDIP_1_0, "wxWindow", "toDIP", 2}, // 290
#else
  {NULL, "wxWindow", "toDIP", 0}, // 290
#endif // wxCHECK_VERSION(3,1,0)
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_ToDIP_2_1, "wxWindow", "toDIP", 2}, // 291
#else
  {NULL, "wxWindow", "toDIP", 0}, // 291
#endif // wxCHECK_VERSION(3,1,0)
  {NULL, "", "", 0}, // 292
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_ToDIP_2_0, "wxWindow", "toDIP", 2}, // 293
#else
  {NULL, "wxWindow", "toDIP", 0}, // 293
#endif // wxCHECK_VERSION(3,1,0)
  {wxTopLevelWindow_GetIcon, "wxTopLevelWindow", "getIcon", 1}, // 294
  {wxTopLevelWindow_GetIcons, "wxTopLevelWindow", "getIcons", 1}, // 295
  {wxTopLevelWindow_GetTitle, "wxTopLevelWindow", "getTitle", 1}, // 296
  {wxTopLevelWindow_IsActive, "wxTopLevelWindow", "isActive", 1}, // 297
  {wxTopLevelWindow_Iconize, "wxTopLevelWindow", "iconize", 2}, // 298
  {wxTopLevelWindow_IsFullScreen, "wxTopLevelWindow", "isFullScreen", 1}, // 299
  {wxTopLevelWindow_IsIconized, "wxTopLevelWindow", "isIconized", 1}, // 300
  {wxTopLevelWindow_IsMaximized, "wxTopLevelWindow", "isMaximized", 1}, // 301
  {wxTopLevelWindow_Maximize, "wxTopLevelWindow", "maximize", 2}, // 302
  {wxTopLevelWindow_RequestUserAttention, "wxTopLevelWindow", "requestUserAttention", 2}, // 303
  {wxTopLevelWindow_SetIcon, "wxTopLevelWindow", "setIcon", 2}, // 304
  {wxTopLevelWindow_SetIcons, "wxTopLevelWindow", "setIcons", 2}, // 305
  {wxTopLevelWindow_CentreOnScreen, "wxTopLevelWindow", "centreOnScreen", 2}, // 306
  {wxTopLevelWindow_SetShape, "wxTopLevelWindow", "setShape", 2}, // 307
  {NULL, "", "", 0}, // 308
  {wxTopLevelWindow_SetTitle, "wxTopLevelWindow", "setTitle", 2}, // 309
  {wxTopLevelWindow_ShowFullScreen, "wxTopLevelWindow", "showFullScreen", 3}, // 310
  {wxFrame_new_0, "wxFrame", "new", 0}, // 311
  {wxFrame_new_4, "wxFrame", "new", 4}, // 312
  {NULL, "wxFrame", "destroy", 1}, // 313 obj destructor wxFrame_destruct
  {wxFrame_Create, "wxFrame", "create", 5}, // 314
  {wxFrame_CreateStatusBar, "wxFrame", "createStatusBar", 2}, // 315
  {wxFrame_CreateToolBar, "wxFrame", "createToolBar", 2}, // 316
  {wxFrame_GetClientAreaOrigin, "wxFrame", "getClientAreaOrigin", 1}, // 317
  {wxFrame_GetMenuBar, "wxFrame", "getMenuBar", 1}, // 318
  {wxFrame_GetStatusBar, "wxFrame", "getStatusBar", 1}, // 319
  {wxFrame_GetStatusBarPane, "wxFrame", "getStatusBarPane", 1}, // 320
  {wxFrame_GetToolBar, "wxFrame", "getToolBar", 1}, // 321
  {wxFrame_ProcessCommand, "wxFrame", "processCommand", 2}, // 322
  {wxFrame_SendSizeEvent, "wxFrame", "sendSizeEvent", 2}, // 323
  {wxFrame_SetMenuBar, "wxFrame", "setMenuBar", 2}, // 324
  {wxFrame_SetStatusBar, "wxFrame", "setStatusBar", 2}, // 325
  {wxFrame_SetStatusBarPane, "wxFrame", "setStatusBarPane", 2}, // 326
  {wxFrame_SetStatusText, "wxFrame", "setStatusText", 3}, // 327
  {wxFrame_SetStatusWidths, "wxFrame", "setStatusWidths", 2}, // 328
  {wxFrame_SetToolBar, "wxFrame", "setToolBar", 2}, // 329
  {wxMiniFrame_new_0, "wxMiniFrame", "new", 0}, // 330
  {wxMiniFrame_new_4, "wxMiniFrame", "new", 4}, // 331
  {NULL, "wxMiniFrame", "destroy", 1}, // 332 obj destructor wxMiniFrame_destruct
  {wxMiniFrame_Create, "wxMiniFrame", "create", 5}, // 333
  {wxSplashScreen_new, "wxSplashScreen", "new", 6}, // 334
  {NULL, "wxSplashScreen", "destroy", 1}, // 335 obj destructor wxSplashScreen_destruct
  {wxSplashScreen_GetSplashStyle, "wxSplashScreen", "getSplashStyle", 1}, // 336
  {wxSplashScreen_GetTimeout, "wxSplashScreen", "getTimeout", 1}, // 337
  {wxPanel_new_0, "wxPanel", "new", 0}, // 338
  {wxPanel_new_2, "wxPanel", "new", 2}, // 339
  {NULL, "wxPanel", "destroy", 1}, // 340 obj destructor wxPanel_destruct
  {wxPanel_InitDialog, "wxPanel", "initDialog", 1}, // 341
  {wxPanel_SetFocusIgnoringChildren, "wxPanel", "setFocusIgnoringChildren", 1}, // 342
  {wxScrolledWindow_new_0, "wxScrolledWindow", "new", 0}, // 343
  {wxScrolledWindow_new_2, "wxScrolledWindow", "new", 2}, // 344
  {wxScrolledWindow_CalcScrolledPosition_4, "wxScrolledWindow", "calcScrolledPosition", 3}, // 345
  {wxScrolledWindow_CalcScrolledPosition_1, "wxScrolledWindow", "calcScrolledPosition", 2}, // 346
  {wxScrolledWindow_CalcUnscrolledPosition_4, "wxScrolledWindow", "calcUnscrolledPosition", 3}, // 347
  {wxScrolledWindow_CalcUnscrolledPosition_1, "wxScrolledWindow", "calcUnscrolledPosition", 2}, // 348
  {wxScrolledWindow_EnableScrolling, "wxScrolledWindow", "enableScrolling", 3}, // 349
  {wxScrolledWindow_GetScrollPixelsPerUnit, "wxScrolledWindow", "getScrollPixelsPerUnit", 1}, // 350
  {NULL, "", "", 0}, // 351
  {wxScrolledWindow_GetViewStart, "wxScrolledWindow", "getViewStart", 1}, // 352
  {wxScrolledWindow_DoPrepareDC, "wxScrolledWindow", "doPrepareDC", 2}, // 353
  {wxScrolledWindow_PrepareDC, "wxScrolledWindow", "prepareDC", 2}, // 354
  {wxScrolledWindow_Scroll_2, "wxScrolledWindow", "scroll", 3}, // 355
  {wxScrolledWindow_Scroll_1, "wxScrolledWindow", "scroll", 2}, // 356
  {wxScrolledWindow_SetScrollbars, "wxScrolledWindow", "setScrollbars", 6}, // 357
  {wxScrolledWindow_SetScrollRate, "wxScrolledWindow", "setScrollRate", 3}, // 358
  {wxScrolledWindow_SetTargetWindow, "wxScrolledWindow", "setTargetWindow", 2}, // 359
  {NULL, "wxScrolledWindow", "'Destroy'", 1}, // 360 obj destructor wxScrolledWindow_destroy
  {wxSashWindow_new_0, "wxSashWindow", "new", 0}, // 361
  {wxSashWindow_new_2, "wxSashWindow", "new", 2}, // 362
  {NULL, "wxSashWindow", "destroy", 1}, // 363 obj destructor wxSashWindow_destruct
  {wxSashWindow_GetSashVisible, "wxSashWindow", "getSashVisible", 2}, // 364
  {wxSashWindow_GetMaximumSizeX, "wxSashWindow", "getMaximumSizeX", 1}, // 365
  {wxSashWindow_GetMaximumSizeY, "wxSashWindow", "getMaximumSizeY", 1}, // 366
  {wxSashWindow_GetMinimumSizeX, "wxSashWindow", "getMinimumSizeX", 1}, // 367
  {wxSashWindow_GetMinimumSizeY, "wxSashWindow", "getMinimumSizeY", 1}, // 368
  {wxSashWindow_SetMaximumSizeX, "wxSashWindow", "setMaximumSizeX", 2}, // 369
  {wxSashWindow_SetMaximumSizeY, "wxSashWindow", "setMaximumSizeY", 2}, // 370
  {wxSashWindow_SetMinimumSizeX, "wxSashWindow", "setMinimumSizeX", 2}, // 371
  {wxSashWindow_SetMinimumSizeY, "wxSashWindow", "setMinimumSizeY", 2}, // 372
  {wxSashWindow_SetSashVisible, "wxSashWindow", "setSashVisible", 3}, // 373
  {wxSashLayoutWindow_new_0, "wxSashLayoutWindow", "new", 0}, // 374
  {wxSashLayoutWindow_new_2, "wxSashLayoutWindow", "new", 2}, // 375
  {wxSashLayoutWindow_Create, "wxSashLayoutWindow", "create", 3}, // 376
  {wxSashLayoutWindow_GetAlignment, "wxSashLayoutWindow", "getAlignment", 1}, // 377
  {wxSashLayoutWindow_GetOrientation, "wxSashLayoutWindow", "getOrientation", 1}, // 378
  {wxSashLayoutWindow_SetAlignment, "wxSashLayoutWindow", "setAlignment", 2}, // 379
  {wxSashLayoutWindow_SetDefaultSize, "wxSashLayoutWindow", "setDefaultSize", 2}, // 380
  {wxSashLayoutWindow_SetOrientation, "wxSashLayoutWindow", "setOrientation", 2}, // 381
  {NULL, "wxSashLayoutWindow", "'Destroy'", 1}, // 382 obj destructor wxSashLayoutWindow_destroy
  {wxGrid_new_0, "wxGrid", "new", 0}, // 383
  {wxGrid_new_3, "wxGrid", "new", 3}, // 384
  {NULL, "wxGrid", "destroy", 1}, // 385 obj destructor wxGrid_destruct
  {wxGrid_AppendCols, "wxGrid", "appendCols", 2}, // 386
  {wxGrid_AppendRows, "wxGrid", "appendRows", 2}, // 387
  {wxGrid_AutoSize, "wxGrid", "autoSize", 1}, // 388
  {wxGrid_AutoSizeColumn, "wxGrid", "autoSizeColumn", 3}, // 389
  {wxGrid_AutoSizeColumns, "wxGrid", "autoSizeColumns", 2}, // 390
  {wxGrid_AutoSizeRow, "wxGrid", "autoSizeRow", 3}, // 391
  {wxGrid_AutoSizeRows, "wxGrid", "autoSizeRows", 2}, // 392
  {wxGrid_BeginBatch, "wxGrid", "beginBatch", 1}, // 393
  {wxGrid_BlockToDeviceRect, "wxGrid", "blockToDeviceRect", 3}, // 394
  {wxGrid_CanDragCell, "wxGrid", "canDragCell", 1}, // 395
  {wxGrid_CanDragColMove, "wxGrid", "canDragColMove", 1}, // 396
#if wxCHECK_VERSION(3,1,4)
  {wxGrid_CanDragGridRowEdges, "wxGrid", "canDragGridRowEdges", 1}, // 397
#else
  {NULL, "wxGrid", "canDragGridRowEdges", 0}, // 397
#endif // wxCHECK_VERSION(3,1,4)
  {wxGrid_CanDragColSize, "wxGrid", "canDragColSize", 2}, // 398
  {wxGrid_CanDragRowSize, "wxGrid", "canDragRowSize", 2}, // 399
  {wxGrid_CanDragGridSize, "wxGrid", "canDragGridSize", 1}, // 400
  {wxGrid_CanEnableCellControl, "wxGrid", "canEnableCellControl", 1}, // 401
  {wxGrid_CellToRect_2, "wxGrid", "cellToRect", 3}, // 402
  {wxGrid_CellToRect_1, "wxGrid", "cellToRect", 2}, // 403
  {wxGrid_ClearGrid, "wxGrid", "clearGrid", 1}, // 404
  {wxGrid_ClearSelection, "wxGrid", "clearSelection", 1}, // 405
  {wxGrid_CreateGrid, "wxGrid", "createGrid", 4}, // 406
  {wxGrid_DeleteCols, "wxGrid", "deleteCols", 2}, // 407
  {wxGrid_DeleteRows, "wxGrid", "deleteRows", 2}, // 408
  {wxGrid_DisableCellEditControl, "wxGrid", "disableCellEditControl", 1}, // 409
  {wxGrid_DisableDragColSize, "wxGrid", "disableDragColSize", 1}, // 410
  {wxGrid_DisableDragGridSize, "wxGrid", "disableDragGridSize", 1}, // 411
  {wxGrid_DisableDragRowSize, "wxGrid", "disableDragRowSize", 1}, // 412
  {wxGrid_EnableCellEditControl, "wxGrid", "enableCellEditControl", 2}, // 413
  {wxGrid_EnableDragColSize, "wxGrid", "enableDragColSize", 2}, // 414
  {wxGrid_EnableDragGridSize, "wxGrid", "enableDragGridSize", 2}, // 415
  {wxGrid_EnableDragRowSize, "wxGrid", "enableDragRowSize", 2}, // 416
  {wxGrid_EnableEditing, "wxGrid", "enableEditing", 2}, // 417
  {wxGrid_EnableGridLines, "wxGrid", "enableGridLines", 2}, // 418
  {wxGrid_EndBatch, "wxGrid", "endBatch", 1}, // 419
  {wxGrid_Fit, "wxGrid", "fit", 1}, // 420
  {wxGrid_ForceRefresh, "wxGrid", "forceRefresh", 1}, // 421
  {wxGrid_GetBatchCount, "wxGrid", "getBatchCount", 1}, // 422
  {wxGrid_GetCellAlignment, "wxGrid", "getCellAlignment", 3}, // 423
  {wxGrid_GetCellBackgroundColour, "wxGrid", "getCellBackgroundColour", 3}, // 424
  {wxGrid_GetCellEditor, "wxGrid", "getCellEditor", 3}, // 425
  {wxGrid_GetCellFont, "wxGrid", "getCellFont", 3}, // 426
  {wxGrid_GetCellRenderer, "wxGrid", "getCellRenderer", 3}, // 427
  {wxGrid_GetCellTextColour, "wxGrid", "getCellTextColour", 3}, // 428
  {wxGrid_GetCellValue_2, "wxGrid", "getCellValue", 3}, // 429
  {wxGrid_GetCellValue_1, "wxGrid", "getCellValue", 2}, // 430
  {wxGrid_GetColLabelAlignment, "wxGrid", "getColLabelAlignment", 1}, // 431
  {wxGrid_GetColLabelSize, "wxGrid", "getColLabelSize", 1}, // 432
  {wxGrid_GetColLabelValue, "wxGrid", "getColLabelValue", 2}, // 433
  {wxGrid_GetColMinimalAcceptableWidth, "wxGrid", "getColMinimalAcceptableWidth", 1}, // 434
  {wxGrid_GetDefaultCellAlignment, "wxGrid", "getDefaultCellAlignment", 1}, // 435
  {wxGrid_GetDefaultCellBackgroundColour, "wxGrid", "getDefaultCellBackgroundColour", 1}, // 436
  {wxGrid_GetDefaultCellFont, "wxGrid", "getDefaultCellFont", 1}, // 437
  {wxGrid_GetDefaultCellTextColour, "wxGrid", "getDefaultCellTextColour", 1}, // 438
  {wxGrid_GetDefaultColLabelSize, "wxGrid", "getDefaultColLabelSize", 1}, // 439
  {wxGrid_GetDefaultColSize, "wxGrid", "getDefaultColSize", 1}, // 440
  {wxGrid_GetDefaultEditor, "wxGrid", "getDefaultEditor", 1}, // 441
  {wxGrid_GetDefaultEditorForCell_2, "wxGrid", "getDefaultEditorForCell", 3}, // 442
  {wxGrid_GetDefaultEditorForCell_1, "wxGrid", "getDefaultEditorForCell", 2}, // 443
  {wxGrid_GetDefaultEditorForType, "wxGrid", "getDefaultEditorForType", 2}, // 444
  {wxGrid_GetDefaultRenderer, "wxGrid", "getDefaultRenderer", 1}, // 445
  {wxGrid_GetDefaultRendererForCell, "wxGrid", "getDefaultRendererForCell", 3}, // 446
  {wxGrid_GetDefaultRendererForType, "wxGrid", "getDefaultRendererForType", 2}, // 447
  {wxGrid_GetDefaultRowLabelSize, "wxGrid", "getDefaultRowLabelSize", 1}, // 448
  {wxGrid_GetDefaultRowSize, "wxGrid", "getDefaultRowSize", 1}, // 449
  {wxGrid_GetGridCursorCol, "wxGrid", "getGridCursorCol", 1}, // 450
  {wxGrid_GetGridCursorRow, "wxGrid", "getGridCursorRow", 1}, // 451
  {wxGrid_GetGridLineColour, "wxGrid", "getGridLineColour", 1}, // 452
  {wxGrid_GridLinesEnabled, "wxGrid", "gridLinesEnabled", 1}, // 453
  {wxGrid_GetLabelBackgroundColour, "wxGrid", "getLabelBackgroundColour", 1}, // 454
  {wxGrid_GetLabelFont, "wxGrid", "getLabelFont", 1}, // 455
  {wxGrid_GetLabelTextColour, "wxGrid", "getLabelTextColour", 1}, // 456
  {wxGrid_GetNumberCols, "wxGrid", "getNumberCols", 1}, // 457
  {wxGrid_GetNumberRows, "wxGrid", "getNumberRows", 1}, // 458
  {wxGrid_GetOrCreateCellAttr, "wxGrid", "getOrCreateCellAttr", 3}, // 459
  {wxGrid_GetRowMinimalAcceptableHeight, "wxGrid", "getRowMinimalAcceptableHeight", 1}, // 460
  {wxGrid_GetRowLabelAlignment, "wxGrid", "getRowLabelAlignment", 1}, // 461
  {wxGrid_GetRowLabelSize, "wxGrid", "getRowLabelSize", 1}, // 462
  {wxGrid_GetRowLabelValue, "wxGrid", "getRowLabelValue", 2}, // 463
  {wxGrid_GetRowSize, "wxGrid", "getRowSize", 2}, // 464
  {wxGrid_GetScrollLineX, "wxGrid", "getScrollLineX", 1}, // 465
  {wxGrid_GetScrollLineY, "wxGrid", "getScrollLineY", 1}, // 466
  {wxGrid_GetSelectedCells, "wxGrid", "getSelectedCells", 1}, // 467
  {wxGrid_GetSelectedCols, "wxGrid", "getSelectedCols", 1}, // 468
  {wxGrid_GetSelectedRows, "wxGrid", "getSelectedRows", 1}, // 469
  {wxGrid_GetSelectionBackground, "wxGrid", "getSelectionBackground", 1}, // 470
  {wxGrid_GetSelectionBlockTopLeft, "wxGrid", "getSelectionBlockTopLeft", 1}, // 471
  {wxGrid_GetSelectionBlockBottomRight, "wxGrid", "getSelectionBlockBottomRight", 1}, // 472
  {wxGrid_GetSelectionForeground, "wxGrid", "getSelectionForeground", 1}, // 473
  {wxGrid_GetGridWindow, "wxGrid", "getGridWindow", 1}, // 474
  {wxGrid_GetGridRowLabelWindow, "wxGrid", "getGridRowLabelWindow", 1}, // 475
  {wxGrid_GetGridColLabelWindow, "wxGrid", "getGridColLabelWindow", 1}, // 476
  {wxGrid_GetGridCornerLabelWindow, "wxGrid", "getGridCornerLabelWindow", 1}, // 477
  {wxGrid_HideCellEditControl, "wxGrid", "hideCellEditControl", 1}, // 478
  {wxGrid_InsertCols, "wxGrid", "insertCols", 2}, // 479
  {wxGrid_InsertRows, "wxGrid", "insertRows", 2}, // 480
  {wxGrid_IsCellEditControlEnabled, "wxGrid", "isCellEditControlEnabled", 1}, // 481
  {wxGrid_IsCurrentCellReadOnly, "wxGrid", "isCurrentCellReadOnly", 1}, // 482
  {wxGrid_IsEditable, "wxGrid", "isEditable", 1}, // 483
  {wxGrid_IsInSelection_2, "wxGrid", "isInSelection", 3}, // 484
  {wxGrid_IsInSelection_1, "wxGrid", "isInSelection", 2}, // 485
  {wxGrid_IsReadOnly, "wxGrid", "isReadOnly", 3}, // 486
  {wxGrid_IsSelection, "wxGrid", "isSelection", 1}, // 487
  {wxGrid_IsVisible_3, "wxGrid", "isVisible", 4}, // 488
  {wxGrid_IsVisible_2, "wxGrid", "isVisible", 3}, // 489
  {wxGrid_MakeCellVisible_2, "wxGrid", "makeCellVisible", 3}, // 490
  {wxGrid_MakeCellVisible_1, "wxGrid", "makeCellVisible", 2}, // 491
  {wxGrid_MoveCursorDown, "wxGrid", "moveCursorDown", 2}, // 492
  {wxGrid_MoveCursorLeft, "wxGrid", "moveCursorLeft", 2}, // 493
  {wxGrid_MoveCursorRight, "wxGrid", "moveCursorRight", 2}, // 494
  {wxGrid_MoveCursorUp, "wxGrid", "moveCursorUp", 2}, // 495
  {wxGrid_MoveCursorDownBlock, "wxGrid", "moveCursorDownBlock", 2}, // 496
  {wxGrid_MoveCursorLeftBlock, "wxGrid", "moveCursorLeftBlock", 2}, // 497
  {wxGrid_MoveCursorRightBlock, "wxGrid", "moveCursorRightBlock", 2}, // 498
  {wxGrid_MoveCursorUpBlock, "wxGrid", "moveCursorUpBlock", 2}, // 499
  {wxGrid_MovePageDown, "wxGrid", "movePageDown", 1}, // 500
  {wxGrid_MovePageUp, "wxGrid", "movePageUp", 1}, // 501
  {wxGrid_RegisterDataType, "wxGrid", "registerDataType", 4}, // 502
  {wxGrid_SaveEditControlValue, "wxGrid", "saveEditControlValue", 1}, // 503
  {wxGrid_SelectAll, "wxGrid", "selectAll", 1}, // 504
  {wxGrid_SelectBlock_5, "wxGrid", "selectBlock", 6}, // 505
  {wxGrid_SelectBlock_3, "wxGrid", "selectBlock", 4}, // 506
  {wxGrid_SelectCol, "wxGrid", "selectCol", 3}, // 507
  {wxGrid_SelectRow, "wxGrid", "selectRow", 3}, // 508
  {wxGrid_SetCellAlignment, "wxGrid", "setCellAlignment", 5}, // 509
  {wxGrid_SetCellBackgroundColour, "wxGrid", "setCellBackgroundColour", 4}, // 510
  {wxGrid_SetCellEditor, "wxGrid", "setCellEditor", 4}, // 511
  {wxGrid_SetCellFont, "wxGrid", "setCellFont", 4}, // 512
  {wxGrid_SetCellRenderer, "wxGrid", "setCellRenderer", 4}, // 513
  {wxGrid_SetCellTextColour, "wxGrid", "setCellTextColour", 4}, // 514
  {wxGrid_SetCellValue_3, "wxGrid", "setCellValue", 4}, // 515
  {wxGrid_SetCellValue_2, "wxGrid", "setCellValue", 3}, // 516
  {wxGrid_SetColAttr, "wxGrid", "setColAttr", 3}, // 517
  {wxGrid_SetColFormatBool, "wxGrid", "setColFormatBool", 2}, // 518
  {wxGrid_SetColFormatNumber, "wxGrid", "setColFormatNumber", 2}, // 519
  {wxGrid_SetColFormatFloat, "wxGrid", "setColFormatFloat", 3}, // 520
  {wxGrid_SetColFormatCustom, "wxGrid", "setColFormatCustom", 3}, // 521
  {wxGrid_SetColLabelAlignment, "wxGrid", "setColLabelAlignment", 3}, // 522
  {wxGrid_SetColLabelSize, "wxGrid", "setColLabelSize", 2}, // 523
  {wxGrid_SetColLabelValue, "wxGrid", "setColLabelValue", 3}, // 524
  {wxGrid_SetColMinimalWidth, "wxGrid", "setColMinimalWidth", 3}, // 525
  {wxGrid_SetColMinimalAcceptableWidth, "wxGrid", "setColMinimalAcceptableWidth", 2}, // 526
  {wxGrid_SetColSize, "wxGrid", "setColSize", 3}, // 527
  {wxGrid_SetDefaultCellAlignment, "wxGrid", "setDefaultCellAlignment", 3}, // 528
  {wxGrid_SetDefaultCellBackgroundColour, "wxGrid", "setDefaultCellBackgroundColour", 2}, // 529
  {wxGrid_SetDefaultCellFont, "wxGrid", "setDefaultCellFont", 2}, // 530
  {wxGrid_SetDefaultCellTextColour, "wxGrid", "setDefaultCellTextColour", 2}, // 531
  {wxGrid_SetDefaultEditor, "wxGrid", "setDefaultEditor", 2}, // 532
  {wxGrid_SetDefaultRenderer, "wxGrid", "setDefaultRenderer", 2}, // 533
  {wxGrid_SetDefaultColSize, "wxGrid", "setDefaultColSize", 3}, // 534
  {wxGrid_SetDefaultRowSize, "wxGrid", "setDefaultRowSize", 3}, // 535
  {wxGrid_SetGridCursor_2, "wxGrid", "setGridCursor", 3}, // 536
  {wxGrid_SetGridCursor_1, "wxGrid", "setGridCursor", 2}, // 537
  {wxGrid_SetGridLineColour, "wxGrid", "setGridLineColour", 2}, // 538
  {wxGrid_SetLabelBackgroundColour, "wxGrid", "setLabelBackgroundColour", 2}, // 539
  {wxGrid_SetLabelFont, "wxGrid", "setLabelFont", 2}, // 540
  {wxGrid_SetLabelTextColour, "wxGrid", "setLabelTextColour", 2}, // 541
  {wxGrid_SetMargins, "wxGrid", "setMargins", 3}, // 542
  {wxGrid_SetReadOnly, "wxGrid", "setReadOnly", 4}, // 543
  {wxGrid_SetRowAttr, "wxGrid", "setRowAttr", 3}, // 544
  {wxGrid_SetRowLabelAlignment, "wxGrid", "setRowLabelAlignment", 3}, // 545
  {wxGrid_SetRowLabelSize, "wxGrid", "setRowLabelSize", 2}, // 546
  {wxGrid_SetRowLabelValue, "wxGrid", "setRowLabelValue", 3}, // 547
  {wxGrid_SetRowMinimalHeight, "wxGrid", "setRowMinimalHeight", 3}, // 548
  {wxGrid_SetRowMinimalAcceptableHeight, "wxGrid", "setRowMinimalAcceptableHeight", 2}, // 549
  {wxGrid_SetRowSize, "wxGrid", "setRowSize", 3}, // 550
  {wxGrid_SetScrollLineX, "wxGrid", "setScrollLineX", 2}, // 551
  {wxGrid_SetScrollLineY, "wxGrid", "setScrollLineY", 2}, // 552
  {wxGrid_SetSelectionBackground, "wxGrid", "setSelectionBackground", 2}, // 553
  {wxGrid_SetSelectionForeground, "wxGrid", "setSelectionForeground", 2}, // 554
  {wxGrid_SetSelectionMode, "wxGrid", "setSelectionMode", 2}, // 555
  {wxGrid_ShowCellEditControl, "wxGrid", "showCellEditControl", 1}, // 556
  {wxGrid_XToCol, "wxGrid", "xToCol", 3}, // 557
  {wxGrid_XToEdgeOfCol, "wxGrid", "xToEdgeOfCol", 2}, // 558
  {wxGrid_YToEdgeOfRow, "wxGrid", "yToEdgeOfRow", 2}, // 559
  {wxGrid_YToRow, "wxGrid", "yToRow", 3}, // 560
  {wxGridCellRenderer_Draw, "wxGridCellRenderer", "draw", 8}, // 561
  {wxGridCellRenderer_GetBestSize, "wxGridCellRenderer", "getBestSize", 6}, // 562
  {wxGridCellEditor_Create, "wxGridCellEditor", "create", 4}, // 563
  {wxGridCellEditor_IsCreated, "wxGridCellEditor", "isCreated", 1}, // 564
  {wxGridCellEditor_SetSize, "wxGridCellEditor", "setSize", 2}, // 565
  {wxGridCellEditor_Show, "wxGridCellEditor", "show", 3}, // 566
  {wxGridCellEditor_Reset, "wxGridCellEditor", "reset", 1}, // 567
  {wxGridCellEditor_StartingKey, "wxGridCellEditor", "startingKey", 2}, // 568
  {wxGridCellEditor_StartingClick, "wxGridCellEditor", "startingClick", 1}, // 569
  {wxGridCellEditor_HandleReturn, "wxGridCellEditor", "handleReturn", 2}, // 570
  {wxGridCellBoolRenderer_new, "wxGridCellBoolRenderer", "new", 0}, // 571
  {wxGridCellBoolRenderer_destroy, "wxGridCellBoolRenderer", "'Destroy'", 1}, // 572
  {wxGridCellBoolEditor_new, "wxGridCellBoolEditor", "new", 0}, // 573
  {wxGridCellBoolEditor_IsTrueValue, "wxGridCellBoolEditor", "isTrueValue", 1}, // 574
  {wxGridCellBoolEditor_UseStringValues, "wxGridCellBoolEditor", "useStringValues", 1}, // 575
  {wxGridCellBoolEditor_destroy, "wxGridCellBoolEditor", "'Destroy'", 1}, // 576
  {wxGridCellFloatRenderer_new, "wxGridCellFloatRenderer", "new", 1}, // 577
  {wxGridCellFloatRenderer_GetPrecision, "wxGridCellFloatRenderer", "getPrecision", 1}, // 578
  {wxGridCellFloatRenderer_GetWidth, "wxGridCellFloatRenderer", "getWidth", 1}, // 579
  {wxGridCellFloatRenderer_SetParameters, "wxGridCellFloatRenderer", "setParameters", 2}, // 580
  {wxGridCellFloatRenderer_SetPrecision, "wxGridCellFloatRenderer", "setPrecision", 2}, // 581
  {wxGridCellFloatRenderer_SetWidth, "wxGridCellFloatRenderer", "setWidth", 2}, // 582
  {wxGridCellFloatRenderer_destroy, "wxGridCellFloatRenderer", "'Destroy'", 1}, // 583
  {wxGridCellFloatEditor_new, "wxGridCellFloatEditor", "new", 1}, // 584
  {wxGridCellFloatEditor_SetParameters, "wxGridCellFloatEditor", "setParameters", 2}, // 585
  {wxGridCellFloatEditor_destroy, "wxGridCellFloatEditor", "'Destroy'", 1}, // 586
  {wxGridCellStringRenderer_new, "wxGridCellStringRenderer", "new", 0}, // 587
  {wxGridCellStringRenderer_destroy, "wxGridCellStringRenderer", "'Destroy'", 1}, // 588
  {wxGridCellTextEditor_new, "wxGridCellTextEditor", "new", 1}, // 589
  {wxGridCellTextEditor_SetParameters, "wxGridCellTextEditor", "setParameters", 2}, // 590
  {wxGridCellTextEditor_destroy, "wxGridCellTextEditor", "'Destroy'", 1}, // 591
  {NULL, "", "", 0}, // 592
  {wxGridCellChoiceEditor_new, "wxGridCellChoiceEditor", "new", 2}, // 593
  {wxGridCellChoiceEditor_SetParameters, "wxGridCellChoiceEditor", "setParameters", 2}, // 594
  {wxGridCellChoiceEditor_destroy, "wxGridCellChoiceEditor", "'Destroy'", 1}, // 595
  {wxGridCellNumberRenderer_new, "wxGridCellNumberRenderer", "new", 0}, // 596
  {wxGridCellNumberRenderer_destroy, "wxGridCellNumberRenderer", "'Destroy'", 1}, // 597
  {wxGridCellNumberEditor_new, "wxGridCellNumberEditor", "new", 1}, // 598
  {wxGridCellNumberEditor_GetValue, "wxGridCellNumberEditor", "getValue", 1}, // 599
  {wxGridCellNumberEditor_SetParameters, "wxGridCellNumberEditor", "setParameters", 2}, // 600
  {wxGridCellNumberEditor_destroy, "wxGridCellNumberEditor", "'Destroy'", 1}, // 601
  {wxGridCellAttr_SetTextColour, "wxGridCellAttr", "setTextColour", 2}, // 602
  {wxGridCellAttr_SetBackgroundColour, "wxGridCellAttr", "setBackgroundColour", 2}, // 603
  {wxGridCellAttr_SetFont, "wxGridCellAttr", "setFont", 2}, // 604
  {wxGridCellAttr_SetAlignment, "wxGridCellAttr", "setAlignment", 3}, // 605
  {wxGridCellAttr_SetReadOnly, "wxGridCellAttr", "setReadOnly", 2}, // 606
  {wxGridCellAttr_SetRenderer, "wxGridCellAttr", "setRenderer", 2}, // 607
  {wxGridCellAttr_SetEditor, "wxGridCellAttr", "setEditor", 2}, // 608
  {wxGridCellAttr_HasTextColour, "wxGridCellAttr", "hasTextColour", 1}, // 609
  {wxGridCellAttr_HasBackgroundColour, "wxGridCellAttr", "hasBackgroundColour", 1}, // 610
  {wxGridCellAttr_HasFont, "wxGridCellAttr", "hasFont", 1}, // 611
  {wxGridCellAttr_HasAlignment, "wxGridCellAttr", "hasAlignment", 1}, // 612
  {wxGridCellAttr_HasRenderer, "wxGridCellAttr", "hasRenderer", 1}, // 613
  {wxGridCellAttr_HasEditor, "wxGridCellAttr", "hasEditor", 1}, // 614
  {wxGridCellAttr_GetTextColour, "wxGridCellAttr", "getTextColour", 1}, // 615
  {wxGridCellAttr_GetBackgroundColour, "wxGridCellAttr", "getBackgroundColour", 1}, // 616
  {wxGridCellAttr_GetFont, "wxGridCellAttr", "getFont", 1}, // 617
  {wxGridCellAttr_GetAlignment, "wxGridCellAttr", "getAlignment", 1}, // 618
  {wxGridCellAttr_GetRenderer, "wxGridCellAttr", "getRenderer", 4}, // 619
  {wxGridCellAttr_GetEditor, "wxGridCellAttr", "getEditor", 4}, // 620
  {wxGridCellAttr_IsReadOnly, "wxGridCellAttr", "isReadOnly", 1}, // 621
  {wxGridCellAttr_SetDefAttr, "wxGridCellAttr", "setDefAttr", 2}, // 622
  {wxDC_Blit, "wxDC", "blit", 6}, // 623
  {wxDC_CalcBoundingBox, "wxDC", "calcBoundingBox", 3}, // 624
  {wxDC_Clear, "wxDC", "clear", 1}, // 625
  {wxDC_CrossHair, "wxDC", "crossHair", 2}, // 626
  {wxDC_DestroyClippingRegion, "wxDC", "destroyClippingRegion", 1}, // 627
  {wxDC_DeviceToLogicalX, "wxDC", "deviceToLogicalX", 2}, // 628
  {wxDC_DeviceToLogicalXRel, "wxDC", "deviceToLogicalXRel", 2}, // 629
  {wxDC_DeviceToLogicalY, "wxDC", "deviceToLogicalY", 2}, // 630
  {wxDC_DeviceToLogicalYRel, "wxDC", "deviceToLogicalYRel", 2}, // 631
  {wxDC_DrawArc, "wxDC", "drawArc", 4}, // 632
  {wxDC_DrawBitmap, "wxDC", "drawBitmap", 4}, // 633
  {wxDC_DrawCheckMark, "wxDC", "drawCheckMark", 2}, // 634
  {wxDC_DrawCircle, "wxDC", "drawCircle", 3}, // 635
  {NULL, "", "", 0}, // 636
  {wxDC_DrawEllipse_2, "wxDC", "drawEllipse", 3}, // 637
  {wxDC_DrawEllipse_1, "wxDC", "drawEllipse", 2}, // 638
  {wxDC_DrawEllipticArc, "wxDC", "drawEllipticArc", 5}, // 639
  {wxDC_DrawIcon, "wxDC", "drawIcon", 3}, // 640
  {wxDC_DrawLabel, "wxDC", "drawLabel", 4}, // 641
  {wxDC_DrawLine, "wxDC", "drawLine", 3}, // 642
  {wxDC_DrawLines, "wxDC", "drawLines", 3}, // 643
  {NULL, "", "", 0}, // 644
  {wxDC_DrawPolygon, "wxDC", "drawPolygon", 3}, // 645
  {NULL, "", "", 0}, // 646
  {wxDC_DrawPoint, "wxDC", "drawPoint", 2}, // 647
  {NULL, "", "", 0}, // 648
  {wxDC_DrawRectangle_2, "wxDC", "drawRectangle", 3}, // 649
  {wxDC_DrawRectangle_1, "wxDC", "drawRectangle", 2}, // 650
  {wxDC_DrawRotatedText, "wxDC", "drawRotatedText", 4}, // 651
  {NULL, "", "", 0}, // 652
  {wxDC_DrawRoundedRectangle_3, "wxDC", "drawRoundedRectangle", 4}, // 653
  {wxDC_DrawRoundedRectangle_2, "wxDC", "drawRoundedRectangle", 3}, // 654
  {wxDC_DrawText, "wxDC", "drawText", 3}, // 655
  {wxDC_EndDoc, "wxDC", "endDoc", 1}, // 656
  {wxDC_EndPage, "wxDC", "endPage", 1}, // 657
  {wxDC_FloodFill, "wxDC", "floodFill", 4}, // 658
  {wxDC_GetBackground, "wxDC", "getBackground", 1}, // 659
  {wxDC_GetBackgroundMode, "wxDC", "getBackgroundMode", 1}, // 660
  {wxDC_GetBrush, "wxDC", "getBrush", 1}, // 661
  {wxDC_GetCharHeight, "wxDC", "getCharHeight", 1}, // 662
  {wxDC_GetCharWidth, "wxDC", "getCharWidth", 1}, // 663
  {wxDC_GetClippingBox, "wxDC", "getClippingBox", 1}, // 664
  {wxDC_GetFont, "wxDC", "getFont", 1}, // 665
  {wxDC_GetLayoutDirection, "wxDC", "getLayoutDirection", 1}, // 666
  {wxDC_GetLogicalFunction, "wxDC", "getLogicalFunction", 1}, // 667
  {wxDC_GetMapMode, "wxDC", "getMapMode", 1}, // 668
  {wxDC_GetMultiLineTextExtent_4, "wxDC", "getMultiLineTextExtent", 3}, // 669
  {wxDC_GetMultiLineTextExtent_1, "wxDC", "getMultiLineTextExtent", 2}, // 670
  {wxDC_GetPartialTextExtents, "wxDC", "getPartialTextExtents", 2}, // 671
  {wxDC_GetPen, "wxDC", "getPen", 1}, // 672
  {wxDC_GetPixel, "wxDC", "getPixel", 2}, // 673
  {wxDC_GetPPI, "wxDC", "getPPI", 1}, // 674
  {NULL, "", "", 0}, // 675
  {wxDC_GetSize, "wxDC", "getSize", 1}, // 676
  {NULL, "", "", 0}, // 677
  {wxDC_GetSizeMM, "wxDC", "getSizeMM", 1}, // 678
  {wxDC_GetTextBackground, "wxDC", "getTextBackground", 1}, // 679
  {wxDC_GetTextExtent_4, "wxDC", "getTextExtent", 3}, // 680
  {wxDC_GetTextExtent_1, "wxDC", "getTextExtent", 2}, // 681
  {wxDC_GetTextForeground, "wxDC", "getTextForeground", 1}, // 682
  {wxDC_GetUserScale, "wxDC", "getUserScale", 1}, // 683
  {wxDC_GradientFillConcentric_3, "wxDC", "gradientFillConcentric", 4}, // 684
  {wxDC_GradientFillConcentric_4, "wxDC", "gradientFillConcentric", 5}, // 685
  {wxDC_GradientFillLinear, "wxDC", "gradientFillLinear", 5}, // 686
  {wxDC_LogicalToDeviceX, "wxDC", "logicalToDeviceX", 2}, // 687
  {wxDC_LogicalToDeviceXRel, "wxDC", "logicalToDeviceXRel", 2}, // 688
  {wxDC_LogicalToDeviceY, "wxDC", "logicalToDeviceY", 2}, // 689
  {wxDC_LogicalToDeviceYRel, "wxDC", "logicalToDeviceYRel", 2}, // 690
  {wxDC_MaxX, "wxDC", "maxX", 1}, // 691
  {wxDC_MaxY, "wxDC", "maxY", 1}, // 692
  {wxDC_MinX, "wxDC", "minX", 1}, // 693
  {wxDC_MinY, "wxDC", "minY", 1}, // 694
  {wxDC_IsOk, "wxDC", "isOk", 1}, // 695
  {wxDC_ResetBoundingBox, "wxDC", "resetBoundingBox", 1}, // 696
  {wxDC_SetAxisOrientation, "wxDC", "setAxisOrientation", 3}, // 697
  {wxDC_SetBackground, "wxDC", "setBackground", 2}, // 698
  {wxDC_SetBackgroundMode, "wxDC", "setBackgroundMode", 2}, // 699
  {wxDC_SetBrush, "wxDC", "setBrush", 2}, // 700
  {NULL, "", "", 0}, // 701
  {wxDC_SetClippingRegion_2, "wxDC", "setClippingRegion", 3}, // 702
  {wxDC_SetClippingRegion_1, "wxDC", "setClippingRegion", 2}, // 703
  {wxDC_SetDeviceOrigin, "wxDC", "setDeviceOrigin", 3}, // 704
  {wxDC_SetFont, "wxDC", "setFont", 2}, // 705
  {wxDC_SetLayoutDirection, "wxDC", "setLayoutDirection", 2}, // 706
  {wxDC_SetLogicalFunction, "wxDC", "setLogicalFunction", 2}, // 707
  {wxDC_SetMapMode, "wxDC", "setMapMode", 2}, // 708
  {wxDC_SetPalette, "wxDC", "setPalette", 2}, // 709
  {wxDC_SetPen, "wxDC", "setPen", 2}, // 710
  {wxDC_SetTextBackground, "wxDC", "setTextBackground", 2}, // 711
  {wxDC_SetTextForeground, "wxDC", "setTextForeground", 2}, // 712
  {wxDC_SetUserScale, "wxDC", "setUserScale", 3}, // 713
  {wxDC_StartDoc, "wxDC", "startDoc", 2}, // 714
  {wxDC_StartPage, "wxDC", "startPage", 1}, // 715
  {wxMirrorDC_new, "wxMirrorDC", "new", 2}, // 716
  {NULL, "wxMirrorDC", "'Destroy'", 1}, // 717 obj destructor wxMirrorDC_destroy
  {wxScreenDC_new, "wxScreenDC", "new", 0}, // 718
  {NULL, "wxScreenDC", "'Destroy'", 1}, // 719 obj destructor wxScreenDC_destroy
#if wxUSE_POSTSCRIPT
  {wxPostScriptDC_new_0, "wxPostScriptDC", "new", 0}, // 720
#else
  {NULL, "wxPostScriptDC", "new", 0}, // 720
#endif // wxUSE_POSTSCRIPT
#if wxUSE_POSTSCRIPT
  {wxPostScriptDC_new_1, "wxPostScriptDC", "new", 1}, // 721
#else
  {NULL, "wxPostScriptDC", "new", 0}, // 721
#endif // wxUSE_POSTSCRIPT
#if wxUSE_POSTSCRIPT
  {NULL, "wxPostScriptDC", "'Destroy'", 1}, // 722 obj destructor wxPostScriptDC_destroy
#else
  {NULL, "wxPostScriptDC", "'Destroy'", 0}, // 722
#endif // wxUSE_POSTSCRIPT
  {wxWindowDC_new, "wxWindowDC", "new", 1}, // 723
  {NULL, "wxWindowDC", "'Destroy'", 1}, // 724 obj destructor wxWindowDC_destroy
  {wxClientDC_new, "wxClientDC", "new", 1}, // 725
  {NULL, "wxClientDC", "'Destroy'", 1}, // 726 obj destructor wxClientDC_destroy
  {wxPaintDC_new, "wxPaintDC", "new", 1}, // 727
  {NULL, "wxPaintDC", "'Destroy'", 1}, // 728 obj destructor wxPaintDC_destroy
  {wxMemoryDC_new_0, "wxMemoryDC", "new", 0}, // 729
  {wxMemoryDC_new_1, "wxMemoryDC", "new", 1}, // 730
  {NULL, "", "", 0}, // 731
  {wxMemoryDC_SelectObject, "wxMemoryDC", "selectObject", 2}, // 732
  {wxMemoryDC_SelectObjectAsSource, "wxMemoryDC", "selectObjectAsSource", 2}, // 733
  {NULL, "wxMemoryDC", "'Destroy'", 1}, // 734 obj destructor wxMemoryDC_destroy
  {wxBufferedDC_new_0, "wxBufferedDC", "new", 0}, // 735
  {wxBufferedDC_new_3, "wxBufferedDC", "new", 3}, // 736
  {wxBufferedDC_new_2, "wxBufferedDC", "new", 2}, // 737
  {NULL, "wxBufferedDC", "destroy", 1}, // 738 obj destructor wxBufferedDC_destruct
  {wxBufferedDC_Init_3, "wxBufferedDC", "init", 4}, // 739
  {wxBufferedDC_Init_2, "wxBufferedDC", "init", 3}, // 740
  {wxBufferedPaintDC_new_3, "wxBufferedPaintDC", "new", 3}, // 741
  {wxBufferedPaintDC_new_2, "wxBufferedPaintDC", "new", 2}, // 742
  {NULL, "wxBufferedPaintDC", "destroy", 1}, // 743 obj destructor wxBufferedPaintDC_destruct
#if wxUSE_GRAPHICS_CONTEXT
  {NULL, "wxGraphicsObject", "destroy", 1}, // 744 obj destructor wxGraphicsObject_destruct
#else
  {NULL, "wxGraphicsObject", "destroy", 0}, // 744
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsObject_GetRenderer, "wxGraphicsObject", "getRenderer", 1}, // 745
#else
  {NULL, "wxGraphicsObject", "getRenderer", 0}, // 745
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsObject_IsNull, "wxGraphicsObject", "isNull", 1}, // 746
#else
  {NULL, "wxGraphicsObject", "isNull", 0}, // 746
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {NULL, "wxGraphicsContext", "destroy", 1}, // 747 obj destructor wxGraphicsContext_destruct
#else
  {NULL, "wxGraphicsContext", "destroy", 0}, // 747
#endif // wxUSE_GRAPHICS_CONTEXT
  {NULL, "", "", 0}, // 748
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_Create_STAT_1, "wxGraphicsContext", "create", 1}, // 749
#else
  {NULL, "wxGraphicsContext", "create", 0}, // 749
#endif // wxUSE_GRAPHICS_CONTEXT
  {NULL, "", "", 0}, // 750
  {NULL, "", "", 0}, // 751
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_Create_STAT_0, "wxGraphicsContext", "create", 0}, // 752
#else
  {NULL, "wxGraphicsContext", "create", 0}, // 752
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_CreatePen, "wxGraphicsContext", "createPen", 2}, // 753
#else
  {NULL, "wxGraphicsContext", "createPen", 0}, // 753
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_CreateBrush, "wxGraphicsContext", "createBrush", 2}, // 754
#else
  {NULL, "wxGraphicsContext", "createBrush", 0}, // 754
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_CreateRadialGradientBrush_7, "wxGraphicsContext", "createRadialGradientBrush", 8}, // 755
#else
  {NULL, "wxGraphicsContext", "createRadialGradientBrush", 0}, // 755
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_CreateRadialGradientBrush_6, "wxGraphicsContext", "createRadialGradientBrush", 7}, // 756
#else
  {NULL, "wxGraphicsContext", "createRadialGradientBrush", 0}, // 756
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_CreateLinearGradientBrush_6, "wxGraphicsContext", "createLinearGradientBrush", 7}, // 757
#else
  {NULL, "wxGraphicsContext", "createLinearGradientBrush", 0}, // 757
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_CreateLinearGradientBrush_5, "wxGraphicsContext", "createLinearGradientBrush", 6}, // 758
#else
  {NULL, "wxGraphicsContext", "createLinearGradientBrush", 0}, // 758
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_CreateFont_2, "wxGraphicsContext", "createFont", 3}, // 759
#else
  {NULL, "wxGraphicsContext", "createFont", 0}, // 759
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_CreateFont_3, "wxGraphicsContext", "createFont", 4}, // 760
#else
  {NULL, "wxGraphicsContext", "createFont", 0}, // 760
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_CreateMatrix, "wxGraphicsContext", "createMatrix", 2}, // 761
#else
  {NULL, "wxGraphicsContext", "createMatrix", 0}, // 761
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_CreatePath, "wxGraphicsContext", "createPath", 1}, // 762
#else
  {NULL, "wxGraphicsContext", "createPath", 0}, // 762
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_Clip_1, "wxGraphicsContext", "clip", 2}, // 763
#else
  {NULL, "wxGraphicsContext", "clip", 0}, // 763
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_Clip_4, "wxGraphicsContext", "clip", 5}, // 764
#else
  {NULL, "wxGraphicsContext", "clip", 0}, // 764
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_ResetClip, "wxGraphicsContext", "resetClip", 1}, // 765
#else
  {NULL, "wxGraphicsContext", "resetClip", 0}, // 765
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_DrawBitmap, "wxGraphicsContext", "drawBitmap", 6}, // 766
#else
  {NULL, "wxGraphicsContext", "drawBitmap", 0}, // 766
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_DrawEllipse, "wxGraphicsContext", "drawEllipse", 5}, // 767
#else
  {NULL, "wxGraphicsContext", "drawEllipse", 0}, // 767
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_DrawIcon, "wxGraphicsContext", "drawIcon", 6}, // 768
#else
  {NULL, "wxGraphicsContext", "drawIcon", 0}, // 768
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_DrawLines, "wxGraphicsContext", "drawLines", 3}, // 769
#else
  {NULL, "wxGraphicsContext", "drawLines", 0}, // 769
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_DrawPath, "wxGraphicsContext", "drawPath", 3}, // 770
#else
  {NULL, "wxGraphicsContext", "drawPath", 0}, // 770
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_DrawRectangle, "wxGraphicsContext", "drawRectangle", 5}, // 771
#else
  {NULL, "wxGraphicsContext", "drawRectangle", 0}, // 771
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_DrawRoundedRectangle, "wxGraphicsContext", "drawRoundedRectangle", 6}, // 772
#else
  {NULL, "wxGraphicsContext", "drawRoundedRectangle", 0}, // 772
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_DrawText_3, "wxGraphicsContext", "drawText", 4}, // 773
#else
  {NULL, "wxGraphicsContext", "drawText", 0}, // 773
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_DrawText_4_0, "wxGraphicsContext", "drawText", 5}, // 774
#else
  {NULL, "wxGraphicsContext", "drawText", 0}, // 774
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_DrawText_4_1, "wxGraphicsContext", "drawText", 5}, // 775
#else
  {NULL, "wxGraphicsContext", "drawText", 0}, // 775
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_DrawText_5, "wxGraphicsContext", "drawText", 6}, // 776
#else
  {NULL, "wxGraphicsContext", "drawText", 0}, // 776
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_FillPath, "wxGraphicsContext", "fillPath", 3}, // 777
#else
  {NULL, "wxGraphicsContext", "fillPath", 0}, // 777
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_StrokePath, "wxGraphicsContext", "strokePath", 2}, // 778
#else
  {NULL, "wxGraphicsContext", "strokePath", 0}, // 778
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_GetPartialTextExtents, "wxGraphicsContext", "getPartialTextExtents", 2}, // 779
#else
  {NULL, "wxGraphicsContext", "getPartialTextExtents", 0}, // 779
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_GetTextExtent, "wxGraphicsContext", "getTextExtent", 2}, // 780
#else
  {NULL, "wxGraphicsContext", "getTextExtent", 0}, // 780
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_Rotate, "wxGraphicsContext", "rotate", 2}, // 781
#else
  {NULL, "wxGraphicsContext", "rotate", 0}, // 781
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_Scale, "wxGraphicsContext", "scale", 3}, // 782
#else
  {NULL, "wxGraphicsContext", "scale", 0}, // 782
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_Translate, "wxGraphicsContext", "translate", 3}, // 783
#else
  {NULL, "wxGraphicsContext", "translate", 0}, // 783
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_GetTransform, "wxGraphicsContext", "getTransform", 1}, // 784
#else
  {NULL, "wxGraphicsContext", "getTransform", 0}, // 784
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_SetTransform, "wxGraphicsContext", "setTransform", 2}, // 785
#else
  {NULL, "wxGraphicsContext", "setTransform", 0}, // 785
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_ConcatTransform, "wxGraphicsContext", "concatTransform", 2}, // 786
#else
  {NULL, "wxGraphicsContext", "concatTransform", 0}, // 786
#endif // wxUSE_GRAPHICS_CONTEXT
  {NULL, "", "", 0}, // 787
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_SetBrush, "wxGraphicsContext", "setBrush", 2}, // 788
#else
  {NULL, "wxGraphicsContext", "setBrush", 0}, // 788
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_SetFont_2, "wxGraphicsContext", "setFont", 3}, // 789
#else
  {NULL, "wxGraphicsContext", "setFont", 0}, // 789
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_SetFont_1, "wxGraphicsContext", "setFont", 2}, // 790
#else
  {NULL, "wxGraphicsContext", "setFont", 0}, // 790
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_SetPen, "wxGraphicsContext", "setPen", 2}, // 791
#else
  {NULL, "wxGraphicsContext", "setPen", 0}, // 791
#endif // wxUSE_GRAPHICS_CONTEXT
  {NULL, "", "", 0}, // 792
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_StrokeLine, "wxGraphicsContext", "strokeLine", 5}, // 793
#else
  {NULL, "wxGraphicsContext", "strokeLine", 0}, // 793
#endif // wxUSE_GRAPHICS_CONTEXT
  {NULL, "", "", 0}, // 794
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsContext_StrokeLines, "wxGraphicsContext", "strokeLines", 2}, // 795
#else
  {NULL, "wxGraphicsContext", "strokeLines", 0}, // 795
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsMatrix_Concat, "wxGraphicsMatrix", "concat", 2}, // 796
#else
  {NULL, "wxGraphicsMatrix", "concat", 0}, // 796
#endif // wxUSE_GRAPHICS_CONTEXT
  {NULL, "", "", 0}, // 797
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsMatrix_Get, "wxGraphicsMatrix", "get", 1}, // 798
#else
  {NULL, "wxGraphicsMatrix", "get", 0}, // 798
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsMatrix_Invert, "wxGraphicsMatrix", "invert", 1}, // 799
#else
  {NULL, "wxGraphicsMatrix", "invert", 0}, // 799
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsMatrix_IsEqual, "wxGraphicsMatrix", "isEqual", 2}, // 800
#else
  {NULL, "wxGraphicsMatrix", "isEqual", 0}, // 800
#endif // wxUSE_GRAPHICS_CONTEXT
  {NULL, "", "", 0}, // 801
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsMatrix_IsIdentity, "wxGraphicsMatrix", "isIdentity", 1}, // 802
#else
  {NULL, "wxGraphicsMatrix", "isIdentity", 0}, // 802
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsMatrix_Rotate, "wxGraphicsMatrix", "rotate", 2}, // 803
#else
  {NULL, "wxGraphicsMatrix", "rotate", 0}, // 803
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsMatrix_Scale, "wxGraphicsMatrix", "scale", 3}, // 804
#else
  {NULL, "wxGraphicsMatrix", "scale", 0}, // 804
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsMatrix_Translate, "wxGraphicsMatrix", "translate", 3}, // 805
#else
  {NULL, "wxGraphicsMatrix", "translate", 0}, // 805
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsMatrix_Set, "wxGraphicsMatrix", "set", 2}, // 806
#else
  {NULL, "wxGraphicsMatrix", "set", 0}, // 806
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsMatrix_TransformPoint, "wxGraphicsMatrix", "transformPoint", 1}, // 807
#else
  {NULL, "wxGraphicsMatrix", "transformPoint", 0}, // 807
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsMatrix_TransformDistance, "wxGraphicsMatrix", "transformDistance", 1}, // 808
#else
  {NULL, "wxGraphicsMatrix", "transformDistance", 0}, // 808
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_MoveToPoint_2, "wxGraphicsPath", "moveToPoint", 3}, // 809
#else
  {NULL, "wxGraphicsPath", "moveToPoint", 0}, // 809
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_MoveToPoint_1, "wxGraphicsPath", "moveToPoint", 2}, // 810
#else
  {NULL, "wxGraphicsPath", "moveToPoint", 0}, // 810
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_AddArc_6, "wxGraphicsPath", "addArc", 7}, // 811
#else
  {NULL, "wxGraphicsPath", "addArc", 0}, // 811
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_AddArc_5, "wxGraphicsPath", "addArc", 6}, // 812
#else
  {NULL, "wxGraphicsPath", "addArc", 0}, // 812
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_AddArcToPoint, "wxGraphicsPath", "addArcToPoint", 6}, // 813
#else
  {NULL, "wxGraphicsPath", "addArcToPoint", 0}, // 813
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_AddCircle, "wxGraphicsPath", "addCircle", 4}, // 814
#else
  {NULL, "wxGraphicsPath", "addCircle", 0}, // 814
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_AddCurveToPoint_6, "wxGraphicsPath", "addCurveToPoint", 7}, // 815
#else
  {NULL, "wxGraphicsPath", "addCurveToPoint", 0}, // 815
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_AddCurveToPoint_3, "wxGraphicsPath", "addCurveToPoint", 4}, // 816
#else
  {NULL, "wxGraphicsPath", "addCurveToPoint", 0}, // 816
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_AddEllipse, "wxGraphicsPath", "addEllipse", 5}, // 817
#else
  {NULL, "wxGraphicsPath", "addEllipse", 0}, // 817
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_AddLineToPoint_2, "wxGraphicsPath", "addLineToPoint", 3}, // 818
#else
  {NULL, "wxGraphicsPath", "addLineToPoint", 0}, // 818
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_AddLineToPoint_1, "wxGraphicsPath", "addLineToPoint", 2}, // 819
#else
  {NULL, "wxGraphicsPath", "addLineToPoint", 0}, // 819
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_AddPath, "wxGraphicsPath", "addPath", 2}, // 820
#else
  {NULL, "wxGraphicsPath", "addPath", 0}, // 820
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_AddQuadCurveToPoint, "wxGraphicsPath", "addQuadCurveToPoint", 5}, // 821
#else
  {NULL, "wxGraphicsPath", "addQuadCurveToPoint", 0}, // 821
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_AddRectangle, "wxGraphicsPath", "addRectangle", 5}, // 822
#else
  {NULL, "wxGraphicsPath", "addRectangle", 0}, // 822
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_AddRoundedRectangle, "wxGraphicsPath", "addRoundedRectangle", 6}, // 823
#else
  {NULL, "wxGraphicsPath", "addRoundedRectangle", 0}, // 823
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_CloseSubpath, "wxGraphicsPath", "closeSubpath", 1}, // 824
#else
  {NULL, "wxGraphicsPath", "closeSubpath", 0}, // 824
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_Contains_2, "wxGraphicsPath", "contains", 3}, // 825
#else
  {NULL, "wxGraphicsPath", "contains", 0}, // 825
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_Contains_3, "wxGraphicsPath", "contains", 4}, // 826
#else
  {NULL, "wxGraphicsPath", "contains", 0}, // 826
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_GetBox, "wxGraphicsPath", "getBox", 1}, // 827
#else
  {NULL, "wxGraphicsPath", "getBox", 0}, // 827
#endif // wxUSE_GRAPHICS_CONTEXT
  {NULL, "", "", 0}, // 828
  {NULL, "", "", 0}, // 829
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_GetCurrentPoint, "wxGraphicsPath", "getCurrentPoint", 1}, // 830
#else
  {NULL, "wxGraphicsPath", "getCurrentPoint", 0}, // 830
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsPath_Transform, "wxGraphicsPath", "transform", 2}, // 831
#else
  {NULL, "wxGraphicsPath", "transform", 0}, // 831
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsRenderer_GetDefaultRenderer, "wxGraphicsRenderer", "getDefaultRenderer", 0}, // 832
#else
  {NULL, "wxGraphicsRenderer", "getDefaultRenderer", 0}, // 832
#endif // wxUSE_GRAPHICS_CONTEXT
  {NULL, "", "", 0}, // 833
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsRenderer_CreateContext, "wxGraphicsRenderer", "createContext", 2}, // 834
#else
  {NULL, "wxGraphicsRenderer", "createContext", 0}, // 834
#endif // wxUSE_GRAPHICS_CONTEXT
  {NULL, "", "", 0}, // 835
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsRenderer_CreateBrush, "wxGraphicsRenderer", "createBrush", 2}, // 836
#else
  {NULL, "wxGraphicsRenderer", "createBrush", 0}, // 836
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsRenderer_CreateLinearGradientBrush, "wxGraphicsRenderer", "createLinearGradientBrush", 6}, // 837
#else
  {NULL, "wxGraphicsRenderer", "createLinearGradientBrush", 0}, // 837
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsRenderer_CreateRadialGradientBrush, "wxGraphicsRenderer", "createRadialGradientBrush", 7}, // 838
#else
  {NULL, "wxGraphicsRenderer", "createRadialGradientBrush", 0}, // 838
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsRenderer_CreateFont_2, "wxGraphicsRenderer", "createFont", 3}, // 839
#else
  {NULL, "wxGraphicsRenderer", "createFont", 0}, // 839
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsRenderer_CreateFont_3, "wxGraphicsRenderer", "createFont", 4}, // 840
#else
  {NULL, "wxGraphicsRenderer", "createFont", 0}, // 840
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsRenderer_CreateMatrix, "wxGraphicsRenderer", "createMatrix", 2}, // 841
#else
  {NULL, "wxGraphicsRenderer", "createMatrix", 0}, // 841
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsRenderer_CreatePath, "wxGraphicsRenderer", "createPath", 1}, // 842
#else
  {NULL, "wxGraphicsRenderer", "createPath", 0}, // 842
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsGradientStops_new, "wxGraphicsGradientStops", "new", 1}, // 843
#else
  {NULL, "wxGraphicsGradientStops", "new", 0}, // 843
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsGradientStops_Item, "wxGraphicsGradientStops", "item", 2}, // 844
#else
  {NULL, "wxGraphicsGradientStops", "item", 0}, // 844
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsGradientStops_GetCount, "wxGraphicsGradientStops", "getCount", 1}, // 845
#else
  {NULL, "wxGraphicsGradientStops", "getCount", 0}, // 845
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsGradientStops_SetStartColour, "wxGraphicsGradientStops", "setStartColour", 2}, // 846
#else
  {NULL, "wxGraphicsGradientStops", "setStartColour", 0}, // 846
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsGradientStops_GetStartColour, "wxGraphicsGradientStops", "getStartColour", 1}, // 847
#else
  {NULL, "wxGraphicsGradientStops", "getStartColour", 0}, // 847
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsGradientStops_SetEndColour, "wxGraphicsGradientStops", "setEndColour", 2}, // 848
#else
  {NULL, "wxGraphicsGradientStops", "setEndColour", 0}, // 848
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsGradientStops_GetEndColour, "wxGraphicsGradientStops", "getEndColour", 1}, // 849
#else
  {NULL, "wxGraphicsGradientStops", "getEndColour", 0}, // 849
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGraphicsGradientStops_Add, "wxGraphicsGradientStops", "add", 3}, // 850
#else
  {NULL, "wxGraphicsGradientStops", "add", 0}, // 850
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {NULL, "wxGraphicsGradientStops", "'Destroy'", 1}, // 851 obj destructor wxGraphicsGradientStops_destroy
#else
  {NULL, "wxGraphicsGradientStops", "'Destroy'", 0}, // 851
#endif // wxUSE_GRAPHICS_CONTEXT
  {wxMenuBar_new_0, "wxMenuBar", "new", 0}, // 852
  {wxMenuBar_new_1, "wxMenuBar", "new", 1}, // 853
  {NULL, "", "", 0}, // 854
  {NULL, "wxMenuBar", "destroy", 1}, // 855 obj destructor wxMenuBar_destruct
  {wxMenuBar_Append, "wxMenuBar", "append", 3}, // 856
  {wxMenuBar_Check, "wxMenuBar", "check", 3}, // 857
  {wxMenuBar_Enable, "wxMenuBar", "enable", 3}, // 858
  {wxMenuBar_EnableTop, "wxMenuBar", "enableTop", 3}, // 859
  {wxMenuBar_FindMenu, "wxMenuBar", "findMenu", 2}, // 860
  {wxMenuBar_FindMenuItem, "wxMenuBar", "findMenuItem", 3}, // 861
  {wxMenuBar_FindItem, "wxMenuBar", "findItem", 2}, // 862
  {wxMenuBar_GetHelpString, "wxMenuBar", "getHelpString", 2}, // 863
  {wxMenuBar_GetLabel, "wxMenuBar", "getLabel", 2}, // 864
  {wxMenuBar_GetMenuLabel, "wxMenuBar", "getMenuLabel", 2}, // 865
  {wxMenuBar_GetMenuLabelText, "wxMenuBar", "getMenuLabelText", 2}, // 866
  {wxMenuBar_GetMenu, "wxMenuBar", "getMenu", 2}, // 867
  {wxMenuBar_GetMenuCount, "wxMenuBar", "getMenuCount", 1}, // 868
  {wxMenuBar_Insert, "wxMenuBar", "insert", 4}, // 869
  {wxMenuBar_IsChecked, "wxMenuBar", "isChecked", 2}, // 870
#if defined(__WXMAC__)
  {wxMenuBar_SetAutoWindowMenu, "wxMenuBar", "setAutoWindowMenu", 1}, // 871
#else
  {NULL, "wxMenuBar", "setAutoWindowMenu", 0}, // 871
#endif // defined(__WXMAC__)
#if defined(__WXMAC__)
  {wxMenuBar_GetAutoWindowMenu, "wxMenuBar", "getAutoWindowMenu", 0}, // 872
#else
  {NULL, "wxMenuBar", "getAutoWindowMenu", 0}, // 872
#endif // defined(__WXMAC__)
#if defined(__WXMAC__)
  {wxMenuBar_OSXGetAppleMenu, "wxMenuBar", "oSXGetAppleMenu", 1}, // 873
#else
  {NULL, "wxMenuBar", "oSXGetAppleMenu", 0}, // 873
#endif // defined(__WXMAC__)
#if defined(__WXMAC__)
  {wxMenuBar_MacGetCommonMenuBar, "wxMenuBar", "macGetCommonMenuBar", 0}, // 874
#else
  {NULL, "wxMenuBar", "macGetCommonMenuBar", 0}, // 874
#endif // defined(__WXMAC__)
#if defined(__WXMAC__)
  {wxMenuBar_MacSetCommonMenuBar, "wxMenuBar", "macSetCommonMenuBar", 1}, // 875
#else
  {NULL, "wxMenuBar", "macSetCommonMenuBar", 0}, // 875
#endif // defined(__WXMAC__)
  {wxMenuBar_IsEnabled, "wxMenuBar", "isEnabled", 2}, // 876
  {wxMenuBar_Remove, "wxMenuBar", "remove", 2}, // 877
  {wxMenuBar_Replace, "wxMenuBar", "replace", 4}, // 878
  {wxMenuBar_SetHelpString, "wxMenuBar", "setHelpString", 3}, // 879
  {wxMenuBar_SetLabel, "wxMenuBar", "setLabel", 3}, // 880
  {wxMenuBar_SetMenuLabel, "wxMenuBar", "setMenuLabel", 3}, // 881
  {wxControl_GetLabel, "wxControl", "getLabel", 1}, // 882
  {wxControl_SetLabel, "wxControl", "setLabel", 2}, // 883
  {wxControlWithItems_Append_1, "wxControlWithItems", "append", 2}, // 884
  {wxControlWithItems_Append_2, "wxControlWithItems", "append", 3}, // 885
  {wxControlWithItems_appendStrings_1, "wxControlWithItems", "appendStrings", 2}, // 886
  {wxControlWithItems_appendStrings_2, "wxControlWithItems", "appendStrings", 3}, // 887
  {wxControlWithItems_Clear, "wxControlWithItems", "clear", 1}, // 888
  {wxControlWithItems_Delete, "wxControlWithItems", "delete", 2}, // 889
  {wxControlWithItems_FindString, "wxControlWithItems", "findString", 3}, // 890
  {wxControlWithItems_getClientData, "wxControlWithItems", "getClientData", 2}, // 891
  {wxControlWithItems_setClientData, "wxControlWithItems", "setClientData", 3}, // 892
  {wxControlWithItems_GetCount, "wxControlWithItems", "getCount", 1}, // 893
  {wxControlWithItems_GetSelection, "wxControlWithItems", "getSelection", 1}, // 894
  {wxControlWithItems_GetString, "wxControlWithItems", "getString", 2}, // 895
  {wxControlWithItems_GetStringSelection, "wxControlWithItems", "getStringSelection", 1}, // 896
  {wxControlWithItems_Insert_2, "wxControlWithItems", "insert", 3}, // 897
  {wxControlWithItems_Insert_3, "wxControlWithItems", "insert", 4}, // 898
  {wxControlWithItems_insertStrings_2, "wxControlWithItems", "insertStrings", 3}, // 899
  {wxControlWithItems_insertStrings_3, "wxControlWithItems", "insertStrings", 4}, // 900
  {wxControlWithItems_IsEmpty, "wxControlWithItems", "isEmpty", 1}, // 901
  {wxControlWithItems_Select, "wxControlWithItems", "select", 2}, // 902
  {wxControlWithItems_SetSelection, "wxControlWithItems", "setSelection", 2}, // 903
  {wxControlWithItems_SetString, "wxControlWithItems", "setString", 3}, // 904
  {wxControlWithItems_SetStringSelection, "wxControlWithItems", "setStringSelection", 2}, // 905
  {wxMenu_new_0, "wxMenu", "new", 0}, // 906
  {wxMenu_new_1, "wxMenu", "new", 1}, // 907
  {wxMenu_new_2, "wxMenu", "new", 2}, // 908
  {NULL, "wxMenu", "destroy", 1}, // 909 obj destructor wxMenu_destruct
  {wxMenu_Append_3, "wxMenu", "append", 4}, // 910
  {wxMenu_Append_4, "wxMenu", "append", 5}, // 911
  {wxMenu_Append_1, "wxMenu", "append", 2}, // 912
  {wxMenu_AppendCheckItem, "wxMenu", "appendCheckItem", 4}, // 913
  {wxMenu_AppendRadioItem, "wxMenu", "appendRadioItem", 4}, // 914
  {wxMenu_AppendSeparator, "wxMenu", "appendSeparator", 1}, // 915
  {wxMenu_Break, "wxMenu", "break", 1}, // 916
  {wxMenu_Check, "wxMenu", "check", 3}, // 917
  {wxMenu_Delete_1_0, "wxMenu", "delete", 2}, // 918
  {wxMenu_Delete_1_1, "wxMenu", "delete", 2}, // 919
  {wxMenu_Destroy_1_0, "wxMenu", "'Destroy'", 2}, // 920
  {wxMenu_Destroy_1_1, "wxMenu", "'Destroy'", 2}, // 921
  {wxMenu_Enable, "wxMenu", "enable", 3}, // 922
  {wxMenu_FindItem_1, "wxMenu", "findItem", 2}, // 923
  {wxMenu_FindItem_2, "wxMenu", "findItem", 2}, // 924
  {wxMenu_FindItemByPosition, "wxMenu", "findItemByPosition", 2}, // 925
  {wxMenu_GetHelpString, "wxMenu", "getHelpString", 2}, // 926
  {wxMenu_GetLabel, "wxMenu", "getLabel", 2}, // 927
  {wxMenu_GetMenuItemCount, "wxMenu", "getMenuItemCount", 1}, // 928
  {NULL, "", "", 0}, // 929
  {wxMenu_GetMenuItems, "wxMenu", "getMenuItems", 1}, // 930
  {wxMenu_GetTitle, "wxMenu", "getTitle", 1}, // 931
  {wxMenu_Insert_2, "wxMenu", "insert", 3}, // 932
  {wxMenu_Insert_3, "wxMenu", "insert", 4}, // 933
  {wxMenu_Insert_5, "wxMenu", "insert", 6}, // 934
  {wxMenu_InsertCheckItem, "wxMenu", "insertCheckItem", 5}, // 935
  {wxMenu_InsertRadioItem, "wxMenu", "insertRadioItem", 5}, // 936
  {wxMenu_InsertSeparator, "wxMenu", "insertSeparator", 2}, // 937
  {wxMenu_IsChecked, "wxMenu", "isChecked", 2}, // 938
  {wxMenu_IsEnabled, "wxMenu", "isEnabled", 2}, // 939
  {wxMenu_Prepend_1, "wxMenu", "prepend", 2}, // 940
  {wxMenu_Prepend_2, "wxMenu", "prepend", 3}, // 941
  {wxMenu_Prepend_4, "wxMenu", "prepend", 5}, // 942
  {wxMenu_PrependCheckItem, "wxMenu", "prependCheckItem", 4}, // 943
  {wxMenu_PrependRadioItem, "wxMenu", "prependRadioItem", 4}, // 944
  {wxMenu_PrependSeparator, "wxMenu", "prependSeparator", 1}, // 945
  {wxMenu_Remove_1_0, "wxMenu", "remove", 2}, // 946
  {wxMenu_Remove_1_1, "wxMenu", "remove", 2}, // 947
  {wxMenu_SetHelpString, "wxMenu", "setHelpString", 3}, // 948
  {wxMenu_SetLabel, "wxMenu", "setLabel", 3}, // 949
  {wxMenu_SetTitle, "wxMenu", "setTitle", 2}, // 950
  {wxMenuItem_new, "wxMenuItem", "new", 1}, // 951
  {NULL, "wxMenuItem", "destroy", 1}, // 952 obj destructor wxMenuItem_destruct
  {wxMenuItem_Check, "wxMenuItem", "check", 2}, // 953
  {wxMenuItem_Enable, "wxMenuItem", "enable", 2}, // 954
  {wxMenuItem_GetBitmap, "wxMenuItem", "getBitmap", 1}, // 955
  {wxMenuItem_GetHelp, "wxMenuItem", "getHelp", 1}, // 956
  {wxMenuItem_GetId, "wxMenuItem", "getId", 1}, // 957
  {wxMenuItem_GetKind, "wxMenuItem", "getKind", 1}, // 958
  {wxMenuItem_GetLabelText, "wxMenuItem", "getLabelText", 1}, // 959
  {wxMenuItem_GetItemLabel, "wxMenuItem", "getItemLabel", 1}, // 960
  {wxMenuItem_GetItemLabelText, "wxMenuItem", "getItemLabelText", 1}, // 961
  {wxMenuItem_GetMenu, "wxMenuItem", "getMenu", 1}, // 962
  {wxMenuItem_GetSubMenu, "wxMenuItem", "getSubMenu", 1}, // 963
  {wxMenuItem_IsCheckable, "wxMenuItem", "isCheckable", 1}, // 964
  {wxMenuItem_IsChecked, "wxMenuItem", "isChecked", 1}, // 965
  {wxMenuItem_IsEnabled, "wxMenuItem", "isEnabled", 1}, // 966
  {wxMenuItem_IsSeparator, "wxMenuItem", "isSeparator", 1}, // 967
  {wxMenuItem_IsSubMenu, "wxMenuItem", "isSubMenu", 1}, // 968
  {wxMenuItem_SetBitmap, "wxMenuItem", "setBitmap", 2}, // 969
  {wxMenuItem_SetHelp, "wxMenuItem", "setHelp", 2}, // 970
  {wxMenuItem_SetMenu, "wxMenuItem", "setMenu", 2}, // 971
  {wxMenuItem_SetSubMenu, "wxMenuItem", "setSubMenu", 2}, // 972
  {wxMenuItem_SetItemLabel, "wxMenuItem", "setItemLabel", 2}, // 973
  {wxToolBar_AddControl, "wxToolBar", "addControl", 3}, // 974
  {wxToolBar_AddSeparator, "wxToolBar", "addSeparator", 1}, // 975
  {wxToolBar_AddTool_1, "wxToolBar", "addTool", 2}, // 976
  {wxToolBar_AddTool_4, "wxToolBar", "addTool", 5}, // 977
  {wxToolBar_AddTool_5, "wxToolBar", "addTool", 6}, // 978
  {wxToolBar_AddCheckTool, "wxToolBar", "addCheckTool", 5}, // 979
  {wxToolBar_AddRadioTool, "wxToolBar", "addRadioTool", 5}, // 980
  {wxToolBar_AddStretchableSpace, "wxToolBar", "addStretchableSpace", 1}, // 981
  {wxToolBar_InsertStretchableSpace, "wxToolBar", "insertStretchableSpace", 2}, // 982
  {wxToolBar_DeleteTool, "wxToolBar", "deleteTool", 2}, // 983
  {wxToolBar_DeleteToolByPos, "wxToolBar", "deleteToolByPos", 2}, // 984
  {wxToolBar_EnableTool, "wxToolBar", "enableTool", 3}, // 985
  {wxToolBar_FindById, "wxToolBar", "findById", 2}, // 986
  {wxToolBar_FindControl, "wxToolBar", "findControl", 2}, // 987
  {wxToolBar_FindToolForPosition, "wxToolBar", "findToolForPosition", 3}, // 988
  {wxToolBar_GetToolSize, "wxToolBar", "getToolSize", 1}, // 989
  {wxToolBar_GetToolBitmapSize, "wxToolBar", "getToolBitmapSize", 1}, // 990
  {wxToolBar_GetMargins, "wxToolBar", "getMargins", 1}, // 991
  {wxToolBar_GetToolEnabled, "wxToolBar", "getToolEnabled", 2}, // 992
  {wxToolBar_GetToolLongHelp, "wxToolBar", "getToolLongHelp", 2}, // 993
  {wxToolBar_GetToolPacking, "wxToolBar", "getToolPacking", 1}, // 994
  {wxToolBar_GetToolPos, "wxToolBar", "getToolPos", 2}, // 995
  {wxToolBar_GetToolSeparation, "wxToolBar", "getToolSeparation", 1}, // 996
  {wxToolBar_GetToolShortHelp, "wxToolBar", "getToolShortHelp", 2}, // 997
  {wxToolBar_GetToolState, "wxToolBar", "getToolState", 2}, // 998
  {wxToolBar_InsertControl, "wxToolBar", "insertControl", 4}, // 999
  {wxToolBar_InsertSeparator, "wxToolBar", "insertSeparator", 2}, // 1000
  {wxToolBar_InsertTool_5, "wxToolBar", "insertTool", 6}, // 1001
  {wxToolBar_InsertTool_2, "wxToolBar", "insertTool", 3}, // 1002
  {wxToolBar_Realize, "wxToolBar", "realize", 1}, // 1003
  {wxToolBar_RemoveTool, "wxToolBar", "removeTool", 2}, // 1004
  {wxToolBar_SetMargins, "wxToolBar", "setMargins", 3}, // 1005
  {wxToolBar_SetToolBitmapSize, "wxToolBar", "setToolBitmapSize", 2}, // 1006
  {wxToolBar_SetToolLongHelp, "wxToolBar", "setToolLongHelp", 3}, // 1007
  {wxToolBar_SetToolPacking, "wxToolBar", "setToolPacking", 2}, // 1008
  {wxToolBar_SetToolShortHelp, "wxToolBar", "setToolShortHelp", 3}, // 1009
  {wxToolBar_SetToolSeparation, "wxToolBar", "setToolSeparation", 2}, // 1010
  {wxToolBar_ToggleTool, "wxToolBar", "toggleTool", 3}, // 1011
  {wxStatusBar_new_0, "wxStatusBar", "new", 0}, // 1012
  {wxStatusBar_new_2, "wxStatusBar", "new", 2}, // 1013
  {NULL, "wxStatusBar", "destroy", 1}, // 1014 obj destructor wxStatusBar_destruct
  {wxStatusBar_Create, "wxStatusBar", "create", 3}, // 1015
  {wxStatusBar_GetFieldRect, "wxStatusBar", "getFieldRect", 2}, // 1016
  {wxStatusBar_GetFieldsCount, "wxStatusBar", "getFieldsCount", 1}, // 1017
  {wxStatusBar_GetStatusText, "wxStatusBar", "getStatusText", 2}, // 1018
  {wxStatusBar_PopStatusText, "wxStatusBar", "popStatusText", 2}, // 1019
  {wxStatusBar_PushStatusText, "wxStatusBar", "pushStatusText", 3}, // 1020
  {wxStatusBar_SetFieldsCount, "wxStatusBar", "setFieldsCount", 3}, // 1021
  {wxStatusBar_SetMinHeight, "wxStatusBar", "setMinHeight", 2}, // 1022
  {wxStatusBar_SetStatusText, "wxStatusBar", "setStatusText", 3}, // 1023
  {wxStatusBar_SetStatusWidths, "wxStatusBar", "setStatusWidths", 2}, // 1024
  {wxStatusBar_SetStatusStyles, "wxStatusBar", "setStatusStyles", 2}, // 1025
  {wxBitmap_new_0, "wxBitmap", "new", 0}, // 1026
  {NULL, "", "", 0}, // 1027
  {wxBitmap_new_4, "wxBitmap", "new", 4}, // 1028
  {wxBitmap_new_3, "wxBitmap", "new", 3}, // 1029
  {wxBitmap_new_2_1, "wxBitmap", "new", 2}, // 1030
  {wxBitmap_new_2_0, "wxBitmap", "new", 2}, // 1031
  {wxBitmap_new_2_2, "wxBitmap", "new", 2}, // 1032
  {wxBitmap_new_2_3, "wxBitmap", "new", 1}, // 1033
  {NULL, "wxBitmap", "destroy", 1}, // 1034 obj destructor wxBitmap_destruct
  {wxBitmap_ConvertToImage, "wxBitmap", "convertToImage", 1}, // 1035
  {wxBitmap_CopyFromIcon, "wxBitmap", "copyFromIcon", 2}, // 1036
  {wxBitmap_Create_3_0, "wxBitmap", "create", 4}, // 1037
  {wxBitmap_Create_2, "wxBitmap", "create", 3}, // 1038
  {wxBitmap_Create_3_1, "wxBitmap", "create", 4}, // 1039
  {wxBitmap_GetDepth, "wxBitmap", "getDepth", 1}, // 1040
  {wxBitmap_GetHeight, "wxBitmap", "getHeight", 1}, // 1041
  {wxBitmap_GetPalette, "wxBitmap", "getPalette", 1}, // 1042
  {wxBitmap_GetMask, "wxBitmap", "getMask", 1}, // 1043
  {wxBitmap_GetWidth, "wxBitmap", "getWidth", 1}, // 1044
  {wxBitmap_GetSubBitmap, "wxBitmap", "getSubBitmap", 2}, // 1045
  {wxBitmap_LoadFile, "wxBitmap", "loadFile", 3}, // 1046
  {wxBitmap_IsOk, "wxBitmap", "isOk", 1}, // 1047
  {wxBitmap_SaveFile, "wxBitmap", "saveFile", 4}, // 1048
  {wxBitmap_SetDepth, "wxBitmap", "setDepth", 2}, // 1049
  {wxBitmap_SetHeight, "wxBitmap", "setHeight", 2}, // 1050
  {wxBitmap_SetMask, "wxBitmap", "setMask", 2}, // 1051
  {wxBitmap_SetPalette, "wxBitmap", "setPalette", 2}, // 1052
  {wxBitmap_SetWidth, "wxBitmap", "setWidth", 2}, // 1053
  {wxIcon_new_0, "wxIcon", "new", 0}, // 1054
  {wxIcon_new_1, "wxIcon", "new", 1}, // 1055
  {wxIcon_new_2, "wxIcon", "new", 2}, // 1056
  {wxIcon_CopyFromBitmap, "wxIcon", "copyFromBitmap", 2}, // 1057
  {NULL, "wxIcon", "destroy", 1}, // 1058 obj destructor wxIcon_destruct
  {wxIconBundle_new_0, "wxIconBundle", "new", 0}, // 1059
  {wxIconBundle_new_1_1, "wxIconBundle", "new", 1}, // 1060
  {wxIconBundle_new_2, "wxIconBundle", "new", 2}, // 1061
  {NULL, "", "", 0}, // 1062
  {wxIconBundle_new_1_0, "wxIconBundle", "new", 1}, // 1063
  {wxIconBundle_destruct, "wxIconBundle", "destroy", 1}, // 1064
  {wxIconBundle_AddIcon_1_0, "wxIconBundle", "addIcon", 2}, // 1065
  {wxIconBundle_AddIcon_2, "wxIconBundle", "addIcon", 3}, // 1066
  {wxIconBundle_AddIcon_1_1, "wxIconBundle", "addIcon", 2}, // 1067
  {wxIconBundle_GetIcon_2, "wxIconBundle", "getIcon", 3}, // 1068
  {wxIconBundle_GetIcon_1, "wxIconBundle", "getIcon", 2}, // 1069
  {wxCursor_new_0, "wxCursor", "new", 0}, // 1070
  {wxCursor_new_2, "wxCursor", "new", 2}, // 1071
  {wxCursor_new_1_1, "wxCursor", "new", 1}, // 1072
  {wxCursor_new_1_0, "wxCursor", "new", 1}, // 1073
  {NULL, "", "", 0}, // 1074
  {NULL, "wxCursor", "destroy", 1}, // 1075 obj destructor wxCursor_destruct
  {wxCursor_IsOk, "wxCursor", "isOk", 1}, // 1076
  {wxMask_new_0, "wxMask", "new", 0}, // 1077
  {wxMask_new_2_0, "wxMask", "new", 2}, // 1078
  {wxMask_new_1, "wxMask", "new", 1}, // 1079
  {wxMask_new_2_1, "wxMask", "new", 2}, // 1080
  {NULL, "wxMask", "destroy", 1}, // 1081 obj destructor wxMask_destruct
  {wxMask_Create_2_0, "wxMask", "create", 3}, // 1082
  {wxMask_Create_1, "wxMask", "create", 2}, // 1083
  {wxMask_Create_2_1, "wxMask", "create", 3}, // 1084
  {wxImage_new_0, "wxImage", "new", 0}, // 1085
  {wxImage_new_3_1, "wxImage", "new", 3}, // 1086
  {wxImage_new_2_2, "wxImage", "new", 2}, // 1087
  {wxImage_new_3_0, "wxImage", "new", 3}, // 1088
  {wxImage_new_2_1, "wxImage", "new", 2}, // 1089
  {wxImage_new_4, "wxImage", "new", 4}, // 1090
  {wxImage_new_3_3, "wxImage", "new", 3}, // 1091
  {wxImage_new_2_0, "wxImage", "new", 2}, // 1092
  {wxImage_new_3_2, "wxImage", "new", 3}, // 1093
  {NULL, "wxImage", "destroy", 1}, // 1094 obj destructor wxImage_destruct
  {wxImage_Blur, "wxImage", "blur", 2}, // 1095
  {wxImage_BlurHorizontal, "wxImage", "blurHorizontal", 2}, // 1096
  {wxImage_BlurVertical, "wxImage", "blurVertical", 2}, // 1097
  {wxImage_ConvertAlphaToMask_1, "wxImage", "convertAlphaToMask", 2}, // 1098
  {wxImage_ConvertAlphaToMask_4, "wxImage", "convertAlphaToMask", 5}, // 1099
  {wxImage_ConvertToGreyscale_3, "wxImage", "convertToGreyscale", 4}, // 1100
  {wxImage_ConvertToGreyscale_0, "wxImage", "convertToGreyscale", 1}, // 1101
  {wxImage_ConvertToMono, "wxImage", "convertToMono", 4}, // 1102
  {wxImage_Copy, "wxImage", "copy", 1}, // 1103
  {wxImage_Create_3_1, "wxImage", "create", 4}, // 1104
  {wxImage_Create_2_1, "wxImage", "create", 3}, // 1105
  {wxImage_Create_3_0, "wxImage", "create", 4}, // 1106
  {wxImage_Create_2_0, "wxImage", "create", 3}, // 1107
  {wxImage_Create_4, "wxImage", "create", 5}, // 1108
  {wxImage_Create_3_2, "wxImage", "create", 4}, // 1109
  {wxImage_Destroy, "wxImage", "'Destroy'", 1}, // 1110
  {wxImage_FindFirstUnusedColour, "wxImage", "findFirstUnusedColour", 2}, // 1111
  {wxImage_GetImageExtWildcard, "wxImage", "getImageExtWildcard", 0}, // 1112
  {wxImage_GetAlpha_0, "wxImage", "getAlpha", 1}, // 1113
  {wxImage_GetAlpha_2, "wxImage", "getAlpha", 3}, // 1114
  {wxImage_GetBlue, "wxImage", "getBlue", 3}, // 1115
  {wxImage_GetData, "wxImage", "getData", 1}, // 1116
  {wxImage_GetGreen, "wxImage", "getGreen", 3}, // 1117
  {wxImage_GetImageCount, "wxImage", "getImageCount", 2}, // 1118
  {wxImage_GetHeight, "wxImage", "getHeight", 1}, // 1119
  {wxImage_GetMaskBlue, "wxImage", "getMaskBlue", 1}, // 1120
  {wxImage_GetMaskGreen, "wxImage", "getMaskGreen", 1}, // 1121
  {wxImage_GetMaskRed, "wxImage", "getMaskRed", 1}, // 1122
  {wxImage_GetOrFindMaskColour, "wxImage", "getOrFindMaskColour", 1}, // 1123
  {wxImage_GetPalette, "wxImage", "getPalette", 1}, // 1124
  {wxImage_GetRed, "wxImage", "getRed", 3}, // 1125
  {wxImage_GetSubImage, "wxImage", "getSubImage", 2}, // 1126
  {wxImage_GetWidth, "wxImage", "getWidth", 1}, // 1127
  {wxImage_HasAlpha, "wxImage", "hasAlpha", 1}, // 1128
  {wxImage_HasMask, "wxImage", "hasMask", 1}, // 1129
  {wxImage_GetOption, "wxImage", "getOption", 2}, // 1130
  {wxImage_GetOptionInt, "wxImage", "getOptionInt", 2}, // 1131
  {wxImage_HasOption, "wxImage", "hasOption", 2}, // 1132
  {wxImage_InitAlpha, "wxImage", "initAlpha", 1}, // 1133
  {wxImage_InitStandardHandlers, "wxImage", "initStandardHandlers", 0}, // 1134
  {wxImage_IsTransparent, "wxImage", "isTransparent", 4}, // 1135
  {wxImage_LoadFile_2, "wxImage", "loadFile", 3}, // 1136
  {wxImage_LoadFile_3, "wxImage", "loadFile", 4}, // 1137
  {wxImage_IsOk, "wxImage", "isOk", 1}, // 1138
  {wxImage_RemoveHandler, "wxImage", "removeHandler", 1}, // 1139
  {wxImage_Mirror, "wxImage", "mirror", 2}, // 1140
  {wxImage_Replace, "wxImage", "replace", 7}, // 1141
  {wxImage_Rescale, "wxImage", "rescale", 4}, // 1142
  {wxImage_Resize, "wxImage", "resize", 4}, // 1143
  {wxImage_Rotate, "wxImage", "rotate", 4}, // 1144
  {wxImage_RotateHue, "wxImage", "rotateHue", 2}, // 1145
  {wxImage_Rotate90, "wxImage", "rotate90", 2}, // 1146
  {wxImage_SaveFile_2_0, "wxImage", "saveFile", 3}, // 1147
  {wxImage_SaveFile_2_1, "wxImage", "saveFile", 3}, // 1148
  {wxImage_SaveFile_1, "wxImage", "saveFile", 2}, // 1149
  {wxImage_Scale, "wxImage", "scale", 4}, // 1150
  {wxImage_Size, "wxImage", "size", 4}, // 1151
  {wxImage_SetAlpha_1, "wxImage", "setAlpha", 2}, // 1152
  {wxImage_SetAlpha_3, "wxImage", "setAlpha", 4}, // 1153
  {wxImage_SetData_1, "wxImage", "setData", 2}, // 1154
  {wxImage_SetData_3, "wxImage", "setData", 4}, // 1155
  {wxImage_SetMask, "wxImage", "setMask", 2}, // 1156
  {wxImage_SetMaskColour, "wxImage", "setMaskColour", 4}, // 1157
  {wxImage_SetMaskFromImage, "wxImage", "setMaskFromImage", 5}, // 1158
  {wxImage_SetOption_2_1, "wxImage", "setOption", 3}, // 1159
  {wxImage_SetOption_2_0, "wxImage", "setOption", 3}, // 1160
  {wxImage_SetPalette, "wxImage", "setPalette", 2}, // 1161
  {wxImage_SetRGB_5, "wxImage", "setRGB", 6}, // 1162
  {wxImage_SetRGB_4, "wxImage", "setRGB", 5}, // 1163
  {wxBrush_new_0, "wxBrush", "new", 0}, // 1164
  {wxBrush_new_2, "wxBrush", "new", 2}, // 1165
  {NULL, "", "", 0}, // 1166
  {wxBrush_new_1, "wxBrush", "new", 1}, // 1167
  {NULL, "wxBrush", "destroy", 1}, // 1168 obj destructor wxBrush_destruct
  {wxBrush_GetColour, "wxBrush", "getColour", 1}, // 1169
  {wxBrush_GetStipple, "wxBrush", "getStipple", 1}, // 1170
  {wxBrush_GetStyle, "wxBrush", "getStyle", 1}, // 1171
  {wxBrush_IsHatch, "wxBrush", "isHatch", 1}, // 1172
  {wxBrush_IsOk, "wxBrush", "isOk", 1}, // 1173
  {wxBrush_SetColour_1, "wxBrush", "setColour", 2}, // 1174
  {wxBrush_SetColour_3, "wxBrush", "setColour", 4}, // 1175
  {wxBrush_SetStipple, "wxBrush", "setStipple", 2}, // 1176
  {wxBrush_SetStyle, "wxBrush", "setStyle", 2}, // 1177
  {wxPen_new_0, "wxPen", "new", 0}, // 1178
  {wxPen_new_2, "wxPen", "new", 2}, // 1179
  {wxPen_new_1, "wxPen", "new", 1}, // 1180
  {NULL, "wxPen", "destroy", 1}, // 1181 obj destructor wxPen_destruct
  {wxPen_GetCap, "wxPen", "getCap", 1}, // 1182
  {wxPen_GetColour, "wxPen", "getColour", 1}, // 1183
  {wxPen_GetJoin, "wxPen", "getJoin", 1}, // 1184
  {wxPen_GetStyle, "wxPen", "getStyle", 1}, // 1185
  {wxPen_GetWidth, "wxPen", "getWidth", 1}, // 1186
  {wxPen_IsOk, "wxPen", "isOk", 1}, // 1187
  {wxPen_SetCap, "wxPen", "setCap", 2}, // 1188
  {wxPen_SetColour_1, "wxPen", "setColour", 2}, // 1189
  {wxPen_SetColour_3, "wxPen", "setColour", 4}, // 1190
  {wxPen_SetJoin, "wxPen", "setJoin", 2}, // 1191
  {wxPen_SetStyle, "wxPen", "setStyle", 2}, // 1192
  {wxPen_SetWidth, "wxPen", "setWidth", 2}, // 1193
  {wxRegion_new_0, "wxRegion", "new", 0}, // 1194
  {wxRegion_new_4, "wxRegion", "new", 4}, // 1195
  {wxRegion_new_2, "wxRegion", "new", 2}, // 1196
  {wxRegion_new_1_0, "wxRegion", "new", 1}, // 1197
  {NULL, "", "", 0}, // 1198
  {wxRegion_new_1_1, "wxRegion", "new", 1}, // 1199
  {NULL, "", "", 0}, // 1200
  {NULL, "wxRegion", "destroy", 1}, // 1201 obj destructor wxRegion_destruct
  {wxRegion_Clear, "wxRegion", "clear", 1}, // 1202
  {wxRegion_Contains_2, "wxRegion", "contains", 3}, // 1203
  {wxRegion_Contains_1_0, "wxRegion", "contains", 2}, // 1204
  {wxRegion_Contains_4, "wxRegion", "contains", 5}, // 1205
  {wxRegion_Contains_1_1, "wxRegion", "contains", 2}, // 1206
  {wxRegion_ConvertToBitmap, "wxRegion", "convertToBitmap", 1}, // 1207
  {wxRegion_GetBox, "wxRegion", "getBox", 1}, // 1208
  {wxRegion_Intersect_4, "wxRegion", "intersect", 5}, // 1209
  {wxRegion_Intersect_1_0, "wxRegion", "intersect", 2}, // 1210
  {wxRegion_Intersect_1_1, "wxRegion", "intersect", 2}, // 1211
  {wxRegion_IsEmpty, "wxRegion", "isEmpty", 1}, // 1212
  {wxRegion_Subtract_1_0, "wxRegion", "subtract", 2}, // 1213
  {wxRegion_Subtract_1_1, "wxRegion", "subtract", 2}, // 1214
  {wxRegion_Offset_2, "wxRegion", "offset", 3}, // 1215
  {wxRegion_Offset_1, "wxRegion", "offset", 2}, // 1216
  {wxRegion_Union_4, "wxRegion", "union", 5}, // 1217
  {wxRegion_Union_1_1, "wxRegion", "union", 2}, // 1218
  {wxRegion_Union_1_0, "wxRegion", "union", 2}, // 1219
  {NULL, "", "", 0}, // 1220
  {wxRegion_Union_3, "wxRegion", "union", 4}, // 1221
  {wxRegion_Xor_4, "wxRegion", "'Xor'", 5}, // 1222
  {wxRegion_Xor_1_0, "wxRegion", "'Xor'", 2}, // 1223
  {wxRegion_Xor_1_1, "wxRegion", "'Xor'", 2}, // 1224
  {wxAcceleratorTable_new_0, "wxAcceleratorTable", "new", 0}, // 1225
  {wxAcceleratorTable_new_2, "wxAcceleratorTable", "new", 2}, // 1226
  {NULL, "", "", 0}, // 1227
  {NULL, "wxAcceleratorTable", "destroy", 1}, // 1228 obj destructor wxAcceleratorTable_destruct
  {wxAcceleratorTable_IsOk, "wxAcceleratorTable", "isOk", 1}, // 1229
  {wxAcceleratorEntry_new_1_0, "wxAcceleratorEntry", "new", 1}, // 1230
  {wxAcceleratorEntry_new_1_1, "wxAcceleratorEntry", "new", 1}, // 1231
  {wxAcceleratorEntry_GetCommand, "wxAcceleratorEntry", "getCommand", 1}, // 1232
  {wxAcceleratorEntry_GetFlags, "wxAcceleratorEntry", "getFlags", 1}, // 1233
  {wxAcceleratorEntry_GetKeyCode, "wxAcceleratorEntry", "getKeyCode", 1}, // 1234
  {wxAcceleratorEntry_Set, "wxAcceleratorEntry", "set", 5}, // 1235
  {wxAcceleratorEntry_destroy, "wxAcceleratorEntry", "'Destroy'", 1}, // 1236
  {NULL, "", "", 0}, // 1237
  {wxCaret_new_3, "wxCaret", "new", 3}, // 1238
  {wxCaret_new_2, "wxCaret", "new", 2}, // 1239
  {wxCaret_Create_3, "wxCaret", "create", 4}, // 1240
  {wxCaret_Create_2, "wxCaret", "create", 3}, // 1241
  {wxCaret_GetBlinkTime, "wxCaret", "getBlinkTime", 0}, // 1242
  {NULL, "", "", 0}, // 1243
  {wxCaret_GetPosition, "wxCaret", "getPosition", 1}, // 1244
  {NULL, "", "", 0}, // 1245
  {wxCaret_GetSize, "wxCaret", "getSize", 1}, // 1246
  {wxCaret_GetWindow, "wxCaret", "getWindow", 1}, // 1247
  {wxCaret_Hide, "wxCaret", "hide", 1}, // 1248
  {wxCaret_IsOk, "wxCaret", "isOk", 1}, // 1249
  {wxCaret_IsVisible, "wxCaret", "isVisible", 1}, // 1250
  {wxCaret_Move_2, "wxCaret", "move", 3}, // 1251
  {wxCaret_Move_1, "wxCaret", "move", 2}, // 1252
  {wxCaret_SetBlinkTime, "wxCaret", "setBlinkTime", 1}, // 1253
  {wxCaret_SetSize_2, "wxCaret", "setSize", 3}, // 1254
  {wxCaret_SetSize_1, "wxCaret", "setSize", 2}, // 1255
  {wxCaret_Show, "wxCaret", "show", 2}, // 1256
  {wxCaret_destroy, "wxCaret", "'Destroy'", 1}, // 1257
  {wxSizer_Add_2_0, "wxSizer", "add", 3}, // 1258
  {wxSizer_Add_2_1, "wxSizer", "add", 3}, // 1259
  {NULL, "", "", 0}, // 1260
  {NULL, "", "", 0}, // 1261
  {wxSizer_Add_3_0, "wxSizer", "add", 4}, // 1262
  {wxSizer_Add_3_1, "wxSizer", "add", 4}, // 1263
  {wxSizer_AddSpacer, "wxSizer", "addSpacer", 2}, // 1264
  {wxSizer_AddStretchSpacer, "wxSizer", "addStretchSpacer", 2}, // 1265
  {wxSizer_CalcMin, "wxSizer", "calcMin", 1}, // 1266
  {wxSizer_Clear, "wxSizer", "clear", 2}, // 1267
  {wxSizer_Detach_1_0, "wxSizer", "detach", 2}, // 1268
  {NULL, "", "", 0}, // 1269
  {wxSizer_Detach_1_1, "wxSizer", "detach", 2}, // 1270
  {wxSizer_Fit, "wxSizer", "fit", 2}, // 1271
  {wxSizer_FitInside, "wxSizer", "fitInside", 2}, // 1272
  {NULL, "", "", 0}, // 1273
  {wxSizer_GetChildren, "wxSizer", "getChildren", 1}, // 1274
  {wxSizer_GetItem_2, "wxSizer", "getItem", 3}, // 1275
  {NULL, "", "", 0}, // 1276
  {wxSizer_GetItem_1, "wxSizer", "getItem", 2}, // 1277
  {wxSizer_GetSize, "wxSizer", "getSize", 1}, // 1278
  {wxSizer_GetPosition, "wxSizer", "getPosition", 1}, // 1279
  {wxSizer_GetMinSize, "wxSizer", "getMinSize", 1}, // 1280
  {wxSizer_Hide_2, "wxSizer", "hide", 3}, // 1281
  {NULL, "", "", 0}, // 1282
  {wxSizer_Hide_1, "wxSizer", "hide", 2}, // 1283
  {wxSizer_Insert_3_0, "wxSizer", "insert", 4}, // 1284
  {wxSizer_Insert_3_1, "wxSizer", "insert", 4}, // 1285
  {NULL, "", "", 0}, // 1286
  {NULL, "", "", 0}, // 1287
  {wxSizer_Insert_4_0, "wxSizer", "insert", 5}, // 1288
  {wxSizer_Insert_4_1, "wxSizer", "insert", 5}, // 1289
  {wxSizer_Insert_2, "wxSizer", "insert", 3}, // 1290
  {wxSizer_InsertSpacer, "wxSizer", "insertSpacer", 3}, // 1291
  {wxSizer_InsertStretchSpacer, "wxSizer", "insertStretchSpacer", 3}, // 1292
  {wxSizer_IsShown_1_0, "wxSizer", "isShown", 2}, // 1293
  {NULL, "", "", 0}, // 1294
  {wxSizer_IsShown_1_1, "wxSizer", "isShown", 2}, // 1295
  {wxSizer_Layout, "wxSizer", "layout", 1}, // 1296
  {wxSizer_Prepend_2_0, "wxSizer", "prepend", 3}, // 1297
  {wxSizer_Prepend_2_1, "wxSizer", "prepend", 3}, // 1298
  {NULL, "", "", 0}, // 1299
  {NULL, "", "", 0}, // 1300
  {wxSizer_Prepend_3_0, "wxSizer", "prepend", 4}, // 1301
  {wxSizer_Prepend_3_1, "wxSizer", "prepend", 4}, // 1302
  {wxSizer_Prepend_1, "wxSizer", "prepend", 2}, // 1303
  {wxSizer_PrependSpacer, "wxSizer", "prependSpacer", 2}, // 1304
  {wxSizer_PrependStretchSpacer, "wxSizer", "prependStretchSpacer", 2}, // 1305
  {wxSizer_Remove_1_1, "wxSizer", "remove", 2}, // 1306
  {wxSizer_Remove_1_0, "wxSizer", "remove", 2}, // 1307
  {wxSizer_Replace_3, "wxSizer", "replace", 4}, // 1308
  {NULL, "", "", 0}, // 1309
  {wxSizer_Replace_2, "wxSizer", "replace", 3}, // 1310
  {wxSizer_SetDimension_4, "wxSizer", "setDimension", 5}, // 1311
  {wxSizer_SetDimension_2, "wxSizer", "setDimension", 3}, // 1312
  {wxSizer_SetMinSize_1, "wxSizer", "setMinSize", 2}, // 1313
  {wxSizer_SetMinSize_2, "wxSizer", "setMinSize", 3}, // 1314
  {wxSizer_SetItemMinSize_3_0, "wxSizer", "setItemMinSize", 4}, // 1315
  {wxSizer_SetItemMinSize_2_0, "wxSizer", "setItemMinSize", 3}, // 1316
  {NULL, "", "", 0}, // 1317
  {NULL, "", "", 0}, // 1318
  {wxSizer_SetItemMinSize_3_1, "wxSizer", "setItemMinSize", 4}, // 1319
  {wxSizer_SetItemMinSize_2_1, "wxSizer", "setItemMinSize", 3}, // 1320
  {wxSizer_SetSizeHints, "wxSizer", "setSizeHints", 2}, // 1321
  {wxSizer_Show_2_0, "wxSizer", "show", 3}, // 1322
  {NULL, "", "", 0}, // 1323
  {wxSizer_Show_2_1, "wxSizer", "show", 3}, // 1324
  {wxSizer_Show_1, "wxSizer", "show", 2}, // 1325
  {wxSizer_ShowItems, "wxSizer", "showItems", 2}, // 1326
  {wxSizerFlags_new, "wxSizerFlags", "new", 1}, // 1327
  {wxSizerFlags_Align, "wxSizerFlags", "align", 2}, // 1328
  {wxSizerFlags_Border_2, "wxSizerFlags", "border", 3}, // 1329
  {wxSizerFlags_Border_1, "wxSizerFlags", "border", 2}, // 1330
  {wxSizerFlags_Center, "wxSizerFlags", "center", 1}, // 1331
  {wxSizerFlags_Expand, "wxSizerFlags", "expand", 1}, // 1332
  {wxSizerFlags_Left, "wxSizerFlags", "left", 1}, // 1333
  {wxSizerFlags_Proportion, "wxSizerFlags", "proportion", 2}, // 1334
  {wxSizerFlags_Right, "wxSizerFlags", "right", 1}, // 1335
  {wxSizerFlags_destroy, "wxSizerFlags", "'Destroy'", 1}, // 1336
  {wxSizerItem_new_3, "wxSizerItem", "new", 3}, // 1337
  {wxSizerItem_new_2_0, "wxSizerItem", "new", 2}, // 1338
  {wxSizerItem_new_2_1, "wxSizerItem", "new", 2}, // 1339
  {NULL, "", "", 0}, // 1340
  {NULL, "", "", 0}, // 1341
  {NULL, "wxSizerItem", "destroy", 1}, // 1342 obj destructor wxSizerItem_destruct
  {wxSizerItem_CalcMin, "wxSizerItem", "calcMin", 1}, // 1343
  {wxSizerItem_DeleteWindows, "wxSizerItem", "deleteWindows", 1}, // 1344
  {wxSizerItem_DetachSizer, "wxSizerItem", "detachSizer", 1}, // 1345
  {wxSizerItem_GetBorder, "wxSizerItem", "getBorder", 1}, // 1346
  {wxSizerItem_GetFlag, "wxSizerItem", "getFlag", 1}, // 1347
  {wxSizerItem_GetMinSize, "wxSizerItem", "getMinSize", 1}, // 1348
  {wxSizerItem_GetPosition, "wxSizerItem", "getPosition", 1}, // 1349
  {wxSizerItem_GetProportion, "wxSizerItem", "getProportion", 1}, // 1350
  {wxSizerItem_GetRatio, "wxSizerItem", "getRatio", 1}, // 1351
  {wxSizerItem_GetRect, "wxSizerItem", "getRect", 1}, // 1352
  {wxSizerItem_GetSize, "wxSizerItem", "getSize", 1}, // 1353
  {wxSizerItem_GetSizer, "wxSizerItem", "getSizer", 1}, // 1354
  {wxSizerItem_GetSpacer, "wxSizerItem", "getSpacer", 1}, // 1355
  {wxSizerItem_GetUserData, "wxSizerItem", "getUserData", 1}, // 1356
  {wxSizerItem_GetWindow, "wxSizerItem", "getWindow", 1}, // 1357
  {wxSizerItem_IsSizer, "wxSizerItem", "isSizer", 1}, // 1358
  {wxSizerItem_IsShown, "wxSizerItem", "isShown", 1}, // 1359
  {wxSizerItem_IsSpacer, "wxSizerItem", "isSpacer", 1}, // 1360
  {wxSizerItem_IsWindow, "wxSizerItem", "isWindow", 1}, // 1361
  {wxSizerItem_SetBorder, "wxSizerItem", "setBorder", 2}, // 1362
  {wxSizerItem_SetDimension, "wxSizerItem", "setDimension", 3}, // 1363
  {wxSizerItem_SetFlag, "wxSizerItem", "setFlag", 2}, // 1364
  {wxSizerItem_SetInitSize, "wxSizerItem", "setInitSize", 3}, // 1365
  {wxSizerItem_SetMinSize_1, "wxSizerItem", "setMinSize", 2}, // 1366
  {wxSizerItem_SetMinSize_2, "wxSizerItem", "setMinSize", 3}, // 1367
  {wxSizerItem_SetProportion, "wxSizerItem", "setProportion", 2}, // 1368
  {wxSizerItem_SetRatio_2, "wxSizerItem", "setRatio", 3}, // 1369
  {wxSizerItem_SetRatio_1_1, "wxSizerItem", "setRatio", 2}, // 1370
  {wxSizerItem_SetRatio_1_0, "wxSizerItem", "setRatio", 2}, // 1371
  {wxSizerItem_AssignSizer, "wxSizerItem", "assignSizer", 2}, // 1372
  {wxSizerItem_AssignSpacer_1, "wxSizerItem", "assignSpacer", 2}, // 1373
  {wxSizerItem_AssignSpacer_2, "wxSizerItem", "assignSpacer", 3}, // 1374
  {wxSizerItem_AssignWindow, "wxSizerItem", "assignWindow", 2}, // 1375
  {wxSizerItem_Show, "wxSizerItem", "show", 2}, // 1376
  {wxBoxSizer_new, "wxBoxSizer", "new", 1}, // 1377
  {wxBoxSizer_GetOrientation, "wxBoxSizer", "getOrientation", 1}, // 1378
  {NULL, "wxBoxSizer", "'Destroy'", 1}, // 1379 obj destructor wxBoxSizer_destroy
  {wxStaticBoxSizer_new_2, "wxStaticBoxSizer", "new", 2}, // 1380
  {wxStaticBoxSizer_new_3, "wxStaticBoxSizer", "new", 3}, // 1381
  {wxStaticBoxSizer_GetStaticBox, "wxStaticBoxSizer", "getStaticBox", 1}, // 1382
  {NULL, "wxStaticBoxSizer", "'Destroy'", 1}, // 1383 obj destructor wxStaticBoxSizer_destroy
  {wxGridSizer_new_3_0, "wxGridSizer", "new", 3}, // 1384
  {wxGridSizer_new_2, "wxGridSizer", "new", 2}, // 1385
  {wxGridSizer_new_4, "wxGridSizer", "new", 4}, // 1386
  {wxGridSizer_new_3_1, "wxGridSizer", "new", 3}, // 1387
  {wxGridSizer_GetCols, "wxGridSizer", "getCols", 1}, // 1388
  {wxGridSizer_GetHGap, "wxGridSizer", "getHGap", 1}, // 1389
  {wxGridSizer_GetRows, "wxGridSizer", "getRows", 1}, // 1390
  {wxGridSizer_GetVGap, "wxGridSizer", "getVGap", 1}, // 1391
  {wxGridSizer_SetCols, "wxGridSizer", "setCols", 2}, // 1392
  {wxGridSizer_SetHGap, "wxGridSizer", "setHGap", 2}, // 1393
  {wxGridSizer_SetRows, "wxGridSizer", "setRows", 2}, // 1394
  {wxGridSizer_SetVGap, "wxGridSizer", "setVGap", 2}, // 1395
  {NULL, "wxGridSizer", "'Destroy'", 1}, // 1396 obj destructor wxGridSizer_destroy
  {wxFlexGridSizer_new_3_0, "wxFlexGridSizer", "new", 3}, // 1397
  {wxFlexGridSizer_new_2, "wxFlexGridSizer", "new", 2}, // 1398
  {wxFlexGridSizer_new_4, "wxFlexGridSizer", "new", 4}, // 1399
  {wxFlexGridSizer_new_3_1, "wxFlexGridSizer", "new", 3}, // 1400
  {wxFlexGridSizer_AddGrowableCol, "wxFlexGridSizer", "addGrowableCol", 3}, // 1401
  {wxFlexGridSizer_AddGrowableRow, "wxFlexGridSizer", "addGrowableRow", 3}, // 1402
  {wxFlexGridSizer_GetFlexibleDirection, "wxFlexGridSizer", "getFlexibleDirection", 1}, // 1403
  {wxFlexGridSizer_GetNonFlexibleGrowMode, "wxFlexGridSizer", "getNonFlexibleGrowMode", 1}, // 1404
  {wxFlexGridSizer_RemoveGrowableCol, "wxFlexGridSizer", "removeGrowableCol", 2}, // 1405
  {wxFlexGridSizer_RemoveGrowableRow, "wxFlexGridSizer", "removeGrowableRow", 2}, // 1406
  {wxFlexGridSizer_SetFlexibleDirection, "wxFlexGridSizer", "setFlexibleDirection", 2}, // 1407
  {wxFlexGridSizer_SetNonFlexibleGrowMode, "wxFlexGridSizer", "setNonFlexibleGrowMode", 2}, // 1408
  {NULL, "wxFlexGridSizer", "'Destroy'", 1}, // 1409 obj destructor wxFlexGridSizer_destroy
  {wxGridBagSizer_new, "wxGridBagSizer", "new", 1}, // 1410
  {wxGridBagSizer_Add_3, "wxGridBagSizer", "add", 4}, // 1411
  {NULL, "", "", 0}, // 1412
  {wxGridBagSizer_Add_1, "wxGridBagSizer", "add", 2}, // 1413
  {wxGridBagSizer_Add_4, "wxGridBagSizer", "add", 5}, // 1414
  {wxGridBagSizer_CalcMin, "wxGridBagSizer", "calcMin", 1}, // 1415
  {wxGridBagSizer_CheckForIntersection_2, "wxGridBagSizer", "checkForIntersection", 3}, // 1416
  {wxGridBagSizer_CheckForIntersection_3, "wxGridBagSizer", "checkForIntersection", 4}, // 1417
  {wxGridBagSizer_FindItem, "wxGridBagSizer", "findItem", 2}, // 1418
  {NULL, "", "", 0}, // 1419
  {wxGridBagSizer_FindItemAtPoint, "wxGridBagSizer", "findItemAtPoint", 2}, // 1420
  {wxGridBagSizer_FindItemAtPosition, "wxGridBagSizer", "findItemAtPosition", 2}, // 1421
  {wxGridBagSizer_FindItemWithData, "wxGridBagSizer", "findItemWithData", 2}, // 1422
  {wxGridBagSizer_GetCellSize, "wxGridBagSizer", "getCellSize", 3}, // 1423
  {wxGridBagSizer_GetEmptyCellSize, "wxGridBagSizer", "getEmptyCellSize", 1}, // 1424
  {wxGridBagSizer_GetItemPosition_1_0, "wxGridBagSizer", "getItemPosition", 2}, // 1425
  {NULL, "", "", 0}, // 1426
  {wxGridBagSizer_GetItemPosition_1_1, "wxGridBagSizer", "getItemPosition", 2}, // 1427
  {wxGridBagSizer_GetItemSpan_1_0, "wxGridBagSizer", "getItemSpan", 2}, // 1428
  {NULL, "", "", 0}, // 1429
  {wxGridBagSizer_GetItemSpan_1_1, "wxGridBagSizer", "getItemSpan", 2}, // 1430
  {wxGridBagSizer_SetEmptyCellSize, "wxGridBagSizer", "setEmptyCellSize", 2}, // 1431
  {wxGridBagSizer_SetItemPosition_2_0, "wxGridBagSizer", "setItemPosition", 3}, // 1432
  {NULL, "", "", 0}, // 1433
  {wxGridBagSizer_SetItemPosition_2_1, "wxGridBagSizer", "setItemPosition", 3}, // 1434
  {wxGridBagSizer_SetItemSpan_2_0, "wxGridBagSizer", "setItemSpan", 3}, // 1435
  {NULL, "", "", 0}, // 1436
  {wxGridBagSizer_SetItemSpan_2_1, "wxGridBagSizer", "setItemSpan", 3}, // 1437
  {NULL, "wxGridBagSizer", "'Destroy'", 1}, // 1438 obj destructor wxGridBagSizer_destroy
  {wxStdDialogButtonSizer_new, "wxStdDialogButtonSizer", "new", 0}, // 1439
  {wxStdDialogButtonSizer_AddButton, "wxStdDialogButtonSizer", "addButton", 2}, // 1440
  {wxStdDialogButtonSizer_Realize, "wxStdDialogButtonSizer", "realize", 1}, // 1441
  {wxStdDialogButtonSizer_SetAffirmativeButton, "wxStdDialogButtonSizer", "setAffirmativeButton", 2}, // 1442
  {wxStdDialogButtonSizer_SetCancelButton, "wxStdDialogButtonSizer", "setCancelButton", 2}, // 1443
  {wxStdDialogButtonSizer_SetNegativeButton, "wxStdDialogButtonSizer", "setNegativeButton", 2}, // 1444
  {NULL, "wxStdDialogButtonSizer", "'Destroy'", 1}, // 1445 obj destructor wxStdDialogButtonSizer_destroy
  {wxFont_new_0, "wxFont", "new", 0}, // 1446
  {wxFont_new_1_1, "wxFont", "new", 1}, // 1447
  {wxFont_new_5_0, "wxFont", "new", 5}, // 1448
  {wxFont_new_5_1, "wxFont", "new", 5}, // 1449
  {wxFont_new_1_0, "wxFont", "new", 1}, // 1450
  {NULL, "wxFont", "destroy", 1}, // 1451 obj destructor wxFont_destruct
  {wxFont_IsFixedWidth, "wxFont", "isFixedWidth", 1}, // 1452
  {wxFont_GetDefaultEncoding, "wxFont", "getDefaultEncoding", 0}, // 1453
  {wxFont_GetFaceName, "wxFont", "getFaceName", 1}, // 1454
  {wxFont_GetFamily, "wxFont", "getFamily", 1}, // 1455
  {wxFont_GetNativeFontInfoDesc, "wxFont", "getNativeFontInfoDesc", 1}, // 1456
  {wxFont_GetNativeFontInfoUserDesc, "wxFont", "getNativeFontInfoUserDesc", 1}, // 1457
  {wxFont_GetPointSize, "wxFont", "getPointSize", 1}, // 1458
  {wxFont_GetStyle, "wxFont", "getStyle", 1}, // 1459
  {wxFont_GetUnderlined, "wxFont", "getUnderlined", 1}, // 1460
  {wxFont_GetWeight, "wxFont", "getWeight", 1}, // 1461
  {wxFont_IsOk, "wxFont", "isOk", 1}, // 1462
  {wxFont_SetDefaultEncoding, "wxFont", "setDefaultEncoding", 1}, // 1463
  {wxFont_SetFaceName, "wxFont", "setFaceName", 2}, // 1464
  {wxFont_SetFamily, "wxFont", "setFamily", 2}, // 1465
  {wxFont_SetPointSize, "wxFont", "setPointSize", 2}, // 1466
  {wxFont_SetStyle, "wxFont", "setStyle", 2}, // 1467
  {wxFont_SetUnderlined, "wxFont", "setUnderlined", 2}, // 1468
  {wxFont_SetWeight, "wxFont", "setWeight", 2}, // 1469
  {wxToolTip_Enable, "wxToolTip", "enable", 1}, // 1470
  {wxToolTip_SetDelay, "wxToolTip", "setDelay", 1}, // 1471
  {wxToolTip_new, "wxToolTip", "new", 1}, // 1472
  {wxToolTip_SetTip, "wxToolTip", "setTip", 2}, // 1473
  {wxToolTip_GetTip, "wxToolTip", "getTip", 1}, // 1474
  {wxToolTip_GetWindow, "wxToolTip", "getWindow", 1}, // 1475
  {NULL, "wxToolTip", "'Destroy'", 1}, // 1476 obj destructor wxToolTip_destroy
  {wxButton_new_0, "wxButton", "new", 0}, // 1477
  {wxButton_new_3, "wxButton", "new", 3}, // 1478
  {wxButton_Create, "wxButton", "create", 4}, // 1479
  {wxButton_GetDefaultSize_STAT_0, "wxButton", "getDefaultSize", 0}, // 1480
#if wxCHECK_VERSION(3,1,3)
  {wxButton_GetDefaultSize_STAT_1, "wxButton", "getDefaultSize", 1}, // 1481
#else
  {NULL, "wxButton", "getDefaultSize", 0}, // 1481
#endif // wxCHECK_VERSION(3,1,3)
  {wxButton_SetDefault, "wxButton", "setDefault", 1}, // 1482
  {wxButton_SetLabel, "wxButton", "setLabel", 2}, // 1483
  {wxButton_GetBitmapDisabled, "wxButton", "getBitmapDisabled", 1}, // 1484
  {wxButton_GetBitmapFocus, "wxButton", "getBitmapFocus", 1}, // 1485
  {wxButton_GetBitmapLabel, "wxButton", "getBitmapLabel", 1}, // 1486
  {wxButton_SetBitmapDisabled, "wxButton", "setBitmapDisabled", 2}, // 1487
  {wxButton_SetBitmapFocus, "wxButton", "setBitmapFocus", 2}, // 1488
  {wxButton_SetBitmapLabel, "wxButton", "setBitmapLabel", 2}, // 1489
  {NULL, "wxButton", "'Destroy'", 1}, // 1490 obj destructor wxButton_destroy
  {wxBitmapButton_new_0, "wxBitmapButton", "new", 0}, // 1491
  {wxBitmapButton_new_4, "wxBitmapButton", "new", 4}, // 1492
  {wxBitmapButton_Create, "wxBitmapButton", "create", 5}, // 1493
  {wxBitmapButton_NewCloseButton, "wxBitmapButton", "newCloseButton", 2}, // 1494
  {NULL, "wxBitmapButton", "'Destroy'", 1}, // 1495 obj destructor wxBitmapButton_destroy
  {wxToggleButton_new_0, "wxToggleButton", "new", 0}, // 1496
  {wxToggleButton_new_4, "wxToggleButton", "new", 4}, // 1497
  {NULL, "wxToggleButton", "destroy", 1}, // 1498 obj destructor wxToggleButton_destruct
  {wxToggleButton_Create, "wxToggleButton", "create", 5}, // 1499
  {wxToggleButton_GetValue, "wxToggleButton", "getValue", 1}, // 1500
  {wxToggleButton_SetValue, "wxToggleButton", "setValue", 2}, // 1501
  {wxCalendarCtrl_new_0, "wxCalendarCtrl", "new", 0}, // 1502
  {wxCalendarCtrl_new_3, "wxCalendarCtrl", "new", 3}, // 1503
  {wxCalendarCtrl_Create, "wxCalendarCtrl", "create", 4}, // 1504
  {NULL, "wxCalendarCtrl", "destroy", 1}, // 1505 obj destructor wxCalendarCtrl_destruct
  {wxCalendarCtrl_SetDate, "wxCalendarCtrl", "setDate", 2}, // 1506
  {wxCalendarCtrl_GetDate, "wxCalendarCtrl", "getDate", 1}, // 1507
#if !wxCHECK_VERSION(2,9,0)
  {wxCalendarCtrl_EnableYearChange, "wxCalendarCtrl", "enableYearChange", 2}, // 1508
#else
  {NULL, "wxCalendarCtrl", "enableYearChange", 0}, // 1508
#endif // !wxCHECK_VERSION(2,9,0)
  {wxCalendarCtrl_EnableMonthChange, "wxCalendarCtrl", "enableMonthChange", 2}, // 1509
  {wxCalendarCtrl_EnableHolidayDisplay, "wxCalendarCtrl", "enableHolidayDisplay", 2}, // 1510
  {wxCalendarCtrl_SetHeaderColours, "wxCalendarCtrl", "setHeaderColours", 3}, // 1511
  {wxCalendarCtrl_GetHeaderColourFg, "wxCalendarCtrl", "getHeaderColourFg", 1}, // 1512
  {wxCalendarCtrl_GetHeaderColourBg, "wxCalendarCtrl", "getHeaderColourBg", 1}, // 1513
  {wxCalendarCtrl_SetHighlightColours, "wxCalendarCtrl", "setHighlightColours", 3}, // 1514
  {wxCalendarCtrl_GetHighlightColourFg, "wxCalendarCtrl", "getHighlightColourFg", 1}, // 1515
  {wxCalendarCtrl_GetHighlightColourBg, "wxCalendarCtrl", "getHighlightColourBg", 1}, // 1516
  {wxCalendarCtrl_SetHolidayColours, "wxCalendarCtrl", "setHolidayColours", 3}, // 1517
  {wxCalendarCtrl_GetHolidayColourFg, "wxCalendarCtrl", "getHolidayColourFg", 1}, // 1518
  {wxCalendarCtrl_GetHolidayColourBg, "wxCalendarCtrl", "getHolidayColourBg", 1}, // 1519
  {wxCalendarCtrl_GetAttr, "wxCalendarCtrl", "getAttr", 2}, // 1520
  {wxCalendarCtrl_SetAttr, "wxCalendarCtrl", "setAttr", 3}, // 1521
  {wxCalendarCtrl_SetHoliday, "wxCalendarCtrl", "setHoliday", 2}, // 1522
  {wxCalendarCtrl_ResetAttr, "wxCalendarCtrl", "resetAttr", 2}, // 1523
  {wxCalendarCtrl_HitTest, "wxCalendarCtrl", "hitTest", 2}, // 1524
  {wxCalendarDateAttr_new_1, "wxCalendarDateAttr", "new", 1}, // 1525
  {wxCalendarDateAttr_new_2, "wxCalendarDateAttr", "new", 2}, // 1526
  {wxCalendarDateAttr_SetTextColour, "wxCalendarDateAttr", "setTextColour", 2}, // 1527
  {wxCalendarDateAttr_SetBackgroundColour, "wxCalendarDateAttr", "setBackgroundColour", 2}, // 1528
  {wxCalendarDateAttr_SetBorderColour, "wxCalendarDateAttr", "setBorderColour", 2}, // 1529
  {wxCalendarDateAttr_SetFont, "wxCalendarDateAttr", "setFont", 2}, // 1530
  {wxCalendarDateAttr_SetBorder, "wxCalendarDateAttr", "setBorder", 2}, // 1531
  {wxCalendarDateAttr_SetHoliday, "wxCalendarDateAttr", "setHoliday", 2}, // 1532
  {wxCalendarDateAttr_HasTextColour, "wxCalendarDateAttr", "hasTextColour", 1}, // 1533
  {wxCalendarDateAttr_HasBackgroundColour, "wxCalendarDateAttr", "hasBackgroundColour", 1}, // 1534
  {wxCalendarDateAttr_HasBorderColour, "wxCalendarDateAttr", "hasBorderColour", 1}, // 1535
  {wxCalendarDateAttr_HasFont, "wxCalendarDateAttr", "hasFont", 1}, // 1536
  {wxCalendarDateAttr_HasBorder, "wxCalendarDateAttr", "hasBorder", 1}, // 1537
  {wxCalendarDateAttr_IsHoliday, "wxCalendarDateAttr", "isHoliday", 1}, // 1538
  {wxCalendarDateAttr_GetTextColour, "wxCalendarDateAttr", "getTextColour", 1}, // 1539
  {wxCalendarDateAttr_GetBackgroundColour, "wxCalendarDateAttr", "getBackgroundColour", 1}, // 1540
  {wxCalendarDateAttr_GetBorderColour, "wxCalendarDateAttr", "getBorderColour", 1}, // 1541
  {wxCalendarDateAttr_GetFont, "wxCalendarDateAttr", "getFont", 1}, // 1542
  {wxCalendarDateAttr_GetBorder, "wxCalendarDateAttr", "getBorder", 1}, // 1543
  {wxCalendarDateAttr_destroy, "wxCalendarDateAttr", "'Destroy'", 1}, // 1544
  {wxCheckBox_new_0, "wxCheckBox", "new", 0}, // 1545
  {wxCheckBox_new_4, "wxCheckBox", "new", 4}, // 1546
  {NULL, "wxCheckBox", "destroy", 1}, // 1547 obj destructor wxCheckBox_destruct
  {wxCheckBox_Create, "wxCheckBox", "create", 5}, // 1548
  {wxCheckBox_GetValue, "wxCheckBox", "getValue", 1}, // 1549
  {wxCheckBox_Get3StateValue, "wxCheckBox", "get3StateValue", 1}, // 1550
  {wxCheckBox_Is3rdStateAllowedForUser, "wxCheckBox", "is3rdStateAllowedForUser", 1}, // 1551
  {wxCheckBox_Is3State, "wxCheckBox", "is3State", 1}, // 1552
  {wxCheckBox_IsChecked, "wxCheckBox", "isChecked", 1}, // 1553
  {wxCheckBox_SetValue, "wxCheckBox", "setValue", 2}, // 1554
  {wxCheckBox_Set3StateValue, "wxCheckBox", "set3StateValue", 2}, // 1555
  {wxCheckListBox_new_0, "wxCheckListBox", "new", 0}, // 1556
  {NULL, "", "", 0}, // 1557
  {wxCheckListBox_new_3, "wxCheckListBox", "new", 3}, // 1558
  {NULL, "wxCheckListBox", "destroy", 1}, // 1559 obj destructor wxCheckListBox_destruct
  {wxCheckListBox_Check, "wxCheckListBox", "check", 3}, // 1560
  {wxCheckListBox_IsChecked, "wxCheckListBox", "isChecked", 2}, // 1561
  {wxChoice_new_0, "wxChoice", "new", 0}, // 1562
  {NULL, "", "", 0}, // 1563
  {wxChoice_new_3, "wxChoice", "new", 3}, // 1564
  {NULL, "wxChoice", "destroy", 1}, // 1565 obj destructor wxChoice_destruct
  {NULL, "", "", 0}, // 1566
  {wxChoice_Create, "wxChoice", "create", 7}, // 1567
  {wxChoice_Delete, "wxChoice", "delete", 2}, // 1568
  {wxChoice_GetColumns, "wxChoice", "getColumns", 1}, // 1569
  {wxChoice_SetColumns, "wxChoice", "setColumns", 2}, // 1570
  {wxComboBox_new_0, "wxComboBox", "new", 0}, // 1571
  {NULL, "", "", 0}, // 1572
  {wxComboBox_new_3, "wxComboBox", "new", 3}, // 1573
  {NULL, "wxComboBox", "destroy", 1}, // 1574 obj destructor wxComboBox_destruct
  {NULL, "", "", 0}, // 1575
  {wxComboBox_Create, "wxComboBox", "create", 8}, // 1576
  {wxComboBox_CanCopy, "wxComboBox", "canCopy", 1}, // 1577
  {wxComboBox_CanCut, "wxComboBox", "canCut", 1}, // 1578
  {wxComboBox_CanPaste, "wxComboBox", "canPaste", 1}, // 1579
  {wxComboBox_CanRedo, "wxComboBox", "canRedo", 1}, // 1580
  {wxComboBox_CanUndo, "wxComboBox", "canUndo", 1}, // 1581
  {wxComboBox_Copy, "wxComboBox", "copy", 1}, // 1582
  {wxComboBox_Cut, "wxComboBox", "cut", 1}, // 1583
  {wxComboBox_GetInsertionPoint, "wxComboBox", "getInsertionPoint", 1}, // 1584
  {wxComboBox_GetLastPosition, "wxComboBox", "getLastPosition", 1}, // 1585
  {wxComboBox_GetValue, "wxComboBox", "getValue", 1}, // 1586
  {wxComboBox_Paste, "wxComboBox", "paste", 1}, // 1587
  {wxComboBox_Redo, "wxComboBox", "redo", 1}, // 1588
  {wxComboBox_Replace, "wxComboBox", "replace", 4}, // 1589
  {wxComboBox_Remove, "wxComboBox", "remove", 3}, // 1590
  {wxComboBox_SetInsertionPoint, "wxComboBox", "setInsertionPoint", 2}, // 1591
  {wxComboBox_SetInsertionPointEnd, "wxComboBox", "setInsertionPointEnd", 1}, // 1592
  {wxComboBox_SetSelection_2, "wxComboBox", "setSelection", 3}, // 1593
  {wxComboBox_SetSelection_1, "wxComboBox", "setSelection", 2}, // 1594
  {wxComboBox_SetValue, "wxComboBox", "setValue", 2}, // 1595
  {wxComboBox_Undo, "wxComboBox", "undo", 1}, // 1596
  {wxGauge_new_0, "wxGauge", "new", 0}, // 1597
  {wxGauge_new_4, "wxGauge", "new", 4}, // 1598
  {NULL, "wxGauge", "destroy", 1}, // 1599 obj destructor wxGauge_destruct
  {wxGauge_Create, "wxGauge", "create", 5}, // 1600
  {wxGauge_GetRange, "wxGauge", "getRange", 1}, // 1601
  {wxGauge_GetValue, "wxGauge", "getValue", 1}, // 1602
  {wxGauge_IsVertical, "wxGauge", "isVertical", 1}, // 1603
  {wxGauge_SetRange, "wxGauge", "setRange", 2}, // 1604
  {wxGauge_SetValue, "wxGauge", "setValue", 2}, // 1605
  {wxGauge_Pulse, "wxGauge", "pulse", 1}, // 1606
  {wxGenericDirCtrl_new_0, "wxGenericDirCtrl", "new", 0}, // 1607
  {wxGenericDirCtrl_new_2, "wxGenericDirCtrl", "new", 2}, // 1608
  {NULL, "wxGenericDirCtrl", "destroy", 1}, // 1609 obj destructor wxGenericDirCtrl_destruct
  {wxGenericDirCtrl_Create, "wxGenericDirCtrl", "create", 3}, // 1610
  {wxGenericDirCtrl_Init, "wxGenericDirCtrl", "init", 1}, // 1611
  {wxGenericDirCtrl_CollapseTree, "wxGenericDirCtrl", "collapseTree", 1}, // 1612
  {wxGenericDirCtrl_ExpandPath, "wxGenericDirCtrl", "expandPath", 2}, // 1613
  {wxGenericDirCtrl_GetDefaultPath, "wxGenericDirCtrl", "getDefaultPath", 1}, // 1614
  {wxGenericDirCtrl_GetPath_0, "wxGenericDirCtrl", "getPath", 1}, // 1615
  {wxGenericDirCtrl_GetPath_1, "wxGenericDirCtrl", "getPath", 2}, // 1616
  {wxGenericDirCtrl_GetFilePath, "wxGenericDirCtrl", "getFilePath", 1}, // 1617
  {wxGenericDirCtrl_GetFilter, "wxGenericDirCtrl", "getFilter", 1}, // 1618
  {wxGenericDirCtrl_GetFilterIndex, "wxGenericDirCtrl", "getFilterIndex", 1}, // 1619
  {wxGenericDirCtrl_GetRootId, "wxGenericDirCtrl", "getRootId", 1}, // 1620
  {wxGenericDirCtrl_GetTreeCtrl, "wxGenericDirCtrl", "getTreeCtrl", 1}, // 1621
  {wxGenericDirCtrl_ReCreateTree, "wxGenericDirCtrl", "reCreateTree", 1}, // 1622
  {wxGenericDirCtrl_SetDefaultPath, "wxGenericDirCtrl", "setDefaultPath", 2}, // 1623
  {wxGenericDirCtrl_SetFilter, "wxGenericDirCtrl", "setFilter", 2}, // 1624
  {wxGenericDirCtrl_SetFilterIndex, "wxGenericDirCtrl", "setFilterIndex", 2}, // 1625
  {wxGenericDirCtrl_SetPath, "wxGenericDirCtrl", "setPath", 2}, // 1626
  {wxStaticBox_new_0, "wxStaticBox", "new", 0}, // 1627
  {wxStaticBox_new_4, "wxStaticBox", "new", 4}, // 1628
  {NULL, "wxStaticBox", "destroy", 1}, // 1629 obj destructor wxStaticBox_destruct
  {wxStaticBox_Create, "wxStaticBox", "create", 5}, // 1630
  {wxStaticLine_new_0, "wxStaticLine", "new", 0}, // 1631
  {wxStaticLine_new_2, "wxStaticLine", "new", 2}, // 1632
  {wxStaticLine_Create, "wxStaticLine", "create", 3}, // 1633
  {wxStaticLine_IsVertical, "wxStaticLine", "isVertical", 1}, // 1634
  {wxStaticLine_GetDefaultSize, "wxStaticLine", "getDefaultSize", 0}, // 1635
  {NULL, "wxStaticLine", "'Destroy'", 1}, // 1636 obj destructor wxStaticLine_destroy
  {wxListBox_new_0, "wxListBox", "new", 0}, // 1637
  {NULL, "", "", 0}, // 1638
  {wxListBox_new_3, "wxListBox", "new", 3}, // 1639
  {NULL, "wxListBox", "destroy", 1}, // 1640 obj destructor wxListBox_destruct
  {NULL, "", "", 0}, // 1641
  {wxListBox_Create, "wxListBox", "create", 7}, // 1642
  {wxListBox_Deselect, "wxListBox", "deselect", 2}, // 1643
  {wxListBox_GetSelections, "wxListBox", "getSelections", 1}, // 1644
  {wxListBox_InsertItems, "wxListBox", "insertItems", 3}, // 1645
  {wxListBox_IsSelected, "wxListBox", "isSelected", 2}, // 1646
  {NULL, "", "", 0}, // 1647
  {NULL, "", "", 0}, // 1648
  {wxListBox_Set, "wxListBox", "set", 2}, // 1649
  {wxListBox_HitTest_1, "wxListBox", "hitTest", 2}, // 1650
  {wxListBox_HitTest_2, "wxListBox", "hitTest", 3}, // 1651
  {wxListBox_SetFirstItem_1_0, "wxListBox", "setFirstItem", 2}, // 1652
  {wxListBox_SetFirstItem_1_1, "wxListBox", "setFirstItem", 2}, // 1653
  {wxListCtrl_new_0, "wxListCtrl", "new", 0}, // 1654
  {NULL, "wxListCtrl", "new", 2}, // 1655 TaylorMade erl only wxListCtrl_new_2
  {NULL, "wxListCtrl", "destroy", 1}, // 1656 obj destructor wxListCtrl_destruct
  {wxListCtrl_Arrange, "wxListCtrl", "arrange", 2}, // 1657
  {wxListCtrl_AssignImageList, "wxListCtrl", "assignImageList", 3}, // 1658
  {wxListCtrl_ClearAll, "wxListCtrl", "clearAll", 1}, // 1659
  {wxListCtrl_Create, "wxListCtrl", "create", 3}, // 1660
  {wxListCtrl_DeleteAllItems, "wxListCtrl", "deleteAllItems", 1}, // 1661
  {wxListCtrl_DeleteColumn, "wxListCtrl", "deleteColumn", 2}, // 1662
  {wxListCtrl_DeleteItem, "wxListCtrl", "deleteItem", 2}, // 1663
  {wxListCtrl_EditLabel, "wxListCtrl", "editLabel", 2}, // 1664
  {wxListCtrl_EnsureVisible, "wxListCtrl", "ensureVisible", 2}, // 1665
  {wxListCtrl_FindItem_3_0, "wxListCtrl", "findItem", 4}, // 1666
  {wxListCtrl_FindItem_3_1, "wxListCtrl", "findItem", 4}, // 1667
  {wxListCtrl_GetColumn, "wxListCtrl", "getColumn", 3}, // 1668
  {wxListCtrl_GetColumnCount, "wxListCtrl", "getColumnCount", 1}, // 1669
  {wxListCtrl_GetColumnWidth, "wxListCtrl", "getColumnWidth", 2}, // 1670
  {wxListCtrl_GetCountPerPage, "wxListCtrl", "getCountPerPage", 1}, // 1671
  {wxListCtrl_GetEditControl, "wxListCtrl", "getEditControl", 1}, // 1672
  {wxListCtrl_GetImageList, "wxListCtrl", "getImageList", 2}, // 1673
  {wxListCtrl_GetItem, "wxListCtrl", "getItem", 2}, // 1674
  {wxListCtrl_GetItemBackgroundColour, "wxListCtrl", "getItemBackgroundColour", 2}, // 1675
  {wxListCtrl_GetItemCount, "wxListCtrl", "getItemCount", 1}, // 1676
  {wxListCtrl_GetItemData, "wxListCtrl", "getItemData", 2}, // 1677
  {wxListCtrl_GetItemFont, "wxListCtrl", "getItemFont", 2}, // 1678
  {wxListCtrl_GetItemPosition, "wxListCtrl", "getItemPosition", 2}, // 1679
  {wxListCtrl_GetItemRect, "wxListCtrl", "getItemRect", 3}, // 1680
  {wxListCtrl_GetItemSpacing, "wxListCtrl", "getItemSpacing", 1}, // 1681
  {wxListCtrl_GetItemState, "wxListCtrl", "getItemState", 3}, // 1682
  {wxListCtrl_GetItemText, "wxListCtrl", "getItemText", 3}, // 1683
  {wxListCtrl_GetItemTextColour, "wxListCtrl", "getItemTextColour", 2}, // 1684
  {wxListCtrl_GetNextItem, "wxListCtrl", "getNextItem", 3}, // 1685
  {wxListCtrl_GetSelectedItemCount, "wxListCtrl", "getSelectedItemCount", 1}, // 1686
  {wxListCtrl_GetTextColour, "wxListCtrl", "getTextColour", 1}, // 1687
  {wxListCtrl_GetTopItem, "wxListCtrl", "getTopItem", 1}, // 1688
  {wxListCtrl_GetViewRect, "wxListCtrl", "getViewRect", 1}, // 1689
  {wxListCtrl_HitTest, "wxListCtrl", "hitTest", 2}, // 1690
  {wxListCtrl_InsertColumn_2, "wxListCtrl", "insertColumn", 3}, // 1691
  {wxListCtrl_InsertColumn_3, "wxListCtrl", "insertColumn", 4}, // 1692
  {wxListCtrl_InsertItem_1, "wxListCtrl", "insertItem", 2}, // 1693
  {wxListCtrl_InsertItem_2_1, "wxListCtrl", "insertItem", 3}, // 1694
  {wxListCtrl_InsertItem_2_0, "wxListCtrl", "insertItem", 3}, // 1695
  {wxListCtrl_InsertItem_3, "wxListCtrl", "insertItem", 4}, // 1696
  {wxListCtrl_RefreshItem, "wxListCtrl", "refreshItem", 2}, // 1697
  {wxListCtrl_RefreshItems, "wxListCtrl", "refreshItems", 3}, // 1698
  {wxListCtrl_ScrollList, "wxListCtrl", "scrollList", 3}, // 1699
  {wxListCtrl_SetBackgroundColour, "wxListCtrl", "setBackgroundColour", 2}, // 1700
  {wxListCtrl_SetColumn, "wxListCtrl", "setColumn", 3}, // 1701
  {wxListCtrl_SetColumnWidth, "wxListCtrl", "setColumnWidth", 3}, // 1702
  {wxListCtrl_SetImageList, "wxListCtrl", "setImageList", 3}, // 1703
  {wxListCtrl_SetItem_1, "wxListCtrl", "setItem", 2}, // 1704
  {wxListCtrl_SetItem_4, "wxListCtrl", "setItem", 5}, // 1705
  {wxListCtrl_SetItemBackgroundColour, "wxListCtrl", "setItemBackgroundColour", 3}, // 1706
  {wxListCtrl_SetItemCount, "wxListCtrl", "setItemCount", 2}, // 1707
  {wxListCtrl_SetItemData, "wxListCtrl", "setItemData", 3}, // 1708
  {wxListCtrl_SetItemFont, "wxListCtrl", "setItemFont", 3}, // 1709
  {wxListCtrl_SetItemImage, "wxListCtrl", "setItemImage", 4}, // 1710
  {wxListCtrl_SetItemColumnImage, "wxListCtrl", "setItemColumnImage", 4}, // 1711
  {wxListCtrl_SetItemPosition, "wxListCtrl", "setItemPosition", 3}, // 1712
  {wxListCtrl_SetItemState, "wxListCtrl", "setItemState", 4}, // 1713
  {wxListCtrl_SetItemText, "wxListCtrl", "setItemText", 3}, // 1714
  {wxListCtrl_SetItemTextColour, "wxListCtrl", "setItemTextColour", 3}, // 1715
  {wxListCtrl_SetSingleStyle, "wxListCtrl", "setSingleStyle", 3}, // 1716
  {wxListCtrl_SetTextColour, "wxListCtrl", "setTextColour", 2}, // 1717
  {wxListCtrl_SetWindowStyleFlag, "wxListCtrl", "setWindowStyleFlag", 2}, // 1718
  {wxListCtrl_SortItems, "wxListCtrl", "sortItems", 2}, // 1719
  {wxListView_ClearColumnImage, "wxListView", "clearColumnImage", 2}, // 1720
  {wxListView_Focus, "wxListView", "focus", 2}, // 1721
  {wxListView_GetFirstSelected, "wxListView", "getFirstSelected", 1}, // 1722
  {wxListView_GetFocusedItem, "wxListView", "getFocusedItem", 1}, // 1723
  {wxListView_GetNextSelected, "wxListView", "getNextSelected", 2}, // 1724
  {wxListView_IsSelected, "wxListView", "isSelected", 2}, // 1725
  {wxListView_Select, "wxListView", "select", 3}, // 1726
  {wxListView_SetColumnImage, "wxListView", "setColumnImage", 3}, // 1727
  {wxListItem_new_0, "wxListItem", "new", 0}, // 1728
  {wxListItem_new_1, "wxListItem", "new", 1}, // 1729
  {wxListItem_Clear, "wxListItem", "clear", 1}, // 1730
  {wxListItem_GetAlign, "wxListItem", "getAlign", 1}, // 1731
  {wxListItem_GetBackgroundColour, "wxListItem", "getBackgroundColour", 1}, // 1732
  {wxListItem_GetColumn, "wxListItem", "getColumn", 1}, // 1733
  {wxListItem_GetFont, "wxListItem", "getFont", 1}, // 1734
  {wxListItem_GetId, "wxListItem", "getId", 1}, // 1735
  {wxListItem_GetImage, "wxListItem", "getImage", 1}, // 1736
  {wxListItem_GetMask, "wxListItem", "getMask", 1}, // 1737
  {wxListItem_GetState, "wxListItem", "getState", 1}, // 1738
  {wxListItem_GetText, "wxListItem", "getText", 1}, // 1739
  {wxListItem_GetTextColour, "wxListItem", "getTextColour", 1}, // 1740
  {wxListItem_GetWidth, "wxListItem", "getWidth", 1}, // 1741
  {wxListItem_SetAlign, "wxListItem", "setAlign", 2}, // 1742
  {wxListItem_SetBackgroundColour, "wxListItem", "setBackgroundColour", 2}, // 1743
  {wxListItem_SetColumn, "wxListItem", "setColumn", 2}, // 1744
  {wxListItem_SetFont, "wxListItem", "setFont", 2}, // 1745
  {wxListItem_SetId, "wxListItem", "setId", 2}, // 1746
  {wxListItem_SetImage, "wxListItem", "setImage", 2}, // 1747
  {wxListItem_SetMask, "wxListItem", "setMask", 2}, // 1748
  {wxListItem_SetState, "wxListItem", "setState", 2}, // 1749
  {wxListItem_SetStateMask, "wxListItem", "setStateMask", 2}, // 1750
  {wxListItem_SetText, "wxListItem", "setText", 2}, // 1751
  {wxListItem_SetTextColour, "wxListItem", "setTextColour", 2}, // 1752
  {wxListItem_SetWidth, "wxListItem", "setWidth", 2}, // 1753
  {NULL, "wxListItem", "'Destroy'", 1}, // 1754 obj destructor wxListItem_destroy
  {wxListItemAttr_new_0, "wxListItemAttr", "new", 0}, // 1755
  {wxListItemAttr_new_3, "wxListItemAttr", "new", 3}, // 1756
  {wxListItemAttr_GetBackgroundColour, "wxListItemAttr", "getBackgroundColour", 1}, // 1757
  {wxListItemAttr_GetFont, "wxListItemAttr", "getFont", 1}, // 1758
  {wxListItemAttr_GetTextColour, "wxListItemAttr", "getTextColour", 1}, // 1759
  {wxListItemAttr_HasBackgroundColour, "wxListItemAttr", "hasBackgroundColour", 1}, // 1760
  {wxListItemAttr_HasFont, "wxListItemAttr", "hasFont", 1}, // 1761
  {wxListItemAttr_HasTextColour, "wxListItemAttr", "hasTextColour", 1}, // 1762
  {wxListItemAttr_SetBackgroundColour, "wxListItemAttr", "setBackgroundColour", 2}, // 1763
  {wxListItemAttr_SetFont, "wxListItemAttr", "setFont", 2}, // 1764
  {wxListItemAttr_SetTextColour, "wxListItemAttr", "setTextColour", 2}, // 1765
  {wxListItemAttr_destroy, "wxListItemAttr", "'Destroy'", 1}, // 1766
  {wxImageList_new_0, "wxImageList", "new", 0}, // 1767
  {wxImageList_new_3, "wxImageList", "new", 3}, // 1768
  {wxImageList_Add_2_0, "wxImageList", "add", 3}, // 1769
  {NULL, "", "", 0}, // 1770
  {wxImageList_Add_2_1, "wxImageList", "add", 3}, // 1771
  {wxImageList_Add_1, "wxImageList", "add", 2}, // 1772
  {wxImageList_Create, "wxImageList", "create", 4}, // 1773
  {wxImageList_Draw, "wxImageList", "draw", 6}, // 1774
  {wxImageList_GetBitmap, "wxImageList", "getBitmap", 2}, // 1775
  {wxImageList_GetIcon, "wxImageList", "getIcon", 2}, // 1776
  {wxImageList_GetImageCount, "wxImageList", "getImageCount", 1}, // 1777
  {wxImageList_GetSize, "wxImageList", "getSize", 2}, // 1778
  {NULL, "", "", 0}, // 1779
  {wxImageList_Remove, "wxImageList", "remove", 2}, // 1780
  {wxImageList_RemoveAll, "wxImageList", "removeAll", 1}, // 1781
  {wxImageList_Replace_3, "wxImageList", "replace", 4}, // 1782
  {NULL, "", "", 0}, // 1783
  {wxImageList_Replace_2, "wxImageList", "replace", 3}, // 1784
  {NULL, "wxImageList", "'Destroy'", 1}, // 1785 obj destructor wxImageList_destroy
  {wxTextAttr_new_0, "wxTextAttr", "new", 0}, // 1786
  {wxTextAttr_new_2, "wxTextAttr", "new", 2}, // 1787
  {wxTextAttr_new_1, "wxTextAttr", "new", 1}, // 1788
  {wxTextAttr_GetAlignment, "wxTextAttr", "getAlignment", 1}, // 1789
  {wxTextAttr_GetBackgroundColour, "wxTextAttr", "getBackgroundColour", 1}, // 1790
  {wxTextAttr_GetFont, "wxTextAttr", "getFont", 1}, // 1791
  {wxTextAttr_GetFontEncoding, "wxTextAttr", "getFontEncoding", 1}, // 1792
  {wxTextAttr_GetFontFaceName, "wxTextAttr", "getFontFaceName", 1}, // 1793
  {wxTextAttr_GetFontSize, "wxTextAttr", "getFontSize", 1}, // 1794
  {wxTextAttr_GetFontStyle, "wxTextAttr", "getFontStyle", 1}, // 1795
  {wxTextAttr_GetFontUnderlined, "wxTextAttr", "getFontUnderlined", 1}, // 1796
  {wxTextAttr_GetFontWeight, "wxTextAttr", "getFontWeight", 1}, // 1797
  {wxTextAttr_GetLeftIndent, "wxTextAttr", "getLeftIndent", 1}, // 1798
  {wxTextAttr_GetLeftSubIndent, "wxTextAttr", "getLeftSubIndent", 1}, // 1799
  {wxTextAttr_GetRightIndent, "wxTextAttr", "getRightIndent", 1}, // 1800
  {wxTextAttr_GetTabs, "wxTextAttr", "getTabs", 1}, // 1801
  {wxTextAttr_GetTextColour, "wxTextAttr", "getTextColour", 1}, // 1802
  {wxTextAttr_HasBackgroundColour, "wxTextAttr", "hasBackgroundColour", 1}, // 1803
  {wxTextAttr_HasFont, "wxTextAttr", "hasFont", 1}, // 1804
  {wxTextAttr_HasTextColour, "wxTextAttr", "hasTextColour", 1}, // 1805
  {wxTextAttr_GetFlags, "wxTextAttr", "getFlags", 1}, // 1806
  {wxTextAttr_IsDefault, "wxTextAttr", "isDefault", 1}, // 1807
  {wxTextAttr_SetAlignment, "wxTextAttr", "setAlignment", 2}, // 1808
  {wxTextAttr_SetBackgroundColour, "wxTextAttr", "setBackgroundColour", 2}, // 1809
  {wxTextAttr_SetFlags, "wxTextAttr", "setFlags", 2}, // 1810
  {wxTextAttr_SetFont, "wxTextAttr", "setFont", 3}, // 1811
  {wxTextAttr_SetFontEncoding, "wxTextAttr", "setFontEncoding", 2}, // 1812
  {wxTextAttr_SetFontFaceName, "wxTextAttr", "setFontFaceName", 2}, // 1813
  {wxTextAttr_SetFontFamily, "wxTextAttr", "setFontFamily", 2}, // 1814
  {wxTextAttr_SetFontSize, "wxTextAttr", "setFontSize", 2}, // 1815
  {wxTextAttr_SetFontPointSize, "wxTextAttr", "setFontPointSize", 2}, // 1816
  {wxTextAttr_SetFontPixelSize, "wxTextAttr", "setFontPixelSize", 2}, // 1817
  {wxTextAttr_SetFontStyle, "wxTextAttr", "setFontStyle", 2}, // 1818
  {wxTextAttr_SetFontUnderlined, "wxTextAttr", "setFontUnderlined", 2}, // 1819
  {wxTextAttr_SetFontWeight, "wxTextAttr", "setFontWeight", 2}, // 1820
  {wxTextAttr_SetLeftIndent, "wxTextAttr", "setLeftIndent", 3}, // 1821
  {wxTextAttr_SetRightIndent, "wxTextAttr", "setRightIndent", 2}, // 1822
  {wxTextAttr_SetTabs, "wxTextAttr", "setTabs", 2}, // 1823
  {wxTextAttr_SetTextColour, "wxTextAttr", "setTextColour", 2}, // 1824
  {wxTextAttr_destroy, "wxTextAttr", "'Destroy'", 1}, // 1825
  {wxTextCtrl_new_0, "wxTextCtrl", "new", 0}, // 1826
  {wxTextCtrl_new_3, "wxTextCtrl", "new", 3}, // 1827
  {NULL, "wxTextCtrl", "destroy", 1}, // 1828 obj destructor wxTextCtrl_destruct
  {wxTextCtrl_AppendText, "wxTextCtrl", "appendText", 2}, // 1829
  {wxTextCtrl_CanCopy, "wxTextCtrl", "canCopy", 1}, // 1830
  {wxTextCtrl_CanCut, "wxTextCtrl", "canCut", 1}, // 1831
  {wxTextCtrl_CanPaste, "wxTextCtrl", "canPaste", 1}, // 1832
  {wxTextCtrl_CanRedo, "wxTextCtrl", "canRedo", 1}, // 1833
  {wxTextCtrl_CanUndo, "wxTextCtrl", "canUndo", 1}, // 1834
  {wxTextCtrl_Clear, "wxTextCtrl", "clear", 1}, // 1835
  {wxTextCtrl_Copy, "wxTextCtrl", "copy", 1}, // 1836
  {wxTextCtrl_Create, "wxTextCtrl", "create", 4}, // 1837
  {wxTextCtrl_Cut, "wxTextCtrl", "cut", 1}, // 1838
  {wxTextCtrl_DiscardEdits, "wxTextCtrl", "discardEdits", 1}, // 1839
  {wxTextCtrl_ChangeValue, "wxTextCtrl", "changeValue", 2}, // 1840
  {wxTextCtrl_EmulateKeyPress, "wxTextCtrl", "emulateKeyPress", 2}, // 1841
  {wxTextCtrl_GetDefaultStyle, "wxTextCtrl", "getDefaultStyle", 1}, // 1842
  {wxTextCtrl_GetInsertionPoint, "wxTextCtrl", "getInsertionPoint", 1}, // 1843
  {wxTextCtrl_GetLastPosition, "wxTextCtrl", "getLastPosition", 1}, // 1844
  {wxTextCtrl_GetLineLength, "wxTextCtrl", "getLineLength", 2}, // 1845
  {wxTextCtrl_GetLineText, "wxTextCtrl", "getLineText", 2}, // 1846
  {wxTextCtrl_GetNumberOfLines, "wxTextCtrl", "getNumberOfLines", 1}, // 1847
  {wxTextCtrl_GetRange, "wxTextCtrl", "getRange", 3}, // 1848
  {wxTextCtrl_GetSelection, "wxTextCtrl", "getSelection", 1}, // 1849
  {wxTextCtrl_GetStringSelection, "wxTextCtrl", "getStringSelection", 1}, // 1850
  {wxTextCtrl_GetStyle, "wxTextCtrl", "getStyle", 3}, // 1851
  {wxTextCtrl_GetValue, "wxTextCtrl", "getValue", 1}, // 1852
  {wxTextCtrl_IsEditable, "wxTextCtrl", "isEditable", 1}, // 1853
  {wxTextCtrl_IsModified, "wxTextCtrl", "isModified", 1}, // 1854
  {wxTextCtrl_IsMultiLine, "wxTextCtrl", "isMultiLine", 1}, // 1855
  {wxTextCtrl_IsSingleLine, "wxTextCtrl", "isSingleLine", 1}, // 1856
  {wxTextCtrl_LoadFile, "wxTextCtrl", "loadFile", 3}, // 1857
  {wxTextCtrl_MarkDirty, "wxTextCtrl", "markDirty", 1}, // 1858
  {wxTextCtrl_Paste, "wxTextCtrl", "paste", 1}, // 1859
  {wxTextCtrl_PositionToXY, "wxTextCtrl", "positionToXY", 2}, // 1860
  {wxTextCtrl_Redo, "wxTextCtrl", "redo", 1}, // 1861
  {wxTextCtrl_Remove, "wxTextCtrl", "remove", 3}, // 1862
  {wxTextCtrl_Replace, "wxTextCtrl", "replace", 4}, // 1863
  {wxTextCtrl_SaveFile, "wxTextCtrl", "saveFile", 2}, // 1864
  {wxTextCtrl_SetDefaultStyle, "wxTextCtrl", "setDefaultStyle", 2}, // 1865
  {wxTextCtrl_SetEditable, "wxTextCtrl", "setEditable", 2}, // 1866
  {wxTextCtrl_SetInsertionPoint, "wxTextCtrl", "setInsertionPoint", 2}, // 1867
  {wxTextCtrl_SetInsertionPointEnd, "wxTextCtrl", "setInsertionPointEnd", 1}, // 1868
  {wxTextCtrl_SetMaxLength, "wxTextCtrl", "setMaxLength", 2}, // 1869
  {wxTextCtrl_SetSelection, "wxTextCtrl", "setSelection", 3}, // 1870
  {wxTextCtrl_SetStyle, "wxTextCtrl", "setStyle", 4}, // 1871
  {wxTextCtrl_SetValue, "wxTextCtrl", "setValue", 2}, // 1872
  {wxTextCtrl_ShowPosition, "wxTextCtrl", "showPosition", 2}, // 1873
  {wxTextCtrl_Undo, "wxTextCtrl", "undo", 1}, // 1874
  {wxTextCtrl_WriteText, "wxTextCtrl", "writeText", 2}, // 1875
  {wxTextCtrl_XYToPosition, "wxTextCtrl", "xYToPosition", 3}, // 1876
  {wxBookCtrlBase_AddPage, "wxBookCtrlBase", "addPage", 4}, // 1877
  {wxBookCtrlBase_InsertPage, "wxBookCtrlBase", "insertPage", 5}, // 1878
  {wxBookCtrlBase_DeletePage, "wxBookCtrlBase", "deletePage", 2}, // 1879
  {wxBookCtrlBase_RemovePage, "wxBookCtrlBase", "removePage", 2}, // 1880
  {wxBookCtrlBase_DeleteAllPages, "wxBookCtrlBase", "deleteAllPages", 1}, // 1881
  {wxBookCtrlBase_GetPage, "wxBookCtrlBase", "getPage", 2}, // 1882
  {wxBookCtrlBase_GetPageCount, "wxBookCtrlBase", "getPageCount", 1}, // 1883
  {wxBookCtrlBase_GetCurrentPage, "wxBookCtrlBase", "getCurrentPage", 1}, // 1884
  {wxBookCtrlBase_AdvanceSelection, "wxBookCtrlBase", "advanceSelection", 2}, // 1885
  {wxBookCtrlBase_SetSelection, "wxBookCtrlBase", "setSelection", 2}, // 1886
  {wxBookCtrlBase_GetSelection, "wxBookCtrlBase", "getSelection", 1}, // 1887
  {wxBookCtrlBase_ChangeSelection, "wxBookCtrlBase", "changeSelection", 2}, // 1888
  {wxBookCtrlBase_HitTest, "wxBookCtrlBase", "hitTest", 2}, // 1889
  {wxBookCtrlBase_GetPageText, "wxBookCtrlBase", "getPageText", 2}, // 1890
  {wxBookCtrlBase_SetPageText, "wxBookCtrlBase", "setPageText", 3}, // 1891
  {wxNotebook_new_0, "wxNotebook", "new", 0}, // 1892
  {wxNotebook_new_3, "wxNotebook", "new", 3}, // 1893
  {NULL, "wxNotebook", "destroy", 1}, // 1894 obj destructor wxNotebook_destruct
  {wxNotebook_AssignImageList, "wxNotebook", "assignImageList", 2}, // 1895
  {wxNotebook_Create, "wxNotebook", "create", 4}, // 1896
  {wxNotebook_GetImageList, "wxNotebook", "getImageList", 1}, // 1897
  {wxNotebook_GetPageImage, "wxNotebook", "getPageImage", 2}, // 1898
  {wxNotebook_GetRowCount, "wxNotebook", "getRowCount", 1}, // 1899
  {wxNotebook_GetThemeBackgroundColour, "wxNotebook", "getThemeBackgroundColour", 1}, // 1900
  {wxNotebook_SetImageList, "wxNotebook", "setImageList", 2}, // 1901
  {wxNotebook_SetPadding, "wxNotebook", "setPadding", 2}, // 1902
  {wxNotebook_SetPageSize, "wxNotebook", "setPageSize", 2}, // 1903
  {wxNotebook_SetPageImage, "wxNotebook", "setPageImage", 3}, // 1904
  {wxChoicebook_new_0, "wxChoicebook", "new", 0}, // 1905
  {wxChoicebook_new_3, "wxChoicebook", "new", 3}, // 1906
  {wxChoicebook_AddPage, "wxChoicebook", "addPage", 4}, // 1907
  {wxChoicebook_AdvanceSelection, "wxChoicebook", "advanceSelection", 2}, // 1908
  {wxChoicebook_AssignImageList, "wxChoicebook", "assignImageList", 2}, // 1909
  {wxChoicebook_Create, "wxChoicebook", "create", 4}, // 1910
  {wxChoicebook_DeleteAllPages, "wxChoicebook", "deleteAllPages", 1}, // 1911
  {wxChoicebook_GetCurrentPage, "wxChoicebook", "getCurrentPage", 1}, // 1912
  {wxChoicebook_GetImageList, "wxChoicebook", "getImageList", 1}, // 1913
  {wxChoicebook_GetPage, "wxChoicebook", "getPage", 2}, // 1914
  {wxChoicebook_GetPageCount, "wxChoicebook", "getPageCount", 1}, // 1915
  {wxChoicebook_GetPageImage, "wxChoicebook", "getPageImage", 2}, // 1916
  {wxChoicebook_GetPageText, "wxChoicebook", "getPageText", 2}, // 1917
  {wxChoicebook_GetSelection, "wxChoicebook", "getSelection", 1}, // 1918
  {wxChoicebook_HitTest, "wxChoicebook", "hitTest", 2}, // 1919
  {wxChoicebook_InsertPage, "wxChoicebook", "insertPage", 5}, // 1920
  {wxChoicebook_SetImageList, "wxChoicebook", "setImageList", 2}, // 1921
  {wxChoicebook_SetPageSize, "wxChoicebook", "setPageSize", 2}, // 1922
  {wxChoicebook_SetPageImage, "wxChoicebook", "setPageImage", 3}, // 1923
  {wxChoicebook_SetPageText, "wxChoicebook", "setPageText", 3}, // 1924
  {wxChoicebook_SetSelection, "wxChoicebook", "setSelection", 2}, // 1925
  {wxChoicebook_ChangeSelection, "wxChoicebook", "changeSelection", 2}, // 1926
  {NULL, "wxChoicebook", "'Destroy'", 1}, // 1927 obj destructor wxChoicebook_destroy
  {wxToolbook_new_0, "wxToolbook", "new", 0}, // 1928
  {wxToolbook_new_3, "wxToolbook", "new", 3}, // 1929
  {wxToolbook_AddPage, "wxToolbook", "addPage", 4}, // 1930
  {wxToolbook_AdvanceSelection, "wxToolbook", "advanceSelection", 2}, // 1931
  {wxToolbook_AssignImageList, "wxToolbook", "assignImageList", 2}, // 1932
  {wxToolbook_Create, "wxToolbook", "create", 4}, // 1933
  {wxToolbook_DeleteAllPages, "wxToolbook", "deleteAllPages", 1}, // 1934
  {wxToolbook_GetCurrentPage, "wxToolbook", "getCurrentPage", 1}, // 1935
  {wxToolbook_GetImageList, "wxToolbook", "getImageList", 1}, // 1936
  {wxToolbook_GetPage, "wxToolbook", "getPage", 2}, // 1937
  {wxToolbook_GetPageCount, "wxToolbook", "getPageCount", 1}, // 1938
  {wxToolbook_GetPageImage, "wxToolbook", "getPageImage", 2}, // 1939
  {wxToolbook_GetPageText, "wxToolbook", "getPageText", 2}, // 1940
  {wxToolbook_GetSelection, "wxToolbook", "getSelection", 1}, // 1941
  {wxToolbook_HitTest, "wxToolbook", "hitTest", 2}, // 1942
  {wxToolbook_InsertPage, "wxToolbook", "insertPage", 5}, // 1943
  {wxToolbook_SetImageList, "wxToolbook", "setImageList", 2}, // 1944
  {wxToolbook_SetPageSize, "wxToolbook", "setPageSize", 2}, // 1945
  {wxToolbook_SetPageImage, "wxToolbook", "setPageImage", 3}, // 1946
  {wxToolbook_SetPageText, "wxToolbook", "setPageText", 3}, // 1947
  {wxToolbook_SetSelection, "wxToolbook", "setSelection", 2}, // 1948
  {wxToolbook_ChangeSelection, "wxToolbook", "changeSelection", 2}, // 1949
  {NULL, "wxToolbook", "'Destroy'", 1}, // 1950 obj destructor wxToolbook_destroy
  {wxListbook_new_0, "wxListbook", "new", 0}, // 1951
  {wxListbook_new_3, "wxListbook", "new", 3}, // 1952
  {wxListbook_AddPage, "wxListbook", "addPage", 4}, // 1953
  {wxListbook_AdvanceSelection, "wxListbook", "advanceSelection", 2}, // 1954
  {wxListbook_AssignImageList, "wxListbook", "assignImageList", 2}, // 1955
  {wxListbook_Create, "wxListbook", "create", 4}, // 1956
  {wxListbook_DeleteAllPages, "wxListbook", "deleteAllPages", 1}, // 1957
  {wxListbook_GetCurrentPage, "wxListbook", "getCurrentPage", 1}, // 1958
  {wxListbook_GetImageList, "wxListbook", "getImageList", 1}, // 1959
  {wxListbook_GetPage, "wxListbook", "getPage", 2}, // 1960
  {wxListbook_GetPageCount, "wxListbook", "getPageCount", 1}, // 1961
  {wxListbook_GetPageImage, "wxListbook", "getPageImage", 2}, // 1962
  {wxListbook_GetPageText, "wxListbook", "getPageText", 2}, // 1963
  {wxListbook_GetSelection, "wxListbook", "getSelection", 1}, // 1964
  {wxListbook_HitTest, "wxListbook", "hitTest", 2}, // 1965
  {wxListbook_InsertPage, "wxListbook", "insertPage", 5}, // 1966
  {wxListbook_SetImageList, "wxListbook", "setImageList", 2}, // 1967
  {wxListbook_SetPageSize, "wxListbook", "setPageSize", 2}, // 1968
  {wxListbook_SetPageImage, "wxListbook", "setPageImage", 3}, // 1969
  {wxListbook_SetPageText, "wxListbook", "setPageText", 3}, // 1970
  {wxListbook_SetSelection, "wxListbook", "setSelection", 2}, // 1971
  {wxListbook_ChangeSelection, "wxListbook", "changeSelection", 2}, // 1972
  {NULL, "wxListbook", "'Destroy'", 1}, // 1973 obj destructor wxListbook_destroy
  {wxTreebook_new_0, "wxTreebook", "new", 0}, // 1974
  {wxTreebook_new_3, "wxTreebook", "new", 3}, // 1975
  {NULL, "wxTreebook", "destroy", 1}, // 1976 obj destructor wxTreebook_destruct
  {wxTreebook_AddPage, "wxTreebook", "addPage", 4}, // 1977
  {wxTreebook_AdvanceSelection, "wxTreebook", "advanceSelection", 2}, // 1978
  {wxTreebook_AssignImageList, "wxTreebook", "assignImageList", 2}, // 1979
  {wxTreebook_Create, "wxTreebook", "create", 4}, // 1980
  {wxTreebook_DeleteAllPages, "wxTreebook", "deleteAllPages", 1}, // 1981
  {wxTreebook_GetCurrentPage, "wxTreebook", "getCurrentPage", 1}, // 1982
  {wxTreebook_GetImageList, "wxTreebook", "getImageList", 1}, // 1983
  {wxTreebook_GetPage, "wxTreebook", "getPage", 2}, // 1984
  {wxTreebook_GetPageCount, "wxTreebook", "getPageCount", 1}, // 1985
  {wxTreebook_GetPageImage, "wxTreebook", "getPageImage", 2}, // 1986
  {wxTreebook_GetPageText, "wxTreebook", "getPageText", 2}, // 1987
  {wxTreebook_GetSelection, "wxTreebook", "getSelection", 1}, // 1988
  {wxTreebook_ExpandNode, "wxTreebook", "expandNode", 3}, // 1989
  {wxTreebook_IsNodeExpanded, "wxTreebook", "isNodeExpanded", 2}, // 1990
  {wxTreebook_HitTest, "wxTreebook", "hitTest", 2}, // 1991
  {wxTreebook_InsertPage, "wxTreebook", "insertPage", 5}, // 1992
  {wxTreebook_InsertSubPage, "wxTreebook", "insertSubPage", 5}, // 1993
  {wxTreebook_SetImageList, "wxTreebook", "setImageList", 2}, // 1994
  {wxTreebook_SetPageSize, "wxTreebook", "setPageSize", 2}, // 1995
  {wxTreebook_SetPageImage, "wxTreebook", "setPageImage", 3}, // 1996
  {wxTreebook_SetPageText, "wxTreebook", "setPageText", 3}, // 1997
  {wxTreebook_SetSelection, "wxTreebook", "setSelection", 2}, // 1998
  {wxTreebook_ChangeSelection, "wxTreebook", "changeSelection", 2}, // 1999
  {wxTreeCtrl_new_0, "wxTreeCtrl", "new", 0}, // 2000
  {wxTreeCtrl_new_2, "wxTreeCtrl", "new", 2}, // 2001
  {NULL, "wxTreeCtrl", "destroy", 1}, // 2002 obj destructor wxTreeCtrl_destruct
  {wxTreeCtrl_AddRoot, "wxTreeCtrl", "addRoot", 3}, // 2003
  {wxTreeCtrl_AppendItem, "wxTreeCtrl", "appendItem", 4}, // 2004
  {wxTreeCtrl_AssignImageList, "wxTreeCtrl", "assignImageList", 2}, // 2005
  {wxTreeCtrl_AssignStateImageList, "wxTreeCtrl", "assignStateImageList", 2}, // 2006
  {wxTreeCtrl_Collapse, "wxTreeCtrl", "collapse", 2}, // 2007
  {wxTreeCtrl_CollapseAndReset, "wxTreeCtrl", "collapseAndReset", 2}, // 2008
  {wxTreeCtrl_Create, "wxTreeCtrl", "create", 3}, // 2009
  {wxTreeCtrl_Delete, "wxTreeCtrl", "delete", 2}, // 2010
  {wxTreeCtrl_DeleteAllItems, "wxTreeCtrl", "deleteAllItems", 1}, // 2011
  {wxTreeCtrl_DeleteChildren, "wxTreeCtrl", "deleteChildren", 2}, // 2012
  {wxTreeCtrl_EditLabel, "wxTreeCtrl", "editLabel", 2}, // 2013
  {wxTreeCtrl_EnsureVisible, "wxTreeCtrl", "ensureVisible", 2}, // 2014
  {wxTreeCtrl_Expand, "wxTreeCtrl", "expand", 2}, // 2015
  {wxTreeCtrl_GetBoundingRect, "wxTreeCtrl", "getBoundingRect", 3}, // 2016
  {wxTreeCtrl_GetChildrenCount, "wxTreeCtrl", "getChildrenCount", 3}, // 2017
  {wxTreeCtrl_GetCount, "wxTreeCtrl", "getCount", 1}, // 2018
  {wxTreeCtrl_GetEditControl, "wxTreeCtrl", "getEditControl", 1}, // 2019
  {wxTreeCtrl_GetFirstChild, "wxTreeCtrl", "getFirstChild", 2}, // 2020
  {wxTreeCtrl_GetNextChild, "wxTreeCtrl", "getNextChild", 3}, // 2021
  {wxTreeCtrl_GetFirstVisibleItem, "wxTreeCtrl", "getFirstVisibleItem", 1}, // 2022
  {wxTreeCtrl_GetImageList, "wxTreeCtrl", "getImageList", 1}, // 2023
  {wxTreeCtrl_GetIndent, "wxTreeCtrl", "getIndent", 1}, // 2024
  {wxTreeCtrl_GetItemBackgroundColour, "wxTreeCtrl", "getItemBackgroundColour", 2}, // 2025
  {wxTreeCtrl_GetItemData, "wxTreeCtrl", "getItemData", 2}, // 2026
  {wxTreeCtrl_GetItemFont, "wxTreeCtrl", "getItemFont", 2}, // 2027
  {wxTreeCtrl_GetItemImage, "wxTreeCtrl", "getItemImage", 3}, // 2028
  {wxTreeCtrl_GetItemText, "wxTreeCtrl", "getItemText", 2}, // 2029
  {wxTreeCtrl_GetItemTextColour, "wxTreeCtrl", "getItemTextColour", 2}, // 2030
  {wxTreeCtrl_GetLastChild, "wxTreeCtrl", "getLastChild", 2}, // 2031
  {wxTreeCtrl_GetNextSibling, "wxTreeCtrl", "getNextSibling", 2}, // 2032
  {wxTreeCtrl_GetNextVisible, "wxTreeCtrl", "getNextVisible", 2}, // 2033
  {wxTreeCtrl_GetItemParent, "wxTreeCtrl", "getItemParent", 2}, // 2034
  {wxTreeCtrl_GetPrevSibling, "wxTreeCtrl", "getPrevSibling", 2}, // 2035
  {wxTreeCtrl_GetPrevVisible, "wxTreeCtrl", "getPrevVisible", 2}, // 2036
  {wxTreeCtrl_GetRootItem, "wxTreeCtrl", "getRootItem", 1}, // 2037
  {wxTreeCtrl_GetSelection, "wxTreeCtrl", "getSelection", 1}, // 2038
  {wxTreeCtrl_GetSelections, "wxTreeCtrl", "getSelections", 1}, // 2039
  {wxTreeCtrl_GetStateImageList, "wxTreeCtrl", "getStateImageList", 1}, // 2040
  {wxTreeCtrl_HitTest, "wxTreeCtrl", "hitTest", 2}, // 2041
  {wxTreeCtrl_InsertItem, "wxTreeCtrl", "insertItem", 5}, // 2042
  {NULL, "", "", 0}, // 2043
  {wxTreeCtrl_IsBold, "wxTreeCtrl", "isBold", 2}, // 2044
  {wxTreeCtrl_IsExpanded, "wxTreeCtrl", "isExpanded", 2}, // 2045
  {wxTreeCtrl_IsSelected, "wxTreeCtrl", "isSelected", 2}, // 2046
  {wxTreeCtrl_IsVisible, "wxTreeCtrl", "isVisible", 2}, // 2047
  {wxTreeCtrl_ItemHasChildren, "wxTreeCtrl", "itemHasChildren", 2}, // 2048
  {wxTreeCtrl_IsTreeItemIdOk, "wxTreeCtrl", "isTreeItemIdOk", 1}, // 2049
  {wxTreeCtrl_PrependItem, "wxTreeCtrl", "prependItem", 4}, // 2050
  {wxTreeCtrl_ScrollTo, "wxTreeCtrl", "scrollTo", 2}, // 2051
  {wxTreeCtrl_SelectItem, "wxTreeCtrl", "selectItem", 3}, // 2052
  {wxTreeCtrl_SetIndent, "wxTreeCtrl", "setIndent", 2}, // 2053
  {wxTreeCtrl_SetImageList, "wxTreeCtrl", "setImageList", 2}, // 2054
  {wxTreeCtrl_SetItemBackgroundColour, "wxTreeCtrl", "setItemBackgroundColour", 3}, // 2055
  {wxTreeCtrl_SetItemBold, "wxTreeCtrl", "setItemBold", 3}, // 2056
  {wxTreeCtrl_SetItemData, "wxTreeCtrl", "setItemData", 3}, // 2057
  {wxTreeCtrl_SetItemDropHighlight, "wxTreeCtrl", "setItemDropHighlight", 3}, // 2058
  {wxTreeCtrl_SetItemFont, "wxTreeCtrl", "setItemFont", 3}, // 2059
  {wxTreeCtrl_SetItemHasChildren, "wxTreeCtrl", "setItemHasChildren", 3}, // 2060
  {wxTreeCtrl_SetItemImage, "wxTreeCtrl", "setItemImage", 4}, // 2061
  {wxTreeCtrl_SetItemText, "wxTreeCtrl", "setItemText", 3}, // 2062
  {wxTreeCtrl_SetItemTextColour, "wxTreeCtrl", "setItemTextColour", 3}, // 2063
  {wxTreeCtrl_SetStateImageList, "wxTreeCtrl", "setStateImageList", 2}, // 2064
  {wxTreeCtrl_SetWindowStyle, "wxTreeCtrl", "setWindowStyle", 2}, // 2065
  {wxTreeCtrl_SortChildren, "wxTreeCtrl", "sortChildren", 2}, // 2066
  {wxTreeCtrl_Toggle, "wxTreeCtrl", "toggle", 2}, // 2067
  {wxTreeCtrl_ToggleItemSelection, "wxTreeCtrl", "toggleItemSelection", 2}, // 2068
  {wxTreeCtrl_Unselect, "wxTreeCtrl", "unselect", 1}, // 2069
  {wxTreeCtrl_UnselectAll, "wxTreeCtrl", "unselectAll", 1}, // 2070
  {wxTreeCtrl_UnselectItem, "wxTreeCtrl", "unselectItem", 2}, // 2071
  {wxScrollBar_new_0, "wxScrollBar", "new", 0}, // 2072
  {wxScrollBar_new_3, "wxScrollBar", "new", 3}, // 2073
  {NULL, "wxScrollBar", "destroy", 1}, // 2074 obj destructor wxScrollBar_destruct
  {wxScrollBar_Create, "wxScrollBar", "create", 4}, // 2075
  {wxScrollBar_GetRange, "wxScrollBar", "getRange", 1}, // 2076
  {wxScrollBar_GetPageSize, "wxScrollBar", "getPageSize", 1}, // 2077
  {wxScrollBar_GetThumbPosition, "wxScrollBar", "getThumbPosition", 1}, // 2078
  {wxScrollBar_GetThumbSize, "wxScrollBar", "getThumbSize", 1}, // 2079
  {wxScrollBar_SetThumbPosition, "wxScrollBar", "setThumbPosition", 2}, // 2080
  {wxScrollBar_SetScrollbar, "wxScrollBar", "setScrollbar", 6}, // 2081
  {wxSpinButton_new_0, "wxSpinButton", "new", 0}, // 2082
  {wxSpinButton_new_2, "wxSpinButton", "new", 2}, // 2083
  {NULL, "wxSpinButton", "destroy", 1}, // 2084 obj destructor wxSpinButton_destruct
  {wxSpinButton_Create, "wxSpinButton", "create", 3}, // 2085
  {wxSpinButton_GetMax, "wxSpinButton", "getMax", 1}, // 2086
  {wxSpinButton_GetMin, "wxSpinButton", "getMin", 1}, // 2087
  {wxSpinButton_GetValue, "wxSpinButton", "getValue", 1}, // 2088
  {wxSpinButton_SetRange, "wxSpinButton", "setRange", 3}, // 2089
  {wxSpinButton_SetValue, "wxSpinButton", "setValue", 2}, // 2090
  {wxSpinCtrl_new_0, "wxSpinCtrl", "new", 0}, // 2091
  {wxSpinCtrl_new_2, "wxSpinCtrl", "new", 2}, // 2092
  {wxSpinCtrl_Create, "wxSpinCtrl", "create", 3}, // 2093
  {wxSpinCtrl_SetValue_1_1, "wxSpinCtrl", "setValue", 2}, // 2094
  {wxSpinCtrl_SetValue_1_0, "wxSpinCtrl", "setValue", 2}, // 2095
  {wxSpinCtrl_GetValue, "wxSpinCtrl", "getValue", 1}, // 2096
  {wxSpinCtrl_SetRange, "wxSpinCtrl", "setRange", 3}, // 2097
  {wxSpinCtrl_SetSelection, "wxSpinCtrl", "setSelection", 3}, // 2098
  {wxSpinCtrl_GetMin, "wxSpinCtrl", "getMin", 1}, // 2099
  {wxSpinCtrl_GetMax, "wxSpinCtrl", "getMax", 1}, // 2100
  {NULL, "wxSpinCtrl", "'Destroy'", 1}, // 2101 obj destructor wxSpinCtrl_destroy
  {wxStaticText_new_0, "wxStaticText", "new", 0}, // 2102
  {wxStaticText_new_4, "wxStaticText", "new", 4}, // 2103
  {wxStaticText_Create, "wxStaticText", "create", 5}, // 2104
  {wxStaticText_GetLabel, "wxStaticText", "getLabel", 1}, // 2105
  {wxStaticText_SetLabel, "wxStaticText", "setLabel", 2}, // 2106
  {wxStaticText_Wrap, "wxStaticText", "wrap", 2}, // 2107
  {NULL, "wxStaticText", "'Destroy'", 1}, // 2108 obj destructor wxStaticText_destroy
  {wxStaticBitmap_new_0, "wxStaticBitmap", "new", 0}, // 2109
  {wxStaticBitmap_new_4, "wxStaticBitmap", "new", 4}, // 2110
  {wxStaticBitmap_Create, "wxStaticBitmap", "create", 5}, // 2111
  {wxStaticBitmap_GetBitmap, "wxStaticBitmap", "getBitmap", 1}, // 2112
  {wxStaticBitmap_SetBitmap, "wxStaticBitmap", "setBitmap", 2}, // 2113
  {NULL, "wxStaticBitmap", "'Destroy'", 1}, // 2114 obj destructor wxStaticBitmap_destroy
  {wxRadioBox_new, "wxRadioBox", "new", 7}, // 2115
  {NULL, "wxRadioBox", "destroy", 1}, // 2116 obj destructor wxRadioBox_destruct
  {wxRadioBox_Create, "wxRadioBox", "create", 8}, // 2117
  {wxRadioBox_Enable_1, "wxRadioBox", "enable", 2}, // 2118
  {wxRadioBox_Enable_2, "wxRadioBox", "enable", 3}, // 2119
  {wxRadioBox_GetSelection, "wxRadioBox", "getSelection", 1}, // 2120
  {wxRadioBox_GetString, "wxRadioBox", "getString", 2}, // 2121
  {wxRadioBox_SetSelection, "wxRadioBox", "setSelection", 2}, // 2122
  {wxRadioBox_Show, "wxRadioBox", "show", 3}, // 2123
  {wxRadioBox_GetColumnCount, "wxRadioBox", "getColumnCount", 1}, // 2124
  {wxRadioBox_GetItemHelpText, "wxRadioBox", "getItemHelpText", 2}, // 2125
  {wxRadioBox_GetItemToolTip, "wxRadioBox", "getItemToolTip", 2}, // 2126
  {wxRadioBox_GetItemFromPoint, "wxRadioBox", "getItemFromPoint", 2}, // 2127
  {wxRadioBox_GetRowCount, "wxRadioBox", "getRowCount", 1}, // 2128
  {wxRadioBox_IsItemEnabled, "wxRadioBox", "isItemEnabled", 2}, // 2129
  {wxRadioBox_IsItemShown, "wxRadioBox", "isItemShown", 2}, // 2130
  {wxRadioBox_SetItemHelpText, "wxRadioBox", "setItemHelpText", 3}, // 2131
  {wxRadioBox_SetItemToolTip, "wxRadioBox", "setItemToolTip", 3}, // 2132
  {wxRadioButton_new_0, "wxRadioButton", "new", 0}, // 2133
  {wxRadioButton_new_4, "wxRadioButton", "new", 4}, // 2134
  {NULL, "wxRadioButton", "destroy", 1}, // 2135 obj destructor wxRadioButton_destruct
  {wxRadioButton_Create, "wxRadioButton", "create", 5}, // 2136
  {wxRadioButton_GetValue, "wxRadioButton", "getValue", 1}, // 2137
  {wxRadioButton_SetValue, "wxRadioButton", "setValue", 2}, // 2138
  {wxSlider_new_0, "wxSlider", "new", 0}, // 2139
  {wxSlider_new_6, "wxSlider", "new", 6}, // 2140
  {NULL, "wxSlider", "destroy", 1}, // 2141 obj destructor wxSlider_destruct
  {wxSlider_Create, "wxSlider", "create", 7}, // 2142
  {wxSlider_GetLineSize, "wxSlider", "getLineSize", 1}, // 2143
  {wxSlider_GetMax, "wxSlider", "getMax", 1}, // 2144
  {wxSlider_GetMin, "wxSlider", "getMin", 1}, // 2145
  {wxSlider_GetPageSize, "wxSlider", "getPageSize", 1}, // 2146
  {wxSlider_GetThumbLength, "wxSlider", "getThumbLength", 1}, // 2147
  {wxSlider_GetValue, "wxSlider", "getValue", 1}, // 2148
  {wxSlider_SetLineSize, "wxSlider", "setLineSize", 2}, // 2149
  {wxSlider_SetPageSize, "wxSlider", "setPageSize", 2}, // 2150
  {wxSlider_SetRange, "wxSlider", "setRange", 3}, // 2151
  {wxSlider_SetThumbLength, "wxSlider", "setThumbLength", 2}, // 2152
  {wxSlider_SetValue, "wxSlider", "setValue", 2}, // 2153
  {wxDialog_new_0, "wxDialog", "new", 0}, // 2154
  {wxDialog_new_4, "wxDialog", "new", 4}, // 2155
  {NULL, "wxDialog", "destroy", 1}, // 2156 obj destructor wxDialog_destruct
  {wxDialog_Create, "wxDialog", "create", 5}, // 2157
  {wxDialog_CreateButtonSizer, "wxDialog", "createButtonSizer", 2}, // 2158
  {wxDialog_CreateStdDialogButtonSizer, "wxDialog", "createStdDialogButtonSizer", 2}, // 2159
  {wxDialog_EndModal, "wxDialog", "endModal", 2}, // 2160
  {wxDialog_GetAffirmativeId, "wxDialog", "getAffirmativeId", 1}, // 2161
  {wxDialog_GetReturnCode, "wxDialog", "getReturnCode", 1}, // 2162
  {wxDialog_IsModal, "wxDialog", "isModal", 1}, // 2163
  {wxDialog_SetAffirmativeId, "wxDialog", "setAffirmativeId", 2}, // 2164
  {wxDialog_SetReturnCode, "wxDialog", "setReturnCode", 2}, // 2165
  {wxDialog_Show, "wxDialog", "show", 2}, // 2166
  {wxDialog_ShowModal, "wxDialog", "showModal", 1}, // 2167
  {wxColourDialog_new_0, "wxColourDialog", "new", 0}, // 2168
  {wxColourDialog_new_2, "wxColourDialog", "new", 2}, // 2169
  {NULL, "wxColourDialog", "destroy", 1}, // 2170 obj destructor wxColourDialog_destruct
  {wxColourDialog_Create, "wxColourDialog", "create", 3}, // 2171
  {wxColourDialog_GetColourData, "wxColourDialog", "getColourData", 1}, // 2172
  {wxColourData_new, "wxColourData", "new", 0}, // 2173
  {NULL, "wxColourData", "destroy", 1}, // 2174 obj destructor wxColourData_destruct
  {wxColourData_GetChooseFull, "wxColourData", "getChooseFull", 1}, // 2175
  {wxColourData_GetColour, "wxColourData", "getColour", 1}, // 2176
  {wxColourData_GetCustomColour, "wxColourData", "getCustomColour", 2}, // 2177
  {wxColourData_SetChooseFull, "wxColourData", "setChooseFull", 2}, // 2178
  {wxColourData_SetColour, "wxColourData", "setColour", 2}, // 2179
  {wxColourData_SetCustomColour, "wxColourData", "setCustomColour", 3}, // 2180
  {wxPalette_new_0, "wxPalette", "new", 0}, // 2181
  {wxPalette_new_1, "wxPalette", "new", 1}, // 2182
  {wxPalette_new_4, "wxPalette", "new", 3}, // 2183
  {NULL, "wxPalette", "destroy", 1}, // 2184 obj destructor wxPalette_destruct
  {wxPalette_Create, "wxPalette", "create", 4}, // 2185
  {wxPalette_GetColoursCount, "wxPalette", "getColoursCount", 1}, // 2186
  {wxPalette_GetPixel, "wxPalette", "getPixel", 4}, // 2187
  {wxPalette_GetRGB, "wxPalette", "getRGB", 2}, // 2188
  {wxPalette_IsOk, "wxPalette", "isOk", 1}, // 2189
  {wxDirDialog_new, "wxDirDialog", "new", 2}, // 2190
  {NULL, "wxDirDialog", "destroy", 1}, // 2191 obj destructor wxDirDialog_destruct
  {wxDirDialog_GetPath, "wxDirDialog", "getPath", 1}, // 2192
  {wxDirDialog_GetMessage, "wxDirDialog", "getMessage", 1}, // 2193
  {wxDirDialog_SetMessage, "wxDirDialog", "setMessage", 2}, // 2194
  {wxDirDialog_SetPath, "wxDirDialog", "setPath", 2}, // 2195
  {wxFileDialog_new, "wxFileDialog", "new", 2}, // 2196
  {NULL, "wxFileDialog", "destroy", 1}, // 2197 obj destructor wxFileDialog_destruct
  {wxFileDialog_GetDirectory, "wxFileDialog", "getDirectory", 1}, // 2198
  {wxFileDialog_GetFilename, "wxFileDialog", "getFilename", 1}, // 2199
  {wxFileDialog_GetFilenames, "wxFileDialog", "getFilenames", 1}, // 2200
  {wxFileDialog_GetFilterIndex, "wxFileDialog", "getFilterIndex", 1}, // 2201
  {wxFileDialog_GetMessage, "wxFileDialog", "getMessage", 1}, // 2202
  {wxFileDialog_GetPath, "wxFileDialog", "getPath", 1}, // 2203
  {wxFileDialog_GetPaths, "wxFileDialog", "getPaths", 1}, // 2204
  {wxFileDialog_GetWildcard, "wxFileDialog", "getWildcard", 1}, // 2205
  {wxFileDialog_SetDirectory, "wxFileDialog", "setDirectory", 2}, // 2206
  {wxFileDialog_SetFilename, "wxFileDialog", "setFilename", 2}, // 2207
  {wxFileDialog_SetFilterIndex, "wxFileDialog", "setFilterIndex", 2}, // 2208
  {wxFileDialog_SetMessage, "wxFileDialog", "setMessage", 2}, // 2209
  {wxFileDialog_SetPath, "wxFileDialog", "setPath", 2}, // 2210
  {wxFileDialog_SetWildcard, "wxFileDialog", "setWildcard", 2}, // 2211
  {wxPickerBase_SetInternalMargin, "wxPickerBase", "setInternalMargin", 2}, // 2212
  {wxPickerBase_GetInternalMargin, "wxPickerBase", "getInternalMargin", 1}, // 2213
  {wxPickerBase_SetTextCtrlProportion, "wxPickerBase", "setTextCtrlProportion", 2}, // 2214
  {wxPickerBase_SetPickerCtrlProportion, "wxPickerBase", "setPickerCtrlProportion", 2}, // 2215
  {wxPickerBase_GetTextCtrlProportion, "wxPickerBase", "getTextCtrlProportion", 1}, // 2216
  {wxPickerBase_GetPickerCtrlProportion, "wxPickerBase", "getPickerCtrlProportion", 1}, // 2217
  {wxPickerBase_HasTextCtrl, "wxPickerBase", "hasTextCtrl", 1}, // 2218
  {wxPickerBase_GetTextCtrl, "wxPickerBase", "getTextCtrl", 1}, // 2219
  {wxPickerBase_IsTextCtrlGrowable, "wxPickerBase", "isTextCtrlGrowable", 1}, // 2220
  {wxPickerBase_SetPickerCtrlGrowable, "wxPickerBase", "setPickerCtrlGrowable", 2}, // 2221
  {wxPickerBase_SetTextCtrlGrowable, "wxPickerBase", "setTextCtrlGrowable", 2}, // 2222
  {wxPickerBase_IsPickerCtrlGrowable, "wxPickerBase", "isPickerCtrlGrowable", 1}, // 2223
  {wxFilePickerCtrl_new_0, "wxFilePickerCtrl", "new", 0}, // 2224
  {wxFilePickerCtrl_new_3, "wxFilePickerCtrl", "new", 3}, // 2225
  {wxFilePickerCtrl_Create, "wxFilePickerCtrl", "create", 4}, // 2226
  {wxFilePickerCtrl_GetPath, "wxFilePickerCtrl", "getPath", 1}, // 2227
  {wxFilePickerCtrl_SetPath, "wxFilePickerCtrl", "setPath", 2}, // 2228
  {NULL, "wxFilePickerCtrl", "'Destroy'", 1}, // 2229 obj destructor wxFilePickerCtrl_destroy
  {wxDirPickerCtrl_new_0, "wxDirPickerCtrl", "new", 0}, // 2230
  {wxDirPickerCtrl_new_3, "wxDirPickerCtrl", "new", 3}, // 2231
  {wxDirPickerCtrl_Create, "wxDirPickerCtrl", "create", 4}, // 2232
  {wxDirPickerCtrl_GetPath, "wxDirPickerCtrl", "getPath", 1}, // 2233
  {wxDirPickerCtrl_SetPath, "wxDirPickerCtrl", "setPath", 2}, // 2234
  {NULL, "wxDirPickerCtrl", "'Destroy'", 1}, // 2235 obj destructor wxDirPickerCtrl_destroy
  {wxColourPickerCtrl_new_0, "wxColourPickerCtrl", "new", 0}, // 2236
  {wxColourPickerCtrl_new_3, "wxColourPickerCtrl", "new", 3}, // 2237
  {wxColourPickerCtrl_Create, "wxColourPickerCtrl", "create", 4}, // 2238
  {wxColourPickerCtrl_GetColour, "wxColourPickerCtrl", "getColour", 1}, // 2239
  {wxColourPickerCtrl_SetColour_1_1, "wxColourPickerCtrl", "setColour", 2}, // 2240
  {wxColourPickerCtrl_SetColour_1_0, "wxColourPickerCtrl", "setColour", 2}, // 2241
  {NULL, "wxColourPickerCtrl", "'Destroy'", 1}, // 2242 obj destructor wxColourPickerCtrl_destroy
  {wxDatePickerCtrl_new_0, "wxDatePickerCtrl", "new", 0}, // 2243
  {wxDatePickerCtrl_new_3, "wxDatePickerCtrl", "new", 3}, // 2244
  {wxDatePickerCtrl_GetRange, "wxDatePickerCtrl", "getRange", 3}, // 2245
  {wxDatePickerCtrl_GetValue, "wxDatePickerCtrl", "getValue", 1}, // 2246
  {wxDatePickerCtrl_SetRange, "wxDatePickerCtrl", "setRange", 3}, // 2247
  {wxDatePickerCtrl_SetValue, "wxDatePickerCtrl", "setValue", 2}, // 2248
  {NULL, "wxDatePickerCtrl", "'Destroy'", 1}, // 2249 obj destructor wxDatePickerCtrl_destroy
  {wxFontPickerCtrl_new_0, "wxFontPickerCtrl", "new", 0}, // 2250
  {wxFontPickerCtrl_new_3, "wxFontPickerCtrl", "new", 3}, // 2251
  {wxFontPickerCtrl_Create, "wxFontPickerCtrl", "create", 4}, // 2252
  {wxFontPickerCtrl_GetSelectedFont, "wxFontPickerCtrl", "getSelectedFont", 1}, // 2253
  {wxFontPickerCtrl_SetSelectedFont, "wxFontPickerCtrl", "setSelectedFont", 2}, // 2254
  {wxFontPickerCtrl_GetMaxPointSize, "wxFontPickerCtrl", "getMaxPointSize", 1}, // 2255
  {wxFontPickerCtrl_SetMaxPointSize, "wxFontPickerCtrl", "setMaxPointSize", 2}, // 2256
  {NULL, "wxFontPickerCtrl", "'Destroy'", 1}, // 2257 obj destructor wxFontPickerCtrl_destroy
  {wxFindReplaceDialog_new_0, "wxFindReplaceDialog", "new", 0}, // 2258
  {wxFindReplaceDialog_new_4, "wxFindReplaceDialog", "new", 4}, // 2259
  {NULL, "wxFindReplaceDialog", "destroy", 1}, // 2260 obj destructor wxFindReplaceDialog_destruct
  {wxFindReplaceDialog_Create, "wxFindReplaceDialog", "create", 5}, // 2261
  {wxFindReplaceDialog_GetData, "wxFindReplaceDialog", "getData", 1}, // 2262
  {wxFindReplaceData_new, "wxFindReplaceData", "new", 1}, // 2263
  {wxFindReplaceData_GetFindString, "wxFindReplaceData", "getFindString", 1}, // 2264
  {wxFindReplaceData_GetReplaceString, "wxFindReplaceData", "getReplaceString", 1}, // 2265
  {wxFindReplaceData_GetFlags, "wxFindReplaceData", "getFlags", 1}, // 2266
  {wxFindReplaceData_SetFlags, "wxFindReplaceData", "setFlags", 2}, // 2267
  {wxFindReplaceData_SetFindString, "wxFindReplaceData", "setFindString", 2}, // 2268
  {wxFindReplaceData_SetReplaceString, "wxFindReplaceData", "setReplaceString", 2}, // 2269
  {NULL, "wxFindReplaceData", "'Destroy'", 1}, // 2270 obj destructor wxFindReplaceData_destroy
  {NULL, "", "", 0}, // 2271
  {wxMultiChoiceDialog_new, "wxMultiChoiceDialog", "new", 5}, // 2272
  {wxMultiChoiceDialog_GetSelections, "wxMultiChoiceDialog", "getSelections", 1}, // 2273
  {wxMultiChoiceDialog_SetSelections, "wxMultiChoiceDialog", "setSelections", 2}, // 2274
  {NULL, "wxMultiChoiceDialog", "'Destroy'", 1}, // 2275 obj destructor wxMultiChoiceDialog_destroy
  {NULL, "", "", 0}, // 2276
  {wxSingleChoiceDialog_new, "wxSingleChoiceDialog", "new", 5}, // 2277
  {wxSingleChoiceDialog_GetSelection, "wxSingleChoiceDialog", "getSelection", 1}, // 2278
  {wxSingleChoiceDialog_GetStringSelection, "wxSingleChoiceDialog", "getStringSelection", 1}, // 2279
  {wxSingleChoiceDialog_SetSelection, "wxSingleChoiceDialog", "setSelection", 2}, // 2280
  {NULL, "wxSingleChoiceDialog", "'Destroy'", 1}, // 2281 obj destructor wxSingleChoiceDialog_destroy
  {wxTextEntryDialog_new_0, "wxTextEntryDialog", "new", 0}, // 2282
  {wxTextEntryDialog_new_3, "wxTextEntryDialog", "new", 3}, // 2283
  {NULL, "wxTextEntryDialog", "destroy", 1}, // 2284 obj destructor wxTextEntryDialog_destruct
  {wxTextEntryDialog_GetValue, "wxTextEntryDialog", "getValue", 1}, // 2285
  {wxTextEntryDialog_SetValue, "wxTextEntryDialog", "setValue", 2}, // 2286
  {wxPasswordEntryDialog_new, "wxPasswordEntryDialog", "new", 3}, // 2287
  {NULL, "wxPasswordEntryDialog", "'Destroy'", 1}, // 2288 obj destructor wxPasswordEntryDialog_destroy
  {wxFontData_new_0, "wxFontData", "new", 0}, // 2289
  {wxFontData_new_1, "wxFontData", "new", 1}, // 2290
  {wxFontData_EnableEffects, "wxFontData", "enableEffects", 2}, // 2291
  {wxFontData_GetAllowSymbols, "wxFontData", "getAllowSymbols", 1}, // 2292
  {wxFontData_GetColour, "wxFontData", "getColour", 1}, // 2293
  {wxFontData_GetChosenFont, "wxFontData", "getChosenFont", 1}, // 2294
  {wxFontData_GetEnableEffects, "wxFontData", "getEnableEffects", 1}, // 2295
  {wxFontData_GetInitialFont, "wxFontData", "getInitialFont", 1}, // 2296
  {wxFontData_GetShowHelp, "wxFontData", "getShowHelp", 1}, // 2297
  {wxFontData_SetAllowSymbols, "wxFontData", "setAllowSymbols", 2}, // 2298
  {wxFontData_SetChosenFont, "wxFontData", "setChosenFont", 2}, // 2299
  {wxFontData_SetColour, "wxFontData", "setColour", 2}, // 2300
  {wxFontData_SetInitialFont, "wxFontData", "setInitialFont", 2}, // 2301
  {wxFontData_SetRange, "wxFontData", "setRange", 3}, // 2302
  {wxFontData_SetShowHelp, "wxFontData", "setShowHelp", 2}, // 2303
  {NULL, "wxFontData", "'Destroy'", 1}, // 2304 obj destructor wxFontData_destroy
  {wxFontDialog_new_0, "wxFontDialog", "new", 0}, // 2305
  {NULL, "", "", 0}, // 2306
  {wxFontDialog_new_2, "wxFontDialog", "new", 2}, // 2307
  {NULL, "", "", 0}, // 2308
  {wxFontDialog_Create, "wxFontDialog", "create", 3}, // 2309
  {wxFontDialog_GetFontData, "wxFontDialog", "getFontData", 1}, // 2310
  {NULL, "", "", 0}, // 2311
  {NULL, "wxFontDialog", "'Destroy'", 1}, // 2312 obj destructor wxFontDialog_destroy
  {wxProgressDialog_new, "wxProgressDialog", "new", 3}, // 2313
  {wxProgressDialog_Resume, "wxProgressDialog", "resume", 1}, // 2314
  {wxProgressDialog_Update, "wxProgressDialog", "update", 3}, // 2315
  {NULL, "wxProgressDialog", "'Destroy'", 1}, // 2316 obj destructor wxProgressDialog_destroy
  {wxMessageDialog_new, "wxMessageDialog", "new", 3}, // 2317
  {NULL, "wxMessageDialog", "'Destroy'", 1}, // 2318 obj destructor wxMessageDialog_destroy
  {wxPageSetupDialog_new, "wxPageSetupDialog", "new", 2}, // 2319
  {NULL, "wxPageSetupDialog", "destroy", 1}, // 2320 obj destructor wxPageSetupDialog_destruct
  {wxPageSetupDialog_GetPageSetupData, "wxPageSetupDialog", "getPageSetupData", 1}, // 2321
  {wxPageSetupDialog_ShowModal, "wxPageSetupDialog", "showModal", 1}, // 2322
  {wxPageSetupDialogData_new_0, "wxPageSetupDialogData", "new", 0}, // 2323
  {NULL, "", "", 0}, // 2324
  {wxPageSetupDialogData_new_1, "wxPageSetupDialogData", "new", 1}, // 2325
  {NULL, "wxPageSetupDialogData", "destroy", 1}, // 2326 obj destructor wxPageSetupDialogData_destruct
  {wxPageSetupDialogData_EnableHelp, "wxPageSetupDialogData", "enableHelp", 2}, // 2327
  {wxPageSetupDialogData_EnableMargins, "wxPageSetupDialogData", "enableMargins", 2}, // 2328
  {wxPageSetupDialogData_EnableOrientation, "wxPageSetupDialogData", "enableOrientation", 2}, // 2329
  {wxPageSetupDialogData_EnablePaper, "wxPageSetupDialogData", "enablePaper", 2}, // 2330
  {wxPageSetupDialogData_EnablePrinter, "wxPageSetupDialogData", "enablePrinter", 2}, // 2331
  {wxPageSetupDialogData_GetDefaultMinMargins, "wxPageSetupDialogData", "getDefaultMinMargins", 1}, // 2332
  {wxPageSetupDialogData_GetEnableMargins, "wxPageSetupDialogData", "getEnableMargins", 1}, // 2333
  {wxPageSetupDialogData_GetEnableOrientation, "wxPageSetupDialogData", "getEnableOrientation", 1}, // 2334
  {wxPageSetupDialogData_GetEnablePaper, "wxPageSetupDialogData", "getEnablePaper", 1}, // 2335
  {wxPageSetupDialogData_GetEnablePrinter, "wxPageSetupDialogData", "getEnablePrinter", 1}, // 2336
  {wxPageSetupDialogData_GetEnableHelp, "wxPageSetupDialogData", "getEnableHelp", 1}, // 2337
  {wxPageSetupDialogData_GetDefaultInfo, "wxPageSetupDialogData", "getDefaultInfo", 1}, // 2338
  {wxPageSetupDialogData_GetMarginTopLeft, "wxPageSetupDialogData", "getMarginTopLeft", 1}, // 2339
  {wxPageSetupDialogData_GetMarginBottomRight, "wxPageSetupDialogData", "getMarginBottomRight", 1}, // 2340
  {wxPageSetupDialogData_GetMinMarginTopLeft, "wxPageSetupDialogData", "getMinMarginTopLeft", 1}, // 2341
  {wxPageSetupDialogData_GetMinMarginBottomRight, "wxPageSetupDialogData", "getMinMarginBottomRight", 1}, // 2342
  {wxPageSetupDialogData_GetPaperId, "wxPageSetupDialogData", "getPaperId", 1}, // 2343
  {wxPageSetupDialogData_GetPaperSize, "wxPageSetupDialogData", "getPaperSize", 1}, // 2344
  {NULL, "", "", 0}, // 2345
  {wxPageSetupDialogData_GetPrintData, "wxPageSetupDialogData", "getPrintData", 1}, // 2346
  {wxPageSetupDialogData_IsOk, "wxPageSetupDialogData", "isOk", 1}, // 2347
  {wxPageSetupDialogData_SetDefaultInfo, "wxPageSetupDialogData", "setDefaultInfo", 2}, // 2348
  {wxPageSetupDialogData_SetDefaultMinMargins, "wxPageSetupDialogData", "setDefaultMinMargins", 2}, // 2349
  {wxPageSetupDialogData_SetMarginTopLeft, "wxPageSetupDialogData", "setMarginTopLeft", 2}, // 2350
  {wxPageSetupDialogData_SetMarginBottomRight, "wxPageSetupDialogData", "setMarginBottomRight", 2}, // 2351
  {wxPageSetupDialogData_SetMinMarginTopLeft, "wxPageSetupDialogData", "setMinMarginTopLeft", 2}, // 2352
  {wxPageSetupDialogData_SetMinMarginBottomRight, "wxPageSetupDialogData", "setMinMarginBottomRight", 2}, // 2353
  {wxPageSetupDialogData_SetPaperId, "wxPageSetupDialogData", "setPaperId", 2}, // 2354
  {wxPageSetupDialogData_SetPaperSize, "wxPageSetupDialogData", "setPaperSize", 2}, // 2355
  {wxPageSetupDialogData_SetPrintData, "wxPageSetupDialogData", "setPrintData", 2}, // 2356
  {wxPrintDialog_new_2_0, "wxPrintDialog", "new", 2}, // 2357
  {wxPrintDialog_new_2_1, "wxPrintDialog", "new", 2}, // 2358
  {NULL, "wxPrintDialog", "destroy", 1}, // 2359 obj destructor wxPrintDialog_destruct
  {wxPrintDialog_GetPrintDialogData, "wxPrintDialog", "getPrintDialogData", 1}, // 2360
  {wxPrintDialog_GetPrintDC, "wxPrintDialog", "getPrintDC", 1}, // 2361
  {wxPrintDialogData_new_0, "wxPrintDialogData", "new", 0}, // 2362
  {wxPrintDialogData_new_1, "wxPrintDialogData", "new", 1}, // 2363
  {NULL, "", "", 0}, // 2364
  {NULL, "wxPrintDialogData", "destroy", 1}, // 2365 obj destructor wxPrintDialogData_destruct
  {wxPrintDialogData_EnableHelp, "wxPrintDialogData", "enableHelp", 2}, // 2366
  {wxPrintDialogData_EnablePageNumbers, "wxPrintDialogData", "enablePageNumbers", 2}, // 2367
  {wxPrintDialogData_EnablePrintToFile, "wxPrintDialogData", "enablePrintToFile", 2}, // 2368
  {wxPrintDialogData_EnableSelection, "wxPrintDialogData", "enableSelection", 2}, // 2369
  {wxPrintDialogData_GetAllPages, "wxPrintDialogData", "getAllPages", 1}, // 2370
  {wxPrintDialogData_GetCollate, "wxPrintDialogData", "getCollate", 1}, // 2371
  {wxPrintDialogData_GetFromPage, "wxPrintDialogData", "getFromPage", 1}, // 2372
  {wxPrintDialogData_GetMaxPage, "wxPrintDialogData", "getMaxPage", 1}, // 2373
  {wxPrintDialogData_GetMinPage, "wxPrintDialogData", "getMinPage", 1}, // 2374
  {wxPrintDialogData_GetNoCopies, "wxPrintDialogData", "getNoCopies", 1}, // 2375
  {wxPrintDialogData_GetPrintData, "wxPrintDialogData", "getPrintData", 1}, // 2376
  {wxPrintDialogData_GetPrintToFile, "wxPrintDialogData", "getPrintToFile", 1}, // 2377
  {wxPrintDialogData_GetSelection, "wxPrintDialogData", "getSelection", 1}, // 2378
  {wxPrintDialogData_GetToPage, "wxPrintDialogData", "getToPage", 1}, // 2379
  {wxPrintDialogData_IsOk, "wxPrintDialogData", "isOk", 1}, // 2380
  {wxPrintDialogData_SetCollate, "wxPrintDialogData", "setCollate", 2}, // 2381
  {wxPrintDialogData_SetFromPage, "wxPrintDialogData", "setFromPage", 2}, // 2382
  {wxPrintDialogData_SetMaxPage, "wxPrintDialogData", "setMaxPage", 2}, // 2383
  {wxPrintDialogData_SetMinPage, "wxPrintDialogData", "setMinPage", 2}, // 2384
  {wxPrintDialogData_SetNoCopies, "wxPrintDialogData", "setNoCopies", 2}, // 2385
  {wxPrintDialogData_SetPrintData, "wxPrintDialogData", "setPrintData", 2}, // 2386
  {wxPrintDialogData_SetPrintToFile, "wxPrintDialogData", "setPrintToFile", 2}, // 2387
  {wxPrintDialogData_SetSelection, "wxPrintDialogData", "setSelection", 2}, // 2388
  {wxPrintDialogData_SetToPage, "wxPrintDialogData", "setToPage", 2}, // 2389
  {wxPrintData_new_0, "wxPrintData", "new", 0}, // 2390
  {wxPrintData_new_1, "wxPrintData", "new", 1}, // 2391
  {NULL, "wxPrintData", "destroy", 1}, // 2392 obj destructor wxPrintData_destruct
  {wxPrintData_GetCollate, "wxPrintData", "getCollate", 1}, // 2393
  {wxPrintData_GetBin, "wxPrintData", "getBin", 1}, // 2394
  {wxPrintData_GetColour, "wxPrintData", "getColour", 1}, // 2395
  {wxPrintData_GetDuplex, "wxPrintData", "getDuplex", 1}, // 2396
  {wxPrintData_GetNoCopies, "wxPrintData", "getNoCopies", 1}, // 2397
  {wxPrintData_GetOrientation, "wxPrintData", "getOrientation", 1}, // 2398
  {wxPrintData_GetPaperId, "wxPrintData", "getPaperId", 1}, // 2399
  {wxPrintData_GetPrinterName, "wxPrintData", "getPrinterName", 1}, // 2400
  {wxPrintData_GetQuality, "wxPrintData", "getQuality", 1}, // 2401
  {wxPrintData_IsOk, "wxPrintData", "isOk", 1}, // 2402
  {wxPrintData_SetBin, "wxPrintData", "setBin", 2}, // 2403
  {wxPrintData_SetCollate, "wxPrintData", "setCollate", 2}, // 2404
  {wxPrintData_SetColour, "wxPrintData", "setColour", 2}, // 2405
  {wxPrintData_SetDuplex, "wxPrintData", "setDuplex", 2}, // 2406
  {wxPrintData_SetNoCopies, "wxPrintData", "setNoCopies", 2}, // 2407
  {wxPrintData_SetOrientation, "wxPrintData", "setOrientation", 2}, // 2408
  {wxPrintData_SetPaperId, "wxPrintData", "setPaperId", 2}, // 2409
  {wxPrintData_SetPrinterName, "wxPrintData", "setPrinterName", 2}, // 2410
  {wxPrintData_SetQuality, "wxPrintData", "setQuality", 2}, // 2411
  {wxPrintPreview_new_2, "wxPrintPreview", "new", 2}, // 2412
  {wxPrintPreview_new_3, "wxPrintPreview", "new", 3}, // 2413
  {NULL, "wxPrintPreview", "destroy", 1}, // 2414 obj destructor wxPrintPreview_destruct
  {wxPrintPreview_GetCanvas, "wxPrintPreview", "getCanvas", 1}, // 2415
  {wxPrintPreview_GetCurrentPage, "wxPrintPreview", "getCurrentPage", 1}, // 2416
  {wxPrintPreview_GetFrame, "wxPrintPreview", "getFrame", 1}, // 2417
  {wxPrintPreview_GetMaxPage, "wxPrintPreview", "getMaxPage", 1}, // 2418
  {wxPrintPreview_GetMinPage, "wxPrintPreview", "getMinPage", 1}, // 2419
  {wxPrintPreview_GetPrintout, "wxPrintPreview", "getPrintout", 1}, // 2420
  {wxPrintPreview_GetPrintoutForPrinting, "wxPrintPreview", "getPrintoutForPrinting", 1}, // 2421
  {wxPrintPreview_IsOk, "wxPrintPreview", "isOk", 1}, // 2422
  {wxPrintPreview_PaintPage, "wxPrintPreview", "paintPage", 3}, // 2423
  {wxPrintPreview_Print, "wxPrintPreview", "print", 2}, // 2424
  {wxPrintPreview_RenderPage, "wxPrintPreview", "renderPage", 2}, // 2425
  {wxPrintPreview_SetCanvas, "wxPrintPreview", "setCanvas", 2}, // 2426
  {wxPrintPreview_SetCurrentPage, "wxPrintPreview", "setCurrentPage", 2}, // 2427
  {wxPrintPreview_SetFrame, "wxPrintPreview", "setFrame", 2}, // 2428
  {wxPrintPreview_SetPrintout, "wxPrintPreview", "setPrintout", 2}, // 2429
  {wxPrintPreview_SetZoom, "wxPrintPreview", "setZoom", 2}, // 2430
  {wxPreviewFrame_new, "wxPreviewFrame", "new", 3}, // 2431
  {NULL, "wxPreviewFrame", "destroy", 1}, // 2432 obj destructor wxPreviewFrame_destruct
  {wxPreviewFrame_CreateControlBar, "wxPreviewFrame", "createControlBar", 1}, // 2433
  {wxPreviewFrame_CreateCanvas, "wxPreviewFrame", "createCanvas", 1}, // 2434
  {wxPreviewFrame_Initialize, "wxPreviewFrame", "initialize", 1}, // 2435
  {wxPreviewFrame_OnCloseWindow, "wxPreviewFrame", "onCloseWindow", 2}, // 2436
  {wxPreviewControlBar_new, "wxPreviewControlBar", "new", 4}, // 2437
  {NULL, "wxPreviewControlBar", "destroy", 1}, // 2438 obj destructor wxPreviewControlBar_destruct
  {wxPreviewControlBar_CreateButtons, "wxPreviewControlBar", "createButtons", 1}, // 2439
  {wxPreviewControlBar_GetPrintPreview, "wxPreviewControlBar", "getPrintPreview", 1}, // 2440
  {wxPreviewControlBar_GetZoomControl, "wxPreviewControlBar", "getZoomControl", 1}, // 2441
  {wxPreviewControlBar_SetZoomControl, "wxPreviewControlBar", "setZoomControl", 2}, // 2442
  {wxPrinter_new, "wxPrinter", "new", 1}, // 2443
  {wxPrinter_CreateAbortWindow, "wxPrinter", "createAbortWindow", 3}, // 2444
  {wxPrinter_GetAbort, "wxPrinter", "getAbort", 1}, // 2445
  {wxPrinter_GetLastError, "wxPrinter", "getLastError", 0}, // 2446
  {wxPrinter_GetPrintDialogData, "wxPrinter", "getPrintDialogData", 1}, // 2447
  {wxPrinter_Print, "wxPrinter", "print", 4}, // 2448
  {wxPrinter_PrintDialog, "wxPrinter", "printDialog", 2}, // 2449
  {wxPrinter_ReportError, "wxPrinter", "reportError", 4}, // 2450
  {wxPrinter_Setup, "wxPrinter", "setup", 2}, // 2451
  {NULL, "wxPrinter", "'Destroy'", 1}, // 2452 obj destructor wxPrinter_destroy
  {wxXmlResource_new_2, "wxXmlResource", "new", 2}, // 2453
  {wxXmlResource_new_1, "wxXmlResource", "new", 1}, // 2454
  {NULL, "wxXmlResource", "destroy", 1}, // 2455 obj destructor wxXmlResource_destruct
  {wxXmlResource_AttachUnknownControl, "wxXmlResource", "attachUnknownControl", 4}, // 2456
  {wxXmlResource_ClearHandlers, "wxXmlResource", "clearHandlers", 1}, // 2457
  {wxXmlResource_CompareVersion, "wxXmlResource", "compareVersion", 5}, // 2458
  {wxXmlResource_Get, "wxXmlResource", "get", 0}, // 2459
  {wxXmlResource_GetFlags, "wxXmlResource", "getFlags", 1}, // 2460
  {wxXmlResource_GetVersion, "wxXmlResource", "getVersion", 1}, // 2461
  {wxXmlResource_GetXRCID, "wxXmlResource", "getXRCID", 2}, // 2462
  {wxXmlResource_InitAllHandlers, "wxXmlResource", "initAllHandlers", 1}, // 2463
  {wxXmlResource_Load, "wxXmlResource", "load", 2}, // 2464
  {wxXmlResource_LoadBitmap, "wxXmlResource", "loadBitmap", 2}, // 2465
  {wxXmlResource_LoadDialog_2, "wxXmlResource", "loadDialog", 3}, // 2466
  {wxXmlResource_LoadDialog_3, "wxXmlResource", "loadDialog", 4}, // 2467
  {wxXmlResource_LoadFrame_2, "wxXmlResource", "loadFrame", 3}, // 2468
  {wxXmlResource_LoadFrame_3, "wxXmlResource", "loadFrame", 4}, // 2469
  {wxXmlResource_LoadIcon, "wxXmlResource", "loadIcon", 2}, // 2470
  {wxXmlResource_LoadMenu, "wxXmlResource", "loadMenu", 2}, // 2471
  {wxXmlResource_LoadMenuBar_2, "wxXmlResource", "loadMenuBar", 3}, // 2472
  {wxXmlResource_LoadMenuBar_1, "wxXmlResource", "loadMenuBar", 2}, // 2473
  {wxXmlResource_LoadPanel_2, "wxXmlResource", "loadPanel", 3}, // 2474
  {wxXmlResource_LoadPanel_3, "wxXmlResource", "loadPanel", 4}, // 2475
  {wxXmlResource_LoadToolBar, "wxXmlResource", "loadToolBar", 3}, // 2476
  {wxXmlResource_Set, "wxXmlResource", "set", 1}, // 2477
  {wxXmlResource_SetFlags, "wxXmlResource", "setFlags", 2}, // 2478
  {wxXmlResource_Unload, "wxXmlResource", "unload", 2}, // 2479
  {NULL, "wxXmlResource", "xrcctrl", 3}, // 2480 TaylorMade erl only wxXmlResource_xrcctrl
  {wxHtmlEasyPrinting_new, "wxHtmlEasyPrinting", "new", 1}, // 2481
  {wxHtmlEasyPrinting_GetPrintData, "wxHtmlEasyPrinting", "getPrintData", 1}, // 2482
  {wxHtmlEasyPrinting_GetPageSetupData, "wxHtmlEasyPrinting", "getPageSetupData", 1}, // 2483
  {wxHtmlEasyPrinting_PreviewFile, "wxHtmlEasyPrinting", "previewFile", 2}, // 2484
  {wxHtmlEasyPrinting_PreviewText, "wxHtmlEasyPrinting", "previewText", 3}, // 2485
  {wxHtmlEasyPrinting_PrintFile, "wxHtmlEasyPrinting", "printFile", 2}, // 2486
  {wxHtmlEasyPrinting_PrintText, "wxHtmlEasyPrinting", "printText", 3}, // 2487
  {wxHtmlEasyPrinting_PageSetup, "wxHtmlEasyPrinting", "pageSetup", 1}, // 2488
  {wxHtmlEasyPrinting_SetFonts, "wxHtmlEasyPrinting", "setFonts", 4}, // 2489
  {wxHtmlEasyPrinting_SetHeader, "wxHtmlEasyPrinting", "setHeader", 3}, // 2490
  {wxHtmlEasyPrinting_SetFooter, "wxHtmlEasyPrinting", "setFooter", 3}, // 2491
  {NULL, "wxHtmlEasyPrinting", "'Destroy'", 1}, // 2492 obj destructor wxHtmlEasyPrinting_destroy
#if wxUSE_GLCANVAS
  {wxGLCanvas_new, "wxGLCanvas", "new", 2}, // 2493
#else
  {NULL, "wxGLCanvas", "new", 0}, // 2493
#endif // wxUSE_GLCANVAS
#if wxUSE_GLCANVAS
  {wxGLCanvas_SetCurrent, "wxGLCanvas", "setCurrent", 2}, // 2494
#else
  {NULL, "wxGLCanvas", "setCurrent", 0}, // 2494
#endif // wxUSE_GLCANVAS
#if wxUSE_GLCANVAS && wxUSE_GLCANVAS_EGL
  {wxGLCanvas_CreateSurface, "wxGLCanvas", "createSurface", 1}, // 2495
#else
  {NULL, "wxGLCanvas", "createSurface", 0}, // 2495
#endif // wxUSE_GLCANVAS && wxUSE_GLCANVAS_EGL
#if wxUSE_GLCANVAS
  {wxGLCanvas_IsDisplaySupported, "wxGLCanvas", "isDisplaySupported", 1}, // 2496
#else
  {NULL, "wxGLCanvas", "isDisplaySupported", 0}, // 2496
#endif // wxUSE_GLCANVAS
#if wxUSE_GLCANVAS
  {wxGLCanvas_SwapBuffers, "wxGLCanvas", "swapBuffers", 1}, // 2497
#else
  {NULL, "wxGLCanvas", "swapBuffers", 0}, // 2497
#endif // wxUSE_GLCANVAS
#if wxUSE_GLCANVAS
  {NULL, "wxGLCanvas", "'Destroy'", 1}, // 2498 obj destructor wxGLCanvas_destroy
#else
  {NULL, "wxGLCanvas", "'Destroy'", 0}, // 2498
#endif // wxUSE_GLCANVAS
#if wxUSE_GLCANVAS
  {wxGLContext_new, "wxGLContext", "new", 2}, // 2499
#else
  {NULL, "wxGLContext", "new", 0}, // 2499
#endif // wxUSE_GLCANVAS
#if wxUSE_GLCANVAS
  {wxGLContext_SetCurrent, "wxGLContext", "setCurrent", 2}, // 2500
#else
  {NULL, "wxGLContext", "setCurrent", 0}, // 2500
#endif // wxUSE_GLCANVAS
#if wxUSE_GLCANVAS && wxCHECK_VERSION(3,1,0)
  {wxGLContext_IsOK, "wxGLContext", "isOK", 1}, // 2501
#else
  {NULL, "wxGLContext", "isOK", 0}, // 2501
#endif // wxUSE_GLCANVAS && wxCHECK_VERSION(3,1,0)
#if wxUSE_GLCANVAS
  {NULL, "wxGLContext", "'Destroy'", 1}, // 2502 obj destructor wxGLContext_destroy
#else
  {NULL, "wxGLContext", "'Destroy'", 0}, // 2502
#endif // wxUSE_GLCANVAS
#if wxUSE_AUI
  {wxAuiManager_new, "wxAuiManager", "new", 1}, // 2503
#else
  {NULL, "wxAuiManager", "new", 0}, // 2503
#endif // wxUSE_AUI
#if wxUSE_AUI
  {NULL, "wxAuiManager", "destroy", 1}, // 2504 obj destructor wxAuiManager_destruct
#else
  {NULL, "wxAuiManager", "destroy", 0}, // 2504
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_AddPane_2_1, "wxAuiManager", "addPane", 3}, // 2505
#else
  {NULL, "wxAuiManager", "addPane", 0}, // 2505
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_AddPane_2_0, "wxAuiManager", "addPane", 3}, // 2506
#else
  {NULL, "wxAuiManager", "addPane", 0}, // 2506
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_AddPane_3, "wxAuiManager", "addPane", 4}, // 2507
#else
  {NULL, "wxAuiManager", "addPane", 0}, // 2507
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_DetachPane, "wxAuiManager", "detachPane", 2}, // 2508
#else
  {NULL, "wxAuiManager", "detachPane", 0}, // 2508
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_GetAllPanes, "wxAuiManager", "getAllPanes", 1}, // 2509
#else
  {NULL, "wxAuiManager", "getAllPanes", 0}, // 2509
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_GetArtProvider, "wxAuiManager", "getArtProvider", 1}, // 2510
#else
  {NULL, "wxAuiManager", "getArtProvider", 0}, // 2510
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_GetDockSizeConstraint, "wxAuiManager", "getDockSizeConstraint", 1}, // 2511
#else
  {NULL, "wxAuiManager", "getDockSizeConstraint", 0}, // 2511
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_GetFlags, "wxAuiManager", "getFlags", 1}, // 2512
#else
  {NULL, "wxAuiManager", "getFlags", 0}, // 2512
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_GetManagedWindow, "wxAuiManager", "getManagedWindow", 1}, // 2513
#else
  {NULL, "wxAuiManager", "getManagedWindow", 0}, // 2513
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_GetManager, "wxAuiManager", "getManager", 1}, // 2514
#else
  {NULL, "wxAuiManager", "getManager", 0}, // 2514
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_GetPane_1_1, "wxAuiManager", "getPane", 2}, // 2515
#else
  {NULL, "wxAuiManager", "getPane", 0}, // 2515
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_GetPane_1_0, "wxAuiManager", "getPane", 2}, // 2516
#else
  {NULL, "wxAuiManager", "getPane", 0}, // 2516
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_HideHint, "wxAuiManager", "hideHint", 1}, // 2517
#else
  {NULL, "wxAuiManager", "hideHint", 0}, // 2517
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_InsertPane, "wxAuiManager", "insertPane", 4}, // 2518
#else
  {NULL, "wxAuiManager", "insertPane", 0}, // 2518
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_LoadPaneInfo, "wxAuiManager", "loadPaneInfo", 3}, // 2519
#else
  {NULL, "wxAuiManager", "loadPaneInfo", 0}, // 2519
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_LoadPerspective, "wxAuiManager", "loadPerspective", 3}, // 2520
#else
  {NULL, "wxAuiManager", "loadPerspective", 0}, // 2520
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_SavePaneInfo, "wxAuiManager", "savePaneInfo", 2}, // 2521
#else
  {NULL, "wxAuiManager", "savePaneInfo", 0}, // 2521
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_SavePerspective, "wxAuiManager", "savePerspective", 1}, // 2522
#else
  {NULL, "wxAuiManager", "savePerspective", 0}, // 2522
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_SetArtProvider, "wxAuiManager", "setArtProvider", 2}, // 2523
#else
  {NULL, "wxAuiManager", "setArtProvider", 0}, // 2523
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_SetDockSizeConstraint, "wxAuiManager", "setDockSizeConstraint", 3}, // 2524
#else
  {NULL, "wxAuiManager", "setDockSizeConstraint", 0}, // 2524
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_SetFlags, "wxAuiManager", "setFlags", 2}, // 2525
#else
  {NULL, "wxAuiManager", "setFlags", 0}, // 2525
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_SetManagedWindow, "wxAuiManager", "setManagedWindow", 2}, // 2526
#else
  {NULL, "wxAuiManager", "setManagedWindow", 0}, // 2526
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_ShowHint, "wxAuiManager", "showHint", 2}, // 2527
#else
  {NULL, "wxAuiManager", "showHint", 0}, // 2527
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_UnInit, "wxAuiManager", "unInit", 1}, // 2528
#else
  {NULL, "wxAuiManager", "unInit", 0}, // 2528
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiManager_Update, "wxAuiManager", "update", 1}, // 2529
#else
  {NULL, "wxAuiManager", "update", 0}, // 2529
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_new_0, "wxAuiPaneInfo", "new", 0}, // 2530
#else
  {NULL, "wxAuiPaneInfo", "new", 0}, // 2530
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_new_1, "wxAuiPaneInfo", "new", 1}, // 2531
#else
  {NULL, "wxAuiPaneInfo", "new", 0}, // 2531
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_BestSize_1, "wxAuiPaneInfo", "bestSize", 2}, // 2532
#else
  {NULL, "wxAuiPaneInfo", "bestSize", 0}, // 2532
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_BestSize_2, "wxAuiPaneInfo", "bestSize", 3}, // 2533
#else
  {NULL, "wxAuiPaneInfo", "bestSize", 0}, // 2533
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Bottom, "wxAuiPaneInfo", "bottom", 1}, // 2534
#else
  {NULL, "wxAuiPaneInfo", "bottom", 0}, // 2534
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_BottomDockable, "wxAuiPaneInfo", "bottomDockable", 2}, // 2535
#else
  {NULL, "wxAuiPaneInfo", "bottomDockable", 0}, // 2535
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Caption, "wxAuiPaneInfo", "caption", 2}, // 2536
#else
  {NULL, "wxAuiPaneInfo", "caption", 0}, // 2536
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_CaptionVisible, "wxAuiPaneInfo", "captionVisible", 2}, // 2537
#else
  {NULL, "wxAuiPaneInfo", "captionVisible", 0}, // 2537
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Centre, "wxAuiPaneInfo", "centre", 1}, // 2538
#else
  {NULL, "wxAuiPaneInfo", "centre", 0}, // 2538
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_CentrePane, "wxAuiPaneInfo", "centrePane", 1}, // 2539
#else
  {NULL, "wxAuiPaneInfo", "centrePane", 0}, // 2539
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_CloseButton, "wxAuiPaneInfo", "closeButton", 2}, // 2540
#else
  {NULL, "wxAuiPaneInfo", "closeButton", 0}, // 2540
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_DefaultPane, "wxAuiPaneInfo", "defaultPane", 1}, // 2541
#else
  {NULL, "wxAuiPaneInfo", "defaultPane", 0}, // 2541
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_DestroyOnClose, "wxAuiPaneInfo", "destroyOnClose", 2}, // 2542
#else
  {NULL, "wxAuiPaneInfo", "destroyOnClose", 0}, // 2542
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Direction, "wxAuiPaneInfo", "direction", 2}, // 2543
#else
  {NULL, "wxAuiPaneInfo", "direction", 0}, // 2543
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Dock, "wxAuiPaneInfo", "dock", 1}, // 2544
#else
  {NULL, "wxAuiPaneInfo", "dock", 0}, // 2544
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Dockable, "wxAuiPaneInfo", "dockable", 2}, // 2545
#else
  {NULL, "wxAuiPaneInfo", "dockable", 0}, // 2545
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Fixed, "wxAuiPaneInfo", "fixed", 1}, // 2546
#else
  {NULL, "wxAuiPaneInfo", "fixed", 0}, // 2546
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Float, "wxAuiPaneInfo", "float", 1}, // 2547
#else
  {NULL, "wxAuiPaneInfo", "float", 0}, // 2547
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Floatable, "wxAuiPaneInfo", "floatable", 2}, // 2548
#else
  {NULL, "wxAuiPaneInfo", "floatable", 0}, // 2548
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_FloatingPosition_1, "wxAuiPaneInfo", "floatingPosition", 2}, // 2549
#else
  {NULL, "wxAuiPaneInfo", "floatingPosition", 0}, // 2549
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_FloatingPosition_2, "wxAuiPaneInfo", "floatingPosition", 3}, // 2550
#else
  {NULL, "wxAuiPaneInfo", "floatingPosition", 0}, // 2550
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_FloatingSize_1, "wxAuiPaneInfo", "floatingSize", 2}, // 2551
#else
  {NULL, "wxAuiPaneInfo", "floatingSize", 0}, // 2551
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_FloatingSize_2, "wxAuiPaneInfo", "floatingSize", 3}, // 2552
#else
  {NULL, "wxAuiPaneInfo", "floatingSize", 0}, // 2552
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Gripper, "wxAuiPaneInfo", "gripper", 2}, // 2553
#else
  {NULL, "wxAuiPaneInfo", "gripper", 0}, // 2553
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_GripperTop, "wxAuiPaneInfo", "gripperTop", 2}, // 2554
#else
  {NULL, "wxAuiPaneInfo", "gripperTop", 0}, // 2554
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_HasBorder, "wxAuiPaneInfo", "hasBorder", 1}, // 2555
#else
  {NULL, "wxAuiPaneInfo", "hasBorder", 0}, // 2555
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_HasCaption, "wxAuiPaneInfo", "hasCaption", 1}, // 2556
#else
  {NULL, "wxAuiPaneInfo", "hasCaption", 0}, // 2556
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_HasCloseButton, "wxAuiPaneInfo", "hasCloseButton", 1}, // 2557
#else
  {NULL, "wxAuiPaneInfo", "hasCloseButton", 0}, // 2557
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_HasFlag, "wxAuiPaneInfo", "hasFlag", 2}, // 2558
#else
  {NULL, "wxAuiPaneInfo", "hasFlag", 0}, // 2558
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_HasGripper, "wxAuiPaneInfo", "hasGripper", 1}, // 2559
#else
  {NULL, "wxAuiPaneInfo", "hasGripper", 0}, // 2559
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_HasGripperTop, "wxAuiPaneInfo", "hasGripperTop", 1}, // 2560
#else
  {NULL, "wxAuiPaneInfo", "hasGripperTop", 0}, // 2560
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_HasMaximizeButton, "wxAuiPaneInfo", "hasMaximizeButton", 1}, // 2561
#else
  {NULL, "wxAuiPaneInfo", "hasMaximizeButton", 0}, // 2561
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_HasMinimizeButton, "wxAuiPaneInfo", "hasMinimizeButton", 1}, // 2562
#else
  {NULL, "wxAuiPaneInfo", "hasMinimizeButton", 0}, // 2562
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_HasPinButton, "wxAuiPaneInfo", "hasPinButton", 1}, // 2563
#else
  {NULL, "wxAuiPaneInfo", "hasPinButton", 0}, // 2563
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Hide, "wxAuiPaneInfo", "hide", 1}, // 2564
#else
  {NULL, "wxAuiPaneInfo", "hide", 0}, // 2564
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_IsBottomDockable, "wxAuiPaneInfo", "isBottomDockable", 1}, // 2565
#else
  {NULL, "wxAuiPaneInfo", "isBottomDockable", 0}, // 2565
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_IsDocked, "wxAuiPaneInfo", "isDocked", 1}, // 2566
#else
  {NULL, "wxAuiPaneInfo", "isDocked", 0}, // 2566
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_IsFixed, "wxAuiPaneInfo", "isFixed", 1}, // 2567
#else
  {NULL, "wxAuiPaneInfo", "isFixed", 0}, // 2567
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_IsFloatable, "wxAuiPaneInfo", "isFloatable", 1}, // 2568
#else
  {NULL, "wxAuiPaneInfo", "isFloatable", 0}, // 2568
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_IsFloating, "wxAuiPaneInfo", "isFloating", 1}, // 2569
#else
  {NULL, "wxAuiPaneInfo", "isFloating", 0}, // 2569
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_IsLeftDockable, "wxAuiPaneInfo", "isLeftDockable", 1}, // 2570
#else
  {NULL, "wxAuiPaneInfo", "isLeftDockable", 0}, // 2570
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_IsMovable, "wxAuiPaneInfo", "isMovable", 1}, // 2571
#else
  {NULL, "wxAuiPaneInfo", "isMovable", 0}, // 2571
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_IsOk, "wxAuiPaneInfo", "isOk", 1}, // 2572
#else
  {NULL, "wxAuiPaneInfo", "isOk", 0}, // 2572
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_IsResizable, "wxAuiPaneInfo", "isResizable", 1}, // 2573
#else
  {NULL, "wxAuiPaneInfo", "isResizable", 0}, // 2573
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_IsRightDockable, "wxAuiPaneInfo", "isRightDockable", 1}, // 2574
#else
  {NULL, "wxAuiPaneInfo", "isRightDockable", 0}, // 2574
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_IsShown, "wxAuiPaneInfo", "isShown", 1}, // 2575
#else
  {NULL, "wxAuiPaneInfo", "isShown", 0}, // 2575
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_IsToolbar, "wxAuiPaneInfo", "isToolbar", 1}, // 2576
#else
  {NULL, "wxAuiPaneInfo", "isToolbar", 0}, // 2576
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_IsTopDockable, "wxAuiPaneInfo", "isTopDockable", 1}, // 2577
#else
  {NULL, "wxAuiPaneInfo", "isTopDockable", 0}, // 2577
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Layer, "wxAuiPaneInfo", "layer", 2}, // 2578
#else
  {NULL, "wxAuiPaneInfo", "layer", 0}, // 2578
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Left, "wxAuiPaneInfo", "left", 1}, // 2579
#else
  {NULL, "wxAuiPaneInfo", "left", 0}, // 2579
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_LeftDockable, "wxAuiPaneInfo", "leftDockable", 2}, // 2580
#else
  {NULL, "wxAuiPaneInfo", "leftDockable", 0}, // 2580
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_MaxSize_1, "wxAuiPaneInfo", "maxSize", 2}, // 2581
#else
  {NULL, "wxAuiPaneInfo", "maxSize", 0}, // 2581
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_MaxSize_2, "wxAuiPaneInfo", "maxSize", 3}, // 2582
#else
  {NULL, "wxAuiPaneInfo", "maxSize", 0}, // 2582
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_MaximizeButton, "wxAuiPaneInfo", "maximizeButton", 2}, // 2583
#else
  {NULL, "wxAuiPaneInfo", "maximizeButton", 0}, // 2583
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_MinSize_1, "wxAuiPaneInfo", "minSize", 2}, // 2584
#else
  {NULL, "wxAuiPaneInfo", "minSize", 0}, // 2584
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_MinSize_2, "wxAuiPaneInfo", "minSize", 3}, // 2585
#else
  {NULL, "wxAuiPaneInfo", "minSize", 0}, // 2585
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_MinimizeButton, "wxAuiPaneInfo", "minimizeButton", 2}, // 2586
#else
  {NULL, "wxAuiPaneInfo", "minimizeButton", 0}, // 2586
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Movable, "wxAuiPaneInfo", "movable", 2}, // 2587
#else
  {NULL, "wxAuiPaneInfo", "movable", 0}, // 2587
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Name, "wxAuiPaneInfo", "name", 2}, // 2588
#else
  {NULL, "wxAuiPaneInfo", "name", 0}, // 2588
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_PaneBorder, "wxAuiPaneInfo", "paneBorder", 2}, // 2589
#else
  {NULL, "wxAuiPaneInfo", "paneBorder", 0}, // 2589
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_PinButton, "wxAuiPaneInfo", "pinButton", 2}, // 2590
#else
  {NULL, "wxAuiPaneInfo", "pinButton", 0}, // 2590
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Position, "wxAuiPaneInfo", "position", 2}, // 2591
#else
  {NULL, "wxAuiPaneInfo", "position", 0}, // 2591
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Resizable, "wxAuiPaneInfo", "resizable", 2}, // 2592
#else
  {NULL, "wxAuiPaneInfo", "resizable", 0}, // 2592
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Right, "wxAuiPaneInfo", "right", 1}, // 2593
#else
  {NULL, "wxAuiPaneInfo", "right", 0}, // 2593
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_RightDockable, "wxAuiPaneInfo", "rightDockable", 2}, // 2594
#else
  {NULL, "wxAuiPaneInfo", "rightDockable", 0}, // 2594
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Row, "wxAuiPaneInfo", "row", 2}, // 2595
#else
  {NULL, "wxAuiPaneInfo", "row", 0}, // 2595
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_SafeSet, "wxAuiPaneInfo", "safeSet", 2}, // 2596
#else
  {NULL, "wxAuiPaneInfo", "safeSet", 0}, // 2596
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_SetFlag, "wxAuiPaneInfo", "setFlag", 3}, // 2597
#else
  {NULL, "wxAuiPaneInfo", "setFlag", 0}, // 2597
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Show, "wxAuiPaneInfo", "show", 2}, // 2598
#else
  {NULL, "wxAuiPaneInfo", "show", 0}, // 2598
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_ToolbarPane, "wxAuiPaneInfo", "toolbarPane", 1}, // 2599
#else
  {NULL, "wxAuiPaneInfo", "toolbarPane", 0}, // 2599
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Top, "wxAuiPaneInfo", "top", 1}, // 2600
#else
  {NULL, "wxAuiPaneInfo", "top", 0}, // 2600
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_TopDockable, "wxAuiPaneInfo", "topDockable", 2}, // 2601
#else
  {NULL, "wxAuiPaneInfo", "topDockable", 0}, // 2601
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_Window, "wxAuiPaneInfo", "window", 2}, // 2602
#else
  {NULL, "wxAuiPaneInfo", "window", 0}, // 2602
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_GetWindow, "wxAuiPaneInfo", "getWindow", 1}, // 2603
#else
  {NULL, "wxAuiPaneInfo", "getWindow", 0}, // 2603
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_GetFrame, "wxAuiPaneInfo", "getFrame", 1}, // 2604
#else
  {NULL, "wxAuiPaneInfo", "getFrame", 0}, // 2604
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_GetDirection, "wxAuiPaneInfo", "getDirection", 1}, // 2605
#else
  {NULL, "wxAuiPaneInfo", "getDirection", 0}, // 2605
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_GetLayer, "wxAuiPaneInfo", "getLayer", 1}, // 2606
#else
  {NULL, "wxAuiPaneInfo", "getLayer", 0}, // 2606
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_GetRow, "wxAuiPaneInfo", "getRow", 1}, // 2607
#else
  {NULL, "wxAuiPaneInfo", "getRow", 0}, // 2607
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_GetPosition, "wxAuiPaneInfo", "getPosition", 1}, // 2608
#else
  {NULL, "wxAuiPaneInfo", "getPosition", 0}, // 2608
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_GetFloatingPosition, "wxAuiPaneInfo", "getFloatingPosition", 1}, // 2609
#else
  {NULL, "wxAuiPaneInfo", "getFloatingPosition", 0}, // 2609
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_GetFloatingSize, "wxAuiPaneInfo", "getFloatingSize", 1}, // 2610
#else
  {NULL, "wxAuiPaneInfo", "getFloatingSize", 0}, // 2610
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiPaneInfo_destroy, "wxAuiPaneInfo", "'Destroy'", 1}, // 2611
#else
  {NULL, "wxAuiPaneInfo", "'Destroy'", 0}, // 2611
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_new_0, "wxAuiNotebook", "new", 0}, // 2612
#else
  {NULL, "wxAuiNotebook", "new", 0}, // 2612
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_new_2, "wxAuiNotebook", "new", 2}, // 2613
#else
  {NULL, "wxAuiNotebook", "new", 0}, // 2613
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_AddPage_3, "wxAuiNotebook", "addPage", 4}, // 2614
#else
  {NULL, "wxAuiNotebook", "addPage", 0}, // 2614
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_AddPage_4, "wxAuiNotebook", "addPage", 5}, // 2615
#else
  {NULL, "wxAuiNotebook", "addPage", 0}, // 2615
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_Create_2, "wxAuiNotebook", "create", 3}, // 2616
#else
  {NULL, "wxAuiNotebook", "create", 0}, // 2616
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_Create_3, "wxAuiNotebook", "create", 4}, // 2617
#else
  {NULL, "wxAuiNotebook", "create", 0}, // 2617
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_DeletePage, "wxAuiNotebook", "deletePage", 2}, // 2618
#else
  {NULL, "wxAuiNotebook", "deletePage", 0}, // 2618
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_GetArtProvider, "wxAuiNotebook", "getArtProvider", 1}, // 2619
#else
  {NULL, "wxAuiNotebook", "getArtProvider", 0}, // 2619
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_GetPage, "wxAuiNotebook", "getPage", 2}, // 2620
#else
  {NULL, "wxAuiNotebook", "getPage", 0}, // 2620
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_GetPageBitmap, "wxAuiNotebook", "getPageBitmap", 2}, // 2621
#else
  {NULL, "wxAuiNotebook", "getPageBitmap", 0}, // 2621
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_GetPageCount, "wxAuiNotebook", "getPageCount", 1}, // 2622
#else
  {NULL, "wxAuiNotebook", "getPageCount", 0}, // 2622
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_GetPageIndex, "wxAuiNotebook", "getPageIndex", 2}, // 2623
#else
  {NULL, "wxAuiNotebook", "getPageIndex", 0}, // 2623
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_GetPageText, "wxAuiNotebook", "getPageText", 2}, // 2624
#else
  {NULL, "wxAuiNotebook", "getPageText", 0}, // 2624
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_GetSelection, "wxAuiNotebook", "getSelection", 1}, // 2625
#else
  {NULL, "wxAuiNotebook", "getSelection", 0}, // 2625
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_InsertPage_4, "wxAuiNotebook", "insertPage", 5}, // 2626
#else
  {NULL, "wxAuiNotebook", "insertPage", 0}, // 2626
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_InsertPage_5, "wxAuiNotebook", "insertPage", 6}, // 2627
#else
  {NULL, "wxAuiNotebook", "insertPage", 0}, // 2627
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_RemovePage, "wxAuiNotebook", "removePage", 2}, // 2628
#else
  {NULL, "wxAuiNotebook", "removePage", 0}, // 2628
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_SetArtProvider, "wxAuiNotebook", "setArtProvider", 2}, // 2629
#else
  {NULL, "wxAuiNotebook", "setArtProvider", 0}, // 2629
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_SetFont, "wxAuiNotebook", "setFont", 2}, // 2630
#else
  {NULL, "wxAuiNotebook", "setFont", 0}, // 2630
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_SetPageBitmap, "wxAuiNotebook", "setPageBitmap", 3}, // 2631
#else
  {NULL, "wxAuiNotebook", "setPageBitmap", 0}, // 2631
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_SetPageText, "wxAuiNotebook", "setPageText", 3}, // 2632
#else
  {NULL, "wxAuiNotebook", "setPageText", 0}, // 2632
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_SetSelection, "wxAuiNotebook", "setSelection", 2}, // 2633
#else
  {NULL, "wxAuiNotebook", "setSelection", 0}, // 2633
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_SetTabCtrlHeight, "wxAuiNotebook", "setTabCtrlHeight", 2}, // 2634
#else
  {NULL, "wxAuiNotebook", "setTabCtrlHeight", 0}, // 2634
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiNotebook_SetUniformBitmapSize, "wxAuiNotebook", "setUniformBitmapSize", 2}, // 2635
#else
  {NULL, "wxAuiNotebook", "setUniformBitmapSize", 0}, // 2635
#endif // wxUSE_AUI
#if wxUSE_AUI
  {NULL, "wxAuiNotebook", "'Destroy'", 1}, // 2636 obj destructor wxAuiNotebook_destroy
#else
  {NULL, "wxAuiNotebook", "'Destroy'", 0}, // 2636
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiTabArt_SetFlags, "wxAuiTabArt", "setFlags", 2}, // 2637
#else
  {NULL, "wxAuiTabArt", "setFlags", 0}, // 2637
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiTabArt_SetMeasuringFont, "wxAuiTabArt", "setMeasuringFont", 2}, // 2638
#else
  {NULL, "wxAuiTabArt", "setMeasuringFont", 0}, // 2638
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiTabArt_SetNormalFont, "wxAuiTabArt", "setNormalFont", 2}, // 2639
#else
  {NULL, "wxAuiTabArt", "setNormalFont", 0}, // 2639
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiTabArt_SetSelectedFont, "wxAuiTabArt", "setSelectedFont", 2}, // 2640
#else
  {NULL, "wxAuiTabArt", "setSelectedFont", 0}, // 2640
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiTabArt_SetColour, "wxAuiTabArt", "setColour", 2}, // 2641
#else
  {NULL, "wxAuiTabArt", "setColour", 0}, // 2641
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiTabArt_SetActiveColour, "wxAuiTabArt", "setActiveColour", 2}, // 2642
#else
  {NULL, "wxAuiTabArt", "setActiveColour", 0}, // 2642
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiDockArt_GetColour, "wxAuiDockArt", "getColour", 2}, // 2643
#else
  {NULL, "wxAuiDockArt", "getColour", 0}, // 2643
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiDockArt_GetFont, "wxAuiDockArt", "getFont", 2}, // 2644
#else
  {NULL, "wxAuiDockArt", "getFont", 0}, // 2644
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiDockArt_GetMetric, "wxAuiDockArt", "getMetric", 2}, // 2645
#else
  {NULL, "wxAuiDockArt", "getMetric", 0}, // 2645
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiDockArt_SetColour, "wxAuiDockArt", "setColour", 3}, // 2646
#else
  {NULL, "wxAuiDockArt", "setColour", 0}, // 2646
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiDockArt_SetFont, "wxAuiDockArt", "setFont", 3}, // 2647
#else
  {NULL, "wxAuiDockArt", "setFont", 0}, // 2647
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiDockArt_SetMetric, "wxAuiDockArt", "setMetric", 3}, // 2648
#else
  {NULL, "wxAuiDockArt", "setMetric", 0}, // 2648
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiSimpleTabArt_new, "wxAuiSimpleTabArt", "new", 0}, // 2649
#else
  {NULL, "wxAuiSimpleTabArt", "new", 0}, // 2649
#endif // wxUSE_AUI
#if wxUSE_AUI
  {wxAuiSimpleTabArt_destroy, "wxAuiSimpleTabArt", "'Destroy'", 1}, // 2650
#else
  {NULL, "wxAuiSimpleTabArt", "'Destroy'", 0}, // 2650
#endif // wxUSE_AUI
  {wxMDIParentFrame_new_0, "wxMDIParentFrame", "new", 0}, // 2651
  {wxMDIParentFrame_new_4, "wxMDIParentFrame", "new", 4}, // 2652
  {NULL, "wxMDIParentFrame", "destroy", 1}, // 2653 obj destructor wxMDIParentFrame_destruct
  {wxMDIParentFrame_ActivateNext, "wxMDIParentFrame", "activateNext", 1}, // 2654
  {wxMDIParentFrame_ActivatePrevious, "wxMDIParentFrame", "activatePrevious", 1}, // 2655
  {wxMDIParentFrame_ArrangeIcons, "wxMDIParentFrame", "arrangeIcons", 1}, // 2656
  {wxMDIParentFrame_Cascade, "wxMDIParentFrame", "cascade", 1}, // 2657
  {wxMDIParentFrame_Create, "wxMDIParentFrame", "create", 5}, // 2658
  {wxMDIParentFrame_GetActiveChild, "wxMDIParentFrame", "getActiveChild", 1}, // 2659
  {wxMDIParentFrame_GetClientWindow, "wxMDIParentFrame", "getClientWindow", 1}, // 2660
  {wxMDIParentFrame_Tile, "wxMDIParentFrame", "tile", 2}, // 2661
  {wxMDIChildFrame_new_0, "wxMDIChildFrame", "new", 0}, // 2662
  {wxMDIChildFrame_new_4, "wxMDIChildFrame", "new", 4}, // 2663
  {NULL, "wxMDIChildFrame", "destroy", 1}, // 2664 obj destructor wxMDIChildFrame_destruct
  {wxMDIChildFrame_Activate, "wxMDIChildFrame", "activate", 1}, // 2665
  {wxMDIChildFrame_Create, "wxMDIChildFrame", "create", 5}, // 2666
  {wxMDIChildFrame_Maximize, "wxMDIChildFrame", "maximize", 2}, // 2667
  {wxMDIChildFrame_Restore, "wxMDIChildFrame", "restore", 1}, // 2668
  {wxMDIClientWindow_new, "wxMDIClientWindow", "new", 0}, // 2669
  {wxMDIClientWindow_CreateClient, "wxMDIClientWindow", "createClient", 3}, // 2670
  {NULL, "wxMDIClientWindow", "'Destroy'", 1}, // 2671 obj destructor wxMDIClientWindow_destroy
  {wxLayoutAlgorithm_new, "wxLayoutAlgorithm", "new", 0}, // 2672
  {NULL, "wxLayoutAlgorithm", "destroy", 1}, // 2673 obj destructor wxLayoutAlgorithm_destruct
  {wxLayoutAlgorithm_LayoutFrame, "wxLayoutAlgorithm", "layoutFrame", 3}, // 2674
  {wxLayoutAlgorithm_LayoutMDIFrame, "wxLayoutAlgorithm", "layoutMDIFrame", 3}, // 2675
  {wxLayoutAlgorithm_LayoutWindow, "wxLayoutAlgorithm", "layoutWindow", 3}, // 2676
  {wxEvent_GetId, "wxEvent", "getId", 1}, // 2677
  {wxEvent_GetSkipped, "wxEvent", "getSkipped", 1}, // 2678
  {wxEvent_GetTimestamp, "wxEvent", "getTimestamp", 1}, // 2679
  {wxEvent_IsCommandEvent, "wxEvent", "isCommandEvent", 1}, // 2680
  {wxEvent_ResumePropagation, "wxEvent", "resumePropagation", 2}, // 2681
  {wxEvent_ShouldPropagate, "wxEvent", "shouldPropagate", 1}, // 2682
  {wxEvent_Skip, "wxEvent", "skip", 2}, // 2683
  {wxEvent_StopPropagation, "wxEvent", "stopPropagation", 1}, // 2684
  {wxCommandEvent_getClientData, "wxCommandEvent", "getClientData", 1}, // 2685
  {wxCommandEvent_GetExtraLong, "wxCommandEvent", "getExtraLong", 1}, // 2686
  {wxCommandEvent_GetInt, "wxCommandEvent", "getInt", 1}, // 2687
  {wxCommandEvent_GetSelection, "wxCommandEvent", "getSelection", 1}, // 2688
  {wxCommandEvent_GetString, "wxCommandEvent", "getString", 1}, // 2689
  {wxCommandEvent_IsChecked, "wxCommandEvent", "isChecked", 1}, // 2690
  {wxCommandEvent_IsSelection, "wxCommandEvent", "isSelection", 1}, // 2691
  {wxCommandEvent_SetInt, "wxCommandEvent", "setInt", 2}, // 2692
  {wxCommandEvent_SetString, "wxCommandEvent", "setString", 2}, // 2693
  {wxScrollEvent_GetOrientation, "wxScrollEvent", "getOrientation", 1}, // 2694
  {wxScrollEvent_GetPosition, "wxScrollEvent", "getPosition", 1}, // 2695
  {wxScrollWinEvent_GetOrientation, "wxScrollWinEvent", "getOrientation", 1}, // 2696
  {wxScrollWinEvent_GetPosition, "wxScrollWinEvent", "getPosition", 1}, // 2697
  {wxMouseEvent_AltDown, "wxMouseEvent", "altDown", 1}, // 2698
  {wxMouseEvent_Button, "wxMouseEvent", "button", 2}, // 2699
  {wxMouseEvent_ButtonDClick, "wxMouseEvent", "buttonDClick", 2}, // 2700
  {wxMouseEvent_ButtonDown, "wxMouseEvent", "buttonDown", 2}, // 2701
  {wxMouseEvent_ButtonUp, "wxMouseEvent", "buttonUp", 2}, // 2702
  {wxMouseEvent_CmdDown, "wxMouseEvent", "cmdDown", 1}, // 2703
  {wxMouseEvent_ControlDown, "wxMouseEvent", "controlDown", 1}, // 2704
  {wxMouseEvent_Dragging, "wxMouseEvent", "dragging", 1}, // 2705
  {wxMouseEvent_Entering, "wxMouseEvent", "entering", 1}, // 2706
  {wxMouseEvent_GetButton, "wxMouseEvent", "getButton", 1}, // 2707
  {wxMouseEvent_GetPosition, "wxMouseEvent", "getPosition", 1}, // 2708
  {NULL, "", "", 0}, // 2709
  {wxMouseEvent_GetLogicalPosition, "wxMouseEvent", "getLogicalPosition", 2}, // 2710
  {wxMouseEvent_GetLinesPerAction, "wxMouseEvent", "getLinesPerAction", 1}, // 2711
  {wxMouseEvent_GetWheelRotation, "wxMouseEvent", "getWheelRotation", 1}, // 2712
  {wxMouseEvent_GetWheelDelta, "wxMouseEvent", "getWheelDelta", 1}, // 2713
  {wxMouseEvent_GetX, "wxMouseEvent", "getX", 1}, // 2714
  {wxMouseEvent_GetY, "wxMouseEvent", "getY", 1}, // 2715
  {wxMouseEvent_IsButton, "wxMouseEvent", "isButton", 1}, // 2716
  {wxMouseEvent_IsPageScroll, "wxMouseEvent", "isPageScroll", 1}, // 2717
  {wxMouseEvent_Leaving, "wxMouseEvent", "leaving", 1}, // 2718
  {wxMouseEvent_LeftDClick, "wxMouseEvent", "leftDClick", 1}, // 2719
  {wxMouseEvent_LeftDown, "wxMouseEvent", "leftDown", 1}, // 2720
  {wxMouseEvent_LeftIsDown, "wxMouseEvent", "leftIsDown", 1}, // 2721
  {wxMouseEvent_LeftUp, "wxMouseEvent", "leftUp", 1}, // 2722
  {wxMouseEvent_MetaDown, "wxMouseEvent", "metaDown", 1}, // 2723
  {wxMouseEvent_MiddleDClick, "wxMouseEvent", "middleDClick", 1}, // 2724
  {wxMouseEvent_MiddleDown, "wxMouseEvent", "middleDown", 1}, // 2725
  {wxMouseEvent_MiddleIsDown, "wxMouseEvent", "middleIsDown", 1}, // 2726
  {wxMouseEvent_MiddleUp, "wxMouseEvent", "middleUp", 1}, // 2727
  {wxMouseEvent_Moving, "wxMouseEvent", "moving", 1}, // 2728
  {wxMouseEvent_RightDClick, "wxMouseEvent", "rightDClick", 1}, // 2729
  {wxMouseEvent_RightDown, "wxMouseEvent", "rightDown", 1}, // 2730
  {wxMouseEvent_RightIsDown, "wxMouseEvent", "rightIsDown", 1}, // 2731
  {wxMouseEvent_RightUp, "wxMouseEvent", "rightUp", 1}, // 2732
  {wxMouseEvent_ShiftDown, "wxMouseEvent", "shiftDown", 1}, // 2733
  {wxMouseEvent_GetWheelAxis, "wxMouseEvent", "getWheelAxis", 1}, // 2734
  {wxMouseEvent_Aux1DClick, "wxMouseEvent", "aux1DClick", 1}, // 2735
  {wxMouseEvent_Aux1Down, "wxMouseEvent", "aux1Down", 1}, // 2736
  {wxMouseEvent_Aux1Up, "wxMouseEvent", "aux1Up", 1}, // 2737
  {wxMouseEvent_Aux2DClick, "wxMouseEvent", "aux2DClick", 1}, // 2738
  {wxMouseEvent_Aux2Down, "wxMouseEvent", "aux2Down", 1}, // 2739
  {wxMouseEvent_Aux2Up, "wxMouseEvent", "aux2Up", 1}, // 2740
  {wxSetCursorEvent_GetCursor, "wxSetCursorEvent", "getCursor", 1}, // 2741
  {wxSetCursorEvent_GetX, "wxSetCursorEvent", "getX", 1}, // 2742
  {wxSetCursorEvent_GetY, "wxSetCursorEvent", "getY", 1}, // 2743
  {wxSetCursorEvent_HasCursor, "wxSetCursorEvent", "hasCursor", 1}, // 2744
  {wxSetCursorEvent_SetCursor, "wxSetCursorEvent", "setCursor", 2}, // 2745
  {wxKeyEvent_AltDown, "wxKeyEvent", "altDown", 1}, // 2746
  {wxKeyEvent_CmdDown, "wxKeyEvent", "cmdDown", 1}, // 2747
  {wxKeyEvent_ControlDown, "wxKeyEvent", "controlDown", 1}, // 2748
  {wxKeyEvent_GetKeyCode, "wxKeyEvent", "getKeyCode", 1}, // 2749
  {wxKeyEvent_GetModifiers, "wxKeyEvent", "getModifiers", 1}, // 2750
  {wxKeyEvent_GetPosition, "wxKeyEvent", "getPosition", 1}, // 2751
  {NULL, "", "", 0}, // 2752
  {wxKeyEvent_GetRawKeyCode, "wxKeyEvent", "getRawKeyCode", 1}, // 2753
  {wxKeyEvent_GetRawKeyFlags, "wxKeyEvent", "getRawKeyFlags", 1}, // 2754
  {wxKeyEvent_GetUnicodeKey, "wxKeyEvent", "getUnicodeKey", 1}, // 2755
  {wxKeyEvent_GetX, "wxKeyEvent", "getX", 1}, // 2756
  {wxKeyEvent_GetY, "wxKeyEvent", "getY", 1}, // 2757
  {wxKeyEvent_HasModifiers, "wxKeyEvent", "hasModifiers", 1}, // 2758
  {wxKeyEvent_MetaDown, "wxKeyEvent", "metaDown", 1}, // 2759
  {wxKeyEvent_ShiftDown, "wxKeyEvent", "shiftDown", 1}, // 2760
  {wxSizeEvent_GetSize, "wxSizeEvent", "getSize", 1}, // 2761
  {wxSizeEvent_GetRect, "wxSizeEvent", "getRect", 1}, // 2762
  {wxMoveEvent_GetPosition, "wxMoveEvent", "getPosition", 1}, // 2763
  {wxMoveEvent_GetRect, "wxMoveEvent", "getRect", 1}, // 2764
  {wxEraseEvent_GetDC, "wxEraseEvent", "getDC", 1}, // 2765
  {wxFocusEvent_GetWindow, "wxFocusEvent", "getWindow", 1}, // 2766
  {wxChildFocusEvent_GetWindow, "wxChildFocusEvent", "getWindow", 1}, // 2767
  {wxMenuEvent_GetMenu, "wxMenuEvent", "getMenu", 1}, // 2768
  {wxMenuEvent_GetMenuId, "wxMenuEvent", "getMenuId", 1}, // 2769
  {wxMenuEvent_IsPopup, "wxMenuEvent", "isPopup", 1}, // 2770
  {wxCloseEvent_CanVeto, "wxCloseEvent", "canVeto", 1}, // 2771
  {wxCloseEvent_GetLoggingOff, "wxCloseEvent", "getLoggingOff", 1}, // 2772
  {wxCloseEvent_SetCanVeto, "wxCloseEvent", "setCanVeto", 2}, // 2773
  {wxCloseEvent_SetLoggingOff, "wxCloseEvent", "setLoggingOff", 2}, // 2774
  {wxCloseEvent_Veto, "wxCloseEvent", "veto", 2}, // 2775
  {wxShowEvent_SetShow, "wxShowEvent", "setShow", 2}, // 2776
  {wxShowEvent_IsShown, "wxShowEvent", "isShown", 1}, // 2777
  {wxIconizeEvent_IsIconized, "wxIconizeEvent", "isIconized", 1}, // 2778
  {wxJoystickEvent_ButtonDown, "wxJoystickEvent", "buttonDown", 2}, // 2779
  {wxJoystickEvent_ButtonIsDown, "wxJoystickEvent", "buttonIsDown", 2}, // 2780
  {wxJoystickEvent_ButtonUp, "wxJoystickEvent", "buttonUp", 2}, // 2781
  {wxJoystickEvent_GetButtonChange, "wxJoystickEvent", "getButtonChange", 1}, // 2782
  {wxJoystickEvent_GetButtonState, "wxJoystickEvent", "getButtonState", 1}, // 2783
  {wxJoystickEvent_GetJoystick, "wxJoystickEvent", "getJoystick", 1}, // 2784
  {wxJoystickEvent_GetPosition, "wxJoystickEvent", "getPosition", 1}, // 2785
  {wxJoystickEvent_GetZPosition, "wxJoystickEvent", "getZPosition", 1}, // 2786
  {wxJoystickEvent_IsButton, "wxJoystickEvent", "isButton", 1}, // 2787
  {wxJoystickEvent_IsMove, "wxJoystickEvent", "isMove", 1}, // 2788
  {wxJoystickEvent_IsZMove, "wxJoystickEvent", "isZMove", 1}, // 2789
  {wxUpdateUIEvent_CanUpdate, "wxUpdateUIEvent", "canUpdate", 1}, // 2790
  {wxUpdateUIEvent_Check, "wxUpdateUIEvent", "check", 2}, // 2791
  {wxUpdateUIEvent_Enable, "wxUpdateUIEvent", "enable", 2}, // 2792
  {wxUpdateUIEvent_Show, "wxUpdateUIEvent", "show", 2}, // 2793
  {wxUpdateUIEvent_GetChecked, "wxUpdateUIEvent", "getChecked", 1}, // 2794
  {wxUpdateUIEvent_GetEnabled, "wxUpdateUIEvent", "getEnabled", 1}, // 2795
  {wxUpdateUIEvent_GetShown, "wxUpdateUIEvent", "getShown", 1}, // 2796
  {wxUpdateUIEvent_GetSetChecked, "wxUpdateUIEvent", "getSetChecked", 1}, // 2797
  {wxUpdateUIEvent_GetSetEnabled, "wxUpdateUIEvent", "getSetEnabled", 1}, // 2798
  {wxUpdateUIEvent_GetSetShown, "wxUpdateUIEvent", "getSetShown", 1}, // 2799
  {wxUpdateUIEvent_GetSetText, "wxUpdateUIEvent", "getSetText", 1}, // 2800
  {wxUpdateUIEvent_GetText, "wxUpdateUIEvent", "getText", 1}, // 2801
  {wxUpdateUIEvent_GetMode, "wxUpdateUIEvent", "getMode", 0}, // 2802
  {wxUpdateUIEvent_GetUpdateInterval, "wxUpdateUIEvent", "getUpdateInterval", 0}, // 2803
  {wxUpdateUIEvent_ResetUpdateTime, "wxUpdateUIEvent", "resetUpdateTime", 0}, // 2804
  {wxUpdateUIEvent_SetMode, "wxUpdateUIEvent", "setMode", 1}, // 2805
  {wxUpdateUIEvent_SetText, "wxUpdateUIEvent", "setText", 2}, // 2806
  {wxUpdateUIEvent_SetUpdateInterval, "wxUpdateUIEvent", "setUpdateInterval", 1}, // 2807
  {wxMouseCaptureChangedEvent_GetCapturedWindow, "wxMouseCaptureChangedEvent", "getCapturedWindow", 1}, // 2808
  {wxPaletteChangedEvent_SetChangedWindow, "wxPaletteChangedEvent", "setChangedWindow", 2}, // 2809
  {wxPaletteChangedEvent_GetChangedWindow, "wxPaletteChangedEvent", "getChangedWindow", 1}, // 2810
  {wxQueryNewPaletteEvent_SetPaletteRealized, "wxQueryNewPaletteEvent", "setPaletteRealized", 2}, // 2811
  {wxQueryNewPaletteEvent_GetPaletteRealized, "wxQueryNewPaletteEvent", "getPaletteRealized", 1}, // 2812
  {wxNavigationKeyEvent_GetDirection, "wxNavigationKeyEvent", "getDirection", 1}, // 2813
  {wxNavigationKeyEvent_SetDirection, "wxNavigationKeyEvent", "setDirection", 2}, // 2814
  {wxNavigationKeyEvent_IsWindowChange, "wxNavigationKeyEvent", "isWindowChange", 1}, // 2815
  {wxNavigationKeyEvent_SetWindowChange, "wxNavigationKeyEvent", "setWindowChange", 2}, // 2816
  {wxNavigationKeyEvent_IsFromTab, "wxNavigationKeyEvent", "isFromTab", 1}, // 2817
  {wxNavigationKeyEvent_SetFromTab, "wxNavigationKeyEvent", "setFromTab", 2}, // 2818
  {wxNavigationKeyEvent_GetCurrentFocus, "wxNavigationKeyEvent", "getCurrentFocus", 1}, // 2819
  {wxNavigationKeyEvent_SetCurrentFocus, "wxNavigationKeyEvent", "setCurrentFocus", 2}, // 2820
  {wxHelpEvent_GetOrigin, "wxHelpEvent", "getOrigin", 1}, // 2821
  {wxHelpEvent_GetPosition, "wxHelpEvent", "getPosition", 1}, // 2822
  {wxHelpEvent_SetOrigin, "wxHelpEvent", "setOrigin", 2}, // 2823
  {wxHelpEvent_SetPosition, "wxHelpEvent", "setPosition", 2}, // 2824
  {wxContextMenuEvent_GetPosition, "wxContextMenuEvent", "getPosition", 1}, // 2825
  {wxContextMenuEvent_SetPosition, "wxContextMenuEvent", "setPosition", 2}, // 2826
  {wxIdleEvent_GetMode, "wxIdleEvent", "getMode", 0}, // 2827
  {wxIdleEvent_RequestMore, "wxIdleEvent", "requestMore", 2}, // 2828
  {wxIdleEvent_MoreRequested, "wxIdleEvent", "moreRequested", 1}, // 2829
  {wxIdleEvent_SetMode, "wxIdleEvent", "setMode", 1}, // 2830
  {wxGridEvent_AltDown, "wxGridEvent", "altDown", 1}, // 2831
  {wxGridEvent_ControlDown, "wxGridEvent", "controlDown", 1}, // 2832
  {wxGridEvent_GetCol, "wxGridEvent", "getCol", 1}, // 2833
  {wxGridEvent_GetPosition, "wxGridEvent", "getPosition", 1}, // 2834
  {wxGridEvent_GetRow, "wxGridEvent", "getRow", 1}, // 2835
  {wxGridEvent_MetaDown, "wxGridEvent", "metaDown", 1}, // 2836
  {wxGridEvent_Selecting, "wxGridEvent", "selecting", 1}, // 2837
  {wxGridEvent_ShiftDown, "wxGridEvent", "shiftDown", 1}, // 2838
  {wxNotifyEvent_Allow, "wxNotifyEvent", "allow", 1}, // 2839
  {wxNotifyEvent_IsAllowed, "wxNotifyEvent", "isAllowed", 1}, // 2840
  {wxNotifyEvent_Veto, "wxNotifyEvent", "veto", 1}, // 2841
  {wxSashEvent_GetEdge, "wxSashEvent", "getEdge", 1}, // 2842
  {wxSashEvent_GetDragRect, "wxSashEvent", "getDragRect", 1}, // 2843
  {wxSashEvent_GetDragStatus, "wxSashEvent", "getDragStatus", 1}, // 2844
  {wxListEvent_GetCacheFrom, "wxListEvent", "getCacheFrom", 1}, // 2845
  {wxListEvent_GetCacheTo, "wxListEvent", "getCacheTo", 1}, // 2846
  {wxListEvent_GetKeyCode, "wxListEvent", "getKeyCode", 1}, // 2847
  {wxListEvent_GetIndex, "wxListEvent", "getIndex", 1}, // 2848
  {wxListEvent_GetColumn, "wxListEvent", "getColumn", 1}, // 2849
  {wxListEvent_GetPoint, "wxListEvent", "getPoint", 1}, // 2850
  {wxListEvent_GetLabel, "wxListEvent", "getLabel", 1}, // 2851
  {wxListEvent_GetText, "wxListEvent", "getText", 1}, // 2852
  {wxListEvent_GetImage, "wxListEvent", "getImage", 1}, // 2853
  {wxListEvent_GetData, "wxListEvent", "getData", 1}, // 2854
  {wxListEvent_GetMask, "wxListEvent", "getMask", 1}, // 2855
  {wxListEvent_GetItem, "wxListEvent", "getItem", 1}, // 2856
  {wxListEvent_IsEditCancelled, "wxListEvent", "isEditCancelled", 1}, // 2857
  {wxDateEvent_GetDate, "wxDateEvent", "getDate", 1}, // 2858
  {wxCalendarEvent_GetWeekDay, "wxCalendarEvent", "getWeekDay", 1}, // 2859
  {wxCalendarEvent_GetDate, "wxCalendarEvent", "getDate", 1}, // 2860
  {wxFileDirPickerEvent_GetPath, "wxFileDirPickerEvent", "getPath", 1}, // 2861
  {wxColourPickerEvent_GetColour, "wxColourPickerEvent", "getColour", 1}, // 2862
  {wxFontPickerEvent_GetFont, "wxFontPickerEvent", "getFont", 1}, // 2863
  {wxStyledTextEvent_GetPosition, "wxStyledTextEvent", "getPosition", 1}, // 2864
  {wxStyledTextEvent_GetKey, "wxStyledTextEvent", "getKey", 1}, // 2865
  {wxStyledTextEvent_GetModifiers, "wxStyledTextEvent", "getModifiers", 1}, // 2866
  {wxStyledTextEvent_GetModificationType, "wxStyledTextEvent", "getModificationType", 1}, // 2867
  {wxStyledTextEvent_GetText, "wxStyledTextEvent", "getText", 1}, // 2868
  {wxStyledTextEvent_GetLength, "wxStyledTextEvent", "getLength", 1}, // 2869
  {wxStyledTextEvent_GetLinesAdded, "wxStyledTextEvent", "getLinesAdded", 1}, // 2870
  {wxStyledTextEvent_GetLine, "wxStyledTextEvent", "getLine", 1}, // 2871
  {wxStyledTextEvent_GetFoldLevelNow, "wxStyledTextEvent", "getFoldLevelNow", 1}, // 2872
  {wxStyledTextEvent_GetFoldLevelPrev, "wxStyledTextEvent", "getFoldLevelPrev", 1}, // 2873
  {wxStyledTextEvent_GetMargin, "wxStyledTextEvent", "getMargin", 1}, // 2874
  {wxStyledTextEvent_GetMessage, "wxStyledTextEvent", "getMessage", 1}, // 2875
  {wxStyledTextEvent_GetWParam, "wxStyledTextEvent", "getWParam", 1}, // 2876
  {wxStyledTextEvent_GetLParam, "wxStyledTextEvent", "getLParam", 1}, // 2877
  {wxStyledTextEvent_GetListType, "wxStyledTextEvent", "getListType", 1}, // 2878
  {wxStyledTextEvent_GetX, "wxStyledTextEvent", "getX", 1}, // 2879
  {wxStyledTextEvent_GetY, "wxStyledTextEvent", "getY", 1}, // 2880
  {wxStyledTextEvent_GetDragText, "wxStyledTextEvent", "getDragText", 1}, // 2881
  {wxStyledTextEvent_GetDragAllowMove, "wxStyledTextEvent", "getDragAllowMove", 1}, // 2882
  {wxStyledTextEvent_GetDragResult, "wxStyledTextEvent", "getDragResult", 1}, // 2883
  {wxStyledTextEvent_GetShift, "wxStyledTextEvent", "getShift", 1}, // 2884
  {wxStyledTextEvent_GetControl, "wxStyledTextEvent", "getControl", 1}, // 2885
  {wxStyledTextEvent_GetAlt, "wxStyledTextEvent", "getAlt", 1}, // 2886
  {utils_wxGetKeyState, "utils", "getKeyState", 1}, // 2887
  {utils_wxGetMousePosition, "utils", "getMousePosition", 0}, // 2888
  {utils_wxGetMouseState, "utils", "getMouseState", 0}, // 2889
  {utils_wxSetDetectableAutoRepeat, "utils", "setDetectableAutoRepeat", 1}, // 2890
  {utils_wxBell, "utils", "bell", 0}, // 2891
  {utils_wxFindMenuItemId, "utils", "findMenuItemId", 3}, // 2892
  {utils_wxFindWindowAtPoint, "utils", "findWindowAtPoint", 1}, // 2893
  {utils_wxBeginBusyCursor, "utils", "beginBusyCursor", 1}, // 2894
  {utils_wxEndBusyCursor, "utils", "endBusyCursor", 0}, // 2895
  {utils_wxIsBusy, "utils", "isBusy", 0}, // 2896
  {utils_wxShutdown, "utils", "shutdown", 1}, // 2897
  {utils_wxShell, "utils", "shell", 1}, // 2898
  {utils_wxLaunchDefaultBrowser, "utils", "launchDefaultBrowser", 2}, // 2899
  {utils_wxGetEmailAddress, "utils", "getEmailAddress", 0}, // 2900
  {utils_wxGetUserId, "utils", "getUserId", 0}, // 2901
  {utils_wxGetHomeDir, "utils", "getHomeDir", 0}, // 2902
  {utils_wxNewId, "utils", "newId", 0}, // 2903
  {utils_wxRegisterId, "utils", "registerId", 1}, // 2904
  {utils_wxGetCurrentId, "utils", "getCurrentId", 0}, // 2905
  {utils_wxGetOsDescription, "utils", "getOsDescription", 0}, // 2906
  {utils_wxIsPlatformLittleEndian, "utils", "isPlatformLittleEndian", 0}, // 2907
  {utils_wxIsPlatform64Bit, "utils", "isPlatform64Bit", 0}, // 2908
  {gdicmn_wxDisplaySize, "gdicmn", "displaySize", 0}, // 2909
  {gdicmn_wxSetCursor, "gdicmn", "setCursor", 1}, // 2910
  {wxPrintout_new, "wxPrintout", "new", 3}, // 2911
  {NULL, "wxPrintout", "destroy", 1}, // 2912 obj destructor wxPrintout_destruct
  {wxPrintout_GetDC, "wxPrintout", "getDC", 1}, // 2913
  {wxPrintout_GetPageSizeMM, "wxPrintout", "getPageSizeMM", 1}, // 2914
  {wxPrintout_GetPageSizePixels, "wxPrintout", "getPageSizePixels", 1}, // 2915
  {wxPrintout_GetPaperRectPixels, "wxPrintout", "getPaperRectPixels", 1}, // 2916
  {wxPrintout_GetPPIPrinter, "wxPrintout", "getPPIPrinter", 1}, // 2917
  {wxPrintout_GetPPIScreen, "wxPrintout", "getPPIScreen", 1}, // 2918
  {wxPrintout_GetTitle, "wxPrintout", "getTitle", 1}, // 2919
  {wxPrintout_IsPreview, "wxPrintout", "isPreview", 1}, // 2920
  {wxPrintout_FitThisSizeToPaper, "wxPrintout", "fitThisSizeToPaper", 2}, // 2921
  {wxPrintout_FitThisSizeToPage, "wxPrintout", "fitThisSizeToPage", 2}, // 2922
  {wxPrintout_FitThisSizeToPageMargins, "wxPrintout", "fitThisSizeToPageMargins", 3}, // 2923
  {wxPrintout_MapScreenSizeToPaper, "wxPrintout", "mapScreenSizeToPaper", 1}, // 2924
  {wxPrintout_MapScreenSizeToPage, "wxPrintout", "mapScreenSizeToPage", 1}, // 2925
  {wxPrintout_MapScreenSizeToPageMargins, "wxPrintout", "mapScreenSizeToPageMargins", 2}, // 2926
  {wxPrintout_MapScreenSizeToDevice, "wxPrintout", "mapScreenSizeToDevice", 1}, // 2927
  {wxPrintout_GetLogicalPaperRect, "wxPrintout", "getLogicalPaperRect", 1}, // 2928
  {wxPrintout_GetLogicalPageRect, "wxPrintout", "getLogicalPageRect", 1}, // 2929
  {wxPrintout_GetLogicalPageMarginsRect, "wxPrintout", "getLogicalPageMarginsRect", 2}, // 2930
  {wxPrintout_SetLogicalOrigin, "wxPrintout", "setLogicalOrigin", 3}, // 2931
  {wxPrintout_OffsetLogicalOrigin, "wxPrintout", "offsetLogicalOrigin", 3}, // 2932
  {wxStyledTextCtrl_new_2, "wxStyledTextCtrl", "new", 2}, // 2933
  {wxStyledTextCtrl_new_0, "wxStyledTextCtrl", "new", 0}, // 2934
  {NULL, "wxStyledTextCtrl", "destroy", 1}, // 2935 obj destructor wxStyledTextCtrl_destruct
  {wxStyledTextCtrl_Create, "wxStyledTextCtrl", "create", 3}, // 2936
  {wxStyledTextCtrl_AddText, "wxStyledTextCtrl", "addText", 2}, // 2937
  {wxStyledTextCtrl_InsertText, "wxStyledTextCtrl", "insertText", 3}, // 2938
  {wxStyledTextCtrl_ClearAll, "wxStyledTextCtrl", "clearAll", 1}, // 2939
  {wxStyledTextCtrl_ClearDocumentStyle, "wxStyledTextCtrl", "clearDocumentStyle", 1}, // 2940
  {wxStyledTextCtrl_GetLength, "wxStyledTextCtrl", "getLength", 1}, // 2941
  {wxStyledTextCtrl_GetCharAt, "wxStyledTextCtrl", "getCharAt", 2}, // 2942
  {wxStyledTextCtrl_GetCurrentPos, "wxStyledTextCtrl", "getCurrentPos", 1}, // 2943
  {wxStyledTextCtrl_GetAnchor, "wxStyledTextCtrl", "getAnchor", 1}, // 2944
  {wxStyledTextCtrl_GetStyleAt, "wxStyledTextCtrl", "getStyleAt", 2}, // 2945
  {wxStyledTextCtrl_Redo, "wxStyledTextCtrl", "redo", 1}, // 2946
  {wxStyledTextCtrl_SetUndoCollection, "wxStyledTextCtrl", "setUndoCollection", 2}, // 2947
  {wxStyledTextCtrl_SelectAll, "wxStyledTextCtrl", "selectAll", 1}, // 2948
  {wxStyledTextCtrl_SetSavePoint, "wxStyledTextCtrl", "setSavePoint", 1}, // 2949
  {wxStyledTextCtrl_CanRedo, "wxStyledTextCtrl", "canRedo", 1}, // 2950
  {wxStyledTextCtrl_MarkerLineFromHandle, "wxStyledTextCtrl", "markerLineFromHandle", 2}, // 2951
  {wxStyledTextCtrl_MarkerDeleteHandle, "wxStyledTextCtrl", "markerDeleteHandle", 2}, // 2952
  {wxStyledTextCtrl_GetUndoCollection, "wxStyledTextCtrl", "getUndoCollection", 1}, // 2953
  {wxStyledTextCtrl_GetViewWhiteSpace, "wxStyledTextCtrl", "getViewWhiteSpace", 1}, // 2954
  {wxStyledTextCtrl_SetViewWhiteSpace, "wxStyledTextCtrl", "setViewWhiteSpace", 2}, // 2955
  {wxStyledTextCtrl_PositionFromPoint, "wxStyledTextCtrl", "positionFromPoint", 2}, // 2956
  {wxStyledTextCtrl_PositionFromPointClose, "wxStyledTextCtrl", "positionFromPointClose", 3}, // 2957
  {wxStyledTextCtrl_GotoLine, "wxStyledTextCtrl", "gotoLine", 2}, // 2958
  {wxStyledTextCtrl_GotoPos, "wxStyledTextCtrl", "gotoPos", 2}, // 2959
  {wxStyledTextCtrl_SetAnchor, "wxStyledTextCtrl", "setAnchor", 2}, // 2960
  {wxStyledTextCtrl_GetCurLine, "wxStyledTextCtrl", "getCurLine", 1}, // 2961
  {wxStyledTextCtrl_GetEndStyled, "wxStyledTextCtrl", "getEndStyled", 1}, // 2962
  {wxStyledTextCtrl_ConvertEOLs, "wxStyledTextCtrl", "convertEOLs", 2}, // 2963
  {wxStyledTextCtrl_GetEOLMode, "wxStyledTextCtrl", "getEOLMode", 1}, // 2964
  {wxStyledTextCtrl_SetEOLMode, "wxStyledTextCtrl", "setEOLMode", 2}, // 2965
  {wxStyledTextCtrl_StartStyling, "wxStyledTextCtrl", "startStyling", 2}, // 2966
  {wxStyledTextCtrl_SetStyling, "wxStyledTextCtrl", "setStyling", 3}, // 2967
  {wxStyledTextCtrl_GetBufferedDraw, "wxStyledTextCtrl", "getBufferedDraw", 1}, // 2968
  {wxStyledTextCtrl_SetBufferedDraw, "wxStyledTextCtrl", "setBufferedDraw", 2}, // 2969
  {wxStyledTextCtrl_SetTabWidth, "wxStyledTextCtrl", "setTabWidth", 2}, // 2970
  {wxStyledTextCtrl_GetTabWidth, "wxStyledTextCtrl", "getTabWidth", 1}, // 2971
  {wxStyledTextCtrl_SetCodePage, "wxStyledTextCtrl", "setCodePage", 2}, // 2972
  {wxStyledTextCtrl_MarkerDefine, "wxStyledTextCtrl", "markerDefine", 4}, // 2973
  {wxStyledTextCtrl_MarkerSetForeground, "wxStyledTextCtrl", "markerSetForeground", 3}, // 2974
  {wxStyledTextCtrl_MarkerSetBackground, "wxStyledTextCtrl", "markerSetBackground", 3}, // 2975
  {wxStyledTextCtrl_MarkerAdd, "wxStyledTextCtrl", "markerAdd", 3}, // 2976
  {wxStyledTextCtrl_MarkerDelete, "wxStyledTextCtrl", "markerDelete", 3}, // 2977
  {wxStyledTextCtrl_MarkerDeleteAll, "wxStyledTextCtrl", "markerDeleteAll", 2}, // 2978
  {wxStyledTextCtrl_MarkerGet, "wxStyledTextCtrl", "markerGet", 2}, // 2979
  {wxStyledTextCtrl_MarkerNext, "wxStyledTextCtrl", "markerNext", 3}, // 2980
  {wxStyledTextCtrl_MarkerPrevious, "wxStyledTextCtrl", "markerPrevious", 3}, // 2981
  {wxStyledTextCtrl_MarkerDefineBitmap, "wxStyledTextCtrl", "markerDefineBitmap", 3}, // 2982
  {wxStyledTextCtrl_MarkerAddSet, "wxStyledTextCtrl", "markerAddSet", 3}, // 2983
  {wxStyledTextCtrl_MarkerSetAlpha, "wxStyledTextCtrl", "markerSetAlpha", 3}, // 2984
  {wxStyledTextCtrl_SetMarginType, "wxStyledTextCtrl", "setMarginType", 3}, // 2985
  {wxStyledTextCtrl_GetMarginType, "wxStyledTextCtrl", "getMarginType", 2}, // 2986
  {wxStyledTextCtrl_SetMarginWidth, "wxStyledTextCtrl", "setMarginWidth", 3}, // 2987
  {wxStyledTextCtrl_GetMarginWidth, "wxStyledTextCtrl", "getMarginWidth", 2}, // 2988
  {wxStyledTextCtrl_SetMarginMask, "wxStyledTextCtrl", "setMarginMask", 3}, // 2989
  {wxStyledTextCtrl_GetMarginMask, "wxStyledTextCtrl", "getMarginMask", 2}, // 2990
  {wxStyledTextCtrl_SetMarginSensitive, "wxStyledTextCtrl", "setMarginSensitive", 3}, // 2991
  {wxStyledTextCtrl_GetMarginSensitive, "wxStyledTextCtrl", "getMarginSensitive", 2}, // 2992
  {wxStyledTextCtrl_StyleClearAll, "wxStyledTextCtrl", "styleClearAll", 1}, // 2993
  {wxStyledTextCtrl_StyleSetForeground, "wxStyledTextCtrl", "styleSetForeground", 3}, // 2994
  {wxStyledTextCtrl_StyleSetBackground, "wxStyledTextCtrl", "styleSetBackground", 3}, // 2995
  {wxStyledTextCtrl_StyleSetBold, "wxStyledTextCtrl", "styleSetBold", 3}, // 2996
  {wxStyledTextCtrl_StyleSetItalic, "wxStyledTextCtrl", "styleSetItalic", 3}, // 2997
  {wxStyledTextCtrl_StyleSetSize, "wxStyledTextCtrl", "styleSetSize", 3}, // 2998
  {wxStyledTextCtrl_StyleSetFaceName, "wxStyledTextCtrl", "styleSetFaceName", 3}, // 2999
  {wxStyledTextCtrl_StyleSetEOLFilled, "wxStyledTextCtrl", "styleSetEOLFilled", 3}, // 3000
  {wxStyledTextCtrl_StyleResetDefault, "wxStyledTextCtrl", "styleResetDefault", 1}, // 3001
  {wxStyledTextCtrl_StyleSetUnderline, "wxStyledTextCtrl", "styleSetUnderline", 3}, // 3002
  {wxStyledTextCtrl_StyleSetCase, "wxStyledTextCtrl", "styleSetCase", 3}, // 3003
  {wxStyledTextCtrl_StyleSetHotSpot, "wxStyledTextCtrl", "styleSetHotSpot", 3}, // 3004
  {wxStyledTextCtrl_SetSelForeground, "wxStyledTextCtrl", "setSelForeground", 3}, // 3005
  {wxStyledTextCtrl_SetSelBackground, "wxStyledTextCtrl", "setSelBackground", 3}, // 3006
  {wxStyledTextCtrl_GetSelAlpha, "wxStyledTextCtrl", "getSelAlpha", 1}, // 3007
  {wxStyledTextCtrl_SetSelAlpha, "wxStyledTextCtrl", "setSelAlpha", 2}, // 3008
  {wxStyledTextCtrl_SetCaretForeground, "wxStyledTextCtrl", "setCaretForeground", 2}, // 3009
  {wxStyledTextCtrl_CmdKeyAssign, "wxStyledTextCtrl", "cmdKeyAssign", 4}, // 3010
  {wxStyledTextCtrl_CmdKeyClear, "wxStyledTextCtrl", "cmdKeyClear", 3}, // 3011
  {wxStyledTextCtrl_CmdKeyClearAll, "wxStyledTextCtrl", "cmdKeyClearAll", 1}, // 3012
  {wxStyledTextCtrl_SetStyleBytes, "wxStyledTextCtrl", "setStyleBytes", 2}, // 3013
  {wxStyledTextCtrl_StyleSetVisible, "wxStyledTextCtrl", "styleSetVisible", 3}, // 3014
  {wxStyledTextCtrl_GetCaretPeriod, "wxStyledTextCtrl", "getCaretPeriod", 1}, // 3015
  {wxStyledTextCtrl_SetCaretPeriod, "wxStyledTextCtrl", "setCaretPeriod", 2}, // 3016
  {wxStyledTextCtrl_SetWordChars, "wxStyledTextCtrl", "setWordChars", 2}, // 3017
  {wxStyledTextCtrl_BeginUndoAction, "wxStyledTextCtrl", "beginUndoAction", 1}, // 3018
  {wxStyledTextCtrl_EndUndoAction, "wxStyledTextCtrl", "endUndoAction", 1}, // 3019
  {wxStyledTextCtrl_IndicatorSetStyle, "wxStyledTextCtrl", "indicatorSetStyle", 3}, // 3020
  {wxStyledTextCtrl_IndicatorGetStyle, "wxStyledTextCtrl", "indicatorGetStyle", 2}, // 3021
  {wxStyledTextCtrl_IndicatorSetForeground, "wxStyledTextCtrl", "indicatorSetForeground", 3}, // 3022
  {wxStyledTextCtrl_IndicatorGetForeground, "wxStyledTextCtrl", "indicatorGetForeground", 2}, // 3023
  {wxStyledTextCtrl_SetWhitespaceForeground, "wxStyledTextCtrl", "setWhitespaceForeground", 3}, // 3024
  {wxStyledTextCtrl_SetWhitespaceBackground, "wxStyledTextCtrl", "setWhitespaceBackground", 3}, // 3025
  {wxStyledTextCtrl_GetStyleBits, "wxStyledTextCtrl", "getStyleBits", 1}, // 3026
  {wxStyledTextCtrl_SetLineState, "wxStyledTextCtrl", "setLineState", 3}, // 3027
  {wxStyledTextCtrl_GetLineState, "wxStyledTextCtrl", "getLineState", 2}, // 3028
  {wxStyledTextCtrl_GetMaxLineState, "wxStyledTextCtrl", "getMaxLineState", 1}, // 3029
  {wxStyledTextCtrl_GetCaretLineVisible, "wxStyledTextCtrl", "getCaretLineVisible", 1}, // 3030
  {wxStyledTextCtrl_SetCaretLineVisible, "wxStyledTextCtrl", "setCaretLineVisible", 2}, // 3031
  {wxStyledTextCtrl_GetCaretLineBackground, "wxStyledTextCtrl", "getCaretLineBackground", 1}, // 3032
  {wxStyledTextCtrl_SetCaretLineBackground, "wxStyledTextCtrl", "setCaretLineBackground", 2}, // 3033
  {wxStyledTextCtrl_AutoCompShow, "wxStyledTextCtrl", "autoCompShow", 3}, // 3034
  {wxStyledTextCtrl_AutoCompCancel, "wxStyledTextCtrl", "autoCompCancel", 1}, // 3035
  {wxStyledTextCtrl_AutoCompActive, "wxStyledTextCtrl", "autoCompActive", 1}, // 3036
  {wxStyledTextCtrl_AutoCompPosStart, "wxStyledTextCtrl", "autoCompPosStart", 1}, // 3037
  {wxStyledTextCtrl_AutoCompComplete, "wxStyledTextCtrl", "autoCompComplete", 1}, // 3038
  {wxStyledTextCtrl_AutoCompStops, "wxStyledTextCtrl", "autoCompStops", 2}, // 3039
  {wxStyledTextCtrl_AutoCompSetSeparator, "wxStyledTextCtrl", "autoCompSetSeparator", 2}, // 3040
  {wxStyledTextCtrl_AutoCompGetSeparator, "wxStyledTextCtrl", "autoCompGetSeparator", 1}, // 3041
  {wxStyledTextCtrl_AutoCompSelect, "wxStyledTextCtrl", "autoCompSelect", 2}, // 3042
  {wxStyledTextCtrl_AutoCompSetCancelAtStart, "wxStyledTextCtrl", "autoCompSetCancelAtStart", 2}, // 3043
  {wxStyledTextCtrl_AutoCompGetCancelAtStart, "wxStyledTextCtrl", "autoCompGetCancelAtStart", 1}, // 3044
  {wxStyledTextCtrl_AutoCompSetFillUps, "wxStyledTextCtrl", "autoCompSetFillUps", 2}, // 3045
  {wxStyledTextCtrl_AutoCompSetChooseSingle, "wxStyledTextCtrl", "autoCompSetChooseSingle", 2}, // 3046
  {wxStyledTextCtrl_AutoCompGetChooseSingle, "wxStyledTextCtrl", "autoCompGetChooseSingle", 1}, // 3047
  {wxStyledTextCtrl_AutoCompSetIgnoreCase, "wxStyledTextCtrl", "autoCompSetIgnoreCase", 2}, // 3048
  {wxStyledTextCtrl_AutoCompGetIgnoreCase, "wxStyledTextCtrl", "autoCompGetIgnoreCase", 1}, // 3049
  {wxStyledTextCtrl_UserListShow, "wxStyledTextCtrl", "userListShow", 3}, // 3050
  {wxStyledTextCtrl_AutoCompSetAutoHide, "wxStyledTextCtrl", "autoCompSetAutoHide", 2}, // 3051
  {wxStyledTextCtrl_AutoCompGetAutoHide, "wxStyledTextCtrl", "autoCompGetAutoHide", 1}, // 3052
  {wxStyledTextCtrl_AutoCompSetDropRestOfWord, "wxStyledTextCtrl", "autoCompSetDropRestOfWord", 2}, // 3053
  {wxStyledTextCtrl_AutoCompGetDropRestOfWord, "wxStyledTextCtrl", "autoCompGetDropRestOfWord", 1}, // 3054
  {wxStyledTextCtrl_RegisterImage, "wxStyledTextCtrl", "registerImage", 3}, // 3055
  {wxStyledTextCtrl_ClearRegisteredImages, "wxStyledTextCtrl", "clearRegisteredImages", 1}, // 3056
  {wxStyledTextCtrl_AutoCompGetTypeSeparator, "wxStyledTextCtrl", "autoCompGetTypeSeparator", 1}, // 3057
  {wxStyledTextCtrl_AutoCompSetTypeSeparator, "wxStyledTextCtrl", "autoCompSetTypeSeparator", 2}, // 3058
  {wxStyledTextCtrl_AutoCompSetMaxWidth, "wxStyledTextCtrl", "autoCompSetMaxWidth", 2}, // 3059
  {wxStyledTextCtrl_AutoCompGetMaxWidth, "wxStyledTextCtrl", "autoCompGetMaxWidth", 1}, // 3060
  {wxStyledTextCtrl_AutoCompSetMaxHeight, "wxStyledTextCtrl", "autoCompSetMaxHeight", 2}, // 3061
  {wxStyledTextCtrl_AutoCompGetMaxHeight, "wxStyledTextCtrl", "autoCompGetMaxHeight", 1}, // 3062
  {wxStyledTextCtrl_SetIndent, "wxStyledTextCtrl", "setIndent", 2}, // 3063
  {wxStyledTextCtrl_GetIndent, "wxStyledTextCtrl", "getIndent", 1}, // 3064
  {wxStyledTextCtrl_SetUseTabs, "wxStyledTextCtrl", "setUseTabs", 2}, // 3065
  {wxStyledTextCtrl_GetUseTabs, "wxStyledTextCtrl", "getUseTabs", 1}, // 3066
  {wxStyledTextCtrl_SetLineIndentation, "wxStyledTextCtrl", "setLineIndentation", 3}, // 3067
  {wxStyledTextCtrl_GetLineIndentation, "wxStyledTextCtrl", "getLineIndentation", 2}, // 3068
  {wxStyledTextCtrl_GetLineIndentPosition, "wxStyledTextCtrl", "getLineIndentPosition", 2}, // 3069
  {wxStyledTextCtrl_GetColumn, "wxStyledTextCtrl", "getColumn", 2}, // 3070
  {wxStyledTextCtrl_SetUseHorizontalScrollBar, "wxStyledTextCtrl", "setUseHorizontalScrollBar", 2}, // 3071
  {wxStyledTextCtrl_GetUseHorizontalScrollBar, "wxStyledTextCtrl", "getUseHorizontalScrollBar", 1}, // 3072
  {wxStyledTextCtrl_SetIndentationGuides, "wxStyledTextCtrl", "setIndentationGuides", 2}, // 3073
  {wxStyledTextCtrl_GetIndentationGuides, "wxStyledTextCtrl", "getIndentationGuides", 1}, // 3074
  {wxStyledTextCtrl_SetHighlightGuide, "wxStyledTextCtrl", "setHighlightGuide", 2}, // 3075
  {wxStyledTextCtrl_GetHighlightGuide, "wxStyledTextCtrl", "getHighlightGuide", 1}, // 3076
  {wxStyledTextCtrl_GetLineEndPosition, "wxStyledTextCtrl", "getLineEndPosition", 2}, // 3077
  {wxStyledTextCtrl_GetCodePage, "wxStyledTextCtrl", "getCodePage", 1}, // 3078
  {wxStyledTextCtrl_GetCaretForeground, "wxStyledTextCtrl", "getCaretForeground", 1}, // 3079
  {wxStyledTextCtrl_GetReadOnly, "wxStyledTextCtrl", "getReadOnly", 1}, // 3080
  {wxStyledTextCtrl_SetCurrentPos, "wxStyledTextCtrl", "setCurrentPos", 2}, // 3081
  {wxStyledTextCtrl_SetSelectionStart, "wxStyledTextCtrl", "setSelectionStart", 2}, // 3082
  {wxStyledTextCtrl_GetSelectionStart, "wxStyledTextCtrl", "getSelectionStart", 1}, // 3083
  {wxStyledTextCtrl_SetSelectionEnd, "wxStyledTextCtrl", "setSelectionEnd", 2}, // 3084
  {wxStyledTextCtrl_GetSelectionEnd, "wxStyledTextCtrl", "getSelectionEnd", 1}, // 3085
  {wxStyledTextCtrl_SetPrintMagnification, "wxStyledTextCtrl", "setPrintMagnification", 2}, // 3086
  {wxStyledTextCtrl_GetPrintMagnification, "wxStyledTextCtrl", "getPrintMagnification", 1}, // 3087
  {wxStyledTextCtrl_SetPrintColourMode, "wxStyledTextCtrl", "setPrintColourMode", 2}, // 3088
  {wxStyledTextCtrl_GetPrintColourMode, "wxStyledTextCtrl", "getPrintColourMode", 1}, // 3089
  {wxStyledTextCtrl_FindText, "wxStyledTextCtrl", "findText", 5}, // 3090
  {wxStyledTextCtrl_FormatRange, "wxStyledTextCtrl", "formatRange", 8}, // 3091
  {wxStyledTextCtrl_GetFirstVisibleLine, "wxStyledTextCtrl", "getFirstVisibleLine", 1}, // 3092
  {wxStyledTextCtrl_GetLine, "wxStyledTextCtrl", "getLine", 2}, // 3093
  {wxStyledTextCtrl_GetLineCount, "wxStyledTextCtrl", "getLineCount", 1}, // 3094
  {wxStyledTextCtrl_SetMarginLeft, "wxStyledTextCtrl", "setMarginLeft", 2}, // 3095
  {wxStyledTextCtrl_GetMarginLeft, "wxStyledTextCtrl", "getMarginLeft", 1}, // 3096
  {wxStyledTextCtrl_SetMarginRight, "wxStyledTextCtrl", "setMarginRight", 2}, // 3097
  {wxStyledTextCtrl_GetMarginRight, "wxStyledTextCtrl", "getMarginRight", 1}, // 3098
  {wxStyledTextCtrl_GetModify, "wxStyledTextCtrl", "getModify", 1}, // 3099
  {wxStyledTextCtrl_SetSelection, "wxStyledTextCtrl", "setSelection", 3}, // 3100
  {wxStyledTextCtrl_GetSelectedText, "wxStyledTextCtrl", "getSelectedText", 1}, // 3101
  {wxStyledTextCtrl_GetTextRange, "wxStyledTextCtrl", "getTextRange", 3}, // 3102
  {wxStyledTextCtrl_HideSelection, "wxStyledTextCtrl", "hideSelection", 2}, // 3103
  {wxStyledTextCtrl_LineFromPosition, "wxStyledTextCtrl", "lineFromPosition", 2}, // 3104
  {wxStyledTextCtrl_PositionFromLine, "wxStyledTextCtrl", "positionFromLine", 2}, // 3105
  {wxStyledTextCtrl_LineScroll, "wxStyledTextCtrl", "lineScroll", 3}, // 3106
  {wxStyledTextCtrl_EnsureCaretVisible, "wxStyledTextCtrl", "ensureCaretVisible", 1}, // 3107
  {wxStyledTextCtrl_ReplaceSelection, "wxStyledTextCtrl", "replaceSelection", 2}, // 3108
  {wxStyledTextCtrl_SetReadOnly, "wxStyledTextCtrl", "setReadOnly", 2}, // 3109
  {wxStyledTextCtrl_CanPaste, "wxStyledTextCtrl", "canPaste", 1}, // 3110
  {wxStyledTextCtrl_CanUndo, "wxStyledTextCtrl", "canUndo", 1}, // 3111
  {wxStyledTextCtrl_EmptyUndoBuffer, "wxStyledTextCtrl", "emptyUndoBuffer", 1}, // 3112
  {wxStyledTextCtrl_Undo, "wxStyledTextCtrl", "undo", 1}, // 3113
  {wxStyledTextCtrl_Cut, "wxStyledTextCtrl", "cut", 1}, // 3114
  {wxStyledTextCtrl_Copy, "wxStyledTextCtrl", "copy", 1}, // 3115
  {wxStyledTextCtrl_Paste, "wxStyledTextCtrl", "paste", 1}, // 3116
  {wxStyledTextCtrl_Clear, "wxStyledTextCtrl", "clear", 1}, // 3117
  {wxStyledTextCtrl_SetText, "wxStyledTextCtrl", "setText", 2}, // 3118
  {wxStyledTextCtrl_GetText, "wxStyledTextCtrl", "getText", 1}, // 3119
  {wxStyledTextCtrl_GetTextLength, "wxStyledTextCtrl", "getTextLength", 1}, // 3120
  {wxStyledTextCtrl_GetOvertype, "wxStyledTextCtrl", "getOvertype", 1}, // 3121
  {wxStyledTextCtrl_SetCaretWidth, "wxStyledTextCtrl", "setCaretWidth", 2}, // 3122
  {wxStyledTextCtrl_GetCaretWidth, "wxStyledTextCtrl", "getCaretWidth", 1}, // 3123
  {wxStyledTextCtrl_SetTargetStart, "wxStyledTextCtrl", "setTargetStart", 2}, // 3124
  {wxStyledTextCtrl_GetTargetStart, "wxStyledTextCtrl", "getTargetStart", 1}, // 3125
  {wxStyledTextCtrl_SetTargetEnd, "wxStyledTextCtrl", "setTargetEnd", 2}, // 3126
  {wxStyledTextCtrl_GetTargetEnd, "wxStyledTextCtrl", "getTargetEnd", 1}, // 3127
  {wxStyledTextCtrl_ReplaceTarget, "wxStyledTextCtrl", "replaceTarget", 2}, // 3128
  {wxStyledTextCtrl_SearchInTarget, "wxStyledTextCtrl", "searchInTarget", 2}, // 3129
  {wxStyledTextCtrl_SetSearchFlags, "wxStyledTextCtrl", "setSearchFlags", 2}, // 3130
  {wxStyledTextCtrl_GetSearchFlags, "wxStyledTextCtrl", "getSearchFlags", 1}, // 3131
  {wxStyledTextCtrl_CallTipShow, "wxStyledTextCtrl", "callTipShow", 3}, // 3132
  {wxStyledTextCtrl_CallTipCancel, "wxStyledTextCtrl", "callTipCancel", 1}, // 3133
  {wxStyledTextCtrl_CallTipActive, "wxStyledTextCtrl", "callTipActive", 1}, // 3134
  {wxStyledTextCtrl_CallTipPosAtStart, "wxStyledTextCtrl", "callTipPosAtStart", 1}, // 3135
  {wxStyledTextCtrl_CallTipSetHighlight, "wxStyledTextCtrl", "callTipSetHighlight", 3}, // 3136
  {wxStyledTextCtrl_CallTipSetBackground, "wxStyledTextCtrl", "callTipSetBackground", 2}, // 3137
  {wxStyledTextCtrl_CallTipSetForeground, "wxStyledTextCtrl", "callTipSetForeground", 2}, // 3138
  {wxStyledTextCtrl_CallTipSetForegroundHighlight, "wxStyledTextCtrl", "callTipSetForegroundHighlight", 2}, // 3139
  {wxStyledTextCtrl_CallTipUseStyle, "wxStyledTextCtrl", "callTipUseStyle", 2}, // 3140
  {wxStyledTextCtrl_VisibleFromDocLine, "wxStyledTextCtrl", "visibleFromDocLine", 2}, // 3141
  {wxStyledTextCtrl_DocLineFromVisible, "wxStyledTextCtrl", "docLineFromVisible", 2}, // 3142
  {wxStyledTextCtrl_WrapCount, "wxStyledTextCtrl", "wrapCount", 2}, // 3143
  {wxStyledTextCtrl_SetFoldLevel, "wxStyledTextCtrl", "setFoldLevel", 3}, // 3144
  {wxStyledTextCtrl_GetFoldLevel, "wxStyledTextCtrl", "getFoldLevel", 2}, // 3145
  {wxStyledTextCtrl_GetLastChild, "wxStyledTextCtrl", "getLastChild", 3}, // 3146
  {wxStyledTextCtrl_GetFoldParent, "wxStyledTextCtrl", "getFoldParent", 2}, // 3147
  {wxStyledTextCtrl_ShowLines, "wxStyledTextCtrl", "showLines", 3}, // 3148
  {wxStyledTextCtrl_HideLines, "wxStyledTextCtrl", "hideLines", 3}, // 3149
  {wxStyledTextCtrl_GetLineVisible, "wxStyledTextCtrl", "getLineVisible", 2}, // 3150
  {wxStyledTextCtrl_SetFoldExpanded, "wxStyledTextCtrl", "setFoldExpanded", 3}, // 3151
  {wxStyledTextCtrl_GetFoldExpanded, "wxStyledTextCtrl", "getFoldExpanded", 2}, // 3152
  {wxStyledTextCtrl_ToggleFold, "wxStyledTextCtrl", "toggleFold", 2}, // 3153
  {wxStyledTextCtrl_EnsureVisible, "wxStyledTextCtrl", "ensureVisible", 2}, // 3154
  {wxStyledTextCtrl_SetFoldFlags, "wxStyledTextCtrl", "setFoldFlags", 2}, // 3155
  {wxStyledTextCtrl_EnsureVisibleEnforcePolicy, "wxStyledTextCtrl", "ensureVisibleEnforcePolicy", 2}, // 3156
  {wxStyledTextCtrl_SetTabIndents, "wxStyledTextCtrl", "setTabIndents", 2}, // 3157
  {wxStyledTextCtrl_GetTabIndents, "wxStyledTextCtrl", "getTabIndents", 1}, // 3158
  {wxStyledTextCtrl_SetBackSpaceUnIndents, "wxStyledTextCtrl", "setBackSpaceUnIndents", 2}, // 3159
  {wxStyledTextCtrl_GetBackSpaceUnIndents, "wxStyledTextCtrl", "getBackSpaceUnIndents", 1}, // 3160
  {wxStyledTextCtrl_SetMouseDwellTime, "wxStyledTextCtrl", "setMouseDwellTime", 2}, // 3161
  {wxStyledTextCtrl_GetMouseDwellTime, "wxStyledTextCtrl", "getMouseDwellTime", 1}, // 3162
  {wxStyledTextCtrl_WordStartPosition, "wxStyledTextCtrl", "wordStartPosition", 3}, // 3163
  {wxStyledTextCtrl_WordEndPosition, "wxStyledTextCtrl", "wordEndPosition", 3}, // 3164
  {wxStyledTextCtrl_SetWrapMode, "wxStyledTextCtrl", "setWrapMode", 2}, // 3165
  {wxStyledTextCtrl_GetWrapMode, "wxStyledTextCtrl", "getWrapMode", 1}, // 3166
  {wxStyledTextCtrl_SetWrapVisualFlags, "wxStyledTextCtrl", "setWrapVisualFlags", 2}, // 3167
  {wxStyledTextCtrl_GetWrapVisualFlags, "wxStyledTextCtrl", "getWrapVisualFlags", 1}, // 3168
  {wxStyledTextCtrl_SetWrapVisualFlagsLocation, "wxStyledTextCtrl", "setWrapVisualFlagsLocation", 2}, // 3169
  {wxStyledTextCtrl_GetWrapVisualFlagsLocation, "wxStyledTextCtrl", "getWrapVisualFlagsLocation", 1}, // 3170
  {wxStyledTextCtrl_SetWrapStartIndent, "wxStyledTextCtrl", "setWrapStartIndent", 2}, // 3171
  {wxStyledTextCtrl_GetWrapStartIndent, "wxStyledTextCtrl", "getWrapStartIndent", 1}, // 3172
  {wxStyledTextCtrl_SetLayoutCache, "wxStyledTextCtrl", "setLayoutCache", 2}, // 3173
  {wxStyledTextCtrl_GetLayoutCache, "wxStyledTextCtrl", "getLayoutCache", 1}, // 3174
  {wxStyledTextCtrl_SetScrollWidth, "wxStyledTextCtrl", "setScrollWidth", 2}, // 3175
  {wxStyledTextCtrl_GetScrollWidth, "wxStyledTextCtrl", "getScrollWidth", 1}, // 3176
  {wxStyledTextCtrl_TextWidth, "wxStyledTextCtrl", "textWidth", 3}, // 3177
  {wxStyledTextCtrl_GetEndAtLastLine, "wxStyledTextCtrl", "getEndAtLastLine", 1}, // 3178
  {wxStyledTextCtrl_TextHeight, "wxStyledTextCtrl", "textHeight", 2}, // 3179
  {wxStyledTextCtrl_SetUseVerticalScrollBar, "wxStyledTextCtrl", "setUseVerticalScrollBar", 2}, // 3180
  {wxStyledTextCtrl_GetUseVerticalScrollBar, "wxStyledTextCtrl", "getUseVerticalScrollBar", 1}, // 3181
  {wxStyledTextCtrl_AppendText, "wxStyledTextCtrl", "appendText", 2}, // 3182
  {wxStyledTextCtrl_GetTwoPhaseDraw, "wxStyledTextCtrl", "getTwoPhaseDraw", 1}, // 3183
  {wxStyledTextCtrl_SetTwoPhaseDraw, "wxStyledTextCtrl", "setTwoPhaseDraw", 2}, // 3184
  {wxStyledTextCtrl_TargetFromSelection, "wxStyledTextCtrl", "targetFromSelection", 1}, // 3185
  {wxStyledTextCtrl_LinesJoin, "wxStyledTextCtrl", "linesJoin", 1}, // 3186
  {wxStyledTextCtrl_LinesSplit, "wxStyledTextCtrl", "linesSplit", 2}, // 3187
  {wxStyledTextCtrl_SetFoldMarginColour, "wxStyledTextCtrl", "setFoldMarginColour", 3}, // 3188
  {wxStyledTextCtrl_SetFoldMarginHiColour, "wxStyledTextCtrl", "setFoldMarginHiColour", 3}, // 3189
  {wxStyledTextCtrl_LineDown, "wxStyledTextCtrl", "lineDown", 1}, // 3190
  {wxStyledTextCtrl_LineDownExtend, "wxStyledTextCtrl", "lineDownExtend", 1}, // 3191
  {wxStyledTextCtrl_LineUp, "wxStyledTextCtrl", "lineUp", 1}, // 3192
  {wxStyledTextCtrl_LineUpExtend, "wxStyledTextCtrl", "lineUpExtend", 1}, // 3193
  {wxStyledTextCtrl_CharLeft, "wxStyledTextCtrl", "charLeft", 1}, // 3194
  {wxStyledTextCtrl_CharLeftExtend, "wxStyledTextCtrl", "charLeftExtend", 1}, // 3195
  {wxStyledTextCtrl_CharRight, "wxStyledTextCtrl", "charRight", 1}, // 3196
  {wxStyledTextCtrl_CharRightExtend, "wxStyledTextCtrl", "charRightExtend", 1}, // 3197
  {wxStyledTextCtrl_WordLeft, "wxStyledTextCtrl", "wordLeft", 1}, // 3198
  {wxStyledTextCtrl_WordLeftExtend, "wxStyledTextCtrl", "wordLeftExtend", 1}, // 3199
  {wxStyledTextCtrl_WordRight, "wxStyledTextCtrl", "wordRight", 1}, // 3200
  {wxStyledTextCtrl_WordRightExtend, "wxStyledTextCtrl", "wordRightExtend", 1}, // 3201
  {wxStyledTextCtrl_Home, "wxStyledTextCtrl", "home", 1}, // 3202
  {wxStyledTextCtrl_HomeExtend, "wxStyledTextCtrl", "homeExtend", 1}, // 3203
  {wxStyledTextCtrl_LineEnd, "wxStyledTextCtrl", "lineEnd", 1}, // 3204
  {wxStyledTextCtrl_LineEndExtend, "wxStyledTextCtrl", "lineEndExtend", 1}, // 3205
  {wxStyledTextCtrl_DocumentStart, "wxStyledTextCtrl", "documentStart", 1}, // 3206
  {wxStyledTextCtrl_DocumentStartExtend, "wxStyledTextCtrl", "documentStartExtend", 1}, // 3207
  {wxStyledTextCtrl_DocumentEnd, "wxStyledTextCtrl", "documentEnd", 1}, // 3208
  {wxStyledTextCtrl_DocumentEndExtend, "wxStyledTextCtrl", "documentEndExtend", 1}, // 3209
  {wxStyledTextCtrl_PageUp, "wxStyledTextCtrl", "pageUp", 1}, // 3210
  {wxStyledTextCtrl_PageUpExtend, "wxStyledTextCtrl", "pageUpExtend", 1}, // 3211
  {wxStyledTextCtrl_PageDown, "wxStyledTextCtrl", "pageDown", 1}, // 3212
  {wxStyledTextCtrl_PageDownExtend, "wxStyledTextCtrl", "pageDownExtend", 1}, // 3213
  {wxStyledTextCtrl_EditToggleOvertype, "wxStyledTextCtrl", "editToggleOvertype", 1}, // 3214
  {wxStyledTextCtrl_Cancel, "wxStyledTextCtrl", "cancel", 1}, // 3215
  {wxStyledTextCtrl_DeleteBack, "wxStyledTextCtrl", "deleteBack", 1}, // 3216
  {wxStyledTextCtrl_Tab, "wxStyledTextCtrl", "tab", 1}, // 3217
  {wxStyledTextCtrl_BackTab, "wxStyledTextCtrl", "backTab", 1}, // 3218
  {wxStyledTextCtrl_NewLine, "wxStyledTextCtrl", "newLine", 1}, // 3219
  {wxStyledTextCtrl_FormFeed, "wxStyledTextCtrl", "formFeed", 1}, // 3220
  {wxStyledTextCtrl_VCHome, "wxStyledTextCtrl", "vCHome", 1}, // 3221
  {wxStyledTextCtrl_VCHomeExtend, "wxStyledTextCtrl", "vCHomeExtend", 1}, // 3222
  {wxStyledTextCtrl_ZoomIn, "wxStyledTextCtrl", "zoomIn", 1}, // 3223
  {wxStyledTextCtrl_ZoomOut, "wxStyledTextCtrl", "zoomOut", 1}, // 3224
  {wxStyledTextCtrl_DelWordLeft, "wxStyledTextCtrl", "delWordLeft", 1}, // 3225
  {wxStyledTextCtrl_DelWordRight, "wxStyledTextCtrl", "delWordRight", 1}, // 3226
  {wxStyledTextCtrl_LineCut, "wxStyledTextCtrl", "lineCut", 1}, // 3227
  {wxStyledTextCtrl_LineDelete, "wxStyledTextCtrl", "lineDelete", 1}, // 3228
  {wxStyledTextCtrl_LineTranspose, "wxStyledTextCtrl", "lineTranspose", 1}, // 3229
  {wxStyledTextCtrl_LineDuplicate, "wxStyledTextCtrl", "lineDuplicate", 1}, // 3230
  {wxStyledTextCtrl_LowerCase, "wxStyledTextCtrl", "lowerCase", 1}, // 3231
  {wxStyledTextCtrl_UpperCase, "wxStyledTextCtrl", "upperCase", 1}, // 3232
  {wxStyledTextCtrl_LineScrollDown, "wxStyledTextCtrl", "lineScrollDown", 1}, // 3233
  {wxStyledTextCtrl_LineScrollUp, "wxStyledTextCtrl", "lineScrollUp", 1}, // 3234
  {wxStyledTextCtrl_DeleteBackNotLine, "wxStyledTextCtrl", "deleteBackNotLine", 1}, // 3235
  {wxStyledTextCtrl_HomeDisplay, "wxStyledTextCtrl", "homeDisplay", 1}, // 3236
  {wxStyledTextCtrl_HomeDisplayExtend, "wxStyledTextCtrl", "homeDisplayExtend", 1}, // 3237
  {wxStyledTextCtrl_LineEndDisplay, "wxStyledTextCtrl", "lineEndDisplay", 1}, // 3238
  {wxStyledTextCtrl_LineEndDisplayExtend, "wxStyledTextCtrl", "lineEndDisplayExtend", 1}, // 3239
  {wxStyledTextCtrl_HomeWrapExtend, "wxStyledTextCtrl", "homeWrapExtend", 1}, // 3240
  {wxStyledTextCtrl_LineEndWrap, "wxStyledTextCtrl", "lineEndWrap", 1}, // 3241
  {wxStyledTextCtrl_LineEndWrapExtend, "wxStyledTextCtrl", "lineEndWrapExtend", 1}, // 3242
  {wxStyledTextCtrl_VCHomeWrap, "wxStyledTextCtrl", "vCHomeWrap", 1}, // 3243
  {wxStyledTextCtrl_VCHomeWrapExtend, "wxStyledTextCtrl", "vCHomeWrapExtend", 1}, // 3244
  {wxStyledTextCtrl_LineCopy, "wxStyledTextCtrl", "lineCopy", 1}, // 3245
  {wxStyledTextCtrl_MoveCaretInsideView, "wxStyledTextCtrl", "moveCaretInsideView", 1}, // 3246
  {wxStyledTextCtrl_LineLength, "wxStyledTextCtrl", "lineLength", 2}, // 3247
  {wxStyledTextCtrl_BraceHighlight, "wxStyledTextCtrl", "braceHighlight", 3}, // 3248
  {wxStyledTextCtrl_BraceBadLight, "wxStyledTextCtrl", "braceBadLight", 2}, // 3249
  {wxStyledTextCtrl_BraceMatch, "wxStyledTextCtrl", "braceMatch", 2}, // 3250
  {wxStyledTextCtrl_GetViewEOL, "wxStyledTextCtrl", "getViewEOL", 1}, // 3251
  {wxStyledTextCtrl_SetViewEOL, "wxStyledTextCtrl", "setViewEOL", 2}, // 3252
  {wxStyledTextCtrl_SetModEventMask, "wxStyledTextCtrl", "setModEventMask", 2}, // 3253
  {wxStyledTextCtrl_GetEdgeColumn, "wxStyledTextCtrl", "getEdgeColumn", 1}, // 3254
  {wxStyledTextCtrl_SetEdgeColumn, "wxStyledTextCtrl", "setEdgeColumn", 2}, // 3255
  {wxStyledTextCtrl_SetEdgeMode, "wxStyledTextCtrl", "setEdgeMode", 2}, // 3256
  {wxStyledTextCtrl_GetEdgeMode, "wxStyledTextCtrl", "getEdgeMode", 1}, // 3257
  {wxStyledTextCtrl_GetEdgeColour, "wxStyledTextCtrl", "getEdgeColour", 1}, // 3258
  {wxStyledTextCtrl_SetEdgeColour, "wxStyledTextCtrl", "setEdgeColour", 2}, // 3259
  {wxStyledTextCtrl_SearchAnchor, "wxStyledTextCtrl", "searchAnchor", 1}, // 3260
  {wxStyledTextCtrl_SearchNext, "wxStyledTextCtrl", "searchNext", 3}, // 3261
  {wxStyledTextCtrl_SearchPrev, "wxStyledTextCtrl", "searchPrev", 3}, // 3262
  {wxStyledTextCtrl_LinesOnScreen, "wxStyledTextCtrl", "linesOnScreen", 1}, // 3263
  {wxStyledTextCtrl_UsePopUp, "wxStyledTextCtrl", "usePopUp", 2}, // 3264
  {wxStyledTextCtrl_SelectionIsRectangle, "wxStyledTextCtrl", "selectionIsRectangle", 1}, // 3265
  {wxStyledTextCtrl_SetZoom, "wxStyledTextCtrl", "setZoom", 2}, // 3266
  {wxStyledTextCtrl_GetZoom, "wxStyledTextCtrl", "getZoom", 1}, // 3267
  {wxStyledTextCtrl_GetModEventMask, "wxStyledTextCtrl", "getModEventMask", 1}, // 3268
  {wxStyledTextCtrl_SetSTCFocus, "wxStyledTextCtrl", "setSTCFocus", 2}, // 3269
  {wxStyledTextCtrl_GetSTCFocus, "wxStyledTextCtrl", "getSTCFocus", 1}, // 3270
  {wxStyledTextCtrl_SetStatus, "wxStyledTextCtrl", "setStatus", 2}, // 3271
  {wxStyledTextCtrl_GetStatus, "wxStyledTextCtrl", "getStatus", 1}, // 3272
  {wxStyledTextCtrl_SetMouseDownCaptures, "wxStyledTextCtrl", "setMouseDownCaptures", 2}, // 3273
  {wxStyledTextCtrl_GetMouseDownCaptures, "wxStyledTextCtrl", "getMouseDownCaptures", 1}, // 3274
  {wxStyledTextCtrl_SetSTCCursor, "wxStyledTextCtrl", "setSTCCursor", 2}, // 3275
  {wxStyledTextCtrl_GetSTCCursor, "wxStyledTextCtrl", "getSTCCursor", 1}, // 3276
  {wxStyledTextCtrl_SetControlCharSymbol, "wxStyledTextCtrl", "setControlCharSymbol", 2}, // 3277
  {wxStyledTextCtrl_GetControlCharSymbol, "wxStyledTextCtrl", "getControlCharSymbol", 1}, // 3278
  {wxStyledTextCtrl_WordPartLeft, "wxStyledTextCtrl", "wordPartLeft", 1}, // 3279
  {wxStyledTextCtrl_WordPartLeftExtend, "wxStyledTextCtrl", "wordPartLeftExtend", 1}, // 3280
  {wxStyledTextCtrl_WordPartRight, "wxStyledTextCtrl", "wordPartRight", 1}, // 3281
  {wxStyledTextCtrl_WordPartRightExtend, "wxStyledTextCtrl", "wordPartRightExtend", 1}, // 3282
  {wxStyledTextCtrl_SetVisiblePolicy, "wxStyledTextCtrl", "setVisiblePolicy", 3}, // 3283
  {wxStyledTextCtrl_DelLineLeft, "wxStyledTextCtrl", "delLineLeft", 1}, // 3284
  {wxStyledTextCtrl_DelLineRight, "wxStyledTextCtrl", "delLineRight", 1}, // 3285
  {wxStyledTextCtrl_GetXOffset, "wxStyledTextCtrl", "getXOffset", 1}, // 3286
  {wxStyledTextCtrl_ChooseCaretX, "wxStyledTextCtrl", "chooseCaretX", 1}, // 3287
  {wxStyledTextCtrl_SetXCaretPolicy, "wxStyledTextCtrl", "setXCaretPolicy", 3}, // 3288
  {wxStyledTextCtrl_SetYCaretPolicy, "wxStyledTextCtrl", "setYCaretPolicy", 3}, // 3289
  {wxStyledTextCtrl_GetPrintWrapMode, "wxStyledTextCtrl", "getPrintWrapMode", 1}, // 3290
  {wxStyledTextCtrl_SetHotspotActiveForeground, "wxStyledTextCtrl", "setHotspotActiveForeground", 3}, // 3291
  {wxStyledTextCtrl_SetHotspotActiveBackground, "wxStyledTextCtrl", "setHotspotActiveBackground", 3}, // 3292
  {wxStyledTextCtrl_SetHotspotActiveUnderline, "wxStyledTextCtrl", "setHotspotActiveUnderline", 2}, // 3293
  {wxStyledTextCtrl_SetHotspotSingleLine, "wxStyledTextCtrl", "setHotspotSingleLine", 2}, // 3294
  {wxStyledTextCtrl_ParaDownExtend, "wxStyledTextCtrl", "paraDownExtend", 1}, // 3295
  {wxStyledTextCtrl_ParaUp, "wxStyledTextCtrl", "paraUp", 1}, // 3296
  {wxStyledTextCtrl_ParaUpExtend, "wxStyledTextCtrl", "paraUpExtend", 1}, // 3297
  {wxStyledTextCtrl_PositionBefore, "wxStyledTextCtrl", "positionBefore", 2}, // 3298
  {wxStyledTextCtrl_PositionAfter, "wxStyledTextCtrl", "positionAfter", 2}, // 3299
  {wxStyledTextCtrl_CopyRange, "wxStyledTextCtrl", "copyRange", 3}, // 3300
  {wxStyledTextCtrl_CopyText, "wxStyledTextCtrl", "copyText", 3}, // 3301
  {wxStyledTextCtrl_SetSelectionMode, "wxStyledTextCtrl", "setSelectionMode", 2}, // 3302
  {wxStyledTextCtrl_GetSelectionMode, "wxStyledTextCtrl", "getSelectionMode", 1}, // 3303
  {wxStyledTextCtrl_LineDownRectExtend, "wxStyledTextCtrl", "lineDownRectExtend", 1}, // 3304
  {wxStyledTextCtrl_LineUpRectExtend, "wxStyledTextCtrl", "lineUpRectExtend", 1}, // 3305
  {wxStyledTextCtrl_CharLeftRectExtend, "wxStyledTextCtrl", "charLeftRectExtend", 1}, // 3306
  {wxStyledTextCtrl_CharRightRectExtend, "wxStyledTextCtrl", "charRightRectExtend", 1}, // 3307
  {wxStyledTextCtrl_HomeRectExtend, "wxStyledTextCtrl", "homeRectExtend", 1}, // 3308
  {wxStyledTextCtrl_VCHomeRectExtend, "wxStyledTextCtrl", "vCHomeRectExtend", 1}, // 3309
  {wxStyledTextCtrl_LineEndRectExtend, "wxStyledTextCtrl", "lineEndRectExtend", 1}, // 3310
  {wxStyledTextCtrl_PageUpRectExtend, "wxStyledTextCtrl", "pageUpRectExtend", 1}, // 3311
  {wxStyledTextCtrl_PageDownRectExtend, "wxStyledTextCtrl", "pageDownRectExtend", 1}, // 3312
  {wxStyledTextCtrl_StutteredPageUp, "wxStyledTextCtrl", "stutteredPageUp", 1}, // 3313
  {wxStyledTextCtrl_StutteredPageUpExtend, "wxStyledTextCtrl", "stutteredPageUpExtend", 1}, // 3314
  {wxStyledTextCtrl_StutteredPageDown, "wxStyledTextCtrl", "stutteredPageDown", 1}, // 3315
  {wxStyledTextCtrl_StutteredPageDownExtend, "wxStyledTextCtrl", "stutteredPageDownExtend", 1}, // 3316
  {wxStyledTextCtrl_WordLeftEnd, "wxStyledTextCtrl", "wordLeftEnd", 1}, // 3317
  {wxStyledTextCtrl_WordLeftEndExtend, "wxStyledTextCtrl", "wordLeftEndExtend", 1}, // 3318
  {wxStyledTextCtrl_WordRightEnd, "wxStyledTextCtrl", "wordRightEnd", 1}, // 3319
  {wxStyledTextCtrl_WordRightEndExtend, "wxStyledTextCtrl", "wordRightEndExtend", 1}, // 3320
  {wxStyledTextCtrl_SetWhitespaceChars, "wxStyledTextCtrl", "setWhitespaceChars", 2}, // 3321
  {wxStyledTextCtrl_SetCharsDefault, "wxStyledTextCtrl", "setCharsDefault", 1}, // 3322
  {wxStyledTextCtrl_AutoCompGetCurrent, "wxStyledTextCtrl", "autoCompGetCurrent", 1}, // 3323
  {wxStyledTextCtrl_Allocate, "wxStyledTextCtrl", "allocate", 2}, // 3324
  {wxStyledTextCtrl_FindColumn, "wxStyledTextCtrl", "findColumn", 3}, // 3325
  {wxStyledTextCtrl_GetCaretSticky, "wxStyledTextCtrl", "getCaretSticky", 1}, // 3326
  {wxStyledTextCtrl_SetCaretSticky, "wxStyledTextCtrl", "setCaretSticky", 2}, // 3327
  {wxStyledTextCtrl_ToggleCaretSticky, "wxStyledTextCtrl", "toggleCaretSticky", 1}, // 3328
  {wxStyledTextCtrl_SetPasteConvertEndings, "wxStyledTextCtrl", "setPasteConvertEndings", 2}, // 3329
  {wxStyledTextCtrl_GetPasteConvertEndings, "wxStyledTextCtrl", "getPasteConvertEndings", 1}, // 3330
  {wxStyledTextCtrl_SelectionDuplicate, "wxStyledTextCtrl", "selectionDuplicate", 1}, // 3331
  {wxStyledTextCtrl_SetCaretLineBackAlpha, "wxStyledTextCtrl", "setCaretLineBackAlpha", 2}, // 3332
  {wxStyledTextCtrl_GetCaretLineBackAlpha, "wxStyledTextCtrl", "getCaretLineBackAlpha", 1}, // 3333
  {wxStyledTextCtrl_StartRecord, "wxStyledTextCtrl", "startRecord", 1}, // 3334
  {wxStyledTextCtrl_StopRecord, "wxStyledTextCtrl", "stopRecord", 1}, // 3335
  {wxStyledTextCtrl_SetLexer, "wxStyledTextCtrl", "setLexer", 2}, // 3336
  {wxStyledTextCtrl_GetLexer, "wxStyledTextCtrl", "getLexer", 1}, // 3337
  {wxStyledTextCtrl_Colourise, "wxStyledTextCtrl", "colourise", 3}, // 3338
  {wxStyledTextCtrl_SetProperty, "wxStyledTextCtrl", "setProperty", 3}, // 3339
  {wxStyledTextCtrl_SetKeyWords, "wxStyledTextCtrl", "setKeyWords", 3}, // 3340
  {wxStyledTextCtrl_SetLexerLanguage, "wxStyledTextCtrl", "setLexerLanguage", 2}, // 3341
  {wxStyledTextCtrl_GetProperty, "wxStyledTextCtrl", "getProperty", 2}, // 3342
  {wxStyledTextCtrl_GetStyleBitsNeeded, "wxStyledTextCtrl", "getStyleBitsNeeded", 1}, // 3343
  {wxStyledTextCtrl_GetCurrentLine, "wxStyledTextCtrl", "getCurrentLine", 1}, // 3344
  {wxStyledTextCtrl_StyleSetSpec, "wxStyledTextCtrl", "styleSetSpec", 3}, // 3345
  {wxStyledTextCtrl_StyleSetFont, "wxStyledTextCtrl", "styleSetFont", 3}, // 3346
  {wxStyledTextCtrl_StyleSetFontAttr, "wxStyledTextCtrl", "styleSetFontAttr", 8}, // 3347
  {wxStyledTextCtrl_StyleSetCharacterSet, "wxStyledTextCtrl", "styleSetCharacterSet", 3}, // 3348
  {wxStyledTextCtrl_StyleSetFontEncoding, "wxStyledTextCtrl", "styleSetFontEncoding", 3}, // 3349
  {wxStyledTextCtrl_CmdKeyExecute, "wxStyledTextCtrl", "cmdKeyExecute", 2}, // 3350
  {wxStyledTextCtrl_SetMargins, "wxStyledTextCtrl", "setMargins", 3}, // 3351
  {wxStyledTextCtrl_GetSelection, "wxStyledTextCtrl", "getSelection", 1}, // 3352
  {wxStyledTextCtrl_PointFromPosition, "wxStyledTextCtrl", "pointFromPosition", 2}, // 3353
  {wxStyledTextCtrl_ScrollToLine, "wxStyledTextCtrl", "scrollToLine", 2}, // 3354
  {wxStyledTextCtrl_ScrollToColumn, "wxStyledTextCtrl", "scrollToColumn", 2}, // 3355
  {wxStyledTextCtrl_SetVScrollBar, "wxStyledTextCtrl", "setVScrollBar", 2}, // 3356
  {wxStyledTextCtrl_SetHScrollBar, "wxStyledTextCtrl", "setHScrollBar", 2}, // 3357
  {wxStyledTextCtrl_GetLastKeydownProcessed, "wxStyledTextCtrl", "getLastKeydownProcessed", 1}, // 3358
  {wxStyledTextCtrl_SetLastKeydownProcessed, "wxStyledTextCtrl", "setLastKeydownProcessed", 2}, // 3359
  {wxStyledTextCtrl_SaveFile, "wxStyledTextCtrl", "saveFile", 2}, // 3360
  {wxStyledTextCtrl_LoadFile, "wxStyledTextCtrl", "loadFile", 2}, // 3361
  {wxStyledTextCtrl_DoDragOver, "wxStyledTextCtrl", "doDragOver", 4}, // 3362
  {wxStyledTextCtrl_DoDropText, "wxStyledTextCtrl", "doDropText", 4}, // 3363
  {wxStyledTextCtrl_GetUseAntiAliasing, "wxStyledTextCtrl", "getUseAntiAliasing", 1}, // 3364
  {wxStyledTextCtrl_AddTextRaw, "wxStyledTextCtrl", "addTextRaw", 3}, // 3365
  {wxStyledTextCtrl_InsertTextRaw, "wxStyledTextCtrl", "insertTextRaw", 3}, // 3366
  {wxStyledTextCtrl_GetCurLineRaw, "wxStyledTextCtrl", "getCurLineRaw", 1}, // 3367
  {wxStyledTextCtrl_GetLineRaw, "wxStyledTextCtrl", "getLineRaw", 2}, // 3368
  {wxStyledTextCtrl_GetSelectedTextRaw, "wxStyledTextCtrl", "getSelectedTextRaw", 1}, // 3369
  {wxStyledTextCtrl_GetTextRangeRaw, "wxStyledTextCtrl", "getTextRangeRaw", 3}, // 3370
  {wxStyledTextCtrl_SetTextRaw, "wxStyledTextCtrl", "setTextRaw", 2}, // 3371
  {wxStyledTextCtrl_GetTextRaw, "wxStyledTextCtrl", "getTextRaw", 1}, // 3372
  {wxStyledTextCtrl_AppendTextRaw, "wxStyledTextCtrl", "appendTextRaw", 3}, // 3373
  {wxArtProvider_GetBitmap, "wxArtProvider", "getBitmap", 2}, // 3374
  {wxArtProvider_GetIcon, "wxArtProvider", "getIcon", 2}, // 3375
  {wxTreeEvent_GetKeyCode, "wxTreeEvent", "getKeyCode", 1}, // 3376
  {wxTreeEvent_GetItem, "wxTreeEvent", "getItem", 1}, // 3377
  {wxTreeEvent_GetKeyEvent, "wxTreeEvent", "getKeyEvent", 1}, // 3378
  {wxTreeEvent_GetLabel, "wxTreeEvent", "getLabel", 1}, // 3379
  {wxTreeEvent_GetOldItem, "wxTreeEvent", "getOldItem", 1}, // 3380
  {wxTreeEvent_GetPoint, "wxTreeEvent", "getPoint", 1}, // 3381
  {wxTreeEvent_IsEditCancelled, "wxTreeEvent", "isEditCancelled", 1}, // 3382
  {wxTreeEvent_SetToolTip, "wxTreeEvent", "setToolTip", 2}, // 3383
  {wxBookCtrlEvent_GetOldSelection, "wxBookCtrlEvent", "getOldSelection", 1}, // 3384
  {wxBookCtrlEvent_GetSelection, "wxBookCtrlEvent", "getSelection", 1}, // 3385
  {wxBookCtrlEvent_SetOldSelection, "wxBookCtrlEvent", "setOldSelection", 2}, // 3386
  {wxBookCtrlEvent_SetSelection, "wxBookCtrlEvent", "setSelection", 2}, // 3387
  {wxFileDataObject_new, "wxFileDataObject", "new", 0}, // 3388
  {wxFileDataObject_AddFile, "wxFileDataObject", "addFile", 2}, // 3389
  {wxFileDataObject_GetFilenames, "wxFileDataObject", "getFilenames", 1}, // 3390
  {wxFileDataObject_destroy, "wxFileDataObject", "'Destroy'", 1}, // 3391
  {wxTextDataObject_new, "wxTextDataObject", "new", 1}, // 3392
  {wxTextDataObject_GetTextLength, "wxTextDataObject", "getTextLength", 1}, // 3393
  {wxTextDataObject_GetText, "wxTextDataObject", "getText", 1}, // 3394
  {wxTextDataObject_SetText, "wxTextDataObject", "setText", 2}, // 3395
  {wxTextDataObject_destroy, "wxTextDataObject", "'Destroy'", 1}, // 3396
  {wxBitmapDataObject_new_1_1, "wxBitmapDataObject", "new", 1}, // 3397
  {wxBitmapDataObject_new_1_0, "wxBitmapDataObject", "new", 1}, // 3398
  {wxBitmapDataObject_GetBitmap, "wxBitmapDataObject", "getBitmap", 1}, // 3399
  {wxBitmapDataObject_SetBitmap, "wxBitmapDataObject", "setBitmap", 2}, // 3400
  {wxBitmapDataObject_destroy, "wxBitmapDataObject", "'Destroy'", 1}, // 3401
  {wxClipboard_new, "wxClipboard", "new", 0}, // 3402
  {NULL, "wxClipboard", "destroy", 1}, // 3403 obj destructor wxClipboard_destruct
  {wxClipboard_AddData, "wxClipboard", "addData", 2}, // 3404
  {wxClipboard_Clear, "wxClipboard", "clear", 1}, // 3405
  {wxClipboard_Close, "wxClipboard", "close", 1}, // 3406
  {wxClipboard_Flush, "wxClipboard", "flush", 1}, // 3407
  {wxClipboard_GetData, "wxClipboard", "getData", 2}, // 3408
  {wxClipboard_IsOpened, "wxClipboard", "isOpened", 1}, // 3409
  {wxClipboard_Open, "wxClipboard", "open", 1}, // 3410
  {wxClipboard_SetData, "wxClipboard", "setData", 2}, // 3411
  {wxClipboard_UsePrimarySelection, "wxClipboard", "usePrimarySelection", 2}, // 3412
  {wxClipboard_IsSupported, "wxClipboard", "isSupported", 2}, // 3413
  {wxClipboard_Get, "wxClipboard", "get", 0}, // 3414
  {wxSpinEvent_GetPosition, "wxSpinEvent", "getPosition", 1}, // 3415
  {wxSpinEvent_SetPosition, "wxSpinEvent", "setPosition", 2}, // 3416
  {wxSplitterWindow_new_0, "wxSplitterWindow", "new", 0}, // 3417
  {wxSplitterWindow_new_2, "wxSplitterWindow", "new", 2}, // 3418
  {NULL, "wxSplitterWindow", "destroy", 1}, // 3419 obj destructor wxSplitterWindow_destruct
  {wxSplitterWindow_Create, "wxSplitterWindow", "create", 3}, // 3420
  {wxSplitterWindow_GetMinimumPaneSize, "wxSplitterWindow", "getMinimumPaneSize", 1}, // 3421
  {wxSplitterWindow_GetSashGravity, "wxSplitterWindow", "getSashGravity", 1}, // 3422
  {wxSplitterWindow_GetSashPosition, "wxSplitterWindow", "getSashPosition", 1}, // 3423
  {wxSplitterWindow_GetSplitMode, "wxSplitterWindow", "getSplitMode", 1}, // 3424
  {wxSplitterWindow_GetWindow1, "wxSplitterWindow", "getWindow1", 1}, // 3425
  {wxSplitterWindow_GetWindow2, "wxSplitterWindow", "getWindow2", 1}, // 3426
  {wxSplitterWindow_Initialize, "wxSplitterWindow", "initialize", 2}, // 3427
  {wxSplitterWindow_IsSplit, "wxSplitterWindow", "isSplit", 1}, // 3428
  {wxSplitterWindow_ReplaceWindow, "wxSplitterWindow", "replaceWindow", 3}, // 3429
  {wxSplitterWindow_SetSashGravity, "wxSplitterWindow", "setSashGravity", 2}, // 3430
  {wxSplitterWindow_SetSashPosition, "wxSplitterWindow", "setSashPosition", 3}, // 3431
  {wxSplitterWindow_SetMinimumPaneSize, "wxSplitterWindow", "setMinimumPaneSize", 2}, // 3432
  {wxSplitterWindow_SetSplitMode, "wxSplitterWindow", "setSplitMode", 2}, // 3433
  {wxSplitterWindow_SplitHorizontally, "wxSplitterWindow", "splitHorizontally", 4}, // 3434
  {wxSplitterWindow_SplitVertically, "wxSplitterWindow", "splitVertically", 4}, // 3435
  {wxSplitterWindow_Unsplit, "wxSplitterWindow", "unsplit", 2}, // 3436
  {wxSplitterWindow_UpdateSize, "wxSplitterWindow", "updateSize", 1}, // 3437
  {wxSplitterEvent_GetSashPosition, "wxSplitterEvent", "getSashPosition", 1}, // 3438
  {wxSplitterEvent_GetX, "wxSplitterEvent", "getX", 1}, // 3439
  {wxSplitterEvent_GetY, "wxSplitterEvent", "getY", 1}, // 3440
  {wxSplitterEvent_GetWindowBeingRemoved, "wxSplitterEvent", "getWindowBeingRemoved", 1}, // 3441
  {wxSplitterEvent_SetSashPosition, "wxSplitterEvent", "setSashPosition", 2}, // 3442
  {wxHtmlWindow_new_0, "wxHtmlWindow", "new", 0}, // 3443
  {wxHtmlWindow_new_2, "wxHtmlWindow", "new", 2}, // 3444
  {wxHtmlWindow_AppendToPage, "wxHtmlWindow", "appendToPage", 2}, // 3445
  {wxHtmlWindow_GetOpenedAnchor, "wxHtmlWindow", "getOpenedAnchor", 1}, // 3446
  {wxHtmlWindow_GetOpenedPage, "wxHtmlWindow", "getOpenedPage", 1}, // 3447
  {wxHtmlWindow_GetOpenedPageTitle, "wxHtmlWindow", "getOpenedPageTitle", 1}, // 3448
  {wxHtmlWindow_GetRelatedFrame, "wxHtmlWindow", "getRelatedFrame", 1}, // 3449
  {wxHtmlWindow_HistoryBack, "wxHtmlWindow", "historyBack", 1}, // 3450
  {wxHtmlWindow_HistoryCanBack, "wxHtmlWindow", "historyCanBack", 1}, // 3451
  {wxHtmlWindow_HistoryCanForward, "wxHtmlWindow", "historyCanForward", 1}, // 3452
  {wxHtmlWindow_HistoryClear, "wxHtmlWindow", "historyClear", 1}, // 3453
  {wxHtmlWindow_HistoryForward, "wxHtmlWindow", "historyForward", 1}, // 3454
  {wxHtmlWindow_LoadFile, "wxHtmlWindow", "loadFile", 2}, // 3455
  {wxHtmlWindow_LoadPage, "wxHtmlWindow", "loadPage", 2}, // 3456
  {wxHtmlWindow_SelectAll, "wxHtmlWindow", "selectAll", 1}, // 3457
  {wxHtmlWindow_SelectionToText, "wxHtmlWindow", "selectionToText", 1}, // 3458
  {wxHtmlWindow_SelectLine, "wxHtmlWindow", "selectLine", 2}, // 3459
  {wxHtmlWindow_SelectWord, "wxHtmlWindow", "selectWord", 2}, // 3460
  {wxHtmlWindow_SetBorders, "wxHtmlWindow", "setBorders", 2}, // 3461
  {wxHtmlWindow_SetFonts, "wxHtmlWindow", "setFonts", 4}, // 3462
  {wxHtmlWindow_SetPage, "wxHtmlWindow", "setPage", 2}, // 3463
  {wxHtmlWindow_SetRelatedFrame, "wxHtmlWindow", "setRelatedFrame", 3}, // 3464
  {wxHtmlWindow_SetRelatedStatusBar_1, "wxHtmlWindow", "setRelatedStatusBar", 2}, // 3465
  {wxHtmlWindow_SetRelatedStatusBar_2, "wxHtmlWindow", "setRelatedStatusBar", 3}, // 3466
  {wxHtmlWindow_ToText, "wxHtmlWindow", "toText", 1}, // 3467
  {NULL, "wxHtmlWindow", "'Destroy'", 1}, // 3468 obj destructor wxHtmlWindow_destroy
  {wxHtmlLinkEvent_GetLinkInfo, "wxHtmlLinkEvent", "getLinkInfo", 1}, // 3469
  {wxSystemSettings_GetColour, "wxSystemSettings", "getColour", 1}, // 3470
  {wxSystemSettings_GetFont, "wxSystemSettings", "getFont", 1}, // 3471
  {wxSystemSettings_GetMetric, "wxSystemSettings", "getMetric", 2}, // 3472
  {wxSystemSettings_GetScreenType, "wxSystemSettings", "getScreenType", 0}, // 3473
  {wxSystemOptions_GetOption, "wxSystemOptions", "getOption", 1}, // 3474
  {wxSystemOptions_GetOptionInt, "wxSystemOptions", "getOptionInt", 1}, // 3475
  {wxSystemOptions_HasOption, "wxSystemOptions", "hasOption", 1}, // 3476
  {wxSystemOptions_IsFalse, "wxSystemOptions", "isFalse", 1}, // 3477
  {wxSystemOptions_SetOption_2_1, "wxSystemOptions", "setOption", 2}, // 3478
  {wxSystemOptions_SetOption_2_0, "wxSystemOptions", "setOption", 2}, // 3479
  {wxAuiNotebookEvent_SetSelection, "wxAuiNotebookEvent", "setSelection", 2}, // 3480
  {wxAuiNotebookEvent_GetSelection, "wxAuiNotebookEvent", "getSelection", 1}, // 3481
  {wxAuiNotebookEvent_SetOldSelection, "wxAuiNotebookEvent", "setOldSelection", 2}, // 3482
  {wxAuiNotebookEvent_GetOldSelection, "wxAuiNotebookEvent", "getOldSelection", 1}, // 3483
  {wxAuiNotebookEvent_SetDragSource, "wxAuiNotebookEvent", "setDragSource", 2}, // 3484
  {wxAuiNotebookEvent_GetDragSource, "wxAuiNotebookEvent", "getDragSource", 1}, // 3485
  {wxAuiManagerEvent_SetManager, "wxAuiManagerEvent", "setManager", 2}, // 3486
  {wxAuiManagerEvent_GetManager, "wxAuiManagerEvent", "getManager", 1}, // 3487
  {wxAuiManagerEvent_SetPane, "wxAuiManagerEvent", "setPane", 2}, // 3488
  {wxAuiManagerEvent_GetPane, "wxAuiManagerEvent", "getPane", 1}, // 3489
  {wxAuiManagerEvent_SetButton, "wxAuiManagerEvent", "setButton", 2}, // 3490
  {wxAuiManagerEvent_GetButton, "wxAuiManagerEvent", "getButton", 1}, // 3491
  {wxAuiManagerEvent_SetDC, "wxAuiManagerEvent", "setDC", 2}, // 3492
  {wxAuiManagerEvent_GetDC, "wxAuiManagerEvent", "getDC", 1}, // 3493
  {wxAuiManagerEvent_Veto, "wxAuiManagerEvent", "veto", 2}, // 3494
  {wxAuiManagerEvent_GetVeto, "wxAuiManagerEvent", "getVeto", 1}, // 3495
  {wxAuiManagerEvent_SetCanVeto, "wxAuiManagerEvent", "setCanVeto", 2}, // 3496
  {wxAuiManagerEvent_CanVeto, "wxAuiManagerEvent", "canVeto", 1}, // 3497
  {wxLogNull_new, "wxLogNull", "new", 0}, // 3498
  {wxLogNull_destruct, "wxLogNull", "destroy", 1}, // 3499
  {wxTaskBarIcon_new, "wxTaskBarIcon", "new", 1}, // 3500
  {NULL, "wxTaskBarIcon", "destroy", 1}, // 3501 obj destructor wxTaskBarIcon_destruct
  {wxTaskBarIcon_PopupMenu, "wxTaskBarIcon", "popupMenu", 2}, // 3502
  {wxTaskBarIcon_RemoveIcon, "wxTaskBarIcon", "removeIcon", 1}, // 3503
  {wxTaskBarIcon_SetIcon, "wxTaskBarIcon", "setIcon", 3}, // 3504
  {wxLocale_new_0, "wxLocale", "new", 0}, // 3505
  {wxLocale_new_2_0, "wxLocale", "new", 2}, // 3506
  {wxLocale_new_2_1, "wxLocale", "new", 2}, // 3507
  {wxLocale_destruct, "wxLocale", "destroy", 1}, // 3508
  {wxLocale_Init_1, "wxLocale", "init", 2}, // 3509
  {wxLocale_Init_2, "wxLocale", "init", 3}, // 3510
  {wxLocale_AddCatalog_1, "wxLocale", "addCatalog", 2}, // 3511
  {wxLocale_AddCatalog_2, "wxLocale", "addCatalog", 3}, // 3512
  {wxLocale_AddCatalog_3, "wxLocale", "addCatalog", 4}, // 3513
  {wxLocale_AddCatalogLookupPathPrefix, "wxLocale", "addCatalogLookupPathPrefix", 1}, // 3514
  {wxLocale_GetCanonicalName, "wxLocale", "getCanonicalName", 1}, // 3515
  {wxLocale_GetLanguage, "wxLocale", "getLanguage", 1}, // 3516
  {wxLocale_GetLanguageName, "wxLocale", "getLanguageName", 1}, // 3517
  {wxLocale_GetLocale, "wxLocale", "getLocale", 1}, // 3518
  {wxLocale_GetName, "wxLocale", "getName", 1}, // 3519
  {wxLocale_GetString_2, "wxLocale", "getString", 3}, // 3520
  {wxLocale_GetString_4, "wxLocale", "getString", 5}, // 3521
  {wxLocale_GetHeaderValue, "wxLocale", "getHeaderValue", 3}, // 3522
  {wxLocale_GetSysName, "wxLocale", "getSysName", 1}, // 3523
  {wxLocale_GetSystemEncoding, "wxLocale", "getSystemEncoding", 0}, // 3524
  {wxLocale_GetSystemEncodingName, "wxLocale", "getSystemEncodingName", 0}, // 3525
  {wxLocale_GetSystemLanguage, "wxLocale", "getSystemLanguage", 0}, // 3526
  {wxLocale_IsLoaded, "wxLocale", "isLoaded", 2}, // 3527
  {wxLocale_IsOk, "wxLocale", "isOk", 1}, // 3528
  {wxActivateEvent_GetActive, "wxActivateEvent", "getActive", 1}, // 3529
#if wxUSE_POPUPWIN
  {wxPopupWindow_new_0, "wxPopupWindow", "new", 0}, // 3530
#else
  {NULL, "wxPopupWindow", "new", 0}, // 3530
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
  {wxPopupWindow_new_2, "wxPopupWindow", "new", 2}, // 3531
#else
  {NULL, "wxPopupWindow", "new", 0}, // 3531
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
  {wxPopupWindow_Create, "wxPopupWindow", "create", 3}, // 3532
#else
  {NULL, "wxPopupWindow", "create", 0}, // 3532
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
  {wxPopupWindow_Position, "wxPopupWindow", "position", 3}, // 3533
#else
  {NULL, "wxPopupWindow", "position", 0}, // 3533
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
  {NULL, "wxPopupWindow", "'Destroy'", 1}, // 3534 obj destructor wxPopupWindow_destroy
#else
  {NULL, "wxPopupWindow", "'Destroy'", 0}, // 3534
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
  {wxPopupTransientWindow_new_0, "wxPopupTransientWindow", "new", 0}, // 3535
#else
  {NULL, "wxPopupTransientWindow", "new", 0}, // 3535
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
  {wxPopupTransientWindow_new_2, "wxPopupTransientWindow", "new", 2}, // 3536
#else
  {NULL, "wxPopupTransientWindow", "new", 0}, // 3536
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
  {wxPopupTransientWindow_Popup, "wxPopupTransientWindow", "popup", 2}, // 3537
#else
  {NULL, "wxPopupTransientWindow", "popup", 0}, // 3537
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
  {wxPopupTransientWindow_Dismiss, "wxPopupTransientWindow", "dismiss", 1}, // 3538
#else
  {NULL, "wxPopupTransientWindow", "dismiss", 0}, // 3538
#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
  {NULL, "wxPopupTransientWindow", "'Destroy'", 1}, // 3539 obj destructor wxPopupTransientWindow_destroy
#else
  {NULL, "wxPopupTransientWindow", "'Destroy'", 0}, // 3539
#endif // wxUSE_POPUPWIN
  {wxOverlay_new, "wxOverlay", "new", 0}, // 3540
  {wxOverlay_destruct, "wxOverlay", "destroy", 1}, // 3541
  {wxOverlay_Reset, "wxOverlay", "reset", 1}, // 3542
  {wxDCOverlay_new_6, "wxDCOverlay", "new", 6}, // 3543
  {wxDCOverlay_new_2, "wxDCOverlay", "new", 2}, // 3544
  {wxDCOverlay_destruct, "wxDCOverlay", "destroy", 1}, // 3545
  {wxDCOverlay_Clear, "wxDCOverlay", "clear", 1}, // 3546
  {wxDropFilesEvent_GetPosition, "wxDropFilesEvent", "getPosition", 1}, // 3547
  {wxDropFilesEvent_GetNumberOfFiles, "wxDropFilesEvent", "getNumberOfFiles", 1}, // 3548
  {wxDropFilesEvent_GetFiles, "wxDropFilesEvent", "getFiles", 1}, // 3549
#if wxUSE_DISPLAY
  {wxDisplay_new_0, "wxDisplay", "new", 0}, // 3550
#else
  {NULL, "wxDisplay", "new", 0}, // 3550
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
  {wxDisplay_new_1_0, "wxDisplay", "new", 1}, // 3551
#else
  {NULL, "wxDisplay", "new", 0}, // 3551
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY && wxCHECK_VERSION(3,1,3)
  {wxDisplay_new_1_1, "wxDisplay", "new", 1}, // 3552
#else
  {NULL, "wxDisplay", "new", 0}, // 3552
#endif // wxUSE_DISPLAY && wxCHECK_VERSION(3,1,3)
#if wxUSE_DISPLAY
  {wxDisplay_destruct, "wxDisplay", "destroy", 1}, // 3553
#else
  {NULL, "wxDisplay", "destroy", 0}, // 3553
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
  {wxDisplay_IsOk, "wxDisplay", "isOk", 1}, // 3554
#else
  {NULL, "wxDisplay", "isOk", 0}, // 3554
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
  {wxDisplay_GetClientArea, "wxDisplay", "getClientArea", 1}, // 3555
#else
  {NULL, "wxDisplay", "getClientArea", 0}, // 3555
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
  {wxDisplay_GetGeometry, "wxDisplay", "getGeometry", 1}, // 3556
#else
  {NULL, "wxDisplay", "getGeometry", 0}, // 3556
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
  {wxDisplay_GetName, "wxDisplay", "getName", 1}, // 3557
#else
  {NULL, "wxDisplay", "getName", 0}, // 3557
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
  {wxDisplay_IsPrimary, "wxDisplay", "isPrimary", 1}, // 3558
#else
  {NULL, "wxDisplay", "isPrimary", 0}, // 3558
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
  {wxDisplay_GetCount, "wxDisplay", "getCount", 0}, // 3559
#else
  {NULL, "wxDisplay", "getCount", 0}, // 3559
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
  {wxDisplay_GetFromPoint, "wxDisplay", "getFromPoint", 1}, // 3560
#else
  {NULL, "wxDisplay", "getFromPoint", 0}, // 3560
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY
  {wxDisplay_GetFromWindow, "wxDisplay", "getFromWindow", 1}, // 3561
#else
  {NULL, "wxDisplay", "getFromWindow", 0}, // 3561
#endif // wxUSE_DISPLAY
#if wxUSE_DISPLAY && wxCHECK_VERSION(3,1,2)
  {wxDisplay_GetPPI, "wxDisplay", "getPPI", 1}, // 3562
#else
  {NULL, "wxDisplay", "getPPI", 0}, // 3562
#endif // wxUSE_DISPLAY && wxCHECK_VERSION(3,1,2)
#if wxUSE_GRAPHICS_CONTEXT
  {wxGCDC_new_1, "wxGCDC", "new", 1}, // 3563
#else
  {NULL, "wxGCDC", "new", 0}, // 3563
#endif // wxUSE_GRAPHICS_CONTEXT
  {NULL, "", "", 0}, // 3564
  {NULL, "", "", 0}, // 3565
#if wxUSE_GRAPHICS_CONTEXT
  {wxGCDC_new_0, "wxGCDC", "new", 0}, // 3566
#else
  {NULL, "wxGCDC", "new", 0}, // 3566
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {NULL, "wxGCDC", "destroy", 1}, // 3567 obj destructor wxGCDC_destruct
#else
  {NULL, "wxGCDC", "destroy", 0}, // 3567
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGCDC_GetGraphicsContext, "wxGCDC", "getGraphicsContext", 1}, // 3568
#else
  {NULL, "wxGCDC", "getGraphicsContext", 0}, // 3568
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
  {wxGCDC_SetGraphicsContext, "wxGCDC", "setGraphicsContext", 2}, // 3569
#else
  {NULL, "wxGCDC", "setGraphicsContext", 0}, // 3569
#endif // wxUSE_GRAPHICS_CONTEXT
  {wxNotificationMessage_new_0, "wxNotificationMessage", "new", 0}, // 3570
  {wxNotificationMessage_new_2, "wxNotificationMessage", "new", 2}, // 3571
  {NULL, "wxNotificationMessage", "destroy", 1}, // 3572 obj destructor wxNotificationMessage_destruct
#if wxCHECK_VERSION(3,1,0)
  {wxNotificationMessage_AddAction, "wxNotificationMessage", "addAction", 3}, // 3573
#else
  {NULL, "wxNotificationMessage", "addAction", 0}, // 3573
#endif // wxCHECK_VERSION(3,1,0)
  {wxNotificationMessage_Close, "wxNotificationMessage", "close", 1}, // 3574
  {wxNotificationMessage_SetFlags, "wxNotificationMessage", "setFlags", 2}, // 3575
#if wxCHECK_VERSION(3,1,0)
  {wxNotificationMessage_SetIcon, "wxNotificationMessage", "setIcon", 2}, // 3576
#else
  {NULL, "wxNotificationMessage", "setIcon", 0}, // 3576
#endif // wxCHECK_VERSION(3,1,0)
  {wxNotificationMessage_SetMessage, "wxNotificationMessage", "setMessage", 2}, // 3577
  {wxNotificationMessage_SetParent, "wxNotificationMessage", "setParent", 2}, // 3578
  {wxNotificationMessage_SetTitle, "wxNotificationMessage", "setTitle", 2}, // 3579
  {wxNotificationMessage_Show, "wxNotificationMessage", "show", 2}, // 3580
#if __WXMSW__ 
  {wxNotificationMessage_UseTaskBarIcon, "wxNotificationMessage", "useTaskBarIcon", 1}, // 3581
#else
  {NULL, "wxNotificationMessage", "useTaskBarIcon", 0}, // 3581
#endif // __WXMSW__ 
#if __WXMSW__ && wxCHECK_VERSION(3,1,0)
  {wxNotificationMessage_MSWUseToasts, "wxNotificationMessage", "mSWUseToasts", 1}, // 3582
#else
  {NULL, "wxNotificationMessage", "mSWUseToasts", 0}, // 3582
#endif // __WXMSW__ && wxCHECK_VERSION(3,1,0)
  {NULL, "", "", 0}, // 3583
#if WXE_WEBVIEW
  {wxWebView_New, "wxWebView", "new", 3}, // 3584
#else
  {NULL, "wxWebView", "new", 0}, // 3584
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_GetCurrentTitle, "wxWebView", "getCurrentTitle", 1}, // 3585
#else
  {NULL, "wxWebView", "getCurrentTitle", 0}, // 3585
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_GetCurrentURL, "wxWebView", "getCurrentURL", 1}, // 3586
#else
  {NULL, "wxWebView", "getCurrentURL", 0}, // 3586
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_GetPageSource, "wxWebView", "getPageSource", 1}, // 3587
#else
  {NULL, "wxWebView", "getPageSource", 0}, // 3587
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_GetPageText, "wxWebView", "getPageText", 1}, // 3588
#else
  {NULL, "wxWebView", "getPageText", 0}, // 3588
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_IsBusy, "wxWebView", "isBusy", 1}, // 3589
#else
  {NULL, "wxWebView", "isBusy", 0}, // 3589
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_IsEditable, "wxWebView", "isEditable", 1}, // 3590
#else
  {NULL, "wxWebView", "isEditable", 0}, // 3590
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_LoadURL, "wxWebView", "loadURL", 2}, // 3591
#else
  {NULL, "wxWebView", "loadURL", 0}, // 3591
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_Print, "wxWebView", "print", 1}, // 3592
#else
  {NULL, "wxWebView", "print", 0}, // 3592
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_Reload, "wxWebView", "reload", 2}, // 3593
#else
  {NULL, "wxWebView", "reload", 0}, // 3593
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW && wxCHECK_VERSION(3,1,1)
  {wxWebView_RunScript, "wxWebView", "runScript", 2}, // 3594
#else
  {NULL, "wxWebView", "runScript", 0}, // 3594
#endif // WXE_WEBVIEW && wxCHECK_VERSION(3,1,1)
#if WXE_WEBVIEW
  {wxWebView_SetEditable, "wxWebView", "setEditable", 2}, // 3595
#else
  {NULL, "wxWebView", "setEditable", 0}, // 3595
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_SetPage, "wxWebView", "setPage", 3}, // 3596
#else
  {NULL, "wxWebView", "setPage", 0}, // 3596
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_Stop, "wxWebView", "stop", 1}, // 3597
#else
  {NULL, "wxWebView", "stop", 0}, // 3597
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_CanCopy, "wxWebView", "canCopy", 1}, // 3598
#else
  {NULL, "wxWebView", "canCopy", 0}, // 3598
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_CanCut, "wxWebView", "canCut", 1}, // 3599
#else
  {NULL, "wxWebView", "canCut", 0}, // 3599
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_CanPaste, "wxWebView", "canPaste", 1}, // 3600
#else
  {NULL, "wxWebView", "canPaste", 0}, // 3600
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_Copy, "wxWebView", "copy", 1}, // 3601
#else
  {NULL, "wxWebView", "copy", 0}, // 3601
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_Cut, "wxWebView", "cut", 1}, // 3602
#else
  {NULL, "wxWebView", "cut", 0}, // 3602
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_Paste, "wxWebView", "paste", 1}, // 3603
#else
  {NULL, "wxWebView", "paste", 0}, // 3603
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_EnableContextMenu, "wxWebView", "enableContextMenu", 2}, // 3604
#else
  {NULL, "wxWebView", "enableContextMenu", 0}, // 3604
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_IsContextMenuEnabled, "wxWebView", "isContextMenuEnabled", 1}, // 3605
#else
  {NULL, "wxWebView", "isContextMenuEnabled", 0}, // 3605
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_CanGoBack, "wxWebView", "canGoBack", 1}, // 3606
#else
  {NULL, "wxWebView", "canGoBack", 0}, // 3606
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_CanGoForward, "wxWebView", "canGoForward", 1}, // 3607
#else
  {NULL, "wxWebView", "canGoForward", 0}, // 3607
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_ClearHistory, "wxWebView", "clearHistory", 1}, // 3608
#else
  {NULL, "wxWebView", "clearHistory", 0}, // 3608
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_EnableHistory, "wxWebView", "enableHistory", 2}, // 3609
#else
  {NULL, "wxWebView", "enableHistory", 0}, // 3609
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_GoBack, "wxWebView", "goBack", 1}, // 3610
#else
  {NULL, "wxWebView", "goBack", 0}, // 3610
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_GoForward, "wxWebView", "goForward", 1}, // 3611
#else
  {NULL, "wxWebView", "goForward", 0}, // 3611
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_ClearSelection, "wxWebView", "clearSelection", 1}, // 3612
#else
  {NULL, "wxWebView", "clearSelection", 0}, // 3612
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_DeleteSelection, "wxWebView", "deleteSelection", 1}, // 3613
#else
  {NULL, "wxWebView", "deleteSelection", 0}, // 3613
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_GetSelectedSource, "wxWebView", "getSelectedSource", 1}, // 3614
#else
  {NULL, "wxWebView", "getSelectedSource", 0}, // 3614
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_GetSelectedText, "wxWebView", "getSelectedText", 1}, // 3615
#else
  {NULL, "wxWebView", "getSelectedText", 0}, // 3615
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_HasSelection, "wxWebView", "hasSelection", 1}, // 3616
#else
  {NULL, "wxWebView", "hasSelection", 0}, // 3616
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_SelectAll, "wxWebView", "selectAll", 1}, // 3617
#else
  {NULL, "wxWebView", "selectAll", 0}, // 3617
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_CanRedo, "wxWebView", "canRedo", 1}, // 3618
#else
  {NULL, "wxWebView", "canRedo", 0}, // 3618
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_CanUndo, "wxWebView", "canUndo", 1}, // 3619
#else
  {NULL, "wxWebView", "canUndo", 0}, // 3619
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_Redo, "wxWebView", "redo", 1}, // 3620
#else
  {NULL, "wxWebView", "redo", 0}, // 3620
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_Undo, "wxWebView", "undo", 1}, // 3621
#else
  {NULL, "wxWebView", "undo", 0}, // 3621
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_Find, "wxWebView", "find", 3}, // 3622
#else
  {NULL, "wxWebView", "find", 0}, // 3622
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_CanSetZoomType, "wxWebView", "canSetZoomType", 2}, // 3623
#else
  {NULL, "wxWebView", "canSetZoomType", 0}, // 3623
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_GetZoom, "wxWebView", "getZoom", 1}, // 3624
#else
  {NULL, "wxWebView", "getZoom", 0}, // 3624
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_GetZoomType, "wxWebView", "getZoomType", 1}, // 3625
#else
  {NULL, "wxWebView", "getZoomType", 0}, // 3625
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_SetZoom, "wxWebView", "setZoom", 2}, // 3626
#else
  {NULL, "wxWebView", "setZoom", 0}, // 3626
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebView_SetZoomType, "wxWebView", "setZoomType", 2}, // 3627
#else
  {NULL, "wxWebView", "setZoomType", 0}, // 3627
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW && wxCHECK_VERSION(3,1,4)
  {wxWebView_GetZoomFactor, "wxWebView", "getZoomFactor", 1}, // 3628
#else
  {NULL, "wxWebView", "getZoomFactor", 0}, // 3628
#endif // WXE_WEBVIEW && wxCHECK_VERSION(3,1,4)
#if WXE_WEBVIEW && wxCHECK_VERSION(3,1,4)
  {wxWebView_SetZoomFactor, "wxWebView", "setZoomFactor", 2}, // 3629
#else
  {NULL, "wxWebView", "setZoomFactor", 0}, // 3629
#endif // WXE_WEBVIEW && wxCHECK_VERSION(3,1,4)
#if WXE_WEBVIEW && wxCHECK_VERSION(3,1,4)
  {wxWebView_IsBackendAvailable, "wxWebView", "isBackendAvailable", 1}, // 3630
#else
  {NULL, "wxWebView", "isBackendAvailable", 0}, // 3630
#endif // WXE_WEBVIEW && wxCHECK_VERSION(3,1,4)
#if WXE_WEBVIEW
  {wxWebViewEvent_GetString, "wxWebViewEvent", "getString", 1}, // 3631
#else
  {NULL, "wxWebViewEvent", "getString", 0}, // 3631
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebViewEvent_GetInt, "wxWebViewEvent", "getInt", 1}, // 3632
#else
  {NULL, "wxWebViewEvent", "getInt", 0}, // 3632
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebViewEvent_GetTarget, "wxWebViewEvent", "getTarget", 1}, // 3633
#else
  {NULL, "wxWebViewEvent", "getTarget", 0}, // 3633
#endif // WXE_WEBVIEW
#if WXE_WEBVIEW
  {wxWebViewEvent_GetURL, "wxWebViewEvent", "getURL", 1}, // 3634
#else
  {NULL, "wxWebViewEvent", "getURL", 0}, // 3634
#endif // WXE_WEBVIEW
};
