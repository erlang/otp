/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
extern void wxWindow_CacheBestSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_CaptureMouse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_Center(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxWindow_CenterOnParent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
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
extern void wxWindow_GetEventHandler(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
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
extern void wxWindow_PopEventHandler(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
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
#endif
extern void wxWindow_GetContentScaleFactor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if wxCHECK_VERSION(3,1,3)
extern void wxWindow_GetDPI(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_FromDIP_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_FromDIP_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_FromDIP_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_FromDIP_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_ToDIP_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_ToDIP_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_ToDIP_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif
#if wxCHECK_VERSION(3,1,0)
extern void wxWindow_ToDIP_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif
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
extern void wxTopLevelWindow_CenterOnScreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
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
#endif
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
extern void wxPostScriptDC_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPostScriptDC_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
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
extern void wxGraphicsObject_GetRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsObject_IsNull(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_Create_STAT_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_Create_STAT_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_CreatePen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_CreateBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_CreateRadialGradientBrush_7(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_CreateRadialGradientBrush_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_CreateLinearGradientBrush_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_CreateLinearGradientBrush_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_CreateFont_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_CreateFont_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_CreateMatrix(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_CreatePath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_Clip_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_Clip_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_ResetClip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_DrawBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_DrawEllipse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_DrawIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_DrawLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_DrawPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_DrawRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_DrawRoundedRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_DrawText_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_DrawText_4_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_DrawText_4_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_DrawText_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_FillPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_StrokePath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_GetPartialTextExtents(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_GetTextExtent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_Rotate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_Scale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_Translate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_GetTransform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_SetTransform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_ConcatTransform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_SetBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_SetFont_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_SetFont_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_SetPen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_StrokeLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsContext_StrokeLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsMatrix_Concat(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsMatrix_Get(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsMatrix_Invert(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsMatrix_IsEqual(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsMatrix_IsIdentity(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsMatrix_Rotate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsMatrix_Scale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsMatrix_Translate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsMatrix_Set(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsMatrix_TransformPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsMatrix_TransformDistance(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_MoveToPoint_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_MoveToPoint_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_AddArc_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_AddArc_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_AddArcToPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_AddCircle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_AddCurveToPoint_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_AddCurveToPoint_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_AddEllipse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_AddLineToPoint_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_AddLineToPoint_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_AddPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_AddQuadCurveToPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_AddRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_AddRoundedRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_CloseSubpath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_Contains_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_Contains_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_GetBox(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_GetCurrentPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsPath_Transform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsRenderer_GetDefaultRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsRenderer_CreateContext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsRenderer_CreateBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsRenderer_CreateLinearGradientBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsRenderer_CreateRadialGradientBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsRenderer_CreateFont_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsRenderer_CreateFont_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsRenderer_CreateMatrix(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsRenderer_CreatePath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsGradientStops_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsGradientStops_Item(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsGradientStops_GetCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsGradientStops_SetStartColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsGradientStops_GetStartColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsGradientStops_SetEndColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsGradientStops_GetEndColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGraphicsGradientStops_Add(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
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
#endif
#if defined(__WXMAC__)
extern void wxMenuBar_GetAutoWindowMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif
#if defined(__WXMAC__)
extern void wxMenuBar_OSXGetAppleMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif
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
extern void wxImage_new_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_4_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_3_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_4_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_new_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Blur(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_BlurHorizontal(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_BlurVertical(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_ConvertAlphaToMask_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_ConvertAlphaToMask_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_ConvertToGreyscale_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_ConvertToGreyscale_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_ConvertToMono(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Copy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Create_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Create_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Create_4_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Create_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Create_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_Create_4_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
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
extern void wxImage_SetAlpha_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SetAlpha_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SetData_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImage_SetData_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
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
#endif
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
#endif
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
extern void wxImageList_Add_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxImageList_Add_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
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
extern void wxGLCanvas_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGLCanvas_SetCurrent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGLCanvas_SwapBuffers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGLContext_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGLContext_SetCurrent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_AddPane_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_AddPane_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_AddPane_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_DetachPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_GetAllPanes(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_GetArtProvider(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_GetDockSizeConstraint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_GetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_GetManagedWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_GetManager(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_GetPane_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_GetPane_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_HideHint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_InsertPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_LoadPaneInfo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_LoadPerspective(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_SavePaneInfo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_SavePerspective(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_SetArtProvider(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_SetDockSizeConstraint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_SetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_SetManagedWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_ShowHint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_UnInit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiManager_Update(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_BestSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_BestSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Bottom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_BottomDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Caption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_CaptionVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Centre(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_CentrePane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_CloseButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_DefaultPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_DestroyOnClose(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Direction(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Dock(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Dockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Fixed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Float(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Floatable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_FloatingPosition_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_FloatingPosition_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_FloatingSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_FloatingSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Gripper(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_GripperTop(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_HasBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_HasCaption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_HasCloseButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_HasFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_HasGripper(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_HasGripperTop(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_HasMaximizeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_HasMinimizeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_HasPinButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Hide(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_IsBottomDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_IsDocked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_IsFixed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_IsFloatable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_IsFloating(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_IsLeftDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_IsMovable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_IsResizable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_IsRightDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_IsShown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_IsToolbar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_IsTopDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Layer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Left(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_LeftDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_MaxSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_MaxSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_MaximizeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_MinSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_MinSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_MinimizeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Movable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Name(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_PaneBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_PinButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Position(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Resizable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Right(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_RightDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Row(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_SafeSet(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_SetFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_ToolbarPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Top(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_TopDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_Window(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_GetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_GetFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_GetDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_GetLayer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_GetRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_GetFloatingPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_GetFloatingSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiPaneInfo_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_AddPage_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_AddPage_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_Create_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_Create_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_DeletePage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_GetArtProvider(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_GetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_GetPageBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_GetPageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_GetPageIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_GetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_InsertPage_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_InsertPage_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_RemovePage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_SetArtProvider(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_SetPageBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_SetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_SetTabCtrlHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiNotebook_SetUniformBitmapSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiTabArt_SetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiTabArt_SetMeasuringFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiTabArt_SetNormalFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiTabArt_SetSelectedFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiTabArt_SetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiTabArt_SetActiveColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiDockArt_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiDockArt_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiDockArt_GetMetric(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiDockArt_SetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiDockArt_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiDockArt_SetMetric(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiSimpleTabArt_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxAuiSimpleTabArt_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
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
extern void wxPopupWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPopupWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPopupWindow_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPopupWindow_Position(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPopupTransientWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPopupTransientWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPopupTransientWindow_Popup(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxPopupTransientWindow_Dismiss(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
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
extern void wxDisplay_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDisplay_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if wxCHECK_VERSION(3,1,3)
extern void wxDisplay_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif
extern void wxDisplay_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDisplay_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDisplay_GetClientArea(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDisplay_GetGeometry(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDisplay_GetName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDisplay_IsPrimary(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDisplay_GetCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDisplay_GetFromPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxDisplay_GetFromWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#if wxCHECK_VERSION(3,1,2)
extern void wxDisplay_GetPPI(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
#endif
extern void wxGCDC_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGCDC_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGCDC_GetGraphicsContext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);
extern void wxGCDC_SetGraphicsContext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd);

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
  {wxWindow_CacheBestSize, "wxWindow", "cacheBestSize", 2}, // 107
  {wxWindow_CaptureMouse, "wxWindow", "captureMouse", 1}, // 108
  {wxWindow_Center, "wxWindow", "center", 2}, // 109
  {wxWindow_CenterOnParent, "wxWindow", "centerOnParent", 2}, // 110
  {wxWindow_ClearBackground, "wxWindow", "clearBackground", 1}, // 111
  {wxWindow_ClientToScreen_2, "wxWindow", "clientToScreen", 3}, // 112
  {wxWindow_ClientToScreen_1, "wxWindow", "clientToScreen", 2}, // 113
  {wxWindow_Close, "wxWindow", "close", 2}, // 114
  {NULL, "", "", 0}, // 115
  {wxWindow_ConvertDialogToPixels, "wxWindow", "convertDialogToPixels", 2}, // 116
  {NULL, "", "", 0}, // 117
  {wxWindow_ConvertPixelsToDialog, "wxWindow", "convertPixelsToDialog", 2}, // 118
  {wxWindow_Destroy, "wxWindow", "'Destroy'", 1}, // 119
  {wxWindow_DestroyChildren, "wxWindow", "destroyChildren", 1}, // 120
  {wxWindow_Disable, "wxWindow", "disable", 1}, // 121
  {wxWindow_DragAcceptFiles, "wxWindow", "dragAcceptFiles", 2}, // 122
  {wxWindow_Enable, "wxWindow", "enable", 2}, // 123
  {wxWindow_FindFocus, "wxWindow", "findFocus", 0}, // 124
  {wxWindow_FindWindow_1_0, "wxWindow", "findWindow", 2}, // 125
  {wxWindow_FindWindow_1_1, "wxWindow", "findWindow", 2}, // 126
  {wxWindow_FindWindowById, "wxWindow", "findWindowById", 2}, // 127
  {wxWindow_FindWindowByName, "wxWindow", "findWindowByName", 2}, // 128
  {wxWindow_FindWindowByLabel, "wxWindow", "findWindowByLabel", 2}, // 129
  {wxWindow_Fit, "wxWindow", "fit", 1}, // 130
  {wxWindow_FitInside, "wxWindow", "fitInside", 1}, // 131
  {wxWindow_Freeze, "wxWindow", "freeze", 1}, // 132
  {wxWindow_GetAcceleratorTable, "wxWindow", "getAcceleratorTable", 1}, // 133
  {wxWindow_GetBackgroundColour, "wxWindow", "getBackgroundColour", 1}, // 134
  {wxWindow_GetBackgroundStyle, "wxWindow", "getBackgroundStyle", 1}, // 135
  {wxWindow_GetBestSize, "wxWindow", "getBestSize", 1}, // 136
  {wxWindow_GetCaret, "wxWindow", "getCaret", 1}, // 137
  {wxWindow_GetCapture, "wxWindow", "getCapture", 0}, // 138
  {wxWindow_GetCharHeight, "wxWindow", "getCharHeight", 1}, // 139
  {wxWindow_GetCharWidth, "wxWindow", "getCharWidth", 1}, // 140
  {NULL, "", "", 0}, // 141
  {wxWindow_GetChildren, "wxWindow", "getChildren", 1}, // 142
  {NULL, "", "", 0}, // 143
  {wxWindow_GetClientSize, "wxWindow", "getClientSize", 1}, // 144
  {wxWindow_GetContainingSizer, "wxWindow", "getContainingSizer", 1}, // 145
  {wxWindow_GetCursor, "wxWindow", "getCursor", 1}, // 146
  {wxWindow_GetDropTarget, "wxWindow", "getDropTarget", 1}, // 147
  {wxWindow_GetEventHandler, "wxWindow", "getEventHandler", 1}, // 148
  {wxWindow_GetExtraStyle, "wxWindow", "getExtraStyle", 1}, // 149
  {wxWindow_GetFont, "wxWindow", "getFont", 1}, // 150
  {wxWindow_GetForegroundColour, "wxWindow", "getForegroundColour", 1}, // 151
  {wxWindow_GetGrandParent, "wxWindow", "getGrandParent", 1}, // 152
  {wxWindow_GetHandle, "wxWindow", "getHandle", 1}, // 153
  {wxWindow_GetHelpText, "wxWindow", "getHelpText", 1}, // 154
  {wxWindow_GetId, "wxWindow", "getId", 1}, // 155
  {wxWindow_GetLabel, "wxWindow", "getLabel", 1}, // 156
  {wxWindow_GetMaxSize, "wxWindow", "getMaxSize", 1}, // 157
  {wxWindow_GetMinSize, "wxWindow", "getMinSize", 1}, // 158
  {wxWindow_GetName, "wxWindow", "getName", 1}, // 159
  {wxWindow_GetParent, "wxWindow", "getParent", 1}, // 160
  {NULL, "", "", 0}, // 161
  {wxWindow_GetPosition, "wxWindow", "getPosition", 1}, // 162
  {wxWindow_GetRect, "wxWindow", "getRect", 1}, // 163
  {NULL, "", "", 0}, // 164
  {wxWindow_GetScreenPosition, "wxWindow", "getScreenPosition", 1}, // 165
  {wxWindow_GetScreenRect, "wxWindow", "getScreenRect", 1}, // 166
  {wxWindow_GetScrollPos, "wxWindow", "getScrollPos", 2}, // 167
  {wxWindow_GetScrollRange, "wxWindow", "getScrollRange", 2}, // 168
  {wxWindow_GetScrollThumb, "wxWindow", "getScrollThumb", 2}, // 169
  {NULL, "", "", 0}, // 170
  {wxWindow_GetSize, "wxWindow", "getSize", 1}, // 171
  {wxWindow_GetSizer, "wxWindow", "getSizer", 1}, // 172
  {wxWindow_GetTextExtent, "wxWindow", "getTextExtent", 3}, // 173
  {wxWindow_GetToolTip, "wxWindow", "getToolTip", 1}, // 174
  {wxWindow_GetUpdateRegion, "wxWindow", "getUpdateRegion", 1}, // 175
  {wxWindow_GetVirtualSize, "wxWindow", "getVirtualSize", 1}, // 176
  {NULL, "", "", 0}, // 177
  {wxWindow_GetWindowStyleFlag, "wxWindow", "getWindowStyleFlag", 1}, // 178
  {wxWindow_GetWindowVariant, "wxWindow", "getWindowVariant", 1}, // 179
  {wxWindow_HasCapture, "wxWindow", "hasCapture", 1}, // 180
  {wxWindow_HasScrollbar, "wxWindow", "hasScrollbar", 2}, // 181
  {wxWindow_HasTransparentBackground, "wxWindow", "hasTransparentBackground", 1}, // 182
  {wxWindow_Hide, "wxWindow", "hide", 1}, // 183
  {wxWindow_InheritAttributes, "wxWindow", "inheritAttributes", 1}, // 184
  {wxWindow_InitDialog, "wxWindow", "initDialog", 1}, // 185
  {wxWindow_InvalidateBestSize, "wxWindow", "invalidateBestSize", 1}, // 186
  {wxWindow_IsEnabled, "wxWindow", "isEnabled", 1}, // 187
  {wxWindow_IsExposed_2, "wxWindow", "isExposed", 3}, // 188
  {wxWindow_IsExposed_1_0, "wxWindow", "isExposed", 2}, // 189
  {wxWindow_IsExposed_4, "wxWindow", "isExposed", 5}, // 190
  {wxWindow_IsExposed_1_1, "wxWindow", "isExposed", 2}, // 191
  {wxWindow_IsRetained, "wxWindow", "isRetained", 1}, // 192
  {wxWindow_IsShown, "wxWindow", "isShown", 1}, // 193
  {wxWindow_IsTopLevel, "wxWindow", "isTopLevel", 1}, // 194
  {wxWindow_IsShownOnScreen, "wxWindow", "isShownOnScreen", 1}, // 195
  {wxWindow_Layout, "wxWindow", "layout", 1}, // 196
  {wxWindow_LineDown, "wxWindow", "lineDown", 1}, // 197
  {wxWindow_LineUp, "wxWindow", "lineUp", 1}, // 198
  {wxWindow_Lower, "wxWindow", "lower", 1}, // 199
  {wxWindow_Move_3, "wxWindow", "move", 4}, // 200
  {wxWindow_Move_2, "wxWindow", "move", 3}, // 201
  {wxWindow_MoveAfterInTabOrder, "wxWindow", "moveAfterInTabOrder", 2}, // 202
  {wxWindow_MoveBeforeInTabOrder, "wxWindow", "moveBeforeInTabOrder", 2}, // 203
  {wxWindow_Navigate, "wxWindow", "navigate", 2}, // 204
  {wxWindow_PageDown, "wxWindow", "pageDown", 1}, // 205
  {wxWindow_PageUp, "wxWindow", "pageUp", 1}, // 206
  {wxWindow_PopEventHandler, "wxWindow", "popEventHandler", 2}, // 207
  {wxWindow_PopupMenu_2, "wxWindow", "popupMenu", 3}, // 208
  {wxWindow_PopupMenu_3, "wxWindow", "popupMenu", 4}, // 209
  {wxWindow_Raise, "wxWindow", "raise", 1}, // 210
  {wxWindow_Refresh, "wxWindow", "refresh", 2}, // 211
  {wxWindow_RefreshRect, "wxWindow", "refreshRect", 3}, // 212
  {wxWindow_ReleaseMouse, "wxWindow", "releaseMouse", 1}, // 213
  {wxWindow_RemoveChild, "wxWindow", "removeChild", 2}, // 214
  {wxWindow_Reparent, "wxWindow", "reparent", 2}, // 215
  {wxWindow_ScreenToClient_2, "wxWindow", "screenToClient", 1}, // 216
  {wxWindow_ScreenToClient_1, "wxWindow", "screenToClient", 2}, // 217
  {wxWindow_ScrollLines, "wxWindow", "scrollLines", 2}, // 218
  {wxWindow_ScrollPages, "wxWindow", "scrollPages", 2}, // 219
  {wxWindow_ScrollWindow, "wxWindow", "scrollWindow", 4}, // 220
  {wxWindow_SetAcceleratorTable, "wxWindow", "setAcceleratorTable", 2}, // 221
  {wxWindow_SetAutoLayout, "wxWindow", "setAutoLayout", 2}, // 222
  {wxWindow_SetBackgroundColour, "wxWindow", "setBackgroundColour", 2}, // 223
  {wxWindow_SetBackgroundStyle, "wxWindow", "setBackgroundStyle", 2}, // 224
  {wxWindow_SetCaret, "wxWindow", "setCaret", 2}, // 225
  {wxWindow_SetClientSize_2, "wxWindow", "setClientSize", 3}, // 226
  {wxWindow_SetClientSize_1_0, "wxWindow", "setClientSize", 2}, // 227
  {wxWindow_SetClientSize_1_1, "wxWindow", "setClientSize", 2}, // 228
  {wxWindow_SetContainingSizer, "wxWindow", "setContainingSizer", 2}, // 229
  {wxWindow_SetCursor, "wxWindow", "setCursor", 2}, // 230
  {wxWindow_SetMaxSize, "wxWindow", "setMaxSize", 2}, // 231
  {wxWindow_SetMinSize, "wxWindow", "setMinSize", 2}, // 232
  {wxWindow_SetOwnBackgroundColour, "wxWindow", "setOwnBackgroundColour", 2}, // 233
  {wxWindow_SetOwnFont, "wxWindow", "setOwnFont", 2}, // 234
  {wxWindow_SetOwnForegroundColour, "wxWindow", "setOwnForegroundColour", 2}, // 235
  {wxWindow_SetDropTarget, "wxWindow", "setDropTarget", 2}, // 236
  {wxWindow_SetExtraStyle, "wxWindow", "setExtraStyle", 2}, // 237
  {wxWindow_SetFocus, "wxWindow", "setFocus", 1}, // 238
  {wxWindow_SetFocusFromKbd, "wxWindow", "setFocusFromKbd", 1}, // 239
  {wxWindow_SetFont, "wxWindow", "setFont", 2}, // 240
  {wxWindow_SetForegroundColour, "wxWindow", "setForegroundColour", 2}, // 241
  {wxWindow_SetHelpText, "wxWindow", "setHelpText", 2}, // 242
  {wxWindow_SetId, "wxWindow", "setId", 2}, // 243
  {wxWindow_SetLabel, "wxWindow", "setLabel", 2}, // 244
  {wxWindow_SetName, "wxWindow", "setName", 2}, // 245
  {wxWindow_SetPalette, "wxWindow", "setPalette", 2}, // 246
  {wxWindow_SetScrollbar, "wxWindow", "setScrollbar", 6}, // 247
  {wxWindow_SetScrollPos, "wxWindow", "setScrollPos", 4}, // 248
  {wxWindow_SetSize_5, "wxWindow", "setSize", 6}, // 249
  {wxWindow_SetSize_2_1, "wxWindow", "setSize", 3}, // 250
  {wxWindow_SetSize_1, "wxWindow", "setSize", 2}, // 251
  {wxWindow_SetSize_2_0, "wxWindow", "setSize", 3}, // 252
  {wxWindow_SetSizeHints_2, "wxWindow", "setSizeHints", 3}, // 253
  {wxWindow_SetSizeHints_3, "wxWindow", "setSizeHints", 4}, // 254
  {wxWindow_SetSizer, "wxWindow", "setSizer", 3}, // 255
  {wxWindow_SetSizerAndFit, "wxWindow", "setSizerAndFit", 3}, // 256
  {wxWindow_SetThemeEnabled, "wxWindow", "setThemeEnabled", 2}, // 257
  {wxWindow_SetToolTip_1_0, "wxWindow", "setToolTip", 2}, // 258
  {wxWindow_SetToolTip_1_1, "wxWindow", "setToolTip", 2}, // 259
  {wxWindow_SetVirtualSize_2, "wxWindow", "setVirtualSize", 3}, // 260
  {wxWindow_SetVirtualSize_1, "wxWindow", "setVirtualSize", 2}, // 261
  {wxWindow_SetWindowStyle, "wxWindow", "setWindowStyle", 2}, // 262
  {wxWindow_SetWindowStyleFlag, "wxWindow", "setWindowStyleFlag", 2}, // 263
  {wxWindow_SetWindowVariant, "wxWindow", "setWindowVariant", 2}, // 264
  {wxWindow_ShouldInheritColours, "wxWindow", "shouldInheritColours", 1}, // 265
  {wxWindow_Show, "wxWindow", "show", 2}, // 266
  {wxWindow_Thaw, "wxWindow", "thaw", 1}, // 267
  {wxWindow_TransferDataFromWindow, "wxWindow", "transferDataFromWindow", 1}, // 268
  {wxWindow_TransferDataToWindow, "wxWindow", "transferDataToWindow", 1}, // 269
  {wxWindow_Update, "wxWindow", "update", 1}, // 270
  {wxWindow_UpdateWindowUI, "wxWindow", "updateWindowUI", 2}, // 271
  {wxWindow_Validate, "wxWindow", "validate", 1}, // 272
  {wxWindow_WarpPointer, "wxWindow", "warpPointer", 3}, // 273
  {wxWindow_SetTransparent, "wxWindow", "setTransparent", 2}, // 274
  {wxWindow_CanSetTransparent, "wxWindow", "canSetTransparent", 1}, // 275
  {wxWindow_IsDoubleBuffered, "wxWindow", "isDoubleBuffered", 1}, // 276
#if wxCHECK_VERSION(3,1,0) || (!defined(__WXMAC__) && wxCHECK_VERSION(3,0,0))
  {wxWindow_SetDoubleBuffered, "wxWindow", "setDoubleBuffered", 2}, // 277
#else
  {NULL, "wxWindow", "setDoubleBuffered", 0}, // 277
#endif
  {wxWindow_GetContentScaleFactor, "wxWindow", "getContentScaleFactor", 1}, // 278
#if wxCHECK_VERSION(3,1,3)
  {wxWindow_GetDPI, "wxWindow", "getDPI", 1}, // 279
#else
  {NULL, "wxWindow", "getDPI", 0}, // 279
#endif
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_FromDIP_1_1, "wxWindow", "fromDIP", 2}, // 280
#else
  {NULL, "wxWindow", "fromDIP", 0}, // 280
#endif
  {NULL, "", "", 0}, // 281
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_FromDIP_1_0, "wxWindow", "fromDIP", 2}, // 282
#else
  {NULL, "wxWindow", "fromDIP", 0}, // 282
#endif
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_FromDIP_2_1, "wxWindow", "fromDIP", 2}, // 283
#else
  {NULL, "wxWindow", "fromDIP", 0}, // 283
#endif
  {NULL, "", "", 0}, // 284
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_FromDIP_2_0, "wxWindow", "fromDIP", 2}, // 285
#else
  {NULL, "wxWindow", "fromDIP", 0}, // 285
#endif
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_ToDIP_1_1, "wxWindow", "toDIP", 2}, // 286
#else
  {NULL, "wxWindow", "toDIP", 0}, // 286
#endif
  {NULL, "", "", 0}, // 287
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_ToDIP_1_0, "wxWindow", "toDIP", 2}, // 288
#else
  {NULL, "wxWindow", "toDIP", 0}, // 288
#endif
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_ToDIP_2_1, "wxWindow", "toDIP", 2}, // 289
#else
  {NULL, "wxWindow", "toDIP", 0}, // 289
#endif
  {NULL, "", "", 0}, // 290
#if wxCHECK_VERSION(3,1,0)
  {wxWindow_ToDIP_2_0, "wxWindow", "toDIP", 2}, // 291
#else
  {NULL, "wxWindow", "toDIP", 0}, // 291
#endif
  {wxTopLevelWindow_GetIcon, "wxTopLevelWindow", "getIcon", 1}, // 292
  {wxTopLevelWindow_GetIcons, "wxTopLevelWindow", "getIcons", 1}, // 293
  {wxTopLevelWindow_GetTitle, "wxTopLevelWindow", "getTitle", 1}, // 294
  {wxTopLevelWindow_IsActive, "wxTopLevelWindow", "isActive", 1}, // 295
  {wxTopLevelWindow_Iconize, "wxTopLevelWindow", "iconize", 2}, // 296
  {wxTopLevelWindow_IsFullScreen, "wxTopLevelWindow", "isFullScreen", 1}, // 297
  {wxTopLevelWindow_IsIconized, "wxTopLevelWindow", "isIconized", 1}, // 298
  {wxTopLevelWindow_IsMaximized, "wxTopLevelWindow", "isMaximized", 1}, // 299
  {wxTopLevelWindow_Maximize, "wxTopLevelWindow", "maximize", 2}, // 300
  {wxTopLevelWindow_RequestUserAttention, "wxTopLevelWindow", "requestUserAttention", 2}, // 301
  {wxTopLevelWindow_SetIcon, "wxTopLevelWindow", "setIcon", 2}, // 302
  {wxTopLevelWindow_SetIcons, "wxTopLevelWindow", "setIcons", 2}, // 303
  {wxTopLevelWindow_CenterOnScreen, "wxTopLevelWindow", "centerOnScreen", 2}, // 304
  {wxTopLevelWindow_SetShape, "wxTopLevelWindow", "setShape", 2}, // 305
  {NULL, "", "", 0}, // 306
  {wxTopLevelWindow_SetTitle, "wxTopLevelWindow", "setTitle", 2}, // 307
  {wxTopLevelWindow_ShowFullScreen, "wxTopLevelWindow", "showFullScreen", 3}, // 308
  {wxFrame_new_0, "wxFrame", "new", 0}, // 309
  {wxFrame_new_4, "wxFrame", "new", 4}, // 310
  {NULL, "wxFrame", "destroy", 1}, // 311 obj destructor wxFrame_destruct
  {wxFrame_Create, "wxFrame", "create", 5}, // 312
  {wxFrame_CreateStatusBar, "wxFrame", "createStatusBar", 2}, // 313
  {wxFrame_CreateToolBar, "wxFrame", "createToolBar", 2}, // 314
  {wxFrame_GetClientAreaOrigin, "wxFrame", "getClientAreaOrigin", 1}, // 315
  {wxFrame_GetMenuBar, "wxFrame", "getMenuBar", 1}, // 316
  {wxFrame_GetStatusBar, "wxFrame", "getStatusBar", 1}, // 317
  {wxFrame_GetStatusBarPane, "wxFrame", "getStatusBarPane", 1}, // 318
  {wxFrame_GetToolBar, "wxFrame", "getToolBar", 1}, // 319
  {wxFrame_ProcessCommand, "wxFrame", "processCommand", 2}, // 320
  {wxFrame_SendSizeEvent, "wxFrame", "sendSizeEvent", 2}, // 321
  {wxFrame_SetMenuBar, "wxFrame", "setMenuBar", 2}, // 322
  {wxFrame_SetStatusBar, "wxFrame", "setStatusBar", 2}, // 323
  {wxFrame_SetStatusBarPane, "wxFrame", "setStatusBarPane", 2}, // 324
  {wxFrame_SetStatusText, "wxFrame", "setStatusText", 3}, // 325
  {wxFrame_SetStatusWidths, "wxFrame", "setStatusWidths", 2}, // 326
  {wxFrame_SetToolBar, "wxFrame", "setToolBar", 2}, // 327
  {wxMiniFrame_new_0, "wxMiniFrame", "new", 0}, // 328
  {wxMiniFrame_new_4, "wxMiniFrame", "new", 4}, // 329
  {NULL, "wxMiniFrame", "destroy", 1}, // 330 obj destructor wxMiniFrame_destruct
  {wxMiniFrame_Create, "wxMiniFrame", "create", 5}, // 331
  {wxSplashScreen_new, "wxSplashScreen", "new", 6}, // 332
  {NULL, "wxSplashScreen", "destroy", 1}, // 333 obj destructor wxSplashScreen_destruct
  {wxSplashScreen_GetSplashStyle, "wxSplashScreen", "getSplashStyle", 1}, // 334
  {wxSplashScreen_GetTimeout, "wxSplashScreen", "getTimeout", 1}, // 335
  {wxPanel_new_0, "wxPanel", "new", 0}, // 336
  {wxPanel_new_2, "wxPanel", "new", 2}, // 337
  {NULL, "wxPanel", "destroy", 1}, // 338 obj destructor wxPanel_destruct
  {wxPanel_InitDialog, "wxPanel", "initDialog", 1}, // 339
  {wxPanel_SetFocusIgnoringChildren, "wxPanel", "setFocusIgnoringChildren", 1}, // 340
  {wxScrolledWindow_new_0, "wxScrolledWindow", "new", 0}, // 341
  {wxScrolledWindow_new_2, "wxScrolledWindow", "new", 2}, // 342
  {wxScrolledWindow_CalcScrolledPosition_4, "wxScrolledWindow", "calcScrolledPosition", 3}, // 343
  {wxScrolledWindow_CalcScrolledPosition_1, "wxScrolledWindow", "calcScrolledPosition", 2}, // 344
  {wxScrolledWindow_CalcUnscrolledPosition_4, "wxScrolledWindow", "calcUnscrolledPosition", 3}, // 345
  {wxScrolledWindow_CalcUnscrolledPosition_1, "wxScrolledWindow", "calcUnscrolledPosition", 2}, // 346
  {wxScrolledWindow_EnableScrolling, "wxScrolledWindow", "enableScrolling", 3}, // 347
  {wxScrolledWindow_GetScrollPixelsPerUnit, "wxScrolledWindow", "getScrollPixelsPerUnit", 1}, // 348
  {NULL, "", "", 0}, // 349
  {wxScrolledWindow_GetViewStart, "wxScrolledWindow", "getViewStart", 1}, // 350
  {wxScrolledWindow_DoPrepareDC, "wxScrolledWindow", "doPrepareDC", 2}, // 351
  {wxScrolledWindow_PrepareDC, "wxScrolledWindow", "prepareDC", 2}, // 352
  {wxScrolledWindow_Scroll_2, "wxScrolledWindow", "scroll", 3}, // 353
  {wxScrolledWindow_Scroll_1, "wxScrolledWindow", "scroll", 2}, // 354
  {wxScrolledWindow_SetScrollbars, "wxScrolledWindow", "setScrollbars", 6}, // 355
  {wxScrolledWindow_SetScrollRate, "wxScrolledWindow", "setScrollRate", 3}, // 356
  {wxScrolledWindow_SetTargetWindow, "wxScrolledWindow", "setTargetWindow", 2}, // 357
  {NULL, "wxScrolledWindow", "'Destroy'", 1}, // 358 obj destructor wxScrolledWindow_destroy
  {wxSashWindow_new_0, "wxSashWindow", "new", 0}, // 359
  {wxSashWindow_new_2, "wxSashWindow", "new", 2}, // 360
  {NULL, "wxSashWindow", "destroy", 1}, // 361 obj destructor wxSashWindow_destruct
  {wxSashWindow_GetSashVisible, "wxSashWindow", "getSashVisible", 2}, // 362
  {wxSashWindow_GetMaximumSizeX, "wxSashWindow", "getMaximumSizeX", 1}, // 363
  {wxSashWindow_GetMaximumSizeY, "wxSashWindow", "getMaximumSizeY", 1}, // 364
  {wxSashWindow_GetMinimumSizeX, "wxSashWindow", "getMinimumSizeX", 1}, // 365
  {wxSashWindow_GetMinimumSizeY, "wxSashWindow", "getMinimumSizeY", 1}, // 366
  {wxSashWindow_SetMaximumSizeX, "wxSashWindow", "setMaximumSizeX", 2}, // 367
  {wxSashWindow_SetMaximumSizeY, "wxSashWindow", "setMaximumSizeY", 2}, // 368
  {wxSashWindow_SetMinimumSizeX, "wxSashWindow", "setMinimumSizeX", 2}, // 369
  {wxSashWindow_SetMinimumSizeY, "wxSashWindow", "setMinimumSizeY", 2}, // 370
  {wxSashWindow_SetSashVisible, "wxSashWindow", "setSashVisible", 3}, // 371
  {wxSashLayoutWindow_new_0, "wxSashLayoutWindow", "new", 0}, // 372
  {wxSashLayoutWindow_new_2, "wxSashLayoutWindow", "new", 2}, // 373
  {wxSashLayoutWindow_Create, "wxSashLayoutWindow", "create", 3}, // 374
  {wxSashLayoutWindow_GetAlignment, "wxSashLayoutWindow", "getAlignment", 1}, // 375
  {wxSashLayoutWindow_GetOrientation, "wxSashLayoutWindow", "getOrientation", 1}, // 376
  {wxSashLayoutWindow_SetAlignment, "wxSashLayoutWindow", "setAlignment", 2}, // 377
  {wxSashLayoutWindow_SetDefaultSize, "wxSashLayoutWindow", "setDefaultSize", 2}, // 378
  {wxSashLayoutWindow_SetOrientation, "wxSashLayoutWindow", "setOrientation", 2}, // 379
  {NULL, "wxSashLayoutWindow", "'Destroy'", 1}, // 380 obj destructor wxSashLayoutWindow_destroy
  {wxGrid_new_0, "wxGrid", "new", 0}, // 381
  {wxGrid_new_3, "wxGrid", "new", 3}, // 382
  {NULL, "wxGrid", "destroy", 1}, // 383 obj destructor wxGrid_destruct
  {wxGrid_AppendCols, "wxGrid", "appendCols", 2}, // 384
  {wxGrid_AppendRows, "wxGrid", "appendRows", 2}, // 385
  {wxGrid_AutoSize, "wxGrid", "autoSize", 1}, // 386
  {wxGrid_AutoSizeColumn, "wxGrid", "autoSizeColumn", 3}, // 387
  {wxGrid_AutoSizeColumns, "wxGrid", "autoSizeColumns", 2}, // 388
  {wxGrid_AutoSizeRow, "wxGrid", "autoSizeRow", 3}, // 389
  {wxGrid_AutoSizeRows, "wxGrid", "autoSizeRows", 2}, // 390
  {wxGrid_BeginBatch, "wxGrid", "beginBatch", 1}, // 391
  {wxGrid_BlockToDeviceRect, "wxGrid", "blockToDeviceRect", 3}, // 392
  {wxGrid_CanDragCell, "wxGrid", "canDragCell", 1}, // 393
  {wxGrid_CanDragColMove, "wxGrid", "canDragColMove", 1}, // 394
#if wxCHECK_VERSION(3,1,4)
  {wxGrid_CanDragGridRowEdges, "wxGrid", "canDragGridRowEdges", 1}, // 395
#else
  {NULL, "wxGrid", "canDragGridRowEdges", 0}, // 395
#endif
  {wxGrid_CanDragColSize, "wxGrid", "canDragColSize", 2}, // 396
  {wxGrid_CanDragRowSize, "wxGrid", "canDragRowSize", 2}, // 397
  {wxGrid_CanDragGridSize, "wxGrid", "canDragGridSize", 1}, // 398
  {wxGrid_CanEnableCellControl, "wxGrid", "canEnableCellControl", 1}, // 399
  {wxGrid_CellToRect_2, "wxGrid", "cellToRect", 3}, // 400
  {wxGrid_CellToRect_1, "wxGrid", "cellToRect", 2}, // 401
  {wxGrid_ClearGrid, "wxGrid", "clearGrid", 1}, // 402
  {wxGrid_ClearSelection, "wxGrid", "clearSelection", 1}, // 403
  {wxGrid_CreateGrid, "wxGrid", "createGrid", 4}, // 404
  {wxGrid_DeleteCols, "wxGrid", "deleteCols", 2}, // 405
  {wxGrid_DeleteRows, "wxGrid", "deleteRows", 2}, // 406
  {wxGrid_DisableCellEditControl, "wxGrid", "disableCellEditControl", 1}, // 407
  {wxGrid_DisableDragColSize, "wxGrid", "disableDragColSize", 1}, // 408
  {wxGrid_DisableDragGridSize, "wxGrid", "disableDragGridSize", 1}, // 409
  {wxGrid_DisableDragRowSize, "wxGrid", "disableDragRowSize", 1}, // 410
  {wxGrid_EnableCellEditControl, "wxGrid", "enableCellEditControl", 2}, // 411
  {wxGrid_EnableDragColSize, "wxGrid", "enableDragColSize", 2}, // 412
  {wxGrid_EnableDragGridSize, "wxGrid", "enableDragGridSize", 2}, // 413
  {wxGrid_EnableDragRowSize, "wxGrid", "enableDragRowSize", 2}, // 414
  {wxGrid_EnableEditing, "wxGrid", "enableEditing", 2}, // 415
  {wxGrid_EnableGridLines, "wxGrid", "enableGridLines", 2}, // 416
  {wxGrid_EndBatch, "wxGrid", "endBatch", 1}, // 417
  {wxGrid_Fit, "wxGrid", "fit", 1}, // 418
  {wxGrid_ForceRefresh, "wxGrid", "forceRefresh", 1}, // 419
  {wxGrid_GetBatchCount, "wxGrid", "getBatchCount", 1}, // 420
  {wxGrid_GetCellAlignment, "wxGrid", "getCellAlignment", 3}, // 421
  {wxGrid_GetCellBackgroundColour, "wxGrid", "getCellBackgroundColour", 3}, // 422
  {wxGrid_GetCellEditor, "wxGrid", "getCellEditor", 3}, // 423
  {wxGrid_GetCellFont, "wxGrid", "getCellFont", 3}, // 424
  {wxGrid_GetCellRenderer, "wxGrid", "getCellRenderer", 3}, // 425
  {wxGrid_GetCellTextColour, "wxGrid", "getCellTextColour", 3}, // 426
  {wxGrid_GetCellValue_2, "wxGrid", "getCellValue", 3}, // 427
  {wxGrid_GetCellValue_1, "wxGrid", "getCellValue", 2}, // 428
  {wxGrid_GetColLabelAlignment, "wxGrid", "getColLabelAlignment", 1}, // 429
  {wxGrid_GetColLabelSize, "wxGrid", "getColLabelSize", 1}, // 430
  {wxGrid_GetColLabelValue, "wxGrid", "getColLabelValue", 2}, // 431
  {wxGrid_GetColMinimalAcceptableWidth, "wxGrid", "getColMinimalAcceptableWidth", 1}, // 432
  {wxGrid_GetDefaultCellAlignment, "wxGrid", "getDefaultCellAlignment", 1}, // 433
  {wxGrid_GetDefaultCellBackgroundColour, "wxGrid", "getDefaultCellBackgroundColour", 1}, // 434
  {wxGrid_GetDefaultCellFont, "wxGrid", "getDefaultCellFont", 1}, // 435
  {wxGrid_GetDefaultCellTextColour, "wxGrid", "getDefaultCellTextColour", 1}, // 436
  {wxGrid_GetDefaultColLabelSize, "wxGrid", "getDefaultColLabelSize", 1}, // 437
  {wxGrid_GetDefaultColSize, "wxGrid", "getDefaultColSize", 1}, // 438
  {wxGrid_GetDefaultEditor, "wxGrid", "getDefaultEditor", 1}, // 439
  {wxGrid_GetDefaultEditorForCell_2, "wxGrid", "getDefaultEditorForCell", 3}, // 440
  {wxGrid_GetDefaultEditorForCell_1, "wxGrid", "getDefaultEditorForCell", 2}, // 441
  {wxGrid_GetDefaultEditorForType, "wxGrid", "getDefaultEditorForType", 2}, // 442
  {wxGrid_GetDefaultRenderer, "wxGrid", "getDefaultRenderer", 1}, // 443
  {wxGrid_GetDefaultRendererForCell, "wxGrid", "getDefaultRendererForCell", 3}, // 444
  {wxGrid_GetDefaultRendererForType, "wxGrid", "getDefaultRendererForType", 2}, // 445
  {wxGrid_GetDefaultRowLabelSize, "wxGrid", "getDefaultRowLabelSize", 1}, // 446
  {wxGrid_GetDefaultRowSize, "wxGrid", "getDefaultRowSize", 1}, // 447
  {wxGrid_GetGridCursorCol, "wxGrid", "getGridCursorCol", 1}, // 448
  {wxGrid_GetGridCursorRow, "wxGrid", "getGridCursorRow", 1}, // 449
  {wxGrid_GetGridLineColour, "wxGrid", "getGridLineColour", 1}, // 450
  {wxGrid_GridLinesEnabled, "wxGrid", "gridLinesEnabled", 1}, // 451
  {wxGrid_GetLabelBackgroundColour, "wxGrid", "getLabelBackgroundColour", 1}, // 452
  {wxGrid_GetLabelFont, "wxGrid", "getLabelFont", 1}, // 453
  {wxGrid_GetLabelTextColour, "wxGrid", "getLabelTextColour", 1}, // 454
  {wxGrid_GetNumberCols, "wxGrid", "getNumberCols", 1}, // 455
  {wxGrid_GetNumberRows, "wxGrid", "getNumberRows", 1}, // 456
  {wxGrid_GetOrCreateCellAttr, "wxGrid", "getOrCreateCellAttr", 3}, // 457
  {wxGrid_GetRowMinimalAcceptableHeight, "wxGrid", "getRowMinimalAcceptableHeight", 1}, // 458
  {wxGrid_GetRowLabelAlignment, "wxGrid", "getRowLabelAlignment", 1}, // 459
  {wxGrid_GetRowLabelSize, "wxGrid", "getRowLabelSize", 1}, // 460
  {wxGrid_GetRowLabelValue, "wxGrid", "getRowLabelValue", 2}, // 461
  {wxGrid_GetRowSize, "wxGrid", "getRowSize", 2}, // 462
  {wxGrid_GetScrollLineX, "wxGrid", "getScrollLineX", 1}, // 463
  {wxGrid_GetScrollLineY, "wxGrid", "getScrollLineY", 1}, // 464
  {wxGrid_GetSelectedCells, "wxGrid", "getSelectedCells", 1}, // 465
  {wxGrid_GetSelectedCols, "wxGrid", "getSelectedCols", 1}, // 466
  {wxGrid_GetSelectedRows, "wxGrid", "getSelectedRows", 1}, // 467
  {wxGrid_GetSelectionBackground, "wxGrid", "getSelectionBackground", 1}, // 468
  {wxGrid_GetSelectionBlockTopLeft, "wxGrid", "getSelectionBlockTopLeft", 1}, // 469
  {wxGrid_GetSelectionBlockBottomRight, "wxGrid", "getSelectionBlockBottomRight", 1}, // 470
  {wxGrid_GetSelectionForeground, "wxGrid", "getSelectionForeground", 1}, // 471
  {wxGrid_GetGridWindow, "wxGrid", "getGridWindow", 1}, // 472
  {wxGrid_GetGridRowLabelWindow, "wxGrid", "getGridRowLabelWindow", 1}, // 473
  {wxGrid_GetGridColLabelWindow, "wxGrid", "getGridColLabelWindow", 1}, // 474
  {wxGrid_GetGridCornerLabelWindow, "wxGrid", "getGridCornerLabelWindow", 1}, // 475
  {wxGrid_HideCellEditControl, "wxGrid", "hideCellEditControl", 1}, // 476
  {wxGrid_InsertCols, "wxGrid", "insertCols", 2}, // 477
  {wxGrid_InsertRows, "wxGrid", "insertRows", 2}, // 478
  {wxGrid_IsCellEditControlEnabled, "wxGrid", "isCellEditControlEnabled", 1}, // 479
  {wxGrid_IsCurrentCellReadOnly, "wxGrid", "isCurrentCellReadOnly", 1}, // 480
  {wxGrid_IsEditable, "wxGrid", "isEditable", 1}, // 481
  {wxGrid_IsInSelection_2, "wxGrid", "isInSelection", 3}, // 482
  {wxGrid_IsInSelection_1, "wxGrid", "isInSelection", 2}, // 483
  {wxGrid_IsReadOnly, "wxGrid", "isReadOnly", 3}, // 484
  {wxGrid_IsSelection, "wxGrid", "isSelection", 1}, // 485
  {wxGrid_IsVisible_3, "wxGrid", "isVisible", 4}, // 486
  {wxGrid_IsVisible_2, "wxGrid", "isVisible", 3}, // 487
  {wxGrid_MakeCellVisible_2, "wxGrid", "makeCellVisible", 3}, // 488
  {wxGrid_MakeCellVisible_1, "wxGrid", "makeCellVisible", 2}, // 489
  {wxGrid_MoveCursorDown, "wxGrid", "moveCursorDown", 2}, // 490
  {wxGrid_MoveCursorLeft, "wxGrid", "moveCursorLeft", 2}, // 491
  {wxGrid_MoveCursorRight, "wxGrid", "moveCursorRight", 2}, // 492
  {wxGrid_MoveCursorUp, "wxGrid", "moveCursorUp", 2}, // 493
  {wxGrid_MoveCursorDownBlock, "wxGrid", "moveCursorDownBlock", 2}, // 494
  {wxGrid_MoveCursorLeftBlock, "wxGrid", "moveCursorLeftBlock", 2}, // 495
  {wxGrid_MoveCursorRightBlock, "wxGrid", "moveCursorRightBlock", 2}, // 496
  {wxGrid_MoveCursorUpBlock, "wxGrid", "moveCursorUpBlock", 2}, // 497
  {wxGrid_MovePageDown, "wxGrid", "movePageDown", 1}, // 498
  {wxGrid_MovePageUp, "wxGrid", "movePageUp", 1}, // 499
  {wxGrid_RegisterDataType, "wxGrid", "registerDataType", 4}, // 500
  {wxGrid_SaveEditControlValue, "wxGrid", "saveEditControlValue", 1}, // 501
  {wxGrid_SelectAll, "wxGrid", "selectAll", 1}, // 502
  {wxGrid_SelectBlock_5, "wxGrid", "selectBlock", 6}, // 503
  {wxGrid_SelectBlock_3, "wxGrid", "selectBlock", 4}, // 504
  {wxGrid_SelectCol, "wxGrid", "selectCol", 3}, // 505
  {wxGrid_SelectRow, "wxGrid", "selectRow", 3}, // 506
  {wxGrid_SetCellAlignment, "wxGrid", "setCellAlignment", 5}, // 507
  {wxGrid_SetCellBackgroundColour, "wxGrid", "setCellBackgroundColour", 4}, // 508
  {wxGrid_SetCellEditor, "wxGrid", "setCellEditor", 4}, // 509
  {wxGrid_SetCellFont, "wxGrid", "setCellFont", 4}, // 510
  {wxGrid_SetCellRenderer, "wxGrid", "setCellRenderer", 4}, // 511
  {wxGrid_SetCellTextColour, "wxGrid", "setCellTextColour", 4}, // 512
  {wxGrid_SetCellValue_3, "wxGrid", "setCellValue", 4}, // 513
  {wxGrid_SetCellValue_2, "wxGrid", "setCellValue", 3}, // 514
  {wxGrid_SetColAttr, "wxGrid", "setColAttr", 3}, // 515
  {wxGrid_SetColFormatBool, "wxGrid", "setColFormatBool", 2}, // 516
  {wxGrid_SetColFormatNumber, "wxGrid", "setColFormatNumber", 2}, // 517
  {wxGrid_SetColFormatFloat, "wxGrid", "setColFormatFloat", 3}, // 518
  {wxGrid_SetColFormatCustom, "wxGrid", "setColFormatCustom", 3}, // 519
  {wxGrid_SetColLabelAlignment, "wxGrid", "setColLabelAlignment", 3}, // 520
  {wxGrid_SetColLabelSize, "wxGrid", "setColLabelSize", 2}, // 521
  {wxGrid_SetColLabelValue, "wxGrid", "setColLabelValue", 3}, // 522
  {wxGrid_SetColMinimalWidth, "wxGrid", "setColMinimalWidth", 3}, // 523
  {wxGrid_SetColMinimalAcceptableWidth, "wxGrid", "setColMinimalAcceptableWidth", 2}, // 524
  {wxGrid_SetColSize, "wxGrid", "setColSize", 3}, // 525
  {wxGrid_SetDefaultCellAlignment, "wxGrid", "setDefaultCellAlignment", 3}, // 526
  {wxGrid_SetDefaultCellBackgroundColour, "wxGrid", "setDefaultCellBackgroundColour", 2}, // 527
  {wxGrid_SetDefaultCellFont, "wxGrid", "setDefaultCellFont", 2}, // 528
  {wxGrid_SetDefaultCellTextColour, "wxGrid", "setDefaultCellTextColour", 2}, // 529
  {wxGrid_SetDefaultEditor, "wxGrid", "setDefaultEditor", 2}, // 530
  {wxGrid_SetDefaultRenderer, "wxGrid", "setDefaultRenderer", 2}, // 531
  {wxGrid_SetDefaultColSize, "wxGrid", "setDefaultColSize", 3}, // 532
  {wxGrid_SetDefaultRowSize, "wxGrid", "setDefaultRowSize", 3}, // 533
  {wxGrid_SetGridCursor_2, "wxGrid", "setGridCursor", 3}, // 534
  {wxGrid_SetGridCursor_1, "wxGrid", "setGridCursor", 2}, // 535
  {wxGrid_SetGridLineColour, "wxGrid", "setGridLineColour", 2}, // 536
  {wxGrid_SetLabelBackgroundColour, "wxGrid", "setLabelBackgroundColour", 2}, // 537
  {wxGrid_SetLabelFont, "wxGrid", "setLabelFont", 2}, // 538
  {wxGrid_SetLabelTextColour, "wxGrid", "setLabelTextColour", 2}, // 539
  {wxGrid_SetMargins, "wxGrid", "setMargins", 3}, // 540
  {wxGrid_SetReadOnly, "wxGrid", "setReadOnly", 4}, // 541
  {wxGrid_SetRowAttr, "wxGrid", "setRowAttr", 3}, // 542
  {wxGrid_SetRowLabelAlignment, "wxGrid", "setRowLabelAlignment", 3}, // 543
  {wxGrid_SetRowLabelSize, "wxGrid", "setRowLabelSize", 2}, // 544
  {wxGrid_SetRowLabelValue, "wxGrid", "setRowLabelValue", 3}, // 545
  {wxGrid_SetRowMinimalHeight, "wxGrid", "setRowMinimalHeight", 3}, // 546
  {wxGrid_SetRowMinimalAcceptableHeight, "wxGrid", "setRowMinimalAcceptableHeight", 2}, // 547
  {wxGrid_SetRowSize, "wxGrid", "setRowSize", 3}, // 548
  {wxGrid_SetScrollLineX, "wxGrid", "setScrollLineX", 2}, // 549
  {wxGrid_SetScrollLineY, "wxGrid", "setScrollLineY", 2}, // 550
  {wxGrid_SetSelectionBackground, "wxGrid", "setSelectionBackground", 2}, // 551
  {wxGrid_SetSelectionForeground, "wxGrid", "setSelectionForeground", 2}, // 552
  {wxGrid_SetSelectionMode, "wxGrid", "setSelectionMode", 2}, // 553
  {wxGrid_ShowCellEditControl, "wxGrid", "showCellEditControl", 1}, // 554
  {wxGrid_XToCol, "wxGrid", "xToCol", 3}, // 555
  {wxGrid_XToEdgeOfCol, "wxGrid", "xToEdgeOfCol", 2}, // 556
  {wxGrid_YToEdgeOfRow, "wxGrid", "yToEdgeOfRow", 2}, // 557
  {wxGrid_YToRow, "wxGrid", "yToRow", 3}, // 558
  {wxGridCellRenderer_Draw, "wxGridCellRenderer", "draw", 8}, // 559
  {wxGridCellRenderer_GetBestSize, "wxGridCellRenderer", "getBestSize", 6}, // 560
  {wxGridCellEditor_Create, "wxGridCellEditor", "create", 4}, // 561
  {wxGridCellEditor_IsCreated, "wxGridCellEditor", "isCreated", 1}, // 562
  {wxGridCellEditor_SetSize, "wxGridCellEditor", "setSize", 2}, // 563
  {wxGridCellEditor_Show, "wxGridCellEditor", "show", 3}, // 564
  {wxGridCellEditor_Reset, "wxGridCellEditor", "reset", 1}, // 565
  {wxGridCellEditor_StartingKey, "wxGridCellEditor", "startingKey", 2}, // 566
  {wxGridCellEditor_StartingClick, "wxGridCellEditor", "startingClick", 1}, // 567
  {wxGridCellEditor_HandleReturn, "wxGridCellEditor", "handleReturn", 2}, // 568
  {wxGridCellBoolRenderer_new, "wxGridCellBoolRenderer", "new", 0}, // 569
  {wxGridCellBoolRenderer_destroy, "wxGridCellBoolRenderer", "'Destroy'", 1}, // 570
  {wxGridCellBoolEditor_new, "wxGridCellBoolEditor", "new", 0}, // 571
  {wxGridCellBoolEditor_IsTrueValue, "wxGridCellBoolEditor", "isTrueValue", 1}, // 572
  {wxGridCellBoolEditor_UseStringValues, "wxGridCellBoolEditor", "useStringValues", 1}, // 573
  {wxGridCellBoolEditor_destroy, "wxGridCellBoolEditor", "'Destroy'", 1}, // 574
  {wxGridCellFloatRenderer_new, "wxGridCellFloatRenderer", "new", 1}, // 575
  {wxGridCellFloatRenderer_GetPrecision, "wxGridCellFloatRenderer", "getPrecision", 1}, // 576
  {wxGridCellFloatRenderer_GetWidth, "wxGridCellFloatRenderer", "getWidth", 1}, // 577
  {wxGridCellFloatRenderer_SetParameters, "wxGridCellFloatRenderer", "setParameters", 2}, // 578
  {wxGridCellFloatRenderer_SetPrecision, "wxGridCellFloatRenderer", "setPrecision", 2}, // 579
  {wxGridCellFloatRenderer_SetWidth, "wxGridCellFloatRenderer", "setWidth", 2}, // 580
  {wxGridCellFloatRenderer_destroy, "wxGridCellFloatRenderer", "'Destroy'", 1}, // 581
  {wxGridCellFloatEditor_new, "wxGridCellFloatEditor", "new", 1}, // 582
  {wxGridCellFloatEditor_SetParameters, "wxGridCellFloatEditor", "setParameters", 2}, // 583
  {wxGridCellFloatEditor_destroy, "wxGridCellFloatEditor", "'Destroy'", 1}, // 584
  {wxGridCellStringRenderer_new, "wxGridCellStringRenderer", "new", 0}, // 585
  {wxGridCellStringRenderer_destroy, "wxGridCellStringRenderer", "'Destroy'", 1}, // 586
  {wxGridCellTextEditor_new, "wxGridCellTextEditor", "new", 1}, // 587
  {wxGridCellTextEditor_SetParameters, "wxGridCellTextEditor", "setParameters", 2}, // 588
  {wxGridCellTextEditor_destroy, "wxGridCellTextEditor", "'Destroy'", 1}, // 589
  {NULL, "", "", 0}, // 590
  {wxGridCellChoiceEditor_new, "wxGridCellChoiceEditor", "new", 2}, // 591
  {wxGridCellChoiceEditor_SetParameters, "wxGridCellChoiceEditor", "setParameters", 2}, // 592
  {wxGridCellChoiceEditor_destroy, "wxGridCellChoiceEditor", "'Destroy'", 1}, // 593
  {wxGridCellNumberRenderer_new, "wxGridCellNumberRenderer", "new", 0}, // 594
  {wxGridCellNumberRenderer_destroy, "wxGridCellNumberRenderer", "'Destroy'", 1}, // 595
  {wxGridCellNumberEditor_new, "wxGridCellNumberEditor", "new", 1}, // 596
  {wxGridCellNumberEditor_GetValue, "wxGridCellNumberEditor", "getValue", 1}, // 597
  {wxGridCellNumberEditor_SetParameters, "wxGridCellNumberEditor", "setParameters", 2}, // 598
  {wxGridCellNumberEditor_destroy, "wxGridCellNumberEditor", "'Destroy'", 1}, // 599
  {wxGridCellAttr_SetTextColour, "wxGridCellAttr", "setTextColour", 2}, // 600
  {wxGridCellAttr_SetBackgroundColour, "wxGridCellAttr", "setBackgroundColour", 2}, // 601
  {wxGridCellAttr_SetFont, "wxGridCellAttr", "setFont", 2}, // 602
  {wxGridCellAttr_SetAlignment, "wxGridCellAttr", "setAlignment", 3}, // 603
  {wxGridCellAttr_SetReadOnly, "wxGridCellAttr", "setReadOnly", 2}, // 604
  {wxGridCellAttr_SetRenderer, "wxGridCellAttr", "setRenderer", 2}, // 605
  {wxGridCellAttr_SetEditor, "wxGridCellAttr", "setEditor", 2}, // 606
  {wxGridCellAttr_HasTextColour, "wxGridCellAttr", "hasTextColour", 1}, // 607
  {wxGridCellAttr_HasBackgroundColour, "wxGridCellAttr", "hasBackgroundColour", 1}, // 608
  {wxGridCellAttr_HasFont, "wxGridCellAttr", "hasFont", 1}, // 609
  {wxGridCellAttr_HasAlignment, "wxGridCellAttr", "hasAlignment", 1}, // 610
  {wxGridCellAttr_HasRenderer, "wxGridCellAttr", "hasRenderer", 1}, // 611
  {wxGridCellAttr_HasEditor, "wxGridCellAttr", "hasEditor", 1}, // 612
  {wxGridCellAttr_GetTextColour, "wxGridCellAttr", "getTextColour", 1}, // 613
  {wxGridCellAttr_GetBackgroundColour, "wxGridCellAttr", "getBackgroundColour", 1}, // 614
  {wxGridCellAttr_GetFont, "wxGridCellAttr", "getFont", 1}, // 615
  {wxGridCellAttr_GetAlignment, "wxGridCellAttr", "getAlignment", 1}, // 616
  {wxGridCellAttr_GetRenderer, "wxGridCellAttr", "getRenderer", 4}, // 617
  {wxGridCellAttr_GetEditor, "wxGridCellAttr", "getEditor", 4}, // 618
  {wxGridCellAttr_IsReadOnly, "wxGridCellAttr", "isReadOnly", 1}, // 619
  {wxGridCellAttr_SetDefAttr, "wxGridCellAttr", "setDefAttr", 2}, // 620
  {wxDC_Blit, "wxDC", "blit", 6}, // 621
  {wxDC_CalcBoundingBox, "wxDC", "calcBoundingBox", 3}, // 622
  {wxDC_Clear, "wxDC", "clear", 1}, // 623
  {wxDC_CrossHair, "wxDC", "crossHair", 2}, // 624
  {wxDC_DestroyClippingRegion, "wxDC", "destroyClippingRegion", 1}, // 625
  {wxDC_DeviceToLogicalX, "wxDC", "deviceToLogicalX", 2}, // 626
  {wxDC_DeviceToLogicalXRel, "wxDC", "deviceToLogicalXRel", 2}, // 627
  {wxDC_DeviceToLogicalY, "wxDC", "deviceToLogicalY", 2}, // 628
  {wxDC_DeviceToLogicalYRel, "wxDC", "deviceToLogicalYRel", 2}, // 629
  {wxDC_DrawArc, "wxDC", "drawArc", 4}, // 630
  {wxDC_DrawBitmap, "wxDC", "drawBitmap", 4}, // 631
  {wxDC_DrawCheckMark, "wxDC", "drawCheckMark", 2}, // 632
  {wxDC_DrawCircle, "wxDC", "drawCircle", 3}, // 633
  {NULL, "", "", 0}, // 634
  {wxDC_DrawEllipse_2, "wxDC", "drawEllipse", 3}, // 635
  {wxDC_DrawEllipse_1, "wxDC", "drawEllipse", 2}, // 636
  {wxDC_DrawEllipticArc, "wxDC", "drawEllipticArc", 5}, // 637
  {wxDC_DrawIcon, "wxDC", "drawIcon", 3}, // 638
  {wxDC_DrawLabel, "wxDC", "drawLabel", 4}, // 639
  {wxDC_DrawLine, "wxDC", "drawLine", 3}, // 640
  {wxDC_DrawLines, "wxDC", "drawLines", 3}, // 641
  {NULL, "", "", 0}, // 642
  {wxDC_DrawPolygon, "wxDC", "drawPolygon", 3}, // 643
  {NULL, "", "", 0}, // 644
  {wxDC_DrawPoint, "wxDC", "drawPoint", 2}, // 645
  {NULL, "", "", 0}, // 646
  {wxDC_DrawRectangle_2, "wxDC", "drawRectangle", 3}, // 647
  {wxDC_DrawRectangle_1, "wxDC", "drawRectangle", 2}, // 648
  {wxDC_DrawRotatedText, "wxDC", "drawRotatedText", 4}, // 649
  {NULL, "", "", 0}, // 650
  {wxDC_DrawRoundedRectangle_3, "wxDC", "drawRoundedRectangle", 4}, // 651
  {wxDC_DrawRoundedRectangle_2, "wxDC", "drawRoundedRectangle", 3}, // 652
  {wxDC_DrawText, "wxDC", "drawText", 3}, // 653
  {wxDC_EndDoc, "wxDC", "endDoc", 1}, // 654
  {wxDC_EndPage, "wxDC", "endPage", 1}, // 655
  {wxDC_FloodFill, "wxDC", "floodFill", 4}, // 656
  {wxDC_GetBackground, "wxDC", "getBackground", 1}, // 657
  {wxDC_GetBackgroundMode, "wxDC", "getBackgroundMode", 1}, // 658
  {wxDC_GetBrush, "wxDC", "getBrush", 1}, // 659
  {wxDC_GetCharHeight, "wxDC", "getCharHeight", 1}, // 660
  {wxDC_GetCharWidth, "wxDC", "getCharWidth", 1}, // 661
  {wxDC_GetClippingBox, "wxDC", "getClippingBox", 1}, // 662
  {wxDC_GetFont, "wxDC", "getFont", 1}, // 663
  {wxDC_GetLayoutDirection, "wxDC", "getLayoutDirection", 1}, // 664
  {wxDC_GetLogicalFunction, "wxDC", "getLogicalFunction", 1}, // 665
  {wxDC_GetMapMode, "wxDC", "getMapMode", 1}, // 666
  {wxDC_GetMultiLineTextExtent_4, "wxDC", "getMultiLineTextExtent", 3}, // 667
  {wxDC_GetMultiLineTextExtent_1, "wxDC", "getMultiLineTextExtent", 2}, // 668
  {wxDC_GetPartialTextExtents, "wxDC", "getPartialTextExtents", 2}, // 669
  {wxDC_GetPen, "wxDC", "getPen", 1}, // 670
  {wxDC_GetPixel, "wxDC", "getPixel", 2}, // 671
  {wxDC_GetPPI, "wxDC", "getPPI", 1}, // 672
  {NULL, "", "", 0}, // 673
  {wxDC_GetSize, "wxDC", "getSize", 1}, // 674
  {NULL, "", "", 0}, // 675
  {wxDC_GetSizeMM, "wxDC", "getSizeMM", 1}, // 676
  {wxDC_GetTextBackground, "wxDC", "getTextBackground", 1}, // 677
  {wxDC_GetTextExtent_4, "wxDC", "getTextExtent", 3}, // 678
  {wxDC_GetTextExtent_1, "wxDC", "getTextExtent", 2}, // 679
  {wxDC_GetTextForeground, "wxDC", "getTextForeground", 1}, // 680
  {wxDC_GetUserScale, "wxDC", "getUserScale", 1}, // 681
  {wxDC_GradientFillConcentric_3, "wxDC", "gradientFillConcentric", 4}, // 682
  {wxDC_GradientFillConcentric_4, "wxDC", "gradientFillConcentric", 5}, // 683
  {wxDC_GradientFillLinear, "wxDC", "gradientFillLinear", 5}, // 684
  {wxDC_LogicalToDeviceX, "wxDC", "logicalToDeviceX", 2}, // 685
  {wxDC_LogicalToDeviceXRel, "wxDC", "logicalToDeviceXRel", 2}, // 686
  {wxDC_LogicalToDeviceY, "wxDC", "logicalToDeviceY", 2}, // 687
  {wxDC_LogicalToDeviceYRel, "wxDC", "logicalToDeviceYRel", 2}, // 688
  {wxDC_MaxX, "wxDC", "maxX", 1}, // 689
  {wxDC_MaxY, "wxDC", "maxY", 1}, // 690
  {wxDC_MinX, "wxDC", "minX", 1}, // 691
  {wxDC_MinY, "wxDC", "minY", 1}, // 692
  {wxDC_IsOk, "wxDC", "isOk", 1}, // 693
  {wxDC_ResetBoundingBox, "wxDC", "resetBoundingBox", 1}, // 694
  {wxDC_SetAxisOrientation, "wxDC", "setAxisOrientation", 3}, // 695
  {wxDC_SetBackground, "wxDC", "setBackground", 2}, // 696
  {wxDC_SetBackgroundMode, "wxDC", "setBackgroundMode", 2}, // 697
  {wxDC_SetBrush, "wxDC", "setBrush", 2}, // 698
  {NULL, "", "", 0}, // 699
  {wxDC_SetClippingRegion_2, "wxDC", "setClippingRegion", 3}, // 700
  {wxDC_SetClippingRegion_1, "wxDC", "setClippingRegion", 2}, // 701
  {wxDC_SetDeviceOrigin, "wxDC", "setDeviceOrigin", 3}, // 702
  {wxDC_SetFont, "wxDC", "setFont", 2}, // 703
  {wxDC_SetLayoutDirection, "wxDC", "setLayoutDirection", 2}, // 704
  {wxDC_SetLogicalFunction, "wxDC", "setLogicalFunction", 2}, // 705
  {wxDC_SetMapMode, "wxDC", "setMapMode", 2}, // 706
  {wxDC_SetPalette, "wxDC", "setPalette", 2}, // 707
  {wxDC_SetPen, "wxDC", "setPen", 2}, // 708
  {wxDC_SetTextBackground, "wxDC", "setTextBackground", 2}, // 709
  {wxDC_SetTextForeground, "wxDC", "setTextForeground", 2}, // 710
  {wxDC_SetUserScale, "wxDC", "setUserScale", 3}, // 711
  {wxDC_StartDoc, "wxDC", "startDoc", 2}, // 712
  {wxDC_StartPage, "wxDC", "startPage", 1}, // 713
  {wxMirrorDC_new, "wxMirrorDC", "new", 2}, // 714
  {NULL, "wxMirrorDC", "'Destroy'", 1}, // 715 obj destructor wxMirrorDC_destroy
  {wxScreenDC_new, "wxScreenDC", "new", 0}, // 716
  {NULL, "wxScreenDC", "'Destroy'", 1}, // 717 obj destructor wxScreenDC_destroy
  {wxPostScriptDC_new_0, "wxPostScriptDC", "new", 0}, // 718
  {wxPostScriptDC_new_1, "wxPostScriptDC", "new", 1}, // 719
  {NULL, "wxPostScriptDC", "'Destroy'", 1}, // 720 obj destructor wxPostScriptDC_destroy
  {wxWindowDC_new, "wxWindowDC", "new", 1}, // 721
  {NULL, "wxWindowDC", "'Destroy'", 1}, // 722 obj destructor wxWindowDC_destroy
  {wxClientDC_new, "wxClientDC", "new", 1}, // 723
  {NULL, "wxClientDC", "'Destroy'", 1}, // 724 obj destructor wxClientDC_destroy
  {wxPaintDC_new, "wxPaintDC", "new", 1}, // 725
  {NULL, "wxPaintDC", "'Destroy'", 1}, // 726 obj destructor wxPaintDC_destroy
  {wxMemoryDC_new_0, "wxMemoryDC", "new", 0}, // 727
  {wxMemoryDC_new_1, "wxMemoryDC", "new", 1}, // 728
  {NULL, "", "", 0}, // 729
  {wxMemoryDC_SelectObject, "wxMemoryDC", "selectObject", 2}, // 730
  {wxMemoryDC_SelectObjectAsSource, "wxMemoryDC", "selectObjectAsSource", 2}, // 731
  {NULL, "wxMemoryDC", "'Destroy'", 1}, // 732 obj destructor wxMemoryDC_destroy
  {wxBufferedDC_new_0, "wxBufferedDC", "new", 0}, // 733
  {wxBufferedDC_new_3, "wxBufferedDC", "new", 3}, // 734
  {wxBufferedDC_new_2, "wxBufferedDC", "new", 2}, // 735
  {NULL, "wxBufferedDC", "destroy", 1}, // 736 obj destructor wxBufferedDC_destruct
  {wxBufferedDC_Init_3, "wxBufferedDC", "init", 4}, // 737
  {wxBufferedDC_Init_2, "wxBufferedDC", "init", 3}, // 738
  {wxBufferedPaintDC_new_3, "wxBufferedPaintDC", "new", 3}, // 739
  {wxBufferedPaintDC_new_2, "wxBufferedPaintDC", "new", 2}, // 740
  {NULL, "wxBufferedPaintDC", "destroy", 1}, // 741 obj destructor wxBufferedPaintDC_destruct
  {NULL, "wxGraphicsObject", "destroy", 1}, // 742 obj destructor wxGraphicsObject_destruct
  {wxGraphicsObject_GetRenderer, "wxGraphicsObject", "getRenderer", 1}, // 743
  {wxGraphicsObject_IsNull, "wxGraphicsObject", "isNull", 1}, // 744
  {NULL, "wxGraphicsContext", "destroy", 1}, // 745 obj destructor wxGraphicsContext_destruct
  {NULL, "", "", 0}, // 746
  {wxGraphicsContext_Create_STAT_1, "wxGraphicsContext", "create", 1}, // 747
  {NULL, "", "", 0}, // 748
  {NULL, "", "", 0}, // 749
  {wxGraphicsContext_Create_STAT_0, "wxGraphicsContext", "create", 0}, // 750
  {wxGraphicsContext_CreatePen, "wxGraphicsContext", "createPen", 2}, // 751
  {wxGraphicsContext_CreateBrush, "wxGraphicsContext", "createBrush", 2}, // 752
  {wxGraphicsContext_CreateRadialGradientBrush_7, "wxGraphicsContext", "createRadialGradientBrush", 8}, // 753
  {wxGraphicsContext_CreateRadialGradientBrush_6, "wxGraphicsContext", "createRadialGradientBrush", 7}, // 754
  {wxGraphicsContext_CreateLinearGradientBrush_6, "wxGraphicsContext", "createLinearGradientBrush", 7}, // 755
  {wxGraphicsContext_CreateLinearGradientBrush_5, "wxGraphicsContext", "createLinearGradientBrush", 6}, // 756
  {wxGraphicsContext_CreateFont_2, "wxGraphicsContext", "createFont", 3}, // 757
  {wxGraphicsContext_CreateFont_3, "wxGraphicsContext", "createFont", 4}, // 758
  {wxGraphicsContext_CreateMatrix, "wxGraphicsContext", "createMatrix", 2}, // 759
  {wxGraphicsContext_CreatePath, "wxGraphicsContext", "createPath", 1}, // 760
  {wxGraphicsContext_Clip_1, "wxGraphicsContext", "clip", 2}, // 761
  {wxGraphicsContext_Clip_4, "wxGraphicsContext", "clip", 5}, // 762
  {wxGraphicsContext_ResetClip, "wxGraphicsContext", "resetClip", 1}, // 763
  {wxGraphicsContext_DrawBitmap, "wxGraphicsContext", "drawBitmap", 6}, // 764
  {wxGraphicsContext_DrawEllipse, "wxGraphicsContext", "drawEllipse", 5}, // 765
  {wxGraphicsContext_DrawIcon, "wxGraphicsContext", "drawIcon", 6}, // 766
  {wxGraphicsContext_DrawLines, "wxGraphicsContext", "drawLines", 3}, // 767
  {wxGraphicsContext_DrawPath, "wxGraphicsContext", "drawPath", 3}, // 768
  {wxGraphicsContext_DrawRectangle, "wxGraphicsContext", "drawRectangle", 5}, // 769
  {wxGraphicsContext_DrawRoundedRectangle, "wxGraphicsContext", "drawRoundedRectangle", 6}, // 770
  {wxGraphicsContext_DrawText_3, "wxGraphicsContext", "drawText", 4}, // 771
  {wxGraphicsContext_DrawText_4_0, "wxGraphicsContext", "drawText", 5}, // 772
  {wxGraphicsContext_DrawText_4_1, "wxGraphicsContext", "drawText", 5}, // 773
  {wxGraphicsContext_DrawText_5, "wxGraphicsContext", "drawText", 6}, // 774
  {wxGraphicsContext_FillPath, "wxGraphicsContext", "fillPath", 3}, // 775
  {wxGraphicsContext_StrokePath, "wxGraphicsContext", "strokePath", 2}, // 776
  {wxGraphicsContext_GetPartialTextExtents, "wxGraphicsContext", "getPartialTextExtents", 2}, // 777
  {wxGraphicsContext_GetTextExtent, "wxGraphicsContext", "getTextExtent", 2}, // 778
  {wxGraphicsContext_Rotate, "wxGraphicsContext", "rotate", 2}, // 779
  {wxGraphicsContext_Scale, "wxGraphicsContext", "scale", 3}, // 780
  {wxGraphicsContext_Translate, "wxGraphicsContext", "translate", 3}, // 781
  {wxGraphicsContext_GetTransform, "wxGraphicsContext", "getTransform", 1}, // 782
  {wxGraphicsContext_SetTransform, "wxGraphicsContext", "setTransform", 2}, // 783
  {wxGraphicsContext_ConcatTransform, "wxGraphicsContext", "concatTransform", 2}, // 784
  {NULL, "", "", 0}, // 785
  {wxGraphicsContext_SetBrush, "wxGraphicsContext", "setBrush", 2}, // 786
  {wxGraphicsContext_SetFont_2, "wxGraphicsContext", "setFont", 3}, // 787
  {wxGraphicsContext_SetFont_1, "wxGraphicsContext", "setFont", 2}, // 788
  {wxGraphicsContext_SetPen, "wxGraphicsContext", "setPen", 2}, // 789
  {NULL, "", "", 0}, // 790
  {wxGraphicsContext_StrokeLine, "wxGraphicsContext", "strokeLine", 5}, // 791
  {NULL, "", "", 0}, // 792
  {wxGraphicsContext_StrokeLines, "wxGraphicsContext", "strokeLines", 2}, // 793
  {wxGraphicsMatrix_Concat, "wxGraphicsMatrix", "concat", 2}, // 794
  {NULL, "", "", 0}, // 795
  {wxGraphicsMatrix_Get, "wxGraphicsMatrix", "get", 1}, // 796
  {wxGraphicsMatrix_Invert, "wxGraphicsMatrix", "invert", 1}, // 797
  {wxGraphicsMatrix_IsEqual, "wxGraphicsMatrix", "isEqual", 2}, // 798
  {NULL, "", "", 0}, // 799
  {wxGraphicsMatrix_IsIdentity, "wxGraphicsMatrix", "isIdentity", 1}, // 800
  {wxGraphicsMatrix_Rotate, "wxGraphicsMatrix", "rotate", 2}, // 801
  {wxGraphicsMatrix_Scale, "wxGraphicsMatrix", "scale", 3}, // 802
  {wxGraphicsMatrix_Translate, "wxGraphicsMatrix", "translate", 3}, // 803
  {wxGraphicsMatrix_Set, "wxGraphicsMatrix", "set", 2}, // 804
  {wxGraphicsMatrix_TransformPoint, "wxGraphicsMatrix", "transformPoint", 1}, // 805
  {wxGraphicsMatrix_TransformDistance, "wxGraphicsMatrix", "transformDistance", 1}, // 806
  {wxGraphicsPath_MoveToPoint_2, "wxGraphicsPath", "moveToPoint", 3}, // 807
  {wxGraphicsPath_MoveToPoint_1, "wxGraphicsPath", "moveToPoint", 2}, // 808
  {wxGraphicsPath_AddArc_6, "wxGraphicsPath", "addArc", 7}, // 809
  {wxGraphicsPath_AddArc_5, "wxGraphicsPath", "addArc", 6}, // 810
  {wxGraphicsPath_AddArcToPoint, "wxGraphicsPath", "addArcToPoint", 6}, // 811
  {wxGraphicsPath_AddCircle, "wxGraphicsPath", "addCircle", 4}, // 812
  {wxGraphicsPath_AddCurveToPoint_6, "wxGraphicsPath", "addCurveToPoint", 7}, // 813
  {wxGraphicsPath_AddCurveToPoint_3, "wxGraphicsPath", "addCurveToPoint", 4}, // 814
  {wxGraphicsPath_AddEllipse, "wxGraphicsPath", "addEllipse", 5}, // 815
  {wxGraphicsPath_AddLineToPoint_2, "wxGraphicsPath", "addLineToPoint", 3}, // 816
  {wxGraphicsPath_AddLineToPoint_1, "wxGraphicsPath", "addLineToPoint", 2}, // 817
  {wxGraphicsPath_AddPath, "wxGraphicsPath", "addPath", 2}, // 818
  {wxGraphicsPath_AddQuadCurveToPoint, "wxGraphicsPath", "addQuadCurveToPoint", 5}, // 819
  {wxGraphicsPath_AddRectangle, "wxGraphicsPath", "addRectangle", 5}, // 820
  {wxGraphicsPath_AddRoundedRectangle, "wxGraphicsPath", "addRoundedRectangle", 6}, // 821
  {wxGraphicsPath_CloseSubpath, "wxGraphicsPath", "closeSubpath", 1}, // 822
  {wxGraphicsPath_Contains_2, "wxGraphicsPath", "contains", 3}, // 823
  {wxGraphicsPath_Contains_3, "wxGraphicsPath", "contains", 4}, // 824
  {wxGraphicsPath_GetBox, "wxGraphicsPath", "getBox", 1}, // 825
  {NULL, "", "", 0}, // 826
  {NULL, "", "", 0}, // 827
  {wxGraphicsPath_GetCurrentPoint, "wxGraphicsPath", "getCurrentPoint", 1}, // 828
  {wxGraphicsPath_Transform, "wxGraphicsPath", "transform", 2}, // 829
  {wxGraphicsRenderer_GetDefaultRenderer, "wxGraphicsRenderer", "getDefaultRenderer", 0}, // 830
  {NULL, "", "", 0}, // 831
  {wxGraphicsRenderer_CreateContext, "wxGraphicsRenderer", "createContext", 2}, // 832
  {NULL, "", "", 0}, // 833
  {wxGraphicsRenderer_CreateBrush, "wxGraphicsRenderer", "createBrush", 2}, // 834
  {wxGraphicsRenderer_CreateLinearGradientBrush, "wxGraphicsRenderer", "createLinearGradientBrush", 6}, // 835
  {wxGraphicsRenderer_CreateRadialGradientBrush, "wxGraphicsRenderer", "createRadialGradientBrush", 7}, // 836
  {wxGraphicsRenderer_CreateFont_2, "wxGraphicsRenderer", "createFont", 3}, // 837
  {wxGraphicsRenderer_CreateFont_3, "wxGraphicsRenderer", "createFont", 4}, // 838
  {wxGraphicsRenderer_CreateMatrix, "wxGraphicsRenderer", "createMatrix", 2}, // 839
  {wxGraphicsRenderer_CreatePath, "wxGraphicsRenderer", "createPath", 1}, // 840
  {wxGraphicsGradientStops_new, "wxGraphicsGradientStops", "new", 1}, // 841
  {wxGraphicsGradientStops_Item, "wxGraphicsGradientStops", "item", 2}, // 842
  {wxGraphicsGradientStops_GetCount, "wxGraphicsGradientStops", "getCount", 1}, // 843
  {wxGraphicsGradientStops_SetStartColour, "wxGraphicsGradientStops", "setStartColour", 2}, // 844
  {wxGraphicsGradientStops_GetStartColour, "wxGraphicsGradientStops", "getStartColour", 1}, // 845
  {wxGraphicsGradientStops_SetEndColour, "wxGraphicsGradientStops", "setEndColour", 2}, // 846
  {wxGraphicsGradientStops_GetEndColour, "wxGraphicsGradientStops", "getEndColour", 1}, // 847
  {wxGraphicsGradientStops_Add, "wxGraphicsGradientStops", "add", 3}, // 848
  {NULL, "wxGraphicsGradientStops", "'Destroy'", 1}, // 849 obj destructor wxGraphicsGradientStops_destroy
  {wxMenuBar_new_0, "wxMenuBar", "new", 0}, // 850
  {wxMenuBar_new_1, "wxMenuBar", "new", 1}, // 851
  {NULL, "", "", 0}, // 852
  {NULL, "wxMenuBar", "destroy", 1}, // 853 obj destructor wxMenuBar_destruct
  {wxMenuBar_Append, "wxMenuBar", "append", 3}, // 854
  {wxMenuBar_Check, "wxMenuBar", "check", 3}, // 855
  {wxMenuBar_Enable, "wxMenuBar", "enable", 3}, // 856
  {wxMenuBar_EnableTop, "wxMenuBar", "enableTop", 3}, // 857
  {wxMenuBar_FindMenu, "wxMenuBar", "findMenu", 2}, // 858
  {wxMenuBar_FindMenuItem, "wxMenuBar", "findMenuItem", 3}, // 859
  {wxMenuBar_FindItem, "wxMenuBar", "findItem", 2}, // 860
  {wxMenuBar_GetHelpString, "wxMenuBar", "getHelpString", 2}, // 861
  {wxMenuBar_GetLabel, "wxMenuBar", "getLabel", 2}, // 862
  {wxMenuBar_GetMenuLabel, "wxMenuBar", "getMenuLabel", 2}, // 863
  {wxMenuBar_GetMenuLabelText, "wxMenuBar", "getMenuLabelText", 2}, // 864
  {wxMenuBar_GetMenu, "wxMenuBar", "getMenu", 2}, // 865
  {wxMenuBar_GetMenuCount, "wxMenuBar", "getMenuCount", 1}, // 866
  {wxMenuBar_Insert, "wxMenuBar", "insert", 4}, // 867
  {wxMenuBar_IsChecked, "wxMenuBar", "isChecked", 2}, // 868
#if defined(__WXMAC__)
  {wxMenuBar_SetAutoWindowMenu, "wxMenuBar", "setAutoWindowMenu", 1}, // 869
#else
  {NULL, "wxMenuBar", "setAutoWindowMenu", 0}, // 869
#endif
#if defined(__WXMAC__)
  {wxMenuBar_GetAutoWindowMenu, "wxMenuBar", "getAutoWindowMenu", 0}, // 870
#else
  {NULL, "wxMenuBar", "getAutoWindowMenu", 0}, // 870
#endif
#if defined(__WXMAC__)
  {wxMenuBar_OSXGetAppleMenu, "wxMenuBar", "oSXGetAppleMenu", 1}, // 871
#else
  {NULL, "wxMenuBar", "oSXGetAppleMenu", 0}, // 871
#endif
  {wxMenuBar_IsEnabled, "wxMenuBar", "isEnabled", 2}, // 872
  {wxMenuBar_Remove, "wxMenuBar", "remove", 2}, // 873
  {wxMenuBar_Replace, "wxMenuBar", "replace", 4}, // 874
  {wxMenuBar_SetHelpString, "wxMenuBar", "setHelpString", 3}, // 875
  {wxMenuBar_SetLabel, "wxMenuBar", "setLabel", 3}, // 876
  {wxMenuBar_SetMenuLabel, "wxMenuBar", "setMenuLabel", 3}, // 877
  {wxControl_GetLabel, "wxControl", "getLabel", 1}, // 878
  {wxControl_SetLabel, "wxControl", "setLabel", 2}, // 879
  {wxControlWithItems_Append_1, "wxControlWithItems", "append", 2}, // 880
  {wxControlWithItems_Append_2, "wxControlWithItems", "append", 3}, // 881
  {wxControlWithItems_appendStrings_1, "wxControlWithItems", "appendStrings", 2}, // 882
  {wxControlWithItems_appendStrings_2, "wxControlWithItems", "appendStrings", 3}, // 883
  {wxControlWithItems_Clear, "wxControlWithItems", "clear", 1}, // 884
  {wxControlWithItems_Delete, "wxControlWithItems", "delete", 2}, // 885
  {wxControlWithItems_FindString, "wxControlWithItems", "findString", 3}, // 886
  {wxControlWithItems_getClientData, "wxControlWithItems", "getClientData", 2}, // 887
  {wxControlWithItems_setClientData, "wxControlWithItems", "setClientData", 3}, // 888
  {wxControlWithItems_GetCount, "wxControlWithItems", "getCount", 1}, // 889
  {wxControlWithItems_GetSelection, "wxControlWithItems", "getSelection", 1}, // 890
  {wxControlWithItems_GetString, "wxControlWithItems", "getString", 2}, // 891
  {wxControlWithItems_GetStringSelection, "wxControlWithItems", "getStringSelection", 1}, // 892
  {wxControlWithItems_Insert_2, "wxControlWithItems", "insert", 3}, // 893
  {wxControlWithItems_Insert_3, "wxControlWithItems", "insert", 4}, // 894
  {wxControlWithItems_insertStrings_2, "wxControlWithItems", "insertStrings", 3}, // 895
  {wxControlWithItems_insertStrings_3, "wxControlWithItems", "insertStrings", 4}, // 896
  {wxControlWithItems_IsEmpty, "wxControlWithItems", "isEmpty", 1}, // 897
  {wxControlWithItems_Select, "wxControlWithItems", "select", 2}, // 898
  {wxControlWithItems_SetSelection, "wxControlWithItems", "setSelection", 2}, // 899
  {wxControlWithItems_SetString, "wxControlWithItems", "setString", 3}, // 900
  {wxControlWithItems_SetStringSelection, "wxControlWithItems", "setStringSelection", 2}, // 901
  {wxMenu_new_0, "wxMenu", "new", 0}, // 902
  {wxMenu_new_1, "wxMenu", "new", 1}, // 903
  {wxMenu_new_2, "wxMenu", "new", 2}, // 904
  {NULL, "wxMenu", "destroy", 1}, // 905 obj destructor wxMenu_destruct
  {wxMenu_Append_3, "wxMenu", "append", 4}, // 906
  {wxMenu_Append_4, "wxMenu", "append", 5}, // 907
  {wxMenu_Append_1, "wxMenu", "append", 2}, // 908
  {wxMenu_AppendCheckItem, "wxMenu", "appendCheckItem", 4}, // 909
  {wxMenu_AppendRadioItem, "wxMenu", "appendRadioItem", 4}, // 910
  {wxMenu_AppendSeparator, "wxMenu", "appendSeparator", 1}, // 911
  {wxMenu_Break, "wxMenu", "break", 1}, // 912
  {wxMenu_Check, "wxMenu", "check", 3}, // 913
  {wxMenu_Delete_1_0, "wxMenu", "delete", 2}, // 914
  {wxMenu_Delete_1_1, "wxMenu", "delete", 2}, // 915
  {wxMenu_Destroy_1_0, "wxMenu", "'Destroy'", 2}, // 916
  {wxMenu_Destroy_1_1, "wxMenu", "'Destroy'", 2}, // 917
  {wxMenu_Enable, "wxMenu", "enable", 3}, // 918
  {wxMenu_FindItem_1, "wxMenu", "findItem", 2}, // 919
  {wxMenu_FindItem_2, "wxMenu", "findItem", 2}, // 920
  {wxMenu_FindItemByPosition, "wxMenu", "findItemByPosition", 2}, // 921
  {wxMenu_GetHelpString, "wxMenu", "getHelpString", 2}, // 922
  {wxMenu_GetLabel, "wxMenu", "getLabel", 2}, // 923
  {wxMenu_GetMenuItemCount, "wxMenu", "getMenuItemCount", 1}, // 924
  {NULL, "", "", 0}, // 925
  {wxMenu_GetMenuItems, "wxMenu", "getMenuItems", 1}, // 926
  {wxMenu_GetTitle, "wxMenu", "getTitle", 1}, // 927
  {wxMenu_Insert_2, "wxMenu", "insert", 3}, // 928
  {wxMenu_Insert_3, "wxMenu", "insert", 4}, // 929
  {wxMenu_Insert_5, "wxMenu", "insert", 6}, // 930
  {wxMenu_InsertCheckItem, "wxMenu", "insertCheckItem", 5}, // 931
  {wxMenu_InsertRadioItem, "wxMenu", "insertRadioItem", 5}, // 932
  {wxMenu_InsertSeparator, "wxMenu", "insertSeparator", 2}, // 933
  {wxMenu_IsChecked, "wxMenu", "isChecked", 2}, // 934
  {wxMenu_IsEnabled, "wxMenu", "isEnabled", 2}, // 935
  {wxMenu_Prepend_1, "wxMenu", "prepend", 2}, // 936
  {wxMenu_Prepend_2, "wxMenu", "prepend", 3}, // 937
  {wxMenu_Prepend_4, "wxMenu", "prepend", 5}, // 938
  {wxMenu_PrependCheckItem, "wxMenu", "prependCheckItem", 4}, // 939
  {wxMenu_PrependRadioItem, "wxMenu", "prependRadioItem", 4}, // 940
  {wxMenu_PrependSeparator, "wxMenu", "prependSeparator", 1}, // 941
  {wxMenu_Remove_1_0, "wxMenu", "remove", 2}, // 942
  {wxMenu_Remove_1_1, "wxMenu", "remove", 2}, // 943
  {wxMenu_SetHelpString, "wxMenu", "setHelpString", 3}, // 944
  {wxMenu_SetLabel, "wxMenu", "setLabel", 3}, // 945
  {wxMenu_SetTitle, "wxMenu", "setTitle", 2}, // 946
  {wxMenuItem_new, "wxMenuItem", "new", 1}, // 947
  {NULL, "wxMenuItem", "destroy", 1}, // 948 obj destructor wxMenuItem_destruct
  {wxMenuItem_Check, "wxMenuItem", "check", 2}, // 949
  {wxMenuItem_Enable, "wxMenuItem", "enable", 2}, // 950
  {wxMenuItem_GetBitmap, "wxMenuItem", "getBitmap", 1}, // 951
  {wxMenuItem_GetHelp, "wxMenuItem", "getHelp", 1}, // 952
  {wxMenuItem_GetId, "wxMenuItem", "getId", 1}, // 953
  {wxMenuItem_GetKind, "wxMenuItem", "getKind", 1}, // 954
  {wxMenuItem_GetLabelText, "wxMenuItem", "getLabelText", 1}, // 955
  {wxMenuItem_GetItemLabel, "wxMenuItem", "getItemLabel", 1}, // 956
  {wxMenuItem_GetItemLabelText, "wxMenuItem", "getItemLabelText", 1}, // 957
  {wxMenuItem_GetMenu, "wxMenuItem", "getMenu", 1}, // 958
  {wxMenuItem_GetSubMenu, "wxMenuItem", "getSubMenu", 1}, // 959
  {wxMenuItem_IsCheckable, "wxMenuItem", "isCheckable", 1}, // 960
  {wxMenuItem_IsChecked, "wxMenuItem", "isChecked", 1}, // 961
  {wxMenuItem_IsEnabled, "wxMenuItem", "isEnabled", 1}, // 962
  {wxMenuItem_IsSeparator, "wxMenuItem", "isSeparator", 1}, // 963
  {wxMenuItem_IsSubMenu, "wxMenuItem", "isSubMenu", 1}, // 964
  {wxMenuItem_SetBitmap, "wxMenuItem", "setBitmap", 2}, // 965
  {wxMenuItem_SetHelp, "wxMenuItem", "setHelp", 2}, // 966
  {wxMenuItem_SetMenu, "wxMenuItem", "setMenu", 2}, // 967
  {wxMenuItem_SetSubMenu, "wxMenuItem", "setSubMenu", 2}, // 968
  {wxMenuItem_SetItemLabel, "wxMenuItem", "setItemLabel", 2}, // 969
  {wxToolBar_AddControl, "wxToolBar", "addControl", 3}, // 970
  {wxToolBar_AddSeparator, "wxToolBar", "addSeparator", 1}, // 971
  {wxToolBar_AddTool_1, "wxToolBar", "addTool", 2}, // 972
  {wxToolBar_AddTool_4, "wxToolBar", "addTool", 5}, // 973
  {wxToolBar_AddTool_5, "wxToolBar", "addTool", 6}, // 974
  {wxToolBar_AddCheckTool, "wxToolBar", "addCheckTool", 5}, // 975
  {wxToolBar_AddRadioTool, "wxToolBar", "addRadioTool", 5}, // 976
  {wxToolBar_AddStretchableSpace, "wxToolBar", "addStretchableSpace", 1}, // 977
  {wxToolBar_InsertStretchableSpace, "wxToolBar", "insertStretchableSpace", 2}, // 978
  {wxToolBar_DeleteTool, "wxToolBar", "deleteTool", 2}, // 979
  {wxToolBar_DeleteToolByPos, "wxToolBar", "deleteToolByPos", 2}, // 980
  {wxToolBar_EnableTool, "wxToolBar", "enableTool", 3}, // 981
  {wxToolBar_FindById, "wxToolBar", "findById", 2}, // 982
  {wxToolBar_FindControl, "wxToolBar", "findControl", 2}, // 983
  {wxToolBar_FindToolForPosition, "wxToolBar", "findToolForPosition", 3}, // 984
  {wxToolBar_GetToolSize, "wxToolBar", "getToolSize", 1}, // 985
  {wxToolBar_GetToolBitmapSize, "wxToolBar", "getToolBitmapSize", 1}, // 986
  {wxToolBar_GetMargins, "wxToolBar", "getMargins", 1}, // 987
  {wxToolBar_GetToolEnabled, "wxToolBar", "getToolEnabled", 2}, // 988
  {wxToolBar_GetToolLongHelp, "wxToolBar", "getToolLongHelp", 2}, // 989
  {wxToolBar_GetToolPacking, "wxToolBar", "getToolPacking", 1}, // 990
  {wxToolBar_GetToolPos, "wxToolBar", "getToolPos", 2}, // 991
  {wxToolBar_GetToolSeparation, "wxToolBar", "getToolSeparation", 1}, // 992
  {wxToolBar_GetToolShortHelp, "wxToolBar", "getToolShortHelp", 2}, // 993
  {wxToolBar_GetToolState, "wxToolBar", "getToolState", 2}, // 994
  {wxToolBar_InsertControl, "wxToolBar", "insertControl", 4}, // 995
  {wxToolBar_InsertSeparator, "wxToolBar", "insertSeparator", 2}, // 996
  {wxToolBar_InsertTool_5, "wxToolBar", "insertTool", 6}, // 997
  {wxToolBar_InsertTool_2, "wxToolBar", "insertTool", 3}, // 998
  {wxToolBar_Realize, "wxToolBar", "realize", 1}, // 999
  {wxToolBar_RemoveTool, "wxToolBar", "removeTool", 2}, // 1000
  {wxToolBar_SetMargins, "wxToolBar", "setMargins", 3}, // 1001
  {wxToolBar_SetToolBitmapSize, "wxToolBar", "setToolBitmapSize", 2}, // 1002
  {wxToolBar_SetToolLongHelp, "wxToolBar", "setToolLongHelp", 3}, // 1003
  {wxToolBar_SetToolPacking, "wxToolBar", "setToolPacking", 2}, // 1004
  {wxToolBar_SetToolShortHelp, "wxToolBar", "setToolShortHelp", 3}, // 1005
  {wxToolBar_SetToolSeparation, "wxToolBar", "setToolSeparation", 2}, // 1006
  {wxToolBar_ToggleTool, "wxToolBar", "toggleTool", 3}, // 1007
  {wxStatusBar_new_0, "wxStatusBar", "new", 0}, // 1008
  {wxStatusBar_new_2, "wxStatusBar", "new", 2}, // 1009
  {NULL, "wxStatusBar", "destroy", 1}, // 1010 obj destructor wxStatusBar_destruct
  {wxStatusBar_Create, "wxStatusBar", "create", 3}, // 1011
  {wxStatusBar_GetFieldRect, "wxStatusBar", "getFieldRect", 2}, // 1012
  {wxStatusBar_GetFieldsCount, "wxStatusBar", "getFieldsCount", 1}, // 1013
  {wxStatusBar_GetStatusText, "wxStatusBar", "getStatusText", 2}, // 1014
  {wxStatusBar_PopStatusText, "wxStatusBar", "popStatusText", 2}, // 1015
  {wxStatusBar_PushStatusText, "wxStatusBar", "pushStatusText", 3}, // 1016
  {wxStatusBar_SetFieldsCount, "wxStatusBar", "setFieldsCount", 3}, // 1017
  {wxStatusBar_SetMinHeight, "wxStatusBar", "setMinHeight", 2}, // 1018
  {wxStatusBar_SetStatusText, "wxStatusBar", "setStatusText", 3}, // 1019
  {wxStatusBar_SetStatusWidths, "wxStatusBar", "setStatusWidths", 2}, // 1020
  {wxStatusBar_SetStatusStyles, "wxStatusBar", "setStatusStyles", 2}, // 1021
  {wxBitmap_new_0, "wxBitmap", "new", 0}, // 1022
  {NULL, "", "", 0}, // 1023
  {wxBitmap_new_4, "wxBitmap", "new", 4}, // 1024
  {wxBitmap_new_3, "wxBitmap", "new", 3}, // 1025
  {wxBitmap_new_2_1, "wxBitmap", "new", 2}, // 1026
  {wxBitmap_new_2_0, "wxBitmap", "new", 2}, // 1027
  {wxBitmap_new_2_2, "wxBitmap", "new", 2}, // 1028
  {wxBitmap_new_2_3, "wxBitmap", "new", 1}, // 1029
  {NULL, "wxBitmap", "destroy", 1}, // 1030 obj destructor wxBitmap_destruct
  {wxBitmap_ConvertToImage, "wxBitmap", "convertToImage", 1}, // 1031
  {wxBitmap_CopyFromIcon, "wxBitmap", "copyFromIcon", 2}, // 1032
  {wxBitmap_Create_3_0, "wxBitmap", "create", 4}, // 1033
  {wxBitmap_Create_2, "wxBitmap", "create", 3}, // 1034
  {wxBitmap_Create_3_1, "wxBitmap", "create", 4}, // 1035
  {wxBitmap_GetDepth, "wxBitmap", "getDepth", 1}, // 1036
  {wxBitmap_GetHeight, "wxBitmap", "getHeight", 1}, // 1037
  {wxBitmap_GetPalette, "wxBitmap", "getPalette", 1}, // 1038
  {wxBitmap_GetMask, "wxBitmap", "getMask", 1}, // 1039
  {wxBitmap_GetWidth, "wxBitmap", "getWidth", 1}, // 1040
  {wxBitmap_GetSubBitmap, "wxBitmap", "getSubBitmap", 2}, // 1041
  {wxBitmap_LoadFile, "wxBitmap", "loadFile", 3}, // 1042
  {wxBitmap_IsOk, "wxBitmap", "isOk", 1}, // 1043
  {wxBitmap_SaveFile, "wxBitmap", "saveFile", 4}, // 1044
  {wxBitmap_SetDepth, "wxBitmap", "setDepth", 2}, // 1045
  {wxBitmap_SetHeight, "wxBitmap", "setHeight", 2}, // 1046
  {wxBitmap_SetMask, "wxBitmap", "setMask", 2}, // 1047
  {wxBitmap_SetPalette, "wxBitmap", "setPalette", 2}, // 1048
  {wxBitmap_SetWidth, "wxBitmap", "setWidth", 2}, // 1049
  {wxIcon_new_0, "wxIcon", "new", 0}, // 1050
  {wxIcon_new_1, "wxIcon", "new", 1}, // 1051
  {wxIcon_new_2, "wxIcon", "new", 2}, // 1052
  {wxIcon_CopyFromBitmap, "wxIcon", "copyFromBitmap", 2}, // 1053
  {NULL, "wxIcon", "destroy", 1}, // 1054 obj destructor wxIcon_destruct
  {wxIconBundle_new_0, "wxIconBundle", "new", 0}, // 1055
  {wxIconBundle_new_1_1, "wxIconBundle", "new", 1}, // 1056
  {wxIconBundle_new_2, "wxIconBundle", "new", 2}, // 1057
  {NULL, "", "", 0}, // 1058
  {wxIconBundle_new_1_0, "wxIconBundle", "new", 1}, // 1059
  {wxIconBundle_destruct, "wxIconBundle", "destroy", 1}, // 1060
  {wxIconBundle_AddIcon_1_0, "wxIconBundle", "addIcon", 2}, // 1061
  {wxIconBundle_AddIcon_2, "wxIconBundle", "addIcon", 3}, // 1062
  {wxIconBundle_AddIcon_1_1, "wxIconBundle", "addIcon", 2}, // 1063
  {wxIconBundle_GetIcon_2, "wxIconBundle", "getIcon", 3}, // 1064
  {wxIconBundle_GetIcon_1, "wxIconBundle", "getIcon", 2}, // 1065
  {wxCursor_new_0, "wxCursor", "new", 0}, // 1066
  {wxCursor_new_2, "wxCursor", "new", 2}, // 1067
  {wxCursor_new_1_1, "wxCursor", "new", 1}, // 1068
  {wxCursor_new_1_0, "wxCursor", "new", 1}, // 1069
  {NULL, "", "", 0}, // 1070
  {NULL, "wxCursor", "destroy", 1}, // 1071 obj destructor wxCursor_destruct
  {wxCursor_IsOk, "wxCursor", "isOk", 1}, // 1072
  {wxMask_new_0, "wxMask", "new", 0}, // 1073
  {wxMask_new_2_0, "wxMask", "new", 2}, // 1074
  {wxMask_new_1, "wxMask", "new", 1}, // 1075
  {wxMask_new_2_1, "wxMask", "new", 2}, // 1076
  {NULL, "wxMask", "destroy", 1}, // 1077 obj destructor wxMask_destruct
  {wxMask_Create_2_0, "wxMask", "create", 3}, // 1078
  {wxMask_Create_1, "wxMask", "create", 2}, // 1079
  {wxMask_Create_2_1, "wxMask", "create", 3}, // 1080
  {wxImage_new_0, "wxImage", "new", 0}, // 1081
  {wxImage_new_3_0, "wxImage", "new", 3}, // 1082
  {wxImage_new_2_1, "wxImage", "new", 2}, // 1083
  {wxImage_new_4_0, "wxImage", "new", 4}, // 1084
  {wxImage_new_3_2, "wxImage", "new", 3}, // 1085
  {wxImage_new_5, "wxImage", "new", 5}, // 1086
  {wxImage_new_4_1, "wxImage", "new", 4}, // 1087
  {wxImage_new_2_0, "wxImage", "new", 2}, // 1088
  {wxImage_new_3_1, "wxImage", "new", 3}, // 1089
  {NULL, "wxImage", "destroy", 1}, // 1090 obj destructor wxImage_destruct
  {wxImage_Blur, "wxImage", "blur", 2}, // 1091
  {wxImage_BlurHorizontal, "wxImage", "blurHorizontal", 2}, // 1092
  {wxImage_BlurVertical, "wxImage", "blurVertical", 2}, // 1093
  {wxImage_ConvertAlphaToMask_1, "wxImage", "convertAlphaToMask", 2}, // 1094
  {wxImage_ConvertAlphaToMask_4, "wxImage", "convertAlphaToMask", 5}, // 1095
  {wxImage_ConvertToGreyscale_3, "wxImage", "convertToGreyscale", 4}, // 1096
  {wxImage_ConvertToGreyscale_0, "wxImage", "convertToGreyscale", 1}, // 1097
  {wxImage_ConvertToMono, "wxImage", "convertToMono", 4}, // 1098
  {wxImage_Copy, "wxImage", "copy", 1}, // 1099
  {wxImage_Create_3_0, "wxImage", "create", 4}, // 1100
  {wxImage_Create_2, "wxImage", "create", 3}, // 1101
  {wxImage_Create_4_0, "wxImage", "create", 5}, // 1102
  {wxImage_Create_3_1, "wxImage", "create", 4}, // 1103
  {wxImage_Create_5, "wxImage", "create", 6}, // 1104
  {wxImage_Create_4_1, "wxImage", "create", 5}, // 1105
  {wxImage_Destroy, "wxImage", "'Destroy'", 1}, // 1106
  {wxImage_FindFirstUnusedColour, "wxImage", "findFirstUnusedColour", 2}, // 1107
  {wxImage_GetImageExtWildcard, "wxImage", "getImageExtWildcard", 0}, // 1108
  {wxImage_GetAlpha_0, "wxImage", "getAlpha", 1}, // 1109
  {wxImage_GetAlpha_2, "wxImage", "getAlpha", 3}, // 1110
  {wxImage_GetBlue, "wxImage", "getBlue", 3}, // 1111
  {wxImage_GetData, "wxImage", "getData", 1}, // 1112
  {wxImage_GetGreen, "wxImage", "getGreen", 3}, // 1113
  {wxImage_GetImageCount, "wxImage", "getImageCount", 2}, // 1114
  {wxImage_GetHeight, "wxImage", "getHeight", 1}, // 1115
  {wxImage_GetMaskBlue, "wxImage", "getMaskBlue", 1}, // 1116
  {wxImage_GetMaskGreen, "wxImage", "getMaskGreen", 1}, // 1117
  {wxImage_GetMaskRed, "wxImage", "getMaskRed", 1}, // 1118
  {wxImage_GetOrFindMaskColour, "wxImage", "getOrFindMaskColour", 1}, // 1119
  {wxImage_GetPalette, "wxImage", "getPalette", 1}, // 1120
  {wxImage_GetRed, "wxImage", "getRed", 3}, // 1121
  {wxImage_GetSubImage, "wxImage", "getSubImage", 2}, // 1122
  {wxImage_GetWidth, "wxImage", "getWidth", 1}, // 1123
  {wxImage_HasAlpha, "wxImage", "hasAlpha", 1}, // 1124
  {wxImage_HasMask, "wxImage", "hasMask", 1}, // 1125
  {wxImage_GetOption, "wxImage", "getOption", 2}, // 1126
  {wxImage_GetOptionInt, "wxImage", "getOptionInt", 2}, // 1127
  {wxImage_HasOption, "wxImage", "hasOption", 2}, // 1128
  {wxImage_InitAlpha, "wxImage", "initAlpha", 1}, // 1129
  {wxImage_InitStandardHandlers, "wxImage", "initStandardHandlers", 0}, // 1130
  {wxImage_IsTransparent, "wxImage", "isTransparent", 4}, // 1131
  {wxImage_LoadFile_2, "wxImage", "loadFile", 3}, // 1132
  {wxImage_LoadFile_3, "wxImage", "loadFile", 4}, // 1133
  {wxImage_IsOk, "wxImage", "isOk", 1}, // 1134
  {wxImage_RemoveHandler, "wxImage", "removeHandler", 1}, // 1135
  {wxImage_Mirror, "wxImage", "mirror", 2}, // 1136
  {wxImage_Replace, "wxImage", "replace", 7}, // 1137
  {wxImage_Rescale, "wxImage", "rescale", 4}, // 1138
  {wxImage_Resize, "wxImage", "resize", 4}, // 1139
  {wxImage_Rotate, "wxImage", "rotate", 4}, // 1140
  {wxImage_RotateHue, "wxImage", "rotateHue", 2}, // 1141
  {wxImage_Rotate90, "wxImage", "rotate90", 2}, // 1142
  {wxImage_SaveFile_2_0, "wxImage", "saveFile", 3}, // 1143
  {wxImage_SaveFile_2_1, "wxImage", "saveFile", 3}, // 1144
  {wxImage_SaveFile_1, "wxImage", "saveFile", 2}, // 1145
  {wxImage_Scale, "wxImage", "scale", 4}, // 1146
  {wxImage_Size, "wxImage", "size", 4}, // 1147
  {wxImage_SetAlpha_2, "wxImage", "setAlpha", 3}, // 1148
  {wxImage_SetAlpha_3, "wxImage", "setAlpha", 4}, // 1149
  {wxImage_SetData_2, "wxImage", "setData", 3}, // 1150
  {wxImage_SetData_4, "wxImage", "setData", 5}, // 1151
  {wxImage_SetMask, "wxImage", "setMask", 2}, // 1152
  {wxImage_SetMaskColour, "wxImage", "setMaskColour", 4}, // 1153
  {wxImage_SetMaskFromImage, "wxImage", "setMaskFromImage", 5}, // 1154
  {wxImage_SetOption_2_1, "wxImage", "setOption", 3}, // 1155
  {wxImage_SetOption_2_0, "wxImage", "setOption", 3}, // 1156
  {wxImage_SetPalette, "wxImage", "setPalette", 2}, // 1157
  {wxImage_SetRGB_5, "wxImage", "setRGB", 6}, // 1158
  {wxImage_SetRGB_4, "wxImage", "setRGB", 5}, // 1159
  {wxBrush_new_0, "wxBrush", "new", 0}, // 1160
  {wxBrush_new_2, "wxBrush", "new", 2}, // 1161
  {NULL, "", "", 0}, // 1162
  {wxBrush_new_1, "wxBrush", "new", 1}, // 1163
  {NULL, "wxBrush", "destroy", 1}, // 1164 obj destructor wxBrush_destruct
  {wxBrush_GetColour, "wxBrush", "getColour", 1}, // 1165
  {wxBrush_GetStipple, "wxBrush", "getStipple", 1}, // 1166
  {wxBrush_GetStyle, "wxBrush", "getStyle", 1}, // 1167
  {wxBrush_IsHatch, "wxBrush", "isHatch", 1}, // 1168
  {wxBrush_IsOk, "wxBrush", "isOk", 1}, // 1169
  {wxBrush_SetColour_1, "wxBrush", "setColour", 2}, // 1170
  {wxBrush_SetColour_3, "wxBrush", "setColour", 4}, // 1171
  {wxBrush_SetStipple, "wxBrush", "setStipple", 2}, // 1172
  {wxBrush_SetStyle, "wxBrush", "setStyle", 2}, // 1173
  {wxPen_new_0, "wxPen", "new", 0}, // 1174
  {wxPen_new_2, "wxPen", "new", 2}, // 1175
  {wxPen_new_1, "wxPen", "new", 1}, // 1176
  {NULL, "wxPen", "destroy", 1}, // 1177 obj destructor wxPen_destruct
  {wxPen_GetCap, "wxPen", "getCap", 1}, // 1178
  {wxPen_GetColour, "wxPen", "getColour", 1}, // 1179
  {wxPen_GetJoin, "wxPen", "getJoin", 1}, // 1180
  {wxPen_GetStyle, "wxPen", "getStyle", 1}, // 1181
  {wxPen_GetWidth, "wxPen", "getWidth", 1}, // 1182
  {wxPen_IsOk, "wxPen", "isOk", 1}, // 1183
  {wxPen_SetCap, "wxPen", "setCap", 2}, // 1184
  {wxPen_SetColour_1, "wxPen", "setColour", 2}, // 1185
  {wxPen_SetColour_3, "wxPen", "setColour", 4}, // 1186
  {wxPen_SetJoin, "wxPen", "setJoin", 2}, // 1187
  {wxPen_SetStyle, "wxPen", "setStyle", 2}, // 1188
  {wxPen_SetWidth, "wxPen", "setWidth", 2}, // 1189
  {wxRegion_new_0, "wxRegion", "new", 0}, // 1190
  {wxRegion_new_4, "wxRegion", "new", 4}, // 1191
  {wxRegion_new_2, "wxRegion", "new", 2}, // 1192
  {wxRegion_new_1_0, "wxRegion", "new", 1}, // 1193
  {NULL, "", "", 0}, // 1194
  {wxRegion_new_1_1, "wxRegion", "new", 1}, // 1195
  {NULL, "", "", 0}, // 1196
  {NULL, "wxRegion", "destroy", 1}, // 1197 obj destructor wxRegion_destruct
  {wxRegion_Clear, "wxRegion", "clear", 1}, // 1198
  {wxRegion_Contains_2, "wxRegion", "contains", 3}, // 1199
  {wxRegion_Contains_1_0, "wxRegion", "contains", 2}, // 1200
  {wxRegion_Contains_4, "wxRegion", "contains", 5}, // 1201
  {wxRegion_Contains_1_1, "wxRegion", "contains", 2}, // 1202
  {wxRegion_ConvertToBitmap, "wxRegion", "convertToBitmap", 1}, // 1203
  {wxRegion_GetBox, "wxRegion", "getBox", 1}, // 1204
  {wxRegion_Intersect_4, "wxRegion", "intersect", 5}, // 1205
  {wxRegion_Intersect_1_0, "wxRegion", "intersect", 2}, // 1206
  {wxRegion_Intersect_1_1, "wxRegion", "intersect", 2}, // 1207
  {wxRegion_IsEmpty, "wxRegion", "isEmpty", 1}, // 1208
  {wxRegion_Subtract_1_0, "wxRegion", "subtract", 2}, // 1209
  {wxRegion_Subtract_1_1, "wxRegion", "subtract", 2}, // 1210
  {wxRegion_Offset_2, "wxRegion", "offset", 3}, // 1211
  {wxRegion_Offset_1, "wxRegion", "offset", 2}, // 1212
  {wxRegion_Union_4, "wxRegion", "union", 5}, // 1213
  {wxRegion_Union_1_1, "wxRegion", "union", 2}, // 1214
  {wxRegion_Union_1_0, "wxRegion", "union", 2}, // 1215
  {NULL, "", "", 0}, // 1216
  {wxRegion_Union_3, "wxRegion", "union", 4}, // 1217
  {wxRegion_Xor_4, "wxRegion", "'Xor'", 5}, // 1218
  {wxRegion_Xor_1_0, "wxRegion", "'Xor'", 2}, // 1219
  {wxRegion_Xor_1_1, "wxRegion", "'Xor'", 2}, // 1220
  {wxAcceleratorTable_new_0, "wxAcceleratorTable", "new", 0}, // 1221
  {wxAcceleratorTable_new_2, "wxAcceleratorTable", "new", 2}, // 1222
  {NULL, "", "", 0}, // 1223
  {NULL, "wxAcceleratorTable", "destroy", 1}, // 1224 obj destructor wxAcceleratorTable_destruct
  {wxAcceleratorTable_IsOk, "wxAcceleratorTable", "isOk", 1}, // 1225
  {wxAcceleratorEntry_new_1_0, "wxAcceleratorEntry", "new", 1}, // 1226
  {wxAcceleratorEntry_new_1_1, "wxAcceleratorEntry", "new", 1}, // 1227
  {wxAcceleratorEntry_GetCommand, "wxAcceleratorEntry", "getCommand", 1}, // 1228
  {wxAcceleratorEntry_GetFlags, "wxAcceleratorEntry", "getFlags", 1}, // 1229
  {wxAcceleratorEntry_GetKeyCode, "wxAcceleratorEntry", "getKeyCode", 1}, // 1230
  {wxAcceleratorEntry_Set, "wxAcceleratorEntry", "set", 5}, // 1231
  {wxAcceleratorEntry_destroy, "wxAcceleratorEntry", "'Destroy'", 1}, // 1232
  {NULL, "", "", 0}, // 1233
  {wxCaret_new_3, "wxCaret", "new", 3}, // 1234
  {wxCaret_new_2, "wxCaret", "new", 2}, // 1235
  {wxCaret_Create_3, "wxCaret", "create", 4}, // 1236
  {wxCaret_Create_2, "wxCaret", "create", 3}, // 1237
  {wxCaret_GetBlinkTime, "wxCaret", "getBlinkTime", 0}, // 1238
  {NULL, "", "", 0}, // 1239
  {wxCaret_GetPosition, "wxCaret", "getPosition", 1}, // 1240
  {NULL, "", "", 0}, // 1241
  {wxCaret_GetSize, "wxCaret", "getSize", 1}, // 1242
  {wxCaret_GetWindow, "wxCaret", "getWindow", 1}, // 1243
  {wxCaret_Hide, "wxCaret", "hide", 1}, // 1244
  {wxCaret_IsOk, "wxCaret", "isOk", 1}, // 1245
  {wxCaret_IsVisible, "wxCaret", "isVisible", 1}, // 1246
  {wxCaret_Move_2, "wxCaret", "move", 3}, // 1247
  {wxCaret_Move_1, "wxCaret", "move", 2}, // 1248
  {wxCaret_SetBlinkTime, "wxCaret", "setBlinkTime", 1}, // 1249
  {wxCaret_SetSize_2, "wxCaret", "setSize", 3}, // 1250
  {wxCaret_SetSize_1, "wxCaret", "setSize", 2}, // 1251
  {wxCaret_Show, "wxCaret", "show", 2}, // 1252
  {wxCaret_destroy, "wxCaret", "'Destroy'", 1}, // 1253
  {wxSizer_Add_2_0, "wxSizer", "add", 3}, // 1254
  {wxSizer_Add_2_1, "wxSizer", "add", 3}, // 1255
  {NULL, "", "", 0}, // 1256
  {NULL, "", "", 0}, // 1257
  {wxSizer_Add_3_0, "wxSizer", "add", 4}, // 1258
  {wxSizer_Add_3_1, "wxSizer", "add", 4}, // 1259
  {wxSizer_AddSpacer, "wxSizer", "addSpacer", 2}, // 1260
  {wxSizer_AddStretchSpacer, "wxSizer", "addStretchSpacer", 2}, // 1261
  {wxSizer_CalcMin, "wxSizer", "calcMin", 1}, // 1262
  {wxSizer_Clear, "wxSizer", "clear", 2}, // 1263
  {wxSizer_Detach_1_0, "wxSizer", "detach", 2}, // 1264
  {NULL, "", "", 0}, // 1265
  {wxSizer_Detach_1_1, "wxSizer", "detach", 2}, // 1266
  {wxSizer_Fit, "wxSizer", "fit", 2}, // 1267
  {wxSizer_FitInside, "wxSizer", "fitInside", 2}, // 1268
  {NULL, "", "", 0}, // 1269
  {wxSizer_GetChildren, "wxSizer", "getChildren", 1}, // 1270
  {wxSizer_GetItem_2, "wxSizer", "getItem", 3}, // 1271
  {NULL, "", "", 0}, // 1272
  {wxSizer_GetItem_1, "wxSizer", "getItem", 2}, // 1273
  {wxSizer_GetSize, "wxSizer", "getSize", 1}, // 1274
  {wxSizer_GetPosition, "wxSizer", "getPosition", 1}, // 1275
  {wxSizer_GetMinSize, "wxSizer", "getMinSize", 1}, // 1276
  {wxSizer_Hide_2, "wxSizer", "hide", 3}, // 1277
  {NULL, "", "", 0}, // 1278
  {wxSizer_Hide_1, "wxSizer", "hide", 2}, // 1279
  {wxSizer_Insert_3_0, "wxSizer", "insert", 4}, // 1280
  {wxSizer_Insert_3_1, "wxSizer", "insert", 4}, // 1281
  {NULL, "", "", 0}, // 1282
  {NULL, "", "", 0}, // 1283
  {wxSizer_Insert_4_0, "wxSizer", "insert", 5}, // 1284
  {wxSizer_Insert_4_1, "wxSizer", "insert", 5}, // 1285
  {wxSizer_Insert_2, "wxSizer", "insert", 3}, // 1286
  {wxSizer_InsertSpacer, "wxSizer", "insertSpacer", 3}, // 1287
  {wxSizer_InsertStretchSpacer, "wxSizer", "insertStretchSpacer", 3}, // 1288
  {wxSizer_IsShown_1_0, "wxSizer", "isShown", 2}, // 1289
  {NULL, "", "", 0}, // 1290
  {wxSizer_IsShown_1_1, "wxSizer", "isShown", 2}, // 1291
  {wxSizer_Layout, "wxSizer", "layout", 1}, // 1292
  {wxSizer_Prepend_2_0, "wxSizer", "prepend", 3}, // 1293
  {wxSizer_Prepend_2_1, "wxSizer", "prepend", 3}, // 1294
  {NULL, "", "", 0}, // 1295
  {NULL, "", "", 0}, // 1296
  {wxSizer_Prepend_3_0, "wxSizer", "prepend", 4}, // 1297
  {wxSizer_Prepend_3_1, "wxSizer", "prepend", 4}, // 1298
  {wxSizer_Prepend_1, "wxSizer", "prepend", 2}, // 1299
  {wxSizer_PrependSpacer, "wxSizer", "prependSpacer", 2}, // 1300
  {wxSizer_PrependStretchSpacer, "wxSizer", "prependStretchSpacer", 2}, // 1301
  {wxSizer_Remove_1_1, "wxSizer", "remove", 2}, // 1302
  {wxSizer_Remove_1_0, "wxSizer", "remove", 2}, // 1303
  {wxSizer_Replace_3, "wxSizer", "replace", 4}, // 1304
  {NULL, "", "", 0}, // 1305
  {wxSizer_Replace_2, "wxSizer", "replace", 3}, // 1306
  {wxSizer_SetDimension_4, "wxSizer", "setDimension", 5}, // 1307
  {wxSizer_SetDimension_2, "wxSizer", "setDimension", 3}, // 1308
  {wxSizer_SetMinSize_1, "wxSizer", "setMinSize", 2}, // 1309
  {wxSizer_SetMinSize_2, "wxSizer", "setMinSize", 3}, // 1310
  {wxSizer_SetItemMinSize_3_0, "wxSizer", "setItemMinSize", 4}, // 1311
  {wxSizer_SetItemMinSize_2_0, "wxSizer", "setItemMinSize", 3}, // 1312
  {NULL, "", "", 0}, // 1313
  {NULL, "", "", 0}, // 1314
  {wxSizer_SetItemMinSize_3_1, "wxSizer", "setItemMinSize", 4}, // 1315
  {wxSizer_SetItemMinSize_2_1, "wxSizer", "setItemMinSize", 3}, // 1316
  {wxSizer_SetSizeHints, "wxSizer", "setSizeHints", 2}, // 1317
  {wxSizer_Show_2_0, "wxSizer", "show", 3}, // 1318
  {NULL, "", "", 0}, // 1319
  {wxSizer_Show_2_1, "wxSizer", "show", 3}, // 1320
  {wxSizer_Show_1, "wxSizer", "show", 2}, // 1321
  {wxSizer_ShowItems, "wxSizer", "showItems", 2}, // 1322
  {wxSizerFlags_new, "wxSizerFlags", "new", 1}, // 1323
  {wxSizerFlags_Align, "wxSizerFlags", "align", 2}, // 1324
  {wxSizerFlags_Border_2, "wxSizerFlags", "border", 3}, // 1325
  {wxSizerFlags_Border_1, "wxSizerFlags", "border", 2}, // 1326
  {wxSizerFlags_Center, "wxSizerFlags", "center", 1}, // 1327
  {wxSizerFlags_Expand, "wxSizerFlags", "expand", 1}, // 1328
  {wxSizerFlags_Left, "wxSizerFlags", "left", 1}, // 1329
  {wxSizerFlags_Proportion, "wxSizerFlags", "proportion", 2}, // 1330
  {wxSizerFlags_Right, "wxSizerFlags", "right", 1}, // 1331
  {wxSizerFlags_destroy, "wxSizerFlags", "'Destroy'", 1}, // 1332
  {wxSizerItem_new_3, "wxSizerItem", "new", 3}, // 1333
  {wxSizerItem_new_2_0, "wxSizerItem", "new", 2}, // 1334
  {wxSizerItem_new_2_1, "wxSizerItem", "new", 2}, // 1335
  {NULL, "", "", 0}, // 1336
  {NULL, "", "", 0}, // 1337
  {NULL, "wxSizerItem", "destroy", 1}, // 1338 obj destructor wxSizerItem_destruct
  {wxSizerItem_CalcMin, "wxSizerItem", "calcMin", 1}, // 1339
  {wxSizerItem_DeleteWindows, "wxSizerItem", "deleteWindows", 1}, // 1340
  {wxSizerItem_DetachSizer, "wxSizerItem", "detachSizer", 1}, // 1341
  {wxSizerItem_GetBorder, "wxSizerItem", "getBorder", 1}, // 1342
  {wxSizerItem_GetFlag, "wxSizerItem", "getFlag", 1}, // 1343
  {wxSizerItem_GetMinSize, "wxSizerItem", "getMinSize", 1}, // 1344
  {wxSizerItem_GetPosition, "wxSizerItem", "getPosition", 1}, // 1345
  {wxSizerItem_GetProportion, "wxSizerItem", "getProportion", 1}, // 1346
  {wxSizerItem_GetRatio, "wxSizerItem", "getRatio", 1}, // 1347
  {wxSizerItem_GetRect, "wxSizerItem", "getRect", 1}, // 1348
  {wxSizerItem_GetSize, "wxSizerItem", "getSize", 1}, // 1349
  {wxSizerItem_GetSizer, "wxSizerItem", "getSizer", 1}, // 1350
  {wxSizerItem_GetSpacer, "wxSizerItem", "getSpacer", 1}, // 1351
  {wxSizerItem_GetUserData, "wxSizerItem", "getUserData", 1}, // 1352
  {wxSizerItem_GetWindow, "wxSizerItem", "getWindow", 1}, // 1353
  {wxSizerItem_IsSizer, "wxSizerItem", "isSizer", 1}, // 1354
  {wxSizerItem_IsShown, "wxSizerItem", "isShown", 1}, // 1355
  {wxSizerItem_IsSpacer, "wxSizerItem", "isSpacer", 1}, // 1356
  {wxSizerItem_IsWindow, "wxSizerItem", "isWindow", 1}, // 1357
  {wxSizerItem_SetBorder, "wxSizerItem", "setBorder", 2}, // 1358
  {wxSizerItem_SetDimension, "wxSizerItem", "setDimension", 3}, // 1359
  {wxSizerItem_SetFlag, "wxSizerItem", "setFlag", 2}, // 1360
  {wxSizerItem_SetInitSize, "wxSizerItem", "setInitSize", 3}, // 1361
  {wxSizerItem_SetMinSize_1, "wxSizerItem", "setMinSize", 2}, // 1362
  {wxSizerItem_SetMinSize_2, "wxSizerItem", "setMinSize", 3}, // 1363
  {wxSizerItem_SetProportion, "wxSizerItem", "setProportion", 2}, // 1364
  {wxSizerItem_SetRatio_2, "wxSizerItem", "setRatio", 3}, // 1365
  {wxSizerItem_SetRatio_1_1, "wxSizerItem", "setRatio", 2}, // 1366
  {wxSizerItem_SetRatio_1_0, "wxSizerItem", "setRatio", 2}, // 1367
  {wxSizerItem_AssignSizer, "wxSizerItem", "assignSizer", 2}, // 1368
  {wxSizerItem_AssignSpacer_1, "wxSizerItem", "assignSpacer", 2}, // 1369
  {wxSizerItem_AssignSpacer_2, "wxSizerItem", "assignSpacer", 3}, // 1370
  {wxSizerItem_AssignWindow, "wxSizerItem", "assignWindow", 2}, // 1371
  {wxSizerItem_Show, "wxSizerItem", "show", 2}, // 1372
  {wxBoxSizer_new, "wxBoxSizer", "new", 1}, // 1373
  {wxBoxSizer_GetOrientation, "wxBoxSizer", "getOrientation", 1}, // 1374
  {NULL, "wxBoxSizer", "'Destroy'", 1}, // 1375 obj destructor wxBoxSizer_destroy
  {wxStaticBoxSizer_new_2, "wxStaticBoxSizer", "new", 2}, // 1376
  {wxStaticBoxSizer_new_3, "wxStaticBoxSizer", "new", 3}, // 1377
  {wxStaticBoxSizer_GetStaticBox, "wxStaticBoxSizer", "getStaticBox", 1}, // 1378
  {NULL, "wxStaticBoxSizer", "'Destroy'", 1}, // 1379 obj destructor wxStaticBoxSizer_destroy
  {wxGridSizer_new_3_0, "wxGridSizer", "new", 3}, // 1380
  {wxGridSizer_new_2, "wxGridSizer", "new", 2}, // 1381
  {wxGridSizer_new_4, "wxGridSizer", "new", 4}, // 1382
  {wxGridSizer_new_3_1, "wxGridSizer", "new", 3}, // 1383
  {wxGridSizer_GetCols, "wxGridSizer", "getCols", 1}, // 1384
  {wxGridSizer_GetHGap, "wxGridSizer", "getHGap", 1}, // 1385
  {wxGridSizer_GetRows, "wxGridSizer", "getRows", 1}, // 1386
  {wxGridSizer_GetVGap, "wxGridSizer", "getVGap", 1}, // 1387
  {wxGridSizer_SetCols, "wxGridSizer", "setCols", 2}, // 1388
  {wxGridSizer_SetHGap, "wxGridSizer", "setHGap", 2}, // 1389
  {wxGridSizer_SetRows, "wxGridSizer", "setRows", 2}, // 1390
  {wxGridSizer_SetVGap, "wxGridSizer", "setVGap", 2}, // 1391
  {NULL, "wxGridSizer", "'Destroy'", 1}, // 1392 obj destructor wxGridSizer_destroy
  {wxFlexGridSizer_new_3_0, "wxFlexGridSizer", "new", 3}, // 1393
  {wxFlexGridSizer_new_2, "wxFlexGridSizer", "new", 2}, // 1394
  {wxFlexGridSizer_new_4, "wxFlexGridSizer", "new", 4}, // 1395
  {wxFlexGridSizer_new_3_1, "wxFlexGridSizer", "new", 3}, // 1396
  {wxFlexGridSizer_AddGrowableCol, "wxFlexGridSizer", "addGrowableCol", 3}, // 1397
  {wxFlexGridSizer_AddGrowableRow, "wxFlexGridSizer", "addGrowableRow", 3}, // 1398
  {wxFlexGridSizer_GetFlexibleDirection, "wxFlexGridSizer", "getFlexibleDirection", 1}, // 1399
  {wxFlexGridSizer_GetNonFlexibleGrowMode, "wxFlexGridSizer", "getNonFlexibleGrowMode", 1}, // 1400
  {wxFlexGridSizer_RemoveGrowableCol, "wxFlexGridSizer", "removeGrowableCol", 2}, // 1401
  {wxFlexGridSizer_RemoveGrowableRow, "wxFlexGridSizer", "removeGrowableRow", 2}, // 1402
  {wxFlexGridSizer_SetFlexibleDirection, "wxFlexGridSizer", "setFlexibleDirection", 2}, // 1403
  {wxFlexGridSizer_SetNonFlexibleGrowMode, "wxFlexGridSizer", "setNonFlexibleGrowMode", 2}, // 1404
  {NULL, "wxFlexGridSizer", "'Destroy'", 1}, // 1405 obj destructor wxFlexGridSizer_destroy
  {wxGridBagSizer_new, "wxGridBagSizer", "new", 1}, // 1406
  {wxGridBagSizer_Add_3, "wxGridBagSizer", "add", 4}, // 1407
  {NULL, "", "", 0}, // 1408
  {wxGridBagSizer_Add_1, "wxGridBagSizer", "add", 2}, // 1409
  {wxGridBagSizer_Add_4, "wxGridBagSizer", "add", 5}, // 1410
  {wxGridBagSizer_CalcMin, "wxGridBagSizer", "calcMin", 1}, // 1411
  {wxGridBagSizer_CheckForIntersection_2, "wxGridBagSizer", "checkForIntersection", 3}, // 1412
  {wxGridBagSizer_CheckForIntersection_3, "wxGridBagSizer", "checkForIntersection", 4}, // 1413
  {wxGridBagSizer_FindItem, "wxGridBagSizer", "findItem", 2}, // 1414
  {NULL, "", "", 0}, // 1415
  {wxGridBagSizer_FindItemAtPoint, "wxGridBagSizer", "findItemAtPoint", 2}, // 1416
  {wxGridBagSizer_FindItemAtPosition, "wxGridBagSizer", "findItemAtPosition", 2}, // 1417
  {wxGridBagSizer_FindItemWithData, "wxGridBagSizer", "findItemWithData", 2}, // 1418
  {wxGridBagSizer_GetCellSize, "wxGridBagSizer", "getCellSize", 3}, // 1419
  {wxGridBagSizer_GetEmptyCellSize, "wxGridBagSizer", "getEmptyCellSize", 1}, // 1420
  {wxGridBagSizer_GetItemPosition_1_0, "wxGridBagSizer", "getItemPosition", 2}, // 1421
  {NULL, "", "", 0}, // 1422
  {wxGridBagSizer_GetItemPosition_1_1, "wxGridBagSizer", "getItemPosition", 2}, // 1423
  {wxGridBagSizer_GetItemSpan_1_0, "wxGridBagSizer", "getItemSpan", 2}, // 1424
  {NULL, "", "", 0}, // 1425
  {wxGridBagSizer_GetItemSpan_1_1, "wxGridBagSizer", "getItemSpan", 2}, // 1426
  {wxGridBagSizer_SetEmptyCellSize, "wxGridBagSizer", "setEmptyCellSize", 2}, // 1427
  {wxGridBagSizer_SetItemPosition_2_0, "wxGridBagSizer", "setItemPosition", 3}, // 1428
  {NULL, "", "", 0}, // 1429
  {wxGridBagSizer_SetItemPosition_2_1, "wxGridBagSizer", "setItemPosition", 3}, // 1430
  {wxGridBagSizer_SetItemSpan_2_0, "wxGridBagSizer", "setItemSpan", 3}, // 1431
  {NULL, "", "", 0}, // 1432
  {wxGridBagSizer_SetItemSpan_2_1, "wxGridBagSizer", "setItemSpan", 3}, // 1433
  {NULL, "wxGridBagSizer", "'Destroy'", 1}, // 1434 obj destructor wxGridBagSizer_destroy
  {wxStdDialogButtonSizer_new, "wxStdDialogButtonSizer", "new", 0}, // 1435
  {wxStdDialogButtonSizer_AddButton, "wxStdDialogButtonSizer", "addButton", 2}, // 1436
  {wxStdDialogButtonSizer_Realize, "wxStdDialogButtonSizer", "realize", 1}, // 1437
  {wxStdDialogButtonSizer_SetAffirmativeButton, "wxStdDialogButtonSizer", "setAffirmativeButton", 2}, // 1438
  {wxStdDialogButtonSizer_SetCancelButton, "wxStdDialogButtonSizer", "setCancelButton", 2}, // 1439
  {wxStdDialogButtonSizer_SetNegativeButton, "wxStdDialogButtonSizer", "setNegativeButton", 2}, // 1440
  {NULL, "wxStdDialogButtonSizer", "'Destroy'", 1}, // 1441 obj destructor wxStdDialogButtonSizer_destroy
  {wxFont_new_0, "wxFont", "new", 0}, // 1442
  {wxFont_new_1_1, "wxFont", "new", 1}, // 1443
  {wxFont_new_5_0, "wxFont", "new", 5}, // 1444
  {wxFont_new_5_1, "wxFont", "new", 5}, // 1445
  {wxFont_new_1_0, "wxFont", "new", 1}, // 1446
  {NULL, "wxFont", "destroy", 1}, // 1447 obj destructor wxFont_destruct
  {wxFont_IsFixedWidth, "wxFont", "isFixedWidth", 1}, // 1448
  {wxFont_GetDefaultEncoding, "wxFont", "getDefaultEncoding", 0}, // 1449
  {wxFont_GetFaceName, "wxFont", "getFaceName", 1}, // 1450
  {wxFont_GetFamily, "wxFont", "getFamily", 1}, // 1451
  {wxFont_GetNativeFontInfoDesc, "wxFont", "getNativeFontInfoDesc", 1}, // 1452
  {wxFont_GetNativeFontInfoUserDesc, "wxFont", "getNativeFontInfoUserDesc", 1}, // 1453
  {wxFont_GetPointSize, "wxFont", "getPointSize", 1}, // 1454
  {wxFont_GetStyle, "wxFont", "getStyle", 1}, // 1455
  {wxFont_GetUnderlined, "wxFont", "getUnderlined", 1}, // 1456
  {wxFont_GetWeight, "wxFont", "getWeight", 1}, // 1457
  {wxFont_IsOk, "wxFont", "isOk", 1}, // 1458
  {wxFont_SetDefaultEncoding, "wxFont", "setDefaultEncoding", 1}, // 1459
  {wxFont_SetFaceName, "wxFont", "setFaceName", 2}, // 1460
  {wxFont_SetFamily, "wxFont", "setFamily", 2}, // 1461
  {wxFont_SetPointSize, "wxFont", "setPointSize", 2}, // 1462
  {wxFont_SetStyle, "wxFont", "setStyle", 2}, // 1463
  {wxFont_SetUnderlined, "wxFont", "setUnderlined", 2}, // 1464
  {wxFont_SetWeight, "wxFont", "setWeight", 2}, // 1465
  {wxToolTip_Enable, "wxToolTip", "enable", 1}, // 1466
  {wxToolTip_SetDelay, "wxToolTip", "setDelay", 1}, // 1467
  {wxToolTip_new, "wxToolTip", "new", 1}, // 1468
  {wxToolTip_SetTip, "wxToolTip", "setTip", 2}, // 1469
  {wxToolTip_GetTip, "wxToolTip", "getTip", 1}, // 1470
  {wxToolTip_GetWindow, "wxToolTip", "getWindow", 1}, // 1471
  {NULL, "wxToolTip", "'Destroy'", 1}, // 1472 obj destructor wxToolTip_destroy
  {wxButton_new_0, "wxButton", "new", 0}, // 1473
  {wxButton_new_3, "wxButton", "new", 3}, // 1474
  {wxButton_Create, "wxButton", "create", 4}, // 1475
  {wxButton_GetDefaultSize_STAT_0, "wxButton", "getDefaultSize", 0}, // 1476
#if wxCHECK_VERSION(3,1,3)
  {wxButton_GetDefaultSize_STAT_1, "wxButton", "getDefaultSize", 1}, // 1477
#else
  {NULL, "wxButton", "getDefaultSize", 0}, // 1477
#endif
  {wxButton_SetDefault, "wxButton", "setDefault", 1}, // 1478
  {wxButton_SetLabel, "wxButton", "setLabel", 2}, // 1479
  {wxButton_GetBitmapDisabled, "wxButton", "getBitmapDisabled", 1}, // 1480
  {wxButton_GetBitmapFocus, "wxButton", "getBitmapFocus", 1}, // 1481
  {wxButton_GetBitmapLabel, "wxButton", "getBitmapLabel", 1}, // 1482
  {wxButton_SetBitmapDisabled, "wxButton", "setBitmapDisabled", 2}, // 1483
  {wxButton_SetBitmapFocus, "wxButton", "setBitmapFocus", 2}, // 1484
  {wxButton_SetBitmapLabel, "wxButton", "setBitmapLabel", 2}, // 1485
  {NULL, "wxButton", "'Destroy'", 1}, // 1486 obj destructor wxButton_destroy
  {wxBitmapButton_new_0, "wxBitmapButton", "new", 0}, // 1487
  {wxBitmapButton_new_4, "wxBitmapButton", "new", 4}, // 1488
  {wxBitmapButton_Create, "wxBitmapButton", "create", 5}, // 1489
  {wxBitmapButton_NewCloseButton, "wxBitmapButton", "newCloseButton", 2}, // 1490
  {NULL, "wxBitmapButton", "'Destroy'", 1}, // 1491 obj destructor wxBitmapButton_destroy
  {wxToggleButton_new_0, "wxToggleButton", "new", 0}, // 1492
  {wxToggleButton_new_4, "wxToggleButton", "new", 4}, // 1493
  {NULL, "wxToggleButton", "destroy", 1}, // 1494 obj destructor wxToggleButton_destruct
  {wxToggleButton_Create, "wxToggleButton", "create", 5}, // 1495
  {wxToggleButton_GetValue, "wxToggleButton", "getValue", 1}, // 1496
  {wxToggleButton_SetValue, "wxToggleButton", "setValue", 2}, // 1497
  {wxCalendarCtrl_new_0, "wxCalendarCtrl", "new", 0}, // 1498
  {wxCalendarCtrl_new_3, "wxCalendarCtrl", "new", 3}, // 1499
  {wxCalendarCtrl_Create, "wxCalendarCtrl", "create", 4}, // 1500
  {NULL, "wxCalendarCtrl", "destroy", 1}, // 1501 obj destructor wxCalendarCtrl_destruct
  {wxCalendarCtrl_SetDate, "wxCalendarCtrl", "setDate", 2}, // 1502
  {wxCalendarCtrl_GetDate, "wxCalendarCtrl", "getDate", 1}, // 1503
#if !wxCHECK_VERSION(2,9,0)
  {wxCalendarCtrl_EnableYearChange, "wxCalendarCtrl", "enableYearChange", 2}, // 1504
#else
  {NULL, "wxCalendarCtrl", "enableYearChange", 0}, // 1504
#endif
  {wxCalendarCtrl_EnableMonthChange, "wxCalendarCtrl", "enableMonthChange", 2}, // 1505
  {wxCalendarCtrl_EnableHolidayDisplay, "wxCalendarCtrl", "enableHolidayDisplay", 2}, // 1506
  {wxCalendarCtrl_SetHeaderColours, "wxCalendarCtrl", "setHeaderColours", 3}, // 1507
  {wxCalendarCtrl_GetHeaderColourFg, "wxCalendarCtrl", "getHeaderColourFg", 1}, // 1508
  {wxCalendarCtrl_GetHeaderColourBg, "wxCalendarCtrl", "getHeaderColourBg", 1}, // 1509
  {wxCalendarCtrl_SetHighlightColours, "wxCalendarCtrl", "setHighlightColours", 3}, // 1510
  {wxCalendarCtrl_GetHighlightColourFg, "wxCalendarCtrl", "getHighlightColourFg", 1}, // 1511
  {wxCalendarCtrl_GetHighlightColourBg, "wxCalendarCtrl", "getHighlightColourBg", 1}, // 1512
  {wxCalendarCtrl_SetHolidayColours, "wxCalendarCtrl", "setHolidayColours", 3}, // 1513
  {wxCalendarCtrl_GetHolidayColourFg, "wxCalendarCtrl", "getHolidayColourFg", 1}, // 1514
  {wxCalendarCtrl_GetHolidayColourBg, "wxCalendarCtrl", "getHolidayColourBg", 1}, // 1515
  {wxCalendarCtrl_GetAttr, "wxCalendarCtrl", "getAttr", 2}, // 1516
  {wxCalendarCtrl_SetAttr, "wxCalendarCtrl", "setAttr", 3}, // 1517
  {wxCalendarCtrl_SetHoliday, "wxCalendarCtrl", "setHoliday", 2}, // 1518
  {wxCalendarCtrl_ResetAttr, "wxCalendarCtrl", "resetAttr", 2}, // 1519
  {wxCalendarCtrl_HitTest, "wxCalendarCtrl", "hitTest", 2}, // 1520
  {wxCalendarDateAttr_new_1, "wxCalendarDateAttr", "new", 1}, // 1521
  {wxCalendarDateAttr_new_2, "wxCalendarDateAttr", "new", 2}, // 1522
  {wxCalendarDateAttr_SetTextColour, "wxCalendarDateAttr", "setTextColour", 2}, // 1523
  {wxCalendarDateAttr_SetBackgroundColour, "wxCalendarDateAttr", "setBackgroundColour", 2}, // 1524
  {wxCalendarDateAttr_SetBorderColour, "wxCalendarDateAttr", "setBorderColour", 2}, // 1525
  {wxCalendarDateAttr_SetFont, "wxCalendarDateAttr", "setFont", 2}, // 1526
  {wxCalendarDateAttr_SetBorder, "wxCalendarDateAttr", "setBorder", 2}, // 1527
  {wxCalendarDateAttr_SetHoliday, "wxCalendarDateAttr", "setHoliday", 2}, // 1528
  {wxCalendarDateAttr_HasTextColour, "wxCalendarDateAttr", "hasTextColour", 1}, // 1529
  {wxCalendarDateAttr_HasBackgroundColour, "wxCalendarDateAttr", "hasBackgroundColour", 1}, // 1530
  {wxCalendarDateAttr_HasBorderColour, "wxCalendarDateAttr", "hasBorderColour", 1}, // 1531
  {wxCalendarDateAttr_HasFont, "wxCalendarDateAttr", "hasFont", 1}, // 1532
  {wxCalendarDateAttr_HasBorder, "wxCalendarDateAttr", "hasBorder", 1}, // 1533
  {wxCalendarDateAttr_IsHoliday, "wxCalendarDateAttr", "isHoliday", 1}, // 1534
  {wxCalendarDateAttr_GetTextColour, "wxCalendarDateAttr", "getTextColour", 1}, // 1535
  {wxCalendarDateAttr_GetBackgroundColour, "wxCalendarDateAttr", "getBackgroundColour", 1}, // 1536
  {wxCalendarDateAttr_GetBorderColour, "wxCalendarDateAttr", "getBorderColour", 1}, // 1537
  {wxCalendarDateAttr_GetFont, "wxCalendarDateAttr", "getFont", 1}, // 1538
  {wxCalendarDateAttr_GetBorder, "wxCalendarDateAttr", "getBorder", 1}, // 1539
  {wxCalendarDateAttr_destroy, "wxCalendarDateAttr", "'Destroy'", 1}, // 1540
  {wxCheckBox_new_0, "wxCheckBox", "new", 0}, // 1541
  {wxCheckBox_new_4, "wxCheckBox", "new", 4}, // 1542
  {NULL, "wxCheckBox", "destroy", 1}, // 1543 obj destructor wxCheckBox_destruct
  {wxCheckBox_Create, "wxCheckBox", "create", 5}, // 1544
  {wxCheckBox_GetValue, "wxCheckBox", "getValue", 1}, // 1545
  {wxCheckBox_Get3StateValue, "wxCheckBox", "get3StateValue", 1}, // 1546
  {wxCheckBox_Is3rdStateAllowedForUser, "wxCheckBox", "is3rdStateAllowedForUser", 1}, // 1547
  {wxCheckBox_Is3State, "wxCheckBox", "is3State", 1}, // 1548
  {wxCheckBox_IsChecked, "wxCheckBox", "isChecked", 1}, // 1549
  {wxCheckBox_SetValue, "wxCheckBox", "setValue", 2}, // 1550
  {wxCheckBox_Set3StateValue, "wxCheckBox", "set3StateValue", 2}, // 1551
  {wxCheckListBox_new_0, "wxCheckListBox", "new", 0}, // 1552
  {NULL, "", "", 0}, // 1553
  {wxCheckListBox_new_3, "wxCheckListBox", "new", 3}, // 1554
  {NULL, "wxCheckListBox", "destroy", 1}, // 1555 obj destructor wxCheckListBox_destruct
  {wxCheckListBox_Check, "wxCheckListBox", "check", 3}, // 1556
  {wxCheckListBox_IsChecked, "wxCheckListBox", "isChecked", 2}, // 1557
  {wxChoice_new_0, "wxChoice", "new", 0}, // 1558
  {NULL, "", "", 0}, // 1559
  {wxChoice_new_3, "wxChoice", "new", 3}, // 1560
  {NULL, "wxChoice", "destroy", 1}, // 1561 obj destructor wxChoice_destruct
  {NULL, "", "", 0}, // 1562
  {wxChoice_Create, "wxChoice", "create", 7}, // 1563
  {wxChoice_Delete, "wxChoice", "delete", 2}, // 1564
  {wxChoice_GetColumns, "wxChoice", "getColumns", 1}, // 1565
  {wxChoice_SetColumns, "wxChoice", "setColumns", 2}, // 1566
  {wxComboBox_new_0, "wxComboBox", "new", 0}, // 1567
  {NULL, "", "", 0}, // 1568
  {wxComboBox_new_3, "wxComboBox", "new", 3}, // 1569
  {NULL, "wxComboBox", "destroy", 1}, // 1570 obj destructor wxComboBox_destruct
  {NULL, "", "", 0}, // 1571
  {wxComboBox_Create, "wxComboBox", "create", 8}, // 1572
  {wxComboBox_CanCopy, "wxComboBox", "canCopy", 1}, // 1573
  {wxComboBox_CanCut, "wxComboBox", "canCut", 1}, // 1574
  {wxComboBox_CanPaste, "wxComboBox", "canPaste", 1}, // 1575
  {wxComboBox_CanRedo, "wxComboBox", "canRedo", 1}, // 1576
  {wxComboBox_CanUndo, "wxComboBox", "canUndo", 1}, // 1577
  {wxComboBox_Copy, "wxComboBox", "copy", 1}, // 1578
  {wxComboBox_Cut, "wxComboBox", "cut", 1}, // 1579
  {wxComboBox_GetInsertionPoint, "wxComboBox", "getInsertionPoint", 1}, // 1580
  {wxComboBox_GetLastPosition, "wxComboBox", "getLastPosition", 1}, // 1581
  {wxComboBox_GetValue, "wxComboBox", "getValue", 1}, // 1582
  {wxComboBox_Paste, "wxComboBox", "paste", 1}, // 1583
  {wxComboBox_Redo, "wxComboBox", "redo", 1}, // 1584
  {wxComboBox_Replace, "wxComboBox", "replace", 4}, // 1585
  {wxComboBox_Remove, "wxComboBox", "remove", 3}, // 1586
  {wxComboBox_SetInsertionPoint, "wxComboBox", "setInsertionPoint", 2}, // 1587
  {wxComboBox_SetInsertionPointEnd, "wxComboBox", "setInsertionPointEnd", 1}, // 1588
  {wxComboBox_SetSelection_2, "wxComboBox", "setSelection", 3}, // 1589
  {wxComboBox_SetSelection_1, "wxComboBox", "setSelection", 2}, // 1590
  {wxComboBox_SetValue, "wxComboBox", "setValue", 2}, // 1591
  {wxComboBox_Undo, "wxComboBox", "undo", 1}, // 1592
  {wxGauge_new_0, "wxGauge", "new", 0}, // 1593
  {wxGauge_new_4, "wxGauge", "new", 4}, // 1594
  {NULL, "wxGauge", "destroy", 1}, // 1595 obj destructor wxGauge_destruct
  {wxGauge_Create, "wxGauge", "create", 5}, // 1596
  {wxGauge_GetRange, "wxGauge", "getRange", 1}, // 1597
  {wxGauge_GetValue, "wxGauge", "getValue", 1}, // 1598
  {wxGauge_IsVertical, "wxGauge", "isVertical", 1}, // 1599
  {wxGauge_SetRange, "wxGauge", "setRange", 2}, // 1600
  {wxGauge_SetValue, "wxGauge", "setValue", 2}, // 1601
  {wxGauge_Pulse, "wxGauge", "pulse", 1}, // 1602
  {wxGenericDirCtrl_new_0, "wxGenericDirCtrl", "new", 0}, // 1603
  {wxGenericDirCtrl_new_2, "wxGenericDirCtrl", "new", 2}, // 1604
  {NULL, "wxGenericDirCtrl", "destroy", 1}, // 1605 obj destructor wxGenericDirCtrl_destruct
  {wxGenericDirCtrl_Create, "wxGenericDirCtrl", "create", 3}, // 1606
  {wxGenericDirCtrl_Init, "wxGenericDirCtrl", "init", 1}, // 1607
  {wxGenericDirCtrl_CollapseTree, "wxGenericDirCtrl", "collapseTree", 1}, // 1608
  {wxGenericDirCtrl_ExpandPath, "wxGenericDirCtrl", "expandPath", 2}, // 1609
  {wxGenericDirCtrl_GetDefaultPath, "wxGenericDirCtrl", "getDefaultPath", 1}, // 1610
  {wxGenericDirCtrl_GetPath_0, "wxGenericDirCtrl", "getPath", 1}, // 1611
  {wxGenericDirCtrl_GetPath_1, "wxGenericDirCtrl", "getPath", 2}, // 1612
  {wxGenericDirCtrl_GetFilePath, "wxGenericDirCtrl", "getFilePath", 1}, // 1613
  {wxGenericDirCtrl_GetFilter, "wxGenericDirCtrl", "getFilter", 1}, // 1614
  {wxGenericDirCtrl_GetFilterIndex, "wxGenericDirCtrl", "getFilterIndex", 1}, // 1615
  {wxGenericDirCtrl_GetRootId, "wxGenericDirCtrl", "getRootId", 1}, // 1616
  {wxGenericDirCtrl_GetTreeCtrl, "wxGenericDirCtrl", "getTreeCtrl", 1}, // 1617
  {wxGenericDirCtrl_ReCreateTree, "wxGenericDirCtrl", "reCreateTree", 1}, // 1618
  {wxGenericDirCtrl_SetDefaultPath, "wxGenericDirCtrl", "setDefaultPath", 2}, // 1619
  {wxGenericDirCtrl_SetFilter, "wxGenericDirCtrl", "setFilter", 2}, // 1620
  {wxGenericDirCtrl_SetFilterIndex, "wxGenericDirCtrl", "setFilterIndex", 2}, // 1621
  {wxGenericDirCtrl_SetPath, "wxGenericDirCtrl", "setPath", 2}, // 1622
  {wxStaticBox_new_0, "wxStaticBox", "new", 0}, // 1623
  {wxStaticBox_new_4, "wxStaticBox", "new", 4}, // 1624
  {NULL, "wxStaticBox", "destroy", 1}, // 1625 obj destructor wxStaticBox_destruct
  {wxStaticBox_Create, "wxStaticBox", "create", 5}, // 1626
  {wxStaticLine_new_0, "wxStaticLine", "new", 0}, // 1627
  {wxStaticLine_new_2, "wxStaticLine", "new", 2}, // 1628
  {wxStaticLine_Create, "wxStaticLine", "create", 3}, // 1629
  {wxStaticLine_IsVertical, "wxStaticLine", "isVertical", 1}, // 1630
  {wxStaticLine_GetDefaultSize, "wxStaticLine", "getDefaultSize", 0}, // 1631
  {NULL, "wxStaticLine", "'Destroy'", 1}, // 1632 obj destructor wxStaticLine_destroy
  {wxListBox_new_0, "wxListBox", "new", 0}, // 1633
  {NULL, "", "", 0}, // 1634
  {wxListBox_new_3, "wxListBox", "new", 3}, // 1635
  {NULL, "wxListBox", "destroy", 1}, // 1636 obj destructor wxListBox_destruct
  {NULL, "", "", 0}, // 1637
  {wxListBox_Create, "wxListBox", "create", 7}, // 1638
  {wxListBox_Deselect, "wxListBox", "deselect", 2}, // 1639
  {wxListBox_GetSelections, "wxListBox", "getSelections", 1}, // 1640
  {wxListBox_InsertItems, "wxListBox", "insertItems", 3}, // 1641
  {wxListBox_IsSelected, "wxListBox", "isSelected", 2}, // 1642
  {NULL, "", "", 0}, // 1643
  {NULL, "", "", 0}, // 1644
  {wxListBox_Set, "wxListBox", "set", 2}, // 1645
  {wxListBox_HitTest_1, "wxListBox", "hitTest", 2}, // 1646
  {wxListBox_HitTest_2, "wxListBox", "hitTest", 3}, // 1647
  {wxListBox_SetFirstItem_1_0, "wxListBox", "setFirstItem", 2}, // 1648
  {wxListBox_SetFirstItem_1_1, "wxListBox", "setFirstItem", 2}, // 1649
  {wxListCtrl_new_0, "wxListCtrl", "new", 0}, // 1650
  {NULL, "wxListCtrl", "new", 2}, // 1651 TaylorMade erl only wxListCtrl_new_2
  {NULL, "wxListCtrl", "destroy", 1}, // 1652 obj destructor wxListCtrl_destruct
  {wxListCtrl_Arrange, "wxListCtrl", "arrange", 2}, // 1653
  {wxListCtrl_AssignImageList, "wxListCtrl", "assignImageList", 3}, // 1654
  {wxListCtrl_ClearAll, "wxListCtrl", "clearAll", 1}, // 1655
  {wxListCtrl_Create, "wxListCtrl", "create", 3}, // 1656
  {wxListCtrl_DeleteAllItems, "wxListCtrl", "deleteAllItems", 1}, // 1657
  {wxListCtrl_DeleteColumn, "wxListCtrl", "deleteColumn", 2}, // 1658
  {wxListCtrl_DeleteItem, "wxListCtrl", "deleteItem", 2}, // 1659
  {wxListCtrl_EditLabel, "wxListCtrl", "editLabel", 2}, // 1660
  {wxListCtrl_EnsureVisible, "wxListCtrl", "ensureVisible", 2}, // 1661
  {wxListCtrl_FindItem_3_0, "wxListCtrl", "findItem", 4}, // 1662
  {wxListCtrl_FindItem_3_1, "wxListCtrl", "findItem", 4}, // 1663
  {wxListCtrl_GetColumn, "wxListCtrl", "getColumn", 3}, // 1664
  {wxListCtrl_GetColumnCount, "wxListCtrl", "getColumnCount", 1}, // 1665
  {wxListCtrl_GetColumnWidth, "wxListCtrl", "getColumnWidth", 2}, // 1666
  {wxListCtrl_GetCountPerPage, "wxListCtrl", "getCountPerPage", 1}, // 1667
  {wxListCtrl_GetEditControl, "wxListCtrl", "getEditControl", 1}, // 1668
  {wxListCtrl_GetImageList, "wxListCtrl", "getImageList", 2}, // 1669
  {wxListCtrl_GetItem, "wxListCtrl", "getItem", 2}, // 1670
  {wxListCtrl_GetItemBackgroundColour, "wxListCtrl", "getItemBackgroundColour", 2}, // 1671
  {wxListCtrl_GetItemCount, "wxListCtrl", "getItemCount", 1}, // 1672
  {wxListCtrl_GetItemData, "wxListCtrl", "getItemData", 2}, // 1673
  {wxListCtrl_GetItemFont, "wxListCtrl", "getItemFont", 2}, // 1674
  {wxListCtrl_GetItemPosition, "wxListCtrl", "getItemPosition", 2}, // 1675
  {wxListCtrl_GetItemRect, "wxListCtrl", "getItemRect", 3}, // 1676
  {wxListCtrl_GetItemSpacing, "wxListCtrl", "getItemSpacing", 1}, // 1677
  {wxListCtrl_GetItemState, "wxListCtrl", "getItemState", 3}, // 1678
  {wxListCtrl_GetItemText, "wxListCtrl", "getItemText", 3}, // 1679
  {wxListCtrl_GetItemTextColour, "wxListCtrl", "getItemTextColour", 2}, // 1680
  {wxListCtrl_GetNextItem, "wxListCtrl", "getNextItem", 3}, // 1681
  {wxListCtrl_GetSelectedItemCount, "wxListCtrl", "getSelectedItemCount", 1}, // 1682
  {wxListCtrl_GetTextColour, "wxListCtrl", "getTextColour", 1}, // 1683
  {wxListCtrl_GetTopItem, "wxListCtrl", "getTopItem", 1}, // 1684
  {wxListCtrl_GetViewRect, "wxListCtrl", "getViewRect", 1}, // 1685
  {wxListCtrl_HitTest, "wxListCtrl", "hitTest", 2}, // 1686
  {wxListCtrl_InsertColumn_2, "wxListCtrl", "insertColumn", 3}, // 1687
  {wxListCtrl_InsertColumn_3, "wxListCtrl", "insertColumn", 4}, // 1688
  {wxListCtrl_InsertItem_1, "wxListCtrl", "insertItem", 2}, // 1689
  {wxListCtrl_InsertItem_2_1, "wxListCtrl", "insertItem", 3}, // 1690
  {wxListCtrl_InsertItem_2_0, "wxListCtrl", "insertItem", 3}, // 1691
  {wxListCtrl_InsertItem_3, "wxListCtrl", "insertItem", 4}, // 1692
  {wxListCtrl_RefreshItem, "wxListCtrl", "refreshItem", 2}, // 1693
  {wxListCtrl_RefreshItems, "wxListCtrl", "refreshItems", 3}, // 1694
  {wxListCtrl_ScrollList, "wxListCtrl", "scrollList", 3}, // 1695
  {wxListCtrl_SetBackgroundColour, "wxListCtrl", "setBackgroundColour", 2}, // 1696
  {wxListCtrl_SetColumn, "wxListCtrl", "setColumn", 3}, // 1697
  {wxListCtrl_SetColumnWidth, "wxListCtrl", "setColumnWidth", 3}, // 1698
  {wxListCtrl_SetImageList, "wxListCtrl", "setImageList", 3}, // 1699
  {wxListCtrl_SetItem_1, "wxListCtrl", "setItem", 2}, // 1700
  {wxListCtrl_SetItem_4, "wxListCtrl", "setItem", 5}, // 1701
  {wxListCtrl_SetItemBackgroundColour, "wxListCtrl", "setItemBackgroundColour", 3}, // 1702
  {wxListCtrl_SetItemCount, "wxListCtrl", "setItemCount", 2}, // 1703
  {wxListCtrl_SetItemData, "wxListCtrl", "setItemData", 3}, // 1704
  {wxListCtrl_SetItemFont, "wxListCtrl", "setItemFont", 3}, // 1705
  {wxListCtrl_SetItemImage, "wxListCtrl", "setItemImage", 4}, // 1706
  {wxListCtrl_SetItemColumnImage, "wxListCtrl", "setItemColumnImage", 4}, // 1707
  {wxListCtrl_SetItemPosition, "wxListCtrl", "setItemPosition", 3}, // 1708
  {wxListCtrl_SetItemState, "wxListCtrl", "setItemState", 4}, // 1709
  {wxListCtrl_SetItemText, "wxListCtrl", "setItemText", 3}, // 1710
  {wxListCtrl_SetItemTextColour, "wxListCtrl", "setItemTextColour", 3}, // 1711
  {wxListCtrl_SetSingleStyle, "wxListCtrl", "setSingleStyle", 3}, // 1712
  {wxListCtrl_SetTextColour, "wxListCtrl", "setTextColour", 2}, // 1713
  {wxListCtrl_SetWindowStyleFlag, "wxListCtrl", "setWindowStyleFlag", 2}, // 1714
  {wxListCtrl_SortItems, "wxListCtrl", "sortItems", 3}, // 1715
  {wxListView_ClearColumnImage, "wxListView", "clearColumnImage", 2}, // 1716
  {wxListView_Focus, "wxListView", "focus", 2}, // 1717
  {wxListView_GetFirstSelected, "wxListView", "getFirstSelected", 1}, // 1718
  {wxListView_GetFocusedItem, "wxListView", "getFocusedItem", 1}, // 1719
  {wxListView_GetNextSelected, "wxListView", "getNextSelected", 2}, // 1720
  {wxListView_IsSelected, "wxListView", "isSelected", 2}, // 1721
  {wxListView_Select, "wxListView", "select", 3}, // 1722
  {wxListView_SetColumnImage, "wxListView", "setColumnImage", 3}, // 1723
  {wxListItem_new_0, "wxListItem", "new", 0}, // 1724
  {wxListItem_new_1, "wxListItem", "new", 1}, // 1725
  {wxListItem_Clear, "wxListItem", "clear", 1}, // 1726
  {wxListItem_GetAlign, "wxListItem", "getAlign", 1}, // 1727
  {wxListItem_GetBackgroundColour, "wxListItem", "getBackgroundColour", 1}, // 1728
  {wxListItem_GetColumn, "wxListItem", "getColumn", 1}, // 1729
  {wxListItem_GetFont, "wxListItem", "getFont", 1}, // 1730
  {wxListItem_GetId, "wxListItem", "getId", 1}, // 1731
  {wxListItem_GetImage, "wxListItem", "getImage", 1}, // 1732
  {wxListItem_GetMask, "wxListItem", "getMask", 1}, // 1733
  {wxListItem_GetState, "wxListItem", "getState", 1}, // 1734
  {wxListItem_GetText, "wxListItem", "getText", 1}, // 1735
  {wxListItem_GetTextColour, "wxListItem", "getTextColour", 1}, // 1736
  {wxListItem_GetWidth, "wxListItem", "getWidth", 1}, // 1737
  {wxListItem_SetAlign, "wxListItem", "setAlign", 2}, // 1738
  {wxListItem_SetBackgroundColour, "wxListItem", "setBackgroundColour", 2}, // 1739
  {wxListItem_SetColumn, "wxListItem", "setColumn", 2}, // 1740
  {wxListItem_SetFont, "wxListItem", "setFont", 2}, // 1741
  {wxListItem_SetId, "wxListItem", "setId", 2}, // 1742
  {wxListItem_SetImage, "wxListItem", "setImage", 2}, // 1743
  {wxListItem_SetMask, "wxListItem", "setMask", 2}, // 1744
  {wxListItem_SetState, "wxListItem", "setState", 2}, // 1745
  {wxListItem_SetStateMask, "wxListItem", "setStateMask", 2}, // 1746
  {wxListItem_SetText, "wxListItem", "setText", 2}, // 1747
  {wxListItem_SetTextColour, "wxListItem", "setTextColour", 2}, // 1748
  {wxListItem_SetWidth, "wxListItem", "setWidth", 2}, // 1749
  {NULL, "wxListItem", "'Destroy'", 1}, // 1750 obj destructor wxListItem_destroy
  {wxListItemAttr_new_0, "wxListItemAttr", "new", 0}, // 1751
  {wxListItemAttr_new_3, "wxListItemAttr", "new", 3}, // 1752
  {wxListItemAttr_GetBackgroundColour, "wxListItemAttr", "getBackgroundColour", 1}, // 1753
  {wxListItemAttr_GetFont, "wxListItemAttr", "getFont", 1}, // 1754
  {wxListItemAttr_GetTextColour, "wxListItemAttr", "getTextColour", 1}, // 1755
  {wxListItemAttr_HasBackgroundColour, "wxListItemAttr", "hasBackgroundColour", 1}, // 1756
  {wxListItemAttr_HasFont, "wxListItemAttr", "hasFont", 1}, // 1757
  {wxListItemAttr_HasTextColour, "wxListItemAttr", "hasTextColour", 1}, // 1758
  {wxListItemAttr_SetBackgroundColour, "wxListItemAttr", "setBackgroundColour", 2}, // 1759
  {wxListItemAttr_SetFont, "wxListItemAttr", "setFont", 2}, // 1760
  {wxListItemAttr_SetTextColour, "wxListItemAttr", "setTextColour", 2}, // 1761
  {wxListItemAttr_destroy, "wxListItemAttr", "'Destroy'", 1}, // 1762
  {wxImageList_new_0, "wxImageList", "new", 0}, // 1763
  {wxImageList_new_3, "wxImageList", "new", 3}, // 1764
  {wxImageList_Add_2_1, "wxImageList", "add", 3}, // 1765
  {NULL, "", "", 0}, // 1766
  {wxImageList_Add_2_0, "wxImageList", "add", 3}, // 1767
  {wxImageList_Add_1, "wxImageList", "add", 2}, // 1768
  {wxImageList_Create, "wxImageList", "create", 4}, // 1769
  {wxImageList_Draw, "wxImageList", "draw", 6}, // 1770
  {wxImageList_GetBitmap, "wxImageList", "getBitmap", 2}, // 1771
  {wxImageList_GetIcon, "wxImageList", "getIcon", 2}, // 1772
  {wxImageList_GetImageCount, "wxImageList", "getImageCount", 1}, // 1773
  {wxImageList_GetSize, "wxImageList", "getSize", 2}, // 1774
  {NULL, "", "", 0}, // 1775
  {wxImageList_Remove, "wxImageList", "remove", 2}, // 1776
  {wxImageList_RemoveAll, "wxImageList", "removeAll", 1}, // 1777
  {wxImageList_Replace_3, "wxImageList", "replace", 4}, // 1778
  {NULL, "", "", 0}, // 1779
  {wxImageList_Replace_2, "wxImageList", "replace", 3}, // 1780
  {NULL, "wxImageList", "'Destroy'", 1}, // 1781 obj destructor wxImageList_destroy
  {wxTextAttr_new_0, "wxTextAttr", "new", 0}, // 1782
  {wxTextAttr_new_2, "wxTextAttr", "new", 2}, // 1783
  {wxTextAttr_new_1, "wxTextAttr", "new", 1}, // 1784
  {wxTextAttr_GetAlignment, "wxTextAttr", "getAlignment", 1}, // 1785
  {wxTextAttr_GetBackgroundColour, "wxTextAttr", "getBackgroundColour", 1}, // 1786
  {wxTextAttr_GetFont, "wxTextAttr", "getFont", 1}, // 1787
  {wxTextAttr_GetLeftIndent, "wxTextAttr", "getLeftIndent", 1}, // 1788
  {wxTextAttr_GetLeftSubIndent, "wxTextAttr", "getLeftSubIndent", 1}, // 1789
  {wxTextAttr_GetRightIndent, "wxTextAttr", "getRightIndent", 1}, // 1790
  {wxTextAttr_GetTabs, "wxTextAttr", "getTabs", 1}, // 1791
  {wxTextAttr_GetTextColour, "wxTextAttr", "getTextColour", 1}, // 1792
  {wxTextAttr_HasBackgroundColour, "wxTextAttr", "hasBackgroundColour", 1}, // 1793
  {wxTextAttr_HasFont, "wxTextAttr", "hasFont", 1}, // 1794
  {wxTextAttr_HasTextColour, "wxTextAttr", "hasTextColour", 1}, // 1795
  {wxTextAttr_GetFlags, "wxTextAttr", "getFlags", 1}, // 1796
  {wxTextAttr_IsDefault, "wxTextAttr", "isDefault", 1}, // 1797
  {wxTextAttr_SetAlignment, "wxTextAttr", "setAlignment", 2}, // 1798
  {wxTextAttr_SetBackgroundColour, "wxTextAttr", "setBackgroundColour", 2}, // 1799
  {wxTextAttr_SetFlags, "wxTextAttr", "setFlags", 2}, // 1800
  {wxTextAttr_SetFont, "wxTextAttr", "setFont", 3}, // 1801
  {wxTextAttr_SetLeftIndent, "wxTextAttr", "setLeftIndent", 3}, // 1802
  {wxTextAttr_SetRightIndent, "wxTextAttr", "setRightIndent", 2}, // 1803
  {wxTextAttr_SetTabs, "wxTextAttr", "setTabs", 2}, // 1804
  {wxTextAttr_SetTextColour, "wxTextAttr", "setTextColour", 2}, // 1805
  {wxTextAttr_destroy, "wxTextAttr", "'Destroy'", 1}, // 1806
  {wxTextCtrl_new_0, "wxTextCtrl", "new", 0}, // 1807
  {wxTextCtrl_new_3, "wxTextCtrl", "new", 3}, // 1808
  {NULL, "wxTextCtrl", "destroy", 1}, // 1809 obj destructor wxTextCtrl_destruct
  {wxTextCtrl_AppendText, "wxTextCtrl", "appendText", 2}, // 1810
  {wxTextCtrl_CanCopy, "wxTextCtrl", "canCopy", 1}, // 1811
  {wxTextCtrl_CanCut, "wxTextCtrl", "canCut", 1}, // 1812
  {wxTextCtrl_CanPaste, "wxTextCtrl", "canPaste", 1}, // 1813
  {wxTextCtrl_CanRedo, "wxTextCtrl", "canRedo", 1}, // 1814
  {wxTextCtrl_CanUndo, "wxTextCtrl", "canUndo", 1}, // 1815
  {wxTextCtrl_Clear, "wxTextCtrl", "clear", 1}, // 1816
  {wxTextCtrl_Copy, "wxTextCtrl", "copy", 1}, // 1817
  {wxTextCtrl_Create, "wxTextCtrl", "create", 4}, // 1818
  {wxTextCtrl_Cut, "wxTextCtrl", "cut", 1}, // 1819
  {wxTextCtrl_DiscardEdits, "wxTextCtrl", "discardEdits", 1}, // 1820
  {wxTextCtrl_ChangeValue, "wxTextCtrl", "changeValue", 2}, // 1821
  {wxTextCtrl_EmulateKeyPress, "wxTextCtrl", "emulateKeyPress", 2}, // 1822
  {wxTextCtrl_GetDefaultStyle, "wxTextCtrl", "getDefaultStyle", 1}, // 1823
  {wxTextCtrl_GetInsertionPoint, "wxTextCtrl", "getInsertionPoint", 1}, // 1824
  {wxTextCtrl_GetLastPosition, "wxTextCtrl", "getLastPosition", 1}, // 1825
  {wxTextCtrl_GetLineLength, "wxTextCtrl", "getLineLength", 2}, // 1826
  {wxTextCtrl_GetLineText, "wxTextCtrl", "getLineText", 2}, // 1827
  {wxTextCtrl_GetNumberOfLines, "wxTextCtrl", "getNumberOfLines", 1}, // 1828
  {wxTextCtrl_GetRange, "wxTextCtrl", "getRange", 3}, // 1829
  {wxTextCtrl_GetSelection, "wxTextCtrl", "getSelection", 1}, // 1830
  {wxTextCtrl_GetStringSelection, "wxTextCtrl", "getStringSelection", 1}, // 1831
  {wxTextCtrl_GetStyle, "wxTextCtrl", "getStyle", 3}, // 1832
  {wxTextCtrl_GetValue, "wxTextCtrl", "getValue", 1}, // 1833
  {wxTextCtrl_IsEditable, "wxTextCtrl", "isEditable", 1}, // 1834
  {wxTextCtrl_IsModified, "wxTextCtrl", "isModified", 1}, // 1835
  {wxTextCtrl_IsMultiLine, "wxTextCtrl", "isMultiLine", 1}, // 1836
  {wxTextCtrl_IsSingleLine, "wxTextCtrl", "isSingleLine", 1}, // 1837
  {wxTextCtrl_LoadFile, "wxTextCtrl", "loadFile", 3}, // 1838
  {wxTextCtrl_MarkDirty, "wxTextCtrl", "markDirty", 1}, // 1839
  {wxTextCtrl_Paste, "wxTextCtrl", "paste", 1}, // 1840
  {wxTextCtrl_PositionToXY, "wxTextCtrl", "positionToXY", 2}, // 1841
  {wxTextCtrl_Redo, "wxTextCtrl", "redo", 1}, // 1842
  {wxTextCtrl_Remove, "wxTextCtrl", "remove", 3}, // 1843
  {wxTextCtrl_Replace, "wxTextCtrl", "replace", 4}, // 1844
  {wxTextCtrl_SaveFile, "wxTextCtrl", "saveFile", 2}, // 1845
  {wxTextCtrl_SetDefaultStyle, "wxTextCtrl", "setDefaultStyle", 2}, // 1846
  {wxTextCtrl_SetEditable, "wxTextCtrl", "setEditable", 2}, // 1847
  {wxTextCtrl_SetInsertionPoint, "wxTextCtrl", "setInsertionPoint", 2}, // 1848
  {wxTextCtrl_SetInsertionPointEnd, "wxTextCtrl", "setInsertionPointEnd", 1}, // 1849
  {wxTextCtrl_SetMaxLength, "wxTextCtrl", "setMaxLength", 2}, // 1850
  {wxTextCtrl_SetSelection, "wxTextCtrl", "setSelection", 3}, // 1851
  {wxTextCtrl_SetStyle, "wxTextCtrl", "setStyle", 4}, // 1852
  {wxTextCtrl_SetValue, "wxTextCtrl", "setValue", 2}, // 1853
  {wxTextCtrl_ShowPosition, "wxTextCtrl", "showPosition", 2}, // 1854
  {wxTextCtrl_Undo, "wxTextCtrl", "undo", 1}, // 1855
  {wxTextCtrl_WriteText, "wxTextCtrl", "writeText", 2}, // 1856
  {wxTextCtrl_XYToPosition, "wxTextCtrl", "xYToPosition", 3}, // 1857
  {wxBookCtrlBase_AddPage, "wxBookCtrlBase", "addPage", 4}, // 1858
  {wxBookCtrlBase_InsertPage, "wxBookCtrlBase", "insertPage", 5}, // 1859
  {wxBookCtrlBase_DeletePage, "wxBookCtrlBase", "deletePage", 2}, // 1860
  {wxBookCtrlBase_RemovePage, "wxBookCtrlBase", "removePage", 2}, // 1861
  {wxBookCtrlBase_DeleteAllPages, "wxBookCtrlBase", "deleteAllPages", 1}, // 1862
  {wxBookCtrlBase_GetPage, "wxBookCtrlBase", "getPage", 2}, // 1863
  {wxBookCtrlBase_GetPageCount, "wxBookCtrlBase", "getPageCount", 1}, // 1864
  {wxBookCtrlBase_GetCurrentPage, "wxBookCtrlBase", "getCurrentPage", 1}, // 1865
  {wxBookCtrlBase_AdvanceSelection, "wxBookCtrlBase", "advanceSelection", 2}, // 1866
  {wxBookCtrlBase_SetSelection, "wxBookCtrlBase", "setSelection", 2}, // 1867
  {wxBookCtrlBase_GetSelection, "wxBookCtrlBase", "getSelection", 1}, // 1868
  {wxBookCtrlBase_ChangeSelection, "wxBookCtrlBase", "changeSelection", 2}, // 1869
  {wxBookCtrlBase_HitTest, "wxBookCtrlBase", "hitTest", 2}, // 1870
  {wxBookCtrlBase_GetPageText, "wxBookCtrlBase", "getPageText", 2}, // 1871
  {wxBookCtrlBase_SetPageText, "wxBookCtrlBase", "setPageText", 3}, // 1872
  {wxNotebook_new_0, "wxNotebook", "new", 0}, // 1873
  {wxNotebook_new_3, "wxNotebook", "new", 3}, // 1874
  {NULL, "wxNotebook", "destroy", 1}, // 1875 obj destructor wxNotebook_destruct
  {wxNotebook_AssignImageList, "wxNotebook", "assignImageList", 2}, // 1876
  {wxNotebook_Create, "wxNotebook", "create", 4}, // 1877
  {wxNotebook_GetImageList, "wxNotebook", "getImageList", 1}, // 1878
  {wxNotebook_GetPageImage, "wxNotebook", "getPageImage", 2}, // 1879
  {wxNotebook_GetRowCount, "wxNotebook", "getRowCount", 1}, // 1880
  {wxNotebook_GetThemeBackgroundColour, "wxNotebook", "getThemeBackgroundColour", 1}, // 1881
  {wxNotebook_SetImageList, "wxNotebook", "setImageList", 2}, // 1882
  {wxNotebook_SetPadding, "wxNotebook", "setPadding", 2}, // 1883
  {wxNotebook_SetPageSize, "wxNotebook", "setPageSize", 2}, // 1884
  {wxNotebook_SetPageImage, "wxNotebook", "setPageImage", 3}, // 1885
  {wxChoicebook_new_0, "wxChoicebook", "new", 0}, // 1886
  {wxChoicebook_new_3, "wxChoicebook", "new", 3}, // 1887
  {wxChoicebook_AddPage, "wxChoicebook", "addPage", 4}, // 1888
  {wxChoicebook_AdvanceSelection, "wxChoicebook", "advanceSelection", 2}, // 1889
  {wxChoicebook_AssignImageList, "wxChoicebook", "assignImageList", 2}, // 1890
  {wxChoicebook_Create, "wxChoicebook", "create", 4}, // 1891
  {wxChoicebook_DeleteAllPages, "wxChoicebook", "deleteAllPages", 1}, // 1892
  {wxChoicebook_GetCurrentPage, "wxChoicebook", "getCurrentPage", 1}, // 1893
  {wxChoicebook_GetImageList, "wxChoicebook", "getImageList", 1}, // 1894
  {wxChoicebook_GetPage, "wxChoicebook", "getPage", 2}, // 1895
  {wxChoicebook_GetPageCount, "wxChoicebook", "getPageCount", 1}, // 1896
  {wxChoicebook_GetPageImage, "wxChoicebook", "getPageImage", 2}, // 1897
  {wxChoicebook_GetPageText, "wxChoicebook", "getPageText", 2}, // 1898
  {wxChoicebook_GetSelection, "wxChoicebook", "getSelection", 1}, // 1899
  {wxChoicebook_HitTest, "wxChoicebook", "hitTest", 2}, // 1900
  {wxChoicebook_InsertPage, "wxChoicebook", "insertPage", 5}, // 1901
  {wxChoicebook_SetImageList, "wxChoicebook", "setImageList", 2}, // 1902
  {wxChoicebook_SetPageSize, "wxChoicebook", "setPageSize", 2}, // 1903
  {wxChoicebook_SetPageImage, "wxChoicebook", "setPageImage", 3}, // 1904
  {wxChoicebook_SetPageText, "wxChoicebook", "setPageText", 3}, // 1905
  {wxChoicebook_SetSelection, "wxChoicebook", "setSelection", 2}, // 1906
  {wxChoicebook_ChangeSelection, "wxChoicebook", "changeSelection", 2}, // 1907
  {NULL, "wxChoicebook", "'Destroy'", 1}, // 1908 obj destructor wxChoicebook_destroy
  {wxToolbook_new_0, "wxToolbook", "new", 0}, // 1909
  {wxToolbook_new_3, "wxToolbook", "new", 3}, // 1910
  {wxToolbook_AddPage, "wxToolbook", "addPage", 4}, // 1911
  {wxToolbook_AdvanceSelection, "wxToolbook", "advanceSelection", 2}, // 1912
  {wxToolbook_AssignImageList, "wxToolbook", "assignImageList", 2}, // 1913
  {wxToolbook_Create, "wxToolbook", "create", 4}, // 1914
  {wxToolbook_DeleteAllPages, "wxToolbook", "deleteAllPages", 1}, // 1915
  {wxToolbook_GetCurrentPage, "wxToolbook", "getCurrentPage", 1}, // 1916
  {wxToolbook_GetImageList, "wxToolbook", "getImageList", 1}, // 1917
  {wxToolbook_GetPage, "wxToolbook", "getPage", 2}, // 1918
  {wxToolbook_GetPageCount, "wxToolbook", "getPageCount", 1}, // 1919
  {wxToolbook_GetPageImage, "wxToolbook", "getPageImage", 2}, // 1920
  {wxToolbook_GetPageText, "wxToolbook", "getPageText", 2}, // 1921
  {wxToolbook_GetSelection, "wxToolbook", "getSelection", 1}, // 1922
  {wxToolbook_HitTest, "wxToolbook", "hitTest", 2}, // 1923
  {wxToolbook_InsertPage, "wxToolbook", "insertPage", 5}, // 1924
  {wxToolbook_SetImageList, "wxToolbook", "setImageList", 2}, // 1925
  {wxToolbook_SetPageSize, "wxToolbook", "setPageSize", 2}, // 1926
  {wxToolbook_SetPageImage, "wxToolbook", "setPageImage", 3}, // 1927
  {wxToolbook_SetPageText, "wxToolbook", "setPageText", 3}, // 1928
  {wxToolbook_SetSelection, "wxToolbook", "setSelection", 2}, // 1929
  {wxToolbook_ChangeSelection, "wxToolbook", "changeSelection", 2}, // 1930
  {NULL, "wxToolbook", "'Destroy'", 1}, // 1931 obj destructor wxToolbook_destroy
  {wxListbook_new_0, "wxListbook", "new", 0}, // 1932
  {wxListbook_new_3, "wxListbook", "new", 3}, // 1933
  {wxListbook_AddPage, "wxListbook", "addPage", 4}, // 1934
  {wxListbook_AdvanceSelection, "wxListbook", "advanceSelection", 2}, // 1935
  {wxListbook_AssignImageList, "wxListbook", "assignImageList", 2}, // 1936
  {wxListbook_Create, "wxListbook", "create", 4}, // 1937
  {wxListbook_DeleteAllPages, "wxListbook", "deleteAllPages", 1}, // 1938
  {wxListbook_GetCurrentPage, "wxListbook", "getCurrentPage", 1}, // 1939
  {wxListbook_GetImageList, "wxListbook", "getImageList", 1}, // 1940
  {wxListbook_GetPage, "wxListbook", "getPage", 2}, // 1941
  {wxListbook_GetPageCount, "wxListbook", "getPageCount", 1}, // 1942
  {wxListbook_GetPageImage, "wxListbook", "getPageImage", 2}, // 1943
  {wxListbook_GetPageText, "wxListbook", "getPageText", 2}, // 1944
  {wxListbook_GetSelection, "wxListbook", "getSelection", 1}, // 1945
  {wxListbook_HitTest, "wxListbook", "hitTest", 2}, // 1946
  {wxListbook_InsertPage, "wxListbook", "insertPage", 5}, // 1947
  {wxListbook_SetImageList, "wxListbook", "setImageList", 2}, // 1948
  {wxListbook_SetPageSize, "wxListbook", "setPageSize", 2}, // 1949
  {wxListbook_SetPageImage, "wxListbook", "setPageImage", 3}, // 1950
  {wxListbook_SetPageText, "wxListbook", "setPageText", 3}, // 1951
  {wxListbook_SetSelection, "wxListbook", "setSelection", 2}, // 1952
  {wxListbook_ChangeSelection, "wxListbook", "changeSelection", 2}, // 1953
  {NULL, "wxListbook", "'Destroy'", 1}, // 1954 obj destructor wxListbook_destroy
  {wxTreebook_new_0, "wxTreebook", "new", 0}, // 1955
  {wxTreebook_new_3, "wxTreebook", "new", 3}, // 1956
  {NULL, "wxTreebook", "destroy", 1}, // 1957 obj destructor wxTreebook_destruct
  {wxTreebook_AddPage, "wxTreebook", "addPage", 4}, // 1958
  {wxTreebook_AdvanceSelection, "wxTreebook", "advanceSelection", 2}, // 1959
  {wxTreebook_AssignImageList, "wxTreebook", "assignImageList", 2}, // 1960
  {wxTreebook_Create, "wxTreebook", "create", 4}, // 1961
  {wxTreebook_DeleteAllPages, "wxTreebook", "deleteAllPages", 1}, // 1962
  {wxTreebook_GetCurrentPage, "wxTreebook", "getCurrentPage", 1}, // 1963
  {wxTreebook_GetImageList, "wxTreebook", "getImageList", 1}, // 1964
  {wxTreebook_GetPage, "wxTreebook", "getPage", 2}, // 1965
  {wxTreebook_GetPageCount, "wxTreebook", "getPageCount", 1}, // 1966
  {wxTreebook_GetPageImage, "wxTreebook", "getPageImage", 2}, // 1967
  {wxTreebook_GetPageText, "wxTreebook", "getPageText", 2}, // 1968
  {wxTreebook_GetSelection, "wxTreebook", "getSelection", 1}, // 1969
  {wxTreebook_ExpandNode, "wxTreebook", "expandNode", 3}, // 1970
  {wxTreebook_IsNodeExpanded, "wxTreebook", "isNodeExpanded", 2}, // 1971
  {wxTreebook_HitTest, "wxTreebook", "hitTest", 2}, // 1972
  {wxTreebook_InsertPage, "wxTreebook", "insertPage", 5}, // 1973
  {wxTreebook_InsertSubPage, "wxTreebook", "insertSubPage", 5}, // 1974
  {wxTreebook_SetImageList, "wxTreebook", "setImageList", 2}, // 1975
  {wxTreebook_SetPageSize, "wxTreebook", "setPageSize", 2}, // 1976
  {wxTreebook_SetPageImage, "wxTreebook", "setPageImage", 3}, // 1977
  {wxTreebook_SetPageText, "wxTreebook", "setPageText", 3}, // 1978
  {wxTreebook_SetSelection, "wxTreebook", "setSelection", 2}, // 1979
  {wxTreebook_ChangeSelection, "wxTreebook", "changeSelection", 2}, // 1980
  {wxTreeCtrl_new_0, "wxTreeCtrl", "new", 0}, // 1981
  {wxTreeCtrl_new_2, "wxTreeCtrl", "new", 2}, // 1982
  {NULL, "wxTreeCtrl", "destroy", 1}, // 1983 obj destructor wxTreeCtrl_destruct
  {wxTreeCtrl_AddRoot, "wxTreeCtrl", "addRoot", 3}, // 1984
  {wxTreeCtrl_AppendItem, "wxTreeCtrl", "appendItem", 4}, // 1985
  {wxTreeCtrl_AssignImageList, "wxTreeCtrl", "assignImageList", 2}, // 1986
  {wxTreeCtrl_AssignStateImageList, "wxTreeCtrl", "assignStateImageList", 2}, // 1987
  {wxTreeCtrl_Collapse, "wxTreeCtrl", "collapse", 2}, // 1988
  {wxTreeCtrl_CollapseAndReset, "wxTreeCtrl", "collapseAndReset", 2}, // 1989
  {wxTreeCtrl_Create, "wxTreeCtrl", "create", 3}, // 1990
  {wxTreeCtrl_Delete, "wxTreeCtrl", "delete", 2}, // 1991
  {wxTreeCtrl_DeleteAllItems, "wxTreeCtrl", "deleteAllItems", 1}, // 1992
  {wxTreeCtrl_DeleteChildren, "wxTreeCtrl", "deleteChildren", 2}, // 1993
  {wxTreeCtrl_EditLabel, "wxTreeCtrl", "editLabel", 2}, // 1994
  {wxTreeCtrl_EnsureVisible, "wxTreeCtrl", "ensureVisible", 2}, // 1995
  {wxTreeCtrl_Expand, "wxTreeCtrl", "expand", 2}, // 1996
  {wxTreeCtrl_GetBoundingRect, "wxTreeCtrl", "getBoundingRect", 3}, // 1997
  {wxTreeCtrl_GetChildrenCount, "wxTreeCtrl", "getChildrenCount", 3}, // 1998
  {wxTreeCtrl_GetCount, "wxTreeCtrl", "getCount", 1}, // 1999
  {wxTreeCtrl_GetEditControl, "wxTreeCtrl", "getEditControl", 1}, // 2000
  {wxTreeCtrl_GetFirstChild, "wxTreeCtrl", "getFirstChild", 2}, // 2001
  {wxTreeCtrl_GetNextChild, "wxTreeCtrl", "getNextChild", 3}, // 2002
  {wxTreeCtrl_GetFirstVisibleItem, "wxTreeCtrl", "getFirstVisibleItem", 1}, // 2003
  {wxTreeCtrl_GetImageList, "wxTreeCtrl", "getImageList", 1}, // 2004
  {wxTreeCtrl_GetIndent, "wxTreeCtrl", "getIndent", 1}, // 2005
  {wxTreeCtrl_GetItemBackgroundColour, "wxTreeCtrl", "getItemBackgroundColour", 2}, // 2006
  {wxTreeCtrl_GetItemData, "wxTreeCtrl", "getItemData", 2}, // 2007
  {wxTreeCtrl_GetItemFont, "wxTreeCtrl", "getItemFont", 2}, // 2008
  {wxTreeCtrl_GetItemImage, "wxTreeCtrl", "getItemImage", 3}, // 2009
  {wxTreeCtrl_GetItemText, "wxTreeCtrl", "getItemText", 2}, // 2010
  {wxTreeCtrl_GetItemTextColour, "wxTreeCtrl", "getItemTextColour", 2}, // 2011
  {wxTreeCtrl_GetLastChild, "wxTreeCtrl", "getLastChild", 2}, // 2012
  {wxTreeCtrl_GetNextSibling, "wxTreeCtrl", "getNextSibling", 2}, // 2013
  {wxTreeCtrl_GetNextVisible, "wxTreeCtrl", "getNextVisible", 2}, // 2014
  {wxTreeCtrl_GetItemParent, "wxTreeCtrl", "getItemParent", 2}, // 2015
  {wxTreeCtrl_GetPrevSibling, "wxTreeCtrl", "getPrevSibling", 2}, // 2016
  {wxTreeCtrl_GetPrevVisible, "wxTreeCtrl", "getPrevVisible", 2}, // 2017
  {wxTreeCtrl_GetRootItem, "wxTreeCtrl", "getRootItem", 1}, // 2018
  {wxTreeCtrl_GetSelection, "wxTreeCtrl", "getSelection", 1}, // 2019
  {wxTreeCtrl_GetSelections, "wxTreeCtrl", "getSelections", 1}, // 2020
  {wxTreeCtrl_GetStateImageList, "wxTreeCtrl", "getStateImageList", 1}, // 2021
  {wxTreeCtrl_HitTest, "wxTreeCtrl", "hitTest", 2}, // 2022
  {wxTreeCtrl_InsertItem, "wxTreeCtrl", "insertItem", 5}, // 2023
  {NULL, "", "", 0}, // 2024
  {wxTreeCtrl_IsBold, "wxTreeCtrl", "isBold", 2}, // 2025
  {wxTreeCtrl_IsExpanded, "wxTreeCtrl", "isExpanded", 2}, // 2026
  {wxTreeCtrl_IsSelected, "wxTreeCtrl", "isSelected", 2}, // 2027
  {wxTreeCtrl_IsVisible, "wxTreeCtrl", "isVisible", 2}, // 2028
  {wxTreeCtrl_ItemHasChildren, "wxTreeCtrl", "itemHasChildren", 2}, // 2029
  {wxTreeCtrl_IsTreeItemIdOk, "wxTreeCtrl", "isTreeItemIdOk", 1}, // 2030
  {wxTreeCtrl_PrependItem, "wxTreeCtrl", "prependItem", 4}, // 2031
  {wxTreeCtrl_ScrollTo, "wxTreeCtrl", "scrollTo", 2}, // 2032
  {wxTreeCtrl_SelectItem, "wxTreeCtrl", "selectItem", 3}, // 2033
  {wxTreeCtrl_SetIndent, "wxTreeCtrl", "setIndent", 2}, // 2034
  {wxTreeCtrl_SetImageList, "wxTreeCtrl", "setImageList", 2}, // 2035
  {wxTreeCtrl_SetItemBackgroundColour, "wxTreeCtrl", "setItemBackgroundColour", 3}, // 2036
  {wxTreeCtrl_SetItemBold, "wxTreeCtrl", "setItemBold", 3}, // 2037
  {wxTreeCtrl_SetItemData, "wxTreeCtrl", "setItemData", 3}, // 2038
  {wxTreeCtrl_SetItemDropHighlight, "wxTreeCtrl", "setItemDropHighlight", 3}, // 2039
  {wxTreeCtrl_SetItemFont, "wxTreeCtrl", "setItemFont", 3}, // 2040
  {wxTreeCtrl_SetItemHasChildren, "wxTreeCtrl", "setItemHasChildren", 3}, // 2041
  {wxTreeCtrl_SetItemImage, "wxTreeCtrl", "setItemImage", 4}, // 2042
  {wxTreeCtrl_SetItemText, "wxTreeCtrl", "setItemText", 3}, // 2043
  {wxTreeCtrl_SetItemTextColour, "wxTreeCtrl", "setItemTextColour", 3}, // 2044
  {wxTreeCtrl_SetStateImageList, "wxTreeCtrl", "setStateImageList", 2}, // 2045
  {wxTreeCtrl_SetWindowStyle, "wxTreeCtrl", "setWindowStyle", 2}, // 2046
  {wxTreeCtrl_SortChildren, "wxTreeCtrl", "sortChildren", 2}, // 2047
  {wxTreeCtrl_Toggle, "wxTreeCtrl", "toggle", 2}, // 2048
  {wxTreeCtrl_ToggleItemSelection, "wxTreeCtrl", "toggleItemSelection", 2}, // 2049
  {wxTreeCtrl_Unselect, "wxTreeCtrl", "unselect", 1}, // 2050
  {wxTreeCtrl_UnselectAll, "wxTreeCtrl", "unselectAll", 1}, // 2051
  {wxTreeCtrl_UnselectItem, "wxTreeCtrl", "unselectItem", 2}, // 2052
  {wxScrollBar_new_0, "wxScrollBar", "new", 0}, // 2053
  {wxScrollBar_new_3, "wxScrollBar", "new", 3}, // 2054
  {NULL, "wxScrollBar", "destroy", 1}, // 2055 obj destructor wxScrollBar_destruct
  {wxScrollBar_Create, "wxScrollBar", "create", 4}, // 2056
  {wxScrollBar_GetRange, "wxScrollBar", "getRange", 1}, // 2057
  {wxScrollBar_GetPageSize, "wxScrollBar", "getPageSize", 1}, // 2058
  {wxScrollBar_GetThumbPosition, "wxScrollBar", "getThumbPosition", 1}, // 2059
  {wxScrollBar_GetThumbSize, "wxScrollBar", "getThumbSize", 1}, // 2060
  {wxScrollBar_SetThumbPosition, "wxScrollBar", "setThumbPosition", 2}, // 2061
  {wxScrollBar_SetScrollbar, "wxScrollBar", "setScrollbar", 6}, // 2062
  {wxSpinButton_new_0, "wxSpinButton", "new", 0}, // 2063
  {wxSpinButton_new_2, "wxSpinButton", "new", 2}, // 2064
  {NULL, "wxSpinButton", "destroy", 1}, // 2065 obj destructor wxSpinButton_destruct
  {wxSpinButton_Create, "wxSpinButton", "create", 3}, // 2066
  {wxSpinButton_GetMax, "wxSpinButton", "getMax", 1}, // 2067
  {wxSpinButton_GetMin, "wxSpinButton", "getMin", 1}, // 2068
  {wxSpinButton_GetValue, "wxSpinButton", "getValue", 1}, // 2069
  {wxSpinButton_SetRange, "wxSpinButton", "setRange", 3}, // 2070
  {wxSpinButton_SetValue, "wxSpinButton", "setValue", 2}, // 2071
  {wxSpinCtrl_new_0, "wxSpinCtrl", "new", 0}, // 2072
  {wxSpinCtrl_new_2, "wxSpinCtrl", "new", 2}, // 2073
  {wxSpinCtrl_Create, "wxSpinCtrl", "create", 3}, // 2074
  {wxSpinCtrl_SetValue_1_1, "wxSpinCtrl", "setValue", 2}, // 2075
  {wxSpinCtrl_SetValue_1_0, "wxSpinCtrl", "setValue", 2}, // 2076
  {wxSpinCtrl_GetValue, "wxSpinCtrl", "getValue", 1}, // 2077
  {wxSpinCtrl_SetRange, "wxSpinCtrl", "setRange", 3}, // 2078
  {wxSpinCtrl_SetSelection, "wxSpinCtrl", "setSelection", 3}, // 2079
  {wxSpinCtrl_GetMin, "wxSpinCtrl", "getMin", 1}, // 2080
  {wxSpinCtrl_GetMax, "wxSpinCtrl", "getMax", 1}, // 2081
  {NULL, "wxSpinCtrl", "'Destroy'", 1}, // 2082 obj destructor wxSpinCtrl_destroy
  {wxStaticText_new_0, "wxStaticText", "new", 0}, // 2083
  {wxStaticText_new_4, "wxStaticText", "new", 4}, // 2084
  {wxStaticText_Create, "wxStaticText", "create", 5}, // 2085
  {wxStaticText_GetLabel, "wxStaticText", "getLabel", 1}, // 2086
  {wxStaticText_SetLabel, "wxStaticText", "setLabel", 2}, // 2087
  {wxStaticText_Wrap, "wxStaticText", "wrap", 2}, // 2088
  {NULL, "wxStaticText", "'Destroy'", 1}, // 2089 obj destructor wxStaticText_destroy
  {wxStaticBitmap_new_0, "wxStaticBitmap", "new", 0}, // 2090
  {wxStaticBitmap_new_4, "wxStaticBitmap", "new", 4}, // 2091
  {wxStaticBitmap_Create, "wxStaticBitmap", "create", 5}, // 2092
  {wxStaticBitmap_GetBitmap, "wxStaticBitmap", "getBitmap", 1}, // 2093
  {wxStaticBitmap_SetBitmap, "wxStaticBitmap", "setBitmap", 2}, // 2094
  {NULL, "wxStaticBitmap", "'Destroy'", 1}, // 2095 obj destructor wxStaticBitmap_destroy
  {wxRadioBox_new, "wxRadioBox", "new", 7}, // 2096
  {NULL, "wxRadioBox", "destroy", 1}, // 2097 obj destructor wxRadioBox_destruct
  {wxRadioBox_Create, "wxRadioBox", "create", 8}, // 2098
  {wxRadioBox_Enable_1, "wxRadioBox", "enable", 2}, // 2099
  {wxRadioBox_Enable_2, "wxRadioBox", "enable", 3}, // 2100
  {wxRadioBox_GetSelection, "wxRadioBox", "getSelection", 1}, // 2101
  {wxRadioBox_GetString, "wxRadioBox", "getString", 2}, // 2102
  {wxRadioBox_SetSelection, "wxRadioBox", "setSelection", 2}, // 2103
  {wxRadioBox_Show, "wxRadioBox", "show", 3}, // 2104
  {wxRadioBox_GetColumnCount, "wxRadioBox", "getColumnCount", 1}, // 2105
  {wxRadioBox_GetItemHelpText, "wxRadioBox", "getItemHelpText", 2}, // 2106
  {wxRadioBox_GetItemToolTip, "wxRadioBox", "getItemToolTip", 2}, // 2107
  {wxRadioBox_GetItemFromPoint, "wxRadioBox", "getItemFromPoint", 2}, // 2108
  {wxRadioBox_GetRowCount, "wxRadioBox", "getRowCount", 1}, // 2109
  {wxRadioBox_IsItemEnabled, "wxRadioBox", "isItemEnabled", 2}, // 2110
  {wxRadioBox_IsItemShown, "wxRadioBox", "isItemShown", 2}, // 2111
  {wxRadioBox_SetItemHelpText, "wxRadioBox", "setItemHelpText", 3}, // 2112
  {wxRadioBox_SetItemToolTip, "wxRadioBox", "setItemToolTip", 3}, // 2113
  {wxRadioButton_new_0, "wxRadioButton", "new", 0}, // 2114
  {wxRadioButton_new_4, "wxRadioButton", "new", 4}, // 2115
  {NULL, "wxRadioButton", "destroy", 1}, // 2116 obj destructor wxRadioButton_destruct
  {wxRadioButton_Create, "wxRadioButton", "create", 5}, // 2117
  {wxRadioButton_GetValue, "wxRadioButton", "getValue", 1}, // 2118
  {wxRadioButton_SetValue, "wxRadioButton", "setValue", 2}, // 2119
  {wxSlider_new_0, "wxSlider", "new", 0}, // 2120
  {wxSlider_new_6, "wxSlider", "new", 6}, // 2121
  {NULL, "wxSlider", "destroy", 1}, // 2122 obj destructor wxSlider_destruct
  {wxSlider_Create, "wxSlider", "create", 7}, // 2123
  {wxSlider_GetLineSize, "wxSlider", "getLineSize", 1}, // 2124
  {wxSlider_GetMax, "wxSlider", "getMax", 1}, // 2125
  {wxSlider_GetMin, "wxSlider", "getMin", 1}, // 2126
  {wxSlider_GetPageSize, "wxSlider", "getPageSize", 1}, // 2127
  {wxSlider_GetThumbLength, "wxSlider", "getThumbLength", 1}, // 2128
  {wxSlider_GetValue, "wxSlider", "getValue", 1}, // 2129
  {wxSlider_SetLineSize, "wxSlider", "setLineSize", 2}, // 2130
  {wxSlider_SetPageSize, "wxSlider", "setPageSize", 2}, // 2131
  {wxSlider_SetRange, "wxSlider", "setRange", 3}, // 2132
  {wxSlider_SetThumbLength, "wxSlider", "setThumbLength", 2}, // 2133
  {wxSlider_SetValue, "wxSlider", "setValue", 2}, // 2134
  {wxDialog_new_0, "wxDialog", "new", 0}, // 2135
  {wxDialog_new_4, "wxDialog", "new", 4}, // 2136
  {NULL, "wxDialog", "destroy", 1}, // 2137 obj destructor wxDialog_destruct
  {wxDialog_Create, "wxDialog", "create", 5}, // 2138
  {wxDialog_CreateButtonSizer, "wxDialog", "createButtonSizer", 2}, // 2139
  {wxDialog_CreateStdDialogButtonSizer, "wxDialog", "createStdDialogButtonSizer", 2}, // 2140
  {wxDialog_EndModal, "wxDialog", "endModal", 2}, // 2141
  {wxDialog_GetAffirmativeId, "wxDialog", "getAffirmativeId", 1}, // 2142
  {wxDialog_GetReturnCode, "wxDialog", "getReturnCode", 1}, // 2143
  {wxDialog_IsModal, "wxDialog", "isModal", 1}, // 2144
  {wxDialog_SetAffirmativeId, "wxDialog", "setAffirmativeId", 2}, // 2145
  {wxDialog_SetReturnCode, "wxDialog", "setReturnCode", 2}, // 2146
  {wxDialog_Show, "wxDialog", "show", 2}, // 2147
  {wxDialog_ShowModal, "wxDialog", "showModal", 1}, // 2148
  {wxColourDialog_new_0, "wxColourDialog", "new", 0}, // 2149
  {wxColourDialog_new_2, "wxColourDialog", "new", 2}, // 2150
  {NULL, "wxColourDialog", "destroy", 1}, // 2151 obj destructor wxColourDialog_destruct
  {wxColourDialog_Create, "wxColourDialog", "create", 3}, // 2152
  {wxColourDialog_GetColourData, "wxColourDialog", "getColourData", 1}, // 2153
  {wxColourData_new, "wxColourData", "new", 0}, // 2154
  {NULL, "wxColourData", "destroy", 1}, // 2155 obj destructor wxColourData_destruct
  {wxColourData_GetChooseFull, "wxColourData", "getChooseFull", 1}, // 2156
  {wxColourData_GetColour, "wxColourData", "getColour", 1}, // 2157
  {wxColourData_GetCustomColour, "wxColourData", "getCustomColour", 2}, // 2158
  {wxColourData_SetChooseFull, "wxColourData", "setChooseFull", 2}, // 2159
  {wxColourData_SetColour, "wxColourData", "setColour", 2}, // 2160
  {wxColourData_SetCustomColour, "wxColourData", "setCustomColour", 3}, // 2161
  {wxPalette_new_0, "wxPalette", "new", 0}, // 2162
  {wxPalette_new_1, "wxPalette", "new", 1}, // 2163
  {wxPalette_new_4, "wxPalette", "new", 3}, // 2164
  {NULL, "wxPalette", "destroy", 1}, // 2165 obj destructor wxPalette_destruct
  {wxPalette_Create, "wxPalette", "create", 4}, // 2166
  {wxPalette_GetColoursCount, "wxPalette", "getColoursCount", 1}, // 2167
  {wxPalette_GetPixel, "wxPalette", "getPixel", 4}, // 2168
  {wxPalette_GetRGB, "wxPalette", "getRGB", 2}, // 2169
  {wxPalette_IsOk, "wxPalette", "isOk", 1}, // 2170
  {wxDirDialog_new, "wxDirDialog", "new", 2}, // 2171
  {NULL, "wxDirDialog", "destroy", 1}, // 2172 obj destructor wxDirDialog_destruct
  {wxDirDialog_GetPath, "wxDirDialog", "getPath", 1}, // 2173
  {wxDirDialog_GetMessage, "wxDirDialog", "getMessage", 1}, // 2174
  {wxDirDialog_SetMessage, "wxDirDialog", "setMessage", 2}, // 2175
  {wxDirDialog_SetPath, "wxDirDialog", "setPath", 2}, // 2176
  {wxFileDialog_new, "wxFileDialog", "new", 2}, // 2177
  {NULL, "wxFileDialog", "destroy", 1}, // 2178 obj destructor wxFileDialog_destruct
  {wxFileDialog_GetDirectory, "wxFileDialog", "getDirectory", 1}, // 2179
  {wxFileDialog_GetFilename, "wxFileDialog", "getFilename", 1}, // 2180
  {wxFileDialog_GetFilenames, "wxFileDialog", "getFilenames", 1}, // 2181
  {wxFileDialog_GetFilterIndex, "wxFileDialog", "getFilterIndex", 1}, // 2182
  {wxFileDialog_GetMessage, "wxFileDialog", "getMessage", 1}, // 2183
  {wxFileDialog_GetPath, "wxFileDialog", "getPath", 1}, // 2184
  {wxFileDialog_GetPaths, "wxFileDialog", "getPaths", 1}, // 2185
  {wxFileDialog_GetWildcard, "wxFileDialog", "getWildcard", 1}, // 2186
  {wxFileDialog_SetDirectory, "wxFileDialog", "setDirectory", 2}, // 2187
  {wxFileDialog_SetFilename, "wxFileDialog", "setFilename", 2}, // 2188
  {wxFileDialog_SetFilterIndex, "wxFileDialog", "setFilterIndex", 2}, // 2189
  {wxFileDialog_SetMessage, "wxFileDialog", "setMessage", 2}, // 2190
  {wxFileDialog_SetPath, "wxFileDialog", "setPath", 2}, // 2191
  {wxFileDialog_SetWildcard, "wxFileDialog", "setWildcard", 2}, // 2192
  {wxPickerBase_SetInternalMargin, "wxPickerBase", "setInternalMargin", 2}, // 2193
  {wxPickerBase_GetInternalMargin, "wxPickerBase", "getInternalMargin", 1}, // 2194
  {wxPickerBase_SetTextCtrlProportion, "wxPickerBase", "setTextCtrlProportion", 2}, // 2195
  {wxPickerBase_SetPickerCtrlProportion, "wxPickerBase", "setPickerCtrlProportion", 2}, // 2196
  {wxPickerBase_GetTextCtrlProportion, "wxPickerBase", "getTextCtrlProportion", 1}, // 2197
  {wxPickerBase_GetPickerCtrlProportion, "wxPickerBase", "getPickerCtrlProportion", 1}, // 2198
  {wxPickerBase_HasTextCtrl, "wxPickerBase", "hasTextCtrl", 1}, // 2199
  {wxPickerBase_GetTextCtrl, "wxPickerBase", "getTextCtrl", 1}, // 2200
  {wxPickerBase_IsTextCtrlGrowable, "wxPickerBase", "isTextCtrlGrowable", 1}, // 2201
  {wxPickerBase_SetPickerCtrlGrowable, "wxPickerBase", "setPickerCtrlGrowable", 2}, // 2202
  {wxPickerBase_SetTextCtrlGrowable, "wxPickerBase", "setTextCtrlGrowable", 2}, // 2203
  {wxPickerBase_IsPickerCtrlGrowable, "wxPickerBase", "isPickerCtrlGrowable", 1}, // 2204
  {wxFilePickerCtrl_new_0, "wxFilePickerCtrl", "new", 0}, // 2205
  {wxFilePickerCtrl_new_3, "wxFilePickerCtrl", "new", 3}, // 2206
  {wxFilePickerCtrl_Create, "wxFilePickerCtrl", "create", 4}, // 2207
  {wxFilePickerCtrl_GetPath, "wxFilePickerCtrl", "getPath", 1}, // 2208
  {wxFilePickerCtrl_SetPath, "wxFilePickerCtrl", "setPath", 2}, // 2209
  {NULL, "wxFilePickerCtrl", "'Destroy'", 1}, // 2210 obj destructor wxFilePickerCtrl_destroy
  {wxDirPickerCtrl_new_0, "wxDirPickerCtrl", "new", 0}, // 2211
  {wxDirPickerCtrl_new_3, "wxDirPickerCtrl", "new", 3}, // 2212
  {wxDirPickerCtrl_Create, "wxDirPickerCtrl", "create", 4}, // 2213
  {wxDirPickerCtrl_GetPath, "wxDirPickerCtrl", "getPath", 1}, // 2214
  {wxDirPickerCtrl_SetPath, "wxDirPickerCtrl", "setPath", 2}, // 2215
  {NULL, "wxDirPickerCtrl", "'Destroy'", 1}, // 2216 obj destructor wxDirPickerCtrl_destroy
  {wxColourPickerCtrl_new_0, "wxColourPickerCtrl", "new", 0}, // 2217
  {wxColourPickerCtrl_new_3, "wxColourPickerCtrl", "new", 3}, // 2218
  {wxColourPickerCtrl_Create, "wxColourPickerCtrl", "create", 4}, // 2219
  {wxColourPickerCtrl_GetColour, "wxColourPickerCtrl", "getColour", 1}, // 2220
  {wxColourPickerCtrl_SetColour_1_1, "wxColourPickerCtrl", "setColour", 2}, // 2221
  {wxColourPickerCtrl_SetColour_1_0, "wxColourPickerCtrl", "setColour", 2}, // 2222
  {NULL, "wxColourPickerCtrl", "'Destroy'", 1}, // 2223 obj destructor wxColourPickerCtrl_destroy
  {wxDatePickerCtrl_new_0, "wxDatePickerCtrl", "new", 0}, // 2224
  {wxDatePickerCtrl_new_3, "wxDatePickerCtrl", "new", 3}, // 2225
  {wxDatePickerCtrl_GetRange, "wxDatePickerCtrl", "getRange", 3}, // 2226
  {wxDatePickerCtrl_GetValue, "wxDatePickerCtrl", "getValue", 1}, // 2227
  {wxDatePickerCtrl_SetRange, "wxDatePickerCtrl", "setRange", 3}, // 2228
  {wxDatePickerCtrl_SetValue, "wxDatePickerCtrl", "setValue", 2}, // 2229
  {NULL, "wxDatePickerCtrl", "'Destroy'", 1}, // 2230 obj destructor wxDatePickerCtrl_destroy
  {wxFontPickerCtrl_new_0, "wxFontPickerCtrl", "new", 0}, // 2231
  {wxFontPickerCtrl_new_3, "wxFontPickerCtrl", "new", 3}, // 2232
  {wxFontPickerCtrl_Create, "wxFontPickerCtrl", "create", 4}, // 2233
  {wxFontPickerCtrl_GetSelectedFont, "wxFontPickerCtrl", "getSelectedFont", 1}, // 2234
  {wxFontPickerCtrl_SetSelectedFont, "wxFontPickerCtrl", "setSelectedFont", 2}, // 2235
  {wxFontPickerCtrl_GetMaxPointSize, "wxFontPickerCtrl", "getMaxPointSize", 1}, // 2236
  {wxFontPickerCtrl_SetMaxPointSize, "wxFontPickerCtrl", "setMaxPointSize", 2}, // 2237
  {NULL, "wxFontPickerCtrl", "'Destroy'", 1}, // 2238 obj destructor wxFontPickerCtrl_destroy
  {wxFindReplaceDialog_new_0, "wxFindReplaceDialog", "new", 0}, // 2239
  {wxFindReplaceDialog_new_4, "wxFindReplaceDialog", "new", 4}, // 2240
  {NULL, "wxFindReplaceDialog", "destroy", 1}, // 2241 obj destructor wxFindReplaceDialog_destruct
  {wxFindReplaceDialog_Create, "wxFindReplaceDialog", "create", 5}, // 2242
  {wxFindReplaceDialog_GetData, "wxFindReplaceDialog", "getData", 1}, // 2243
  {wxFindReplaceData_new, "wxFindReplaceData", "new", 1}, // 2244
  {wxFindReplaceData_GetFindString, "wxFindReplaceData", "getFindString", 1}, // 2245
  {wxFindReplaceData_GetReplaceString, "wxFindReplaceData", "getReplaceString", 1}, // 2246
  {wxFindReplaceData_GetFlags, "wxFindReplaceData", "getFlags", 1}, // 2247
  {wxFindReplaceData_SetFlags, "wxFindReplaceData", "setFlags", 2}, // 2248
  {wxFindReplaceData_SetFindString, "wxFindReplaceData", "setFindString", 2}, // 2249
  {wxFindReplaceData_SetReplaceString, "wxFindReplaceData", "setReplaceString", 2}, // 2250
  {NULL, "wxFindReplaceData", "'Destroy'", 1}, // 2251 obj destructor wxFindReplaceData_destroy
  {NULL, "", "", 0}, // 2252
  {wxMultiChoiceDialog_new, "wxMultiChoiceDialog", "new", 5}, // 2253
  {wxMultiChoiceDialog_GetSelections, "wxMultiChoiceDialog", "getSelections", 1}, // 2254
  {wxMultiChoiceDialog_SetSelections, "wxMultiChoiceDialog", "setSelections", 2}, // 2255
  {NULL, "wxMultiChoiceDialog", "'Destroy'", 1}, // 2256 obj destructor wxMultiChoiceDialog_destroy
  {NULL, "", "", 0}, // 2257
  {wxSingleChoiceDialog_new, "wxSingleChoiceDialog", "new", 5}, // 2258
  {wxSingleChoiceDialog_GetSelection, "wxSingleChoiceDialog", "getSelection", 1}, // 2259
  {wxSingleChoiceDialog_GetStringSelection, "wxSingleChoiceDialog", "getStringSelection", 1}, // 2260
  {wxSingleChoiceDialog_SetSelection, "wxSingleChoiceDialog", "setSelection", 2}, // 2261
  {NULL, "wxSingleChoiceDialog", "'Destroy'", 1}, // 2262 obj destructor wxSingleChoiceDialog_destroy
  {wxTextEntryDialog_new_0, "wxTextEntryDialog", "new", 0}, // 2263
  {wxTextEntryDialog_new_3, "wxTextEntryDialog", "new", 3}, // 2264
  {NULL, "wxTextEntryDialog", "destroy", 1}, // 2265 obj destructor wxTextEntryDialog_destruct
  {wxTextEntryDialog_GetValue, "wxTextEntryDialog", "getValue", 1}, // 2266
  {wxTextEntryDialog_SetValue, "wxTextEntryDialog", "setValue", 2}, // 2267
  {wxPasswordEntryDialog_new, "wxPasswordEntryDialog", "new", 3}, // 2268
  {NULL, "wxPasswordEntryDialog", "'Destroy'", 1}, // 2269 obj destructor wxPasswordEntryDialog_destroy
  {wxFontData_new_0, "wxFontData", "new", 0}, // 2270
  {wxFontData_new_1, "wxFontData", "new", 1}, // 2271
  {wxFontData_EnableEffects, "wxFontData", "enableEffects", 2}, // 2272
  {wxFontData_GetAllowSymbols, "wxFontData", "getAllowSymbols", 1}, // 2273
  {wxFontData_GetColour, "wxFontData", "getColour", 1}, // 2274
  {wxFontData_GetChosenFont, "wxFontData", "getChosenFont", 1}, // 2275
  {wxFontData_GetEnableEffects, "wxFontData", "getEnableEffects", 1}, // 2276
  {wxFontData_GetInitialFont, "wxFontData", "getInitialFont", 1}, // 2277
  {wxFontData_GetShowHelp, "wxFontData", "getShowHelp", 1}, // 2278
  {wxFontData_SetAllowSymbols, "wxFontData", "setAllowSymbols", 2}, // 2279
  {wxFontData_SetChosenFont, "wxFontData", "setChosenFont", 2}, // 2280
  {wxFontData_SetColour, "wxFontData", "setColour", 2}, // 2281
  {wxFontData_SetInitialFont, "wxFontData", "setInitialFont", 2}, // 2282
  {wxFontData_SetRange, "wxFontData", "setRange", 3}, // 2283
  {wxFontData_SetShowHelp, "wxFontData", "setShowHelp", 2}, // 2284
  {NULL, "wxFontData", "'Destroy'", 1}, // 2285 obj destructor wxFontData_destroy
  {wxFontDialog_new_0, "wxFontDialog", "new", 0}, // 2286
  {NULL, "", "", 0}, // 2287
  {wxFontDialog_new_2, "wxFontDialog", "new", 2}, // 2288
  {NULL, "", "", 0}, // 2289
  {wxFontDialog_Create, "wxFontDialog", "create", 3}, // 2290
  {wxFontDialog_GetFontData, "wxFontDialog", "getFontData", 1}, // 2291
  {NULL, "", "", 0}, // 2292
  {NULL, "wxFontDialog", "'Destroy'", 1}, // 2293 obj destructor wxFontDialog_destroy
  {wxProgressDialog_new, "wxProgressDialog", "new", 3}, // 2294
  {wxProgressDialog_Resume, "wxProgressDialog", "resume", 1}, // 2295
  {wxProgressDialog_Update, "wxProgressDialog", "update", 3}, // 2296
  {NULL, "wxProgressDialog", "'Destroy'", 1}, // 2297 obj destructor wxProgressDialog_destroy
  {wxMessageDialog_new, "wxMessageDialog", "new", 3}, // 2298
  {NULL, "wxMessageDialog", "'Destroy'", 1}, // 2299 obj destructor wxMessageDialog_destroy
  {wxPageSetupDialog_new, "wxPageSetupDialog", "new", 2}, // 2300
  {NULL, "wxPageSetupDialog", "destroy", 1}, // 2301 obj destructor wxPageSetupDialog_destruct
  {wxPageSetupDialog_GetPageSetupData, "wxPageSetupDialog", "getPageSetupData", 1}, // 2302
  {wxPageSetupDialog_ShowModal, "wxPageSetupDialog", "showModal", 1}, // 2303
  {wxPageSetupDialogData_new_0, "wxPageSetupDialogData", "new", 0}, // 2304
  {NULL, "", "", 0}, // 2305
  {wxPageSetupDialogData_new_1, "wxPageSetupDialogData", "new", 1}, // 2306
  {NULL, "wxPageSetupDialogData", "destroy", 1}, // 2307 obj destructor wxPageSetupDialogData_destruct
  {wxPageSetupDialogData_EnableHelp, "wxPageSetupDialogData", "enableHelp", 2}, // 2308
  {wxPageSetupDialogData_EnableMargins, "wxPageSetupDialogData", "enableMargins", 2}, // 2309
  {wxPageSetupDialogData_EnableOrientation, "wxPageSetupDialogData", "enableOrientation", 2}, // 2310
  {wxPageSetupDialogData_EnablePaper, "wxPageSetupDialogData", "enablePaper", 2}, // 2311
  {wxPageSetupDialogData_EnablePrinter, "wxPageSetupDialogData", "enablePrinter", 2}, // 2312
  {wxPageSetupDialogData_GetDefaultMinMargins, "wxPageSetupDialogData", "getDefaultMinMargins", 1}, // 2313
  {wxPageSetupDialogData_GetEnableMargins, "wxPageSetupDialogData", "getEnableMargins", 1}, // 2314
  {wxPageSetupDialogData_GetEnableOrientation, "wxPageSetupDialogData", "getEnableOrientation", 1}, // 2315
  {wxPageSetupDialogData_GetEnablePaper, "wxPageSetupDialogData", "getEnablePaper", 1}, // 2316
  {wxPageSetupDialogData_GetEnablePrinter, "wxPageSetupDialogData", "getEnablePrinter", 1}, // 2317
  {wxPageSetupDialogData_GetEnableHelp, "wxPageSetupDialogData", "getEnableHelp", 1}, // 2318
  {wxPageSetupDialogData_GetDefaultInfo, "wxPageSetupDialogData", "getDefaultInfo", 1}, // 2319
  {wxPageSetupDialogData_GetMarginTopLeft, "wxPageSetupDialogData", "getMarginTopLeft", 1}, // 2320
  {wxPageSetupDialogData_GetMarginBottomRight, "wxPageSetupDialogData", "getMarginBottomRight", 1}, // 2321
  {wxPageSetupDialogData_GetMinMarginTopLeft, "wxPageSetupDialogData", "getMinMarginTopLeft", 1}, // 2322
  {wxPageSetupDialogData_GetMinMarginBottomRight, "wxPageSetupDialogData", "getMinMarginBottomRight", 1}, // 2323
  {wxPageSetupDialogData_GetPaperId, "wxPageSetupDialogData", "getPaperId", 1}, // 2324
  {wxPageSetupDialogData_GetPaperSize, "wxPageSetupDialogData", "getPaperSize", 1}, // 2325
  {NULL, "", "", 0}, // 2326
  {wxPageSetupDialogData_GetPrintData, "wxPageSetupDialogData", "getPrintData", 1}, // 2327
  {wxPageSetupDialogData_IsOk, "wxPageSetupDialogData", "isOk", 1}, // 2328
  {wxPageSetupDialogData_SetDefaultInfo, "wxPageSetupDialogData", "setDefaultInfo", 2}, // 2329
  {wxPageSetupDialogData_SetDefaultMinMargins, "wxPageSetupDialogData", "setDefaultMinMargins", 2}, // 2330
  {wxPageSetupDialogData_SetMarginTopLeft, "wxPageSetupDialogData", "setMarginTopLeft", 2}, // 2331
  {wxPageSetupDialogData_SetMarginBottomRight, "wxPageSetupDialogData", "setMarginBottomRight", 2}, // 2332
  {wxPageSetupDialogData_SetMinMarginTopLeft, "wxPageSetupDialogData", "setMinMarginTopLeft", 2}, // 2333
  {wxPageSetupDialogData_SetMinMarginBottomRight, "wxPageSetupDialogData", "setMinMarginBottomRight", 2}, // 2334
  {wxPageSetupDialogData_SetPaperId, "wxPageSetupDialogData", "setPaperId", 2}, // 2335
  {wxPageSetupDialogData_SetPaperSize, "wxPageSetupDialogData", "setPaperSize", 2}, // 2336
  {wxPageSetupDialogData_SetPrintData, "wxPageSetupDialogData", "setPrintData", 2}, // 2337
  {wxPrintDialog_new_2_0, "wxPrintDialog", "new", 2}, // 2338
  {wxPrintDialog_new_2_1, "wxPrintDialog", "new", 2}, // 2339
  {NULL, "wxPrintDialog", "destroy", 1}, // 2340 obj destructor wxPrintDialog_destruct
  {wxPrintDialog_GetPrintDialogData, "wxPrintDialog", "getPrintDialogData", 1}, // 2341
  {wxPrintDialog_GetPrintDC, "wxPrintDialog", "getPrintDC", 1}, // 2342
  {wxPrintDialogData_new_0, "wxPrintDialogData", "new", 0}, // 2343
  {wxPrintDialogData_new_1, "wxPrintDialogData", "new", 1}, // 2344
  {NULL, "", "", 0}, // 2345
  {NULL, "wxPrintDialogData", "destroy", 1}, // 2346 obj destructor wxPrintDialogData_destruct
  {wxPrintDialogData_EnableHelp, "wxPrintDialogData", "enableHelp", 2}, // 2347
  {wxPrintDialogData_EnablePageNumbers, "wxPrintDialogData", "enablePageNumbers", 2}, // 2348
  {wxPrintDialogData_EnablePrintToFile, "wxPrintDialogData", "enablePrintToFile", 2}, // 2349
  {wxPrintDialogData_EnableSelection, "wxPrintDialogData", "enableSelection", 2}, // 2350
  {wxPrintDialogData_GetAllPages, "wxPrintDialogData", "getAllPages", 1}, // 2351
  {wxPrintDialogData_GetCollate, "wxPrintDialogData", "getCollate", 1}, // 2352
  {wxPrintDialogData_GetFromPage, "wxPrintDialogData", "getFromPage", 1}, // 2353
  {wxPrintDialogData_GetMaxPage, "wxPrintDialogData", "getMaxPage", 1}, // 2354
  {wxPrintDialogData_GetMinPage, "wxPrintDialogData", "getMinPage", 1}, // 2355
  {wxPrintDialogData_GetNoCopies, "wxPrintDialogData", "getNoCopies", 1}, // 2356
  {wxPrintDialogData_GetPrintData, "wxPrintDialogData", "getPrintData", 1}, // 2357
  {wxPrintDialogData_GetPrintToFile, "wxPrintDialogData", "getPrintToFile", 1}, // 2358
  {wxPrintDialogData_GetSelection, "wxPrintDialogData", "getSelection", 1}, // 2359
  {wxPrintDialogData_GetToPage, "wxPrintDialogData", "getToPage", 1}, // 2360
  {wxPrintDialogData_IsOk, "wxPrintDialogData", "isOk", 1}, // 2361
  {wxPrintDialogData_SetCollate, "wxPrintDialogData", "setCollate", 2}, // 2362
  {wxPrintDialogData_SetFromPage, "wxPrintDialogData", "setFromPage", 2}, // 2363
  {wxPrintDialogData_SetMaxPage, "wxPrintDialogData", "setMaxPage", 2}, // 2364
  {wxPrintDialogData_SetMinPage, "wxPrintDialogData", "setMinPage", 2}, // 2365
  {wxPrintDialogData_SetNoCopies, "wxPrintDialogData", "setNoCopies", 2}, // 2366
  {wxPrintDialogData_SetPrintData, "wxPrintDialogData", "setPrintData", 2}, // 2367
  {wxPrintDialogData_SetPrintToFile, "wxPrintDialogData", "setPrintToFile", 2}, // 2368
  {wxPrintDialogData_SetSelection, "wxPrintDialogData", "setSelection", 2}, // 2369
  {wxPrintDialogData_SetToPage, "wxPrintDialogData", "setToPage", 2}, // 2370
  {wxPrintData_new_0, "wxPrintData", "new", 0}, // 2371
  {wxPrintData_new_1, "wxPrintData", "new", 1}, // 2372
  {NULL, "wxPrintData", "destroy", 1}, // 2373 obj destructor wxPrintData_destruct
  {wxPrintData_GetCollate, "wxPrintData", "getCollate", 1}, // 2374
  {wxPrintData_GetBin, "wxPrintData", "getBin", 1}, // 2375
  {wxPrintData_GetColour, "wxPrintData", "getColour", 1}, // 2376
  {wxPrintData_GetDuplex, "wxPrintData", "getDuplex", 1}, // 2377
  {wxPrintData_GetNoCopies, "wxPrintData", "getNoCopies", 1}, // 2378
  {wxPrintData_GetOrientation, "wxPrintData", "getOrientation", 1}, // 2379
  {wxPrintData_GetPaperId, "wxPrintData", "getPaperId", 1}, // 2380
  {wxPrintData_GetPrinterName, "wxPrintData", "getPrinterName", 1}, // 2381
  {wxPrintData_GetQuality, "wxPrintData", "getQuality", 1}, // 2382
  {wxPrintData_IsOk, "wxPrintData", "isOk", 1}, // 2383
  {wxPrintData_SetBin, "wxPrintData", "setBin", 2}, // 2384
  {wxPrintData_SetCollate, "wxPrintData", "setCollate", 2}, // 2385
  {wxPrintData_SetColour, "wxPrintData", "setColour", 2}, // 2386
  {wxPrintData_SetDuplex, "wxPrintData", "setDuplex", 2}, // 2387
  {wxPrintData_SetNoCopies, "wxPrintData", "setNoCopies", 2}, // 2388
  {wxPrintData_SetOrientation, "wxPrintData", "setOrientation", 2}, // 2389
  {wxPrintData_SetPaperId, "wxPrintData", "setPaperId", 2}, // 2390
  {wxPrintData_SetPrinterName, "wxPrintData", "setPrinterName", 2}, // 2391
  {wxPrintData_SetQuality, "wxPrintData", "setQuality", 2}, // 2392
  {wxPrintPreview_new_2, "wxPrintPreview", "new", 2}, // 2393
  {wxPrintPreview_new_3, "wxPrintPreview", "new", 3}, // 2394
  {NULL, "wxPrintPreview", "destroy", 1}, // 2395 obj destructor wxPrintPreview_destruct
  {wxPrintPreview_GetCanvas, "wxPrintPreview", "getCanvas", 1}, // 2396
  {wxPrintPreview_GetCurrentPage, "wxPrintPreview", "getCurrentPage", 1}, // 2397
  {wxPrintPreview_GetFrame, "wxPrintPreview", "getFrame", 1}, // 2398
  {wxPrintPreview_GetMaxPage, "wxPrintPreview", "getMaxPage", 1}, // 2399
  {wxPrintPreview_GetMinPage, "wxPrintPreview", "getMinPage", 1}, // 2400
  {wxPrintPreview_GetPrintout, "wxPrintPreview", "getPrintout", 1}, // 2401
  {wxPrintPreview_GetPrintoutForPrinting, "wxPrintPreview", "getPrintoutForPrinting", 1}, // 2402
  {wxPrintPreview_IsOk, "wxPrintPreview", "isOk", 1}, // 2403
  {wxPrintPreview_PaintPage, "wxPrintPreview", "paintPage", 3}, // 2404
  {wxPrintPreview_Print, "wxPrintPreview", "print", 2}, // 2405
  {wxPrintPreview_RenderPage, "wxPrintPreview", "renderPage", 2}, // 2406
  {wxPrintPreview_SetCanvas, "wxPrintPreview", "setCanvas", 2}, // 2407
  {wxPrintPreview_SetCurrentPage, "wxPrintPreview", "setCurrentPage", 2}, // 2408
  {wxPrintPreview_SetFrame, "wxPrintPreview", "setFrame", 2}, // 2409
  {wxPrintPreview_SetPrintout, "wxPrintPreview", "setPrintout", 2}, // 2410
  {wxPrintPreview_SetZoom, "wxPrintPreview", "setZoom", 2}, // 2411
  {wxPreviewFrame_new, "wxPreviewFrame", "new", 3}, // 2412
  {NULL, "wxPreviewFrame", "destroy", 1}, // 2413 obj destructor wxPreviewFrame_destruct
  {wxPreviewFrame_CreateControlBar, "wxPreviewFrame", "createControlBar", 1}, // 2414
  {wxPreviewFrame_CreateCanvas, "wxPreviewFrame", "createCanvas", 1}, // 2415
  {wxPreviewFrame_Initialize, "wxPreviewFrame", "initialize", 1}, // 2416
  {wxPreviewFrame_OnCloseWindow, "wxPreviewFrame", "onCloseWindow", 2}, // 2417
  {wxPreviewControlBar_new, "wxPreviewControlBar", "new", 4}, // 2418
  {NULL, "wxPreviewControlBar", "destroy", 1}, // 2419 obj destructor wxPreviewControlBar_destruct
  {wxPreviewControlBar_CreateButtons, "wxPreviewControlBar", "createButtons", 1}, // 2420
  {wxPreviewControlBar_GetPrintPreview, "wxPreviewControlBar", "getPrintPreview", 1}, // 2421
  {wxPreviewControlBar_GetZoomControl, "wxPreviewControlBar", "getZoomControl", 1}, // 2422
  {wxPreviewControlBar_SetZoomControl, "wxPreviewControlBar", "setZoomControl", 2}, // 2423
  {wxPrinter_new, "wxPrinter", "new", 1}, // 2424
  {wxPrinter_CreateAbortWindow, "wxPrinter", "createAbortWindow", 3}, // 2425
  {wxPrinter_GetAbort, "wxPrinter", "getAbort", 1}, // 2426
  {wxPrinter_GetLastError, "wxPrinter", "getLastError", 0}, // 2427
  {wxPrinter_GetPrintDialogData, "wxPrinter", "getPrintDialogData", 1}, // 2428
  {wxPrinter_Print, "wxPrinter", "print", 4}, // 2429
  {wxPrinter_PrintDialog, "wxPrinter", "printDialog", 2}, // 2430
  {wxPrinter_ReportError, "wxPrinter", "reportError", 4}, // 2431
  {wxPrinter_Setup, "wxPrinter", "setup", 2}, // 2432
  {NULL, "wxPrinter", "'Destroy'", 1}, // 2433 obj destructor wxPrinter_destroy
  {wxXmlResource_new_2, "wxXmlResource", "new", 2}, // 2434
  {wxXmlResource_new_1, "wxXmlResource", "new", 1}, // 2435
  {NULL, "wxXmlResource", "destroy", 1}, // 2436 obj destructor wxXmlResource_destruct
  {wxXmlResource_AttachUnknownControl, "wxXmlResource", "attachUnknownControl", 4}, // 2437
  {wxXmlResource_ClearHandlers, "wxXmlResource", "clearHandlers", 1}, // 2438
  {wxXmlResource_CompareVersion, "wxXmlResource", "compareVersion", 5}, // 2439
  {wxXmlResource_Get, "wxXmlResource", "get", 0}, // 2440
  {wxXmlResource_GetFlags, "wxXmlResource", "getFlags", 1}, // 2441
  {wxXmlResource_GetVersion, "wxXmlResource", "getVersion", 1}, // 2442
  {wxXmlResource_GetXRCID, "wxXmlResource", "getXRCID", 2}, // 2443
  {wxXmlResource_InitAllHandlers, "wxXmlResource", "initAllHandlers", 1}, // 2444
  {wxXmlResource_Load, "wxXmlResource", "load", 2}, // 2445
  {wxXmlResource_LoadBitmap, "wxXmlResource", "loadBitmap", 2}, // 2446
  {wxXmlResource_LoadDialog_2, "wxXmlResource", "loadDialog", 3}, // 2447
  {wxXmlResource_LoadDialog_3, "wxXmlResource", "loadDialog", 4}, // 2448
  {wxXmlResource_LoadFrame_2, "wxXmlResource", "loadFrame", 3}, // 2449
  {wxXmlResource_LoadFrame_3, "wxXmlResource", "loadFrame", 4}, // 2450
  {wxXmlResource_LoadIcon, "wxXmlResource", "loadIcon", 2}, // 2451
  {wxXmlResource_LoadMenu, "wxXmlResource", "loadMenu", 2}, // 2452
  {wxXmlResource_LoadMenuBar_2, "wxXmlResource", "loadMenuBar", 3}, // 2453
  {wxXmlResource_LoadMenuBar_1, "wxXmlResource", "loadMenuBar", 2}, // 2454
  {wxXmlResource_LoadPanel_2, "wxXmlResource", "loadPanel", 3}, // 2455
  {wxXmlResource_LoadPanel_3, "wxXmlResource", "loadPanel", 4}, // 2456
  {wxXmlResource_LoadToolBar, "wxXmlResource", "loadToolBar", 3}, // 2457
  {wxXmlResource_Set, "wxXmlResource", "set", 1}, // 2458
  {wxXmlResource_SetFlags, "wxXmlResource", "setFlags", 2}, // 2459
  {wxXmlResource_Unload, "wxXmlResource", "unload", 2}, // 2460
  {NULL, "wxXmlResource", "xrcctrl", 4}, // 2461 TaylorMade erl only wxXmlResource_xrcctrl
  {wxHtmlEasyPrinting_new, "wxHtmlEasyPrinting", "new", 1}, // 2462
  {wxHtmlEasyPrinting_GetPrintData, "wxHtmlEasyPrinting", "getPrintData", 1}, // 2463
  {wxHtmlEasyPrinting_GetPageSetupData, "wxHtmlEasyPrinting", "getPageSetupData", 1}, // 2464
  {wxHtmlEasyPrinting_PreviewFile, "wxHtmlEasyPrinting", "previewFile", 2}, // 2465
  {wxHtmlEasyPrinting_PreviewText, "wxHtmlEasyPrinting", "previewText", 3}, // 2466
  {wxHtmlEasyPrinting_PrintFile, "wxHtmlEasyPrinting", "printFile", 2}, // 2467
  {wxHtmlEasyPrinting_PrintText, "wxHtmlEasyPrinting", "printText", 3}, // 2468
  {wxHtmlEasyPrinting_PageSetup, "wxHtmlEasyPrinting", "pageSetup", 1}, // 2469
  {wxHtmlEasyPrinting_SetFonts, "wxHtmlEasyPrinting", "setFonts", 4}, // 2470
  {wxHtmlEasyPrinting_SetHeader, "wxHtmlEasyPrinting", "setHeader", 3}, // 2471
  {wxHtmlEasyPrinting_SetFooter, "wxHtmlEasyPrinting", "setFooter", 3}, // 2472
  {NULL, "wxHtmlEasyPrinting", "'Destroy'", 1}, // 2473 obj destructor wxHtmlEasyPrinting_destroy
  {wxGLCanvas_new, "wxGLCanvas", "new", 2}, // 2474
  {wxGLCanvas_SetCurrent, "wxGLCanvas", "setCurrent", 2}, // 2475
  {wxGLCanvas_SwapBuffers, "wxGLCanvas", "swapBuffers", 1}, // 2476
  {NULL, "wxGLCanvas", "'Destroy'", 1}, // 2477 obj destructor wxGLCanvas_destroy
  {wxGLContext_new, "wxGLContext", "new", 2}, // 2478
  {wxGLContext_SetCurrent, "wxGLContext", "setCurrent", 2}, // 2479
  {NULL, "wxGLContext", "'Destroy'", 1}, // 2480 obj destructor wxGLContext_destroy
  {wxAuiManager_new, "wxAuiManager", "new", 1}, // 2481
  {NULL, "wxAuiManager", "destroy", 1}, // 2482 obj destructor wxAuiManager_destruct
  {wxAuiManager_AddPane_2_1, "wxAuiManager", "addPane", 3}, // 2483
  {wxAuiManager_AddPane_2_0, "wxAuiManager", "addPane", 3}, // 2484
  {wxAuiManager_AddPane_3, "wxAuiManager", "addPane", 4}, // 2485
  {wxAuiManager_DetachPane, "wxAuiManager", "detachPane", 2}, // 2486
  {wxAuiManager_GetAllPanes, "wxAuiManager", "getAllPanes", 1}, // 2487
  {wxAuiManager_GetArtProvider, "wxAuiManager", "getArtProvider", 1}, // 2488
  {wxAuiManager_GetDockSizeConstraint, "wxAuiManager", "getDockSizeConstraint", 1}, // 2489
  {wxAuiManager_GetFlags, "wxAuiManager", "getFlags", 1}, // 2490
  {wxAuiManager_GetManagedWindow, "wxAuiManager", "getManagedWindow", 1}, // 2491
  {wxAuiManager_GetManager, "wxAuiManager", "getManager", 1}, // 2492
  {wxAuiManager_GetPane_1_1, "wxAuiManager", "getPane", 2}, // 2493
  {wxAuiManager_GetPane_1_0, "wxAuiManager", "getPane", 2}, // 2494
  {wxAuiManager_HideHint, "wxAuiManager", "hideHint", 1}, // 2495
  {wxAuiManager_InsertPane, "wxAuiManager", "insertPane", 4}, // 2496
  {wxAuiManager_LoadPaneInfo, "wxAuiManager", "loadPaneInfo", 3}, // 2497
  {wxAuiManager_LoadPerspective, "wxAuiManager", "loadPerspective", 3}, // 2498
  {wxAuiManager_SavePaneInfo, "wxAuiManager", "savePaneInfo", 2}, // 2499
  {wxAuiManager_SavePerspective, "wxAuiManager", "savePerspective", 1}, // 2500
  {wxAuiManager_SetArtProvider, "wxAuiManager", "setArtProvider", 2}, // 2501
  {wxAuiManager_SetDockSizeConstraint, "wxAuiManager", "setDockSizeConstraint", 3}, // 2502
  {wxAuiManager_SetFlags, "wxAuiManager", "setFlags", 2}, // 2503
  {wxAuiManager_SetManagedWindow, "wxAuiManager", "setManagedWindow", 2}, // 2504
  {wxAuiManager_ShowHint, "wxAuiManager", "showHint", 2}, // 2505
  {wxAuiManager_UnInit, "wxAuiManager", "unInit", 1}, // 2506
  {wxAuiManager_Update, "wxAuiManager", "update", 1}, // 2507
  {wxAuiPaneInfo_new_0, "wxAuiPaneInfo", "new", 0}, // 2508
  {wxAuiPaneInfo_new_1, "wxAuiPaneInfo", "new", 1}, // 2509
  {wxAuiPaneInfo_BestSize_1, "wxAuiPaneInfo", "bestSize", 2}, // 2510
  {wxAuiPaneInfo_BestSize_2, "wxAuiPaneInfo", "bestSize", 3}, // 2511
  {wxAuiPaneInfo_Bottom, "wxAuiPaneInfo", "bottom", 1}, // 2512
  {wxAuiPaneInfo_BottomDockable, "wxAuiPaneInfo", "bottomDockable", 2}, // 2513
  {wxAuiPaneInfo_Caption, "wxAuiPaneInfo", "caption", 2}, // 2514
  {wxAuiPaneInfo_CaptionVisible, "wxAuiPaneInfo", "captionVisible", 2}, // 2515
  {wxAuiPaneInfo_Centre, "wxAuiPaneInfo", "centre", 1}, // 2516
  {wxAuiPaneInfo_CentrePane, "wxAuiPaneInfo", "centrePane", 1}, // 2517
  {wxAuiPaneInfo_CloseButton, "wxAuiPaneInfo", "closeButton", 2}, // 2518
  {wxAuiPaneInfo_DefaultPane, "wxAuiPaneInfo", "defaultPane", 1}, // 2519
  {wxAuiPaneInfo_DestroyOnClose, "wxAuiPaneInfo", "destroyOnClose", 2}, // 2520
  {wxAuiPaneInfo_Direction, "wxAuiPaneInfo", "direction", 2}, // 2521
  {wxAuiPaneInfo_Dock, "wxAuiPaneInfo", "dock", 1}, // 2522
  {wxAuiPaneInfo_Dockable, "wxAuiPaneInfo", "dockable", 2}, // 2523
  {wxAuiPaneInfo_Fixed, "wxAuiPaneInfo", "fixed", 1}, // 2524
  {wxAuiPaneInfo_Float, "wxAuiPaneInfo", "float", 1}, // 2525
  {wxAuiPaneInfo_Floatable, "wxAuiPaneInfo", "floatable", 2}, // 2526
  {wxAuiPaneInfo_FloatingPosition_1, "wxAuiPaneInfo", "floatingPosition", 2}, // 2527
  {wxAuiPaneInfo_FloatingPosition_2, "wxAuiPaneInfo", "floatingPosition", 3}, // 2528
  {wxAuiPaneInfo_FloatingSize_1, "wxAuiPaneInfo", "floatingSize", 2}, // 2529
  {wxAuiPaneInfo_FloatingSize_2, "wxAuiPaneInfo", "floatingSize", 3}, // 2530
  {wxAuiPaneInfo_Gripper, "wxAuiPaneInfo", "gripper", 2}, // 2531
  {wxAuiPaneInfo_GripperTop, "wxAuiPaneInfo", "gripperTop", 2}, // 2532
  {wxAuiPaneInfo_HasBorder, "wxAuiPaneInfo", "hasBorder", 1}, // 2533
  {wxAuiPaneInfo_HasCaption, "wxAuiPaneInfo", "hasCaption", 1}, // 2534
  {wxAuiPaneInfo_HasCloseButton, "wxAuiPaneInfo", "hasCloseButton", 1}, // 2535
  {wxAuiPaneInfo_HasFlag, "wxAuiPaneInfo", "hasFlag", 2}, // 2536
  {wxAuiPaneInfo_HasGripper, "wxAuiPaneInfo", "hasGripper", 1}, // 2537
  {wxAuiPaneInfo_HasGripperTop, "wxAuiPaneInfo", "hasGripperTop", 1}, // 2538
  {wxAuiPaneInfo_HasMaximizeButton, "wxAuiPaneInfo", "hasMaximizeButton", 1}, // 2539
  {wxAuiPaneInfo_HasMinimizeButton, "wxAuiPaneInfo", "hasMinimizeButton", 1}, // 2540
  {wxAuiPaneInfo_HasPinButton, "wxAuiPaneInfo", "hasPinButton", 1}, // 2541
  {wxAuiPaneInfo_Hide, "wxAuiPaneInfo", "hide", 1}, // 2542
  {wxAuiPaneInfo_IsBottomDockable, "wxAuiPaneInfo", "isBottomDockable", 1}, // 2543
  {wxAuiPaneInfo_IsDocked, "wxAuiPaneInfo", "isDocked", 1}, // 2544
  {wxAuiPaneInfo_IsFixed, "wxAuiPaneInfo", "isFixed", 1}, // 2545
  {wxAuiPaneInfo_IsFloatable, "wxAuiPaneInfo", "isFloatable", 1}, // 2546
  {wxAuiPaneInfo_IsFloating, "wxAuiPaneInfo", "isFloating", 1}, // 2547
  {wxAuiPaneInfo_IsLeftDockable, "wxAuiPaneInfo", "isLeftDockable", 1}, // 2548
  {wxAuiPaneInfo_IsMovable, "wxAuiPaneInfo", "isMovable", 1}, // 2549
  {wxAuiPaneInfo_IsOk, "wxAuiPaneInfo", "isOk", 1}, // 2550
  {wxAuiPaneInfo_IsResizable, "wxAuiPaneInfo", "isResizable", 1}, // 2551
  {wxAuiPaneInfo_IsRightDockable, "wxAuiPaneInfo", "isRightDockable", 1}, // 2552
  {wxAuiPaneInfo_IsShown, "wxAuiPaneInfo", "isShown", 1}, // 2553
  {wxAuiPaneInfo_IsToolbar, "wxAuiPaneInfo", "isToolbar", 1}, // 2554
  {wxAuiPaneInfo_IsTopDockable, "wxAuiPaneInfo", "isTopDockable", 1}, // 2555
  {wxAuiPaneInfo_Layer, "wxAuiPaneInfo", "layer", 2}, // 2556
  {wxAuiPaneInfo_Left, "wxAuiPaneInfo", "left", 1}, // 2557
  {wxAuiPaneInfo_LeftDockable, "wxAuiPaneInfo", "leftDockable", 2}, // 2558
  {wxAuiPaneInfo_MaxSize_1, "wxAuiPaneInfo", "maxSize", 2}, // 2559
  {wxAuiPaneInfo_MaxSize_2, "wxAuiPaneInfo", "maxSize", 3}, // 2560
  {wxAuiPaneInfo_MaximizeButton, "wxAuiPaneInfo", "maximizeButton", 2}, // 2561
  {wxAuiPaneInfo_MinSize_1, "wxAuiPaneInfo", "minSize", 2}, // 2562
  {wxAuiPaneInfo_MinSize_2, "wxAuiPaneInfo", "minSize", 3}, // 2563
  {wxAuiPaneInfo_MinimizeButton, "wxAuiPaneInfo", "minimizeButton", 2}, // 2564
  {wxAuiPaneInfo_Movable, "wxAuiPaneInfo", "movable", 2}, // 2565
  {wxAuiPaneInfo_Name, "wxAuiPaneInfo", "name", 2}, // 2566
  {wxAuiPaneInfo_PaneBorder, "wxAuiPaneInfo", "paneBorder", 2}, // 2567
  {wxAuiPaneInfo_PinButton, "wxAuiPaneInfo", "pinButton", 2}, // 2568
  {wxAuiPaneInfo_Position, "wxAuiPaneInfo", "position", 2}, // 2569
  {wxAuiPaneInfo_Resizable, "wxAuiPaneInfo", "resizable", 2}, // 2570
  {wxAuiPaneInfo_Right, "wxAuiPaneInfo", "right", 1}, // 2571
  {wxAuiPaneInfo_RightDockable, "wxAuiPaneInfo", "rightDockable", 2}, // 2572
  {wxAuiPaneInfo_Row, "wxAuiPaneInfo", "row", 2}, // 2573
  {wxAuiPaneInfo_SafeSet, "wxAuiPaneInfo", "safeSet", 2}, // 2574
  {wxAuiPaneInfo_SetFlag, "wxAuiPaneInfo", "setFlag", 3}, // 2575
  {wxAuiPaneInfo_Show, "wxAuiPaneInfo", "show", 2}, // 2576
  {wxAuiPaneInfo_ToolbarPane, "wxAuiPaneInfo", "toolbarPane", 1}, // 2577
  {wxAuiPaneInfo_Top, "wxAuiPaneInfo", "top", 1}, // 2578
  {wxAuiPaneInfo_TopDockable, "wxAuiPaneInfo", "topDockable", 2}, // 2579
  {wxAuiPaneInfo_Window, "wxAuiPaneInfo", "window", 2}, // 2580
  {wxAuiPaneInfo_GetWindow, "wxAuiPaneInfo", "getWindow", 1}, // 2581
  {wxAuiPaneInfo_GetFrame, "wxAuiPaneInfo", "getFrame", 1}, // 2582
  {wxAuiPaneInfo_GetDirection, "wxAuiPaneInfo", "getDirection", 1}, // 2583
  {wxAuiPaneInfo_GetLayer, "wxAuiPaneInfo", "getLayer", 1}, // 2584
  {wxAuiPaneInfo_GetRow, "wxAuiPaneInfo", "getRow", 1}, // 2585
  {wxAuiPaneInfo_GetPosition, "wxAuiPaneInfo", "getPosition", 1}, // 2586
  {wxAuiPaneInfo_GetFloatingPosition, "wxAuiPaneInfo", "getFloatingPosition", 1}, // 2587
  {wxAuiPaneInfo_GetFloatingSize, "wxAuiPaneInfo", "getFloatingSize", 1}, // 2588
  {wxAuiPaneInfo_destroy, "wxAuiPaneInfo", "'Destroy'", 1}, // 2589
  {wxAuiNotebook_new_0, "wxAuiNotebook", "new", 0}, // 2590
  {wxAuiNotebook_new_2, "wxAuiNotebook", "new", 2}, // 2591
  {wxAuiNotebook_AddPage_3, "wxAuiNotebook", "addPage", 4}, // 2592
  {wxAuiNotebook_AddPage_4, "wxAuiNotebook", "addPage", 5}, // 2593
  {wxAuiNotebook_Create_2, "wxAuiNotebook", "create", 3}, // 2594
  {wxAuiNotebook_Create_3, "wxAuiNotebook", "create", 4}, // 2595
  {wxAuiNotebook_DeletePage, "wxAuiNotebook", "deletePage", 2}, // 2596
  {wxAuiNotebook_GetArtProvider, "wxAuiNotebook", "getArtProvider", 1}, // 2597
  {wxAuiNotebook_GetPage, "wxAuiNotebook", "getPage", 2}, // 2598
  {wxAuiNotebook_GetPageBitmap, "wxAuiNotebook", "getPageBitmap", 2}, // 2599
  {wxAuiNotebook_GetPageCount, "wxAuiNotebook", "getPageCount", 1}, // 2600
  {wxAuiNotebook_GetPageIndex, "wxAuiNotebook", "getPageIndex", 2}, // 2601
  {wxAuiNotebook_GetPageText, "wxAuiNotebook", "getPageText", 2}, // 2602
  {wxAuiNotebook_GetSelection, "wxAuiNotebook", "getSelection", 1}, // 2603
  {wxAuiNotebook_InsertPage_4, "wxAuiNotebook", "insertPage", 5}, // 2604
  {wxAuiNotebook_InsertPage_5, "wxAuiNotebook", "insertPage", 6}, // 2605
  {wxAuiNotebook_RemovePage, "wxAuiNotebook", "removePage", 2}, // 2606
  {wxAuiNotebook_SetArtProvider, "wxAuiNotebook", "setArtProvider", 2}, // 2607
  {wxAuiNotebook_SetFont, "wxAuiNotebook", "setFont", 2}, // 2608
  {wxAuiNotebook_SetPageBitmap, "wxAuiNotebook", "setPageBitmap", 3}, // 2609
  {wxAuiNotebook_SetPageText, "wxAuiNotebook", "setPageText", 3}, // 2610
  {wxAuiNotebook_SetSelection, "wxAuiNotebook", "setSelection", 2}, // 2611
  {wxAuiNotebook_SetTabCtrlHeight, "wxAuiNotebook", "setTabCtrlHeight", 2}, // 2612
  {wxAuiNotebook_SetUniformBitmapSize, "wxAuiNotebook", "setUniformBitmapSize", 2}, // 2613
  {NULL, "wxAuiNotebook", "'Destroy'", 1}, // 2614 obj destructor wxAuiNotebook_destroy
  {wxAuiTabArt_SetFlags, "wxAuiTabArt", "setFlags", 2}, // 2615
  {wxAuiTabArt_SetMeasuringFont, "wxAuiTabArt", "setMeasuringFont", 2}, // 2616
  {wxAuiTabArt_SetNormalFont, "wxAuiTabArt", "setNormalFont", 2}, // 2617
  {wxAuiTabArt_SetSelectedFont, "wxAuiTabArt", "setSelectedFont", 2}, // 2618
  {wxAuiTabArt_SetColour, "wxAuiTabArt", "setColour", 2}, // 2619
  {wxAuiTabArt_SetActiveColour, "wxAuiTabArt", "setActiveColour", 2}, // 2620
  {wxAuiDockArt_GetColour, "wxAuiDockArt", "getColour", 2}, // 2621
  {wxAuiDockArt_GetFont, "wxAuiDockArt", "getFont", 2}, // 2622
  {wxAuiDockArt_GetMetric, "wxAuiDockArt", "getMetric", 2}, // 2623
  {wxAuiDockArt_SetColour, "wxAuiDockArt", "setColour", 3}, // 2624
  {wxAuiDockArt_SetFont, "wxAuiDockArt", "setFont", 3}, // 2625
  {wxAuiDockArt_SetMetric, "wxAuiDockArt", "setMetric", 3}, // 2626
  {wxAuiSimpleTabArt_new, "wxAuiSimpleTabArt", "new", 0}, // 2627
  {wxAuiSimpleTabArt_destroy, "wxAuiSimpleTabArt", "'Destroy'", 1}, // 2628
  {wxMDIParentFrame_new_0, "wxMDIParentFrame", "new", 0}, // 2629
  {wxMDIParentFrame_new_4, "wxMDIParentFrame", "new", 4}, // 2630
  {NULL, "wxMDIParentFrame", "destroy", 1}, // 2631 obj destructor wxMDIParentFrame_destruct
  {wxMDIParentFrame_ActivateNext, "wxMDIParentFrame", "activateNext", 1}, // 2632
  {wxMDIParentFrame_ActivatePrevious, "wxMDIParentFrame", "activatePrevious", 1}, // 2633
  {wxMDIParentFrame_ArrangeIcons, "wxMDIParentFrame", "arrangeIcons", 1}, // 2634
  {wxMDIParentFrame_Cascade, "wxMDIParentFrame", "cascade", 1}, // 2635
  {wxMDIParentFrame_Create, "wxMDIParentFrame", "create", 5}, // 2636
  {wxMDIParentFrame_GetActiveChild, "wxMDIParentFrame", "getActiveChild", 1}, // 2637
  {wxMDIParentFrame_GetClientWindow, "wxMDIParentFrame", "getClientWindow", 1}, // 2638
  {wxMDIParentFrame_Tile, "wxMDIParentFrame", "tile", 2}, // 2639
  {wxMDIChildFrame_new_0, "wxMDIChildFrame", "new", 0}, // 2640
  {wxMDIChildFrame_new_4, "wxMDIChildFrame", "new", 4}, // 2641
  {NULL, "wxMDIChildFrame", "destroy", 1}, // 2642 obj destructor wxMDIChildFrame_destruct
  {wxMDIChildFrame_Activate, "wxMDIChildFrame", "activate", 1}, // 2643
  {wxMDIChildFrame_Create, "wxMDIChildFrame", "create", 5}, // 2644
  {wxMDIChildFrame_Maximize, "wxMDIChildFrame", "maximize", 2}, // 2645
  {wxMDIChildFrame_Restore, "wxMDIChildFrame", "restore", 1}, // 2646
  {wxMDIClientWindow_new, "wxMDIClientWindow", "new", 0}, // 2647
  {wxMDIClientWindow_CreateClient, "wxMDIClientWindow", "createClient", 3}, // 2648
  {NULL, "wxMDIClientWindow", "'Destroy'", 1}, // 2649 obj destructor wxMDIClientWindow_destroy
  {wxLayoutAlgorithm_new, "wxLayoutAlgorithm", "new", 0}, // 2650
  {NULL, "wxLayoutAlgorithm", "destroy", 1}, // 2651 obj destructor wxLayoutAlgorithm_destruct
  {wxLayoutAlgorithm_LayoutFrame, "wxLayoutAlgorithm", "layoutFrame", 3}, // 2652
  {wxLayoutAlgorithm_LayoutMDIFrame, "wxLayoutAlgorithm", "layoutMDIFrame", 3}, // 2653
  {wxLayoutAlgorithm_LayoutWindow, "wxLayoutAlgorithm", "layoutWindow", 3}, // 2654
  {wxEvent_GetId, "wxEvent", "getId", 1}, // 2655
  {wxEvent_GetSkipped, "wxEvent", "getSkipped", 1}, // 2656
  {wxEvent_GetTimestamp, "wxEvent", "getTimestamp", 1}, // 2657
  {wxEvent_IsCommandEvent, "wxEvent", "isCommandEvent", 1}, // 2658
  {wxEvent_ResumePropagation, "wxEvent", "resumePropagation", 2}, // 2659
  {wxEvent_ShouldPropagate, "wxEvent", "shouldPropagate", 1}, // 2660
  {wxEvent_Skip, "wxEvent", "skip", 2}, // 2661
  {wxEvent_StopPropagation, "wxEvent", "stopPropagation", 1}, // 2662
  {wxCommandEvent_getClientData, "wxCommandEvent", "getClientData", 1}, // 2663
  {wxCommandEvent_GetExtraLong, "wxCommandEvent", "getExtraLong", 1}, // 2664
  {wxCommandEvent_GetInt, "wxCommandEvent", "getInt", 1}, // 2665
  {wxCommandEvent_GetSelection, "wxCommandEvent", "getSelection", 1}, // 2666
  {wxCommandEvent_GetString, "wxCommandEvent", "getString", 1}, // 2667
  {wxCommandEvent_IsChecked, "wxCommandEvent", "isChecked", 1}, // 2668
  {wxCommandEvent_IsSelection, "wxCommandEvent", "isSelection", 1}, // 2669
  {wxCommandEvent_SetInt, "wxCommandEvent", "setInt", 2}, // 2670
  {wxCommandEvent_SetString, "wxCommandEvent", "setString", 2}, // 2671
  {wxScrollEvent_GetOrientation, "wxScrollEvent", "getOrientation", 1}, // 2672
  {wxScrollEvent_GetPosition, "wxScrollEvent", "getPosition", 1}, // 2673
  {wxScrollWinEvent_GetOrientation, "wxScrollWinEvent", "getOrientation", 1}, // 2674
  {wxScrollWinEvent_GetPosition, "wxScrollWinEvent", "getPosition", 1}, // 2675
  {wxMouseEvent_AltDown, "wxMouseEvent", "altDown", 1}, // 2676
  {wxMouseEvent_Button, "wxMouseEvent", "button", 2}, // 2677
  {wxMouseEvent_ButtonDClick, "wxMouseEvent", "buttonDClick", 2}, // 2678
  {wxMouseEvent_ButtonDown, "wxMouseEvent", "buttonDown", 2}, // 2679
  {wxMouseEvent_ButtonUp, "wxMouseEvent", "buttonUp", 2}, // 2680
  {wxMouseEvent_CmdDown, "wxMouseEvent", "cmdDown", 1}, // 2681
  {wxMouseEvent_ControlDown, "wxMouseEvent", "controlDown", 1}, // 2682
  {wxMouseEvent_Dragging, "wxMouseEvent", "dragging", 1}, // 2683
  {wxMouseEvent_Entering, "wxMouseEvent", "entering", 1}, // 2684
  {wxMouseEvent_GetButton, "wxMouseEvent", "getButton", 1}, // 2685
  {wxMouseEvent_GetPosition, "wxMouseEvent", "getPosition", 1}, // 2686
  {NULL, "", "", 0}, // 2687
  {wxMouseEvent_GetLogicalPosition, "wxMouseEvent", "getLogicalPosition", 2}, // 2688
  {wxMouseEvent_GetLinesPerAction, "wxMouseEvent", "getLinesPerAction", 1}, // 2689
  {wxMouseEvent_GetWheelRotation, "wxMouseEvent", "getWheelRotation", 1}, // 2690
  {wxMouseEvent_GetWheelDelta, "wxMouseEvent", "getWheelDelta", 1}, // 2691
  {wxMouseEvent_GetX, "wxMouseEvent", "getX", 1}, // 2692
  {wxMouseEvent_GetY, "wxMouseEvent", "getY", 1}, // 2693
  {wxMouseEvent_IsButton, "wxMouseEvent", "isButton", 1}, // 2694
  {wxMouseEvent_IsPageScroll, "wxMouseEvent", "isPageScroll", 1}, // 2695
  {wxMouseEvent_Leaving, "wxMouseEvent", "leaving", 1}, // 2696
  {wxMouseEvent_LeftDClick, "wxMouseEvent", "leftDClick", 1}, // 2697
  {wxMouseEvent_LeftDown, "wxMouseEvent", "leftDown", 1}, // 2698
  {wxMouseEvent_LeftIsDown, "wxMouseEvent", "leftIsDown", 1}, // 2699
  {wxMouseEvent_LeftUp, "wxMouseEvent", "leftUp", 1}, // 2700
  {wxMouseEvent_MetaDown, "wxMouseEvent", "metaDown", 1}, // 2701
  {wxMouseEvent_MiddleDClick, "wxMouseEvent", "middleDClick", 1}, // 2702
  {wxMouseEvent_MiddleDown, "wxMouseEvent", "middleDown", 1}, // 2703
  {wxMouseEvent_MiddleIsDown, "wxMouseEvent", "middleIsDown", 1}, // 2704
  {wxMouseEvent_MiddleUp, "wxMouseEvent", "middleUp", 1}, // 2705
  {wxMouseEvent_Moving, "wxMouseEvent", "moving", 1}, // 2706
  {wxMouseEvent_RightDClick, "wxMouseEvent", "rightDClick", 1}, // 2707
  {wxMouseEvent_RightDown, "wxMouseEvent", "rightDown", 1}, // 2708
  {wxMouseEvent_RightIsDown, "wxMouseEvent", "rightIsDown", 1}, // 2709
  {wxMouseEvent_RightUp, "wxMouseEvent", "rightUp", 1}, // 2710
  {wxMouseEvent_ShiftDown, "wxMouseEvent", "shiftDown", 1}, // 2711
  {wxMouseEvent_GetWheelAxis, "wxMouseEvent", "getWheelAxis", 1}, // 2712
  {wxSetCursorEvent_GetCursor, "wxSetCursorEvent", "getCursor", 1}, // 2713
  {wxSetCursorEvent_GetX, "wxSetCursorEvent", "getX", 1}, // 2714
  {wxSetCursorEvent_GetY, "wxSetCursorEvent", "getY", 1}, // 2715
  {wxSetCursorEvent_HasCursor, "wxSetCursorEvent", "hasCursor", 1}, // 2716
  {wxSetCursorEvent_SetCursor, "wxSetCursorEvent", "setCursor", 2}, // 2717
  {wxKeyEvent_AltDown, "wxKeyEvent", "altDown", 1}, // 2718
  {wxKeyEvent_CmdDown, "wxKeyEvent", "cmdDown", 1}, // 2719
  {wxKeyEvent_ControlDown, "wxKeyEvent", "controlDown", 1}, // 2720
  {wxKeyEvent_GetKeyCode, "wxKeyEvent", "getKeyCode", 1}, // 2721
  {wxKeyEvent_GetModifiers, "wxKeyEvent", "getModifiers", 1}, // 2722
  {wxKeyEvent_GetPosition, "wxKeyEvent", "getPosition", 1}, // 2723
  {NULL, "", "", 0}, // 2724
  {wxKeyEvent_GetRawKeyCode, "wxKeyEvent", "getRawKeyCode", 1}, // 2725
  {wxKeyEvent_GetRawKeyFlags, "wxKeyEvent", "getRawKeyFlags", 1}, // 2726
  {wxKeyEvent_GetUnicodeKey, "wxKeyEvent", "getUnicodeKey", 1}, // 2727
  {wxKeyEvent_GetX, "wxKeyEvent", "getX", 1}, // 2728
  {wxKeyEvent_GetY, "wxKeyEvent", "getY", 1}, // 2729
  {wxKeyEvent_HasModifiers, "wxKeyEvent", "hasModifiers", 1}, // 2730
  {wxKeyEvent_MetaDown, "wxKeyEvent", "metaDown", 1}, // 2731
  {wxKeyEvent_ShiftDown, "wxKeyEvent", "shiftDown", 1}, // 2732
  {wxSizeEvent_GetSize, "wxSizeEvent", "getSize", 1}, // 2733
  {wxSizeEvent_GetRect, "wxSizeEvent", "getRect", 1}, // 2734
  {wxMoveEvent_GetPosition, "wxMoveEvent", "getPosition", 1}, // 2735
  {wxMoveEvent_GetRect, "wxMoveEvent", "getRect", 1}, // 2736
  {wxEraseEvent_GetDC, "wxEraseEvent", "getDC", 1}, // 2737
  {wxFocusEvent_GetWindow, "wxFocusEvent", "getWindow", 1}, // 2738
  {wxChildFocusEvent_GetWindow, "wxChildFocusEvent", "getWindow", 1}, // 2739
  {wxMenuEvent_GetMenu, "wxMenuEvent", "getMenu", 1}, // 2740
  {wxMenuEvent_GetMenuId, "wxMenuEvent", "getMenuId", 1}, // 2741
  {wxMenuEvent_IsPopup, "wxMenuEvent", "isPopup", 1}, // 2742
  {wxCloseEvent_CanVeto, "wxCloseEvent", "canVeto", 1}, // 2743
  {wxCloseEvent_GetLoggingOff, "wxCloseEvent", "getLoggingOff", 1}, // 2744
  {wxCloseEvent_SetCanVeto, "wxCloseEvent", "setCanVeto", 2}, // 2745
  {wxCloseEvent_SetLoggingOff, "wxCloseEvent", "setLoggingOff", 2}, // 2746
  {wxCloseEvent_Veto, "wxCloseEvent", "veto", 2}, // 2747
  {wxShowEvent_SetShow, "wxShowEvent", "setShow", 2}, // 2748
  {wxShowEvent_IsShown, "wxShowEvent", "isShown", 1}, // 2749
  {wxIconizeEvent_IsIconized, "wxIconizeEvent", "isIconized", 1}, // 2750
  {wxJoystickEvent_ButtonDown, "wxJoystickEvent", "buttonDown", 2}, // 2751
  {wxJoystickEvent_ButtonIsDown, "wxJoystickEvent", "buttonIsDown", 2}, // 2752
  {wxJoystickEvent_ButtonUp, "wxJoystickEvent", "buttonUp", 2}, // 2753
  {wxJoystickEvent_GetButtonChange, "wxJoystickEvent", "getButtonChange", 1}, // 2754
  {wxJoystickEvent_GetButtonState, "wxJoystickEvent", "getButtonState", 1}, // 2755
  {wxJoystickEvent_GetJoystick, "wxJoystickEvent", "getJoystick", 1}, // 2756
  {wxJoystickEvent_GetPosition, "wxJoystickEvent", "getPosition", 1}, // 2757
  {wxJoystickEvent_GetZPosition, "wxJoystickEvent", "getZPosition", 1}, // 2758
  {wxJoystickEvent_IsButton, "wxJoystickEvent", "isButton", 1}, // 2759
  {wxJoystickEvent_IsMove, "wxJoystickEvent", "isMove", 1}, // 2760
  {wxJoystickEvent_IsZMove, "wxJoystickEvent", "isZMove", 1}, // 2761
  {wxUpdateUIEvent_CanUpdate, "wxUpdateUIEvent", "canUpdate", 1}, // 2762
  {wxUpdateUIEvent_Check, "wxUpdateUIEvent", "check", 2}, // 2763
  {wxUpdateUIEvent_Enable, "wxUpdateUIEvent", "enable", 2}, // 2764
  {wxUpdateUIEvent_Show, "wxUpdateUIEvent", "show", 2}, // 2765
  {wxUpdateUIEvent_GetChecked, "wxUpdateUIEvent", "getChecked", 1}, // 2766
  {wxUpdateUIEvent_GetEnabled, "wxUpdateUIEvent", "getEnabled", 1}, // 2767
  {wxUpdateUIEvent_GetShown, "wxUpdateUIEvent", "getShown", 1}, // 2768
  {wxUpdateUIEvent_GetSetChecked, "wxUpdateUIEvent", "getSetChecked", 1}, // 2769
  {wxUpdateUIEvent_GetSetEnabled, "wxUpdateUIEvent", "getSetEnabled", 1}, // 2770
  {wxUpdateUIEvent_GetSetShown, "wxUpdateUIEvent", "getSetShown", 1}, // 2771
  {wxUpdateUIEvent_GetSetText, "wxUpdateUIEvent", "getSetText", 1}, // 2772
  {wxUpdateUIEvent_GetText, "wxUpdateUIEvent", "getText", 1}, // 2773
  {wxUpdateUIEvent_GetMode, "wxUpdateUIEvent", "getMode", 0}, // 2774
  {wxUpdateUIEvent_GetUpdateInterval, "wxUpdateUIEvent", "getUpdateInterval", 0}, // 2775
  {wxUpdateUIEvent_ResetUpdateTime, "wxUpdateUIEvent", "resetUpdateTime", 0}, // 2776
  {wxUpdateUIEvent_SetMode, "wxUpdateUIEvent", "setMode", 1}, // 2777
  {wxUpdateUIEvent_SetText, "wxUpdateUIEvent", "setText", 2}, // 2778
  {wxUpdateUIEvent_SetUpdateInterval, "wxUpdateUIEvent", "setUpdateInterval", 1}, // 2779
  {wxMouseCaptureChangedEvent_GetCapturedWindow, "wxMouseCaptureChangedEvent", "getCapturedWindow", 1}, // 2780
  {wxPaletteChangedEvent_SetChangedWindow, "wxPaletteChangedEvent", "setChangedWindow", 2}, // 2781
  {wxPaletteChangedEvent_GetChangedWindow, "wxPaletteChangedEvent", "getChangedWindow", 1}, // 2782
  {wxQueryNewPaletteEvent_SetPaletteRealized, "wxQueryNewPaletteEvent", "setPaletteRealized", 2}, // 2783
  {wxQueryNewPaletteEvent_GetPaletteRealized, "wxQueryNewPaletteEvent", "getPaletteRealized", 1}, // 2784
  {wxNavigationKeyEvent_GetDirection, "wxNavigationKeyEvent", "getDirection", 1}, // 2785
  {wxNavigationKeyEvent_SetDirection, "wxNavigationKeyEvent", "setDirection", 2}, // 2786
  {wxNavigationKeyEvent_IsWindowChange, "wxNavigationKeyEvent", "isWindowChange", 1}, // 2787
  {wxNavigationKeyEvent_SetWindowChange, "wxNavigationKeyEvent", "setWindowChange", 2}, // 2788
  {wxNavigationKeyEvent_IsFromTab, "wxNavigationKeyEvent", "isFromTab", 1}, // 2789
  {wxNavigationKeyEvent_SetFromTab, "wxNavigationKeyEvent", "setFromTab", 2}, // 2790
  {wxNavigationKeyEvent_GetCurrentFocus, "wxNavigationKeyEvent", "getCurrentFocus", 1}, // 2791
  {wxNavigationKeyEvent_SetCurrentFocus, "wxNavigationKeyEvent", "setCurrentFocus", 2}, // 2792
  {wxHelpEvent_GetOrigin, "wxHelpEvent", "getOrigin", 1}, // 2793
  {wxHelpEvent_GetPosition, "wxHelpEvent", "getPosition", 1}, // 2794
  {wxHelpEvent_SetOrigin, "wxHelpEvent", "setOrigin", 2}, // 2795
  {wxHelpEvent_SetPosition, "wxHelpEvent", "setPosition", 2}, // 2796
  {wxContextMenuEvent_GetPosition, "wxContextMenuEvent", "getPosition", 1}, // 2797
  {wxContextMenuEvent_SetPosition, "wxContextMenuEvent", "setPosition", 2}, // 2798
  {wxIdleEvent_GetMode, "wxIdleEvent", "getMode", 0}, // 2799
  {wxIdleEvent_RequestMore, "wxIdleEvent", "requestMore", 2}, // 2800
  {wxIdleEvent_MoreRequested, "wxIdleEvent", "moreRequested", 1}, // 2801
  {wxIdleEvent_SetMode, "wxIdleEvent", "setMode", 1}, // 2802
  {wxGridEvent_AltDown, "wxGridEvent", "altDown", 1}, // 2803
  {wxGridEvent_ControlDown, "wxGridEvent", "controlDown", 1}, // 2804
  {wxGridEvent_GetCol, "wxGridEvent", "getCol", 1}, // 2805
  {wxGridEvent_GetPosition, "wxGridEvent", "getPosition", 1}, // 2806
  {wxGridEvent_GetRow, "wxGridEvent", "getRow", 1}, // 2807
  {wxGridEvent_MetaDown, "wxGridEvent", "metaDown", 1}, // 2808
  {wxGridEvent_Selecting, "wxGridEvent", "selecting", 1}, // 2809
  {wxGridEvent_ShiftDown, "wxGridEvent", "shiftDown", 1}, // 2810
  {wxNotifyEvent_Allow, "wxNotifyEvent", "allow", 1}, // 2811
  {wxNotifyEvent_IsAllowed, "wxNotifyEvent", "isAllowed", 1}, // 2812
  {wxNotifyEvent_Veto, "wxNotifyEvent", "veto", 1}, // 2813
  {wxSashEvent_GetEdge, "wxSashEvent", "getEdge", 1}, // 2814
  {wxSashEvent_GetDragRect, "wxSashEvent", "getDragRect", 1}, // 2815
  {wxSashEvent_GetDragStatus, "wxSashEvent", "getDragStatus", 1}, // 2816
  {wxListEvent_GetCacheFrom, "wxListEvent", "getCacheFrom", 1}, // 2817
  {wxListEvent_GetCacheTo, "wxListEvent", "getCacheTo", 1}, // 2818
  {wxListEvent_GetKeyCode, "wxListEvent", "getKeyCode", 1}, // 2819
  {wxListEvent_GetIndex, "wxListEvent", "getIndex", 1}, // 2820
  {wxListEvent_GetColumn, "wxListEvent", "getColumn", 1}, // 2821
  {wxListEvent_GetPoint, "wxListEvent", "getPoint", 1}, // 2822
  {wxListEvent_GetLabel, "wxListEvent", "getLabel", 1}, // 2823
  {wxListEvent_GetText, "wxListEvent", "getText", 1}, // 2824
  {wxListEvent_GetImage, "wxListEvent", "getImage", 1}, // 2825
  {wxListEvent_GetData, "wxListEvent", "getData", 1}, // 2826
  {wxListEvent_GetMask, "wxListEvent", "getMask", 1}, // 2827
  {wxListEvent_GetItem, "wxListEvent", "getItem", 1}, // 2828
  {wxListEvent_IsEditCancelled, "wxListEvent", "isEditCancelled", 1}, // 2829
  {wxDateEvent_GetDate, "wxDateEvent", "getDate", 1}, // 2830
  {wxCalendarEvent_GetWeekDay, "wxCalendarEvent", "getWeekDay", 1}, // 2831
  {wxCalendarEvent_GetDate, "wxCalendarEvent", "getDate", 1}, // 2832
  {wxFileDirPickerEvent_GetPath, "wxFileDirPickerEvent", "getPath", 1}, // 2833
  {wxColourPickerEvent_GetColour, "wxColourPickerEvent", "getColour", 1}, // 2834
  {wxFontPickerEvent_GetFont, "wxFontPickerEvent", "getFont", 1}, // 2835
  {wxStyledTextEvent_GetPosition, "wxStyledTextEvent", "getPosition", 1}, // 2836
  {wxStyledTextEvent_GetKey, "wxStyledTextEvent", "getKey", 1}, // 2837
  {wxStyledTextEvent_GetModifiers, "wxStyledTextEvent", "getModifiers", 1}, // 2838
  {wxStyledTextEvent_GetModificationType, "wxStyledTextEvent", "getModificationType", 1}, // 2839
  {wxStyledTextEvent_GetText, "wxStyledTextEvent", "getText", 1}, // 2840
  {wxStyledTextEvent_GetLength, "wxStyledTextEvent", "getLength", 1}, // 2841
  {wxStyledTextEvent_GetLinesAdded, "wxStyledTextEvent", "getLinesAdded", 1}, // 2842
  {wxStyledTextEvent_GetLine, "wxStyledTextEvent", "getLine", 1}, // 2843
  {wxStyledTextEvent_GetFoldLevelNow, "wxStyledTextEvent", "getFoldLevelNow", 1}, // 2844
  {wxStyledTextEvent_GetFoldLevelPrev, "wxStyledTextEvent", "getFoldLevelPrev", 1}, // 2845
  {wxStyledTextEvent_GetMargin, "wxStyledTextEvent", "getMargin", 1}, // 2846
  {wxStyledTextEvent_GetMessage, "wxStyledTextEvent", "getMessage", 1}, // 2847
  {wxStyledTextEvent_GetWParam, "wxStyledTextEvent", "getWParam", 1}, // 2848
  {wxStyledTextEvent_GetLParam, "wxStyledTextEvent", "getLParam", 1}, // 2849
  {wxStyledTextEvent_GetListType, "wxStyledTextEvent", "getListType", 1}, // 2850
  {wxStyledTextEvent_GetX, "wxStyledTextEvent", "getX", 1}, // 2851
  {wxStyledTextEvent_GetY, "wxStyledTextEvent", "getY", 1}, // 2852
  {wxStyledTextEvent_GetDragText, "wxStyledTextEvent", "getDragText", 1}, // 2853
  {wxStyledTextEvent_GetDragAllowMove, "wxStyledTextEvent", "getDragAllowMove", 1}, // 2854
  {wxStyledTextEvent_GetDragResult, "wxStyledTextEvent", "getDragResult", 1}, // 2855
  {wxStyledTextEvent_GetShift, "wxStyledTextEvent", "getShift", 1}, // 2856
  {wxStyledTextEvent_GetControl, "wxStyledTextEvent", "getControl", 1}, // 2857
  {wxStyledTextEvent_GetAlt, "wxStyledTextEvent", "getAlt", 1}, // 2858
  {utils_wxGetKeyState, "utils", "getKeyState", 1}, // 2859
  {utils_wxGetMousePosition, "utils", "getMousePosition", 0}, // 2860
  {utils_wxGetMouseState, "utils", "getMouseState", 0}, // 2861
  {utils_wxSetDetectableAutoRepeat, "utils", "setDetectableAutoRepeat", 1}, // 2862
  {utils_wxBell, "utils", "bell", 0}, // 2863
  {utils_wxFindMenuItemId, "utils", "findMenuItemId", 3}, // 2864
  {utils_wxFindWindowAtPoint, "utils", "findWindowAtPoint", 1}, // 2865
  {utils_wxBeginBusyCursor, "utils", "beginBusyCursor", 1}, // 2866
  {utils_wxEndBusyCursor, "utils", "endBusyCursor", 0}, // 2867
  {utils_wxIsBusy, "utils", "isBusy", 0}, // 2868
  {utils_wxShutdown, "utils", "shutdown", 1}, // 2869
  {utils_wxShell, "utils", "shell", 1}, // 2870
  {utils_wxLaunchDefaultBrowser, "utils", "launchDefaultBrowser", 2}, // 2871
  {utils_wxGetEmailAddress, "utils", "getEmailAddress", 0}, // 2872
  {utils_wxGetUserId, "utils", "getUserId", 0}, // 2873
  {utils_wxGetHomeDir, "utils", "getHomeDir", 0}, // 2874
  {utils_wxNewId, "utils", "newId", 0}, // 2875
  {utils_wxRegisterId, "utils", "registerId", 1}, // 2876
  {utils_wxGetCurrentId, "utils", "getCurrentId", 0}, // 2877
  {utils_wxGetOsDescription, "utils", "getOsDescription", 0}, // 2878
  {utils_wxIsPlatformLittleEndian, "utils", "isPlatformLittleEndian", 0}, // 2879
  {utils_wxIsPlatform64Bit, "utils", "isPlatform64Bit", 0}, // 2880
  {gdicmn_wxDisplaySize, "gdicmn", "displaySize", 0}, // 2881
  {gdicmn_wxSetCursor, "gdicmn", "setCursor", 1}, // 2882
  {wxPrintout_new, "wxPrintout", "new", 1}, // 2883
  {NULL, "wxPrintout", "destroy", 1}, // 2884 obj destructor wxPrintout_destruct
  {wxPrintout_GetDC, "wxPrintout", "getDC", 1}, // 2885
  {wxPrintout_GetPageSizeMM, "wxPrintout", "getPageSizeMM", 1}, // 2886
  {wxPrintout_GetPageSizePixels, "wxPrintout", "getPageSizePixels", 1}, // 2887
  {wxPrintout_GetPaperRectPixels, "wxPrintout", "getPaperRectPixels", 1}, // 2888
  {wxPrintout_GetPPIPrinter, "wxPrintout", "getPPIPrinter", 1}, // 2889
  {wxPrintout_GetPPIScreen, "wxPrintout", "getPPIScreen", 1}, // 2890
  {wxPrintout_GetTitle, "wxPrintout", "getTitle", 1}, // 2891
  {wxPrintout_IsPreview, "wxPrintout", "isPreview", 1}, // 2892
  {wxPrintout_FitThisSizeToPaper, "wxPrintout", "fitThisSizeToPaper", 2}, // 2893
  {wxPrintout_FitThisSizeToPage, "wxPrintout", "fitThisSizeToPage", 2}, // 2894
  {wxPrintout_FitThisSizeToPageMargins, "wxPrintout", "fitThisSizeToPageMargins", 3}, // 2895
  {wxPrintout_MapScreenSizeToPaper, "wxPrintout", "mapScreenSizeToPaper", 1}, // 2896
  {wxPrintout_MapScreenSizeToPage, "wxPrintout", "mapScreenSizeToPage", 1}, // 2897
  {wxPrintout_MapScreenSizeToPageMargins, "wxPrintout", "mapScreenSizeToPageMargins", 2}, // 2898
  {wxPrintout_MapScreenSizeToDevice, "wxPrintout", "mapScreenSizeToDevice", 1}, // 2899
  {wxPrintout_GetLogicalPaperRect, "wxPrintout", "getLogicalPaperRect", 1}, // 2900
  {wxPrintout_GetLogicalPageRect, "wxPrintout", "getLogicalPageRect", 1}, // 2901
  {wxPrintout_GetLogicalPageMarginsRect, "wxPrintout", "getLogicalPageMarginsRect", 2}, // 2902
  {wxPrintout_SetLogicalOrigin, "wxPrintout", "setLogicalOrigin", 3}, // 2903
  {wxPrintout_OffsetLogicalOrigin, "wxPrintout", "offsetLogicalOrigin", 3}, // 2904
  {wxStyledTextCtrl_new_2, "wxStyledTextCtrl", "new", 2}, // 2905
  {wxStyledTextCtrl_new_0, "wxStyledTextCtrl", "new", 0}, // 2906
  {NULL, "wxStyledTextCtrl", "destroy", 1}, // 2907 obj destructor wxStyledTextCtrl_destruct
  {wxStyledTextCtrl_Create, "wxStyledTextCtrl", "create", 3}, // 2908
  {wxStyledTextCtrl_AddText, "wxStyledTextCtrl", "addText", 2}, // 2909
  {wxStyledTextCtrl_InsertText, "wxStyledTextCtrl", "insertText", 3}, // 2910
  {wxStyledTextCtrl_ClearAll, "wxStyledTextCtrl", "clearAll", 1}, // 2911
  {wxStyledTextCtrl_ClearDocumentStyle, "wxStyledTextCtrl", "clearDocumentStyle", 1}, // 2912
  {wxStyledTextCtrl_GetLength, "wxStyledTextCtrl", "getLength", 1}, // 2913
  {wxStyledTextCtrl_GetCharAt, "wxStyledTextCtrl", "getCharAt", 2}, // 2914
  {wxStyledTextCtrl_GetCurrentPos, "wxStyledTextCtrl", "getCurrentPos", 1}, // 2915
  {wxStyledTextCtrl_GetAnchor, "wxStyledTextCtrl", "getAnchor", 1}, // 2916
  {wxStyledTextCtrl_GetStyleAt, "wxStyledTextCtrl", "getStyleAt", 2}, // 2917
  {wxStyledTextCtrl_Redo, "wxStyledTextCtrl", "redo", 1}, // 2918
  {wxStyledTextCtrl_SetUndoCollection, "wxStyledTextCtrl", "setUndoCollection", 2}, // 2919
  {wxStyledTextCtrl_SelectAll, "wxStyledTextCtrl", "selectAll", 1}, // 2920
  {wxStyledTextCtrl_SetSavePoint, "wxStyledTextCtrl", "setSavePoint", 1}, // 2921
  {wxStyledTextCtrl_CanRedo, "wxStyledTextCtrl", "canRedo", 1}, // 2922
  {wxStyledTextCtrl_MarkerLineFromHandle, "wxStyledTextCtrl", "markerLineFromHandle", 2}, // 2923
  {wxStyledTextCtrl_MarkerDeleteHandle, "wxStyledTextCtrl", "markerDeleteHandle", 2}, // 2924
  {wxStyledTextCtrl_GetUndoCollection, "wxStyledTextCtrl", "getUndoCollection", 1}, // 2925
  {wxStyledTextCtrl_GetViewWhiteSpace, "wxStyledTextCtrl", "getViewWhiteSpace", 1}, // 2926
  {wxStyledTextCtrl_SetViewWhiteSpace, "wxStyledTextCtrl", "setViewWhiteSpace", 2}, // 2927
  {wxStyledTextCtrl_PositionFromPoint, "wxStyledTextCtrl", "positionFromPoint", 2}, // 2928
  {wxStyledTextCtrl_PositionFromPointClose, "wxStyledTextCtrl", "positionFromPointClose", 3}, // 2929
  {wxStyledTextCtrl_GotoLine, "wxStyledTextCtrl", "gotoLine", 2}, // 2930
  {wxStyledTextCtrl_GotoPos, "wxStyledTextCtrl", "gotoPos", 2}, // 2931
  {wxStyledTextCtrl_SetAnchor, "wxStyledTextCtrl", "setAnchor", 2}, // 2932
  {wxStyledTextCtrl_GetCurLine, "wxStyledTextCtrl", "getCurLine", 1}, // 2933
  {wxStyledTextCtrl_GetEndStyled, "wxStyledTextCtrl", "getEndStyled", 1}, // 2934
  {wxStyledTextCtrl_ConvertEOLs, "wxStyledTextCtrl", "convertEOLs", 2}, // 2935
  {wxStyledTextCtrl_GetEOLMode, "wxStyledTextCtrl", "getEOLMode", 1}, // 2936
  {wxStyledTextCtrl_SetEOLMode, "wxStyledTextCtrl", "setEOLMode", 2}, // 2937
  {wxStyledTextCtrl_StartStyling, "wxStyledTextCtrl", "startStyling", 2}, // 2938
  {wxStyledTextCtrl_SetStyling, "wxStyledTextCtrl", "setStyling", 3}, // 2939
  {wxStyledTextCtrl_GetBufferedDraw, "wxStyledTextCtrl", "getBufferedDraw", 1}, // 2940
  {wxStyledTextCtrl_SetBufferedDraw, "wxStyledTextCtrl", "setBufferedDraw", 2}, // 2941
  {wxStyledTextCtrl_SetTabWidth, "wxStyledTextCtrl", "setTabWidth", 2}, // 2942
  {wxStyledTextCtrl_GetTabWidth, "wxStyledTextCtrl", "getTabWidth", 1}, // 2943
  {wxStyledTextCtrl_SetCodePage, "wxStyledTextCtrl", "setCodePage", 2}, // 2944
  {wxStyledTextCtrl_MarkerDefine, "wxStyledTextCtrl", "markerDefine", 4}, // 2945
  {wxStyledTextCtrl_MarkerSetForeground, "wxStyledTextCtrl", "markerSetForeground", 3}, // 2946
  {wxStyledTextCtrl_MarkerSetBackground, "wxStyledTextCtrl", "markerSetBackground", 3}, // 2947
  {wxStyledTextCtrl_MarkerAdd, "wxStyledTextCtrl", "markerAdd", 3}, // 2948
  {wxStyledTextCtrl_MarkerDelete, "wxStyledTextCtrl", "markerDelete", 3}, // 2949
  {wxStyledTextCtrl_MarkerDeleteAll, "wxStyledTextCtrl", "markerDeleteAll", 2}, // 2950
  {wxStyledTextCtrl_MarkerGet, "wxStyledTextCtrl", "markerGet", 2}, // 2951
  {wxStyledTextCtrl_MarkerNext, "wxStyledTextCtrl", "markerNext", 3}, // 2952
  {wxStyledTextCtrl_MarkerPrevious, "wxStyledTextCtrl", "markerPrevious", 3}, // 2953
  {wxStyledTextCtrl_MarkerDefineBitmap, "wxStyledTextCtrl", "markerDefineBitmap", 3}, // 2954
  {wxStyledTextCtrl_MarkerAddSet, "wxStyledTextCtrl", "markerAddSet", 3}, // 2955
  {wxStyledTextCtrl_MarkerSetAlpha, "wxStyledTextCtrl", "markerSetAlpha", 3}, // 2956
  {wxStyledTextCtrl_SetMarginType, "wxStyledTextCtrl", "setMarginType", 3}, // 2957
  {wxStyledTextCtrl_GetMarginType, "wxStyledTextCtrl", "getMarginType", 2}, // 2958
  {wxStyledTextCtrl_SetMarginWidth, "wxStyledTextCtrl", "setMarginWidth", 3}, // 2959
  {wxStyledTextCtrl_GetMarginWidth, "wxStyledTextCtrl", "getMarginWidth", 2}, // 2960
  {wxStyledTextCtrl_SetMarginMask, "wxStyledTextCtrl", "setMarginMask", 3}, // 2961
  {wxStyledTextCtrl_GetMarginMask, "wxStyledTextCtrl", "getMarginMask", 2}, // 2962
  {wxStyledTextCtrl_SetMarginSensitive, "wxStyledTextCtrl", "setMarginSensitive", 3}, // 2963
  {wxStyledTextCtrl_GetMarginSensitive, "wxStyledTextCtrl", "getMarginSensitive", 2}, // 2964
  {wxStyledTextCtrl_StyleClearAll, "wxStyledTextCtrl", "styleClearAll", 1}, // 2965
  {wxStyledTextCtrl_StyleSetForeground, "wxStyledTextCtrl", "styleSetForeground", 3}, // 2966
  {wxStyledTextCtrl_StyleSetBackground, "wxStyledTextCtrl", "styleSetBackground", 3}, // 2967
  {wxStyledTextCtrl_StyleSetBold, "wxStyledTextCtrl", "styleSetBold", 3}, // 2968
  {wxStyledTextCtrl_StyleSetItalic, "wxStyledTextCtrl", "styleSetItalic", 3}, // 2969
  {wxStyledTextCtrl_StyleSetSize, "wxStyledTextCtrl", "styleSetSize", 3}, // 2970
  {wxStyledTextCtrl_StyleSetFaceName, "wxStyledTextCtrl", "styleSetFaceName", 3}, // 2971
  {wxStyledTextCtrl_StyleSetEOLFilled, "wxStyledTextCtrl", "styleSetEOLFilled", 3}, // 2972
  {wxStyledTextCtrl_StyleResetDefault, "wxStyledTextCtrl", "styleResetDefault", 1}, // 2973
  {wxStyledTextCtrl_StyleSetUnderline, "wxStyledTextCtrl", "styleSetUnderline", 3}, // 2974
  {wxStyledTextCtrl_StyleSetCase, "wxStyledTextCtrl", "styleSetCase", 3}, // 2975
  {wxStyledTextCtrl_StyleSetHotSpot, "wxStyledTextCtrl", "styleSetHotSpot", 3}, // 2976
  {wxStyledTextCtrl_SetSelForeground, "wxStyledTextCtrl", "setSelForeground", 3}, // 2977
  {wxStyledTextCtrl_SetSelBackground, "wxStyledTextCtrl", "setSelBackground", 3}, // 2978
  {wxStyledTextCtrl_GetSelAlpha, "wxStyledTextCtrl", "getSelAlpha", 1}, // 2979
  {wxStyledTextCtrl_SetSelAlpha, "wxStyledTextCtrl", "setSelAlpha", 2}, // 2980
  {wxStyledTextCtrl_SetCaretForeground, "wxStyledTextCtrl", "setCaretForeground", 2}, // 2981
  {wxStyledTextCtrl_CmdKeyAssign, "wxStyledTextCtrl", "cmdKeyAssign", 4}, // 2982
  {wxStyledTextCtrl_CmdKeyClear, "wxStyledTextCtrl", "cmdKeyClear", 3}, // 2983
  {wxStyledTextCtrl_CmdKeyClearAll, "wxStyledTextCtrl", "cmdKeyClearAll", 1}, // 2984
  {wxStyledTextCtrl_SetStyleBytes, "wxStyledTextCtrl", "setStyleBytes", 2}, // 2985
  {wxStyledTextCtrl_StyleSetVisible, "wxStyledTextCtrl", "styleSetVisible", 3}, // 2986
  {wxStyledTextCtrl_GetCaretPeriod, "wxStyledTextCtrl", "getCaretPeriod", 1}, // 2987
  {wxStyledTextCtrl_SetCaretPeriod, "wxStyledTextCtrl", "setCaretPeriod", 2}, // 2988
  {wxStyledTextCtrl_SetWordChars, "wxStyledTextCtrl", "setWordChars", 2}, // 2989
  {wxStyledTextCtrl_BeginUndoAction, "wxStyledTextCtrl", "beginUndoAction", 1}, // 2990
  {wxStyledTextCtrl_EndUndoAction, "wxStyledTextCtrl", "endUndoAction", 1}, // 2991
  {wxStyledTextCtrl_IndicatorSetStyle, "wxStyledTextCtrl", "indicatorSetStyle", 3}, // 2992
  {wxStyledTextCtrl_IndicatorGetStyle, "wxStyledTextCtrl", "indicatorGetStyle", 2}, // 2993
  {wxStyledTextCtrl_IndicatorSetForeground, "wxStyledTextCtrl", "indicatorSetForeground", 3}, // 2994
  {wxStyledTextCtrl_IndicatorGetForeground, "wxStyledTextCtrl", "indicatorGetForeground", 2}, // 2995
  {wxStyledTextCtrl_SetWhitespaceForeground, "wxStyledTextCtrl", "setWhitespaceForeground", 3}, // 2996
  {wxStyledTextCtrl_SetWhitespaceBackground, "wxStyledTextCtrl", "setWhitespaceBackground", 3}, // 2997
  {wxStyledTextCtrl_GetStyleBits, "wxStyledTextCtrl", "getStyleBits", 1}, // 2998
  {wxStyledTextCtrl_SetLineState, "wxStyledTextCtrl", "setLineState", 3}, // 2999
  {wxStyledTextCtrl_GetLineState, "wxStyledTextCtrl", "getLineState", 2}, // 3000
  {wxStyledTextCtrl_GetMaxLineState, "wxStyledTextCtrl", "getMaxLineState", 1}, // 3001
  {wxStyledTextCtrl_GetCaretLineVisible, "wxStyledTextCtrl", "getCaretLineVisible", 1}, // 3002
  {wxStyledTextCtrl_SetCaretLineVisible, "wxStyledTextCtrl", "setCaretLineVisible", 2}, // 3003
  {wxStyledTextCtrl_GetCaretLineBackground, "wxStyledTextCtrl", "getCaretLineBackground", 1}, // 3004
  {wxStyledTextCtrl_SetCaretLineBackground, "wxStyledTextCtrl", "setCaretLineBackground", 2}, // 3005
  {wxStyledTextCtrl_AutoCompShow, "wxStyledTextCtrl", "autoCompShow", 3}, // 3006
  {wxStyledTextCtrl_AutoCompCancel, "wxStyledTextCtrl", "autoCompCancel", 1}, // 3007
  {wxStyledTextCtrl_AutoCompActive, "wxStyledTextCtrl", "autoCompActive", 1}, // 3008
  {wxStyledTextCtrl_AutoCompPosStart, "wxStyledTextCtrl", "autoCompPosStart", 1}, // 3009
  {wxStyledTextCtrl_AutoCompComplete, "wxStyledTextCtrl", "autoCompComplete", 1}, // 3010
  {wxStyledTextCtrl_AutoCompStops, "wxStyledTextCtrl", "autoCompStops", 2}, // 3011
  {wxStyledTextCtrl_AutoCompSetSeparator, "wxStyledTextCtrl", "autoCompSetSeparator", 2}, // 3012
  {wxStyledTextCtrl_AutoCompGetSeparator, "wxStyledTextCtrl", "autoCompGetSeparator", 1}, // 3013
  {wxStyledTextCtrl_AutoCompSelect, "wxStyledTextCtrl", "autoCompSelect", 2}, // 3014
  {wxStyledTextCtrl_AutoCompSetCancelAtStart, "wxStyledTextCtrl", "autoCompSetCancelAtStart", 2}, // 3015
  {wxStyledTextCtrl_AutoCompGetCancelAtStart, "wxStyledTextCtrl", "autoCompGetCancelAtStart", 1}, // 3016
  {wxStyledTextCtrl_AutoCompSetFillUps, "wxStyledTextCtrl", "autoCompSetFillUps", 2}, // 3017
  {wxStyledTextCtrl_AutoCompSetChooseSingle, "wxStyledTextCtrl", "autoCompSetChooseSingle", 2}, // 3018
  {wxStyledTextCtrl_AutoCompGetChooseSingle, "wxStyledTextCtrl", "autoCompGetChooseSingle", 1}, // 3019
  {wxStyledTextCtrl_AutoCompSetIgnoreCase, "wxStyledTextCtrl", "autoCompSetIgnoreCase", 2}, // 3020
  {wxStyledTextCtrl_AutoCompGetIgnoreCase, "wxStyledTextCtrl", "autoCompGetIgnoreCase", 1}, // 3021
  {wxStyledTextCtrl_UserListShow, "wxStyledTextCtrl", "userListShow", 3}, // 3022
  {wxStyledTextCtrl_AutoCompSetAutoHide, "wxStyledTextCtrl", "autoCompSetAutoHide", 2}, // 3023
  {wxStyledTextCtrl_AutoCompGetAutoHide, "wxStyledTextCtrl", "autoCompGetAutoHide", 1}, // 3024
  {wxStyledTextCtrl_AutoCompSetDropRestOfWord, "wxStyledTextCtrl", "autoCompSetDropRestOfWord", 2}, // 3025
  {wxStyledTextCtrl_AutoCompGetDropRestOfWord, "wxStyledTextCtrl", "autoCompGetDropRestOfWord", 1}, // 3026
  {wxStyledTextCtrl_RegisterImage, "wxStyledTextCtrl", "registerImage", 3}, // 3027
  {wxStyledTextCtrl_ClearRegisteredImages, "wxStyledTextCtrl", "clearRegisteredImages", 1}, // 3028
  {wxStyledTextCtrl_AutoCompGetTypeSeparator, "wxStyledTextCtrl", "autoCompGetTypeSeparator", 1}, // 3029
  {wxStyledTextCtrl_AutoCompSetTypeSeparator, "wxStyledTextCtrl", "autoCompSetTypeSeparator", 2}, // 3030
  {wxStyledTextCtrl_AutoCompSetMaxWidth, "wxStyledTextCtrl", "autoCompSetMaxWidth", 2}, // 3031
  {wxStyledTextCtrl_AutoCompGetMaxWidth, "wxStyledTextCtrl", "autoCompGetMaxWidth", 1}, // 3032
  {wxStyledTextCtrl_AutoCompSetMaxHeight, "wxStyledTextCtrl", "autoCompSetMaxHeight", 2}, // 3033
  {wxStyledTextCtrl_AutoCompGetMaxHeight, "wxStyledTextCtrl", "autoCompGetMaxHeight", 1}, // 3034
  {wxStyledTextCtrl_SetIndent, "wxStyledTextCtrl", "setIndent", 2}, // 3035
  {wxStyledTextCtrl_GetIndent, "wxStyledTextCtrl", "getIndent", 1}, // 3036
  {wxStyledTextCtrl_SetUseTabs, "wxStyledTextCtrl", "setUseTabs", 2}, // 3037
  {wxStyledTextCtrl_GetUseTabs, "wxStyledTextCtrl", "getUseTabs", 1}, // 3038
  {wxStyledTextCtrl_SetLineIndentation, "wxStyledTextCtrl", "setLineIndentation", 3}, // 3039
  {wxStyledTextCtrl_GetLineIndentation, "wxStyledTextCtrl", "getLineIndentation", 2}, // 3040
  {wxStyledTextCtrl_GetLineIndentPosition, "wxStyledTextCtrl", "getLineIndentPosition", 2}, // 3041
  {wxStyledTextCtrl_GetColumn, "wxStyledTextCtrl", "getColumn", 2}, // 3042
  {wxStyledTextCtrl_SetUseHorizontalScrollBar, "wxStyledTextCtrl", "setUseHorizontalScrollBar", 2}, // 3043
  {wxStyledTextCtrl_GetUseHorizontalScrollBar, "wxStyledTextCtrl", "getUseHorizontalScrollBar", 1}, // 3044
  {wxStyledTextCtrl_SetIndentationGuides, "wxStyledTextCtrl", "setIndentationGuides", 2}, // 3045
  {wxStyledTextCtrl_GetIndentationGuides, "wxStyledTextCtrl", "getIndentationGuides", 1}, // 3046
  {wxStyledTextCtrl_SetHighlightGuide, "wxStyledTextCtrl", "setHighlightGuide", 2}, // 3047
  {wxStyledTextCtrl_GetHighlightGuide, "wxStyledTextCtrl", "getHighlightGuide", 1}, // 3048
  {wxStyledTextCtrl_GetLineEndPosition, "wxStyledTextCtrl", "getLineEndPosition", 2}, // 3049
  {wxStyledTextCtrl_GetCodePage, "wxStyledTextCtrl", "getCodePage", 1}, // 3050
  {wxStyledTextCtrl_GetCaretForeground, "wxStyledTextCtrl", "getCaretForeground", 1}, // 3051
  {wxStyledTextCtrl_GetReadOnly, "wxStyledTextCtrl", "getReadOnly", 1}, // 3052
  {wxStyledTextCtrl_SetCurrentPos, "wxStyledTextCtrl", "setCurrentPos", 2}, // 3053
  {wxStyledTextCtrl_SetSelectionStart, "wxStyledTextCtrl", "setSelectionStart", 2}, // 3054
  {wxStyledTextCtrl_GetSelectionStart, "wxStyledTextCtrl", "getSelectionStart", 1}, // 3055
  {wxStyledTextCtrl_SetSelectionEnd, "wxStyledTextCtrl", "setSelectionEnd", 2}, // 3056
  {wxStyledTextCtrl_GetSelectionEnd, "wxStyledTextCtrl", "getSelectionEnd", 1}, // 3057
  {wxStyledTextCtrl_SetPrintMagnification, "wxStyledTextCtrl", "setPrintMagnification", 2}, // 3058
  {wxStyledTextCtrl_GetPrintMagnification, "wxStyledTextCtrl", "getPrintMagnification", 1}, // 3059
  {wxStyledTextCtrl_SetPrintColourMode, "wxStyledTextCtrl", "setPrintColourMode", 2}, // 3060
  {wxStyledTextCtrl_GetPrintColourMode, "wxStyledTextCtrl", "getPrintColourMode", 1}, // 3061
  {wxStyledTextCtrl_FindText, "wxStyledTextCtrl", "findText", 5}, // 3062
  {wxStyledTextCtrl_FormatRange, "wxStyledTextCtrl", "formatRange", 8}, // 3063
  {wxStyledTextCtrl_GetFirstVisibleLine, "wxStyledTextCtrl", "getFirstVisibleLine", 1}, // 3064
  {wxStyledTextCtrl_GetLine, "wxStyledTextCtrl", "getLine", 2}, // 3065
  {wxStyledTextCtrl_GetLineCount, "wxStyledTextCtrl", "getLineCount", 1}, // 3066
  {wxStyledTextCtrl_SetMarginLeft, "wxStyledTextCtrl", "setMarginLeft", 2}, // 3067
  {wxStyledTextCtrl_GetMarginLeft, "wxStyledTextCtrl", "getMarginLeft", 1}, // 3068
  {wxStyledTextCtrl_SetMarginRight, "wxStyledTextCtrl", "setMarginRight", 2}, // 3069
  {wxStyledTextCtrl_GetMarginRight, "wxStyledTextCtrl", "getMarginRight", 1}, // 3070
  {wxStyledTextCtrl_GetModify, "wxStyledTextCtrl", "getModify", 1}, // 3071
  {wxStyledTextCtrl_SetSelection, "wxStyledTextCtrl", "setSelection", 3}, // 3072
  {wxStyledTextCtrl_GetSelectedText, "wxStyledTextCtrl", "getSelectedText", 1}, // 3073
  {wxStyledTextCtrl_GetTextRange, "wxStyledTextCtrl", "getTextRange", 3}, // 3074
  {wxStyledTextCtrl_HideSelection, "wxStyledTextCtrl", "hideSelection", 2}, // 3075
  {wxStyledTextCtrl_LineFromPosition, "wxStyledTextCtrl", "lineFromPosition", 2}, // 3076
  {wxStyledTextCtrl_PositionFromLine, "wxStyledTextCtrl", "positionFromLine", 2}, // 3077
  {wxStyledTextCtrl_LineScroll, "wxStyledTextCtrl", "lineScroll", 3}, // 3078
  {wxStyledTextCtrl_EnsureCaretVisible, "wxStyledTextCtrl", "ensureCaretVisible", 1}, // 3079
  {wxStyledTextCtrl_ReplaceSelection, "wxStyledTextCtrl", "replaceSelection", 2}, // 3080
  {wxStyledTextCtrl_SetReadOnly, "wxStyledTextCtrl", "setReadOnly", 2}, // 3081
  {wxStyledTextCtrl_CanPaste, "wxStyledTextCtrl", "canPaste", 1}, // 3082
  {wxStyledTextCtrl_CanUndo, "wxStyledTextCtrl", "canUndo", 1}, // 3083
  {wxStyledTextCtrl_EmptyUndoBuffer, "wxStyledTextCtrl", "emptyUndoBuffer", 1}, // 3084
  {wxStyledTextCtrl_Undo, "wxStyledTextCtrl", "undo", 1}, // 3085
  {wxStyledTextCtrl_Cut, "wxStyledTextCtrl", "cut", 1}, // 3086
  {wxStyledTextCtrl_Copy, "wxStyledTextCtrl", "copy", 1}, // 3087
  {wxStyledTextCtrl_Paste, "wxStyledTextCtrl", "paste", 1}, // 3088
  {wxStyledTextCtrl_Clear, "wxStyledTextCtrl", "clear", 1}, // 3089
  {wxStyledTextCtrl_SetText, "wxStyledTextCtrl", "setText", 2}, // 3090
  {wxStyledTextCtrl_GetText, "wxStyledTextCtrl", "getText", 1}, // 3091
  {wxStyledTextCtrl_GetTextLength, "wxStyledTextCtrl", "getTextLength", 1}, // 3092
  {wxStyledTextCtrl_GetOvertype, "wxStyledTextCtrl", "getOvertype", 1}, // 3093
  {wxStyledTextCtrl_SetCaretWidth, "wxStyledTextCtrl", "setCaretWidth", 2}, // 3094
  {wxStyledTextCtrl_GetCaretWidth, "wxStyledTextCtrl", "getCaretWidth", 1}, // 3095
  {wxStyledTextCtrl_SetTargetStart, "wxStyledTextCtrl", "setTargetStart", 2}, // 3096
  {wxStyledTextCtrl_GetTargetStart, "wxStyledTextCtrl", "getTargetStart", 1}, // 3097
  {wxStyledTextCtrl_SetTargetEnd, "wxStyledTextCtrl", "setTargetEnd", 2}, // 3098
  {wxStyledTextCtrl_GetTargetEnd, "wxStyledTextCtrl", "getTargetEnd", 1}, // 3099
  {wxStyledTextCtrl_ReplaceTarget, "wxStyledTextCtrl", "replaceTarget", 2}, // 3100
  {wxStyledTextCtrl_SearchInTarget, "wxStyledTextCtrl", "searchInTarget", 2}, // 3101
  {wxStyledTextCtrl_SetSearchFlags, "wxStyledTextCtrl", "setSearchFlags", 2}, // 3102
  {wxStyledTextCtrl_GetSearchFlags, "wxStyledTextCtrl", "getSearchFlags", 1}, // 3103
  {wxStyledTextCtrl_CallTipShow, "wxStyledTextCtrl", "callTipShow", 3}, // 3104
  {wxStyledTextCtrl_CallTipCancel, "wxStyledTextCtrl", "callTipCancel", 1}, // 3105
  {wxStyledTextCtrl_CallTipActive, "wxStyledTextCtrl", "callTipActive", 1}, // 3106
  {wxStyledTextCtrl_CallTipPosAtStart, "wxStyledTextCtrl", "callTipPosAtStart", 1}, // 3107
  {wxStyledTextCtrl_CallTipSetHighlight, "wxStyledTextCtrl", "callTipSetHighlight", 3}, // 3108
  {wxStyledTextCtrl_CallTipSetBackground, "wxStyledTextCtrl", "callTipSetBackground", 2}, // 3109
  {wxStyledTextCtrl_CallTipSetForeground, "wxStyledTextCtrl", "callTipSetForeground", 2}, // 3110
  {wxStyledTextCtrl_CallTipSetForegroundHighlight, "wxStyledTextCtrl", "callTipSetForegroundHighlight", 2}, // 3111
  {wxStyledTextCtrl_CallTipUseStyle, "wxStyledTextCtrl", "callTipUseStyle", 2}, // 3112
  {wxStyledTextCtrl_VisibleFromDocLine, "wxStyledTextCtrl", "visibleFromDocLine", 2}, // 3113
  {wxStyledTextCtrl_DocLineFromVisible, "wxStyledTextCtrl", "docLineFromVisible", 2}, // 3114
  {wxStyledTextCtrl_WrapCount, "wxStyledTextCtrl", "wrapCount", 2}, // 3115
  {wxStyledTextCtrl_SetFoldLevel, "wxStyledTextCtrl", "setFoldLevel", 3}, // 3116
  {wxStyledTextCtrl_GetFoldLevel, "wxStyledTextCtrl", "getFoldLevel", 2}, // 3117
  {wxStyledTextCtrl_GetLastChild, "wxStyledTextCtrl", "getLastChild", 3}, // 3118
  {wxStyledTextCtrl_GetFoldParent, "wxStyledTextCtrl", "getFoldParent", 2}, // 3119
  {wxStyledTextCtrl_ShowLines, "wxStyledTextCtrl", "showLines", 3}, // 3120
  {wxStyledTextCtrl_HideLines, "wxStyledTextCtrl", "hideLines", 3}, // 3121
  {wxStyledTextCtrl_GetLineVisible, "wxStyledTextCtrl", "getLineVisible", 2}, // 3122
  {wxStyledTextCtrl_SetFoldExpanded, "wxStyledTextCtrl", "setFoldExpanded", 3}, // 3123
  {wxStyledTextCtrl_GetFoldExpanded, "wxStyledTextCtrl", "getFoldExpanded", 2}, // 3124
  {wxStyledTextCtrl_ToggleFold, "wxStyledTextCtrl", "toggleFold", 2}, // 3125
  {wxStyledTextCtrl_EnsureVisible, "wxStyledTextCtrl", "ensureVisible", 2}, // 3126
  {wxStyledTextCtrl_SetFoldFlags, "wxStyledTextCtrl", "setFoldFlags", 2}, // 3127
  {wxStyledTextCtrl_EnsureVisibleEnforcePolicy, "wxStyledTextCtrl", "ensureVisibleEnforcePolicy", 2}, // 3128
  {wxStyledTextCtrl_SetTabIndents, "wxStyledTextCtrl", "setTabIndents", 2}, // 3129
  {wxStyledTextCtrl_GetTabIndents, "wxStyledTextCtrl", "getTabIndents", 1}, // 3130
  {wxStyledTextCtrl_SetBackSpaceUnIndents, "wxStyledTextCtrl", "setBackSpaceUnIndents", 2}, // 3131
  {wxStyledTextCtrl_GetBackSpaceUnIndents, "wxStyledTextCtrl", "getBackSpaceUnIndents", 1}, // 3132
  {wxStyledTextCtrl_SetMouseDwellTime, "wxStyledTextCtrl", "setMouseDwellTime", 2}, // 3133
  {wxStyledTextCtrl_GetMouseDwellTime, "wxStyledTextCtrl", "getMouseDwellTime", 1}, // 3134
  {wxStyledTextCtrl_WordStartPosition, "wxStyledTextCtrl", "wordStartPosition", 3}, // 3135
  {wxStyledTextCtrl_WordEndPosition, "wxStyledTextCtrl", "wordEndPosition", 3}, // 3136
  {wxStyledTextCtrl_SetWrapMode, "wxStyledTextCtrl", "setWrapMode", 2}, // 3137
  {wxStyledTextCtrl_GetWrapMode, "wxStyledTextCtrl", "getWrapMode", 1}, // 3138
  {wxStyledTextCtrl_SetWrapVisualFlags, "wxStyledTextCtrl", "setWrapVisualFlags", 2}, // 3139
  {wxStyledTextCtrl_GetWrapVisualFlags, "wxStyledTextCtrl", "getWrapVisualFlags", 1}, // 3140
  {wxStyledTextCtrl_SetWrapVisualFlagsLocation, "wxStyledTextCtrl", "setWrapVisualFlagsLocation", 2}, // 3141
  {wxStyledTextCtrl_GetWrapVisualFlagsLocation, "wxStyledTextCtrl", "getWrapVisualFlagsLocation", 1}, // 3142
  {wxStyledTextCtrl_SetWrapStartIndent, "wxStyledTextCtrl", "setWrapStartIndent", 2}, // 3143
  {wxStyledTextCtrl_GetWrapStartIndent, "wxStyledTextCtrl", "getWrapStartIndent", 1}, // 3144
  {wxStyledTextCtrl_SetLayoutCache, "wxStyledTextCtrl", "setLayoutCache", 2}, // 3145
  {wxStyledTextCtrl_GetLayoutCache, "wxStyledTextCtrl", "getLayoutCache", 1}, // 3146
  {wxStyledTextCtrl_SetScrollWidth, "wxStyledTextCtrl", "setScrollWidth", 2}, // 3147
  {wxStyledTextCtrl_GetScrollWidth, "wxStyledTextCtrl", "getScrollWidth", 1}, // 3148
  {wxStyledTextCtrl_TextWidth, "wxStyledTextCtrl", "textWidth", 3}, // 3149
  {wxStyledTextCtrl_GetEndAtLastLine, "wxStyledTextCtrl", "getEndAtLastLine", 1}, // 3150
  {wxStyledTextCtrl_TextHeight, "wxStyledTextCtrl", "textHeight", 2}, // 3151
  {wxStyledTextCtrl_SetUseVerticalScrollBar, "wxStyledTextCtrl", "setUseVerticalScrollBar", 2}, // 3152
  {wxStyledTextCtrl_GetUseVerticalScrollBar, "wxStyledTextCtrl", "getUseVerticalScrollBar", 1}, // 3153
  {wxStyledTextCtrl_AppendText, "wxStyledTextCtrl", "appendText", 2}, // 3154
  {wxStyledTextCtrl_GetTwoPhaseDraw, "wxStyledTextCtrl", "getTwoPhaseDraw", 1}, // 3155
  {wxStyledTextCtrl_SetTwoPhaseDraw, "wxStyledTextCtrl", "setTwoPhaseDraw", 2}, // 3156
  {wxStyledTextCtrl_TargetFromSelection, "wxStyledTextCtrl", "targetFromSelection", 1}, // 3157
  {wxStyledTextCtrl_LinesJoin, "wxStyledTextCtrl", "linesJoin", 1}, // 3158
  {wxStyledTextCtrl_LinesSplit, "wxStyledTextCtrl", "linesSplit", 2}, // 3159
  {wxStyledTextCtrl_SetFoldMarginColour, "wxStyledTextCtrl", "setFoldMarginColour", 3}, // 3160
  {wxStyledTextCtrl_SetFoldMarginHiColour, "wxStyledTextCtrl", "setFoldMarginHiColour", 3}, // 3161
  {wxStyledTextCtrl_LineDown, "wxStyledTextCtrl", "lineDown", 1}, // 3162
  {wxStyledTextCtrl_LineDownExtend, "wxStyledTextCtrl", "lineDownExtend", 1}, // 3163
  {wxStyledTextCtrl_LineUp, "wxStyledTextCtrl", "lineUp", 1}, // 3164
  {wxStyledTextCtrl_LineUpExtend, "wxStyledTextCtrl", "lineUpExtend", 1}, // 3165
  {wxStyledTextCtrl_CharLeft, "wxStyledTextCtrl", "charLeft", 1}, // 3166
  {wxStyledTextCtrl_CharLeftExtend, "wxStyledTextCtrl", "charLeftExtend", 1}, // 3167
  {wxStyledTextCtrl_CharRight, "wxStyledTextCtrl", "charRight", 1}, // 3168
  {wxStyledTextCtrl_CharRightExtend, "wxStyledTextCtrl", "charRightExtend", 1}, // 3169
  {wxStyledTextCtrl_WordLeft, "wxStyledTextCtrl", "wordLeft", 1}, // 3170
  {wxStyledTextCtrl_WordLeftExtend, "wxStyledTextCtrl", "wordLeftExtend", 1}, // 3171
  {wxStyledTextCtrl_WordRight, "wxStyledTextCtrl", "wordRight", 1}, // 3172
  {wxStyledTextCtrl_WordRightExtend, "wxStyledTextCtrl", "wordRightExtend", 1}, // 3173
  {wxStyledTextCtrl_Home, "wxStyledTextCtrl", "home", 1}, // 3174
  {wxStyledTextCtrl_HomeExtend, "wxStyledTextCtrl", "homeExtend", 1}, // 3175
  {wxStyledTextCtrl_LineEnd, "wxStyledTextCtrl", "lineEnd", 1}, // 3176
  {wxStyledTextCtrl_LineEndExtend, "wxStyledTextCtrl", "lineEndExtend", 1}, // 3177
  {wxStyledTextCtrl_DocumentStart, "wxStyledTextCtrl", "documentStart", 1}, // 3178
  {wxStyledTextCtrl_DocumentStartExtend, "wxStyledTextCtrl", "documentStartExtend", 1}, // 3179
  {wxStyledTextCtrl_DocumentEnd, "wxStyledTextCtrl", "documentEnd", 1}, // 3180
  {wxStyledTextCtrl_DocumentEndExtend, "wxStyledTextCtrl", "documentEndExtend", 1}, // 3181
  {wxStyledTextCtrl_PageUp, "wxStyledTextCtrl", "pageUp", 1}, // 3182
  {wxStyledTextCtrl_PageUpExtend, "wxStyledTextCtrl", "pageUpExtend", 1}, // 3183
  {wxStyledTextCtrl_PageDown, "wxStyledTextCtrl", "pageDown", 1}, // 3184
  {wxStyledTextCtrl_PageDownExtend, "wxStyledTextCtrl", "pageDownExtend", 1}, // 3185
  {wxStyledTextCtrl_EditToggleOvertype, "wxStyledTextCtrl", "editToggleOvertype", 1}, // 3186
  {wxStyledTextCtrl_Cancel, "wxStyledTextCtrl", "cancel", 1}, // 3187
  {wxStyledTextCtrl_DeleteBack, "wxStyledTextCtrl", "deleteBack", 1}, // 3188
  {wxStyledTextCtrl_Tab, "wxStyledTextCtrl", "tab", 1}, // 3189
  {wxStyledTextCtrl_BackTab, "wxStyledTextCtrl", "backTab", 1}, // 3190
  {wxStyledTextCtrl_NewLine, "wxStyledTextCtrl", "newLine", 1}, // 3191
  {wxStyledTextCtrl_FormFeed, "wxStyledTextCtrl", "formFeed", 1}, // 3192
  {wxStyledTextCtrl_VCHome, "wxStyledTextCtrl", "vCHome", 1}, // 3193
  {wxStyledTextCtrl_VCHomeExtend, "wxStyledTextCtrl", "vCHomeExtend", 1}, // 3194
  {wxStyledTextCtrl_ZoomIn, "wxStyledTextCtrl", "zoomIn", 1}, // 3195
  {wxStyledTextCtrl_ZoomOut, "wxStyledTextCtrl", "zoomOut", 1}, // 3196
  {wxStyledTextCtrl_DelWordLeft, "wxStyledTextCtrl", "delWordLeft", 1}, // 3197
  {wxStyledTextCtrl_DelWordRight, "wxStyledTextCtrl", "delWordRight", 1}, // 3198
  {wxStyledTextCtrl_LineCut, "wxStyledTextCtrl", "lineCut", 1}, // 3199
  {wxStyledTextCtrl_LineDelete, "wxStyledTextCtrl", "lineDelete", 1}, // 3200
  {wxStyledTextCtrl_LineTranspose, "wxStyledTextCtrl", "lineTranspose", 1}, // 3201
  {wxStyledTextCtrl_LineDuplicate, "wxStyledTextCtrl", "lineDuplicate", 1}, // 3202
  {wxStyledTextCtrl_LowerCase, "wxStyledTextCtrl", "lowerCase", 1}, // 3203
  {wxStyledTextCtrl_UpperCase, "wxStyledTextCtrl", "upperCase", 1}, // 3204
  {wxStyledTextCtrl_LineScrollDown, "wxStyledTextCtrl", "lineScrollDown", 1}, // 3205
  {wxStyledTextCtrl_LineScrollUp, "wxStyledTextCtrl", "lineScrollUp", 1}, // 3206
  {wxStyledTextCtrl_DeleteBackNotLine, "wxStyledTextCtrl", "deleteBackNotLine", 1}, // 3207
  {wxStyledTextCtrl_HomeDisplay, "wxStyledTextCtrl", "homeDisplay", 1}, // 3208
  {wxStyledTextCtrl_HomeDisplayExtend, "wxStyledTextCtrl", "homeDisplayExtend", 1}, // 3209
  {wxStyledTextCtrl_LineEndDisplay, "wxStyledTextCtrl", "lineEndDisplay", 1}, // 3210
  {wxStyledTextCtrl_LineEndDisplayExtend, "wxStyledTextCtrl", "lineEndDisplayExtend", 1}, // 3211
  {wxStyledTextCtrl_HomeWrapExtend, "wxStyledTextCtrl", "homeWrapExtend", 1}, // 3212
  {wxStyledTextCtrl_LineEndWrap, "wxStyledTextCtrl", "lineEndWrap", 1}, // 3213
  {wxStyledTextCtrl_LineEndWrapExtend, "wxStyledTextCtrl", "lineEndWrapExtend", 1}, // 3214
  {wxStyledTextCtrl_VCHomeWrap, "wxStyledTextCtrl", "vCHomeWrap", 1}, // 3215
  {wxStyledTextCtrl_VCHomeWrapExtend, "wxStyledTextCtrl", "vCHomeWrapExtend", 1}, // 3216
  {wxStyledTextCtrl_LineCopy, "wxStyledTextCtrl", "lineCopy", 1}, // 3217
  {wxStyledTextCtrl_MoveCaretInsideView, "wxStyledTextCtrl", "moveCaretInsideView", 1}, // 3218
  {wxStyledTextCtrl_LineLength, "wxStyledTextCtrl", "lineLength", 2}, // 3219
  {wxStyledTextCtrl_BraceHighlight, "wxStyledTextCtrl", "braceHighlight", 3}, // 3220
  {wxStyledTextCtrl_BraceBadLight, "wxStyledTextCtrl", "braceBadLight", 2}, // 3221
  {wxStyledTextCtrl_BraceMatch, "wxStyledTextCtrl", "braceMatch", 2}, // 3222
  {wxStyledTextCtrl_GetViewEOL, "wxStyledTextCtrl", "getViewEOL", 1}, // 3223
  {wxStyledTextCtrl_SetViewEOL, "wxStyledTextCtrl", "setViewEOL", 2}, // 3224
  {wxStyledTextCtrl_SetModEventMask, "wxStyledTextCtrl", "setModEventMask", 2}, // 3225
  {wxStyledTextCtrl_GetEdgeColumn, "wxStyledTextCtrl", "getEdgeColumn", 1}, // 3226
  {wxStyledTextCtrl_SetEdgeColumn, "wxStyledTextCtrl", "setEdgeColumn", 2}, // 3227
  {wxStyledTextCtrl_SetEdgeMode, "wxStyledTextCtrl", "setEdgeMode", 2}, // 3228
  {wxStyledTextCtrl_GetEdgeMode, "wxStyledTextCtrl", "getEdgeMode", 1}, // 3229
  {wxStyledTextCtrl_GetEdgeColour, "wxStyledTextCtrl", "getEdgeColour", 1}, // 3230
  {wxStyledTextCtrl_SetEdgeColour, "wxStyledTextCtrl", "setEdgeColour", 2}, // 3231
  {wxStyledTextCtrl_SearchAnchor, "wxStyledTextCtrl", "searchAnchor", 1}, // 3232
  {wxStyledTextCtrl_SearchNext, "wxStyledTextCtrl", "searchNext", 3}, // 3233
  {wxStyledTextCtrl_SearchPrev, "wxStyledTextCtrl", "searchPrev", 3}, // 3234
  {wxStyledTextCtrl_LinesOnScreen, "wxStyledTextCtrl", "linesOnScreen", 1}, // 3235
  {wxStyledTextCtrl_UsePopUp, "wxStyledTextCtrl", "usePopUp", 2}, // 3236
  {wxStyledTextCtrl_SelectionIsRectangle, "wxStyledTextCtrl", "selectionIsRectangle", 1}, // 3237
  {wxStyledTextCtrl_SetZoom, "wxStyledTextCtrl", "setZoom", 2}, // 3238
  {wxStyledTextCtrl_GetZoom, "wxStyledTextCtrl", "getZoom", 1}, // 3239
  {wxStyledTextCtrl_GetModEventMask, "wxStyledTextCtrl", "getModEventMask", 1}, // 3240
  {wxStyledTextCtrl_SetSTCFocus, "wxStyledTextCtrl", "setSTCFocus", 2}, // 3241
  {wxStyledTextCtrl_GetSTCFocus, "wxStyledTextCtrl", "getSTCFocus", 1}, // 3242
  {wxStyledTextCtrl_SetStatus, "wxStyledTextCtrl", "setStatus", 2}, // 3243
  {wxStyledTextCtrl_GetStatus, "wxStyledTextCtrl", "getStatus", 1}, // 3244
  {wxStyledTextCtrl_SetMouseDownCaptures, "wxStyledTextCtrl", "setMouseDownCaptures", 2}, // 3245
  {wxStyledTextCtrl_GetMouseDownCaptures, "wxStyledTextCtrl", "getMouseDownCaptures", 1}, // 3246
  {wxStyledTextCtrl_SetSTCCursor, "wxStyledTextCtrl", "setSTCCursor", 2}, // 3247
  {wxStyledTextCtrl_GetSTCCursor, "wxStyledTextCtrl", "getSTCCursor", 1}, // 3248
  {wxStyledTextCtrl_SetControlCharSymbol, "wxStyledTextCtrl", "setControlCharSymbol", 2}, // 3249
  {wxStyledTextCtrl_GetControlCharSymbol, "wxStyledTextCtrl", "getControlCharSymbol", 1}, // 3250
  {wxStyledTextCtrl_WordPartLeft, "wxStyledTextCtrl", "wordPartLeft", 1}, // 3251
  {wxStyledTextCtrl_WordPartLeftExtend, "wxStyledTextCtrl", "wordPartLeftExtend", 1}, // 3252
  {wxStyledTextCtrl_WordPartRight, "wxStyledTextCtrl", "wordPartRight", 1}, // 3253
  {wxStyledTextCtrl_WordPartRightExtend, "wxStyledTextCtrl", "wordPartRightExtend", 1}, // 3254
  {wxStyledTextCtrl_SetVisiblePolicy, "wxStyledTextCtrl", "setVisiblePolicy", 3}, // 3255
  {wxStyledTextCtrl_DelLineLeft, "wxStyledTextCtrl", "delLineLeft", 1}, // 3256
  {wxStyledTextCtrl_DelLineRight, "wxStyledTextCtrl", "delLineRight", 1}, // 3257
  {wxStyledTextCtrl_GetXOffset, "wxStyledTextCtrl", "getXOffset", 1}, // 3258
  {wxStyledTextCtrl_ChooseCaretX, "wxStyledTextCtrl", "chooseCaretX", 1}, // 3259
  {wxStyledTextCtrl_SetXCaretPolicy, "wxStyledTextCtrl", "setXCaretPolicy", 3}, // 3260
  {wxStyledTextCtrl_SetYCaretPolicy, "wxStyledTextCtrl", "setYCaretPolicy", 3}, // 3261
  {wxStyledTextCtrl_GetPrintWrapMode, "wxStyledTextCtrl", "getPrintWrapMode", 1}, // 3262
  {wxStyledTextCtrl_SetHotspotActiveForeground, "wxStyledTextCtrl", "setHotspotActiveForeground", 3}, // 3263
  {wxStyledTextCtrl_SetHotspotActiveBackground, "wxStyledTextCtrl", "setHotspotActiveBackground", 3}, // 3264
  {wxStyledTextCtrl_SetHotspotActiveUnderline, "wxStyledTextCtrl", "setHotspotActiveUnderline", 2}, // 3265
  {wxStyledTextCtrl_SetHotspotSingleLine, "wxStyledTextCtrl", "setHotspotSingleLine", 2}, // 3266
  {wxStyledTextCtrl_ParaDownExtend, "wxStyledTextCtrl", "paraDownExtend", 1}, // 3267
  {wxStyledTextCtrl_ParaUp, "wxStyledTextCtrl", "paraUp", 1}, // 3268
  {wxStyledTextCtrl_ParaUpExtend, "wxStyledTextCtrl", "paraUpExtend", 1}, // 3269
  {wxStyledTextCtrl_PositionBefore, "wxStyledTextCtrl", "positionBefore", 2}, // 3270
  {wxStyledTextCtrl_PositionAfter, "wxStyledTextCtrl", "positionAfter", 2}, // 3271
  {wxStyledTextCtrl_CopyRange, "wxStyledTextCtrl", "copyRange", 3}, // 3272
  {wxStyledTextCtrl_CopyText, "wxStyledTextCtrl", "copyText", 3}, // 3273
  {wxStyledTextCtrl_SetSelectionMode, "wxStyledTextCtrl", "setSelectionMode", 2}, // 3274
  {wxStyledTextCtrl_GetSelectionMode, "wxStyledTextCtrl", "getSelectionMode", 1}, // 3275
  {wxStyledTextCtrl_LineDownRectExtend, "wxStyledTextCtrl", "lineDownRectExtend", 1}, // 3276
  {wxStyledTextCtrl_LineUpRectExtend, "wxStyledTextCtrl", "lineUpRectExtend", 1}, // 3277
  {wxStyledTextCtrl_CharLeftRectExtend, "wxStyledTextCtrl", "charLeftRectExtend", 1}, // 3278
  {wxStyledTextCtrl_CharRightRectExtend, "wxStyledTextCtrl", "charRightRectExtend", 1}, // 3279
  {wxStyledTextCtrl_HomeRectExtend, "wxStyledTextCtrl", "homeRectExtend", 1}, // 3280
  {wxStyledTextCtrl_VCHomeRectExtend, "wxStyledTextCtrl", "vCHomeRectExtend", 1}, // 3281
  {wxStyledTextCtrl_LineEndRectExtend, "wxStyledTextCtrl", "lineEndRectExtend", 1}, // 3282
  {wxStyledTextCtrl_PageUpRectExtend, "wxStyledTextCtrl", "pageUpRectExtend", 1}, // 3283
  {wxStyledTextCtrl_PageDownRectExtend, "wxStyledTextCtrl", "pageDownRectExtend", 1}, // 3284
  {wxStyledTextCtrl_StutteredPageUp, "wxStyledTextCtrl", "stutteredPageUp", 1}, // 3285
  {wxStyledTextCtrl_StutteredPageUpExtend, "wxStyledTextCtrl", "stutteredPageUpExtend", 1}, // 3286
  {wxStyledTextCtrl_StutteredPageDown, "wxStyledTextCtrl", "stutteredPageDown", 1}, // 3287
  {wxStyledTextCtrl_StutteredPageDownExtend, "wxStyledTextCtrl", "stutteredPageDownExtend", 1}, // 3288
  {wxStyledTextCtrl_WordLeftEnd, "wxStyledTextCtrl", "wordLeftEnd", 1}, // 3289
  {wxStyledTextCtrl_WordLeftEndExtend, "wxStyledTextCtrl", "wordLeftEndExtend", 1}, // 3290
  {wxStyledTextCtrl_WordRightEnd, "wxStyledTextCtrl", "wordRightEnd", 1}, // 3291
  {wxStyledTextCtrl_WordRightEndExtend, "wxStyledTextCtrl", "wordRightEndExtend", 1}, // 3292
  {wxStyledTextCtrl_SetWhitespaceChars, "wxStyledTextCtrl", "setWhitespaceChars", 2}, // 3293
  {wxStyledTextCtrl_SetCharsDefault, "wxStyledTextCtrl", "setCharsDefault", 1}, // 3294
  {wxStyledTextCtrl_AutoCompGetCurrent, "wxStyledTextCtrl", "autoCompGetCurrent", 1}, // 3295
  {wxStyledTextCtrl_Allocate, "wxStyledTextCtrl", "allocate", 2}, // 3296
  {wxStyledTextCtrl_FindColumn, "wxStyledTextCtrl", "findColumn", 3}, // 3297
  {wxStyledTextCtrl_GetCaretSticky, "wxStyledTextCtrl", "getCaretSticky", 1}, // 3298
  {wxStyledTextCtrl_SetCaretSticky, "wxStyledTextCtrl", "setCaretSticky", 2}, // 3299
  {wxStyledTextCtrl_ToggleCaretSticky, "wxStyledTextCtrl", "toggleCaretSticky", 1}, // 3300
  {wxStyledTextCtrl_SetPasteConvertEndings, "wxStyledTextCtrl", "setPasteConvertEndings", 2}, // 3301
  {wxStyledTextCtrl_GetPasteConvertEndings, "wxStyledTextCtrl", "getPasteConvertEndings", 1}, // 3302
  {wxStyledTextCtrl_SelectionDuplicate, "wxStyledTextCtrl", "selectionDuplicate", 1}, // 3303
  {wxStyledTextCtrl_SetCaretLineBackAlpha, "wxStyledTextCtrl", "setCaretLineBackAlpha", 2}, // 3304
  {wxStyledTextCtrl_GetCaretLineBackAlpha, "wxStyledTextCtrl", "getCaretLineBackAlpha", 1}, // 3305
  {wxStyledTextCtrl_StartRecord, "wxStyledTextCtrl", "startRecord", 1}, // 3306
  {wxStyledTextCtrl_StopRecord, "wxStyledTextCtrl", "stopRecord", 1}, // 3307
  {wxStyledTextCtrl_SetLexer, "wxStyledTextCtrl", "setLexer", 2}, // 3308
  {wxStyledTextCtrl_GetLexer, "wxStyledTextCtrl", "getLexer", 1}, // 3309
  {wxStyledTextCtrl_Colourise, "wxStyledTextCtrl", "colourise", 3}, // 3310
  {wxStyledTextCtrl_SetProperty, "wxStyledTextCtrl", "setProperty", 3}, // 3311
  {wxStyledTextCtrl_SetKeyWords, "wxStyledTextCtrl", "setKeyWords", 3}, // 3312
  {wxStyledTextCtrl_SetLexerLanguage, "wxStyledTextCtrl", "setLexerLanguage", 2}, // 3313
  {wxStyledTextCtrl_GetProperty, "wxStyledTextCtrl", "getProperty", 2}, // 3314
  {wxStyledTextCtrl_GetStyleBitsNeeded, "wxStyledTextCtrl", "getStyleBitsNeeded", 1}, // 3315
  {wxStyledTextCtrl_GetCurrentLine, "wxStyledTextCtrl", "getCurrentLine", 1}, // 3316
  {wxStyledTextCtrl_StyleSetSpec, "wxStyledTextCtrl", "styleSetSpec", 3}, // 3317
  {wxStyledTextCtrl_StyleSetFont, "wxStyledTextCtrl", "styleSetFont", 3}, // 3318
  {wxStyledTextCtrl_StyleSetFontAttr, "wxStyledTextCtrl", "styleSetFontAttr", 8}, // 3319
  {wxStyledTextCtrl_StyleSetCharacterSet, "wxStyledTextCtrl", "styleSetCharacterSet", 3}, // 3320
  {wxStyledTextCtrl_StyleSetFontEncoding, "wxStyledTextCtrl", "styleSetFontEncoding", 3}, // 3321
  {wxStyledTextCtrl_CmdKeyExecute, "wxStyledTextCtrl", "cmdKeyExecute", 2}, // 3322
  {wxStyledTextCtrl_SetMargins, "wxStyledTextCtrl", "setMargins", 3}, // 3323
  {wxStyledTextCtrl_GetSelection, "wxStyledTextCtrl", "getSelection", 1}, // 3324
  {wxStyledTextCtrl_PointFromPosition, "wxStyledTextCtrl", "pointFromPosition", 2}, // 3325
  {wxStyledTextCtrl_ScrollToLine, "wxStyledTextCtrl", "scrollToLine", 2}, // 3326
  {wxStyledTextCtrl_ScrollToColumn, "wxStyledTextCtrl", "scrollToColumn", 2}, // 3327
  {wxStyledTextCtrl_SetVScrollBar, "wxStyledTextCtrl", "setVScrollBar", 2}, // 3328
  {wxStyledTextCtrl_SetHScrollBar, "wxStyledTextCtrl", "setHScrollBar", 2}, // 3329
  {wxStyledTextCtrl_GetLastKeydownProcessed, "wxStyledTextCtrl", "getLastKeydownProcessed", 1}, // 3330
  {wxStyledTextCtrl_SetLastKeydownProcessed, "wxStyledTextCtrl", "setLastKeydownProcessed", 2}, // 3331
  {wxStyledTextCtrl_SaveFile, "wxStyledTextCtrl", "saveFile", 2}, // 3332
  {wxStyledTextCtrl_LoadFile, "wxStyledTextCtrl", "loadFile", 2}, // 3333
  {wxStyledTextCtrl_DoDragOver, "wxStyledTextCtrl", "doDragOver", 4}, // 3334
  {wxStyledTextCtrl_DoDropText, "wxStyledTextCtrl", "doDropText", 4}, // 3335
  {wxStyledTextCtrl_GetUseAntiAliasing, "wxStyledTextCtrl", "getUseAntiAliasing", 1}, // 3336
  {wxStyledTextCtrl_AddTextRaw, "wxStyledTextCtrl", "addTextRaw", 3}, // 3337
  {wxStyledTextCtrl_InsertTextRaw, "wxStyledTextCtrl", "insertTextRaw", 3}, // 3338
  {wxStyledTextCtrl_GetCurLineRaw, "wxStyledTextCtrl", "getCurLineRaw", 1}, // 3339
  {wxStyledTextCtrl_GetLineRaw, "wxStyledTextCtrl", "getLineRaw", 2}, // 3340
  {wxStyledTextCtrl_GetSelectedTextRaw, "wxStyledTextCtrl", "getSelectedTextRaw", 1}, // 3341
  {wxStyledTextCtrl_GetTextRangeRaw, "wxStyledTextCtrl", "getTextRangeRaw", 3}, // 3342
  {wxStyledTextCtrl_SetTextRaw, "wxStyledTextCtrl", "setTextRaw", 2}, // 3343
  {wxStyledTextCtrl_GetTextRaw, "wxStyledTextCtrl", "getTextRaw", 1}, // 3344
  {wxStyledTextCtrl_AppendTextRaw, "wxStyledTextCtrl", "appendTextRaw", 3}, // 3345
  {wxArtProvider_GetBitmap, "wxArtProvider", "getBitmap", 2}, // 3346
  {wxArtProvider_GetIcon, "wxArtProvider", "getIcon", 2}, // 3347
  {wxTreeEvent_GetKeyCode, "wxTreeEvent", "getKeyCode", 1}, // 3348
  {wxTreeEvent_GetItem, "wxTreeEvent", "getItem", 1}, // 3349
  {wxTreeEvent_GetKeyEvent, "wxTreeEvent", "getKeyEvent", 1}, // 3350
  {wxTreeEvent_GetLabel, "wxTreeEvent", "getLabel", 1}, // 3351
  {wxTreeEvent_GetOldItem, "wxTreeEvent", "getOldItem", 1}, // 3352
  {wxTreeEvent_GetPoint, "wxTreeEvent", "getPoint", 1}, // 3353
  {wxTreeEvent_IsEditCancelled, "wxTreeEvent", "isEditCancelled", 1}, // 3354
  {wxTreeEvent_SetToolTip, "wxTreeEvent", "setToolTip", 2}, // 3355
  {wxBookCtrlEvent_GetOldSelection, "wxBookCtrlEvent", "getOldSelection", 1}, // 3356
  {wxBookCtrlEvent_GetSelection, "wxBookCtrlEvent", "getSelection", 1}, // 3357
  {wxBookCtrlEvent_SetOldSelection, "wxBookCtrlEvent", "setOldSelection", 2}, // 3358
  {wxBookCtrlEvent_SetSelection, "wxBookCtrlEvent", "setSelection", 2}, // 3359
  {wxFileDataObject_new, "wxFileDataObject", "new", 0}, // 3360
  {wxFileDataObject_AddFile, "wxFileDataObject", "addFile", 2}, // 3361
  {wxFileDataObject_GetFilenames, "wxFileDataObject", "getFilenames", 1}, // 3362
  {wxFileDataObject_destroy, "wxFileDataObject", "'Destroy'", 1}, // 3363
  {wxTextDataObject_new, "wxTextDataObject", "new", 1}, // 3364
  {wxTextDataObject_GetTextLength, "wxTextDataObject", "getTextLength", 1}, // 3365
  {wxTextDataObject_GetText, "wxTextDataObject", "getText", 1}, // 3366
  {wxTextDataObject_SetText, "wxTextDataObject", "setText", 2}, // 3367
  {wxTextDataObject_destroy, "wxTextDataObject", "'Destroy'", 1}, // 3368
  {wxBitmapDataObject_new_1_1, "wxBitmapDataObject", "new", 1}, // 3369
  {wxBitmapDataObject_new_1_0, "wxBitmapDataObject", "new", 1}, // 3370
  {wxBitmapDataObject_GetBitmap, "wxBitmapDataObject", "getBitmap", 1}, // 3371
  {wxBitmapDataObject_SetBitmap, "wxBitmapDataObject", "setBitmap", 2}, // 3372
  {wxBitmapDataObject_destroy, "wxBitmapDataObject", "'Destroy'", 1}, // 3373
  {wxClipboard_new, "wxClipboard", "new", 0}, // 3374
  {NULL, "wxClipboard", "destroy", 1}, // 3375 obj destructor wxClipboard_destruct
  {wxClipboard_AddData, "wxClipboard", "addData", 2}, // 3376
  {wxClipboard_Clear, "wxClipboard", "clear", 1}, // 3377
  {wxClipboard_Close, "wxClipboard", "close", 1}, // 3378
  {wxClipboard_Flush, "wxClipboard", "flush", 1}, // 3379
  {wxClipboard_GetData, "wxClipboard", "getData", 2}, // 3380
  {wxClipboard_IsOpened, "wxClipboard", "isOpened", 1}, // 3381
  {wxClipboard_Open, "wxClipboard", "open", 1}, // 3382
  {wxClipboard_SetData, "wxClipboard", "setData", 2}, // 3383
  {wxClipboard_UsePrimarySelection, "wxClipboard", "usePrimarySelection", 2}, // 3384
  {wxClipboard_IsSupported, "wxClipboard", "isSupported", 2}, // 3385
  {wxClipboard_Get, "wxClipboard", "get", 0}, // 3386
  {wxSpinEvent_GetPosition, "wxSpinEvent", "getPosition", 1}, // 3387
  {wxSpinEvent_SetPosition, "wxSpinEvent", "setPosition", 2}, // 3388
  {wxSplitterWindow_new_0, "wxSplitterWindow", "new", 0}, // 3389
  {wxSplitterWindow_new_2, "wxSplitterWindow", "new", 2}, // 3390
  {NULL, "wxSplitterWindow", "destroy", 1}, // 3391 obj destructor wxSplitterWindow_destruct
  {wxSplitterWindow_Create, "wxSplitterWindow", "create", 3}, // 3392
  {wxSplitterWindow_GetMinimumPaneSize, "wxSplitterWindow", "getMinimumPaneSize", 1}, // 3393
  {wxSplitterWindow_GetSashGravity, "wxSplitterWindow", "getSashGravity", 1}, // 3394
  {wxSplitterWindow_GetSashPosition, "wxSplitterWindow", "getSashPosition", 1}, // 3395
  {wxSplitterWindow_GetSplitMode, "wxSplitterWindow", "getSplitMode", 1}, // 3396
  {wxSplitterWindow_GetWindow1, "wxSplitterWindow", "getWindow1", 1}, // 3397
  {wxSplitterWindow_GetWindow2, "wxSplitterWindow", "getWindow2", 1}, // 3398
  {wxSplitterWindow_Initialize, "wxSplitterWindow", "initialize", 2}, // 3399
  {wxSplitterWindow_IsSplit, "wxSplitterWindow", "isSplit", 1}, // 3400
  {wxSplitterWindow_ReplaceWindow, "wxSplitterWindow", "replaceWindow", 3}, // 3401
  {wxSplitterWindow_SetSashGravity, "wxSplitterWindow", "setSashGravity", 2}, // 3402
  {wxSplitterWindow_SetSashPosition, "wxSplitterWindow", "setSashPosition", 3}, // 3403
  {wxSplitterWindow_SetMinimumPaneSize, "wxSplitterWindow", "setMinimumPaneSize", 2}, // 3404
  {wxSplitterWindow_SetSplitMode, "wxSplitterWindow", "setSplitMode", 2}, // 3405
  {wxSplitterWindow_SplitHorizontally, "wxSplitterWindow", "splitHorizontally", 4}, // 3406
  {wxSplitterWindow_SplitVertically, "wxSplitterWindow", "splitVertically", 4}, // 3407
  {wxSplitterWindow_Unsplit, "wxSplitterWindow", "unsplit", 2}, // 3408
  {wxSplitterWindow_UpdateSize, "wxSplitterWindow", "updateSize", 1}, // 3409
  {wxSplitterEvent_GetSashPosition, "wxSplitterEvent", "getSashPosition", 1}, // 3410
  {wxSplitterEvent_GetX, "wxSplitterEvent", "getX", 1}, // 3411
  {wxSplitterEvent_GetY, "wxSplitterEvent", "getY", 1}, // 3412
  {wxSplitterEvent_GetWindowBeingRemoved, "wxSplitterEvent", "getWindowBeingRemoved", 1}, // 3413
  {wxSplitterEvent_SetSashPosition, "wxSplitterEvent", "setSashPosition", 2}, // 3414
  {wxHtmlWindow_new_0, "wxHtmlWindow", "new", 0}, // 3415
  {wxHtmlWindow_new_2, "wxHtmlWindow", "new", 2}, // 3416
  {wxHtmlWindow_AppendToPage, "wxHtmlWindow", "appendToPage", 2}, // 3417
  {wxHtmlWindow_GetOpenedAnchor, "wxHtmlWindow", "getOpenedAnchor", 1}, // 3418
  {wxHtmlWindow_GetOpenedPage, "wxHtmlWindow", "getOpenedPage", 1}, // 3419
  {wxHtmlWindow_GetOpenedPageTitle, "wxHtmlWindow", "getOpenedPageTitle", 1}, // 3420
  {wxHtmlWindow_GetRelatedFrame, "wxHtmlWindow", "getRelatedFrame", 1}, // 3421
  {wxHtmlWindow_HistoryBack, "wxHtmlWindow", "historyBack", 1}, // 3422
  {wxHtmlWindow_HistoryCanBack, "wxHtmlWindow", "historyCanBack", 1}, // 3423
  {wxHtmlWindow_HistoryCanForward, "wxHtmlWindow", "historyCanForward", 1}, // 3424
  {wxHtmlWindow_HistoryClear, "wxHtmlWindow", "historyClear", 1}, // 3425
  {wxHtmlWindow_HistoryForward, "wxHtmlWindow", "historyForward", 1}, // 3426
  {wxHtmlWindow_LoadFile, "wxHtmlWindow", "loadFile", 2}, // 3427
  {wxHtmlWindow_LoadPage, "wxHtmlWindow", "loadPage", 2}, // 3428
  {wxHtmlWindow_SelectAll, "wxHtmlWindow", "selectAll", 1}, // 3429
  {wxHtmlWindow_SelectionToText, "wxHtmlWindow", "selectionToText", 1}, // 3430
  {wxHtmlWindow_SelectLine, "wxHtmlWindow", "selectLine", 2}, // 3431
  {wxHtmlWindow_SelectWord, "wxHtmlWindow", "selectWord", 2}, // 3432
  {wxHtmlWindow_SetBorders, "wxHtmlWindow", "setBorders", 2}, // 3433
  {wxHtmlWindow_SetFonts, "wxHtmlWindow", "setFonts", 4}, // 3434
  {wxHtmlWindow_SetPage, "wxHtmlWindow", "setPage", 2}, // 3435
  {wxHtmlWindow_SetRelatedFrame, "wxHtmlWindow", "setRelatedFrame", 3}, // 3436
  {wxHtmlWindow_SetRelatedStatusBar_1, "wxHtmlWindow", "setRelatedStatusBar", 2}, // 3437
  {wxHtmlWindow_SetRelatedStatusBar_2, "wxHtmlWindow", "setRelatedStatusBar", 3}, // 3438
  {wxHtmlWindow_ToText, "wxHtmlWindow", "toText", 1}, // 3439
  {NULL, "wxHtmlWindow", "'Destroy'", 1}, // 3440 obj destructor wxHtmlWindow_destroy
  {wxHtmlLinkEvent_GetLinkInfo, "wxHtmlLinkEvent", "getLinkInfo", 1}, // 3441
  {wxSystemSettings_GetColour, "wxSystemSettings", "getColour", 1}, // 3442
  {wxSystemSettings_GetFont, "wxSystemSettings", "getFont", 1}, // 3443
  {wxSystemSettings_GetMetric, "wxSystemSettings", "getMetric", 2}, // 3444
  {wxSystemSettings_GetScreenType, "wxSystemSettings", "getScreenType", 0}, // 3445
  {wxSystemOptions_GetOption, "wxSystemOptions", "getOption", 1}, // 3446
  {wxSystemOptions_GetOptionInt, "wxSystemOptions", "getOptionInt", 1}, // 3447
  {wxSystemOptions_HasOption, "wxSystemOptions", "hasOption", 1}, // 3448
  {wxSystemOptions_IsFalse, "wxSystemOptions", "isFalse", 1}, // 3449
  {wxSystemOptions_SetOption_2_1, "wxSystemOptions", "setOption", 2}, // 3450
  {wxSystemOptions_SetOption_2_0, "wxSystemOptions", "setOption", 2}, // 3451
  {wxAuiNotebookEvent_SetSelection, "wxAuiNotebookEvent", "setSelection", 2}, // 3452
  {wxAuiNotebookEvent_GetSelection, "wxAuiNotebookEvent", "getSelection", 1}, // 3453
  {wxAuiNotebookEvent_SetOldSelection, "wxAuiNotebookEvent", "setOldSelection", 2}, // 3454
  {wxAuiNotebookEvent_GetOldSelection, "wxAuiNotebookEvent", "getOldSelection", 1}, // 3455
  {wxAuiNotebookEvent_SetDragSource, "wxAuiNotebookEvent", "setDragSource", 2}, // 3456
  {wxAuiNotebookEvent_GetDragSource, "wxAuiNotebookEvent", "getDragSource", 1}, // 3457
  {wxAuiManagerEvent_SetManager, "wxAuiManagerEvent", "setManager", 2}, // 3458
  {wxAuiManagerEvent_GetManager, "wxAuiManagerEvent", "getManager", 1}, // 3459
  {wxAuiManagerEvent_SetPane, "wxAuiManagerEvent", "setPane", 2}, // 3460
  {wxAuiManagerEvent_GetPane, "wxAuiManagerEvent", "getPane", 1}, // 3461
  {wxAuiManagerEvent_SetButton, "wxAuiManagerEvent", "setButton", 2}, // 3462
  {wxAuiManagerEvent_GetButton, "wxAuiManagerEvent", "getButton", 1}, // 3463
  {wxAuiManagerEvent_SetDC, "wxAuiManagerEvent", "setDC", 2}, // 3464
  {wxAuiManagerEvent_GetDC, "wxAuiManagerEvent", "getDC", 1}, // 3465
  {wxAuiManagerEvent_Veto, "wxAuiManagerEvent", "veto", 2}, // 3466
  {wxAuiManagerEvent_GetVeto, "wxAuiManagerEvent", "getVeto", 1}, // 3467
  {wxAuiManagerEvent_SetCanVeto, "wxAuiManagerEvent", "setCanVeto", 2}, // 3468
  {wxAuiManagerEvent_CanVeto, "wxAuiManagerEvent", "canVeto", 1}, // 3469
  {wxLogNull_new, "wxLogNull", "new", 0}, // 3470
  {wxLogNull_destruct, "wxLogNull", "destroy", 1}, // 3471
  {wxTaskBarIcon_new, "wxTaskBarIcon", "new", 1}, // 3472
  {NULL, "wxTaskBarIcon", "destroy", 1}, // 3473 obj destructor wxTaskBarIcon_destruct
  {wxTaskBarIcon_PopupMenu, "wxTaskBarIcon", "popupMenu", 2}, // 3474
  {wxTaskBarIcon_RemoveIcon, "wxTaskBarIcon", "removeIcon", 1}, // 3475
  {wxTaskBarIcon_SetIcon, "wxTaskBarIcon", "setIcon", 3}, // 3476
  {wxLocale_new_0, "wxLocale", "new", 0}, // 3477
  {wxLocale_new_2_0, "wxLocale", "new", 2}, // 3478
  {wxLocale_new_2_1, "wxLocale", "new", 2}, // 3479
  {wxLocale_destruct, "wxLocale", "destroy", 1}, // 3480
  {wxLocale_Init_1, "wxLocale", "init", 2}, // 3481
  {wxLocale_Init_2, "wxLocale", "init", 3}, // 3482
  {wxLocale_AddCatalog_1, "wxLocale", "addCatalog", 2}, // 3483
  {wxLocale_AddCatalog_2, "wxLocale", "addCatalog", 3}, // 3484
  {wxLocale_AddCatalog_3, "wxLocale", "addCatalog", 4}, // 3485
  {wxLocale_AddCatalogLookupPathPrefix, "wxLocale", "addCatalogLookupPathPrefix", 1}, // 3486
  {wxLocale_GetCanonicalName, "wxLocale", "getCanonicalName", 1}, // 3487
  {wxLocale_GetLanguage, "wxLocale", "getLanguage", 1}, // 3488
  {wxLocale_GetLanguageName, "wxLocale", "getLanguageName", 1}, // 3489
  {wxLocale_GetLocale, "wxLocale", "getLocale", 1}, // 3490
  {wxLocale_GetName, "wxLocale", "getName", 1}, // 3491
  {wxLocale_GetString_2, "wxLocale", "getString", 3}, // 3492
  {wxLocale_GetString_4, "wxLocale", "getString", 5}, // 3493
  {wxLocale_GetHeaderValue, "wxLocale", "getHeaderValue", 3}, // 3494
  {wxLocale_GetSysName, "wxLocale", "getSysName", 1}, // 3495
  {wxLocale_GetSystemEncoding, "wxLocale", "getSystemEncoding", 0}, // 3496
  {wxLocale_GetSystemEncodingName, "wxLocale", "getSystemEncodingName", 0}, // 3497
  {wxLocale_GetSystemLanguage, "wxLocale", "getSystemLanguage", 0}, // 3498
  {wxLocale_IsLoaded, "wxLocale", "isLoaded", 2}, // 3499
  {wxLocale_IsOk, "wxLocale", "isOk", 1}, // 3500
  {wxActivateEvent_GetActive, "wxActivateEvent", "getActive", 1}, // 3501
  {wxPopupWindow_new_0, "wxPopupWindow", "new", 0}, // 3502
  {wxPopupWindow_new_2, "wxPopupWindow", "new", 2}, // 3503
  {wxPopupWindow_Create, "wxPopupWindow", "create", 3}, // 3504
  {wxPopupWindow_Position, "wxPopupWindow", "position", 3}, // 3505
  {NULL, "wxPopupWindow", "'Destroy'", 1}, // 3506 obj destructor wxPopupWindow_destroy
  {wxPopupTransientWindow_new_0, "wxPopupTransientWindow", "new", 0}, // 3507
  {wxPopupTransientWindow_new_2, "wxPopupTransientWindow", "new", 2}, // 3508
  {wxPopupTransientWindow_Popup, "wxPopupTransientWindow", "popup", 2}, // 3509
  {wxPopupTransientWindow_Dismiss, "wxPopupTransientWindow", "dismiss", 1}, // 3510
  {NULL, "wxPopupTransientWindow", "'Destroy'", 1}, // 3511 obj destructor wxPopupTransientWindow_destroy
  {wxOverlay_new, "wxOverlay", "new", 0}, // 3512
  {wxOverlay_destruct, "wxOverlay", "destroy", 1}, // 3513
  {wxOverlay_Reset, "wxOverlay", "reset", 1}, // 3514
  {wxDCOverlay_new_6, "wxDCOverlay", "new", 6}, // 3515
  {wxDCOverlay_new_2, "wxDCOverlay", "new", 2}, // 3516
  {wxDCOverlay_destruct, "wxDCOverlay", "destroy", 1}, // 3517
  {wxDCOverlay_Clear, "wxDCOverlay", "clear", 1}, // 3518
  {wxDropFilesEvent_GetPosition, "wxDropFilesEvent", "getPosition", 1}, // 3519
  {wxDropFilesEvent_GetNumberOfFiles, "wxDropFilesEvent", "getNumberOfFiles", 1}, // 3520
  {wxDropFilesEvent_GetFiles, "wxDropFilesEvent", "getFiles", 1}, // 3521
  {wxDisplay_new_0, "wxDisplay", "new", 0}, // 3522
  {wxDisplay_new_1_0, "wxDisplay", "new", 1}, // 3523
#if wxCHECK_VERSION(3,1,3)
  {wxDisplay_new_1_1, "wxDisplay", "new", 1}, // 3524
#else
  {NULL, "wxDisplay", "new", 0}, // 3524
#endif
  {wxDisplay_destruct, "wxDisplay", "destroy", 1}, // 3525
  {wxDisplay_IsOk, "wxDisplay", "isOk", 1}, // 3526
  {wxDisplay_GetClientArea, "wxDisplay", "getClientArea", 1}, // 3527
  {wxDisplay_GetGeometry, "wxDisplay", "getGeometry", 1}, // 3528
  {wxDisplay_GetName, "wxDisplay", "getName", 1}, // 3529
  {wxDisplay_IsPrimary, "wxDisplay", "isPrimary", 1}, // 3530
  {wxDisplay_GetCount, "wxDisplay", "getCount", 0}, // 3531
  {wxDisplay_GetFromPoint, "wxDisplay", "getFromPoint", 1}, // 3532
  {wxDisplay_GetFromWindow, "wxDisplay", "getFromWindow", 1}, // 3533
#if wxCHECK_VERSION(3,1,2)
  {wxDisplay_GetPPI, "wxDisplay", "getPPI", 1}, // 3534
#else
  {NULL, "wxDisplay", "getPPI", 0}, // 3534
#endif
  {wxGCDC_new_1, "wxGCDC", "new", 1}, // 3535
  {NULL, "", "", 0}, // 3536
  {NULL, "", "", 0}, // 3537
  {wxGCDC_new_0, "wxGCDC", "new", 0}, // 3538
  {NULL, "wxGCDC", "destroy", 1}, // 3539 obj destructor wxGCDC_destruct
  {wxGCDC_GetGraphicsContext, "wxGCDC", "getGraphicsContext", 1}, // 3540
  {wxGCDC_SetGraphicsContext, "wxGCDC", "setGraphicsContext", 2}, // 3541
};
