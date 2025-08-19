%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxFileDialog).
-moduledoc """
This class represents the file chooser dialog.

The path and filename are distinct elements of a full file pathname. If path is
?wxEmptyString, the current directory will be used. If filename is ?wxEmptyString, no
default filename will be supplied. The wildcard determines what files are displayed in the
file selector, and file extension supplies a type extension for the required filename.

The typical usage for the open file dialog is:

The typical usage for the save file dialog is instead somewhat simpler:

Remark: All implementations of the `m:wxFileDialog` provide a wildcard filter. Typing a
filename containing wildcards (*, ?) in the filename text item, and clicking on Ok, will
result in only those files matching the pattern being displayed. The wildcard may be a
specification for multiple types of file with a description for each, such as: It must be
noted that wildcard support in the native Motif file dialog is quite limited: only one
file type is supported, and it is displayed without the descriptive test; "BMP files
(*.bmp)|*.bmp" is displayed as "*.bmp", and both "BMP files (*.bmp)|*.bmp|GIF files
(*.gif)|*.gif" and "Image files|*.bmp;*.gif" are errors. On Mac macOS in the open file
dialog the filter choice box is not shown by default. Instead all given wildcards are
appplied at the same time: So in the above example all bmp, gif and png files are
displayed. To enforce the display of the filter choice set the corresponding `m:wxSystemOptions`
before calling the file open dialog: But in contrast to Windows and Unix, where the file
type choice filters only the selected files, on Mac macOS even in this case the dialog
shows all files matching all file types. The files which does not match the currently
selected file type are greyed out and are not selectable.

## Styles

This class supports the following styles:

* wxFD_DEFAULT_STYLE: Equivalent to `wxFD_OPEN`.

* wxFD_OPEN: This is an open dialog; usually this means that the default button's label of
the dialog is "Open". Cannot be combined with `wxFD_SAVE`.

* wxFD_SAVE: This is a save dialog; usually this means that the default button's label of
the dialog is "Save". Cannot be combined with `wxFD_OPEN`.

* wxFD_OVERWRITE_PROMPT: For save dialog only: prompt for a confirmation if a file will be
overwritten.

* wxFD_NO_FOLLOW: Directs the dialog to return the path and file name of the selected
shortcut file, not its target as it does by default. Currently this flag is only
implemented in wxMSW and wxOSX (where it prevents aliases from being resolved). The
non-dereferenced link path is always returned, even without this flag, under Unix and so
using it there doesn't do anything. This flag was added in wxWidgets 3.1.0.

* wxFD_FILE_MUST_EXIST: For open dialog only: the user may only select files that actually
exist. Notice that under macOS the file dialog with `wxFD_OPEN` style always behaves as if
this style was specified, because it is impossible to choose a file that doesn't exist
from a standard macOS file dialog.

* wxFD_MULTIPLE: For open dialog only: allows selecting multiple files.

* wxFD_CHANGE_DIR: Change the current working directory (when the dialog is dismissed) to
the directory where the file(s) chosen by the user are.

* wxFD_PREVIEW: Show the preview of the selected files (currently only supported by wxGTK).

* wxFD_SHOW_HIDDEN: Show hidden files. This flag was added in wxWidgets 3.1.3

See:
* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_file)

* ?wxFileSelector()

This class is derived, and can use functions, from:

* `m:wxDialog`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxFileDialog](https://docs.wxwidgets.org/3.2/classwx_file_dialog.html)
""".
-include("wxe.hrl").
-export([destroy/1,getDirectory/1,getFilename/1,getFilenames/1,getFilterIndex/1,
  getMessage/1,getPath/1,getPaths/1,getWildcard/1,new/1,new/2,setDirectory/2,
  setFilename/2,setFilterIndex/2,setMessage/2,setPath/2,setWildcard/2]).

%% inherited exports
-export([cacheBestSize/2,canSetTransparent/1,captureMouse/1,center/1,center/2,
  centerOnParent/1,centerOnParent/2,centerOnScreen/1,centerOnScreen/2,
  centre/1,centre/2,centreOnParent/1,centreOnParent/2,centreOnScreen/1,
  centreOnScreen/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  createButtonSizer/2,createStdDialogButtonSizer/2,destroyChildren/1,
  disable/1,disconnect/1,disconnect/2,disconnect/3,dragAcceptFiles/2,
  enable/1,enable/2,endModal/2,findWindow/2,fit/1,fitInside/1,freeze/1,
  getAcceleratorTable/1,getAffirmativeId/1,getBackgroundColour/1,getBackgroundStyle/1,
  getBestSize/1,getCaret/1,getCharHeight/1,getCharWidth/1,getChildren/1,
  getClientSize/1,getContainingSizer/1,getContentScaleFactor/1,getCursor/1,
  getDPI/1,getDPIScaleFactor/1,getDropTarget/1,getExtraStyle/1,getFont/1,
  getForegroundColour/1,getGrandParent/1,getHandle/1,getHelpText/1,
  getIcon/1,getIcons/1,getId/1,getLabel/1,getMaxSize/1,getMinSize/1,getName/1,
  getParent/1,getPosition/1,getRect/1,getReturnCode/1,getScreenPosition/1,
  getScreenRect/1,getScrollPos/2,getScrollRange/2,getScrollThumb/2,
  getSize/1,getSizer/1,getTextExtent/2,getTextExtent/3,getThemeEnabled/1,
  getTitle/1,getToolTip/1,getUpdateRegion/1,getVirtualSize/1,getWindowStyleFlag/1,
  getWindowVariant/1,hasCapture/1,hasScrollbar/2,hasTransparentBackground/1,
  hide/1,iconize/1,iconize/2,inheritAttributes/1,initDialog/1,invalidateBestSize/1,
  isActive/1,isDoubleBuffered/1,isEnabled/1,isExposed/2,isExposed/3,
  isExposed/5,isFrozen/1,isFullScreen/1,isIconized/1,isMaximized/1,isModal/1,
  isRetained/1,isShown/1,isShownOnScreen/1,isTopLevel/1,layout/1,lineDown/1,
  lineUp/1,lower/1,maximize/1,maximize/2,move/2,move/3,move/4,moveAfterInTabOrder/2,
  moveBeforeInTabOrder/2,navigate/1,navigate/2,pageDown/1,pageUp/1,parent_class/1,
  popupMenu/2,popupMenu/3,popupMenu/4,raise/1,refresh/1,refresh/2,refreshRect/2,
  refreshRect/3,releaseMouse/1,removeChild/2,reparent/2,requestUserAttention/1,
  requestUserAttention/2,screenToClient/1,screenToClient/2,scrollLines/2,
  scrollPages/2,scrollWindow/3,scrollWindow/4,setAcceleratorTable/2,
  setAffirmativeId/2,setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,
  setCaret/2,setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,
  setFont/2,setForegroundColour/2,setHelpText/2,setIcon/2,setIcons/2,
  setId/2,setLabel/2,setMaxSize/2,setMinSize/2,setName/2,setOwnBackgroundColour/2,
  setOwnFont/2,setOwnForegroundColour/2,setPalette/2,setReturnCode/2,
  setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,setShape/2,
  setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,
  setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,
  setThemeEnabled/2,setTitle/2,setToolTip/2,setTransparent/2,setVirtualSize/2,
  setVirtualSize/3,setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,
  shouldInheritColours/1,show/1,show/2,showFullScreen/2,showFullScreen/3,
  showModal/1,thaw/1,transferDataFromWindow/1,transferDataToWindow/1,
  update/1,updateWindowUI/1,updateWindowUI/2,validate/1,warpPointer/3]).

-type wxFileDialog() :: wx:wx_object().
-export_type([wxFileDialog/0]).
-doc false.
parent_class(wxDialog) -> true;
parent_class(wxTopLevelWindow) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => new(Parent, [])}).
-spec new(Parent) -> wxFileDialog() when
	Parent::wxWindow:wxWindow().

new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

-doc """
Constructor.

Use `wxDialog:showModal/1` to show the dialog.
""".
-spec new(Parent, [Option]) -> wxFileDialog() when
	Parent::wxWindow:wxWindow(),
	Option :: {'message', unicode:chardata()}
		 | {'defaultDir', unicode:chardata()}
		 | {'defaultFile', unicode:chardata()}
		 | {'wildCard', unicode:chardata()}
		 | {'style', integer()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'sz', {W::integer(), H::integer()}}.
new(#wx_ref{type=ParentT}=Parent, Options)
 when is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({message, Message}) ->   Message_UC = unicode:characters_to_binary(Message),{message,Message_UC};
          ({defaultDir, DefaultDir}) ->   DefaultDir_UC = unicode:characters_to_binary(DefaultDir),{defaultDir,DefaultDir_UC};
          ({defaultFile, DefaultFile}) ->   DefaultFile_UC = unicode:characters_to_binary(DefaultFile),{defaultFile,DefaultFile_UC};
          ({wildCard, WildCard}) ->   WildCard_UC = unicode:characters_to_binary(WildCard),{wildCard,WildCard_UC};
          ({style, _style} = Arg) -> Arg;
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({sz, {_szW,_szH}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent, Opts,?get_env(),?wxFileDialog_new),
  wxe_util:rec(?wxFileDialog_new).

-doc "Returns the default directory.".
-spec getDirectory(This) -> unicode:charlist() when
	This::wxFileDialog().
getDirectory(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFileDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxFileDialog_GetDirectory),
  wxe_util:rec(?wxFileDialog_GetDirectory).

-doc """
Returns the default filename.

Note: This function can't be used with dialogs which have the `wxFD_MULTIPLE` style, use `getFilenames/1`
instead.
""".
-spec getFilename(This) -> unicode:charlist() when
	This::wxFileDialog().
getFilename(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFileDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxFileDialog_GetFilename),
  wxe_util:rec(?wxFileDialog_GetFilename).

-doc """
Fills the array `filenames` with the names of the files chosen.

This function should only be used with the dialogs which have `wxFD_MULTIPLE` style, use `getFilename/1`
for the others.

Note that under Windows, if the user selects shortcuts, the filenames include paths,
since the application cannot determine the full path of each referenced file by appending
the directory containing the shortcuts to the filename.
""".
-spec getFilenames(This) -> [unicode:charlist()] when
	This::wxFileDialog().
getFilenames(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFileDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxFileDialog_GetFilenames),
  wxe_util:rec(?wxFileDialog_GetFilenames).

-doc """
Returns the index into the list of filters supplied, optionally, in the wildcard
parameter.

Before the dialog is shown, this is the index which will be used when the dialog is first displayed.

After the dialog is shown, this is the index selected by the user.
""".
-spec getFilterIndex(This) -> integer() when
	This::wxFileDialog().
getFilterIndex(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFileDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxFileDialog_GetFilterIndex),
  wxe_util:rec(?wxFileDialog_GetFilterIndex).

-doc "Returns the message that will be displayed on the dialog.".
-spec getMessage(This) -> unicode:charlist() when
	This::wxFileDialog().
getMessage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFileDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxFileDialog_GetMessage),
  wxe_util:rec(?wxFileDialog_GetMessage).

-doc """
Returns the full path (directory and filename) of the selected file.

Note: This function can't be used with dialogs which have the `wxFD_MULTIPLE` style, use `getPaths/1`
instead.
""".
-spec getPath(This) -> unicode:charlist() when
	This::wxFileDialog().
getPath(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFileDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxFileDialog_GetPath),
  wxe_util:rec(?wxFileDialog_GetPath).

-doc """
Fills the array `paths` with the full paths of the files chosen.

This function should only be used with the dialogs which have `wxFD_MULTIPLE` style, use `getPath/1`
for the others.
""".
-spec getPaths(This) -> [unicode:charlist()] when
	This::wxFileDialog().
getPaths(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFileDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxFileDialog_GetPaths),
  wxe_util:rec(?wxFileDialog_GetPaths).

-doc "Returns the file dialog wildcard.".
-spec getWildcard(This) -> unicode:charlist() when
	This::wxFileDialog().
getWildcard(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFileDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxFileDialog_GetWildcard),
  wxe_util:rec(?wxFileDialog_GetWildcard).

-doc "Sets the default directory.".
-spec setDirectory(This, Directory) -> 'ok' when
	This::wxFileDialog(), Directory::unicode:chardata().
setDirectory(#wx_ref{type=ThisT}=This,Directory)
 when ?is_chardata(Directory) ->
  ?CLASS(ThisT,wxFileDialog),
  Directory_UC = unicode:characters_to_binary(Directory),
  wxe_util:queue_cmd(This,Directory_UC,?get_env(),?wxFileDialog_SetDirectory).

-doc """
Sets the default filename.

In wxGTK this will have little effect unless a default directory has previously been set.
""".
-spec setFilename(This, Setfilename) -> 'ok' when
	This::wxFileDialog(), Setfilename::unicode:chardata().
setFilename(#wx_ref{type=ThisT}=This,Setfilename)
 when ?is_chardata(Setfilename) ->
  ?CLASS(ThisT,wxFileDialog),
  Setfilename_UC = unicode:characters_to_binary(Setfilename),
  wxe_util:queue_cmd(This,Setfilename_UC,?get_env(),?wxFileDialog_SetFilename).

-doc "Sets the default filter index, starting from zero.".
-spec setFilterIndex(This, FilterIndex) -> 'ok' when
	This::wxFileDialog(), FilterIndex::integer().
setFilterIndex(#wx_ref{type=ThisT}=This,FilterIndex)
 when is_integer(FilterIndex) ->
  ?CLASS(ThisT,wxFileDialog),
  wxe_util:queue_cmd(This,FilterIndex,?get_env(),?wxFileDialog_SetFilterIndex).

-doc "Sets the message that will be displayed on the dialog.".
-spec setMessage(This, Message) -> 'ok' when
	This::wxFileDialog(), Message::unicode:chardata().
setMessage(#wx_ref{type=ThisT}=This,Message)
 when ?is_chardata(Message) ->
  ?CLASS(ThisT,wxFileDialog),
  Message_UC = unicode:characters_to_binary(Message),
  wxe_util:queue_cmd(This,Message_UC,?get_env(),?wxFileDialog_SetMessage).

-doc """
Sets the path (the combined directory and filename that will be returned when the dialog
is dismissed).
""".
-spec setPath(This, Path) -> 'ok' when
	This::wxFileDialog(), Path::unicode:chardata().
setPath(#wx_ref{type=ThisT}=This,Path)
 when ?is_chardata(Path) ->
  ?CLASS(ThisT,wxFileDialog),
  Path_UC = unicode:characters_to_binary(Path),
  wxe_util:queue_cmd(This,Path_UC,?get_env(),?wxFileDialog_SetPath).

-doc """
Sets the wildcard, which can contain multiple file types, for example: "BMP files
(\*.bmp)|\*.bmp|GIF files (\*.gif)|\*.gif".

Note that the native Motif dialog has some limitations with respect to wildcards; see the
Remarks section above.
""".
-spec setWildcard(This, WildCard) -> 'ok' when
	This::wxFileDialog(), WildCard::unicode:chardata().
setWildcard(#wx_ref{type=ThisT}=This,WildCard)
 when ?is_chardata(WildCard) ->
  ?CLASS(ThisT,wxFileDialog),
  WildCard_UC = unicode:characters_to_binary(WildCard),
  wxe_util:queue_cmd(This,WildCard_UC,?get_env(),?wxFileDialog_SetWildcard).

-doc "Destroys the object".
-spec destroy(This::wxFileDialog()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFileDialog),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxDialog
-doc false.
showModal(This) -> wxDialog:showModal(This).
-doc false.
show(This, Options) -> wxDialog:show(This, Options).
-doc false.
show(This) -> wxDialog:show(This).
-doc false.
setReturnCode(This,RetCode) -> wxDialog:setReturnCode(This,RetCode).
-doc false.
setAffirmativeId(This,Id) -> wxDialog:setAffirmativeId(This,Id).
-doc false.
isModal(This) -> wxDialog:isModal(This).
-doc false.
getReturnCode(This) -> wxDialog:getReturnCode(This).
-doc false.
getAffirmativeId(This) -> wxDialog:getAffirmativeId(This).
-doc false.
endModal(This,RetCode) -> wxDialog:endModal(This,RetCode).
-doc false.
createStdDialogButtonSizer(This,Flags) -> wxDialog:createStdDialogButtonSizer(This,Flags).
-doc false.
createButtonSizer(This,Flags) -> wxDialog:createButtonSizer(This,Flags).
 %% From wxTopLevelWindow
-doc false.
showFullScreen(This,Show, Options) -> wxTopLevelWindow:showFullScreen(This,Show, Options).
-doc false.
showFullScreen(This,Show) -> wxTopLevelWindow:showFullScreen(This,Show).
-doc false.
setTitle(This,Title) -> wxTopLevelWindow:setTitle(This,Title).
-doc false.
setShape(This,Region) -> wxTopLevelWindow:setShape(This,Region).
-doc false.
centreOnScreen(This, Options) -> wxTopLevelWindow:centreOnScreen(This, Options).
-doc false.
centerOnScreen(This, Options) -> wxTopLevelWindow:centerOnScreen(This, Options).
-doc false.
centreOnScreen(This) -> wxTopLevelWindow:centreOnScreen(This).
-doc false.
centerOnScreen(This) -> wxTopLevelWindow:centerOnScreen(This).
-doc false.
setIcons(This,Icons) -> wxTopLevelWindow:setIcons(This,Icons).
-doc false.
setIcon(This,Icon) -> wxTopLevelWindow:setIcon(This,Icon).
-doc false.
requestUserAttention(This, Options) -> wxTopLevelWindow:requestUserAttention(This, Options).
-doc false.
requestUserAttention(This) -> wxTopLevelWindow:requestUserAttention(This).
-doc false.
maximize(This, Options) -> wxTopLevelWindow:maximize(This, Options).
-doc false.
maximize(This) -> wxTopLevelWindow:maximize(This).
-doc false.
isMaximized(This) -> wxTopLevelWindow:isMaximized(This).
-doc false.
isIconized(This) -> wxTopLevelWindow:isIconized(This).
-doc false.
isFullScreen(This) -> wxTopLevelWindow:isFullScreen(This).
-doc false.
iconize(This, Options) -> wxTopLevelWindow:iconize(This, Options).
-doc false.
iconize(This) -> wxTopLevelWindow:iconize(This).
-doc false.
isActive(This) -> wxTopLevelWindow:isActive(This).
-doc false.
getTitle(This) -> wxTopLevelWindow:getTitle(This).
-doc false.
getIcons(This) -> wxTopLevelWindow:getIcons(This).
-doc false.
getIcon(This) -> wxTopLevelWindow:getIcon(This).
 %% From wxWindow
-doc false.
getDPI(This) -> wxWindow:getDPI(This).
-doc false.
getContentScaleFactor(This) -> wxWindow:getContentScaleFactor(This).
-doc false.
setDoubleBuffered(This,On) -> wxWindow:setDoubleBuffered(This,On).
-doc false.
isDoubleBuffered(This) -> wxWindow:isDoubleBuffered(This).
-doc false.
canSetTransparent(This) -> wxWindow:canSetTransparent(This).
-doc false.
setTransparent(This,Alpha) -> wxWindow:setTransparent(This,Alpha).
-doc false.
warpPointer(This,X,Y) -> wxWindow:warpPointer(This,X,Y).
-doc false.
validate(This) -> wxWindow:validate(This).
-doc false.
updateWindowUI(This, Options) -> wxWindow:updateWindowUI(This, Options).
-doc false.
updateWindowUI(This) -> wxWindow:updateWindowUI(This).
-doc false.
update(This) -> wxWindow:update(This).
-doc false.
transferDataToWindow(This) -> wxWindow:transferDataToWindow(This).
-doc false.
transferDataFromWindow(This) -> wxWindow:transferDataFromWindow(This).
-doc false.
thaw(This) -> wxWindow:thaw(This).
-doc false.
shouldInheritColours(This) -> wxWindow:shouldInheritColours(This).
-doc false.
setWindowVariant(This,Variant) -> wxWindow:setWindowVariant(This,Variant).
-doc false.
setWindowStyleFlag(This,Style) -> wxWindow:setWindowStyleFlag(This,Style).
-doc false.
setWindowStyle(This,Style) -> wxWindow:setWindowStyle(This,Style).
-doc false.
setVirtualSize(This,Width,Height) -> wxWindow:setVirtualSize(This,Width,Height).
-doc false.
setVirtualSize(This,Size) -> wxWindow:setVirtualSize(This,Size).
-doc false.
setToolTip(This,TipString) -> wxWindow:setToolTip(This,TipString).
-doc false.
setThemeEnabled(This,Enable) -> wxWindow:setThemeEnabled(This,Enable).
-doc false.
setSizerAndFit(This,Sizer, Options) -> wxWindow:setSizerAndFit(This,Sizer, Options).
-doc false.
setSizerAndFit(This,Sizer) -> wxWindow:setSizerAndFit(This,Sizer).
-doc false.
setSizer(This,Sizer, Options) -> wxWindow:setSizer(This,Sizer, Options).
-doc false.
setSizer(This,Sizer) -> wxWindow:setSizer(This,Sizer).
-doc false.
setSizeHints(This,MinW,MinH, Options) -> wxWindow:setSizeHints(This,MinW,MinH, Options).
-doc false.
setSizeHints(This,MinW,MinH) -> wxWindow:setSizeHints(This,MinW,MinH).
-doc false.
setSizeHints(This,MinSize) -> wxWindow:setSizeHints(This,MinSize).
-doc false.
setSize(This,X,Y,Width,Height, Options) -> wxWindow:setSize(This,X,Y,Width,Height, Options).
-doc false.
setSize(This,X,Y,Width,Height) -> wxWindow:setSize(This,X,Y,Width,Height).
-doc false.
setSize(This,Width,Height) -> wxWindow:setSize(This,Width,Height).
-doc false.
setSize(This,Rect) -> wxWindow:setSize(This,Rect).
-doc false.
setScrollPos(This,Orientation,Pos, Options) -> wxWindow:setScrollPos(This,Orientation,Pos, Options).
-doc false.
setScrollPos(This,Orientation,Pos) -> wxWindow:setScrollPos(This,Orientation,Pos).
-doc false.
setScrollbar(This,Orientation,Position,ThumbSize,Range, Options) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range, Options).
-doc false.
setScrollbar(This,Orientation,Position,ThumbSize,Range) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range).
-doc false.
setPalette(This,Pal) -> wxWindow:setPalette(This,Pal).
-doc false.
setName(This,Name) -> wxWindow:setName(This,Name).
-doc false.
setLabel(This,Label) -> wxWindow:setLabel(This,Label).
-doc false.
setId(This,Winid) -> wxWindow:setId(This,Winid).
-doc false.
setHelpText(This,HelpText) -> wxWindow:setHelpText(This,HelpText).
-doc false.
setForegroundColour(This,Colour) -> wxWindow:setForegroundColour(This,Colour).
-doc false.
setFont(This,Font) -> wxWindow:setFont(This,Font).
-doc false.
setFocusFromKbd(This) -> wxWindow:setFocusFromKbd(This).
-doc false.
setFocus(This) -> wxWindow:setFocus(This).
-doc false.
setExtraStyle(This,ExStyle) -> wxWindow:setExtraStyle(This,ExStyle).
-doc false.
setDropTarget(This,Target) -> wxWindow:setDropTarget(This,Target).
-doc false.
setOwnForegroundColour(This,Colour) -> wxWindow:setOwnForegroundColour(This,Colour).
-doc false.
setOwnFont(This,Font) -> wxWindow:setOwnFont(This,Font).
-doc false.
setOwnBackgroundColour(This,Colour) -> wxWindow:setOwnBackgroundColour(This,Colour).
-doc false.
setMinSize(This,Size) -> wxWindow:setMinSize(This,Size).
-doc false.
setMaxSize(This,Size) -> wxWindow:setMaxSize(This,Size).
-doc false.
setCursor(This,Cursor) -> wxWindow:setCursor(This,Cursor).
-doc false.
setContainingSizer(This,Sizer) -> wxWindow:setContainingSizer(This,Sizer).
-doc false.
setClientSize(This,Width,Height) -> wxWindow:setClientSize(This,Width,Height).
-doc false.
setClientSize(This,Size) -> wxWindow:setClientSize(This,Size).
-doc false.
setCaret(This,Caret) -> wxWindow:setCaret(This,Caret).
-doc false.
setBackgroundStyle(This,Style) -> wxWindow:setBackgroundStyle(This,Style).
-doc false.
setBackgroundColour(This,Colour) -> wxWindow:setBackgroundColour(This,Colour).
-doc false.
setAutoLayout(This,AutoLayout) -> wxWindow:setAutoLayout(This,AutoLayout).
-doc false.
setAcceleratorTable(This,Accel) -> wxWindow:setAcceleratorTable(This,Accel).
-doc false.
scrollWindow(This,Dx,Dy, Options) -> wxWindow:scrollWindow(This,Dx,Dy, Options).
-doc false.
scrollWindow(This,Dx,Dy) -> wxWindow:scrollWindow(This,Dx,Dy).
-doc false.
scrollPages(This,Pages) -> wxWindow:scrollPages(This,Pages).
-doc false.
scrollLines(This,Lines) -> wxWindow:scrollLines(This,Lines).
-doc false.
screenToClient(This,Pt) -> wxWindow:screenToClient(This,Pt).
-doc false.
screenToClient(This) -> wxWindow:screenToClient(This).
-doc false.
reparent(This,NewParent) -> wxWindow:reparent(This,NewParent).
-doc false.
removeChild(This,Child) -> wxWindow:removeChild(This,Child).
-doc false.
releaseMouse(This) -> wxWindow:releaseMouse(This).
-doc false.
refreshRect(This,Rect, Options) -> wxWindow:refreshRect(This,Rect, Options).
-doc false.
refreshRect(This,Rect) -> wxWindow:refreshRect(This,Rect).
-doc false.
refresh(This, Options) -> wxWindow:refresh(This, Options).
-doc false.
refresh(This) -> wxWindow:refresh(This).
-doc false.
raise(This) -> wxWindow:raise(This).
-doc false.
popupMenu(This,Menu,X,Y) -> wxWindow:popupMenu(This,Menu,X,Y).
-doc false.
popupMenu(This,Menu, Options) -> wxWindow:popupMenu(This,Menu, Options).
-doc false.
popupMenu(This,Menu) -> wxWindow:popupMenu(This,Menu).
-doc false.
pageUp(This) -> wxWindow:pageUp(This).
-doc false.
pageDown(This) -> wxWindow:pageDown(This).
-doc false.
navigate(This, Options) -> wxWindow:navigate(This, Options).
-doc false.
navigate(This) -> wxWindow:navigate(This).
-doc false.
moveBeforeInTabOrder(This,Win) -> wxWindow:moveBeforeInTabOrder(This,Win).
-doc false.
moveAfterInTabOrder(This,Win) -> wxWindow:moveAfterInTabOrder(This,Win).
-doc false.
move(This,X,Y, Options) -> wxWindow:move(This,X,Y, Options).
-doc false.
move(This,X,Y) -> wxWindow:move(This,X,Y).
-doc false.
move(This,Pt) -> wxWindow:move(This,Pt).
-doc false.
lower(This) -> wxWindow:lower(This).
-doc false.
lineUp(This) -> wxWindow:lineUp(This).
-doc false.
lineDown(This) -> wxWindow:lineDown(This).
-doc false.
layout(This) -> wxWindow:layout(This).
-doc false.
isShownOnScreen(This) -> wxWindow:isShownOnScreen(This).
-doc false.
isTopLevel(This) -> wxWindow:isTopLevel(This).
-doc false.
isShown(This) -> wxWindow:isShown(This).
-doc false.
isRetained(This) -> wxWindow:isRetained(This).
-doc false.
isExposed(This,X,Y,W,H) -> wxWindow:isExposed(This,X,Y,W,H).
-doc false.
isExposed(This,X,Y) -> wxWindow:isExposed(This,X,Y).
-doc false.
isExposed(This,Pt) -> wxWindow:isExposed(This,Pt).
-doc false.
isEnabled(This) -> wxWindow:isEnabled(This).
-doc false.
isFrozen(This) -> wxWindow:isFrozen(This).
-doc false.
invalidateBestSize(This) -> wxWindow:invalidateBestSize(This).
-doc false.
initDialog(This) -> wxWindow:initDialog(This).
-doc false.
inheritAttributes(This) -> wxWindow:inheritAttributes(This).
-doc false.
hide(This) -> wxWindow:hide(This).
-doc false.
hasTransparentBackground(This) -> wxWindow:hasTransparentBackground(This).
-doc false.
hasScrollbar(This,Orient) -> wxWindow:hasScrollbar(This,Orient).
-doc false.
hasCapture(This) -> wxWindow:hasCapture(This).
-doc false.
getWindowVariant(This) -> wxWindow:getWindowVariant(This).
-doc false.
getWindowStyleFlag(This) -> wxWindow:getWindowStyleFlag(This).
-doc false.
getVirtualSize(This) -> wxWindow:getVirtualSize(This).
-doc false.
getUpdateRegion(This) -> wxWindow:getUpdateRegion(This).
-doc false.
getToolTip(This) -> wxWindow:getToolTip(This).
-doc false.
getThemeEnabled(This) -> wxWindow:getThemeEnabled(This).
-doc false.
getTextExtent(This,String, Options) -> wxWindow:getTextExtent(This,String, Options).
-doc false.
getTextExtent(This,String) -> wxWindow:getTextExtent(This,String).
-doc false.
getSizer(This) -> wxWindow:getSizer(This).
-doc false.
getSize(This) -> wxWindow:getSize(This).
-doc false.
getScrollThumb(This,Orientation) -> wxWindow:getScrollThumb(This,Orientation).
-doc false.
getScrollRange(This,Orientation) -> wxWindow:getScrollRange(This,Orientation).
-doc false.
getScrollPos(This,Orientation) -> wxWindow:getScrollPos(This,Orientation).
-doc false.
getScreenRect(This) -> wxWindow:getScreenRect(This).
-doc false.
getScreenPosition(This) -> wxWindow:getScreenPosition(This).
-doc false.
getRect(This) -> wxWindow:getRect(This).
-doc false.
getPosition(This) -> wxWindow:getPosition(This).
-doc false.
getParent(This) -> wxWindow:getParent(This).
-doc false.
getName(This) -> wxWindow:getName(This).
-doc false.
getMinSize(This) -> wxWindow:getMinSize(This).
-doc false.
getMaxSize(This) -> wxWindow:getMaxSize(This).
-doc false.
getLabel(This) -> wxWindow:getLabel(This).
-doc false.
getId(This) -> wxWindow:getId(This).
-doc false.
getHelpText(This) -> wxWindow:getHelpText(This).
-doc false.
getHandle(This) -> wxWindow:getHandle(This).
-doc false.
getGrandParent(This) -> wxWindow:getGrandParent(This).
-doc false.
getForegroundColour(This) -> wxWindow:getForegroundColour(This).
-doc false.
getFont(This) -> wxWindow:getFont(This).
-doc false.
getExtraStyle(This) -> wxWindow:getExtraStyle(This).
-doc false.
getDPIScaleFactor(This) -> wxWindow:getDPIScaleFactor(This).
-doc false.
getDropTarget(This) -> wxWindow:getDropTarget(This).
-doc false.
getCursor(This) -> wxWindow:getCursor(This).
-doc false.
getContainingSizer(This) -> wxWindow:getContainingSizer(This).
-doc false.
getClientSize(This) -> wxWindow:getClientSize(This).
-doc false.
getChildren(This) -> wxWindow:getChildren(This).
-doc false.
getCharWidth(This) -> wxWindow:getCharWidth(This).
-doc false.
getCharHeight(This) -> wxWindow:getCharHeight(This).
-doc false.
getCaret(This) -> wxWindow:getCaret(This).
-doc false.
getBestSize(This) -> wxWindow:getBestSize(This).
-doc false.
getBackgroundStyle(This) -> wxWindow:getBackgroundStyle(This).
-doc false.
getBackgroundColour(This) -> wxWindow:getBackgroundColour(This).
-doc false.
getAcceleratorTable(This) -> wxWindow:getAcceleratorTable(This).
-doc false.
freeze(This) -> wxWindow:freeze(This).
-doc false.
fitInside(This) -> wxWindow:fitInside(This).
-doc false.
fit(This) -> wxWindow:fit(This).
-doc false.
findWindow(This,Id) -> wxWindow:findWindow(This,Id).
-doc false.
enable(This, Options) -> wxWindow:enable(This, Options).
-doc false.
enable(This) -> wxWindow:enable(This).
-doc false.
dragAcceptFiles(This,Accept) -> wxWindow:dragAcceptFiles(This,Accept).
-doc false.
disable(This) -> wxWindow:disable(This).
-doc false.
destroyChildren(This) -> wxWindow:destroyChildren(This).
-doc false.
convertPixelsToDialog(This,Sz) -> wxWindow:convertPixelsToDialog(This,Sz).
-doc false.
convertDialogToPixels(This,Sz) -> wxWindow:convertDialogToPixels(This,Sz).
-doc false.
close(This, Options) -> wxWindow:close(This, Options).
-doc false.
close(This) -> wxWindow:close(This).
-doc false.
clientToScreen(This,X,Y) -> wxWindow:clientToScreen(This,X,Y).
-doc false.
clientToScreen(This,Pt) -> wxWindow:clientToScreen(This,Pt).
-doc false.
clearBackground(This) -> wxWindow:clearBackground(This).
-doc false.
centreOnParent(This, Options) -> wxWindow:centreOnParent(This, Options).
-doc false.
centerOnParent(This, Options) -> wxWindow:centerOnParent(This, Options).
-doc false.
centreOnParent(This) -> wxWindow:centreOnParent(This).
-doc false.
centerOnParent(This) -> wxWindow:centerOnParent(This).
-doc false.
centre(This, Options) -> wxWindow:centre(This, Options).
-doc false.
center(This, Options) -> wxWindow:center(This, Options).
-doc false.
centre(This) -> wxWindow:centre(This).
-doc false.
center(This) -> wxWindow:center(This).
-doc false.
captureMouse(This) -> wxWindow:captureMouse(This).
-doc false.
cacheBestSize(This,Size) -> wxWindow:cacheBestSize(This,Size).
 %% From wxEvtHandler
-doc false.
disconnect(This,EventType, Options) -> wxEvtHandler:disconnect(This,EventType, Options).
-doc false.
disconnect(This,EventType) -> wxEvtHandler:disconnect(This,EventType).
-doc false.
disconnect(This) -> wxEvtHandler:disconnect(This).
-doc false.
connect(This,EventType, Options) -> wxEvtHandler:connect(This,EventType, Options).
-doc false.
connect(This,EventType) -> wxEvtHandler:connect(This,EventType).
