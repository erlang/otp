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

-include_lib("wx/include/wx.hrl").


-define(TEXTSIZE, 9).

-define(Border, {border, 2}).
-define(BorderOpt,[{flag,?wxALL}, ?Border]).

-define(menuID_FILE_QUIT, 102).
-define(menuID_FILE_SAVE_WARNINGS, 100).
-define(menuID_FILE_SAVE_LOG, 101).

-define(menuID_WARN_MATCH_FAILURES, 200).
-define(menuID_WARN_FAIL_FUN_CALLS, 201).
-define(menuID_WARN_BAD_FUN, 202).
-define(menuID_WARN_OPAQUE, 203).
-define(menuID_WARN_LIST_CONSTR, 204).
-define(menuID_WARN_UNUSED_FUN, 205).
-define(menuID_WARN_ERROR_HANDLING_FUN, 206).
-define(menuID_WARN_NO_RETURN_FUN, 207).
-define(menuID_WARN_UNEXPORTED_FUN, 208).
-define(menuID_WARN_RACE_CONDITIONS, 209).
-define(menuID_WARN_WRONG_CONTRACTS, 210).
-define(menuID_WARN_CONTRACT_SYNTAX, 211).

-define(menuID_PLT_INIT_EMPTY, 300).
-define(menuID_PLT_SHOW_CONTENTS, 301).
-define(menuID_PLT_SEARCH_CONTENTS, 302).

-define(menuID_OPTIONS_MACRO, 400).
-define(menuID_OPTIONS_INCLUDE_DIR, 401).

-define(menuID_HELP_MANUAL, 500).
-define(menuID_HELP_WARNING_OPTIONS, 501).
-define(menuID_HELP_ABOUT, 499).

-define(LABEL1,502).
-define(LABEL2,503).
-define(LABEL3,504).
-define(LABEL4,505).
-define(LABEL5,505).
-define(LABEL6,506).
-define(LABEL7,507).
-define(LABEL8,508).
-define(LABEL9,509).

-define(ChosenBox,510).
-define(LogBox,511).
-define(FilePicker,512).
-define(DirPicker,513).
-define(WarningsBox,521).

-define(Del_Button,514).
-define(DelAll_Button,515).
-define(ClearLog_Button,516).
-define(Add_Button,517).
-define(AddDir_Button,532).
-define(AddRec_Button,518).
-define(ClearWarn_Button,519).
-define(Run_Button,520).
-define(Stop_Button,522).
-define(ExplWarn_Button,523).
-define(RADIOBOX, 524).

-define(Dialog, 525).
-define(Dialog_Ok, 526).
-define(Dialog_Cancel, 527).
-define(Dialog_Mes, 528).

-define(MESSAGE, 529).
-define(Message_Info, 530).
-define(Message_Ok, 531).


-define(Message, 534).
-define(SaveWarn, 533).
-define(SearchPltDialog, 535).
-define(ModLabel, 536).
-define(FunLabel, 537).
-define(ArLabel, 538).
-define(ModText, 539).
-define(FunText, 540).
-define(ArText, 541).
-define(SearchButton, 542).
-define(Search_Cancel, 543).

-define(IncludeDir, 544).
-define(InclLabel, 545).
-define(InclPicker, 546).
-define(InclBox, 547).
-define(InclAdd, 548).
-define(InclDel, 549).
-define(InclDelAll, 550).
-define(InclOk, 551).
-define(InclCancel, 552).

-define(MacroDir, 553).
-define(MacroLabel, 554).
-define(MacroText, 555).
-define(TermLabel, 556).
-define(TermText, 557).
-define(MacroBox, 558).
-define(MacroAdd, 559).
-define(MacroDel, 560).
-define(MacroDelAll, 561).
-define(MacroOk, 562).
-define(MacroCancel, 563).

-define(ExplWin, 564).
-define(ExplText, 565).
-define(ExplButton, 566).
-define(ExplOk, 567).
