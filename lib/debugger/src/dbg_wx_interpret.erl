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
%%

%%
-module(dbg_wx_interpret).

-include_lib("kernel/include/file.hrl").
-include_lib("wx/include/wx.hrl").

%% External exports
-export([start/4]).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% start(Win, Pos, Dir, Mode)
%%   GS  = Graphics system id
%%   Dir = string()
%%   Pos = {X,Y}
%%   Mode = local | global
%%--------------------------------------------------------------------
start(Win, Pos, SDir, Mode) ->
    Title = "Interpret Modules",

    FD = dbg_wx_filedialog_win:new(Win, -1, 
				   [{message,Title}, {pos,Pos},
				    {defaultDir,SDir},
				    {sort, type},
				    {filter, fun filter_files/2}]),

    case wxFileDialog:showModal(FD) of
	?wxID_OK ->
	    Files = dbg_wx_filedialog_win:getFilenames(FD),
	    Dir   = dbg_wx_filedialog_win:getDirectory(FD),
	    dbg_wx_filedialog_win:destroy(FD),
	    interpret_all(Dir, Files, Mode, Win, []),
	    self() ! {dbg_ui_interpret, Dir},
	    ok;
	_ ->
	    dbg_wx_filedialog_win:destroy(FD),
	    cancel
    end.

filter_files(Dir, Name) ->
    case filename:extension(Name) of 
	".erl" ->
	    File = filename:join(Dir, Name),
	    case int:interpretable(File) of
		true ->
		    {"erl src", erl_src, {0,0,0}};
		_ ->
		    {"erl src no dbg", erl_src, {128,128,128}}
	    end;
	".hrl" ->
	    {"erl hrl", erl_hrl, {128,128,128}};
	".beam" ->
	    {"erl bin", erl_bin, {128,128,128}};
	_ ->
	    {"file",    file,    {128,128,128}}
    end.

%%% Standard file browser variant
%% start(Win, Pos, SDir, Mode) ->
%%     Title = "Interpret Dialog",
%%     Filter = "*.erl",

%%     FD = FileDialog:new(Win, [{message,Title},{pos, Pos},
%% 				{defaultDir,SDir},
%% 				{wildCard, Filter},
%% 				{style,?wxFD_OPEN bor ?wxFD_MULTIPLE}]),

%%     case wxFileDialog:showModal(FD) of
%% 	?wxID_OK ->
%% 	    Files = wxFileDialog:getFilenames(FD),
%% 	    Dir   = wxFileDialog:getDirectory(FD),
%% 	    wxFileDialog:destroy(FD),
%% 	    interpret_all(Dir, Files, Mode, Win),
%% 	    self() ! {dbg_ui_interpret, Dir},
%% 	    ok;
%% 	_ ->
%% 	    wxFileDialog:destroy(FD),
%% 	    cancel
%%     end.

interpret_all(Dir, [File0|Files], Mode, Window, Errors) ->
    File = filename:join(Dir, File0),
    Res = case Mode of
	      local -> int:i(File);
	      global -> int:ni(File)
	  end,
    case Res of
	{module, _Mod} ->
	    interpret_all(Dir, Files, Mode, Window, Errors);
	error ->
	    interpret_all(Dir, Files, Mode, Window, [File0|Errors])
    end;
interpret_all(_Dir, [], _Mode, _Window, []) ->
    true;
interpret_all(Dir, [], _Mode, Window, Errors) ->
    Msg = [begin
	       File = filename:join(Dir, Name),
	       Error = format_error(int:interpretable(File)),
	       ["\n   ",Name,": ",Error]
	   end || Name <- Errors],
    All = ["Error when interpreting: ", Msg],
    dbg_wx_win:confirm(Window, lists:flatten(All)),
    true.

format_error({error,no_beam}) -> "No BEAM file";
format_error({error,no_debug_info}) -> "No debug_info in BEAM file";
format_error({error,badarg}) -> "File does not exist";
format_error({error,{app,App}}) ->
    "Cannot interpret "++atom_to_list(App)++" modules".
