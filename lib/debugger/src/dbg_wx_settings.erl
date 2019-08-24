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
-module(dbg_wx_settings).

-include_lib("kernel/include/file.hrl").
-include_lib("wx/include/wx.hrl").

%% External exports
-export([load/3, save/3]).


%%====================================================================
%% External exports
%%====================================================================

%%====================================================================
%% load/save(Win, Pos, Str) -> {ok, FileName} | cancel
%%====================================================================
load(Win, Pos, SFile) ->
    Str = "Load Settings Dialog",
    open_win(Win, Pos, SFile, Str, ?wxFD_OPEN).

save(Win, Pos, SFile) ->
    Str = "Save Settings Dialog",
    open_win(Win, Pos, SFile, Str, ?wxFD_SAVE).


%%====================================================================
%% Internal functions
%%====================================================================

open_win(Win, Pos, SFile, Str, What) ->
    {SDir, SFileName} =
        if
            %% If settings are saved for the first time, and to
            %% the default directory HOME/erlang.tools/debugger,
            %% make sure the directory exists, or create it if
            %% desired and possible
            SFile==default -> {default_settings_dir(Win), "NoName.state"};
            true -> {filename:dirname(SFile), filename:basename(SFile)}
        end,

    FD = wxFileDialog:new(Win, [{message,Str},{pos, Pos},
                                {defaultDir,SDir},
                                {defaultFile,SFileName},
                                {wildCard, "*.state"},
                                {style,What}]),
    case wxFileDialog:showModal(FD) of
        ?wxID_OK ->
            NewFile = wxFileDialog:getPath(FD),
	    {ok, NewFile};
        _ ->
            wxFileDialog:destroy(FD),
            cancel
    end.

default_settings_dir(Win) ->
    {ok, [[Home]]} = init:get_argument(home),
    DefDir = filename:join([Home, ".erlang_tools", "debugger"]),

    case filelib:is_dir(DefDir) of
	true -> DefDir;
	false ->
	    {ok, CWD} = file:get_cwd(),
	    
	    Msg = ["Default directory ", DefDir, " does not exist. ",
		   "Click OK to create it or ",
		   "Cancel to use other directory."],
	    case dbg_wx_win:confirm(Win, Msg) of
		ok ->
		    ToolsDir = filename:dirname(DefDir),
		    case filelib:is_dir(ToolsDir) of
			true ->
			    case file:make_dir(DefDir) of
				ok -> DefDir;
				_Error -> CWD
			    end;
			false ->
			    case file:make_dir(ToolsDir) of
				ok ->
				    case file:make_dir(DefDir) of
					ok -> DefDir;
					_Error -> CWD
				    end;
				_Error -> CWD
			    end
		    end;
		cancel -> CWD
	    end
    end.
