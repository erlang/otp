%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%% Author: Lennart Öhman, lennart.ohman@st.se
-module(inviso_autostart).

-export([autostart/1,which_config_file/0]).

%% This module implements the default autostart module for the inviso runtime
%% component.
%% It will:
%% (1) Open the autostart configuration file (either the default or the one
%%     pointed out by the runtime_tools application parameter inviso_autostart_config).
%% (2) Check that the incarnation counter has not reached 0. If so, we do not
%%     allow (yet) one autostart.
%% (3) Rewrite the configuration file if there was an incarnation counter.
%%     (With the counter decreased).
%% (4) Inspect the content of the configuration file and pass paramters in the
%%     return value (which is interpreted by the runtime component).
%%
%% CONTENT OF A CONFIGURATION FILE:
%% A plain text file containing erlang tuple terms, each ended with a period(.).
%% The following parameters are recognized:
%% {repeat,N} N=interger(),
%%   The number of remaining allowed autostart incarnations of inviso.
%% {options,Options} Options=list()
%%   The options which controls the runtime component, such as overload and
%%   dependency.
%% {mfa,{Mod,Func,Args}} Args=list()
%%   Controls how a spy process initiating tracing, patterns and flags shall
%%   be started.
%% {tag,Tag}
%%   The tag identifying the runtime component to control components.
%% =============================================================================

%% This function is run in the runtime component's context during autostart
%% to determine whether to continue and if, then how.
autostart(_AutoModArgs) ->
    ConfigFile=
	case application:get_env(inviso_autostart_conf) of
	    {ok,FileName} when is_list(FileName) -> % Use this filename then.
		FileName;
	    {ok,{load,FileNames,{M,F}}} ->  % First load the module, then...
		case try_load_module(FileNames) of
		    ok ->
			autostart_apply(M,F);
		    false ->                % No such module available
			"inviso_autostart.config"
		end;
	    {ok,{gettia_asc,asc_file}} ->   % Uggly hack to not have to change in GSN-CPS.
		case try_load_module(["/tmp/DPE_COMMONLOG/gettia_asc",
				      "/tmp/DPE_COMMONLOG/gettia_overload"]) of
		    ok ->
			autostart_apply(gettia_asc,asc_file);
		    false ->                % No such module available
			false
		end;
	    {ok,{M,F}} ->                   % Use M:F(node())
		autostart_apply(M,F);
	    {ok,no_autostart} ->
		false;
	    _ ->                            % Use a default name, in CWD!
		"inviso_autostart.config"
	end,
    if
	is_list(ConfigFile) ->
	    case file:consult(ConfigFile) of
		{ok,Terms} ->               % There is a configuration.
		    case handle_repeat(ConfigFile,Terms) of
			ok ->               % Handled or not, we shall continue.
			    {get_mfa(Terms),get_options(Terms),get_tag(Terms)};
			stop ->             % We are out of allowed starts.
			    true            % Then no autostart.
		    end;
		{error,_} ->                % There is no config file
		    true                    % Then no autostart!
	    end;
	true ->                             % Skip it then.
	    true
    end.

autostart_apply(M,F) ->
    case catch M:F(node()) of
	FileName when is_list(FileName) ->
	    FileName;
	no_autostart ->                     % No autostart after all.
	    false;
	_ ->
	    "inviso_autostart.config"
    end.

%% This function is necessary since it is not always the case that all code-paths
%% are set at the time of an autostart.
try_load_module([AbsFileName|Rest]) when is_list(AbsFileName) ->
    case catch code:load_abs(AbsFileName) of % May not be a proper filename.
	{module,_Mod} ->
	    try_load_module(Rest);
	_ ->
	    false
    end;
try_load_module([]) ->                      % Load all beam files successfully.
    ok;
try_load_module(AbsFileName) when is_list(AbsFileName) ->
    try_load_module([AbsFileName]).
%% -----------------------------------------------------------------------------

%% Function returning the filename probably used as autostart config file.
%% Note that this function must be executed at the node in question.
which_config_file() ->
    case application:get_env(runtime_tools,inviso_autostart_conf) of
	{ok,FileName} when is_list(FileName) -> % Use this filename then.
	    FileName;
	{ok,{M,F}} ->                       % Use M:F(node())
	    case catch M:F(node()) of
		FileName when is_list(FileName) ->
		    FileName;
		_ ->
		    {ok,CWD}=file:get_cwd(),
		    filename:join(CWD,"inviso_autostart.config")
	    end;
	_ ->                                % Use a default name, in CWD!
	    {ok,CWD}=file:get_cwd(),
	    filename:join(CWD,"inviso_autostart.config")
    end.
%% -----------------------------------------------------------------------------


%% Help function which finds out if there is a limit on the number of times
%% we shall autostart. If there is a repeat parameter and it is greater than
%% zero, the file must be rewritten with the parameter decreased with one.
%% Returns 'ok' or 'stop'.
handle_repeat(FileName,Terms) ->
    case lists:keysearch(repeat,1,Terms) of
	{value,{_,N}} when N>0 ->           % Controlls how many time more.
	    handle_repeat_rewritefile(FileName,Terms,N-1),
	    ok;                             % Indicate that we shall continue.
	{value,_} ->                        % No we have reached the limit.
	    stop;
	false ->                            % There is no repeat parameter.
	    ok                              % No restrictions then!
    end.

%% Help function which writes the configuration file again, but with the
%% repeat parameter set to NewN.
%% Returns nothing significant.
handle_repeat_rewritefile(FileName,Term,NewN) ->
    case file:open(FileName,[write]) of
	{ok,FD} ->
	    NewTerm=lists:keyreplace(repeat,1,Term,{repeat,NewN}),
	    handle_repeat_rewritefile_2(FD,NewTerm),
	    file:close(FD);
	{error,_Reason} ->                  % Not much we can do then?!
	    error
    end.

handle_repeat_rewritefile_2(FD,[Tuple|Rest]) ->
    io:format(FD,"~w.~n",[Tuple]),
    handle_repeat_rewritefile_2(FD,Rest);
handle_repeat_rewritefile_2(_,[]) ->
    true.
%% -----------------------------------------------------------------------------

%% Three help functions finding the parameters possible to give to the runtime
%% component. Note that some of them have default values, should the parameter
%% not exist.
get_mfa(Terms) ->
    case lists:keysearch(mfa,1,Terms) of
	{value,{_,MFA}} ->
	    MFA;
	false ->
	    false
    end.

get_options(Terms) ->
    case lists:keysearch(options,1,Terms) of
	{value,{_,Options}} ->
	    Options;
	false ->
	    []
    end.

get_tag(Terms) ->
    case lists:keysearch(tag,1,Terms) of
	{value,{_,Tag}} ->
	    Tag;
	false ->
	    default_tag
    end.
%% -----------------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

