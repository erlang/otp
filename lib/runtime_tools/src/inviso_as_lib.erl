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

%%% File    : inviso_as_lib.erl
%%% Author  : Lennart Öhman <lennart.ohman@st.se>
%%% Description : 
%% The purpose of the inviso autostart library is to provide useful functions
%% for anyone wanting to customize the autostart mechanism in the inviso
%% tracer. It is intended to work well with the example 'inviso_autostart_server'.
%%%
%%% Created : 15 Dec 2005 by Lennart Öhman
%% -----------------------------------------------------------------------------

-module(inviso_as_lib).

-export([setup_autostart/7,setup_autostart/8,setup_autostart/9,
	 inhibit_autostart/1,
	 set_repeat/2,set_repeat_2/2]).
%% -----------------------------------------------------------------------------

%% setup_autostart(Node,Repeat,Options,TracerData,CmdFiles,Bindings) = ok|{error,Reason}.
%%    Repeat=integer(), where 0 means no (more) autostarts.
%%    Options=List of options as taken by the runtime component at start-up.
%%    TracerData= Tracerdata as given to inviso_rt:init_tracing.
%%    CmdFiles=[FileName,...] list of string(), files that will be executed
%%      by the subprocess started during autostart.
%%    Bindings=[{VarName,Value},...] Variable bindings for CmdFiles.
%%      VarName=atom(),
%%
%% This function creates the inviso_autostart.config file on Erlang node Node.
%% This is useful when you wish to prepare for an autostarted trace.
setup_autostart(Node,Repeat,Options,TracerData,CmdFiles,Bindings,Translations) ->
    setup_autostart(Node,Repeat,Options,TracerData,CmdFiles,
		    Bindings,Translations,inviso_std_ref,off).
setup_autostart(Node,Repeat,Options,TracerData,CmdFiles,Bindings,Translations,RTtag) ->
    setup_autostart(Node,Repeat,Options,TracerData,CmdFiles,
		    Bindings,Translations,RTtag,off).
setup_autostart(Node,Repeat,Options,TracerData,CmdFiles,Bindings,Translations,RTtag,Dbg) ->
    case rpc:call(Node,inviso_autostart,which_config_file,[]) of
	FileName when is_list(FileName) ->      % Write to this file then.
	    {String,Args}=format_config_file(Repeat,TracerData,Options,CmdFiles,
					     Bindings,Translations,RTtag,Dbg),
	    Bytes=list_to_binary(io_lib:format(String,Args)),
	    case rpc:call(Node,file,write_file,[FileName,Bytes]) of
		ok ->
		    ok;
		{error,Reason} ->
		    {error,{write_file,Reason}};
		{badrpc,Reason} ->
		    {error,{badrpc,{write_file,Reason}}}
	    end;
	{error,Reason} ->
	    {error,{which_config_file,Reason}};
	{badrpc,Reason} ->
	    {error,{badrpc,{which_config_file,Reason}}}
    end.
%% -----------------------------------------------------------------------------

%% inhibit_autostart(Node) = ok|{error,Reason}
%%
%% Inhibits autostart by simply making the repeat parameter zero in the
%% configuration file at node Node. All other parameters are left untouched.
inhibit_autostart(Node) ->
    set_repeat(Node,0).
%% -----------------------------------------------------------------------------

%% set_repeat(Node,N)=ok | {error,Reason}
%%   N=integer(), the number of time autostart shall be allowed.
set_repeat(Node,N) ->
    case examine_config_file(Node) of
	{ok,FileName,Terms} ->
	    NewTerms=[{repeat,N}|lists:keydelete(repeat,1,Terms)],
	    case rpc:call(Node,?MODULE,set_repeat_2,[FileName,NewTerms]) of
		{badrpc,Reason} ->
		    {error,{badrpc,{open,Reason}}};
		Result ->
		    Result
	    end;
	{error,Reason} ->
	    {error,Reason}
    end.

%% Must be a sepparate function to do rpc on. The entire function must be done
%% in one rpc call. Otherwise the FD will die since it is linked to the opening
%% process.
set_repeat_2(FileName,NewTerms) ->
    case file:open(FileName,[write]) of
	{ok,FD} ->
	    String=lists:flatten(lists:map(fun(_)->"~w.~n" end,NewTerms)),
	    case catch io:format(FD,String,NewTerms) of
		ok ->
		    file:close(FD),
		    ok;
		{'EXIT',Reason} ->
		    file:close(FD),
		    {error,{format,Reason}}
	    end;
	{error,Reason} ->
	    {error,Reason}
    end.
%% -----------------------------------------------------------------------------

examine_config_file(Node) ->
    case rpc:call(Node,inviso_autostart,which_config_file,[]) of
	FileName when is_list(FileName) ->     % Read this file, and then modify it.
	    case rpc:call(Node,file,consult,[FileName]) of
		{ok,Terms} ->
		    {ok,FileName,Terms};
		{error,Reason} ->
		    {error,{consult,Reason}};
		{badrpc,Reason} ->
		    {error,{badrpc,{consult,Reason}}}
	    end;
	{error,Reason} ->
	    {error,{which_config_file,Reason}};
	{badrpc,Reason} ->
	    {error,{badrpc,{which_config_file,Reason}}}
    end.
%% -----------------------------------------------------------------------------

format_config_file(Repeat,TracerData,Options,CmdFiles,Bindings,Translations,RTtag,Dbg) ->
    String="~w.~n~w.~n~w.~n~w.~n",
    Args=[{repeat,Repeat},
	  {mfa,{inviso_autostart_server,init,[[{tracerdata,TracerData},
					       {cmdfiles,CmdFiles},
					       {bindings,Bindings},
					       {translations,Translations},
					       {debug,Dbg}]]}},
	  {options,Options},
	  {tag,RTtag}],
    {String,Args}.
%% -----------------------------------------------------------------------------







