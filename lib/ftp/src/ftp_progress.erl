%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2018. All Rights Reserved.
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
%% Description: This module impements a temporary process that 
%% performes progress reporting during file transfer calling a user 
%% defined callback function. Its life span is as long as the ftp connection
%% processes that spawned it lives. The purpose of this process is to 
%% shild the ftp connection process from errors and time consuming operations
%% in the user defined callback function. 

-module(ftp_progress).

%% Internal API
-export([start_link/1, report/2, stop/1]).

%% Spawn export
-export([init/1]).

-include_lib("kernel/include/file.hrl").

-record(progress, {
	  file                  :: string() | 'undefined',
	  cb_module             :: module(),
	  cb_function           :: atom(),
	  init_progress_term    :: term(),
	  current_progress_term :: term()
	 }).

%%%=========================================================================
%%%  Internal application API  
%%%=========================================================================
%%--------------------------------------------------------------------------
%% start_link(Options) -> ignore | pid()
%%	Options = ignore | {CBModule, CBFunction, InitProgressTerm}
%%
%% Description: Starts the progress report process unless progress reporting
%% should not be performed.
%%--------------------------------------------------------------------------
-type options() :: 'ignore' | {module(), atom(), term()}.
-spec start_link(options()) -> 'ignore' | pid().
start_link(ignore) ->
    ignore;
start_link(Options) ->
    spawn_link(?MODULE, init, [Options]).

%%--------------------------------------------------------------------------
%% report_progress(Pid, Report) -> ok
%%      Pid = pid()
%%	Report = {local_file, File} | {remote_file, File} | 
%%               {transfer_size, Size}
%%      Size = integer()
%%
%% Description: Reports progress to the reporting process that calls the
%% user defined callback function.
%%--------------------------------------------------------------------------
-type report() :: {'local_file', string()} | {'remote_file', string()}
                | {'transfer_size', non_neg_integer()}.
-spec report(pid(), report()) -> 'ok'.
report(Pid, Report) ->
    Pid ! {progress_report, Report},
    ok.

%%--------------------------------------------------------------------------
%% stop(Pid) -> ok
%%	Pid = pid()
%%
%% Description: 
%%--------------------------------------------------------------------------   
-spec stop(pid()) -> 'ok'.
stop(Pid) ->
    Pid ! stop,
    ok.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
init(Options) ->
    loop(progress(Options)).

loop(Progress) ->
    receive 
	{progress_report, Report} ->
	    NewProgress = report_progress(Report, Progress),
	    loop(NewProgress);
	stop ->
	    ok
    end.

progress({CBModule, CBFunction, InitProgressTerm}) when is_atom(CBModule),
							is_atom(CBFunction) -> 
    #progress{cb_module = CBModule,
	      cb_function = CBFunction,
	      init_progress_term = InitProgressTerm,
	      current_progress_term = InitProgressTerm}.

report_progress({local_file, File}, Progress) ->
    {ok, FileInfo} = file:read_file_info(File),
    report_progress({file_size, FileInfo#file_info.size}, 
		    Progress#progress{file = File});

report_progress({remote_file, File}, Progress) ->
    report_progress({file_size, unknown}, Progress#progress{file = File});

report_progress(Size, #progress{file = File, 
				cb_module = CBModule,
				cb_function = CBFunction,
				current_progress_term = Term,
				init_progress_term = InitTerm} = Progress) ->

    NewProgressTerm = CBModule:CBFunction(Term, File, Size),
    
    case Size of 
	{transfer_size, 0} ->
	    %% Transfer is compleat reset initial values
	    Progress#progress{current_progress_term = InitTerm,
			      file = undefined};
	_ ->
	    Progress#progress{current_progress_term = NewProgressTerm}
    end.
