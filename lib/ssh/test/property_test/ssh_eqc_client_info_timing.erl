%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2018. All Rights Reserved.
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

-module(ssh_eqc_client_info_timing).

-compile(export_all).

-proptest(eqc).
-proptest([triq,proper]).

-ifndef(EQC).
-ifndef(PROPER).
-ifndef(TRIQ).
-define(EQC,true).
%%-define(PROPER,true).
%%-define(TRIQ,true).
-endif.
-endif.
-endif.

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-define(MOD_eqc,eqc).

-else.
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(MOD_eqc,proper).

-else.
-ifdef(TRIQ).
-define(MOD_eqc,triq).
-include_lib("triq/include/triq.hrl").

-endif.
-endif.
-endif.


%%% Properties:

prop_seq(Config) ->
    error_logger:tty(false),
    {ok,Pid} = ssh_eqc_event_handler:add_report_handler(),
    {_, _, Port} = init_daemon(Config),
    numtests(1000,
	     ?FORALL(Delay, choose(0,100),%% Micro seconds
		     try 
			 send_bad_sequence(Port, Delay, Pid),
			 not any_relevant_error_report(Pid)
		     catch
			 C:E:S -> ct:log("~p:~p~n~p",[C,E,S]),
				false
		     end
		    )).

send_bad_sequence(Port, Delay, Pid) ->
    send_bad_sequence(Port, Delay, Pid, 10).

send_bad_sequence(Port, Delay, Pid, N) ->
    case gen_tcp:connect("localhost",Port,[]) of
        {ok,S} ->
            gen_tcp:send(S,"Illegal info-string\r\n"),
            ssh_test_lib:sleep_microsec(Delay),
            gen_tcp:close(S);

        {error,econnreset} when N>0 ->
            timer:sleep(1),
            send_bad_sequence(Port, Delay, Pid, N-1)
    end.

any_relevant_error_report(Pid) ->
    {ok, Reports} = ssh_eqc_event_handler:get_reports(Pid),
    lists:any(fun({error_report,_,{_,supervisor_report,L}}) when is_list(L) -> 
		      lists:member({reason,{badmatch,{error,closed}}}, L);
		 (_) ->
		      false
	      end, Reports).

%%%================================================================
init_daemon(Config) ->
    ok = begin ssh:stop(), ssh:start() end,
    DataDir = proplists:get_value(data_dir, Config),
    ssh_test_lib:daemon([{system_dir,DataDir}]).

