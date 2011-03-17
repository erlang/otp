%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

-module(register_SUITE).


%-define(line_trace, 1).

-include_lib("test_server/include/test_server.hrl").

%-compile(export_all).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([otp_8099/1]).

-define(DEFAULT_TIMEOUT, ?t:minutes(2)).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [otp_8099].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Case, Config) when is_list(Config) ->
    Dog = ?t:timetrap(?DEFAULT_TIMEOUT),
    [{watchdog, Dog}, {testcase, Case} | Config].

end_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

%%
%% Test cases
%%

-define(OTP_8099_NAME, otp_8099_reg_proc).

otp_8099(Config) when is_list(Config) ->
    case catch erlang:system_info(lock_counting) of
	true -> {skipped,
		 "Lock counting enabled. Current lock counting "
		 "implementation cannot handle this many "
		 "processes."};
	_ ->
	    otp_8099_test(1000000)
    end.

otp_8099_test(0) ->
    ok;
otp_8099_test(N) ->
    ?line P = spawn(fun () -> otp_8099_proc() end),
    ?line case catch register(?OTP_8099_NAME, P) of
	      true ->
		  ?line ok;
	      _ ->
		  ?line OP = whereis(?OTP_8099_NAME),
		  ?line (catch unregister(?OTP_8099_NAME)),
		  ?line (catch exit(OP, kill)),
		  ?line true = (catch register(?OTP_8099_NAME, P))
	  end,
    ?line P = whereis(?OTP_8099_NAME),
    ?line exit(P, kill),
    ?line otp_8099_test(N-1).

otp_8099_proc() ->
    receive _ -> ok end,
    otp_8099_proc().

%%
%% Utils
%%

