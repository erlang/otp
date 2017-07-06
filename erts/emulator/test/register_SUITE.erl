%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

-module(register_SUITE).


%-define(line_trace, 1).

-include_lib("common_test/include/ct.hrl").

%-compile(export_all).
-export([all/0, suite/0]).

-export([otp_8099/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() -> 
    [otp_8099].

%%
%% Test cases
%%

-define(OTP_8099_NAME, otp_8099_reg_proc).

otp_8099(Config) when is_list(Config) ->
    otp_8099_test(1000000).

otp_8099_test(0) ->
    ok;
otp_8099_test(N) ->
    P = spawn(fun () -> otp_8099_proc() end),
    case catch register(?OTP_8099_NAME, P) of
	      true ->
		  ok;
	      _ ->
		  OP = whereis(?OTP_8099_NAME),
		  (catch unregister(?OTP_8099_NAME)),
		  (catch exit(OP, kill)),
		  true = (catch register(?OTP_8099_NAME, P))
	  end,
    P = whereis(?OTP_8099_NAME),
    exit(P, kill),
    otp_8099_test(N-1).

otp_8099_proc() ->
    receive _ -> ok end,
    otp_8099_proc().
