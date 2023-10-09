%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Megaco transport behaviour module
%%----------------------------------------------------------------------

-module(megaco_transport).

-callback send_message(Handle, Msg) -> ok | {cancel, Reason :: term()} | Error when
      Handle :: term(),
      Msg :: iodata(),
      Error :: term().

-callback send_message(Handle, Msg, Resend) -> ok | {cancel, Reason :: term()} | Error  when
      Handle :: term(),
      Msg :: iodata(),
      Resend :: boolean(),
      Error :: term().

-callback resend_message(Handle, Msg) -> ok | {cancel, Reason :: term()} | Error when
      Handle :: term(),
      Msg :: iodata(),
      Error :: term().

-optional_callbacks([{send_message,3},{resend_message,2}]).
