%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2019. All Rights Reserved.
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

-module(snmp_verbosity).

-include_lib("stdlib/include/erl_compile.hrl").

-export([print/4,print/5,printc/4,validate/1]).

-export([process_args/2]).

print(silence,_Severity,_Format,_Arguments) ->
    ok;
print(Verbosity,Severity,Format,Arguments) ->
    print1(printable(Verbosity,Severity),Format,Arguments).


print(silence,_Severity,_Module,_Format,_Arguments) ->
    ok;
print(Verbosity,Severity,Module,Format,Arguments) ->
    print1(printable(Verbosity,Severity),Module,Format,Arguments).


printc(silence,_Severity,_Format,_Arguments) ->
    ok;
printc(Verbosity,Severity,Format,Arguments) ->
    print2(printable(Verbosity,Severity),Format,Arguments).


print1(false,_Format,_Arguments) -> ok;
print1(Verbosity,Format,Arguments) ->
    V = image_of_verbosity(Verbosity),
    S = image_of_sname(get(sname)),
    A = process_args(Arguments, []),
    (catch io:format("*** [~s] SNMP ~s ~s *** ~n" 
		     "   " ++ Format ++ "~n",
		     [timestamp(), S, V | A])).

print1(false,_Module,_Format,_Arguments) -> ok;
print1(Verbosity,Module,Format,Arguments) ->
    V = image_of_verbosity(Verbosity),
    S = image_of_sname(get(sname)),
    A = process_args(Arguments, []),
    (catch io:format("*** [~s] SNMP ~s ~s ~s *** ~n" 
		     "   " ++ Format ++ "~n",
		     [timestamp(), S, Module, V | A])).


print2(false,_Format,_Arguments) -> ok;
print2(_Verbosity,Format,Arguments) ->
    A = process_args(Arguments, []),
    (catch io:format(Format ++ "~n",A)).


timestamp() ->
    snmp_misc:formated_timestamp().

process_args([], Acc) ->
    lists:reverse(Acc);
process_args([{vapply, {M,F,A}}|T], Acc) 
  when is_atom(M) andalso is_atom(F) andalso is_list(A) ->
    process_args(T, [(catch apply(M,F,A))|Acc]);
process_args([H|T], Acc) ->
    process_args(T, [H|Acc]).


%% printable(Verbosity,Severity)
printable(info,info)      -> info;
printable(log,info)       -> info;
printable(log,log)        -> log;
printable(debug,info)     -> info;
printable(debug,log)      -> log;
printable(debug,debug)    -> debug;
printable(trace,V)        -> V;
printable(_Verb,_Sev)     -> false.


image_of_verbosity(info)  -> "INFO";
image_of_verbosity(log)   -> "LOG";
image_of_verbosity(debug) -> "DEBUG";
image_of_verbosity(trace) -> "TRACE";
image_of_verbosity(_)     -> "".

%% ShortName
image_of_sname(ma)        -> "MASTER-AGENT";
image_of_sname(maw)       -> io_lib:format("MASTER-AGENT-worker(~p)",[self()]);
image_of_sname(madis)     -> io_lib:format("MASTER-AGENT-discovery_inform_sender(~p)",
					   [self()]);
image_of_sname(mais)      -> io_lib:format("MASTER-AGENT-inform_sender(~p)",
					   [self()]);
image_of_sname(mats)      -> io_lib:format("MASTER-AGENT-trap_sender(~p)",
					   [self()]);
image_of_sname(maph)      -> io_lib:format("MASTER-AGENT-pdu_handler(~p)",
					   [self()]);
image_of_sname(sa)        -> "SUB-AGENT";
image_of_sname(saw)       -> io_lib:format("SUB-AGENT-worker(~p)",[self()]);
image_of_sname(sais)      -> io_lib:format("SUB-AGENT-inform_sender(~p)",
					   [self()]);
image_of_sname(sats)      -> io_lib:format("SUB-AGENT-trap_sender(~p)",
					   [self()]);
image_of_sname(saph)      -> io_lib:format("SUB-AGENT-pdu_handler(~p)",
					   [self()]);
image_of_sname(nif)       -> "A-NET-IF";
image_of_sname(ldb)       -> "A-LOCAL-DB";
image_of_sname(ns)        -> "A-NOTE-STORE";
image_of_sname(ss)        -> "A-SYMBOLIC-STORE";
image_of_sname(asup)      -> "A-SUPERVISOR";
image_of_sname(ms)        -> "A-MIB-SERVER";
image_of_sname(tcs)       -> "A-TARGET-CACHE-SERVER";
image_of_sname(conf)      -> "A-CONF";

image_of_sname(abs)       -> "A-BKP";
image_of_sname(albs)      -> "A-LDB-BKP";
image_of_sname(ambs)      -> "A-MS-BKP";
image_of_sname(asbs)      -> "A-SS-BKP";
image_of_sname(mcbs)      -> "M-C-BKP";

image_of_sname(mse)       -> "M-SERVER";
image_of_sname(msew)      -> io_lib:format("M-SERVER-worker(~p)", [self()]);
image_of_sname(mns)       -> "M-NOTE-STORE";
image_of_sname(mnif)      -> "M-NET-IF";
image_of_sname(mnifl)     -> "M-NET-IF-LOGGER";
image_of_sname(mnifw)     -> io_lib:format("M-NET-IF-worker(~p)", [self()]);
image_of_sname(mconf)     -> "M-CONF";

image_of_sname(lc)        -> io_lib:format("LOG-CONVERTER(~p)", [self()]);

image_of_sname(mgr)       -> "MGR";
image_of_sname(mgr_misc)  -> "MGR_MISC";

image_of_sname(undefined) -> "";
image_of_sname(N) when is_list(N) -> N; % Used in testing
image_of_sname(N)         -> lists:flatten(io_lib:format("~p", [N])).


validate(info)  -> info;
validate(log)   -> log;
validate(debug) -> debug;
validate(trace) -> trace;
validate(_)     -> silence.
