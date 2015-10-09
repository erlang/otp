%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: httpd_verbosity.erl,v 1.1 2008/12/17 09:53:34 mikpe Exp $
%%
-module(httpd_verbosity).

-include_lib("stdlib/include/erl_compile.hrl").

-export([print/4,print/5,printc/4,validate/1]).

print(silence,_Severity,_Format,_Arguments) ->
    ok;
print(Verbosity,Severity,Format,Arguments) ->
    print1(printable(Verbosity,Severity),Format,Arguments).


print(silence,_Severity,_Module,_Format,_Arguments) ->
    ok;
print(Verbosity,Severity,Module,Format,Arguments) ->
    print1(printable(Verbosity,Severity),Module,Format,Arguments).


printc(silence,Severity,Format,Arguments) ->
    ok;
printc(Verbosity,Severity,Format,Arguments) ->
    print2(printable(Verbosity,Severity),Format,Arguments).


print1(false,_Format,_Arguments) -> ok;
print1(Verbosity,Format,Arguments) ->
    V = image_of_verbosity(Verbosity),
    S = image_of_sname(get(sname)),
    io:format("** HTTPD ~s ~s: " ++ Format ++ "~n",[S,V]++Arguments).

print1(false,_Module,_Format,_Arguments) -> ok;
print1(Verbosity,Module,Format,Arguments) ->
    V = image_of_verbosity(Verbosity),
    S = image_of_sname(get(sname)),
    io:format("** HTTPD ~s ~s ~s: " ++ Format ++ "~n",[S,Module,V]++Arguments).


print2(false,_Format,_Arguments) -> ok;
print2(_Verbosity,Format,Arguments) ->
    io:format(Format ++ "~n",Arguments).


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
image_of_sname(acc)           -> "ACCEPTOR";
image_of_sname(acc_sup)       -> "ACCEPTOR_SUP";
image_of_sname(auth)          -> "AUTH";
image_of_sname(man)           -> "MANAGER";
image_of_sname(misc_sup)      -> "MISC_SUP";
image_of_sname(sec)           -> "SECURITY";
image_of_sname(P) when pid(P) -> io_lib:format("REQUEST_HANDLER(~p)",[P]);
image_of_sname(undefined)     -> "";
image_of_sname(V)             -> io_lib:format("~p",[V]).


validate(info)  -> info;
validate(log)   -> log;
validate(debug) -> debug;
validate(trace) -> trace;
validate(_)     -> silence.
