%%%-------------------------------------------------------------------
%%% File    : odbc_debug.erl
%%% Author  : Ingela Anderton Andin <ingela@erix.ericsson.se>
%%% Description : Issuse standard tracing on an odbc connection process
%%%
%%% Created : 12 Dec 2003 by Ingela Anderton Andin <ingela@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(odbc_debug).

-export([trace_odbc/2]).

%%%========================================================================
%%% Debug functions
%%%========================================================================

%%--------------------------------------------------------------------------
%% trace_odbc(Process, OnOff, <Level>) -> ok
%%	Process  - pid() | Name | {global, Name} | {Name, Node} 
%%	OnOff   - on | off
%%      Level   - exported | all
%% Description: Turns on tracing of messages sent and recived by
%%              the server <Process> and tracing on all, or all exported 
%%              functions, according to level <Level>, in this module.
%%              Result will be printed on stdout.
%%--------------------------------------------------------------------------
trace_odbc(Process, OnOff) ->
    trace_odbc(Process, OnOff, exported).

trace_odbc(Process, on, exported) ->
    dbg:tracer(),
    dbg:tp(odbc, [{'_', [], [{return_trace}]}]),
    dbg:p(Process, [call, m]),
    ok; 

trace_odbc(Process, on, all) ->
    dbg:tracer(),
    dbg:tpl(odbc, [{'_', [], [{return_trace}]}]),
    dbg:p(Process, [call, m]),
    ok;

trace_odbc(_Process, off, _Level) ->
    dbg:stop(),
    ok.
