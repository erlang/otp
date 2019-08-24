%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Main API module for Event Tracer
%%----------------------------------------------------------------------
%%
%% The Event Tracer (et) uses the built-in trace mechanism in Erlang and
%% provides tools for collection and graphical viewing of trace data.
%%
%% et_collector
%%
%%   An Erlang trace client which collects and stores trace data.
%%   Provides hooks for trace data filtering and group communication
%%   between processes (such as et_viewer-processes). The trace data
%%   is preferably traced et-module calls, but may in fact be any
%%   Erlang trace data.
%%
%%   It do also provide functionality for global control of trace
%%   pattern settings. If used, the one et_collector-process is
%%   registered globally. On all connected Erlang nodes, it starts an
%%   Erlang tracer process which sends its trace data to a local port
%%   (the port number is generated). On the node where the global
%%   et_collector is running, the corresponding Erlang trace client
%%   processes are started (one for each node), configured to
%%   transform the trace data into event records and possibly hand
%%   them over to the collector. Whenever new nodes are
%%   (dis)connected, this is monitored and new tracer/client pair of
%%   processes are automatically started and eventually the trace
%%   pattern are set on these nodes.
%%
%%   Trace data can also be loaded from one or more files.
%%
%% et_viewer
%%
%%   A graphical sequence chart tool. It is connected to a
%%   et_collector-process, which it polls regulary for more trace
%%   events to display. Before the event is displayed a user defined
%%   filter function is applied in order to skip, accept as is or
%%   transform the event. Several et_viewer-processes may share the
%%   same et_collector in order to provide different simultaneous
%%   views of the same trace data.
%%   
%% et_contents_viewer
%%
%%   A graphical tool which displays a detailed view of one trace
%%   event. Normally started from the et_viewer.
%%   
%% et_selector
%%
%%   A library module with low level functions for activation of
%%   Erlang trace patterns. It do also implement a default filter
%%   function which transforms the raw trace data into the event
%%   record data structure that is used as internal format by the rest
%%   of the application. Customized transform functions can be
%%   alternatively be used (by et_viewer, et_contents_viewer and
%%   et_collector), if needed.
%%   
%% et
%%
%%   A library module with a few event report functions that are
%%   intended to be invoked from other applications. The functions are
%%   extremely light weight as they do nothing besides returning an
%%   atom. These functions are specifically designed to be traced
%%   for. The global trace patterns in et_collector defaults to trace
%%   on these functions.
%%----------------------------------------------------------------------

-module(et).

-export([
	 trace_me/4, phone_home/4, report_event/4,
	 trace_me/5, phone_home/5, report_event/5
        ]).

%%----------------------------------------------------------------------
%% Reports an event, such as a message
%%
%% trace_me(DetailLevel, FromTo, Label, Contents) -> hopefully_traced
%% trace_me(DetailLevel, From, To, Label, Contents) -> hopefully_traced
%% report_event(DetailLevel, FromTo, Label, Contents) -> hopefully_traced
%% report_event(DetailLevel, From, To, Label, Contents) -> hopefully_traced
%% phone_home(DetailLevel, FromTo, Label, Contents) -> hopefully_traced
%% phone_home(DetailLevel, From, To, Label, Contents) -> hopefully_traced
%%
%% DetailLevel = integer(X) when X =< 0, X >= 100
%% From        = actor()
%% To          = actor()
%% FromTo      = actor()
%% Label       = atom() | string() | term()
%% Contents    = [{Key, Value}] | term()
%%
%% actor()  = term()
%%
%% These functions are intended to be invoked at strategic places
%% in user applications in order to enable simplified tracing.
%% The functions are extremely light weight as they do nothing
%% besides returning an atom. These functions are designed for
%% being traced. The global tracing mechanism in et_collector
%% defaults to set its trace pattern to these functions.
%%   
%% The label is intended to provide a brief summary of the event.
%% A simple tag would do.
%%
%% The contents can be any term but in order to simplify
%% post processing of the traced events, a plain list
%% of {Key, Value} tuples is preferred.
%%
%% Some events, such as messages, are directed from some actor to another.
%% Other events (termed actions) may be undirected and only have one actor.
%%----------------------------------------------------------------------

trace_me(DetailLevel, FromTo, Label, Contents)
  when is_integer(DetailLevel) ->
    ?MODULE:trace_me(DetailLevel, FromTo, FromTo, Label, Contents).

trace_me(DetailLevel, _From, _To, _Label, _Contents)
  when is_integer(DetailLevel) ->
    hopefully_traced.

phone_home(DetailLevel, FromTo, Label, Contents) ->
    %% N.B External call
    ?MODULE:trace_me(DetailLevel, FromTo, FromTo, Label, Contents).

phone_home(DetailLevel, From, To, Label, Contents) ->
    %% N.B External call
    ?MODULE:trace_me(DetailLevel, From, To, Label, Contents).

report_event(DetailLevel, FromTo, Label, Contents) ->
    %% N.B External call
    ?MODULE:trace_me(DetailLevel, FromTo, FromTo, Label, Contents).

report_event(DetailLevel, From, To, Label, Contents)
  when is_integer(DetailLevel) ->
    %% N.B External call
    ?MODULE:trace_me(DetailLevel, From, To, Label, Contents).

