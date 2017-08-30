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


%% Description  : Simple event-based processor (front-end to xmerl_scanner)


-module(xmerl_simple).

-export([file/2,
	 string/2]).

-include("xmerl.hrl").

-record(state, {content_acc = [],
		attr_acc = [],
		content_stack = [],
		attr_stack = []}).

file(Fname, Opts) ->
    Opts1 = scanner_options(Opts),
    xmerl_scan:file(Fname, Opts1).

string(Str, Opts) ->
    Opts1 = scanner_options(Opts),
    xmerl_scan:string(Str, Opts1).

scanner_options(Opts) ->
    EventS = #state{},
    scanner_options(Opts,
		    [{event_fun, fun event/2, EventS},
		     {acc_fun, fun(_, Acc, S) -> {Acc,S} end},
		     {close_fun, fun close/1}]).

scanner_options([H|T], Opts) ->
    case catch keyreplace(H, 1, Opts) of
	false ->
	    scanner_options(T, [H|Opts]);
	NewOpts ->
	    scanner_options(T, NewOpts)
    end;
scanner_options([], Opts) ->
    Opts.

keyreplace(X, Pos, [H|T]) when element(Pos, X) == element(Pos, H) ->
    [X|T];
keyreplace(X, Pos, [H|T]) ->
    [H|keyreplace(X, Pos, T)];
keyreplace(_X, _Pos, []) ->
    throw(false).


close(S) ->
    ES = xmerl_scan:event_state(S),
    #state{attr_stack = [],
	   content_stack = [],
	   %% attr_acc may contain document attributes
	   content_acc = Content} = ES,
    lists:reverse(Content).

event(#xmerl_event{event = started, data = #xmlElement{}}, S) ->
    #state{content_acc = CAcc,
	   attr_acc = AAcc,
	   content_stack = CSt,
	   attr_stack = ASt} = ES = xmerl_scan:event_state(S),
    xmerl_scan:event_state(ES#state{content_acc = [],
				    attr_acc = [],
				    content_stack = [CAcc | CSt],
				    attr_stack = [AAcc | ASt]}, S);

event(#xmerl_event{event = ended, data = #xmlElement{name = Name}}, S) ->
    #state{content_acc = CAcc,
	   attr_acc = AAcc,
	   content_stack = [PrevCAcc | CSt],
	   attr_stack = [PrevAAcc | ASt]} = ES = xmerl_scan:event_state(S),
    Simple = {Name, lists:reverse(AAcc), lists:reverse(CAcc)},
    xmerl_scan:event_state(ES#state{content_acc = [Simple|PrevCAcc],
				    attr_acc = PrevAAcc,
				    content_stack = CSt,
				    attr_stack = ASt}, S);

event(#xmerl_event{event = ended, data = #xmlAttribute{name = Name,
						       value = Value}}, S) ->
    #state{attr_acc = AAcc} = ES = xmerl_scan:event_state(S),
    Simple = {Name, Value},
    xmerl_scan:event_state(ES#state{attr_acc = [Simple|AAcc]}, S);

event(#xmerl_event{event = ended, data = #xmlText{value = Text}}, S) ->
    #state{content_acc = CAcc} = ES = xmerl_scan:event_state(S),
    xmerl_scan:event_state(ES#state{content_acc = [Text|CAcc]}, S);

event(_E, S) ->
    S.
