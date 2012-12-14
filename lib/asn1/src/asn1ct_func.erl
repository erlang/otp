%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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
%%

-module(asn1ct_func).
-export([start_link/0,need/1,call/3,generate/1]).
-export([init/1,handle_call/3,handle_cast/2,terminate/2]).

start_link() ->
    {ok,Pid} = gen_server:start_link(?MODULE, [], []),
    put(?MODULE, Pid),
    ok.

call(M, F, Args) ->
    MFA = {M,F,length(Args)},
    need(MFA),
    asn1ct_gen:emit([F,"(",call_args(Args, ""),")"]).

need(MFA) ->
    asn1ct_rtt:assert_defined(MFA),
    cast({need,MFA}).

generate(Fd) ->
    req({generate,Fd}),
    erase(?MODULE),
    ok.

req(Req) ->
    gen_server:call(get(?MODULE), Req, infinity).

cast(Req) ->
    gen_server:cast(get(?MODULE), Req).

%%% Internal functions.

-record(st, {used}).

init([]) ->
    St = #st{used=gb_sets:empty()},
    {ok,St}.

handle_cast({need,MFA}, #st{used=Used0}=St) ->
    case gb_sets:is_member(MFA, Used0) of
	false ->
	    Used = pull_in_deps(gb_sets:singleton(MFA), Used0),
	    {noreply,St#st{used=Used}};
	true ->
	    {noreply,St}
    end.

handle_call({generate,Fd}, _From, #st{used=Used}=St) ->
    generate(Fd, Used),
    {stop,normal,ok,St}.

terminate(_, _) ->
    ok.

call_args([A|As], Sep) ->
    [Sep,A|call_args(As, ", ")];
call_args([], _) -> [].

generate(Fd, Used0) ->
    Used1 = gb_sets:to_list(Used0),
    Used = sofs:set(Used1, [mfa]),
    Code = sofs:relation(asn1ct_rtt:code(), [{mfa,code}]),
    Funcs0 = sofs:image(Code, Used),
    Funcs = sofs:to_external(Funcs0),
    io:put_chars(Fd, Funcs).

pull_in_deps(Ws0, Used0) ->
    case gb_sets:is_empty(Ws0) of
	true ->
	    Used0;
	false ->
	    {MFA,Ws1} = gb_sets:take_smallest(Ws0),
	    Used = gb_sets:add(MFA, Used0),
	    Needs = asn1ct_rtt:dependencies(MFA),
	    Ws = update_worklist(Needs, Used, Ws1),
	    pull_in_deps(Ws, Used)
    end.

update_worklist([H|T], Used, Ws) ->
    case gb_sets:is_member(H, Used) of
	false ->
	    update_worklist(T, Used, gb_sets:add(H, Ws));
	true ->
	    update_worklist(T, Used, Ws)
    end;
update_worklist([], _, Ws) -> Ws.
