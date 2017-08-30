%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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

-module(asn1ct_func).
-export([start_link/0,need/1,call/3,call_gen/3,call_gen/4,
	 generate/1,is_used/1]).
-export([init/1,handle_call/3,handle_cast/2,terminate/2]).

start_link() ->
    {ok,Pid} = gen_server:start_link(?MODULE, [], []),
    put(?MODULE, Pid),
    ok.

call(M, F, Args) ->
    A = length(Args),
    MFA = {M,F,A},
    need(MFA),
    case M of
	binary ->
	    asn1ct_gen:emit(["binary:",F,"(",call_args(Args, ""),")"]);
	_ ->
	    asn1ct_gen:emit([F,"(",call_args(Args, ""),")"])
    end.

need({binary,_,_}) ->
    ok;
need({erlang,_,_}) ->
    ok;
need(MFA) ->
    asn1ct_rtt:assert_defined(MFA),
    cast({need,MFA}).

call_gen(Prefix, Key, Gen, Args) when is_function(Gen, 2) ->
    F = req({gen_func,Prefix,Key,Gen}),
    asn1ct_gen:emit([{asis,F},"(",call_args(Args, ""),")"]).

call_gen(Prefix, Key, Gen) when is_function(Gen, 2) ->
    req({gen_func,Prefix,Key,Gen}).

generate(Fd) ->
    do_generate(Fd),
    Used0 = req(get_used),
    erase(?MODULE),
    Used = sofs:set(Used0, [mfa]),
    Code = sofs:relation(asn1ct_rtt:code(), [{mfa,code}]),
    Funcs0 = sofs:image(Code, Used),
    Funcs = sofs:to_external(Funcs0),
    ok = file:write(Fd, Funcs).

is_used({_,_,_}=MFA) ->
    req({is_used,MFA}).


req(Req) ->
    gen_server:call(get(?MODULE), Req, infinity).

cast(Req) ->
    gen_server:cast(get(?MODULE), Req).

%%% Internal functions.

-record(st, {used,				%Used functions
	     gen,				%Dynamically generated functions
	     gc=1				%Counter for generated functions
	    }).

init([]) ->
    St = #st{used=gb_sets:empty(),gen=gb_trees:empty()},
    {ok,St}.

handle_cast({need,MFA}, #st{used=Used0}=St) ->
    case gb_sets:is_member(MFA, Used0) of
	false ->
	    Used = pull_in_deps(gb_sets:singleton(MFA), Used0),
	    {noreply,St#st{used=Used}};
	true ->
	    {noreply,St}
    end.

handle_call(get_used, _From, #st{used=Used}=St) ->
    {stop,normal,gb_sets:to_list(Used),St};
handle_call(get_gen, _From, #st{gen=G0}=St) ->
    {L,G} = do_get_gen(gb_trees:to_list(G0), [], []),
    {reply,L,St#st{gen=gb_trees:from_orddict(G)}};
handle_call({gen_func,Prefix,Key,GenFun}, _From, #st{gen=G0,gc=Gc0}=St) ->
    case gb_trees:lookup(Key, G0) of
	none ->
	    Name = list_to_atom(Prefix ++ integer_to_list(Gc0)),
	    Gc = Gc0 + 1,
	    G = gb_trees:insert(Key, {Name,GenFun}, G0),
	    {reply,Name,St#st{gen=G,gc=Gc}};
	{value,{Name,_}} ->
	    {reply,Name,St}
    end;
handle_call({is_used,MFA}, _From, #st{used=Used}=St) ->
    {reply,gb_sets:is_member(MFA, Used),St}.


terminate(_, _) ->
    ok.

call_args([A|As], Sep) ->
    [Sep,A|call_args(As, ", ")];
call_args([], _) -> [].

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

do_get_gen([{_,{_,done}}=Keep|T], Gacc, Kacc) ->
    do_get_gen(T, Gacc, [Keep|Kacc]);
do_get_gen([{K,{Name,_}=V}|T], Gacc, Kacc) ->
    do_get_gen(T, [V|Gacc], [{K,{Name,done}}|Kacc]);
do_get_gen([], Gacc, Kacc) ->
    {lists:sort(Gacc),lists:reverse(Kacc)}.

do_generate(Fd) ->
    case req(get_gen) of
	[] ->
	    ok;
	[_|_]=Gen ->
	    _ = [begin
		     ok = file:write(Fd, "\n"),
		     GenFun(Fd, Name)
		 end || {Name,GenFun} <- Gen],
	    do_generate(Fd)
    end.
