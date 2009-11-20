%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
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
%% Author: Lennart Öhman, lennart.ohman@st.se
%%
%% This module implements the meta tracer process belonging to the
%% runtime component. Its main purpose is to write the ti-file (traceinformation).
%% The ti-file contains translations between process id:s and what ever "you"
%% want to read in the merged and formatted logfile.
%% This process interacts with the runtime component process.
%%
%% Currently it handles the following types of ti-files:
%%   Plain raw, binary log.
%%   Relay to other inviso_rt_meta process on another node.
%%
%% The TI file will be on binary format and each entry is:
%%   <<LengthIndicator:32, {Pid,Alias,Op,NowStamp} >>
%%       Pid=pid(), or if OP==unalias pid()|any_other_than_pid()
%%       Op=alias|unalias
%% -----------------------------------------------------------------------------
-module(inviso_rt_meta).

%% -----------------------------------------------------------------------------
%% API exports.
%% -----------------------------------------------------------------------------

-export([start/2,start/5]).
-export([stop/1,suspend/1]).
-export([init_tpm/5,init_tpm/8]).
-export([tpm/5,tpm/6,tpm/9,tpm_tracer/5,tpm_tracer/6,tpm_tracer/9]).
-export([tpm_ms/6,tpm_ms_tracer/6,ctpm_ms/5,ctpm/4]).
-export([local_register/1,global_register/1]).
-export([remove_local_register/1,remove_global_register/1]).

-export([write_ti/1]).

-export([get_tracer/0,tpm_ms/5,tpm_ms_tracer/5,list_tpm_ms/3,ctpm_ms/4]).

-export([metacast_call/5,metacast_return_from/6]).
-export([get_state/1]).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Internal exports.
%% -----------------------------------------------------------------------------

-export([init/6]).
-export([init_std_publld/2,clean_std_publld/1]).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Constants.
%% -----------------------------------------------------------------------------

-define(NAMED_MS_TAB,inviso_rt_meta_named_ms).

%% -----------------------------------------------------------------------------


%% =============================================================================
%% Exported API (Meant to be used by a runtime component).
%% =============================================================================

%% start(TiData,Tracer)={ok,Pid} | {error,Reason}
%% start(TiData,Tracer,InitPublLDmfa,RemovePublLDmfa,CleanPublLDmf)=
%%     {ok,Pid} | {error,Reason}
%%   TiData={file,FileName}|{relay,Node}
%%   Tracer=pid()|port()
%%   FileName=string()
%%   InitPublLDmfa={Mod,Func,ArgList}
%%   RemovePublLDmf={Mod,Func} | void
%%     RemovePublLDmf(PublLD)->nothing significant.
%%     These functions are called to create and destroy the public loopdata
%%     structure available to the meta-trace CallFunc and ReturnFunc.
%%   CleanPublLDmf={Mod,Func}
%%     This function will periodically be called to clean the public LD from
%%     pending meta-trace messages waiting for a corresponding return_from
%%     message.
%%
%% Starts a meta-tracer process, opening the ti-file specified in TiData. PublLD
%% is used to communicate data, typically between a call and return_from.
%% If no special initialization function is specified a standard one is used.
%% Note that the meta tracer function must know "who" is the regular tracer
%% (process or port). This because it must be possible to append {tracer,Tracer}
%% in meta match specs.
start(TiData,Tracer) ->
    Pid=spawn_link(?MODULE,
		   init,
		   [self(),
		    TiData,
		    Tracer,
		    {?MODULE,init_std_publld,[2,[]]},
		    void,
		    {?MODULE,clean_std_publld}]),
    wait_for_reply(Pid).
start(TiData,Tracer,InitPublLDmfa,RemovePublLDmf,CleanPublLDmf) ->
    Pid=spawn_link(?MODULE,
		   init,
		   [self(),TiData,Tracer,InitPublLDmfa,RemovePublLDmf,CleanPublLDmf]),
    wait_for_reply(Pid).

wait_for_reply(Pid) ->
    receive
	{Pid,ok} ->
	    {ok,Pid};
	{Pid,{error,Reason}} ->
	    {error,Reason}
    after
	10000 ->                             % After very long time.
	    exit(Pid,kill),                  % It must be hanging.
	    {error,time_out}
    end.
%% -----------------------------------------------------------------------------

%% stop(Pid)=ok
%%   Pid=Adders to the meta tracer, pid().
%% Shutsdown the metatracer.
stop(Pid) ->
    Pid ! {stop,self()},
    ok.
%% -----------------------------------------------------------------------------

%% suspend(Pid)=ok
%%   Pid=Adders to the meta tracer, pid().
%% Suspends the meta tracer by removing all meta trace patterns.
suspend(Pid) ->
    Pid ! {suspend,self()},
    ok.
%% -----------------------------------------------------------------------------

%% init_tpm(Pid,Mod,Func,Arity,CallFunc)=
%% init_tpm(Pid,Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc)=ok|{error,Reason}.
%%   Pid=Address to meta tracer process, pid().
%%   Mod,Func=Pointing out the function which shall be meta traced, atom().
%%   Arity=As above, integer().
%%   InitFunc,RemoveFunc={Module,Function}|fun(), functions being called when
%%     to initialize the public loopdata structure, and to reset it.
%%       InitFunc(Mod,Func,Arity,PublLD)->{ok,NewPublLD,Output}
%%         Supposed to initialize whatever needs to be done before
%%         handling any incoming meta-trace message for the Mod:Func/Arity.
%%       RemoveFunc(Mod,Func,Arity,PublLD)->{ok,NewPublLD}
%%         Called when meta tracing of Mod:Func/Arity is stopped. It is supposed
%%         to clear datastructures away from the PublLD.
%% Initializes the public loopdata for this function. Note that we can not use wildcards
%% here (even if it is perfectly legal in Erlang). It also sets the CallFunc and
%% ReturnFunc for the meta traced function. The function is hence ready to be
%% meta traced with either tpm/5 or tpm_ms/5.
%% This function is synchronous, waiting for a reply from the meta server.
init_tpm(Pid,Mod,Func,Arity,CallFunc) ->
    init_tpm(Pid,Mod,Func,Arity,void,CallFunc,void,void).
init_tpm(Pid,Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc) ->
    send_wait(Pid,
	      {init_tpm,{Mod,Func,Arity},InitFunc,CallFunc,ReturnFunc,RemoveFunc}).
%% -----------------------------------------------------------------------------

%% tpm(Pid,Mod,Func,Arity,MatchSpec)={ok,N}|{error,Reason}
%% tpm(Pid,Mod,Func,Arity,MatchSpec,CallFunc)={ok,N}|{error,Reason}
%% tpm(Pid,Mod,Func,Arity,MatchSpec,InitFunc,CallFunc,ReturnFunc,RemoveFunc)=
%%   Pid=Address to meta tracer process, pid().
%%   Mod,Func=Pointing out the function which shall be meta traced, atom().
%%   Arity=As above, integer().
%%   MatchSpec=List of match specification, possibly empty. Remember {return_trace}
%%     if expecting return_from messages.
%%   InitFunc,CallFunc,ReturnFunc,RemoveFunc={Module,Function}|fun(),
%%     functions being called when these functions are called by the meta trace
%%     server at certain events.
%%       CallFunc(CallingPid,ActualArgList,PublLD)->{ok,NewPrivLD,Output}
%%       ReturnFunc(CallingPid,ReturnValue,PublLD)->{ok,NewPrivLD,Output}
%%         When a call respectively return_from trace message arrives for the meta
%%         traced function, the corresponding function is called.
%%         The ReturnFunc must handle the fact that a return_from message arrives
%%         for a call which was never noticed. This because the message queue of the
%%         meta tracer may have been emptied.
%%   Reason=badarg | 
%%   Output=Characters to be written to the ti-file, bin() | 'void'
%% The tpm/5 function simply starts meta tracing for the function. It must
%% previously have been initialized.
%% tpm/6 & /9 initializes the function and starts meta tracing.
tpm(Pid,Mod,Func,Arity,MatchSpec)
  when is_atom(Mod),is_atom(Func),is_integer(Arity),is_list(MatchSpec),Mod/='_',Func/='_'->
    send_wait(Pid,{tpm,{Mod,Func,Arity,MatchSpec}});
tpm(_,_,_,_,_) ->
    {error,badarg}.

tpm(Pid,Mod,Func,Arity,MatchSpec,CallFunc) ->
    tpm(Pid,Mod,Func,Arity,MatchSpec,void,CallFunc,void,void).

tpm(Pid,Mod,Func,Arity,MatchSpec,InitFunc,CallFunc,ReturnFunc,RemoveFunc)
  when is_atom(Mod),is_atom(Func),is_integer(Arity),is_list(MatchSpec),Mod/='_',Func/='_' ->
    send_wait(Pid,{tpm,{Mod,Func,Arity,MatchSpec},InitFunc,CallFunc,ReturnFunc,RemoveFunc});
tpm(_,_,_,_,_,_,_,_,_) ->
    {error,badarg}.
%% -----------------------------------------------------------------------------

%% Same as tpm/X but the meta tracer will automatically append {tracer,Tracer}
%% to the enable list in a {trace,Disable,Enable} match spec action term.
tpm_tracer(Pid,Mod,Func,Arity,MatchSpec)
  when is_atom(Mod),is_atom(Func),is_integer(Arity),is_list(MatchSpec),Mod/='_',Func/='_'->
    send_wait(Pid,{tpm_tracer,{Mod,Func,Arity,MatchSpec}});
tpm_tracer(_,_,_,_,_) ->
    {error,badarg}.

tpm_tracer(Pid,Mod,Func,Arity,MatchSpec,CallFunc) ->
    tpm_tracer(Pid,Mod,Func,Arity,MatchSpec,void,CallFunc,void,void).

tpm_tracer(Pid,Mod,Func,Arity,MatchSpec,InitFunc,CallFunc,ReturnFunc,RemoveFunc)
  when is_atom(Mod),is_atom(Func),is_integer(Arity),is_list(MatchSpec),Mod/='_',Func/='_' ->
    send_wait(Pid,{tpm_tracer,
		   {Mod,Func,Arity,MatchSpec},
		   InitFunc,CallFunc,ReturnFunc,RemoveFunc});
tpm_tracer(_,_,_,_,_,_,_,_,_) ->
    {error,badarg}.
%% -----------------------------------------------------------------------------

%% tpm_ms(Pid,Mod,Func,Arity,MSname,MS)={ok,N}|{error,Reason}
%%   Pid=Address to meta tracer process, pid().
%%   Mod,Func=Pointing out the function to which we shall add a match-spec., atom().
%%   Arity=As above, integer().
%%   MSname=A name to be used if this MS shall be removed later. term().
%%   MatchSpec=List of match specification, Remember {return_trace}
%%     if expecting return_from messages.
%% This function adds a list of match-specs to the already existing ones. It
%% uses an internal database to keep track of existing match-specs. If the
%% match-spec does not result in any meta traced functions (for whatever reason),
%% the MS is not saved in the database. The previously known match-specs are
%% not removed.
tpm_ms(Pid,Mod,Func,Arity,MSname,MS) ->
    send_wait(Pid,{tpm_ms,{Mod,Func,Arity},MSname,MS}).
%% -----------------------------------------------------------------------------

%% Same as tpm_ms/6 but the meta tracer will automatically append {tracer,Tracer}
%% to the enable list in a {trace,Disable,Enable} match spec action term.
tpm_ms_tracer(Pid,Mod,Func,Arity,MSname,MS) ->
    send_wait(Pid,{tpm_ms_tracer,{Mod,Func,Arity},MSname,MS}).
%% -----------------------------------------------------------------------------

%% ctpm_ms(Pid,Mod,Func,Arity)=ok
%%
%% Removes a names match-spec from the meta traced function. Note that is never
%% a fault to remove an MS. Not even from a function which is non existant.
ctpm_ms(Pid,Mod,Func,Arity,MSname) ->
    send_wait(Pid,{ctpm_ms,{Mod,Func,Arity},MSname}).
%% -----------------------------------------------------------------------------

%% Quick versions for erlang:register/2 which also uses a default CallFunc
%% and a default ReturnFunc.
local_register(Pid) ->
    Res1=tpm(Pid,
	     erlang,register,2,[{'_',[],[{exception_trace}]}],
	     fun metafunc_init/4,fun local_register_call/3,
	     fun local_register_return/3,void),
    Res2=tpm(Pid,
	     erlang,unregister,1,[],
	     void,fun local_unregister_call/3,void,void),
    {Res1,Res2}.
%% -----------------------------------------------------------------------------

%% Quick version for global:register_name/2, /3.
global_register(Pid) ->
    Res1=tpm(Pid,global,handle_call,3,[{[{register,'_','_','_'},'_','_'],[],[]}],
	void,fun global_register_call/3,void,void),
    Res2=tpm(Pid,global,delete_global_name,2,[],
	     void,fun global_unregister_call/3,void,void),
    {Res1,Res2}.
%% -----------------------------------------------------------------------------

%% ctpm(Pid,Mod,Func,Arity)=ok|{error,bad_mfa}
%%
%% Removes the meta trace pattern for the function, means stops generating output
%% for this function. The public LD may be cleared by the previously entered
%% RemoveFunc.
ctpm(Pid,Mod,Func,Arity) ->
    send_wait(Pid,{ctpm,{Mod,Func,Arity}}).
%% -----------------------------------------------------------------------------

%% remove_local_register(Pid)={Res1,Res2}
%%   Res1,Res2=ok|{error,Reason}
remove_local_register(Pid) ->
    Res1=ctpm(Pid,erlang,register,2),
    Res2=ctpm(Pid,erlang,unregister,1),
    {Res1,Res2}.
%% -----------------------------------------------------------------------------

%% remove_global_register(Pid)={Res1,Res2}
%%   Res1,Res2=ok|{error,Reason}
remove_global_register(Pid) ->
    Res1=ctpm(Pid,global,handle_call,3),
    Res2=ctpm(Pid,global,delete_global_name,2),
    {Res1,Res2}.
%% -----------------------------------------------------------------------------

%% Exported help functions which may be used in programming CallFunc and/or
%% ReturnFunc. Useful if the call is done on one node but must trigger the
%% start of something at other nodes.
metacast_call(Nodes,OrigPid,M,F,Args) ->
    multicast(Nodes,{trace_ts,OrigPid,call,{M,F,Args},void}),
    ok.

metacast_return_from(Nodes,OrigPid,M,F,Arity,Value) ->
    multicast(Nodes,{trace_ts,OrigPid,return_from,{M,F,Arity},Value,void}),
    ok.

multicast([Node|Rest],Msg) ->
    {?MODULE,Node} ! Msg,
    multicast(Rest,Msg);
multicast([],_) ->
    true.
%% -----------------------------------------------------------------------------

%% get_states(Pid)={ok,LD,PubLD}.
get_state(Pid) ->
    send_wait(Pid,get_state).
%% -----------------------------------------------------------------------------


send_wait(To,Msg) ->
    Ref=make_ref(),
    MRef=erlang:monitor(process,To),
    To ! {Msg,Ref,self()},
    receive
	{inviso_rt_meta_reply,Ref,Reply} ->
	    erlang:demonitor(MRef),
	    Reply;
	{'DOWN',MRef,_,_To,_Reason} ->
	    {error,no_metatracer}
    end.

reply(To,Ref,Reply) ->
    To ! {inviso_rt_meta_reply,Ref,Reply}.
%% -----------------------------------------------------------------------------

%% =============================================================================
%% Special API.
%% =============================================================================

%% write_ti(OutPut)=
%%   OutPut=binary()
%% Makes an extra entry into the trace information file (ti-file). This is useful
%% if a pid-alias association is learned in another way than through a meta traced
%% function call. Note that this API can only be used locally at the node in
%% question.
write_ti(OutPut) ->
    catch ?MODULE ! {write_ti,OutPut}.
%% -----------------------------------------------------------------------------


%% =============================================================================
%% API intended to be used on CallFuncs and RemoveFuncs.
%% =============================================================================

%% The reason there must be a special API for CallFuncs and RemoveFuncs are is
%% that those functions are executed inside *this* process context. Hence they
%% can not make function calls requiering this process to receive messages.

%% Returns the tracer used for regular tracing. The reason this is implemented
%% in this way is that this function is intended to be used in meta trace call-
%% back functions. And there we can not have message passing API:s to the meta
%% trace(!).
get_tracer() ->
    get(tracer).
%% -----------------------------------------------------------------------------

%% Function equivalent to inviso_rt:tpm_ms/6. This function can *only* be used
%% inside a CallFunc or a RemoveFunc.
tpm_ms(Mod,Func,Arity,MSname,MS) ->
    case check_mfarity_exists(Mod,Func,Arity) of
	yes ->                               % Ok, and args must be ok then also.
	    {ok,h_tpm_ms(Mod,Func,Arity,MSname,MS)};
	no ->
	    {error,not_initiated}
    end.
%% -----------------------------------------------------------------------------

tpm_ms_tracer(Mod,Func,Arity,MSname,MS) ->
    case check_mfarity_exists(Mod,Func,Arity) of
	yes ->                               % Ok, and args must be ok then also.
	    NewMS=add_tracer(MS,get_tracer()),
	    {ok,h_tpm_ms(Mod,Func,Arity,MSname,NewMS)};
	no ->
	    {error,not_initiated}
    end.
%% -----------------------------------------------------------------------------

%% Function that returns all MSname in use for Mod:Func/Arity
list_tpm_ms(Mod,Func,Arity) ->
    {ok,h_list_tpm_ms(Mod,Func,Arity)}.
%% -----------------------------------------------------------------------------

%% Function equivalent to inviso_rt:ctpm_ms/5. This function can *only* be used
%% inside a CallFunc or a RemoveFunc.
ctpm_ms(Mod,Func,Arity,MSname) ->
    h_ctpm_ms(Mod,Func,Arity,MSname),
    ok.
%% -----------------------------------------------------------------------------


%% =============================================================================
%% The server implemenation.
%% =============================================================================

init(Parent,TiData,Tracer,InitPublLDmfa,RemovePublLDmf,CleanPublLDmf) ->
    process_flag(priority,high),            % Since we may receive from many procs.
    register(?MODULE,self()),               % So we can act as relay receiver.
    case open_traceinfo_file(TiData) of
	{ok,TI} ->                          % The ti.-file.
	    TId=ets:new(?NAMED_MS_TAB,[named_table,set,protected]),
	    PublLD=do_init_publ_ld(InitPublLDmfa),
	    Parent ! {self(),ok},
	    put(tracer,Tracer),             % Uggly quick fix!
	    loop(Parent,
		 Tracer,
		 TI,
		 mk_new_ld(InitPublLDmfa,RemovePublLDmf,CleanPublLDmf,TId),
		 PublLD,
		 now());
	{error,Reason} ->
	    Parent ! {self(),{error,Reason}}
    end.
%% -----------------------------------------------------------------------------

loop(Parent,Tracer,TI,LD,PrevPublLD,PrevCleanTime) ->
    {PublLD,CleanTime}=throw_old_failed(get_cleanpublldmf_ld(LD),PrevPublLD,PrevCleanTime),
    receive
	{{init_tpm,{Mod,Func,Arity},InitFunc,CallFunc,ReturnFunc,RemoveFunc},Ref,Parent} ->
	    case check_mfarity_exists(Mod,Func,Arity) of
		no ->                       % Good then we can add it!
		    case check_tpm_args(Mod,Func,Arity) of
			true ->             % Args are ok.
			    {NewLD,NewPublLD}=
				h_init_tpm(Mod,Func,Arity,
					   InitFunc,CallFunc,ReturnFunc,RemoveFunc,
					   TI,LD,PublLD),
			    reply(Parent,Ref,ok),
			    loop(Parent,Tracer,TI,NewLD,NewPublLD,CleanTime);
			false ->            % Faulty arguments,
			    reply(Parent,Ref,{error,bad_mfa}),
			    loop(Parent,Tracer,TI,LD,PublLD,CleanTime)
		    end;
		yes ->                      % If it already exists, cant init again.
		    reply(Parent,Ref,{error,already_initiated}),
		    loop(Parent,Tracer,TI,LD,PublLD,CleanTime)
	    end;
	{{tpm,{Mod,Func,Arity,MS},InitFunc,CallFunc,ReturnFunc,RemoveFunc},Ref,Parent} ->
	    case check_mfarity_exists(Mod,Func,Arity) of
		no ->                       % Good then we can add it!
		    case check_tpm_args(Mod,Func,Arity) of
			true ->             % Args are ok.
			    {NewLD,NewPublLD,N}=
				h_tpm(Mod,Func,Arity,MS,
				      InitFunc,CallFunc,ReturnFunc,RemoveFunc,
				      TI,LD,PublLD),
			    reply(Parent,Ref,{ok,N}),
			    loop(Parent,Tracer,TI,NewLD,NewPublLD,CleanTime);
			false ->
			    reply(Parent,Ref,{error,bad_mfa}),
			    loop(Parent,Tracer,TI,LD,PublLD,CleanTime)
		    end;
		yes ->
		    reply(Parent,Ref,{error,already_initiated}),
		    loop(Parent,Tracer,TI,LD,PublLD,CleanTime)
	    end;
	{{tpm,{Mod,Func,Arity,MS}},Ref,Parent} ->
	    case check_mfarity_exists(Mod,Func,Arity) of
		yes ->                      % Ok, and args must be ok then also.
		    {NewLD,N}=h_tpm(Mod,Func,Arity,MS,LD),
		    reply(Parent,Ref,{ok,N}),
		    loop(Parent,Tracer,TI,NewLD,PublLD,CleanTime);
		no ->                       % Must be initiated before.
		    reply(Parent,Ref,{error,not_initiated}),
		    loop(Parent,Tracer,TI,LD,PublLD,CleanTime)
	    end;
	{{tpm_tracer,{Mod,Func,Arity,MS},InitFunc,CallFunc,ReturnFunc,RemoveFunc},Ref,Parent} ->
	    case check_mfarity_exists(Mod,Func,Arity) of
		no ->                       % Good then we can add it!
		    case check_tpm_args(Mod,Func,Arity) of
			true ->             % Args are ok.
			    NewMS=add_tracer(MS,Tracer),
			    {NewLD,NewPublLD,N}=
				h_tpm(Mod,Func,Arity,NewMS,
				      InitFunc,CallFunc,ReturnFunc,RemoveFunc,
				      TI,LD,PublLD),
			    reply(Parent,Ref,{ok,N}),
			    loop(Parent,Tracer,TI,NewLD,NewPublLD,CleanTime);
			false ->
			    reply(Parent,Ref,{error,bad_mfa}),
			    loop(Parent,Tracer,TI,LD,PublLD,CleanTime)
		    end;
		yes ->
		    reply(Parent,Ref,{error,already_initiated}),
		    loop(Parent,Tracer,TI,LD,PublLD,CleanTime)
	    end;
	{{tpm_tracer,{Mod,Func,Arity,MS}},Ref,Parent} ->
	    case check_mfarity_exists(Mod,Func,Arity) of
		yes ->                      % Ok, and args must be ok then also.
		    NewMS=add_tracer(MS,Tracer),
		    {NewLD,N}=h_tpm(Mod,Func,Arity,NewMS,LD),
		    reply(Parent,Ref,{ok,N}),
		    loop(Parent,Tracer,TI,NewLD,PublLD,CleanTime);
		no ->                       % Must be initiated before.
		    reply(Parent,Ref,{error,not_initiated}),
		    loop(Parent,Tracer,TI,LD,PublLD,CleanTime)
	    end;
	{{tpm_ms,{Mod,Func,Arity},MSname,MS},Ref,Parent} ->
	    case check_mfarity_exists(Mod,Func,Arity) of
		yes ->                      % Ok, and args must be ok then also.
		    reply(Parent,Ref,{ok,h_tpm_ms(Mod,Func,Arity,MSname,MS)}),
		    loop(Parent,Tracer,TI,LD,PublLD,CleanTime);
		no ->
		    reply(Parent,Ref,{error,not_initiated}),
		    loop(Parent,Tracer,TI,LD,PublLD,CleanTime)
	    end;
	{{tpm_ms_tracer,{Mod,Func,Arity},MSname,MS},Ref,Parent} ->
	    case check_mfarity_exists(Mod,Func,Arity) of
		yes ->                      % Ok, and args must be ok then also.
		    NewMS=add_tracer(MS,Tracer),
		    reply(Parent,Ref,{ok,h_tpm_ms(Mod,Func,Arity,MSname,NewMS)}),
		    loop(Parent,Tracer,TI,LD,PublLD,CleanTime);
		no ->
		    reply(Parent,Ref,{error,not_initiated}),
		    loop(Parent,Tracer,TI,LD,PublLD,CleanTime)
	    end;
	{{ctpm_ms,{Mod,Func,Arity},MSname},Ref,Parent} ->
	    reply(Parent,Ref,ok),
	    h_ctpm_ms(Mod,Func,Arity,MSname),
	    loop(Parent,Tracer,TI,LD,PublLD,CleanTime);
	{{ctpm,{Mod,Func,Arity}},Ref,Parent} ->
	    case get_remove_func_ld(Mod,Func,Arity,LD) of
		false ->                    % Incorrect Mod:Func/Arity!
		    reply(Parent,Ref,{error,bad_mfa}),
		    loop(Parent,Tracer,TI,LD,PublLD,CleanTime); % Do nothing!
		MF ->                       % {M,F}, Func or 'void'.
		    catch erlang:trace_pattern({Mod,Func,Arity},false,[meta]),
		    NewPublLD=do_removefunc(MF,Mod,Func,Arity,PublLD),
		    NewLD=ctpm_ld(Mod,Func,Arity,LD),
		    reply(Parent,Ref,ok),
		    loop(Parent,Tracer,TI,NewLD,NewPublLD,CleanTime)
	    end;
	{suspend,Parent} ->                 % Removes all meta trace patterns.
	    stop_all_meta_tracing(get_all_meta_funcs_ld(LD),PublLD,LD),
	    do_remove_publ_ld(get_removepublldmf_ld(LD),PublLD),
	    NewPublLD=do_init_publ_ld(get_initpublldmfa_ld(LD)),
	    loop(Parent,Tracer,TI,reset_ld(LD),NewPublLD,CleanTime);
	{stop,Parent} ->                    % Make a controlled shutdown.
	    stop_all_meta_tracing(get_all_meta_funcs_ld(LD),PublLD,LD),
	    do_remove_publ_ld(get_removepublldmf_ld(LD),PublLD),
	    close_traceinfo_file(TI);       % And then simply terminate.
	{trace_ts,Pid,call,{M,F,Args},TS} ->
	    case handle_meta(get_call_func_ld(M,F,length(Args),LD),Pid,{call,Args,TS},PublLD) of
		{ok,NewPublLD,Output} when is_binary(Output);is_list(Output) ->
		    write_output(TI,Output),
		    loop(Parent,Tracer,TI,LD,NewPublLD,CleanTime);
		{ok,NewPublLD,_} ->         % No output to the ti-file this time.
		    loop(Parent,Tracer,TI,LD,NewPublLD,CleanTime);
		_ ->                        % Not handled correct, not much to do.
		    loop(Parent,Tracer,TI,LD,PublLD,CleanTime)
	    end;
	{trace_ts,Pid,TypeTag,{M,F,Arity},Value,TS}
	  when TypeTag==return_from;TypeTag==exception_from ->
	    case handle_meta(get_return_func_ld(M,F,Arity,LD),Pid,{TypeTag,Value,TS},PublLD) of
		{ok,NewPublLD,Output} when is_binary(Output);is_list(Output) ->
		    write_output(TI,Output),
		    loop(Parent,Tracer,TI,LD,NewPublLD,CleanTime);
		{ok,NewPublLD,_} ->         % No output to the ti-file this time.
		    loop(Parent,Tracer,TI,LD,NewPublLD,CleanTime);
		_ ->                        % Not handled correct, not much to do.
		    loop(Parent,Tracer,TI,LD,PublLD,CleanTime)
	    end;
	{relayed_meta,Bin} ->
	    write_output(TI,Bin),
	    loop(Parent,Tracer,TI,LD,PublLD,CleanTime);
	{write_ti,OutPut} ->
	    write_output(TI,OutPut),
	    loop(Parent,Tracer,TI,LD,PublLD,CleanTime);
	{get_state,Ref,From} ->             % Debug function.
	    reply(From,Ref,{ok,LD,PublLD}),
	    loop(Parent,Tracer,TI,LD,PublLD,CleanTime);
	_Other ->
	    loop(Parent,Tracer,TI,LD,PublLD,CleanTime)
    end.


%% =============================================================================
%% First level help functions.
%% =============================================================================

%% Function which opens the trace-information file(s). It must understand
%% the tidata specification which is part of the tracerdata given to the
%% runtime component during init_tracing.
%% It must return an internal notation of the time of file open and a
%% useful descriptor the write_output function can use.
%% Returns {ok,TiDescriptor} or {error,Reason}.
open_traceinfo_file({file,FileName}) ->     % A plain raw binary file.
    case file:open(FileName,[write,raw,binary]) of
	{ok,FD} ->
	    {ok,{file,FD}};
	{error,Reason} ->
	    {error,{open,[FileName,Reason]}}
    end;
open_traceinfo_file({relay,ToNode}) ->      % Use distributed Erlang.
    {ok,{relay,ToNode}};
open_traceinfo_file(IncorrectTI) ->
    {error,{badarg,IncorrectTI}}.
%% -----------------------------------------------------------------------------

close_traceinfo_file({file,FD}) ->
    file:close(FD);
close_traceinfo_file(_) ->
    ok.
%% -----------------------------------------------------------------------------

%% Help function handling initializing meta tracing of a function.
%% Returns {NewLD,NewPublLD}.
h_init_tpm(Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc,TI,LD,PublLD) ->
    case do_initfunc(InitFunc,Mod,Func,Arity,PublLD) of
	{NewPublLD,Output} ->
	    write_output(TI,Output),
	    NewLD=init_tpm_ld(Mod,Func,Arity,CallFunc,ReturnFunc,RemoveFunc,LD),
	    {NewLD,NewPublLD};
	false ->                            % The initfunc did not do anything.
	    NewLD=init_tpm_ld(Mod,Func,Arity,CallFunc,ReturnFunc,RemoveFunc,LD),
	    {NewLD,PublLD}
    end.
%% -----------------------------------------------------------------------------

%% Help function handling initializing meta tracing of a function and also
%% set the meta trace pattern as specified.
%% Returns {NewLD,NewPublLD,N}.
h_tpm(Mod,Func,Arity,MS,InitFunc,CallFunc,ReturnFunc,RemoveFunc,TI,LD,PublLD) ->
    {NewLD,NewPublLD}=
	h_init_tpm(Mod,Func,Arity,InitFunc,CallFunc,ReturnFunc,RemoveFunc,TI,LD,PublLD),
    case set_meta_tracing(Mod,Func,Arity,MS) of
	true ->                              % Ok, set one pattern.
	    {NewLD,NewPublLD,1};
	false ->
	    {NewLD,NewPublLD,0}
    end.
%% -----------------------------------------------------------------------------

%% Help function handling setting meta trace patter for a function which has
%% already been intialized. Note that we must remove all potentially stored
%% match-specs, if this function has been given match-specs before with
%% tpm_ms.
%% Returns a {NewLD,N}.
h_tpm(Mod,Func,Arity,MS,LD) ->
    case set_meta_tracing(Mod,Func,Arity,MS) of
	true ->
	    {remove_ms_ld(Mod,Func,Arity,LD),1};
	false ->
	    {LD,0}
    end.
%% -----------------------------------------------------------------------------

%% Help function that adds a match-spec to Mod:Func/Arity. It is not defined
%% in which order the match-specs will be given to the BIF.
%% Note that if an MS with the same name as an exiting is inserted, the previous
%% match-spec will be removed.
%% Very important to realise is that the empty meta match spec [] imposes no
%% restrictions what so ever on the generating of meta trace call messages.
%% Uncontrolled sending of such messages may quickly drain power from the system.
%% Since an empty match-spec will "disappear" when added to other match specs,
%% the empty match is transformed to what it actually is: [{'_',[],[]}].
%% Returns 0 or 1 indicating failure or success.
h_tpm_ms(Mod,Func,Arity,MSname,MS) ->
    MSsNames=get_ms_ld(Mod,Func,Arity),     % Fetch all previous match-specs.
    TransformedMS=h_tpm_ms_convert_null_ms(MS),
    MSsNames1=lists:keydelete(MSname,1,MSsNames), % If it already existed, it is gone!
    NewMSs=lists:flatten([TransformedMS,lists:map(fun({_Name,MSx})->MSx end,MSsNames1)]),
    case set_meta_tracing(Mod,Func,Arity,NewMSs) of
	true ->                             % We only save the MS if it was good.
	    put_ms_ld(Mod,Func,Arity,MSname,TransformedMS,MSsNames1),
	    1;
	false ->
	    0
    end.

%% Help function converting the null match spec into, still a null match spec,
%% on a proper match spec format. This because it will otherwise be difficult
%% to see the difference between no active tpm_ms and all a set of null ms.
h_tpm_ms_convert_null_ms([]) ->
    [{'_',[],[]}];
h_tpm_ms_convert_null_ms(MS) ->
    MS.
%% -----------------------------------------------------------------------------

%% Help function returning a list of all names used for match-functions for
%% the Mod:Func/Arity in question.
h_list_tpm_ms(Mod,Func,Arity) ->
    MSsNames=get_ms_ld(Mod,Func,Arity),     % A list of {MSname,MS}.
    lists:map(fun({MSname,_})->MSname end,MSsNames).
%% -----------------------------------------------------------------------------

%% Function that removes a named match-spec. Returns nothing significant.
%% Note that if we end up with no match-specs, we must remove the meta trace
%% patten all together. That is bringing the function back to just initiated.
h_ctpm_ms(Mod,Func,Arity,MSname) ->
    case get_ms_ld(Mod,Func,Arity) of
	[] ->                               % The name does certainly not exist!
	    true;                           % We don't have to do anything.
	MSsNames ->
	    case lists:keysearch(MSname,1,MSsNames) of
		{value,{_,_MS}} ->          % Ok, we must do something!
		    NewMSsNames=lists:keydelete(MSname,1,MSsNames),
		    case lists:flatten(lists:map(fun({_Name,MS})->MS end,NewMSsNames)) of
			[] ->               % This means stop meta tracing.
			    set_meta_tracing(Mod,Func,Arity,false);
			NewMSs ->
			    set_meta_tracing(Mod,Func,Arity,NewMSs)
		    end,
		    set_ms_ld(Mod,Func,Arity,NewMSsNames);
		false ->                    % But this name does not exist.
		    true                    % So we do not have to do anything.
	    end
    end.
%% -----------------------------------------------------------------------------

%% Function that checks the arguments to the meta trace pattern. The reason we
%% must do this is that we can only allow meta tracing on specific functions and
%% not using wildpatterns. Otherwise the meta trace server will not understand
%% which callfunc for instance to call when a meta-trace message is generated
%% for a function.
%% Returns 'true' or 'false'.
check_tpm_args(Mod,Func,Arity)
  when is_atom(Mod),is_atom(Func),is_integer(Arity),Mod/='_',Func/='_' ->
    true;
check_tpm_args(_,_,_) ->
    false.
%% -----------------------------------------------------------------------------

%% Help function which calls the actual BIF setting meta-trace-patterns.
%% Returns 'true' or 'false'.
set_meta_tracing(Mod,Func,Arity,MS) when is_atom(Mod) ->
    case erlang:module_loaded(Mod) of
	true ->
	    set_meta_tracing_2(Mod,Func,Arity,MS);
	false ->                            % The module is not loaded.
	    case code:ensure_loaded(Mod) of
		{module,_Mod} ->
		    set_meta_tracing_2(Mod,Func,Arity,MS);
		{error,_Reason} ->          % Could not load the module.
		    false                   % No use try to trace.
	    end
    end;
set_meta_tracing(_,_,_,_) ->
    false.

set_meta_tracing_2(Mod,Func,Arity,MS) ->
    case catch erlang:trace_pattern({Mod,Func,Arity},MS,[meta]) of
	0 ->                                % Hmm, nothing happend :-)
	    false;
	N when is_integer(N) ->                % The normal case, some functions were hit.
	    true;
	{'EXIT',_Reason} ->
	    false
    end.
%% -----------------------------------------------------------------------------

%% Help function which removes all meta trace pattern for the functions mentioned
%% in the list being first argument. It also executes the remove funcs for each
%% and every no longer meta traced function. This done since some of the remove
%% functions may do side-effects (like deleteing ETS tables).
%% Returns nothing significant.
stop_all_meta_tracing([{M,F,Arity}|Rest],PublLD,LD) ->
    catch erlang:trace_pattern({M,F,Arity},false,[meta]),
    NewPublLD=do_removefunc(get_remove_func_ld(M,F,Arity,LD),M,F,Arity,PublLD),
    stop_all_meta_tracing(Rest,NewPublLD,LD);
stop_all_meta_tracing([],_,_) ->
    true.
%% -----------------------------------------------------------------------------

%% This function calls the function registered to be handler for a certain
%% meta-traced function. Such a function or fun must take three arguments
%% and return {ok,NewPrivLD,OutPutBinary} or 'false'. OutPutBinary may be
%% something else, and is then ignored.
handle_meta({M,F},Pid,Arg1,PrivLD) ->
    (catch M:F(Pid,Arg1,PrivLD));
handle_meta(Fun,Pid,Arg1,PrivLD) when is_function(Fun) ->
    (catch Fun(Pid,Arg1,PrivLD));
handle_meta(_,_,_,_) ->                     % Don't know how to do this.
    false.
%% -----------------------------------------------------------------------------

%% Help function writing output from a callback function to the ti-file.
%% Output can be a binary or a list of binaries.
write_output(TI,[OutPut|Rest]) ->
    write_output(TI,OutPut),
    write_output(TI,Rest);
write_output({file,FD},Bin) when is_binary(Bin) -> % Plain direct-binary file
    Size=byte_size(Bin),
    file:write(FD,list_to_binary([<<0,Size:32>>,Bin]));
write_output({relay,ToNode},Bin) when is_atom(ToNode),is_binary(Bin) ->
    {inviso_rt_meta,ToNode} ! {relayed_meta,Bin};
write_output(_,_) ->                        % Don't understand, just skip.
    true.
%% -----------------------------------------------------------------------------


%% =============================================================================
%% Various help functions.
%% =============================================================================

%% Help function initializing the public loopdata structure. Note that if the
%% supplied InitPublLDmfa is faulty we let the structure become the error.
%% The error will most likely turn up in an error report somewhere, eventually.
do_init_publ_ld({M,F,Args}) when is_atom(M),is_atom(F),is_list(Args) ->
    case catch apply(M,F,Args) of
	{'EXIT',_Reason} ->
	    {error,init_publ_ld_func};      % Let the struct be this error!
	InitialPublLD ->
	    InitialPublLD
    end;
do_init_publ_ld(_) ->
    {error,init_publ_ld_func}.
%% -----------------------------------------------------------------------------

%% Help function which removes the public loopdata structure. The function does
%% not necessarily have to exist. Returns nothing significant.
do_remove_publ_ld({M,F},PublLD) when is_atom(M),is_atom(F) ->
    catch M:F(PublLD);
do_remove_publ_ld(_,_) ->
    true.
%% -----------------------------------------------------------------------------	

%% Hlp function initializing a particular meta traced function into the public
%% loopdata. Note that the function is not mandatory.
%% Returns {NewPublLD,Output} or 'false'.
do_initfunc({M,F},Mod,Func,Arity,PublLD) when is_atom(M),is_atom(F) ->
    case catch M:F(Mod,Func,Arity,PublLD) of
	{ok,NewPublLD,Output} ->
	    {NewPublLD,Output};
	_ ->                                % Everything else is an error.
	    false                           % Act as no initialization function.
    end;
do_initfunc(Fun,Mod,Func,Arity,PublLD) when is_function(Fun) ->
    case catch Fun(Mod,Func,Arity,PublLD) of
	{ok,NewPublLD,Output} ->
	    {NewPublLD,Output};
	_ ->                                % Everything else is an error.
	    false                           % Act as no initialization function.
    end;
do_initfunc(_,_,_,_,_) ->                   % Perhaps too generous, should be 'void' only.
    false.
%% -----------------------------------------------------------------------------

%% Help function removing a particular meta traced function from the public
%% loopdata. Note that we do not make much noice should the call back function
%% be faulty.
do_removefunc({M,F},Mod,Func,Arity,PublLD) when is_atom(M),is_atom(F) ->
    case catch M:F(Mod,Func,Arity,PublLD) of
	{ok,NewPublLD} ->
	    NewPublLD;
	_ ->                                % Everything else is an error.
	    PublLD                          % Act as no initialization function.
    end;
do_removefunc(Fun,Mod,Func,Arity,PublLD) when is_function(Fun) ->
    case catch Fun(Mod,Func,Arity,PublLD) of
	{ok,NewPublLD} ->
	    NewPublLD;
	_ ->                                % Everything else is an error.
	    PublLD                          % Act as no initialization function.
    end;
do_removefunc(_,_,_,_,PublLD) ->
    PublLD.
%% -----------------------------------------------------------------------------

%% Function that, if the time has come, goes through the priv-ld structure and
%% cleans away entryn left behind. The usual cause is that the function call
%% caused an exception and there were therefore no matching return_from.
%% Returns {NewPrivLD,now()}.
throw_old_failed({M,F},PrivLD,PrevClean) ->
    case difference_in_now(PrevClean,now(),60) of % We clean once every minute.
	true ->
	    case catch apply(M,F,[PrivLD]) of
		{'EXIT',_Reason} ->         % Something went wrong, ignore it.
		    {PrivLD,now()};         % Just keep the old priv-ld.
		NewPrivLD ->                % The function must return a priv-ld.
		    {NewPrivLD,now()}
	    end;
	false ->                            % Not time yet!
	    {PrivLD,PrevClean}
    end.
%% -----------------------------------------------------------------------------

%% Help function comparing two now timestamps. Returns true or false depending
%% on if S2 is more than DiffS seconds after S1. Only works for differences
%% less than 1 million seconds.
difference_in_now({MegaS1,S1,_},{MegaS2,S2,_},DiffS) ->
    if
	MegaS1+1<MegaS2 ->                  % More than 1 Mega sec. difference.
	    true;
	MegaS1==MegaS2,S1+DiffS<S2 ->
	    true;
	MegaS1+1==MegaS2,S1+DiffS<S2+1000000 ->
	    true;
	true ->
	    false
    end.
%% -----------------------------------------------------------------------------

%% This help function adds a {tracer,Tracer} to the enable-list in a 'trace'
%% match spec action. The reason for this is that the author of the a meta
%% match spec meant to turn tracing on for the process executing the match spec
%% can not know the tracer. This since the match spec is most likely authored
%% at the control component's node, and not here.
%% Note the double tuple necessary to make it just precise a tuple!
%% Returns a new match spec.
add_tracer([MS1|Rest],Tracer) ->
    [add_tracer_2(MS1,Tracer)|add_tracer(Rest,Tracer)];
add_tracer([],_) ->
    [];
add_tracer(NotList,_Tracer) ->              % Can be 'false', but also an error.
    NotList.

add_tracer_2({Head,Cond,Body},Tracer) ->
    {Head,Cond,add_tracer_3(Body,Tracer)};
add_tracer_2(Faulty,_Tracer) ->
    Faulty.

add_tracer_3([{trace,Disable,Enable}|Rest],Tracer) when is_list(Enable) ->
    [{trace,Disable,Enable++[{{tracer,Tracer}}]}|Rest];
add_tracer_3([ActionTerm|Rest],Tracer) ->
    [ActionTerm|add_tracer_3(Rest,Tracer)];
add_tracer_3([],_Tracer) ->
    [];
add_tracer_3(FaultyBody,_Tracer) ->
    FaultyBody.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Help functions handling internal loopdata.
%% -----------------------------------------------------------------------------

-record(ld,{init_publ_ld_mfa,               % {M,F,Args}
	    remove_publ_ld_mf,              % {M,F} | void
	    clean_publ_ld_mf,               % {Mod,Func}
	    ms_mfarities=notable,           % ETS holding names match functions.
	    call_mfarities=[],              % [{{M,F,Arity},2-TupleOrFun},...]
	    return_mfarities=[],            % [{{M,F,Arity},2-TupleOrFun},...]
	    remove_mfarities=[]
	   }).

mk_new_ld(InitPublLDmfa,RemovePublLDmf,CleanPublLDmf,TId) ->
    #ld{
	   init_publ_ld_mfa=InitPublLDmfa,
	   remove_publ_ld_mf=RemovePublLDmf,
	   clean_publ_ld_mf=CleanPublLDmf,
	   ms_mfarities=TId
       }.
%% -----------------------------------------------------------------------------

%% Function which restores the internal loop data to somekind of initial state.
%% This is useful when tracing has been suspended.
reset_ld(#ld{init_publ_ld_mfa=InitPublLDmfa,
	     remove_publ_ld_mf=RemovePublLDmf,
	     clean_publ_ld_mf=CleanPublLDmf,
	     ms_mfarities=TId}) ->
    ets:match_delete(TId,{'_','_'}),        % Empty the table.
    #ld{init_publ_ld_mfa=InitPublLDmfa,
	remove_publ_ld_mf=RemovePublLDmf,
	clean_publ_ld_mf=CleanPublLDmf,
        ms_mfarities=TId}.
%% -----------------------------------------------------------------------------

get_initpublldmfa_ld(#ld{init_publ_ld_mfa=InitPublLDmfa}) ->
    InitPublLDmfa.
%% -----------------------------------------------------------------------------

get_removepublldmf_ld(#ld{remove_publ_ld_mf=RemovePublLDmf}) ->
    RemovePublLDmf.
%% -----------------------------------------------------------------------------

get_cleanpublldmf_ld(#ld{clean_publ_ld_mf=CleanPublLDmf}) ->
    CleanPublLDmf.
%% -----------------------------------------------------------------------------

%% Help function adding data associated with a meta traced function to the
%% internal loopdata. Called when meta tracing is activated for M:F/Arity.
init_tpm_ld(M,F,Arity,CallFunc,ReturnFunc,RemoveFunc,LD) ->
    ets:insert(LD#ld.ms_mfarities,{{M,F,Arity},[]}),
    CallFuncs=LD#ld.call_mfarities,
    ReturnFuncs=LD#ld.return_mfarities,
    RemoveFuncs=LD#ld.remove_mfarities,
    LD#ld{call_mfarities=[{{M,F,Arity},CallFunc}|CallFuncs],
	  return_mfarities=[{{M,F,Arity},ReturnFunc}|ReturnFuncs],
	  remove_mfarities=[{{M,F,Arity},RemoveFunc}|RemoveFuncs]}.
%% -----------------------------------------------------------------------------

%% Help function which answers the question if we have already initiated the
%% function. It is done by looking in the ETS-table with named match-functions.
%% If there is an entry in the set-type table for M:F/Arity, the function is
%% initiated.
%% Returns 'yes' or 'no'.
check_mfarity_exists(M,F,Arity) ->
    case ets:lookup(?NAMED_MS_TAB,{M,F,Arity}) of
	[] ->
	    no;
	[_] ->
	    yes
    end.
%% -----------------------------------------------------------------------------

%% Help function adding an entry with [{MSname,MSlist}|MSsNames] for M:F/Arity.
%% Note that any already existing entry is removed.
%% Returns nothing significant.
put_ms_ld(M,F,Arity,MSname,MS,MSsNames) ->
    ets:insert(?NAMED_MS_TAB,{{M,F,Arity},[{MSname,MS}|MSsNames]}).
%% -----------------------------------------------------------------------------

%% Help function taking a list of {MSname,MSs} and storing them in the
%% internal loop data structure. The storage is actually implemented as an ETS
%% table. Any previous list of {MSname,MSs} associated with this {M,F,Arity} will
%% be lost. Returns nothing significant.
set_ms_ld(M,F,Arity,MSsNames) ->
    ets:insert(?NAMED_MS_TAB,{{M,F,Arity},MSsNames}).
%% -----------------------------------------------------------------------------

%% Help function fetching a list of {MSname,MatchSpecs} for a M:F/Arity. The
%% match-functions are stored in an ETS table searchable on {M,F,Arity}.
get_ms_ld(M,F,Arity) ->
    case ets:lookup(?NAMED_MS_TAB,{M,F,Arity}) of
	[{_MFArity,MSsNames}] ->
	    MSsNames;
	[] ->
	    []
    end.
%% -----------------------------------------------------------------------------

%% Help function removing all saved match-specs for a certain M:F/Arity.
%% Returns a new loopdata structure.
remove_ms_ld(M,F,Arity,LD) ->
    ets:delete(LD#ld.ms_mfarities,{M,F,Arity}),
    LD.
%% -----------------------------------------------------------------------------

%% Help function which removes all information about a meta traced function from
%% the internal loopdata. Returns a new loopdata structure.
ctpm_ld(M,F,Arity,LD) ->
    ets:delete(LD#ld.ms_mfarities,{M,F,Arity}),
    NewCallFuncs=lists:keydelete({M,F,Arity},1,LD#ld.call_mfarities),
    NewReturnFuncs=lists:keydelete({M,F,Arity},1,LD#ld.return_mfarities),
    NewRemoveFuncs=lists:keydelete({M,F,Arity},1,LD#ld.remove_mfarities),
    LD#ld{call_mfarities=NewCallFuncs,
	  return_mfarities=NewReturnFuncs,
	  remove_mfarities=NewRemoveFuncs}.
%% -----------------------------------------------------------------------------

get_call_func_ld(M,F,Arity,#ld{call_mfarities=CallFuncs}) ->
    case lists:keysearch({M,F,Arity},1,CallFuncs) of
	{value,{_,MF}} ->
	    MF;
	false ->
	    false
    end.
%% -----------------------------------------------------------------------------

get_return_func_ld(M,F,Arity,#ld{return_mfarities=CallFuncs}) ->
    case lists:keysearch({M,F,Arity},1,CallFuncs) of
	{value,{_,MF}} ->
	    MF;
	false ->
	    false
    end.
%% -----------------------------------------------------------------------------

get_remove_func_ld(M,F,Arity,#ld{remove_mfarities=RemoveFuncs}) ->
    case lists:keysearch({M,F,Arity},1,RemoveFuncs) of
	{value,{_,MF}} ->
	    MF;
	false ->
	    false
    end.
%% -----------------------------------------------------------------------------

%% Function returning a list of all {Mod,Func,Arity} which are currently meta
%% traced. It does do by listifying the call_mfarities field in the internal
%% loopdata.
get_all_meta_funcs_ld(#ld{call_mfarities=CallFuncs}) ->
    lists:map(fun({MFArity,_})->MFArity end,CallFuncs).
%% -----------------------------------------------------------------------------


%% =============================================================================
%% Functions for the standard PublLD structure.
%%
%% It is tuple {Part1,GlobalData} where Part1 is of length at least 2.
%% Where each field is a list of tuples. The last item in each tuple shall be
%% a now tuple, making it possible to clean it away should it be too old to be
%% relevant (there was no return_from message due to a failure).
%% Other fields can be used for other functions.
%% The GlobalData is not cleaned but instead meant to store data must be passed
%% to each CallFunc when a meta trace message arrives.
%% =============================================================================
		      
%% Function returning our standard priv-loopdata structure.
init_std_publld(Size,GlobalData) ->
    {list_to_tuple(lists:duplicate(Size,[])),GlobalData}.
%% -----------------------------------------------------------------------------

%% Function capable of cleaning out a standard publ-ld. The last element of each
%% tuple must be the now item.
%% Returns a new publ-ld structure.
clean_std_publld({Part1,GlobalData}) ->
    {clean_std_publld_2(Part1,now(),tuple_size(Part1),[]),GlobalData}.

clean_std_publld_2(_,_,0,Accum) ->
    list_to_tuple(Accum);
clean_std_publld_2(PublLD,Now,Index,Accum) ->
    NewTupleList=clean_std_publld_3(element(Index,PublLD),Now),
    clean_std_publld_2(PublLD,Now,Index-1,[NewTupleList|Accum]).

clean_std_publld_3([Tuple|Rest],Now) ->
    PrevNow=element(tuple_size(Tuple),Tuple), % Last item shall be the now item.
    case difference_in_now(PrevNow,Now,30) of
	true ->                             % Remove it then!
	    clean_std_publld_3(Rest,Now);
	false ->                            % Keep it!
	    [Tuple|clean_std_publld_3(Rest,Now)]
    end;
clean_std_publld_3([],_) ->
    [].
%% -----------------------------------------------------------------------------

%% =============================================================================
%% Functions used as handling functions (as funs) for registered process names.
%% (Given that we use the standard priv-ld, otherwise you must do your own!).
%% =============================================================================

%% Call-back for initializing the meta traced functions there are quick functions
%% for. Returns a new public loop data structure.
metafunc_init(erlang,register,2,{Part1,GlobalData}) ->
    {setelement(1,Part1,[]),GlobalData}.
%% -----------------------------------------------------------------------------

%% Call-function for erlang:register/2.
%% This function adds the call to register/2 to a standard priv-ld structure.
%% Note that we *must* search for previous entries from the same process. If such
%% still in structure it means a failed register/2 call. It must first be removed
%% so it can not be mixed up with this one. Since meta-trace message will arrive
%% in order, there was no return_from message for that call if we are here now.
local_register_call(CallingPid,{call,[Alias,Pid],TS},{Part1,GlobalData}) ->
    TupleList=element(1,Part1),             % The register/2 entry in a std. priv-ld.
    NewTupleList=lists:keydelete(CallingPid,1,TupleList), % If present, remove previous call.
    {ok,
     {setelement(1,Part1,[{CallingPid,{Alias,Pid},TS}|NewTupleList]),GlobalData},
     void}.

%% Return-function for the erlang:register/2 BIF.
%% This function formulates the output and removes the corresponding call entry
%% from the standard priv-ld structure.
local_register_return(CallingPid,{return_from,_Val,_TS},PublLD={Part1,GlobalData}) ->
    TupleList=element(1,Part1),             % The register/2 entry in a std. priv-ld.
    case lists:keysearch(CallingPid,1,TupleList) of
	{value,{_,{Alias,Pid},NowTS}} ->
	    NewTupleList=lists:keydelete(CallingPid,1,TupleList),
	    {ok,
	     {setelement(1,Part1,NewTupleList),GlobalData},
	     term_to_binary({Pid,Alias,alias,NowTS})};
	false ->                            % Strange, then don't know what to do.
	    {ok,PublLD,void}                % Do nothing seems safe.
    end;
local_register_return(CallingPid,{exception_from,_Val,_TS},{Part1,GlobalData}) ->
    TupleList=element(1,Part1),             % The register/2 entry in a std. priv-ld.
    NewTupleList=lists:keydelete(CallingPid,1,TupleList),
    {ok,{setelement(1,Part1,NewTupleList),GlobalData},void}; % No association then.
local_register_return(_,_,PublLD) ->        % Don't understand this.
    {ok,PublLD,void}.

%% When unregister/1 us called we simply want a unalias entry in the ti-file.
%% We can unfortunately not connect it with a certain pid.
local_unregister_call(_CallingPid,{_TypeTag,[Alias],TS},PublLD) ->
    {ok,PublLD,term_to_binary({undefined,Alias,unalias,TS})}.
%% -----------------------------------------------------------------------------

%% Call-function for global:register_name/2,/3.
%% This function is actually the call function for the handle_call/3 in the
%% global server. Note that we must check that we only do this on the node
%% where Pid actually resides.
global_register_call(_CallingPid,{call,[{register,Alias,P,_},_,_],TS},PublLD)
  when node(P)==node()->
    {ok,PublLD,term_to_binary({P,{global,Alias},alias,TS})};
global_register_call(_CallingPid,_,PublLD) ->
    {ok,PublLD,void}.

%% Call-function for global:unregister_name. It acutally checks on the use of
%% global:delete_global_name/2 which is called when ever a global name is removed.
global_unregister_call(_CallingPid,{call,[Alias,P],TS},PublLD) when node(P)==node()->
    {ok,PublLD,term_to_binary({P,{global,Alias},unalias,TS})};
global_unregister_call(_CallingPid,_,PublLD) ->
    {ok,PublLD,void}.
%% -----------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

