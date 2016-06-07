%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
-module(etop_txt).
-author('siri@erix.ericsson.se').

%%-compile(export_all).
-export([init/1,stop/1]).
-export([do_update/3]).

-include("etop.hrl").
-include("etop_defs.hrl").

-import(etop,[loadinfo/1,meminfo/2]).

-define(PROCFORM,"~-15w~-20s~8w~8w~8w~8w ~-20s~n").

stop(Pid) -> Pid ! stop.

init(Config) ->
    loop(Config).

loop(Config) ->
    Info = do_update(Config),
    receive 
	stop -> stopped;
	{dump,Fd} -> do_update(Fd,Info,Config), loop(Config); 
	{config,_,Config1} -> loop(Config1)
    after Config#opts.intv -> loop(Config)
    end.

do_update(Config) ->
    Info = etop:update(Config),
    do_update(standard_io,Info,Config).

do_update(Fd,Info,Config) ->
    {Cpu,NProcs,RQ,Clock} = loadinfo(Info),
    io:nl(Fd),
    writedoubleline(Fd),
    case Info#etop_info.memi of
	undefined ->
	    io:fwrite(Fd, " ~-72w~10s~n"
		      " Load:  cpu  ~8w~n"
		      "        procs~8w~n"
		      "        runq ~8w~n",
		      [Config#opts.node,Clock,
		       Cpu,NProcs,RQ]);
	Memi ->
	    [Tot,Procs,Atom,Bin,Code,Ets] = 
		meminfo(Memi, [total,processes,atom,binary,code,ets]),
	    io:fwrite(Fd, ?SYSFORM,
		      [Config#opts.node,Clock,
		       Cpu,Tot,Bin,
		       NProcs,Procs,Code,
		       RQ,Atom,Ets])
    end,
    io:nl(Fd),
    writepinfo_header(Fd),
    writesingleline(Fd),
    writepinfo(Fd,Info#etop_info.procinfo),
    writedoubleline(Fd),
    io:nl(Fd),
    Info.

writepinfo_header(Fd) ->
    io:fwrite(Fd,"Pid            Name or Initial Func    Time    Reds  Memory    MsgQ Current Function~n",[]).

writesingleline(Fd) ->
    io:fwrite(Fd,"----------------------------------------------------------------------------------------~n",[]).
writedoubleline(Fd) ->
    io:fwrite(Fd,"========================================================================================~n",[]).
 
writepinfo(Fd,[#etop_proc_info{pid=Pid,
			       mem=Mem,
			       reds=Reds,
			       name=Name,
			       runtime=Time,
			       cf=MFA,
			       mq=MQ}
	       |T]) ->
    io:fwrite(Fd,?PROCFORM,[Pid,to_list(Name),Time,Reds,Mem,MQ,formatmfa(MFA)]), 
    writepinfo(Fd,T);
writepinfo(_Fd,[]) ->
    ok.


formatmfa({M, F, A}) ->
    io_lib:format("~w:~w/~w",[M, F, A]);
formatmfa(Other) ->
    %% E.g. when running hipe - the current_function for some
    %% processes will be 'undefined'
    io_lib:format("~w",[Other]).

to_list(Name) when is_atom(Name) -> atom_to_list(Name);
to_list({_M,_F,_A}=MFA) -> formatmfa(MFA).
