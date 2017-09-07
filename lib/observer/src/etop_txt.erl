%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2017. All Rights Reserved.
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
-export([do_update/4]).

-include("etop.hrl").
-include("etop_defs.hrl").

-import(etop,[loadinfo/2,meminfo/2]).

stop(Pid) -> Pid ! stop.

init(Config) ->
    loop(#etop_info{},Config).

loop(Prev,Config) ->
    Info = do_update(Prev,Config),
    receive 
	stop -> stopped;
	{dump,Fd} -> do_update(Fd,Info,Prev,Config), loop(Info,Config);
	{config,_,Config1} -> loop(Info,Config1)
    after Config#opts.intv -> loop(Info,Config)
    end.

do_update(Prev,Config) ->
    Info = etop:update(Config),
    do_update(standard_io,Info,Prev,Config).

do_update(Fd,Info,Prev,Config) ->
    {Cpu,NProcs,RQ,Clock} = loadinfo(Info,Prev),
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
    writepinfo(Fd,Info#etop_info.procinfo,modifier(Fd)),
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
	       |T],
           Modifier) ->
    io:fwrite(Fd,proc_format(Modifier),
              [Pid,to_string(Name,Modifier),Time,Reds,Mem,MQ,
               to_string(MFA,Modifier)]),
    writepinfo(Fd,T,Modifier);
writepinfo(_Fd,[],_) ->
    ok.

proc_format(Modifier) ->
    "~-15w~-20"++Modifier++"s~8w~8w~8w~8w ~-20"++Modifier++"s~n".

to_string({M,F,A},Modifier) ->
    io_lib:format("~w:~"++Modifier++"w/~w",[M,F,A]);
to_string(Other,Modifier) ->
    io_lib:format("~"++Modifier++"w",[Other]).

modifier(Device) ->
    case encoding(Device) of
        latin1 -> "";
        _ -> "t"
    end.

encoding(Device) ->
    case io:getopts(Device) of
        List when is_list(List) ->
            proplists:get_value(encoding,List,latin1);
        _ ->
            latin1
    end.

