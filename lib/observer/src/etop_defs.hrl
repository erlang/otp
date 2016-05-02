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
-define(SYSFORM,
	" ~-72w~10s~n"
	" Load:  cpu  ~8w               Memory:  total    ~8w    binary   ~8w~n"
	"        procs~8w                        processes~8w    code     ~8w~n"
	"        runq ~8w                        atom     ~8w    ets      ~8w~n").

-record(opts, {node=node(), port = 8415, accum = false, intv = 5000, lines = 10,
	       width = 700, height = 340, sort = runtime, tracing = on,
	       %% Other state information
	       out_mod=etop_txt, out_proc, server, host, tracer, store,
	       accum_tab, remote}).
