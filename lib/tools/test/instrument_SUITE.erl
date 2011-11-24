%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2011. All Rights Reserved.
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
-module(instrument_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2]).

-export(['+Mim true'/1, '+Mis true'/1]).

-include_lib("test_server/include/test_server.hrl").

init_per_testcase(_Case, Config) ->
    ?line Dog=?t:timetrap(10000),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    ['+Mim true', '+Mis true'].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


'+Mim true'(doc) -> ["Check that memory data can be read and processed"];
'+Mim true'(suite) -> [];
'+Mim true'(Config) when is_list(Config) ->
    ?line Node = start_slave("+Mim true"),
    ?line MD = rpc:call(Node, instrument, memory_data, []),
    ?line [{total,[{sizes,S1,S2,S3},{blocks,B1,B2,B3}]}]
	= rpc:call(Node, instrument, memory_status, [total]),
    ?line stop_slave(Node),
    ?line true = S1 =< S2,
    ?line true = S2 =< S3,
    ?line true = B1 =< B2,
    ?line true = B2 =< B3,
    ?line MDS = instrument:sort(MD),
    ?line {Low, High} = instrument:mem_limits(MDS),
    ?line true = Low < High,
    ?line {_, AL} = MDS,
    ?line SumBlocks = instrument:sum_blocks(MD),
    ?line case SumBlocks of
	      N when is_integer(N) ->
		  ?line N = lists:foldl(fun ({_,_,Size,_}, Sum) ->
						Size+Sum
					end,
					0,
					AL),
		  ?line N =< S3;
	      Other ->
		  ?line ?t:fail(Other)
	  end,
    ?line lists:foldl(
	    fun ({TDescr,Addr,Size,Proc}, MinAddr) ->
		    ?line true = TDescr /= invalid_type,
		    ?line true = is_integer(TDescr),
		    ?line true = is_integer(Addr),
		    ?line true = is_integer(Size),
		    ?line true = Addr >= MinAddr,
		    ?line case Proc of
			      {0, Number, Serial} ->
				  ?line true = is_integer(Number),
				  ?line true = is_integer(Serial);
			      undefined ->
				  ok;
			      BadProc ->
				  ?line ?t:fail({badproc, BadProc})
			  end,
		    ?line NextMinAddr = Addr+Size,
		    ?line true = NextMinAddr =< High,
		    ?line NextMinAddr
	    end,
	    Low,
	    AL),
    ?line {_, DAL} = instrument:descr(MDS),
    ?line lists:foreach(
	    fun ({TDescr,_,_,Proc}) ->
		    ?line true = TDescr /= invalid_type,
		    ?line true = is_atom(TDescr) orelse is_list(TDescr),
		    ?line true = is_pid(Proc) orelse Proc == undefined
	    end,
	    DAL),
    ?line ASL = lists:map(fun ({_,A,S,_}) -> {A,S} end, AL),
    ?line ASL = lists:map(fun ({_,A,S,_}) -> {A,S} end, DAL),
    ?line instrument:holes(MDS),
    ?line {comment,
	   "total status - sum of blocks = " ++ integer_to_list(S1-SumBlocks)}.

'+Mis true'(doc) -> ["Check that memory data can be read and processed"];
'+Mis true'(suite) -> [];
'+Mis true'(Config) when is_list(Config) ->
    ?line Node = start_slave("+Mis true"),
    ?line [{total,[{sizes,S1,S2,S3},{blocks,B1,B2,B3}]}]
	= rpc:call(Node, instrument, memory_status, [total]),
    ?line true = S1 =< S2,
    ?line true = S2 =< S3,
    ?line true = B1 =< B2,
    ?line true = B2 =< B3,
    ?line true = is_list(rpc:call(Node,instrument,memory_status,[allocators])),
    ?line true = is_list(rpc:call(Node,instrument,memory_status,[classes])),
    ?line true = is_list(rpc:call(Node,instrument,memory_status,[types])),
    ?line ok.

start_slave(Args) ->
    ?line {A, B, C} = now(),
    ?line MicroSecs = A*1000000000000 + B*1000000 + C,
    ?line Name = "instr_" ++ integer_to_list(MicroSecs),
    ?line Pa = filename:dirname(code:which(?MODULE)),
    ?line {ok, Node} = ?t:start_node(list_to_atom(Name),
				     slave,
				     [{args, "-pa " ++ Pa ++ " " ++ Args}]),
    ?line Node.


stop_slave(Node) ->
    ?line true = ?t:stop_node(Node).
