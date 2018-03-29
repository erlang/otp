%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

-module(ftp_simple_client_server).

-compile(export_all).

-ifndef(EQC).
-ifndef(PROPER).
-define(EQC,true).
%%-define(PROPER,true).
-endif.
-endif.


-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-define(MOD_eqc, eqc).
-define(MOD_eqc_gen, eqc_gen).
-define(MOD_eqc_statem, eqc_statem).

-else.
-ifdef(PROPER).

-include_lib("proper/include/proper.hrl").
-define(MOD_eqc, proper).
-define(MOD_eqc_gen, proper_gen).
-define(MOD_eqc_statem, proper_statem).

-endif.
-endif.

-record(state, {
	  initialized = false,
	  priv_dir,
	  data_dir,
	  servers = [], % [ {IP,Port,Userid,Pwd} ]
	  clients = [], % [ client_ref() ]
	  store = []    % [ {Name,Contents} ]
	 }).

-define(fmt(F,A), io:format(F,A)).
%%-define(fmt(F,A), ok).

-define(v(K,L), proplists:get_value(K,L)).

%%%================================================================
%%%
%%% Properties
%%% 

%% This function is for normal eqc calls:
prop_ftp() ->
    {ok,PWD} = file:get_cwd(),
    prop_ftp(filename:join([PWD,?MODULE_STRING++"_data"]),
	     filename:join([PWD,?MODULE_STRING,"_files"])).

%% This function is for calls from common_test test cases:
prop_ftp(Config) ->
    prop_ftp(filename:join([?v(property_dir,Config), ?MODULE_STRING++"_data"]),
	     ?v(priv_dir,Config) ).


prop_ftp(DataDir, PrivDir) ->
    S0 = #state{data_dir = DataDir,
		priv_dir = PrivDir},
    ?FORALL(Cmds,  more_commands(10,commands(?MODULE,S0)),
	 aggregate(command_names(Cmds),
	   begin {_H,S,Result} = run_commands(?MODULE,Cmds),
		 % io:format('**** Result=~p~n',[Result]),
		 % io:format('**** S=~p~n',[S]),
		 % io:format('**** _H=~p~n',[_H]),
		 % io:format('**** Cmds=~p~n',[Cmds]),
		 [cmnd_stop_server(X) || X <- S#state.servers],
		 [ftp:stop_service(X) || {ok,X} <- S#state.clients],
		 Result==ok
	   end)
	   ).

%%%================================================================
%%%
%%% State model
%%% 

%% @doc Returns the state in which each test case starts. (Unless a different 
%%      initial state is supplied explicitly to, e.g. commands/2.)
-spec initial_state() ->?MOD_eqc_statem:symbolic_state().
initial_state() -> 
    ?fmt("Initial_state()~n",[]),
    #state{}.

%% @doc Command generator, S is the current state
-spec command(S :: ?MOD_eqc_statem:symbolic_state()) -> ?MOD_eqc_gen:gen(eqc_statem:call()).

command(#state{initialized=false,
	       priv_dir=PrivDir}) -> 
    {call,?MODULE,cmnd_init,[PrivDir]};

command(#state{servers=[],
	       priv_dir=PrivDir,
	       data_dir=DataDir}) -> 
    {call,?MODULE,cmnd_start_server,[PrivDir,DataDir]};

command(#state{servers=Ss=[_|_],
	       clients=[]}) -> 
    {call,?MODULE,cmnd_start_client,[oneof(Ss)]};

command(#state{servers=Ss=[_|_],
	       clients=Cs=[_|_],
	       store=Store=[_|_]
	      }) ->
    frequency([
	       { 5, {call,?MODULE,cmnd_start_client,[oneof(Ss)]}},
	       { 5, {call,?MODULE,cmnd_stop_client,[oneof(Cs)]}},
	       {10, {call,?MODULE,cmnd_put,[oneof(Cs),file_path(),file_contents()]}},
	       {20, {call,?MODULE,cmnd_get,[oneof(Cs),oneof(Store)]}},
	       {10, {call,?MODULE,cmnd_delete,[oneof(Cs),oneof(Store)]}}
	      ]);
		 
command(#state{servers=Ss=[_|_],
	       clients=Cs=[_|_],
	       store=[]
	      }) ->
    frequency([
	       {5, {call,?MODULE,cmnd_start_client,[oneof(Ss)]}},
	       {5, {call,?MODULE,cmnd_stop_client,[oneof(Cs)]}},
	       {10, {call,?MODULE,cmnd_put,[oneof(Cs),file_path(),file_contents()]}}
	      ]).
		 
%% @doc Precondition, checked before command is added to the command sequence. 
-spec precondition(S :: ?MOD_eqc_statem:symbolic_state(), C :: ?MOD_eqc_statem:call()) -> boolean().

precondition(#state{clients=Cs}, {call, _, cmnd_put, [C,_,_]}) -> lists:member(C,Cs);

precondition(#state{clients=Cs, store=Store}, 
	     {call, _, cmnd_get, [C,X]}) -> lists:member(C,Cs) andalso lists:member(X,Store);

precondition(#state{clients=Cs, store=Store},
	     {call, _, cmnd_delete, [C,X]}) -> lists:member(C,Cs) andalso lists:member(X,Store);

precondition(#state{servers=Ss}, {call, _, cmnd_start_client, _}) ->  Ss =/= [];

precondition(#state{clients=Cs}, {call, _, cmnd_stop_client, [C]}) -> lists:member(C,Cs);

precondition(#state{initialized=IsInit}, {call, _, cmnd_init, _}) -> IsInit==false;

precondition(_S, {call, _, _, _}) -> true.


%% @doc Postcondition, checked after command has been evaluated
%%      Note: S is the state before next_state(S,_,C) 
-spec postcondition(S :: ?MOD_eqc_statem:dynamic_state(), C :: ?MOD_eqc_statem:call(), 
                    Res :: term()) -> boolean().

postcondition(_S, {call, _, cmnd_get, [_,{_Name,Expected}]}, {ok,Value}) ->
    Value == Expected;

postcondition(S, {call, _, cmnd_delete, [_,{Name,_Expected}]}, ok) ->
    ?fmt("file:read_file(..) = ~p~n",[file:read_file(filename:join(S#state.priv_dir,Name))]),
    {error,enoent} == file:read_file(filename:join(S#state.priv_dir,Name));

postcondition(S, {call, _, cmnd_put,  [_,Name,Value]}, ok) -> 
    {ok,Bin} = file:read_file(filename:join(S#state.priv_dir,Name)),
    Bin == unicode:characters_to_binary(Value);

postcondition(_S, {call, _, cmnd_stop_client, _}, ok) -> true;

postcondition(_S, {call, _, cmnd_start_client, _}, {ok,_}) -> true;

postcondition(_S, {call, _, cmnd_init, _}, ok) -> true;

postcondition(_S, {call, _, cmnd_start_server, _}, {ok,_}) -> true.


%% @doc Next state transformation, S is the current state. Returns next state.
-spec next_state(S :: ?MOD_eqc_statem:symbolic_state(), 
		 V :: ?MOD_eqc_statem:var(), 
                 C :: ?MOD_eqc_statem:call()) -> ?MOD_eqc_statem:symbolic_state().

next_state(S, _V, {call, _, cmnd_put, [_,Name,Val]}) ->
    S#state{store = [{Name,Val} | lists:keydelete(Name,1,S#state.store)]};

next_state(S, _V, {call, _, cmnd_delete, [_,{Name,_Val}]}) ->
    S#state{store = lists:keydelete(Name,1,S#state.store)};

next_state(S, V, {call, _, cmnd_start_client, _}) ->
    S#state{clients = [V | S#state.clients]};

next_state(S, V, {call, _, cmnd_start_server, _}) ->
    S#state{servers = [V | S#state.servers]};

next_state(S, _V, {call, _, cmnd_stop_client, [C]}) ->
    S#state{clients = S#state.clients -- [C]};

next_state(S, _V, {call, _, cmnd_init, _}) ->
    S#state{initialized=true};

next_state(S, _V, {call, _, _, _}) ->
    S.

%%%================================================================
%%%
%%% Data model
%%% 

file_path() -> non_empty(list(alphanum_char())).
%%file_path() -> non_empty( list(oneof([alphanum_char(), utf8_char()])) ).

%%file_contents() -> list(alphanum_char()).
file_contents() -> list(oneof([alphanum_char(), utf8_char()])).
    
alphanum_char() -> oneof(lists:seq($a,$z) ++ lists:seq($A,$Z) ++ lists:seq($0,$9)).

utf8_char() -> oneof("åäöÅÄÖ話话カタカナひらがな").

%%%================================================================
%%%
%%% Commands doing something with the System Under Test
%%% 

cmnd_init(PrivDir) ->
    ?fmt('Call cmnd_init(~p)~n',[PrivDir]),
    os:cmd("killall vsftpd"),
    clear_files(PrivDir),
    ok.

cmnd_start_server(PrivDir, DataDir) ->
    ?fmt('Call cmnd_start_server(~p, ~p)~n',[PrivDir,DataDir]),
    Cmnd = ["vsftpd ", filename:join(DataDir,"vsftpd.conf"),
	    " -oftpd_banner=erlang_otp_testing"
	    " -oanon_root=",PrivDir
	   ],
    ?fmt("Cmnd=~s~n",[Cmnd]),
    case os:cmd(Cmnd) of
	[] ->
	    {ok,{"localhost",9999,"ftp","usr@example.com"}};
	Other ->
	    {error,Other}
    end.

cmnd_stop_server({ok,{_Host,Port,_Usr,_Pwd}}) ->
    os:cmd("kill `netstat -tpln | grep "++integer_to_list(Port)++" | awk '{print $7}' | awk -F/ '{print $1}'`").

cmnd_start_client({ok,{Host,Port,Usr,Pwd}}) ->
    ?fmt('Call cmnd_start_client(~p)...',[{Host,Port,Usr,Pwd}]),
    case ftp:start_service([{host,Host},{port,Port}]) of
	{ok,Client} ->
	    ?fmt("~p...",[{ok,Client}]),
	    case ftp:user(Client, Usr, Pwd) of
		ok -> 
		    ?fmt("OK!~n",[]),
		    {ok,Client};
		Other -> 
		    ?fmt("Other1=~p~n",[Other]),
		    ftp:stop_service(Client), Other
	    end;
	Other -> 
	    ?fmt("Other2=~p~n",[Other]),
	    Other
    end.
		     
cmnd_stop_client({ok,Client}) ->
    ?fmt('Call cmnd_stop_client(~p)~n',[Client]),
    ftp:stop_service(Client). %% -> ok | Other

cmnd_delete({ok,Client}, {Name,_ExpectedValue}) ->
    ?fmt('Call cmnd_delete(~p, ~p)~n',[Client,Name]),
    R=ftp:delete(Client, Name),
    ?fmt("R=~p~n",[R]),
    R.

cmnd_put({ok,Client}, Name, Value) ->
    ?fmt('Call cmnd_put(~p, ~p, ~p)...',[Client, Name, Value]),
    R = ftp:send_bin(Client, unicode:characters_to_binary(Value), Name), % ok | {error,Error}
    ?fmt('~p~n',[R]),
    R.

cmnd_get({ok,Client}, {Name,_ExpectedValue}) ->
    ?fmt('Call cmnd_get(~p, ~p)~n',[Client,Name]),
    case ftp:recv_bin(Client, Name) of
	{ok,Bin} -> {ok, unicode:characters_to_list(Bin)};
	Other -> Other
    end.


clear_files(Dir) ->
    os:cmd(["rm -fr ",filename:join(Dir,"*")]).
