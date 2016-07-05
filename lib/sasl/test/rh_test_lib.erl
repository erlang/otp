-module(rh_test_lib).

-export([cmd/3]).
-export([erlsrv/3,
	 erlsrv/4]).
-export([get_service_args/3,
	 get_service_args/4,
	 get_start_erl_args/1,
	 get_start_erl_args/3,
	 get_client_args/3,
	 get_client_args/4]).
-export([clean_dir/1,
	 clean_dir/2]).

-include_lib("kernel/include/file.hrl").

cmd(Cmd,Args,Env) ->
    case open_port({spawn_executable, Cmd}, [{args,Args},{env,Env}]) of
        Port when is_port(Port) ->
            unlink(Port),
            catch erlang:port_close(Port), % migth already be closed, so catching
	    ok;
        Error ->
            Error
    end.

erlsrv(Erlsrv,Action,Name) ->
    erlsrv(Erlsrv,Action,Name,"").
erlsrv(Erlsrv,Action,Name,Rest) ->
    Cmd = "\"" ++ Erlsrv ++ "\" " ++ atom_to_list(Action) ++ " " ++
	Name ++ " " ++ Rest,
    io:format("erlsrv cmd: ~p~n",[Cmd]),
    Port = open_port({spawn, Cmd}, [stream, {line, 100}, eof, in]),
    Res = recv_prog_output(Port),
    case Res of
	[] ->
	    failed;
	_Y ->
	    io:format("erlsrv res: ~p~n",[_Y]),
	    ok
    end.

recv_prog_output(Port) ->
    receive
	{Port, {data, {eol,Data}}} ->
	    %%io:format("Got data: ~s~n", [Data]),
	    [ Data, "\n" | recv_prog_output(Port)];
	{Port, {data, {noeol,Data}}} ->
	    %%io:format("Got data: ~s~n", [Data]),
	    [ Data | recv_prog_output(Port)];
	{Port, _Other} ->
	    %%io:format("Got ~p from port~n", [_Other]),
	    Port ! {self(), close},
	    receive
		{Port,closed} ->
		    []
	    end
    end.

get_service_args(RootDir, Sname, StartErlArgs) ->
    get_service_args(RootDir, "", Sname, StartErlArgs).
get_service_args(RootDir, RelClientDir, Sname, StartErlArgs) ->
    LogDir = filename:nativename(filename:join([RootDir,RelClientDir,"log"])),
    %% start_erl.exe will be found since it is in the same directory as erlsrv.exe
    %% And heart_restart.bat will be found since the erts bin dir is
    %% always in the path for the erlang virtual machine.
    " -machine start_erl.exe -workdir " ++ LogDir ++
	" -debugtype new -sname " ++ atom_to_list(Sname) ++
	" -env HEART_COMMAND=heart_restart.bat -args \"" ++ StartErlArgs ++ "\"".

get_start_erl_args(RootDir) ->
    get_start_erl_args(RootDir,"","").
get_start_erl_args(RootDir,RelClientDir,ExtraArgs) ->
    Cookie = atom_to_list(erlang:get_cookie()),
    RelDir = filename:join([RootDir,RelClientDir,"releases"]),
    ExtraArgs ++ " -setcookie " ++ Cookie ++
	" -heart ++ -rootdir " ++ filename:nativename(RootDir) ++
	" -reldir " ++ filename:nativename(RelDir).

%% Must be called on the master node
get_client_args(Client,Sname,RootDir) ->
    get_client_args(Client,Sname,RootDir,node()).
get_client_args(Client,Sname,RootDir,Master) ->
    {ok,Host} = inet:gethostname(),
    Node = atom_to_list(Sname) ++ "@" ++ Host,
    RelClientDir = filename:join(["clients","type1",Node]),
    ClientDir = filename:join([RootDir,RelClientDir]),
    StartPrg = filename:join([ClientDir,"bin","start"]),
    {" -sasl start_prg \\\\\\\"" ++ StartPrg ++ "\\\\\\\" masters \[" ++
	 single_quote() ++ atom_to_list(Master) ++ single_quote() ++
	 get_client_extra_master(Client,Host) ++
	 "\] client_directory \\\\\\\"" ++ ClientDir ++  "\\\\\\\"" ++
	 get_client_loader_args(Client,Sname,Host),
     RelClientDir}.

get_client_loader_args(client1,Sname,Host) ->
    {ok,IpTuple} = inet:getaddr(Host,inet),
    IpAddr =  inet_parse:ntoa(IpTuple),
    " -loader inet -id " ++
	atom_to_list(Sname) ++ " -hosts " ++ IpAddr;
get_client_loader_args(_,_,_) ->
    "".

get_client_extra_master(client2,Host) ->
    "," ++ single_quote() ++ "master2@" ++ Host ++ single_quote();
get_client_extra_master(_,_) ->
    "".

single_quote() ->
    case os:type() of
	{win32,_} ->
	    "\'";
	_ ->
	    "\\'"
    end.

clean_dir(Dir) ->
    clean_dir(Dir,false).
clean_dir(Dir,Save) ->
    test_server:format("========  current dir ~tp~n",[Dir]),
    Dirs = filelib:wildcard(filename:join(Dir,"*")),
    test_server:format("========  deleting  ~tp~n",[Dirs]),

    ok = rm_rf(Dirs,Save),
    Remaining = filelib:wildcard(filename:join(Dir,"*")),
    test_server:format("========  remaining  ~tp~n",[Remaining]),

    case Remaining of
	[] ->
	    ok;
	_ ->
	    rm_rf(Remaining,Save),
	    Remaining2 = filelib:wildcard(filename:join(Dir,"*")),
	    test_server:format("========  remaining after second try ~tp~n",
			       [Remaining2])
    end,

    ok.


rm_rf([File|Files],Save) ->
    case Save andalso filename:basename(File)=="save" of
	true ->
	    rm_rf(Files,Save);
	false ->
	    case file:read_link_info(File) of
		{ok,#file_info{type=directory}} ->
		    MoreFiles = filelib:wildcard(filename:join(File,"*")),
		    rm_rf(MoreFiles,Save),
		    file:del_dir(File),
		    rm_rf(Files,Save);
		{ok,#file_info{}} ->
		    file:delete(File),
		    rm_rf(Files,Save);
		Other ->
		    test_server:format("========  could not delete file  ~p~n"
				       "read_link_info -> ~p~n",[File,Other]),
		    rm_rf(Files,Save)
	    end
    end;
rm_rf([],_) ->
    ok.
