%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2017. All Rights Reserved.
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
-module(ct_webtool).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% The general idea is:                                               %%
%%                                                                    %%
%%                                                                    %%
%% 1. Scan through the path for *.tool files and find all the web     %%
%%    based tools. Query each tool for configuration data.            %%
%% 2. Add Alias for Erlscript and html for each tool to               %%
%%    the webserver configuration data.                               %%
%% 3. Start the webserver.                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API functions
-export([start/0, start/2, stop/0]).

%% Starting Webtool from a shell script
-export([script_start/0, script_start/1]).

%% Web api
-export([started_tools/2, toolbar/2, start_tools/2, stop_tools/2]).

%% API against other tools
-export([is_localhost/0]).

%% Debug export s
-export([get_tools1/1]).
-export([debug/1, stop_debug/0, debug_app/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state,{priv_dir,app_data,supvis,web_data,started=[]}).

-define(MAX_NUMBER_OF_WEBTOOLS,256).
-define(DEFAULT_PORT,8888).% must be >1024 or the user must be root on unix
-define(DEFAULT_ADDR,{127,0,0,1}).

-define(WEBTOOL_ALIAS,{ct_webtool,[{alias,{erl_alias,"/ct_webtool",[ct_webtool]}}]}).
-define(HEADER,"Pragma:no-cache\r\n Content-type: text/html\r\n\r\n").
-define(HTML_HEADER,"<HTML>\r\n<HEAD>\r\n<TITLE>WebTool</TITLE>\r\n</HEAD>\r\n<BODY BGCOLOR=\"#FFFFFF\">\r\n").
-define(HTML_HEADER_RELOAD,"<HTML>\r\n<HEAD>\r\n<TITLE>WebTool
                             </TITLE>\r\n</HEAD>\r\n
                             <BODY BGCOLOR=\"#FFFFFF\" onLoad=reloadCompiledList()>\r\n").

-define(HTML_END,"</BODY></HTML>").

-define(SEND_URL_TIMEOUT,5000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% For debugging only.                                              %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start tracing with
%% debug(Functions).
%% Functions = local | global | FunctionList
%% FunctionList = [Function]
%% Function = {FunctionName,Arity} | FunctionName |
%%            {Module, FunctionName, Arity} | {Module,FunctionName}
debug(F) -> 
    {ok, _} = ttb:tracer(all,[{file,"webtool.trc"}]), % tracing all nodes
    {ok, _} = ttb:p(all,[call,timestamp]),
    MS = [{'_',[],[{return_trace},{message,{caller}}]}],
    _ = tp(F,MS),
    {ok, _} = ttb:ctp(?MODULE,stop_debug), % don't want tracing of the stop_debug func
    ok.
tp(local,MS) -> % all functions
    ttb:tpl(?MODULE,MS);
tp(global,MS) -> % all exported functions
    ttb:tp(?MODULE,MS);
tp([{M,F,A}|T],MS) -> % Other module
    {ok, _} = ttb:tpl(M,F,A,MS),
    tp(T,MS);
tp([{M,F}|T],MS) when is_atom(F) -> % Other module
    {ok, _} = ttb:tpl(M,F,MS),
    tp(T,MS);
tp([{F,A}|T],MS) -> % function/arity
    {ok, _} = ttb:tpl(?MODULE,F,A,MS),
    tp(T,MS);
tp([F|T],MS) -> % function
    {ok, _} = ttb:tpl(?MODULE,F,MS),
    tp(T,MS);
tp([],_MS) ->
    ok.
stop_debug() ->
    ttb:stop([format]).

debug_app(Mod) ->
    {ok, _} = ttb:tracer(all,[{file,"webtool_app.trc"},{handler,{fun out/4,true}}]),
    {ok, _} = ttb:p(all,[call,timestamp]),
    MS = [{'_',[],[{return_trace},{message,{caller}}]}],
    {ok, _} = ttb:tp(Mod,MS),
    ok.
   
out(_,{trace_ts,Pid,call,MFA={M,F,A},{W,_,_},TS},_,S) 
  when W==webtool;W==mod_esi-> 
    io:format("~w: (~p)~ncall ~ts~n", [TS,Pid,ffunc(MFA)]),
    [{M,F,length(A)}|S];
out(_,{trace_ts,Pid,return_from,MFA,R,TS},_,[MFA|S]) ->
    io:format("~w: (~p)~nreturned from ~ts -> ~tp~n", [TS,Pid,ffunc(MFA),R]),
    S;
out(_,_,_,_) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% Functions called via script.                                     %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
script_start() ->
    usage(),
    halt().
script_start([App]) ->
    DefaultBrowser = 
	case os:type() of
	    {win32,_} -> iexplore;
	    _ -> firefox
	end,
    script_start([App,DefaultBrowser]);
script_start([App,Browser]) ->
    io:format("Starting webtool...\n"),
    {ok, _} = start(),
    AvailableApps = get_applications(),
    {OSType,_} = os:type(),
    case lists:keysearch(App,1,AvailableApps) of
	{value,{App,StartPage}} ->
	    io:format("Starting ~w...\n",[App]),
	    start_tools([],"app=" ++ atom_to_list(App)),
	    PortStr = integer_to_list(get_port()),
	    Url = case StartPage of
		      "/" ++ Page -> 
			  "http://localhost:" ++ PortStr ++ "/" ++ Page;
		      _ -> 
			  "http://localhost:" ++ PortStr ++ "/" ++ StartPage
		  end,
	    _ = case Browser of 
		none ->
		    ok;
                iexplore when OSType == win32->
                    io:format("Starting internet explorer...\n"),
                    {ok,R} = win32reg:open(""),
		    Key="\\local_machine\\SOFTWARE\\Microsoft\\IE Setup\\Setup",
                    ok = win32reg:change_key(R,Key),
                    {ok,Val} = win32reg:value(R,"Path"),
		    IExplore=filename:join(win32reg:expand(Val),"iexplore.exe"),
                    os:cmd("\"" ++ IExplore ++ "\" " ++ Url);
		_ when OSType == win32 ->
                    io:format("Starting ~tw...\n",[Browser]),
                    os:cmd("\"" ++ atom_to_list(Browser) ++ "\" " ++ Url);
		B when B==firefox; B==mozilla ->
		    io:format("Sending URL to ~w...",[Browser]),
		    BStr = atom_to_list(Browser),
		    SendCmd = BStr ++ " -raise -remote \'openUrl(" ++ 
			Url ++ ")\'",
		    Port = open_port({spawn,SendCmd},[exit_status]),
		    receive 
			{Port,{exit_status,0}} -> 
			    io:format("done\n"),
			    ok;
			{Port,{exit_status,_Error}} ->
			    io:format(" not running, starting ~w...\n",
				      [Browser]),
			    _ = os:cmd(BStr ++ " " ++ Url),
			    ok
		    after ?SEND_URL_TIMEOUT ->
			    io:format(" failed, starting ~w...\n",[Browser]),
			    erlang:port_close(Port),
			    os:cmd(BStr ++ " " ++ Url)
		    end;
		_ ->
		    io:format("Starting ~tw...\n",[Browser]),
		    os:cmd(atom_to_list(Browser) ++ " " ++ Url)
	    end,
	    ok;
	false ->
	    stop(),
	    io:format("\n{error,{unknown_app,~p}}\n",[App]),
	    halt()
    end.

usage() ->
    io:format("Starting webtool...\n"),
    {ok, _} = start(),
    Apps = lists:map(fun({A,_}) -> A end,get_applications()),
    io:format(
      "\nUsage: start_webtool application [ browser ]\n"
      "\nAvailable applications are: ~p\n"
      "Default browser is \'iexplore\' (Internet Explorer) on Windows "
      "or else \'firefox\'\n",
      [Apps]),
    stop().


get_applications() ->
    gen_server:call(ct_web_tool,get_applications).
    
get_port() ->
    gen_server:call(ct_web_tool,get_port).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% Api functions to the genserver.                                  %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----------------------------------------------------------------------
%
%----------------------------------------------------------------------

start()->
    start(standard_path,standard_data).

start(Path,standard_data)->
    case get_standard_data() of
	{error,Reason} ->
	    {error,Reason};
	Data ->
	    start(Path,Data)
    end;
    
start(standard_path,Data)->
    Path=get_path(),
    start(Path,Data);
    
start(Path,Port) when is_integer(Port)->
    Data = get_standard_data(Port),
    start(Path,Data);
	
start(Path,Data0)->
    Data = Data0 ++ rest_of_standard_data(),
    case gen_server:start({local,ct_web_tool},ct_webtool,{Path,Data},[]) of
		{error, {already_started, Pid}} ->
		    {ok, Pid};
		Else ->
		    Else
    end.

stop()->
    gen_server:call(ct_web_tool,stoppit).

%----------------------------------------------------------------------
%Web Api functions called by the web
%----------------------------------------------------------------------
started_tools(Env,Input)->
    gen_server:call(ct_web_tool,{started_tools,Env,Input}).

toolbar(Env,Input)->    
    gen_server:call(ct_web_tool,{toolbar,Env,Input}).

start_tools(Env,Input)->
    gen_server:call(ct_web_tool,{start_tools,Env,Input}).

stop_tools(Env,Input)->
    gen_server:call(ct_web_tool,{stop_tools,Env,Input}).
%----------------------------------------------------------------------
%Support API for other tools
%----------------------------------------------------------------------

is_localhost()->
    gen_server:call(ct_web_tool,is_localhost).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%%The gen_server callback functions that builds the webbpages       %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(get_applications,_,State)->
    MS = ets:fun2ms(fun({Tool,{web_data,{_,Start}}}) -> {Tool,Start} end),
    Tools = ets:select(State#state.app_data,MS),
    {reply,Tools,State};

handle_call(get_port,_,State)->
    {value,{port,Port}}=lists:keysearch(port,1,State#state.web_data),
    {reply,Port,State};

handle_call({started_tools,_Env,_Input},_,State)->
    {reply,started_tools_page(State),State};

handle_call({toolbar,_Env,_Input},_,State)->
    {reply,toolbar(),State};

handle_call({start_tools,Env,Input},_,State)->
    {NewState,Page}=start_tools_page(Env,Input,State),
    {reply,Page,NewState};

handle_call({stop_tools,Env,Input},_,State)->
    {NewState,Page}=stop_tools_page(Env,Input,State),
    {reply,Page,NewState};

handle_call(stoppit,_From,Data)->
    {stop,normal,ok,Data};

handle_call(is_localhost,_From,Data)->
    Result=case proplists:get_value(bind_address, Data#state.web_data) of
	?DEFAULT_ADDR ->
	    true;
	_IpNumber ->
	    false 
    end,
    {reply,Result,Data}.


handle_info(_Message,State)->
    {noreply,State}.

handle_cast(_Request,State)->
    {noreply,State}.

code_change(_,State,_)->
    {ok,State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The other functions needed by the gen_server behaviour 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----------------------------------------------------------------------
% Start the gen_server
%----------------------------------------------------------------------
init({Path,Config})->
    ct_util:mark_process(),
    case filelib:is_dir(Path) of
	true ->
	    {ok, Table} = get_tool_files_data(),
	    insert_app(?WEBTOOL_ALIAS, Table),
	    case ct_webtool_sup:start_link() of
		{ok, Pid} ->
		    case start_webserver(Table, Path, Config) of
			{ok, _} ->
			    print_url(Config),	
			    {ok,#state{priv_dir=Path,
				       app_data=Table,
				       supvis=Pid,
				       web_data=Config}};
			{error, Error} ->
			    {stop, {error, Error}}
		    end;
		Error ->
		    {stop,Error}
	    end;
	false ->
	   {stop, {error, error_dir}}
    end.

terminate(_Reason,Data)->
    %%shut down the webbserver
    shutdown_server(Data),
    %%Shutdown the different tools that are started with application:start
    shutdown_apps(Data),
    %%Shutdown the supervisor and its children will die
    shutdown_supervisor(Data),
    ok.

print_url(ConfigData)->
    Server=proplists:get_value(server_name,ConfigData,"undefined"),
    Port=proplists:get_value(port,ConfigData,"undefined"),
    {A,B,C,D}=proplists:get_value(bind_address,ConfigData,"undefined"),
    io:format("WebTool is available at http://~ts:~w/~n",[Server,Port]),
    io:format("Or  http://~w.~w.~w.~w:~w/~n",[A,B,C,D,Port]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% begin build the pages
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------
%The page that shows the started tools
%----------------------------------------------------------------------
started_tools_page(State)->
    [?HEADER,?HTML_HEADER,started_tools(State),?HTML_END].

toolbar()->
    [?HEADER,?HTML_HEADER,toolbar_page(),?HTML_END].

               
start_tools_page(_Env,Input,State)->
    %%io:format("~n======= ~n ~p ~n============~n",[Input]),
    case get_tools(Input) of
	{tools,Tools}->
	    %%io:format("~n======= ~n ~p ~n============~n",[Tools]),
	    {ok,NewState}=handle_apps(Tools,State,start),
	    {NewState,[?HEADER,?HTML_HEADER_RELOAD,reload_started_apps(),
		       show_unstarted_apps(NewState),?HTML_END]};
	_ ->
	    {State,[?HEADER,?HTML_HEADER,show_unstarted_apps(State),?HTML_END]}
    end.

stop_tools_page(_Env,Input,State)->
    case get_tools(Input) of
	{tools,Tools}->
	    {ok,NewState}=handle_apps(Tools,State,stop),
	    {NewState,[?HEADER,?HTML_HEADER_RELOAD,reload_started_apps(),
		       show_started_apps(NewState),?HTML_END]};
	_ ->
	    {State,[?HEADER,?HTML_HEADER,show_started_apps(State),?HTML_END]}
    end.
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Functions that start and config the webserver
%% 1. Collect the config data
%% 2. Start webserver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------
% Start the webserver
%----------------------------------------------------------------------
start_webserver(Data,Path,Config)->
    case get_conf_data(Data,Path,Config) of
	{ok,Conf_data}->
	    %%io:format("Conf_data: ~p~n",[Conf_data]),
	    start_server(Conf_data);
	{error,Error} ->
	    {error,{error_server_conf_file,Error}}
    end.

start_server(Conf_data)->
    case inets:start(httpd, Conf_data, stand_alone) of
	{ok,Pid}->
	    {ok,Pid};
	Error->
	    {error,{server_error,Error}}
    end.

%----------------------------------------------------------------------
% Create config data for the webserver 
%----------------------------------------------------------------------
get_conf_data(Data,Path,Config)->
    Aliases=get_aliases(Data),
    ServerRoot = filename:join([Path,"root"]),
    MimeTypesFile = filename:join([ServerRoot,"conf","mime.types"]),
    case httpd_conf:load_mime_types(MimeTypesFile) of
	{ok,MimeTypes} ->
	    Config1 = Config ++ Aliases,
	    Config2 = [{server_root,ServerRoot},
			    {document_root,filename:join([Path,"root/doc"])},
			    {mime_types,MimeTypes} |
			    Config1],
	    {ok,Config2};
	Error ->
	    Error
    end.

%----------------------------------------------------------------------
% Control the path for *.tools files 
%----------------------------------------------------------------------
get_tool_files_data()->
    Tools=get_tools1(code:get_path()),
    %%io:format("Data : ~p ~n",[Tools]),
    get_file_content(Tools).

%----------------------------------------------------------------------
%Control that the data in the file really is erlang terms
%---------------------------------------------------------------------- 
get_file_content(Tools)->
    Get_data=fun({tool,ToolData}) ->
		     %%io:format("Data : ~p ~n",[ToolData]),
		     case proplists:get_value(config_func,ToolData) of
			 {M,F,A}->
			     case catch apply(M,F,A) of
				 {'EXIT',_} ->
				     bad_data;
				 Data when is_tuple(Data) ->
				     Data;
				 _->
				     bad_data  
			     end;
			 _ ->
				bad_data
		     end
	     end,
    insert_file_content([X ||X<-lists:map(Get_data,Tools),X/=bad_data]).

%----------------------------------------------------------------------
%Insert the data from the file in to the ets:table
%----------------------------------------------------------------------
insert_file_content(Content)->
    Table=ets:new(app_data,[bag]),
    lists:foreach(fun(X)->
			  insert_app(X,Table)
		  end,Content),
    {ok,Table}.

%----------------------------------------------------------------------
%Control that we got a a tuple of a atom and a list if so add the 
%elements in the list to the ets:table
%----------------------------------------------------------------------
insert_app({Name,Key_val_list},Table) when is_list(Key_val_list),is_atom(Name)->
    %%io:format("ToolData: ~p: ~p~n",[Name,Key_val_list]),
    lists:foreach(
      fun({alias,{erl_alias,Alias,Mods}}) ->
	      Key_val = {erl_script_alias,{Alias,Mods}},
	      %%io:format("Insert: ~p~n",[Key_val]),
	      ets:insert(Table,{Name,Key_val});
	 (Key_val_pair)->
	      %%io:format("Insert: ~p~n",[Key_val_pair]),
	      ets:insert(Table,{Name,Key_val_pair})
      end,
      Key_val_list);

insert_app(_,_)->
    ok.
   
%----------------------------------------------------------------------
% Select all the alias in the database
%----------------------------------------------------------------------
get_aliases(Data)->
    MS = ets:fun2ms(fun({_,{erl_script_alias,Alias}}) -> 
			    {erl_script_alias,Alias};
		       ({_,{alias,Alias}}) -> 
			    {alias,Alias} 
		    end),
    ets:select(Data,MS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% Helper functions                                                 %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_standard_data(Port)->
    [
     {port,Port},
     {bind_address,?DEFAULT_ADDR},
     {server_name,"localhost"}
    ].

get_standard_data()->
    case get_free_port(?DEFAULT_PORT,?MAX_NUMBER_OF_WEBTOOLS) of
	{error,Reason} -> {error,Reason};
	Port ->
	    [
	     {port,Port},
	     {bind_address,?DEFAULT_ADDR},
	     {server_name,"localhost"}
	    ]
    end.

get_free_port(_Port,0) ->
    {error,no_free_port_found};
get_free_port(Port,N) ->
    case gen_tcp:connect("localhost",Port,[]) of
	{error, _Reason} ->
	    Port;
	{ok,Sock} ->
	    gen_tcp:close(Sock),
	    get_free_port(Port+1,N-1)
    end.

rest_of_standard_data() ->
    [
     %% Do not allow the server to be crashed by malformed http-request
     {max_header_siz,1024},
     {max_header_action,reply414},
     %% Go on a straight ip-socket
     {com_type,ip_comm},
     %% Do not change the order of these module names!!
     {modules,[mod_alias,
	       mod_auth,
	       mod_esi,
	       mod_actions,
	       mod_cgi,
	       mod_include,
	       mod_dir,
	       mod_get,
	       mod_head,
	       mod_log,
	       mod_disk_log]},
     {directory_index,["index.html"]},
     {default_type,"text/plain"}
    ].


get_path()->
    code:priv_dir(webtool).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These functions is used to shutdown the webserver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------
% Shut down the webbserver 
%----------------------------------------------------------------------
shutdown_server(State)->
    {Addr,Port} = get_addr_and_port(State#state.web_data),
    inets:stop(httpd,{Addr,Port}).

get_addr_and_port(Config) ->
    Addr = proplists:get_value(bind_address,Config,?DEFAULT_ADDR),
    Port = proplists:get_value(port,Config,?DEFAULT_PORT),
    {Addr,Port}.

%----------------------------------------------------------------------
% Select all apps in the table and close them
%----------------------------------------------------------------------
shutdown_apps(State)->
    Data=State#state.app_data,
    MS = ets:fun2ms(fun({_,{start,HowToStart}}) -> HowToStart end),
    lists:foreach(fun(Start_app)->
			  stop_app(Start_app)
		  end,
		  ets:select(Data,MS)).

%----------------------------------------------------------------------
%Shuts down the supervisor that supervises tools that is not
%Designed as applications
%----------------------------------------------------------------------
shutdown_supervisor(State)->
    %io:format("~n==================~n"),
    ct_webtool_sup:stop(State#state.supvis).
    %io:format("~n==================~n").

%----------------------------------------------------------------------
%close the individual apps.
%----------------------------------------------------------------------  
stop_app({child,_Real_name})->
    ok;

stop_app({app,Real_name})->
    application:stop(Real_name);

stop_app({func,_Start,Stop})->    
    case Stop of
	{M,F,A} ->
	    catch apply(M,F,A);
	_NoStop ->
	    ok
    end.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% These functions creates the webpage where the user can select if 
%% to start apps or to stop apps
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

toolbar_page()->
    "<TABLE>
       <TR>
         <TD>
             <B>Select Action</B>
         </TD>
       </TR>
       <TR>
         <TD>
            <A HREF=\"./start_tools\" TARGET=right> Start Tools</A>
         </TD>
       </TR>
       <TR>
         <TD>
            <A HREF=\"./stop_tools\" TARGET=right> Stop Tools</A>
	 </TD> 
      </TR> 
    </TABLE>".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% These functions creates the webbpage that  shows the started apps
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------
% started_tools(State)->String (html table)
% State is a record of type state
%----------------------------------------------------------------------
started_tools(State)->
    Names=get_started_apps(State#state.app_data,State#state.started),
    "<TABLE BORDER=1 WIDTH=100%>
	"++ make_rows(Names,[],0) ++"
     </TABLE>".
%----------------------------------------------------------------------
%get_started_apps(Data,Started)-> [{web_name,link}]
%selects the started apps from the ets table of apps.
%----------------------------------------------------------------------
    
get_started_apps(Data,Started)->
    SelectData=fun({Name,Link}) ->
		       {Name,Link}
	       end,
    MS = lists:map(fun(A) -> {{A,{web_data,'$1'}},[],['$1']} end,Started),

    [{"WebTool","/tool_management.html"} | 
     [SelectData(X) || X <- ets:select(Data,MS)]].

%----------------------------------------------------------------------
% make_rows(List,Result,Fields)-> String (The rows of a htmltable
% List a list of tupler discibed above
% Result an accumulator for the result
% Field, counter that counts the number of cols in each row.
%----------------------------------------------------------------------
make_rows([],Result,Fields)->
    Result ++ fill_out(Fields);
make_rows([Data|Paths],Result,Field)when Field==0->
   make_rows(Paths,Result ++ "<TR>" ++ make_field(Data),Field+1);

make_rows([Path|Paths],Result,Field)when Field==4->
   make_rows(Paths,Result ++ make_field(Path) ++ "</TR>",0);

make_rows([Path|Paths],Result,Field)->
   make_rows(Paths,Result ++ make_field(Path),Field+1).

%----------------------------------------------------------------------
% make_fields(Path)-> String that is a field i a html table
% Path is a name url tuple {Name,url}
%----------------------------------------------------------------------
make_field(Path)->
    "<TD WIDTH=20%>" ++ get_name(Path) ++ "</TD>".


%----------------------------------------------------------------------
%get_name({Nae,Url})->String that represents a <A> tag in html. 
%----------------------------------------------------------------------
get_name({Name,Url})->
    "<A HREF=\"" ++ Url ++ "\" TARGET=app_frame>" ++ Name ++ "</A>".


%----------------------------------------------------------------------
% fill_out(Nr)-> String, that represent Nr fields in a html-table.
%----------------------------------------------------------------------
fill_out(Nr)when Nr==0->
    [];
fill_out(Nr)when Nr==4->
    "<TD WIDTH=\"20%\" >&nbsp</TD></TR>";

fill_out(Nr)->
    "<TD WIDTH=\"20%\">&nbsp</TD>" ++ fill_out(Nr+1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%These functions starts applicatons and builds the page showing tools
%%to start
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%---------------------------------------------------------------------- 
%Controls whether the user selected a tool to start
%----------------------------------------------------------------------
get_tools(Input)->
    case uri_string:dissect_query(Input) of
	[]->
	    no_tools;
	 Tools->
	    FormatData=fun({_Name,Data}) -> list_to_atom(Data) end,
	    SelectData=
		fun({Name,_Data}) -> string:equal(Name,"app") end,
	    {tools,[FormatData(X)||X<-Tools,SelectData(X)]}
    end.

%----------------------------------------------------------------------
% Selects the data to start  the applications the user has ordered 
% starting of 
%----------------------------------------------------------------------
handle_apps([],State,_Cmd)->
    {ok,State};

handle_apps([Tool|Tools],State,Cmd)->
    case ets:match_object(State#state.app_data,{Tool,{start,'_'}}) of
	[]->
	    Started = case Cmd of
			  start ->
			      [Tool|State#state.started];
			  stop ->
			      lists:delete(Tool,State#state.started)
		      end,
	    {ok,#state{priv_dir=State#state.priv_dir,
		       app_data=State#state.app_data,
		       supvis=State#state.supvis,
		       web_data=State#state.web_data,
		       started=Started}};
	ToStart ->
	    case handle_apps2(ToStart,State,Cmd) of
		{ok,NewState}->
		    handle_apps(Tools,NewState,Cmd);
		_->
		    handle_apps(Tools,State,Cmd)
	    end
    end.

%----------------------------------------------------------------------
%execute every start or stop data about a tool.
%----------------------------------------------------------------------
handle_apps2([{Name,Start_data}],State,Cmd)->
    case handle_app({Name,Start_data},State#state.app_data,State#state.supvis,Cmd) of
	ok->
	    Started = case Cmd of
			  start ->
			      [Name|State#state.started];
			  stop ->
			      
			      lists:delete(Name,State#state.started)
		      end,
	    {ok,#state{priv_dir=State#state.priv_dir,
		       app_data=State#state.app_data,
		       supvis=State#state.supvis,
		       web_data=State#state.web_data,
		       started=Started}};
	_->
	    error
    end;

handle_apps2([{Name,Start_data}|Rest],State,Cmd)->
    case handle_app({Name,Start_data},State#state.app_data,State#state.supvis,Cmd)of
	ok->
	    handle_apps2(Rest,State,Cmd);
	_->
	    error
    end.


%----------------------------------------------------------------------
% Handle start and stop of applications
%---------------------------------------------------------------------- 

handle_app({Name,{start,{func,Start,Stop}}},Data,_Pid,Cmd)->
    Action = case Cmd of
		 start ->
		     Start;
		 _ ->
		     Stop
	     end,    
    case Action of
	{M,F,A} ->
	    case catch apply(M,F,A) of
		{'EXIT',_} = Exit->
		    %%! Here the tool disappears from the webtool interface!!
		    io:format("\n=======ERROR (webtool, line ~w) =======\n"
			      "Could not start application \'~p\'\n\n"
			      "~w:~tw(~ts) ->\n"
			      "~tp\n\n",
			      [?LINE,Name,M,F,format_args(A),Exit]),
		    ets:delete(Data,Name);
		_OK->
		    ok
	    end;
	_NoStart ->
	    ok
    end;
	    

handle_app({Name,{start,{child,ChildSpec}}},Data,Pid,Cmd)->
    case Cmd of
	start ->
	    case catch supervisor:start_child(Pid,ChildSpec) of
		{ok,_}->
		    ok;
		{ok,_,_}->
		    ok;
		{error,Reason}->
		    %%! Here the tool disappears from the webtool interface!!
		    io:format("\n=======ERROR (webtool, line ~w) =======\n"
			      "Could not start application \'~p\'\n\n"
			      "supervisor:start_child(~p,~tp) ->\n"
			      "~tp\n\n",
			      [?LINE,Name,Pid,ChildSpec,{error,Reason}]),
		    ets:delete(Data,Name);
		Error ->
		    %%! Here the tool disappears from the webtool interface!!
		    io:format("\n=======ERROR (webtool, line ~w) =======\n"
			      "Could not start application \'~p\'\n\n"
			      "supervisor:start_child(~p,~tp) ->\n"
			      "~tp\n\n",
			      [?LINE,Name,Pid,ChildSpec,Error]),
		    ets:delete(Data,Name)
	    end;
	stop ->
	    case catch supervisor:terminate_child(websup,element(1,ChildSpec)) of
		ok ->
		    supervisor:delete_child(websup,element(1,ChildSpec));
		_ ->
		    error
	    end
    end;



handle_app({Name,{start,{app,Real_name}}},Data,_Pid,Cmd)->
    case Cmd of
	start ->
	    case application:start(Real_name,temporary) of
		ok->
		    io:write(Name),
		    ok;
		{error,{already_started,_}}->
		    %% Remove it from the database so we dont start
		    %% anything already started
		    ets:match_delete(Data,{Name,{start,{app,Real_name}}}),
		    ok;
		{error,_Reason}=Error->
		    %%! Here the tool disappears from the webtool interface!!
		    io:format("\n=======ERROR (webtool, line ~w) =======\n"
			      "Could not start application \'~p\'\n\n"
			      "application:start(~p,~p) ->\n"
			      "~tp\n\n",
			      [?LINE,Name,Real_name,temporary,Error]),
		    ets:delete(Data,Name)
	    end;
	
	stop ->
	    application:stop(Real_name)
    end;

%----------------------------------------------------------------------
% If the data is incorrect delete the app
%----------------------------------------------------------------------
handle_app({Name,Incorrect},Data,_Pid,Cmd)->
    %%! Here the tool disappears from the webtool interface!!
    io:format("\n=======ERROR (webtool, line ~w) =======\n"
	      "Could not ~w application \'~p\'\n\n"
	      "Incorrect data: ~tp\n\n",
	      [?LINE,Cmd,Name,Incorrect]),
    ets:delete(Data,Name).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% this functions creates the page that shows the unstarted tools   %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reload_started_apps()->
    "<script>
        function reloadCompiledList()
        {
          parent.parent.top1.document.location.href=\"/webtool/webtool/started_tools\";
        }
     </script>".

show_unstarted_apps(State)->
  "<TABLE HEIGHT=100%  WIDTH=100% BORDER=0> 
    <TR HEIGHT=80%><TD ALIGN=\"center\" VALIGN=\"middle\"> 
      <FORM NAME=\"stop_apps\" ACTION=\"/webtool/webtool/start_tools\" >
       <TABLE BORDER=1 WIDTH=60%>
	 <TR BGCOLOR=\"#8899AA\">
	   <TD ALIGN=CENTER COLSPAN=2><FONT SIZE=4>Available Tools<FONT></TD>
	 </TR>
 	<TR>
	   <TD WIDTH=50%>
	       <TABLE BORDER=0>
	           "++ list_available_apps(State)++"
                   <TR><TD COLSPAN=2>&nbsp;</TD></TR>
                   <TR>
                      <TD COLSPAN=2 ALIGN=\"center\">
                         <INPUT TYPE=submit VALUE=\"Start\">
                      </TD>
                   </TR>
                </TABLE>
            </TD>
           <TD>   
             To Start a Tool:
             <UL>
             <LI>Select the
             checkbox for each tool to
             start.</LI>
             <LI>Click on the 
             button marked <EM>Start</EM>.</LI></UL>
            </TD>         
         </TR>
      </TABLE> 
    </FORM>
   </TD></TR>
   <TR><TD>&nbsp;</TD></TR>
   </TABLE>".



list_available_apps(State)->
    MS = ets:fun2ms(fun({Tool,{web_data,{Name,_}}}) -> {Tool,Name} end),
    Unstarted_apps=
	lists:filter(
	  fun({Tool,_})->
		  false==lists:member(Tool,State#state.started)
	  end,
	  ets:select(State#state.app_data,MS)),
    case Unstarted_apps of
	[]->
	    "<TR><TD>All tools are started</TD></TR>";
	_->
	    list_apps(Unstarted_apps)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% these functions creates the page that shows the started apps     %%
%% the user can select to shutdown                                  %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show_started_apps(State)->
  "<TABLE HEIGHT=100%  WIDTH=100% BORDER=0> 
    <TR HEIGHT=80%><TD ALIGN=\"center\" VALIGN=\"middle\"> 
      <FORM NAME=\"stop_apps\" ACTION=\"/webtool/webtool/stop_tools\" >
       <TABLE BORDER=1 WIDTH=60%>
	 <TR BGCOLOR=\"#8899AA\">
	   <TD ALIGN=CENTER COLSPAN=2><FONT SIZE=4>Started Tools<FONT></TD>
	 </TR>
 	<TR>
	   <TD WIDTH=50%>
	       <TABLE BORDER=0>
	           "++ list_started_apps(State)++"
                   <TR><TD COLSPAN=2>&nbsp;</TD></TR>
                   <TR>
                      <TD COLSPAN=2 ALIGN=\"center\">
                         <INPUT TYPE=submit VALUE=\"Stop\">
                      </TD>
                   </TR>
                </TABLE>
            </TD>
           <TD>   
             Stop a Tool:
             <UL>
             <LI>Select the
             checkbox for each tool to
             stop.</LI>
             <LI>Click on the 
             button marked <EM>Stop</EM>.</LI></UL>
            </TD>         
         </TR>
      </TABLE> 
    </FORM>
   </TD></TR>
   <TR><TD>&nbsp;</TD></TR>
   </TABLE>".

list_started_apps(State)->
    MS = lists:map(fun(A) -> {{A,{web_data,{'$1','_'}}},[],[{{A,'$1'}}]} end,
		   State#state.started),
    Started_apps= ets:select(State#state.app_data,MS),
    case Started_apps of
	[]->
	    "<TR><TD>No tool is started yet.</TD></TR>";
	_->
	    list_apps(Started_apps)
    end.


list_apps(Apps) ->
      lists:map(fun({Tool,Name})->
			"<TR><TD>
                            <INPUT TYPE=\"checkbox\" NAME=\"app\" VALUE=\""  
			    ++ atom_to_list(Tool) ++ "\">
                               " ++ Name ++ "     
                            </TD></TR>"
		end,
		Apps).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                   %%
%% Collecting the data from the  *.tool files                        %%
%%                                                                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----------------------------------------
% get_tools(Dirs) => [{M,F,A},{M,F,A}...{M,F,A}]
%   Dirs - [string()] Directory names
% Calls get_tools2/2 recursively for a number of directories
% to retireve the configuration data for the web based tools.
%----------------------------------------
get_tools1(Dirs)->
    get_tools1(Dirs,[]).

get_tools1([Dir|Rest],Data) when is_list(Dir) ->
    Tools=case filename:basename(Dir) of
	      %% Dir is an 'ebin' directory, check in '../priv' as well
	      "ebin" ->
		  [get_tools2(filename:join(filename:dirname(Dir),"priv")) |
		   get_tools2(Dir)];
	      _ ->
		  get_tools2(Dir)
	  end,
    get_tools1(Rest,[Tools|Data]);

get_tools1([],Data) ->
  lists:flatten(Data).

%----------------------------------------
% get_tools2(Directory) => DataList
%   DataList : [WebTuple]|[]
%   WebTuple: {tool,[{web,M,F,A}]}
%
%----------------------------------------
get_tools2(Dir)->
    get_tools2(tool_files(Dir),[]).

get_tools2([ToolFile|Rest],Data) ->
    case get_tools3(ToolFile) of
	{tool,WebData} ->
	    get_tools2(Rest,[{tool,WebData}|Data]);
	{error,_Reason} ->
	    get_tools2(Rest,Data);
	nodata ->
	    get_tools2(Rest,Data)
    end;

get_tools2([],Data) ->
    Data.

%----------------------------------------
% get_tools3(ToolFile) => {ok,Tool}|{error,Reason}|nodata 
%   Tool: {tool,[KeyValTuple]}
%   ToolFile - string() A .tool file
%   Now we have the file get the data and sort it out
%----------------------------------------
get_tools3(ToolFile) ->
    case file:consult(ToolFile) of
	{error,open} ->
	    {error,nofile};
	{error,read} ->
	    {error,format};
	{ok,[{version,"1.2"},ToolInfo]} when is_list(ToolInfo)->
	    webdata(ToolInfo);
	{ok,[{version,_Vsn},_Info]} ->
	    {error,old_version};
	{ok,_Other} ->
	    {error,format}
    end.


%----------------------------------------------------------------------
% webdata(TupleList)-> ToolTuple| nodata
% ToolTuple: {tool,[{config_func,{M,F,A}}]}
%
% There are a little unneccesary work in this format but it is extendable
%----------------------------------------------------------------------
webdata(TupleList)-> 
    case proplists:get_value(config_func,TupleList,nodata) of
	{M,F,A} ->
	    {tool,[{config_func,{M,F,A}}]};
	_ ->
	   nodata
    end.


%=============================================================================
% Functions for getting *.tool configuration files
%=============================================================================

%----------------------------------------
% tool_files(Dir) => ToolFiles
%   Dir - string() Directory name
%   ToolFiles - [string()]
% Return the list of all files in Dir ending with .tool (appended to Dir)
%----------------------------------------
tool_files(Dir) ->
    case file:list_dir(Dir) of
	{ok,Files} ->
	    filter_tool_files(Dir,Files);
	{error,_Reason} ->
	    []
    end.

%----------------------------------------
% filter_tool_files(Dir,Files) => ToolFiles
%   Dir - string() Directory name
%   Files, ToolFiles - [string()] File names
% Filters out the files in Files ending with .tool and append them to Dir
%----------------------------------------
filter_tool_files(_Dir,[]) ->
    [];
filter_tool_files(Dir,[File|Rest]) ->
    case filename:extension(File) of
	".tool" ->
	    [filename:join(Dir,File)|filter_tool_files(Dir,Rest)];
	_ ->
	    filter_tool_files(Dir,Rest)
    end.


%%%-----------------------------------------------------------------
%%% format functions
ffunc({M,F,A}) when is_list(A) ->
    io_lib:format("~w:~tw(~ts)\n",[M,F,format_args(A)]);
ffunc({M,F,A}) when is_integer(A) ->
    io_lib:format("~w:~tw/~w\n",[M,F,A]).

format_args([]) ->
    "";
format_args(Args) ->
    Str = lists:append(["~tp"|lists:duplicate(length(Args)-1,",~tp")]),
    io_lib:format(Str,Args).
