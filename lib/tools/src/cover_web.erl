%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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

-module(cover_web).
-author('marting@erix.ericsson.se').
-behaviour(gen_server).

%%Export of configuration function
-export([configData/0]).
%% External exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-export([start_link/0,start/0,stop/0]).
-export([menu_frame/2,nodes_frame/2,import_frame/2,
	 compile_frame/2,result_frame/2]).
-export([list_dir/2,compile/2,add_node/2,remove_node/2,result/2,
	 calls/2,coverage/2,import/2]).

-record(state,{dir}).

-include_lib("kernel/include/file.hrl").

%% Timeouts
-define(DEFAULT_TIME,10000).
-define(MAX_COMPILE_TIME,60000).
-define(MAX_ANALYSE_TIME,30000).

%% Colors
-define(INFO_BG_COLOR,"#C0C0EA").

%%%----------------------------------------------------------------------
%%% API - called from erlang shell
%%%----------------------------------------------------------------------
%% Start webtool and webcover from erlang shell
start() ->
    webtool:start(),
    webtool:start_tools([],"app=webcover"),
    ok.

%% Stop webtool and webcover from erlang shell
stop() ->
    webtool:stop_tools([],"app=webcover"),
    webtool:stop().



%%%----------------------------------------------------------------------
%%% API - called from webtool
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, webcover_server},cover_web, [], []).


nodes_frame(Env,Input)->
    call({nodes_frame,Env,Input}).

add_node(Env,Input)->
    call({add_node,Env,Input}).

remove_node(Env,Input)->
    call({remove_node,Env,Input}).

compile_frame(Env,Input)->
    call({compile_frame,Env,Input}).

list_dir(Env,Input) ->
    call({list_dir,Env,Input}).

compile(Env,Input)->
    call({compile,Env,Input},?MAX_COMPILE_TIME).

result_frame(Env,Input)->
    call({result_frame,Env,Input}).

result(Env,Input) ->
    call({result,Env,Input},?MAX_ANALYSE_TIME).

calls(Env,Input) ->
    call({calls,Env,Input}).

coverage(Env,Input) ->
    call({coverage,Env,Input}).

import_frame(Env,Input)->
    call({import_frame,Env,Input}).

import(Env,Input)->
    call({import,Env,Input}).

menu_frame(Env,Input)->
    call({menu_frame,Env,Input}).

call(Msg) ->
    call(Msg,?DEFAULT_TIME).
call(Msg,Time) ->
    gen_server:call(webcover_server,Msg,Time).
    
			    

configData()->
    {webcover,[{web_data,{"WebCover","/webcover"}},
               {alias,{"/webcover",code:priv_dir(tools)}},
	       {alias,{erl_alias,"/webcover/erl",[cover_web]}},
	       {start,{child,{{local,webcover_server},
			      {cover_web,start_link,[]},
			      permanent,100,worker,[cover_web]}}}
	      ]}.


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    cover:start(),
    CS = whereis(cover_server),
    link(CS),
    GL = spawn_link(fun group_leader_proc/0),
    group_leader(GL,CS),

    %% Must trap exists in order to have terminate/2 executed when
    %% crashing because of a linked process crash.
    process_flag(trap_exit,true),
    {ok,Cwd} = file:get_cwd(),
    {ok, #state{dir=Cwd}}.

group_leader_proc() ->
    register(cover_group_leader_proc,self()),
    group_leader_loop([]).
group_leader_loop(Warnings) ->
    receive
	{io_request,From,ReplyAs,{put_chars,io_lib,Func,[Format,Args]}} ->
	    Msg = (catch io_lib:Func(Format,Args)),
	    From ! {io_reply,ReplyAs,ok},
	    case lists:member(Msg,Warnings) of
		true -> group_leader_loop(Warnings);
		false -> group_leader_loop([Msg|Warnings])
	    end;
	{io_request,From,ReplyAs,{put_chars,_Encoding,io_lib,Func,[Format,Args]}} ->
	    Msg = (catch io_lib:Func(Format,Args)),
	    From ! {io_reply,ReplyAs,ok},
	    case lists:member(Msg,Warnings) of
		true -> group_leader_loop(Warnings);
		false -> group_leader_loop([Msg|Warnings])
	    end;
	IoReq when element(1,IoReq)=:= io_request ->
	    group_leader() ! IoReq,
	    group_leader_loop(Warnings);
	{From,get_warnings} ->
	    Warnings1 = 
		receive 
		    {io_request,From,ReplyAs,
		     {put_chars,io_lib,Func,[Format,Args]}} ->
			Msg = (catch io_lib:Func(Format,Args)),
			From ! {io_reply,ReplyAs,ok},
			case lists:member(Msg,Warnings) of
			    true -> Warnings;
			    false -> [Msg|Warnings]
			end
		after 0 ->
			Warnings
		end,
	    From ! {warnings,Warnings1},
	    group_leader_loop([])
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({nodes_frame,_Env,_Input},_From,State)->
    {reply,nodes_frame1(),State};

handle_call({add_node,_Env,Input},_From,State)->
    {reply,do_add_node(Input),State};

handle_call({remove_node,_Env,Input},_From,State)->
    {reply,do_remove_node(Input),State};

handle_call({compile_frame,_Env,_Input},_From,State)->
    {reply,compile_frame1(State#state.dir),State};

handle_call({list_dir,_Env,Input},_From,State)->
    Dir = get_input_data(Input,"path"),
    case filelib:is_dir(Dir) of
	true ->
	    {reply,compile_frame1(Dir),State#state{dir=Dir}};
	false ->
	    Err = Dir ++ " is not a directory",
	    {reply,compile_frame1(State#state.dir,Err),State}
    end;
handle_call({compile,_Env,Input},_From,State)->
    {reply,do_compile(Input,State#state.dir),State};

handle_call({result_frame,_Env,_Input},_From,State)->
    {reply,result_frame1(),State};

handle_call({result,_Env,Input},_From,State)->
    {reply,handle_result(Input),State};

handle_call({calls,_Env,Input},_From,State)->
    {reply,call_page(Input),State};

handle_call({coverage,_Env,Input},_From,State)->
    {reply,coverage_page(Input),State};

handle_call({import_frame,_Env,_Input},_From,State)->
    {ok,Cwd} = file:get_cwd(),
    {reply,import_frame1(Cwd),State};

handle_call({import,_Env,Input},_From,State)->
    {reply,do_import(Input),State};

handle_call({menu_frame,_Env,_Input},_From,State)->
    {reply,menu_frame1(),State};

handle_call(_Request, _From, State) ->
    Reply = bad_request,
    {reply, Reply, State}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({'EXIT',_Pid,Reason}, State) ->
    {stop, Reason, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    cover:stop(),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% The functions that creates the whole pages by collecting all the   %%
%% neccessary data for each page. These functions are the public      %%
%% interface.                                                         %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%----------------------------------------------------------------------
%% Returns the page to the left frame
%%----------------------------------------------------------------------
menu_frame1()->
    [header(),html_header(""),menu_body(),html_end()].

%%----------------------------------------------------------------------
%% Creates the page where the user can add and remove nodes
%%----------------------------------------------------------------------

nodes_frame1()->
    nodes_frame1([]).
nodes_frame1(Err)->
    [header(),html_header("Add/remove nodes"),nodes_body(Err),html_end()].

%%----------------------------------------------------------------------
%% Creates the page where the user can cover compile modules
%%----------------------------------------------------------------------

compile_frame1(Dir)->
    compile_frame1(Dir,[]).
compile_frame1(Dir,Err) ->
    [header(),html_header("Cover compile"),compile_body(Dir,Err),html_end()].

%%----------------------------------------------------------------------
%% Creates the page where the user can handle results
%%----------------------------------------------------------------------

result_frame1()->
    result_frame1([]).
result_frame1(Err) ->
    [header(),html_header("Show cover results"),result_body(Err),html_end()].

%%----------------------------------------------------------------------
%%The beginning of the page that clear the cover information on a cover
%%compiled module
%%----------------------------------------------------------------------
call_page(Input)->
    [header(),html_header("Code coverage"),call_result(Input),html_end()].

coverage_page(Input)->
    [header(),html_header("Code coverage"),coverage_result(Input),html_end()].

%%----------------------------------------------------------------------
%% Creates the page where the user an import files
%%----------------------------------------------------------------------
import_frame1(Dir) ->
    import_frame1(Dir,"").
import_frame1(Dir,Err) ->
    [header(),html_header("Import coverdata"),import_body(Dir,Err),html_end()].
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% The functions that build the body of the menu frame                %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
menu_body() ->
    Nodes = cover:which_nodes(),
    Modules = cover:modules(),
    Imported = cover:imported(),
    ["<A HREF=\"./nodes_frame\" TARGET=\"main\">Nodes</A><BR>\n",
     "<A HREF=\"./compile_frame\" TARGET=\"main\">Compile</A><BR>\n",
     "<A HREF=\"./import_frame\" TARGET=\"main\">Import</A><BR>\n",
     "<A HREF=\"./result_frame\" TARGET=\"main\">Result</A>\n",
     "<P><B>Nodes:</B>\n",
     "<UL>\n",
     lists:map(fun(N) -> "<LI>"++atom_to_list(N)++"</LI>\n" end,[node()|Nodes]),
     "</UL>\n",
     "<P><B>Compiled modules:</B>\n",
     "<UL>\n",
     lists:map(fun(M) -> "<LI>"++atom_to_list(M)++"</LI>\n" end,Modules),
     "</UL>\n",
     "<P><B>Imported files:</B>\n",
     "<UL>\n",
     "<FONT SIZE=-1>\n",
     lists:map(fun(F) -> 
		       Short = filename:basename(F),
		       "<LI TITLE=\""++F++"\">"++Short++"</LI>\n" end,Imported),
     "</FONT>\n",
     "</UL>\n"].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% The functions that build the body of the nodes frame               %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nodes_body(Err) ->
    CN = cover:which_nodes(),
    Fun = fun(N) -> 
		  NStr = atom_to_list(N),
		  ["<OPTION VALUE=",NStr,
		   " onClick=\"node.value=selected_node.value\">",NStr,
		   "</OPTION>\n"] 
	  end,
    AllNodes = lists:append(lists:map(Fun,nodes()--CN)),
    CoverNodes = lists:append(lists:map(Fun,CN)),

    [reload_menu_script(Err),
     "<H1 ALIGN=center>Nodes</H1>\n",
     "<TABLE BORDER=0 WIDTH=600 ALIGN=center>\n",     
     "<TR><TD BGCOLOR=",?INFO_BG_COLOR," COLSPAN=2>\n",
     "<P>You can run cover over several nodes simultaneously. Coverage data\n",
     "from all involved nodes will be merged during analysis.\n",
     "<P>Select or enter node names to add or remove here.\n",
     "</TD></TR>\n",
     "<TR><TD COLSPAN=2><BR><BR></TD></TR>\n",
     "<FORM ACTION=\"./add_node\" NAME=add_node>\n",
     "<TR><TD VALIGN=top>Add node:</TD>\n",
     "<TD><INPUT TYPE=text NAME=\"node\" SIZE=40 >",
     "<INPUT TYPE=submit\n",
     " onClick=\"if(!node.value){node.value=selected_node.value};\" VALUE=Add>"
     "<BR><SELECT NAME=selected_node TITLE=\"Select node\">\n",
     AllNodes ++
     "</SELECT>\n",
     "</TD></TR>\n"
     "</FORM>\n",
     "<TR><TD COLSPAN=2><BR><BR></TD></TR>\n",
     "<FORM ACTION=\"./remove_node\" NAME=remove_node>\n",
     "<TR><TD>Remove node:</TD>\n", 
     "<TD><SELECT NAME=node TITLE=\"Select node\">\n",
     CoverNodes ++
     "</SELECT>\n",
     "<INPUT TYPE=submit VALUE=Remove>"
     "</TD></TR>\n",
     "</FORM>",
     "</TABLE>"].
          

do_add_node(Input) ->
    NodeStr = get_input_data(Input, "node"),
    Node = list_to_atom(NodeStr),
    case net_adm:ping(Node) of
	pong ->
	    cover:start(Node),
	    nodes_frame1();
	pang ->
	    nodes_frame1("Node \\\'" ++ NodeStr ++ "\\\' is not alive")
    end.

do_remove_node(Input) ->
    Node = list_to_atom(get_input_data(Input, "node")),
    cover:stop(Node),
    nodes_frame1().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                     %
% The functions that is used when the user wants to compile something %
%                                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compile_body(Dir,Err) ->
    Erls = filelib:wildcard(filename:join(Dir,"*.erl")),
    Beams = filelib:wildcard(filename:join(Dir,"*.beam")),
    
    [reload_menu_script(Err),
     "<H1 ALIGN=center>Compile</H1>\n",
     "<TABLE WIDTH=600 ALIGN=center BORDER=0>\n",
     "<TR><TD COLSPAN=3 BGCOLOR=",?INFO_BG_COLOR,">\n",
     "Each module which shall be part of the cover analysis must be prepared\n",
     "or 'cover compiled'. On this page you can select .erl files and/or\n",
     ".beam files to include in the analysis. If you select a .erl file it\n",
     "will first be compiled with the Erlang compiler and then prepared for\n",
     "coverage analysis. If you select a .beam file it will be prepared for\n",
     "coverage analysis directly.\n",
     "</TD></TR>\n",
     "<FORM ACTION=\"./list_dir\" NAME=list_dir>\n",
     "<TR><TD WIDTH=30% BGCOLOR=",?INFO_BG_COLOR," ROWSPAN=2>\n",
     "To list a different directory, enter the directory name here.\n",
     "</TD>\n",
     "<TH COLSPAN=2><BR>List directory:<BR></TH>\n",
     "</TR>\n",
     "<TR><TD ALIGN=center COLSPAN=2>\n",
     "<INPUT TYPE=text NAME=\"path\" SIZE=40 VALUE=",Dir,">",
     "<INPUT TYPE=submit VALUE=Ok>",
     "<BR><BR></TD></TR>\n",
     "</FORM>\n",
     "<FORM ACTION=\"./compile\" NAME=compile_selection>\n",
     "<TR><TD BGCOLOR=",?INFO_BG_COLOR," ROWSPAN=2>\n",
     "<P>Select one or more .erl or .beam files to prepare for coverage\n"
     "analysis, and click the \"Compile\" button.\n",
     "<P>To reload the original file after coverage analysis is complete,\n"
     "select one or more files and click the \"Uncompile\" button, or\n",
     "simply click the \"Uncompile all\" button to reload all originals.\n"
     "</TD>\n",
     "<TH>.erl files</TH><TH>.beam files</TH></TR>\n",
     "<TR><TD ALIGN=center VALIGN=top>\n",
     "<SELECT NAME=erl TITLE=\"Select .erl files to compile\" MULTIPLE=true",
     " SIZE=15>\n",
     list_modules(Erls) ++
     "</SELECT></TD>\n",
     "<TD ALIGN=center VALIGN=top>\n",
     "<SELECT NAME=beam TITLE=\"Select .beam files to compile\"MULTIPLE=true",
     " SIZE=15>\n",
     list_modules(Beams) ++
     "</SELECT></TD></TR>\n"
     "<TR><TD BGCOLOR=",?INFO_BG_COLOR," ROWSPAN=2>\n",
     "Compile options are only needed for .erl files. The options must be\n"
     "given e.g. like this: \n"
     "<FONT SIZE=-1>[{i,\"/my/path/include\"},{i,\"/other/path/\"}]</FONT>\n"
     "</TD>\n",
     "<TH COLSPAN=2><BR>Compile options:<BR></TH>\n",
     "</TR>\n",
     "<TR><TD COLSPAN=2 ALIGN=center>\n",
     "<INPUT TYPE=text NAME=\"options\" SIZE=40>\n",
     "<INPUT TYPE=hidden NAME=\"action\"></TD></TR>\n",
     "<TR><TD></TD><TD ALIGN=center COLSPAN=2>\n",
     "<INPUT TYPE=submit onClick=\"action.value=\'compile\';\"VALUE=Compile>",
     "<INPUT TYPE=submit onClick=\"action.value=\'uncompile\';\" ",
     "VALUE=Uncompile>",
     "<INPUT TYPE=submit onClick=\"action.value=\'uncompile_all\';\" ",
     "VALUE=\"Uncompile all\">",
     "<BR><INPUT TYPE=reset VALUE=\"Reset form\"></TD></TR>\n",
     "</FORM>\n",
     "</TABLE>\n"].

list_modules([File|Files]) ->
    Mod = filename:basename(File),
    ["<OPTION VALUE=",File," onDblClick=\"action.value=\'compile\';submit();\">",
     Mod,"</OPTION>\n" | list_modules(Files)];
list_modules([]) ->
    [].

do_compile(Input,Dir) ->
    {Erls,Beams,Opts,Action} = get_compile_input(parse(Input),[],[]),
    Errs = 
	case Action of
	    "compile" ->
		do_compile(Erls,Beams,Opts,[]);
	    "uncompile" ->
		do_uncompile(Erls++Beams);
	    "uncompile_all" ->
		do_uncompile(cover:modules())
	end,
    compile_frame1(Dir,Errs).

get_compile_input([{"erl",File}|Input],Erl,Beam) ->
    get_compile_input(Input,[File|Erl],Beam);
get_compile_input([{"beam",File}|Input],Erl,Beam) ->
    get_compile_input(Input,Erl,[File|Beam]);
get_compile_input([{"options",Opts0},{"action",Action}],Erl,Beam) ->
    Opts = parse_options(Opts0),
    {Erl,Beam,Opts,Action}.

do_compile([Erl|Erls],Beams,Opts,Errs) ->
    case cover:compile_module(Erl,Opts) of
	{ok,_} ->
	    do_compile(Erls,Beams,Opts,Errs);
	{error,File} ->
	    do_compile(Erls,Beams,Opts,["\\n"++File|Errs])
    end;
do_compile([],[Beam|Beams],Opts,Errs) ->
    case cover:compile_beam(Beam) of
	{ok,_} ->
	    do_compile([],Beams,Opts,Errs);
	{error,{no_abstract_code,File}} ->
	    do_compile([],Beams,Opts,["\\n"++File++" (no_abstract_code)"|Errs])
    end;
do_compile([],[],_,[]) ->
    [];
do_compile([],[],_,Errs) ->
    "Compilation failed for the following files:" ++ Errs.

parse_options(Options)->
    case erl_scan:string(Options ++".") of
	{ok,Tokens,_Line} ->
	    case erl_parse:parse_exprs(Tokens) of
		{ok,X}->
		    case lists:map(fun erl_parse:normalise/1, X) of
			[List] when is_list(List) -> List;
			List -> List
		    end;
		_ ->
		    []
	    end;
	_ ->
	    []
    end.
	    

do_uncompile(Files) ->
    lists:foreach(
      fun(File) ->
	      Module = 
		  if is_atom(File) -> 
			  File;
		     true -> 
			  ModStr = filename:basename(filename:rootname(File)),
			  list_to_atom(ModStr)
		  end,
	      case code:which(Module) of
		  cover_compiled ->
		      code:purge(Module),
		      case code:load_file(Module) of
			  {module, Module} ->
			      ok;
			  {error, _Reason2} ->
			      code:delete(Module)
		      end;
		  _ ->
		      ok
	      end
      end,
      Files),
    [].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                     %
% The functions that builds the body of the page for coverage analysis%
% 		                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
result_body(Err) ->
    [reload_menu_script(Err),
     "<H1 ALIGN=center>Result</H1>\n",
     "<TABLE BORDER=0 WIDTH=600 ALIGN=center>\n",
     "<TR><TD BGCOLOR=",?INFO_BG_COLOR,">\n",
     "<P>After executing all your tests you can view the result of the\n",
     "coverage analysis here. For each module you can\n",
     "<DL>\n",
     "<DT><B>Analyse to file</B></DT>\n",
     "<DD>The source code of the module is shown with the number of calls\n",
     "to each line stated in the left margin. Lines which are never called\n",
     "are colored red.</DD>\n",
     "<DT><B>Analyse coverage</B></DT>\n",
     "<DD>Show the number of covered and uncovered lines in the module.</DD>\n",
     "<DT><B>Analyse calls</B></DT>\n",
     "<DD>Show the number of calls in the module.</DD>\n",
     "<DT><B>Reset module</B></DT>\n",
     "<DD>Delete all coverage data for the module.</DD>\n",
     "<DT><B>Export module</B></DT>\n",
     "<DD>Write all coverage data for the module to a file. The data can\n",
     "later be imported from the \"Import\" page.</DD>\n",
     "</DL>\n",
     "<P>You can also reset or export data for all modules with the\n",
     "<B>Reset all</B> and <B>Export all</B> actions respectively. For these\n",
     "two actions there is no need to select a module.\n",
     "<P>Select module and action from the drop down menus below, and click\n",
     "the \"Execute\" button.\n",
     "</TD></TR>\n",
     "<TR><TD><BR><BR>\n",
     result_selections(),
     "</TD></TR></TABLE>"].

result_selections() ->
    ModList = filter_modlist(cover:modules()++cover:imported_modules(),[]),

    ["<FORM ACTION=\"./result\" NAME=result_selection>\n",
     "<TABLE WIDTH=\"300\" BORDER=0 ALIGN=center>\n",
     "<TR><TD ALIGN=left>\n",
     "Module:\n",
     "<BR><SELECT NAME=module TITLE=\"Select module\">\n",
     ModList ++
     "</SELECT>\n",
     "</TD>\n",
     "<TD ALIGN=left>\n",
     "Action:\n",
     "<BR><SELECT NAME=action TITLE=\"Select action\">\n",
     "<OPTION VALUE=\"analyse_to_file\">Analyse to file</OPTION>\n"
     "<OPTION VALUE=\"coverage\">Analyse coverage</OPTION>\n"
     "<OPTION VALUE=\"calls\">Analyse calls</OPTION>\n"
     "<OPTION VALUE=\"reset\">Reset module</OPTION>\n"
     "<OPTION VALUE=\"reset_all\">Reset all</OPTION>\n"
     "<OPTION VALUE=\"export\">Export module</OPTION>\n"
     "<OPTION VALUE=\"export_all\">Export all</OPTION>\n"
     "</SELECT>\n",
     "</TD>\n",
     "<TD ALIGN=center VALIGN=bottom><INPUT TYPE=submit VALUE=Execute>\n"
     "</TD></TR>\n"
     "</TABLE>\n",
     "</FORM>\n"].

filter_modlist([M|Ms],Already) ->
    case lists:member(M,Already) of
	true ->
	    filter_modlist(Ms,Already);
	false ->
	    MStr = atom_to_list(M),
	    ["<OPTION VALUE=",MStr,">",MStr,"</OPTION>\n" |
		filter_modlist(Ms,[M|Already])]
    end;
filter_modlist([],_Already) ->
    [].
    


handle_result(Input) ->
    case parse(Input) of
	[{"module",M},{"action",A}] ->
	    case A of
		"analyse_to_file" ->
		    case cover:analyse_to_file(list_to_atom(M),[html]) of
			{ok,File} ->
			    case file:read_file(File) of
				{ok,HTML}->
				    file:delete(File),
				    [header(),
				     reload_menu_script(""),
				     binary_to_list(HTML)];
				_ ->
				    result_frame1("Can not read file" ++ File)
			    end;
			{error,no_source_code_found} ->
			    result_frame1("No source code found for \\\'" ++
					  M ++ "\\\'")
		    end;
		"calls" ->
		    call_page(Input);
		"coverage" ->
		    coverage_page(Input);
		"reset" ->
		    cover:reset(list_to_atom(M)),
		    result_frame1("Coverage data for \\\'" ++ M ++ 
				  "\\\' is now reset");
		"reset_all" ->
		    cover:reset(),
		    result_frame1("All coverage data is now reset");
		"export" ->
		    ExportFile = generate_filename(M),
		    cover:export(ExportFile,list_to_atom(M)),
		    result_frame1("Coverage data for \\\'" ++ M ++ 
				  "\\\' is now exported to file \\\"" ++
				  ExportFile ++ "\\\"");
		"export_all" ->
		    ExportFile = generate_filename("COVER"),
		    cover:export(ExportFile),
		    result_frame1(
		      "All coverage data is now exported to file \\\"" ++
		      ExportFile ++ "\\\"")
	    end;
	[{"action",_A}] ->
	    result_frame1("No module is selected")
    end.

generate_filename(Prefix) ->
    {ok,Cwd} = file:get_cwd(),
    filename:join(Cwd,Prefix ++ "_" ++ ts() ++ ".coverdata").
    
ts() ->
    {{Y,M,D},{H,Min,S}} = calendar:now_to_local_time(now()),
    io_lib:format("~4.4.0w~2.2.0w~2.2.0w-~2.2.0w~2.2.0w~2.2.0w",
		  [Y,M,D,H,Min,S]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                     %
% The functions that builds the body of the page that shows the calls %
% 		                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
call_result(Input)->
    Mod = list_to_atom(get_input_data(Input, "module")),
    case cover:analyse(Mod,calls) of
	{error,_}->
	    error_body();
	{ok,_} ->
	    call_result2(Mod,Input)
    end.

call_result2(Mod,Input)->
    Result = 
	case get_input_data(Input,"what") of
	    "mod" ->
		call_result(mod,Mod);
	    "func" ->
		call_result(func,Mod);
	    "clause" ->
		call_result(clause,Mod);
	    _->
		call_result(all,Mod)
	end,
    result_choice("calls",Mod) ++ Result.

result_choice(Level,Mod)->
    ModStr=atom_to_list(Mod),
    [reload_menu_script(""),
     "<TABLE WIDTH=100%><TR>\n",
     "<TD><A HREF=./",Level,"?module=",ModStr,"&what=all>All Data</A></TD>\n",
     "<TD><A HREF=./",Level,"?module=",ModStr,"&what=mod>Module</A></TD>\n",
     "<TD><A HREF=./",Level,"?module=",ModStr,"&what=func>Function</A></TD>\n",
     "<TD><A HREF=./",Level,"?module=",ModStr,"&what=clause>Clause</A></TD>\n",
     "</TR></TABLE><BR>\n"].

call_result(Mode,Module)->
    Content = 
	case Mode of
	    mod->
		format_cover_call(cover:analyse(Module,calls,module),mod);
	    func->
		format_cover_call(cover:analyse(Module,calls,function),func);
	    clause->
		format_cover_call(cover:analyse(Module,calls,clause),clause);
	    _->
		format_cover_call(cover:analyse(Module,calls,module),mod) ++
		   format_cover_call(cover:analyse(Module,calls,function),func)++
		   format_cover_call(cover:analyse(Module,calls,clause),clause)
	end,
    getModDate(Module,date())++"<BR>"++
    "<TABLE WIDTH=\"100%\" BORDER=1>"
                ++ Content ++"</TABLE>".


format_cover_call({error,_},_)->
    ["<TR><TD>\n",
     "<BR><BR><BR><BR>\n",
     "<FONT SIZE=5>The selected module is not Cover Compiled</FONT>\n",
     "<BR>\n",
     "</TD></TR>\n"];

format_cover_call({ok,{Mod,Calls}},mod)->
    ["<TR BGCOLOR=\"#8899AA\"><TD COLSPAN=5><B>Module calls</B></TD></TR>\n", 
     "<TR><TD COLSPAN=4><I>Module</I></TD>",
     "<TD ALIGN=\"right\"><I>Number of calls</I></TD></TR>\n",
     "<TR><TD COLSPAN=4>" ++ atom_to_list(Mod) ++"</TD>"
     "<TD ALIGN=\"right\">" ++ integer_to_list(Calls)++"</TD></TR>\n"];

format_cover_call({ok,Calls},func)->
    ["<TR BGCOLOR=\"#8899AA\"><TD COLSPAN=5><B>Function calls</B></TD></TR>\n",
     "<TR><TD><I>Module</I></TD><TD><I>Function</I></TD>",
     "<TD COLSPAN=2 ALIGN=\"right\"><I>Arity</I></TD>",
     "<TD ALIGN=\"right\"><I>Number of calls </I></TD></TR>\n",
     lists:append(
       lists:map(
	 fun({{Mod,Func,Arity},Nr_of_calls})->
		 ["<TR><TD WIDTH=\"20%\">"++ atom_to_list(Mod)++"</TD>\n",
		  "<TD WIDTH=\"20%\" >" ++ atom_to_list(Func) ++" </TD>\n",
		  "<TD COLSPAN=2 WIDTH=\"40%\" ALIGN=\"right\">",
		  integer_to_list(Arity), 
		  "</TD>\n",
		  "<TD WIDTH=\"20%\" ALIGN=\"right\">",
		  integer_to_list(Nr_of_calls), 
		  "</TD></TR>\n"]
	 end,
	 Calls))];

format_cover_call({ok,Calls},clause)->
    ["<TR BGCOLOR=\"#8899AA\"><TD COLSPAN=5><B>Clause calls</B></TD></TR>\n",
     "<TR><TD><I>Module</I></TD><TD><I>Function</I></TD>",
     "<TD ALIGN=\"right\"><I>Arity</I></TD>",
     "<TD ALIGN=\"right\"><I>Ordinal</I></TD>",
     "<TD ALIGN=\"right\"><I>Number of calls</I></TD></TR>\n",
     lists:append(
       lists:map(
	 fun({{Mod,Func,Arity,Ord},Nr_of_calls})->
		 ["<TR><TD WIDTH=\"20%\" >", atom_to_list(Mod), "</TD>\n",  
		  "<TD WIDTH=\"20%\" >", atom_to_list(Func), "</TD>\n",
		  "<TD WIDTH=\"20%\" ALIGN=\"right\">",
		  integer_to_list(Arity),
		  "</TD>\n",
		  "<TD WIDTH=\"20%\" ALIGN=\"right\">",
		  integer_to_list(Ord),
		  "</TD>\n",
		  "<TD WIDTH=\"20%\" ALIGN=\"right\">",
		  integer_to_list(Nr_of_calls),
		  "</TD></TR>\n"]
	 end,
	 Calls))].


error_body()->
    ["<TABLE WIDTH=\"100%\" BORDER=1>\n",
     "<TR ALIGN=\"center\">\n",
     "<TD>\n",
     "<BR><BR><BR><BR><BR><BR>\n",
     "<FONT SIZE=5>The selected module is not Cover Compiled</FONT>\n",
     "<BR>\n",
     "</TD>\n",
     "</TR>\n",
     "</TABLE>\n"].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                     %
% The functions that builds the body of the page that shows coverage  %
% 		                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
coverage_result(Input)->
    Mod = list_to_atom(get_input_data(Input, "module")),
    case cover:analyse(Mod,coverage) of
	{error,_}->
	    error_body();
	{ok,_} ->
	    coverage_result2(Mod,Input)
    end.

coverage_result2(Mod,Input)->
    Result = 
	case get_input_data(Input,"what") of
	    "mod" ->
		coverage_result(mod,Mod);
	    "func" ->
		coverage_result(func,Mod);
	    "clause" ->
		coverage_result(clause,Mod);
	    _->
		coverage_result(all,Mod)
	end,
    result_choice("coverage",Mod) ++ Result.

coverage_result(Mode,Module)->
    Content = 
	case Mode of
	    mod->
		format_cover_coverage(cover:analyse(Module,coverage,module),
				      mod);
	    func->
		format_cover_coverage(cover:analyse(Module,coverage,function),
				      func);
	    clause->
		format_cover_coverage(cover:analyse(Module,coverage,clause),
				      clause);
	    _->
		format_cover_coverage(cover:analyse(Module,coverage,module),
				      mod) ++
		   format_cover_coverage(cover:analyse(Module,coverage,function),
					 func)++
		   format_cover_coverage(cover:analyse(Module,coverage,clause),
					 clause)
	      end,
    getModDate(Module,date())++"<BR>"++
    "<TABLE WIDTH=\"100%\" BORDER=1>"
                ++ Content ++"</TABLE>".

getModDate(Module,{Year,Mon,Day})->
    "<TABLE>
      <TR>
        <TD>Module:</TD> 
        <TD>" ++ atom_to_list(Module) ++ "</TD> 
      </TR>
     <TR>
        <TD>Date:</TD> 
        <TD>" ++ integer_to_list(Day) ++ "/" ++ 
                 integer_to_list(Mon) ++"&nbsp;-&nbsp;"++ 
                 integer_to_list(Year)  ++ 
       "</TD> 
     </TR>
   </TABLE>".


format_cover_coverage({error,_},_)->
     "<TR><TD>
      <BR><BR><BR><BR>
      <FONT SIZE=5>The selected module is not Cover Compiled</FONT>
      <BR>  
      </TD></TR>";


format_cover_coverage({ok,{Mod,{Cov,Not_cov}}},mod)->
    ["<TR BGCOLOR=\"#8899AA\"><TD COLSPAN=6><B>Module coverage</B></TD></TR>\n",
     "<TR><TD COLSPAN=4><I>Module</I></TD>\n",
     "<TD ALIGN=\"right\"><I>Covered</I></TD>\n"
     "<TD ALIGN=\"RIGHT\" NOWRAP=\"true\"><I>Not Covered</I></TD>\n",
     "</TR>\n",
     "<TR><TD COLSPAN=4>", atom_to_list(Mod), "</TD>\n"
     "<TD ALIGN=\"right\">", integer_to_list(Cov), "</TD>\n"
     "<TD ALIGN=\"right\" >", integer_to_list(Not_cov), "</TD></TR>\n"];

format_cover_coverage({ok,Cov_res},func)->
    ["<TR BGCOLOR=\"#8899AA\"><TD COLSPAN=6><B>Function coverage</B></TD>\n",
     "</TR>\n",
     "<TR><TD><I>Module</I></TD><TD><I>Function</I></TD>",
     "<TD ALIGN=\"right\"><I>Arity</I></TD>",
     "<TD COLSPAN=2 ALIGN=\"right\"><I>Covered</I></TD>",
     "<TD ALIGN=\"right\" STYLE=\"white-space:nowrap\"><I>Not Covered</I></TD>",
     "</TR>\n",
     lists:append(
       lists:map(
	 fun({{Mod,Func,Arity},{Cov,Not_cov}})->
		 ["<TR><TD WIDTH=\"20%\" >"++ atom_to_list(Mod) ++" </TD>\n",
		  "<TD WIDTH=\"20%\" >" ++ atom_to_list(Func) ++"</TD>\n",
		  "<TD WIDTH=\"40%\" ALIGN=\"right\">",
		  integer_to_list(Arity),
		  "</TD>\n",  
		  "<TD WIDTH=\"40%\" ALIGN=\"right\" COLSPAN=2>",
		  integer_to_list(Cov),
		  "</TD>\n"
		  "<TD WIDTH=\"20%\" ALIGN=\"right\">",
		  integer_to_list(Not_cov),
		  "</TD></TR>\n"]
	 end,
	 Cov_res))];

format_cover_coverage({ok,Cov_res},clause)->
    ["<TR BGCOLOR=\"#8899AA\"><TD COLSPAN=6><B>Clause coverage</B></TD></TR>\n",
     "<TR><TD><I>Module</I></TD><TD><I>Function</I></TD>\n",
     "<TD ALIGN=\"right\"><I>Arity</I></TD>\n",
     "<TD ALIGN=\"right\"><I>Ordinal<I></TD>\n",
     "<TD ALIGN=\"right\">Covered</TD>\n",
     "<TD ALIGN=\"right\" STYLE=\"white-space:nowrap\">Not Covered</TD></TR>\n",
     lists:append(
       lists:map(
	 fun({{Mod,Func,Arity,Ord},{Cov,Not_cov}})->
		 ["<TR><TD WIDTH=\"20%\" >"++ atom_to_list(Mod) ++"</TD>\n",  
		  "<TD WIDTH=\"20%\" >" ++ atom_to_list(Func) ++" </TD>\n",
		  "<TD WIDTH=\"20%\" ALIGN=\"right\">",
		  integer_to_list(Arity),
		  "</TD>\n"
		  "<TD WIDTH=\"20%\" ALIGN=\"right\">",
		  integer_to_list(Ord),
		  "</TD>\n"
		  "<TD WIDTH=\"20%\" ALIGN=\"right\">",
		  integer_to_list(Cov),
		  "</TD>\n"
		  "<TD WIDTH=\"20%\" ALIGN=\"right\">",
		  integer_to_list(Not_cov),
		  "</TD></TR>\n"]
	 end,
	 Cov_res))].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                     %
% The functions that builds the body of the import page               %
% 		                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
import_body(Dir,Err) ->
    [reload_menu_script(Err),
     "<H1 ALIGN=center>Import</H1>\n",
     "<TABLE BORDER=0 WIDTH=600 ALIGN=center>\n",     
     "<TR><TD BGCOLOR=",?INFO_BG_COLOR,">\n",
     "<P>You can import coverage data from a previous analysis. If you do so\n",
     "the imported data will be merged with the current coverage data.\n",
     "<P>You can export data from the current analysis from the \"Result\"\n",
     "page.\n",
     "<P>Select the file to import here.\n",
     "</TD></TR>\n",
     "<TR><TD ALIGN=center><BR><BR>\n",
     "<FORM NAME=change_import_dir METHOD=post ACTION=\"./import\">\n",
     "<B>Change directory:</B><BR>\n",
     "<INPUT TYPE=text NAME=\"file\" SIZE=30 VALUE=",Dir,">",
     "<INPUT TYPE=hidden NAME=dir VALUE=",Dir,">\n",
     "<INPUT TYPE=submit VALUE=Ok><BR>\n",
     "</FORM>\n",
     browse_import(Dir),
     "</TABLE>"].

browse_import(Dir) ->
    {ok,List} = file:list_dir(Dir),
    Sorted = lists:reverse(lists:sort(List)),
    {Dirs,Files} = filter_files(Dir,Sorted,[],[]),
    ["<FORM NAME=browse_import METHOD=post ACTION=\"./import\">\n"
     "<SELECT NAME=file TITLE=\"Select import file\" SIZE=10>\n",
     "<OPTION VALUE=\"..\" onDblClick=submit()>../</OPTION>\n",
     Dirs,
     Files,
     "</SELECT>\n",
     "<INPUT TYPE=hidden NAME=dir VALUE=",Dir,">\n",
     "<BR><INPUT TYPE=submit VALUE=Ok>\n"
     "</FORM>\n"].

filter_files(Dir,[File|Files],Ds,Fs) ->
    case filename:extension(File) of
	".coverdata" ->
	    Fs1 = ["<OPTION VALUE=",File," onDblClick=submit()>",
		   File,"</OPTION>\n" | Fs],
	    filter_files(Dir,Files,Ds,Fs1);
	_ ->
	    FullName = filename:join(Dir,File),
	    case filelib:is_dir(FullName) of
		true ->
		    Ds1 = ["<OPTION VALUE=",File," onDblClick=submit()>",
			   File,"/</OPTION>\n" | Ds],
		    filter_files(Dir,Files,Ds1,Fs);
		false ->
		    filter_files(Dir,Files,Ds,Fs)
	    end
    end;
filter_files(_Dir,[],Ds,Fs) ->
    {Ds,Fs}.




do_import(Input) ->
    case parse(Input) of
	[{"file",File0},{"dir",Dir}] ->
	    File = filename:join(Dir,File0),
	    case filelib:is_dir(File) of
		true ->
		    import_frame1(File);
		false ->
		    case filelib:is_file(File) of
			true ->
			    case cover:import(File) of
				ok ->
				    import_frame1(Dir);
				{error,{cant_open_file,ExportFile,_Reason}} ->
				    import_frame1(Dir,
						  "Error importing file\\n\\\"" 
						  ++ ExportFile ++ "\\\"")
			    end;
			false ->
			    import_frame1(Dir,
					  "Error importing file\\n\\\"" ++ 
					  File ++ "\\\"")
		    end
	    end;
	[{"dir",Dir}] ->
	    import_frame1(Dir,"No file is selected")
    end.
	    

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                     %
% Different private helper functions                                   %
% 		                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Create the Header for the page If we now the mimetype use that type %%
%%otherwise use text                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
header() ->
    header("text/html").
header(MimeType) ->
    "Pragma:no-cache\r\n" ++
    "Content-type: " ++ MimeType ++ "\r\n\r\n".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
%%Create the Htmlheader set the title of the page                     %%       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
html_header(Title) ->    
    "<HTML>\n" ++
	"<HEAD>\n" ++
	"<TITLE>" ++ Title ++  "</TITLE>\n" ++
	"</HEAD>\n"
	"<BODY BGCOLOR=\"#FFFFFF\">\n".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Close the body- and Html tags                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
html_end()->
    "</BODY></HTML>".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A script which reloads the menu frame and possibly pops up an alert%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reload_menu_script(Err) ->
    ["<SCRIPT>\n",
     "function reloadMenu()\n",
     "  {\n",
     "    parent.menu.document.location.href=\"./menu_frame\";\n",
     case Err of
	 "" -> "";
	 _ -> "    alert(\""++Err++"\");\n"
     end,
     case get_warnings() of
	 [] -> 
	     "";
	 Warnings -> 
	     "    alert(\""++fix_newline(lists:flatten(Warnings))++"\");\n"
     end,
     "  }\n",
     "</SCRIPT>\n",
     "<BODY onLoad=reloadMenu() BGCOLOR=\"#FFFFFF\">"].

fix_newline([$\n|Rest]) ->
    [$\\,$n|fix_newline(Rest)];
fix_newline([$"|Rest]) ->
    [$\\,$"|fix_newline(Rest)];
fix_newline([Char|Rest]) ->
    [Char|fix_newline(Rest)];
fix_newline([]) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Control the input data and return the intresting values or error   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_input_data(Input,Key)->
    case lists:keysearch(Key,1,parse(Input)) of
	{value,{Key,Value}} ->
	    Value;
	false ->
	    undefined
    end.

parse(Input) ->
    httpd:parse_query(Input).


get_warnings() ->
    cover_group_leader_proc ! {self(), get_warnings},
    receive {warnings,Warnings} ->
	    Warnings
    end.
