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

%%%---------------------------------------------------------------------
%%% File    : webappmon.erl
%%% Author  : Martin G. <marting@erix.ericsson.se>
%%% Purpose : Frontend to the webbased version of appmon.
%%% Created : 24 Apr 2001 by Martin G. <marting@erix.ericsson.se>
%%%---------------------------------------------------------------------

-module(appmon_web).

%% The functions that the user can call to interact with the genserver
-export([init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([terminate/2,code_change/3]).

-export([node_info/2,application_info/2,application_env/2]).
-export([proc_info/2,trace/2]).
-export([start/0,stop/0,start_link/0]).

%% Export the function that returns the configuration data needed by
%% webtool
-export([configData/0]).


%% The following directive caters for (significantly) faster native
%% code compilation of one function in this file by the HiPE compiler
%% on register-poor architectures like the x86.
-compile([{hipe,[{regalloc,graph_color}]}]).

-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start the genserver                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link()->
    gen_server:start_link({local,webappmon_server},appmon_web,[],[]).
start()->
    gen_server:start({local,webappmon_server},appmon_web,[],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stop the genserver                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
stop()->
    gen_server:call(webappmon_server,stop,1000).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the page that shows the nodes and the apps on the sel node       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
node_info(Env,Input)->
    gen_server:call(webappmon_server,{node_data,Env,Input}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the application process tree                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
application_info(Env,Input)->
    gen_server:call(webappmon_server,{app_data,Env,Input}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the page that shows the data about the process                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
proc_info(Env,Input)->
    gen_server:call(webappmon_server,{proc_data,Env,Input}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the spec on the app                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
application_env(Env,Input)->
    gen_server:call(webappmon_server,{app_env,Env,Input}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Toggle the trace flag for the selected process                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trace(Env,Input)->
    gen_server:call(webappmon_server,{trace,Env,Input}).

configData()->
    {appmon,[{web_data,{"WebAppmon","/appmon/main_frame.html"}},
	     	{alias,{"/appmon",code:priv_dir(appmon)}},
		{alias,{erl_alias,"/erl",[appmon_web]}},
		{start,{child,{backend,{process_info,start_link,[]},
			       permanent,100,worker,[process_info]}}},
	     {start,{child,{{local,webappmon_server},
			    {appmon_web,start_link,[]},
			    permanent,100,worker,[appmon_web]}}}
	    ]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                      %
% Callback functions for the genserver                                 %
%                                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Arg)->
    {ok,[]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create the different pages                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({node_data,_Env,Input},_From,State)->
    {reply,app_and_node_sel_page(Input),State};

handle_call({app_data,_Env,Input},_From,State)->
    {reply,process_tree_page(Input),State};

handle_call({proc_data,_Env,Input},_From,State)->
    {reply,process_specifickation_page(Input),State};

handle_call({app_env,_Env,Input},_From,State)->
    {reply,application_specifickation_page(Input),State};

handle_call({trace,_Env,Input},_From,State)->
    {reply,toggle_trace(Input),State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Shutdown the genserver                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
terminate(_,_State)->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callback function currently not used ...                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast(_,State)->
    {noreply,State}.

handle_info(_,State)->
    {noreply,State}.

code_change(_OldVsn,State,_Extra)->
    {ok,State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% Private functions to create the part of the sides that is common   %%
%% to all sides                                                       %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create the Header for the page If we now the mimetype use that type%%
%% otherwise use text                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
header() ->
    header("text/html").
header(MimeType) ->
    "Content-type: " ++ MimeType ++ "\r\n\r\n".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create the Htmlheader sett the title of the side to nothing if     %%
%% we dont know the name of the side                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
html_header()->
   html_header("").

html_header(Part) ->    
    "<HTML BGCOLOR=\"#FFFFFF\">\n" ++
	"<HEAD>\n" ++
	"<TITLE>Appmon " ++ Part ++  "</TITLE>\n" ++
	"</HEAD>\n".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Close the Html tag and if neccessay add some clean upp             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
html_end()->
    "</HTML>".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% The functions that creates the whole pages by collecting           %%
%% the necessary data                                                 %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns the page where the user see's which nodes and apps that    %%
%% are availible for monitoring                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
app_and_node_sel_page(Input)->
    [header(),
     html_header(),
     node_body(httpd:parse_query(Input)),
     html_end()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns the process tree for the application whose name is         %%
%% the first value in the Inputs key/value list                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_tree_page(Input)->
    [header(),
     html_header(),
     application_javascript(httpd:parse_query(Input)),
     application_body(httpd:parse_query(Input)),
     html_end()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Send trace on or off to the process thats pid is the third arg of  %%
%% the inputs key/val list. Then it returns the process tree for the  %%
%% the application that is the first key/val pair of input            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
toggle_trace(Input)->
   send_trace(httpd:parse_query(Input)),
   [header(),
    html_header(),
     application_javascript(httpd:parse_query(Input)),
     application_body(httpd:parse_query(Input)),
    html_end()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creates the page that shows all information about the process that %%
%% that is the first arg of th input key/val pairs                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_specifickation_page(Input)->
    [header(),
     html_header(),
     process_body(httpd:parse_query(Input)),
     html_end()].

application_specifickation_page(Input)->
    [header(),
     html_header(),
     application_env_body(httpd:parse_query(Input)),
     html_end()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The Private functions that do the job                              %%
%% To build the the page that shows the applications                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Build the body of the side that shows the node name and            %%
%% the application list                                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
node_body([{"node",Node}|_Rest])->
    case  process_info:is_node(Node) of
	{true,Controlled_node,Name} ->
	    "<BODY BGCOLOR=\"#FFFFFF\">" ++
		node_selections_javascripts() ++
		node_selection(Controlled_node) ++
		node_title() ++
		application_tree(Controlled_node,Name) ++
		"</BODY>";
	       
	{false,Server_node,Name} ->
	    "<BODY BGCOLOR=\"#FFFFFF\">" ++
		node_selections_javascripts() ++
		node_selection(Server_node) ++
		node_title() ++
		application_tree(Server_node,Name) ++
		"</BODY>"
    end;

node_body(_Whatever)->
    node_body([{atom_to_list(node),atom_to_list(node())}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns the javascript that sets a new node to monitor             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
node_selections_javascripts()->
    "<SCRIPT>
      function node_selected()
      {                                     
         parent.frames.base_frames.location=\"../../appmon/start_info.html\"
         window.location =\"./node_info?node=\" + " ++ 
           "document.node_selection.nodes[document.node_selection.nodes.selectedIndex].value;
      }
      </SCRIPT>".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Insert the html code that shows the combobox where the user can    %%
%% select another node to monitor                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
node_selection(Node)->
    " <FORM ACTION=\"./node_info\" NAME=node_selection>\n
           <TABLE WIDTH=\"100%\" BORDER=\"0\">\n
              <TR><TD ALIGN=\"center\">\n 
                  <SELECT NAME=nodes onChange=\"node_selected()\">\n" ++
	print_nodes(order_nodes(Node,process_info:get_nodes())) ++
	"</SELECT>\n
              </TD></TR>\n
            </TABLE>\n
          </FORM>".
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Add the node we are working with in the beginning of the list and  %%
%% remove it from other places so its always the first in the listbox %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
order_nodes(Node,Node_list)->
    [Node|lists:delete(Node,Node_list)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Take the list of nodes and make it to a list of options to the     %%
%% the combobox                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_nodes([])->
    [];
print_nodes([Node|Rest])->
    "<OPTION value=\"" ++
	atom_to_list(Node) ++
	"\">" ++
	atom_to_list(Node) ++
	"\n" ++
	print_nodes(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create the header of the node info page i.e. the name of the node  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
node_title()->
   " <TABLE WIDTH=\"100%\" BORDER=\"0\">
	<TR><TD ALIGN=\"center\"><FONT SIZE=5>Applications</FONT></TD></TR>
    </TABLE>\n".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Build the body of the side that shows the node i.e the application %%
%% list                                                               %%
%% Node and Node_name are the same just different types               %%
%% Node are the atom  Node_name the string of the node name           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
application_tree(Node,Node_name)->
    Fun=fun(Name)->
		Name_str =atom_to_list(Name),
		"<LI><A HREF=\"./application_info?name=" ++ Name_str  ++
		    "&mode=all&node=" ++ Node_name ++
		    "\" TARGET=main><B>" ++ Name_str ++
		    "</B></A>" ++ print_space(15-length(Name_str),[]) ++
		    "<A HREF=\"./application_env?name=" ++ Name_str ++
		    "&node=" ++ Node_name ++
		    "\" TARGET=\"main\"><FONT SIZE=2>spec</FONT></A></LI>\n" 
	end,
    "<UL>" ++ 
	lists:map(Fun, (process_info:get_applications(Node))) ++
	"</UL>" ++
	"<FORM Name=reload>" ++
	"<INPUT TYPE=\"button\" onClick=\"node_selected()\" 
              VALUE=\"Reload\">\n" ++
	"</FORM>" ++
	"<!--<A HREF=\"../../appmon/application_help.html\" TARGET=main>Help</A>-->".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Print spaces between the application name and the spec link       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_space(N,Space)when N >0 ->
    print_space(N-1,"&nbsp;" ++ Space);
print_space(_N,Space)->
    Space.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The Private functions that do the job                              %%
%% To build the the page that shows process in an application         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Generates the javascript that govern the look of the page that      %%
%%the processes of an application                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Args is the list whit input args should be App Mode, Node
application_javascript(Args)when length(Args)>=3 ->
    Vars=
	"<SCRIPT>
            var proc;
            var app=\"" ++ element(2,lists:nth(1,Args)) ++ "\";
            var node=\"" ++ element(2,lists:nth(3,Args)) ++ "\";",
    CommonFuncs=
	" function reload_bottom_frame() 
         {parent.proc_data.location.href=\"/appmon/blank.html\";}

	 function show_process_info() 
         {
            if(proc.indexOf(\"#Port\")== -1)
            {
    	      if(proc.charAt(0)==\'<\')
                 window.location=\"./proc_info?name=\" + proc + \"&node=\" + node  
              else
                {
                  start=proc.indexOf(\"<\");
                  endpoint=proc.lastIndexOf(\">\");
                  window.location=\"./proc_info?name=\" + proc.slice(start,endpoint+1) + \"&node=\" + node  ;
               }
           }          
        }

        function trace()
        {
           if(proc.charAt(0)==\'<\')
              window.location=\"./trace?name=\" + app +  \"&mode=\" + get_mode() +  \"&node=\" + node + \"&proc=\" +  proc;
           else
           {
              start=proc.indexOf(\"<\");
              endpoint=proc.lastIndexOf(\">\");
              window.location=\"./trace?name=\" + app + \"&mode=\" + get_mode() + \"&node=\" + node + \"&proc=\" + 
                 proc.slice(start,endpoint+1) ;
           }         
        
       }
   
       function reload_page()\n
       {
          window.location=\"./application_info?name=\" + app + \"&mode=\" + get_mode() + \"&node=\" + node ; 
       }
       function get_mode()
       {
          for (i= 0; i < document.reload_form.mode.length; i++)
          {
             if (document.reload_form.mode[i].checked)
                  return(document.reload_form.mode[i].value);
          }
          return(\"all\");
       }",   
    Vars++CommonFuncs++"</SCRIPT>";
application_javascript(_)->
    "<SCRIPT></SCRIPT>".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create the body i.e the process tree for the applications whose    %%
%% name is the second arg in the first tuple                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%args is the list [{_,Appname},{_,Mode},{_Node}]
application_body(Args) when is_list(Args),length(Args) >= 3 ->
    App=element(2,lists:nth(1,Args)),
    Mode=element(2,lists:nth(2,Args)),
    Node=element(2,lists:nth(3,Args)),
    "<BODY BGCOLOR=\"FFFFFF\" onLoad=reload_bottom_frame() >"
	++ mode_selection(Mode) ++ 
	selected_app_header(App,Node) ++ process_tree(App,Mode,Node)++ 
	"</BODY>";

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% If the pattern above ain't match then something is wrong           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
application_body(_Whatever)->
    "Please use the links to the left".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create the part of the process tree page side where the user can   %%
%% select the mode the view the tree in.                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mode_selection(Mode)->
    "<FORM NAME=\"reload_form\">\n" ++
	"<TABLE>" ++
	"<TR>\n" ++
	"<!--<TD><INPUT TYPE=\"button\" NAME=\"refresh_but\" VALUE=\"Reload\" onClick=\"reload_page()\">
         &nbsp;&nbsp;&nbsp;&nbsp;</TD>\n-->" ++
	print_radio_buttons([{"all","All processes"},{"sup_child","Supervised processes"},
	{"sup","Only supervisors"}],Mode) ++
	"</TR>\n </TABLE>\n" ++
	"</FORM>".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Print the radiobuttons. if the mode is the one the current         %%
%% radiobutton represent set the one checked                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_radio_buttons([],_)->
    [];
print_radio_buttons([{Mode,Name}|Rest],Mode)->
    "<TD><INPUT TYPE=\"radio\" NAME=\"mode\" CHECKED=\"true\" VALUE=\""++ 
	Mode ++"\" onClick=\"reload_page()\">&nbsp;&nbsp;" ++Name ++
	"</TD>\n" ++ print_radio_buttons(Rest,Mode);
print_radio_buttons([{Mode1,Name}|Rest],Mode)->
    "<TD><INPUT TYPE=\"radio\" NAME=\"mode\" VALUE=\""++ Mode1 ++
	"\" onClick=\"reload_page()\">&nbsp;&nbsp;" ++ Name ++
	"</TD>\n" ++
	print_radio_buttons(Rest,Mode).
       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The part that shows the name of the application  that the process  %%
%% tree represent                                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
selected_app_header(App,Node)->
    {Year,Mon,Day}=date(),
    "<TABLE>
        <TR> 
          <TD>Node:</TD> 
          <TD>" ++Node ++"</TD>
        </TR>
        <TR> 
          <TD>Application:</TD> 
          <TD>" ++App ++"</TD>
        </TR>
       <TR>  
          <TD>Date:</TD> 
          <TD>" ++ integer_to_list(Day) ++ "/" ++ 
                 integer_to_list(Mon) ++"&nbsp;-&nbsp;"++ 
                 integer_to_list(Year)  ++ 
         "</TD> 
        </TR>
     </TABLE>
     <TABLE WIDTH=100%>
       <TR>
         <TD>
             <HR WIDTH=\"80%\">
             <!--<FONT SIZE=4>Process tree</FONT>
             <HR ALIGN=\"center\" WIDTH=\"80%\">-->
         </TD>
       </TR>
     </TABLE>".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%get the process tree from process_info and build the nested         %%
%% unordered list that represent the applications process tree        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_tree(App,Mode,Node)->
    case process_info:get_processes(list_to_atom(App),
				    list_to_atom(Mode),
				    list_to_atom(Node)) of
	unknown->
	    "Unknown application please update application tree";
	{Tree,Traced_dict} ->
	    "<UL>" ++
		htmlify_tree(Tree,Traced_dict,1,Node,Mode,App) ++
		"</UL>"
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Build each node in the tree and then build its children            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
htmlify_tree({Pid,Childs,Childs2},Proc_tab,N,Node,Mode,App)->
    case ets:lookup(Proc_tab,Pid) of
	[] when N<3->
	     print_pid(Pid,Node,Mode,App,notrace)++
		htmlify_prim_child(Childs,Proc_tab,N+1,Node,Mode,App) ++
		htmlify_sec_child(Childs2);
	[_Traced]->
	    print_pid(Pid,Node,Mode,App,"<FONT SIZE=2 COLOR=\"firebrick\">Stop Trace</FONT>")++
		htmlify_prim_child(Childs,Proc_tab,N+1,Node,Mode,App) ++
		htmlify_sec_child(Childs2);
	[]-> 
	    print_pid(Pid,Node,Mode,App,"<FONT SIZE=2>Start Trace</FONT>")++
		htmlify_prim_child(Childs,Proc_tab,N+1,Node,Mode,App) ++
		htmlify_sec_child(Childs2)	      	 
    end.

print_pid(Pid,Node,_Mode,_App,notrace)->
    "<LI><A TARGET=\"proc_data\" STYLE=\"text-decoration:none; color:blue\" HREF=\"./proc_info?name=" ++ urlify_pid(Pid) ++ 
	"&node="++ Node  ++" \" >"++ htmlify_pid(Pid,[])  ++ 
	"</A>";

print_pid([$P,$o,$r,$t|Rest],_Node,_Mode,_App,_TrMode)->
    "<LI>" ++ htmlify_pid([$P,$o,$r,$t|Rest],[]);

print_pid(Pid,Node,Mode,App,TrMode)->
    "<LI><A TARGET=\"proc_data\" STYLE=\"text-decoration:none; color:blue\" HREF=\"./proc_info?name=" ++ 
	urlify_pid(Pid) ++ "&node="++ Node  ++" \" >"++
	htmlify_pid(Pid,[])  ++ "</A>"++ 
	"&nbsp;&nbsp;&nbsp
                <A HREF=\"./trace?app="++App++"&mode="++Mode++
	"&node="++Node++"&proc="++urlify_pid(Pid)++"\">
               "++TrMode++"</A>".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Change the '<' sign and the '>' sign to the html representation   %%
%% of the sign                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
htmlify_pid([60|Pid],New)->
    htmlify_pid(Pid,";tl&"++New);
htmlify_pid([139|Pid],New)->
    htmlify_pid(Pid,";tl&"++New);

htmlify_pid([62|Pid],New)->
    htmlify_pid(Pid,";tg&"++New);
htmlify_pid([155|Pid],New)->
    htmlify_pid(Pid,";tg&"++New);
htmlify_pid([Chr|Pid],New)->
    htmlify_pid(Pid,[Chr|New]);
htmlify_pid([],New)->
    lists:reverse(New).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Change the < and > sign to the representation of the signs in      %%
%% the HTTP protocol                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
urlify_pid(Pid) ->
    case regexp:first_match(Pid,"[<].*[>]") of
	{match,Start,Len}->
	    "%3C"++string:substr(Pid,Start+1,Len-2)++"%3E";
	_->
	    Pid
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Change the < and > sign from the representation of the signs in    %%
%% the HTTP protocol                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unurlify_pid(Pid)->
    unurlify_pid(Pid,[]).

unurlify_pid([$%,$3,$C|Rest],New)->
    unurlify_pid(Rest,[60|New]);

unurlify_pid([$%,$3,$E|Rest],New)->
    unurlify_pid(Rest,[62|New]);
unurlify_pid([Chr|Rest],New)->
    unurlify_pid(Rest,[Chr|New]);

unurlify_pid([],New)->
    lists:reverse(New).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Make html of the list of primary childs                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
htmlify_prim_child([],_Proc_tab,_N,_Node,_Mode,_App)->
    [];

htmlify_prim_child(Childs,Proc_tab,N,Node,Mode,App)->
    Fun=fun(Child)->
		htmlify_tree(Child,Proc_tab,N,Node,Mode,App)
	end,
    "<UL>\n" ++ lists:map(Fun,Childs) ++ "</UL>\n".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Make html of hte list whit sedondary childs, they has no childs    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
htmlify_sec_child([])->
    [];

htmlify_sec_child(Sec_child_list)->
    Htmlify_child=fun(Pid1)->
			  "<LI><FONT COLOR=\"#FF2222\">" ++ Pid1 ++
			      "</FONT></LI>\n"
		  end,
    "<UL>" ++ lists:map(Htmlify_child,Sec_child_list) ++ "</UL>\n".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The Private functions that do the job                              %%
%% To build the the page that shows process data                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The function that creates the collects the various part of         %%
%% the side that shows information about a specific process,          %%
%% Pid_name should be the list representation of a pid                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_body(Args) when length(Args)==2->
    Pid=element(2,lists:nth(1,Args)),
    Node=element(2,lists:nth(2,Args)),
    "<BODY BGCOLOR=\"#FFFFFF\">"  ++
	process_information_table(Pid,Node) ++ "</BODY>";

process_body(_)->
    "<BODY BGCOLOR=\"#FFFFFF\">Please dont call this side manually</BODY>".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create the table that shows the name of the pid to show extended   %%
%% info about                                                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Get the table that shows the extended info about a process         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_information_table(Pid_name,Node)->
    PidID=unurlify_pid(Pid_name),
    case catch list_to_pid(PidID) of
	Pid when is_pid(Pid) -> 
	    get_process_table(Pid,Node);
	_Other ->
	    io_lib:format("Not a process id ~s",[PidID])
    end.
	      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create the table that shoows the extended info about processes     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_process_table(Pid,Node_name) when is_list(Node_name)->
    Node=list_to_atom(Node_name),
    get_process_table(Pid,Node);

get_process_table(Pid,Node) when is_atom(Node)->
    case lists:member(Node,[node()|nodes()]) of
	true-> 
	    Proc_data=process_info:get_process_data(Pid,Node),
	    "<TABLE BORDER=1 >
                <TR BGCOLOR=\"#8899AA\"><TD COLSPAN=6 ALIGN=\"center\" > 
                       <FONT size=4> Process" ++
		htmlify_pid(pid_to_list(Pid),[])  ++  "</FONT>
               </TD></TR>" ++
		   start_process_proc_data(Proc_data) ++
		"</TABLE><BR><BR>";
	_ ->
	    "Please try again the Node dont exists"
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The process information is quite messy tidi it up by creating a    %%
%% table that looks like key           val                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_process_proc_data(Proc_data)->
    %%Pic out the special cases the links and the process dict
    {Special,Usual}=split_proc_data(Proc_data),
    Usual2=append_empty(Usual),
    UsualProcData=process_proc_data(Usual2,0),
    SpecProcData=process_proc_data(Special),
    UsualProcData++SpecProcData.

append_empty(List) when length(List) rem 2 == 0 ->
    List;
append_empty(List)->
    append_empty(lists:append(List,[empty])).

split_proc_data(Proc_data)->
    Spec=lists:map(fun(Key)->
			   case lists:keysearch(Key,1,Proc_data) of
			       {value,Data}->
				   Data;
			       _ ->
				   not_included
			   end
		   end,[links,dictionary,messages]),
    Spec2=clear(Spec,[]),
    Usual=lists:filter(fun({Key,_Val})->
			       case Key of
				   messages ->
				       false;
				   links ->
				       false;
				   dictionary ->
				       false;
				   _ ->
				       true
			       end
		       end,Proc_data),
    {Spec2,Usual}.
		       
clear([],New)->
    New;
clear([not_included|Spec],New)->
    clear(Spec,New);
clear([Other|Spec],New)->
    clear(Spec,[Other|New]).

process_proc_data(Data,3)->
    "</TR>"++process_proc_data(Data,0);
process_proc_data([],_N)->
    [];
process_proc_data(Data,0)->
    "<TR>"++process_proc_data(Data,1);

process_proc_data([empty|Data],N)->
    "<TD>&nbsp;</TD><TD>&nbsp;</TD> "++process_proc_data(Data,N+1);

process_proc_data([{current_function,MFA}|Rest],N)->
    "<TD NOWRAP=true><FONT SIZE=3><B>Current function:</B></TD><TD><FONT SIZE=3>"++ 
	io_lib:format("~p",[MFA]) ++"</TD>\n " ++
	process_proc_data(Rest,N+1);

process_proc_data([{error_handler,Mod}|Rest],N)->
    "<TD NOWRAP=\"true\"><B><FONT SIZE=3>Error handler:</B></TD><TD><FONT SIZE=3>" 
	++ atom_to_list(Mod)  ++ "</TD>\n" ++
	process_proc_data(Rest,N+1);

process_proc_data([{group_leader,Grp}|Rest],N)->
    "<TD NOWRAP=\"true\"><FONT SIZE=3><B>Group leader:</B></TD><TD><FONT SIZE=3>" ++ 
	htmlify_pid(pid_to_list(Grp),[])  ++ "</TD>\n " ++
	process_proc_data(Rest,N+1);

process_proc_data([{heap_size,Size}|Rest],N)->
    "<TD NOWRAP=\"true\"><FONT SIZE=3><B>Heap size:</B></TD><TD><FONT SIZE=3>" 
	  ++ integer_to_list(Size)  ++ "</TD>\n " ++
	process_proc_data(Rest,N+1);

process_proc_data([{initial_call,MFA}|Rest],N)->
    "<TD NOWRAP=\"true\"><FONT SIZE=3><B>Initial call:</B></TD><TD><FONT SIZE=3>"++ 
         io_lib:format("~p",[MFA]) ++"</TD>\n " ++
	process_proc_data(Rest,N+1);
	
process_proc_data([{message_queue_len,Size}|Rest],N)->
    "<TD NOWRAP=\"true\"><FONT SIZE=3><B>Message queue length:</B></TD><TD><FONT SIZE=3>" ++
	integer_to_list(Size)  ++ "</TD>\n " ++
	process_proc_data(Rest,N+1);

process_proc_data([{priority,Level}|Rest],N)->
    "<TD><FONT SIZE=3><B>Process priority:</B></TD><TD><FONT SIZE=3>" ++
	atom_to_list(Level)  ++ "</TD>\n " ++
	process_proc_data(Rest,N+1);

process_proc_data([{reductions,Number}|Rest],N)->
    "<TD ><FONT SIZE=3><B>Number of executed reductions:</B></TD>
         <TD><FONT SIZE=3>" ++ integer_to_list(Number)  ++ "</TD>\n " ++
	process_proc_data(Rest,N+1);

process_proc_data([{registered_name,Name}|Rest],N)->
    "<TD NOWRAP=\"true\"><FONT SIZE=3><B>Process Name:</B></TD><TD><FONT SIZE=3>" 
	++ atom_to_list(Name)  ++ "</TD>\n" ++
	process_proc_data(Rest,N+1);

process_proc_data([{stack_size,Size}|Rest],N)->
    "<TD NOWRAP=\"true\"><FONT SIZE=3><B>Stack size:</B></TD><TD><FONT SIZE=3>" 
	++ integer_to_list(Size)  ++ "</TD>\n " ++
	process_proc_data(Rest,N+1);

process_proc_data([{status,Status}|Rest],N)->
    "<TD NOWRAP=\"true\"><FONT SIZE=3><B>Process status:</B></TD><TD><FONT SIZE=3>" 
	++ atom_to_list(Status)  ++ "</TD>\n " ++
	process_proc_data(Rest,N+1);

process_proc_data([{trap_exit,Boolean}|Rest],N)->
    "<TD NOWRAP=\"true\" ><FONT SIZE=3><B>Trap Exit:</B></TD><TD><FONT SIZE=3>" 
	++ atom_to_list(Boolean)  ++ "</TD>\n " ++
	process_proc_data(Rest,N+1);

process_proc_data([{Key,Val}|Rest],N)->
    "<TD NOWRAP=\"true\" ><FONT SIZE=3><B>" ++  io_lib:write(Key) ++
	"</B></TD><TD><FONT SIZE=3>" ++ io_lib:write(Val)  ++
	"</TD>\n " ++
	process_proc_data(Rest,N). 

process_proc_data([])->
    [];
process_proc_data([{links,List_of_pids}|Rest])->
    "<TR><TD NOWRAP=\"true\"><FONT SIZE=3><B>Links:</B></TD><TD COLSPAN=5><FONT SIZE=3>"++ print_links(List_of_pids)  ++"</TD></TR>\n " ++
	process_proc_data(Rest);

process_proc_data([{messages,Queue}|Rest])->
    "<TR><TD NOWRAP=\"true\"><FONT SIZE=3><B>Message Queue:</B></TD><TD COLSPAN=5><FONT SIZE=3>" ++ io_lib:write(Queue)  ++ "</TD></TR>\n " ++
	process_proc_data(Rest);

process_proc_data([{dictionary,Dict}|Rest])->
    "<TR><TD NOWRAP=\"true\"><FONT SIZE=3><B>Process dictionary:</B></TD><TD COLSPAN=5><FONT SIZE=3>&nbsp;</TD></TR>\n " ++
	get_dictionary_data(Dict) ++
	process_proc_data(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% In the process info there are the links to other processes print   %%
%% this pid                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_links(Pids)->
    print_links(Pids,[]).
			 
print_links([],Links)->
    htmlify_pid(Links,[]);

print_links([Pid],Links) when is_pid(Pid) ->
    print_links([],Links ++ pid_to_list(Pid));

print_links([Pid],Links) when is_port(Pid) ->
    print_links([],Links ++ erlang:port_to_list(Pid));

print_links([Pid|Rest],Links) when is_pid(Pid) ->
    print_links(Rest,Links ++ pid_to_list(Pid) ++ ", ");

print_links([Pid|Rest],Links) when is_port(Pid) ->
    print_links(Rest,Links ++ erlang:port_to_list(Pid) ++ ", ").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fix the data in the process dictionary                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_dictionary_data([])->
    [];

get_dictionary_data([{Key,Val}|Dict])->
    FormatedVal=add_space(htmlify_pid(lists:flatten(fix_type(Val)),[])),
    "<TR><TD><FONT SIZE=3>" ++
	htmlify_pid(lists:flatten(fix_type(Key)),[]) ++
	"</TD><TD COLSPAN=5><FONT SIZE=3>" ++ 
	FormatedVal++ "</TD></TR>\n" ++
	get_dictionary_data(Dict).

add_space(List)->
    add_space(List,0,[]).
add_space([],_Len,New) ->
    lists:reverse(New);
add_space([Char|Rest],Len,New)when Len<50 ->
    add_space(Rest,Len+1,[Char|New]);

add_space([$\,|Rest],_Len,New) ->
    add_space(Rest,0,[$\ ,$,|New]);

add_space([Char|Rest],Len,New) ->
    add_space(Rest,Len+1,[Char|New]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interpret the type of the data and make it to a list               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fix_type(Val) when is_atom(Val) ->
    atom_to_list(Val);
fix_type(Val) when is_float(Val) ->
    float_to_list(Val);
fix_type(Val) when is_integer(Val) ->
    integer_to_list(Val);
fix_type(Val) when is_list(Val) ->
    case io_lib:printable_list(Val) of
	true->
	    case Val of
		[]->
		    io_lib:write(Val);
		_->
		    Val
	    end;
	_->
	    io_lib:write(Val)
    end;
fix_type(Val) when is_pid(Val) ->
    pid_to_list(Val);
fix_type(Val) when is_port(Val) ->
    erlang:port_to_list(Val);
fix_type(Val) when is_tuple(Val) ->
    io_lib:write(Val);
fix_type(_Val) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The Private functions that send the trace signal to the process    %%
%% thats the 4 member of the Arg list                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_trace(Args)when length(Args)>=4->
    {_,Proc}=lists:nth(4,Args),
    Pid2=unurlify_pid(Proc),
    process_info:send_trace(Pid2);
 
send_trace(_Args)->
    arg_error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions that prints the application environment          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
application_env_body(Args)when length(Args)>=2 ->
    App=element(2,lists:nth(1,Args)),
    Node=element(2,lists:nth(2,Args)),
    "<SCRIPT>
       function reload_bottom_frame() 
         {parent.proc_data.location.href=\"/appmon/blank.html\";}
     </SCRIPT>
    <BODY BGCOLOR=\"#FFFFFF\" onLoad=reload_bottom_frame()>"
	++ application_env_table(App,Node) ++ "</BODY>";

application_env_body(_)->
    "<BODY BGCOLOR=\"#FFFFFF\">Please dont call this side manually</BODY>".

application_env_table(App,Node)->
    case process_info:get_application_keys(list_to_atom(App),
					   list_to_atom(Node)) of
	{ok,List}->
	    "<TABLE BORDER=1>" ++ application_env_head(App,Node) ++
		print_key_list(List,[]) ++ "</TABLE>";
	_ ->
	    "Please try again,something went wrong"
    end.

application_env_head(App,Node)->
    "<TR BGCOLOR=\"#8899AA\"><TD ALIGN=\"center\" COLSPAN=3> 
                <FONT SIZE=6>" ++ App ++ "@" ++ Node ++ "</FONT>\n
           </TD></TR>
           <TR><TD COLSPAN=3>&nbsp</TD></TR>
          <TR BGCOLOR=\"#8899AA\">
             <TD><B>Key</B></TD><TD><B>Val/Sec. key</B></TD><TD><B>Sec. Val</B></TD>
         </TR>".

print_key_list([],Result)->
    Result;

print_key_list([{application,Name}|Rest],Result)->
    print_key_list(Rest,Result ++ print_key("Application name :",Name));

print_key_list([{description,Desc}|Rest],Result)->
    print_key_list(Rest,Result ++ print_key("Description :",Desc));

print_key_list([{vsn,Ver}|Rest],Result)->
    print_key_list(Rest,Result ++ print_key("Version :",Ver));

print_key_list([{id,Id}|Rest],Result)->
    print_key_list(Rest,Result ++ print_key("ID:",fix_type(Id)));

print_key_list([{modules,Mods}|Rest],Result)->
    print_key_list(Rest,Result ++ print_key("Modules:","&nbsp;") ++
		   print_secondary_list(Mods,[]));

print_key_list([{maxP,Max}|Rest],Result)->
    print_key_list(Rest,Result ++
		   print_key("Max nr of processes",fix_type(Max)));

print_key_list([{maxT,Max}|Rest],Result)->
    print_key_list(Rest,Result ++
		   print_key("Max running sec:",fix_type(Max)));

print_key_list([{registered,Names}|Rest],Result)->    
    print_key_list(Rest,Result ++
		   print_key("Registered names:","&nbsp;") ++
		   print_secondary_list(Names,[]));

print_key_list([{applications,Apps}|Rest],Result)->
    print_key_list(Rest,Result ++ print_key("Depends on:","&nbsp") ++
		   print_secondary_list(Apps,[]));

print_key_list([{included_applications,Inc_apps}|Rest],Result)->
    print_key_list(Rest,Result ++
		   print_key("Included applications:",
			     fix_type(Inc_apps)));

print_key_list([{env,Env}|Rest],Result)->
    print_key_list(Rest,Result ++
		   print_key("Environment:",fix_type(Env)));

print_key_list([{mod,Mod}|Rest],Result)->
    print_key_list(Rest,Result ++
		   print_key("Application callback mod:",
			     fix_type(Mod)));

print_key_list([{start_phases,Phase_arg}|Rest],Result)->
    print_key_list(Rest,Result ++
		   print_key("Application callback mod:",
			     fix_type(Phase_arg)));

print_key_list([_|Rest],Result)->
    print_key_list(Rest,Result).

print_key(Label,Val)->
    "<TR>
        <TD><B>" ++ Label ++ "</B></TD><TD>" ++ Val ++
	"</TD><TD>&nbsp;</TD>
    </TR>".

print_key2(Label,Val)->
    "<TR>
        <TD>&nbsp;</TD><TD>" ++ Label ++ "</TD><TD>" ++ Val ++  "</TD>
    </TR>".

print_secondary_list([],Result)->
    Result;
print_secondary_list([{Mod,Ver}|Rest],Result) ->
    print_secondary_list(Rest,Result ++
			 print_key2(fix_type(Mod),fix_type(Ver)));

print_secondary_list([Mod|Rest],Result) ->
    print_secondary_list(Rest,Result ++
			 print_key2(fix_type(Mod),"&nbsp;")).
