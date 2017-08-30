%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : orber_web.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module(orber_web).

-export([menu/2, 
	 configure/2,
	 info/2,
	 nameservice/2,
	 ifr_select/2,
	 ifr_data/2, 
	 create/2,
	 delete_ctx/2,
	 add_ctx/2,
	 delete_obj/2]).

%%----------------------------------------------------------------------
%%-------------- Defines & Includes ------------------------------------
%%----------------------------------------------------------------------

-include("ifr_objects.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming_NamingContext.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

-define(DEBUG_LEVEL, 5).

-define(INFO_DATA, 
	[{iiop_timeout, "IIOP Request Timeout"},
	 {iiop_connection_timeout, "IIOP Connection Timeout"},
	 {iiop_setup_connection_timeout, "IIOP Setup Connection Timeout"},
	 {iiop_port, "IIOP Port"},
	 {domain, "Orber Domain"},
	 {orber_nodes, "Nodes in Domain"},
	 {giop_version, "Default GIOP Version"},
	 {objectkeys_gc_time, "Objectkeys GC"},
	 {get_interceptors, "Using Interceptors"},
	 {get_debug_level, "Debug Level"},
	 {get_ORBInitRef, "ORBInitRef"},
	 {get_ORBDefaultInitRef, "ORBDefaultInitRef"}]).

-define(IFR_DATA, [{"ir_ModuleDef", "Modules"},
		   {"ir_InterfaceDef", "Interfaces"},
		   {"ir_StructDef", "Structs"},
		   {"ir_UnionDef", "Unions"},
		   {"ir_ExceptionDef", "Exceptions"},
		   {"ir_ConstantDef", "Constants"},
		   {"ir_EnumDef", "Enumerants"},
		   {"ir_AliasDef", "Aliases"},
		   {"ir_AttributeDef", "Attributes"},
		   {"ir_OperationDef", "Operations"},
		   {"ir_Contained", "Contained"},
		   {"ir_TypedefDef", "Typedef"}]).


%%----------------------------------------------------------------------
%%-------------- External API ------------------------------------------
%%----------------------------------------------------------------------
%% Function   : create
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
create(_Env, [{"node",NodeStr}]) ->
    Node  = list_to_atom(NodeStr),
    is_running(Node, NodeStr),
    ["<BODY BGCOLOR=\"#FFFFFF\">
      <TABLE border=0 BGCOLOR=\"#FFFFFF\">
      <TD ALIGN=\"center\" COLSPAN=2><FONT SIZE=6>Create a New Object</FONT></TD></TR>
      <TR><TD><FORM METHOD=\"POST\" ACTION=\"./create\">
      <TR><TD><INPUT TYPE=\"HIDDEN\" NAME=\"node\" VALUE=\"", NodeStr, "\">
      <TR><TD><B>Module</B></TD><TD><INPUT TYPE=\"TEXT\" SIZE=\"50\" NAME=\"module\" VALUE=\"\"></TD></TR>
      <TR><TD><B>Arguments</B></TD><TD><INPUT TYPE=\"TEXT\" SIZE=\"50\" NAME=\"arguments\"></TD></TR>
      <TR><TD><B>Options</B></TD><TD><INPUT TYPE=\"TEXT\" SIZE=\"50\" NAME=\"options\"></TD></TR>
      <TR><TD><B>Name String</B></TD><TD><INPUT TYPE=\"TEXT\" SIZE=\"50\" NAME=\"namestr\"></TD></TR>
      <TR><TD><B>Operation to use</B></TD>
      <TD><B>&nbsp;&nbsp;&nbsp;<INPUT type=\"radio\" name=\"bind\" value=\"bind\" CHECKED=\"true\">Bind</B>
      <B>&nbsp;&nbsp;&nbsp;<INPUT type=\"radio\" name=\"bind\" value=\"rebind\">Rebind</B></TD></TR>
      <TR><TD ALIGN=\"center\" COLSPAN=2><INPUT TYPE=\"SUBMIT\" VALUE=\"Create it\"></FORM></TD></TR></TABLE>"];
create(_Env, [{"node",NodeStr}, {"module", ModStr}, {"arguments",ArgsStr},
	      {"options",OptionsStr}, {"namestr", Name}, {"bind", How}]) ->
    Node  = list_to_atom(NodeStr),
    Mod   = list_to_atom(ModStr),
    Args = parse_data(ArgsStr),
    Options = parse_data(OptionsStr),
    case catch rpc:call(Node, Mod, oe_create, [Args, [{sup_child, true}|Options]]) of
	{ok, Pid, Object} ->
	    case catch bind(Node, Object, Name, How) of
		{ok, IOR} ->
		    ["<BODY BGCOLOR=\"#FFFFFF\"><BR><B>Successfully created the object:</B><BR><BR>", IOR];
		{ok, IOR, Path} ->
		    ["<BODY BGCOLOR=\"#FFFFFF\"><BR><B>Successfully created and stored the object as: \"", 
		     Path, "\" (", pid_to_list(Pid), ")</B><BR><BR>", IOR];
		What ->
		    rpc:call(Node, corba, dispose, [Object]),
		    orber:dbg("[~p] orber_web:create(~p, ~p, ~p, ~p, ~p); 
Unable to bind object: ~p", [?LINE, Node, Mod, Args, Options, Name, What], ?DEBUG_LEVEL),
		    ["<BODY BGCOLOR=\"#FFFFFF\">Unable to bind object in the NameService using: ", Name]
	    end;
	Object when element(2, Object) == pseudo ->
	    case catch bind(Node, Object, Name, How) of
		{ok, IOR} ->
		    ["<BODY BGCOLOR=\"#FFFFFF\"><BR><B>Successfully created the object:</B><BR><BR>", IOR];
		{ok, IOR, _} ->
		    ["<BODY BGCOLOR=\"#FFFFFF\"><BR><B>Successfully created and stored the object as :\"", Name, "\"</B><BR><BR>", IOR];
		What ->
		    rpc:call(Node, corba, dispose, [Object]),
		    orber:dbg("[~p] orber_web:create(~p, ~p, ~p, ~p, ~p); 
Unable to bind object: ~p", [?LINE, Node, Mod, Args, Options, Name, What], ?DEBUG_LEVEL),
		    ["<BODY BGCOLOR=\"#FFFFFF\">Unable to bind object in the NameService using: ", Name]
	    end;
	What->
	    orber:dbg("[~p] orber_web:create(~p, ~p, ~p, ~p, ~p); 
Unable to create object: ~p", [?LINE, Node, Mod, Args, Options, Name, What], ?DEBUG_LEVEL),
	    ["<BODY BGCOLOR=\"#FFFFFF\">Unable to create the object."]
    end.

bind(Node, Obj, "", _) ->
    IOR = rpc:call(Node, corba, object_to_string, [Obj]),
    {ok, IOR};
bind(Node, Obj, NameStr, How) ->
    NS = check(rpc:call(Node, corba, resolve_initial_references, ["NameService"])),
    Name = check(rpc:call(Node, 'CosNaming_NamingContextExt', to_name, [NS, NameStr])),
    case How of
	"bind" ->
	    check(rpc:call(Node, 'CosNaming_NamingContext', bind, [NS, Name, Obj])),
	    IOR = rpc:call(Node, corba, object_to_string, [Obj]),
	    {ok, IOR, NameStr};
	"rebind" ->
	    check(rpc:call(Node, 'CosNaming_NamingContext', rebind, [NS, Name, Obj])),
	    IOR = rpc:call(Node, corba, object_to_string, [Obj]),
	    {ok, IOR, NameStr}
    end.


%%----------------------------------------------------------------------
%% Function   : delete_ctx
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
delete_ctx(_Env, [{"node",NodeStr}, {"context", Ref}]) ->
    Node = list_to_atom(NodeStr),
    {Ctx, NS} = remote_resolve(Node, Ref),
    Name = check(rpc:call(Node, 'CosNaming_NamingContextExt', to_name, [NS, Ref])),
    check(rpc:call(Node, 'CosNaming_NamingContextExt', unbind, [NS, Name])),
    check(rpc:call(Node, 'CosNaming_NamingContextExt', destroy, [Ctx])),
    ["<BODY BGCOLOR=\"#FFFFFF\"> 
      <TABLE BORDER=0><TR BGCOLOR=\"#FFFFFF\"><TD ALIGN=\"center\" COLSPAN=1> 
      <FONT SIZE=6>Successfully deleted the Context: ", Ref, "</FONT>\n
      </TD></TR></TABLE>
      <FORM Name=goback><INPUT TYPE=\"button\" onClick=javascript:history.go(-2) VALUE=\"Go Back\">\n</FORM>"].
	
%%----------------------------------------------------------------------
%% Function   : add_ctx
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
add_ctx(_Env, [{"node",_NodeStr}, {"context", "root"}, {"id", ""}]) ->
    ["<BODY BGCOLOR=\"#FFFFFF\"> 
      <TABLE BORDER=0><TR BGCOLOR=\"#FFFFFF\"><TD ALIGN=\"center\" COLSPAN=1> 
      <FONT SIZE=4>You must supply a NameString such as:<BR>
                   See also 'Interoperable Naming Service' in the User's Guide.</FONT>\n
      </TD></TR></TABLE>
      <FORM Name=goback><INPUT TYPE=\"button\" onClick=javascript:history.go(-1) VALUE=\"Go Back\">\n</FORM>"];
add_ctx(_Env, [{"node",NodeStr}, {"context", "root"}, {"id", Id}]) ->
    Node = list_to_atom(NodeStr),
    NS = check(rpc:call(Node, corba, resolve_initial_references, ["NameService"])),
    Name = check(rpc:call(Node, 'CosNaming_NamingContextExt', to_name, [NS, Id])),
    check(rpc:call(Node, 'CosNaming_NamingContextExt', bind_new_context, [NS, Name])),
    ["<BODY BGCOLOR=\"#FFFFFF\"> 
      <TABLE BORDER=0><TR BGCOLOR=\"#FFFFFF\"><TD ALIGN=\"center\" COLSPAN=1> 
      <FONT SIZE=6>Successfully bound the new Context: ", Id, "</FONT>\n
      </TD></TR></TABLE>
      <FORM Name=goback><INPUT TYPE=\"button\" onClick=javascript:history.go(-1) VALUE=\"Go Back\">\n</FORM>"];
add_ctx(_Env, [{"node",NodeStr}, {"context", Ref}, {"id", Id}]) ->
    NameStr = Ref ++ "/" ++ Id,
    Node = list_to_atom(NodeStr),
    NS = check(rpc:call(Node, corba, resolve_initial_references, ["NameService"])),
    Name = check(rpc:call(Node, 'CosNaming_NamingContextExt', to_name, [NS, NameStr])),
    check(rpc:call(Node, 'CosNaming_NamingContextExt', bind_new_context, [NS, Name])),
    ["<BODY BGCOLOR=\"#FFFFFF\"> 
      <TABLE BORDER=0><TR BGCOLOR=\"#FFFFFF\"><TD ALIGN=\"center\" COLSPAN=1> 
      <FONT SIZE=6>Successfully bound the new Context: ", NameStr, "</FONT>\n
      </TD></TR></TABLE>
      <FORM Name=goback><INPUT TYPE=\"button\" onClick=javascript:history.go(-1) VALUE=\"Go Back\">\n</FORM>"].
	
%%----------------------------------------------------------------------
%% Function   : delete_obj
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
delete_obj(_Env, [{"node",NodeStr}, {"context", Ref}, {"action", "unbind"}]) ->
    Node = list_to_atom(NodeStr),
    NS = check(rpc:call(Node, corba, resolve_initial_references, ["NameService"])),
    Name = check(rpc:call(Node, 'CosNaming_NamingContextExt', to_name, [NS, Ref])),
    check(rpc:call(Node, 'CosNaming_NamingContextExt', unbind, [NS, Name])),
    ["<BODY BGCOLOR=\"#FFFFFF\"> 
      <TABLE BORDER=0><TR BGCOLOR=\"#FFFFFF\"><TD ALIGN=\"center\" COLSPAN=1> 
      <FONT SIZE=6>Successfully unbound the Object: ", Ref, "</FONT>\n
      </TD></TR></TABLE>
      <FORM Name=goback><INPUT TYPE=\"button\" onClick=javascript:history.go(-2) VALUE=\"Go Back\">\n</FORM>"];
delete_obj(_Env, [{"node",NodeStr}, {"context", Ref}, {"action", "both"}]) ->
    Node = list_to_atom(NodeStr),
    {Obj, NS} = remote_resolve(Node, Ref),
    check(rpc:call(Node, corba, dispose, [Obj])),
    Name = check(rpc:call(Node, 'CosNaming_NamingContextExt', to_name, [NS, Ref])),
    check(rpc:call(Node, 'CosNaming_NamingContextExt', unbind, [NS, Name])),
    ["<BODY BGCOLOR=\"#FFFFFF\"> 
      <TABLE BORDER=0><TR BGCOLOR=\"#FFFFFF\"><TD ALIGN=\"center\" COLSPAN=1> 
      <FONT SIZE=6>Successfully disposed an unbound the Object: ", Ref, "</FONT>\n
      </TD></TR></TABLE>
      <FORM Name=goback><INPUT TYPE=\"button\" onClick=javascript:history.go(-2) VALUE=\"Go Back\">\n</FORM>"].
	


%%----------------------------------------------------------------------
%% Function   : nameservice
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
nameservice(_Env, [{"node",NodeStr}, {"context", "root"}]) ->
    Node = list_to_atom(NodeStr),
    is_running(Node, NodeStr),
    Object = check(rpc:call(Node, corba, resolve_initial_references, ["NameService"])),
    Prefix = "<TR><TD><A HREF=\"./nameservice?node=" ++ NodeStr ++ "&context=",
    case catch create_context_list(Node, NodeStr, Prefix, Object, "root") of
	{ok, Data} ->
	    ["<BODY BGCOLOR=\"#FFFFFF\">
              <TABLE BORDER=0><TR BGCOLOR=\"#FFFFFF\"><TD ALIGN=\"center\" COLSPAN=2> 
              <FONT SIZE=6>NameService</FONT>\n
              </TD></TR><TR BGCOLOR=\"#FFFF00\"><TD ALIGN=\"center\" COLSPAN=2> 
              <FONT SIZE=4>Root Context</FONT>\n
              </TD></TR>", Data,
              "<TR><TD><FORM Name=addctx METHOD=\"POST\" ACTION=\"./add_ctx\">
              <INPUT TYPE=\"HIDDEN\" NAME=\"node\" VALUE=\"", NodeStr, "\">
              <INPUT TYPE=\"HIDDEN\" NAME=\"context\" VALUE=\"root\">
              <INPUT TYPE=\"TEXT\" SIZE=\"20\" NAME=\"id\"></TD>
              <TD><INPUT TYPE=\"SUBMIT\" VALUE=\"New Context\"></TD></FORM></TR></TABLE>"];
	Why ->
	    orber:dbg("[~p] orber_web:nameservice(~p, root); 
Unable to create context list: ~p", [?LINE, NodeStr, Why], ?DEBUG_LEVEL),
	    throw({error, "<BODY BGCOLOR=\"#FFFFFF\">Unable to create a look up the Root Context data"})  
    end;
nameservice(_Env, [{"node",NodeStr}, {"context", Ref}]) ->
    Node = list_to_atom(NodeStr),
    {Object, _NS} = remote_resolve(Node, Ref),
    Prefix = "<TR><TD><A HREF=\"./nameservice?node=" ++ NodeStr ++ "&context="++Ref++"/",
    case catch create_context_list(Node, NodeStr, Prefix, Object, Ref) of
	{ok, Data} ->
	    ["<BODY BGCOLOR=\"#FFFFFF\">
             <TABLE BORDER=0><TR BGCOLOR=\"#FFFFFF\"><TD ALIGN=\"center\" COLSPAN=2> 
              <FONT SIZE=6>NameService</FONT></TD></TR>
              <TR BGCOLOR=\"#FFFF00\"><TD ALIGN=\"center\" COLSPAN=2> 
              <FONT SIZE=4>", Ref, "</FONT></TD></TR>", Data, 
              "<TR><TD><FORM Name=addctx METHOD=\"POST\" ACTION=\"./add_ctx\">
               <INPUT TYPE=\"HIDDEN\" NAME=\"node\" VALUE=\"", NodeStr, "\">
               <INPUT TYPE=\"HIDDEN\" NAME=\"context\" VALUE=\"", Ref, "\">
               <INPUT TYPE=\"TEXT\" SIZE=\"20\" NAME=\"id\"></TD>
               <TD><INPUT TYPE=\"SUBMIT\" VALUE=\"New Context\"></TD></FORM></TR>
	      </TABLE>
              <FORM Name=goback><INPUT TYPE=\"button\" onClick=javascript:history.go(-1) VALUE=\"Go Back\"></FORM></TD>"];
	Why ->
 	    orber:dbg("[~p] orber_web:nameservice(~p, ~p); 
Unable to create context list: ~p", [?LINE, NodeStr, Ref, Why], ?DEBUG_LEVEL),
	    throw({error, ["<BODY BGCOLOR=\"#FFFFFF\">Unable to look up the Context: ", Ref, 
			   "<BR><BR>If You just deleted it, use the 'Go Back' button next time."]})
    end;
nameservice(_Env, [{"node",NodeStr}, {"context", Ref}, {"object", Obj}]) ->
    case catch create_object_data(NodeStr, Ref, Obj) of
	{ok, Data} ->
	    Data;
	Why ->
	    orber:dbg("[~p] orber_web:nameservice(~p, ~p, ~p); 
Unable to create data for object: ~p", [?LINE, NodeStr, Ref, Obj, Why], ?DEBUG_LEVEL),
	    throw({error, ["<BODY BGCOLOR=\"#FFFFFF\">Unable to look up the Object stored as: ", Ref, 
			   "<BR><BR>If You just unbound it, use the 'Go Back' button next time."]})
    end.

create_context_list(Node, NodeStr, Prefix, Object, Ref) ->
    case check(rpc:call(Node, 'CosNaming_NamingContext', list, [Object, 100])) of
	{ok, [], BI} when Ref == "root" ->
	    catch rpc:call(Node, 'CosNaming_BindingIterator', destroy, [BI]),
	    {ok, "<TR><TD ALIGN=\"center\" COLSPAN=2><FONT SIZE=3><B>EMPTY<B></FONT></TD></TR>"};
	{ok, [], BI} ->
	    catch rpc:call(Node, 'CosNaming_BindingIterator', destroy, [BI]),
	    {ok, "<TR><TD ALIGN=\"center\"><FONT SIZE=3><B>EMPTY<B></FONT></TD>
                  <TD ALIGN=\"center\"><FORM Name=deletectx METHOD=\"POST\" ACTION=\"./delete_ctx\">
                  <INPUT TYPE=\"HIDDEN\" NAME=\"node\" VALUE=\"" ++ NodeStr ++ "\">
                  <INPUT TYPE=\"HIDDEN\" NAME=\"context\" VALUE=\"" ++ Ref ++ "\">
                  <INPUT TYPE=\"SUBMIT\" VALUE=\"Delete Context\"></FORM></TD></TR>"};
	{ok, BL, BI} when length(BL) < 100 ->
	    catch rpc:call(Node, 'CosNaming_BindingIterator', destroy, [BI]),
	    {ok, convert_contexts(BL, [], Prefix, Object, Node)};
	{ok, BL, BI} ->
	    Data = convert_contexts(BL, [], Prefix, Object, Node),
	    {ok, create_context_list_helper(Node, BI, Data, Object, Prefix)}
    end.

create_context_list_helper(Node, BI, Acc, Ctx, Prefix) ->
    case check(rpc:call(Node, 'CosNaming_BindingIterator', next_n, [BI, 100])) of
	{true, BL} ->
	    NewAcc = convert_contexts(BL, Acc, Prefix, Ctx, Node),
	    create_context_list_helper(Node, BI, NewAcc, Ctx, Prefix);
	{false, BL} ->
	    catch rpc:call(Node, 'CosNaming_BindingIterator', destroy, [BI]),
	    convert_contexts(BL, Acc, Prefix, Ctx, Node)
    end.

convert_contexts([], Acc, _Prefix, _Ctx, _Node) ->
    Acc;
convert_contexts([#'CosNaming_Binding'{binding_name = Name, 
				       binding_type = ncontext}|T], 
		 Acc, Prefix, Ctx, Node) ->
    NameStr = check(rpc:call(Node, 'CosNaming_NamingContextExt', to_string, [Ctx, Name])),
    convert_contexts(T, [Prefix, NameStr, "\" TARGET=main><B>", NameStr, "</B></A></TD><TD><B>ncontext</B></TD></TR>"|Acc],
		     Prefix, Ctx, Node);
convert_contexts([#'CosNaming_Binding'{binding_name = Name, 
				       binding_type = nobject}|T], 
		 Acc, Prefix, Ctx, Node) ->
    NameStr = check(rpc:call(Node, 'CosNaming_NamingContextExt', to_string, [Ctx, Name])),
    convert_contexts(T, [Prefix, NameStr, "&object=o \" TARGET=main><B>", NameStr, "</B></A></TD><TD><B>nobject</B></A></TD></TR>"|Acc],
		     Prefix, Ctx, Node).


create_object_data(NodeStr, Ref, _Obj) ->
    Node = list_to_atom(NodeStr),
    {Object, _NS} = remote_resolve(Node, Ref),
    LongIORStr = check(rpc:call(Node, corba, object_to_string, [Object])),
    IFRId  = check(rpc:call(Node, iop_ior, get_typeID, [Object])),
    Exists = check(rpc:call(Node, corba_object, non_existent, [Object])),
    IORStr = split_IOR(1, LongIORStr, []),
    {Data, External}
	= case rpc:call(Node, iop_ior, get_key, [Object]) of
	      {external, {Host, Port, _OK, _, _, #host_data{version = {Ma, Mi}}}} ->
		  {[{"IFR Id", IFRId},
		    {"Stored As", Ref},
		    {"External Object", "true"},
		    {"Non Existent", atom_to_list(Exists)},
		    {"Host", Host},
		    {"Port", integer_to_list(Port)},
		    {"IIOP Version", integer_to_list(Ma) ++"."++ integer_to_list(Mi)},
		    {"IOR String", IORStr}], true};
	       {'internal', _Key, _, _, _} ->
		  Pid = check(rpc:call(Node, corba, get_pid, [Object])),
		  Interface = check(rpc:call(Node, corba, request_from_iiop, 
					     [Object, oe_get_interface, false, false, false, []])),
		   InterfaceData = parse_interface(Interface, []),
		  {[{"IFR Id", IFRId},
		    {"Stored As", Ref},
		    {"External Object", "false"},
		    {"Non Existent", atom_to_list(Exists)},
		    {"Pid", pid_to_list(Pid)},
		    {"IOR String", IORStr}|InterfaceData], false};
	      {'internal_registered', {pseudo, Key}, _, _, _} ->
		  Interface = check(rpc:call(Node, corba, request_from_iiop, 
					     [Object, oe_get_interface, false, false, false, []])),
		  InterfaceData = parse_interface(Interface, []),
		  {[{"IFR Id", IFRId},
		    {"Stored As", Ref},
		    {"External Object", "false"},
		    {"Non Existent", atom_to_list(Exists)},
		    {"Pseudo Object", atom_to_list(Key)},
		    {"IOR", IORStr}|InterfaceData], false};
	      {'internal_registered', Key, _, _, _} ->
		  Pid = check(rpc:call(Node, corba, get_pid, [Object])),
		  Interface = check(rpc:call(Node, corba, request_from_iiop, 
					     [Object, oe_get_interface, false, false, false, []])),
		  InterfaceData = parse_interface(Interface, []),
		  {[{"IFR Id", IFRId},
		    {"Stored As", Ref},
		    {"External Object", "false"},
		    {"Non Existent", atom_to_list(Exists)},
		    {"Locally Registered", atom_to_list(Key)},
		    {"Pid", pid_to_list(Pid)},
		    {"IOR String", IORStr}|InterfaceData], false}
	  end,
    Buttons = case {Exists, External} of
		  {false, false} ->
		      ["<TABLE BORDER=0><TR BGCOLOR=\"#FFFFFF\"><TD ALIGN=\"center\">
 		        <TD><FORM Name=goback><INPUT TYPE=\"button\" onClick=javascript:history.go(-1) VALUE=\"Go Back\"></FORM></TD>

		        <TD ALIGN=\"center\"><FORM Name=unbindobj METHOD=\"POST\" ACTION=\"./delete_obj\">
                        <INPUT TYPE=\"HIDDEN\" NAME=\"node\" VALUE=\"", NodeStr, "\">
                        <INPUT TYPE=\"HIDDEN\" NAME=\"context\" VALUE=\"", Ref, "\">
                        <INPUT TYPE=\"HIDDEN\" NAME=\"action\" VALUE=\"unbind\">
                        <INPUT TYPE=\"SUBMIT\" VALUE=\"Unbind\"></FORM></TD>
  		        <TD ALIGN=\"center\"><FORM Name=unbinddeletobj METHOD=\"POST\" ACTION=\"./delete_obj\">
                        <INPUT TYPE=\"HIDDEN\" NAME=\"node\" VALUE=\"", NodeStr, "\">
                        <INPUT TYPE=\"HIDDEN\" NAME=\"context\" VALUE=\"", Ref, "\">
                        <INPUT TYPE=\"HIDDEN\" NAME=\"action\" VALUE=\"both\">
                        <INPUT TYPE=\"SUBMIT\" VALUE=\"Unbind & Dispose\"></FORM></TD></TR></TABLE>"];
		  _ ->
		      ["<TABLE BORDER=0><TR BGCOLOR=\"#FFFFFF\"><TD ALIGN=\"center\">
 		        <TD><FORM Name=goback><INPUT TYPE=\"button\" onClick=javascript:history.go(-1) VALUE=\"Go Back\"></FORM></TD>
		        <TD ALIGN=\"center\"><FORM Name=unbindobj METHOD=\"POST\" ACTION=\"./delete_obj\">
                        <INPUT TYPE=\"HIDDEN\" NAME=\"node\" VALUE=\"", NodeStr, "\">
                        <INPUT TYPE=\"HIDDEN\" NAME=\"context\" VALUE=\"", Ref, "\">
                        <INPUT TYPE=\"HIDDEN\" NAME=\"action\" VALUE=\"unbind\">
                        <INPUT TYPE=\"SUBMIT\" VALUE=\"Unbind\"></FORM></TD></TR></TABLE>"]
	      end,
    {ok, ["<BODY BGCOLOR=\"#FFFFFF\">",
	  simple_table("2", "NameService", [{"Key", "Value"}|Data]),
	  Buttons]}.

parse_interface([],  [{_, Op}|Acc]) ->
    [{"Operations", Op}|Acc];
parse_interface([], []) ->
    [{"Operations", "-"}];
parse_interface([{Operation,{_,Args,_}}|T], Acc) ->
    parse_interface(T, [{"", Operation ++ "/" ++ integer_to_list(length(Args))}|Acc]).


split_IOR(_, [], Acc) ->
    lists:reverse(Acc);
split_IOR(50, Str, Acc) ->
    split_IOR(1, Str, ["<BR>"|Acc]);
split_IOR(N, [H|T], Acc) ->
    split_IOR(N+1, T, [H|Acc]).



%%----------------------------------------------------------------------
%% Function   : configure
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
configure(_Env, [{"node",NodeStr}, {"data", DataStr}]) ->
    Node = list_to_atom(NodeStr),
    Data =  parse_data(DataStr),
    case catch rpc:call(Node, orber, multi_configure, [Data]) of
	ok ->
	    "<BODY BGCOLOR=\"#FFFFFF\">Configuration successfull.";
	Why ->
 	    orber:dbg("[~p] orber_web:configure(~p, ~p); 
Unable to change configuration due to: ~p", [?LINE, NodeStr, DataStr, Why], ?DEBUG_LEVEL),
	    "<BODY BGCOLOR=\"#FFFFFF\">Unable to change the configuration.<BR>
             Check the spelling and/or if it is possible to update all the keys if Orber is started."
    end.


%%----------------------------------------------------------------------
%% Function   : ifr_select
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
ifr_select(_Env, [{"node",NodeStr}]) ->
    Node = list_to_atom(NodeStr),
    is_running(Node, NodeStr),
    ["<BODY BGCOLOR=\"#FFFFFF\"> 
      <TABLE BORDER=0><TR BGCOLOR=\"#FFFFFF\"><TD ALIGN=\"center\" COLSPAN=1> 
      <FONT SIZE=6>Interface Repository</FONT>
      </TD></TR>", create_ifr_table(?IFR_DATA, NodeStr, []), "</TABLE>"].

%%----------------------------------------------------------------------
%% Function   : ifr_data
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
ifr_data(_Env, [{"node",NodeStr}, {"table", TableStr}]) ->
    Node = list_to_atom(NodeStr),
    Table =  list_to_atom(TableStr),
    WildPattern = get_wild_pattern(Table, Node),
    Records = check(rpc:call(Node, mnesia, dirty_match_object, [WildPattern])),
    Data = extract_ids(Records, []),
    ["<BODY BGCOLOR=\"#FFFFFF\">",
	simple_table("1", "Interface Repository", [TableStr|Data]),
     "<FORM Name=goback><INPUT TYPE=\"button\" onClick=javascript:history.go(-1) VALUE=\"Go Back\"></FORM>"].

extract_ids([], Acc) ->
    lists:sort(Acc);
extract_ids([#ir_ModuleDef{id=Id}|T], Acc) ->
    extract_ids(T, [Id|Acc]);
extract_ids([#ir_InterfaceDef{id=Id}|T], Acc) ->
    extract_ids(T, [Id|Acc]);
extract_ids([#ir_StructDef{id=Id}|T], Acc) ->
    extract_ids(T, [Id|Acc]);
extract_ids([#ir_UnionDef{id=Id}|T], Acc) ->
    extract_ids(T, [Id|Acc]);
extract_ids([#ir_ExceptionDef{id=Id}|T], Acc) ->
    extract_ids(T, [Id|Acc]);
extract_ids([#ir_ConstantDef{id=Id}|T], Acc) ->
    extract_ids(T, [Id|Acc]);
extract_ids([#ir_EnumDef{id=Id}|T], Acc) ->
    extract_ids(T, [Id|Acc]);
extract_ids([#ir_AliasDef{id=Id}|T], Acc) ->
    extract_ids(T, [Id|Acc]);
extract_ids([#ir_AttributeDef{id=Id}|T], Acc) ->
    extract_ids(T, [Id|Acc]);
extract_ids([#ir_OperationDef{id=Id}|T], Acc) ->
    extract_ids(T, [Id|Acc]);
extract_ids([#ir_Contained{id=Id}|T], Acc) ->
    extract_ids(T, [Id|Acc]);
extract_ids([#ir_TypedefDef{id=Id}|T], Acc) ->
    extract_ids(T, [Id|Acc]).

get_wild_pattern(ir_ModuleDef, Node) -> 
    P = check(rpc:call(Node, mnesia, table_info, [ir_ModuleDef, wild_pattern])),
    P#ir_ModuleDef{id='$1'};
get_wild_pattern(ir_InterfaceDef, Node) -> 
    P = check(rpc:call(Node, mnesia, table_info, [ir_InterfaceDef, wild_pattern])),
    P#ir_InterfaceDef{id='$1'};
get_wild_pattern(ir_StructDef, Node) -> 
    P = check(rpc:call(Node, mnesia, table_info, [ir_StructDef, wild_pattern])),
    P#ir_StructDef{id='$1'};
get_wild_pattern(ir_UnionDef, Node) -> 
    P = check(rpc:call(Node, mnesia, table_info, [ir_UnionDef, wild_pattern])),
    P#ir_UnionDef{id='$1'};
get_wild_pattern(ir_ExceptionDef, Node) -> 
    P = check(rpc:call(Node, mnesia, table_info, [ir_ExceptionDef, wild_pattern])),
    P#ir_ExceptionDef{id='$1'};
get_wild_pattern(ir_ConstantDef, Node) -> 
    P = check(rpc:call(Node, mnesia, table_info, [ir_ConstantDef, wild_pattern])),
    P#ir_ConstantDef{id='$1'};
get_wild_pattern(ir_EnumDef, Node) -> 
    P = check(rpc:call(Node, mnesia, table_info, [ir_EnumDef, wild_pattern])),
    P#ir_EnumDef{id='$1'};
get_wild_pattern(ir_AliasDef, Node) -> 
    P = check(rpc:call(Node, mnesia, table_info, [ir_AliasDef, wild_pattern])),
    P#ir_AliasDef{id='$1'};
get_wild_pattern(ir_AttributeDef, Node) ->
    P = check(rpc:call(Node, mnesia, table_info, [ir_AttributeDef, wild_pattern])),
    P#ir_AttributeDef{id='$1'};
get_wild_pattern(ir_OperationDef, Node) -> 
    P = check(rpc:call(Node, mnesia, table_info, [ir_OperationDef, wild_pattern])),
    P#ir_OperationDef{id='$1'};
get_wild_pattern(ir_Contained, Node) -> 
    P = check(rpc:call(Node, mnesia, table_info, [ir_Contained, wild_pattern])),
    P#ir_Contained{id='$1'};
get_wild_pattern(ir_TypedefDef, Node) -> 
    P = check(rpc:call(Node, mnesia, table_info, [ir_TypedefDef, wild_pattern])),
    P#ir_TypedefDef{id='$1'}.

create_ifr_table([], _Node, Result) ->
    lists:append(lists:reverse(Result));
create_ifr_table([{Table,Desc}|Rest], Node, Result) ->
    create_ifr_table(Rest, Node, 
		     ["<TR><TD><A HREF=\"./ifr_data?node=" ++ Node ++ 
		      "&table="++Table++"\" TARGET=main><B>" ++ Desc ++"</B></A></TD></TR>"|Result]).


%%----------------------------------------------------------------------
%% Function   : info
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
info(_Env, [{"node",NodeStr}]) ->
    Node = list_to_atom(NodeStr),
    is_running(Node, NodeStr),
    Data = create_info_data(?INFO_DATA, Node, []),
    ["<BODY BGCOLOR=\"#FFFFFF\">",
     simple_table("2", "Configuration", [{"Key", "Value"}|Data],
		  ["<TR><TD><FORM METHOD=\"POST\" ACTION=\"./configure\">
                    <INPUT TYPE=\"HIDDEN\" NAME=\"node\" VALUE=\"", NodeStr, "\">
                    <INPUT TYPE=\"TEXT\" SIZE=\"35\" NAME=\"data\" VALUE=\"[{Key, Value}]\">
                    </TD><TD><INPUT TYPE=\"SUBMIT\" VALUE=\"Change it\"></TD></FORM></TR>"])].


create_info_data([], _Node, Result) ->
    lists:reverse(Result);
create_info_data([{Func,Desc}|Rest], Node, Result) ->
    Data = convert_type(check(rpc:call(Node, orber, Func, []))),
    create_info_data(Rest, Node, [{Desc, Data}|Result]).

convert_type(Data) when is_integer(Data) ->
    integer_to_list(Data);
convert_type(Data) when is_atom(Data) ->
    atom_to_list(Data);
convert_type(Data) when is_float(Data) ->
    float_to_list(Data);
convert_type(Data) when is_pid(Data) ->
    pid_to_list(Data);
convert_type(Data) when is_port(Data) ->
    erlang:port_to_list(Data);
convert_type(Data) when is_tuple(Data) ->
    io_lib:write(Data);
convert_type([]) ->
    [];
convert_type(Data) when is_list(Data) ->
    case io_lib:printable_list(Data) of
	true->
	    Data;
	_->
	    io_lib:write(Data)
    end;
convert_type(_Data) ->
    [].


%%----------------------------------------------------------------------
%% Function   : menu
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
menu(_Env, Args)->    
    ["<BODY BGCOLOR=\"#FFFFFF\">", node_selections_javascripts(), node_body(Args, [node()|nodes()])].

menu_title()->
   " <TABLE WIDTH=\"100%\" BORDER=\"0\">
	<TR><TD ALIGN=\"center\"><FONT SIZE=5>Menu</FONT></TD></TR>
    </TABLE>\n".


node_body([], Nodes)->
    Node = node(),
    [node_selections_javascripts(), node_selection(Node, Nodes), menu_title(),
    menu_options(atom_to_list(Node))];
node_body([{"node",Node}|_], Nodes)->
    [node_selections_javascripts(), node_selection(list_to_atom(Node), Nodes), menu_title(),
    menu_options(Node)];
node_body([_|Rest], Nodes) ->
    node_body(Rest, Nodes).



%%----------------------------------------------------------------------
%% Function   : node_selections_javascripts
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
node_selections_javascripts()->
    "<SCRIPT>
      function node_selected()
      {                                     
         parent.frames.main.location=\"/orber/start_info.html\"
         window.location =\"./menu?node=\" + " ++ 
           "document.node_selection.nodes[document.node_selection.nodes.selectedIndex].value;
      }
      </SCRIPT>".



%%----------------------------------------------------------------------
%% Function   : node_selection
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
node_selection(Node, Nodes)->
    ["<FORM ACTION=\"./node_info\" NAME=node_selection>\n
           <TABLE WIDTH=\"100%\" BORDER=\"0\">\n
              <TR><TD ALIGN=\"center\">\n 
                  <SELECT NAME=nodes onChange=\"node_selected()\">\n",
	                  print_nodes(Node, Nodes),
                    "</SELECT>\n
              </TD></TR>\n
            </TABLE>\n
          </FORM>"].

%%----------------------------------------------------------------------
%% Function   : print_nodes
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
print_nodes(Node,Nodes)->
    print_nodes_helper([Node|lists:delete(Node,Nodes)]).

print_nodes_helper([])->
    [];
print_nodes_helper([Node|Rest])->
    NodeStr = atom_to_list(Node),
    ["<OPTION value=\"", NodeStr, "\">", NodeStr, "\n" | print_nodes_helper(Rest)].

%%----------------------------------------------------------------------
%% Function   : print_nodes
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
menu_options(Node)->
    ["<UL><LI><A HREF=\"./info?node=", Node, "\" TARGET=main><B>Configuration</B></A></UL>",
     "<UL><LI><A HREF=\"./nameservice?node=", Node, "&context=root\" TARGET=main><B>Name Service</B></A></UL>",
     "<UL><LI><A HREF=\"./ifr_select?node=", Node, "\" TARGET=main><B>IFR Types</B></A></UL>",
     "<UL><LI><A HREF=\"./create?node=", Node, "\" TARGET=main><B>Create Object</B></A></UL>",
     "<FORM Name=reload><INPUT TYPE=\"button\" onClick=\"node_selected()\" VALUE=\"Reload\">\n</FORM>",
     "<!--<A HREF=\"../../orber/application_help.html\" TARGET=main>Help</A>-->"].

%%----------------------------------------------------------------------
%%----------------- MISC Functions -------------------------------------
%%----------------------------------------------------------------------
%% Function   : simple_table
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
simple_table(Cols, Title, Data) ->
    ["<TABLE BORDER=0><TR BGCOLOR=\"#FFFFFF\"><TD ALIGN=\"center\" COLSPAN=",
     Cols, "><FONT SIZE=6>", Title, "</FONT>\n</TD></TR>", add_data(Data), "</TABLE>"].

simple_table(Cols, Title, Data, Extra) ->
    ["<TABLE BORDER=0><TR BGCOLOR=\"#FFFFFF\"><TD ALIGN=\"center\" COLSPAN=",
     Cols, "><FONT SIZE=6>", Title, "</FONT>\n</TD></TR>", add_data(Data), 
     Extra, "</TABLE>"].


% Temporarily removed to avoid a silly dialyzer warning
%add_data([]) ->
%    "";
add_data([{C1, C2, C3, C4}|T]) ->
    add_data(T, ["<TR BGCOLOR=\"#FFFF00\"><TD><B>" ++ C1 ++ "</B></TD><TD><B>" 
		 ++ C2 ++ "</B></TD><TD><B>" ++ C3 ++ "</B></TD><TD><B>"
		 ++ C4 ++ "</B></TD></TR>"]);
add_data([{C1, C2, C3}|T]) ->
    add_data(T, ["<TR BGCOLOR=\"#FFFF00\"><TD><B>" ++ C1 ++ "</B></TD><TD><B>"
		 ++ C2 ++ "</B></TD><TD><B>" ++ C3 ++ "</B></TD></TR>"]);
add_data([{C1, C2}|T]) ->
    add_data(T, ["<TR BGCOLOR=\"#FFFF00\"><TD><B>" ++ C1 ++ "</B></TD><TD><B>" 
		 ++ C2 ++ "</B></TD></TR>"]);
add_data([C1|T]) ->
    add_data(T, ["<TR BGCOLOR=\"#FFFF00\"><TD><B>" ++ C1 ++ "</B></TD></TR>"]).


add_data([], Acc) ->
    lists:reverse(Acc);
add_data([{C1, C2, C3, C4}|T], Acc) ->
    add_data(T, ["<TR><TD><B>"++C1++"</B></TD><TD>"++C2++"</TD><TD>"
		 ++C3++"</TD><TD>"++C4++"</TD></TR>"|Acc]);
add_data([{C1, C2, C3}|T], Acc) ->
    add_data(T, ["<TR><TD><B>"++C1++"</B></TD><TD>"++C2++"</TD><TD>"
		 ++C3++"</TD></TR>"|Acc]);
add_data([{C1, C2}|T], Acc) ->
    add_data(T, ["<TR><TD><B>"++C1++"</B></TD><TD>"++C2++"</TD></TR>"|Acc]);
add_data([C1|T], Acc) ->
    add_data(T, ["<TR><TD>"++C1++"</TD></TR>"|Acc]).

%%----------------------------------------------------------------------
%% Function   : check
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
check(Data) ->
    check(Data, "").

check({badrpc, {'EXCEPTION', E}}, Comment) ->
    EList = atom_to_list(element(1, E)),
    throw({error, ["<BODY BGCOLOR=\"#FFFFFF\">Got the exception: ", EList, "<BR><BR>", Comment]});
check({badrpc,{'EXIT',{undef,_}}}, Comment) ->
    throw({error, ["<BODY BGCOLOR=\"#FFFFFF\">Tried to invoke undefined module or operation.<BR><BR>", Comment]});    
check({badrpc,nodedown}, Comment) ->
    throw({error, ["<BODY BGCOLOR=\"#FFFFFF\">Node down - unable to complete the requested operation.<BR><BR>", Comment]});    
check({badrpc, {'EXIT', _R}}, Comment) ->
    throw({error, ["<BODY BGCOLOR=\"#FFFFFF\">Invoking the requested operation resulted in an EXIT.<BR><BR>", Comment]});
check({badrpc, {'EXIT', _R1, _R2}}, Comment) ->
    throw({error, ["<BODY BGCOLOR=\"#FFFFFF\">Invoking the requested operation resulted in an EXIT.<BR><BR>", Comment]});
check({'EXCEPTION', E}, Comment) ->
    EList = atom_to_list(element(1, E)),
    throw({error, ["<BODY BGCOLOR=\"#FFFFFF\">Got the exception: ", EList, "<BR><BR>", Comment]});
check({'EXIT',{undef,_}}, Comment) ->
    throw({error, ["<BODY BGCOLOR=\"#FFFFFF\">Tried to invoke operation using undefined module or operation.<BR><BR>", Comment]});    
check({'EXIT', _R}, Comment) ->
    throw({error, ["<BODY BGCOLOR=\"#FFFFFF\">Invoking the requested operation resulted in an EXIT.<BR><BR>", Comment]});
check({'EXIT', _R1, _R2}, Comment) ->
    throw({error, ["<BODY BGCOLOR=\"#FFFFFF\">Invoking the requested operation resulted in an EXIT.<BR><BR>", Comment]});
check(Reply, _) ->
    Reply.


%%----------------------------------------------------------------------
%% Function   : is_running
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
is_running(Node, NodeStr) ->
    case rpc:call(Node, application, which_applications, []) of
	{badrpc, _} ->
	    throw(["<BODY BGCOLOR=\"#FFFFFF\"> 
                    <TABLE BORDER=0><TR BGCOLOR=\"#FFFFFF\"><TD ALIGN=\"center\" COLSPAN=1> 
                    <FONT SIZE=6>Orber not started on node: ", NodeStr, "</FONT>
                    </TD></TR></TABLE>"]);
	Apps ->
	    is_running2(Apps, NodeStr)
    end.

is_running2([], NodeStr) ->
    throw(["<BODY BGCOLOR=\"#FFFFFF\"> 
            <TABLE BORDER=0><TR BGCOLOR=\"#FFFFFF\"><TD ALIGN=\"center\" COLSPAN=1> 
            <FONT SIZE=6>Orber not started on node: ", NodeStr, "</FONT>
            </TD></TR></TABLE>"]);
is_running2([{orber, _, _} |_], _) ->
     true;
is_running2([_ |As], NodeStr) ->
    is_running2(As, NodeStr).


%%----------------------------------------------------------------------
%% Function   : parse_data
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
parse_data([])->
    [];
parse_data(Options)->
    case erl_scan:string(Options ++ ".") of
	{ok,Tokens,_Line} ->
	    case erl_parse:parse_term(Tokens) of
		{ok,X}->
		    X;
		Why ->
		    orber:dbg("[~p] orber_web:parse_data(~p);
erl_parse:parse_term failed.
Malformed data: ~p", [?LINE, Options, Why], ?DEBUG_LEVEL),
		    throw({error, ["<BODY BGCOLOR=\"#FFFFFF\">Unable to parse supplied data: ", 
			   Options]})
	    end;
	Why ->
	    orber:dbg("[~p] orber_web:parse_data(~p); 
erl_scan:string failed.
Malformed data: ~p", [?LINE, Options, Why], ?DEBUG_LEVEL),
	    throw({error, ["<BODY BGCOLOR=\"#FFFFFF\">Unable to parse supplied data: ", Options]})
    end.

%%----------------------------------------------------------------------
%% Function   : remote_resolve
%% Returns    : 
%% Description: 
%%----------------------------------------------------------------------
remote_resolve(Node, Ref) ->
    NS = check(rpc:call(Node, corba, resolve_initial_references, ["NameService"]),
	       "Failed to resolve initial refrence (NameService)"),
    case rpc:call(Node, 'CosNaming_NamingContextExt', resolve_str, [NS, Ref]) of
	{'EXCEPTION', E} when is_record(E, 'CosNaming_NamingContext_NotFound') ->
	    throw({ok, ["<BODY BGCOLOR=\"#FFFFFF\">Unable to look up the Object: ", Ref,
			"<BR><BR>Reason: CosNaming_NamingContext_NotFound",
			"<BR><BR>If You just deleted it, use the 'Go Back' button next time."]});
	{'EXCEPTION', E} when is_record(E, 'CosNaming_NamingContext_CannotProceed') ->
	    throw({ok, ["<BODY BGCOLOR=\"#FFFFFF\">Unable to look up the Object: ", Ref,
			"<BR><BR>Reason: CosNaming_NamingContext_CannotProceed",
			"<BR><BR>If You just deleted it, use the 'Go Back' button next time."]});
	{badrpc, {'EXCEPTION', E}} when is_record(E, 'CosNaming_NamingContext_NotFound') ->
	    throw({ok, ["<BODY BGCOLOR=\"#FFFFFF\">Unable to look up the Object: ", Ref,
			"<BR><BR>Reason: CosNaming_NamingContext_NotFound",
			"<BR><BR>If You just deleted it, use the 'Go Back' button next time."]});
	{badrpc, {'EXCEPTION', E}} when is_record(E, 'CosNaming_NamingContext_CannotProceed') ->
	    throw({ok, ["<BODY BGCOLOR=\"#FFFFFF\">Unable to look up the Object: ", Ref,
			"<BR><BR>Reason: CosNaming_NamingContext_CannotProceed",
			"<BR><BR>If You just deleted it, use the 'Go Back' button next time."]});
	FoundObj ->
		{FoundObj, NS}
	end.
 


%%----------------------------------------------------------------------
%%                           END OF MODULE
%%----------------------------------------------------------------------
