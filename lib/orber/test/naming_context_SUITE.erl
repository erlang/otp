%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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
%%
%%-----------------------------------------------------------------
%% 
%% Description:
%% Test suite for Name service
%%
%%-----------------------------------------------------------------
-module(naming_context_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/include/corba.hrl").

-define(default_timeout, ?t:minutes(5)).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------

-export([name_context/1, check_list/1, name_context_ext/1]).

-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, 
	 end_per_testcase/2]).


%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(REMAP_EXCEPT(F), case catch F of
			     {'EXCEPTION', E} -> exit(E);
			     {'EXIT', E} -> exit(E);
			     R -> R
			 end).

-define(match(ExpectedRes, Expr),
        fun() ->
                AcTuAlReS = (catch (Expr)),
                case AcTuAlReS of
                    ExpectedRes ->
                        io:format("------ CORRECT RESULT ------~n~p~n",
                                  [AcTuAlReS]),
                        AcTuAlReS;
                    _ ->
                        io:format("###### ERROR ERROR ######~n~p~n",
                                  [AcTuAlReS]),
                        ?line exit(AcTuAlReS)
                end
        end()).
 
%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    cases().

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


cases() -> 
    [name_context, check_list, name_context_ext].

%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------

init_per_testcase(_Case, Config) ->
    Path = code:which(?MODULE),
    code:add_pathz(filename:join(filename:dirname(Path), "idl_output")),
    ?line Dog=test_server:timetrap(?default_timeout),
    orber:jump_start(0),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    Path = code:which(?MODULE),
    code:del_path(filename:join(filename:dirname(Path), "idl_output")),
    orber:jump_stop(),
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

%%-----------------------------------------------------------------
%% Test Case: name handling tests
%% Description: 
%%-----------------------------------------------------------------
name_context(doc) -> ["Description", "more description"];
name_context(suite) -> [];
name_context(_) ->
    ?REMAP_EXCEPT(name_context_run()).

name_context_run() ->
    ?line Ns = corba:resolve_initial_references("NameService"),

    ?match({'EXCEPTION', #'NO_PERMISSION'{}},
	   'CosNaming_NamingContextExt':destroy(Ns)),

    %% Create a test context.
    ?line Tc = 'CosNaming_NamingContext':bind_new_context(Ns,
				[#'CosNaming_NameComponent'{id="testcontext",
							    kind=""}]),
    %% Start testing
    ?line 'CosNaming_NamingContext':bind(Tc, [#'CosNaming_NameComponent'
					      {id="hej",
					       kind=""}], Ns),
    ?line Ns = 'CosNaming_NamingContext':resolve(Tc,
				       [#'CosNaming_NameComponent'{id="hej",
								   kind=""}]),
    ?line Nc = 'CosNaming_NamingContext':new_context(Tc),
    ?line 'CosNaming_NamingContext':bind(Tc, [#'CosNaming_NameComponent'
					      {id="stop",
					       kind=""}], Nc),
    ?line Nc = 'CosNaming_NamingContext':resolve(Tc,
				       [#'CosNaming_NameComponent'{id="stop",
								   kind=""}]),
    ?line {'EXCEPTION', E0} =
	(catch 'CosNaming_NamingContext':bind(Tc,
			          [#'CosNaming_NameComponent'{id="stop",
							      kind=""}], Ns)),
    ?line ok = 'CosNaming_NamingContext':rebind(Tc,
			          [#'CosNaming_NameComponent'{id="stop",
							      kind=""}], Ns),
    ?line {'CosNaming_NamingContext_AlreadyBound', _} = E0,
    ?line 'CosNaming_NamingContext':bind_context(Tc,
				   [#'CosNaming_NameComponent'{id="evaluate",
							       kind=""}], Nc),
    ?line Nc =
	'CosNaming_NamingContext':resolve(Tc,
			      [#'CosNaming_NameComponent'{id="evaluate",
							  kind=""}]),
    ?line 'CosNaming_NamingContext':bind(Tc,
			      [#'CosNaming_NameComponent'{id="evaluate",
							  kind=""},
			       #'CosNaming_NameComponent'{id="hej",
							  kind=""}], Ns),
    ?line ok = 'CosNaming_NamingContext':rebind(Tc,
			      [#'CosNaming_NameComponent'{id="evaluate",
							  kind=""},
			       #'CosNaming_NameComponent'{id="hej",
							  kind=""}], Ns),
    ?line Ns = 'CosNaming_NamingContext':resolve(Tc,
			      [#'CosNaming_NameComponent'{id="evaluate",
							  kind=""},
			       #'CosNaming_NameComponent'{id="hej",
							  kind=""}]),
    ?line {'EXCEPTION', E1} =
	(catch 'CosNaming_NamingContext':resolve(Tc,
			       [#'CosNaming_NameComponent'{id="stop",
							   kind=""},
				#'CosNaming_NameComponent'{id="hej",
							   kind=""}])),
    ?line ?match(ok, orber_diagnostics:nameservice()),

    ?line {'CosNaming_NamingContext_CannotProceed', _,_,_} = E1,
    ?line {'EXCEPTION', E2} = (catch 'CosNaming_NamingContext':destroy(Nc)),
    ?line {'CosNaming_NamingContext_NotEmpty', _} = E2,
    ?line ok = 'CosNaming_NamingContext':unbind(Tc,
				[#'CosNaming_NameComponent'{id="evaluate",
							    kind=""},
				 #'CosNaming_NameComponent'{id="hej",
							    kind=""}]),
    ?line ok = 'CosNaming_NamingContext':destroy(Nc),
    ?line ok = 'CosNaming_NamingContext':unbind(Tc,
				 [#'CosNaming_NameComponent'{id="evaluate",
							     kind=""}]),
    ?line ok = 'CosNaming_NamingContext':unbind(Tc,
				 [#'CosNaming_NameComponent'{id="stop",
							     kind=""}]),
    ?line ok = 'CosNaming_NamingContext':unbind(Tc,
				 [#'CosNaming_NameComponent'{id="hej",
							     kind=""}]),
    ?line case 'CosNaming_NamingContext':list(Tc, 3) of
	      {ok, [], ?ORBER_NIL_OBJREF} ->
		  ok;
	      _ ->
		  exit(not_empty)
	  end,
    ?line ok = 'CosNaming_NamingContext':unbind(Ns,
			      [#'CosNaming_NameComponent'{id="testcontext",
							  kind=""}]),
    ?line ok = 'CosNaming_NamingContext':destroy(Tc),
    ok.



check_list(doc) -> 
    ["Check that the CosNaming::NamingContext::list()", 
     "returns ok.",
     "Own Id: OTP-2023"];
check_list(suite) -> [];
check_list(Config) when is_list(Config) ->
    ?REMAP_EXCEPT(check_list_run(Config)).

check_list_run(_Config) ->
    create_default_contexts(),
    ?line Ns = corba:resolve_initial_references("NameService"),
    ?line {_, BL, _} = ?match({ok, _, ?ORBER_NIL_OBJREF}, 
			      'CosNaming_NamingContext':list(Ns, 256)),
    
    FF = fun(X) -> XX = hd(X#'CosNaming_Binding'.binding_name),
		   XX#'CosNaming_NameComponent'.id end,
		  
    L = lists:sort(lists:map(FF, BL)),
    ?line ["host", "workgroup"] = L,
    
    %% Test next_n/2
    ?line {_, _, BI} = ?match({ok, [], _BI}, 'CosNaming_NamingContext':list(Ns, 0)),
    ?line ?match({true, []}, 'CosNaming_BindingIterator':next_n(BI, 0)),
    ?line ?match({true, [_]}, 'CosNaming_BindingIterator':next_n(BI, 1)),
    ?line ?match({false, [_]}, 'CosNaming_BindingIterator':next_n(BI, 1)),
    ?line ?match({false, []}, 'CosNaming_BindingIterator':next_n(BI, 1)),
    ?line ?match(ok, 'CosNaming_BindingIterator':destroy(BI)),

    ?line {_, _, BI2} = ?match({ok, [], _BI2}, 'CosNaming_NamingContext':list(Ns, 0)),
    ?line ?match({true, _}, 'CosNaming_BindingIterator':next_one(BI2)),
    ?line ?match({true, _}, 'CosNaming_BindingIterator':next_one(BI2)),
    ?line ?match({false, _}, 'CosNaming_BindingIterator':next_one(BI2)),
    ?line ?match(ok, 'CosNaming_BindingIterator':destroy(BI2)),
    ?line ?match(ok, orber_diagnostics:nameservice()),
    ok.

create_default_contexts() ->
    HostComponent = lname_component:set_id(lname_component:create(),
					   "host"),
    HostsComponent = lname_component:set_id(lname_component:create(),
					    "hosts"),
    ResourcesComponent = lname_component:set_id(lname_component:create(),
						"resources"),
    DevelopmentComponent = lname_component:set_id(lname_component:create(),
					   "development"),
    FactoriesComponent = lname_component:set_id(lname_component:create(),
					   "factories"),
    WGComponent = lname_component:set_id(lname_component:create(),
					   "workgroup"),
    %% Creation of Naming Context host and it's subcontexts
    NS = corba:resolve_initial_references("NameService"),
    H = 'CosNaming_NamingContext':bind_new_context(NS,
		lname:insert_component(lname:create(), 1, HostComponent)),
    HR = 'CosNaming_NamingContext':bind_new_context(H,
		lname:insert_component(lname:create(), 1, ResourcesComponent)),
    'CosNaming_NamingContext':bind_new_context(HR,
		lname:insert_component(lname:create(), 1, FactoriesComponent)),
    HD = 'CosNaming_NamingContext':bind_new_context(H,
	lname:insert_component(lname:create(), 1, DevelopmentComponent)),
    HDR = 'CosNaming_NamingContext':bind_new_context(HD,
		lname:insert_component(lname:create(), 1, ResourcesComponent)),
    'CosNaming_NamingContext':bind_new_context(HDR,
		lname:insert_component(lname:create(), 1, FactoriesComponent)),
      %% Creation of Naming Context workgroup and it's subcontexts
    W = 'CosNaming_NamingContext':bind_new_context(NS,
		lname:insert_component(lname:create(), 1, WGComponent)),
    'CosNaming_NamingContext':bind_new_context(W,
		lname:insert_component(lname:create(), 1, HostsComponent)),
    WR = 'CosNaming_NamingContext':bind_new_context(W,
		lname:insert_component(lname:create(), 1, ResourcesComponent)),
    'CosNaming_NamingContext':bind_new_context(WR,
		lname:insert_component(lname:create(), 1, FactoriesComponent)),
    WD = 'CosNaming_NamingContext':bind_new_context(W,
	lname:insert_component(lname:create(), 1, DevelopmentComponent)),
    WDR = 'CosNaming_NamingContext':bind_new_context(WD,
		lname:insert_component(lname:create(), 1, ResourcesComponent)),
    'CosNaming_NamingContext':bind_new_context(WDR,
		lname:insert_component(lname:create(), 1, FactoriesComponent)),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 
%% Description: 
%%-----------------------------------------------------------------
name_context_ext(doc) -> ["Description", "more description"];
name_context_ext(suite) -> [];
name_context_ext(_Config) ->
    ?REMAP_EXCEPT(name_context_ext_run()).

name_context_ext_run() ->
    ?line NS = ?match({_,pseudo,_, _,_, _},
		      corba:resolve_initial_references("NameService")),
    
    Name1 = [#'CosNaming_NameComponent'{id="\\<id1\\>", kind="kind1"},
	     #'CosNaming_NameComponent'{id="id2", kind="kind2"}],
    String1 = "\\<id1\\>.kind1/id2.kind2",
    Name2 = [#'CosNaming_NameComponent'{id="id1", kind=""},
	     #'CosNaming_NameComponent'{id="id2", kind=""},
	     #'CosNaming_NameComponent'{id="id3", kind=""}],
    String2 = "id1/id2/id3",
    Name3 = [#'CosNaming_NameComponent'{id="id1", kind="kind1"},
	     #'CosNaming_NameComponent'{id="", kind=""},
	     #'CosNaming_NameComponent'{id="id3", kind="kind3"}],
    String3 = "id1.kind1/./id3.kind3",
    Name4 = [#'CosNaming_NameComponent'{id="id1", kind="kind1"},
	     #'CosNaming_NameComponent'{id="i.d.2", kind="kind2"},
	     #'CosNaming_NameComponent'{id="id3", kind="kind3"}],
    String4 = "id1.kind1/i\\.d\\.2.kind2/id3.kind3",
    Name5 = [#'CosNaming_NameComponent'{id="id1", kind=""},
	     #'CosNaming_NameComponent'{id="i/d/2", kind="kind2"},
	     #'CosNaming_NameComponent'{id="id3", kind=""}],
    String5 = "id1/i\\/d\\/2.kind2/id3",

    BadString1 = "id1./id2/id3",
    BadString2 = "id1//id3",

    ?match(String1, 'CosNaming_NamingContextExt':to_string(NS, Name1)),
    ?match(String2, 'CosNaming_NamingContextExt':to_string(NS, Name2)),
    ?match(String3, 'CosNaming_NamingContextExt':to_string(NS, Name3)),
    ?match(String4, 'CosNaming_NamingContextExt':to_string(NS, Name4)),
    ?match(String5, 'CosNaming_NamingContextExt':to_string(NS, Name5)),
    ?match(Name1, 'CosNaming_NamingContextExt':to_name(NS, String1)),
    ?match(Name2, 'CosNaming_NamingContextExt':to_name(NS, String2)),
    ?match(Name3, 'CosNaming_NamingContextExt':to_name(NS, String3)),
    ?match(Name4, 'CosNaming_NamingContextExt':to_name(NS, String4)),
    ?match(Name5, 'CosNaming_NamingContextExt':to_name(NS, String5)),

    ?match({'EXCEPTION', {'CosNaming_NamingContext_InvalidName',_}}, 
	   'CosNaming_NamingContextExt':to_name(NS, BadString1)),
    ?match({'EXCEPTION', {'CosNaming_NamingContext_InvalidName',_}}, 
	   'CosNaming_NamingContextExt':to_name(NS, BadString2)),

    %% Create a test context.
    ?line Tc = ?match({_,pseudo,_, _,_, _},
		      'CosNaming_NamingContext':bind_new_context(NS,
				[#'CosNaming_NameComponent'{id="testcontext",
							    kind=""}])),
    ?match(ok, 'CosNaming_NamingContext':bind(Tc, [#'CosNaming_NameComponent'
						   {id="hej",
						    kind=""}], NS)),

    ?match(NS, 'CosNaming_NamingContextExt':resolve_str(Tc, "hej")),

    ?match("corbaloc:rir:", 'CosNaming_NamingContextExt':to_url(Tc, "rir:", "")),
    ?match("corbaname:rir:/NameService#org/erlang/",
	   'CosNaming_NamingContextExt':to_url(Tc, "rir:/NameService", "org/erlang/")),
    ?match("corbaloc::1.1@555%3cxyz.com:9999/Dev/NameService",
	   'CosNaming_NamingContextExt':to_url(Tc, ":1.1@555\\<xyz.com:9999/Dev/NameService", "")),

    %% Bad port
    ?match({'EXCEPTION', {'CosNaming_NamingContextExt_InvalidAddress',_}}, 
	   'CosNaming_NamingContextExt':to_url(Tc, ":1.1@555xyz.com:99a9/", "")),
    %% BAd IIOP-version
    ?match({'EXCEPTION', {'CosNaming_NamingContextExt_InvalidAddress',_}}, 
	   'CosNaming_NamingContextExt':to_url(Tc, ":1@555xyz.com:99a9/", "")),
    %% Bad IIOP-version
    ?match({'EXCEPTION', {'CosNaming_NamingContextExt_InvalidAddress',_}}, 
	   'CosNaming_NamingContextExt':to_url(Tc, ":@555xyz.com:99a9/", "")),
    %% Bad protocol
    ?match({'EXCEPTION', {'CosNaming_NamingContextExt_InvalidAddress',_}}, 
	   'CosNaming_NamingContextExt':to_url(Tc, "iop:@555xyz.com:99a9/", "")),
    %% Unsupported protocol
    ?match({'EXCEPTION', {'CosNaming_NamingContextExt_InvalidAddress',_}}, 
	   'CosNaming_NamingContextExt':to_url(Tc, "atm:@555xyz.com:9999/", "")),
    %% Bad Name
    ?match({'EXCEPTION', {'CosNaming_NamingContext_InvalidName',_}}, 
	   'CosNaming_NamingContextExt':to_url(Tc, ":555xyz.com:9999/", "id1./id2.kind2")),

    ok.


