%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: orber_cosnaming_utils.erl
%% Modified:
%%
%%-----------------------------------------------------------------
-module(orber_cosnaming_utils).

-include("orber_cosnaming.hrl").
-include("CosNaming.hrl").
-include("CosNaming_NamingContext.hrl").
-include("CosNaming_NamingContextExt.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([query_result/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([addresses/1, name/1, 
	 check_addresses/1, check_name/1, 
	 key/1, select_type/1, lookup/1, lookup/2,
	 escape_string/1, unescape_string/1,
	 name2string/1, string2name/1]).

%%-----------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Defines
%%-----------------------------------------------------------------
%% DEFAULT VALUES:
%%
%% IIOP:
%%  - port:         2809
%%  - iiop version: 1.0
-define(DEF_VERS,      {1,0}).
-define(DEF_PORT,      2809).
-define(DEF_KEY,       "NameService").
-define(HTTP_DEF_PORT, 80).

%% DEBUG INFO
-define(DEBUG_LEVEL, 5).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%% Check a read transaction
query_result({atomic, Qres}) -> 
    case Qres of
	[Hres] ->
	    Hres#orber_CosNaming.nameindex;
	[Hres|Tres] ->
	    orber:dbg("[~p] orber_cosnaming_utils:query_result(~p);~n"
		      "Multiple Hits: ~p", [?LINE, Qres, [Hres|Tres]], ?DEBUG_LEVEL),
	    error;
	[] ->
	    orber:dbg("[~p] orber_cosnaming_utils:query_result();~n"
		      "No hit", [?LINE], ?DEBUG_LEVEL),
	    error;
	Other ->
	    orber:dbg("[~p] orber_cosnaming_utils:query_result(~p);~n"
		      "Mnesia Access Failed ~p", [?LINE, Qres, Other], ?DEBUG_LEVEL),
	    error
    end;
query_result({aborted, Qres}) -> 
    orber:dbg("[~p] orber_cosnaming_utils:query_result(~p);~n"
		      "Mnesia Access Aborted", [?LINE, Qres], ?DEBUG_LEVEL),
    error;
query_result(What) -> 
    orber:dbg("[~p] orber_cosnaming_utils:query_result(~p);~n"
		      "Mnesia Access Failed", [?LINE, What], ?DEBUG_LEVEL),
    error.


%%----------------------------------------------------------------------
%% Function   : check_addresses
%% Arguments  : 
%% Description: 
%% Returns    : 
%%----------------------------------------------------------------------
check_addresses(Str) ->
    {_, Rest2} = addresses(Str),
    case key(Rest2) of
	{_, []} ->
	    ok;
	What ->
	    orber:dbg("[~p] orber_cosnaming_utils:check_addresses(~p);~n"
		      "Key ~p", [?LINE, Str, What], ?DEBUG_LEVEL),
	    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
    end.

%%----------------------------------------------------------------------
%% Function   : check_name
%% Arguments  : 
%% Description: 
%% Returns    : 
%%----------------------------------------------------------------------
check_name(Str) ->
    name(Str).

%%----------------------------------------------------------------------
%% Function   : select_type
%% Arguments  : A corbaloc/corbaname-string.
%% Description: 
%% Returns    : A tuple which contain data about what connection we want to use |
%%              {'EXCEPTION', #'CosNaming_NamingContextExt_InvalidAddress'{}}
%%----------------------------------------------------------------------
select_type([$c, $o, $r, $b, $a, $l, $o, $c, $:|Rest1]) ->
    {Addresses, Rest2} = addresses(Rest1),
    case key(Rest2) of
	{Key, []} ->
	    {corbaloc, Addresses, Key};
	What ->
	    orber:dbg("[~p] orber_cosnaming_utils:select_type(~p);~n"
		      "Key ~p", [?LINE, Rest1, What], ?DEBUG_LEVEL),
	    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
    end;
select_type([$c, $o, $r, $b, $a, $n, $a, $m, $e, $:|Rest1]) ->
    {Addresses, Rest2} = addresses(Rest1),
    {Key, Rest3} = key(Rest2),
    Name = name(Rest3),
    {corbaname, Addresses, Key, string2name(Name)};

select_type([$f, $i, $l, $e, $:, $/ |Rest]) ->
    file(Rest);
select_type([$f, $t, $p, $:, $/, $/ |Rest]) ->
    ftp(Rest);
select_type([$h, $t, $t, $p, $:, $/, $/ |Rest]) ->
    http(Rest);

select_type(What) ->
    orber:dbg("[~p] orber_cosnaming_utils:select_type(~p);~n"
		      "Malformed or unsupported type.", 
	      [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{}).


%%----------------------------------------------------------------------
%% Function   : addresses
%% Arguments  : A corbaloc string.
%% Description: 
%% Returns    : A list of addresses an the remaining part possibly containg
%%              a Key and a stringified Name
%%----------------------------------------------------------------------
addresses(Str) ->
    addresses(address(protocol, Str, [], []), []).

addresses({false, rir, Rest}, []) ->
    {rir, Rest};
addresses({false, Adr, Rest}, Addresses) ->
    {lists:reverse([Adr|Addresses]), Rest};
addresses({true, Adr, Rest}, Addresses) ->
    addresses(address(protocol, Rest, [], []), [Adr|Addresses]).
%% Which protocol.
address(protocol, [$:|T], [], []) ->
    address(version, T, [], [iiop]);
address(protocol, [$i, $i, $o, $p, $:|T], [], []) ->
    address(version, T, [], [iiop]);
address(protocol, [$s,$s,$l, $i, $o, $p, $:|T], [], []) ->
    address(version, T, [], [ssliop]);
address(protocol, [$r, $i, $r, $:|T], [], []) ->
    {false, rir, T};
address(protocol, What, _, _) ->
    orber:dbg("[~p] orber_cosnaming_utils:address(~p);~n"
		      "Malformed or unsupported protocol.", 
	      [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{});


%% Parsed one address, no version found or port found.
address(version, [$,|T], Acc, Previous) ->
    {true, lists:reverse([?DEF_PORT, lists:reverse(Acc), ?DEF_VERS|Previous]), T};
address(version, [$/|T], Acc, Previous) ->
    {false, lists:reverse([?DEF_PORT, lists:reverse(Acc), ?DEF_VERS|Previous]), T};
%% Found iiop version.
address(version, [$@|T], Acc, Previous) ->
    case Acc of
	[Minor, $., Major] ->
	    address(address, T, [], [{Major-$0, Minor-$0}|Previous]);
	What ->
	    orber:dbg("[~p] orber_cosnaming_utils:address(~p);~n"
		      "Malformed or unsupported version.", 
		      [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
    end;

%% Found no iiop version, switch to ipv6.
address(version, [$[|T], [], Previous) ->
    address(ipv6, T, [], [?DEF_VERS|Previous]);
    
%% Found no iiop version, switch to port. In this case Acc contains the
%% Host.
address(version, [$:|T], Acc, Previous) ->
    address(port, T, [], [lists:reverse(Acc), ?DEF_VERS|Previous]);

%% Found IPv6
address(address, [$[|T], [], Previous) ->
    address(ipv6, T, [], Previous);

%% Found port
address(address, [$:|T], Acc, Previous) ->
    address(port, T, [], [lists:reverse(Acc)|Previous]);
	
address(ipv6, [$]|T], Acc, Previous) ->
    address(address, T, Acc, Previous);

%% Parsed one address, port not found.
address(address, [$,|T], [], Previous) ->
    {true, lists:reverse([?DEF_PORT|Previous]), T};
address(address, [$/|T], [], Previous) ->
    {false, lists:reverse([?DEF_PORT|Previous]), T};
address(address, [$,|T], Acc, Previous) ->
    {true, lists:reverse([?DEF_PORT, lists:reverse(Acc)|Previous]), T};
address(address, [$/|T], Acc, Previous) ->
    {false, lists:reverse([?DEF_PORT, lists:reverse(Acc)|Previous]), T};

%% Parsed one address.
address(port, [$/|T], Acc, Previous) ->
    case catch list_to_integer(lists:reverse(Acc)) of
	Port when is_integer(Port) ->
	    {false, lists:reverse([Port|Previous]), T};
	What ->
	    orber:dbg("[~p] orber_cosnaming_utils:address(~p);~n"
		      "Malformed port.", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
    end;
address(port, [$,|T], Acc, Previous) ->
    case catch list_to_integer(lists:reverse(Acc)) of
	Port when is_integer(Port) ->
	    {true, lists:reverse([Port|Previous]), T};
	What ->
	    orber:dbg("[~p] orber_cosnaming_utils:address(~p);~n"
		      "Malformed port.", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
    end;

%% EOS, check how far we have reached so far and add necessary default values.
address(version, [], Acc, Previous) ->
    {false, lists:reverse([?DEF_PORT, lists:reverse(Acc), ?DEF_VERS|Previous]), []};
address(port, [], [], Previous) ->
    {false, lists:reverse([?DEF_PORT|Previous]), []};
address(port, [], Acc, Previous) ->
    case catch list_to_integer(lists:reverse(Acc)) of
	Port when is_integer(Port) ->
	    {false, lists:reverse([Port|Previous]), []};
	What ->
	    orber:dbg("[~p] orber_cosnaming_utils:address(~p);~n"
		      "Malformed port.", [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
    end;
address(address, [], [], Previous) ->
    {false, lists:reverse([?DEF_PORT|Previous]), []};
address(address, [], Acc, Previous) ->
    {false, lists:reverse([?DEF_PORT, lists:reverse(Acc)|Previous]), []};
    
address(Type, [H|T], Acc, Previous) ->
    address(Type, T, [H|Acc], Previous).

%%----------------------------------------------------------------------
%% Function   : key
%% Arguments  : A string which contain a Key we want to use and, if defined,
%%              stringified NameComponent sequence.
%% Description: 
%% Returns    : The Key and the remaining part, i.e., a stringified 
%%              NameComponent sequence.
%%----------------------------------------------------------------------
key(Str) ->
    key(Str, []).
key([], []) ->
    {?DEF_KEY, []};
key([], Acc) ->
    {lists:reverse(Acc), []};
key([$#|T], []) ->
    {?DEF_KEY, T};
key([$#|T], Acc) ->
    {lists:reverse(Acc), T};
key([$/|T], []) ->
    key(T, []);
key([H|T], Acc) ->
    key(T, [H|Acc]).

%%----------------------------------------------------------------------
%% Function   : name
%% Arguments  : A string describing a NameComponent sequence.
%% Description: 
%% Returns    : The input string |
%%              {'EXCEPTION', #'CosNaming_NamingContext_InvalidName'{}}
%%----------------------------------------------------------------------
name(Str) ->
    name(Str, []).
name([], Acc) ->
    lists:reverse(Acc);
name([$., $/|_T], _) ->
    corba:raise(#'CosNaming_NamingContext_InvalidName'{});
name([$/, $/|_T], _) ->
    corba:raise(#'CosNaming_NamingContext_InvalidName'{});
name([$/|T], []) ->
    name(T, []);
name([H|T], Acc) ->
    name(T, [H|Acc]).


%%----------------------------------------------------------------------
%% Function   : file
%% Arguments  : A string describing connection parameters.
%% Description: 
%% Returns    : A tuple consisting of data extracted from the given string.
%%----------------------------------------------------------------------
file(File) ->
    {file, File}.

%%----------------------------------------------------------------------
%% Function   : ftp
%% Arguments  : A string describing connection parameters.
%% Description: 
%% Returns    : A tuple consisting of data extracted from the given string.
%%----------------------------------------------------------------------
ftp(Address) ->
    %% Perhaps we should run some checks here?
    {ftp, Address}.

%%----------------------------------------------------------------------
%% Function   : http
%% Arguments  : A string describing connection parameters.
%% Description: 
%% Returns    : A tuple consisting of data extracted from the given string.
%%----------------------------------------------------------------------
http(Address) ->
    case string:tokens(Address, ":") of
	[Host, Rest] ->
	    %% At his stage we know that address contains a Port number.
	    {Port, Key} = split_to_slash(Rest, []),
	    case catch list_to_integer(Port) of
		PortInt when is_integer(PortInt) ->
		    {http, Host, PortInt, Key};
		_ ->
		    orber:dbg("[~p] orber_cosnaming_utils:http(~p);~n"
			      "Malformed key; should be http://Host:Port/path/name.html~n"
			      "or http://Host/path/name.html", 
			      [?LINE, Address], ?DEBUG_LEVEL),
		    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
	    end;
	[Address] ->
	    %% Use default port
	    {Host, Key} = split_to_slash(Address, []),
	    {http, Host, ?HTTP_DEF_PORT, Key};
	_What ->
	    orber:dbg("[~p] orber_cosnaming_utils:http(~p);~n"
		      "Malformed key; should be http://Host:Port/path/name.html~n"
		      "or http://Host/path/name.html", 
		      [?LINE, Address], ?DEBUG_LEVEL),
	    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
    end.

split_to_slash([], _Acc) ->
    orber:dbg("[~p] orber_cosnaming_utils:split_to_slash();~n"
	      "No Key given Host:Port/Key.html", [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{});
split_to_slash([$/|Rest], Acc) ->
    {lists:reverse(Acc), [$/|Rest]};
split_to_slash([H|T], Acc) ->
    split_to_slash(T, [H|Acc]).

%%----------------------------------------------------------------------
%% Function   : lookup
%% Arguments  : A tuple which contain data about what connection we want to use.
%% Description: 
%% Returns    : Object |
%%              {'EXCEPTION', E}
%%----------------------------------------------------------------------
lookup(Data) ->
    lookup(Data, []).

lookup({corbaname, rir, _Key, []}, Ctx) ->
    %% If no object key supplied NameService is defined to be default.
    corba:resolve_initial_references("NameService", Ctx);
lookup({corbaname, rir, Key, Name}, Ctx) ->
    NS = corba:resolve_initial_references(Key, Ctx),
    'CosNaming_NamingContext':resolve(NS, Ctx, Name);

lookup({corbaloc, rir, Key}, Ctx) ->
     corba:resolve_initial_references(Key, Ctx);

lookup({corbaname, [], _Key, _Name}, _Ctx) ->
    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{});
lookup({corbaname, Addresses, Key, ""}, Ctx) ->
    %% Not Name-string defined, which is the same as corbaloc.
    lookup({corbaloc, Addresses, Key}, Ctx);
lookup({corbaname, [[iiop, Vers, Host, Port]|Addresses], Key, Name}, Ctx) ->
    NS = iop_ior:create_external(Vers, key2id(Key), Host, Port, Key),
    case catch 'CosNaming_NamingContext':resolve(NS, Ctx, Name) of
	{'EXCEPTION', _} ->
	    lookup({corbaname, Addresses, Key, Name}, Ctx);
	Obj ->
	    Obj
    end;
%%% Corbaname via SSL
lookup({corbaname, [[ssliop, Vers, Host, Port]|Addresses], Key, Name}, Ctx) ->
    SSLComponent = 
	#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
			       component_data=#'SSLIOP_SSL'{target_supports = 2, 
							    target_requires = 2, 
							    port = Port}},
    NS = iop_ior:create_external(Vers, key2id(Key), Host, Port, Key, [SSLComponent]),
    case catch 'CosNaming_NamingContext':resolve(NS, Ctx, Name) of
	{'EXCEPTION', _} ->
	    lookup({corbaname, Addresses, Key, Name}, Ctx);
	Obj ->
	    Obj
    end;
lookup({corbaname, [_|Addresses], Key, Name}, Ctx) ->
    lookup({corbaname, Addresses, Key, Name}, Ctx);

lookup({corbaloc, [], _Key}, _Ctx) ->
    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{});
lookup({corbaloc, [[iiop, Vers, Host, Port]|Addresses], Key}, Ctx) ->
    ObjRef = iop_ior:create_external(Vers, key2id(Key), Host, Port, Key),
    OldVal = put(orber_forward_notify, true),
    case catch corba_object:non_existent(ObjRef, Ctx) of
	{location_forward, Result} ->
	    put(orber_forward_notify, OldVal),
	    Result;
	false ->
	    put(orber_forward_notify, OldVal),
	    ObjRef;
	true ->
	    put(orber_forward_notify, OldVal),
	    lookup({corbaloc, Addresses, Key}, Ctx);
	_ ->
	    %% May be located on a version using '_not_existent'
            %% see CORBA2.3.1 page 15-34 try again.
	    case catch corba_object:not_existent(ObjRef, Ctx) of
		{location_forward, Result} ->
		    put(orber_forward_notify, OldVal),
		    Result;
		false ->
		    put(orber_forward_notify, OldVal),
		    ObjRef;
		_ ->
		    put(orber_forward_notify, OldVal),
		    lookup({corbaloc, Addresses, Key}, Ctx)
	    end
    end;

%%% Corbaloc via SSL
lookup({corbaloc, [[ssliop, Vers, Host, Port]|Addresses], Key}, Ctx) ->
    SSLComponent = 
	#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
			       component_data=#'SSLIOP_SSL'{target_supports = 2, 
							    target_requires = 2, 
							    port = Port}},
    ObjRef = iop_ior:create_external(Vers, key2id(Key), Host, Port, Key, [SSLComponent]),
    OldVal = put(orber_forward_notify, true),

    case catch corba_object:non_existent(ObjRef, Ctx) of
	{location_forward, Result} ->
	    put(orber_forward_notify, OldVal),
	    Result;
	false ->
	    put(orber_forward_notify, OldVal),
	    ObjRef;
	true ->
	    put(orber_forward_notify, OldVal),
	    lookup({corbaloc, Addresses, Key}, Ctx);
	_ ->
	    %% May be located on a version using '_not_existent'
            %% see CORBA2.3.1 page 15-34 try again.
	    case catch corba_object:not_existent(ObjRef, Ctx) of
		{location_forward, Result} ->
		    put(orber_forward_notify, OldVal),
		    Result;
		false ->
		    put(orber_forward_notify, OldVal),
		    ObjRef;
		_ ->
		    put(orber_forward_notify, OldVal),
		    lookup({corbaloc, Addresses, Key}, Ctx)
	    end
    end;
			
lookup({corbaloc, [_|Addresses], Key}, Ctx) ->
    lookup({corbaloc, Addresses, Key}, Ctx);
    

lookup({file, File}, _Ctx) ->
    case file:read_file(File) of
	{ok, IOR} ->
	    binary_to_list(IOR);
	{error, Reason} ->
	    orber:dbg("[~p] orber_cosnaming_utils:lookup(~p);~n"
		      "Failed to access file: ~p.", 
		      [?LINE, File, Reason], ?DEBUG_LEVEL),
	    corba:raise(#'CosNaming_NamingContext_InvalidName'{})
    end;
lookup({http, Host, Port, Key}, _Ctx) ->
    SetupTimeout = orber:iiop_setup_connection_timeout(),
    SendTimeout = orber:iiop_timeout(),
    {ok, Socket} = create_connection(Host, Port, SetupTimeout),
    Request = "GET " ++ Key ++ " HTTP/1.0\r\n\r\n",   
    case gen_tcp:send(Socket, Request) of
	ok ->
	    receive_msg(Socket, [], SendTimeout);
	{error, Reason} ->
	    orber:dbg("[~p] orber_cosnaming_utils:lookup();~n"
		      "Failed to send request: ~p.", 
		      [?LINE, Reason], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end;
lookup({ftp, _Address}, _Ctx) ->
    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{});
lookup(_, _Ctx) ->
    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{}).


receive_msg(Socket, Acc, Timeout) ->
    receive 
	{tcp_closed, Socket} ->
	    case re:split(Acc,"\r\n\r\n",[{return,list}]) of
		[_Header, Body] ->
		    Body;
		What ->
		    orber:dbg("[~p] orber_cosnaming_utils:receive_msg();~n"
			      "HTTP server closed the connection before sending a complete reply: ~p.", 
			      [?LINE, What], ?DEBUG_LEVEL),
		    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
	    end;
	{tcp, Socket, Response} ->
	    receive_msg(Socket, Acc ++ Response, Timeout);
	{tcp_error, Socket, Reason} ->
	    orber:dbg("[~p] orber_cosnaming_utils:receive_msg();~n"
		      "connection failed: ~p.", 
		      [?LINE, Reason], ?DEBUG_LEVEL),
	    gen_tcp:close(Socket),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    after Timeout ->
	    gen_tcp:close(Socket),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end.

create_connection(Host, Port, Timeout) ->
    case gen_tcp:connect(Host,Port,[{packet,0},{reuseaddr,true}], Timeout) of
	{ok,Socket} ->
	    {ok,Socket};
	Error ->
	    orber:dbg("[~p] orber_cosnaming_utils:create_connection(~p, ~p, ~p);~n"
		      "Reason: ~p", 
		      [?LINE, Host, Port, Timeout, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------------------
%% Function   : key2id
%% Arguments  : An objectkey (e.g. NameService)
%% Description: 
%% Returns    : The associated IFR-id
%%----------------------------------------------------------------------
key2id(Key) ->
    %% We need this test to avoid returning an exit if an XX:typeID()
    %% fails (e.g. the module doesn't exist).
    case catch key2id_helper(Key) of
	{ok, Id} ->
	    Id;
	_ ->
	    ""
    end.


key2id_helper("NameService") -> 
    {ok, 'CosNaming_NamingContext':typeID()};
key2id_helper("RootPOA") -> 
    {ok, "IDL:omg.org/PortableServer/POA:1.0"};
key2id_helper("POACurrent") -> 
    {ok, "IDL:omg.org/PortableServer/Current:1.0"};
key2id_helper("InterfaceRepository") -> 
    {ok, "IDL:omg.org/CORBA/Repository:1.0"};
key2id_helper("TradingService") -> 
    {ok, "IDL:omg.org/CosTrading/Lookup:1.0"};
key2id_helper("TransactionCurrent") -> 
    {ok, "IDL:omg.org/CosTransactions/Current:1.0"};
key2id_helper("DynAnyFactory") -> 
    {ok, "IDL:omg.org/DynamicAny/DynAnyFactory:1.0"};
key2id_helper("ORBPolicyManager") -> 
    {ok, "IDL:omg.org/CORBA/PolicyManager:1.0"};
key2id_helper("PolicyCurrent") -> 
    {ok, "IDL:omg.org/CORBA/PolicyCurrent:1.0"};
key2id_helper("NotificationService") -> 
    {ok, "IDL:omg.org/CosNotifyChannelAdmin/EventChannelFactory:1.0"};
key2id_helper("TypedNotificationService") -> 
    {ok, "IDL:omg.org/CosTypedNotifyChannelAdmin::TypedEventChannelFactory:1.0"};
key2id_helper("CodecFactory") -> 
    {ok, "IDL:omg.org/IOP/CodecFactory:1.0"};
key2id_helper("PICurrent") -> 
    {ok, "IDL:omg.org/PortableInterceptors/Current:1.0"};
%% Should we use SecurityLevel1 instead?? This key can be either.
key2id_helper("SecurityCurrent") -> 
    {ok, "IDL:omg.org/SecurityLevel2/Current:1.0"};
%% Unknown - use the empty string. Might not work for all other ORB's but it's
%% the only option we've got.
key2id_helper(_) -> 
    {ok, ""}.
    


%%----------------------------------------------------------------------
%% Function   : name2string
%% Arguments  : A sequence of NameComponents
%% Description: 
%% Returns    : A string describing the sequence.
%%----------------------------------------------------------------------
name2string(Name) ->
    name2string(lists:reverse(Name), []).
name2string([], Acc) ->
    lists:flatten(Acc);
name2string([#'CosNaming_NameComponent'{id="", kind=""}], Acc) ->
    name2string([], [$.|Acc]);
name2string([#'CosNaming_NameComponent'{id=ID, kind=""}], Acc) ->
    name2string([], [convert_reserved(ID)|Acc]);
name2string([#'CosNaming_NameComponent'{id=ID, kind=Kind}], Acc) ->
    name2string([], [convert_reserved(ID), $., convert_reserved(Kind)|Acc]);
name2string([#'CosNaming_NameComponent'{id="", kind=""}|T], Acc) ->
    name2string(T, [$/, $.|Acc]);
name2string([#'CosNaming_NameComponent'{id=ID, kind=""}|T], Acc) ->
    name2string(T, [$/, convert_reserved(ID)|Acc]);
name2string([#'CosNaming_NameComponent'{id=ID, kind=Kind}|T], Acc) ->
    name2string(T, [$/, convert_reserved(ID), $., convert_reserved(Kind)|Acc]);
name2string(What, Acc) ->
    orber:dbg("[~p] orber_cosnaming_utils:name2string(~p)~n"
	      "Malformed NameComponent: ~p", 
	      [?LINE, Acc, What], ?DEBUG_LEVEL),
    corba:raise(#'CosNaming_NamingContext_InvalidName'{}).

%% '/' and '.' are reserved as separators but can be overridden by using '\'.
convert_reserved([]) ->
    [];
convert_reserved([$/|T]) ->
    [$\\, $/|convert_reserved(T)];
convert_reserved([$.|T]) ->
    [$\\, $.|convert_reserved(T)];
convert_reserved([$\\, H|T]) ->
    [$\\, H|convert_reserved(T)];
convert_reserved([H|T]) ->
    [H|convert_reserved(T)].


%%----------------------------------------------------------------------
%% Function   : string2name
%% Arguments  : A string describing a sequence of NameComponents.
%% Description: 
%% Returns    : A sequence of NameComponents
%%----------------------------------------------------------------------
string2name([]) ->
    [];
string2name(Str) ->
    {NC, Rest} = get_NC(id, Str, [], []),
    [NC|string2name(Rest)].
    
get_NC(id, [], ID, _Kind) ->
    {#'CosNaming_NameComponent'{id=lists:reverse(ID), kind=""}, []};
get_NC(kind, [], ID, Kind) ->
    {#'CosNaming_NameComponent'{id=lists:reverse(ID), kind=lists:reverse(Kind)}, []};
%% // is not allowed; must be /./
get_NC(id, [$/|_T], [], _) ->
    orber:dbg("[~p] orber_cosnaming_utils:get_NC();~n"
	      "'//' not allowed, use '/./'", [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'CosNaming_NamingContext_InvalidName'{});
get_NC(id, [$., $/|T], [], _) ->
    {#'CosNaming_NameComponent'{id="", kind=""}, T};
%% End of this ID/Kind; in this case kind eq. "".
get_NC(id, [$/|T], ID, _Kind) ->
    {#'CosNaming_NameComponent'{id=lists:reverse(ID), kind=""}, T};
get_NC(kind, [$/|T], ID, Kind) ->
    {#'CosNaming_NameComponent'{id=lists:reverse(ID), kind=lists:reverse(Kind)}, T};
%% ID exist but it's not allowed to write "id1./id2.kind2".
get_NC(id, [$., $/|_T], _, _) ->
    orber:dbg("[~p] orber_cosnaming_utils:get_NC();~n"
	      "'id1./id2.kind2' not allowed, use 'id1/id2.kind2'", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'CosNaming_NamingContext_InvalidName'{});
get_NC(id, [$\\, $., H|T], ID, Kind) ->
    get_NC(id, T, [H, $.|ID], Kind);
get_NC(id, [$\\, $/, H|T], ID, Kind) ->
    get_NC(id, T, [H, $/|ID], Kind);
get_NC(kind, [$\\, $., H|T], ID, Kind) ->
    get_NC(kind, T, ID, [H|Kind]);
get_NC(kind, [$\\, $/, H|T], ID, Kind) ->
    get_NC(kind, T, ID, [H|Kind]);
get_NC(id, [$.|T], ID, Kind) ->
    get_NC(kind, T, ID, Kind);
get_NC(id, [H|T], ID, Kind) ->
    get_NC(id, T, [H|ID], Kind);
get_NC(kind, [H|T], ID, Kind) ->
    get_NC(kind, T, ID, [H|Kind]);
get_NC(Type, Data, ID, Kind) ->
    orber:dbg("[~p] orber_cosnaming_utils:get_NC(~p, ~p, ~p, ~p);~n"
	      "Unknown", [?LINE, Type, Data, ID, Kind], ?DEBUG_LEVEL),
    corba:raise(#'CosNaming_NamingContext_InvalidName'{}).


%% Converts \< to '%3c' 
escape_string(Str) ->
    escape_string(Str, []).
escape_string([], Acc) ->
    lists:reverse(Acc);
escape_string([$\\, Char |T], Acc) ->
    escape_string(T, [code_character(16#0f band Char), 
		      code_character(16#0f band (Char bsr 4)),$%|Acc]);
escape_string([Char|T], Acc) ->
    escape_string(T, [Char|Acc]).


code_character(N) when N < 10 ->
    $0 + N;
code_character(N) ->
    $a + (N - 10).

%% Converts '%3c' to \<
unescape_string(Str) ->
    unescape_string(Str, []).
unescape_string([], Acc) ->    
    lists:reverse(Acc);
unescape_string([$%, H1, H2 |T], Acc) ->
    I1 = hex2int(H1),
    I2 = hex2int(H2),
    I = I1 * 16 + I2,
    unescape_string(T, [I, $\\|Acc]);
unescape_string([H|T], Acc) ->
    unescape_string(T, [H|Acc]).

hex2int(H) when H >= $a ->
    10 + H - $a;
hex2int(H) when H >= $A ->
    10 + H -$A;
hex2int(H) ->
    H - $0.

%%-------------------------- END OF MODULE -----------------------------
