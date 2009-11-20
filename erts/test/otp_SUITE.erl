%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
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

-module(otp_SUITE).

-export([all/1,init_per_suite/1,end_per_suite/1]).
-export([undefined_functions/1,deprecated_not_in_obsolete/1,
	 obsolete_but_not_deprecated/1,call_to_deprecated/1,
         call_to_size_1/1,strong_components/1]).

-include("test_server.hrl").
-import(lists, [filter/2,foldl/3,foreach/2]).

all(suite) ->
    [undefined_functions,deprecated_not_in_obsolete,
     obsolete_but_not_deprecated,call_to_deprecated,
     call_to_size_1,strong_components].

init_per_suite(Config) ->
    Dog = test_server:timetrap(?t:minutes(10)),
    Root = code:root_dir(),
    Server = daily_xref,
    ?line xref:start(Server),
    ?line xref:set_default(Server, [{verbose,false},
                                    {warnings,false},
                                    {builtins,true}]),
    ?line {ok,_Relname} = xref:add_release(Server, Root, {name,otp}),

    %% If we are running the tests in the source tree, the ERTS application
    %% is not in the code path. We must add it explicitly.
    case code:lib_dir(erts) of
	{error,bad_name} ->
	    Erts = filename:join([code:root_dir(),"erts","preloaded","ebin"]),
	    ?line {ok,_} = xref:add_directory(Server, Erts, []);
	_ ->
	    ok
    end,
	    
    ?line ?t:timetrap_cancel(Dog),
    [{xref_server,Server}|Config].

end_per_suite(Config) ->
    Server = ?config(xref_server, Config),
    catch xref:stop(Server),
    Config.

undefined_functions(Config) when is_list(Config) ->
    Server = ?config(xref_server, Config),

    %% Exclude calls from generated modules in the SSL application.
    ExcludeFrom = "SSL-PKIX|PKIX.*|ssl_pkix_oid",
    ?line UndefS = xref_base:analysis(undefined_function_calls),
    ?line Q = io_lib:format("Undef = ~s,"
		      "ExcludedFrom = ~p:_/_,"
		      "Undef - Undef | ExcludedFrom", 
		      [UndefS,ExcludeFrom]),
    ?line {ok,Undef0} = xref:q(Server, lists:flatten(Q)),
    ?line Undef1 = hipe_filter(Undef0),
    ?line Undef2 = ssl_crypto_filter(Undef1),
    ?line Undef3 = edoc_filter(Undef2),
    ?line Undef = eunit_filter(Undef3),
    ?line Undef = megaco_filter(Undef),

    case Undef of
	[] -> ok;
	_ ->
	    foreach(fun ({MFA1,MFA2}) ->
			    io:format("~s calls undefined ~s",
				      [format_mfa(MFA1),format_mfa(MFA2)])
		    end, Undef),
	    ?line ?t:fail({length(Undef),undefined_functions_in_otp})

    end,

    ok.

hipe_filter(Undef) ->
    case erlang:system_info(hipe_architecture) of
	undefined ->
	    filter(fun ({_,{hipe_bifs,_,_}}) -> false;
		       ({_,{hipe,_,_}}) -> false;
		       ({_,{hipe_consttab,_,_}}) -> false;
		       ({_,{hipe_converters,_,_}}) -> false;
		       ({{code,_,_},{Mod,_,_}}) ->
			   not is_hipe_module(Mod);
		       ({{code_server,_,_},{Mod,_,_}}) ->
			   not is_hipe_module(Mod);
		       ({{compile,_,_},{Mod,_,_}}) ->
			   not is_hipe_module(Mod);
		       ({{hipe,_,_},{Mod,_,_}}) ->
			   %% See comment for the next clause.
			   not is_hipe_module(Mod);
		       ({{cerl_to_icode,translate_flags1,2},
			 {hipe_rtl_arch,endianess,0}}) ->
			   false;
		       ({{Caller,_,_},{Callee,_,_}}) ->
			   %% Part of the hipe application is here
			   %% for the sake of Dialyzer. There are many
			   %% undefined calls within the hipe application.
			   not is_hipe_module(Caller) orelse
			       not is_hipe_module(Callee);
		       (_) -> true
		   end, Undef);
	_Arch ->
	    filter(fun ({{Mod,_,_},{hipe_bifs,write_u64,2}}) ->
			   %% Unavailable except in 64 bit AMD. Ignore it.
			   not is_hipe_module(Mod);
		       (_) -> true
		   end, Undef)
    end.

is_hipe_module(Mod) ->
    case atom_to_list(Mod) of
	"hipe_"++_ -> true;
	_ -> false
    end.

ssl_crypto_filter(Undef) ->
    case {code:lib_dir(crypto),code:lib_dir(ssl)} of
	{{error,bad_name},{error,bad_name}} ->
	    filter(fun({_,{ssl,_,_}}) -> false;
		      ({_,{crypto,_,_}}) -> false;
		      (_) -> true
		   end, Undef);
	{_,_} -> Undef
    end.

edoc_filter(Undef) ->
    %% Filter away function call that is catched.
    filter(fun({{edoc_lib,uri_get_http,1},{http,request_sync,2}}) -> false;
	      (_) -> true
	   end, Undef).

eunit_filter(Undef) ->
    filter(fun({{eunit_test,wrapper_test_exported_,0},
		{eunit_test,nonexisting_function,0}}) -> false;
	      (_) -> true
	   end, Undef).

megaco_filter(Undef) ->
    %% Intentional calls to undefined functions.
    filter(fun({{megaco_compact_text_encoder,encode_action_reply,3},
		{megaco_compact_text_encoder_v3,encode_action_reply,2}}) -> false;
	      ({{megaco_compact_text_encoder,encode_action_request,3},
		{megaco_compact_text_encoder_v3,encode_action_request,2}}) -> false;
	      ({{megaco_compact_text_encoder,encode_action_requests,3},
		{megaco_compact_text_encoder_v3,encode_action_requests,2}}) -> false;
	      ({{megaco_compact_text_encoder,encode_command_request,3},
		{megaco_compact_text_encoder_v3,encode_command_request,2}}) -> false;
	      ({{megaco_compact_text_encoder,encode_message,3},
		{megaco_compact_text_encoder_v3,encode_message,2}}) -> false;
	      ({{megaco_compact_text_encoder,encode_transaction,3},
		{megaco_compact_text_encoder_v3,encode_transaction,2}}) -> false;
	      ({{megaco_pretty_text_encoder,encode_action_reply,3},
		{megaco_pretty_text_encoder_v3,encode_action_reply,2}}) -> false;
	      ({{megaco_pretty_text_encoder,encode_action_request,3},
		{megaco_pretty_text_encoder_v3,encode_action_request,2}}) -> false;
	      ({{megaco_pretty_text_encoder,encode_action_requests,3},
		{megaco_pretty_text_encoder_v3,encode_action_requests,2}}) -> false;
	      ({{megaco_pretty_text_encoder,encode_command_request,3},
		{megaco_pretty_text_encoder_v3,encode_command_request,2}}) -> false;
	      ({{megaco_pretty_text_encoder,encode_message,3},
		{megaco_pretty_text_encoder_v3,encode_message,2}}) -> false;
	      ({{megaco_pretty_text_encoder,encode_transaction,3},
		{megaco_pretty_text_encoder_v3,encode_transaction,2}}) -> false;
	      (_) -> true
	   end, Undef).

deprecated_not_in_obsolete(Config) when is_list(Config) ->
    ?line Server = ?config(xref_server, Config),
    ?line {ok,DeprecatedFunctions} = xref:q(Server, "DF"),

    ?line L = foldl(fun({M,F,A}=MFA, Acc) ->
			    case otp_internal:obsolete(M, F, A) of
				no -> [MFA|Acc];
				_ -> Acc
			    end
		    end, [], DeprecatedFunctions),
    case L of
	[] -> ok;
	_ ->
	    io:put_chars("The following functions have -deprecated() attributes,\n"
			 "but are not listed in otp_internal:obsolete/3.\n"),
	    ?line print_mfas(L),
	    ?line ?t:fail({length(L),deprecated_but_not_obsolete})
    end.

obsolete_but_not_deprecated(Config) when is_list(Config) ->
    ?line Server = ?config(xref_server, Config),
    ?line {ok,NotDeprecated} = xref:q(Server, "X - DF"),

    ?line L = foldl(fun({M,F,A}=MFA, Acc) ->
			    case otp_internal:obsolete(M, F, A) of
				no -> Acc;
				_ -> [MFA|Acc]
			    end
		    end, [], NotDeprecated),

    case L of
	[] -> ok;
	_ ->
	    io:put_chars("The following functions are listed "
			 "in otp_internal:obsolete/3,\n"
			 "but don't have -deprecated() attributes.\n"),
	    ?line print_mfas(L),
	    ?line ?t:fail({length(L),obsolete_but_not_deprecated})
    end.
    
    
call_to_deprecated(Config) when is_list(Config) ->
    Server = ?config(xref_server, Config),
    ?line {ok,DeprecatedCalls} = xref:q(Server, "strict(E || DF)"),
    foreach(fun ({MFA1,MFA2}) ->
		    io:format("~s calls deprecated ~s",
			      [format_mfa(MFA1),format_mfa(MFA2)])
	    end, DeprecatedCalls),
    {comment,integer_to_list(length(DeprecatedCalls))++" calls to deprecated functions"}.

call_to_size_1(Config) when is_list(Config) ->
    Server = ?config(xref_server, Config),

    %% Applications that do not call erlang:size/1:
    Apps = [compiler,debugger,kernel,observer,parsetools,
	    runtime_tools,stdlib,tools,webtool],

    Fs = [{erlang,size,1}],

    Q1 = io_lib:format("E || ~p : Fun", [Fs]),
    ?line {ok,AllCallsToSize1} = xref:q(Server, lists:flatten(Q1)),

    Q2 = io_lib:format("E | ~p : App || ~p : Fun", [Apps,Fs]),
    ?line {ok,CallsToSize1} = xref:q(Server, lists:flatten(Q2)),

    case CallsToSize1 of
	[] -> 
            ok;
	_ ->
            io:format("These calls cause an error:~n"),
	    foreach(fun ({MFA1,MFA2}) ->
			    io:format("~s calls soon to be deprecated ~s",
				      [format_mfa(MFA1),format_mfa(MFA2)])
		    end, CallsToSize1)
    end,

    %% Enumerate calls to erlang:size/1 from other applications than
    %% the ones known not to call erlang:size/1:
    case AllCallsToSize1--CallsToSize1 of
        [] ->
            ok;
        Calls ->
            io:format("~n~nThese calls do not cause an error (yet):~n"),
            foreach(fun ({MFA1,MFA2}) ->
                            io:format("~s calls soon to be deprecated ~s",
                                      [format_mfa(MFA1),format_mfa(MFA2)])
                    end, Calls)
    end,
    case CallsToSize1 of
	[] -> 
            ok;
	_ ->
	    ?line ?t:fail({length(CallsToSize1),calls_to_size_1})
    end.

strong_components(Config) when is_list(Config) ->
    Server = ?config(xref_server, Config),
    ?line {ok,Cs} = xref:q(Server, "components AE"),
    io:format("\n\nStrong components:\n\n~p\n", [Cs]),
    ok.

%%%
%%% Common help functions.
%%%
    

print_mfas([MFA|T]) ->
    io:format("~s\n", [format_mfa(MFA)]),
    print_mfas(T);
print_mfas([]) -> ok.

format_mfa({M,F,A}) ->
    lists:flatten(io_lib:format("~s:~s/~p", [M,F,A])).
