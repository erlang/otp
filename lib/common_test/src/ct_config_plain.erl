%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : ct_config_plain.erl
%% Description : CT callback module for reading configs from text files
%%
%% Created : 15 February 2010
%%----------------------------------------------------------------------
-module(ct_config_plain).
-export([read_config_file/1]).

read_config_file(ConfigFile) ->
    case file:consult(ConfigFile) of
	{ok,Config} ->
	    {ok, Config};
	{error,enoent} ->
	    {error, config_file_error, enoent};
	{error,Reason} ->
	    Key =
		case application:get_env(common_test, decrypt) of
		    {ok,KeyOrFile} ->
			case KeyOrFile of
			    {key,K} ->
				K;
			    {file,F} ->
				ct_config:get_crypt_key_from_file(F)
			end;
		    _ ->
			ct_config:get_crypt_key_from_file()
		end,
	    case Key of
		{error,no_crypt_file} ->
		    {error, config_file_error, Reason};
		{error,CryptError} ->
		    {error, decrypt_file_error, CryptError};
		_ when is_list(Key) ->
		    case ct_config:decrypt_config_file(ConfigFile, undefined, {key,Key}) of
			{ok,CfgBin} ->
			    case read_config_terms(CfgBin) of
				{error,ReadFail} ->
				    {error, config_file_error, ReadFail};
				Config ->
				    {ok, Config}
			    end;
			{error,DecryptFail} ->
			    {error, decrypt_config_error, DecryptFail}
		    end;
		_ ->
		    {error, bad_decrypt_key, Key}
	    end
    end.

read_config_terms(Bin) when is_binary(Bin) ->
    case catch binary_to_list(Bin) of
	{'EXIT',_} ->
	    {error,invalid_textfile};
	Lines ->
	    read_config_terms(Lines)
    end;
read_config_terms(Lines) when is_list(Lines) ->
    read_config_terms1(erl_scan:tokens([], Lines, 0), 1, [], []).

read_config_terms1({done,{ok,Ts,EL},Rest}, L, Terms, _) ->
    case erl_parse:parse_term(Ts) of
	{ok,Term} when Rest == [] ->
	    lists:reverse([Term|Terms]);
	{ok,Term} ->
	    read_config_terms1(erl_scan:tokens([], Rest, 0),
			       EL+1, [Term|Terms], Rest);
	_ ->
	    {error,{bad_term,{L,EL}}}
    end;
read_config_terms1({done,{eof,_},_}, _, Terms, Rest) when Rest == [] ->
    lists:reverse(Terms);
read_config_terms1({done,{eof,EL},_}, L, _, _) ->
    {error,{bad_term,{L,EL}}};
read_config_terms1({done,{error,Info,EL},_}, L, _, _) ->
    {error,{Info,{L,EL}}};
read_config_terms1({more,_}, L, Terms, Rest) ->
    case string:tokens(Rest, [$\n,$\r,$\t]) of
	[] ->
	    lists:reverse(Terms);
	_ ->
	    {error,{bad_term,L}}
    end.
