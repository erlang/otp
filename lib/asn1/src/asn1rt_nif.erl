%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2017. All Rights Reserved.
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
-module(asn1rt_nif).

%% Nif interface for asn1

-export([encode_per_complete/1,
	 decode_ber_tlv/1,
	 encode_ber_tlv/1]).

-compile(no_native).
-on_load(load_nif/0).

-define(ASN1_NIF_VSN,1).

load_nif() ->
    LibBaseName = "asn1rt_nif",
    PrivDir = code:priv_dir(asn1),
    LibName = case erlang:system_info(build_type) of
		  opt ->
		      LibBaseName;
		  Type ->
		      LibTypeName = LibBaseName ++ "."  ++ atom_to_list(Type),
		      case (filelib:wildcard(
			      filename:join(
				[PrivDir,
				 "lib",
				 LibTypeName ++ "*"])) /= []) orelse
			  (filelib:wildcard(
			     filename:join(
			       [PrivDir,
				"lib",
				erlang:system_info(system_architecture),
				LibTypeName ++ "*"])) /= []) of
			  true -> LibTypeName;
			  false -> LibBaseName
		      end
	      end,
    Lib = filename:join([PrivDir, "lib", LibName]),
    Status = case erlang:load_nif(Lib, ?ASN1_NIF_VSN) of
		 ok -> ok;
		 {error, {load_failed, _}}=Error1 ->
		     ArchLibDir =
			 filename:join([PrivDir, "lib",
					erlang:system_info(system_architecture)]),
		     Candidate =
			 filelib:wildcard(filename:join([ArchLibDir,LibName ++ "*" ])),
		     case Candidate of
			 [] -> Error1;
			 _ ->
			     ArchLib = filename:join([ArchLibDir, LibName]),
			     erlang:load_nif(ArchLib, ?ASN1_NIF_VSN)
		     end;
		 Error1 -> Error1
	     end,
    case Status of
	ok -> ok;
	{error, {E, Str}} ->
	    error_logger:error_msg("Unable to load asn1 nif library. "
				   "Failed with error:~n\"~p, ~s\"~n",[E,Str]),
	    Status
    end.

decode_ber_tlv(Binary) ->
    case decode_ber_tlv_raw(Binary) of
	{error,Reason} ->
	    exit({error,{asn1,Reason}});
	Other ->
	    Other
    end.

encode_per_complete(TagValueList) ->
    case encode_per_complete_raw(TagValueList) of
	{error,Reason} -> handle_error(Reason, TagValueList);
	Other when is_binary(Other) -> Other
    end.

handle_error([], _)->
    exit({error,{asn1,enomem}});
handle_error($1, L) ->			 % error in complete in driver
    exit({error,{asn1,L}});
handle_error(ErrL, L) ->
    exit({error,{asn1,ErrL,L}}).

encode_per_complete_raw(_TagValueList) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE}).

decode_ber_tlv_raw(_Binary) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE}).

encode_ber_tlv(_TagValueList) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE}).
