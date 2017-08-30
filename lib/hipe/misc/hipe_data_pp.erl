%% -*- erlang-indent-level: 2 -*-
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <2008-04-20 14:57:08 richard>
%% ====================================================================
%%  Module   :	hipe_data_pp
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-02-25 Erik Johansson (happi@it.uu.se): Created.
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_data_pp).
-export([pp/4]).

%%-----------------------------------------------------------------------------

-include("hipe_consttab.hrl").

-type hipe_code_type() :: 'icode' | 'rtl' | 'arm' | 'ppc' | 'sparc' | 'x86'.

%%-----------------------------------------------------------------------------
%%
%% Pretty print

-spec pp(io:device(), hipe_consttab(), hipe_code_type(), string()) -> 'ok'.

pp(Dev, Table, CodeType, Pre) ->
  Ls = hipe_consttab:labels(Table),
  lists:foreach(fun ({{_, ref}, _}) -> ok;
		    ({L, E}) -> pp_element(Dev, L, E, CodeType, Pre)
		end, 
		[{L, hipe_consttab:lookup(L, Table)} || L <- Ls]).

pp_element(Dev, Name, Element, CodeType, Prefix) ->
  %% Alignment
  case hipe_consttab:const_align(Element) of
    4 -> ok; %% Wordalignment is assumed
    Alignment -> 
      io:format(Dev, "    .align~w\n", [Alignment])
  end,
  %% Local or exported?
  Exported = hipe_consttab:const_exported(Element), 
  case CodeType of
    rtl ->
      case Exported of
	true ->
	  io:format(Dev, "DL~w: ", [Name]);
	false ->
	  io:format(Dev, ".DL~w: ", [Name])
      end;
    _ -> 
      io:format(Dev, "~w ", [Name])
  end,
  %% Type and data...
  case hipe_consttab:const_type(Element) of
    term ->
      io:format(Dev, "~w\n", [hipe_consttab:const_data(Element)]);
    sorted_block ->
      Data = hipe_consttab:const_data(Element),
      pp_block(Dev, {word, lists:sort(Data)}, CodeType, Prefix);
    block ->
      pp_block(Dev, hipe_consttab:const_data(Element), CodeType, Prefix)
  end.

pp_block(Dev, {word, Data, SortOrder}, CodeType, Prefix) ->
  case CodeType of 
    rtl ->
      io:format(Dev, "\n",[]);
    _ ->
      ok
  end,
  pp_wordlist(Dev, Data, CodeType, Prefix),
  case CodeType of 
    rtl ->
      io:format(Dev, ";; Sorted by ~w\n",[SortOrder]);
    _ ->
      ok
  end;
pp_block(Dev, {word, Data}, CodeType, Prefix) ->
  case CodeType of 
    rtl ->
      io:format(Dev, ".word\n",[]);
    _ ->
      ok
  end,
  pp_wordlist(Dev, Data, CodeType, Prefix);
pp_block(Dev, {byte, Data}, CodeType, _Prefix) ->
  case CodeType of 
    rtl ->
      io:format(Dev, ".byte\n   ",[]);
    _ -> 
      ok
  end,
  pp_bytelist(Dev, Data, CodeType),
  case CodeType of 
    rtl ->
      io:format(Dev, "      ;; ~s\n   ", [Data]);
    _ -> ok
  end.
  
pp_wordlist(Dev, [{label, L}|Rest], CodeType, Prefix) ->
  case CodeType of 
    rtl ->
      io:format(Dev, "      &L~w\n", [L]);
    _ -> 
      io:format(Dev, "      <~w>\n", [L])
  end,
  pp_wordlist(Dev, Rest, CodeType, Prefix);
pp_wordlist(Dev, [D|Rest], CodeType, Prefix) ->
  case CodeType of 
    rtl ->
      io:format(Dev, "      ~w\n", [D]);
    _ -> 
      io:format(Dev, "      ~w\n", [D])
  end,
  pp_wordlist(Dev, Rest, CodeType, Prefix);
pp_wordlist(_Dev, [], _CodeType, _Prefix) ->
  ok.

pp_bytelist(Dev, [D], CodeType) ->
  case CodeType of 
    rtl ->
      io:format(Dev, "~w\n", [D]);
    _ -> 
      io:format(Dev, "~w\n", [D])
  end,
  ok;
pp_bytelist(Dev, [D|Rest], CodeType) ->
  case CodeType of 
    rtl ->
      io:format(Dev, "~w,", [D]);
    _ -> 
      io:format(Dev, "~w,", [D])
  end,
  pp_bytelist(Dev, Rest, CodeType);
pp_bytelist(Dev, [], _CodeType) ->
  io:format(Dev, "\n", []).
