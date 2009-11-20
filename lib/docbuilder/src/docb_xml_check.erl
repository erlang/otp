%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999-2000, Ericsson 
%% Utvecklings AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(docb_xml_check).

-export([validate/1]).

%% validate(File) -> ok | error | {error, badfile}
%%   File = string(), file name with or without ".xml" extension
%% If XML validation fails for a file, the error information from
%% Xmerl is printed to terminal and the function returns error.
validate(File0) ->
    File = case filename:extension(File0) of
	       ".xml" -> File0;
	       _ -> File0++".xml"
	   end,
    case filelib:is_regular(File) of
	true ->
	    DtdDir = docb_util:dtd_dir(),
	    case catch xmerl_scan:file(File, [{validation,true},
					      {fetch_path,[DtdDir]}]) of
		{'EXIT', Error} ->
		    io:format("~p~n", [Error]),
		    error;
		{_Doc, _Misc} ->
		    ok
	    end;
	false ->
	    {error, badfile}
    end.
