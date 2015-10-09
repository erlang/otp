%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999-2000, Ericsson 
%% Utvecklings AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(docgen_xml_check).

-export([validate/1]).
-deprecated([{validate,1,next_major_release}]).

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
	    DtdDir = filename:join(code:priv_dir(erl_docgen), "dtd"),
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
