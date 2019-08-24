%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(init_tc).

-export([run/1]).

%% The argument should be a list of filenames (atoms), without extension
%% A .c extension is assumed.
%%

run([Name|Rest]) ->
    case catch run1(atom_to_list(Name)) of
	{'EXIT', Reason} ->
	    io:format("Failed: ~p~n", [Reason]),
	    halt(1);
	_Other ->
	    run(Rest)
    end;
run([]) ->
    ok.

run1(Name) ->
    CFile = Name ++ ".c",
    {ok, Bin} = file:read_file(CFile),
    RE = "\nTESTCASE\\(([_a-zA-Z0-9]*)\\)",
    {match, Cases0} = re:run(Bin, RE, [{capture,all_but_first,list},global]),
    Cases = lists:concat(Cases0),
    generate(Name, Cases).

generate(TcName, Cases) ->
    Hrl = TcName ++ "_cases.hrl",
    {ok, HrlFile} = file:open(Hrl, write),
    generate_hrl(Cases, HrlFile, {TcName, 0}),
    file:close(HrlFile),
    C = TcName ++ "_decl.c",
    {ok, CFile} = file:open(C, write),
    generate_c(Cases, CFile, TcName),
    file:close(CFile).

generate_hrl([Case|Rest], File, {Name, Number}) ->
    io:format(File, "-define(~s, {filename:join(proplists:get_value(data_dir,Config),\"~s\"), ~w}).~n", [Case, Name, Number]),
    generate_hrl(Rest, File, {Name, Number+1});
generate_hrl([], _, _) ->
    ok.

generate_c(Cases, File, TcName) ->
    E= case lists:prefix("ei_", TcName) of 
	   true -> "ei_";
	   false -> ""
       end,
    io:format(File, "#include \"~srunner.h\"\n", [E]),
    lists:foreach(
      fun(Case) ->
	      io:format(File, "extern void ~s(void);~n",
			[Case]) end,
      Cases),
    io:format(File, "~nstatic TestCase test_cases[] = {~n", []),
    lists:foreach(fun(Case) -> io:format(File, "  ~s,~n", [Case]) end, Cases),
    io:format(File, "~s",
	      [["};\n\n",
		"#ifdef VXWORKS\n",
		"int ",	TcName,	"(int argc, char* argv[])\n",
		"#else\n",
		"int main(int argc, char* argv[])\n",
		"#endif\n",
		"{\n",
		"    run_tests(argv[0], test_cases, ",
		"sizeof(test_cases)/sizeof(test_cases[0]));\n",
		"    return 0;\n",
		"}\n"]]).
