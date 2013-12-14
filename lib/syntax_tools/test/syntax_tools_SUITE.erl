%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(syntax_tools_SUITE).

-include_lib("test_server/include/test_server.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

%% Test cases
-export([smoke_test/1,revert/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [smoke_test,revert].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% Read and parse all source in the OTP release.
smoke_test(Config) when is_list(Config) ->
    ?line Dog = ?t:timetrap(?t:minutes(12)),
    ?line Wc = filename:join([code:lib_dir(),"*","src","*.erl"]),
    ?line Fs = filelib:wildcard(Wc),
    ?line io:format("~p files\n", [length(Fs)]),
    ?line case p_run(fun smoke_test_file/1, Fs) of
	      0 -> ok;
	      N -> ?line ?t:fail({N,errors})
	  end,
    ?line ?t:timetrap_cancel(Dog).

smoke_test_file(File) ->
    case epp_dodger:parse_file(File) of
	{ok,Forms} ->
	    [print_error_markers(F, File) || F <- Forms],
	    ok;
	{error,Reason} ->
	    io:format("~s: ~p\n", [File,Reason]),
	    error
    end.

print_error_markers(F, File) ->
    case erl_syntax:type(F) of
	error_marker ->
	    {L,M,Info} = erl_syntax:error_marker_info(F),
	    io:format("~s:~p: ~s", [File,L,M:format_error(Info)]);
	_ ->
	    ok
    end.
    

%% Read with erl_parse, wrap and revert with erl_syntax and check for equality.
revert(Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(12)),
    Wc = filename:join([code:lib_dir("stdlib"),"src","*.erl"]),
    Fs = filelib:wildcard(Wc),
    Path = [filename:join(code:lib_dir(stdlib), "include"),
            filename:join(code:lib_dir(kernel), "include")],
    io:format("~p files\n", [length(Fs)]),
    case p_run(fun (File) -> revert_file(File, Path) end, Fs) of
        0 -> ok;
        N -> ?line ?t:fail({N,errors})
        end,
    ?line ?t:timetrap_cancel(Dog).

revert_file(File, Path) ->
    case epp:parse_file(File, Path, []) of
        {ok,Fs0} ->
            Fs1 = erl_syntax:form_list(Fs0),
            Fs2 = erl_syntax_lib:map(fun (Node) -> Node end, Fs1),
            Fs3 = erl_syntax:form_list_elements(Fs2),
            Fs4 = [ erl_syntax:revert(Form) || Form <- Fs3 ],
            {ok,_} = compile:forms(Fs4, [report,strong_validation]),
            ok
    end.

p_run(Test, List) ->
    N = erlang:system_info(schedulers),
    p_run_loop(Test, List, N, [], 0).

p_run_loop(_, [], _, [], Errors) ->
    Errors;
p_run_loop(Test, [H|T], N, Refs, Errors) when length(Refs) < N ->
    {_,Ref} = erlang:spawn_monitor(fun() -> exit(Test(H)) end),
    p_run_loop(Test, T, N, [Ref|Refs], Errors);
p_run_loop(Test, List, N, Refs0, Errors0) ->
    receive
	{'DOWN',Ref,process,_,Res} ->
	    Errors = case Res of
			 ok -> Errors0;
			 error -> Errors0+1
		     end,
	    Refs = Refs0 -- [Ref],
	    p_run_loop(Test, List, N, Refs, Errors)
    end.
    
