%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
%%% Purpose : Assert macro


%% ?ASSERT/2 - will simply return true if the first argument evaluates to true
%%    otherwise it will exit and output (via the error logger) the
%%    second string
%%
%% Arguments:
%%   Flag	Expression that should evalueate to true or false
%%   String	String to return as a part of the exit reason as well
%%   		be to be sent to the error logger.
%%
%% Returns:
%%   true	If the Flag expression evaluates to true
%%   
%% Exits:
%%   {'EXIT', {assertion_failed, String}}
%%  		If the Flag expression evaluates to something other than
%%		true.
%%
%% Usage notes:
%%   Please note that the Flag argument must be a valid expression that 
%%   evaluates to true.
%%
%%   Also, avoid any side effects in the Flag, as everything performed
%%   within the scope of the ?ASSERT macro will not be present when
%%   the code is not compiled with the debug_on flag.
%%
%%   Side effects include the binding of a variable, sending of a
%%   message, etc.
%%

-ifdef(debug_on).
-define(ASSERT(Flag, String),
	case Flag of
	    true ->
		true;
	   _ ->
		S2 =
		    lists:flatten(
		      io_lib:format(
			"=ASSERT====~nPid:~p, Module:~p, Line:~p~nTermination because assertion failed:~n~p",
			[self(),?MODULE, ?LINE,String])),
		error_logger:error_report(S2),
		exit({assertion_failed, String})
	end
       ).

-define(ALWAYS_ASSERT(String),
	S2 = lists:flatten(
	       io_lib:format(
		 "=ASSERT====~nPid:~p, Module:~p, Line:~p~nTermination because of unconditional assert:~n~p",
		 [self(),?MODULE, ?LINE, String])),
	error_logger:error_report(S2),
	exit({always_assert, String})
	).
-else.
-define(ASSERT(_Flag,_String), true).
-define(ALWAYS_ASSERT(_String), true).
-endif.

				      

