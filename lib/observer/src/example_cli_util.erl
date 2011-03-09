%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-consulting.com>
%%% @author  Marcus Ljungblad, Erlang Training and Consulting Ltd
%%% @doc The module contents the general util functinos for the 
%%%      example_cli.erl module  
%%%
%%% Version: 1.0

%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met: 
%%% * Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer. 
%%% * Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution. 
%%% * Neither the name of the Erlang Training & Consulting nor the names of its
%%% contributors may be used to endorse or promote products
%%% derived from this software without specific prior written permission.
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
%%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------
-module(example_cli_util).
-author('<protest@erlang-consulting.com>').
-copyright('Erlang Training & Consulting Ltd.').


-export([ask/2,
         ask/3
        ]).

-export([valid_yesno/2,
         valid_multi/2,
         valid_ip/2,
         valid_ip_port/2,
	 valid_integer/2,
         valid_range/2,
         valid_str/2,
         valid_node/2
        ]).

%%--------------------------------------------------------------------
%% @doc Place a question with an accompanying validator at the 
%% command prompt.
%% @end
%%--------------------------------------------------------------------	  
-spec ask(atom(), function()) -> term().    
ask(Prompt, Validator) ->
    ask(Prompt, Validator, nil).

%%--------------------------------------------------------------------
%% @doc Place a question with an accompanying validator at the 
%% command prompt.
%% @end
%%--------------------------------------------------------------------	  
-spec ask(atom(), function(), term()) -> term().    
ask(Prompt, Validator, Arg) ->
    io:format("[Q] ~s ~s: ",[Prompt, Validator(prompt, Arg)]),
    [_|ReversedData] = lists:reverse(io:get_line('')),
    Data = lists:reverse(ReversedData),
    case Validator(Data, Arg) of
	{error, Reason} ->
	    io:format("[Err] ~s ~n",[Reason]),
	    ask(Prompt, Validator, Arg);
	ok ->
	    Data
    end.   

%%--------------------------------------------------------------------
%% @doc Yes / no validator used by ask/2. 
%% @end
%%--------------------------------------------------------------------	  
-spec valid_yesno(atom(), any()) -> term().    
valid_yesno(prompt,_) ->
    "[yes/no] ";
valid_yesno(Input,_) when (Input == "yes");
			  (Input == "y");
			  (Input == "no");
			  (Input == "n") ->
    ok;
valid_yesno(_,_) ->
    {error, "Please type yes or no"}.


%%--------------------------------------------------------------------
%% @doc Multiple choice validator used by ask/3. 
%% @end
%%--------------------------------------------------------------------	  
-spec valid_multi(atom(), any()) -> term().    
valid_multi(prompt,_Answers) ->
    %[_|Prompt] = lists:foldl(fun(Ans,Acc) ->
	%			     lists:concat([Acc,"/",Ans])
		%	     end,[],Answers),
%    lists:concat(["[",Prompt,"]"]);
    "";
valid_multi(Input,Answers) ->
    case lists:member(Input, Answers) of
	true ->
	    ok;
	false ->
	    {error, "Invalid input"}
    end.

%%--------------------------------------------------------------------
%% @doc Integer validator used by ask/3 
%% @end
%%--------------------------------------------------------------------	  
-spec valid_integer(atom(), any()) -> term().
valid_integer(prompt, _) ->
    "";
valid_integer(Input, _) ->
    case catch list_to_integer(Input) of
	{'EXIT', _} ->
	    {error, "[!] Must be a number"};
	_ ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc Range validator used by ask/3. 
%% @end
%%--------------------------------------------------------------------	  
-spec valid_range(atom(), tuple(integer(), integer())) -> term().    
valid_range(prompt,{Min,Max}) ->
    lists:concat(["[",Min,"-",Max,"] "]);
valid_range(prompt, nil) ->
    "[1-65535] ";    
valid_range(Input, nil) ->
    valid_range(Input,{1,65535});
valid_range(Input, {Min,Max}) ->
    case catch list_to_integer(Input) of
	{'EXIT', _} ->
	    {error, lists:concat(["[!] Must be a number between ",Min," and ",Max])};
	N ->
	    if (N >= Min) andalso (N =< Max) ->
		    ok;
	       true ->
		    {error, lists:concat(["[!] Must be a number between ",Min," and ",Max])}
	    end    
    end.


%%--------------------------------------------------------------------
%% @doc IP validator used by ask/2. 
%% @end
%%--------------------------------------------------------------------	  
-spec valid_ip(atom(), any()) -> term().    
valid_ip(prompt,_) ->
    "";
valid_ip(Input,_) ->
    case catch string:tokens(Input,".") of
	[A,B,C,D] ->
	    case catch lists:all(fun(X) ->
					 NX = list_to_integer(X),
					 (NX >= 0) andalso (NX < 256)
				 end, [A,B,C,D]) of
		true ->
		    ok;
		false ->
		    {error, "Not a valid IP"};
		{'EXIT',_} ->
		    {error, "Not a valid IP"}
	    end;
	_ ->
	    {error, "Not a valid IP"}
    end.


%%--------------------------------------------------------------------
%% @doc IP:Port validator used by ask/2. 
%% @end
%%--------------------------------------------------------------------	  
-spec valid_ip_port(atom(), any()) -> term().    
valid_ip_port(prompt,_) ->
    "";
valid_ip_port(Input,_) ->
    case catch string:tokens(Input,":") of
	[IP,Port] ->
	    case {valid_ip(IP,nil),valid_range(Port,nil)} of
		{ok, ok} ->
		    ok;
		_ ->
		    {error, "[!] Not a valid IP/port format. Must be: aaa.bbb.ccc.ddd:portnum"}
	    end;
	[IP] ->
	    valid_ip(IP,nil);
	_ ->
	    {error, "[!] Not a valid IP/port format. Must be: aaa.bbb.ccc.ddd:portnum"}
    end.

%%--------------------------------------------------------------------
%% @doc Node name validator used by ask/2. 
%% @end
%%--------------------------------------------------------------------	  
-spec valid_node(atom(), any()) -> term().    
valid_node(prompt,_) ->
    "[<name>@<hostname>]";
valid_node(Input,_) ->
    case string:tokens(Input,"@") of
	[_Name,_HostName] ->
	    ok;
	_ ->
	    {error, "Unknown format"}
    end.

%%--------------------------------------------------------------------
%% @doc String validator used by ask/2. 
%% @end
%%--------------------------------------------------------------------	  
-spec valid_str(atom(), any()) -> term().    
valid_str(prompt,_) ->
    "";
valid_str(_,_) ->
    ok.

