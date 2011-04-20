%%%-------------------------------------------------------------------
%%% @author  Aniko Nagyne Vig <aniko@erlang-consulting.com>
%%% @author  Marcus Ljungblad, Erlang Training and Consulting Ltd
%%% @author  Bartlomiej Puzon <bartlomiej.puzon@erlang-solutions.com>
%%% @doc  The module provides a command line user interface from an 
%%%         Erlang shell by calling the start/0 function.
%%%        The functionalities:
%%%        <ul> 
%%%           <li> Add trace case </li>
%%%           <li> List/Run trace cases </li>
%%%           <li> Save trace configuration to file </li>
%%%           <li> Load trace configuration from file </li>
%%%           <li> Set cookie </li>
%%%         </ul>
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
-module(example_cli).
-author('<protest@erlang-consulting.com>').
-copyright('Erlang Training & Consulting Ltd.').
-export([start/0]).
-include("example_server.hrl").


%%--------------------------------------------------------------------
%% @doc Starts a terminal like configuration client for the ttb 
%% wrapper. 
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.   
start() ->
    io:format("ttb Demo GUI~n"
              "===========================================================~n"
              "~n"),              
    
    example_server:start_link(),

    main_menu([]).


main_menu(State) ->
    io:format("~n~n"
              " > Main Menu~n"
              "-----------------------------------------------------------~n"
              " 1) Add trace case ~n"
              " 2) List/Run trace cases ~n"
              " 3) Save configuration to file~n"
              " 4) Load configuration from file~n"
              " 5) Set the magic cookie~n"
              " 6) Exit~n"),              
    
    case example_cli_util:ask("Choice", fun example_cli_util:valid_range/2, {1, 6}) of
        "1" ->        
            add_trace_case(State),
            main_menu(State);
        "2" ->
            NewState = list_trace_cases(State),
            main_menu(NewState);
        "3" ->
            save_to_file(State),
            main_menu(State);
        "4" ->
            load_from_file(State),    
            main_menu(State);
        "5" ->
            set_magic_cookie(),
            main_menu(State);
        "6" ->
            example_server:stop(),
            onviso:clean(),
            io:format("~n Exiting..."),
            ok
    end.

add_trace_case(State) ->
    case lists:keymember(name, 1, State) of 
        true ->        
            io:format("~n"
                      " 1. Add Trace Case~n"
                      "-----------------------------------------------------------~n"
                      " Working on trace case: ~p ~n"                      
                      " 1) Add pattern ~n"
                      " 2) Add nodes ~n"
                      " 3) Add flag specifiction ~n"
                      " 4) Add merge configuration ~n"
		      " 5) Add trace options ~n"
                      " 6) Save ~n"
                      " 7) Cancel ~n",
                      [proplists:get_value(name, State)]),
            case example_cli_util:ask("Choice", fun example_cli_util:valid_range/2, {1, 7}) of
                "1" ->
                    NewState = add_pattern(State),                    
                    add_trace_case(NewState);
                "2" ->
                    NewState = add_node(State),
                    add_trace_case(NewState);
                "3" ->
                    NewState = add_flag_spec(State),
                    add_trace_case(NewState);
                "4" ->
                    NewState = add_merge_configuration(State),
                    add_trace_case(NewState);
		"5" ->
		    NewState = add_trace_opts(State),
		    add_trace_case(NewState);
                "6" ->
                    {patterns, Patterns} = retrieve_field(patterns, State),
                    {nodes, Nodes} = retrieve_field(nodes, State),
                    {flags, Flags} = retrieve_field(flags, State),
                    {merge_confs, Merge} = retrieve_field(merge_confs, State),
		    {trace_opts, TraceOpts} = retrieve_field(trace_opts, State, "[]"),
		   
                    Name = lists:keyfind(name, 1, State),

                    case example_config_gen:validate(Patterns, 
                                                     Nodes, 
                                                     Flags, 
                                                     Merge, 
                                                     TraceOpts,
                                                     Name) of 
                        {ok, #trace_case{} = Case} ->
                            example_server:add_trace_case(Case),
                            io:format("~nTrace case added to the server."),
                            trace_menu_cleanup(State);
                        {error, List} ->
                            io:format("~n"
                                      "Something went wrong on the way." 
                                      "Here's the error output: ~n"
                                      "~p ~n"
                                      "#fail -_- ", [List]),
                            trace_menu_cleanup(State)
                    end;                    
                "7" ->
                    trace_menu_cleanup(State)
            end;                
        false ->
            case example_cli_util:ask("Trace case name", fun example_cli_util:valid_str/2) of
                Name ->
                    NewState = [{name, Name}|State],
                    add_trace_case(NewState)
            end
    end.               

trace_menu_cleanup(State) ->
    S1 = lists:keydelete(patterns, 1, State),
    S2 = lists:keydelete(nodes, 1, S1),
    S3 = lists:keydelete(flags, 1, S2),
    S4 = lists:keydelete(merge_conf, 1, S3),                    
    lists:keydelete(name, 1, S4).


add_pattern(State) ->
    io:format("~n"
              " 1.1. Add pattern to Trace Case~n"
              "-----------------------------------------------------------~n"),
    
    Module = example_cli_util:ask("Module name", fun example_cli_util:valid_str/2),
    Function = example_cli_util:ask("Function name", fun example_cli_util:valid_str/2),
    Arity = example_cli_util:ask("Arguments (use: _ for any)", fun example_cli_util:valid_str/2),
    MS = case example_cli_util:ask("Match specification (default: [])", fun example_cli_util:valid_str/2) of
	     "" ->
		 "[]";
	     Other ->
		 Other
	 end,

    NewPattern = {Module, Function, Arity, MS},
    
    io:format("You entered: ~n"
              "{~p, ~p, ~p, ~p} ~n", 
             [Module, Function, Arity, MS]),              
    
    case example_cli_util:ask("Is this correct?", fun example_cli_util:valid_yesno/2) of
        "no" ->
            State;
        "n" ->
            State;
        _Yes ->
            case lists:keymember(patterns, 1, State) of
                true ->
                    {patterns, Patterns} = lists:keyfind(patterns, 1, State),
                    NewPatterns = [NewPattern|Patterns],
                    lists:keyreplace(patterns, 1, State,
                                     {patterns, NewPatterns});
                false ->
                    [{patterns, [NewPattern]}|State]
            end            
    end.

add_node(State) ->   
    io:format("~n"
              " 1.2. Add node to Trace Case~n"
              "-----------------------------------------------------------~n"),
    
    Node = example_cli_util:ask("Enter node name", fun example_cli_util:valid_node/2),
    
    io:format("~n"
              " Added node ~p~n", [Node]),

    case lists:keymember(nodes, 1, State) of
        true ->
            {nodes, PrevNodes} = lists:keyfind(
                                   nodes, 1, State),
            lists:keyreplace(nodes, 1, State, 
                             {nodes, [Node|PrevNodes]});
        false ->
            [{nodes, [Node]}|State]
    end.   


add_flag_spec(State) ->
    io:format("~n"
              " 1.3. Add flag specification to Trace Case~n"
              "-----------------------------------------------------------~n"),

    Scope = example_cli_util:ask("Set scope", fun example_cli_util:valid_multi/2, 
                            ["all", "new", "existing"]),
    
    Flags = example_cli_util:ask("Add flags (separate multiple with comma)", 
                         fun example_cli_util:valid_str/2),  

    CompleteSpec = string:join(["{", Scope, ",[", Flags, "]}"], ""),
    
    io:format("~n Added flag specification: ~p~n", [CompleteSpec]),
    
    lists:keystore(flags, 1, State, {flags, [CompleteSpec]}).

add_merge_configuration(State) ->
    io:format("~n"
              " 1.4. Add merge configuration to Trace Case~n"
              "-----------------------------------------------------------~n"),

    Fun = case example_cli_util:ask("Create the handler as {fun(), initial_value} (default: undefined)", 
                                 fun example_cli_util:valid_str/2) of
                   "" -> 
                       "undefined";
                   BFun ->
                       BFun
               end,

    Comment = example_cli_util:ask("Create a comment for this merge configuration (default: \"\")",
			   fun example_cli_util:valid_str/2),

    io:format("~n"
              " Handler created: ~n"
              "   ~p~n"
	      " Comment: ~s~n", [Fun, Comment]),
    
    case example_cli_util:ask("Is this correct", fun example_cli_util:valid_yesno/2) of
        "no" ->
            State;
        "n" ->
            State;
        _Yes ->
	    NewConf = [{'fun', Fun}, {comment, Comment}],
	    case lists:keymember(merge_confs, 1, State) of
		true ->
		    {merge_confs, MergeConfs} = lists:keyfind(merge_confs, 1, State),
		    NewConfs = [NewConf | MergeConfs],
		    lists:keyreplace(merge_confs, 1, State, {merge_confs, NewConfs});
		false ->
		    [{merge_confs, [NewConf]} | State]
	    end
    end.

add_trace_opts(State) ->
    io:format("~n"
              " 1.5. Add options to Trace Case~n"
              "-----------------------------------------------------------~n"),

    Opts = example_cli_util:ask("Add trace options (default: [])", 
                         fun example_cli_util:valid_str/2),  
    Opts2 = case Opts of
		"" -> "[]";
		Opts -> Opts
	    end,

    io:format("~n Added options: ~p~n", [Opts2]),
    
    lists:keystore(trace_opts, 1, State, {trace_opts, Opts2}).
    
            

list_trace_cases(State) ->
    io:format("~n"
              " 2. List trace cases~n"
              "-----------------------------------------------------------~n"),
    
    Traces = example_server:get_trace_cases(),
    Length = length(Traces),
    NoTraces = lists:zip(lists:seq(1, Length), Traces),
    
    io:format("~n"
              "Traces currently added: ~n"
              "~p~n", [NoTraces]),
    
    io:format("-----------------------------------------------------------~n"
              " 1) Run trace~n"
              " 2) Stop trace~n"
              " 3) Merge trace~n"
              " 4) Return to main menu~n"),

    case example_cli_util:ask("Choice", fun example_cli_util:valid_range/2, {1,4}) of
        "1" ->
            Case = example_cli_util:ask("Pick trace case", 
                                fun example_cli_util:valid_range/2, {1,Length}),
	    Comment = example_cli_util:ask("Add comment (default: \"\")", 
                                fun example_cli_util:valid_str/2),
            
            io:format("Starting trace case ~p ~n", [Case]),

            case example_server:run_trace(list_to_integer(Case), Comment) of                
                ok ->
                    io:format("~nStarted trace case~n");
                _Error ->
                    io:format("~nCould not start trace case.")                    
            end,
            State;
        "2" ->
            example_server:stop_trace(),
            
            io:format("~n"
                      "Stopped trace~n"),
            State;            
        "3" ->
            Case = example_cli_util:ask("Pick trace case", 
                                fun example_cli_util:valid_range/2, {1,Length}),
	    MergeId = example_cli_util:ask("Pick merge configuration",
				  fun example_cli_util:valid_integer/2, ""),
	    RunId = example_cli_util:ask("Pick run instance",
				 fun example_cli_util:valid_integer/2, ""),
            io:format("Begin merging trace case (~p) - "
                      "Note: this may take a while.~n", [Case]),
            
            case catch example_server:merge_trace(list_to_integer(Case),
                                                  list_to_integer(RunId),
                                                  list_to_integer(MergeId)) of
                {'EXIT', _}  ->
                    io:format("~nCould not merge trace case.");
                _ ->
                    io:format("~nTrace items merged successfully!~n", [])
            end,
            State;
        "4" ->
            State
    end.
        

save_to_file(State) ->
    io:format("~n"
              " 3. Save current configuration to file~n"
              "-----------------------------------------------------------~n"),    
    FileName = example_cli_util:ask("Choose filename", fun example_cli_util:valid_str/2),
    case filelib:is_file(FileName) of
	false ->
	    do_save_to_file(State, FileName);
	true ->
	    Sure = example_cli_util:ask("File exists, overwrite?", fun example_cli_util:valid_yesno/2),
	    case Sure of
		"yes" ->
		    do_save_to_file(State, FileName);
		"y" ->
		    do_save_to_file(State, FileName);
		_ ->
		    State
	    end
    end.

do_save_to_file(State, FileName) ->
    case example_server:save_to_file(FileName) of
	{ok, _} ->
	    io:format("~n"
		      "Saved to file: ~p ~n", [FileName]);
	Error ->
	    io:format("~n"
		      "Saving to ~p failed: ~p ~n", [FileName, Error])
    end,
    State.

load_from_file(State) ->
    io:format("~n"
              " 4. Load configuration from file (Warning: current config"
              " will be overwritten.~n"
              "-----------------------------------------------------------~n"),    
    FileName = example_cli_util:ask("Choose filename", fun example_cli_util:valid_str/2),

    case example_server:read_from_file(FileName) of
        {ok, Results} ->            
            io:format("~n"
                      "Loaded configuration from: ~p ~n"
                      "Results:~n~p~n" , [FileName, Results]);
        Error ->
            io:format("~n"
                      "Could not load configuration from: ~p ~n"
                      "Error: ~p~n", [FileName, Error])
    end,
    State.

set_magic_cookie() ->
    io:format("~n"
              " 5. Set the magic (Erlang) cookie~n"
              "-----------------------------------------------------------~n"),

    Cookie = example_cli_util:ask("Make magic with", fun example_cli_util:valid_str/2),
    erlang:set_cookie(node(), list_to_atom(Cookie)),
    
    io:format("~n"
              "Sim sala bim. Magic complete.").

retrieve_field(Key, List) ->
    retrieve_field(Key, List, []).

retrieve_field(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
	{Key, Val} ->
	    {Key, Val};
	false ->
	    {Key, Default}
    end.
    

