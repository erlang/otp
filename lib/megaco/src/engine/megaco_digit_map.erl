%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Parse and evaluate digit maps
%%----------------------------------------------------------------------
%%
%% digitMap           =  digitString
%%                    /   LWSP "(" LWSP digitStringList LWSP ")" LWSP 
%% digitStringList    = digitString *( LWSP "|" LWSP digitString ) 
%% digitString        = 1*(digitStringElement) 
%% digitStringElement = digitPosition [DOT] 
%% digitPosition      = digitMapLetter / digitMapRange
%% digitMapRange      = ("x" / LWSP "[" LWSP digitLetter LWSP "]" LWSP)
%% digitLetter        = *((DIGIT "-" DIGIT ) / digitMapLetter)
%% digitMapLetter     = DIGIT   ; Basic event symbols 
%%                    / %x41-4B ; a-k
%%                    / %x61-6B ; A-K 
%%                    / "T"     ; Start inter-event timers
%%                    / "S"     ; Short inter-event timers
%%                    / "L"     ; Long  inter-event timers, e.g. 16 sec
%%                    / "Z"     ; Long duration modifier
%% DIGIT              = %x30-39 ; 0-9 
%%                   
%%----------------------------------------------------------------------
%% Example of a digit map:
%% 
%% (0| 00|[1-7]xxx|8xxxxxxx|Fxxxxxxx|Exx|91xxxxxxxxxx|9011x.) 
%% 
%% DM = "(0| 00|[1-7]xxx|8xxxxxxx|Fxxxxxxx|Exx|91xxxxxxxxxx|9011x.)".
%% DM = "xxx | xxL3 | xxS4".
%% megaco:parse_digit_map(DM).
%% megaco:test_digit_event(DM, "1234").
%% megaco:test_digit_event(DM, "12ssss3").
%% megaco:test_digit_event(DM, "12ssss4").
%% megaco:test_digit_event(DM, "12ssss5").
%% 
%%----------------------------------------------------------------------

-module(megaco_digit_map).

-export([parse/1, eval/1, eval/2, report/2, test/2]). % Public
-export([test_eval/2]).                               % Internal

-include_lib("megaco/src/app/megaco_internal.hrl").
-include("megaco_message_internal.hrl").
-include_lib("megaco/src/text/megaco_text_tokens.hrl").

-record(state_transition, {mode, next, cont}).

-record(timers, {mode       = state_dependent,
		 start 	    = 0,
		 short 	    = timer_to_millis(3), 
		 long  	    = timer_to_millis(9),
		 duration   = 100,      % (not used) 100 ms <-> 9.9 sec
		 unexpected = reject}). % ignore | reject


%%----------------------------------------------------------------------
%% Parses a digit map body, represented as a list of chars,
%% into a list of state transitions.
%% 
%% Returns {ok, StateTransitionList} | {error, Reason}
%% 
%%----------------------------------------------------------------------

parse(DigitMapBody) when is_list(DigitMapBody) ->
    ?d("parse -> entry with"
       "~n   DigitMapBody: ~p", [DigitMapBody]),
    case parse_digit_map(DigitMapBody) of
	{ok, STL} ->
	    {ok, duration_cleanup(STL, [])};
	{error, Reason} ->
	    {error, Reason}
    end;
parse(_DigitMapBody) ->
    {error, not_a_digit_map_body}.

duration_cleanup([], Acc) ->
    Acc;
duration_cleanup([STL|T], Acc) ->
    #state_transition{cont = Events} = STL,
    Events2 = duration_events_cleanup(Events, []),
    duration_cleanup(T, [STL#state_transition{cont = Events2}|Acc]).

duration_events_cleanup([], Acc) ->
    lists:reverse(Acc);
duration_events_cleanup([duration_event, Event|Events], Acc) ->
    duration_events_cleanup(Events, [{duration_event, Event}|Acc]);
duration_events_cleanup([Event|Events], Acc) ->
    duration_events_cleanup(Events, [Event|Acc]).
    
parse_digit_map(Chars) ->
    parse_digit_map(Chars, 1, [], []).

parse_digit_map(Chars, Line, DS, STL) ->
    ?d("parse_digit_map -> entry with"
       "~n   Chars: ~p"
       "~n   DS:    ~p", [Chars, DS]),
    case megaco_text_scanner:skip_sep_chars(Chars, Line) of
	{[], _Line2} when (DS =/= []) ->
	    case parse_digit_string(DS) of
		{ok, DS2} ->
		    ST = #state_transition{mode = state_dependent,
					   next = start,
					   cont = DS2},
		    STL2 = lists:reverse([ST | STL]),
		    {ok, STL2};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{[Char | Chars2], Line2} ->
	    case Char of
		$( when (DS =:= []) andalso (STL =:= []) ->
		    parse_digit_map(Chars2, Line2, DS, STL);
		$) when (DS =/= []) ->
		    case megaco_text_scanner:skip_sep_chars(Chars2, Line2) of
			{[], _Line3} ->
			    case parse_digit_string(DS) of
				{ok, DS2} ->
				    ST = #state_transition{mode = state_dependent,
							   next = start,
							   cont = DS2},
				    STL2 = lists:reverse([ST | STL]),
				    {ok, STL2};
				{error, Reason} ->
				    {error, Reason}
			    end;
			{Chars3, Line3} ->
			    Trash =  lists:reverse(Chars3),
			    {error, {round_bracket_mismatch, Trash, Line3}}
		    end;
		$| when (DS =/= []) ->
		    case parse_digit_string(DS) of
			{ok, DS2} ->
			    ST = #state_transition{mode = state_dependent,
						   next = start,
						   cont = DS2},
			    parse_digit_map(Chars2, Line2, [], [ST | STL]);
			{error, Reason} ->
			    {error, Reason}
		    end;
		_ when ( Char =/= $( ) andalso 
		       ( Char =/= $| ) andalso 
		       ( Char =/= $) ) ->
		    parse_digit_map(Chars2, Line2, [Char | DS], STL);
		_ ->
		    {error, {round_bracket_mismatch, Line2}}
	    end;
	{[], Line2} ->
	    {error, {digit_string_expected, Line2}}
    end.

parse_digit_string(Chars) ->
    ?d("parse_digit_string -> entry with"
       "~n   Chars: ~p", [Chars]),
    parse_digit_string(Chars, []).

parse_digit_string([Char | Chars], DS) ->
    ?d("parse_digit_string -> entry with"
       "~n   Char:  ~p"
       "~n   Chars: ~p"
       "~n   DS:    ~p", [[Char], Chars, DS]),
    case Char of
	$] ->
	    parse_digit_letter(Chars, [], DS);
	$[ ->
	    {error, square_bracket_mismatch};
	$x ->
	    parse_digit_string(Chars, [{range, $0, $9} | DS]);
	$. ->
	    parse_digit_string(Chars, [zero_or_more | DS]);

	I when (I >= $0) andalso (I =< $9) ->
	    parse_digit_string(Chars, [{single, I} | DS]);

	A when (A >= $a) andalso (A =< $k) ->
	    parse_digit_string(Chars, [{single, A} | DS]);
	A when (A >= $A) andalso (A =< $K) ->
	    parse_digit_string(Chars, [{single, A} | DS]);

	$S ->
	    parse_digit_string(Chars, [use_short_timer | DS]);
	$s ->
	    parse_digit_string(Chars, [use_short_timer | DS]);

	$L ->
	    parse_digit_string(Chars, [use_long_timer | DS]);
	$l ->
	    parse_digit_string(Chars, [use_long_timer | DS]);

        $Z when length(Chars) > 0 ->
            parse_digit_string(Chars, [duration_event | DS]);
        $z when length(Chars) > 0 ->
            parse_digit_string(Chars, [duration_event | DS]);

        $Z ->
            {error, duration_not_allowed_as_last_char};
        $z ->
            {error, duration_not_allowed_as_last_char};

	BadChar ->
	    {error, {illegal_char_in_digit_string, BadChar}}
    end;
parse_digit_string([], DM) ->
    ?d("parse_digit_string -> entry when done with"
       "~n   DM: ~p", [DM]),
    {ok, DM}.


parse_digit_letter([Char | Chars], DL, DS) ->
    ?d("parse_digit_letter -> entry with"
       "~n   Char:  ~p"
       "~n   Chars: ~p"
       "~n   DL:    ~p"
       "~n   DS:    ~p", [[Char], Chars, DL, DS]),
    case Char of
	$[ ->
	    parse_digit_string(Chars, [{letter, DL} | DS]);
	$] ->
	    {error, square_bracket_mismatch};
	To when (To >= $0) andalso (To =< $9) ->
	    case Chars of
		[$-, From | Chars2] when (From >= $0) andalso (From =< $9) ->
		    parse_digit_letter(Chars2, [{range, From, To} | DL], DS);
		_ ->
		    parse_digit_letter(Chars, [{single, To} | DL], DS)
	    end;

	A when (A >= $a) andalso (A =< $k) ->
	    parse_digit_letter(Chars, [{single, A} | DL], DS);
	A when (A >= $A) andalso (A =< $K) ->
	    parse_digit_letter(Chars, [{single, A} | DL], DS);

	$S ->
	    parse_digit_letter(Chars, [use_short_timer | DL], DS);
	$s ->
	    parse_digit_letter(Chars, [use_short_timer | DL], DS);

	$L ->
	    parse_digit_letter(Chars, [use_long_timer | DL], DS);
	$l ->
	    parse_digit_letter(Chars, [use_long_timer | DL], DS);

	$Z ->
	    parse_digit_letter(Chars, [duration_event | DL], DS);
	$z ->
	    parse_digit_letter(Chars, [duration_event | DL], DS);

	BadChar ->
	    {error, {illegal_char_between_square_brackets, BadChar}}
    end;
parse_digit_letter([], _DL, _DS) ->
    {error, square_bracket_mismatch}.


%%----------------------------------------------------------------------
%% Collect digit map letters according to digit map
%% Returns {ok, Letters} | {error, Reason}
%%----------------------------------------------------------------------
     
eval(DMV) when is_record(DMV, 'DigitMapValue') ->
    case parse(DMV#'DigitMapValue'.digitMapBody) of
	{ok, DigitMapBody} ->
	    eval(DigitMapBody, DMV);
	{error, Reason} ->
	    {error, Reason}
    end;
eval(STL) when is_list(STL) ->
     eval(STL, #timers{}).
	
eval(STL, #'DigitMapValue'{startTimer    = Start,
			   shortTimer    = Short,
			   longTimer     = Long,
			   durationTimer = Duration}) ->
    Timers = #timers{start    = timer_to_millis(Start),
		     short    = timer_to_millis(Short),
		     long     = timer_to_millis(Long),
		     duration = duration_to_millis(Duration)},
    eval(STL, Timers);
eval(STL, {ignore, #'DigitMapValue'{startTimer    = Start,
				    shortTimer    = Short,
				    longTimer     = Long,
				    durationTimer = Duration}}) ->
    Timers = #timers{start      = timer_to_millis(Start),
		     short      = timer_to_millis(Short),
		     long       = timer_to_millis(Long),
		     duration   = duration_to_millis(Duration),
		     unexpected = ignore},
    eval(STL, Timers);
eval(STL, {reject, #'DigitMapValue'{startTimer    = Start,
				    shortTimer    = Short,
				    longTimer     = Long,
				    durationTimer = Duration}}) ->
    Timers = #timers{start      = timer_to_millis(Start),
		     short      = timer_to_millis(Short),
		     long       = timer_to_millis(Long),
		     duration   = duration_to_millis(Duration),
		     unexpected = reject},
    eval(STL, Timers);
eval(STL, Timers) when is_list(STL) andalso 
		       is_record(hd(STL), state_transition) andalso
		       is_record(Timers, timers) ->
    ?d("eval -> entry with"
       "~n   STL:    ~p"
       "~n   Timers: ~p", [STL, Timers]),
    case collect(start, mandatory_event, Timers, lists:reverse(STL), []) of
	{error, _} = Error ->
	    ?d("eval -> error:"
	       "~n   Error: ~p", [Error]),
	    Error;
	OK ->
	    ?d("eval -> ok:"
	       "~n   OK: ~p", [OK]),
	    OK
    end;
eval(DigitMapBody, ignore) ->
    eval(DigitMapBody, #timers{unexpected = ignore});
eval(DigitMapBody, reject) ->
    eval(DigitMapBody, #timers{unexpected = reject});
eval(DigitMapBody, Timers) ->
    case parse(DigitMapBody) of
	{ok, STL} ->
	    eval(STL, Timers);
	{error, Reason} ->
	    {error, Reason}
    end.

%% full | unambiguous

collect(Event, State, Timers, STL, Letters) ->
    ?d("collect -> entry with"
       "~n   Event:  ~p"
       "~n   State:  ~p"
       "~n   Timers: ~p"
       "~n   STL:    ~p", [Event, State, Timers, STL]),
    case handle_event(Event, State, Timers, STL, Letters) of
	{completed_full, _Timers2, _STL2, Letters2} ->
	    completed(full, Letters2);
	{completed, _Timers2, _STL2, Letters2} ->
	    completed(unambiguous, Letters2);
	{State2, Timers2, STL2, Letters2} ->
	    ?d("collect -> "
	       "~n   State2:   ~p"
	       "~n   Timers2:  ~p"
	       "~n   Letters2: ~p", [State2, Timers2, Letters2]),
	    MaxWait = choose_timer(State2, Event, Timers2),
	    ?d("collect -> Timer chosen: "
	       "~n   MaxWait: ~p", [MaxWait]),
	    receive
		{?MODULE, _FromPid, Event2} ->
		    ?d("collect -> Got event: "
		       "~n   ~p", [Event2]),
		    collect(Event2, State2, Timers2, STL2, Letters2)
	    after MaxWait ->
		    ?d("collect -> timeout after ~w", [MaxWait]),
		    collect(inter_event_timeout, 
			    State2, Timers2, STL2, Letters2)
	    end;

	{error, Reason} ->
	    ?d("collect -> error: "
	       "~n   Reason: ~p", [Reason]),
	    {error, Reason}
    end.

choose_timer(_State, start, #timers{start = 0}) ->
    ?d("choose_timer(start) -> entry", []),
    infinity;
choose_timer(_State, start, #timers{start = T}) ->
    ?d("choose_timer(start) -> entry with"
       "~n   T: ~p", [T]),
    T;
choose_timer(State, _Event, T) ->
    ?d("choose_timer(~p) -> entry with"
       "~n   State: ~p"
       "~n   T:     ~p", [_Event, State, T]),
    do_choose_timer(State, T).

do_choose_timer(mandatory_event, #timers{mode = state_dependent, long = T}) ->
    T;
do_choose_timer(optional_event, #timers{mode = state_dependent, short = T}) ->
    T;
do_choose_timer(_State, #timers{mode = use_short_timer, short = T}) ->
    T;
do_choose_timer(_State, #timers{mode = use_long_timer, long = T}) ->
    T.

timer_to_millis(asn1_NOVALUE) -> infinity; 
timer_to_millis(infinity)     -> infinity;
timer_to_millis(Seconds)      -> timer:seconds(Seconds).
    
%% Time for duration is in hundreds of milliseconds
duration_to_millis(asn1_NOVALUE) -> 100;
duration_to_millis(Time) when is_integer(Time) -> Time*100.

completed(Kind, {Letters, Event}) when is_list(Letters) ->
    ?d("completed -> entry with"
       "~n   Kind:  ~p"
       "~n   Event: ~s", [Kind, [Event]]),
    {ok, {Kind, duration_letter_cleanup(Letters, []), Event}};
completed(Kind, Letters) when is_list(Letters) ->
    ?d("completed -> entry with"
       "~n   Kind: ~p", [Kind]),
    {ok, {Kind, duration_letter_cleanup(Letters, [])}}.

duration_letter_cleanup([], Acc) ->
    Acc;
duration_letter_cleanup([{long, Letter}|Letters], Acc) ->
    duration_letter_cleanup(Letters, [$Z,Letter|Acc]);
duration_letter_cleanup([Letter|Letters], Acc) ->
    duration_letter_cleanup(Letters, [Letter|Acc]).

unexpected_event(Event, STL, Letters) ->
    Expected = [Next || #state_transition{next = Next} <- STL],
    SoFar    = lists:reverse(Letters),
    Reason   = {unexpected_event, Event, SoFar, Expected},
    {error, Reason}.
    

%%----------------------------------------------------------------------
%% Handles a received event according to digit map
%% State ::= optional_event | mandatory_event
%% 
%% Returns {State, NewSTL, Letters} | {error, Reason}
%%----------------------------------------------------------------------
handle_event(inter_event_timeout, optional_event, Timers, STL, Letters) ->
    {completed_full, Timers, STL, Letters}; % 7.1.14.5 2
handle_event(cancel, _State, _Timers, STL, Letters) ->
    unexpected_event(cancel, STL, Letters);
handle_event(start, _State, Timers, STL, Letters) ->
    {State2, Timers2, STL2} = compute(Timers, STL),
    {State2, Timers2, STL2, Letters};
handle_event(Event, State, Timers, STL, Letters) ->
    ?d("handle_event -> entry when"
       "~n   Event:   ~p"
       "~n   State:   ~p"
       "~n   Timers:  ~p"
       "~n   Letters: ~p", [Event, State, Timers, Letters]),
    {STL2, Collect, KeepDur} = match_event(Event, STL), 
    ?d("handle_event -> match event result: "
       "~n   Collect: ~p"
       "~n   KeepDur: ~p"
       "~n   STL2:    ~p", [Collect, KeepDur, STL2]),
    case STL2 of
	[] when (State =:= optional_event) -> % 7.1.14.5 5
	    ?d("handle_event -> complete-full with event - 7.1.14.5 5", []),
	    {completed_full, Timers, [], {Letters, Event}};
	[] when (Timers#timers.unexpected =:= ignore) ->
	    ok = io:format("<WARNING> Ignoring unexpected event: ~p~n"
			   "Expected: ~p~n",
			   [Event, STL]),
	    {State, Timers, STL, Letters};
	[] when (Timers#timers.unexpected =:= reject) ->
	    ?d("handle_event -> unexpected (reject)", []),
	    unexpected_event(Event, STL, Letters);
	_ ->
	    {State3, Timers2, STL3} = compute(Timers, STL2),
	    ?d("handle_event -> computed: "
	       "~n   State3:  ~p"
	       "~n   Timers2: ~p"
	       "~n   STL3:    ~p", [State3, Timers2, STL3]),
	    case Collect of
		true when (KeepDur =:= true) -> 
		    {State3, Timers2, STL3, [Event | Letters]};
		true -> 
		    case Event of
			{long, ActualEvent} ->
			    {State3, Timers2, STL3, [ActualEvent | Letters]};
			_ ->
			    {State3, Timers2, STL3, [Event | Letters]}
		    end;
		false -> 
		    {State3, Timers2, STL3, Letters}
	    end
    end.

match_event(Event, STL) ->
    MatchingDuration = matching_duration_event(Event, STL),
    match_event(Event, STL, [], false, false, MatchingDuration).

match_event(Event, [ST | OldSTL], NewSTL, Collect, KeepDur, MatchingDuration)
  when is_record(ST, state_transition) ->
    ?d("match_event -> entry with"
       "~n   Event:            ~p"
       "~n   ST:               ~p"
       "~n   NewSTL:           ~p"
       "~n   Collect:          ~p"
       "~n   KeepDur:          ~p"
       "~n   MatchingDuration: ~p", 
       [Event, ST, NewSTL, Collect, KeepDur, MatchingDuration]),
    case ST#state_transition.next of
	{single, Event} ->
	    ?d("match_event -> keep ST (1)", []),
	    match_event(Event, OldSTL, [ST | NewSTL], true, KeepDur,
			MatchingDuration);

        {single, Single} when (Event =:= {long, Single}) andalso 
			      (MatchingDuration =:= false) ->
	    %% Chap 7.1.14.5 point 4
	    ?d("match_event -> keep ST - change to ordinary event (2)", []),
            match_event(Event, OldSTL, [ST | NewSTL], true, KeepDur,
			MatchingDuration);

	{range, From, To} when (Event >= From) andalso (Event =< To) ->
	    ?d("match_event -> keep ST (3)", []),
	    ST2 = ST#state_transition{next = {single, Event}},
	    match_event(Event, OldSTL, [ST2 | NewSTL], true, KeepDur,
			MatchingDuration);

	{range, From, To} ->
	    case Event of
		{long, R} when (R >= From) andalso 
		               (R =< To)   andalso 
                               (MatchingDuration =:= false) ->
		    ?d("match_event -> keep ST (4)", []),
		    ST2 = ST#state_transition{next = {single, R}},
		    match_event(Event, OldSTL, [ST2 | NewSTL], true, true,
				MatchingDuration);
		_ ->
		    ?d("match_event -> drop ST - "
		       "change to ordinary event (5)", []),
		    match_event(Event, OldSTL, NewSTL, Collect, KeepDur,
				MatchingDuration) 
	    end;

        {duration_event, {single, Single}} when (Event =:= {long, Single}) ->
	    ?d("match_event -> keep ST (5)", []),
            match_event(Event, OldSTL, [ST | NewSTL], true, true,
			MatchingDuration);

        {duration_event, {range, From, To}} ->
	    case Event of
		{long, R} when (R >= From) andalso (R =< To) ->
		    ?d("match_event -> keep ST (6)", []),
		    match_event(Event, OldSTL, [ST | NewSTL], true, true,
				MatchingDuration);
		_ ->
		    ?d("match_event -> drop ST (7)", []),
		    match_event(Event, OldSTL, NewSTL, Collect, KeepDur,
				MatchingDuration) 
	    end;

	Event ->
	    ?d("match_event -> keep ST (8)", []),
	    match_event(Event, OldSTL, [ST | NewSTL], Collect, KeepDur,
			MatchingDuration);

	{letter, Letters} ->
	    case match_letter(Event, Letters, MatchingDuration) of
		{true, ChangedEvent} ->
		    ?d("match_event -> keep ST (9)", []),
		    ST2 = ST#state_transition{next = ChangedEvent},
		    match_event(Event, OldSTL, [ST2 | NewSTL], true, KeepDur,
				MatchingDuration);
		true ->
		    ?d("match_event -> keep ST (10)", []),
		    match_event(Event, OldSTL, [ST | NewSTL], true, KeepDur,
				MatchingDuration);
		false ->
		    ?d("match_event -> drop ST (11)", []),
		    match_event(Event, OldSTL, NewSTL, Collect, KeepDur,
				MatchingDuration)
	    end;

	_ ->
	    ?d("match_event -> drop ST (12)", []),
	    match_event(Event, OldSTL, NewSTL, Collect, KeepDur,
			MatchingDuration)
    end;
match_event(Event, [H | T], NewSTL, Collect, KeepDur0, MatchingDuration) 
  when is_list(H) ->
    ?d("match_event -> entry with"
       "~n   Event:            ~p"
       "~n   H:                ~p"
       "~n   NewSTL:           ~p"
       "~n   Collect:          ~p"
       "~n   KeepDur0:         ~p"
       "~n   MatchingDuration: ~p", 
       [Event, H, NewSTL, Collect, KeepDur0, MatchingDuration]),
    {NewSTL2, _Letters, KeepDur} = 
	match_event(Event, H, NewSTL, Collect, KeepDur0, MatchingDuration),
    ?d("match_event -> "
       "~n   NewSTLs: ~p", [NewSTL2]),
    match_event(Event, T, NewSTL2, Collect, KeepDur,
		MatchingDuration);
match_event(_Event, [], NewSTL, Collect, KeepDur, _MatchingDuration) ->
    ?d("match_event -> entry with"
       "~n   NewSTL:  ~p"
       "~n   Collect: ~p"
       "~n   KeepDur: ~p", [NewSTL, Collect, KeepDur]),
    {lists:reverse(NewSTL), Collect, KeepDur}.
    

match_letter(_Event, [], _MatchingDuration) ->
    false;
match_letter(Event, [Letter | Letters], MatchingDuration) ->
    ?d("match_letter -> entry with"
       "~n   Event:            ~p"
       "~n   Letter:           ~p", 
       [Event, Letter]),
    case Letter of
	{single, Event} ->
	    ?d("match_letter -> keep ST (1)", []),
	    true;

        {single, Single} when (Event =:= {long, Single}) andalso 
			      (MatchingDuration =:= false) ->
	    %% Chap 7.1.14.5 point 4
	    ?d("match_letter -> keep ST - change to ordinary event (2)", []),
            true;

	{range, From, To} when (Event >= From) andalso (Event =< To) ->
	    ?d("match_letter -> keep ST (3)", []),
	    {true, {single, Event}};

	{range, From, To} ->
	    case Event of
		{long, R} when (R >= From) andalso 
		               (R =< To)   andalso 
                               (MatchingDuration =:= false) ->
		    ?d("match_letter -> keep ST (4)", []),
		    {true, {single, R}};
		_ ->
		    ?d("match_letter -> drop ST - "
		       "change to ordinary event (5)", []),
		    match_letter(Event, Letters, MatchingDuration)
	    end;

        {duration_event, {single, Single}} when (Event =:= {long, Single}) ->
	    ?d("match_letter -> keep ST (5)", []),
            true;

        {duration_event, {range, From, To}} ->
	    case Event of
		{long, R} when (R >= From) andalso (R =< To) ->
		    ?d("match_letter -> keep ST (6)", []),
		    true;
		_ ->
		    ?d("match_letter -> drop ST (7)", []),
		    match_letter(Event, Letters, MatchingDuration)
	    end;
	
	_ ->
	    ?d("match_letter -> drop ST (8)", []),	    
	    match_letter(Event, Letters, MatchingDuration)

    end.
	

    

matching_duration_event({long, Event}, STL) ->
    Nexts = [Next || #state_transition{next = Next} <- STL],
    mde(Event, Nexts);
matching_duration_event(_Event, _STL) ->
    false.


mde(_, []) ->
    false;
mde(Event, [{duration_event, {single, Event}}|_]) ->
    true;
mde(Event, [{duration_event, {range, From, To}}|_]) 
  when Event >= From, Event =< To ->
    true;
mde(Event, [_|Nexts]) ->
    mde(Event, Nexts).


%%----------------------------------------------------------------------
%% Compute new state transitions
%% Returns {State, Timers, NewSTL}
%%----------------------------------------------------------------------
compute(Timers, OldSTL) ->
    ?d("compute -> entry with"
       "~n   Timers: ~p"
       "~n   OldSTL: ~p", [Timers, OldSTL]),
    {State, GlobalMode, NewSTL} = 
	compute(mandatory_event, state_dependent, OldSTL, []),
    ?d("compute -> "
       "~n   State:      ~p"
       "~n   GlobalMode: ~p"
       "~n   NewSTL:     ~p", [State, GlobalMode, NewSTL]),
    Timers2 = Timers#timers{mode = GlobalMode},
    ?d("compute -> "
       "~n   Timers2: ~p", [Timers2]),
    {State, Timers2, NewSTL}.

compute(State, GlobalMode, [ST | OldSTL], NewSTL) 
  when is_record(ST, state_transition) ->
    ?d("compute(~w) -> entry with"
       "~n   GlobalMode: ~p"
       "~n   ST:         ~p"
       "~n   NewSTL:     ~p", [State, GlobalMode, ST, NewSTL]),
    Cont = ST#state_transition.cont,
    Mode = ST#state_transition.mode,
    {State2, GlobalMode2, NewSTL2} =
	compute_cont(Cont, Mode, GlobalMode, State, NewSTL),
    compute(State2, GlobalMode2, OldSTL, NewSTL2);
compute(State, GlobalMode, [H | T], NewSTL) when is_list(H) ->
    ?d("compute(~w) -> entry with"
       "~n   GlobalMode: ~p"
       "~n   H:          ~p"
       "~n   NewSTL:     ~p", [State, GlobalMode, H, NewSTL]),
    {State2, GlobalMode2, NewSTL2} = compute(State, GlobalMode, H, NewSTL),
    compute(State2, GlobalMode2, T, NewSTL2);
compute(State, GlobalMode, [], NewSTL) ->
    ?d("compute(~w) -> entry with"
       "~n   GlobalMode: ~p"
       "~n   NewSTL:     ~p", [State, GlobalMode, NewSTL]),
    case NewSTL of
	[] -> {completed, GlobalMode, NewSTL};
	_  -> {State,     GlobalMode, NewSTL}
    end.

compute_cont([Next | Cont] = All, Mode, GlobalMode, State, STL) ->
    ?d("compute_cont -> entry with"
       "~n   Next:       ~p"
       "~n   Mode:       ~p"
       "~n   GlobalMode: ~p", [Next, Mode, GlobalMode]),
    case Next of
	%% Retain long timer if that has already been chosen
	use_short_timer when GlobalMode =:= use_long_timer ->
	    compute_cont(Cont, Mode, GlobalMode, State, STL);
	use_short_timer ->
	    Mode2 = use_short_timer,
	    compute_cont(Cont, Mode2, GlobalMode, State, STL);
	use_long_timer ->
	    Mode2 = use_long_timer,
	    compute_cont(Cont, Mode2, GlobalMode, State, STL);
	[] ->
	    %% Skip empty list
	    case Cont of
		[zero_or_more | Cont2] ->
		    compute_cont(Cont2, Mode, GlobalMode, State, STL);
		_ ->
		    compute_cont(Cont, Mode, GlobalMode, State, STL)
	    end;
	_ ->
	    GlobalMode2 =
		case Mode of
		    state_dependent -> GlobalMode;
		    _               -> Mode
		end,
	    case Cont of
		[zero_or_more | Cont2] ->
		    ST = make_cont(Mode, Next, All),
		    compute_cont(Cont2, Mode, GlobalMode2, State, [ST | STL]);
		_ ->
		    ST = make_cont(Mode, Next, Cont),
		    {State, GlobalMode2, [ST | STL]}
	    end
    end;
compute_cont([], GlobalMode, _Mode, _State, STL) ->
    {optional_event, GlobalMode, STL}.

make_cont(Mode, [Next | Cont2], Cont) ->
    #state_transition{mode = Mode, next = Next, cont = [Cont2 | Cont]};
make_cont(Mode, Next, Cont) ->
    #state_transition{mode = Mode, next = Next, cont = Cont}.


%%----------------------------------------------------------------------
%% Send one or more events to event collector process
%% 
%% Events ::= Event* | Event
%% Event  ::= $0-$9 | $a-$k | $A-$K | $S | $L | $Z
%% $S means sleep one second
%% $L means sleep ten seconds
%% $Z means cancel
%% Returns ok | {error, Reason}
%%----------------------------------------------------------------------

report(Pid, [H | T])->
    case report(Pid, H) of
	ok ->
	    report(Pid, T);
	{error, Reason} ->
	    {error, Reason}
    end;
report(_Pid, [])->
    ok;
report(Pid, Event) when is_pid(Pid) ->
    case Event of
	I when I >= $0, I =< $9 -> cast(Pid, Event);
	A when A >= $a, A =< $k -> cast(Pid, Event);
	A when A >= $A, A =< $K -> cast(Pid, Event);
	cancel                  -> cast(Pid, Event);
	$Z                      -> cast(Pid, cancel);
	$z                      -> cast(Pid, cancel);
	$R                      -> timer:sleep(100);  % 100 ms
	$r                      -> timer:sleep(100);  % 100 ms
	$S                      -> sleep(1);  % 1 sec (1000 ms)
	$s                      -> sleep(1);  % 1 sec (1000 ms)
	$L                      -> sleep(10); % 10 sec (10000 ms)
	$l                      -> sleep(10); % 10 sec (10000 ms)
        {long, I} when (I >= $0) and (I =< $9) -> cast(Pid, {long, I});
        {long, A} when (A >= $a) and (A =< $k) -> cast(Pid, {long, A});
        {long, A} when (A >= $A) and (A =< $K) -> cast(Pid, {long, A});
%%         {long, I} when (I >= $0) and (I =< $9) -> long(Pid, I);
%%         {long, A} when (A >= $a) and (A =< $k) -> long(Pid, A);
%%         {long, A} when (A >= $A) and (A =< $K) -> long(Pid, A);
 	_                       -> {error, {illegal_event, Event}}
    end.

%% long(Pid, Event) ->
%%     cast(Pid, long),
%%     cast(Pid, Event).
%% 
sleep(Sec) ->
    timer:sleep(timer:seconds(Sec)),
    ok.

cast(Pid, Event) ->
    Pid ! {?MODULE, self(), Event},
    ok.

%%----------------------------------------------------------------------
%% Feed digit map collector with events
%% Returns: {ok, Letters} | {error, Reason}
%%----------------------------------------------------------------------

test(DigitMap, Events) ->
    Self = self(),
    Pid = spawn_link(?MODULE, test_eval, [DigitMap, Self]),
    report(Pid, Events),
    receive
	{Self, Pid, Res} ->
	    Res;
	{'EXIT', Pid, Reason} ->
	    {error, {'EXIT', Reason}}
    end.

test_eval(DigitMap, Parent) ->
    Res = eval(DigitMap),
    unlink(Parent),
    Parent ! {Parent, self(), Res},
    exit(normal).
