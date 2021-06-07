%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2020. All Rights Reserved.
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
-module(binary).
%%
%% Implemented in this module:
-export([replace/3,replace/4,
         encode_hex/1, decode_hex/1]).

-export_type([cp/0]).

-opaque cp() :: {'am' | 'bm', reference()}.
-type part() :: {Start :: non_neg_integer(), Length :: integer()}.

%%% BIFs.

-export([at/2, bin_to_list/1, bin_to_list/2, bin_to_list/3,
         compile_pattern/1, copy/1, copy/2, decode_unsigned/1,
         decode_unsigned/2, encode_unsigned/1, encode_unsigned/2,
         first/1, last/1, list_to_bin/1, longest_common_prefix/1,
         longest_common_suffix/1, match/2, match/3, matches/2,
         matches/3, part/2, part/3, referenced_byte_size/1,
         split/2, split/3]).

%% We must inline these functions so that the stacktrace points to
%% the correct function.
-compile({inline, [badarg_with_cause/2, badarg_with_info/1,
                   error_with_info/2]}).

-spec at(Subject, Pos) -> byte() when
      Subject :: binary(),
      Pos :: non_neg_integer().

at(_, _) ->
    erlang:nif_error(undef).

-spec bin_to_list(Subject) -> [byte()] when
      Subject :: binary().

bin_to_list(Subject) ->
    try
        binary_to_list(Subject)
    catch
        error:Reason ->
            error_with_info(Reason, [Subject])
    end.

-spec bin_to_list(Subject, PosLen) -> [byte()] when
      Subject :: binary(),
      PosLen :: part().

bin_to_list(Subject, {Pos, Len}) ->
    try
        bin_to_list(Subject, Pos, Len)
    catch
        error:Reason ->
            error_with_info(Reason, [Subject, {Pos, Len}])
    end;
bin_to_list(Subject, BadPosLen) ->
    badarg_with_info([Subject, BadPosLen]).

-spec bin_to_list(Subject, Pos, Len) -> [byte()] when
      Subject :: binary(),
      Pos :: non_neg_integer(),
      Len :: integer().

bin_to_list(Subject, Pos, Len) when not is_binary(Subject);
                                    not is_integer(Pos);
                                    not is_integer(Len) ->
    %% binary_to_list/3 allows bitstrings as long as the slice fits, and we
    %% want to badarg when Pos/Len aren't integers instead of raising badarith
    %% when adjusting args for binary_to_list/3.
    badarg_with_info([Subject, Pos, Len]);
bin_to_list(Subject, Pos, 0) when Pos >= 0, Pos =< byte_size(Subject) ->
    %% binary_to_list/3 doesn't handle this case.
    [];
bin_to_list(Subject, Pos, Len) when Len < 0 ->
    try
        bin_to_list(Subject, Pos + Len, -Len)
    catch
        error:Reason ->
            error_with_info(Reason, [Subject, Pos, Len])
    end;
bin_to_list(Subject, Pos, Len) when Len > 0 ->
    try
        binary_to_list(Subject, Pos + 1, Pos + Len)
    catch
        error:Reason ->
            error_with_info(Reason, [Subject, Pos, Len])
    end;
bin_to_list(Subject, Pos, Len) ->
    badarg_with_info([Subject, Pos, Len]).

-spec compile_pattern(Pattern) -> cp() when
      Pattern :: binary() | [binary()].

compile_pattern(_) ->
    erlang:nif_error(undef).

-spec copy(Subject) -> binary() when
      Subject :: binary().

copy(_) ->
    erlang:nif_error(undef).

-spec copy(Subject, N) -> binary() when
      Subject :: binary(),
      N :: non_neg_integer().

copy(_, _) ->
    erlang:nif_error(undef).

-spec decode_unsigned(Subject) -> Unsigned when
      Subject :: binary(),
      Unsigned :: non_neg_integer().

decode_unsigned(_) ->
    erlang:nif_error(undef).

-spec decode_unsigned(Subject, Endianness) -> Unsigned when
      Subject :: binary(),
      Endianness :: big | little,
      Unsigned :: non_neg_integer().

decode_unsigned(_, _) ->
    erlang:nif_error(undef).

-spec encode_unsigned(Unsigned) -> binary() when
      Unsigned :: non_neg_integer().

encode_unsigned(_) ->
    erlang:nif_error(undef).

-spec encode_unsigned(Unsigned, Endianness) -> binary() when
      Unsigned :: non_neg_integer(),
      Endianness :: big | little.

encode_unsigned(_, _) ->
    erlang:nif_error(undef).

-spec first(Subject) -> byte() when
      Subject :: binary().

first(_) ->
    erlang:nif_error(undef).

-spec last(Subject) -> byte() when
      Subject :: binary().

last(_) ->
    erlang:nif_error(undef).

-spec list_to_bin(ByteList) -> binary() when
      ByteList :: iolist().

list_to_bin(_) ->
    erlang:nif_error(undef).

-spec longest_common_prefix(Binaries) -> non_neg_integer() when
      Binaries :: [binary()].

longest_common_prefix(_) ->
    erlang:nif_error(undef).

-spec longest_common_suffix(Binaries) -> non_neg_integer() when
      Binaries :: [binary()].

longest_common_suffix(_) ->
    erlang:nif_error(undef).

-spec match(Subject, Pattern) -> Found | nomatch when
      Subject :: binary(),
      Pattern :: binary() | [binary()] | cp(),
      Found :: part().

match(_, _) ->
    erlang:nif_error(undef).

-spec match(Subject, Pattern, Options) -> Found | nomatch when
      Subject :: binary(),
      Pattern :: binary() | [binary()] | cp(),
      Found :: part(),
      Options :: [Option],
      Option :: {scope, part()}.

match(_, _, _) ->
    erlang:nif_error(undef).

-spec matches(Subject, Pattern) -> Found when
      Subject :: binary(),
      Pattern :: binary() | [binary()] | cp(),
      Found :: [part()].

matches(_, _) ->
    erlang:nif_error(undef).

-spec matches(Subject, Pattern, Options) -> Found when
      Subject :: binary(),
      Pattern :: binary() | [binary()] | cp(),
      Found :: [part()],
      Options :: [Option],
      Option :: {scope, part()}.

matches(_, _, _) ->
    erlang:nif_error(undef).

-spec part(Subject, PosLen) -> binary() when
      Subject :: binary(),
      PosLen :: part().

part(_, _) ->
    erlang:nif_error(undef).

-spec part(Subject, Pos, Len) -> binary() when
      Subject :: binary(),
      Pos :: non_neg_integer(),
      Len :: integer().

part(_, _, _) ->
    erlang:nif_error(undef).

-spec referenced_byte_size(Binary) -> non_neg_integer() when
      Binary :: binary().

referenced_byte_size(_) ->
    erlang:nif_error(undef).

-spec split(Subject, Pattern) -> Parts when
      Subject :: binary(),
      Pattern :: binary() | [binary()] | cp(),
      Parts :: [binary()].

split(_, _) ->
    erlang:nif_error(undef).

-spec split(Subject, Pattern, Options) -> Parts when
      Subject :: binary(),
      Pattern :: binary() | [binary()] | cp(),
      Options :: [Option],
      Option :: {scope, part()} | trim | global | trim_all,
      Parts :: [binary()].

split(_, _, _) ->
    erlang:nif_error(undef).

%%% End of BIFs.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% replace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec replace(Subject, Pattern, Replacement) -> Result when
      Subject :: binary(),
      Pattern :: binary() | [ binary() ] | cp(),
      Replacement :: binary(),
      Result :: binary().

replace(H,N,R) ->
    try
        replace(H,N,R,[])
    catch
        error:Reason ->
            error_with_info(Reason, [H,N,R])
    end.

-spec replace(Subject, Pattern, Replacement, Options) -> Result when
      Subject :: binary(),
      Pattern :: binary() | [ binary() ] | cp(),
      Replacement :: binary(),
      Options :: [Option],
      Option :: global | {scope, part()} | {insert_replaced, InsPos},
      InsPos :: OnePos | [ OnePos ],
      OnePos :: non_neg_integer(),
      Result :: binary().

replace(Haystack,Needles,Replacement,Options) ->
    try
	true = is_binary(Replacement), % Make badarg instead of function clause
	{Part,Global,Insert} = get_opts_replace(Options,{no,false,[]}),
	Moptlist = case Part of
		       no ->
			   [];
		       {A,B} ->
			   [{scope,{A,B}}]
		   end,
	MList = if
		    Global ->
			binary:matches(Haystack,Needles,Moptlist);
		    true ->
			case binary:match(Haystack,Needles,Moptlist) of
			    nomatch -> [];
			    Match -> [Match]
			end
		end,
	ReplList = case Insert of
		       [] ->
			   Replacement;
		       Y when is_integer(Y) ->
			   splitat(Replacement,0,[Y]);
		       Li when is_list(Li) ->
			   splitat(Replacement,0,lists:sort(Li))
		   end,
	erlang:iolist_to_binary(do_replace(Haystack,MList,ReplList,0))
   catch
       throw:badopt ->
           badarg_with_cause([Haystack,Needles,Replacement,Options], badopt);
       _:_ ->
           badarg_with_info([Haystack,Needles,Replacement,Options])
   end.


do_replace(H,[],_,N) ->
    [binary:part(H,{N,byte_size(H)-N})];
do_replace(H,[{A,B}|T],Replacement,N) ->
    [binary:part(H,{N,A-N}),
     if
	 is_list(Replacement) ->
	     do_insert(Replacement, binary:part(H,{A,B}));
	 true ->
	     Replacement
     end
     | do_replace(H,T,Replacement,A+B)].

do_insert([X],_) ->
    [X];
do_insert([H|T],R) ->
    [H,R|do_insert(T,R)].

splitat(H,N,[]) ->
    [binary:part(H,{N,byte_size(H)-N})];
splitat(H,N,[I|T]) ->
    [binary:part(H,{N,I-N})|splitat(H,I,T)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Simple helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_opts_replace([],{Part,Global,Insert}) ->
    {Part,Global,Insert};
get_opts_replace([{scope,{A,B}} | T],{_Part,Global,Insert}) ->
    get_opts_replace(T,{{A,B},Global,Insert});
get_opts_replace([global | T],{Part,_Global,Insert}) ->
    get_opts_replace(T,{Part,true,Insert});
get_opts_replace([{insert_replaced,N} | T],{Part,Global,_Insert}) ->
    get_opts_replace(T,{Part,Global,N});
get_opts_replace(_,_) ->
    throw(badopt).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hex encoding functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(HEX(X), (hex(X)):16).
-compile({inline,[hex/1]}).
-spec encode_hex(Bin) -> Bin2 when
      Bin :: binary(),
      Bin2 :: <<_:_*16>>.
encode_hex(Data) when byte_size(Data) rem 8 =:= 0 ->
    << <<?HEX(A),?HEX(B),?HEX(C),?HEX(D),?HEX(E),?HEX(F),?HEX(G),?HEX(H)>> || <<A,B,C,D,E,F,G,H>> <= Data >>;
encode_hex(Data) when byte_size(Data) rem 7 =:= 0 ->
    << <<?HEX(A),?HEX(B),?HEX(C),?HEX(D),?HEX(E),?HEX(F),?HEX(G)>> || <<A,B,C,D,E,F,G>> <= Data >>;
encode_hex(Data) when byte_size(Data) rem 6 =:= 0 ->
    << <<?HEX(A),?HEX(B),?HEX(C),?HEX(D),?HEX(E),?HEX(F)>> || <<A,B,C,D,E,F>> <= Data >>;
encode_hex(Data) when byte_size(Data) rem 5 =:= 0 ->
    << <<?HEX(A),?HEX(B),?HEX(C),?HEX(D),?HEX(E)>> || <<A,B,C,D,E>> <= Data >>;
encode_hex(Data) when byte_size(Data) rem 4 =:= 0 ->
    << <<?HEX(A),?HEX(B),?HEX(C),?HEX(D)>> || <<A,B,C,D>> <= Data >>;
encode_hex(Data) when byte_size(Data) rem 3 =:= 0 ->
    << <<?HEX(A),?HEX(B),?HEX(C)>> || <<A,B,C>> <= Data >>;
encode_hex(Data) when byte_size(Data) rem 2 =:= 0 ->
    << <<?HEX(A),?HEX(B)>> || <<A,B>> <= Data >>;
encode_hex(Data) when is_binary(Data) ->
    << <<?HEX(N)>> || <<N>> <= Data >>;
encode_hex(Bin) ->
    badarg_with_info([Bin]).

hex(X) ->
    element(
      X+1, {16#3030, 16#3031, 16#3032, 16#3033, 16#3034, 16#3035, 16#3036, 16#3037, 16#3038, 16#3039, 16#3041, 16#3042, 16#3043, 16#3044, 16#3045, 16#3046,
            16#3130, 16#3131, 16#3132, 16#3133, 16#3134, 16#3135, 16#3136, 16#3137, 16#3138, 16#3139, 16#3141, 16#3142, 16#3143, 16#3144, 16#3145, 16#3146,
            16#3230, 16#3231, 16#3232, 16#3233, 16#3234, 16#3235, 16#3236, 16#3237, 16#3238, 16#3239, 16#3241, 16#3242, 16#3243, 16#3244, 16#3245, 16#3246,
            16#3330, 16#3331, 16#3332, 16#3333, 16#3334, 16#3335, 16#3336, 16#3337, 16#3338, 16#3339, 16#3341, 16#3342, 16#3343, 16#3344, 16#3345, 16#3346,
            16#3430, 16#3431, 16#3432, 16#3433, 16#3434, 16#3435, 16#3436, 16#3437, 16#3438, 16#3439, 16#3441, 16#3442, 16#3443, 16#3444, 16#3445, 16#3446,
            16#3530, 16#3531, 16#3532, 16#3533, 16#3534, 16#3535, 16#3536, 16#3537, 16#3538, 16#3539, 16#3541, 16#3542, 16#3543, 16#3544, 16#3545, 16#3546,
            16#3630, 16#3631, 16#3632, 16#3633, 16#3634, 16#3635, 16#3636, 16#3637, 16#3638, 16#3639, 16#3641, 16#3642, 16#3643, 16#3644, 16#3645, 16#3646,
            16#3730, 16#3731, 16#3732, 16#3733, 16#3734, 16#3735, 16#3736, 16#3737, 16#3738, 16#3739, 16#3741, 16#3742, 16#3743, 16#3744, 16#3745, 16#3746,
            16#3830, 16#3831, 16#3832, 16#3833, 16#3834, 16#3835, 16#3836, 16#3837, 16#3838, 16#3839, 16#3841, 16#3842, 16#3843, 16#3844, 16#3845, 16#3846,
            16#3930, 16#3931, 16#3932, 16#3933, 16#3934, 16#3935, 16#3936, 16#3937, 16#3938, 16#3939, 16#3941, 16#3942, 16#3943, 16#3944, 16#3945, 16#3946,
            16#4130, 16#4131, 16#4132, 16#4133, 16#4134, 16#4135, 16#4136, 16#4137, 16#4138, 16#4139, 16#4141, 16#4142, 16#4143, 16#4144, 16#4145, 16#4146,
            16#4230, 16#4231, 16#4232, 16#4233, 16#4234, 16#4235, 16#4236, 16#4237, 16#4238, 16#4239, 16#4241, 16#4242, 16#4243, 16#4244, 16#4245, 16#4246,
            16#4330, 16#4331, 16#4332, 16#4333, 16#4334, 16#4335, 16#4336, 16#4337, 16#4338, 16#4339, 16#4341, 16#4342, 16#4343, 16#4344, 16#4345, 16#4346,
            16#4430, 16#4431, 16#4432, 16#4433, 16#4434, 16#4435, 16#4436, 16#4437, 16#4438, 16#4439, 16#4441, 16#4442, 16#4443, 16#4444, 16#4445, 16#4446,
            16#4530, 16#4531, 16#4532, 16#4533, 16#4534, 16#4535, 16#4536, 16#4537, 16#4538, 16#4539, 16#4541, 16#4542, 16#4543, 16#4544, 16#4545, 16#4546,
            16#4630, 16#4631, 16#4632, 16#4633, 16#4634, 16#4635, 16#4636, 16#4637, 16#4638, 16#4639, 16#4641, 16#4642, 16#4643, 16#4644, 16#4645, 16#4646}).

-spec decode_hex(Bin) -> Bin2 when
      Bin :: <<_:_*16>>,
      Bin2 :: binary().
decode_hex(Bin) when byte_size(Bin) rem 2 =:= 0 ->
    << <<(unhex(Int))>> || <<Int:16>> <= Bin >>;
decode_hex(Bin) ->
    badarg_with_info([Bin]).

%% This function pattern-matches on the hexadecimal representation of a pair of characters
%% for example, 16#3030 is matching on the integers <<48, 48>>, which is ascii for <<"00">>
unhex(16#3030) -> 0; unhex(16#3031) -> 1; unhex(16#3032) -> 2; unhex(16#3033) -> 3; unhex(16#3034) -> 4; unhex(16#3035) -> 5; unhex(16#3036) -> 6; unhex(16#3037) -> 7; unhex(16#3038) -> 8; unhex(16#3039) -> 9;
unhex(16#3041) -> 10; unhex(16#3042) -> 11; unhex(16#3043) -> 12; unhex(16#3044) -> 13; unhex(16#3045) -> 14; unhex(16#3046) -> 15;
unhex(16#3061) -> 10; unhex(16#3062) -> 11; unhex(16#3063) -> 12; unhex(16#3064) -> 13; unhex(16#3065) -> 14; unhex(16#3066) -> 15;
unhex(16#3130) -> 16; unhex(16#3131) -> 17; unhex(16#3132) -> 18; unhex(16#3133) -> 19; unhex(16#3134) -> 20; unhex(16#3135) -> 21; unhex(16#3136) -> 22; unhex(16#3137) -> 23; unhex(16#3138) -> 24; unhex(16#3139) -> 25;
unhex(16#3141) -> 26; unhex(16#3142) -> 27; unhex(16#3143) -> 28; unhex(16#3144) -> 29; unhex(16#3145) -> 30; unhex(16#3146) -> 31;
unhex(16#3161) -> 26; unhex(16#3162) -> 27; unhex(16#3163) -> 28; unhex(16#3164) -> 29; unhex(16#3165) -> 30; unhex(16#3166) -> 31;
unhex(16#3230) -> 32; unhex(16#3231) -> 33; unhex(16#3232) -> 34; unhex(16#3233) -> 35; unhex(16#3234) -> 36; unhex(16#3235) -> 37; unhex(16#3236) -> 38; unhex(16#3237) -> 39; unhex(16#3238) -> 40; unhex(16#3239) -> 41;
unhex(16#3241) -> 42; unhex(16#3242) -> 43; unhex(16#3243) -> 44; unhex(16#3244) -> 45; unhex(16#3245) -> 46; unhex(16#3246) -> 47;
unhex(16#3261) -> 42; unhex(16#3262) -> 43; unhex(16#3263) -> 44; unhex(16#3264) -> 45; unhex(16#3265) -> 46; unhex(16#3266) -> 47;
unhex(16#3330) -> 48; unhex(16#3331) -> 49; unhex(16#3332) -> 50; unhex(16#3333) -> 51; unhex(16#3334) -> 52; unhex(16#3335) -> 53; unhex(16#3336) -> 54; unhex(16#3337) -> 55; unhex(16#3338) -> 56; unhex(16#3339) -> 57;
unhex(16#3341) -> 58; unhex(16#3342) -> 59; unhex(16#3343) -> 60; unhex(16#3344) -> 61; unhex(16#3345) -> 62; unhex(16#3346) -> 63;
unhex(16#3361) -> 58; unhex(16#3362) -> 59; unhex(16#3363) -> 60; unhex(16#3364) -> 61; unhex(16#3365) -> 62; unhex(16#3366) -> 63;
unhex(16#3430) -> 64; unhex(16#3431) -> 65; unhex(16#3432) -> 66; unhex(16#3433) -> 67; unhex(16#3434) -> 68; unhex(16#3435) -> 69; unhex(16#3436) -> 70; unhex(16#3437) -> 71; unhex(16#3438) -> 72; unhex(16#3439) -> 73;
unhex(16#3441) -> 74; unhex(16#3442) -> 75; unhex(16#3443) -> 76; unhex(16#3444) -> 77; unhex(16#3445) -> 78; unhex(16#3446) -> 79;
unhex(16#3461) -> 74; unhex(16#3462) -> 75; unhex(16#3463) -> 76; unhex(16#3464) -> 77; unhex(16#3465) -> 78; unhex(16#3466) -> 79;
unhex(16#3530) -> 80; unhex(16#3531) -> 81; unhex(16#3532) -> 82; unhex(16#3533) -> 83; unhex(16#3534) -> 84; unhex(16#3535) -> 85; unhex(16#3536) -> 86; unhex(16#3537) -> 87; unhex(16#3538) -> 88; unhex(16#3539) -> 89;
unhex(16#3541) -> 90; unhex(16#3542) -> 91; unhex(16#3543) -> 92; unhex(16#3544) -> 93; unhex(16#3545) -> 94; unhex(16#3546) -> 95;
unhex(16#3561) -> 90; unhex(16#3562) -> 91; unhex(16#3563) -> 92; unhex(16#3564) -> 93; unhex(16#3565) -> 94; unhex(16#3566) -> 95;
unhex(16#3630) -> 96; unhex(16#3631) -> 97; unhex(16#3632) -> 98; unhex(16#3633) -> 99; unhex(16#3634) -> 100; unhex(16#3635) -> 101; unhex(16#3636) -> 102; unhex(16#3637) -> 103; unhex(16#3638) -> 104; unhex(16#3639) -> 105;
unhex(16#3641) -> 106; unhex(16#3642) -> 107; unhex(16#3643) -> 108; unhex(16#3644) -> 109; unhex(16#3645) -> 110; unhex(16#3646) -> 111;
unhex(16#3661) -> 106; unhex(16#3662) -> 107; unhex(16#3663) -> 108; unhex(16#3664) -> 109; unhex(16#3665) -> 110; unhex(16#3666) -> 111;
unhex(16#3730) -> 112; unhex(16#3731) -> 113; unhex(16#3732) -> 114; unhex(16#3733) -> 115; unhex(16#3734) -> 116; unhex(16#3735) -> 117; unhex(16#3736) -> 118; unhex(16#3737) -> 119; unhex(16#3738) -> 120; unhex(16#3739) -> 121;
unhex(16#3741) -> 122; unhex(16#3742) -> 123; unhex(16#3743) -> 124; unhex(16#3744) -> 125; unhex(16#3745) -> 126; unhex(16#3746) -> 127;
unhex(16#3761) -> 122; unhex(16#3762) -> 123; unhex(16#3763) -> 124; unhex(16#3764) -> 125; unhex(16#3765) -> 126; unhex(16#3766) -> 127;
unhex(16#3830) -> 128; unhex(16#3831) -> 129; unhex(16#3832) -> 130; unhex(16#3833) -> 131; unhex(16#3834) -> 132; unhex(16#3835) -> 133; unhex(16#3836) -> 134; unhex(16#3837) -> 135; unhex(16#3838) -> 136; unhex(16#3839) -> 137;
unhex(16#3841) -> 138; unhex(16#3842) -> 139; unhex(16#3843) -> 140; unhex(16#3844) -> 141; unhex(16#3845) -> 142; unhex(16#3846) -> 143;
unhex(16#3861) -> 138; unhex(16#3862) -> 139; unhex(16#3863) -> 140; unhex(16#3864) -> 141; unhex(16#3865) -> 142; unhex(16#3866) -> 143;
unhex(16#3930) -> 144; unhex(16#3931) -> 145; unhex(16#3932) -> 146; unhex(16#3933) -> 147; unhex(16#3934) -> 148; unhex(16#3935) -> 149; unhex(16#3936) -> 150; unhex(16#3937) -> 151; unhex(16#3938) -> 152; unhex(16#3939) -> 153;
unhex(16#3941) -> 154; unhex(16#3942) -> 155; unhex(16#3943) -> 156; unhex(16#3944) -> 157; unhex(16#3945) -> 158; unhex(16#3946) -> 159;
unhex(16#3961) -> 154; unhex(16#3962) -> 155; unhex(16#3963) -> 156; unhex(16#3964) -> 157; unhex(16#3965) -> 158; unhex(16#3966) -> 159;
unhex(16#4130) -> 160; unhex(16#4131) -> 161; unhex(16#4132) -> 162; unhex(16#4133) -> 163; unhex(16#4134) -> 164; unhex(16#4135) -> 165; unhex(16#4136) -> 166; unhex(16#4137) -> 167; unhex(16#4138) -> 168; unhex(16#4139) -> 169;
unhex(16#4141) -> 170; unhex(16#4142) -> 171; unhex(16#4143) -> 172; unhex(16#4144) -> 173; unhex(16#4145) -> 174; unhex(16#4146) -> 175;
unhex(16#4161) -> 170; unhex(16#4162) -> 171; unhex(16#4163) -> 172; unhex(16#4164) -> 173; unhex(16#4165) -> 174; unhex(16#4166) -> 175;
unhex(16#4230) -> 176; unhex(16#4231) -> 177; unhex(16#4232) -> 178; unhex(16#4233) -> 179; unhex(16#4234) -> 180; unhex(16#4235) -> 181; unhex(16#4236) -> 182; unhex(16#4237) -> 183; unhex(16#4238) -> 184; unhex(16#4239) -> 185;
unhex(16#4241) -> 186; unhex(16#4242) -> 187; unhex(16#4243) -> 188; unhex(16#4244) -> 189; unhex(16#4245) -> 190; unhex(16#4246) -> 191;
unhex(16#4261) -> 186; unhex(16#4262) -> 187; unhex(16#4263) -> 188; unhex(16#4264) -> 189; unhex(16#4265) -> 190; unhex(16#4266) -> 191;
unhex(16#4330) -> 192; unhex(16#4331) -> 193; unhex(16#4332) -> 194; unhex(16#4333) -> 195; unhex(16#4334) -> 196; unhex(16#4335) -> 197; unhex(16#4336) -> 198; unhex(16#4337) -> 199; unhex(16#4338) -> 200; unhex(16#4339) -> 201;
unhex(16#4341) -> 202; unhex(16#4342) -> 203; unhex(16#4343) -> 204; unhex(16#4344) -> 205; unhex(16#4345) -> 206; unhex(16#4346) -> 207;
unhex(16#4361) -> 202; unhex(16#4362) -> 203; unhex(16#4363) -> 204; unhex(16#4364) -> 205; unhex(16#4365) -> 206; unhex(16#4366) -> 207;
unhex(16#4430) -> 208; unhex(16#4431) -> 209; unhex(16#4432) -> 210; unhex(16#4433) -> 211; unhex(16#4434) -> 212; unhex(16#4435) -> 213; unhex(16#4436) -> 214; unhex(16#4437) -> 215; unhex(16#4438) -> 216; unhex(16#4439) -> 217;
unhex(16#4441) -> 218; unhex(16#4442) -> 219; unhex(16#4443) -> 220; unhex(16#4444) -> 221; unhex(16#4445) -> 222; unhex(16#4446) -> 223;
unhex(16#4461) -> 218; unhex(16#4462) -> 219; unhex(16#4463) -> 220; unhex(16#4464) -> 221; unhex(16#4465) -> 222; unhex(16#4466) -> 223;
unhex(16#4530) -> 224; unhex(16#4531) -> 225; unhex(16#4532) -> 226; unhex(16#4533) -> 227; unhex(16#4534) -> 228; unhex(16#4535) -> 229; unhex(16#4536) -> 230; unhex(16#4537) -> 231; unhex(16#4538) -> 232; unhex(16#4539) -> 233;
unhex(16#4541) -> 234; unhex(16#4542) -> 235; unhex(16#4543) -> 236; unhex(16#4544) -> 237; unhex(16#4545) -> 238; unhex(16#4546) -> 239;
unhex(16#4561) -> 234; unhex(16#4562) -> 235; unhex(16#4563) -> 236; unhex(16#4564) -> 237; unhex(16#4565) -> 238; unhex(16#4566) -> 239;
unhex(16#4630) -> 240; unhex(16#4631) -> 241; unhex(16#4632) -> 242; unhex(16#4633) -> 243; unhex(16#4634) -> 244; unhex(16#4635) -> 245; unhex(16#4636) -> 246; unhex(16#4637) -> 247; unhex(16#4638) -> 248; unhex(16#4639) -> 249;
unhex(16#4641) -> 250; unhex(16#4642) -> 251; unhex(16#4643) -> 252; unhex(16#4644) -> 253; unhex(16#4645) -> 254; unhex(16#4646) -> 255;
unhex(16#4661) -> 250; unhex(16#4662) -> 251; unhex(16#4663) -> 252; unhex(16#4664) -> 253; unhex(16#4665) -> 254; unhex(16#4666) -> 255;
unhex(16#6130) -> 160; unhex(16#6131) -> 161; unhex(16#6132) -> 162; unhex(16#6133) -> 163; unhex(16#6134) -> 164; unhex(16#6135) -> 165; unhex(16#6136) -> 166; unhex(16#6137) -> 167; unhex(16#6138) -> 168; unhex(16#6139) -> 169;
unhex(16#6141) -> 170; unhex(16#6142) -> 171; unhex(16#6143) -> 172; unhex(16#6144) -> 173; unhex(16#6145) -> 174; unhex(16#6146) -> 175;
unhex(16#6161) -> 170; unhex(16#6162) -> 171; unhex(16#6163) -> 172; unhex(16#6164) -> 173; unhex(16#6165) -> 174; unhex(16#6166) -> 175;
unhex(16#6230) -> 176; unhex(16#6231) -> 177; unhex(16#6232) -> 178; unhex(16#6233) -> 179; unhex(16#6234) -> 180; unhex(16#6235) -> 181; unhex(16#6236) -> 182; unhex(16#6237) -> 183; unhex(16#6238) -> 184; unhex(16#6239) -> 185;
unhex(16#6241) -> 186; unhex(16#6242) -> 187; unhex(16#6243) -> 188; unhex(16#6244) -> 189; unhex(16#6245) -> 190; unhex(16#6246) -> 191;
unhex(16#6261) -> 186; unhex(16#6262) -> 187; unhex(16#6263) -> 188; unhex(16#6264) -> 189; unhex(16#6265) -> 190; unhex(16#6266) -> 191;
unhex(16#6330) -> 192; unhex(16#6331) -> 193; unhex(16#6332) -> 194; unhex(16#6333) -> 195; unhex(16#6334) -> 196; unhex(16#6335) -> 197; unhex(16#6336) -> 198; unhex(16#6337) -> 199; unhex(16#6338) -> 200; unhex(16#6339) -> 201;
unhex(16#6341) -> 202; unhex(16#6342) -> 203; unhex(16#6343) -> 204; unhex(16#6344) -> 205; unhex(16#6345) -> 206; unhex(16#6346) -> 207;
unhex(16#6361) -> 202; unhex(16#6362) -> 203; unhex(16#6363) -> 204; unhex(16#6364) -> 205; unhex(16#6365) -> 206; unhex(16#6366) -> 207;
unhex(16#6430) -> 208; unhex(16#6431) -> 209; unhex(16#6432) -> 210; unhex(16#6433) -> 211; unhex(16#6434) -> 212; unhex(16#6435) -> 213; unhex(16#6436) -> 214; unhex(16#6437) -> 215; unhex(16#6438) -> 216; unhex(16#6439) -> 217;
unhex(16#6441) -> 218; unhex(16#6442) -> 219; unhex(16#6443) -> 220; unhex(16#6444) -> 221; unhex(16#6445) -> 222; unhex(16#6446) -> 223;
unhex(16#6461) -> 218; unhex(16#6462) -> 219; unhex(16#6463) -> 220; unhex(16#6464) -> 221; unhex(16#6465) -> 222; unhex(16#6466) -> 223;
unhex(16#6530) -> 224; unhex(16#6531) -> 225; unhex(16#6532) -> 226; unhex(16#6533) -> 227; unhex(16#6534) -> 228; unhex(16#6535) -> 229; unhex(16#6536) -> 230; unhex(16#6537) -> 231; unhex(16#6538) -> 232; unhex(16#6539) -> 233;
unhex(16#6541) -> 234; unhex(16#6542) -> 235; unhex(16#6543) -> 236; unhex(16#6544) -> 237; unhex(16#6545) -> 238; unhex(16#6546) -> 239;
unhex(16#6561) -> 234; unhex(16#6562) -> 235; unhex(16#6563) -> 236; unhex(16#6564) -> 237; unhex(16#6565) -> 238; unhex(16#6566) -> 239;
unhex(16#6630) -> 240; unhex(16#6631) -> 241; unhex(16#6632) -> 242; unhex(16#6633) -> 243; unhex(16#6634) -> 244; unhex(16#6635) -> 245; unhex(16#6636) -> 246; unhex(16#6637) -> 247; unhex(16#6638) -> 248; unhex(16#6639) -> 249;
unhex(16#6641) -> 250; unhex(16#6642) -> 251; unhex(16#6643) -> 252; unhex(16#6644) -> 253; unhex(16#6645) -> 254; unhex(16#6646) -> 255;
unhex(16#6661) -> 250; unhex(16#6662) -> 251; unhex(16#6663) -> 252; unhex(16#6664) -> 253; unhex(16#6665) -> 254; unhex(16#6666) -> 255;
unhex(Char) ->
    badarg_with_info([<<Char:16>>]).

badarg_with_cause(Args, Cause) ->
    erlang:error(badarg, Args, [{error_info, #{module => erl_stdlib_errors,
                                               cause => Cause}}]).
badarg_with_info(Args) ->
    erlang:error(badarg, Args, [{error_info, #{module => erl_stdlib_errors}}]).

error_with_info(Reason, Args) ->
    erlang:error(Reason, Args, [{error_info, #{module => erl_stdlib_errors}}]).
