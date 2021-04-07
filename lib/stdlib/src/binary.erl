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
-spec encode_hex(Bin) -> Bin2 when
      Bin :: binary(),
      Bin2 :: <<_:_*16>>.
encode_hex(Data) when byte_size(Data) band 7 =:= 0 ->
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

unhex(12336) ->  0; unhex(12337) ->  1; unhex(12338) ->  2; unhex(12339) ->  3; unhex(12340) ->  4; unhex(12341) ->  5; unhex(12342) ->  6; unhex(12343) ->  7; unhex(12344) ->  8; unhex(12345) ->  9;
unhex(12353) -> 10; unhex(12354) -> 11; unhex(12355) -> 12; unhex(12356) -> 13; unhex(12357) -> 14; unhex(12358) -> 15;
unhex(12385) -> 10; unhex(12386) -> 11; unhex(12387) -> 12; unhex(12388) -> 13; unhex(12389) -> 14; unhex(12390) -> 15;
unhex(12592) -> 16; unhex(12593) -> 17; unhex(12594) -> 18; unhex(12595) -> 19; unhex(12596) -> 20; unhex(12597) -> 21; unhex(12598) -> 22; unhex(12599) -> 23; unhex(12600) -> 24; unhex(12601) -> 25;
unhex(12609) -> 26; unhex(12610) -> 27; unhex(12611) -> 28; unhex(12612) -> 29; unhex(12613) -> 30; unhex(12614) -> 31;
unhex(12641) -> 26; unhex(12642) -> 27; unhex(12643) -> 28; unhex(12644) -> 29; unhex(12645) -> 30; unhex(12646) -> 31;
unhex(12848) -> 32; unhex(12849) -> 33; unhex(12850) -> 34; unhex(12851) -> 35; unhex(12852) -> 36; unhex(12853) -> 37; unhex(12854) -> 38; unhex(12855) -> 39; unhex(12856) -> 40; unhex(12857) -> 41;
unhex(12865) -> 42; unhex(12866) -> 43; unhex(12867) -> 44; unhex(12868) -> 45; unhex(12869) -> 46; unhex(12870) -> 47;
unhex(12897) -> 42; unhex(12898) -> 43; unhex(12899) -> 44; unhex(12900) -> 45; unhex(12901) -> 46; unhex(12902) -> 47;
unhex(13104) -> 48; unhex(13105) -> 49; unhex(13106) -> 50; unhex(13107) -> 51; unhex(13108) -> 52; unhex(13109) -> 53; unhex(13110) -> 54; unhex(13111) -> 55; unhex(13112) -> 56; unhex(13113) -> 57;
unhex(13121) -> 58; unhex(13122) -> 59; unhex(13123) -> 60; unhex(13124) -> 61; unhex(13125) -> 62; unhex(13126) -> 63;
unhex(13153) -> 58; unhex(13154) -> 59; unhex(13155) -> 60; unhex(13156) -> 61; unhex(13157) -> 62; unhex(13158) -> 63;
unhex(13360) -> 64; unhex(13361) -> 65; unhex(13362) -> 66; unhex(13363) -> 67; unhex(13364) -> 68; unhex(13365) -> 69; unhex(13366) -> 70; unhex(13367) -> 71; unhex(13368) -> 72; unhex(13369) -> 73;
unhex(13377) -> 74; unhex(13378) -> 75; unhex(13379) -> 76; unhex(13380) -> 77; unhex(13381) -> 78; unhex(13382) -> 79;
unhex(13409) -> 74; unhex(13410) -> 75; unhex(13411) -> 76; unhex(13412) -> 77; unhex(13413) -> 78; unhex(13414) -> 79;
unhex(13616) -> 80; unhex(13617) -> 81; unhex(13618) -> 82; unhex(13619) -> 83; unhex(13620) -> 84; unhex(13621) -> 85; unhex(13622) -> 86; unhex(13623) -> 87; unhex(13624) -> 88; unhex(13625) -> 89;
unhex(13633) -> 90; unhex(13634) -> 91; unhex(13635) -> 92; unhex(13636) -> 93; unhex(13637) -> 94; unhex(13638) -> 95;
unhex(13665) -> 90; unhex(13666) -> 91; unhex(13667) -> 92; unhex(13668) -> 93; unhex(13669) -> 94; unhex(13670) -> 95;
unhex(13872) -> 96; unhex(13873) -> 97; unhex(13874) -> 98; unhex(13875) -> 99; unhex(13876) -> 100; unhex(13877) -> 101; unhex(13878) -> 102; unhex(13879) -> 103; unhex(13880) -> 104; unhex(13881) -> 105;
unhex(13889) -> 106; unhex(13890) -> 107; unhex(13891) -> 108; unhex(13892) -> 109; unhex(13893) -> 110; unhex(13894) -> 111;
unhex(13921) -> 106; unhex(13922) -> 107; unhex(13923) -> 108; unhex(13924) -> 109; unhex(13925) -> 110; unhex(13926) -> 111;
unhex(14128) -> 112; unhex(14129) -> 113; unhex(14130) -> 114; unhex(14131) -> 115; unhex(14132) -> 116; unhex(14133) -> 117; unhex(14134) -> 118; unhex(14135) -> 119; unhex(14136) -> 120; unhex(14137) -> 121;
unhex(14145) -> 122; unhex(14146) -> 123; unhex(14147) -> 124; unhex(14148) -> 125; unhex(14149) -> 126; unhex(14150) -> 127;
unhex(14177) -> 122; unhex(14178) -> 123; unhex(14179) -> 124; unhex(14180) -> 125; unhex(14181) -> 126; unhex(14182) -> 127;
unhex(14384) -> 128; unhex(14385) -> 129; unhex(14386) -> 130; unhex(14387) -> 131; unhex(14388) -> 132; unhex(14389) -> 133; unhex(14390) -> 134; unhex(14391) -> 135; unhex(14392) -> 136; unhex(14393) -> 137;
unhex(14401) -> 138; unhex(14402) -> 139; unhex(14403) -> 140; unhex(14404) -> 141; unhex(14405) -> 142; unhex(14406) -> 143;
unhex(14433) -> 138; unhex(14434) -> 139; unhex(14435) -> 140; unhex(14436) -> 141; unhex(14437) -> 142; unhex(14438) -> 143;
unhex(14640) -> 144; unhex(14641) -> 145; unhex(14642) -> 146; unhex(14643) -> 147; unhex(14644) -> 148; unhex(14645) -> 149; unhex(14646) -> 150; unhex(14647) -> 151; unhex(14648) -> 152; unhex(14649) -> 153;
unhex(14657) -> 154; unhex(14658) -> 155; unhex(14659) -> 156; unhex(14660) -> 157; unhex(14661) -> 158; unhex(14662) -> 159;
unhex(14689) -> 154; unhex(14690) -> 155; unhex(14691) -> 156; unhex(14692) -> 157; unhex(14693) -> 158; unhex(14694) -> 159;
unhex(16688) -> 160; unhex(16689) -> 161; unhex(16690) -> 162; unhex(16691) -> 163; unhex(16692) -> 164; unhex(16693) -> 165; unhex(16694) -> 166; unhex(16695) -> 167; unhex(16696) -> 168; unhex(16697) -> 169;
unhex(16705) -> 170; unhex(16706) -> 171; unhex(16707) -> 172; unhex(16708) -> 173; unhex(16709) -> 174; unhex(16710) -> 175;
unhex(16737) -> 170; unhex(16738) -> 171; unhex(16739) -> 172; unhex(16740) -> 173; unhex(16741) -> 174; unhex(16742) -> 175;
unhex(16944) -> 176; unhex(16945) -> 177; unhex(16946) -> 178; unhex(16947) -> 179; unhex(16948) -> 180; unhex(16949) -> 181; unhex(16950) -> 182; unhex(16951) -> 183; unhex(16952) -> 184; unhex(16953) -> 185;
unhex(16961) -> 186; unhex(16962) -> 187; unhex(16963) -> 188; unhex(16964) -> 189; unhex(16965) -> 190; unhex(16966) -> 191;
unhex(16993) -> 186; unhex(16994) -> 187; unhex(16995) -> 188; unhex(16996) -> 189; unhex(16997) -> 190; unhex(16998) -> 191;
unhex(17200) -> 192; unhex(17201) -> 193; unhex(17202) -> 194; unhex(17203) -> 195; unhex(17204) -> 196; unhex(17205) -> 197; unhex(17206) -> 198; unhex(17207) -> 199; unhex(17208) -> 200; unhex(17209) -> 201;
unhex(17217) -> 202; unhex(17218) -> 203; unhex(17219) -> 204; unhex(17220) -> 205; unhex(17221) -> 206; unhex(17222) -> 207;
unhex(17249) -> 202; unhex(17250) -> 203; unhex(17251) -> 204; unhex(17252) -> 205; unhex(17253) -> 206; unhex(17254) -> 207;
unhex(17456) -> 208; unhex(17457) -> 209; unhex(17458) -> 210; unhex(17459) -> 211; unhex(17460) -> 212; unhex(17461) -> 213; unhex(17462) -> 214; unhex(17463) -> 215; unhex(17464) -> 216; unhex(17465) -> 217;
unhex(17473) -> 218; unhex(17474) -> 219; unhex(17475) -> 220; unhex(17476) -> 221; unhex(17477) -> 222; unhex(17478) -> 223;
unhex(17505) -> 218; unhex(17506) -> 219; unhex(17507) -> 220; unhex(17508) -> 221; unhex(17509) -> 222; unhex(17510) -> 223;
unhex(17712) -> 224; unhex(17713) -> 225; unhex(17714) -> 226; unhex(17715) -> 227; unhex(17716) -> 228; unhex(17717) -> 229; unhex(17718) -> 230; unhex(17719) -> 231; unhex(17720) -> 232; unhex(17721) -> 233;
unhex(17729) -> 234; unhex(17730) -> 235; unhex(17731) -> 236; unhex(17732) -> 237; unhex(17733) -> 238; unhex(17734) -> 239;
unhex(17761) -> 234; unhex(17762) -> 235; unhex(17763) -> 236; unhex(17764) -> 237; unhex(17765) -> 238; unhex(17766) -> 239;
unhex(17968) -> 240; unhex(17969) -> 241; unhex(17970) -> 242; unhex(17971) -> 243; unhex(17972) -> 244; unhex(17973) -> 245; unhex(17974) -> 246; unhex(17975) -> 247; unhex(17976) -> 248; unhex(17977) -> 249;
unhex(17985) -> 250; unhex(17986) -> 251; unhex(17987) -> 252; unhex(17988) -> 253; unhex(17989) -> 254; unhex(17990) -> 255;
unhex(18017) -> 250; unhex(18018) -> 251; unhex(18019) -> 252; unhex(18020) -> 253; unhex(18021) -> 254; unhex(18022) -> 255;
unhex(24880) -> 160; unhex(24881) -> 161; unhex(24882) -> 162; unhex(24883) -> 163; unhex(24884) -> 164; unhex(24885) -> 165; unhex(24886) -> 166; unhex(24887) -> 167; unhex(24888) -> 168; unhex(24889) -> 169;
unhex(24897) -> 170; unhex(24898) -> 171; unhex(24899) -> 172; unhex(24900) -> 173; unhex(24901) -> 174; unhex(24902) -> 175;
unhex(24929) -> 170; unhex(24930) -> 171; unhex(24931) -> 172; unhex(24932) -> 173; unhex(24933) -> 174; unhex(24934) -> 175;
unhex(25136) -> 176; unhex(25137) -> 177; unhex(25138) -> 178; unhex(25139) -> 179; unhex(25140) -> 180; unhex(25141) -> 181; unhex(25142) -> 182; unhex(25143) -> 183; unhex(25144) -> 184; unhex(25145) -> 185;
unhex(25153) -> 186; unhex(25154) -> 187; unhex(25155) -> 188; unhex(25156) -> 189; unhex(25157) -> 190; unhex(25158) -> 191;
unhex(25185) -> 186; unhex(25186) -> 187; unhex(25187) -> 188; unhex(25188) -> 189; unhex(25189) -> 190; unhex(25190) -> 191;
unhex(25392) -> 192; unhex(25393) -> 193; unhex(25394) -> 194; unhex(25395) -> 195; unhex(25396) -> 196; unhex(25397) -> 197; unhex(25398) -> 198; unhex(25399) -> 199; unhex(25400) -> 200; unhex(25401) -> 201;
unhex(25409) -> 202; unhex(25410) -> 203; unhex(25411) -> 204; unhex(25412) -> 205; unhex(25413) -> 206; unhex(25414) -> 207;
unhex(25441) -> 202; unhex(25442) -> 203; unhex(25443) -> 204; unhex(25444) -> 205; unhex(25445) -> 206; unhex(25446) -> 207;
unhex(25648) -> 208; unhex(25649) -> 209; unhex(25650) -> 210; unhex(25651) -> 211; unhex(25652) -> 212; unhex(25653) -> 213; unhex(25654) -> 214; unhex(25655) -> 215; unhex(25656) -> 216; unhex(25657) -> 217;
unhex(25665) -> 218; unhex(25666) -> 219; unhex(25667) -> 220; unhex(25668) -> 221; unhex(25669) -> 222; unhex(25670) -> 223;
unhex(25697) -> 218; unhex(25698) -> 219; unhex(25699) -> 220; unhex(25700) -> 221; unhex(25701) -> 222; unhex(25702) -> 223;
unhex(25904) -> 224; unhex(25905) -> 225; unhex(25906) -> 226; unhex(25907) -> 227; unhex(25908) -> 228; unhex(25909) -> 229; unhex(25910) -> 230; unhex(25911) -> 231; unhex(25912) -> 232; unhex(25913) -> 233;
unhex(25921) -> 234; unhex(25922) -> 235; unhex(25923) -> 236; unhex(25924) -> 237; unhex(25925) -> 238; unhex(25926) -> 239;
unhex(25953) -> 234; unhex(25954) -> 235; unhex(25955) -> 236; unhex(25956) -> 237; unhex(25957) -> 238; unhex(25958) -> 239;
unhex(26160) -> 240; unhex(26161) -> 241; unhex(26162) -> 242; unhex(26163) -> 243; unhex(26164) -> 244; unhex(26165) -> 245; unhex(26166) -> 246; unhex(26167) -> 247; unhex(26168) -> 248; unhex(26169) -> 249;
unhex(26177) -> 250; unhex(26178) -> 251; unhex(26179) -> 252; unhex(26180) -> 253; unhex(26181) -> 254; unhex(26182) -> 255;
unhex(26209) -> 250; unhex(26210) -> 251; unhex(26211) -> 252; unhex(26212) -> 253; unhex(26213) -> 254; unhex(26214) -> 255;
unhex(Char) ->
    badarg_with_cause([<<Char>>], invalid_hex).

badarg_with_cause(Args, Cause) ->
    erlang:error(badarg, Args, [{error_info, #{module => erl_stdlib_errors,
                                               cause => Cause}}]).
badarg_with_info(Args) ->
    erlang:error(badarg, Args, [{error_info, #{module => erl_stdlib_errors}}]).

error_with_info(Reason, Args) ->
    erlang:error(Reason, Args, [{error_info, #{module => erl_stdlib_errors}}]).
