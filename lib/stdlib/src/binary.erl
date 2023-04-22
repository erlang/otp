%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2023. All Rights Reserved.
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
-export([replace/3, replace/4,
         encode_hex/1, encode_hex/2, decode_hex/1]).

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
-compile({inline, [hex/2]}).
-spec encode_hex(Bin) -> Bin2 when
      Bin :: binary(),
      Bin2 :: <<_:_*16>>.
encode_hex(Bin) when is_binary(Bin) ->
    encode_hex(Bin, uppercase);
encode_hex(Bin) ->
    error_with_info(badarg, [Bin]).

-spec encode_hex(Bin, Case) -> Bin2 when
      Bin :: binary(),
      Case :: lowercase | uppercase,
      Bin2 :: <<_:_*16>>.
encode_hex(Bin, uppercase) when is_binary(Bin) ->
    encode_hex1(Bin, 1);
encode_hex(Bin, lowercase) when is_binary(Bin) ->
    encode_hex1(Bin, 257);
encode_hex(Bin, Case) ->
    error_with_info(badarg, [Bin, Case]).

encode_hex1(Data, Offset) ->
    <<First:(bit_size(Data) div 64)/binary-unit:64, Rest/binary>> = Data,
    Hex = << <<(hex(A, Offset)):16, (hex(B, Offset)):16, (hex(C, Offset)):16, (hex(D, Offset)):16,
               (hex(E, Offset)):16, (hex(F, Offset)):16, (hex(G, Offset)):16, (hex(H, Offset)):16>> ||
              <<A,B,C,D,E,F,G,H>> <= First >>,
    encode_hex2(Rest, Offset, Hex).

encode_hex2(<<A,Data/binary>>, Offset, Acc) ->
    encode_hex2(Data, Offset, <<Acc/binary, (hex(A, Offset)):16>>);
encode_hex2(<<>>, _Offset, Acc) ->
    Acc.

hex(X, Offset) ->
    element(
      X + Offset, {
                   %% Used for Uppercase
                   16#3030, 16#3031, 16#3032, 16#3033, 16#3034, 16#3035, 16#3036, 16#3037, 16#3038, 16#3039, 16#3041, 16#3042, 16#3043, 16#3044, 16#3045, 16#3046,
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
                   16#4630, 16#4631, 16#4632, 16#4633, 16#4634, 16#4635, 16#4636, 16#4637, 16#4638, 16#4639, 16#4641, 16#4642, 16#4643, 16#4644, 16#4645, 16#4646,
                   %% Used for Lowercase 
                   16#3030, 16#3031, 16#3032, 16#3033, 16#3034, 16#3035, 16#3036, 16#3037, 16#3038, 16#3039, 16#3061, 16#3062, 16#3063, 16#3064, 16#3065, 16#3066,
                   16#3130, 16#3131, 16#3132, 16#3133, 16#3134, 16#3135, 16#3136, 16#3137, 16#3138, 16#3139, 16#3161, 16#3162, 16#3163, 16#3164, 16#3165, 16#3166,
                   16#3230, 16#3231, 16#3232, 16#3233, 16#3234, 16#3235, 16#3236, 16#3237, 16#3238, 16#3239, 16#3261, 16#3262, 16#3263, 16#3264, 16#3265, 16#3266,
                   16#3330, 16#3331, 16#3332, 16#3333, 16#3334, 16#3335, 16#3336, 16#3337, 16#3338, 16#3339, 16#3361, 16#3362, 16#3363, 16#3364, 16#3365, 16#3366,
                   16#3430, 16#3431, 16#3432, 16#3433, 16#3434, 16#3435, 16#3436, 16#3437, 16#3438, 16#3439, 16#3461, 16#3462, 16#3463, 16#3464, 16#3465, 16#3466,
                   16#3530, 16#3531, 16#3532, 16#3533, 16#3534, 16#3535, 16#3536, 16#3537, 16#3538, 16#3539, 16#3561, 16#3562, 16#3563, 16#3564, 16#3565, 16#3566,
                   16#3630, 16#3631, 16#3632, 16#3633, 16#3634, 16#3635, 16#3636, 16#3637, 16#3638, 16#3639, 16#3661, 16#3662, 16#3663, 16#3664, 16#3665, 16#3666,
                   16#3730, 16#3731, 16#3732, 16#3733, 16#3734, 16#3735, 16#3736, 16#3737, 16#3738, 16#3739, 16#3761, 16#3762, 16#3763, 16#3764, 16#3765, 16#3766,
                   16#3830, 16#3831, 16#3832, 16#3833, 16#3834, 16#3835, 16#3836, 16#3837, 16#3838, 16#3839, 16#3861, 16#3862, 16#3863, 16#3864, 16#3865, 16#3866,
                   16#3930, 16#3931, 16#3932, 16#3933, 16#3934, 16#3935, 16#3936, 16#3937, 16#3938, 16#3939, 16#3961, 16#3962, 16#3963, 16#3964, 16#3965, 16#3966,
                   16#6130, 16#6131, 16#6132, 16#6133, 16#6134, 16#6135, 16#6136, 16#6137, 16#6138, 16#6139, 16#6161, 16#6162, 16#6163, 16#6164, 16#6165, 16#6166,
                   16#6230, 16#6231, 16#6232, 16#6233, 16#6234, 16#6235, 16#6236, 16#6237, 16#6238, 16#6239, 16#6261, 16#6262, 16#6263, 16#6264, 16#6265, 16#6266,
                   16#6330, 16#6331, 16#6332, 16#6333, 16#6334, 16#6335, 16#6336, 16#6337, 16#6338, 16#6339, 16#6361, 16#6362, 16#6363, 16#6364, 16#6365, 16#6366,
                   16#6430, 16#6431, 16#6432, 16#6433, 16#6434, 16#6435, 16#6436, 16#6437, 16#6438, 16#6439, 16#6461, 16#6462, 16#6463, 16#6464, 16#6465, 16#6466,
                   16#6530, 16#6531, 16#6532, 16#6533, 16#6534, 16#6535, 16#6536, 16#6537, 16#6538, 16#6539, 16#6561, 16#6562, 16#6563, 16#6564, 16#6565, 16#6566,
                   16#6630, 16#6631, 16#6632, 16#6633, 16#6634, 16#6635, 16#6636, 16#6637, 16#6638, 16#6639, 16#6661, 16#6662, 16#6663, 16#6664, 16#6665, 16#6666}).

-compile({inline, [unhex/1]}).
-spec decode_hex(Bin) -> Bin2 when
      Bin :: <<_:_*16>>,
      Bin2 :: binary().
decode_hex(Data) when byte_size(Data) rem 2 =:= 0 ->
    try
        decode_hex1(Data)
    catch
        error:badarg ->
            badarg_with_info([Data])
    end;
decode_hex(Data) ->
    badarg_with_info([Data]).

decode_hex1(Data) ->
    <<First:(byte_size(Data) div 8)/binary-unit:64, Rest/binary>> = Data,
    Bin = << <<(unhex(A)):4, (unhex(B)):4, (unhex(C)):4, (unhex(D)):4,
               (unhex(E)):4, (unhex(F)):4, (unhex(G)):4, (unhex(H)):4>> ||
              <<A,B,C,D,E,F,G,H>> <= First >>,
    decode_hex2(Rest, Bin).

decode_hex2(<<A,Data/binary>>, Acc) ->
    decode_hex2(Data, <<Acc/binary-unit:4, (unhex(A)):4>>);
decode_hex2(<<>>, Acc) ->
    Acc.

unhex(X) ->
    element(X,
            {nonono, no, no, no, no, no, no, no, no, no, no, no, no, no, no, %1
             no, no, no, no, no, no, no, no, no, no, no, no, no, no, no, no, %16
             no, no, no, no, no, no, no, no, no, no, no, no, no, no, no, no, %32
              0,  1,  2,  3,  4,  5,  6,  7,  8,  9, no, no, no, no, no, no, %48
             no, 10, 11, 12, 13, 14, 15, no, no, no, no, no, no, no, no, no, %64
             no, no, no, no, no, no, no, no, no, no, no, no, no, no, no, no, %80
             no, 10, 11, 12, 13, 14, 15, no, no, no, no, no, no, no, no, no  %96
            }).

badarg_with_cause(Args, Cause) ->
    erlang:error(badarg, Args, [{error_info, #{module => erl_stdlib_errors,
                                               cause => Cause}}]).
badarg_with_info(Args) ->
    erlang:error(badarg, Args, [{error_info, #{module => erl_stdlib_errors}}]).

error_with_info(Reason, Args) ->
    erlang:error(Reason, Args, [{error_info, #{module => erl_stdlib_errors}}]).
