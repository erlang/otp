%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

-module(literals).
-export([a/0,b/0,huge_bignum/0,funs/0,
         binary/0,unused_binaries/0,bits/0]).
-export([msg1/0,msg2/0,msg3/0,msg4/0,msg5/0]).

a() ->
    {a,42.0,[7,38877938333399637266518333334747]}.

b() ->
    [{init,get_flag,1},
     {init,get_flags,0},
     {init,get_args,0},
     {string,re_sh_to_awk,1},
     {string,re_parse,1},
     {string,re_match,2},
     {string,re_sub,3},
     {string,re_gsub,3},
     {string,re_split,2},
     {string,index,2},
     {erl_eval,seq,2},
     {erl_eval,seq,3},
     {erl_eval,arg_list,2},
     {erl_eval,arg_list,3},
     {erl_pp,seq,1},
     {erl_pp,seq,2},
     {io,scan_erl_seq,1},
     {io,scan_erl_seq,2},
     {io,scan_erl_seq,3},
     {io,parse_erl_seq,1},
     {io,parse_erl_seq,2},
     {io,parse_erl_seq,3},
     {io,parse_exprs,2},
     {io_lib,scan,1},
     {io_lib,scan,2},
     {io_lib,scan,3},
     {io_lib,reserved_word,1},
     {lists,keymap,4},
     {lists,all,3},
     {lists,any,3},
     {lists,map,3},
     {lists,flatmap,3},
     {lists,foldl,4},
     {lists,foldr,4},
     {lists,mapfoldl,4},
     {lists,mapfoldr,4},
     {lists,filter,3},
     {lists,foreach,3},
     {erlang,old_binary_to_term,1},
     {erlang,info,1},
     {file,file_info,1},
     {dict,dict_to_list,1},
     {dict,list_to_dict,1},
     {orddict,dict_to_list,1},
     {orddict,list_to_dict,1},
     {sets,new_set,0},
     {sets,set_to_list,1},
     {sets,list_to_set,1},
     {sets,subset,2},
     {ordsets,new_set,0},
     {ordsets,set_to_list,1},
     {ordsets,list_to_set,1},
     {ordsets,subset,2},
     {calendar,local_time_to_universal_time,1}].

huge_bignum() ->
    36#9987333333392789234879423987243987423432879423879234897423879423874328794323248423872348742323487423987423879243872347824374238792437842374283926276478623462342363243SDKJFSDLEFHDSHJFE48H3838973879JFSDKJLFASLKJVBJKLEJKLDYEIOEHFEOU39873487SFHJSLDFASUIDFHSDHFEYR0R987YDFHDHFDLKHFSIDFHSIDFSIFDHSIFHWIHR07373767667987769707660766789076874238792437842374283926276478623462342363243SDKJFSDLEFHDSHJFE48H3838973879JFSDKJLFASLKJVBJKLEJKLDYEIOEHFEOU39873487SFHJSLDFASUIDFHSDHFEYR0R987YDFHDHFDLKHFSIDFHSIDFSIFDHSIFHWIHR0737376766798779987333333392789234879423987243987423432879423879234897423879423874328794323248423872348742323487423987423879243872347824374238792437842374283926276478623462342363243SDKJFSDLEFHDSHJFE48H3838973879JFSDKJLFASLKJVBJKLEJKLDYEIOEHFEOU39873487SFHJSLDFASUIDFHSDHFEYR0R987YDFHDHFDLKHFSIDFHSIDFSIFDHSIFHWIHR07373767667987769707660766789076874238792437842374283926276478623462342363243SDKJFSDLEFHDSHJFE48H3838973879JFSDKJLFASLKJVBJKLEJKLDYEIOEHFEOU39873487SFHJSLDFASUIDFHSDHFEYR0R987YDFHDHFDLKHFSIDFHSIDFSIFDHSIFHWIHR07373767667987779JFSDKJLFASLKJVBJKLEJKLDYEIOEHFEOU39873487SFHJSLDFASUIDFHSDHFEYR0R987YDFHDHFDLKHFSIDFHSIDFSIFDHSIFHWIHR07373767667987769707660766789076874238792437842374283926276478623462342363243SDKJFSDLEFHDSHJFE48H3838973879JFSDKJLFASLKJVBJKLEJKLDYEIOEHFEOU39873487SFHJSLDFASUIDFHSDHFEYR0R987YDFHDHFDLKHFSIDFHSIDFSIFDHSIFHWIHR0737376766798779987333333392789234879423987243987423432879423879234897423879423874328794323248423872348742323487423987423879243872347824374238792437842374283926276478623462342363243SDKJFSDLEFHDSHJFE48H3838973879JFSDKJLFASLKJVBJKLEJKLDYEIOEHFEOU39873487SFHJSLDFASUIDFHSDHFEYR0R987YDFHDHFDLKHFSIDFHSIDFSIFDHSIFHWIHR07373767667987769707660766789076874238792437842374283926276478623462342363243SDKJFSDLEFHDSHJFE48H3838973879JFSDKJLFASLKJVBJKLEJKLDYEIOEHFEOU39873487SFHJSLDFASUIDFHSDHFEYR0R987YDFHDHFDLKHFSIDFHSIDFSIFDHSIFHWIHR073737676679877.

-define(TIMES_FOUR(X), X,X,X,X).
-define(BYTES_256, 0:256,1:256,2:256,3:256, 4:256,5:256,6:256,7:256).
-define(KB_1, ?TIMES_FOUR(?BYTES_256)).
-define(KB_4, ?TIMES_FOUR(?KB_1)).
-define(KB_16, ?TIMES_FOUR(?KB_4)).
-define(KB_64, ?TIMES_FOUR(?KB_16)).
-define(KB_128, ?TIMES_FOUR(?KB_64)).
-define(MB_1, ?TIMES_FOUR(?KB_128)).

binary() ->
    %% Too big to be a heap binary.
    <<?MB_1>>.

unused_binaries() ->
    {<<?KB_128>>,<<?BYTES_256>>}.

bits() ->
    {bits,<<42:13,?MB_1>>}.

msg1() -> "halloj".
msg2() -> {"hello","world"}.
msg3() -> <<"halloj">>.
msg4() -> #{ 1=> "hello", b => "world"}.
msg5() -> {1,2,3,4,5,6}.

funs() ->
    %% Literal funs (in a non-literal list).
    [fun ?MODULE:a/0,
     fun() -> ok end].                          %No environment.
