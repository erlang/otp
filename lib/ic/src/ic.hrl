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


%%------------------------------------------------------------
%% Configuration macros
-define(CORBAMOD, corba).
-define(ORBNAME, orber).
-define(CORBAHRL, "corba.hrl").
-define(CALL, "call").
-define(CAST, "cast").
-define(IFRREGID, "register").
-define(IFRTYPESHRL, "ifr_types.hrl").

-define(GENSERVMOD, gen_server).

%%------------------------------------------------------------
%% Flags. NOTE! Once assigned  value may NOT be changed. Deprecate ok.
%% Default flags. Can be changed if we change the default behavior.
-define(IC_FLAG_TEMPLATE_1, 16#01).
-define(IC_FLAG_TEMPLATE_2, 16#02).

-define(IC_INIT_FLAGS, 16#00).

%% Flag operations
%% USAGE: Boolean = ?IC_FLAG_TEST(Flags, ?IC_ATTRIBUTE)
-define(IC_FLAG_TEST(_F1, _I1),   ((_F1 band _I1) == _I1)).

%% USAGE: NewFlags = ?IC_SET_TRUE(Flags, ?IC_ATTRIBUTE)
-define(IC_SET_TRUE(_F2, _I2),    (_I2 bor _F2)).

%% USAGE: NewFlags = ?IC_SET_FALSE(Flags, ?IC_ATTRIBUTE)
-define(IC_SET_FALSE(_F3, _I3),   ((_I3 bxor 16#ff) band _F3)).

%% USAGE: NewFlags = ?IC_SET_FALSE_LIST(Flags, [?IC_SEC_ATTRIBUTE, ?IC_SOME])
-define(IC_SET_FALSE_LIST(_F4, _IList1),
        lists:foldl(fun(_I4, _F5) ->
                            ((_I4 bxor 16#ff) band _F5)
                    end, 
                    _F4, _IList1)).

%% USAGE: NewFlags = ?IC_SET_TRUE_LIST(Flags, [?IC_ATTRIBUTE, ?IC_SOME])
-define(IC_SET_TRUE_LIST(_F6, _IList2),
        lists:foldl(fun(_I6, _F7) ->
                            (_I6 bor _F7)
                    end, 
                    _F6, _IList2)).

%% USAGE: Boolean = ?IC_FLAG_TEST_LIST(Flags, [?IC_CONTEXT, ?IC_THING])
-define(IC_FLAG_TEST_LIST(_F8, _IList3),
        lists:all(fun(_I7) ->
                          ((_F8 band _I7) == _I7)
                  end,
                  _IList3)).


%%------------------------------------------------------------
%% Usefull macros

-define(ifthen(P,ACTION), if P -> ACTION; true->true end).


%%------------------------------------------------------------
%% Option macros

-define(ifopt(G,OPT,ACTION), 
	case ic_options:get_opt(G,OPT) of true -> ACTION; _ -> ok end).

-define(ifopt2(G,OPT,ACT1,ACT2), 
	case ic_options:get_opt(G,OPT) of true -> ACT1; _ -> ACT2 end).

-define(ifnopt(G,OPT,ACTION), 
	case ic_options:get_opt(G,OPT) of false -> ACTION; _ -> ok end).


%% Internal record
-record(id_of, {id, type, tk}).

%%--------------------------------------------------------------------
%% The generator object definition

-record(genobj, {symtab, impl, options, warnings, auxtab,
		 tktab, pragmatab, c_typedeftab,
		 skelfile=[], skelfiled=[], skelscope=[],
		 stubfile=[], stubfiled=[], stubscope=[],
		 includefile=[], includefiled=[],
		 interfacefile=[],interfacefiled=[],
		 helperfile=[],helperfiled=[],
		 holderfile=[],holderfiled=[], 
		 filestack=0, do_gen=true, sysfile=false}).

%%--------------------------------------------------------------------
%% The scooped id definition
-record(scoped_id,	{type=local, line=-1, id=""}).








%%--------------------------------------------------------------------
%% Secret macros
%%
%%	NOTE these macros are not general, they cannot be used
%%	everywhere.
%%
-define(lookup(T,K), case ets:lookup(T, K) of [{_X, _Y}] -> _Y; _->[] end).
-define(insert(T,K,V), ets:insert(T, {K, V})).


%%---------------------------------------------------------------------
%%
%% Java specific macros
%%
%%
-define(ERLANGPACKAGE,"com.ericsson.otp.erlang.").
-define(ICPACKAGE,"com.ericsson.otp.ic.").


%%
%% Macros for reporting encode/decode errors in C back-ends.
%%
%%

-define(emit_c_enc_rpt(Fd, Fill, Fmt, Vals), 
	begin
	    CType = ic_cbe:mk_c_type2(G, N, T),
	    ic_codegen:emit_c_enc_rpt(Fd, Fill, "~s : " ++ Fmt, [CType| Vals])
	end).
-define(emit_c_dec_rpt(Fd, Fill, Fmt, Vals), 
	begin
	    CType = ic_cbe:mk_c_type2(G, N, T),
	    ic_codegen:emit_c_dec_rpt(Fd, Fill, "~s : " ++ Fmt, [CType| Vals])
	end).





