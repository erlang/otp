%% -*- erlang-indent-level: 2 -*-
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Provides abstract datatypes for HiPE's RTL (Register Transfer Language).
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------

-record(alu, {dst, src1, op, src2}).
-record(alub, {dst, src1, op, src2, 'cond', true_label, false_label, p}).
-record(call, {dstlist, 'fun', arglist, type, continuation,
    failcontinuation, normalcontinuation = []}).
-record(comment, {text}).
-record(enter, {'fun', arglist, type}).
-record(fconv, {dst, src}).
-record(fixnumop, {dst, src, type}).
-record(fload, {dst, src, offset}).
-record(fmove, {dst, src}).
-record(fp, {dst, src1, op, src2}).
-record(fp_unop, {dst, src, op}).
-record(fstore, {base, offset, src}).
-record(gctest, {words}).
-record(goto, {label}).
-record(goto_index, {block, index, labels}).
-record(label, {name}).
-record(load, {dst, src, offset, size, sign}).
-record(load_address, {dst, addr, type}).
-record(load_atom, {dst, atom}).
-record(load_word_index, {dst, block, index}).
-record(move, {dst, src}).
-record(multimove, {dstlist, srclist}).
-record(phi, {dst, id, arglist}).
-record(return, {varlist}).
-record(store, {base, offset, src, size}).
-record(switch, {src, labels, sorted_by=[]}).

%%---------------------------------------------------------------------

%% An efficient macro to convert byte sizes to bit sizes
-define(bytes_to_bits(Bytes), ((Bytes) bsl 3)).  % (N * 8)

%%---------------------------------------------------------------------
