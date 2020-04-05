%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
-module(erl_posix_msg).

%% Converts from errno identifiers to error messages.

-export([message/1]).

-spec message(atom()) -> string().

message(T) ->
    binary_to_list(message_1(T)).

message_1(e2big) -> <<"argument list too long">>;
message_1(eacces) -> <<"permission denied">>;
message_1(eaddrinuse) -> <<"address already in use">>;
message_1(eaddrnotavail) -> <<"can't assign requested address">>;
message_1(eadv) -> <<"advertise error">>;
message_1(eafnosupport) -> <<"address family not supported by protocol family">>;
message_1(eagain) -> <<"resource temporarily unavailable">>;
message_1(ealign) -> <<"EALIGN">>;
message_1(ealready) -> <<"operation already in progress">>;
message_1(ebade) -> <<"bad exchange descriptor">>;
message_1(ebadf) -> <<"bad file number">>;
message_1(ebadfd) -> <<"file descriptor in bad state">>;
message_1(ebadmsg) -> <<"not a data message">>;
message_1(ebadr) -> <<"bad request descriptor">>;
message_1(ebadrpc) -> <<"RPC structure is bad">>;
message_1(ebadrqc) -> <<"bad request code">>;
message_1(ebadslt) -> <<"invalid slot">>;
message_1(ebfont) -> <<"bad font file format">>;
message_1(ebusy) -> <<"file busy">>;
message_1(echild) -> <<"no children">>;
message_1(echrng) -> <<"channel number out of range">>;
message_1(ecomm) -> <<"communication error on send">>;
message_1(econnaborted) -> <<"software caused connection abort">>;
message_1(econnrefused) -> <<"connection refused">>;
message_1(econnreset) -> <<"connection reset by peer">>;
message_1(edeadlk) -> <<"resource deadlock avoided">>;
message_1(edeadlock) -> <<"resource deadlock avoided">>;
message_1(edestaddrreq) -> <<"destination address required">>;
message_1(edirty) -> <<"mounting a dirty fs w/o force">>;
message_1(edom) -> <<"math argument out of range">>;
message_1(edotdot) -> <<"cross mount point">>;
message_1(edquot) -> <<"disk quota exceeded">>;
message_1(eduppkg) -> <<"duplicate package name">>;
message_1(eexist) -> <<"file already exists">>;
message_1(efault) -> <<"bad address in system call argument">>;
message_1(efbig) -> <<"file too large">>;
message_1(eftype) -> <<"EFTYPE">>;
message_1(ehostdown) -> <<"host is down">>;
message_1(ehostunreach) -> <<"host is unreachable">>;
message_1(eidrm) -> <<"identifier removed">>;
message_1(einit) -> <<"initialization error">>;
message_1(einprogress) -> <<"operation now in progress">>;
message_1(eintr) -> <<"interrupted system call">>;
message_1(einval) -> <<"invalid argument">>;
message_1(eio) -> <<"I/O error">>;
message_1(eisconn) -> <<"socket is already connected">>;
message_1(eisdir) -> <<"illegal operation on a directory">>;
message_1(eisnam) -> <<"is a name file">>;
message_1(elbin) -> <<"ELBIN">>;
message_1(el2hlt) -> <<"level 2 halted">>;
message_1(el2nsync) -> <<"level 2 not synchronized">>;
message_1(el3hlt) -> <<"level 3 halted">>;
message_1(el3rst) -> <<"level 3 reset">>;
message_1(elibacc) -> <<"cannot access a needed shared library">>;
message_1(elibbad) -> <<"accessing a corrupted shared library">>;
message_1(elibexec) -> <<"cannot exec a shared library directly">>;
message_1(elibmax) ->
    <<"attempting to link in more shared libraries than system limit">>;
message_1(elibscn) -> <<".lib section in a.out corrupted">>;
message_1(elnrng) -> <<"link number out of range">>;
message_1(eloop) -> <<"too many levels of symbolic links">>;
message_1(emfile) -> <<"too many open files">>;
message_1(emlink) -> <<"too many links">>;
message_1(emsgsize) -> <<"message too long">>;
message_1(emultihop) -> <<"multihop attempted">>;
message_1(enametoolong) -> <<"file name too long">>;
message_1(enavail) -> <<"not available">>;
message_1(enet) -> <<"ENET">>;
message_1(enetdown) -> <<"network is down">>;
message_1(enetreset) -> <<"network dropped connection on reset">>;
message_1(enetunreach) -> <<"network is unreachable">>;
message_1(enfile) -> <<"file table overflow">>;
message_1(enoano) -> <<"anode table overflow">>;
message_1(enobufs) -> <<"no buffer space available">>;
message_1(enocsi) -> <<"no CSI structure available">>;
message_1(enodata) -> <<"no data available">>;
message_1(enodev) -> <<"no such device">>;
message_1(enoent) -> <<"no such file or directory">>;
message_1(enoexec) -> <<"exec format error">>;
message_1(enolck) -> <<"no locks available">>;
message_1(enolink) -> <<"link has be severed">>;
message_1(enomem) -> <<"not enough memory">>;
message_1(enomsg) -> <<"no message of desired type">>;
message_1(enonet) -> <<"machine is not on the network">>;
message_1(enopkg) -> <<"package not installed">>;
message_1(enoprotoopt) -> <<"bad proocol option">>;
message_1(enospc) -> <<"no space left on device">>;
message_1(enosr) -> <<"out of stream resources or not a stream device">>;
message_1(enostr) -> <<"not a stream">>;
message_1(enosym) -> <<"unresolved symbol name">>;
message_1(enosys) -> <<"function not implemented">>;
message_1(enotblk) -> <<"block device required">>;
message_1(enotconn) -> <<"socket is not connected">>;
message_1(enotdir) -> <<"not a directory">>;
message_1(enotempty) -> <<"directory not empty">>;
message_1(enotnam) -> <<"not a name file">>;
message_1(enotsock) -> <<"socket operation on non-socket">>;
message_1(enotsup) -> <<"operation not supported">>;
message_1(enotty) -> <<"inappropriate device for ioctl">>;
message_1(enotuniq) -> <<"name not unique on network">>;
message_1(enxio) -> <<"no such device or address">>;
message_1(eopnotsupp) -> <<"operation not supported on socket">>;
message_1(eoverflow) -> <<"offset too large for file system">>;
message_1(eperm) -> <<"not owner">>;
message_1(epfnosupport) -> <<"protocol family not supported">>;
message_1(epipe) -> <<"broken pipe">>;
message_1(eproclim) -> <<"too many processes">>;
message_1(eprocunavail) -> <<"bad procedure for program">>;
message_1(eprogmismatch) -> <<"program version wrong">>;
message_1(eprogunavail) -> <<"RPC program not available">>;
message_1(eproto) -> <<"protocol error">>;
message_1(eprotonosupport) -> <<"protocol not suppored">>;
message_1(eprototype) -> <<"protocol wrong type for socket">>;
message_1(erange) -> <<"math result unrepresentable">>;
message_1(erefused) -> <<"EREFUSED">>;
message_1(eremchg) -> <<"remote address changed">>;
message_1(eremdev) -> <<"remote device">>;
message_1(eremote) -> <<"pathname hit remote file system">>;
message_1(eremoteio) -> <<"remote i/o error">>;
message_1(eremoterelease) -> <<"EREMOTERELEASE">>;
message_1(erofs) -> <<"read-only file system">>;
message_1(erpcmismatch) -> <<"RPC version is wrong">>;
message_1(erremote) -> <<"object is remote">>;
message_1(eshutdown) -> <<"can't send after socket shutdown">>;
message_1(esocktnosupport) -> <<"socket type not supported">>;
message_1(espipe) -> <<"invalid seek">>;
message_1(esrch) -> <<"no such process">>;
message_1(esrmnt) -> <<"srmount error">>;
message_1(estale) -> <<"stale remote file handle">>;
message_1(esuccess) -> <<"Error 0">>;
message_1(etime) -> <<"timer expired">>;
message_1(etimedout) -> <<"connection timed out">>;
message_1(etoomanyrefs) -> <<"too many references: can't splice">>;
message_1(etxtbsy) -> <<"text file or pseudo-device busy">>;
message_1(euclean) -> <<"structure needs cleaning">>;
message_1(eunatch) -> <<"protocol driver not attached">>;
message_1(eusers) -> <<"too many users">>;
message_1(eversion) -> <<"version mismatch">>;
message_1(ewouldblock) -> <<"operation would block">>;
message_1(exdev) -> <<"cross-domain link">>;
message_1(exfull) -> <<"message tables full">>;
message_1(nxdomain) -> <<"non-existing domain">>;
message_1(exbadport) -> <<"inet_drv bad port state">>;
message_1(exbadseq) -> <<"inet_drv bad request sequence">>;
message_1(_) -> <<"unknown POSIX error">>.
