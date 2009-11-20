%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(erl_posix_msg).

%% Converts from errno identifiers to error messages.

-export([message/1]).

-spec message(atom()) -> string().

message(e2big) -> "argument list too long";
message(eacces) -> "permission denied";
message(eaddrinuse) -> "address already in use";
message(eaddrnotavail) -> "can't assign requested address";
message(eadv) -> "advertise error";
message(eafnosupport) -> "address family not supported by protocol family";
message(eagain) -> "resource temporarily unavailable";
message(ealign) -> "EALIGN";
message(ealready) -> "operation already in progress";
message(ebade) -> "bad exchange descriptor";
message(ebadf) -> "bad file number";
message(ebadfd) -> "file descriptor in bad state";
message(ebadmsg) -> "not a data message";
message(ebadr) -> "bad request descriptor";
message(ebadrpc) -> "RPC structure is bad";
message(ebadrqc) -> "bad request code";
message(ebadslt) -> "invalid slot";
message(ebfont) -> "bad font file format";
message(ebusy) -> "file busy";
message(echild) -> "no children";
message(echrng) -> "channel number out of range";
message(ecomm) -> "communication error on send";
message(econnaborted) -> "software caused connection abort";
message(econnrefused) -> "connection refused";
message(econnreset) -> "connection reset by peer";
message(edeadlk) -> "resource deadlock avoided";
message(edeadlock) -> "resource deadlock avoided";
message(edestaddrreq) -> "destination address required";
message(edirty) -> "mounting a dirty fs w/o force";
message(edom) -> "math argument out of range";
message(edotdot) -> "cross mount point";
message(edquot) -> "disk quota exceeded";
message(eduppkg) -> "duplicate package name";
message(eexist) -> "file already exists";
message(efault) -> "bad address in system call argument";
message(efbig) -> "file too large";
message(ehostdown) -> "host is down";
message(ehostunreach) -> "host is unreachable";
message(eidrm) -> "identifier removed";
message(einit) -> "initialization error";
message(einprogress) -> "operation now in progress";
message(eintr) -> "interrupted system call";
message(einval) -> "invalid argument";
message(eio) -> "I/O error";
message(eisconn) -> "socket is already connected";
message(eisdir) -> "illegal operation on a directory";
message(eisnam) -> "is a name file";
message(elbin) -> "ELBIN";
message(el2hlt) -> "level 2 halted";
message(el2nsync) -> "level 2 not synchronized";
message(el3hlt) -> "level 3 halted";
message(el3rst) -> "level 3 reset";
message(elibacc) -> "can not access a needed shared library";
message(elibbad) -> "accessing a corrupted shared library";
message(elibexec) -> "can not exec a shared library directly";
message(elibmax) ->
    "attempting to link in more shared libraries than system limit";
message(elibscn) -> ".lib section in a.out corrupted";
message(elnrng) -> "link number out of range";
message(eloop) -> "too many levels of symbolic links";
message(emfile) -> "too many open files";
message(emlink) -> "too many links";
message(emsgsize) -> "message too long";
message(emultihop) -> "multihop attempted";
message(enametoolong) -> "file name too long";
message(enavail) -> "not available";
message(enet) -> "ENET";
message(enetdown) -> "network is down";
message(enetreset) -> "network dropped connection on reset";
message(enetunreach) -> "network is unreachable";
message(enfile) -> "file table overflow";
message(enoano) -> "anode table overflow";
message(enobufs) -> "no buffer space available";
message(enocsi) -> "no CSI structure available";
message(enodata) -> "no data available";
message(enodev) -> "no such device";
message(enoent) -> "no such file or directory";
message(enoexec) -> "exec format error";
message(enolck) -> "no locks available";
message(enolink) -> "link has be severed";
message(enomem) -> "not enough memory";
message(enomsg) -> "no message of desired type";
message(enonet) -> "machine is not on the network";
message(enopkg) -> "package not installed";
message(enoprotoopt) -> "bad proocol option";
message(enospc) -> "no space left on device";
message(enosr) -> "out of stream resources or not a stream device";
message(enosym) -> "unresolved symbol name";
message(enosys) -> "function not implemented";
message(enotblk) -> "block device required";
message(enotconn) -> "socket is not connected";
message(enotdir) -> "not a directory";
message(enotempty) -> "directory not empty";
message(enotnam) -> "not a name file";
message(enotsock) -> "socket operation on non-socket";
message(enotsup) -> "operation not supported";
message(enotty) -> "inappropriate device for ioctl";
message(enotuniq) -> "name not unique on network";
message(enxio) -> "no such device or address";
message(eopnotsupp) -> "operation not supported on socket";
message(eperm) -> "not owner";
message(epfnosupport) -> "protocol family not supported";
message(epipe) -> "broken pipe";
message(eproclim) -> "too many processes";
message(eprocunavail) -> "bad procedure for program";
message(eprogmismatch) -> "program version wrong";
message(eprogunavail) -> "RPC program not available";
message(eproto) -> "protocol error";
message(eprotonosupport) -> "protocol not suppored";
message(eprototype) -> "protocol wrong type for socket";
message(erange) -> "math result unrepresentable";
message(erefused) -> "EREFUSED";
message(eremchg) -> "remote address changed";
message(eremdev) -> "remote device";
message(eremote) -> "pathname hit remote file system";
message(eremoteio) -> "remote i/o error";
message(eremoterelease) -> "EREMOTERELEASE";
message(erofs) -> "read-only file system";
message(erpcmismatch) -> "RPC version is wrong";
message(erremote) -> "object is remote";
message(eshutdown) -> "can't send after socket shutdown";
message(esocktnosupport) -> "socket type not supported";
message(espipe) -> "invalid seek";
message(esrch) -> "no such process";
message(esrmnt) -> "srmount error";
message(estale) -> "stale remote file handle";
message(esuccess) -> "Error 0";
message(etime) -> "timer expired";
message(etimedout) -> "connection timed out";
message(etoomanyrefs) -> "too many references: can't splice";
message(etxtbsy) -> "text file or pseudo-device busy";
message(euclean) -> "structure needs cleaning";
message(eunatch) -> "protocol driver not attached";
message(eusers) -> "too many users";
message(eversion) -> "version mismatch";
message(ewouldblock) -> "operation would block";
message(exdev) -> "cross-domain link";
message(exfull) -> "message tables full";
message(nxdomain) -> "non-existing domain";
message(_) -> "unknown POSIX error".
