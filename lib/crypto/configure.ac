dnl Process this file with autoconf to produce a configure script. -*-Autoconf-*-
dnl
dnl %CopyrightBegin%
dnl
dnl Copyright Ericsson AB 2018-2025. All Rights Reserved.
dnl
dnl Licensed under the Apache License, Version 2.0 (the "License");
dnl you may not use this file except in compliance with the License.
dnl You may obtain a copy of the License at
dnl
dnl     http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl Unless required by applicable law or agreed to in writing, software
dnl distributed under the License is distributed on an "AS IS" BASIS,
dnl WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
dnl See the License for the specific language governing permissions and
dnl limitations under the License.
dnl
dnl %CopyrightEnd%
dnl

dnl define([AC_CACHE_LOAD], )dnl
dnl define([AC_CACHE_SAVE], )dnl

AC_INIT
AC_CONFIG_SRCDIR([vsn.mk])
AC_PREREQ([2.72])

m4_include([otp.m4])

AC_CONFIG_AUX_DIR([${ERL_TOP}/make/autoconf])

ERL_PUSH_WERROR

ERL_CANONICAL_SYSTEM_TYPE

AC_LANG(C)

LM_PRECIOUS_VARS

## Delete previous failed configure results
if test -f ./CONF_INFO; then
   rm ./CONF_INFO
fi

if test "$cross_compiling" = "yes"; then
    CROSS_COMPILING=yes
else
    CROSS_COMPILING=no
fi
AC_SUBST(CROSS_COMPILING)

ERL_XCOMP_SYSROOT_INIT

AC_PROG_CC
LM_PROG_LD
LM_WINDOWS_ENVIRONMENT

ERL_DED

AC_MSG_CHECKING([for multiarch directory])
multiarch_dir=
if test "$GCC" = "yes"; then
   multiarch_dir=`$CC $CFLAGS -print-multiarch 2>/dev/null`
   if test $? -ne 0; then
     multiarch_dir=
   fi
fi
if test "$multiarch_dir" = ""; then
   AC_MSG_RESULT([not found])
else
   AC_MSG_RESULT([$multiarch_dir])
fi

dnl
dnl SSL, SSH and CRYPTO need the OpenSSL libraries
dnl
dnl Check flags --with-ssl, --without-ssl --with-ssl=PATH.
dnl If no option is given or --with-ssl is set without a path then we
dnl search for OpenSSL libraries and header files in the standard locations. 
dnl If set to --without-ssl we disable the use of SSL, SSH and CRYPTO.
dnl If set to --with-ssl=PATH we use that path as the prefix, i.e. we
dnl use "PATH/include" and "PATH/lib".

AC_CHECK_SIZEOF(void *)

std_ssl_locations="\
/usr/local \
/usr/sfw \
/usr \
/opt/local \
/usr/pkg \
/usr/local/openssl \
/usr/local/opt/openssl \
/opt/homebrew/opt/openssl \
/usr/lib/openssl \
/usr/openssl \
/usr/local/ssl \
/usr/lib/ssl \
/usr/ssl \
/"
			
AC_ARG_WITH(ssl,
AS_HELP_STRING([--with-ssl=PATH], [base location of OpenSSL include and lib directories])
AS_HELP_STRING([--with-ssl], [use SSL (default)])
AS_HELP_STRING([--without-ssl], [don't use SSL]))

AC_ARG_WITH(ssl-lib-subdir,
AS_HELP_STRING([--with-ssl-lib-subdir=RELATIVE_PATH],
	       [specify extra OpenSSL lib sub-directory to search in (relative to base directory)]),
[
case "$with_ssl_lib_subdir" in
     yes|no)
	with_ssl_lib_subdir=
	;;
     *)
	;;
esac
],
[with_ssl_lib_subdir=]) #default

AC_ARG_WITH(ssl-incl,
AS_HELP_STRING([--with-ssl-incl=PATH],
	       [base location of OpenSSL include dir (if different than base location specified by --with-ssl=PATH)]),
[
case X$with_ssl in
    X | Xyes | Xno) AC_MSG_ERROR([--with-ssl-incl=PATH set without --with-ssl=PATH]);;
esac
],
[with_ssl_incl=$with_ssl]) #default

AC_ARG_WITH(ssl-zlib,
AS_HELP_STRING([--with-ssl-zlib=PATH], [Path to static zlib library to link the
    				       crypto NIF with. This zlib library is most
				       often not necessary but might be needed in
				       order to link the NIF in some cases.]),
[], [with_ssl_zlib=default]) #default

AC_ARG_WITH(ssl-rpath,
AS_HELP_STRING([--with-ssl-rpath=yes|no|PATHS],
               [runtime library path for OpenSSL. Default is "yes", which equates to a
	       number of standard locations. If "no", then no runtime
	       library paths will be used. Anything else should be a
	       comma or colon separated list of paths.]),
[
case X$with_ssl in
    Xno) AC_MSG_ERROR([--with-ssl-rpath set without --with-ssl]);;
esac
],
[with_ssl_rpath=default]) #default


AC_ARG_ENABLE(dynamic-ssl-lib,
AS_HELP_STRING([--enable-dynamic-ssl-lib],
               [enable using dynamic openssl libraries when linking the crypto NIF])
AS_HELP_STRING([--disable-dynamic-ssl-lib],
               [disable using dynamic openssl libraries when linking the crypto NIF]),
[ case "$enableval" in
    no) enable_dynamic_ssl=no ;;
    *)  enable_dynamic_ssl=yes ;;
  esac ], enable_dynamic_ssl=undefined)

AC_ARG_ENABLE(evp-dh,
AS_HELP_STRING([--disable-evp-dh],
	       [intentionally undocumented workaround]),
[ case "$enableval" in
    no)   DISABLE_EVP_DH=1;;
    *)    DISABLE_EVP_DH=0;;
  esac ], DISABLE_EVP_DH=0)


AC_ARG_ENABLE(evp-hmac,
AS_HELP_STRING([--disable-evp-hmac],
	       [intentionally undocumented workaround]),
[ case "$enableval" in
    no)   DISABLE_EVP_HMAC=1;;
    *)    DISABLE_EVP_HMAC=0;;
  esac ], DISABLE_EVP_HMAC=0)

AC_ARG_ENABLE(otp-test-engine,
AS_HELP_STRING([--disable-otp-test-engine],
       [Disable build of the otp_test_engine. (default is --enable-otp-test-engine, unless for LibreSSL >= 3.5.0 where default is --disable-otp-test-engine)]),
[ case "$enableval" in
    no)   DISABLE_OTP_TEST_ENGINE=yes;;
    *)    DISABLE_OTP_TEST_ENGINE=no;;
  esac ], DISABLE_OTP_TEST_ENGINE=default)

AC_DEFUN([ERL_LINK_CRYPTO_IFELSE], [
    test_cflags=$1
    test_ldflags=$2
    test_libs=$3

    saveCFLAGS="$CFLAGS"
    saveLDFLAGS="$LDFLAGS"
    saveLIBS="$LIBS"
    CFLAGS="$DED_BASIC_CFLAGS $test_cflags"
    LDFLAGS="$DED_LDFLAGS_CONFTEST $test_ldflags"
    LIBS="$LIBS $test_libs"

    AC_LINK_IFELSE(
	[AC_LANG_PROGRAM(
	    [[
		#include <stdio.h>
		#include <openssl/hmac.h>
	    ]],
	    [[ 
		HMAC(0, 0, 0, 0, 0, 0, 0);
	    ]])],
	[
	    $4
	],
	[
	    $5
	])

    CFLAGS="$saveCFLAGS"
    LDFLAGS="$saveLDFLAGS"
    LIBS="$saveLIBS"
])

AC_DEFUN([ERL_CRYPTO_CHECK_LIB], [
    # Check for static and dynamic crypto library in the directory
    # given as argument...
    # 
    # Written as a macro in order to avoid writing the tests twice...
    idir=$1
    prfx=$2
    rdir=$3
    dir="$prfx$rdir"
    static_crypto_lib_name=crypto
    static_ssl_lib_name=ssl
    static_extra_libs="$forced_static_zlib"
    static_lib_dir=
    dynamic_crypto_lib_name=crypto
    dynamic_ssl_lib_name=ssl
    dynamic_extra_libs="$forced_static_zlib"
    dynamic_lib_dir=
    dynamic_runtime_lib_dir=
    found_static_lib=no
    found_dynamic_lib=no

    save_CPPFLAGS=$CPPFLAGS
    CPPFLAGS="-I$idir/include"
    AC_EGREP_CPP(^yes.?$,[
#include <openssl/opensslv.h>
#if OPENSSL_VERSION_NUMBER >= 0x0090803fL
yes
#endif
		 ],
		 [valid_include=yes],
		 [valid_include=no])
    AC_EGREP_CPP(^yes.?$,[
#include <openssl/opensslv.h>
#if OPENSSL_VERSION_MAJOR == 3
yes
#endif
		 ],
		 [v3_include=yes],
		 [v3_include=no])


    if test "x$DISABLE_OTP_TEST_ENGINE" = "xdefault"; then
# Get default value for the --disable-otp-test-engine. Depends on cryptolib version
          AC_EGREP_CPP(^yes.?$,[
#include <openssl/opensslv.h>
#if LIBRESSL_VERSION_NUMBER >= 0x3050000fL
yes
#endif
        		 ],
	                 [DISABLE_OTP_TEST_ENGINE=yes],
                         [DISABLE_OTP_TEST_ENGINE=no])
    fi

    CPPFLAGS="$save_CPPFLAGS"

    AS_IF([test $valid_include != yes],
          [
              # Not a valid include...
              static_crypto_lib_name=
              static_ssl_lib_name=
              static_extra_libs=
              static_lib_dir=
              dynamic_crypto_lib_name=
              dynamic_ssl_lib_name=
              dynamic_extra_libs=
              dynamic_lib_dir=
              dynamic_runtime_lib_dir=
          ],
          [test "$host_os" = "win32"],
          [
              # Windows...
              AS_IF([test "x$ac_cv_sizeof_void_p" = "x8"],
                    [
                        lib_bits=64
                    ],
                    [
                        lib_bits=32
                    ])
              AS_IF([test -f "$dir/lib/VC/static/libcrypto${lib_bits}MD.lib"],
                    [
                        static_crypto_lib_name=libcrypto${lib_bits}MD
                        static_ssl_lib_name="libssl${lib_bits}MD"
                        static_extra_libs="$static_extra_libs -lCRYPT32 -lWS2_32"
                        static_lib_dir="$dir/lib/VC/static"
                        found_static_lib=yes
                    ])
              AS_IF([test -f "$dir/lib/VC/libcrypto${lib_bits}MD.lib"],
                    [
                        dynamic_crypto_lib_name=libcrypto${lib_bits}MD
                        dynamic_ssl_lib_name="libssl${lib_bits}MD"
                        dynamic_runtime_lib_dir="$rdir/lib/VC"
                        dynamic_lib_dir="$dir/lib/VC"
                        found_dynamic_lib=yes
                    ])
          ],
          [
              # Not windows...
              AS_CASE(["$host_os"],
                      [darwin*],
                      [
                          static_ext=.a
                          dynamic_ext=.dylib
                      ],
                      [
                          static_ext=.a
                          dynamic_ext=.so
                      ])
              for sdir in $search_subdirs; do
                  AS_IF([test -f "$dir/$sdir/lib$dynamic_crypto_lib_name$dynamic_ext"],
                        [
                            found_dynlib=yes
                        ],
                        [
                            found_dynlib=no
                            AS_CASE([$host_os],
                                    [openbsd*],
                                    [
                                        # libcrypto.so got a suffix on (at least some) openbsd. 
                                        # If we find libcrypto.so with suffix, we try to link
                                        # to it using -lcrypto ...
                                        for f in "$dir/$sdir/lib$dynamic_crypto_lib_name$dynamic_ext"*; do
                                               AS_IF([test -f "$f"], [found_dynlib=yes; break])
                                        done
                                    ])
                       ])
                  AS_IF([test $found_dynlib = yes],
                        [
                            # Found dynamic library; check if it links...
                            dynamic_lib_dir="$dir/$sdir"
                            dynamic_runtime_lib_dir="$rdir/$sdir"
                            ERL_LINK_CRYPTO_IFELSE(["-I$idir/include"],
                                                   ["-L$dynamic_lib_dir"],
                                                   ["-l$dynamic_crypto_lib_name $dynamic_extra_libs"],
                                                   [found_dynamic_lib=yes],
                                                   [found_dynamic_lib=no])
                            AS_IF([test $found_dynamic_lib = no -a $static_zlib != no],
                                  [
                                      # No, but linking with zlib as well might help...
                                      dynamic_extra_libs="$dynamic_extra_libs $static_zlib"
                                      ERL_LINK_CRYPTO_IFELSE(["-I$idir/include"],
                                                             ["-L$dynamic_lib_dir"],
                                                             ["-l$dynamic_crypto_lib_name $dynamic_extra_libs"],
                                                             [found_dynamic_lib=yes],
                                                             [found_dynamic_lib=no])
                                  ])
                        ])
                  AS_IF([test -f "$dir/$sdir/lib$static_crypto_lib_name$static_ext"],
                        [
                            # Found static library; check if it links...
                            static_lib_dir="$dir/$sdir"
                            ERL_LINK_CRYPTO_IFELSE(["-I$idir/include"],
                                                   [],
                                                   ["$dir/$sdir/lib$static_crypto_lib_name$static_ext $static_extra_libs"],
                                                   [found_static_lib=yes],
                                                   [found_static_lib=no])
                            AS_IF([test $found_static_lib = no -a $static_zlib != no],
                                  [
                                      # No, but inking with zlib as well might help...
                                      static_extra_libs="$static_extra_libs $static_zlib"
                                      ERL_LINK_CRYPTO_IFELSE(["-I$idir/include"],
                                                             [],
                                                             ["$dir/$sdir/lib$static_crypto_lib_name$static_ext $static_extra_libs"],
                                                             [found_static_lib=yes],
                                                             [found_static_lib=no])
                                  ])
                        ])
                  AS_IF([test $found_static_lib = yes -o $found_dynamic_lib = yes],
                        [break])
              done
          ])
])

# Remove all SKIP files from previous runs
for a in ssl crypto ssh; do
  rm -f "$ERL_TOP/lib/$a/SKIP"
done

# Setup subdirectories to search in...
search_subdirs="lib"
if test "$ac_cv_sizeof_void_p" = "8"; then
   search_subdirs="$search_subdirs lib64 lib/64"
else
   search_subdirs="$search_subdirs lib32 lib/32"
fi
test "$multiarch_dir" = "" || search_subdirs="lib/$multiarch_dir $search_subdirs"
test "$with_ssl_lib_subdir" = "" || search_subdirs="$with_ssl_lib_subdir $search_subdirs"

# We might need to link with zlib, so first check if we can find
# a static zlib to use if needed...
static_zlib=no
forced_static_zlib=
case "$with_ssl_zlib" in
     no)
     	;;
     default | yes)
     	AC_MSG_CHECKING([for static zlib])
     	for dir in $std_ssl_locations; do
	    for sdir in $search_subdirs; do
	    	if test -f "$erl_xcomp_sysroot$dir/$sdir/libz.a"; then
		   static_zlib="$erl_xcomp_sysroot$dir/$sdir/libz.a"
		   break
		fi
	    done
	    test "$static_zlib" = "no" || break
	done
	AC_MSG_RESULT([$static_zlib])
	if test $with_ssl_zlib = yes; then
	   if test $static_zlib = no; then
		AC_MSG_ERROR([linking with zlib requested by user, but no such library was found])
	   fi
	   forced_static_zlib="$static_zlib"
	   static_zlib=no
	fi;;
     *) # PATH
     	AC_MSG_CHECKING([for static zlib])
     	if test -f "$with_ssl_zlib/libz.a"; then
	   forced_static_zlib="$with_ssl_zlib/libz.a"
	else
	   for sdir in $search_subdirs; do
	       if test -f "$with_ssl_zlib/$sdir/libz.a"; then
	       	  forced_static_zlib="$with_ssl_zlib/$sdir/libz.a"
		  break
	       fi
	   done
	fi
	if test "$forced_static_zlib" = ""; then
	   AC_MSG_ERROR([linking with zlib requested by user, but no such library was found in $with_ssl_zlib])
	fi
	AC_MSG_RESULT([$forced_static_zlib])
	;;
esac

if test $enable_dynamic_ssl = undefined; then
   if test "$host_os" = "win32" ; then
      enable_dynamic_ssl=default_no
   else
      enable_dynamic_ssl=default_yes
   fi
fi

case $enable_dynamic_ssl in
     yes|default_yes)
	SSL_DYNAMIC_ONLY=yes;;
     *)
	SSL_DYNAMIC_ONLY=no;;
esac

AS_CASE(["$erl_xcomp_without_sysroot-$with_ssl"],
        [yes-* | no-no],
        [
            SSL_APP=
            CRYPTO_APP=
            SSH_APP=
            if test "$with_ssl" != "no"; then
	           skip="Cannot search for ssl; missing cross system root (erl_xcomp_sysroot)."
               fi
            for a in ssl crypto ssh; do
                echo "$skip" > $ERL_TOP/lib/$a/SKIP
                done
        ],
        [no-yes | no-],
        [
            # We search for OpenSSL in the common OS standard locations.
            SSL_APP=ssl
            CRYPTO_APP=crypto
            SSH_APP=ssh

            AS_IF([test "$host_os" != "win32"],
                  [
                      std_win_ssl_locations=
                  ],
                  [
                      AS_IF([test "x$ac_cv_sizeof_void_p" = "x8"],
                            [
	                        std_win_ssl_locations="/mnt/c/OpenSSL-Win64 /c/OpenSSL-Win64 /mnt/c/opt/local64/pgm/OpenSSL /opt/local64/pgm/OpenSSL /cygdrive/c/OpenSSL-Win64"
                            ],
                            [
                                std_win_ssl_locations="/mnt/c/OpenSSL-Win32 /c/OpenSSL-Win32 /mnt/c/OpenSSL /c/OpenSSL /cygdrive/c/OpenSSL /opt/local/pgm/OpenSSL /opt/local32/pgm/OpenSSL /mnt/c/opt/local/pgm/OpenSSL /mnt/c/opt/local32/pgm/OpenSSL /cygdrive/c/OpenSSL-Win32"
                            ])
	          ])

           usable_crypto=no
           AC_MSG_CHECKING([for OpenSSL crypto library with version >= 0.9.8c in standard locations])
           for rdir in $std_win_ssl_locations $std_ssl_locations; do
               AS_IF([test -f "$erl_xcomp_isysroot$rdir/include/openssl/opensslv.h"],
                     [
                         SSL_INCDIR="$erl_xcomp_isysroot$rdir"
                         SSL_INCLUDE="-I$SSL_INCDIR/include"
                         ERL_CRYPTO_CHECK_LIB(["$SSL_INCDIR"], ["$erl_xcomp_sysroot"], ["$rdir"])
                         AS_CASE([$enable_dynamic_ssl-$found_dynamic_lib-$found_static_lib],
                                 [no-*-no],
                                 [
                                     # Dynamic linking against ssl library disabled by user,
                                     # and no static library found...
                                 ],
                                 [yes-no-*],
                                 [
                                     # Static linking against ssl library disabled by user,
                                     # and no dynamic library found...
                                 ],
                                 [*-no-no],
                                 [
                                     # Neither static nor dynamic ssl library found...
                                 ],
                                 [no-*-yes | default_no-*-yes | default_yes-no-yes],
                                 [
                                     # Link statically...
                                     SSL_CRYPTO_LIBNAME=$static_crypto_lib_name
                                     SSL_SSL_LIBNAME=$static_ssl_lib_name
                                     SSL_LIBDIR="$static_lib_dir"
                                     SSL_EXTRA_LIBS="$static_extra_libs"
                                     SSL_RUNTIME_LIBDIR=
                                     SSL_DYNAMIC_ONLY=no
                                     usable_crypto=yes
                                     break
                                 ],
                                 [*-yes-*],
                                 [
                                     # Link dynamically...
                                     SSL_CRYPTO_LIBNAME=$dynamic_crypto_lib_name
                                     SSL_SSL_LIBNAME=$dynamic_ssl_lib_name
                                     SSL_EXTRA_LIBS="$dynamic_extra_libs"
                                     SSL_LIBDIR="$dynamic_lib_dir"
                                     SSL_RUNTIME_LIBDIR="$dynamic_runtime_lib_dir"
                                     SSL_DYNAMIC_ONLY=yes
                                     usable_crypto=yes
                                     break
                                 ])
                     ])
            done

            AS_IF([test $usable_crypto = yes],
                  [AS_IF([test $SSL_DYNAMIC_ONLY = yes],
       	                 [AC_MSG_RESULT([dynamic $SSL_CRYPTO_LIBNAME library in $SSL_LIBDIR])],
                         [AC_MSG_RESULT([static $SSL_CRYPTO_LIBNAME library in $SSL_LIBDIR])])],
                  [
                         AC_MSG_RESULT([no])
                         SSL_APP=
                         CRYPTO_APP=
                         SSH_APP=

                         SSL_INCDIR=
                         SSL_INCLUDE=
                         SSL_CRYPTO_LIBNAME=
                         SSL_SSL_LIBNAME=
                         SSL_INCDIR=
                         SSL_RUNTIME_LIBDIR=
                         SSL_LIBDIR=

                         AS_CASE(["$with_ssl-$enable_dynamic_ssl"],
                                 [yes-yes],
                                 [
                                     AC_MSG_ERROR([crypto library with dynamic linking requested by user, but no such library was found])
                                 ],
                                 [yes-no],
                                 [
                                     AC_MSG_ERROR([crypto library with static linking requested by user, but no such library was found])
                                 ],
                                 [yes-*],
                                 [
                                     AC_MSG_ERROR([crypto library requested by user, but no such library was found])
                                 ],
                                 [
                                     AC_MSG_WARN([No (usable) OpenSSL found, skipping ssl, ssh and crypto applications])
                                 ])

                         for a in ssl crypto ssh; do
                             echo "No usable OpenSSL found" > $ERL_TOP/lib/$a/SKIP
                         done
                  ])
        ],
        [
            # Option given with PATH to package
            AS_IF([test ! -d "$with_ssl"],
                  [AC_MSG_ERROR(Invalid path to option --with-ssl=PATH)])
            AS_IF([test ! -d "$with_ssl_incl"],
                  [AC_MSG_ERROR(Invalid path to option --with-ssl-incl=PATH)])
            AC_MSG_CHECKING([for OpenSSL header in $with_ssl_incl])
            AS_IF([test -f "$with_ssl_incl/include/openssl/opensslv.h"],
                  [AC_MSG_RESULT([yes])],
                  [AC_MSG_ERROR([no OpenSSL header found in $with_ssl_incl])])
            SSL_INCDIR="$with_ssl_incl"
            AC_MSG_CHECKING([for OpenSSL in $with_ssl])
            ERL_CRYPTO_CHECK_LIB(["$with_ssl_incl"], [""], ["$with_ssl"])
            AS_CASE([$enable_dynamic_ssl-$found_dynamic_lib-$found_static_lib],
                    [no-*-no],
       	            [
                        AC_MSG_ERROR([dynamic linking against crypto library disabled by user, but no static library found in $with_ssl])
                    ],
                    [yes-no-*],
       	            [
                        AC_MSG_ERROR([static linking against crypto library disabled by user, but no dynamic library found in $with_ssl])
                    ],
                    [*-no-no],
       	            [
                        AC_MSG_ERROR([neither static nor dynamic crypto library found in $with_ssl])
                    ],
                    [no-*-yes | default_no-*-yes | default_yes-no-yes],
                    [
                        # Link statically...
   	                    SSL_CRYPTO_LIBNAME=$static_crypto_lib_name
                        SSL_SSL_LIBNAME=$static_ssl_lib_name
                        SSL_LIBDIR="$static_lib_dir"
                        SSL_EXTRA_LIBS="$static_extra_libs"
                        SSL_RUNTIME_LIBDIR=
                        SSL_DYNAMIC_ONLY=no
                    ],
	                [*-yes-*],
                    [
                        # Link dynamically...
                        SSL_CRYPTO_LIBNAME=$dynamic_crypto_lib_name
                        SSL_SSL_LIBNAME=$dynamic_ssl_lib_name
                        SSL_EXTRA_LIBS="$dynamic_extra_libs"
                        SSL_LIBDIR="$dynamic_lib_dir"
                        SSL_RUNTIME_LIBDIR="$dynamic_runtime_lib_dir"
                        SSL_DYNAMIC_ONLY=yes
                   ])
            AS_IF([test $SSL_DYNAMIC_ONLY = yes],
                  [AC_MSG_RESULT([dynamic $SSL_CRYPTO_LIBNAME library in $SSL_LIBDIR])],
                  [AC_MSG_RESULT([static $SSL_CRYPTO_LIBNAME library in $SSL_LIBDIR])])
            SSL_INCLUDE="-I$with_ssl_incl/include"
            SSL_APP=ssl
            CRYPTO_APP=crypto
            SSH_APP=ssh
            AS_IF([test "$cross_compiling" = "yes"],
                  [
                      SSL_RUNTIME_LIBDIR=`echo "$SSL_LIBDIR" | sed -n "s|^$erl_xcomp_sysroot\(/*\)\(.*\)\$|/\2|p"`
                  ],
                  [
                      SSL_RUNTIME_LIBDIR="$SSL_LIBDIR"
                  ])
        ])dnl AS_CASE

SSL_DED_LD_RUNTIME_LIBRARY_PATH=
ded_ld_rflg="$DED_LD_FLAG_RUNTIME_LIBRARY_PATH"

AS_CASE(["$with_ssl_rpath"],
        [no],
        [
            # Use no ssl runtime library path
            SSL_DED_LD_RUNTIME_LIBRARY_PATH=
        ],
        [yes | default],
        [
            # Use standard lib locations for ssl runtime library path
            AC_MSG_CHECKING([for runtime library path to use])

            if test $enable_dynamic_ssl = no -a "$with_ssl_rpath" = yes; then
               AC_MSG_ERROR([requested by user, but user also requested static linking])
            fi

            AS_IF([test "$CRYPTO_APP" = ""],
                  [
                      AS_IF([test "$with_ssl_rpath" = yes],
                            [AC_MSG_ERROR([requested by user, but could not be used since no crypto library was found])])
                      AC_MSG_RESULT([])
                  ],
                  [test "$ded_ld_rflg" = ""],
                  [
                      AS_IF([test "$with_ssl_rpath" = yes],
                            [AC_MSG_ERROR([requested by user, but cannot be set on this platform])])
                      AC_MSG_RESULT([])
                  ],
                  [test "$SSL_DYNAMIC_ONLY" != "yes"],
                  [
                      AS_IF([test "$with_ssl_rpath" = yes],
                            [AC_MSG_WARN([requested by user, but dynamic linking will not be used])],
                            [AC_MSG_RESULT([])])
                  ],
                  [
                      # Try to find a suitable rpath based on default install directories...
                      libdirs="/lib"

                      dir_lib64=no
                      dir_lib_64=no
                      dir_lib32=no
                      dir_lib_32=no
                      dir_multiarch=no
                      dir_with_ssl_lib_subdir=no

                      AS_IF([test "$multiarch_dir" != ""], [dir_multiarch=yes])
                      AS_IF([test "$with_ssl_lib_subdir" != ""], [dir_with_ssl_lib_subdir=yes])

                      AS_CASE(["$SSL_RUNTIME_LIBDIR"],
                              [*/lib/64 | */lib/64/], [dir_lib_64=yes],
                              [*/lib64 | */lib64/], [dir_lib64=yes],
                              [*/lib/32 | */lib/32/], [dir_lib_32=yes],
                              [*/lib32 | */lib32/], [dir_lib32=yes])

                      for dir in $std_ssl_locations; do
                          AS_IF([test "$ac_cv_sizeof_void_p" = "8"],
                                [
                                    AS_IF([test $dir_lib_64 = no && test -d "$erl_xcomp_sysroot$dir/lib/64"],
                                          [dir_lib_64=yes])
                                    AS_IF([test $dir_lib64 = no && test -d "$erl_xcomp_sysroot$dir/lib64"],
                                          [dir_lib64=yes])
                                ],
                                [
                                    AS_IF([test $dir_lib_32 = no && test -d "$erl_xcomp_sysroot$dir/lib/32"],
                                          [dir_lib_32=yes])
                                    AS_IF([test $dir_lib32 = no && test -d "$erl_xcomp_sysroot$dir/lib32"],
                                          [dir_lib32=yes])
                                ])
                          AS_IF([test $dir_multiarch = no && test -d "$erl_xcomp_sysroot$dir/lib/$multiarch_dir"],
                                [dir_multiarch=yes])
                      done

                      AS_IF([test $dir_with_ssl_lib_subdir = yes],
                            [libdirs="/$with_ssl_lib_subdir $libdirs"])
                      AS_IF([test $dir_multiarch = yes],
                            [libdirs="/lib/$multiarch_dir $libdirs"])
                      AS_IF([test $dir_lib_64 = yes],
                            [libdirs="/lib/64 $libdirs"])
                      AS_IF([test $dir_lib64 = yes],
                            [libdirs="/lib64 $libdirs"])
                      AS_IF([test $dir_lib_32 = yes],
                            [libdirs="/lib/32 $libdirs"])
                      AS_IF([test $dir_lib32 = yes],
                            [libdirs="/lib32 $libdirs"])

                      #
                      # We try the follwing runtime paths in the
                      # following order:
                      # 1. Standard ssl locations with all variations
                      #    of lib subdirectories that we know of
                      # 2. Standard ssl locations with all variations
                      #    of lib subdirectories that we know of and
                      #    exist on this machine
                      # 3. Determined $SSL_RUNTIME_LIBDIR
                      # If none of the above links, we skip runtime
                      # library path...
                      #
                      for type in std x_std curr; do

                          ded_ld_rpath="$ded_ld_rflg$SSL_RUNTIME_LIBDIR"
                          rpath="$SSL_RUNTIME_LIBDIR"

                          AS_IF([test $type != curr],
                                [
                                    for ldir in $libdirs; do
                                        for dir in $std_ssl_locations; do
                                            AS_IF([test "$SSL_RUNTIME_LIBDIR" = "$dir$ldir"],
                                                  [continue])
                                            AS_IF([test $type = x_std && test '!' -d "$erl_xcomp_sysroot$dir$ldir"],
                                                  [continue])
                                            AS_IF([test "$dir" = "/"],
                                                  [libdir="$ldir"],
                                                  [libdir="$dir$ldir"])
                                            ded_ld_rpath="$ded_ld_rpath $ded_ld_rflg$libdir"
                                            rpath="$rpath:$libdir"
                                        done
                                    done
                                ])

                          # Check that it still links when we add the runtime path...
                          ERL_LINK_CRYPTO_IFELSE(["$SSL_INCLUDE"],
                                                 ["$ded_ld_rpath -L$SSL_LIBDIR"],
                                                 ["-l$SSL_CRYPTO_LIBNAME $SSL_EXTRA_LIBS"],
                                                 [rpath_success=yes],
                                                 [rpath_success=no])

                          AS_IF([test "$rpath_success" = "yes"], [break])

                      done

                      AS_IF([test "$rpath_success" != "yes"],
                            [
                                ded_ld_rpath=
                                rpath=
                            ])

                      SSL_DED_LD_RUNTIME_LIBRARY_PATH="$ded_ld_rpath"

                      AS_IF([test "$rpath" != ""],
                            [
                                AC_MSG_RESULT([$rpath])
                            ],
                            [test "$with_ssl_rpath" = yes],
                            [
                                AC_MSG_ERROR([requested by user, but could not be used])
                            ],
                            [
                                AC_MSG_WARN([unable to set run path during linking])
                            ])
                  ])
        # end yes|default
        ],
        [
            # Use ssl runtime library paths set by --with-ssl-rpath
            AC_MSG_CHECKING([whether runtime library path can be set])

            AS_IF([test $enable_dynamic_ssl = no],
                  [
                      AC_MSG_ERROR([runtime library path requested by user, but user also requested static linking])
                  ],
                  [test "$CRYPTO_APP" = ""],
                  [
                      AC_MSG_ERROR([runtime library path requested by user, but could not be set since no crypto library was found])
                  ],
                  [test "$ded_ld_rflg" = ""],
                  [
                      AC_MSG_ERROR([runtime library path requested by user, but cannot be set on this platform])
                  ],
                  [test "$SSL_DYNAMIC_ONLY" != "yes"],
                  [
                      AC_MSG_WARN([runtime library path requested by user, but dynamic linking will not be used])
                  ],
                  [
                      # Check that it links...
                      ded_ld_rpath=
                      delimit=
                      rpath=
                      rpath_delimit=
                      save_ifs="$IFS"; IFS=",:"
                      for dir in $with_ssl_rpath; do
                          ded_ld_rpath="$ded_ld_rpath$delimit$ded_ld_rflg$dir"
                          delimit=" "
                          rpath="$rpath$rpath_delimit$dir"
                          rpath_delimit=":"
                      done
                      IFS="$save_ifs"
                      SSL_DED_LD_RUNTIME_LIBRARY_PATH="$ded_ld_rpath"
                      ERL_LINK_CRYPTO_IFELSE(["$SSL_INCLUDE"],
                                             ["$ded_ld_rpath -L$SSL_LIBDIR"],
                                             ["-l$SSL_CRYPTO_LIBNAME $SSL_EXTRA_LIBS"],
                                             [rpath_success=yes],
                                             [rpath_success=no])
                      AS_IF([test "$rpath_success" = yes],
                            [AC_MSG_RESULT([yes; using $rpath])],
                            [AC_MSG_ERROR([runtime library path requested by user, but link failed])])
                  ])
        ])dnl AS_CASE    

AC_ARG_ENABLE(fips,
AS_HELP_STRING([--enable-fips], [enable OpenSSL FIPS mode support])
AS_HELP_STRING([--disable-fips], [disable OpenSSL FIPS mode support (default)]),
[ case "$enableval" in
    yes) enable_fips_support=yes ;;
    *)   enable_fips_support=no ;;
  esac ], enable_fips_support=no)

SSL_FLAGS=
AS_IF([test "$enable_fips_support" = "yes"],
      [
          AS_IF([test "$CRYPTO_APP" = ""],
                [AC_MSG_ERROR([FIPS support requested, but no crypto library found])])
          # Insert check for FIPS support here
          SSL_FLAGS="-DFIPS_SUPPORT"
      ])

AS_IF([test "x$v3_include" = "xyes"],
      [
       AS_IF([test "x$SSL_DYNAMIC_ONLY" = "xno" ],
             [
              echo "Static linking with OpenSSL 3.0 *MAY* require special configuring of the cryptolib." >> ./CONF_INFO
              AC_MSG_WARN(******************************************************************)
              AC_MSG_WARN(* Static linking *MAY* require re-configuring the 3.0 cryptolib. *)
              AC_MSG_WARN(******************************************************************)
             ])
      ])

CRYPTO_NO_DEPRECATE_WARN=
AS_IF([test "x$v3_include" = "xyes" && test "$CRYPTO_APP" != ""],
      [CRYPTO_NO_DEPRECATE_WARN="-Wno-deprecated-declarations"
      ])

saveCFLAGS="$CFLAGS"
saveLDFLAGS="$LDFLAGS"
saveLIBS="$LIBS"
CFLAGS="$DED_BASIC_CFLAGS $SSL_INCLUDE"
AS_IF([test $SSL_DYNAMIC_ONLY = yes],
      [
          LDFLAGS="$DED_LDFLAGS_CONFTEST $ded_ld_rpath -L$SSL_LIBDIR"
          LIBS="$LIBS -l$SSL_CRYPTO_LIBNAME $SSL_EXTRA_LIBS"
      ],
      [
          LDFLAGS="$DED_LDFLAGS_CONFTEST"
          AS_IF([test "$host_os" = "win32"],
                [LIBS="$LIBS $SSL_LIBDIR/$SSL_CRYPTO_LIBNAME.lib $SSL_EXTRA_LIBS"],
                [LIBS="$LIBS $SSL_LIBDIR/lib$SSL_CRYPTO_LIBNAME.a $SSL_EXTRA_LIBS"])
      ])
AC_CHECK_DECL([CRYPTO_memcmp],
	      [have_crypto_memcmp_decl=yes],
	      [have_crypto_memcmp_decl=no],
	      [#include <openssl/crypto.h>])
AS_IF([test $have_crypto_memcmp_decl = yes],
      [
          AC_MSG_CHECKING([whether CRYPTO_memcmp can be linked])
          AC_LINK_IFELSE(
              [AC_LANG_PROGRAM([[
                                    #include <openssl/crypto.h>
                               ]],
                               [[
                                    CRYPTO_memcmp("a", "b", 1);
                               ]])],
              [
                  SSL_FLAGS="-DHAVE_OPENSSL_CRYPTO_MEMCMP $SSL_FLAGS"
                  AC_MSG_RESULT([yes])
              ],
              [
                  AC_MSG_RESULT([no])
              ])
      ])
CFLAGS="$saveCFLAGS"
LDFLAGS="$saveLDFLAGS"
LIBS="$saveLIBS"

ERL_PUSH_WERROR

AC_SUBST(SSL_INCLUDE)
AC_SUBST(SSL_INCDIR)
AC_SUBST(SSL_LIBDIR)
AC_SUBST(SSL_FLAGS)
AC_SUBST(SSL_CRYPTO_LIBNAME)
AC_SUBST(SSL_SSL_LIBNAME)
AC_SUBST(SSL_EXTRA_LIBS)
AC_SUBST(SSL_DED_LD_RUNTIME_LIBRARY_PATH)
AC_SUBST(SSL_DYNAMIC_ONLY)
AC_SUBST(DISABLE_EVP_DH)
AC_SUBST(DISABLE_EVP_HMAC)
AC_SUBST(DISABLE_OTP_TEST_ENGINE)
AC_SUBST(CRYPTO_NO_DEPRECATE_WARN)

AC_CONFIG_FILES([c_src/$host/Makefile:c_src/Makefile.in])
AC_OUTPUT

