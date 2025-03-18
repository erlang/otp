dnl Process this file with autoconf to produce a configure script. -*-m4-*-

dnl %CopyrightBegin%
dnl
dnl Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

AC_INIT

m4_include([otp.m4])

AC_CONFIG_AUX_DIR([${ERL_TOP}/make/autoconf])

AC_PREREQ([2.71])

ERL_PUSH_WERROR

## Delete previous failed configure results
if test -f ./CONF_INFO; then
   rm ./CONF_INFO
fi

AC_DEFUN([WX_MSG_ERROR],
[	echo "$1" >> ./CONF_INFO
    WXERL_CAN_BUILD_DRIVER=false
    if test X"$with_wx" = X"yes" ; then
        AC_MSG_ERROR([$1])
    else
        AC_MSG_WARN([$1])
    fi
])

AC_ARG_WITH(wx,
[  --with-wxdir=PATH    specify location of wxWidgets include and lib
  --with-wx             use wxWidgets (default)
  --without-wx          don't use wxWidgets])

ERL_CANONICAL_SYSTEM_TYPE

case $host_os in
    mingw32)
	if test "X$host" = "X"; then
	   host=win32
	fi
	;;
    *)
	;;
esac

TARGET=$host
AC_SUBST(TARGET)

# The resulting binary should be a library, not an executable. Therefore, the
# various PIE flags are not needed.
CFLAGS="$(echo $CFLAGS | sed -e 's!-fPIE!!g' -e 's!-fpie!!g')"
LDFLAGS="$(echo $LDFLAGS | sed -e 's!-fPIE!!' -e 's!-fpie!!' -e 's!-pie!!')"

AC_PROG_CC
AC_PROG_CXX
AC_PROG_RANLIB
AC_PROG_CPP
LM_PROG_LD

AC_LANG_PUSH([C++])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
  [[#ifndef __cplusplus
    #error "broken C++"
    #endif]])],,
  [CXX=;])
AC_LANG_POP([C++])
if test "X$CXX" = X ; then
   WX_MSG_ERROR([Can not find C++ compiler])
fi
WXERL_CAN_BUILD_DRIVER=false

AC_MSG_NOTICE(Building for [$host_os])
WXERL_CAN_BUILD_DRIVER=true

ERL_OSSF_FLAGS

LM_WINDOWS_ENVIRONMENT

USER_CFLAGS=$CFLAGS

if test  X"$MIXED_VC" = X"yes" ; then
   CFLAGS="-Owx"
fi

AC_CHECK_SIZEOF(void *)

PTHR_CFLAGS="-D_THREAD_SAFE -D_REENTRANT"

OBJC_CC=$CC
OBJC_CFLAGS=""
CXXFLAGS=""
dnl NOTE: CPPFLAGS will be included in CFLAGS at the end
AS_CASE([$host_os],

    [darwin*],
    [
	AC_MSG_CHECKING([Checking wxWidgets for min version:])
	WX_CC=`wx-config --cc`
	MAC_MIN=`echo "$WX_CC" | sed 's/^[[^ ]]*\ *//'`
	AC_MSG_RESULT([$MAC_MIN])

	AC_MSG_CHECKING([if compiler accepts -ObjC])
  	CFLAGS="$CFLAGS -ObjC"
  	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[;]])],[accept_objc_flag=true],[accept_objc_flag=false])
  	if test "X$accept_objc_flag" = "Xtrue"; then
	   AC_MSG_RESULT([yes])
           C_ONLY_FLAGS="-ObjC"
	else
	   dnl We are probably trying to build with a non-Apple gcc,
	   dnl which is good as long as we do not try to build Cocoa
	   dnl code. We need an Apple compiler for just that (Objective C)
	   AC_MSG_RESULT([no])
	   AC_MSG_CHECKING([for a Cocoa compliant Objective C compiler])
	   SEARCHFOR=""
	   SEARCHFORXX=""
	   save_IFS=$IFS
	   IFS=:
	   set $PATH
	   IFS=$save_IFS
	   while test X"$1" != X""; do
	       dnl Add all possible paths to a real apple gcc
	       SEARCHFOR="$1/gcc-apple-4.2 $SEARCHFOR"
	       SEARCHFORXX="$1/g++-apple-4.2 $SEARCHFORXX"
	       shift
	   done
	   dnl Add LLVM compilers, they will work in this case
	   SEARCHFOR="/usr/bin/clang /usr/bin/gcc $SEARCHFOR"	
	   SEARCHFORXX="/usr/bin/clang /usr/bin/g++ $SEARCHFORXX"	
	   APPLE_CC=""
	   APPLE_CXX=""
	   dnl SEARCHFOR is reversed, so we want to find the last existing
	   dnl executable in the list
	   IFS=" "
	   set $SEARCHFORXX
	   for x in $SEARCHFOR; do
	       if test -x $x; then
	       	  APPLE_CC=$x
	       fi
	       if test -x "$1"; then
	       	  APPLE_CXX="$1"
	       fi
	       shift	
	   done	
	   IFS=$save_IFS
	   if test X$APPLE_CC = X -o X$APPLE_CXX = X; then
	      AC_MSG_RESULT([no])
	      dnl Complete failure, we cannot build Cocoa code
	      WX_MSG_ERROR([Can not find compiler to compile Cocoa applications])
	   else
	      dnl We think we found an Apple compiler and will add 
	      dnl Apple specific options
	      AC_MSG_RESULT([$APPLE_CC ($APPLE_CXX)])
	      dnl We should use this compiler for all of wx - hack...
	      CC=$APPLE_CC
	      CXX=$APPLE_CXX
	      dnl Both clang and gcc accept these flags...
	      #CXXFLAGS="-x c++ $CXXFLAGS"
	      OBJC_CC=$APPLE_CC
	      OBJC_CFLAGS="-ObjC"
	   fi
  	fi
  	CFLAGS="$USER_CFLAGS $MAC_MIN -Wno-deprecated-declarations"
	CPPFLAGS="$CPPFLAGS -D_MACOSX $PTHR_CFLAGS"
    ],

    [mingw32],
    [
	CFLAGS="$CFLAGS -DWIN32 -DWINVER=0x0600 -D_WINDOWS -D_UNICODE -DUNICODE"
	CPPFLAGS="$CPPFLAGS -D_WIN32_WINNT=0x0600"
	AC_MSG_WARN([Reverting to 32-bit time_t])
	CPPFLAGS="$CPPFLAGS -D_USE_32BIT_TIME_T"
    ],

    [win32],
    [
	CFLAGS="$CFLAGS -DWIN32 -DWINVER=0x0600 -D_WINDOWS -D_UNICODE -DUNICODE"
	CPPFLAGS="$CPPFLAGS -D_WIN32_WINNT=0x0600"
    ],

    [
        GLIB_CFLAGS=`pkg-config --silence-errors --cflags glib-2.0`
        GLIB_LIBS=`pkg-config --silence-errors --libs glib-2.0`
        if test X"$GLIB_CFLAGS" != X ; then
            HAVE_GLIB="-DHAVE_GLIB=1"
        fi
        CFLAGS="$CFLAGS -Wno-deprecated-declarations"
       	CPPFLAGS="$CPPFLAGS -D_GNU_SOURCE $PTHR_CFLAGS $HAVE_GLIB"
    ])

AC_SUBST(GLIB_CFLAGS)
AC_SUBST(GLIB_LIBS)

AC_SUBST(OBJC_CC)
AC_SUBST(OBJC_CFLAGS)

EXTRA_LDFLAGS=

case $host_os in
    darwin*)
        # beam.smp has not been built yet, so we must not use the
        # -bundle_loader option in the configuration tests.
	LDFLAGS="$MAC_MIN -bundle $LDFLAGS"
	EXTRA_LDFLAGS=" -bundle_loader ${ERL_TOP}/bin/$host/beam.smp"
	GL_LIBS="-framework OpenGL"
	;;
    win32)
        LDFLAGS="-dll $LDFLAGS"
	GL_LIBS="-lglu32 -lOpengl32"
	;;
    mingw32)
	LDFLAGS="-shared -fPIC $LDFLAGS"
	GL_LIBS="-lglu32 -lOpengl32"
	;;
    *)
        # When checking linkage we'd like to see that symbols are resolved.
        # In order to notice errors, we must test without `-shared`.
	LDFLAGS="-fPIC $LDFLAGS"
	EXTRA_LDFLAGS=" -shared "
	GL_LIBS="-lGL -lGLU"
	;;
esac

dnl ----------------------------------------------------------------------
dnl Include CPPFLAGS in CFLAGS
dnl ----------------------------------------------------------------------

case $host_os in
    mingw32)
	DEBUG_CFLAGS="-g -Wall -DDEBUG $CFLAGS"
	CFLAGS="-g -Wall -O2 $CFLAGS -fomit-frame-pointer"
	;;
    win32)
	DEBUG_CFLAGS="-g -Wall $CFLAGS -DDEBUG"
	CFLAGS="-g -Wall -O2 $CFLAGS"
	;;
    darwin*)
	DEBUG_CFLAGS="-g -Wall -fPIC $CFLAGS -DDEBUG"
	# omit-frame-pointer causes seg faults with 10.9 and clang
	CFLAGS="-g -Wall -fPIC $CFLAGS"
	;;
    *)
	DEBUG_CFLAGS="-g -Wall -fPIC $CFLAGS -DDEBUG"
	CFLAGS="-Wall -fPIC $CFLAGS -fomit-frame-pointer"
	;;
esac

dnl
dnl  Use -O1 -fno-move-loop-invariants for wxe_funcs.cpp to reduce
dnl  compilation time
dnl

AS_IF([test "x$GCC" = xyes -a X"$host_os" != X"win32"],
 [
   CXXNOOPT="-O1"
   LM_TRY_ENABLE_CFLAG([-fno-move-loop-invariants], [CXXNOOPT])
   LM_TRY_ENABLE_CFLAG([-fno-var-tracking-assignments], [CXXNOOPT])
 ])

dnl
dnl  Opengl tests
dnl

AS_IF([test X"$host_os" != X"win32"],
  [
    AC_CHECK_HEADERS([GL/gl.h], [],
    	[AC_CHECK_HEADERS([OpenGL/gl.h])])
    AS_IF([test X"$ac_cv_header_GL_gl_h" != Xyes && test X"$ac_cv_header_OpenGL_gl_h" != Xyes],
      [
       	saved_CPPFLAGS="$CPPFLAGS"
	AC_MSG_NOTICE(Checking for OpenGL headers in /usr/X11R6)
       	CPPFLAGS="-isystem /usr/X11R6/include $CPPFLAGS"
       	$as_unset ac_cv_header_GL_gl_h
       	AC_CHECK_HEADERS([GL/gl.h])
       	AS_IF([test X"$ac_cv_header_GL_gl_h" != Xyes],
          [
	    AC_MSG_NOTICE(Checking for OpenGL headers in /usr/local)
	    CPPFLAGS="-isystem /usr/local/include $saved_CPPFLAGS"
	    $as_unset ac_cv_header_GL_gl_h
	    AC_CHECK_HEADERS([GL/gl.h])
	    AS_IF([test X"$ac_cv_header_GL_gl_h" != Xyes],
              [
		WX_MSG_ERROR([No OpenGL headers found, wx will NOT be usable])
		CPPFLAGS="$saved_CPPFLAGS"
              ],
              [
	    	GL_LIBS="-L/usr/local/lib $GL_LIBS"
              ])
	  ],
          [
    	    GL_LIBS="-L/usr/X11R6/lib $GL_LIBS"
       	  ])
      ])
  ],
  [
    AC_CHECK_HEADERS([gl/gl.h],[],[],[#include <windows.h>])
  ])

AS_IF([test X"$host_os" != X"win32"],
  [
    AC_CHECK_HEADERS([GL/glu.h], [],
        [AC_CHECK_HEADERS([OpenGL/glu.h])])
    if test X"$ac_cv_header_GL_glu_h" != Xyes &&
        test X"$ac_cv_header_OpenGL_glu_h" != Xyes
    then
    WX_MSG_ERROR([No GLU headers found, wx will NOT be usable])
    fi
  ],
  [
    AC_CHECK_HEADERS([gl/glu.h],[],[],[#include <windows.h>])
  ])

AC_SUBST(GL_LIBS)

DEBUG_CXXFLAGS="$CXXFLAGS $DEBUG_CFLAGS $CPPFLAGS"
DEBUG_CFLAGS="$DEBUG_CFLAGS $CPPFLAGS $C_ONLY_FLAGS"

CXXNOOPTFLAGS="$CXXFLAGS $CFLAGS $CPPFLAGS $CXXNOOPT"
CXXFLAGS="$CXXFLAGS $CFLAGS $CPPFLAGS"
CFLAGS="$CFLAGS $CPPFLAGS $C_ONLY_FLAGS"

AC_SUBST(DEBUG_CFLAGS)
AC_SUBST(DEBUG_CXXFLAGS)

ERLC=erlc
ERL=erl
ERLANG_ROOT_DIR=$ERL_TOP
AC_SUBST(ERLC)

AC_SUBST(ERLANG_ROOT_DIR)

dnl
dnl  Check for wxwidgets
dnl 
AS_IF(
  [test "$cross_compiling" = "yes"],
  [
    WX_MSG_ERROR([Cross compilation of the wx driver is not supported yet, wx will NOT be usable])
  ],

  [test  X"$MIXED_VC" = X"no"],
  [
    m4_include(wxwin-nothrow.m4)

    AM_OPTIONS_WXCONFIG
    reqwx=3.0.2
	AM_PATH_WXCONFIG($reqwx, [], [], [core,base,html,webview], [--unicode])
	AC_MSG_CHECKING(for wxwidgets webview)
	AC_LANG_PUSH(C++)
	saved_CXXFLAGS=$CXXFLAGS
	CXXFLAGS="$CXXFLAGS $WX_CXXFLAGS"
        saved_LIBS=$LIBS
        LIBS=$WX_LIBS

	AC_LINK_IFELSE(
 	  [AC_LANG_PROGRAM([[
	  #include "wx/wx.h"
          #include "wx/webview.h"
	  ]], [[
             wxWebView::New();
	  ]])],
	  HAVE_WEBVIEW_SUPPORT=yes, HAVE_WEBVIEW_SUPPORT=no)

	CXXFLAGS=$saved_CXXFLAGS
        LIBS=$saved_LIBS
	AC_LANG_POP(C++)
	AC_MSG_RESULT($HAVE_WEBVIEW_SUPPORT)

	if test X"$HAVE_WEBVIEW_SUPPORT" != X"yes" ; then
	  WXELIBS=stc,xrc,html,adv,xml,core,base,gl,aui
	  echo "wxWidgets was not compiled with --enable-webview or wxWebView developer package is not installed, wxWebView will NOT be available" >> ./CONF_INFO
	else
	  WXELIBS=stc,xrc,html,adv,xml,core,base,gl,aui,webview
          AC_DEFINE([WXE_WEBVIEW])
	fi

    # Try to find debug libs first 
    AM_PATH_WXCONFIG($reqwx, wxWinWithGLDBG=1, wxWinWithGLDBG=0, [$WXELIBS], [--unicode --debug=yes])

    AC_MSG_CHECKING(for debug build of wxWidgets)
    if test "$wxWinWithGLDBG" = 1; then
      AC_MSG_RESULT(yes);
    else
      AC_MSG_RESULT(no);
    fi

    DEBUG_WX_CFLAGS=$WX_CFLAGS
    DEBUG_WX_CXXFLAGS=$WX_CXXFLAGS
    DEBUG_WX_LIBS=$WX_LIBS
    DEBUG_WX_LIBS_STATIC=$WX_LIBS_STATIC
    AC_SUBST(DEBUG_WX_CFLAGS)
    AC_SUBST(DEBUG_WX_CXXFLAGS)
    AC_SUBST(DEBUG_WX_LIBS)
    AC_SUBST(DEBUG_WX_LIBS_STATIC)
    
    AM_PATH_WXCONFIG($reqwx, wxWinWithGL=1, wxWinWithGL=0, [$WXELIBS], [--unicode --debug=no])

    AC_MSG_CHECKING(for standard build of wxWidgets)
    if test "$wxWinWithGL" = 1; then
      AC_MSG_RESULT(yes);
    else
      AC_MSG_RESULT(no);
    fi

    if test "x$WX_LIBS_STATIC" = "x"; then
       WX_HAVE_STATIC_LIBS=false
    else
       WX_HAVE_STATIC_LIBS=true
    fi

    wxWin=0

    if test "$wxWinWithGLDBG" = 1; then
        if test "$wxWinWithGL" = 1; then
    	   wxWin=1
	   FORCED_DEBUG_BUILD=false
    	else 
    	   wxWin=1
	   FORCED_DEBUG_BUILD=debug
	   WX_CFLAGS=$DEBUG_WX_CFLAGS
	   WX_CXXFLAGS=$DEBUG_WX_CXXFLAGS
	   WX_LIBS=$DEBUG_WX_LIBS
	   WX_LIBS_STATIC=$DEBUG_WX_LIBS_STATIC
	fi
    elif test "$wxWinWithGL" = 1; then
    	wxWin=1
	FORCED_DEBUG_BUILD=false
    fi    
    AC_SUBST(FORCED_DEBUG_BUILD)
    RC_FILE_TYPE=o

    if test "$wxWin" != 1; then
       WX_MSG_ERROR([
        wxWidgets must be installed on your system.
 
        Please check that wx-config is in path, the directory
        where wxWidgets libraries are installed (returned by
        'wx-config --libs' or 'wx-config --static --libs' command)
        is in LD_LIBRARY_PATH or equivalent variable and
        wxWidgets version is $reqwx or above.])
    fi
  ],
  
  [ #else
    AC_MSG_CHECKING(for wxWidgets in standard locations)
    echo
    # Check whether --with-wxdir was given.
    AC_MSG_NOTICE(OptionCheck: [$with_wxdir $with_wx_prefix])

    if test "${with_wxdir+set}" = set; then :
      withval=$with_wxdir;  CWXWIN0=$withval
    else
      # Check whether --with-wx-prefix was given.
      if test "${with_wx_prefix+set}" = set; then :
        withval=$with_wx_prefix; CWXWIN0=$withval
      else
        CWXWIN0=""
      fi
    fi

    CWXWIN_CONFIG=`win32_path.sh -u $wx_config_name 2>/dev/null`
    CWXWIN1=`dirname $CWXWIN_CONFIG 2>/dev/null`
    CWXWIN2=`dirname $CWXWIN1 2>/dev/null`

    if test -z "$PROGRAMFILES" ; then 
       PROGRAMFILES="c:/Program Files"
    fi

    CWXWIN_PROG=`win32_path.sh -u "$PROGRAMFILES" 2>/dev/null`

    CWXWIN3="$CWXWIN_PROG/wxWidgets-3.*.*
    CWXWIN4="$CWXWIN_PROG/wxMSW-3.*.*

    DOC_OPT1=/opt/local/pgm
    DOC_OPT2=/mnt/c/opt/local/pgm
    CWX_DOCUMENTED="$DOC_OPT1/wxWidgets-3.*.* $DOC_OPT1/wxMSW-3.*.*"
    CWX_DOCUMENTED="$DOC_OPT2/wxWidgets-3.*.* $DOC_OPT2/wxMSW-3.*.* $CWX_DOCUMENTED"

    case $ac_cv_sizeof_void_p in
      8)
        case $host_cpu in
          aarch64)
            VC_LIB=lib/vc_arm64_lib
            WX_ARCH=aarch64
            ;;
          *)
            VC_LIB=lib/vc_x64_lib
            WX_ARCH=x64
            ;;
        esac
        DOC_OPT64_1=/opt/local64/pgm
        DOC_OPT64_2=/mnt/c/opt/local64/pgm
        CWX_DOCUMENTED="$DOC_OPT64_1/wxWidgets-3.*.* $DOC_OPT64_1/wxMSW-3.*.* $CWX_DOCUMENTED"
        CWX_DOCUMENTED="$DOC_OPT64_2/wxWidgets-3.*.* $DOC_OPT64_2/wxMSW-3.*.* $CWX_DOCUMENTED"
        ;;
      *)
        VC_LIB=lib/vc_lib
        WX_ARCH=x86
        DOC_OPT3=/opt/local32/pgm
        DOC_OPT4=/mnt/c/opt/local32/pgm
        CWX_DOCUMENTED="$DOC_OPT3/wxWidgets-3.*.* $DOC_OPT3/wxMSW-3.*.* $CWX_DOCUMENTED"
        CWX_DOCUMENTED="$DOC_OPT4/wxWidgets-3.*.* $DOC_OPT4/wxMSW-3.*.* $CWX_DOCUMENTED"
        ;;
    esac

    CWXPATH="$CWXWIN0 $CWXWIN1 $CWXWIN2 $CWX_DOCUMENTED $CWXWIN3 $CWXWIN4"

    for dir in $CWXPATH; do
        AC_MSG_NOTICE(Checking: [$dir])
	if test -f $dir/include/wx/wx.h; then
	    WXINCLUDE_MSVC=$dir/include/msvc
	    WXINCLUDE_PLAIN=$dir/include
	    WX_CFLAGS="-EHsc -D_UNICODE -DUNICODE -I$WXINCLUDE_MSVC -I$WXINCLUDE_PLAIN -D__WXMSW__"
	    WX_CXXFLAGS="-TP $WX_CFLAGS"
	    WX_LIBDIR=$dir/$VC_LIB
	    WX_RESCOMP="rc.sh -I$WXINCLUDE_PLAIN -D __WIN32__"
	    RC_FILE_TYPE=res
  	    base=`ls $WX_LIBDIR/wxbase*.lib 2> /dev/null | egrep 'wxbase[[0-9]]*u\.lib'`
  	    corelib_number=`echo $base | sed 's,.*\([[0-9]].\)u\.lib,\1,'`
	    if test '!' -z "$corelib_number"; then
		WXLIBNO=$corelib_number
		WX_LIBS0="wxmsw${WXLIBNO}u_stc wxmsw${WXLIBNO}u_xrc wxmsw${WXLIBNO}u_html"
		WX_LIBS1="wxmsw${WXLIBNO}u_adv wxbase${WXLIBNO}u_xml wxmsw${WXLIBNO}u_core"
		WX_LIBS2="wxbase${WXLIBNO}u wxmsw${WXLIBNO}u_gl wxmsw${WXLIBNO}u_aui"
		WX_LIBS3="wxregexu wxexpat wxtiff wxjpeg wxpng wxzlib"
		WX_SYSLIBS="winspool winmm oleaut32 ole32 gdiplus"
		WX_LIBS_STATIC="-L$WX_LIBDIR"
		for lib in $WX_LIBS0 $WX_LIBS1 $WX_LIBS2 $WX_LIBS3 $WX_SYSLIBS; do
		   WX_LIBS_STATIC="$WX_LIBS_STATIC -l$lib"
		done
                WXDIR=$dir
		AC_MSG_RESULT([Found: $dir])
		break
	    fi
	fi
    done

    if test -z "$WX_LIBS_STATIC"; then
	AC_MSG_RESULT([failed])
	WX_MSG_ERROR([Cannot find core lib version for wxWidgets])
    fi

    AC_MSG_CHECKING(for edge webview loader)
    WEBVIEW_DLL=$WXDIR/3rdparty/webview2/build/native/$WX_ARCH/WebView2Loader.dll
    if test -f "$WEBVIEW_DLL"; then
	AC_MSG_RESULT([$WEBVIEW_DLL])
        WX_WEBVIEW_DLL=$WEBVIEW_DLL
    else
        AC_MSG_RESULT(not found in $WEBVIEW_DLL)
        WX_WEBVIEW_DLL=
    fi


    WX_HAVE_STATIC_LIBS=true
    AC_SUBST(WX_CFLAGS)
    AC_SUBST(WX_CXXFLAGS)
    AC_SUBST(WX_LIBS_STATIC)
    AC_SUBST(WX_RESCOMP)
    AC_SUBST(WX_WEBVIEW_DLL)
    AC_DEFINE([WXE_WEBVIEW])
  ])

AS_IF([test "$WXERL_CAN_BUILD_DRIVER" != "false"],
[

AC_SUBST(WX_HAVE_STATIC_LIBS)
AC_SUBST(RC_FILE_TYPE)

AC_MSG_CHECKING(for wxwidgets 3.0 compatibility )
AC_LANG_PUSH(C++)
saved_CXXFLAGS=$CXXFLAGS
CXXFLAGS="$CXXFLAGS $WX_CXXFLAGS"

AC_COMPILE_IFELSE(
 [AC_LANG_PROGRAM([[
  #include "wx/wx.h"
  ]], [[
  #if wxMAJOR_VERSION > 2 && (wxMINOR_VERSION == 0 || WXWIN_COMPATIBILITY_3_0 == 1)
  ;
  #else
  #error barf
  #endif
  ]])],
  HAVE_COMPAT30_SUPPORT=yes, HAVE_COMPAT30_SUPPORT=no)

CXXFLAGS=$saved_CXXFLAGS
AC_LANG_POP(C++)
AC_MSG_RESULT($HAVE_COMPAT30_SUPPORT)

if test X"$HAVE_COMPAT30_SUPPORT" != X"yes" ; then
   WX_MSG_ERROR([wxWidgets was not compiled with --enable-compat30, wx will NOT be useable])
fi

AC_MSG_CHECKING(for wxwidgets opengl support)
AC_LANG_PUSH(C++)
saved_CXXFLAGS=$CXXFLAGS
CXXFLAGS="$CXXFLAGS $WX_CXXFLAGS"

AC_COMPILE_IFELSE(
 [AC_LANG_PROGRAM([[
  #include "wx/wx.h"
  ]], [[
  #if wxUSE_GLCANVAS
  ;
  #else
  #error barf
  #endif
  ]])],
[HAVE_GL_SUPPORT=yes
 AC_DEFINE(HAVE_GL_SUPPORT, [1], [Define if wxwidgets have gl support])],
 HAVE_GL_SUPPORT=no)

CXXFLAGS=$saved_CXXFLAGS
AC_LANG_POP(C++)
AC_MSG_RESULT($HAVE_GL_SUPPORT)
AC_SUBST(HAVE_GL_SUPPORT)

if test X"$HAVE_GL_SUPPORT" != X"yes" ; then
   WX_MSG_ERROR([wxWidgets don't have gl support, wx will NOT be useable])
fi

dnl Check for GLintptr
dnl We define the types glext.h so we don't depend on glext.h is updated
dnl to the latest version, but some systems defined them in gl.h 
dnl i.e Darwin and Solaris.

AC_CHECK_TYPES([GLintptr, GLintptrARB, GLchar, 
                GLcharARB, GLhalfARB, GLint64EXT], 
	       [], [], 
	       [#ifdef WIN32
		# include <windows.h>
	        # include <gl/gl.h>
	        #elif defined(HAVE_GL_GL_H)
	        # include <GL/gl.h>
                #elif defined(HAVE_OPENGL_GL_H)
		# include <OpenGL/gl.h>
		#endif 
		])

AC_MSG_CHECKING(GLU Callbacks uses Tiger Style)
AC_LANG_PUSH(C++)
saved_CXXFLAGS=$CXXFLAGS
CXXFLAGS="$CXXFLAGS $WX_CXXFLAGS"

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
                #ifdef WIN32
		# include <windows.h>
	        # include <gl/glu.h>
	        #elif defined(HAVE_GL_GL_H)
	        # include <GL/glu.h>
                #elif defined(HAVE_OPENGL_GL_H)
		# include <OpenGL/glu.h>
		#endif 
		#ifndef CALLBACK
		# define CALLBACK
		#endif
		void CALLBACK foo() {};
	       ]], [[
		   GLUtesselator* tess;
		   gluTessCallback(tess,GLU_TESS_VERTEX,(GLvoid (*)(...)) foo);
		]])],[TESS_CB_TIGER_STYLE=yes
		 AC_DEFINE(TESS_CB_TIGER_STYLE, [1], [GLU Callbacks are Tiger style])],[TESS_CB_TIGER_STYLE=no])
AC_MSG_RESULT($TESS_CB_TIGER_STYLE)
AC_SUBST(TESS_CB_TIGER_STYLE)

dnl
dnl Check that we can link all the libraries
dnl

AC_MSG_CHECKING(if we can link wxwidgets programs)
saved_LIBS=$LIBS

if test X"$WX_HAVE_STATIC_LIBS" = X"true" ; then
   LIBS=$WX_LIBS_STATIC   
else
   LIBS=$WX_LIBS
fi

AC_LINK_IFELSE([AC_LANG_SOURCE([[
                #include "wx/wx.h"
		#include "wx/stc/stc.h"


		class MyApp : public wxApp
		{
		   virtual bool OnInit() {
		   	   // Test that we have a FromUTF8
			   // it isn't in too old wxWidgets versions
		   	   wxString test = wxString::FromUTF8((const char *)"foo");
		   	   wxStyledTextCtrl * foo = new wxStyledTextCtrl();
			   return foo;
                  };
		};
		IMPLEMENT_APP(MyApp)
               ]])],
	       [
		CAN_LINK_WX=yes
	       ],
	       [
		CAN_LINK_WX=no
	       ])

CXXFLAGS=$saved_CXXFLAGS
LIBS=$saved_LIBS
AC_LANG_POP(C++)
AC_MSG_RESULT($CAN_LINK_WX)

if test X"$CAN_LINK_WX" != X"yes" ; then
   WX_MSG_ERROR([Can not link wx program are all developer packages installed?])
fi

]) dnl - if test "$WXERL_CAN_BUILD_DRIVER" != "false"

LDFLAGS="$LDFLAGS$EXTRA_LDFLAGS"

AC_SUBST(WXERL_CAN_BUILD_DRIVER)

AS_IF([test "x$GCC" = xyes],
  [
    # Treat certain GCC warnings as errors
    LM_TRY_ENABLE_CFLAG([-Werror=return-type], [CFLAGS])
    LM_TRY_ENABLE_CFLAG([-Werror=return-type], [CXXFLAGS])
  ])

#############################################################################

dnl 

case $host_os in
    mingw32|win32)
	RUN_ERL=werl
	SO_EXT=.dll;;
    *)
	RUN_ERL=erl
	SO_EXT=.so;;
esac

AC_SUBST(SO_EXT)
AC_SUBST(RUN_ERL)
AC_SUBST(CXXNOOPTFLAGS)

WXERL_SYS_TYPE=$TARGET

AC_SUBST(WXERL_SYS_TYPE)

mkdir -p $WXERL_SYS_TYPE
CONFIG_STATUS=$WXERL_SYS_TYPE/config.status

ERL_POP_WERROR

dnl

AC_CONFIG_FILES([ 
	config.mk
	c_src/Makefile 
        ])

AC_OUTPUT

CORES=`ls core* 2>/dev/null`
if test X"$CORES" != X"" ; then
   echo "Configure dumped core files" > ignore_core_files
fi
