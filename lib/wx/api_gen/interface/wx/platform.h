/////////////////////////////////////////////////////////////////////////////
// Name:        platform.h
// Purpose:     interface of global functions
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/** @addtogroup group_funcmacro_version */
//@{

/**
    Returns @true if the compiler being used is GNU C++ and its version is
    at least major.minor or greater. Returns @false otherwise.

    @header{wx/platform.h}
*/
#define wxCHECK_GCC_VERSION( major, minor )

/**
    Returns @true if the compiler being used is Sun CC Pro and its version is
    at least major.minor or greater. Returns @false otherwise.

    @header{wx/platform.h}
*/
#define wxCHECK_SUNCC_VERSION( major, minor )

/**
    Returns @true if the compiler being used is Visual C++ and its version is
    at least major or greater. Returns @false otherwise.

    @header{wx/platform.h}
*/
#define wxCHECK_VISUALC_VERSION( major )

/**
    Returns @true if the version of w32api headers used is major.minor or
    greater. Otherwise, and also if we are not compiling with MinGW32/Cygwin
    under Win32 at all, returns @false.

    @header{wx/platform.h}
*/
#define wxCHECK_W32API_VERSION( major, minor )

//@}

