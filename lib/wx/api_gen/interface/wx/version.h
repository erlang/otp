/////////////////////////////////////////////////////////////////////////////
// Name:        version.h
// Purpose:     wxWidgets version numbers
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/** @addtogroup group_funcmacro_version */
//@{

/**
    This is a macro which evaluates to @true if the current wxWidgets version
    is at least major.minor.release.

    For example, to test if the program is compiled with wxWidgets 2.2 or
    higher, the following can be done:

    @code
        wxString s;
    #if wxCHECK_VERSION(2, 2, 0)
        if ( s.StartsWith("foo") )
    #else // replacement code for old version
        if ( strncmp(s, "foo", 3) == 0 )
    #endif
        {
            ...
        }
    @endcode

    @header{wx/version.h}
*/
#define wxCHECK_VERSION( major, minor, release )

/**
    Same as wxCHECK_VERSION() but also checks that wxSUBRELEASE_NUMBER is at
    least subrel.

    @header{wx/version.h}
*/
#define wxCHECK_VERSION_FULL( major, minor, release, subrel )

//@}

