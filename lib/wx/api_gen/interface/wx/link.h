/////////////////////////////////////////////////////////////////////////////
// Name:        link.h
// Purpose:     interface of global functions
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/** @addtogroup group_funcmacro_byteorder */
//@{

/**
    This macro can be used in conjunction with the wxFORCE_LINK_MODULE() macro
    to force the linker to include in its output a specific object file.

    In particular, you should use this macro in the source file which you want
    to force for inclusion. The @c moduleName needs to be a name not already in
    use in other wxFORCE_LINK_THIS_MODULE() macros, but is not required to be
    e.g. the same name of the source file (even if it's a good choice).

    @header{wx/link.h}
*/
#define wxFORCE_LINK_THIS_MODULE( moduleName )

/**
    This macro can be used in conjunction with the wxFORCE_LINK_THIS_MODULE()
    macro to force the linker to include in its output a specific object file.

    In particular, you should use this macro in a source file which you know
    for sure is linked in the output (e.g. the source file containing the
    @c main() of your app). The @c moduleName is the name of the module you
    want to forcefully link (i.e. the name you used in the relative
    wxFORCE_LINK_THIS_MODULE() macro.

    @header{wx/link.h}
*/
#define wxFORCE_LINK_MODULE( moduleName )

//@}

