/////////////////////////////////////////////////////////////////////////////
// Name:        xlocale.h
// Purpose:     interface of wxXLocale
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////


/**
    @class wxXLocale

    This class represents a locale object used by so-called xlocale API.

    Unlike wxLocale it doesn't provide any non-trivial operations but simply
    provides a portable wrapper for POSIX @c locale_t type.

    It exists solely to be provided as an argument to various @c wxFoo_l() functions
    which are the extensions of the standard locale-dependent functions (hence the
    name xlocale). These functions do exactly the same thing as the corresponding
    standard @c foo() except that instead of using the global program locale
    they use the provided wxXLocale object.

    See @ref group_funcmacro_locale for a list of wxXLocale-enabled functions.

    Conversely, if a program wanted to output the number in French locale, even if
    the current locale is different, it could use wxXLocale(wxLANGUAGE_FRENCH).


    @section xlocale_avail Availability

    This class is fully implemented only under the platforms where xlocale POSIX
    API or equivalent is available. Currently the xlocale API is available under
    most of the recent Unix systems (including Linux, various BSD and macOS) and
    Microsoft Visual C++ standard library provides a similar API starting from
    version 8 (Visual Studio 2005).

    If neither POSIX API nor Microsoft proprietary equivalent are available, this
    class is still available but works in degraded mode: the only supported locale
    is the C one and attempts to create wxXLocale object for any other locale will
    fail. You can use the preprocessor macro @c wxHAS_XLOCALE_SUPPORT to test if
    full xlocale API is available or only skeleton C locale support is present.

    Notice that wxXLocale is new in wxWidgets 2.9.0 and is not compiled in if
    @c wxUSE_XLOCALE was set to 0 during the library compilation.


    @library{wxbase}
    @category{cfg}

    @stdobjects
    @li ::wxNullXLocale

    @see wxLocale
*/
class wxXLocale
{
public:
    /**
        Creates an uninitialized locale object, IsOk() method will return @false.
    */
    wxXLocale();

    /**
        Creates the locale object corresponding to the specified language.
    */
    wxXLocale(wxLanguage lang);

    /**
        Creates the locale object corresponding to the specified locale string.
        The locale string is system-dependent, use constructor taking wxLanguage
        for better portability.
    */
    wxXLocale(const char* loc);

    /**
        Returns the global object representing the "C" locale.
        For an even shorter access to this object a global @c wxCLocale variable
        (implemented as a macro) is provided and can be used instead of calling
        this method.
    */
    static wxXLocale& GetCLocale();

    /**
        Returns @true if this object is initialized, i.e.\ represents a valid locale
        or @false otherwise.
    */
    bool IsOk() const;

    /**
        Comparison operator.
    */
    bool operator==(const wxXLocale& loc) const;
};

/**
    An empty and invalid wxXLocale object.
*/
wxXLocale wxNullXLocale;



// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_locale */
//@{

int wxIsalnum_l(wchar_t c, const wxXLocale& loc);
int wxIsalpha_l(wchar_t c, const wxXLocale& loc);
int wxIscntrl_l(wchar_t c, const wxXLocale& loc);
int wxIsdigit_l(wchar_t c, const wxXLocale& loc);
int wxIsgraph_l(wchar_t c, const wxXLocale& loc);
int wxIslower_l(wchar_t c, const wxXLocale& loc);
int wxIsprint_l(wchar_t c, const wxXLocale& loc);
int wxIspunct_l(wchar_t c, const wxXLocale& loc);
int wxIsspace_l(wchar_t c, const wxXLocale& loc);
int wxIsupper_l(wchar_t c, const wxXLocale& loc);
int wxIsxdigit_l(wchar_t c, const wxXLocale& loc);
wchar_t wxTolower_l(wchar_t c, const wxXLocale& loc);
wchar_t wxToupper_l(wchar_t c, const wxXLocale& loc);

double wxStrtod_l(const wchar_t *c, wchar_t **endptr, const wxXLocale& loc);
long wxStrtol_l(const wchar_t *c, wchar_t **endptr, int base, const wxXLocale& loc);
unsigned long wxStrtoul_l(const wchar_t *c, wchar_t **endptr, int base, const wxXLocale& loc);

//@}

