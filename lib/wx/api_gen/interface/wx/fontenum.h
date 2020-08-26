/////////////////////////////////////////////////////////////////////////////
// Name:        fontenum.h
// Purpose:     interface of wxFontEnumerator
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxFontEnumerator

    wxFontEnumerator enumerates either all available fonts on the system or only
    the ones with given attributes - either only fixed-width (suited for use in
    programs such as terminal emulators and the like) or the fonts available in
    the given encoding).

    To do this, you just have to call one of EnumerateXXX() functions - either
    wxFontEnumerator::EnumerateFacenames() or wxFontEnumerator::EnumerateEncodings()
    and the corresponding callback (wxFontEnumerator::OnFacename() or
    wxFontEnumerator::OnFontEncoding()) will be called repeatedly until either
    all fonts satisfying the specified criteria are exhausted or the callback
    returns @false.

    @section fontenum_virtual Virtual functions to override

    Either OnFacename or OnFontEncoding should be overridden depending on
    whether you plan to call EnumerateFacenames or EnumerateEncodings.
    Of course, if you call both of them, you should override both functions.

    @library{wxcore}
    @category{gdi}

    @see @ref overview_fontencoding, @ref page_samples_font, wxFont, wxFontMapper
*/
class wxFontEnumerator
{
public:
    wxFontEnumerator();
    virtual ~wxFontEnumerator();

    /**
        Call OnFontEncoding() for each encoding supported by the given font -
        or for each encoding supported by at least some font if @a font is not specified.
    */
    virtual bool EnumerateEncodings(const wxString& font = wxEmptyString);

    /**
        Call OnFacename() for each font which supports given encoding (only if
        it is not @c wxFONTENCODING_SYSTEM) and is of fixed width
        (if @a fixedWidthOnly is @true).

        Calling this function with default arguments will result in enumerating all
        fonts available on the system.
    */
    virtual bool EnumerateFacenames(wxFontEncoding encoding = wxFONTENCODING_SYSTEM,
                                    bool fixedWidthOnly = false);

    /**
        Return array of strings containing all encodings found by
        EnumerateEncodings().
    */
    static wxArrayString GetEncodings(const wxString& facename = wxEmptyString);

    /**
        Return array of strings containing all facenames found by
        EnumerateFacenames().
    */
    static wxArrayString GetFacenames(wxFontEncoding encoding = wxFONTENCODING_SYSTEM,
                                      bool fixedWidthOnly = false);

    /**
        Returns @true if the given string is valid face name, i.e. it's the face name
        of an installed font and it can safely be used with wxFont::SetFaceName.
    */
    static bool IsValidFacename(const wxString& facename);

    /**
        Invalidate cache used by some of the methods of this class internally.

        This method should be called if the list of the fonts available on the
        system changes, for whatever reason. In particular, it is called
        automatically by wxFont::AddPrivateFont().

        @since 3.1.1
     */
    static void InvalidateCache();

    /**
        Called by EnumerateFacenames() for each match.

        Return @true to continue enumeration or @false to stop it.
    */
    virtual bool OnFacename(const wxString& font);

    /**
        Called by EnumerateEncodings() for each match.

        Return @true to continue enumeration or @false to stop it.
    */
    virtual bool OnFontEncoding(const wxString& font,
                                const wxString& encoding);
};

