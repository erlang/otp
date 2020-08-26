/////////////////////////////////////////////////////////////////////////////
// Name:        metafile.h
// Purpose:     interface of wxMetafileDC
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxMetafileDC

    This is a type of device context that allows a metafile object to be
    created (Windows only), and has most of the characteristics of a normal
    @b wxDC.
    The wxMetafileDC::Close member must be called after drawing into the
    device context, to return a metafile. The only purpose for this at
    present is to allow the metafile to be copied to the clipboard
    (see wxMetafile).

    Adding metafile capability to an application should be easy if you
    already write to a wxDC; simply pass the wxMetafileDC to your drawing
    function instead. You may wish to conditionally compile this code so it
    is not compiled under X (although no harm will result if you leave it in).

    Note that a metafile saved to disk is in standard Windows metafile format,
    and cannot be imported into most applications. To make it importable,
    call the function ::wxMakeMetafilePlaceable after closing your disk-based
    metafile device context.

    @library{wxcore}
    @category{dc}

    @see wxMetafile, wxDC
*/
class wxMetafileDC : public wxDC
{
public:
    /**
        Constructor.
        If no filename is passed, the metafile is created in memory.
    */
    wxMetafileDC(const wxString& filename = wxEmptyString);

    /**
        Destructor.
    */
    ~wxMetafileDC();

    /**
        This must be called after the device context is finished with.
        A metafile is returned, and ownership of it passes to the calling
        application (so it should be destroyed explicitly).
    */
    wxMetafile* Close();
};



/**
    @class wxMetafile

    A @b wxMetafile represents the MS Windows metafile object, so metafile
    operations have no effect in X. In wxWidgets, only sufficient functionality
    has been provided for copying a graphic to the clipboard; this may be extended
    in a future version.

    Presently, the only way of creating a metafile is to use a wxMetafileDC.

    @onlyfor{wxmsw}

    @library{wxcore}
    @category{gdi}

    @see wxMetafileDC
*/
class wxMetafile : public wxObject
{
public:
    /**
        Constructor.

        If a filename is given, the Windows disk metafile is read in.
        Check whether this was performed successfully by using the IsOk() member.
    */
    wxMetafile(const wxString& filename = wxEmptyString);

    /**
        Destructor.

        See @ref overview_refcount_destruct for more info.
    */
    ~wxMetafile();

    /**
        Returns @true if the metafile is valid.
    */
    bool IsOk();

    /**
        Plays the metafile into the given device context, returning
        @true if successful.
    */
    bool Play(wxDC* dc);

    /**
        Passes the metafile data to the clipboard. The metafile can no longer be
        used for anything, but the wxMetafile object must still be destroyed by
        the application.

        Below is a example of metafile, metafile device context and clipboard use
        from the @c hello.cpp example. Note the way the metafile dimensions
        are passed to the clipboard, making use of the device context's ability
        to keep track of the maximum extent of drawing commands.

        @code
        wxMetafileDC dc;
        if (dc.IsOk())
        {
            Draw(dc, false);
            wxMetafile *mf = dc.Close();
            if (mf)
            {
                bool success = mf->SetClipboard((int)(dc.MaxX() + 10), (int)(dc.MaxY() + 10));
                delete mf;
            }
        }
        @endcode
    */
    bool SetClipboard(int width = 0, int height = 0);
};



// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_gdi */
//@{

/**
    Given a filename for an existing, valid metafile (as constructed using
    wxMetafileDC) makes it into a placeable metafile by prepending a header
    containing the given bounding box. The bounding box may be obtained from a
    device context after drawing into it, using the functions wxDC::MinX(),
    wxDC::MinY(), wxDC::MaxX() and wxDC::MaxY().

    In addition to adding the placeable metafile header, this function adds the
    equivalent of the following code to the start of the metafile data:

    @code
    SetMapMode(dc, MM_ANISOTROPIC);
    SetWindowOrg(dc, minX, minY);
    SetWindowExt(dc, maxX - minX, maxY - minY);
    @endcode

    This simulates the wxMM_TEXT mapping mode, which wxWidgets assumes.

    Placeable metafiles may be imported by many Windows applications, and can
    be used in RTF (Rich Text Format) files.

    @a scale allows the specification of scale for the metafile.

    This function is only available under Windows.

    @header{wx/metafile.h}
*/
bool wxMakeMetafilePlaceable(const wxString& filename,
                              int minX, int minY,
                              int maxX, int maxY,
                              float scale = 1.0);

//@}

