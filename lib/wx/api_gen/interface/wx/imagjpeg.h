/////////////////////////////////////////////////////////////////////////////
// Name:        imagjpeg.h
// Purpose:     interface of wxJPEGHandler
// Author:      Samuel Dunn
// Licence:     wxWindows licence
////////////////////////////////////////////////////////////////////////////

/**
    @class wxJPEGHandler

    This is the image handler for the JPEG format.

    @library{wxcore}
    @category{gdi}

    @see wxImage, wxImageHandler, wxInitAllImageHandlers()
*/
class wxJPEGHandler : public wxImageHandler
{
public:
    /**
        Default constructor for wxJPEGHandler
    */
    wxJPEGHandler();

    /**
        Retrieve the version information about the JPEG library used by this
        handler.

        @since 2.9.2
    */
    static wxVersionInfo GetLibraryVersionInfo();

    // allow the parent class's documentation through.
    virtual bool LoadFile(wxImage *image, wxInputStream& stream, bool verbose=true, int index=-1);
    virtual bool SaveFile(wxImage *image, wxOutputStream& stream, bool verbose=true);

protected:
    virtual bool DoCanRead(wxInputStream& stream);
};
