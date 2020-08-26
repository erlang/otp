/////////////////////////////////////////////////////////////////////////////
// Name:        imagtga.h
// Purpose:     interface of wxTGAHandler
// Author:      Samuel Dunn
// Licence:     wxWindows licence
////////////////////////////////////////////////////////////////////////////

/**
    @class wxTGAHandler

    This is the image handler for the TGA format.

    @library{wxcore}
    @category{gdi}

    @see wxImage, wxImageHandler, wxInitAllImageHandlers()
*/
class wxTGAHandler : public wxImageHandler
{
public:
    /**
        Default constructor for wxTGAHandler
    */
    wxTGAHandler();

    // allow the parent class's documentation through.
    virtual bool LoadFile(wxImage *image, wxInputStream& stream, bool verbose=true, int index=-1);
    virtual bool SaveFile(wxImage *image, wxOutputStream& stream, bool verbose=true);

protected:
    virtual bool DoCanRead(wxInputStream& stream);
};
