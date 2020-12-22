/////////////////////////////////////////////////////////////////////////////
// Name:        imagxpm.h
// Purpose:     interface of wxXPMHandler
// Author:      Samuel Dunn
// Licence:     wxWindows licence
////////////////////////////////////////////////////////////////////////////

/**
    @class wxXPMHandler

    This is the image handler for the XPM format.

    @library{wxcore}
    @category{gdi}

    @see wxImage, wxImageHandler, wxInitAllImageHandlers()
*/
class wxXPMHandler : public wxImageHandler
{
public:
    /**
        Default constructor for wxXPMHandler
    */
    wxXPMHandler();

    // allow the parent class's documentation through.
    virtual bool LoadFile(wxImage *image, wxInputStream& stream, bool verbose=true, int index=-1);
    virtual bool SaveFile(wxImage *image, wxOutputStream& stream, bool verbose=true);

protected:
    virtual bool DoCanRead(wxInputStream& stream);
};