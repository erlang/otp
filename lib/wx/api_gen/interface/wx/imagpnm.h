/////////////////////////////////////////////////////////////////////////////
// Name:        imagpnm.h
// Purpose:     interface of wxPNMHandler
// Author:      Samuel Dunn
// Licence:     wxWindows licence
////////////////////////////////////////////////////////////////////////////

/**
    @class wxPNMHandler

    This is the image handler for the PNM format.

    @library{wxcore}
    @category{gdi}

    @see wxImage, wxImageHandler, wxInitAllImageHandlers()
*/
class wxPNMHandler : public wxImageHandler
{
public:
    /**
        Default constructor for wxPNMHandler
    */
    wxPNMHandler();

    // allow the parent class's documentation through.
    virtual bool LoadFile(wxImage *image, wxInputStream& stream, bool verbose=true, int index=-1);
    virtual bool SaveFile(wxImage *image, wxOutputStream& stream, bool verbose=true);

protected:
    virtual bool DoCanRead(wxInputStream& stream);
};
