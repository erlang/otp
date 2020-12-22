/////////////////////////////////////////////////////////////////////////////
// Name:        imagpcx.h
// Purpose:     interface of wxPCXHandler
// Author:      Samuel Dunn
// Licence:     wxWindows licence
////////////////////////////////////////////////////////////////////////////

/**
    @class wxPCXHandler

    This is the image handler for the PCX format.

    @library{wxcore}
    @category{gdi}

    @see wxImage, wxImageHandler, wxInitAllImageHandlers()
*/
class wxPCXHandler : public wxImageHandler
{
public:
    /**
        Default constructor for wxPCXHandler
    */
    wxPCXHandler();

    // allow the parent class's documentation through.
    virtual bool LoadFile(wxImage *image, wxInputStream& stream, bool verbose=true, int index=-1);
    virtual bool SaveFile(wxImage *image, wxOutputStream& stream, bool verbose=true);

protected:
    virtual bool DoCanRead(wxInputStream& stream);
};
