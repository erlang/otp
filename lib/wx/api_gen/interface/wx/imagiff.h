/////////////////////////////////////////////////////////////////////////////
// Name:        imagiff.h
// Purpose:     interface of wxIFFHandler
// Author:      Samuel Dunn
// Licence:     wxWindows licence
////////////////////////////////////////////////////////////////////////////

/**
    @class wxIFFHandler

    This is the image handler for the IFF format.

    @library{wxcore}
    @category{gdi}

    @see wxImage, wxImageHandler, wxInitAllImageHandlers()
*/
class wxIFFHandler : public wxImageHandler
{
public:
    /**
        Default constructor for wxIFFHandler
    */
    wxIFFHandler();

    virtual bool LoadFile(wxImage *image, wxInputStream& stream, bool verbose=true, int index=-1);
    virtual bool SaveFile(wxImage *image, wxOutputStream& stream, bool verbose=true);

protected:
    virtual bool DoCanRead(wxInputStream& stream);
};
