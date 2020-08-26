/////////////////////////////////////////////////////////////////////////////
// Name:        imagpng.h
// Purpose:     interface of wxPNGHandler
// Author:      Samuel Dunn
// Licence:     wxWindows licence
////////////////////////////////////////////////////////////////////////////

#define wxIMAGE_OPTION_PNG_FORMAT                   wxT("PngFormat")
#define wxIMAGE_OPTION_PNG_BITDEPTH                 wxT("PngBitDepth")
#define wxIMAGE_OPTION_PNG_FILTER                   wxT("PngF")
#define wxIMAGE_OPTION_PNG_COMPRESSION_LEVEL        wxT("PngZL")
#define wxIMAGE_OPTION_PNG_COMPRESSION_MEM_LEVEL    wxT("PngZM")
#define wxIMAGE_OPTION_PNG_COMPRESSION_STRATEGY     wxT("PngZS")
#define wxIMAGE_OPTION_PNG_COMPRESSION_BUFFER_SIZE  wxT("PngZB")

/* These are already in interface/wx/image.h
    They were likely put there as a stopgap, but they've been there long enough
    that that location trumps moving them here.
enum
{
    wxPNG_TYPE_COLOUR = 0,
    wxPNG_TYPE_GREY = 2,
    wxPNG_TYPE_GREY_RED = 3,
    wxPNG_TYPE_PALETTE = 4
};
*/

/**
    @class wxPNGHandler

    This is the image handler for the PNG format.

    @library{wxcore}
    @category{gdi}

    @see wxImage, wxImageHandler, wxInitAllImageHandlers()
*/
class wxPNGHandler : public wxImageHandler
{
public:
    /**
    Default constructor for wxPNGHandler
    */
    wxPNGHandler();

    // let parent class's documentation through.
    virtual bool LoadFile( wxImage *image, wxInputStream& stream, bool verbose=true, int index=-1 );
    virtual bool SaveFile( wxImage *image, wxOutputStream& stream, bool verbose=true );

protected:
    virtual bool DoCanRead( wxInputStream& stream );
};
