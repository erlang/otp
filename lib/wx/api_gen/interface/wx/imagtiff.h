/////////////////////////////////////////////////////////////////////////////
// Name:        imagtiff.h
// Purpose:     interface of wxTIFFHandler
// Author:      Samuel Dunn
// Licence:     wxWindows licence
////////////////////////////////////////////////////////////////////////////

#define wxIMAGE_OPTION_TIFF_BITSPERSAMPLE           wxString("BitsPerSample")
#define wxIMAGE_OPTION_TIFF_SAMPLESPERPIXEL         wxString("SamplesPerPixel")
#define wxIMAGE_OPTION_TIFF_COMPRESSION             wxString("Compression")
#define wxIMAGE_OPTION_TIFF_PHOTOMETRIC             wxString("Photometric")
#define wxIMAGE_OPTION_TIFF_IMAGEDESCRIPTOR         wxString("ImageDescriptor")

/**
    @class wxTIFFHandler

    This is the image handler for the TIFF format.

    @library{wxcore}
    @category{gdi}

    @see wxImage, wxImageHandler, wxInitAllImageHandlers()
*/
class wxTIFFHandler : public wxImageHandler
{
public:
    /**
    Default constructor for wxTIFFHandler
    */
    wxTIFFHandler();

    /**
        Retrieve the version information about the TIFF library used by this
        handler.

        @since 2.9.2
    */
    static wxVersionInfo GetLibraryVersionInfo();

    // let the parent class' (wxImageHandler) documentation through for these methods
    virtual bool LoadFile(wxImage *image, wxInputStream& stream, bool verbose=true, int index=-1);

protected:
    virtual bool SaveFile(wxImage *image, wxOutputStream& stream, bool verbose=true);
    virtual int DoGetImageCount(wxInputStream& stream);
    virtual bool DoCanRead(wxInputStream& stream);
};

