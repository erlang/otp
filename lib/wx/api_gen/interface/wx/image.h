/////////////////////////////////////////////////////////////////////////////
// Name:        image.h
// Purpose:     interface of wxImageHandler and wxImage
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    Possible values for the image resolution option.

    @see wxImage::GetOptionInt().
 */
enum wxImageResolution
{
    /// Resolution not specified.
    wxIMAGE_RESOLUTION_NONE = 0,

    /// Resolution specified in inches.
    wxIMAGE_RESOLUTION_INCHES = 1,

    /// Resolution specified in centimetres.
    wxIMAGE_RESOLUTION_CM = 2
};

/**
    Image resize algorithm.

    This is used with wxImage::Scale() and wxImage::Rescale().
 */
enum wxImageResizeQuality
{
    /// Simplest and fastest algorithm.
    wxIMAGE_QUALITY_NEAREST,

    /// Compromise between wxIMAGE_QUALITY_NEAREST and wxIMAGE_QUALITY_BICUBIC.
    wxIMAGE_QUALITY_BILINEAR,

    /// Highest quality but slowest execution time.
    wxIMAGE_QUALITY_BICUBIC,

    /**
    Use surrounding pixels to calculate an average that will be used for
    new pixels. This method is typically used when reducing the size of
    an image.
    */
    wxIMAGE_QUALITY_BOX_AVERAGE,

    /**
    Default image resizing algorithm used by wxImage::Scale(). Currently
    the same as wxIMAGE_QUALITY_NEAREST.
    */
    wxIMAGE_QUALITY_NORMAL,

    /**
    Best image resizing algorithm. Since version 2.9.2 this results in
    wxIMAGE_QUALITY_BOX_AVERAGE being used when reducing the size of the
    image (meaning that both the new width and height will be smaller than
    the original size). Otherwise wxIMAGE_QUALITY_BICUBIC is used.
    */
    wxIMAGE_QUALITY_HIGH
};

/**
    Possible values for PNG image type option.

    @see wxImage::GetOptionInt().
 */
enum wxImagePNGType
{
    wxPNG_TYPE_COLOUR = 0,      ///< Colour PNG image.
    wxPNG_TYPE_GREY = 2,        ///< Greyscale PNG image converted from RGB.
    wxPNG_TYPE_GREY_RED = 3,    ///< Greyscale PNG image using red as grey.
    wxPNG_TYPE_PALETTE = 4      ///< Palette encoding.
};


/**
   Image option names.
*/
#define wxIMAGE_OPTION_QUALITY                          wxString("quality")
#define wxIMAGE_OPTION_FILENAME                         wxString("FileName")
#define wxIMAGE_OPTION_RESOLUTION                       wxString("Resolution")
#define wxIMAGE_OPTION_RESOLUTIONX                      wxString("ResolutionX")
#define wxIMAGE_OPTION_RESOLUTIONY                      wxString("ResolutionY")
#define wxIMAGE_OPTION_RESOLUTIONUNIT                   wxString("ResolutionUnit")
#define wxIMAGE_OPTION_MAX_WIDTH                        wxString("MaxWidth")
#define wxIMAGE_OPTION_MAX_HEIGHT                       wxString("MaxHeight")
#define wxIMAGE_OPTION_ORIGINAL_WIDTH                   wxString("OriginalWidth")
#define wxIMAGE_OPTION_ORIGINAL_HEIGHT                  wxString("OriginalHeight")

#define wxIMAGE_OPTION_BMP_FORMAT                       wxString("wxBMP_FORMAT")
#define wxIMAGE_OPTION_CUR_HOTSPOT_X                    wxString("HotSpotX")
#define wxIMAGE_OPTION_CUR_HOTSPOT_Y                    wxString("HotSpotY")

#define wxIMAGE_OPTION_GIF_COMMENT                      wxString("GifComment")
#define wxIMAGE_OPTION_GIF_TRANSPARENCY                 wxString("Transparency")
#define wxIMAGE_OPTION_GIF_TRANSPARENCY_HIGHLIGHT       wxString("Highlight")
#define wxIMAGE_OPTION_GIF_TRANSPARENCY_UNCHANGED       wxString("Unchanged")

#define wxIMAGE_OPTION_PNG_FORMAT                       wxString("PngFormat")
#define wxIMAGE_OPTION_PNG_BITDEPTH                     wxString("PngBitDepth")
#define wxIMAGE_OPTION_PNG_FILTER                       wxString("PngF")
#define wxIMAGE_OPTION_PNG_COMPRESSION_LEVEL            wxString("PngZL")
#define wxIMAGE_OPTION_PNG_COMPRESSION_MEM_LEVEL        wxString("PngZM")
#define wxIMAGE_OPTION_PNG_COMPRESSION_STRATEGY         wxString("PngZS")
#define wxIMAGE_OPTION_PNG_COMPRESSION_BUFFER_SIZE      wxString("PngZB")

#define wxIMAGE_OPTION_TIFF_BITSPERSAMPLE               wxString("BitsPerSample")
#define wxIMAGE_OPTION_TIFF_SAMPLESPERPIXEL             wxString("SamplesPerPixel")
#define wxIMAGE_OPTION_TIFF_COMPRESSION                 wxString("Compression")
#define wxIMAGE_OPTION_TIFF_PHOTOMETRIC                 wxString("Photometric")
#define wxIMAGE_OPTION_TIFF_IMAGEDESCRIPTOR             wxString("ImageDescriptor")


enum
{
    wxBMP_24BPP        = 24, // default, do not need to set
    //wxBMP_16BPP      = 16, // wxQuantize can only do 236 colors?
    wxBMP_8BPP         =  8, // 8bpp, quantized colors
    wxBMP_8BPP_GREY    =  9, // 8bpp, rgb averaged to greys
    wxBMP_8BPP_GRAY    =  wxBMP_8BPP_GREY,
    wxBMP_8BPP_RED     = 10, // 8bpp, red used as greyscale
    wxBMP_8BPP_PALETTE = 11, // 8bpp, use the wxImage's palette
    wxBMP_4BPP         =  4, // 4bpp, quantized colors
    wxBMP_1BPP         =  1, // 1bpp, quantized "colors"
    wxBMP_1BPP_BW      =  2  // 1bpp, black & white from red
};


/**
    @class wxImageHandler

    This is the base class for implementing image file loading/saving, and
    image creation from data.
    It is used within wxImage and is not normally seen by the application.

    If you wish to extend the capabilities of wxImage, derive a class from
    wxImageHandler and add the handler using wxImage::AddHandler in your
    application initialization.

    Note that all wxImageHandlers provided by wxWidgets are part of
    the @ref page_libs_wxcore library.
    For details about the default handlers, please see the section
    @ref image_handlers in the wxImage class documentation.


    @section imagehandler_note Note (Legal Issue)

    This software is based in part on the work of the Independent JPEG Group.
    (Applies when wxWidgets is linked with JPEG support.
    wxJPEGHandler uses libjpeg created by IJG.)


    @stdobjects
    ::wxNullImage

    @library{wxcore}
    @category{gdi}

    @see wxImage, wxInitAllImageHandlers()
*/
class wxImageHandler : public wxObject
{
public:
    /**
        Default constructor.

        In your own default constructor, initialise the members
        m_name, m_extension and m_type.
    */
    wxImageHandler();

    /**
        Destroys the wxImageHandler object.
    */
    virtual ~wxImageHandler();

    /**
        Returns @true if this handler supports the image format contained in the
        given stream.

        This function doesn't modify the current stream position (because it
        restores the original position before returning; this however requires the
        stream to be seekable; see wxStreamBase::IsSeekable).
    */
    bool CanRead( wxInputStream& stream );

    /**
        Returns @true if this handler supports the image format contained in the
        file with the given name.

        This function doesn't modify the current stream position (because it
        restores the original position before returning; this however requires the
        stream to be seekable; see wxStreamBase::IsSeekable).
    */
    bool CanRead( const wxString& filename );

    /**
        Gets the preferred file extension associated with this handler.

        @see GetAltExtensions()
    */
    const wxString& GetExtension() const;

    /**
        Returns the other file extensions associated with this handler.

        The preferred extension for this handler is returned by GetExtension().

        @since 2.9.0
    */
    const wxArrayString& GetAltExtensions() const;

    /**
        If the image file contains more than one image and the image handler is capable
        of retrieving these individually, this function will return the number of
        available images.

        @param stream
            Opened input stream for reading image data.
            This function doesn't modify the current stream position (because it
            restores the original position before returning; this however requires the
            stream to be seekable; see wxStreamBase::IsSeekable).

        @return Number of available images. For most image handlers, this is 1
                (exceptions are TIFF and ICO formats as well as animated GIFs
                for which this function returns the number of frames in the
                animation).
    */
    int GetImageCount(wxInputStream& stream);

    /**
        Gets the MIME type associated with this handler.
    */
    const wxString& GetMimeType() const;

    /**
        Gets the name of this handler.
    */
    const wxString& GetName() const;

    /**
        Gets the image type associated with this handler.
    */
    wxBitmapType GetType() const;

    /**
        Loads an image from a stream, putting the resulting data into @a image.

        If the image file contains more than one image and the image handler is
        capable of retrieving these individually, @a index indicates which image
        to read from the stream.

        @param image
            The image object which is to be affected by this operation.
        @param stream
            Opened input stream for reading image data.
        @param verbose
            If set to @true, errors reported by the image handler will produce
            wxLogMessages.
        @param index
            The index of the image in the file (starting from zero).

        @return @true if the operation succeeded, @false otherwise.

        @see wxImage::LoadFile, wxImage::SaveFile, SaveFile()
    */
    virtual bool LoadFile(wxImage* image, wxInputStream& stream,
                          bool verbose = true, int index = -1);

    /**
        Saves an image in the output stream.

        @param image
            The image object which is to be affected by this operation.
        @param stream
            Opened output stream for writing the data.
        @param verbose
            If set to @true, errors reported by the image handler will produce
            wxLogMessages.

        @return @true if the operation succeeded, @false otherwise.

        @see wxImage::LoadFile, wxImage::SaveFile, LoadFile()
    */
    virtual bool SaveFile(wxImage* image, wxOutputStream& stream,
                          bool verbose = true);

    /**
        Sets the preferred file extension associated with this handler.

        @param extension
            File extension without leading dot.

        @see SetAltExtensions()
    */
    void SetExtension(const wxString& extension);

    /**
        Sets the alternative file extensions associated with this handler.

        @param extensions
            Array of file extensions.

        @see SetExtension()

        @since 2.9.0
    */
    void SetAltExtensions(const wxArrayString& extensions);

    /**
        Sets the handler MIME type.

        @param mimetype
            Handler MIME type.
    */
    void SetMimeType(const wxString& mimetype);

    /**
        Sets the handler name.

        @param name
            Handler name.
    */
    void SetName(const wxString& name);

    /**
       Sets the bitmap type for the handler.

       @param type
           The bitmap type.
    */
    void SetType(wxBitmapType type);

    /**
        Retrieve the version information about the image library used by this
        handler.

        This method is not present in wxImageHandler class itself but is
        present in a few of the classes deriving from it, currently
        wxJPEGHandler, wxPNGHandler and wxTIFFHandler. It returns the
        information about the version of the image library being used for the
        corresponding handler implementation.

        @since 2.9.2
     */
     static wxVersionInfo GetLibraryVersionInfo();

protected:
    /**
       Called to get the number of images available in a multi-image file
       type, if supported.

       NOTE: this function is allowed to change the current stream position
             since GetImageCount() will take care of restoring it later
    */
    virtual int DoGetImageCount( wxInputStream& stream );

    /**
       Called to test if this handler can read an image from the given stream.

       NOTE: this function is allowed to change the current stream position
             since CallDoCanRead() will take care of restoring it later
    */
    virtual bool DoCanRead( wxInputStream& stream ) = 0;
};


/**
    Constant used to indicate the alpha value conventionally defined as
    the complete transparency.
*/
const unsigned char wxIMAGE_ALPHA_TRANSPARENT = 0;

/**
    Constant used to indicate the alpha value conventionally defined as
    the complete opacity.
*/
const unsigned char wxIMAGE_ALPHA_OPAQUE = 0xff;

const unsigned char wxIMAGE_ALPHA_THRESHOLD = 0x80;


/**
    @class wxImage

    This class encapsulates a platform-independent image.

    An image can be created from data, or using wxBitmap::ConvertToImage.
    An image can be loaded from a file in a variety of formats, and is extensible
    to new formats via image format handlers. Functions are available to set and
    get image bits, so it can be used for basic image manipulation.

    A wxImage cannot (currently) be drawn directly to a wxDC.
    Instead, a platform-specific wxBitmap object must be created from it using
    the wxBitmap::wxBitmap(wxImage,int depth) constructor.
    This bitmap can then be drawn in a device context, using wxDC::DrawBitmap.

    More on the difference between wxImage and wxBitmap: wxImage is just a
    buffer of RGB bytes with an optional buffer for the alpha bytes. It is all
    generic, platform independent and image file format independent code. It
    includes generic code for scaling, resizing, clipping, and other manipulations
    of the image data. OTOH, wxBitmap is intended to be a wrapper of whatever is
    the native image format that is quickest/easiest to draw to a DC or to be the
    target of the drawing operations performed on a wxMemoryDC. By splitting the
    responsibilities between wxImage/wxBitmap like this then it's easier to use
    generic code shared by all platforms and image types for generic operations and
    platform specific code where performance or compatibility is needed.

    One colour value of the image may be used as a mask colour which will lead to
    the automatic creation of a wxMask object associated to the bitmap object.


    @section image_alpha Alpha channel support

    Starting from wxWidgets 2.5.0 wxImage supports alpha channel data, that is
    in addition to a byte for the red, green and blue colour components for each
    pixel it also stores a byte representing the pixel opacity.

    An alpha value of 0 corresponds to a transparent pixel (null opacity) while
    a value of 255 means that the pixel is 100% opaque.
    The constants ::wxIMAGE_ALPHA_TRANSPARENT and ::wxIMAGE_ALPHA_OPAQUE can be
    used to indicate those values in a more readable form.

    While all images have RGB data, not all images have an alpha channel. Before
    using wxImage::GetAlpha you should check if this image contains an alpha
    channel with wxImage::HasAlpha. Currently the BMP, PNG, TGA, and TIFF format
    handlers have full alpha channel support for loading so if you want to use
    alpha you have to use one of these formats. If you initialize the image
    alpha channel yourself using wxImage::SetAlpha, you should save it in
    either PNG, TGA, or TIFF format to avoid losing it as these are the only
    handlers that currently support saving with alpha.


    @section image_handlers Available image handlers

    The following image handlers are available.
    wxBMPHandler is always installed by default.
    To use other image formats, install the appropriate handler with
    wxImage::AddHandler or call ::wxInitAllImageHandlers().

    - wxBMPHandler: For loading (including alpha support) and saving, always installed.
    - wxPNGHandler: For loading and saving. Includes alpha support.
    - wxJPEGHandler: For loading and saving.
    - wxGIFHandler: For loading and saving (see below).
    - wxPCXHandler: For loading and saving (see below).
    - wxPNMHandler: For loading and saving (see below).
    - wxTIFFHandler: For loading and saving. Includes alpha support.
    - wxTGAHandler: For loading and saving. Includes alpha support.
    - wxIFFHandler: For loading only.
    - wxXPMHandler: For loading and saving.
    - wxICOHandler: For loading and saving.
    - wxCURHandler: For loading and saving.
    - wxANIHandler: For loading only.

    When saving in PCX format, wxPCXHandler will count the number of different
    colours in the image; if there are 256 or less colours, it will save as 8 bit,
    else it will save as 24 bit.

    Loading PNMs only works for ASCII or raw RGB images.
    When saving in PNM format, wxPNMHandler will always save as raw RGB.

    Saving GIFs requires images of maximum 8 bpp (see wxQuantize), and the alpha channel converted to a mask (see wxImage::ConvertAlphaToMask).
    Saving an animated GIF requires images of the same size (see wxGIFHandler::SaveAnimation)

    @library{wxcore}
    @category{gdi}

    @stdobjects
    ::wxNullImage

    @see wxBitmap, wxInitAllImageHandlers(), wxPixelData
*/
class wxImage : public wxObject
{
public:
    /**
        A simple class which stores red, green and blue values as 8 bit unsigned integers
        in the range of 0-255.
    */
    class RGBValue
    {
    public:
        /**
            Constructor for RGBValue, an object that contains values for red, green
            and blue which represent the value of a color.

            It is used by wxImage::HSVtoRGB and wxImage::RGBtoHSV, which convert
            between HSV color space and RGB color space.
        */
        RGBValue(unsigned char r=0, unsigned char g=0, unsigned char b=0);

        unsigned char red;
        unsigned char green;
        unsigned char blue;
    };

    /**
        A simple class which stores hue, saturation and value as doubles in the range 0.0-1.0.
    */
    class HSVValue
    {
    public:
        /**
            Constructor for HSVValue, an object that contains values for hue, saturation
            and value which represent the value of a color.

            It is used by wxImage::HSVtoRGB() and wxImage::RGBtoHSV(), which convert
            between HSV color space and RGB color space.
        */
        HSVValue(double h=0.0, double s=0.0, double v=0.0);

        double hue;
        double saturation;
        double value;
    };

    /**
        Creates an empty wxImage object without an alpha channel.
    */
    wxImage();

    /**
        Creates an image with the given size and clears it if requested.

        Does not create an alpha channel.

        @param width
            Specifies the width of the image.
        @param height
            Specifies the height of the image.
        @param clear
            If @true, initialize the image to black.
    */
    wxImage(int width, int height, bool clear = true);

    /**
        @overload
    */
    wxImage(const wxSize& sz, bool clear = true);

    /**
        Creates an image from data in memory. If @a static_data is @false
        then the wxImage will take ownership of the data and free it
        afterwards. For this, it has to be allocated with @e malloc.

        @param width
            Specifies the width of the image.
        @param height
            Specifies the height of the image.
        @param data
            A pointer to RGB data
        @param static_data
            Indicates if the data should be free'd after use

    */
    wxImage(int width, int height, unsigned char* data, bool static_data = false);

    /**
        @overload
    */
    wxImage(const wxSize& sz, unsigned char* data, bool static_data = false);

    /**
        Creates an image from data in memory. If @a static_data is @false
        then the wxImage will take ownership of the data and free it
        afterwards. For this, it has to be allocated with @e malloc.

        @param width
            Specifies the width of the image.
        @param height
            Specifies the height of the image.
        @param data
            A pointer to RGB data
        @param alpha
            A pointer to alpha-channel data
        @param static_data
            Indicates if the data should be free'd after use

    */
    wxImage(int width, int height, unsigned char* data, unsigned char* alpha,
            bool static_data = false );

    /**
        @overload
    */
    wxImage(const wxSize& sz, unsigned char* data, unsigned char* alpha,
            bool static_data = false);

    /**
        Creates an image from XPM data.

        @param xpmData
            A pointer to XPM image data.

        @beginWxPerlOnly
        Not supported by wxPerl.
        @endWxPerlOnly
    */
    wxImage(const char* const* xpmData);

    /**
        Creates an image from a file.

        @param name
            Name of the file from which to load the image.
        @param type
            May be one of the following:
            @li wxBITMAP_TYPE_BMP: Load a Windows bitmap file.
            @li wxBITMAP_TYPE_GIF: Load a GIF bitmap file.
            @li wxBITMAP_TYPE_JPEG: Load a JPEG bitmap file.
            @li wxBITMAP_TYPE_PNG: Load a PNG bitmap file.
            @li wxBITMAP_TYPE_PCX: Load a PCX bitmap file.
            @li wxBITMAP_TYPE_PNM: Load a PNM bitmap file.
            @li wxBITMAP_TYPE_TIFF: Load a TIFF bitmap file.
            @li wxBITMAP_TYPE_TGA: Load a TGA bitmap file.
            @li wxBITMAP_TYPE_XPM: Load a XPM bitmap file.
            @li wxBITMAP_TYPE_ICO: Load a Windows icon file (ICO).
            @li wxBITMAP_TYPE_CUR: Load a Windows cursor file (CUR).
            @li wxBITMAP_TYPE_ANI: Load a Windows animated cursor file (ANI).
            @li wxBITMAP_TYPE_ANY: Will try to autodetect the format.
        @param index
            Index of the image to load in the case that the image file contains
            multiple images. This is only used by GIF, ICO and TIFF handlers.
            The default value (-1) means "choose the default image" and is
            interpreted as the first image (index=0) by the GIF and TIFF handler
            and as the largest and most colourful one by the ICO handler.

        @remarks Depending on how wxWidgets has been configured and by which
                 handlers have been loaded, not all formats may be available.
                 Any handler other than BMP must be previously initialized with
                wxImage::AddHandler or wxInitAllImageHandlers.

        @note
            You can use GetOptionInt() to get the hotspot when loading cursor files:
            @code
            int hotspot_x = image.GetOptionInt(wxIMAGE_OPTION_CUR_HOTSPOT_X);
            int hotspot_y = image.GetOptionInt(wxIMAGE_OPTION_CUR_HOTSPOT_Y);
            @endcode

        @see LoadFile()
    */
    wxImage(const wxString& name, wxBitmapType type = wxBITMAP_TYPE_ANY, int index = -1);

    /**
        Creates an image from a file using MIME-types to specify the type.

        @param name
            Name of the file from which to load the image.
        @param mimetype
            MIME type string (for example 'image/jpeg')
        @param index
            See description in wxImage(const wxString&, wxBitmapType, int) overload.
    */
    wxImage(const wxString& name, const wxString& mimetype, int index = -1);

    /**
        Creates an image from a stream.

        @param stream
            Opened input stream from which to load the image. Currently,
            the stream must support seeking.
        @param type
            See description in wxImage(const wxString&, wxBitmapType, int) overload.
        @param index
            See description in wxImage(const wxString&, wxBitmapType, int) overload.
    */
    wxImage(wxInputStream& stream, wxBitmapType type = wxBITMAP_TYPE_ANY, int index = -1);

    /**
        Creates an image from a stream using MIME-types to specify the type.

        @param stream
            Opened input stream from which to load the image. Currently,
            the stream must support seeking.
        @param mimetype
            MIME type string (for example 'image/jpeg')
        @param index
            See description in wxImage(const wxString&, wxBitmapType, int) overload.
    */
    wxImage(wxInputStream& stream, const wxString& mimetype, int index = -1);

    /**
        Destructor.

        See @ref overview_refcount_destruct "reference-counted object destruction"
        for more info.
    */
    virtual ~wxImage();



    /**
        @name Image creation, initialization and deletion functions
    */
    //@{

    /**
        Returns an identical copy of this image.
    */
    wxImage Copy() const;

    /**
        Creates a fresh image.
        See wxImage::wxImage(int,int,bool) for more info.

        @return @true if the call succeeded, @false otherwise.
    */
    bool Create(int width, int height, bool clear = true);

    /**
        @overload
    */
    bool Create( const wxSize& sz, bool clear = true );

    /**
        Creates a fresh image.
        See wxImage::wxImage(int,int,unsigned char*,bool) for more info.

        @return @true if the call succeeded, @false otherwise.
    */
    bool Create( int width, int height, unsigned char* data, bool static_data = false );

    /**
        @overload
    */
    bool Create( const wxSize& sz, unsigned char* data, bool static_data = false );

    /**
        Creates a fresh image.
        See wxImage::wxImage(int,int,unsigned char*,unsigned char*,bool) for more info.

        @return @true if the call succeeded, @false otherwise.
    */
    bool Create( int width, int height, unsigned char* data, unsigned char* alpha, bool static_data = false );

    /**
        @overload
    */
    bool Create( const wxSize& sz, unsigned char* data, unsigned char* alpha, bool static_data = false );

    /**
        Initialize the image data with zeroes (the default) or with the
        byte value given as @a value.

        @since 2.9.0
    */
    void Clear(unsigned char value = 0);

    /**
        Destroys the image data.
    */
    void Destroy();

    /**
        Initializes the image alpha channel data.

        It is an error to call it if the image already has alpha data.
        If it doesn't, alpha data will be by default initialized to all pixels
        being fully opaque. But if the image has a mask colour, all mask pixels
        will be completely transparent.
    */
    void InitAlpha();

    //@}


    /**
        @name Image manipulation functions
    */
    //@{

    /**
        Blurs the image in both horizontal and vertical directions by the
        specified pixel @a blurRadius. This should not be used when using
        a single mask colour for transparency.

        @see BlurHorizontal(), BlurVertical()
    */
    wxImage Blur(int blurRadius) const;

    /**
        Blurs the image in the horizontal direction only. This should not be used
        when using a single mask colour for transparency.

        @see Blur(), BlurVertical()
    */
    wxImage BlurHorizontal(int blurRadius) const;

    /**
        Blurs the image in the vertical direction only. This should not be used
        when using a single mask colour for transparency.

        @see Blur(), BlurHorizontal()
    */
    wxImage BlurVertical(int blurRadius) const;

    /**
        Returns a mirrored copy of the image.
        The parameter @a horizontally indicates the orientation.
    */
    wxImage Mirror(bool horizontally = true) const;

    /**
        Copy the data of the given @a image to the specified position in this image.
    */
    void Paste(const wxImage& image, int x, int y);

    /**
        Replaces the colour specified by @e r1,g1,b1 by the colour @e r2,g2,b2.
    */
    void Replace(unsigned char r1, unsigned char g1,
                 unsigned char b1, unsigned char r2,
                 unsigned char g2, unsigned char b2);

    /**
        Changes the size of the image in-place by scaling it: after a call to this
        function,the image will have the given width and height.

        For a description of the @a quality parameter, see the Scale() function.
        Returns the (modified) image itself.

        @see Scale()
    */
    wxImage& Rescale(int width, int height,
                     wxImageResizeQuality quality = wxIMAGE_QUALITY_NORMAL);

    /**
        Changes the size of the image in-place without scaling it by adding either a
        border with the given colour or cropping as necessary.

        The image is pasted into a new image with the given @a size and background
        colour at the position @a pos relative to the upper left of the new image.

        If @a red = green = blue = -1 then use either the  current mask colour
        if set or find, use, and set a suitable mask colour for any newly exposed
        areas.

        @return The (modified) image itself.

        @see Size()
    */
    wxImage& Resize(const wxSize& size, const wxPoint& pos, int red = -1,
                    int green = -1, int blue = -1);

    /**
        Rotates the image about the given point, by @a angle radians.

        Passing @true to @a interpolating results in better image quality, but is slower.

        If the image has a mask, then the mask colour is used for the uncovered
        pixels in the rotated image background. Else, black (rgb 0, 0, 0) will be used.

        Returns the rotated image, leaving this image intact.
    */
    wxImage Rotate(double angle, const wxPoint& rotationCentre,
                   bool interpolating = true,
                   wxPoint* offsetAfterRotation = NULL) const;

    /**
        Returns a copy of the image rotated 90 degrees in the direction
        indicated by @a clockwise.
    */
    wxImage Rotate90(bool clockwise = true) const;

    /**
        Returns a copy of the image rotated by 180 degrees.

        @since 2.9.2
    */
    wxImage Rotate180() const;

    /**
        Rotates the hue of each pixel in the image by @e angle, which is a double in
        the range of -1.0 to +1.0, where -1.0 corresponds to -360 degrees and +1.0
        corresponds to +360 degrees.
    */
    void RotateHue(double angle);

    /**
        Returns a scaled version of the image.

        This is also useful for scaling bitmaps in general as the only other way
        to scale bitmaps is to blit a wxMemoryDC into another wxMemoryDC.

        The parameter @a quality determines what method to use for resampling
        the image, see wxImageResizeQuality documentation.

        It should be noted that although using @c wxIMAGE_QUALITY_HIGH produces much nicer
        looking results it is a slower method. Downsampling will use the box averaging
        method which seems to operate very fast. If you are upsampling larger images using
        this method you will most likely notice that it is a bit slower and in extreme
        cases it will be quite substantially slower as the bicubic algorithm has to process a
        lot of data.

        It should also be noted that the high quality scaling may not work as expected
        when using a single mask colour for transparency, as the scaling will blur the
        image and will therefore remove the mask partially. Using the alpha channel
        will work.

        Example:
        @code
        // get the bitmap from somewhere
        wxBitmap bmp = ...;

        // rescale it to have size of 32*32
        if ( bmp.GetWidth() != 32 || bmp.GetHeight() != 32 )
        {
            wxImage image = bmp.ConvertToImage();
            bmp = wxBitmap(image.Scale(32, 32));

            // another possibility:
            image.Rescale(32, 32);
            bmp = image;
        }
        @endcode

        @see Rescale()
    */
    wxImage Scale(int width, int height,
                   wxImageResizeQuality quality = wxIMAGE_QUALITY_NORMAL) const;

    /**
        Returns a resized version of this image without scaling it by adding either a
        border with the given colour or cropping as necessary.

        The image is pasted into a new image with the given @a size and background
        colour at the position @a pos relative to the upper left of the new image.

        If @a red = green = blue = -1 then the areas of the larger image not covered
        by this image are made transparent by filling them with the image mask colour
        (which will be allocated automatically if it isn't currently set).

        Otherwise, the areas will be filled with the colour with the specified RGB components.

        @see Resize()
    */
    wxImage Size(const wxSize& size, const wxPoint& pos, int red = -1,
                 int green = -1, int blue = -1) const;

    //@}


    /**
        @name Conversion functions
    */
    //@{

    /**
        If the image has alpha channel, this method converts it to mask.

        If the image has an alpha channel, all pixels with alpha value less
        than @a threshold are replaced with the mask colour and the alpha
        channel is removed. Otherwise nothing is done.

        The mask colour is chosen automatically using FindFirstUnusedColour(),
        see the overload below if this is not appropriate.

        @return Returns @true on success, @false on error.
    */
    bool ConvertAlphaToMask(unsigned char threshold = wxIMAGE_ALPHA_THRESHOLD);

    /**
        If the image has alpha channel, this method converts it to mask using
        the specified colour as the mask colour.

        If the image has an alpha channel, all pixels with alpha value less
        than @a threshold are replaced with the mask colour and the alpha
        channel is removed. Otherwise nothing is done.

        @since 2.9.0

        @param mr
            The red component of the mask colour.
        @param mg
            The green component of the mask colour.
        @param mb
            The blue component of the mask colour.
        @param threshold
            Pixels with alpha channel values below the given threshold are
            considered to be transparent, i.e. the corresponding mask pixels
            are set. Pixels with the alpha values above the threshold are
            considered to be opaque.

        @return Returns @true on success, @false on error.
     */
    bool ConvertAlphaToMask(unsigned char mr, unsigned char mg, unsigned char mb,
                            unsigned char threshold = wxIMAGE_ALPHA_THRESHOLD);

    /**
        Returns a greyscale version of the image.

        The returned image uses the luminance component of the original to
        calculate the greyscale. Defaults to using the standard ITU-T BT.601
        when converting to YUV, where every pixel equals
        (R * @a weight_r) + (G * @a weight_g) + (B * @a weight_b).
    */
    wxImage ConvertToGreyscale(double weight_r, double weight_g, double weight_b) const;

    /**
        Returns a greyscale version of the image.
        @since 2.9.0
    */
    wxImage ConvertToGreyscale() const;

    /**
        Returns monochromatic version of the image.

        The returned image has white colour where the original has @e (r,g,b)
        colour and black colour everywhere else.
    */
    wxImage ConvertToMono(unsigned char r, unsigned char g, unsigned char b) const;

    /**
        Returns disabled (dimmed) version of the image.
        @since 2.9.0
    */
    wxImage ConvertToDisabled(unsigned char brightness = 255) const;

    //@}


    /**
        @name Miscellaneous functions
    */
    //@{

    /**
        Computes the histogram of the image. @a histogram is a reference to
        wxImageHistogram object. wxImageHistogram is a specialization of
        wxHashMap "template" and is defined as follows:

        @code
        class WXDLLEXPORT wxImageHistogramEntry
        {
        public:
            wxImageHistogramEntry() : index(0), value(0) {}
            unsigned long index;
            unsigned long value;
        };

        WX_DECLARE_EXPORTED_HASH_MAP(unsigned long, wxImageHistogramEntry,
                                    wxIntegerHash, wxIntegerEqual,
                                    wxImageHistogram);
        @endcode

        @return Returns number of colours in the histogram.
    */
    unsigned long ComputeHistogram(wxImageHistogram& histogram) const;

    /**
        Finds the first colour that is never used in the image.
        The search begins at given initial colour and continues by increasing
        R, G and B components (in this order) by 1 until an unused colour is
        found or the colour space exhausted.

        The parameters @a r, @a g, @a b are pointers to variables to save the colour.

        The parameters @a startR, @a startG, @a startB define the initial values
        of the colour.
        The returned colour will have RGB values equal to or greater than these.

        @return Returns @false if there is no unused colour left, @true on success.

        @note
            This method involves computing the histogram, which is a
            computationally intensive operation.
    */
    bool FindFirstUnusedColour(unsigned char* r, unsigned char* g,
                               unsigned char* b, unsigned char startR = 1,
                               unsigned char startG = 0,
                               unsigned char startB = 0) const;

    /**
        Assignment operator, using @ref overview_refcount "reference counting".

        @param image
            Image to assign.

        @return Returns 'this' object.
    */
    wxImage& operator=(const wxImage& image);

    //@}


    /**
        @name Getters
    */
    //@{

    /**
        Returns pointer to the array storing the alpha values for this image.

        This pointer is @NULL for the images without the alpha channel. If the image
        does have it, this pointer may be used to directly manipulate the alpha values
        which are stored as the RGB ones.
    */
    unsigned char* GetAlpha() const;

    /**
        Returns the image data as an array.

        This is most often used when doing direct image manipulation.
        The return value points to an array of characters in RGBRGBRGB... format
        in the top-to-bottom, left-to-right order, that is the first RGB triplet
        corresponds to the first pixel of the first row, the second one ---
        to the second pixel of the first row and so on until the end of the first
        row, with second row following after it and so on.

        You should not delete the returned pointer nor pass it to SetData().
    */
    unsigned char* GetData() const;

    /**
        Return alpha value at given pixel location.
    */
    unsigned char GetAlpha(int x, int y) const;

    /**
        Returns the red intensity at the given coordinate.
    */
    unsigned char GetRed(int x, int y) const;

    /**
        Returns the green intensity at the given coordinate.
    */
    unsigned char GetGreen(int x, int y) const;

    /**
        Returns the blue intensity at the given coordinate.
    */
    unsigned char GetBlue(int x, int y) const;

    /**
        Gets the red value of the mask colour.
    */
    unsigned char GetMaskRed() const;

    /**
        Gets the green value of the mask colour.
    */
    unsigned char GetMaskGreen() const;

    /**
        Gets the blue value of the mask colour.
    */
    unsigned char GetMaskBlue() const;

    /**
        Gets the width of the image in pixels.

        @see GetHeight(), GetSize()
    */
    int GetWidth() const;

    /**
        Gets the height of the image in pixels.

        @see GetWidth(), GetSize()
    */
    int GetHeight() const;

    /**
        Returns the size of the image in pixels.

        @since 2.9.0

        @see GetHeight(), GetWidth()
    */
    wxSize GetSize() const;

    /**
        Gets a user-defined string-valued option.

        Generic options:
        @li @c wxIMAGE_OPTION_FILENAME: The name of the file from which the image
            was loaded.

        Options specific to wxGIFHandler:
        @li @c wxIMAGE_OPTION_GIF_COMMENT: The comment text that is read from
            or written to the GIF file. In an animated GIF each frame can have
            its own comment. If there is only a comment in the first frame of
            a GIF it will not be repeated in other frames.

        @param name
            The name of the option, case-insensitive.
        @return
            The value of the option or an empty string if not found. Use
            HasOption() if an empty string can be a valid option value.

        @see SetOption(), GetOptionInt(), HasOption()
    */
    wxString GetOption(const wxString& name) const;

    /**
        Gets a user-defined integer-valued option.

        The function is case-insensitive to @a name.
        If the given option is not present, the function returns 0.
        Use HasOption() if 0 is a possibly valid value for the option.

        Generic options:
        @li @c wxIMAGE_OPTION_MAX_WIDTH and @c wxIMAGE_OPTION_MAX_HEIGHT: If either
            of these options is specified, the loaded image will be scaled down
            (preserving its aspect ratio) so that its width is less than the
            max width given if it is not 0 @em and its height is less than the
            max height given if it is not 0. This is typically used for loading
            thumbnails and the advantage of using these options compared to
            calling Rescale() after loading is that some handlers (only JPEG
            one right now) support rescaling the image during loading which is
            vastly more efficient than loading the entire huge image and
            rescaling it later (if these options are not supported by the
            handler, this is still what happens however). These options must be
            set before calling LoadFile() to have any effect.

        @li @c wxIMAGE_OPTION_ORIGINAL_WIDTH and @c wxIMAGE_OPTION_ORIGINAL_HEIGHT:
            These options will return the original size of the image if either
            @c wxIMAGE_OPTION_MAX_WIDTH or @c wxIMAGE_OPTION_MAX_HEIGHT is
            specified.
            @since 2.9.3

        @li @c wxIMAGE_OPTION_QUALITY: JPEG quality used when saving. This is an
            integer in 0..100 range with 0 meaning very poor and 100 excellent
            (but very badly compressed). This option is currently ignored for
            the other formats.

        @li @c wxIMAGE_OPTION_RESOLUTIONUNIT: The value of this option determines
            whether the resolution of the image is specified in centimetres or
            inches, see wxImageResolution enum elements.

        @li @c wxIMAGE_OPTION_RESOLUTION, @c wxIMAGE_OPTION_RESOLUTIONX and
            @c wxIMAGE_OPTION_RESOLUTIONY: These options define the resolution of
            the image in the units corresponding to @c wxIMAGE_OPTION_RESOLUTIONUNIT
            options value. The first option can be set before saving the image
            to set both horizontal and vertical resolution to the same value.
            The X and Y options are set by the image handlers if they support
            the image resolution (currently BMP, JPEG and TIFF handlers do) and
            the image provides the resolution information and can be queried
            after loading the image.

        Options specific to wxPNGHandler:
        @li @c wxIMAGE_OPTION_PNG_FORMAT: Format for saving a PNG file, see
            wxImagePNGType for the supported values.
        @li @c wxIMAGE_OPTION_PNG_BITDEPTH: Bit depth for every channel (R/G/B/A).
        @li @c wxIMAGE_OPTION_PNG_FILTER: Filter for saving a PNG file, see libpng
            (http://www.libpng.org/pub/png/libpng-1.2.5-manual.html) for possible values
            (e.g. PNG_FILTER_NONE, PNG_FILTER_SUB, PNG_FILTER_UP, etc).
        @li @c wxIMAGE_OPTION_PNG_COMPRESSION_LEVEL: Compression level (0..9) for
            saving a PNG file. An high value creates smaller-but-slower PNG file.
            Note that unlike other formats (e.g. JPEG) the PNG format is always
            lossless and thus this compression level doesn't tradeoff the image
            quality.
        @li @c wxIMAGE_OPTION_PNG_COMPRESSION_MEM_LEVEL: Compression memory usage
            level (1..9) for saving a PNG file. An high value means the saving
            process consumes more memory, but may create smaller PNG file.
        @li @c wxIMAGE_OPTION_PNG_COMPRESSION_STRATEGY: Possible values are 0 for
            default strategy, 1 for filter, and 2 for Huffman-only.
            You can use OptiPNG (http://optipng.sourceforge.net/) to get a suitable
            value for your application.
        @li @c wxIMAGE_OPTION_PNG_COMPRESSION_BUFFER_SIZE: Internal buffer size
            (in bytes) for saving a PNG file. Ideally this should be as big as
            the resulting PNG file. Use this option if your application produces
            images with small size variation.

        Options specific to wxTIFFHandler:
        @li @c wxIMAGE_OPTION_TIFF_BITSPERSAMPLE: Number of bits per
            sample (channel). Currently values of 1 and 8 are supported. A
            value of 1 results in a black and white image. A value of 8 (the
            default) can mean greyscale or RGB, depending on the value of
            @c wxIMAGE_OPTION_TIFF_SAMPLESPERPIXEL.
        @li @c wxIMAGE_OPTION_TIFF_SAMPLESPERPIXEL: Number of samples
            (channels) per pixel. Currently values of 1 and 3 are supported.
            A value of 1 results in either a greyscale (by default) or black and
            white image, depending on the value of
            @c wxIMAGE_OPTION_TIFF_BITSPERSAMPLE. A value of 3 (the default)
            will result in an RGB image.
        @li @c wxIMAGE_OPTION_TIFF_COMPRESSION: Compression type. By default
            it is set to 1 (COMPRESSION_NONE). Typical other values are
            5 (COMPRESSION_LZW) and 7 (COMPRESSION_JPEG). See tiff.h for more
            options.
        @li @c wxIMAGE_OPTION_TIFF_PHOTOMETRIC: Specifies the photometric
            interpretation. By default it is set to 2 (PHOTOMETRIC_RGB) for RGB
            images and 0 (PHOTOMETRIC_MINISWHITE) for greyscale or black and
            white images. It can also be set to 1 (PHOTOMETRIC_MINISBLACK) to
            treat the lowest value as black and highest as white.
            If you want a greyscale image it is also sufficient to only specify
            @c wxIMAGE_OPTION_TIFF_PHOTOMETRIC and set it to either
            PHOTOMETRIC_MINISWHITE or PHOTOMETRIC_MINISBLACK. The other values
            are taken care of.

        Options specific to wxGIFHandler:
        @li @c wxIMAGE_OPTION_GIF_TRANSPARENCY: How to deal with transparent pixels.
            By default, the color of transparent pixels is changed to bright pink, so
            that if the image is accidentally drawn without transparency, it will be
            obvious.
            Normally, this would not be noticed, as these pixels will not be rendered.
            But in some cases it might be useful to load a GIF without making any
            modifications to its colours.
            Use @c wxIMAGE_OPTION_GIF_TRANSPARENCY_UNCHANGED to keep the colors correct.
            Use @c wxIMAGE_OPTION_GIF_TRANSPARENCY_HIGHLIGHT to convert transparent pixels
            to pink (default).
            This option has been added in wxWidgets 3.1.1.

        @note
        Be careful when combining the options @c wxIMAGE_OPTION_TIFF_SAMPLESPERPIXEL,
        @c wxIMAGE_OPTION_TIFF_BITSPERSAMPLE, and @c wxIMAGE_OPTION_TIFF_PHOTOMETRIC.
        While some measures are taken to prevent illegal combinations and/or
        values, it is still easy to abuse them and come up with invalid
        results in the form of either corrupted images or crashes.

        @param name
            The name of the option, case-insensitive.
        @return
            The value of the option or 0 if not found.
            Use HasOption() if 0 can be a valid option value.

        @see SetOption(), GetOption()
    */
    int GetOptionInt(const wxString& name) const;

    /**
        Get the current mask colour or find a suitable unused colour that could be
        used as a mask colour. Returns @true if the image currently has a mask.
    */
    bool GetOrFindMaskColour(unsigned char* r, unsigned char* g,
                             unsigned char* b) const;

    /**
        Returns the palette associated with the image.
        Currently the palette is only used when converting to wxBitmap under Windows.

        Some of the wxImage handlers have been modified to set the palette if
        one exists in the image file (usually 256 or less colour images in
        GIF or PNG format).
    */
    const wxPalette& GetPalette() const;

    /**
        Returns a sub image of the current one as long as the rect belongs entirely
        to the image.
    */
    wxImage GetSubImage(const wxRect& rect) const;

    /**
        Gets the type of image found by LoadFile() or specified with SaveFile().

        @since 2.9.0
    */
    wxBitmapType GetType() const;

    /**
        Returns @true if this image has alpha channel, @false otherwise.

        @see GetAlpha(), SetAlpha()
    */
    bool HasAlpha() const;

    /**
        Returns @true if there is a mask active, @false otherwise.
    */
    bool HasMask() const;

    /**
        Returns @true if the given option is present.
        The function is case-insensitive to @a name.

        The lists of the currently supported options are in GetOption() and
        GetOptionInt() function docs.

        @see SetOption(), GetOption(), GetOptionInt()
    */
    bool HasOption(const wxString& name) const;

    /**
        Returns @true if image data is present.
    */
    bool IsOk() const;

    /**
        Returns @true if the given pixel is transparent, i.e.\ either has the mask
        colour if this image has a mask or if this image has alpha channel and alpha
        value of this pixel is strictly less than @a threshold.
    */
    bool IsTransparent(int x, int y,
                       unsigned char threshold = wxIMAGE_ALPHA_THRESHOLD) const;

    //@}


    /**
        @name Loading and saving functions
    */
    //@{

    /**
        Loads an image from an input stream.

        If the file can't be loaded, this function returns false and logs an
        error using wxLogError(). If the file can be loaded but some problems
        were detected while doing it, it can also call wxLogWarning() to notify
        about these problems. If this is undesirable, use SetLoadFlags() to
        reset @c Load_Verbose flag and suppress these warnings.

        @param stream
            Opened input stream from which to load the image.
            Currently, the stream must support seeking.
        @param type
            May be one of the following:
            @li wxBITMAP_TYPE_BMP: Load a Windows bitmap file.
            @li wxBITMAP_TYPE_GIF: Load a GIF bitmap file.
            @li wxBITMAP_TYPE_JPEG: Load a JPEG bitmap file.
            @li wxBITMAP_TYPE_PNG: Load a PNG bitmap file.
            @li wxBITMAP_TYPE_PCX: Load a PCX bitmap file.
            @li wxBITMAP_TYPE_PNM: Load a PNM bitmap file.
            @li wxBITMAP_TYPE_TIFF: Load a TIFF bitmap file.
            @li wxBITMAP_TYPE_TGA: Load a TGA bitmap file.
            @li wxBITMAP_TYPE_XPM: Load a XPM bitmap file.
            @li wxBITMAP_TYPE_ICO: Load a Windows icon file (ICO).
            @li wxBITMAP_TYPE_CUR: Load a Windows cursor file (CUR).
            @li wxBITMAP_TYPE_ANI: Load a Windows animated cursor file (ANI).
            @li wxBITMAP_TYPE_ANY: Will try to autodetect the format.
        @param index
            Index of the image to load in the case that the image file contains
            multiple images. This is only used by GIF, ICO and TIFF handlers.
            The default value (-1) means "choose the default image" and is
            interpreted as the first image (index=0) by the GIF and TIFF handler
            and as the largest and most colourful one by the ICO handler.

        @return @true if the operation succeeded, @false otherwise.
                If the optional index parameter is out of range, @false is
                returned and a call to wxLogError() takes place.

        @remarks Depending on how wxWidgets has been configured, not all formats
                 may be available.

        @note
            You can use GetOptionInt() to get the hotspot when loading cursor files:
            @code
            int hotspot_x = image.GetOptionInt(wxIMAGE_OPTION_CUR_HOTSPOT_X);
            int hotspot_y = image.GetOptionInt(wxIMAGE_OPTION_CUR_HOTSPOT_Y);
            @endcode

        @see SaveFile()
    */
    virtual bool LoadFile(wxInputStream& stream,
                          wxBitmapType type = wxBITMAP_TYPE_ANY,
                          int index = -1);

    /**
        Loads an image from a file.
        If no handler type is provided, the library will try to autodetect the format.

        @param name
            Name of the file from which to load the image.
        @param type
            See the description in the LoadFile(wxInputStream&, wxBitmapType, int) overload.
        @param index
            See the description in the LoadFile(wxInputStream&, wxBitmapType, int) overload.
    */
    virtual bool LoadFile(const wxString& name,
                          wxBitmapType type = wxBITMAP_TYPE_ANY,
                          int index = -1);

    /**
        Loads an image from a file.
        If no handler type is provided, the library will try to autodetect the format.

        @param name
            Name of the file from which to load the image.
        @param mimetype
            MIME type string (for example 'image/jpeg')
        @param index
            See the description in the LoadFile(wxInputStream&, wxBitmapType, int) overload.
    */
    virtual bool LoadFile(const wxString& name, const wxString& mimetype,
                          int index = -1);

    /**
        Loads an image from an input stream.

        @param stream
            Opened input stream from which to load the image.
            Currently, the stream must support seeking.
        @param mimetype
            MIME type string (for example 'image/jpeg')
        @param index
            See the description in the LoadFile(wxInputStream&, wxBitmapType, int) overload.
    */
    virtual bool LoadFile(wxInputStream& stream, const wxString& mimetype,
                          int index = -1);

    /**
        Saves an image in the given stream.

        @param stream
            Opened output stream to save the image to.
        @param mimetype
            MIME type.

        @return @true if the operation succeeded, @false otherwise.

        @remarks Depending on how wxWidgets has been configured, not all formats
                 may be available.

        @note
            You can use SetOption() to set the hotspot when saving an image
            into a cursor file (default hotspot is in the centre of the image):
            @code
            image.SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_X, hotspotX);
            image.SetOption(wxIMAGE_OPTION_CUR_HOTSPOT_Y, hotspotY);
            @endcode

        @see LoadFile()
    */
    virtual bool SaveFile(wxOutputStream& stream,
                          const wxString& mimetype) const;

    /**
        Saves an image in the named file.

        @param name
            Name of the file to save the image to.
        @param type
            Currently these types can be used:
            @li wxBITMAP_TYPE_BMP: Save a BMP image file.
            @li wxBITMAP_TYPE_JPEG: Save a JPEG image file.
            @li wxBITMAP_TYPE_PNG: Save a PNG image file.
            @li wxBITMAP_TYPE_PCX: Save a PCX image file
                (tries to save as 8-bit if possible, falls back to 24-bit otherwise).
            @li wxBITMAP_TYPE_PNM: Save a PNM image file (as raw RGB always).
            @li wxBITMAP_TYPE_TIFF: Save a TIFF image file.
            @li wxBITMAP_TYPE_XPM: Save a XPM image file.
            @li wxBITMAP_TYPE_ICO: Save a Windows icon file (ICO).
                The size may be up to 255 wide by 127 high. A single image is saved
                in 8 colors at the size supplied.
            @li wxBITMAP_TYPE_CUR: Save a Windows cursor file (CUR).
    */
    virtual bool SaveFile(const wxString& name, wxBitmapType type) const;

    /**
        Saves an image in the named file.

        @param name
            Name of the file to save the image to.
        @param mimetype
            MIME type.
    */
    virtual bool SaveFile(const wxString& name, const wxString& mimetype) const;

    /**
        Saves an image in the named file.

        File type is determined from the extension of the file name.
        Note that this function may fail if the extension is not recognized!
        You can use one of the forms above to save images to files with
        non-standard extensions.

        @param name
            Name of the file to save the image to.
    */
    virtual bool SaveFile(const wxString& name) const;

    /**
        Saves an image in the given stream.

        @param stream
            Opened output stream to save the image to.
        @param type
            MIME type.
    */
    virtual bool SaveFile(wxOutputStream& stream, wxBitmapType type) const;

    //@}



    /**
        @name Setters
    */
    //@{

    /**
       This function is similar to SetData() and has similar restrictions.

        The pointer passed to it may however be @NULL in which case the function
        will allocate the alpha array internally -- this is useful to add alpha
        channel data to an image which doesn't have any.

        If the pointer is not @NULL, it must have one byte for each image pixel
        and be allocated with malloc().
        wxImage takes ownership of the pointer and will free it unless @a static_data
        parameter is set to @true -- in this case the caller should do it.
    */
    void SetAlpha(unsigned char* alpha = NULL,
                  bool static_data = false);

    /**
        Sets the alpha value for the given pixel.
        This function should only be called if the image has alpha channel data,
        use HasAlpha() to check for this.
    */
    void SetAlpha(int x, int y, unsigned char alpha);

    /**
        Removes the alpha channel from the image.

        This function should only be called if the image has alpha channel data,
        use HasAlpha() to check for this.

        @since 2.9.1
    */
    void ClearAlpha();

    /**
        Sets the image data without performing checks.

        The data given must have the size (width*height*3) or results will be
        unexpected. Don't use this method if you aren't sure you know what you
        are doing.

        The data must have been allocated with @c malloc(), @b NOT with
        @c operator new.

        If @a static_data is @false, after this call the pointer to the data is
        owned by the wxImage object, that will be responsible for deleting it.
        Do not pass to this function a pointer obtained through GetData().
    */
    void SetData(unsigned char* data, bool static_data = false);

    /**
        @overload
    */
    void SetData(unsigned char* data, int new_width, int new_height,
                 bool static_data = false);

    /**
        Sets the default value for the flags used for loading image files.

        This method changes the global value of the flags used for all the
        subsequently created wxImage objects by default. It doesn't affect the
        already existing objects.

        By default, the global flags include @c Load_Verbose flag value.

        @see LoadFile(), SetLoadFlags(), GetDefaultLoadFlags()

        @since 3.1.0
     */
    static void SetDefaultLoadFlags(int flags);

    /**
        Sets the flags used for loading image files by this object.

        The flags will affect any future calls to LoadFile() for this object.
        To change the flags for all image objects, call SetDefaultLoadFlags()
        before creating any of them.

        Currently the only defined flag is @c Load_Verbose which determines if
        the non-fatal (i.e. not preventing the file from being loaded
        completely) problems should result in the calls to wxLogWarning()
        function. It is recommended to customize handling of these warnings by
        e.g. defining a custom log target (see @ref overview_log), but if such
        warnings should be completely suppressed, clearing this flag provides a
        simple way to do it, for example:
        @code
            wxImage image;
            image.SetLoadFlags(image.GetLoadFlags() & ~wxImage::Load_Verbose);
            image.LoadFile(...);
        @endcode

        @see LoadFile(), SetLoadFlags(), GetLoadFlags()

        @since 3.1.0
     */
    void SetLoadFlags(int flags);

    /**
        Specifies whether there is a mask or not.

        The area of the mask is determined by the current mask colour.
    */
    void SetMask(bool hasMask = true);

    /**
        Sets the mask colour for this image (and tells the image to use the mask).
    */
    void SetMaskColour(unsigned char red, unsigned char green,
                       unsigned char blue);

    /**
        Sets image's mask so that the pixels that have RGB value of mr,mg,mb in
        mask will be masked in the image.

        This is done by first finding an unused colour in the image, setting
        this colour as the mask colour and then using this colour to draw all
        pixels in the image who corresponding pixel in mask has given RGB value.

        The parameter @a mask is the mask image to extract mask shape from.
        It must have the same dimensions as the image.

        The parameters @a mr, @a mg, @a mb are the RGB values of the pixels in
        mask that will be used to create the mask.

        @return Returns @false if mask does not have same dimensions as the image
                or if there is no unused colour left. Returns @true if the mask
                was successfully applied.

        @note
            Note that this method involves computing the histogram, which is a
            computationally intensive operation.
    */
    bool SetMaskFromImage(const wxImage& mask, unsigned char mr,
                          unsigned char mg,
                          unsigned char mb);

    /**
        Sets a user-defined option. The function is case-insensitive to @a name.

        For example, when saving as a JPEG file, the option @b quality is
        used, which is a number between 0 and 100 (0 is terrible, 100 is very good).

        The lists of the currently supported options are in GetOption() and
        GetOptionInt() function docs.

        @see GetOption(), GetOptionInt(), HasOption()
    */
    void SetOption(const wxString& name, const wxString& value);

    /**
        @overload
    */
    void SetOption(const wxString& name, int value);

    /**
        Associates a palette with the image.

        The palette may be used when converting wxImage to wxBitmap (MSW only at present)
        or in file save operations (none as yet).
    */
    void SetPalette(const wxPalette& palette);

    /**
       Set the color of the pixel at the given x and y coordinate.
    */

    void SetRGB( int x, int y, unsigned char r, unsigned char g, unsigned char b );

    /**
        Sets the colour of the pixels within the given rectangle.

        This routine performs bounds-checks for the coordinate so it can be considered
        a safe way to manipulate the data.
    */
    void SetRGB(const wxRect& rect,
                unsigned char red,
                unsigned char green,
                unsigned char blue);

    /**
        Set the type of image returned by GetType().

        This method is mostly used internally by the library but can also be
        called from the user code if the image was created from data in the
        given bitmap format without using LoadFile() (which would set the type
        correctly automatically).

        Notice that the image must be created before this function is called.

        @since 2.9.0

        @param type
            One of bitmap type constants, @c wxBITMAP_TYPE_INVALID is a valid
            value for it and can be used to reset the bitmap type to default
            but @c wxBITMAP_TYPE_MAX is not allowed here.
    */
    void SetType(wxBitmapType type);

    //@}



    /**
        @name Handler management functions
    */
    //@{

    /**
        Register an image handler.

        Typical example of use:
        @code
            wxImage::AddHandler(new wxPNGHandler);
        @endcode

        See @ref image_handlers for a list of the available handlers. You can
        also use ::wxInitAllImageHandlers() to add handlers for all the image
        formats supported by wxWidgets at once.

        @param handler
            A heap-allocated handler object which will be deleted by wxImage
            if it is removed later by RemoveHandler() or at program shutdown.
    */
    static void AddHandler(wxImageHandler* handler);

    /**
        Deletes all image handlers.
        This function is called by wxWidgets on exit.
    */
    static void CleanUpHandlers();

    /**
        Finds the handler with the given name.

        @param name
            The handler name.

        @return A pointer to the handler if found, @NULL otherwise.

        @see wxImageHandler
    */
    static wxImageHandler* FindHandler(const wxString& name);

    /**
        Finds the handler associated with the given extension and type.

        @param extension
            The file extension, such as "bmp".
        @param imageType
            The image type; one of the ::wxBitmapType values.

        @return A pointer to the handler if found, @NULL otherwise.

        @see wxImageHandler
    */
    static wxImageHandler* FindHandler(const wxString& extension,
                                       wxBitmapType imageType);

    /**
        Finds the handler associated with the given image type.

        @param imageType
            The image type; one of the ::wxBitmapType values.

        @return A pointer to the handler if found, @NULL otherwise.

        @see wxImageHandler
    */
    static wxImageHandler* FindHandler(wxBitmapType imageType);

    /**
        Finds the handler associated with the given MIME type.

        @param mimetype
            MIME type.

        @return A pointer to the handler if found, @NULL otherwise.

        @see wxImageHandler
    */
    static wxImageHandler* FindHandlerMime(const wxString& mimetype);

    /**
        Returns the static list of image format handlers.

        @see wxImageHandler
    */
    static wxList& GetHandlers();

    /**
        Internal use only. Adds standard image format handlers.
        It only install wxBMPHandler for the time being, which is used by wxBitmap.

        This function is called by wxWidgets on startup, and shouldn't be called by
        the user.

        @see wxImageHandler, wxInitAllImageHandlers(), wxQuantize
    */
    static void InitStandardHandlers();

    /**
        Adds a handler at the start of the static list of format handlers.

        @param handler
            A new image format handler object. There is usually only one instance
            of a given handler class in an application session.

        @see wxImageHandler
    */
    static void InsertHandler(wxImageHandler* handler);

    /**
        Finds the handler with the given name, and removes it.

        The handler is also deleted.

        @param name
            The handler name.

        @return @true if the handler was found and removed, @false otherwise.

        @see wxImageHandler
    */
    static bool RemoveHandler(const wxString& name);

    //@}


    /**
        Returns @true if at least one of the available image handlers can read
        the file with the given name.

        See wxImageHandler::CanRead for more info.
    */
    static bool CanRead(const wxString& filename);

    /**
        Returns @true if at least one of the available image handlers can read
        the data in the given stream.

        See wxImageHandler::CanRead for more info.
    */
    static bool CanRead(wxInputStream& stream);

    /**
        Returns the currently used default file load flags.

        See SetDefaultLoadFlags() for more information about these flags.

        @since 3.1.0
     */
    static int GetDefaultLoadFlags();

    //@{
    /**
        If the image file contains more than one image and the image handler is
        capable of retrieving these individually, this function will return the
        number of available images.

        For the overload taking the parameter @a filename, that's the name
        of the file to query.
        For the overload taking the parameter @a stream, that's the opened input
        stream with image data.

        See wxImageHandler::GetImageCount() for more info.

        The parameter @a type may be one of the following values:
        @li wxBITMAP_TYPE_BMP: Load a Windows bitmap file.
        @li wxBITMAP_TYPE_GIF: Load a GIF bitmap file.
        @li wxBITMAP_TYPE_JPEG: Load a JPEG bitmap file.
        @li wxBITMAP_TYPE_PNG: Load a PNG bitmap file.
        @li wxBITMAP_TYPE_PCX: Load a PCX bitmap file.
        @li wxBITMAP_TYPE_PNM: Load a PNM bitmap file.
        @li wxBITMAP_TYPE_TIFF: Load a TIFF bitmap file.
        @li wxBITMAP_TYPE_TGA: Load a TGA bitmap file.
        @li wxBITMAP_TYPE_XPM: Load a XPM bitmap file.
        @li wxBITMAP_TYPE_ICO: Load a Windows icon file (ICO).
        @li wxBITMAP_TYPE_CUR: Load a Windows cursor file (CUR).
        @li wxBITMAP_TYPE_ANI: Load a Windows animated cursor file (ANI).
        @li wxBITMAP_TYPE_ANY: Will try to autodetect the format.

        @return Number of available images. For most image handlers, this is 1
                (exceptions are TIFF and ICO formats as well as animated GIFs
                for which this function returns the number of frames in the
                animation).
    */
    static int GetImageCount(const wxString& filename,
                             wxBitmapType type = wxBITMAP_TYPE_ANY);
    static int GetImageCount(wxInputStream& stream,
                             wxBitmapType type = wxBITMAP_TYPE_ANY);
    //@}

    /**
        Iterates all registered wxImageHandler objects, and returns a string containing
        file extension masks suitable for passing to file open/save dialog boxes.

        @return The format of the returned string is @c "(*.ext1;*.ext2)|*.ext1;*.ext2".
                It is usually a good idea to prepend a description before passing
                the result to the dialog.
                Example:
                @code
                wxFileDialog FileDlg( this, "Choose Image", ::wxGetCwd(), "",
                                      _("Image Files ") + wxImage::GetImageExtWildcard(),
                                      wxFD_OPEN );
                @endcode

        @see wxImageHandler
    */
    static wxString GetImageExtWildcard();

    /**
        Returns the file load flags used for this object.

        See SetLoadFlags() for more information about these flags.

        @since 3.1.0
     */
    int GetLoadFlags() const;

    /**
        Converts a color in RGB color space to HSV color space.
    */
    static wxImage::HSVValue RGBtoHSV(const wxImage::RGBValue& rgb);

    /**
        Converts a color in HSV color space to RGB color space.
    */
    static wxImage::RGBValue HSVtoRGB(const wxImage::HSVValue& hsv);
};


class wxImageHistogram : public wxImageHistogramBase
{
public:
    wxImageHistogram();

    // get the key in the histogram for the given RGB values
    static unsigned long MakeKey(unsigned char r,
                                 unsigned char g,
                                 unsigned char b);

    // find first colour that is not used in the image and has higher
    // RGB values than RGB(startR, startG, startB)
    //
    // returns true and puts this colour in r, g, b (each of which may be NULL)
    // on success or returns false if there are no more free colours
    bool FindFirstUnusedColour(unsigned char *r,
                               unsigned char *g,
                               unsigned char *b,
                               unsigned char startR = 1,
                               unsigned char startG = 0,
                               unsigned char startB = 0 ) const;
};

/**
    An instance of an empty image without an alpha channel.
*/
wxImage wxNullImage;


// ============================================================================
// Global functions/macros
// ============================================================================

/** @addtogroup group_funcmacro_appinitterm */
//@{

/**
    Initializes all available image handlers.

    This function calls wxImage::AddHandler() for all the available image
    handlers (see @ref image_handlers for the full list). Calling it is the
    simplest way to initialize wxImage but it creates and registers even the
    handlers your program may not use. If you want to avoid the overhead of
    doing this you need to call wxImage::AddHandler() manually just for the
    handlers that you do want to use.

    @see wxImage, wxImageHandler

    @header{wx/image.h}
*/
void wxInitAllImageHandlers();

//@}

