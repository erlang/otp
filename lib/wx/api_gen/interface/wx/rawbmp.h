/////////////////////////////////////////////////////////////////////////////
// Name:        rawbmp.h
// Purpose:     interface of wxPixelData
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxPixelData

    A class template with ready to use implementations for getting
    direct and efficient access to wxBitmap's internal data and
    wxImage's internal data through a standard interface. It is
    possible to extend this class (interface) to other types of
    image content.

    Implemented on Windows, GTK+ and macOS:
       @li wxNativePixelData: Class to access to wxBitmap's internal data
           without alpha channel (RGB).
       @li wxAlphaPixelData: Class to access to wxBitmap's internal data with
           alpha channel (RGBA).

    Implemented everywhere:
       @li wxImagePixelData: Class to access to wxImage's internal data with
           alpha channel (RGBA).

    wxMSW note: efficient access is only possible to the bits of the so called
    device independent bitmaps (DIB) under MSW. To ensure that wxBitmap uses a
    DIB internally and not a device dependent bitmap (DDB), you need to pass an
    explicit depth to its ctor, i.e. either 24 or 32, as by default wxBitmap
    creates a DDB of the screen depth.

    Example:

    @code
    wxBitmap bmp(width, size, 24); // explicit depth important under MSW
    wxNativePixelData data(bmp);
    if ( !data )
    {
        // ... raw access to bitmap data unavailable, do something else ...
        return;
    }

    if ( data.GetWidth() < 20 || data.GetHeight() < 20 )
    {
        // ... complain: the bitmap it too small ...
        return;
    }

    wxNativePixelData::Iterator p(data);

    // we draw a (10, 10)-(20, 20) rect manually using the given r, g, b
    p.Offset(data, 10, 10);

    for ( int y = 0; y < 10; ++y )
    {
        wxNativePixelData::Iterator rowStart = p;

        for ( int x = 0; x < 10; ++x, ++p )
        {
            p.Red() = r;
            p.Green() = g;
            p.Blue() = b;
        }

        p = rowStart;
        p.OffsetY(data, 1);
    }
    @endcode

    @library{wxcore}
    @category{gdi}

    @see wxBitmap, wxImage
*/
template <class Image, class PixelFormat = wxPixelFormatFor<Image> >
class wxPixelData :
    public wxPixelDataOut<Image>::template wxPixelDataIn<PixelFormat>
{
public:
    /**
        The type of the class we're working with.
    */
    typedef Image ImageType;

    /**
        Create pixel data object representing the entire image.
    */
    wxPixelData(Image& image);


    /**
        Create pixel data object representing the area of the image defined by
        @a rect.
    */
    wxPixelData(Image& i, const wxRect& rect);

    /**
        Create pixel data object representing the area of the image defined by
        @a pt and @a sz.
    */
    wxPixelData(Image& i, const wxPoint& pt, const wxSize& sz);

    /**
        Return @true of if we could get access to bitmap data successfully.
    */
    operator bool() const;

    /**
        Return the iterator pointing to the origin of the image.
    */
    Iterator GetPixels() const;

    /**
        Returns origin of the rectangular region this wxPixelData represents.
    */
    wxPoint GetOrigin() const;

    /**
        Return width of the region this wxPixelData represents.
    */
    int GetWidth() const;

    /**
        Return height of the region this wxPixelData represents.
    */
    int GetHeight() const;

    /**
        Return the area which this wxPixelData represents in the image.
    */
    wxSize GetSize() const;

    /**
        Return the distance between two rows.
    */
    int GetRowStride() const;


    /**
        The iterator of class wxPixelData.
    */
    class Iterator
    {
    public:

        /**
            Reset the iterator to point to (0, 0).
        */
        void Reset(const PixelData& data);

        /**
            Initializes the iterator to point to the origin of the given pixel
            data.
        */
        Iterator(PixelData& data);

        /**
            Initializes the iterator to point to the origin of the given Bitmap.
        */
        Iterator(wxBitmap& bmp, PixelData& data);

        /**
            Default constructor.
        */
        Iterator();

        /**
            Return @true if this iterator is valid.
        */
        bool IsOk() const;

        /**
            Advance the iterator to the next pixel, prefix version.
        */
        Iterator& operator++();

        /**
            Advance the iterator to the next pixel, postfix (hence less
            efficient -- don't use it unless you absolutely must) version.
        */
        Iterator operator++(int);

        /**
            Move @a x pixels to the right and @a y down.

            @note The rows won't wrap automatically.
        */
        void Offset(const PixelData& data, int x, int y);

        /**
            Move @a x pixels to the right.

            @note The rows won't wrap automatically.
        */
        void OffsetX(const PixelData&data, int x);

        /**
            Move @a y rows to the bottom
        */
        void OffsetY(const PixelData& data, int y);

        /**
            Go to the given position
        */
        void MoveTo(const PixelData& data, int x, int y);

        //@{
        /**
            Data Access: Access to individual colour components.
        */
        ChannelType& Red();
        ChannelType& Green();
        ChannelType& Blue();
        ChannelType& Alpha();
        //@}
    };
};

