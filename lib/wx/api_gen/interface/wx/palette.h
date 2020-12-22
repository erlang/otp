/////////////////////////////////////////////////////////////////////////////
// Name:        palette.h
// Purpose:     interface of wxPalette
// Author:      wxWidgets team
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

/**
    @class wxPalette

    A palette is a table that maps pixel values to RGB colours. It allows the
    colours of a low-depth bitmap, for example, to be mapped to the available
    colours in a display. The notion of palettes is becoming more and more
    obsolete nowadays and only the MSW port is still using a native palette.
    All other ports use generic code which is basically just an array of
    colours.

    It is likely that in the future the only use for palettes within wxWidgets
    will be for representing colour indices from images (such as GIF or PNG).
    The image handlers for these formats have been modified to create a palette
    if there is such information in the original image file (usually 256 or less
    colour images). See wxImage for more information.

    @library{wxcore}
    @category{gdi}

    @stdobjects
    ::wxNullPalette

    @see wxDC::SetPalette(), wxBitmap
*/
class wxPalette : public wxGDIObject
{
public:

    /**
        Default constructor.
    */
    wxPalette();

    /**
        Copy constructor, uses @ref overview_refcount.

        @param palette
            A reference to the palette to copy.
    */
    wxPalette(const wxPalette& palette);

    /**
        Creates a palette from arrays of size @a n, one for each red, blue or
        green component.

        @param n
            The number of indices in the palette.
        @param red
            An array of red values.
        @param green
            An array of green values.
        @param blue
            An array of blue values.

        @beginWxPerlOnly
        In wxPerl this method takes as parameters
        3 array references (they must be of the same length).
        @endWxPerlOnly

        @see Create()
    */
    wxPalette(int n, const unsigned char* red,
              const unsigned char* green,
              const unsigned char* blue);

    /**
        Destructor.

        @see @ref overview_refcount_destruct "reference-counted object destruction"
    */
    virtual ~wxPalette();

    /**
        Creates a palette from arrays of size @a n, one for each red, blue or
        green component.

        @param n
            The number of indices in the palette.
        @param red
            An array of red values.
        @param green
            An array of green values.
        @param blue
            An array of blue values.

        @return @true if the creation was successful, @false otherwise.

        @see wxPalette()
    */
    bool Create(int n, const unsigned char* red,
                const unsigned char* green,
                const unsigned char* blue);

    /**
        Returns number of entries in palette.
    */
    virtual int GetColoursCount() const;

    /**
        Returns a pixel value (index into the palette) for the given RGB values.

        @param red
            Red value.
        @param green
            Green value.
        @param blue
            Blue value.

        @return The nearest palette index or @c wxNOT_FOUND for unexpected errors.

        @see GetRGB()
    */
    int GetPixel(unsigned char red, unsigned char green,
                 unsigned char blue) const;

    /**
        Returns RGB values for a given palette index.

        @param pixel
            The palette index.
        @param red
            Receives the red value.
        @param green
            Receives the green value.
        @param blue
            Receives the blue value.

        @return @true if the operation was successful.

        @beginWxPerlOnly
        In wxPerl this method takes only the @a pixel parameter and
        returns a 3-element list (or the empty list upon failure).
        @endWxPerlOnly

        @see GetPixel()
    */
    bool GetRGB(int pixel, unsigned char* red, unsigned char* green,
                unsigned char* blue) const;

    /**
        Returns @true if palette data is present.
    */
    virtual bool IsOk() const;

    /**
        Assignment operator, using @ref overview_refcount.
    */
    wxPalette& operator =(const wxPalette& palette);
};


/**
    An empty palette.
*/
wxPalette wxNullPalette;


